;;;; store.lisp --- durable store lifecycle and GC scheduling. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The organism holds the current turn's working set in the heap; everything
;;;; older lives in the event log (events.lisp). This file owns exactly:
;;;;   - the SQLite handle lifecycle (open, close, fork safety, save hook)
;;;;   - single-process ownership: the .lock/.owner sidecar pair that makes
;;;;     exactly one live process the store's authority and lets every other
;;;;     process discover and attach to that owner's gateway endpoint
;;;;   - the schema (events table only; messages/turns are gone)
;;;;   - paging primitives (delegated to events.lisp for the event table)
;;;;   - the argument guards every store entry point opens with
;;;;   - GC scheduling helpers (cap-nursery, collect-during-wait)
;;;;
;;;; Two foreign-state hazards, both measured on this machine:
;;;;
;;;;   1. SAVE-LISP-AND-DIE with an open handle. cl-sqlite loads libsqlite3, and
;;;;      a connection holds a C pointer in the Lisp heap. A core saved with a
;;;;      handle open segfaults inside SQLITE::PREPARE-STATEMENT on first use
;;;;      after restore. Unlike A2's TLS case there is no call-site flag that
;;;;      avoids it, because the handle is the point. So CLOSE-STORE is pushed
;;;;      onto SB-EXT:*SAVE-HOOKS* by OPEN-STORE: verified that a core saved
;;;;      through that hook restores clean and reopens the same file.
;;;;
;;;;   2. fork() with an open handle. Measured: the child writes through the
;;;;      inherited handle and SQLite reports success. That is the dangerous
;;;;      outcome, not the safe one — two processes sharing one connection's
;;;;      locking state is documented corruption, and it fails silently rather
;;;;      than loudly. Nothing forks any more — every child is a fresh
;;;;      process — but CLOSE-STORE in a process that is not the handle's owner
;;;;      still disowns rather than disconnects, by the owner pid.

(in-package #:nodecode.kernel)

;;; --- limits and configuration -------------------------------------------

(defun home-store-path (home)
  "The store of the organism whose home is HOME: <home>/history.db."
  (merge-pathnames "history.db" (uiop:ensure-directory-pathname home)))

(defun snapshot-store (from to &aux (db (sqlite:connect (namestring from) :busy-timeout 5000)))
  "The store at FROM copied to TO as SQLite copies a database its writer
goes on writing: VACUUM INTO over a connection of its own, one consistent
snapshot, never a copy of a file being written. => TO."
  ;; Never the store's own connection: every use of that one holds the store
  ;; lock, and a copy that writes a gigabyte would stop every turn and route
  ;; in the image for as long as it took. FROM must exist: a connect makes an
  ;; empty database where there is none.
  (unwind-protect (sqlite:execute-non-query db "vacuum into ?" (namestring to))
    (sqlite:disconnect db))
  to)

;;; Resolved at process start (paths.lisp), never baked by the build.
(define-startup-parameter *default-store-path*
  (home-store-path *home*)
  "Where the organism keeps its transcript when no path is given.")

;;; Paged rather than whole-table because the point of this file is that
;;; history outgrows the heap.
(defparameter *default-page-size* 100
  "Rows PAGE-EVENTS returns when the caller does not say.")

;;; A caller asking for more is asking to defeat the reason history is
;;; off-heap in the first place.
(defconstant +max-page-size+ 5000
  "Ceiling on one page.")

;;; --- conditions ---------------------------------------------------------

(define-condition store-error (error) ()
  (:documentation "Base class for durable-history failures."))

(define-error store-not-open (store-error) ()
  (:report "store is not open")
  (:documentation "Signalled when a read or write is attempted with no handle."))

(define-error store-broken (store-not-open) (reason)
  (:report "the store cannot write right now (~a); it reconnects by itself once writes go through again"
           reason)
  (:documentation "Signalled in place of STORE-NOT-OPEN while a store that a failed write broke
waits for its next reconnect (HEAL-STORE)."))

(define-error store-in-use (store-error)
    (path (owner-pid :initform nil) (owner-port :initform nil))
  (:report "store ~a is already owned by ~:[another live process~;process ~:*~a~]~@[ (gateway on port ~a)~]"
           path owner-pid owner-port)
  ;; OWNER-PORT, when present, is the gateway endpoint that owner published;
  ;; the entry ladder attaches to it instead of treating this refusal as
  ;; fatal.
  (:documentation
   "A live process holds the store's ownership lock."))

(define-error store-already-open (store-error) (path requested-path)
  (:report "store ~a is open; refusing to displace it with ~a — close it first"
           path requested-path)
  ;; Every other thread reaches the store through *STORE*, so an implicit
  ;; displacement would close the live image's history out from under them;
  ;; the caller must CLOSE-STORE deliberately first.
  (:documentation
   "OPEN-STORE was asked for a different path while a store is open."))

(define-error store-unreadable (store-error) (path (reason :initform "not an SQLite database"))
  (:report "~a is not a nodecode store (~a); move it aside and run nodecode again"
           path reason)
  ;; The one sentence for both: the launch preflight says it of a file with no
  ;; SQLite header before anything boots, and a boot says it of what got past
  ;; that. REASON is SQLite's own words for a damaged file (CORRUPT). PATH is
  ;; the full path; a surface writes the operator's home as `~'.
  (:documentation
   "SQLite refused the file at the store's path as a database: not one (NOTADB)
or a damaged one (CORRUPT)."))

(define-error store-page-too-large (store-error) (requested)
  (:report "page size ~:D exceeds the ~:D row ceiling"
           requested +max-page-size+))

(define-error no-transaction (store-error) ()
  (:report "AFTER-COMMIT outside a store transaction")
  (:documentation
   "A deferral was asked for with no transaction to wait for: there is no
commit it could follow, and running it at once would report a write as
durable that never was."))

;;; --- the handle ---------------------------------------------------------

(define-record (store (:copier nil))
  ;; The cl-sqlite handle, or NIL once closed.
  (handle nil)
  (path nil :type (or null pathname string))
  ;; The .owner holder sidecar (JSON: pid, published gateway port).
  (lease-path nil :type (or null pathname string))
  ;; The lock on the .lock sidecar (LOCK-FILE: an fcntl record lock's fd, a
  ;; no-sharing handle on Windows). The lock, not the sidecar, is the
  ;; ownership authority: the kernel releases it when the owner dies, so a
  ;; crash cannot wedge the next startup.
  (lock-fd nil :type (or null integer))
  ;; The pid that opened it. A forked child inherits the struct but must not use
  ;; the handle; comparing pids is how a child notices without being told.
  (owner-pid (sb-posix:getpid) :type integer)
  ;; What this process committed and still holds in memory, per session id
  ;; (events.lisp: REMEMBER-COMMITTED-EVENT fills it, COMMITTED-EVENTS-AFTER
  ;; drains it). A slot, not a global, so the memory lives and dies with the
  ;; store it mirrors: a reopened store starts empty by construction. Its own
  ;; lock (SB-EXT:WITH-LOCKED-HASH-TABLE) guards it and the rings inside.
  (committed (make-hash-table :test #'equal) :type hash-table)
  ;; Why the connection was dropped while the lease is still held -- a write
  ;; failed and so did its ROLLBACK (BREAK-STORE) -- or NIL while it serves.
  ;; A broken store reconnects on its next use (REQUIRE-OPEN-STORE), never
  ;; before RETRY-AT, an INTERNAL-REAL-TIME; each failed try doubles
  ;; RETRY-DELAY, in seconds, up to *STORE-RETRY-LIMIT*. BROKEN-AT is when it
  ;; broke, for the line that says it is back.
  (broken nil :type (or null string))
  (broken-at 0 :type integer)
  (retry-at 0 :type integer)
  (retry-delay 0 :type real))

(defvar *store* nil
  "The image's durable store, or NIL. OPEN-STORE sets it; CLOSE-STORE clears it.")

;;; Pushed once per image: hazard 1 in the header is fatal and easy to
;;; reintroduce, so the guard lives here rather than in whichever slice
;;; remembers to save.
(defvar *save-hook-installed* nil
  "Whether CLOSE-STORE has been pushed onto SB-EXT:*SAVE-HOOKS*.")

(defvar *store-open-hooks* '()
  "Kernel recovery functions called after schema application and handle ownership.")

(defvar *schema-prepare-hooks* '()
  "Functions of one HANDLE, run before *SCHEMA* so a stale table can be dropped.")

(defun store-open-p (&optional (store *store*))
  "True when STORE has a live handle owned by this process."
  (and store
       store.handle
       (= store.owner-pid (sb-posix:getpid))))

(defun require-open-store (&optional (store *store*))
  "Return STORE's handle, or signal STORE-NOT-OPEN -- STORE-BROKEN while a
failed write has it waiting to reconnect (HEAL-STORE)."
  (unless (or (store-open-p store) (heal-store store))
    (if (and store store.broken)
        (error 'store-broken :reason store.broken)
        (error 'store-not-open)))
  store.handle)

;;; --- serialization ------------------------------------------------------
;;;
;;; One connection, one lock. The image shares a single SQLite handle across
;;; the peer-writer, WS-reader, turn, and boot threads, and neither layer
;;; below is safe to share bare: cl-sqlite's per-connection statement cache
;;; is unsynchronized Lisp state, and a read that interleaves inside another
;;; thread's open IMMEDIATE transaction observes uncommitted rows. So every
;;; access — reads included — serializes on *STORE-LOCK*. The lock is
;;; recursive because transactions call the same read helpers they guard.

(defvar *store-lock* (bt2:make-recursive-lock :name "store")
  "Serializes every use of the store's one SQLite connection.")

(defvar *store-reader* nil
  "Inside WITH-STORE-READER, this thread's own read-only connection to the
store; NIL elsewhere.")

(defvar *after-commit* :none
  "Inside CALL-WITH-STORE-TRANSACTION, the functions AFTER-COMMIT deferred to
the commit, newest first; :NONE outside one, so a deferral with no
transaction to wait for is refused rather than run at once or lost.")

(defmacro with-store-handle ((handle) &body body &aux (run (gensym "RUN")))
  "Run BODY with HANDLE bound to the open store's connection, store lock held
-- or to this thread's reader inside WITH-STORE-READER, no lock."
  ;; Every SQLite access outside CALL-WITH-STORE-TRANSACTION goes through
  ;; here.
  `(flet ((,run (,handle) (declare (ignorable ,handle)) ,@body))
     (declare (dynamic-extent #',run))
     (if (and *store-reader* (eq *after-commit* :none))
         (,run *store-reader*)
         (bt2:with-recursive-lock-held (*store-lock*)
           (,run (require-open-store))))))

(defmacro when-store-handle ((handle &optional absent) &body body)
  "Run BODY as WITH-STORE-HANDLE does, or answer ABSENT without running it
when no store is open."
  ;; The read-side twin: a projection that must degrade rather than signal on
  ;; a closed store — a count that answers 0 before boot, a directory that
  ;; answers nothing — states its absent value here. Written out, the
  ;; STORE-OPEN-P guard and the value it fell back to were two lines wrapped
  ;; around the read by hand at every such reader, and the pair drifted: some
  ;; answered NIL where their caller wanted 0.
  `(if (store-open-p)
       (with-store-handle (,handle) ,@body)
       ,absent))

(defun call-with-store-reader (function)
  "Run FUNCTION with this thread's reads on a read-only connection of their
own, off *STORE-LOCK*."
  ;; A long read on the shared connection holds the lock every writer needs:
  ;; a cold Dashboard usage scan held it for 9-17 s and every tool result,
  ;; turn fact and card waited behind it (T-331). WAL readers never block the
  ;; writer, so a scan that needs no uncommitted rows reads on a connection
  ;; of its own, opened for the extent and closed after. Inside a transaction
  ;; the reads stay on the transaction's connection (a writer composes its
  ;; own reads and must see its rows); a nested scope reuses the outer
  ;; reader; with no store open the reads fail as they always did. The
  ;; reader is query_only: a write inside the scope is an error, never a
  ;; second writer.
  (if (or *store-reader* (not (eq *after-commit* :none)) (not (store-open-p)))
      (funcall function)
      (let ((reader (sqlite:connect (store-path *store*) :busy-timeout 5000)))
        (unwind-protect
             (progn (sqlite:execute-non-query reader "pragma query_only = on")
                    (let ((*store-reader* reader))
                      (funcall function)))
          (ignore-errors (sqlite:disconnect reader))))))

(defmacro with-store-reader (() &body body)
  "Run BODY with its store reads on a read-only connection of their own,
off *STORE-LOCK* (CALL-WITH-STORE-READER)."
  `(call-with-store-reader (lambda () ,@body)))

(defun after-commit (function)
  "Run FUNCTION, of no arguments, once the enclosing transaction commits —
after the COMMIT statement, in the order deferred, still under the store
lock."
  ;; A transaction that rolls back drops what it deferred: the consequence of
  ;; a write must not outlive the write it follows from, and a writer that
  ;; wants an in-memory mirror of what it just made durable has no other way
  ;; to know the row is really there. Signals outside a transaction.
  (when (eq *after-commit* :none)
    (error 'no-transaction))
  (push function *after-commit*)
  function)

(defun call-with-store-transaction (function)
  "Run FUNCTION with the store handle inside one IMMEDIATE transaction."
  ;; The one transaction authority for the image: writers compose reads and
  ;; appends inside FUNCTION and the commit/rollback discipline lives here
  ;; only. A non-local exit out of FUNCTION — an interruption landing between
  ;; two of its statements — rolls back on the way out, so the connection
  ;; never stays inside an open transaction that would refuse every later
  ;; BEGIN. What FUNCTION deferred through AFTER-COMMIT runs once the commit
  ;; is settled and the transaction closed, outside the rollback handler: a
  ;; deferral that signals reaches the caller as its own error, never as a
  ;; ROLLBACK of a transaction that is no longer open.
  (bt2:with-recursive-lock-held (*store-lock*)
    (let ((handle (require-open-store))
          (settled nil)
          (*after-commit* '()))
      (sqlite:execute-non-query handle "begin immediate")
      (multiple-value-prog1
          (with-cleanup ((unless settled
                           (roll-back handle nil)))
            (handler-case
                (multiple-value-prog1 (funcall function handle)
                  (sqlite:execute-non-query handle "commit")
                  (setf settled t))
              (error (condition)
                (setf settled t)
                (roll-back handle condition)
                (error condition))))
        (dolist (deferred (reverse *after-commit*))
          (funcall deferred))))))

(defmacro with-store-transaction ((handle &key within) &body body
                                  &aux (write (gensym "WRITE")) (open (gensym "OPEN")))
  "Run BODY with HANDLE bound to the store handle inside one IMMEDIATE
transaction -- the caller's own when WITHIN evaluates to its handle."
  ;; The macro twin of WITH-STORE-HANDLE, and the form every writer should
  ;; reach for: CALL-WITH-STORE-TRANSACTION stays exported for the few callers
  ;; that pass a closure they already have. A writer that takes the handle of
  ;; a transaction already open (WITHIN) commits with it, so a caller can land
  ;; many such writes as one; a transaction never nests, so without it this
  ;; opens its own.
  (if within
      `(flet ((,write (,handle) (declare (ignorable ,handle)) ,@body))
         (let ((,open ,within))
           (if ,open (,write ,open) (call-with-store-transaction #',write))))
      `(call-with-store-transaction (lambda-ignoring (,handle) ,@body))))

;;; --- the connection guards itself ---------------------------------------
;;;
;;; The lock above is the caller's discipline; this is the connection's own.
;;; A snippet can reach the bare handle — (require-open-store), or the struct's
;;; slot — and loop EXECUTE-TO-LIST on the handle from its own thread, no lock,
;;; beside the turn thread's durable writes. An EVAL-INTERRUPT that stops it
;;; unwinds that thread out of a glibc frame (INTERRUPT-THREAD runs its
;;; function wherever the thread is, foreign frames included). Without the
;;; guards below, the connection's recursive mutex would be left locked with no
;;; owner; every later statement in the image waits on it forever, and the
;;; shell would see only "background failed: I/O timeout" from the control call
;;; that never gets the store.
;;;
;;; So every cl-sqlite entry that touches a connection or a statement is
;;; advised here: on the store's connection it holds *STORE-LOCK*, and on any
;;; connection it runs with interrupts deferred, so an interruption — an eval
;;; stop, a runaway probe, a thread termination — lands between statements,
;;; never inside a foreign frame. Whoever calls, however they got the handle.

(defun call-guarding-connection (db thunk)
  "Run THUNK as one access to DB: under *STORE-LOCK* when DB is the store's
connection, interrupts deferred for the extent either way."
  (flet ((run () (sb-sys:without-interrupts (funcall thunk))))
    (declare (dynamic-extent #'run))
    (if (and *store* (eq db (store-handle *store*)))
        (bt2:with-recursive-lock-held (*store-lock*) (run))
        (run))))

(defparameter *guarded-connection-entries*
  '((sqlite:prepare-statement . :db)
    (sqlite:execute-non-query . :db) (sqlite:execute-non-query/named . :db)
    (sqlite:execute-to-list . :db) (sqlite:execute-to-list/named . :db)
    (sqlite:execute-single . :db) (sqlite:execute-single/named . :db)
    (sqlite:execute-one-row-m-v . :db) (sqlite:execute-one-row-m-v/named . :db)
    (sqlite:last-insert-rowid . :db) (sqlite:set-busy-timeout . :db)
    (sqlite:disconnect . :db)
    (sqlite:step-statement . :statement) (sqlite:finalize-statement . :statement)
    (sqlite:reset-statement . :statement) (sqlite::clear-statement-bindings . :statement)
    (sqlite::really-finalize-statement . :statement)
    (sqlite:bind-parameter . :statement) (sqlite:statement-column-value . :statement))
  "Every cl-sqlite entry that touches a connection or a statement, with where
its connection is: the first argument, or the statement's DB.")

(defun guard-connection-entries ()
  "Advise each of *GUARDED-CONNECTION-ENTRIES* with CALL-GUARDING-CONNECTION."
  ;; Idempotent: a reload replaces the advice rather than stacking it.
  (loop for (name . where) in *guarded-connection-entries*
        do (encapsulate-afresh
            name 'store-guard
            (ecase where
              (:db (lambda (next db &rest args)
                     (call-guarding-connection db (lambda () (apply next db args)))))
              (:statement (lambda (next statement &rest args)
                            (call-guarding-connection
                             (sqlite::db statement)
                             (lambda () (apply next statement args)))))))))

(guard-connection-entries)

;;; --- reading rows ---------------------------------------------------------
;;; cl-sqlite's EXECUTE-TO-LIST reads a row a column at a time through the
;;; guarded STATEMENT-COLUMN-VALUE — the lock taken and interrupts deferred
;;; once per column — each text column through cffi's general UTF-8 decoder
;;; and each blob a byte at a time. The store reads through SQL-ROWS.

(defun column-value (handle column)
  "Column COLUMN of statement HANDLE's current row as cl-sqlite reads it:
NIL, an integer, a double, a string, or a blob's octets."
  ;; SQLite's order: a column's bytes are asked for before their count.
  (flet ((octets (pointer count &aux (octets (make-array count :element-type '(unsigned-byte 8))))
           (sb-kernel:copy-ub8-from-system-area pointer 0 octets 0 count)
           octets))
    (ecase (sqlite-ffi:sqlite3-column-type handle column)
      (:null nil)
      (:integer (sqlite-ffi:sqlite3-column-int64 handle column))
      (:float (sqlite-ffi:sqlite3-column-double handle column))
      (:blob (octets (sqlite-ffi:sqlite3-column-blob handle column)
                     (sqlite-ffi:sqlite3-column-bytes handle column)))
      (:text
       ;; Decode once, straight from SQLite's buffer. An ASCII-first copy
       ;; followed by SBCL's Unicode sizing/decoding pass cost ~0.5 s on
       ;; history's large text columns. COUNT preserves embedded NULs.
       (let* ((pointer (sqlite-ffi:sqlite3-column-blob handle column))
              (count (sqlite-ffi:sqlite3-column-bytes handle column)))
         (if (zerop count) (make-string 0)
             (cffi:foreign-string-to-lisp pointer :count count :encoding :utf-8)))))))

(defun sql-rows (db sql &rest parameters)
  "SQL's rows on DB with PARAMETERS bound in order, each a list of its
column values: cl-sqlite's EXECUTE-TO-LIST, in one guarded access."
  (call-guarding-connection
   db (lambda ()
        (sqlite::with-prepared-statement statement (db sql parameters)
          (loop with handle = (sqlite::handle statement)
                while (sqlite:step-statement statement)
                collect (loop for column below (sqlite-ffi:sqlite3-column-count handle)
                              collect (column-value handle column)))))))

;;; --- schema -------------------------------------------------------------
;;; The schema is now just the events table. *SCHEMA* is the list events.lisp
;;; extends with its own DDL at load time. That extension seam is preserved for
;;; compatibility with the C0 coexistence test.

(defparameter *schema*
  '()
  "Applied by OPEN-STORE in order. Events.lisp appends its DDL at load time.")

;;; events.lisp extends *SCHEMA* when it loads. Guard duplicates.
;;; (This guard block is in events.lisp; here we just define the starting list.)

(defun apply-schema (handle)
  (dolist (hook *schema-prepare-hooks*)
    (funcall hook handle))
  (dolist (statement *schema*)
    (sqlite:execute-non-query handle statement)))

;;; --- a broken connection heals ------------------------------------------
;;;
;;; A write whose ROLLBACK fails too leaves its connection unusable -- every
;;; later BEGIN would be refused -- and the usual cause passes: a full disk, a
;;; write into a mapped file the filesystem could not back. So the store drops
;;; that connection and keeps the rest: the lease above all, so no standby
;;; takes the database while this process still runs as its owner, and
;;; *STORE*, so the next use reconnects (REQUIRE-OPEN-STORE) and the outage
;;; lasts as long as its cause. It once reconnected a single time and, that
;;; failing too, closed outright: the lease went to a standby, and this
;;; process refused every write with STORE-NOT-OPEN until it was restarted.
;;; Only CLOSE-STORE, a deliberate close, gives the lease up.

(defparameter *store-retry-limit* 30
  "The longest wait, in seconds, between a broken store's reconnect tries.")

(defun connect-store-handle (path)
  "A fresh connection to the database at PATH with the schema applied, or NIL
-- the half-made connection closed, never leaked -- when either step fails."
  (let ((fresh nil))
    (handler-case (progn (setf fresh (sqlite:connect path))
                         (apply-schema fresh)
                         fresh)
      (error ()
        (when fresh (ignore-errors (sqlite:disconnect fresh)))
        nil))))

(defun heal-store (&optional (store *store*))
  "Reconnect STORE when a failed write broke it and its next try is due:
true once it has a connection again."
  (and store store.broken store.lock-fd
       (= store.owner-pid (sb-posix:getpid))
       (>= (get-internal-real-time) store.retry-at)
       (bt2:with-recursive-lock-held (*store-lock*)
         (cond (store.handle t)             ; another thread healed it meanwhile
               ((not (and store.broken store.lock-fd)) nil)
               (t (let ((fresh (connect-store-handle store.path))
                        (now (get-internal-real-time)))
                    (cond (fresh
                           (let ((reason store.broken))
                             (setf store.handle fresh
                                   store.broken nil
                                   store.retry-at 0
                                   store.retry-delay 0)
                             (say-store "store: reconnected ~,1f s after a write failed (~a); writes go through again"
                                        (float (/ (- now store.broken-at) internal-time-units-per-second))
                                        reason))
                           t)
                          (t
                           (setf store.retry-delay (min *store-retry-limit*
                                                        (max 1/2 (* 2 store.retry-delay)))
                                 store.retry-at (+ now (round (* store.retry-delay
                                                                 internal-time-units-per-second))))
                           nil))))))))

(defun break-store (store reason)
  "Drop STORE's connection, which a failed ROLLBACK left unusable, keeping its
lease, and try at once to reconnect (HEAL-STORE)."
  (when (and store store.handle (= store.owner-pid (sb-posix:getpid)))
    (bt2:with-recursive-lock-held (*store-lock*)
      (ignore-errors (sqlite:disconnect store.handle))
      (setf store.handle nil
            store.broken (or (ignore-errors (princ-to-string reason)) "a write failed")
            store.broken-at (get-internal-real-time)
            store.retry-at 0
            store.retry-delay 0)
      (unless (heal-store store)
        (say-store "store: a write failed and so did its rollback (~a); the connection is dropped, the lease kept, and the next use reconnects"
                   store.broken)))))

(defun say-store (control &rest arguments)
  "Warn CONTROL with ARGUMENTS for the log -- a line lost, never a write failed
anew, when the log sits on the very disk that is full."
  (ignore-errors (apply #'warn control arguments)))

(defun roll-back (handle reason)
  "Roll HANDLE's open transaction back; when the ROLLBACK fails too, the
connection is unusable and BREAK-STORE drops it, REASON the failure that
ended the transaction (the ROLLBACK's own when NIL)."
  (handler-case (sqlite:execute-non-query handle "rollback")
    (error (failure) (break-store *store* (or reason failure)))))

;;; --- lifecycle ----------------------------------------------------------

;;; Ownership is distilled from the Zig gateway's database lifetime lease
;;; (src/db/lease.zig). Two sidecars next to the database:
;;;
;;;   <db>.lock   the authority: an fcntl record lock held for the store's
;;;               lifetime. It dies with its holder, so there is no stale-pid
;;;               reclamation path and no way for a crash to wedge startup.
;;;   <db>.owner  the advertisement: JSON naming the owner pid and, once a
;;;               gateway listener is bound (PUBLISH-STORE-ENDPOINT), its
;;;               port — so a contender that loses the lock can attach as a
;;;               client instead of refusing.

(defun lease-path-for-store (path)
  (pathname (concatenate 'string (namestring path) ".owner")))

(defun lock-path-for-store (path)
  (pathname (concatenate 'string (namestring path) ".lock")))

(defun write-store-holder (holder-path &key pid port)
  "Atomically replace the holder sidecar: write a temp file, then rename."
  (write-file-atomically
   holder-path (format nil "~a~%" (shasht:write-json (json-object "pid" pid :opt "port" port) nil))
   :mode #o600))

(defun read-store-holder (path)
  "Read PATH's holder sidecar."
  ;; Values: owner pid, published gateway port — either may be NIL when the
  ;; sidecar is missing, torn, or unpublished.
  (with-handlers ((error () (values nil nil)))
    (with-open-file (stream (lease-path-for-store path) :direction :input)
      (with-json ((pid :integer "pid") (port :integer "port"))
          (nlk:decode-json stream)
        (values (and pid (plusp pid) pid) (and port (plusp port) port))))))

(defun store-lease-held-p (path)
  "True when a live process holds PATH's ownership lock."
  ;; A probe, not a claim: FILE-LOCKED-P never contends with the owner.
  (file-locked-p (lock-path-for-store path)))

(defun store-lease-mine-p (path)
  "True when this process holds PATH's ownership lock."
  ;; *HELD-LOCKS* is the only way to ask: opening the lock file to probe it
  ;; would release it (FILE-LOCKED-P).
  (let ((native (uiop:native-namestring (lock-path-for-store (merge-pathnames path)))))
    (sb-ext:with-locked-hash-table (*held-locks*)
      (find native (alexandria:hash-table-values *held-locks*) :test #'equal))))

(defun store-death-verdict (path)
  "Classify PATH's ownership state for recovery decisions."
  ;; Values: verdict, advertised owner pid, advertised gateway port.
  ;;
  ;;   :live            a live process holds the ownership lock.
  ;;   :dirty-death     nobody holds the lock but the holder sidecar survives —
  ;;                    the owner died without RELEASE-STORE-LEASE (the kernel
  ;;                    freed the lock with the process; nobody deleted the
  ;;                    advertisement). The one state that licenses automatic
  ;;                    resurrection.
  ;;   :clean-shutdown  nobody holds the lock and no sidecar advertises an owner:
  ;;                    an orderly close, or no owner ever ran. Resurrection here
  ;;                    would restart a server the operator deliberately stopped.
  ;;
  ;; The lock is the authority and the sidecar is testimony: a torn or missing
  ;; sidecar under a free lock reads as :clean-shutdown, which fails safe — no
  ;; automatic respawn.
  (multiple-value-bind (pid port) (read-store-holder path)
    (cond ((store-lease-held-p path) (values :live pid port))
          (pid (values :dirty-death pid port))
          (t (values :clean-shutdown nil nil)))))

(defun release-store-lease (store)
  (when store.lease-path
    (ignore-errors (delete-file store.lease-path)))
  (when store.lock-fd
    (ignore-errors (unlock-file store.lock-fd)))
  (setf store.lease-path nil
        store.lock-fd nil))

(defun publish-store-endpoint (port &optional (store *store*))
  "Rewrite the holder sidecar with the gateway's bound PORT — the lease.zig
publishEndpoint seam — so a contender that loses the lock can attach instead
of dying."
  ;; NIL retracts a previous publication. No-op unless this process owns an
  ;; open store.
  (when (and (store-open-p store) store.lease-path)
    (write-store-holder store.lease-path
                        :pid (sb-posix:getpid) :port port)
    port))

(defun open-store (&key (path *default-store-path*))
  "Open the durable store at PATH, apply the schema, and make it the image's
store."
  ;; Returns the STORE. Idempotent for an already-open identical path; signals
  ;; STORE-ALREADY-OPEN for a different path rather than displacing —
  ;; displacement closed a live gateway's history under its turn threads when
  ;; an in-image eval ran a store fixture.
  (bt2:with-recursive-lock-held (*store-lock*)
    (when (store-open-p *store*)
      (if (equal (namestring (store-path *store*)) (namestring path))
          (return-from open-store *store*)
          (error 'store-already-open :path (store-path *store*)
                                     :requested-path path)))
    (let* ((full (merge-pathnames path))
           (directory (make-pathname :directory (pathname-directory full)))
           ;; The store's directory exists, 0700, before the lease is taken.
           (lock-fd (progn (ensure-directories-exist directory)
                           (ignore-errors (sb-posix:chmod (namestring directory) #o700))
                           (lock-file (lock-path-for-store full))))
           (lease-path (lease-path-for-store full))
           (store (make-store :path full :lease-path lease-path :lock-fd lock-fd)))
      (unless lock-fd
        ;; The owner writes its sidecar right after locking; ride that
        ;; publication window (lease.zig polls the same way) rather than
        ;; reporting an anonymous owner on a microsecond race.
        (wait-until (lambda () (read-store-holder full)) (deadline-after 0.25))
        (multiple-value-bind (pid port) (read-store-holder full)
          (error 'store-in-use :path full :owner-pid pid :owner-port port)))
      (write-store-holder lease-path :pid (sb-posix:getpid))
      (handler-case
          (progn
            (setf store.handle (sqlite:connect full))
            (apply-schema store.handle)
            (setf *store* store)
            (unless *save-hook-installed*   ; hazard 1
              (push (lambda () (ignore-errors (close-store))) sb-ext:*save-hooks*)
              (setf *save-hook-installed* t))
            (dolist (hook *store-open-hooks*)
              (funcall hook))
            store)
        (error (condition)
          ;; The lock and holder are ours — the lease was just taken above —
          ;; so unwind in ownership order: retract the published store and close
          ;; the handle FIRST, release the lease LAST. Open hooks perform durable
          ;; writes, so releasing the lease while the handle lives would let a
          ;; second process acquire the same database under a live writer. The
          ;; store's lock-fd slot is cleared before the fd is closed here so no
          ;; later CLOSE-STORE can close the (possibly reused) fd number again.
          (close-store store)
          (case (and (typep condition 'sqlite:sqlite-error) (sqlite:sqlite-error-code condition))
            (:notadb (error 'store-unreadable :path (uiop:native-namestring full)))
            (:corrupt (error 'store-unreadable
                             :path (uiop:native-namestring full)
                             :reason (or (sqlite:sqlite-error-message condition) "a damaged database")))
            (t (error condition))))))))

(defun close-store (&optional (store *store*))
  "Close STORE's handle and clear it."
  ;; Safe to call on a closed or NIL store, and safe in a forked child: a
  ;; child whose pid does not own the handle drops its reference without
  ;; closing the parent's fd.
  (when store
    (cond ((/= (store-owner-pid store) (sb-posix:getpid))
           ;; Hazard 2. Disown, do not disconnect — and drop the inherited
           ;; lock fd reference without closing it (fcntl locks are unshared
           ;; by fork; the parent's claim is the parent's business).
           (setf store.handle nil
                 store.lock-fd nil))
          (t
           ;; Disconnect under *STORE-LOCK*: a reader inside WITH-STORE-HANDLE
           ;; must never have the connection freed mid-query. Waiters then see
           ;; the cleared handle and refuse with STORE-NOT-OPEN. The forked
           ;; child branch above stays lock-free on purpose — a lock inherited
           ;; held across fork would never be released in the child.
           (bt2:with-recursive-lock-held (*store-lock*)
             (when store.handle (ignore-errors (sqlite:disconnect store.handle)))
             (setf store.handle nil))
           (release-store-lease store))))
  (when (eq store *store*)
    (setf *store* nil))
  store)

;;; --- argument guards ----------------------------------------------------

(defmacro check-strings (&rest places)
  "CHECK-TYPE every one of PLACES against STRING."
  ;; The guard every durable entry point opens with: an identity argument — a
  ;; session id, a turn id, a state key, a model id — is refused here rather
  ;; than reaching SQLite as a bind parameter of the wrong type, where the
  ;; mistake becomes a stored row instead of a TYPE-ERROR. One line per
  ;; argument, the guard was a ladder of identical lines at the head of nearly
  ;; every writer and reader in the store; one line per group it is the same
  ;; refusal, with the same condition and the same STORE-VALUE restart.
  `(progn ,@(mapcar (lambda (place) `(check-type ,place string)) places)))

(deftype session-id () "A durable session id (MAKE-DURABLE-ID)." 'string)
(deftype turn-id () "A durable turn id (MAKE-DURABLE-ID)." 'string)
(deftype log-position () "A global position in the event log; 0 is before the first fact." '(integer 0))

(defun check-page-size (limit)
  (unless (and (integerp limit) (plusp limit))
    (error 'type-error :datum limit :expected-type '(integer 1)))
  (when (> limit +max-page-size+)
    (error 'store-page-too-large :requested limit))
  limit)

