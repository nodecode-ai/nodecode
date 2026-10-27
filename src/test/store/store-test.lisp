;;;; store-test.lisp --- A3 gate: off-heap transcript and scheduled collection.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three gate claims, each with a test that would fail if the claim were false:
;;;;
;;;;   1. Live-heap size stays flat across 100 recorded turns.
;;;;   2. The forced full-GC pause lands inside provider wait, not inside a frame.
;;;;   3. SB-EXT:*GC-RUN-TIME* is readable by the organism as a fitness input.
;;;;
;;;; Claim 2 needs a real blocking request to be meaningful, so its live form runs
;;;; under NODECODE_LIVE=1 alongside A2's live tests. An offline form proves
;;;; the same mechanism against a deterministic sleep, so the scheduling logic is
;;;; covered in the default run and only the provider timing needs the network.

(in-package #:nodecode.test)

(nlk:access (session nle::session) (store nlk::store))

;;; --- helpers ------------------------------------------------------------
;;; TEMP-DIRECTORY is defined here rather than in support.lisp because that file
;;; belongs to A1's reservation scope. It follows the same shape as A1's
;;; TEMP-SOCKET-PATH: per-process, private, removed by the fixture.

(defun temp-directory (&optional (tag "store"))
  "Create and return a fresh private directory for one test."
  (let ((dir (pathname (format nil "/tmp/nl-~a-~a-~a/" (sb-posix:getpid) tag (random 100000)))))
    (ensure-directories-exist dir)
    (ignore-errors (sb-posix:chmod (namestring dir) #o700))
    dir))

(defmacro with-temp-directory ((dir tag &key close-store) &body body)
  "Run BODY with DIR bound to a fresh 0700 directory tagged TAG, and remove
the tree on the way out however BODY leaves — the one cleanup authority the
raw-directory fixtures share, so no test restates the unwind itself."
  ;; CLOSE-STORE additionally closes the store on the way out, for the tests
  ;; that open one directly rather than through WITH-TEMP-STORE. It is
  ;; deliberately opt-in: closing unconditionally would displace an outer
  ;; fixture's store from a nested extent, which is the very hazard
  ;; WITH-TEMP-STORE's refusal path exists to prevent.
  `(let ((,dir (temp-directory ,tag)))
     (nlk:with-cleanup (,@(when close-store '((ignore-errors (nlk:close-store))))
                        (ignore-errors (uiop:delete-directory-tree ,dir :validate t)))
       ,@body)))

(defmacro with-temp-store ((&optional var) &body body &aux (dir (gensym "DIR"))
                                                           (opened (gensym "OPENED"))
                                                           (handle (or var (gensym "STORE"))))
  "Open a store in a fresh 0700 directory, bind VAR to it, and remove the tree."
  ;; VAR is optional because most tests want an OPEN STORE, not a handle: they
  ;; reach the store through *STORE* by way of the kernel's own entry points
  ;; and never name the object. `(with-temp-store () ...)' says exactly that.
  ;; The handle-binding form stays for the two dozen tests that assert on the
  ;; object itself. Either way the binding carries its own IGNORABLE — a call
  ;; site must never restate one the macro already owns.
  ;;
  ;; A real file rather than :memory: on purpose: WAL mode, the save hook, and
  ;; the fork hazard are all properties of an on-disk database, and an
  ;; in-memory store would pass tests that the real one fails.
  ;;
  ;; In a live image whose store is open, OPEN-STORE refuses loudly with
  ;; STORE-ALREADY-OPEN instead of displacing the image's history — storeful
  ;; tests belong in the offline suite, not in an in-image eval.
  `(with-temp-directory (,dir "store")
     (let ((,opened nil))
       (unwind-protect
            (let ((,handle (setf ,opened (nlk:open-store
                                          :path (merge-pathnames "history.db"
                                                                 ,dir)))))
              (declare (ignorable ,handle))
              ,@body)
         ;; Close only when this fixture's open succeeded: ,OPENED non-NIL
         ;; means no store was open at entry, so whatever is open now —
         ;; the fixture's own store, or one the body reopened across a
         ;; simulated restart — belongs to this extent. When OPEN-STORE
         ;; refused to displace a live image's store, closing the default
         ;; *STORE* here would be the very displacement the refusal
         ;; prevented, so the refusal path must not close anything. The
         ;; directory itself goes with WITH-TEMP-DIRECTORY's own unwind.
         (when ,opened
           (ignore-errors (nlk:close-store)))))))

(defun reopen-store (path)
  "Close the store and open PATH again: a simulated restart."
  (nlk:close-store)
  (nlk:open-store :path path))

(defmacro with-restartable-store ((path session-id &rest create-keys) &body body)
  "Run BODY with PATH naming a fresh temp directory's history.db, opened as
the store with SESSION-ID created on it (CREATE-KEYS reach CREATE-SESSION),
the store closed and the directory removed on unwind: the fixture for a
simulated restart, which reopens PATH itself."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir ,session-id :close-store t)
       (let ((,path (merge-pathnames "history.db" ,dir)))
         (nlk:open-store :path ,path)
         (nlk:create-session :id ,session-id ,@create-keys)
         ,@body))))

(defmacro with-session-store ((session-id) &body body)
  "Open a temp store holding one session, SESSION-ID, and run BODY."
  ;; The storeful preamble almost every test in this tree opens with: a store
  ;; alone is not a fixture, because nothing durable can be admitted, recalled
  ;; or folded until a session fact exists to hang it on. Written by hand that
  ;; is WITH-TEMP-STORE and an ENSURE-DURABLE-SESSION on the line under it, at
  ;; every one of eighty-odd sites; owned here it is one form, and the
  ;; session's scratch cwd comes with it rather than being restated.
  ;;
  ;; The session is ensured, not created: ENSURE-DURABLE-SESSION is
  ;; idempotent, so a body that reopens the store across a simulated restart
  ;; and re-enters this extent still names the same session. A test that needs
  ;; a session on a particular cwd — the workspace-capture and identity gates
  ;; — states its own CREATE-SESSION inside a plain WITH-TEMP-STORE, because
  ;; the cwd is the claim there and a fixture must not own it.
  `(with-temp-store ()
     (ensure-durable-session ,session-id)
     ,@body))

(defmacro with-admitted-turn ((turn session-id command-id input &rest bindings) &body body)
  "WITH-SESSION-STORE over SESSION-ID with TURN admitted on it under COMMAND-ID
carrying INPUT, then BINDINGS, LET* bindings that may name TURN."
  `(with-session-store (,session-id)
     (let* ((,turn (nlk:admit-turn ,session-id ,command-id ,input))
            ,@bindings)
       ,@body)))


;;; --- lifecycle ----------------------------------------------------------

(deftest store-opens-and-reports-its-path (with-temp-store (store))
  (is (nlk::store-open-p store) "store is open")
  (is (pathnamep store.path) "path is a pathname")
  (is (probe-file store.path) "database file exists on disk"))

(deftest store-directory-is-private-to-its-owner (with-temp-store (store))
  (let* ((dir (make-pathname :directory (pathname-directory store.path)))
         (mode (logand (sb-posix:stat-mode (sb-posix:stat (namestring dir))) #o777)))
    ;; The transcript is the organism's memory, and the turn loop's only tool
    ;; is EVAL, so anything that reads it reads the whole history.
    ;; ENSURE-DIRECTORIES-EXIST honours umask, so the mode is set explicitly
    ;; rather than inherited from the operator's shell.
    (is (= mode #o700) (format nil "store directory mode is 0700, got ~o" mode))))

(deftest opening-the-same-path-twice-reuses-the-handle (with-temp-store (store))
  (is (eq store (nlk:open-store :path store.path)) "second open returns the same store"))

(deftest a-refused-fixture-does-not-close-the-open-store (with-temp-store (store))
  ;; The refusal alone is not enough: the fixture's cleanup used to close the
  ;; default *STORE* unconditionally, displacing through the back door what
  ;; OPEN-STORE had just refused to displace through the front.
  (signals-error nlk::store-already-open
    (with-temp-store () t))
  (is (nlk::store-open-p store) "the outer store survives the inner fixture")
  (is (eq store nlk::*store*) "*store* still names the outer store"))

(deftest run-all-refuses-inside-a-live-image (with-temp-store (store))
  ;; The suite is destructive to its host — global config SETFs, evolved
  ;; package rebuild — so the runner takes the displacement posture one seam
  ;; earlier: an open store means a serving image, and RUN-ALL must refuse
  ;; before touching anything. The temp store stands in for the live one.
  (signals-error simple-error (run-all))
  (is (nlk::store-open-p store) "the store survives the refusal"))

(deftest closing-is-idempotent (with-temp-store (store))
  (nlk:close-store store)
  (is (not (nlk::store-open-p store)) "closed store reports not open")
  (is (progn (nlk:close-store store) t) "second close does not signal"))

(deftest a-closed-store-refuses-reads-and-writes (with-temp-store ())
  (nlk:close-store)
  (is (signals-error nlk::store-not-open
          (nlk:record-event "s" "probe" (list :n 1))))
  (is (signals-error nlk::store-not-open (nlk:page-events))))

(deftest a-live-owner-lock-refuses-a-second-store-open ()
  ;; Ownership is an fcntl record lock, which is per-process, so a foreign
  ;; holder needs a real second process. python3 takes the same lock the
  ;; organism takes; the sidecar it never writes proves the refusal comes
  ;; from the lock, not from holder metadata.
  (with-temp-directory (dir "store-lease-live")
    (let ((path (merge-pathnames "history.db" dir)))
      (with-lock-holder ((nlk::lock-path-for-store path))
        (is (signals-error nlk::store-in-use (nlk:open-store :path path)))
        (is (nlk::store-lease-held-p path))))))

(deftest a-dead-owners-debris-does-not-block-open ()
  ;; The kernel released the dead owner's lock with its process, so staleness
  ;; is only ever sidecar debris: a leftover holder file and lock file must
  ;; not stop the next owner, and the holder is rewritten to name it.
  (with-temp-directory (dir "store-lease-stale" :close-store t)
    (let* ((path (merge-pathnames "history.db" dir))
           (holder-path (nlk::lease-path-for-store path)))
      (write-temp-file holder-path "{\"pid\":999999999,\"port\":42999}")
      (write-temp-file (nlk::lock-path-for-store path))
      (is (not (nlk::store-lease-held-p path)))
      (let ((store (nlk:open-store :path path)))
        (is (nlk:store-open-p store) "dead owner's debris is replaced")
        (is-values (pid port) (nlk::read-store-holder path) (pid = (sb-posix:getpid))
          (port null "no endpoint is inherited from the dead owner")))
      (nlk:close-store)
      (is (not (probe-file holder-path)))
      (is (not (nlk::store-lease-held-p path))))))

(deftest a-file-sqlite-refuses-is-a-store-that-cannot-be-read ()
  ;; what a crash or a full disk leaves at the store's path, garbage or a
  ;; damaged page: SQLite's NOTADB and CORRUPT come out as one sentence naming
  ;; the file and what to do, and the lease goes with the failed open (CB-102)
  (with-temp-directory (dir "store-unreadable")
    (let ((path (merge-pathnames "history.db" dir)))
      (flet ((refusal (bytes)
               (alexandria:write-byte-vector-into-file bytes path :if-exists :supersede)
               (refusal-text nlk::store-unreadable (nlk:open-store :path path) (nlk:close-store))))
        (is-carrying (text (refusal (make-array 8000 :element-type '(unsigned-byte 8) :initial-element 171)))
          (:= (format nil "~a is not a nodecode store (not an SQLite database); move it aside and run ~
                           nodecode again"
                      (namestring path))))
        (is (not (nlk::store-lease-held-p path)) "the lease is let go")
        (is (not (probe-file (nlk::lease-path-for-store path))) "and its holder sidecar")
        ;; a real store, then its first page past the header scribbled over
        (delete-file path)
        (let ((db (sqlite:connect (namestring path))))
          (sqlite:execute-non-query db "create table t (x)")
          (sqlite:disconnect db))
        (let ((bytes (nlk::file-octets path)))
          (loop for at from 100 below (length bytes) do (setf (aref bytes at) (mod (* at 131) 256)))
          (is-carrying (text (refusal bytes))
            "(database disk image is malformed); move it aside and run nodecode again"
            (:absent "Code " "no SQLite condition text")))))))

(deftest the-owner-asking-about-its-own-store-keeps-the-lease (with-temp-store (store))
  ;; The ownership probe must never cost the owner its lease. POSIX drops
  ;; every record lock a process holds on a file when it closes any
  ;; descriptor for it, so FILE-LOCKED-P answers a lock of this process's
  ;; own from the locks it took (os.lisp) rather than opening the file.
  ;; Without that, a shell that owned the store lost it by asking after it,
  ;; and a second organism opened the same database.
  (let ((path store.path))
    (is (nlk::store-lease-held-p path) "the owner reads its own lease as held")
    (dotimes (i 5) (nlk::store-death-verdict path))
    (multiple-value-bind (verdict pid) (nlk::store-death-verdict path)
      (is (eq :live verdict) "its own store reads live, however often it asks")
      (is (eql (sb-posix:getpid) pid) "and names the owner"))
    (is (nlk::store-lease-held-p path) "the lease outlives every probe")))

(deftest store-death-verdict-classifies-ownership-debris ()
  ;; The recovery decision seam: the lock is the authority, the sidecar is
  ;; testimony. Only the dead-owner's-debris state (sidecar under a free
  ;; lock) licenses a client's automatic resurrection; a missing sidecar
  ;; means the owner closed orderly and must stay down.
  (with-temp-directory (dir "store-death-verdict")
    (let* ((path (merge-pathnames "history.db" dir))
           (holder-path (nlk::lease-path-for-store path))
           (lock (nlk::lock-path-for-store path)))
      (is (eq :clean-shutdown (nlk::store-death-verdict path)))
      (write-temp-file lock)
      (is (eq :clean-shutdown (nlk::store-death-verdict path)))
      (write-temp-file holder-path "{\"pid\":999999999,\"port\":42999}")
      (is-values (verdict pid port) (nlk::store-death-verdict path) (verdict eq :dirty-death)
        (pid eql 999999999 "the dead owner is named")
        (port eql 42999 "the dead owner's port is carried"))
      (with-lock-holder (lock)
        (is (eq :live (nlk::store-death-verdict path)))))))

(deftest the-owner-publishes-and-retracts-its-gateway-endpoint (with-temp-store (store))
  ;; PUBLISH-STORE-ENDPOINT is the lease.zig publishEndpoint seam: the port a
  ;; contender attaches to instead of dying on the ownership lock.
  (multiple-value-bind (pid port) (nlk::read-store-holder store.path)
    (is (= (sb-posix:getpid) pid) "the holder names the owner on open")
    (is (null port) "no endpoint before a gateway binds"))
  (nlk::publish-store-endpoint 43125)
  (is (eql 43125 (nth-value 1 (nlk::read-store-holder store.path))))
  (nlk::publish-store-endpoint nil)
  (is (null (nth-value 1 (nlk::read-store-holder store.path)))))

(deftest the-save-hook-is-installed-so-a-snapshot-cannot-carry-a-handle (with-temp-store ())
  ;; Hazard 1 in store.lisp's header: a core saved with an open handle
  ;; segfaults in SQLITE::PREPARE-STATEMENT after restore. Asserting the hook
  ;; is present is cheap; A6 is where a saved core is actually exercised.
  (is nlk::*save-hook-installed* "save hook recorded as installed")
  (is (plusp (length sb-ext:*save-hooks*)) "a save hook is registered"))

;;; --- durable facts ------------------------------------------------------

;;; --- durable facts: event log ------------------------------------------
;;; Messages and turns tables are gone; the event log is the single durable
;;; authority. These tests cover the event-log path that replaced them.

(deftest tool-events-round-trip-through-the-log (with-admitted-turn (turn "s-1" "cmd-tools" "eval"))
  (record-tool-use turn "call-99" "eval" "{\"form\":\"(+ 1 2)\"}" "3")
  (let ((events (session-events "s-1" :type '(or nlk:tool-call-event nlk:tool-result-event))))
    (is (= 2 (length events)) "two tool events")
    (let ((tc (first events))
          (tr (second events)))
      (is (typep tc 'nlk:tool-call-event) "first is tool-call-event")
      (is (typep tr 'nlk:tool-result-event) "second is tool-result-event")
      (is (equal "call-99" (nlk:tool-call-event-call-id tc)) "call-id round-trips")
      (is (equal "eval" (nlk:tool-call-event-tool-name tc)) "tool-name round-trips")
      (is (search "form" (nlk:tool-call-event-arguments tc)) "arguments round-trip")
      (is (equal "3" (nlk:tool-result-event-result tr)) "result round-trips"))))

;;; --- paging -------------------------------------------------------------

;;; --- paging -------------------------------------------------------------

(deftest paging-events-walks-forward-by-seq-without-rescanning (with-session-store ("s-1"))
  (dotimes (i 25)
    (nlk:record-event "s-1" "probe" (list :n i)))
  (let* ((first-page (nlk:page-events :session-id "s-1" :limit 10))
         (cursor (nlk:event-log-position (car (last first-page))))
         (second-page (nlk:page-events :session-id "s-1" :after cursor :limit 10)))
    (is (= 10 (length first-page)) "first page is full")
    (is (= 10 (length second-page)) "second page is full")
    (is (> (nlk:event-log-position (car second-page)) cursor) "second page starts after the cursor")
    (is (= 26 (nlk:event-count :session-id "s-1")) "create plus 25 messages stored")))

(deftest paging-events-filters-by-session (with-session-store ("s-a"))
  (ensure-durable-session "s-b")
  (nlk:record-event "s-a" "probe" (list :n 1))
  (nlk:record-event "s-b" "probe" (list :n 2))
  (nlk:record-event "s-a" "probe" (list :n 3))
  (is (= 3 (length (nlk:page-events :session-id "s-a"))) "create plus two events for s-a")
  (is (= 2 (length (nlk:page-events :session-id "s-b"))) "create plus one event for s-b")
  (is (= 3 (nlk:event-count :session-id "s-a")) "count filters too"))

(deftest an-oversized-page-is-refused (with-temp-store (store))
  (is (signals-error nlk::store-page-too-large
        (nlk:page-events :limit (1+ nlk::+max-page-size+))))
  (is (signals-error type-error (nlk:page-events :limit 0))))

;;; --- the fork hazard ----------------------------------------------------

(deftest closing-in-a-child-does-not-close-the-parents-handle (with-temp-store (store))
  (finish-output)
  (let ((pid (sb-posix:fork)))
    (when (zerop pid)
      ;; CLOSE-STORE in a child must disown rather than disconnect.
      (nlk:close-store)
      (sb-posix:_exit 0))
    (sb-posix:waitpid pid 0))
  (is (nlk::store-open-p) "parent's handle survives a child's CLOSE-STORE")
  (ensure-durable-session "s-1")
  (is (progn (nlk:record-event "s-1" "probe" (list :n 1)) t)))

;;; --- collection: the nursery cap ----------------------------------------

(defmacro with-saved-nursery (&body body)
  "Run BODY with the nursery, every generation's budget and age rule, and the
runtime's release level put back on unwind: the collection gates cap them and
must leave the image as found."
  (let ((nursery (gensym "NURSERY")) (budgets (gensym "BUDGETS")) (ages (gensym "AGES"))
        (limit (gensym "LIMIT")))
    `(let ((,nursery (sb-ext:bytes-consed-between-gcs))
           (,budgets (loop for g from 0 to 5
                           collect (sb-ext:generation-bytes-consed-between-gcs g)))
           (,ages (loop for g from 0 to 5
                        collect (sb-ext:generation-minimum-age-before-gc g)))
           (,limit (sb-alien:extern-alien "small_generation_limit" (sb-alien:signed 8))))
       (unwind-protect (progn ,@body)
         ;; A raise that ended puts its nursery back on a timer of its own,
         ;; shortly after: landing in the next test, it would leave that one
         ;; the nursery this one capped.
         (await (:tries 200 :rest 0.01) (null nlk::*nursery-floor*))
         (setf (sb-ext:bytes-consed-between-gcs) ,nursery
               (sb-alien:extern-alien "small_generation_limit" (sb-alien:signed 8)) ,limit)
         (loop for g from 0 to 5 for budget in ,budgets for age in ,ages
               do (setf (sb-ext:generation-bytes-consed-between-gcs g) budget
                        (sb-ext:generation-minimum-age-before-gc g) age))))))

(defun stop-idle-collection (&aux (timer nlk::*idle-timer*))
  (when timer (sb-ext:unschedule-timer timer) (setf nlk::*idle-timer* nil)))

(defmacro with-idle-collection ((interval) &body body)
  "Run BODY with the idle collector running at INTERVAL, stopped on the way
out however BODY leaves — the stop is what keeps a timer out of the next test."
  `(nlk:with-cleanup ((stop-idle-collection)) (nlk:collect-while-idle ,interval) ,@body))

(deftest capping-the-nursery-returns-the-previous-value (with-saved-nursery)
  (let* ((original (sb-ext:bytes-consed-between-gcs)))
    (is (= (nlk:cap-nursery (* 8 1024 1024)) original) "returns the value it replaced")
    (is (= (* 8 1024 1024) (sb-ext:bytes-consed-between-gcs)) "nursery applied")))

(deftest capping-the-nursery-refuses-a-nonsense-budget ()
  (is (signals-error type-error (nlk:cap-nursery 0)) "zero refused")
  (is (signals-error type-error (nlk:cap-nursery -1)) "negative refused"))

(deftest capping-the-nursery-holds-every-generation-to-the-consing-budget (with-saved-nursery)
  ;; The promoted garbage a generation may hold before it is collected is
  ;; the other half of the resident-size trade: SBCL's own figure is a fifth
  ;; of the nursery, per generation. It is the organism's budget whatever the
  ;; nursery.
  (nlk:cap-nursery (* 8 1024 1024))
  (is (every (lambda (g) (= nlk::*consing-budget*
                            (sb-ext:generation-bytes-consed-between-gcs g)))
             '(0 1 2 3 4 5))))

(defvar *nursery-junk* nil
  "Garbage for the collection tests: held in a special so the compiler keeps it.")

;;; An object a stack slot points to is pinned where it is rather than raised,
;;; so a local would leave it young.
(defun promote-dead-junk (&key (count 2000) full)
  "Promote COUNT small arrays, 10 KB each, and drop them: to the oldest
generation when FULL, what a long turn leaves there; else to generation 1, two
nursery collections on. => that generation's size while they were live."
  (setf *nursery-junk* (loop repeat count
                             collect (make-array 10000 :element-type '(unsigned-byte 8)
                                                       :initial-element 1)))
  (if full (sb-ext:gc :full t) (progn (sb-ext:gc) (sb-ext:gc)))
  (prog1 (sb-ext:generation-bytes-allocated (if full 5 1))
    (setf *nursery-junk* nil)))

(deftest capping-the-nursery-collects-on-the-budget-alone-and-gives-pages-back (with-saved-nursery)
  ;; A turn's data dies young by the clock and old by the collector's count:
  ;; the age rule would keep it past its budget, and a collection below
  ;; generation 2 would keep its pages.
  (nlk:cap-nursery (* 8 1024 1024))
  (is (every (lambda (g) (zerop (sb-ext:generation-minimum-age-before-gc g))) '(0 1 2 3 4 5)))
  #+linux
  (is (zerop (sb-alien:extern-alien "small_generation_limit" (sb-alien:signed 8))) "every collection releases"))

(deftest a-nursery-raise-comes-back-when-the-last-one-ends (with-saved-nursery)
  ;; Two threads that each put back the nursery they found would leave it
  ;; raised for good when they end in the order they began. The last puts it
  ;; back shortly after it ends, on a thread of its own, so its caller does
  ;; not wait on the collection.
  (nlk:cap-nursery (* 2 1024 1024))
  (flet ((comes-back-p ()
           (await (:tries 300 :rest 0.01)
             (and (= (* 2 1024 1024) (sb-ext:bytes-consed-between-gcs))
                  (= (* 2 1024 1024) (sb-ext:generation-bytes-consed-between-gcs 1))))))
    (let* ((nlk::*nursery-restore-delay* 0.3)
           (inside (bt2:make-semaphore))
           (leave (bt2:make-semaphore))
           (raiser (bt2:make-thread (lambda ()
                                      (nlk:with-nursery ((* 32 1024 1024))
                                        (bt2:signal-semaphore inside)
                                        (bt2:wait-on-semaphore leave))))))
      (bt2:wait-on-semaphore inside)
      (nlk:with-nursery ((* 64 1024 1024))
        (is (= (* 64 1024 1024) (sb-ext:bytes-consed-between-gcs)) "the larger ask holds")
        (is (= (* 64 1024 1024) (sb-ext:generation-bytes-consed-between-gcs 1)) "and the generations take as much")
        (bt2:signal-semaphore leave)
        (bt2:join-thread raiser)
        (is (= (* 64 1024 1024) (sb-ext:bytes-consed-between-gcs)) "the first to end puts nothing back"))
      (is (= (* 64 1024 1024) (sb-ext:bytes-consed-between-gcs)) "nor the last, as it returns")
      (is (comes-back-p) "shortly after, the nursery and the budgets the first found come back")
      (is (signals-error simple-error (nlk:with-nursery ((* 32 1024 1024)) (error "the load failed"))))
      (is (comes-back-p) "so do they after a body that unwinds"))))

(deftest a-nursery-raise-moves-the-pending-collection-out (with-saved-nursery)
  ;; The runtime moves its trigger only at a collection: a raise that sets the
  ;; nursery alone still collects within the 2 MB the small nursery leaves free,
  ;; copying what the work has built so far.
  (nlk:cap-nursery (* 2 1024 1024))
  (sb-ext:gc)
  (flet ((room-left ()
           (- (sb-alien:extern-alien "auto_gc_trigger" sb-alien:unsigned-long) (sb-kernel:dynamic-usage))))
    (is (< (room-left) (* 4 1024 1024)))
    (nlk:with-nursery ((* 64 1024 1024))
      (is (> (room-left) (* 60 1024 1024)) "the raise reaches the collection already due"))))

(deftest a-collection-past-the-slack-earns-a-full-walk
    (with-saved-globals (sb-ext:*after-gc-hooks* nlk::*forced-heap-at* (nlk::*heap-slack* (* 4 1024 1024))
                         (nlk::*heap-slack-ratio* 0)))
  ;; Promoted garbage past the slack, then any collection: the walk follows
  ;; on a thread of its own and measures live afresh.
  (sb-ext:gc :full t)
  (setf nlk::*forced-heap-at* (sb-kernel:dynamic-usage))
  (nlk:cap-garbage)
  (let ((before nlk::*forced-heap-at*))
    (promote-dead-junk :count 800)
    (sb-ext:gc)
    (is (await (:rest 0.05) (/= before nlk::*forced-heap-at*)) "the walk ran")
    (is (< (- (sb-kernel:dynamic-usage) nlk::*forced-heap-at*) nlk::*heap-slack*) "and took the garbage")))

(deftest a-slack-walk-waits-for-the-operator-to-rest
    (with-saved-globals (sb-ext:*after-gc-hooks* nlk::*forced-heap-at* nlk::*idle-input-at*
                         (nlk::*heap-slack* (* 4 1024 1024)) (nlk::*heap-slack-ratio* 0)
                         (nlk::*slack-walk-rest* 1/2)))
  ;; A walk stops every thread, and one inside a drag or a scroll is a stall
  ;; the operator watches: while keys or the pointer keep coming it waits,
  ;; and it walks once they rest.
  (sb-ext:gc :full t)
  (nlk:cap-garbage)
  (let ((before nlk::*forced-heap-at*)
        (until (+ (get-internal-real-time) (floor internal-time-units-per-second 3))))
    ;; The input is coming before the junk is: its allocation collects on
    ;; its own under a nursery smaller than it (a capped one).
    (nlk:note-operator-input)
    (promote-dead-junk :count 800)
    (nlk:note-operator-input)
    (sb-ext:gc)
    (loop while (< (get-internal-real-time) until)
          do (nlk:note-operator-input) (sleep 0.02))
    (is (= before nlk::*forced-heap-at*) "no walk while the input kept coming")
    (is (await (:rest 0.05) (/= before nlk::*forced-heap-at*)) "the walk once it rested")))

(deftest the-card-marks-past-the-heap-go-back-and-the-heap-stays-whole ()
  ;; A released mark reads CARD_MARKED, the state the runtime filled with, so
  ;; an old object pointing at a young one is still scanned and kept.
  #+(and linux (or x86-64 arm64))
  (let ((old (make-array 10 :initial-element nil)))
    (sb-ext:gc :full t)
    (setf (aref old 0) (list 1 2 3))
    (is (plusp (nlk:release-card-marks)) "marks released")
    (sb-ext:gc)
    (sb-ext:gc :full t)
    (is (equal '(1 2 3) (aref old 0)) "the young object survived"))
  #-(and linux (or x86-64 arm64))
  (is (null (nlk:release-card-marks)) "no release off Linux"))

(deftest an-idle-organism-collects-the-generation-that-outgrew-its-budget (with-saved-nursery)
  ;; Promote a soon-dead structure into generation 1 past its budget, stop
  ;; allocating, and the idle timer collects it within two intervals.
  (let ((count nlk::*idle-collections*)
        (releases nlk::*image-releases*)
        (promotion (sb-ext:generation-number-of-gcs-before-promotion 1)))
    (sb-ext:gc :full t)
    (nlk:cap-nursery (* 8 1024 1024))
    ;; At a budget of *CONSING-BUDGET*, the collection that promotes the junk
    ;; into generation 1 finds it over budget and collects it in the same
    ;; call, and whether that carries it on to generation 2 is how many
    ;; collections generation 1 has had -- the heap the suite left. Pinned,
    ;; generation 1 keeps what it takes in until a collection names it, and
    ;; a full walk first empties it and zeroes every generation's count.
    (setf (sb-ext:generation-number-of-gcs-before-promotion 1) 1000)
    (unwind-protect
         (with-idle-collection (0.2)
           ;; Held by a special: a binding nothing reads again is dead to
           ;; the collector, and the junk would never live to be promoted.
           (setf *nursery-junk* (loop repeat 2000
                                      collect (make-array 10000 :element-type '(unsigned-byte 8)
                                                                :initial-element 1)))
           ;; Two nursery collections: the first keeps survivors young, the
           ;; second promotes them. Many small objects rather than one large
           ;; one, which the allocator would put on pages of its own.
           (sb-ext:gc) (sb-ext:gc)
           (is (> (sb-ext:generation-bytes-allocated 1) (* 8 1024 1024)))
           (setf *nursery-junk* nil)
           (let ((held (sb-ext:generation-bytes-allocated 1)))
             ;; Long enough for the collection, and for the quiet stretch the
             ;; image-page release rides.
             (sleep 1.4)
             (is (> nlk::*idle-collections* count) "the idle timer collected")
             (is (< (sb-ext:generation-bytes-allocated 1) held)))
           (is (nth-value 1 (get-properties (nlk::collection-stats) '(:idle-collections))))
           (is (integerp (getf (nlk::collection-stats) :image-bytes)))
           ;; The quiet gave the image's pages back. How many stay resident is
           ;; the host's: MADV_PAGEOUT passes over a page another process maps
           ;; too, and every SBCL on the machine maps the same core.
           (is (> nlk::*image-releases* releases) "the quiet released the image's pages"))
      (setf (sb-ext:generation-number-of-gcs-before-promotion 1) promotion))))

(deftest a-long-enough-quiet-earns-the-full-walk (with-saved-nursery)
  ;; The generational collection leaves garbage a young generation holds for
  ;; an older one that is dead too. Only a full walk finds it, and only a
  ;; quiet that has lasted is worth one.
  (with-saved-globals ((nlk::*forced-heap-at* 0) (nlk::*idle-checks* 0)
                       (nlk::*idle-input-at* nil))
    ;; No operator at the keys: a key inside the interval is work.
    (nlk:cap-nursery (* 8 1024 1024))
    (with-idle-collection (0.1)
      (let ((collections nlk::*idle-collections*))
        ;; Four checks of quiet are not enough for the walk.
        (sleep 0.35)
        (is (< nlk::*idle-checks* nlk::*idle-checks-before-full*))
        (sleep 0.5)
        (is (>= nlk::*idle-checks* nlk::*idle-checks-before-full*))
        (is (> nlk::*idle-collections* collections) "the walk ran")
        (is (< (- (sb-kernel:dynamic-usage) nlk::*forced-heap-at*)
               (* 8 1024 1024)))))))

(deftest work-in-the-interval-is-not-quiet
    (with-saved-globals ((nlk::*idle-checks* 4) (nlk::*idle-input-at* nil) *nursery-junk*))
  (with-idle-collection (0.1)
    ;; Cons through two checks: each one finds the organism working. The list
    ;; goes to a special: a binding nothing reads, the compiler never conses.
    (dotimes (round 2)
      (setf *nursery-junk* (make-list 200000))
      (sleep 0.12))
    (is (< nlk::*idle-checks* 4) "consing zeroes the count of quiet checks")))

(deftest the-idle-collector-arms-again-in-an-image-whose-dump-unscheduled-it ()
  ;; A warm image is dumped with every timer unscheduled and *IDLE-TIMER*
  ;; still naming one: MAIN's arming must schedule a fresh one, or no warm
  ;; launch ever collects while idle.
  (nlk:with-cleanup ((stop-idle-collection))
    (nlk:collect-while-idle 60)
    (sb-ext:unschedule-timer nlk::*idle-timer*)
    (is (sb-ext:timer-scheduled-p (nlk:collect-while-idle 60)) "armed again")))

(deftest a-long-quiet-empties-the-nursery-to-a-collection-that-returns-its-pages
    (with-saved-globals ((nlk::*idle-checks* 4) nlk::*idle-consed-at* (nlk::*idle-input-at* nil)
                         nlk::*idle-consing*))
  ;; Without the fifth quiet check, a nursery emptied by its own collections
  ;; keeps its pages. That check collects it at a level that hands them back.
  (sb-ext:gc)
  (setf *nursery-junk* (make-array (* 6 1024 1024) :element-type '(unsigned-byte 8))
        *nursery-junk* nil
        nlk::*idle-consing* most-positive-fixnum
        nlk::*idle-consed-at* (sb-ext:get-bytes-consed))
  (is (> (sb-ext:generation-bytes-allocated 0) nlk::*consing-budget*) "the nursery holds garbage")
  (nlk::collect-when-idle)
  (is (< (sb-ext:generation-bytes-allocated 0) nlk::*consing-budget*) "the quiet emptied it"))

(deftest no-key-yet-is-quiet-from-the-first-check
    (with-saved-globals ((nlk::*idle-checks* 0) nlk::*idle-consed-at* (nlk::*idle-input-at* nil)))
  ;; Internal real time counts from the process's start: an input stamp of 0
  ;; read as a key at launch, so a fresh process held off its idle collector
  ;; for its first *IDLE-INTERVAL* with no operator there. No stamp is quiet.
  (setf nlk::*idle-consed-at* (sb-ext:get-bytes-consed))
  (nlk::collect-when-idle)
  (is (= 1 nlk::*idle-checks*) "a process no key has reached is quiet"))

(deftest operator-input-in-the-interval-is-not-quiet
    (with-saved-globals ((nlk::*idle-checks* 4) nlk::*idle-consed-at* nlk::*idle-input-at*))
  ;; A keystroke is work too: a collection is a stop-the-world pause, and
  ;; one behind a burst of keys stalls the frame the operator waits for.
  (let ((collections nlk::*idle-collections*))
    (setf nlk::*idle-consed-at* (sb-ext:get-bytes-consed))
    (nlk:note-operator-input)
    (nlk::collect-when-idle)
    (is (zerop nlk::*idle-checks*) "a key inside the interval zeroes the count")
    (is (= collections nlk::*idle-collections*) "and nothing is collected")
    (setf nlk::*idle-input-at* (- (get-internal-real-time)
                                  (* 2 nlk::*idle-interval*
                                     internal-time-units-per-second))
          nlk::*idle-consed-at* (sb-ext:get-bytes-consed))
    (nlk::collect-when-idle)
    (is (= 1 nlk::*idle-checks*) "a key older than the interval is quiet again")))

(deftest trimming-memory-gives-pages-back-and-reports-both-sides ()
  ;; The image's mapped pages are the part no collection touches: this test
  ;; image has read most of them, and the trim hands them back to the kernel.
  (let ((count nlk::*forced-collections*))
    (multiple-value-bind (before after) (nlk:trim-memory)
      #+linux (progn
                (is (integerp before) "resident bytes read before")
                (is (integerp after) "resident bytes read after")
                (is (< after before) "the process is smaller after the trim"))
      #-linux (is (and (null before) (null after)) "no residency figure off Linux")
      (is (= count nlk::*forced-collections*))
      (is (equal (list :before before :after after) nlk::*last-trim*))
      (is (nth-value 1 (get-properties (nlk::collection-stats) '(:last-trim)))))))

;;; --- collection: scheduling into the wait --------------------------------

(deftest a-forced-collection-fires-inside-a-blocking-call (let ((before nlk::*forced-collections*)))
  ;; The mechanism, proven against a deterministic sleep rather than the network:
  ;; the timer must fire while the body is still blocked, in this thread, and
  ;; the body's value comes back through the macro.
  (is (eq :answered (nlk:collect-during-wait (sleep 0.4) :answered)) "the body's value")
  (is (> nlk::*forced-collections* before) "a collection completed during the body")
  (is (numberp nlk::*last-gc-pause*) "pause was measured")
  (is (< nlk::*last-gc-pause* 0.4) "pause is shorter than the wait it hid inside"))

(deftest a-forced-collection-reaches-the-generation-that-outgrew-its-budget (with-saved-nursery)
  ;; What a long turn promotes dies in the oldest generation, and SBCL's own
  ;; cascade never walks there: the provider wait does. Promote soon-dead
  ;; data to generation 5, drop it, and one forced collection reclaims it.
  (let ((baseline (copy-seq nlk::*generation-baseline*))
        (count nlk::*forced-collections*))
    (nlk:with-cleanup ((replace nlk::*generation-baseline* baseline))
      (with-saved-globals (nlk::*forced-consed-at* nlk::*forced-heap-at*)
        (nlk:cap-nursery (* 8 1024 1024))
        (let ((held (promote-dead-junk :full t)))
          (is (> held (* 8 1024 1024)))
          (setf (svref nlk::*generation-baseline* 5) 0
                nlk::*forced-consed-at* 0
                nlk::*forced-heap-at* 0)
          (is (eql 5 (nlk::overgrown-generation)) "the oldest generation is the overgrown one")
          (nlk::force-collection)
          (is (> nlk::*forced-collections* count) "the wait's collection ran")
          (is (< (sb-ext:generation-bytes-allocated 5) (- held (* 8 1024 1024))))
          (is (null (nlk::overgrown-generation)) "which the rule now counts as collected")
          (is (plusp nlk::*forced-heap-at*)))))))

(deftest scheduling-a-collection-uses-a-normal-thread (let ((caller (bt2:current-thread))))
  ;; The callback must not interrupt the provider or UI thread. :THREAD T
  ;; creates a short-lived ordinary thread for the timer callback; it is gone
  ;; by the time the wait returns.
  (nlk:collect-during-wait (sleep 0.2))
  (is (not (eq caller nlk::*forced-collection-thread*))))

(deftest the-timer-does-not-outlive-a-failed-body (let ((fired-before nlk::*forced-collections*)))
  ;; A provider error is ordinary. A timer left armed would fire into the next
  ;; request, so the macro must unschedule on the error path too.
  (handler-case (nlk:collect-during-wait (error "provider blew up")) (error () nil))
  (sleep 0.15)
  (is (= fired-before nlk::*forced-collections*)))

;;; --- collection: the fitness input --------------------------------------

(deftest gc-run-time-is-readable-as-a-fitness-input ()
  (nlk:collect-during-wait (sleep 0.2))
  (let ((reported (nlk:gc-run-time)))
    (is (integerp reported) "gc-run-time is an integer")
    (is (plusp reported) "collection time has accumulated after a forced collection")
    (is (= reported sb-ext:*gc-run-time*) "reports SBCL's own counter, not a copy")))

(deftest collection-stats-are-a-single-snapshot (let ((stats (nlk::collection-stats))))
  ;; Present, not set: :LAST-PAUSE stays NIL until a forced collection has
  ;; run, and which test ran one first is the suite's order.
  (dolist (key '(:gc-run-time :last-pause :forced-collections :nursery :heap-bytes))
    (is (get-properties stats (list key))
        (format nil "~a present in the snapshot" key)))
  (is (integerp (getf stats :heap-bytes)) "heap-bytes is a number")
  (is (plusp (getf stats :nursery)) "nursery is positive"))

;;; --- collection: attribution to visible work -----------------------------

(defmacro labelled-span (label &body body)
  "WITH-GC-ATTRIBUTION over BODY with *ATTRIBUTION-LABEL* bound to LABEL."
  `(let ((nlk:*attribution-label* ,label))
     (nlk:with-gc-attribution ,@body)))

(defmacro with-clean-attribution (&body body)
  "BODY over a gc-attribution window just reset."
  `(progn (nlk:reset-gc-attribution) ,@body))

(deftest a-collection-inside-a-span-is-attributed-to-it (with-clean-attribution)
  (labelled-span :collected-pass (sb-ext:gc :full t))
  (let ((stats (nlk:gc-attribution-stats)))
    (is (= 1 (getf stats :spans)) "one span observed")
    (is (= 1 (getf stats :gc-spans)) "the collection was blamed on it")
    (is (plusp (getf stats :gc-time)) "collection time accumulated")
    (is (<= (getf stats :gc-worst) (getf stats :gc-time)))
    (is (eq :collected-pass (getf stats :gc-worst-label)) "the worst collected span's own label")))

(deftest a-quiet-span-is-counted-but-not-blamed (with-clean-attribution)
  ;; A body that conses nothing cannot trigger a collection, so the span
  ;; must land in :SPANS and stay out of :GC-SPANS; its values pass through.
  (is (equal '(1 2) (multiple-value-list (nlk:with-gc-attribution (values 1 2)))))
  (let ((stats (nlk:gc-attribution-stats)))
    (is (= 1 (getf stats :spans)) "the span was counted")
    (is (zerop (getf stats :gc-spans)) "no collection, no blame")))

(deftest attribution-records-a-span-that-exits-sideways (with-clean-attribution)
  ;; UPDATE leaves through RETURN-FROM on the quit path; the spans that
  ;; stall longest are the likeliest to exit non-locally, so recording must
  ;; ride the unwind.
  (block outer (nlk:with-gc-attribution (return-from outer :gone)))
  (is (= 1 (getf (nlk:gc-attribution-stats) :spans))))

(deftest the-worst-span-carries-the-label-it-ran-under (with-clean-attribution)
  ;; The label turns "something took 122ms" into "which kind of pass did":
  ;; only the span that sets a new worst may write it, so a later shorter
  ;; span must not steal the blame.
  (labelled-span :heavy (sleep 0.02))
  (labelled-span :light 1)
  (is (eq :heavy (getf (nlk:gc-attribution-stats) :span-worst-label))))

(deftest the-by-label-table-accumulates-per-kind (with-clean-attribution)
  (labelled-span :typing 1)
  (labelled-span :typing 2)
  (labelled-span :painting (sleep 0.01))
  (is-let* ((rows (getf (nlk:gc-attribution-stats) :by-label))
            (typing (assoc :typing rows))
            (painting (assoc :painting rows)))
    ((second typing) = 2 "two typing spans counted")
    ((second painting) = 1 "one painting span counted") ((first (first rows)) eq :painting)))

(deftest the-worst-span-records-cpu-next-to-wall (with-clean-attribution)
  ;; A sleeping span burns wall but no CPU — the preemption signature the
  ;; cpu column exists to expose.
  (nlk:with-gc-attribution (sleep 0.03))
  (let ((stats (nlk:gc-attribution-stats)))
    (is (plusp (getf stats :span-worst)) "wall time recorded")
    (is (< (getf stats :span-worst-cpu) (getf stats :span-worst)))))

(deftest resetting-the-window-zeroes-every-counter ()
  (labelled-span :stale (sb-ext:gc))
  (nlk:reset-gc-attribution)
  (let ((stats (nlk:gc-attribution-stats)))
    (is-table (in) (zerop in) ((getf stats :spans) "spans zeroed")
      ((getf stats :gc-spans) "blamed spans zeroed") ((getf stats :span-time) "span time zeroed")
      ((getf stats :span-worst-cpu) "worst cpu zeroed"))
    (is-shape stats (:span-worst-label null "worst label cleared")
      (:gc-worst-label null "gc worst label cleared") (:by-label null "label table emptied"))))

;;; --- the gate: a flat heap across a long transcript ---------------------

(deftest live-heap-stays-flat-across-100-recorded-turns (with-temp-store ())
  (let ((session (intern-durable-session "heap-flat")))
    ;; Warm up, then measure the live set after a full collection so the number
    ;; is a live-set measurement rather than an allocation high-water mark.
    (dotimes (i 5)
      (nle::append-message session (nle::message "user" (format nil "warmup ~a" i)))
      (nlk:record-event (nle:session-id session) "probe"
                        (list :n i :role "user")))
    (sb-ext:gc :full t)
    (let ((baseline (sb-kernel:dynamic-usage)))
      (dotimes (i 100)
        ;; Each turn appends to in-heap history (bounded by session.lisp) and
        ;; records durable events (unbounded, off-heap). If history accumulated
        ;; in the image, this loop is where the heap would grow.
        (nle::append-message session (nle::message "user" (format nil "~a: ~a" i (make-string 400 :initial-element #\x))))
        (nle::append-message session (nle::message "assistant" (format nil "reply ~a" i)))
        (nlk:record-event (nle:session-id session) "probe"
                          (list :n i :role "user"))
        (nlk:record-event (nle:session-id session) "probe"
                          (list :n i :role "assistant")))
      (sb-ext:gc :full t)
      (let* ((after (sb-kernel:dynamic-usage))
             (growth (- after baseline))
             (ceiling (* 8 1024 1024)))
        (is (= 206 (nlk:event-count :session-id (nle:session-id session))))
        (is (<= (length session.messages) nle::*max-history-messages*))
        (format t "       heap: baseline ~:D B, after 100 turns ~:D B, growth ~:D B (ceiling ~:D B)~%"
                baseline after growth ceiling)
        (is (< growth ceiling)
            (format nil "live heap grew ~:D bytes across 100 turns, ceiling ~:D" growth ceiling))))))

(deftest a-hundred-events-page-back-out-in-order (with-session-store ("s-1"))
  (dotimes (i 100)
    (nlk:record-event "s-1" "probe" (list :n i)))
  ;; Reading the whole event log back through pages proves bounding the heap
  ;; costs nothing: history is still reachable off-heap.
  (let ((seen 0) (cursor 0) (last-seq 0) (ordered t))
    (loop for page = (nlk:page-events :session-id "s-1" :after cursor :limit 25)
          while page
          do (dolist (event page)
               (unless (> (nlk:event-log-position event) last-seq) (setf ordered nil))
               (setf last-seq (nlk:event-log-position event))
               (incf seen))
             (setf cursor last-seq))
    (is (= 101 seen) "create plus every message paged back out")
    (is ordered "pages came back in strictly increasing seq order")))

;;; --- live: the pause lands inside provider wait -------------------------

(deftest live-collection-lands-inside-provider-wait ()
  (if (not (live-p))
      (is t "skipped: set NODECODE_LIVE=1 to measure GC against a real provider")
      (with-temp-store ()
        (let ((session (intern-durable-session "live-gc"))
              (before nlk::*forced-collections*))
          ;; Cons enough that a full collection has real work, so the measured
          ;; pause is not trivially zero.
          (let ((junk (loop repeat 120 collect (make-array 20000))))
            (declare (ignorable junk))
            (let* ((start (get-internal-real-time))
                   (answer (nlk:collect-during-wait
                             (nle:run "Reply with the single word: ok" :session session)))
                   (elapsed (/ (- (get-internal-real-time) start)
                               (coerce internal-time-units-per-second 'double-float))))
              (is (stringp answer) "the provider answered")
              (is (> nlk::*forced-collections* before) "a collection fired during the request")
              (let ((pause nlk::*last-gc-pause*))
                (format t "       gc pause ~,1f ms inside a ~,1f ms provider round trip~%"
                        (* 1000 pause) (* 1000 elapsed))
                ;; The claim is containment: the pause happened while the organism
                ;; was blocked anyway, so it cost no visible time.
                (is (< pause elapsed) "the pause fits inside the request it hid in")
                (is (< (/ pause elapsed) 0.5)))
              ;; And the turn it hid inside is durable afterwards: the event log
              ;; holds at least the user-message and assistant-message events.
              (is (plusp (nlk:event-count :session-id (nle:session-id session))))))))))

;;; --- gate: one connection, one lock --------------------------------------
;;; S-cluster regression seams. The image shares a single SQLite connection
;;; across the peer-writer, WS-reader, turn, and boot threads: reads must
;;; serialize with writes on *STORE-LOCK*, an open transaction must be
;;; invisible to every other thread, and a hook failure during OPEN-STORE
;;; must never release ownership while the handle lives.

(deftest concurrent-readers-and-writers-share-one-connection (with-session-store ("conc-anchor"))
  (let ((errors '())
        (errors-lock (bt2:make-lock :name "conc-errors")))
    (flet ((spawn (work)
             (bt2:make-thread
              (lambda ()
                (handler-case (funcall work)
                  (error (condition)
                    (bt2:with-lock-held (errors-lock)
                      (push (princ-to-string condition) errors))))))))
      (mapc #'bt2:join-thread
            (nconc
             (loop for w from 0 below 2
                   collect (let ((w w))
                             (spawn (lambda ()
                                      (dotimes (i 30)
                                        (nlk:create-session
                                         :id (format nil "conc-w~d-~d" w i)))))))
             (loop repeat 2
                   collect (spawn (lambda ()
                                    (dotimes (i 60)
                                      (nlk:page-events :limit 50)
                                      (nlk:active-turn-p "conc-anchor")
                                      (nlk:event-count))))))))
    (is (null errors)
        (format nil "no thread signalled, got: ~{~a~^ | ~}" errors))
    (is (= 61 (nlk:event-count)))))

(deftest open-transactions-are-invisible-to-other-threads (with-temp-store (store))
  (nlk:create-session :id "iso-sess")
  (let ((in-txn (bt2:make-semaphore :name "iso-in-txn"))
        (observed-active nil)
        (reader nil))
    (nlk:with-handlers ((error () nil))
      (nlk::with-store-transaction (handle)
        (nlk::with-event-owner 'nlk::admit-turn
          (nlk::append-event-row
           handle "iso-sess" nlk::+kind-turn-started+
           (list :input "dirty")
           :turn-id "iso-turn" :command-id "iso-cmd"))
        ;; The row is written but uncommitted. A reader starting now
        ;; must block on the store lock, never observe this row.
        (setf reader
              (bt2:make-thread
               (lambda ()
                 (bt2:signal-semaphore in-txn)
                 (when (nlk:active-turn-p "iso-sess")
                   (setf observed-active t)))))
        (bt2:wait-on-semaphore in-txn)
        (sleep 0.1)
        (error "abort the transaction")))
    (when reader (bt2:join-thread reader))
    (is (not observed-active))
    (is (not (nlk:active-turn-p "iso-sess")))))

(deftest store-connection-carries-a-busy-timeout (with-temp-store (store))
  (nlk::with-store-handle (handle)
    (is (eql 5000 (sqlite:execute-single handle "pragma busy_timeout")))))

(deftest open-store-hook-failure-releases-ownership-cleanly ()
  (with-temp-directory (dir "hookfail" :close-store t)
    (let ((path (merge-pathnames "history.db" dir))
          (hook (lambda () (error "synthetic open-hook failure"))))
      ;; Open hooks perform durable writes, so the failure path must
      ;; close the handle BEFORE releasing the lease: releasing first
      ;; would let a second process acquire the store under a live
      ;; writer.
      (with-saved-globals ((nlk::*store-open-hooks* (cons hook nlk::*store-open-hooks*)))
        (is (signals-error error (nlk:open-store :path path))))
      (is (null nlk::*store*) "no live store survives the failed open")
      (is (not (nlk::store-lease-held-p path)))
      (is (not (probe-file (nlk::lease-path-for-store path))))
      (is (nlk::store-open-p (nlk:open-store :path path))))))

;;; --- the connection guards itself ----------------------------------------
;;; Without this, a snippet would loop EXECUTE-TO-LIST on the bare handle from its
;;; own thread, no lock, and an EVAL-INTERRUPT that stops it would unwind the
;;; thread out of a glibc frame, leaving the connection's mutex locked with no
;;; owner. Every statement in the image would then wait on it forever.

(defun connection-guard-facts (db)
  "Inside CALL-GUARDING-CONNECTION on DB: (STORE-LOCK-HELD-P INTERRUPTS-ENABLED-P)."
  (nlk::call-guarding-connection
   db (lambda () (list (sb-thread:holding-mutex-p (bt2:lock-native-lock nlk::*store-lock*))
                       sb-sys:*interrupts-enabled*))))

(deftest store-guard-holds-the-lock-for-the-store-connection-only (with-temp-store ())
  (is (equal '(t nil) (connection-guard-facts (nlk::require-open-store))))
  (let ((other (sqlite:connect ":memory:")))
    (nlk:with-cleanup ((sqlite:disconnect other))
      (is (equal '(nil nil) (connection-guard-facts other))))))

(deftest store-guard-advises-every-connection-entry ()
  (loop for (name) in nlk::*guarded-connection-entries*
        do (is (sb-int:encapsulated-p name 'nlk::store-guard)
               (format nil "~s carries the store guard" name))))

(deftest sql-rows-reads-each-column-as-cl-sqlite-does (with-temp-store ())
  ;; The store's row reader answers cl-sqlite's own values, of cl-sqlite's own
  ;; types, for every kind of column SQLite keeps.
  (let ((handle (nlk::require-open-store))
        (select "select null, 9007199254740993, -0.5, 'turn-1', 'café 中 😀', '',
                        cast('{\"k\":1}' as blob), x'', ?
                 union all select 1, 2, 3.0, 'a', 'b', 'c', x'00ff', x'01', ?"))
    (flet ((same (rows other)
             (and (= 2 (length rows))
                  (equalp rows other)
                  (equal (mapcar #'type-of (reduce #'append rows))
                         (mapcar #'type-of (reduce #'append other))))))
      (is (same (sqlite:execute-to-list handle select "x" "y")
                (nlk::sql-rows handle select "x" "y")))
      (is (null (nlk::sql-rows handle "select 1 where 0"))))))

(deftest store-guard-lands-an-interruption-after-the-access (with-temp-store ())
  (let* ((handle (nlk::require-open-store))
         (log '()) (inside nil) (go nil)
         (worker (nlk:spawn "guarded-access"
                   (nlk::call-guarding-connection
                    handle (lambda ()
                             (setf inside t)
                             (loop until go do (sleep 0.001))
                             (push :access log))))))
    (loop until inside do (sleep 0.001))
    (bt2:interrupt-thread worker (lambda () (push :interruption log)))
    (sleep 0.02)
    (setf go t)
    (sb-thread:join-thread (bt2:thread-native-thread worker) :default nil :timeout 5)
    (is (equal '(:interruption :access) log))))

(deftest store-guard-survives-unwinding-a-bare-handle-loop (with-temp-store ())
  ;; The incident replayed: a thread hammering the bare handle with no lock of
  ;; its own, unwound forty times by an interruption that signals; the store
  ;; must still answer afterwards.
  (let* ((handle (nlk::require-open-store))
         (stop nil)
         (rounds 0)
         (worker (nlk:spawn "bare-handle-loop"
                   (loop until stop
                         do (handler-case
                                (loop repeat 20
                                      do (sqlite:execute-to-list handle "select 1")
                                         (incf rounds))
                              (error () nil))))))
    (nlk:with-cleanup ((setf stop t)
                       (sb-thread:join-thread (bt2:thread-native-thread worker)
                                              :default nil :timeout 5))
      (loop repeat 40
            do (sleep 0.002)
               (bt2:interrupt-thread worker (lambda () (error "unwound"))))
      (let ((answered (bt2:make-semaphore :name "store-answered")))
        (nlk:spawn "store-probe"
          (nlk:session-exists-p "nobody")
          (bt2:signal-semaphore answered))
        (is (bt2:wait-on-semaphore answered :timeout 5)))
      (is (plusp rounds) "the loop ran"))))

;;; --- what a transaction defers to its commit --------------------------------

(deftest after-commit-runs-once-the-commit-lands-and-never-on-rollback (with-temp-store ())
  (let ((ran '()))
    (nlk::with-store-transaction (handle)
      (nlk:after-commit (lambda () (push :first ran)))
      (nlk:after-commit (lambda () (push :second ran)))
      (is (null ran) "nothing deferred runs before the commit"))
    (is (equal '(:first :second) (reverse ran)))
    (nlk:with-handlers ((error () nil))
      (nlk::with-store-transaction (handle)
        (nlk:after-commit (lambda () (push :lost ran)))
        (error "abort the transaction")))
    (is (not (member :lost ran)) "a rollback drops what it deferred")
    (is (signals-error nlk::no-transaction (nlk:after-commit (lambda () nil))))))
