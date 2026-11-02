;;;; bodies.lisp --- the exact bytes of every request a model was sent.
;;;; Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The request fact (events.lisp PROVIDER-REQUEST-EVENT) names a body by its
;;;; SHA-256; this table keeps the body itself, so what went out can be read
;;;; back byte for byte. Stored whole, bodies would be large, and each body
;;;; repeats the one before it but for the few kilobytes the round added. So
;;;; a body is kept as a delta against the body kept before it -- the
;;;; session's own last one, else the store's newest, which is how a fork's
;;;; first request finds the parent history it repeats -- and a chain of
;;;; deltas starts over from a body kept whole once rebuilding it would copy
;;;; more than *REQUEST-CHAIN-BYTES*.
;;;;
;;;; A delta is copies and inserts over the base followed by the target's own
;;;; bytes already written: the shared prefix and suffix are one copy each,
;;;; and the middle is matched a window at a time (a rolling hash over the
;;;; base, verified before use), so an edit early in the history -- a turn set
;;;; aside, a tool result shortened -- costs the edit and not the rest. The
;;;; image links no compressor (dexador's chipz only inflates), so the
;;;; target's own earlier bytes are the only other source: a body kept whole
;;;; still shrinks where it repeats itself.
;;;;
;;;; Retention: debug-only. Nothing is rebuilt from these rows and nothing
;;;; else reads them; they persist across restarts, bounded by the caller's
;;;; cap (request_bodies.keep_mb): past it the oldest chains go whole, so a
;;;; row never outlives the base it needs. A round with no row -- recorded
;;;; before bodies were kept, or dropped past the cap -- simply has none.

(in-package #:nodecode.kernel)

(defparameter *request-bodies-schema*
  '("create table if not exists request_bodies (
       id integer primary key autoincrement,
       session_id text not null,
       turn_id text not null,
       round integer not null,
       sha256 text not null,
       size integer not null,
       base integer,
       root integer not null,
       chain integer not null,
       bytes integer not null,
       delta blob not null)"
    "create index if not exists request_bodies_by_round
       on request_bodies (session_id, turn_id, round)"
    ;; Covering: the size read and the prune read this and never the deltas.
    "create index if not exists request_bodies_by_root
       on request_bodies (root, bytes, size)")
  "One row per kept request: its identity, SIZE and SHA256 as sent, the row
it is a delta against (BASE, NULL when kept whole), the whole row its chain
starts from (ROOT), the bytes rebuilding it copies (CHAIN), and the DELTA
with its length (BYTES).")

;; AUTOINCREMENT because an id is the key *LAST-KEPT-BODY* is trusted by: a
;; pruned row's id must never name another body.
(setf *schema* (distinct (append *schema* *request-bodies-schema*)))

;;; A rebuild copies at memory speed -- that 94 MB chain in 5 ms -- so the
;;; bound is set by what a body kept whole costs: at the operator's 0.9 MB
;;; bodies, one per gigabyte of chain is under a kilobyte a round.
(defparameter *request-chain-bytes* (* 1024 1024 1024)
  "The most a rebuild may copy: a chain whose next body would take it past
this starts over from a body kept whole.")

;;; --- the delta ---------------------------------------------------------------

(defparameter +delta-window+ 16
  "The bytes a match must share before it is taken, and the stride the
sources are indexed at: any run twice as long is found.")

(defparameter +hash-factor+ 2654435761
  "The rolling hash's multiplier, odd so no bit of a byte is lost.")

(defun window-hash (octets start)
  "The rolling hash of +DELTA-WINDOW+ bytes of OCTETS from START."
  (declare (type octets octets) (type fixnum start))
  (let ((hash 0) (factor +hash-factor+))
    (declare (type (unsigned-byte 32) hash factor))
    (loop for i of-type fixnum from start below (+ start +delta-window+)
          do (setf hash (ldb (byte 32 0) (+ (* hash factor) (aref octets i)))))
    hash))

(defun encode-delta (base target)
  "TARGET as a delta against BASE, both octets => the delta's octets: ops,
each opening with a varint N. An even N inserts the N/2 bytes that follow it;
an odd N copies (N-1)/2 bytes from the offset in the varint after it, in the
source that is BASE followed by TARGET as far as it is written."
  (declare (type octets base target))
  (let* ((window +delta-window+)
         (factor +hash-factor+)
         (top (loop with top of-type (unsigned-byte 32) = 1
                    repeat (1- window)
                    do (setf top (ldb (byte 32 0) (* top factor)))
                    finally (return top)))
         (blen (length base))
         (tlen (length target))
         (prefix (or (mismatch base target) tlen))
         (suffix (loop with most = (- (min blen tlen) prefix)
                       for n of-type fixnum from 0 below most
                       while (= (aref base (- blen n 1)) (aref target (- tlen n 1)))
                       finally (return n)))
         (end (- tlen suffix))
         (index (make-hash-table :size (+ 16 (floor (+ blen (- end prefix)) window))))
         (sink (make-array (+ 64 (floor tlen 8)) :element-type '(unsigned-byte 8)
                                                  :adjustable t :fill-pointer 0)))
    (declare (type fixnum window blen tlen prefix suffix end)
             (type (unsigned-byte 32) factor top))
    (labels ((varint (n)
               (loop do (vector-push-extend (if (> n 127) (logior 128 (ldb (byte 7 0) n)) n) sink)
                        (setf n (ash n -7))
                     while (plusp n)))
             (insert (start stop)
               (when (< start stop)
                 (varint (* 2 (- stop start)))
                 (let ((at (fill-pointer sink)))
                   (when (> (+ at (- stop start)) (array-dimension sink 0))
                     (setf sink (adjust-array sink (max (+ at (- stop start)) (* 2 at)))))
                   (setf (fill-pointer sink) (+ at (- stop start)))
                   (replace sink target :start1 at :start2 start :end2 stop))))
             (copy (from length)
               (when (plusp length)
                 (varint (1+ (* 2 length)))
                 (varint from)))
             (source-byte (at)
               (if (< at blen) (aref base at) (aref target (- at blen)))))
      ;; All of BASE, not only past the shared prefix: a round's new bytes
      ;; often repeat old ones (a file read again), 15% of a real session's.
      (loop for at from 0 to (- blen window) by window
            do (setf (gethash (window-hash base at) index) at))
      (copy 0 prefix)
      ;; LITERAL is where the bytes not yet copied begin; INDEXED is the next
      ;; window of TARGET to index, once it is wholly behind AT.
      (let ((at prefix) (literal prefix) (indexed prefix)
            (hash (if (<= (+ prefix window) end) (window-hash target prefix) 0)))
        (declare (type fixnum at literal indexed) (type (unsigned-byte 32) hash))
        (loop while (<= (+ at window) end)
              do (let* ((from (gethash hash index))
                        (from-base (and from (< from blen)))
                        (limit (if from-base blen (+ blen at))))
                   (if (and from
                            (<= (+ from window) limit)
                            (loop for k of-type fixnum below window
                                  always (= (source-byte (+ from k)) (aref target (+ at k)))))
                       (let ((length window))
                         (declare (type fixnum length))
                         ;; Back into the pending literal (the base only: a
                         ;; copy of TARGET's own bytes must end where it starts).
                         (when from-base
                           (loop while (and (> at literal) (> from 0)
                                            (= (aref base (1- from)) (aref target (1- at))))
                                 do (decf at) (decf from) (incf length)))
                         (loop while (and (< (+ at length) end) (< (+ from length) limit)
                                          (= (source-byte (+ from length)) (aref target (+ at length))))
                               do (incf length))
                         (insert literal at)
                         (copy from length)
                         (setf at (+ at length) literal at)
                         (when (<= (+ at window) end)
                           (setf hash (window-hash target at))))
                       (progn
                         (incf at)
                         (when (<= (+ at window) end)
                           (setf hash (ldb (byte 32 0)
                                           (+ (* (ldb (byte 32 0) (- hash (* (aref target (1- at)) top)))
                                                 factor)
                                              (aref target (+ at window -1)))))))))
                 (loop while (<= (+ indexed window) at)
                       do (setf (gethash (window-hash target indexed) index) (+ blen indexed))
                          (incf indexed window)))
        (insert literal end))
      (copy (- blen suffix) suffix)
      (coerce sink 'octets))))

(defun apply-delta (delta base base-length out)
  "Write the body DELTA (ENCODE-DELTA's) makes of BASE's first BASE-LENGTH
bytes into OUT => the body's length."
  (declare (type octets delta base out) (type fixnum base-length))
  (let ((at 0) (i 0))
    (declare (type fixnum at i))
    (flet ((varint ()
             (loop for shift of-type fixnum from 0 by 7
                   for byte = (aref delta i)
                   do (incf i)
                   sum (ash (logand byte 127) shift) into n
                   while (>= byte 128)
                   finally (return n))))
      (loop while (< i (length delta))
            do (let* ((n (varint)) (length (ash n -1)))
                 (if (evenp n)
                     (progn (replace out delta :start1 at :start2 i :end2 (+ i length))
                            (incf i length))
                     (let ((from (varint)))
                       (if (< from base-length)
                           (replace out base :start1 at :start2 from :end2 (+ from length))
                           (replace out out :start1 at :start2 (- from base-length)
                                            :end2 (+ (- from base-length) length)))))
                 (incf at length))))
    at))

;;; --- the rows ------------------------------------------------------------------

(defun rebuild-request-body (handle id)
  "Row ID's body, rebuilt along its chain and checked against its SHA-256 =>
(values OCTETS BASE), BASE the body it was kept against or NIL when it was
kept whole; NIL for no such row."
  ;; Two buffers the chain's largest body long, written in turn: each delta
  ;; reads the body before it from one and writes its own into the other, so
  ;; a chain of hundreds conses two bodies, not hundreds.
  (let* ((chain (sql-rows handle
                          "with recursive chain (id, base) as (
                             select id, base from request_bodies where id = ?
                             union all
                             select r.id, r.base from request_bodies r join chain on r.id = chain.base)
                           select size, sha256, delta from chain join request_bodies using (id)
                            order by id"
                          id))
         (room (reduce #'max chain :key #'first :initial-value 0))
         (body (make-array room :element-type '(unsigned-byte 8)))
         (base (make-array room :element-type '(unsigned-byte 8)))
         (length 0) (base-length 0))
    (when chain
      (loop for (nil nil delta) in chain
            do (rotatef body base)
               (setf base-length length
                     length (apply-delta delta base base-length body)))
      (let ((octets (subseq body 0 length))
            (sha256 (second (car (last chain)))))
        (unless (string= sha256 (sha256-text octets))
          (error "request body ~d rebuilt to ~d bytes that are not ~a" id length sha256))
        (values octets (and (rest chain) (subseq base 0 base-length)))))))

(defvar *last-kept-body* nil
  "(ID . WEAK-POINTER) to the octets kept last: the next keep's base, without
a rebuild, for as long as its turn still holds them. Retention: live-only; a
store opening clears it.")

(defun forget-last-kept-body ()
  "Drop *LAST-KEPT-BODY*: its id names a row of the store it was kept in."
  (setf *last-kept-body* nil))

(pushnew 'forget-last-kept-body *store-open-hooks*)

(defun keep-request-body (session-id turn-id round octets &key cap)
  "Keep OCTETS, the exact body round ROUND of TURN-ID in SESSION-ID sent, as
a delta against the body kept before it; then, while the kept bodies take
more than CAP bytes, drop the chains whose newest row is oldest. => the id
of its row."
  (check-strings session-id turn-id)
  (check-type octets octets)
  (with-store-transaction (handle)
    (destructuring-bind (&optional base-id root chain)
        (first (sql-rows handle
                         "select id, root, chain from request_bodies
                           where id = coalesce((select max(id) from request_bodies where session_id = ?),
                                               (select max(id) from request_bodies))"
                         session-id))
      (let* ((size (length octets))
             (whole (or (null base-id) (> (+ chain size) *request-chain-bytes*)))
             (kept *last-kept-body*)
             (delta (encode-delta (cond (whole (make-array 0 :element-type '(unsigned-byte 8)))
                                        ((and kept (eql base-id (car kept))
                                              (sb-ext:weak-pointer-value (cdr kept))))
                                        (t (rebuild-request-body handle base-id)))
                                  octets)))
        (sqlite:execute-non-query
         handle
         "insert into request_bodies (session_id, turn_id, round, sha256, size, base, root, chain, bytes, delta)
          values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
         session-id turn-id round (sha256-text octets) size
         (if whole nil base-id) (if whole 0 root) (if whole size (+ chain size))
         (length delta) delta)
        (let ((id (sqlite:last-insert-rowid handle)))
          (when whole
            (sqlite:execute-non-query handle "update request_bodies set root = id where id = ?" id))
          ;; Trusted only once the row is: a rolled-back insert's id is free again.
          (after-commit (lambda () (setf *last-kept-body* (cons id (sb-ext:make-weak-pointer octets)))))
          (when cap
            (let ((total (sqlite:execute-single handle "select sum(bytes) from request_bodies")))
              (when (> total cap)
                (loop for (oldest bytes) in (sql-rows handle
                                                      "select root, sum(bytes) from request_bodies
                                                        group by root order by max(id)")
                      while (> total cap)
                      do (sqlite:execute-non-query handle "delete from request_bodies where root = ?" oldest)
                         (decf total bytes)))))
          id)))))

(defun forget-request-bodies (handle ids)
  "Take every body the sessions IDS kept out, inside HANDLE's transaction;
a body of another session kept against one of them is kept whole instead."
  ;; A session's first body is kept against the store's newest, so a chain
  ;; runs through many sessions, and dropping the chains that hold a row of
  ;; IDS, as the cap does, would take other sessions' bodies with them. So a
  ;; row about to lose its base is rebuilt while its chain still stands and
  ;; written back whole, and the rows under it follow it to their new root,
  ;; which the cap drops them by. Their CHAIN still counts the rows gone: an
  ;; overcount, which only starts a later chain over sooner.
  (let* ((in (format nil "(~{?~*~^, ~})" ids))
         (orphans (loop for (id) in (apply #'sql-rows handle
                                           (format nil "select id from request_bodies
                                                         where session_id not in ~a and base in
                                                           (select id from request_bodies where session_id in ~a)"
                                                   in in)
                                           (append ids ids))
                        collect (cons id (rebuild-request-body handle id)))))
    (apply #'sqlite:execute-non-query handle
           (format nil "delete from request_bodies where session_id in ~a" in) ids)
    (loop for (id . octets) in orphans
          for delta = (encode-delta (make-array 0 :element-type '(unsigned-byte 8)) octets)
          do (sqlite:execute-non-query
              handle "update request_bodies set base = null, root = id, chain = size, bytes = ?, delta = ?
                       where id = ?"
              (length delta) delta id)
             (sqlite:execute-non-query
              handle "update request_bodies set root = ? where id in (
                        with recursive under (id) as (
                          select id from request_bodies where base = ?
                          union all
                          select r.id from request_bodies r join under on r.base = under.id)
                        select id from under)"
              id id))))

(defun kept-request-body (session-id turn-id round)
  "The body round ROUND of TURN-ID in SESSION-ID sent, exactly => (values
OCTETS PREVIOUS FROM): PREVIOUS the body it was kept against and FROM that
body's (SESSION-ID TURN-ID ROUND), both NIL for one kept whole; NIL when the
round's body was not kept."
  (check-strings session-id turn-id)
  (with-store-handle (handle)
    (destructuring-bind (&optional id base)
        (first (sql-rows handle "select id, base from request_bodies
                                  where session_id = ? and turn_id = ? and round = ?
                                  order by id desc limit 1"
                         session-id turn-id round))
      (when id
        (multiple-value-bind (octets previous) (rebuild-request-body handle id)
          (values octets previous
                  (and base (first (sql-rows handle "select session_id, turn_id, round
                                                       from request_bodies where id = ?"
                                             base)))))))))

(defun last-request-body (session-id)
  "The body SESSION-ID's newest kept request sent, exactly, or NIL when it
kept none or no store is open."
  ;; What a turn's first round shares a prefix with: the turn before's last
  ;; round. Remembered as the next keep's base, which is the same row, so the
  ;; round that reads it rebuilds it once.
  (check-strings session-id)
  (when-store-handle (handle nil)
    (let ((id (sqlite:execute-single handle "select max(id) from request_bodies where session_id = ?"
                                     session-id))
          (kept *last-kept-body*))
      (when id
        (or (and kept (eql id (car kept)) (sb-ext:weak-pointer-value (cdr kept)))
            (let ((octets (rebuild-request-body handle id)))
              (setf *last-kept-body* (cons id (sb-ext:make-weak-pointer octets)))
              octets))))))

(defun request-bodies-size ()
  "What the kept bodies take => (values BYTES COUNT SIZE): the bytes stored,
the requests kept and the bytes they were as sent; zeros with no store."
  (when-store-handle (handle (values 0 0 0))
    (values-list (first (sql-rows handle "select coalesce(sum(bytes), 0), count(*),
                                                 coalesce(sum(size), 0)
                                            from request_bodies")))))
