;;;; standby.lisp --- a session id held in memory until its first block.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A session that has no message is not a session yet: a shell opened and
;;;; never typed in, a /clear, a lane's fork before its ask lands, a cron
;;;; job between fires. Those ids live here until the first block makes them
;;;; real. Nothing durable names a standby id -- no SESSION.CREATED, no
;;;; configuration fact, no transcript row -- so an image that ends with the
;;;; id still standing by leaves nothing behind: this registry is memory,
;;;; and the whole record dies with the process.
;;;;
;;;; The append gate (events.lisp SESSION-FACT-GATE) is the other half: it
;;;; holds the few configuration facts a standby session may carry
;;;; (+STANDBY-WAITED-KINDS+), and refuses a block that would land before
;;;; the identity did. A block writer materializes first, inside its own
;;;; transaction (MATERIALIZE-STANDBY-SESSION, events.lisp), so identity,
;;;; the facts that waited and the block commit as one.
;;;;
;;;; Registration is idempotent and the first identity wins, exactly as
;;;; lineage is fixed at creation; a later call may fill in what an earlier
;;;; one left unsaid (a lane names its room after the id was minted).

(in-package #:nodecode.kernel)

(define-record (standby-record (:constructor %make-standby-record)
                               (:export id cwd parent anchor-turn-id
                                        anchor-log-position facts))
  (id (error "id required") :type string :read-only t)
  ;; What the created fact will carry: CWD as the mint resolved it, PARENT
  ;; and the fork anchor as the mint read them. NIL is "the mint did not
  ;; say", not "none": materialization decides what the fact carries.
  (cwd nil :type (or null string))
  (parent nil :type (or null string))
  (anchor-turn-id nil :type (or null string))
  ;; The fork anchor's frozen watermark, read at mint: everything the parent
  ;; records above it stays invisible to the child, exactly as it would if
  ;; the created fact had been written then.
  (anchor-log-position nil :type (or null log-position))
  ;; Facts that named this id before it had a block, oldest first: plists
  ;; (:kind KIND :payload PAYLOAD :options OPTIONS), replayed in order by
  ;; MATERIALIZE-STANDBY-SESSION.
  (facts '() :type list))

(access (record standby-record))

(defvar *standby-sessions* (make-hash-table :test #'equal :synchronized t)
  "Standby session ids -- minted, not yet durable -- by id.")

(defun on-standby-p (session-id)
  "True when SESSION-ID was minted and is still only standing by: known to
this image, absent from the log."
  ;; STANDBY-RECORD-P tests a record's type; this tests an id's state, and an
  ;; id is what callers hold.
  (not (null (standby-session-record session-id))))

(defun standby-session-record (session-id)
  "The standby record for SESSION-ID, or NIL."
  ;; The record is live: callers materialize and forget through the functions
  ;; here, never by editing it.
  (check-type session-id session-id)
  (gethash session-id *standby-sessions*))

(defun standby-session-head-turn-id (session-id)
  "The head turn a fork of SESSION-ID anchors at, resolved through the
standby chain: a session still standing by has no turns of its own, so its
head is the anchor it was minted with -- an ancestor's turn -- and a mint
with no parent has none."
  ;; The durable reader (SESSION-HEAD-TURN-ID) answers the same question for a
  ;; session that is already in the log.
  (when-let (record (standby-session-record session-id))
    (or record.anchor-turn-id
        (let ((parent record.parent)) (and parent (standby-session-head-turn-id parent))))))

(defun register-standby-session (session-id &key cwd parent anchor-turn-id
                                              anchor-log-position)
  "Hold SESSION-ID standing by and return its record."
  ;; Registering an id already standing by keeps the identity it was minted
  ;; with and fills in only what the first call left unsaid.
  (check-type session-id session-id)
  (sb-ext:with-locked-hash-table (*standby-sessions*)
    (let ((record (alexandria:ensure-gethash session-id *standby-sessions*
                                             (%make-standby-record :id session-id))))
      (setf record.cwd (or record.cwd cwd)
            record.parent (or record.parent parent))
      (when (and anchor-turn-id (null record.anchor-turn-id))
        (setf record.anchor-turn-id anchor-turn-id)
        (setf record.anchor-log-position anchor-log-position))
      record)))

(defun hold-standby-session-fact (session-id kind payload options)
  "Hold one fact a standby session carries before its first block."
  ;; OPTIONS
  ;; are APPEND-EVENT-ROW's keywords, kept as the caller wrote them but for
  ;; :EXPECTED-SEQUENCE, which is dropped: these facts replay in the order they
  ;; were held, which is the order they happened, and an expectation read before
  ;; the identity existed cannot bind the log. An :EVENT-ID the caller already
  ;; minted rides along, so a fact held and written twice is still one fact.
  ;; => T when the id was standing by and the fact is now held.
  (sb-ext:with-locked-hash-table (*standby-sessions*)
    (when-let (record (gethash session-id *standby-sessions*))
      (setf record.facts
            (nconc record.facts
                   (list (list :kind kind :payload payload
                               :options (alexandria:remove-from-plist
                                         options :expected-sequence)))))
      t)))

(defun standby-session-fact (session-id kind)
  "The payload of the newest fact of KIND held on SESSION-ID, as the log
will read it back (a JSON object), or NIL when none is held."
  ;; A standby session's configuration reads the way a durable one's does:
  ;; the model pinned before the first message is the one every surface names
  ;; meanwhile, and the one that message runs on.
  (sb-ext:with-locked-hash-table (*standby-sessions*)
    (when-let (fact (when-let (record (gethash session-id *standby-sessions*))
                      (find kind record.facts :key (lambda (fact) (getf fact :kind))
                                              :test #'string= :from-end t)))
      (payload-object (getf fact :payload)))))

(defun take-standby-session-facts (session-id)
  "The facts held on SESSION-ID, oldest first, taken -- the record is left
with none. Materialization is their only caller."
  (sb-ext:with-locked-hash-table (*standby-sessions*)
    (when-let (record (gethash session-id *standby-sessions*))
      (prog1 record.facts
        (setf record.facts '())))))

(defun forget-standby-session (session-id)
  "Drop SESSION-ID's standby record -- it is durable now, or abandoned."
  ;; => T when there was one to drop.
  (check-type session-id session-id)
  (not (null (remhash session-id *standby-sessions*))))
