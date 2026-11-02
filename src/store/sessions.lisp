;;;; sessions.lisp --- a session whole: deleted for good, written out as one
;;;; file, and read back in from one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A session's facts are every row whose session_id names it, in each table
;;;; that holds one (+SESSION-TABLES+): the log, the directory and the open
;;;; turns the log keeps in step with itself (events.lisp), the stream seeds
;;;; beside them, and the session-state KV with its fold (state.lisp). A
;;;; session's children -- the forks that compose it, the seats spawned
;;;; from it -- hang off it by the parent their created facts wrote,
;;;; and go where it goes. So do the request bodies they kept (bodies.lisp,
;;;; FORGET-REQUEST-BODIES), which a file does not carry: they are debug-only.
;;;;
;;;; A prune is a delete of many, picked from the directory: the sessions
;;;; no one has touched in a while, by the source that began them or by
;;;; having nothing said in them (PRUNE-PICKS).
;;;;
;;;; A delete is the one road by which a row leaves the log. Nothing is
;;;; verified across rows: a row's payload and contract hashes are its own
;;;; (VERIFY-ROW-HASHES), and no row carries another's, so taking a session's
;;;; rows out leaves every other row as readable as it was. The pages they held
;;;; go on SQLite's free list and later writes fill them; the file does not
;;;; shrink, since the store runs no auto_vacuum and no VACUUM.
;;;;
;;;; The file is JSON lines: a header naming its sessions, then their rows as
;;;; each table holds them -- a log row as its portable envelope
;;;; (+EVENT-ENVELOPE-COLUMNS+, the payload the exact text its hash is over)
;;;; with the position it had, a KV row with its own ids. The rows of a fork's
;;;; composed ancestors come along (the header says which), so a fork reads
;;;; back whole. Files run together read as one: an import takes every header,
;;;; drops a row it already took, and lands the rows in the order they had.
;;;; Each log row lands through ADOPT-EVENT, identity, sequence, time and
;;;; provenance as recorded; the positions are this store's own, so every
;;;; position a payload or a KV row names is carried to where its row landed,
;;;; and a fork composes, a floor stands and a rewind folds as they did.

(in-package #:nodecode.kernel)

(define-errors (event-log-error) ((:export))
  (session-running (sessions) "a turn is running in ~{~a~^, ~}: stop it, then try again" sessions)
  (sessions-present (sessions)
   "this store already holds ~{~a~^, ~}; delete what it holds to import the copy" sessions))

(define-error session-file-invalid (event-log-error) (detail)
  (:export)
  (:report "not a session file nodecode can read: ~a" detail)
  (:signal refuse-session-file))

(defparameter +session-tables+
  '("events" "open_turns" "session_directory" "gateway_sequences"
    "session_state_events" "session_state")
  "Every table holding a session's rows by its session_id.")

(defparameter +sessions-file-format+ "nodecode.sessions"
  "What a session file's header says it is.")

;;; Each writer of these stamps SESSION-LOG-WATERMARK or a position it read:
;;; a fork's anchor (session.created), a capture's (session.workspace_captured),
;;; a head move's watermark (session.head_moved), a floor's watermark and range
;;; (session.context.evicted), a within-turn eviction's
;;; (turn.context.evicted). A fact that comes to name a position joins this list.
(defparameter +position-keys+
  '("anchor-log-position" "anchor-position" "source-watermark" "watermark"
    "retained-from-sequence" "evicted-start-sequence" "evicted-end-sequence")
  "The payload keys whose integer is a log position.")

(defparameter +session-state-columns+
  '(("session_state_events" "id" "session_id" "ws" "key" "value_json" "log_seq" "occurred_at")
    ("session_state" "session_id" "ws" "key" "ver" "value_json" "updated_log_seq" "updated_at"))
  "The KV's tables and the columns a file carries of each: the fold's VER is
the id of the event that wrote it, LOG_SEQ and UPDATED_LOG_SEQ positions.")

(defparameter +exported-event-columns+
  (cons "log_position" (mapcar #'first +event-envelope-columns+))
  "A log row as a file carries it: its envelope and the position it had.")

(defun in-list (ids)
  "The SQL list of one placeholder per id of IDS."
  (format nil "(~{?~*~^, ~})" ids))

(defun session-in (ids)
  "The EVENTS :WHERE that holds a row to the sessions IDS."
  (cons (format nil "session_id in ~a" (in-list ids)) ids))

(defun session-tree (handle session-id)
  "SESSION-ID and every session under it -- each session whose created fact
names one of them as parent -- SESSION-ID first; NIL when the store holds no
SESSION-ID."
  (let ((ids (mapcar #'first
                     (sql-rows handle "with recursive tree(id) as (
                                         select session_id from session_directory
                                          where session_id = ? and created_at is not null
                                         union
                                         select d.session_id from session_directory d
                                           join tree on d.parent = tree.id
                                          where d.created_at is not null)
                                       select id from tree"
                               session-id))))
    (and ids (cons session-id (remove session-id ids :test #'string=)))))

(defun composed-ancestors (handle session-id)
  "The sessions SESSION-ID composes, nearest first: the parent its anchor
turn ties it to, and that parent's, while each is a fork."
  ;; A parent with no anchor is provenance alone and composes nothing.
  (mapcar #'first
          (sql-rows handle "with recursive up(id) as (
                              select parent from session_directory
                               where session_id = ? and anchor_turn_id is not null
                              union
                              select d.parent from session_directory d join up on d.session_id = up.id
                               where d.anchor_turn_id is not null)
                            select id from up"
                    session-id)))

(defun refuse-running (handle ids)
  "Signal SESSION-RUNNING when a turn is open in any of IDS."
  (when-let (running (mapcar #'first
                             (apply #'sql-rows handle
                                    (format nil "select distinct session_id from open_turns
                                                  where session_id in ~a" (in-list ids))
                                    ids)))
    (error 'session-running :sessions running)))

;;; --- delete ---------------------------------------------------------------------

(defun delete-session (session-id &key dry-run)
  "Delete SESSION-ID and every session under it, each of their rows in every
table that holds one, in one transaction; with DRY-RUN write nothing and
answer the same."
  ;; => (:sessions IDS :turns N :events N), SESSION-ID first: what goes, which
  ;; is what a confirmation names. Refuses a SESSION-ID the store does not hold
  ;; (UNKNOWN-SESSION) and a turn open in any of them (SESSION-RUNNING): a turn
  ;; writes on, and its facts would land in a session that is gone.
  (check-type session-id session-id)
  (flet ((plan (handle &aux (ids (or (session-tree handle session-id)
                                     (error 'unknown-session :session-id session-id))))
           (refuse-running handle ids)
           (list :sessions ids
                 :turns (events :kind +kind-turn-started+ :where (session-in ids) :as :count)
                 :events (events :where (session-in ids) :as :count))))
    (if dry-run
        (with-store-handle (handle) (plan handle))
        (with-store-transaction (handle)
          (let* ((plan (plan handle))
                 (ids (getf plan :sessions))
                 (store *store*))
            (dolist (table +session-tables+)
              (apply #'sqlite:execute-non-query handle
                     (format nil "delete from ~a where session_id in ~a" table (in-list ids))
                     ids))
            (forget-request-bodies handle ids)
            ;; What this process committed stays in memory per session
            ;; (COMMITTED-EVENTS-AFTER): a ring left standing would hand a
            ;; consumer these facts again after an import of the same id.
            (after-commit (lambda ()
                            (sb-ext:with-locked-hash-table (store.committed)
                              (dolist (id ids) (remhash id store.committed)))))
            plan)))))

;;; --- prune ----------------------------------------------------------------------
;;; A prune is a delete of many, picked by what the directory knows of each
;;; session (events.lisp): its last activity, the source that began it,
;;; whether anything was said in it. A session goes with every session under
;;; it, as a delete takes it, so a session is judged with them: it is old only
;;; when nothing under it moved since the cut, and empty only when nothing
;;; under it holds a word -- a stale parent never takes a fork the operator
;;; used this morning. A session under another the prune takes is not named
;;; again; one whose parent stays can go alone (an old seat, its session
;;; kept).

(defun prune-picks (&key (days 30) source empty (now (get-universal-time)))
  "The sessions a prune takes, each with every session under it: those whose
last activity, and every session's under them, is more than DAYS days before
NOW (a universal time); begun by SOURCE (what SESSION-SOURCE-KIND
answers) when it is given, the empty string the sessions it answers none
for; with no message in them or under them when EMPTY. => (values PICKS
RUNNING): the ids that go, oldest activity first, none under another; and the
ones the same filters matched that a turn open in their tree keeps, which a
delete refuses (SESSION-RUNNING)."
  ;; One read of the directory and one of the open turns, whatever the store
  ;; holds; the tree is walked here, a session's branch answered once.
  (check-type days (integer 0))
  (let ((cut (- now (* days 86400)))
        (rows (make-hash-table :test #'equal))
        (kids (make-hash-table :test #'equal))
        (running-ids (make-hash-table :test #'equal))
        (branches (make-hash-table :test #'equal)))
    (when-store-handle (handle)
      (dolist (row (sql-rows handle "select session_id, parent, updated_at, source, message_count
                                       from session_directory where created_at is not null"))
        (setf (gethash (first row) rows) row))
      (dolist (row (sql-rows handle "select distinct session_id from open_turns"))
        (setf (gethash (first row) running-ids) t)))
    (flet ((parent-of (id &aux (parent (second (gethash id rows))))
             (and (stringp parent) (string/= parent id) (gethash parent rows) parent)))
      (loop for id being the hash-keys of rows
            do (when-let (parent (parent-of id)) (push id (gethash parent kids))))
      (labels ((branch (id)
                 ;; (NEWEST MESSAGES RUNNING) over ID and every session under
                 ;; it, NEWEST a universal time. Seeded before the walk, so a
                 ;; parent cycle the log should never hold reads a half answer.
                 (or (gethash id branches)
                     (let* ((row (gethash id rows))
                            (newest (or (iso-universal (third row)) now))
                            (messages (fifth row))
                            (running (gethash id running-ids)))
                       (setf (gethash id branches) (list newest messages running))
                       (dolist (kid (gethash id kids))
                         (destructuring-bind (kid-newest kid-messages kid-running) (branch kid)
                           (setf newest (max newest kid-newest)
                                 messages (+ messages kid-messages)
                                 running (or running kid-running))))
                       (setf (gethash id branches) (list newest messages running)))))
               (matched-p (id)
                 (bind (((newest messages _) (values-list (branch id))))
                   (and (< newest cut)
                        (or (null source) (equal source (or (fourth (gethash id rows)) "")))
                        (or (not empty) (zerop messages)))))
               (running-p (id) (third (branch id)))
               (topmost (ids &aux (set (string-set ids)))
                 ;; IDS without any that another of IDS holds above it.
                 (remove-if (lambda (id)
                              (loop for up = (parent-of id) then (parent-of up)
                                    for steps below (hash-table-count rows)
                                    while up thereis (gethash up set)))
                            ids)))
        (let ((matched (loop for id being the hash-keys of rows when (matched-p id) collect id)))
          (values (sort (topmost (remove-if #'running-p matched))
                        (lambda (a b &aux (one (first (branch a))) (two (first (branch b))))
                          (or (< one two) (and (= one two) (string< a b)))))
                  (sort (topmost (remove-if-not #'running-p matched)) #'string<)))))))

;;; --- export ---------------------------------------------------------------------

(defun column-object (columns row)
  "ROW's values as a JSON object keyed by COLUMNS, a NULL column absent."
  (apply #'make-json-object (loop for column in columns
                                  for value in row
                                  when value append (list column value))))

(defun export-sessions (session-id)
  "SESSION-ID, every session under it and every session it composes, as one
session file: the UTF-8 octets of its JSON lines."
  ;; Refuses as DELETE-SESSION does, a turn running in any of them included:
  ;; a copy of an open turn would land open, with nothing to run it. The rows
  ;; are read under one hold of the store and encoded after it.
  (check-type session-id session-id)
  (destructuring-bind (ids ancestors rows state)
      (with-store-handle (handle)
        (let* ((ids (or (session-tree handle session-id)
                        (error 'unknown-session :session-id session-id)))
               (ancestors (composed-ancestors handle session-id))
               (all (append ancestors ids)))
          (refuse-running handle all)
          (list ids ancestors
                (events :aggregate-type "session" :where (session-in all)
                        :as :rows :columns +exported-event-columns+)
                (loop for (table . columns) in +session-state-columns+
                      for sql = (format nil "select ~{~a~^, ~} from ~a where session_id in ~a"
                                        columns table (in-list all))
                      collect (list table columns (apply #'sql-rows handle sql all))))))
    ;; JSON lines: the header, the log rows, then each KV table's rows.
    (coerce
     (flexi-streams:with-output-to-sequence (out)
       (flet ((emit (object)
                (write-sequence (encode-json-octets object) out)
                (write-byte 10 out)))
         (emit (json-object "format" +sessions-file-format+ "version" 1 "exported_at" (iso-now)
                            "sessions" (coerce ids 'vector) "ancestors" (coerce ancestors 'vector)))
         (dolist (row rows)
           (emit (json-object "events" (column-object +exported-event-columns+ row))))
         (loop for (table columns table-rows) in state
               do (dolist (row table-rows)
                    (emit (make-json-object table (column-object columns row)))))))
     'octets)))

;;; --- import ---------------------------------------------------------------------

(defun session-file-lines (octets &aux (start 0) (line 0) (objects '()))
  "OCTETS, a session file, as the JSON object each of its lines is; a blank
line is skipped."
  (loop while (< start (length octets))
        do (let ((end (or (position 10 octets :start start) (length octets))))
             (incf line)
             (unless (every (lambda (octet) (member octet '(9 13 32))) (subseq octets start end))
               (let ((object (ignore-errors (decode-json (subseq octets start end) :whole t))))
                 (unless (hash-table-p object)
                   (refuse-session-file "line ~d is not a JSON object" line))
                 (push object objects)))
             (setf start (1+ end))))
  (nreverse objects))

(defun carried-envelope (row here incoming &aux (payload (gethash "payload" row)))
  "ROW, a log row a file carries, as the envelope ADOPT-EVENT takes: each
position its payload names carried by HERE -- a fork's anchor only when the
parent it names is INCOMING, landing here too; one it composes in place is
at the positions it always had."
  (when (and (stringp payload) (some (lambda (key) (search key payload)) +position-keys+))
    (let ((object (decode-event-payload-object payload))
          (moved nil))
      (dolist (key +position-keys+)
        (let ((old (gethash key object)))
          (when (and (integerp old)
                     (or (string/= key "anchor-log-position")
                         (member (gethash "parent" object) incoming :test #'equal)))
            (setf (gethash key object) (funcall here old) moved t))))
      (when moved
        (setf payload (encode-json-object object)))))
  (loop for (column key) in +event-envelope-columns+
        for value = (if (string= column "payload") payload (gethash column row))
        when (and value (not (eq value :null)))
          append (list key value)))

(defun read-session-file (octets &aux (headers '())
                                       (rows (make-hash-table :test #'equal))
                                       (state (make-hash-table :test #'equal)))
  "OCTETS, a session file or several run together, read. => (values NAMED
ANCESTORS ROWS STATE): the sessions its headers name, in their order; the
sessions they name as composed beside them; its log rows by event id; its KV
rows by table, session, ws, key and id."
  ;; A row seen twice is kept once: files run together carry a shared
  ;; ancestor's rows in each.
  (flet ((listed (key)
           (distinct (loop for header in (reverse headers)
                           append (coerce (json-value header :array key) 'list)))))
    (dolist (object (session-file-lines octets))
      (let* ((table (find-if (lambda (key) (hash-table-p (gethash key object)))
                             (cons "events" (mapcar #'first +session-state-columns+))))
             (row (and table (gethash table object))))
        (cond ((gethash "format" object)
               (unless (and (equal +sessions-file-format+ (gethash "format" object))
                            (eql 1 (gethash "version" object)))
                 (refuse-session-file "the header says ~a version ~a"
                                      (gethash "format" object) (gethash "version" object)))
               (push object headers))
              ((null table) (refuse-session-file "a line is neither a header nor a row"))
              ((string/= table "events")
               (setf (gethash (list table (gethash "session_id" row) (gethash "ws" row)
                                    (gethash "key" row) (gethash "id" row))
                              state)
                     row))
              ((and (json-value row :integer "log_position") (json-value row :text "event_id")
                    (json-value row :text "session_id"))
               (setf (gethash (gethash "event_id" row) rows) row))
              (t (refuse-session-file "a log row has no position, event id or session")))))
    (let* ((named (listed "sessions"))
           (ancestors (remove-if (lambda (id) (member id named :test #'equal)) (listed "ancestors"))))
      (unless (and named (every #'stringp (append named ancestors)))
        (refuse-session-file "no header names its sessions"))
      (values named ancestors rows state))))

(defun import-sessions (octets)
  "Recreate the sessions of OCTETS -- a file EXPORT-SESSIONS wrote, or several
run together -- in one transaction."
  ;; => (:sessions IDS :kept IDS :events N): the sessions recreated, in the
  ;; order the headers name them (each file's own first); the composed
  ;; ancestors the store already held, which keep their rows and positions and
  ;; are composed in place; the log rows that landed. A session the headers
  ;; name that the store already holds is SESSIONS-PRESENT and nothing lands:
  ;; its event and turn ids are in the log already, and payloads name turns by
  ;; id, so a second copy under a new id would be a rewrite of every one of
  ;; them. A file that is not one is SESSION-FILE-INVALID, and a row the log
  ;; refuses is ADOPT-EVENT's refusal; either way nothing lands.
  (multiple-value-bind (named ancestors rows state) (read-session-file octets)
    (let* ((kept (remove-if-not #'session-exists-p ancestors))
           (incoming (remove-if (lambda (id) (member id kept :test #'equal))
                                (append named ancestors)))
           (log (sort (loop for row being the hash-values of rows
                            unless (member (gethash "session_id" row) kept :test #'equal)
                              collect row)
                      #'< :key (lambda (row) (gethash "log_position" row)))))
      (when-let (present (remove-if-not #'session-exists-p named))
        (error 'sessions-present :sessions present))
      (dolist (row (append log (alexandria:hash-table-values state)))
        (unless (member (json-value row :string "session_id") (append incoming kept) :test #'equal)
          (refuse-session-file "a row of ~a, which no header names" (json-value row :any "session_id"))))
      (dolist (id incoming)
        (unless (find-if (lambda (row) (and (equal id (gethash "session_id" row))
                                            (equal +kind-session-created+ (gethash "kind" row))))
                         log)
          (refuse-session-file "~a has no created fact" id)))
      (with-store-transaction (handle)
        (let ((landed (make-array 256 :adjustable t :fill-pointer 0))
              (versions (make-hash-table)))
          ;; A position lands where the newest row at or below it landed:
          ;; the rows land in the order they had, so every row a watermark
          ;; covered stays under it and every row above stays above.
          (flet ((here (old)
                   (if (and (integerp old) (plusp old))
                       (or (cdr (find old landed :key #'car :test #'>= :from-end t)) 0)
                       old)))
            (dolist (row log)
              (vector-push-extend (cons (gethash "log_position" row)
                                        (adopt-event (carried-envelope row #'here incoming)
                                                     :within handle))
                                  landed))
            ;; The KV's events first, in the order of their ids, so the
            ;; fold's VER finds the id each landed under.
            (loop for (table . columns) in +session-state-columns+
                  for inserted = (remove "id" columns :test #'string=)
                  do (dolist (row (sort (loop for (name session) being the hash-keys of state
                                                using (hash-value row)
                                              when (and (string= name table)
                                                        (member session incoming :test #'equal))
                                                collect row)
                                        #'< :key (lambda (row) (or (gethash "id" row) 0))))
                       (apply #'sqlite:execute-non-query handle
                              (format nil "insert into ~a (~{~a~^, ~}) values ~a"
                                      table inserted (in-list inserted))
                              (loop for column in inserted
                                    for value = (gethash column row)
                                    collect (cond ((member column '("log_seq" "updated_log_seq")
                                                           :test #'string=)
                                                   (here value))
                                                  ((string= column "ver")
                                                   (or (gethash value versions)
                                                       (refuse-session-file "a state row names no state event")))
                                                  ((eq value :null) nil)
                                                  (t value))))
                       (when (gethash "id" row)
                         (setf (gethash (gethash "id" row) versions)
                               (sqlite:last-insert-rowid handle)))))
            (list :sessions incoming :kept kept :events (length landed))))))))
