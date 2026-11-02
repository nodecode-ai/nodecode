;;;; transcripts.lisp --- foreign conversations as exchanges with their own clock.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two shapes hold every transcript the ecosystem writes, and neither is
;;;; a world's invention:
;;;;
;;;;   a JSONL transcript   one line per message — a role, a content, a
;;;;                        timestamp — or one line per session carrying
;;;;                        its messages. Claude Code, Codex, pi and a
;;;;                        Hermes export all write one of the two.
;;;;   a sqlite store       a session table and a message table, joined by
;;;;                        a session column. Hermes, OpenCode and Crush.
;;;;
;;;; So the reader matches COLUMNS and MEMBERS by name, never a schema by
;;;; world: whichever column is called id, which one names the session,
;;;; which one holds the role. A store whose columns say nothing is left
;;;; alone and named in the report.
;;;;
;;;; Each record's messages fold into exchanges: one user side (the user's
;;;; lines up to the answer, joined) and one assistant side (every
;;;; assistant text before the next user line, joined), stamped with the
;;;; answer's time — the shape NLK:RECORD-EXCHANGE-TURN writes as one
;;;; settled turn. Tool rounds are dropped: the store has no way to say a
;;;; tool call happened without having run it. A prompt with no answer is
;;;; dropped and counted.
;;;;
;;;; Conversations are never on a first frame. A real box carries gigabytes
;;;; of them and a provider key is bytes; the plan reads them only when
;;;; they are asked for, newest first, under the folder's session budget.

(in-package #:nodecode.import)

(nlk:define-record (record (:copier nil))
  "One foreign session as read."
  ;; EXCHANGES are plists (:input :answer :at :elapsed-ms), oldest first;
  ;; DROPPED counts prompts that had no answer; MESSAGES the rows read.
  id world source title model cwd started-at
  (exchanges '()) (dropped 0) (messages 0))

;;; --- time ------------------------------------------------------------------

(defun seconds-from-any (value)
  "VALUE as seconds since 1970: a number (milliseconds when it is large
enough to be one), or an ISO-8601 string. NIL for anything else."
  (cond ((and (realp value) (> value 1d11)) (/ value 1000))
        ((realp value) value)
        ((and (stringp value) (>= (length value) 19) (char= (char value 10) #\T))
         (nlk:when-let (universal (nlk:iso-universal value)) (- universal nlk:+unix-epoch+)))))

(defun iso-from-any (value &aux (seconds (seconds-from-any value)))
  (and seconds (nlk:iso-time seconds)))

;;; --- messages -----------------------------------------------------------------

(defun message-text (content)
  "CONTENT as the text the operator saw: a string as it is; an array of
parts (multimodal, or a tool round) joined over its text members; nothing
for the rest."
  (labels ((parts-text (parts)
             (let ((texts (texts-of parts "text" "content" "input_text")))
               (and texts (format nil "~{~a~^~%~}" texts)))))
    (cond ((stringp content)
           (let ((trimmed (nlk:trimmed content)))
             (cond ((zerop (length trimmed)) nil)
                   ((and (char= (char trimmed 0) #\[) (cl:search "\"type\"" trimmed))
                    (let ((parts (handler-case (nlk:decode-json trimmed) (error () nil))))
                      (if (vectorp parts) (parts-text parts) trimmed)))
                   (t trimmed))))
          ((vectorp content) (parts-text content))
          ((hash-table-p content)
           (let ((index (object-index content)))
             (or (text-of index "text")
                 (let ((parts (member-of index "content" "parts")))
                   (cond ((stringp parts) parts)
                         ((vectorp parts) (parts-text parts))))))))))

(defun record-from-messages (id world source rows &key title model cwd started)
  ;; ROWS fold into exchanges; DROPPED counts prompts never answered and answers never asked.
  (let ((exchanges '())
        (dropped 0)
        (user-lines '())
        (user-at nil)
        (answer-lines '())
        (answer-at nil))
    (flet ((flush ()
             (cond ((and user-lines answer-lines)
                    (push (list :input (format nil "~{~a~^~%~%~}" (reverse user-lines))
                                :answer (format nil "~{~a~^~%~%~}" (reverse answer-lines))
                                :at (and answer-at (nlk:iso-time answer-at))
                                :elapsed-ms (and user-at answer-at
                                                 (max 0 (round (* 1000 (- answer-at user-at))))))
                          exchanges))
                   ((or user-lines answer-lines) (incf dropped)))
             (setf user-lines '() user-at nil answer-lines '() answer-at nil)))
      (dolist (message rows)
        (destructuring-bind (&key role text at &allow-other-keys) message
          (when (and (stringp text) (plusp (length text)))
            (cond ((equal role "user")
                   ;; A `[Note: ...]' row is the harness talking to itself.
                   (unless (ppcre:scan "(?s)\\A\\[Note:.*\\][ \\n]*\\z" text)
                     (when answer-lines (flush))
                     (push text user-lines)
                     (unless user-at (setf user-at at))))
                  ((equal role "assistant")
                   (cond (user-lines (push text answer-lines)
                                     (when at (setf answer-at at)))
                         (t (incf dropped))))))))
      (flush))
    (make-record :id id :world world :source source :title title :model model :cwd cwd
                 :started-at (or started
                                 (nlk:when-let (at (getf (first rows) :at)) (nlk:iso-time at)))
                 :exchanges (nreverse exchanges) :dropped dropped :messages (length rows))))

;;; --- the JSONL transcript ---------------------------------------------------

(defun line-message (object)
  "The (:ROLE :TEXT :AT) plist one transcript line carries, :ROLE NIL when it names none."
  (let* ((index (object-index object))
         (at (seconds-from-any (or (member-of index "timestamp" "at" "time")
                                   (member-of index "createdAt"))))
         ;; The wrapper a line keeps its message in: Claude Code's message, Codex's payload.
         (nest (loop for name in '("message" "payload" "msg" "data")
                     for value = (member-of index name)
                     when (hash-table-p value) return (object-index value))))
    (flet ((role-of (index)
             (or (text-of index "role")
                 (find (text-of index "type") '("user" "assistant") :test #'equal)))
           (content-of (index) (member-of index "content" "text" "parts" "message")))
      (list :role (or (role-of index) (and nest (role-of nest)))
            :text (message-text (or (and nest (content-of nest)) (content-of index)))
            :at (or at (and nest (seconds-from-any (member-of nest "timestamp" "at" "time"))))))))

(defun jsonl-records (path world source &aux (objects '()))
  "The sessions of the JSONL at PATH: one per line when each line carries
its own messages, else the whole file as one session named by the file."
  ;; At most 20000 lines, one that will not decode skipped: a live transcript ends mid-line.
  ;; A sidechain line is a subagent's, never the conversation's: Claude Code
  ;; marks each one isSidechain and names the PARENT's sessionId on it, so a
  ;; file of them read as a session took its parent's id (T-349).
  (ignore-errors
   (with-open-file (in path :direction :input :external-format
                       '(:utf-8 :replacement #\?))
     (loop repeat 20000
           for line = (read-line in nil nil)
           while line
           do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                (when (and (plusp (length trimmed)) (char= (char trimmed 0) #\{))
                  (let ((object (ignore-errors (nlk:decode-json trimmed))))
                    (when (and (hash-table-p object)
                               (not (eq t (gethash "isSidechain" object))))
                      (push object objects))))))))
  (setf objects (nreverse objects))
  (cond
    ((null objects) '())
    ;; One session per line: each object carries its own messages.
    ((vectorp (member-of (object-index (first objects)) "messages"))
     (loop for object in objects
           for index = (object-index object)
           for id = (or (text-of index "id" "sessionId") (nlk:short-digest source 10))
           for rows = (loop for row across (or (member-of index "messages") #())
                            when (hash-table-p row)
                              collect (line-message row))
           collect (record-from-messages
                    id world source rows
                    :title (text-of index "title")
                    :model (text-of index "model")
                    :cwd (text-of index "cwd")
                    :started (iso-from-any (or (member-of index "startedAt")
                                               (member-of index "createdAt"))))))
    (t
     (let* ((rows (mapcar #'line-message objects))
            (head (object-index (first objects)))
            (id (or (text-of head "sessionId" "id")
                    (pathname-name path))))
       (list (record-from-messages id world source rows
                                   ;; the first line that names one: Claude Code's transcripts
                                   ;; open with queue-operation lines that carry none
                                   :cwd (loop for object in objects
                                              thereis (text-of (object-index object) "cwd"))
                                   :title (let ((first-user
                                                  (find "user" rows :key (lambda (r) (getf r :role))
                                                                    :test #'equal)))
                                            (and first-user (one-line-of (getf first-user :text) 70)))))))))

;;; --- the sqlite store --------------------------------------------------------

(defun table-columns (db table)
  (mapcar #'second (sqlite:execute-to-list db (format nil "pragma table_info(\"~a\")" table))))

(defun column-like (columns &rest wanted)
  "The first of COLUMNS whose normalized name is one of WANTED."
  (dolist (name wanted)
    (nlk:when-let (found (find (normalize-key name) columns
                               :key #'normalize-key :test #'string=))
      (return found))))

(defun column-containing (columns word)
  (find-if (lambda (name) (cl:search word (normalize-key name))) columns))

(defun quoted (name) (format nil "\"~a\"" name))

(defun db-records (path world source &aux (db (sqlite:connect (namestring path) :busy-timeout 2000)))
  "Every session of the sqlite store at PATH as a RECORD, newest first."
  ;; Columns are matched by name: a store whose tables say nothing is refused
  ;; by name rather than guessed at.
  (nlk:with-cleanup ((ignore-errors (sqlite:disconnect db)))
    (let* ((names (mapcar #'first (sqlite:execute-to-list
                                   db "select name from sqlite_master where type = 'table'")))
           (session-table (column-like names "sessions" "session"))
           (message-table (column-like names "messages" "message")))
      (unless (and session-table message-table)
        (fail "~a holds no session and message tables" path))
      (let* ((session-columns (table-columns db session-table))
             (message-columns (table-columns db message-table))
             (id (or (column-like session-columns "id" "session_id")
                     (fail "~a's ~a table has no id column" path session-table)))
             (title (column-like session-columns "title" "summary" "name"))
             (model (column-like session-columns "model" "model_id"))
             (cwd (column-like session-columns "cwd" "directory" "path" "workdir"))
             (started (or (column-like session-columns "started_at" "created_at" "created" "time"
                                       "timestamp")
                          (column-containing session-columns "created")))
             (reference (or (column-containing message-columns "session")
                            (fail "~a's ~a table names no session" path message-table)))
             (role (or (column-like message-columns "role" "sender" "kind" "type")
                       (fail "~a's ~a table has no role column" path message-table)))
             (content (or (column-like message-columns "content" "text" "body" "parts" "data")
                          (fail "~a's ~a table has no content column" path message-table)))
             (at (or (column-like message-columns "timestamp" "created_at" "created" "time" "at")
                     (column-containing message-columns "created"))))
        (flet ((column (name) (if name (quoted name) "NULL")))
          (let ((rows (sqlite:execute-to-list
                       db (format nil "select ~a, ~a, ~a, ~a, ~a from ~a order by ~a desc"
                                  (quoted id) (column title) (column model) (column cwd)
                                  (column started) (quoted session-table)
                                  (if started (quoted started) (quoted id))))))
            (loop for (session-id title-text model-text cwd-text started-at) in rows
                  when (or (stringp session-id) (integerp session-id))
                    collect (let* ((key (princ-to-string session-id))
                                   (messages
                                     (loop for (role-text content-text message-at)
                                             in (sqlite:execute-to-list
                                                 db (format nil "select ~a, ~a, ~a from ~a ~
                                                                      where ~a = ? order by ~a"
                                                            (quoted role) (quoted content)
                                                            (column at) (quoted message-table)
                                                            (quoted reference)
                                                            (if at (quoted at) "rowid"))
                                                 key)
                                           collect (list :role (and (stringp role-text)
                                                                    (string-downcase role-text))
                                                         :text (message-text content-text)
                                                         :at (seconds-from-any message-at)))))
                              (record-from-messages key world source messages
                                                    :title title-text :model model-text
                                                    :cwd cwd-text
                                                    :started (iso-from-any started-at))))))))))

;;; --- finding the stores ------------------------------------------------------

(defun session-stores (home &key first &aux (found '())
                                            (budget 4000))
  "((KIND . PATH) ...) for every transcript store under the home's roots:
`:sqlite' for a database, `:jsonl' for a transcript file."
  ;; The session directories the config walk refuses to enter are exactly the
  ;; ones walked here. FIRST answers only whether the home carries any store
  ;; at all, one walk that stops at the first and enters no hidden directory:
  ;; what a plan that left the conversations out says, so they read as waiting
  ;; rather than lost.
  (labels ((walk (directory depth)
             (when (and (plusp budget) (<= depth (+ +walk-file-depth+ 2)))
               (dolist (file (or (ignore-errors (uiop:directory-files directory)) '()))
                 (decf budget)
                 (let ((type (string-downcase (or (pathname-type file) ""))))
                   (cond ((member type '("db" "sqlite" "sqlite3") :test #'string=)
                          (push (cons :sqlite file) found))
                         ((string= type "jsonl")
                          (push (cons :jsonl file) found))))
                 (when (and first found)
                   (return-from session-stores t)))
               (dolist (sub (or (ignore-errors (uiop:subdirectories directory)) '()))
                 (let ((name (nlk:folder-name sub)))
                   (unless (or (null name) (skip-directory-p home name first))
                     (walk sub (1+ depth))))))))
    (dolist (root home.roots)
      (when (uiop:directory-pathname-p root) (walk root 0))))
  found)

(defun session-facts (home &key (budget (getf (import-settings) :session-budget 200)))
  "The :SESSIONS facts the home's transcript stores yield, newest store
first, at most BUDGET conversations over all of them."
  ;; A store that will not read is a note, never a stop.
  (let ((stores (sort (session-stores home) #'>
                      :key (lambda (pair) (or (ignore-errors (file-write-date (cdr pair))) 0))))
        (world (home-world-name home))
        (root (uiop:ensure-directory-pathname (first home.roots)))
        (left budget))
    (loop for (kind . path) in stores
          while (plusp left)
          nconc (let* ((source (enough-namestring path root))
                       (records (handler-case
                                    (ecase kind
                                      (:sqlite (db-records path world source))
                                      (:jsonl (jsonl-records path world source)))
                                  (error (condition)
                                    (push (format nil "~a: ~a" source condition) home.notes)
                                    '()))))
                  (loop for record in records
                        when (and (plusp left) record.exchanges)
                          do (decf left)
                          and collect (make-fact :kind :sessions
                                                 :id (format nil "~a-~a" world record.id)
                                                 :world world :source source
                                                 :value (list :record record)))))))
