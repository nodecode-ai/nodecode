;;;; board.lisp --- the thread board on Slack: the threads the bot answers
;;;; in, by who moves next, in one message it keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; channels.slack.board_channel names a channel the bot keeps ONE message
;;;; in, edited in place: every open thread the bot answers in, by who moves
;;;; next -- the sections, a thread's state and when it leaves are the kit's
;;;; (NCK:BOARD-ROW); this file knows the threads and draws the message.
;;;;
;;;; A Slack thread is the replies under one message, and the bot's answer
;;;; opens one under its ask, so the board knows a thread by what passes the
;;;; bot: its own posts in one, which the host's executor shows it
;;;; (NOTE-SLACK-WRITE -- a turn's card puts the thread on the board, any
;;;; other post says the bot spoke last), and what people say there (the
;;;; events, NOTE-SLACK-MESSAGE). A DM is one person's and stays off it. A
;;;; thread's name is the first line of the message it hangs off and its
;;;; link Slack's permalink, each read once (conversations.replies,
;;;; chat.getPermalink). What the board knows is kept in the store as one
;;;; value, so a restart reads the same threads; it is kept only while a
;;;; board runs. Running and waiting are the kit's lanes, and an answer's
;;;; waiting mark the kit's (NCK:ANSWER-WAITING).
;;;;
;;;; Slack closes no thread, so a finished one leaves the board: a Done
;;;; thread after 48 hours quiet, any other after 7 days. Show all shows the
;;;; presser every open thread in a message only they see; Clear done, a room
;;;; operator's, takes every Done thread off the board.

(in-package #:nodecode-channel-slack)

(defparameter +board-pass-seconds+ 60
  "How often the board reads its threads again with nothing new said.")

(defparameter +board-settle-seconds+ 3
  "The least time between two passes: what a turn's burst of posts waits.")

(defparameter +board-reads-per-pass+ 10
  "The most threads one pass reads the name and link of, newest first: a
backlog is read over several passes.")

(defparameter +board-kept+ 500
  "The most threads the board keeps, newest first: what one stored value
holds.")

(defparameter +board-state+ "channel-slack-board"
  "The state the board keeps: \"threads\", every thread it knows as one JSON
array, and \"message:<channel>\", the ts of its message in that channel.")

(defstruct (slack-board (:copier nil) (:constructor %make-slack-board))
  (adapter nil)
  (channel "" :type string)
  ;; "<channel>:<root ts>" -> (:channel :root :opened :said :bot :said-id
  ;; :question :read :name :link): what the bot saw of each thread it answers
  ;; in; :said the moment anything was last said there, :bot whether the bot
  ;; said it, :said-id the bot's message that was, :question the line it
  ;; ended on; :read once the name and the link were asked for.
  (threads (make-hash-table :test #'equal))
  (lock (sb-thread:make-mutex :name "channel-slack-board"))
  ;; Whether THREADS changed since the store was last told.
  (dirty nil)
  (message-id nil)
  ;; What the message said as last sent, so a pass that changes nothing
  ;; edits nothing.
  (sent nil)
  ;; The newest pass's open threads, NCK:BOARD-ROW's rows with :channel
  ;; :root :name :link.
  (rows '() :type list)
  (queue (make-work-queue "channel-slack-board" :cap 16))
  (stop-p nil))

(defvar *slack-board* nil
  "The live thread board, or NIL when no board_channel is set: threads are
noted only while it runs.")

;;; --- the threads, as the bot saw them -------------------------------------------------

(defun thread-said (record)
  "The newest moment anything happened in RECORD's thread."
  (max (or (getf record :said) 0) (or (getf record :opened) 0)))

(defun board-threads-json (records)
  (nlk:encode-json-object
   (coerce (loop for record in records
                 collect (nlk:json-object
                          "channel" (getf record :channel) "root" (getf record :root)
                          "opened" (or (getf record :opened) 0) "said" (or (getf record :said) 0)
                          "bot" (if (getf record :bot) 1 0)
                          "said_id" (or (getf record :said-id) "")
                          "question" (or (getf record :question) "")
                          "read" (if (getf record :read) 1 0)
                          "name" (or (getf record :name) "") "link" (or (getf record :link) "")))
           'vector)))

(defun thread-record-of (object)
  (flet ((text (key) (let ((value (nlk:json-value object :string key)))
                       (and (plusp (length value)) value)))
         (number (key) (or (nlk:json-value object :integer key) 0)))
    (list :channel (text "channel") :root (text "root")
          :opened (number "opened") :said (number "said") :bot (= 1 (number "bot"))
          :said-id (text "said_id") :question (text "question")
          :read (= 1 (number "read")) :name (text "name") :link (text "link"))))

(defun load-board (board &aux (channel (slack-board-channel board)))
  "Read what the store keeps of BOARD: its threads, and its message in its
channel."
  (when (nlk:store-open-p)
    (nlk:with-handlers ((error (condition) (warn "slack: the board could not read its threads: ~a" condition)))
      (let ((threads (nlk:session-state-get +board-state+ "threads"))
            (message (nlk:session-state-get +board-state+ (format nil "message:~a" channel))))
        (loop for object across (or (and threads (nlk:decode-json threads)) #())
              for record = (thread-record-of object)
              when (and (getf record :channel) (getf record :root))
                do (setf (gethash (book-key (getf record :channel) (getf record :root))
                                  (slack-board-threads board))
                         record))
        (setf (slack-board-message-id board)
              (and message (nlk:json-value (nlk:decode-json message) :string "ts")))))))

(defun save-board (board &aux (records '()) (dirty nil))
  "Tell the store what BOARD knows of its threads, when that changed: the
newest +BOARD-KEPT+, the rest forgotten."
  (sb-thread:with-mutex ((slack-board-lock board))
    (when (slack-board-dirty board)
      (let* ((table (slack-board-threads board))
             (all (sort (loop for key being the hash-keys of table using (hash-value record)
                              collect (cons key record))
                        #'> :key (lambda (entry) (thread-said (cdr entry))))))
        (dolist (entry (nthcdr +board-kept+ all))
          (remhash (car entry) table))
        (setf records (mapcar #'cdr (subseq all 0 (min +board-kept+ (length all))))
              dirty t
              (slack-board-dirty board) nil))))
  (when (and dirty (nlk:store-open-p))
    (nlk:with-handlers ((error (condition) (warn "slack: the board could not keep its threads: ~a" condition)))
      (nlk:session-state-put +board-state+ "threads" (board-threads-json records)))))

(defun set-thread (board key changes &key create channel root)
  "Under BOARD's lock: CHANGES, a plist, over what BOARD knows of thread KEY
-- one it knows, or, with CREATE, one in CHANNEL that ROOT starts. =>
whether the thread is on the board."
  (sb-thread:with-mutex ((slack-board-lock board))
    (let ((record (gethash key (slack-board-threads board))))
      (when (or record create)
        (let ((record (copy-list (or record (list :channel channel :root root :opened (unix-ms))))))
          (loop for (name value) on changes by #'cddr
                do (setf (getf record name) value))
          (setf (gethash key (slack-board-threads board)) record
                (slack-board-dirty board) t))
        t))))

(defun forget-thread (board key)
  "Take thread KEY off BOARD."
  (sb-thread:with-mutex ((slack-board-lock board))
    (when (remhash key (slack-board-threads board))
      (setf (slack-board-dirty board) t))))

(defun note-thread (channel root &rest changes &key create &allow-other-keys
                    &aux (board *slack-board*))
  "What the bot saw of the thread ROOT starts in CHANNEL: CHANGES, a plist,
over what it knew -- only of a thread on the board (CREATE: one the bot
posts in now; never a DM's). NIL when no board runs."
  (when (and board channel root (not (dm-channel-p channel)))
    (when (set-thread board (book-key channel root)
                      (loop for (name value) on changes by #'cddr
                            unless (eq name :create) append (list name value))
                      :create create :channel channel :root root)
      (queue-push (slack-board-queue board) :pass)
      t)))

(defun posted-words (body)
  "The words a post's BODY says: its markdown block's, else its text."
  (and (hash-table-p body)
       (or (loop for block across (let ((blocks (gethash "blocks" body)))
                                    (if (vectorp blocks) blocks #()))
                 when (and (hash-table-p block) (equal "markdown" (gethash "type" block)))
                   return (gethash "text" block))
           (gethash "text" body))))

(defun note-slack-write (plan result &aux (body (and (typep plan 'request-plan) (request-plan-body plan))))
  "What a write the bot made says of its threads: a post in one. A turn's
card (the post SEND_CARD labels) puts its thread on the board; any other
post there, an answer's words or files, says the bot spoke last. PLAN NIL
-- a typing beat Slack cannot show -- says nothing."
  (when (and *slack-board* (hash-table-p body) (execution-ok-p result))
    (multiple-value-bind (channel thread words)
        (cond ((equal (request-plan-path plan) "/chat.postMessage")
               ;; an answer's chunk, or a turn's card
               (values (gethash "channel" body) (gethash "thread_ts" body) (posted-words body)))
              ((equal (request-plan-method plan) +upload-method+)
               (let ((message (gethash "message" body)))
                 (values (gethash "channel_id" body)
                         (or (and (hash-table-p message) (gethash "thread_ts" message))
                             (gethash "thread_ts" body))
                         (posted-words message)))))
      (when (and channel thread)
        (if (equal (request-plan-audit-label plan) "send_card")
            (note-thread channel thread :create t :said (unix-ms))
            (note-thread channel thread :create t :said (unix-ms) :bot t
                         :said-id (slack-message-id (execution-body result))
                         :question (and words (board-question words))))))))

(defun note-slack-message (adapter message)
  "What a message event says of the bot's threads: someone other than the
bot spoke in one."
  (when *slack-board*
    (let ((channel (nlk:json-value message :string "channel"))
          (ts (nlk:json-value message :string "ts"))
          (root (nlk:json-value message :string "thread_ts"))
          (subtype (nlk:json-value message :string "subtype")))
      (when (and channel ts root (not (equal ts root))
                 (or (null subtype) (member subtype '("file_share" "thread_broadcast" "me_message")
                                            :test #'equal))
                 (null (nlk:json-value message :string "bot_id"))
                 (not (equal (nlk:json-value message :string "user") (slack-adapter-bot-user-id adapter))))
        (note-thread channel root :said (ts-ms ts) :bot nil :said-id nil :question nil)))))

(defun board-noting-executor (inner)
  "INNER, every write it carries out told to the board (NOTE-SLACK-WRITE)."
  (make-plan-executor
   :run (lambda (plan &aux (result (execute-plan inner plan)))
          (nlk:with-handlers ((error (condition) (warn "slack: the board could not note a write: ~a" condition)))
            (note-slack-write plan result))
          result)))

(defun lane-thread-key (book target &aux (channel (getf target :channel-id))
                                         (message (getf target :message-id)))
  "The board's key of the thread a lane whose TARGET is that answers in: the
thread its ask lives in, or the one the answer opens under it."
  (and channel message
       (book-key channel (or (thread-root book channel message) message))))

;;; --- names and links ---------------------------------------------------------------------

(defun thread-name (adapter text)
  "The name the board gives a thread whose first message says TEXT: its
first line as a person reads it, the bot's own mention left out and anyone
else's as their name."
  (let* ((plain (slack-plain-text (slack-strip-mention (or text "") (slack-adapter-bot-user-id adapter))))
         (named (ppcre:regex-replace-all "<@([A-Z0-9]+)(?:\\|[^>]*)?>" plain
                                         (lambda (match id)
                                           (declare (ignore match))
                                           (format nil "@~a" (or (slack-user-name adapter id) id)))
                                         :simple-calls t))
         (line (find-if (lambda (line) (plusp (length line)))
                        (mapcar (lambda (line) (nlk:trimmed (ppcre:regex-replace "^[-*>#]+\\s*" (nlk:trimmed line) "")))
                                (uiop:split-string named :separator '(#\Newline))))))
    (and line (nlk:clip (remove-if (lambda (ch) (find ch "[]`*")) line) 60 :ellipsis "…"))))

(defun board-execute (board plan)
  (execute-plan (host-executor (slack-adapter-host (slack-board-adapter board))) plan))

(defun read-thread (board channel root)
  "What Slack says of the thread ROOT starts in CHANNEL: (values NAME LINK),
its first message's first line (THREAD-NAME) and its permalink, NIL for
either Slack would not say."
  (let* ((said (board-execute board (rest-plan "GET" (format nil "/conversations.replies?channel=~a&ts=~a&limit=1"
                                                           channel root)
                                             "board_thread_root" nil 10)))
         (messages (and (execution-ok-p said) (nlk:json-array (execution-body said) "messages")))
         (first (and messages (plusp (length messages)) (aref messages 0)))
         (linked (board-execute board (rest-plan "GET" (format nil "/chat.getPermalink?channel=~a&message_ts=~a"
                                                             channel root)
                                               "board_thread_link" nil 10))))
    (values (and (hash-table-p first) (equal root (nlk:json-value first :string "ts"))
                 (thread-name (slack-board-adapter board) (nlk:json-value first :string "text")))
            (and (execution-ok-p linked)
                 (let ((link (nlk:json-value (execution-body linked) :string "permalink")))
                   (and (plusp (length link)) link))))))

(defun read-thread-names (board rows)
  "Read the name and link of ROWS' threads not read yet, the newest
+BOARD-READS-PER-PASS+ of them. => ROWS, each named and linked as its
thread now is."
  (let ((unread (sb-thread:with-mutex ((slack-board-lock board))
                  (loop for row in rows
                        for record = (gethash (getf row :id) (slack-board-threads board))
                        when (and record (not (getf record :read)))
                          collect row))))
    (loop for row in unread
          repeat +board-reads-per-pass+
          do (multiple-value-bind (name link) (read-thread board (getf row :channel) (getf row :root))
               (set-thread board (getf row :id) (list :read t :name name :link link))))
    (sb-thread:with-mutex ((slack-board-lock board))
      (loop for row in rows
            for record = (gethash (getf row :id) (slack-board-threads board))
            collect (append (list :name (getf record :name) :link (getf record :link)) row)))))

;;; --- the message -------------------------------------------------------------------------

(defun slack-board-line (row question-p)
  "ROW as the board and Show all say it: an item, the thread's name as its
link and its age -- running or waiting for one whose turn does -- and, with
QUESTION-P, the question it waits on under it."
  (let ((name (or (getf row :name) "A thread"))
        (link (getf row :link)))
    (format nil "- ~:[~a~*~;[~a](~a)~] · ~a~@[~%  ↳ ~a~]"
            link name link (board-when row)
            (and question-p (getf row :question)))))

(defun slack-board-text (rows)
  "ROWS, the open threads newest first, as the board's words: a heading;
each section holding a thread, its name and count over its newest
+BOARD-ROWS+ (Needs you's each with the question it waits on); under them,
small (SLACK-ANSWER-BODY's footer), the count and when a thread leaves."
  (slack-answer-body
   (format nil "**Threads**~%~%~{~a~^~%~%~}"
           (or (loop for (key name) in +board-sections+
                     for members = (board-section-rows rows key)
                     for shown = (subseq members 0 (min +board-rows+ (length members)))
                     when members
                       collect (format nil "**~a** · ~d~%~{~a~^~%~}~@[~%+~d more: Show all~]"
                                       name (length members)
                                       (mapcar (lambda (row) (slack-board-line row (eq key :needs))) shown)
                                       (and (> (length members) (length shown))
                                            (- (length members) (length shown)))))
               (list "No thread is open.")))
   (format nil "~d open · a done thread leaves the board after ~d h quiet, any after ~d days"
           (length rows) (floor +board-done-close-ms+ 3600000) (floor +board-quiet-close-ms+ 86400000))))

(defun slack-board-controls (rows &aux (done (length (board-section-rows rows :done))))
  "The board's buttons, one row: Show all, and Clear done while any is."
  (list (append (list (list "Show all" "ncs:board:all"))
                (and (plusp done)
                     (list (list (format nil "Clear done (~d)" done) "ncs:board:done" :danger))))))

(defun slack-board-publish (board &aux (channel (slack-board-channel board))
                                       (target (list :channel-id channel)))
  "Say the newest pass in the board's message: edit it in place, or post it
when there is none yet or it was deleted."
  (let* ((rows (slack-board-rows board))
         (text (slack-board-text rows))
         (controls (slack-board-controls rows))
         (said (format nil "~a~%~s" text controls))
         (id (slack-board-message-id board)))
    (unless (equal said (slack-board-sent board))
      (let ((edited (and id (board-execute board (edit-plan target id text :controls controls :retry t)))))
        (cond ((and edited (execution-ok-p edited) (execution-body edited))
               (setf (slack-board-sent board) said))
              ;; Only a message that is gone is posted again -- the Slack
              ;; executor settles message_not_found as done, with no body --
              ;; and any other refusal keeps the one board: the next pass
              ;; tries the edit again.
              ((or (null edited) (execution-ok-p edited))
               (let ((posted (board-execute board (api-plan "chat.postMessage"
                                                            (message-body target text :controls controls)
                                                            "board_post" :retry t))))
                 (if (execution-ok-p posted)
                     (let ((new (slack-message-id (execution-body posted))))
                       (setf (slack-board-message-id board) new
                             (slack-board-sent board) said)
                       (when (nlk:store-open-p)
                         (nlk:session-state-put +board-state+ (format nil "message:~a" channel)
                                                (nlk:encode-json-object (nlk:json-object "ts" new)))))
                     (warn "slack: the board could not post in ~a: ~a" channel (execution-error posted)))))
              (t (warn "slack: the board could not edit its message: ~a" (execution-error edited))))))))

;;; --- the board's pass ------------------------------------------------------------------

(defun slack-board-pass (board &aux (adapter (slack-board-adapter board))
                                    (host (slack-adapter-host adapter))
                                    (now (unix-ms)))
  "Sort the open threads, take those past their quiet off the board, read
the names and links not read yet, and say the rest in the board's message.
=> the open threads' rows."
  (let ((phases (board-lane-phases host :key (lambda (target)
                                               (lane-thread-key (slack-adapter-threads adapter) target))))
        (records (sb-thread:with-mutex ((slack-board-lock board))
                   (loop for key being the hash-keys of (slack-board-threads board) using (hash-value record)
                         collect (cons key record))))
        (rows '()))
    (loop for (key . record) in records
          for said-id = (getf record :said-id)
          do (multiple-value-bind (row leave-p)
                 (board-row key :phase (gethash key phases) :bot-p (getf record :bot)
                                :text (getf record :question)
                                :quiet-ms (- now (thread-said record))
                                ;; The mark is read each pass, never kept;
                                ;; a Slack lane's room is its channel.
                                :waits (and (getf record :bot) said-id
                                            (answer-waiting host (getf record :channel) said-id)))
               (cond (row (push (append row (list :channel (getf record :channel) :root (getf record :root)))
                                rows))
                     (leave-p (forget-thread board key)))))
    (setf (slack-board-rows board)
          (read-thread-names board (sort rows #'< :key (lambda (row) (getf row :quiet-ms)))))
    (save-board board)
    (slack-board-publish board)
    (slack-board-rows board)))

;;; --- its buttons --------------------------------------------------------------------------

(defun slack-board-pressed (adapter press &aux (data (getf press :data)) (board *slack-board*))
  "Socket thread: a PRESS on one of the board's own buttons, answered on a
thread of its own -- Show all shows the presser every open thread in a
message only they see; Clear done, a room operator's, takes every Done
thread off the board -- or NIL for any other press."
  (when (and board (stringp data) (eql 0 (search "ncs:board:" data)))
    (nlk:spawn "channel-slack-board-press"
      (nlk:with-handlers ((error (condition) (warn "slack: a board press failed: ~a" condition)))
        (let* ((host (slack-adapter-host adapter))
               (rows (slack-board-rows board))
               (done (board-section-rows rows :done))
               (clearer (and (equal data "ncs:board:done") (operator-p host (getf press :user-id))))
               (said (cond ((equal data "ncs:board:all")
                            (board-list-text rows #'slack-board-line :limit 11000))
                           ((not (equal data "ncs:board:done")) "This button is out of date.")
                           ((not clearer) "Only the room's operators can clear the board.")
                           ((null done) "No thread is done.")
                           (t (format nil "Cleared ~d done thread~:p from the board." (length done))))))
          (when (and clearer done)
            (dolist (row done) (forget-thread board (getf row :id)))
            (queue-push (slack-board-queue board) :pass))
          (dolist (plan (ack-plan press :text said))
            (board-execute board plan)))))
    t))

(defun start-slack-board (adapter section &aux (channel (config-string section "board_channel")))
  "Start the thread board channels.slack.board_channel names, on a thread of
its own. => its stop thunk, or NIL when no board channel is named."
  (when (plusp (length channel))
    (let ((board (%make-slack-board :adapter adapter :channel channel)))
      (load-board board)
      ;; One board edits the message: a board already running stands down.
      (nlk:when-let (old *slack-board*)
        (setf (slack-board-stop-p old) t)
        (queue-push (slack-board-queue old) :stop))
      (setf *slack-board* board)
      ;; /threads is the kit's; it answers this board's list.
      (let ((thunk (register-board "slack" (lambda ()
                                             (board-list-text (slack-board-rows board) #'slack-board-line)))))
        (nlk:spawn "channel-slack-board"
          (loop until (slack-board-stop-p board)
                do (nlk:with-handlers ((error (condition) (warn "slack: a board pass failed: ~a" condition)))
                     (slack-board-pass board))
                   (queue-pop (slack-board-queue board) +board-pass-seconds+)
                   ;; A burst of notes -- a turn's posts -- is one pass.
                   (unless (slack-board-stop-p board)
                     (sleep +board-settle-seconds+))))
        (lambda ()
          (setf (slack-board-stop-p board) t)
          (queue-push (slack-board-queue board) :stop)
          (unregister-board "slack" thunk)
          (when (eq *slack-board* board)
            (setf *slack-board* nil)))))))
