;;;; board.lisp --- the thread board on Telegram: the forum topics the bot
;;;; opened, by who moves next, in one message it keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; channels.telegram.board_chat names a chat the bot keeps ONE message in,
;;;; edited in place: every open forum topic the bot opened, by who moves next
;;;; -- the sections, a topic's state and when it closes are the kit's
;;;; (NCK:BOARD-ROW); this file knows the topics and draws the message.
;;;;
;;;; A bot can list neither a group's topics nor a topic's messages, so the
;;;; board knows a topic by what passes the bot: the topics it opens, renames,
;;;; closes and deletes (its own writes, which the host's executor shows it,
;;;; NOTE-TELEGRAM-WRITE) and what is said in them (the updates,
;;;; NOTE-TELEGRAM-UPDATE): who spoke last, when, and the bot's message that
;;;; was. The record is kept in the store, one key a topic and an index of
;;;; them, so a restart reads the same topics; it is kept only while a board
;;;; runs. Running and waiting are the kit's lanes, and an answer's waiting
;;;; mark the kit's (NCK:ANSWER-WAITING).
;;;;
;;;; The board closes what is finished (closeForumTopic: the topic stays,
;;;; read-only to all but the group's admins). Show all sends the presser
;;;; every open topic in their own chat with the bot -- Telegram shows a group
;;;; line to no one alone; Close all done, a room operator's, closes every
;;;; Done topic.

(in-package #:nodecode-channel-telegram)

(defparameter +board-pass-seconds+ 60
  "How often the board reads its topics again with nothing new said.")

(defparameter +board-settle-seconds+ 3
  "The least time between two passes: what a turn's burst of posts waits.")

(defparameter +board-closes-per-pass+ 20
  "The most topics one pass closes: a backlog closes over several passes.")

(defparameter +board-state+ "channel-telegram-board"
  "The state namespace the board keeps its message's id in, one key a board
chat.")

(defparameter +topics-state+ "channel-telegram-topics"
  "The state namespace the topics the bot opened are kept in: one key a
topic, CHAT:TOPIC, and \"index\", the keys.")

(defstruct (telegram-board (:copier nil) (:constructor %make-telegram-board))
  (adapter nil)
  (chat "" :type string)
  ;; CHAT:TOPIC -> (:chat :topic :name :opened :said :bot :said-id :question
  ;; :closed): what the bot saw of each topic it opened; :said the moment
  ;; anything was last said there, :bot whether the bot said it, :said-id
  ;; the bot's message that was, :question the line it ended on.
  (topics (make-hash-table :test #'equal))
  (lock (sb-thread:make-mutex :name "channel-telegram-board"))
  (message-id nil)
  ;; What the message said as last sent, so a pass that changes nothing
  ;; edits nothing.
  (sent nil)
  ;; The newest pass's open topics, NCK:BOARD-ROW's rows with :name :chat
  ;; :topic.
  (rows '() :type list)
  ;; Topics past their quiet the newest pass left for the next.
  (closing 0 :type integer)
  (queue (make-work-queue "channel-telegram-board" :cap 16))
  (stop-p nil))

(defvar *telegram-board* nil
  "The live thread board, or NIL when no board_chat is set: the topic notes
are kept only while it runs.")

;;; --- the topics, as the bot saw them --------------------------------------------------

(defun topic-key (chat topic)
  "The key one forum TOPIC is kept under: a topic id is unique only inside
its CHAT."
  (format nil "~a:~a" chat topic))

(defun topic-record-json (record)
  (nlk:encode-json-object
   (nlk:json-object "chat" (getf record :chat) "topic" (getf record :topic)
                    "name" (or (getf record :name) "")
                    "opened" (or (getf record :opened) 0) "said" (or (getf record :said) 0)
                    "bot" (if (getf record :bot) 1 0)
                    "said_id" (or (getf record :said-id) "")
                    "question" (or (getf record :question) "")
                    "closed" (if (getf record :closed) 1 0))))

(defun topic-record-of (json &aux (object (nlk:decode-json json)))
  (flet ((text (key) (let ((value (nlk:json-value object :string key)))
                       (and (plusp (length value)) value)))
         (number (key) (or (nlk:json-value object :integer key) 0)))
    (list :chat (text "chat") :topic (text "topic") :name (text "name")
          :opened (number "opened") :said (number "said") :bot (= 1 (number "bot"))
          :said-id (text "said_id") :question (text "question") :closed (= 1 (number "closed")))))

(defun load-topics (board)
  "Read the topics the store keeps into BOARD."
  (when (nlk:store-open-p)
    (nlk:with-handlers ((error (condition) (warn "telegram: the board could not read its topics: ~a" condition)))
      (let ((index (nlk:session-state-get +topics-state+ "index")))
        (loop for key across (or (and index (nlk:decode-json index)) #())
              for json = (nlk:session-state-get +topics-state+ key)
              when json
                do (setf (gethash key (telegram-board-topics board)) (topic-record-of json)))))))

(defun keep-topic (board key record &aux (topics (telegram-board-topics board)))
  "Under BOARD's lock: RECORD becomes what is known of topic KEY, NIL forgets
it, and the store is told -- the index only when the keys changed."
  (let ((known (nth-value 1 (gethash key topics))))
    (if record
        (setf (gethash key topics) record)
        (remhash key topics))
    (when (nlk:store-open-p)
      (nlk:with-handlers ((error (condition) (warn "telegram: the board could not keep topic ~a: ~a" key condition)))
        (if record
            (nlk:session-state-put +topics-state+ key (topic-record-json record))
            (nlk:session-state-delete +topics-state+ key))
        (unless (eq known (and record t))
          (nlk:session-state-put +topics-state+ "index"
                                 (nlk:encode-json-object
                                  (coerce (loop for key being the hash-keys of topics collect key)
                                          'vector))))))))

(defun note-topic (chat topic &rest changes &key create &allow-other-keys
                   &aux (board *telegram-board*))
  "What the bot saw of TOPIC in CHAT: CHANGES, a plist, over what it knew --
only of a topic it opened (CREATE: one it opens now). NIL when no board
runs."
  (when (and board chat topic)
    (let ((key (topic-key chat topic))
          (changes (loop for (name value) on changes by #'cddr
                         unless (eq name :create) append (list name value))))
      (sb-thread:with-mutex ((telegram-board-lock board))
        (let ((record (gethash key (telegram-board-topics board))))
          (when (or record create)
            (let ((record (copy-list (or record (list :chat chat :topic topic :opened (unix-ms))))))
              (loop for (name value) on changes by #'cddr
                    do (setf (getf record name) value))
              (keep-topic board key (unless (getf changes :gone) record)))
            (queue-push (telegram-board-queue board) :pass)
            t))))))

(defun body-field (body name)
  "Field NAME of a plan's BODY: a JSON object, or a multipart alist."
  (cond ((hash-table-p body) (gethash name body))
        ((listp body) (cdr (assoc name body :test #'equal)))))

(defun said-question (body)
  "The line a post's BODY ends on, as the board shows it, or NIL: a card's
HTML is no words of the conversation's."
  (let* ((rich (body-field body "rich_message"))
         (text (cond ((hash-table-p rich) (gethash "markdown" rich))
                     (t (body-field body "caption")))))
    (and (stringp text) (board-question text))))

(defun note-telegram-write (plan result &aux (body (request-plan-body plan)))
  "What a write the bot made says of its topics: one it opened, renamed,
closed, reopened or deleted, or a post in one -- the bot spoke last there."
  ;; A card (a rich message's HTML) is the turn's chrome, not words said:
  ;; the answer that follows it is.
  (when (and *telegram-board* (execution-ok-p result))
    (let* ((method (telegram-method-name (request-plan-path plan)))
           (chat (number-string (body-field body "chat_id")))
           (topic (number-string (body-field body "message_thread_id")))
           (answer (execution-body result))
           (now (unix-ms)))
      (cond ((equal method "createForumTopic")
             (note-topic chat (topic-id-of answer) :create t
                         :name (nlk:json-value answer :string "result" "name") :said now))
            ((equal method "editForumTopic") (note-topic chat topic :name (body-field body "name")))
            ((equal method "closeForumTopic") (note-topic chat topic :closed t))
            ((equal method "reopenForumTopic") (note-topic chat topic :closed nil :said now))
            ((equal method "deleteForumTopic") (note-topic chat topic :gone t))
            ((and topic (member method '("sendRichMessage" "sendMessage" "sendPhoto" "sendDocument"
                                         "sendVoice" "sendMediaGroup")
                                :test #'equal)
                  (not (let ((rich (body-field body "rich_message")))
                         (and (hash-table-p rich) (gethash "html" rich)))))
             (let ((result (nlk:json-value answer :any "result")))
               (note-topic chat topic :said now :bot t
                           :said-id (number-string
                                     (nlk:json-value (if (vectorp result) (aref result 0) result)
                                                     :any "message_id"))
                           :question (said-question body))))))))

(defun note-telegram-update (adapter update)
  "What an update says of the bot's topics: a person spoke in one, or it was
closed, reopened or renamed."
  (let ((message (gethash "message" update)))
    (when (and *telegram-board* (hash-table-p message) (gethash "message_thread_id" message))
      (let ((chat (number-string (nlk:json-value message :any "chat" "id")))
            (topic (number-string (gethash "message_thread_id" message)))
            (said (* 1000 (or (nlk:json-value message :integer "date") 0))))
        (cond ((gethash "forum_topic_closed" message) (note-topic chat topic :closed t))
              ((gethash "forum_topic_reopened" message) (note-topic chat topic :closed nil :said said))
              ((gethash "forum_topic_edited" message)
               (nlk:when-let (name (nlk:json-value message :string "forum_topic_edited" "name"))
                 (note-topic chat topic :name name)))
              ((equal (number-string (nlk:json-value message :any "from" "id"))
                      (telegram-adapter-bot-user-id adapter)))
              (t (note-topic chat topic :said said :bot nil :said-id nil :question nil)))))))

(defun board-noting-executor (inner)
  "INNER, every write it carries out told to the board (NOTE-TELEGRAM-WRITE)."
  (make-plan-executor
   :run (lambda (plan &aux (result (execute-plan inner plan)))
          (nlk:with-handlers ((error (condition) (warn "telegram: the board could not note a write: ~a" condition)))
            (note-telegram-write plan result))
          result)))

;;; --- the message -------------------------------------------------------------------------

(defun topic-link (chat topic)
  "The t.me link that opens TOPIC in a supergroup CHAT (-100...), or NIL."
  (and (stringp chat) (> (length chat) 4) (string= "-100" chat :end2 4)
       (format nil "https://t.me/c/~a/~a" (subseq chat 4) topic)))

(defun telegram-board-line (row question-p)
  "ROW as the board and Show all say it: the topic's name as its link, its
age -- running or waiting for one whose turn does -- and, with QUESTION-P,
the question it waits on under it."
  (let ((name (remove-if (lambda (ch) (find ch "[]")) (or (getf row :name) (format nil "topic ~a" (getf row :topic)))))
        (link (topic-link (getf row :chat) (getf row :topic))))
    (format nil "~:[~a~*~;[~a](~a)~] · ~a~@[~%↳ ~a~]"
            link name link (board-when row)
            (and question-p (getf row :question)))))

(defun telegram-board-text (rows &key (closing 0))
  "ROWS, the open topics newest first, as the board's words: a heading; each
section holding a topic, its name and count over its newest +BOARD-ROWS+
(Needs you's each with the question it waits on); the counts last."
  (format nil "**Threads**~%~%~{~a~^~%~%~}~%~%_~d open~[~:;~:* · ~d more closing, quiet too long~] · done topics close after ~d h quiet, any after ~d days_"
          (or (loop for (key name) in +board-sections+
                    for members = (board-section-rows rows key)
                    for shown = (subseq members 0 (min +board-rows+ (length members)))
                    when members
                      collect (format nil "**~a** · ~d~%~{~a~^~%~}~@[~%+~d more: Show all~]"
                                      name (length members)
                                      (mapcar (lambda (row) (telegram-board-line row (eq key :needs))) shown)
                                      (and (> (length members) (length shown))
                                           (- (length members) (length shown)))))
              (list "No topic is open."))
          (length rows) closing
          (floor +board-done-close-ms+ 3600000) (floor +board-quiet-close-ms+ 86400000)))

(defun telegram-board-controls (rows &aux (done (length (board-section-rows rows :done))))
  "The board's buttons, one row: Show all, and Close all done while any is."
  (list (append (list (list "Show all" "nct:board:all"))
                (and (plusp done)
                     (list (list (format nil "Close all done (~d)" done) "nct:board:done" :danger))))))

;;; --- the board's pass -------------------------------------------------------------------

(defun board-execute (board plan)
  (execute-plan (host-executor (telegram-adapter-host (telegram-board-adapter board))) plan))

(defun board-close-topic (board row)
  "Close ROW's topic (closeForumTopic). => whether it closed."
  (let ((result (board-execute board (topic-close-plan (getf row :chat) (getf row :topic)))))
    (or (execution-ok-p result)
        (progn (warn "telegram: the board could not close topic ~a: ~a" (getf row :id) (execution-error result))
               nil))))

(defun telegram-board-pass (board &aux (host (telegram-adapter-host (telegram-board-adapter board)))
                                       (now (unix-ms)))
  "Sort the open topics, close those past their quiet, and say the rest in
the board's message. => the open topics' rows."
  (let ((phases (board-lane-phases host :key (lambda (target &aux (topic (getf target :thread-id)))
                                               (and topic (topic-key (getf target :channel-id) topic)))))
        (records (sb-thread:with-mutex ((telegram-board-lock board))
                   (loop for key being the hash-keys of (telegram-board-topics board) using (hash-value record)
                         unless (getf record :closed) collect (cons key record))))
        (rows '())
        (closing '()))
    (loop for (key . record) in records
          for topic = (getf record :topic)
          for said-id = (getf record :said-id)
          do (multiple-value-bind (row close-p)
                 (board-row key :phase (gethash key phases) :bot-p (getf record :bot)
                                :text (getf record :question)
                                :quiet-ms (- now (max (getf record :said) (getf record :opened)))
                                ;; The mark is read each pass, never kept.
                                :waits (and (getf record :bot) said-id (answer-waiting host topic said-id)))
               (let ((row (and row (append row (list :name (getf record :name) :chat (getf record :chat)
                                                     :topic topic)))))
                 (cond (row (push row rows))
                       (close-p (push (list :id key :chat (getf record :chat) :topic topic) closing))))))
    (let ((closed (loop for row in closing
                        repeat +board-closes-per-pass+
                        count (board-close-topic board row))))
      (setf (telegram-board-rows board) (sort rows #'< :key (lambda (row) (getf row :quiet-ms)))
            (telegram-board-closing board) (- (length closing) closed)))
    (telegram-board-publish board)
    (telegram-board-rows board)))

(defun message-gone-p (result)
  "Whether an edit's RESULT says its message is no longer there to edit."
  (let ((error (or (execution-error result) "")))
    (or (search "message to edit not found" error)
        (search "MESSAGE_ID_INVALID" error))))

(defun telegram-board-publish (board &aux (chat (telegram-board-chat board)) (target (list :channel-id chat)))
  "Say the newest pass in the board's message: edit it in place, or post it
when there is none yet or it was deleted."
  (let* ((rows (telegram-board-rows board))
         (text (telegram-board-text rows :closing (telegram-board-closing board)))
         (controls (telegram-board-controls rows))
         (said (format nil "~a~%~s" text controls))
         (id (telegram-board-message-id board)))
    (unless (equal said (telegram-board-sent board))
      (let ((edited (and id (board-execute board (edit-message-plan target id text :controls controls :retry t)))))
        (cond ((and edited (execution-ok-p edited)) (setf (telegram-board-sent board) said))
              ;; Only a message that is gone is posted again: any other refusal
              ;; keeps the one board, and the next pass tries the edit again.
              ((or (null edited) (message-gone-p edited))
               (let ((posted (board-execute board (first (send-message-plan target (first (split-text-chunks text +telegram-message-text-limit+))
                                                                           :controls controls)))))
                 (if (execution-ok-p posted)
                     (let ((new (telegram-message-id (execution-body posted))))
                       (setf (telegram-board-message-id board) new (telegram-board-sent board) said)
                       (when (nlk:store-open-p)
                         (nlk:session-state-put +board-state+ chat new)))
                     (warn "telegram: the board could not post in ~a: ~a" chat (execution-error posted)))))
              (t (warn "telegram: the board could not edit its message: ~a" (execution-error edited))))))))

;;; --- its buttons --------------------------------------------------------------------------

(defun telegram-board-pressed (adapter callback &aux (data (gethash "data" callback)) (board *telegram-board*))
  "Poll thread: a press on one of the board's own buttons, answered on a
thread of its own -- Show all sends the presser every open topic in their
own chat with the bot; Close all done, a room operator's, closes every Done
topic -- or NIL for any other press."
  (when (and board (stringp data) (eql 0 (search "nct:board:" data)))
    (let ((query (number-string (gethash "id" callback)))
          (user (number-string (nlk:json-value callback :any "from" "id"))))
      (nlk:spawn "channel-telegram-board-press"
        (nlk:with-handlers ((error (condition) (warn "telegram: a board press failed: ~a" condition)))
          (let* ((host (telegram-adapter-host adapter))
                 (rows (telegram-board-rows board))
                 (done (board-section-rows rows :done))
                 (closer (and (equal data "nct:board:done") (operator-p host user)))
                 (said (cond ((equal data "nct:board:all")
                              (let ((sent (board-execute board (first (send-message-plan
                                                                        (list :channel-id user)
                                                                        (first (split-text-chunks
                                                                                (board-list-text rows #'telegram-board-line)
                                                                                +telegram-message-text-limit+)))))))
                                (if (execution-ok-p sent)
                                    "Every open topic is in your chat with the bot."
                                    "Open a chat with the bot first: Telegram lets a bot write only to who wrote to it.")))
                             ((not (equal data "nct:board:done")) "This button is out of date.")
                             ((not closer) "Only the room's operators can close topics from the board.")
                             ((null done) "No topic is done.")
                             (t (format nil "Closing ~d done topic~:p." (length done))))))
            (execute-delivery (answer-callback-plans (list :id query) :text said) (host-executor host))
            (when (and closer done)
              (dolist (row done) (board-close-topic board row))
              (queue-push (telegram-board-queue board) :pass)))))
      t)))

(defun start-telegram-board (adapter section &aux (chat (config-string section "board_chat")))
  "Start the thread board channels.telegram.board_chat names, on a thread of
its own. => its stop thunk, or NIL when no board chat is named."
  (when (plusp (length chat))
    (let ((board (%make-telegram-board :adapter adapter :chat chat
                                       :message-id (and (nlk:store-open-p)
                                                        (nlk:session-state-get +board-state+ chat)))))
      (load-topics board)
      ;; One board edits the message: a board already running stands down.
      (nlk:when-let (old *telegram-board*)
        (setf (telegram-board-stop-p old) t)
        (queue-push (telegram-board-queue old) :stop))
      (setf *telegram-board* board)
      ;; /threads is the kit's; it answers this board's list.
      (let ((thunk (register-board "telegram" (lambda ()
                                                (board-list-text (telegram-board-rows board)
                                                                 #'telegram-board-line)))))
        (nlk:spawn "channel-telegram-board"
          (loop until (telegram-board-stop-p board)
                do (nlk:with-handlers ((error (condition) (warn "telegram: a board pass failed: ~a" condition)))
                     (telegram-board-pass board))
                   (queue-pop (telegram-board-queue board) +board-pass-seconds+)
                   ;; A burst of notes -- a turn's posts -- is one pass.
                   (sleep +board-settle-seconds+)))
        (lambda ()
          (setf (telegram-board-stop-p board) t)
          (queue-push (telegram-board-queue board) :stop)
          (unregister-board "telegram" thunk)
          (when (eq *telegram-board* board)
            (setf *telegram-board* nil)))))))
