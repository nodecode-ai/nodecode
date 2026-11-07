;;;; board-test.lisp --- the thread board on Telegram.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The board knows a topic only by what passes the bot: the topics it
;;;; opens and what is said in them, its own posts and the updates. A pass
;;;; sorts them by who moves next and keeps one message saying so, edited in
;;;; place; its buttons answer the presser.

(in-package #:nodecode.test)

(defun board-test-executor (plans)
  "An executor recording every plan in the cons PLANS (its car, newest
first) and answering as the Bot API does: a topic opened, a message posted
with the next id, anything else acknowledged."
  (let ((next 89))
    (nck:make-plan-executor
     :run (lambda (plan &aux (path (nck:request-plan-path plan)))
            (push plan (car plans))
            (nck:make-execution
             :ok-p t :status 200 :attempts 1
             :body (cond ((search "createForumTopic" path)
                          (cell-json "{\"ok\": true, \"result\": {\"message_thread_id\": 7, \"name\": \"Fix the build\"}}"))
                         ((search "/send" path)
                          (cell-json (format nil "{\"ok\": true, \"result\": {\"message_id\": ~d}}" (incf next))))
                         (t (cell-json "{\"ok\": true, \"result\": true}"))))))))

(defun board-test-update (&rest fields)
  "A message update in topic 7 of -100555 carrying FIELDS, a plist of its
keys and values."
  (let ((message (nlk:json-object "message_id" 50 "message_thread_id" 7 "date" 1700000000
                                  "chat" (nlk:json-object "id" -100555 "type" "supergroup" "is_forum" t))))
    (loop for (key value) on fields by #'cddr
          do (setf (gethash key message) value))
    (nlk:json-object "update_id" 1 "message" message)))

(defmacro with-test-board ((board adapter plans) &body body)
  "BODY with BOARD the running board of ADAPTER -- a host whose writes the
board is told -- keeping its message in -100555, PLANS the cons the
executor records into."
  `(let* ((,plans (list '()))
          (,adapter (telegram-gap-adapter :executor (nct:board-noting-executor (board-test-executor ,plans))))
          (,board (nct::%make-telegram-board :adapter ,adapter :chat "-100555"))
          (nct:*telegram-board* ,board))
     ,@body))

(defun board-test-say (adapter plan)
  "Carry PLAN out through ADAPTER's host, as the kit would."
  (nck:execute-plan (nck:host-executor (nct::telegram-adapter-host adapter)) plan))

(deftest channel-telegram-the-board-knows-the-topics-it-opened ()
  ;; A topic the bot opens is on the board; whoever spoke last there decides
  ;; its section -- a person's words wait for an answer, the bot's answer is
  ;; done or, marked waiting, needs you -- and a card is no words. A topic
  ;; closed leaves it, reopened comes back; a topic the bot never opened is
  ;; never on it.
  (with-test-board (board adapter plans)
    (board-test-say adapter (nct::topic-plan '(:channel-id "-100555") "Fix the build"))
    (is (equal '("-100555:7") (loop for key being the hash-keys of (nct:telegram-board-topics board) collect key)))
    (nct:note-telegram-update adapter (board-test-update "text" "why is it red?" "from" (nlk:json-object "id" 42)))
    (nct:telegram-board-pass board)
    (is (equal '(:noans) (mapcar (lambda (row) (getf row :state)) (nct:telegram-board-rows board))))
    (is (equal "Fix the build" (getf (first (nct:telegram-board-rows board)) :name)))
    (let* ((target '(:channel-id "-100555" :thread-id "7"))
           (answer (nct:telegram-message-id
                    (nck:execution-body
                     (board-test-say adapter (send-plan target (first (nck:split-text-chunks (format nil "It was the pin.~%~%Push it now?") 4000))))))))
      ;; A turn's card: its HTML, the chrome over the words.
      (board-test-say adapter (nct::bot-plan "/sendRichMessage"
                                             (nlk:json-object "chat_id" "-100555" "message_thread_id" 7
                                                              "rich_message" (nlk:json-object "html" "<b>Thinking</b>"))
                                             30 t "send_rich_message"))
      (nct:telegram-board-pass board)
      (is (equal '(:done) (mapcar (lambda (row) (getf row :state)) (nct:telegram-board-rows board))) "the answer, unmarked, waits on nobody")
      (with-stubbed-fdefinition (nck:answer-waiting (host room message-id)
                                  (and (equal room "7") (equal message-id answer) ""))
        (nct:telegram-board-pass board)))
    (let ((row (first (nct:telegram-board-rows board))))
      (is (eq :needs (getf row :state)) "the answer the card did not displace, marked waiting")
      (is (equal "Push it now?" (getf row :question))))
    (nct:note-telegram-update adapter (board-test-update "forum_topic_closed" (nlk:json-object)))
    (nct:telegram-board-pass board)
    (is (null (nct:telegram-board-rows board)) "a closed topic leaves the board")
    (nct:note-telegram-update adapter (board-test-update "forum_topic_reopened" (nlk:json-object)))
    (nct:telegram-board-pass board)
    (is (= 1 (length (nct:telegram-board-rows board))))
    (nct:note-telegram-update adapter (nlk:json-object "update_id" 2
                                                      "message" (nlk:json-object "message_id" 51 "message_thread_id" 9 "date" 1700000000
                                                                                 "chat" (nlk:json-object "id" -100555)
                                                                                 "from" (nlk:json-object "id" 42) "text" "hi")))
    (is (= 1 (hash-table-count (nct:telegram-board-topics board))) "a topic the bot never opened"))
  (let ((nct:*telegram-board* nil))
    (is (null (nct:note-telegram-write (nct::topic-plan '(:channel-id "-100555") "x")
                                       (nck:make-execution :ok-p t :status 200 :body (cell-json "{\"ok\": true, \"result\": {\"message_thread_id\": 8}}")))) "no board, nothing noted")))

(deftest channel-telegram-the-board-keeps-one-message ()
  ;; The first pass posts the board in its chat, with Show all and, while a
  ;; topic is done, Close all done; a pass that changes nothing sends
  ;; nothing; one that does edits the same message.
  (with-test-board (board adapter plans)
    (board-test-say adapter (nct::topic-plan '(:channel-id "-100555") "Fix the build"))
    (board-test-say adapter (send-plan '(:channel-id "-100555" :thread-id "7")
                                                   (first (nck:split-text-chunks "Fixed." 4000))))
    (setf (car plans) '())
    (nct:telegram-board-pass board)
    (let ((post (first (car plans))))
      (is (search "sendRichMessage" (nck:request-plan-path post)))
      (is (equal "-100555" (gethash "chat_id" (nck:request-plan-body post))))
      (is (null (gethash "message_thread_id" (nck:request-plan-body post))) "in the chat, no topic")
      (let ((markdown (gethash "markdown" (gethash "rich_message" (nck:request-plan-body post))))
            (buttons (map 'list (lambda (button) (gethash "text" button))
                          (aref (gethash "inline_keyboard" (gethash "reply_markup" (nck:request-plan-body post))) 0))))
        (is (search "**Done** · 1" markdown))
        (is (search "[Fix the build](https://t.me/c/555/7)" markdown) "the topic's name, its link")
        (is (equal '("Show all" "Close all done (1)") buttons))))
    (is (equal "91" (nct::telegram-board-message-id board)))
    (setf (car plans) '())
    (nct:telegram-board-pass board)
    (is (null (car plans)) "nothing changed, nothing sent")
    (nct:note-telegram-update adapter (board-test-update "text" "one more thing" "from" (nlk:json-object "id" 42)))
    (nct:telegram-board-pass board)
    (let ((edit (first (car plans))))
      (is (search "editMessageText" (nck:request-plan-path edit)))
      (is (eql 91 (gethash "message_id" (nck:request-plan-body edit))))
      (is (search "**No answer** · 1" (gethash "markdown" (gethash "rich_message" (nck:request-plan-body edit))))))))

(deftest channel-telegram-the-board-closes-what-went-quiet ()
  ;; A done topic quiet 48 hours is closed by the pass and leaves the board.
  (with-test-board (board adapter plans)
    (board-test-say adapter (nct::topic-plan '(:channel-id "-100555") "Fix the build"))
    (board-test-say adapter (send-plan '(:channel-id "-100555" :thread-id "7")
                                                   (first (nck:split-text-chunks "Fixed." 4000))))
    (let ((record (gethash "-100555:7" (nct:telegram-board-topics board))))
      (setf (getf record :said) 0 (getf record :opened) 0
            (gethash "-100555:7" (nct:telegram-board-topics board)) record))
    (setf (car plans) '())
    (nct:telegram-board-pass board)
    (is (find-if (lambda (plan) (search "closeForumTopic" (nck:request-plan-path plan))) (car plans)))
    (is (null (nct:telegram-board-rows board)))
    (is (getf (gethash "-100555:7" (nct:telegram-board-topics board)) :closed) "its own close, noted")))

(deftest channel-telegram-the-boards-buttons-answer-the-presser ()
  ;; Show all sends the presser the list in their own chat with the bot;
  ;; Close all done is an operator's, and closes every done topic.
  (with-test-board (board adapter plans)
    (setf (nct:telegram-board-rows board)
          (list (list :id "-100555:7" :state :done :quiet-ms 60000 :name "Fix the build" :chat "-100555" :topic "7")))
    (flet ((press (data user)
             (setf (car plans) '())
             (is (eq t (nct:telegram-handle-update
                        adapter (nlk:json-object "update_id" 3
                                                 "callback_query" (nlk:json-object "id" "cb1" "data" data
                                                                                   "from" (nlk:json-object "id" user)
                                                                                   "message" (nlk:json-object "message_id" 91 "chat" (nlk:json-object "id" -100555)))))))
             (loop repeat 200
                   until (find-if (lambda (plan) (search "answerCallbackQuery" (nck:request-plan-path plan))) (car plans))
                   do (sleep 0.01))
             (reverse (car plans))))
      (let ((plans (press "nct:board:all" 42)))
        (is (equal "42" (gethash "chat_id" (nck:request-plan-body (first plans)))) "the list, in the presser's own chat")
        (is (search "Fix the build" (gethash "markdown" (gethash "rich_message" (nck:request-plan-body (first plans))))))
        (is (search "your chat with the bot" (gethash "text" (nck:request-plan-body (second plans))))))
      (let ((nck::*hosts* nil))
        (with-stubbed-fdefinition (nck:operator-p (host user-id) (declare (ignore host)) (equal user-id "1"))
          (let ((plans (press "nct:board:done" 42)))
            (is (search "operators" (gethash "text" (nck:request-plan-body (first plans)))))
            (is (= 1 (length plans)) "refused, nothing closed"))
          (press "nct:board:done" 1)
          (loop repeat 200
                until (find-if (lambda (plan) (search "closeForumTopic" (nck:request-plan-path plan))) (car plans))
                do (sleep 0.01))
          (is (find-if (lambda (plan) (search "closeForumTopic" (nck:request-plan-path plan))) (car plans))))))))
