;;;; board-test.lisp --- the thread board on Slack.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the board knows of a thread (the bot's posts, people's replies, a
;;;; DM left out), how it names and draws them, and the board end to end over
;;;; the fake Slack: a channel ask answered in its thread shows on the board's
;;;; one message as Done, linked; /threads lists it; Show all answers the
;;;; presser alone; Clear done takes it off. The sections and when a thread
;;;; leaves are the kit's (board.lisp there).

(in-package #:nodecode.test)

(defun slack-test-board ()
  "A board no thread runs for: what the notes write into."
  (ncs::%make-slack-board :channel "CB"))

(defun slack-post-plan (channel thread words &key (label "send_message"))
  (nck::make-request-plan :method "POST" :path "/chat.postMessage" :audit-label label
                          :body (nlk:json-object "channel" channel :opt "thread_ts" thread "text" words
                                                 "blocks" (vector (nlk:json-object "type" "markdown"
                                                                                   "text" words)))))

(defun slack-posted (ts)
  (nck::make-execution :ok-p t :status 200 :body (nlk:json-object "ok" t "ts" ts)))

(defun slack-board-record (board channel root)
  (gethash (ncs::book-key channel root) (ncs::slack-board-threads board)))

(deftest channel-slack-board-notes-who-spoke-last ()
  (let* ((ncs::*slack-board* (slack-test-board))
         (board ncs::*slack-board*)
         (adapter (ncs::%make-slack-adapter :bot-user-id "UBOT")))
    (ncs::note-slack-write (slack-post-plan "C1" "1.1" "Working" :label "send_card") (slack-posted "1.2"))
    (is-present (record (slack-board-record board "C1" "1.1")) "a turn's card puts its thread on the board"
      (is (null (getf record :bot)) "a card is not words said"))
    (ncs::note-slack-write (slack-post-plan "C1" "1.1" (format nil "Done.~%**Shall I ship it?**")) (slack-posted "1.3"))
    (is-present (record (slack-board-record board "C1" "1.1")) "the answer is noted"
      (is (eq t (getf record :bot)) "the bot spoke last")
      (is (equal "1.3" (getf record :said-id)))
      (is (equal "Shall I ship it?" (getf record :question)) "the line the answer ends on"))
    (ncs::note-slack-message adapter (slack-message "C1" "1.4" "yes" :thread "1.1"))
    (is-present (record (slack-board-record board "C1" "1.1")) "a reply is noted"
      (is (null (getf record :bot)) "someone else spoke last")
      (is (null (getf record :said-id))))
    (ncs::note-slack-message adapter (slack-message "C1" "1.5" "echo" :thread "1.1" :user "UBOT" :bot-id "BBOT"))
    (is (null (getf (slack-board-record board "C1" "1.1") :bot)) "the bot's own echo is not someone speaking")
    (ncs::note-slack-message adapter (slack-message "C1" "2.2" "elsewhere" :thread "2.1"))
    (is (null (slack-board-record board "C1" "2.1")) "a thread the bot is not in stays off")
    (ncs::note-slack-write (slack-post-plan "D1" "3.1" "private") (slack-posted "3.2"))
    (is (null (slack-board-record board "D1" "3.1")) "a DM's thread stays off")
    (ncs::note-slack-write (slack-post-plan "C1" nil "top level") (slack-posted "4.1"))
    (ncs::note-slack-write nil (slack-posted "4.2"))
    (is (= 1 (hash-table-count (ncs::slack-board-threads board))) "a post in no thread opens none")))

(deftest channel-slack-board-names-a-thread-as-its-first-line ()
  (let ((adapter (ncs::%make-slack-adapter :bot-user-id "UBOT")))
    (setf (gethash "U2" (ncs::slack-adapter-names adapter)) "kim")
    (is (equal "deploy the site now" (ncs::thread-name adapter (format nil "<@UBOT> deploy the *site* now~%more"))))
    (is (equal "@kim look at this" (ncs::thread-name adapter "<@U2> look at [this]")))
    (is (equal "a list" (ncs::thread-name adapter (format nil "~%- a list"))))
    (is (null (ncs::thread-name adapter "<@UBOT>")) "a message saying nothing names nothing")))

(deftest channel-slack-board-draws-threads-by-who-moves-next ()
  (let ((rows (list (list :id "C1:1.1" :state :needs :quiet-ms 120000 :question "Ship it?"
                          :name "deploy the site" :link "https://x/p11")
                    (list :id "C1:2.2" :state :running :quiet-ms 1000 :name "fix the build")
                    (list :id "C1:3.3" :state :done :quiet-ms 7200000 :link "https://x/p33"))))
    (multiple-value-bind (words footer) (ncs::split-footer (ncs::slack-board-text rows))
      (is (search (format nil "**Needs you** · 1~%- [deploy the site](https://x/p11) · 2m~%  ↳ Ship it?") words) "Needs you first, its question under it")
      (is (search "- fix the build · running" words) "a thread with no link is named plainly")
      (is (search "- [A thread](https://x/p33) · 2h" words) "a thread with no name yet is still linked")
      (is (< (search "Needs you" words) (search "Running" words) (search "Done" words)))
      (is (search "3 open" footer) "the count rides the small footer"))
    (is (equal '((("Show all" "ncs:board:all") ("Clear done (1)" "ncs:board:done" :danger)))
               (ncs::slack-board-controls rows)))
    (is (search "No thread is open." (ncs::slack-board-text '())))))

(defun slack-board-calls (fake channel)
  "Every post and edit of the board's message in CHANNEL so far, oldest
first, as its words."
  (loop for body in (append (slack-calls fake "chat.postMessage") (slack-calls fake "chat.update"))
        when (and (hash-table-p body) (equal channel (gethash "channel" body)))
          collect (ignore-errors (slack-block-text body))))

(defun slack-board-press (fake id value)
  (fake-slack-push fake (slack-envelope
                         id "interactive"
                         (nlk:json-object
                          "type" "block_actions"
                          "team" (nlk:json-object "id" "T1")
                          "user" (nlk:json-object "id" "U1" "username" "kim")
                          "container" (nlk:json-object "type" "message" "message_ts" "1700000000.000101"
                                                       "channel_id" "CB")
                          "actions" (vector (nlk:json-object "type" "button" "action_id" "nodecode-1"
                                                             "value" value "action_ts" id))))))

(defun slack-ephemerals (fake needle)
  (remove-if-not (lambda (body) (and (equal "U1" (gethash "user" body))
                                     (search needle (or (ignore-errors (slack-block-text body)) ""))))
                 (slack-calls fake "chat.postEphemeral")))

(deftest channel-slack-board-lists-the-threads-the-bot-answers-in ()
  (with-slack-lane (fake "allowed_channels" (vector "C1") "allowed_users" (vector "U1")
                         "owner" (vector "U1") "require_mention" t "board_channel" "CB")
    (let* ((root "1700000002.000100")
           (line "[what is up](https://fake.slack.com/archives/C1/p1700000002000100)"))
      (is (await (:timeout 10) (slack-board-calls fake "CB")) "the board posted its message")
      (setf (fake-slack-replies fake) (list (slack-message "C1" root "<@UBOT> what is up")))
      (fake-slack-push fake (slack-event-envelope "e1" (slack-message "C1" root "<@UBOT> what is up")))
      (is (await (:timeout 15) (first (slack-answers fake "slack reply"))) "the ask was answered")
      (is (await (:timeout 20) (find-if (lambda (text) (and text (search "**Done**" text) (search line text)))
                                        (slack-board-calls fake "CB"))) "the board shows the thread as Done, named by its ask and linked")
      (is (search line (nck::board-command nil (format nil "slack-C1-m~a" root))) "/threads answers the board's list")
      (slack-board-press fake "p1" "ncs:board:all")
      (is (await (:timeout 10) (slack-ephemerals fake line)) "Show all answers the presser alone")
      (slack-board-press fake "p2" "ncs:board:done")
      (is (await (:timeout 10) (slack-ephemerals fake "Cleared 1 done thread")))
      (is (await (:timeout 20) (let ((newest (car (last (slack-board-calls fake "CB")))))
                                 (and newest (not (search line newest))))) "Clear done took the thread off the board"))))
