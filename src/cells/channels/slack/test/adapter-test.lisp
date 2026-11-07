;;;; adapter-test.lisp --- the Slack lane end to end, over a fake Slack.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane runs as it runs for an operator: its live executors and its
;;;; Socket Mode lap against fake-slack.lisp on 127.0.0.1, the real nodecode
;;;; gateway behind it (WITH-ADAPTER-LANE, the provider a stub). Covers: the
;;;; socket, the acknowledgement, a DM answered in the DM, a channel ask
;;;; answered in its thread and continued there unmentioned, a socket Slack
;;;; moves, the slash command, and the probe. The digest flow itself is the
;;;; kit's (host-test.lisp).

(in-package #:nodecode.test)

(defun slack-section (fake token-path soul-path &rest members)
  "channels.slack against FAKE, both tokens in TOKEN-PATH, MEMBERS over it."
  (let ((section (nlk:json-object "bot_token_file" token-path "app_token_file" token-path
                                  "api_base" (fake-slack-api-base fake)
                                  "request_timeout_seconds" 5
                                  "soul_file" soul-path)))
    (loop for (key value) on members by #'cddr
          do (setf (gethash key section) value))
    section))

(defun slack-block-text (body)
  "The markdown a posted BODY says."
  (nlk:json-value (aref (nlk:json-array body "blocks") 0) :string "text"))

(defun slack-answers (fake needle)
  "Every chat.postMessage body so far whose markdown says NEEDLE, oldest first."
  (remove-if-not (lambda (body) (search needle (or (ignore-errors (slack-block-text body)) "")))
                 (slack-calls fake "chat.postMessage")))

(defmacro with-slack-lane ((fake &rest members) &body body)
  "BODY against a started Slack lane over FAKE -- MEMBERS the section's own --
once its socket is open; the provider answers `slack reply'."
  `(with-fake-slack (,fake)
     (with-adapter-lane (:reply "slack reply" :token "xoxb-fake" :start ncs:start-channel
                         :bindings ((executor nil))
                         :section (slack-section ,fake token-path soul-path ,@members)
                         :plans nil
                         :after ((is (adapter-threads-stopped-p "channel-slack"))))
       (is (await (:timeout 10) (fake-slack-sockets ,fake)) "the lane opened its socket")
       ,@body)))

(deftest channel-slack-dm-answered-over-the-socket ()
  (with-slack-lane (fake "allowed_users" (vector "U1") "owner" (vector "U1"))
    (let ((ts "1700000001.000100"))
      (fake-slack-push fake (slack-event-envelope "e1" (slack-message "D1" ts "hi bot"
                                                                      :channel-type "im")))
      (is (await (:timeout 5) (fake-slack-acked-p fake "e1")) "the envelope was acknowledged")
      (is-present (answer (await (:timeout 15) (first (slack-answers fake "slack reply")))) "the answer was posted"
        (is (equal "D1" (gethash "channel" answer)))
        (is (null (gethash "thread_ts" answer)) "a DM answers in the DM"))
      (is-lane-carrying "slack" "slack-D1" (format nil "slack-D1-m~a" ts) "slack-channel"
                        "(Slack user id U1)" "(ncs:request")
      (is (wire-row (format nil "kim (operator) [m~a uU1]: hi bot" ts) (first messages-seen)
                    "content") "the line names its speaker by the name Slack gives")
      ;; Slack sends an unacknowledged envelope again; the ask is one turn.
      (fake-slack-push fake (slack-event-envelope "e1" (slack-message "D1" ts "hi bot"
                                                                      :channel-type "im")))
      (is-exchange-written-back-once "slack-D1" (format nil "slack-D1-m~a" ts)
                                     "the redelivered envelope admitted no second turn"))))

(deftest channel-slack-channel-ask-threads-and-goes-on ()
  (with-slack-lane (fake "allowed_channels" (vector "C1") "allowed_users" (vector "U1")
                         "require_mention" t)
    (let ((root "1700000002.000100")
          (lane "slack-C1-m1700000002.000100"))
      (fake-slack-push fake (slack-event-envelope
                             "e1" (slack-message "C1" root "<@UBOT> what is up")))
      (is-present (answer (await (:timeout 15) (first (slack-answers fake "slack reply")))) "the answer was posted"
        (is (equal "C1" (gethash "channel" answer)))
        (is (equal root (gethash "thread_ts" answer)) "in the ask's thread"))
      (is (await (:timeout 15) (plusp (count-session-events lane "turn.completed"))))
      ;; A channel line that mentions nobody is context, not an ask.
      (fake-slack-push fake (slack-event-envelope
                             "e2" (slack-message "C1" "1700000002.000200" "just chatting")))
      ;; In the thread the bot answers in, no mention is needed, and the
      ;; lane the ask opened takes the next turn.
      (fake-slack-push fake (slack-event-envelope
                             "e3" (slack-message "C1" "1700000002.000300" "and more"
                                                 :thread root)))
      (is (await (:timeout 15) (= 2 (length (slack-answers fake "slack reply")))) "the follow-up was answered")
      (is (every (lambda (body) (equal root (gethash "thread_ts" body)))
                 (slack-answers fake "slack reply")) "both answers are in the one thread")
      (is (await (:timeout 15) (= 2 (count-session-events lane "turn.completed"))))
      (is (= 2 (count-session-events lane "turn.started")) "one lane, two turns")
      (is (not (nlk:session-exists-p "slack-C1-m1700000002.000300")) "no second lane")
      (is (not (nlk:session-exists-p "slack-C1-m1700000002.000200")) "chatter opened nothing"))))

(deftest channel-slack-socket-moves-and-the-command-answers ()
  (with-slack-lane (fake "allowed_users" (vector "U1") "owner" (vector "U1"))
    ;; Slack moves the socket on its own schedule and says so first.
    (fake-slack-push fake (nlk:json-object "type" "disconnect" "reason" "refresh_requested"))
    (is (await (:timeout 10) (and (>= (fake-slack-opened fake) 2) (fake-slack-sockets fake))) "a fresh socket replaced the one Slack moved")
    (is (getf (nck:channel-status "slack") :connected))
    (fake-slack-push fake (slack-envelope
                           "s1" "slash_commands"
                           (nlk:json-object "command" "/nodecode" "text" "help"
                                            "channel_id" "C1" "user_id" "U1" "user_name" "kim"
                                            "response_url" (format nil "http://127.0.0.1:~d/response/1"
                                                                   (fake-slack-port fake)))))
    (is (await (:timeout 5) (fake-slack-acked-p fake "s1")))
    (is-present (response (await (:timeout 15) (first (slack-calls fake "response")))) "/nodecode help answered through its response_url"
      (is (equal "in_channel" (gethash "response_type" response)))
      (is (plusp (length (slack-block-text response))) "the help says something"))
    (is (null (slack-calls fake "chat.postMessage")) "a command is no turn")))

(deftest channel-slack-a-press-on-a-choice-runs-its-line ()
  ;; A choice says its line when pressed: the presser's line runs, and its
  ;; answer becomes the message pressed, through the response_url the press
  ;; came with. A press on anything else is the kit's control press.
  (with-slack-lane (fake "allowed_users" (vector "U1") "owner" (vector "U1"))
    (flet ((press (id value)
             (slack-envelope
              id "interactive"
              (nlk:json-object
               "type" "block_actions"
               "response_url" (format nil "http://127.0.0.1:~d/response/~a" (fake-slack-port fake) id)
               "team" (nlk:json-object "id" "T1")
               "user" (nlk:json-object "id" "U1" "username" "kim")
               "container" (nlk:json-object "type" "message" "message_ts" "1700000005.000100"
                                            "channel_id" "D1" "is_ephemeral" nil)
               "message" (nlk:json-object "ts" "1700000005.000100" "text" "Pick one")
               "actions" (vector (nlk:json-object "type" "button" "action_id" "nodecode-1"
                                                  "value" value "action_ts" "1700000005.000200"))))))
      (fake-slack-push fake (press "p1" (nck:press-token "/help")))
      (is (await (:timeout 5) (fake-slack-acked-p fake "p1")))
      (is-present (response (await (:timeout 15) (first (slack-calls fake "response")))) "the line the choice says was answered through the press's response_url"
        (is (eq t (gethash "replace_original" response)) "the answer becomes the message pressed")
        (is (plusp (length (slack-block-text response))) "the help says something"))
      (is (null (slack-calls fake "chat.postMessage")) "a command is no turn"))))

(defun slack-edit (channel ts text &key (user "U1") bot-id)
  "A message_changed event: the message TS in CHANNEL now says TEXT."
  (nlk:json-object "type" "message" "subtype" "message_changed" "channel" channel
                   "channel_type" "channel" "hidden" t "ts" (format nil "~a9" ts)
                   "message" (nlk:json-object "type" "message" "text" text "ts" ts
                                              :opt "user" (and (null bot-id) user)
                                              :opt "bot_id" bot-id
                                              "edited" (nlk:json-object "user" user "ts" ts))))

(defun prompt-says-p (messages-seen needle)
  "Whether the newest prompt the provider was handed says NEEDLE."
  (some (lambda (message) (search needle (or (nlk:json-value message :string "content") "")))
        (coerce (first messages-seen) 'list)))

(deftest channel-slack-an-edit-that-mentions-the-bot-runs ()
  ;; A line that mentioned nobody is chatter; edited to mention the bot, it is
  ;; an ask, answered in its thread. The lane's own edits wake nothing.
  (with-slack-lane (fake "allowed_channels" (vector "C1") "allowed_users" (vector "U1")
                         "require_mention" t)
    (let ((ts "1700000003.000100"))
      (fake-slack-push fake (slack-event-envelope "e1" (slack-message "C1" ts "what is up")))
      (fake-slack-push fake (slack-event-envelope "e2" (slack-edit "C1" "1700000003.000050" "working"
                                                                   :bot-id "BBOT")))
      (fake-slack-push fake (slack-event-envelope "e3" (slack-edit "C1" ts "<@UBOT> what is up")))
      (is-present (answer (await (:timeout 15) (first (slack-answers fake "slack reply")))) "the edited ask was answered"
        (is (equal ts (gethash "thread_ts" answer)) "in the ask's thread"))
      (is (await (:timeout 15) (plusp (count-session-events (format nil "slack-C1-m~a" ts) "turn.completed"))))
      (is (= 1 (length (slack-answers fake "slack reply"))) "one ask, one answer"))))

(deftest channel-slack-an-ask-in-someone-elses-thread-reads-its-root ()
  ;; The bot mentioned inside a thread it does not speak in reads the thread's
  ;; first message as the one the ask answers.
  (with-slack-lane (fake "allowed_channels" (vector "C1") "allowed_users" (vector "U1" "U2")
                         "require_mention" t)
    (let ((root "1700000004.000100"))
      (setf (fake-slack-replies fake)
            (list (nlk:json-object "type" "message" "user" "U2" "ts" root
                                   "text" "the root says hello")))
      (fake-slack-push fake (slack-event-envelope
                             "e1" (slack-message "C1" "1700000004.000300" "<@UBOT> thoughts?"
                                                 :thread root :parent-user "U2")))
      (is (await (:timeout 15) (first (slack-answers fake "slack reply"))) "the ask was answered")
      (is (equal (format nil "channel=C1&ts=~a&limit=1" root)
                 (first (slack-calls fake "conversations.replies"))) "the thread's first message was read")
      (is (prompt-says-p messages-seen "the root says hello") "the ask carries what it answers"))))

(deftest channel-slack-what-was-said-while-away-is-caught-up ()
  ;; Read back once a socket says hello after a gap: the ask said meanwhile
  ;; is answered, marked late; one older than the gap, and the bot's own, are
  ;; left be.
  (with-slack-lane (fake "allowed_channels" (vector "C1") "allowed_users" (vector "U1")
                         "require_mention" t)
    (let* ((now (ncs::unix-ms))
           (missed (ncs::ms-ts (- now 60000)))
           (older (ncs::ms-ts (- now 3600000))))
      (setf (fake-slack-history fake)
            (list (nlk:json-object "type" "message" "user" "UBOT" "ts" (ncs::ms-ts (- now 30000))
                                   "text" "my own")
                  (nlk:json-object "type" "message" "user" "U1" "ts" missed
                                   "text" "<@UBOT> are you there?")
                  (nlk:json-object "type" "message" "user" "U1" "ts" older
                                   "text" "<@UBOT> long ago")))
      (ncs::start-catch-up ncs::*slack-adapter* (- now 300000))
      (is-present (answer (await (:timeout 15) (first (slack-answers fake "slack reply")))) "the missed ask was answered"
        (is (equal missed (gethash "thread_ts" answer)) "in its thread"))
      (is (prompt-says-p messages-seen "are you there?"))
      (is (equal "channel=C1&limit=50" (first (slack-calls fake "conversations.history"))))
      (sleep 1)
      (is (= 1 (length (slack-answers fake "slack reply"))) "the older ask and the bot's own are left be")))
  (is (equal "1700000000.123000" (ncs::ms-ts (ncs::ts-ms "1700000000.123456"))) "a ts read to the millisecond and back"))

(deftest channel-slack-probe-names-what-the-tokens-see ()
  (with-fake-slack (fake)
    (with-temp-file (token-path :contents "xoxb-fake" :type "txt")
      (let* ((section (slack-section fake token-path nil "allowed_users" (vector "U1")))
             (text (ncs:probe-channel section)))
        (is (search "bot token: ok -- <@UBOT> in workspace Fake (T1)" text) text)
        (is (search "app token: ok" text) text)
        (is (search "#general C1, #ops (private) G2, DM with kim D1" text) text)
        (is (search "people: kim U1" text) text)
        (is (not (search "xoxb-fake" text)) "the token is never said")
        (is (equal '(("C1" . "#general") ("G2" . "#ops (private)") ("D1" . "DM with kim"))
                   (ncs:probe-channel-choices section)))
        (is (equal '(("U1" . "kim")) (ncs:probe-user-choices section)))))))
