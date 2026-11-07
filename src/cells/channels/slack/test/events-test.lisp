;;;; events-test.lisp --- Slack events and socket frames as the kit reads them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure: the event shapes are docs.slack.dev's, built by the fake's helpers.

(in-package #:nodecode.test)

(deftest channel-slack-plain-text-reads-as-typed ()
  ;; A mention stays the handle the model writes back; a channel link, a
  ;; link, @here and the escaped characters read as a person sees them.
  (is (equal "<@U1> see #general and docs (https://x.dev) or https://y.dev, @here: a < b & c > d"
             (ncs:slack-plain-text
              "<@U1> see <#C1|general> and <https://x.dev|docs> or <https://y.dev>, <!here>: a &lt; b &amp; c &gt; d")))
  (is (equal "mail kim@x.dev" (ncs:slack-plain-text "mail <mailto:kim@x.dev|kim@x.dev>")))
  (is (equal "" (ncs:slack-plain-text nil))))

(deftest channel-slack-mention-is-the-user-handle ()
  (is (ncs:slack-mention-p "hey <@UBOT> look" "UBOT"))
  (is (ncs:slack-mention-p "<@UBOT|nodecode> look" "UBOT"))
  (is (not (ncs:slack-mention-p "hey <@UOTHER> look" "UBOT")))
  (is (not (ncs:slack-mention-p "hey UBOT" "UBOT")))
  (is (equal "look here" (ncs:slack-strip-mention "<@UBOT> look here" "UBOT")))
  (is (equal "look" (ncs:slack-strip-mention "<@UBOT|nodecode> look" "UBOT"))))

(deftest channel-slack-message-maps-to-the-candidate ()
  (is-present (dm (ncs:slack-message-candidate
                   (slack-message "D1" "1.1" "hi &amp; bye" :channel-type "im")
                   :bot-user-id "UBOT" :user-name "kim")) "a DM is something said"
    (is (equal "hi & bye" (gethash "text" dm)))
    (is-source dm "platform" "slack" "chat_kind" "direct_message" "channel_id" "D1"
               "message_id" "1.1" "user_id" "U1" "user_name" "kim" "workspace_id" "T1"
               "reply_to_message_id" nil)
    (is (not (nck:candidate-addressed-p dm))))
  ;; A reply in a thread answers the thread's first message, and is addressed
  ;; when the thread hangs off the bot or the bot speaks in it.
  (is-present (reply (ncs:slack-message-candidate
                      (slack-message "C1" "2.2" "and more" :thread "1.1" :parent-user "UBOT")
                      :bot-user-id "UBOT")) "a reply under the bot's message"
    (is-source reply "chat_kind" "thread" "reply_to_message_id" "1.1")
    (is (nck:candidate-addressed-p reply)))
  (let ((seen '()))
    (is-present (reply (ncs:slack-message-candidate
                        (slack-message "C1" "2.3" "go on" :thread "1.1")
                        :bot-user-id "UBOT"
                        :bot-thread-p (lambda (channel root) (push (list channel root) seen) t))) "a reply in a thread the bot answers in"
      (is (nck:candidate-addressed-p reply))
      (is (equal '(("C1" "1.1")) seen))))
  (is-present (parent (ncs:slack-message-candidate
                       (slack-message "C1" "1.1" "the ask" :thread "1.1") :bot-user-id "UBOT")) "a thread's own first message replies to nothing"
    (is-source parent "reply_to_message_id" nil))
  (is (null (ncs:slack-message-candidate (slack-message "C1" "3.3" "x" :subtype "message_changed"))) "an edit is bookkeeping")
  (is (null (ncs:slack-message-candidate (slack-message "C1" "3.4" "x" :subtype "channel_join"))))
  (is-present (bot (ncs:slack-message-candidate (slack-message "C1" "3.5" "beep" :bot-id "B2"))) "another bot's line maps, and says it is a bot's"
    (is (eq t (gethash "is_bot" (gethash "source" bot)))))
  (is-present (shared (ncs:slack-message-candidate
                       (slack-message "C1" "4.4" "look" :subtype "file_share"
                                                        :files (vector (nlk:json-object
                                                                        "id" "F1" "name" "shot.png"
                                                                        "mimetype" "image/png" "size" 12
                                                                        "url_private_download" "https://files/F1")
                                                                       (nlk:json-object
                                                                        "id" "F2" "name" "a.zip"
                                                                        "mimetype" "application/zip"
                                                                        "url_private_download" "https://files/F2"))))) "a file share carries every file: the kit reads what it can in, and saves the rest"
    (let ((files (gethash "attachments" shared)))
      (is (= 2 (length files)))
      (is-shape (aref files 0) ("id" "F1") ("download" "https://files/F1")
                ("media_type" "image/png") ("filename" "shot.png") ("size" = 12))
      (is-shape (aref files 1) ("id" "F2") ("download" "https://files/F2")
                ("media_type" "application/zip") ("filename" "a.zip") ("size" = 0)))))

(deftest channel-slack-command-is-the-line-it-spells ()
  (flet ((command (text)
           (ncs:slack-command-candidate
            (nlk:json-object "command" "/nodecode" "text" text "channel_id" "C1" "user_id" "U1"
                             "user_name" "kim" "team_id" "T1" "response_url" "http://r/1"))))
    (is (equal "/models grok" (gethash "text" (command "models grok"))))
    (is (equal "/stop" (gethash "text" (command "/stop"))))
    (is (equal "/help" (gethash "text" (command "  "))))
    (let ((candidate (command "help")))
      (is (nck:candidate-addressed-p candidate))
      (is-source candidate "response_url" "http://r/1" "chat_kind" "channel" "message_id" nil))
    (is-source (ncs:slack-command-candidate
                (nlk:json-object "text" "help" "channel_id" "D9" "user_id" "U1"))
               "chat_kind" "direct_message")))

(deftest channel-slack-button-press-is-the-kit-press ()
  (let ((press (ncs:slack-control-press
                (nlk:json-object "type" "block_actions" "trigger_id" "tr"
                                 "user" (nlk:json-object "id" "U1")
                                 "container" (nlk:json-object "message_ts" "5.5" "channel_id" "C1"
                                                              "thread_ts" "4.4")
                                 "actions" (vector (nlk:json-object "action_id" "nodecode-0"
                                                                    "value" "nck:stop:s1"
                                                                    "action_ts" "6.6"))))))
    (is (equal '(:id "6.6" :data "nck:stop:s1" :user-id "U1" :user-name nil :message-id "5.5"
                 :channel-id "C1" :thread-id "4.4")
               press)))
  (is (null (ncs:slack-control-press (nlk:json-object "type" "view_submission")))))

(deftest channel-slack-press-on-a-choice-is-its-line ()
  ;; A pick from a menu is a press too, its value the option's; a press on a
  ;; choice is the presser saying its line where the message pressed is, the
  ;; response_url its return path and the words it was posted with its card.
  (let* ((payload (nlk:json-object
                   "type" "block_actions" "response_url" "https://hooks.slack.com/actions/1"
                   "team" (nlk:json-object "id" "T1")
                   "user" (nlk:json-object "id" "U1" "username" "kim")
                   "container" (nlk:json-object "type" "message" "message_ts" "5.5"
                                                "channel_id" "C1" "thread_ts" "1.1")
                   "message" (nlk:json-object "ts" "5.5" "text" "Pick one")
                   "actions" (vector (nlk:json-object "type" "static_select" "action_ts" "9.9"
                                                      "selected_option" (nlk:json-object "value" "nck:p:abc")))))
         (press (ncs:slack-control-press payload))
         (candidate (ncs:slack-press-candidate payload "/think high" :card-text "## Pick one")))
    (is (equal "nck:p:abc" (getf press :data)) "the option picked is the data")
    (is (equal "kim" (getf press :user-name)))
    (is (equal "/think high" (gethash "text" candidate)))
    (is-source candidate "platform" "slack" "chat_kind" "thread" "channel_id" "C1" "user_id" "U1"
                         "press_data" "nck:p:abc" "reply_to_message_id" "5.5"
                         "response_url" "https://hooks.slack.com/actions/1"
                         "card_text" "## Pick one" "workspace_id" "T1")
    (is (nck:candidate-pressed-p candidate))
    (is (eq t (source-field-of candidate "addressed")))
    (is (null (source-field-of candidate "card_ephemeral")))
    (is (equal "Pick one" (source-field-of (ncs:slack-press-candidate payload "/x") "card_text")) "without the book's words, the message's own")))

(deftest channel-slack-an-edit-is-the-message-it-became ()
  ;; message_changed carries the message as it now stands: it reads as a new
  ;; message would, the same ts, its channel carried over. A bot's message --
  ;; the lane's own status lines -- is never an ask, edited or not.
  (let* ((event (nlk:json-object "type" "message" "subtype" "message_changed" "channel" "C1"
                                 "channel_type" "channel" "ts" "9.9"
                                 "message" (nlk:json-object "type" "message" "user" "U1" "ts" "5.5"
                                                            "text" "<@UBOT> now I ask")))
         (edited (ncs::slack-edited-message event))
         (candidate (ncs:slack-message-candidate edited :bot-user-id "UBOT")))
    (is (equal "C1" (gethash "channel" edited)))
    (is-source candidate "message_id" "5.5" "channel_id" "C1" "user_id" "U1")
    (is (equal "<@UBOT> now I ask" (gethash "text" candidate))))
  (is (null (ncs::slack-edited-message
             (nlk:json-object "type" "message" "subtype" "message_changed" "channel" "C1"
                              "message" (nlk:json-object "type" "message" "bot_id" "BBOT" "ts" "5.6"
                                                         "text" "working")))))
  (is (null (ncs::slack-edited-message (slack-message "C1" "5.7" "plain")))))

(deftest channel-slack-a-reaction-is-a-wordless-reply ()
  (let ((candidate (ncs::slack-reaction-candidate
                    (nlk:json-object "type" "reaction_added" "user" "U1" "reaction" "thumbsup"
                                     "item_user" "UBOT"
                                     "item" (nlk:json-object "type" "message" "channel" "C1" "ts" "5.5"))
                    :team-id "T1" :bot-user-id "UBOT" :root-of (constantly "1.1"))))
    (is (equal ":thumbsup:" (gethash "text" candidate)))
    (is-source candidate "reply_to_message_id" "5.5" "reaction" "add" "chat_kind" "thread"
                         "workspace_id" "T1")
    (is (eq t (source-field-of candidate "addressed")) "on the bot's own message"))
  (is (equal "remove" (source-field-of
                       (ncs::slack-reaction-candidate
                        (nlk:json-object "type" "reaction_removed" "user" "U1" "reaction" "x"
                                         "item" (nlk:json-object "type" "message" "channel" "D1" "ts" "5.5")))
                       "reaction")))
  (is (null (ncs::slack-reaction-candidate
             (nlk:json-object "type" "reaction_added" "user" "UBOT" "reaction" "eyes"
                              "item" (nlk:json-object "type" "message" "channel" "C1" "ts" "5.5"))
             :bot-user-id "UBOT")) "the bot's own marks are not read back")
  (is (null (ncs::slack-reaction-candidate
             (nlk:json-object "type" "reaction_added" "user" "U1" "reaction" "eyes"
                              "item" (nlk:json-object "type" "file" "file" "F1"))))))

(deftest channel-slack-access-by-workspace-group-and-wake-word ()
  ;; With allowed_workspaces set, someone of another organization in a shared
  ;; channel is no one the bot answers; a user group the section names and a
  ;; wake word address the bot as its own mention does.
  (let ((policy (ncs:slack-inbound-policy :allowed-workspaces '("T1") :allowed-channels '("C1")
                                          :require-mention t :bot-user-id "UBOT"
                                          :mention-groups '("S9") :mention-patterns '("hey nodecode"))))
    (flet ((said (text &rest keys)
             (ncs:slack-message-candidate (apply #'slack-message "C1" "5.5" text keys) :bot-user-id "UBOT")))
      (is-inbound policy (said "<@UBOT> hi") :answer)
      (let ((stranger (said "<@UBOT> hi")))
        (setf (gethash "workspace_id" (gethash "source" stranger)) "T2")
        (is-inbound policy stranger :reject "workspace_not_allowed"))
      (is-inbound policy (said "<!subteam^S9|@oncall> help") :answer)
      (is-inbound policy (said "<!subteam^S8|@other> help") :observe "mention_required")
      (is-inbound policy (said "Hey Nodecode, status?") :answer)
      (is-inbound policy (said "just chatting") :observe "mention_required")))
  (is (equal "<!subteam^S9|@oncall> help" (ncs:slack-plain-text "<!subteam^S9|@oncall> help")) "a group mention keeps its handle")
  ;; Read off the event: the sender's own workspace, before the channel's.
  (is (equal "T2" (source-field-of
                   (ncs:slack-message-candidate
                    (let ((event (slack-message "C1" "5.5" "hi")))
                      (setf (gethash "user_team" event) "T2")
                      event))
                   "workspace_id"))))

(defun source-field-of (candidate key)
  (gethash key (gethash "source" candidate)))

(deftest channel-slack-socket-frames ()
  (is (equal '((:hello)) (ncs:read-socket-frame (nlk:json-object "type" "hello"))))
  (is (equal '((:reconnect "refresh_requested"))
             (ncs:read-socket-frame (nlk:json-object "type" "disconnect" "reason" "refresh_requested"))))
  (is (eq :fatal (first (first (ncs:read-socket-frame
                                (nlk:json-object "type" "disconnect" "reason" "link_disabled"))))))
  ;; An envelope is acknowledged first, whatever it holds.
  (let* ((payload (nlk:json-object "type" "event_callback"))
         (actions (ncs:read-socket-frame (slack-envelope "e1" "events_api" payload))))
    (is (equal '(:ack "e1") (first actions)))
    (is (equal (list :envelope "events_api" payload) (second actions))))
  (is (equal '((:ack "e2")) (ncs:read-socket-frame (nlk:json-object "envelope_id" "e2" "type" "x"))))
  (is (null (ncs:read-socket-frame (nlk:json-object "type" "mystery"))))
  (is (equal "{\"envelope_id\":\"e1\"}" (nlk:encode-json-object (ncs:ack-frame "e1")))))
