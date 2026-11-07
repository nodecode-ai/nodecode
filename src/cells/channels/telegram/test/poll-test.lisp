;;;; poll-test.lisp --- update mapping, offset math, admission policy.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun tg-message (json &rest keys)
  "The candidate MAP-TELEGRAM-MESSAGE derives from the message object JSON, KEYS its identity."
  (apply #'nct:map-telegram-message (cell-json json) keys))

(defun tg-json (id fields &key (chat 555) (type "private") user-name)
  "Message ID from user 42, named USER-NAME, in the chat CHAT of TYPE — by default the
private chat 555 — as JSON; FIELDS the object's other members."
  (format nil "{\"message_id\": ~d, \"chat\": {\"id\": ~d, \"type\": ~s}, ~
               \"from\": {\"id\": 42~@[, \"username\": ~s~]}, ~a}"
          id chat type user-name fields))

(defun tg-said (id fields &rest keys)
  "TG-MESSAGE over the TG-JSON of message ID with FIELDS: KEYS reach both, each taking its own."
  (apply #'tg-message (apply #'tg-json id fields :allow-other-keys t keys)
         :allow-other-keys t keys))

(deftest channel-telegram-update-folding (let ((updates (cell-json
                                                         "[{\"update_id\": 7,
                     \"message\": {\"message_id\": 1,
                                   \"chat\": {\"id\": 555,
                                              \"type\": \"private\"},
                                   \"from\": {\"id\": 42,
                                              \"username\": \"kim\"},
                                   \"text\": \"hello\"}},
                    {\"update_id\": 9,
                     \"edited_message\": {\"chat\": {\"id\": 1}}},
                    {\"update_id\": 10,
                     \"message\": {\"message_id\": 2,
                                   \"chat\": {\"id\": 555,
                                              \"type\": \"private\"},
                                   \"from\": {\"id\": 42},
                                   \"reply_to_message\": {\"message_id\": 1,
                                                          \"from\": {\"id\": 7}},
                                   \"text\": \"now do signup\"}},
                    {\"update_id\": 12,
                     \"message\": {\"message_id\": 3,
                                   \"chat\": {\"id\": -100200,
                                              \"type\": \"supergroup\",
                                              \"title\": \"Lab\"},
                                   \"message_thread_id\": 33,
                                   \"from\": {\"id\": 43,
                                              \"is_bot\": true},
                                   \"caption\": \"look at this\"}}]"))))
  (is (= 13 (nct:next-update-offset updates nil)))
  (let ((candidates (loop for update across updates
                          for candidate = (nct:map-telegram-message
                                           (gethash "message" update))
                          when candidate collect candidate)))
    (is (= 3 (length candidates)) "edited_message maps to no candidate")
    (let ((first-candidate (first candidates)))
      (is-source first-candidate :text "hello" "chat_kind" "direct_message" "channel_id" "555"
                 "user_id" "42" "reply_to_message_id" nil)
      (is (equal "telegram-555"
                 (nck:room-session-id "telegram" first-candidate))))
    ;; The reply gesture: Telegram's own pointer to another message is the
    ;; whole branch structure of a flat chat.
    (is (equal "1" (nck:source-field (second candidates) "reply_to_message_id")))
    (let ((topic (third candidates)))
      (is-source topic :text "look at this" "chat_kind" "thread" "thread_id" "33")
      (is (equal "telegram--100200-t33"
                 (nck:room-session-id "telegram" topic)))
      (is (equal '(:channel-id "-100200" :thread-id "33"
                   :message-id "3")
                 (nck:channel-target topic)))))
  ;; The offset never regresses.
  (is (= 99 (nct:next-update-offset #() 99))))

(deftest channel-telegram-get-updates-plan ()
  (let ((plan (nct:get-updates-plan 55 :poll-timeout-seconds 10)))
    (is-plan plan :path "/getUpdates" :timeout 20 :retry nil "offset" 55 "timeout" 10)
    ;; messages, an edit, presses on the controls they carry, and a reaction
    (is (equalp #("message" "edited_message" "callback_query" "message_reaction")
                (gethash "allowed_updates" (nck:request-plan-body plan)))))
  (let ((plan (nct:get-updates-plan nil :allowed-updates
                                    '("message" "message_reaction"))))
    (is (null (gethash "offset" (nck:request-plan-body plan))))
    (is (equalp #("message" "message_reaction") (plan-field plan "allowed_updates")))))

(deftest channel-telegram-policy-vocabulary (let ((policy (nct:telegram-inbound-policy
                                                           :allowed-chats '("555")
                                                           :require-mention t
                                                           :allow-private-chats t
                                                           :allow-group-chats nil
                                                           :allow-forum-topics nil
                                                           :bot-username "MyBot"))))
  (flet ((candidate (text kind channel &rest source)
           (nlk:json-object "text" text
                            "source" (apply #'nlk:make-json-object
                                            "chat_kind" kind "channel_id" channel
                                            (append source '("user_id" "1"))))))
    (is (eq :answer
            (nck:decide-inbound policy (candidate "anything" "direct_message" "555"))))
    (is-inbound policy (candidate "@mybot hi" "group" "555")
                :reject "group_chats_disabled")
    (is-inbound policy (candidate "@mybot hi" "thread" "555" "thread_id" "9")
                :reject "forum_topics_disabled")
    ;; An unaddressed group message is room chatter: the host buffers it
    ;; as context for the next ask instead of dropping it.
    (is-inbound policy (candidate "no ping" "channel" "555")
                :observe "mention_required")
    (is-inbound policy (candidate "@mybot hi" "channel" "777")
                :reject "chat_not_allowed")))

(deftest channel-telegram-reply-to-the-bot-is-addressed
    (flet ((candidate (&rest identity)
             (apply #'tg-said 5 "\"reply_to_message\": {\"message_id\": 4,
                                           \"from\": {\"id\": 777,
                                                      \"is_bot\": true,
                                                      \"username\": \"OrgBot\"}},
                    \"text\": \"/undo\"" :chat -1 :type "supergroup" identity))))
  ;; The section promises that a reply to the bot counts as a mention. A
  ;; message replying to one of the bot's own is `addressed' — by the
  ;; bot's id, or by its handle when only that is known — and the kit's
  ;; mention gate waives itself for it.
  (is (nck:candidate-addressed-p (candidate :bot-user-id "777")))
  (is (nck:candidate-addressed-p (candidate :bot-username "orgbot")))
  (is (not (nck:candidate-addressed-p
            (candidate :bot-user-id "1" :bot-username "other"))))
  (is (not (nck:candidate-addressed-p (candidate))))
  (is (not (nck:candidate-addressed-p
            (tg-said 6 "\"text\": \"/undo\"" :chat -1 :type "supergroup" :bot-user-id "777")))))

(deftest channel-telegram-reply-carries-the-answered-message ()
  ;; Telegram ships the replied message on every reply; the ask carries it
  ;; exactly as Discord's does.
  (let ((reply (gethash "reply" (tg-said 8 "\"reply_to_message\": {\"message_id\": 4,
                                             \"from\": {\"id\": 777,
                                                        \"username\": \"OrgBot\",
                                                        \"is_bot\": true},
                                             \"text\": \"the answered words\"},
                      \"text\": \"and then?\"" :chat -1 :type "supergroup"))))
    (is-shape reply ("text" "the answered words") ("user_name" "OrgBot") ("id" "4")))
  (is (null (gethash "reply" (tg-said 9 "\"text\": \"plain\"" :chat -1 :type "supergroup")))))

(deftest channel-telegram-message-carries-image-attachments ()
  ;; A photo rides the candidate as the largest of its sizes, and a document
  ;; beside it whatever it declares. Nothing is downloaded here —
  ;; the entry names the file by the id the Bot API addresses it with.
  (let* ((candidate (tg-said 20 "\"caption\": \"how many calories is this\",
                               \"photo\": [
                                 {\"file_id\": \"small\", \"width\": 90, \"height\": 90,
                                  \"file_size\": 500},
                                 {\"file_id\": \"big\", \"width\": 1280,
                                  \"height\": 1280, \"file_size\": 220000},
                                 {\"file_id\": \"mid\", \"width\": 640, \"height\": 640,
                                  \"file_size\": 40000}]"))
         (images (gethash "attachments" candidate)))
    (is (= 1 (length images)) "the picture is one attachment")
    (is-shape (aref images 0) ("file_id" "big") ("media_type" "image/jpeg") ("filename" "photo.jpg")
      ("size" = 220000))
    (is (equal "how many calories is this" (gethash "text" candidate))))
  ;; A document rides whatever it declares: a pdf is a file the lane opens.
  (let ((image (aref (gethash "attachments" (tg-said 21 "\"document\": {\"file_id\": \"doc1\",
                                             \"file_name\": \"scan.png\",
                                             \"mime_type\": \"image/png\",
                                             \"file_size\": 4211}")) 0)))
    (is-shape image ("file_id" "doc1") ("filename" "scan.png") ("media_type" "image/png")))
  (is-shape (aref (gethash "attachments"
                          (tg-said 22 "\"document\": {\"file_id\": \"doc2\",
                                                    \"file_name\": \"report.pdf\",
                                                    \"mime_type\": \"application/pdf\"}"))
                 0)
    ("file_id" "doc2") ("media_type" "application/pdf"))
  ;; A textless photo is still a line: the room's marker stands where the
  ;; text would, and the kit lets the message through.
  (let ((candidate (tg-said 23 "\"photo\": [{\"file_id\": \"p1\", \"width\": 100,
                                           \"height\": 100, \"file_size\": 9}]")))
    (is (equal "" (gethash "text" candidate)))
    (is (= 1 (length (gethash "attachments" candidate))))
    ;; The entry names its file by id until the adapter attaches its fetcher.
    (is (search "[an image is attached"
                (nck:speaker-line candidate
                                  :attachments (coerce (gethash "attachments" candidate)
                                                       'list))))))

(deftest channel-telegram-message-carries-recordings ()
  ;; A voice note, an audio file and a round video note are each a recording:
  ;; the file id, the declared type (or the kind's own), and the length the
  ;; Bot API declares as seconds. A document rides when it declares audio.
  (flet ((recording (id fields &aux (attachments (gethash "attachments" (tg-said id fields))))
           (is (= 1 (length attachments)))
           (aref attachments 0)))
    (let ((voice (recording 30 "\"voice\": {\"file_id\": \"v1\", \"duration\": 6,
                                            \"file_unique_id\": \"uv1\",
                                            \"mime_type\": \"audio/ogg\",
                                            \"file_size\": 22399}")))
      (is-shape voice ("file_id" "v1") ("id" "uv1" "the file's unique id is its identity")
        ("media_type" "audio/ogg") ("filename" "voice.ogg") ("size" = 22399) ("seconds" = 6)))
    (let ((audio (recording 31 "\"caption\": \"what is he saying\",
                                \"audio\": {\"file_id\": \"a1\", \"duration\": 94,
                                            \"mime_type\": \"audio/mp4\",
                                            \"file_name\": \"memo.m4a\"}")))
      (is-shape audio ("media_type" "audio/mp4") ("filename" "memo.m4a") ("seconds" = 94)))
    (let ((note (recording 32 "\"video_note\": {\"file_id\": \"n1\", \"length\": 240,
                                                \"duration\": 12, \"file_size\": 90000}")))
      (is-shape note ("media_type" "video/mp4") ("filename" "video-note.mp4") ("seconds" = 12)))
    (let ((document (recording 33 "\"document\": {\"file_id\": \"d9\",
                                                  \"file_name\": \"call.mp3\",
                                                  \"mime_type\": \"audio/mpeg\"}")))
      (is-shape document ("file_id" "d9") ("media_type" "audio/mpeg"))))
  ;; A reply to a voice note carries the note it answers.
  (let* ((candidate (tg-said 34 "\"text\": \"do u hear this\",
                               \"reply_to_message\": {
                                 \"message_id\": 30, \"from\": {\"id\": 42},
                                 \"voice\": {\"file_id\": \"v1\", \"file_unique_id\": \"uv1\",
                                             \"duration\": 6}}"))
         (answered (gethash "attachments" (gethash "reply" candidate))))
    (is (= 1 (length answered)))
    (is (equal "uv1" (gethash "id" (aref answered 0))))))

(deftest channel-telegram-a-topic-the-bot-opened-is-one-conversation ()
  ;; Telegram files every line of a forum topic as a reply to the service
  ;; message that opened it. That is no gesture of the person's: it neither
  ;; replies to anything nor addresses the bot -- but a topic the BOT opened
  ;; (for an ask) is one conversation, joined, where no mention is needed, as
  ;; in a Discord thread the bot is in. A real reply to the bot still is one.
  (flet ((in-topic (creator &optional (reply-fields ""))
           (tg-message
            (format nil "{\"message_id\": 20, \"message_thread_id\": 3, \"date\": 1700000000,
              \"chat\": {\"id\": -100555, \"type\": \"supergroup\", \"is_forum\": true, \"title\": \"Lab\"},
              \"from\": {\"id\": 42, \"username\": \"kim\"}, \"text\": \"and the tests?\",
              \"reply_to_message\": {\"message_id\": 3, ~a
                                   \"from\": {\"id\": ~a, \"username\": ~s}}}"
                    reply-fields creator (if (eql creator 777) "orgbot" "ada"))
            :bot-user-id "777" :bot-username "orgbot")))
    (let ((bot-topic (in-topic 777 "\"forum_topic_created\": {\"name\": \"Fix the build\"},"))
          (their-topic (in-topic 99 "\"forum_topic_created\": {\"name\": \"General chat\"},"))
          (reply (in-topic 777 "\"text\": \"Done: 3 files.\",")))
      (flet ((field (candidate key) (gethash key (nck:candidate-source candidate))))
        (is-source bot-topic :text "and the tests?" "chat_kind" "thread" "thread_id" "3"
                   "reply_to_message_id" nil "chat_title" "Lab")
        (is (eq t (field bot-topic "joined")))
        (is (null (field bot-topic "addressed")))
        (is (eq t (field bot-topic "chat_is_forum")))
        (is (eql 1700000000000 (field bot-topic "sent_ms")))
        (is (null (gethash "reply" bot-topic)) "the topic's root is not the message answered")
        (is (null (field their-topic "joined")))
        (is (null (field their-topic "addressed")))
        (is (null (field reply "joined")))
        (is (eq t (field reply "addressed")))
        (is-source reply "reply_to_message_id" "3")))))

(deftest channel-telegram-a-reaction-is-a-wordless-reply ()
  ;; Telegram says a person's reaction set before and after; each emoji that
  ;; came or went is one gesture, pointing at the message it sits on. A bot's,
  ;; the bot's own and a custom emoji are none.
  (flet ((reaction (user old new)
           (nct::map-telegram-reaction
            (cell-json (format nil "{\"chat\": {\"id\": -100555, \"type\": \"supergroup\"},
                                    \"message_id\": 90, \"date\": 1700000000, \"user\": ~a,
                                    \"old_reaction\": ~a, \"new_reaction\": ~a}" user old new))
            :bot-user-id "777")))
    (let ((changed (reaction "{\"id\": 42, \"username\": \"kim\"}"
                             "[{\"type\": \"emoji\", \"emoji\": \"👍\"}]"
                             "[{\"type\": \"emoji\", \"emoji\": \"🔥\"}, {\"type\": \"custom_emoji\", \"custom_emoji_id\": \"5\"}]")))
      (is (= 2 (length changed)))
      (is-source (first changed) :text "🔥" "reaction" "add" "reply_to_message_id" "90"
                 "channel_id" "-100555" "user_id" "42" "chat_kind" "group" "message_id" nil)
      (is-source (second changed) :text "👍" "reaction" "remove"))
    (is (null (reaction "{\"id\": 9, \"is_bot\": true}" "[]" "[{\"type\": \"emoji\", \"emoji\": \"👍\"}]")))
    (is (null (reaction "{\"id\": 777}" "[]" "[{\"type\": \"emoji\", \"emoji\": \"👀\"}]")) "the bot's own eye")))

(deftest channel-telegram-policy-reads-the-wider-rules (let ((policy (nct:telegram-inbound-policy
                                                                  :allowed-users '("1")
                                                                  :allowed-roles '("administrator")
                                                                  :require-mention t
                                                                  :free-response-chats '("-100777")
                                                                  :ignored-chats '("-100888")
                                                                  :mention-patterns '("nodecode")
                                                                  :allow-private-chats t
                                                                  :allow-group-chats t
                                                                  :allow-forum-topics t
                                                                  :bot-username "MyBot"))))
  ;; The Discord section's room rules, read the same way: a chat that answers
  ;; every line, one never read, a wake word, and a group's admins driving the
  ;; bot as a listed user does.
  (flet ((said (text channel &key (user "1") roles)
           (nlk:json-object "text" text
                            "source" (nlk:json-object "chat_kind" "group" "channel_id" channel
                                                      "user_id" user "role_ids" roles))))
    (is (eq :answer (nck:decide-inbound policy (said "no ping here" "-100777"))))
    (is-inbound policy (said "@mybot hi" "-100888") :reject "channel_ignored")
    ;; a wake word is a mention
    (is (eq :answer (nck:decide-inbound policy (said "Nodecode, what broke?" "-100555"))))
    (is-inbound policy (said "no ping" "-100555") :observe "mention_required")
    (is (eq :answer (nck:decide-inbound policy (said "@mybot hi" "-100555" :user "9"
                                                                         :roles '("administrator")))))
    (is-inbound policy (said "@mybot hi" "-100555" :user "9") :reject "user_not_allowed")))
