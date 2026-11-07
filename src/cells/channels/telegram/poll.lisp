;;;; poll.lisp --- getUpdates long-polling: plans and pure update folding.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One Bot API message becomes the shared candidate shape (the kit names
;;;; the room and lane sessions from it), one getUpdates batch folds to
;;;; candidates plus the next offset (1 + max update_id, gaps included). The
;;;; callback-query/approval lane is not ported — the approval subsystem is
;;;; excluded from this organism by design.

(in-package #:nodecode-channel-telegram)

(defun number-string (value)
  "Telegram ids arrive as JSON numbers; the wire shape wants strings."
  (cond ((stringp value) (and (plusp (length value)) value))
        ((integerp value) (format nil "~a" value))))

(defun reply-to-bot-p (message bot-user-id bot-username)
  "Whether MESSAGE replies to one of the bot's own messages: the replied
message's author is the bot by id, or by username when only that is known."
  (let ((from (nlk:json-value message :object "reply_to_message" "from")))
    (and from
         (or (and bot-user-id
                  (equal bot-user-id (number-string (gethash "id" from))))
             (and bot-username
                  (string-equal (string-left-trim "@" bot-username)
                                (or (nlk:json-value from :string "username")
                                    ""))))
         t)))

(defun telegram-attachment-entry (file media-type filename &key seconds-p)
  "One Bot API file object as a candidate attachment entry, or NIL when it
names no file. MEDIA-TYPE and FILENAME stand in for what the object does not
declare; SECONDS-P carries the length a recording declares."
  (nlk:when-let (file-id (nlk:json-value file :string "file_id"))
    (nlk:json-object
     "id" (nlk:json-value file :string "file_unique_id")
     "file_id" file-id
     "media_type" (or (nlk:json-value file :string "mime_type") media-type)
     "filename" (or (nlk:json-value file :string "file_name") filename)
     "size" (or (nlk:json-value file :integer "file_size") 0)
     :when seconds-p "seconds" (nlk:json-value file :number "duration"))))

(defun telegram-message-attachments (message)
  "MESSAGE's attachments as the candidate's attachments array: the largest
photo; a voice note, an audio file, a round video note — each a recording,
its declared length carried as seconds; a video; and a document — one entry
each with the file id the Bot API addresses
it by, the file's unique id (one file seen twice, in a message and in a reply
to it, is one file), the filename, the declared type and the size the
platform gave it."
  ;; NIL when the message carries none. Nothing is downloaded here: the bytes
  ;; are fetched when the ask's turn is prepared, from the fetcher
  ;; ATTACH-TELEGRAM-FILE-FETCHERS leaves on the entry where the update is
  ;; admitted.
  (let ((entries '())
        (photo nil)
        (best-score -1)
        (document (nlk:json-value message :object "document")))
    (flet ((take (entry) (when entry (push entry entries))))
      ;; One picture at several resolutions: the largest by declared size, pixels breaking a tie.
      (loop for size across (nlk:json-array message "photo")
            for score = (or (nlk:json-value size :integer "file_size")
                            (* (or (nlk:json-value size :integer "width") 0)
                               (or (nlk:json-value size :integer "height") 0)))
            when (and (hash-table-p size) (>= score best-score))
              do (setf photo size
                       best-score score))
      (take (telegram-attachment-entry photo "image/jpeg" "photo.jpg"))
      ;; A recording is its own message kind, whatever type it declares.
      (loop for (key media-type filename) in '(("voice" "audio/ogg" "voice.ogg")
                                               ("audio" "audio/mpeg" "audio.mp3")
                                               ("video_note" "video/mp4" "video-note.mp4"))
            do (take (telegram-attachment-entry (nlk:json-value message :object key)
                                                media-type filename :seconds-p t)))
      ;; A video and a document ride whatever they are: the kit reads what it
      ;; can in and saves the rest for the lane's tools.
      (take (telegram-attachment-entry (nlk:json-value message :object "video")
                                       "video/mp4" "video.mp4"))
      (take (telegram-attachment-entry document nil "")))
    (when entries
      (coerce (nreverse entries) 'vector))))

(defun map-telegram-press (callback line &aux (message (nlk:json-value callback :object "message"))
                                             (from (nlk:json-value callback :object "from")))
  "The candidate a press on a choice is -- CALLBACK the callback_query
Telegram sent, LINE what the choice says (SAID-LINE): the presser saying
LINE where the card pressed is, a reply to it -- or NIL without a chat or
someone who pressed."
  ;; Addressed by construction, as a Discord press is: the person pressed
  ;; something one of this bot's messages carries. It has no message id of
  ;; its own; press_data and the query's id are its return path
  ;; (PRESS-RESPONSE-PLAN), the card its answer becomes.
  (nlk:when-let (chat-id (and from (number-string (nlk:json-value message :any "chat" "id"))))
    (let* ((chat-type (or (nlk:json-value message :string "chat" "type") "private"))
           (thread-id (number-string (gethash "message_thread_id" message))))
      (nlk:json-object
       "text" line
       "source"
       (nlk:json-object
        "platform" "telegram"
        "chat_kind" (cond (thread-id "thread")
                          ((equal chat-type "private") "direct_message")
                          ((member chat-type '("group" "supergroup") :test #'equal) "group")
                          (t "channel"))
        "channel_id" chat-id
        "thread_id" thread-id
        "user_id" (number-string (nlk:json-value from :any "id"))
        "user_name" (or (nlk:json-value from :string "username")
                        (nlk:json-value from :string "first_name"))
        "pressed" t
        "press_data" (gethash "data" callback)
        "callback_query_id" (number-string (gethash "id" callback))
        "reply_to_message_id" (number-string (gethash "message_id" message))
        "card_text" (nlk:json-value message :string "text")
        "addressed" t
        "is_bot" (and (nlk:json-value from :any "is_bot") t))))))

(defun map-telegram-message (message &key bot-user-id bot-username)
  "One Bot API message object as the shared candidate, or NIL when it has
no chat id."
  ;; Captions count as text (photo posts carry their prompt there), and an
  ;; attachment rides the candidate's attachments array — the file id here,
  ;; the bytes when the ask's turn is prepared. A reply to one of the bot's
  ;; own messages is `addressed': the platform routed it to us, so the mention
  ;; gate has nothing to ask — the section's promise that a reply counts as a
  ;; mention. BOT-USER-ID and BOT-USERNAME are the identity that decides it;
  ;; neither known, nothing is addressed.
  (nlk:when-let (chat-id (number-string (nlk:json-value message :any "chat" "id")))
    (let* ((from (gethash "from" message))
           (root (topic-root message))
           (ref (and (not root) (nlk:json-value message :object "reply_to_message")))
           (chat-type (or (nlk:json-value message :string "chat" "type") "private"))
           (group-p (member chat-type '("group" "supergroup")
                            :test #'equal))
           (thread-id (number-string
                       (gethash "message_thread_id" message)))
           (chat-kind (cond (thread-id "thread")
                            ((equal chat-type "private")
                             "direct_message")
                            (group-p "group")
                            (t "channel"))))
      (nlk:json-object
       "text" (or (nlk:json-value message :string "text")
                  (nlk:json-value message :string "caption")
                  "")
       "attachments" (telegram-message-attachments message)
       "reply" (and ref (nlk:json-object
                         "id" (number-string (gethash "message_id" ref))
                         "user_id" (number-string (nlk:json-value ref :any "from" "id"))
                         "user_name" (or (nlk:json-value ref :string "from" "username")
                                         (nlk:json-value ref :string "from" "first_name"))
                         "text" (or (nlk:json-value ref :string "text")
                                    (nlk:json-value ref :string "caption")
                                    "")
                         ;; A reply to a voice note is about what it says.
                         "attachments" (telegram-message-attachments ref)))
       "source"
       (nlk:json-object
        "platform" "telegram"
        "chat_kind" chat-kind
        "channel_id" chat-id
        "thread_id" thread-id
        "user_id" (number-string (nlk:json-value from :any "id"))
        "user_name" (or (nlk:json-value from :string "username")
                        (nlk:json-value from :string "first_name"))
        "message_id" (number-string (gethash "message_id" message))
        ;; The reply gesture. Telegram's own pointer to another message
        ;; is the whole branch structure of a flat chat: a reply to
        ;; anything a lane produced continues that lane.
        "reply_to_message_id"
        (and ref (number-string (gethash "message_id" ref)))
        "addressed" (and ref (reply-to-bot-p message bot-user-id bot-username))
        ;; A topic the bot opened for an ask is one conversation, as a
        ;; Discord thread is: a line typed in it needs no mention. Topics
        ;; people opened stay standing rooms, where the mention rule holds.
        "joined" (and root (not (nlk:json-value from :any "is_bot"))
                      (reply-to-bot-p message bot-user-id bot-username))
        "chat_is_forum" (and (nlk:json-value message :any "chat" "is_forum") t)
        "chat_title" (nlk:json-value message :string "chat" "title")
        ;; When it was said (an edit: when it was edited), for the catch-up
        ;; after the bot was away (TELEGRAM-CATCH-UP).
        "sent_ms" (nlk:when-let (seconds (or (nlk:json-value message :integer "edit_date")
                                             (nlk:json-value message :integer "date")))
                    (* 1000 seconds))
        "is_bot" (and (nlk:json-value from :any "is_bot") t))))))

(defun topic-root (message &aux (ref (nlk:json-value message :object "reply_to_message")))
  "The service message that opened the forum topic MESSAGE was typed in, when
MESSAGE answers nothing else: Telegram files a topic's lines as replies to
it, a reply that is no gesture of the person's. NIL otherwise."
  (and ref (gethash "message_thread_id" message)
       (nlk:json-value ref :object "forum_topic_created")
       ref))

(defun reaction-emojis (reactions)
  "The emoji of each plain reaction in REACTIONS, a ReactionType array."
  (loop for reaction across (if (vectorp reactions) reactions #())
        when (equal "emoji" (nlk:json-value reaction :string "type"))
          collect (nlk:json-value reaction :string "emoji")))

(defun map-telegram-reaction (update &key bot-user-id
                                     &aux (user (nlk:json-value update :object "user")))
  "The candidates one message_reaction UPDATE is -- one per emoji added or
taken back, the emoji its text and the message it sits on its reply gesture,
as the kit reads a reaction (NCK:REACTION-NOTICED) -- or NIL: an anonymous
admin's, a bot's, the bot's own, or a change that is no plain emoji."
  ;; Telegram says the set before and after; the difference is the gesture.
  ;; Whose message it sits on it does not say: the kit finds the lane by the
  ;; message's address, and a reaction on nothing of ours goes nowhere.
  (let* ((chat-id (number-string (nlk:json-value update :any "chat" "id")))
         (user-id (number-string (nlk:json-value user :any "id")))
         (message-id (number-string (nlk:json-value update :any "message_id")))
         (before (reaction-emojis (nlk:json-value update :any "old_reaction")))
         (after (reaction-emojis (nlk:json-value update :any "new_reaction")))
         (chat-type (or (nlk:json-value update :string "chat" "type") "private")))
    (when (and chat-id user-id message-id
               (not (nlk:json-value user :any "is_bot"))
               (not (equal user-id bot-user-id)))
      (flet ((one (emoji kind)
               (nlk:json-object
                "text" emoji
                "source"
                (nlk:json-object
                 "platform" "telegram"
                 "chat_kind" (if (equal chat-type "private") "direct_message" "group")
                 "channel_id" chat-id
                 "user_id" user-id
                 "user_name" (or (nlk:json-value user :string "username")
                                 (nlk:json-value user :string "first_name"))
                 "reply_to_message_id" message-id
                 "reaction" kind
                 "addressed" t))))
        (append (loop for emoji in (set-difference after before :test #'equal)
                      collect (one emoji "add"))
                (loop for emoji in (set-difference before after :test #'equal)
                      collect (one emoji "remove")))))))

(defun next-update-offset (updates last-offset &aux (next-offset last-offset))
  "The getUpdates offset after UPDATES: 1 + the highest update_id seen,
never below LAST-OFFSET."
  ;; Advances over every update — unmappable ones included — so a poison
  ;; update can never wedge the poll.
  (loop for update across (if (vectorp updates) updates #())
        for id = (nlk:json-value update :integer "update_id")
        when id
          do (setf next-offset (max (or next-offset 0) (1+ id))))
  next-offset)

(defparameter +default-allowed-updates+ '("message" "edited_message" "callback_query"
                                          "message_reaction")
  "What the poll asks Telegram for when channels.telegram.allowed_updates
is unset: the kinds the adapter itself acts on -- messages, an edit that
turns one into an ask, presses on the controls its messages carry, and a
reaction on one of its own messages.")

(defun get-updates-plan (offset &key (poll-timeout-seconds 2)
                                     (allowed-updates +default-allowed-updates+))
  "The long-poll plan."
  ;; ALLOWED-UPDATES is the update kinds Telegram should deliver — the
  ;; Telegram twin of Discord's intents; every kind listed reaches
  ;; TELEGRAM-HANDLE-UPDATE and whatever advises it. The window is short
  ;; (seconds, not Telegram's customary 25-50) because the poll thread's
  ;; blocking read is also the lane's stop latency: nothing may interrupt a
  ;; thread parked inside a TLS read; doing so would corrupt libssl and
  ;; wedge process exit, so the supervisor simply waits a lap out —
  ;; and a lap is at most one window long. An update ends the window
  ;; immediately, so message latency is unaffected; only the idle re-poll
  ;; cadence rises. The HTTP timeout carries a +10s margin over the window so
  ;; the server's own timeout always wins the race, and retry_server_errors
  ;; stays NIL: the poll loop is its own retry.
  (bot-plan "/getUpdates" (nlk:json-object
                           "timeout" poll-timeout-seconds
                           "allowed_updates" (coerce allowed-updates 'vector)
                           :when (integerp offset) "offset" offset)
            (+ poll-timeout-seconds 10) nil "get_updates"))

(defun telegram-inbound-policy (&key allowed-chats allowed-threads
                                     allowed-users allowed-roles require-mention
                                     free-response-chats require-mention-chats
                                     ignored-chats mention-patterns
                                     allow-private-chats allow-group-chats
                                     allow-forum-topics bot-username bot-user-id
                                     &allow-other-keys)
  "The kit policy carrying the Zig pack's Telegram reason vocabulary."
  ;; ALLOWED-ROLES are a group member's standing there -- \"creator\",
  ;; \"administrator\" -- which the adapter puts on source.role_ids
  ;; (TELEGRAM-AUTHOR-ROLES). A wake word in MENTION-PATTERNS addresses the bot
  ;; as its @handle does.
  (make-inbound-policy
   :allowed-channels allowed-chats
   :allowed-thread-ids allowed-threads
   :allowed-users allowed-users
   :allowed-roles allowed-roles
   :dm-policy (if allow-private-chats :enabled :disabled)
   :group-policy (if allow-group-chats :enabled :disabled)
   :thread-policy (if allow-forum-topics :enabled :disabled)
   :require-mention require-mention
   :free-response-channels free-response-chats
   :require-mention-channels require-mention-chats
   :ignored-channels ignored-chats
   :mention-target bot-username
   :mention-test (lambda (text target)
                   (or (telegram-mention-p text target)
                       (some (lambda (pattern)
                               (and (stringp pattern) (plusp (length pattern))
                                    (search pattern (or text "") :test #'char-equal)))
                             mention-patterns)))
   :self-id bot-user-id
   :channel-match :exact
   :reasons '(:channel-not-allowed "chat_not_allowed"
              :thread-not-supported "forum_topics_disabled"
              :dm-disabled "private_chats_disabled"
              :group-disabled "group_chats_disabled"
              :mention-required-without-user "mention_required_without_bot_username")))
