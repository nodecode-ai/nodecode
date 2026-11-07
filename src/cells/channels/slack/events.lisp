;;;; events.lisp --- Slack events as the kit's candidates. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A message event becomes the shared candidate shape (the kit names the
;;;; room and the lane from it), a slash command the command line it spells,
;;;; a button press the kit's normal control press. Nothing here touches the
;;;; network: a person's display name and whether the bot already speaks in a
;;;; thread are the adapter's to know, passed in.
;;;;
;;;; Rooms and threads. A Slack thread is not a channel of its own: it is the
;;;; replies hung off one message, named by that message's ts. The kit's lane
;;;; is the same shape -- one ask and what follows from it -- so a message in a
;;;; thread is normalized as a reply to the thread's first message, in the
;;;; channel's room. The ask that opened a thread is a lane's address, so a
;;;; reply in the thread continues that lane; the bot answers in the thread
;;;; (rest.lisp threads every reply to its root).

(in-package #:nodecode-channel-slack)

;;; --- text ------------------------------------------------------------------------

(defun slack-plain-text (text)
  "Slack's message TEXT as a person reads it: a channel link as its #name, a
link as its label and address, @here as typed, the three escaped characters
as themselves. A user mention stays <@U...> and a group's <!subteam^S...>:
the handle the model writes to mention them back, and the one a group the
section names is matched by (SLACK-GROUP-MENTION-P)."
  (flet ((reference (match inner)
           (let ((label (let ((bar (position #\| inner))) (and bar (subseq inner (1+ bar)))))
                 (head (subseq inner 0 (or (position #\| inner) (length inner)))))
             (cond ((uiop:string-prefix-p "@" inner) match)
                   ((uiop:string-prefix-p "#" inner) (if label (format nil "#~a" label) match))
                   ((uiop:string-prefix-p "!subteam^" inner) match)
                   ((uiop:string-prefix-p "!date^" inner) (or label match))
                   ((uiop:string-prefix-p "!" inner) (format nil "@~a" (subseq head 1)))
                   ((uiop:string-prefix-p "mailto:" head) (or label (subseq head 7)))
                   ((or (null label) (string= label head)) head)
                   (t (format nil "~a (~a)" label head))))))
    (let ((plain (ppcre:regex-replace-all
                  "<([^<>]*)>" (or text "")
                  (lambda (target start end match-start match-end reg-starts reg-ends)
                    (declare (ignore start end))
                    (reference (subseq target match-start match-end)
                               (subseq target (aref reg-starts 0) (aref reg-ends 0)))))))
      (uiop:frob-substrings plain '("&lt;" "&gt;" "&amp;")
                            (lambda (entity emit)
                              (funcall emit (cond ((string= entity "&lt;") "<")
                                                  ((string= entity "&gt;") ">")
                                                  (t "&"))))))))

(defun slack-mention-p (text bot-user-id)
  "Whether TEXT mentions BOT-USER-ID: Slack writes a mention as <@U...>."
  (and (stringp text) (stringp bot-user-id) (plusp (length bot-user-id))
       (or (search (format nil "<@~a>" bot-user-id) text)
           (search (format nil "<@~a|" bot-user-id) text))
       t))

(defun slack-strip-mention (text bot-user-id)
  "TEXT with our own mention removed: the model reads the ask, not the
handle that routed it."
  (when (and (stringp bot-user-id) (plusp (length bot-user-id)))
    (setf text (ppcre:regex-replace-all
                (format nil "<@~a(?:\\|[^>]*)?>" (ppcre:quote-meta-chars bot-user-id))
                (or text "") "")))
  (nlk:trimmed text))

;;; --- messages --------------------------------------------------------------------

(defparameter +message-subtypes+ '("file_share" "thread_broadcast" "bot_message" "me_message")
  "The message subtypes that are something said; every other subtype -- an
edit, a deletion, a join, a topic change -- is the channel's bookkeeping.")

(defun slack-chat-kind (channel-type channel-id)
  "The kit's chat kind for a Slack channel: a DM, a group DM, or a channel,
public or private."
  ;; An event names the kind; a slash command only the id, whose D says DM.
  (cond ((equal channel-type "im") "direct_message")
        ((equal channel-type "mpim") "group")
        (channel-type "channel")
        ((uiop:string-prefix-p "D" (or channel-id "")) "direct_message")
        (t "channel")))

(defun slack-attachments (event)
  "EVENT's files as the candidate's attachments array: the file's id, its
declared type, name and size, and the address its bytes are fetched from
with the bot token."
  ;; NIL when it carries none. Every file rides: the kit reads what it can in
  ;; and saves the rest for the lane's tools (NCK::READ-ATTACHMENT). The fetch
  ;; thunk is the adapter's to add (ATTACH-SLACK-FILE-FETCHERS): the address
  ;; alone answers a sign-in page.
  (let ((entries (loop for file across (nlk:json-array event "files")
                       for type = (nlk:json-value file :string "mimetype")
                       for url = (or (nlk:json-value file :string "url_private_download")
                                     (nlk:json-value file :string "url_private"))
                       when url
                         collect (nlk:json-object
                                  "id" (nlk:json-value file :string "id")
                                  "download" url
                                  "media_type" type
                                  "filename" (or (nlk:json-value file :string "name") "")
                                  "size" (or (nlk:json-value file :integer "size") 0)))))
    (and entries (coerce entries 'vector))))

(defun slack-message-candidate (event &key team-id bot-user-id bot-thread-p user-name)
  "One message EVENT as the shared candidate, or NIL for an event that is
not something said: the words, the files, and the source coordinates, the
thread as the reply gesture."
  ;; BOT-THREAD-P answers whether the bot already speaks in the thread a
  ;; (CHANNEL THREAD-TS) names: a message there is addressed, the way a reply
  ;; to the bot is, so a thread the bot answers in needs no mention to go on.
  ;; A thread under one of the bot's own messages is addressed the same way.
  (let* ((subtype (nlk:json-value event :string "subtype"))
         (channel (nlk:json-value event :string "channel"))
         (ts (nlk:json-value event :string "ts"))
         (thread (nlk:json-value event :string "thread_ts"))
         (root (and thread (not (equal thread ts)) thread)))
    (when (and (equal "message" (nlk:json-value event :string "type"))
               channel ts
               (or (null subtype) (member subtype +message-subtypes+ :test #'equal)))
      (nlk:json-object
       "text" (slack-plain-text (nlk:json-value event :string "text"))
       "attachments" (slack-attachments event)
       "source"
       (nlk:json-object
        "platform" "slack"
        ;; A reply in a channel's thread is in a thread already: the kit
        ;; opens none for it, and the reply gesture finds the lane.
        "chat_kind" (let ((kind (slack-chat-kind (nlk:json-value event :string "channel_type")
                                                 channel)))
                      (if (and root (not (equal kind "direct_message"))) "thread" kind))
        ;; The sender's own workspace: in a channel shared with another
        ;; organization (Slack Connect) its people carry theirs, which is what
        ;; allowed_workspaces gates.
        "workspace_id" (or (nlk:json-value event :string "user_team")
                           (nlk:json-value event :string "team")
                           team-id)
        "channel_id" channel
        "user_id" (nlk:json-value event :string "user")
        "user_name" user-name
        "message_id" ts
        ;; The thread is the reply gesture: its first message is what this
        ;; one answers, and a lane that ask opened goes on here.
        "reply_to_message_id" root
        "addressed" (and root
                         (or (and bot-user-id
                                  (equal bot-user-id (nlk:json-value event :string "parent_user_id")))
                             (and bot-thread-p (funcall bot-thread-p channel root)))
                         t)
        "is_bot" (and (or (nlk:json-value event :string "bot_id")
                          (equal subtype "bot_message"))
                      t))))))

(defun slack-edited-message (event)
  "The message a message_changed EVENT says a person's message became, in
the shape a new one arrives in -- its channel and the channel's kind carried
over -- or NIL for any other event, and for an edit of a bot's message (the
lane's own status lines are edited all turn long)."
  (let ((message (and (equal "message_changed" (nlk:json-value event :string "subtype"))
                      (nlk:json-value event :object "message"))))
    (when (and message
               (null (nlk:json-value message :string "subtype"))
               (null (nlk:json-value message :string "bot_id"))
               (nlk:json-value message :string "user"))
      (let ((edited (make-hash-table :test #'equal)))
        (maphash (lambda (key value) (setf (gethash key edited) value)) message)
        (setf (gethash "type" edited) "message"
              (gethash "channel" edited) (nlk:json-value event :string "channel"))
        (nlk:when-let (kind (nlk:json-value event :string "channel_type"))
          (setf (gethash "channel_type" edited) kind))
        (unless (gethash "team" edited)
          (nlk:when-let (team (nlk:json-value event :string "team"))
            (setf (gethash "team" edited) team)))
        edited))))

(defun slack-reaction-candidate (event &key team-id bot-user-id root-of)
  "A reaction_added or reaction_removed EVENT as the shared candidate -- a
line the room said without typing one: the emoji, :name: as Slack names it,
its words, the message it was left on its reply gesture -- or NIL for the
bot's own and for one left on anything but a message. ROOT-OF answers the
thread a (CHANNEL TS) message lives in, when known."
  ;; The bot's own: the kit's marks come and go all turn long, and a route that
  ;; read them back would answer itself. ADDRESSED is the fact a reply
  ;; carries -- the message reacted to is the bot's -- so a reaction on one of
  ;; ours has nothing left for the mention gate to ask.
  (let* ((type (nlk:json-value event :string "type"))
         (added (equal type "reaction_added"))
         (item (nlk:json-value event :object "item"))
         (channel (nlk:json-value item :string "channel"))
         (ts (nlk:json-value item :string "ts"))
         (user (nlk:json-value event :string "user"))
         (name (nlk:json-value event :string "reaction")))
    (when (and (or added (equal type "reaction_removed"))
               (equal "message" (nlk:json-value item :string "type"))
               channel ts user name
               (not (equal user bot-user-id)))
      (let ((kind (slack-chat-kind nil channel))
            (root (and root-of (funcall root-of channel ts))))
        (nlk:json-object
         "text" (format nil ":~a:" name)
         "source"
         (nlk:json-object
          "platform" "slack"
          "chat_kind" (if (and root (not (equal kind "direct_message"))) "thread" kind)
          "workspace_id" (or (nlk:json-value event :string "user_team") team-id)
          "channel_id" channel
          "user_id" user
          ;; The gesture points at the message it sits on, and at nothing
          ;; else: a reaction is not a message and has no ts of its own.
          "reply_to_message_id" ts
          "reaction" (if added "add" "remove")
          "addressed" (and bot-user-id
                           (equal bot-user-id (nlk:json-value event :string "item_user"))
                           t)
          "is_bot" nil))))))

(defun slack-reply-context (message)
  "MESSAGE, a thread's first message as conversations.replies answers it,
as the reply a candidate carries (NCK::CANDIDATE-REPLY): the message an ask
in someone else's thread answers."
  (nlk:json-object "id" (or (nlk:json-value message :string "ts") "")
                   "user_id" (or (nlk:json-value message :string "user")
                                 (nlk:json-value message :string "bot_id"))
                   "user_name" (nlk:json-value message :string "user_profile" "display_name")
                   "text" (slack-plain-text (nlk:json-value message :string "text"))
                   "attachments" (slack-attachments message)))

;;; --- the slash command and the button --------------------------------------------

(defun slack-command-candidate (payload)
  "A slash command PAYLOAD -- the app's one command, /nodecode, and what was
typed after it -- as the candidate for the command line it spells: `/nodecode
models grok' is `/models grok', a bare `/nodecode' is `/help'."
  ;; Addressed by construction: the person picked this app's command. The
  ;; source carries the response_url, the return path the platform's
  ;; PLAN-RESPOND answers through; a command is not a message and has no id.
  (let* ((typed (nlk:trimmed (or (nlk:json-value payload :string "text") "")))
         (channel (nlk:json-value payload :string "channel_id")))
    (when channel
      (nlk:json-object
       "text" (cond ((zerop (length typed)) "/help")
                    ((char= #\/ (char typed 0)) typed)
                    (t (format nil "/~a" typed)))
       "source"
       (nlk:json-object
        "platform" "slack"
        "chat_kind" (slack-chat-kind nil channel)
        "workspace_id" (nlk:json-value payload :string "team_id")
        "channel_id" channel
        "user_id" (nlk:json-value payload :string "user_id")
        "user_name" (nlk:json-value payload :string "user_name")
        "response_url" (nlk:json-value payload :string "response_url")
        "addressed" t
        "is_bot" nil)))))

(defun slack-press-value (action)
  "The data one ACTION of a block_actions payload carries: a button's value,
or the value of the option a select was set to."
  (or (nlk:json-value action :string "value")
      (nlk:json-value action :string "selected_option" "value")))

(defun slack-control-press (payload)
  "A block_actions PAYLOAD -- a press on a button a lane posted, or a pick
from its menu -- as the kit's normal control press, or NIL for any other
interaction."
  ;; The socket's acknowledgement is the press's answer: there is no spinner
  ;; to stop. What the kit says back to the presser alone (a card's Details)
  ;; is ACK-PLAN's, in the thread the pressed message lives in.
  (let ((action (and (equal "block_actions" (nlk:json-value payload :string "type"))
                     (find-if #'hash-table-p (nlk:json-array payload "actions")))))
    (when (and action (slack-press-value action))
      (list :id (or (nlk:json-value action :string "action_ts")
                    (nlk:json-value payload :string "trigger_id"))
            :data (slack-press-value action)
            :user-id (nlk:json-value payload :string "user" "id")
            :user-name (or (nlk:json-value payload :string "user" "username")
                           (nlk:json-value payload :string "user" "name"))
            :message-id (or (nlk:json-value payload :string "container" "message_ts")
                            (nlk:json-value payload :string "message" "ts"))
            :channel-id (or (nlk:json-value payload :string "container" "channel_id")
                            (nlk:json-value payload :string "channel" "id"))
            :thread-id (or (nlk:json-value payload :string "container" "thread_ts")
                           (nlk:json-value payload :string "message" "thread_ts"))))))

(defun slack-press-candidate (payload line &key card-text)
  "The candidate a press on a choice is -- PAYLOAD the block_actions Slack
sent, LINE what the choice says (NCK:SAID-LINE): the presser saying LINE
where the message pressed is, a reply to it -- or NIL without a channel or
someone who pressed. CARD-TEXT is the words that message was posted with,
which Slack hands back only as rich text (NCS:PRESSED-WORDS)."
  ;; Addressed by construction, as a Discord or Telegram press is: the person
  ;; pressed something one of this bot's messages carries. It has no message
  ;; of its own; press_data and the response_url are its return path
  ;; (RESPOND-PLAN), the message pressed the card its answer becomes.
  (let* ((press (slack-control-press payload))
         (channel (getf press :channel-id))
         (user (getf press :user-id)))
    (when (and press channel user)
      (let ((kind (slack-chat-kind nil channel)))
        (nlk:json-object
         "text" line
         "source"
         (nlk:json-object
          "platform" "slack"
          "chat_kind" (if (and (getf press :thread-id) (not (equal kind "direct_message")))
                          "thread"
                          kind)
          "workspace_id" (or (nlk:json-value payload :string "team" "id")
                             (nlk:json-value payload :string "user" "team_id"))
          "channel_id" channel
          "user_id" user
          "user_name" (getf press :user-name)
          "pressed" t
          "press_data" (getf press :data)
          "response_url" (nlk:json-value payload :string "response_url")
          ;; a private answer's card: what answers a press on it stays private
          "card_ephemeral" (nlk:json-value payload :boolean "container" "is_ephemeral")
          "reply_to_message_id" (getf press :message-id)
          "card_text" (or card-text (nlk:json-value payload :string "message" "text"))
          "addressed" t
          "is_bot" nil))))))

;;; --- the policy ------------------------------------------------------------------

(defun slack-group-mention-p (text groups)
  "Whether TEXT mentions one of GROUPS, user group ids: Slack writes a group
mention as <!subteam^S...>."
  (and (stringp text)
       (some (lambda (group)
               (and (stringp group) (plusp (length group))
                    (or (search (format nil "<!subteam^~a>" group) text)
                        (search (format nil "<!subteam^~a|" group) text))))
             groups)
       t))

(defun slack-wake-word-p (text patterns)
  "Whether TEXT says one of PATTERNS, wake words matched without case."
  (and (stringp text)
       (some (lambda (pattern)
               (and (stringp pattern) (plusp (length pattern))
                    (search pattern text :test #'char-equal)))
             patterns)
       t))

(defun slack-inbound-policy (&key allowed-workspaces allowed-channels allowed-users require-mention
                                  free-response-channels require-mention-channels
                                  ignored-channels mention-patterns mention-groups
                                  allow-bots dm-policy group-policy
                                  bot-user-id &allow-other-keys)
  "The kit policy over Slack's rooms: workspaces and channels by id, the
mention as <@U...> -- or a group in MENTION-GROUPS, or a wake word in
MENTION-PATTERNS."
  (make-inbound-policy
   ;; The workspace a person belongs to: with it set, someone from another
   ;; organization in a shared channel is no one the bot answers.
   :allowed-scopes allowed-workspaces
   :allowed-channels allowed-channels
   :allowed-users allowed-users
   :dm-policy (if (equal dm-policy "disabled") :disabled :enabled)
   :group-policy (if (equal group-policy "disabled") :disabled :enabled)
   :require-mention require-mention
   :free-response-channels free-response-channels
   :require-mention-channels require-mention-channels
   :ignored-channels ignored-channels
   :mention-target bot-user-id
   :mention-test (lambda (text target)
                   (or (slack-mention-p text target)
                       (slack-group-mention-p text mention-groups)
                       (slack-wake-word-p text mention-patterns)))
   ;; Other bots' messages: "all", "mentions" (one that mentions this bot),
   ;; else none. Its own are always ignored.
   :allow-bots (cond ((equal allow-bots "all") :all)
                     ((equal allow-bots "mentions") :mentions)
                     (t :none))
   :self-id bot-user-id
   :channel-match :exact
   :reasons '(:scope-not-allowed "workspace_not_allowed"
              :mention-required-without-user "mention_required_without_bot_user")))
