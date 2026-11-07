;;;; adapter.lisp --- the Telegram adapter: the platform, the poll lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host — rooms, lanes, the gate, the digest, the status line, the
;;;; answer, the write-back — is the kit's (host.lisp, room.lisp). This file
;;;; is what Telegram contributes: how a message, an edit, a delete and a
;;;; typing beat are spelled (rest.lisp, bound into a PLATFORM), how a
;;;; mention is written, where the chat is, and the getUpdates long poll
;;;; that turns updates into candidates.
;;;;
;;;; Thread topology (kit rule):
;;;;   - channel-telegram-poll (supervised lap): one getUpdates long poll
;;;;     per lap, then admission through the host. This thread owns the
;;;;     offset.
;;;;   - the host's :FRAME fold and channel-telegram-deliver pool: the kit's.

(in-package #:nodecode-channel-telegram)

(nlk:access (host channel-host))

;; The thread board (board.lisp, loaded after this file).
(declaim (ftype (function (t t) t) start-telegram-board telegram-board-pressed note-telegram-update)
         (ftype (function (t) t) board-noting-executor))

(defstruct (telegram-adapter (:copier nil)
                             (:constructor %make-telegram-adapter))
  (host nil)
  (poll-timeout 10 :type real)
  (poll-interval-ms 1000 :type integer)
  ;; The bot's own identity for the chat contract and the mention strip:
  ;; channels.telegram.bot_username, else getMe's; the id only ever comes
  ;; from getMe.
  (bot-username nil :type (or null string))
  (bot-user-id nil :type (or null string))
  ;; channels.telegram.allowed_updates: the update kinds the poll asks for.
  (allowed-updates +default-allowed-updates+ :type list)
  ;; getUpdates offset, owned by the poll thread.
  (offset nil)
  ;; channels.telegram.catch_up_minutes: how old a message Telegram kept while
  ;; the bot was away may be and still be answered (TELEGRAM-CATCH-UP).
  (catch-up-minutes 60 :type integer)
  ;; Whether the section gates on a member's standing (allowed_roles): only
  ;; then is a group's admin list read (TELEGRAM-AUTHOR-ROLES).
  (roles-p nil :type boolean)
  ;; chat id -> (EXPIRES-UNIX-MS . ((USER-ID . ROLES) ...)), getChatAdministrators
  ;; cached on the poll thread.
  (admins (make-hash-table :test #'equal :synchronized t))
  ;; The forum groups the bot has heard from (chat.is_forum): where an ask
  ;; can open a topic of its own.
  (forums (make-hash-table :test #'equal :synchronized t)))

(defvar *telegram-names* (make-hash-table :test #'equal :synchronized t)
  "user id -> the name they go by, as the chats last showed it: what a mention
of them reads (MENTION-MARKDOWN). Live-only.")

(defvar *topic-chats* (make-hash-table :test #'equal :synchronized t)
  "forum topic id -> the chat it is in, as the chats last showed it: a
topic id is a message id, unique only inside its group, and a call that
names a topic must name its chat (TELEGRAM-TOPIC-CHAT). Live-only.")

(defun unix-ms ()
  "Now, in milliseconds since 1970: the clock Telegram's dates are on."
  (* 1000 (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0))))

(nlk:access (adapter telegram-adapter))

(defvar *telegram-adapter* nil
  "The live Telegram adapter, or NIL when the lane is stopped.")

;;; --- what Telegram contributes to the host --------------------------------------

(defun telegram-strip-mention (text username)
  "TEXT with our own @handle removed, whatever case it was typed in."
  ;; The model should read the ask, not the handle that routed it.
  (when (and (stringp username) (plusp (length username)))
    (let ((handle (if (char= #\@ (char username 0))
                      username
                      (concatenate 'string "@" username))))
      (setf text (remove-all handle text :test #'char-equal))))
  (nlk:trimmed text))

;;; Data, so an operator's layer can reword it; the ids above it are the
;;; adapter's.
(defparameter +telegram-api-primer+
  "Telegram's Bot API is open to you as the bot, from eval:
  (nct:request \"POST\" PATH &key body headers timeout) => (:status N :body VALUE)
PATH is any Bot API method, e.g. \"/setMessageReaction\"; a JSON body is a keyword plist: (:chat_id <chat> :message_id 12 :reaction #((:type \"emoji\" :emoji \"👍\"))). A reply carrying something comes back as (:ok T :result …), arrays as vectors, null as :NULL. A file upload is an alist: ((\"chat_id\" . \"<chat>\") (\"document\" . #p\"/path\")). A write that only acknowledges — Telegram’s {\"ok\":true,\"result\":true} — comes back with an empty :body, since a boolean says the request was accepted and nothing about what is there; where the object can be read it carries :state too, the object read fresh afterwards. That covers the pin and chat verbs (getChat), the member verbs (getChatMember) and the command menu (getMyCommands); :read \"/getChat?chat_id=…\" names another read and :read nil none. No read here confirms a MESSAGE: a reaction, an edit and a deletion cannot be checked from this door, so say what you sent and never that it is there. Bots cannot read chat history — the ids in the user lines are the handles you have. To ping someone, write [their name](tg://user?id=<their user id>) in your answer: Telegram tags and notifies them by that link; a bare name notifies no one. The adapter posts your final answer as a reply to the ask: do not send it again yourself."
  "What the lane contract says about reaching Telegram itself.")

(defun telegram-where-text (candidate bot-username bot-user-id)
  "Where the chat is and who the bot is, for the lane contract: the chat,
the topic, the kind, the bot's handle and id — whichever are known; an
unknown identity is left out, not invented."
  (let ((target (channel-target candidate)))
    (format nil "Telegram chat ~a~@[, topic ~a~]~@[ (~a)~].~
~:[~@[ The bot's user id is ~a.~]~;~:* The bot is @~a~@[, user id ~a~].~]"
            (or (getf target :channel-id) "unknown")
            (getf target :thread-id)
            (source-field candidate "chat_kind")
            bot-username
            bot-user-id)))

(defun telegram-topic-chat (thread-id &aux (adapter *telegram-adapter*) (chats '()))
  "The chat the forum topic THREAD-ID is in: the one a lane working there
names, else the one the chats last showed it in; NIL when none, or when two
lanes in two groups share the id."
  (when adapter
    (map-lanes (host-lanes adapter.host)
               (lambda (lane &aux (target (lane-target lane)))
                 (when (equal thread-id (getf target :thread-id))
                   (pushnew (getf target :channel-id) chats :test #'equal)))))
  (cond ((rest chats) nil)
        (chats (first chats))
        (t (values (gethash thread-id *topic-chats*)))))

(defun telegram-forum-p (chat-id &aux (adapter *telegram-adapter*))
  "Whether CHAT-ID is a forum group the live adapter has heard from."
  (and adapter chat-id (gethash chat-id adapter.forums) t))

(defun telegram-platform (&key bot-username bot-user-id (forum-p #'telegram-forum-p)
                          &aux (platform (telegram-platform-record bot-username bot-user-id forum-p)))
  "The Telegram PLATFORM (TELEGRAM-PLATFORM-RECORD), and what it carries
beside the record: a topic closes as Telegram closes one -- locked, kept, and
open again when an admin reopens it."
  (setf (platform-plan-close-thread platform)
        (lambda (thread-id &key (timeout-seconds 30))
          (nlk:when-let (chat (telegram-topic-chat thread-id))
            (topic-close-plan chat thread-id :timeout-seconds timeout-seconds)))
        (platform-closed-note platform)
        "Closed: the topic is locked. An admin can reopen it from its menu.")
  platform)

(defun telegram-platform-record (bot-username bot-user-id forum-p)
  "The Telegram PLATFORM: Bot API plans, rich messages in 16384-character
chunks, the @handle mention, per-chat message ids (so an address carries
the chat), a plain-text footer, a typing beat that lives ~5 s, reactions
set in one call, the command menu as setMyCommands, a forum topic per ask
where FORUM-P says the chat is a forum, and a mention as a link to the
person."
  ;; A command's private answer goes to the asker's own chat with the bot
  ;; (PRESS-RESPONSE-PLAN): Telegram has no way to show a group line to one
  ;; person alone.
  (make-platform
   :id "telegram"
   :name "Telegram"
   :noun "chat"
   :owner-label "Telegram user id"
   :session-prefix "telegram"
   :contract-section "telegram-chat"
   :text-limit +telegram-message-text-limit+
   :typing-refresh-ms 4000
   :plan-message #'send-message-plan
   :plan-edit #'edit-message-plan
   :plan-delete #'delete-message-plan
   :plan-typing #'typing-plan
   :plan-reaction #'reaction-plans
   :plan-commands (lambda (entries &key timeout-seconds)
                    (list (set-my-commands-plan
                           entries :timeout-seconds timeout-seconds)))
   :message-id-of #'telegram-message-id
   :address-of #'telegram-address
   :strip-mention (lambda (text) (telegram-strip-mention text bot-username))
   :where-text (lambda (candidate)
                 (telegram-where-text candidate bot-username bot-user-id))
   :api-primer +telegram-api-primer+
   :seams-primer "  nct:telegram-handle-update (adapter update) — every raw getUpdates entry within channels.telegram.allowed_updates (default: message, edited_message, callback_query and message_reaction; widen it for my_chat_member, ...), on the poll thread: return fast. update is the hash table Telegram sent."
   ;; A choice is an inline button whose press routes as the line it says
   ;; (TELEGRAM-HANDLE-CALLBACK), answered by becoming the card's next view
   ;; (PRESS-RESPONSE-PLAN); any other control press is answered with
   ;; answerCallbackQuery.
   :choices t
   :plan-respond #'press-response-plan
   :plan-control-ack #'answer-callback-plans
   ;; A forum group's topic is the thread an ask opens (the kit's thread
   ;; path); a group that is no forum, and a private chat, keep the ask where
   ;; it was typed.
   :plan-thread (lambda (target name &key message-id (timeout-seconds 30))
                  (declare (ignore message-id))
                  (when (or (getf target :thread-id)
                            (and forum-p (funcall forum-p (getf target :channel-id))))
                    (topic-plan target name :timeout-seconds timeout-seconds)))
   :thread-id-of #'topic-id-of
   :plan-delete-thread (lambda (thread-id &key (timeout-seconds 30))
                         (nlk:when-let (chat (telegram-topic-chat thread-id))
                           (topic-delete-plan chat thread-id :timeout-seconds timeout-seconds)))
   :mention (lambda (user-id) (mention-markdown user-id (gethash user-id *telegram-names*)))
   :plan-file (lambda (target pathname &key content reply-to voice timeout-seconds)
                "A photo renders inline and anything else is a document, and the file's
own bytes say which; a minute is the floor, a file being heavier than a message."
                (telegram-file-plan target pathname :content content :reply-to reply-to
                                    :voice voice
                                    :timeout-seconds (max 60 (or timeout-seconds 0))))))

;;; --- the door ----------------------------------------------------------------------------

(defun request (method path &rest arguments &key body headers timeout retry
                (read (read-back-path path body))
                &aux (host (and *telegram-adapter* (telegram-adapter-host *telegram-adapter*))))
  "One Telegram Bot API call as the bot: METHOD PATH, answered as
(:status N :body VALUE [:error TEXT]) — the shape the eval snippet prints
legibly, see NCK:CALL."
  ;; PATH is the method, "/sendMessage"; it joins
  ;; onto the token-bearing base URL. BODY is a keyword plist (a JSON object),
  ;; an NLK:JSON-OBJECT, or a multipart alist (("chat_id" . "1")
  ;; ("document" . pathname)). Any method the token allows; nothing here
  ;; narrows it. The token stays on the executor and never appears in the
  ;; answer. Blocks on the caller's thread.
  ;;
  ;; A Bot API write that only acknowledges — Telegram answers it {"ok":true,
  ;; "result":true} — comes back with an empty :BODY, because a boolean says
  ;; the request was accepted and nothing about what is there. Where the
  ;; object can be read it comes back carrying :STATE as well, the object read
  ;; fresh afterwards. READ picks which object: it defaults to
  ;; READ-BACK-PATH's answer for this method and body, a string names another
  ;; read, and NIL asks for none.
  ;;
  ;; A message is the case that has no read. A reaction, an edit and a
  ;; deletion answer with that acknowledgement and nothing here confirms them,
  ;; so they carry no :STATE and what is said of them is what was sent, never
  ;; that it is there. TIMEOUT and RETRY default as NCK:CALL's do.
  (declare (ignore headers timeout retry))
  (unless (and host host.executor)
    (error "the telegram channel is not running"))
  (apply #'call (wrap-telegram-executor host.executor) method path :read read arguments))

;;; --- ingress (poll thread) --------------------------------------------------------------

(defun telegram-file-fetcher (adapter file-id)
  "A thunk that returns the bytes behind FILE-ID, or signals with the
whole truth in its message — never the token, never the URL."
  ;; Built when the update is admitted, on the poll thread; called when the
  ;; ask's turn is prepared, on the kit's delivery pool, so no download ever
  ;; parks the poll. The ceiling a file may take is the kit's, read off the
  ;; size the entry declares before this runs.
  (lambda (&aux (result (execute-plan (host-executor adapter.host) (get-file-plan file-id)))
                (path (and result.ok-p (nlk:json-value result.body :string "result" "file_path"))))
    ;; One getFile call names the download path.
    (unless path
      (error "Telegram did not return a download path for this file"))
    (nlk:with-handlers ((error ()
                          (error "the download from Telegram failed")))
      (dex:get (telegram-file-url path)
               :force-binary t :connect-timeout 10 :read-timeout 30))))

(defun attach-telegram-file-fetchers (adapter candidate)
  "CANDIDATE's attachment entries — the ones the adapter put there with a
file id, on the message and on the message it replies to — each gain the
thunk that fetches the file, bound to ADAPTER."
  ;; Called once, as the update is admitted; nothing here touches the network.
  (dolist (holder (list candidate (nlk:json-value candidate :object "reply")))
    (let ((attachments (nlk:json-value holder :any "attachments")))
      (when (vectorp attachments)
        (loop for attachment across attachments
              when (and (nlk:json-value attachment :string "file_id")
                        (null (gethash "fetch" attachment)))
                do (setf (gethash "fetch" attachment)
                         (telegram-file-fetcher
                          adapter
                          (nlk:json-value attachment :string "file_id")))))))
  candidate)

(defun telegram-handle-update (adapter update)
  "One raw getUpdates entry UPDATE, as Telegram sent it, on the poll
thread: a message update maps to the shared candidate and goes through the
host; an edit that now addresses the bot is the ask it became; a reaction is
a line of context for the conversation it sits in; a callback-query update —
a press on a control a lane posted — is relayed to the kit, which answers it
and acts; every other kind falls through."
  ;; An attachment gains its fetcher
  ;; here — the file id now, the bytes when the ask's turn is prepared — so the
  ;; thread never waits on a download. Exported as the seam a layer advises —
  ;; (hook 'nct:telegram-handle-update key fn) sees edits, reactions, callback
  ;; queries, membership changes, whatever channels.telegram.allowed_updates
  ;; asks for — with the poll's own rule: return fast, this thread is the one
  ;; that polls. Returns the candidate admitted, or NIL.
  (let ((message (gethash "message" update))
        (edited (gethash "edited_message" update))
        (reaction (gethash "message_reaction" update))
        (callback (gethash "callback_query" update)))
    ;; What it says of the bot's topics reaches the thread board first.
    (nlk:with-handlers ((error (condition) (warn "telegram: the board could not note an update: ~a" condition)))
      (note-telegram-update adapter update))
    (cond
      ((hash-table-p message)
       (nlk:when-let (candidate (telegram-arrival adapter message))
         (handle-candidate adapter.host candidate)
         candidate))
      ((hash-table-p edited) (telegram-handle-edit adapter edited))
      ((hash-table-p reaction) (telegram-handle-reaction adapter reaction))
      ((hash-table-p callback)
       (telegram-handle-callback adapter callback)))))

(defun telegram-arrival (adapter message)
  "MESSAGE as the candidate the host is handed, or NIL: no chat, or one
the catch-up leaves unanswered (TELEGRAM-CATCH-UP). On the way it notes what
the chat showed -- the sender's name, a forum, a topic's chat -- and the
sender's standing where the section gates on it."
  (nlk:when-let (candidate (map-telegram-message message
                                                 :bot-user-id adapter.bot-user-id
                                                 :bot-username adapter.bot-username))
    (note-telegram-chat adapter candidate)
    (when (and adapter.roles-p (not (equal "direct_message" (source-field candidate "chat_kind"))))
      (setf (gethash "role_ids" (candidate-source candidate))
            (telegram-author-roles adapter (source-field candidate "channel_id")
                                   (source-field candidate "user_id"))))
    (nlk:when-let (candidate (telegram-catch-up adapter candidate))
      (attach-telegram-file-fetchers adapter candidate))))

(defun note-telegram-chat (adapter candidate)
  "Keep what CANDIDATE shows of its chat: its sender's name (a mention reads
it), whether the chat is a forum (an ask there can open a topic) and the
chat a topic is in (a topic call names it)."
  (let ((user (source-field candidate "user_id"))
        (name (source-field candidate "user_name"))
        (chat (source-field candidate "channel_id"))
        (thread (source-field candidate "thread_id")))
    (when (and user name) (setf (gethash user *telegram-names*) name))
    (when (and chat (gethash "chat_is_forum" (candidate-source candidate)))
      (setf (gethash chat adapter.forums) t))
    (when (and chat thread) (setf (gethash thread *topic-chats*) chat))))

(defparameter +late-ms+ 60000
  "How old a message may be when the poll reads it and still count as said
just now: anything older was said while the bot was away.")

(defun telegram-catch-up (adapter candidate &aux (sent (gethash "sent_ms" (candidate-source candidate))))
  "CANDIDATE, or NIL when it is a message Telegram kept while the bot was
away that is not to be answered: older than channels.telegram.catch_up_minutes
(0: every one), or one that already opened a lane before the gap. One that is
answered says how late it is (source.late_minutes, which the lane reads as
\"sent N minutes ago, while you were offline\")."
  ;; Telegram keeps what nobody polled for 24 hours and hands it over at the
  ;; next getUpdates: the catch-up is that backlog, read as Discord's
  ;; read-back is.
  (let ((age (and (integerp sent) (- (unix-ms) sent))))
    (cond ((or (null age) (< age +late-ms+)) candidate)
          ((or (zerop adapter.catch-up-minutes)
               (> age (* 60000 adapter.catch-up-minutes)))
           (format *error-output* "~&telegram: message ~a in ~a was sent ~d minutes ago, past catch_up_minutes; not answered~%"
                   (source-field candidate "message_id") (source-field candidate "channel_id")
                   (round age 60000))
           nil)
          ((candidate-taken-p adapter.host candidate) nil)
          (t (setf (gethash "late_minutes" (candidate-source candidate))
                   (max 1 (round age 60000)))
             candidate))))

(defun telegram-author-roles (adapter chat-id user-id &aux (now (unix-ms)))
  "USER-ID's standing in the group CHAT-ID as role names -- (\"creator\"
\"administrator\") for its creator, (\"administrator\") for an admin, NIL for
anyone else -- off the group's admin list, read at most every five minutes."
  ;; One getChatAdministrators per group per five minutes, on the poll thread
  ;; and short: a failed read is tried again in a minute, and meanwhile nobody
  ;; holds a role there.
  (let ((entry (gethash chat-id adapter.admins)))
    (unless (and entry (> (car entry) now))
      (let* ((result (execute-plan (host-executor adapter.host)
                                   (chat-admins-plan chat-id :timeout-seconds 5)))
             (members (and (execution-ok-p result)
                           (nlk:json-value (execution-body result) :any "result"))))
        (setf entry (cons (+ now (if (vectorp members) 300000 60000))
                          (loop for member across (if (vectorp members) members #())
                                for status = (nlk:json-value member :string "status")
                                collect (cons (number-string (nlk:json-value member :any "user" "id"))
                                              (if (equal status "creator")
                                                  (list "creator" "administrator")
                                                  (list status)))))
              (gethash chat-id adapter.admins) entry)))
    (cdr (assoc user-id (cdr entry) :test #'equal))))

(defun telegram-handle-edit (adapter message)
  "An edited MESSAGE: the ask it became when its new words address the bot
-- its @handle, a wake word -- and it opened no lane before; NIL for every
other edit."
  ;; As Discord's MESSAGE_UPDATE: the room's own mention rule decides, so an
  ;; edit wakes the bot exactly when the same words typed fresh would have
  ;; been said to it, and an edit to an ask already taken asks nothing again.
  (nlk:when-let (candidate (telegram-arrival adapter message))
    (when (and (not (gethash "is_bot" (candidate-source candidate)))
               (plusp (length (nlk:trimmed (or (gethash "text" candidate) ""))))
               (mention-involves-p (host-policy adapter.host) (gethash "text" candidate))
               (not (candidate-taken-p adapter.host candidate)))
      (handle-candidate adapter.host candidate)
      candidate)))

(defun telegram-handle-reaction (adapter update)
  "A message_reaction UPDATE: each emoji added or taken back on one of the
bot's messages becomes a line of context for that conversation's next ask
(NCK:REACTION-NOTICED). => the candidates handed on."
  (let ((candidates (map-telegram-reaction update :bot-user-id adapter.bot-user-id)))
    (dolist (candidate candidates candidates)
      ;; Telegram's message_reaction names the chat and the message, never the
      ;; forum topic: the lane that posted the message knows where it sits, so
      ;; the line lands in that topic's room and its next ask reads it.
      (let* ((source (candidate-source candidate))
             (lane (lane-for-address
                    (host-lanes adapter.host)
                    (telegram-address (list :channel-id (gethash "channel_id" source))
                                      (gethash "reply_to_message_id" source))))
             (thread (and lane (getf (lane-target lane) :thread-id))))
        (when thread
          (setf (gethash "thread_id" source) thread)))
      (when (and adapter.roles-p (not (equal "direct_message" (source-field candidate "chat_kind"))))
        (setf (gethash "role_ids" (candidate-source candidate))
              (telegram-author-roles adapter (source-field candidate "channel_id")
                                     (source-field candidate "user_id"))))
      (reaction-noticed adapter.host candidate))))

(defun telegram-handle-callback (adapter callback)
  "One callback_query UPDATE: a press on an inline control a lane posted."
  ;; A press on a choice is the presser saying its line (MAP-TELEGRAM-PRESS),
  ;; admitted and answered as typed words are. Any other press is parsed into
  ;; the kit's normal payload and relayed -- the kit answers the query and
  ;; acts; this thread only reads the hash table. Returns the candidate or the
  ;; payload, or NIL for a press missing its sender, message or data.
  (let* ((data (gethash "data" callback))
         (line (said-line data))
         (candidate (and line (map-telegram-press callback line))))
    ;; The thread board's own buttons are the board's (board.lisp).
    (when (telegram-board-pressed adapter callback)
      (return-from telegram-handle-callback t))
    (when candidate
      (handle-candidate adapter.host candidate)
      (return-from telegram-handle-callback candidate))
    (when (and (nlk:json-value callback :object "from")
               (nlk:json-value callback :object "message" "chat") (stringp data))
      (let* ((chat (number-string (nlk:json-value callback :any "message" "chat" "id")))
             (thread (number-string (nlk:json-value callback :any "message" "message_thread_id")))
             (payload (list :id (number-string (gethash "id" callback))
                           :data data
                           :user-id (number-string (nlk:json-value callback :any "from" "id"))
                           :user-name (or (nlk:json-value callback :string "from" "username")
                                          (nlk:json-value callback :string "from" "first_name"))
                           :message-id (number-string
                                        (nlk:json-value callback :any "message" "message_id"))
                           :channel-id chat
                           ;; The topic the pressed message is in: a Close
                           ;; press closes it (NCK:CLOSE-PRESSED).
                           :thread-id thread)))
        (when thread (setf (gethash thread *topic-chats*) chat))
        (control-pressed adapter.host payload)
        payload))))

(defun telegram-poll-lap (adapter stop-p)
  "One getUpdates long poll."
  ;; Returns the supervisor verdict: seconds to wait before the next lap.
  (when (funcall stop-p) (return-from telegram-poll-lap :stop))
  (let ((result (execute-plan
                 (host-executor adapter.host)
                 (get-updates-plan adapter.offset
                                   :poll-timeout-seconds
                                   adapter.poll-timeout
                                   :allowed-updates
                                   adapter.allowed-updates)))
        (interval (max 0.025
                       (min 30 (/ adapter.poll-interval-ms
                                  1000.0)))))
    (when (funcall stop-p) (return-from telegram-poll-lap :stop))
    (cond
      ((not (execution-ok-p result))
       (set-channel-status "telegram" :connected nil
                           :detail result.error)
       ;; A 409 is a duplicate poller holding this token. Loud, persistent,
       ;; and slow — never a hot conflict loop.
       (let ((conflict-p (= 409 result.status)))
         (warn (if conflict-p "~a" "telegram getUpdates failed: ~a") result.error)
         (if conflict-p 5.0 (max interval 1.0))))
      (t
       (set-channel-status "telegram" :state :running :connected t
                           :detail nil)
       (let ((updates (nlk:json-value result.body :any "result")))
         (when (vectorp updates)
           ;; The offset moves first, over every update: one that wedges
           ;; its handler is not polled again.
           (setf adapter.offset (next-update-offset updates adapter.offset))
           (loop for update across updates
                 when (hash-table-p update)
                   ;; One boundary per update, outside the seam, so advice
                   ;; that signals costs that update and not the poll.
                   do (handler-case (telegram-handle-update adapter update)
                        (error (condition)
                          (warn "telegram update handling signalled: ~a"
                                condition))))))
       interval))))

;;; --- the channel entry ---------------------------------------------------------------------

(defun start-channel (section &key executor)
  "Start the Telegram lane from its channels.telegram config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides the live Bot API executor — the
  ;; scripted-executor test seam.
  ;;
  ;; Fail-closed: refuses unless at least one of allowed_chats / allowed_users
  ;; is populated. allowed_users says who may talk; owner says whose
  ;; instructions are standing policy (the kit's room, AUTHORITY) and is
  ;; warned about, not required, when a multi-person chat leaves it unset. An
  ;; owner may always talk: the ids join allowed_users when that list is the
  ;; gate.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "telegram")) section))
         (token (resolve-channel-secret section "token"))
         (configured-users (getf settings :allowed-users))
         (owners (resolve-owners "telegram" (getf settings :owner)
                                 configured-users "allowed_chats"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (bot-username (getf settings :bot-username))
         (bot-user-id nil)
         (api-base (config-string section "api_base" +telegram-api-base+))
         (request-timeout (config-integer section "request_timeout_seconds"
                                          60 :min 1)))
    (require-non-empty-allowlist "telegram"
                                 "allowed_chats" (getf settings :allowed-chats)
                                 "allowed_users" allowed-users
                                 "allowed_roles" (getf settings :allowed-roles))
    ;; The other-addressee names: a line that opens by naming one of these is
    ;; someone else's, observed rather than answered.
    (set-other-addressees "telegram" (getf settings :other-addressees))
    (let ((executor (or executor (make-telegram-executor
                                  :api-base api-base :token token))))
      ;; Mention policy needs the bot's @username; hydrate via getMe when
      ;; the config does not pin one. Loud on failure: with require_mention
      ;; every group message would be rejected until it is fixed.
      (when (and (getf settings :require-mention) (null bot-username))
        (let ((result (execute-plan executor (bot-plan "/getMe" (nlk:json-object)
                                                       request-timeout t "get_me"))))
          (if result.ok-p
              (setf bot-username (nlk:json-value result.body :string "result" "username")
                    bot-user-id (number-string
                                 (nlk:json-value result.body :any "result" "id")))
              (warn "telegram: could not hydrate bot username (~a); with ~
                     require_mention every group message will be rejected ~
                     until channels.telegram.bot_username is set"
                    result.error))))
      (let* ((host (make-host-from-section
                    section (telegram-platform :bot-username bot-username
                                               :bot-user-id bot-user-id)
                    ;; Every write the host makes is told to the thread
                    ;; board: the topics it opens and what it says in them.
                    :executor (board-noting-executor executor)
                    :owners owners
                    :soul-path (soul-path section)
                    :request-timeout 60
                    ;; The declared members name the policy's own keys; the
                    ;; rest are read here.
                    :policy (apply #'telegram-inbound-policy
                                   :allowed-users allowed-users
                                   :bot-username bot-username
                                   :bot-user-id bot-user-id
                                   :allow-private-chats (config-boolean
                                                         section "allow_private_chats" t)
                                   :allow-group-chats (config-boolean section "allow_group_chats" t)
                                   :allow-forum-topics (config-boolean
                                                        section "allow_forum_topics" t)
                                   settings)))
             (adapter
               (%make-telegram-adapter
                :host host
                ;; Short window by design: the parked getUpdates read is
                ;; also the lane's stop latency (see GET-UPDATES-PLAN).
                :poll-timeout (config-integer
                               section "get_updates_timeout_seconds" 2
                               :min 0)
                :poll-interval-ms (config-integer
                                   section "poll_interval_ms" 1000 :min 25)
                :allowed-updates (or (config-string-list section
                                                         "allowed_updates")
                                     +default-allowed-updates+)
                :catch-up-minutes (config-integer section "catch_up_minutes" 60 :min 0)
                :roles-p (and (getf settings :allowed-roles) t)
                :bot-username bot-username
                :bot-user-id bot-user-id))
             (stop-board nil))
        (run-host host "poll" (lambda (stop-p) (telegram-poll-lap adapter stop-p))
                  :on-start (lambda ()
                              (setf *telegram-adapter* adapter
                                    *telegram-file-root*
                                    (format nil "~a/file/bot~a"
                                            (string-right-trim "/" api-base) token))
                              (setf stop-board (start-telegram-board adapter section)))
                  :on-stop (lambda ()
                             (when stop-board
                               (ignore-errors (funcall stop-board))
                               (setf stop-board nil))
                             ;; A stale stop thunk racing a respawned lane must
                             ;; not clear the fresh adapter's binding.
                             (when (eq *telegram-adapter* adapter)
                               (setf *telegram-adapter* nil
                                     *telegram-file-root* nil))))))))
