;;;; gateway.lisp --- Discord Gateway websocket session reducer. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; 1:1 port of the Zig-era pack's gateway.ts: the irreducible protocol
;;;; state machine (hello/identify/resume/heartbeat/close) kept pure. All
;;;; I/O — websocket frames, REST, admission — lives in adapter.lisp.
;;;;
;;;; Payloads are decoded-wire hash tables (shasht). Actions are tagged
;;;; lists: (:send PAYLOAD) (:start-heartbeat INTERVAL-MS)
;;;; (:reconnect RESUME-P REASON) (:fatal REASON)
;;;; (:ready SESSION-ID APPLICATION-ID) (:dispatch PAYLOAD).

(in-package #:nodecode-channel-discord)

(defparameter +discord-gateway-url+
  "wss://gateway.discord.gg/?v=10&encoding=json")

(defparameter +op-dispatch+ 0)
(defparameter +op-heartbeat+ 1)
(defparameter +op-identify+ 2)
(defparameter +op-resume+ 6)
(defparameter +op-reconnect+ 7)
(defparameter +op-invalid-session+ 9)
(defparameter +op-hello+ 10)
(defparameter +op-heartbeat-ack+ 11)

(nlk:define-record (dgw-session (:copier %copy-dgw-session) (:export :constructor :readers))
  (connection "disconnected" :type string)
  (sequence nil :type (or null integer))
  (session-id nil :type (or null string))
  (resume-gateway-url nil :type (or null string))
  ;; READY's application.id: the id application commands are registered
  ;; under. Kept across resumes; a fresh identify re-reads it.
  (application-id nil :type (or null string))
  (last-heartbeat-ack t :type boolean))

(nlk:access (next dgw-session) (session dgw-session))

(defun initial-gateway-session ()
  (make-dgw-session))

(defmacro copy-session (session &rest overrides &aux (next (gensym "NEXT")))
  "Fresh DGW-SESSION with OVERRIDES (slot-keyword value ...) applied — the
reducer never mutates its input."
  ;; A macro because every call names its slots literally: the struct's own
  ;; copier plus one SETF per override says it without a hand-written field
  ;; list, which is the part that silently rots the next time a slot joins the
  ;; struct. The copier stays %-private so the reducer still has exactly one
  ;; way to derive a session.
  `(let ((,next (%copy-dgw-session ,session)))
     ,@(loop for (key value) on overrides by #'cddr
             for slot = (intern (format nil "DGW-SESSION-~a" key) :ncd)
             collect `(setf (,slot ,next) ,value))
     ,next))

(defun reset-session (session)
  (copy-session session :sequence nil :session-id nil
                        :resume-gateway-url nil))

(defun gateway-url-with-params (url &aux (uri (quri:uri url)))
  "URL with v=10 and encoding=json guaranteed in the query, existing
parameters preserved in order."
  (let ((params (quri:uri-query-params uri)))
    (loop for pair in '(("v" . "10") ("encoding" . "json"))
          unless (assoc (car pair) params :test #'equal)
            do (setf params (append params (list pair))))
    (setf (quri:uri-query-params uri) params)
    (quri:render-uri uri)))

(defun can-resume-p (session) (and session.session-id session.sequence t))

(defmacro define-payload (name lambda-list op &body d-specs)
  "Define NAME, of LAMBDA-LIST, as the gateway payload of opcode OP whose d
object D-SPECS build (NLK:JSON-OBJECT's specs)."
  `(defun ,name ,lambda-list
     (nlk:json-object "op" ,op "d" (nlk:json-object ,@d-specs))))

(define-payload identify-payload (token intents) +op-identify+
  "token" token
  "intents" intents
  "large_threshold" 50
  "properties" (nlk:json-object "os" "nodecode"
                                "browser" "nodecode"
                                "device" "nodecode"))

(define-payload resume-payload (token session) +op-resume+
  "token" token
  "session_id" (or session.session-id "")
  "seq" (or session.sequence 0))

(defun heartbeat-payload (session)
  ;; shasht writes NIL as false, so an absent sequence is written as an
  ;; explicit JSON null — the wire value Discord expects for "no sequence".
  (nlk:json-object "op" +op-heartbeat+
                   "d" (or session.sequence :null)))

(defparameter +heartbeat-ack-grace-ms+ 15000
  "How long a heartbeat may go unacknowledged before the connection is taken
for dead: Discord acknowledges within milliseconds.")

(defun heartbeat-tick (session elapsed-ms interval-ms)
  "(values SESSION ACTION) for the heartbeat ELAPSED-MS after the last one
was sent: a beat once INTERVAL-MS has passed since an acknowledged one, a
reconnect once the last one has gone unacknowledged past the grace — else
NIL."
  ;; The grace is what catches a socket that died without closing: waiting
  ;; for the next beat to find the ack missing took up to two intervals, 80 s
  ;; of a bot that looked connected and heard nothing.
  (cond ((not session.last-heartbeat-ack)
         (when (>= elapsed-ms (min interval-ms +heartbeat-ack-grace-ms+))
           (let ((next (copy-session session :connection "reconnecting")))
             (values next (list :reconnect (can-resume-p next)
                                "heartbeat_ack_timeout")))))
        ((>= elapsed-ms interval-ms)
         (let ((next (copy-session session :last-heartbeat-ack nil)))
           (values next (list :send (heartbeat-payload next)))))))

(defun payload-op (payload)
  (nlk:json-value payload :integer "op"))

(defun dispatch-data (payload event)
  "PAYLOAD's d object when PAYLOAD is a gateway dispatch of EVENT, else NIL."
  (and (eql (payload-op payload) +op-dispatch+)
       (equal (gethash "t" payload) event)
       (nlk:json-value payload :object "d")))

(defun reduce-gateway-payload (session payload token intents
                               &optional resume-requested)
  "(values SESSION ACTIONS) for one inbound gateway payload."
  (let* ((next (copy-session session
                             :sequence (or (nlk:json-value payload :integer "s") session.sequence)))
         (op (payload-op payload))
         (actions '()))
    (cond
      ((eql op +op-hello+)
       (let ((interval (or (nlk:json-value payload :integer "d" "heartbeat_interval")
                           45000))
             (resume (and resume-requested (can-resume-p next))))
         (setf next (copy-session next
                                  :connection (if resume
                                                  "resuming"
                                                  "identifying")
                                  :last-heartbeat-ack t))
         (push (list :start-heartbeat interval) actions)
         (push (list :send (if resume
                               (resume-payload token next)
                               (identify-payload token intents)))
               actions)))
      ((eql op +op-heartbeat-ack+)
       (setf next (copy-session next :last-heartbeat-ack t)))
      ((eql op +op-reconnect+)
       (setf next (copy-session next :connection "reconnecting"))
       (push (list :reconnect (can-resume-p next)
                   "discord_gateway_reconnect")
             actions))
      ((eql op +op-invalid-session+)
       (let ((resume (and (eq (gethash "d" payload) t)
                          (can-resume-p next))))
         (unless resume (setf next (reset-session next)))
         (setf next (copy-session next :connection "reconnecting"))
         (push (list :reconnect resume "discord_gateway_invalid_session")
               actions)))
      ((eql op +op-dispatch+)
       (let ((event-type (gethash "t" payload)))
         (when (equal event-type "READY")
           (nlk:when-let (session-id (nlk:json-value payload :string "d" "session_id"))
             (setf next (copy-session
                         next
                         :connection "ready"
                         :session-id session-id
                         :resume-gateway-url
                         (nlk:json-value payload :string "d" "resume_gateway_url")
                         :application-id
                         (or (nlk:json-value payload :string "d" "application" "id")
                             next.application-id)))
             (push (list :ready session-id
                         next.application-id)
                   actions)))
         (when (equal event-type "RESUMED")
           (setf next (copy-session next :connection "ready"))
           (push (list :resumed) actions))
         (push (list :dispatch payload) actions))))
    (values next (nreverse actions))))

(defun reduce-gateway-close (session code)
  "(values SESSION ACTION) for a websocket close."
  ;; Fatal codes (bad token, bad intents, ...) must stop the lane, never spin
  ;; a reconnect loop.
  (if (member code '(4004 4010 4011 4012 4013 4014))
      (values (copy-session (reset-session session) :connection "fatal")
              (list :fatal (format nil "discord_gateway_fatal_close_~a"
                                   code)))
      (let* ((resume (and (can-resume-p session)
                          (member code '(1000 1001 1006 1011 4000 4001))
                          t))
             (next (copy-session (if resume session (reset-session session))
                                 :connection "reconnecting")))
        (values next (list :reconnect resume
                           (format nil "discord_gateway_close_~a" code))))))

;;; --- ingress normalization -------------------------------------------------
;;; MESSAGE_CREATE and INTERACTION_CREATE -> the shared candidate shape
;;; admission evaluates. A slash interaction becomes the slash line it
;;; spells, addressed by construction, with its own return path in tow.

;;; --- thread knowledge ----------------------------------------------------------------
;;; A MESSAGE_CREATE inside a thread names the thread channel and nothing
;;; above it: Discord does not put the parent on the message, so the adapter
;;; learns the thread set from the events that do carry it — GUILD_CREATE's
;;; active threads, THREAD_CREATE / THREAD_UPDATE / THREAD_DELETE as they
;;; come, and a starter message's own thread field. Hash writes only; this
;;; runs on the connection lap.

;;; How a message inside a thread is told from one in a plain channel: the
;;; message names only the thread.
(defvar *known-threads* (make-hash-table :test #'equal)
  "Thread channel id -> parent channel id, kept from the gateway's own thread
dispatches.")

(defun note-thread-channel (thread &aux (id (nlk:json-value thread :string "id")))
  "THREAD — a thread channel object (a GUILD_CREATE threads entry, the d of
a THREAD_CREATE or THREAD_UPDATE, a starter message's thread) — into
*KNOWN-THREADS* under its id, with its parent."
  ;; Answers the id, or NIL for anything that is not one.
  (when id
    (setf (gethash id *known-threads*)
          (or (nlk:json-value thread :string "parent_id")
              (gethash id *known-threads*))))
  id)

(defun thread-channel-parent (id)
  "The parent channel of thread channel ID, or NIL when the adapter has not
heard of the thread."
  (and id (gethash id *known-threads*)))

(defvar *channel-topics* (make-hash-table :test #'equal)
  "Channel id -> the topic its admins set, kept from the gateway's own
channel dispatches.")

(defun note-channel-topic (channel &aux (id (nlk:json-value channel :string "id")))
  "CHANNEL — a GUILD_CREATE channels entry, the d of a CHANNEL_CREATE or
CHANNEL_UPDATE — into *CHANNEL-TOPICS*: its topic, or none."
  (when id
    (let ((topic (nlk:json-value channel :string "topic")))
      (if (and topic (plusp (length (nlk:trimmed topic))))
          (setf (gethash id *channel-topics*) topic)
          (remhash id *channel-topics*)))))

(defun channel-topic (id)
  "The topic of channel ID, or NIL when it has none the adapter has heard."
  (and id (gethash id *channel-topics*)))

(defun note-channel-dispatches (payload)
  "Keep *KNOWN-THREADS* and *CHANNEL-TOPICS* in step with PAYLOAD, any
gateway dispatch the connection delivers: GUILD_CREATE seeds the active
threads and the channels' topics, THREAD_CREATE and THREAD_UPDATE note a
thread, CHANNEL_CREATE and CHANNEL_UPDATE a topic, THREAD_DELETE and
CHANNEL_DELETE forget theirs."
  ;; Answers T for an event it kept. Runs on the connection lap thread: hash
  ;; writes, no I/O.
  (when (eql (payload-op payload) +op-dispatch+)
    (let ((name (gethash "t" payload))
          (data (gethash "d" payload)))
      (flet ((one-of (&rest names) (member name names :test #'equal))
             (forget (table)
               (nlk:when-let (id (nlk:json-value data :string "id"))
                 (remhash id table))))
        (cond ((one-of "GUILD_CREATE")
               (loop for thread across (nlk:json-array data "threads")
                     do (note-thread-channel thread))
               (loop for channel across (nlk:json-array data "channels")
                     do (note-channel-topic channel)))
              ((one-of "THREAD_CREATE" "THREAD_UPDATE") (note-thread-channel data))
              ((one-of "CHANNEL_CREATE" "CHANNEL_UPDATE") (note-channel-topic data))
              ((one-of "THREAD_DELETE") (forget *known-threads*))
              ((one-of "CHANNEL_DELETE") (forget *channel-topics*))
              (t (return-from note-channel-dispatches nil)))
        t))))

;;; --- self knowledge ----------------------------------------------------------
;;; What addresses the bot. A Discord mention is an entity reference, and the
;;; bot is two entities: its user, and the managed role Discord made for it —
;;; same name, offered beside the user in every picker. A room that picks the
;;; role has addressed the bot, so the role ids have to be known before the
;;; mention gate can answer. HYDRATE-SELF-ROLES (rest.lisp) reads them off the
;;; bot's own member object in each guild it is in, and the mention rule reads
;;; them here at decision time, so learning them late still admits the next
;;; ask. Hash writes only; safe on any thread.

(defvar *self-role-ids* (make-hash-table :test #'equal)
  "Role id -> T: the roles the connection's own user holds.")

(defun discord-self-role-ids ()
  "The roles the bot itself holds, as a list — what makes <@&id> a mention."
  (loop for id being the hash-keys of *self-role-ids* collect id))

(defun note-self-roles (role-ids)
  "Remember ROLE-IDS as roles that address the bot. Answers the whole set."
  (dolist (id role-ids)
    (when (and (stringp id) (plusp (length id)))
      (setf (gethash id *self-role-ids*) t)))
  (discord-self-role-ids))

(defparameter +discord-file-hosts+ '("cdn.discordapp.com" "media.discordapp.net")
  "The hosts Discord serves a message's files from.")

(defvar *file-hosts* +discord-file-hosts+
  "The hosts an attachment is fetched from: Discord's own, and the configured
api_base's, set at each start.")

(defun file-host-p (url &aux (host (ignore-errors (quri:uri-host (quri:uri url)))))
  "Whether URL names one of *FILE-HOSTS*."
  (and host (member host *file-hosts* :test #'string-equal) t))

(defun discord-attachments (message)
  "MESSAGE's attachments as the candidate's attachments array: one entry
each with the url to fetch, the declared type, filename and size, the
attachment's own id — the same file seen twice, in a message and in a reply
to it, is one file — and the seconds a voice message declares."
  ;; NIL when it carries none. Every file rides: the kit reads what it can in
  ;; and saves the rest for the lane's tools (NCK::READ-ATTACHMENT). A url on
  ;; a host that is not Discord's is not fetched: the bot's own fetch would
  ;; otherwise reach whatever address a payload named, this machine's own
  ;; included (Hermes checks each one the same way).
  (let ((attachments (nlk:json-value message :array "attachments")))
    (and attachments (plusp (length attachments))
         (coerce (loop for attachment across attachments
                       for type = (nlk:json-value attachment :string "content_type")
                       for url = (nlk:json-value attachment :string "url")
                       when (and url (not (file-host-p url)))
                         do (warn "discord: an attachment at ~a is not fetched: not a Discord host"
                                  (ignore-errors (quri:uri-host (quri:uri url))))
                       when (and url (file-host-p url))
                         collect (nlk:json-object
                                  "id" (nlk:json-value attachment :string "id")
                                  "url" url
                                  "media_type" type
                                  "filename" (or (nlk:json-value attachment :string "filename")
                                                 "")
                                  "size" (or (nlk:json-value attachment :integer "size") 0)
                                  "seconds" (nlk:json-value attachment :number "duration_secs")))
                 'vector))))

(defun discord-message-candidate (data bot-user-id)
  "DATA — the d object of a MESSAGE_CREATE or a MESSAGE_UPDATE dispatch —
as the one candidate shape admission reads: the words, the attachments, and the
source coordinates, the reply gesture included."
  ;; NIL for an object with no channel to live in.
  (let* ((channel-id (nlk:json-value data :string "channel_id"))
         (parent (or (nlk:json-value data :string "parent_id")
                     (nlk:json-value data :string "channel"
                                     "parent_id")
                     (thread-channel-parent channel-id)))
         (thread (or (nlk:json-value data :string "thread_id")
                     (and parent channel-id)))
         (guild (nlk:json-value data :string "guild_id"))
         (ref (nlk:json-value data :object "referenced_message"))
         ;; A forward points at its original, in another channel, and
         ;; carries it whole as a snapshot: what was forwarded is what the
         ;; person said, and the pointer is no reply gesture.
         (forwarded (and (eql 1 (nlk:json-value data :integer "message_reference" "type"))
                         (let ((snapshots (nlk:json-array data "message_snapshots")))
                           (and (plusp (length snapshots))
                                (nlk:json-value (aref snapshots 0) :object "message")))))
         (own (or (nlk:json-value data :string "content") "")))
    (note-thread-channel (nlk:json-value data :any "thread"))
    (when channel-id
      (let* ((chat-kind (cond (thread "thread")
                              (guild "channel")
                              (t "direct_message")))
             (active (if thread thread channel-id))
             (parent-channel (when thread (or parent channel-id))))
        (nlk:json-object
         "text" (if forwarded
                    (format nil "~@[~a~%~][forwarded message] ~a"
                            (and (plusp (length own)) own)
                            (or (nlk:json-value forwarded :string "content") ""))
                    own)
         "attachments" (let ((own (discord-attachments data))
                             (theirs (and forwarded (discord-attachments forwarded))))
                         (if theirs (concatenate 'vector own theirs) own))
         "reply" (and ref (nlk:json-object
                           "id" (or (nlk:json-value ref :string "id") "")
                           "user_id" (nlk:json-value ref :string "author" "id")
                           "user_name" (nlk:json-value ref :string "author" "username")
                           "text" (or (nlk:json-value ref :string "content") "")
                           ;; A reply to a voice note is about what it says.
                           "attachments" (discord-attachments ref)))
         "source"
         (nlk:json-object
          "platform" "discord"
          "chat_kind" chat-kind
          "workspace_id" guild
          "channel_id" active
          "parent_channel_id" parent-channel
          "thread_id" (and thread active)
          "user_id" (nlk:json-value data :string "author" "id")
          "user_name" (nlk:json-value data :string "author"
                                      "username")
          "message_id" (nlk:json-value data :string "id")
          ;; The reply gesture. Discord's own pointer to another
          ;; message is the whole branch structure of a flat channel:
          ;; a reply to anything a lane produced continues that lane.
          "reply_to_message_id"
          (and (not forwarded) (nlk:json-value data :string "message_reference" "message_id"))
          "addressed" (and bot-user-id
                           (equal bot-user-id
                                  (nlk:json-value data :string
                                                  "referenced_message"
                                                  "author" "id"))
                           t)
          "is_bot" (and (nlk:json-value data :any "author" "bot") t)
          ;; The roles the author holds in this guild, which allowed_roles
          ;; reads; a direct message carries none.
          "role_ids" (nlk:json-value data :array "member" "roles")))))))

(defun route-discord-message (payload &key bot-user-id)
  "The normalized candidate for a MESSAGE_CREATE dispatch, or NIL for every
other payload."
  ;; A reply to one of the bot's own messages — referenced_message authored by
  ;; BOT-USER-ID — is `addressed': the platform routed it to us, so the
  ;; mention gate has nothing to ask (the section's promise that a reply
  ;; counts as a mention).
  (discord-message-candidate (dispatch-data payload "MESSAGE_CREATE") bot-user-id))

(defun route-discord-message-update (payload &key bot-user-id involves-p)
  "The normalized candidate for a MESSAGE_UPDATE dispatch whose edit makes
the message something said to us, or NIL for every other update."
  ;; An edit wakes the bot exactly when its new words involve it: INVOLVES-P
  ;; over the edited text, which the adapter builds from the room's own
  ;; mention rule — so an entity mention and a configured wake word wake it
  ;; here exactly as they do on admission. Everything else an update can be is
  ;; not an ask: the lane editing its own status lines all turn long, another
  ;; bot's message, a partial update that carries no text at all (a pin, an
  ;; embed refresh, a reaction summary), an update that does not even name its
  ;; author, and an edit that leaves a message as unaddressed as it already
  ;; was. Nor is an update nobody typed: Discord restates the whole message,
  ;; mention and all, when a thread is opened on it or deleted from it, when
  ;; it is pinned or its embed resolves, and only a typed edit stamps
  ;; edited_timestamp. Without the stamp, deleting an old ask's thread after
  ;; its lane was reaped read as that ask arriving again: a second admission
  ;; under the first one's command id, which the engine dedupes, so no turn
  ;; ran and its card said "Waiting on the provider" for five hours.
  (let* ((data (dispatch-data payload "MESSAGE_UPDATE"))
         (content (nlk:json-value data :string "content")))
    (when (and (nlk:json-value data :string "edited_timestamp")
               (nlk:json-value data :object "author")
               (not (nlk:json-value data :any "author" "bot"))
               (stringp content)
               (plusp (length (nlk:trimmed content)))
               involves-p
               (funcall involves-p content))
      (discord-message-candidate data bot-user-id))))

(defun press-data (data)
  "What a component press in the INTERACTION_CREATE object DATA carries back:
the picked option's value for a select, the custom_id for a button."
  (if (eql (nlk:json-value data :integer "data" "component_type") 3) ; STRING_SELECT
      (let ((values (nlk:json-value data :array "data" "values")))
        (and (plusp (length values)) (nlk:json-value (aref values 0) :text)))
      (nlk:json-value data :text "data" "custom_id")))

(defun interaction-thread-parent (data &aux (channel-id (nlk:json-value data :string "channel_id")))
  "The parent channel of the thread an interaction's DATA comes from, or NIL
outside a thread: a thread the adapter has heard of says so, and so does the
payload's own channel object (types 10 to 12)."
  (or (thread-channel-parent channel-id)
      (and (member (nlk:json-value data :integer "channel" "type") '(10 11 12))
           (nlk:json-value data :string "channel" "parent_id"))))

(defun route-discord-interaction (payload &aux (data (dispatch-data payload "INTERACTION_CREATE")))
  "The normalized candidate for an INTERACTION_CREATE dispatch that is an
application command — its text the slash line the command spells, `/name'
plus the `args' option, so it runs through the host exactly as a typed
`/name args' would — or a press on a choice a message carries (SAID-LINE)
— its text the line the choice says, a reply to the message pressed."
  ;; Addressed by construction: the person picked this bot's command, or
  ;; pressed something one of its messages carries. The source carries
  ;; interaction_id, interaction_token and application_id, the return path the
  ;; platform's PLAN-RESPOND answers through; there is no message id, an
  ;; interaction is not a message. A press also carries `pressed', which holds
  ;; its answer as an edit of the message pressed, `card_text', that
  ;; message's words, and `card_flags', its flags: an answer to a press on a
  ;; message of components is one too (INTERACTION-RESPONSE-PLAN). A command
  ;; typed inside a thread is placed exactly as a message typed there is — the thread, its parent beside it — so it acts on
  ;; the thread's room: /stop there finds the thread's turn, and a parent the
  ;; allowlist names admits it. The payload's own channel object says it is a
  ;; thread (types 10 to 12) and names the parent; a thread the adapter has
  ;; heard of already says so too. NIL for every other payload.
  (let* ((kind (nlk:json-value data :integer "type"))
         (pressed (and (eql kind 3) (said-line (press-data data)))) ; MESSAGE_COMPONENT
         (name (and (eql kind 2) (nlk:json-value data :text "data" "name"))) ; APPLICATION_COMMAND
         (args (loop for option across (nlk:json-array data "data" "options")
                     when (equal "args" (nlk:json-value option :string "name"))
                       return (nlk:json-value option :string "value")))
         (channel-id (nlk:json-value data :string "channel_id"))
         (parent (interaction-thread-parent data))
         (guild (nlk:json-value data :string "guild_id"))
         (user (or (nlk:json-value data :object "member" "user")
                   (nlk:json-value data :object "user"))))
    (when (and (or name pressed) channel-id)
      (nlk:json-object
       "text" (or pressed
                  (format nil "/~a~@[ ~a~]" name (and args (plusp (length args)) args)))
       "source"
       (nlk:json-object
        "platform" "discord"
        "chat_kind" (cond (parent "thread") (guild "channel") (t "direct_message"))
        "workspace_id" guild
        "channel_id" channel-id
        "parent_channel_id" parent
        "thread_id" (and parent channel-id)
        "user_id" (nlk:json-value user :string "id")
        "user_name" (nlk:json-value user :string "username")
        "interaction_id" (nlk:json-value data :string "id")
        "interaction_token" (nlk:json-value data :string "token")
        "application_id" (nlk:json-value data :string "application_id")
        :when pressed "pressed" t
        ;; what the card the press's answer draws opens on (NCK:CANDIDATE-PRESS)
        :when pressed "press_data" (press-data data)
        :when pressed "reply_to_message_id" (nlk:json-value data :string "message" "id")
        :when pressed "card_text" (nlk:json-value data :string "message" "content")
        :when pressed "card_flags" (nlk:json-value data :integer "message" "flags")
        "addressed" t
        "is_bot" (and (nlk:json-value user :any "bot") t)
        "role_ids" (nlk:json-value data :array "member" "roles"))))))

(defun route-discord-control (payload)
  "The kit's normal control press for an INTERACTION_CREATE dispatch that
is a press on a message's component, or NIL for every other payload — a
slash command is not a press, and neither is a message."
  ;; A press carries what its ack and the act both need: the data the button
  ;; or the menu's option sent, the person who pressed and their name, the message and
  ;; channel it sits on, and the interaction's id, token and application, the
  ;; return path the platform's ack answers through -- the application the
  ;; one its follow-up messages post under. A press on a choice routes as the
  ;; line it says (ROUTE-DISCORD-INTERACTION), which is tried first.
  (let* ((data (dispatch-data payload "INTERACTION_CREATE"))
         (pressed (press-data data))
         (user (or (nlk:json-value data :object "member" "user")
                   (nlk:json-value data :object "user"))))
    (when (and (eql (nlk:json-value data :integer "type") 3) ; MESSAGE_COMPONENT: a press
               pressed)
      (list :id (nlk:json-value data :string "id")
            :token (nlk:json-value data :string "token")
            :application-id (nlk:json-value data :string "application_id")
            :data pressed
            :user-id (nlk:json-value user :string "id")
            ;; The name a card says stopped it: the one the room reads.
            :user-name (or (nlk:json-value user :string "global_name")
                           (nlk:json-value user :string "username"))
            :message-id (nlk:json-value data :string "message" "id")
            :channel-id (nlk:json-value data :string "channel_id")))))

(defun reaction-emoji-text (data &aux (name (nlk:json-value data :string "emoji" "name"))
                                      (id (nlk:json-value data :string "emoji" "id")))
  "The emoji a reaction dispatch carries, as the room reads it: the literal
character for a unicode reaction, and `:name:' for one of a guild's own
custom emoji — which is how Discord writes a custom emoji everywhere a bot
cannot render the image. NIL when the payload names no emoji at all."
  (if id (format nil ":~a:" (or name id)) name))

(defun route-discord-reaction (payload &key bot-user-id)
  "The normalized candidate for a MESSAGE_REACTION_ADD or a
MESSAGE_REACTION_REMOVE dispatch — a line the room said without typing one —
or NIL for every other payload."
  ;; The candidate carries the emoji as its text and the message it was left
  ;; on as its reply gesture, because that is what a reaction is: a wordless
  ;; reply. It has no message id of its own, and the bracket the model reads
  ;; says so. `addressed' is the same fact a reply carries — the reacted
  ;; message is one of ours (message_author_id, which only the ADD dispatch
  ;; ships) — so a reaction on our own message has nothing left for the
  ;; mention gate to ask.
  ;;
  ;; NIL for the bot's own reaction, whatever it is: the kit's eye lifecycle
  ;; adds and removes reactions all turn long, and a route that read those back
  ;; would answer itself. NIL for any other bot's too, where the guild tells us
  ;; (a direct message ships no member object, and the kit's own policy rejects a
  ;; bot author in any case). Hash reads only — the thread that runs this is the
  ;; one that heartbeats.
  (let* ((added (dispatch-data payload "MESSAGE_REACTION_ADD"))
         (data (or added (dispatch-data payload "MESSAGE_REACTION_REMOVE")))
         (user-id (nlk:json-value data :string "user_id"))
         (message-id (nlk:json-value data :string "message_id"))
         (channel-id (nlk:json-value data :string "channel_id"))
         (emoji (and data (reaction-emoji-text data)))
         (bot-p (and (nlk:json-value data :any "member" "user" "bot") t)))
    (when (and user-id message-id channel-id emoji
               (not (equal user-id bot-user-id))
               (not bot-p))
      (let* ((parent (thread-channel-parent channel-id))
             (thread (and parent channel-id))
             (guild (nlk:json-value data :string "guild_id")))
        (nlk:json-object
         "text" emoji
         "source"
         (nlk:json-object
          "platform" "discord"
          "chat_kind" (cond (thread "thread") (guild "channel") (t "direct_message"))
          "workspace_id" guild
          "channel_id" channel-id
          "parent_channel_id" (and thread parent)
          "thread_id" thread
          "user_id" user-id
          "user_name" (nlk:json-value data :string "member" "user" "username")
          ;; The gesture points at the message it sits on, and at nothing
          ;; else: a reaction is not a message and has no id of its own.
          "reply_to_message_id" message-id
          "reaction" (if added "add" "remove")
          "addressed" (and bot-user-id
                           (equal bot-user-id
                                  (nlk:json-value data :string "message_author_id"))
                           t)
          "is_bot" bot-p
          "role_ids" (nlk:json-value data :array "member" "roles")))))))

(defun route-discord-autocomplete (payload)
  "The kit's normal completion request for an INTERACTION_CREATE dispatch
that is a slash command's on-the-fly argument completion, or NIL for every
other payload: the command's name, the typed tail of the option the person
focused, who is typing, where — the channel, and the thread inside it, as a
command typed there is placed — and the interaction's id and token, the
return path the one answer goes through."
  (let* ((data (dispatch-data payload "INTERACTION_CREATE"))
         (channel (nlk:json-value data :string "channel_id"))
         (parent (interaction-thread-parent data))
         (user (or (nlk:json-value data :object "member" "user")
                   (nlk:json-value data :object "user"))))
    (when (eql (nlk:json-value data :integer "type") 4) ; APPLICATION_COMMAND_AUTOCOMPLETE
      (let ((focused (loop for option across (nlk:json-array data "data" "options")
                           when (nlk:json-value option :any "focused")
                             return option)))
        (list :id (nlk:json-value data :string "id")
              :token (nlk:json-value data :string "token")
              :command (nlk:json-value data :text "data" "name")
              :text (and focused (nlk:json-value focused :string "value"))
              :user-id (nlk:json-value user :string "id")
              :channel-id (or parent channel)
              :thread-id (and parent channel))))))

(defun discord-inbound-policy (&key allowed-guilds allowed-channels
                                    allowed-users allowed-roles dm-policy group-policy
                                    require-mention free-response-channels
                                    require-mention-channels ignored-channels
                                    mention-patterns
                                    allow-bots
                                    thread-behavior
                                    bot-user-id &allow-other-keys)
  "The kit policy carrying the Zig pack's Discord reason vocabulary."
  (make-inbound-policy
   :allowed-scopes allowed-guilds
   :allowed-channels allowed-channels
   :allowed-users allowed-users
   :allowed-roles allowed-roles
   :dm-policy (if (equal dm-policy "disabled") :disabled :enabled)
   :group-policy (if (equal group-policy "disabled") :disabled :enabled)
   :thread-policy (if (eq thread-behavior :disabled) :disabled :enabled)
   :require-mention require-mention
   :free-response-channels free-response-channels
   :require-mention-channels require-mention-channels
   :ignored-channels ignored-channels
   :mention-target bot-user-id
   ;; An entity mention of the bot's user or a role it holds, else a wake word.
   :mention-test (lambda (text target)
                   (or (discord-mention-p text target (discord-self-role-ids))
                       (some (lambda (pattern)
                               (and (stringp pattern)
                                    (plusp (length pattern))
                                    (search pattern (or text "") :test #'char-equal)))
                             mention-patterns)))
   :allow-bots (cond ((equal allow-bots "all") :all)
                     ((equal allow-bots "mentions") :mentions)
                     (t :none))
   :self-id bot-user-id
   :channel-match :discord
   :reasons '(:scope-not-allowed "guild_not_allowed"
              :thread-not-supported "threads_disabled"
              :mention-required-without-user "mention_required_without_bot_user")))
