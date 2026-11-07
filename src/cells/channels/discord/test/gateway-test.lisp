;;;; gateway-test.lisp --- table-driven tests over the pure Discord reducer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Mirrored from the Zig-era pack's gateway.ts semantics: HELLO drives
;;;; identify vs resume, a missed ack reconnects, INVALID_SESSION honors its
;;;; d flag, both close tables hold, READY captures resume identity, and
;;;; sequence numbers track.

(in-package #:nodecode.test)

(nlk:access (next ncd::dgw-session))

(defun dgw-reduce (session json &key (token "t") (intents 1) resume)
  "REDUCE-GATEWAY-PAYLOAD over one frame written as a JSON literal."
  ;; TOKEN and INTENTS reach only the identify payload, so every row that is
  ;; not about identifying leaves them at their defaults.
  (ncd:reduce-gateway-payload session (cell-json json) token intents
                              resume))

(defun dgw-dispatch (event d-json)
  "The gateway dispatch of EVENT whose d object is D-JSON — the envelope
around it is the same every time."
  (cell-json (format nil "{\"op\": 0, \"t\": \"~a\", \"d\": ~a}" event d-json)))

(defun dgw-message (d-json &rest keys)
  "The candidate ROUTE-DISCORD-MESSAGE derives from the MESSAGE_CREATE
dispatch whose d object is D-JSON. KEYS reach the router as they are."
  (apply #'ncd:route-discord-message (dgw-dispatch "MESSAGE_CREATE" d-json) keys))

(defun dgw-d (id content &key (channel "c1") (guild "g1") (author "u1") user-name bot (fields ""))
  "The d object of message ID saying CONTENT in GUILD's CHANNEL from user AUTHOR, as JSON:
USER-NAME the author's username, BOT the author's bot flag as JSON text, and FIELDS the object's
other members, each followed by a comma. A NIL GUILD is a direct message, a NIL CONTENT none said."
  (format nil "{~a\"id\": ~s, \"channel_id\": ~s, ~@[\"guild_id\": ~s, ~]~@[\"content\": ~s, ~]~
               \"author\": {\"id\": ~s~@[, \"username\": ~s~]~@[, \"bot\": ~a~]}}"
          fields id channel guild content author user-name bot))

(defun dgw-said (id content &rest keys &key bot-user-id &allow-other-keys)
  "DGW-MESSAGE over the DGW-D of message ID saying CONTENT under the rest of KEYS; BOT-USER-ID
reaches the router."
  (dgw-message (apply #'dgw-d id content :allow-other-keys t keys) :bot-user-id bot-user-id))

(defun dgw-ready-session ()
  "A session that has identified: resumable."
  (ncd:make-dgw-session :connection "ready" :sequence 42
                        :session-id "sess-1"
                        :resume-gateway-url "wss://resume.example/"))

(deftest channel-discord-hello-identifies (multiple-value-bind (session actions))
  (dgw-reduce (ncd:initial-gateway-session)
              "{\"op\": 10, \"d\": {\"heartbeat_interval\": 41250}}"
              :token "tok-abc" :intents 46593)
(is (equal "identifying" (ncd:dgw-session-connection session)))
(is-shape actions (length = 2) (first '(:start-heartbeat 41250)))
(is (eq :send (first (second actions))))
(is-shape (second (second actions)) ("op" = 2 "HELLO without resume identifies")
  ((:string "d" "token") "tok-abc") ((:integer "d" "intents") = 46593)
  ((:integer "d" "large_threshold") = 50) ((:string "d" "properties" "os") "nodecode")))

(deftest channel-discord-hello-resumes-when-requested ()
  (multiple-value-bind (session actions)
      (dgw-reduce (dgw-ready-session)
                  "{\"op\": 10, \"d\": {\"heartbeat_interval\": 1000}}"
                  :token "tok-abc" :resume t)
    (is (equal "resuming" (ncd:dgw-session-connection session)))
    (let ((payload (second (second actions))))
      (is-shape payload ("op" = 6 "resumable session resumes") ((:string "d" "session_id") "sess-1")
        ((:integer "d" "seq") = 42))))
  ;; Resume requested but nothing to resume: identify.
  (multiple-value-bind (session actions)
      (dgw-reduce (ncd:initial-gateway-session)
                  "{\"op\": 10, \"d\": {\"heartbeat_interval\": 1000}}"
                  :token "tok-abc" :resume t)
    (is (equal "identifying" (ncd:dgw-session-connection session)))
    (is (= 2 (gethash "op" (second (second actions)))))))

(deftest channel-discord-heartbeat-tick ()
  ;; An acked session heartbeats once the interval is up, and arms the ack
  ;; flag; before it, nothing is due.
  (is (null (ncd:heartbeat-tick (dgw-ready-session) 40000 41250)))
  (is-values (session action) (ncd:heartbeat-tick (dgw-ready-session) 41250 41250)
    ((ncd:dgw-session-last-heartbeat-ack session) not) ((first action) eq :send)
    ((gethash "op" (second action)) = 1) ((gethash "d" (second action)) = 42))
  ;; An ack that has not come 15 s after the beat is a dead socket: it
  ;; reconnects with resume then, not a whole interval later.
  (let ((unacked (ncd:make-dgw-session :sequence 42 :session-id "sess-1"
                                       :last-heartbeat-ack nil)))
    (is (null (ncd:heartbeat-tick unacked 14000 41250)))
    (is-values (session action) (ncd:heartbeat-tick unacked 15000 41250)
      ((ncd:dgw-session-connection session) "reconnecting")
      (action '(:reconnect t "heartbeat_ack_timeout"))))
  ;; Ack restores the flag.
  (is-values (session actions)
      (dgw-reduce (ncd:make-dgw-session :last-heartbeat-ack nil) "{\"op\": 11}")
    (actions null) ((ncd:dgw-session-last-heartbeat-ack session) is)))

(deftest channel-discord-reconnect-and-invalid-session ()
  (is-values (session actions) (dgw-reduce (dgw-ready-session) "{\"op\": 7}")
    ((ncd:dgw-session-connection session) "reconnecting")
    (actions '((:reconnect t "discord_gateway_reconnect"))))
  ;; INVALID_SESSION d:true keeps resume identity.
  (is-values (session actions) (dgw-reduce (dgw-ready-session) "{\"op\": 9, \"d\": true}")
    ((ncd:dgw-session-session-id session) "sess-1")
    (actions '((:reconnect t "discord_gateway_invalid_session"))))
  ;; INVALID_SESSION d:false resets it.
  (is-values (session actions) (dgw-reduce (dgw-ready-session) "{\"op\": 9, \"d\": false}")
    ((ncd:dgw-session-session-id session) null) ((ncd:dgw-session-sequence session) null)
    (actions '((:reconnect nil "discord_gateway_invalid_session")))))

(deftest channel-discord-ready-captures-resume-identity (multiple-value-bind (session actions))
  (dgw-reduce (ncd:initial-gateway-session)
              "{\"op\": 0, \"t\": \"READY\", \"s\": 1,
                    \"d\": {\"session_id\": \"sess-9\",
                            \"resume_gateway_url\":
                            \"wss://resume.example/\"}}")
(is-shape session (ncd:dgw-session-connection "ready") (ncd:dgw-session-session-id "sess-9")
  (ncd:dgw-session-resume-gateway-url "wss://resume.example/")
  (ncd:dgw-session-sequence = 1 "sequence tracks from s"))
(is (equal '(:ready "sess-9" nil) (first actions)))
(is (eq :dispatch (first (second actions)))))

(deftest channel-discord-resumed-is-connected-again ()
  ;; A resume is connected again: the lap sets /channels back to connected,
  ;; where it read "not connected" until the next fresh READY.
  (is-values (session actions)
      (dgw-reduce (ncd:make-dgw-session :connection "resuming" :sequence 7 :session-id "sess-1")
                  "{\"op\": 0, \"t\": \"RESUMED\", \"s\": 8, \"d\": {}}")
    ((ncd:dgw-session-connection session) "ready")
    ((mapcar #'first actions) '(:resumed :dispatch))))

(deftest channel-discord-ready-names-the-application (multiple-value-bind (session actions))
  ;; READY's application.id is what application commands register under;
  ;; the session keeps it, the :ready action carries it to the lap, and a
  ;; later READY without one keeps the id already known.
  (dgw-reduce (ncd:initial-gateway-session)
              "{\"op\": 0, \"t\": \"READY\", \"s\": 1,
                    \"d\": {\"session_id\": \"sess-9\",
                            \"application\": {\"id\": \"app1\",
                                              \"flags\": 0}}}")
(is (equal "app1" (ncd:dgw-session-application-id session)))
(is (equal '(:ready "sess-9" "app1") (first actions)))
(is-values (next actions)
    (dgw-reduce session "{\"op\": 0, \"t\": \"READY\", \"s\": 2,
                              \"d\": {\"session_id\": \"sess-10\"}}")
  (next.application-id "app1") ((first actions) '(:ready "sess-10" "app1"))))

(deftest channel-discord-sequence-tracking (let ((session (ncd:make-dgw-session :sequence 5))))
  ;; s: null keeps the tracked sequence; s: N advances it.
  (is (= 5 (ncd:dgw-session-sequence
            (dgw-reduce session "{\"op\": 0, \"t\": \"X\", \"s\": null,
                              \"d\": {}}"))))
  (is (= 6 (ncd:dgw-session-sequence
            (dgw-reduce session "{\"op\": 0, \"t\": \"X\", \"s\": 6,
                              \"d\": {}}")))))

(deftest channel-discord-close-tables ()
  ;; 4014 with MESSAGE_CONTENT asked for is the one fatal close a lane
  ;; survives: it identifies again without that intent and answers mentions,
  ;; replies and DMs — the words Discord still sends it. Already without it,
  ;; the close is another privileged intent, and stays fatal.
  (let ((all (logior (ash 1 0) ncd::+discord-message-content-intent+)))
    (is (eql (ash 1 0) (ncd::intents-without-refused 4014 all)))
    (is (null (ncd::intents-without-refused 4014 (ash 1 0))))
    (is (null (ncd::intents-without-refused 4004 all))))
  ;; websocket-driver reports a peer's close with no code — it rode only the
  ;; frame the driver echoed — so the socket keeps the one it echoed.
  (let ((socket (ncd::make-discord-socket "ws://127.0.0.1:1/")))
    (is (eql 1000 (ncd::socket-closed-code socket nil 1000)))
    (setf (ncd::socket-close-code socket) 4014)
    (is (eql 4014 (ncd::socket-closed-code socket nil 1000)))
    (is (eql 4004 (ncd::socket-closed-code socket 4004 1000))))
  (dolist (code '(4004 4010 4011 4012 4013 4014))
    (is-values (session action) (ncd:reduce-gateway-close (dgw-ready-session) code)
      ((ncd:dgw-session-connection session) "fatal" (format nil "close ~a is fatal" code))
      ((first action) eq :fatal) ((search (format nil "~a" code) (second action)) is)))
  (dolist (code '(1000 1001 1006 1011 4000 4001))
    (is-values (session action) (ncd:reduce-gateway-close (dgw-ready-session) code)
      ((ncd:dgw-session-connection session) "reconnecting")
      (action (list :reconnect t (format nil "discord_gateway_close_~a" code))
              (format nil "close ~a resumes" code))))
  ;; A code in neither table reconnects fresh (no resume).
  (is-values (session action) (ncd:reduce-gateway-close (dgw-ready-session) 4005)
    ((ncd:dgw-session-session-id session) null)
    (action '(:reconnect nil "discord_gateway_close_4005")))
  ;; A resumable code without resume identity cannot resume.
  (is (equal '(:reconnect nil "discord_gateway_close_1006")
             (nth-value 1 (ncd:reduce-gateway-close (ncd:initial-gateway-session) 1006)))))

(deftest channel-discord-gateway-url-params ()
  (is-each (ncd:gateway-url-with-params)
    ("wss://gateway.discord.gg/" "wss://gateway.discord.gg/?v=10&encoding=json" nil)
    ("wss://x.example/?v=9&encoding=etf" "wss://x.example/?v=9&encoding=etf" nil)
    ("wss://x.example/?a=1" "wss://x.example/?a=1&v=10&encoding=json" nil)))

(defun dgw-thread-create! (id parent)
  "Teach the adapter the thread ID on PARENT the way THREAD_CREATE does."
  (ncd::note-channel-dispatches
   (dgw-dispatch "THREAD_CREATE" (format nil "{\"id\": \"~a\", \"parent_id\": \"~a\"}" id parent))))

(deftest channel-discord-thread-knowledge ()
  ;; A MESSAGE_CREATE inside a thread names only the thread channel; the
  ;; adapter learns the parent from the gateway's own thread events.
  (clrhash ncd::*known-threads*)
  (ncd::note-channel-dispatches
   (cell-json "{\"op\": 0, \"t\": \"GUILD_CREATE\",
                  \"d\": {\"id\": \"g1\",
                          \"threads\": [{\"id\": \"t9\", \"parent_id\": \"c1\"}]}}"))
  (is (equal "c1" (ncd::thread-channel-parent "t9")))
  (dgw-thread-create! "t10" "c2")
  (is (equal "c2" (ncd::thread-channel-parent "t10")))
  (ncd::note-channel-dispatches (dgw-dispatch "THREAD_DELETE" "{\"id\": \"t10\"}"))
  (is (null (ncd::thread-channel-parent "t10")))
  ;; A starter message carries its thread; the message itself is channel.
  (let ((candidate (dgw-said "m20" "new topic" :channel "c3"
                             :fields "\"thread\": {\"id\": \"t11\", \"parent_id\": \"c3\"},")))
    (is-present candidate "a starter message routes"
      (is (equal "channel" (nck:source-field candidate "chat_kind")))
      (is (equal "c3" (ncd::thread-channel-parent "t11")))))
  (clrhash ncd::*known-threads*))

(deftest channel-discord-message-routing ()
  ;; Guild channel message.
  (let ((candidate (dgw-said "m1" "hi there" :user-name "kim" :bot "false")))
    (is-present candidate "a guild channel message routes"
      (is-source candidate :text "hi there" "chat_kind" "channel" "channel_id" "c1"
                 "workspace_id" "g1" "thread_id" nil)
      (is (equal "discord-c1" (nck:room-session-id "discord" candidate)))
      (is (equal '(:channel-id "c1" :thread-id nil :message-id "m1")
                 (nck:channel-target candidate)))))
  ;; Thread message: the message names only the thread channel — Discord
  ;; puts no parent on it — so the parent comes from the gateway's own
  ;; thread events, and the room is still keyed on the parent.
  (dgw-thread-create! "t9" "c1")
  (let ((candidate (dgw-said "m2" "in thread" :channel "t9")))
    (is-present candidate "a thread message routes"
      (is (equal "thread" (nck:source-field candidate "chat_kind")))
      (is (equal "discord-c1-tt9" (nck:room-session-id "discord" candidate)))
      (is (equal '(:channel-id "c1" :thread-id "t9" :message-id "m2")
                 (nck:channel-target candidate))))
    (clrhash ncd::*known-threads*))
  ;; Direct message: no guild.
  (let ((candidate (dgw-said "m3" "psst" :channel "dm1" :guild nil)))
    (is-present candidate "a message with no guild routes"
      (is-source candidate "chat_kind" "direct_message")))
  ;; The reply gesture: Discord's own pointer to another message is the
  ;; whole branch structure of a flat channel.
  (let ((candidate (dgw-said "m4" "now do signup" :author "u2"
                            :fields "\"message_reference\": {\"message_id\": \"a1\"},")))
    (is-present candidate "a reply routes"
      (is (equal "a1" (nck:source-field candidate "reply_to_message_id")))))
  (is (null (nck:source-field (dgw-said "m5" "plain" :author "u2") "reply_to_message_id")))
  ;; Non-message dispatches route nowhere.
  (is (null (ncd:route-discord-message (dgw-dispatch "TYPING_START" "{}"))))
  (is (null (ncd:route-discord-message (cell-json "{\"op\": 11}")))))

(defparameter +dgw-edited+ "\"edited_timestamp\": \"2026-10-20T11:18:07.000000+00:00\","
  "The FIELDS that stamp a d object as an edit someone typed (DGW-D).")

(defun dgw-update (d-json &key (involves-p (lambda (text)
                                             (search "<@999>" text))))
  "The candidate ROUTE-DISCORD-MESSAGE-UPDATE derives from the
MESSAGE_UPDATE dispatch whose d object is D-JSON."
  ;; INVOLVES-P stands in for the room's mention rule: the default recognizes
  ;; the entity mention of the bot these tests route to.
  (ncd:route-discord-message-update (dgw-dispatch "MESSAGE_UPDATE" d-json)
                                    :bot-user-id "999" :involves-p involves-p))

(deftest channel-discord-message-update-routing ()
  ;; The edit that turns an unaddressed message into an ask is an ask: the
  ;; lane reads the words that now involve it. Without this, an edit that
  ;; adds a mention would not route.
  (let ((candidate (dgw-update (dgw-d "m7" "is this possible <@999>" :author "u2"
                                      :user-name "kim" :fields +dgw-edited+))))
    (is-present candidate "an edit whose words involve the bot routes"
      (is-source candidate :text "is this possible <@999>" "chat_kind" "channel"
                 "channel_id" "c1" "user_id" "u2" "message_id" "m7")))
  ;; A wake word is involvement too: the predicate is the room's own rule,
  ;; and this router never restates it.
  (is-present (dgw-update (dgw-d "m8" "hey bot, take a look" :author "u2" :fields +dgw-edited+)
                          :involves-p (lambda (text) (search "hey bot" text)))
              "an edit the room's mention rule matches routes")
  ;; An edit inside a thread routes the way the thread's own messages do:
  ;; the thread is known from its own events, and the parent follows.
  (dgw-thread-create! "t1" "c1")
  (let ((candidate (dgw-update (dgw-d "m9" "<@999> look" :channel "t1" :author "u2"
                                      :fields +dgw-edited+))))
    (is-present candidate "an edit in a thread routes"
      (is-source candidate "chat_kind" "thread" "thread_id" "t1" "parent_channel_id" "c1"))
    (clrhash ncd::*known-threads*))
  ;; Everything else an update can be is not an ask.
  (is-table (d-json) (null (dgw-update d-json))
    ((dgw-d "m10" "just fixing a typo" :author "u2" :fields +dgw-edited+))
    ((dgw-d "m11" "working round 7 <@999>" :author "999" :bot "true" :fields +dgw-edited+))
    ((dgw-d "m12" nil :author "u2" :fields "\"pinned\": true,"))
    ("{\"id\": \"m13\", \"channel_id\": \"c1\",
       \"guild_id\": \"g1\", \"content\": \"now <@999>\"}")
    ((dgw-d "m14" "   " :author "u2" :fields +dgw-edited+))
    ;; The whole message restated by an update nobody typed -- a thread
    ;; opened on it or deleted from it, a pin -- carries no edit stamp: an
    ;; ask already answered must not arrive again (2026-10-20, "are you here?").
    ((dgw-d "m15" "<@999> are you here?" :author "u2"))
    ((dgw-d "m16" "<@999> are you here?" :author "u2"
            :fields "\"edited_timestamp\": null, \"flags\": 32,")))
  ;; The update router is not the create router.
  (is (null (ncd:route-discord-message-update
             (dgw-dispatch "MESSAGE_CREATE" "{}")
             :involves-p (lambda (text) (declare (ignore text)) t))))
  (is (null (ncd:route-discord-message (dgw-dispatch "MESSAGE_UPDATE" "{}")))))

(defun dgw-interaction (d-json &optional (route #'ncd:route-discord-interaction))
  "The candidate ROUTE derives from the INTERACTION_CREATE dispatch whose d
object is D-JSON: ROUTE-DISCORD-INTERACTION's by default, the kit's control
press through ROUTE-DISCORD-CONTROL, its completion request through
ROUTE-DISCORD-AUTOCOMPLETE."
  (funcall route (dgw-dispatch "INTERACTION_CREATE" d-json)))

(deftest channel-discord-interaction-routing ()
  ;; A slash interaction is the command line it spells, addressed by
  ;; construction, with its return path in the source and no message id.
  (let ((candidate (dgw-interaction
                    "{\"id\": \"i1\", \"token\": \"tok\", \"type\": 2,
                      \"guild_id\": \"g1\", \"channel_id\": \"c1\",
                      \"data\": {\"name\": \"models\",
                                 \"options\": [{\"name\": \"args\", \"type\": 3,
                                               \"value\": \"a6api grok-4.6\"}]},
                      \"member\": {\"user\": {\"id\": \"u1\",
                                              \"username\": \"kim\"}}}")))
    (is-present candidate "a guild slash interaction routes"
      (is-source candidate :text "/models a6api grok-4.6" "chat_kind" "channel" "channel_id" "c1"
                 "workspace_id" "g1" "user_id" "u1" "user_name" "kim" "interaction_id" "i1"
                 "interaction_token" "tok" "message_id" nil)
      (is (nck:candidate-addressed-p candidate))
      (is (equal "discord-c1" (nck:room-session-id "discord" candidate)))))
  (let ((candidate (dgw-interaction
                    "{\"id\": \"i2\", \"token\": \"tok\", \"type\": 2,
                      \"channel_id\": \"dm1\",
                      \"data\": {\"name\": \"help\"},
                      \"user\": {\"id\": \"u1\", \"username\": \"kim\"}}")))
    (is-present candidate "a DM interaction routes"
      (is-source candidate :text "/help" "chat_kind" "direct_message" "user_id" "u1")))
  ;; A command typed inside a thread is placed as a message typed there is:
  ;; the thread, its parent beside it, so /stop there finds the thread's turn
  ;; and a parent the allowlist names admits it. A text channel under a
  ;; category carries a parent_id too, and stays a channel.
  (let ((candidate (dgw-interaction
                    "{\"id\": \"i4\", \"token\": \"tok\", \"type\": 2,
                      \"application_id\": \"app1\",
                      \"guild_id\": \"g1\", \"channel_id\": \"t1\",
                      \"channel\": {\"id\": \"t1\", \"type\": 11, \"parent_id\": \"c1\"},
                      \"data\": {\"name\": \"stop\"},
                      \"member\": {\"user\": {\"id\": \"u1\", \"username\": \"kim\"}}}")))
    (is-present candidate "a slash command in a thread routes to the thread"
      (is-source candidate "chat_kind" "thread" "channel_id" "t1" "parent_channel_id" "c1"
                 "thread_id" "t1" "application_id" "app1")
      (is (equal "discord-c1-tt1" (nck:room-session-id "discord" candidate)))))
  (let ((candidate (dgw-interaction
                    "{\"id\": \"i5\", \"token\": \"tok\", \"type\": 2,
                      \"guild_id\": \"g1\", \"channel_id\": \"c2\",
                      \"channel\": {\"id\": \"c2\", \"type\": 0, \"parent_id\": \"cat1\"},
                      \"data\": {\"name\": \"help\"},
                      \"member\": {\"user\": {\"id\": \"u1\", \"username\": \"kim\"}}}")))
    (is-source candidate "chat_kind" "channel" "channel_id" "c2" "thread_id" nil))
  (is (null (dgw-interaction
             "{\"id\": \"i3\", \"token\": \"tok\", \"type\": 3,
               \"channel_id\": \"c1\", \"data\": {\"custom_id\": \"x\"}}")))
  (is (null (ncd:route-discord-interaction
             (dgw-dispatch "MESSAGE_CREATE" "{}")))))

(deftest channel-discord-control-routing ()
  ;; A press on a lane's button is the kit's normal control payload: the
  ;; custom_id is the data the act reads, and the interaction's own id and
  ;; token ride along — the return path its ack answers through.
  (flet ((route (d-json) (dgw-interaction d-json #'ncd:route-discord-control)))
    (let ((press (route
                  "{\"id\": \"i9\", \"token\": \"tok\", \"type\": 3,
                  \"application_id\": \"a1\",
                  \"guild_id\": \"g1\", \"channel_id\": \"c1\",
                  \"data\": {\"custom_id\": \"nck:stop:discord-c1-m9\",
                             \"component_type\": 2},
                  \"member\": {\"user\": {\"id\": \"u1\",
                                          \"username\": \"kim\"}},
                  \"message\": {\"id\": \"m9\"}}")))
      (is-present press "a guild button press routes"
        (is-shape press (:id "i9") (:token "tok" "the ack's return path")
          (:application-id "a1" "the application its follow-ups post under")
          (:data "nck:stop:discord-c1-m9") (:user-id "u1") (:message-id "m9") (:channel-id "c1"))))
    (let ((press (route
                  "{\"id\": \"i10\", \"token\": \"tok\", \"type\": 3,
                  \"channel_id\": \"dm1\",
                  \"data\": {\"custom_id\": \"nck:stop:discord-dm1-m1\"},
                  \"user\": {\"id\": \"u1\"},
                  \"message\": {\"id\": \"m1\"}}")))
      (is-present press "a DM button press routes"
        (is (equal "u1" (getf press :user-id)))))
    (is-table (d-json) (null (route d-json))
      ("{\"id\": \"i11\", \"token\": \"tok\", \"type\": 2,
         \"channel_id\": \"c1\", \"data\": {\"name\": \"help\"}}")
      ("{\"id\": \"i12\", \"token\": \"tok\", \"type\": 3,
         \"channel_id\": \"c1\", \"data\": {\"component_type\": 2}}"))
    (is (null (ncd:route-discord-control
               (dgw-dispatch "MESSAGE_CREATE" "{}"))))))

(deftest channel-discord-choice-press-routing ()
  ;; A press on a choice routes as the line it says, a reply to the message
  ;; pressed: a button's custom_id carries the line, a select's picked value
  ;; does. The source says it was pressed and carries the card's words, so
  ;; the answer can stand in its place.
  (let ((press (dgw-interaction
                (format nil "{\"id\": \"i9\", \"token\": \"tok\", \"type\": 3, \"application_id\": \"app1\",
                  \"guild_id\": \"g1\", \"channel_id\": \"c1\",
                  \"data\": {\"custom_id\": \"~a\", \"component_type\": 2},
                  \"member\": {\"user\": {\"id\": \"u1\", \"username\": \"kim\"}, \"roles\": [\"r1\"]},
                  \"message\": {\"id\": \"m9\", \"content\": \"What matters most?\"}}"
                        (nck:press-token "Zero setup")))))
    (is-present press "a choice's button press routes"
      (is-source press :text "Zero setup" "chat_kind" "channel" "channel_id" "c1" "user_id" "u1"
                 "reply_to_message_id" "m9" "card_text" "What matters most?"
                 "interaction_id" "i9" "interaction_token" "tok" "application_id" "app1"
                 "press_data" (nck:press-token "Zero setup") "message_id" nil)
      (is (nck:candidate-pressed-p press))
      (is (nck:candidate-addressed-p press))))
  (let ((pick (dgw-interaction
               (format nil "{\"id\": \"i10\", \"token\": \"tok\", \"type\": 3,
                 \"channel_id\": \"dm1\",
                 \"data\": {\"custom_id\": \"nck:menu:0\", \"component_type\": 3,
                            \"values\": [\"~a\"]},
                 \"user\": {\"id\": \"u1\"},
                 \"message\": {\"id\": \"m1\", \"content\": \"## Model Picker\"}}"
                       (nck:press-token "/models p1")))))
    (is-present pick "a menu pick routes as the line its value says"
      (is-source pick :text "/models p1" "chat_kind" "direct_message" "reply_to_message_id" "m1")))
  ;; Any other press is a control, its data the select's value where it has one.
  ;; A press this home never made is no line: the control path says it is
  ;; out of date.
  (is (null (dgw-interaction
             "{\"id\": \"i11\", \"token\": \"tok\", \"type\": 3, \"channel_id\": \"c1\",
               \"data\": {\"custom_id\": \"nck:p:000000000000\"}}")))
  (is (equal "x2" (getf (dgw-interaction
                         "{\"id\": \"i12\", \"token\": \"tok\", \"type\": 3, \"channel_id\": \"c1\",
                           \"data\": {\"custom_id\": \"m\", \"component_type\": 3, \"values\": [\"x2\"]},
                           \"user\": {\"id\": \"u1\"}, \"message\": {\"id\": \"m1\"}}"
                         #'ncd:route-discord-control)
                        :data))))

(deftest channel-discord-autocomplete-routing ()
  ;; An option completed while it is typed is the kit's normal completion
  ;; request: the command, the typed tail of the focused option, who is
  ;; typing, and the interaction's own id and token — the return path the
  ;; one answer goes through.
  (flet ((route (d-json) (dgw-interaction d-json #'ncd:route-discord-autocomplete)))
    (let ((request (route
                    "{\"id\": \"i20\", \"token\": \"tok\", \"type\": 4,
                    \"guild_id\": \"g1\", \"channel_id\": \"c1\",
                    \"data\": {\"name\": \"models\",
                               \"options\": [{\"name\": \"args\", \"type\": 3,
                                              \"value\": \"grok\",
                                              \"focused\": true}]},
                    \"member\": {\"user\": {\"id\": \"u1\",
                                            \"username\": \"kim\"}}}")))
      (is-present request "a guild autocomplete interaction routes"
        (is-shape request (:id "i20") (:token "tok" "the answer's return path") (:command "models")
          (:text "grok") (:user-id "u1") (:channel-id "c1"))))
    (let ((request (route
                    "{\"id\": \"i21\", \"token\": \"tok\", \"type\": 4,
                    \"channel_id\": \"dm1\",
                    \"data\": {\"name\": \"models\",
                               \"options\": [{\"name\": \"args\", \"type\": 3,
                                              \"value\": \"\", \"focused\": true}]},
                    \"user\": {\"id\": \"u1\"}}")))
      (is-present request "a DM autocomplete interaction routes"
        (is-shape request (:user-id "u1") (:text ""))))
    ;; Typed in a thread, it is placed as a command typed there is: the
    ;; thread, under its channel.
    (let ((request (route
                    "{\"id\": \"i23\", \"token\": \"tok\", \"type\": 4,
                    \"guild_id\": \"g1\", \"channel_id\": \"t9\",
                    \"channel\": {\"id\": \"t9\", \"type\": 11, \"parent_id\": \"c1\"},
                    \"data\": {\"name\": \"agent\",
                               \"options\": [{\"name\": \"args\", \"type\": 3,
                                              \"value\": \"\", \"focused\": true}]},
                    \"member\": {\"user\": {\"id\": \"u1\"}}}")))
      (is-present request "an autocomplete typed in a thread routes"
        (is-shape request (:channel-id "c1" "under its channel") (:thread-id "t9"))))
    (is (null (route
               "{\"id\": \"i22\", \"token\": \"tok\", \"type\": 2,
               \"channel_id\": \"c1\", \"data\": {\"name\": \"help\"}}")))
    (is (null (ncd:route-discord-autocomplete
               (dgw-dispatch "MESSAGE_CREATE" "{}"))))))

(defun dgw-reaction (event d-json &rest keys)
  "The candidate ROUTE-DISCORD-REACTION derives from the EVENT dispatch
whose d object is D-JSON. KEYS reach the router as they are."
  (apply #'ncd:route-discord-reaction (dgw-dispatch event d-json) keys))

(deftest channel-discord-reaction-routing ()
  ;; A reaction is a line the room said without typing one: the emoji is the
  ;; candidate's text, the message it sits on is its reply gesture, and it
  ;; has no message id of its own — a reaction is not a message. A reaction
  ;; on one of ours is addressed, exactly as a reply to one is.
  (let ((added (dgw-reaction
                "MESSAGE_REACTION_ADD"
                "{\"user_id\": \"u1\", \"channel_id\": \"c1\",
                  \"message_id\": \"a1\", \"guild_id\": \"g1\",
                  \"message_author_id\": \"999\",
                  \"emoji\": {\"id\": null, \"name\": \"✅\"},
                  \"member\": {\"user\": {\"id\": \"u1\",
                                          \"username\": \"kim\"}}}"
                :bot-user-id "999")))
    (is-present added "a guild reaction routes"
      (is (equal "✅" (gethash "text" added)) "the emoji reaches the room as it was sent")
      (is (null (nck:source-field added "message_id")) "a reaction is not a message")
      (is-source added "reaction" "add" "reply_to_message_id" "a1" "user_id" "u1" "user_name" "kim"
                 "channel_id" "c1" "workspace_id" "g1" "chat_kind" "channel")
      (is (nck:candidate-addressed-p added) "left on a message of ours")))
  ;; Taken back: the same gesture, the other way round.
  (let ((removed (dgw-reaction
                  "MESSAGE_REACTION_REMOVE"
                  "{\"user_id\": \"u1\", \"channel_id\": \"dm1\",
                    \"message_id\": \"a1\",
                    \"emoji\": {\"id\": null, \"name\": \"✅\"}}"
                  :bot-user-id "999")))
    (is-present removed "a direct-message reaction routes"
      (is-source removed "reaction" "remove" "chat_kind" "direct_message")
      (is (not (nck:candidate-addressed-p removed)))))
  ;; A guild's own emoji has no character to send: Discord's own spelling of
  ;; one, which is what a bot that cannot render the image writes.
  (is (equal ":shipit:"
             (gethash "text"
                      (dgw-reaction "MESSAGE_REACTION_ADD"
                                    "{\"user_id\": \"u1\", \"channel_id\": \"c1\",
                                      \"message_id\": \"a1\", \"guild_id\": \"g1\",
                                      \"emoji\": {\"id\": \"55\", \"name\": \"shipit\"}}"
                                    :bot-user-id "999"))))
  ;; The eye the kit leaves on an ask is a reaction too: a route that read
  ;; our own back would answer itself. Another bot's is nobody's ask either.
  (is-table (d-json) (null (dgw-reaction "MESSAGE_REACTION_ADD" d-json :bot-user-id "999"))
    ("{\"user_id\": \"999\", \"channel_id\": \"c1\",
       \"message_id\": \"a1\", \"guild_id\": \"g1\",
       \"emoji\": {\"id\": null, \"name\": \"👀\"}}")
    ("{\"user_id\": \"u2\", \"channel_id\": \"c1\",
       \"message_id\": \"a1\", \"guild_id\": \"g1\",
       \"emoji\": {\"id\": null, \"name\": \"✅\"},
       \"member\": {\"user\": {\"id\": \"u2\", \"bot\": true}}}")
    ("{\"user_id\": \"u1\", \"channel_id\": \"c1\",
       \"message_id\": \"a1\", \"guild_id\": \"g1\"}"))
  (is (null (ncd:route-discord-reaction (dgw-dispatch "MESSAGE_CREATE" "{}")))))

(deftest channel-discord-reply-to-the-bot-is-addressed ()
  ;; The section promises that a reply to the bot counts as a mention: a
  ;; message whose referenced message the bot authored is addressed.
  (flet ((candidate (&rest identity)
           (apply #'dgw-said "m6" "/undo" :author "u2"
                  :fields "\"message_reference\": {\"message_id\": \"a1\"},
                           \"referenced_message\": {\"id\": \"a1\",
                                                    \"author\": {\"id\": \"999\",
                                                                 \"bot\": true}},"
                  identity)))
    (is (nck:candidate-addressed-p (candidate :bot-user-id "999")))
    (is (equal "a1" (nck:source-field (candidate :bot-user-id "999")
                                      "reply_to_message_id")))
    (is (not (nck:candidate-addressed-p (candidate :bot-user-id "1"))))
    (is (not (nck:candidate-addressed-p (candidate))))))

(deftest channel-discord-reply-carries-the-answered-message ()
  ;; The reply gesture decides the lane; what was answered must ride with the
  ;; ask — the lane reads it without a fetch.
  (let ((reply (gethash "reply" (dgw-said "m7" "and? " :author "u2" :bot-user-id "999"
                                          :fields "\"message_reference\": {\"message_id\": \"a2\"},
                      \"referenced_message\": {\"id\": \"a2\",
                                               \"author\": {\"id\": \"999\",
                                                            \"username\": \"scrap\",
                                                            \"bot\": true},
                                               \"content\": \"v0.1 of ours is running\"},"))))
    (is-shape reply ("text" "v0.1 of ours is running") ("user_name" "scrap") ("id" "a2")))
  (is (null (gethash "reply" (dgw-said "m8" "plain" :author "u2" :bot-user-id "999")))))

(deftest channel-discord-self-role-mentions-address-the-bot ()
  ;; Operator report: asks typed with the @scrap the picker offers fell on the
  ;; floor and the room saw nothing at all. That pill was the role — Discord
  ;; gives every bot a managed role carrying its own name — and the mention
  ;; rule knew only the user's <@id>. The roles are what the bot's own member
  ;; object carries in each guild it is in: HYDRATE-SELF-ROLES notes them and
  ;; the rule reads what it noted.
  (let ((ncd:*self-role-ids* (make-hash-table :test #'equal)))
    (let ((policy (ncd:discord-inbound-policy
                   :allowed-guilds '("g1")
                   :allowed-users '("42")
                   :require-mention t
                   :bot-user-id "999")))
      (flet ((involves (text) (nck:mention-involves-p policy text)))
        (is (not (involves "<@&456> oi")))
        (ncd:note-self-roles '("456"))
        (is (involves "<@&456> oi"))
        (is (involves "<@999> oi") "and the user pill still does")
        (is (not (involves "<@&457> oi"))))
      (is-inbound policy (dgw-said "m1" "<@&456> oi" :author "42")
                  :answer nil "an ask that mentioned the bot with the role pill answers"))))

(deftest channel-discord-speaking-policy-is-per-channel ()
  (let ((policy (ncd:discord-inbound-policy
                 :allowed-channels '("free" "gated" "off")
                 :require-mention nil
                 :free-response-channels '("free")
                 :require-mention-channels '("gated")
                 :ignored-channels '("off")
                 :mention-patterns '("hey nodecode")
                 :bot-user-id "999")))
    (flet ((candidate (channel text) (dgw-said "m1" text :channel channel)))
      (is-inbound policy (candidate "free" "hello") :answer)
      (is-inbound policy (candidate "gated" "hello") :observe "mention_required")
      (is-inbound policy (candidate "gated" "HEY NODECODE, hello") :answer)
      (is-inbound policy (candidate "off" "hello") :reject "channel_ignored")
      (is-inbound policy (candidate "other" "hello") :reject "channel_not_allowed"))))

(deftest channel-discord-message-carries-image-attachments ()
  ;; Every attachment rides the candidate by reference — the url to fetch,
  ;; not bytes: the kit reads in what it can and saves the rest.
  (let ((candidate (dgw-said "m9" "what does this say?"
                            :fields "\"attachments\": [
                        {\"url\": \"https://cdn.discordapp.com/x.png\",
                         \"content_type\": \"image/png\",
                         \"filename\": \"x.png\", \"size\": 4211},
                        {\"url\": \"https://cdn.discordapp.com/y.pdf\",
                         \"content_type\": \"application/pdf\",
                         \"filename\": \"y.pdf\", \"size\": 12}],")))
    (is-present candidate "a message with attachments routes"
      (is-present (image (first (nck:candidate-attachments candidate)))
        "the image is carried, and the pdf beside it"
        (is (= 2 (length (nck:candidate-attachments candidate))))
        (is-shape image ("url" "https://cdn.discordapp.com/x.png") ("media_type" "image/png")
          ("filename" "x.png") ("size" = 4211))
        (is-shape (second (nck:candidate-attachments candidate)) ("media_type" "application/pdf")))))
  ;; A message with no attachments carries none.
  (is (null (nck:candidate-attachments (dgw-said "m10" "plain"))) "no attachments, none carried")
  ;; Only the declared type is read here — a file Discord calls an image can
  ;; still be refused where it is fetched (the kit sniffs the bytes).
  (let ((candidate (dgw-said "m11" ""
                            :fields "\"attachments\": [{\"url\": \"https://cdn.discordapp.com/z.jpg\",
                                                        \"content_type\": \"image/jpeg\",
                                                        \"filename\": \"z.jpg\", \"size\": 9}],")))
    (is-present candidate "a textless message with an image routes"
      (is (= 1 (length (nck:candidate-attachments candidate))))
      (is (search "[an image is attached"
                  (nck:speaker-line candidate
                                    :attachments (nck:candidate-attachments candidate)))))))

(deftest channel-discord-a-file-off-discord-is-not-fetched ()
  ;; The bot fetches what a payload names with its own network reach, this
  ;; machine's included: a url that is not on Discord's file hosts, or the
  ;; configured api_base's, is dropped with a warning.
  (let ((texts (warnings-of
                 (is-present (candidate (dgw-said "m14" "look"
                                                  :fields "\"attachments\": [
                        {\"url\": \"https://cdn.discordapp.com/a.png\", \"content_type\": \"image/png\",
                         \"filename\": \"a.png\", \"size\": 1},
                        {\"url\": \"http://127.0.0.1:8080/admin\", \"content_type\": \"text/plain\",
                         \"filename\": \"b.txt\", \"size\": 1}],")) "it routes"
                   (is (equal '("https://cdn.discordapp.com/a.png")
                              (mapcar (lambda (file) (gethash "url" file))
                                      (nck:candidate-attachments candidate))))))))
    (is (search "not a Discord host" (first texts))))
  (with-saved-globals ((ncd::*file-hosts* (list* "127.0.0.1" ncd::+discord-file-hosts+)))
    (is (ncd::file-host-p "http://127.0.0.1:7300/attachments/1/b.txt"))))

(deftest channel-discord-roles-and-other-bots-come-from-the-section ()
  ;; The author's roles ride the candidate for allowed_roles to read, and
  ;; allow_bots is read in the section's words.
  (let ((candidate (dgw-said "m15" "deploy?" :fields "\"member\": {\"roles\": [\"r1\", \"r2\"]},")))
    (is (equal '("r1" "r2") (nck::candidate-role-ids candidate)))
    (is-inbound (ncd:discord-inbound-policy :allowed-roles '("r2") :bot-user-id "999") candidate :answer)
    (is-inbound (ncd:discord-inbound-policy :allowed-roles '("r9") :bot-user-id "999") candidate
                :reject "user_not_allowed"))
  (is-each (nck::inbound-policy-allow-bots)
    ((ncd:discord-inbound-policy) :none nil)
    ((ncd:discord-inbound-policy :allow-bots "mentions") :mentions nil)
    ((ncd:discord-inbound-policy :allow-bots "all") :all nil)))

(deftest channel-discord-voice-message-carries-its-recording ()
  ;; A Discord voice message is a textless message whose one attachment is
  ;; audio/ogg with the length it declares: it rides the candidate like an
  ;; image, its seconds with it, and its line stands at the colon where the
  ;; transcript is read in. A video beside it rides too.
  (let ((candidate (dgw-said "m12" "" :user-name "v1se" :fields "\"flags\": 8192,
                      \"attachments\": [
                        {\"url\": \"https://cdn.discordapp.com/voice-message.ogg\",
                         \"content_type\": \"audio/ogg\",
                         \"filename\": \"voice-message.ogg\", \"size\": 22399,
                         \"duration_secs\": 5.52, \"waveform\": \"AAAA\"},
                        {\"url\": \"https://cdn.discordapp.com/clip.mp4\",
                         \"content_type\": \"video/mp4\",
                         \"filename\": \"clip.mp4\", \"size\": 900}],")))
    (is-present (recording (first (nck:candidate-attachments candidate)))
      "the voice message is carried, and the video"
      (is (= 2 (length (nck:candidate-attachments candidate))))
      (is-shape recording ("media_type" "audio/ogg") ("filename" "voice-message.ogg")
        ("size" = 22399))
      (is (< 5.51 (gethash "seconds" recording) 5.53)))
    (is (equal "v1se [mm12 uu1]:"
               (nck:speaker-line candidate
                                 :attachments (nck:candidate-attachments candidate))))
    (is (null (gethash "id" (first (nck:candidate-attachments candidate))))))
  ;; A reply to a voice message carries the recording it answers, by the
  ;; attachment's own id.
  (let ((candidate (dgw-said "m13" "do u hear this"
                            :fields "\"referenced_message\": {
                        \"id\": \"m12\", \"content\": \"\",
                        \"author\": {\"id\": \"u1\", \"username\": \"peas\"},
                        \"attachments\": [
                          {\"id\": \"1549867778293502102\",
                           \"url\": \"https://cdn.discordapp.com/qb8rzbq.ogg\",
                           \"content_type\": \"audio/ogg\",
                           \"filename\": \"qb8rzbq.ogg\", \"size\": 22399,
                           \"duration_secs\": 5.52}]},")))
    (is-present (recording (first (nck:candidate-attachments
                                   (gethash "reply" candidate))))
      "the answered voice message rides the reply"
      (is-shape recording ("id" "1549867778293502102") ("media_type" "audio/ogg")))))

(deftest channel-discord-a-forward-is-read-whole ()
  ;; A forwarded message points at its original in another channel and
  ;; carries it as a snapshot: its words and files are what the person
  ;; said, under their own, and the pointer is no reply gesture.
  (let ((candidate (dgw-said "m20" "thoughts?"
                             :fields "\"message_reference\": {\"type\": 1, \"message_id\": \"o1\",
                                                           \"channel_id\": \"other\"},
                                      \"message_snapshots\": [{\"message\": {
                                        \"content\": \"the build is red on main\",
                                        \"attachments\": [{\"url\": \"https://cdn.discordapp.com/ci.png\",
                                                          \"content_type\": \"image/png\",
                                                          \"filename\": \"ci.png\", \"size\": 10}]}}],")))
    (is (equal (format nil "thoughts?~%[forwarded message] the build is red on main")
               (gethash "text" candidate)))
    (is (= 1 (length (nck:candidate-attachments candidate))))
    (is (null (nck:source-field candidate "reply_to_message_id")))
    ;; A plain reply keeps its gesture.
    (is (equal "o2" (nck:source-field (dgw-said "m21" "yes" :fields "\"message_reference\": {\"message_id\": \"o2\"},")
                                      "reply_to_message_id")))))
