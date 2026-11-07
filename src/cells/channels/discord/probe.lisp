;;;; probe.lisp --- what the token can see, and how the operator gets one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) resolves the secret the way
;;;; START-CHANNEL does, asks Discord who the bot is and where it may
;;;; speak, and answers a text — never the token — so channel and user ids
;;;; are picked by name from the conversation instead of typed out of
;;;; Developer Mode. openclaw's probe.ts / resolve-users.ts are the shape;
;;;; chrome's do-doctor is the in-tree precedent. The section declaration
;;;; at the end is the adapter's one statement of what channels.discord is
;;;; made of (NLK:DEFINE-SECTION): the kit's refusal, the setup wizard's
;;;; panel, the model's setup primer and the cells report all read it.

(in-package #:nodecode-channel-discord)

(defparameter +probed-channel-types+ '(0 5)
  "GUILD_TEXT and GUILD_ANNOUNCEMENT: where a text message can land.")

(defparameter +probed-guilds-cap+ 10)

(defun probe-array (body)
  "A top-level JSON array from an execution body — the executor decodes it
to a vector (NCK PARSE-JSON-BODY) — or NIL for any other shape."
  (and (vectorp body) (not (stringp body)) body))

(defun probe-text-channels (execution)
  "The text channels a guild's channel read EXECUTION answered, as a list:
none when it failed."
  (remove-if-not
   (lambda (channel)
     (member (nlk:json-value channel :integer "type") +probed-channel-types+))
   (coerce (or (and (execution-ok-p execution) (probe-array (execution-body execution))) #())
           'list)))

(defun probe-guilds (section executor timeout)
  "(values PROBLEM GUILDS ME FETCH) with SECTION's token: the line saying why
nothing further was read, else NIL; the first +PROBED-GUILDS-CAP+ guilds as
(ID NAME CHANNELS-EXECUTION); the /users/@me execution; the probe's GET."
  (let* ((token (resolve-channel-secret section "bot_token"))
         (executor (or executor
                       (make-discord-executor
                        :api-base (config-string section "api_base"
                                                 +discord-rest-api-base+)
                        :token token))))
    (flet ((fetch (path)
             (execute-plan executor (rest-plan "GET" path "probe" nil timeout))))
      (let* ((me (fetch "/users/@me"))
             (guilds (and (execution-ok-p me) (fetch "/users/@me/guilds")))
             (vector (and guilds (execution-ok-p guilds)
                          (probe-array (execution-body guilds)))))
        (values (cond ((not (execution-ok-p me))
                       (format nil "token: ~a"
                               (if (eql 401 (execution-status me))
                                   "unauthorized (401): the token is wrong or revoked; reset it on the Bot page of the developer portal and save the new one"
                                   (probe-failure me))))
                      ((not (execution-ok-p guilds))
                       (format nil "guilds: ~a" (probe-failure guilds)))
                      ((or (null vector) (zerop (length vector)))
                       "guilds: none — invite the bot with the OAuth2 URL from the developer portal"))
                (loop for guild across (or vector #())
                      for count from 0 below +probed-guilds-cap+
                      collect (let ((id (nlk:json-value guild :string "id")))
                                (list id (nlk:json-value guild :string "name")
                                      (fetch (format nil "/guilds/~a/channels" id)))))
                me
                #'fetch)))))

(defun probe-channel (section &key executor (timeout 15))
  "What the token in SECTION can see, as lines: the bot's identity, each
guild with its text channels, whether the Message Content intent is
granted."
  ;; EXECUTOR overrides the live one (the scripted test seam). Read only; the
  ;; answer never carries the token (the executor redacts every failure text).
  ;; A wrong token is one line naming the 401.
  (nlk:bind (((problem guilds me fetch) (probe-guilds section executor timeout)) (lines '()))
    (flet ((say (control &rest args)
             (push (apply #'format nil control args) lines)))
      (when (execution-ok-p me)
        (let* ((body (execution-body me))
               (discriminator (nlk:json-value body :string "discriminator")))
          ;; A bot still on the old discriminators reads nodecode#4821.
          (say "token: ok — bot ~a~@[#~a~] (id ~a)"
               (nlk:json-value body :string "username")
               (and (not (equal discriminator "0")) discriminator)
               (nlk:json-value body :string "id"))))
      (when problem (say "~a" problem))
      ;; A guild's text channels read as `#name id' entries, capped at 30.
      (loop for (id name channels) in guilds
            for text = (probe-text-channels channels)
            for shown = (subseq text 0 (min (length text) 30))
            do (say "guild ~s (id ~a): ~a" name id
                    (cond ((not (execution-ok-p channels))
                           (format nil "channels: ~a" (probe-failure channels)))
                          ((null text) "no text channels")
                          (t (format nil "~{~a~^, ~}~@[, +~d more~]"
                                     (mapcar (lambda (channel)
                                               (format nil "#~a ~a"
                                                       (nlk:json-value channel :string "name")
                                                       (nlk:json-value channel :string "id")))
                                             shown)
                                     (and (> (length text) 30) (- (length text) 30)))))))
      (let ((application (and (execution-ok-p me) (funcall fetch "/applications/@me"))))
        (when (and application (execution-ok-p application))
          (nlk:when-let (flags (nlk:json-value (execution-body application)
                                               :integer "flags"))
            ;; GATEWAY_MESSAGE_CONTENT, or _LIMITED (granted under 100 servers).
            (say (if (logtest flags (logior (ash 1 18) (ash 1 19)))
                     "intents: message content granted"
                     "intents: message content not granted — enable Message Content Intent on the Bot page, or keep require_mention true"))))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun probe-channel-choices (section &key executor (timeout 15))
  "The text channels the token in SECTION can post to, as ((id . label)
...) with the server named in the label -- what a setup panel offers for
allowed_channels, so ids are picked by name."
  ;; Refuses (CONFIG-REFUSAL) in the probe's own words when the token is wrong
  ;; or the bot is in no server.
  (multiple-value-bind (problem guilds) (probe-guilds section executor timeout)
    (when problem (config-error "~a" problem))
    (loop for (nil name channels) in guilds
          append (mapcar (lambda (channel)
                           (cons (nlk:json-value channel :string "id")
                                 (format nil "#~a  ~a"
                                         (nlk:json-value channel :string "name")
                                         name)))
                         (probe-text-channels channels)))))

;;; The declaration. Types and constraints are what START-CHANNEL reads and
;;; RESOLVE-CHANNEL-SECRET / REQUIRE-NON-EMPTY-ALLOWLIST enforce; the kit
;;; refuses on SECTION-PROBLEMS before either runs, so the refusal, the
;;; panel and the primer say the same thing in the same words.
(nlk:define-section ("channels" "discord")
  (:guide "discord.com/developers/applications: New Application; on the Bot page, Reset Token and copy the token — it is kept in a file or an environment variable, never in the config; under Privileged Gateway Intents enable Message Content Intent, or keep require_mention true; OAuth2 > URL Generator: scopes bot and applications.commands (the second is what lets the bot's slash commands show in the server), permissions View Channels, Send Messages, Read Message History, Add Reactions, Attach Files, and Connect and Speak for voice; open the generated URL and pick the server; with the token saved, channels and users are picked by name; Discord's Developer Mode (User Settings > Advanced) also puts Copy ID on the right-click menu.")
  (:check #'probe-channel)
  (:one-of "bot_token_env" "bot_token_file")
  (:any-of "allowed_channels" "allowed_users" "allowed_roles")
  ("bot_token_env" :env :doc "environment variable holding the bot token")
  ("bot_token_file" :path :doc "file holding the bot token")
  ("allowed_channels" :list :doc "channel ids the bot answers in"
                      :choices #'probe-channel-choices)
  ("allowed_users" :list :doc "user ids allowed to drive the bot")
  ("allowed_roles" :list :doc "role ids: a server member holding any one may drive the bot as a listed user does (in the server; a direct message carries no roles)")
  ("allow_bots" :choice :options '("none" "mentions" "all") :default "none"
                :doc "other bots' messages: none ignores them, mentions answers one that @mentions this bot, all treats them as people's; the bot's own are always ignored")
  ("pairing" :boolean :default t
             :doc "a direct message from someone not allowed is answered with a pairing code, and the operator is told; /channels pair CODE lets them in, /channels unpair ID takes it back")
  ("owner" :list :doc "the operator's user id, whose word is standing policy in the room")
  ("allowed_guilds" :list :doc "server ids; empty admits every server the bot is in")
  ("require_mention" :boolean :default t
                     :doc "default for channels not listed below: answer when @mentioned, replied to, or in a DM; false answers every unlisted channel and needs the Message Content intent")
  ("free_response_channels" :list
                            :choices #'probe-channel-choices
                            :doc "channel ids where every allowed human message may open a turn")
  ("require_mention_channels" :list
                               :choices #'probe-channel-choices
                               :doc "channel ids where a message must mention or reply to the bot")
  ("ignored_channels" :list
                       :choices #'probe-channel-choices
                       :doc "channel ids the bot reads neither as prompts nor room context")
  ("mention_patterns" :list
                       :doc "case-insensitive wake words that address the bot in mention-gated channels")
  ("other_addressees" :list
                       :doc "names other than the bot's that a message may open by addressing (\"ada, ...\"); such a message is observed, never answered")
  ("dm_policy" :choice :options '("allow" "disabled") :default "allow"
               :doc "direct messages")
  ("group_policy" :choice :options '("allow" "disabled") :default "allow"
                  :doc "group direct messages")
  ("reactions" :boolean :default :false
               :doc "mark each ask on the message itself: 👀 from admission through the working turn, then ✅ when its answer lands or ❌ when it fails; needs Add Reactions")
  ("stream" :boolean :default t
            :doc "the model's words reach the room as it writes them, a message edited as it grows; the answer still posts fresh, pinging its asker, and its draft goes; false, each round's words arrive whole")
  ("voice_replies" :choice :options '("off" "on" "tts") :default "off"
                   :doc "an answer comes as a voice message too, below its words: on, to an ask said in a voice message; tts, to every ask; off, never; /voice on, tts or off sets it for one room")
  ("turn_budget_minutes" :integer :default 0
                        :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("room_tokens" :integer :default 40000
                :doc "the most history the room keeps, in estimated tokens (the provider counts about a quarter more); past it the older half is evicted, so every ask opens on at most this much; 0 keeps everything")
  ("catch_up_minutes" :integer :default 60
                     :doc "after the bot was away — a restart, a machine that was off — read what the channels said in the last this-many minutes of it and answer the asks it missed, each marked as late; 0 never")
  ("soul_file" :path :doc "a SOUL.md whose text is the room's standing persona")
  ("board_channel" :string
                   :choices #'probe-channel-choices
                   :doc "a channel the bot keeps one message in, edited in place: every open thread it started, by who moves next (needs you, no answer, running, done); a done thread closes after 48 hours quiet, any other after 7 days")
  ;; Voice needs none of these: /voice join sits beside whoever typed it.
  ;; Discord requires its end-to-end encryption (DAVE) for voice now, so the
  ;; first join fetches libdave into the cache — see discord/dave.lisp.
  ("voice_channel_id" :string
                      :doc "a voice channel of the bot's own: voice_autojoin sits in it, and /voice join typed by someone in no voice channel; absent, the bot sits only beside whoever asks or whom it follows")
  ("voice_text_channel_id" :string
                           :choices #'probe-channel-choices
                           :doc "the channel the voice lane talks through — its transcript, status and answers; defaults to the voice channel's own chat")
  ("voice_speakers" :list
                    :doc "user ids whose speech opens a turn; empty falls back to allowed_users, then owner, then whoever typed /voice join")
  ("voice_autojoin" :boolean :default :false
                    :doc "sit in the voice channel whenever somebody is in it: join when the first person comes in, leave when the last one goes; false waits for /voice join")
  ("voice_follow" :list
                  :doc "user ids the bot follows into voice: it joins the channel the first of them sits in, moves when they move, and leaves when they leave")
  ("voice_idle_minutes" :integer :default 5
                        :doc "a seat /voice join took is given up after this many minutes with nobody speaking to the bot; 0 stays"))
