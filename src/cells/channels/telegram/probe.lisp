;;;; probe.lisp --- what the token can see, and how the operator gets one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) resolves the secret the way
;;;; START-CHANNEL does, asks Telegram who the bot is and which chats have
;;;; written to it, and answers a text — never the token — so chat and
;;;; user ids are picked by name from the conversation. Bots cannot list
;;;; their chats; the pending update stream is the one window, which is why
;;;; the primer tells the operator to message the bot first. The section
;;;; declaration at the end is the adapter's one statement of what
;;;; channels.telegram is made of (NLK:DEFINE-SECTION): the kit's refusal,
;;;; the setup wizard's panel, the model's setup primer and the cells
;;;; report all read it.

(in-package #:nodecode-channel-telegram)

(nlk:access (me execution))

(defun chats-problem (me updates chats &optional report-p)
  "Why PROBE-READS' answer lists no chat, or NIL: the token, a running lane
holding the update stream, a failed read, no chat written yet. REPORT-P
words it for the probe's report rather than a setup panel's refusal."
  (cond
    ((not (execution-ok-p me))
     (format nil "token: ~a"
             (if (eql 401 me.status)
                 "unauthorized (401): the token is wrong or revoked; ask @BotFather for a new one (/token) and save it"
                 (probe-failure me))))
    ((and (not (execution-ok-p updates)) (eql 409 (execution-status updates)))
     (format nil "chats: the telegram lane is running and holds the update stream; its chats are in /channels~:[~; and every user line carries the [m.. u.. r..] handles~]"
             report-p))
    ((not (execution-ok-p updates))
     (format nil "chats: ~a" (probe-failure updates)))
    ((null chats)
     (format nil "chats: none have written to the bot yet — ~:[send it a message, then try again~;have the operator send it a message, then probe again~]"
             report-p))))

(defun probe-reads (section executor timeout)
  "(values ME UPDATES CHATS) with SECTION's token: getMe; when it answered,
getUpdates without an offset (nothing consumed); when that answered, its
distinct chats."
  (let* ((token (resolve-channel-secret section "token"))
         (executor (or executor
                       (make-telegram-executor
                        :api-base (config-string section "api_base"
                                                 +telegram-api-base+)
                        :token token))))
    (flet ((post (path body)
             (execute-plan executor (bot-plan path body timeout nil "probe"))))
      (let* ((me (post "/getMe" (nlk:json-object)))
             (updates (and me.ok-p (post "/getUpdates" (nlk:json-object "timeout" 0 "limit" 100))))
             (chats '()))
        ;; First appearance first, each chat a plist (:id :kind :name :from-id :from-name).
        (loop for update across (or (and updates (execution-ok-p updates)
                                         (nlk:json-value (execution-body updates) :any "result"))
                                    #())
              for message = (or (nlk:json-value update :object "message")
                                (nlk:json-value update :object "edited_message")
                                (nlk:json-value update :object "channel_post"))
              for chat = (nlk:json-value message :object "chat")
              for id = (nlk:json-value chat :any "id")
              when (and id (not (find id chats :key (lambda (entry) (getf entry :id))
                                               :test #'equal)))
                do (push (list :id id
                               :kind (nlk:json-value chat :string "type")
                               :name (or (nlk:json-value chat :string "title")
                                         (nlk:json-value chat :string "username")
                                         (nlk:json-value chat :string "first_name"))
                               :from-id (nlk:json-value message :any "from" "id")
                               :from-name (or (nlk:json-value message :string "from" "username")
                                              (nlk:json-value message :string "from" "first_name")))
                         chats))
        (values me updates (nreverse chats))))))

(defun probe-channel (section &key executor (timeout 15) &aux (lines '()))
  "What the token in SECTION can see, as lines: the bot's identity and the
chats that have written to it."
  ;; EXECUTOR overrides the live one (the scripted test seam). Read only — the
  ;; pending updates are read without an offset, so nothing is consumed; a
  ;; running lane holds the update stream and the probe says so instead. The
  ;; answer never carries the token (the executor redacts it from every
  ;; failure text).
  (flet ((say (control &rest args)
           (push (apply #'format nil control args) lines)))
    (multiple-value-bind (me updates chats) (probe-reads section executor timeout)
      (when (execution-ok-p me)
        (say "token: ok — bot @~a (id ~a)"
             (nlk:json-value me.body :string "result" "username")
             (nlk:json-value me.body :any "result" "id")))
      (nlk:if-let (problem (chats-problem me updates chats t))
        (say "~a" problem)
        (dolist (chat chats)
          (say "chat ~s (~a, chat id ~a~@[, from ~a~]~@[ user id ~a~])"
               (or (getf chat :name) "?")
               (getf chat :kind)
               (getf chat :id)
               (getf chat :from-name)
               (getf chat :from-id)))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun probe-update-chats (section &key executor (timeout 15))
  "The chats that have written to the token in SECTION, as PROBE-READS'
plists."
  ;; Refuses (CONFIG-REFUSAL) in the probe's words when the token is wrong,
  ;; when a running lane holds the update stream, or when no chat has written
  ;; yet.
  (multiple-value-bind (me updates chats) (probe-reads section executor timeout)
    (nlk:when-let (problem (chats-problem me updates chats))
      (config-error "~a" problem))
    chats))

(defun probe-chat-choices (section &key executor (timeout 15))
  "The chats that wrote to the bot as ((chat-id . label) ...) -- what a
setup panel offers for allowed_chats."
  (mapcar (lambda (chat)
            (cons (princ-to-string (getf chat :id))
                  (format nil "~a  ~a" (or (getf chat :name) "?") (getf chat :kind))))
          (probe-update-chats section :executor executor :timeout timeout)))

(defun probe-user-choices (section &key executor (timeout 15) &aux (choices '()))
  "The users who wrote to the bot as ((user-id . label) ...), each once --
what a setup panel offers for allowed_users."
  (dolist (chat (probe-update-chats section :executor executor :timeout timeout))
    (let ((id (getf chat :from-id)))
      (when (and id (not (assoc (princ-to-string id) choices :test #'string=)))
        (push (cons (princ-to-string id) (or (getf chat :from-name) "?")) choices))))
  (nreverse choices))

;;; The declaration. Types and constraints are what START-CHANNEL reads and
;;; RESOLVE-CHANNEL-SECRET / REQUIRE-NON-EMPTY-ALLOWLIST enforce; the kit
;;; refuses on SECTION-PROBLEMS before either runs, so the refusal, the
;;; panel and the primer say the same thing in the same words.
(nlk:define-section ("channels" "telegram")
  (:guide "message @BotFather: /newbot, then copy the token it answers with — it is kept in a file or an environment variable, never in the config; for a group, /setprivacy Disable lets the bot read every group message (else keep require_mention true and mention it), then add the bot to the group; send the bot a message first: a chat is picked by name only once it has written to the bot, and not while a running lane is reading it.")
  (:check #'probe-channel)
  (:one-of "token_env" "token_file")
  (:any-of "allowed_chats" "allowed_users")
  ("token_env" :env :doc "environment variable holding the bot token")
  ("token_file" :path :doc "file holding the bot token")
  ("allowed_chats" :list :doc "chat ids; a group's is negative, a private chat's is the user's id"
                   :choices #'probe-chat-choices)
  ("allowed_users" :list :doc "user ids allowed to drive the bot"
                   :choices #'probe-user-choices)
  ("board_chat" :string
                :choices #'probe-chat-choices
                :doc "a chat the bot keeps one message in, edited in place: every open forum topic it opened, by who moves next (needs you, no answer, running, done); a done topic closes after 48 hours quiet, any other after 7 days")
  ("owner" :list :doc "the operator's user id, whose word is standing policy in the chat"
           :choices #'probe-user-choices)
  ("pairing" :boolean :default t
             :doc "a private chat from someone not allowed is answered with a pairing code, and the operator is told; /channels pair CODE lets them in")
  ("allowed_roles" :list
                   :doc "a group member's standing that lets them drive the bot as a listed user does: administrator (every admin, the creator too) or creator; read off the group's admin list every five minutes, and nothing in a private chat")
  ("allowed_threads" :list :doc "forum topic ids; empty admits every topic")
  ("require_mention" :boolean :default t
                     :doc "in groups, answer when @mentioned or replied to")
  ("free_response_chats" :list :choices #'probe-chat-choices
                         :doc "chat ids where every allowed message may open a turn, mention or not (the bot must read every group message: /setprivacy Disable)")
  ("require_mention_chats" :list :choices #'probe-chat-choices
                           :doc "chat ids where a message must mention or reply to the bot even when require_mention is false")
  ("ignored_chats" :list :choices #'probe-chat-choices
                   :doc "chat ids the bot reads neither as asks nor as context")
  ("mention_patterns" :list
                      :doc "case-insensitive wake words that address the bot as its @handle does, anywhere in a line")
  ("other_addressees" :list
                      :doc "names other than the bot's that a line may open by addressing (\"ada, ...\"); such a line is observed, never answered")
  ("thread_behavior" :choice :options '("create_per_message" "reply_in_place" "disabled")
                     :default "create_per_message"
                     :doc "in a forum group where the bot may manage topics, an ask opens a topic of its own named from its first words (create_per_message); reply_in_place and disabled answer where it was typed; groups that are no forum and private chats always answer in place")
  ("flat_channels" :list :choices #'probe-chat-choices
                   :doc "forum chat ids where asks are answered in place even with create_per_message")
  ("catch_up_minutes" :integer :default 60
                      :doc "after the bot was away — a restart, a machine that was off — answer the asks Telegram kept from the last this-many minutes, each marked as late, and skip older ones and any already answered; 0 answers none of them")
  ("max_concurrent_turns" :integer :default 4
                          :doc "turns that may run at once across every chat; more wait their turn")
  ("status_update_ms" :integer :default 2000
                      :doc "how often a running turn's card is edited, in ms")
  ("text_chunk_limit" :integer :default 16384
                      :doc "the most characters one message carries; a longer answer comes in parts")
  ("delivery_workers" :integer :default 4
                      :doc "threads that post to Telegram at once")
  ("bot_username" :string :doc "hydrated from getMe when unset")
  ("reactions" :boolean :default :false
               :doc "mark each ask on the message itself: 👀 from admission through the working turn, cleared when the turn ends")
  ("stream" :boolean :default t
            :doc "the model's words reach the chat as it writes them, a message edited as it grows; the answer still posts fresh, notifying its asker, and its draft goes; false, each round's words arrive whole")
  ("voice_replies" :choice :options '("off" "on" "tts") :default "off"
                   :doc "an answer comes as a voice message too, below its words: on, to an ask said in a voice message; tts, to every ask; off, never")
  ("turn_budget_minutes" :integer :default 0
                        :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("room_tokens" :integer :default 40000
                :doc "the most history the chat keeps, in estimated tokens (the provider counts about a quarter more); past it the older half is evicted, so every ask opens on at most this much; 0 keeps everything")
  ("soul_file" :path :doc "a SOUL.md whose text is the chat's standing persona"))
