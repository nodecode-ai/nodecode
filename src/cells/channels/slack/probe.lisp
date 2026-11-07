;;;; probe.lisp --- what the tokens can see, and how the operator gets them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) resolves both secrets the way START-CHANNEL does,
;;;; asks Slack who the bot is, whether the app token opens a socket, which
;;;; conversations the bot is in and who is in the workspace, and answers a
;;;; text -- never a token -- so channel and user ids are picked by name from
;;;; the conversation. The section declaration at the end is the adapter's one
;;;; statement of what channels.slack is made of (NLK:DEFINE-SECTION): the
;;;; kit's refusal, the setup panel, the model's setup primer and the addons
;;;; report all read it.

(in-package #:nodecode-channel-slack)

(nlk:access (result execution))

(defparameter +probe-shown+ 30
  "How many channels or people one probe line names before it counts the rest.")

(defun probe-executors (section executor)
  "(values BOT APP): the executors the bot token and the app token speak
through; EXECUTOR, the scripted test seam, stands for both."
  (if executor
      (let ((wrapped (wrap-slack-executor executor)))
        (values wrapped wrapped))
      (let ((api-base (config-string section "api_base" +slack-api-base+)))
        (values (make-slack-executor :api-base api-base
                                     :token (resolve-channel-secret section "bot_token"))
                (make-slack-executor :api-base api-base
                                     :token (resolve-channel-secret section "app_token"))))))

(defun probe-reads (section executor timeout)
  "(values ME SOCKET CONVERSATIONS PEOPLE), each an execution: auth.test,
apps.connections.open (a ticket nobody uses), and -- when the bot token
answered -- the conversations the bot is in and the workspace's people."
  (multiple-value-bind (bot app) (probe-executors section executor)
    (flet ((read-plan (path label)
             (execute-plan bot (rest-plan "GET" path label nil timeout))))
      (let* ((me (execute-plan bot (api-plan "auth.test" nil "auth_test" :timeout-seconds timeout)))
             (socket (execute-plan app (api-plan "apps.connections.open" nil "connections_open"
                                                 :timeout-seconds timeout))))
        (values me socket
                (and (execution-ok-p me) (read-plan "/users.conversations?types=public_channel,private_channel,mpim,im&exclude_archived=true&limit=200"
                                          "conversations"))
                (and (execution-ok-p me) (read-plan "/users.list?limit=200" "people")))))))

(defun probe-people (people)
  "PEOPLE's answer as (ID . NAME) conses: the humans, neither bots nor
deactivated, in the order Slack listed them."
  (loop for user across (nlk:json-array (and people (execution-body people)) "members")
        unless (or (nlk:json-value user :boolean "is_bot")
                   (nlk:json-value user :boolean "deleted")
                   (equal "USLACKBOT" (nlk:json-value user :string "id")))
          collect (cons (nlk:json-value user :string "id")
                        (or (nlk:json-value user :text "profile" "display_name")
                            (nlk:json-value user :text "real_name")
                            (nlk:json-value user :text "name")))))

(defun probe-conversations (conversations people)
  "CONVERSATIONS' answer as (ID . LABEL) conses: a channel by its #name,
private ones said so, a DM by the person it is with."
  (loop for channel across (nlk:json-array (and conversations (execution-body conversations))
                                           "channels")
        for id = (nlk:json-value channel :string "id")
        collect (cons id
                      (cond ((nlk:json-value channel :boolean "is_im")
                             (let ((user (nlk:json-value channel :string "user")))
                               (format nil "DM with ~a" (or (cdr (assoc user people :test #'equal))
                                                            user))))
                            ((nlk:json-value channel :boolean "is_mpim") "group DM")
                            (t (format nil "#~a~:[~; (private)~]"
                                       (nlk:json-value channel :string "name")
                                       (nlk:json-value channel :boolean "is_private")))))))

(defun token-problem (me socket)
  "Why the tokens do not serve a lane, or NIL."
  (flet ((why (result noun hint)
           (unless (execution-ok-p result)
             (format nil "~a: ~a~@[ -- ~a~]" noun (probe-failure result)
                     (and (search "invalid_auth" (or result.error "")) hint)))))
    (or (why me "bot token" "copy the Bot User OAuth Token (xoxb-) from OAuth & Permissions")
        (why socket "app token"
             "generate an App-Level Token (xapp-) with connections:write under Basic Information"))))

(defun probe-channel (section &key executor (timeout 15) &aux (lines '()))
  "What the tokens in SECTION can see, as lines: the bot's identity, whether
Socket Mode answers, the conversations the bot is in and the people in the
workspace."
  ;; EXECUTOR overrides the live ones (the scripted test seam). Read only: the
  ;; socket ticket is never dialed. The answer never carries a token (the
  ;; executors redact every failure text).
  (flet ((say (control &rest args)
           (push (apply #'format nil control args) lines))
         (named (pairs)
           (format nil "~{~a~^, ~}~@[, +~d more~]"
                   (mapcar (lambda (pair) (format nil "~a ~a" (cdr pair) (car pair)))
                           (subseq pairs 0 (min (length pairs) +probe-shown+)))
                   (and (> (length pairs) +probe-shown+) (- (length pairs) +probe-shown+)))))
    (multiple-value-bind (me socket conversations people) (probe-reads section executor timeout)
      (when (execution-ok-p me)
        (say "bot token: ok -- <@~a> in workspace ~a (~a)"
             (nlk:json-value (execution-body me) :string "user_id")
             (nlk:json-value (execution-body me) :string "team")
             (nlk:json-value (execution-body me) :string "team_id")))
      (when (execution-ok-p socket)
        (say "app token: ok -- Socket Mode answers"))
      (nlk:when-let (problem (token-problem me socket))
        (say "~a" problem))
      (when conversations
        (let* ((people (and people (execution-ok-p people) (probe-people people)))
               (rooms (and (execution-ok-p conversations)
                           (probe-conversations conversations people))))
          (say "~a" (cond ((not (execution-ok-p conversations))
                           (format nil "channels: ~a" (probe-failure conversations)))
                          ((null rooms)
                           "channels: none yet -- /invite the bot into a channel, or open a DM with it and say something")
                          (t (format nil "channels: ~a" (named rooms)))))
          (when people
            (say "people: ~a" (named people))))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun probe-choices (section executor timeout reader)
  "READER's (ID . LABEL) conses over PROBE-READS' answer, refused in the
probe's words when the tokens do not serve."
  (multiple-value-bind (me socket conversations people) (probe-reads section executor timeout)
    (nlk:when-let (problem (token-problem me socket))
      (config-error "~a" problem))
    (funcall reader conversations (and people (execution-ok-p people) (probe-people people)))))

(defun probe-channel-choices (section &key executor (timeout 15))
  "The conversations the bot is in as ((id . label) ...) -- what a setup
panel offers for allowed_channels."
  (probe-choices section executor timeout
                 (lambda (conversations people)
                   (and conversations (execution-ok-p conversations)
                        (probe-conversations conversations people)))))

(defun probe-user-choices (section &key executor (timeout 15))
  "The workspace's people as ((id . name) ...) -- what a setup panel offers
for allowed_users and owner."
  (probe-choices section executor timeout
                 (lambda (conversations people)
                   (declare (ignore conversations))
                   people)))

;;; The declaration. Types and constraints are what START-CHANNEL reads and
;;; RESOLVE-CHANNEL-SECRET / REQUIRE-NON-EMPTY-ALLOWLIST enforce; the kit
;;; refuses on SECTION-PROBLEMS before either runs, so the refusal, the panel
;;; and the primer say the same thing in the same words.
(nlk:define-section ("channels" "slack")
  (:guide "api.slack.com/apps: Create New App, From a manifest, pick the workspace and paste manifest.json from this add-on's folder -- it turns on Socket Mode, the bot user, the Messages tab, the scopes, the events and the /nodecode command; Install to Workspace, then copy the Bot User OAuth Token (xoxb-) from OAuth & Permissions; under Basic Information, App-Level Tokens, generate one with the connections:write scope and copy it (xapp-); both are kept in files or environment variables, never in the config; /invite the bot into a channel, or open a DM with it; with the tokens saved, channels and people are picked by name.")
  (:check #'probe-channel)
  (:one-of "bot_token_env" "bot_token_file")
  (:one-of "app_token_env" "app_token_file")
  (:any-of "allowed_channels" "allowed_users")
  ("bot_token_env" :env :doc "environment variable holding the bot token (xoxb-)")
  ("bot_token_file" :path :doc "file holding the bot token (xoxb-)")
  ("app_token_env" :env :doc "environment variable holding the app-level token (xapp-)")
  ("app_token_file" :path :doc "file holding the app-level token (xapp-)")
  ("allowed_channels" :list :doc "conversation ids the bot answers in; a DM's is its D id"
                      :choices #'probe-channel-choices)
  ("allowed_users" :list :doc "user ids allowed to drive the bot"
                   :choices #'probe-user-choices)
  ("owner" :list :doc "the operator's user id, whose word is standing policy in the channel"
           :choices #'probe-user-choices)
  ("require_mention" :boolean :default t
                     :doc "in channels, answer when @mentioned or in a thread the bot answers in; a DM always answers")
  ("allow_bots" :choice :options '("none" "mentions" "all") :default "none"
                :doc "other bots' messages: none ignores them, mentions answers one that @mentions this bot, all treats them as people's; the bot's own are always ignored")
  ("free_response_channels" :list :choices #'probe-channel-choices
                            :doc "channel ids where every allowed message may open a turn")
  ("require_mention_channels" :list :choices #'probe-channel-choices
                              :doc "channel ids where a message must mention the bot, whatever require_mention says")
  ("ignored_channels" :list :choices #'probe-channel-choices
                      :doc "channel ids the bot reads neither as prompts nor as context")
  ("allowed_workspaces" :list
                        :doc "workspace ids (T...) whose people may drive the bot; empty admits anyone the other lists admit -- set it when a channel is shared with another organization (Slack Connect)")
  ("mention_patterns" :list
                      :doc "case-insensitive wake words that address the bot where a mention is required")
  ("mention_groups" :list
                    :doc "user group ids (S...) whose mention addresses the bot where a mention is required, as its own does")
  ("dm_policy" :choice :options '("allow" "disabled") :default "allow"
               :doc "direct messages")
  ("group_policy" :choice :options '("allow" "disabled") :default "allow"
                  :doc "group direct messages")
  ("reactions" :boolean :default :false
               :doc "mark each ask on the message itself: 👀 from admission through the working turn, then ✅ when its answer lands or ❌ when it fails; needs reactions:write")
  ("board_channel" :string :choices #'probe-channel-choices
                   :doc "a channel the bot keeps one message in, edited in place: every thread it answers in, by who moves next (needs you, no answer, running, done), named by its ask and linked; a done thread leaves after 48 hours quiet, any other after 7 days; DM threads never show")
  ("catch_up_minutes" :integer :default 60
                      :doc "after the bot was away -- a restart, a machine that was off -- read what its conversations said in the last this-many minutes of it and answer the asks it missed, each marked as late; 0 never")
  ("show_tools" :boolean :default :false
                :doc "show the turn's calls in the thread as their own messages -- the trail the transcript walks; false, the thread shows the turn's own words only")
  ("turn_budget_minutes" :integer :default 0
                         :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona"))
