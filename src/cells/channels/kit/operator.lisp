;;;; operator.lisp --- what the operator sets for the rooms: per channel, who
;;;; is let in, and where the bot reports.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four controls, each Hermes had and a room lacked:
;;;;
;;;;   agents, routes    channels.<id>.agents names the agents a room's asks
;;;;                     may run as — each its folder, persona, standing
;;;;                     prompt, model and skills — and channels.<id>.routes
;;;;                     sends asks to one by server, channel, thread, person
;;;;                     or role; /agent typed in a room hands the room to one
;;;;                     (after Hermes' profile routes and OpenClaw's
;;;;                     bindings).
;;;;   the room's model  /models typed in a room sets that room's model and
;;;;                     no other: every lane forked there runs on it, a
;;;;                     thread's on its channel's unless it has its own. The
;;;;                     organism's default, which the shells' /models moves,
;;;;                     stays where it was — before, a pick in a room moved it
;;;;                     for the whole organism, the operator's terminal
;;;;                     included.
;;;;   pairing           a direct message from someone no allowlist names is
;;;;                     answered with a pairing code; the person gives it to
;;;;                     the operator, and /channels pair CODE lets them in.
;;;;                     The code is never in a notice or a log: it is what
;;;;                     proves the person asking the operator is the one who
;;;;                     wrote to the bot (Hermes' rule). The web page lists
;;;;                     the asks without it and approves one by its id, as
;;;;                     Hermes' admin page does (PAIRING-JSON).
;;;;   the home channel  /sethome typed in a room makes it where the bot
;;;;                     reports to the operator: that it is back after a
;;;;                     restart, the release it runs once it is new, who
;;;;                     asked to be let in, who was refused.
;;;;
;;;; The room's agent and model, the paired people and the home channel are
;;;; kept in the host's own state session (HANDOFF-SESSION), so a restart
;;;; keeps them; the agents and routes are config, read at each start.

(in-package #:nodecode-channel-kit)

;;; --- agents and routes ----------------------------------------------------------------------
;;; An agent is what a room's asks run as, short of a whole other organism:
;;; the folder its lanes work in — their tools' directory, the AGENTS.md they
;;; read, the project memory they keep — its persona, its standing prompt, its
;;; model and the skills it reads first. This process runs every one, as
;;; Hermes' multiplexed profiles and OpenClaw's agents run in theirs; a whole
;;; other home is a profile, an organism of its own with a bot of its own.
;;;
;;; A route sends asks to an agent by where they were said and who said them:
;;; a server (guild), a channel and its threads, one thread, a person (user),
;;; a role. Every key a route names must match, and the most specific route
;;; that does wins: person, then thread, channel, role, server, each
;;; outweighing every key below it together — Hermes' additive rule
;;; (gateway/profile_routing.py), with roles where OpenClaw ranks them, above
;;; the server and below the channel — and a tie goes to the route written
;;; first. An ask no route takes runs as the channel itself: its soul_file,
;;; no standing prompt, the organism's model, the room's folder.

(defparameter +agent-keys+ '("folder" "soul_file" "prompt" "provider" "model" "skills")
  "What an agents entry may name.")

(defparameter +route-keys+ '(("user" :user 16) ("thread" :thread 8) ("channel" :channel 4)
                             ("role" :role 2) ("guild" :guild 1))
  "What a route may match on, as (KEY KEYWORD WEIGHT): a route's specificity
is the sum of the weights of the keys it names.")

(defparameter +own-agent+ "default"
  "The agent name that names none: the channel itself.")

(defun read-agents (channel-id section)
  "channels.<CHANNEL-ID>.agents in SECTION as ((NAME . PLIST) ...), each PLIST
(:folder :soul-path :prompt :provider :model :skills); a malformed entry is a
CONFIG-REFUSAL naming it."
  (let ((table (and (hash-table-p section) (gethash "agents" section))))
    (cond ((null table) '())
          ((not (hash-table-p table))
           (config-error "channels.~a.agents is an object keyed by agent name" channel-id))
          (t (loop for name being the hash-keys of table using (hash-value entry)
                   collect (cons name (agent-plist channel-id name entry)))))))

(defun agent-plist (channel-id name entry)
  "One agents ENTRY, under NAME, as a plist."
  (flet ((refuse (control &rest arguments)
           (config-error "channels.~a.agents.~a: ~?" channel-id name control arguments)))
    ;; One word, so /agent NAME can say it.
    (when (or (string-equal name +own-agent+) (not (ppcre:scan "^[\\w.-]+\\z" name)))
      (refuse "an agent's name is one word, and not ~a" +own-agent+))
    (unless (hash-table-p entry)
      (refuse "an object of ~{~a~^, ~}" +agent-keys+))
    (loop for key being the hash-keys of entry
          unless (member key +agent-keys+ :test #'string=)
            do (refuse "no setting ~s; one of ~{~a~^, ~}" key +agent-keys+))
    (let ((provider (config-string entry "provider"))
          (model (config-string entry "model"))
          (folder (config-string entry "folder")))
      ;; A provider alone would run the organism's model under another lane.
      (when (and provider (not model))
        (refuse "a provider needs its model"))
      (list :folder (and folder
                         ;; A lane in a folder that is not there fails its
                         ;; every call, so the start refuses it instead.
                         (handler-case (nlk:check-session-cwd folder)
                           (error (condition) (refuse "folder: ~a" condition))))
            :soul-path (config-string entry "soul_file")
            :prompt (config-string entry "prompt")
            :provider provider
            :model model
            :skills (config-string-list entry "skills")))))

(defun read-routes (channel-id section agents)
  "channels.<CHANNEL-ID>.routes in SECTION as plists (:agent :weight and the
keys each matches on), the most specific first and ties in the order
written. :AGENT is a name of AGENTS, or NIL for the channel itself; a route
that names no agent of AGENTS, or nothing to match, is a CONFIG-REFUSAL
naming it."
  (let ((routes (and (hash-table-p section) (gethash "routes" section))))
    (cond ((null routes) '())
          ((not (vectorp routes))
           (config-error "channels.~a.routes is a list of routes" channel-id))
          (t (stable-sort (loop for route across routes
                                for index from 0
                                collect (route-plist channel-id index route agents))
                          #'> :key (lambda (route) (getf route :weight)))))))

(defun route-plist (channel-id index route agents &aux (keys (mapcar #'first +route-keys+)))
  "One ROUTES entry, the INDEXth, as a plist."
  (flet ((refuse (control &rest arguments)
           (config-error "channels.~a.routes[~d]: ~?" channel-id index control arguments)))
    (unless (hash-table-p route)
      (refuse "an object of an agent and one or more of ~{~a~^, ~}" keys))
    (loop for key being the hash-keys of route
          unless (or (string= key "agent") (member key keys :test #'string=))
            do (refuse "no key ~s; agent, and one or more of ~{~a~^, ~}" key keys))
    (let ((agent (config-string route "agent"))
          (matches '())
          (weight 0))
      (unless (or (equal agent +own-agent+) (assoc agent agents :test #'string=))
        (refuse "~:[names no agent~;~:*no agent ~s~]; channels.~a.agents defines ~:[none~;~:*~{~a~^, ~}~], ~
                 and ~s is the channel itself"
                agent channel-id (mapcar #'car agents) +own-agent+))
      (loop for (key keyword key-weight) in +route-keys+
            for value = (gethash key route)
            when value
              do (unless (stringp value)
                   ;; A Discord id is past what a JSON number holds exactly.
                   (refuse "~a is an id, written as a string" key))
                 (setf matches (list* keyword (config-string route key) matches))
                 (incf weight key-weight))
      (when (zerop weight)
        (refuse "matches nothing: name one or more of ~{~a~^, ~}" keys))
      (list* :agent (if (equal agent +own-agent+) nil agent) :weight weight matches))))

(defun route-matches-p (route source target)
  "Whether every key ROUTE names matches an ask said at TARGET by the person
SOURCE — a candidate's source, or NIL where nobody is known — describes."
  ;; A thread's TARGET names its channel as the parent's, so a channel's
  ;; route takes its threads.
  (flet ((said (key) (and (hash-table-p source) (gethash key source))))
    (loop for (key value) on route by #'cddr
          always (ecase key
                   ((:agent :weight) t)
                   (:user (equal value (said "user_id")))
                   (:thread (equal value (getf target :thread-id)))
                   (:channel (equal value (getf target :channel-id)))
                   (:role (let ((roles (said "role_ids")))
                            (and (typep roles 'sequence) (find value roles :test #'equal))))
                   (:guild (equal value (said "workspace_id")))))))

(defun route-text (route)
  "ROUTE in words: the keys it matches on."
  (format nil "~{~a~^ and ~}"
          (loop for (key keyword) in +route-keys+
                for value = (getf route keyword)
                when value collect (format nil "~a ~a" key value))))

(defun agent-of (host name)
  "HOST's agent NAME as its plist, or NIL: the channel itself."
  (and name (cdr (assoc name host.agents :test #'string=))))

(defun agent-standing (agent noun)
  "What AGENT — an agent's plist, or NIL for the channel itself — sets for
the lanes it runs as the lane contract's last paragraph — its prompt, then
the skills to view — or NIL when it sets neither."
  ;; Standing, not live: it is a fact of the room every lane there shares, so
  ;; it rides with the contract ahead of the history (LANE-CONTRACT).
  (let ((prompt (getf agent :prompt))
        (skills (getf agent :skills)))
    (when (or prompt skills)
      (with-output-to-string (out)
        (when prompt
          (format out "The operator's standing instructions for this ~a, which bind every ask in it:~%~a"
                  noun prompt))
        (when (and prompt skills) (format out "~%~%"))
        (when skills
          (format out "The operator bound these skills to this ~a: ~{~a~^, ~}. Read each — ~
                       (help 'NAME) — before you answer the first ask here, and follow it."
                  noun skills))))))

;;; --- what a room set for itself: its agent, its model, its voice ---------------------------

(defparameter +room-agents-key+ "room-agents"
  "The state key the agents /agent handed rooms to are kept under.")

(defparameter +room-models-key+ "room-models"
  "The state key the models /models set in rooms are kept under.")

(defparameter +room-efforts-key+ "room-efforts"
  "The state key the reasoning effort /think sets in rooms are kept under.")

(defparameter +room-voices-key+ "room-voices"
  "The state key the voice replies /voice set in rooms are kept under.")

(defvar *room-settings-lock* (bt2:make-lock :name "channel-room-settings")
  "Serializes a read-change-write of a host's room settings.")

(defun room-settings (host key)
  "HOST's rooms' settings kept under the state KEY, as a hash: room session
id -> value."
  (or (and (nlk:store-open-p)
           (nlk:when-let (json (nlk:session-state-get (handoff-session host) key))
             (let ((table (nlk:decode-json json))) (and (hash-table-p table) table))))
      (make-hash-table :test #'equal)))

(defun set-room-setting (host key room value)
  "Keep VALUE as ROOM's setting under KEY; VALUE NIL clears ROOM's."
  (bt2:with-lock-held (*room-settings-lock*)
    (let ((table (room-settings host key)))
      (if value (setf (gethash room table) value) (remhash room table))
      (nlk:session-state-put (handoff-session host) key (nlk:encode-json-object table)))))

(defun agent-for (host room target source)
  "(values NAME WHY ROUTE): the agent asks in ROOM at TARGET, said by the
person SOURCE describes, run as — the one /agent handed ROOM to (WHY :room),
else the one it handed the channel room a thread hangs in (WHY :parent),
else the most specific route that matches (WHY :route, ROUTE it) — or NIL,
the channel itself."
  ;; A room handed to an agent the config no longer defines goes back to the
  ;; routes.
  (loop with set = (room-settings host +room-agents-key+)
        for (room why) in (list (list room :room) (list (room-parent-session-id host target) :parent))
        for name = (and room (gethash room set))
        when (agent-of host name)
          do (return-from agent-for (values name why)))
  (nlk:when-let (route (find-if (lambda (route) (route-matches-p route source target)) host.routes))
    (values (getf route :agent) :route route)))

(defun agent-command (args room &aux (words (nlk:split-words (or args ""))))
  "/agent ARGS typed in ROOM: which agent answers there and why; NAME hands
ROOM to that agent, default gives it back to the routes."
  ;; Handing a room over retires its lanes and its threads', as a head move
  ;; does: a lane works in its agent's folder, and a direct message's next
  ;; line would otherwise go on with the agent it left.
  (multiple-value-bind (host target) (room-host room)
    (unless host (error "~a is no room of a running channel" room))
    (agent-report host room target words)))

(defun agent-choices (text room)
  "The /agent argument completions for the tail TEXT typed in ROOM: its
channel's agents in the order the config names them, each beside its folder,
then default — none outside a channel's room."
  (nlk:when-let (host (room-host room))
    (loop with needle = (nlk:trimmed text)
          for (name . agent) in (append host.agents (list (list +own-agent+)))
          when (search needle name :test #'char-equal)
            collect (list :name (cond ((equal name +own-agent+) "default — back to the routes")
                                      ((getf agent :folder)
                                       (format nil "~a — ~a" name (getf agent :folder)))
                                      (t name))
                          :value name))))

(defun agent-report (host room target words)
  "AGENT-COMMAND's answer to WORDS in HOST's ROOM at TARGET."
  (let ((here (if (getf target :thread-id) "thread" "channel"))
        (names (mapcar #'car host.agents)))
    (flet ((who (name) (if name (format nil "agent ~a" name) "the channel itself"))
           (hand (name)
             (set-room-setting host +room-agents-key+ room name)
             (retire-lanes host (lambda (parent)
                                  (or (equal room parent)
                                      (equal room (room-parent-session-id
                                                   host (session-target (platform-session-prefix host.platform)
                                                                        parent)))))))
           (now () (agent-for host room target *command-source*)))
      (cond
        ((null words)
         (multiple-value-bind (name why route) (now)
           (format nil "this ~a is answered by ~a~a~%~:[no agents: channels.~a.agents defines them~;~
                        ~:*agents: ~{~a~^, ~}; /agent NAME hands this ~a to one~]"
                   here (who name)
                   (case why
                     (:room ", handed to it with /agent; /agent default gives it back")
                     (:parent ", as its channel was handed with /agent")
                     (:route (format nil ", by the route on ~a" (route-text route)))
                     (t ""))
                   names (if names here (host-id host)))))
        ((rest words) "usage: /agent [NAME | default]")
        ((string-equal (first words) +own-agent+)
         (hand nil)
         (format nil "this ~a is answered by ~a from its next ask" here (who (now))))
        ((null (agent-of host (first words)))
         (error "no agent ~a; ~:[channels.~a.agents defines none~;~:*the agents are ~{~a~^, ~}~]"
                (first words) names (host-id host)))
        (t (hand (first words))
           (format nil "this ~a is answered by ~a from its next ask" here (who (first words))))))))

(defun room-model (host room)
  "(values PROVIDER MODEL) /models set in ROOM, or NIL."
  (nlk:when-let (entry (and room (gethash room (room-settings host +room-models-key+))))
    (values (nlk:json-value entry :string "provider") (nlk:json-value entry :string "model"))))

(defun set-room-model (host room provider model)
  "Make ROOM run on PROVIDER's MODEL; MODEL NIL clears ROOM's."
  (set-room-setting host +room-models-key+ room
                    (and model (nlk:json-object "provider" provider "model" model))))

(defun room-effort (host room)
  "ROOM's reasoning-effort pick, or NIL when it follows the provider default."
  (and room (gethash room (room-settings host +room-efforts-key+))))

(defun set-room-effort (host room effort)
  "Make ROOM use EFFORT from its next ask; NIL follows the provider default."
  (set-room-setting host +room-efforts-key+ room effort))

(defun lane-effort (host room target)
  "ROOM's effort pick, inherited by a thread from its channel, or NIL."
  (loop for candidate in (list room (room-parent-session-id host target))
        for effort = (room-effort host candidate)
        when effort do (return effort)))

(defun effort-target (host room target agent)
  "(values PROVIDER MODEL LADDER): the target ROOM's next lane binds."
  (multiple-value-bind (provider model) (lane-model host room target agent)
    (unless model
      (multiple-value-setq (provider model) (nle:gateway-target nil)))
    (values provider model (nle::effort-ladder provider model))))

(defun apply-lane-effort (host ask)
  "Apply ASK's room effort after its room model has been applied."
  (let ((effort (lane-effort host ask.room ask.target)))
    (multiple-value-bind (provider model) (nlk:session-model-selection ask.lane)
      (unless (equal effort (nlk:session-model-effort ask.lane))
        (nlk:record-session-model-selection ask.lane
                                            :provider provider
                                            :model model
                                            :effort effort)))))

(defun lane-model (host room target agent)
  "(values PROVIDER MODEL WHY) a lane of ROOM at TARGET, run as AGENT (a
plist, or NIL), runs on: what /models set in ROOM, else in the channel room a
thread hangs in (WHY :room), else AGENT's model (WHY :agent), else NIL — the
organism's default."
  (dolist (room (list room (room-parent-session-id host target)))
    (multiple-value-bind (provider model) (room-model host room)
      (when model (return-from lane-model (values provider model :room)))))
  (nlk:when-let (model (getf agent :model))
    (values (getf agent :provider) model :agent)))

(defun pin-room-model (host session room target agent)
  "Pin SESSION to the model ROOM at TARGET, run as AGENT, runs on
(LANE-MODEL), or clear its pin to follow the organism's default, when its
pin says otherwise."
  (multiple-value-bind (provider model) (lane-model host room target agent)
    (multiple-value-bind (pinned-provider pinned-model) (nlk:session-model-selection session)
      (unless (and (equal provider pinned-provider) (equal model pinned-model))
        (nlk:record-session-model-selection session :provider provider :model model)))))

(defun apply-lane-model (host ask)
  "Pin ASK's lane to the model its room runs on (PIN-ROOM-MODEL)."
  ;; A lane is a session of its own, forked from the room, and a pin does not
  ;; ride a fork: the lane carries it. A lane still standing by holds the pin
  ;; until the ask's admission makes it durable, which is before its turn
  ;; binds a model.
  (pin-room-model host ask.lane ask.room ask.target (agent-of host ask.agent)))

(defun lane-voice-replies (host room target)
  "Whether a lane of ROOM at TARGET answers in a voice message too, as a key
of +VOICE-REPLIES+: what /voice set in ROOM, else in the channel room a
thread hangs in, else the section's voice_replies."
  (or (loop with set = (room-settings host +room-voices-key+)
            for room in (list room (room-parent-session-id host target))
            thereis (and room (gethash room set)))
      host.voice-replies))

(defun room-voice-replies (room)
  "The key of +VOICE-REPLIES+ ROOM — a room session of a running channel's —
answers with (LANE-VOICE-REPLIES), or NIL for a session no channel holds."
  (multiple-value-bind (host target) (room-host room)
    (and host (lane-voice-replies host room target))))

(defun room-lane-model (room)
  "(values PROVIDER MODEL) ROOM -- a room session of a running channel's --
runs on (LANE-MODEL, its agent aside), or NIL: the organism's default."
  (multiple-value-bind (host target) (room-host room)
    (and host (lane-model host room target nil))))

(defun set-voice-replies (room mode &aux (host (room-host room)))
  "Make ROOM — a room session of a running channel's — answer in voice
messages as MODE, a key of +VOICE-REPLIES+, says. => the line that says so."
  ;; Kept even when it is what the section says: the room chose it, and a
  ;; section changed later does not change the room's mind.
  (let ((meaning (cdr (assoc mode +voice-replies+ :test #'equal))))
    (cond ((null meaning)
           (error "voice replies are ~{~a~^, ~}, not ~s" (mapcar #'car +voice-replies+) mode))
          ((null host) (error "~a is no room of a running channel" room))
          (t (set-room-setting host +room-voices-key+ room mode)
             (format nil "voice: ~a." meaning)))))

(defun model-provider (model &aux (found (loop for (provider . nil) in (nle:models)
                                                   when (member model (ignore-errors (nle:models provider))
                                                                :test #'string=)
                                                     collect provider)))
  "The configured provider that lists MODEL, or a refusal naming why none
does: no provider lists it, or more than one does."
  (cond ((null found)
         (error "no configured provider lists ~a; /models list shows the catalog" model))
        ((rest found)
         (error "~a is served by ~{~a~^ and ~}; name one: /models ~a ~a"
                model found (first found) model))
        (t (first found))))

(defun checked-pick (provider model)
  "PROVIDER, confirmed to be configured and to list MODEL — its listing
unreachable, the model is taken on its word — or a refusal saying why."
  (multiple-value-bind (ids listing-error) (nle:models provider)
    (unless (or listing-error (member model ids :test #'string=))
      (error "~a lists no model ~a; /models list ~a shows its ~d" provider model provider (length ids)))
    provider))

(defun room-models-command (host candidate args room &aux (words (nlk:split-words (or args "")))
                                                         (target (channel-target candidate))
                                                         (agent (agent-of host (agent-for host room target
                                                                                          (candidate-source candidate))))
                                                         (here (if (getf target :thread-id) "thread" "channel"))
                                                         (cards (platform-choices host.platform)))
  "Worker thread: the room's /models ARGS — the model ROOM runs on, or a
pick of one for ROOM alone; where the platform's messages carry choices, a
picker card, (values TEXT CONTROLS)."
  ;; The catalog is the organism's: `list' and `aux' answer as the shells'
  ;; /models does (NLE:SLASH). A pick names a model, or a provider and a
  ;; model; `default' goes back to what the room ran on before one. With
  ;; cards, bare /models is the picker's first view and a lone provider its
  ;; second (MODELS-CARD), `page N' after either the page a menu shows.
  (flet ((page (word) (or (and word (parse-integer word :junk-allowed t)) 1))
         (paged-p (tail) (or (null tail)
                             (and (string-equal (first tail) "page") (rest tail) (null (cddr tail))))))
    (cond
      ((and words (member (first words) '("list" "aux") :test #'string-equal))
       (nle:slash (format nil "/models ~a" args) :session-id room))
      ((and words (string-equal (first words) "backups"))
       (backups-command (rest words) (multiple-value-list (picker-current host room target agent))))
      ((and cards (paged-p words))
       (providers-card host room target agent (page (second words))))
      ((null words)
       (multiple-value-bind (provider model why) (lane-model host room target agent)
         (if model
             (format nil "this ~a runs ~@[~a/~]~a, ~:[set by its agent~;set with /models~]; ~
                          /models <provider> <model> changes it, /models default goes back"
                     here provider model (eq why :room))
             ;; The organism's report ends saying how a pick is made, and here
             ;; a pick is this room's.
             (format nil "this ~a runs the organism's default:~%~a" here (nle:slash "/models" :session-id room)))))
      ((and (null (rest words)) (member (first words) '("default" "reset") :test #'string-equal))
       (set-room-model host room nil nil)
       (multiple-value-bind (provider model) (lane-model host room target agent)
         (format nil "this ~a runs ~:[the organism's default~;~:*~@[~a/~]~a~] from its next ask"
                 here provider model)))
      ((and (paged-p (rest words)) (assoc (first words) (nle:models) :test #'string-equal))
       (if cards
           (models-card host room target agent (first words) (page (third words)))
           (nle:slash (format nil "/models list ~a" (first words)) :session-id room)))
      ((rest (rest words)) "usage: /models [provider] [model] | default | list [provider]")
      (t
       (let* ((model (first (last words)))
              (provider (if (rest words) (checked-pick (first words) model) (model-provider model)))
              ;; A pick asks its reasoning effort, as the shells' own /models
              ;; does (OPEN-MODELS-REASONING-PICKER): /think stays the quick
              ;; toggle for later, and this is the one ask that follows a
              ;; pick straight through, no /think typed.
              (ladder (and cards (nle::effort-ladder provider model))))
         (set-room-model host room provider model)
         (let ((said (format nil "this ~a runs ~a/~a from its next ask; the organism's default is unchanged"
                             here provider model)))
           (if ladder
               (multiple-value-bind (effort-text controls) (effort-card host room provider model ladder nil)
                 (values (format nil "~a~%~%~a" said effort-text) controls))
               said)))))))

;;; --- the effort picker (a /think card) ---------------------------------------------------

(defun room-effort-command (host candidate args room
                            &aux
                            (words (nlk:split-words (or args "")))
                            (target (channel-target candidate))
                            (agent (agent-of host (agent-for host room target
                                                             (candidate-source candidate))))
                            (here (if (getf target :thread-id) "thread" "channel"))
                            (cards (platform-choices host.platform)))
  "Worker thread: the room's /think ARGS — a reasoning effort for ROOM alone,
or a picker card where the platform carries choices."
  (multiple-value-bind (provider model ladder) (effort-target host room target agent)
    (let ((current (lane-effort host room target)))
      (cond
        ((and cards (null words))
         (effort-card host room provider model ladder current))
        ((null words)
         (format nil "this ~a uses ~a/~a at ~a~%/think <rung> changes it; /think default follows the provider"
                 here provider model (or current "provider default")))
        ((and (null (rest words))
              (member (first words) '("default" "reset") :test #'string-equal))
         (set-room-effort host room nil)
         (format nil "this ~a follows ~a/~a's provider default from its next ask"
                 here provider model))
        ((rest words)
         "usage: /think [off | minimal | low | medium | high | xhigh | max | default]")
        ((null ladder)
         (format nil "~a/~a offers no reasoning control" provider model))
        ((not (member (first words) ladder :test #'string-equal))
         (error "~s is not a reasoning effort ~a/~a offers; the ladder: ~{~a~^ ~}"
                (first words) provider model ladder))
        (t
         (set-room-effort host room (first words))
         (format nil "this ~a uses ~a/~a at ~a from its next ask; the organism's default is unchanged"
                 here provider model (first words)))))))

(defun effort-card (host room provider model ladder current)
  "(values TEXT CONTROLS): the room's reasoning-effort picker."
  (declare (ignore host room))
  (values (format nil "## Reasoning Effort~%Target: ~a/~a~%Current: ~a~%Select the effort for this room's next asks."
                  provider model (or current "provider default"))
          (list (list :menu "Select effort"
                      (loop for rung in ladder
                            collect (menu-choice rung (format nil "/think ~a" rung)
                                                 :description (if (equal rung "off")
                                                                  "Answer without reasoning."
                                                                  "")
                                                 :current (equal rung current))))
                (choice "Provider default" "/think default"
                        :disabled (null current)))))

;;; --- the model picker (a /models card) ---------------------------------------------------
;;; Where a platform's messages carry choices, bare /models answers with a
;;; card after OpenClaw's Discord picker: a menu of the providers, then of
;;; one provider's models, with a way back and a way to the default. Every
;;; step is a CHOICE saying a /models line — /models PROVIDER, /models
;;; PROVIDER MODEL, /models default — so a press is that command typed,
;;; admitted as typed, and its answer takes the card's place. OpenClaw's
;;; Submit step is not copied: picking a model picks it. A menu shows 25; a
;;; longer list pages.

(defparameter +picker-page-size+ 25
  "The most options one picker menu shows, a Discord select's own cap.")

(defun picker-page (items page)
  "(values SHOWN PAGE PAGES): the PAGEth run of +PICKER-PAGE-SIZE+ ITEMS, PAGE
clamped to the PAGES there are."
  (let* ((pages (max 1 (ceiling (length items) +picker-page-size+)))
         (page (min (max 1 page) pages))
         (start (* (1- page) +picker-page-size+)))
    (values (subseq items start (min (length items) (+ start +picker-page-size+))) page pages)))

(defun picker-pager (line page pages)
  "The row that pages a picker's menu — back, where it is, on — each a choice
saying LINE with the page it goes to; NIL for a menu one page holds."
  (when (> pages 1)
    (list (list (choice "Prev" (format nil "~a page ~d" line (1- page)) :disabled (= page 1))
                (list (format nil "Page ~d/~d" page pages) "nck:page" :secondary t)
                (choice "Next" (format nil "~a page ~d" line (1+ page)) :disabled (= page pages))))))

(defun picker-current (host room target agent)
  "(values PROVIDER MODEL): the model ROOM's next ask, run as AGENT, runs on —
LANE-MODEL's pick, else the organism's default."
  (multiple-value-bind (provider model) (lane-model host room target agent)
    (if model
        (values provider model)
        (multiple-value-bind (provider model) (nle:gateway-target nil)
          (values provider model)))))

(defun providers-card (host room target agent page &aux (providers (nle:models)))
  "(values TEXT CONTROLS): the picker's first view — the model ROOM runs on,
and a menu of the providers, each saying /models PROVIDER."
  (multiple-value-bind (current-provider current-model) (picker-current host room target agent)
    (multiple-value-bind (shown page pages) (picker-page providers page)
      (values (format nil "## Model Picker~%Current model: ~@[~a/~]~a~%Select a provider (~d available)."
                      current-provider current-model (length providers))
              (list* (list :menu "Select provider"
                           (loop for (provider . count) in shown
                                 ;; This menu navigates; selecting the current
                                 ;; provider must still open its model list.
                                 collect (menu-choice provider (format nil "/models ~a" provider)
                                                      :description (format nil "~d model~:p~@[ · ~a~]" count
                                                                           (and (equal provider current-provider)
                                                                                "current")))))
                     (append (picker-pager "/models" page pages)
                             (list (list (choice (format nil "Backups (~d)" (length (backups-chain)))
                                                 "/models backups")))))))))

(defun models-card (host room target agent provider page)
  "(values TEXT CONTROLS): the picker's second view — PROVIDER's models, each
saying /models PROVIDER MODEL, then the way back to the providers and the way
to the default, which only a room that picked its own can take."
  (multiple-value-bind (ids listing-error) (nle:models provider)
    (multiple-value-bind (current-provider current-model) (picker-current host room target agent)
      (multiple-value-bind (default-provider default-model) (nle:gateway-target nil)
        (multiple-value-bind (shown page pages) (picker-page ids page)
          (values (format nil "## Model Picker~%Current model: ~@[~a/~]~a~%Default: ~a/~a~%~
                               Select a ~a model~@[ (~a)~].~@[~%listing: ~a~]"
                          current-provider current-model default-provider default-model provider
                          (and (> pages 1) (format nil "page ~d/~d, ~d models" page pages (length ids)))
                          listing-error)
                  (append (list (list :menu (format nil "Select ~a model" provider)
                                      (loop for id in shown
                                            collect (menu-choice id (format nil "/models ~a ~a" provider id)
                                                                 :current (and (equal id current-model)
                                                                               (equal provider current-provider))))))
                          (picker-pager (format nil "/models ~a" provider) page pages)
                          (list (list (choice "Providers" "/models")
                                      ;; ROOM's own pick: a thread running its
                                      ;; channel's has nothing to reset.
                                      (choice "Reset to default" "/models default"
                                              :disabled (null (nth-value 1 (room-model host room)))))))))))))

;;; --- the backups (a /models card) --------------------------------------------------------
;;; The failover chain is the organism's: every room and shell shares one. The
;;; Model Picker's Backups button opens it as a card -- a menu per backup, each
;;; option saying what history knows of that model, the triggers that send a
;;; failing round to the chain as toggles, and a test that asks each backup
;;; once. Every control says a /models backups line, so a press is that line
;;; typed and the card it answers takes the pressed one's place. The card is a
;;; view of failover.chain and failover.on, read afresh and written through
;;; NLE's own setters, never a copy of them.

(defparameter +backup-slots+ 3
  "The backups the card offers, a menu each: with the triggers' row and the
card's own buttons, Discord's five rows.")

(defparameter +backup-candidates+ 20
  "The most models a backup's menu offers besides its own and None.")

(defun backups-chain ()
  "The failover chain, (PROVIDER MODEL) lists in order."
  (mapcar (lambda (entry) (list (getf entry :provider) (getf entry :model)))
          (nle::failover-chain)))

(defun backup-when (iso &aux (universal (and iso (nle::iso-universal iso))))
  "How long ago ISO, a stored time, was, as a card says it: no clock reading,
so every reader's timezone reads it alike."
  (when universal
    (let ((minutes (floor (max 0 (- (get-universal-time) universal)) 60)))
      (cond ((< minutes 1) "just now")
            ((< minutes 60) (format nil "~d min ago" minutes))
            ((< minutes (* 48 60)) (format nil "~d h ago" (floor minutes 60)))
            (t (format nil "~d days ago" (floor minutes (* 24 60))))))))

(defvar *backup-records* nil
  "While a card is drawn, a table of the records it has read, keyed (PROVIDER
MODEL): its three menus offer the same models.")

(defun backup-record-text (provider model &aux (key (list provider model)))
  "What history says of PROVIDER MODEL as a backup, in a card's words."
  (destructuring-bind (&key answered-at tried rescued)
      (if *backup-records*
          (alexandria:ensure-gethash key *backup-records* (nle::failover-record provider model))
          (nle::failover-record provider model))
    (format nil "~:[never answered~;last answered ~:*~a~]~:[~; · rescued ~d of ~d~]"
            (backup-when answered-at) (plusp tried) rescued tried)))

(defun backup-option-text (provider model main)
  "A backup menu option's description: the record, and a warning when PROVIDER
is MAIN's, (PROVIDER MODEL) the room runs on, which an outage or a rate limit
would take down with it."
  (if (equal provider (first main))
      (format nil "same provider as this room's model · ~a" (backup-record-text provider model))
      (backup-record-text provider model)))

(defun backup-options (slot current chain main)
  "Backup SLOT's menu: CURRENT first (shown picked), the models used most that
neither CHAIN holds nor MAIN, the room's own (PROVIDER MODEL), is, and None."
  (flet ((line (provider model) (format nil "/models backups ~d ~a ~a" slot provider model)))
    (let ((others (loop for entry across (nle::frequent-models-json)
                        for pair = (list (gethash "provider" entry) (gethash "model" entry))
                        unless (or (member pair chain :test #'equal) (equal pair main))
                          collect pair into pairs
                        finally (return (subseq pairs 0 (min (length pairs) +backup-candidates+))))))
      (append
       (when current
         (destructuring-bind (provider model) current
           (list (menu-choice (format nil "Backup ~d: ~a/~a" slot provider model) (line provider model)
                              :description (backup-option-text provider model main)
                              :current t))))
       (loop for (provider model) in others
             collect (menu-choice (format nil "~a/~a" provider model) (line provider model)
                                  :description (backup-option-text provider model main)))
       (list (menu-choice "None" (format nil "/models backups ~d none" slot)
                          :description (if current "take this backup out" "leave this backup empty")))))))

(defun backups-card (main &key tested)
  "(values TEXT CONTROLS): the backups card -- the chain with what history
says of each backup, or what TESTED, ((PROVIDER MODEL) OK REASON SECONDS)
entries, found; a menu per backup; the triggers; Test and the way back."
  (let ((chain (backups-chain))
        (on (nle::failover-triggers))
        (*backup-records* (make-hash-table :test #'equal)))
    (values
     (with-output-to-string (out)
       (format out "## Backup Models~%When a turn's model fails, the turn tries these in order, in every room and shell.")
       (if chain
           (loop for (provider model) in chain
                 for index from 1
                 for test = (assoc (list provider model) tested :test #'equal)
                 do (format out "~%~d. ~a/~a: ~a" index provider model
                            (if test
                                (destructuring-bind (ok reason seconds) (rest test)
                                  (if ok
                                      (format nil "✅ answered in ~,1f s" seconds)
                                      (format nil "❌ ~a (~,1f s)" reason seconds)))
                                (backup-record-text provider model))))
           (format out "~%No backups: a failing turn ends on its own model."))
       (format out "~%Switches on: ~:[nothing, so the backups are never used~;~:*~{~a~^, ~}~]."
               (loop for (id . label) in nle::+failover-triggers+
                     when (member id on :test #'string=) collect label)))
     (append
      (loop for slot from 1 to +backup-slots+
            collect (list :menu (format nil "Backup ~d: none" slot)
                          (backup-options slot (nth (1- slot) chain) chain main)))
      (list (loop for (id . label) in nle::+failover-triggers+
                  for set = (member id on :test #'string=)
                  collect (choice (if set (format nil "✓ ~a" label) label)
                                  (format nil "/models backups ~:[on~;off~] ~a" set id)
                                  :style (if set :primary :secondary)))
            (list (choice "Test backups" "/models backups test" :style :primary :disabled (null chain))
                  (choice "Model Picker" "/models")))))))

(defun backups-wrote (outcome detail)
  "Stand when a failover write said OUTCOME :WRITTEN, else signal DETAIL."
  (unless (eq outcome :written)
    (error "the config was not written: ~a" (or detail outcome))))

(defun backups-command (words main &aux (chain (backups-chain))
                                       (slot (and words (parse-integer (first words) :junk-allowed t))))
  "(values TEXT CONTROLS): /models backups WORDS -- the card bare; N PROVIDER
MODEL, N MODEL or N none sets backup N; on or off TRIGGER switches a
trigger; test asks each backup once. MAIN is the room's (PROVIDER MODEL)."
  (cond
    ((null words) (backups-card main))
    ((and (string-equal (first words) "test") (null (rest words)))
     (backups-card main
                   :tested (loop for pair in chain
                                 collect (cons pair (multiple-value-list (apply #'nle::probe-model pair))))))
    ((and (member (first words) '("on" "off") :test #'string-equal) (rest words) (null (cddr words)))
     (let ((id (string-downcase (second words)))
           (on (nle::failover-triggers)))
       (unless (assoc id nle::+failover-triggers+ :test #'string=)
         (error "no trigger ~a; the triggers: ~{~a~^ ~}" id (mapcar #'car nle::+failover-triggers+)))
       (multiple-value-call #'backups-wrote
         (nle::set-failover-triggers (if (string-equal (first words) "on")
                                         (adjoin id on :test #'string=)
                                         (remove id on :test #'string=))))
       (backups-card main)))
    ((and slot (<= 1 slot +backup-slots+) (rest words) (null (cdddr words)))
     (let* ((args (rest words))
            (pair (cond ((and (null (rest args))
                              (member (first args) '("none" "remove") :test #'string-equal))
                         nil)
                        ((rest args) (list (checked-pick (first args) (second args)) (second args)))
                        (t (list (model-provider (first args)) (first args)))))
            ;; SLOT is replaced, or appended past the chain's end; a model
            ;; picked where it already stood elsewhere moves, never doubles.
            (chain (remove-duplicates (append (subseq chain 0 (min (1- slot) (length chain)))
                                              (and pair (list pair))
                                              (nthcdr slot chain))
                                      :test #'equal :from-end t)))
       (multiple-value-call #'backups-wrote (nle::set-failover-chain chain))
       (backups-card main)))
    (t "usage: /models backups [test | N provider model | N none | on TRIGGER | off TRIGGER]")))

;;; --- a command's dialog as a card --------------------------------------------------------
;;; A command answers a shell with a dialog (NLE:LIST-DIALOG, NLE:SLASH's
;;; second value) beside its words, and a room takes the words alone; where
;;; the platform's messages carry choices, the room gets the dialog too, as
;;; a card (minimize the need for args on the slash command; max dialog
;;; interactivity on Discord and any other channels): its title and what it
;;; says, a row of its tabs, a menu of its
;;; rows a page at a time, Refresh, and Back to the card it was opened from.
;;; A row's press says the line the shell's Enter runs (DIALOG-ROW-PRESS),
;;; so a card needs no handler of its own; a row that runs nothing is said
;;; in the card's words. Whatever the line answers takes the card's place:
;;; a dialog its card, and words alone are said over the card pressed, read
;;; again, as a shell says them in a toast over its panel (ANSWER-GESTURE).

(defparameter +card-said-rows+ 12
  "The most rows that run nothing a dialog's card says in its words.")

(defun dialog-row-options (row &aux (options (nlk:json-value row :array "options")))
  "The picks ROW, one of a dialog's rows, offers (NLE:LIST-DIALOG's
OPTIONS), as a vector of rows of their own, or NIL for a row with none."
  (and options (plusp (length options)) options))

(defun dialog-row-press (row &aux (run (nlk:json-value row :text "run" "enter"))
                                  (value (nlk:trimmed (or (nlk:json-value row :text "value") ""))))
  "(values LINE DRAFT): the line a press on ROW, one of a dialog's rows,
says -- its RUN's enter line, what the shell's Enter runs, else its VALUE
when that is a slash line, DRAFT then, since the shell's Enter hands it to
the composer to be read before it runs -- or NIL for a row that runs
nothing."
  (cond ((plusp (length run)) (values run nil))
        ((and (< 1 (length value)) (char= #\/ (char value 0))) (values value t))))

(defun dialog-tab-rows (dialog tab &aux (tabs (coerce (nlk:json-array dialog "tabs") 'list))
                                        (rows (coerce (nlk:json-array dialog "rows") 'list)))
  "(values ROWS TABS TAB): the rows of DIALOG its TABth tab shows -- a tab's
VALUE the KEY its rows carry, one with none every row -- its tabs, and TAB
clamped to them, NIL when it has none."
  (let* ((tab (and tabs (min (max 0 (or tab 0)) (1- (length tabs)))))
         (key (and tab (nlk:json-value (nth tab tabs) :string "value"))))
    (values (if key
                (remove-if-not (lambda (row) (equal key (nlk:json-value row :string "key"))) rows)
                rows)
            tabs tab)))

(defparameter +card-row-buttons+ 6
  "The most rows that run a line a dialog's card gives a button each; a tab
with more keeps them in a menu.")

(defun line-choice (label button &key mark detail)
  "A line of a card's -- MARK (a square or a glyph, or NIL), LABEL, DETAIL
-- with BUTTON, a CHOICE, beside it, or NIL for a line that runs nothing:
(:ROW MARK LABEL DETAIL BUTTON), drawn as one line with its button where
the platform can, as a button that says the line where it cannot."
  (list :row mark label detail button))

(defparameter +tone-squares+
  '((:red . "🟥") (:orange . "🟧") (:yellow . "🟨") (:green . "🟩") (:blue . "🟦")
    (:purple . "🟪") (:brown . "🟫") (:black . "⬛") (:white . "⬜"))
  "The squares a room draws a colour as, its text taking none.")

(defun tone-square (tone &aux (key (and tone (find-symbol (string-upcase tone) :keyword))))
  "The square a room draws TONE, a theme tone's name, as: the one nearest
the colour the organism's palette paints it (NLE::*PALETTE*, the theme the
shells paint in), NIL for a tone the palette lacks."
  ;; A grey goes white or black by its lightness, a dark warm colour brown,
  ;; the rest by hue.
  (nlk:when-let (rgb (let ((style (and key (gethash key nle::*palette*))))
                       (and style (nle::visual-style-fg style))))
    (multiple-value-bind (hue saturation lightness) (nle::rgb-hsl rgb)
      (cdr (assoc (cond ((< saturation 0.2) (if (>= lightness 0.6) :white :black))
                        ((and (< lightness 0.35) (< hue 50)) :brown)
                        ((or (< hue 15) (>= hue 330)) :red)
                        ((< hue 40) :orange)
                        ((< hue 70) :yellow)
                        ((< hue 170) :green)
                        ((< hue 250) :blue)
                        (t :purple))
                  +tone-squares+)))))

(defun row-mark (row &aux (mark (nlk:trimmed (or (nlk:json-value row :text "mark") ""))))
  "A dialog ROW's mark as a room draws it: a block, its tone's square
(TONE-SQUARE); any other glyph as it is; NIL for none."
  (cond ((zerop (length mark)) nil)
        ((every (lambda (char) (find char "█▉▊▋▌▍▎▏■▇▆▅▄▃▂▁")) mark)
         (or (tone-square (nlk:json-value row :text "mark_tone")) mark))
        (t mark)))

(defun picture-strip (dialog &aux (picture (nlk:json-value dialog :object "picture")))
  "DIALOG's picture in one row as a room draws it -- each cell of its
STRIP (NLE:LIST-DIALOG) its palette tone's square, ink and paper black and
white without one -- or NIL for a picture with none."
  (let ((strip (nlk:json-value picture :text "strip"))
        (palette (coerce (or (nlk:json-value picture :array "palette") #()) 'list)))
    (when (plusp (length strip))
      (with-output-to-string (out)
        (loop for char across strip
              for index = (digit-char-p char 36)
              do (write-string (cond ((null palette)
                                      (cdr (assoc (if (eql index 1) :black :white) +tone-squares+)))
                                     ((and index (nth index palette) (tone-square (nth index palette))))
                                     (t (cdr (assoc :black +tone-squares+))))
                               out))))))

(defun dialog-tone (dialog)
  "The card tone (OFFER-CARD's) DIALOG's own TONE (NLE:LIST-DIALOG) wears,
NIL for none."
  (cdr (assoc (nlk:json-value dialog :text "tone")
              '(("success" . :done) ("warn" . :paused) ("danger" . :failed))
              :test #'equal)))

(defun dialog-enter-words (dialog &aux (hint (or (nlk:json-value dialog :text "hint") "")))
  "What DIALOG's hint says Enter does on a row -- `open', `evict through'
-- or NIL: what a row's button, and the menu its rows go in, say."
  (ppcre:register-groups-bind (words) ("(?:^|  )enter ((?:[^ ]| (?! ))+)" hint)
    words))

(defun untagged (text)
  "TEXT without the handle bracket a room's line carries before its colon
(HANDLE-TAG): a person's words as a card names them."
  (ppcre:regex-replace-all " \\[(?:[mur][^\\s\\]]+ ?)+\\](?=:)" text ""))

(defun first-capital (words)
  "WORDS with their first letter capital: a hint's `open' as a button's
`Open'."
  (if (plusp (length words))
      (concatenate 'string (string-upcase (subseq words 0 1)) (subseq words 1))
      words))

(defun card-columns (text)
  "TEXT, a row's detail laid out in a shell's columns, as words: its runs
of spaces one ` · '."
  (ppcre:regex-replace-all " {2,}" (nlk:trimmed text) " · "))

(defun dialog-card (dialog line &key tab (page 1) back back-tab back-page)
  "(values TEXT CONTROLS PANEL): DIALOG -- the JSON a command's answer
carries (NLE:LIST-DIALOG) -- as the card answering LINE: on its TABth tab,
the PAGEth run of that tab's rows; BACK, the line of the card it was opened
from, the way back to it, on BACK-TAB and BACK-PAGE. PANEL is the card
where the platform draws one (OFFER-CARD's): its title, the muted words
beside it, the picture in one row (PICTURE-STRIP), the summary, its tone."
  ;; A tab of few rows lists them, each a line with its button where it
  ;; runs one (LINE-CHOICE) -- every part of /context one press away, never
  ;; behind a menu; a longer one keeps its words in the text and its
  ;; pressable rows in a menu, a page at a time.
  (multiple-value-bind (rows tabs tab) (dialog-tab-rows dialog tab)
    (let* ((picked (remove-if-not #'dialog-row-options rows))
           (pressable (remove-if (lambda (row) (or (member row picked) (not (dialog-row-press row)))) rows))
           (said (remove-if (lambda (row) (or (dialog-row-press row) (member row picked)
                                              (and (equal "" (nlk:trimmed (or (nlk:json-value row :text "label") "")))
                                                   (equal "" (nlk:trimmed (or (nlk:json-value row :text "detail") ""))))))
                            rows))
           ;; few enough that a platform draws each with its button
           (lined (and (<= (length pressable) +card-row-buttons+)
                       (<= (+ (length pressable) (length said) (length picked)) (* 2 +card-row-buttons+))))
           (enter (dialog-enter-words dialog)))
      (multiple-value-bind (shown page pages) (if lined (values pressable 1 1) (picker-page pressable page))
        (labels ((field (object key) (nlk:trimmed (or (nlk:json-value object :text key) "")))
                 (words (text) (and (plusp (length text)) text))
                 (label (row) (words (untagged (field row "label"))))
                 (detail (row) (words (card-columns (field row "detail"))))
                 (here (&rest view)
                   ;; this card's own view, VIEW over it: its tab, its page, its way back
                   (append view (list :tab tab :page page :back back :back-tab back-tab :back-page back-page)))
                 (menu (row)
                   ;; ROW's options as its menu, each saying the row it is a pick of
                   (list :menu (format nil "~@[~a~]…" (label row))
                         (loop for option across (dialog-row-options row)
                               for line-of = (dialog-row-press option)
                               when line-of
                                 collect (menu-choice (format nil "~@[~a · ~]~a" (label row) (or (label option) line-of))
                                                      line-of
                                                      :description (detail option)
                                                      :current (plusp (length (field option "mark")))
                                                      :back line :back-tab tab :back-page page))))
                 (press (row &optional (as (or (words (field row "verb")) enter "open")))
                   ;; the choice ROW's press says, and the card it answers comes back to
                   (multiple-value-bind (press draft) (dialog-row-press row)
                     (choice (format nil "~a~:[~;…~]" (first-capital as) draft) press
                             :confirm draft :back line :back-tab tab :back-page page))))
          (let* ((title (or (words (field dialog "title")) line))
                 (context (words (field dialog "context")))
                 (strip (picture-strip dialog))
                 (summary (words (field dialog "summary")))
                 (said-lines (append (loop for row in (remove-if-not (lambda (row) (or (member row said) (member row picked))) rows)
                                           repeat +card-said-rows+
                                           ;; a row with no label carries on the one above it
                                           collect (if (label row)
                                                       (format nil "- ~@[~a ~]~a~@[ · ~a~]" (row-mark row) (label row) (detail row))
                                                       (format nil "  ~a" (field row "detail"))))
                                     (and (> (+ (length said) (length picked)) +card-said-rows+)
                                          (list (format nil "- +~d more" (- (+ (length said) (length picked)) +card-said-rows+))))))
                 (empty (and (null rows) (or (words (field dialog "empty_label")) "Nothing here."))))
            (values
             (format nil "## ~a~@[~%~a~]~@[~%~a~]~@[~%~a~]~{~%~a~}~@[~%~a~]"
                     title context strip summary said-lines empty)
             (append
              (and tabs
                   (if (<= (length tabs) 5)
                       (list (loop for each in tabs
                                   for index from 0
                                   collect (apply #'choice (format nil "~a~@[ (~a)~]" (field each "label")
                                                                   (nlk:json-value each :integer "count"))
                                                  line :style (if (eql index tab) :primary :secondary)
                                                  (here :tab index :page 1))))
                       (list (list :menu "Tab"
                                   (loop for each in tabs
                                         for index from 0
                                         collect (apply #'menu-choice (field each "label") line
                                                        :current (eql index tab) (here :tab index :page 1)))))))
              (if lined
                  ;; every row in its order, a button on those that run a line, a menu
                  ;; on those that offer picks
                  (loop for row in (remove-if-not (lambda (row) (or (member row pressable) (member row said)
                                                                    (member row picked)))
                                                  rows)
                        collect (if (member row picked)
                                    (menu row)
                                    (line-choice (or (label row) "") (and (member row pressable) (press row))
                                                 :mark (row-mark row) :detail (detail row))))
                  (append
                   (mapcar #'menu picked)
                   (and shown
                       (list (list :menu (if enter
                                             (format nil "~a…" (first-capital enter))
                                             (format nil "Pick one of ~d" (length pressable)))
                                   (loop for row in shown
                                         for mark = (field row "mark")
                                         collect (multiple-value-bind (press draft) (dialog-row-press row)
                                                   (menu-choice (or (label row) press) press
                                                                :description (words (format nil "~@[~a ~]~a" (words mark)
                                                                                            (or (detail row) "")))
                                                                :confirm draft
                                                                :back line :back-tab tab :back-page page))))))))
              (and (> pages 1)
                   (list (list (apply #'choice "Prev" line :disabled (= page 1) (here :page (1- page)))
                               (list (format nil "Page ~d/~d" page pages) "nck:page" :secondary t)
                               (apply #'choice "Next" line :disabled (= page pages) (here :page (1+ page))))))
              (list (append (and back (list (choice "Back" back :tab back-tab :page back-page)))
                            (list (apply #'choice "Refresh" line (here))))))
             (list :title title :context context :gauge strip
                   ;; a lined card says its rows as its lines
                   :text (format nil "~{~a~^~%~}"
                                 (remove nil (list* summary (append (and (not lined) said-lines) (list empty)))))
                   :tone (dialog-tone dialog)))))))))

(defun confirm-card (line press)
  "(values TEXT CONTROLS PANEL): the card that asks before LINE runs -- a
dialog row's draft, which a shell hands to the composer to be read before it
is sent -- with Run, and Back to the card it was picked on, PRESS saying
which."
  (let ((back (nlk:json-value press :text "back"))
        (tab (nlk:json-value press :integer "back_tab"))
        (page (nlk:json-value press :integer "back_page")))
    (values (format nil "## Run this?~%~a" line)
            (list (list* (choice "Run" line :style :primary :back back :back-tab tab :back-page page)
                         (and back (list (choice "Back" back :tab tab :page page)))))
            (list :title "Run this?" :text line))))

;;; --- pairing ---------------------------------------------------------------------------------

(defparameter +pairing-alphabet+ "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"
  "What a pairing code is spelled from: no 0/O, no 1/I.")

(defparameter +pairing-code-ms+ 3600000
  "How long a pairing code stands.")

(defparameter +pairing-ask-ms+ 600000
  "How often one person may be given a code.")

(defparameter +pairing-pending-cap+ 3
  "The most codes one host has standing at once.")

(defparameter +paired-key+ "paired"
  "The state key the people the operator paired are kept under.")

(defvar *pairing-codes* (make-hash-table :test #'equal :synchronized t)
  "Code -> (:host :user :name :target :at :id): the codes standing, :ID the
handle the operator's page approves an ask by. Live-only: a restart drops
them, and the person asks again.")

(defvar *pairing-asked* (make-hash-table :test #'equal :synchronized t)
  "(host id . user id) -> when that person was last given a code.")

(defun pairing-code ()
  "Eight characters of +PAIRING-ALPHABET+ from the system's entropy."
  ;; 32 letters divide 256: each byte picks one uniformly.
  (map 'string (lambda (byte) (char +pairing-alphabet+ (mod byte 32))) (nlk:random-bytes 8)))

(defun standing-codes (host-id &aux (now (now-ms)))
  "HOST-ID's codes still standing, as (CODE . ENTRY); the expired ones go."
  (let ((standing '()))
    (sb-ext:with-locked-hash-table (*pairing-codes*)
      (loop for code being the hash-keys of *pairing-codes* using (hash-value entry)
            do (cond ((> (- now (getf entry :at)) +pairing-code-ms+) (remhash code *pairing-codes*))
                     ((equal host-id (getf entry :host)) (push (cons code entry) standing)))))
    standing))

(defun pairing-candidate-p (host candidate reason)
  "Whether CANDIDATE, rejected for REASON, is a person writing to HOST
directly whom a pairing code could let in."
  (and host.pairing-p
       (equal reason (inbound-reason host.policy :user-not-allowed))
       (equal "direct_message" (source-field candidate "chat_kind"))
       (not (candidate-bot-p candidate))
       (source-field candidate "user_id")
       t))

(defun offer-pairing (host candidate &aux (user (source-field candidate "user_id"))
                                          (name (or (source-field candidate "user_name") "someone"))
                                          (key (cons (host-id host) user))
                                          (now (now-ms)))
  "Worker thread: answer CANDIDATE, a direct message from someone not let in,
with a pairing code, and tell the operator who asked — at most once in ten
minutes a person."
  (let ((asked (gethash key *pairing-asked*)))
    (unless (and asked (< (- now asked) +pairing-ask-ms+))
      (setf (gethash key *pairing-asked*) now)
      (let ((target (channel-target candidate)))
        (if (>= (length (standing-codes (host-id host))) +pairing-pending-cap+)
            (post-message host target "The bot's operator has too many requests waiting; try again later."
                          :what "pairing refusal")
            (let ((code (pairing-code)))
              (setf (gethash code *pairing-codes*)
                    (list :host (host-id host) :user user :name name :target target :at now
                          :id (nlk:make-durable-id "ask")))
              (post-message host target
                            (format nil "I don't know you yet. Your pairing code is ~a: give it to the ~
                                         bot's operator, who can let you in with it. It lasts an hour."
                                    code)
                            :what "pairing code")
              (tell-operator host (format nil "~a: ~a (~a) wrote to the bot and asks to be let in; ~
                                               /channels pair <their code> does it"
                                          (host-id host) name user))))))))

(defun paired (host)
  "The people the operator paired on HOST, as JSON objects {id, name, at}."
  (or (and (nlk:store-open-p)
           (nlk:when-let (json (nlk:session-state-get (handoff-session host) +paired-key+))
             (coerce (nlk:decode-json json) 'list)))
      '()))

(defun save-paired (host people)
  "Keep PEOPLE as HOST's paired, and let the gate read them."
  (nlk:session-state-put (handoff-session host) +paired-key+
                         (nlk:encode-json-object (coerce people 'vector)))
  (note-paired host people))

(defun note-paired (host people)
  "Let the admission gate read PEOPLE as HOST's paired users."
  (setf (gethash (host-id host) *paired-users*)
        (mapcar (lambda (person) (nlk:json-value person :string "id")) people)))

(defun pair (code &aux (entry (gethash (string-upcase (remove #\Space (or code ""))) *pairing-codes*)))
  "Let in the person CODE was given to. => what happened, as a line."
  (let ((host (and entry (find (getf entry :host) *hosts* :key #'host-id :test #'equal))))
    (cond
      ((or (null entry) (> (- (now-ms) (getf entry :at)) +pairing-code-ms+))
       "no such pairing code standing; the person can write to the bot again for a new one")
      ((null host) (format nil "the ~a channel is not running" (getf entry :host)))
      (t
       (remhash (string-upcase (remove #\Space code)) *pairing-codes*)
       (let ((user (getf entry :user)))
         (save-paired host (append (remove user (paired host)
                                           :key (lambda (person) (nlk:json-value person :string "id"))
                                           :test #'equal)
                                   (list (nlk:json-object "id" user "name" (getf entry :name)
                                                          "at" (nlk:iso-now)))))
         (on-worker host
           (post-message host (getf entry :target) "You're in: the operator let you in. Ask away."
                         :what "pairing welcome"))
         (format nil "~a: ~a (~a) is let in" (host-id host) (getf entry :name) user))))))

(defun unpair (user)
  "Take back the pairing of USER, a user id, on every host. => a line."
  (let ((hosts (loop for host in *hosts*
                     when (find user (paired host) :key (lambda (person) (nlk:json-value person :string "id"))
                                                   :test #'equal)
                       collect host)))
    (dolist (host hosts)
      (save-paired host (remove user (paired host)
                                :key (lambda (person) (nlk:json-value person :string "id"))
                                :test #'equal)))
    (if hosts
        (format nil "~{~a~^, ~}: ~a is no longer let in" (mapcar #'host-id hosts) user)
        (format nil "nobody paired has the id ~a" user))))

(defun pairing-report ()
  "Who is paired on every host, and how many codes stand."
  (format nil "~{~a~^~%~}"
          (or (loop for host in *hosts*
                    for people = (paired host)
                    for standing = (length (standing-codes (host-id host)))
                    when (or people (plusp standing))
                      collect (format nil "~a: paired ~:[nobody~;~:*~{~a~^, ~}~]~[~:;; ~:*~d code~:p waiting~]"
                                      (host-id host)
                                      (mapcar (lambda (person)
                                                (format nil "~a (~a)" (nlk:json-value person :string "name")
                                                        (nlk:json-value person :string "id")))
                                              people)
                                      standing))
              (list "nobody is paired and no code is waiting"))))

(defun pairing-json (&aux (now (now-ms)))
  "PAIRING-REPORT as the operator's page reads it, per started host: whether a
stranger's direct message is offered a code at all, the asks standing — who
wrote, how long ago, how long the code has left, and the ask's id to approve
it by — and the people let in."
  ;; Never the code: it is the person's to bring (Hermes' admin surface lists
  ;; its asks the same way, approved by id).
  (map 'vector
       (lambda (host)
         (nlk:json-object
          "id" (host-id host)
          "on" host.pairing-p
          "asks" (map 'vector
                      (lambda (standing &aux (entry (cdr standing)) (age (- now (getf entry :at))))
                        (nlk:json-object "id" (getf entry :id) "user" (getf entry :user)
                                         "name" (getf entry :name)
                                         "age_seconds" (floor age 1000)
                                         "seconds_left" (floor (- +pairing-code-ms+ age) 1000)))
                      (sort (standing-codes (host-id host)) #'< :key (lambda (standing) (getf (cdr standing) :at))))
          "paired" (coerce (paired host) 'vector)))
       (reverse *hosts*)))

(defun ask-code (id)
  "The code of the standing ask whose id is ID, or NIL: what PAIR takes when
the operator's page approves the ask it lists."
  (sb-ext:with-locked-hash-table (*pairing-codes*)
    (loop for code being the hash-keys of *pairing-codes* using (hash-value entry)
          when (equal id (getf entry :id)) return code)))

;;; --- the home channel ---------------------------------------------------------------------

(defparameter +home-key+ "home"
  "The state key a host's home channel is kept under.")

(defun home-target (host)
  "HOST's home channel as a target plist, or NIL when none is set."
  (and (nlk:store-open-p)
       (nlk:when-let (json (nlk:session-state-get (handoff-session host) +home-key+))
         (nlk:json-plist (nlk:decode-json json)))))

(defun set-home (host target)
  "Make TARGET HOST's home channel; NIL clears it."
  (if target
      (nlk:session-state-put (handoff-session host) +home-key+
                             (nlk:encode-json-object
                              (nlk:plist-json (list :channel-id (getf target :channel-id)
                                                    :thread-id (getf target :thread-id)))))
      (nlk:session-state-delete (handoff-session host) +home-key+)))

(defun say-home (host text)
  "TEXT posted in HOST's home channel when one is set, on its worker."
  (nlk:when-let (home (home-target host))
    (on-worker host (post-message host home text :what "home channel notice"))))

(defvar *home-notes* (make-hash-table :test #'equal :synchronized t)
  "Host id -> the note of the organism's its home channel last heard: a note
comes as a frame, and again at a host's start while it is fresh.")

(defun say-note-home (host text)
  "TEXT, a note the organism said (NLE:ORGANISM-NOTE), in HOST's home channel
once."
  (when (sb-ext:with-locked-hash-table (*home-notes*)
          (unless (equal text (gethash (host-id host) *home-notes*))
            (setf (gethash (host-id host) *home-notes*) text)))
    (say-home host text)))

(defun tell-operator (host text &key (level :info))
  "TEXT to the operator: a notice on every attached shell, and a post in
HOST's home channel when one is set."
  ;; The notice carries no key, so it never reaches the board a model reads.
  (nle:notice text :level level)
  (say-home host text))

(defun room-host (session-id)
  "The started host whose room SESSION-ID is, and the room's target, or NIL."
  (loop for host in *hosts*
        for target = (session-target (platform-session-prefix host.platform) session-id)
        when target return (values host target)))

(defun sethome-command (args session-id)
  "/sethome: make the room SESSION-ID is the home channel — `off' clears it —
or, from a shell, say where each home is."
  (multiple-value-bind (host target) (room-host session-id)
    (cond
      ((null host)
       (format nil "~{~a~^~%~}"
               (or (loop for host in *hosts*
                         collect (format nil "~a: ~:[no home channel; type /sethome in the one you want~;~:*home is ~a~]"
                                         (host-id host)
                                         (nlk:when-let (home (home-target host))
                                           (format nil "channel ~a~@[ thread ~a~]"
                                                   (getf home :channel-id) (getf home :thread-id)))))
                   (list "no channel adapters running"))))
      ((string-equal (nlk:trimmed (or args "")) "off")
       (set-home host nil)
       "no home channel now: the bot reports to the operator's shells alone")
      (t
       (set-home host target)
       "this is the home channel now: restart notices, new releases, pairing requests and refused commands come here"))))
