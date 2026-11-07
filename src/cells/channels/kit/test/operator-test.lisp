;;;; operator-test.lisp --- what the operator sets for the rooms: agents
;;;; and routes, the room's agent and model, pairing, the home channel, and a
;;;; turn on a lane the reaper let go.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun operator-host (&rest keys)
  "A started test host with one delivery worker; KEYS ride into TEST-HOST."
  (let ((host (apply #'test-host :workers 1 keys)))
    (nck:start-host host)
    host))

(defun agents-and-routes (agents &optional (routes "[]"))
  "(values AGENTS ROUTES) the section whose agents and routes are the JSON
AGENTS and ROUTES reads as."
  (let* ((section (cell-json (format nil "{\"agents\": ~a, \"routes\": ~a}" agents routes)))
         (agents (nck::read-agents "test" section)))
    (values agents (nck::read-routes "test" section agents))))

(defun routed-agent (host channel &key thread user roles guild)
  "(values NAME WHY) the ask USER, holding ROLES, says in CHANNEL — in its
THREAD, on the server GUILD — runs as."
  (let ((candidate (test-candidate :channel channel :thread thread :parent (and thread channel)
                                   :user user :roles roles)))
    (when guild
      (setf (gethash "workspace_id" (nck::candidate-source candidate)) guild))
    (nck::agent-for host (nck:room-session-id "chat" candidate) (nck:channel-target candidate)
                     (nck::candidate-source candidate))))

(deftest channel-operator-routes-send-asks-to-agents ()
  ;; channels.<id>.agents names what a room's asks may run as, and routes send
  ;; them there by server, channel, thread, person or role: every key a route
  ;; names must match, the most specific route wins, a tie goes to the route
  ;; written first, a channel's route takes its threads, and an ask no route
  ;; takes runs as the channel itself.
  (multiple-value-bind (agents routes)
      (agents-and-routes "{\"coder\": {\"prompt\": \"Release talk only.\", \"provider\": \"p1\",
                                       \"model\": \"m1\", \"skills\": [\"release\", \"triage\"]},
                           \"support\": {}, \"ops\": {}, \"lead\": {}}"
                         "[{\"agent\": \"support\", \"guild\": \"g1\"},
                           {\"agent\": \"ops\", \"guild\": \"g1\", \"role\": \"r1\"},
                           {\"agent\": \"coder\", \"channel\": \"100\"},
                           {\"agent\": \"default\", \"channel\": \"100\", \"thread\": \"42\"},
                           {\"agent\": \"lead\", \"user\": \"u7\"},
                           {\"agent\": \"support\", \"channel\": \"200\"},
                           {\"agent\": \"ops\", \"channel\": \"200\"}]")
    (let ((host (test-host :name "routes" :agents agents :routes routes)))
      (is-each (routed-agent host)
        ("300" nil "no route, the channel itself")
        ("300" :guild "g1" "support" "the server's")
        ("300" :guild "g1" :roles '("r2" "r1") "ops" "a role outweighs the server")
        ("100" :guild "g1" :roles '("r1") "coder" "a channel outweighs both")
        ("100" :thread "43" "coder" "a channel's route takes its threads")
        ("100" :thread "42" nil "a thread's own route outweighs it: default")
        ("100" :thread "42" :user "u7" "lead" "a person outweighs every place")
        ("200" "support" "a tie goes to the route written first"))
      (multiple-value-bind (name why route) (routed-agent host "300" :guild "g1" :roles '("r1"))
        (is (equal '("ops" :route "role r1 and guild g1") (list name why (nck::route-text route)))))
      (is-present (standing (nck::agent-standing (nck::agent-of host "coder") "channel")) "its paragraph"
        (is (search "standing instructions for this channel" standing))
        (is (search "Release talk only." standing))
        (is (search "release, triage" standing))
        (is (search "(help 'NAME)" standing))
        (is (search standing (nck:lane-contract :standing standing))))
      (is (null (nck::agent-standing (nck::agent-of host "support") "channel")) "an agent that sets none")
      (is (null (nck::agent-standing nil "channel")) "the channel itself")))
  ;; What it cannot read refuses the section at the start, naming the entry.
  (is-table (agents routes needle) (search needle (refusal-text nlk:config-refusal
                                                               (agents-and-routes agents routes)))
    ("{\"coder\": {\"modle\": \"x\"}}" "[]" "agents.coder: no setting \"modle\"")
    ("{\"coder\": {\"provider\": \"p\"}}" "[]" "a provider needs its model")
    ("{\"coder\": {\"folder\": \"/nowhere/at/all\"}}" "[]" "agents.coder: folder:")
    ("{\"default\": {}}" "[]" "an agent's name is one word, and not default")
    ("{\"two words\": {}}" "[]" "an agent's name is one word")
    ("{\"coder\": {}}" "[{\"agent\": \"cdoer\", \"user\": \"u1\"}]"
     "routes[0]: no agent \"cdoer\"; channels.test.agents defines coder, and \"default\" is the channel itself")
    ("{}" "[{\"user\": \"u1\"}]" "routes[0]: names no agent; channels.test.agents defines none")
    ("{\"coder\": {}}" "[{\"agent\": \"coder\"}]" "routes[0]: matches nothing")
    ("{\"coder\": {}}" "[{\"agent\": \"coder\", \"chanel\": \"1\"}]" "routes[0]: no key \"chanel\"")
    ("{\"coder\": {}}" "[{\"agent\": \"coder\", \"user\": 1}]" "user is an id, written as a string")))

(deftest channel-operator-agent-hands-a-room-to-an-agent ()
  ;; /agent in a room says who answers there and why; /agent NAME hands the
  ;; room to that agent, its threads with it, ahead of every route, and
  ;; retires the lanes forked there and in its threads, so the next line runs
  ;; as the agent; /agent default gives the room back to the routes. A name no
  ;; agent has, and a room no running channel holds, are refused.
  (with-temp-store ()
    (multiple-value-bind (agents routes)
        (agents-and-routes "{\"coder\": {}, \"support\": {}}" "[{\"agent\": \"support\", \"user\": \"u7\"}]")
      (let* ((host (test-host :name "agent-command" :agents agents :routes routes))
             (nck::*hosts* (list host))
             (kim (nck::candidate-source (test-candidate :user "u7"))))
        (flet ((agent (room &optional (args "") source)
                 (let ((nck:*command-source* source)) (nck::agent-command args room)))
               (lane (id room)
                 (nck:ensure-session room)
                 (nck:ensure-session id :parent room)
                 (nck:intern-lane (nck:host-lanes host) id :target '(:channel-id "100")
                                                           :parent-session-id room))
               (kept (id) (and (nck:find-lane (nck:host-lanes host) id) t)))
          (is (search "this channel is answered by the channel itself" (agent "chat-100")))
          (is (search "agents: coder, support; /agent NAME hands this channel to one" (agent "chat-100")))
          (is (search "answered by agent support, by the route on user u7" (agent "chat-100" "" kim)))
          (lane "chat-100-m1" "chat-100")
          (lane "chat-100-t42-m2" "chat-100-t42")
          (lane "chat-200-m3" "chat-200")
          (nlk:materialize-standby-session "chat-100-t42-m2")
          (is (search "this channel is answered by agent coder from its next ask" (agent "chat-100" "coder")))
          (is-each (kept)
            ("chat-100-m1" nil "the room's lanes are retired")
            ("chat-100-t42-m2" nil "and its threads'")
            ("chat-200-m3" t "another room's are not"))
          (is-each (routed-agent host)
            ("100" :user "u7" "coder" "the room's agent outweighs every route")
            ("100" :thread "42" "coder" "a thread runs as its channel was handed"))
          (is (search "handed to it with /agent; /agent default gives it back" (agent "chat-100" "" kim)))
          (is (search "as its channel was handed with /agent" (agent "chat-100-t42")))
          ;; Default goes back to the routes.
          (is (search "this channel is answered by agent support from its next ask"
                      (agent "chat-100" "default" kim)))
          (is (null (routed-agent host "100")))
          (is (search "no agent cdoer; the agents are coder, support"
                      (refusal-text error (agent "chat-100" "cdoer"))))
          (is (search "usage: /agent" (agent "chat-100" "coder now")))
          (is (search "no room of a running channel" (refusal-text error (agent "tui-1" "coder")))))))))

(deftest channel-operator-agent-completes-its-room-s-agents ()
  ;; /agent's argument completes, while it is typed, to the agents of the room
  ;; it is typed in, in the order the config names them, each beside its
  ;; folder, then default; the tail typed narrows them, and a session no
  ;; channel holds completes nothing. The menu's request completes in the
  ;; room it was typed in: a thread's, placed under its channel.
  (let ((folder (namestring (uiop:temporary-directory)))
        (platform (test-platform))
        (asked '()))
    (setf (nck:platform-plan-autocomplete platform)
          (lambda (payload choices &key timeout-seconds)
            (declare (ignore timeout-seconds))
            (push (list payload choices) asked)
            '()))
    (multiple-value-bind (agents routes)
        (agents-and-routes (format nil "{\"coder\": {\"folder\": ~s}, \"support\": {}}" folder))
      (let* ((host (test-host :name "agent-choices" :agents agents :routes routes :workers 1
                              :platform platform))
             (coder (format nil "coder — ~a" (getf (nck::agent-of host "coder") :folder))))
        ;; Set, not bound: the menu's request is answered on the host's worker.
        (with-saved-globals (nle::*registered-commands* nck::*hosts*)
          (setf nck::*hosts* (list host))
          (nlk:with-cleanup ((nck:stop-delivery-worker (nck:host-worker host)))
            (flet ((offered (text room &optional (key :name))
                     (mapcar (lambda (choice) (getf choice key)) (nck::agent-choices text room))))
              (is (equal (list coder "support" "default — back to the routes") (offered "" "chat-100")))
              (is (equal '("support") (offered "SUP" "chat-100-t42" :value)))
              (is (equal '("default") (offered "def" "chat-100" :value)))
              (is (null (offered "" "tui-1")))
              (nck:register-channel-commands)
              (nle:register-command "probe-pack" "where" (constantly nil)
                                    :complete (lambda (text session-id)
                                                (list (list :name session-id :value text))))
              (flet ((complete (command)
                       (setf asked '())
                       (nck:completion-requested host (list :id "i1" :token "tok" :command command
                                                            :text "co" :user-id "mike"
                                                            :channel-id "100" :thread-id "42"))
                       (and (await (:timeout 5) asked) (second (first asked)))))
                (is (equal (list (list :name coder :value "coder")) (complete "agent")))
                ;; A thread's request completes in the thread's room.
                (is (equal '((:name "chat-100-t42" :value "co")) (complete "where")))))))))))

(deftest channel-operator-an-ask-runs-as-its-agent ()
  ;; A routed ask's lane works in its agent's folder, with its persona and
  ;; its standing prompt; a reply to that lane goes on as it, whoever wrote
  ;; it; an ask no route takes runs as the channel itself.
  (with-temp-file (soul :type "md" :contents "Answer as the release captain.")
    (let ((folder (directory-namestring soul)))
      (with-ask-host (host (lambda (&key executor)
                             (multiple-value-bind (agents routes)
                                 (agents-and-routes
                                  (format nil "{\"captain\": {\"folder\": ~s, \"soul_file\": ~s,
                                                              \"prompt\": \"Release talk only.\"}}"
                                          folder soul)
                                  "[{\"agent\": \"captain\", \"channel\": \"100\"}]")
                               (let ((host (test-host :executor executor :workers 1 :name "ask-agent"
                                                      :agents agents :routes routes)))
                                 (setf (nck:host-policy host) (channel-policy :allowed-channels '("100" "200")))
                                 (nck:start-host host)
                                 host)))
                      :answers ("ok" "ok") :responses (replies "p1" "p2"))
        (nck:handle-candidate host (test-candidate :channel "100" :message "5" :text "when do we ship?"))
        (is-present (lane (await-lane host "chat-100-m5")) "the ask's lane"
          (is (equal "captain" (nck::lane-agent lane)))
          (is (await () (nlk:session-exists-p "chat-100-m5")))
          ;; It works in the agent's folder, with its persona and its prompt.
          (is (equal (namestring (truename folder))
                     (namestring (truename (nlk:find-session-cwd "chat-100-m5")))))
          (is (equal "Answer as the release captain."
                     (nlk:get-harness-section "chat-100-m5" nck:+soul-section+)))
          (is (search "Release talk only."
                      (nlk:get-harness-section "chat-100-m5" (nck::platform-contract-section
                                                              (nck::host-platform host)))))
          ;; A reply from someone no route sends there goes on as the lane does.
          (is (equal "captain" (nck::ask-agent (nck::build-ask host (test-candidate :channel "100" :message "6"
                                                                                    :user "u9")
                                                               "chat-100-m5" t)))))
        (nck:handle-candidate host (test-candidate :channel "200" :message "7"))
        (is-present (lane (await-lane host "chat-200-m7")) "an ask no route takes"
          (is (null (nck::lane-agent lane)))
          (is (await () (nlk:session-exists-p "chat-200-m7")))
          (is (null (nlk:get-harness-section "chat-200-m7" nck:+soul-section+)) "the channel's own persona"))))))

(defmacro with-catalog (&body body)
  "BODY with the organism's catalog two providers: p1 serving m1 and shared,
p2 serving m2 and shared."
  `(with-stubbed-fdefinition (nle:models (&optional provider)
                              (cond ((null provider) '(("p1" . 2) ("p2" . 2)))
                                    ((equal provider "p1") (values '("m1" "shared") nil))
                                    ((equal provider "p2") (values '("m2" "shared") nil))
                                    (t (error "no provider ~s is configured" provider))))
     ,@body))

(deftest channel-operator-a-model-set-in-a-room-stays-there ()
  ;; /models typed in a room sets that room's model and no other, checked
  ;; against the catalog first; a thread runs on its channel's unless it has
  ;; its own, and the model of the agent the room runs as comes after any
  ;; room's pick.
  (with-temp-store ()
    (with-catalog
      (let ((host (multiple-value-bind (agents routes)
                      (agents-and-routes "{\"coder\": {\"model\": \"m1\", \"provider\": \"p1\"}}"
                                         "[{\"agent\": \"coder\", \"channel\": \"100\"}]")
                    (test-host :name "room-models" :agents agents :routes routes)))
            (channel (test-candidate :channel "100"))
            (thread (test-candidate :channel "100" :thread "42" :parent "100")))
        (flet ((models (candidate args)
                 (nck::room-models-command host candidate args (nck:room-session-id "chat" candidate)))
               (lane-model (room target)
                 (multiple-value-list (nck::lane-model host room target (nck::agent-of host "coder")))))
          (is (equal '("p1" "m1" :agent) (lane-model "chat-100" '(:channel-id "100"))))
          (is (search "runs p1/m1, set by its agent" (models channel "")))
          (is (search "this channel runs p2/m2 from its next ask" (models channel "m2")))
          (is (search "the organism's default is unchanged" (models channel "p1 shared")))
          (is (equal '("p1" "shared" :room) (lane-model "chat-100" '(:channel-id "100"))))
          (is (equal '("p1" "shared" :room) (lane-model "chat-100-t42" '(:channel-id "100" :thread-id "42"))))
          (is (search "this thread runs p2/m2" (models thread "p2 m2")))
          (is (equal '("p2" "m2" :room) (lane-model "chat-100-t42" '(:channel-id "100" :thread-id "42"))))
          ;; A model more than one provider serves, one none serves, a
          ;; provider that does not list it: each refuses, and nothing moves.
          (is (search "served by p1 and p2" (refusal-text error (models channel "shared"))))
          (is (search "no configured provider lists m9" (refusal-text error (models channel "m9"))))
          (is (search "p2 lists no model m1" (refusal-text error (models channel "p2 m1"))))
          (is (equal '("p1" "shared" :room) (lane-model "chat-100" '(:channel-id "100"))))
          (is (search "runs p1/m1 from its next ask" (models channel "default")))
          (is (equal '("p1" "m1" :agent) (lane-model "chat-100" '(:channel-id "100")))))))))

(deftest channel-operator-provider-navigation-is-not-preselected ()
  ;; Provider selection navigates; it must not start selected just because
  ;; the current model uses that provider. Otherwise choosing it again can
  ;; be a no-op in the client instead of opening its model list.
  (with-temp-store ()
    (with-catalog
      (with-stubbed-fdefinition (nle:gateway-target (session-id)
                                  (values "p1" "m1"))
        (let* ((host (test-host :name "provider-navigation"
                                :platform (test-platform :choices t)))
               (candidate (test-candidate :channel "100")))
          (multiple-value-bind (text controls)
              (nck::room-models-command host candidate "" "chat-100")
            (let ((options (third (first controls))))
              (is (search "Current model: p1/m1" text))
              (is (notany #'fourth options) "provider navigation has no default selection")
              (is (search "current" (third (first options))) "the current provider remains visibly identified")
              (multiple-value-bind (next-text next-controls)
                  (nck::run-command host candidate (nck:said-line (second (first options))))
                (is (search "Select a p1 model" next-text))
                (is (equal '("m1" "shared")
                           (mapcar #'first (third (first next-controls)))) "choosing the current provider opens its models")
                (is (null (nth-value 1 (nck::room-model host "chat-100"))) "navigation does not change the room model")))))))))

(deftest channel-operator-backups-card-writes-the-failover-chain ()
  ;; The Model Picker's Backups card is the failover chain's own view: a menu
  ;; per backup, the triggers as toggles, Test and the way back -- Discord's
  ;; five rows -- each press a /models backups line that writes the config.
  (with-temp-store ()
    (with-catalog
      (with-temp-file (path :contents "{}")
        (let ((nle::*shared-config-path* path))
          (with-stubbed-fdefinitions
              ((nle:gateway-target (session-id) (values "p1" "m1"))
               (nle::frequent-models-json ()
                 (vector (nlk:json-object "provider" "p1" "model" "m1" "turns" 9)
                         (nlk:json-object "provider" "p2" "model" "m2" "turns" 3)))
               (nle::failover-record (provider model) (list :answered-at nil :tried 0 :rescued 0)))
            (let ((host (test-host :name "backups" :platform (test-platform :choices t)))
                  (channel (test-candidate :channel "100")))
              (flet ((backups (args)
                       (multiple-value-list (nck::room-models-command host channel args "chat-100"))))
                (destructuring-bind (text controls) (backups "backups")
                  (is (search "## Backup Models" text))
                  (is (search "No backups" text))
                  (is (= 5 (length controls)) "three menus, the triggers, Test and back: Discord's five rows")
                  (is (equal '("p2/m2" "None") (mapcar #'first (third (first controls)))) "the room's own model is never its backup"))
                (backups "backups 1 p2 m2")
                (is (equal '((:provider "p2" :model "m2")) (nle::failover-chain)) "a pick writes the chain")
                (destructuring-bind (text controls) (backups "backups off refusal")
                  (is (equal '("rate_limit" "outage" "slow_start" "broken_answer") (nle::failover-triggers)) "a toggle writes the triggers")
                  (is (search "1. p2/m2: never answered" text))
                  (is (search "Switches on: rate limit, outage, slow start, broken answer." text))
                  (is (equal '("refusal" "/models backups on refusal" :secondary nil)
                             (fifth (press-lines (fourth controls)))) "an off trigger offers to switch it on"))
                (backups "backups 1 none")
                (is (null (nle::failover-chain)) "None takes the backup out")))))))))

(deftest channel-operator-models-is-a-picker-where-messages-carry-choices ()
  ;; Bare /models where messages carry choices is OpenClaw's picker: a menu
  ;; of the providers, then of one provider's models with the way back and
  ;; the way to the default, each step a /models line its press says, so a
  ;; pick is that line typed. Where messages carry none, the words stay, and
  ;; a lone provider lists its models.
  (with-temp-store ()
    (with-catalog
      (with-stubbed-fdefinition (nle:gateway-target (session-id)
                                  (values "p1" "m1"))
        (let ((host (test-host :name "models-picker" :platform (test-platform :choices t)))
              (plain (test-host :name "models-words"))
              (channel (test-candidate :channel "100")))
          (flet ((models (args &optional (host host))
                   (multiple-value-list (nck::room-models-command host channel args "chat-100"))))
            (destructuring-bind (text controls) (models "")
              (is (search "## Model Picker" text))
              (is (search "Current model: p1/m1" text) "the organism's default, named")
              (is (search "Select a provider (2 available)." text))
              (is (equal '((:menu "Select provider"
                            (("p1" "/models p1" "2 models · current" nil)
                             ("p2" "/models p2" "2 models" nil)))
                           (("Backups (0)" "/models backups" :secondary nil)))
                         (press-lines controls))))
            (destructuring-bind (text controls) (models "p2")
              (is (search "Default: p1/m1" text))
              (is (search "Select a p2 model." text))
              (is (null (search "listing" text)) "a listing that answered says nothing of it")
              (is (equal '(:menu "Select p2 model"
                           (("m2" "/models p2 m2" nil nil)
                            ("shared" "/models p2 shared" nil nil)))
                         (press-lines (first controls))))
              ;; A room with no pick of its own has no default to go back to.
              (is (equal '(("Providers" "/models" :secondary nil)
                           ("Reset to default" "/models default" :secondary t))
                         (press-lines (second controls)))))
            (is (search "this channel runs p2/m2 from its next ask" (first (models "p2 m2"))))
            (destructuring-bind (text controls) (models "p2")
              (is (search "Current model: p2/m2" text))
              (is (fourth (first (third (first controls)))) "the room's pick is shown picked")
              (is (null (fourth (second (second controls)))) "and the default is open"))
            ;; A thread running its channel's pick has none of its own to
            ;; reset; once it picks, it does, and the reset goes back to the
            ;; channel's.
            (let ((thread (test-candidate :channel "100" :thread "42" :parent "100")))
              (flet ((thread-models (args)
                       (multiple-value-list (nck::room-models-command host thread args "chat-100-t42"))))
                (destructuring-bind (text controls) (thread-models "p2")
                  (is (search "Current model: p2/m2" text))
                  (is (fourth (second (second controls))) "the channel's pick is not the thread's to reset"))
                (is (search "this thread runs p2/shared" (first (thread-models "p2 shared"))))
                (is (null (fourth (second (second (second (thread-models "p2")))))))
                (is (search "this thread runs p2/m2 from its next ask" (first (thread-models "default"))))))
            (is (search "runs the organism's default from its next ask" (first (models "default"))))
            (is (null (second (models "" plain))) "no choices, no card")
            (is (search "p1: 2 models" (first (models "p1" plain))))))))
    ;; A menu shows 25; a longer list pages, Prev and Next each saying the
    ;; page they go to.
    (is (equal '((25 26 27 28 29) 2 2)
               (multiple-value-list (nck::picker-page (loop for i below 30 collect i) 7))))
    (is (equal '((("Prev" "/models p1 page 1" :secondary nil)
                  ("Page 2/2" "nck:page" :secondary t)
                  ("Next" "/models p1 page 3" :secondary t)))
               (press-lines (nck::picker-pager "/models p1" 2 2))))
    (is (null (nck::picker-pager "/models" 1 1)))))

(deftest channel-operator-a-models-pick-asks-its-effort ()
  ;; A /models pick asks its reasoning effort straight through, as the
  ;; shells' own /models does (OPEN-MODELS-REASONING-PICKER): no need to
  ;; type /think, which stays the quick way to change it again later. A
  ;; model the catalog says does not reason gets no such card, and nor does
  ;; a platform whose messages carry no choices, which has no way to show
  ;; one.
  (with-temp-store ()
    (with-catalog
      (with-stubbed-fdefinition (nle::effort-ladder (provider model)
                                  (cond ((equal model "shared") nil)
                                        (t '("off" "low" "high"))))
        (let ((host (test-host :name "models-effort" :platform (test-platform :choices t)))
              (plain (test-host :name "models-effort-words"))
              (channel (test-candidate :channel "100")))
          (flet ((models (args &optional (host host))
                   (multiple-value-list (nck::room-models-command host channel args "chat-100"))))
            (destructuring-bind (text controls) (models "p2 m2")
              (is (search "this channel runs p2/m2 from its next ask" text))
              (is (search (format nil "~%~%## Reasoning Effort~%Target: p2/m2") text))
              (is (equal '((:menu "Select effort"
                            (("off" "/think off" "Answer without reasoning." nil)
                             ("low" "/think low" "" nil)
                             ("high" "/think high" "" nil)))
                           ("Provider default" "/think default" :secondary t))
                         (press-lines controls)) "the same card /think opens"))
            ;; A model the catalog says does not reason: the words alone.
            (is (equal (list (format nil "this channel runs p2/shared from its next ask; the organism's default is unchanged"))
                       (models "p2 shared")))
            ;; A platform whose messages carry none: the words alone either way.
            (is (equal (list (format nil "this channel runs p2/m2 from its next ask; the organism's default is unchanged"))
                       (models "p2 m2" plain)))))))))

(deftest channel-operator-effort-is-a-room-picker ()
  ;; Discord's /think mirrors /models: the bare command opens a private menu,
  ;; a pick belongs to this room, and a thread inherits its channel's pick.
  (with-temp-store ()
    (with-stubbed-fdefinition (nle:gateway-target (session-id)
                                (values (or session-id "p1") "m1"))
      (with-stubbed-fdefinition (nle::effort-ladder (provider model)
                                  (if (or provider model)
                                      '("off" "low" "high")
                                      '("off" "low" "high")))
        (let* ((host (test-host :name "effort-picker" :platform (test-platform :choices t)))
               (channel (test-candidate :channel "100"))
               (thread (test-candidate :channel "100" :thread "42" :parent "100")))
          (flet ((think (candidate args room)
                   (multiple-value-list (nck::room-effort-command host candidate args room))))
            (destructuring-bind (text controls) (think channel "" "chat-100")
              (is (search "## Reasoning Effort" text))
              (is (search "Current: provider default" text))
              (is (equal '((:menu "Select effort"
                            (("off" "/think off" "Answer without reasoning." nil)
                             ("low" "/think low" "" nil)
                             ("high" "/think high" "" nil)))
                           ("Provider default" "/think default" :secondary t))
                         (press-lines controls))))
            (is (search "uses p1/m1 at high from its next ask"
                        (first (think channel "high" "chat-100"))))
            (destructuring-bind (text controls) (think channel "" "chat-100")
              (is (search "Current: high" text))
              (is (fourth (third (third (first controls)))) "the selected effort is marked current")
              (is (null (fourth (second controls))) "provider default is not disabled after a pick"))
            (is (search "Current: high" (first (think thread "" "chat-100-t42"))))
            (is (search "follows p1/m1's provider default"
                        (first (think thread "default" "chat-100-t42"))))
            (is (equal "high" (nck::lane-effort host "chat-100-t42" '(:channel-id "100" :thread-id "42"))) "clearing a thread reveals the inherited channel pick")))))))

(deftest channel-operator-a-room-effort-reaches-its-lane ()
  ;; The room setting is projected onto each lane's durable model selection
  ;; just before its turn starts, and clearing it removes the lane pick.
  (with-temp-store ()
    (with-stubbed-fdefinition (nle::effort-ladder (provider model)
                                (if (or provider model)
                                    '("off" "low" "high")
                                    '("off" "low" "high")))
      (let* ((host (test-host :name "lane-effort"))
             (ask (nck:make-ask :room "chat-100" :lane "chat-100-m5"
                                :target '(:channel-id "100"))))
        (nlk:create-session :id "chat-100-m5")
        (nck::set-room-effort host "chat-100" "high")
        (nck::apply-lane-effort host ask)
        (is (equal "high" (nlk:session-model-effort "chat-100-m5")))
        (nck::set-room-effort host "chat-100" nil)
        (nck::apply-lane-effort host ask)
        (is (null (nlk:session-model-effort "chat-100-m5")))))))

(deftest channel-operator-a-lane-runs-on-its-rooms-model ()
  ;; A lane is a session of its own, and a pin does not ride a fork: the
  ;; lane is pinned to its room's model at each ask, and let go of it when
  ;; the room goes back to the organism's default.
  (with-temp-store ()
    (let ((host (test-host :name "lane-model"))
          (ask (nck:make-ask :room "chat-100" :lane "chat-100-m5" :target '(:channel-id "100"))))
      (nlk:create-session :id "chat-100-m5")
      (flet ((pin () (multiple-value-list (nlk:session-model-selection "chat-100-m5"))))
        (nck::apply-lane-model host ask)
        (is (equal '(nil nil) (pin)))
        (nck::set-room-model host "chat-100" "p1" "m1")
        (nck::apply-lane-model host ask)
        (is (equal '("p1" "m1") (pin)))
        (nck::set-room-model host "chat-100" nil nil)
        (nck::apply-lane-model host ask)
        (is (equal '(nil nil) (pin)))))))

(deftest channel-operator-a-room-sets-what-its-answers-are-said-in ()
  ;; channels.<id>.voice_replies is every room's until /voice on, tts or off
  ;; sets a room's own; a thread of the room hears as the room does until it
  ;; says otherwise. A word that is none of the three, and a room no running
  ;; channel holds, are refused in their own words.
  (with-temp-store ()
    (let* ((host (test-host :name "room-voices"))
           (nck::*hosts* (list host))
           (thread '(:channel-id "100" :thread-id "42")))
      (flet ((heard (room target) (nck::lane-voice-replies host room target)))
        (is (equal "off" (heard "chat-100" '(:channel-id "100"))))
        (is (search "every answer" (nck:set-voice-replies "chat-100" "tts")))
        (is-each (heard)
          ("chat-100" '(:channel-id "100") "tts" "the room's own")
          ("chat-100-t42" thread "tts" "a thread hears as its room"))
        (nck:set-voice-replies "chat-100-t42" "off")
        (is-each (heard)
          ("chat-100-t42" thread "off" "until it says otherwise")
          ("chat-100" '(:channel-id "100") "tts" "and its room keeps its own"))
        (is (search "not \"loud\"" (refusal-text error (nck:set-voice-replies "chat-100" "loud"))))
        (is (search "no room" (refusal-text error (nck:set-voice-replies "tui-1" "on"))))))))

(deftest channel-operator-a-stranger-is-given-a-code-and-let-in ()
  ;; A direct message from someone no allowlist names is answered with a
  ;; pairing code; the operator hears who asked, never the code, which is
  ;; what the person must bring them; /channels pair CODE lets them in and
  ;; /channels unpair takes it back.
  (with-temp-store ()
    (let* ((executor (nck:make-recording-executor))
           (host (operator-host :executor executor :pairing t :name "pairing"))
           (told '())
           (dm (test-candidate :channel "d7" :kind "direct_message" :user "u7" :user-name "sai")))
      (setf (nck:host-policy host) (channel-policy :allowed-users '("mike")))
      (nlk:with-cleanup ((nck:stop-host host)
                         (remhash "test" nck::*paired-users*)
                         (clrhash nck::*pairing-asked*)
                         (clrhash nck::*pairing-codes*))
        (with-stubbed-fdefinition (nle:notice (text &rest keys) (push text told))
          (is (eq :reject (nck:handle-candidate host dm)))
          (is-present (offer (await-plan executor :path "/rooms/d7/messages" :content "pairing code")) "a code"
            (let ((code (ppcre:scan-to-strings "[A-HJ-NP-Z2-9]{8}" (plan-content offer))))
              (is (stringp code))
              (is (search "sai (u7) wrote to the bot" (first told)))
              (is (not (search code (first told))) "the code is the person's to bring")
              ;; Asking again at once gives no second code.
              (nck:handle-candidate host dm)
              (sleep 0.3)
              (is (= 1 (length (plans-matching (recorded-plans executor) :content "pairing code"))))
              (is (search "no such pairing code" (nck::channels-command "pair ZZZZZZZZ")))
              (is (equal "test: sai (u7) is let in"
                         (nck::channels-command (format nil "pair ~(~a~)" code))))
              (is (await-plan executor :path "/rooms/d7/messages" :content "You're in"))
              (is-inbound (nck:host-policy host) dm :answer nil "paired")
              (is (search "sai (u7)" (nck::channels-command "paired")))
              (is (search "u7 is no longer let in" (nck::channels-command "unpair u7")))
              (is-inbound (nck:host-policy host) dm :reject "user_not_allowed")
              (is (null (nck::paired host))))))))))

(deftest channel-operator-pairing-on-the-operators-page (with-temp-gateway (port))
  ;; /api/channels lists, per channel, who asks to be let in and who is, and
  ;; moves them through PAIR and UNPAIR: an ask is approved by the id the page
  ;; was given, never by a code it was sent, and a code typed in still works.
  (with-kit-cell ("{\"channels\": {}}")
    (let* ((executor (nck:make-recording-executor))
           (host (operator-host :executor executor :pairing t :name "page-pairing"))
           (ana (test-candidate :channel "d8" :kind "direct_message" :user "u8" :user-name "ana"))
           (bo (test-candidate :channel "d9" :kind "direct_message" :user "u9" :user-name "bo")))
      (setf (nck:host-policy host) (channel-policy :allowed-users '("mike")))
      (nlk:with-cleanup ((nck:stop-host host)
                         (remhash "test" nck::*paired-users*)
                         (clrhash nck::*pairing-asked*)
                         (clrhash nck::*pairing-codes*))
        (with-stubbed-fdefinition (nle:notice (text &rest keys) nil)
          (flet ((code-of (dm room)
                   (nck:handle-candidate host dm)
                   (ppcre:scan-to-strings "[A-HJ-NP-Z2-9]{8}"
                                          (plan-content (await-plan executor :path (format nil "/rooms/~a/messages" room)
                                                                    :content "pairing code"))))
                 (pairing (body)
                   (find "test" (nlk:json-value body :array "pairing")
                         :key (lambda (host) (nlk:json-value host :string "id")) :test #'equal)))
            (let ((ana-code (code-of ana "d8"))
                  (bo-code (code-of bo "d9")))
              (with-gateway-http (port :get "/api/channels")
                (is (= 200 status))
                (let ((sent (nlk:encode-json-object body)))
                  (is (notany (lambda (code) (search code sent)) (list ana-code bo-code)) "never a code"))
                (is-present (asks (nlk:json-value (pairing body) :array "asks")) "the channel's asks"
                  (is (equal '("ana" "bo") (map 'list (lambda (ask) (nlk:json-value ask :string "name")) asks)))
                  (is (equal "u8" (nlk:json-value (aref asks 0) :string "user")))
                  (is (<= 3590 (nlk:json-value (aref asks 0) :integer "seconds_left") 3600) "an hour, less its age")
                  (is (eq t (nlk:json-value (pairing body) :any "on")))
                  ;; Approved by its id: ana is let in, and bo still asks.
                  (with-gateway-http (port :post (format nil "/api/channels?op=pair&ask=~a"
                                                         (nlk:json-value (aref asks 0) :string "id")))
                    (is (equal "test: ana (u8) is let in" (nlk:json-value body :string "text")))
                    (is (equal '("u8") (map 'list (lambda (person) (nlk:json-value person :string "id"))
                                            (nlk:json-value (pairing body) :array "paired"))))
                    (is (= 1 (length (nlk:json-value (pairing body) :array "asks")))))))
              (is-inbound (nck:host-policy host) ana :answer nil "paired from the page")
              ;; A code the person brought; one nobody was given.
              (with-gateway-http (port :post "/api/channels?op=pair&code=ZZZZZZZZ")
                (is (search "no such pairing code" (nlk:json-value body :string "text"))))
              (with-gateway-http (port :post (format nil "/api/channels?op=pair&code=~(~a~)" bo-code))
                (is (equal "test: bo (u9) is let in" (nlk:json-value body :string "text")))
                (is (zerop (length (nlk:json-value (pairing body) :array "asks"))))))
            (with-gateway-http (port :post "/api/channels?op=unpair&user=u8")
              (is (search "u8 is no longer let in" (nlk:json-value body :string "text")))
              (is (equal '("u9") (map 'list (lambda (person) (nlk:json-value person :string "id"))
                                      (nlk:json-value (pairing body) :array "paired")))))
            (is-inbound (nck:host-policy host) ana :reject "user_not_allowed")))))))

(deftest channel-operator-the-home-channel-hears-the-bot ()
  ;; /sethome typed in a room makes it where the bot tells the operator
  ;; what they need to hear; from a shell it says where each home is.
  (with-temp-store ()
    (let* ((executor (nck:make-recording-executor))
           (host (operator-host :executor executor :name "home")))
      (nlk:with-cleanup ((nck:stop-host host))
        (with-stubbed-fdefinition (nle:notice (text &rest keys) nil)
          (is (search "test: no home channel" (nck::sethome-command "" "s-shell")))
          (is (search "this is the home channel now" (nck::sethome-command "" "chat-100-t42")))
          (is (equal '("100" "42") (let ((home (nck::home-target host)))
                                     (list (getf home :channel-id) (getf home :thread-id)))))
          (nck::tell-operator host "sai was refused /stop")
          (is (await-plan executor :path "/rooms/42/messages" :content "sai was refused /stop"))
          ;; The organism's note, in the model's words: a boot that is back, a
          ;; release it runs for the first time (NLE:ORGANISM-NOTE). It comes
          ;; as a frame, and again at a host's start while it is fresh: the
          ;; home channel hears it once.
          (with-saved-globals ((nck::*home-notes* (make-hash-table :test #'equal :synchronized t)))
            (nck::observe-frame host (list :kind "organism.note" :session-id nil
                                           :payload (nlk:json-object "text" "Back after a 3m update, now on 0.0.1+tv8.")))
            (is (await-plan executor :path "/rooms/42/messages" :content "Back after a 3m update, now on 0.0.1+tv8."))
            (with-stubbed-fdefinition (nle:organism-note () "Back after a 3m update, now on 0.0.1+tv8.")
              (let ((again (operator-host :executor executor :name "home")))
                (nlk:with-cleanup ((nck:stop-host again))
                  ;; A host starting while the note is fresh does not post it again.
                  (sleep 0.5)
                  (is (= 1 (length (plans-matching (recorded-plans executor)
                                                   :content "Back after a 3m update, now on 0.0.1+tv8.")))))))
            ;; One that started before the note came posts it at its start.
            (with-stubbed-fdefinition (nle:organism-note () "Back after 5s.")
              (let ((late (operator-host :executor executor :name "home")))
                (nlk:with-cleanup ((nck:stop-host late))
                  (is (await-plan executor :path "/rooms/42/messages" :content "Back after 5s."))))))
          (is (search "no home channel now" (nck::sethome-command "off" "chat-100")))
          (is (null (nck::home-target host))))))))

(deftest channel-operator-a-turn-on-a-reaped-lane-is-answered-where-it-lives ()
  ;; A cron fire an hour on, a background job's wake: a turn starting on a
  ;; lane the reaper let go is taken up from the lane's id and answered in
  ;; its room, and it holds a slot while it runs.
  (with-temp-store ()
    (nlk:create-session :id "chat-123")
    (nlk:create-session :id "chat-123-m1" :parent "chat-123")
    (with-digest-host (host nil :responses (replies "s1" "a1"))
      (fact "turn.started" (nlk:json-object "input" "cron fire"))
      (is-present (lane (nck:find-lane (nck:host-lanes host) "chat-123-m1")) "the lane is back"
        (is (equal "123" (getf (nck:lane-target lane) :channel-id)))
        (is (equal "1" (nck:lane-trigger-message-id lane))))
      (said "the nightly report")
      (fact "turn.completed" (nlk:json-object))
      (is (await-plan executor :path "/rooms/123/messages" :content "the nightly report"))
      ;; A session that names no lane of the host's is none of its business.
      (fold-fact host "s-elsewhere" "turn.started" (nlk:json-object))
      (is (null (nck:find-lane (nck:host-lanes host) "s-elsewhere"))))))
