;;;; plan.lisp --- what maps, through which seam, and the report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A plan is the list of ITEMs an import would make, one per piece of the
;;;; homes it read, each with a status — `imported' (would, or did),
;;;; `skipped' with its reason, `conflict' where this organism already has
;;;; one and --overwrite was not said, `error' — and, for the imported
;;;; ones, the action that lands it. A dry run is the plan without its
;;;; actions run; APPLY-PLAN runs them in order and turns a failure into
;;;; that item's error, never a stop. Every action writes through the seam
;;;; that owns the destination: NLE:CONFIG-SET for the shared config, the
;;;; auth writer for keys, the scribe for memories and skills, which become
;;;; definitions in the knowledge cell, the cron folder's own verbs, the
;;;; store's CREATE-SESSION and RECORD-EXCHANGE-TURN with the exchange's
;;;; own clock. A folder an item needs is installed, loaded and started on
;;;; the way (ENSURE-CELL), so a fresh box imports whole.
;;;;
;;;; A plan reads MORE THAN ONE home. A real box carries several — the
;;;; keys are in one, the skills in another, the bots in a third — and
;;;; picking a single source throws away most of what the operator has. So
;;;; the homes are merged, newest first (by the manifest's last-used path),
;;;; and the merge rules are the same for every world:
;;;;
;;;;   providers      union; the first world to name a provider wins it, and
;;;;                  an endpoint never lands under a built-in's name
;;;;   default model  the one the homes ran most, lately, on a key that comes
;;;;                  over (tally.lisp); else the newest world's pick whose
;;;;                  provider can be reached
;;;;   MCP servers    deduped by name and command
;;;;   instructions   deduped by content hash
;;;;   skills         first by world; a later collision is a conflict
;;;;   memory         deduped by name, or by content hash for an unnamed entry
;;;;   usage          every world's ledger lines, once
;;;;   channels       one section per bot: the same token in two worlds is one
;;;;                  bot, and a different token is another, left out by name
;;;;   sessions       every world's, newest first, under the budget
;;;;
;;;; and at every one of them a fact THIS organism already holds wins: an
;;;; import never clobbers what is here.
;;;;
;;;; Secrets move with everything else: a key lands in auth.json, a bot
;;;; token in a 0600 file under this home's secrets/ — and the report never
;;;; shows a value. A migration that leaves the keys behind leaves nothing
;;;; working. Two things are never brought: an OAuth grant, which is the
;;;; other harness's client's, and an MCP server's secret env or header,
;;;; which the mcp folder reads from config.jsonc alone; it stays in its
;;;; home and is named.

(in-package #:nodecode.import)

(nlk:access (any fact) (fact fact) (first fact) (row nlk::detected-world))

;;; Profiles last: each is a home of its own, created empty here and imported
;;; under its own launch.
(defparameter +kinds+
  '("providers" "instructions" "memory" "skills" "usage" "standing" "mcp" "channels" "cron" "sessions"
    "profiles")
  "The pieces, in the order they are planned and applied: what later ones
stand on — a provider before the default model that names it, the sections
before the folders that read them — comes first.")

(nlk:define-record (item (:copier nil) (:export kind status reason source destination))
  "One thing the import would do."
  ;; STATUS is imported, skipped, conflict or error; REASON says why for the
  ;; last three and what for the first; DETAIL a plist of counts the report
  ;; shows; ACTION the thunk that lands it, or KEEP for a definition: its
  ;; text and the ledger's seed, (TEXT &key AT HOT SEED), kept with its run.
  kind status reason source destination world (detail '()) action keep)

(nlk:define-record (plan (:copier nil) (:constructor %make-plan) (:export items))
  "One import: the HOMES read newest first, the switches (OVERWRITE,
TAKEOVER, ONLY, WITHOUT), the ITEMS (oldest first once built), what says a
foreign gateway is RUNNING, whether the takeover STOPPED it, and where the
report went."
  (homes '()) settings overwrite (takeover t) only without (items '()) (running '())
  stopped applied report-path)

(defun add-item (plan kind status &key fact reason (source (and fact fact.source))
                                       destination (world (and fact fact.world)) detail action keep)
  (first (push (make-item :kind kind :status status :reason reason :source source
                          :destination destination :world world :detail detail :action action
                          :keep keep)
               plan.items)))

(defun kind-wanted-p (plan kind)
  "Whether the plan reads and lands KIND: named by ONLY when ONLY names
anything, and never named by WITHOUT."
  (and (or (null plan.only)
           (member kind plan.only :test #'string-equal))
       (not (member kind plan.without :test #'string-equal))))

(defun kind-list (text)
  "A comma- or blank-separated list of kind names, empties dropped."
  (and text (remove "" (uiop:split-string text :separator ", ") :test #'string=)))

(defun plan-facts (plan &optional kind)
  "Every fact the plan's homes hold, newest home first; KIND narrows."
  (let ((facts (loop for home in (plan-homes plan) append home.facts)))
    (if kind (remove kind facts :key #'fact-kind :test-not #'eq) facts)))

(defun plan-takeover-units (plan)
  "The systemd user units the plan's worlds declare, deduped."
  (remove-duplicates (loop for home in (plan-homes plan)
                           for world = home.world
                           when (and world world.takeover-unit) collect it)
                     :test #'string=))

(defun plan-takeover-unit (plan)
  "The one unit a takeover stops: the first of the plan's units that RUNNING
says is up, or NIL when none is — a gateway outside its unit is not stopped
from here."
  (find-if (lambda (unit) (some (lambda (line) (cl:search unit line)) plan.running))
           (plan-takeover-units plan)))

(defun merge-facts (facts &key fill union (from-end t) &aux (first (first facts))
                                                            (value (copy-list first.value)))
  "The facts of one id as one: each FILL key from the first fact that carries
it, each UNION key's lists joined, the sources named together, a repeat at its
first mention (its last when FROM-END is NIL)."
  (dolist (fact (rest facts))
    (dolist (key fill)
      (unless (getf value key) (setf (getf value key) (getf fact.value key))))
    (dolist (key union)
      (setf (getf value key)
            (nlk:distinct (append (getf value key) (getf fact.value key))))))
  (make-fact :kind first.kind :id first.id :world first.world :value value
             :source (format nil "~{~a~^, ~}"
                             (remove-duplicates (mapcar #'fact-source facts)
                                                :test #'equal :from-end from-end))))

(defun merged-facts (plan kind &key fill union (from-end t) (facts (plan-facts plan kind))
                     &aux (groups '()))
  "FACTS (KIND's, by default) one per id, in the order the newest home first
names them, each id's merged by MERGE-FACTS."
  ;; A home splits what one id means: an endpoint in one file and its key in
  ;; another, a bot's token in the environment and its manners in a config.
  (dolist (fact facts)
    (nlk:if-let (group (assoc fact.id groups :test #'equal))
      (nconc group (list fact))
      (push (list fact.id fact) groups)))
  (loop for (nil . group) in (nreverse groups)
        collect (merge-facts group :fill fill :union union :from-end from-end)))

(defun fact-with (fact source &rest members &aux (value (copy-list fact.value)))
  "FACT, named as read from SOURCE (its own when NIL), with MEMBERS, a plist,
set in its value."
  (loop for (key member) on members by #'cddr do (setf (getf value key) member))
  (make-fact :kind fact.kind :id fact.id :world fact.world :source (or source fact.source)
             :value value))

;;; --- providers ----------------------------------------------------------------

(defun plan-provider-entry (plan fact id sdk base-url models &key why dialers)
  "One providers.<ID> item: WHY says where an endpoint the home did not name
came from, DIALERS the endpoints a key's name could have meant."
  ;; A provider this organism's catalog already serves needs no entry at all —
  ;; its lane and endpoint resolve from the catalog — so one is written only
  ;; for an endpoint the catalog does not carry, or a wire it would get wrong.
  (unless (and (catalog-entry id)
               (or (null base-url)
                   (equal (catalog-normalize-base base-url) (catalog-normalize-base (catalog-base id))))
               (or (null sdk) (equal sdk (catalog-sdk id))))
    (multiple-value-bind (status reason)
        (cond ((null sdk)
               (values "skipped" (format nil "no endpoint in the home~:[ and models.dev does not carry ~a~;~:* and ~{~a~^ and ~} dial its domain different ways~*~]: the key still lands, add providers.~a.base_url and .sdk to reach it"
                                         (mapcar (lambda (any)
                                                   (destructuring-bind (&key base sdk &allow-other-keys) (fact-value any)
                                                     (format nil "~a's ~a (~a, ~a)" (world-label (fact-world any)) (fact-id any) base sdk)))
                                                 dialers)
                                         id id)))
              ((and (config-present-p "providers" id) (not (plan-overwrite plan)))
               (values "conflict" "providers entry already here (--overwrite replaces its members)"))
              (t (values "imported"
                         (format nil "sdk ~a~@[, ~d model~:p~]~@[, ~a~]" sdk (and models (length models)) why))))
      (add-item plan "providers" status :fact fact :reason reason
                :destination (format nil "providers.~a" id)
                :action (lambda ()
                          (nle:config-set
                           (list "providers" id)
                           (nlk:json-object
                            "sdk" sdk
                            :opt "base_url" base-url
                            :when models "models"
                            (coerce (mapcar (lambda (id) (nlk:json-object "id" id)) models) 'vector))))))))

(defun endpoint-domain (url &aux (host (endpoint-host url)))
  "URL's domain label -- `subrouter' for https://api.subrouter.ai/v1 -- or
NIL for an address with none."
  (and host (notevery (lambda (ch) (or (digit-char-p ch) (char= ch #\.))) host)
       (let ((labels (uiop:split-string host :separator ".")))
         (and (rest labels) (nth (- (length labels) 2) labels)))))

(defun inferred-endpoint (plan fact &aux (value (fact-value fact)))
  "=> (values BASE SDK DIALERS) for FACT, a key read with no endpoint: the
endpoint another provider of the homes dials on the domain the key's
variable names -- SUBROUTER_AICOST_API_KEY, subrouter.ai -- when they all
dial it one way; else NIL, NIL and each provider that dials it, so none is
guessed."
  (let* ((word (string-downcase (first (uiop:split-string (or (getf value :env) (fact-id fact))
                                                          :separator "_-"))))
         (dialers (remove-if-not (lambda (any &aux (base (getf (fact-value any) :base)))
                                   (and base (getf (fact-value any) :sdk) (equal word (endpoint-domain base))))
                                 (plan-facts plan :provider)))
         (ways (remove-duplicates (mapcar (lambda (any)
                                            (list (catalog-normalize-base (getf (fact-value any) :base))
                                                  (getf (fact-value any) :sdk)))
                                          dialers)
                                  :test #'equal)))
    (if (and ways (null (rest ways)))
        (values (getf (fact-value (first dialers)) :base) (getf (fact-value (first dialers)) :sdk) dialers)
        (values nil nil dialers))))

(defun joined-provider-facts (plan &aux (facts (plan-facts plan :provider)) (spent '()))
  "The plan's :PROVIDER facts, an endpoint that names the variable its key is
read from holding that key: what a home keeps in its environment under
`MOCK_KEY' is the key of the endpoint that says `MOCK_KEY', whatever provider
the variable's name would have filed it under. A key another id's endpoint took
lands once, under that endpoint."
  (flet ((key-holder (name)
           (find-if (lambda (fact) (and (equal name (getf fact.value :env)) (getf fact.value :key)))
                    facts)))
    (remove-if (lambda (fact) (member fact spent))
               (mapcar (lambda (fact &aux (name (getf fact.value :key-env))
                                          (holder (and name (null (getf fact.value :key)) (key-holder name))))
                         (cond ((null holder) fact)
                               ;; The same id merges by itself; another's key is spent here.
                               (t (let ((same (equal (fact-id holder) fact.id)))
                                    (unless same (pushnew holder spent))
                                    (fact-with fact (and (not same)
                                                         (format nil "~a, ~a" fact.source (fact-source holder)))
                                               :key (getf (fact-value holder) :key))))))
                       facts))))

(defun homed-model-facts (plan)
  "The plan's :MODEL facts, a pick that names the provider one endpoint of its
world was moved off (PROVIDER-FACT) naming the id that endpoint landed under."
  (let ((moved (loop for fact in (plan-facts plan :provider)
                     when (getf fact.value :claimed)
                       collect (list fact.world (getf fact.value :claimed) fact.id))))
    (mapcar (lambda (fact &aux (to (remove-duplicates
                                    (loop for (world claimed id) in moved
                                          when (and (equal world fact.world)
                                                    (equal claimed (getf fact.value :provider)))
                                            collect id)
                                    :test #'equal)))
              (if (and to (null (rest to))) (fact-with fact nil :provider (first to)) fact))
            (plan-facts plan :model))))

(defun plan-providers (plan &aux (landed '()))
  ;; A box with no catalog cache yet: the plan needs its rows, so wait on the fetch.
  (unless (nle::models-catalog-table)
    (nlk:when-let (fetch (nle::refresh-models-catalog))
      (ignore-errors (bt2:join-thread fetch))))
  ;; One entry per id, newest world first, the halves a home splits (endpoint, key) rejoined.
  (dolist (fact (merged-facts plan :provider :fill '(:key :key-env :base :sdk :models)
                                   :facts (joined-provider-facts plan)))
    (destructuring-bind (&key id key key-env base sdk models &allow-other-keys) fact.value
      ;; A named environment variable is a key only while that variable is
      ;; actually set here: the name alone moves nothing.
      (when (and (null key) key-env)
        (setf key (uiop:getenv key-env)))
      (cond
        ((and (null key) (null base))
         (add-item plan "providers" "skipped" :fact fact
                   :destination (format nil "providers.~a" id)
                   :reason (if key-env
                               (format nil "its key is read from ~a, which is not set here"
                                       key-env)
                               "no key and no endpoint of its own")))
        (t
         ;; A key the home named no endpoint for, which the catalog does
         ;; not carry: another home's endpoint on the domain its name says.
         (multiple-value-bind (inferred wire dialers)
             (and key (null base) (null sdk) (not (catalog-entry id)) (inferred-endpoint plan fact))
           (plan-provider-entry plan fact id (or sdk wire) (or base inferred) models
                                :why (and inferred
                                          (format nil "endpoint ~a from ~a's ~a, on the domain ~a names"
                                                  inferred (world-label (fact-world (first dialers)))
                                                  (fact-id (first dialers)) (or (getf fact.value :env) id)))
                                :dialers (and (null inferred) (rest dialers) dialers)))
         (when (and key (plusp (length key)))
           (push id landed)
           (if (and (auth-key-present-p id) (not plan.overwrite))
               (add-item plan "providers" "conflict" :fact fact
                         :reason "a key for it is already in auth.json (--overwrite replaces it)"
                         :destination (format nil "auth.json api_keys.~a" id))
               (add-item plan "providers" "imported" :fact fact
                         :reason (format nil "key from ~a" fact.source)
                         :destination (format nil "auth.json api_keys.~a" id)
                         :action (lambda () (nle::save-provider-api-key id key)))))))))
  ;; The grants that stay behind, so the operator knows what to connect again.
  (dolist (fact (remove-duplicates (plan-facts plan :oauth) :key #'fact-id :test #'equal :from-end t))
    (add-item plan "providers" "skipped" :fact fact
              :reason (format nil "an OAuth grant of ~a's own client; connect ~a again in /setup"
                              fact.world fact.id)))
  (plan-default-model plan landed))

(defun placed-model-facts (plan landed)
  "The plan's :MODEL facts (HOMED-MODEL-FACTS), a pick that names no provider
placed on the one provider of LANDED that serves it -- its catalog, or the
home's own list for it -- under the id that provider serves it by, the name
the home wrote kept as :WRITTEN; the providers of a pick served by more than
one kept as :SERVING."
  ;; `opus' in Claude Code's settings beside the Anthropic key it keeps, or
  ;; `gpt-5.6' in Codex's beside its OpenAI key, is that provider's model:
  ;; dropped as naming no provider, it read as reaching none (am-04).
  (flet ((served (id name)
           (or (catalog-model-id id name)
               (and (some (lambda (fact) (and (equal id fact.id)
                                              (member name (getf fact.value :models) :test #'equal)))
                          (plan-facts plan :provider))
                    name))))
    (mapcar (lambda (fact &aux (name (getf fact.value :model)))
              (if (getf fact.value :provider)
                  fact
                  (let ((serving (loop for id in (reverse landed)
                                       for model = (served id name)
                                       when model collect (cons id model))))
                    (if (and serving (null (rest serving)))
                        (fact-with fact nil :provider (car (first serving)) :model (cdr (first serving))
                                            :written name)
                        (fact-with fact nil :serving (mapcar #'car serving))))))
            (homed-model-facts plan))))

(defun tallied-model-facts (plan landed &aux (keyed (plan-facts plan :provider))
                                             (signed (mapcar #'fact-world (plan-facts plan :oauth)))
                                             (picks (make-hash-table :test #'equal)))
  "The plan's :TALLY facts that ran on a key this import lands or this
organism holds, as :MODEL facts on the id that key is under, the tally as
:TALLY, one per provider and model, the most used first."
  ;; A row ran on the key its own world keeps for the provider it names: the
  ;; world's provider fact of that id, or of the name an endpoint was moved
  ;; off (CLAIMED). A row that names none -- Claude Code's ledger -- ran on
  ;; the one key its world keeps that serves the model, unless the world
  ;; keeps a sign-in too, which is what such a client runs on. A Claude Max
  ;; plan's tokens stay with Claude Code: they never pick a model to bill to
  ;; a key, Claude Code's own refused one or a plugin's.
  (dolist (fact (plan-facts plan :tally))
    (destructuring-bind (&key provider model score tokens days last effort &allow-other-keys) fact.value
      (let* ((ids (unless (and (null provider) (member fact.world signed :test #'equal))
                    (remove-duplicates
                     (loop for any in keyed
                           when (and (equal (fact-world any) fact.world)
                                     (if provider
                                         (or (equal (fact-id any) provider)
                                             (equal (getf (fact-value any) :claimed) provider))
                                         (catalog-model-id (fact-id any) model)))
                             collect (fact-id any))
                     :test #'equal)))
             (id (and ids (null (rest ids)) (first ids))))
        (when (and id (or (member id landed :test #'equal) (auth-key-present-p id)))
          (let* ((name (or (catalog-model-id id model) model))
                 (held (gethash (cons id name) picks)))
            (if held
                ;; the same model on the same key, run from two worlds
                (destructuring-bind (&key ((:score s)) ((:tokens n)) ((:days d)) ((:last l)) ((:effort e)) worlds)
                    (getf held :tally)
                  (setf (getf held :tally)
                        (list :score (+ s score) :tokens (+ n tokens) :days (+ d days) :last (max l last)
                              :effort (or e effort) :worlds (append worlds (list fact.world)))))
                (setf (gethash (cons id name) picks)
                      (list :model name :provider id :source fact.source :world fact.world
                            :tally (list :score score :tokens tokens :days days :last last
                                         :effort effort :worlds (list fact.world))))))))))
  (mapcar (lambda (pick)
            (destructuring-bind (&key model provider source world tally) pick
              (make-fact :kind :model :id model :world world :source source
                         :value (list :model model :provider provider :tally tally))))
          (sort (loop for pick being the hash-values of picks collect pick)
                #'> :key (lambda (pick) (getf (getf pick :tally) :score)))))

(defun model-key (name)
  "NAME as models compare across providers: no provider prefix, no `[1m]'
or `@default' tail, dots as dashes, lowercase."
  (let* ((name (subseq name (1+ (or (position #\/ name :from-end t) -1))))
         (name (subseq name 0 (or (position-if (lambda (ch) (member ch '(#\[ #\@))) name) (length name)))))
    (substitute #\- #\. (string-downcase name))))

(defun served-model-id (plan id name &aux (key (model-key name)))
  "The id provider ID serves the model NAME by, from its catalog or the list
its home gave (`anthropic/claude-opus-5.5' for claude-opus-5-5 on
openrouter), or NIL."
  (or (catalog-model-id id name)
      (nlk:when-let (models (nlk:json-value (nle::models-catalog-table) :object id "models"))
        (loop for model being the hash-keys of models
              when (equal key (model-key model)) return model))
      (loop for fact in (plan-facts plan :provider)
            thereis (and (equal id fact.id)
                         (find key (getf fact.value :models) :key #'model-key :test #'equal)))))

(defun passed-over-model (plan landed pick &aux (scores (make-hash-table :test #'equal)))
  "When the homes ran another model than PICK, the default model's :MODEL
fact, more than it, on a credential that stays behind: its reason, naming
the providers this import lands that serve it. Else NIL."
  ;; The default is deepseek, and the closing block says where Opus can run
  ;; here and that it bills per token.
  (dolist (fact (plan-facts plan :tally))
    (destructuring-bind (&key model score tokens days &allow-other-keys) fact.value
      (let ((held (alexandria:ensure-gethash (model-key model) scores
                    (list :model model :score 0 :tokens 0 :days 0 :worlds '()))))
        (loop for (key n) on (list :score score :tokens tokens :days days) by #'cddr
              do (incf (getf held key) n))
        (pushnew fact.world (getf held :worlds) :test #'equal))))
  (let ((top (first (sort (loop for held being the hash-values of scores collect held)
                          #'> :key (lambda (held) (getf held :score)))))
        (picked (gethash (model-key (getf (fact-value pick) :model)) scores)))
    (when (and top (not (eq top picked))
               (> (getf top :score) (if picked (getf picked :score) 0)))
      (destructuring-bind (&key model tokens days worlds &allow-other-keys) top
        (let ((routes (loop for id in (reverse landed)
                            for served = (served-model-id plan id model)
                            when served collect (format nil "~a as ~a" id served)))
              (signed (some (lambda (world) (find world (plan-facts plan :oauth) :key #'fact-world :test #'equal))
                            worlds)))
          (format nil "~a is what ~{~a~^ and ~} ran most lately (~a tokens over ~d day~:p), on ~:[a key that did not come over~;a sign-in that stays there~]; ~:[no key here serves it: /models connects one~;~:*~{~a~^, ~} serve~:[s~;~] it, billed per token: /models picks it~]"
                  model (mapcar #'world-label worlds) (token-count tokens) days signed
                  (subseq routes 0 (min 3 (length routes))) (rest routes)))))))

(defun token-count (tokens)
  "TOKENS as a report says it: 110M, 5.1B."
  (cond ((>= tokens 1000000000) (format nil "~,1fB" (/ tokens 1d9)))
        ((>= tokens 1000000) (format nil "~dM" (round tokens 1000000)))
        ((>= tokens 1000) (format nil "~dk" (round tokens 1000)))
        (t (format nil "~d" tokens))))

(defun world-label (world)
  "What a report calls the world named WORLD: its label, or the name itself."
  (nlk:if-let (row (nlk:find-agent-world world)) (nlk:agent-world-label row) world))

(defun plan-default-model (plan landed)
  (let* ((facts (sort (placed-model-facts plan landed)
                      #'< :key (lambda (fact) (or (getf fact.value :depth) 99))))
         ;; The model the homes ran most, lately, on a key that lands here;
         ;; else the shallowest pick of the newest home whose provider the
         ;; plan can actually reach.
         (fact (or (first (tallied-model-facts plan landed))
                   (find-if (lambda (fact &aux (provider (getf fact.value :provider)))
                              (and provider (member provider landed :test #'equal)))
                            facts)
                   (find-if (lambda (fact &aux (provider (getf fact.value :provider)))
                              (and provider (or (auth-key-present-p provider)
                                                (config-present-p "providers" provider))))
                            facts)
                   (find-if (lambda (fact &aux (value fact.value))
                              (nth-value 1 (nlk:json-value (nle::models-catalog-table)
                                                           :any (getf value :provider) "models" (getf value :model))))
                            facts))))
    (destructuring-bind (&key provider model written tally &allow-other-keys) (and fact fact.value)
      (cond
        ((null fact)
         (nlk:when-let (any (first (placed-model-facts plan landed)))
           (destructuring-bind (&key model provider serving &allow-other-keys) any.value
             (add-item plan "providers" "skipped" :fact any :destination "general.default_model"
                       :reason (if provider
                                   (format nil "~a names ~a, which no provider here can reach"
                                           model provider)
                                   (format nil "~a's ~a names no provider, and ~:[no key imported here serves it: pick a model in /models~;~:*~{~a~^ and ~} each serve it: pick one in /models~]"
                                           (world-label any.world) model serving))))))
        ((and (config-present-p "general" "default_model") (not (plan-overwrite plan)))
         (add-item plan "providers" "conflict" :fact fact :destination "general.default_model"
                   :reason (format nil "a default model is already set (--overwrite moves it to ~a/~a)"
                                   provider model)))
        (t
         (let ((effort (and tally (ignore-errors (nle::admitted-effort (getf tally :effort) provider model))))
               (passed (and tally (passed-over-model plan landed fact))))
           (add-item plan "providers" "imported" :fact fact :destination "general.default_model"
                     ;; A bare pick placed on its provider says so, and what it was read as.
                     :reason (cond (tally
                                    (destructuring-bind (&key tokens days last worlds &allow-other-keys) tally
                                      (format nil "~a on ~a~@[ at ~a effort~], the model ~{~a~^ and ~} ran most lately: ~a tokens over ~d day~:p of the last ~d, last on ~a"
                                              model provider effort (mapcar #'world-label worlds) (token-count tokens)
                                              days +tally-days+ (subseq (nlk:iso-time last) 0 10))))
                                   ((null written) (format nil "~a on ~a" model provider))
                                   ((string= written model)
                                    (format nil "~a on ~a, the one imported provider that serves it"
                                            model provider))
                                   (t (format nil "~a on ~a, for ~a's ~a"
                                              model provider (world-label fact.world) written)))
                     ;; SELECT-DEFAULT-MODEL, not the bare write: it makes the
                     ;; pick this image's selection as well as the config's, so
                     ;; the frame after a silent import is a conversation on the
                     ;; model the operator was already using rather than a shell
                     ;; that has to be restarted to see it. It also resolves the
                     ;; provider's lane first, so a pick this organism cannot
                     ;; serve is this item's error and not a failure at the
                     ;; first turn.
                     :action (lambda ()
                               (nlk:bind (((_ _ outcome detail) (nle::select-default-model
                                                                 :provider provider :model model)))
                                 (unless (eq outcome :written) (fail "~a" detail))
                                 ;; the effort it ran at, through /think's writer
                                 (when effort (nle::select-default-effort effort))
                                 ;; Every attached shell renders CACHED model
                                 ;; facts — the frame path never dials — so a
                                 ;; selection that landed anywhere but a shell's
                                 ;; own slash op stays invisible in its footer
                                 ;; until the next reconnect. This is the frame
                                 ;; that says the default moved.
                                 (ignore-errors
                                  (nle::broadcast-model-changed)))))
           (when passed
             (add-item plan "providers" "skipped" :destination "general.default_model" :reason passed))))))))

;;; --- instructions --------------------------------------------------------------

(defun plan-instructions (plan)
  ;; Two files, by what each home's file is (home.lisp +PERSONA-NAMES+): its
  ;; rules for every session land in this home's AGENTS.md, which every
  ;; session reads (context.lisp OPERATOR-RULES-SECTION) and refuses whole
  ;; past the core's +INSTRUCTIONS-BYTE-LIMIT+; a persona in its SOUL.md, the
  ;; voice the chat bots speak in (channels/kit/soul.lisp), which no terminal
  ;; session reads and a bot refuses whole past the harness budget. Each
  ;; file is held to its reader's own limit.
  (let ((facts (remove-duplicates (plan-facts plan :instructions)
                                  :key (lambda (fact) (getf fact.value :hash))
                                  :test #'equal :from-end t)))
    (loop for (name . keys)
            in `(("AGENTS.md" :what "rules every session reads" :reader "a session"
                              :limit ,nle::+instructions-byte-limit+ :measure ,#'nlk:utf-8-size)
                 ("SOUL.md" :what "the voice your chat bots speak in; a terminal session does not read SOUL.md"
                            :reader "a chat bot" :limit ,nlk::+max-harness-bytes+ :measure ,#'length
                            :persona t))
          do (apply #'plan-instructions-file plan name
                    (remove-if-not (lambda (fact) (eq (getf keys :persona) (getf fact.value :persona)))
                                   facts)
                    keys))))

;;; A rules file is what the operator wrote, then one section a home's file,
;;; each under the heading RULES-HEADING writes: a later import adds its own
;;; after them, and --overwrite replaces that one section, never the rest. A
;;; persona is one voice: a SOUL.md holding the operator's own is not added
;;; to unasked.

(defun rules-heading (fact)
  "The heading FACT's text lands under: its world, or the path of a home read
at one as the operator writes it, and its file."
  (format nil "## ~a — ~a"
          (if (nlk:find-agent-world fact.world)
              fact.world
              (nlk:home-abbreviated (uiop:native-namestring fact.world)))
          fact.source))

(defun rules-heading-p (line)
  "Whether LINE is a RULES-HEADING: of a world this build knows, or of a home
read at a path, whose world is that path."
  (ppcre:register-groups-bind (world) ("^## (.+?) — \\S" line)
    (or (member world (nlk:agent-world-names) :test #'string=)
        (uiop:string-prefix-p "~" world)
        (ignore-errors (uiop:absolute-pathname-p world)))))

(defun rules-pieces (text &aux (pieces (list (list nil))))
  "TEXT, a rules file, as ((HEADING . TEXT) ...): what stands ahead of the
first RULES-HEADING under NIL, then each section from its heading on, blank
lines trimmed off both ends of each."
  (dolist (line (uiop:split-string text :separator '(#\Newline)))
    ;; A line's CR is no part of its heading: a file an editor saved with
    ;; CRLF endings still holds the sections an import wrote.
    (let ((heading (string-right-trim '(#\Return) line)))
      (when (rules-heading-p heading) (push (list heading) pieces)))
    (push line (rest (first pieces))))
  (loop for (heading . lines) in (nreverse pieces)
        for body = (string-trim '(#\Newline #\Return) (format nil "~{~a~^~%~}" (reverse lines)))
        unless (equal body "") collect (cons heading body)))

(defun rules-text (pieces)
  "PIECES as the file's text, a blank line between each two."
  (format nil "~{~a~%~^~%~}" (mapcar #'cdr pieces)))

(defun plan-instructions-file (plan name facts &key what reader limit (measure #'length) persona)
  "One item a fact of FACTS, landing its text under its own heading in this
home's file NAME, WHAT that file is to the operator: added after what the
file holds, or in place of the section it landed before under --overwrite,
and left out, said, where it would take the file past LIMIT, what READER
reads, sized by READER's own MEASURE of a text. A PERSONA past the operator's
own voice lands only under --overwrite, in its place."
  (let ((destination (merge-pathnames name (getf plan.settings :home))))
    (labels ((pieces-now () (rules-pieces (or (nlk:read-text destination) "")))
             (shown (path) (nlk:home-abbreviated (uiop:native-namestring path)))
             (read-from (fact)
               ;; The file the home read, where the operator opens it: its
               ;; source under the home's first root (home.lisp READ-HOMES).
               (let ((home (find fact.world plan.homes :key #'home-world-name :test #'equal)))
                 (shown (if home
                            (merge-pathnames fact.source (uiop:ensure-directory-pathname (first home.roots)))
                            fact.source))))
             (again (fact)
               ;; A home read at a path is named by that path again.
               (if (nlk:find-agent-world fact.world)
                   (format nil "nodecode import ~a" fact.world)
                   (format nil "nodecode import --source ~a" (shown fact.world))))
             (plain (text) (remove #\Return text)))
      (let ((pieces (pieces-now)))
        (dolist (fact facts)
          (let* ((heading (rules-heading fact))
                 (body (string-trim '(#\Newline #\Return) (getf fact.value :text)))
                 (section (cons heading (format nil "~a~%~%~a" heading body)))
                 (old (cdr (assoc heading pieces :test #'equal)))
                 ;; The operator's own voice, which this one would speak beside.
                 (mine (and persona (not old) (cdr (assoc nil pieces))))
                 (landed (lambda (pieces)
                           (nlk::put-in-place (if mine (remove nil pieces :key #'car) pieces)
                                              heading (cdr section) #'equal)))
                 (size (funcall measure (rules-text (funcall landed pieces)))))
            (multiple-value-bind (status reason)
                (cond
                  ((some (lambda (piece) (cl:search (plain body) (plain (cdr piece)))) pieces)
                   (values "skipped" (format nil "already in ~a" name)))
                  ((and old (not plan.overwrite))
                   (values "conflict" (format nil "~a already holds `~a', with other text (--overwrite replaces that section alone)"
                                              name heading)))
                  ((and mine (not plan.overwrite))
                   (values "conflict" (format nil "~a holds a voice of your own, and your chat bots speak in one (--overwrite puts this one in its place)"
                                              name)))
                  ;; xh-201: a file past the limit is refused whole by its
                  ;; reader, so what would take it there stays out, named.
                  ((> size limit)
                   (values "skipped"
                           (if (> (funcall measure body) limit)
                               (format nil "left out: ~a is ~a, past the ~a ~a reads — trim it and run `~a'"
                                       (read-from fact) (nlk:size-text (funcall measure body))
                                       (nlk:size-text limit) reader (again fact))
                               (format nil "left out: with it ~a would be ~a, past the ~a ~a reads — trim ~a or what is already there, and run `~a'"
                                       (shown destination) (nlk:size-text size) (nlk:size-text limit) reader
                                       (read-from fact) (again fact)))))
                  (t
                   (setf pieces (funcall landed pieces))
                   (values "imported"
                           (format nil "~a; ~a `~a'~@[ (it was ~a)~]; with it ~a is ~a of the ~a ~a reads"
                                   what (cond (old "replaced its own section")
                                              (mine "in place of your own, under")
                                              (t "added under"))
                                   heading (and (or old mine) (nlk:size-text (funcall measure (or old mine))))
                                   name (nlk:size-text size) (nlk:size-text limit) reader))))
              (add-item plan "instructions" status :fact fact :reason reason
                        :destination (namestring destination)
                        ;; The file as it stands when the action runs: the items
                        ;; ahead of this one have landed their sections by then.
                        :action (lambda ()
                                  (nlk:write-file-atomically
                                   destination (rules-text (funcall landed (pieces-now)))))))))))))

;;; --- memory and skills: definitions, through the scribe -------------------------------
;;; A remembered entry is a define-memory and a skill a prose define-skill,
;;; kept in this organism's knowledge cell the way an eval keeps one: the
;;; form evaluated, then filed by the scribe with its own bytes (one file and
;;; one commit each, under the session `<world> import'), and the use ledger
;;; seeded with the keep at the date the entry carries -- so an import that
;;; wrote every file today ranks each by its own history -- and with a view
;;; of each entry the home's hot index listed, so the first index lists that
;;; set.

(defun definition-text (operator name description lines &rest options
                        &aux (head (let ((*print-pretty* nil))
                                     (with-output-to-string (out)
                                       (format out "(~(~a~) ~(~a~)~%  ~s" operator name (nlk:one-line description))
                                       (loop for (key value) on options by #'cddr
                                             when value
                                               do (format out "~%  ~(~s~) ~a" key
                                                          (if (keywordp value)
                                                              (format nil "~(~s~)" value)
                                                              (prin1-to-string value))))))))
  "OPERATOR's definition of NAME as the text the scribe keeps: DESCRIPTION
on one line, the OPTIONS that carry a value, then each line of LINES as a ;;
line, its trailing blanks off -- a string of several split as NLK:LINES
splits one, an empty one an empty line -- the closing paren on a line of
its own. The second value is the form without its lines, which reads as
the same form."
  ;; Made at its length, a line copied once from where it stands: split into
  ;; strings and written through a stream.
  (flet ((each-line (function)
           ;; FUNCTION over each line's (STRING START END), END past its trailing blanks
           (dolist (string lines)
             (let ((pieces '()))
               (loop for start = 0 then (1+ newline)
                     for newline = (position #\Newline string :start start)
                     for end = (or newline (length string))
                     do (push (cons start (if (and newline (> end start)
                                                   (char= #\Return (char string (1- end))))
                                              (1- end)
                                              end))
                              pieces)
                     while newline)
               (unless (zerop (length string))
                 (loop while (and pieces (= (car (first pieces)) (cdr (first pieces)))) do (pop pieces)))
               (loop for (start . end) in (nreverse pieces)
                     do (funcall function string start
                                 (let ((last (position-if-not (lambda (char) (member char '(#\Space #\Tab #\Return)))
                                                              string :start start :end end :from-end t)))
                                   (if last (1+ last) start))))))))
    (let ((text (make-string (+ (length head) 4
                                (let ((size 0))
                                  (each-line (nlk:lambda-ignoring (string start end)
                                               (incf size (+ 5 (if (> end start) (1+ (- end start)) 0)))))
                                  size))))
          (at (length head)))
      (replace text head)
      (each-line (lambda (string start end)
                   (replace text #.(format nil "~%  ;;") :start1 at)
                   (incf at 5)
                   (when (> end start)
                     (setf (char text at) #\Space)
                     (replace text string :start1 (1+ at) :start2 start :end2 end)
                     (incf at (1+ (- end start))))))
      (replace text #.(format nil "~%  )") :start1 at)
      (values text (concatenate 'string head ")")))))

(defun definable-name-p (name)
  "Whether NAME reads as the symbol a definition takes: lower case, digits
and . _ -, never a number."
  (and (stringp name)
       (ppcre:scan "^[a-z0-9][a-z0-9._-]*$" name)
       (symbolp (let ((*read-eval* nil) (*package* (find-package '#:nodecode.evolved)))
                  (ignore-errors (read-from-string name))))))

(defun live-name-p (name &aux (symbol (find-symbol (string-upcase name) '#:nodecode.evolved)))
  "Whether NAME already names something this image runs: what a define-memory
of it would refuse."
  (and symbol (handler-case (progn (nle::check-knowledge-name symbol 'nle:define-memory) nil)
                (error () t))))

(defvar *kept* nil
  "The names this organism keeps, read once per plan (MAKE-PLAN binds it).")

(defun kept-p (name)
  "Whether this organism already keeps a definition named NAME."
  (gethash (string-downcase name)
           (or *kept*
               (setf *kept* (nlk:string-set
                             (mapcar #'nlk:knowledge-entry-name (nlk:knowledge-entries)))))))

(defun evaluate-definition (text &aux (*package* (find-package '#:nodecode.evolved))
                                      (form (let ((*read-eval* nil)) (read-from-string text))))
  "TEXT, one definition, read and evaluated in the evolved package: => the form."
  (with-output-to-string (*standard-output*) (eval form))
  form)

(defun form-text (parts &aux (head "(progn")
                               (text (make-string (+ (length head)
                                                     (loop for part in parts sum (1+ (length part)))))))
  "`(progn' and each of PARTS on a line of its own, made at its length: through
FORMAT, an import's 8 MB of definitions were copied as its buffer grew."
  (replace text head)
  (loop with at = (length head)
        for part in parts
        do (setf (char text at) #\Newline)
           (replace text part :start1 (1+ at))
           (incf at (1+ (length part))))
  text)

(defun keep-definitions (run)
  "RUN, (ITEM . FORM) each FORM evaluated already, kept by the scribe as one
form under the session `WORLDS import'; each item's ledger seeded with its keep
at :AT under its own world's session unless :SEED is NIL, and with a view now
when the home's index had it :HOT. An item the scribe did not keep is an error."
  ;; One form, so one commit: kept one by one, an import is one commit per
  ;; memory and a long `Importing…'; kept a world at a time, a box's five
  ;; homes are five commits.
  (let* ((*package* (find-package '#:nodecode.evolved))
         (text (form-text (mapcar (lambda (pair) (first (item-keep (car pair)))) run)))
         (kept (make-hash-table :test #'equal))
         (said (with-output-to-string (*standard-output*)
                 (dolist (key (nlk:record-definitions
                               (cons 'progn (mapcar #'cdr run)) text (length text)
                               :session-id (nlk:import-session
                                            (nlk:distinct (mapcar (lambda (pair) (item-world (car pair))) run)))
                               :ledger nil))
                   (setf (gethash key kept) t)))))
    (loop for (item . form) in run
          for session = (nlk:import-session (list item.world))
          do (destructuring-bind (&key at hot (seed t) &allow-other-keys) (rest item.keep)
               (handler-case
                   (cond ((not (gethash (nlk:definition-key form) kept))
                          (fail "~a not kept: ~a" item.destination (nlk:one-line said)))
                         (t (when seed
                              (nlk:note-knowledge-use item.destination "keep"
                                                      :session session :at (or at (nlk:iso-now))))
                            (when hot
                              (nlk:note-knowledge-use item.destination "view" :session session))))
                 (error (condition)
                   (setf item.status "error" item.reason (princ-to-string condition))))))))

(defun plan-keep (plan kind fact name text &rest keys &key reason &allow-other-keys)
  "One item keeping TEXT, the definition of NAME a FACT became: a conflict
when NAME is no name a definition takes or one this image already runs, a
skip when this organism keeps NAME already and the plan does not overwrite."
  (cond ((not (definable-name-p name))
         (add-item plan kind "conflict" :fact fact
                   :reason (format nil "~a is not a name a definition takes; rename it by hand" name)))
        ((live-name-p name)
         (add-item plan kind "conflict" :fact fact :destination name
                   :reason "names a live definition in this image; rename it by hand"))
        ((and (kept-p name) (not plan.overwrite))
         (add-item plan kind "skipped" :fact fact :destination name
                   :reason "already here (--overwrite replaces it)"))
        (t (add-item plan kind "imported" :fact fact :destination name :reason reason
                     :keep (cons text (alexandria:remove-from-plist keys :reason))))))

(defun plan-memory (plan &aux (hot (let ((table (make-hash-table :test #'equalp)))
                                     (dolist (fact (plan-facts plan :hot) table)
                                       (setf (gethash fact.id table) t))))
                              (seen (make-hash-table :test #'equal)))
  "Each remembered entry a home holds, as a define-memory: its name, or a
digest of it for an entry that names none; its description, type, project
and sources; the dates and the body as its prose. A second entry of one
name -- a user's and a project's memory both called so -- is a conflict the
report names, never a silent drop."
  ;; Each entry's text is made on eight lanes, its item added in turn: one
  ;; by one.
  (let ((rows (loop for fact in (plan-facts plan :memory)
                    for name = (string-downcase (or (getf fact.value :name) fact.id))
                    when (cond ((null (gethash name seen))
                                (setf (gethash name seen) fact.source)
                                (list fact name))
                               ;; an unnamed entry is named by its digest: the same text twice is one entry
                               ((getf fact.value :name)
                                (list fact name (gethash name seen))))
                      collect it)))
    (loop for (fact name first) in rows
          for entry in (nlk:parallel-map (lambda (row)
                                           (and (null (third row))
                                                (memory-entry (first row) (second row) hot)))
                                         rows)
          do (if first
                 (add-item plan "memory" "conflict" :fact fact :destination name
                           :reason (format nil "a memory named ~a came from ~a first; rename one by hand"
                                           name first))
                 (apply #'plan-keep plan "memory" fact name entry)))))

(defun memory-entry (fact name hot &aux (value fact.value))
  "FACT, a remembered entry, as the define-memory NAME: (TEXT . KEYS) for
PLAN-KEEP, HOT the names a home's hot index lists."
  (let* ((body (getf value :text))
         (description (or (getf value :description) (one-line-of body 100)))
         (type (let ((type (getf value :type)))
                 (and type (find type nle::+memory-types+ :test #'string-equal))))
         (lines (append (and (or (getf value :created) (getf value :updated))
                             (list (format nil "~@[created ~a~]~:[~; · ~]~@[updated ~a~]"
                                           (getf value :created)
                                           (and (getf value :created) (getf value :updated))
                                           (getf value :updated))
                                   ""))
                        ;; the description is the docstring; what follows it is the prose
                        (let ((prose (if (uiop:string-prefix-p description body)
                                         (subseq body (length description))
                                         body)))
                          (and (plusp (length prose)) (list prose))))))
    (multiple-value-bind (text form)
        (definition-text "define-memory" name description lines
                         :type type :project (getf value :project)
                         :sources (getf value :sources) :origin fact.world)
      (list text :form form
                 :reason (format nil "~@[~(~a~): ~]~a" type (nlk:clip description 80))
                 :at (or (getf value :updated) (getf value :created))
                 :hot (gethash name hot)))))

(defun skill-project (directory &aux (path (namestring directory))
                                     (at (cl:search "/.nodecode/skills/" path))
                                     (root (and at (subseq path 0 at))))
  "The repository a skill belongs to when it sits in that repository's
.nodecode/skills/, else NIL: the home's own library is the operator's."
  (and root
       (not (equal (uiop:ensure-directory-pathname root) (user-homedir-pathname)))
       (probe-file (format nil "~a/.git" root))
       root))

(defun plan-skills (plan &aux (seen (make-hash-table :test #'equal))
                              (used (mapcar (lambda (fact) (getf fact.value :name))
                                            (plan-facts plan :usage))))
  "Each skill a home holds, as a prose define-skill: SKILL.md's description
its docstring, its other frontmatter and its procedure the prose, a small
text file beside it carried along and anything else named."
  ;; Each skill's text is made on eight lanes, its item added in turn, as a
  ;; memory's is (PLAN-MEMORY).
  (let ((rows (loop for fact in (plan-facts plan :skill)
                    collect (list fact (shiftf (gethash fact.id seen) (or (gethash fact.id seen) fact.world))))))
    (loop for (fact first) in rows
          for entry in (nlk:parallel-map (lambda (row) (and (null (second row)) (skill-entry (first row) used)))
                                         rows)
          do (if first
                 (add-item plan "skills" "conflict" :fact fact
                           :reason (format nil "a skill named ~a came from ~a first" fact.id first))
                 (apply #'plan-keep plan "skills" fact fact.id entry)))))

(defun skill-entry (fact used &aux (name fact.id) (directory (getf fact.value :directory)))
  "FACT, a skill, as the prose define-skill of its name: (TEXT . KEYS) for
PLAN-KEEP, USED the names a home's ledger says were used."
  (multiple-value-bind (fields body) (frontmatter (or (getf fact.value :text) ""))
    (multiple-value-bind (support named) (values-list (getf fact.value :support))
      (let ((description (or (cdr (assoc "description" fields :test #'string-equal))
                             (let ((line (one-line-of body 100)))
                               (and (plusp (length line)) line))
                             name)))
        (multiple-value-bind (text form)
            (definition-text "define-skill" name description
                             (append (loop for (key . value) in fields
                                           unless (member key '("name" "description")
                                                          :test #'string-equal)
                                             collect (format nil "~a: ~a" key value))
                                     (let ((prose (nlk:trimmed body)))
                                       (and (plusp (length prose)) (list prose)))
                                     support)
                             :project (skill-project directory) :origin fact.world)
          (list text :form form
                     :reason (format nil "~a~@[ (not carried: ~{~a~^, ~})~]"
                                     (nlk:clip (nlk:one-line description) 80) named)
                     :at (ignore-errors (nlk:iso-time (- (file-write-date (merge-pathnames "SKILL.md" directory))
                                                         nlk:+unix-epoch+)))
                     :seed (not (member name used :test #'string-equal))))))))

(defun plan-usage (plan &aux (facts (plan-facts plan :usage)))
  "The use a home's ledgers record, into this organism's use ledger as it
happened -- views, keeps, sightings -- once: lines already brought from a
world are not brought again."
  (when facts
    (let ((worlds (nlk:distinct (mapcar #'fact-world facts))))
      (if (some (lambda (line) (member (nlk:json-value line :string "imported") worlds :test #'equal))
                (nlk:knowledge-ledger-lines))
          (add-item plan "usage" "skipped" :reason "already here" :destination "usage.jsonl")
          (add-item plan "usage" "imported" :destination "usage.jsonl"
                    :reason (format nil "~d line~:p of use: views, keeps and sightings" (length facts))
                    :action (lambda ()
                              (dolist (fact facts)
                                (let ((value fact.value))
                                  (nlk:note-knowledge-use (getf value :name) (getf value :kind)
                                                          :session (getf value :session)
                                                          :at (or (getf value :at) (nlk:iso-now))
                                                          :extra (list "imported" fact.world))))))))))

(defun plan-standing (plan &aux (names (nlk:distinct
                                       (loop for home in plan.homes
                                             for world = home.world
                                             when world append (nlk:agent-world-retire world)))))
  "The standing sections a world's retired carriers pinned on this organism's
own sessions, unpinned: a session that held one reads the index in its place."
  (when names
    (let ((held (and (nlk:store-open-p)
                     (loop for entry in (nlk:list-sessions)
                           for id = (getf entry :id)
                           append (loop for name in names
                                        when (nlk:get-harness-section id name)
                                          collect (cons id name))))))
      (if (null held)
          (add-item plan "standing" "skipped" :reason (format nil "no session holds ~{~a~^ or ~}" names))
          (add-item plan "standing" "imported"
                    :reason (format nil "~d pin~:p of ~{~a~^ and ~} on ~d session~:p, unpinned"
                                    (length held) names (length (nlk:distinct (mapcar #'car held))))
                    :action (lambda ()
                              (loop for (id . name) in held
                                    do (nlk:clear-harness-section id name))))))))

;;; --- MCP servers -----------------------------------------------------------------

(defun without-secrets (object &aux (entry (nlk:make-json-object))
                                    (left '()))
  "OBJECT, an MCP server's entry, less the members of its `env' and `headers'
that are secrets (SECRET-MEMBER-P); an emptied map goes too. => (values ENTRY
LEFT), LEFT their names as `env A', `headers B'."
  (maphash (lambda (key value)
             (if (and (member key '("env" "headers") :test #'string=) (hash-table-p value))
                 (let ((kept (nlk:make-json-object)))
                   (maphash (lambda (name text)
                              (if (secret-member-p name text)
                                  (push (format nil "~a ~a" key name) left)
                                  (setf (gethash name kept) text)))
                            value)
                   (when (plusp (hash-table-count kept)) (setf (gethash key entry) kept)))
                 (setf (gethash key entry) value)))
           object)
  (values entry (nreverse left)))

(defparameter +fetching-runners+
  '(("npx" . "npm") ("bunx" . "npm") ("pnpx" . "npm") ("uvx" . "PyPI") ("pipx" . "PyPI")
    ("docker" . "a container registry") ("podman" . "a container registry"))
  "The commands that fetch the server they run, and where from.")

(defun runner-name (command)
  "COMMAND's program name as a runner is known by: no directory, no `.cmd',
`.exe' or `.bat', lower case."
  (string-downcase (ppcre:regex-replace "(?i)\\.(cmd|exe|bat)\\z"
                                        (car (last (ppcre:split "[/\\\\]" command))) "")))

(defun masked-word (word &aux (equals (position #\= word)))
  "WORD with what could be a secret in it shown as `[set]': the value of a
NAME=VALUE pair that is one (SECRET-MEMBER-P), a URL's user and password."
  (ppcre:regex-replace "(?<=://)[^/@\\s]+(?=@)"
                       (if (and equals (secret-member-p (subseq word 0 equals) (subseq word (1+ equals))))
                           (format nil "~a=[set]" (subseq word 0 equals))
                           word)
                       "[set]"))

(defun server-line (object &aux (command (gethash "command" object)))
  "What the MCP server entry OBJECT would run, as the report says it: its
command -- through a leading `cmd /c' -- and, for a runner that fetches its
package, the words up to that package and where it is fetched from, then
how many arguments follow, never their text; else its url (MASKED-WORD)."
  ;; Without this, every argument would be printed, and a server's secret rides
  ;; its arguments as often as its env -- `--header Authorization: Bearer ...',
  ;; `-e GITHUB_TOKEN=...', a database URL's password -- to the screen and to
  ;; the report file. The words up to the package are masked still, and the
  ;; word after a flag named like a secret is `[set]'.
  (if (null command)
      (masked-word (gethash "url" object))
      (let* ((words (cons command (coerce (or (gethash "args" object) #()) 'list)))
             (start (if (and (equal (runner-name command) "cmd") (equalp (second words) "/c")) 2 0))
             (source (and (nth start words)
                          (cdr (assoc (runner-name (nth start words)) +fetching-runners+ :test #'string=))))
             (package (and source (position-if-not (lambda (word) (uiop:string-prefix-p "-" word))
                                                   words :start (1+ start))))
             (shown (min (length words) (if package (1+ package) (1+ start)))))
        (format nil "~{~a~^ ~}~@[ ~a~]~@[ (fetched from ~a)~]"
                (loop with hide = nil
                      for word in (subseq words 0 shown)
                      collect (if (shiftf hide nil) "[set]" (masked-word word))
                      do (setf hide (and (uiop:string-prefix-p "-" word) (not (find #\= word))
                                         (some (lambda (needle) (cl:search needle (string-downcase word)))
                                               +secret-words+))))
                (and (< shown (length words))
                     (format nil "~:[with ~d~;and ~d more~] argument~:p" (> shown 1) (- (length words) shown)))
                source))))

(defun plan-mcp (plan &aux (seen (make-hash-table :test #'equal)))
  ;; The mcp folder takes env and headers only from config.jsonc, as written --
  ;; no reference to a file, no ${VAR} -- so a secret in either has no 0600
  ;; place to go. It stays in the home it came from, and is said.
  ;;
  ;; An imported server lands off. Without this, a launch would start every one
  ;; before any task, and npx and uvx would fetch and run their packages
  ;; unasked. The report says what each would run and from where, and the
  ;; operator starts the ones they want.
  (dolist (fact (plan-facts plan :mcp))
    (let* ((name fact.id)
           (object (getf fact.value :object))
           (command (gethash "command" object))
           (url (gethash "url" object)))
      (unless (shiftf (gethash (format nil "~a ~a" name (or command url)) seen) t)
        (multiple-value-bind (status reason)
            (cond
              ((and (config-present-p "mcp" "servers" name) (not (plan-overwrite plan)))
               (values "conflict" "already here (--overwrite replaces its members)"))
              ((not (or command url))
               (values "skipped" "names neither a command nor an http(s) url, so there is nothing to start"))
              ;; The command resolves on $PATH, by stats: a server that cannot start is worse than none.
              ((and command
                    (not (and (stringp command)
                              (plusp (length command))
                              (if (find #\/ command)
                                  (probe-file command)
                                  (nlk::executable-on-path command)))))
               (values "skipped" (format nil "~a is not on this box's PATH" command)))
              (t (values "imported" (format nil "~a; lands off: /mcp on ~a starts it~{; ~a~}"
                                            (server-line object) name (getf fact.value :notes)))))
          (add-item plan "mcp" status :fact fact :reason reason
                    :destination (format nil "mcp.servers.~a" name)
                    :action (lambda ()
                              (need-cell "nodecode-mcp")
                              (let ((entry (without-secrets object)))
                                (setf (gethash "enabled" entry) :false)
                                (nle:config-set (list "mcp" "servers" name) entry))))
          (nlk:when-let (left (and (equal status "imported") (nth-value 1 (without-secrets object))))
            (add-item plan "mcp" "skipped" :fact fact :destination (format nil "mcp.servers.~a" name)
                      :reason (format nil "left out, being secrets: ~{~a~^, ~}. They stay in that home. The mcp folder takes env and headers only from config.jsonc, so add them under mcp.servers.~a in ~a yourself, and chmod 600 that file"
                                      left name (nlk:home-abbreviated
                                                 (uiop:native-namestring nle::*shared-config-path*))))))))))

;;; --- channels ---------------------------------------------------------------------

(defun plan-channel (plan fact)
  "One channels.<PLATFORM> section from what a home carries: the token in a
0600 file under this home's secrets/, the allowlists, the home chat, and the
adapter restarted so it reads the section as written."
  (destructuring-bind (&key platform token token-file users chats left disabled token-env token-field chats-key
                       &allow-other-keys)
      fact.value
    (let* ((destination (format nil "channels.~a" platform))
           ;; A bot switched off in its home is not polled there: it lands off, and nothing holds it.
           (running (and plan.running (not disabled) t))
           (unit (plan-takeover-unit plan))
           (takeover (and running plan.takeover unit t))
           (held (and running (not takeover))))
      (nlk:if-let (refusal
                   (cond ((and (config-present-p "channels" platform) (not (plan-overwrite plan)))
                          '("conflict" "section already here (--overwrite replaces its members)"))
                         ((null token)
                          (list "skipped"
                                (if token-file
                                    (format nil "its token file ~a cannot be read" token-file)
                                    (format nil "an allowlist but no ~a in the home" token-env))))
                         ((and (null users) (null chats))
                          (list "error" (format nil "no allowlist in the home: a channel never starts open to the world~{; ~a~}"
                                                left)))))
        (add-item plan "channels" (first refusal) :fact fact :reason (second refusal)
                  :destination destination)
        (let* ((file (merge-pathnames (format nil "channels.~a.~a" platform token-field)
                                      nle::*secrets-directory*))
               (section (nlk:make-json-object token-field (namestring file)))
               (base (format nil "~d user~:p, ~d chat~:p, token in a 0600 file"
                             (length users) (length chats))))
          (loop for (key ids) in `(("allowed_users" ,users) ("owner" ,users) (,chats-key ,chats))
                when ids do (setf (gethash key section) (coerce ids 'vector)))
          (loop for (key field) in '((:require-mention "require_mention") (:reactions "reactions"))
                for flag = (getf fact.value key)
                unless (null flag)
                  do (setf (gethash field section) (if (eq flag :false) :false t)))
          (let ((item (add-item plan "channels" "imported" :fact fact
                                :reason (format nil "~a~a~{; ~a~}" base
                                                (cond (disabled (format nil "; off in ~a, so it lands off" fact.world))
                                                      (held "; off while the other harness runs this bot — it comes on by itself once that gateway stops")
                                                      (takeover (format nil "; live once ~a is stopped" unit))
                                                      (t ""))
                                                left)
                                :destination destination
                                :detail (append (and running (list :bot t))
                                                (and held (list :held t))))))
            (setf item.action
                  (lambda (&aux (off (or disabled held (and takeover (not plan.stopped)))))
                    (when (and off (not held) (not disabled))
                      (setf item.reason
                            (format nil "~a; off: a foreign gateway still runs, so it comes on by itself once that gateway stops~{; ~a~}"
                                    base left)))
                    (setf (gethash "enabled" section) (if off :false t))
                    (nle::write-secret-file file token)
                    (nle:config-set (list "channels" platform) section)
                    (set-held plan.settings platform (and off (not disabled)))
                    (need-cell (format nil "nodecode-channel-~a" platform) :fresh t)))))))))

(defparameter +bot-merge+
  '(:fill (:token :token-file :disabled :require-mention :reactions) :union (:users :chats :left) :from-end nil)
  "How the facts of one bot are one: its token and its manners live apart, and
its allowlist is every list that names it.")

(defun bot-groups (plan &aux (groups '()))
  "The plan's :CHANNEL facts as bots, newest first, each a list of one bot's
facts. One home's are one bot's -- its token in an environment file, its manners
in a config -- and homes that hold the same token are one bot; a home with
another token is another, and a home with no token is its own."
  (dolist (home (plan-homes plan))
    (dolist (fact (apply #'merged-facts plan :channel
                         :facts (remove :channel home.facts :key #'fact-kind :test-not #'eq)
                         +bot-merge+))
      (nlk:if-let (group (and (getf fact.value :token)
                              (find-if (lambda (group &aux (other (first group)))
                                         (and (equal fact.id (fact-id other))
                                              (equal (getf fact.value :token)
                                                     (getf (fact-value other) :token))))
                                       groups)))
        (nconc group (list fact))
        (push (list fact) groups))))
  (nreverse groups))

(defun plan-channels (plan &aux (mark plan.items) (kept '()))
  ;; One section per bot. Two bots of one platform are two people's and the
  ;; section is one: the newest home's is kept and the other is named and left
  ;; out, never joined -- an allowlist and its owners are that bot's. The
  ;; same token in two homes is one bot, and both homes' lists are its.
  (dolist (group (bot-groups plan))
    (let* ((fact (apply #'merge-facts group +bot-merge+))
           (platform (getf fact.value :platform))
           (rival (and (getf fact.value :token) (cdr (assoc platform kept :test #'equal))))
           (worlds (nlk:distinct (mapcar #'fact-world group))))
      (cond ((and rival (not (and (config-present-p "channels" platform) (not (plan-overwrite plan)))))
             (add-item plan "channels" "conflict" :fact fact :destination (format nil "channels.~a" platform)
                       :reason (format nil "~{~a~^ and ~} and ~{~a~^ and ~} both have a ~:(~a~) bot; kept ~{~a~^ and ~}, left ~{~a~^ and ~}'s out"
                                       rival worlds platform rival worlds)))
            (t (when (getf fact.value :token) (push (cons platform worlds) kept))
               (plan-channel plan fact)))))
  ;; Items are newest first until MAKE-PLAN reverses them: the takeover goes
  ;; in behind the sections it frees, so it runs ahead of them — and only
  ;; when one of them is a bot to free. Two long-pollers on one bot token fight.
  (let ((new (ldiff plan.items mark))
        (unit (plan-takeover-unit plan)))
    (when (and (plan-takeover-units plan) plan.takeover plan.running
               (some (lambda (item) (getf item.detail :bot)) new))
      (setf plan.items
            (append
             new
             (list (if unit
                       (let ((item (make-item :kind "channels" :status "imported" :source unit
                                              :reason (format nil "stop and disable it, so the bots answer from here; `systemctl --user enable --now ~a' gives them back"
                                                              unit))))
                         (setf item.action
                               (lambda (&aux (done (funcall *gateway-stop* unit))
                                             (left (funcall *running-probe* (plan-takeover-units plan))))
                                 (setf plan.stopped (null left)
                                       item.reason
                                       (format nil "~a; `systemctl --user enable --now ~a' gives the bots back~@[; still running: ~a, so the bots come on by themselves once it stops~]"
                                               done unit (first left)))))
                         item)
                       (make-item :kind "channels" :status "skipped" :source (first plan.running)
                                  :reason "a foreign gateway outside its systemd unit is not stopped from here; its bots land off and come on by themselves once it stops")))
             mark)))))

;;; --- cron -------------------------------------------------------------------------

(defun plan-cron (plan)
  (flet ((cron (verb &rest arguments) (apply #'uiop:symbol-call "NODECODE-CRON" verb arguments)))
    (dolist (fact (remove-duplicates (plan-facts plan :cron)
                                     :key (lambda (fact) (getf fact.value :name))
                                     :test #'equal :from-end t))
      (destructuring-bind (&key name schedule prompt model enabled &allow-other-keys) fact.value
        (cond ((null prompt)
               (add-item plan "cron" "skipped" :fact fact
                         :reason "a script- or skill-only job; recreate it around a prompt"))
              ((nlk:when-let (package (find-package "NODECODE-CRON"))
                 (some (lambda (job) (equal name (cron "JOB-NAME" job)))
                       (symbol-value (find-symbol "*JOBS*" package))))
               (add-item plan "cron" "skipped" :reason "already here" :fact fact))
              (t (add-item plan "cron" "imported" :fact fact
                           :reason (format nil "~a~:[, paused~;~]" schedule enabled)
                           :destination name
                           :action (lambda ()
                                     (need-cell "nodecode-cron")
                                     (let ((added (cron "ADD-JOB" schedule prompt
                                                         :name name :model model)))
                                       (unless enabled
                                         (cron "PAUSE" (cron "JOB-ID" added))))))))))))

;;; --- profiles ---------------------------------------------------------------------
;;; A foreign profile is a whole home of its own; a Nodecode profile is the
;;; same idea (NLK:PROFILE-CREATE). This plan runs in ONE home — every seam
;;; it writes through reads this process's — so a profile's own pieces
;;; cannot land here: the profile is created and the item says the one
;;; command that imports the rest under that home's own launch.

(defun plan-profiles (plan)
  (dolist (home plan.homes)
    (let ((found '()))
      (dolist (root home.roots)
        (let ((profiles (and (uiop:directory-pathname-p root)
                             (probe-file (merge-pathnames "profiles/" root)))))
          (dolist (sub (and profiles (ignore-errors (uiop:subdirectories profiles))))
            (unless (hidden-name-p (nlk:folder-name sub))
              (push (cons (nlk:folder-name sub) sub) found)))))
      (dolist (pair (sort (nreverse found) #'string< :key #'car))
        (destructuring-bind (name . directory) pair
          (let ((command (format nil "nodecode -p ~a import ~a --source ~a"
                                 name (home-world-name home)
                                 (string-right-trim "/" (namestring directory)))))
            (flet ((add (status reason &rest more)
                     (apply #'add-item plan "profiles" status :world (home-world-name home)
                            :reason reason :source (format nil "profiles/~a" name) more)))
              (cond ((not (nlk:profile-name-p name))
                     (add "error" (format nil "~s is not a profile name here (a-z, 0-9, _ and -)" name)))
                    ((member name nlk:+reserved-profile-names+ :test #'string=)
                     (add "skipped" "a reserved name here"))
                    ((nlk:find-profile name)
                     (add "skipped" (format nil "already here; its own import is `~a'" command)
                          :destination (namestring (nlk:profile-home name))))
                    (t (add "imported"
                            (format nil "an empty home; then `~a' brings its config, memory, skills and sessions"
                                    command)
                            :destination (namestring (nlk:profile-home name))
                            :action (lambda () (nlk:profile-create name :clone :blank))))))))))))

;;; --- sessions ---------------------------------------------------------------------

(defun plan-sessions (plan)
  (dolist (fact (plan-facts plan :sessions))
    (let ((id fact.id)
          (record (getf fact.value :record)))
      (if (and (nlk:store-open-p) (nlk:session-exists-p id))
          (add-item plan "sessions" "skipped" :reason "already here" :fact fact :destination id)
          (add-item plan "sessions" "imported" :fact fact :destination id
                    :reason (format nil "~d exchange~:p~@[, ~d prompt~:p without an answer dropped~]~@[: ~a~]"
                                    (length record.exchanges)
                                    (and (plusp record.dropped) record.dropped)
                                    record.title)
                    :action (lambda (&aux (started (or record.started-at
                                                       (getf (first record.exchanges) :at))))
                              (nlk:create-session :id id :cwd record.cwd :occurred-at started)
                              (when record.title
                                (nlk:record-event id nlk::+kind-session-title+
                                                  (list :title record.title)
                                                  :occurred-at started))
                              (dolist (exchange record.exchanges)
                                (nlk:record-exchange-turn
                                 id (getf exchange :input) (getf exchange :answer)
                                 :occurred-at (getf exchange :at)
                                 :facts (append (list :recorder record.world)
                                                (and (stringp record.model)
                                                     (list :model record.model))
                                                (let ((elapsed (getf exchange :elapsed-ms)))
                                                  (and (integerp elapsed)
                                                       (list :elapsed-ms elapsed))))))))))))

;;; --- the plan ------------------------------------------------------------------------

(defun make-plan (&key source overwrite (takeover t) only without
                        (settings (import-settings)))
  "The plan for SOURCE — a world name, a path, a repository (its skills and
MCP servers alone, +SOURCE-KINDS+), a list of world names, or every world on
the box — over every kind in +KINDS+, or ONLY the named ones,
leaving out any WITHOUT names; OVERWRITE replaces what is already here;
TAKEOVER, the default, stops a foreign gateway so its bots answer from here."
  ;; Nothing is written; the actions wait on APPLY-PLAN. A list is the setup
  ;; walk's unasked offer: the worlds on the box less those the operator declined.
  (let* ((repository (and (stringp source) (not (nlk:find-agent-world source)) (parse-source source)))
         (homes (if (or (null source) (member source '("" "all") :test #'equalp))
                    (mapcar (lambda (row) (open-home row.world))
                            (or (nlk:detect-agent-worlds)
                                (fail
                                 "no coding-agent home found on this box (looked for ~{~a~^, ~})"
                                 (nlk:agent-world-names))))
                    (mapcar #'open-home (if (consp source) source (list source)))))
         (plan (%make-plan :homes homes :settings settings
                           :overwrite overwrite :takeover takeover
                           :only (if repository (source-kinds only) (kind-list only))
                           :without (kind-list without))))
    (read-homes homes :sessions (kind-wanted-p plan "sessions")
                      :text (some (lambda (kind) (kind-wanted-p plan kind))
                                  '("instructions" "memory" "skills" "usage"))
                      :tally (kind-wanted-p plan "providers"))
    ;; Only a plan that brings bots over asks after a foreign gateway: the ask
    ;; is a systemctl run.
    (when (kind-wanted-p plan "channels")
      (setf plan.running
            (ignore-errors (funcall *running-probe* (plan-takeover-units plan)))))
    (let ((*kept* nil))
      (dolist (kind +kinds+)
        (when (kind-wanted-p plan kind)
          (nlk:with-handlers ((error (condition)
                                (add-item plan kind "error" :reason (princ-to-string condition))))
            (funcall (intern (format nil "PLAN-~:@(~a~)" kind) :nodecode.import) plan)))))
    (setf plan.items (nreverse plan.items))
    plan))

(defun apply-plan (plan &key progress)
  "Run every imported item's action, in order; a failure is that item's
error and the next item still runs. A definition is evaluated in that order
and kept with its run (KEEP-DEFINITIONS) before the next action runs. The
report lands beside the markers."
  (let ((run '()))
    (labels ((notify (phase item) (when progress (funcall progress phase item)))
             (keep-run ()
               (when run
                 (let ((kept (nreverse run)))
                   (keep-definitions kept)
                   (dolist (pair kept) (notify "finished" (car pair))))
                 (setf run '()))))
      (dolist (item plan.items)
        (when (equal item.status "imported")
          (unless item.keep (keep-run))
          (notify "importing" item)
          (handler-case (if item.keep
                            ;; read from its :FORM, the definition without its
                            ;; prose (DEFINITION-TEXT), when the plan made one: a
                            ;; memory's prose is ;; lines, each character of which
                            ;; the reader walks to skip -- read whole would slow
                            ;; its apply without this.
                            (push (cons item (evaluate-definition (getf (rest item.keep) :form
                                                                        (first item.keep))))
                                  run)
                            (when item.action (funcall item.action)))
            (error (condition) (setf item.status "error" item.reason (princ-to-string condition))))
          ;; Definitions finish only after the batch is persisted above.
          (unless (find item run :key #'car)
            (notify "finished" item))))
      (keep-run)))
  (setf plan.applied t)
  (write-report plan)
  plan)

;;; --- the report -----------------------------------------------------------------------

(defun summary-counts (plan)
  "((STATUS . COUNT) ...) over imported, skipped, conflict, error."
  (mapcar (lambda (status)
            (cons status (count status plan.items :key #'item-status :test #'string=)))
          '("imported" "skipped" "conflict" "error")))

(defun summary-line (plan)
  (destructuring-bind (imported skipped conflicts errors) (mapcar #'cdr (summary-counts plan))
    (format nil "~d ~a · ~d skipped · ~d conflict~:p · ~d error~:p"
            imported (if plan.applied "imported" "to import") skipped conflicts errors)))

(defun plan-source-line (plan)
  "The homes the report names: the ones that actually held something."
  ;; A world detected and empty is noise on the one line an operator reads.
  (nlk:if-let (labels (mapcar #'home-name (or (remove-if-not #'home-facts (plan-homes plan)) plan.homes)))
    (format nil "~{~a~#[~; and ~:;, ~]~}" labels) "nothing"))

(defun report-text (plan)
  "The report, one line per item under the summary."
  (with-output-to-string (out)
    (format out "import: ~a~a~%" (plan-source-line plan)
            (if plan.applied "" " (dry run — nothing written)"))
    (when plan.running
      (format out "another gateway: running — ~{~a~^; ~}~%" plan.running))
    (dolist (item plan.items)
      (format out "  [~a] ~a~@[ ~a~]~@[ → ~a~]~@[ — ~a~]~%"
              item.kind
              (if (and (equal item.status "imported") (not plan.applied))
                  "would import" item.status)
              item.source
              ;; a file lands under the home, written as the operator writes it
              (and (plusp (length item.destination)) (nlk:home-abbreviated item.destination))
              item.reason))
    (dolist (home plan.homes)
      (dolist (note home.notes)
        (format out "  [note] ~a: ~a~%" (home-world-name home) note)))
    (format out "summary: ~a~%" (summary-line plan))
    (when plan.report-path
      (format out "report: ~a~%" (nlk:home-abbreviated (namestring plan.report-path))))))

(defun write-report (plan)
  "The report as a file under the organism's home, named by the clock."
  (let ((path (merge-pathnames
               (format nil "report-~a.txt" (substitute #\- #\: (nlk::iso-now)))
               (merge-pathnames "import/" (getf plan.settings :home)))))
    (nlk:with-handlers ((error () (setf (plan-report-path plan) nil)))
      (setf plan.report-path path)
      (nlk:write-file-atomically path (report-text plan)))
    plan))

(defun item-json (item)
  (nlk:json-object "kind" item.kind
                   "status" item.status
                   "reason" (or item.reason "")
                   "source" (or item.source "")
                   "destination" (or item.destination "")
                   "world" (or item.world "")))

(defun plan-json (plan)
  "The plan as the entrypoint answers it: the summary counts, the items,
the worlds read, what says a foreign gateway runs, the BOTS it also polls,
the unit a TAKEOVER stops, the unmapped files, and the report."
  (nlk:json-object
   "summary" (alexandria:alist-hash-table (summary-counts plan) :test #'equal)
   "applied" (if plan.applied t :false)
   "worlds" (coerce (mapcar #'home-world-name plan.homes) 'vector)
   "items" (coerce (mapcar #'item-json plan.items) 'vector)
   "running" (coerce plan.running 'vector)
   "bots" (map 'vector #'item-destination
               (remove-if-not (lambda (item) (getf item.detail :bot)) plan.items))
   "takeover" (or (plan-takeover-unit plan) :null)
   "unmapped" (coerce (loop for home in (plan-homes plan) append home.unmapped) 'vector)
   ;; The worlds whose sign-in is an OAuth grant this organism cannot
   ;; reuse: what the provider stage says when it is the one thing left.
   "grants" (coerce (nlk:distinct
                     (loop for home in (plan-homes plan)
                           when (find :oauth home.facts :key #'fact-kind)
                             collect home.name))
                    'vector)
   ;; Conversations the plan was told to leave where they are, when a home
   ;; carries any: the one line that tells the operator they are not lost.
   "sessions_left" (if (and (not (kind-wanted-p plan "sessions"))
                            (some (lambda (home) (session-stores home :first t)) plan.homes))
                       t :false)
   "report" (report-text plan)
   "report_path" (if plan.report-path (namestring plan.report-path) :null)
   "status_text" (summary-line plan)))
