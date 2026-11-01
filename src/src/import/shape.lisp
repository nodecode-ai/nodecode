;;;; shape.lisp --- the artifact shapes every agent home is made of.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The readers here are per SHAPE, not per world. A reader per world
;;;; scales with the number of worlds and needs somebody who has each one
;;;; installed; but the ecosystem already standardized on a small set of
;;;; artifacts, and every home is some arrangement of them:
;;;;
;;;;   credential entry   a key beside a provider or an endpoint, or a
;;;;                      NAME_API_KEY in an environment file
;;;;   OAuth grant        a refresh or id token, or `mode: oauth' — named,
;;;;                      never carried: it is the other harness's client's
;;;;   MCP server map     command/args/env or url/headers under any wrapper
;;;;   model pick         a model id, with or without its provider
;;;;   cron job           a schedule beside a prompt
;;;;   channel bot        a platform token beside an allowlist
;;;;
;;;; plus the three that are files rather than tree members — SKILL.md,
;;;; instruction markdown, memory folders (home.lisp) — and conversations
;;;; (sessions.lisp).
;;;;
;;;; One classifier walks a decoded tree, whatever format it came from:
;;;; JSON, YAML (yaml.lisp) and TOML (toml.lisp) all decode to the same
;;;; hash-table shape. The walk is RECURSIVE with a trail, because the
;;;; interesting members are rarely at the top: a first prototype that
;;;; classified top-level members only missed a whole home's auth profiles,
;;;; which sit at agents.<name>.auth.profiles.<provider>:<label>.
;;;;
;;;; Strictness is the other lesson. A loose MCP rule — "an object with a
;;;; command" — matched a test matrix and an agent's own config on a real
;;;; box. A map is an MCP map only when every member is a server by
;;;; structure: nothing but the keys an MCP server has, and either a
;;;; command or an http(s) url. The remaining guard is at apply, where the
;;;; command has to resolve on PATH.

(in-package #:nodecode.import)

;;; --- what a reader yields ---------------------------------------------------

(nlk:define-record (fact (:copier nil) (:export kind id value))
  "One thing a home holds, as the shape readers found it."
  ;; KIND is the shape (:provider :oauth :model :mcp :skill :instructions
  ;; :memory :cron :channel :sessions); ID names it within its kind; SOURCE is
  ;; where it was read from, relative to the world's root, and goes in the
  ;; report; WORLD the world's name; VALUE the plist the plan works from,
  ;; which is the only place a secret ever sits.
  kind id source world (value '()))

;;; --- member lookup, however the home spells it ------------------------------
;;; api_key, apiKey, API-KEY and APIKey are one member under four spellings,
;;; and a reader that matches them one at a time grows a case list per
;;; world. Normalizing the key once — lowercase, separators dropped — is
;;; the whole of it.

(defun normalize-key (key)
  (and (stringp key)
       (string-downcase (remove-if-not #'alphanumericp key))))

(defun object-index (object &aux (index (make-hash-table
                                         :test #'equal
                                         :size (if (hash-table-p object) (max 1 (hash-table-count object)) 1))))
  "OBJECT's members under their normalized keys => a hash table of
NORMALIZED -> (ORIGINAL-KEY . VALUE). The first spelling wins."
  (when (hash-table-p object)
    (maphash (lambda (key value &aux (normal (normalize-key key)))
               (when normal (alexandria:ensure-gethash normal index (cons key value))))
             object))
  index)

(defvar *normal-names* nil
  "Each member name a reader asked for, normalized (NORMAL-NAME), while a
home is read (READ-HOMES binds it); NIL normalizes every ask.")

(defun normal-name (name)
  "NORMALIZE-KEY of NAME, a name a reader asks for, kept from the first ask
while a home is read."
  ;; Asked of every object a config file holds, up to 4,000 a file: made each
  ;; time, the readers' own names were most of a keys-only plan's work.
  (if *normal-names*
      (alexandria:ensure-gethash name *normal-names* (normalize-key name))
      (normalize-key name)))

(defun member-of (index &rest names)
  "The first of NAMES present in INDEX, as (values VALUE ORIGINAL-KEY)."
  (declare (dynamic-extent names))
  (dolist (name names (values nil nil))
    (nlk:when-let (pair (gethash (normal-name name) index))
      (return (values (cdr pair) (car pair))))))

(defun trim-text (value)
  (and (stringp value) (let ((text (string-trim " " value))) (and (plusp (length text)) text))))

(defun text-of (index &rest names)
  "The first of NAMES whose value is a non-empty string."
  (declare (dynamic-extent names))
  (dolist (name names nil)
    (nlk:when-let (text (trim-text (cdr (gethash (normal-name name) index))))
      (return text))))

(defun texts-of (items &rest names)
  "Each of the vector ITEMS that is a string, or an object holding one of NAMES (TEXT-OF): those texts."
  (loop for item across items
        for text = (if (stringp item) item (and (hash-table-p item) (apply #'text-of (object-index item) names)))
        when text collect text))

(defun boolean-of (index name default)
  "NAME as a boolean: absent or null is DEFAULT, :FALSE and NIL are false."
  (multiple-value-bind (value key) (member-of index name)
    (if (or (null key) (eq value :null)) default (not (member value '(nil :false))))))

(defun url-p (text)
  (and (stringp text) (ppcre:scan "\\Ahttps?://" text)))

(defun string-members (value)
  "VALUE as a list of strings: a vector of them, one string, or none."
  (loop for item across (if (typep value '(and vector (not string))) value (vector value))
        when (stringp item) collect item
        when (realp item) collect (princ-to-string item)))

;;; --- shape: the OAuth grant -------------------------------------------------

(defun secret-looking-p (text)
  "Whether TEXT is shaped like a credential: long enough to be one, one run
of non-blank characters, and not a URL or a path."
  ;; The structural guard that keeps a settings file's `key: seven_day' and a
  ;; CI workflow's `id-token: write' out of the credential and grant shapes.
  (and (stringp text)
       (>= (length text) 16)
       (not (url-p text))
       (not (ppcre:scan "[ \\t\\n\\r]|\\A~?/" text))))

(defparameter +secret-words+
  '("token" "key" "secret" "password" "passwd" "authorization" "bearer" "cookie" "credential")
  "Words a member's name carries when its value is a secret.")

(defun secret-member-p (name value)
  "Whether the member NAME holding VALUE is a secret: named like one, or
valued like a credential (SECRET-LOOKING-P, with a letter and a digit in it,
so `streamable-http' is not one)."
  (and (trim-text value)
       (or (some (lambda (word) (cl:search word (string-downcase name))) +secret-words+)
           (and (secret-looking-p value)
                (find-if #'alpha-char-p value)
                (find-if #'digit-char-p value)))))

;;; --- shape: the credential entry --------------------------------------------

(defparameter +key-names+
  '("key" "apiKey" "token" "authToken" "secretKey" "accessKey")
  "Members that hold a provider key outright.")

(defparameter +key-env-names+
  '("envKey" "keyEnv" "apiKeyEnv" "apiKeyEnvVar")
  "Members that name the environment variable the key is read from.")

(defparameter +container-keys+
  '("providers" "provider" "apikeys" "apikey" "keys" "auth" "credentials" "credentialpool"
    "custom" "customproviders" "profiles" "accounts" "models" "config" "settings"
    "modelproviders" "llm" "llms" "agents" "agent" "env" "environment" "default"
    "options" "rows" "data" "items" "entries" "cache" "cached" "utilization")
  "Normalized keys that name a container, not the thing inside it: a trail
ending in one of these says nothing about which provider a member is for.")

(defun entry-provider-id (index trail)
  "Which provider a credential entry is for: what it says itself, then the
nearest key on its trail, then the name it carries, then the endpoint it
names — a home that gives only a base URL still lands under the id this
organism's catalog knows."
  ;; The trail outranks the entry's own `id' because that id is often the
  ;; credential's, not the provider's.
  (or (provider-id-text (or (text-of index "provider" "providerId") ""))
      ;; The nearest trail key that can be a provider's name, a `:label' part dropped.
      (loop for key in (reverse trail)
            for normal = (normalize-key key)
            ;; (No letter or digit in a key, or digits alone, is no provider's name: EVERY of
            ;; nothing is true.)
            thereis (and (not (member normal +container-keys+ :test #'string=))
                         (not (every #'digit-char-p normal))
                         (provider-id-text
                          (subseq key 0 (position-if (lambda (ch) (find ch ":/ ")) key)))))
      (provider-id-text (or (text-of index "name" "id") ""))
      (gethash (catalog-normalize-base (text-of-base index)) (second (vocabulary)))))

(defun text-of-base (index)
  (let ((base (text-of index "baseUrl" "api" "apiBase" "endpoint" "url" "host")))
    (and (url-p base) base)))

(defparameter +credential-containers+
  '("providers" "provider" "auth" "authprofiles" "credentials" "credentialpool"
    "apikeys" "keys" "secrets" "accounts" "customproviders" "modelproviders"
    "endpoints" "profiles" "llm" "llms" "services" "models" "modelsstore")
  "Normalized keys that say the subtree under them is about reaching a
provider. A key member somewhere else in a settings file is a coincidence.")

(defun endpoint-host (url)
  "URL's host and port, lowercased, as (values HOST PORT); NIL for a URL with none."
  (ppcre:register-groups-bind (host port) ("(?i)\\Ahttps?://(?:[^/@]*@)?([^/:?#]+)(?::(\\d+))?" url)
    (values (string-downcase host) port)))

(defun builtin-endpoint (id)
  "The endpoint the provider ID dials with no config entry: the catalog's api
base, else its lane's default. NIL for an id this organism does not serve by itself."
  (or (catalog-base id)
      (nlk:when-let (lane (nle::find-lane-by-name (or (catalog-sdk id) id) nil))
        (nle::provider-lane-default-endpoint lane))))

(defun endpoint-id (base world)
  "A provider id of its own for the endpoint BASE, one a built-in never has:
the catalog's row when BASE is that provider's api, else the host's words
(`dashscope.aliyuncs.com' is `dashscope-aliyuncs', `127.0.0.1:11434' is
`127-0-0-1-11434'), with WORLD after them when a built-in already goes by that."
  (or (gethash (catalog-normalize-base base) (second (vocabulary)))
      (multiple-value-bind (host port) (endpoint-host base)
        (let* ((labels (uiop:split-string (or host "") :separator "."))
               (named (and host (notevery (lambda (ch) (or (digit-char-p ch) (char= ch #\.))) host)
                           (string/= host "localhost")))
               (words (if named
                          (or (remove-if (lambda (label) (member label '("api" "www") :test #'string=))
                                         (butlast labels))
                              labels)
                          (append labels (and port (list port)))))
               (id (or (provider-id-text (format nil "~{~a~^-~}" words))
                       (provider-id-text world))))
          (if (or (catalog-entry id) (nle::find-lane-by-name id nil))
              (format nil "~a-~a" id (provider-id-text world))
              id)))))

(defun provider-fact (index trail source world)
  "The :PROVIDER fact a credential entry yields, or NIL when no id resolves."
  ;; An endpoint under a built-in's name that is not the built-in's own is
  ;; another provider that a home files under a wire's name -- Qwen Code lists
  ;; every OpenAI-compatible endpoint under `modelProviders.openai'. It lands
  ;; under an id of its own, CLAIMED remembering the name the home used, so an
  ;; import never repoints the built-in and its key never files under it. So
  ;; does an endpoint a home names by its own key, and the key leaves the
  ;; source line; with no endpoint there is no id.
  (let* ((claimed (entry-provider-id index trail))
         (base (text-of-base index))
         (id (cond ((nle::key-shaped-p claimed) (and base (endpoint-id base world)))
                   ((and claimed base
                         (nlk:when-let (own (builtin-endpoint claimed))
                           (not (equal (endpoint-host base) (endpoint-host own)))))
                    (endpoint-id base world))
                   (t claimed)))
         (key (apply #'text-of index +key-names+))
         (key-env (apply #'text-of index +key-env-names+))
         (wire (string-downcase
                (or (text-of index "sdk" "wireApi" "apiMode" "npm" "protocol" "format" "type")
                    "")))
         ;; The wire the home names in this organism's alphabet, the catalog's, then the endpoint's.
         (sdk (or (and (member wire '("openai-completions" "openai-responses" "anthropic" "google")
                               :test #'string=)
                       wire)
                  (nle::catalog-npm-lane-name wire)
                  (loop for (name . words) in '(("anthropic" "anthropic")
                                                ("openai-responses" "responses")
                                                ("google" "gemini" "google" "vertex")
                                                ("openai-completions" "openai" "chat" "completions"
                                                 "compatible"))
                        when (some (lambda (word) (cl:search word wire)) words) return name)
                  (catalog-sdk id)
                  (nlk:when-let (normal (catalog-normalize-base base))
                    (if (or (uiop:string-suffix-p normal "/anthropic")
                            (uiop:string-suffix-p normal "/messages"))
                        "anthropic"
                        "openai-completions"))))
         (models (let ((value (member-of index "models")))
                   (cond ((hash-table-p value) (sort (alexandria:hash-table-keys value) #'string<))
                         ((vectorp value) (texts-of value "id" "name" "model"))))))
    (when id
      (make-fact :kind :provider :id id :world world
                 :source (format nil "~a~@[ ~{~a~^.~}~]" source
                                 (mapcar (lambda (key) (if (nle::key-shaped-p key) "(its key)" key)) trail))
                 :value (list :id id :sdk sdk :base base :key key :key-env key-env
                              :models models :claimed (and (not (equal id claimed)) claimed))))))

;;; --- shape: an environment file, or a map keyed by variable name ------------

;;; A home that names another platform's token is named in the report and
;;; nothing else: there is no adapter here to run it.
(defparameter +platforms+
  '(("telegram" "TELEGRAM_BOT_TOKEN" "token_file" "allowed_chats")
    ("discord" "DISCORD_BOT_TOKEN" "bot_token_file" "allowed_channels"))
  "The platforms this organism runs a bot on, each as (PLATFORM TOKEN-ENV
TOKEN-FIELD CHATS-KEY).")

(defun env-map-facts (index source world)
  "A :PROVIDER fact for every member of the node whose KEY is an
environment variable name holding a provider key."
  ;; This is how a .env file reads, and how a flat `{"OPENAI_API_KEY": "..."}'
  ;; map reads: the same shape, two spellings.
  (loop for (key . value) being the hash-values of index
        ;; A NAME_API_KEY-shaped key, never a bot token; ACMEROUTER_API_KEY is `acmerouter'.
        for id = (and (every (lambda (ch) (or (upper-case-p ch) (digit-char-p ch) (char= ch #\_))) key)
                      (or (some (lambda (suffix) (uiop:string-suffix-p key suffix))
                                '("_API_KEY" "_KEY" "_TOKEN" "_SECRET"))
                          (provider-for-env key))
                      (trim-text value)
                      (not (uiop:string-suffix-p key "_BOT_TOKEN"))
                      (or (provider-for-env key)
                          (let ((base key))
                            (dolist (suffix '("_API_KEY" "_APIKEY" "_KEY" "_TOKEN" "_SECRET"
                                              "_AUTH_TOKEN"))
                              (when (uiop:string-suffix-p base suffix)
                                (setf base (subseq base 0 (- (length base) (length suffix))))))
                            (provider-id-text (substitute #\- #\_ base)))))
        when id
          collect (make-fact
                   :kind :provider :id id :world world
                   :source (format nil "~a ~a" source key)
                   :value (list :id id :sdk (catalog-sdk id) :base nil
                                :key (string-trim " " value)
                                :env key))))

;;; --- shape: the MCP server map ----------------------------------------------

(defun command-words (value)
  "A server's command as (PROGRAM . ARGUMENTS): a string is the program, a
list is the program and the arguments it was written with — OpenCode says
`command: [\"npx\", \"-y\", \"pkg\"]' where Claude says `command: \"npx\"'
beside `args'."
  (cond ((trim-text value) (list (trim-text value)))
        ((vectorp value) (string-members value))))

(defun mcp-map-p (object trail)
  "Whether OBJECT is a map of MCP servers: every member a server, and
either more than one of them or a key that names the map."
  (and (hash-table-p object)
       (plusp (hash-table-count object))
       (loop for entry being the hash-values of object
             ;; A server by structure: only server members, and a command or an http(s) url.
             always (and (hash-table-p entry)
                         (plusp (hash-table-count entry))
                         ;; Every member an MCP server entry may carry, normalized.
                         (loop for name being the hash-keys of entry
                               always (member (normalize-key name)
                                              '("command" "args" "env" "url" "headers" "type" "transport"
                                                "enabled" "disabled" "timeout" "timeoutms" "startuptimeoutsec"
                                                "tooltimeoutsec" "cwd" "description" "name" "alwaysallow"
                                                "autoapprove" "trust" "icon" "note" "tools" "envfile"
                                                "startuptimeoutms" "roots" "oauth" "headersenv" "workingdirectory"
                                                "environment" "scope" "disabledtools" "instructions")
                                              :test #'string=))
                         (let ((index (object-index entry)))
                           (or (command-words (member-of index "command"))
                               (url-p (text-of index "url"))))))
       (or (> (hash-table-count object) 1)
           (member (normalize-key (car (last trail)))
                   '("mcpservers" "mcp" "servers" "mcpserver" "contextservers" "mcpservermap")
                   :test #'equal))))

(defun string-keyed-members (table &aux (kept (nlk:json-object)))
  "TABLE's members under string keys, as an object; NIL when there are none."
  (when (hash-table-p table)
    (loop for key being the hash-keys of table using (hash-value value)
          when (and (stringp key) (or (stringp value) (realp value)))
            do (setf (gethash key kept) (if (stringp value) value (princ-to-string value)))))
  (and (plusp (hash-table-count kept)) kept))

(defun mcp-facts (object trail source world)
  (loop for name being the hash-keys of object using (hash-value entry)
        for index = (object-index entry)
        for command = (command-words (member-of index "command"))
        for args = (append (rest command) (string-members (member-of index "args")))
        for url = (text-of index "url")
        for timeout-ms = (let ((ms (member-of index "timeoutMs")) (s (member-of index "timeout")))
                          (if (realp ms) ms (and (realp s) (* 1000 s))))
        for server = (nlk:json-object
                      :when command "command" (first command)
                      :when args "args" (coerce args 'vector)
                      :opt "env" (string-keyed-members (member-of index "env" "environment"))
                      :when (url-p url) "url" url
                      :opt "headers" (string-keyed-members (member-of index "headers"))
                      :when timeout-ms "timeout_ms" (round timeout-ms)
                      :when (or (not (boolean-of index "enabled" t))
                                (boolean-of index "disabled" nil))
                      "enabled" :false)
        for notes = (and (text-of index "cwd" "workingDirectory")
                         (list "cwd dropped: the mcp folder runs a server from the organism's own directory"))
        collect (make-fact
                 :kind :mcp :id name :world world
                 :source (format nil "~a~@[ ~{~a~^.~}~]" source trail)
                 :value (list :name name :object server :notes notes))))

;;; --- shape: the model pick --------------------------------------------------

(defparameter +model-provider-names+
  '("provider" "modelProvider" "providerId")
  "Members that name the provider beside it.")

(defun model-fact (index trail source world)
  "The :MODEL fact a node yields, or NIL."
  ;; DEPTH rides the value: a pick at the root of a config outranks one inside
  ;; an agent's own section.
  (let* ((top (text-of index "model" "defaultModel" "modelId" "selectedModel"))
         (nested (member-of index "model"))
         ;; `model: {default: ..., provider: ...}' is the same pick one level in.
         (sub (and (null top) (hash-table-p nested) (object-index nested)))
         (model (if sub (text-of sub "default" "id" "name") top)))
    (when model
      ;; `anthropic/claude-x' names both; the first separator splits (`openai/gpt-oss-120b').
      (let* ((provider (or (and sub (apply #'text-of sub +model-provider-names+))
                           (apply #'text-of index +model-provider-names+)))
             (at (position-if (lambda (ch) (member ch '(#\/ #\:))) model))
             (rest (and at (subseq model (1+ at))))
             (id (or (and provider (provider-id-text provider))
                     (and at (provider-id-text (subseq model 0 at)))))
             ;; A provider named beside the model owns the whole text unless
             ;; the text leads with that provider: openrouter's model is
             ;; `anthropic/claude-sonnet-4.5', ollama's `llama3:8b'.
             (name (cond ((and at (zerop (length rest))) model)
                         ((and provider at (not (equal (provider-id-text (subseq model 0 at)) id))) model)
                         (t (or rest model)))))
        (make-fact :kind :model :id name :world world
                   :source (format nil "~a~@[ ~{~a~^.~}~]" source trail)
                   :value (list :model name :provider id
                                :depth (if sub (1+ (length trail)) (length trail))))))))

;;; --- shape: the cron job ----------------------------------------------------

(defun cron-job-fact (index source world)
  "The :CRON fact a job object yields, or NIL when it is not one: a
schedule beside something to RUN."
  ;; A job with a script rather than a prompt is still a fact — the report
  ;; names what it could not bring — but a bare schedule object, which is a
  ;; member of the job above it, is not a job.
  (let* ((schedule
           (flet ((every-minutes (minutes)
                    (and (realp minutes) (plusp minutes) (format nil "every ~dm" (max 1 (round minutes))))))
             (let ((schedule (member-of index "schedule")))
               (cond
                 ((text-of index "schedule"))
                 ((hash-table-p schedule)
                  (let* ((sub (object-index schedule))
                         (kind (text-of sub "kind" "type"))
                         (expr (text-of sub "expr" "expression" "cron")))
                    (or (and expr (or (null kind) (string-equal kind "cron")) expr)
                        (every-minutes (member-of sub "minutes" "everyMinutes"))
                        (text-of sub "runAt" "at")
                        expr)))
                 (t (or (text-of index "cron" "cronExpression" "expr")
                        (every-minutes (member-of index "intervalMinutes"))))))))
         (prompt (text-of index "prompt" "message" "task" "instruction" "text"))
         (runs (or prompt (text-of index "script" "skill" "command" "workflow"))))
    (when (and schedule runs)
      (let ((name (or (text-of index "name" "id" "title") "job")))
        (make-fact :kind :cron :id name :world world :source source
                   :value (list :name name :schedule schedule :prompt prompt
                                :model (text-of index "model")
                                :enabled (and (boolean-of index "enabled" t)
                                              (not (boolean-of index "paused" nil)))))))))

;;; --- shape: the channel bot -------------------------------------------------

(defun split-ids (text)
  "A comma- or blank-separated list of ids as ALLOWLIST-IDS reads a list of them."
  (when (stringp text)
    (allowlist-ids (coerce (uiop:split-string text :separator ", ") 'vector))))

(defun chat-id-of (text)
  "A home-channel id with any `:thread' part dropped."
  (when (stringp text)
    (string-trim " " (subseq text 0 (position #\: text)))))

(defun allowlist-ids (value)
  "VALUE, an allowlist as a home writes it, as the ids in it: a `tg:' or
`discord:' before an id dropped, and what names nobody -- `*', which lets
everyone in, or an `accessGroup:' name -- left out."
  (loop for text in (string-members value)
        for id = (ppcre:regex-replace "\\A[A-Za-z]+:(?=-?\\d+\\z)" (string-trim '(#\Space #\Tab #\@) text) "")
        unless (or (string= id "") (string= id "*") (find #\: id))
          collect id))

(defun token-file-text (path &aux (home (uiop:native-namestring (nlk:agent-home-path ""))))
  "The bot token in the file PATH names, read as its harness reads it --
`~/' and a relative path against that harness's home, a plain file of at most
4096 bytes, never a link -- or NIL; the file as a report names it, the home
as `~/', second."
  (let* ((path (string-trim " " path))
         (file (uiop:native-namestring
                (if (uiop:absolute-pathname-p (uiop:parse-native-namestring path))
                    (uiop:parse-native-namestring path)
                    (nlk:agent-home-path (if (uiop:string-prefix-p "~/" path) (subseq path 2) path)))))
         ;; Openclaw refuses a link and all but a plain file; a pipe would hold the first frame.
         (text (multiple-value-bind (kind stat) (nlk::lstat-kind file)
                 (and (eq kind :file) (<= (sb-posix:stat-size stat) 4096) (nlk:read-text file)))))
    (values (and text (plusp (length (nlk:trimmed text))) (nlk:trimmed text))
            (if (uiop:string-prefix-p home file) (format nil "~~/~a" (subseq file (length home))) file))))

(defun channel-facts (index trail source world)
  "The :CHANNEL facts a node yields: one per platform it names."
  ;; Only near the root of a document — a bot section sits at a home's top
  ;; level, under `platforms' or `channels', or in its environment file, never
  ;; six levels into a transcript. Openclaw's section keeps its token in the
  ;; file `tokenFile' names, which wins, or inline, and says who may talk as
  ;; `allowFrom' (direct messages) and `groupAllowFrom' (senders in the groups
  ;; its `groups' lists, which is not read), both user ids
  ;; (docs/channels/telegram/access-control.md, extensions/telegram/src/token.ts).
  (when (<= (length trail) 2)
    ;; SECTION is the member keyed by the platform; INDEX doubles as the environment file.
    (loop for (name token-env token-field chats-key) in +platforms+
          for upper = (string-upcase name)
          for section = (object-index (member-of index name))
          for token-file = (text-of section "tokenFile")
          for (file-token file-shown) = (and token-file (multiple-value-list (token-file-text token-file)))
          for token = (if token-file
                          file-token
                          (or (text-of section "token" "botToken") (text-of index token-env)))
          ;; `dmPolicy: disabled' refuses direct messages, so allowFrom lets people in only in groups.
          for dms-off = (equalp (text-of section "dmPolicy") "disabled")
          for users = (nlk:distinct
                       (append (allowlist-ids (member-of section "allowedUsers"))
                               (and (not dms-off) (allowlist-ids (member-of section "allowFrom")))
                               (split-ids (text-of index (format nil "~a_ALLOWED_USERS" upper)))))
          ;; Who may talk only in a group is not let in: a listed user talks here anywhere.
          for left = (append (and (allowlist-ids (member-of section "groupAllowFrom"))
                                  (list "groupAllowFrom left out: it names who may talk in groups, and no group is imported"))
                             (and dms-off (allowlist-ids (member-of section "allowFrom"))
                                  (list "allowFrom left out: direct messages are off there (dmPolicy disabled), and no group is imported")))
          for homes = (list (chat-id-of (text-of section "homeChannel"))
                            (chat-id-of (text-of index (format nil "~a_HOME_CHANNEL" upper))))
          for chats = (remove-duplicates
                       (append (remove "*" (string-members (member-of section "allowedChats" "allowedChannels"))
                                       :test #'string=)
                               (remove nil homes)
                               (split-ids (text-of index (format nil "~a_GROUP_ALLOWED_CHATS" upper))))
                       :test #'string=)
          for mention = (member-of section "requireMention")
          for reactions = (member-of section "reactions")
          when (or token token-file users chats mention reactions)
            collect (make-fact :kind :channel :id name :world world :source source
                               :value (list :platform name :token token :token-file file-shown
                                            :users users :chats chats :left left
                                            :disabled (not (boolean-of section "enabled" t))
                                            :token-field token-field :chats-key chats-key
                                            :token-env token-env
                                            :require-mention mention :reactions reactions)))))

;;; --- the classifier ---------------------------------------------------------

(defun classify-tree (root &key source world)
  "Every fact ROOT — a decoded JSON, YAML or TOML tree — holds, as a list."
  ;; A node read WHOLE is claimed, so the walk does not read its members again
  ;; as something else: an MCP server's `env' is not four credentials, and an
  ;; OAuth grant's access token is not a key. A credential entry is never
  ;; claimed — a home's root object can carry both a key and the map of MCP
  ;; servers under it, and claiming the root would lose the servers.
  (let ((facts '())
        ;; A bounded probe: a home is read on a first frame; one pathological file must not own it.
        (budget 4000))
    (labels ((visit (object index trail)
               (cond
                 ((mcp-map-p object trail)
                  (setf facts (revappend (mcp-facts object trail source world) facts))
                  t)
                 ;; An OAuth grant, named, never carried; a grant-named member must look like a token.
                 ((or (some (lambda (name) (secret-looking-p (text-of index name)))
                            '("refresh" "refreshToken" "idToken" "oauth" "oauthToken"))
                      (equalp "oauth" (text-of index "type" "mode" "authType" "method"))
                      (and (text-of index "accessToken")
                           (or (member-of index "expires") (member-of index "expires_at")
                               (member-of index "expiry") (member-of index "expiresAt"))))
                  (let ((id (or (entry-provider-id index trail)
                                (and trail (format nil "~{~a~^.~}" trail))
                                source)))
                    (push (make-fact :kind :oauth :id id :world world
                                     :source (format nil "~a~@[ ~{~a~^.~}~]" source trail)
                                     :value (list :id id))
                          facts))
                  t)
                 (t
                  ;; A key where a credential belongs: its file, a key saying so, or an endpoint.
                  (when (and (or (secret-looking-p (apply #'text-of index +key-names+))
                                 (apply #'text-of index +key-env-names+))
                             (or (some (lambda (key)
                                         (member (normalize-key key) +credential-containers+
                                                 :test #'string=))
                                       trail)
                                 (member (normalize-key (pathname-name (pathname (or source ""))))
                                         '("auth" "credentials" "authprofiles" "keys" "secrets"
                                           "apikeys" "env")
                                         :test #'equal)
                                 (text-of-base index)))
                    (nlk:when-let (fact (provider-fact index trail source world))
                      (push fact facts)))
                  (setf facts (revappend (env-map-facts index source world) facts))
                  (dolist (fact (list (model-fact index trail source world)
                                      (cron-job-fact index source world)))
                    (when fact (push fact facts)))
                  (setf facts (revappend (channel-facts index trail source world) facts))
                  nil)))
             (descend (node trail depth)
               (cond
                 ;; Six deep reaches agents.<name>.auth.profiles.<id>.<member>, not a transcript.
                 ((or (<= budget 0) (> depth 6)) nil)
                 ((hash-table-p node)
                  (decf budget)
                  (unless (visit node (object-index node) trail)
                    (loop for key being the hash-keys of node using (hash-value value)
                          do (descend value (append trail (list key)) (1+ depth)))))
                 ((vectorp node)
                  (loop for item across node
                        do (descend item trail (1+ depth)))))))
      (descend root '() 0))
    (nreverse facts)))
