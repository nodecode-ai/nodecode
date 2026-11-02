;;;; settings-sheet.lisp --- /settings: every setting on one sheet in the middle of
;;;; the frame, each changed where it stands.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The sheet reads the route the web page's Settings tab reads (GET
;;;; /api/gateway/config, surface/settings.lisp) and writes a member at a time
;;;; through it, so the config owner's validation and writer are the only
;;;; ones. The rail is organized by what a section is for, each setting in
;;;; one place: General, Models and Latest changes
;;;; first, then Agent, Tools, Channels and System (+SETTINGS-PAGES+), a
;;;; page gathering the config sections that are one thing to the operator.
;;;; General and Models are the shell's own: the theme and the transcript
;;;; view land through the writers /themes and /details land through, the
;;;; reasoning effort through /think's; the default, auxiliary and
;;;; reflections models open the model picker, and every provider's section
;;;; stands under them. Latest changes lists what the release this
;;;; organism runs changed (GET /api/gateway/release). A wide window shows
;;;; the pages in a rail beside the open one, every setting's options on its
;;;; row; a narrow one shows the pages, or one of them, at a time, a
;;;; setting's options once it is chosen. Typing searches every setting.
;;;; General's last row gives every changed setting its default back, as
;;;; Delete gives one, once the operator has seen which.

(in-package #:nodecode.evolved)

;;; --- the words a setting is said in -----------------------------------------

(defun settings-section-fields (section)
  "Every field SECTION declares, in its order: a secret one says only whether
it is set, never what it holds."
  (coerce (json-array section "fields") 'list))

(defun settings-label (name)
  "Display policy only: canonical keys remain untouched in row values and writes."
  (or (cdr (assoc name
                 '(("notify" . "Notifications") ("update" . "Updates")
                   ("tui" . "Terminal") ("notification_popups" . "Popups")
                   ("request_bodies" . "Request logging") ("websearch" . "Web search")
                   ("claude-code" . "Claude Code") ("qa" . "Diagnostics")
                   ("kimi-code" . "Kimi Code") ("openai-codex" . "OpenAI Codex")
                   ("experience" . "Reflections") ("speech" . "Spoken replies")
                   ("transcription" . "Audio transcription")
                   ("require_mention" . "Require a mention") ("stream" . "Stream replies")
                   ("reactions" . "React to messages") ("pairing" . "Allow pairing")
                   ("allow_bots" . "Messages from bots") ("dm_policy" . "Direct messages")
                   ("group_policy" . "Group direct messages")
                   ("turn_budget_minutes" . "Turn time limit")
                   ("room_tokens" . "Conversation history")
                   ("catch_up_minutes" . "Catch-up window")
                   ("voice_autojoin" . "Join voice automatically")
                   ("voice_idle_minutes" . "Voice idle timeout")
                   ("hot_budget" . "Index size") ("stale_days" . "Inactive after")
                   ("keep_mb" . "Stored request limit") ("check_minutes" . "Check for updates every")
                   ("max_seconds" . "Maximum recording length")
                   ("max_characters" . "Maximum spoken length") ("speed_percent" . "Speech speed")
                   ("timeout_seconds" . "Request time limit") ("timeout_minutes" . "Job time limit")
                   ("recap" . "Include reflection recap") ("reflect" . "Reflect on")
                   ("recap_lines" . "Recap length") ("verification_repair_attempts" . "Verification retries")
                   ("session_budget" . "Conversations per import") ("share" . "Share diagnostics")
                   ("nodes" . "Team size") ("budget_tokens" . "Token budget")
                   ("budget_seconds" . "Time budget") ("max_results" . "Results per search")
                   ("background" . "Keep Chrome in background"))
                 :test #'equal))
      ;; A secret taken by reference is two members: the variable holding it
      ;; and the file holding it.
      (loop for (suffix . word) in '(("_env" . " variable") ("_file" . " file"))
            for stem = (and (uiop:string-suffix-p name suffix) (> (length name) (length suffix))
                            (subseq name 0 (- (length name) (length suffix))))
            when stem return (concatenate 'string (settings-label stem) word))
      (string-capitalize (substitute #\Space #\- (substitute #\Space #\_ name))
                         :end (min 1 (length name)))))

(defun settings-choice-label (field option)
  "Readable choice text; selection still submits the owner's exact option."
  (or (and (equal "voice_replies" (json-value field :string "name"))
           (cdr (assoc option '(("on" . "Voice messages only") ("tts" . "Every reply")
                                ("off" . "Off")) :test #'equal)))
      (cdr (assoc option '(("auto" . "Automatic") ("ask" . "Ask first")
                          ("attached" . "Attached shells") ("deny" . "Block")
                          ("tts" . "Text to speech") ("duckduckgo" . "DuckDuckGo"))
                  :test #'equal))
      (settings-label option)))

(defun settings-field-value (field)
  "FIELD's configured value, including false, else its declared default."
  (multiple-value-bind (value present) (gethash "value" field)
    (if present (values value t) (gethash "default" field))))

(defun settings-field-text (field)
  "Configured value, else the declared default; no guessed effective value.
A secret says only whether it is set; a list of ids and a variable's name
say it in the words the organism reads them with."
  (multiple-value-bind (value present) (settings-field-value field)
    (cond ((json-value field :boolean "secret")
           (if (json-value field :boolean "set") "Set" "Not set"))
          ((not present) "Not set")
          ((member (json-value field :string "type") '("list" "env") :test #'equal)
           (json-value field :string "text"))
          ((equal "boolean" (json-value field :string "type")) (if value "On" "Off"))
          ((equal "choice" (json-value field :string "type")) (settings-choice-label field value))
          (t (format nil "~a~a" value
                     (or (loop for (suffix . unit) in '(("_minutes" . " min") ("_seconds" . " sec")
                                                       ("_days" . " days") ("_percent" . "%")
                                                       ("_mb" . " MB") ("_ms" . " ms")
                                                       ("_tokens" . " tokens") ("_characters" . " characters")
                                                       ("_lines" . " lines"))
                               when (uiop:string-suffix-p (json-value field :string "name") suffix)
                                 return unit)
                         ""))))))

(defun settings-apply-text (section)
  "The config owner's application timing, not a claim that a cell restarted."
  (cond ((equal "now" (json-value section :string "applies")) "applies now")
        ((equal "cell" (json-value section :string "applies"))
         ;; A cell is named as its section is: nodecode-chrome is Chrome.
         (let ((cell (json-value section :string "cell")))
           (format nil "restart ~a to apply"
                   (settings-label (if (uiop:string-prefix-p "nodecode-" cell) (subseq cell 9) cell)))))
        (t "restart Nodecode to apply")))

(defun settings-probe (model section &optional field)
  "What SECTION's declaration can ask, asked where the organism runs (POST
/api/gateway/config/probe): FIELD's choices, or without one the section's
check."
  (gateway-model-http-call model :post "/api/gateway/config/probe"
                           (encode-json-object (json-object "path" (json-array section "path")
                                                            :opt "field" field))))

;;; --- the rows ---------------------------------------------------------------
;;; A row is said once, from the route's answer (or the shell's own state, for
;;; General), and painted from that every frame: the answer is the authority,
;;; a row its projection, rebuilt whole whenever an answer lands.

(defstruct (setting (:constructor make-setting
                        (&key label kind text value set options doc min part field
                              apply remote act)))
  "One row of the sheet: what it is called; KIND, how it changes -- :TOGGLE,
:CHOICE, :NUMBER, :TEXT, :LIST, :IDS (a list the platform names), :SECRET,
:PASTE (a secret kept by reference), :CHECK, :ACTION (a command it opens),
:RESET (every setting back to its default), :CHANGE (a line of what a
release changed) or :NOTE (nothing); TEXT, the words its value is said in,
and VALUE itself; SET, whether it is set rather than at its default; the
OPTIONS a choice steps through as (VALUE . LABEL); the DOC said under it
once it is chosen. A change lands through FIELD of PART's section on the
config route, or for a row of the shell's own through APPLY, (MODEL VALUE)
on the loop, or REMOTE, (MODEL VALUE) off it; ACT is what an action opens,
a command's name or a function of the MODEL."
  label kind text value set options doc min part field apply remote act)

(defstruct (settings-section (:constructor make-settings-section
                                 (&key key label group guide needs json applies head)))
  "One section of the sheet, or one page of the rail: its KEY (its config
section's, or the page's own), LABEL, the GROUP heading it in the rail, its
GUIDE, what it still NEEDS, when a change APPLIES -- NIL for a page whose
sections apply apart -- its JSON as the route answered it, and its ROWS. A
section a page gathers with others stands under its HEAD there; the page
shows its BADGE beside its name until it is opened, and why its own read
FAILED, whole."
  key label group guide needs json applies head badge failed (rows '()))

(nlk:access (setting setting) (part settings-section) (page settings-section))

(defparameter +field-labels+
  '(("codex-auth.endpoint" . "Sign-in endpoint") ("welcome.note" . "Welcome note"))
  "A field's name in one section's words, where the name alone says another
thing beside the fields it stands with: the ChatGPT sign-in's endpoint under
OpenAI Codex's base url, the welcome's note among the notifications.")

(defun field-setting (part field)
  "FIELD, one of PART's declared fields, as a row."
  (let* ((type (json-value field :string "type"))
         (kind (cond ((json-value field :boolean "secret") :secret)
                     ((equal type "boolean") :toggle)
                     ((equal type "choice") :choice)
                     ((equal type "integer") :number)
                     ((equal type "list") (if (json-value field :boolean "choices") :ids :list))
                     (t :text))))
    (make-setting
     :label (let ((name (json-value field :string "name")))
              (or (cdr (assoc (format nil "~a.~a" part.key name) +field-labels+ :test #'equal))
                  (settings-label name)))
     :kind kind :part part :field field
     :text (settings-field-text field)
     :value (let ((value (settings-field-value field)))
              (case kind
                (:toggle (json-true-p value))
                ((:list :ids) (coerce (or (json-value value :array) #()) 'list))
                (:secret nil)
                (t value)))
     :set (json-value field :boolean "set")
     :min (json-value field :integer "min")
     :doc (or (json-value field :text "doc") "")
     :options (case kind
                (:toggle '((t . "On") (nil . "Off")))
                ;; A choice with no default can be left unset, and says so.
                (:choice (append (unless (nth-value 1 (gethash "default" field))
                                   (list (cons nil "Not set")))
                                 (map 'list (lambda (option)
                                              (cons option (settings-choice-label field option)))
                                      (json-array field "options"))))))))

(defun paste-setting (part &aux (section part.json) (secret (json-value section :object "secret")))
  "The row that takes PART's secret by reference while neither member that
would name it is set: what is pasted is kept in a 0600 file, which the
section then names; NIL for a section with no such secret, or one set."
  (when secret
    (let ((members (list (json-value secret :string "env") (json-value secret :string "file")))
          (noun (or (json-value secret :text "noun") "secret")))
      (unless (some (lambda (field)
                      (and (member (json-value field :string "name") members :test #'equal)
                           (json-value field :boolean "set")))
                    (settings-section-fields section))
        (make-setting :label (string-capitalize noun :end 1) :kind :paste :part part :text "Not set"
                      :doc (format nil "Paste the ~a: it is kept in ~a, readable by you alone, ~
                                        and never shown."
                                   noun (json-value secret :string "to")))))))

(defparameter +models-fields+ '(("experience" "provider" "model"))
  "The fields a section names a model by that stand on Models as one row of
their own, not on their section's page: (KEY NAME...).")

(defun config-section (section &aux (path (coerce (json-array section "path") 'list))
                                    (key (json-value section :string "key")))
  "SECTION, as the config route answered it, as a section of the sheet. Its
GROUP is where it stands when +SETTINGS-PAGES+ names no page for it: a
section nested in another under its parent's name, the core's own under
System, every other one a cell's, under Tools; a provider's stands on
Models (SETTINGS-PAGES)."
  (let ((part (make-settings-section
               :key key
               :label (settings-label (car (last path)))
               :group (cond ((rest path) (settings-label (first path)))
                            ((equal "now" (json-value section :string "applies")) "System")
                            (t "Tools"))
               :guide (or (json-value section :text "guide") "")
               :needs (let ((needs (json-value section :text "needs")))
                        (and needs (string/= needs "complete") needs))
               :applies (settings-apply-text section)
               :json section))
        (models (cdr (assoc key +models-fields+ :test #'equal))))
    (setf part.rows (append (when (and (equal "channels" (first path)) (rest path))
                              ;; A channel sets up step by step on a sheet of its own.
                              (let ((id (second path)))
                                (list (make-setting :label "Set up" :kind :action :part part :text "Step by step"
                                                    :doc (format nil "The token, a wait while the bot joins a server, then its channels and people from a checklist: /gateway setup ~a." id)
                                                    :act (lambda (model) (open-channel-setup model id))))))
                            (when-let (row (paste-setting part)) (list row))
                            (loop for field in (settings-section-fields section)
                                  unless (member (json-value field :string "name") models :test #'equal)
                                    collect (field-setting part field))
                            (when (json-value section :boolean "check")
                              (list (make-setting :label "Check" :kind :check :part part :text "Ask now"
                                                  :doc "Ask the platform whether this section works as it is set.")))))
    part))

(defun identity-text (model-id provider)
  "A model and its provider as one row says them."
  (if (plusp (length model-id))
      (format nil "~a~@[ · ~a~]" model-id (json-value provider :text))
      "Not set"))

(defun choice-texts (part)
  "PART, each of its choices saying its value in its option's words."
  (dolist (setting part.rows part)
    (when (eq setting.kind :choice)
      (setf setting.text (or (cdr (assoc setting.value setting.options :test #'equal)) "Not set")))))

(defun general-section (model)
  "The shell's own look, which no config section declares: the theme and the
transcript view, which land through the writers /themes and /details land
through, and the row that gives every changed setting its default back."
  (let* ((part (make-settings-section
                :key "general" :label "General" :group "General" :applies "applies now"
                :guide "How this shell looks, and every setting you changed given its default back."))
         (theme (or (saved-theme-selection) (configured-theme-name)))
         (view (chat-model-tool-detail model)))
    (setf part.rows
          (list (make-setting :label "Theme" :kind :choice :part part
                              :value theme :set (and (saved-theme-selection) t)
                              ;; An unreadable themes file leaves the theme in use.
                              :options (mapcar (lambda (name) (cons name name))
                                               (or (ignore-errors (theme-catalog)) (list theme)))
                              :doc "The palette every shell on this machine paints with."
                              :apply (lambda (model value)
                                       (if value
                                           (slash-themes-op model value)
                                           (with-slash-notice ("theme switch")
                                             (save-theme-selection nil)
                                             (engage-theme)
                                             nil))))
                (make-setting :label "Transcript view" :kind :choice :part part
                              :value view :set (eq view :developer)
                              :options '((:standard . "Standard") (:developer . "Developer"))
                              :doc "Standard shows each call as its name and comment; Developer groups routine calls and previews what they wrote. Ctrl+O switches it too."
                              :apply (lambda (model value)
                                       (toggle-tool-detail model (or value :standard))
                                       (render-state model)
                                       nil))
                (make-setting :label "Reset to defaults" :kind :reset :part part
                              :doc "Every setting changed here back to its default. The models, the secrets and what a section needs to work stay as they are.")))
    (choice-texts part)))

(defun section-path-to (part name)
  "The member keys of PART's member NAME from the config's root."
  (concatenate 'vector (json-array (settings-section-json part) "path") (vector name)))

(defun reflections-model-lands (experience)
  "Where a pick for the Reflections model lands: EXPERIENCE's provider and
model members, through the config route. => (values NOTICE FAILED)."
  (lambda (model tokens)
    (handler-case
        (destructuring-bind (provider model-id) tokens
          (loop for (name value) in `(("provider" ,provider) ("model" ,model-id))
                do (gateway-model-http-call model :post "/api/gateway/config"
                                            (json-object "path" (section-path-to experience name)
                                                         "value" value)))
          (values (format nil "reflections model: ~a/~a · ~a" provider model-id
                          (settings-section-applies experience))
                  nil))
      (nlk:config-refusal (condition) (values (nlk:config-refusal-detail condition) t))
      (error (condition) (values (princ-to-string condition) t)))))

(defun reflections-model-setting (experience)
  "The Reflections model as one row: EXPERIENCE's provider and model, which
the model picker sets together and Delete takes out together."
  (let* ((fields (settings-section-fields (settings-section-json experience)))
         (field (lambda (name) (find name fields :key (lambda (field) (json-value field :string "name")) :test #'equal)))
         (set (loop for name in '("provider" "model")
                    when (json-value (funcall field name) :boolean "set") collect name))
         (value (lambda (name) (json-value (funcall field name) :text "value"))))
    (make-setting :label "Reflections model" :kind :action :part experience
                  :act (lambda (model) (open-models-picker model :lands (reflections-model-lands experience)))
                  :set (and set t)
                  :text (if (funcall value "model")
                            (identity-text (funcall value "model") (funcall value "provider"))
                            "The session's own")
                  :doc "The model a reflection runs on: the session's own, sharing its prompt cache, unless one is picked here."
                  :remote (lambda (model value)
                            (declare (ignore value))
                            (dolist (name set)
                              (gateway-model-http-call model :post "/api/gateway/config"
                                                       (json-object "path" (section-path-to experience name)
                                                                    "remove" t)))))))

(defun models-section (target parts providers)
  "The models the shell runs on: the default a new session starts on, the
reasoning effort -- TARGET's, (:LADDER :EFFORT :DEFAULT-PROVIDER
:DEFAULT-MODEL) as GET /api/gateway/target answered it, NIL until it lands
-- the auxiliary model, the reflections' when PARTS hold their section; then
every one of PROVIDERS' settings, each under its name (PROVIDER-PARTS)."
  (let* ((page (make-settings-section
                :key "models" :label "Models" :group "General" :applies "applies now"
                :guide "The model a new session starts on, the auxiliary model, the reflections' model, the decider, and the providers that serve them."))
         (ladder (getf target :ladder))
         (effort (getf target :effort))
         ;; The config's pin, (PROVIDER MODEL): AUXILIARY-MODEL would resolve
         ;; the fallback's reasoning rung through the model catalog, on the
         ;; loop, and *AUXILIARY-MODEL* is seeded at a store open, which an
         ;; attached shell never makes.
         (aux (multiple-value-list (shared-model-pin "auxiliary_model")))
         (decider (multiple-value-list (shared-model-pin "decider_model")))
         (experience (find "experience" parts :key #'settings-section-key :test #'equal)))
    (setf page.rows
          (append
           (list (make-setting :label "Default model" :kind :action :part page
                               :act (lambda (model) (open-models-picker model :lands :default))
                               :text (if target
                                         (identity-text (getf target :default-model) (getf target :default-provider))
                                         "Reading…")
                               :doc "The model every new session starts on; /models picks this session's own as well.")
                 (if ladder
                     (make-setting :label "Reasoning effort" :kind :choice :part page
                                   :value effort :set (and effort t)
                                   :options (cons '(nil . "Default")
                                                  (mapcar (lambda (rung)
                                                            (cons rung (if (equal rung "xhigh")
                                                                           "XHigh"
                                                                           (string-capitalize rung))))
                                                          ladder))
                                   :doc "How hard the model thinks, from the next turn, here and in every new session; Default is the model's own."
                                   :remote #'apply-effort)
                     (make-setting :label "Reasoning effort" :kind :note :part page
                                   :text (if target "None for this model" "Reading…")
                                   :doc "This model offers no reasoning effort controls."))
                 (make-setting :label "Auxiliary model" :kind :action :part page
                               :act (lambda (model) (open-models-provider-picker model :tab "aux"))
                               :set (and (first aux) t)
                               :text (if (first aux)
                                         (identity-text (second aux) (first aux))
                                         "Same as main")
                               :doc "Naming a session, a recap, a summary: they run on this model, on /models' Auxiliary tab.")
                 (make-setting :label "Decider model" :kind :action :part page
                               :act (lambda (model) (open-models-provider-picker model :tab "decider"))
                               :set (and (first decider) t)
                               :text (if (first decider)
                                         (identity-text (second decider) (first decider))
                                         "None")
                               :doc "Judgments as probabilities (POST /v1/systemone): the decider a guard, a triage or a router asks, on /models' Decider tab."))
           (and experience (list (reflections-model-setting experience)))
           (loop for part in providers append part.rows)))
    (choice-texts page)))

(define-startup-parameter *changes-seen-path*
  (home "changes-seen")
  "Where /settings keeps the release whose Latest changes the operator last
opened, so the page's `new' goes once it is seen. NIL disables persistence;
tests bind.")

(defun changes-seen ()
  "The release whose Latest changes were last opened, or NIL."
  (when *changes-seen-path*
    (let ((label (nlk:trimmed (or (ignore-errors (nlk:read-text *changes-seen-path*)) ""))))
      (and (plusp (length label)) label))))

(defun see-changes (label)
  "Keep LABEL as the release whose Latest changes were opened; best-effort."
  (when (and *changes-seen-path* label)
    (ignore-errors
      (nlk::with-output-file (out *changes-seen-path*)
        (write-string label out))
      t)))

(defun latest-section (release)
  "What the release the organism runs changed, as the page Latest changes:
RELEASE as GET /api/gateway/release answered it, (:ERROR TEXT) when it did
not, NIL until it lands -- a change a row, newest first, and the release's
label where a section says when it applies. Its badge says `new' until it
is opened on this release (*CHANGES-SEEN-PATH*)."
  (let* ((failure (off-loop-failure release))
         (answer (and (not failure) release))
         (label (json-value answer :string "label"))
         (from (json-value answer :string "from"))
         (changes (coerce (or (json-value answer :array "changes") #()) 'list))
         (page (make-settings-section
                :key "latest" :label "Latest changes" :group "General" :applies label
                :guide (cond ((null release) "Reading what this release changed…")
                             (failure "")
                             ((not (json-value answer :boolean "listed"))
                              (format nil "The release host has no list of what ~a changed." label))
                             (from (format nil "What ~a changed since ~a, the release this home ran before: ~d change~:p, newest first."
                                           label from (length changes)))
                             (t (format nil "What changed up to ~a: ~d change~:p, newest first."
                                        label (length changes)))))))
    (setf page.failed failure
          page.badge (and changes (not (equal label (changes-seen))) "new")
          page.rows (loop for change in changes
                          for head = (json-value change :text "head")
                          collect (make-setting :label head :kind :change :part page :doc head
                                                :text (json-value change :text "label"))))
    page))

(defun gateway-page ()
  "This home's background gateway as a page of System: on, a user service
that keeps the organism running with no shell open and brings it up at
every login -- what a home nodecode names starts with
(NLK:GATEWAY-BY-DEFAULT-P) -- off what `nodecode gateway off' does. NIL
where no service manager runs one."
  (when (nlk:gateway-unit-path)
    (let* ((page (make-settings-section
                  :key "gateway" :label "Gateway" :group "System" :applies "applies now"
                  :guide "This home's organism kept running by the operating system: in the background, at every login, after a crash."))
           (setting (make-setting :label "Run in the background" :kind :choice :part page
                                  :value (if (nlk:gateway-installed-p) nil :off)
                                  :set (nlk:gateway-off-p)
                                  :options '((nil . "On") (:off . "Off"))
                                  :doc "On, the gateway is a user service: sessions, schedules and bots go on with no shell open, and it comes back at every login and after a crash. Off, the organism runs only while a shell is open.")))
      ;; The service manager is asked off the loop; the row reads the unit
      ;; file again once it answers.
      (setf setting.apply
            (lambda (model value)
              (off-loop model
                        (lambda () (if (eq value :off) (nlk:gateway-off) (nlk:gateway-on)) t)
                        (lambda (model answer &aux (settings (chat-model-dialog model)))
                          (if (not (settings-sheet-p settings))
                              :quiet
                              (progn
                                (settings-rebuild model settings)
                                (if-let (failure (off-loop-failure answer))
                                  (settings-say settings setting failure :danger)
                                  (sheet-flash settings (if (eq value :off)
                                                            "Gateway off · runs while a shell is open"
                                                            "Gateway on · comes up at every login")
                                               :success))
                                t))))
              nil)
            page.rows (list setting))
      (choice-texts page))))

;;; --- the pages ------------------------------------------------------------------

(defparameter +settings-pages+
  '(("Agent" "Reflections" nil "experience")
    ("Agent" "Memory" nil "index")
    ("Agent" "Team" nil "team")
    ("Agent" "Schedules" nil "cron")
    ("Tools" "Web search" nil "websearch")
    ("Tools" "Chrome" nil "chrome")
    ("Tools" "Guard" nil "guard")
    ("Channels" "Discord" nil "channels.discord")
    ("Channels" "Telegram" nil "channels.telegram")
    ("Channels" "Voice"
     "Voice messages the channels receive, read as text, and replies said out loud: both on this machine unless a hosted API is named."
     ("transcription" "Transcription") ("speech" "Speech"))
    ("Channels" "Link" nil "link")
    ("System" "Updates" nil "update")
    ("System" "Notifications" "A desktop notice when a turn ends, the popups this shell shows, and the brief nodecode says when it comes back."
     "tui" "notify" "welcome")
    ("System" "Import" nil "import")
    ("System" "Diagnostics"
     "What this machine can send nodecode.ai once a week, and how much of each model call's body the store keeps for the Run view."
     ("qa" "Weekly report") ("request_bodies" "Request log")))
  "The rail's pages past General, Models and Latest changes, by what each is
for, as (GROUP LABEL GUIDE SECTION...): a SECTION is a config section's key,
or (KEY HEAD) for one standing under HEAD on a page it shares. A section no
page names stands on a page of its own in its group (CONFIG-SECTION), a
provider's on Models.")

(defparameter +provider-joins+ '(("codex-auth" . "openai-codex"))
  "Provider sections that stand inside another's block on Models,
(KEY . INTO): the ChatGPT sign-in Codex's backend is reached with.")

(defun settings-page (group label guide parts)
  "PARTS, the sections one page gathers, as that page: listed under GROUP
as LABEL, said with GUIDE -- a page of one, with its section's own. A
change on a page of sections that apply apart says when under each
section's head."
  (if (rest parts)
      (let* ((applies (settings-section-applies (first parts)))
             (common (and (every (lambda (part) (equal part.applies applies)) parts) applies))
             (needs (remove nil (mapcar #'settings-section-needs parts)))
             (page (make-settings-section :key (string-downcase label) :label label :group group
                                          :guide (or guide "") :applies common
                                          :needs (and needs (format nil "~{~a~^; ~}" needs)))))
        (dolist (part parts)
          (unless (or part.head common) (setf part.head part.label)))
        (setf page.rows (loop for part in parts append part.rows))
        page)
      ;; A page of one section says its own guide: a page's speaks of all
      ;; it gathers.
      (let ((part (first parts)))
        (setf part.label label part.group group part.head nil)
        part)))

(defun provider-parts (parts)
  "The providers' sections among PARTS, as the config route marks them, in
their order, each under its own name -- save one that joins another's
(+PROVIDER-JOINS+), which follows it with no head of its own."
  (let ((providers (remove-if-not (lambda (part) (json-value part.json :boolean "provider")) parts)))
    (dolist (part providers) (setf part.head part.label))
    (loop for (key . into) in +provider-joins+
          for part = (find key providers :key #'settings-section-key :test #'equal)
          for host = (find into providers :key #'settings-section-key :test #'equal)
          when (and part host)
            do (let* ((rest (remove part providers))
                      (at (1+ (position host rest))))
                 (setf part.head nil
                       providers (append (subseq rest 0 at) (list part) (subseq rest at)))))
    providers))

(defun settings-pages (model target release parts)
  "PARTS, every config section the route answered, as the rail's pages in
its order: General, Models over TARGET, Latest changes over RELEASE, then
Agent, Tools, Channels, System -- the Gateway first (GATEWAY-PAGE) -- and
any other group, a group's named pages (+SETTINGS-PAGES+) before its
sections no page names."
  (let* ((claimed '())
         (named (loop for (group label guide . specs) in +settings-pages+
                      for members = (loop for spec in specs
                                          for (key head) = (if (consp spec) spec (list spec))
                                          for part = (find key parts :key #'settings-section-key :test #'equal)
                                          when part
                                            do (setf part.head head)
                                               (push part claimed)
                                            and collect part)
                      when members collect (settings-page group label guide members)))
         (providers (provider-parts (remove-if (lambda (part) (member part claimed)) parts)))
         (own (remove-if (lambda (part) (or (member part claimed) (member part providers))) parts))
         (pages (append (when-let (gateway (gateway-page)) (list gateway)) named own)))
    (append (list (general-section model)
                  (models-section target parts providers)
                  (latest-section release))
            (loop for group in (remove-duplicates (append '("Agent" "Tools" "Channels" "System")
                                                          (mapcar #'settings-section-group pages))
                                                  :test #'equal :from-end t)
                  append (remove group pages :key #'settings-section-group :test-not #'equal)))))

;;; --- the sheet ----------------------------------------------------------------

(defstruct (settings-sheet (:include sheet-dialog))
  "/settings: the route's last ANSWER, the TARGET Models reads, the RELEASE
Latest changes reads, the SECTIONS they project to -- the rail's pages --
and why the last read FAILED; where the operator
is -- FOCUS on the :RAIL of sections or the :PANE of one, the SECTION-AT
and SETTING-AT chosen, the first row each list shows (RAIL-TOP, PANE-TOP),
the QUERY typed and the result HIT under it, the focus the query came from
(BACK) -- and what is under way: the field typed into (EDIT, (SETTING .
TEXT)), the ids being marked (IDS, (SETTING CHOICES MARKS CURSOR)), what was
SAID under a setting ((TEXT TONE KEY LABEL)), and the rows a reset asked to
give their defaults back (ARMED, ((PART . SETTING) ...)). The last paint leaves
whether it showed one view at a time (STACKED), and the sections' two
COLUMNS, ((INDEX COLUMN ROW) ...); the key row's line, whether a call is
busy and the pointer's targets are the sheet's own (SHEET-DIALOG). The
section /settings was ASKED to open at stays until the first read lands."
  (answer nil) (target nil) (release nil) (sections '()) (failed nil)
  (focus :rail) (section-at 0) (setting-at 0) (rail-top 0) (pane-top 0)
  (query "" :type string) (hit 0) (back :rail)
  (edit nil) (ids nil) (said nil) (armed nil)
  (columns '()) (asked nil))

(nlk:access (settings settings-sheet))

(defun settings-sections (model settings &aux (answer settings.answer))
  "Every page the sheet shows, in its order (SETTINGS-PAGES). A section with
no field shows nothing and is left out."
  (let* ((parts (loop for section across (or (json-value answer :array "sections") #())
                      when (plusp (length (json-array section "fields")))
                        collect (config-section section)))
         (sections (settings-pages model settings.target settings.release parts))
         (count (length (settings-defaults sections))))
    (dolist (setting (settings-section-rows (first sections)) sections)
      (when (eq setting.kind :reset)
        (setf setting.text (if (plusp count) (format nil "~d changed" count) "Nothing changed"))))))

(defun settings-rebuild (model settings)
  "Project SETTINGS's sections anew from its answer, the choice staying on
the page and the setting of the same names, of the same section where a
page has two."
  (let* ((part (nth settings.section-at settings.sections))
         (setting (and part (nth settings.setting-at part.rows))))
    (setf settings.sections (settings-sections model settings))
    (let* ((at (or (and part (position part.key settings.sections
                                        :key #'settings-section-key :test #'equal))
                   (min settings.section-at (1- (length settings.sections)))))
           (rows (settings-section-rows (nth at settings.sections))))
      (setf settings.section-at at
            settings.setting-at (or (and setting
                                         (position-if (lambda (row)
                                                        (and (equal (setting-label row) setting.label)
                                                             (equal (settings-section-key (setting-part row))
                                                                    (settings-section-key setting.part))))
                                                      rows))
                                    (and setting (position setting.label rows
                                                           :key #'setting-label :test #'equal))
                                    (max 0 (min settings.setting-at (1- (length rows)))))
            settings.hit (max 0 (min settings.hit (1- (length (settings-results settings)))))))))

(defun settings-rank (query text)
  "Where QUERY stands in TEXT: 0 when it is all of it, 1 at its start, 2 at
a word's, 3 inside a word; NIL when it is not there."
  (let ((at (search query text :test #'char-equal)))
    (cond ((null at) nil)
          ((string-equal query text) 0)
          ((zerop at) 1)
          ((find (char text (1- at)) " _/-·(") 2)
          (t 3))))

(defun settings-results (settings &aux (query settings.query))
  "What QUERY finds, as (PART . SETTING) in the order they show: every
setting whose name holds it, every setting of a page whose name does, the
pages ranked by their best find and kept in their order otherwise. What a
release changed is read, not searched."
  (when (plusp (length query))
    (let ((found '()))
      (loop for part in settings.sections
            for order from 0
            for by-section = (settings-rank query part.label)
            for rows = (loop for setting in part.rows
                             for rank = (and (not (eq setting.kind :change))
                                             (or (settings-rank query setting.label)
                                                 (and by-section (+ 4 by-section))))
                             when rank collect (cons rank setting))
            when rows
              do (push (list (reduce #'min rows :key #'car) order part (mapcar #'cdr rows)) found))
      (loop for (nil nil part rows) in (stable-sort (nreverse found) #'< :key #'first)
            append (mapcar (lambda (setting) (cons part setting)) rows)))))

(defun settings-current (settings)
  "=> (values SETTING PART), the setting the keys act on: the result chosen
under a query, else the chosen setting of the open section; NIL on the rail."
  (cond ((plusp (length settings.query))
         (let ((hit (nth settings.hit (settings-results settings))))
           (values (cdr hit) (car hit))))
        ((eq settings.focus :pane)
         (let ((part (nth settings.section-at settings.sections)))
           (values (and part (nth settings.setting-at part.rows)) part)))))

(defun settings-said (settings setting &aux (said settings.said))
  "What was said under SETTING, (TEXT TONE), or NIL."
  (when (and said setting
             (equal (third said) (settings-section-key setting.part))
             (equal (fourth said) setting.label))
    (list (first said) (second said))))

(defun settings-say (settings setting text tone)
  "Say TEXT in TONE under SETTING until the operator moves on."
  (setf settings.armed nil
        settings.said (list text tone (settings-section-key setting.part) setting.label)))

(defun target-plist (snapshot)
  "TARGET-SNAPSHOT's values, as a list, as the plist Models reads."
  (when snapshot
    (destructuring-bind (provider model-id ladder facts) snapshot
      (list :provider provider :model model-id :ladder ladder :effort (getf facts :effort)
            :default-provider (getf facts :default-provider) :default-model (getf facts :default-model)))))

(defun settings-read (model settings)
  "Read every section, and the target Models reads, off the loop; then what
the release changed."
  (sheet-call model settings
                 (lambda ()
                   (list (gateway-model-http-call model :get "/api/gateway/config" nil)
                         (ignore-errors (multiple-value-list (target-snapshot model)))))
                 (lambda (model answer)
                   (let ((failure (off-loop-failure answer)))
                     (unless failure
                       (setf settings.answer (first answer)
                             settings.target (target-plist (second answer))))
                     ;; A config that does not read still answers its sections.
                     (setf settings.failed (or failure (json-value settings.answer :text "error")))
                     (settings-rebuild model settings)
                     (when-let (key settings.asked)
                       (setf settings.asked nil)
                       (settings-show-section settings key))
                     (settings-read-release model settings)))
                 "Reading settings…"))

(defun settings-show-section (settings key)
  "Open the page that holds the config section KEY, at that section's first
setting. => whether a page holds it."
  (loop for page in settings.sections
        for at from 0
        for row = (position key page.rows :test #'equal
                                          :key (lambda (setting) (settings-section-key (setting-part setting))))
        when (or row (equal key page.key))
          do (settings-choose settings at)
             (setf settings.focus :pane settings.setting-at (or row 0))
             (return t)))

(defun settings-read-release (model settings)
  "Read what the release the organism runs changed, off the loop and apart
from the sheet's one call at a time: the organism asks the release host for
it, and a change made meanwhile waits on nothing."
  (off-loop model
            (lambda () (gateway-model-http-call model :get "/api/gateway/release" nil))
            (lambda (model answer)
              (if (not (eq settings (chat-model-dialog model)))
                  :quiet
                  (progn (setf settings.release answer)
                         (settings-rebuild model settings)
                         t)))))

(defun settings-landed (model settings setting answer)
  "A write's ANSWER: the sections it says, and `Saved' with when it applies,
or the refusal under SETTING in its own words."
  (if-let (failure (off-loop-failure answer))
    (settings-say settings setting failure :danger)
    (let ((applies (settings-section-applies setting.part)))
      (setf settings.answer answer)
      (settings-rebuild model settings)
      (sheet-flash settings (format nil "Saved · ~a" applies)
                      (if (equal applies "applies now") :success :sheet-restart)))))

(defun settings-write (model settings setting request)
  "Send REQUEST, one write of SETTING's section, to the config route."
  (sheet-call model settings
                 (lambda () (gateway-model-http-call model :post "/api/gateway/config" request))
                 (lambda (model answer) (settings-landed model settings setting answer))
                 "Saving…"))

(defun settings-commit (model settings setting value &key remove)
  "Land VALUE as SETTING's -- or with REMOVE, its default back: a row of the
shell's own through its writer, any other through the config route. One
call at a time: a change while one is under way waits for the operator to
make it again."
  (unless settings.busy
    (setf settings.said nil)
    (cond
      (setting.apply
       (let ((notice (funcall setting.apply model value)))
         (settings-rebuild model settings)
         (if (stringp notice)
             (settings-say settings setting notice :danger)
             (sheet-flash settings "Saved · applies now" :success))))
      (setting.remote
       ;; The effort is the target's, the reflections model two config
       ;; members: both are read back.
       (sheet-call model settings
                      (lambda ()
                        (funcall setting.remote model value)
                        (list (multiple-value-list (target-snapshot model))
                              (gateway-model-http-call model :get "/api/gateway/config" nil)))
                      (lambda (model answer)
                        (if-let (failure (off-loop-failure answer))
                          (settings-say settings setting failure :danger)
                          (let ((applies (settings-section-applies setting.part)))
                            (setf settings.target (target-plist (first answer))
                                  settings.answer (second answer))
                            (settings-rebuild model settings)
                            (if (equal applies "applies now")
                                (sheet-flash settings "Saved · from the next turn" :success)
                                (sheet-flash settings (format nil "Saved · ~a" applies) :sheet-restart)))))
                      "Saving…"))
      (t
       (let ((path (setting-path setting)))
         (settings-write model settings setting
                         (if remove
                             (json-object "path" path "remove" t)
                             ;; A false is spelled so on the wire.
                             (json-object "path" path "value" (if (null value) :false value)))))))))

(defun setting-path (setting)
  "The member keys of SETTING's field from the config's root."
  (concatenate 'vector (json-array (settings-section-json setting.part) "path")
               (vector (json-value setting.field :string "name"))))

(defun settings-resettable-p (setting)
  "Whether Delete gives SETTING something back: a config member that is set,
or a row of the shell's own set over its default -- an effort picked over
the model's own, a theme picked, the Developer view."
  (and setting setting.set (not (eq setting.kind :secret))
       (or setting.field setting.remote setting.apply)
       t))

(defun settings-reset (model settings setting)
  "Give SETTING its default back."
  (when (settings-resettable-p setting)
    (if setting.field
        (settings-commit model settings setting nil :remove t)
        (settings-commit model settings setting nil))))

(defun settings-defaults (sections)
  "What Reset to defaults gives back, as (PART . SETTING) in the sheet's
order: every setting of SECTIONS Delete would reset, save the members a
section cannot do without (a required one, a member of a ONE-OF or ANY-OF
group: the secret's reference, the ids a channel answers)."
  (loop for part in sections
        append (loop for setting in part.rows
                     when (and (settings-resettable-p setting)
                               (not (and setting.field (json-value setting.field :boolean "essential"))))
                       collect (cons part setting))))

(defun settings-armed (settings setting)
  "The rows Enter on SETTING gives their defaults back, while the sheet asks
under it; NIL once the operator moved on."
  (and (settings-said settings setting) settings.armed))

(defun settings-arm (settings setting)
  "Enter on Reset to defaults: say under SETTING how many it gives back, and
that Enter again does, the rows listed under it."
  (if-let (rows (settings-defaults settings.sections))
    (progn (settings-say settings setting
                         (if (rest rows)
                             (format nil "Enter again gives these ~d their defaults back; Esc keeps them."
                                     (length rows))
                             "Enter again gives this one its default back; Esc keeps it.")
                         :danger)
           (setf settings.armed rows))
    (settings-say settings setting "Every setting is at its default." :system)))

(defun reset-label (row &aux (page (car row)) (setting (cdr row)))
  "ROW, (PAGE . SETTING), as a reset says it: the head it stands under, else
its page, then its name."
  (format nil "~a › ~a" (or (settings-section-head setting.part) page.label) setting.label))

(defun reset-off-loop (model row &aux (setting (cdr row)))
  "Give ROW's setting its default back off the loop: a config member taken
out through the route, the effort through its writer. => why not, or NIL."
  (with-handlers ((nlk:config-refusal (condition) (nlk:config-refusal-detail condition))
                  (error (condition) (princ-to-string condition)))
    (cond (setting.field
           (gateway-model-http-call model :post "/api/gateway/config"
                                    (encode-json-object (json-object "path" (setting-path setting) "remove" t))))
          (setting.remote (funcall setting.remote model nil)))
    nil))

(defun settings-reset-all (model settings asking rows)
  "Give each of ROWS, (PART . SETTING), its default back, ASKING the row
that asked: the theme and the view through their writers on the loop, the
effort through its writer and each config member through the route off it,
one at a time -- a refusal said under ASKING in its own words, the rest
still written -- and every section read anew."
  (unless settings.busy
    (let ((kept (loop for row in rows
                      for apply = (setting-apply (cdr row))
                      for notice = (and apply (funcall apply model nil))
                      when (stringp notice) collect (format nil "~a: ~a" (reset-label row) notice)))
          (later (find "applies now" rows :key (lambda (row) (settings-section-applies (setting-part (cdr row))))
                                          :test-not #'equal)))
      (setf settings.said nil settings.armed nil)
      (sheet-call model settings
                  (lambda ()
                    (let ((refused (loop for row in rows
                                         for why = (reset-off-loop model row)
                                         when why collect (format nil "~a: ~a" (reset-label row) why))))
                      (list (gateway-model-http-call model :get "/api/gateway/config" nil)
                            (ignore-errors (multiple-value-list (target-snapshot model)))
                            refused)))
                  (lambda (model answer)
                    (let ((failure (off-loop-failure answer)))
                      (unless failure
                        (setf settings.answer (first answer)
                              settings.target (target-plist (second answer))
                              kept (append kept (third answer))))
                      (settings-rebuild model settings)
                      (cond (failure (settings-say settings asking failure :danger))
                            (kept (settings-say settings asking
                                                (format nil "~d of ~d kept their value. ~{~a~^; ~}"
                                                        (length kept) (length rows) kept)
                                                :danger))
                            (t (sheet-flash settings
                                            (format nil "Reset ~d setting~:p · ~:[applies now~;some apply after a restart~]"
                                                    (length rows) later)
                                            (if later :sheet-restart :success))))))
                  "Resetting…"))))

;;; --- changing a setting where it stands ----------------------------------------

(defun number-step (value)
  "How far one arrow moves the number VALUE: a tenth of its order, at least 1."
  (max 1 (expt 10 (1- (floor (log (max 1 (abs value)) 10))))))

(defun settings-nudge (model settings setting direction &key wrap)
  "Step SETTING one option -- a switch's On and Off are two -- or one step
of its number, in DIRECTION (-1 or 1), wrapping past either end when WRAP;
nothing past an end otherwise."
  (case setting.kind
    ((:toggle :choice)
     (let* ((count (length setting.options))
            (at (position setting.value setting.options :key #'car :test #'equal))
            (to (cond ((null at) (if (plusp direction) 0 (1- count)))
                      (wrap (mod (+ at direction) count))
                      (t (max 0 (min (1- count) (+ at direction)))))))
       (unless (eql at to)
         (let ((value (car (nth to setting.options))))
           ;; A choice left unset has its member taken out; a switch's
           ;; Off is a false like any other value.
           (if (and (null value) setting.field (eq setting.kind :choice))
               (settings-commit model settings setting nil :remove t)
               (settings-commit model settings setting value))))))
    (:number
     (let* ((value setting.value)
            (floor (or setting.min 0))
            (to (cond ((integerp value) (max floor (+ value (* direction (number-step value)))))
                      ((plusp direction) (max floor 1)))))
       (when (and to (not (eql to value)))
         (settings-commit model settings setting to))))))

(defun settings-edit-begin (settings setting)
  "Open SETTING's field in place, holding what it is set to -- nothing for
a secret, which is never shown."
  (setf settings.said nil
        settings.edit (cons setting
                            (case setting.kind
                              ((:secret :paste) "")
                              ((:list) (format nil "~{~a~^ ~}" setting.value))
                              (t (if setting.value (princ-to-string setting.value) ""))))))

(defun settings-edit-submit (model settings &aux (setting (car settings.edit))
                                                 (text (nlk:trimmed (cdr settings.edit))))
  "Enter in a field: write what it holds, or say why not and keep it open."
  (case setting.kind
    (:number
     (if-let (number (ignore-errors (parse-integer text)))
       (progn (setf settings.edit nil) (settings-commit model settings setting number))
       (settings-say settings setting "Enter a whole number; nothing was saved." :danger)))
    (:list
     (setf settings.edit nil)
     (settings-commit model settings setting
                      (coerce (split-words (substitute #\Space #\, text)) 'vector)))
    (:secret
     (setf settings.edit nil)
     (when (plusp (length text)) (settings-commit model settings setting text)))
    (:paste
     (setf settings.edit nil)
     (when (plusp (length text))
       (settings-write model settings setting
                       (json-object "path" (json-array (settings-section-json setting.part) "path")
                                    "secret" text))))
    (t
     (setf settings.edit nil)
     (if (plusp (length text))
         (settings-commit model settings setting text)
         (settings-commit model settings setting nil :remove t)))))

(defun settings-ask-ids (model settings setting)
  "Ask the platform which ids SETTING may name (SETTINGS-PROBE), and mark
the ones it names now; an id it names that the platform did not offer stays
a choice."
  (unless settings.busy
    (setf settings.said nil)
    (sheet-call model settings
                   (lambda () (settings-probe model (settings-section-json setting.part)
                                              (json-value setting.field :string "name")))
                   (lambda-ignoring (model answer)
                     (if-let (failure (off-loop-failure answer))
                       (settings-say settings setting failure :danger)
                       (let* ((offered (loop for choice across (or (json-value answer :array "choices") #())
                                             collect (cons (json-value choice :string "id")
                                                           (or (json-value choice :text "label")
                                                               (json-value choice :string "id")))))
                              (kept (loop for id in setting.value
                                          unless (assoc id offered :test #'equal)
                                            collect (cons id "kept from the config"))))
                         (setf settings.ids (list setting (append offered kept)
                                                  (copy-list setting.value) 0)))))
                   "Asking…")))

(defun settings-check (model settings setting)
  "Run SETTING's section's own check where the organism runs it, and say its
answer -- or its refusal -- under the row."
  (unless settings.busy
    (setf settings.said nil)
    (sheet-call model settings
                   (lambda () (settings-probe model (settings-section-json setting.part)))
                   (lambda-ignoring (model answer)
                     (if-let (failure (off-loop-failure answer))
                       (settings-say settings setting failure :danger)
                       (settings-say settings setting
                                     (first-line (or (json-value answer :text "text")
                                                     "the check said nothing"))
                                     :system)))
                   "Checking…")))

(defun settings-act (model settings setting)
  "Enter on SETTING: a switch or a choice steps on, a value opens its field,
ids ask the platform, a check runs, an action closes the sheet for its
command or its picker."
  (case setting.kind
    ((:toggle :choice) (settings-nudge model settings setting 1 :wrap t))
    ((:number :text :list :secret :paste) (settings-edit-begin settings setting))
    (:ids (settings-ask-ids model settings setting))
    (:check (settings-check model settings setting))
    (:reset (settings-arm settings setting))
    (:action
     (dismiss-dialog model nil)
     (multiple-value-bind (notice) (if (functionp setting.act)
                                       (funcall setting.act model)
                                       (execute-slash-command model setting.act ""))
       (when notice (show-status model notice))))))

;;; --- the keys -----------------------------------------------------------------

(defun settings-search (settings query)
  "Make QUERY what the sheet searches for, from its first result; emptied,
the sheet goes back where it was."
  (when (zerop (length settings.query))
    (setf settings.back settings.focus))
  (setf settings.query query settings.hit 0 settings.pane-top 0 settings.said nil)
  (when (zerop (length query))
    (setf settings.focus settings.back)))

(defun settings-open (settings)
  "Open the chosen section: its first setting chosen."
  (setf settings.focus :pane settings.setting-at 0 settings.pane-top 0 settings.said nil))

(defun settings-choose (settings at &aux (page (nth at settings.sections)))
  "Choose the page AT, at its first setting. A page chosen has been seen:
its badge goes, and Latest changes keeps the release it lists as seen."
  (setf settings.section-at at settings.setting-at 0 settings.pane-top 0 settings.said nil)
  (when (and page page.badge)
    (setf page.badge nil)
    (when (equal page.key "latest") (see-changes page.applies))))

(defun settings-step-section (settings delta)
  "Choose the section DELTA away, wrapping, at its first setting."
  (settings-choose settings (mod (+ settings.section-at delta) (length settings.sections))))

(defun settings-move (settings delta)
  "Move the choice DELTA rows in the list the keys are on, stopping at its ends."
  (setf settings.said nil)
  (flet ((moved (at count) (max 0 (min (1- count) (+ at delta)))))
    (cond ((plusp (length settings.query))
           (setf settings.hit (moved settings.hit (length (settings-results settings)))))
          ((eq settings.focus :rail)
           (let ((at (moved settings.section-at (length settings.sections))))
             (unless (= at settings.section-at)
               (settings-choose settings at))))
          (t
           (setf settings.setting-at
                 (moved settings.setting-at
                        (length (settings-section-rows (nth settings.section-at settings.sections)))))))))

(defun settings-sideways (settings delta)
  "On the two columns of sections, move to the nearest in the column DELTA over."
  (when-let (here (find settings.section-at settings.columns :key #'first))
    (when-let (best (alexandria:extremum (remove (+ (second here) delta) settings.columns
                                                 :key #'second :test-not #'=)
                                         #'< :key (lambda (spot) (abs (- (third spot) (third here))))))
      (settings-choose settings (first best)))))

(defun settings-edit-key (model settings key ctrl alt)
  "A key while a field is open: Enter writes it, Esc leaves it as it was,
the rest edits it."
  (case key
    (:enter (settings-edit-submit model settings))
    (:escape (setf settings.edit nil settings.said nil))
    (t (when-let (text (edit-text-field (cdr settings.edit) key :ctrl ctrl :alt alt))
         (setf (cdr settings.edit) text)))))

(defun settings-ids-key (model settings key)
  "A key while ids are marked: the arrows move, Space marks, Enter writes
the marks, Esc leaves them as they were."
  (destructuring-bind (setting choices marks cursor) settings.ids
    (case key
      ((:up :down)
       (setf (fourth settings.ids)
             (max 0 (min (1- (length choices)) (+ cursor (if (eq key :up) -1 1))))))
      (:escape (setf settings.ids nil))
      (:enter
       (setf settings.ids nil)
       (settings-commit model settings setting
                        (coerce (loop for (id) in choices
                                      when (member id marks :test #'equal) collect id)
                                'vector)))
      (t (when (and (eql key #\Space) choices)
           (let ((id (car (nth cursor choices))))
             (setf (third settings.ids)
                   (if (member id marks :test #'equal)
                       (remove id marks :test #'equal)
                       (append marks (list id))))))))))

(defun settings-key (model settings key &key ctrl alt shift)
  "Every key the shell's chords leave the open sheet."
  (let ((setting (settings-current settings))
        (stacked settings.stacked))
    (cond
      (settings.edit (settings-edit-key model settings key ctrl alt))
      (settings.ids (settings-ids-key model settings key))
      ;; Asked to reset: Enter does, Esc keeps every setting as it is.
      ((and (settings-armed settings setting) (member key '(:enter :escape)))
       (if (eq key :enter)
           (settings-reset-all model settings setting (settings-armed settings setting))
           (setf settings.said nil settings.armed nil)))
      ;; Typing searches every setting; Space is a key of its own unless a
      ;; query is going and the result under it takes no Space.
      ((sheet-type-query settings.query key ctrl alt (lambda (query) (settings-search settings query))
                         :space (and (plusp (length settings.query))
                                     (not (member (and setting setting.kind) '(:toggle :choice))))))
      ((eq key :escape)
       (cond ((plusp (length settings.query)) (settings-search settings ""))
             ((eq settings.focus :pane) (setf settings.focus :rail settings.said nil))
             (t (dismiss-dialog model nil))))
      ((or (member key '(:up :down))
           (and ctrl (characterp key) (member key '(#\p #\n) :test #'char-equal)))
       (settings-move settings (if (or (eq key :up) (eql key #\p) (eql key #\P)) -1 1)))
      ((member key '(:page-up :page-down))
       (settings-move settings (if (eq key :page-up) -10 10)))
      ((eq key :tab)
       (cond ((plusp (length settings.query)))
             ((and stacked (eq settings.focus :pane)) (settings-step-section settings (if shift -1 1)))
             ((eq settings.focus :pane) (setf settings.focus :rail settings.said nil))
             (t (settings-open settings))))
      ;; A query that found nothing has nothing for the keys to act on.
      ((and (null setting) (plusp (length settings.query))))
      ;; On the rail: the arrows across move between the two columns of a
      ;; narrow window's sections, and into the open section on a wide one.
      ((and (null setting) (eq settings.focus :rail) (member key '(:left :right)))
       (if stacked
           (settings-sideways settings (if (eq key :left) -1 1))
           (when (eq key :right) (settings-open settings))))
      ((and (null setting) (eq settings.focus :rail) (member key '(:enter #\Space)))
       (settings-open settings))
      ((null setting))
      ;; Left is always the way back to the sections, whichever setting is
      ;; chosen: the rail stands to its left. Right steps the value on, round
      ;; past its last option; Shift with either arrow steps it either way,
      ;; stopping at its ends.
      ((and (eq key :left) (not shift))
       (when (zerop (length settings.query))
         (setf settings.focus :rail settings.said nil)))
      ((member key '(:left :right))
       (when (member setting.kind '(:toggle :choice :number))
         (settings-nudge model settings setting (if (eq key :left) -1 1)
                         :wrap (and (eq key :right) (not shift)))))
      ((eql key #\Space)
       (when (member setting.kind '(:toggle :choice))
         (settings-nudge model settings setting 1 :wrap t)))
      ((eq key :enter) (settings-act model settings setting))
      ((or (eq key :delete) (and ctrl (characterp key) (char-equal key #\x)))
       (settings-reset model settings setting)))))

(defun settings-paste (model settings text)
  "A paste lands where typing would: in the open field, else in the query."
  (declare (ignore model))
  (if settings.edit
      (setf (cdr settings.edit) (concatenate 'string (cdr settings.edit) text))
      (settings-search settings (clip (concatenate 'string settings.query text)
                                      *max-dialog-search-chars* :ellipsis ""))))

(defun settings-wheel (model settings delta x y)
  "A notch over the sheet moves the choice the way the arrows do, in the
list under the pointer: over the rail the page, over the pane its settings."
  (declare (ignore model y))
  (unless (or settings.edit settings.ids)
    (when (and settings.layout (not settings.stacked) (zerop (length settings.query)))
      (destructuring-bind (rows left top width) settings.layout
        (declare (ignore rows top))
        (setf settings.focus (if (< (- x left) (rail-width width)) :rail :pane))))
    (settings-move settings delta)))

;;; --- painting -----------------------------------------------------------------

(defun quiet-text-p (text)
  "Whether TEXT says a setting holds nothing, which paints quiet."
  (member text '("Not set" "None" "none" "Reading…" "None for this model" "Nothing changed"
                 "The session's own")
          :test #'equal))

(defun options-width (setting)
  "Columns SETTING's options take side by side, 0 for a setting with none."
  (if (member setting.kind '(:toggle :choice))
      (loop for (nil . label) in setting.options sum (+ 2 (text-display-width label)))
      0))

(defun settings-keys (settings &aux (setting (settings-current settings)) (stacked settings.stacked))
  "The keys that act where the operator is, in the order the key row names them."
  (let ((act (and setting
                  (case setting.kind
                    ((:toggle :choice) '(("→" "change")))
                    (:number '(("→" "up") ("enter" "type")))
                    (:action '(("enter" "open")))
                    (:check '(("enter" "check")))
                    (:ids '(("enter" "choose")))
                    (:reset '(("enter" "review")))
                    ((:note :change) '())
                    (t '(("enter" "edit"))))))
        (reset (and (settings-resettable-p setting) '(("del" "default")))))
    (cond (settings.edit '(("enter" "save") ("esc" "cancel") ("ctrl+u" "clear")))
          (settings.ids '(("space" "mark") ("enter" "save") ("esc" "cancel")))
          ((settings-armed settings setting) '(("enter" "reset") ("esc" "keep")))
          ((plusp (length settings.query)) (append act '(("↑↓" "move") ("esc" "clear")) reset))
          ((eq settings.focus :rail)
           (if stacked
               '(("↑↓" "move") ("enter" "open") ("type" "search") ("esc" "close"))
               '(("↑↓" "section") ("→" "open") ("type" "search") ("esc" "close"))))
          (stacked (append act '(("↑↓" "move") ("←" "sections") ("tab" "next")) reset))
          (t (append act '(("↑↓" "setting") ("←" "sections")) reset)))))

(defun focus-setting-action (index)
  "A click's action that chooses the setting at INDEX: the result under a
query, the row of the open section otherwise."
  (lambda-ignoring (model settings)
    (setf settings.said nil)
    (if (plusp (length settings.query))
        (setf settings.hit index)
        (setf settings.setting-at index settings.focus :pane))))

(defun hover-setting-action (index)
  "The pointer's action resting on the setting at INDEX: choose it as a click
does, unless it is chosen already or a list is open. => whether it moved."
  (lambda (model settings)
    (unless (or settings.ids
                (if (plusp (length settings.query))
                    (eql index settings.hit)
                    (and (eq settings.focus :pane) (eql index settings.setting-at))))
      (funcall (focus-setting-action index) model settings)
      t)))

(defun settings-hover (model settings x y)
  "The pointer resting on a row chooses it, the way the arrows would: a page
in the rail, a setting in the pane, an id in an open list. An open field
keeps the choice where it is."
  (unless settings.edit
    (sheet-hover model settings x y)))

(defun pick-option-action (setting value index)
  "A click's action that chooses SETTING at INDEX and lands VALUE as its."
  (lambda (model settings)
    (funcall (focus-setting-action index) model settings)
    (unless (equal value setting.value)
      (if (and (null value) setting.field (eq setting.kind :choice))
          (settings-commit model settings setting nil :remove t)
          (settings-commit model settings setting value)))))

(defun paint-chips (settings canvas row x width setting focused ground index)
  "SETTING's options side by side from X on ROW, the one it holds filled --
or, when WIDTH cannot hold them, the one it holds between arrows and where
it stands among them. => the column after them."
  (let* ((options setting.options)
         (active (position setting.value options :key #'car :test #'equal))
         (none (eq (effective-color-profile) :none)))
    (if (<= (options-width setting) width)
        (let ((at x))
          (loop for (value . label) in options
                for position from 0
                for on = (eql position active)
                do (canvas-put canvas at row (if (and on none) (format nil "[~a]" label) (format nil " ~a " label))
                               (cond ((not on) (sheet-ink :muted :ground ground))
                                     (focused (sheet-ink :sheet-chip-on))
                                     (setting.set (sheet-ink :sheet-chip-set))
                                     (t (sheet-ink :sheet-chip))))
                   (sheet-hit settings row at (+ at 2 (text-display-width label))
                                 (pick-option-action setting value index))
                   (incf at (+ 2 (text-display-width label))))
          at)
        (let* ((label (or (cdr (nth (or active 0) options)) "Not set"))
               (count (format nil "~d/~d" (1+ (or active 0)) (length options)))
               (arrow (sheet-ink (if focused :dialog-accent :muted) :ground ground))
               (at (canvas-put canvas x row "‹" arrow)))
          (sheet-hit settings row x (+ x 2)
                        (pick-option-action setting (car (nth (max 0 (1- (or active 0))) options)) index))
          (setf at (canvas-put canvas (1+ at) row (clip-text-to-width label (max 3 (- width 6 (length count))))
                               (sheet-ink (cond (focused :foreground) (setting.set :dialog-accent) (t :system))
                                             :ground ground :bold focused)))
          (canvas-put canvas (1+ at) row "›" arrow)
          (sheet-hit settings row (1+ at) (+ at 3)
                        (pick-option-action setting (car (nth (min (1- (length options)) (1+ (or active 0)))
                                                              options))
                                            index))
          (when (<= (+ at 4 (length count)) (+ x width))
            (canvas-put canvas (+ at 4) row count (sheet-ink :muted :ground ground)))
          (+ at 4 (length count))))))

(defun paint-field (settings canvas row x width ground)
  "The open field from X on ROW, WIDTH columns at most: what it holds -- a
secret as one * a character -- on the field's ground. => the caret's cell."
  (declare (ignore ground))
  (let* ((setting (car settings.edit))
         (text (if (member setting.kind '(:secret :paste))
                   (make-string (length (cdr settings.edit)) :initial-element #\*)
                   (cdr settings.edit)))
         (box (min width 40))
         (field (sheet-ground :composer)))
    (canvas-fill canvas x row box 1 (sheet-ink nil :ground field))
    (cons (canvas-put canvas (1+ x) row (clip-tail text (- box 3)) (sheet-ink :composer :ground field))
          row)))

(defun paint-control (settings canvas row x width setting focused ground index)
  "SETTING's value from X on ROW, WIDTH columns: its options, its number
between arrows once chosen, its field while open, else its words. => the
caret's cell while its field is open, else NIL."
  (cond
    ((and settings.edit (eq (car settings.edit) setting))
     (paint-field settings canvas row x width ground))
    ((member setting.kind '(:toggle :choice))
     (paint-chips settings canvas row x width setting focused ground index)
     nil)
    (t
     (let* ((text setting.text)
            (ink (sheet-ink (cond ((quiet-text-p text) :muted) (setting.set :foreground) (t :system))
                               :ground ground))
            (hint (and focused
                       (case setting.kind
                         (:number nil)
                         (:action (if (stringp setting.act)
                                      (format nil "enter opens /~a" setting.act)
                                      "enter picks a model"))
                         (:check "enter asks now")
                         (:note nil)
                         (:ids "enter chooses")
                         (:reset "enter reviews")
                         ((:secret :paste) "enter pastes")
                         (t "enter edits")))))
       (if (and focused (eq setting.kind :number))
           (let ((at (canvas-put canvas x row "‹ " (sheet-ink :muted :ground ground))))
             (sheet-hit settings row x (+ x 2)
                           (lambda (model settings) (settings-nudge model settings setting -1)))
             (setf at (canvas-put canvas at row (clip-text-to-width text (- width 4))
                                  (sheet-ink :foreground :ground ground :bold t)))
             (canvas-put canvas at row " ›" (sheet-ink :dialog-accent :ground ground))
             (sheet-hit settings row at (+ at 2)
                           (lambda (model settings) (settings-nudge model settings setting 1))))
           (let ((at (canvas-put canvas x row (clip-text-to-width text width) ink)))
             (when (and hint (<= (+ at 2 (length hint)) (+ x width)))
               (canvas-put canvas (+ at 2) row hint (sheet-ink :muted :ground ground)))))
       nil))))

(defun focus-extras (settings setting width doc-lines)
  "The lines a chosen SETTING carries under it, WIDTH columns wide: the ids
being marked, what was said -- whole -- and under a reset's ask the rows it
gives back, else at most DOC-LINES of its description, an … where it was
cut."
  (let ((said (settings-said settings setting)))
    (cond ((and settings.ids (eq (first settings.ids) setting))
           (loop for choice in (second settings.ids)
                 for index from 0
                 collect (list :pick setting choice index)))
          (said
           (append (loop for line in (wrapped-lines (first said) width)
                         collect (list :said line (second said)))
                   (loop for row in (settings-armed settings setting)
                         collect (list :doc (clip-text-to-width
                                             (format nil "~a  ~a" (reset-label row) (setting-text (cdr row)))
                                             width)))))
          (t
           (let ((lines (wrapped-lines setting.doc width)))
             (when (> (length lines) doc-lines)
               (setf lines (subseq lines 0 doc-lines))
               (setf (car (last lines))
                     (concatenate 'string (take-display-prefix (car (last lines)) (1- width)) "…")))
             (loop for line in lines
                   unless (zerop (length line)) collect (list :doc line)))))))

(defun setting-lines (settings entries style width label-width control-width doc-lines)
  "The lines ENTRIES paint in STYLE -- :INLINE, a name column and the
options beside it on every row, or :QUIET, a name and its value at the right
edge, the chosen one's options under it when they do not fit beside -- in
a pane WIDTH columns wide, the chosen one's description at most DOC-LINES
lines. ENTRIES are (SETTING FOCUSED INDEX), (:HEAD PART) over a page's
results, or (:PART-HEAD PART APPLIES) over the settings of one section a
page gathers, a blank line before it. A change of a release is one line,
the whole of it once it is chosen. => (values LINES FOCUS-LINE)."
  (let ((lines '()) (focus-line 0))
    (dolist (entry entries)
      (case (first entry)
        (:head (push entry lines))
        (:part-head
         (when (and lines (not (eq :gap (first (first lines)))))
           (push (list :gap) lines))
         (push entry lines))
        (t
          (destructuring-bind (setting focused index) entry
            (when focused (setf focus-line (length lines)))
            (let ((need (options-width setting)))
              (cond
                ((eq setting.kind :change)
                 (let ((said (if focused
                                 (wrapped-lines setting.label (- width 13))
                                 (list (clip-text-to-width setting.label (- width 13))))))
                   (push (list :change setting focused index (first said)) lines)
                   (dolist (line (rest said))
                     (push (list :doc line 13 focused) lines))))
                ((eq style :inline)
                 (let ((stack (and (> need control-width) (<= need (- width 2)))))
                   (push (list :field setting focused index stack) lines)
                   (when stack (push (list :control setting focused index) lines))
                   (when focused
                     (dolist (line (focus-extras settings setting (if stack (- width 2) control-width)
                                                 doc-lines))
                       (push (append line (list (if stack 2 (+ 3 label-width)) nil)) lines)))))
                (t
                 (let ((beside (or (not focused) (zerop need)
                                   (<= (+ (text-display-width setting.label) 4 need) (- width 2)))))
                   (push (list :row setting focused index beside) lines)
                   (when focused
                     (cond ((and settings.edit (eq (car settings.edit) setting))
                            (push (list :control setting t index) lines))
                           ((not beside) (push (list :control setting t index) lines)))
                     (dolist (line (focus-extras settings setting (- width 2) doc-lines))
                       (push (append line (list 2 t)) lines)))))))))))
    (values (nreverse lines) focus-line)))

(defun page-entries (page focus)
  "PAGE's settings as SETTING-LINES entries, FOCUS the index of the chosen
one or NIL: each section the page gathers under its head, (:PART-HEAD PART
APPLIES), APPLIES when a change there applies otherwise than on the page."
  (let ((entries '()) (last page))
    (loop for setting in page.rows
          for index from 0
          for part = setting.part
          do (unless (eq part last)
               (setf last part)
               (when (and part.head (not (eq part page)))
                 (push (list :part-head part (and (not (equal part.applies page.applies)) part.applies))
                       entries)))
             (push (list setting (eql index focus) index) entries))
    (nreverse entries)))

(defun applies-ink (applies ground)
  "When a change applies, as a section's head says it: a restart in its own tone."
  (sheet-ink (if (uiop:string-prefix-p "restart" applies) :sheet-restart :muted) :ground ground))

(defun paint-lines (settings canvas lines focus-line top rows px pw label-width ground)
  "LINES down ROWS rows from TOP in the pane at PX, PW wide, scrolled to keep
the chosen setting and what it carries in view. => the open field's caret,
or NIL."
  (let* ((last focus-line)
         (cursor nil)
         (lift (sheet-ground :sheet-focus))
         (none (eq (effective-color-profile) :none)))
    ;; The chosen setting's own lines follow it; marking ids, the cursor is
    ;; the line kept in view.
    (loop while (and (nth (1+ last) lines)
                     (member (first (nth (1+ last) lines)) '(:control :doc :said :pick)))
          do (incf last))
    (when settings.ids
      (setf last (min last (+ focus-line 1 (fourth settings.ids)))))
    (let ((top-line (max 0 (min settings.pane-top (1- focus-line)))))
      (setf top-line (min focus-line (max top-line (- last rows -1)))
            settings.pane-top top-line)
      (loop for i from 0 below rows
            for line = (nth (+ top-line i) lines)
            for r = (+ top i)
            while line
            do (let* ((kind (first line))
                      (block (case kind
                               ((:field :row :control :change) (third line))
                               ((:doc :said :pick) (car (last line)))))
                      (ground (if block lift ground)))
                 (when block
                   (canvas-fill canvas (- px 2) r (+ pw 4) 1 (sheet-ink nil :ground lift))
                   (when (and none (member kind '(:field :row :change)))
                     (canvas-put canvas (- px 2) r "›" nil)))
                 (case kind
                   (:title
                    (let* ((part (second line))
                           (applies part.applies)
                           (at (canvas-put canvas px r (clip-text-to-width part.label pw)
                                           (sheet-ink :title :ground ground :bold t))))
                      (when (and applies (<= (+ at 2 (text-display-width applies)) (+ px pw)))
                        (canvas-put canvas (- (+ px pw) (text-display-width applies)) r applies
                                    (applies-ink applies ground)))))
                   (:part-head
                    (destructuring-bind (part applies) (rest line)
                      (let ((at (canvas-put canvas px r (clip-text-to-width (string-upcase (or part.head part.label)) pw)
                                            (sheet-ink :muted :ground ground))))
                        (when (and applies (<= (+ at 2 (text-display-width applies)) (+ px pw)))
                          (canvas-put canvas (- (+ px pw) (text-display-width applies)) r applies
                                      (applies-ink applies ground))))))
                   (:change
                    (destructuring-bind (setting focused index text) (rest line)
                      (canvas-put canvas (+ px 2) r setting.text (sheet-ink :muted :ground ground))
                      (canvas-put canvas (+ px 13) r text
                                  (sheet-ink (if focused :dialog-accent :system) :ground ground))
                      (sheet-hit settings r (- px 2) (+ px pw 2) (focus-setting-action index)
                                 :hover (hover-setting-action index))))
                   ((:guide :note) (canvas-put canvas px r (clip-text-to-width (second line) pw)
                                               (sheet-ink :muted :ground ground)))
                   (:needs (canvas-put canvas px r (clip-text-to-width (second line) pw)
                                       (sheet-ink :sheet-restart :ground ground)))
                   (:failed (canvas-put canvas px r (second line) (sheet-ink :danger :ground ground)))
                   (:head (canvas-put canvas px r (clip-text-to-width (settings-section-label (second line)) pw)
                                      (sheet-ink :muted :ground ground)))
                   (:doc (canvas-put canvas (+ px (third line)) r (second line)
                                     (sheet-ink :muted :ground ground)))
                   (:said (canvas-put canvas (+ px (fourth line)) r (second line)
                                      (sheet-ink (third line) :ground ground)))
                   (:pick
                    (destructuring-bind (setting (id . label) index x block) (rest line)
                      (declare (ignore setting block))
                      (let* ((marked (member id (third settings.ids) :test #'equal))
                             (here (= index (fourth settings.ids)))
                             (at (canvas-put canvas (+ px x) r (if marked "[x] " "[ ] ")
                                             (sheet-ink (if marked :dialog-accent :muted) :ground ground))))
                        (setf at (canvas-put canvas at r (clip-text-to-width label (- (+ px pw) at 2))
                                             (sheet-ink (if here :dialog-accent :system) :ground ground
                                                                                            :bold here)))
                        (unless (equal id label)
                          (canvas-put canvas (+ at 2) r id (sheet-ink :muted :ground ground) (+ px pw)))
                        (sheet-hit settings r (- px 2) (+ px pw 2)
                                      (lambda-ignoring (model settings)
                                        (setf (fourth settings.ids) index)
                                        (settings-ids-key nil settings #\Space))
                                      :hover (lambda-ignoring (model settings)
                                               (unless (eql index (fourth settings.ids))
                                                 (setf (fourth settings.ids) index)
                                                 t))))))
                   (:field
                    (destructuring-bind (setting focused index stack) (rest line)
                      (when setting.set (canvas-put canvas px r "•" (sheet-ink :dialog-accent :ground ground)))
                      (canvas-put canvas (+ px 2) r
                                  (clip-text-to-width setting.label (if stack (- pw 2) label-width))
                                  (sheet-ink (if focused :dialog-accent :system) :ground ground))
                      (sheet-hit settings r (- px 2) (+ px 2 label-width 1) (focus-setting-action index))
                      (sheet-hit settings r (- px 2) (+ px pw 2) nil :hover (hover-setting-action index))
                      (unless stack
                        (let ((x (+ px 2 label-width 1)))
                          (when-let (caret (paint-control settings canvas r x (- (+ px pw) x) setting
                                                          focused ground index))
                            (setf cursor caret))))))
                   (:control
                    (destructuring-bind (setting focused index) (rest line)
                      (when-let (caret (paint-control settings canvas r (+ px 2) (- pw 2) setting
                                                      focused ground index))
                        (setf cursor caret))))
                   (:row
                    (destructuring-bind (setting focused index beside) (rest line)
                      (let ((end (+ px pw)) (used 0))
                        (when setting.set (canvas-put canvas px r "•" (sheet-ink :dialog-accent :ground ground)))
                        (cond ((and focused (not beside)))
                              ((and focused settings.edit (eq (car settings.edit) setting)))
                              ((and focused (plusp (options-width setting)))
                               (setf used (options-width setting))
                               (paint-chips settings canvas r (- end used) used setting t ground index))
                              ((and focused (eq setting.kind :number))
                               (setf used (+ 4 (text-display-width setting.text)))
                               (paint-control settings canvas r (- end used) used setting t ground index))
                              (t
                               (let ((shown (clip-tail setting.text (max 8 (floor pw 2)))))
                                 (setf used (text-display-width shown))
                                 (canvas-put canvas (- end used) r shown
                                             (sheet-ink (cond ((quiet-text-p setting.text) :muted)
                                                                 ((or focused setting.set) :foreground)
                                                                 (t :system))
                                                           :ground ground)))))
                        (canvas-put canvas (+ px 2) r
                                    (clip-text-to-width setting.label (max 1 (- pw 2 used 2)))
                                    (sheet-ink (if focused :dialog-accent :system) :ground ground))
                        (sheet-hit settings r (- px 2) (max (+ px 2) (- end used))
                                      (focus-setting-action index))
                        (sheet-hit settings r (- px 2) (+ px pw 2) nil
                                   :hover (hover-setting-action index))))))))
      (paint-scroll-marks canvas (+ px pw 1) top (+ top rows -1) top-line rows (length lines) ground))
    cursor))

(defun result-entries (settings)
  "The results of the query as SETTING-LINES entries, each section's under
its name."
  (let ((entries '()) (last nil))
    (loop for (part . setting) in (settings-results settings)
          for index from 0
          do (unless (eq part last)
               (push (list :head part) entries)
               (setf last part))
             (push (list setting (= index settings.hit) index) entries))
    (nreverse entries)))

(defun rail-entries (settings)
  "The rail's lines: each group's name over its pages, a blank line before
it, General's none -- as (:GAP), (:HEAD TEXT) and (:SECTION INDEX)."
  (let ((entries '()) (group nil))
    (loop for part in settings.sections
          for index from 0
          do (unless (equal group part.group)
               (setf group part.group)
               (unless (equal group "General")
                 (when entries (push (list :gap) entries))
                 (push (list :head (string-upcase group)) entries)))
             (push (list :section index) entries))
    (nreverse entries)))

(defun section-mark (canvas x row part ground on)
  "What stands at the end of PART's name ending at column X on ROW: its
badge until it is chosen, else a dot while any of its settings is set."
  (cond ((and part.badge (not on))
         (canvas-put canvas (- x (text-display-width part.badge) -1) row part.badge
                     (sheet-ink :dialog-accent :ground ground)))
        ((section-set-p part)
         (canvas-put canvas x row "•" (sheet-ink :dialog-accent :ground ground)))))

(defun section-set-p (part)
  "Whether any setting of PART is set rather than at its default."
  (some #'setting-set part.rows))

(defun open-section-action (index)
  "A click's action on the section at INDEX: choose it, or open it once chosen."
  (lambda-ignoring (model settings)
    (if (and (= index settings.section-at) settings.stacked)
        (settings-open settings)
        (progn (settings-choose settings index)
               (setf settings.query "" settings.focus (if settings.stacked :rail :pane))))))

(defun hover-section-action (index)
  "The pointer's action resting on the section at INDEX: choose it, the keys
on the rail, as the arrows there would; under a query or an open list,
nothing. => whether it moved."
  (lambda-ignoring (model settings)
    (unless (or settings.ids (plusp (length settings.query))
                (and (= index settings.section-at) (eq settings.focus :rail)))
      (unless (= index settings.section-at) (settings-choose settings index))
      (setf settings.focus :rail)
      t)))

(defun paint-rail (settings canvas top rows width rail pane)
  "The rail's sections down ROWS rows from TOP, WIDTH columns, scrolled to
keep the chosen one in view: the chosen one on the pane's ground, so it
reads as the tab the pane hangs from."
  (let* ((entries (rail-entries settings))
         (item (position (list :section settings.section-at) entries :test #'equal))
         (offset (max 0 (min settings.rail-top (1- item)) (- item rows -1))))
    (setf offset (min offset (max 0 (- (length entries) rows)))
          settings.rail-top offset)
    (loop for i from 0 below rows
          for entry = (nth (+ offset i) entries)
          for r = (+ top i)
          while entry
          do (case (first entry)
               (:gap)
               (:head (canvas-put canvas 3 r (second entry) (sheet-ink :muted :ground rail)))
               (t
                 (let* ((index (second entry))
                        (part (nth index settings.sections))
                        (on (and (= index settings.section-at) (zerop (length settings.query))))
                        (ground (if on pane rail)))
                   (when on (canvas-fill canvas 0 r width 1 (sheet-ink nil :ground pane)))
                   (canvas-put canvas 3 r (clip-text-to-width part.label (- width 7))
                               (sheet-ink (cond ((not on) :system)
                                                   ((eq settings.focus :rail) :dialog-accent)
                                                   (t :foreground))
                                             :ground ground :bold on))
                   (section-mark canvas (- width 3) r part ground on)
                   (sheet-hit settings r 0 width (open-section-action index)
                              :hover (hover-section-action index))))))
    (paint-scroll-marks canvas (- width 2) top (+ top rows -1) offset rows (length entries) rail)))

(defun settings-failed-lines (settings width)
  "Why the last read failed, whole, broken to WIDTH columns as (:FAILED
LINE) lines; NIL when it did not."
  (when settings.failed
    (loop for line in (wrapped-lines settings.failed width)
          collect (list :failed line))))

(defun pane-head (settings part width guide-lines)
  "The lines over the open page's settings: its name and when a change
applies, why its own read failed, whole, as many lines of its guide as
GUIDE-LINES, what it still needs, then a gap; over a query's results, how
many it found."
  (append (settings-failed-lines settings width)
          (if (plusp (length settings.query))
              (let ((count (length (settings-results settings))))
                (list (list :note (if (plusp count)
                                      (format nil "~d setting~:p match~:[~;es~]" count (= count 1))
                                      (format nil "Nothing matches “~a”" settings.query)))
                      (list :gap)))
              (append (list (list :title part))
                      (and part.failed
                           (loop for line in (wrapped-lines part.failed width)
                                 collect (list :failed line)))
                      (loop for line in (wrapped-lines part.guide width)
                            for count from 0
                            while (< count guide-lines)
                            collect (list :guide line))
                      ;; The route says it in the operator's words, `still
                      ;; needed: ...' its own.
                      (when part.needs (list (list :needs (string-capitalize part.needs :end 1))))
                      (list (list :gap))))))

(defun rail-width (width)
  "The columns the rail takes of a wide sheet WIDTH columns across."
  (if (>= width 104) 26 22))

(defun paint-wide (settings canvas edges)
  "The wide sheet: the rail of sections beside the open one, each setting's
options on its row while the pane holds them. => the caret's cell, or NIL."
  (with-wide-sheet (canvas edges (rail-width width))
    (let* ((tight (< inner 20))
           (hint-row (+ top inner (if tight -1 -2)))
           (title-row (+ top (if tight 0 1)))
           (content-top (+ title-row 2))
           (rows (- hint-row content-top (if tight 0 1)))
           (style (if (>= pw 70) :inline :quiet))
           (label-width (max 10 (min 25 (floor (* pw 36) 100))))
           (control-width (- pw 2 label-width 1))
           (part (nth settings.section-at settings.sections))
           (cursor nil))
      (canvas-put canvas 3 title-row "Settings" (sheet-ink :title :ground rail :bold t))
      (setf cursor (paint-search-field canvas px title-row pw settings.query "Search every setting" field))
      (paint-rail settings canvas content-top (- (+ top inner) content-top 1) rail-width rail pane)
      (let ((head (pane-head settings part pw (cond ((>= rows 18) 2) ((>= rows 12) 1) (t 0)))))
        (multiple-value-bind (lines focus-line)
            (setting-lines settings
                           (if (plusp (length settings.query))
                               (result-entries settings)
                               (page-entries part (and (eq settings.focus :pane) settings.setting-at)))
                           style pw label-width control-width (if (>= rows 18) 3 2))
          (when (eq settings.focus :rail) (setf settings.pane-top 0))
          (when-let (caret (paint-lines settings canvas (append head lines) (+ (length head) focus-line)
                                        content-top rows px pw label-width pane))
            (setf cursor caret))))
      (paint-keys settings canvas hint-row px (+ px pw) (settings-keys settings) settings.flash pane)
      cursor)))

(defun section-columns (entries rows)
  "ENTRIES, RAIL-ENTRIES' lines, laid in columns ROWS rows deep, each group
whole in one and a blank line between two a column holds: ((INDEX COLUMN
ROW) ...), INDEX into ENTRIES; NIL when a group is deeper than ROWS."
  (let ((groups '()) (group '()))
    (loop for entry in entries
          for i from 0
          do (if (eq (first entry) :gap)
                 (progn (push (nreverse group) groups) (setf group '()))
                 (push i group)))
    (push (nreverse group) groups)
    (let ((column 0) (row 0) (spots '()))
      (dolist (group (nreverse groups) (nreverse spots))
        (when (> (length group) rows) (return nil))
        (when (and (plusp row) (> (+ row 1 (length group)) rows))
          (incf column)
          (setf row 0))
        (when (plusp row) (incf row))
        (dolist (i group)
          (push (list i column row) spots)
          (incf row))))))

(defun paint-sections (settings canvas top rows px pw ground)
  "The pages down ROWS rows from TOP: in as many columns as their groups
take whole, while a column keeps 16 columns, else in one that scrolls to
keep the chosen one in view."
  (let* ((entries (rail-entries settings))
         (spots (section-columns entries rows))
         (columns (if spots (1+ (reduce #'max spots :key #'second :initial-value 0)) 1))
         (column-width (floor (- pw (* 4 (1- columns))) columns))
         (scrolls nil))
    (when (or (null spots) (< column-width 16))
      (let ((item (position (list :section settings.section-at) entries :test #'equal)))
        (setf columns 1 column-width pw scrolls t
              settings.rail-top (min (max 0 (min settings.rail-top (1- item)) (- item rows -1))
                                     (max 0 (- (length entries) rows)))
              spots (loop for i from settings.rail-top below (min (length entries) (+ settings.rail-top rows))
                          collect (list i 0 (- i settings.rail-top))))))
    (setf settings.columns '())
    (loop for (i column row) in spots
          for entry = (nth i entries)
          for x = (+ px (* column (+ column-width 4)))
          for r = (+ top row)
          do (case (first entry)
               (:gap)
               (:head (canvas-put canvas x r (second entry) (sheet-ink :muted :ground ground)))
               (t
                (let* ((index (second entry))
                       (part (nth index settings.sections))
                       (on (= index settings.section-at))
                       (ground (if on (sheet-ground :sheet-focus) ground)))
                  (when (> columns 1) (push (list index column row) settings.columns))
                  (when on
                    (canvas-fill canvas (- x 2) r (+ column-width 4) 1 (sheet-ink nil :ground ground))
                    (when (eq (effective-color-profile) :none) (canvas-put canvas (- x 2) r "›" nil)))
                  (canvas-put canvas x r (clip-text-to-width part.label (- column-width 3))
                              (sheet-ink (if on :dialog-accent :system) :ground ground :bold on))
                  (section-mark canvas (+ x column-width -1) r part ground on)
                  (sheet-hit settings r (- x 2) (+ x column-width 2) (open-section-action index)
                             :hover (hover-section-action index))))))
    (when scrolls
      (paint-scroll-marks canvas (+ px pw 1) top (+ top rows -1) settings.rail-top rows (length entries) ground))))

(defun paint-stacked (settings canvas edges)
  "The narrow sheet: the sections, or the open one, or a query's results, one
view at a time, each setting one line until it is chosen. => the caret's
cell, or NIL."
  (with-stacked-sheet (canvas edges)
    (let* ((list-top (+ header-row 1 (if (>= inner 8) 1 0)))
           (rows (- hint-row (if (>= inner 9) 1 0) list-top))
           (part (nth settings.section-at settings.sections))
           (cursor nil))
      (flet ((right (text ink)
               (when (< (+ (text-display-width text) 2) pw)
                 (canvas-put canvas (- (+ px pw) (text-display-width text)) header-row text ink)))
             (lines (entries)
               (multiple-value-bind (lines focus-line)
                   (setting-lines settings entries :quiet pw 0 0 (if (>= rows 10) 2 1))
                 (let ((head (settings-failed-lines settings pw)))
                   (when-let (caret (paint-lines settings canvas (append head lines)
                                                 (+ (length head) focus-line)
                                                 list-top rows px pw 0 pane))
                     (setf cursor caret))))))
        (cond
          ((plusp (length settings.query))
           ;; The header is the search line.
           (let ((count (length (settings-results settings))))
             (setf cursor (paint-search-line canvas px header-row pw settings.query pane 22))
             (right (if (plusp count) (format nil "~d match~:[es~;~]" count (= count 1)) "no match")
                    (sheet-ink :muted :ground pane))
             (lines (result-entries settings))))
          ((eq settings.focus :rail)
           (canvas-put canvas px header-row "Settings" (sheet-ink :title :ground pane :bold t))
           (right "type to search" (sheet-ink :muted :ground pane))
           (let ((failed (settings-failed-lines settings pw)))
             (loop for (nil line) in failed
                   for r from list-top
                   do (canvas-put canvas px r line (sheet-ink :danger :ground pane)))
             (paint-sections settings canvas (+ list-top (length failed)) (- rows (length failed))
                             px pw pane)))
          (t
           (let* ((count (length settings.sections))
                  (where (format nil "‹ ~d/~d ›" (1+ settings.section-at) count))
                  (at (canvas-put canvas px header-row "Settings › " (sheet-ink :muted :ground pane))))
             (sheet-hit settings header-row px at
                           (lambda-ignoring (model settings)
                             (setf settings.focus :rail settings.said nil)))
             (canvas-put canvas at header-row
                         (clip-text-to-width part.label (max 1 (- pw 11 (length where) 2)))
                         (sheet-ink :title :ground pane :bold t))
             (when (< (+ (length where) 14) pw)
               (let ((from (- (+ px pw) (length where))))
                 (canvas-put canvas from header-row where (sheet-ink :muted :ground pane))
                 (sheet-hit settings header-row from (+ from 2)
                               (lambda-ignoring (model settings)
                                 (settings-step-section settings -1)))
                 (sheet-hit settings header-row (+ from (length where) -2) (+ from (length where))
                               (lambda-ignoring (model settings)
                                 (settings-step-section settings 1)))))
             (lines (page-entries part settings.setting-at))))))
      (paint-keys settings canvas hint-row px (+ px pw) (settings-keys settings) settings.flash pane)
      cursor)))

(defparameter +settings-sheet-size+ '(108 32)
  "The columns and rows the sheet asks for: what a wide window gives it, the
whole rail standing in it unscrolled.")

(defun settings-paint (model settings width height)
  "The sheet for a WIDTH x HEIGHT frame: wide from 84 columns, the rail beside
the pane; narrower, one view at a time."
  (paint-sheet model settings width height +settings-sheet-size+ #'paint-wide #'paint-stacked))

(defun slash-settings-op (model &optional section)
  "Open /settings: the sheet at once over the shell, General first -- or the
page holding the config SECTION once it is read (/cells' Enter) -- every
section read off the loop."
  (let ((settings (make-settings-sheet :kind :settings :title "Settings"
                                       :paint #'settings-paint :key #'settings-key
                                       :paste #'settings-paste :click #'sheet-click
                                       :wheel #'settings-wheel :hover #'settings-hover
                                       :asked section)))
    (setf (chat-model-dialog model) settings)
    (settings-rebuild model settings)
    (settings-read model settings)
    nil))
