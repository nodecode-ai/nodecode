;;;; slash-test.lisp --- U1 gates: slash catalog, parser, dropdown, dispatch.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. The parser splits /name from args, lowercases the name, and rejects
;;;;      non-slash text.
;;;;   2. Aliases resolve to their catalog entry.
;;;;   3. Suggestions prefix-filter the catalog and close on whitespace.
;;;;   4. /help renders every catalog entry.
;;;;   5. Enter on a slash command dispatches locally: notice set, composer
;;;;      reset, no turn started — even while a turn is in flight.
;;;;   6. Unknown commands keep the composer text and explain themselves.
;;;;   7. The dropdown overlays the transcript band without changing frame
;;;;      geometry; :down moves the selection; Tab completes name + space.
;;;;   8. /themes lists the catalog, reports failed switches, and repaints
;;;;      the transcript band on every palette move.
;;;;   9. /models (local) shows and mutates the provider specials.
;;;;  10. /evict (local) folds closed turns through the durable boundary.
;;;;  11. /context renders occupancy, the window, and the meters.
;;;;  14. /web is `nodecode web' on a shell: WEB-TEXT's answer in the notice
;;;;      lane, and no room is offered it.
;;;;
;;;; Loads after ui-test: reuses MAKE-FRAME-MODEL, MAKE-TEST-SESSION, and
;;;; DO-TURNS from there.

(in-package #:nodecode.test)

(nlk:access (entry nle::slash-command) (think nle::slash-command))

(defun dialog-after (model &rest keys)
  "MODEL's dialog once KEYS are pressed on it, a string among them typed a
character at a time; NIL once it closed."
  (apply #'dialog-keys model (mapcan (lambda (k) (if (stringp k) (coerce k 'list) (list k))) keys))
  model.dialog)

;;; --- claim 1: parser ------------------------------------------------------

(deftest slash-parse-splits-name-and-args ()
  (multiple-value-bind (name args) (nle::parse-slash-input "/themes Dracula")
    (is (equal "themes" name) "name parses without its slash")
    (is (equal "Dracula" args) "args keep their verbatim case"))
  (multiple-value-bind (name args) (nle::parse-slash-input "  /HELP  ")
    (is (equal "help" name) "name lowercases and survives padding")
    (is (equal "" args) "absent args parse as the empty string"))
  (is (null (nle::parse-slash-input "hello /world")))
  (is (null (nle::slash-input-p "hello")) "slash-input-p rejects prose")
  (is (nle::slash-input-p "  /evict") "slash-input-p accepts padded input"))

;;; --- claim 2: aliases -----------------------------------------------------

(deftest slash-alias-resolves-to-entry ()
  (is-let* ((entry (nle::find-slash-command "quit")))
    (entry is "alias quit resolves") (entry.name "exit"))
  (is-let* ((entry (nle::find-slash-command "compact"))) (entry is "alias compact resolves")
    (entry.name "evict") ((member "compact" entry.aliases :test #'equal) is))
  (is (null (nle::find-slash-command "nonesuch")) "unknown names miss"))

;;; --- claim 3: suggestions -------------------------------------------------

(deftest slash-suggestions-prefix-filter ()
  (is (= (length nle::*slash-commands*)
         (length (nle::slash-suggestions "/"))))
  (is-each ((lambda (text) (mapcar #'nle::slash-command-name (nle::slash-suggestions text))))
    ("/th" '("themes" "think") "/th matches themes and think")
    ("/the" '("themes") "/the matches one command")
    ("/qu" '("exit") "/qu matches the exit alias")
    ("/themes " nil "whitespace closes the dropdown")
    ("/zz" nil "no match, no dropdown")
    ("hello" nil "prose never suggests")))

;;; --- claim 4: /help opens the help dialog -----------------------------------

(deftest slash-help-covers-the-catalog ()
  (let* ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "help-catalog"))
         (rows (nle::help-dialog-rows model))
         (labels (mapcar #'nle::picker-row-label rows)))
    (dolist (entry nle::*slash-commands*)
      (is (find (nle::slash-command-label entry) labels :test #'equal)
          (format nil "help lists ~a" entry.name)))
    (dolist (spec nle::*keybind-actions*)
      (is (find (second spec) (mapcar #'nle::picker-row-detail rows) :test #'equal)
          (format nil "help lists the ~a keybind" (first spec))))
    (is (subsetp '("Ctrl+O" "Ctrl+V / Ctrl+Alt+V") labels :test #'equal))))

;;; --- claims 5-6: Enter dispatch through the real update seam ---------------

(deftests slash-enter (session-id in-flight typed checks)
  (with-frame-model (model 80 24 :session-id session-id)
    (when in-flight (setf (nle::chat-model-turn-in-flight model) t))
    (type-string model typed :enter)
    (is (nle::chat-model-dialog model))
    checks)
  (dispatches-without-a-turn "slash-gate" nil "/help"
   (progn (is (equal "" (composer-value model)))
          (is (null (nle::chat-model-turn-in-flight model)))))
  (works-while-a-turn-is-in-flight "slash-inflight" t "/help" nil)
  (runs-the-dropdown-selection "slash-select" nil "/he" nil))

(deftest slash-unknown-keeps-composer-text ()
  (with-frame-model (model 80 24 :session-id "slash-unknown")
    (type-string model "/frobnicate now" :enter)
    (is (search "unknown command /frobnicate"
                (nle::status-said model)))
    (is-shape model (composer-value "/frobnicate now") (.turn-in-flight null))))

;;; --- claim 7: dropdown ----------------------------------------------------

(deftest slash-dropdown-docks-below-the-composer ()
  (with-frame-model (model 80 24 :session-id "slash-dropdown" :typed "/the")
    (let ((rows (nle::compose-frame model)))
      (is (= 24 (length rows)))
      ;; One match on 80x24: head 0-2, band 3-14, gap 15, composer
      ;; 16-18, the entry row 19 plus the panel's padding row 20, gap
      ;; 21, footer 22, final blank 23 (the Zig composer lift: the
      ;; dropdown docks BELOW the box and the band yields its rows).
      (is (eql 2 (search "> /themes" (nth 19 rows))))
      (is (not (search "[name]" (nth 19 rows)))))
    (nle::editor-reset model.editor)
    (is (not (rows-carry-p "/themes" (nle::compose-frame model))))))

(deftest slash-dropdown-names-the-command-hotkey ()
  ;; The `/` palette is where an operator learns a command's key: a command
  ;; that also answers to a chord wears it at the right edge of its row
  ;; (SLASH-COMMAND-KEYBINDS), read through the model's own resolved keymap —
  ;; so an override shows the chord the operator actually has, an unbound
  ;; action shows nothing, and a command with no chord costs the window no
  ;; column at all.
  (with-frame-model (model 80 24 :session-id "slash-hotkeys")
    (flet ((hotkey (command)
             (nle::composer-suggestion-hotkey
              model
              (find command (nle::composer-suggestions-for model)
                    :key #'nle::composer-suggestion-text :test #'equal))))
      (type-string model "/")
      (is (equal "Ctrl+S" (hotkey "/sessions")) "/sessions answers to Ctrl+S")
      (is (equal "?" (hotkey "/help")) "and /help to ?")
      (is (null (hotkey "/clear")) "a command with no chord has no hotkey")
      ;; The key comes from the model's keymap, not from a baked string: an
      ;; override moves it, and unbinding the action takes it away.
      (setf model.keymap
            (nle::resolve-keybinds '((:sessions-dialog . "ctrl+g"))))
      (is (equal "Ctrl+G" (hotkey "/sessions")) "an override repaints the lane")
      (is (equal "?" (hotkey "/help")) "and leaves the chords it did not touch")
      (setf model.keymap
            (nle::resolve-keybinds '((:sessions-dialog . "none"))))
      (is (null (hotkey "/sessions")) "an unbound chord leaves the lane empty")
      ;; The lane follows the query: rows that matched still carry their
      ;; command's chord, and a command outside the window has no lane.
      (nle::editor-reset model.editor)
      (type-string model "/hist")
      (is (equal '("/history")
                 (mapcar #'nle::composer-suggestion-text
                         (nle::composer-suggestions-for model))))
      (is (equal "Ctrl+R" (hotkey "/history")) "the history picker's chord"))
    (nle::editor-reset model.editor)
    (setf model.keymap (nle::resolve-keybinds '()))
    (type-string model "/")
    ;; The composed frame is what the operator reads. The search starts at
    ;; the dropdown's first row: the composer above it paints the selected
    ;; completion as a ghost, so `/help' appears there too — the composer's
    ;; text, not a painted row.
    (is-let* ((rows (nle::compose-frame model))
              (first (row-index-of "> /help" rows
                                   :required "the dropdown's first row"))
              (keyed (row-carrying "/sessions" rows :start first))
              (help-row (row-carrying "/help" rows :start first))
              (plain (row-carrying "/clear" rows :start first))
              (at (search "Ctrl+S" keyed)))
      (at is "/sessions wears its chord in the frame") ((search "?" help-row) = at)
      ((subseq plain at (+ at 6)) string= (make-string 6 :initial-element #\Space)))))

(deftest a-click-on-a-slash-dropdown-row-chooses-it ()
  ;; The dropdown is a menu: a click on a row chooses it as Enter chooses
  ;; the highlighted one, whichever row held the highlight -- /help, clicked
  ;; while the next row is highlighted, opens its own dialog.
  (with-frame-model (model 80 24 :session-id "slash-click")
    (type-string model "/" :down)
    (is (= 1 model.slash-selection))
    (nle::compose-frame model)
    (multiple-value-bind (x y) (frame-cell-of model "/help")
      (is (equal '(:suggestion . 0) (nle::frame-target-at-position model (1+ x) y)) "the row is its own target")
      (click-at model (1+ x) y))
    (is (search "Shortcuts" (nle::picker-dialog-title model.dialog)) "the clicked row's command ran")))

(deftest slash-dropdown-navigates-and-completes ()
  (with-frame-model (model 80 24 :session-id "slash-nav")
    (type-string model "/" :down)
    (is (= 1 model.slash-selection))
    (dolist (key '(:up :up)) (press model key))
    (is (= (1- (length nle::*slash-commands*))
           model.slash-selection))
    (dolist (key '(:down :down :tab)) (press model key))
    (let ((completed (format nil "/~a " (nle::slash-command-name (second nle::*slash-commands*)))))
      (is-shape model (composer-value completed) (nle::slash-active-suggestions null))
      (press model :down)
      (is (equal completed (composer-value model))))))

;;; --- claim 8: /themes -----------------------------------------------------

(deftest slash-themes-picker-and-failure-report ()
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "themes-op")))
    (is (null (nle::slash-themes-op model "")) "bare /themes opens the picker")
    (is-present (dialog (nle::chat-model-dialog model)) "the theme picker is open"
      (is (find "Tungstene" (nle::picker-dialog-rows dialog)
                :key #'nle::picker-row-label :test #'equal)))
    (setf model.dialog nil)
    (is (search "theme switch failed"
                (nle::slash-themes-op model "no-such-theme-exists")))))

(deftest slash-themes-hover-previews-and-esc-restores ()
  ;; The Zig ThemePreviewSession: the picker opens on the engaged theme,
  ;; every highlight the tick delivers (flushed by hand here) hot-loads that
  ;; row's palette without persisting, and Esc restores exactly the
  ;; pre-dialog palette.
  (nlk:bind ((nle::*theme-selection-path* nil) (nle::*palette* nle::*palette*)
             (model (make-instance 'nle:gateway-chat-model :ws nil :session-id "themes-preview"))
             (original nle::*palette*) (catalog (nle::theme-catalog))
             (engaged (nle::configured-theme-name)))
    (nle::slash-themes-op model "")
    (is-present (dialog model.dialog) "the theme picker is open"
      (is (= (or (position engaged catalog :test #'equal) 0)
             dialog.selection))
      (nle::picker-dialog-key model :down)
      (nle::picker-flush-highlight model)
      (let* ((next (nle::picker-row-value (nth dialog.selection dialog.rows))))
        (is (equalp nle::*palette*
                    (nle::palette-from-theme-colors
                     (nle::theme-colors-for-name
                      next :user (nle::read-user-themes))))))
      (is (null (nle::saved-theme-selection)))
      (is (null (dialog-after model :escape)) "Esc dismisses the picker")
      (is (eq original nle::*palette*)))))

(deftest slash-themes-repaints-the-transcript-band (let ((nle:*color-profile* :truecolor)
                                                         (nle:*palette* nle:*palette*)))
  ;; Regression: every /themes gesture moves *PALETTE* outside the render
  ;; path — the picker previews on each highlight, Enter engages, Esc
  ;; restores — while the transcript band alone reads finished SGR strings
  ;; out of the viewport, which only RENDER-STATE refreshes. Header,
  ;; composer, docked panel, and footer all recompose from live tones on the
  ;; next frame, so the shell re-themed AROUND a transcript still painted in
  ;; the outgoing palette until some unrelated event forced a re-projection.
  ;; The tall window keeps the prompt bubble inside the band the docked
  ;; picker leaves it.
  (nle::engage-theme :name "Tungstene")
  (with-frame-model (model 60 44 :session-id "themes-repaint"
                     :paint ((nle::ingest-message-fact "user" "prompt under the theme")))
    (flet ((band-row ()
             ;; The header carries the first prompt as the session title,
             ;; so the band's own copy of it is the later row.
             (find "prompt under the theme" (nle::compose-frame model) :test #'search :from-end t)))
      (let ((tungstene (band-row)))
        (is (search "48;2;59;57;55" tungstene))
        (nle::slash-themes-op model "")
        (nle::picker-dialog-key model :down)
        (nle::picker-flush-highlight model)
        (is (not (nle::projection-cache-live-p model)))
        (is (not (equal tungstene (band-row))))
        (is (nle::projection-cache-live-p model))
        (nle::picker-dialog-key model :escape)
        (is (equal tungstene (band-row)))))))

;;; --- claim 10: /evict -------------------------------------------------------

(deftest slash-evict-folds-percent-share (with-temp-store ())
  ;; Three same-shaped turns: equal token estimates make the forced
  ;; boundary walk exact — 50% lands past the two oldest turns, and the
  ;; realized share is floor(200/3) = 66.
  (do-turns (make-test-session "slash-evict") 3)
  (is-table (line expected) (equal expected (nle:slash line :session-id "slash-evict"))
    ("/evict 50" "Evicted 2 turns (66% of history context) · 2 messages retained")
    ("/evict 50" "nothing to evict — 2 of 3 turns already evicted; the newest turn is always kept")
    ("/evict 0" nle::*evict-usage-hint* "zero percent renders the usage hint, not a request")
    ("/evict 150" nle::*evict-usage-hint* "overflow percent renders the usage hint")
    ("/evict 40%" "nothing to evict — 2 of 3 turns already evicted; the newest turn is always kept"
     "a trailing percent sign parses — the fold already emptied"))
  (is-values (notice cmd)
      (nle::execute-slash-command (make-instance 'nle::chat-model :session-id "slash-evict")
                                  "evict" "bogus")
    (cmd null "evict never returns a command") (notice nle::*evict-usage-hint*)))

;;; --- claim 11: the Zig control-surface commands -----------------------------

(deftest slash-catalog-carries-the-zig-control-commands ()
  (flet ((entry (name) (nle::find-slash-command name)))
    (is-let* ((think (entry "think")))
      (think is "/think registers")
      (think.description "Open provider-native reasoning controls")
      ((entry "effort") think "/effort aliases /think")
      (think.help-detail
       "Bare opens reasoning controls; /think <rung> applies it from the next turn, here and in every new session; /think default picks the model's own"))
    (is-each (nle::slash-command-description)
      ((entry "models") "Show or change the main, auxiliary and decider models" "models owns provider wiring")))
  (is (null (nle::find-slash-command "run")) "/run left with the run projection")
  (is (null (nle::find-slash-command "extensions")) "/extensions left with the extension registry")
  (with-frame-model (model 80 24 :session-id "zig-help")
    (let ((details (mapcar #'nle::picker-row-detail (nle::help-dialog-rows model))))
      (is (find (nle::slash-command-help-detail (nle::find-slash-command "models")) details :test #'equal)))))

(deftest slash-usage-guard-matches-the-zig-gate ()
  (is (null (nle::slash-no-args-guard "think" "")))
  (is-each (nle::slash-no-args-guard)
    ("think" "help" "Usage: /think" "help renders the usage line")
    ("think" "HELP" "Usage: /think" "help is case-insensitive")
    ("setup" "--help" "Usage: /setup" "--help renders the usage line")
    ("setup" "now" "`/setup` does not take arguments." "other args render the Zig rejection"))
  (with-frame-model (model 80 24 :session-id "guard")
    (multiple-value-bind (notice cmd)
        (nle::execute-slash-command model "setup" "help")
      (is (null cmd) "usage help returns no command")
      (is (equal "Usage: /setup" notice) "the dispatch applies the gate"))
    (is (null model.dialog))))

(defun json-text (tree)
  "TREE as JSON text: a keyword plist is an object, a vector an array, :NULL
null, and a string is the text already."
  (if (stringp tree) tree (nlk:encode-json-object (nlk:plist-json tree))))

(defmacro with-model-listings ((&key config catalog cache) &body body)
  "Run BODY over the files a models picker reads — the shared CONFIG, the
models.dev CATALOG and the provider-models CACHE, each text written to a temp
file only when given — with no auth file, no provider's own key in the
environment, and every listing memo cleared."
  ;; The operator's ANTHROPIC_API_KEY connected Anthropic under `just test',
  ;; and the lists these tests read grew a Suggested section.
  (let ((form `(with-temp-file (nle::*auth-file-path*)
                 (let ((nle::*shared-config-memo* nil)
                       (nle::*provider-models-memo* nil)
                       (nle::*models-catalog-memo* nil)
                       ;; The files are read where the organism serves;
                       ;; a shell holds its answer.
                       (nle::*serving* t))
                   (with-stubbed-fdefinition (nle::provider-env-key (provider) nil)
                     ,@body)))))
    (loop for (text place . keys) in `((,cache nle::*provider-models-cache-path*)
                                       (,catalog nle::*models-catalog-path*)
                                       (,config nle::*shared-config-path* :type "jsonc"))
          when text do (setf form `(with-temp-file (,place ,@keys :contents (json-text ,text))
                                     ,form)))
    form))

(defmacro with-slash-dialog ((dialog model command args) why &body body)
  "Assert /COMMAND ARGS opens a dialog on MODEL silently, WHY, then run BODY
with DIALOG bound to the dialog it opened, when it opened one."
  `(progn
     (is (null (nle::execute-slash-command ,model ,command ,args)) ,why)
     (is-present (,dialog (nle::chat-model-dialog ,model)) ,why
       ,@body)))

(defmacro with-model-detail-dialog ((dialog model) why &body body)
  `(progn
     (is (null (nle::open-models-picker ,model)) ,why)
     (let ((,dialog (nle::chat-model-dialog ,model))) ,@body)))

(defparameter +keyed-models-config+
  '(:providers (:anthropic (:api_key "k" :models #((:id "claude-x" :context_window 200000)))
                :zeta (:sdk "openai-completions" :base_url "https://zeta.example/v1" :api_key "k"
                       :models #((:id "z-model" :context_window 32000)))))
  "A shared config typing one model each for two keyed providers.")

(defparameter +claude-x-catalog+ '(:anthropic (:models (:claude-x (:name "Claude X"))))
  "A models.dev catalog naming +KEYED-MODELS-CONFIG+'s anthropic model.")

(defparameter +typed-zeta-config+
  '(:providers (:zeta (:sdk "openai-completions" :base_url "https://zeta.example/v1" :api_key "k"
                       :models #((:id "typed")))))
  "A shared config typing one model for one listable provider, zeta.")

(defparameter +listed-providers-config+
  '(:providers (:zeta (:sdk "openai-completions" :base_url "https://zeta.example/v1" :api_key "k"
                       :models #((:id "pinned-alias") (:id "z-2" :context_window 32000)))
                :local (:sdk "openai-completions" :base_url "http://127.0.0.1:8080/v1")
                :locked (:sdk "anthropic" :base_url "https://locked.example/v1" :api_key "k")))
  "A shared config naming a keyed provider, a keyless local server and a
provider whose listing will fail.")

(defun listed-providers-cache (&optional (now (get-universal-time)))
  "The provider-models cache +LISTED-PROVIDERS-CONFIG+'s providers left at
NOW: two listings and the failed one."
  `(:zeta (:fetched_at ,now :base_url "https://zeta.example/v1" :error :null
           :models #((:id "z-1" :display "Zeta One") (:id "z-2" :context_window 64000)))
    :local (:fetched_at ,now :base_url "http://127.0.0.1:8080/v1" :error :null
            :models #((:id "qwen" :context_window 262144)))
    :locked (:fetched_at ,now :base_url "https://locked.example/v1" :error "HTTP 401" :models #())))

(defun row-facts (row)
  "A picker ROW as (label detail suffix value): what the models list shows and picks."
  (mapcar (lambda (reader) (funcall reader row))
          '(nle::picker-row-label nle::picker-row-detail nle::picker-row-suffix
            nle::picker-row-value)))

;;; --- claim 13b: bare /models reads the shared config ------------------------
;;; The Zig gateway_models_catalog client pipeline: the models.dev catalog is
;;; merged with the configured provider models (a known row's missing context
;;; is filled, config-only models are appended) and retained to the
;;; credentialed provider set, sorted model id then provider id.

(deftest slash-models-picker-reads-the-shared-config ()
  (with-model-listings (:config +keyed-models-config+
                        :catalog '(:anthropic (:models (:claude-x (:name "Claude X Opus"
                                                                   :tool_call t)))
                                   :unconfigured (:models (:nope (:name "Nope" :tool_call t)))))
    ;; No auth file: the credentialed set comes from the config's keys.
    (is (equal '(("Claude X Opus" "anthropic" "  • ctx 200k" ("anthropic" "claude-x"))
                 ("z-model" "zeta" "  • ctx 32k" ("zeta" "z-model")))
               (mapcar #'row-facts (nle::models-catalog-picker-rows))))
    (with-frame-model (model 80 24 :session-id "models-dlg")
      (with-model-detail-dialog (dialog model) "bare /models opens silently"
        (is (equal "Models (2)" (nle::picker-dialog-title dialog)) "the models dialog is open")))))

(deftest slash-models-picker-rows-fold-one-place-per-pair ()
  ;; The fold finds a pair by hash, not by asking every row built so
  ;; far — the quadratic scan the rebuild used to pay. A pair the catalog and
  ;; the config both name stays ONE row, its context budget filled by
  ;; whichever source has it. A display name stays even when its spelling
  ;; differs from the codename only in case and punctuation.
  (with-model-listings (:config '(:providers (:anthropic (:api_key "k"
                                                          :models #((:id "claude-x"
                                                                     :context_window 200000)))))
                        :catalog "{
  \"anthropic\": {\"models\": {\"claude-x\": {\"name\": \"claude-x\", \"limit\": {\"context\": 100000}, \"tool_call\": true},
                             \"claude-y\": {\"name\": \"Claude Y\", \"tool_call\": true}}}
}")
    (is-let* ((rows (nle::models-catalog-picker-rows)))
      ((length rows) = 2 "one row per pair, not one per source")
      ((mapcar #'row-facts rows) '(("claude-x" "anthropic" "  • ctx 100k" ("anthropic" "claude-x"))
                                   ("Claude Y" "anthropic" "" ("anthropic" "claude-y")))))))

(deftest slash-models-picker-names-a-row-its-own-namespace-lacks ()
  ;; A relay lists ids its own models.dev namespace never carries.
  ;; Such a row takes the name most namespaces give its id -- not the
  ;; first namespace's codename -- and the window a turn on it resolves;
  ;; an id no namespace carries keeps its codename and shows no window.
  (with-model-listings (:config '(:providers (:anthropic (:api_key "k"
                                                          :models #((:id "m1") (:id "nowhere")))))
                        :catalog "{
  \"aaa\": {\"models\": {\"m1\": {\"name\": \"m1\", \"limit\": {\"context\": 300000}}}},
  \"bbb\": {\"models\": {\"m1\": {\"name\": \"Model One\"}}},
  \"ccc\": {\"models\": {\"m1\": {\"name\": \"Model One\"}}}
}")
    (let* ((rows (mapcar #'row-facts (nle::models-catalog-picker-rows)))
           (named (find '("anthropic" "m1") rows :key #'fourth :test #'equal))
           (unknown (find '("anthropic" "nowhere") rows :key #'fourth :test #'equal)))
      (is (= 2 (length rows)) "the two configured models, nothing from unretained namespaces")
      (is (equal '("Model One" "anthropic" "  • ctx 300k" ("anthropic" "m1")) named) "named by the namespaces' consensus, sized by the capability seam")
      (is (equal '("nowhere" "anthropic" "" ("anthropic" "nowhere")) unknown) "an id no namespace carries keeps its codename"))))

(deftest slash-models-picker-lists-the-models-a-turn-can-run-on ()
  ;; A turn offers tools and reads text back: setup listed speech, live and
  ;; translation models among a provider's chat ones. The
  ;; catalog's word decides; a model it does not know stays.
  (with-model-listings (:config '(:providers (:anthropic (:api_key "k" :models #((:id "typed")))))
                        :catalog "{
  \"anthropic\": {\"models\": {
    \"chat\": {\"tool_call\": true, \"modalities\": {\"input\": [\"text\"], \"output\": [\"text\"]}},
    \"tts\": {\"tool_call\": false, \"modalities\": {\"input\": [\"text\"], \"output\": [\"audio\"]}},
    \"live\": {\"tool_call\": true, \"modalities\": {\"input\": [\"audio\"], \"output\": [\"text\", \"audio\"]}},
    \"embed\": {\"modalities\": {\"input\": [\"text\"], \"output\": [\"text\"]}}}}
}")
    (is (equal '("chat" "typed")
               (mapcar #'nle::picker-row-label (nle::models-catalog-picker-rows))))))

(deftest slash-models-auxiliary-tab-picks-the-auxiliary-model ()
  ;; /models picks both models under one strip: Main, this session's, and
  ;; Auxiliary, the auxiliary target. Tab moves between them; the Auxiliary
  ;; tab opens on the model in use, Same as main beside it, over the list
  ;; both tabs share. A model picked there lands as the auxiliary model and
  ;; pins no session, then asks its own reasoning effort, Default its
  ;; lightest; Esc from its models goes back to the tab it came from.
  ;; /model-aux, a second place to look, is gone.
  (is (null (nle::find-builtin-slash-command "model-aux")))
  (with-saved-globals ((nle::*auxiliary-model* nil))
    (with-model-listings (:config +keyed-models-config+ :catalog +claude-x-catalog+)
      (with-frame-model (model 100 40 :session-id "models-auxiliary")
        (let ((picked '()))
          (with-stubbed-fdefinitions
              ((nle::slash-auxiliary-model-op (model tokens &key (effort nil effort-p))
                 (push (if effort-p (list tokens :effort effort) tokens) picked)
                 "auxiliary model: picked")
               (nle::gateway-model-http-call (model method path body)
                 (nlk:decode-json "{\"provider\":\"zeta\",\"model\":\"z-model\",\"effort\":\"low\",
                                   \"efforts\":[\"off\",\"low\",\"high\"]}")))
            (with-slash-dialog (dialog model "models" "") "/models opens silently"
              (is (equal '("Main" "Auxiliary" "Decider" "Backup")
                         (mapcar (lambda (tab) (getf tab :label)) (nle::picker-dialog-tabs dialog))))
              (is (= 0 (nle::picker-dialog-tab dialog)) "Main is in front")
              (is (search "←→ main/auxiliary/decider/backup" (nle::picker-dialog-hint dialog)))
              ;; Main shows none of Auxiliary's rows.
              (is (notany (lambda (row) (equal "aux" (nle::picker-row-key row)))
                          (nle::picker-visible-rows dialog)))
              (is (= 1 (nle::picker-dialog-tab (dialog-after model :right))) "Right walks to Auxiliary")
              (is (equal "Session names, recaps, summaries"
                         (nle::picker-row-label (first (nle::picker-visible-rows dialog)))))
              ;; It opens on the one in use: Same as main, with none set.
              (is (equal '(:model :default) (picker-selected-value dialog)))
              (is (search "in use" (nle::picker-row-detail
                                    (nth (nle::picker-dialog-selection dialog)
                                         (nle::picker-visible-rows dialog)))))
              (dialog-keys model :enter)
              (is (equal '("default") (first picked)) "Same as main takes the auxiliary model out")
              (is (null model.dialog)))
            (is (null (nle::execute-slash-command model "models" "aux")))
            (is (= 1 (nle::picker-dialog-tab model.dialog)) "/models aux opens on Auxiliary")
            (is (enter-row model "zeta" :provider))
            (is-shape model.dialog (nle::picker-dialog-kind eq :auxiliary-models)
              (nle::picker-dialog-title "Auxiliary model (2)"))
            (is (search "enter use as auxiliary" (nle::picker-dialog-hint model.dialog)))
            (is (eq :model-providers (nle::picker-dialog-kind (dialog-after model :escape))) "Esc goes back to the providers")
            (is (= 1 (nle::picker-dialog-tab model.dialog)) "on the tab it left")
            (enter-row model "zeta" :provider)
            (is (enter-row model "z-model"))
            (is (equal '("zeta" "z-model") (first picked)))
            ;; Its reasoning effort is asked next, the rung it runs at marked.
            (is-shape model.dialog (nle::picker-dialog-kind eq :think)
              (nle::picker-dialog-subtitle "zeta/z-model, the auxiliary model"))
            (is (equal '(nil "off" "low" "high")
                       (mapcar #'nle::picker-row-value (nle::picker-visible-rows model.dialog))))
            (is (equal "The lightest it offers"
                       (nle::picker-row-detail (first (nle::picker-visible-rows model.dialog)))))
            (is (equal "low" (picker-selected-value model.dialog)))
            (is (enter-row model "high"))
            (is (equal '(("zeta" "z-model") :effort "high") (first picked)))
            (is (null model.dialog))))
        (is (search "auxiliary model: zeta/z-model" (nle:slash "/models aux zeta z-model")))
        (is (equal '(:provider "zeta" :model "z-model" :effort nil) nle::*auxiliary-model*))
        (let ((nle::*shared-config-memo* nil))
          (is (equal "z-model" (nlk:json-value (nle::read-shared-config)
                                               :string "general" "auxiliary_model" "model"))))
        (is (equal "openai-completions" nle::*provider*) "no session model moved")
        ;; The tab opens on the auxiliary model in use, Same as main under it.
        (nle::execute-slash-command model "models" "aux")
        (is (equal '(:model ("zeta" "z-model")) (picker-selected-value model.dialog)))
        (is (equal '("z-model" "Same as main")
                   (mapcar #'nle::picker-row-label
                           (remove-if #'nle::picker-row-heading
                                      (subseq (nle::picker-visible-rows model.dialog) 0 4)))))
        (dialog-keys model :escape)
        (is (search "auxiliary model: same as main" (nle:slash "/models aux default")))
        (is (null nle::*auxiliary-model*))
        (let ((nle::*shared-config-memo* nil))
          (is (null (nlk:json-value (nle::read-shared-config) :object "general" "auxiliary_model"))))
        (is (search "usage:" (nle::execute-slash-command model "models" "aux a b c")))))))

(deftest a-dialog-pick-paints-the-notice-it-lands ()
  ;; A pick whose answer is a notice -- the auxiliary model's, landed from a
  ;; /models row -- shows in the very next frame: the notice is the
  ;; projection's tail, and the key that closes the dialog projects nothing
  ;; anew; without this, /models would close onto a blank frame until a resize.
  (with-frame-model (model 100 30 :session-id "dialog-pick-notice")
    (nle::compose-frame model)
    (nle::open-picker-dialog model "Probe" (list (nle::make-picker-row :label "one" :value 1))
                             (nlk:lambda-ignoring (model value)
                               (format nil "picked ~d" value)))
    (nle::compose-frame model)
    (press model :enter)
    (is (null model.dialog))
    (is (rows-carry-p "picked 1" (nle::compose-frame model)))))

(defparameter +local-and-decider-config+
  '(:providers (:zeta (:sdk "openai-completions" :base_url "https://zeta.example/v1" :api_key "k"
                       :models #((:id "z-model")))
                :home (:sdk "openai-completions" :base_url "http://127.0.0.1:8080/v1" :api_key "k"
                       :models #((:id "q-local")))
                :djev (:base_url "http://100.101.102.103:8011")))
  "A remote keyed provider, one served from this machine, and DJev moved to a
tailnet address.")

(deftest slash-models-decider-tab-and-local-models ()
  ;; /models' Main tab gathers the models a machine of the operator's own
  ;; serves under Local, above the providers, each a pick like a Frequently
  ;; used shortcut. Its third tab, Decider, shows the deciders alone -- no
  ;; chat model, no provider -- the one in use first, None when none is; a
  ;; pick lands general.decider_model, and an id no decider has is refused
  ;; before anything moves. A decider's config entry moves its address only:
  ;; it is no lane to list.
  (with-model-listings (:config +local-and-decider-config+)
    (with-frame-model (model 100 40 :session-id "models-decider")
      (let ((picked '()))
        (with-stubbed-fdefinitions
            ((nle::slash-decider-model-op (model tokens) (push (list :decider tokens) picked)
               "decider model: picked")
             (nle::slash-models-op (model tokens) (push (list :main tokens) picked)
               "session model: picked")
             (nle::open-models-reasoning-picker (model) nil))
          (with-slash-dialog (dialog model "models" "") "/models opens silently"
            (let* ((rows (nle::picker-visible-rows dialog))
                   (at (position "Local" rows :key #'nle::picker-row-label :test #'equal)))
              (is (and at (nle::picker-row-heading (nth at rows))) "Main has a Local heading")
              (is (equal '("q-local") (loop for row in (nthcdr (1+ at) rows)
                                            until (nle::picker-row-heading row)
                                            collect (nle::picker-row-label row))) "under it the models this machine serves, never a remote one")
              (is (< at (position "Providers" rows :key #'nle::picker-row-label :test #'equal)) "above the providers")
              (is (notany (lambda (row) (equal "decider" (nle::picker-row-key row))) rows) "Main shows no decider"))
            (is (enter-row model "q-local"))
            (is (equal '(:main ("home" "q-local")) (first picked)) "a Local row picks its pair"))
          (nle::execute-slash-command model "models" "decider")
          (let ((dialog model.dialog))
            (is (= 2 (nle::picker-dialog-tab dialog)) "/models decider opens on Decider")
            (is (equal '("Judgments as probabilities" "None" "DJev" "")
                       (mapcar #'nle::picker-row-label (nle::picker-visible-rows dialog))) "the deciders alone, None in use first")
            (is (equal '(:model (:decider nil)) (picker-selected-value dialog)) "it opens on the one in use")
            (is (equal "dgemma · local · 100.101.102.103:8011"
                       (nle::picker-row-detail (third (nle::picker-visible-rows dialog)))) "DJev names its model and the address its entry moved it to"))
          (is (enter-row model "DJev"))
          (is (equal '(:decider ("djev")) (first picked)) "Enter picks it")
          (is (null model.dialog))))
      (is (notany (lambda (id) (string-equal id "djev")) (nle::listable-provider-ids)) "a decider is never listed as a lane")
      (is (search "decider model: none" (nle:slash "/models decider")))
      (is (search "decider model: djev/dgemma" (nle:slash "/models decider djev")))
      (let ((nle::*shared-config-memo* nil))
        (is (equal '("djev" "dgemma") (subseq (multiple-value-list (nle::shared-model-pin "decider_model")) 0 2))))
      (is (signals-error nle::provider-config-error (nle:slash "/models decider nope")))
      (let ((nle::*shared-config-memo* nil))
        (is (equal "djev" (nle::shared-model-pin "decider_model")) "a refused id moves nothing"))
      ;; The tab now opens on DJev, in use.
      (nle::execute-slash-command model "models" "decider")
      (is (equal '(:model (:decider "djev")) (picker-selected-value model.dialog)))
      (is (search "in use" (nle::picker-row-detail (second (nle::picker-visible-rows model.dialog)))))
      (dialog-keys model :escape)
      (is (search "decider model: none" (nle:slash "/models decider none")))
      (let ((nle::*shared-config-memo* nil))
        (is (null (nle::shared-model-pin "decider_model")))))))

(deftest slash-models-backup-tab-reads-and-writes-the-chain ()
  ;; /models' fourth tab, Backup, is the failover chain every session and room
  ;; share: each backup in order with what history says of it, Add, the
  ;; triggers as rows Enter switches, and Test. The state is the gateway's
  ;; route, read off the loop; a pick posts and opens the tab again.
  (with-model-listings (:config +local-and-decider-config+)
    (with-frame-model (model 100 40 :session-id "models-backup")
      (let* ((posts '())
             (state (nlk:decode-json "{\"chain\":[{\"provider\":\"deepseek\",\"model\":\"deepseek-flash\",\"answered_at\":null,\"tried\":3,\"rescued\":1}],\"triggers\":[{\"id\":\"rate_limit\",\"label\":\"rate limit\",\"on\":true},{\"id\":\"refusal\",\"label\":\"refusal\",\"on\":true}]}")))
        (with-stubbed-fdefinitions
            ((nle::off-loop (model thunk continuation) (funcall continuation model (funcall thunk)))
             (nle::start-provider-models-refresh (model) nil)
             (nle::gateway-model-http-call (model method path body)
               (cond ((search "frequent" path) (nlk:decode-json "{\"models\":[]}"))
                     ((search "failover/test" path)
                      (nlk:decode-json "{\"results\":[{\"provider\":\"deepseek\",\"model\":\"deepseek-flash\",\"ok\":true,\"reason\":null,\"seconds\":1.1}]}"))
                     (t (when (eq method :post) (push (list path (nlk:encode-json-object body)) posts))
                        state))))
          (setf (nle::chat-model-program model) t)
          (with-slash-dialog (dialog model "models" "backups") "/models backups opens silently"
            (is (= 3 (nle::picker-dialog-tab dialog)) "on the Backup tab")
            (is (equal '("Tried in order · enter takes one out" "1. deepseek-flash" "Add a backup…"
                         "Test backups" "" "Switch when the model hits" "✓ rate limit" "✓ refusal" "")
                       (mapcar #'nle::picker-row-label (nle::picker-visible-rows dialog))) "the chain, Add, the triggers, Test")
            (is (search "deepseek · never answered · rescued 1/3" (nle::picker-row-detail (second (nle::picker-visible-rows dialog)))) "what history says of the backup")
            (is (notany (lambda (row) (equal "backup" (nle::picker-row-key row)))
                        (progn (dialog-after model :left) (nle::picker-visible-rows model.dialog))) "no other tab shows its rows"))
          (nle::execute-slash-command model "models" "backups")
          (is (enter-row model "✓ refusal"))
          (is (equal '("/api/gateway/failover" "{\"on\":[\"rate_limit\"]}") (first posts)) "Enter switches the trigger off")
          (is (and model.dialog (= 3 (nle::picker-dialog-tab model.dialog))) "the tab opens again")
          (is (enter-row model "1. deepseek-flash"))
          (is (equal '("/api/gateway/failover" "{\"chain\":[]}") (first posts)) "Enter takes the backup out")
          (is (enter-row model "Test backups"))
          (is (rows-carry-p "deepseek-flash ✓ 1.1 s" (nle::compose-frame model)) "Test says what each backup answered"))))))

(deftest local-endpoint-p-reads-the-host ()
  ;; Local is a machine of the operator's own, whatever port or path it serves on.
  (is-table (url expected) (eq expected (nle::local-endpoint-p url))
    ("http://127.0.0.1:4000/v1" t) ("http://localhost:20127/v1" t) ("http://[::1]:8000" t)
    ("http://192.168.1.4:8080" t) ("http://10.0.0.2" t) ("http://172.20.1.1" t)
    ("http://100.101.102.103:8011" t "a tailnet address") ("http://box.tail1234.ts.net" t)
    ("http://nas.local:9000" t) ("http://[fd7a:115c::1]:80" t)
    ("https://api.deepseek.com/v1" nil) ("http://172.32.0.1" nil) ("http://100.128.0.1" nil)
    ("http://8.8.8.8" nil) ("" nil) (nil nil)))

;;; --- claim 9c: the room's two seams into the auxiliary target -----------------

(deftest slash-model-commands-complete-the-catalog-rows ()
  ;; The completion menu — the kit's autocomplete path, the one way a room
  ;; ask sees a command's argument at all — offers /models the catalog rows,
  ;; `aux' and `decider' ahead of them while the tail could still be either,
  ;; after `aux ' the same rows as auxiliary picks, and after `decider ' the
  ;; deciders.
  (with-model-listings (:config +keyed-models-config+ :catalog +claude-x-catalog+)
    (flet ((names (text)
             (mapcar (lambda (choice) (getf choice :name))
                     (nle:slash-completions "models" text))))
      (is (equal '("aux — the auxiliary model" "decider — the decider model" "backups — the backup models" "anthropic/claude-x" "zeta/z-model") (names "")))
      (is (equal '("decider djev" "decider none") (names "decider ")) "after `decider ', the deciders and none")
      (is (equal '("decider djev") (names "decider dj")))
      (is (equal '("anthropic/claude-x") (names "claude")))
      (is (equal '("anthropic/claude-x" "zeta/z-model") (names "aux ")))
      (is (equal "aux" (getf (first (nle:slash-completions "models" "au")) :value)))
      (is (equal "anthropic claude-x"
                 (getf (first (nle:slash-completions "models" "claude")) :value)))
      (is (equal "aux zeta z-model"
                 (getf (first (nle:slash-completions "models" "aux z-m")) :value))))))

(defun catalog-field (name field &optional (catalog (nle:slash-catalog)))
  "FIELD of the entry NAME in the headless CATALOG (NLE:SLASH-CATALOG's plists)."
  (getf (find name catalog :key (lambda (entry) (getf entry :name)) :test #'string=) field))

(deftest slash-completions-reach-an-cell-command-with-its-session ()
  ;; A cell's command completes as a builtin does, and its completer sees
  ;; the session the command would act on, as its headless arm does.
  (with-saved-globals (nle::*registered-commands*)
    (nle:register-command "probe-pack" "probe" (constantly "ran")
                          :argument-hint "word"
                          :complete (lambda (text session-id)
                                      (list (list :name (format nil "~a in ~a" text session-id)
                                                  :value text))))
    (nle:register-command "probe-pack" "plain" (constantly "ran"))
    (is (eq t (catalog-field "probe" :autocomplete)) "the catalog marks it")
    (is (null (catalog-field "plain" :autocomplete)) "a command with no completer is not marked")
    (is (equal '((:name "wo in s-room" :value "wo"))
               (nle:slash-completions "probe" "wo" :session-id "s-room")))
    (is (null (nle:slash-completions "plain" "wo")))))

(deftest slash-models-aux-headless-answers-the-auxiliary-model ()
  ;; The room's seam: NLE:SLASH runs HEADLESS-MODELS, so `/models aux'
  ;; answers the auxiliary model with how to change it — never the session
  ;; summary — a spelled pick lands as the target the answer then names, and
  ;; `default' takes it out.
  (with-saved-globals ((nle::*auxiliary-model* '(:provider "zeta" :model "z-model" :effort "low")))
    (let ((answer (nle::headless-models "aux" nil)))
      (is (uiop:string-prefix-p "auxiliary model: zeta/z-model (low)" answer))
      (is (search "/models aux <provider> <model> changes it" answer))
      (is (null (search "provider: " answer)) "not the session summary"))
    (setf nle::*auxiliary-model* nil)
    (is (uiop:string-prefix-p "auxiliary model: same as main" (nle::headless-models "aux" nil)))
    (with-model-listings (:config +keyed-models-config+)
      (is (uiop:string-prefix-p "auxiliary model: zeta/z-model"
                                (nle::headless-models "aux zeta z-model" nil)))
      (is (equal '(:provider "zeta" :model "z-model" :effort nil)
                 nle::*auxiliary-model*))
      (is (uiop:string-prefix-p "auxiliary model: same as main"
                                (nle::headless-models "aux default" nil)))
      (is (null nle::*auxiliary-model*)))
    (with-model-listings (:config +listed-providers-config+ :cache (listed-providers-cache))
      (is (search "zeta: 3 models — /models list zeta"
                  (nle::headless-models "list" nil)))
      (is (search "  z-1" (nle::headless-models "list zeta" nil))))))

(deftest models-verb-answers-the-picker-catalog ()
  ;; The model-facing read of the same catalog the /models dialog shows:
  ;; counts per provider bare, ids per provider, the listing failure
  ;; beside them — as values, with no dialog and no shell. The verb is
  ;; one call.
  (with-model-listings (:config +listed-providers-config+ :cache (listed-providers-cache))
    (is (equal '(("local" . 1) ("locked" . 0) ("zeta" . 3))
               (nle:models)))
    (is-values (ids error) (nle:models "zeta") (ids '("pinned-alias" "z-2" "z-1"))
      (error null "and no failure to report"))
    (is-values (ids error) (nle:models "locked") (ids null "a failed listing answers no ids")
      (error "HTTP 401" "and says why"))
    (is (nth-value 1 (ignore-errors (nle:models "nope"))))))

(deftest slash-models-picker-fills-from-the-fetched-listings (let ((now (get-universal-time))))
  ;; A provider is {base_url, sdk, api_key}: its rows come from the cached
  ;; GET /models answer, merged with whatever the config still types by
  ;; hand, and a keyless local server lists too. A provider whose listing
  ;; failed and that has no row from any source keeps one placeholder row.
  (with-model-listings (:config +listed-providers-config+ :cache (listed-providers-cache now))
    (is (equal '(("pinned-alias" "zeta" "" ("zeta" "pinned-alias"))
                 ("qwen" "local" "  • ctx 262.1k" ("local" "qwen"))
                 ("z-2" "zeta" "  • ctx 32k" ("zeta" "z-2"))
                 ("Zeta One" "zeta" "" ("zeta" "z-1"))
                 ("locked/" "no listing (HTTP 401)" "" nil))
               (mapcar #'row-facts (nle::models-catalog-picker-rows))))
    (with-frame-model (model 80 24 :session-id "models-fetched")
      (with-model-detail-dialog (dialog model) "bare /models opens silently"
        (is (eq :models (nle::picker-dialog-kind dialog)))
        (is (equal "Models (5)" (nle::picker-dialog-title dialog)))
        ;; Narrow to zeta and rest on z-2, then let a refresh land
        ;; that adds a model ahead of it: the search text stays
        ;; and the selection follows its value.
        (is (equal '("zeta" "z-2") (picker-selected-value (dialog-after model "zeta" :down))))
        (write-temp-file
         nle::*provider-models-cache-path*
         (format nil "{
  \"zeta\": {\"fetched_at\": ~d, \"base_url\": \"https://zeta.example/v1\", \"error\": null,
            \"models\": [{\"id\": \"z-0\"}, {\"id\": \"z-1\"}, {\"id\": \"z-2\"}]},
  \"local\": {\"fetched_at\": ~d, \"base_url\": \"http://127.0.0.1:8080/v1\", \"error\": null, \"models\": []},
  \"locked\": {\"fetched_at\": ~d, \"base_url\": \"https://locked.example/v1\", \"error\": null, \"models\": [{\"id\": \"opened\"}]}
}" now now now))
        (setf nle::*provider-models-memo* nil)
        (nle::install-refreshed-models model)
        (is (eq dialog (nle::chat-model-dialog model)))
        (is-shape dialog (nle::picker-dialog-title "Models (5)")
          (nle::picker-dialog-search "zeta")
          (visible-labels '("pinned-alias" "z-2" "z-1" "z-0"))
          (picker-selected-value '("zeta" "z-2")))))))

(deftest slash-models-picker-names-the-sdk-a-provider-lacks ()
  ;; A provider entry naming no sdk cannot be listed at all, and its
  ;; hand-typed rows alone hid that: the operator saw one a6api model where
  ;; the listing carries ninety, and no reason. The listing failure is the
  ;; config refusal, verbatim, and the placeholder trails the typed rows.
  (with-model-listings (:config '(:providers (:nosdk (:base_url "https://nosdk.example/v1"
                                                      :api_key "k" :models #((:id "typed"))))))
    (is-values (rows error) (nle::list-provider-models "nosdk")
      (rows null "nothing is fetched without a wire") ((search "providers.nosdk.sdk" error) is))
    (let ((now (get-universal-time)))
      (with-temp-file (nle::*provider-models-cache-path*
                       :contents (format nil "{
  \"nosdk\": {\"fetched_at\": ~d, \"base_url\": \"https://nosdk.example/v1\", \"error\": \"provider \\\"nosdk\\\" names no sdk: set providers.nosdk.sdk\", \"models\": []}
}" now))
        (let ((rows (nle::models-catalog-picker-rows)))
          (is (equal '("typed" "nosdk/")
                     (mapcar #'nle::picker-row-label rows)))
          (is (search "names no sdk" (nle::picker-row-detail (second rows)))))))))

(deftest slash-models-picker-keeps-a-listing-its-refresh-failed-to-renew ()
  ;; A refresh that failed keeps the rows the last listing carried, with
  ;; the reason beside them: the picker shows the models it had, not an
  ;; empty provider with a placeholder, and the models verb says why.
  (with-model-listings (:config '(:providers (:blip (:sdk "openai-completions"
                                                     :base_url "https://blip.example/v1"
                                                     :api_key "k")))
                        :cache `(:blip (:fetched_at ,(get-universal-time) :error "connection reset"
                                        :base_url "https://blip.example/v1"
                                        :models #((:id "kept" :context_window 8192)))))
    (is (equal '(("kept" "blip" "  • ctx 8.2k" ("blip" "kept")))
               (mapcar #'row-facts (nle::models-catalog-picker-rows))))
    (is-values (ids error) (nle:models "blip") (ids '("kept") "the verb answers the kept ids")
      (error "connection reset" "and the reason beside them"))))

(deftest slash-models-picker-repaints-when-its-first-listing-lands (let ((now (get-universal-time))))
  ;; The row memo is rebuilt only when a listing file is read anew; an
  ;; absent file and a file just written and not yet read both leave the
  ;; read record NIL. Without the write-date identity and the writer
  ;; dropping the read memo, the first listing a box fetches would never
  ;; reach its picker, open or reopened. The identity is the cache file's
  ;; own write date, the way auth.json's already was, and the writer drops
  ;; the read memo the way FETCH-MODELS-CATALOG always did.
  (with-model-listings (:config +typed-zeta-config+)
    ;; No cache file at all: a box that has never fetched a listing.
    (with-temp-file (nle::*provider-models-cache-path*)
      (with-frame-model (model 80 24 :session-id "models-landed")
        (with-model-detail-dialog (dialog model) "bare /models opens silently"
          (is (equal "Models (1)" (nle::picker-dialog-title dialog)))
          ;; What REFRESH-PROVIDER-MODELS does once its workers answer:
          ;; merge into the root and write it through the one writer.
          (nle::write-provider-models-cache
           (nle::json-object "zeta" (nle::json-object "fetched_at" now "base_url" "https://zeta.example/v1" "error" :null
                                                      "models" (vector (nle::json-object "id" "z-1")
                                                                       (nle::json-object "id" "z-2:free")))))
          (nle::install-refreshed-models model)
          (is (equal "Models (3)" (nle::picker-dialog-title dialog)) "the landed rows are in the open picker, no memo reset by hand")
          (is (equal '("typed" "z-2:free" "z-1") (row-labels dialog)) "newest first: an undated listing by version, the higher first"))
        ;; And a reopen builds from the file, not from the rows it opened on.
        (with-model-detail-dialog (dialog model) "a second /models opens silently"
          (is (equal "Models (3)" (nle::picker-dialog-title dialog))))))))

(deftest slash-models-refresh-forgets-the-listings-before-opening ()
  (with-model-listings (:config +typed-zeta-config+
                        :cache (format nil "{\"zeta\": {\"fetched_at\": ~d, \"base_url\": \"https://zeta.example/v1\", \"error\": null, \"models\": [{\"id\": \"fetched\"}]}}"
                                           (get-universal-time)))
    (with-frame-model (model 80 24 :session-id "models-refresh")
      (with-slash-dialog (dialog model "models" "refresh") "/models refresh opens silently"
        (is (nle::connect-dialog-p dialog))
        (is (find "zeta" (row-labels dialog) :test #'equal)))
      (is (null (nle::provider-models-cache-entry "zeta")))
      (is (equal '("zeta") (nle::stale-listable-provider-ids))))))

;;; --- claim 14: /think ------------------------------------------------------

;;; --- claim 15: /fast -------------------------------------------------------

;;; --- claim 16: /models ----------------------------------------------------

(defun enter-row (model label &optional field)
  "Seat MODEL's open dialog on the first row it shows whose label is LABEL, or
whose value's FIELD is when FIELD is given, and press Enter there; answers the
row's index, NIL when no row matched."
  (let* ((dialog model.dialog)
         (at (position label (nle::picker-visible-rows dialog) :test #'equal
                       :key (if field
                                (lambda (row) (getf (nle::picker-row-value row) field))
                                #'nle::picker-row-label))))
    (setf (nle::picker-dialog-selection dialog) at)
    (dialog-keys model :enter)
    at))

(defun step-after (model &rest keys)
  "The step MODEL's connect dialog stands on once KEYS are pressed on it."
  (nle::connect-dialog-step (apply #'dialog-after model keys)))

;;; The provider list is the models.dev catalog, not the four SDK lanes this
;;; image registers (the Zig connectDialogCatalogFromGateway shape). The
;;; fixture below stands in for the cache file the gateway writes, so the
;;; list is deterministic and independent of whatever the machine has
;;; fetched.

(defparameter +connect-catalog-fixture+
  "{\"anthropic\":{\"name\":\"Anthropic\",\"npm\":\"@ai-sdk/anthropic\",\"models\":{\"a\":{\"tool_call\":true},\"b\":{\"tool_call\":true}}},
    \"acme-labs\":{\"name\":\"Acme Labs\",\"npm\":\"@ai-sdk/openai-compatible\",\"models\":{\"one\":{\"tool_call\":true}}},
    \"barecorp\":{\"npm\":\"@ai-sdk/openai-compatible\",\"models\":{\"x\":{\"tool_call\":true},\"y\":{\"tool_call\":true},\"z\":{\"tool_call\":true}}},
    \"speechco\":{\"name\":\"Speechco\",\"npm\":\"@ai-sdk/openai-compatible\",\"models\":{\"tts\":{}}},
    \"lost\":{\"name\":\"Lost\",\"npm\":\"@nowhere/sdk\",\"models\":{\"q\":{\"tool_call\":true}}}}"
  "Five models.dev providers: two named, one that must fall back to its id
as the display name, one with no model a turn can run on, and one whose
package no lane speaks.")

(deftest slash-connect-walks-the-zig-wizard ()
  (with-model-listings (:catalog +connect-catalog-fixture+)
    (with-frame-model (model 80 24 :session-id "connect-wiz")
      (with-slash-dialog (dialog model "models" "") "/models opens silently"
        (is-present (wizard (nle::connect-dialog-p dialog)) "the wizard is open"
          (let ((rows (nle::picker-visible-rows dialog))) ; the Main tab's
            ;; a pinned provider first, a heading, the rest by name; no lane
            ;; speaks Lost's package, and Speechco has no model a turn runs on
            (is (equal '("Anthropic" "More providers, A to Z" "Acme Labs" "barecorp" "Other provider")
                       (mapcar #'nle::picker-row-label rows)))
            (is (nle::picker-row-heading (second rows)))
            (is (equal "anthropic"
                       (getf (nle::picker-row-value (first rows))
                             :provider)))
            (is (equal "console.anthropic.com/settings/keys"
                       (getf (nle::picker-row-value (first rows)) :key-page)))
            (is (equal "2 models" (nle::picker-row-detail (first rows))))
            (is (equal "1 model" (nle::picker-row-detail (third rows)))))
          ;; The arrows pass over the heading, both ways.
          (is (= 2 (nle::connect-dialog-selection (dialog-after model :down))))
          (is (= 0 (nle::connect-dialog-selection (dialog-after model :up))))
          ;; Up wraps to Other provider; Enter enters the custom step.
          (is (eq :custom-provider (step-after model :up :enter)))
          ;; An invalid id (uppercase) is refused in place.
          (is (eq :custom-provider (step-after model #\B #\a #\d :enter)))
          (nle::picker-dialog-key model #\u :ctrl t)
          (is (eq :api-key (step-after model "acme-labs" :enter)))
          (dialog-keys model #\s #\k #\- #\1 #\2 #\3)
          (is-carrying (panel (format nil "~{~a~%~}" (nle::dialog-panel-rows model 60 nil)))
            "API key" "******" (:absent "sk-123")
            "Paste your acme-labs API key."
            " enter  save   esc  back")
          ;; Enter saves and returns to the list, where the typed id reads
          ;; connected: Suggested, resting on it.
          (is (eq :provider (step-after model :enter)) "a saved key has no screen of its own")
          (is (zerop (length dialog.input)))
          (is (equal '("Suggested" "Acme Labs" "Connect another" "Anthropic"
                       "More providers, A to Z" "barecorp" "Other provider")
                     (mapcar #'nle::picker-row-label (nle::picker-visible-rows dialog))))
          (is (= 1 (nle::connect-dialog-selection dialog)) "resting on the connected provider")
          (is (equal "sk-123" (nlk:json-value
                               (shasht:read-json (uiop:read-file-string nle::*auth-file-path*))
                               :string "api_keys" "acme-labs" "key")))
          (is (not (nle::connect-dialog-p (dialog-after model :escape))))))
      ;; Esc semantics: back out step by step, search clears first.
      (nle::execute-slash-command model "models" "")
      (let ((dialog model.dialog))
        (enter-row model "anthropic" :provider)
        (is (eq :api-key (nle::connect-dialog-step dialog)))
        (is (eq :provider (step-after model :escape)))
        (is (equal "" (nle::connect-dialog-search (dialog-after model "op" :escape))))
        (is (null (dialog-after model :escape)))))))

(deftest connect-credential-sink-says-whether-the-key-works ()
  ;; `works' is the provider's answer to the key. A key nothing answered -- no
  ;; answer in time, a 5xx, a 404, a check that signalled -- may be right: it
  ;; is saved, says it was not checked and why, and the list keeps it off
  ;; `connected'. A key the check never
  ;; asked about (NIL, the hermetic posture) is only saved. A refusal saves
  ;; nothing: the key's pastes another, and a port nothing listens on is the
  ;; server's, so the same key is checked again.
  (with-model-listings (:catalog +connect-catalog-fixture+)
    (with-frame-model (model 80 24 :session-id "connect-sink")
      (is-table (check expected)
          (equal expected
                 (let ((said nil))
                   (clrhash nle::*key-checks*)
                   (with-stubbed-fdefinition (nle::provider-key-check (provider key) check)
                     (funcall (nle::connect-credential-sink) model "anthropic" "sk-1"
                              (nlk:lambda-ignoring (model message error)
                                (setf said (list message error)))))
                   (list said (nle::key-check-state "anthropic"))))
          ((values :works nil) '(("Anthropic key works, saved to auth.json" nil) nil))
          (nil '(("Anthropic key saved to auth.json" nil) nil))
          ((values :unchecked "no answer from 127.0.0.1:9 in 5 s")
           '(("Anthropic key saved to auth.json, not checked: no answer from 127.0.0.1:9 in 5 s" nil)
             "not checked"))
          ;; a check that signalled is said plainly, never in its condition's words
          ((error "#<SB-INT:SIMPLE-ERROR offline>")
           '(("Anthropic key saved to auth.json, not checked: the check itself failed" nil) "not checked"))
          ((values :refused "Anthropic refused this key (HTTP 401)")
           '(("Anthropic refused this key (HTTP 401); nothing was saved. Enter pastes another." t) nil))
          ((values :unreachable "Anthropic could not check the key: nothing listens at 127.0.0.1:1")
           '(("Anthropic could not check the key: nothing listens at 127.0.0.1:1; nothing was saved. Start the server, then Enter checks again; Esc picks another provider."
              :retry)
             nil))))))

(deftest connect-checks-a-key-again-where-nothing-listened (let ((checks '()) (verdict :unreachable)))
  ;; A key checked against a stopped server said `Enter pastes another' and
  ;; emptied the field, though the key was not the problem (xm-202): Enter
  ;; checks the same key again once the server is up, and Esc picks another
  ;; provider, since any key would meet the same stopped server.
  (with-model-listings (:catalog +connect-catalog-fixture+)
    (with-stubbed-fdefinition (nle::provider-key-check (provider key)
                                (push key checks)
                                (if (eq verdict :unreachable)
                                    (values :unreachable (format nil "~a could not check the key: nothing listens at 127.0.0.1:1" provider))
                                    (values verdict nil)))
      (with-frame-model (model 80 24 :session-id "connect-retry")
        (with-slash-dialog (dialog model "models" "") "/models opens silently"
          (dialog-keys model :enter)
          (is (eq :result (step-after model "sk-9" :enter)))
          (is-carrying (words (nle::connect-dialog-result-message dialog))
            "nothing listens at 127.0.0.1:1" "nothing was saved" "Start the server, then Enter checks again"
            (:absent "pastes another"))
          (is (search "enter check again" dialog.hint))
          (is (null (nle::configured-provider-inventory)) "nothing was saved")
          ;; the server is up: Enter checks the same key, which lands
          (setf verdict :works)
          (dialog-keys model :enter)
          (is (equal '("sk-9" "sk-9") checks) "the same key, checked again")
          (is (eq :provider (nle::connect-dialog-step dialog)))
          (is (equal '("anthropic") (nle::configured-provider-inventory)))
          (is (null (nle::connect-dialog-retry dialog)) "the key is let go once it lands")
          ;; Esc from a result nothing listened for goes back to the providers
          (setf verdict :unreachable)
          (dialog-keys model :down :enter)
          (is (eq :result (step-after model "sk-10" :enter)))
          (is (eq :provider (step-after model :escape)))
          (is (null (nle::connect-dialog-retry dialog)) "and forgets the key"))))))

(deftest connect-checking-counts-up-to-its-bound (let ((nle::*provider-models-fetch-timeout-seconds* 5)))
  ;; A static `checking...' read as a hang for the check's five seconds
  ;; (ca-102): the step names the provider and the bound, and its clock moves
  ;; on the tick while the check runs, and only then.
  (with-model-listings (:catalog +connect-catalog-fixture+)
    (with-frame-model (model 80 24 :session-id "connect-checking")
      (with-slash-dialog (dialog model "models" "") "/models opens silently"
        (dialog-keys model :enter)
        (nle::connect-enter-step dialog :checking)
        (is (equal "Asking Anthropic whether it takes this key, waiting up to 5 seconds." dialog.subtitle))
        (is-carrying (panel (format nil "~{~a~%~}" (nle::dialog-panel-rows model 80 nil)))
          "checking… 0.0s")
        (is dialog.on-tick "the tick repaints the clock")
        (setf (nle::connect-dialog-checking-since dialog)
              (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
        (is (funcall dialog.on-tick model dialog) "a moved clock repaints")
        (is (not (funcall dialog.on-tick model dialog)) "a still one does not")
        (is (search "checking… 2." (nle::connect-checking-clock dialog)))
        (nle::connect-enter-step dialog :api-key)
        (is (null dialog.on-tick) "no clock once the check is over")))))

(deftest connect-list-says-which-providers-are-connected-and-the-models-keep-to-them ()
  ;; A provider is connected by a saved key or by its own <ID>_API_KEY, never
  ;; by the environment's SDK-family default: one OPENAI_API_KEY would
  ;; connect every chat-completions provider. Nor does a saved key no lane
  ;; speaks: without this, a saved key for such a provider would still have the
  ;; list ask its credential source, which signals, and /setup would open
  ;; nothing. The connected ones are Suggested, each row
  ;; says so, the rest are there to connect, and the models are theirs alone.
  (with-model-listings (:catalog "{
  \"anthropic\": {\"name\": \"Anthropic\", \"npm\": \"@ai-sdk/anthropic\", \"models\": {\"claude-x\": {\"tool_call\": true}}},
  \"acme-labs\": {\"name\": \"Acme Labs\", \"npm\": \"@ai-sdk/openai-compatible\", \"api\": \"https://acme-labs.test/v1\", \"models\": {\"one\": {\"tool_call\": true}}},
  \"barecorp\": {\"npm\": \"@ai-sdk/openai-compatible\", \"api\": \"https://barecorp.test/v1\", \"models\": {\"x\": {\"tool_call\": true}}},
  \"lost\": {\"name\": \"Lost\", \"npm\": \"@nowhere/sdk\", \"models\": {\"q\": {\"tool_call\": true}}}
}")
    (with-stubbed-fdefinition (nle::provider-env-key (provider)
                                (and (string-equal provider "acme-labs") "sk-env"))
      (nle::save-provider-api-key "anthropic" "sk-1")
      (nle::save-provider-api-key "lost" "sk-2")
      (is (equal '("acme-labs" "anthropic") (nle::connected-provider-ids)))
      (let ((rows (nle::connect-picker-rows)))
        (is (equal '("Suggested" "Anthropic" "Acme Labs" "Connect another"
                     "barecorp" "Other provider")
                   (mapcar #'nle::picker-row-label rows)))
        (is (equal '("1 model  connected (auth.json)" "1 model  connected (env)" "1 model")
                   (mapcar #'nle::picker-row-detail (list (second rows) (third rows) (fifth rows))))))
      ;; A saved key nothing answered is not called connected.
      (setf (gethash "anthropic" nle::*key-checks*)
            (list :unchecked "Anthropic not checked: no answer from 127.0.0.1:9 in 5 s"))
      (is (equal "1 model  not checked (auth.json)"
                 (nle::picker-row-detail (second (nle::connect-picker-rows)))))
      (clrhash nle::*key-checks*)
      ;; Nor is one no turn can run on: a providers entry with no base_url
      ;; read `acme connected'.
      (with-stubbed-fdefinition (nle::selection-refusal (provider)
                                  (and (string-equal provider "acme-labs") "providers.acme-labs has no base_url"))
        (is (equal "1 model  needs attention (env)"
                   (nle::picker-row-detail (third (nle::connect-picker-rows)))))
        ;; whatever its key: one typed for it is saved `not checked' over the refusal
        (setf (gethash "acme-labs" nle::*key-checks*)
              (list :unchecked "Acme Labs key was not checked: providers.acme-labs has no base_url"))
        (is (equal "1 model  needs attention (env)"
                   (nle::picker-row-detail (third (nle::connect-picker-rows)))))
        (clrhash nle::*key-checks*))
      (is (equal '("claude-x" "one")
                 (mapcar #'nle::picker-row-label (nle::models-catalog-picker-rows)))))))

(defparameter +other-provider-catalog+
  "{\"groq\":{\"name\":\"Groq\",\"npm\":\"@ai-sdk/groq\",\"models\":{\"llama\":{\"tool_call\":true}}},
    \"acme-labs\":{\"name\":\"Acme Labs\",\"npm\":\"@ai-sdk/openai-compatible\",\"models\":{\"one\":{\"tool_call\":true}}}}"
  "Two models.dev providers in the shape the real catalog has them: Groq, whose
package no lane speaks and whose entry names no api base, and Acme Labs, whose
package rides the chat lane.")

(deftest connect-other-provider-refuses-an-id-no-lane-drives ()
  ;; Without the id check, a provider no lane can dial would be saved
  ;; with no connected row. The id is checked against what the build
  ;; serves before a key is asked for, and a refusal saves nothing and
  ;; returns to the id.
  (with-model-listings (:catalog +other-provider-catalog+ :config "{}")
    (with-frame-model (model 100 30 :session-id "connect-other")
      (with-slash-dialog (dialog model "models" "") "/models opens silently"
        (is (eq :custom-provider (step-after model :up :enter)))
        (is (eq :result (step-after model "groq" :enter)) "refused in place, no key asked for")
        (is-carrying (words (nle::connect-dialog-result-message dialog))
          "groq is in models.dev" "no lane for its API" "providers.groq" "Nothing was saved.")
        (is (null (nle::configured-provider-inventory)) "no secret was stored")
        ;; The words break to the panel's width, the remedy and all, where
        ;; they once ran off its right edge.
        (is-carrying (panel (format nil "~{~a~%~}" (nle::dialog-panel-rows model 60 nil)))
          "models.dev," "providers.groq" "config.jsonc" "saved.")
        ;; Enter returns to the id as it was typed.
        (dialog-keys model :enter)
        (is-shape dialog (nle::connect-dialog-step eq :custom-provider) (.input "groq"))
        ;; An id models.dev does not carry is refused in its own words.
        (nle::picker-dialog-key model #\u :ctrl t)
        (is (eq :result (step-after model "nosuchco" :enter)))
        (is (search "nosuchco is not a models.dev provider"
                    (nle::connect-dialog-result-message dialog)))
        ;; One a lane drives is asked its key.
        (dialog-keys model :enter)
        (nle::picker-dialog-key model #\u :ctrl t)
        (is (eq :api-key (step-after model "acme-labs" :enter)))))))

(deftest connect-other-provider-takes-an-id-a-config-entry-serves ()
  ;; The door the refusal names: providers.groq with an sdk and a base_url is
  ;; what a turn resolves a lane from, so the id step accepts it. An entry
  ;; that names no sdk is refused in the config's own words.
  (with-model-listings (:catalog +other-provider-catalog+
                        :config "{\"providers\": {
  \"groq\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://groq.example/v1\"},
  \"nosdk\": {\"base_url\": \"https://nosdk.example/v1\"}}}")
    (with-frame-model (model 100 30 :session-id "connect-config")
      (flet ((typed (id)
               "The wizard, freshly opened, after ID is typed at Other provider."
               (nle::execute-slash-command model "models" "")
               (dialog-keys model :up :enter)
               (dialog-after model id :enter)
               model.dialog))
        (is (eq :api-key (nle::connect-dialog-step (typed "groq"))) "a configured lane is a lane")
        (let ((dialog (typed "nosdk")))
          (is (eq :result (nle::connect-dialog-step dialog)))
          (is (search "providers.nosdk.sdk" (nle::connect-dialog-result-message dialog))))))))

(deftest provider-id-refusal-says-what-the-build-cannot-serve ()
  (with-model-listings (:catalog +other-provider-catalog+ :config "{}")
    (is-table (id refused) (equal refused (and (nle::provider-id-refusal id) t))
      ("acme-labs" nil) ("groq" t) ("nosuchco" t)))
  ;; with no catalog to read, the words say so rather than call the id unknown
  (with-model-listings (:config "{}")
    (with-saved-globals ((nle::*models-catalog-path* nil))
      (is (search "not loaded yet" (nle::provider-id-refusal "groq"))))))

(deftest models-picker-lists-only-providers-a-lane-drives ()
  ;; A fresh box lists the whole catalog; a row no lane can serve is a pick
  ;; the gateway refuses, so it is not offered.
  (with-model-listings (:catalog +other-provider-catalog+ :config "{}")
    (is (equal '("one")
               (mapcar #'nle::picker-row-label (nle::models-catalog-picker-rows))))))

(deftest the-models-picker-lands-where-it-is-told ()
  ;; a bare /models pick pins this shell's session
  ;; alone, so the setup walk, which sets the organism up, opens the same
  ;; picker landing its pick as the default instead.
  (with-model-listings (:catalog +other-provider-catalog+ :config "{}")
    (with-frame-model (model 100 30 :session-id "pick-lands")
      (let ((landed '()))
        (with-stubbed-fdefinition (nle::slash-models-op (model tokens) (push (cons :session tokens) landed) "session")
          (with-stubbed-fdefinition (nle::slash-default-model-op (model tokens) (push (cons :default tokens) landed) "default")
            (nle::open-models-picker model)
            (funcall (nle::picker-dialog-on-select model.dialog) model '("acme-labs" "one"))
            (nle::open-models-picker model :lands :default)
            (funcall (nle::picker-dialog-on-select model.dialog) model '("acme-labs" "one"))))
        (is (equal '((:default "acme-labs" "one") (:session "acme-labs" "one")) landed))))))

(deftest models-pick-the-gateway-refuses-says-why-in-the-composer-status ()
  ;; The refused pick closed the picker and said nothing: its notice was set
  ;; where no frame carried it, and then a toast nobody saw with popups off.
  ;; The composer status says it now -- the refusal in words, held until the
  ;; composer moves -- and to a walk that chained the picker it is no pick.
  (with-model-listings (:catalog +other-provider-catalog+ :config "{}")
    (with-frame-model (model 100 30 :session-id "pick-refused")
      (let ((picked :unset))
        (nle::open-models-picker model :then (nlk:lambda-ignoring (model tokens)
                                               (setf picked tokens)))
        (let ((on-select (nle::picker-dialog-on-select model.dialog)))
          (with-stubbed-fdefinition (nle::slash-models-op (model tokens)
                                      (values "model selection failed: Gateway HTTP 400 (unknown_provider)" t))
            (is (null (funcall on-select model '("groq" "llama"))) "the status carries it")
            (is (null picked) "no pick for the walk")
            (is (search "groq is in models.dev, but this build has no lane" (nle::status-said model)))
            (is (nle::composer-status-error-p model.composer-status) "a failure, held")
            ;; a provider that has a lane: the gateway's own words
            (funcall on-select model '("acme-labs" "one"))
            (is (search "Gateway HTTP 400" (nle::status-said model))))
          (with-stubbed-fdefinition (nle::slash-models-op (model tokens) "session model: acme-labs one")
            (setf picked :unset)
            (is (equal "session model: acme-labs one" (funcall on-select model '("acme-labs" "one"))))
            (is (equal '("acme-labs" "one") picked))
            (is (null (nle::chat-model-toasts model)) "nothing toasts")))))))

(deftest slash-notice-tells-a-refusal-from-an-answer ()
  ;; No gateway endpoint: the control call refuses, and the second value says
  ;; the text is a refusal.
  (with-frame-model (model 100 30 :session-id "notice-second")
    (is-values (text failed) (nle::slash-models-op model '("groq" "llama"))
      (failed is) ((search "model selection failed" text) is))))

;;; --- claim 18: a cell command's own dialog ------------------------------
;;; The text is what every caller reads. An answer may also carry a docked
;;; picker (GATEWAY.LISP LIST-DIALOG), and the shell that ran the command --
;;; the one holding the terminal -- opens it: the cell describes the list,
;;; the shell paints it. Enter hands the highlighted row's value to the
;;; composer, which is how a picker of things to name lands in a draft.

(defun register-picker-command (&key trigger)
  "Register the throwaway command the dialog gates drive: /pick answers a
line AND a three-row picker, opened by TRIGGER too when one is given."
  (nle:register-command
   "picker-pack" "pick"
   (nlk:lambda-ignoring (args session-id)
     (values "pick something"
             (nle:list-dialog
              :title "Pick (2)" :context "two of them" :searchable t
              :search-placeholder "Type to filter picks…"
              :empty-label "Nothing to pick"
              :hint "esc close  enter pick"
              :rows '((:label "alpha" :detail "the first one" :value "$alpha ")
                      (:key "user   " :label "beta" :detail "the second one"
                       :value "$beta " :mark "• " :mark-tone :success)))))
   :description "Answers with a picker"
   :trigger trigger))

;;; --- claim 19: a dialog's tab strip -------------------------------------------
;;; A list whose rows are bucketed says the bucket once, in the tab row
;;; (LIST-DIALOG's :TABS), instead of once per row: the strip paints every tab
;;; with the rows behind it, Tab walks the strip, and the rows the selected tab
;;; shows are the ones whose KEY is its VALUE. The strip is chrome the dialog
;;; record does not carry: OPEN-PICKER-DIALOG's own note says why a live image
;;; registers it in a table instead (*PICKER-TABS*, beside *PICKER-ARROW-HANDLERS*).

(defparameter +scope-tabs+ '((:label "all" :count 3 :value nil)
                             (:label "user" :count 2 :value "user")
                             (:label "project" :count 1 :value "project"))
  "The tab strip /scope's picker declares: its rows sit behind the last two.")

(defun register-tabs-command ()
  "Register the throwaway command the tab-strip gate drives: /scope answers
one line and a picker whose three rows sit behind three tabs."
  (nle:register-command
   "scope-pack" "scope"
   (nlk:lambda-ignoring (args session-id)
     (values "scope: 3 rows"
             (nle:list-dialog
              :title "Scope" :context "3 rows" :tabs +scope-tabs+
              :rows '((:key "user" :label "alpha" :detail "the first one"
                       :value "$alpha ")
                      (:key "user" :label "beta" :detail "the second one"
                       :value "$beta ")
                      (:key "project" :label "gamma" :detail "the third one"
                       :value "$gamma ")))))
   :description "Answers with a tabbed picker"))

;;; --- claim 20: a row's keys run lines -----------------------------------------
;;; A row may name, in its VALUE's place, the slash lines its keys run
;;; (LIST-DIALOG's :RUN): Space moves the thing the row is and the list is
;;; read again, the cursor on that row wherever it moved; Enter opens its record over the list, whose
;;; Esc comes back to the list read again; and a line whose record is gone
;;; gives way to the list under it (the shell's ANSWER-GESTURE).

(defun register-rows-command (items)
  "Register the throwaway command the row-key gate drives over ITEMS, a
cons whose car is the rows as (NAME . ON) conses: /rows lists them, the on
ones first, /rows flip NAME moves one, /rows show NAME is its record, whose
last row runs /rows drop NAME."
  (flet ((find-item (name)
           (or (assoc name (car items) :test #'equal) (error "no row ~a" name))))
    (nle:register-command
     "rows-pack" "rows"
     (lambda (args session-id &aux (words (nlk:split-words args)) (name (second words)))
       (declare (ignore session-id))
       (nlk:dispatch (first words) equal
         (nil (values "rows"
                      (nle:list-dialog
                       :title "Rows"
                       :rows (loop for (name . on) in (append (remove-if-not #'cdr (car items))
                                                              (remove-if #'cdr (car items)))
                                   collect (list :label name :detail (if on "on" "off")
                                                 :run (list :enter (format nil "/rows show ~a" name)
                                                            :space (format nil "/rows flip ~a" name)))))))
         ("flip" (let ((item (find-item name)))
                   (setf (cdr item) (not (cdr item)))
                   (format nil "~a flipped" name)))
         ("show" (values name
                         (nle:list-dialog
                          :title "Row" :context name
                          :rows (list (list :label "state" :detail (if (cdr (find-item name)) "on" "off"))
                                      (list :label "Drop" :run (list :enter (format nil "/rows drop ~a" name)))))))
         ("drop" (setf (car items) (cl:remove (find-item name) (car items)))
                 (format nil "~a dropped" name))))
     :description "Answers with rows whose keys run lines")))

(deftest dialog-tabs-cross-the-wire ()
  ;; A shell that is not this process reads the answer's JSON: the strip
  ;; encodes through DIALOG-JSON and decodes through DIALOG-TABS,
  ;; so the tabs a remote shell paints are the ones the cell declared.
  (let ((answer (nle:list-dialog
                 :title "Scope" :context "3 rows" :tabs +scope-tabs+
                 :rows '((:key "user" :label "alpha" :detail "the first one"
                          :value "$alpha " :verb "leave")
                         (:key "user" :label "beta")))))
    (is (equal +scope-tabs+ (nle::dialog-tabs (nle::dialog-json answer))))
    (let ((rows (nle::json-value (nle::dialog-json answer) :array "rows")))
      (is (equal "user" (nlk:json-value (aref rows 0) :string "key")))
      ;; a row's own button word crosses; a row that names none carries none
      (is (equal '("leave" nil) (map 'list (lambda (row) (nlk:json-value row :string "verb")) rows))))))

(deftest dialog-row-options-cross-the-wire ()
  ;; A row's OPTIONS -- the picks its line offers, the one in use marked --
  ;; encode through DIALOG-JSON beside the row, each with its own line; a
  ;; row that offers none carries none.
  (let ((rows (nle::json-value
               (nle::dialog-json
                (nle:list-dialog
                 :rows (list (list :label "Speaks with" :run '(:enter "/voice speaks")
                                   :options (list (list :label "bf_isabella" :detail "kokoro" :mark "●"
                                                        :run '(:enter "/voice speaks bf_isabella"))
                                                  (list :label "af_heart" :run '(:enter "/voice speaks af_heart"))))
                             (list :label "Hearing"))))
               :array "rows")))
    (is (equal '(("bf_isabella" "kokoro" "●" "/voice speaks bf_isabella") ("af_heart" "" "" "/voice speaks af_heart"))
               (map 'list (lambda (option)
                            (list (nlk:json-value option :string "label") (nlk:json-value option :string "detail")
                                  (nlk:json-value option :string "mark") (nlk:json-value option :string "run" "enter")))
                    (nlk:json-value (aref rows 0) :array "options"))))
    (is (null (nlk:json-value (aref rows 1) :array "options")) "a row with no picks carries none")))

(deftest dialog-picture-crosses-the-wire ()
  ;; The picture encodes through DIALOG-JSON and decodes through
  ;; DIALOG-PICTURE: the code a shell in another process paints is
  ;; the one the cell drew.
  (flet ((crossed (picture)
           (nle::dialog-picture
            (nle::dialog-json (nle:list-dialog :title "Link" :picture picture)))))
    (is-table (picture) (equal picture (crossed picture))
      ('(:rows ("101" "010") :label "The code"))
      ;; A palette and the place beside the list cross with it.
      ('(:rows ("012" "210") :label "The grid" :palette ("" "context-tools" "thought") :beside t))
      (nil "a dialog with none carries none"))))

;;; --- /cells: catalog and rendering ------------------------------

(deftest slash-catalog-carries-cells (let ((entry (nle::find-slash-command "cells"))))
  (is-present entry "/cells is in the catalog"
    (is (string= "/cells" (nle::slash-command-usage entry))))
  (is (null (nle::find-slash-command "persist")) "/persist is gone with the journal"))

;;; --- /generations: the sealed generations, and the way back to one ----------

(deftest slash-catalog-carries-generations (let ((entry (nle::find-slash-command "generations"))))
  (is-present entry "/generations is in the catalog"
    (is (string= "/generations [N]" (nle::slash-command-usage entry)))
    (is (eq :args (nle::slash-command-dispatch entry)) "the shell hands it the typed tail")
    (is (eq 'nle::headless-generations (nle::slash-command-headless entry)))
    (is (null (nle::slash-command-session entry)) "it acts on no session")))

(deftest headless-generations-lists-and-takes-a-number ()
  ;; The scratch home of the hermetic posture holds no manifest.
  (is (search "No generation sealed" (nle::headless-generations "" nil)) "the list, bare")
  (is (search "no generation 7" (nle::headless-generations "7" nil)) "a number nothing sealed")
  (is (search "/generations [N [go|keep|discard]]" (nle::headless-generations "x y" nil)) "anything else is the usage"))

(deftest slash-cells-renders-the-folder-report ()
  (is (search "no cells" (nle::render-cells '())))
  (is-carrying (text (nle::render-cells
                      (list (nlk::make-cell :name "mike" :kind :layer :state :loaded
                                             :defined '((:function . nle::slash-cells-probe))
                                             :overrides '((:function . nle::compose-frame)))
                            (nlk::make-cell :name "broken" :state :failed
                                             :failures '((nil . "no such package")))
                            (nlk::make-cell :name "partial" :kind :layer :state :failed
                                             :defined '((:function . nle::slash-cells-probe))
                                             :failures '(("_dm-rev_.lisp" . "the discord channel is not running"))))))
    ("cell mike: layer, 1 definition, overrides compose-frame"
     "a loaded layer renders its kind, count and overrides")
    ("cell broken: FAILED no such package" "a folder that failed as a unit renders its condition")
    (:absent "cell broken: library" "and no summary line: its kind and count describe nothing")
    ("cell partial: layer, 1 definition" "a layer with a failed file still renders what loaded")
    ("cell partial: FAILED _dm-rev_.lisp: the discord channel is not running"
     "and the FAILED line names the file"))
  (nlk:note-standing "channels.discord" "discord: refused — no token" :error)
  (is-carrying (text (nle::render-cells
                      (list (nlk::make-cell :name "kit" :kind :peripheral :state :loaded
                                             :started-p t))))
    ("cell kit: peripheral, 0 definitions, started" "a started peripheral says so")
    ("discord: refused — no token" "and the board's lines follow the records")))

;;; --- claim 11: the headless surface --------------------------------------
;;; NLE:SLASH is the catalog without a shell: a channel cell, a cron job
;;; or an eval snippet hands it one line and reads the answer a shell would
;;; have shown. Builtins with a headless arm land on the canonical surfaces;
;;; a cell's command runs through its own arm; the rest answer that they
;;; need a shell; an unknown name says so.

(deftest slash-headless-runs-the-catalog-against-a-session (with-temp-store ())
  (flet ((say (line) (nle:slash line :session-id "slash-headless")))
    (with-saved-globals (nle::*registered-commands*)
      (do-turns (make-test-session "slash-headless") 2)
      (nle:register-command "probe-pack" "probe"
                            (lambda (args session-id) (format nil "probe ~a for ~a" args session-id))
                            :description "Echo the tail" :argument-hint "text")
      (nle:register-command "probe-pack" "help" (constantly "shadowed")
                            :description "shadows a builtin")
      (let ((catalog (nle:slash-catalog)))
        (is (equal '("help" "clear" "rename" "undo" "redo" "models"
                     "evict" "context" "status" "usage"
                     "cells" "generations" "doctor" "backup" "profile" "import" "think" "gc" "probe")
                   (mapcar (lambda (entry) (getf entry :name)) catalog)))
        (is (equal "/probe text" (catalog-field "probe" :usage catalog))))
      (is-carrying (help (say "/help"))
        ("/models [provider] [model] | aux [provider] [model | default] | decider [id | none] | backups | list | refresh — Show or change the main, auxiliary and decider models"
         "/help renders the catalog, usage first")
        (:absent "/model-aux" "the auxiliary model is /models' own")
        ("/probe text — Echo the tail")
        (:absent "/sessions" "a shell-only command is not offered")
        (:absent "/web" "the link carries the operator token: never offered to a room"))
      (is-table (answer line) (equal answer (say line))
        ("probe hello there for slash-headless" "/probe hello there")
        ("/sessions needs an interactive shell" "/SESSIONS")
        ("/web needs an interactive shell" "/web")
        ("unknown command /nope; /help lists them" "/nope"))
      (is (search "no cells" (nle:slash "/cells")))
      (is (search "gc time" (nle:slash "/gc")) "/gc needs no session")
      (is (equal "Usage: /gc [reset]" (nle:slash "/gc bogus")))
      (let ((second-turn (nlk:session-head-turn-id "slash-headless")))
        (is (equal "rewound to before: turn 2 input" (say "/undo")))
        (is (not (equal second-turn
                        (nlk:session-head-turn-id "slash-headless"))))
        (is (equal (format nil "redone: the head is back on ~a" second-turn) (say "/redo")))
        (is (equal second-turn (nlk:session-head-turn-id "slash-headless")))
        (is (equal "cleared: the session is back at its start; /redo returns" (say "/clear")))
        (is (null (nlk:session-head-turn-id "slash-headless")))
        (is-table (answer line) (equal answer (say line))
          ("nothing to undo" "/undo")
          ("nothing to clear: the session is at its start" "/new"))
        (is (equal (format nil "redone: the head is back on ~a" second-turn) (say "/redo"))))
      (let ((report (say "/models")))
        (is (search "provider: " report) "bare /models reports the target")
        (is (search "/models <provider> <model> changes it" report)))
      (dolist (line '("/models a b c" "/models aux a b c" "/models decider a b c"))
        (is (equal "usage: /models [provider] [model] | aux [provider] [model | default] | decider [id | none] | list [provider]"
                   (say line)) line))
      (let ((aux (say "/models aux")))
        (is (uiop:string-prefix-p "auxiliary model: " aux))
        (is (search "/models aux <provider> <model> changes it" aux)))
      (is (equal "unknown command /model-aux; /help lists them" (say "/model-aux")))
      (let ((think (say "/think")))
        (is (or (search "ladder:" think)
                (search "offers no reasoning control" think))))
      (is (signals-error error (say "/think bogus")))
      (is (equal nle::*evict-usage-hint*
                 (say "/evict 0")))
      (is-table (in) (signals-error error in) ((nle:slash "/undo" :session-id nil))
        ((nle:slash "/undo" :session-id "s-none")) ((nle:slash "hello"))))))

;;; --- claim 14: /web is `nodecode web' on a shell ---------------------------

(deftest web-slash-answers-what-nodecode-web-answers (with-chat-model (model "web-slash"))
  ;; One answer, two shells: the verb prints WEB-TEXT's answer, the slash
  ;; posts it to the notice lane off the loop, a refusal as an error.
  (let ((notices '()) (asked '()))
    (with-saved-globals ((nle::*system-notice-hook*
                          (lambda (text error-p) (push (cons text error-p) notices))))
      (with-stubbed-fdefinition (nle::web-text (words)
                                 (push words asked)
                                 (if words
                                     (values "not running" 1 :error)
                                     (values "http://127.0.0.1:1/web/#t=x" 0 :output)))
        (is (null (nle::execute-slash-command model "web" "")) "the answer lands as a notice")
        (is (await (:timeout 5) notices))
        (is (null (nle::execute-slash-command model "web" "--help")))
        (is (await (:timeout 5) (second notices)))))
    (is (equal '(("--help") ()) asked) "the words reach WEB-TEXT as the verb's do")
    ;; A refusal is an error notice, the link is not.
    (is (equal '(("not running" . t) ("http://127.0.0.1:1/web/#t=x")) notices))))

(deftest web-names-a-folder-the-organism-has-not-loaded-as-installed
    (with-temp-trees ((folder "web-unloaded")))
  ;; `not installed: run /setup … mark web' over a folder /setup no longer
  ;; offered, because the folder is there: the organism has not loaded it.
  (with-stubbed-fdefinitions ((nle::organism-port () 4321)
                              (nle::gateway-configured-operator-token () "t")
                              (nlk:http (&rest request) (values "" 404)))
    (with-saved-globals ((nlk::*cells-directory* folder))
      (is-carrying (text (nle::web-text '())) "is not installed" "/cells")
      (ensure-directories-exist (merge-pathnames "nodecode-web/" folder))
      (is-carrying (text (nle::web-text '())) "has not loaded it" "restart" (:absent "/setup")))))

;;; --- claim 11: /context rendering ------------------------------------------

(defmacro is-context-report ((&rest arguments) &body checks)
  "IS-CARRYING's CHECKS over the /context report RENDER-CONTEXT-REPORT draws
from ARGUMENTS, the text bound as TEXT."
  `(is-carrying (text (nle::render-context-report ,@arguments)) ,@checks))

(deftest context-report-renders-occupancy-window-and-meters ()
  (is-context-report (:label "deepseek/deepseek-v4-flash (max)"
                      :session-id "s-context"
                      :usage (list :input 1200 :output 42 :cached 11280
                                   :reasoning 30 :rounds 2)
                      :session-usage (list :input 34100 :output 12000 :cached 30000
                                           :reasoning 3400 :cost-known t :cost 0.0042)
                      :context-usage (list :input 1200 :cached 11280 :cache-write 800)
                      :window 1000000 :window-grade "configured")
    ("context · deepseek/deepseek-v4-flash (max) · s-context" "identity and session lead")
    ("occupancy: 13.3k / 1M (1.3%) · free 986.7k (configured)"
     "occupancy is the newest round's prompt over the window")
    ("turn: ↑1.2k c90.4% ↓42 r30 · 2 rounds" "the turn meter sums its rounds")
    ("session: ↑34.1k c46.8% ↓12k r3.4k · $0.0042" "the session bill carries its known cost")))

(deftest context-report-degrades-when-the-window-is-unknown ()
  (is-context-report (:label "x/y" :context-usage (list :input 500 :cached 1500))
    ("occupancy: 2k · window unknown" "no window, no percent, still the prompt's size"))
  (is-context-report (:label "x/y")
    ("occupancy: no provider round yet" "an empty session says so")
    (:absent "turn:" "no meter lines without usage")))

(deftest context-report-splits-the-estimate-into-its-parts ()
  ;; The occupancy an operator reads is not always history: the parts line
  ;; names the standing prompt, the retained turns, and what /evict could
  ;; still drop, so `/evict says nothing' never contradicts `/context says
  ;; 25k'.
  (is-context-report (:label "x/y" :context-estimate 20461
                      :context-parts (list :prompt 17600 :tools 2500 :standing 20100
                                           :history 361 :kept 1 :turns 95
                                           :evictable 0))
    ("parts: 20.1k standing (prompt 17.6k · tools 2.5k) · 361 history (newest 1 of 95 turns kept) · evictable: none"
     "the standing prompt, the retained history and the evictable count"))
  (is-context-report (:label "x/y" :context-estimate 9000
                      :context-parts (list :prompt 4000 :tools 1000 :standing 5000
                                           :history 4000 :kept 4 :turns 9
                                           :evictable 2))
    ("evictable: 2 turns" "turns /evict can still drop are counted, not hidden"))
  (is-context-report (:label "x/y" :context-usage (list :input 500 :cached 1500))
    (:absent "parts:" "a measured round has no estimate to break down")))

(deftest context-report-estimates-occupancy-without-a-round ()
  ;; No provider round is not an empty context: the estimate stands in for
  ;; the measured prompt, the `~' marks it as an estimate, and the
  ;; provenance names which number the lane is reading.
  (is-context-report (:label "x/y" :context-estimate 23100
                      :window 200000 :window-grade "configured")
    ("occupancy: ~23.1k / 200k (11.6%) · free 176.9k (configured) · no provider round yet, estimated from the record"
     "the estimate renders in the measured shape, marked"))
  (is-context-report (:label "x/y" :context-estimate 23100 :window-floor 366871)
    ("occupancy: ~23.1k · window unknown · proven floor 366.9k (at most 6.3%) · no provider round yet, estimated from the record"
     "with no window the proven floor is what the lane reads against")
    (:absent "turn:" "an estimated occupancy is no license for meter lines")))

(deftest context-report-names-the-proven-floor-under-an-unknown-window ()
  ;; A window no source knows is a fact about the window, never a license to
  ;; read the lane as zero: the durable floor is named, the occupancy's
  ;; share of it is a bound, and a round's own measured prompt keeps its
  ;; plain number (the tenth-precision percent still resolves it).
  (is-context-report (:label "x/y" :context-usage (list :input 500 :cached 1500)
                      :window-floor 366871)
    ("occupancy: 2k · window unknown · proven floor 366.9k (at most 0.5%)"
     "the floor is named and the share is a bound"))
  (is-context-report (:label "x/y" :context-estimate 400000 :window-floor 366871)
    ("occupancy: ~400k · window unknown · proven floor 366.9k"
     "an occupancy past the floor carries no bound")
    (:absent "at most" "a share over 100% is no bound at all"))
  (is-context-report (:label "x/y" :window-floor 366871)
    ("occupancy: no provider round yet · proven floor 366.9k"
     "even an empty session names the window evidence it has")))

(deftest context-estimate-reads-the-durable-record (with-temp-store ())
  ;; The estimate's one source is the session's own record: turns retained,
  ;; nothing invented, and no estimate at all for a session the store has
  ;; never seen (NIL is what the readout reports; zero is not).
  (seed-user-inputs "slash-context-estimate" (list "first input" "second input"))
  (multiple-value-bind (estimate parts)
      (nle::session-context-estimate "slash-context-estimate"
                                     "openai-completions" "gpt-test")
    (is (integerp estimate) "a retained record estimates to a number")
    (is (plusp estimate) "and that number is never zero")
    ;; The second value is the number's own breakdown: a readout shows it
    ;; so an occupancy /evict refuses to touch reads as standing prompt.
    (is (equal estimate (+ (getf parts :standing) (getf parts :history))))
    (is (equal (getf parts :standing) (+ (getf parts :prompt) (getf parts :tools))))
    (is (<= (getf parts :kept) (getf parts :turns)))
    (is (integerp (getf parts :evictable))))
  (is (null (nle::session-context-estimate "slash-context-absent" nil nil))))

;;; --- /context as a picture ------------------------------------------------

(defun context-test-breakdown (&key (units '()))
  "A breakdown CONTEXT-GRID-PICTURE reads: 400 tokens of standing prompt,
200 of tools, 1400 of history, over a WINDOW of 400 cells of 10 tokens."
  (list :total 2000 :estimated nil :units units
        :parts (loop for (key tokens) in '((:prompt 400) (:tools 200) (:index 0) (:notes 0)
                                           (:history 1400))
                     collect (list :key key :tokens tokens :items '()))))

(deftest context-grid-fills-the-window-in-part-order ()
  ;; One cell is 1/400 of the window: the parts fill it in order, the free
  ;; room follows, and past the 85% line the free cells are the zone's.
  ;; A part's own view mutes the others.
  (flet ((char-of (entry) (nle::context-palette-char entry)))
    (let ((rows (nle::context-grid-picture (context-test-breakdown) 4000)))
      (is (= 10 (length rows)) "ten pixel rows, five text rows")
      (is (every (lambda (row) (= 40 (length row))) rows) "forty cells across")
      (is (every (lambda (c) (char= c (char-of "context-prompt"))) (first rows)) "the prompt's 40 cells open it")
      (is (string= (make-string 20 :initial-element (char-of "context-tools")) (subseq (second rows) 0 20)))
      (is (char= (char-of "thought") (char (third rows) 0)) "history follows in its tone")
      (is (char= (char-of "context-free") (char (nth 5 rows) 0)) "then the free room")
      (is (char= (char-of "context-zone") (char (nth 8 rows) 39)) "the zone past 340 cells")
      (is (every (lambda (c) (char= c (char-of "context-zone"))) (nth 9 rows))))
    (let ((rows (nle::context-grid-picture (context-test-breakdown) 4000 :focus :tools)))
      (is (char= (char-of "context-other") (char (first rows) 0)) "another part's cells are muted")
      (is (char= (char-of "context-tools") (char (second rows) 0)) "the part's own keep their tone"))
    (let ((rows (nle::context-grid-picture (context-test-breakdown) nil)))
      ;; With no window there is no line to draw.
      (is (notany (lambda (row) (find (char-of "context-zone") row)) rows)))))

(deftest context-dialog-wears-its-window-state-and-draws-one-row ()
  ;; A room's card reads the dialog's TONE as its edge and the picture's
  ;; STRIP as its gauge: a tenth of the window a cell, the parts in order,
  ;; the zone's cells past the eviction line; the free row stands only
  ;; where the window is known.
  (flet ((dialog (window) (nle::dialog-json (nle::context-dialog (context-test-breakdown) window
                                                                   "summary" :muted nil nil)))
         (labels-of (json) (map 'list (lambda (row) (gethash "label" row)) (gethash "rows" json))))
    (let* ((json (dialog 4000))
           (strip (gethash "strip" (gethash "picture" json))))
      (is (equal "success" (gethash "tone" json)) "half the window is fine")
      (is (= nle::+context-strip-cells+ (length strip)) "one cell a tenth of the window")
      (is (eql (char strip 0) (nle::context-palette-char (third (assoc :prompt nle::+context-parts+)))) "the strip fills in part order")
      (is (eql (char strip 9) (nle::context-palette-char "context-zone")) "past the eviction line the free cells are the zone's")
      (is (find "free" (labels-of json) :test #'string=) "a known window has free room"))
    (is (equal "warn" (gethash "tone" (dialog 2600))) "three quarters warms")
    (is (equal "danger" (gethash "tone" (dialog 2300))) "past the eviction line is danger")
    (let ((json (dialog nil)))
      (is (null (gethash "tone" json)) "no window wears no state")
      (is (null (find "free" (labels-of json) :test #'string=)) "no window, no free row"))))

(deftest context-history-rows-evict-through-each-turn-but-the-newest ()
  ;; Each retained turn is a row; Enter evicts through it, the selected
  ;; row says what that frees, and the newest turn says why it stays.
  (let* ((units (list (list :turn-id "a" :number 4 :start 10 :end 20 :tokens 300 :prompt "fix it" :terminal t)
                      (list :turn-id "b" :number 5 :start 21 :end 30 :tokens 500 :prompt (format nil "~%run the tests~%again") :terminal t)
                      (list :turn-id "c" :number 6 :start 31 :end 40 :tokens 200 :prompt "" :terminal t)))
         (rows (nle::context-history-rows (context-test-breakdown :units units) 4000)))
    (is (equal '("t4" "t5" "t6") (mapcar (lambda (row) (getf row :key)) rows)) "numbered as the session counts them")
    ;; The first line of each turn's prompt.
    (is (equal '("fix it" "run the tests" "(no prompt in the window)") (mapcar (lambda (row) (getf row :label)) rows)))
    (is (equal '(:enter "/evict through a") (getf (first rows) :run)))
    (is (equal '(:enter "/evict through b") (getf (second rows) :run)))
    (is (null (getf (third rows) :run)) "the newest turn never goes")
    (is (search "−300 → 42.5%" (getf (first rows) :suffix)) "what Enter frees and what is left")
    (is (search "−800 → 30.0%" (getf (second rows) :suffix)))
    (is (search "kept: newest" (getf (third rows) :suffix))))
  ;; A turn's late fact is a unit after the next turn began: the turn is one
  ;; row, standing where its last unit does, and evicting through it frees
  ;; every unit up to there.
  (let* ((units (list (list :turn-id "a" :number 1 :start 1 :end 5 :tokens 100 :prompt "first" :terminal t)
                      (list :turn-id "b" :number 2 :start 6 :end 9 :tokens 200 :prompt "second" :terminal t)
                      (list :turn-id "a" :number 1 :start 10 :end 10 :tokens 10 :prompt "" :terminal t)
                      (list :turn-id "c" :number 3 :start 11 :end 20 :tokens 50 :prompt "third" :terminal t)))
         (rows (nle::context-history-rows (context-test-breakdown :units units) 4000)))
    (is (equal '("t2" "t1" "t3") (mapcar (lambda (row) (getf row :key)) rows)) "ordered by where each turn ends")
    (is (search "110" (getf (second rows) :detail)) "its size sums its units")
    (is (search "−310" (getf (second rows) :suffix)) "through t1 frees every unit up to its last")))

(deftest context-summary-names-the-line-and-warns-past-it ()
  (is (equal '("x/y · learned window · evicts at 85% (170k), down to 50%" :muted)
             (multiple-value-list (nle::context-summary "x/y" "learned" 200000 88500))))
  ;; Past the line the next request evicts, and says so in the danger tone.
  (is (eq :danger (nth-value 1 (nle::context-summary "x/y" "learned" 200000 171000))))
  (is (search "window unknown" (nle::context-summary "x/y" "unknown" nil 88500))))

(deftest context-reads-the-estimate-once-an-eviction-outdates-the-round (with-temp-store ())
  ;; The newest round's measured prompt is the occupancy until an eviction
  ;; lands after it: then it counts turns the window no longer holds, and the
  ;; record's estimate stands, marked, until the next round reports.
  (let ((sid (nle:session-id (do-turns (make-test-session "ctx-stale") 2))))
    (with-durable-turn (turn sid :command-id "ctx-stale-usage" :input "measured" :complete t)
      (nlk:record-turn-usage turn :model "ctx-model" :input-tokens 7000 :output-tokens 10))
    (flet ((context () (gethash "context" (nth-value 1 (nle:slash "/context" :session-id sid)))))
      (is (equal "7k" (context)) "a measured round is the occupancy")
      (is (search "Evicted" (nle:slash "/evict 50" :session-id sid)))
      (is (uiop:string-prefix-p "~" (context)) "after an eviction the estimate stands, marked"))))

(deftest context-answers-its-grid-and-opens-each-part (with-temp-store ())
  ;; /context answers its text and the picker a shell paints: the grid beside
  ;; the parts, each opening its own view, and history's turns, where Enter
  ;; evicts through one.
  (let ((sid (nle:session-id (do-turns (make-test-session "ctx-grid") 3))))
    (multiple-value-bind (text dialog) (nle:slash "/context" :session-id sid)
      (is (search "occupancy:" text) "the text answer stands for a headless caller")
      (is-let* ((picture (gethash "picture" dialog))
                (labels (map 'list (lambda (row) (gethash "label" row)) (gethash "rows" dialog))))
        ((gethash "beside" picture) is "the grid stands beside the list")
        ((length (gethash "rows" picture)) = 10)
        ((find "standing prompt" labels :test #'string=) is)
        ((find "history · 3 turns" labels :test #'string=) is "history counts its turns"))
      (is (null (find "free" (map 'list (lambda (row) (gethash "label" row)) (gethash "rows" dialog)) :test #'string=)) "no window, no free row: the grid's whole is what is in it, and free would read a measured zero")
      (is (null (gethash "tone" dialog)) "no window, no state to wear"))
    (multiple-value-bind (text dialog) (nle:slash "/context history" :session-id sid)
      (is (search "t1" text) "the part's items read as lines too")
      (let ((rows (coerce (gethash "rows" dialog) 'list)))
        (is (= 3 (length rows)))
        (is (search "/evict through " (gethash "enter" (gethash "run" (first rows)))))
        (is (null (gethash "run" (third rows))) "the newest turn runs nothing")
        (let ((line (gethash "enter" (gethash "run" (second rows)))))
          ;; Evicting through the second turn takes both.
          (is (equal "Evicted 2 turns (66% of history context) · 2 messages retained"
                     (nle:slash line :session-id sid)))
          ;; A turn already gone says so.
          (is (search "not in the window" (nle:slash line :session-id sid))))))
    ;; The list reads the window again.
    (is (= 1 (length (gethash "rows" (nth-value 1 (nle:slash "/context history" :session-id sid))))))
    (is (equal nle::+context-usage+ (nle:slash "/context everything" :session-id sid)))
    (is (search "no provider round" (nle:slash "/context time" :session-id sid)))))

(deftest bare-profile-opens-the-homes-and-a-verb-still-answers-text ()
  ;; The roster was a paragraph; it is a picker now, and Enter on a row
  ;; asks the kernel for the plan rather than deciding anything itself.
  (is-let* ((entry (nle::find-builtin-slash-command "profile")))
    (entry is "the command is in the catalog")
    (entry.dispatch eq :args "the shell hands it the model") (entry.op eq 'nle::profile-slash-op)
    ((search "switch" entry.usage) is "the usage names the new verb")))

(deftest the-profiles-picker-is-a-roster-you-can-walk ()
  (nlk:profile-create "coder" :clone :blank :description "for code")
  (nlk:profile-use "coder")
  (let ((rows (nle::profile-picker-rows)))
    (is (= (1+ (length (nlk:profiles))) (length rows)) "one row per profile, and the row that makes another")
    (let ((row (find "coder" rows :key #'nle::picker-row-label :test #'string=)))
      (is row "the new profile has a row")
      (is (equal "coder" (nle::picker-row-value row)) "whose value is the name Enter switches to")
      (is-carrying (detail (nle::picker-row-detail row))
        ("boots by default" "the roster's facts ride along") ("for code" "the description too")
        (is (search (string-right-trim "/" (namestring (nlk:profile-home "coder"))) detail))))
    ;; The create row rides last: the homes read first, the act after them.
    (let ((action (car (last rows))))
      (is (eql nle::+new-profile-action+ (nle::picker-row-value action)) "the last row is the create gesture, not a home")
      (is (string= "+" (nle::picker-row-mark action)) "whose mark says the row acts")
      (is (search "home of its own" (nle::picker-row-detail action)) "and what it makes"))
    ;; The mark column is the roster's star, a blank, or the act's plus.
    (is (every (lambda (row)
                 (member (nle::picker-row-mark row) '("*" " " "+") :test #'string=))
               rows) "every mark is one of the three")))

(deftest the-roster-makes-a-home-from-its-own-row ()
  ;; The easy way the roster grew: /profile, ENTER on its last row, a name,
  ;; ENTER — no verb typed. The home lands through the same NLK:PROFILE-CREATE
  ;; the `create' verb calls, so the clone default and every refusal are that
  ;; function's, and the roster comes back seated on it.
  (with-frame-model (model 100 30 :session-id "profile-new-row")
    (nle::open-profiles-picker model)
    (let* ((dialog (nle::chat-model-dialog model))
           (rows (nle::picker-visible-rows dialog)))
      (is (eq :profiles (nle::picker-dialog-kind dialog)) "the roster is open")
      (is (eql nle::+new-profile-action+ (nle::picker-row-value (car (last rows)))) "the create row is the last thing on it")
      (setf (nle::picker-dialog-selection dialog) (1- (length rows)))
      (nle::picker-dialog-key model :enter)
      (setf dialog (nle::chat-model-dialog model))
      (is (equal "New profile" (nle::picker-dialog-input-label dialog)) "ENTER on the create row opens the name field")
      (is (null (nle::picker-dialog-rows dialog)) "in place of the list")
      (mapc (lambda (ch) (nle::picker-dialog-key model ch)) (coerce "writer" 'list))
      (is (equal "writer" (nle::picker-dialog-input dialog)) "the field takes the name")
      (nle::picker-dialog-key model :enter)
      (setf dialog (nle::chat-model-dialog model))
      (is (uiop:directory-exists-p (nlk:profile-home "writer")) "ENTER makes the home")
      (is (probe-file (merge-pathnames "profile.jsonc" (nlk:profile-home "writer"))) "with the manifest PROFILE-CREATE writes")
      (is (member "writer" (mapcar #'nlk:profile-record-name (nlk:profiles)) :test #'string=) "and the roster lists it")
      (is (and (eq :profiles (nle::picker-dialog-kind dialog)) (eq t (nle::picker-dialog-searchable dialog))) "the roster is back")
      (is (equal "writer" (nle::picker-row-value (nth (nle::picker-dialog-selection dialog) (nle::picker-visible-rows dialog)))) "seated on the new home, where the next ENTER takes this shell")
      (is (search "created writer" (nle::status-said model)) "and the notice says what happened"))))

(deftest the-create-field-keeps-the-name-and-answers-a-refusal ()
  ;; A name already taken, or one that is not a profile name, leaves the field
  ;; open with the words in it and the reason under it — the gesture was right,
  ;; the name needs fixing — and ESC is the way back that makes nothing.
  (nlk:profile-create "taken" :clone :blank)
  (with-frame-model (model 100 30 :session-id "profile-refusals")
    (flet ((open-field (name)
             "The gesture twice over: the roster, its create row, a name."
             (nle::open-profiles-picker model)
             (let ((dialog (nle::chat-model-dialog model)))
               (setf (nle::picker-dialog-selection dialog)
                     (1- (length (nle::picker-visible-rows dialog))))
               (apply #'dialog-keys model :enter (append (coerce name 'list) '(:enter)))
               (nle::chat-model-dialog model))))
      (let ((dialog (open-field "taken")))
        (is (equal "New profile" (nle::picker-dialog-input-label dialog)) "a name already taken leaves the field open")
        (is (equal "taken" (nle::picker-dialog-input dialog)) "with the name still in it")
        (is (search "already exists" (nle::picker-dialog-input-context dialog)) "and the reason under it"))
      (let ((dialog (open-field "WEB")))
        (is (equal "New profile" (nle::picker-dialog-input-label dialog)) "so does a name that is not one")
        (is (search "not a profile name" (nle::picker-dialog-input-context dialog)) "with that reason"))
      (is (eq :profiles (nle::picker-dialog-kind (dialog-after model :escape))) "ESC in the field is the roster, not the end of the dialog")
      (is (equal '("default" "taken") (mapcar #'nlk:profile-record-name (nlk:profiles))) "and nothing was made"))))

(deftest the-picker-hands-a-pick-to-the-performer-and-never-execs ()
  ;; Enter reaches PROFILE-SWITCH through PROFILE-COMMAND, which hands the
  ;; plan to whoever installed a performer. The callback itself execs
  ;; nothing: the shell does that on the way out.
  (with-stubbed-fdefinition (nlk:organism-binary () "/opt/nodecode/current/nodecode")
    (nlk:profile-create "coder" :clone :blank)
    (let ((parked '()))
      (with-saved-globals ((nodecode.kernel::*profile-switch-performer*
                            (lambda (name binary argv)
                              (setf parked (list name binary argv))
                              (format nil "switching to ~a" name))))
        (is (equal "switching to coder"
                   (nlk:profile-command (list "switch" "coder") :command "/profile")))
        (is (equal "coder" (first parked)) "and the plan is parked, not performed")
        (is (equal '("/opt/nodecode/current/nodecode" "-p" "coder") (third parked)))))))

(deftest a-pick-ends-the-loop-even-when-the-parkers-image-predates-it ()
  ;; The parker is a closure created when the shell started: a shell whose
  ;; image predates the parker's own send can never be handed it. The pick
  ;; gesture can, because a definition reaches a running shell live — so the
  ;; gesture ends the loop too, and CALL-WITH-PROFILE-SWITCH's tail performs
  ;; the parked plan once the terminal is back.
  (with-stubbed-fdefinition (nlk:organism-binary () "/opt/nodecode/current/nodecode")
    (nlk:profile-create "coder" :clone :blank)
    (let ((program (nlt:make-program nil))
          (parked '()))
      (setf (nlt:program-running program) t)
      (with-saved-globals ((nodecode.kernel::*profile-switch-performer*
                            ;; An older image's parker: it takes the plan and says
                            ;; nothing about the loop — the shape this shell actually has.
                            (lambda (name binary argv)
                              (setf parked (list name binary argv))
                              (format nil "switching to ~a" name))))
        (is (search "switching to coder" (nle::switch-profile-here program "coder")))
        (is (equal "coder" (first parked)) "the plan reaches the shell's performer")
        (is (member "-p" (third parked) :test #'string=) "as the switch plan")
        (is (some #'nlt:quit-msg-p
                  (nlt::mailbox-drain (nlt::program-mailbox program) :timeout 1)))))))

(deftest a-parked-switch-keeps-the-terminal-and-consumes-its-plan ()
  ;; The handover, as the shell's own seam states it: the pick promises this
  ;; screen to the successor (NLT:*KEEP-TERMINAL*, so RUN leaves the modes,
  ;; the raw settings and the tty alone), the plan is visible to the unwinds
  ;; that behave differently on the way out, and it is consumed exactly once —
  ;; an image that survives a refused exec has nothing parked.
  (with-saved-globals ((nodecode.kernel::*profile-switch-parked* :stale)
                       (nlt:*keep-terminal* nil))
    (is (eq :exited
            (nle::call-with-profile-switch
             (nlt:make-program nil)
             (lambda ()
               (is (null nodecode.kernel::*profile-switch-parked*))
               (funcall nodecode.kernel::*profile-switch-performer*
                        "coder" "/opt/nodecode" '("/opt/nodecode" "-p" "coder"))
               (is nlt:*keep-terminal* "and the pick promises this screen")
               (is (equal (list "coder" "/opt/nodecode" (list "/opt/nodecode" "-p" "coder"))
                          nodecode.kernel::*profile-switch-parked*))
               :exited))))
    (is (null nodecode.kernel::*profile-switch-parked*))))

(deftest the-handover-word-is-read-once-and-nothing-inherits-it (let ((nle::*handover-launch* nil)))
  ;; The environment is the one channel an exec keeps: one word says this
  ;; image inherited a live terminal, and the successor consumes it, so a
  ;; process it spawns is an ordinary launch.
  (sb-posix:setenv nlk::+handover-variable+ "1" 1)
  (nlk:with-cleanup ((sb-posix:unsetenv nlk::+handover-variable+))
    (is (nle::handover-launch-p) "the successor reads the word")
    (is (null (uiop:getenv nlk::+handover-variable+)) "and it is spent")
    (is (null (nle::handover-launch-p)) "a second read is no launch")))

(deftest an-image-that-cannot-serve-gives-the-inherited-screen-back ()
  ;; Before its first frame a handover successor is the only process on that
  ;; terminal: a boot that dies there releases the modes rather than leave
  ;; the operator on a screen nobody owns. Once its TUI has taken the screen,
  ;; the release belongs to the TUI's own unwind instead.
  (with-saved-globals ((nle::*handover-launch* t) (nlt:*inherit-screen* t))
    (let ((fired nil))
      (with-stubbed-fdefinition (nlt:release-inherited-modes (&optional stream)
                                (setf fired t))
        (nle::handover-give-back)
        (is fired "a successor that never painted gives the screen back")
        (is (null nle::*handover-launch*) "and claims it once")
        (setf fired nil
              nle::*handover-launch* t
              nlt:*inherit-screen* nil)
        (nle::handover-give-back)
        (is (null fired))))))

(deftest a-refused-exec-gives-the-screen-and-the-plan-back ()
  ;; An exec the kernel refuses is the one path that still owes the terminal
  ;; back: the modes are released and the promise is withdrawn, so the shell
  ;; that stays behind is a shell in a normal terminal.
  (with-stubbed-fdefinition (nlt:release-terminal (program) :released)
    (with-saved-globals (nodecode.kernel::*profile-switch-parked* nlt:*keep-terminal*)
      (let ((released '()))
        (let ((nodecode.kernel::*profile-switch-performer*
                (nlk:lambda-ignoring (name binary argv)
                  (setf released t)
                  (format nil "exec ~a: refused" name))))
          (is (eq :exited
                  (nle::call-with-profile-switch
                   (nlt:make-program nil)
                   (lambda ()
                     (funcall nodecode.kernel::*profile-switch-performer*
                              "coder" "/opt/nodecode" '("/opt/nodecode" "-p" "coder"))
                     :exited)))))
        (is released "the performer was reached")
        (is (null nlt:*keep-terminal*) "and the promise is withdrawn")))))

(deftest a-refused-switch-parks-nothing-and-ends-nothing (let ((program (nlt:make-program nil))
                                                               (asked '())))
  (setf program.running t)
  (with-saved-globals ((nodecode.kernel::*profile-switch-performer*
                        (nlk:lambda-ignoring (name binary argv)
                          (push name asked)
                          (format nil "parked ~a" name))))
    (is (search "profile: no profile \"nobody\"" (nle::switch-profile-here program "nobody")))
    (is (null asked) "the performer is never reached")
    (is (null (nlt::mailbox-drain (nlt::program-mailbox program) :timeout 0.1)))))

(deftest a-headless-profile-declines-the-gesture-it-has-no-terminal-for ()
  ;; A channel lane and the model's eval reach /profile through the
  ;; headless arm. Every verb but one behaves as it always did; `switch'
  ;; would replace a gateway serving every attached shell, so it says where
  ;; the gesture lives instead of performing it.
  (let ((entry (nle::find-builtin-slash-command "profile")))
    (is (eq 'nle::profile-headless-text entry.headless)))
  (nlk:profile-create "coder" :clone :blank)
  (let ((declined (nle::profile-headless-text "switch coder" "s-any")))
    (is (search "has none" declined) "it says why it cannot")
    (is (search "/profile switch coder" declined) "where the gesture lives")
    (is (search "/profile use coder" declined) "and what moves a home from here"))
  (is (search "coder" (nle::profile-headless-text "" "s-any")) "bare is still the roster text")
  (is (search "boots by default" (nle::profile-headless-text "use coder" "s-any"))))

(deftest slash-dropdown-parts-label-and-description-and-marks-a-cut ()
  ;; Without the two-column gap, the widest label's alias would run into its
  ;; description; without the cut, a description too long for its row would
  ;; stop mid-word at the edge of an 80-column window. Two columns part the
  ;; widest label from its description, and a cut description ends in `…'.
  (with-frame-model (model 80 24 :session-id "slash-gap" :typed "/tree")
    (let ((row (row-carrying "(/checkpoints)" (nle::compose-frame model))))
      (is (search "/tree (/checkpoints)  Browse the session tree" row) "two columns before the description")
      (is (uiop:string-suffix-p (string-right-trim " " row) "…") "the cut says so")
      (is (= 80 (nle::text-display-width row)) "the row keeps its width"))))

(deftest models-frequent-section-promotes-available-pairs-without-duplicates ()
  (let* ((rows (loop for i from 1 to 7 collect
                     (nle::make-picker-row :label (format nil "vendor/m~d" i)
                                          :value (list "vendor" (format nil "m~d" i)))))
         (history (nlk:decode-json "[{\"provider\":\"gone\",\"model\":\"m1\"},
 {\"provider\":\"vendor\",\"model\":\"m7\"},{\"provider\":\"vendor\",\"model\":\"m6\"},
 {\"provider\":\"vendor\",\"model\":\"m5\"},{\"provider\":\"vendor\",\"model\":\"m4\"},
 {\"provider\":\"vendor\",\"model\":\"m3\"},{\"provider\":\"vendor\",\"model\":\"m2\"}]"))
         (grouped (nle::models-frequent-picker-rows rows history)))
    (is (eq rows (nle::models-frequent-picker-rows rows #())))
    (is (equal '("Frequently used" "vendor/m7" "vendor/m6" "vendor/m5" "vendor/m4"
                 "vendor/m3" "" "Other models" "vendor/m1" "vendor/m2")
               (mapcar #'nle::picker-row-label grouped)))
    (is (= 7 (length (remove-if #'nle::picker-row-heading grouped))))
    (is (equal '("vendor/m1" "vendor/m2" "vendor/m3" "vendor/m4" "vendor/m5" "vendor/m6" "vendor/m7")
               (mapcar #'nle::picker-row-label rows)))))

(deftest models-frequent-read-is-off-loop-and-belongs-to-the-opening-dialog ()
  (with-model-listings (:config +listed-providers-config+ :cache (listed-providers-cache))
    (with-frame-model (model 100 40 :session-id "models-history")
      (let ((pending nil) (asked nil))
        (flet ((land () (destructuring-bind (thunk . continuation) (pop pending)
                         (funcall continuation model (funcall thunk)))))
          (with-stubbed-fdefinition (nle::off-loop (model thunk continuation)
                                      (setf pending (append pending (list (cons thunk continuation)))))
            (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body)
                                        (push path asked)
                                        (nlk:decode-json "{\"models\":[{\"provider\":\"zeta\",\"model\":\"z-2\",\"turns\":3}]}"))
              (with-stubbed-fdefinition (nle::start-provider-models-refresh (model) nil)
                (setf (nle::chat-model-program model) t)
                (nle::open-models-picker model)
                (is (null asked) "no HTTP read on the loop")
                (is (= 1 (length pending)))
                (land)
                (let ((dialog model.dialog))
                  (is (equal "Frequently used" (nle::picker-row-label (first (nle::picker-dialog-rows dialog)))))
                  (is (equal '("zeta" "z-2") (picker-selected-value dialog)))
                  (is (equal "Models (5)" (nle::picker-dialog-title dialog)))
                  (dialog-after model "z-2")
                  (is (notany #'nle::picker-row-heading (nle::picker-visible-rows dialog)))
                  (nle::install-refreshed-models model)
                  (is (equal '("zeta" "z-2") (picker-selected-value dialog)))
                  (is (equal "z-2" (nle::picker-dialog-search dialog))))
                (dialog-keys model :escape)
                (nle::open-models-picker model)
                (dialog-keys model :escape)
                (is (eq :quiet (land)))
                (is (null model.dialog) "Esc is never undone")
                (nle::open-models-picker model)
                (nle::open-picker-dialog model "Another panel" nil nil)
                (let ((other model.dialog))
                  (is (eq :quiet (land)))
                  (is (eq other model.dialog)))
                (nle::open-models-picker model)
                (dialog-keys model :escape)
                (nle::open-models-picker model)
                (let ((reopened model.dialog))
                  (is (eq :quiet (land)))
                  (is (eq reopened model.dialog))
                  (is (notany #'nle::picker-row-heading (nle::picker-dialog-rows reopened)))
                  (land)
                  (is (nle::picker-row-heading (first (nle::picker-dialog-rows reopened)))))))))))))

(deftest slash-models-picker-model-name-precedes-provider ()
  (with-model-listings (:config '(:providers (:anthropic (:api_key "k" :display_name "Anthropic"
                                                          :models #((:id "typed")))))
                        :catalog '(:anthropic (:models (:blank (:name "   " :tool_call t)
                                                        :claude-x (:name "Claude X" :tool_call t
                                                                   :limit (:context 100000))))))
    (let ((rows (nle::models-catalog-picker-rows)))
      (is (equal '("blank" "Claude X" "typed") (mapcar #'nle::picker-row-label rows)) "display name leads, including equivalent spelling; blank or absent name falls back to codename")
      (is (equal '("anthropic" "anthropic" "anthropic")
                 (mapcar #'nle::picker-row-detail rows)) "provider immediately follows the model")
      (is (equal '(("anthropic" "blank") ("anthropic" "claude-x") ("anthropic" "typed"))
                 (mapcar #'nle::picker-row-value rows)) "presentation never changes selection tokens")
      (let ((nle::*color-profile* :none))
        (is (equal "Claude X  anthropic"
                   (nle::segs-text (nle::dialog-entry-segs (second rows) nil))) "the shared picker paints exactly two spaces before the provider")))))

(deftest slash-models-picker-context-only-on-highlight ()
  (with-model-listings (:config +keyed-models-config+
                        :catalog "{\"anthropic\":{\"models\":{\"claude-x\":{\"name\":\"Claude X\",\"tool_call\":true}}}}")
    (let ((row (first (nle::models-catalog-picker-rows))))
      (is (equal "Claude X  anthropic" (nle::segs-text (nle::dialog-entry-segs row nil))))
      (is (equal "Claude X  anthropic  • ctx 200k" (nle::segs-text (nle::dialog-entry-segs row t)))))
    (with-frame-model (model 100 30 :session-id "models-context-hover")
      (nle::open-models-picker model)
      (nle::compose-frame model)
      (let ((first (nle::dialog-body-band model model.dialog)))
        (nle:update model (nlt:make-mouse-motion-msg :x 6 :y (1+ first)))
        (is (rows-carry-p "Claude X  anthropic" (nle::compose-frame model)))
        (is (not (rows-carry-p "ctx 200k" (nle::compose-frame model))))
        (nle:update model (nlt:make-mouse-motion-msg :x 6 :y first))
        (is (rows-carry-p "Claude X  anthropic  • ctx 200k" (nle::compose-frame model)))
        (dialog-keys model :down)
        (is (not (rows-carry-p "ctx 200k" (nle::compose-frame model))))
        (dialog-keys model :up)
        (is (rows-carry-p "Claude X  anthropic  • ctx 200k" (nle::compose-frame model)))))))

;;; Put latest versions of models on top, not at the bottom, and make sure
;;; that not only the provider can be found through search, but also the
;;; model name.

(deftest models-list-newest-first (let ((now (get-universal-time))))
  ;; Every list a model is picked from reads MODEL-CHOICES: the later
  ;; models.dev release date first; a router's listing, which dates nothing,
  ;; takes the date its bare id carries anywhere in the catalog; an undated
  ;; model follows, the higher version first (z-10 above z-2).
  (with-model-listings (:config '(:providers (:anthropic (:api_key "k")
                                              :zeta (:sdk "openai-completions" :api_key "k"
                                                     :base_url "https://zeta.example/v1")))
                        :catalog '(:anthropic (:models (:claude-old (:tool_call t :release_date "2025-01-01")
                                                        :claude-new (:tool_call t :release_date "2026-09-22")
                                                        :claude-mid (:tool_call t :release_date "2026-01-10"))))
                        :cache `(:zeta (:fetched_at ,now :base_url "https://zeta.example/v1" :error :null
                                        :models #((:id "z-2") (:id "anthropic/claude-new") (:id "z-10")))))
    (is (equal '(("anthropic" "claude-new") ("zeta" "anthropic/claude-new")
                 ("anthropic" "claude-mid") ("anthropic" "claude-old")
                 ("zeta" "z-10") ("zeta" "z-2"))
               (mapcar #'nle::picker-row-value (nle::models-catalog-picker-rows))) "newest first, a router's row dated by its bare id, undated rows by version")))

(deftest models-root-search-finds-models-by-name ()
  ;; While /models' search box holds a query, every model lists under the
  ;; providers, newest first, and Enter on one picks it as a Frequently used
  ;; shortcut does; a provider's own name still finds its row first, and an
  ;; empty box is the provider list again.
  (with-model-listings (:config '(:providers (:anthropic (:api_key "k")))
                        :catalog '(:anthropic (:name "Anthropic" :npm "@ai-sdk/anthropic"
                                               :models (:claude-opus-4 (:name "Claude Opus 4" :tool_call t
                                                                        :release_date "2025-05-22")
                                                        :claude-haiku-4 (:name "Claude Haiku 4" :tool_call t
                                                                         :release_date "2025-10-15")
                                                        :claude-opus-5 (:name "Claude Opus 5" :tool_call t
                                                                        :release_date "2026-09-22")))))
    (with-frame-model (model 100 30 :session-id "models-root-search")
      (let ((picked nil))
        (with-stubbed-fdefinitions ((nle::slash-models-op (model tokens) (setf picked tokens) "selected")
                                    (nle::open-models-reasoning-picker (model) nil))
          (nle::open-models-provider-picker model)
          (let ((dialog model.dialog))
            (flet ((models-shown ()
                     (loop for row in (nle::picker-visible-rows dialog)
                           for pair = (getf (nle::picker-row-value row) :model)
                           when pair collect pair)))
              (is (equal "Search providers and models" (nle::picker-dialog-search-placeholder dialog)))
              (is (null (models-shown)) "an empty box lists providers")
              (dialog-after model "opus")
              (is (equal '(("anthropic" "claude-opus-5") ("anthropic" "claude-opus-4")) (models-shown)) "a model's name finds it, the newest first")
              (dialog-keys model :backspace :backspace :backspace :backspace)
              (is (null (models-shown)) "an emptied box is the provider list again")
              (dialog-after model "anthr")
              (is (equal "anthropic" (getf (picker-selected-value dialog) :provider)) "a provider's name still finds its row first")
              (dialog-keys model :backspace :backspace :backspace :backspace :backspace)
              (dialog-after model "opus" :enter)
              (is (equal '("anthropic" "claude-opus-5") picked) "Enter on a found model picks it"))))))))

;;; Provider-first /models: one entry point for keys and selection.
(deftest models-provider-root-replaces-connect-completely ()
  (is (null (nle::find-builtin-slash-command "connect")))
  (is (null (nle::slash-suggestions "/connect")))
  (with-model-listings (:catalog +connect-catalog-fixture+)
    (with-frame-model (model 100 30 :session-id "models-provider-root" :slash "models")
      (is-shape model.dialog (nle::connect-dialog-p is "root offers providers, not models")
        (nle::picker-dialog-kind eq :model-providers) (nle::picker-dialog-title "Models"))
      (is (not (find "Continue" (nle::picker-dialog-rows model.dialog)
                     :key #'nle::picker-row-label :test #'equal)))
      (is (eq :api-key (step-after model :enter)) "unconnected provider asks for its key")
      (is (null (dialog-after model :escape :escape))))))

(deftests models-provider (steps)
  (with-model-listings (:config +listed-providers-config+ :cache (listed-providers-cache))
    (with-frame-model (model 100 40 :session-id "models-provider")
      steps))
  (pick-filters-and-escape-returns-to-providers
   (progn
     (nle::execute-slash-command model "models" "")
     (is (nle::connect-dialog-p model.dialog))
     ;; Both providers are configured but absent from models.dev.
     (is (enter-row model "zeta") "configured providers survive without a models.dev entry")
     (is (eq :models (nle::picker-dialog-kind model.dialog)))
     (is (equal '("zeta" "zeta" "zeta")
                (mapcar (lambda (row) (first (nle::picker-row-value row)))
                        (remove-if #'nle::picker-row-heading (nle::picker-dialog-rows model.dialog)))))
     (nle::install-refreshed-models model)
     (is (every (lambda (row) (equal "zeta" (first (nle::picker-row-value row))))
                (remove-if #'nle::picker-row-heading (nle::picker-dialog-rows model.dialog))))
     (is (nle::connect-dialog-p (dialog-after model :escape)))
     (is (eq :model-providers (nle::picker-dialog-kind model.dialog)))
     (is (null (dialog-after model :escape)))))
  (frequent-shortcuts-stay-model-specific
   (let ((picked nil))
     (with-stubbed-fdefinitions
         ((nle::slash-models-op (model tokens) (setf picked tokens) "selected")
          (nle::target-snapshot (model)
            (values "zeta" "z-2" '("low" "medium" "high") '(:effort "medium"))))
       (nle::execute-slash-command model "models" "")
       (let ((dialog model.dialog))
         (nle::install-frequent-models model dialog
           (nlk:decode-json "{\"models\":[{\"provider\":\"gone\",\"model\":\"missing\"},{\"provider\":\"zeta\",\"model\":\"z-2\"}]}"))
         (is (equal "Frequently used" (nle::picker-row-label (first (nle::picker-visible-rows dialog)))))
         (is (equal '("zeta" "z-2") (getf (nle::picker-row-value (second (nle::picker-visible-rows dialog))) :model)))
         (is (find "zeta" (nle::picker-dialog-rows dialog) :key #'nle::picker-row-label :test #'equal))
         (enter-row model '("zeta" "z-2") :model)
         (is (equal '("zeta" "z-2") picked))
         (is (eq :think (nle::picker-dialog-kind model.dialog)))
         (is (equal '(nil "low" "medium" "high")
                    (mapcar #'nle::picker-row-value (nle::picker-visible-rows model.dialog))))))))
  (local-endpoint-opens-models-without-a-key
   (progn
     (nle::open-models-provider-picker model)
     (is (enter-row model "local" :provider))
     (is (eq :models (nle::picker-dialog-kind model.dialog)))
     (is (equal '("qwen") (mapcar (lambda (row) (second (nle::picker-row-value row)))
                                  (nle::picker-dialog-rows model.dialog))))))
  (model-pick-opens-reasoning-effort
   (with-stubbed-fdefinitions
       ((nle::slash-models-op (model tokens) "selected")
        (nle::target-snapshot (model)
          (values "zeta" "z-2" '("low" "medium" "high") '(:effort "medium")))
        (nle::apply-effort (model effort) nil))
     (nle::open-models-provider-picker model)
     (is (enter-row model "zeta"))
     (is (eq :models (nle::picker-dialog-kind model.dialog)))
     (is (enter-row model "z-2"))
     (is (eq :think (nle::picker-dialog-kind model.dialog)))
     (is (equal "Reasoning effort" (nle::picker-dialog-title model.dialog)))
     (is (equal '(nil "low" "medium" "high")
                (mapcar #'nle::picker-row-value
                        (nle::picker-visible-rows model.dialog))))))
  (history-completions-belong-to-the-opening-root
   (let ((pending '())
         (history (nlk:decode-json "{\"models\":[{\"provider\":\"zeta\",\"model\":\"z-2\"}]}")))
     (with-stubbed-fdefinitions ((nle::off-loop (model thunk continuation)
                                   (push continuation pending))
                                 (nle::start-provider-models-refresh (model) nil)
                                 ;; The Backup tab's read is a completion of its
                                 ;; own (INSTALL-BACKUP-STATE); these are history's.
                                 (nle::start-models-backup-read (model dialog) nil))
       (setf (nle::chat-model-program model) t)
       ;; Closing uses the real Esc gesture; a late answer never reopens.
       (nle::open-models-provider-picker model)
       (dialog-keys model :escape)
       (funcall (pop pending) model history)
       (is (null model.dialog))
       ;; Replacing the root with another dialog leaves that dialog intact.
       (nle::open-models-provider-picker model)
       (nle::open-help-dialog model)
       (let ((other model.dialog))
         (funcall (pop pending) model history)
         (is (eq other model.dialog)))
       ;; Reopening makes a new identity; only its own completion installs.
       (nle::open-models-provider-picker model)
       (let ((old (pop pending)))
         (dialog-keys model :escape)
         (nle::open-models-provider-picker model)
         (let ((fresh model.dialog))
           (funcall old model history)
           (is (eq fresh model.dialog))
           (is (not (find "Frequently used" (nle::picker-dialog-rows fresh)
                          :key #'nle::picker-row-label :test #'equal)))
           (funcall (pop pending) model history)
           (is (equal '(:model ("zeta" "z-2")) (picker-selected-value fresh)))))))))

(deftest models-provider-history-keeps-key-step-and-seats-first-shortcut ()
  (with-model-listings (:config +listed-providers-config+ :catalog +connect-catalog-fixture+
                        :cache (listed-providers-cache))
    (with-frame-model (model 100 40 :session-id "models-provider-late-key")
      (let ((history (nlk:decode-json "{\"models\":[{\"provider\":\"zeta\",\"model\":\"z-2\"}]}")))
        (nle::open-models-provider-picker model)
        (let ((dialog model.dialog))
          (is (= 1 (nle::picker-dialog-selection dialog)) "provider heading is not a selectable seat")
          (nle::install-frequent-models model dialog history)
          (is (equal '(:model ("zeta" "z-2")) (picker-selected-value dialog))))
        (nle::open-models-provider-picker model)
        (let ((dialog model.dialog))
          (enter-row model "anthropic" :provider)
          (dialog-after model "sk-kept-secret")
          (let ((rows (nle::picker-dialog-rows dialog))
                (seat (nle::picker-dialog-selection dialog)))
            (is (eq :quiet (nle::install-frequent-models model dialog history)))
            (is-shape dialog (nle::connect-dialog-step eq :api-key)
              (nle::picker-dialog-input "sk-kept-secret") (nle::picker-dialog-rows eq rows)
              (nle::picker-dialog-selection = seat)))
          (dialog-keys model :escape)
          (is (equal "Frequently used" (nle::picker-row-label (first (nle::picker-visible-rows dialog)))))
          (is (equal "anthropic" (getf (picker-selected-value dialog) :provider))))))))

(deftest models-effort-panel-lets-typing-through ()
  ;; After a /models pick the reasoning effort opens by itself. A `yo' typed
  ;; at it vanished and its Enter picked a rung (Mac take, 2026-10-21): a
  ;; printable key closes that panel, keeping the effort, and lands in the
  ;; composer. Its own keys still work, and /think's panel, which the operator
  ;; opened, keeps every key.
  (with-frame-model (model 100 30 :session-id "models-effort-type-through")
    (let ((applied '()))
      (with-stubbed-fdefinitions
          ((nle::target-snapshot (model) (values "p" "m" '("low" "high") (list :effort "low")))
           (nle::apply-effort (model effort) (push effort applied) nil))
        (nle::open-models-reasoning-picker model)
        (is (eq :think (nle::picker-dialog-kind model.dialog)))
        (press model #\y #\o)
        (is (null model.dialog) "typing closed it")
        (is (equal "yo" (composer-value model)) "and reached the composer")
        (is (null applied) "keeping the effort that stands")
        (nle::open-models-reasoning-picker model)
        (press model :down :enter)
        (is (null model.dialog))
        (is (equal '("high") applied) "Enter picks the highlighted rung")
        (nle::open-think-dialog model "p" "m" '("low" "high") "low"
                                (lambda (model effort) (nle::apply-effort model effort)))
        (press model #\x)
        (is (eq :think (nle::picker-dialog-kind model.dialog)) "/think's own panel keeps its keys")))))
