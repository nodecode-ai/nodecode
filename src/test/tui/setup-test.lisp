;;;; setup-test.lisp --- the setup wizard, and the picker's checklist and
;;;; input modes it stands on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The wizard is a chain of the shell's own dialogs, so the gates drive it
;;;; the way an operator does — PICKER-DIALOG-KEY — and assert what landed
;;;; where the writers put it: auth.json, the shared config, the report. The catalog fixture is /models's
;;;; (+CONNECT-CATALOG-FIXTURE+, slash-test.lisp).

(in-package #:nodecode.test)

;;; --- the picker's two new modes --------------------------------------------------

(deftest picker-checklist-marks-with-space-and-confirms-the-marks ()
  (with-frame-model (model 80 24 :session-id "checklist")
    (let ((chosen :untouched)
          (toggles '()))
      (nle::open-picker-dialog
       model "Pick"
       (picker-rows '("alpha" :value "a") '("beta" :value "b") '("gamma" :value "c"))
       (lambda (model marks) (declare (ignore model)) (setf chosen marks) nil)
       :on-toggle (nlk:lambda-ignoring (model value marked)
                    (push (cons value marked) toggles))
       :marks (list "b"))
      (let ((dialog model.dialog))
        (is (equal '("[ ] alpha" "[x] beta" "[ ] gamma") (row-labels dialog)))
        (nle::picker-dialog-key model #\Space)
        (is (equal '("b" "a") dialog.marks) "Space marks the row")
        (is (equal '(("a" . t)) toggles) "and tells on-toggle")
        (is (row-carrying "[x] alpha" (nle::dialog-panel-rows model 60 nil)))
        (dialog-keys model :down :down #\Space :up #\Space)
        (is-shape dialog (.marks '("a" "c")) (row-labels '("[x] alpha" "[ ] beta" "[x] gamma")))
        (is (eq dialog model.dialog) "Space keeps the dialog open")
        (nle::picker-dialog-key model :enter)
        (is (equal '("a" "c") chosen) "Enter hands on-select the marks, not a row")
        (is (null model.dialog) "and closes")))))

(deftest picker-input-mode-edits-a-field-and-parks-the-cursor ()
  (with-frame-model (model 80 30 :session-id "input-mode")
    (let ((answer :none))
      (nle::open-picker-dialog model "Field" '()
                               (lambda (model value) (declare (ignore model value)) nil))
      (let ((dialog model.dialog))
        (setf dialog.input-label "Token"
              dialog.input-context "the rule"
              dialog.on-input
              (nlk:lambda-ignoring (model text)
                (setf answer text
                      dialog.input-label nil)))
        (dialog-keys model #\a #\b #\c)
        (is (equal "abc" dialog.input) "printable keys edit the field")
        (let* ((rows (nle::compose-frame model))
               (park model.parked-cursor))
          (is-present park "the hardware cursor parks on the field"
            (is (search "Token  abc" (nth (cdr park) rows)))
            ;; content column 4 + "Token" (5) + the two-space gap + "abc".
            (is (= 14 (car park)) "just past the value"))
          (is (row-carrying "the rule" rows)))
        (is (not (nle::dialog-list-selectable-p dialog)))
        ;; a secret masks
        (setf dialog.input-secret-p t)
        (let ((rows (nle::compose-frame model)))
          (is (row-carrying "***" rows) "a secret paints as stars")
          (is (not (rows-carry-p "abc" rows)) "never as itself"))
        (nle::picker-dialog-key model :enter)
        (is (equal "abc" answer) "Enter hands the text to on-input")
        (is (null dialog.input-label) "and the handler left input mode")
        (setf dialog.input-label "Again"
              answer :none)
        (nle::picker-dialog-key model :escape)
        (is (null answer) "Esc hands NIL to on-input")
        (is (nle::chat-model-dialog model) "and the dialog stays open")
        ;; without a handler Esc just leaves the field
        (setf dialog.input-label "Bare" dialog.on-input nil)
        (nle::picker-dialog-key model :escape)
        (is (null dialog.input-label))
        (is (nle::chat-model-dialog model))))))

;;; --- the wizard ---------------------------------------------------------------

(defmacro with-setup-box ((&optional config) &body body)
  "Run BODY on a box whose models.dev cache is /models's fixture, with no key
anywhere -- no provider's own key in the environment either -- and a fresh
shared config holding CONFIG, or absent without one."
  `(with-temp-file (nle::*models-catalog-path* :contents +connect-catalog-fixture+)
     (with-temp-file (nle::*auth-file-path*)
       (with-temp-file (nle::*shared-config-path* :setf t :contents ,config)
         (let ((nle::*models-catalog-memo* nil)
               ;; The picker reads these files where the organism serves.
               (nle::*serving* t)
               (nle::*api-key* nil))
           (with-stubbed-fdefinition (nle::provider-env-key (provider) nil)
             ,@body))))))

(defun said-rows (model)
  "The texts of the system rows in MODEL's transcript, oldest first: what the
walk must never leave there."
  (loop for row in (nle::transcript-rows (nle::chat-model-state model))
        when (eq :system-notice (nle::transcript-row-kind row))
          collect (nle::transcript-row-text row)))

(defun page-said (model)
  "What MODEL's open setup panel says over its rows -- the walk's last page,
its key check, a Details page -- one painted row a line, or the empty string."
  (let ((dialog (nle::chat-model-dialog model)))
    (if (nle::picker-dialog-p dialog)
        (format nil "~{~a~^~%~}" (mapcar #'nle::segs-text (nle::picker-dialog-summary-rows dialog)))
        "")))

(defun ready-page-p (model)
  "Whether MODEL's open dialog is the walk's last page."
  (let ((dialog (nle::chat-model-dialog model)))
    (and (nle::picker-dialog-p dialog) (eq :setup-ready (nle::picker-dialog-kind dialog)))))

(defun page-text (run &key (width 1000) (caveats (nle::setup-caveats run)))
  "RUN's last page as its panel paints the lines over its rows at WIDTH, one a
line: wide enough by default that no value wraps."
  (format nil "~{~a~^~%~}"
          (mapcar #'nle::segs-text (nle::setup-line-rows (nle::setup-ready-lines run caveats) width))))

(defun third-row-text (dialog)
  "The text of a connect DIALOG's third header row, its search row; none for another dialog."
  (nle::segs-text (and (nle::connect-dialog-p dialog) (nle::docked-dialog-third-row-segs dialog))))

(defun run-setup (model)
  "Type /setup on MODEL as an operator does once its first replay has landed;
answers what the command answers."
  (setf (nle::gateway-model-last-sequence model) 0)
  (nle::execute-slash-command model "setup" ""))

(deftest connect-step-hints-give-each-key-its-own-action ()
  ;; A footer that names two keys for one action (`enter return  esc
  ;; return') tells the operator nothing about either key.
  (dolist (entry nle::+connect-steps+)
    (let ((hint (getf (rest entry) :hint)) (parts '()) (start 0))
      (loop for at = (search "  " hint :start2 start)
            do (push (subseq hint start at) parts)
            while at do (setf start (+ at 2)))
      (let ((actions (mapcar (lambda (part) (subseq part (1+ (or (position #\Space part) -1)))) parts)))
        (is (= (length actions) (length (remove-duplicates actions :test #'string=))) (format nil "~(~a~): ~s" (first entry) hint))))))

(deftest setup-first-launch-required-without-provider-or-saved-model ()
  (with-saved-globals ((nle::*shared-config-path* nil))
    (is (null (nle::setup-required-p)) "the hermetic posture never asks"))
  (with-setup-box ()
    ;; Whether the environment connects the default lane is the box's
    ;; business; the two readings agree with each other either way.
    (is (eq t (nle::setup-required-p)) "no saved model still needs setup")
    (nle::save-provider-api-key "acme-labs" "sk-1")
    (is (nle::provider-connected-p) "an auth.json entry connects")
    (is (eq t (nle::setup-required-p)) "a key without a model still needs setup")
    (nle::persist-shared-model-pin "default_model" "acme-labs" "fixture")
    (with-stubbed-fdefinition (nle::default-model-configured-p () t)
      (is (null (nle::setup-required-p)) "a saved usable model suppresses first launch"))))

(deftest setup-wizard-walks-provider-model-and-effort (with-setup-box ())
  ;; The walk is the provider, the model and its reasoning effort, and ends on
  ;; its last page; a cell is turned on in /cells and set up in /settings,
  ;; never here. The chat opens on nothing the walk said.
  (with-frame-model (model 100 30 :session-id "setup-wiz")
    (with-stubbed-fdefinitions
        ((nle::slash-default-model-op (model tokens)
           (nle::select-default-model :provider (first tokens) :model (second tokens))
           "default model saved")
         (nle::target-snapshot (model)
           (values nle::*provider* nle::*model* nil nil)))
      (is (null (run-setup model)))
      ;; --- provider: the /models list. A saved key returns to it,
      ;; Suggested, and Enter on it goes on.
      (let ((dialog (nle::chat-model-dialog model)))
        (is (nle::connect-dialog-p dialog) "the walk opens on the providers")
        (when (nle::connect-dialog-p dialog)
          (is (equal "Step 1. Choose provider · step 1/3"
                     (nle::picker-dialog-context dialog)))
          ;; nodecode.ai's sign-in leads; Anthropic's key is the row under it
          (dialog-keys model :down :enter)
          (is (eq :api-key (nle::connect-dialog-step dialog)))
          (dialog-keys model #\s #\k #\- #\9 :enter)
          (is (eq :provider (nle::connect-dialog-step dialog))
              (nle::connect-dialog-result-message dialog))
          (is (equal '("Suggested" "Anthropic") (subseq (row-labels dialog) 0 2)))
          (nle::picker-dialog-key model :enter)))
      (is (nle::provider-connected-p) "a provider is connected past the first stage")
      ;; --- model: the /models picker when the catalog has rows to pick
      ;; from; Enter picks and the effort follows.
      (let ((dialog (nle::chat-model-dialog model)))
        (is (and (nle::picker-dialog-p dialog)
                 (eq :models (nle::picker-dialog-kind dialog))))
        (when (and (nle::picker-dialog-p dialog)
                   (eq :models (nle::picker-dialog-kind dialog)))
          (nle::picker-dialog-key model :enter)))
      ;; The model leads to effort; this model has no controls.
      (is (eq :think (nle::picker-dialog-kind model.dialog)))
      (is (equal "Step 3. Choose reasoning effort" (nle::picker-dialog-context model.dialog)))
      (nle::picker-dialog-key model :enter)
      (is (ready-page-p model) "setup ends after reasoning, on its last page")
      (is (equal "Ready" (nle::picker-dialog-context model.dialog)))
      (is-carrying (text (page-said model))
        "✓ provider    anthropic" "✓ model "
        (:absent " cells " "it installed no cell, and claims none"))
      (is (equal '(:start) (mapcar #'nle::picker-row-value (nle::picker-dialog-rows model.dialog))) "no import ran, so nothing to detail")
      (is (null (said-rows model)) "the chat holds nothing the walk said")
      (nle::picker-dialog-key model :enter)
      (is (null model.dialog) "Start closes it")
      (is (null (nle::setup-issues model)) "over nothing to check")
      ;; a second walk opens on the list with the connected provider
      ;; Suggested; Esc on the models and the providers ends it on a page
      ;; that names no model
      (run-setup model)
      (let ((dialog (nle::chat-model-dialog model)))
        (is (nle::connect-dialog-p dialog))
        (is (equal '("Suggested" "Anthropic" "Connect another")
                   (subseq (row-labels dialog) 0 3)))
        (nle::picker-dialog-key model :enter))
      (let ((dialog (nle::chat-model-dialog model)))
        (when (and (nle::picker-dialog-p dialog) (eq :models (nle::picker-dialog-kind dialog)))
          (nle::picker-dialog-key model :escape)))
      (nle::picker-dialog-key model :escape)
      (is (ready-page-p model))
      (is (not (search "✓ model" (page-said model))))
      (is (null (said-rows model))))))

(deftest setup-details-lists-each-item-by-world-and-comes-back ()
  ;; The import's report was a transcript row the chat opened on. It is a
  ;; page of the walk's dialog now: each item under its world, what became of
  ;; it beside it, and Esc or Enter back to the page it was opened from, on
  ;; its Details row.
  (let* ((report (import-answer 2 1 1
                  "applied" t "worlds" (vector "codex" "claude")
                  "items" (vector (nlk:json-object "kind" "skills" "status" "imported" "world" "codex"
                                                   "source" "SKILL.md" "destination" "skills/a" "reason" "")
                                  (nlk:json-object "kind" "mcp" "status" "skipped" "world" "claude"
                                                   "source" "x.json" "destination" "mcp.p" "reason" "needs bun")
                                  (nlk:json-object "kind" "memory" "status" "imported" "world" "claude"
                                                   "source" "MEMORY.md" "destination" "memory" "reason" "")
                                  (nlk:json-object "kind" "sessions" "status" "error" "world" "codex"
                                                   "source" "r.jsonl" "destination" "" "reason" "unreadable"))))
         (run (nle::make-setup-run :steps '(:provider :model :effort) :report report)))
    (with-frame-model (model 100 30 :session-id "setup-details")
      (with-stubbed-fdefinition (nle::setup-caveats (run) '())
        (nle::setup-stage-finish model run)
        (is (ready-page-p model))
        (let ((rows (nle::picker-dialog-rows model.dialog)))
          (is (equal '(:start :details) (mapcar #'nle::picker-row-value rows)))
          (is (equal "what came over, and why 2 did not" (nle::picker-row-detail (second rows)))))
        (dialog-keys model :down :enter)
        (let ((dialog model.dialog))
          (is (eq :setup-details (nle::picker-dialog-kind dialog)))
          (is (equal "2 imported · 1 skipped · 0 conflicts · 1 error" (nle::picker-dialog-subtitle dialog)))
          ;; world by world, in the order the report read them
          (is (equal '("codex" "claude")
                     (loop for row in (nle::picker-dialog-rows dialog)
                           when (nle::picker-row-heading row) collect (nle::picker-row-label row))))
          (is-carrying (text (format nil "~{~a ~a~^~%~}"
                                     (loop for row in (nle::picker-dialog-rows dialog)
                                           unless (nle::picker-row-heading row)
                                             append (list (nle::picker-row-label row)
                                                          (nle::picker-row-detail row)))))
            "✓ Skill" "! Conversation" "unreadable" "– " "needs bun")
          (is (not (nle::picker-row-heading (nle::selected-of (nle::picker-visible-rows dialog)
                                                              (nle::picker-dialog-selection dialog)))) "the highlight rests on an item, not its world"))
        (nle::picker-dialog-key model :escape)
        (is (ready-page-p model) "Esc goes back to the page")
        (is (eq :details (nle::picker-highlighted-value model.dialog)) "on the row it left from")
        (is (null (said-rows model)))))))

(deftest setup-a-saved-key-returns-to-the-providers-and-enter-goes-on ()
  ;; The walk connects every provider the operator has a key for, then picks
  ;; a model from all of them. A saved key returns to the list rather than
  ;; stopping at `Credential setup is complete': the provider is Suggested
  ;; and selected, and the other providers remain there to connect.
  (with-setup-box ((format nil "{}~%"))
    (with-frame-model (model 100 30 :session-id "setup-saved-key")
      (run-setup model)
      (let ((dialog model.dialog))
        (is (nle::connect-dialog-p dialog) "the walk opens on the providers")
        (when (nle::connect-dialog-p dialog)
          (is (equal '("Sign in to nodecode.ai" "" "Or use a provider's key" "Anthropic") (subseq (row-labels dialog) 0 4)) "no Suggested before a key: nodecode.ai's sign-in leads")
          (dialog-keys model :down :enter #\s #\k :enter)
          (is (eq dialog model.dialog) "a saved key returns to the list")
          (is (equal '("Suggested" "Anthropic" "Connect another")
                     (subseq (row-labels dialog) 0 3)))
          (is (search "connected (auth.json)" (nle::picker-row-detail (second dialog.rows))))
          ;; Anthropic once, under Suggested; the rest still there to connect
          (is (not (member "Anthropic" (nthcdr 3 (row-labels dialog)) :test #'equal)))
          (is (member "Acme Labs" (nthcdr 3 (row-labels dialog)) :test #'equal))
          (is (= 1 (nle::connect-dialog-selection dialog)) "resting on Anthropic")
          (is (search "esc skip" (nle::picker-dialog-hint dialog)) "Esc still moves the walk on")
          (is (search "saved to auth.json" (or (nle::status-said model) "")) "the key's line over it")
          (nle::picker-dialog-key model :enter)
          ;; Enter on it opens the model stage
          (let ((next model.dialog))
            (is (and (nle::picker-dialog-p next) (not (nle::connect-dialog-p next))
                     (eq :models (nle::picker-dialog-kind next))))
            (is (nle::chat-model-setup-walk model) "and the walk goes on"))))
      (loop repeat 8 while (and model.dialog (not (ready-page-p model)))
            do (nle::picker-dialog-key model :escape))
      (is (ready-page-p model) "Esc walks back to the walk's last page")
      (nle::picker-dialog-key model :escape)
      (is (null model.dialog) "and Esc there starts"))))

(defun gateway-models-answer ()
  "What GET /api/gateway/models answers on this box, as a shell holds the
decoded body: each model a turn can run on, and each connected provider."
  (let ((nle::*serving* t))
    (nle::make-json-object
     "models" (coerce (loop for row in (nle::model-choices)
                           unless (getf row :placeholder)
                             collect (nle::make-json-object "provider" (getf row :provider)
                                                            "model" (getf row :model)))
                     'vector)
     "unlisted" #()
     "providers" (coerce (loop for id in (nle::connected-provider-ids)
                              collect (nle::make-json-object "id" id "connected" t "ready" t))
                         'vector))))

(deftest setup-a-key-saved-in-an-attached-shell-reads-connected ()
  ;; A shell reads the provider list from its gateway (*GATEWAY-MODELS*), an
  ;; answer it holds from before the key existed. Rebuilt from that answer, a
  ;; key just saved left its provider unconnected: the list came back on its
  ;; first row, and Enter on the provider asked for the same key again
  ;; (Windows test drive, 2026-10-21). The save reads the gateway's list again
  ;; first, so the provider is Suggested and selected, and Enter goes on.
  (with-setup-box ((format nil "{}~%"))
    (with-frame-model (model 100 30 :session-id "setup-shell-saved-key")
      (let ((reads 0))
        (with-saved-globals ((nle::*serving* nil)
                             (nle::*gateway-models* (nle::gateway-models-facts (gateway-models-answer))))
          (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &rest keys)
                                      (declare (ignore model body keys))
                                      (if (and (eq method :get) (equal path "/api/gateway/models"))
                                          (progn (incf reads) (gateway-models-answer))
                                          (nle::make-json-object)))
            (run-setup model)
            (let ((dialog model.dialog))
              (is (nle::connect-dialog-p dialog) "the walk opens on the providers")
              (when (nle::connect-dialog-p dialog)
                (is (equal '("Sign in to nodecode.ai" "" "Or use a provider's key" "Anthropic") (subseq (row-labels dialog) 0 4)) "no Suggested before a key: nodecode.ai's sign-in leads")
                (dialog-keys model :down :enter #\s #\k :enter)
                (is (eq dialog model.dialog) "a saved key returns to the list")
                (is (plusp reads) "the save read the gateway's list again")
                (is (equal '("Suggested" "Anthropic" "Connect another")
                           (subseq (row-labels dialog) 0 3)))
                (is (= 1 (nle::connect-dialog-selection dialog)) "resting on the provider just saved")
                (nle::picker-dialog-key model :enter)
                (is (not (eq :api-key (and (nle::connect-dialog-p dialog) (nle::connect-dialog-step dialog)))) "Enter on it never asks for the key again")
                (let ((next model.dialog))
                  (is (eq :models (and (nle::picker-dialog-p next) (nle::picker-dialog-kind next))) "Enter goes on to the models"))))
            (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape))))))))

(deftest setup-enter-on-a-connected-provider-goes-on-with-its-key (let ((nle::*key-checks* (make-hash-table :test 'equalp))))
  ;; Enter on a provider the import has just brought a key for asks for
  ;; that key. A provider whose key answers goes on to its models, on its
  ;; newest; Ctrl+E is where a new key is pasted, and a refused key's row
  ;; still asks for one on Enter.
  (with-setup-box ((format nil "{}~%"))
    (write-temp-file nle::*models-catalog-path* "{
  \"anthropic\": {\"name\": \"Anthropic\", \"npm\": \"@ai-sdk/anthropic\",
                \"models\": {\"old\": {\"tool_call\": true, \"release_date\": \"2025-01-01\"},
                             \"new\": {\"tool_call\": true, \"release_date\": \"2026-02-01\"}}}}")
    (setf nle::*models-catalog-memo* nil)
    (nle::save-provider-api-key "anthropic" "sk-1")
    (with-frame-model (model 100 30 :session-id "setup-use-connected")
      (run-setup model)
      (let ((dialog model.dialog))
        (is (equal '("Suggested" "Anthropic") (subseq (row-labels dialog) 0 2)))
        (is (search "ctrl+e new key" (nle::picker-dialog-hint dialog)))
        (setf dialog.selection 1)
        (nle::picker-dialog-key model #\e :ctrl t)
        (is (eq :api-key (nle::connect-dialog-step dialog)) "Ctrl+E asks for a new key")
        (is (equal "anthropic" (nle::connect-dialog-provider-id dialog)))
        (nle::picker-dialog-key model :escape)
        (is (eq :provider (nle::connect-dialog-step dialog)))
        (setf dialog.selection 1)
        (nle::picker-dialog-key model :enter)
        (let ((next model.dialog))
          (is (eq :models (and (nle::picker-dialog-p next) (nle::picker-dialog-kind next))) "Enter goes on to the models")
          (is (equal '("anthropic" "new") (nle::picker-highlighted-value next)) "on the provider's newest")))
      (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape))
      (setf (gethash "anthropic" nle::*key-checks*) (list :refused "Anthropic refused the imported key"))
      (nle::execute-slash-command model "models" "")
      (let ((dialog model.dialog))
        (setf dialog.selection 1)
        (is (equal "Anthropic" (nle::picker-row-label (nth 1 (nle::picker-visible-rows dialog)))))
        (nle::picker-dialog-key model :enter)
        (is (eq :api-key (nle::connect-dialog-step dialog)) "a refused key's row asks for a new one")))))

(deftest connect-refuses-a-key-typed-as-a-provider-id ()
  ;; A key pasted where Other provider's id goes is refused before a key is
  ;; asked for, and the refusal does not say it back. Without this, a key
  ;; would be saved as a provider id and appear in the lists.
  (let ((pasted "sk-sel-0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f"))
    (dolist (id '("thinkingmachines" "subrouterai-1781313642618" "opencode-go"))
      (is (not (nle::key-shaped-p id)) (format nil "~a is a name" id)))
    (is (nle::key-shaped-p pasted))
    (with-frame-model (model 100 30 :session-id "connect-key-as-id")
      (nle::open-connect-dialog
       model (lambda () (list (nle::make-picker-row :label "Other provider" :value (list :custom t))))
       nil nil)
      (let ((dialog model.dialog))
        (nle::picker-dialog-key model :enter)
        (is (eq :custom-provider (nle::connect-dialog-step dialog)))
        (apply #'dialog-keys model (append (coerce pasted 'list) (list :enter)))
        (is (eq :result (nle::connect-dialog-step dialog)) "refused before the key step")
        (is (search "reads as an API key" (nle::connect-dialog-result-message dialog)))
        (is (not (search pasted (nle::connect-dialog-result-message dialog))) "and not said back")))))

(deftest setup-arrows-walk-the-steps-and-each-says-what-it-needs (with-setup-box ("{}"))
  ;; Every setup entrypoint walks provider/model/effort. A key field retains
  ;; its own arrows; model and effort can return to the preceding step.
  (write-temp-file nle::*models-catalog-path* "{\"anthropic\":{\"name\":\"Anthropic\",\"npm\":\"@ai-sdk/anthropic\",\"models\":{\"claude-x\":{\"tool_call\":true}}}}")
  (setf nle::*models-catalog-memo* nil)
  (with-frame-model (model 100 30 :session-id "setup-arrows")
          (with-stubbed-fdefinitions
              ((nle::slash-default-model-op (model tokens)
                 (nle::select-default-model :provider (first tokens) :model (second tokens))
                 "default model saved")
               (nle::target-snapshot (model)
                 (values nle::*provider* nle::*model* nil nil)))
    (run-setup model)
    (let ((dialog model.dialog))
      (is (equal "Step 1. Choose provider · step 1/3" dialog.context))
      (is (search "←→ steps" dialog.hint))
      (nle::picker-dialog-key model :left)
      (is (eq dialog model.dialog) "the first step has none before it")
      ;; past nodecode.ai's sign-in, to Anthropic's key
      (dialog-keys model :down :enter)
      (is (eq :api-key (nle::connect-dialog-step dialog)))
      (nle::picker-dialog-key model :left)
      (is (eq :api-key (nle::connect-dialog-step dialog)) "the key field keeps arrows")
      (dialog-keys model #\s #\k :enter :right)
      (is (eq :models (nle::picker-dialog-kind model.dialog)))
      (is (equal "Step 2. Choose model · step 2/3" (nle::picker-dialog-context model.dialog)))
      (nle::picker-dialog-key model :left)
      (is (nle::connect-dialog-p model.dialog))
      (dialog-keys model :right :enter)
      (is (eq :think (nle::picker-dialog-kind model.dialog)))
      (is (equal "Step 3. Choose reasoning effort" (nle::picker-dialog-context model.dialog)))
      (nle::picker-dialog-key model :left)
      (is (eq :models (nle::picker-dialog-kind model.dialog)))
      (nle::picker-dialog-key model :escape)
      (is (nle::connect-dialog-p model.dialog))
      (nle::picker-dialog-key model :escape)
      (is (ready-page-p model))))))

(deftest setup-opens-on-the-landing-screen ()
  ;; Without this, the first frame would be the walk's dialog under a blank
  ;; band, a raw session id and the gateway's address. The landing screen
  ;; sits over the walk's dialog, and neither the header's session id nor the
  ;; footer's gateway shows until the walk ends.
  (with-frame-model (model 120 40 :session-id "setup-landing" :http-port 4321)
    (nle::open-connect-dialog
     model (lambda () (list (nle::make-picker-row :label "OpenAI" :value (list :provider "openai")))) nil nil)
    (setf model.setup-walk (nle::make-setup-run))
    (let ((rows (nle::compose-frame model)))
      (is (nle::home-screen-p model))
      (is (rows-carry-p "█▀▄█" rows) "the logo")
      (is (rows-carry-p "OpenAI" rows) "over the dialog")
      (is (not (rows-carry-p "setup-landing" rows)) "and no session id"))
    ;; A status stands in the composer and keeps the landing screen; the
    ;; header names no session until the walk ends.
    (nle::show-status model "Google key works, saved to auth.json")
    (let ((rows (nle::compose-frame model)))
      (is (nle::home-screen-p model))
      (is (not (rows-carry-p "setup-landing" rows))))
    (is (notany (lambda (segments) (search "Gateway" (nle::segs-text segments))) (nle::footer-right-candidates model)))
    (setf model.setup-walk nil)
    (is (not (rows-carry-p "setup-landing" (nle::compose-frame model))) "an empty landing screen never exposes a raw session id")
    (is (search "Gateway" (nle::segs-text (first (nle::footer-right-candidates model)))))))

(deftest setup-copy-speaks-in-the-operators-words ()
  ;; The walk numbers its steps, names a secret by what it holds, and ends
  ;; on a page with one line per thing it did.
  (let ((run (nle::make-setup-run :steps '(:provider :model :effort))))
    (is (equal "Setup · step 2/3" (nle::setup-context run :model "Setup")))
    (is (equal "Import" (nle::setup-context run :import "Import"))))
  (is (equal "bot token"
             (nlk:secret-pair-noun (cons (nlk:make-section-field "bot_token_env" :env)
                                         (nlk:make-section-field "bot_token_file" :path)))))
  (is (equal "default false"
             (nle::section-field-value-text
              (nlk::make-section-field "reactions" :boolean :default :false) nil)))
  (let ((text (page-text (nle::make-setup-run :providers (list "acme") :model '("acme" "m1")
                                              :notes (list "reasoning effort: high"))
                         :caveats nil)))
    (is (equal (format nil "✓ provider    acme~%✓ model       acme/m1~%· note        reasoning effort: high")
               text)))
  ;; Given the row's width, a value wider than its column goes on under
  ;; itself, a model id longer than the column cut at its edge: it went on at
  ;; the left edge, under the labels (ca-109).
  (let ((lines (nle::split-lines
                (page-text (nle::make-setup-run :providers (list "anthropic")
                                                :model '("anthropic" "claude-sonnet-5-5-20260901"))
                           :width 30
                           :caveats (list (list "check" (format nil "start: a condition~%reported on two lines")))))))
    (is (every (lambda (line) (<= (nle::text-display-width line) 30)) lines))
    ;; the id goes on in its own column, and so does a value's second line
    (is-table (needle below)
      (let ((at (position needle lines :test #'search)))
        (and at (uiop:string-prefix-p (make-string 14 :initial-element #\Space)
                                      (nth (+ at below) lines))))
      ("✓ model " 1) ("reported on" 0))))

(deftest setup-summary-names-the-model-only-when-the-config-holds-it (with-setup-box ())
  ;; A pick whose write failed moved the process and left the file, and the
  ;; closing block named a model the next launch would not find (vr-05).
  (flet ((summary (run)
           (page-text run))
         (recorded (picked &aux (run (nle::make-setup-run :providers (list "acme"))))
           (nle::setup-record-model run picked)
           run))
    (let ((run (recorded '("acme" "m1"))))
      (is (null (nle::setup-run-model run)) "no config written, no model claimed")
      (is-carrying (text (summary run))
        "– model       not saved"
        "acme/m1 was not saved: "
        " does not hold the model; pick it again with /models"
        (:absent "✓ model       acme/m1" "and none named")))
    (nle::persist-shared-model-pin "default_model" "acme" "m1")
    (is (equal '("acme" "m1") (nle::setup-run-model (recorded '("acme" "m1")))) "written, so claimed")
    (is (equal '("acme" "m1") (nle::setup-run-model (recorded '("m1")))) "a model-only pick too")
    (let ((run (recorded '("acme" "m2"))))
      (is (null (nle::setup-run-model run)) "the config holds another model")
      (is (equal '("acme" "m2") (first (nle::setup-run-unsaved run))) "the pick is kept as it was made"))
    (write-temp-file nle::*shared-config-path* "{ \"general\": { \"default_model\": ")
    (setf nle::*shared-config-memo* nil)
    (let ((run (recorded '("acme" "m1"))))
      (is (null (nle::setup-run-model run)))
      (is-carrying (text (summary run))
        (is (search (nle::home-abbreviated (uiop:native-namestring nle::*shared-config-path*))
                    text) "the file is named")
        "is not valid JSONC (byte "
        "fix it or move it aside, then pick the model again with /models")
      ;; the file mended, a pick in the same walk lands and the unsaved is gone
      (write-temp-file nle::*shared-config-path* "{}")
      (nle::persist-shared-model-pin "default_model" "acme" "m1")
      (nle::setup-record-model run '("acme" "m1"))
      (is (equal '("acme" "m1") (nle::setup-run-model run)))
      (is-carrying (text (summary run))
        (:absent "not saved" "no word of the earlier failure")))))

(deftest setup-suggests-the-providers-newest-model-with-tool-calls ()
  (with-temp-file (nle::*models-catalog-path*
                   :contents "{\"acme\":{\"models\":{\"a\":{\"release_date\":\"2026-01-01\",\"tool_call\":true},\"b\":{\"release_date\":\"2026-05-01\",\"tool_call\":false},\"c\":{\"release_date\":\"2026-03-01\",\"tool_call\":true}}}}")
    (let ((nle::*models-catalog-memo* nil))
      (is (equal "c" (nle::suggested-model "acme")))
      (is (null (nle::suggested-model "nobody")) "no provider, no suggestion"))))

(deftest setup-says-which-key-did-not-answer-and-keeps-its-model ()
  ;; After a refused imported key the step line read `ready', its row
  ;; `connected', the prompt `the imported key did not answer', naming no
  ;; provider and no way on, and Enter at the model step replaced the imported
  ;; model with the newest (am-07). The prompt names the provider and what
  ;; happened, the selection rests on the row Enter pastes a new key on, and
  ;; the model step starts on the model the config names.
  (with-model-listings (:catalog "{\"anthropic\":{\"name\":\"Anthropic\",\"npm\":\"@ai-sdk/anthropic\",\"models\":{
                                    \"a\":{\"tool_call\":true,\"release_date\":\"2026-01-01\"},
                                    \"b\":{\"tool_call\":true,\"release_date\":\"2026-06-01\"}}}}"
                        :config "{\"general\": {\"default_model\": {\"provider\": \"anthropic\", \"model\": \"a\"}}}")
    (nle::save-provider-api-key "anthropic" "sk-imported")
    (let ((run (nle::make-setup-run :providers (list "anthropic") :steps '(:provider :model :effort))))
      (setf (gethash "anthropic" nle::*key-checks*) (list :refused "Anthropic refused the imported key"))
      (with-frame-model (model 100 30 :session-id "setup-attention")
        (nle::setup-stage-provider model run)
        (let ((dialog model.dialog))
          (is (equal "Step 1. Choose provider · step 1/3" (nle::connect-dialog-walk dialog)))
          (is (equal "Anthropic refused the imported key · Enter on its row pastes a new key"
                     dialog.search-placeholder))
          (is (equal "2 models  key refused (auth.json)"
                     (nle::picker-row-detail (find "Anthropic" (nle::connect-dialog-rows dialog)
                                                   :key #'nle::picker-row-label :test #'equal))))
          ;; resting on its row
          (is (equal '(:provider "anthropic") (subseq (nle::picker-highlighted-value dialog) 0 2)))
          (dialog-keys model :enter)
          (is (eq :api-key (nle::connect-dialog-step dialog)) "where Enter opens the key prompt"))
        (setf model.dialog nil)
        (nle::setup-stage-model model run)
        (let ((dialog model.dialog))
          ;; the model the config names, not the newest
          (is (equal '("anthropic" "a") (nle::picker-highlighted-value dialog)))
          (is (search "a, the model this home names" dialog.search-placeholder)))
        ;; a named model the list does not carry is said, and the newest is offered
        (nle::persist-shared-model-pin "default_model" "anthropic" "gone")
        (setf model.dialog nil)
        (nle::setup-stage-model model run)
        (let ((dialog model.dialog))
          (is (equal '("anthropic" "b") (nle::picker-highlighted-value dialog)))
          (is (equal "gone, the model this home names, is not listed · suggested: b, the newest with tool calls · type to search"
                     dialog.search-placeholder)))))))

(deftest setup-verb-and-slash-are-in-the-tables ()
  (nlk:bind (((serve _ _ _ ephemeral setup)
              (nle::parse-launch-arguments '("setup" "--ephemeral"))))
    (is (null serve))
    (is ephemeral)
    (is setup "`setup' asks for the wizard on the first frame"))
  (is (signals-error nle::launch-usage-error
        (nle::parse-launch-arguments '("serve" "setup"))))
  (is (find "setup" nle::*slash-commands* :key #'nle::slash-command-name :test #'string=)))

(deftest connect-list-takes-the-fetched-catalog-when-it-lands ()
  ;; A box without the models.dev cache opens /models on the Other-provider
  ;; entry alone; the fetch's message fills the list in, or names the failure
  ;; on the search row, the step's header, with the keys saved here that wait
  ;; on it for an endpoint. Said as the subtitle it never showed, and an
  ;; offline box with a deepseek key read an empty list (xm-204).
  (with-temp-file (nle::*models-catalog-path*)
    (with-temp-file (nle::*auth-file-path*)
      (let ((nle::*models-catalog-memo* nil)
            (nle::*shared-config-path* nil)
            (nle::*shared-config-memo* nil)
            (nle::*provider-models-cache-path* nil))
        (nle::save-provider-api-key "deepseek" "sk-offline")
        (with-frame-model (model 80 24 :session-id "catalog-fetch" :slash "models")
          (let ((dialog model.dialog))
            (is (nle::connect-dialog-p dialog))
            ;; The Main tab's rows: the dialog holds every tab's.
            (is (= 1 (length (nle::picker-visible-rows dialog))))
            (nle::install-fetched-catalog model "host not found")
            ;; the way on inside the 74 columns an 80-column row shows
            (is (equal "no models.dev list (host not found) · deepseek needs it · reopen online"
                       dialog.search-placeholder))
            (nle::save-provider-api-key "deepseek" nil)
            (write-temp-file nle::*models-catalog-path* +connect-catalog-fixture+)
            (nle::install-fetched-catalog model nil)
            (is (= 5 (length (nle::picker-visible-rows dialog))) "three providers under a heading, and Other provider")
            (is (equal "anthropic" (getf (nle::picker-row-value (first (nle::picker-visible-rows dialog))) :provider)))
            (is (zerop dialog.selection) "resting on the first row")
            (is (equal "Search providers and models" dialog.search-placeholder) "the failure goes with it")))))))

(defun import-answer (imported skipped error &rest members)
  "An import route's answer: its summary of IMPORTED, SKIPPED and ERROR items
and no conflict, then MEMBERS, alternating keys and values."
  (apply #'nlk:make-json-object "summary" (nlk:json-object "imported" imported "skipped" skipped
                                                           "conflict" 0 "error" error)
         members))

(defun served-p-config (port &optional (lead ""))
  "The shared config naming p/m, p served at 127.0.0.1:PORT; LEAD leads its members."
  (format nil "{~a\"general\": {\"default_model\": {\"provider\": \"p\", \"model\": \"m\"}}, ~
               \"providers\": {\"p\": {\"sdk\": \"openai-completions\", ~
               \"base_url\": \"http://127.0.0.1:~d/v1\"}}}" lead port))

(defmacro with-hermes-box ((marker tag &optional config) &body body &aux (box (gensym "BOX")))
  "Run BODY over a scratch box whose agent home carries a Hermes config: MARKER
is bound to the import marker's path, the shared config is a fresh file that
holds CONFIG when one is given, and the import stage's worlds are Hermes alone."
  ;; TAG names the scratch paths; the box and the marker go however BODY
  ;; leaves.
  `(let ((,box (temp-path ,(format nil "agent-box~@[-~a~]" tag)))
         (,marker (temp-path ,(format nil "import-marker~@[-~a~]" tag) "txt")))
     (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree ,box :validate t))
                        (ignore-errors (delete-file ,marker)))
       (write-temp-file (merge-pathnames ".hermes/config.yaml" ,box) "model: {}")
       (with-temp-file (nle::*shared-config-path* :setf t)
         ,@(when config `((write-temp-file nle::*shared-config-path* ,config)))
         ;; a landed import re-seeds the selection from CONFIG: it must not outlive the box
         (with-saved-globals (nle::*provider* nle::*model* nle::*endpoint*
                              nle::*auxiliary-model* nle::*model-config-refusal*)
           (let ((nlk::*agent-home* (uiop:ensure-directory-pathname ,box))
                 (nle::*import-worlds* (list (nlk:find-agent-world "hermes")))
                 (nle::*import-marker* ,marker))
             ,@body))))))

(defmacro with-held-verify ((jobs marker tag config report) &body body)
  "Run BODY in WITH-HERMES-BOX over CONFIG with p connected at 127.0.0.1:1 and
every import call answering three keys and REPORT; JOBS, bound fresh, gathers
what the walk sends off the loop, newest first, and only a first job runs."
  `(let ((,jobs '()))
     (with-hermes-box (,marker ,tag ,config)
       (with-stubbed-fdefinitions
           ((nle::provider-connected-p () t)
            (nle::effective-endpoint (&optional lane) "http://127.0.0.1:1/v1/chat/completions")
            (nle::setup-import-call (model method request takeover &key progress)
              (import-answer 3 0 0
               "applied" (equal method "apply")
               "worlds" (vector "hermes")
               "items" (vector (nlk:json-object "kind" "providers" "status" "imported"
                                                "destination" "general.default_model"))
               "report" ,report))
            (nle::off-loop (model thunk continuation)
              (when (< (length (push (cons thunk continuation) ,jobs)) 2)
                (funcall continuation model (funcall thunk)))))
         ,@body))))

(deftest setup-import-asks-before-it-brings-a-past-over-and-walks-the-verb (let ((calls '())
                                                                                (scopes '())))
  ;; A coding-agent home on the box opens the wizard on the import stage,
  ;; and the offer asks before it reads: nothing is called until Import.
  ;; Then the apply runs with no plan before it -- the keys alone while the
  ;; walk waits, the rest beside it -- the marker is left, a live foreign
  ;; gateway is left running (the takeover is off), and the next /setup goes
  ;; straight to the provider. A typed request keeps its preview: `nodecode
  ;; import hermes --dry-run' still walks the stage alone, and --keep-running
  ;; plans without the takeover.
  (with-hermes-box (marker nil)
    (with-stubbed-fdefinitions
        ((nle::setup-import-call (model method request takeover &key progress)
           (push (list method (getf request :source) (getf request :dry-run) takeover)
                 calls)
           (push (list (getf request :only) (getf request :without)) scopes)
           (import-answer 4 1 0
            "worlds" (vector "hermes")
            "grants" (vector "Hermes Agent")
            "running" (vector "systemd user unit hermes-gateway.service is active")
            "bots" (vector "channels.discord")
            "takeover" "hermes-gateway.service"
            "report" "the report")))
      (with-frame-model (model 100 30 :session-id "setup-import")
        (is (null (run-setup model)) "/setup opens silently")
        (is (eq :setup-import-offer (nle::picker-dialog-kind model.dialog)) "on the question")
        (is (null calls) "which reads nothing before it is answered")
        (nle::picker-dialog-key model :enter)
        (is (equal '(("apply" nil nil nil) ("apply" nil nil nil)) calls) "Import: two applies, no plan")
        ;; the keys while the walk waits, and beside them the rest but the
        ;; conversations -- asked first, where nothing runs off the loop
        (is (equal '((nil "providers,sessions") ("providers" nil)) (reverse scopes)))
        (is (not (eq :setup-import-confirm
                     (and (nle::picker-dialog-p model.dialog)
                          (nle::picker-dialog-kind model.dialog)))))
        (is (probe-file marker) "the marker is left")
        (let ((dialog model.dialog))
          (is (nle::connect-dialog-p dialog))
          (is (search "Hermes Agent's sign-in stays there" (third-row-text dialog))))
        (setf model.dialog nil)
        ;; Offered once: the marker keeps the next /setup off the stage.
        (setf calls '())
        (run-setup model)
        (is (null calls) "no second offer")
        (setf model.dialog nil)
        ;; The verb form walks the stage alone, dry, and still previews.
        (setf model.setup-requested (list :import "hermes" :dry-run t :source "/tmp/h"))
        (nle::slash-setup-op model)
        (is (equal '("plan" "/tmp/h" t t) (first calls)) "the verb's request reaches the plan")
        (is (ready-page-p model) "a dry run asks nothing, and ends on the walk's last page")
        (is (search "dry run" (page-said model)) "which says so")
        (is (null (said-rows model)) "and the chat holds none of it")
        (setf model.setup-requested (list :import "hermes" :dry-run t :keep-running t))
        (nle::slash-setup-op model)
        (is (equal '("plan" nil t nil) (first calls)))))))

(deftest setup-import-source-and-a-live-gateway-reach-the-closing-line
    (with-temp-trees ((root "setup-import-tree")))
  ;; The migrate path ends on ONE line, and it says two things an operator
  ;; would want told without having asked: where it came from, and that
  ;; somebody else's gateway is still running with its bots — with the
  ;; command that moves them here, which asks first. Discovery leaves the
  ;; provider Suggested but still asks the operator to choose a model; the
  ;; numbered path is provider/model/effort for /setup too. Left/Right keep
  ;; those choices reachable, and Escape eventually closes with the import's
  ;; source, live-gateway caveat and conversations-left command.
  (let ((name (string-downcase (format nil "nl-setup-import-~36r" (random (expt 36 6))))))
    ;; ROOT/src/ as the source root beside ROOT/cells/NAME: a catalog of one.
    (ensure-directories-exist (merge-pathnames "src/" root))
    (write-cell-folder (merge-pathnames "cells/" root) name
                        (list (cons "cell" (format nil "(defpackage #:~a (:use #:cl))" name))))
    (with-saved-globals ((nlk::*source-root* (merge-pathnames "src/" root)))
      (with-hermes-box (marker "line" (served-p-config 9))
        (with-stubbed-fdefinitions
            ((nle::provider-connected-p () t)
             (nle::connected-provider-ids () (list "p"))
             (nle::picker-connected-provider-ids () (list "p"))
             (nle::models-catalog-picker-rows ()
               (list (nle::make-picker-row :label "m" :value '("p" "m"))))
             ;; The keys while the walk waits; the rest, the bot with it, behind.
             (nle::setup-import-call (model method request takeover &key progress)
               (if (equal "providers" (getf request :only))
                   (import-answer 3 0 0
                    "applied" t
                    "sessions_left" t
                    "worlds" (vector "hermes" "codex")
                    "running" (vector)
                    "bots" (vector)
                    "takeover" :null
                    "report" "the keys' report")
                   (import-answer 4 2 0
                    "applied" t
                    "worlds" (vector "hermes" "codex")
                    "running" (vector)
                    "bots" (vector "channels.telegram")
                    "takeover" "hermes-gateway.service"
                    "report" "the report"))))
          (with-frame-model (model 100 30 :session-id "setup-import-line")
            (run-setup model)
            (nle::picker-dialog-key model :enter)
            (let ((dialog (nle::chat-model-dialog model)))
              (is (nle::connect-dialog-p dialog) "the provider list opens after an import too")
              (when (nle::connect-dialog-p dialog)
                (is (equal "Step 1. Choose provider · step 1/3" (nle::picker-dialog-context dialog)))
                (is (equal "Suggested" (first (row-labels dialog))))
                (nle::picker-dialog-key model :enter)))
            (let ((dialog (nle::chat-model-dialog model)))
              (is (and (nle::picker-dialog-p dialog)
                       (eq :models (nle::picker-dialog-kind dialog))) "an imported model never skips the operator's model choice")
              (when (nle::picker-dialog-p dialog)
                (is (equal "Step 2. Choose model · step 2/3" (nle::picker-dialog-context dialog)))
                (nle::picker-dialog-key model :left)
                (is (nle::connect-dialog-p (nle::chat-model-dialog model)) "Left walks back to providers")
                (nle::picker-dialog-key model :right)
                (is (equal "Step 2. Choose model · step 2/3"
                           (nle::picker-dialog-context (nle::chat-model-dialog model))))))
            (nle::picker-dialog-key model :escape)
            (is (nle::connect-dialog-p (nle::chat-model-dialog model)) "Esc from models returns to providers")
            (nle::picker-dialog-key model :escape)
            (is (ready-page-p model) "Esc ends the walk on its last page")
            (is (null (said-rows model)) "and the chat holds none of it")
            (is-carrying (text (nlk:one-line (page-said model)))
              ("imported from hermes and codex: 7 imported"
               "the closing line names every world it read")
              ("hermes-gateway.service is still running" "and says whose gateway it left")
              ("channels.telegram stays with it" "and which bot stays")
              ("`nodecode import hermes' asks first, then moves it here"
               "with the command that finishes it")
              (:absent "took over" "a yes to the first frame's offer takes nothing over")
              ("/import --only sessions"
               "and the conversations it left read as waiting, not lost"))))))))

(deftest setup-import-verb-asks-before-it-takes-a-live-gateway-over (let ((calls '())))
  ;; `nodecode import hermes' is somebody asking, so it shows the plan and
  ;; waits: Import takes the running gateway's bots over (the takeover is on in
  ;; the apply), keeping it leaves the gateway alone and says how to finish, Skip
  ;; writes nothing. Only the unasked first frame never takes over.
  (with-hermes-box (marker "verb")
    (with-stubbed-fdefinitions
        ((nle::setup-import-call (model method request takeover &key progress)
           (push (list method takeover) calls)
           (import-answer 4 0 0
            "applied" (equal method "apply")
            "worlds" (vector "hermes")
            "running" (vector "systemd user unit hermes-gateway.service is active")
            "bots" (vector "channels.telegram")
            "takeover" "hermes-gateway.service"
            "items" (vector)
            "report" "the report")))
      (with-frame-model (model 100 30 :session-id "setup-import-verb")
        (flet ((walk (&rest keys)
                 (setf calls '())
                 (setf model.setup-requested (list :import "hermes"))
                 (nle::slash-setup-op model)
                 (let* ((dialog model.dialog)
                        (asked (and (nle::picker-dialog-p dialog)
                                    (eq :setup-import-confirm dialog.kind)
                                    (mapcar #'nle::picker-row-value dialog.rows))))
                   (dolist (key keys) (nle::picker-dialog-key model key))
                   (values asked (reverse calls) (nlk:one-line (page-said model))))))
          (nlk:bind (((asked calls notice) (walk :enter)))
            (is (equal '(:takeover :keep :skip) asked) "the verb asks, and offers the takeover first")
            (is (equal '(("plan" t) ("apply" t)) calls) "Import takes the gateway over")
            (is (search "took over hermes-gateway.service" notice))
            (is (search "systemctl --user enable --now hermes-gateway.service" notice) "and the command that gives it back")
            ;; The plan and its landing are the dialog's, its question and its
            ;; last page: the chat holds none of them (ip-01).
            (is (null (said-rows model))))
          (nlk:bind (((_ calls notice) (walk :down :enter)))
            (is (equal '(("plan" t) ("apply" nil)) calls) "the second row leaves it running")
            (is (search "hermes-gateway.service is still running" notice))
            (is (search "`nodecode import hermes' asks first" notice)))
          (nlk:bind (((_ calls notice) (walk :escape)))
            (is (equal '(("plan" t)) calls) "Esc writes nothing")
            (is (search "import skipped" notice))))))))

(deftest setup-import-remembers-a-skip-and-never-imports-that-world-unasked (let ((calls '())))
  ;; Without a recorded skip, a bare launch would read the same home
  ;; and import it unasked: keys, bot token, memory, sessions, cron. Skip or
  ;; Esc puts the world on the marker as declined; the walk's unasked import
  ;; passes it over and still reads a world nobody declined; asking by name
  ;; offers it again, and taking it clears the Skip.
  (with-hermes-box (marker "declined")
    (write-temp-file (merge-pathnames ".codex/config.toml" nlk::*agent-home*) "model = \"m\"")
    (with-stubbed-fdefinitions
        ((nle::setup-import-call (model method request takeover &key progress)
           (let ((import (getf request :import)))
             (push (list method import) calls)
             (import-answer 4 0 0
              "applied" (equal method "apply")
              "worlds" (coerce (if (consp import) import (list import)) 'vector)
              "running" (vector) "bots" (vector) "takeover" :null
              "items" (vector) "report" "the report"))))
      (with-frame-model (model 100 30 :session-id "setup-import-declined")
        (flet ((walk (requested &rest keys)
                 (setf calls '() model.dialog nil model.setup-requested requested)
                 (progn (setf (nle::gateway-model-last-sequence model) 0)
                        (nle::slash-setup-op model))
                 (let ((asked (and (nle::picker-dialog-p model.dialog)
                                   (eq :setup-import-confirm (nle::picker-dialog-kind model.dialog)))))
                   (dolist (key keys) (nle::picker-dialog-key model key))
                   (values (reverse calls) asked))))
          (multiple-value-bind (calls asked) (walk (list :import "hermes") :down :enter)
            (is asked "the verb asks")
            (is (equal '(("plan" "hermes")) calls) "Skip writes nothing"))
          (is (equal '("hermes") (nle::declined-import-worlds)) "and is recorded by world")
          (is (not (nle::import-done-p)) "a Skip is no import")
          (let ((nle::*import-worlds* (list (nlk:find-agent-world "hermes"))))
            (is (null (walk t)) "the first frame, `setup' and /setup import nothing declined"))
          ;; A world nobody declined is still offered, alone, and Import brings
          ;; it over: its keys, then the rest.
          (let ((nle::*import-worlds* (mapcar #'nlk:find-agent-world '("hermes" "codex"))))
            (is (null (walk t)) "the offer reads nothing before it is answered")
            (is (equal '(("apply" ("codex")) ("apply" ("codex"))) (walk t :enter))))
          (is (equal '("hermes") (nle::declined-import-worlds)) "and the Skip stands")
          (multiple-value-bind (calls asked) (walk (list :import "hermes") :enter)
            (is asked "asking by name offers a declined world again")
            (is (equal '(("plan" "hermes") ("apply" "hermes")) calls)))
          (is (null (nle::declined-import-worlds)) "taking it clears the Skip")
          (write-temp-file marker "{}")
          (walk (list :import "hermes") :escape)
          (is (equal '("hermes") (nle::declined-import-worlds)) "Esc is a Skip too")
          (let ((nle::*import-worlds* (list (nlk:find-agent-world "hermes"))))
            (is (null (walk t)))))))))

(deftest setup-first-frame-names-what-it-found-and-a-skip-reads-nothing (let ((calls '())))
  ;; Another harness's home holds the operator's keys and bot tokens: the
  ;; first frame names the agents it found and opens none of their files
  ;; until Import. Skip or Esc reads and writes nothing but the Skip, the
  ;; walk goes on to the provider, and the next launch does not ask again.
  (with-hermes-box (marker "offer")
    (with-stubbed-fdefinitions
        ((nle::setup-import-call (model method request takeover &key progress)
           (push method calls)
           (import-answer 1 0 0 "applied" t "worlds" (vector "hermes")
                          "items" (vector) "report" "the report")))
      (with-frame-model (model 100 30 :session-id "setup-import-offer")
        (flet ((walk (&rest keys)
                 (setf calls '() model.dialog nil)
                 (run-setup model)
                 (let ((asked (and (nle::picker-dialog-p model.dialog)
                                   (eq :setup-import-offer (nle::picker-dialog-kind model.dialog))
                                   model.dialog)))
                   (dolist (key keys) (nle::picker-dialog-key model key))
                   asked)))
          (let ((asked (walk)))
            (is asked "a home on the box is a question")
            (when asked
              ;; naming the agent it found
              (is (search "Found Hermes Agent on this machine" (nle::picker-dialog-subtitle asked)))
              (is (equal '(:import :skip) (mapcar #'nle::picker-row-value (nle::picker-dialog-rows asked))))))
          (is (null calls) "nothing is read before the answer")
          (dolist (keys '((:escape) (:down :enter)))
            (ignore-errors (delete-file marker))
            (apply #'walk keys)
            (is (null calls) "Skip and Esc read nothing")
            (is (equal '("hermes") (nle::declined-import-worlds)) "and put the world on the marker")
            (is (nle::connect-dialog-p model.dialog) "the walk goes on to the provider"))
          ;; a Skip is not asked again
          (is (null (walk)))
          (is (null calls)))))))

(deftest setup-first-frame-imports-only-the-agents-ticked (let ((calls '())))
  ;; Two homes on the box: the question offers them all, one by one, or none.
  ;; Choose which opens a checklist with every agent ticked; Esc goes back to
  ;; the question. Space unticks one and Enter brings the rest over, the
  ;; unticked one put on the marker as declined; none ticked is a Skip.
  (with-hermes-box (marker "choose")
    (write-temp-file (merge-pathnames ".codex/config.toml" nlk::*agent-home*) "model = \"m\"")
    (with-stubbed-fdefinitions
        ((nle::setup-import-call (model method request takeover &key progress)
           (push (list method (getf request :import)) calls)
           (import-answer 1 0 0 "applied" t "worlds" (coerce (getf request :import) 'vector)
                          "items" (vector) "report" "the report")))
      (let ((nle::*import-worlds* (mapcar #'nlk:find-agent-world '("hermes" "codex"))))
        (with-frame-model (model 100 30 :session-id "setup-import-choose")
          (flet ((walk (&rest keys)
                   (setf calls '() model.dialog nil)
                   (ignore-errors (delete-file marker))
                   (run-setup model)
                   (dolist (key keys) (nle::picker-dialog-key model key))
                   (reverse calls))
                 (kind ()
                   (and (nle::picker-dialog-p model.dialog) (nle::picker-dialog-kind model.dialog))))
            (is (null (walk)) "the question reads nothing")
            (is (equal '(:import :choose :skip) (mapcar #'nle::picker-row-value (nle::picker-dialog-rows model.dialog))))
            (is (search "Import from all 2" (nle::picker-row-label (first (nle::picker-dialog-rows model.dialog)))))
            (is (null (walk :down :enter :escape)) "Esc on the checklist reads nothing")
            (is (eq :setup-import-offer (kind)) "and goes back to the question")
            (walk :down :enter)
            (is (eq :setup-import-choose (kind)) "Choose which opens the checklist")
            (let* ((first-world (nle::picker-row-value (first (nle::picker-dialog-rows model.dialog))))
                   (other (if (equal first-world "hermes") "codex" "hermes")))
              (is (null (set-exclusive-or '("hermes" "codex") (nle::picker-dialog-marks model.dialog) :test #'equal)) "every agent starts ticked")
              (nle::picker-dialog-key model #\Space)
              (nle::picker-dialog-key model :enter)
              (is (equal (list (list "apply" (list other)) (list "apply" (list other))) (reverse calls)) "Enter brings the ticked one over")
              (is (equal (list first-world) (nle::declined-import-worlds)) "and the unticked one is declined"))
            (is (null (walk :down :enter #\Space :down #\Space :enter)) "none ticked reads nothing")
            (is (null (set-exclusive-or '("hermes" "codex") (nle::declined-import-worlds) :test #'equal)) "and is a Skip")))))))

(deftest setup-import-checks-the-imported-target-before-it-claims-it ()
  ;; A key that worked in another harness is not a key that works now. When
  ;; the import selected a default model, the walk asks that target to
  ;; answer once before it says the provider is there: a refusal is named on
  ;; the closing line in the provider's own words, and the provider list says
  ;; which provider refused the imported key. An endpoint nothing listens on
  ;; is said there too, as the server's (xm-201). An answer ends the walk on
  ;; the imported model, the first frame taking a task.
  (flet ((walk (verdict &aux (calls '()))
           (with-hermes-box (marker "verify"
                             "{\"general\": {\"default_model\": {\"provider\": \"p\", \"model\": \"m\"}},
                                 \"providers\": {\"p\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:1/v1\"}}}")
             (with-stubbed-fdefinitions
                 ((nle::provider-connected-p () t)
                  (nle::effective-endpoint (&optional lane) "http://127.0.0.1:1/v1/chat/completions")
                  (nle::setup-import-call (model method request takeover &key progress)
                    (push method calls)
                    (if (equal method "verify")
                        verdict
                        (nlk:json-object
                         "summary" (nlk:json-object "imported" 3 "skipped" 0
                                                    "conflict" 0 "error" 0)
                         "applied" (equal method "apply")
                         "worlds" (vector "hermes")
                         ;; the keys set the default model; the rest, beside them --
                         ;; asked first, where nothing runs off the loop -- sets none
                         "items" (if (equal "providers" (getf request :only))
                                     (vector (nlk:json-object
                                              "kind" "providers" "status" "imported"
                                              "destination" "general.default_model"
                                              "reason" "m on p, the model Hermes ran most lately")
                                             (nlk:json-object
                                              "kind" "providers" "status" "skipped"
                                              "destination" "general.default_model"
                                              "reason" "o is what Claude Code ran most lately, on a sign-in that stays there"))
                                     (vector))
                         "report" "the report"))))
               (with-frame-model (model 100 30 :session-id "setup-import-verify")
                 (run-setup model)
                 (nle::picker-dialog-key model :enter)
                 (let ((dialog model.dialog))
                   (values (reverse calls)
                           dialog
                           ;; the walk's last page, when it ended
                           (page-said model))))))))
    (nlk:bind (((calls dialog _)
                (walk (nlk:json-object "ok" :false "provider" "p" "model" "m" "code" "auth"
                                       "reason" "HTTP 401 invalid api key"))))
      (is (equal '("apply" "apply" "verify") calls))
      (is (nle::connect-dialog-p dialog))
      (is (search "p refused the imported key" (third-row-text dialog))))
    (nlk:bind (((calls dialog notice)
                (walk (nlk:json-object "ok" t "provider" "p" "model" "m" "reason" :null))))
      (is (equal '("apply" "apply" "verify") calls))
      (is (eq :setup-ready (nle::picker-dialog-kind dialog)) "an answer ends the walk on its last page")
      (is-carrying (text (nlk:one-line notice))
        "✓ model p/m" "m on p, the model Hermes ran most lately — /models changes it"
        "o is what Claude Code ran most lately, on a sign-in that stays there"
        (:absent "did not answer") (:absent "not checked")))
    (multiple-value-bind (calls dialog notice)
        (walk (nlk:json-object "ok" :false "provider" "p" "model" "m" "code" "connection_refused"
                               "reason" "provider request failed [p m]: nothing listens at 127.0.0.1:1 — is the server running?"))
      (declare (ignore notice))
      (is (equal '("apply" "apply" "verify") calls))
      ;; a port nothing listens on is no verdict on the imported key: the server's, said plainly
      (is-carrying (text (third-row-text dialog))
        "nothing listens at 127.0.0.1:1 for p — is its server running?"
        (:absent "refused") (:absent "provider request failed")))
    ;; Any other failure may not be the key's: the row says the provider and
    ;; what happened, never the whole report with its remedies.
    (nlk:bind (((calls dialog _)
                (walk (nlk:json-object "ok" :false "provider" "p" "model" "m" "code" "server_error"
                                       "status" 500 "reason" "down"))))
      (is (equal '("apply" "apply" "verify") calls))
      (is-carrying (text (and (nle::connect-dialog-p dialog) dialog.search-placeholder))
        "p key was not checked: 127.0.0.1:1 answered HTTP 500"
        (:absent "provider request failed") (:absent "connect it again") (:absent "refused")))
    ;; Below HTTP the reason is the transport fact the request named, never a
    ;; condition's class: `PROVIDER-ERROR: provider request failed [...]: host
    ;; not found'.
    (nlk:bind (((_ dialog _)
                (walk (nlk:json-object "ok" :false "provider" "p" "model" "m"
                                       "code" "host not found at api.p.test:443" "reason" "host not found at api.p.test:443"))))
      (is (equal "p key was not checked: host not found at api.p.test:443"
                 (and (nle::connect-dialog-p dialog) dialog.search-placeholder))))))

(deftest setup-import-verify-names-its-endpoint-gives-up-on-a-deadline-and-yields-to-esc ()
  ;; A stopped local server never answers a dial that polls to its own timeout,
  ;; and the check said `please wait' for 28 to 57 seconds, named nothing, and
  ;; ignored Esc (am-08). Now the wait names host:port, the walk sets its own
  ;; short deadline, Esc skips the check, and either way the provider stays
  ;; imported with one line saying it was not verified. JOBS holds what the walk
  ;; sent off the loop, newest first; the apply runs at once and the
  ;; check and its deadline are held for the test to settle in the order it likes.
  (with-held-verify (jobs marker "verify-wait" (served-p-config 1) "the report")
    (with-frame-model (model 100 30 :session-id "setup-import-verify-wait")
      (flet ((walk ()
               (setf jobs '()
                     model.dialog nil
                     model.state (nle::initial-chat-state)
                     model.setup-requested (list :import "hermes" :yes t))
               (nle::slash-setup-op model)
               (values (cdr (second jobs)) (cdr (first jobs))))
             (answer (&optional (ok t))
               (nlk:json-object "ok" (if ok t :false) "provider" "p" "reason" "HTTP 401")))
        ;; The wait: what it is asking, and what leaves it.
        (multiple-value-bind (verified deadline) (walk)
          (declare (ignore verified deadline))
          (let ((dialog model.dialog))
            (is (and (nle::picker-dialog-p dialog) (eq :setup-import dialog.kind)))
            (is (search "127.0.0.1:1" dialog.subtitle) "it names the endpoint it is asking")
            (is (search "… p           asking 127.0.0.1:1, up to 4 seconds" (page-said model)) "in its live line")
            (is (search "esc" dialog.hint) "and says Esc skips it")))
        ;; Esc: on at once, the provider stays, a late answer changes nothing.
        (multiple-value-bind (verified) (walk)
          (nle::picker-dialog-key model :escape)
          (is (ready-page-p model))
          (is-carrying (text (nlk:one-line (page-said model)))
            ("p key was not checked: the check was skipped — /models takes it again and checks it"
             "the skip is said in one line"))
          (let ((said (page-said model)))
            (is (eq :quiet (funcall verified model (answer nil))) "an answer after the skip is dropped")
            (is (equal said (page-said model)))))
        ;; The deadline: the walk goes on without knowing, and does not call it a refusal.
        (multiple-value-bind (verified deadline) (walk)
          (funcall deadline model :late)
          (is (ready-page-p model))
          (is-carrying (text (nlk:one-line (page-said model)))
            ("p key was not checked: nothing answered at 127.0.0.1:1 within 4 seconds" "the wait is named")
            (:absent "refused"))
          (let ((said (page-said model)))
            (is (eq :quiet (funcall verified model (answer nil))) "an answer after the deadline is dropped")
            (is (equal said (page-said model)))))
        ;; An answer first: nothing to say, and the deadline that fires later is dropped.
        (multiple-value-bind (verified deadline) (walk)
          (funcall verified model (answer))
          (is (ready-page-p model))
          (is (not (search "not checked" (page-said model))))
          (let ((said (page-said model)))
            (is (eq :quiet (funcall deadline model :late)))
            (is (equal said (page-said model))))
          (is (null (said-rows model)) "the chat holds none of it"))))))

(deftest setup-last-page-leads-with-the-caveat-and-the-chat-holds-none-of-it ()
  ;; The block and the import report were toasts of four rows that left after
  ;; five seconds (ip-01, xm-201), then rows in the transcript the chat opened
  ;; on, a toast pointing at them. They are the walk's own last page now: the
  ;; caveat first, each value wrapping inside its own column, the way back to
  ;; the providers first, and Start over a caveat leaves its count in the
  ;; footer, which no frame retires.
  (with-held-verify (jobs marker "closing-rows"
                     (served-p-config 1 "\"tui\": {\"notification_popups\": true}, ")
                     (format nil "import: Hermes Agent~%  [providers] imported config.yaml~%"))
    (with-frame-model (model 44 16 :session-id "setup-closing-rows")
      (setf model.setup-requested (list :import "hermes" :yes t))
      (nle::slash-setup-op model)
      ;; JOBS, newest first: the deadline, then the check it races.
      (funcall (cdr (second jobs)) model
               (nlk:json-object "ok" :false "provider" "p" "code" "connection_refused"
                                "reason" "nothing listens at 127.0.0.1:1 — is the server running?"))
      (is (ready-page-p model) "the walk ends on its last page")
      (is (null (said-rows model)) "the chat holds nothing the walk said")
      (is (null (nle::chat-model-toasts model)) "nor a toast that points at it")
      (let ((dialog model.dialog)
            (lines (nle::split-lines (page-said model))))
        (is (search "to check" (nle::picker-dialog-context dialog)))
        (is (uiop:string-prefix-p "! check" (first lines)) "the caveat leads")
        (is (search "nothing listens at 127.0.0.1:1 for p — is its server running?"
                    (nlk:one-line (page-said model))))
        (is (not (search "type a task" (format nil "~{~a~}" (mapcar #'nle::picker-row-detail dialog.rows)))) "no task to try over it: it would fail at once")
        (is (equal '(:provider :start) (subseq (mapcar #'nle::picker-row-value dialog.rows) 0 2)) "the way back comes first")
        ;; Every row sits in the page's columns and fits the panel, a path
        ;; wider than its column included.
        (let ((width (nle::setup-panel-width model)))
          (dolist (line lines)
            (is (<= (nle::text-display-width line) width) line)
            (is (char= #\Space (char line 1)) line)))
        (is (row-carrying "! check" (nle::compose-frame model)) "painted in a 44-column window"))
      ;; Start anyway: the chat opens empty and the footer keeps the count.
      (nle::picker-dialog-key model :escape)
      (is (null model.dialog))
      (is (plusp (length (nle::setup-issues model))))
      (is (row-carrying "setup issue" (nle::compose-frame model)) "the footer says it")
      ;; a walk that ends with nothing to check takes it down
      (nle::forget-setup-issues model)
      (is (not (rows-carry-p "setup issue" (nle::compose-frame model)))))))

(deftest setup-import-that-partly-failed-is-a-caveat ()
  ;; Items the import could not bring over were only counted in a note, and
  ;; the walk's toast said `type a task' over them. They qualify the walk: its
  ;; last page leads with them and offers no task to try.
  ;; p is served, so no problem of its model's leads the page
  (with-hermes-box (marker "partly-failed" (served-p-config 9))
    (with-stubbed-fdefinitions
        ((nle::provider-connected-p () t)
         (nle::setup-import-call (model method request takeover &key progress)
           (import-answer 1 0 2
            "applied" (equal method "apply")
            "worlds" (vector "hermes")
            "items" (vector)
            "report" (format nil "import: Hermes Agent~%")))
         (nle::off-loop (model thunk continuation) (funcall continuation model (funcall thunk))))
      (with-frame-model (model 80 24 :session-id "setup-partly-failed")
        (setf model.setup-requested (list :import "hermes" :yes t))
        (nle::slash-setup-op model)
        (is (ready-page-p model))
        (is-carrying (block (nlk:one-line (page-said model)))
          "check import: 2 items failed — Details names them")
        (is (search "to check" (nle::picker-dialog-context model.dialog)))
        (is (notany (lambda (row) (eq :start (nle::picker-row-value row)))
                    (list (first (nle::picker-dialog-rows model.dialog)))) "the first row is the way back, not Start")
        (is (null (said-rows model)))))))

(deftest setup-closing-block-says-each-key-it-could-not-check-first ()
  ;; A refused imported key closed on `openrouter did not answer with the
  ;; imported key: provider request failed [openrouter anthropic/[redacted]]
  ;; with status 401: Incorrect API key provided · /models saves a new key —
  ;; connect it again', third, under a held gateway and a failed import item,
  ;; and the toast led with the gateway; a key typed at Connect and saved `not
  ;; checked' left no word at all once its dialog closed (X-closing-refused,
  ;; X-closing-unchecked). Each key's standing is read from what the walk
  ;; recorded, said first, once, with the provider's answer and one way on.
  ;; acme-labs is served: its key, not its address, is the question
  (with-setup-box ("{\"general\": {\"default_model\": {\"provider\": \"anthropic\", \"model\": \"a\"}},
                     \"providers\": {\"acme-labs\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:9/v1\"}}}")
    (let ((run (nle::make-setup-run
                :providers (list "anthropic" "acme-labs")
                ;; newest first, as the walk pushes them
                :caveats (list "import: 1 item failed — Details names it"
                               "hermes-gateway.service is still running, so channels.telegram stays with it")
                :answers (list (cons "anthropic" "HTTP 401: Incorrect API key provided")))))
      (setf (gethash "anthropic" nle::*key-checks*) (list :refused "Anthropic refused the imported key")
            (gethash "acme-labs" nle::*key-checks*)
            (list :unchecked "Acme Labs key was not checked: 127.0.0.1:9 answered HTTP 500"))
      (let ((caveats (nle::setup-caveats run)))
        ;; the provider's first, then the walk's own
        (is (equal '("Anthropic refused the imported key with HTTP 401: Incorrect API key provided — /models saves a new key"
                     "Acme Labs key was not checked: 127.0.0.1:9 answered HTTP 500 — /models takes it again and checks it"
                     "hermes-gateway.service is still running, so channels.telegram stays with it"
                     "import: 1 item failed — Details names it")
                   (mapcar #'second (subseq caveats 0 (min 4 (length caveats))))))
        ;; the last page leads with the key
        (is (uiop:string-prefix-p "! check       Anthropic refused the imported key with HTTP 401"
                                  (first (nle::split-lines (page-text run :caveats caveats))))))
      (let ((text (nlk:one-line (page-text run))))
        (is (= 1 (length (ppcre:all-matches-as-strings "/models saves" text))) "one way on")
        (is-carrying text (:absent "[redacted]") (:absent "[anthropic") (:absent "connect it again")))
      ;; a key that answers by the end of the walk is no longer said
      (remhash "anthropic" nle::*key-checks*)
      (is-carrying (text (nlk:one-line (page-text run)))
        (:absent "refused") "check Acme Labs key was not checked"))))

(deftest setup-says-a-provider-no-turn-can-run-on-needs-attention ()
  ;; providers.acme with no base_url read `acme connected' under `still
  ;; needs a model', and the walk closed on `problem model: mock-fast on
  ;; acme', with neither the reason nor the file (X-nobase).
  (with-setup-box ("{\"general\": {\"default_model\": {\"provider\": \"acme\", \"model\": \"mock-fast\"}},
                     \"providers\": {\"acme\": {\"sdk\": \"openai-completions\", \"api_key\": \"acme-key\"}}}")
    (let ((run (nle::make-setup-run :steps '(:provider :model :effort))))
      (with-frame-model (model 100 30 :session-id "setup-nobase")
        (nle::setup-stage-provider model run)
        (let ((dialog model.dialog))
          (is (uiop:string-prefix-p "providers.acme has no base_url" dialog.search-placeholder)
              dialog.search-placeholder)
          (is-present (acme (find "acme" (nle::connect-dialog-rows dialog)
                                  :key #'nle::picker-row-label :test #'equal)) "acme's row"
            (is (search "needs attention" (nle::picker-row-detail acme))))))
      (is-present (problem (first (nle::setup-caveats run))) "the model's problem, first"
        (is (equal "problem" (first problem)))
        (is-carrying (text (second problem))
          "mock-fast on acme cannot run a turn — providers.acme has no base_url" "set providers.acme.base_url in"))
      ;; the page leads with what is wrong, where `model: mock-fast on acme' read as the pick
      (is (uiop:string-prefix-p "! problem     mock-fast on acme cannot run a turn"
                                (first (nle::split-lines (page-text run)))))
      ;; A key typed at Connect for it is saved `not checked' with the
      ;; refusal: said once, as the refusal, never as a key /models could
      ;; check again, and the toast still leads with it. Nor is a provider
      ;; the walk connected and no turn can run on a key to check again.
      (setf (nle::setup-run-providers run) (list "acme" "groq")
            (gethash "acme" nle::*key-checks*)
            (list :unchecked (format nil "acme key was not checked: ~a" (nle::selection-refusal "acme"))))
      (with-stubbed-fdefinition (nle::selection-refusal (provider)
                                  (format nil "providers.~a has no base_url: set providers.~:*~a.base_url" provider))
        (setf (gethash "groq" nle::*key-checks*)
              (list :unchecked "Groq key was not checked: providers.groq has no base_url: set providers.groq.base_url"))
        (let ((caveats (nle::setup-caveats run)))
          (is (equal '("mock-fast on acme cannot run a turn — providers.acme has no base_url: set providers.acme.base_url"
                       "groq cannot run a turn — providers.groq has no base_url: set providers.groq.base_url")
                     (mapcar #'second (subseq caveats 0 (min 2 (length caveats))))))
          ;; said once
          (is (= 1 (count-if (lambda (caveat) (search "providers.acme has no base_url" (second caveat))) caveats)))
          (is (notany (lambda (caveat) (or (search "/models" (second caveat)) (search "key was not checked" (second caveat))))
                      caveats))
          (is (uiop:string-prefix-p "! problem     mock-fast on acme cannot run a turn"
                                    (first (nle::split-lines (page-text run :caveats caveats))))))))))

(deftest setup-takes-a-pins-verdict-from-the-gateway-in-a-shell ()
  ;; The walk's step line said `provider claude-code is not one this box
  ;; knows' over a pin every turn ran on (the operator, 2026-10-22): the shell
  ;; asked its own lanes, and a lane a cell registers runs in the gateway only.
  (with-setup-box ("{\"general\": {\"default_model\": {\"provider\": \"cellprov\", \"model\": \"m-1\"}}}")
    (let ((run (nle::make-setup-run :steps '(:provider :model :effort)))
          (answer (nlk:decode-json "{\"models\":[],\"unlisted\":[],\"providers\":[{\"id\":\"cellprov\",\"connected\":true,\"listable\":false,\"ready\":true}]}")))
      (is (nle::provider-turn-refusal "cellprov") "no lane here: this image's own lanes refuse it")
      (with-frame-model (model 100 30 :session-id "setup-shell-cell-pin")
        (with-saved-globals ((nle::*serving* nil)
                             (nle::*gateway-models* (nle::gateway-models-facts answer)))
          (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &rest keys)
                                      (declare (ignore model body keys))
                                      (if (and (eq method :get) (equal path "/api/gateway/models"))
                                          answer
                                          (nle::make-json-object)))
            (nle::setup-stage-provider model run)
            (let ((dialog model.dialog))
              (is (not (search "cellprov" (or dialog.search-placeholder ""))) "the step line owes nothing on a pin the gateway serves"))
            (is (notany (lambda (caveat) (search "cellprov" (second caveat))) (nle::setup-caveats run)) "nor does the closing page")
            (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape))))))))

(deftest setup-closing-block-says-a-folder-that-did-not-load-in-one-line
    (with-temp-trees ((root "setup-broken-tree") (folder "setup-broken-cells")))
  ;; Without this, a folder that fails to compile would put the compiler's
  ;; whole log in the closing block and the toast would lead with `prs:
  ;; FAILED COMPILE-FILE-ERROR while compiling #<CL-SOURCE-FILE
  ;; "nodecode-prs"…'. One line says it, and /cells says why. The organism
  ;; turns it on (INSTALL-AND-LOAD-CELL, as SWITCH-CELL-ON does); the walk's
  ;; end reads it from the folder's record.
  (let ((name (string-downcase (format nil "nl-setup-broken-~36r" (random (expt 36 6)))))
        (source (merge-pathnames "src/" root)))
    (ensure-directories-exist source)
    (write-cell-folder (merge-pathnames "cells/" root) name
                        (list (cons "cell" (format nil "(defpackage #:~a (:use #:cl))
(in-package #:~a)
(defmacro boom () (error \"no such clause\"))
(defun broken () (boom))" name name))))
    (with-setup-box ("{\"general\": {\"default_model\": {\"provider\": \"anthropic\", \"model\": \"a\"}}}")
      (with-saved-globals ((nlk::*cells-directory* folder) (nlk::*source-root* source) nlk::*cells*)
        (let ((asdf:*central-registry* asdf:*central-registry*)
              (asdf::*immutable-systems* asdf::*immutable-systems*))
          (with-frame-model (model 100 30 :session-id "setup-broken")
            (nle::install-and-load-cell name (nlk:shipped-cells))
            (nle::setup-stage-finish model (nle::make-setup-run))
            (is (ready-page-p model) "the walk ends on its last page")
            (is-present (problem (find name (nle::setup-caveats (nle::make-setup-run))
                                       :key #'second :test #'search)) "the folder's problem"
              (is (equal (list "problem" (format nil "~a did not load — /cells names why" name)) problem)))
            (is-carrying (text (nlk:one-line (page-said model)))
              (format nil "~a did not load — /cells names why" name)
              (:absent "COMPILE-FILE-ERROR") (:absent "#<") (:absent "caught ERROR") (:absent "no such clause"))
            ;; said once
            (is (= 1 (count-if (lambda (line) (search "did not load" line))
                               (nle::split-lines (page-said model)))))
            (is (uiop:string-prefix-p "! problem" (page-said model)) "leading the page")
            (is (search "to check" (nle::picker-dialog-context model.dialog)))))))))

(deftest setup-import-rest-landing-after-the-walk-is-one-toast (with-hermes-box (marker "rest"))
  ;; The rest of the homes lands behind the walk, seconds after the keys. A
  ;; walk already closed, its last page gone, has nowhere to put it: one toast
  ;; says where it came from and what qualifies it, never a row in the chat,
  ;; and the marker records the whole import, not the keys alone.
  (let ((run (nle::make-setup-run :steps '(:provider :model :effort)))
        (item (lambda (kind status &optional (source "s.json"))
                (nlk:json-object "kind" kind "status" status "source" source "world" "hermes"
                                 "destination" (if (equal kind "providers") "auth.json api_keys.p" "x")))))
    (setf (nle::setup-run-import run)
          (import-answer 1 0 0 "applied" t "worlds" (vector "hermes")
                         "items" (vector (funcall item "providers" "imported" ".env"))))
    (with-frame-model (model 100 30 :session-id "setup-rest")
      (setf (nle::chat-model-setup-walk model) nil)
      (nle::setup-import-rest-landed
       model run
       (import-answer 2 0 1 "applied" t "worlds" (vector "hermes" "codex")
                      "items" (vector (funcall item "memory" "imported" "MEMORY.md")
                                      (funcall item "mcp" "imported" "config.yaml")
                                      (funcall item "skills" "error"))))
      (is (null (said-rows model)) "no row in the chat")
      (is-present (toast (first (nle::chat-model-toasts model))) "one toast"
        (is-carrying (text (nle::toast-text toast))
          "imported from hermes and codex: 3 imported · 0 skipped · 0 conflicts · 1 error"
          "import: 1 item failed")
        (is (nle::toast-stays toast) "a failure stays until it is closed"))
      (is (= 3 (gethash "imported" (nlk:decode-json (nlk:read-text marker)))) "the marker has the whole import")
      (is (null (nle::setup-run-notes run)) "a closed walk's page is not written to"))))

(deftest setup-import-rest-landing-under-the-last-page-redraws-it (with-hermes-box (marker "rest-page"))
  ;; The rest can land after the walk's last steps but while its last page is
  ;; up: the page says it, on the row the operator was on, and no toast or
  ;; row says it again.
  (let ((run (nle::make-setup-run :steps '(:provider :model :effort)))
        (item (lambda (kind status &optional (source "s.json"))
                (nlk:json-object "kind" kind "status" status "source" source "world" "hermes"
                                 "destination" (if (equal kind "providers") "auth.json api_keys.p" "x")))))
    (setf (nle::setup-run-import run)
          (import-answer 1 0 0 "applied" t "worlds" (vector "hermes")
                         "items" (vector (funcall item "providers" "imported" ".env"))))
    (with-frame-model (model 100 30 :session-id "setup-rest-page")
      (with-stubbed-fdefinition (nle::setup-caveats (run) '())
        (nle::setup-stage-finish model run)
        (is (ready-page-p model))
        (is (not (search "1 memory" (page-said model))))
        (nle::setup-import-rest-landed
         model run
         (import-answer 2 0 0 "applied" t "worlds" (vector "hermes")
                        "items" (vector (funcall item "memory" "imported" "MEMORY.md")
                                        (funcall item "mcp" "imported" "config.yaml"))))
        (is (ready-page-p model) "the page is still up")
        (is-carrying (text (nlk:one-line (page-said model)))
          "✓ 1 memory hermes" "imported from hermes: 3 imported")
        (is (member :details (mapcar #'nle::picker-row-value (nle::picker-dialog-rows model.dialog))) "and its Details now lists the items")
        (is (null (nle::chat-model-toasts model)))
        (is (null (said-rows model)))))))

(deftest setup-import-rest-answering-first-waits-for-the-keys ()
  ;; The rest runs beside the keys and can answer first. Said then, its row
  ;; would count it alone and the keys' landing would put a marker over it
  ;; that forgets it: it is held until the keys' answer is taken, then said
  ;; over both.
  (with-hermes-box (marker "rest-first")
    (let ((run (nle::make-setup-run :steps '(:provider :model :effort)))
          (rest (import-answer 2 0 0
                 "applied" t "worlds" (vector "hermes")
                 "items" (vector (nlk:json-object "kind" "memory" "status" "imported"
                                                  "source" "MEMORY.md" "world" "hermes"
                                                  "destination" "x")))))
      (with-frame-model (model 100 30 :session-id "setup-rest-first")
        (setf (nle::chat-model-setup-walk model) nil)
        (with-stubbed-fdefinitions ((nle::setup-import-call (model method request takeover &key progress) rest))
          (nle::setup-import-rest model run '(:import ("hermes") :offered t :only "providers")))
        (is (consp (nle::setup-run-behind run)) "held while the keys are out")
        (is (null (probe-file marker)) "no marker for the rest alone")
        (setf (nle::setup-run-import run)
              (import-answer 1 0 0 "applied" t "worlds" (vector "hermes") "items" #()))
        (nle::setup-import-rest-due model run)
        (is (eq :landed (nle::setup-run-behind run)))
        (is (null (said-rows model)) "no row in the chat")
        (is-carrying (text (nle::toast-text (first (nle::chat-model-toasts model))))
          "imported from hermes: 3 imported · 0 skipped · 0 conflicts · 0 errors")
        (is (= 3 (gethash "imported" (nlk:decode-json (nlk:read-text marker)))) "the marker has both")))))

(deftest setup-import-asks-again-while-a-run-left-the-keys-or-the-bots-behind (let ((connected nil)
                                                                                    (calls '())))
  ;; An import that RAN is not an import that LANDED. A run that writes
  ;; nothing leaves no marker, a run that leaves a watched kind behind records
  ;; it, and that kind re-opens the offer for exactly as long as this home
  ;; still lacks it — no longer, or the wizard would ask forever.
  (with-hermes-box (marker "again" "{}")
    (flet ((plan-value (imported &rest items)
             (import-answer imported (length items) 0
              "items" (map 'vector
                           (lambda (pair)
                             (nlk:json-object "kind" (first pair)
                                              "status" (second pair)))
                           items))))
      (with-stubbed-fdefinitions
          ((nle::provider-connected-p () connected)
           (nle::setup-import-call (model method request takeover &key progress)
             (push method calls)
             (import-answer 2 1 0
              "running" (vector)
              "bots" (vector)
              "takeover" :null
              "report" "the report")))
        ;; A run that landed nothing does not spend the offer.
        (nle::note-import-done (plan-value 0))
        (is (not (probe-file marker)) "nothing landed, so no marker")
        (is (not (nle::import-done-p)) "and the import is not done")
        ;; That box's run: much landed, the keys and the bot did not.
        (nle::note-import-done
         (plan-value 1583 '("providers" "skipped") '("channels" "skipped")))
        (is (probe-file marker) "a run that landed leaves the marker")
        (is (not (nle::import-done-p)))
        (is (equal '("providers" "channels")
                   (nle::read-import-marker)))
        ;; The offer is the operator-visible half: /setup opens the
        ;; stage again rather than walking past it.
        (with-frame-model (model 100 30 :session-id "setup-import-again")
          (setf calls '())
          (run-setup model)
          (is (eq :setup-import-offer (nle::picker-dialog-kind model.dialog)) "the offer is asked again")
          (nle::picker-dialog-key model :enter)
          (is (not (member "plan" calls :test #'equal)) "the apply reads the home once")
          (is (member "apply" calls :test #'equal))
          (setf model.dialog nil))
        ;; That run wrote its own marker; put that box's one back, which
        ;; is the state the rest of this gate is about.
        (nle::note-import-done
         (plan-value 1583 '("providers" "skipped") '("channels" "skipped")))
        ;; Each kind stops being asked about the moment it is here.
        (write-temp-file nle::*shared-config-path*
                         "{\"channels\": {\"discord\": {\"enabled\": true}}}")
        (is (not (nle::import-done-p)))
        (setf connected t)
        (is (nle::import-done-p))
        ;; A marker an older build wrote names no outcome at all, so
        ;; the home is asked directly — which is what that box carries.
        (write-temp-file marker "2026-09-11T22:44:06.715Z")
        (is (nle::import-done-p) "an older marker on a home that wants nothing: done")
        (setf connected nil)
        (is (not (nle::import-done-p)))))))

(deftest setup-import-waits-for-its-own-gateway-to-serve-the-folder (let ((calls 0)))
  ;; A shell's first frame opens the walk before its own gateway serves: the
  ;; import stage's run was answered 503 `no gateway endpoint is published
  ;; yet', the walk moved on to the provider, and the note surfaced only
  ;; after the operator had Esc'd through every stage — a fresh install's
  ;; Hermes import did nothing. The run waits on exactly the two answers that
  ;; mean `not serving it yet', and on nothing else.
  (flet ((failing (status code detail &optional served-on)
           ;; A run that counts its calls and refuses each one, until the
           ;; SERVED-ON'th when given.
           (lambda ()
             (incf calls)
             (if (and served-on (>= calls served-on))
                 :served
                 (error 'nle::gateway-http-error :status status :code code :detail detail)))))
    (is (eq :served (nle::await-owner (failing 503 "gateway_unreachable" "not yet" 3) :pause 0.01)))
    (is (= 3 calls))
    (setf calls 0)
    (is (search "boom" (refusal-text nle::gateway-http-error
                         (nle::await-owner (failing 500 "internal" "boom") :pause 0.01))))
    (is (= 1 calls) "any other answer is the answer, at once")
    (setf calls 0)
    (signals-error nle::gateway-http-error
      (nle::await-owner (failing 503 "gateway_unreachable" "never") :seconds 0.05 :pause 0.01))
    (is (> calls 1) "a gateway that never serves is given up at the deadline")
    ;; A shell whose own organism died at boot has no gateway coming: the
    ;; walk heard it after the whole minute of `please wait' (xa-203). It
    ;; hears the boot's own line at once.
    (setf calls 0)
    (let ((nle::*organism-boot-failure* "could not start: the store is a lie"))
      (is (search "could not start"
                  (refusal-text error
                    (nle::await-owner (failing 503 "gateway_unreachable" "never") :pause 0.01))))
      (is (= 1 calls) "at once, not at the deadline"))))

(deftest footer-says-setting-up-then-how-to-add-a-model-never-the-baked-default ()
  ;; A fresh box has no model: the image carries a build-time default
  ;; (openai-completions deepseek-v4-flash-free) nobody chose and no provider
  ;; was connected for. Without this, the footer would name that default
  ;; through the whole setup walk as though it were the operator's model.
  ;; The slot says the walk is running while it is up, how to get a model
  ;; once it is gone, and the model the moment one is configured.
  (is (not (or (nle::credential-env "NODECODE_PROVIDER")
               (nle::credential-env "NODECODE_MODEL"))))
  (with-setup-box ((format nil "{}~%"))
    (let ((nle::*provider* "openai-completions")
          (nle::*model* "deepseek-v4-flash-free")
          (nle::*endpoint* nil))
      (with-frame-model (model 100 30 :session-id "setup-footer")
        (land-model-facts model :provider "openai-completions"
                                :model "deepseek-v4-flash-free" :unconfigured t)
        (flet ((footer () (nth 28 (nle::compose-frame model)))
               (close-walk ()
                 (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape))))
          (is-carrying (text (footer)) "run /setup to add a model" (:absent "? help")
            (:absent "deepseek" "never the baked default"))
          (nle::execute-slash-command model "setup" "")
          (let ((provider-stage (nle::connect-dialog-p model.dialog)))
            (is-carrying (text (footer)) "setting up…" (:absent "? help" "alone"))
            (close-walk)
            (is (null model.setup-walk) "Esc ends the walk")
            (is (search "run /setup to add a model" (footer)))
            (when provider-stage
              (is (null (said-rows model)) "the walk left the chat as it was")))
          (nle::select-default-model :model "m-1")
          (land-model-facts model :provider "openai-completions" :model "m-1")
          (is (search "openai-completions m-1 · ← sessions" (footer)))
          (nle::execute-slash-command model "setup" "")
          (is (search "openai-completions m-1 · ← sessions" (footer)))
          (close-walk))))))

;;; Every setup entrypoint uses the same provider/model/effort walk.
(deftest setup-first-launch-explicit-entrypoints-paint-before-reading ()
  (dolist (requested (list nil t))
    (with-frame-model (model 100 30 :session-id "setup-entrypoint")
      (let ((reads 0) (dispatched nil))
        (setf (nle::chat-model-setup-requested model) requested)
        (with-stubbed-fdefinitions
            ((nle::offered-import-worlds () (incf reads) nil)
             (nle::off-loop (model thunk then)
               (setf dispatched t))
             (nle::setup-stage-provider (model run) nil))
          (nle::execute-slash-command model "setup" "")
          (is (zerop reads) "setup must paint before reading another harness home")
          (is dispatched "discovery is dispatched off-loop")
          (is (equal '(:provider :model :effort)
                     (nle::setup-run-steps (nle::chat-model-setup-walk model))))
          (is (null (nle::chat-model-setup-requested model)) "launch intent is consumed once")
          (let ((dialog (nle::chat-model-dialog model)))
            (is (nle::picker-dialog-p dialog))
            (when (nle::picker-dialog-p dialog)
              (is (equal "Setup" (nle::picker-dialog-title dialog)))
              (is (eq :dialog-accent (nle::picker-dialog-title-tone dialog)))
              (is (not (search "This might take a moment" (nle::picker-dialog-subtitle dialog))))
              (is (equal "Preparing Nodecode" (nle::picker-dialog-context dialog))))))))))

(deftest setup-first-launch-locates-real-items-without-exposing-secrets ()
  (let ((value (nlk:json-object
                "items" (vector
                         (nlk:json-object "kind" "providers" "status" "imported" "source" "settings.json" "world" "claude" "destination" "auth.json api_keys.anthropic")
                         (nlk:json-object "kind" "providers" "status" "conflict" "source" "settings.json" "world" "claude" "destination" "general.default_model")
                         (nlk:json-object "kind" "mcp" "status" "imported" "source" "settings.json" "world" "claude" "destination" "general")
                         (nlk:json-object "kind" "memory" "status" "imported" "source" "MEMORY.md" "world" "claude" "destination" "memory")
                         (nlk:json-object "kind" "memory" "status" "imported" "source" "facts.md" "world" "claude" "destination" "memory")
                         (nlk:json-object "kind" "providers" "status" "skipped" "source" "auth.json" "world" "codex" "destination" "")))))
    (is (equal '((:keys "✓" "1 key" "anthropic from claude")
                 (:configs "✓" "1 config" "claude")
                 (:memory "✓" "1 memory" "claude"))
               (nle::setup-discovery-entries value)))))

(deftest setup-first-launch-absent-discovery-is-not-zero ()
  ;; a home that could not be read says so, never `0 keys'
  (is (equal '((:keys "!" "home" "could not be read, so nothing in it is counted"))
             (nle::setup-discovery-entries (nlk:json-object))))
  (is (equal '((:keys "–" "0 keys" "") (:configs "–" "0 configs" "") (:memory "–" "0 memories" ""))
             (nle::setup-discovery-entries (nlk:json-object "items" #())))))

(deftest setup-first-launch-wait-is-themed-and-discards-late-completion ()
  (let ((completion nil) (continued 0)
        (run (nle::make-setup-run :steps '(:provider :model :effort))))
    (with-frame-model (model 100 30 :session-id "first-launch" :replayed t)
      (with-stubbed-fdefinition (nle::off-loop (model thunk then)
                                 (setf completion then))
        (nle::setup-import-wait model run "Reading" (constantly nil)
                               (lambda (model value) (declare (ignore model value)) (incf continued)))
        (let ((dialog model.dialog))
          (is (equal "Setup" (nle::picker-dialog-title dialog)))
          (is (eq :dialog-accent (nle::picker-dialog-title-tone dialog)))
          (is (not (search "This might take a moment" (nle::picker-dialog-subtitle dialog))))
          (is (functionp (nle::picker-dialog-on-tick dialog)))
          (dialog-keys model :escape)
          (funcall completion model (nlk:json-object "items" #()))
          (is (zerop continued) "Escape cannot be undone by a late scan")
          (is (null model.dialog)))))))

(deftest setup-first-launch-provider-narrows-models-even-after-refresh ()
  (with-model-listings (:catalog +connect-catalog-fixture+)
    (nle::save-provider-api-key "anthropic" "fixture")
    (nle::save-provider-api-key "openai" "fixture")
    (with-frame-model (model 100 30 :session-id "first-launch" :replayed t)
      (let ((run (nle::make-setup-run :steps '(:provider :model :effort))))
        (nle::setup-stage-model model run "anthropic")
        (let ((dialog model.dialog))
          (is (search "Step 2. Choose model" (nle::picker-dialog-context dialog)))
          (is (every (lambda (row) (equal "anthropic" (first (nle::picker-row-value row))))
                     (nle::picker-visible-rows dialog)))
          (nle::install-refreshed-models model)
          (is (every (lambda (row) (equal "anthropic" (first (nle::picker-row-value row))))
                     (nle::picker-visible-rows dialog))))))))

(deftest setup-first-launch-wait-keeps-dialog-identity-after-replace-and-reopen ()
  (let ((pending '()) (continued 0)
        (run (nle::make-setup-run :steps '(:provider :model :effort))))
    (with-frame-model (model 100 30 :session-id "first-launch" :replayed t)
      (with-stubbed-fdefinition (nle::off-loop (model thunk then)
                                 (push then pending))
        (flet ((open-wait ()
                 (nle::setup-import-wait model run "Reading" (constantly nil)
                                        (lambda (model value)
                                          (declare (ignore model value)) (incf continued)))))
          (open-wait)
          (nle::open-picker-dialog model "Replacement" '() (constantly nil))
          (let ((replacement model.dialog))
            (funcall (pop pending) model nil)
            (is (eq replacement model.dialog))
            (is (zerop continued)))
          (open-wait)
          (let ((old (pop pending)))
            (dialog-keys model :escape)
            (open-wait)
            (let ((reopened model.dialog))
              (funcall old model nil)
              (is (eq reopened model.dialog))
              (is (zerop continued))
              (funcall (pop pending) model nil)
              (is (= 1 continued))
              (is (null model.dialog)))))))))

(deftest setup-first-launch-tick-updates-only-the-work-mark ()
  (with-frame-model (model 100 30 :session-id "first-launch" :replayed t)
    (with-stubbed-fdefinition (nle::off-loop (model thunk then))
      (nle::setup-import-wait model (nle::make-setup-run :steps '(:provider :model :effort))
                             "Reading" (constantly nil) (constantly nil))
    (let* ((dialog model.dialog) (tick (nle::picker-dialog-on-tick dialog)) (elapsed 0))
      (with-stubbed-fdefinition (nle::elapsed-ms (start end)
                                 elapsed)
        (is (funcall tick model dialog))
        (let ((title (nle::picker-dialog-title dialog))
              (words (nle::picker-dialog-subtitle dialog))
              (summary (nle::segs-text (first (nle::picker-dialog-summary-rows dialog)))))
          (is (null (funcall tick model dialog)) "unchanged paint is quiet")
          (setf elapsed 750)
          (is (funcall tick model dialog))
          (is (equal title (nle::picker-dialog-title dialog)) "the header never pulses")
          (is (equal words (nle::picker-dialog-subtitle dialog)) "no accumulated dots")
          (is (not (equal summary (nle::segs-text (first (nle::picker-dialog-summary-rows dialog))))))
          (setf elapsed 12000)
          (funcall tick model dialog)
          (is (equal "Reading" (nle::picker-dialog-subtitle dialog)))))))))

(deftest setup-first-launch-effort-uses-target-ladder-and-canonical-writer ()
  (let ((picked :unset) (finished 0) (run (nle::make-setup-run :steps '(:provider :model :effort))))
    (with-frame-model (model 100 30 :session-id "first-launch" :replayed t)
      (with-stubbed-fdefinitions
          ((nle::target-snapshot (model)
             (values "fixture" "reasoner" '("low" "medium" "high") '(:effort "medium")))
           (nle::apply-effort (model effort) (setf picked effort))
           (nle::setup-stage-finish (model run) (incf finished)))
        (nle::setup-stage-effort model run)
        (is (equal "Step 3. Choose reasoning effort" (nle::picker-dialog-context model.dialog)))
        (is (equal '(nil "low" "medium" "high") (mapcar #'nle::picker-row-value (nle::picker-visible-rows model.dialog))))
        (dialog-keys model :down :enter)
        (is (equal "high" picked))
        (is (= 1 finished))))))

(deftest setup-first-launch-no-reasoning-controls-finishes-without-a-write ()
  (let ((finished 0) (run (nle::make-setup-run :steps '(:provider :model :effort))))
    (with-frame-model (model 100 30 :session-id "first-launch" :replayed t)
      (with-stubbed-fdefinitions
          ((nle::target-snapshot (model) (values "fixture" "plain" nil nil))
           (nle::apply-effort (model effort) (error "must not write"))
           (nle::setup-stage-finish (model run) (incf finished)))
        (nle::setup-stage-effort model run)
        (is (search "no reasoning effort controls" (nle::picker-dialog-subtitle model.dialog)))
        (dialog-keys model :enter)
        (is (= 1 finished))
        (is (null (nle::setup-run-caveats run)))))))

(deftest setup-first-launch-discovery-waits-for-initial-replay-before-reporting ()
  (let ((completion nil) (continued 0)
        (run (nle::make-setup-run :steps '(:provider :model :effort))))
    (with-frame-model (model 100 30 :session-id "first-launch")
      (with-stubbed-fdefinition (nle::off-loop (model thunk then)
                                 (setf completion then))
        (nle::setup-import-wait model run "Reading" (constantly nil)
                               (nlk:lambda-ignoring (model value)
                                 (incf continued)
                                 (setf (nle::chat-model-state model)
                                       (nle::ingest-system-notice (nle::chat-model-state model) "Located 0 keys."))
                                 (nle::render-state model)))
        (let ((dialog model.dialog))
          (funcall completion model nil)
          (is (zerop continued) "do not report into a state startup replay will replace")
          (setf (nle::gateway-model-last-sequence model) 0
                (nle::gateway-model-replay-staging model) (nle::initial-chat-state))
          (funcall (nle::picker-dialog-on-tick dialog) model dialog)
          (is (zerop continued) "a partial replay is not readiness")
          (nle::show-replayed-state model)
          (funcall (nle::picker-dialog-on-tick dialog) model dialog)
          (is (= 1 continued))
          (is (search "Located 0 keys." (car (last (said-rows model)))))
          (is (row-carrying "Located 0 keys." (nle::compose-frame model))))))))


;;; /setup's provider step is the shared list, including its pointer contract.

(defmacro with-setup-provider-picker ((model dialog &optional (count 60)) &body body)
  `(with-model-listings (:config "{}" :catalog "{}")
     (with-frame-model (,model 100 30 :session-id "setup-pointer")
       (with-stubbed-fdefinitions
           ((nle::start-models-catalog-fetch (model) nil)
            ;; the list's shape alone: nodecode.ai's sign-in row is its own gate's
            (nle::models-sign-in-rows () nil)
            (nle::connect-picker-rows ()
              (cons (nle::make-picker-row :label "Providers" :heading t)
                    (loop for i from 1 to ,count
                          collect (nle::make-picker-row
                                   :label (format nil "Provider ~2,'0d" i)
                                   :value (list :provider (format nil "provider-~2,'0d" i)))))))
         (nle::setup-stage-provider
          ,model (nle::make-setup-run :steps '(:provider :model :effort)))
         (let ((,dialog (nle::chat-model-dialog ,model)))
           ,@body)))))

(deftest setup-provider-list-follows-the-shared-pointer-gestures ()
  (with-setup-provider-picker (model dialog)
    (is (eq t (nle::dialog-list-selectable-p dialog)) "the provider step is exactly Boolean-selectable")
    (multiple-value-bind (first height) (nle::dialog-body-band model dialog)
      (let ((bar (- model.term-width nle::*docked-panel-pad-right*))
            (parked (transcript-offset model)))
        (mouse-update model #'nlt:make-mouse-motion-msg 5 (+ first 2))
        (is (= 2 dialog.selection) "hover highlights the painted provider")
        (mouse-update model #'nlt:make-mouse-wheel-msg 5 (+ first 2) :direction :down :count 3)
        (is (equal '(3 5) (list dialog.view-offset dialog.selection)) "the wheel moves the list under the pointer")
        (mouse-update model #'nlt:make-mouse-wheel-msg 5 (1- first) :direction :down)
        (is (= 4 dialog.view-offset) "a header notch stays with the modal list")
        (is (= parked (transcript-offset model)) "panel gestures leave the transcript parked")
        (mouse-update model #'nlt:make-mouse-click-msg bar first :button :left)
        (is (equal '(:dialog . 0) model.scrollbar-drag) "the shared scrollbar owns its thumb")
        (mouse-update model #'nlt:make-mouse-motion-msg bar (+ first (1- height)) :button :left)
        (is (> dialog.view-offset 4) "dragging reaches later providers")
        (mouse-update model #'nlt:make-mouse-release-msg bar (+ first (1- height)) :button :left)
        (is (null model.scrollbar-drag))
        (is (eq :provider (nle::connect-dialog-step dialog)) "releasing the bar never submits a provider")
        (press model :home)
        (mouse-update model #'nlt:make-mouse-motion-msg 5 first)
        (is (= 1 dialog.selection) "a heading takes no hover")
        (click-at model 5 first)
        (is (eq :provider (nle::connect-dialog-step dialog)) "a heading click opens nothing")
        (click-at model 5 (+ first 2))
        (is (eq :api-key (nle::connect-dialog-step dialog)) "a row click uses the same activation as Enter")
        (is (equal "provider-02" (nle::connect-dialog-provider-id dialog)))
        (is (= parked (transcript-offset model)))))))

(deftest setup-non-list-steps-ignore-list-pointer-gestures ()
  (with-setup-provider-picker (model dialog)
    (nle::connect-ask-key dialog (second dialog.rows))
    (dolist (step '(:custom-provider :api-key :checking :result))
      (setf (nle::connect-dialog-result-message dialog) "Refused fixture key")
      (nle::connect-enter-step dialog step)
      (setf dialog.selection 1 dialog.view-offset 0)
      (let ((first (nle::dialog-body-band model dialog))
            (bar (- model.term-width nle::*docked-panel-pad-right*))
            (input dialog.input))
        (is (eq nil (nle::dialog-list-selectable-p dialog)) "fields and status steps are exactly Boolean-ineligible")
        (is (null (nle::dialog-hover-index model first)))
        (is (null (nle::dialog-scrollbar-metrics model)))
        (mouse-update model #'nlt:make-mouse-motion-msg 5 first)
        (mouse-update model #'nlt:make-mouse-wheel-msg 5 first :direction :down :count 3)
        (mouse-update model #'nlt:make-mouse-click-msg bar first :button :left)
        (is (null model.scrollbar-drag))
        (mouse-update model #'nlt:make-mouse-release-msg bar first :button :left)
        (click-at model 5 first)
        (is (eq step (nle::connect-dialog-step dialog)) "no hidden provider row is activated")
        (is (equal '(0 1) (list dialog.view-offset dialog.selection)))
        (is (equal input dialog.input) "the field remains unchanged")))))

(deftest setup-provider-keys-keep-shared-picker-parity (with-setup-provider-picker (model dialog))
  (let ((generic (nle::make-picker-dialog :title "Providers" :rows dialog.rows :searchable t)))
    (nle::picker-seat-top generic)
    (dolist (key '(:down :page-down :end :page-up :home #\0 #\2 :escape))
      (let ((states
              (loop for picker in (list dialog generic)
                    do (setf model.dialog picker)
                       (press model key)
                    collect (list (nle::picker-dialog-selection picker)
                                  (nle::picker-dialog-view-offset picker)
                                  (nle::picker-dialog-search picker)
                                  (mapcar #'nle::picker-row-label (nle::picker-visible-rows picker))))))
        (is (equal (first states) (second states)) "navigation, paging, filtering and search-Esc use the shared ladder")))
    (setf model.dialog dialog)
    (let ((finished 0))
      (with-stubbed-fdefinition (nle::setup-stage-finish (model run)
                                 (incf finished))
        (press model :escape)
        (is (null model.dialog))
        (is (= 1 finished) "the walk's Escape advances exactly once")))))


(deftest setup-first-launch-compact-panel-restores-theme-and-real-work (dolist (width '(44 80 100)))
  (with-frame-model (model width 30 :session-id "setup-status")
    (with-stubbed-fdefinition (nle::off-loop (model thunk then))
      (nle::setup-import-wait model (nle::make-setup-run :steps '(:provider :model :effort))
                             "Finding keys, config, and memory…" (constantly nil) (constantly nil)))
    (let* ((dialog model.dialog)
           (summary (first (nle::picker-dialog-summary-rows dialog)))
           (rows (nle::dialog-panel-rows model width nil)))
      (is (equal "Setup" (nle::picker-dialog-title dialog)))
      (is (equal "Preparing Nodecode" (nle::picker-dialog-context dialog)))
      (is (eq :dialog-accent (nle::picker-dialog-title-tone dialog)))
      (is (eq :muted (nle::picker-dialog-summary-tone dialog)))
      (is (equal '(:dialog-accent :foreground) (mapcar #'nle::seg-tone summary)))
      (is (row-carrying "Finding keys" rows) "the real phase is visible")
      (is (not (rows-carry-p "Next:" rows)))
      (is-table (in) (not (rows-carry-p in rows)) ("ctrl+f") ("This might take") ("..."))
      (is (<= (nle::docked-dialog-frame-rows model) 9) "a short task is a compact panel"))))

(deftest setup-first-launch-import-hides-roadmap-and-hotkeys ()
  (with-frame-model (model 100 30 :session-id "import-progress")
    (with-stubbed-fdefinition (nle::off-loop (model thunk then))
      (nle::setup-import-wait model (nle::make-setup-run :steps '(:provider :model :effort))
                             "Importing…" (constantly nil) (constantly nil)))
    (let ((rows (nle::dialog-panel-rows model 100 nil)))
      (is (row-carrying "Importing" rows))
      (is-table (in) (not (rows-carry-p in rows)) ("Next:") ("esc") ("ctrl+f"))
      (is (<= (nle::docked-dialog-frame-rows model) 6)))))

(deftest setup-first-launch-import-progress-is-real-and-dialog-bound ()
  (let ((worker nil) (completion nil) (report nil) (finished 0))
    (with-frame-model (model 100 30 :session-id "import-progress" :replayed t)
      (with-stubbed-fdefinition (nle::off-loop (model thunk then)
                                 (setf worker thunk completion then))
        (nle::setup-import-wait
         model (nle::make-setup-run :steps '(:provider :model :effort)) "Importing…"
         (lambda (progress) (setf report progress) nil)
         (lambda (model value) (declare (ignore model value)) (incf finished))
         :importing t))
      (funcall worker)
      (let ((dialog model.dialog)
            (item (nlk:json-object "kind" "memory" "destination" "loopback-notes" "status" "imported")))
        (funcall report (nlk:json-object "phase" "importing" "item" item))
        (is (rows-carry-p "Importing Memory · loopback-notes" (nle::dialog-panel-rows model 100 nil)))
        (is (not (rows-carry-p "✓" (nle::dialog-panel-rows model 100 nil))) "a planned item is not yet imported")
        (funcall report (nlk:json-object "phase" "finished" "item" item))
        (is (rows-carry-p "✓  Memory · loopback-notes" (nle::dialog-panel-rows model 100 nil)))
        (loop for n below 6 do
          (funcall report (nlk:json-object "phase" "finished" "item"
                                         (nlk:json-object "kind" "skills" "destination" (format nil "skill-~d" n) "status" "imported"))))
        (is (= 5 (length (nle::picker-dialog-summary-rows dialog))) "current work plus four recent outcomes")
        (let ((failed (nlk:json-object "kind" "memory" "destination" "failed-notes" "status" "error")))
          (funcall report (nlk:json-object "phase" "finished" "item" failed))
          (is (rows-carry-p "failed-notes" (nle::dialog-panel-rows model 100 nil)))
          (is (not (rows-carry-p "✓  Memory · failed-notes" (nle::dialog-panel-rows model 100 nil))) "failure never paints success"))
        (dialog-keys model :escape)
        (is (null model.dialog) "Escape still cancels without its hotkey copy")
        (funcall report (nlk:json-object "phase" "finished" "item" item))
        (funcall completion model nil)
        (is (null model.dialog))
        (is (zerop finished))
        (setf model.dialog (nle::make-picker-dialog :title "Replacement"))
        (funcall report (nlk:json-object "phase" "finished" "item" item))
        (is (equal "Replacement" (nle::picker-dialog-title model.dialog)))))))

(deftest setup-import-stream-returns-result-and-closes-the-response ()
  (with-frame-model (model 100 30 :session-id "import-stream")
    (let* ((events '())
           (input (make-string-input-stream
                   (format nil "data: ~a~%~%data: ~a~%~%"
                           (nlk:encode-json-object (nlk:json-object "type" "item" "phase" "finished" "item" (nlk:json-object "kind" "memory" "status" "imported" "destination" "notes")))
                           (nlk:encode-json-object (nlk:json-object "type" "result" "result" (nlk:json-object "applied" t "items" #())))))))
      (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &key raw stream)
                                 (assert (eq method :post))
                                 (assert (equal path "/api/import"))
                                 (assert stream)
                                 (assert (eq t (gethash "stream" body)))
                                 input)
        (let ((result (nle::setup-import-call model "apply" '(:import "fixture") nil :progress (lambda (event) (push event events)))))
          (is (eq t (gethash "applied" result)))
          (is (= 1 (length events)))
          (is (equal "finished" (gethash "phase" (first events))))
          (is (not (open-stream-p input))))))))

(deftest setup-streamed-route-keeps-the-clack-response-function ()
  (let ((reply (lambda (responder) (declare (ignore responder)) nil)))
    (is (eq reply (nle::gateway-answer reply)))))

(deftest setup-import-stream-refuses-truncation-errors-and-unknown-events ()
  (with-frame-model (model 100 30 :session-id "import-stream-error")
    (dolist (event (list nil (nlk:json-object "type" "unknown")
                        (nlk:json-object "type" "result" "result" (nlk:json-object "error" "fixture failed"))))
      (let ((input (make-string-input-stream
                    (if event (format nil "data: ~a~%~%" (nlk:encode-json-object event)) ""))))
        (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &key stream)
                                   input)
          (is (handler-case
                  (progn (nle::setup-import-call model "apply" '(:import "fixture") nil :progress (constantly nil)) nil)
                (error () t)) "a failed stream is not a completed import")
          (is (not (open-stream-p input)) "every terminal branch closes the response"))))))

(deftest setup-http-stream-refusal-stays-typed-and-closes-its-body ()
  (let ((body (make-string-input-stream "{\"error\":{\"code\":\"not_found\",\"message\":\"fixture not serving yet\"}}")))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                               (values body 404 nil))
      (is (handler-case
              (progn (nle::gateway-http-session-request 1 :post "/api/import" "{}" nil :stream t) nil)
            (nle::gateway-http-error (condition)
              (and (= 404 (nle::gateway-http-error-status condition))
                   (equal "not_found" (nle::gateway-http-error-code condition))
                   (equal "fixture not serving yet" (nle::gateway-http-error-detail condition))))))
      (is (not (open-stream-p body))))))

(deftest setup-models-step-waits-for-a-list-not-yet-loaded ()
  ;; A shell whose gateway list had not landed yet (*GATEWAY-MODELS* NIL)
  ;; sent the walk's model step back to its providers, the bare /models
  ;; report standing over their key hints (Mac take, 2026-10-21). The step
  ;; opens on an empty list that says it is loading, and the list fills in
  ;; place when the read lands.
  (with-setup-box ((format nil "{}~%"))
    (write-temp-file nle::*models-catalog-path* "{
  \"anthropic\": {\"name\": \"Anthropic\", \"npm\": \"@ai-sdk/anthropic\",
                \"models\": {\"new\": {\"tool_call\": true, \"release_date\": \"2026-02-01\"}}}}")
    (setf nle::*models-catalog-memo* nil)
    (nle::save-provider-api-key "anthropic" "sk-1")
    (with-frame-model (model 100 30 :session-id "setup-models-not-loaded")
      (with-saved-globals ((nle::*serving* nil) (nle::*gateway-models* nil))
        (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &rest keys)
                                    (declare (ignore model method path body keys))
                                    (nle::make-json-object))
          (nle::setup-stage-model model (nle::make-setup-run :steps '(:provider :model :effort)))
          (let ((dialog model.dialog))
            (is (eq :models (and (nle::picker-dialog-p dialog) (nle::picker-dialog-kind dialog))) "the walk stays on its model step")
            (when (nle::picker-dialog-p dialog)
              (is (search "Step 2" (nle::picker-dialog-context dialog)))
              (is (equal "Loading the model list…" (nle::picker-dialog-empty-label dialog)))
              (is (not (rows-carry-p "provider:" (nle::compose-frame model))) "no /models report over the panel")
              ;; the gateway's list lands: the same panel fills in place
              (setf nle::*gateway-models* (nle::gateway-models-facts (gateway-models-answer)))
              (nle::install-refreshed-models model)
              (is (eq dialog model.dialog))
              (is (plusp (length (nle::picker-visible-rows dialog))) "its rows arrive")
              (is (equal "No models found" (nle::picker-dialog-empty-label dialog)))))
          (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape)))))))

(deftest setup-issue-count-follows-the-target ()
  ;; The footer counted `no model configured' on under a model /models had
  ;; set and that was answering (Mac take, 2026-10-21). The caveats are read
  ;; again once the shell's model facts move, and one a later pick answered
  ;; leaves the count; a frame alone reads nothing again.
  (let ((run (nle::make-setup-run :steps '(:provider :model :effort)))
        (caveats (list (list "problem" "model: no model configured — /models picks one"))))
    (with-frame-model (model 100 30 :session-id "setup-issue-follows")
      (with-stubbed-fdefinition (nle::setup-caveats (run) (declare (ignore run)) caveats)
        (nle::setup-stage-finish model run)
        (is (ready-page-p model))
        (nle::picker-dialog-key model :escape)
        (is (null model.dialog) "Start anyway")
        (is (row-carrying "1 setup issue" (nle::compose-frame model)) "the footer counts it")
        (setf caveats '())
        (is (row-carrying "1 setup issue" (nle::compose-frame model)) "a frame alone reads nothing again")
        ;; a pick lands: the shell's model facts are a new answer
        (setf (nle::gateway-model-model-facts model) (list :provider "anthropic" :model "new"))
        (is (not (rows-carry-p "setup issue" (nle::compose-frame model))) "the answered caveat leaves the count")
        (is (null (nle::setup-issues model)))))))

(deftest a-shell-import-walks-the-import-stage (let ((walked '())))
  ;; T-350: /import in a shell is `nodecode import' -- the walk's import
  ;; stage, its plan read off the loop over a pinged stream -- where as a
  ;; gateway command it held the loop and failed at the 30 s read. A room and
  ;; the page keep the text answer through the same entry's headless arm.
  (with-stubbed-fdefinitions ((nle::slash-setup-op (model)
                                (push (nle::chat-model-setup-requested model) walked)
                                nil))
    (with-frame-model (model 100 30 :session-id "slash-import")
      (is (null (nle::slash-import-op model "claude --only sessions --yes")))
      (let ((request (first walked)))
        (is (equal "claude" (getf request :import)))
        (is (equal "sessions" (getf request :only)))
        (is (eq t (getf request :yes))))
      (nle::slash-import-op model "")
      (is (eq t (getf (first walked) :import)) "bare: every world")
      (is-carrying (text (nle::slash-import-op model "notaworld"))
        ("not a world this build reads" "a refusal, as the verb's"))
      (is-carrying (text (nle::slash-import-op model "claude stray"))
        ("unknown word stray" "a stray word is refused"))
      (is (= 2 (length walked)) "a refusal walks nothing")))
  (let ((entry (nle::find-builtin-slash-command "import")))
    (is (eq 'nle::headless-import (nle::slash-command-headless entry)))
    (is (= 1 (count "import" (nle::slash-catalog) :key (lambda (row) (getf row :name)) :test #'equal)) "one /import in the catalog")))
