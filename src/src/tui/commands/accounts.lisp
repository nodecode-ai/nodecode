;;;; accounts.lisp --- /usage, /account, /think. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- /usage ------------------------------------------------------------------
;;; On a shell /usage is a dialog: a tab per range, each filled by its own read
;;; of the gateway's usage (the store is the gateway's), off the loop, the one
;;; asked for first; Tab moves between them, Enter on a session opens it. A
;;; channel or the web page's `/' gets the same read as text (HEADLESS-USAGE).

(defparameter +usage-tabs+ '(("today" "Today") ("7d" "7 days") ("30d" "30 days") ("all" "All"))
  "The /usage dialog's tabs: each range and its label, in strip order.")

(defun usage-dialog-rows (range usage)
  "RANGE's rows in the /usage dialog, each keyed by RANGE so its tab shows
them: the sentence, the totals, spend by day, the models, the costliest
sessions -- Enter opens one -- and the folders. NIL USAGE is a read still
out; an OFF-LOOP failure is said in one row."
  (flet ((row (label detail &key value (mark "") (tone :muted))
           (make-picker-row :key range :label label :detail detail :value value
                            :mark mark :mark-tone tone))
         (pad (text) (format nil "~34a" (stub-clip (or text "") 34))))
    (cond
      ((null usage) (list (row "Adding it up…" "")))
      ((off-loop-failure usage)
       (list (row "Could not read usage" (off-loop-failure usage) :mark "×" :tone :danger)))
      (t
       (let* ((totals (usage-field usage "totals"))
              (cost (usage-field totals "cost_usd"))
              (days (coerce (or (usage-field usage "days") #()) 'list))
              (models (coerce (or (usage-field usage "models") #()) 'list))
              (sessions (coerce (or (usage-field usage "sessions") #()) 'list))
              (folders (coerce (or (usage-field usage "folders") #()) 'list)))
         (append
          ;; The nodecode.ai account first, as the footer's meter last read it.
          (account-slab-rows range)
          (list (row (usage-headline usage)
                     (format nil "~@[since ~a · ~]API-equivalent prices" (usage-since usage))
                     :mark "■" :tone :accent))
          (loop for sentence in (usage-sentences usage) collect (row sentence ""))
          (list (row (pad "Spend")
                     (format nil "~a  sum cost_usd~@[ · ~:d unpriced~]" (usd-text cost)
                             (let ((unpriced (usage-field totals "unpriced")))
                               (and unpriced (plusp unpriced) unpriced))))
                (row (pad "Model calls")
                     (format nil "~:d  provider requests" (or (usage-field totals "calls") 0)))
                (row (pad "Sessions")
                     (format nil "~:d  ~a" (or (usage-field totals "sessions") 0)
                             (counted (or (usage-field totals "turns") 0) "turn")))
                (row (pad "From cache")
                     (format nil "~a  cache hit~@[ · ~a~]"
                             (share-text (usage-field totals "cached_share"))
                             (let ((saved (usage-field totals "saved_usd")))
                               (cond ((not (realp saved)) nil)
                                     ((> saved 0.005) (format nil "saved about ~a" (usd-text saved)))
                                     ((< saved -0.005)
                                      (format nil "writes cost ~a more than reads saved" (usd-text (- saved))))))))
                (row (pad "First word, typical")
                     (format nil "~a  TTFT p50 · p90 ~a" (ms-text (usage-field usage "wait_p50_ms"))
                             (ms-text (usage-field usage "wait_p90_ms"))))
                (row (pad "Failed turns")
                     (format nil "~:d  turn.failed" (or (usage-field totals "failed_turns") 0))))
          (when (rest days)
            (let* ((priced (some (lambda (day) (usage-field day "cost_usd")) days))
                   (shown (last days 60))
                   (values (mapcar (lambda (day) (or (usage-field day (if priced "cost_usd" "calls")) 0))
                                   shown))
                   (top (reduce #'max values :initial-value 0)))
              (list (row (pad (if priced "Spend by day" "Model calls by day"))
                         (format nil "~a  ~a – ~a"
                                 (map 'string (lambda (value)
                                                (if (or (zerop top) (zerop value))
                                                    #\·
                                                    (char "▁▂▃▄▅▆▇█" (min 7 (floor (* 8 value) (* 1.0001 top))))))
                                      values)
                                 (subseq (usage-field (first shown) "day") 5)
                                 (subseq (usage-field (car (last shown)) "day") 5))))))
          (when models
            (cons (row (pad "Models") "calls · from cache · spend")
                  (loop for model in (subseq models 0 (min 8 (length models)))
                        collect (row (pad (format nil "  ~a" (usage-field model "model")))
                                     (format nil "~:d · ~a · ~a" (usage-field model "calls")
                                             (share-text (usage-field model "cached_share"))
                                             (usd-text (usage-field model "cost_usd")))))))
          (when sessions
            (cons (row (pad (if cost "Costliest sessions" "Busiest sessions")) "enter opens one")
                  (loop for session in sessions
                        for title = (usage-field session "title")
                        collect (row (pad (format nil "  ~a" (if (plusp (length title)) title
                                                                 (usage-field session "id"))))
                                     (format nil "~a · ~a · ~a" (folder-tail (usage-field session "folder"))
                                             (ms-text (usage-field session "ms"))
                                             (if (usage-field session "cost_usd")
                                                 (usd-text (usage-field session "cost_usd"))
                                                 (counted (usage-field session "calls") "call")))
                                     :value (list :session (usage-field session "id"))))))
          (when folders
            (cons (row (pad "Folders") "sessions · time · spend")
                  (loop for folder in folders
                        collect (row (pad (format nil "  ~a" (folder-tail (usage-field folder "folder"))))
                                     (format nil "~:d · ~a · ~a" (usage-field folder "sessions")
                                             (ms-text (usage-field folder "ms"))
                                             (usd-text (usage-field folder "cost_usd")))))))))))))

(defun dialog-read (model thunk continuation &aux (dialog model.dialog))
  "OFF-LOOP for the dialog open now: CONTINUATION takes (MODEL DIALOG VALUE)
while DIALOG is still the one open; one closed or replaced meanwhile takes
nothing, and owes no frame."
  (off-loop model thunk
            (lambda (model value)
              (if (eq dialog model.dialog) (funcall continuation model dialog value) :quiet))))

(defun usage-dialog-pick (model value)
  "Enter on a /usage row: a session's row opens that session."
  (when (and (consp value) (eq (first value) :session))
    (switch-gateway-session model (second value))))

(defun usage-dialog-account-rows (rows)
  "ROWS, the /usage dialog's, with each tab's nodecode.ai account rows (their
value (:ACCOUNT)) drawn again from *ACCOUNT-METER*; a tab whose read is still
out has none and takes them when it lands (USAGE-DIALOG-ROWS)."
  (loop for (id) in +usage-tabs+
        for own = (remove id rows :key #'picker-row-key :test-not #'equal)
        append (if (find '(:account) own :key #'picker-row-value :test #'equal)
                   (append (account-slab-rows id)
                           (remove '(:account) own :key #'picker-row-value :test #'equal))
                   own)))

(defun dialog-account-read (model redraw)
  "Read the nodecode.ai account's meter again from the gate for the dialog open now, off the loop:
it lands in *ACCOUNT-METER* and the dialog's rows become (REDRAW ROWS). The footer reads it at
connect and after a turn only, so a top-up bought while this shell was open read `API credit:
none' in the dialog until the next prompt (checkout audit, 2026-10-22)."
  (dialog-read model
               (lambda () (gateway-model-http-call model :get "/api/gateway/account?fresh=1" nil))
               (lambda-ignoring (model dialog value)
                 (unless (off-loop-failure value)
                   (setf *account-meter* (account-meter-of value)
                         dialog.rows (funcall redraw dialog.rows)))
                 nil)))

(defun slash-usage-op (model &optional args &aux (range (usage-range-word args)))
  "Open the /usage dialog on the range ARGS names, its reads off the loop."
  (cond
    ((null range) "Usage: /usage [today | 7d | 30d | all]")
    (t (open-picker-dialog model "Usage"
                           (loop for (id) in +usage-tabs+ append (usage-dialog-rows id nil))
                           #'usage-dialog-pick
                           :kind :usage
                           :context "every session"
                           :empty-label "Nothing ran in this range"
                           :hint "tab range  enter open session  esc close  ↑↓ navigate")
       (register-picker-tabs model.dialog (loop for (id label) in +usage-tabs+
                                                collect (list :label label :value id))
                             :active (position range +usage-tabs+ :key #'first :test #'equal))
       (dolist (id (cons range (remove range (mapcar #'first +usage-tabs+) :test #'equal)))
         (let ((id id))
           (dialog-read model
                        (lambda ()
                          (gateway-model-http-call
                           model :get (format nil "/api/gateway/usage?range=~a" id) nil))
                        (lambda-ignoring (model dialog value)
                          (setf dialog.rows (append (remove id dialog.rows :key #'picker-row-key
                                                                           :test #'equal)
                                                    (usage-dialog-rows id value)))
                          nil))))
       (dialog-account-read model #'usage-dialog-account-rows)
       nil)))

;;; --- /account ----------------------------------------------------------------
;;; What the footer's account badge opens: the nodecode.ai account alone, the
;;; plan's slab and the API credit's, no ranges and none of /usage's breakdown
;;; -- asked 2026-10-25: "just show the Nodecode.ai account and the API
;;; credit box, thats it". No headless arm: a room or a page shown this home's
;;; balance would read someone's account.

(defun slash-account-op (model)
  "Open the nodecode.ai account dialog: the plan's slab and the API credit's as the footer's meter
last read them, then read again from the gate (DIALOG-ACCOUNT-READ)."
  (flet ((rows (&optional stale)
           (declare (ignore stale))
           (account-slab-rows "")))
    (open-picker-dialog model "Usage" (rows) (constantly nil)
                        :kind :account
                        :empty-label "Not signed in to nodecode.ai: /login signs in"
                        :hint "esc close")
    (dialog-account-read model #'rows))
  nil)

;;; --- /think ------------------------------------------------------------------
;;; The picker and the footer read one answer, GET /api/gateway/target
;;; (TARGET-SNAPSHOT): the target this shell's session binds, its effort pick
;;; and the ladder that target admits. A pick lands through POST
;;; /api/gateway/sessions/<id>/effort (APPLY-EFFORT).

(defun target-snapshot (model)
  "=> (values PROVIDER MODEL-ID LADDER FACTS) from the gateway's target for
this shell's session: LADDER the effort rungs it admits, weakest first, FACTS
the footer's plist (:provider :model :unconfigured :pinned :pin-provider
:pin-model :effort :refusal :endpoint), and the default a new session starts on
(:default-provider :default-model)."
  ;; One dial answers the /think picker and the chrome, and an attached client
  ;; has no store to ask instead. The scope is what makes the answer address
  ;; the target the next turn on this session will bind rather than the
  ;; gateway's bare default.
  (let* ((target (gateway-model-http-call
                  model :get
                  (format nil "/api/gateway/target~@[?session_id=~a~]" model.session-id)
                  nil))
         (provider (json-value target :string "provider"))
         (model-id (json-value target :string "model"))
         (pin (json-value target :object "pin"))
         (default (json-value target :object "default")))
    (values provider model-id
            (coerce (json-array target "efforts") 'list)
            (list :provider provider
                  :model model-id
                  :unconfigured (json-value target :boolean "unconfigured")
                  :pinned (and pin t)
                  :pin-provider (json-value pin :text "provider")
                  :pin-model (json-value pin :text "model")
                  :effort (json-value target :text "effort")
                  :refusal (json-value target :text "refusal")
                  :endpoint (json-value target :text "endpoint")
                  :default-provider (json-value default :text "provider")
                  :default-model (json-value default :text "model")))))

(defun open-think-dialog (model provider model-id ladder effort on-select
                          &key (default "Use the provider default")
                          (subtitle (format nil "~a/~a" provider model-id)))
  ;; The items: Default -- the model's own reasoning, or what DEFAULT says
  ;; it is -- then every rung of the target's ladder weakest first, the
  ;; effort the target binds marked current and highlighted; the panel
  ;; shrinks to the rung count.
  (let ((rows (cons (make-picker-row :label "Default"
                                     :detail default
                                     :value nil)
                    (mapcar (lambda (rung)
                              (make-picker-row
                               :label rung
                               :detail (cond ((equal rung effort) "current")
                                             ((equal rung "off") "Answer without reasoning.")
                                             (t ""))
                               :value rung))
                            ladder))))
    (open-picker-dialog model
                        "Reasoning effort"
                        rows
                        on-select
                        :kind :think
                        :selection (if-let (index (position effort ladder :test #'equal))
                                     (1+ index) 0)
                        :subtitle subtitle
                        :empty-label "Reasoning controls are unavailable."
                        :hint "esc close  up/down navigate"))
  nil)

(defun open-effort-view (model on-effort on-continue &rest options)
  "The target's reasoning-effort view: its ladder's picker, ON-EFFORT taking the
pick, or -- a model with no controls -- one Continue row, ON-CONTINUE taking it."
  (multiple-value-bind (provider model-id ladder facts) (target-snapshot model)
    (if ladder
        (open-think-dialog model provider model-id ladder (getf facts :effort) on-effort)
        (apply #'open-picker-dialog model "Reasoning effort"
               (list (make-picker-row :label "Continue" :value :continue)) on-continue
               :kind :think :subtitle "This model has no reasoning effort controls." options))))

(defun apply-effort (model effort)
  "Land EFFORT — one rung of the target's ladder, or NIL for the model's own —
on this shell's session from its next turn, and as the default new sessions
start on: POST /api/gateway/sessions/<id>/effort (SET-SESSION-EFFORT)."
  ;; Returns NIL, so a picker's on-select can tail-call it.
  (session-control-call model :post "effort"
                        :body (json-object "effort" (or effort :null)))
  ;; The pick is this session's effort now: a launch's held one would only
  ;; be refused against it.
  (when (held-launch-target model)
    (let ((held (copy-list model.launch-target)))
      (remf held :effort)
      (setf model.launch-target held)))
  ;; The pick the footer renders came from the gateway with the identity it
  ;; belongs to; the landed pick makes that pair stale.
  (refresh-gateway-model-facts model)
  nil)

(defun slash-think-op (model &optional args)
  "Open the reasoning-effort picker, or apply one ARGS rung."
  ;; Returns notice text.
  (with-slash-notice ("Reasoning controls are unavailable"
                      :control "~a: ~a")
    (bind (((provider model-id ladder facts) (target-snapshot model))
           (tokens (split-words (or args "")))
           (value (unless (string-equal "default" (first tokens)) (first tokens))))
      (cond
        ((null ladder) "No reasoning control is available for this model.")
        ((> (length tokens) 1) (slash-usage "think"))
        (tokens
         (with-slash-notice ("reasoning control")
           (apply-effort model value)
           (format nil "reasoning effort for ~a/~a: ~a; applies from the next turn"
                   provider model-id (or value "provider default"))))
        (t
         (open-think-dialog
          model provider model-id ladder (getf facts :effort)
          (lambda (model value)
            ;; Enter is silent: the pick lands for the next turn and the
            ;; picker closes.
            (with-slash-notice ("reasoning control")
              (apply-effort model value)))))))))

