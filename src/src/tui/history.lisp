;;;; history.lisp --- the help dialog and prompt history. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- help dialog -------------------------------------------------------------
;;; The Zig help_dialog.zig distilled onto the picker: the slash-command
;;; catalog (label + description) followed by the keybind listing, each
;;; keybind row showing the currently configured chord from the resolved
;;; keymap. Searchable, like every Zig help-dialog view.

(defun help-dialog-rows (model)
  "The help dialog's rows: every slash command (the gateway's cell commands
included), then every keybind action with its resolved key label."
  ;; A command carrying the Zig help_detail copy shows it; the dropdown
  ;; description covers the rest.
  (append
   (loop for entry in (append *slash-commands* *gateway-commands*)
         collect (make-picker-row
                  :label (slash-command-label entry)
                  :detail (or (json-value entry.help-detail :text) entry.description)))
   (loop for (action label) in *keybind-actions*
         collect (make-picker-row
                  :label (keybind-chords-label (chat-model-keymap model)
                                               action)
                  :detail label))))

(defun open-help-dialog (model)
  "Open the keyboard help dialog (the `?` trigger and the /help command)."
  ;; The Zig shortcut picker counts its entries inside the tab label
  ;; (shortcuts_tab_label).
  (let ((rows (help-dialog-rows model)))
    (open-picker-dialog model (format nil "Shortcuts (~d)" (length rows))
                        rows
                        nil
                        :searchable t
                        :search-placeholder "Type to search shortcuts…"
                        :empty-label "No shortcuts found."
                        :footer-hints :help))
  nil)

;;; --- prompt history --------------------------------------------------------
;;; The Zig composer history, distilled (src/tui/model/composer.zig +
;;; root.zig navigateComposerHistoryPrevious/Next): newest-first entries,
;;; dedupe-to-front on submit, a 1000-entry cap, prefix-filtered walking —
;;; the draft the walk started from filters which entries Up/Down visit and
;;; is restored when Down passes the newest match. Durable across restarts
;;; as one JSON-encoded prompt per line, appended on submit, loaded lazily
;;; on first recall.

(defparameter *max-composer-history-entries* 1000
  "Bound on recallable prompts (Zig max_composer_history_entries).")

(define-startup-parameter *composer-history-path*
  (home "composer-history")
  "Where submitted prompts persist, one JSON string per line. Tests rebind.")

(defun ensure-composer-history (model)
  (unless (chat-model-history-loaded model)
    ;; An unreadable line skips; a NIL path (the test runner's) persists nothing.
    (let ((entries (and *composer-history-path*
                        (read-json-lines *composer-history-path*
                                         :test (lambda (value) (json-value value :text))))))
      (setf (chat-model-prompt-history model)
            (subseq (reverse entries) 0 (min (length entries) *max-composer-history-entries*))
            (chat-model-history-loaded model) t))))

(defun reset-history-navigation (model)
  (setf (chat-model-history-index model) nil
        (chat-model-history-stash model) ""))

(defun note-submitted-prompt (model text)
  "Record TEXT on submit: dedupe to the front, cap, persist, and leave
navigation reset (Zig appendComposerHistory + history.appendToPath)."
  (ensure-composer-history model)
  (let ((entries (cons text (remove text (chat-model-prompt-history model)
                                    :test #'string=))))
    (setf (chat-model-prompt-history model)
          (subseq entries 0 (min (length entries)
                                 *max-composer-history-entries*))))
  (when *composer-history-path*
    (ignore-errors (append-json-line *composer-history-path* text)))
  (reset-history-navigation model))

(defun composer-cursor-at-end-p (ta &aux (value (editor-value ta))
                                         (newline (position #\Newline value :from-end t)))
  "Whether the caret sits at the very end of the composer text — the Zig
gate on entering history navigation."
  (and (= (editor-row ta)
          (1- (editor-line-count ta)))
       (= (editor-col ta) (- (length value) (if newline (1+ newline) 0)))))

(defun replace-composer-text (model text &aux (ta (chat-model-editor model)))
  "Install TEXT with the caret at its end: reset, then insert."
  (editor-reset ta)
  (editor-insert ta text))

(defun composer-history-previous (model)
  "Walk one entry older; T when the key was consumed."
  ;; Declines — so the cursor moves instead — when the caret is mid-text,
  ;; nothing matches, or history is empty (Zig
  ;; navigateComposerHistoryPrevious).
  (ensure-composer-history model)
  (let ((ta (chat-model-editor model)))
    (when (and (chat-model-prompt-history model)
               (composer-cursor-at-end-p ta))
      (let* ((index (chat-model-history-index model))
             (prefix (if index
                         (chat-model-history-stash model)
                         (editor-value ta)))
             (entries (chat-model-prompt-history model)))
        (loop for candidate from (if index (1+ index) 0)
                below (length entries)
              for entry = (nth candidate entries)
              when (and (uiop:string-prefix-p prefix entry)
                        ;; A fresh walk skips the entry equal to the draft.
                        (not (and (null index)
                                  (plusp (length prefix))
                                  (string= entry prefix))))
                do (when (null index)
                     (setf (chat-model-history-stash model) prefix))
                   (replace-composer-text model entry)
                   (setf (chat-model-history-index model) candidate)
                   (return t))))))

(defun composer-history-next (model)
  "Walk one entry newer, restoring the stashed draft past the newest match;
T when the key was consumed. Declines while not navigating."
  (when-let (index (chat-model-history-index model))
    (let ((prefix (chat-model-history-stash model))
          (entries (chat-model-prompt-history model)))
      (loop for candidate from (1- index) downto 0
            for entry = (nth candidate entries)
            when (uiop:string-prefix-p prefix entry)
              do (replace-composer-text model entry)
                 (setf (chat-model-history-index model) candidate)
                 (return-from composer-history-next t))
      (replace-composer-text model prefix)
      (reset-history-navigation model)
      t)))

(defparameter *max-prompt-history-rows* 5000
  "Bound on the prompts the history dialog reads out of the store in one
read, newest first — a bound on the read, not on the surface: a read that
hit it says so in the title.")

(defun prompt-history-origin (session-id)
  "Which lane a prompt arrived on, read off the session id that carried it."
  ;; The channel lanes and the cron jobs name themselves in their ids;
  ;; anything else is a shell of this home's own — the TUI's session, an
  ;; agent's, a subagent's.
  (or (find-if (lambda (lane) (uiop:string-prefix-p lane (or session-id "")))
               '("discord" "telegram" "cron"))
      "tui"))

(defun prompt-history (&key (limit *max-prompt-history-rows*))
  "Every prompt committed to a turn on this store, newest first, as (LANE
TEXT): the lane it arrived on (PROMPT-HISTORY-ORIGIN) and the prompt verbatim.
The organism's side of a shell's history: GET /api/gateway/prompts serves it."
  ;; The store is the only record that spans every way a prompt reaches this
  ;; organism — the TUI composer, a Discord or Telegram lane, a cron fire — so
  ;; this is what `every prompt' means; the composer ring beside it holds one
  ;; home's typed drafts and only those. Two kinds of row are not a person's
  ;; prompt and are left out: a `follow-up' disposition (queue state, whose
  ;; promotion commits the same prompt a second time as `started'), and the
  ;; reflection fork's own words ([experience] ...), which the harness injects
  ;; into a session rather than anyone typing them.
  ;;
  ;; => NIL when no store is open or it holds none.
  (when (nlk:store-open-p)
    ;; Keep the bound on input events BEFORE exclusions. Read plain prompts
    ;; directly; only structured messages need the canonical JSON/content
    ;; reader. Fetching and decoding the entire envelope cost ~1 s here.
    (loop for (session-id disposition prompt-id type raw) in
            (nlk:events :kind "turn.input_committed" :order :newest :limit limit
                        :as :rows
                        :columns '("session_id" "json_extract(payload, '$.disposition')"
                                   "json_extract(payload, '$.prompt-id')"
                                   "json_type(payload, '$.message')"
                                   "json_extract(payload, '$.message')"))
          unless (or (equal disposition "follow-up")
                     (and (stringp prompt-id) (uiop:string-prefix-p "experience:" prompt-id))
                     (and session-id (uiop:string-prefix-p "experience" session-id)))
            append (let ((text (cond ((equal type "text") raw)
                                     ((equal type "object") (message-content (nlk:decode-json raw)))
                                     (t ""))))
                     (unless (or (zerop (length text)) (uiop:string-prefix-p "[experience]" text))
                       (list (list (prompt-history-origin session-id) text)))))))

(nlk:define-side-table prompt-history-read (:weakness :key)
  "Live-only history read per shell: cached rows, readiness, in-flight read
and the dialog that requested them. The store remains the history authority.")

(nlk:define-side-table prompt-history-search (:weakness :key)
  "Live-only full rows and the one coalesced query read per history dialog.")

(defun stage-prompt-history-search (dialog query &optional seat
                                    &aux (state (prompt-history-search dialog)))
  "Echo the query now; rank only its rested value off the UI loop."
  (setf (getf state :query) query (getf state :at) (get-internal-real-time)
        (getf state :seat) seat
        (picker-dialog-rows dialog) (unless (plusp (length query)) (getf state :rows))
        (picker-dialog-on-tick dialog) (when (and (getf state :ready) (plusp (length query)))
                                         #'flush-prompt-history-search)
        (picker-dialog-empty-label dialog) (cond ((not (getf state :ready)) "Reading prompt history…")
                                                ((plusp (length query)) "Searching prompts…")
                                                (t "No prompts submitted yet"))
        (picker-dialog-no-match-label dialog) (picker-dialog-empty-label dialog))
  (picker-rows-filtered-for dialog query))

(defun flush-prompt-history-search (model dialog &aux (state (prompt-history-search dialog)))
  "Start at most one ranking after 120 ms of query rest; the frame remains free."
  (when (and (eq dialog (chat-model-dialog model)) (not (getf state :reading))
             (>= (- (get-internal-real-time) (getf state :at))
                 (* 0.12 internal-time-units-per-second)))
    (let ((query (getf state :query)) (rows (getf state :rows)))
      (setf (getf state :reading) t (picker-dialog-on-tick dialog) nil)
      (off-loop model
                (lambda () (picker-visible-rows (make-picker-dialog :rows (copy-list rows) :search query)))
                (lambda (model answer)
                  (setf (getf state :reading) nil)
                  (cond
                    ((not (eq dialog (chat-model-dialog model))) :quiet)
                    ((or (not (equal query dialog.search)) (not (eq rows (getf state :rows))))
                     (when (plusp (length (picker-dialog-search dialog)))
                       (setf (picker-dialog-on-tick dialog) #'flush-prompt-history-search))
                     :quiet)
                    (t
                     (setf (picker-dialog-rows dialog) (unless (off-loop-failure answer) answer)
                           (picker-dialog-on-tick dialog) nil
                           (picker-dialog-empty-label dialog)
                           (or (off-loop-failure answer) "No prompts match this filter")
                           (picker-dialog-no-match-label dialog) (picker-dialog-empty-label dialog)
                           (picker-dialog-selection dialog)
                           (or (position (getf state :seat) (picker-dialog-rows dialog)
                                         :key #'picker-row-value :test #'equal) 0))
                     (picker-rows-filtered-for dialog query))))))))

(defun prompt-history-title (rows)
  "Count the loaded rows, disclosing the existing read bound with a plus."
  (format nil "History (~d~:[~;+~])" (length rows)
          (= (length rows) *max-prompt-history-rows*)))

(defun read-prompt-history-rows (model drafts &optional cached)
  "Read and prepare history off the loop; name a refusal when using drafts."
  (let* ((answer (handler-case (gateway-model-http-call model :get "/api/gateway/prompts" nil)
                   (error (condition) (list :error (nlk:safe-condition-report condition)))))
         (failure (off-loop-failure answer))
         (rows (unless failure
                 (loop for prompt across (json-array answer "prompts")
                       for text = (json-value prompt :string "text")
                       when text collect (make-picker-row :key (json-value prompt :string "lane")
                                                         :label (nlk:one-line text) :value text)))))
    (when (and rows (equalp rows cached)) (setf rows cached))
    (list :rows (or rows (loop for text in drafts
                              collect (make-picker-row :label (nlk:one-line text) :value text)))
          :failed failure
          :detail (cond (failure (format nil "Store history unavailable: ~a" failure))
                        ((and (null rows) drafts) "Store history is empty — showing typed prompts")))))

(defun install-prompt-history-rows (model answer &aux (state (prompt-history-read model))
                                                     (dialog (getf state :dialog)))
  "Land on the UI loop, preserving search and the highlighted prompt; never reopen."
  (setf (getf state :reading) nil)
  (let ((failure (off-loop-failure answer)))
    (unless (or failure (and (getf answer :failed)
                            (or (getf state :ready) (null (getf answer :rows)))))
      (setf (getf state :rows) (getf answer :rows) (getf state :ready) t))
    (unless (and dialog (eq dialog (chat-model-dialog model)))
      (return-from install-prompt-history-rows :quiet))
    (bind ((rows (getf state :rows))
           (ready (getf state :ready))
           (searched (prompt-history-search dialog)))
      (setf dialog.title (if ready (prompt-history-title rows) "History (unavailable)")
            dialog.context (or failure
                               (when (getf answer :failed)
                                 (format nil "~a — showing ~a" (getf answer :detail)
                                         (if (eq rows (getf answer :rows))
                                             "typed prompts" "previously loaded history")))
                               (getf answer :detail) "")
            dialog.subtitle dialog.context)
      (when (zerop (length dialog.search))
        (setf dialog.empty-label
              (if ready "No prompts submitted yet" "History unavailable — reopen to retry")))
      ;; The worker reuses the cached list when the store returned the
      ;; same facts. A refresh must not blank already-ranked search hits.
      (unless (and (eq (getf searched :rows) rows) (eq (getf searched :ready) ready))
        (let* ((pick (nth dialog.selection (picker-visible-rows dialog)))
               (value (and pick (picker-row-value pick))))
          (setf (getf searched :rows) rows (getf searched :ready) ready)
          (if (plusp (length dialog.search))
              (stage-prompt-history-search dialog dialog.search value)
              (setf dialog.rows rows
                    dialog.selection (or (and pick (position value rows :key #'picker-row-value
                                                                         :test #'equal))
                                         0))))))))

(defun open-prompt-history-dialog (model)
  "Ctrl+R and /history: paint first, refresh the store-backed rows off the loop."
  (ensure-composer-history model)
  (let* ((state (or (prompt-history-read model)
                    (setf (prompt-history-read model) (list :ready nil :rows nil :reading nil :dialog nil))))
         (rows (getf state :rows)))
    (open-picker-dialog model (if (getf state :ready) (prompt-history-title rows) "History (reading…)")
                        rows
                        (lambda (model text)
                          (replace-composer-text model text)
                          (reset-history-navigation model)
                          nil)
                        :kind :prompt-history :searchable t :on-search #'stage-prompt-history-search
                        :search-placeholder "Type to search prompts…"
                        :empty-label (if (getf state :ready) "No prompts submitted yet" "Reading prompt history…")
                        :no-match-label "No prompts match this filter"
                        :hint "enter to composer  esc/ctrl+r close  ↑↓ navigate  type search")
    (setf (getf state :dialog) (chat-model-dialog model)
          (prompt-history-search (chat-model-dialog model))
          (list :rows rows :ready (getf state :ready) :query "" :at 0 :reading nil :seat nil))
    ;; Reopening while the read runs reuses it, and seats its answer in this
    ;; dialog only. Neither closing nor a different dialog can be undone.
    (unless (getf state :reading)
      (setf (getf state :reading) t)
      (let ((drafts (copy-list (chat-model-prompt-history model)))
            (cached (getf state :rows)))
        (off-loop model (lambda () (read-prompt-history-rows model drafts cached))
                  #'install-prompt-history-rows))))
  nil)

