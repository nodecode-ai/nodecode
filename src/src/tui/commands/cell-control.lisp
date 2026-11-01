;;;; cell-control.lisp --- the gateway's cell commands and the dialog a command's answer carries. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- the gateway's cell commands -----------------------------------------
;;; A shell runs its builtins in its own image and a cell's command where
;;; the cell runs: it reads the catalog the gateway serves (GET
;;; /api/gateway/slash, whose builtins SET-GATEWAY-COMMANDS drops) and runs one
;;; through POST /api/gateway/slash, as the web page does. The answer's text is
;;; the notice; the panel it carries opens as this shell's picker.

(defun gateway-command-entries (model)
  "The commands MODEL's gateway serves (GET /api/gateway/slash), as the plists
SET-GATEWAY-COMMANDS takes."
  (loop for command across (json-array (gateway-model-http-call model :get "/api/gateway/slash" nil)
                                       "commands")
        collect (list :name (json-value command :string "name")
                      :description (or (json-value command :string "description") "")
                      :usage (or (json-value command :string "usage") "")
                      :session (json-value command :boolean "session")
                      :trigger (json-value command :text "trigger"))))

(defun gateway-slash-answer (model line)
  "The answer MODEL's gateway gives the slash LINE (POST /api/gateway/slash),
run in MODEL's session."
  (gateway-model-http-call
   model :post "/api/gateway/slash"
   (json-object "line" line :opt "session_id" (json-value model.session-id :text))))

(defun run-gateway-command (model entry args &key trigger)
  "Run ENTRY, a cell's command MODEL's gateway serves, with its verbatim
argument tail ARGS, and answer the notice: the answer's text, or NIL when the
panel it carries opened; TRIGGER when the composer's trigger character opened
it."
  (let* ((name (slash-command-name entry))
         (line (format nil "/~a~@[ ~a~]" name (json-value args :text))))
    (with-slash-notice ((format nil "/~a" name))
      (let ((answer (gateway-slash-answer model line)))
        (if (json-value answer :object "dialog")
            (progn (open-answer-dialog model answer :line line :trigger trigger) nil)
            (json-value answer :string "text"))))))

;;; --- the dialog a command's answer carries --------------------------------
;;; A command's answer may carry a docked picker (GATEWAY.LISP LIST-DIALOG):
;;; the shell that ran the command owns the panel, so a cell describes the
;;; list once and every shell paints it. The reader is the same on both
;;; transports -- the value's JSON -- so a local call and a gateway run open
;;; the same dialog.

(defun dialog-rows (dialog)
  "The picker rows DIALOG's JSON carries (LIST-DIALOG's ROWS), each row's own
JSON its value: what Enter inserts and the lines its keys run are read off it."
  (loop for row across (json-array dialog "rows")
        for tone = (json-value row :string "mark_tone")
        collect (make-picker-row
                 :key (or (json-value row :string "key") "")
                 :label (or (json-value row :string "label") "")
                 :detail (or (json-value row :string "detail") "")
                 :suffix (or (json-value row :string "suffix") "")
                 :value row
                 :mark (or (json-value row :string "mark") "")
                 :mark-tone (if tone (intern (string-upcase tone) :keyword) :muted))))

(defun dialog-tabs (dialog)
  "The tab strip DIALOG's JSON carries (LIST-DIALOG's TABS) as the plists
REGISTER-PICKER-TABS takes, or NIL for a dialog with no strip."
  (loop for tab across (json-array dialog "tabs")
        collect (list* :label (or (json-value tab :string "label") "")
                       (json-members tab :any "count" "value"))))

(defun dialog-picture (dialog)
  "The picture DIALOG's JSON carries (LIST-DIALOG's PICTURE) as the plist
REGISTER-PICKER-PICTURE takes, or NIL for a dialog with none."
  (when-let (picture (json-value dialog :object "picture"))
    (let ((palette (coerce (remove-if-not #'stringp (json-array picture "palette")) 'list)))
      (list* :rows (coerce (remove-if-not #'stringp (json-array picture "rows")) 'list)
             :label (or (json-value picture :string "label") "")
             (append (and palette (list :palette palette))
                     (and (json-value picture :boolean "beside") (list :beside t)))))))

(defun open-answer-dialog (model answer &key line trigger under (selection 0) label)
  "Open the dialog ANSWER -- the answer to the slash LINE -- carries as
MODEL's picker, on the row LABEL names, else row SELECTION, over UNDER: the
answer dialogs it was opened from, nearest first, as (LINE SELECTION LABEL).
A command whose answer is its text alone opens nothing."
  ;; The row's VALUE is what Enter hands the composer: a command that answers
  ;; with a list of things to name picks one into the draft at the caret,
  ;; which is how /index lands a `$name'. Opened by its TRIGGER, the
  ;; character the operator typed is still at the caret, and a value that
  ;; begins with it takes its place. A row's RUN lines outrank it: Enter and
  ;; Space run them (ANSWER-GESTURE), and Esc -- or Enter on a row that does
  ;; neither -- goes back to the dialog under this one, read again.
  (when-let (dialog (json-value answer :object "dialog"))
    (let* ((rows (dialog-rows dialog))
           (picker nil))
      (labels ((stack (&aux (row (selected-of (picker-visible-rows picker)
                                              (picker-dialog-selection picker))))
                 ;; This dialog atop UNDER, on the row the cursor is on: read
                 ;; again, the row keeps the cursor wherever the list moved it.
                 (cons (list line (or (position row rows) 0) (and row (picker-row-label row)))
                       under))
               (gesture (model value key)
                 (when-let (run (json-value value :text "run" key))
                   (setf (chat-model-dialog model) nil)
                   (answer-gesture model run (stack))
                   t)))
        (open-picker-dialog
         model
         (or (json-value dialog :string "title") "Pick one")
         rows
         (lambda (model value &aux (text (json-value value :text "value")))
           (cond ((gesture model value "enter"))
                 (text
                  (when (and trigger (uiop:string-prefix-p trigger text)
                             (uiop:string-suffix-p (editor-value model.editor) trigger))
                    (loop repeat (length trigger)
                          do (editor-handle-key model.editor (nlt:make-key-press-msg :code :backspace))))
                  (editor-insert model.editor text))
                 (t (reopen-answer-dialog model under)))
           nil)
         :selection (or (and label (position label rows :key #'picker-row-label :test #'equal))
                        (min selection (max 0 (1- (length rows)))))
         :on-cancel (lambda (model) (reopen-answer-dialog model under))
         :on-space (and (find-if (lambda (row) (json-value (picker-row-value row) :text "run" "space"))
                                 rows)
                        (lambda (model dialog)
                          (gesture model (picker-highlighted-value dialog) "space")))
         :searchable (json-value dialog :boolean "searchable")
         :search-placeholder (or (json-value dialog :string "search_placeholder") "")
         :empty-label (or (json-value dialog :string "empty_label") "Nothing to pick")
         :no-match-label (json-value dialog :string "no_match_label")
         :hint (json-value dialog :string "hint")
         :context (or (json-value dialog :string "context") "")
         :subtitle (or (json-value dialog :string "summary") "")
         :summary-tone (if-let (tone (json-value dialog :string "summary_tone"))
                         (intern (string-upcase tone) :keyword)
                         :muted)
         :kind :answer)
        (setf picker model.dialog)
        (register-picker-tabs picker (dialog-tabs dialog))
        (register-picker-picture picker (dialog-picture dialog))))))

(defun answer-gesture (model line stack)
  "Run LINE, the line a row's key names in the answer dialog atop STACK, where
the command ran: a dialog it answers opens over STACK; else what it says is a
toast, the one line the panel leaves in sight, and STACK is read again
(REOPEN-ANSWER-DIALOG)."
  (multiple-value-bind (answer failed) (with-slash-notice (line) (gateway-slash-answer model line))
    (if (and (not failed) (json-value answer :object "dialog"))
        (open-answer-dialog model answer :line line :under stack)
        (progn (show-status model (if failed answer (json-value answer :string "text")) :error-p failed)
               (reopen-answer-dialog model stack)))))

(defun reopen-answer-dialog (model stack)
  "Open the answer dialog atop STACK -- (LINE SELECTION LABEL) entries,
nearest first -- read again on its row; one whose line answers no dialog now,
what it showed gone, gives way to the one under it. An empty STACK opens
nothing."
  (loop for ((line selection label) . under) on stack
        for answer = (ignore-errors (gateway-slash-answer model line))
        when (json-value answer :object "dialog")
          return (open-answer-dialog model answer :line line :under under
                                                  :selection selection :label label)))

