;;;; bare-test.lisp --- a command is its name: the dialog it answers typed alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. No command shows words after its name: the catalog's usage is empty
;;;;      for every builtin, and a cell's argument hint is taken and shown
;;;;      nowhere.
;;;;   2. A builtin typed alone opens its dialog, and Enter on a row runs the
;;;;      row's line in the shell.
;;;;   3. A row that asks hands the composer its label: the frame shows it,
;;;;      Enter runs the line with the text, and Esc puts it away.
;;;;   4. Headless, a builtin typed alone answers the same dialog, a row's
;;;;      line and ask in its JSON, and the line a row names still runs.
;;;;
;;;; Loads after slash-test: reuses WITH-FRAME-MODEL, TYPE-STRING and PRESS.

(in-package #:nodecode.test)

;;; --- claim 1: no words after a name -----------------------------------------

(deftest no-command-shows-words-after-its-name ()
  (dolist (entry nle::*slash-commands*)
    (is (equal "" (nle::slash-command-usage entry)) (nle::slash-command-name entry))
    (is (equal (format nil "/~a" (nle::slash-command-name entry))
               (nle::slash-command-label entry))))
  (let ((nle::*registered-commands* '()))
    (let ((entry (nle::register-command "bare-test" "bare-probe" (lambda (args session-id)
                                                                   (declare (ignore session-id))
                                                                   args)
                                        :argument-hint "on | off" :complete 'ignore)))
      (is (equal "" (nle::slash-command-usage entry)) "the hint is taken and shown nowhere")
      (is (null (nle::slash-command-completer entry)) "nothing completes after the name"))))

;;; --- claim 2: a builtin typed alone opens its dialog -------------------------

(deftest a-builtin-typed-alone-opens-its-dialog ()
  (with-frame-model (model 100 30 :session-id "bare-details")
    (type-string model "/details" :enter)
    (is-present (dialog (nle::chat-model-dialog model)) "the viewport dialog is open"
      (is (equal '("Standard" "Developer")
                 (mapcar #'nle::picker-row-label (nle::picker-dialog-rows dialog))))
      (is (equal "●" (nle::picker-row-mark (first (nle::picker-dialog-rows dialog))))
          "the viewport in use is marked"))
    (dialog-keys model :down :enter)
    (is (null (nle::chat-model-dialog model)) "a pick closes it")
    (is (eq :developer (nle::chat-model-tool-detail model)) "the row's line ran here")))

;;; --- claim 3: a row that asks hands the composer its label ---------------------

(defmacro with-probe-dialog ((dialog) &body body)
  "Run BODY with the builtin /details answering DIALOG typed alone."
  `(let ((saved (gethash "details" nle::*bare-dialogs*)))
     (unwind-protect
          (progn (nle::define-bare-dialog "details" (session-id model) ,dialog)
                 ,@body)
       (setf (gethash "details" nle::*bare-dialogs*) saved))))

(deftest a-row-that-asks-hands-the-composer-its-label ()
  (with-probe-dialog ((nle::list-dialog :title "Viewport"
                                        :rows (list (nle::ask-row "Type one" "Viewport" "/details"
                                                                  :placeholder "standard or developer"
                                                                  :verb "switch"))))
    (with-frame-model (model 100 30 :session-id "bare-ask")
      (type-string model "/details" :enter)
      (dialog-keys model :enter)
      (is (null (nle::chat-model-dialog model)) "the dialog gave way to the composer")
      (is-present (ask (nle::chat-model-composer-ask model)) "the composer wears the ask"
        (is (equal "Viewport" (getf ask :label)))
        (is (equal "/details" (getf ask :line))))
      (let ((rows (nle::compose-frame model)))
        (is (rows-carry-p " Viewport " rows) "the label stands over the text")
        (is (rows-carry-p "standard or developer" rows) "the empty field says what it waits for")
        (is (rows-carry-p " switch " rows) "Enter's verb is named"))
      (type-string model "/no-command")
      (is (null (nle::composer-suggestions-for model)) "an ask's text opens no dropdown")
      (nle::editor-reset model.editor)
      (press model :enter)
      (is (nle::chat-model-composer-ask model) "an empty ask waits")
      (type-string model "developer" :enter)
      (is (null (nle::chat-model-composer-ask model)) "Enter spent the ask")
      (is (eq :developer (nle::chat-model-tool-detail model)) "the line ran with the text")
      (is (equal "" (composer-value model)))
      ;; Esc puts the next one away, its text kept as a plain draft.
      (type-string model "/details" :enter)
      (dialog-keys model :enter)
      (type-string model "standard")
      (press model :escape)
      (is (null (nle::chat-model-composer-ask model)) "Esc put it away")
      (is (eq :developer (nle::chat-model-tool-detail model)) "nothing ran")
      (is (equal "standard" (composer-value model))))))

;;; --- claim 4: headless, the same dialog ---------------------------------------

(deftest headless-a-builtin-typed-alone-answers-its-dialog ()
  (multiple-value-bind (text dialog) (nle::slash "/gc")
    (is (search "Restart the window" text) "the rows in words, for a caller that draws none")
    (is-present (rows (and dialog (nle::json-array dialog "rows"))) "the dialog rides the answer"
      (is (equal '("/gc show" "/gc reset")
                 (map 'list (lambda (row) (nle::json-value row :text "run" "enter")) rows)))))
  (is (search "collections" (string-downcase (nle::slash "/gc show"))) "a row's line still runs")
  (let ((json (nle::dialog-json
               (nle::list-dialog :rows (list (nle::ask-row "Name it" "Name" "/rename"
                                                           :placeholder "its name" :verb "rename"))))))
    (is (equal '("Name" "/rename" "its name" "rename")
               (mapcar (lambda (key) (nle::json-value (aref (nle::json-array json "rows") 0) :text "ask" key))
                       '("label" "line" "placeholder" "verb"))))
    (is (equal "/rename my name" (nle::ask-line "/rename" (format nil "my~%name"))))))
