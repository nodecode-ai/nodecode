;;;; bare-test.lisp --- a command is its name: the dialog it answers typed alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. No command shows words after its name: every label is the bare
;;;;      name, the catalog a room reads carries no usage, and a cell's
;;;;      argument hint and completer are taken and shown nowhere.
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
    (is (equal (format nil "/~a" (nle::slash-command-name entry))
               (nle::slash-command-label entry))
        (nle::slash-command-name entry)))
  (let ((nle::*registered-commands* '()))
    (let ((entry (nle::register-command "bare-test" "bare-probe" (lambda (args session-id)
                                                                   (declare (ignore session-id))
                                                                   args)
                                        :argument-hint "on | off" :complete 'ignore)))
      (is (equal "/bare-probe" (nle::slash-command-label entry)) "the hint is taken and shown nowhere")
      (is-present (row (find "bare-probe" (nle:slash-catalog) :key #'second :test #'equal)) "a room reads it"
        (is (null (getf row :usage)) "with no usage")
        (is (null (getf row :autocomplete)) "and nothing to complete"))
      (is (null (nle:slash-completions "bare-probe" "o")) "nothing completes after the name")
      (is (search "/bare-probe — " (nle::slash-help-text "" nil)) "help names it alone"))))

;;; --- claim 2: a builtin typed alone opens its dialog -------------------------

(deftest a-builtin-typed-alone-opens-its-dialog ()
  (with-frame-model (model 100 30 :session-id "bare-details")
    (type-string model "/details" :enter)
    (is-present (dialog (nle::chat-model-dialog model)) "the viewport dialog is open"
      (is (equal '("Standard" "Developer")
                 (mapcar #'nle::picker-row-label (nle::picker-dialog-rows dialog))))
      (is (equal "●" (nle::picker-row-mark (first (nle::picker-dialog-rows dialog)))) "the one in use is marked"))
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
      ;; Esc puts the next one away with its text: Enter would send it on as a prompt.
      (type-string model "/details" :enter)
      (dialog-keys model :enter)
      (type-string model "standard")
      (press model :escape)
      (is (null (nle::chat-model-composer-ask model)) "Esc put it away")
      (is (eq :developer (nle::chat-model-tool-detail model)) "nothing ran")
      (is (equal "" (composer-value model)) "its text went with it"))))

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

;;; --- claim 5: /export saves here, or where it is told ------------------------------

(deftest export-typed-alone-saves-here-or-asks-where ()
  (with-frame-model (model 100 30 :session-id "bare-export")
    (is (null (nle::bare-dialog "export" "bare-export" model)) "a session with no prompt has nothing to save")
    (is (null (nle::bare-dialog "export" "bare-export" nil)) "a room has no export")
    (with-stubbed-fdefinition (nle::home-screen-p (model) nil)
      (is-present (rows (getf (nle::bare-dialog "export" "bare-export" model) :rows)) "the dialog opens"
        (destructuring-bind (here elsewhere) rows
          (is-carrying (line (getf (getf here :run) :enter))
            (is (uiop:string-prefix-p "/export " line))
            (is (uiop:absolute-pathname-p (uiop:parse-native-namestring (subseq line 8))) "this folder, whole"))
          (is (equal "/export" (getf (getf elsewhere :ask) :line)) "a path elsewhere is asked for"))))))

;;; --- claim 6: /evict offers its shares and a turn -----------------------------------

(deftest evict-typed-alone-offers-its-shares-and-a-turn (with-temp-store ())
  (do-turns (make-test-session "bare-evict") 3)
  (multiple-value-bind (text dialog) (nle:slash "/evict" :session-id "bare-evict")
    (is (search "Half the history" text))
    ;; Half leads, as typed alone it did before; a turn is picked from
    ;; /context's history.
    (is (equal '("/evict 50" "/evict 25" "/evict 75" "/evict 100" "/context history")
               (map 'list (lambda (row) (nle::json-value row :text "run" "enter"))
                    (nle::json-array dialog "rows")))))
  (is (search "Evicted" (nle:slash "/evict 50" :session-id "bare-evict")) "a row's line still runs"))

(deftest evict-typed-alone-in-a-shell-opens-the-same-dialog ()
  (with-frame-model (model 100 30 :session-id "bare-evict-shell")
    (type-string model "/compact" :enter)
    (is-present (dialog (nle::chat-model-dialog model)) "its alias opens the same dialog"
      (is (equal "Half the history" (nle::picker-row-label (first (nle::picker-dialog-rows dialog))))))))

;;; --- claim 7: /gateway's verbs are its rows -------------------------------------------

(deftest gateway-typed-alone-offers-every-verb-a-shell-runs ()
  (with-frame-model (model 100 30 :session-id "bare-gateway")
    (flet ((lines (inside)
             (with-stubbed-fdefinition (nlk:inside-own-gateway-p (&key home)
                                         (declare (ignore home))
                                         inside)
               (mapcar (lambda (row) (getf (getf row :run) :enter))
                       (getf (nle::bare-dialog "gateway" "bare-gateway" model) :rows)))))
      (is (equal '("/gateway status" "/gateway on" "/gateway restart" "/gateway off" "/gateway logs"
                   "/gateway setup discord" "/gateway setup telegram" "/gateway setup slack")
                 (lines nil)))
      (is (equal '("/gateway restart --force" "/gateway off --force") (last (lines t) 2)) "inside it"))
    (is (null (nle::bare-dialog "gateway" "bare-gateway" nil)) "a room runs no gateway verb")))

;;; --- claim 8: an ask stands over a draft, and gives it back ----------------------------

(deftest an-ask-gives-the-draft-it-stood-over-back ()
  (with-frame-model (model 100 30 :session-id "bare-draft")
    (flet ((ask ()
             (let ((json (nle::dialog-json
                          (nle::list-dialog :rows (list (nle::ask-row "Type one" "Viewport" "/details"))))))
               (nle::ask-in-composer model (nle::json-value (aref (nle::json-array json "rows") 0)
                                                            :object "ask")))))
      (type-string model "half a prompt $")
      (ask)
      (is (equal "" (composer-value model)) "the ask takes the composer")
      (type-string model "developer" :enter)
      (is (eq :developer (nle::chat-model-tool-detail model)) "its line ran")
      (is (equal "half a prompt $" (composer-value model)) "the draft is back")
      (ask)
      (type-string model "standard")
      (press model :escape)
      (is (eq :developer (nle::chat-model-tool-detail model)) "Esc ran nothing")
      (is (equal "half a prompt $" (composer-value model)) "and gave the draft back"))))

;;; --- claim 9: Tab writes the name alone --------------------------------------------------

(deftest tab-writes-the-command-name-alone ()
  (with-saved-globals (nle::*gateway-commands*)
    (nle::set-gateway-commands (list (list :name "gen" :description "a probe")))
    ;; A whole name leads the longer one it also starts.
    (is (equal "/gen" (nle::slash-command-label (first (nle::slash-suggestions "/gen")))))
    (with-frame-model (model 100 30 :session-id "bare-tab")
      (type-string model "/mod" :tab)
      (is (equal "/models" (composer-value model)) "no blank after the name invites words")
      ;; The dropdown stays on it, for Enter.
      (is (equal "/models" (nle::composer-suggestion-text (nle::selected-composer-suggestion model)))))))
