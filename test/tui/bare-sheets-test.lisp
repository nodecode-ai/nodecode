;;;; bare-sheets-test.lisp --- the sheets' commands typed alone where no sheet opens.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. Without a shell, /models, /usage, /cells and /profile typed alone
;;;;      answer the dialog their sheet would be: rows that run their lines,
;;;;      rows that ask for the one line a pick cannot name. A shell gets
;;;;      none of them: its sheet opens.
;;;;   2. /import typed alone answers where to read from, on a shell too; a
;;;;      preview's rows land it, the held repositories' take one back.
;;;;   3. What the typed lines did that the sheets lacked is a gesture there:
;;;;      the roster's Right on a home, its rows past the homes, the import
;;;;      question's Replace and Only, the models list's Ctrl+R.
;;;;
;;;; Loads after bare-test: reuses WITH-FRAME-MODEL and DIALOG-KEYS.

(in-package #:nodecode.test)

(defun dialog-lines (dialog)
  "The line each of DIALOG's rows, its JSON, runs or asks for, in order; NIL
for a row that runs nothing."
  (map 'list (lambda (row)
               (or (nlk:json-value row :text "run" "enter")
                   (nlk:json-value row :text "ask" "line")))
       (nlk:json-array dialog "rows")))

;;; --- claim 1: without a shell, the sheet as a dialog -----------------------------

(deftest a-room-s-bare-models-is-the-sheet-s-three-tabs ()
  (multiple-value-bind (text dialog) (nle:slash "/models" :session-id nil)
    (is (search "Models" text))
    (is (equal '("main" "aux" "decider")
               (map 'list (lambda (tab) (nlk:json-value tab :string "value")) (nlk:json-array dialog "tabs"))))
    (let ((lines (dialog-lines dialog)))
      (is (member "/models" lines :test #'equal) "any other main model is asked for")
      (is (member "/models aux default" lines :test #'equal) "the auxiliary model given back")
      (is (member "/models aux" lines :test #'equal) "any other auxiliary model asked for")
      (is (member "/models decider none" lines :test #'equal) "no decider")
      (is (every (lambda (row) (member (nlk:json-value row :string "key") '("main" "aux" "decider") :test #'equal))
                 (coerce (nlk:json-array dialog "rows") 'list)))))
  (is (null (nle::bare-dialog "models" nil t)) "a shell opens its sheet"))

(deftest a-room-s-bare-usage-offers-the-four-ranges ()
  (multiple-value-bind (text dialog) (nle:slash "/usage")
    (is (search "Today" text))
    (is (equal '("/usage today" "/usage 7d" "/usage 30d" "/usage all") (dialog-lines dialog))))
  (is (null (nle::bare-dialog "usage" nil t)) "a shell opens its tabs"))

(deftest a-room-s-bare-cells-is-the-report-and-the-new-row ()
  (multiple-value-bind (text dialog) (nle:slash "/cells")
    (is (search "+ New cell" text))
    (let ((rows (coerce (nlk:json-array dialog "rows") 'list)))
      (is (= (+ 1 (length (nlk:cells)) (length (nlk:standing-notices))) (length rows)))
      (is (equal "/cells new" (car (last (dialog-lines dialog)))) "made by talking")))
  (is (search "Make a cell that" (nle:slash "/cells new")) "its line still says how")
  (is (null (nle::bare-dialog "cells" nil t)) "a shell opens its sheet"))

(deftest a-room-s-bare-profile-reaches-every-verb ()
  (nlk:profile-create "coder" :clone :blank)
  (multiple-value-bind (text dialog) (nle:slash "/profile")
    (is (search "coder" text))
    (is (equal '("/profile default" "/profile coder" "/profile create" "/profile create --blank"
                 "/profile create --clone-all" "/profile import" "/profile install")
               (dialog-lines dialog))))
  ;; A home's row opens what it takes; switching is a shell's.
  (multiple-value-bind (text dialog) (nle:slash "/profile coder")
    (is (search "coder" text))
    (is (equal '("/profile use coder" "/profile export coder" "/profile export coder --whole"
                 "/profile create --from coder" "/profile delete coder")
               (dialog-lines dialog))))
  (is (not (find "/profile delete default" (dialog-lines (nth-value 1 (nle:slash "/profile default"))) :test #'equal)))
  ;; A delete nobody said yes to asks, its one row the line that means it.
  (multiple-value-bind (text dialog) (nle:slash "/profile delete coder")
    (is (search "whole home" text))
    (is (not (search "--yes" text)) "the words name no line")
    (is (equal '("/profile delete coder --yes") (dialog-lines dialog))))
  (is (nlk:find-profile "coder") "asking deleted nothing")
  (nle:slash "/profile delete coder --yes")
  (is (null (nlk:find-profile "coder")) "the row's line does")
  (is (null (nle::bare-dialog "profile" nil t)) "a shell opens its roster"))

;;; --- claim 2: /import ------------------------------------------------------------

(deftest bare-import-asks-where-to-read-from ()
  (multiple-value-bind (text dialog) (nle:slash "/import")
    (is (search "Every agent found here" text))
    (let ((lines (dialog-lines dialog)))
      (is (equal "/import all" (first lines)) "every world, previewed")
      (is (member "/import --source" lines :test #'equal) "a folder or a repository is asked for")
      (is (equal "/import --sources" (car (last lines))) "and the repositories held")))
  (is (nle::bare-dialog "import" nil t) "a shell asks the same before its walk")
  (multiple-value-bind (text dialog) (nle:slash "/import --sources")
    (is (search "no repository source here" text))
    (is (not (search "OWNER/REPO" text)) "naming no line to type")
    (is (null dialog) "nothing to take back")))

(deftest a-shell-s-bare-import-opens-the-question-first ()
  (with-frame-model (model 100 30 :session-id "bare-import")
    (type-string model "/import" :enter)
    (is-present (dialog (nle::chat-model-dialog model)) "a picker opens, not the walk"
      (is (equal "Every agent found here" (nle::picker-row-label (first (nle::picker-dialog-rows dialog))))))
    (is (null (nle::chat-model-setup-walk model)) "nothing was read yet")))

;;; --- claim 3: the gestures the typed lines were ----------------------------------

(deftest the-roster-s-right-opens-what-else-a-home-takes ()
  (nlk:profile-create "coder" :clone :blank)
  (with-frame-model (model 100 30 :session-id "profile-actions")
    (nle::open-profiles-picker model :seat "coder")
    (is (search "→ more" (nle::picker-dialog-hint (nle::chat-model-dialog model))) "the hint says Right is there")
    (dialog-keys model :right)
    (let ((actions (nle::chat-model-dialog model)))
      (is (equal '(:switch :use :export :export-whole :from :delete)
                 (mapcar #'nle::picker-row-value (nle::picker-dialog-rows actions))))
      (dialog-keys model :down :enter)
      (is (search "coder boots by default" (nle::status-said model)) "Boot it by default ran `use'")
      (is (eq :profiles (nle::picker-dialog-kind (nle::chat-model-dialog model))) "and the roster is back"))
    (dialog-keys model :right)
    (is (eq :unuse (nle::picker-row-value (second (nle::picker-dialog-rows (nle::chat-model-dialog model))))))
    (dialog-keys model :escape)
    (is (search "Homes" (nle::picker-dialog-context (nle::chat-model-dialog model))) "Esc is the roster")))

(deftest the-roster-makes-an-empty-home-and-a-fork-from-its-rows ()
  (with-frame-model (model 100 30 :session-id "profile-kinds")
    (nle::open-profiles-picker model)
    (let ((dialog (nle::chat-model-dialog model)))
      (setf (nle::picker-dialog-selection dialog)
            (position '(:new :blank) (nle::picker-visible-rows dialog) :key #'nle::picker-row-value :test #'equal)))
    (apply #'dialog-keys model :enter (append (coerce "empty" 'list) '(:enter)))
    (is (nlk:find-profile "empty") "the empty home is made")
    (is (search "created empty" (nle::status-said model)))))

(deftest the-import-question-offers-replace-and-only ()
  (with-frame-model (model 100 30 :session-id "import-question")
    (let ((run (nle::make-setup-run))
          (value (nlk:json-object
                  "summary" (nlk:json-object "imported" 2 "skipped" 1 "conflicts" 0 "errors" 0)
                  "items" (vector (nlk:json-object "kind" "skills" "status" "imported")
                                  (nlk:json-object "kind" "memory" "status" "imported")
                                  (nlk:json-object "kind" "providers" "status" "skipped"))
                  "bots" #() "takeover" :null)))
      (nle::setup-import-confirm model run (list :import t) value (lambda (model) (declare (ignore model))))
      (let ((labels (mapcar #'nle::picker-row-label (nle::picker-dialog-rows (nle::chat-model-dialog model)))))
        (is (member "Replace what is here" labels :test #'equal) "what --overwrite did")
        (is (member "Only some of it…" labels :test #'equal) "what --only did"))
      (let ((dialog (nle::chat-model-dialog model)))
        (setf (nle::picker-dialog-selection dialog)
              (position :only (nle::picker-dialog-rows dialog) :key #'nle::picker-row-value)))
      (dialog-keys model :enter)
      (is (equal '("skills" "memory") (mapcar #'nle::picker-row-value (nle::picker-dialog-rows (nle::chat-model-dialog model)))))
      (is (eq :setup-import-confirm (nle::picker-dialog-kind (dialog-after model :escape))) "Esc is the question again"))))

(deftest ctrl-r-on-the-models-list-refetches-every-list ()
  (with-frame-model (model 100 30 :session-id "models-refetch")
    (nle::open-models-provider-picker model)
    (let ((dropped '()))
      (with-stubbed-fdefinitions ((nle::drop-provider-models-cache (ids) (setf dropped (list ids))))
        (is (nle::connect-dialog-key model (nle::chat-model-dialog model) #\r :ctrl t) "the key is the list's"))
      (is dropped "every fetched list forgotten")
      (is (search "fetching every provider's models again" (nle::status-said model))))))
