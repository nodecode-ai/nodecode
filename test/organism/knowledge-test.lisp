;;;; knowledge-test.lisp --- memories and skills as definitions; the one index.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: a memory and a prose skill are definitions the
;;;; scribe files in a knowledge cell beside the layer -- one file, one
;;;; commit, a ledger line -- and a code skill is the layer's; a name the
;;;; image uses is refused; the forgetting verbs take them out, and a name has
;;;; one home; the scan reads files as data; the loader records a knowledge
;;;; cell without loading it and the launch key ignores its files; the index
;;;; ranks by kind -- a memory never cools, a skill does -- and lays out within
;;;; its budget; an import ranks after the organism's own, its rules cold,
;;;; until the organism's own use vouches for it; a project's definition reads only under its root; help reads
;;;; a kept one and notes a view; search finds hot and cold; the pin holds
;;;; through a keep and is taken again when marked or evicted; /index answers
;;;; its meter and its picker; a settings clone links the cell (profile-test's
;;;; CREATE-SHARES-WHAT-BELONGS-TO-THE-OPERATOR-RATHER-THAN-THE-HOME); patch
;;;; refuses prose.

(in-package #:nodecode.test)

(defun knowledge-git (cell &rest args)
  "git ARGS in the fixture's CELL: what it printed."
  (values (apply #'nlk::git-in-layer (nlk::folder-under cell nlk::*cells-directory*) args)))

(defun ledger-kinds (name)
  "The kinds of NAME's ledger lines, oldest first."
  (loop for line in (nlk:read-json-lines (nlk:home "usage.jsonl"))
        when (equal name (nlk:json-value line :string "name"))
          collect (nlk:json-value line :string "kind")))

(defun restart-ledger (&rest lines)
  "The ledger made of LINES alone, each (NAME KIND &key session at)."
  (uiop:delete-file-if-exists (nlk:home "usage.jsonl"))
  (loop for (name kind . keys) in lines
        do (apply #'nlk:note-knowledge-use name kind keys)))

(defun days-ago (days)
  (nlk:iso-time (- (nlk:unix-now) (* days 86400))))

(defparameter +settings+ '(:hot-budget 8192 :stale-days 30))

(deftest knowledge-the-ledger-ranks-by-running-totals-each-line-folded-once (with-knowledge-fixture ())
  ;; Each line most of the fold's 256-byte head, so two ledgers can open on
  ;; the same bytes and differ only past them.
  (let ((session (format nil "s-~a" (make-string 120 :initial-element #\a)))
        (at (days-ago 2))
        (old (days-ago 31)))
    (restart-ledger (list "kn-fold" "keep" :session session :at at)
                    (list "kn-fold" "view" :session session :at old)
                    (list "kn-fold" "view" :session session :at at))
    (is (equal 2 (first (gethash "kn-fold" (nlk:knowledge-stats :stale-days 30)))) "a line past stale_days adds nothing")
    (nlk:note-knowledge-use "kn-fold" "helped" :session "s-b")
    (nlk:note-knowledge-use "kn-new" "view" :session "experience-x")
    (let ((stats (nlk:knowledge-stats :stale-days 30)))
      (is (equal 3 (first (gethash "kn-fold" stats))) "a line appended after a fold is folded in")
      (is (equal 1/10 (first (gethash "kn-new" stats))) "and a name it brings")
      (is (eq t (third (gethash "kn-fold" stats))) "its own keep vouches"))
    ;; Another ledger under the same name: its first two lines the old one's
    ;; bytes, then other lines, longer than what was folded.
    (restart-ledger (list "kn-fold" "keep" :session session :at at)
                    (list "kn-fold" "view" :session session :at old)
                    (list "kn-other" "harm" :session session :at at)
                    (list "kn-other" "harm" :session session :at at)
                    (list "kn-other" "harm" :session session :at at))
    (let ((stats (nlk:knowledge-stats :stale-days 30)))
      (is (equal 1 (first (gethash "kn-fold" stats))) "a rewritten ledger is folded again from its first line")
      (is (equal -3 (first (gethash "kn-other" stats))))
      (is (null (gethash "kn-new" stats)) "nothing of the old ledger stands"))))

(defparameter +kn-memory+ "(define-memory kn-absent
  \"The operator reads an absent measurement as absent, never as zero.\"
  :type :feedback
  ;; Corrected live on /context: it printed zero for a lane with turns.
  ;; Say `unknown' instead.
  )")

;;; --- keeping ---------------------------------------------------------------------

(deftest knowledge-a-memory-is-one-committed-file-in-the-knowledge-cell (with-knowledge-fixture ())
  (is (search "[memory: kept kn-absent]" (keep-definition-text +kn-memory+ :session "s-k")))
  (is-present (file (kept-text "kn-absent")) "the memory's own file in tester-memory"
    (is (search +kn-memory+ file) "the model's own bytes, prose and all")
    (is (search "(in-package :nodecode.evolved)" file)))
  (is (nlk:knowledge-folder-p (merge-pathnames "tester-memory/" nlk::*cells-directory*)))
  (is (null (search "kn-absent" (nlk:read-text (merge-pathnames "tester-memory/tester-memory.asd"
                                                                 nlk::*cells-directory*)))))
  (is (search "kn-absent — session s-k" (knowledge-git "tester-memory" "log" "--format=%s" "-1")))
  (is (string= "" (knowledge-git "tester-memory" "status" "--porcelain")) "committed whole")
  (is (null (uiop:directory-exists-p (nlk:layer-directory))) "and nothing in the layer")
  (is (equal '("keep") (ledger-kinds "kn-absent")) "a keep line in the ledger"))

(deftest knowledge-a-prose-skill-is-the-cells-and-a-code-skill-the-layers (with-knowledge-fixture ())
  (keep-definition-text "(define-skill kn-split \"Split red tests from a shared baseline.\"
  ;; 1. Run the suite on your tree.
  )")
  (is (kept-text "kn-split") "prose is knowledge")
  (is (search "[layer: kept kn-double]"
              (keep-definition-text "(define-skill kn-double (x) \"Twice X.\" (* 2 x))")))
  (is (nlk:read-text (merge-pathnames "kn-double.lisp" (nlk:layer-directory))) "code is the layer's")
  (is (null (kept-text "kn-double")))
  (is (= 4 (funcall (find-symbol "KN-DOUBLE" '#:nodecode.evolved) 2)) "and runs"))

(deftest knowledge-cell-files-a-topic-cell-and-refuses-a-cell-of-code (with-knowledge-fixture ())
  (keep-definition-text "(define-memory kn-lab \"The lab box is reached over SSH.\" :type :reference :cell \"lab\")")
  (is (kept-text "kn-lab" "lab") "the topic cell holds it")
  (is (nlk:knowledge-folder-p (merge-pathnames "lab/" nlk::*cells-directory*)))
  (keep-definition-text "(define-memory kn-lab \"The lab box is reached over SSH only.\" :type :reference)")
  (is (search "SSH only" (kept-text "kn-lab" "lab")) "kept again where it lives")
  (is (null (kept-text "kn-lab")) "not a second copy in the organism's own cell")
  (write-temp-file (merge-pathnames "guard/nodecode-guard.asd" nlk::*cells-directory*)
                   "(defsystem \"nodecode-guard\")")
  (is (search "a cell of code" (keep-definition-text "(define-memory kn-guard \"A guard fact.\" :cell \"guard\")")))
  (is (null (kept-text "kn-guard" "guard")) "nothing is written into it"))

(deftest knowledge-a-name-the-image-uses-is-refused (with-knowledge-fixture ())
  (flet ((refusal (form) (refusal-text error (macroexpand-1 form))))
    (is (search "already names a function" (refusal '(nle:define-memory nle:edit "A fact."))))
    (is (search "already names a variable" (refusal '(nle:define-memory nle:*system-prompt* "A fact."))))
    (is (search "locked package" (refusal '(nle:define-memory otherwise "A fact."))))
    (is (search ":type is one of" (refusal '(nle:define-memory nle::kn-x "A fact." :type :whim))))
    (is (search "a skill has no :type" (refusal '(nle:define-skill nle::kn-x "A step." :type :lesson))))
    (is (search "none of them" (refusal '(nle:define-memory nle::kn-x "A fact." :colour :red))))
    (is (search ":project is the repository's root" (refusal '(nle:define-memory nle::kn-x "A fact." :project t)))))
  ;; a code skill is fbound by its own definition, and keeps again
  (keep-definition-text "(define-skill kn-twice (x) \"Twice.\" (* 2 x))")
  (is (search "[layer: kept kn-twice]"
              (keep-definition-text "(define-skill kn-twice (x) \"Twice, again.\" (+ x x))"))))

(deftest knowledge-forgetting-takes-a-memory-and-a-code-skill-out (with-knowledge-fixture ())
  (keep-definition-text +kn-memory+)
  (keep-definition-text "(define-skill kn-thrice (x) \"Thrice.\" (* 3 x))")
  (is (search "[memory: forgot kn-absent]" (keep-definition-text "(unintern 'kn-absent)")))
  (is (null (kept-text "kn-absent")) "the file is gone")
  (is (search "forgot kn-absent" (knowledge-git "tester-memory" "log" "--format=%s" "-1")))
  (is (search "[layer: forgot kn-thrice]" (keep-definition-text "(fmakunbound 'kn-thrice)")))
  (is (null (probe-file (merge-pathnames "kn-thrice.lisp" (nlk:layer-directory)))))
  (is (null (search "kn-thrice" (nlk:read-text (merge-pathnames "tester.asd" (nlk:layer-directory))))))
  (is (equal '("keep" "forget") (ledger-kinds "kn-absent"))))

(deftest knowledge-a-name-has-one-home (with-knowledge-fixture ())
  (keep-definition-text "(define-skill kn-move \"Move it.\"
  ;; by hand
  )")
  (let ((said (keep-definition-text "(define-skill kn-move () \"Move it, as code.\" :moved)")))
    (is (search "[layer: kept kn-move]" said))
    (is (search "[memory: moved kn-move to layer]" said) "the prose leaves its cell"))
  (is (null (kept-text "kn-move"))))

;;; --- the scan ----------------------------------------------------------------------

(deftest knowledge-the-scan-reads-files-as-data-and-again-when-they-change (with-knowledge-fixture ())
  (keep-definition-text +kn-memory+)
  (keep-definition-text "(defun kn-probe () 1)")
  (keep-definition-text "(defun kn-helper () \"Help with a thing.\" 2)")
  (write-temp-file (merge-pathnames "tester-memory/kn-unread.lisp" nlk::*cells-directory*)
                   "(in-package :nodecode.evolved)
(define-memory kn-unread \"Written by hand.\" :sources (kn-unseen-symbol))")
  (is (member "kn-probe" (mapcar #'nlk:knowledge-entry-name (nlk:knowledge-entries)) :test #'string=))
  (let ((visible (mapcar #'nlk:knowledge-entry-name (nlk::visible-entries nil))))
    (is (null (member "kn-probe" visible :test #'string=)) "an undocumented one is not listed")
    (is (member "kn-helper" visible :test #'string=))
    (is (member "kn-unread" visible :test #'string=) "a file written by hand reads"))
  (is (null (find-symbol "KN-UNSEEN-SYMBOL" '#:nodecode.evolved)) "and interns nothing where it lives")
  ;; kept again inside the same second: the next read sees the new text
  (keep-definition-text (ppcre:regex-replace "never as zero" +kn-memory+ "never as nought"))
  (is (search "nought" (nlk:knowledge-entry-description (nlk:knowledge-entry-named "kn-absent" nil)))))

;;; --- hot and cold ----------------------------------------------------------------------

(deftest knowledge-a-memory-never-cools-and-a-skill-does (with-knowledge-fixture ())
  (keep-definition-text "(define-memory kn-old \"An old fact.\" :type :lesson)")
  (keep-definition-text "(define-skill kn-stale \"A stale procedure.\")")
  (keep-definition-text "(define-skill kn-fresh \"A fresh procedure.\")")
  (restart-ledger (list "kn-old" "keep" :at (days-ago 40))
                  (list "kn-stale" "keep" :at (days-ago 40))
                  (list "kn-fresh" "keep"))
  (multiple-value-bind (hot cold) (nlk::ranked-knowledge nil +settings+)
    (is (equal '("kn-fresh" "kn-old") (mapcar #'nlk:knowledge-entry-name hot)))
    (is (equal '("kn-stale") (mapcar #'nlk:knowledge-entry-name cold)) "a skill idle past stale_days does")))

(deftest knowledge-rank-is-evidence-then-the-operators-word (with-knowledge-fixture ())
  (dolist (name '("kn-task" "kn-reviewed" "kn-harmed" "kn-lesson" "kn-feedback"))
    (keep-definition-text (format nil "(define-memory ~a \"Fact ~a.\" :type ~a)"
                                  name name (if (string= name "kn-feedback") ":feedback" ":lesson"))))
  (apply #'restart-ledger
         (list "kn-task" "view" :session "s-task")
         (list "kn-harmed" "view" :session "s-task")
         (list "kn-harmed" "harm" :session "experience-s-task-1")
         (loop repeat 5 collect (list "kn-reviewed" "view" :session "experience-s-task-1")))
  (is (equal '("kn-task" "kn-reviewed" "kn-feedback")
             (subseq (mapcar #'nlk:knowledge-entry-name (nlk::ranked-knowledge nil +settings+)) 0 3))))

(deftest knowledge-an-import-ranks-after-the-organisms-own-until-vouched-for (with-knowledge-fixture ())
  ;; Fresh, every one: the organism's own first whatever the import's score, an
  ;; import's rule of the operator's word cold, its fact after. The import's
  ;; seeded keep and a sighting brought from its home's ledger vouch for
  ;; nothing; a helped sighting of the organism's own does, and the index then
  ;; heads it as the organism's own.
  (keep-definition-text "(define-memory kn-own-fact \"Own fact.\" :type :project)")
  (keep-definition-text "(define-memory kn-own-rule \"Own rule.\" :type :feedback)")
  (keep-definition-text "(define-memory kn-in-fact \"Imported fact.\" :type :project :origin \"hermes\")")
  (keep-definition-text "(define-memory kn-in-rule \"Imported rule.\" :type :feedback :origin \"hermes\")")
  (keep-definition-text "(define-memory kn-in-used \"Imported, then used.\" :type :user :origin \"hermes\")")
  (apply #'restart-ledger
         (list "kn-in-fact" "keep" :session (nlk:import-session '("hermes")))
         (list "kn-in-rule" "keep" :session (nlk:import-session '("hermes")))
         (list "kn-in-rule" "helped" :session "s-there" :extra '("imported" "hermes"))
         (list "kn-in-used" "keep" :session (nlk:import-session '("hermes")))
         (loop repeat 3 collect (list "kn-in-fact" "view" :session "s-task")))
  (flet ((names (entries) (mapcar #'nlk:knowledge-entry-name entries)))
    (multiple-value-bind (hot cold) (nlk::ranked-knowledge nil +settings+)
      (is (equal '("kn-own-rule" "kn-own-fact" "kn-in-fact") (names hot)) "its own first; the import's fact scores 4")
      (is (equal '("kn-in-rule" "kn-in-used") (names cold)) "an import's rule is not the operator's word"))
    (let ((text (nlk:knowledge-surface nil +settings+)))
      (is (search (format nil "~%[memory]~%kn-own-rule — Own rule.~%kn-own-fact — Own fact.~%[memory from hermes]~%kn-in-fact — Imported fact.") text)))
    ;; 60 characters: one described line (24) within half, the three names (35
    ;; with also:) after it, two described lines (48) and the last name not.
    (multiple-value-bind (described named) (nlk::knowledge-tiers (nlk::ranked-knowledge nil +settings+) 60)
      (is (equal '("kn-own-rule") (names described)) "a tight budget describes its own first")
      (is (equal '("kn-own-fact" "kn-in-fact") (names named))))
    (nlk:note-knowledge-use "kn-in-used" "helped" :session "s-task")
    (is (equal '("kn-in-used" "kn-own-rule" "kn-own-fact" "kn-in-fact") (names (nlk::ranked-knowledge nil +settings+))) "vouched for, it is its own")
    (is (search (format nil "~%[memory]~%kn-in-used — Imported, then used.~%kn-own-rule")
                (nlk:knowledge-surface nil +settings+)))))

(deftest knowledge-tiers-describe-again-what-the-budget-still-holds ()
  ;; The skills index's measure: four described lines of 27 chars (108) over
  ;; a budget of 100 describe one within half of it, name the rest, then
  ;; describe again while the whole holds -- three.
  (let ((entries (loop for name in '("s-a" "s-b" "s-c" "s-d")
                       collect (nlk::make-knowledge-entry :name name :description "twenty characters ok"))))
    (flet ((tiers (budget)
             (multiple-value-bind (described named) (nlk::knowledge-tiers entries budget)
               (list (length described) (length named)))))
      (is-table (in want) (equal want (tiers in)) (100 '(3 1)) (80 '(2 2))
        (108 '(4 0) "all four fit described")))))

(deftest knowledge-the-index-groups-by-cell-and-counts-the-rest (with-knowledge-fixture ())
  (keep-definition-text +kn-memory+)
  (keep-definition-text "(define-memory kn-tail \"Lab runs remote.\" :type :reference :cell \"lab\")")
  (keep-definition-text "(defun kn-tool () \"Run the tool.\" 3)")
  (let ((text (nlk:knowledge-surface nil +settings+)))
    (is (uiop:string-prefix-p "(help 'name) reads one before you follow it" text))
    (is (search (format nil "~%[memory]~%kn-absent — The operator reads an absent measurement as absent, never as zero.") text))
    (is (search (format nil "~%[lab]~%kn-tail — Lab runs remote.") text))
    (is (search (format nil "~%[tester]~%kn-tool — Run the tool.") text) "the layer's own, by its name")
    (is (null (search "finds" text)) "nothing left out to count"))
  (let ((text (nlk:knowledge-surface nil '(:hot-budget 0 :stale-days 30))))
    (is (search "(index \"text\") finds 3 more" text) "what does not fit is counted")
    (is (null (search "[memory]" text)))))

(deftest knowledge-a-projects-definition-reads-only-under-its-root (with-knowledge-fixture ())
  (with-temp-store ()
    (with-temp-workspace (project ".git/HEAD")
      (nlk:create-session :id "s-in" :cwd (namestring project))
      (nlk:create-session :id "s-out" :cwd (namestring nlk::*cells-directory*))
      (keep-definition-text (format nil "(define-memory kn-here \"This repository builds with just.\" :type :project :project ~s)"
                                    (string-right-trim "/" (namestring project))))
      (is (search "kn-here" (nlk:knowledge-surface "s-in" +settings+)) "listed under its root")
      (is (equal "" (nlk:knowledge-surface "s-out" +settings+)) "elsewhere neither listed nor counted")
      (is (null (nlk:knowledge-entry-named "kn-here" "s-out")) "nor found"))))

;;; --- reading ----------------------------------------------------------------------------

(deftest knowledge-help-reads-a-kept-memory-from-its-file (with-knowledge-fixture ())
  (keep-definition-text +kn-memory+)
  (let ((text (let ((nlk:*scribe-session-id* "s-reader")) (nle:help "kn-absent"))))
    (is (uiop:string-prefix-p "kn-absent — memory, feedback, in " text))
    (is (search (format nil "~%The operator reads an absent measurement as absent, never as zero.~%") text))
    (is (search (format nil "~%Corrected live on /context: it printed zero for a lane with turns.~%Say `unknown' instead.") text)))
  (is (equal '("keep" "view") (ledger-kinds "kn-absent")) "a read is a view in the ledger"))

(deftest knowledge-search-finds-by-name-description-and-text (with-knowledge-fixture ())
  (keep-definition-text +kn-memory+)
  (keep-definition-text "(define-skill kn-cold-one \"An old procedure.\"
  ;; caps the cards at 590W
  )")
  (restart-ledger (list "kn-cold-one" "keep" :at (days-ago 40)))
  (is (search "kn-absent [memory] — The operator" (nlk:knowledge-search "absent" nil +settings+)) "by name")
  (is (search "kn-absent" (nlk:knowledge-search "measurement" nil +settings+)) "by description")
  (is (search "kn-cold-one [memory, cold]" (nlk:knowledge-search "590W" nil +settings+)) "by text, cold too")
  (is (search "nothing kept mentions" (nlk:knowledge-search "zebra" nil +settings+))))

;;; --- the pin ------------------------------------------------------------------------------

(deftest knowledge-the-index-pin-holds-through-a-keep-and-is-taken-again-when-marked (with-knowledge-fixture ())
  (with-session-store ("s1")
    (keep-definition-text +kn-memory+)
    (nle::read-harness-sections "s1")
    (let ((pinned (nlk:get-harness-section "s1" "index")))
      (is (search "kn-absent" pinned) "pinned at the first request")
      (keep-definition-text "(define-memory kn-later \"Kept after the pin.\")" :session "s1")
      (is (equal pinned (cdr (assoc "index" (nle::read-harness-sections "s1") :test #'string=))))
      (nlk:mark-index-refresh "s1")
      (nle::read-harness-sections "s1")
      (is (search "kn-later" (nlk:get-harness-section "s1" "index")) "marked, the next request pins it again")
      (let ((version (nth-value 1 (nlk:get-harness-section "s1" "index"))))
        (nlk:mark-index-refresh "s1")
        (nle::read-harness-sections "s1")
        (is (= version (nth-value 1 (nlk:get-harness-section "s1" "index"))) "an unchanged text writes nothing")))))

(deftest knowledge-two-conversations-of-a-project-share-the-head-through-its-instructions (with-knowledge-fixture ())
  (with-temp-directory (project "shared-prefix")
    (with-open-file (out (merge-pathnames "AGENTS.md" project) :direction :output)
      (write-string "PROJECT RULES" out))
    (with-temp-store ()
      (nlk:create-session :id "first" :cwd (namestring project))
      (keep-definition-text +kn-memory+)
      (let ((one (nle::read-harness-sections "first")))
        (keep-definition-text "(define-memory kn-between \"Kept between two conversations.\")")
        (nlk:create-session :id "second" :cwd (namestring project))
        (let* ((two (nle::read-harness-sections "second"))
               (a (nle::message-content (nth-value 1 (nle::prompt-messages "BASE" one))))
               (b (nle::message-content (nth-value 1 (nle::prompt-messages "BASE" two))))
               (rules-end (+ (search "PROJECT RULES" a) (length "PROJECT RULES"))))
          (is (not (equal (cdr (assoc "index" one :test #'string=)) (cdr (assoc "index" two :test #'string=)))) "the two indexes differ")
          (is (equal "index" (car (car (last one)))) "the index rides behind every other section")
          (is (< (search "PROJECT RULES" a) (search "kn-absent" a)) "the instructions ride ahead of it")
          (is (>= (mismatch a b) rules-end) "the two heads match byte for byte through the instructions"))))))

(deftest knowledge-an-advancing-floor-marks-the-index (with-temp-store ())
  (seed-user-inputs "room-k" '("one" "two" "three"))
  (let ((turns (nle::history-turns-from-events (nle::collect-session-events "room-k"))))
    (nlk:with-cleanup ((remhash "room-k" nlk::*index-refresh*))
      (nle::persist-history-boundary "room-k" turns 2 2 :reason "test")
      (is (null (gethash "room-k" nlk::*index-refresh*)) "an unmoved floor marks nothing")
      (nle::persist-history-boundary "room-k" turns 0 2 :reason "test")
      (is (gethash "room-k" nlk::*index-refresh*) "an advance marks the conversation"))))

(deftest knowledge-index-command-answers-the-meter-and-a-picker (with-knowledge-fixture ())
  (with-session-store ("s-ix")
    (keep-definition-text +kn-memory+)
    (multiple-value-bind (text dialog) (nle::index-slash "" "s-ix")
      (is (equal "1 hot · 0 cold · 1 cell" text))
      (is (equal "Index" (getf dialog :title)))
      (is-present (row (first (getf dialog :rows))) "a row per definition"
        (is (equal "$kn-absent " (getf row :value)) "Enter inserts $name")
        (is (equal "memory" (getf row :key)) "its cell is its tab")))
    (is (search "pinned again" (nle::index-slash "reload" "s-ix")))
    (is (gethash "s-ix" nlk::*index-refresh*))
    (remhash "s-ix" nlk::*index-refresh*)
    (is (search "kn-absent [memory]" (nle::index-slash "absent" "s-ix")))))

;;; --- the folder ------------------------------------------------------------------------------

(deftest knowledge-the-loader-records-a-knowledge-cell-and-never-loads-it (with-knowledge-fixture ())
  (keep-definition-text +kn-memory+)
  (keep-definition-text "(defun kn-layered () \"Layered.\" 5)")
  (let ((key (nlk::folder-key)))
    (keep-definition-text "(define-memory kn-second \"A second fact.\")")
    (is (equal key (nlk::folder-key)) "a keep in a knowledge cell moves no launch key")
    (keep-definition-text "(defun kn-layered () \"Layered again.\" 6)")
    (is (not (equal key (nlk::folder-key))) "a layer file does"))
  (is-present (cell (find "tester-memory" (load-cell-fixture nlk::*cells-directory*)
                          :key #'nlk:cell-name :test #'string=)) "on the report"
    (is (eq :knowledge (nlk:cell-kind cell)))
    (is (search "cell tester-memory: knowledge, 2 definitions"
                (nlk:cells-folder-text '("tester-memory"))))))

(deftest knowledge-patch-refuses-a-definition-carrying-prose (with-knowledge-fixture ())
  (keep-definition-text "(defun kn-patched () ;; why it is one
  1)")
  (is (search "carries ;; prose" (handler-case (progn (nle::patch-definition (find-symbol "KN-PATCHED" '#:nodecode.evolved) 1 2) "")
                                    (error (e) (princ-to-string e)))))
  (keep-definition-text "(defun kn-plain () 1)")
  (nle::patch-definition (find-symbol "KN-PLAIN" '#:nodecode.evolved) 1 2)
  (is (= 2 (funcall (find-symbol "KN-PLAIN" '#:nodecode.evolved))) "a definition with none patches"))
