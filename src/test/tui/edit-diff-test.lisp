;;;; edit-diff-test.lisp --- what a call's edits changed shows as a diff, wherever the file is.
(in-package #:nodecode.test)

;;; --- the producer: EDIT and WRITE-FILE stamp their own diff -----------------

(defun numbered-lines (count &rest changes)
  "Lines `l1'..`lCOUNT', one per line, each a final newline; CHANGES is
LINE-NUMBER TEXT pairs written in their place."
  (format nil "~{~a~%~}"
          (loop for k from 1 to count
                collect (or (getf changes k) (format nil "l~d" k)))))

(deftest a-verb-diff-is-the-fewest-changed-lines-in-gits-hunks ()
  ;; Four lines of context, as the workspace bracket's `--unified=4', and
  ;; changes whose context meets share one hunk.
  (let ((file (nle::verb-diff-file "a.py" (numbered-lines 10) (numbered-lines 11 4 "FOUR" 11 "eleven"))))
    (is-shape file ("path" "a.py") ("added" 2) ("removed" 1))
    (is (equal (format nil "diff --git a/a.py b/a.py~%--- a/a.py~%+++ b/a.py~%@@ -1,10 +1,11 @@~%~{~a~%~}"
                       '(" l1" " l2" " l3" "-l4" "+FOUR" " l5" " l6" " l7" " l8" " l9" " l10" "+eleven"))
               (nlk:json-value file :string "patch"))))
  ;; Two changes far apart are two hunks, each numbered where it starts,
  ;; and the transcript separates them with its elision row.
  (let* ((file (nle::verb-diff-file "b.txt" (numbered-lines 30) (numbered-lines 30 2 "L2" 25 "L25")))
         (patch (nlk:json-value file :string "patch")))
    (is (search (format nil "@@ -1,6 +1,6 @@~%") patch))
    (is (search (format nil "@@ -21,9 +21,9 @@~%") patch))
    (is (equal '(:context :removed :added :context :context :context :context
                 :context :context :context :context :context :removed :added
                 :context :context :context :context)
               (mapcar #'nle::diff-line-sign (nle::parse-unified-patch patch))))
    (is (equal "..." (nle::diff-line-text (nth 7 (nle::parse-unified-patch patch))))))
  ;; A file the call made is all additions, from /dev/null.
  (let ((patch (nlk:json-value (nle::verb-diff-file "new.md" nil (format nil "a~%b~%")) :string "patch")))
    (is (search (format nil "--- /dev/null~%+++ b/new.md~%@@ -0,0 +1,2 @@~%+a~%+b~%") patch)))
  (is (null (nle::verb-diff-file "same.txt" (numbered-lines 3) (numbered-lines 3))) "no line changed, no diff"))

(deftest edits-outside-the-session-folder-carry-their-own-diff ()
  ;; The bracket walks the session's folder alone: an edit in another tree
  ;; reached the transcript as its receipt and no diff. The verb stamps it,
  ;; one file per file, from the text before the call first changed it.
  (with-temp-directory (dir "edit-diff")
    (let* ((path (merge-pathnames "notes.md" dir))
           (shown (nlk:home-abbreviated (uiop:native-namestring (truename dir))))
           (nle::*executing-tool-call-id* nil)
           (nle::*tool-result-metadata* nil))
      (nle::write-file path (format nil "# Notes~%~%## Unreleased~%~%## 0.1.0~%"))
      (setf nle::*executing-tool-call-id* "call-edit")
      (nle::edit path (format nil "## Unreleased~%") (format nil "## Unreleased~%- greet takes a person~%"))
      (nle::edit path "- greet takes a person" "- greet takes a person and punctuation")
      (let ((files (nlk:json-array nle::*tool-result-metadata* "verb_diffs")))
        (is (= 1 (length files)) "one file changed twice is one diff")
        (is-shape (aref files 0)
          ("path" (format nil "~a/notes.md" (string-right-trim "/" shown)))
          ("added" 1) ("removed" 0) ("verb" "edit"))
        (let ((patch (nlk:json-value (aref files 0) :string "patch")))
          (is (search (format nil " ## Unreleased~%+- greet takes a person and punctuation~% ~%") patch) "diffed from before the call, to the file as it ends")))
      ;; The bracket's own files stand; a verb's file it already names is not
      ;; listed twice, and the working key goes.
      (let ((metadata nle::*tool-result-metadata*)
            (verb-path (nlk:json-value (aref (nlk:json-array nle::*tool-result-metadata* "verb_diffs") 0) :text "path")))
        (setf (gethash "workspace_diff" metadata)
              (nlk:json-object "files" (vector (nlk:json-object "path" "greet.py" "added" 1 "removed" 1))))
        (nle::merge-verb-diffs metadata)
        (is (null (nth-value 1 (gethash "verb_diffs" metadata))))
        (is (equal (list "greet.py" verb-path)
                   (map 'list (lambda (file) (nlk:json-value file :text "path"))
                        (nlk:json-array metadata "workspace_diff" "files"))))))
    ;; Outside a tool call nothing is stamped.
    (let ((nle::*executing-tool-call-id* nil)
          (nle::*tool-result-metadata* nil))
      (nle::write-file (merge-pathnames "loose.txt" dir) "x")
      (is (null nle::*tool-result-metadata*)))))

(deftest a-verbs-own-diff-is-not-a-second-entry-of-its-call ()
  ;; The call's receipt already names the file it edited: the title says
  ;; `Edited ~/notes.md' once, not again for the diff's spelling of it.
  (let ((activity (nle::tool-call-activity
                   "eval" "(edit \"/home/someone/notes.md\" ($ \"old\") ($ \"new\"))"
                   :metadata (nlk:json-object
                              "calls" (vector (nlk:json-object "verb" "edit" "source" "/home/someone/notes.md"))
                              "workspace_diff" (nlk:json-object
                                                "files" (vector (nlk:json-object "path" "~/notes.md" "added" 1
                                                                                 "removed" 0 "verb" "edit")))))))
    (is (equal '((:edit "/home/someone/notes.md" nil)) (nle::activity-entries activity)))))

;;; --- the transcript: a run shows what its edits changed ---------------------

(defun edit-diff-metadata (path before after)
  "An edit call's result metadata: its receipt and the diff of PATH."
  (nlk:json-object "calls" (vector (nlk:json-object "verb" "edit" "source" path))
                   "workspace_diff" (nlk:json-object
                                     "files" (vector (nle::verb-diff-file path before after)))))

(defun edit-diff-state (&key (commands 1) (before (numbered-lines 6)) (after (numbered-lines 6 3 "THREE"))
                        (turn-id "edit-turn"))
  "A settled turn: COMMANDS plain calls, then one edit of greet.py from
BEFORE to AFTER, then its answer."
  (let ((state (nle::ingest-message-fact nil "user" "change greet" :turn-id turn-id)))
    (dotimes (i commands)
      (setf state (nle::ingest-tool-call-fact state (format nil "call-~d" i) "eval" "{\"form\":\"(sh \\\"ls\\\")\"}" :turn-id turn-id)
            state (nle::ingest-tool-result-fact state (format nil "call-~d" i) "eval" "listed" :turn-id turn-id)))
    (setf state (nle::ingest-tool-call-fact state "call-edit" "eval"
                                            "{\"form\":\";; greet takes a person\\n(edit \\\"greet.py\\\" ($ \\\"old\\\") ($ \\\"new\\\"))\"}"
                                            :turn-id turn-id)
          state (nle::ingest-tool-result-fact state "call-edit" "eval" "edited greet.py" :turn-id turn-id
                                              :metadata (edit-diff-metadata "greet.py" before after))
          state (nle::ingest-message-fact state "assistant" "done" :turn-id turn-id)
          state (nle::ingest-turn-terminal state "completed" :turn-id turn-id))
    state))

(defun edit-diff-lines (state)
  (mapcan (lambda (block) (copy-list (nle::display-block-plain-lines block)))
          (nle:project-state state :width 100)))

(deftest a-settled-run-shows-the-diffs-its-edits-made ()
  ;; A turn's run folded every call behind `Ran 2 commands', edits too: what
  ;; changed took three clicks to see and read as commands run. Shut, the
  ;; run now names its edits and lists the call that made them, each
  ;; changed file under it with its diff; a plain command stays folded.
  (let* ((nle::*tool-detail* :standard)
         (state (edit-diff-state))
         (lines (edit-diff-lines state)))
    (is (rows-carry-p "Ran 1 command, edited 1 file +1 -1" lines))
    (is (rows-carry-p "greet takes a person +1 -1" lines) "the edit's own line")
    (is (rows-carry-p "+1 -1 greet.py" lines) "its file's header")
    ;; The diff under its header, above the answer.
    (is (< (row-index-of "+1 -1 greet.py" lines :required t)
           (row-index-of "3 - l3" lines :required t)
           (row-index-of "3 + THREE" lines :required t)
           (row-index-of "done" lines :required t)))
    (is (not (rows-carry-p "listed" lines)) "a plain command stays folded")
    ;; A run of edits alone is worded as edits.
    (is (rows-carry-p "Edited 1 file +1 -1" (edit-diff-lines (edit-diff-state :commands 0))))
    ;; Opened, the run lists every call, the edit's diff still under it.
    (let* ((group (find-if #'nle::tool-group-p (mapcar #'nle::display-block-row (nle:project-state state :width 100))))
           (nle::*expanded-tool-call* (nle::tool-group-key group))
           (open (edit-diff-lines state)))
      (is (rows-carry-p "3 + THREE" open))
      (is (< (row-index-of "Ran 1 command, edited 1 file" open :required t)
             (row-index-of "3 + THREE" open :required t))))))

(deftest a-long-diff-shows-its-head-until-its-file-is-opened ()
  ;; A diff stands under its file's header to *DIFF-PREVIEW-LINES*, the rest
  ;; counted on a line that opens it, as the header does.
  (let* ((nle::*tool-detail* :standard)
         (state (edit-diff-state :commands 0 :before (numbered-lines 2)
                                 :after (numbered-lines 40 1 "first")))
         (lines (edit-diff-lines state))
         (diff (remove-if-not (lambda (line) (ppcre:scan "^ *[0-9]+ [-+ ] " line)) lines)))
    (is (= nle::*diff-preview-lines* (length diff)) "the head of the diff")
    (is (rows-carry-p (format nil "… +~d lines" (- 41 nle::*diff-preview-lines*)) lines) "the rest counted")
    (let* ((group (find-if #'nle::tool-group-p (mapcar #'nle::display-block-row (nle:project-state state :width 100))))
           (row (find "call-edit" (nle::tool-group-members group) :key #'nle::transcript-row-tool-call-id :test #'equal))
           (key (list :diff (nle::tool-row-expand-key row) "greet.py"))
           (more (find-if (lambda (line) (search "… +" (nle::lline-text line)))
                          (nle::workspace-diff-llines row))))
      (is (every (lambda (span) (equal key (nle::mspan-link span))) (nle::lline-spans more)) "the count opens the file, as its header does")
      (let* ((nle::*expanded-tool-call* (list :fold key))
             (open (edit-diff-lines state)))
        (is (rows-carry-p "40 + l40" open) "opened, the whole diff")
        (is (not (rows-carry-p "… +" open)))))))
