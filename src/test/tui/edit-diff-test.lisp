;;;; edit-diff-test.lisp --- a call's workspace-snapshot diffs show under it, unfolded.
(in-package #:nodecode.test)

;;; The workspace bracket diffs the session's folder around every call and
;;; stamps what changed on its result as workspace_diff. The transcript draws
;;; from that alone, whatever verb or command made the change.

(defun numbered-lines (count &rest changes)
  "Lines `l1'..`lCOUNT', one per line, each a final newline; CHANGES is
LINE-NUMBER TEXT pairs written in their place."
  (format nil "~{~a~%~}"
          (loop for k from 1 to count
                collect (or (getf changes k) (format nil "l~d" k)))))

(defun snapshot-diff-file (path hunk-lines)
  "A workspace_diff document file for PATH whose one hunk is HUNK-LINES,
each a line of the unified patch after its `@@' header (sign first)."
  (let ((old 0) (new 0) (added 0) (removed 0))
    (dolist (line hunk-lines)
      (case (char line 0)
        (#\Space (incf old) (incf new))
        (#\- (incf old) (incf removed))
        (#\+ (incf new) (incf added))))
    (nlk:json-object "path" path "added" added "removed" removed
                     "patch" (format nil "diff --git a/~a b/~a~%--- a/~a~%+++ b/~a~%@@ -1,~d +1,~d @@~%~{~a~%~}"
                                     path path path path old new hunk-lines))))

(defun edit-diff-state (&key (commands 1)
                             (hunk '(" l1" " l2" "-l3" "+THREE" " l4" " l5" " l6"))
                             (turn-id "edit-turn"))
  "A settled turn: COMMANDS plain calls, then one call whose snapshot
changed greet.py by HUNK, then its answer."
  (let ((state (nle::ingest-message-fact nil "user" "change greet" :turn-id turn-id)))
    (dotimes (i commands)
      (setf state (nle::ingest-tool-call-fact state (format nil "call-~d" i) "eval" "{\"form\":\"(sh \\\"ls\\\")\"}" :turn-id turn-id)
            state (nle::ingest-tool-result-fact state (format nil "call-~d" i) "eval" "listed" :turn-id turn-id)))
    (setf state (nle::ingest-tool-call-fact state "call-edit" "eval"
                                            "{\"form\":\";; greet takes a person\\n(edit \\\"greet.py\\\" ($ \\\"old\\\") ($ \\\"new\\\"))\"}"
                                            :turn-id turn-id)
          state (nle::ingest-tool-result-fact state "call-edit" "eval" "edited greet.py" :turn-id turn-id
                                              :metadata (nlk:json-object
                                                         "workspace_diff" (nlk:json-object
                                                                           "files" (vector (snapshot-diff-file "greet.py" hunk)))))
          state (nle::ingest-message-fact state "assistant" "done" :turn-id turn-id)
          state (nle::ingest-turn-terminal state "completed" :turn-id turn-id))
    state))

(defun edit-diff-lines (state)
  (mapcan (lambda (block) (copy-list (nle::display-block-plain-lines block)))
          (nle:project-state state :width 100)))

(deftest a-settled-run-shows-the-diffs-its-snapshots-took ()
  ;; A turn's run folded every call behind `Ran 2 commands', edits too: what
  ;; changed took three clicks to see and read as commands run. Shut, the
  ;; run now names the files its snapshots saw change and lists the call
  ;; that changed them, each file under it with its diff; a call that
  ;; changed nothing stays folded.
  (let* ((nle::*tool-detail* :standard)
         (state (edit-diff-state))
         (lines (edit-diff-lines state)))
    (is (rows-carry-p "Ran 1 command, edited 1 file +1 -1" lines))
    (is (rows-carry-p "greet takes a person +1 -1" lines) "the call's own line")
    (is (rows-carry-p "+1 -1 greet.py" lines) "its file's header")
    ;; The diff under its header, above the answer.
    (is (< (row-index-of "+1 -1 greet.py" lines :required t)
           (row-index-of "3 - l3" lines :required t)
           (row-index-of "3 + THREE" lines :required t)
           (row-index-of "done" lines :required t)))
    (is (not (rows-carry-p "listed" lines)) "a call that changed nothing stays folded")
    ;; A run whose every call changed files is worded as edits.
    (is (rows-carry-p "Edited 1 file +1 -1" (edit-diff-lines (edit-diff-state :commands 0))))
    ;; Opened, the run lists every call, the diff still under its call.
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
         (state (edit-diff-state :commands 0
                                 :hunk (append '("-l1" "+first" " l2")
                                               (loop for k from 3 to 40 collect (format nil "+l~d" k)))))
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

(defun edit-call-fact (state note paths &key (turn-id "edits-turn") (call t) (result t))
  "STATE after an Eval noted NOTE: its call when CALL, then its result when
RESULT, whose snapshot changed each of PATHS by one line."
  (when call
    (setf state (nle::ingest-tool-call-fact state note "eval"
                                            (format nil "{\"form\":\";; ~a\\n(sh \\\"true\\\")\"}" note)
                                            :turn-id turn-id)))
  (when result
    (setf state (nle::ingest-tool-result-fact
                 state note "eval" "ok" :turn-id turn-id
                 :metadata (and paths
                                (nlk:json-object
                                 "workspace_diff"
                                 (nlk:json-object "files" (map 'vector (lambda (path)
                                                                         (snapshot-diff-file path '(" l1" "-l2" "+TWO" " l3")))
                                                               paths)))))))
  state)

(defun edit-calls-state (calls &key (turn-id "edits-turn") (settled t))
  "A turn whose CALLS run in order, each (NOTE . PATHS) for EDIT-CALL-FACT;
then, when SETTLED, its answer and its end."
  (let ((state (nle::ingest-message-fact nil "user" "change things" :turn-id turn-id)))
    (loop for (note . paths) in calls
          do (setf state (edit-call-fact state note paths :turn-id turn-id)))
    (when settled
      (setf state (nle::ingest-message-fact state "assistant" "done" :turn-id turn-id)
            state (nle::ingest-turn-terminal state "completed" :turn-id turn-id)))
    state))

(deftest a-newer-edit-takes-the-place-of-the-diffs-before-it ()
  ;; Every call that changed files kept its diffs under the run, so a turn of
  ;; N edits stood N diff renders tall. The run's newest call that changed
  ;; files holds them now, every file it changed; an earlier one folds into
  ;; the count like a command, and its line still opens its own.
  (let* ((nle::*tool-detail* :standard)
         (state (edit-calls-state '(("retry" "app/webhooks.py") ("readme" "README.md")
                                    ("release" "pyproject.toml" "app/__init__.py") ("check"))))
         (lines (edit-diff-lines state)))
    (is (rows-carry-p "Ran 1 command, edited 4 files +4 -4" lines) "the title still counts every edit")
    (is (rows-carry-p "release +2 -2" lines) "the newest edit's own line")
    (is (rows-carry-p "+1 -1 pyproject.toml" lines) "its first file")
    (is (rows-carry-p "+1 -1 app/__init__.py" lines) "and its second: a call keeps all its diffs")
    (is (= 2 (count-if (lambda (line) (search "2 + TWO" line)) lines)) "both files' diffs, nobody else's")
    (is (notany (lambda (needle) (rows-carry-p needle lines)) '("webhooks.py" "README.md" "retry" "readme")) "the earlier edits fold, diffs and lines")
    (is (not (rows-carry-p "check" lines)) "a call that changed nothing stays folded")
    (let* ((group (find-if #'nle::tool-group-p (mapcar #'nle::display-block-row (nle:project-state state :width 100))))
           (retry (find "retry" (nle::tool-group-members group) :key #'nle::transcript-row-tool-call-id :test #'equal))
           (open (let ((nle::*expanded-tool-call* (nle::tool-group-key group))) (edit-diff-lines state)))
           (opened (let ((nle::*expanded-tool-call* (nle::tool-row-expand-key retry))) (edit-diff-lines state))))
      ;; Opened, the run lists every call; the diffs stand under the newest edit alone.
      (is (every (lambda (needle) (rows-carry-p needle open)) '("retry +1 -1" "readme +1 -1" "release +2 -2" "check")) "every call listed")
      (is (not (rows-carry-p "webhooks.py" open)) "an earlier edit lists its line, not its diffs")
      (is (rows-carry-p "+1 -1 app/__init__.py" open) "the newest edit's diffs")
      ;; An earlier edit's line opens its own diffs; the newest's stay.
      (is (rows-carry-p "+1 -1 app/webhooks.py" opened) "an edit clicked open shows its diffs")
      (is (rows-carry-p "+1 -1 app/__init__.py" opened) "beside the newest's"))))

(deftest a-live-run-hands-the-diffs-to-each-newer-edit ()
  ;; Live, the diffs move as edits land: an edit's stand under it while the
  ;; next call runs, a later call that changed files takes their place, and a
  ;; call that changed nothing leaves them where they are.
  (let* ((nle::*tool-detail* :standard)
         (state (edit-call-fact (edit-calls-state '(("retry" "app/webhooks.py")) :settled nil) "readme" nil :result nil)))
    (is (rows-carry-p "+1 -1 app/webhooks.py" (edit-diff-lines state)) "the last edit's diffs while the next call runs")
    (setf state (edit-call-fact state "readme" '("README.md") :call nil))
    (let ((lines (edit-diff-lines state)))
      (is (rows-carry-p "+1 -1 README.md" lines) "the newer edit's diffs")
      (is (not (rows-carry-p "webhooks.py" lines)) "in the place of the older one's"))
    (setf state (edit-call-fact state "check" nil))
    (let ((lines (edit-diff-lines state)))
      (is (rows-carry-p "+1 -1 README.md" lines) "a call that changed nothing leaves them")
      (is (not (rows-carry-p "webhooks.py" lines)) "and brings nothing back"))))
