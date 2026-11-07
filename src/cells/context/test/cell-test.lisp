;;;; cell-test.lisp --- full skill survives the real prompt/history projection.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(define-test-slice "context-cell" "CONTEXT-CELL-" :recipe "context")

(defun context-cell-install ()
  ;; Tests reset hooks; install exactly the shipped cell's contribution.
  (nle:hook 'nle::read-harness-sections "nodecode-context" #'nodecode-context::standing-skill))

(defun context-cell-request (id)
  (multiple-value-bind (base sections tools)
      (nle::turn-prompt id (nle::snapshot-tool-bindings))
    (let ((turns (nle::history-turns-for-session (nle:intern-session id))))
      (nodecode.working-context-proof::context-text
       (nle::compile-turn-context
        (nle::make-context-request
         :session-id id
         :provider-config (nle::make-effective-provider-config
                           :provider "fixture" :lane "openai-completions"
                           :endpoint "http://127.0.0.1:1" :api-key "inert"
                           :model "fixture" :context-tokens 100000)
         :base-system-prompt base :harness-sections sections :tools tools
         :history-turns turns
         :history-boundary (nle::session-floor-boundary id turns)
         :history-floors (nlk:retained-history-floors id)))))))

(defun context-cell-once (id)
  (= 1 (nodecode.working-context-proof::occurrences
        nodecode-context::*skill* (context-cell-request id))))

(deftest context-cell-small-full-text-independent-of-index ()
  (context-cell-install)
  (context-cell-install)
  (let ((sections (nle::kept-sections (nle::read-harness-sections nil))))
    (is (= 1 (count "context" sections :key #'car :test #'equal)) "loading twice installs one section")
    (is (equal nodecode-context::*skill* (cdr (assoc "context" sections :test #'equal))) "full skill, not just its name, is always supplied")
    (is (<= 1 (length nodecode-context::*skill*) 800) "the hot instruction stays small")
    (is (search "During substantial tool work" nodecode-context::*skill*) "review also happens during the turn")
    (is (search "at useful checkpoints" nodecode-context::*skill*) "cleanup follows useful checkpoints, not a fixed schedule")
    (is (search "after settling a subproblem" nodecode-context::*skill*) "a settled subproblem is a concrete checkpoint")
    (is (search "edit your context at useful checkpoints" nodecode-context::*skill*) "a checkpoint is an edit, not a look")
    (is (search "a level cleared" nodecode-context::*skill*) "a settle point is named concretely, as ARC-AGI-3's level clear was")
    (is (search "before your final answer" nodecode-context::*skill*) "pre-final review remains")
    (is (search "not after every tool call" nodecode-context::*skill*) "avoid a per-tool editing ritual")
    (is (search "Window headroom is not the test" nodecode-context::*skill*) "a large window never excuses keeping settled blocks")
    (is (search "drop settled ones you will not reread" nodecode-context::*skill*) "drops follow reread need, no deletion quota")
    (is (search "a drop is lossless, (recall-entry 57)" nodecode-context::*skill*) "and says how a dropped block comes back")
    (is (search "(context :drop '(12 . 56) :note" nodecode-context::*skill*) "the skill says how: one call, named by seq")
    (is (search "[seq 57]" nodecode-context::*skill*) "and where the seq is read")
    (is (search "without exporting, reading or printing anything" nodecode-context::*skill*) "no export, read or print round")
    (is (search "not while a subproblem is still open" nodecode-context::*skill*) "no wipe in the middle of open work")))

(deftest context-cell-requests-survive-edits-eviction-and-forks ()
  (context-cell-install)
  (nodecode.working-context-proof::with-database
   (lambda ()
     (nlk:create-session :id "context-cell-parent" :cwd "/tmp/")
     (nodecode.working-context-proof::enter "context-cell-parent" "OLD QUESTION")
     (nlk:record-assistant-message nle::*current-durable-turn*
                                   (nle::message "assistant" "DISPOSABLE OLD OUTPUT"))
     (nodecode.working-context-proof::finish)
     (nodecode.working-context-proof::enter "context-cell-parent" "KEEP CURRENT ASK")
     ;; Even an empty ranked index and a removed durable section cannot cool the skill.
     (nlk:set-harness-section "context-cell-parent" "index" "")
     (nlk:set-harness-section "context-cell-parent" "context" "")
     (is (context-cell-once "context-cell-parent") "skill occurs once in the compiled provider request")
     (nodecode.working-context-proof::sync "context-cell-parent")
     (is (not (search nodecode-context::*skill*
                      (uiop:read-file-string (nle::context-file "context-cell-parent")))) "skill is outside the editable file body")
     (nodecode.working-context-proof::replace-body "context-cell-parent" "")
     (nodecode.working-context-proof::sync "context-cell-parent")
     (let ((request (context-cell-request "context-cell-parent")))
       (is (not (search "DISPOSABLE OLD OUTPUT" request)) "empty edit really deletes the old working context")
       (is (search "KEEP CURRENT ASK" request) "protected user input survives"))
     (is (context-cell-once "context-cell-parent") "empty edit cannot delete the skill")
     (let ((anchor (nlk:durable-turn-turn-id nle::*current-durable-turn*)))
       (nodecode.working-context-proof::finish)
       (nlk:create-session :id "context-cell-child" :parent "context-cell-parent" :anchor-turn-id anchor))
     (nlk:advance-history-floor "context-cell-parent"
                                (1+ (nlk:session-log-cursor "context-cell-parent")))
     (nodecode.working-context-proof::enter "context-cell-parent" "AFTER EVICTION")
     (is (context-cell-once "context-cell-parent") "history eviction repins the complete skill")
     (nle::clear-hydration-cache)
     (is (context-cell-once "context-cell-parent") "cache loss cannot cool the skill")
     (nodecode.working-context-proof::finish)
     (nodecode.working-context-proof::enter "context-cell-child" "FORK CONTINUATION")
     (is (context-cell-once "context-cell-child") "fork keeps the same complete skill")
     (nodecode.working-context-proof::finish))))

(deftest context-cell-reopens-with-the-skill ()
  (context-cell-install)
  (let ((path (namestring (nlk:home "context-cell-replay.db"))))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (nodecode.working-context-proof::with-database
            (lambda ()
              (nlk:create-session :id "context-cell-replay" :cwd "/tmp/")
              (nodecode.working-context-proof::enter "context-cell-replay" "FIRST")
              (is (context-cell-once "context-cell-replay") "first request pins full skill")
              (nodecode.working-context-proof::finish)) :path path)
           (nodecode.working-context-proof::with-database
            (lambda ()
              (nodecode.working-context-proof::enter "context-cell-replay" "AFTER REOPEN")
              (is (context-cell-once "context-cell-replay") "SQLite reopen retains full skill")
              (nodecode.working-context-proof::finish)) :path path))
      (when (probe-file path) (delete-file path)))))
