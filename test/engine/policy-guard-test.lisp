;;;; policy-guard-test.lisp --- the fixed rules a tool call cannot write.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; With policy.self on, an agent's EDIT/WRITE-FILE of the charter (the home's
;;;; AGENTS.md), the shared config or the store's history, and its CONFIG-SET of
;;;; a fixed member, are refused before anything is written; the operator's
;;;; hand (no tool call) and the mode off are today's behaviour. The `policy'
;;;; switch is refused to a tool call in both modes.

(in-package #:nodecode.test)

(defmacro with-policy-mode ((self &key (tool-call "call_policy")) &body body)
  "BODY with policy.self reading SELF and *EXECUTING-TOOL-CALL-ID* bound to TOOL-CALL."
  `(with-stubbed-fdefinition (nle::policy-self-p () ,self)
     (let ((nle::*executing-tool-call-id* ,tool-call))
       ,@body)))

(defun policy-refusal-text (thunk)
  "The message THUNK signals, NIL when it returns."
  (handler-case (progn (funcall thunk) nil)
    (error (e) (princ-to-string e))))

(deftest policy-guard-refuses-a-tool-calls-write-of-the-charter ()
  (let ((charter (nlk:home "AGENTS.md")))
    (nle::write-file-text charter "operator rule")
    (with-policy-mode (t)
      (is (search "charter" (or (policy-refusal-text (lambda () (nle::write-file-text charter "loosened"))) "")) "the charter write is refused, the refusal naming it")
      (is (equal "operator rule" (uiop:read-file-string charter)) "nothing was written")
      (is (null (policy-refusal-text (lambda () (nle::write-file-text (nlk:home "policy/AGENTS.md") "own rule")))) "the agent's own AGENTS.md in the store is its own to write"))
    (with-policy-mode (nil)
      (is (null (policy-refusal-text (lambda () (nle::write-file-text charter "edited")))) "with the mode off a write is today's"))
    (with-policy-mode (t :tool-call nil)
      (is (null (policy-refusal-text (lambda () (nle::write-file-text charter "operator's edit")))) "the operator's hand is no tool call and passes"))))

(deftest policy-guard-refuses-a-charter-created-from-nothing ()
  ;; A home without AGENTS.md: a file the agent wrote there would ride as the
  ;; operator's rules.
  (let ((charter (nlk:home "AGENTS.md")))
    (ignore-errors (delete-file charter))
    (with-policy-mode (t)
      (is (policy-refusal-text (lambda () (nle::write-file-text charter "rules I wrote"))))
      (is (null (probe-file charter)) "no charter appeared"))))

#-win32
(deftest policy-guard-follows-a-link-to-the-charter ()
  ;; On a box whose home AGENTS.md is a link into a dotfiles tree, writing the
  ;; file the link reaches is writing the charter.
  (let* ((target (nlk:home "dotfiles/AGENTS.md"))
         (charter (nlk:home "AGENTS.md")))
    (ensure-directories-exist target)
    (with-open-file (out target :direction :output :if-exists :supersede) (write-string "fixed" out))
    (ignore-errors (delete-file charter))
    (uiop:run-program (list "ln" "-s" (uiop:native-namestring target) (uiop:native-namestring charter)))
    (with-policy-mode (t)
      (is (policy-refusal-text (lambda () (nle::write-file-text target "loosened"))) "the link's target is refused")
      (is (policy-refusal-text (lambda () (nle::write-file-text charter "loosened"))) "the link itself is refused")
      (is (equal "fixed" (uiop:read-file-string target))))))

(deftest policy-guard-refuses-the-store-history-and-the-config-file ()
  (with-temp-shared-config ("{}" path)
    (with-policy-mode (t)
      (is (search "config-set" (or (policy-refusal-text (lambda () (nle::write-file-text path "{\"policy\":{}}"))) "")) "the config file is refused whole, config-set named as the way")
      (is (policy-refusal-text (lambda () (nle::write-file-text (nlk:home "policy/.git/refs/heads/adopted") "0000"))) "a hand-moved adopted ref is refused"))))

(deftest policy-guard-keeps-the-switch-off-a-tool-call-in-both-modes ()
  (with-temp-shared-config ("{}" path)
    (dolist (self '(nil t))
      (with-policy-mode (self)
        (is (search "switch" (or (policy-refusal-text (lambda () (nle:config-set '("policy" "self") t))) ""))
            (format nil "policy.self refused with the mode ~:[off~;on~]" self))
        (is (policy-refusal-text (lambda () (nle:config-set '() '(:policy (:self :false))))) "an object write at the root naming policy is refused")
        (is (policy-refusal-text (lambda () (nle:config-set '("policy") :remove))) "removing the member is refused")))
    (with-policy-mode (nil :tool-call nil)
      (nle:config-set '("policy" "self") t)
      (is (search "\"self\"" (uiop:read-file-string path)) "the operator throws the switch"))))

(deftest policy-guard-holds-owner-guard-and-the-guard-cell-while-on ()
  (with-temp-shared-config ("{\"cells\": [\"nodecode-guard\"], \"channels\": {\"discord\": {\"owner\": \"1\"}}}" path)
    (with-policy-mode (t)
      (is (policy-refusal-text (lambda () (nle:config-set '("channels" "discord" "owner") "2"))) "owner refused")
      (is (policy-refusal-text (lambda () (nle:config-set '("channels" "discord") '(:owner "2" :enabled t)))) "an object merge that names owner is refused")
      (is (policy-refusal-text (lambda () (nle:config-set '("channels") :remove))) "removing what holds owner is refused")
      (is (null (policy-refusal-text (lambda () (nle:config-set '("channels" "discord") '(:require_mention t))))) "the rest of a channel stays the agent's to set")
      (is (policy-refusal-text (lambda () (nle:config-set '("guard" "enabled") :false))) "the guard is refused")
      (is (policy-refusal-text (lambda () (nle:config-set '("cells") #("other")))) "dropping the guard cell is refused")
      (is (null (policy-refusal-text (lambda () (nle:config-set '("cells") #("nodecode-guard" "other"))))) "a cell added beside it passes"))
    (with-policy-mode (nil)
      (is (null (policy-refusal-text (lambda () (nle:config-set '("channels" "discord" "owner") "3")))) "with the mode off the owner is today's to set")
      (is (search "\"3\"" (uiop:read-file-string path))))))
