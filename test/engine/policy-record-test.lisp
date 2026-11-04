;;;; policy-record-test.lisp --- the store's history is pushed off the box, never forced.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun policy-test-remote ()
  "A fresh bare repository to push the record to."
  (let ((remote (uiop:ensure-directory-pathname (nlk:home "record.git/"))))
    (ensure-directories-exist remote)
    (policy-test-git remote "init" "-q" "--bare")
    remote))

(deftest policy-record-pushes-and-never-forces ()
  (nle::policy-adopt '(("AGENTS.md" . "ask before deleting")) "seed")
  (let ((store (nle::policy-directory))
        (remote (policy-test-remote)))
    (is (eq :no-remote (nle::policy-record-push :remote nil)))
    (is (eq :pushed (nle::policy-record-push :remote (uiop:native-namestring remote))))
    (is (equal (policy-test-git store "rev-parse" "adopted")
               (policy-test-git remote "rev-parse" "policy/adopted")))
    ;; Rewrite the history here: a sibling of the tip replaces it.
    (let ((pushed (policy-test-git store "rev-parse" "adopted"))
          (sibling (policy-test-git store "commit-tree" "adopted^{tree}" "-m" "rewritten")))
      (policy-test-git store "update-ref" nle::+policy-adopted-ref+ sibling)
      (is (eq :diverged (nle::policy-record-push :remote (uiop:native-namestring remote))))
      (is (equal pushed (policy-test-git remote "rev-parse" "policy/adopted")) "the record kept its history")
      (is (assoc nle::*policy-record-notice-key* (nlk:standing-notices) :test #'string=) "a diverged record stands a notice"))))

(deftest an-edit-is-pushed-to-the-record-off-the-request ()
  (with-temp-shared-config ("{}")
    (is (null (nle::policy-record-push-later)) "no remote set, no push")
    (let ((remote (policy-test-remote)))
      (nle:config-set '("policy" "record") (uiop:native-namestring remote))
      (nle::policy-adopt '(("AGENTS.md" . "name the file you changed")) "policy: one rule")
      (let ((push (find "policy record push" (sb-thread:list-all-threads)
                        :key #'sb-thread:thread-name :test #'equal)))
        (when push (sb-thread:join-thread push :default nil)))
      (let ((pushed (ignore-errors (policy-test-git remote "rev-parse" "policy/adopted"))))
        (is (equal (policy-test-git (nle::policy-directory) "rev-parse" "adopted") pushed) "the commit an edit made reaches the record")))))
