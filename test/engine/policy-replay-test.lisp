;;;; policy-replay-test.lisp --- the evidence a policy review judges by
;;;; (policy-replay.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermetic: the model is a fixture that asks first on a push whenever its
;;;; policy says ASK FIRST and refuses a deletion when it says NEVER DELETE,
;;;; and the asks are either the fixture's or a scratch store's.

(in-package #:nodecode.test)

(defparameter +replay-strict+ "Act on asks. ASK FIRST before any push."
  "A policy under which the fixture model asks before a push.")

(defparameter +replay-asks+
  '("push the fix to main" "summarise the thread" "delete the old branch" "push the docs")
  "What the agent was asked in the fixture's window.")

(defun replay-fixture-model (system user)
  "Ask first on a push when SYSTEM says ASK FIRST, refuse a deletion when it
says NEVER DELETE, act otherwise."
  (cond ((and (search "ASK FIRST" system) (search "push" user))
         "{\"decision\": \"ask\", \"step\": \"ask before pushing\"}")
        ((and (search "NEVER DELETE" system) (search "delete" user))
         "{\"decision\": \"refuse\", \"step\": \"decline\"}")
        (t "{\"decision\": \"act\", \"step\": \"do it\"}")))

(deftest replay-decision-reads-json-then-words ()
  (is (eq :ask (nle::replay-decision "{\"decision\": \"ask\", \"step\": \"x\"}")))
  (is (eq :refuse (nle::replay-decision "I would refuse, then act later")) "the first decision word")
  (is (eq :silent (nle::replay-decision "prose {\"decision\":\"silent\"} more")))
  (is (eq :unparsed (nle::replay-decision "no idea"))))

(deftest replay-counts-flips-and-their-direction ()
  (with-saved-globals ((nle::*policy-replay-complete* #'replay-fixture-model))
    (let ((stricter (nle::policy-replay "Act on asks." +replay-strict+ :asks +replay-asks+))
          (looser (nle::policy-replay +replay-strict+ "Act on asks." :asks +replay-asks+))
          (mixed (nle::policy-replay +replay-strict+ "Act on asks. NEVER DELETE a branch." :asks +replay-asks+)))
      (is (= 4 (getf stricter :turns)))
      (is (= 2 (getf stricter :flips)) "both pushes flip")
      (is (= 2 (getf stricter :stricter)) "act -> ask is stricter")
      (is (zerop (getf stricter :looser)))
      (is (= 2 (getf looser :looser)) "ask -> act is looser")
      (is (equal '(:ask :act) (let ((row (first (getf looser :items)))) (list (getf row :old) (getf row :new)))))
      (is (= 2 (getf mixed :looser)) "a tightening that drops a rule shows its loosening")
      (is (= 1 (getf mixed :stricter)) "beside what it tightened"))))

(deftest replay-scores-a-no-op-edit-as-no-flips-however-noisy-the-model ()
  (let ((coin nil))
    (with-saved-globals ((nle::*policy-replay-complete*
                          (lambda (system user)
                            (declare (ignore system user))
                            (setf coin (not coin))
                            (if coin "{\"decision\": \"act\"}" "{\"decision\": \"ask\"}"))))
      (let ((report (nle::policy-replay "same" "same" :asks +replay-asks+)))
        (is (zerop (getf report :flips)) "a model that disagrees with itself flips nothing")
        (is (= 4 (getf report :noise)) "its disagreement is counted as noise")))))

(deftest replay-keeps-a-failed-call-out-of-the-flips ()
  (with-saved-globals ((nle::*policy-replay-complete* (lambda (s u) (declare (ignore s u)) (error "down"))))
    (let ((report (nle::policy-replay "a" "b" :asks '("x"))))
      (is (= 1 (getf report :failed)))
      (is (zerop (getf report :flips))))))

(deftest replay-puts-each-policy-file-before-the-fixed-question ()
  (let ((system (nle::replay-system '(("AGENTS.md" . "rules") ("SOUL.md" . nil) ("SYSTEM.md" . "prompt")))))
    (is (search "<policy-file name=\"AGENTS.md\">" system))
    (is (not (search "SOUL.md" system)) "a file the policy does not have is left out")
    (let ((policy-at (search "prompt" system)) (question-at (search nle::+replay-instruction+ system)))
      (is (< policy-at question-at) "the question comes after the policy, never inside it"))))

(deftest replay-reads-the-asks-the-log-committed-in-its-window (with-temp-store ())
  (seed-completed-turn "replay-a" "push the fix" "done")
  (seed-completed-turn "replay-a" "push the fix" "done again")
  (seed-completed-turn "replay-b" "summarise the thread" "summary")
  (let ((all (nle::replay-asks :since 0))
        (one (nle::replay-asks :since 0 :session-ids '("replay-b")))
        (none (nle::replay-asks :since 0 :until 1)))
    (let ((texts (mapcar (lambda (row) (getf row :text)) all)))
      (is (equal '("push the fix" "summarise the thread") texts) "each text once, oldest first"))
    (is (equal '("replay-b") (mapcar (lambda (row) (getf row :session)) one)))
    (is (null none) "nothing before its window")))
