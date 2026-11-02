;;;; working-context-test.lisp --- the normal test lane shares the live proof.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)
(deftest working-context-request-replay-and-forks ()
  (is (plusp (getf (nodecode.working-context-proof:run) :passed))))

(deftest retired-task-surface-absent ()
  (is (not (eq :external (nth-value 1 (find-symbol "TASK" :nodecode.evolved)))))
  (is (notany (lambda (name &aux (symbol (find-symbol name :nodecode.evolved)))
                (and symbol (or (fboundp symbol) (boundp symbol))))
              '("TASK-STRING" "TASK-EVENTS" "TASK-PROJECT" "TASK-STATE"
                "TASK-DESCRIPTION" "TASK-TEXT" "TASK-CURRENT-TURN" "TASK-USER-SOURCE"
                "TASK-APPEND" "TASK-CAPTURE" "TASK-CHECK-CAPTURE" "TASK" "TASK-LIVE-SECTION"
                "TASK-WORKING-TEXT" "REFRESH-TASK-CONTEXT" "*TASK-CONTRACT*"
                "*TASK-INPUT-LIMIT*" "*TASK-CONTEXT-LIMIT*" "+TASK-CHECKER-VERSION+")))
  (is (not (find-package :nodecode.task-proof)))
  (is (not (find "TASK" nle::*help-verbs* :key #'symbol-name :test #'equal)))
  (is (not (assoc :task nle::*help-topics*)))
  (is (not (search "  :task -" (nle:help))))
  (is (not (loop for kind being the hash-keys of nlk::*owned-kind-owners*
                 thereis (uiop:string-prefix-p "task." kind))))
  (is (notany (lambda (kind) (uiop:string-prefix-p "task." kind))
              nlk::*replay-suppressed-kinds*))
  (nodecode.working-context-proof::with-database
   (lambda ()
     (nlk:create-session :id "retired-task" :cwd "/tmp/")
     (is (not (assoc "task" (nth-value 1 (nle::current-prompt "retired-task" nil))
                     :test #'equal))))))
