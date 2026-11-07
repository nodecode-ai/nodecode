;;;; nodecode-goal.asd --- a session keeps taking turns until its goal's check passes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `goal' section, watches the :FRAME point for
;;;; turns ending, starts the one worker that takes each goal's next step,
;;;; registers /goal and puts the manual on (help :goal).
;;;;
;;;; A goal is facts in the event log, on the session's own log and the
;;;; goal's aggregate; where one stands is a fold of them and the session's
;;;; turns. Every dependency is already in the serving image.

(defsystem "nodecode-goal"
  :description "A session keeps taking turns until its goal's check passes"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-goal/test"))))

(defsystem "nodecode-goal/test"
  :description "Goal tests. Registered into the shared nodecode.test registry; RUN-GOAL-TESTS filters by the GOAL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-goal" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-goal-tests)))
