;;;; nodecode-team.asd --- N identical sessions on one task, sharing a directory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `team' section, advises NLE:TURN-BUDGET for
;;;; the nodes' sessions, registers /team, which seats a session's standing
;;;; team, keeps the nodes' definitions out of the layer, and puts a primer
;;;; on every session's harness sections so the model knows the team and
;;;; (team:open ...) exist.
;;;;
;;;; The protocol is team@N (arXiv 2609.21032): the nodes are ordinary
;;;; sessions, the channel is the files of one directory, a slot is claimed
;;;; by an atomic mkdir, and nothing here relays a message or runs a
;;;; controller. What the cell owns is the directory's layout, the one
;;;; prompt paragraph, what each node has left, and the parent's watch.
;;;;
;;;; Every dependency is already in the serving image. The .asd sits INSIDE
;;;; its folder: install by putting — or symlinking — the directory under
;;;; ~/.nodecode/cells/.

(defsystem "nodecode-team"
  :description "Several sessions work one task together in a shared folder"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-team/test"))))

(defsystem "nodecode-team/test"
  :description "Team tests. Registered into the shared nodecode.test registry; RUN-TEAM-TESTS filters by the TEAM-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-team" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-team-tests)))
