;;;; nodecode-board.asd --- sessions that join a board read each other's posts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `board' section, hooks the :ROUND point so a
;;;; member hears the posts it has not seen at its next model request,
;;;; registers /board and puts the manual on (help :board).
;;;;
;;;; A post is a fact in the event log, on the board's own aggregate; the
;;;; file under the home is its rendering. Every dependency is already in the
;;;; serving image.

(defsystem "nodecode-board"
  :description "Sessions that join a board read each other's posts"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-board/test"))))

(defsystem "nodecode-board/test"
  :description "Board tests. Registered into the shared nodecode.test registry; RUN-BOARD-TESTS filters by the BOARD-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-board" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-board-tests)))
