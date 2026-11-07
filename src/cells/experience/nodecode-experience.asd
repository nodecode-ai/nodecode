;;;; nodecode-experience.asd --- the experience loop: reflection and recaps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel cells.lisp) loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which reads the `experience'
;;;; config section, observes the :FRAME point for turns ending, fences the
;;;; reflections' tool calls, starts one reflector thread and registers
;;;; /experience.
;;;;
;;;; This cell is the backward pass over what the organism keeps: after an
;;;; operator turn a fork of the session reflects on it - one request behind
;;;; the origin's cached prefix, with tools - recording sightings with
;;;; verbatim quotes in the use ledger the index ranks by (knowledge.lisp),
;;;; keeping memories and skills as definitions, and answering a recap that
;;;; is recorded back into the session as its own note.
;;;;
;;;; Every dependency is already in the serving image. The .asd sits INSIDE
;;;; its folder: install by putting -- or symlinking -- the
;;;; directory under ~/.nodecode/cells/.

(defsystem "nodecode-experience"
  :description "Learns from each turn: reflection and recaps"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "reflect")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-experience/test"))))

(defsystem "nodecode-experience/test"
  :description "Experience tests. Registered into the shared nodecode.test registry; RUN-EXPERIENCE-TESTS filters by the EXPERIENCE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-experience" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "ledger-test")
               (:file "loop-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-experience-tests)))
