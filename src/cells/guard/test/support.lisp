;;;; support.lisp --- guard test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Guard tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a GUARD- name
;;;; prefix; RUN-GUARD-TESTS runs exactly that slice, so this system's test-op
;;;; never re-runs the core suite and `just test` never runs guard tests.
;;;;
;;;; Fixtures the core suite owns are reused rather than rebuilt: EXECUTE-WIRE-CALL
;;;; comes from test/engine/turn-test.lisp, which loads as
;;;; part of nodecode/test. The hermetic posture LET-binds NLE:*HOOKS* to
;;;; '() around every test body, so a hook installed by START-CELL unwinds
;;;; with the test and never guards a later one.

(in-package #:nodecode.test)

(define-test-slice "guard" "GUARD" :start nodecode-guard:start-cell)
