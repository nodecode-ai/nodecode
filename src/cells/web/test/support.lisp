;;;; support.lisp --- web test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Web tests register into the shared nodecode.test registry under the
;;;; WEB-CELL- prefix; RUN-WEB-TESTS runs exactly that slice.

(in-package #:nodecode.test)

(define-test-slice "web" "WEB-CELL-" :start nodecode-web:start-cell)
