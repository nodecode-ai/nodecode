;;;; nodecode-qa.asd --- one page of counts a week, by consent.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `qa' section, registers the
;;;; report_issue tool and /qa, and — only when the section says
;;;; share: weekly — starts the one thread that sends a page of counts a
;;;; week. See README.md beside this file for exactly what a page holds.
;;;;
;;;; Every dependency is already in the serving image. The .asd sits INSIDE
;;;; its folder: install by putting — or symlinking — the directory under
;;;; ~/.nodecode/cells/.

(defsystem "nodecode-qa"
  :description "Anonymous usage counts, one page a week, sent only if you say so"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "page")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-qa/test"))))

(defsystem "nodecode-qa/test"
  :description "QA tests. Registered into the shared nodecode.test registry; RUN-QA-TESTS filters by the QA-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-qa" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "page-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-qa-tests)))
