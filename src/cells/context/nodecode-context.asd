;;;; SPDX-License-Identifier: MIT
(defsystem "nodecode-context"
  :description "Always-visible context-editing skill"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-context/test"))))

(defsystem "nodecode-context/test"
  :depends-on ("nodecode-context" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-context-cell-tests)))
