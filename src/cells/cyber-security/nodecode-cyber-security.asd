;;;; nodecode-cyber-security.asd --- the breaches and the fixes that follow them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader
;;;; loads it at boot when this directory sits under ~/.nodecode/cells/, or
;;;; the gateway installs it live when `nodecode add nc://nodecode-cyber-security'
;;;; is accepted. Nothing here dials until asked, and the feed it reads is
;;;; refreshed at runtime, so an installed copy never goes stale.

(defsystem "nodecode-cyber-security"
  :description "Cyber security: the latest breaches and the fix for each, re-scanned every 6 hours; checks your server and website and walks you through patching"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "feed")
               (:file "detect")
               (:file "validate")
               (:file "check")
               (:file "scan")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cyber-security/test"))))

(defsystem "nodecode-cyber-security/test"
  :description "Tests for the cyber-security cell; RUN-CYBER-SECURITY-TESTS filters the shared registry by the CYBER-SECURITY- prefix."
  :license "MIT"
  :depends-on ("nodecode-cyber-security" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "feed-test")
               (:file "detect-test")
               (:file "scan-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cyber-security-tests)))
