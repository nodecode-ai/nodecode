;;;; nodecode-channel-slack.asd --- Slack channel adapter add-on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-ADDON when channels.slack is
;;;; present in the shared config. The .asd sits inside its own folder so
;;;; the kit finds it through the folder registry. Shipped with every
;;;; release beside the kit, Discord and Telegram (src/cells/bundle.json),
;;;; and tested with them (just channels-test).
;;;;
;;;; Slack is reached over Socket Mode -- a websocket the app opens -- so the
;;;; organism needs no public address.

(defsystem "nodecode-channel-slack"
  :description "Talk to nodecode through a Slack app"
  :license "MIT"
  :version "0.1.0"
  ;; The kit brings what the socket and a file's download are made of
  ;; (websocket-driver, dexador), and the organism the rest; naming only the
  ;; kit keeps the hub's depends_on the one folder a person installs with it.
  :depends-on ("nodecode-channel-kit")
  :serial t
  :components ((:file "package")
               (:file "socket")
               (:file "events")
               (:file "rest")
               (:file "probe")
               (:file "adapter")
               (:file "board")
               (:static-file "manifest.json"))
  :in-order-to ((test-op (test-op "nodecode-channel-slack/test"))))

(defsystem "nodecode-channel-slack/test"
  :description "Slack adapter tests, registered under the CHANNEL-SLACK- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-slack" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "fake-slack")
               (:file "events-test")
               (:file "rest-test")
               (:file "adapter-test")
               (:file "board-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-slack-tests)))
