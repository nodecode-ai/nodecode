;;;; nodecode-channel-telegram.asd --- Telegram channel adapter cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.telegram is
;;;; enabled in the shared config. The .asd sits inside its own folder:
;;;; the kit finds it through the folder registry.

(defsystem "nodecode-channel-telegram"
  :description "Talk to nodecode through a Telegram bot"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-channel-kit"
               ;; dexador is declared explicitly: the download of an
               ;; attachment's bytes is this system's own call.
               "dexador")
  :serial t
  :components ((:file "package")
               (:file "poll")
               (:file "rest")
               (:file "probe")
               (:file "adapter")
               (:file "board"))
  :in-order-to ((test-op (test-op "nodecode-channel-telegram/test"))))

(defsystem "nodecode-channel-telegram/test"
  :description "Telegram adapter tests, registered under the CHANNEL- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-telegram" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "poll-test")
               (:file "rest-test")
               (:file "adapter-test")
               (:file "board-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
