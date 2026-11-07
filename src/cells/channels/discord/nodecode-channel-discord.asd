;;;; nodecode-channel-discord.asd --- Discord channel adapter cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode-channel-kit's START-CELL when channels.discord is
;;;; enabled in the shared config. The .asd sits inside its own folder:
;;;; the kit finds it through the folder registry.

(defsystem "nodecode-channel-discord"
  :description "Talk to nodecode through a Discord bot"
  :license "MIT"
  :version "0.1.0"
  ;; The voice transport cipher is AES-256-GCM (aead_aes256_gcm_rtpsize).
  ;; The core loads sha256 alone, on purpose, so the two subsystems it
  ;; takes are named here.
  :depends-on ("nodecode-channel-kit" "ironclad/cipher/aes" "ironclad/aead/gcm" "quri")
  :serial t
  :components ((:file "package")
               (:file "gateway")
               (:file "rest")
               (:file "probe")
               ;; dave and voice are the media layer and hold no adapter
               ;; state, so they come before it; voicelap is built on the
               ;; adapter and comes after.
               (:file "dave")
               (:file "voice")
               (:file "adapter")
               (:file "board")
               (:file "voicelap"))
  :in-order-to ((test-op (test-op "nodecode-channel-discord/test"))))

(defsystem "nodecode-channel-discord/test"
  :description "Discord adapter tests, registered under the CHANNEL- prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-discord" "nodecode-channel-kit/test")
  :pathname "test/"
  :serial t
  :components ((:file "gateway-test")
               (:file "rest-test")
               (:file "voice-test")
               (:file "adapter-test")
               (:file "board-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
