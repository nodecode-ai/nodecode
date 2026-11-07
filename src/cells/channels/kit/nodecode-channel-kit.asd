;;;; nodecode-channel-kit.asd --- shared channel cell machinery.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel cells.lisp) loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls this system's START-CELL after recovery — the channel host
;;;; that reads channels.<id> sections and starts each enabled adapter system.
;;;;
;;;; The .asd sits INSIDE its folder: a cell is a directory
;;;; carrying its .asd, and it is installed by putting — or symlinking — that
;;;; directory under ~/.nodecode/cells/. Presence is enabled; nothing in
;;;; config names it. The `just *` recipes register each cell
;;;; directory with ASDF the same way.
;;;;
;;;; bordeaux-threads is declared explicitly: the core system only receives it
;;;; transitively, and this system's delivery and supervision threads depend
;;;; on the bt2 API directly. websocket-driver and cl+ssl likewise:
;;;; SEVER-WS-TRANSPORT reaches the raw fd beneath a platform TLS websocket.
;;;;
;;;; The gateway itself is reached in-process — NLE:HOOK :FRAME for egress,
;;;; NLE:SUBMIT for ingress, the NLK session API — never over loopback.

(defsystem "nodecode-channel-kit"
  :description "Shared plumbing for the chat bots; comes with Discord or Telegram"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "bordeaux-threads" "shasht" "dexador"
               "websocket-driver" "cl+ssl" "cl-base64" "ironclad/digest/sha256")
  :serial t
  :components ((:file "package")
               (:static-file "tools/table-png.py")
               (:file "config")
               (:file "fetch")
               (:file "soul")
               (:file "admission")
               (:file "outbound")
               (:file "delivery")
               (:file "digest")
               (:file "supervise")
               (:file "organism")
               (:file "status")
               (:file "room")
               (:file "transcribe")
               (:file "speech")
               (:file "tables")
               (:file "host")
               (:file "board")
               (:file "operator")
               (:file "setup")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-channel-kit/test"))))

(defsystem "nodecode-channel-kit/test"
  :description "Kit tests. Registered into the shared nodecode.test registry; RUN-CHANNEL-TESTS filters by the CHANNEL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-channel-kit" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "config-test")
               (:file "soul-test")
               (:file "lease-test")
               (:file "admission-test")
               (:file "outbound-test")
               (:file "delivery-test")
               (:file "digest-test")
               (:file "supervise-test")
               (:file "organism-test")
               (:file "room-test")
               (:file "transcribe-test")
               (:file "host-test")
               (:file "thread-test")
               (:file "board-test")
               (:file "cell-test")
               (:file "operator-test")
               ;; LAST on purpose: placed ahead of thread-test. Without this,
               ;; CHANNEL-THREAD-ASK-INSIDE-A-THREAD-CARRIES-ITS-OWN-LINE's
               ;; ten-second AWAIT-PLAN would tip in the full suite.
               (:file "tables-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-channel-tests)))
