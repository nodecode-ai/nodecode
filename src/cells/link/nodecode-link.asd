;;;; nodecode-link.asd --- this machine's own page, from anywhere: one line out
;;;; to a relay, and every browser the operator allowed through it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: nothing in src/src names
;;;; this system. `/link on' turns it on: the machine dials ONE outbound
;;;; WebSocket to the relay (uplink.nodecode.ai by default) and keeps it, and
;;;; every request a browser makes of this machine's address arrives down it
;;;; as a stream (the contract: nodecode-marketplace/uplink/PROTOCOL.md).
;;;; Nothing listens here and nothing is stored there: the sessions, the store
;;;; and the turns stay on this machine, and what crosses the line is the web
;;;; cell's page and the gateway's own traffic, replayed on loopback with the
;;;; operator's token, which never leaves the machine. A browser is let in by
;;;; the operator at the machine, once, after comparing a 6-digit code.
;;;;
;;;; Dependencies: nodecode (dexador and websocket-driver come through it),
;;;; and ironclad's SHA-256, which the core does not load -- an allowed
;;;; browser's cookie is kept as its digest.

(defsystem "nodecode-link"
  :description "Open this machine's page from anywhere; /link on turns it on"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad/digest/sha256")
  :serial t
  :components ((:file "package")
               ;; pair before line: the line's streams ask it who is allowed.
               (:file "pair")
               (:file "line")
               (:file "proxy")
               (:file "qr")
               (:file "cell")
               (:static-file "page/pair.html")
               (:static-file "page/pair.js")
               (:static-file "page/pair.css"))
  :in-order-to ((test-op (test-op "nodecode-link/test"))))

(defsystem "nodecode-link/test"
  :description "Link tests. Registered into the shared nodecode.test registry; RUN-LINK-TESTS filters by the LINK-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-link" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-link-tests)))
