;;;; nodecode-web.asd --- the organism in a browser tab, on the gateway's own port.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: nothing in src/src names
;;;; this system. It serves one page from page/ at /web/ (NLE:ROUTE); the page
;;;; speaks the gateway's own sync protocol to the gateway it came from, asks
;;;; for its sessions as rows (surface/rows.lisp), and sends turns the way a
;;;; shell does. No build step: the page is the files in page/.

(defsystem "nodecode-web"
  :description "Use nodecode in a browser tab; run nodecode web to open it"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-web/test"))))

(defsystem "nodecode-web/test"
  :description "Web tests. Registered into the shared nodecode.test registry; RUN-WEB-TESTS filters by the WEB-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-web" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test")
               (:file "inline-color-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-web-tests)))
