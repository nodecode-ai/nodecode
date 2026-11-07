;;;; nodecode-websearch.asd --- web search and page fetch from eval.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel cells.lisp) loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which materializes the
;;;; `websearch' config section and puts one static primer on every session's
;;;; harness sections so the model knows (web:search ...) and (web:fetch ...)
;;;; exist.
;;;;
;;;; A fifth axis in the catalogue: channels ADD a surface, the guard
;;;; INTERCEPTS, chrome gives the organism a PERIPHERAL, notify OBSERVES
;;;; egress — this one is pure VOCABULARY: two functions over the network and
;;;; a primer, no thread, no slash command, no lease. pi-web-access
;;;; (github.com/nicobailon/pi-web-access) reduced to its kernel: three keyed
;;;; JSON search providers over a keyless public floor (DuckDuckGo's HTML
;;;; endpoint and Exa's MCP endpoint, merged), one fetch that turns a page —
;;;; HTML, PDF, text — into bounded text, the page cached so the model reads
;;;; on by offset instead of downloading twice.
;;;;
;;;; Every dependency below is already in the serving image (dexador, shasht,
;;;; cl-ppcre, quri, bordeaux-threads ride with nodecode). The .asd sits
;;;; INSIDE its folder: install by putting — or symlinking — the
;;;; directory under ~/.nodecode/cells/. Presence is enabled; the package is
;;;; named after the system, which is how the loader finds START-CELL.

(defsystem "nodecode-websearch"
  :description "Web search and page fetch; no key needed"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "http")
               (:file "extract")
               (:file "search")
               (:file "fetch")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-websearch/test"))))

(defsystem "nodecode-websearch/test"
  :description "Websearch tests. Registered into the shared nodecode.test registry; RUN-WEBSEARCH-TESTS filters by the WEBSEARCH-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-websearch" "nodecode/test" "flexi-streams")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-websearch-tests)))
