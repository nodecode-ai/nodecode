;;;; nodecode-perf.asd --- the scaling gate's own system.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A system of its own rather than more files in nodecode/test: the gate
;;;; stands on the test system's fixtures (the shell over a temp store, the
;;;; seeded turn, the wire fold), so it loads after it, and neither the
;;;; product nor the suite carries it.

(asdf:defsystem "nodecode-perf"
  :description "The scaling gate: hot paths timed at several sizes, their growth fitted."
  :license "MIT"
  :depends-on ("nodecode" "nodecode/test")
  :serial t
  :components ((:module "perf"
                :serial t
                :components ((:file "package")
                             (:file "source")
                             (:file "scaling")))))
