;;;; nodecode-guard.asd --- pre-tool-call guard cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel cells.lisp) loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls this system's START-CELL after recovery, which compiles
;;;; the `guard` section into waist textrules and hooks them onto the core's
;;;; :TOOL point.
;;;;
;;;; The second catalogued extension example, and deliberately a different axis
;;;; from the rooms (rooms/): a room ADDS a surface, this one INTERCEPTS the
;;;; organism's own behaviour. Both ride the same one-convention seam — and
;;;; this one is the catalogue example of NLE:HOOK, the whole cell being one
;;;; file of rule data around one hook call.
;;;;
;;;; The .asd sits INSIDE its folder: a cell is a directory
;;;; carrying its .asd, and it is installed by putting — or symlinking — that
;;;; directory under ~/.nodecode/cells/. Presence is enabled; nothing in
;;;; config names it. The `just *` recipes register each cell
;;;; directory with ASDF the same way.

(defsystem "nodecode-guard"
  :description "Refuses risky shell commands before they run"
  :license "MIT"
  :version "0.2.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-guard/test"))))

(defsystem "nodecode-guard/test"
  :description "Guard tests. Registered into the shared nodecode.test registry; RUN-GUARD-TESTS filters by the GUARD- name prefix."
  :license "MIT"
  :depends-on ("nodecode-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-guard-tests)))
