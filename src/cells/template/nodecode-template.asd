;;;; nodecode-template.asd --- a peripheral cell, ready to copy.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copy this directory to ~/.nodecode/cells/<name>/, rename this file and
;;;; the system to <name>, and the organism loads it at the next boot. See
;;;; src/CELLS.md for the contract. The .asd sits inside its folder; the
;;;; system is named by the file.

(defsystem "nodecode-template"
  :description "A peripheral: one START-CELL and one hook."
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell")))
