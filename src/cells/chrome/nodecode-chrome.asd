;;;; nodecode-chrome.asd --- Chrome control through a companion extension.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel cells.lisp) loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which starts the bridge the
;;;; extension polls and registers the /chrome slash command.
;;;;
;;;; The third catalogued extension example, on a third axis: channels ADD a
;;;; surface, the guard INTERCEPTS the organism, this one gives the organism a
;;;; PERIPHERAL — the user's real, signed-in Chrome — reached from EVAL
;;;; as plain functions in the CHROME package (mono-tool: no new tool is
;;;; registered). chrome/browser-extension/ is pi-chrome's MV3 extension,
;;;; vendored with the changes its NOTICE lists (MIT); the pi host side is
;;;; re-implemented here, minus its per-session authorization: an HTTP
;;;; long-poll bridge on 127.0.0.1:17318, a <harness> primer block every
;;;; session carries, and the formatters that make a page snapshot fit a tool
;;;; result.
;;;;
;;;; Threat model, verbatim in spirit from pi-chrome's SECURITY.md: the bridge
;;;; is loopback-only and refuses non-extension browser origins (so a web page
;;;; cannot CORS-drive Chrome), but it cannot authenticate local processes —
;;;; the extension carries no token. A hostile local process
;;;; running as you already owns the image; this is a speed bump, as the guard
;;;; cell is (tools/nodecode-guard/cell.lisp). Loading the cell and the
;;;; extension is the grant: every session may drive Chrome.
;;;;
;;;; Every dependency named below is already in the serving image (a transitive
;;;; dependency of nodecode): LOAD-CELLS freezes the loaded systems
;;;; immutable and then loads this one at boot, so a dependency the image lacks
;;;; would have to compile from a vendor/ bundle or fail the folder.
;;;;
;;;; The .asd sits INSIDE its folder: a cell is a directory
;;;; carrying its .asd, and it is installed by putting — or symlinking — that
;;;; directory under ~/.nodecode/cells/. Presence is enabled; nothing in
;;;; config names it. The `just *` recipes register each cell
;;;; directory with ASDF the same way.

(defsystem "nodecode-chrome"
  :description "Drive Chrome; needs the companion extension"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "clack" "clack-handler-hunchentoot" "hunchentoot"
               "flexi-streams" "usocket" "dexador" "quri" "shasht" "cl-base64"
               "bordeaux-threads")
  :serial t
  :components ((:file "package")
               (:file "bridge")
               (:file "format")
               (:file "surface")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-chrome/test"))))

(defsystem "nodecode-chrome/test"
  :description "Chrome cell tests. Registered into the shared nodecode.test registry; RUN-CHROME-TESTS filters by the CHROME- name prefix."
  :license "MIT"
  :depends-on ("nodecode-chrome" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "bridge-test")
               (:file "format-test")
               (:file "surface-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chrome-tests)))
