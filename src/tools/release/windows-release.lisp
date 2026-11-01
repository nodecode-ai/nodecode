;;;; windows-release.lisp --- the Windows arm of `just release'.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by tools/release/build-windows.ps1 in a fresh SBCL whose working directory
;;;; is src/: the system through quicklisp (pinned to the dist the Linux
;;;; build compiles against), then program-op on nodecode/bin, the
;;;; same dump the justfile makes. The --dynamic-space-size the script passes
;;;; is load-bearing exactly as it is there: the dump saves its runtime
;;;; options, so the organism's heap ceiling is set at the build.

(require :asdf)

(defparameter *quicklisp-dist*
  "http://beta.quicklisp.org/dist/quicklisp/2026-01-01/distinfo.txt"
  "The dist every build resolves dependencies from, on every platform.")

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (load (or (probe-file setup)
            (error "quicklisp is not installed for this user (~a); build-windows.ps1 installs it"
                   setup))))

(let ((installed (uiop:symbol-call :ql-dist :find-dist "quicklisp")))
  (unless (and installed (equal (uiop:symbol-call :ql-dist :version installed) "2026-01-01"))
    (uiop:symbol-call :ql-dist :install-dist *quicklisp-dist* :replace t :prompt nil)))

(push (truename "./") asdf:*central-registry*)
(push (truename "cells/context/") asdf:*central-registry*)
;; Quickload the system itself, not a hand-kept dependency list: the
;; central registry carries this tree, the pinned dist everything under it.
(uiop:symbol-call :ql :quickload "nodecode" :silent t)
(asdf:make :nodecode/bin)
(format t "~&windows-release: dumped ~a~%"
        (first (asdf:output-files 'asdf:program-op :nodecode/bin)))
