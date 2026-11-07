;;;; package.lisp --- the CHROME package: model-facing vocabulary and conditions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CHROME so a EVAL form reads (chrome:snapshot).
;;;; It is meant to be used QUALIFIED only: TYPE, FILL and INSPECT shadow their
;;;; CL namesakes (pi-chrome's own verb names), and a package that (:use)d this
;;;; one would inherit that clash. Inside this package, declarations spell
;;;; CL:TYPE.
;;;;
;;;; Exports fall in three groups: the cell entry the loader finds by name;
;;;; the model-facing functions the harness primer teaches; and the conditions
;;;; whose printed names are part of that contract — the eval snippet renders a
;;;; signalled condition as "ERROR: CHROME-OFFLINE: ...", which is exactly the
;;;; text the primer tells the model to act on.

(defpackage #:nodecode-chrome
  (:use #:cl)
  (:nicknames #:chrome)
  (:shadow #:type #:fill #:inspect)
  (:export
   ;; model-facing vocabulary (see the primer in cell.lisp)
   #:send #:snapshot #:inspect #:click #:type #:fill #:key #:navigate #:wait-for
   #:evaluate #:screenshot #:tabs #:status
   ;; conditions
   #:chrome-error #:chrome-offline #:chrome-timeout #:chrome-command-failed))

(in-package #:nodecode-chrome)

(nlk:define-peripheral chrome :not-running t)

(define-condition chrome-offline (chrome-error) ()
  (:default-initargs
   :detail "The Chrome bridge is not running in this image (cell not started). Ask the user to run /chrome doctor.")
  (:documentation "No bridge to send through."))

(define-condition chrome-timeout (chrome-error) ()
  (:documentation "The extension did not answer within the send timeout; DETAIL classifies why."))

(nlk:define-error chrome-command-failed (chrome-error) (action)
  (:report (lambda (condition stream)
             (format stream "~a failed: ~a"
                     (chrome-command-failed-action condition)
                     (chrome-error-detail condition))))
  (:documentation "The extension answered ok:false; DETAIL is its error string."))
