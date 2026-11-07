;;;; package.lisp --- the PRS package: model-facing vocabulary, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PRS so an EVAL form reads (prs:rank 12 :p1 ...).
;;;; It is meant to be used QUALIFIED only: MERGE shadows its CL namesake (the
;;;; model's verb for landing a PR), and a package that (:use)d this one would
;;;; inherit that clash. Inside this package, sequence merging spells CL:MERGE.
;;;;
;;;; Every exported verb returns a STRING -- the eval snippet prints the value
;;;; with ~S, so a string reads as text -- and every refusal is PRS-ERROR,
;;;; which the eval snippet renders as "ERROR: PRS-ERROR: <detail>". The detail
;;;; has been through REDACT: the token never reaches the model.

(defpackage #:nodecode-prs
  (:use #:cl)
  (:nicknames #:prs)
  (:shadow #:merge)
  (:export
   ;; model-facing vocabulary (see the primer in cell.lisp)
   #:queue #:show #:rank #:prep #:merge
   ;; the one condition
   #:prs-error))

(in-package #:nodecode-prs)

(nlk:define-peripheral prs :not-running t :lock "prs" :recursive t :filter redact)

(defun redact (text)
  "TEXT with the configured token replaced by [redacted]."
  (let ((token (and *prs* (getf *prs* :token))))
    (if (and token (plusp (length token)))
        (uiop:frob-substrings text (list token) "[redacted]")
        text)))
