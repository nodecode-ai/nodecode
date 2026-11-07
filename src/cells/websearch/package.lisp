;;;; package.lisp --- the WEB package: model-facing vocabulary and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed WEB so a EVAL form reads (web:search "...").
;;;; It is meant to be used QUALIFIED only: SEARCH shadows its CL namesake (the
;;;; model's verb), and a package that (:use)d this one would inherit that
;;;; clash. Inside this package, string searching spells CL:SEARCH.
;;;;
;;;; Every exported function returns a STRING — the eval snippet prints the value
;;;; with ~S, so a string reads as text and anything else as a printed object —
;;;; and every failure is WEBSEARCH-ERROR, which the eval snippet renders as
;;;; "ERROR: WEBSEARCH-ERROR: <detail>", the text the primer tells the model to
;;;; act on. The detail has been through REDACT: a configured api key never
;;;; reaches the model, whichever library's message carried it.

(defpackage #:nodecode-websearch
  (:use #:cl)
  (:nicknames #:web)
  (:shadow #:search)
  (:export
   ;; model-facing vocabulary (see the primer in cell.lisp)
   #:search #:fetch
   ;; the one condition
   #:websearch-error))

(in-package #:nodecode-websearch)

;;; --- the failure contract ---------------------------------------------------
;;; FAIL's detail goes through REDACT; page and provider text is always an
;;; argument to it, never the control string.

(nlk:define-peripheral websearch :not-running t :filter redact)

(defun redact (text)
  "TEXT with every configured api key replaced by [redacted]."
  (uiop:frob-substrings text
                        (remove "" (and *websearch* (mapcar #'cdr (getf *websearch* :keys)))
                                :test #'string=)
                        "[redacted]"))

(defmacro with-redacted-errors (&body body)
  "Every condition leaving BODY is a WEBSEARCH-ERROR whose text has been
through REDACT."
  ;; The turn's cancel condition passes untouched: Esc during a slow fetch is
  ;; the turn's unwind, not a web failure.
  `(handler-case (progn ,@body)
     (websearch-error (condition) (error condition))
     (nlk:turn-cancelled-condition (condition) (error condition))
     (error (condition)
       (fail "~a" (or (ignore-errors (princ-to-string condition))
                      (type-of condition))))))
