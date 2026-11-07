;;;; package.lisp --- the CRON package: model-facing vocabulary, settings, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CRON so a EVAL form reads (cron:add ...). It
;;;; is meant to be used QUALIFIED only: REMOVE shadows its CL namesake (the
;;;; model's verb for taking a job away), and a package that (:use)d this one
;;;; would inherit that clash. Inside this package, sequence removal spells
;;;; CL:REMOVE.
;;;;
;;;; Every exported function returns a STRING -- the eval snippet prints the
;;;; value with ~S, so a string reads as text -- and every refusal is
;;;; CRON-ERROR, which the eval snippet renders as "ERROR: CRON-ERROR: <detail>",
;;;; the text the primer tells the model to act on.

(defpackage #:nodecode-cron
  (:use #:cl)
  (:nicknames #:cron)
  (:shadow #:remove)
  (:export
   ;; model-facing vocabulary (see the primer in cell.lisp)
   #:add #:jobs #:show #:edit #:pause #:resume #:remove #:run #:runs
   ;; the one condition
   #:cron-error))

(in-package #:nodecode-cron)

;;; The lock is recursive: a verb that fires a job publishes frames on its
;;; own thread, and the :FRAME hook takes the lock again underneath it.
(nlk:define-peripheral cron :not-running t :lock "cron" :recursive t)

