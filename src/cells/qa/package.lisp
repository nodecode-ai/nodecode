;;;; package.lisp --- the QA package: the lock and the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed QA so an eval form reads (qa:show).
;;;; Every exported verb answers a STRING and every refusal is
;;;; QA-ERROR, rendered by the eval snippet as "ERROR: QA-ERROR:
;;;; <detail>" — the memory cell's contract.
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive
;;;; from the one `qa' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :share), and *QA* below is the variable they land in.

(defpackage #:nodecode-qa
  (:use #:cl)
  (:nicknames #:qa)
  (:export
   ;; verbs, every one a string
   #:show #:send #:notes #:push-notes #:clear-notes
   ;; the one condition
   #:qa-error))

(in-package #:nodecode-qa)

;;; The lock is over the two state rows.
(nlk:define-peripheral qa :not-running t :lock "qa")
