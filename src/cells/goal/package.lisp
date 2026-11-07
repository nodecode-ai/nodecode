;;;; package.lisp --- the GOAL package: three verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed GOAL so an eval form reads (goal:start ...).
;;;; Every exported verb answers a STRING and every refusal is GOAL-ERROR,
;;;; rendered by the eval snippet as "ERROR: GOAL-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `goal' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :turns).

(defpackage #:nodecode-goal
  (:use #:cl)
  (:nicknames #:goal)
  (:export
   ;; verbs, every one a string
   #:start #:status #:stop
   ;; data, so a layer can reword it
   #:*kickoff-input* #:*continue-input* #:*judge-input* #:*attempt-input* #:*pick-input*
   ;; the one condition
   #:goal-error))

(in-package #:nodecode-goal)

(nlk:define-peripheral goal :not-running t :lock "goal" :recursive t)
