;;;; package.lisp --- the TEAM package: the verbs, the prompt, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TEAM so an eval form reads (team:open ...). It is
;;;; meant to be used QUALIFIED only: OPEN shadows its CL namesake, and a
;;;; package that (:use)d this one would inherit that clash. Inside this
;;;; package a stream is opened with CL:OPEN or WITH-OPEN-FILE.
;;;;
;;;; Every exported verb answers a STRING and every refusal is TEAM-ERROR,
;;;; rendered by the eval snippet as "ERROR: TEAM-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive
;;;; from the one `team' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :nodes), and *TEAM* below is the variable they land in.

(defpackage #:nodecode-team
  (:use #:cl)
  (:nicknames #:team)
  (:shadow #:open)
  (:export
   ;; verbs, every one a string
   #:open #:watch #:best #:stop
   ;; data, so a layer can reword it
   #:*prompt* #:*unscored* #:*continue*
   ;; the one condition
   #:team-error))

(in-package #:nodecode-team)

(nlk:define-peripheral team :not-running t)
