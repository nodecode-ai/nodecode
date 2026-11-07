;;;; package.lisp --- the EXPERIENCE package: vocabulary, settings, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed EXPERIENCE so an eval form reads
;;;; (experience:sight ...), and used qualified: its verbs are the cell's
;;;; own words, not Lisp's.
;;;;
;;;; Every exported function returns a STRING and every refusal is
;;;; EXPERIENCE-ERROR, rendered by the eval snippet as "ERROR: EXPERIENCE-ERROR:
;;;; <detail>".
;;;;
;;;; The state every file shares -- the running settings, the lock, and the
;;;; live tables the frame observer and the verbs both read -- is declared
;;;; here, ahead of every use, so no file references a special variable a
;;;; later file defines.

(defpackage #:nodecode-experience
  (:use #:cl)
  (:nicknames #:experience)
  (:export
   ;; model-facing vocabulary (see the primer in cell.lisp)
   #:sight
   #:sightings
   #:attention
   #:reflect
   ;; the one condition
   #:experience-error))

(in-package #:nodecode-experience)

;;; The lock is over the live tables.
(nlk:define-peripheral experience :not-running t :lock "experience" :recursive t)

;;; --- the live tables ----------------------------------------------------------
;;; Live-only: rebuilt empty at every start. The durable facts they index --
;;; sessions, turns, ledger lines -- outlive them.

(defvar *children* (make-hash-table :test 'equal :synchronized t)
  "Reflection session id -> (ORIGIN . TURN): the session and the turn a
child reflects on. A sighting from a child is attributed to its origin.")

(defvar *recorded* (make-hash-table :test 'equal :synchronized t)
  "Turn ids this cell recorded into a session (the recaps): their own
completion is never reflected on.")

(defvar *announced* (make-hash-table :test 'equal :synchronized t)
  "Reflection session id -> T while that reflection's recap is marked for
the operator (the ATTENTION verb): the recap is then posted to the room as
a note, not only recorded in the session.")

(defun ours-p (session-id)
  "True for a session this cell created: a reflection."
  (and (stringp session-id) (uiop:string-prefix-p "experience-" session-id)))

;;; --- the ingress ------------------------------------------------------------------
;;; The one seam that puts text into a session: a reflection into its child.
;;; Called on the reflector thread or a verb's caller thread, never on the
;;; publishing thread (the cron rule); a test stubs NLE:SUBMIT.

(defun submit-into (session prompt command-id id)
  "PROMPT into SESSION through the in-process ingress. => the disposition."
  (nlk:active-input-admission-disposition
   ;; Provenance: source `experience', id the origin session.
   (nle:submit session prompt :command-id command-id :source "experience" :source-id id)))
