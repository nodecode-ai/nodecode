;;;; support.lisp --- experience test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Experience tests register into the SAME nodecode.test registry
;;;; under an EXPERIENCE-CELL- name prefix; RUN-EXPERIENCE-TESTS runs that
;;;; slice. Every test starts the cell over a fresh fixture folder (the
;;;; core's WITH-KNOWLEDGE-FIXTURE: a layer and its knowledge cell, a
;;;; scratch home whose use ledger the sightings land in), with no reflector
;;;; thread and no gateway: the :FRAME observer is funcalled by hand with a
;;;; NEXT the test controls, the reflector's steps (SETTLE-REFLECTION and
;;;; the rest) are called directly, and the one ingress, NLE:SUBMIT, is
;;;; stubbed to a log (WITH-SUBMIT-LOG).

(in-package #:nodecode.test)

(define-test-slice "experience" "EXPERIENCE-CELL-" :start nodecode-experience:start-cell)

(defmacro with-experience-runtime ((&rest settings) &body body)
  "Run BODY with the experience cell started over a fresh fixture folder
(SETTINGS its section members), no thread; stopped on unwind."
  `(with-knowledge-fixture ()
     (nlk:with-cleanup ((nodecode-experience::stop-cell))
       (let ((nodecode-experience::*workers* nil))
         (experience-start ,@settings))
       ,@body)))

(defmacro with-experience-store ((&rest settings) &body body)
  "WITH-EXPERIENCE-RUNTIME over SETTINGS, all of it over a temp store."
  `(with-temp-store () (with-experience-runtime ,settings ,@body)))

(defun experience-publish (session type turn-id &optional payload)
  "One TYPE fact of TURN-ID in SESSION through the installed :frame hook, its
NEXT answering :NEXT."
  (cell-publish "nodecode-experience"
                 (cell-frame-op type :session session :turn turn-id :payload payload) :next))

(defun experience-reflection (&rest keys)
  "A reflection of origin s-o under command c, started at 0; KEYS override."
  (apply #'nodecode-experience::make-reflection
         (append keys '(:origin "s-o" :command-id "c" :started 0))))

(defun experience-refusal (verb &rest args)
  "The text of the EXPERIENCE-ERROR that VERB signals for ARGS."
  (refusal-text experience:experience-error (apply verb args)))

(defmacro with-submit-log ((var) &body body)
  "Run BODY with NLE:SUBMIT stubbed: every call is pushed onto VAR as
(SESSION PROMPT COMMAND-ID SOURCE) and answers a :STARTED admission."
  `(let ((,var '()))
     (with-stubbed-fdefinition (nle:submit (session prompt &key command-id source &allow-other-keys)
                                 (push (list session prompt command-id source) ,var)
                                 (nlk::make-active-input-admission :disposition :started))
       ,@body)))

(defun experience-seed-definition (session-id name text)
  "One organism.definition_recorded fact into SESSION-ID's log, the
scribe's own shape: turn-less, so the turn's span says whose it is."
  (seed-definition-fact session-id name "DEFUN" text))
