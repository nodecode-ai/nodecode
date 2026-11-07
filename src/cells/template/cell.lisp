;;;; cell.lisp --- the peripheral's one file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package <SYSTEM-NAME>, and every contribution is
;;;; filed under that name downcased, which is this folder's name.
;;;;
;;;; Two declarations and nothing else. NLK:DEFINE-PERIPHERAL gives this
;;;; package its condition, FAIL, the settings variable *TEMPLATE* and
;;;; RUNNING-SETTINGS. NLE:DEFINE-CELL declares the config section, what the
;;;; folder contributes, and derives START-CELL, STOP-CELL and SETTING from
;;;; them: stop takes back exactly what start put in, a second start leaves
;;;; one of everything, and a start that fails halfway leaves nothing.
;;;; See src/CELLS.md, "Declaring a cell", and "Under it, and beside it"
;;;; for the lower-level primitives when the declaration does not fit.

(defpackage #:nodecode-template
  (:use #:cl))

(in-package #:nodecode-template)

(nlk:define-peripheral template :not-running t)

(defparameter *greeting* "hello from the template"
  "Data over code: what a layer would rebind lives in a DEFPARAMETER.")

(defun greet (args session-id)
  "One /template invocation => the answer's text."
  (declare (ignore session-id))
  (format nil "~a~@[ (~a)~], ~a times" *greeting* args (setting :times)))

(defun observe-tool (op next)
  "A :TOOL point hook: OP is (:name TOOL-NAME :arguments HASH :call-id ID)."
  ;; Rewrite it by rebuilding OP, veto it by not calling NEXT, or observe it
  ;; like this. The around shape is the whole interception vocabulary.
  (funcall next op))

(nle:define-cell template
  ;; The section, once, as data: the setup panel, the model's primer, the
  ;; report's line for an unconfigured folder and the typed values below all
  ;; derive from it. A member of the wrong shape refuses at start —
  ;; NLK:CONFIG-REFUSAL — and the gateway makes that the record's REFUSED
  ;; state and one standing notice naming this folder; (restart-cells)
  ;; tries again after the operator fixes it. A present section is enabled;
  ;; an explicit "enabled": false vetoes, and then nothing below installs.
  (:section ("template")
    (:guide "nothing here is required; times bounds how loudly the template greets")
    ("times" :integer :default 1 :min 1 :doc "how many times to say it"))
  (:hook :tool #'observe-tool)
  (:command "template" 'greet
            :description "The template cell says hello"
            :argument-hint "[NAME]")
  ;; Everything no clause covers — a directory, a lease, a worker — with its
  ;; own inverse recorded beside it. Something worth saying that is not a
  ;; refusal is (nle:notice text :key "nodecode-template"): every shell
  ;; renders it, and with a key it stands until replaced or cleared by a NIL.
  (:start (lambda ()
            (nle:notice (format nil "template: ~a" *greeting*)
                        :key "nodecode-template")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-template"))))))
