;;;; cell.lisp --- a built-in skill, outside editable or ranked memory.
;;;; SPDX-License-Identifier: MIT
(defpackage #:nodecode-context (:use #:cl))
(in-package #:nodecode-context)

(defparameter *skill*
  "During substantial tool work, edit your context at useful checkpoints: after settling a subproblem (a level cleared, a fix proved) and before your final answer; not after every tool call, not while a subproblem is still open (if you must, :keep blocks you still use). Every tool result ends with its seq, [seq 57]. Edit in one call, without exporting, reading or printing anything: (context :drop '(12 . 56) :note \"…\") drops those blocks, (context :drop t :keep '(57) :note \"…\") keeps only seq 57. The note is your own words, kept at the end: constraints, decisions, verified state, open work, evidence pointers. Window headroom is not the test: kept blocks ride every request, so drop settled ones you will not reread; a drop is lossless, (recall-entry 57) reads one back. Correctness outranks size.")

(defun standing-skill (next session-id)
  "Always supply the full skill in protected standing context, never the ranked index."
  (acons "context" *skill*
         (remove "context" (funcall next session-id) :key #'car :test #'equal)))

(nle:hook 'nle::read-harness-sections "nodecode-context" #'standing-skill)
