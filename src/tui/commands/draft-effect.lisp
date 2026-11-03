;;;; draft-effect.lisp --- the draft an answer leaves in the input box (#199). Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; A cell's command may read the input box (REGISTER-COMMAND :READS
;;; (:DRAFT)) and say what the box holds next: a (:draft MODE TEXT) effect
;;; in its answer or on a row of its panel (GATEWAY.LISP ANSWER-EFFECT-JSON).
;;; The box is this shell's, so this shell applies it; a caller with no box
;;; reads the effect's fallback in the answer's text instead. Together they
;;; make a stash: set the draft aside, bring it back later.

(defun apply-draft-effect (model mode text)
  "Put TEXT in MODEL's input box as MODE says: :SET replaces the draft,
:INSERT puts TEXT at the caret, :APPEND after the draft's last line; the
caret is left after TEXT."
  (let ((editor model.editor))
    (ecase mode
      (:set (editor-reset editor))
      (:insert)
      (:append (setf editor.row (1- (editor-line-count editor)))
       (editor-cursor-end editor)))
    (editor-insert editor (or text ""))
    ;; An edit like any other: a history walk ends, the menu re-homes.
    (reset-history-navigation model)
    (setf (chat-model-slash-selection model) 0)
    model))

(define-answer-effect "draft" (model effect)
  ;; A mode this shell does not know leaves the box as it is.
  (when-let (mode (find (json-value effect :string "mode") '(:set :insert :append)
                        :key #'string-downcase :test #'equal))
    (apply-draft-effect model mode (or (json-value effect :string "text") ""))))
