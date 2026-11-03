;;;; questions-dialog.lisp --- the organism's questions on a terminal shell. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A turn may stop and ask the person watching its session (questions.lisp
;;;; ASK-OPERATOR): whether to trust a folder's skills, a guard's Allow or
;;;; Deny. The web page and the desktop app put up every session's question;
;;;; a terminal shell shows one session, so its subscribe says it answers its
;;;; own ("answers_own") and it puts up the oldest open question of the
;;;; session it shows as a docked dialog: the question's detail lines above,
;;;; one row per choice. Enter sends the choice (answer_question); a board
;;;; that no longer holds the question takes the dialog down. Esc hides it,
;;;; and the turn waits on until the question expires or the turn is stopped.

(in-package #:nodecode.evolved)

(defvar *question-dialogs* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "A dialog this shell put up for a question -> that question's id.")

(defun session-questions (model payload &aux (session-id (chat-model-session-id model)))
  "The open questions of PAYLOAD, an organism.questions board, that belong
to MODEL's session, oldest first."
  (loop for question across (or (json-value payload :array "questions") #())
        when (and (hash-table-p question)
                  (equal session-id (json-value question :string "session_id")))
          collect question))

(defun open-question-dialog (model question)
  "Put QUESTION up over MODEL as a dialog: the first line of its detail
under the title, the rest as headings, a row for each choice, the first
choice selected."
  (let* ((id (json-value question :string "question_id"))
         (session-id (json-value question :string "session_id"))
         (lines (remove "" (uiop:split-string (or (json-value question :string "detail") "")
                                              :separator '(#\Newline))
                        :test #'string=))
         (choices (coerce (or (json-value question :array "choices") #()) 'list)))
    (open-picker-dialog model (or (json-value question :string "title") "A question")
                        (append (mapcar (lambda (line) (make-picker-row :label line :heading t)) (rest lines))
                                (mapcar (lambda (choice) (make-picker-row :label choice :value choice))
                                        choices))
                        (lambda (model choice)
                          (send-gateway-command (gateway-model-ws model) :answer-question
                                                (json-object "session_id" session-id
                                                             "question_id" id
                                                             "answer" choice))
                          nil)
                        :kind :question
                        :subtitle (or (first lines) "")
                        :selection (length (rest lines))
                        :hint "enter answer  esc hide  ↑↓ choose")
    (setf (gethash (chat-model-dialog model) *question-dialogs*) id)))

(defun show-session-questions (model payload &aux (dialog (chat-model-dialog model)))
  "Fold PAYLOAD, an organism.questions board, into MODEL: the oldest open
question of its session goes up unless it is up already, and a question
dialog whose question the board no longer holds comes down."
  (let* ((questions (session-questions model payload))
         (shown (and dialog (gethash dialog *question-dialogs*))))
    (cond ((and shown (find shown questions :test #'equal
                                            :key (lambda (question)
                                                   (json-value question :string "question_id")))))
          (questions (open-question-dialog model (first questions)))
          (shown (dismiss-dialog model nil)))
    (render-state model)))
