;;;; bare.lisp --- what a slash command opens when its name is all that was typed.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- a command with no arguments --------------------------------------------
;;;
;;; Every slash command is its name and nothing after it. The choices a command
;;; used to take as typed words -- a sub-verb, an id, a flag, a free-text
;;; argument -- are rows here: what the operator picks, not what the operator
;;; spells. One constructor answers the list dialog the shell opens as its
;;; docked picker (LIST-DIALOG), so a command writes its choices as data and
;;; the shell, a room and the page draw them alike.
;;;
;;; A row's :RUN is the command line Enter runs, spelled in full, exactly the
;;; verb+id the operator used to type, so the catalog's own commands do the
;;; work and a row never re-implements one.

(defun slash-choice-dialog (title choices &key context summary searchable hint empty tone)
  "The dialog a bare command opens: TITLE over CHOICES, each a plist of :LABEL
(the line the operator reads), :DETAIL (what it does, muted beside it), :KEY
(its key column), :MARK and :MARK-TONE (its state), and :RUN (the command
line Enter runs) or :VALUE (the text Enter puts in the composer). A :HEADING
row names the rows under it and is never run. => the LIST-DIALOG spec, ready
for the answer a shell opens as its picker."
  (list :title title
        :context (or context "")
        :summary summary
        :searchable searchable
        :hint hint
        :empty-label (or empty "Nothing to choose")
        :rows (mapcar (lambda (choice)
                        (let ((run (getf choice :run)))
                          (list :key (or (getf choice :key) "")
                                :label (or (getf choice :label) "")
                                :detail (or (getf choice :detail) "")
                                :mark (or (getf choice :mark) "")
                                :mark-tone (or (getf choice :mark-tone) :muted)
                                :value (getf choice :value)
                                :heading (and (getf choice :heading) t)
                                :run (and run (list :enter run :space run)))))
                      choices)))

(defun slash-choice (label &rest args)
  "One row of SLASH-CHOICE-DIALOG: LABEL plus :DETAIL, :KEY, :MARK, :RUN."
  (list* :label label args))

(defun slash-heading (label &rest args)
  "A CHOICES row that names the rows under it and is never run."
  (list* :label label :heading t args))

(defun slash-safe-word-p (word)
  "Whether WORD can follow a command name as one word: no blank, no quote and
no backslash, the four characters a command line cannot carry bare."
  (every (lambda (char)
           (not (member char '(#\Space #\Tab #\Return #\Newline #\" #\\))))
         word))

(defun slash-bare-line (name &rest words)
  "The command line a row runs: NAME and WORDS after it. A word that is not
SLASH-SAFE-WORD-P is left as it stands, so an id or a tail with a space in it
still reaches the command that knows how to read it."
  (format nil "/~a~{ ~a~}" name (mapcar (lambda (word) (princ-to-string word)) words)))
