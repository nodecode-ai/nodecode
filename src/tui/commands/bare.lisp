;;;; bare.lisp --- a command is its name: what one answers typed alone.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- a command is its name --------------------------------------------------
;;;
;;; Nothing is typed after a command's name. What a command took as words --
;;; a verb, an id, a flag, free text -- is a row of the dialog it answers
;;; typed alone (LIST-DIALOG, gateway.lisp): a row's :RUN names the line its
;;; Enter runs, a row's :ASK the one line of text a surface asks for under a
;;; label before its line runs (ASK-LINE). The lines are the command's own
;;; words, so its parser stays the one place a verb is read, and a key
;;; binding or a script that names a line still runs it; no surface shows
;;; them, suggests them or asks anyone to spell them.
;;;
;;; A builtin with no sheet of its own files the dialog here
;;; (DEFINE-BARE-DIALOG): a shell opens it as its picker and runs a row's
;;; line itself (OPEN-BARE-DIALOG), and SLASH answers it to a room or the
;;; web page, which run a row's line where the command ran. A cell's command
;;; answers the same LIST-DIALOG from its RUN called with no argument.

(defvar *bare-dialogs* (make-hash-table :test #'equalp)
  "Builtin name => the function of (SESSION-ID MODEL) answering the dialog it
opens typed alone (DEFINE-BARE-DIALOG).")

(defmacro define-bare-dialog (name (session-id model) &body body)
  "File BODY as the dialog the builtin NAME answers typed alone: SESSION-ID
the session it acts on, MODEL the shell that asks, NIL for a room or the
page. BODY answers a LIST-DIALOG, or NIL to leave the command its own bare
answer."
  `(setf (gethash ,name *bare-dialogs*)
         (lambda (,session-id ,model)
           (declare (ignorable ,session-id ,model))
           ,@body)))

(defun bare-args-p (args)
  "Whether ARGS, the tail a command line carried, is nothing."
  (zerop (length (nlk:trimmed (or args "")))))

(defun bare-dialog (name session-id &optional model)
  "The dialog the builtin NAME answers typed alone in SESSION-ID, asked by the
shell MODEL or by no shell, or NIL for a command that files none."
  (when-let (make (gethash name *bare-dialogs*))
    (funcall make session-id model)))

(defun run-row (label line &rest keys &key detail mark mark-tone key verb suffix)
  "A dialog row whose Enter runs LINE: LABEL what it says, KEYS the rest of
LIST-DIALOG's row vocabulary."
  (declare (ignore detail mark mark-tone key verb suffix))
  (list* :label label :run (list :enter line) keys))

(defun ask-row (label ask line &rest keys &key placeholder detail mark mark-tone key verb)
  "A dialog row whose Enter asks for one line of text under ASK, the label
the composer wears, then runs LINE with the text after it (ASK-LINE):
PLACEHOLDER what the empty field says, VERB the word Enter does there."
  (declare (ignore detail mark mark-tone key))
  (list* :label label
         :ask (list :label ask :line line :placeholder (or placeholder "") :verb (or verb ""))
         (loop for (name value) on keys by #'cddr
               unless (eq name :placeholder) append (list name value))))

(defun ask-line (line text)
  "The line an ask for LINE runs once TEXT is given: LINE, a blank, TEXT."
  (format nil "~a ~a" line (nlk:trimmed (substitute #\Space #\Newline (or text "")))))

(defun dialog-words (dialog)
  "DIALOG, a LIST-DIALOG, as the text a caller that draws no panel reads:
its title and summary, then a line a row."
  (format nil "~a~@[~%~a~]~{~%~a~}"
          (getf dialog :title)
          (json-value (getf dialog :summary) :text)
          (loop for row in (getf dialog :rows)
                for detail = (json-value (getf row :detail) :text)
                collect (format nil "- ~a~@[ -- ~a~]" (getf row :label) detail))))

;;; --- the shell --------------------------------------------------------------

(defun open-bare-dialog (model dialog)
  "Open DIALOG, a builtin's answer typed alone, as MODEL's picker: a row's
line runs in this shell, a row's ask moves to the composer. => (values NIL
NIL), the answer of a command whose panel opened."
  (open-answer-dialog model (json-object "dialog" (dialog-json dialog)) :here t)
  (values nil nil))

(defun ask-in-composer (model ask)
  "Hand MODEL's composer ASK, a row's ask as its JSON: the composer wears its
label and the text typed there runs its line on Enter (SUBMIT-ASK); Esc puts
it away."
  (setf (chat-model-composer-ask model)
        (list :label (or (json-value ask :text "label") "Text")
              :line (json-value ask :text "line")
              :placeholder (json-value ask :text "placeholder")
              :verb (or (json-value ask :text "verb") "run")))
  (editor-reset (chat-model-editor model))
  (show-status model nil))

(defun submit-ask (model text &aux (ask (chat-model-composer-ask model)))
  "Run the ask MODEL's composer wears with TEXT, what was typed under its
label; an empty one says what it waits for. => (values handled cmd)."
  (if (zerop (length (nlk:trimmed (or text ""))))
      (progn (show-status model (format nil "Type the ~(~a~) first, or Esc" (getf ask :label)))
             (render-state model)
             (values t nil))
      (progn (setf (chat-model-composer-ask model) nil)
             (editor-reset (chat-model-editor model))
             (submit-slash model (ask-line (getf ask :line) text) :dropdown nil))))

(defun put-ask-away (model)
  "Take the ask MODEL's composer wears off it, the text typed under it kept
as a plain draft. => true when there was one."
  (when-let (ask (shiftf (chat-model-composer-ask model) nil))
    (show-status model (format nil "~a put away" (getf ask :label)))
    t))

;;; --- the builtins' dialogs ---------------------------------------------------

(define-bare-dialog "rename" (session-id model)
  ;; The name is a fact the gateway holds; the shell reads its own copy.
  (let ((title (if model
                   (ignore-errors (session-display-title model :fallback ""))
                   (ignore-errors (nlk::session-title session-id)))))
    (list-dialog :title "Name this session"
                 :summary (if (plusp (length (or title "")))
                              (format nil "Now: ~a" title)
                              "No name yet")
                 :rows (list (ask-row "Type a new name" "Name" "/rename"
                                      :detail "one line, 64 characters at most"
                                      :placeholder "The session's new name" :verb "rename")
                             (run-row "The name the first prompt makes" "/rename default"
                                      :detail "give the made name back")))))

(define-bare-dialog "details" (session-id model)
  ;; The viewport is this shell's own; a room has none.
  (when model
    (let ((now (chat-model-tool-detail model)))
      (list-dialog :title "Viewport"
                   :summary "Ctrl+O switches them too"
                   :rows (loop for (mode label detail)
                                 in '((:standard "Standard"
                                       "each call as its name and a comment, input and output on a click")
                                      (:developer "Developer"
                                       "routine calls grouped, output previewed"))
                               collect (run-row label (format nil "/details ~(~a~)" mode)
                                                :detail detail
                                                :mark (if (eq mode now) "●" "")
                                                :mark-tone :accent))))))

(define-bare-dialog "gc" (session-id model)
  (list-dialog :title "Collections"
               :summary "What collecting garbage cost this image, and the spans it stalled"
               :rows (list (run-row "Show the counters" "/gc show"
                                    :detail "collections, their cost and the stalls they caused")
                           (run-row "Restart the window" "/gc reset"
                                    :detail "count the stalls again from now"))))
