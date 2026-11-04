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

(defun ask-in-composer (model ask &aux (editor (chat-model-editor model)))
  "Hand MODEL's composer ASK, a row's ask as its JSON: the composer wears its
label and the text typed there runs its line on Enter (SUBMIT-ASK); Esc puts
it away. A draft the composer held waits under the ask and comes back once
it is done (PUT-DRAFT-BACK)."
  ;; A dialog a trigger opened (the index's `$') stands over a prompt being
  ;; written: its ask must not cost that prompt.
  (setf (chat-model-composer-ask model)
        (list :label (or (json-value ask :text "label") "Text")
              :line (json-value ask :text "line")
              :placeholder (json-value ask :text "placeholder")
              :verb (or (json-value ask :text "verb") "run")
              :draft (editor-value editor)))
  (editor-reset editor)
  (show-status model nil))

(defun put-draft-back (model draft &aux (editor (chat-model-editor model)))
  "Put DRAFT, what MODEL's composer held before an ask took it, back in a
composer the ask's line left empty."
  (when (and (plusp (length (or draft "")))
             (zerop (length (editor-value editor))))
    (editor-insert editor draft)))

(defun submit-ask (model text &aux (ask (chat-model-composer-ask model)))
  "Run the ask MODEL's composer wears with TEXT, what was typed under its
label; an empty one says what it waits for. => (values handled cmd)."
  (if (zerop (length (nlk:trimmed (or text ""))))
      (progn (show-status model (format nil "Type the ~(~a~) first, or Esc" (getf ask :label)))
             (render-state model)
             (values t nil))
      (progn (setf (chat-model-composer-ask model) nil)
             (editor-reset (chat-model-editor model))
             (multiple-value-prog1 (submit-slash model (ask-line (getf ask :line) text) :dropdown nil)
               (put-draft-back model (getf ask :draft))))))

(defun put-ask-away (model)
  "Take the ask MODEL's composer wears off it: the draft it stood over comes
back, or with none, the text typed under it stays as a plain draft. => true
when there was one."
  (when-let (ask (shiftf (chat-model-composer-ask model) nil))
    (when (plusp (length (or (getf ask :draft) "")))
      (editor-reset (chat-model-editor model))
      (put-draft-back model (getf ask :draft)))
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

(define-bare-dialog "export" (session-id model)
  ;; The file is written where the shell runs, so a room has no export; a
  ;; session with no prompt has nothing to write, which the op says.
  (when (and model (not (home-screen-p model)))
    (let ((here (uiop:native-namestring (uiop:ensure-directory-pathname (workspace-directory)))))
      (list-dialog :title "Export as Markdown"
                   :summary "The prompts, the answers, the plans and a line per call, in a file named after the session"
                   :rows (list (run-row "Save it in this folder" (format nil "/export ~a" here)
                                        :detail here)
                               (ask-row "Save it somewhere else" "Path" "/export"
                                        :detail "a folder, or the file to write"
                                        :placeholder "A folder or a file, from this shell's folder"
                                        :verb "export"))))))

(define-bare-dialog "evict" (session-id model)
  ;; Half is what the command did typed alone before it had a dialog, so it
  ;; leads: /compact, Enter, still compacts.
  (list-dialog :title "Evict context"
               :summary "Drop the oldest turns from what the model is sent; the transcript keeps them"
               :rows (append
                      (loop for (percent label) in '((50 "Half the history")
                                                     (25 "A quarter of it")
                                                     (75 "Three quarters of it")
                                                     (100 "All of it that can go"))
                            collect (run-row label (format nil "/evict ~d" percent)
                                             :detail (format nil "~d% of the retained turns, oldest first" percent)))
                      (list (run-row "Through a turn you pick" "/context history"
                                     :detail "the retained turns, each with what evicting through it frees")))))

(define-bare-dialog "gateway" (session-id model)
  ;; The verbs of GATEWAY-TEXT a shell can run, and the channel walk; `run'
  ;; is the command line's own. A forced stop or restart is for a shell that
  ;; runs inside the gateway it would end, which the plain one refuses.
  (when model
    (list-dialog
     :title "Gateway"
     :summary "This organism kept running by the operating system: in the background, at every login"
     :rows (append
            (list (run-row "Where it stands" "/gateway status"
                           :detail "serving or not, what it keeps running, whether it starts at login")
                  (run-row "Turn it on" "/gateway on"
                           :detail "run it in the background from now on, and at every login")
                  (run-row "Restart it" "/gateway restart"
                           :detail "on this build, with the config as it is now")
                  (run-row "Turn it off" "/gateway off"
                           :detail "stop it, and stop starting it at login; this shell takes the organism over")
                  (run-row "Its recent log" "/gateway logs" :detail "the last 60 lines"))
            (loop for (id label) in '(("discord" "Discord") ("telegram" "Telegram") ("slack" "Slack"))
                  collect (run-row (format nil "Set up ~a" label) (format nil "/gateway setup ~a" id)
                                   :detail "the bot's token, its server, then its rooms and people"))
            (list (run-row "Restart it from inside it" "/gateway restart --force"
                           :detail "when this shell runs inside the gateway the restart ends")
                  (run-row "Turn it off from inside it" "/gateway off --force"
                           :detail "when this shell runs inside the gateway the stop ends"))))))

(define-bare-dialog "think" (session-id model)
  ;; A shell opens its own picker over the target it reads (SLASH-THINK-OP);
  ;; a room or the page gets the ladder of the session's model as rows. A
  ;; model with no ladder leaves the command its own answer, which says so.
  (unless model
    (bind (((provider model-id _ _ _ effort) (gateway-target (and (stringp session-id) session-id)))
           (ladder (effort-ladder provider model-id))
           (now (and ladder (effective-effort effort provider model-id))))
      (flet ((row (label line detail current)
               (run-row label line :detail detail :mark (if current "●" "") :mark-tone :accent)))
        (when ladder
          (list-dialog :title "Reasoning effort"
                       :summary (format nil "~a/~a, from the next turn, here and in every new session"
                                        provider model-id)
                       :rows (cons (row "Default" "/think default" "the model's own" (null now))
                                   (loop for rung in ladder
                                         collect (row rung (format nil "/think ~a" rung)
                                                      (if (equal rung "off") "answer without reasoning" "")
                                                      (equal rung now))))))))))
