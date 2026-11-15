;;;; local-command.lisp --- `!' in the composer: a command run where this shell runs. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; `!git status' and Enter runs `git status' on the machine this shell runs
;;; on, in its working directory -- the folder the @ catalog walks -- through
;;; the same sh -c every command of nodecode's own goes through (SHELL-ARGV).
;;; Its output fills a view as it comes, terminal controls removed as the
;;; exec view removes them (SANITIZE-OCTETS). Nothing of it reaches the model
;;; on its own: Enter in the view puts the command and its output in the
;;; composer, a [Paste #N] past the paste bounds, for the person to send with
;;; a question or to clear. This is the shell's own gesture, so no cell code
;;; runs here (#505); a gateway that runs elsewhere never sees the command.

(defparameter *local-command-kept-lines* 2000
  "The most output lines a `!' command keeps, its last ones: the view and
what Enter attaches. The lines before them are counted, not kept.")

(defstruct (local-command (:constructor %make-local-command (line directory)))
  (line "" :type string)
  (directory nil)
  (lock (bt2:make-lock :name "local-command"))
  (pid nil)
  ;; The kept lines, newest first, and how many were written in all.
  (lines '())
  (written 0 :type integer)
  ;; NIL while it runs; the exit status once it ended, 128 plus the signal
  ;; that ended it as WAIT-FOR-CHILD says, or the text of why it never ran.
  (exit nil)
  (stopped nil))

(nlk:access (bang local-command))

(defun draft-local-command (text)
  "The command TEXT, a composer's draft, asks to run here: what follows a
leading `!', trimmed, \"\" for a `!' alone; NIL when TEXT does not start
with one."
  (and (plusp (length text)) (char= #\! (char text 0))
       (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq text 1))))

(defun note-local-command-output (bang sanitizer text)
  "Keep TEXT, a line BANG's command wrote, through SANITIZER."
  (let ((clean (octets-to-text
                (sanitize-octets sanitizer (sb-ext:string-to-octets
                                            (uiop:strcat text (string #\Newline))
                                            :external-format '(:utf-8 :replacement #\?))))))
    (bt2:with-lock-held (bang.lock)
      (dolist (line (split-lines (string-right-trim '(#\Newline) clean)))
        (incf bang.written)
        (push line bang.lines)
        ;; Trimmed in steps, so a chatty command costs one copy per
        ;; *LOCAL-COMMAND-KEPT-LINES* lines rather than one per line.
        (when (> (length bang.lines) (* 2 *local-command-kept-lines*))
          (setf bang.lines (subseq bang.lines 0 *local-command-kept-lines*)))))))

(defun start-local-command (line &key (directory (uiop:getcwd)))
  "Start LINE through sh -c in DIRECTORY, its output read on a thread of its
own into the LOCAL-COMMAND this answers."
  (let ((bang (%make-local-command line directory)))
    (handler-case
        (multiple-value-bind (pid output errors process)
            (spawn-program (shell-argv line) :directory directory
                                             :output :stream :error-output :output :input :null)
          (declare (ignore errors))
          (setf bang.pid pid)
          (spawn "nodecode-local-command"
            (let ((sanitizer (make-text-sanitizer)))
              (unwind-protect
                   (ignore-errors
                    (loop for text = (read-line output nil)
                          while text do (note-local-command-output bang sanitizer text)))
                (ignore-errors (close output))
                (let ((status (or (ignore-errors (wait-for-child pid process)) 1)))
                  (bt2:with-lock-held (bang.lock) (setf bang.exit status)))))))
      (error (condition)
        (setf bang.exit (plain-error-text condition))))
    bang))

(defun stop-local-command (bang)
  "End BANG's command and everything it started, when it still runs."
  (when (and bang.pid (null bang.exit))
    (setf bang.stopped t)
    (kill-tree bang.pid)))

(defun local-command-state (bang)
  "BANG as it stands: (values LINES OMITTED EXIT), LINES oldest first."
  (bt2:with-lock-held (bang.lock)
    (let ((lines (reverse (subseq bang.lines 0 (min (length bang.lines) *local-command-kept-lines*)))))
      (values lines (- bang.written (length lines)) bang.exit))))

(defun local-command-exit-words (bang exit)
  "How BANG ended, EXIT its status: `exit 0', `stopped', or why it never ran."
  (cond ((stringp exit) exit)
        (bang.stopped "stopped")
        (t (format nil "exit ~d" exit))))

(defun local-command-attachment (bang)
  "The text Enter puts in the composer: the command, its kept output, and how
it ended when that was not exit 0."
  (multiple-value-bind (lines omitted exit) (local-command-state bang)
    (format nil "$ ~a~@[~%[~d earlier line~:p left out]~]~{~%~a~}~@[~%[~a]~]"
            bang.line (and (plusp omitted) omitted) lines
            (and (not (eql exit 0)) (local-command-exit-words bang exit)))))

(defun local-command-view-parts (bang)
  "BANG's view as it stands: (values ROWS CONTEXT HINTS DONE)."
  (multiple-value-bind (lines omitted exit) (local-command-state bang)
    (values (append (when (plusp omitted)
                      (list (make-picker-row :label (format nil "⋮ +~d line~:p hidden" omitted)
                                             :value bang :tone :muted)))
                    (loop for line in lines collect (make-picker-row :label line :value bang))
                    ;; Every key of the view needs a row to land on: Enter,
                    ;; Ctrl+X.
                    (when (and (null lines) (not (stringp exit)))
                      (list (make-picker-row :label (if exit "(no output)" "(no output yet)")
                                             :value bang :tone :muted)))
                    (when (stringp exit)
                      (list (make-picker-row :label (format nil "error: ~a" exit)
                                             :value bang :tone :danger))))
            (format nil " ~:[running~;~:*~a~] · ~d line~:p "
                    (and exit (local-command-exit-words bang exit)) (+ omitted (length lines)))
            (if exit
                '(("enter" . "attach") ("esc" . "close") ("up/down" . "scroll"))
                '(("ctrl+x" . "stop") ("esc" . "close and stop") ("up/down" . "scroll")))
            (and exit t))))

(defun refresh-local-command-view (model dialog bang)
  "Re-read BANG into DIALOG, keeping the last row highlighted while the reader
was already there. => T when the view moved."
  (multiple-value-bind (rows context hints done) (local-command-view-parts bang)
    (when done
      ;; Ended: nothing moves any more, so the view stops asking for ticks.
      (setf dialog.on-tick nil dialog.on-delete nil))
    (unless (and (equal (mapcar #'picker-row-label rows) (mapcar #'picker-row-label dialog.rows))
                 (equal context dialog.context)
                 (equal hints dialog.footer-hints))
      (let ((at-tail (>= dialog.selection (1- (length dialog.rows)))))
        (setf dialog.rows rows dialog.context context dialog.footer-hints hints)
        (when at-tail
          (setf dialog.selection (max 0 (1- (length rows))))))
      (when (eq dialog (chat-model-dialog model))
        (render-status-frame model))
      t)))

(defun run-local-command (model text &aux (line (draft-local-command text)))
  "Enter on TEXT, a draft that starts with `!': run its command here and
open the view on it. => NIL, the command the update runs after it."
  (when (zerop (length line))
    (show-status model "Type a command after ! to run it here" :error-p t)
    (return-from run-local-command nil))
  (note-submitted-prompt model text)
  (editor-reset (chat-model-editor model))
  (show-status model nil)
  (let* ((bang (start-local-command line))
         (dialog nil))
    (open-picker-dialog
     model "Command" '()
     (lambda (model bang)
       (if (null bang.exit)
           ;; Enter on a running command keeps the view: what it wrote so
           ;; far is not yet what it says.
           (progn (setf (chat-model-dialog model) dialog)
                  "Still running: Ctrl+X stops it")
           (progn (attach-composer-paste model (local-command-attachment bang))
                  (format nil "Attached: $ ~a" bang.line))))
     :kind :local-command
     :subtitle (format nil "$ ~a" line)
     :empty-label "No output yet"
     :on-cancel (lambda (model) (declare (ignore model)) (stop-local-command bang))
     :on-delete (lambda (model bang) (declare (ignore model))
                  (stop-local-command bang)
                  "Stopping")
     :on-tick (lambda (model dialog) (refresh-local-command-view model dialog bang)))
    (setf dialog (chat-model-dialog model))
    (refresh-local-command-view model dialog bang)
    nil))
