;;;; shell.lisp --- the shell grammar: a command string as the simple
;;;; commands sh would run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One authority for what (sh ...) hands sh -c: tool-summary.lisp reads it to
;;;; name what a transcript card should say of the command. Not a shell: it
;;;; splits far enough to name each command's program, words, redirects and
;;;; here-documents, and marks a command substitution as one that may do
;;;; anything. Pure: no I/O, no state.

(in-package #:nodecode.evolved)

;;; --- the shell command ------------------------------------------------------
;;; A command string split the way sh would split it, far enough to name the
;;; program each command runs and the files it reads or writes. Not a shell:
;;; quoting, escapes, operators, redirections and here-documents, and any
;;; command substitution marks the command as one that may do anything.

(defstruct (shell-command (:constructor make-shell-command
                              (words redirects substitution start end)))
  "One simple command: its WORDS with quotes removed, its REDIRECTS as (OP .
TARGET), SUBSTITUTION true when a $( ) or backquote ran something inside it,
and its START and END offsets in the command string."
  words redirects substitution start end)

(defun shell-pipelines (command &aux (pos 0)
                                     (len (length command))
                                     (pipelines '())
                                     (pipeline '())
                                     (words '())
                                     (redirects '())
                                     (substitution nil)
                                     (word nil)
                                     (start nil)
                                     (pending nil)
                                     (heredocs '())
                                     (head t)
                                     (closers '())
                                     (nested 0))
  "COMMAND as a list of pipelines, each a list of SHELL-COMMANDs in pipe
order."
  ;; Lists split on ; & && || and newlines; a here-document's body lines are
  ;; skipped, never read as commands. Never signals: an unterminated quote
  ;; closes where the text stops.
  ;;
  ;; A compound command -- for, select, while and until ... done, if ... fi,
  ;; case ... esac, { ... }, ( ... ) -- is one command: the operators inside it
  ;; end words, not commands, until the word that closes it. Without this, a
  ;; compound command would be read as separate commands, its `do' and `done'
  ;; among them. HEAD is whether the next word stands where a command's name
  ;; does, the only place a reserved word is one; CLOSERS the words that close
  ;; the compounds open here, innermost first; NESTED the parens open inside a
  ;; $( ), $(( )) or <( ) substitution, whose operators are its own text.
  (labels ((ch (i) (and (< i len) (char command i)))
           (out ()
             (unless start (setf start pos))
             (or word (setf word (make-string-output-stream))))
           (reserve (text)
             ;; A reserved word opens a compound, closes the innermost one, or
             ;; hands the command position on; any other word takes it.
             (let ((closer (and head (cdr (assoc text '(("for" . "done") ("select" . "done")
                                                        ("while" . "done") ("until" . "done")
                                                        ("if" . "fi") ("case" . "esac") ("{" . "}"))
                                                 :test #'string=)))))
               (cond ((plusp nested))
                     (closer (push closer closers)
                             ;; a for's name and a case's word stand before its body
                             (setf head (not (member text '("for" "select" "case") :test #'string=))))
                     ((and head (equal text (first closers))) (pop closers))
                     ((and head (member text '("do" "then" "else" "elif" "!") :test #'string=)))
                     (t (setf head nil)))))
           (separate (width flush)
             ;; An operator WIDTH characters long: inside a substitution it is
             ;; the substitution's text, inside a compound it ends a word and the
             ;; next word is a command's name, else it ends the command (FLUSH).
             ;; The word before it is read first: `done;' closes its loop.
             (cond ((plusp nested) (write-string command (out) :start pos :end (min len (+ pos width))))
                   (t (flush-word)
                      (if closers (setf head t) (funcall flush))))
             (incf pos width))
           (flush-word (&optional descriptor-p)
             ;; DESCRIPTOR-P: a word of digits right before a redirect operator
             ;; is its file descriptor, not an argument.
             (when word
               (let ((text (get-output-stream-string word)))
                 (setf word nil)
                 (unless (and descriptor-p (null pending) (every #'digit-char-p text))
                   (cond ((null pending) (push text words) (reserve text))
                         ((member pending '("<<" "<<-") :test #'equal)
                          (push (cons text (equal pending "<<-")) heredocs))
                         (t (push (cons pending text) redirects)))
                   (setf pending nil)))))
           (flush-command ()
             (flush-word)
             (when (or words redirects)
               (push (make-shell-command (nreverse words) (nreverse redirects)
                                         substitution start pos)
                     pipeline))
             (setf words '() redirects '() substitution nil start nil
                   pending nil head t))
           (flush-pipeline ()
             (flush-command)
             (when pipeline (push (nreverse pipeline) pipelines))
             (setf pipeline '()))
           (substitution-at-p (i)
             (or (eql (ch i) #\`)
                 (and (eql (ch i) #\$) (eql (ch (1+ i)) #\())))
           (keep () (write-char (char command pos) (out)) (incf pos)))
    (loop while (< pos len)
          do (let ((c (char command pos)))
               (cond
                 ((char= c #\Newline)
                  (separate 1 #'flush-pipeline)
                  (when heredocs
                    (dolist (heredoc (reverse heredocs))
                      (loop while (< pos len)
                            do (let* ((eol (or (position #\Newline command :start pos) len))
                                      (line (subseq command pos eol)))
                                 (setf pos (min len (1+ eol)))
                                 (when (string= (car heredoc)
                                                (if (cdr heredoc)
                                                    (string-left-trim '(#\Tab) line)
                                                    (string-right-trim '(#\Return) line)))
                                   (return)))))
                    (setf heredocs '())))
                 ((member c '(#\Space #\Tab #\Return))
                  (flush-word)
                  (incf pos))
                 ((and (char= c #\#) (null word))
                  (setf pos (or (position #\Newline command :start pos) len)))
                 ((char= c #\')
                  (let ((close (position #\' command :start (1+ pos))))
                    (write-string command (out) :start (1+ pos) :end (or close len))
                    (setf pos (if close (1+ close) len))))
                 ((char= c #\")
                  (let ((stream (out)))
                    (incf pos)
                    (loop while (< pos len)
                          do (let ((d (char command pos)))
                               (cond ((char= d #\") (incf pos) (return))
                                     ((and (char= d #\\)
                                           (member (ch (1+ pos)) '(#\" #\\ #\$ #\`)))
                                      (write-char (ch (1+ pos)) stream)
                                      (incf pos 2))
                                     (t (when (substitution-at-p pos)
                                          (setf substitution t))
                                        (keep)))))))
                 ((char= c #\\)
                  (let ((stream (out))
                        (next (ch (1+ pos))))
                    (when (and next (char/= next #\Newline))
                      (write-char next stream))
                    ;; A trailing backslash escapes nothing, so the step past the
                    ;; character it would escape clamps to the command's end — an
                    ;; END one past the text is a stage every reader slices by.
                    ;; Without the clamp, SHELL-COMMAND-ENTRIES would signal
                    ;; SUBSEQ out of bounds; the scanner owns its offsets.
                    (setf pos (min len (+ pos 2)))))
                 ((substitution-at-p pos)
                  (setf substitution t)
                  (keep))
                 ;; The parens of a $( ), a $(( )) or a <( ) nest inside it, never
                 ;; a compound's.
                 ((and (char= c #\() (or (plusp nested)
                                         (and (plusp pos) (member (ch (1- pos)) '(#\$ #\< #\>)))))
                  (incf nested)
                  (keep))
                 ((and (char= c #\)) (plusp nested))
                  (decf nested)
                  (keep))
                 ((and (char= c #\() head (null word))
                  (unless start (setf start pos))
                  (push "(" words)
                  (push ")" closers)
                  (incf pos))
                 ;; A ) closes a subshell, or inside a case ends a pattern; either
                 ;; way a command's name comes next.
                 ((and (char= c #\)) closers)
                  (flush-word)
                  (when (equal ")" (first closers)) (pop closers))
                  (setf head t)
                  (incf pos))
                 ((and (char= c #\&) (eql (ch (1+ pos)) #\>))
                  (flush-word)
                  (unless start (setf start pos))
                  (incf pos 2)
                  (when (eql (ch pos) #\>) (incf pos))
                  (setf pending ">"))
                 ((and (member c '(#\& #\|)) (eql (ch (1+ pos)) c))
                  (separate 2 #'flush-pipeline))
                 ((member c '(#\; #\&)) (separate 1 #'flush-pipeline))
                 ((char= c #\|) (separate 1 #'flush-command))
                 ((member c '(#\> #\<))
                  (flush-word t)
                  (unless start (setf start pos))
                  (let ((op (or (find-if (lambda (op)
                                           (string= op command :start2 pos
                                                               :end2 (min len (+ pos (length op)))))
                                         '("<<<" "<<-" "<<" ">>" ">&" ">|" "<&" "<|"))
                                (string c))))
                    (incf pos (length op))
                    (let ((from pos))
                      ;; Descriptor duplication: 2>&1, >&2, <&- — no
                      ;; file. With no descriptor after it, >&word is
                      ;; bash's other spelling of &>word, and /bin/sh
                      ;; is bash where the operator runs: the next
                      ;; word is the file.
                      (when (member op '(">&" "<&") :test #'string=)
                        (loop while (and (ch pos) (or (digit-char-p (ch pos)) (char= (ch pos) #\-)))
                              do (incf pos)))
                      (when (= from pos) (setf pending op)))))
                 (t (keep)))))
    (flush-pipeline)
    (nreverse pipelines)))

(defun shell-flag-p (word)
  (and (> (length word) 1) (char= (char word 0) #\-)))

(defun shell-program-name (word)
  "WORD's program name: its last path component, so /usr/bin/rg is rg."
  (subseq word (1+ (or (position #\/ word :from-end t) -1))))

(defun shell-strip-prefix (words)
  "WORDS past the environment assignments and the wrappers that run the
command after them (env, command, time, timeout 60, nice -n 5)."
  (loop for word = (first words)
        do (cond ((null word) (return words))
                 ((ppcre:scan "^\\w+=" word) (pop words))
                 (t
                  (let ((wrapper (assoc (shell-program-name word)
                                        '(("env" "-u" "-C") ("command") ("time") ("nice" "-n")
                                          ("nohup") ("stdbuf") ("timeout" "-s" "-k") ("exec"))
                                        :test #'string=)))
                    (unless wrapper (return words))
                    (pop words)
                    (loop while (and words (shell-flag-p (first words)))
                          do (let ((flag (pop words)))
                               (when (and (member flag (cdr wrapper) :test #'string=)
                                          (not (find #\= flag)))
                                 (pop words))))
                    ;; timeout's duration is its one positional before the command.
                    (when (and words (string= (car wrapper) "timeout"))
                      (pop words)))))))
