;;;; cell.lisp --- the whole guard cell: rule data and one :tool hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A pre-tool-call guard, and the catalogue example of the generic hook
;;;; seam: the engine is the core waist's textrules, the interception is one
;;;; NLE:HOOK on the :TOOL point, and the durable denial record is the
;;;; ordinary TURN.TOOL_RESULT fact the refusal string journals as, recorded
;;;; failed through NLE:FAILURE. A refused config signals
;;;; NLK:CONFIG-REFUSAL, which the gateway's START-CELLS turns into one loud
;;;; warning with the gateway booting regardless.
;;;;
;;;; NOT a security boundary: containment is an environment gate
;;;; and eval has full host authority -- this is a speed bump.
;;;;
;;;; WHAT THE RULES READ. This is a MONO-TOOL organism: EVAL's
;;;; form carries file-edit payloads, comments and test strings beside the
;;;; commands it runs. A call is READ, not grepped: the spawn calls
;;;; ((sh ...), run-program, launch-program, run-shell-command)
;;;; are found in the form, each one's command is taken from its literal
;;;; arguments (a string, a ($ "name") the strings object resolves, or a
;;;; (list ...) of strings), and only that text meets the shell rules. A
;;;; command the form cannot show -- built from a variable, or the spawner
;;;; passed as a function -- falls back to the whole call, read loosely.
;;;; Without this, a comment naming a rule, a file name, or a test string
;;;; would be refused as if it were the command.
;;;;
;;;; A shell command is cut into the commands it runs (an unquoted ; & | or
;;;; newline separates them) and each command starts a line, so a rule reads a
;;;; command's own words and never its arguments': `rg -n sudo src' passes,
;;;; `cd x && sudo ls' does not.
;;;;
;;;; Config, a sibling top-level key next to `channels':
;;;;   "cells": ["nodecode-guard"],
;;;;   "guard": {"enabled": true,
;;;;             "deny":  [{"id": "...", "pattern": "...", "reason": "...",
;;;;                        "gated": false}],
;;;;             "allow": ["..."]}
;;;; `gated' means the rule reads the commands the call runs (the built-in
;;;; family's reading); an ungated rule reads the whole call. `allow' exempts
;;;; only the region it matches (span-scoped, waist/textrules.lisp).

(defpackage #:nodecode-guard
  (:use #:cl))

(in-package #:nodecode-guard)

;;; --- what a call runs ------------------------------------------------------

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defun operator-name (x)
  "X's symbol name, lowercased, or NIL. Symbols are compared by name so a
form read in any package is recognized."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun dollar-name (x)
  "The strings-object name a ($ \"name\") form reads, or NIL."
  (and (consp x)
       (equal "$" (operator-name (car x)))
       (stringp (second x))
       (second x)))

(defun literal-command (x strings)
  "The text X alone names as a command, or NIL when X hides it."
  (cond ((stringp x) x)
        ((dollar-name x) (and strings (gethash (dollar-name x) strings)))
        ((and (consp x)
              (member (operator-name (car x)) '("list" "quote") :test #'equal))
         ;; (list "rm" "-rf" "/") and '("rm" "-rf" "/") are argv, and what
         ;; separates a command from its flag there is a quote, not
         ;; whitespace; a live directory vanished proving it.
         (let ((items (if (equal "quote" (operator-name (car x))) (second x) (rest x))))
           (when (and items
                      (listp items)
                      (every (lambda (item) (or (stringp item) (dollar-name item))) items))
             (format nil "~{~a~^ ~}"
                     (mapcar (lambda (item) (or (dollar-name item) item)) items)))))
        (t nil)))

(defun spawn-call-command (call strings)
  "CALL's command text, or NIL when one of its arguments hides it."
  (let ((rest (rest call)) (parts '()) (hidden nil))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest)) ; a keyword and its value are settings
                 (let ((text (literal-command (first rest) strings)))
                   (cond (text (push text parts))
                         (t (setf hidden t)))
                   (setf rest (cdr rest)))))
    (when (and (not hidden) parts)
      (format nil "~{~a~^ ~}" (nreverse parts)))))

(defun call-commands (form-text strings)
  "The commands FORM-TEXT runs, and how to read them.
=> (values TEXT MODE): MODE :COMMAND when the form shows them, :LOOSE when
one is hidden behind a variable or a function, NIL when the form runs nothing."
  (let ((commands '()) (hidden nil) (saw nil))
    (labels ((walk (x)
               (cond ((consp x)
                      (let ((name (operator-name (car x))))
                        (when (member name +spawn-operators+ :test #'equal)
                          (setf saw t)
                          (let ((command (spawn-call-command x strings)))
                            (if command (push command commands) (setf hidden t)))))
                      (walk (car x))
                      (walk (cdr x)))
                     (t (when (member (operator-name x) +spawn-operators+ :test #'equal)
                          (setf saw t))))))
      (when (stringp form-text)
        (ignore-errors
          (with-input-from-string (in form-text)
            (let ((*read-eval* nil) (*package* (find-package :nodecode-guard)))
              (loop for form = (read in nil :eof)
                    until (eq form :eof)
                    do (walk form))))))
      (cond ((and saw (not hidden))
             (values (format nil "~{~a~^~%~}" (nreverse commands)) :command))
            (saw (values form-text :loose))
            (t (values nil nil))))))

(defparameter +backslash+ (code-char 92) "The shell's escape character.")
(defparameter +quote-chars+ (format nil "~c~c" (code-char 34) (code-char 39))
  "The characters that open and close a shell quotation.")
(defparameter +command-breaks+ ";|&"
  "An unquoted one of these starts the next command.")
(defparameter +token-breaks+
  (format nil " ~c~c~c~c~c()[]{};|&,"
          #\Tab #\Newline #\Return (code-char 34) (code-char 39))
  "What separates one token of a loosely read call from the next.")

(defun command-lines (text)
  "TEXT's commands, one per line and trimmed: an unquoted ; & | or newline
starts the next one, and a quoted one is part of the word it sits in."
  (let ((lines '()) (start 0) (i 0) (n (length text)) (quote nil))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c +backslash+) (incf i 2))
                     (quote (when (char= c quote) (setf quote nil)) (incf i))
                     ((position c +quote-chars+) (setf quote c) (incf i))
                     ((or (find c +command-breaks+) (char= c #\Newline))
                      (push (subseq text start i) lines)
                      (setf start (1+ i))
                      (incf i))
                     (t (incf i)))))
    (push (subseq text start) lines)
    (format nil "~{~a~^~%~}"
            (remove "" (mapcar (lambda (line)
                                  (string-trim (list #\Space #\Tab #\Return) line))
                                (nreverse lines))))))

(defun loose-lines (text)
  "TEXT tokenized broadly, every token on its own line: the reading a call
gets when its command is hidden, and the reading every call got before."
  (with-output-to-string (out)
    (loop for c across text
          do (if (find c +token-breaks+)
                 (terpri out)
                 (write-char c out)))))

(defun printed-forms (text)
  "TEXT's forms printed back with their comments gone, or TEXT itself when it
will not read."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil) (*package* (find-package :nodecode-guard)) (parts '()))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push (write-to-string form :readably t :case :downcase) parts))
          (format nil "~{~a~^ ~}" (nreverse parts))))
    (error () text)))

(defun carried-strings (strings)
  "Every value STRINGS carries, space separated, or the empty string."
  (if (hash-table-p strings)
      (with-output-to-string (out)
        (loop for value being the hash-values of strings
              do (format out " ~a" value)))
      ""))

(defun whole-call-text (form-text strings)
  "FORM-TEXT with its comments gone, plus every string the call carries: what
an ungated rule reads."
  (concatenate 'string (printed-forms form-text) (carried-strings strings)))


;;; --- the rule family -------------------------------------------------------

;; Every pattern starts at a command's first word: (?<![^\n]) is "the start of
;; the text or a line", so an argument that happens to spell a dangerous word is
;; never at a position a rule looks at. A known wrapper is stepped over (env
;; sudo rm), and so is a leading assignment.
(defparameter +command-start+ "(?<![^\\n])")
(defparameter +wrapper-prefix+
  "(?:(?:sudo|doas|env|nohup|exec|command|nice|time)\\s+|timeout\\s+\\S+\\s+|\\w+=\\S+\\s+)*")
(defparameter +device+ "(?:sd|nvme|disk|mmcblk|vd|xvd|hd)\\w*")
;; A critical path is what Claude Code refuses to remove in any mode: the
;; filesystem root, a top-level folder, home, and the directory a session runs
;; in or its parents -- never the scratch folder beside it.
(defparameter +critical-target+
  (concatenate 'string
               "(?:\\$\\{?HOME\\}?/?\\*?|~/?\\*?|/(?:\\*|\\.\\.?)?|"
               "/(?:home|root|usr|etc|var|opt|srv|boot|data|tmp|lib|bin|sbin)(?:/\\*|/\\.\\.?)?|"
               "/home/[\\w.@%-]+/?|\\.\\.?(?:/\\.\\.?)*(?:/\\*)?|\\*/?)"))
(defparameter +git-options+ "(?:\\s+-[\\w-]+(?:=\\S+|\\s+\\S+)?)*")

(defun anchored (body)
  "BODY as a rule that reads a command's first word."
  (concatenate 'string +command-start+ +wrapper-prefix+ body))

(defun git-command (body)
  "BODY as a rule about a command of GIT's, its own options stepped over."
  (anchored (concatenate 'string "git\\b" +git-options+ "\\s+" body)))

(defparameter +shell-rules+
  (list
   (list :id "rm-critical"
         :reason "deletion of a critical path (the root, a top-level folder, home, or a parent directory)"
         :pattern (anchored (format nil "(?:sudo\\s+|doas\\s+)?(?:rm|rmdir)\\b(?:\\s+-[\\w-]+)*\\s+(?:~a)(?=\\s|$)"
                                    +critical-target+)))
   (list :id "privilege" :reason "privilege escalation"
         :pattern (anchored "(?:sudo|doas)\\b"))
   (list :id "world-writable" :reason "world-writable permissions"
         :pattern (anchored "(?:chmod|chown)\\b[^\\n]*\\b777\\b"))
   (list :id "mkfs" :reason "filesystem creation"
         :pattern (anchored "mkfs(?:\\.[\\w-]+)?\\b"))
   (list :id "dd-to-device" :reason "dd onto a device node"
         :pattern (anchored (format nil "dd\\b[^\\n]*\\bof=/dev/~a" +device+)))
   (list :id "redirect-device" :reason "redirect onto a device node"
         :pattern (format nil ">\\s*/dev/~a" +device+))
   (list :id "curl-pipe-shell" :reason "download piped straight into a shell"
         :pattern (anchored
                   (concatenate 'string
                                "(?:curl|wget)\\b[^\\n]*\\n\\s*"
                                "(?:(?:sudo|doas|env|nohup|exec|command|nice|time)\\s+)*"
                                "(?:ba|z)?sh\\b")))
   (list :id "host-power" :reason "host power state"
         :pattern (anchored
                   "(?:shutdown|reboot|halt|poweroff)\\b|systemctl\\s+(?:poweroff|reboot|halt|shutdown)\\b"))
   (list :id "git-reset-hard" :reason "discards uncommitted work"
         :pattern (git-command "reset\\s+--?(?:[\\w-]+\\s+)*(?:hard|keep)\\b"))
   (list :id "git-clean" :reason "discards untracked files"
         :pattern (git-command "clean\\s+-[\\w-]*f[\\w-]*\\b"))
   (list :id "git-push-force" :reason "force push"
         :pattern (git-command "push\\b[^\\n]*(?:--force\\b|--force-with-lease\\b|\\s-f\\b|\\s+\\+\\S)"))
   (list :id "git-discard" :reason "discards working-tree changes"
         :pattern (git-command "(?:checkout|restore)\\b(?:\\s+--)?\\s+\\.(?=\\s|$)")))
  "The built-in family, read against the commands a call runs.")

(defparameter +fork-bomb-rule+
  (list :id "fork-bomb" :reason "fork bomb"
        :pattern ":\\(\\)\\s*\\{.*\\|.*&.*\\}\\s*;\\s*:")
  "Read against the whole call: its own separators are what it is made of.")

(nlk:define-peripheral guard :not-running t)

(defun compile-rules (values table)
  "The two rule sets the hook consults: :SPAWN reads the commands a call runs,
:ANYWHERE reads the whole call. `deny' entries join the set their `gated' names."
  (let ((gated '()) (ungated '()))
    ;; The deny array is objects, a shape the section declaration does not
    ;; carry, so it is read here; `allow' is declared and comes in VALUES.
    (loop for entry across (nlk:json-array table "deny")
          when (hash-table-p entry)
            do (let ((spec (list :id (nlk:config-string entry "id")
                                 :pattern (nlk:config-string entry "pattern")
                                 :reason (nlk:config-string entry "reason"))))
                 (if (nlk:config-boolean entry "gated" nil)
                     (push spec gated)
                     (push spec ungated))))
    (list :spawn (nlk:compile-text-rules
                  :rules (append +shell-rules+ (nreverse gated))
                  :allow (getf values :allow))
          :anywhere (nlk:compile-text-rules
                     :rules (cons +fork-bomb-rule+ (nreverse ungated))
                     :allow (getf values :allow)))))

(defun spawn-refusal (form-text strings)
  "The refusal the commands a call runs earn, or NIL."
  (multiple-value-bind (text mode) (call-commands form-text strings)
    (when text
      (nlk:text-rules-refusal (setting :spawn)
                              (if (eq mode :command) (command-lines text) (loose-lines text))))))

(defun refuse-or-run (op next)
  "The :TOOL point: a matched rule answers a refusal, the call's failure, and
NEXT never runs."
  (let* ((arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments)))
         (text (if (stringp form) form (format nil "~s" form)))
         (refusal (or (spawn-refusal text strings)
                      (nlk:text-rules-refusal (setting :anywhere)
                                              (whole-call-text text strings)))))
    (if refusal
        ;; The command is theirs to run.
        (nle:failure (format nil "ERROR: refused by nodecode-guard: ~a; the call did not run -- give the operator the command to run themselves" refusal))
        (funcall next op))))

(nle:define-cell guard
  (:section ("guard")
    (:guide "deny adds {id, pattern, reason, gated} rules; gated reads the commands a call runs, ungated the whole call; allow exempts the region it matches")
    ("allow" :list :doc "patterns whose matching region is exempt from every rule"))
  ;; The compiled engines are settings: one compile per start, and the hook
  ;; reads them off the running cell rather than closing over them.
  (:settings (lambda (values table) (append (compile-rules values table) values)))
  (:hook :tool #'refuse-or-run))
