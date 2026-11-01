;;;; lint-structure.lisp --- structural lint for Common Lisp sources.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three checks, in order of strength:
;;;;
;;;;   1. READ every top-level form. An unbalanced file fails here. This is the
;;;;      reader's own verdict, not a character count, so strings, #\( literals,
;;;;      and comments cannot fool it.
;;;;
;;;;   2. Sibling alignment. Elements of one list that each begin a line should
;;;;      start at the same column. When they do not, the parens and the layout
;;;;      disagree, and the code runs at a nesting the author did not write.
;;;;
;;;;   3. An IS description that begins its own line. IS already reports
;;;;      (format nil "~s was false" FORM) when the description is omitted, so a
;;;;      description that could not fit beside its form buys a line of prose
;;;;      over a form the reader can already see. Either it fits on the form's
;;;;      line or it is not saying enough to cost a line.
;;;;
;;;; Check 2 is the reason this file exists. A CASE clause can read as a sibling
;;;; but evaluate as the body of the clause above it, which turns (:empty nil)
;;;; into a call to the undefined function :EMPTY on the first blank SSE line.
;;;; Check 1 cannot see that: adding a paren in one place and dropping one
;;;; elsewhere leaves the file balanced and readable. Alignment localises it,
;;;; because the misplaced clause is the one element of its list that starts a
;;;; line at a column none of its siblings use.
;;;;
;;;; Reformatting is deliberately not offered. PPRINT would discard every
;;;; comment in these files, and the comments carry the reasoning; a formatter
;;;; that loses them costs more than the misindentation it fixes.

(defpackage #:nodecode.lint
  (:use #:common-lisp)
  (:export #:lint-files #:main))

(in-package #:nodecode.lint)

;;; --- exemptions ----------------------------------------------------------

;;; LOOP's clauses are a keyword grammar, not evaluated siblings: `do (a)' on
;;; one line and `(b)' on the next are both elements of the same list at
;;; different columns by design, so alignment says nothing there.
(defparameter *unaligned-heads*
  '("LOOP" "LOOP-FINISH")
  "Heads whose element lists are not sibling lists at all.")

;;; In (handler-case FORM . clauses) the protected FORM is indented past the
;;; clauses; the two groups are siblings that are not meant to line up.
;;;
;;; For these, alignment is checked in the weaker form that still catches
;;; misnesting: the line-starting elements may occupy at most two columns, and
;;; every line at the deeper column must precede every line at the shallower
;;; one. A clause that fell into the wrong body breaks that ordering, or adds
;;; a third column, in every case seen so far.
(defparameter *distinguished-arg-heads*
  '("HANDLER-CASE" "HANDLER-BIND" "RESTART-CASE" "RESTART-BIND"
    "UNWIND-PROTECT" "MULTIPLE-VALUE-BIND" "DESTRUCTURING-BIND"
    "WITH-OPEN-FILE" "WITH-INPUT-FROM-STRING" "WITH-OUTPUT-TO-STRING"
    "WITH-SLOTS" "WITH-ACCESSORS" "WITH-LOCK-HELD" "WITH-STANDARD-IO-SYNTAX"
    "LET" "LET*" "FLET" "LABELS" "MACROLET" "SYMBOL-MACROLET"
    "DO" "DO*" "DOLIST" "DOTIMES" "PROG1" "MULTIPLE-VALUE-PROG1" "IS-VALUES" "IS-LET*"
    "CASE" "ECASE" "CCASE" "TYPECASE" "ETYPECASE" "LAMBDA"
    "WHEN" "UNLESS" "IF" "COND" "BLOCK" "CATCH" "RETURN-FROM" "EVAL-WHEN"
    "REGISTER-GROUPS-BIND")
  "Heads whose standard indentation puts distinguished arguments at a deeper
column than the body that follows.")

;;; This codebase defines its own body macros — WITH-STUB-PROVIDER,
;;; WITH-MOCK-SSE-STREAM, DEFINE-HOOK, DEFTEST — and a lint that had to be
;;; told about each one would fall behind the code it checks.
(defparameter *distinguished-arg-prefixes*
  '("WITH-" "WITHOUT-" "DEF" "DO-" "MAP-")
  "Head prefixes that follow the same convention by naming convention alone.")

(defun strip-package-qualifier (head)
  "TUI:DEFMESSAGE -> DEFMESSAGE."
  ;; The convention a head follows is a property of the macro, not of the
  ;; package it is read from.
  (subseq head (1+ (or (position #\: head :from-end t) -1))))

;;; --- lexer ---------------------------------------------------------------
;;; Enough of the reader's surface syntax to find where each list's elements
;;; begin: comments, strings, character literals, block comments, and |escaped
;;; symbols| all suppress the parens and tokens inside them.

(defstruct (frame (:conc-name frame-))
  (open-line 0 :type fixnum)
  (head nil)
  (want-head t)
  (elements 0 :type fixnum)
  (starts '()))

(defstruct (lexer (:conc-name lx-))
  (stack '())
  (in-string nil)
  (in-block-comment 0 :type fixnum)
  (in-bars nil)
  (findings '())
  (prose '()))

(defun two-group-offenders (starts &aux (columns (remove-duplicates (mapcar #'cdr starts)))
                                        (deep (reduce #'max columns))
                                        (shallow (reduce #'min columns)))
  "NIL when STARTS is a deeper group followed by a shallower group, else the
lines that break that shape."
  ;; This is the relaxed rule for heads with distinguished arguments. A third
  ;; column, or a deeper line appearing after the body has started, means a
  ;; form is nested somewhere its indentation does not admit.
  (cond
    ;; One column: perfectly aligned siblings.
    ((= deep shallow) '())
    ;; More than two columns: not the distinguished-argument shape.
    ((> (length columns) 2)
     (remove shallow starts :key #'cdr))
    ;; Two columns: every deep line must come before every shallow line.
    (t
     (remove shallow (nthcdr (1+ (position shallow starts :key #'cdr)) starts) :key #'cdr))))

(defun close-frame (lx &aux (frame (pop (lx-stack lx))))
  "Pop the innermost list and record a finding when its line-starting elements
disagree on column."
  (when frame
    (let* ((starts (reverse (frame-starts frame)))
           (head (frame-head frame))
           (name (and head (strip-package-qualifier head))))
      (when (and (> (length starts) 1)
                 (not (member name *unaligned-heads* :test #'equal)))
        (let ((bad (if (and name
                            (or (member name *distinguished-arg-heads* :test #'equal)
                                (some (lambda (prefix) (eql 0 (search prefix name)))
                                      *distinguished-arg-prefixes*)))
                       (two-group-offenders starts)
                       (remove (cdr (first starts)) starts :key #'cdr))))
          (when bad
            (push (list :open-line (frame-open-line frame)
                        :head head
                        :base-line (car (first starts))
                        :base-column (cdr (first starts))
                        :offenders bad)
                  (lx-findings lx))))))))

(defun lex-line (lx line lineno &aux (i 0) (len (length line)) seen-code)
  (flet ((element (start &optional headless)
           ;; HEADLESS: a list, string or character begins the element, so
           ;; the innermost list wants no head any more.
           (let ((frame (first (lx-stack lx))))
             (when frame
               (incf (frame-elements frame))
               (unless seen-code (push (cons lineno start) (frame-starts frame)))
               (when headless (setf (frame-want-head frame) nil))))
           (setf seen-code t))
         (next-is (c)
           (and (< (1+ i) len) (char= (char line (1+ i)) c))))
    ;; Every arm reads the character at I; the step past it is the loop's,
    ;; and an arm that consumes more moves I past the rest itself.
    (loop while (< i len)
          for ch = (char line i)
          do (cond
               ;; Inside a #| block comment: only |# ends it, and they nest.
               ((plusp (lx-in-block-comment lx))
                (cond ((and (char= ch #\|) (next-is #\#))
                       (decf (lx-in-block-comment lx)) (incf i))
                      ((and (char= ch #\#) (next-is #\|))
                       (incf (lx-in-block-comment lx)) (incf i))))
               ;; Inside a string: \ escapes, " ends.
               ((lx-in-string lx)
                (cond ((char= ch #\\) (incf i))
                      ((char= ch #\") (setf (lx-in-string lx) nil))))
               ;; Inside |escaped symbol|.
               ((lx-in-bars lx)
                (when (char= ch #\|) (setf (lx-in-bars lx) nil)))
               ;; A ; comment ends the line.
               ((char= ch #\;) (setf i len))
               ;; #\x character literal: the next char is data, even ( or ".
               ((and (char= ch #\#) (next-is #\\))
                (element i t)
                (incf i 2))
               ((and (char= ch #\#) (next-is #\|))
                (incf (lx-in-block-comment lx)) (incf i))
               ((char= ch #\")
                ;; Check 3. (IS FORM "description") puts the description
                ;; third; a description that starts the line is one IS never
                ;; needed, since it prints FORM itself when none is given.
                (let ((opens-line (not seen-code)))
                  (element i t)
                  (let ((frame (first (lx-stack lx))))
                    (when (and opens-line frame
                               (= (frame-elements frame) 3)
                               (equal (and (frame-head frame)
                                           (strip-package-qualifier (frame-head frame)))
                                      "IS"))
                      (push (cons lineno i) (lx-prose lx)))))
                (setf (lx-in-string lx) t))
               ((char= ch #\|)
                (element i)
                (setf (lx-in-bars lx) t))
               ;; Reader macro prefixes attach to the element that follows,
               ;; so they open the element but are not the element itself.
               ((member ch '(#\' #\` #\,))
                (element i))
               ((member ch '(#\( #\[))
                (element i t)
                (push (make-frame :open-line lineno) (lx-stack lx)))
               ((member ch '(#\) #\]))
                (setf seen-code t)
                (close-frame lx))
               ((member ch '(#\Space #\Tab #\Return)))
               (t
                (element i)
                (let* ((end (or (position-if (lambda (c)
                                               (member c '(#\Space #\Tab #\( #\) #\[ #\] #\" #\; #\')))
                                             line :start i)
                                len))
                       (text (string-upcase (subseq line i end)))
                       (frame (first (lx-stack lx))))
                  (when frame
                    (when (frame-want-head frame)
                      (setf (frame-head frame) text))
                    (setf (frame-want-head frame) nil))
                  (setf i (1- end)))))
             (incf i))))

;;; --- check 1: the reader's verdict ---------------------------------------

(defun read-check (path)
  "READ every top-level form in PATH."
  ;; Returns (values ok-p message).
  ;;
  ;; Symbols are interned into a throwaway package with *READ-SUPPRESS* on and
  ;; *READ-EVAL* off: this reads shape, it does not load code, so a file that names
  ;; packages it does not itself define still reads.
  (let ((scratch (or (find-package :nodecode.lint.scratch)
                     (make-package :nodecode.lint.scratch
                                   :use '(:common-lisp)))))
    (handler-case
        (with-open-file (in path :direction :input)
          (let ((*package* scratch)
                (*read-eval* nil)
                (*read-suppress* t))
            (values t (format nil "~d top-level form~:p"
                              (loop for form = (read in nil in) until (eq form in) count t)))))
      (end-of-file ()
        (values nil "unbalanced parens: end of file reached inside a form"))
      (reader-error (e)
        (values nil (format nil "reader error: ~a" e)))
      (error (e)
        (values nil (format nil "~a: ~a" (type-of e) e))))))

;;; --- driver: check 2, sibling alignment, runs on what check 1 read --------

(defun lint-file (path &key (max-findings 10) &aux (lx (make-lexer)))
  "Lint PATH. Returns T when it passes both checks."
  (multiple-value-bind (ok message) (read-check path)
    (unless ok
      (format t "~&FAIL ~a~%       ~a~%" path message)
      (return-from lint-file nil))
    (with-open-file (in path :direction :input)
      (loop for lineno from 1
            for line = (read-line in nil nil)
            while line
            do (lex-line lx line lineno)))
    ;; Any frame still open at EOF is check 1's problem, not check 2's.
    (let ((findings (sort (reverse (lx-findings lx)) #'<
                          :key (lambda (f) (getf f :open-line))))
          (prose (reverse (lx-prose lx))))
      (when (and (null findings) (null prose))
        (format t "~&ok   ~a (~a)~%" path message)
        (return-from lint-file t))
      (format t "~&FAIL ~a (~a)~%" path message)
      (when prose
        (format t "       ~d IS description~:p begin a line of their own; IS prints the form itself~%"
                (length prose))
        (loop for (lineno . column) in prose
              for shown from 0 below max-findings
              do (format t "         line ~d column ~d~%" lineno column))
        (when (> (length prose) max-findings)
          (format t "         ... and ~d more~%" (- (length prose) max-findings))))
      (when findings
        (format t "       parens and indentation disagree~%"))
      (loop for f in findings
            for shown from 0 below max-findings
            do (destructuring-bind (&key open-line head base-line base-column offenders) f
                 (format t "       (~a at line ~d): siblings start at column ~d (line ~d), but~%"
                         (or head "list") open-line base-column base-line)
                 (loop for (lineno . column) in offenders
                       do (format t "         line ~d starts at column ~d~%" lineno column))))
      (when (> (length findings) max-findings)
        (format t "       ... and ~d more~%" (- (length findings) max-findings))))))

(defun lint-files (paths)
  "Lint PATHS. Returns the number that failed."
  (count-if-not #'lint-file paths))

(defun main (&aux (args (rest sb-ext:*posix-argv*)))
  (when (null args)
    (format *error-output*
            "~&usage: sbcl --script src/tools/check/lint-structure.lisp FILE...~%")
    (sb-ext:exit :code 2))
  (let ((failed (lint-files args)))
    (format t "~&~d file~:p checked, ~d failed~%" (length args) failed)
    (sb-ext:exit :code (if (plusp failed) 1 0))))

(main)
