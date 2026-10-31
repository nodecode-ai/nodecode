;;;; toml.lisp --- a TOML config file as the decoded shape JSON takes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The other half of the ecosystem's config vocabulary: Codex writes
;;;; ~/.codex/config.toml, and several younger harnesses follow it. This
;;;; reads TOML v1.0 into the same shape yaml.lisp and shasht produce --
;;;; hash tables with string keys, vectors, strings, numbers, T, :FALSE and
;;;; :NULL -- so one shape classifier (shape.lisp) walks a .toml tree
;;;; exactly as it walks a .json or .yaml one, and NLK:JSON-VALUE reads it.
;;;;
;;;; What it takes: comments, bare and quoted keys, dotted keys, tables,
;;;; arrays of tables, basic and literal strings (multi-line included),
;;;; integers in every base, floats, booleans, arrays and inline tables.
;;;; Offset date-times and their kin come through as the strings they are
;;;; written as: nothing here compares them, and a string keeps the
;;;; operator's own text. A document that does not parse is an IMPORT-ERROR
;;;; carrying the line -- there is no per-section recovery here, because
;;;; TOML's own grammar gives a reader no way to resynchronize inside a
;;;; table it has lost its place in.

(in-package #:nodecode.import)

;;; --- the cursor ------------------------------------------------------------

(defstruct (toml (:constructor make-toml (text)) (:copier nil) (:predicate nil))
  "One document being read: TEXT and the character POS into it."
  text (pos 0))

(nlk:access (in toml))

(defun toml-end-p (in)
  (>= in.pos (length in.text)))

(defun toml-peek (in &optional (ahead 0) &aux (at (+ in.pos ahead)))
  (and (< at (length in.text)) (char in.text at)))

(defun toml-take (in &aux (ch (toml-peek in)))
  (when ch (incf in.pos))
  ch)

(defun toml-looking-at (in text &aux (end (+ in.pos (length text))))
  (and (<= end (length in.text)) (string= text in.text :start2 in.pos :end2 end)))

(defun toml-fail (in format-control &rest args)
  (fail "line ~d: ~a"
        (1+ (count #\Newline in.text :end (min in.pos (length in.text))))
        (apply #'format nil format-control args)))

(defmacro toml-scan (in regex)
  "What REGEX matches at IN's cursor, taken; NIL when it does not match there."
  `(let* ((in ,in)
          (end (nth-value 1 (ppcre:scan ,(format nil "\\A(?:~a)" regex) in.text :start in.pos))))
     (and end (subseq in.text (shiftf in.pos end) end))))

(defun toml-skip-blanks (in)
  "Spaces and tabs."
  (toml-scan in "[ \\t]*"))

(defun toml-skip-space (in &key newlines)
  "Blanks and comments; NEWLINES also crosses line ends, which is what an
array and an inline table's members may do."
  ;; A `#' comment to the end of its line, the newline left.
  (if newlines
      (loop while (toml-scan in "[ \\t\\r\\n]+|#[^\\n]*"))
      (toml-scan in "[ \\t]*(?:#[^\\n]*)?")))

(defun toml-end-of-line (in)
  "Blanks, a comment, then the line end — anything else on the line is a refusal."
  (loop do (toml-skip-space in) while (toml-scan in "\\r"))
  (unless (or (toml-end-p in) (toml-scan in "\\n"))
    (toml-fail in "~s where the line should end" (toml-peek in))))

;;; --- strings ---------------------------------------------------------------

(defun toml-escape (in &aux (ch (toml-take in)))
  "The character after a backslash in a basic string."
  (case ch
    (#\b #\Backspace) (#\t #\Tab) (#\n #\Newline) (#\f #\Page) (#\r #\Return)
    (#\" #\") (#\\ #\\)
    ((#\u #\U)
     (let* ((width (if (char= ch #\u) 4 8))
            (end (+ in.pos width)))
       (unless (<= end (length in.text))
         (toml-fail in "a truncated \\~a escape" ch))
       ;; Hex digits only (PARSE-INTEGER would take a sign), and a Unicode scalar
       ;; value: not a surrogate, not past #x10FFFF.
       (let ((code (and (every (lambda (c) (digit-char-p c 16)) (subseq in.text in.pos end))
                        (parse-integer in.text :start in.pos :end end :radix 16))))
         (unless code (toml-fail in "a \\~a escape that is not hex" ch))
         (unless (and (< code char-code-limit) (not (<= #xd800 code #xdfff)))
           (toml-fail in "a \\~a escape that is not a Unicode scalar value" ch))
         (setf in.pos end)
         (code-char code))))
    (t (toml-fail in "\\~a is not an escape" ch))))

(defun toml-read-string (in &aux (delimiter (or (toml-scan in "\"\"\"|'''|[\"']")
                                                (toml-fail in "not a string")))
                                  (literal (char= (char delimiter 0) #\'))
                                  (multi (> (length delimiter) 1)))
  "Whichever of the four string forms starts here."
  ;; A newline straight after a multi-line opener is dropped, as TOML says.
  (when multi (toml-scan in "\\r?\\n?"))
  (with-output-to-string (out)
    (loop
      (when (toml-looking-at in delimiter)
        (incf in.pos (length delimiter))
        ;; Up to two more quote characters belong to the value.
        (loop repeat (if multi 2 0)
              while (eql (toml-peek in) (char delimiter 0))
              do (write-char (toml-take in) out))
        (return))
      (let ((ch (toml-take in)))
        (cond ((null ch)
               (if multi
                   (toml-fail in "a multi-line string with no closing delimiter")
                   (toml-fail in "a ~:[~;literal ~]string with no closing quote" literal)))
              ((and (char= ch #\Newline) (not multi))
               (toml-fail in "a newline inside a one-line ~:[~;literal ~]string" literal))
              ((or literal (char/= ch #\\)) (write-char ch out))
              ;; A backslash at a line end swallows the newline and the
              ;; indentation after it.
              ((and multi (toml-scan in "[\\n\\r \\t]+")))
              (t (write-char (toml-escape in) out)))))))

;;; --- keys ------------------------------------------------------------------

(defun toml-read-key-part (in)
  (cond ((member (toml-peek in) '(#\" #\')) (toml-read-string in))
        ((toml-scan in "[\\w-]+"))
        (t (toml-fail in "~s is not a key" (toml-peek in)))))

(defun toml-read-key-path (in &aux (parts (list (toml-read-key-part in))))
  "A dotted key as a list of its parts."
  (loop while (toml-scan in "[ \\t]*\\.[ \\t]*") do (push (toml-read-key-part in) parts))
  (toml-skip-blanks in)
  (nreverse parts))

;;; --- scalars ---------------------------------------------------------------

(defun toml-number (token &aux (text (remove #\_ token)))
  "TOKEN as a number, or NIL when it is not one."
  ;; Underscores are separators; 0x, 0o and 0b name a base. A float reaches the
  ;; reader only once the whole token is known to be [+-]digits[.digits][eE[+-]digits].
  (flet ((radix (prefix base)
           (and (> (length text) 2)
                (string-equal prefix (subseq text 0 2))
                (ignore-errors (parse-integer text :start 2 :radix base)))))
    (or (radix "0x" 16) (radix "0o" 8) (radix "0b" 2)
        (ignore-errors (parse-integer text))
        (and (ppcre:scan "\\A[+-]?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\\z" text)
             (ignore-errors
              (with-standard-io-syntax
                (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
                  (float (read-from-string text) 1d0))))))))

;;; --- values ----------------------------------------------------------------

(defun toml-read-value (in)
  (cond ((member (toml-peek in) '(#\" #\')) (toml-read-string in))
        ((eql (toml-peek in) #\[) (toml-take in)
         (let ((items (make-array 0 :adjustable t :fill-pointer 0)))
           (loop
             (toml-skip-space in :newlines t)
             (cond ((toml-end-p in) (toml-fail in "an array with no closing bracket"))
                   ((eql (toml-peek in) #\]) (toml-take in) (return))
                   (t (vector-push-extend (toml-read-value in) items)
                      (toml-skip-space in :newlines t)
                      (unless (or (toml-scan in ",") (eql (toml-peek in) #\]))
                        (toml-fail in "~s between array members" (toml-peek in))))))
           items))
        ((eql (toml-peek in) #\{) (toml-take in)
         (let ((table (nlk:json-object)))
           (toml-skip-space in :newlines t)
           (unless (toml-scan in "}")
             (loop
               (toml-skip-space in :newlines t)
               (let ((path (toml-read-key-path in)))
                 (unless (eql (toml-take in) #\=)
                   (toml-fail in "an inline-table member with no ="))
                 (toml-skip-blanks in)
                 (toml-put table path (toml-read-value in) in))
               (toml-skip-space in :newlines t)
               (cond ((eql (toml-peek in) #\,) (toml-take in))
                     ((eql (toml-peek in) #\}) (toml-take in) (return))
                     (t (toml-fail in "~s between inline-table members" (toml-peek in))))))
           table))
        ;; The run of characters a number, a boolean or a date-time is written with.
        (t (let ((token (toml-scan in "[\\w+.:-]*")))
             (when (zerop (length token))
               (toml-fail in "~s where a value should be" (toml-peek in)))
             (cond ((string= token "true") t)
                   ((string= token "false") :false)
                   (t (or (toml-number token)
                          ;; A date, a time, or an offset date-time: kept verbatim. A
                          ;; TOML date-time may carry one space between its date and
                          ;; its time, which the bare-token run stopped at.
                          (and (>= (length token) 10)
                               (digit-char-p (char token 0))
                               (digit-char-p (or (toml-peek in 1) #\a))
                               (nlk:when-let (time (toml-scan in " [\\w+.:-]*"))
                                 (concatenate 'string token time)))
                          token)))))))

;;; --- the tree --------------------------------------------------------------

(defun toml-descend (table path in &key array &aux (at table))
  "The table PATH names under TABLE, made on the way."
  ;; ARRAY means the last part names an array of tables and a fresh member is
  ;; appended to it.
  (loop for (part . rest) on path
        for next = (gethash part at)
        do (cond
             ((and array (not rest))
              (let ((fresh (nlk:json-object)))
                (cond ((null next)
                       (setf (gethash part at)
                             (make-array 1 :adjustable t :fill-pointer 1 :initial-element fresh)))
                      ((and (vectorp next) (array-has-fill-pointer-p next))
                       (vector-push-extend fresh next))
                      (t (toml-fail in "~a is already a value, not an array of tables" part)))
                (setf at fresh)))
             ((hash-table-p next) (setf at next))
             ;; A path through an array of tables walks its last member.
             ((and rest (vectorp next) (plusp (length next))
                   (hash-table-p (aref next (1- (length next)))))
              (setf at (aref next (1- (length next)))))
             ((null next) (setf at (setf (gethash part at) (nlk:json-object))))
             (t (toml-fail in "~a is already a value, not a table" part))))
  at)

(defun toml-put (table path value in)
  "VALUE at PATH under TABLE, the tables on the way made."
  (let ((at (if (rest path) (toml-descend table (butlast path) in) table))
        (last (car (last path))))
    (when (nth-value 1 (gethash last at))
      (toml-fail in "~a is set twice" last))
    (setf (gethash last at) value)))

(defun read-toml (text &aux (in (make-toml text))
                            (root (nlk:json-object))
                            (current root))
  "TEXT, a TOML document, as a hash table with string keys — the shape
NLK:JSON-VALUE walks. Signals IMPORT-ERROR naming the line it lost."
  (loop
    (toml-skip-space in :newlines t)
    (when (toml-end-p in) (return))
    (cond
      ((eql (toml-peek in) #\[)
       (let ((array (string= (toml-scan in "\\[\\[?") "[[")))
         (toml-skip-blanks in)
         (let ((path (toml-read-key-path in)))
           (unless (if array (toml-scan in "\\]\\]") (eql (toml-take in) #\]))
             (toml-fail in (if array
                               "an array-of-tables header with no ]]"
                               "a table header with no ]")))
           (setf current (toml-descend root path in :array array))
           (toml-end-of-line in))))
      (t
       (let ((path (toml-read-key-path in)))
         (unless (toml-scan in "=[ \\t]*")
           (toml-fail in "a key with no ="))
         (toml-put current path (toml-read-value in) in)
         (toml-end-of-line in)))))
  root)
