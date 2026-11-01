;;;; yaml.lisp --- a YAML config file as the decoded shape JSON takes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Several agent homes write their settings with a block-style YAML
;;;; dumper -- PyYAML's is the one this reader was written against: `key:
;;;; value' lines, nested blocks two columns in, sequences as `- item'
;;;; lines that sit at their key's own column, `{}' and `[]' for what is
;;;; empty, quotes only where a scalar needs them. This reads that dialect
;;;; and the few flow forms a hand edit adds -- `[a, b]', `{k: v}' -- into
;;;; hash tables with string keys, vectors, strings, numbers, T, :FALSE and
;;;; :NULL: the shape shasht gives JSON, so NLK:JSON-VALUE walks a YAML
;;;; config exactly as it walks config.jsonc, and one shape classifier
;;;; (shape.lisp) reads every format the same way. Anchors, tags, aliases
;;;; and multi-document streams are refused by name rather than guessed at;
;;;; a refusal is an IMPORT-ERROR carrying the line.

(in-package #:nodecode.import)

;;; --- lines ---------------------------------------------------------------

(defstruct (yline (:constructor yline (indent text number)))
  "One meaningful line: INDENT its column, TEXT what follows the indent with
the comment stripped, NUMBER its 1-based line in the document."
  indent text number)

(nlk:access (left yline) (line yline) (next yline))

(defun strip-comment (line)
  "LINE without a trailing `# comment': a # at the start or after a blank,
outside quotes."
  (values (ppcre:scan-to-strings
           "\\A(?:[^'\"#]+|'[^']*'?|\"(?:[^\"\\\\]+|\\\\.?)*\"?|(?<=[^ \\t])#)*" line)))

;;; --- the parser ----------------------------------------------------------

(defstruct (yparser (:constructor make-yparser (lines raw)))
  lines raw (pos 0 :type fixnum))

(nlk:access (p yparser))

(defun ypeek (p &aux (lines p.lines)) (and (< p.pos (length lines)) (aref lines p.pos)))

(defun ynext (p)
  (prog1 (ypeek p) (incf p.pos)))

(defun sequence-line-p (text)
  (or (string= text "-") (uiop:string-prefix-p "- " text)))

(defun read-yaml (text)
  "TEXT, a YAML document in the block style PyYAML writes, as the decoded
shape shasht gives JSON => (values VALUE REFUSALS)."
  ;; An empty document is an empty object. REFUSALS names the top-level
  ;; sections that did not read, so a caller can report them and use
  ;; everything else; a document whose top level is a sequence has no sections
  ;; to isolate and is refused whole.
  (let* ((raw (map 'vector (lambda (line) (string-right-trim '(#\Return) line))
                   (uiop:split-string text :separator '(#\Newline))))
         (lines (loop for line across raw
                      for number from 1
                      for content = (string-right-trim '(#\Space #\Tab) (strip-comment line))
                      for trimmed = (string-trim '(#\Space #\Tab) content)
                      unless (member trimmed '("" "---" "...") :test #'string=)
                        do (when (uiop:string-prefix-p "%" trimmed)
                             (fail-document "config.yaml line ~d: directives are not read" number))
                        and collect (let ((indent (position #\Space content :test-not #'char=)))
                                      (yline indent (subseq content indent) number))))
         (p (make-yparser (coerce lines 'vector) raw)))
    (cond
      ((null (ypeek p)) (values (make-hash-table :test #'equal) '()))
      ((sequence-line-p (yline-text (ypeek p)))
       (let* ((value (parse-sequence p (yline-indent (ypeek p))))
              (left (ypeek p)))
         (when left (fail "config.yaml line ~d: unexpected `~a'" left.number left.text))
         (values value '())))
      (t (parse-mapping p (yline-indent (ypeek p)) t)))))

(defun parse-node (p indent &aux (line (ypeek p)))
  (cond ((null line) :null)
        ((sequence-line-p (yline-text line)) (parse-sequence p indent))
        (t (parse-mapping p indent))))

(defun quoted-p (text)
  (and (plusp (length text)) (member (char text 0) '(#\' #\"))))

(defun split-key (text &aux (n (length text)))
  "(values KEY REST) when TEXT is a `key: rest' or `key:' line, else NIL."
  ;; The colon that splits is the first one followed by a blank or the end,
  ;; outside quotes — so a URL's `://' never splits.
  (let ((at (length (ppcre:scan-to-strings
                     "\\A(?:'[^']*'?|\"(?:[^\"\\\\]+|\\\\.?)*\"?|[ \\t]*:)?(?:[^:]+|:(?=[^ \\t]))*"
                     text))))
    (when (< at n)
      (let ((key (string-trim '(#\Space #\Tab) (subseq text 0 at))))
        (values (if (quoted-p key) (parse-scalar key) key)
                (subseq text (1+ at)))))))

(defun parse-mapping (p indent &optional document &aux (table (make-hash-table :test #'equal))
                                                       (refusals '()))
  (loop for line = (ypeek p)
        for start = p.pos
        while (and line
                   (= line.indent indent)
                   (not (sequence-line-p line.text)))
        do (handler-case
               (multiple-value-bind (key rest) (split-key line.text)
                 (unless key
                   (fail "config.yaml line ~d: not a `key: value' line: ~a"
                         line.number line.text))
                 (when (and (stringp key) (plusp (length key)) (find (char key 0) "&*!"))
                   (fail-document "config.yaml line ~d: anchors and tags are not read"
                                  line.number))
                 (ynext p)
                 (setf (gethash (princ-to-string key) table)
                       (parse-value p line indent rest)))
             ;; The DOCUMENT's own keys: one that does not read costs itself alone.
             ;; A whole-document refusal stands: recovering past an anchor
             ;; would be guessing at values elsewhere that depend on it.
             ((and error (not yaml-unreadable)) (condition)
               (unless document (error condition))
               (setf p.pos (1+ start))
               (loop for next = (ypeek p)
                     while (and next (> next.indent indent))
                     do (ynext p))
               (push (cons (nlk:if-let (key (ignore-errors (split-key line.text)))
                             (princ-to-string key) line.text)
                           (princ-to-string condition))
                     refusals))))
  (nlk:when-let (left (and document (ypeek p)))
    (push (cons left.text (format nil "config.yaml line ~d: unexpected `~a'" left.number left.text))
          refusals)
    (loop while (ypeek p) do (ynext p)))
  (values table (nreverse refusals)))

(defun fold-scalar (p indent text)
  "TEXT, a scalar that began on a `key:' or `- ' line at INDENT, with
every deeper line that follows folded into it — one space per line break,
which is what PyYAML's block style means by a value wrapped over several
lines and what it reads back:

    creative: You are a creative assistant."
  ;; Think outside the box and offer
  ;;       innovative solutions.
  ;;
  ;; A value already on the line and a deeper block are exclusive in YAML, so
  ;; a line indented past the key cannot be anything but this value
  ;; continuing: consuming it here is the only reading. Left unread it
  ;; survived to the end of the document and READ-YAML refused the whole file
  ;; as `unexpected solutions.' — which cost an operator every provider in
  ;; their config.yaml, since one wrapped line in a `personalities:' block
  ;; three sections away failed the parse and the import lost the lot.
  ;;
  ;; A quoted scalar wraps too, and its own quote says where it ends:
  ;;
  ;;     pirate: 'Arrr! Ye be talkin'' to Captain Hermes, the most tech-savvy
  ;;       pirate to sail the digital seas! Yo ho ho!'
  ;;
  ;; so an OPEN quote folds until the line that closes it, and a quote already
  ;; closed on its line is left exactly as it was. Flow scalars are not
  ;; claimed: they have their own continuation rules and this reader does not
  ;; read them.
  ;; QUOTED-OPEN-P: TEXT opens a quote that nothing in it closes (`''' and `\\\"' do not).
  (flet ((quoted-open-p (text)
           (and (quoted-p text)
                (not (ppcre:scan "(?s)\\A(?:'(?>(?:[^']|'')*)'|\"(?>(?:[^\"\\\\]|\\\\.)*)\")"
                                 text)))))
    (if (or (flow-p text) (and (quoted-p text) (not (quoted-open-p text))))
        text
        (let ((open (quoted-open-p text))
              (joined text))
          ;; An open quote decides, not the indentation: fold on until the line that
          ;; closes it, then hand the whole value to the decoder, which takes
          ;; everything up to the last quote.
          (loop for next = (ypeek p)
                while (and next (if open (quoted-open-p joined) (> next.indent indent)))
                do (setf joined (format nil "~a ~a" joined next.text))
                   (ynext p))
          joined))))

(defun parse-value (p line indent rest)
  "The value after `key:' on LINE: REST when it says one, else the block
that follows deeper — or a sequence at the same column, PyYAML's habit."
  (let ((rest (string-trim '(#\Space #\Tab) rest)))
    (cond ((zerop (length rest))
           (let ((next (ypeek p)))
             (cond ((null next) :null)
                   ((> (yline-indent next) indent) (parse-node p next.indent))
                   ((and (= (yline-indent next) indent)
                         (sequence-line-p (yline-text next)))
                    (parse-sequence p indent))
                   (t :null))))
          ((block-scalar-p rest) (parse-block-scalar p line indent rest))
          (t (parse-scalar (fold-scalar p indent rest))))))

(defun parse-sequence (p indent &aux (items '()))
  (loop for line = (ypeek p)
        while (and line
                   (= line.indent indent)
                   (sequence-line-p line.text))
        do (let ((rest (string-trim '(#\Space #\Tab) (subseq line.text 1))))
             (cond ((zerop (length rest))
                    (ynext p)
                    (let ((next (ypeek p)))
                      (push (if (and next (> next.indent indent)) (parse-node p next.indent) :null)
                            items)))
                   ((and (not (quoted-p rest)) (not (flow-p rest)) (split-key rest))
                    ;; `- key: value': a mapping whose first line is this
                    ;; item, its body two columns in — the line is rewritten
                    ;; in place and the mapping reader takes over.
                    (setf line.indent (+ indent 2)
                          line.text rest)
                    (push (parse-mapping p (+ indent 2)) items))
                   ((block-scalar-p rest)
                    (ynext p)
                    (push (parse-block-scalar p line indent rest) items))
                   (t (ynext p)
                      ;; A sequence item wraps the same way a mapping value
                      ;; does; the `- key: value' shape above has already
                      ;; claimed the only other reading of a deeper line.
                      (push (parse-scalar (fold-scalar p indent rest)) items)))))
  (coerce (nreverse items) 'vector))

;;; --- scalars -------------------------------------------------------------

(defun flow-p (text)
  (and (plusp (length text)) (member (char text 0) '(#\[ #\{))))

(defun block-scalar-p (text)
  (ppcre:scan "\\A[|>][-+0-9]*\\z" text))

(defun parse-block-scalar (p line indent header)
  "The literal (|) or folded (>) block after LINE: the raw lines that follow
deeper than INDENT, their common indent stripped, joined by newlines or
folded to blanks; `-' after the marker drops the final newline."
  (let* ((folded (char= (char header 0) #\>))
         (chomp (cond ((find #\- header) :strip) ((find #\+ header) :keep) (t :clip)))
         (lines (loop with block-indent = nil for number from (1+ line.number) to (length p.raw)
                      for text = (aref p.raw (1- number))
                      for blank = (zerop (length (string-trim '(#\Space #\Tab) text)))
                      for this-indent = (or (position #\Space text :test-not #'char=) (length text))
                      while (or blank (> this-indent indent))
                      do (unless (or blank block-indent) (setf block-indent this-indent))
                      collect (if blank "" (subseq text (min block-indent (length text)))))))
    ;; Skip the meaningful lines the block consumed.
    (loop while (and (ypeek p) (<= (yline-number (ypeek p)) (+ line.number (length lines))))
          do (ynext p))
    ;; Trailing blank lines belong to chomping, not the text.
    (let* ((trimmed (reverse (member-if #'plusp (reverse lines) :key #'length)))
           (body (if folded
                     (with-output-to-string (out)
                       (loop for piece in trimmed
                             for first = t then nil
                             do (cond ((string= piece "") (write-char #\Newline out))
                                      (first (write-string piece out))
                                      (t (write-char #\Space out) (write-string piece out)))))
                     (format nil "~{~a~^~%~}" trimmed))))
      (ecase chomp
        (:strip body)
        (:clip (concatenate 'string body (string #\Newline)))
        (:keep (concatenate 'string body (string #\Newline)
                            (make-string (- (length lines) (length trimmed))
                                         :initial-element #\Newline)))))))

(defun quoted-scalar (text &aux (q (char text 0))
                                (inner (subseq text 1 (max 1 (or (position q text :from-end t) 1)))))
  (flet ((unescape (match &aux (code (and (= (length match) 6)
                                          (parse-integer match :start 2 :radix 16))))
           (case (char match 1)
             (#\n (string #\Newline))
             (#\t (string #\Tab))
             (#\r (string #\Return))
             (t (if code (string (code-char code)) (subseq match 1))))))
    (if (char= q #\')
        (ppcre:regex-replace-all "''" inner "'")
        (ppcre:regex-replace-all "(?s)\\\\(?:u[0-9A-Fa-f]{4}|.)" inner #'unescape
                                 :simple-calls t))))

(defun split-flow (inner &aux (pieces '()) (depth 0) (start 0) (at 0) (n (length inner)))
  "INNER split on the commas outside quotes and outside a nested [..] or {..},
each piece trimmed, empties dropped."
  (loop while (< at n)
        do (case (char inner at)
             (#\' (setf at (or (position #\' inner :start (1+ at)) n)))
             (#\" (incf at)
                  (loop while (and (< at n) (char/= (char inner at) #\"))
                        do (when (char= (char inner at) #\\) (incf at))
                           (incf at)))
             ((#\[ #\{) (incf depth))
             ((#\] #\}) (when (plusp depth) (decf depth)))
             (#\, (when (zerop depth)
                   (push (subseq inner start at) pieces)
                   (setf start (1+ at)))))
           (incf at))
  (push (subseq inner start) pieces)
  (remove "" (mapcar (lambda (piece) (string-trim '(#\Space #\Tab) piece)) (nreverse pieces))
          :test #'string=))

(defun parse-scalar (text)
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (cond ((zerop (length text)) :null)
          ((member text '("null" "~" "Null" "NULL") :test #'string=) :null)
          ((member text '("true" "True" "TRUE") :test #'string=) t)
          ((member text '("false" "False" "FALSE") :test #'string=) :false)
          ((quoted-p text) (quoted-scalar text))
          ((member (char text 0) '(#\[ #\{))
           (let* ((sequence (char= (char text 0) #\[))
                  (close (position (if sequence #\] #\}) text :from-end t))
                  (pieces (split-flow (string-trim '(#\Space #\Tab)
                                                   (subseq text 1 (or close (length text)))))))
             (if sequence
                 (coerce (mapcar #'parse-scalar pieces) 'vector)
                 (let ((table (make-hash-table :test #'equal)))
                   (dolist (piece pieces table)
                     (multiple-value-bind (key rest) (split-key piece)
                       (unless key (fail "config.yaml: `~a' is not a `key: value' member" piece))
                       (setf (gethash (princ-to-string key) table) (parse-scalar rest))))))))
          ((member (char text 0) '(#\& #\* #\!))
           ;; Whole-document, not this section: an alias here means some other
           ;; section's value is defined by a construct this reader does not
           ;; follow, so carrying on would be guessing at settings elsewhere.
           (fail-document "config.yaml: anchors, aliases and tags are not read (`~a')" text))
          ;; A YAML core-schema number: never a dotted version, never a plus-signed count.
          ((ppcre:scan "\\A-?\\d+(?:\\.\\d+)?\\z" text)
           (with-standard-io-syntax
             (let ((*read-default-float-format* 'double-float)
                   (*read-eval* nil))
               (read-from-string text))))
          (t text))))

;;; --- .env ----------------------------------------------------------------

(defun read-env-file (text &aux (entries '()))
  "TEXT, a .env file, as an alist of (NAME . VALUE): `NAME=value' lines,
an optional `export ', quotes stripped, blank and # lines skipped."
  ;; A later line for the same name wins.
  (dolist (raw (uiop:split-string text :separator '(#\Newline)))
    (let* ((line (string-trim '(#\Space #\Tab #\Return) raw))
           (line (if (uiop:string-prefix-p "export " line)
                     (string-trim '(#\Space #\Tab) (subseq line 7))
                     line))
           (eq (position #\= line)))
      (when (and eq (plusp eq) (not (uiop:string-prefix-p "#" line)))
        (let ((name (string-trim '(#\Space #\Tab) (subseq line 0 eq)))
              (value (string-trim '(#\Space #\Tab) (subseq line (1+ eq)))))
          (when (and (> (length value) 1) (quoted-p value)
                     (char= (char value 0) (char value (1- (length value)))))
            (setf value (subseq value 1 (1- (length value)))))
          (when (plusp (length name))
            (setf entries (cons (cons name value)
                                (remove name entries :key #'car :test #'string=))))))))
  (nreverse entries))
