;;;; editor.lisp --- the composer's editing engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's textarea component (src/components/textarea.lisp,
;;;; upstream 6f69592 plus the nodecode fork's readline and soft-wrap work) and
;;;; reduced to the one configuration the shells construct: soft-wrapped at the
;;;; composer's own width, no prompt, no gutter, no painted caret — the shell
;;;; parks the terminal's hardware cursor at EDITOR-CURSOR-SCREEN-POSITION —
;;;; and a height that follows the content up to a cap.
;;;;
;;;; Two authorities the engine leans on instead of carrying its own:
;;;; BREAK-FRAGMENTS (theme) breaks a logical line into visual rows, so the
;;;; caret's visual row is computed by the same rule that paints the row;
;;;; TEXT-DISPLAY-WIDTH (theme) measures the caret's column, so a wide or a
;;;; pictured control glyph parks the cursor where it was painted. A widget
;;;; re-wrapping with its own table could only disagree with the paint.
;;;;
;;;; The engine mutates in place. Every text mutation bumps EPOCH; the joined
;;;; value and the visual-line map are memoized on it (and on the width, for
;;;; the map), so a frame that reads them several times pays the walk once
;;;; per edit rather than once per read. Cursor motion leaves the epoch alone:
;;;; EDITOR-HANDLE-KEY answers whether the text changed, which is what the
;;;; shell's "any real edit ends a history walk" rule asks.

(in-package #:nodecode.evolved)

;;; --- the buffer ----------------------------------------------------------

(nlk:define-record (editor (:constructor make-editor
                               (&key (width 40) (max-height 0) (placeholder "")
                                     (char-limit 0))))
  "The composer's text and caret."
  ;; LINES is the logical lines (a vector of strings, never
  ;; empty), ROW/COL the caret as a logical line index and a character offset
  ;; within it, WIDTH the wrap width in columns, MAX-HEIGHT the cap on visible
  ;; rows (0 = uncapped), OFFSET the first visible visual row, PLACEHOLDER the
  ;; text the shell shows while the buffer is empty, CHAR-LIMIT the total
  ;; character budget (0 = unlimited), EPOCH the edit counter. MAKE-EDITOR
  ;; takes the four the shells choose; an editor starts empty.
  (lines (vector ""))
  (row 0 :type fixnum)
  (col 0 :type fixnum)
  (width 40 :type fixnum)
  (max-height 0 :type fixnum)
  (offset 0 :type fixnum)
  (placeholder "")
  (char-limit 0 :type fixnum)
  (epoch 0 :type fixnum)
  (value-memo nil)
  (visual-memo nil))

(declaim (inline %editor-touch))
(defun %editor-touch (editor)
  "Mark the text changed: bump the epoch the memos key on."
  (incf editor.epoch)
  editor)

(defun %editor-set-lines (editor start end rows &aux (lines editor.lines))
  "Replace logical lines START..END with the strings ROWS."
  (setf editor.lines (concatenate 'vector (subseq lines 0 start) rows (subseq lines end)))
  editor)

(defun frame-lines (text)
  "TEXT split on newlines, keeping empty rows."
  ;; One empty row for empty TEXT: a frame row exists even when its content is
  ;; blank (UIOP:SPLIT-STRING answers no rows for it).
  (or (uiop:split-string (or text "") :separator '(#\Newline)) (list "")))

(defun split-lines (text)
  "TEXT split on newlines, NIL for empty text rather than the one empty row
FRAME-LINES keeps: a message with no content contributes nothing to a
transcript instead of a blank line."
  ;; The two are one split with two empty conventions, and every newline
  ;; consumer in the shell wants one of them.
  (when (and (stringp text) (plusp (length text)))
    (frame-lines text)))

(defmacro memoized (place (&rest fields) &body body)
  "PLACE's memoized answer while every one of FIELDS is still EQL to what it
was when that answer was made, else BODY, stored back under the fresh fields."
  ;; The one shape every render cache writes by hand, the editor's and
  ;; the transcript's: a simple-vector of the inputs the answer is a function of,
  ;; then the answer. The guard and the
  ;; store come from the one FIELDS list, so they cannot read different fields
  ;; or read them in a different order -- that drift is a stale render nothing
  ;; ever invalidates, and it is the reason this is a macro. PLACE is any
  ;; setf-able place read once and written once: a side table's entry, a
  ;; structure slot. Each field is evaluated exactly once, before BODY, and
  ;; anything in PLACE that is not one of these vectors reads as a miss.
  (let ((entry (gensym "ENTRY"))
        (value (gensym "VALUE"))
        (names (loop repeat (length fields) collect (gensym "FIELD"))))
    `(let ((,entry ,place)
           ,@(mapcar #'list names fields))
       (if (and (simple-vector-p ,entry)
                ,@(loop for name in names
                        for index from 0
                        collect `(eql (svref ,entry ,index) ,name)))
           (svref ,entry ,(length fields))
           (let ((,value (progn ,@body)))
             (setf ,place (vector ,@names ,value))
             ,value)))))

(defun editor-value (editor)
  "The whole text, lines joined by newlines. Memoized on the edit epoch."
  (memoized editor.value-memo (editor.epoch)
    (format nil "~{~A~^~%~}" (coerce editor.lines 'list))))

(defun editor-line-count (editor)
  "The number of logical lines."
  (length editor.lines))

(defun %editor-current-line (editor)
  (aref editor.lines editor.row))

(defun editor-reset (editor)
  "Empty the buffer and home the caret."
  (%editor-set-lines editor 0 (length editor.lines) (list ""))
  (setf editor.row 0
        editor.col 0
        editor.offset 0)
  (%editor-touch editor))

(defun editor-insert (editor text)
  "Insert TEXT at the caret and leave the caret after it; a newline in TEXT
splits the line."
  ;; Trimmed to CHAR-LIMIT when one is set — the budget counts characters,
  ;; newlines excluded. => EDITOR.
  (let ((text (if (plusp editor.char-limit)
                  (let ((used (reduce #'+ editor.lines :key #'length)))
                    (subseq text 0 (min (length text) (max 0 (- editor.char-limit used)))))
                  text)))
    (when (plusp (length text))
      ;; The caret's line becomes TEXT's lines, the text before the caret
      ;; heading the first and the text after it closing the last.
      (let* ((row editor.row)
             (current (aref editor.lines row))
             (tail (subseq current editor.col))
             (parts (frame-lines text)))
        (setf (first parts) (concatenate 'string (subseq current 0 editor.col) (first parts))
              (car (last parts)) (concatenate 'string (car (last parts)) tail))
        (%editor-set-lines editor row (1+ row) parts)
        (setf editor.row (+ row (length parts) -1)
              editor.col (- (length (car (last parts))) (length tail)))
        (%editor-touch editor))))
  editor)

;;; --- deletion ------------------------------------------------------------

(defun %editor-splice (editor start end &optional (text ""))
  "Replace characters START..END of the caret's line with TEXT."
  (let ((line (%editor-current-line editor)))
    (setf (aref editor.lines editor.row)
          (concatenate 'string (subseq line 0 start) text (subseq line end))))
  (%editor-touch editor))

;;; --- caret motion --------------------------------------------------------

(defun editor-cursor-end (editor)
  "Caret to the end of its line."
  (setf editor.col (length (%editor-current-line editor)))
  editor)

(defun editor-cursor-left (editor)
  "Caret one character left, wrapping to the end of the previous line."
  (cond ((plusp (editor-col editor)) (decf editor.col))
        ((plusp (editor-row editor))
         (decf editor.row)
         (editor-cursor-end editor)))
  editor)

(defun editor-cursor-right (editor)
  "Caret one character right, wrapping to the start of the next line."
  (cond ((< (editor-col editor) (length (%editor-current-line editor)))
         (incf editor.col))
        ((< editor.row (1- (editor-line-count editor))) (incf editor.row) (setf editor.col 0)))
  editor)

;;; --- words ---------------------------------------------------------------
;;; A word character is alphanumeric, the readline convention.

(defun %prev-word-boundary (line pos &aux (i (min (max 0 pos) (length line))))
  "The start of the word at or before POS, skipping the whitespace first."
  (let ((end (position-if #'alphanumericp line :end i :from-end t)))
    (if end (1+ (or (position-if-not #'alphanumericp line :end end :from-end t) -1)) 0)))

(defun %next-word-boundary (line pos &aux (n (length line)) (i (min (max 0 pos) n)))
  "Past the word at POS and the whitespace after it: the next word's start,
or the line end."
  (let ((end (or (position-if-not #'alphanumericp line :start i) n)))
    (or (position-if #'alphanumericp line :start end) n)))

(defun editor-word (editor direction &key delete &aux (col editor.col)
                                                      (line (%editor-current-line editor)))
  "Caret to the start of the previous word (DIRECTION -1) or past the current
word, or to the line end (DIRECTION 1); with DELETE, delete that word instead
(readline Ctrl+W / Alt+Backspace, Alt+D / Ctrl+Delete), the caret at its start."
  (let ((to (if (minusp direction)
                (%prev-word-boundary line col)
                (%next-word-boundary line col))))
    (cond ((not delete) (setf editor.col to))
          ((/= to col)
           (%editor-splice editor (min to col) (max to col))
           (setf editor.col (min to col)))))
  editor)

(defun editor-transpose-chars (editor &aux (line (%editor-current-line editor))
                                           (len (length line))
                                           (col editor.col)
                                           (start (if (>= col len) (- len 2) (1- col))))
  "Swap the characters around the caret (readline Ctrl+T): at the line end
the last two, mid-line the one before and the one at, advancing the caret;
nothing at the line start."
  (when (and (>= len 2) (>= start 0))
    (%editor-splice editor start (+ start 2)
                    (reverse (subseq line start (+ start 2))))
    (when (< col len)
      (incf editor.col)))
  editor)

(defun editor-case-word (editor fn &aux (line (%editor-current-line editor))
                                        (n (length line))
                                        (start (or (position-if #'alphanumericp line
                                                                :start (min editor.col n))
                                                   n)))
  "Apply FN (string -> string) to the word at or after the caret, then move
the caret past it."
  (when (< start n)
    (let ((end (or (position-if-not #'alphanumericp line :start start) n)))
      (%editor-splice editor start end (funcall fn (subseq line start end)))
      (setf editor.col end)))
  editor)

;;; --- visual rows ---------------------------------------------------------
;;; The wrap map: every logical line broken at WIDTH by BREAK-FRAGMENTS, as
;;; a vector of (row start end). Memoized on the epoch and the width.

(defun %editor-visuals (editor &aux (width (max 1 editor.width)))
  (memoized editor.visual-memo (editor.epoch width)
    (coerce (loop for row from 0
                  for line across (editor-lines editor)
                  nconc (loop for (start . end) in (break-fragments line width width)
                              collect (list row start end)))
            'vector)))

(defun %editor-cursor-visual (editor visuals &aux (row editor.row) (col editor.col))
  "Index into VISUALS of the row holding the caret."
  ;; A caret exactly on a break belongs to the row before it: the reserved
  ;; trailing column, not the next row's first cell.
  (or (position-if (lambda (v)
                     (and (= (first v) row) (<= (second v) col (third v))))
                   visuals)
      0))

(defun %editor-visual-step (editor delta &aux (visuals (%editor-visuals editor))
                                              (current (%editor-cursor-visual editor visuals))
                                              (target (+ current delta)))
  "Move the caret DELTA visual rows, keeping its offset within the row."
  ;; => T when it moved.
  (when (and (<= 0 target) (< target (length visuals)))
    (let* ((offset (- editor.col (second (aref visuals current)))) (to (aref visuals target)))
      (setf editor.row (first to)
            editor.col
            (max (second to) (min (+ (second to) offset) (third to))))
      t)))

(defun editor-height (editor &aux (rows (length (%editor-visuals editor))) (cap editor.max-height))
  "Rows the editor shows: the visual row count, at least one, capped at
MAX-HEIGHT when one is set."
  (max 1 (if (plusp cap) (min rows cap) rows)))

(defun %editor-settle (editor &aux (visuals (%editor-visuals editor))
                                   (height (editor-height editor))
                                   (cursor (%editor-cursor-visual editor visuals))
                                   (offset editor.offset))
  "Keep the caret's visual row inside the visible window and the window
inside the content. => the visual map."
  (cond ((< cursor offset) (setf offset cursor))
        ((>= cursor (+ offset height)) (setf offset (1+ (- cursor height)))))
  (setf editor.offset
        (max 0 (min offset (max 0 (- (length visuals) height)))))
  visuals)

(defun editor-visible-lines (editor &aux (visuals (%editor-settle editor)))
  "The visible rows, exactly EDITOR-HEIGHT strings: each visual row's text,
blank past the content. Settles the window around the caret first."
  (loop for i from 0 below (editor-height editor)
        for vi = (+ editor.offset i)
        collect (if (< vi (length visuals))
                    (destructuring-bind (row start end) (aref visuals vi)
                      (subseq (aref editor.lines row) start end))
                    "")))

(defun editor-hidden-lines (editor &aux (visuals (%editor-settle editor))
                                        (top editor.offset)
                                        (bottom (min (1- (length visuals))
                                                     (+ top (editor-height editor) -1))))
  "=> (values ABOVE BELOW): the logical lines that start above the visible
rows and those that end below them -- a line the window cuts in two counts
on the side its hidden part is, so the first line starting in view is line
ABOVE + 1."
  (labels ((line-of (index) (first (aref visuals index)))
           (same-line-p (a b)
             (and (< -1 a (length visuals)) (< -1 b (length visuals)) (= (line-of a) (line-of b)))))
    (if (minusp bottom)
        (values 0 0)
        (values (+ (line-of top) (if (same-line-p (1- top) top) 1 0))
                (+ (- (length editor.lines) 1 (line-of bottom))
                   (if (same-line-p bottom (1+ bottom)) 1 0))))))

(defun editor-cursor-screen-position (editor &aux (visuals (%editor-settle editor))
                                                  (vi (%editor-cursor-visual editor visuals))
                                                  (y (- vi editor.offset)))
  "The caret's cell within the visible rows as (values X Y), zero-based: X
in display columns from the left edge, Y in rows from the top visible row."
  ;; NIL when the caret row is not visible.
  (when (and (<= 0 y) (< y (editor-height editor)) (< vi (length visuals)))
    (let* ((start (second (aref visuals vi)))
           (line (%editor-current-line editor))
           (col (max start (min editor.col (length line)))))
      (values (text-display-width (subseq line start col)) y))))

;;; --- keys ----------------------------------------------------------------

(defun editor-handle-key (editor msg &aux (before editor.epoch))
  "Apply the key press MSG to EDITOR: the arrows and Home/End, the readline
motion, kill and word chords, Backspace/Delete, Enter (and its Ctrl+M /
Ctrl+J spellings) as a newline, and plain characters as text."
  ;; A ctrl or alt chord this table does not name is a gesture, not text: it
  ;; never types its base letter. => T when the text changed; motion alone
  ;; answers NIL.
  (when (nlt:key-press-msg-p msg)
    (let* ((key (nlt:key-event-code msg))
           (mod (nlt:key-event-mod msg))
           (ctrl (nlt:mod-contains mod nlt:+mod-ctrl+))
           (alt (nlt:mod-contains mod nlt:+mod-alt+)))
      (flet ((chord (char) (and (characterp key) (char= key char))))
        (cond
          ;; Word motion first: Ctrl/Alt + arrow outranks the plain arrow.
          ((or (and (or ctrl alt) (eq key :left)) (and alt (chord #\b))) (editor-word editor -1))
          ((or (and (or ctrl alt) (eq key :right)) (and alt (chord #\f))) (editor-word editor 1))
          ;; Up/Down step one VISUAL row, keeping the caret's offset
          ;; within it: a logical line wider than WIDTH occupies several
          ;; visual rows, and stepping the logical row instead would jump
          ;; the caret past every wrapped row of it.
          ((eq key :up) (%editor-visual-step editor -1))
          ((eq key :down) (%editor-visual-step editor 1))
          ((or (eq key :left) (and ctrl (chord #\b))) (editor-cursor-left editor))
          ((or (eq key :right) (and ctrl (chord #\f))) (editor-cursor-right editor))
          ;; Document begin/end outrank line start/end below.
          ((and ctrl (eq key :home)) (setf editor.row 0 editor.col 0))
          ((and ctrl (eq key :end))
           (setf editor.row (1- (editor-line-count editor)))
           (editor-cursor-end editor))
          ((or (and ctrl (chord #\w)) (and (or ctrl alt) (eq key :backspace)))
           (editor-word editor -1 :delete t))
          ((or (and alt (chord #\d)) (and (or ctrl alt) (eq key :delete)))
           (editor-word editor 1 :delete t))
          ((and ctrl (chord #\t)) (editor-transpose-chars editor))
          ;; readline Alt+C, Alt+L and Alt+U
          ((and alt (chord #\c)) (editor-case-word editor #'string-capitalize))
          ((and alt (chord #\l)) (editor-case-word editor #'string-downcase))
          ((and alt (chord #\u)) (editor-case-word editor #'string-upcase))
          ((or (eq key :home) (and ctrl (chord #\a))) (setf editor.col 0))
          ((or (eq key :end) (and ctrl (chord #\e))) (editor-cursor-end editor))
          ;; Backspace steps the caret left, then deletes at it as Delete does.
          ((or (eq key :delete) (and ctrl (chord #\d))
               (and (eq key :backspace)
                    (or (plusp (editor-col editor)) (plusp (editor-row editor)))
                    (editor-cursor-left editor)))
           (cond ((< editor.col (length (aref editor.lines editor.row)))
                  (%editor-splice editor editor.col (1+ editor.col)))
                 ((< editor.row (1- (editor-line-count editor)))
                  (%editor-set-lines editor editor.row (+ editor.row 2)
                                     (list (concatenate 'string (aref editor.lines editor.row)
                                                        (aref editor.lines (1+ editor.row)))))
                  (%editor-touch editor))))
          ;; Ctrl+J is the raw LF byte and Ctrl+M the raw CR byte: both are
          ;; the readline newline chord.
          ((or (eq key :enter) (and ctrl (or (chord #\m) (chord #\j))))
           (editor-insert editor (string #\Newline)))
          ((and ctrl (chord #\k))
           (let ((end (length (%editor-current-line editor))))
             (when (< editor.col end)
               (%editor-splice editor editor.col end))))
          ((and ctrl (chord #\u))
           (cond ((plusp editor.col)
                  (%editor-splice editor 0 editor.col)
                  (setf editor.col 0))
                 ((and (zerop (length (%editor-current-line editor)))
                       (> (editor-line-count editor) 1))
                  ;; Remove just this empty logical line, not its neighbors.
                  (%editor-set-lines editor editor.row (1+ editor.row) nil)
                  (when (plusp editor.row)
                    (decf editor.row)
                    (editor-cursor-end editor))
                  (%editor-touch editor))))
          ((and (characterp key) (not ctrl) (not alt))
           (editor-insert editor (string key)))))))
  (/= before editor.epoch))
