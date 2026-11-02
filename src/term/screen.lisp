;;;; screen.lisp --- the screen as cells, and the diff that paints it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's cells.lisp (see NOTICE).
;;;;
;;;; A cell is one terminal column: its character, the columns it paints
;;;; (0 for the trailing half of a wide glyph, 2 for its leading half), its
;;;; SGR foreground and background as the parameter strings the terminal will
;;;; be sent ("38;2;R;G;B", "31", NIL for default), an attribute bitmask, and
;;;; the OSC 8 link it sits in. A screen buffer is a row-major grid of them.
;;;; PAINT-ROWS lays row strings into a buffer at a position, over what is
;;;; already there; PARSE-STYLED-STRING is that into a blanked buffer, one
;;;; row string per screen row;
;;;; RENDER-DIFF writes the minimal escape bytes that turn the previous buffer
;;;; into the new one. No colour object exists anywhere in this: the shell's
;;;; own SGR bytes flow through the parser into the cells and out again.

(in-package #:nodecode.term)

;;; --- attributes ----------------------------------------------------------

(defconstant +attr-bold+          #b00000001)
(defconstant +attr-italic+        #b00000010)
(defconstant +attr-underline+     #b00000100)
(defconstant +attr-blink+         #b00001000)
(defconstant +attr-reverse+       #b00010000)
(defconstant +attr-strikethrough+ #b00100000)
(defconstant +attr-faint+         #b01000000)

(defparameter +sgr-attributes+
  `((1 "1" ,+attr-bold+) (2 "2" ,+attr-faint+) (3 "3" ,+attr-italic+)
    (4 "4" ,+attr-underline+) (5 "5" ,+attr-blink+) (7 "7" ,+attr-reverse+)
    (9 "9" ,+attr-strikethrough+))
  "Each SGR attribute parameter with its text and the attribute bit it sets,
in parameter order. The parameter twenty above one clears its bit again.")

;;; --- cells ---------------------------------------------------------------

(nlk:define-record (cell (:constructor make-cell) (:export :readers))
  "One terminal column."
  (char #\Space :type character)
  (width 1 :type (integer 0 2))
  (fg nil)
  (bg nil)
  (attrs 0 :type fixnum)
  (link nil :type (or null string)))

(nlk:access (a cell) (b cell) (cell cell) (lead cell) (tail cell))

(defun cell-equal (a b)
  "Whether cells A and B paint identically."
  (and (char= a.char b.char)
       (= a.width b.width)
       (equal a.fg b.fg)
       (equal a.bg b.bg)
       (= a.attrs b.attrs)
       (equal a.link b.link)))

(declaim (inline set-cell))
(defun set-cell (cell char width fg bg attrs link)
  "Set every field of CELL at once."
  (setf cell.char char cell.width width cell.fg fg cell.bg bg cell.attrs attrs cell.link link))

;;; --- the screen buffer ---------------------------------------------------

(nlk:define-record (screen-buffer (:constructor %make-screen-buffer) (:export width height))
  "A WIDTH x HEIGHT grid of cells, row-major."
  (width 80 :type fixnum)
  (height 24 :type fixnum)
  (cells #() :type simple-vector))

(nlk:access (buffer screen-buffer) (new screen-buffer) (old screen-buffer))

(defun reusable-screen-buffer (buffer width height)
  "BUFFER when it is a WIDTH x HEIGHT buffer, blanked; a fresh one otherwise."
  ;; The else branch is a frame whose geometry moved: the spare's cells
  ;; address rows and columns that no longer exist, so it is dropped, not
  ;; trimmed.
  (if (and buffer
           (= buffer.width width)
           (= buffer.height height))
      ;; Blanked through each cell, never one shared blank: the parser fills a
      ;; cell with SETF, so shared columns would all show the last glyph.
      (loop for cell across buffer.cells do (set-cell cell #\Space 1 nil nil 0 nil)
            finally (return buffer))
      (%make-screen-buffer :width width :height height
                           :cells (map-into (make-array (* width height)) #'make-cell))))

(defvar *sgr-strings* (make-hash-table :test #'eql)
  "Colour key -> its SGR text: a basic colour's value, or SGR-EXTENDED-STRING's packed key.")

(defun sgr-basic-string (value)
  "The SGR parameter text for a basic colour VALUE (30-37, 40-47, 90-107)."
  ;; The old parser formatted one string per style change per frame; the
  ;; palette is a few dozen values wide, so the table converges at once and a
  ;; frame allocates no colour text at all. Equality of two cells' colours is
  ;; then string equality of shared objects.
  (alexandria:ensure-gethash value *sgr-strings* (format nil "~D" value)))

(defun sgr-extended-string (value mode a b c &aux (key (+ (ash value 32)
                                                          (ash mode 24)
                                                          (ash (ldb (byte 8 0) a) 16)
                                                          (ash (ldb (byte 8 0) b) 8)
                                                          (ldb (byte 8 0) c))))
  "VALUE;5;A or VALUE;2;A;B;C for a 38/48 extended colour, formatted once."
  ;; The key is one integer — the lookup allocates nothing, which is the
  ;; point: this runs for every truecolor run of every row of every frame.
  ;; Each channel's own low byte: (BYTE 8 8) read the high byte of an 8-bit
  ;; value, 0 for every colour, so every truecolor run keyed alike and the
  ;; first colour a frame painted became every colour it painted.
  (alexandria:ensure-gethash key *sgr-strings*
                             (if (= mode 5)
                                 (format nil "~D;5;~D" value a)
                                 (format nil "~D;2;~D;~D;~D" value a b c))))

(declaim (inline screen-buffer-ref))
(defun screen-buffer-ref (buffer x y)
  "The cell at column X, row Y."
  (aref buffer.cells (+ (* y buffer.width) x)))

;;; --- the view parser -----------------------------------------------------

(declaim (inline unseat-cell))
(defun unseat-cell (buffer x y &aux (cell (screen-buffer-ref buffer x y)))
  "Make the cell at column X, row Y stand alone before it is painted over:
the other half of a wide glyph it belongs to becomes a blank in that glyph's
style."
  ;; The glyph cannot survive losing a half, and a lead cell left claiming two
  ;; columns makes RENDER-DIFF step over the cell painted beside it.
  (case cell.width
    (0 (when (plusp x)
         (let ((lead (screen-buffer-ref buffer (1- x) y)))
           (when (= 2 lead.width)
             (setf lead.char #\Space lead.width 1)))))
    (2 (when (< (1+ x) buffer.width)
         (let ((tail (screen-buffer-ref buffer (1+ x) y)))
           (when (zerop tail.width)
             (setf tail.char #\Space tail.width 1)))))))

(defun paint-rows (buffer rows &key (x 0) (y 0) width wrap)
  "Lay ROWS, a list of row strings, into BUFFER over what it holds, the
first at column X of row Y and each on the screen row below the one before:
SGR sequences setting the style the following cells carry, OSC 8 setting
the link, every other escape skipped at zero width, a tab one space, a wide
glyph two cells (the second blank at width 0), a zero-width glyph dropped.
A painted cell with no background of its own keeps the one under it. A row
stops at WIDTH columns from X (the buffer's right edge by default); with
WRAP it goes on at column X of the next screen row instead. => BUFFER."
  ;; Style and link carry across rows as they would across a terminal's lines.
  ;; Only the cells a glyph lands on change — the region is not blanked — so
  ;; a layer painted this way sits on the frame under it, and a background
  ;; left unset shows that frame through (the half-block edge of a box
  ;; painted over a filled row). A wide glyph cut by the region's edge on
  ;; either side loses its other half to a blank (UNSEAT-CELL). Rows past the
  ;; buffer's last row are not walked: nothing on screen can show them.
  (let* ((height buffer.height)
         (left x)
         (right (min buffer.width (+ x (or width buffer.width))))
         (fg nil) (bg nil) (attrs 0) (link nil)
         ;; One parameter array for the whole parse. An SGR list is written and
         ;; read inside one escape, and every index a sequence reads is one it
         ;; wrote for that sequence, so one array serves all of them: a frame
         ;; carries about a thousand escapes and this used to allocate an array
         ;; for each of them.
         (params (make-array 16 :initial-element 0)))
    (flet ((put (ch cw)
             ;; CW columns from X fit before RIGHT and Y is on the buffer.
             ;; Both cells are unseated before either is painted: a tail
             ;; unseated after its lead was painted would blank the lead.
             (unseat-cell buffer x y)
             (when (= cw 2) (unseat-cell buffer (1+ x) y))
             (let ((cell (screen-buffer-ref buffer x y)))
               (set-cell cell ch cw fg (or bg cell.bg) attrs link))
             (when (= cw 2)
               (let ((tail (screen-buffer-ref buffer (1+ x) y)))
                 (set-cell tail #\Space 0 fg (or bg tail.bg) attrs link)))
             (incf x cw))
           (sgr (params count)
             ;; PARAMS holds COUNT parameter values, an empty one 0 as
             ;; ECMA-48 has it: a bare ESC[m is a reset.
             (do ((p 0 (1+ p))) ((>= p count))
               (let ((v (aref params p)))
                 (cond
                   ((= v 0) (setf fg nil bg nil attrs 0))
                   ((= v 21) (setf attrs (logior attrs +attr-underline+)))
                   ((= v 22) (setf attrs (logandc2 attrs (logior +attr-bold+ +attr-faint+))))
                   ((assoc v +sgr-attributes+)
                    (setf attrs (logior attrs (third (assoc v +sgr-attributes+)))))
                   ((assoc (- v 20) +sgr-attributes+)
                    (setf attrs (logandc2 attrs (third (assoc (- v 20) +sgr-attributes+)))))
                   ((or (<= 30 v 37) (<= 90 v 97)) (setf fg (sgr-basic-string v)))
                   ((= v 39) (setf fg nil))
                   ((or (<= 40 v 47) (<= 100 v 107)) (setf bg (sgr-basic-string v)))
                   ((= v 49) (setf bg nil))
                   ((or (= v 38) (= v 48))
                    ;; 38;5;N / 38;2;R;G;B and the 48 pair
                    ;; SPAN is how many parameters the mode takes after 38 or 48.
                    (let ((span (and (< (1+ p) count)
                                     (case (aref params (1+ p)) (5 2) (2 4)))))
                      (when (and span (< (+ p span) count))
                        (let ((value (sgr-extended-string
                                      v (aref params (1+ p)) (aref params (+ p 2))
                                      (if (= span 4) (aref params (+ p 3)) 0)
                                      (if (= span 4) (aref params (+ p 4)) 0))))
                          (if (= v 38) (setf fg value) (setf bg value))
                          (incf p span))))))))))
      (dolist (string rows)
       ;; Nothing below the last screen row can be seen: stop walking a view
       ;; that is longer than the screen it lands on.
       (when (>= y height) (return))
       (loop with i = 0 with len = (length string) while (< i len)
            do (let ((ch (char string (shiftf i (1+ i)))))
                 (if (char= ch #\Escape)
                     (when (< i len)
                       (let ((next (char string (shiftf i (1+ i)))))
                         ;; Any other escape: ESC plus one byte.
                         (cond
                           ;; CSI: parameters until a final byte 0x40-0x7E.
                           ((char= next #\[)
                            (loop with count = 1 with current = 0 while (< i len)
                                  do (let ((c (char string (shiftf i (1+ i)))))
                                       (cond
                                         ((digit-char-p c)
                                          (setf current (+ (* current 10) (digit-char-p c))))
                                         ((or (char= c #\;) (char= c #\:))
                                          (setf (aref params (1- count)) current)
                                          (when (< count (length params)) (incf count))
                                          (setf current 0))
                                         ((<= #x40 (char-code c) #x7E)
                                          (setf (aref params (1- count)) current)
                                          (when (char= c #\m)
                                            (sgr params count))
                                          (return))))))
                           ;; OSC: the body until BEL or ST; OSC 8 is a link.
                           ((char= next #\])
                            (let* ((bel (position #\Bel string :start i))
                                   (st (search '(#\Escape #\\) string :start2 i))
                                   (end (if (and bel st) (min bel st) (or bel st)))
                                   (body (subseq string i (or end len))))
                              (setf i (if end (+ end (if (eql end bel) 1 2)) len))
                              (when (and (>= (length body) 2) (string= "8;" body :end2 2))
                                (let ((p2 (position #\; body :start 2)))
                                  ;; an empty URI closes the link
                                  (setf link (and p2 (< (1+ p2) (length body))
                                                  (subseq body (1+ p2)))))))))))
                     ;; a tab is one column wide and paints as a space
                     (let ((cw (codepoint-width ch)))
                       (unless (zerop cw)
                         ;; Past the edge the row goes on below, or is cut:
                         ;; nothing after the first glyph that did not fit
                         ;; lands on this row.
                         (when (> (+ x cw) right)
                           (if wrap (setf y (1+ y) x left) (setf x right)))
                         (when (and (< y height) (<= (+ x cw) right))
                           (put (if (char= ch #\Tab) #\Space ch) cw)))))))
       (incf y) (setf x left)))
    buffer))

(defun parse-styled-string (rows width height &key buffer)
  "Lay ROWS, a list of row strings, out into a blank WIDTH x HEIGHT buffer,
each row on its own screen row and a row that overflows WIDTH wrapping onto
the next (PAINT-ROWS)."
  ;; BUFFER, when given and of the right size, is blanked and parsed into —
  ;; the painter hands back the spare it kept from the frame before, so a
  ;; frame allocates no grid at all.
  (paint-rows (reusable-screen-buffer buffer width height) rows :wrap t))

;;; --- the diff painter ----------------------------------------------------

(defun render-diff (old new stream &aux (w new.width)
                                        (h new.height)
                                        (cur-x 0) (cur-y 0)
                                        (wrote nil)
                                        (fg nil) (bg nil) (attrs 0) (link nil))
  "Write to STREAM the escape bytes that turn buffer OLD (NIL for a full
paint) into buffer NEW: only changed cells, with the shortest cursor motion
between them, an SGR only when the style changes, OSC 8 opened and closed
around linked cells, and the style reset at the end."
  (dotimes (y h)
    (loop with x = 0 while (< x w)
          do (let* ((cell (screen-buffer-ref new x y))
                    (before (and old (< y old.height) (< x old.width) (screen-buffer-ref old x y)))
                    (step (max 1 cell.width)))
               (unless (and before (or (eq before cell)
                                       (cell-equal before cell)))
                 (unless (and (= cur-x x) (= cur-y y))
                   (cond ((/= cur-y y)
                          (format stream "~C[~D;~DH" #\Escape (1+ y) (1+ x)))
                         ((= x 0) (write-char #\Return stream))
                         ((> x cur-x) (format stream "~C[~DC" #\Escape (- x cur-x)))
                         (t (format stream "~C[~DD" #\Escape (- cur-x x))))
                   (setf cur-x x cur-y y))
                 (when (or (not (equal cell.fg fg))
                           (not (equal cell.bg bg))
                           (/= cell.attrs attrs))
                   (format stream "~C[0~{;~A~}m" #\Escape
                           (nconc (loop for (nil text bit) in +sgr-attributes+
                                        when (logtest cell.attrs bit) collect text)
                                  (when cell.fg (list cell.fg))
                                  (when cell.bg (list cell.bg))))
                   (setf fg cell.fg bg cell.bg attrs cell.attrs))
                 (unless (equal cell.link link)
                   (format stream "~C]8;;~@[~A~]~C\\" #\Escape cell.link #\Escape)
                   (setf link cell.link))
                 (unless (zerop cell.width)
                   (write-char cell.char stream))
                 (incf cur-x step)
                 (setf wrote t))
               (incf x step))))
  (when (and wrote (or fg bg (/= attrs 0)))
    (format stream "~C[0m" #\Escape))
  (when (and wrote link)
    (format stream "~C]8;;~C\\" #\Escape #\Escape)))
