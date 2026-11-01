;;;; sheet.lisp --- a sheet's canvas: the cells a dialog in the middle of the
;;;; frame paints, its inks, the cells a click acts on, and the line its key
;;;; row says.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; /settings and /generations are sheets (tui/dialog.lisp SHEET-DIALOG). Each paints
;;;; its window cell by cell on a canvas as its own layout says it, names the
;;;; cells a click acts on as it paints them, and runs its calls off the loop
;;;; with what they are doing, then what they did, on its key row.

(in-package #:nodecode.evolved)

;;; --- the canvas -----------------------------------------------------------------
;;; A sheet is painted cell by cell, as its layout is said: a name at one
;;; column, its value at the right edge, options from a third. Each cell holds
;;; a character and its ink; the second cell of a wide glyph holds NIL.

(defstruct (sheet-canvas (:constructor %make-sheet-canvas (width height chars inks)))
  width height chars inks)

(nlk:access (canvas sheet-canvas))

(defun make-sheet-canvas (width height)
  (%make-sheet-canvas width height
                      (make-array (list height width) :initial-element #\Space)
                      (make-array (list height width) :initial-element nil)))

(defun canvas-unseat (canvas x y)
  "Make the cell at X, Y stand alone before it is painted over: the other
half of a wide glyph it belongs to becomes a blank."
  (let ((chars canvas.chars))
    (cond ((and (null (aref chars y x)) (plusp x))
           (setf (aref chars y (1- x)) #\Space))
          ((and (characterp (aref chars y x)) (= 2 (char-display-width (aref chars y x)))
                (< (1+ x) canvas.width))
           (setf (aref chars y (1+ x)) #\Space)))))

(defun canvas-fill (canvas x y width height ink)
  "Blank WIDTH x HEIGHT cells from X, Y in INK, inside CANVAS."
  (loop for row from (max 0 y) below (min canvas.height (+ y height))
        do (loop for column from (max 0 x) below (min canvas.width (+ x width))
                 do (canvas-unseat canvas column row)
                    (setf (aref canvas.chars row column) #\Space
                          (aref canvas.inks row column) ink))))

(defun canvas-put (canvas x y text ink &optional (end canvas.width))
  "TEXT in INK from column X of row Y, stopping at the first glyph that would
pass column END (the canvas's right edge by default). => the column after
the last cell painted."
  (loop for ch across text
        for width = (char-display-width ch)
        while (<= (+ x width) (min end canvas.width))
        when (plusp width)
          do (when (and (<= 0 x) (< -1 y canvas.height))
               (canvas-unseat canvas x y)
               (setf (aref canvas.chars y x) ch (aref canvas.inks y x) ink)
               (when (= width 2)
                 (canvas-unseat canvas (1+ x) y)
                 (setf (aref canvas.chars y (1+ x)) nil (aref canvas.inks y (1+ x)) ink)))
             (incf x width))
  x)

(defun canvas-rows (canvas)
  "CANVAS as row strings: each run of one ink painted in it."
  (loop for row below canvas.height
        collect (let ((out (make-string-output-stream))
                      (run (make-string-output-stream))
                      (ink :none))
                  (flet ((flush ()
                           (let ((text (get-output-stream-string run)))
                             (when (plusp (length text))
                               (write-string (styled text (and (visual-style-p ink) ink)) out)))))
                    (loop for column below canvas.width
                          for ch = (aref canvas.chars row column)
                          when ch
                            do (unless (eq ink (aref canvas.inks row column))
                                 (flush)
                                 (setf ink (aref canvas.inks row column)))
                               (write-char ch run))
                    (flush))
                  (get-output-stream-string out))))

(defvar *sheet-inks* (cons nil nil)
  "(PALETTE . TABLE): the inks the sheet has painted with under PALETTE, so
one ink is one object and a run of it one SGR span.")

(defun sheet-ink (tone &key ground bold edge)
  "TONE's ink on GROUND (an rgb, or NIL): its own background when it has
one, else GROUND's; BOLD as asked or as the tone has it. TONE may be an rgb
too, a colour the sheet derives from the palette, its ink that colour on
GROUND. With EDGE, the ink of a half block of GROUND, its other half the
cell behind."
  (unless (eq (car *sheet-inks*) *palette*)
    (setf *sheet-inks* (cons *palette* (make-hash-table :test 'equal))))
  (let ((key (list tone ground bold edge)))
    (or (gethash key (cdr *sheet-inks*))
        (setf (gethash key (cdr *sheet-inks*))
              (cond (edge (vstyle :fg ground))
                    ((consp tone) (vstyle :fg tone :bg ground :bold bold))
                    (t (let ((style (and tone (style-for-tone tone))))
                         (vstyle :fg (and style (visual-style-fg style))
                                 :bg (or (and style (visual-style-bg style)) ground)
                                 :bold (or bold (and style (visual-style-bold style)))))))))))

(defun sheet-ground (tone)
  "The rgb TONE's background is."
  (visual-style-bg (style-for-tone tone)))

;;; --- painting -----------------------------------------------------------------

(defun sheet-hit (sheet row from to action &key hover)
  "Make the cells FROM below TO of ROW a pointer target: a click there runs
ACTION with (MODEL SHEET), the pointer resting there HOVER, which answers
whether it moved anything. Either may be NIL."
  (push (list row from to action hover) sheet.hits))

(defun paint-lanes (canvas edges lanes)
  "Fill each (X WIDTH GROUND) of LANES down CANVAS: with EDGES its first and
last rows are half blocks of the ground, the cell behind showing in their
other half. => (values TOP INNER), the first row inside and how many."
  (let ((none (eq (effective-color-profile) :none))
        (height canvas.height))
    (loop for (x width ground) in lanes
          do (if edges
                 (progn (canvas-fill canvas x 1 width (- height 2) (sheet-ink nil :ground ground))
                        (unless none
                          (canvas-put canvas x 0 (make-string width :initial-element #\▄)
                                      (sheet-ink nil :ground ground :edge t))
                          (canvas-put canvas x (1- height) (make-string width :initial-element #\▀)
                                      (sheet-ink nil :ground ground :edge t))))
                 (canvas-fill canvas x 0 width height (sheet-ink nil :ground ground))))
    (if edges (values 1 (- height 2)) (values 0 height))))

(defun clip-tail (text width)
  "The last WIDTH columns of TEXT, an … leading when it was cut."
  (if (<= (text-display-width text) width)
      text
      (let ((kept (reverse (take-display-prefix (reverse text) (max 0 (1- width))))))
        (concatenate 'string "…" kept))))

(defun paint-keys (sheet canvas row from to keys flash ground)
  "KEYS, (KEY WORD) in their order, as filled caps and their words on ROW
from column FROM, as many whole as fit before TO and FLASH, which stands
flush against TO: a cap naming one key, and its word, SHEET's to click for
that key (HINT-KEY-PRESS)."
  (let* ((flash-text (car flash))
         (room (- to from (if flash-text (+ 2 (text-display-width flash-text)) 0)))
         (used 0)
         (at from))
    (loop for (key word) in keys
          for width = (+ (text-display-width key) 3 (text-display-width word) (if (plusp used) 2 0))
          while (<= (+ used width) room)
          do (when (plusp used) (incf at 2))
             (let ((start at))
               (setf at (canvas-put canvas at row (format nil " ~a " key) (sheet-ink :key-chip)))
               (setf at (canvas-put canvas at row (format nil " ~a" word) (sheet-ink :muted :ground ground)))
               ;; The click answers the key press itself: the release hands it
               ;; to UPDATE, which runs it as a typed one.
               (when (hint-key-press key)
                 (let ((key key))
                   (sheet-hit sheet row start at
                              (lambda (model sheet)
                                (declare (ignore model sheet))
                                (hint-key-press key))))))
             (incf used width))
    (when (and flash-text (<= from (- to (text-display-width flash-text))))
      (canvas-put canvas (- to (text-display-width flash-text)) row flash-text
                  (sheet-ink (cadr flash) :ground ground)))))

;;; --- the pointer --------------------------------------------------------------

(defun sheet-target (sheet x y which)
  "The WHICH (:action or :hover) of the first target the last paint put on
the frame cell X, Y of SHEET, or NIL."
  (destructuring-bind (rows left top width) sheet.layout
    (declare (ignore rows width))
    (let ((column (- x left)) (row (- y top)))
      (loop for (at from to action hover) in sheet.hits
            for target = (if (eq which :hover) hover action)
            when (and target (= at row) (<= from column) (< column to))
              return target))))

(defun sheet-click (model sheet x y)
  "A left click on the frame cell X, Y of SHEET: the target the last
paint put there, if any."
  (when-let (action (sheet-target sheet x y :action))
    (funcall action model sheet)))

(defun sheet-hover (model sheet x y)
  "The pointer resting on the frame cell X, Y of SHEET: the hover the last
paint put there, if any. => whether it moved anything."
  (when-let (hover (sheet-target sheet x y :hover))
    (funcall hover model sheet)))

;;; --- the key row's line, and calls off the loop ------------------------------

(defparameter +sheet-flash-ms+ 3000
  "How long a saved line stays on the sheet's key row.")

(defun sheet-flash (sheet text tone &key stays)
  "Put TEXT in TONE at the right of the key row: for +SHEET-FLASH-MS+, or
while it STAYS, until a call replaces it."
  (setf sheet.flash (list* text tone (if stays nil (get-internal-real-time)))
        ;; The frame clock runs while a line has a time to go, and only then.
        sheet.on-tick (unless stays #'sheet-tick)))

(defun sheet-tick (model sheet)
  "The frame clock's beat: take a saved line down once its time is up.
=> whether that moved anything."
  (declare (ignore model))
  (let ((born (cddr sheet.flash)))
    (when (or (null born) (>= (since-ms born) +sheet-flash-ms+))
      (setf sheet.flash nil sheet.on-tick nil)
      t)))

(defun sheet-call (model sheet thunk then saying)
  "Run THUNK off the loop, SHEET busy and SAYING so on its key row; THEN
takes (MODEL ANSWER) once it lands, while SHEET is still the dialog
open. A sheet closed or replaced meanwhile takes nothing: a write sent
stays written."
  (setf sheet.busy t)
  (sheet-flash sheet saying :muted :stays t)
  (off-loop model thunk
            (lambda (model answer)
              (if (not (eq sheet (chat-model-dialog model)))
                  :quiet
                  (progn (setf sheet.busy nil sheet.flash nil)
                         (funcall then model answer)
                         t))))
  nil)
