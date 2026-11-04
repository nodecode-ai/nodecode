;;;; raster.lisp --- pictures for terminals without kitty graphics.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two ways to draw a picture where the kitty protocol is not spoken. SIXEL
;;;; (Windows Terminal since 1.22, foot, WezTerm, mlterm, xterm -ti vt340,
;;;; Konsole) takes the pixels themselves, quantized to a palette, as one DCS
;;;; string written at the cursor: the image lives in the cells it covers, so
;;;; text written into one of them erases that part of it, and nothing
;;;; transmits it once and places it again. HALF BLOCKS need no protocol at
;;;; all: each cell is the upper-half block in the colour of one pixel over a
;;;; ground in the colour of the one under it, which any terminal that keeps
;;;; its colours draws (the classic Windows console among them). Both start
;;;; from RASTER-COLORS, one packed RGB per pixel or -1 where it shows through.

(in-package #:nodecode.term)

;;; --- pixels to colours ----------------------------------------------------

(declaim (inline %pack-rgb))
(defun %pack-rgb (r g b)
  "R, G, B packed into one fixnum, red highest."
  (logior (ash r 16) (ash g 8) b))

(defun unpack-rgb (color)
  "The (r g b) list of a packed COLOR."
  (list (ldb (byte 8 16) color) (ldb (byte 8 8) color) (ldb (byte 8 0) color)))

(defun raster-colors (data width height channels &optional canvas)
  "One packed RGB per pixel of DATA, WIDTH x HEIGHT pixels of CHANNELS 8-bit
samples (1 gray, 2 gray and alpha, 3 RGB, 4 RGBA), row-major: -1 where a
pixel is less than half opaque, and a partly transparent one laid over
CANVAS, an (r g b), or over black when CANVAS is NIL."
  ;; Half opaque is the line because a cell can only show a pixel or what is
  ;; under it: a sixel bit is set or not, and a half block's half is drawn or
  ;; left to the ground.
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum width height channels))
  (destructuring-bind (cr cg cb) (or canvas '(0 0 0))
    (let ((out (make-array (* width height) :element-type 'fixnum)))
      (flet ((over (v c a) (round (+ (* v a) (* c (- 255 a))) 255)))
        (dotimes (i (* width height) out)
          (let* ((at (* i channels))
                 (gray (< channels 3))
                 (r (aref data at))
                 (g (if gray r (aref data (+ at 1))))
                 (b (if gray r (aref data (+ at 2))))
                 (a (case channels (2 (aref data (+ at 1))) (4 (aref data (+ at 3))) (t 255))))
            (setf (aref out i)
                  (cond ((< a 128) -1)
                        ((= a 255) (%pack-rgb r g b))
                        (t (%pack-rgb (over r cr a) (over g cg a) (over b cb a)))))))))))

;;; --- a palette: median cut over a 15-bit histogram -----------------------
;;; A sixel names its colours by register, and a terminal holds a few hundred
;;; at most (Windows Terminal and xterm 256). The colours are binned at five
;;; bits a channel, and boxes of bins are halved at their weighted median along
;;; their widest channel until there are as many as registers: a chart or a
;;; screenshot, which has fewer bins than that, keeps every colour it has, and a
;;; photo's are spread where its colours are.

(defparameter *sixel-colors* 256
  "Colour registers a sixel image uses at most.")

(declaim (inline %bin-of %bin-channel))
(defun %bin-of (color)
  "The 15-bit histogram bin of a packed COLOR."
  (logior (ash (ldb (byte 5 19) color) 10) (ash (ldb (byte 5 11) color) 5) (ldb (byte 5 3) color)))

(defun %bin-channel (bin channel)
  "BIN's five-bit value on CHANNEL, 0 red, 1 green, 2 blue."
  (ldb (byte 5 (* 5 (- 2 channel))) bin))

(defun %widest-channel (box)
  "(values CHANNEL RANGE): the channel BOX's bins spread furthest along."
  (let ((best 0) (range -1))
    (dotimes (channel 3 (values best range))
      (let ((lo 31) (hi 0))
        (loop for bin across box
              do (let ((v (%bin-channel bin channel))) (setf lo (min lo v) hi (max hi v))))
        (when (> (- hi lo) range) (setf best channel range (- hi lo)))))))

(defun %split-box (box counts)
  "BOX's bins, sorted along their widest channel, halved where half their
pixels lie on either side: (values LOWER UPPER), neither empty."
  (let* ((channel (%widest-channel box))
         (sorted (sort (copy-seq box) #'< :key (lambda (bin) (%bin-channel bin channel))))
         (half (/ (loop for bin across sorted sum (aref counts bin)) 2))
         (cut (loop with seen = 0
                    for index from 0 below (1- (length sorted))
                    do (incf seen (aref counts (aref sorted index)))
                    when (>= seen half) return index
                    finally (return (- (length sorted) 2)))))
    (values (subseq sorted 0 (1+ cut)) (subseq sorted (1+ cut)))))

(defun quantize-colors (colors &optional (limit *sixel-colors*))
  "COLORS, packed RGBs with -1 for a clear pixel (RASTER-COLORS), brought to
at most LIMIT colours: (values INDICES PALETTE), INDICES one palette index
per pixel, -1 where clear, PALETTE a vector of (r g b), each its bins'
pixels' mean."
  (declare (type (simple-array fixnum (*)) colors))
  (let ((counts (make-array 32768 :element-type 'fixnum :initial-element 0))
        (sums (make-array (* 3 32768) :element-type 'fixnum :initial-element 0)))
    (loop for color across colors
          unless (minusp color)
            do (let ((bin (%bin-of color)))
                 (incf (aref counts bin))
                 (incf (aref sums (* 3 bin)) (ldb (byte 8 16) color))
                 (incf (aref sums (+ 1 (* 3 bin))) (ldb (byte 8 8) color))
                 (incf (aref sums (+ 2 (* 3 bin))) (ldb (byte 8 0) color))))
    (let ((boxes (list (coerce (loop for bin below 32768 when (plusp (aref counts bin)) collect bin)
                               'simple-vector))))
      ;; The box split next is the one spread widest: a few pixels of a colour
      ;; far from the rest keep a register of their own, where splitting by
      ;; population would spend every register on a page's white.
      (loop while (< (length boxes) limit)
            do (let ((box (loop with best = nil and best-range = 0
                                for box in boxes
                                for range = (nth-value 1 (%widest-channel box))
                                when (and (> (length box) 1) (> range best-range))
                                  do (setf best box best-range range)
                                finally (return best))))
                 (unless box (return))
                 (multiple-value-bind (lower upper) (%split-box box counts)
                   (setf boxes (list* lower upper (remove box boxes :test #'eq))))))
      (let ((map (make-array 32768 :element-type 'fixnum :initial-element -1))
            (palette (make-array (length (remove 0 boxes :key #'length)) :fill-pointer 0)))
        (dolist (box boxes)
          (when (plusp (length box))
            (let ((n 0) (r 0) (g 0) (b 0) (index (fill-pointer palette)))
              (loop for bin across box
                    do (incf n (aref counts bin))
                       (incf r (aref sums (* 3 bin)))
                       (incf g (aref sums (+ 1 (* 3 bin))))
                       (incf b (aref sums (+ 2 (* 3 bin))))
                       (setf (aref map bin) index))
              (vector-push (list (round r n) (round g n) (round b n)) palette))))
        (values (map '(simple-array fixnum (*))
                     (lambda (color) (if (minusp color) -1 (aref map (%bin-of color))))
                     colors)
                (coerce palette 'simple-vector))))))

;;; --- sixel ----------------------------------------------------------------

(defun %write-sixel-run (out char count)
  "COUNT of the sixel CHAR, run-length coded once a repeat is shorter."
  (if (> count 3)
      (format out "!~D~C" count char)
      (loop repeat count do (write-char char out))))

(defun sixel-data (indices palette width height)
  "The DCS string that draws INDICES, WIDTH x HEIGHT palette indices with -1
for a clear pixel (QUANTIZE-COLORS), at the cursor: 1:1 pixels, the
PALETTE in registers by percentage, and clear pixels left as the cells
under them are."
  ;; P2 = 1 keeps the cells' own ground under a clear pixel; the raster
  ;; attributes say 1:1 and the size, so no terminal guesses an aspect. Each
  ;; band of six rows is one line per colour present in it, `$' between them
  ;; and `-' to the next band, every line cut at its last set bit.
  (declare (type (simple-array fixnum (*)) indices) (type fixnum width height))
  (let* ((colors (length palette))
         (bits (make-array (* (max colors 1) width) :element-type '(unsigned-byte 8) :initial-element 0))
         (present (make-array (max colors 1) :element-type 'bit :initial-element 0)))
    (with-output-to-string (out)
      (format out "~CP0;1;0q\"1;1;~D;~D" #\Escape width height)
      (loop for (r g b) across palette
            for register from 0
            do (format out "#~D;2;~D;~D;~D" register (round (* r 100) 255) (round (* g 100) 255) (round (* b 100) 255)))
      (loop for top from 0 below height by 6
            do (let ((used '()))
                 (loop for y from top below (min height (+ top 6))
                       for bit = (ash 1 (- y top))
                       do (dotimes (x width)
                            (let ((color (aref indices (+ (* y width) x))))
                              (unless (minusp color)
                                (when (zerop (aref present color))
                                  (setf (aref present color) 1)
                                  (push color used))
                                (setf (aref bits (+ (* color width) x))
                                      (logior bit (aref bits (+ (* color width) x))))))))
                 (loop for (color . more) on (sort used #'<)
                       for start = (* color width)
                       for end = (loop for x from (+ start width -1) downto start
                                       unless (zerop (aref bits x)) return (1+ x)
                                       finally (return start))
                       do (format out "#~D" color)
                          (loop with run-char = nil and run = 0
                                for x from start below end
                                for char = (code-char (+ 63 (aref bits x)))
                                do (if (eql char run-char)
                                       (incf run)
                                       (progn (when run-char (%write-sixel-run out run-char run))
                                              (setf run-char char run 1)))
                                finally (when run-char (%write-sixel-run out run-char run)))
                          (when more (write-char #\$ out))
                          (fill bits 0 :start start :end (+ start width))
                          (setf (aref present color) 0))
                 (when (< (+ top 6) height) (write-char #\- out))))
      (format out "~C\\" #\Escape))))

;;; --- half blocks ----------------------------------------------------------

(defun half-block-grid (colors width height sgr)
  "COLORS, WIDTH x HEIGHT packed RGBs with -1 for clear (RASTER-COLORS), as
cells: a vector of rows, each a vector of WIDTH entries, every one NIL where
both its pixels are clear or (CHAR FG BG), the upper half block over the
lower pixel's ground, the lower one alone over :KEEP, the cell's own
ground. SGR answers a packed colour's SGR text, foreground when its second
argument is true."
  (let ((cache (make-hash-table)))
    (flet ((text (color foreground-p)
             (alexandria:ensure-gethash (if foreground-p color (- -2 color)) cache
                                        (funcall sgr color foreground-p))))
      (coerce
       (loop for top from 0 below height by 2
             collect (coerce
                      (loop for x below width
                            for up = (aref colors (+ (* top width) x))
                            for down = (if (< (1+ top) height) (aref colors (+ (* (1+ top) width) x)) -1)
                            collect (cond ((and (minusp up) (minusp down)) nil)
                                          ((minusp down) (list #\UPPER_HALF_BLOCK (text up t) :keep))
                                          ((minusp up) (list #\LOWER_HALF_BLOCK (text down t) :keep))
                                          (t (list #\UPPER_HALF_BLOCK (text up t) (text down nil)))))
                      'simple-vector))
       'simple-vector))))

;;; --- the cells a picture covers ------------------------------------------
;;; The renderer's half of the bargain a sixel strikes: its pixels live in the
;;; cells, so a cell the diff writes loses them, and a picture that moves
;;; leaves them behind until something writes over the cells it left.

(defun %placement-cells (p buffer function)
  "Call FUNCTION on each cell of BUFFER the placement P covers."
  (let* ((image (image-placement-image p))
         (x0 (image-placement-x p))
         (y0 (image-placement-y p))
         (x1 (min (screen-buffer-width buffer) (+ x0 (or (terminal-image-columns image) 0))))
         (y1 (min (screen-buffer-height buffer)
                  (+ y0 (or (image-placement-rows p) (terminal-image-rows image) 0)))))
    (loop for y from (max 0 y0) below y1
          do (loop for x from (max 0 x0) below x1
                   do (funcall function (screen-buffer-ref buffer x y))))))

(defun %protocol-p (p protocol)
  "Whether the placement P draws through PROTOCOL."
  (eq protocol (terminal-image-protocol (image-placement-image p))))

(defun forget-departed-sixels (old-buffer old new)
  "Mark the cells of OLD-BUFFER under each sixel placement of OLD that NEW
drops as painted by nothing, so the diff writes them again: text written
over a sixel's cells is what takes it off the screen."
  (when old-buffer
    (dolist (p old)
      (when (and (%protocol-p p :sixel) (not (member p new :test #'equalp)))
        (%placement-cells p old-buffer (lambda (cell) (setf (cell-attrs cell) -1)))))))

(defun %cells-differ-p (old-buffer new-buffer p)
  "Whether any cell the placement P covers paints otherwise in NEW-BUFFER
than in OLD-BUFFER."
  (let* ((image (image-placement-image p))
         (x0 (image-placement-x p))
         (y0 (image-placement-y p))
         (x1 (min (screen-buffer-width new-buffer) (screen-buffer-width old-buffer)
                  (+ x0 (or (terminal-image-columns image) 0))))
         (y1 (min (screen-buffer-height new-buffer) (screen-buffer-height old-buffer)
                  (+ y0 (or (image-placement-rows p) (terminal-image-rows image) 0)))))
    (loop for y from (max 0 y0) below y1
            thereis (loop for x from (max 0 x0) below x1
                            thereis (not (cell-equal (screen-buffer-ref old-buffer x y)
                                                     (screen-buffer-ref new-buffer x y)))))))

(defun damaged-sixels (old-buffer new-buffer old new)
  "The sixel placements of NEW that OLD already had and the diff from
OLD-BUFFER to NEW-BUFFER will write over: each is drawn again after it."
  (when old-buffer
    (loop for p in new
          when (and (%protocol-p p :sixel)
                    (member p old :test #'equalp)
                    (%cells-differ-p old-buffer new-buffer p))
            collect p)))

(defun paint-block-images (buffer placements)
  "Lay each half-block placement of PLACEMENTS into BUFFER's cells, over what
the rows painted there."
  (dolist (p placements buffer)
    (when (%protocol-p p :blocks)
      (let ((grid (terminal-image-data (image-placement-image p))))
        (loop for row across grid
              for y from (image-placement-y p)
              repeat (or (image-placement-rows p) (length grid))
              while (< y (screen-buffer-height buffer))
              do (loop for spec across row
                       for x from (image-placement-x p)
                       while (< x (screen-buffer-width buffer))
                       when (and spec (>= x 0) (>= y 0))
                         do (destructuring-bind (char fg bg) spec
                              (let ((cell (screen-buffer-ref buffer x y)))
                                (unseat-cell buffer x y)
                                (set-cell cell char 1 fg (if (eq bg :keep) (cell-bg cell) bg) 0 nil)))))))))
