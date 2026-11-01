;;;; gif.lisp --- GIF read here: its first frame, as PIL shows it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; FIT-IMAGE (image-fit.lisp) shrinks a picture once it has its pixels,
;;;; and no library the build carries reads GIF: without this file a GIF --
;;;; a screen recording, a meme, a chart a tool exported -- needs
;;;; ImageMagick or ffmpeg on the box. GIF-DECODE reads a GIF's first frame:
;;;; its LZW codes, the frame's own palette or the global one, the
;;;; transparent index of the graphic control extension before it, and
;;;; interlaced rows. The frame lands on its logical screen as PIL lays it
;;;; (Pillow's GifImagePlugin): the screen widened to a frame that
;;;; overhangs it, the rest of it the transparent index when there is one,
;;;; else index 0, and an index past the palette gray, as Pillow's palette
;;;; ramp leaves it. A file cut short keeps what it decoded.

(in-package #:nodecode.evolved)

(define-error gif-malformed (error) (reason)
  (:report "~a" reason)
  (:documentation
   "Bytes GIF-DECODE cannot read as a GIF, REASON saying why."))

(defun %gif-fail (control &rest arguments)
  (error 'gif-malformed :reason (apply #'format nil control arguments)))

(declaim (inline %gif-u16))
(defun %gif-u16 (data at)
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum at))
  (logior (aref data at) (ash (aref data (1+ at)) 8)))

(defun %gif-sub-blocks (data pos end)
  "The data sub-blocks from POS in DATA, each a length byte and that many
bytes, up to the empty one: => (values their bytes joined, the position past
them). A sub-block the end of DATA cuts short is dropped, as Pillow drops it."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum pos end))
  (let ((spans '()) (total 0))
    (declare (type fixnum total))
    (loop
      (when (>= pos end) (return))
      (let ((size (aref data pos)))
        (incf pos)
        (when (or (zerop size) (> (+ pos size) end)) (setf pos (min end (+ pos size))) (return))
        (push (cons pos size) spans)
        (incf total size)
        (incf pos size)))
    (let ((out (make-array total :element-type '(unsigned-byte 8))) (at 0))
      (loop for (from . size) in (nreverse spans)
            do (replace out data :start1 at :start2 from :end2 (+ from size)) (incf at size))
      (values out pos))))

(defun %gif-lzw (codes min-size count)
  "The first COUNT pixel indices the LZW CODES make with MIN-SIZE bits a
literal, as Pillow's GifDecode.c makes them: a code size that grows as the
table reaches it, to 12 bits, and a table that stops growing at 4096 codes
until a clear code. => (values INDICES DECODED), DECODED the indices the
codes held, at most COUNT."
  (declare (type (simple-array (unsigned-byte 8) (*)) codes) (type (integer 1 11) min-size)
           (type fixnum count) (optimize speed (safety 1)))
  (let* ((out (make-array count :element-type '(unsigned-byte 8) :initial-element 0))
         (prefix (make-array 4096 :element-type '(unsigned-byte 16) :initial-element 0))
         (suffix (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
         (head (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
         (lengths (make-array 4096 :element-type '(unsigned-byte 16) :initial-element 0))
         (clear (ash 1 min-size)) (eoi (1+ clear))
         (size (1+ min-size)) (next (+ clear 2)) (prev -1)
         (pos 0) (end (length codes)) (buf 0) (bits 0) (at 0))
    (declare (type fixnum clear eoi size next prev pos end bits at) (type (unsigned-byte 32) buf))
    (dotimes (i clear)
      (setf (aref suffix i) (logand i #xFF) (aref head i) (logand i #xFF) (aref lengths i) 1))
    (flet ((emit (code)
             ;; the string of CODE into OUT at AT, back to front along its prefixes
             (declare (type fixnum code))
             (let ((n (aref lengths code)) (c code))
               (declare (type fixnum n c))
               (loop for k of-type fixnum from (1- n) downto 0
                     do (let ((p (+ at k)))
                          (when (< p count) (setf (aref out p) (aref suffix c))))
                        (setf c (aref prefix c)))
               (incf at n))))
      (loop
        (loop while (and (< bits size) (< pos end))
              do (setf buf (logior buf (ash (aref codes pos) bits)))
                 (incf pos)
                 (incf bits 8))
        (when (< bits size) (return))
        (let ((code (logand buf (1- (ash 1 size)))))
          (declare (type fixnum code))
          (setf buf (ash buf (- size)) bits (- bits size))
          (cond ((= code clear)
                 (setf size (1+ min-size) next (+ clear 2) prev -1))
                ((= code eoi) (return))
                ((< prev 0)
                 (when (> code clear) (return))
                 (emit code)
                 (setf prev code))
                ((> code next) (return))
                (t
                 ;; the new entry: the previous string and the first index of
                 ;; this one -- which, for the code the table is about to
                 ;; make, is the previous string's own first
                 (when (< next 4096)
                   (setf (aref prefix next) prev
                         (aref suffix next) (aref head (if (< code next) code prev))
                         (aref head next) (aref head prev)
                         (aref lengths next) (min 4095 (1+ (aref lengths prev))))
                   (when (and (= next (1- (ash 1 size))) (< size 12)) (incf size))
                   (incf next))
                 (emit code)
                 (setf prev code)))
          (when (>= at count) (return)))))
    (values out (min at count))))

(defun gif-decode (octets)
  "OCTETS, a GIF, its first frame decoded on its logical screen as PIL
shows it: => (values DATA WIDTH HEIGHT CHANNELS), DATA row-major RGB, or RGBA
when the frame has a transparent index. Signals GIF-MALFORMED, saying why,
for bytes it cannot read."
  (let* ((data (if (typep octets '(simple-array (unsigned-byte 8) (*)))
                   octets
                   (coerce octets '(simple-array (unsigned-byte 8) (*)))))
         (end (length data)))
    (unless (and (>= end 13) (every (lambda (c i) (= (char-code c) (aref data i))) "GIF8" '(0 1 2 3))
                 (member (aref data 4) '(55 57)) (= (aref data 5) 97))
      (%gif-fail "its bytes do not open a GIF"))
    (let ((screen-width (%gif-u16 data 6)) (screen-height (%gif-u16 data 8))
          (pos 13) (transparent nil))
      (flet ((palette (flags what)
               ;; the palette FLAGS say follows at POS, read past; NIL when there is none
               (when (logbitp 7 flags)
                 (let ((size (* 3 (ash 2 (logand flags 7)))))
                   (when (> (+ pos size) end) (%gif-fail "~a is cut short" what))
                   (prog1 (subseq data pos (+ pos size)) (incf pos size))))))
        (loop with global = (palette (aref data 10) "its global palette") do
          (when (>= pos end) (%gif-fail "it ends before its first frame"))
          (let ((tag (aref data pos)))
            (incf pos)
            (case tag
              (#x21
               (when (>= pos end) (%gif-fail "it ends inside an extension"))
               (let ((label (aref data pos)))
                 (incf pos)
                 (when (and (= label #xF9) (< (+ pos 4) end) (>= (aref data pos) 4)
                            (logbitp 0 (aref data (+ pos 1))))
                   (setf transparent (aref data (+ pos 4))))
                 (setf pos (nth-value 1 (%gif-sub-blocks data pos end)))))
              (#x2C
               (when (> (+ pos 10) end) (%gif-fail "its first frame's descriptor is cut short"))
               (let* ((left (%gif-u16 data pos)) (top (%gif-u16 data (+ pos 2)))
                      (width (%gif-u16 data (+ pos 4))) (height (%gif-u16 data (+ pos 6)))
                      (flags (aref data (+ pos 8)))
                      (canvas-width (max screen-width (+ left width)))
                      (canvas-height (max screen-height (+ top height))))
                 (incf pos 9)
                 (when (or (zerop canvas-width) (zerop canvas-height))
                   (%gif-fail "it is ~dx~d" canvas-width canvas-height))
                 (when (> (* canvas-width canvas-height) *image-max-pixels*)
                   (%gif-fail "it is ~:dx~:d, past the ~:d pixels one image is opened at"
                              canvas-width canvas-height *image-max-pixels*))
                 (setf global (or (palette flags "its first frame's palette") global))
                 (when (>= pos end) (%gif-fail "its first frame has no image data"))
                 (let ((min-size (aref data pos)))
                   (unless (<= 1 min-size 11)
                     (%gif-fail "its LZW codes start at ~d bits" min-size))
                   (return
                     (%gif-canvas (%gif-lzw (%gif-sub-blocks data (1+ pos) end) min-size (* width height))
                                  left top width height (logbitp 6 flags)
                                  canvas-width canvas-height global transparent)))))
              (#x3B (%gif-fail "it holds no frame")))))))))

(defun %gif-canvas (indices left top width height interlaced canvas-width canvas-height palette transparent)
  "The frame's INDICES, WIDTH x HEIGHT, laid at LEFT, TOP on a CANVAS-WIDTH x
CANVAS-HEIGHT screen of the TRANSPARENT index or 0, its rows in the
interlaced order when INTERLACED, and coloured by PALETTE: => (values DATA
CANVAS-WIDTH CANVAS-HEIGHT CHANNELS)."
  (declare (type (simple-array (unsigned-byte 8) (*)) indices)
           (type fixnum left top width height canvas-width canvas-height))
  (let* ((channels (if transparent 4 3))
         (fill (or transparent 0))
         (screen (make-array (* canvas-width canvas-height) :element-type '(unsigned-byte 8)
                                                            :initial-element fill))
         (rows (if interlaced
                   (coerce (loop for (start step) in '((0 8) (4 8) (2 4) (1 2))
                                 nconc (loop for r from start below height by step collect r))
                           'simple-vector)
                   nil))
         (colours (make-array 768 :element-type '(unsigned-byte 8)))
         (out (make-array (* canvas-width canvas-height channels) :element-type '(unsigned-byte 8))))
    (dotimes (i 256)
      (dotimes (k 3)
        (setf (aref colours (+ (* 3 i) k))
              (if (and palette (< (+ (* 3 i) k) (length palette))) (aref palette (+ (* 3 i) k)) i))))
    (dotimes (i height)
      (let ((y (+ top (if rows (svref rows i) i))))
        (replace screen indices :start1 (+ (* y canvas-width) left) :start2 (* i width) :end2 (* (1+ i) width))))
    (dotimes (p (* canvas-width canvas-height))
      (let ((index (aref screen p)) (o (* p channels)))
        (setf (aref out o) (aref colours (* 3 index))
              (aref out (+ o 1)) (aref colours (+ 1 (* 3 index)))
              (aref out (+ o 2)) (aref colours (+ 2 (* 3 index))))
        (when transparent
          (setf (aref out (+ o 3)) (if (= index transparent) 0 255)))))
    (values out canvas-width canvas-height channels)))

(defun gif-pixels (octets)
  "OCTETS, a GIF, its first frame decoded to RGB or RGBA PIXELS for
FIT-IMAGE: *IMAGE-DECODERS*'s image/gif. Signals IMAGE-UNFIT, saying why,
for bytes it cannot read -- and for any other error inside the decoder, so a
file no one foresaw costs the picture, never the turn."
  (handler-case (multiple-value-bind (data width height channels) (gif-decode octets)
                  (%make-pixels width height channels data))
    (error (e)
      (error 'image-unfit :reason (format nil "the GIF does not decode here: ~a" e)))))
