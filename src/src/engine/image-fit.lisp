;;;; image-fit.lisp --- an image as a provider takes it: shrunk, re-encoded.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; pi's resize (earendil-works/pi, packages/coding-agent/src/utils/
;;;; image-resize-core.ts, with the smaller-of fix of its #11577) in Lisp. An
;;;; image past *IMAGE-MAX-SIDE* or *IMAGE-SEND-MAX-BYTES* is decoded, shrunk
;;;; by area and re-encoded, keeping whichever of PNG and JPEG comes out
;;;; smaller, so a photo goes as a JPEG and a screenshot as a PNG; when
;;;; neither fits, JPEG at lower qualities, then a quarter smaller a side. An
;;;; image within both goes as its own bytes. PNG is read and written here,
;;;; JPEG through cl-jpeg; a GIF, a WebP or a progressive JPEG goes through
;;;; whichever transcoder the box has (nlt:*PNG-TRANSCODERS*) and then
;;;; through the PNG reader, and with none installed it goes as itself when
;;;; it is within what a provider takes whole. No codec is a program the box
;;;; must have: PNG and baseline JPEG, what screenshots and cameras write,
;;;; fit on every platform the binary runs on.

(in-package #:nodecode.evolved)

;;; --- the ceilings ----------------------------------------------------------

(defparameter *image-max-side* 2000
  "The longest side, in pixels, an image reaches a model at: pi's, and
Anthropic's for a request that carries more than twenty images. The
providers shrink a larger image themselves, so the pixels past it buy
nothing but upload.")

(defparameter *image-send-max-bytes* (floor (* 9/2 1024 1024 3) 4)
  "Ceiling on one image's bytes as a provider receives them. Anthropic counts
its 5 MiB per image on the base64 text, 4/3 of the bytes, and pi keeps half
a MiB under that: 4.5 MiB of base64, 3,538,944 bytes.")

(defparameter *image-whole-max-bytes* (floor (* 5 1024 1024 3) 4)
  "Ceiling on an image that goes as itself because no codec here opens it:
Anthropic's 5 MiB of base64, with no headroom, the most a provider takes.")

(defparameter *image-whole-max-side* 8000
  "Ceiling on either side of an image that goes as itself unshrunk:
Anthropic's.")

(defparameter *image-max-pixels* (* 8000 8000)
  "Ceiling on the pixels an image is decoded at, read from its header before
any of it is opened: past it the bytes stay shut, so a few kilobytes that
claim 30000 x 30000 cost nothing.")

(defparameter *image-jpeg-qualities* '(80 70 55 40)
  "The JPEG qualities FIT-IMAGE tries, best first: pi's. The first is the one
weighed against PNG.")

(defparameter *image-read-max-bytes* (* 64 1024 1024)
  "Ceiling on one image file LOOK reads to fit: a camera's largest JPEG and
then some, so a file of no image a model could be shown is never read whole.")

(define-error image-unfit (error) (reason)
  (:report "~a" reason)
  (:documentation
   "An image FIT-IMAGE cannot bring within what a provider takes, REASON
saying why in words a refusal can carry."))

;;; --- pixels ------------------------------------------------------------------
;;; Decoded, an image is its samples, row-major, 8 bits each: one channel
;;; for gray, three for RGB, four for RGBA. Every reader below lands here and
;;; every writer starts here.

(defstruct (pixels (:constructor %make-pixels (width height channels data)))
  "WIDTH x HEIGHT pixels of CHANNELS 8-bit samples -- 1 gray, 3 RGB, 4 RGBA --
row-major in DATA."
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (channels 3 :type (integer 1 4))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets))

(defun blank-pixels (width height channels)
  "WIDTH x HEIGHT pixels of CHANNELS samples, all zero."
  (%make-pixels width height channels
                (make-array (* width height channels) :element-type '(unsigned-byte 8))))

(defun big-endian (octets index count)
  "The COUNT bytes of OCTETS at INDEX as one unsigned integer, high byte first."
  (loop for i below count sum (ash (aref octets (+ index i)) (* 8 (- count i 1)))))

;;; --- PNG, read ---------------------------------------------------------------
;;; Every PNG the format allows: gray, RGB, palette, gray with alpha and RGBA,
;;; at each depth it allows, a tRNS colour or table, Adam7 interlaced or not.
;;; Sixteen-bit samples keep their high byte. Ancillary chunks are skipped.

(declaim (inline paeth))
(defun paeth (a b c)
  "The PNG Paeth predictor of left A, up B and upper-left C."
  (declare (type fixnum a b c))
  (let* ((p (- (+ a b) c)) (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a) ((<= pb pc) b) (t c))))

(defun png-unfilter (raw start stride bpp prior)
  "Undo, in place, the filter on the STRIDE bytes of RAW at START, whose type
is the byte before them; PRIOR is where the row above starts, already undone,
or NIL for a pass's first row. BPP is the bytes of one pixel, at least 1."
  (declare (type octets raw) (type fixnum start stride bpp)
           (type (or null fixnum) prior) (optimize speed))
  (macrolet ((at (i) `(aref raw (the fixnum (+ start ,i))))
             (left (i) `(if (>= ,i bpp) (aref raw (the fixnum (+ start (- ,i bpp)))) 0))
             (up (i) `(if prior (aref raw (the fixnum (+ prior ,i))) 0))
             (corner (i) `(if (and prior (>= ,i bpp)) (aref raw (the fixnum (+ prior (- ,i bpp)))) 0)))
    (let ((filter (aref raw (1- start))))
      (case filter
        (0)
        (1 (loop for i of-type fixnum from bpp below stride
                 do (setf (at i) (ldb (byte 8 0) (+ (at i) (left i))))))
        (2 (when prior
             (loop for i of-type fixnum below stride
                   do (setf (at i) (ldb (byte 8 0) (+ (at i) (up i)))))))
        (3 (loop for i of-type fixnum below stride
                 do (setf (at i) (ldb (byte 8 0) (+ (at i) (ash (+ (left i) (up i)) -1))))))
        (4 (loop for i of-type fixnum below stride
                 do (setf (at i) (ldb (byte 8 0) (+ (at i) (paeth (left i) (up i) (corner i)))))))
        (t (error 'image-unfit :reason (format nil "a PNG row names filter ~d, which PNG has not" filter)))))))

(defparameter +adam7+ '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2))
  "Adam7's passes, each (X0 Y0 DX DY).")

(defun png-pixels (octets)
  "The PNG OCTETS decoded to PIXELS: gray without transparency stays one
channel, a palette or RGB without it three, anything with alpha or a tRNS
four. Signals IMAGE-UNFIT on bytes it cannot read."
  (let ((pos 8) (end (length octets)) width height depth color interlace palette transparency (idat '()))
    (loop while (<= (+ pos 12) end)
          do (let* ((length (big-endian octets pos 4))
                    (type (map 'string #'code-char (subseq octets (+ pos 4) (+ pos 8))))
                    (start (+ pos 8))
                    (stop (min end (+ start length))))
               (cond ((string= type "IHDR")
                      (setf width (big-endian octets start 4) height (big-endian octets (+ start 4) 4)
                            depth (aref octets (+ start 8)) color (aref octets (+ start 9))
                            interlace (aref octets (+ start 12))))
                     ((string= type "PLTE") (setf palette (subseq octets start stop)))
                     ((string= type "tRNS") (setf transparency (subseq octets start stop)))
                     ((string= type "IDAT") (push (subseq octets start stop) idat))
                     ((string= type "IEND") (loop-finish)))
               (setf pos (+ stop 4))))
    (flet ((refuse (reason) (error 'image-unfit :reason reason)))
      (unless (and width height (plusp width) (plusp height) idat) (refuse "the PNG has no header or no image data"))
      (unless (member color '(0 2 3 4 6)) (refuse (format nil "PNG colour type ~d is not one PNG has" color)))
      (unless (member depth (case color (0 '(1 2 4 8 16)) (3 '(1 2 4 8)) (t '(8 16))))
        (refuse (format nil "PNG colour type ~d does not come at ~d bits" color depth)))
      (when (> (* width height) *image-max-pixels*)
        (refuse (format nil "it is ~:dx~:d, over the ~:d pixels one image is opened at" width height *image-max-pixels*)))
      (when (and (= color 3) (null palette)) (refuse "the PNG names a palette it does not carry"))
      (let* ((samples (ecase color (0 1) (2 3) (3 1) (4 2) (6 4)))
             (bpp (max 1 (floor (* samples depth) 8)))
             (passes (if (eql interlace 1) +adam7+ '((0 0 1 1))))
             (channels (cond ((or (member color '(4 6)) transparency) 4) ((= color 0) 1) (t 3)))
             (expected (loop for (x0 y0 dx dy) in passes
                             for pw = (ceiling (max 0 (- width x0)) dx)
                             for ph = (ceiling (max 0 (- height y0)) dy)
                             when (and (plusp pw) (plusp ph))
                               sum (* ph (1+ (ceiling (* pw samples depth) 8)))))
             (raw (progn
                    (when (> expected (* 4 *image-max-pixels*))
                      (refuse (format nil "it would open at ~:d bytes" expected)))
                    (handler-case (chipz:decompress nil 'chipz:zlib (apply #'concatenate 'octets (nreverse idat)))
                      (error (e) (refuse (format nil "its image data does not inflate: ~a" e))))))
             (raw (coerce raw 'octets))
             (out (blank-pixels width height channels))
             (data (pixels-data out))
             (offset 0))
        (when (< (length raw) expected) (refuse "its image data stops short"))
        (loop for (x0 y0 dx dy) in passes
              for pw = (ceiling (max 0 (- width x0)) dx)
              for ph = (ceiling (max 0 (- height y0)) dy)
              for stride = (ceiling (* pw samples depth) 8)
              when (and (plusp pw) (plusp ph))
                do (loop for row below ph
                         for start = (+ offset 1 (* row (1+ stride)))
                         do (png-unfilter raw start stride bpp (and (plusp row) (- start stride 1)))
                            (png-put-row raw start pw depth color palette transparency
                                         data channels width x0 dx (+ y0 (* row dy))))
                   (incf offset (* ph (1+ stride))))
        out))))

(defun png-put-row (raw start pw depth color palette transparency data channels width x0 dx y)
  "Write the PW pixels of one undone PNG row of RAW at START into DATA, the
CHANNELS-sample image WIDTH wide, at row Y from column X0 every DX columns."
  (declare (type octets raw data) (type fixnum start pw depth color channels width x0 dx y)
           (optimize speed))
  (let ((base (* (+ (* y width) x0) channels))
        (step (* dx channels)))
    (declare (type fixnum base step))
    ;; The rows nearly every PNG has, copied whole.
    (when (and (= depth 8) (= dx 1) (null transparency)
               (or (and (= color 0) (= channels 1)) (and (= color 2) (= channels 3)) (and (= color 6) (= channels 4))))
      (replace data raw :start1 base :start2 start :end2 (+ start (* pw channels)))
      (return-from png-put-row))
    (let* ((samples (ecase color (0 1) (2 3) (3 1) (4 2) (6 4)))
           (top (1- (ash 1 depth)))
           (key (and transparency (member color '(0 2))
                     (loop for i from 0 below (length transparency) by 2
                           collect (big-endian transparency i 2)))))
      (declare (type fixnum samples top))
      (flet ((sample (i k)
               (declare (type fixnum i k))
               (let ((n (+ (* i samples) k)))
                 (declare (type fixnum n))
                 (case depth
                   (8 (aref raw (+ start n)))
                   (16 (logior (ash (aref raw (+ start (* 2 n))) 8) (aref raw (+ start (* 2 n) 1))))
                   (t (let ((bit (* n depth)))
                        (ldb (byte depth (- 8 depth (mod bit 8))) (aref raw (+ start (floor bit 8)))))))))
             (eight (v)
               (declare (type fixnum v))
               (case depth (16 (ash v -8)) (8 v) (t (floor (* v 255) top)))))
        (loop for i of-type fixnum below pw
              for at of-type fixnum from base by step
              do (multiple-value-bind (r g b a)
                     (ecase color
                       (0 (let* ((v (sample i 0)) (e (eight v)))
                            (values e e e (if (and key (= v (first key))) 0 255))))
                       (2 (let ((r (sample i 0)) (g (sample i 1)) (b (sample i 2)))
                            (values (eight r) (eight g) (eight b)
                                    (if (and key (= r (first key)) (= g (second key)) (= b (third key))) 0 255))))
                       (3 (let ((index (sample i 0)))
                            (if (< (+ (* 3 index) 2) (length palette))
                                (values (aref palette (* 3 index)) (aref palette (+ (* 3 index) 1))
                                        (aref palette (+ (* 3 index) 2))
                                        (if (and transparency (< index (length transparency)))
                                            (aref transparency index)
                                            255))
                                (values 0 0 0 255))))
                       (4 (let ((e (eight (sample i 0)))) (values e e e (eight (sample i 1)))))
                       (6 (values (eight (sample i 0)) (eight (sample i 1)) (eight (sample i 2))
                                  (eight (sample i 3)))))
                   (case channels
                     (1 (setf (aref data at) r))
                     (3 (setf (aref data at) r (aref data (+ at 1)) g (aref data (+ at 2)) b))
                     (t (setf (aref data at) r (aref data (+ at 1)) g (aref data (+ at 2)) b
                              (aref data (+ at 3)) a)))))))))

;;; --- PNG, written ------------------------------------------------------------
;;; Eight bits a sample, gray, RGB or RGBA as the pixels are; each row under
;;; whichever filter leaves its bytes smallest as signed values (libpng's
;;; heuristic); zlib by salza2.

(defparameter +crc-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c))))
  "The CRC-32 of each byte, for PNG's chunk checksums.")

(defun png-chunk (stream type data)
  "Write the PNG chunk of TYPE, four letters, carrying DATA to STREAM."
  (let ((head (map 'octets #'char-code type))
        (crc #xFFFFFFFF))
    (declare (type (unsigned-byte 32) crc))
    (loop for byte across (concatenate 'octets head data)
          do (setf crc (logxor (aref +crc-table+ (logand (logxor crc byte) #xFF)) (ash crc -8))))
    (loop for shift from 24 downto 0 by 8 do (write-byte (ldb (byte 8 shift) (length data)) stream))
    (write-sequence head stream)
    (write-sequence data stream)
    (setf crc (logxor crc #xFFFFFFFF))
    (loop for shift from 24 downto 0 by 8 do (write-byte (ldb (byte 8 shift) crc) stream))))

(defun png-filtered (pixels)
  "PIXELS' rows as PNG's image data before compression: each row its
filter's type byte, then its bytes under that filter."
  (let* ((data (pixels-data pixels)) (bpp (pixels-channels pixels))
         (stride (* (pixels-width pixels) bpp)) (height (pixels-height pixels))
         (raw (make-array (* height (1+ stride)) :element-type '(unsigned-byte 8))))
    (declare (type octets data raw) (type fixnum bpp stride height) (optimize speed))
    (dotimes (y height raw)
      (let ((row (* y stride)) (prior (and (plusp y) (* (1- y) stride))) (best 0) (least most-positive-fixnum))
        (declare (type fixnum row best least) (type (or null fixnum) prior))
        (macrolet ((predicted (filter i)
                     `(let ((a (if (>= ,i bpp) (aref data (+ row (- ,i bpp))) 0))
                            (b (if prior (aref data (+ prior ,i)) 0))
                            (c (if (and prior (>= ,i bpp)) (aref data (+ prior (- ,i bpp))) 0)))
                        (declare (type fixnum a b c) (ignorable a b c))
                        (ldb (byte 8 0) (- (aref data (+ row ,i))
                                           (ecase ,filter (0 0) (1 a) (2 b) (3 (ash (+ a b) -1)) (4 (paeth a b c))))))))
          (dotimes (filter 5)
            (let ((sum 0))
              (declare (type fixnum sum))
              (dotimes (i stride)
                (let ((v (predicted filter i))) (incf sum (if (< v 128) v (- 256 v)))))
              (when (< sum least) (setf least sum best filter))))
          (let ((out (+ (* y (1+ stride)) 1)))
            (declare (type fixnum out))
            (setf (aref raw (1- out)) best)
            (dotimes (i stride) (setf (aref raw (+ out i)) (predicted best i)))))))))

(defun pixels-png (pixels)
  "PIXELS as the bytes of a PNG file."
  (flexi-streams:with-output-to-sequence (out)
    (write-sequence (coerce #(137 80 78 71 13 10 26 10) 'octets) out)
    (let ((header (make-array 13 :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for (value at) in `((,(pixels-width pixels) 0) (,(pixels-height pixels) 4))
            do (loop for k below 4 do (setf (aref header (+ at k)) (ldb (byte 8 (* 8 (- 3 k))) value))))
      (setf (aref header 8) 8
            (aref header 9) (ecase (pixels-channels pixels) (1 0) (3 2) (4 6)))
      (png-chunk out "IHDR" header))
    (png-chunk out "IDAT" (salza2:compress-data (png-filtered pixels) 'salza2:zlib-compressor))
    (png-chunk out "IEND" (make-array 0 :element-type '(unsigned-byte 8)))))

;;; --- JPEG ----------------------------------------------------------------------
;;; Through cl-jpeg, which reads and writes baseline JPEG: what every camera
;;; and screenshot tool writes. Its samples run blue, green, red; a pixel here
;;; runs red first. A progressive or arithmetic-coded JPEG it refuses goes
;;; through a transcoder (IMAGE-PIXELS).

(defparameter +jpeg-luminance+
  #(16 11 10 16 24 40 51 61 12 12 14 19 26 58 60 55 14 13 16 24 40 57 69 56
    14 17 22 29 51 87 80 62 18 22 37 56 68 109 103 77 24 35 55 64 81 104 113 92
    49 64 78 87 103 121 120 101 72 92 95 98 112 100 103 99)
  "JPEG's Annex K luminance table, row by row: quality 50.")

(defparameter +jpeg-chrominance+
  #(17 18 24 47 99 99 99 99 18 21 26 66 99 99 99 99 24 26 56 99 99 99 99 99
    47 66 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99)
  "JPEG's Annex K chrominance table, row by row: quality 50.")

(defun jpeg-tables (quality &key gray)
  "The quantisation tables of QUALITY, 1 to 100, by libjpeg's scaling, in the
shape cl-jpeg takes them: a vector of tables, each a vector of eight rows --
luminance and chrominance, or with GRAY luminance alone."
  (let ((scale (if (< quality 50) (floor 5000 quality) (- 200 (* 2 quality)))))
    (flet ((table (base)
             (let ((rows (make-array 8 :element-type '(simple-array (unsigned-byte 8) (*)))))
               (dotimes (r 8 rows)
                 (setf (aref rows r) (make-array 8 :element-type '(unsigned-byte 8)))
                 (dotimes (c 8)
                   (setf (aref (aref rows r) c)
                         (max 1 (min 255 (floor (+ (* (aref base (+ (* 8 r) c)) scale) 50) 100)))))))))
      (if gray
          (vector (table +jpeg-luminance+))
          (vector (table +jpeg-luminance+) (table +jpeg-chrominance+))))))

(defun swap-red-blue (data channels)
  "Swap, in place, the first and third sample of every pixel of DATA."
  (declare (type octets data) (type fixnum channels) (optimize speed))
  (loop for at of-type fixnum from 0 below (- (length data) 2) by channels
        do (rotatef (aref data at) (aref data (+ at 2))))
  data)

(defun jpeg-pixels (octets)
  "The baseline JPEG OCTETS decoded to gray or RGB PIXELS, as stored: its
EXIF orientation is the caller's (JPEG-ORIENTATION)."
  (multiple-value-bind (buffer height width ncomp)
      (handler-case (flexi-streams:with-input-from-sequence (in octets) (jpeg:decode-stream in))
        (error (e) (error 'image-unfit :reason (format nil "the JPEG does not decode here: ~a" e))))
    (case ncomp
      (1 (%make-pixels width height 1 (coerce buffer 'octets)))
      (3 (%make-pixels width height 3 (swap-red-blue (coerce buffer 'octets) 3)))
      (t (error 'image-unfit :reason (format nil "a JPEG of ~d components is not one this reads" ncomp))))))

;;; Colour goes at full resolution, 4:4:4: cl-jpeg halves chroma by taking
;;; every other sample, which aliases colour detail, and a provider charges
;;; for pixels, not bytes -- a model reading coloured text or a chart's lines
;;; loses less for the third more a photo weighs.
(defun pixels-jpeg (pixels quality)
  "PIXELS as the bytes of a baseline JPEG of QUALITY, colour unsubsampled,
alpha laid over white since JPEG carries none."
  (let* ((channels (pixels-channels pixels))
         (gray (= channels 1))
         (buffer (if gray (pixels-data pixels) (swap-red-blue (pixels-data (opaque-pixels pixels)) 3))))
    (flexi-streams:with-output-to-sequence (out)
      (jpeg:encode-image-stream out buffer (if gray 1 3) (pixels-height pixels) (pixels-width pixels)
                                :q-tabs (jpeg-tables quality :gray gray)
                                :sampling (if gray '((1 1)) '((1 1) (1 1) (1 1)))))))

(defun opaque-pixels (pixels)
  "PIXELS with no alpha, a fresh copy in three channels: RGBA laid over
white, RGB copied."
  (let* ((source (pixels-data pixels)) (channels (pixels-channels pixels))
         (count (* (pixels-width pixels) (pixels-height pixels)))
         (out (blank-pixels (pixels-width pixels) (pixels-height pixels) 3))
         (data (pixels-data out)))
    (declare (type octets source data) (type fixnum channels count) (optimize speed))
    (ecase channels
      (3 (replace data source))
      (1 (dotimes (i count) (let ((v (aref source i))) (setf (aref data (* 3 i)) v (aref data (+ 1 (* 3 i))) v (aref data (+ 2 (* 3 i))) v))))
      (4 (dotimes (i count)
           (let ((a (aref source (+ 3 (* 4 i)))))
             (dotimes (k 3)
               (setf (aref data (+ k (* 3 i)))
                     (floor (+ (* (aref source (+ k (* 4 i))) a) (* 255 (- 255 a)) 127) 255)))))))
    out))

(defun jpeg-orientation (octets)
  "The EXIF orientation of the JPEG OCTETS, 1 to 8; 1 when it names none."
  (let ((pos 2) (end (length octets)))
    (loop while (< (+ pos 4) end)
          do (unless (= (aref octets pos) #xFF) (return 1))
             (let ((marker (aref octets (1+ pos))) (length (big-endian octets (+ pos 2) 2)))
               (when (member marker '(#xDA #xD9)) (return 1))
               (when (and (= marker #xE1) (< (+ pos 10) end)
                          (equalp (subseq octets (+ pos 4) (+ pos 10)) #(69 120 105 102 0 0)))
                 (return
                   (let* ((tiff (+ pos 10))
                          (little (and (< (1+ tiff) end) (= (aref octets tiff) #x49))))
                     (flet ((u (at count)
                              (if (> (+ at count) end)
                                  0
                                  (if little
                                      (loop for i below count sum (ash (aref octets (+ at i)) (* 8 i)))
                                      (big-endian octets at count)))))
                       (let* ((ifd (+ tiff (u (+ tiff 4) 4))) (entries (u ifd 2)))
                         (or (loop for e below entries
                                   for at = (+ ifd 2 (* 12 e))
                                   when (= (u at 2) #x0112)
                                     return (let ((value (u (+ at 8) 2))) (if (<= 1 value 8) value 1)))
                             1))))))
               (incf pos (+ 2 length)))
          finally (return 1))))

(defun orient-pixels (pixels orientation)
  "PIXELS turned upright by EXIF ORIENTATION: 2 to 8 mirror and turn them,
5 to 8 swapping width and height; 1 is PIXELS itself."
  (if (eql orientation 1)
      pixels
      (let* ((w (pixels-width pixels)) (h (pixels-height pixels)) (ch (pixels-channels pixels))
             (swap (>= orientation 5))
             (out (blank-pixels (if swap h w) (if swap w h) ch))
             (ow (pixels-width out)) (oh (pixels-height out))
             (from (pixels-data pixels)) (to (pixels-data out)))
        (declare (type fixnum w h ch ow oh) (type octets from to))
        (dotimes (y oh out)
          (dotimes (x ow)
            (multiple-value-bind (sx sy)
                (ecase orientation
                  (2 (values (- w 1 x) y)) (3 (values (- w 1 x) (- h 1 y))) (4 (values x (- h 1 y)))
                  (5 (values y x)) (6 (values y (- h 1 x))) (7 (values (- w 1 y) (- h 1 x)))
                  (8 (values (- w 1 y) x)))
              (replace to from :start1 (* (+ (* y ow) x) ch) :start2 (* (+ (* sy w) sx) ch)
                               :end2 (+ (* (+ (* sy w) sx) ch) ch))))))))

;;; --- shrinking -----------------------------------------------------------------
;;; By area: each new pixel is the mean of the old ones it covers, a part of
;;; a pixel by the part it covers -- the filter that cannot alias a shrink.
;;; Alpha is premultiplied, so a transparent pixel's colour never bleeds.

(defun area-weights (from to)
  "How FROM samples fold into TO, TO no more than FROM: (values STARTS COUNTS
WEIGHTS), new sample I the sum over the COUNTS[I] old ones from STARTS[I]
of each times its next weight, the weights of each summing to 1."
  (let* ((scale (/ from to)) (starts (make-array to :element-type 'fixnum))
         (counts (make-array to :element-type 'fixnum)) (weights '()))
    (dotimes (i to)
      (let* ((lo (* i scale)) (hi (* (1+ i) scale)) (first (floor lo)) (last (1- (ceiling hi))))
        (setf (aref starts i) first (aref counts i) (1+ (- last first)))
        (loop for j from first to last
              do (push (coerce (/ (- (min hi (1+ j)) (max lo j)) scale) 'single-float) weights))))
    (values starts counts (coerce (nreverse weights) '(simple-array single-float (*))))))

(defun shrink-pixels (pixels width height)
  "PIXELS resampled by area to WIDTH x HEIGHT, neither larger than PIXELS'."
  (let ((w (pixels-width pixels)) (h (pixels-height pixels)))
    (if (and (= w width) (= h height))
        pixels
        (multiple-value-bind (xs xn xw) (area-weights w width)
          (multiple-value-bind (ys yn yw) (area-weights h height)
            (shrink-by pixels width height xs xn xw ys yn yw))))))

(defun shrink-by (pixels width height xs xn xw ys yn yw)
  "SHRINK-PIXELS' work, given AREA-WEIGHTS across and down."
  (declare (type (simple-array fixnum (*)) xs xn ys yn)
           (type (simple-array single-float (*)) xw yw) (type fixnum width height)
           (optimize speed))
  (let* ((w (pixels-width pixels)) (ch (pixels-channels pixels)) (alpha (= ch 4))
         (from (pixels-data pixels))
         (out (blank-pixels width height ch)) (to (pixels-data out))
         (row (make-array (* w ch) :element-type 'single-float))
         (y-weight 0))
    (declare (type fixnum w ch y-weight) (type octets from to) (type (simple-array single-float (*)) row))
    (dotimes (oy height out)
      (fill row 0.0)
      ;; Down: the old rows this new row covers, summed by their weights.
      (loop for sy of-type fixnum from (aref ys oy) repeat (aref yn oy)
            for wy of-type single-float = (aref yw y-weight)
            do (incf y-weight)
               (let ((base (* sy w ch)))
                 (declare (type fixnum base))
                 (if alpha
                     (loop for x of-type fixnum below w
                           for at of-type fixnum from base by 4
                           for r of-type fixnum from 0 by 4
                           do (let ((a (* wy (aref from (+ at 3)))))
                                (declare (type single-float a))
                                (incf (aref row r) (* a (aref from at)))
                                (incf (aref row (+ r 1)) (* a (aref from (+ at 1))))
                                (incf (aref row (+ r 2)) (* a (aref from (+ at 2))))
                                (incf (aref row (+ r 3)) a)))
                     (loop for i of-type fixnum below (* w ch)
                           do (incf (aref row i) (* wy (aref from (+ base i))))))))
      ;; Across: the summed row folded into the new one.
      (let ((x-weight 0))
        (declare (type fixnum x-weight))
        (dotimes (ox width)
          (let ((sums (make-array 4 :element-type 'single-float :initial-element 0.0)))
            (declare (dynamic-extent sums))
            (loop for sx of-type fixnum from (aref xs ox) repeat (aref xn ox)
                  for wx of-type single-float = (aref xw x-weight)
                  do (incf x-weight)
                     (dotimes (k ch) (incf (aref sums k) (* wx (aref row (+ (* sx ch) k))))))
            (let ((at (* (+ (* oy width) ox) ch)))
              (declare (type fixnum at))
              (flet ((byte-of (v) (declare (type single-float v)) (the fixnum (max 0 (min 255 (round v))))))
                (if alpha
                    (let ((a (aref sums 3)))
                      (dotimes (k 3) (setf (aref to (+ at k)) (if (> a 0.0) (byte-of (/ (aref sums k) a)) 0)))
                      (setf (aref to (+ at 3)) (byte-of a)))
                    (dotimes (k ch) (setf (aref to (+ at k)) (byte-of (aref sums k)))))))))))))

(defun opaque-p (pixels)
  "Whether PIXELS carry no alpha, or alpha that hides nothing."
  (or (/= (pixels-channels pixels) 4)
      (let ((data (pixels-data pixels)))
        (declare (type octets data) (optimize speed))
        (loop for at of-type fixnum from 3 below (length data) by 4 always (= (aref data at) 255)))))

;;; --- decoding, whatever the format ---------------------------------------------

(defparameter *image-decoders* '(("image/png" . png-pixels) ("image/jpeg" . jpeg-pixels))
  "Media type -> the function that decodes such bytes to PIXELS here, as they
are stored, signalling IMAGE-UNFIT on bytes it cannot read. A type with no
entry, or bytes its entry refuses, goes through a transcoder.")

(defun transcoded-pixels (octets)
  "OCTETS opened by the first of nlt:*PNG-TRANSCODERS* this box has, as
PIXELS; NIL when none of them makes a PNG of them."
  (let* ((stem (format nil "nodecode/fit/~a" (subseq (nlk::sha256-text octets) 7)))
         (source (nlk:cache-path (format nil "~a.source" stem)))
         (target (nlk:cache-path (format nil "~a.png" stem))))
    (nlk:with-cleanup ((ignore-errors (delete-file source)) (ignore-errors (delete-file target)))
      (ensure-directories-exist source)
      (alexandria:write-byte-vector-into-file octets source :if-exists :supersede)
      (when (nlt:transcode-to-png source target)
        (png-pixels (alexandria:read-file-into-byte-vector target))))))

(defun image-pixels (octets media-type)
  "OCTETS, an image of MEDIA-TYPE, decoded to PIXELS as stored: PNG and
baseline JPEG here, anything else through a transcoder. Signals
IMAGE-UNFIT, saying why, when nothing opens it."
  (multiple-value-bind (width height) (nlt:image-dimensions octets)
    (when (and width height (> (* width height) *image-max-pixels*))
      (error 'image-unfit
             :reason (format nil "it is ~:dx~:d, over the ~:d pixels one image is opened at"
                             width height *image-max-pixels*))))
  (let ((own (handler-case (when-let (decoder (cdr (assoc media-type *image-decoders* :test #'equal)))
                             (funcall decoder octets))
               (image-unfit (e) e))))
    (cond ((pixels-p own) own)
          ((with-handlers ((error () nil)) (transcoded-pixels octets)))
          (t (error 'image-unfit
                    :reason (format nil "~a; no ImageMagick, ffmpeg or sips here opens it either"
                                    (if own (image-unfit-reason own) (format nil "this reads no ~a" media-type))))))))

;;; --- fitting ---------------------------------------------------------------------

(defun fit-size (width height side)
  "WIDTH x HEIGHT scaled down to no more than SIDE on either, the shape kept:
(values WIDTH HEIGHT)."
  (let ((scale (min 1 (/ side (max width height)))))
    (values (max 1 (round (* width scale))) (max 1 (round (* height scale))))))

(defun fit-image (octets &key (max-side *image-max-side*) (max-bytes *image-send-max-bytes*))
  "OCTETS, an image's bytes, as a provider takes them: OCTETS themselves when
the image is no wider or taller than MAX-SIDE and no larger than MAX-BYTES;
else decoded, turned upright by its EXIF orientation, shrunk to MAX-SIDE and
re-encoded as the smaller of PNG and JPEG, then as JPEG at lower qualities,
then a quarter smaller a side, until it fits. An image no codec here opens
goes as itself when it is within what a provider takes whole. => (values
OCTETS MEDIA-TYPE WIDTH HEIGHT); signals IMAGE-UNFIT when nothing fits."
  (let ((media-type (or (image-media-type octets)
                        (error 'image-unfit :reason "its bytes open no PNG, JPEG, GIF or WebP"))))
    (multiple-value-bind (width height) (nlt:image-dimensions octets)
      (when (and width height (<= (max width height) max-side) (<= (length octets) max-bytes))
        (return-from fit-image (values octets media-type width height)))
      (let ((pixels (handler-case (image-pixels octets media-type)
                      (image-unfit (e)
                        (if (and width height (<= (max width height) *image-whole-max-side*)
                                 (<= (length octets) *image-whole-max-bytes*))
                            (return-from fit-image (values octets media-type width height))
                            (error e))))))
        (multiple-value-bind (w h) (fit-size (pixels-width pixels) (pixels-height pixels) max-side)
          (let* ((base (orient-pixels (shrink-pixels pixels w h)
                                      (if (equal media-type "image/jpeg") (jpeg-orientation octets) 1)))
                 (base (if (and (= (pixels-channels base) 4) (opaque-p base)) (opaque-pixels base) base)))
            (loop for p = base then (shrink-pixels base (max 1 (floor (* 3 (pixels-width p)) 4))
                                                   (max 1 (floor (* 3 (pixels-height p)) 4)))
                  do (let* ((png (pixels-png p))
                            (jpeg (pixels-jpeg p (first *image-jpeg-qualities*)))
                            (best (if (< (length jpeg) (length png)) jpeg png)))
                       (flet ((done (bytes)
                                (return-from fit-image
                                  (values bytes (if (eq bytes png) "image/png" "image/jpeg")
                                          (pixels-width p) (pixels-height p)))))
                         (when (<= (length best) max-bytes) (done best))
                         (dolist (quality (rest *image-jpeg-qualities*))
                           (let ((lower (pixels-jpeg p quality)))
                             (when (<= (length lower) max-bytes) (done lower))))))
                  until (and (= (pixels-width p) 1) (= (pixels-height p) 1)))
            (error 'image-unfit :reason (format nil "not even one pixel of it comes under ~:d bytes" max-bytes))))))))
