;;;; jpeg.lisp --- JPEG read here: baseline and progressive, pixel for pixel as libjpeg reads it.
;;;;
;;;; SPDX-License-Identifier: MIT AND IJG
;;;;
;;;; FIT-IMAGE (image-fit.lisp) shrinks a picture once it has its pixels,
;;;; and cl-jpeg reads baseline JPEG only: a progressive one -- what a
;;;; browser saves a picture off the web as, and what many phones, photo
;;;; tools and image servers write -- needed ImageMagick or ffmpeg on the
;;;; box. JPEG-DECODE reads every JPEG in use: Huffman-coded baseline,
;;;; extended and progressive (spectral selection and successive
;;;; approximation), restart intervals, any sampling factors, gray, YCbCr,
;;;; RGB and Adobe's CMYK and YCCK. It refuses the forms nothing common
;;;; writes -- arithmetic coding, lossless, hierarchical, 12-bit samples.
;;;; A sequential file cut short decodes as far as it goes, the rest gray,
;;;; as libjpeg leaves it; a progressive one shows the scans it has, without
;;;; the block smoothing libjpeg lays over coefficients still missing.
;;;;
;;;; Every step is libjpeg's (https://ijg.org) as libjpeg-turbo
;;;; (https://libjpeg-turbo.org) runs it under PIL and the browsers, so a
;;;; picture decodes to the pixels everyone else sees: its Huffman decoding
;;;; and restart handling (jdhuff.c, jdphuff.c, jdmarker.c), the "islow"
;;;; integer inverse DCT (jidctint.c), "fancy" triangle-filter chroma
;;;; upsampling and its choice of method (jdsample.c), YCbCr to RGB through
;;;; its fixed-point tables (jdcolor.c), and the standard Huffman tables for
;;;; a Motion-JPEG frame that carries none (jstdhuff.c). CMYK becomes RGB as
;;;; PIL makes it. This software is based in part on the work of the
;;;; Independent JPEG Group.

(in-package #:nodecode.evolved)

;;; --- refusals and tables -------------------------------------------------------

(define-error jpeg-malformed (error) (reason)
  (:report "~a" reason)
  (:documentation
   "Bytes JPEG-DECODE cannot read as a JPEG it decodes, REASON saying why."))

(defun %jpeg-fail (control &rest arguments)
  (error 'jpeg-malformed :reason (apply #'format nil control arguments)))

(declaim (type (simple-array (unsigned-byte 8) (80)) *jpeg-natural-order*))
(sb-ext:defglobal *jpeg-natural-order*
    (make-array 80 :element-type '(unsigned-byte 8)
                   :initial-contents '(0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5
                                       12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
                                       35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51
                                       58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63
                                       63 63 63 63 63 63 63 63 63 63 63 63 63 63 63 63))
  "Zigzag index -> row-major index in an 8 x 8 block, then sixteen 63s:
libjpeg's jpeg_natural_order, whose tail keeps a corrupt run inside the block.")

;;; jdcolor.c's tables, libjpeg's FIX(c) being c x 2^16 rounded: 91881 is
;;; FIX(1.40200), 116130 FIX(1.77200), 46802 FIX(0.71414), 22554 FIX(0.34414).
(macrolet ((define-ycc-table (name form documentation)
             `(progn
                (declaim (type (simple-array (signed-byte 32) (256)) ,name))
                (sb-ext:defglobal ,name
                    (let ((table (make-array 256 :element-type '(signed-byte 32))))
                      (dotimes (i 256 table)
                        (let ((x (- i 128)))
                          (setf (aref table i) ,form))))
                  ,documentation))))
  (define-ycc-table *jpeg-cr-red* (ash (+ (* 91881 x) 32768) -16)
    "Cr -> what it adds to red: 1.402 (Cr - 128), rounded (Cr_r_tab).")
  (define-ycc-table *jpeg-cb-blue* (ash (+ (* 116130 x) 32768) -16)
    "Cb -> what it adds to blue: 1.772 (Cb - 128), rounded (Cb_b_tab).")
  (define-ycc-table *jpeg-cr-green* (* -46802 x)
    "Cr -> what it adds to green, -0.71414 (Cr - 128) in 16.16 fixed point (Cr_g_tab).")
  (define-ycc-table *jpeg-cb-green* (+ (* -22554 x) 32768)
    "Cb -> what it adds to green, -0.34414 (Cb - 128) in 16.16 fixed point,
plus the half that rounds the sum (Cb_g_tab)."))

(defparameter *jpeg-standard-huffman*
  '((:DC 0 (0 1 5 1 1 1 1 1 1 0 0 0 0 0 0 0) (0 1 2 3 4 5 6 7 8 9 10 11))
    (:DC 1 (0 3 1 1 1 1 1 1 1 1 1 0 0 0 0 0) (0 1 2 3 4 5 6 7 8 9 10 11))
    (:AC 0 (0 2 1 3 3 2 4 3 5 5 4 4 0 0 1 125) (1 2 3 0 4 17 5 18 33 49 65 6 19 81 97 7 34 113 20 50 129 145 161 8 35 66 177 193 21 82 209 240 36 51 98 114 130 9 10 22 23 24 25 26 37 38 39 40 41 42 52 53 54 55 56 57 58 67 68 69 70 71 72 73 74 83 84 85 86 87 88 89 90 99 100 101 102 103 104 105 106 115 116 117 118 119 120 121 122 131 132 133 134 135 136 137 138 146 147 148 149 150 151 152 153 154 162 163 164 165 166 167 168 169 170 178 179 180 181 182 183 184 185 186 194 195 196 197 198 199 200 201 202 210 211 212 213 214 215 216 217 218 225 226 227 228 229 230 231 232 233 234 241 242 243 244 245 246 247 248 249 250))
    (:AC 1 (0 2 1 2 4 4 3 4 7 5 4 4 0 1 2 119) (0 1 2 3 17 4 5 33 49 6 18 65 81 7 97 113 19 34 50 129 8 20 66 145 161 177 193 9 35 51 82 240 21 98 114 209 10 22 36 52 225 37 241 23 24 25 26 38 39 40 41 42 53 54 55 56 57 58 67 68 69 70 71 72 73 74 83 84 85 86 87 88 89 90 99 100 101 102 103 104 105 106 115 116 117 118 119 120 121 122 130 131 132 133 134 135 136 137 138 146 147 148 149 150 151 152 153 154 162 163 164 165 166 167 168 169 170 178 179 180 181 182 183 184 185 186 194 195 196 197 198 199 200 201 202 210 211 212 213 214 215 216 217 218 226 227 228 229 230 231 232 233 234 242 243 244 245 246 247 248 249 250)))
  "JPEG's own Huffman tables (ITU T.81 Annex K.3), each (CLASS SLOT COUNTS
SYMBOLS), COUNTS by code length 1 to 16: what libjpeg-turbo decodes a scan
with when the file defines no table for slot 0 or 1 (jstdhuff.c), as a
Motion-JPEG frame -- a webcam's, an IP camera's -- does not.")

(declaim (inline %jpeg-u16 %jpeg-extend %jpeg-wrap16 %jpeg-wrap32 %jpeg-clamp))
(defun %jpeg-u16 (data at)
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum at))
  (logior (ash (aref data at) 8) (aref data (1+ at))))

(defun %jpeg-extend (bits size)
  "The SIZE-bit magnitude BITS as the signed value it codes (HUFF_EXTEND)."
  (declare (type (unsigned-byte 16) bits) (type (integer 1 16) size))
  (if (< bits (ash 1 (1- size))) (+ bits 1 (- (ash 1 size))) bits))

(defun %jpeg-wrap16 (x)
  "X as C stores it in a short: its low 16 bits, signed."
  (declare (type integer x))
  (- (logand (+ x #x8000) #xFFFF) #x8000))

(defun %jpeg-wrap32 (x)
  "X as C stores it in an int: its low 32 bits, signed."
  (declare (type integer x))
  (- (logand (+ x #x80000000) #xFFFFFFFF) #x80000000))

(defun %jpeg-clamp (x)
  (declare (type fixnum x))
  (if (< x 0) 0 (if (> x 255) 255 x)))

;;; --- Huffman tables --------------------------------------------------------------

(defstruct (jpeg-huffman (:constructor %make-jpeg-huffman ()))
  "A Huffman table made for decoding, libjpeg's d_derived_tbl: LOOKUP maps
the next 9 bits to (length << 8) | symbol for a code of up to 9 bits, 0 for
a longer one, which MAXCODE and OFFSET by code length then find."
  (lookup (make-array 512 :element-type '(unsigned-byte 16) :initial-element 0)
   :type (simple-array (unsigned-byte 16) (512)))
  (maxcode (make-array 18 :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (18)))
  (offset (make-array 18 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (18)))
  (symbols (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (256))))

(defun %jpeg-huffman (counts symbols dc)
  "The decoding table of COUNTS, a vector of 17 whose element L counts the
codes L bits long, and SYMBOLS in code order, as a DHT segment lists them. DC
says it codes DC differences, whose symbols stop at 15. Refuses counts that
make no prefix code, or one with an all-ones code, as libjpeg does."
  (let ((table (%make-jpeg-huffman)) (code 0) (p 0))
    (declare (type fixnum code p))
    (when (> (length symbols) 256)
      (%jpeg-fail "a Huffman table lists ~d symbols, past 256" (length symbols)))
    (replace (jpeg-huffman-symbols table) symbols)
    (when (and dc (some (lambda (s) (> s 15)) symbols))
      (%jpeg-fail "a DC Huffman table holds a symbol past 15"))
    (loop for length from 1 to 16
          for n = (aref counts length)
          do (setf (aref (jpeg-huffman-offset table) length) (- p code))
             (dotimes (i n)
               (when (>= code (1- (ash 1 length)))
                 (%jpeg-fail "a Huffman table's code lengths make no prefix code"))
               (when (<= length 9)
                 (let ((start (ash code (- 9 length))))
                   (fill (jpeg-huffman-lookup table) (logior (ash length 8) (aref symbols p))
                         :start start :end (+ start (ash 1 (- 9 length))))))
               (incf code)
               (incf p))
             (setf (aref (jpeg-huffman-maxcode table) length) (if (plusp n) (1- code) -1))
             (setf code (ash code 1)))
    (setf (aref (jpeg-huffman-maxcode table) 17) #xFFFFF)
    table))

(defun %jpeg-standard-table (class slot)
  "The standard table libjpeg-turbo takes for CLASS (:dc or :ac) in SLOT when
the file defines none there."
  (let ((entry (find-if (lambda (e) (and (eq (first e) class) (= (second e) slot)))
                        *jpeg-standard-huffman*)))
    (unless entry
      (%jpeg-fail "a scan reads Huffman table ~d, which the file does not define" slot))
    (%jpeg-huffman (coerce (cons 0 (third entry)) 'simple-vector)
                   (coerce (fourth entry) '(simple-array (unsigned-byte 8) (*)))
                   (eq class :dc))))

;;; --- the entropy-coded bits ------------------------------------------------------

(defstruct (jpeg-bits (:constructor %make-jpeg-bits (data end)) (:conc-name jb-))
  "A scan's entropy-coded data read bit by bit from DATA at POS: BUF holds
COUNT bits not yet read, the next highest. MARKER is the marker the data
ran into, 0 while none; SHORT is set once a read wanted bits past it --
libjpeg's insufficient_data -- and the scan's coded units then go
undecoded."
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (buf 0 :type (unsigned-byte 56))
  (count 0 :type (integer 0 63))
  (marker 0 :type (unsigned-byte 8))
  (short nil :type boolean))

(defun %jb-fill (b)
  "Feed B's buffer from its bytes until it holds over 48 bits or the data
ends at a marker, kept in MARKER: FF 00 is a data FF, FF fill bytes are
skipped, and past the last byte stands an EOI, as libjpeg's memory source
fakes one."
  (declare (type jpeg-bits b) (optimize speed (safety 0)))
  (let ((data (jb-data b)) (pos (jb-pos b)) (end (jb-end b))
        (buf (jb-buf b)) (count (jb-count b)))
    (declare (type fixnum pos end) (type (unsigned-byte 56) buf) (type (integer 0 63) count))
    (loop while (and (<= count 48) (zerop (jb-marker b)))
          do (let ((byte (if (< pos end) (aref data pos) -1)))
               (declare (type (integer -1 255) byte))
               (cond ((< byte 0) (setf (jb-marker b) #xD9))
                     ((/= byte #xFF)
                      (incf pos)
                      (setf buf (logand (logior (ash buf 8) byte) #xFFFFFFFFFFFFFF)
                            count (+ count 8)))
                     (t
                      (incf pos)
                      (loop while (and (< pos end) (= (aref data pos) #xFF)) do (incf pos))
                      (cond ((>= pos end) (setf (jb-marker b) #xD9))
                            ((zerop (aref data pos))
                             (incf pos)
                             (setf buf (logand (logior (ash buf 8) #xFF) #xFFFFFFFFFFFFFF)
                                   count (+ count 8)))
                            (t (setf (jb-marker b) (aref data pos))
                               (incf pos)))))))
    (setf (jb-pos b) pos (jb-buf b) buf (jb-count b) count)
    b))

(declaim (inline %jb-bits %jb-decode))
(defun %jb-bits (b n)
  "The next N bits of B, 0 to 16 of them, as an unsigned integer: zeros
past the end of its data, which marks it short."
  (declare (type jpeg-bits b) (type (integer 0 16) n) (optimize speed (safety 0)))
  (when (< (jb-count b) n)
    (%jb-fill b)
    (when (< (jb-count b) n)
      (setf (jb-short b) t
            (jb-buf b) (ash (ldb (byte (jb-count b) 0) (jb-buf b)) (- 32 (jb-count b)))
            (jb-count b) 32)))
  (let ((count (- (jb-count b) n)))
    (setf (jb-count b) count)
    (ldb (byte n count) (jb-buf b))))

(defun %jb-decode-slow (b table)
  "The next symbol of B by TABLE, a bit at a time (jpeg_huff_decode): a
code longer than 9 bits, or the last bits of the data. A code no table
holds reads as 0, as libjpeg reads it."
  (declare (type jpeg-bits b) (type jpeg-huffman table) (optimize speed (safety 1)))
  (let ((code (%jb-bits b 1)) (length 1) (maxcode (jpeg-huffman-maxcode table)))
    (declare (type fixnum code) (type (integer 1 17) length))
    (loop while (> code (aref maxcode length))
          do (setf code (logior (ash code 1) (%jb-bits b 1)))
             (incf length))
    (if (> length 16)
        0
        (aref (jpeg-huffman-symbols table)
              (logand (+ code (aref (jpeg-huffman-offset table) length)) #xFF)))))

(defun %jb-decode (b table)
  "The next symbol of B by TABLE."
  (declare (type jpeg-bits b) (type jpeg-huffman table) (optimize speed (safety 0)))
  (when (< (jb-count b) 16) (%jb-fill b))
  (let ((count (jb-count b)))
    (if (< count 9)
        (%jb-decode-slow b table)
        (let ((entry (aref (jpeg-huffman-lookup table) (ldb (byte 9 (- count 9)) (jb-buf b)))))
          (if (zerop entry)
              (%jb-decode-slow b table)
              (progn (setf (jb-count b) (- count (ash entry -8)))
                     (logand entry #xFF)))))))

(defun %jpeg-next-marker (data pos end)
  "libjpeg's next_marker from POS: past any bytes to an FF, past FF fill to
the marker's code, an FF 00 skipped as data. => (values CODE POSITION after
it), or NIL at the end of DATA."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum pos end))
  (loop
    (loop while (and (< pos end) (/= (aref data pos) #xFF)) do (incf pos))
    (loop while (and (< pos end) (= (aref data pos) #xFF)) do (incf pos))
    (when (>= pos end) (return (values nil end)))
    (let ((code (aref data pos)))
      (incf pos)
      (unless (zerop code) (return (values code pos))))))

(defun %jpeg-restart (b expected)
  "At a restart boundary: B's bits left behind, the marker found and RST
EXPECTED swallowed, or the stream resynchronised as
jpeg_resync_to_restart does when the markers are out of step."
  (declare (type jpeg-bits b) (type (integer 0 7) expected))
  (setf (jb-buf b) 0 (jb-count b) 0)
  (flet ((advance ()
           (multiple-value-bind (marker after) (%jpeg-next-marker (jb-data b) (jb-pos b) (jb-end b))
             (setf (jb-marker b) (or marker #xD9) (jb-pos b) after))))
    (when (zerop (jb-marker b)) (advance))
    (if (= (jb-marker b) (+ #xD0 expected))
        (setf (jb-marker b) 0)
        (flet ((rst (offset) (+ #xD0 (logand (+ expected offset) 7))))
          (loop
            (let ((marker (jb-marker b)))
              (cond ((< marker #xC0) (advance))
                    ((or (< marker #xD0) (> marker #xD7)) (return))
                    ((or (= marker (rst 1)) (= marker (rst 2))) (return))
                    ((or (= marker (rst -1)) (= marker (rst -2))) (advance))
                    (t (setf (jb-marker b) 0) (return))))))))
  (when (zerop (jb-marker b)) (setf (jb-short b) nil)))

;;; --- the frame ---------------------------------------------------------------------

(defstruct (jpeg-component (:conc-name jc-) (:constructor %make-jpeg-component (id h v tq)))
  "One component of a frame: its id, sampling factors H x V, quantization
slot TQ and the table latched from it at the component's first scan; DW x
DH samples in WIB x HIB blocks, stored BPL x BL blocks to whole coded
units, in PLANE as samples, or in COEFS while a progressive frame gathers
its coefficients; PRED the DC prediction; DC and AC its scan's Huffman
tables."
  (id 0 :type (unsigned-byte 8))
  (h 1 :type (integer 1 4))
  (v 1 :type (integer 1 4))
  (tq 0 :type (integer 0 3))
  (quant nil :type (or null (simple-array (unsigned-byte 16) (64))))
  (dw 0 :type fixnum) (dh 0 :type fixnum)
  (wib 0 :type fixnum) (hib 0 :type fixnum)
  (bpl 0 :type fixnum) (bl 0 :type fixnum)
  (plane nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (coefs nil :type (or null (simple-array (signed-byte 16) (*))))
  (pred 0 :type (signed-byte 32))
  (dc nil :type (or null jpeg-huffman))
  (ac nil :type (or null jpeg-huffman)))

(defstruct (jpeg-frame (:conc-name jf-) (:constructor %make-jpeg-frame))
  "A JPEG's frame: WIDTH x HEIGHT pixels of COMPONENTS, sampled at most
HMAX x VMAX, in MCUX x MCUY coded units when its scans interleave."
  (width 0 :type fixnum) (height 0 :type fixnum)
  (progressive nil :type boolean)
  (components #() :type simple-vector)
  (hmax 1 :type (integer 1 4)) (vmax 1 :type (integer 1 4))
  (mcux 0 :type fixnum) (mcuy 0 :type fixnum))

(defun %jpeg-frame (data start length progressive)
  "The frame a SOF segment of LENGTH bytes at START in DATA describes, its
planes or coefficient stores made."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start length))
  (when (< length 6) (%jpeg-fail "its frame header is cut short"))
  (let ((precision (aref data start))
        (height (%jpeg-u16 data (+ start 1)))
        (width (%jpeg-u16 data (+ start 3)))
        (count (aref data (+ start 5))))
    (unless (= precision 8) (%jpeg-fail "its samples are ~d bits; this reads 8" precision))
    (when (zerop height) (%jpeg-fail "its height is left to a DNL marker, which this does not read"))
    (when (zerop width) (%jpeg-fail "it is 0 pixels wide"))
    (unless (member count '(1 3 4)) (%jpeg-fail "it has ~d components; this reads 1, 3 or 4" count))
    (unless (= length (+ 6 (* 3 count)))
      (%jpeg-fail "its frame header is ~d bytes, not the ~d of ~d components" length (+ 6 (* 3 count)) count))
    (when (> (* width height) *image-max-pixels*)
      (%jpeg-fail "it is ~:dx~:d, past the ~:d pixels one image is opened at" width height *image-max-pixels*))
    (let ((components (make-array count :initial-element nil)))
      (dotimes (i count)
        (let* ((at (+ start 6 (* 3 i))) (id (aref data at)) (hv (aref data (+ at 1)))
               (h (ash hv -4)) (v (logand hv 15)) (tq (aref data (+ at 2))))
          (unless (and (<= 1 h 4) (<= 1 v 4))
            (%jpeg-fail "component ~d is sampled ~dx~d; the format allows 1 to 4" id h v))
          (unless (<= tq 3) (%jpeg-fail "component ~d names quantization table ~d" id tq))
          (when (find id components :key (lambda (c) (and c (jc-id c))))
            (%jpeg-fail "two components share the id ~d" id))
          (setf (svref components i) (%make-jpeg-component id h v tq))))
      (let ((hmax (reduce #'max components :key #'jc-h))
            (vmax (reduce #'max components :key #'jc-v)))
        (loop for c across components
              do (let ((h (jc-h c)) (v (jc-v c)))
                   (unless (and (zerop (mod hmax h)) (zerop (mod vmax v)))
                     (%jpeg-fail "component ~d is sampled ~dx~d against ~dx~d, a fraction this does not upsample"
                                 (jc-id c) h v hmax vmax))
                   (let* ((dw (ceiling (* width h) hmax)) (dh (ceiling (* height v) vmax))
                          (wib (ceiling dw 8)) (hib (ceiling dh 8))
                          (bpl (* h (ceiling wib h))) (bl (* v (ceiling hib v))))
                     (setf (jc-dw c) dw (jc-dh c) dh (jc-wib c) wib (jc-hib c) hib
                           (jc-bpl c) bpl (jc-bl c) bl)
                     (if progressive
                         (setf (jc-coefs c) (make-array (* bpl bl 64) :element-type '(signed-byte 16)
                                                                      :initial-element 0))
                         (setf (jc-plane c) (make-array (* bpl bl 64) :element-type '(unsigned-byte 8)
                                                                      :initial-element 128))))))
        (%make-jpeg-frame :width width :height height :progressive progressive
                          :components components :hmax hmax :vmax vmax
                          :mcux (ceiling width (* 8 hmax)) :mcuy (ceiling height (* 8 vmax)))))))

(defun %jpeg-dqt (data start stop quant)
  "A DQT segment's tables, DATA from START to STOP, into the four slots of
QUANT, each in row-major order."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start stop))
  (let ((p start) (natural *jpeg-natural-order*))
    (loop while (< p stop)
          do (let* ((precision (ash (aref data p) -4)) (slot (logand (aref data p) 15))
                    (size (if (zerop precision) 64 128)))
               (unless (and (<= precision 1) (<= slot 3))
                 (%jpeg-fail "a quantization table names precision ~d, slot ~d" precision slot))
               (when (> (+ p 1 size) stop) (%jpeg-fail "a quantization table is cut short"))
               (let ((table (make-array 64 :element-type '(unsigned-byte 16))))
                 (dotimes (i 64)
                   (setf (aref table (aref natural i))
                         (if (zerop precision)
                             (aref data (+ p 1 i))
                             (%jpeg-u16 data (+ p 1 (* 2 i))))))
                 (setf (svref quant slot) table))
               (incf p (1+ size))))))

(defun %jpeg-dht (data start stop dc-tables ac-tables)
  "A DHT segment's tables, DATA from START to STOP, into the slots of
DC-TABLES and AC-TABLES."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start stop))
  (let ((p start))
    (loop while (< p stop)
          do (when (> (+ p 17) stop) (%jpeg-fail "a Huffman table is cut short"))
             (let ((class (ash (aref data p) -4)) (slot (logand (aref data p) 15))
                   (counts (make-array 17 :initial-element 0)) (total 0))
               (loop for l from 1 to 16
                     do (setf (svref counts l) (aref data (+ p l)))
                        (incf total (svref counts l)))
               (incf p 17)
               (when (or (> total 256) (> (+ p total) stop))
                 (%jpeg-fail "a Huffman table lists more symbols than its segment holds"))
               (unless (and (<= class 1) (<= slot 3))
                 (%jpeg-fail "a Huffman table names class ~d, slot ~d" class slot))
               (setf (svref (if (zerop class) dc-tables ac-tables) slot)
                     (%jpeg-huffman counts (subseq data p (+ p total)) (zerop class)))
               (incf p total)))))

(defun %jpeg-scan-header (data start length frame quant dc-tables ac-tables)
  "The scan a SOS header of LENGTH bytes at START in DATA opens: => (values
COMPONENTS SS SE AH AL), COMPONENTS the frame's in scan order with the
tables the scan reads set and each one's quantization table latched."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start length))
  (when (< length 1) (%jpeg-fail "a scan header is cut short"))
  (let ((count (aref data start)))
    (unless (and (<= 1 count 4) (= length (+ 4 (* 2 count))))
      (%jpeg-fail "a scan header names ~d components in ~d bytes" count length))
    (let* ((progressive (jf-progressive frame))
           (at (+ start 1 (* 2 count)))
           (ss (aref data at)) (se (aref data (+ at 1)))
           (ah (ash (aref data (+ at 2)) -4)) (al (logand (aref data (+ at 2)) 15))
           (components (make-array count :initial-element nil)))
      (when progressive
        (when (or (if (zerop ss) (/= se 0) (or (> ss se) (> se 63) (/= count 1)))
                  (and (/= ah 0) (/= al (1- ah)))
                  (> al 13))
          (%jpeg-fail "a progressive scan's parameters ~d ~d ~d ~d break the format's rules" ss se ah al)))
      (dotimes (i count)
        (let* ((id (aref data (+ start 1 (* 2 i))))
               (tables (aref data (+ start 2 (* 2 i))))
               (dc (ash tables -4)) (ac (logand tables 15))
               (c (or (find id (jf-components frame) :key #'jc-id)
                      (%jpeg-fail "a scan names component ~d, which the frame has not" id))))
          (when (find c components) (%jpeg-fail "a scan names component ~d twice" id))
          (unless (and (<= dc 3) (<= ac 3)) (%jpeg-fail "a scan names a Huffman slot past 3"))
          (setf (svref components i) c)
          (when (or (not progressive) (and (zerop ss) (zerop ah)))
            (setf (jc-dc c) (or (svref dc-tables dc) (%jpeg-standard-table :dc dc))))
          (when (or (not progressive) (plusp ss))
            (setf (jc-ac c) (or (svref ac-tables ac) (%jpeg-standard-table :ac ac))))
          (unless (jc-quant c)
            (setf (jc-quant c)
                  (copy-seq (or (svref quant (jc-tq c))
                                (%jpeg-fail "component ~d's quantization table ~d is not defined" id (jc-tq c))))))))
      (when (> count 1)
        (let ((blocks (loop for c across components sum (* (jc-h c) (jc-v c)))))
          (when (> blocks 10)
            (%jpeg-fail "a scan's coded unit holds ~d blocks, past the format's 10" blocks))))
      (if progressive
          (values components ss se ah al)
          (values components 0 63 0 0)))))

;;; --- decoding a scan ---------------------------------------------------------------
;;; A sequential frame's blocks go through the inverse DCT as they decode,
;;; into the component's plane; a progressive frame's coefficients gather
;;; scan by scan and are transformed once the last has come.

(defun %jpeg-scan (frame data start end components ss se ah al interval)
  "Decode the entropy-coded data of one scan from START in DATA, restart
markers every INTERVAL coded units when it is not 0. => the position to look
for the next marker from."
  (declare (type jpeg-frame frame) (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end) (type simple-vector components)
           (type (integer 0 63) ss se) (type (integer 0 15) ah al)
           (type (unsigned-byte 16) interval)
           (optimize speed (safety 1)))
  (let* ((b (%make-jpeg-bits data end))
         (natural *jpeg-natural-order*)
         (mode (cond ((not (jf-progressive frame)) 0)
                     ((zerop ss) (if (zerop ah) 1 2))
                     (t (if (zerop ah) 3 4))))
         (single (= (length components) 1))
         (lead (svref components 0))
         (mcux (if single (jc-wib lead) (jf-mcux frame)))
         (mcuy (if single (jc-hib lead) (jf-mcuy frame)))
         (unit (make-array 64 :element-type '(signed-byte 16) :initial-element 0))
         (ws (make-array 64 :element-type '(signed-byte 32) :initial-element 0))
         (eobrun 0) (togo interval) (expected 0)
         (p1 (ash 1 al)) (m1 (ash -1 al)))
    (declare (type fixnum mcux mcuy eobrun togo) (type (integer 0 7) expected)
             (type (integer 0 4) mode) (type jpeg-component lead)
             (type (integer 1 8192) p1) (type (integer -8192 -1) m1))
    (setf (jb-pos b) start)
    (loop for c across components do (setf (jc-pred c) 0))
    (labels ((dc-step (c)
               ;; the next DC difference added to C's prediction
               (declare (type jpeg-component c))
               (let ((s (%jb-decode b (jc-dc c))))
                 (unless (zerop s)
                   (let ((pred (+ (jc-pred c) (%jpeg-extend (%jb-bits b s) s))))
                     (unless (typep pred '(signed-byte 32))
                       (%jpeg-fail "a DC coefficient runs past what an int holds"))
                     (setf (jc-pred c) pred)))
                 (jc-pred c)))
             (sequential (c bx by)
               (declare (type jpeg-component c) (type fixnum bx by))
               (fill unit 0)
               (setf (aref unit 0) (%jpeg-wrap16 (dc-step c)))
               (let ((ac (jc-ac c)) (k 1))
                 (declare (type fixnum k))
                 (loop while (< k 64)
                       do (let* ((rs (%jb-decode b ac)) (r (ash rs -4)) (s (logand rs 15)))
                            (cond ((plusp s)
                                   (incf k r)
                                   (setf (aref unit (aref natural k)) (%jpeg-extend (%jb-bits b s) s))
                                   (incf k))
                                  ((= r 15) (incf k 16))
                                  (t (return))))))
               (let ((stride (* 8 (jc-bpl c))))
                 (%jpeg-idct unit 0 (jc-quant c) ws (jc-plane c) (+ (* by 8 stride) (* bx 8)) stride)))
             (dc-first (c bx by)
               (declare (type jpeg-component c) (type fixnum bx by))
               (setf (aref (jc-coefs c) (* 64 (+ (* by (jc-bpl c)) bx)))
                     (%jpeg-wrap16 (ash (dc-step c) al))))
             (dc-refine (c bx by)
               (declare (type jpeg-component c) (type fixnum bx by))
               (when (= 1 (%jb-bits b 1))
                 (let ((coefs (jc-coefs c)) (at (* 64 (+ (* by (jc-bpl c)) bx))))
                   (setf (aref coefs at) (%jpeg-wrap16 (logior (aref coefs at) p1))))))
             (ac-first (c bx by)
               (declare (type jpeg-component c) (type fixnum bx by))
               (if (plusp eobrun)
                   (decf eobrun)
                   (let ((coefs (jc-coefs c)) (base (* 64 (+ (* by (jc-bpl c)) bx))) (ac (jc-ac c)) (k ss))
                     (declare (type fixnum k base))
                     (loop while (<= k se)
                           do (let* ((rs (%jb-decode b ac)) (r (ash rs -4)) (s (logand rs 15)))
                                (cond ((plusp s)
                                       (incf k r)
                                       (setf (aref coefs (+ base (aref natural k)))
                                             (%jpeg-wrap16 (ash (%jpeg-extend (%jb-bits b s) s) al)))
                                       (incf k))
                                      ((= r 15) (incf k 16))
                                      (t (setf eobrun (ash 1 r))
                                         (when (plusp r) (incf eobrun (%jb-bits b r)))
                                         (decf eobrun)
                                         (return))))))))
             (ac-refine (c bx by)
               (declare (type jpeg-component c) (type fixnum bx by))
               (let ((coefs (jc-coefs c)) (base (* 64 (+ (* by (jc-bpl c)) bx))) (ac (jc-ac c)) (k ss))
                 (declare (type fixnum k base))
                 (flet ((correct (at)
                          ;; a correction bit for the coefficient at AT, already nonzero
                          (declare (type fixnum at))
                          (when (= 1 (%jb-bits b 1))
                            (let ((value (aref coefs at)))
                              (when (zerop (logand value p1))
                                (setf (aref coefs at)
                                      (%jpeg-wrap16 (if (>= value 0) (+ value p1) (+ value m1)))))))))
                   (when (zerop eobrun)
                     (loop named band
                           while (<= k se)
                           do (let* ((rs (%jb-decode b ac)) (r (ash rs -4)) (s (logand rs 15)))
                                (declare (type fixnum r s))
                                (cond ((plusp s) (setf s (if (= 1 (%jb-bits b 1)) p1 m1)))
                                      ((/= r 15)
                                       (setf eobrun (ash 1 r))
                                       (when (plusp r) (incf eobrun (%jb-bits b r)))
                                       (return-from band)))
                                ;; past nonzero coefficients, correcting each, and R zero ones
                                (loop named advance
                                      do (let ((at (+ base (aref natural k))))
                                           (if (/= 0 (aref coefs at))
                                               (correct at)
                                               (when (< (decf r) 0) (return-from advance)))
                                           (incf k))
                                      while (<= k se))
                                (unless (zerop s)
                                  (setf (aref coefs (+ base (aref natural k))) s))
                                (incf k))))
                   (when (plusp eobrun)
                     (loop while (<= k se)
                           do (let ((at (+ base (aref natural k))))
                                (unless (zerop (aref coefs at)) (correct at))
                                (incf k)))
                     (decf eobrun)))))
             (one (c bx by)
               (declare (type jpeg-component c) (type fixnum bx by))
               (case mode
                 (0 (sequential c bx by))
                 (1 (dc-first c bx by))
                 (2 (dc-refine c bx by))
                 (3 (ac-first c bx by))
                 (t (ac-refine c bx by)))))
      (dotimes (my mcuy)
        (dotimes (mx mcux)
          (when (plusp interval)
            (when (zerop togo)
              (%jpeg-restart b expected)
              (setf expected (logand (1+ expected) 7) togo interval eobrun 0)
              (loop for c across components do (setf (jc-pred c) 0)))
            (decf togo))
          (unless (jb-short b)
            (if single
                (one lead mx my)
                (loop for c across components
                      do (let ((h (jc-h c)) (v (jc-v c)))
                           (dotimes (by v)
                             (dotimes (bx h)
                               (one c (+ (* mx h) bx) (+ (* my v) by)))))))))))
    (cond ((zerop (jb-marker b)) (jb-pos b))
          ((>= (jb-pos b) end) end)
          (t (- (jb-pos b) 2)))))

;;; --- the inverse DCT: jidctint.c's jpeg_idct_islow ---------------------------------

(defun %jpeg-idct (in base quant ws out offset stride)
  "The 64 coefficients of IN from BASE, row-major, times QUANT, through
libjpeg's integer inverse DCT into the 8 x 8 samples of OUT from OFFSET, rows
STRIDE apart; WS is the workspace between its two passes."
  (declare (type (simple-array (signed-byte 16) (*)) in)
           (type (simple-array (unsigned-byte 16) (64)) quant)
           (type (simple-array (signed-byte 32) (64)) ws)
           (type (simple-array (unsigned-byte 8) (*)) out)
           (type fixnum base offset stride)
           (optimize speed (safety 0)))
  (unless (and (<= 0 base) (<= (+ base 64) (length in)) (<= 0 offset) (< 0 stride)
               (<= (+ offset (* 7 stride) 8) (length out)))
    (error "a JPEG block falls outside its plane"))
  ;; columns: dequantised, scaled up by 2^2 (PASS1_BITS)
  (dotimes (col 8)
    (macrolet ((q (row) `(* (aref in (+ base col ,(* 8 row))) (aref quant (+ col ,(* 8 row)))))
               (w (row) `(aref ws (+ col ,(* 8 row)))))
      (if (and (zerop (aref in (+ base col 8))) (zerop (aref in (+ base col 16)))
               (zerop (aref in (+ base col 24))) (zerop (aref in (+ base col 32)))
               (zerop (aref in (+ base col 40))) (zerop (aref in (+ base col 48)))
               (zerop (aref in (+ base col 56))))
          (let ((dc (%jpeg-wrap32 (ash (q 0) 2))))
            (setf (w 0) dc (w 1) dc (w 2) dc (w 3) dc (w 4) dc (w 5) dc (w 6) dc (w 7) dc))
          (let* ((z2 (q 2)) (z3 (q 6))
                 (z1 (* (+ z2 z3) 4433))
                 (even2 (+ z1 (* z3 -15137)))
                 (even3 (+ z1 (* z2 6270)))
                 (even0 (ash (+ (q 0) (q 4)) 13))
                 (even1 (ash (- (q 0) (q 4)) 13))
                 (tmp10 (+ even0 even3)) (tmp13 (- even0 even3))
                 (tmp11 (+ even1 even2)) (tmp12 (- even1 even2))
                 (t7 (q 7)) (t5 (q 5)) (t3 (q 3)) (t1 (q 1))
                 (o1 (* (+ t7 t1) -7373))
                 (o2 (* (+ t5 t3) -20995))
                 (o5 (* (+ t7 t3 t5 t1) 9633))
                 (o3 (+ (* (+ t7 t3) -16069) o5))
                 (o4 (+ (* (+ t5 t1) -3196) o5))
                 (odd0 (+ (* t7 2446) o1 o3))
                 (odd1 (+ (* t5 16819) o2 o4))
                 (odd2 (+ (* t3 25172) o2 o3))
                 (odd3 (+ (* t1 12299) o1 o4)))
            (flet ((descale (x) (%jpeg-wrap32 (ash (+ x 1024) -11))))
              (declare (inline descale))
              (setf (w 0) (descale (+ tmp10 odd3)) (w 7) (descale (- tmp10 odd3))
                    (w 1) (descale (+ tmp11 odd2)) (w 6) (descale (- tmp11 odd2))
                    (w 2) (descale (+ tmp12 odd1)) (w 5) (descale (- tmp12 odd1))
                    (w 3) (descale (+ tmp13 odd0)) (w 4) (descale (- tmp13 odd0))))))))
  ;; rows: descaled by 2^18 and centred on 128
  (dotimes (row 8)
    (let ((r (* 8 row)) (o (+ offset (* row stride))))
      (declare (type fixnum r o))
      (macrolet ((w (i) `(aref ws (+ r ,i)))
                 (put (i x) `(setf (aref out (+ o ,i)) (%jpeg-clamp (+ 128 (ash (+ ,x 131072) -18))))))
        (if (and (zerop (w 1)) (zerop (w 2)) (zerop (w 3)) (zerop (w 4))
                 (zerop (w 5)) (zerop (w 6)) (zerop (w 7)))
            (let ((v (%jpeg-clamp (+ 128 (ash (+ (w 0) 16) -5)))))
              (dotimes (i 8) (setf (aref out (+ o i)) v)))
            (let* ((z2 (w 2)) (z3 (w 6))
                   (z1 (* (+ z2 z3) 4433))
                   (even2 (+ z1 (* z3 -15137)))
                   (even3 (+ z1 (* z2 6270)))
                   (even0 (ash (+ (w 0) (w 4)) 13))
                   (even1 (ash (- (w 0) (w 4)) 13))
                   (tmp10 (+ even0 even3)) (tmp13 (- even0 even3))
                   (tmp11 (+ even1 even2)) (tmp12 (- even1 even2))
                   (t7 (w 7)) (t5 (w 5)) (t3 (w 3)) (t1 (w 1))
                   (o1 (* (+ t7 t1) -7373))
                   (o2 (* (+ t5 t3) -20995))
                   (o5 (* (+ t7 t3 t5 t1) 9633))
                   (o3 (+ (* (+ t7 t3) -16069) o5))
                   (o4 (+ (* (+ t5 t1) -3196) o5))
                   (odd0 (+ (* t7 2446) o1 o3))
                   (odd1 (+ (* t5 16819) o2 o4))
                   (odd2 (+ (* t3 25172) o2 o3))
                   (odd3 (+ (* t1 12299) o1 o4)))
              (put 0 (+ tmp10 odd3)) (put 7 (- tmp10 odd3))
              (put 1 (+ tmp11 odd2)) (put 6 (- tmp11 odd2))
              (put 2 (+ tmp12 odd1)) (put 5 (- tmp12 odd1))
              (put 3 (+ tmp13 odd0)) (put 4 (- tmp13 odd0)))))))
  out)

(defun %jpeg-transform-all (frame)
  "A progressive FRAME's gathered coefficients through the inverse DCT into
each component's plane, the coefficients then let go. A component no scan
carried stays gray, as libjpeg leaves it."
  (let ((ws (make-array 64 :element-type '(signed-byte 32) :initial-element 0)))
    (loop for c across (jf-components frame)
          do (let* ((bpl (jc-bpl c)) (bl (jc-bl c)) (stride (* 8 bpl))
                    (plane (make-array (* bpl bl 64) :element-type '(unsigned-byte 8) :initial-element 128))
                    (coefs (jc-coefs c)) (quant (jc-quant c)))
               (declare (type fixnum bpl bl stride))
               (when quant
                 (dotimes (by bl)
                   (dotimes (bx bpl)
                     (%jpeg-idct coefs (* 64 (+ (* by bpl) bx)) quant ws plane
                                 (+ (* by 8 stride) (* bx 8)) stride))))
               (setf (jc-plane c) plane (jc-coefs c) nil)))))

;;; --- upsampling and colour ---------------------------------------------------------

(defun %jpeg-upsample (c y hmax vmax width row)
  "Component C's samples on output row Y at full size, WIDTH of them into
ROW, by libjpeg-turbo's method for its sampling: as stored at full size;
the triangle filter for 2:1 across (h2v1, past two samples wide), 2:1 down
(h1v2) or both (h2v2, past two wide), the rows past the top and bottom
edges the edge rows themselves; any other whole ratio by repeating
samples."
  (declare (type jpeg-component c) (type fixnum y width) (type (integer 1 4) hmax vmax)
           (type (simple-array (unsigned-byte 8) (*)) row) (optimize speed (safety 1)))
  (let ((h (jc-h c)) (v (jc-v c)) (plane (jc-plane c)) (stride (* 8 (jc-bpl c)))
        (dw (jc-dw c)) (dh (jc-dh c)))
    (declare (type (simple-array (unsigned-byte 8) (*)) plane) (type fixnum stride dw dh))
    (flet ((neighbour (r)
             ;; the row a triangle filter blends with row R: above it on an
             ;; even output row, below on an odd one
             (declare (type fixnum r))
             (* stride (if (evenp y) (max 0 (1- r)) (min (1- dh) (1+ r))))))
      (cond ((and (= h hmax) (= v vmax))
             (replace row plane :start2 (* y stride) :end2 (+ (* y stride) width)))
            ((and (= (* 2 h) hmax) (= v vmax) (> dw 2))
             (let ((in (* y stride)))
               (declare (type fixnum in))
               (setf (aref row 0) (aref plane in)
                     (aref row 1) (ash (+ (* 3 (aref plane in)) (aref plane (1+ in)) 2) -2))
               (loop for i of-type fixnum from 1 below (1- dw)
                     do (let ((here (* 3 (aref plane (+ in i)))))
                          (setf (aref row (* 2 i)) (ash (+ here (aref plane (+ in i -1)) 1) -2)
                                (aref row (1+ (* 2 i))) (ash (+ here (aref plane (+ in i 1)) 2) -2))))
               (let ((i (1- dw)))
                 (setf (aref row (* 2 i)) (ash (+ (* 3 (aref plane (+ in i))) (aref plane (+ in i -1)) 1) -2)
                       (aref row (1+ (* 2 i))) (aref plane (+ in i))))))
            ((and (= h hmax) (= (* 2 v) vmax))
             (let* ((r (ash y -1)) (near (* r stride)) (far (neighbour r)) (bias (if (evenp y) 1 2)))
               (declare (type fixnum near far))
               (dotimes (x dw)
                 (setf (aref row x) (ash (+ (* 3 (aref plane (+ near x))) (aref plane (+ far x)) bias) -2)))))
            ((and (= (* 2 h) hmax) (= (* 2 v) vmax) (> dw 2))
             (let* ((r (ash y -1)) (near (* r stride)) (far (neighbour r)))
               (declare (type fixnum near far))
               (flet ((sum (i)
                        (declare (type fixnum i))
                        (+ (* 3 (aref plane (+ near i))) (aref plane (+ far i)))))
                 (declare (inline sum))
                 (let* ((last (sum 0)) (here (sum 1)) (next 0))
                   (declare (type fixnum last here next))
                   (setf (aref row 0) (ash (+ (* 4 last) 8) -4)
                         (aref row 1) (ash (+ (* 3 last) here 7) -4))
                   (loop for i of-type fixnum from 1 below (1- dw)
                         do (setf next (sum (1+ i))
                                  (aref row (* 2 i)) (ash (+ (* 3 here) last 8) -4)
                                  (aref row (1+ (* 2 i))) (ash (+ (* 3 here) next 7) -4)
                                  last here
                                  here next))
                   (let ((i (1- dw)))
                     (setf (aref row (* 2 i)) (ash (+ (* 3 here) last 8) -4)
                           (aref row (1+ (* 2 i))) (ash (+ (* 4 here) 7) -4)))))))
            (t
             (let ((across (floor hmax h)) (in (* stride (floor y (floor vmax v)))))
               (declare (type fixnum in) (type (integer 1 4) across))
               (dotimes (x width)
                 (setf (aref row x) (aref plane (+ in (floor x across)))))))))
    row))

(defun %jpeg-colour-space (frame jfif adobe)
  "What FRAME's components hold, as libjpeg guesses it: :gray; :ycc, or :rgb
for an Adobe transform 0 or the ids R, G, B; :cmyk, or :ycck for Adobe's
transform 2."
  (let ((components (jf-components frame)))
    (case (length components)
      (1 :gray)
      (3 (cond (jfif :ycc)
               (adobe (if (zerop adobe) :rgb :ycc))
               ((equal (map 'list #'jc-id components) '(82 71 66)) :rgb)
               (t :ycc)))
      (t (if (and adobe (/= adobe 0)) :ycck :cmyk)))))

(defun %jpeg-colour-row (space rows out at width)
  "One row of WIDTH pixels from the full-size component ROWS, in SPACE, into
OUT from AT: gray as it is, everything else as RGB -- YCbCr by jdcolor.c's
tables, CMYK and YCCK as PIL turns Adobe's inverted CMYK into RGB."
  (declare (type simple-vector rows) (type (simple-array (unsigned-byte 8) (*)) out)
           (type fixnum at width) (optimize speed (safety 1)))
  (let ((r0 (svref rows 0)))
    (declare (type (simple-array (unsigned-byte 8) (*)) r0))
    (if (eq space :gray)
        (replace out r0 :start1 at :end2 width)
        (let ((r1 (svref rows 1)) (r2 (svref rows 2))
              (crr *jpeg-cr-red*) (cbb *jpeg-cb-blue*) (crg *jpeg-cr-green*) (cbg *jpeg-cb-green*))
          (declare (type (simple-array (unsigned-byte 8) (*)) r1 r2))
          (case space
            (:ycc
             (dotimes (x width)
               (let ((yy (aref r0 x)) (cb (aref r1 x)) (cr (aref r2 x)) (o (+ at (* 3 x))))
                 (declare (type fixnum o))
                 (setf (aref out o) (%jpeg-clamp (+ yy (aref crr cr)))
                       (aref out (+ o 1)) (%jpeg-clamp (+ yy (ash (+ (aref cbg cb) (aref crg cr)) -16)))
                       (aref out (+ o 2)) (%jpeg-clamp (+ yy (aref cbb cb)))))))
            (:rgb
             (dotimes (x width)
               (let ((o (+ at (* 3 x))))
                 (declare (type fixnum o))
                 (setf (aref out o) (aref r0 x) (aref out (+ o 1)) (aref r1 x) (aref out (+ o 2)) (aref r2 x)))))
            (t
             (let ((r3 (svref rows 3)) (ycck (eq space :ycck)))
               (declare (type (simple-array (unsigned-byte 8) (*)) r3))
               (flet ((ink (stored k)
                        ;; PIL's cmyk2rgb on the inverted ink: k - k (255 - stored) / 255
                        (declare (type (unsigned-byte 8) stored k))
                        (let ((tmp (+ (* (- 255 stored) k) 128)))
                          (%jpeg-clamp (- k (ash (+ (ash tmp -8) tmp) -8))))))
                 (declare (inline ink))
                 (dotimes (x width)
                   (let ((o (+ at (* 3 x))) (k (aref r3 x)) (c (aref r0 x)) (m (aref r1 x)) (ye (aref r2 x)))
                     (declare (type fixnum o) (type (unsigned-byte 8) c m ye))
                     (when ycck
                       (let ((yy c) (cb m) (cr ye))
                         (setf c (%jpeg-clamp (- 255 (+ yy (aref crr cr))))
                               m (%jpeg-clamp (- 255 (+ yy (ash (+ (aref cbg cb) (aref crg cr)) -16))))
                               ye (%jpeg-clamp (- 255 (+ yy (aref cbb cb)))))))
                     (setf (aref out o) (ink c k) (aref out (+ o 1)) (ink m k) (aref out (+ o 2)) (ink ye k)))))))))))
  out)

(defun %jpeg-colour (frame space)
  "FRAME's planes as WIDTH x HEIGHT pixels in SPACE: => (values DATA
CHANNELS), one channel for gray and three, RGB, for the rest."
  (let* ((width (jf-width frame)) (height (jf-height frame))
         (components (jf-components frame))
         (channels (if (eq space :gray) 1 3))
         (out (make-array (* width height channels) :element-type '(unsigned-byte 8)))
         (rows (map 'simple-vector
                    (lambda (c) (declare (ignore c)) (make-array (+ width 16) :element-type '(unsigned-byte 8)))
                    components)))
    (dotimes (y height)
      (loop for c across components
            for row across rows
            do (%jpeg-upsample c y (jf-hmax frame) (jf-vmax frame) width row))
      (%jpeg-colour-row space rows out (* y width channels) width))
    (values out channels)))

;;; --- the file ----------------------------------------------------------------------

(defun jpeg-decode (octets)
  "OCTETS, a JPEG file, decoded as libjpeg-turbo decodes it under PIL: =>
(values DATA WIDTH HEIGHT CHANNELS), DATA its samples row-major, 8 bits each,
CHANNELS 1 for a gray JPEG and 3, RGB, for any other -- as stored, its EXIF
orientation not applied. Signals JPEG-MALFORMED, saying why, for bytes it
cannot read."
  (let* ((data (if (typep octets '(simple-array (unsigned-byte 8) (*)))
                   octets
                   (coerce octets '(simple-array (unsigned-byte 8) (*)))))
         (end (length data))
         (quant (make-array 4 :initial-element nil))
         (dc-tables (make-array 4 :initial-element nil))
         (ac-tables (make-array 4 :initial-element nil))
         (interval 0) (frame nil) (scans 0) (jfif nil) (adobe nil) (pos 2))
    (unless (and (>= end 2) (= (aref data 0) #xFF) (= (aref data 1) #xD8))
      (%jpeg-fail "its bytes do not open a JPEG"))
    (flet ((says (at text)
             (and (<= (+ at (length text)) end)
                  (every (lambda (ch i) (= (char-code ch) (aref data (+ at i))))
                         text (alexandria:iota (length text))))))
      (loop
        (multiple-value-bind (marker after) (%jpeg-next-marker data pos end)
          (when (or (null marker) (= marker #xD9)) (return))
          (setf pos after)
          (cond ((or (<= #xD0 marker #xD7) (= marker #x01)))
                ((= marker #xD8) (%jpeg-fail "it opens a second image inside the first"))
                (t
                 (unless (<= (+ pos 2) end) (%jpeg-fail "a marker segment is cut short"))
                 (let* ((length (%jpeg-u16 data pos)) (start (+ pos 2)) (stop (+ pos length)))
                   (when (or (< length 2) (> stop end))
                     (%jpeg-fail "a marker segment runs past the end of the file"))
                   (setf pos stop)
                   (case marker
                     ((#xC0 #xC1 #xC2)
                      (when frame (%jpeg-fail "it holds two frames"))
                      (setf frame (%jpeg-frame data start (- length 2) (= marker #xC2))))
                     ((#xC3 #xC7 #xCB #xCF) (%jpeg-fail "it is a lossless JPEG, which this does not read"))
                     ((#xC5 #xC6 #xCD #xCE) (%jpeg-fail "it is a hierarchical JPEG, which this does not read"))
                     ((#xC9 #xCA) (%jpeg-fail "it is an arithmetic-coded JPEG, which this does not read"))
                     (#xC4 (%jpeg-dht data start stop dc-tables ac-tables))
                     (#xDB (%jpeg-dqt data start stop quant))
                     (#xDD (when (< length 4) (%jpeg-fail "its restart interval is cut short"))
                      (setf interval (%jpeg-u16 data start)))
                     (#xE0 (when (and (>= length 16) (says start (format nil "JFIF~c" (code-char 0))))
                             (setf jfif t)))
                     (#xEE (when (and (>= length 14) (says start "Adobe"))
                             (setf adobe (aref data (+ start 11)))))
                     (#xDA
                      (unless frame (%jpeg-fail "a scan comes before the frame header"))
                      (multiple-value-bind (components ss se ah al)
                          (%jpeg-scan-header data start (- length 2) frame quant dc-tables ac-tables)
                        (setf pos (%jpeg-scan frame data stop end components ss se ah al interval))
                        (incf scans))))))))))
    (unless frame (%jpeg-fail "it holds no frame"))
    (when (zerop scans) (%jpeg-fail "it holds no scan"))
    (when (jf-progressive frame) (%jpeg-transform-all frame))
    (multiple-value-bind (pixels channels) (%jpeg-colour frame (%jpeg-colour-space frame jfif adobe))
      (values pixels (jf-width frame) (jf-height frame) channels))))

(defun jpeg-pixels (octets)
  "OCTETS, a JPEG, decoded to gray or RGB PIXELS for FIT-IMAGE, as stored:
*IMAGE-DECODERS*'s image/jpeg, its EXIF orientation the caller's
(JPEG-ORIENTATION). Signals IMAGE-UNFIT, saying why, for bytes it cannot
read -- and for any other error inside the decoder, so a file no one
foresaw costs the picture, never the turn."
  (handler-case (multiple-value-bind (data width height channels) (jpeg-decode octets)
                  (%make-pixels width height channels data))
    (jpeg-malformed (e)
      (error 'image-unfit :reason (format nil "the JPEG does not decode here: ~a" (jpeg-malformed-reason e))))
    (error (e)
      (error 'image-unfit :reason (format nil "the JPEG does not decode here: ~a" e)))))
