;;;; qr.lisp --- this machine's address as a QR code, for a phone's camera.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A phone reaches the address fastest by pointing its camera at it, so the
;;;; /link panel and the web page's Link tab both show it as a QR code. This is
;;;; the whole encoder for that one job: bytes mode, error correction level M
;;;; (a seventh of the code can be lost and it still reads), the smallest
;;;; version that holds the text, and the mask the standard's penalty rules
;;;; score lowest -- after Project Nayuki's QR Code generator (MIT), the
;;;; reference encoder, whose steps and tables these are. What it answers is
;;;; rows of `1' dark and `0' light, the picture a list dialog carries.

(in-package #:nodecode-link)

(defparameter +qr-ecc-per-block+
  #(10 16 26 18 24 16 18 22 22 26 30 22 22 24 24 28 28 26 26 26
    26 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28)
  "Error correction codewords in each block, versions 1 to 40, level M.")

(defparameter +qr-blocks+
  #(1 1 1 2 2 4 4 4 5 5 5 8 9 9 10 10 11 13 14 16
    17 17 18 20 21 23 25 26 28 29 31 33 35 37 38 40 43 45 47 49)
  "Blocks the codewords are cut into, versions 1 to 40, level M.")

;;; --- Reed-Solomon over GF(2^8) ------------------------------------------------------

(defun gf* (x y)
  "X times Y in GF(2^8), modulo the code's polynomial x^8 + x^4 + x^3 + x^2 + 1."
  (let ((z 0))
    (loop for i from 7 downto 0
          do (setf z (logxor (ash z 1) (* (ash z -7) #x11d)))
             (when (logbitp i y) (setf z (logxor z x))))
    z))

(defun rs-divisor (degree)
  "The generator polynomial of DEGREE, highest term first, its leading 1 left out."
  (let ((divisor (make-array degree :initial-element 0))
        (root 1))
    (setf (aref divisor (1- degree)) 1)
    (dotimes (i degree divisor)
      (dotimes (j degree)
        (setf (aref divisor j) (gf* (aref divisor j) root))
        (when (< (1+ j) degree)
          (setf (aref divisor j) (logxor (aref divisor j) (aref divisor (1+ j))))))
      (setf root (gf* root 2)))))

(defun rs-remainder (data divisor)
  "The error correction codewords for DATA: its remainder by DIVISOR."
  (let ((remainder (make-array (length divisor) :initial-element 0)))
    (loop for octet across data
          for factor = (logxor octet (aref remainder 0))
          do (replace remainder remainder :start2 1)
             (setf (aref remainder (1- (length remainder))) 0)
             (dotimes (i (length remainder))
               (setf (aref remainder i) (logxor (aref remainder i) (gf* (aref divisor i) factor)))))
    remainder))

;;; --- the codewords ------------------------------------------------------------------

(defun qr-raw-modules (version)
  "The modules VERSION leaves for codewords once its patterns are drawn."
  (let ((modules (+ (* (+ (* 16 version) 128) version) 64)))
    (when (>= version 2)
      (let ((aligns (+ (floor version 7) 2)))
        (decf modules (- (* (- (* 25 aligns) 10) aligns) 55))
        (when (>= version 7) (decf modules 36))))
    modules))

(defun qr-data-codewords (version)
  (- (floor (qr-raw-modules version) 8)
     (* (aref +qr-ecc-per-block+ (1- version)) (aref +qr-blocks+ (1- version)))))

(defun qr-version (length)
  "The smallest version whose data holds LENGTH bytes."
  (or (loop for version from 1 to 40
            when (<= (+ 4 (if (< version 10) 8 16) (* 8 length))
                     (* 8 (qr-data-codewords version)))
              return version)
      (error "link: ~d bytes are too many for a QR code" length)))

(defun qr-data (octets version)
  "OCTETS in bytes mode, ended and padded to VERSION's data codewords."
  (let* ((capacity (* 8 (qr-data-codewords version)))
         (bits (make-array capacity :element-type 'bit :fill-pointer 0)))
    (flet ((put (value width)
             (loop for i from (1- width) downto 0 do (vector-push (ldb (byte 1 i) value) bits))))
      (put 4 4)
      (put (length octets) (if (< version 10) 8 16))
      (loop for octet across octets do (put octet 8))
      (put 0 (min 4 (- capacity (fill-pointer bits))))
      (put 0 (mod (- (fill-pointer bits)) 8))
      ;; The two pad bytes the standard names, in turn: #xEC, #x11.
      (loop for pad = #xec then (logxor pad #xfd)
            while (< (fill-pointer bits) capacity)
            do (put pad 8)))
    (let ((data (make-array (/ capacity 8))))
      (dotimes (i (length data) data)
        (setf (aref data i) (loop for j below 8 sum (ash (aref bits (+ (* 8 i) j)) (- 7 j))))))))

(defun qr-codewords (data version)
  "DATA cut into VERSION's blocks, each followed by its error correction, and
the blocks interleaved a codeword at a time -- the order they are drawn in."
  (let* ((blocks (aref +qr-blocks+ (1- version)))
         (ecc (aref +qr-ecc-per-block+ (1- version)))
         (raw (floor (qr-raw-modules version) 8))
         (short (- blocks (mod raw blocks)))
         (short-length (floor raw blocks))
         (divisor (rs-divisor ecc))
         ;; A short block holds one codeword fewer, and a 0 where the long
         ;; ones hold it, skipped when they interleave.
         (pieces (loop with start = 0
                       for i below blocks
                       for end = (+ start (- short-length ecc) (if (< i short) 0 1))
                       for piece = (subseq data start end)
                       collect (concatenate 'vector piece (if (< i short) #(0) #())
                                            (rs-remainder piece divisor))
                       do (setf start end))))
    (coerce (loop for i to short-length
                  nconc (loop for piece in pieces
                              for j from 0
                              unless (and (= i (- short-length ecc)) (< j short))
                                collect (aref piece i)))
            'vector)))

;;; --- the symbol -----------------------------------------------------------------------

(defun qr-alignment-positions (version size)
  "Where VERSION's alignment patterns are centered, along either axis."
  (unless (= version 1)
    (let* ((count (+ (floor version 7) 2))
           (step (* 2 (floor (+ (* 8 version) (* 3 count) 5) (- (* 4 count) 4)))))
      (cons 6 (loop for i below (1- count)
                    collect (- size 7 (* step (- count 2 i))))))))

(defun qr-mask-p (mask x y)
  "Whether MASK flips the module at X, Y."
  (zerop (ecase mask
           (0 (mod (+ x y) 2))
           (1 (mod y 2))
           (2 (mod x 3))
           (3 (mod (+ x y) 3))
           (4 (mod (+ (floor x 3) (floor y 2)) 2))
           (5 (+ (mod (* x y) 2) (mod (* x y) 3)))
           (6 (mod (+ (mod (* x y) 2) (mod (* x y) 3)) 2))
           (7 (mod (+ (mod (+ x y) 2) (mod (* x y) 3)) 2)))))

(defun qr-penalty (dark size)
  "The standard's penalty for DARK: long runs, 2x2 blocks, what looks like a
finder pattern, and a dark share far from half."
  (let ((score 0)
        (line (make-array size :element-type 'bit)))
    (dolist (across '(t nil))
      (dotimes (a size)
        (dotimes (b size)
          (setf (aref line b) (if across (aref dark a b) (aref dark b a))))
        (loop with run = 1
              for b from 1 to size
              do (if (and (< b size) (= (aref line b) (aref line (1- b))))
                     (incf run)
                     (progn (when (>= run 5) (incf score (- run 2)))
                            (setf run 1))))
        (loop for b to (- size 11)
              do (when (or (not (mismatch #*10111010000 line :start2 b :end2 (+ b 11)))
                           (not (mismatch #*00001011101 line :start2 b :end2 (+ b 11))))
                   (incf score 40)))))
    (dotimes (y (1- size))
      (dotimes (x (1- size))
        (when (= (aref dark y x) (aref dark y (1+ x)) (aref dark (1+ y) x) (aref dark (1+ y) (1+ x)))
          (incf score 3))))
    (let* ((total (* size size))
           (count (loop for i below total count (= 1 (row-major-aref dark i)))))
      (+ score (* 10 (floor (abs (- (* 20 count) (* 10 total))) total))))))

(defun qr-matrix (text)
  "TEXT's QR code: a square bit array, (aref code y x), 1 dark."
  (let* ((octets (sb-ext:string-to-octets text :external-format :utf-8))
         (version (qr-version (length octets)))
         (size (+ 17 (* 4 version)))
         (dark (make-array (list size size) :element-type 'bit :initial-element 0))
         (fixed (make-array (list size size) :element-type 'bit :initial-element 0)))
    (labels ((pattern (x y on)
               (setf (aref dark y x) (if on 1 0) (aref fixed y x) 1))
             (format-bits (mask)
               ;; Level M's two bits are 00, so the data is the mask alone.
               (let ((remainder mask))
                 (dotimes (i 10) (setf remainder (logxor (ash remainder 1) (* (ash remainder -9) #x537))))
                 (let ((bits (logxor (logior (ash mask 10) remainder) #x5412)))
                   (flet ((bit-on (i) (logbitp i bits)))
                     (loop for i from 0 to 5 do (pattern 8 i (bit-on i)))
                     (pattern 8 7 (bit-on 6))
                     (pattern 8 8 (bit-on 7))
                     (pattern 7 8 (bit-on 8))
                     (loop for i from 9 below 15 do (pattern (- 14 i) 8 (bit-on i)))
                     (loop for i below 8 do (pattern (- size 1 i) 8 (bit-on i)))
                     (loop for i from 8 below 15 do (pattern 8 (+ (- size 15) i) (bit-on i)))
                     (pattern 8 (- size 8) t)))))
             (flip (mask)
               (dotimes (y size)
                 (dotimes (x size)
                   (when (and (zerop (aref fixed y x)) (qr-mask-p mask x y))
                     (setf (aref dark y x) (- 1 (aref dark y x))))))))
      (dotimes (i size)
        (pattern 6 i (evenp i))
        (pattern i 6 (evenp i)))
      (loop for (cx cy) in (list (list 3 3) (list (- size 4) 3) (list 3 (- size 4)))
            do (loop for dy from -4 to 4
                     do (loop for dx from -4 to 4
                              for x = (+ cx dx)
                              for y = (+ cy dy)
                              when (and (< -1 x size) (< -1 y size))
                                do (pattern x y (not (member (max (abs dx) (abs dy)) '(2 4)))))))
      (let* ((positions (qr-alignment-positions version size))
             (last (1- (length positions))))
        (loop for i from 0
              for cy in positions
              do (loop for j from 0
                       for cx in positions
                       unless (or (and (= i 0) (= j 0)) (and (= i 0) (= j last)) (and (= i last) (= j 0)))
                         do (loop for dy from -2 to 2
                                  do (loop for dx from -2 to 2
                                           do (pattern (+ cx dx) (+ cy dy)
                                                       (/= 1 (max (abs dx) (abs dy)))))))))
      (format-bits 0)
      (when (>= version 7)
        (let ((remainder version))
          (dotimes (i 12) (setf remainder (logxor (ash remainder 1) (* (ash remainder -11) #x1f25))))
          (let ((bits (logior (ash version 12) remainder)))
            (dotimes (i 18)
              (let ((a (+ (- size 11) (mod i 3)))
                    (b (floor i 3)))
                (pattern a b (logbitp i bits))
                (pattern b a (logbitp i bits)))))))
      ;; The codewords, two columns at a time from the right, up then down,
      ;; stepping over the vertical timing pattern.
      (let* ((codewords (qr-codewords (qr-data octets version) version))
             (total (* 8 (length codewords)))
             (i 0)
             (right (1- size)))
        (loop while (>= right 1)
              do (when (= right 6) (setf right 5))
                 (dotimes (vertical size)
                   (dotimes (j 2)
                     (let* ((x (- right j))
                            (y (if (zerop (logand (1+ right) 2)) (- size 1 vertical) vertical)))
                       (when (and (zerop (aref fixed y x)) (< i total))
                         (setf (aref dark y x)
                               (ldb (byte 1 (- 7 (logand i 7))) (aref codewords (ash i -3))))
                         (incf i)))))
                 (decf right 2)))
      (let ((best (loop with best = 0
                        with lowest = nil
                        for mask below 8
                        do (flip mask)
                           (format-bits mask)
                           (let ((penalty (qr-penalty dark size)))
                             (when (or (null lowest) (< penalty lowest))
                               (setf best mask lowest penalty)))
                           (flip mask)
                        finally (return best))))
        (flip best)
        (format-bits best)))
    dark))

(defun qr-rows (text &key (margin 2))
  "TEXT as a QR code with MARGIN light modules around it: one string a row,
`1' dark and `0' light."
  ;; The standard asks for a margin of four; two is what a phone's camera
  ;; needs, and a terminal has few rows to spare.
  (let* ((code (qr-matrix text))
         (size (array-dimension code 0))
         (side (+ size (* 2 margin))))
    (loop for y below side
          collect (let ((row (make-string side :initial-element #\0)))
                    (when (< -1 (- y margin) size)
                      (dotimes (x size)
                        (when (= 1 (aref code (- y margin) x))
                          (setf (char row (+ x margin)) #\1))))
                    row))))
