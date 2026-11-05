;;;; raster-test.lisp --- sixel and half-block pictures.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun esc-text (control &rest arguments)
  "CONTROL formatted with ~E standing for ESC."
  (apply #'format nil (ppcre:regex-replace-all "~E" control (string #\Escape)) arguments))

(deftest sixel-data-writes-registers-bands-and-runs ()
  ;; Five wide, seven tall: red but for a clear corner and a blue last
  ;; column. Two bands of six rows; a line per colour, cut at its last set
  ;; bit, run-length coded past three repeats.
  (let ((indices (make-array 35 :element-type 'fixnum :initial-element 0)))
    (dotimes (y 7) (setf (aref indices (+ (* y 5) 4)) 1))
    (setf (aref indices 0) -1)
    (is (equal (esc-text "~EP0;1;0q\"1;1;5;7#0;2;100;0;0#1;2;0;0;100#0}~~~~~~$#1!4?~~-#0!4@$#1!4?@~E\\")
               (nlt:sixel-data indices #((255 0 0) (0 0 255)) 5 7)))
    (is (equal (esc-text "~EP0;1;0q\"1;1;5;1#0;2;100;0;0#1;2;0;0;100#0!4@$#1!4?@~E\\")
               (nlt:sixel-data indices #((255 0 0) (0 0 255)) 5 1 :top 6)) "from a first row: the rows below it alone")))

(deftest a-palette-keeps-few-colours-whole-and-caps-many ()
  (multiple-value-bind (indices palette)
      (nlt:quantize-colors (coerce (list #xff0000 -1 #x0000ff #xff0000 #x00ff00) '(simple-array fixnum (*))))
    (is (= 3 (length palette)) "three colours, three registers")
    (is (= -1 (aref indices 1)) "a clear pixel stays clear")
    (is (equal '(255 0 0) (aref palette (aref indices 0))) "a colour that had a bin to itself comes back exact")
    (is (= (aref indices 0) (aref indices 3)))
    (is (equal '(0 0 255) (aref palette (aref indices 2)))))
  (let ((many (make-array 4096 :element-type 'fixnum)))
    (dotimes (i 4096) (setf (aref many i) (logior (ash (* 16 (ldb (byte 4 8) i)) 16) (ash (* 16 (ldb (byte 4 4) i)) 8) (* 16 (ldb (byte 4 0) i)))))
    (multiple-value-bind (indices palette) (nlt:quantize-colors many 16)
      (is (= 16 (length palette)) "4096 colours in 16 registers")
      (is (every (lambda (index) (< -1 index 16)) indices)))))

(deftest raster-colours-read-alpha-against-the-canvas ()
  (let ((data (coerce '(255 0 0 255  0 255 0 0  255 255 255 200  10 20 30 127)
                      '(simple-array (unsigned-byte 8) (*)))))
    (is (equalp #(#xff0000 -1 #xc8c8c8 -1) (nlt:raster-colors data 4 1 4 '(0 0 0))) "opaque as itself, under half opaque clear, between laid over the canvas"))
  (is (equalp #(#x404040) (nlt:raster-colors (coerce '(64) '(simple-array (unsigned-byte 8) (*))) 1 1 1)) "gray is its sample thrice"))

(deftest half-blocks-pair-each-pixel-with-the-one-under-it ()
  ;; Two wide, three tall: the last row has nothing under it, and a clear
  ;; pixel over an opaque one turns the block over.
  (let ((grid (nlt:half-block-grid (coerce (list #x010203 -1  #x040506 #x070809  #x0a0b0c -1)
                                           '(simple-array fixnum (*)))
                                   2 3
                                   (lambda (color foreground-p) (list (if foreground-p :fg :bg) color)))))
    (is (= 2 (length grid)) "two pixel rows a cell row")
    (is (equal (list #\UPPER_HALF_BLOCK '(:fg #x010203) '(:bg #x040506)) (aref (aref grid 0) 0)))
    (is (equal (list #\LOWER_HALF_BLOCK '(:fg #x070809) :keep) (aref (aref grid 0) 1)) "clear over opaque: the lower block over the cell's own ground")
    (is (equal (list #\UPPER_HALF_BLOCK '(:fg #x0a0b0c) :keep) (aref (aref grid 1) 0)) "a last odd row over the cell's own ground")
    (is (null (aref (aref grid 1) 1)) "both clear: the cell is left alone")))

(deftest octants-and-sextants-are-unicode-s-own-patterns ()
  ;; Every octant mask draws: the block elements, Unicode's run from
  ;; U+1CD00 (BLOCK OCTANT-3 first, -2345678 last), and the eight kept in
  ;; other blocks as their complement over swapped colours.
  (let ((glyphs nlt::*octant-glyphs*))
    (is (= 248 (count-if #'identity glyphs)))
    (is (eql (code-char #x1CD00) (aref glyphs (nlt::%cells-mask "3"))) "BLOCK OCTANT-3")
    (is (eql (code-char #x1CDE5) (aref glyphs (nlt::%cells-mask "2345678"))) "BLOCK OCTANT-2345678")
    (is (eql #\LEFT_HALF_BLOCK (aref glyphs (nlt::%cells-mask "1357"))))
    ;; Kept elsewhere: drawn as the complement.
    (is (every (lambda (cells)
                 (let ((mask (nlt::%cells-mask cells)))
                   (and (null (aref glyphs mask)) (aref glyphs (logxor 255 mask)))))
               '("1" "2" "7" "8" "12" "35" "46" "123456"))))
  ;; Each sextant's name lists its own pixels.
  (let ((glyphs nlt::*sextant-glyphs*))
    (is (every #'identity glyphs))
    (loop for mask below 64
          for char = (aref glyphs mask)
          when (> (char-code char) #x1FAFF)
            do (is (string= (format nil "BLOCK_SEXTANT-~{~d~}"
                                    (loop for i below 6 when (logbitp i mask) collect (1+ i)))
                            (char-name char))))))

(deftest octants-split-a-cell-between-two-colours ()
  (let ((r #xff0000) (b #x0000ff))
    (flet ((cell (pixels &optional (width 2) (height 4))
             (nlt:mosaic-grid (coerce pixels '(simple-array fixnum (*))) width height
                              (lambda (color foreground-p) (list (if foreground-p :fg :bg) color))
                              '(2 . 4))))
      ;; Pixels 1 2 3 red over blue.
      (is (equal (list (aref nlt::*octant-glyphs* 7) (list :fg r) (list :bg b))
                 (aref (aref (cell (list r r r b b b b b)) 0) 0)))
      ;; Pixel 1 alone is kept elsewhere: its complement, blue over red.
      (is (equal (list (code-char #x1CDE5) (list :fg b) (list :bg r))
                 (aref (aref (cell (list r b b b b b b b)) 0) 0)))
      ;; One colour: a full block.
      (is (equal (list #\FULL_BLOCK (list :fg r) (list :bg r))
                 (aref (aref (cell (list r r r r r r r r)) 0) 0)))
      ;; Clear pixels show the cell's own ground.
      (is (equal (list (code-char #x1CDE5) (list :fg r) :keep)
                 (aref (aref (cell (list -1 r r r r r r r)) 0) 0)))
      ;; A lone pixel no glyph draws over the ground takes the nearest
      ;; pattern that keeps it.
      (is (equal (list #\QUADRANT_UPPER_LEFT (list :fg r) :keep)
                 (aref (aref (cell (list r -1 -1 -1 -1 -1 -1 -1)) 0) 0)))
      (is (null (aref (aref (cell (list -1 -1 -1 -1 -1 -1 -1 -1)) 0) 0)) "all clear: left alone")
      (let ((grid (cell (make-list 15 :initial-element r) 3 5)))
        (is (and (= 2 (length grid)) (= 2 (length (aref grid 0)))) "3 x 5 pixels: two cells by two, the edge's clear")
        (is (equal (list (aref nlt::*octant-glyphs* (nlt::%cells-mask "1357")) (list :fg r) :keep)
                   (aref (aref grid 0) 1)))))))

(deftest the-terminal-says-its-attributes-and-cell-size ()
  (let ((msg (key-from-string (csi "?61;4;6;7;14;21;22;23;24;28;32;42;52c"))))
    (is (nlt:device-attributes-msg-p msg) "Windows Terminal's DA1 answer")
    (is (equal '(61 4 6 7 14 21 22 23 24 28 32 42 52) (nlt:device-attributes-msg-attributes msg))))
  (is (equal '(61 1 6) (nlt:device-attributes-msg-attributes (key-from-string (csi "?61;1;6c")))) "the console's, which lists no sixel")
  (let ((msg (key-from-string (csi "6;20;10t"))))
    (is (nlt:cell-size-msg-p msg))
    (is (and (eql 10 (nlt:cell-size-msg-width msg)) (eql 20 (nlt:cell-size-msg-height msg)))))
  (is (nlt:color-scheme-msg-p (key-from-string (csi "?997;1n"))) "the colour-scheme report still reads")
  (let ((msg (key-from-string (format nil "~CP>|VTE(8401)~C\\" #\Escape #\Escape))))
    (is (nlt:terminal-version-msg-p msg) "XTVERSION's answer")
    (is (equal "VTE(8401)" (nlt:terminal-version-msg-name msg))))
  (is (equal (esc-text "~E[16t~E[>q~E[c") (with-output-to-string (out) (nlt:request-terminal-attributes out))) "the cell size asked first, then the name, DA1 last"))

(defun frame-bytes (renderer rows images)
  "What RENDERER writes painting ROWS with IMAGES."
  (with-output-to-string (stream)
    (setf (nlt:renderer-output-stream renderer) stream)
    (nlt:render renderer (nlt:make-view rows :images images))))

(deftest a-sixel-is-drawn-again-when-its-cells-are-written-and-goes-with-them ()
  ;; A sixel lives in the cells it covers: text written into one takes that
  ;; part of it, and nothing deletes it but writing the cells it left.
  (let* ((renderer (nlt:make-renderer :width 10 :height 4))
         (image (nlt:make-terminal-image :protocol :sixel :data "<SIXEL>" :columns 3 :rows 2))
         (here (nlt:make-image-placement image :x 1 :y 1 :rows 2))
         (blank (list "" "" "" "")))
    (let ((first (frame-bytes renderer blank (list here))))
      (is (search (esc-text "~E[2;2H<SIXEL>") first) "drawn at its cell, 1-based"))
    (is (null (search "<SIXEL>" (frame-bytes renderer blank (list here)))) "an unchanged frame writes it no more")
    (let ((written (frame-bytes renderer (list "" "  X" "" "") (list here))))
      (is (search "X" written))
      (is (and (search "<SIXEL>" written) (< (search "X" written) (search "<SIXEL>" written))) "text in its cells: drawn again, after the text"))
    (is (null (search "<SIXEL>" (frame-bytes renderer (list "" "  X" "" "") (list here)))))
    (let ((gone (frame-bytes renderer (list "" "  X" "" "") '())))
      (is (null (search "<SIXEL>" gone)))
      (is (search "X" gone) "the cells it left are written again, so it goes")
      (is (null (search "_Ga=d" gone)) "no kitty delete for a sixel"))))

(deftest half-block-pictures-are-cells-of-the-frame ()
  (let* ((renderer (nlt:make-renderer :width 6 :height 2))
         (image (nlt:make-terminal-image
                 :protocol :blocks :columns 2 :rows 1
                 :data (vector (vector (list #\UPPER_HALF_BLOCK "38;2;255;0;0" "48;2;0;0;255") nil))))
         (bytes (frame-bytes renderer (list "ab" "") (list (nlt:make-image-placement image :x 1 :y 0 :rows 1)))))
    (is (search (esc-text "~E[0;38;2;255;0;0;48;2;0;0;255m~C" #\UPPER_HALF_BLOCK) bytes) "the block in the pixels' colours")
    (is (search "a" bytes) "the cell it leaves alone keeps the row's own")
    (is (null (search "b" bytes)) "the cell it covers does not")
    (is (null (search (esc-text "~EP") bytes)) "and nothing but cells is written")))
