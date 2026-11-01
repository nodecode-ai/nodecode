;;;; screen-test.lisp --- the cell parser and the diff painter.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from the cell cases of cl-tuition's tests/test-terminal-caps.lisp
;;;; (Copyright (C) 2025 Anthony Green, MIT), plus the contract the frame
;;;; guard leans on: what the parser lays out is exactly what the painter
;;;; writes, glyph widths included.

(in-package #:nodecode.test)

(nlk:access (buffer nlt::screen-buffer) (cell nlt::cell) (reset nlt::cell))

(defparameter +osc8-open+ (format nil "~C]8;;https://example.com~C\\" #\Escape #\Escape))
(defparameter +osc8-close+ (format nil "~C]8;;~C\\" #\Escape #\Escape))

(defun painted-row (buffer y)
  "The characters row Y of BUFFER paints, trailing-half cells skipped."
  (with-output-to-string (out)
    (dotimes (x buffer.width)
      (let ((cell (nlt:screen-buffer-ref buffer x y)))
        (when (plusp cell.width)
          (write-char cell.char out))))))

(deftest parser-carries-osc8-links-into-cells ()
  (let ((buffer (nlt:parse-styled-string
                 (list (concatenate 'string +osc8-open+ "ab" +osc8-close+ "cd")) 10 1)))
    (is (string= "https://example.com" (nlt:cell-link (nlt:screen-buffer-ref buffer 0 0))))
    (is (string= "https://example.com" (nlt:cell-link (nlt:screen-buffer-ref buffer 1 0))))
    (is (null (nlt:cell-link (nlt:screen-buffer-ref buffer 2 0))))))

(deftest parser-stores-sgr-as-parameter-strings ()
  (let ((buffer (nlt:parse-styled-string
                 (list (format nil "~C[31ma~C[38;2;1;2;3mb~C[48;5;7mc~C[1;4md~C[0me"
                               #\Escape #\Escape #\Escape #\Escape #\Escape))
                 10 1)))
    (is (equal "31" (nlt:cell-fg (nlt:screen-buffer-ref buffer 0 0))) "a basic colour is its number")
    (is (equal "38;2;1;2;3" (nlt:cell-fg (nlt:screen-buffer-ref buffer 1 0))) "truecolor is the whole run")
    (is (equal "48;5;7" (nlt:cell-bg (nlt:screen-buffer-ref buffer 2 0))) "a 256-colour bg too")
    (is (equal "38;2;1;2;3" (nlt:cell-fg (nlt:screen-buffer-ref buffer 2 0))) "the fg persists")
    (is (= (logior nlt:+attr-bold+ nlt:+attr-underline+)
           (nlt:cell-attrs (nlt:screen-buffer-ref buffer 3 0))))
    (let ((reset (nlt:screen-buffer-ref buffer 4 0)))
      (is (and (null reset.fg) (null reset.bg) (zerop reset.attrs))))))

(deftest parser-keeps-every-extended-colour-apart ()
  ;; The parameter-text table is keyed by the colour: a key that read each
  ;; channel's high byte (always 0) handed the first colour a frame parsed to
  ;; every colour after it, and the whole shell painted in one tone.
  (let ((buffer (nlt:parse-styled-string
                 (list (format nil "~C[38;2;252;168;100ma~C[38;2;231;229;228mb~C[38;5;7mc~C[38;5;208md~C[48;2;12;10;9me~C[48;2;20;17;16mf"
                               #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape))
                 10 1)))
    (flet ((fg (x) (nlt:cell-fg (nlt:screen-buffer-ref buffer x 0)))
           (bg (x) (nlt:cell-bg (nlt:screen-buffer-ref buffer x 0))))
      (is (equal "38;2;252;168;100" (fg 0)) "the first truecolor run is its own")
      (is (equal "38;2;231;229;228" (fg 1)) "a second truecolor run is not the first's")
      (is (equal "38;5;7" (fg 2)) "a 256-colour run is its own")
      (is (equal "38;5;208" (fg 3)) "and a second one too")
      (is (equal "48;2;12;10;9" (bg 4)) "a truecolor bg is its own")
      (is (equal "48;2;20;17;16" (bg 5)) "and a second bg is not the first's"))))

(deftest parser-lays-glyphs-out-by-their-width ()
  (let ((buffer (nlt:parse-styled-string (list (format nil "a~C日b~Cc" #\Tab (code-char #x200B)))
                                         10 1)))
    (is (string= "a 日bc" (string-right-trim " " (painted-row buffer 0))))
    (is (= 2 (nlt:cell-width (nlt:screen-buffer-ref buffer 2 0))) "the ideograph takes two columns")
    (is (= 0 (nlt:cell-width (nlt:screen-buffer-ref buffer 3 0))) "its trailing cell is width 0")
    (is (char= #\b (nlt:cell-char (nlt:screen-buffer-ref buffer 4 0))) "the next glyph lands after it")))

(deftest parser-wraps-an-overwide-row-and-lays-each-row-on-its-own ()
  (let ((buffer (nlt:parse-styled-string (list "abcdef" "xy") 4 3)))
    (is (string= "abcd" (painted-row buffer 0)) "the first four columns")
    (is (string= "ef  " (painted-row buffer 1)) "the overflow wraps to the next screen row")
    (is (string= "xy  " (painted-row buffer 2)) "and the next row starts on the row after")))

(deftest paint-rows-lays-rows-at-a-position-over-what-is-there ()
  (let ((buffer (nlt:parse-styled-string (list "abcdef" "ghijkl") 6 3)))
    (nlt:paint-rows buffer (list "XY" "Z") :x 2 :y 1)
    (is (string= "abcdef" (painted-row buffer 0)) "a row above the position is left alone")
    (is (string= "ghXYkl" (painted-row buffer 1)) "the first row lands at the column, the rest of its row stays")
    (is (string= "  Z   " (painted-row buffer 2)) "the next row lands below it, at the same column")))

(deftest paint-rows-cuts-a-row-at-its-width-and-wraps-only-when-asked ()
  (let ((buffer (nlt:parse-styled-string (list "abcdef" "ghijkl") 6 2)))
    (nlt:paint-rows buffer (list "XYZW") :x 1 :width 2)
    (is (string= "aXYdef" (painted-row buffer 0)) "cut at two columns")
    (is (string= "ghijkl" (painted-row buffer 1)) "nothing spills onto the row below"))
  (let ((buffer (nlt:parse-styled-string (list "abcdef" "ghijkl") 6 2)))
    (nlt:paint-rows buffer (list "XYZW") :x 1 :width 2 :wrap t)
    (is (string= "aXYdef" (painted-row buffer 0)))
    (is (string= "gZWjkl" (painted-row buffer 1)) "with WRAP the rest goes on at the same column below")))

(deftest paint-rows-carries-style-across-rows-and-links-into-cells ()
  (let ((buffer (nlt:parse-styled-string (list "" "") 4 2)))
    (nlt:paint-rows buffer (list (format nil "~C[31mA" #\Escape) "B") :x 1)
    (is (equal "31" (nlt:cell-fg (nlt:screen-buffer-ref buffer 1 1))) "the style carries to the next row")
    (nlt:paint-rows buffer (list (concatenate 'string +osc8-open+ "L" +osc8-close+ "M")) :x 2)
    (is (string= "https://example.com" (nlt:cell-link (nlt:screen-buffer-ref buffer 2 0))))
    (is (null (nlt:cell-link (nlt:screen-buffer-ref buffer 3 0))) "the link closes where it closed")))

(deftest paint-rows-keeps-the-background-under-a-cell-that-sets-none ()
  (let ((buffer (nlt:parse-styled-string (list (format nil "~C[44mabcd" #\Escape)) 4 1)))
    (nlt:paint-rows buffer (list (format nil "~C[31m▄~C[42m▀" #\Escape #\Escape)) :x 1)
    (is (equal "44" (nlt:cell-bg (nlt:screen-buffer-ref buffer 1 0))) "no background of its own: the one under it shows")
    (is (equal "31" (nlt:cell-fg (nlt:screen-buffer-ref buffer 1 0))) "while the glyph and its colour are the painted ones")
    (is (equal "42" (nlt:cell-bg (nlt:screen-buffer-ref buffer 2 0))) "a background of its own replaces it")))

(deftest paint-rows-blanks-a-wide-glyph-it-cuts-in-half ()
  ;; a, the ideograph over columns 1-2, b.
  (let ((buffer (nlt:parse-styled-string (list "a日b") 4 1)))
    (nlt:paint-rows buffer (list "X") :x 2)
    (is (string= "a Xb" (painted-row buffer 0)) "painting its right half blanks its left half")
    (is (= 1 (nlt:cell-width (nlt:screen-buffer-ref buffer 1 0))) "which paints one column again"))
  (let ((buffer (nlt:parse-styled-string (list "a日b") 4 1)))
    (nlt:paint-rows buffer (list "Y") :x 1)
    (is (string= "aY b" (painted-row buffer 0)) "painting its left half blanks its right half")
    (is (= 1 (nlt:cell-width (nlt:screen-buffer-ref buffer 2 0)))))
  (let ((buffer (nlt:parse-styled-string (list "abcd") 4 1)))
    (nlt:paint-rows buffer (list "日") :x 3)
    (is (string= "abcd" (painted-row buffer 0)) "a wide glyph with one column left before the edge is cut whole"))
  (let* ((before (nlt:parse-styled-string (list "a日b") 4 1))
         (after (nlt:paint-rows (nlt:parse-styled-string (list "a日b") 4 1) (list "X") :x 2))
         (written (with-output-to-string (out) (nlt:render-diff before after out))))
    (is (search " X" written) "the painter writes the blank and the glyph beside it")))

(deftest painter-writes-only-the-changed-cells ()
  (let* ((before (nlt:parse-styled-string (list "abcd") 4 1))
         (after (nlt:parse-styled-string (list "abXd") 4 1))
         (written (with-output-to-string (out) (nlt:render-diff before after out))))
    (is (search (csi "2C") written) "the cursor moves two columns right")
    (is (search "X" written) "and the changed cell is written")
    (is (null (search "a" written)) "the unchanged cells are not")))

(deftest painter-opens-and-closes-links-and-resets-the-style ()
  (let ((linked-red (concatenate 'string +osc8-open+ (format nil "~C[31mab" #\Escape) +osc8-close+)))
    ;; The link and the style end at cell 2, where the blanks begin.
    (let ((written (with-output-to-string (out)
                     (nlt:render-diff nil (nlt:parse-styled-string (list linked-red) 4 1) out))))
      (is-table (in) (search in written) ("]8;;https://example.com" "the link opens")
        (+osc8-close+ "and closes") ((csi "0;31m") "the style is set once"))
      (is (< (search (csi "0m") written) (search +osc8-close+ written))))
    ;; Linked, styled cells at the frame's end: the painter resets and
    ;; closes after the last cell.
    (let* ((written (with-output-to-string (out)
                      (nlt:render-diff nil (nlt:parse-styled-string (list linked-red) 2 1) out)))
           (tail (concatenate 'string (csi "0m") +osc8-close+)))
      (is (string= tail (subseq written (- (length written) (length tail))))))))

(deftest painter-paints-a-full-frame-from-nothing ()
  (let* ((buffer (nlt:parse-styled-string (list "ab" "cd") 2 2))
         (written (with-output-to-string (out) (nlt:render-diff nil buffer out))))
    (is-table (in) (search in written) ("ab" "the first row")
      ((csi "2;1H") "an absolute move to the second row") ("cd" "the second row"))))
