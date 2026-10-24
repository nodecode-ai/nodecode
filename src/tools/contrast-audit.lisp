;;;; contrast-audit.lisp --- opt-in diagnostics; no runtime hooks or mutation.
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode.contrast-audit
  (:use #:common-lisp)
  (:export #:audit-buffer #:audit-rows #:format-report))

(in-package #:nodecode.contrast-audit)

(defun rgb-p (value)
  "Whether VALUE is a three-channel 8-bit RGB list."
  (and (listp value) (= 3 (length value))
       (every (lambda (channel) (typep channel '(integer 0 255))) value)))

(defun sgr-rgb (sgr default ansi-palette)
  "Resolve a parsed cell SGR without assuming terminal-defined system colors."
  (if (null sgr) default
      (let* ((parts (mapcar #'parse-integer (uiop:split-string sgr :separator ";")))
             (code (first parts))
             (index (cond ((and (member code '(38 48)) (= (second parts) 5)) (third parts))
                          ((<= 30 code 37) (- code 30))
                          ((<= 40 code 47) (- code 40))
                          ((<= 90 code 97) (+ 8 (- code 90)))
                          ((<= 100 code 107) (+ 8 (- code 100))))))
        (cond ((and (member code '(38 48)) (= (second parts) 2)) (cddr parts))
              ((and index (< index 16)) (and ansi-palette (elt ansi-palette index)))
              (index (nodecode.evolved::ansi256-rgb index))))))

(defun system-color-p (sgr)
  "Whether SGR references one of the terminal-defined 16 system colors."
  (when sgr
    (let* ((parts (mapcar #'parse-integer (uiop:split-string sgr :separator ";")))
           (code (first parts)))
      (or (<= 30 code 37) (<= 40 code 47) (<= 90 code 97) (<= 100 code 107)
          (and (member code '(38 48)) (= (second parts) 5) (< (third parts) 16))))))

(defun cell-result (cell x y foreground background ansi-palette faint-factor exclude)
  "Classify one cell; faint is terminal-dependent unless explicitly modeled."
  (let ((result (list :x x :y y :char (nodecode.term:cell-char cell))))
    (flet ((finish (status &optional reason)
             (append result (list :status status :reason reason))))
      (cond ((or (zerop (nodecode.term:cell-width cell))
                 (find (nodecode.term:cell-char cell) '(#\Space #\Tab #\Newline #\Return)))
             (finish :ignored :blank-or-continuation))
            ((and exclude (funcall exclude cell x y)) (finish :ignored :excluded))
            (t
             (let ((fg (sgr-rgb (nodecode.term:cell-fg cell) foreground ansi-palette))
                   (bg (sgr-rgb (nodecode.term:cell-bg cell) background ansi-palette))
                   (attrs (nodecode.term:cell-attrs cell)))
               (when (logtest nodecode.term::+attr-reverse+ attrs) (rotatef fg bg))
               (cond ((not (and (rgb-p fg) (rgb-p bg))) (finish :skipped :unknown-color))
                     ((and (logtest nodecode.term::+attr-bold+ attrs)
                           (or (system-color-p (nodecode.term:cell-fg cell))
                               (system-color-p (nodecode.term:cell-bg cell))))
                      (finish :skipped :terminal-bold))
                     ((and (logtest nodecode.term::+attr-faint+ attrs) (null faint-factor))
                      (finish :skipped :terminal-faint))
                     (t
                      (when (logtest nodecode.term::+attr-faint+ attrs)
                        (setf fg (mapcar (lambda (ink ground)
                                          (round (+ ground (* faint-factor (- ink ground))))) fg bg)))
                      (append result (list :status :measured :fg fg :bg bg
                                           :ratio (nodecode.evolved::contrast-ratio fg bg)))))))))))

(defun audit-buffer (buffer &key (threshold 4.5d0) foreground background ansi-palette
                                  faint-factor exclude)
  "Audit an already composed screen BUFFER without changing it.
Defaults are explicit RGB lists; NIL means unknown. ANSI-PALETTE supplies the
terminal's actual 16 system colors. FAINT-FACTOR, if supplied, models SGR 2 as
an sRGB interpolation toward the background (0..1); no universal factor exists.
EXCLUDE is a (CELL X Y) predicate for known non-text decorations. Coordinates
are zero-based. Return counts and failure/skip details; blanks are ignored."
  (unless (and (realp threshold) (<= 1 threshold 21))
    (error "Contrast threshold must be between 1 and 21."))
  (dolist (rgb (list foreground background))
    (unless (or (null rgb) (rgb-p rgb)) (error "Expected an RGB list or NIL: ~s" rgb)))
  (when (and ansi-palette (not (and (= (length ansi-palette) 16) (every #'rgb-p ansi-palette))))
    (error "ANSI-PALETTE must contain exactly 16 RGB lists."))
  (unless (or (null faint-factor) (and (realp faint-factor) (<= 0 faint-factor 1)))
    (error "FAINT-FACTOR must be NIL or between 0 and 1."))
  (let ((checked 0) (passed 0) (failed 0) (skipped 0) (ignored 0) (issues nil))
    (dotimes (y (nodecode.term:screen-buffer-height buffer))
      (dotimes (x (nodecode.term:screen-buffer-width buffer))
        (let* ((cell (nodecode.term:screen-buffer-ref buffer x y))
               (result (cell-result cell x y foreground background ansi-palette faint-factor exclude)))
          (case (getf result :status)
            (:measured
             (incf checked)
             (if (>= (getf result :ratio) threshold) (incf passed)
                 (progn (incf failed) (setf (getf result :status) :failed) (push result issues))))
            (:skipped (incf skipped) (push result issues))
            (:ignored (incf ignored))))))
    (list :threshold threshold :checked checked :passed passed :failed failed :skipped skipped
          :ignored ignored :faint-factor faint-factor :issues (nreverse issues))))

(defun audit-rows (rows width &rest options &key &allow-other-keys)
  "Parse complete rendered frame ROWS at WIDTH, then audit their final cells.
Not a terminal emulator or a parser for arbitrary cursor-motion captures."
  (apply #'audit-buffer (nodecode.term:parse-styled-string rows width (length rows)) options))

(defun format-report (report)
  "Render REPORT as plain developer-facing text, with no terminal controls."
  (with-output-to-string (out)
    (format out "Contrast >= ~,2f:1: ~d checked, ~d passed, ~d failed, ~d skipped, ~d ignored~%"
            (getf report :threshold) (getf report :checked) (getf report :passed)
            (getf report :failed) (getf report :skipped) (getf report :ignored))
    (when (getf report :faint-factor)
      (format out "SGR 2 modeled using faint factor ~a (not a measured terminal behavior).~%"
              (getf report :faint-factor)))
    (dolist (issue (getf report :issues))
      (format out "(~d,~d) ~a: ~a~%" (getf issue :x) (getf issue :y) (getf issue :status)
              (if (getf issue :ratio) (format nil "~,3f:1" (getf issue :ratio))
                  (getf issue :reason))))))
