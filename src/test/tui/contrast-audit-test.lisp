;;;; contrast-audit-test.lisp --- optional developer diagnostics.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(defun audit-sgr (params text)
  "One explicitly styled test run."
  (format nil "~c[~am~a~c[0m" #\Escape params text #\Escape))

(deftest contrast-audit-truecolor-threshold-and-coordinates ()
  (let* ((rows (list (concatenate 'string
                      (audit-sgr "38;2;255;255;255;48;2;0;0;0" "A")
                      (audit-sgr "38;2;110;110;110;48;2;100;100;100" "B"))))
         (result (nodecode.contrast-audit:audit-rows rows 2)))
    (is (= 2 (getf result :checked)))
    (is (= 1 (getf result :passed)))
    (is (= 1 (getf result :failed)))
    (is (= 1 (getf (first (getf result :issues)) :x)))
    (is (= 0 (getf (first (getf result :issues)) :y)))
    (is (= 0 (getf (nodecode.contrast-audit:audit-rows rows 2 :threshold 1) :failed)))
    (is (search "1 failed" (nodecode.contrast-audit:format-report result)))))

(deftest contrast-audit-unknown-colors-are-not-guessed ()
  (dolist (params '("38;2;255;255;255" "31;48;2;0;0;0" "38;5;1;48;5;0"))
    (let ((result (nodecode.contrast-audit:audit-rows (list (audit-sgr params "X")) 1)))
      (is (= 0 (getf result :checked)))
      (is (= 1 (getf result :skipped)))
      (is (eq :unknown-color (getf (first (getf result :issues)) :reason))))))

(deftest contrast-audit-explicit-defaults-and-system-palette ()
  (let ((palette (make-list 16 :initial-element '(0 0 0))))
    (setf (nth 1 palette) '(255 255 255))
    (dolist (params '("31;40" "38;5;1;48;5;0"))
      (is (= 1 (getf (nodecode.contrast-audit:audit-rows
                       (list (audit-sgr params "X")) 1 :ansi-palette palette) :passed))))
    (is (= 1 (getf (nodecode.contrast-audit:audit-rows '("X") 1
                     :foreground '(255 255 255) :background '(0 0 0)) :passed)))))

(deftest contrast-audit-256-and-reverse ()
  (let* ((rows (list (audit-sgr "7;38;5;232;48;5;255" "X")))
         (buffer (nlt:parse-styled-string rows 1 1))
         (cell (nlt:screen-buffer-ref buffer 0 0))
         (before (list (nlt:cell-fg cell) (nlt:cell-bg cell) (nlt:cell-attrs cell)))
         (result (nodecode.contrast-audit:audit-buffer buffer :threshold 21))
         (issue (first (getf result :issues))))
    (is (equal '(238 238 238) (getf issue :fg)))
    (is (equal '(8 8 8) (getf issue :bg)))
    (is (equal before (list (nlt:cell-fg cell) (nlt:cell-bg cell) (nlt:cell-attrs cell))))))

(deftest contrast-audit-faint-needs-explicit-model ()
  (let ((rows (list (audit-sgr "2;38;2;255;255;255;48;2;0;0;0" "X"))))
    (let ((result (nodecode.contrast-audit:audit-rows rows 1)))
      (is (= 1 (getf result :skipped)))
      (is (eq :terminal-faint (getf (first (getf result :issues)) :reason))))
    (let* ((result (nodecode.contrast-audit:audit-rows rows 1 :faint-factor 0.1))
           (issue (first (getf result :issues))))
      (is (= 1 (getf result :failed)))
      (is (equal '(26 26 26) (getf issue :fg)))
      (is (search "not a measured terminal behavior" (nodecode.contrast-audit:format-report result))))))

(deftest contrast-audit-decoration-and-wide-glyph-boundaries ()
  (let* ((rows (list (audit-sgr "38;2;100;100;100;48;2;100;100;100" "界 ─")))
         (result (nodecode.contrast-audit:audit-rows rows 4
                   :exclude (lambda (cell x y) (declare (ignore x y)) (char= #\─ (nlt:cell-char cell))))))
    (is (= 1 (getf result :checked)))
    (is (= 1 (getf result :failed)))
    (is (= 3 (getf result :ignored)))))

(deftest contrast-audit-validates-input ()
  (dolist (options '((:threshold 0) (:threshold 22) (:foreground (1 2))
                    (:background (1 2 300)) (:ansi-palette ((0 0 0))) (:faint-factor 2)))
    (is (handler-case (progn (apply #'nodecode.contrast-audit:audit-rows '("X") 1 options) nil)
          (error () t)))))

(deftest contrast-audit-composed-renderer-integration ()
  ;; Exercise real emitted colors, not merely synthetic SGR or palette values.
  (dolist (name (nle:theme-catalog :user nil))
    (let ((nle::*palette* (theme-palette name)))
      (dolist (nle::*color-profile* '(:truecolor :ansi256))
        (dolist (ground '(:title-band :slab :composer :user-block))
          (let ((fill (nle::visual-style-bg (nle:style-for-tone ground))))
            (dolist (tone '(:foreground :muted :thought-strong :code-quiet :inline-code :link))
              (let* ((paint (nle::seg-render (nle::make-seg :text "X" :tone tone)
                             fill :force-bg t :shadow t))
                     (report (nodecode.contrast-audit:audit-rows (list paint) 1
                               :background (nle::canvas-rgb))))
                (is (= 1 (getf report :checked)) (format nil "~a ~a ~a ~a" name nle::*color-profile* ground tone))
                (is (= 1 (getf report :passed)))
                (is (zerop (getf report :skipped)))))))))))

(deftest contrast-audit-inherits-final-layer-background ()
  ;; An overlay with no explicit background inherits its painted underlay,
  ;; not the terminal default supplied to the audit.
  (let ((buffer (nlt:parse-styled-string
                  (list (audit-sgr "48;2;100;100;100" " ")) 1 1)))
    (nlt:paint-rows buffer (list (audit-sgr "38;2;110;110;110" "X")))
    (let* ((report (nodecode.contrast-audit:audit-buffer buffer :background '(0 0 0)))
           (issue (first (getf report :issues))))
      (is (= 1 (getf report :failed)))
      (is (equal '(100 100 100) (getf issue :bg)))
      (is (< (getf issue :ratio) 1.2d0)))))

(deftest contrast-audit-bold-system-colors-remain-uncertain ()
  (let* ((palette (make-list 16 :initial-element '(0 0 0)))
         (report (nodecode.contrast-audit:audit-rows
                   (list (audit-sgr "1;37;40" "X")) 1 :ansi-palette palette)))
    (is (zerop (getf report :checked)))
    (is (= 1 (getf report :skipped)))
    (is (eq :terminal-bold (getf (first (getf report :issues)) :reason)))))
