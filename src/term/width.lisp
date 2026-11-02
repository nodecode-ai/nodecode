;;;; width.lisp --- how many columns a codepoint paints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's style.lisp width table (see NOTICE).
;;;;
;;;; One table for measure and paint: the cell parser lays a row out by it,
;;;; and theme's CHAR-DISPLAY-WIDTH (which pictures controls first) measures
;;;; by it, so a row can never be measured wider or narrower than it paints.
;;;; The table is derived from SBCL's bundled Unicode database, so widths
;;;; track the Unicode version the host Lisp ships rather than a
;;;; hand-maintained range list.

(in-package #:nodecode.term)

(defparameter *codepoint-widths*
  (let ((table (make-array char-code-limit :element-type '(unsigned-byte 2)
                                           :initial-element 1)))
    ;; Zero for nonspacing and enclosing marks and format controls (VS16,
    ;; ZWJ, bidi marks); two for East Asian Wide and Fullwidth, which
    ;; covers every emoji-presentation codepoint.
    (loop for code from #x0300 below char-code-limit
          for ch = (code-char code)
          when ch
            do (case (sb-unicode:general-category ch)
                 ((:mn :me :cf) (setf (aref table code) 0))
                 (t (when (member (sb-unicode:east-asian-width ch) '(:w :f))
                      (setf (aref table code) 2)))))
    ;; wcwidth conventions the raw properties miss.
    (fill table 0 :end 32)
    (setf (aref table #x09) 1)                     ; tab paints as one space
    (setf (aref table #x7F) 0)
    (setf (aref table #xAD) 1)                     ; soft hyphen paints
    (fill table 0 :start #x1160 :end #x1200)       ; Hangul jamo medial/final
                                                   ; join the leading jamo
    table)
  "Display width (0, 1, or 2) for every codepoint, indexed by char-code.")

(declaim (type (simple-array (unsigned-byte 2) (*)) *codepoint-widths*))

(declaim (inline codepoint-width))
(defun codepoint-width (ch)
  "Columns CH paints: 0, 1, or 2. A C0 control paints nothing here (tab
paints one space); the shell pictures controls before they reach a row."
  (aref *codepoint-widths* (char-code ch)))
