;;;; indentation-test.lisp --- shared text columns and continuation lanes.
(in-package #:nodecode.test)

(deftest indentation-picker-descriptions-use-one-display-column ()
  (let* ((nle::*color-profile* :none)
         (dialog (nle::make-picker-dialog :rows (list
                    (nle::make-picker-row :label "短" :detail "first description")
                    (nle::make-picker-row :label "/mutate" :detail "second description")
                    (nle::make-picker-row :label "/eval" :detail "third description"))))
         (rows (nle::docked-dialog-list-rows dialog 64 nil 4))
         (columns (loop for row in rows for needle in '("first" "second" "third")
                        collect (nle::text-display-width (subseq row 0 (search needle row))))))
    (is (apply #'= columns) "Unicode labels and ASCII labels share the same detail lane")
    (is (every (lambda (row) (= 64 (nle::text-display-width row))) rows))
    (is (= 4 (length rows)) "padding and viewport row ownership stay unchanged")))

(deftest indentation-plain-notices-preserve-list-and-explicit-leading-indent ()
  (let ((nle::*color-profile* :none))
    (dolist (text '("  - Keep every continuation under the item text while wrapping" "    Keep the explicit leading indentation when this sentence wraps" "· Keep the guide continuation under its bullet text while wrapping"))
      (let* ((line (first (nle::system-notice-llines text nil)))
             (rows (nth-value 1 (nle::lline-rows line 32)))
             (hang (cond ((search "- " text) 4) ((search "· " text) 2) (t 4))))
        (is (> (length rows) 1))
        (is (= (+ 3 hang) (position-if-not (lambda (ch) (char= ch #\Space)) (second rows))) "the label lane holds a notice's words")
        (is (every (lambda (row) (<= (nle::text-display-width row) 32)) rows))))))

(deftest indentation-markdown-lists-retain-nesting-and-paragraph-breaks ()
  (let* ((nle::*color-profile* :none)
         (lines (nle::markdown-llines (format nil "- First item has **several words** that need wrapping~%  10. Nested item has another sentence that needs wrapping~%~%A separate paragraph.") :width 35))
         (rows (nth-value 1 (nle::llines-rows lines 35))))
    ;; A bullet is a dot; a number stands right-aligned a column out, its
    ;; words where the nesting puts them.
    (dolist (n '("   • First item" "    10. Nested item")) (is (rows-carry-p n rows) n))
    (is (rows-carry-p "        sentence" rows) "a nested item's wrap hangs under its words")
    (is (member "   " rows :test #'equal) "the paragraph separator is retained")
    (is (equal '(2 6) (mapcar #'nle::lline-hang (subseq lines 0 2))))))

(deftest indentation-setup-summary-wraps-values-without-losing-their-column ()
  ;; The walk's last page keeps the columns its transcript block had: a value
  ;; wider than its column goes on under itself, never under the labels.
  (dolist (width '(26 40 72))
    (let ((lines (mapcar #'nle::segs-text
                         (nle::setup-line-rows
                          (nle::setup-ready-lines
                           (nle::make-setup-run :providers '("fixture") :model '("fixture" "model")
                                                :notes '("A long explanation keeps its words and wraps under the value"))
                           nil)
                          width))))
      (is (every (lambda (line) (<= (nle::text-display-width line) width)) lines))
      (is (every (lambda (line) (and (>= (length line) 14) (char= #\Space (char line 13)))) lines)))))

(deftest indentation-narrow-picker-keeps-detail-visible-and-selection-stable ()
  (let* ((nle::*color-profile* :none)
         (rows (list (nle::make-picker-row :label "A very long descriptive label" :detail "FIRST detail" :value :first)
                     (nle::make-picker-row :label "Short" :detail "SECOND detail" :value :second)))
         (dialog (nle::make-picker-dialog :rows rows :selection 1)))
    (dolist (width '(32 40 80))
      (let ((plain (nle::docked-dialog-list-rows dialog width nil 2)))
        (dolist (needle '("FIRST" "SECOND")) (is (rows-carry-p needle plain) needle))
        (is (every (lambda (row) (= width (nle::text-display-width row))) plain))
        (is-shape dialog (.selection 1) (nle::picker-highlighted-value eq :second))))))

(deftest indentation-command-labels-leave-readable-description-room ()
  (let* ((nle::*color-profile* :none)
         (dialog (nle::make-picker-dialog :rows (list
                    (nle::make-picker-row :label "/help" :detail "Show commands and shortcuts")
                    (nle::make-picker-row :label "/a-command-with-several-optional-arguments [name | default]" :detail "Second description"))))
         (rows (nle::docked-dialog-list-rows dialog 70 nil 2)))
    (dolist (n '("Show commands and shortcuts" "Second description")) (is (rows-carry-p n rows) n))
    (is (every (lambda (row) (= 70 (nle::text-display-width row))) rows))))

(deftest indentation-setup-table-resize-hangs-under-the-value ()
  (let* ((nle::*color-profile* :none)
         (line (first (nle::system-notice-llines "    model       fixture/a-long-model-name-that-wraps" nil)))
         (rows (nth-value 1 (nle::lline-rows line 32))))
    (is (> (length rows) 1))
    (is (= 16 (nle::lline-hang line)))
    (is (= 19 (position-if-not (lambda (ch) (char= ch #\Space)) (second rows))))))

(deftest indentation-timestamp-keeps-the-whole-markdown-list-in-its-lane ()
  (let* ((nle::*color-profile* :none)
         (body (nle::markdown-llines (format nil "- First item needs enough words to wrap~%  - Nested item also wraps across the narrow lane~%- Second parent item") :width 32))
         (stamped (nle::stamp-transcript-llines body "10:22 PM" 80)))
    (is (equal "10:22 PM" (nle::seg-text (nle::lline-aside (first stamped)))) "the clock rides the first item")
    (is (null (nle::lline-aside (first body))) "the cached line is never written")
    (is (equal (mapcar #'nle::lline-text body) (mapcar #'nle::lline-text stamped)))
    (is (equal (mapcar #'nle::lline-hang body) (mapcar #'nle::lline-hang stamped)))
    (is (equal (mapcar #'nle::lline-role body) (mapcar #'nle::lline-role stamped)) "the list stays where it stands")))
