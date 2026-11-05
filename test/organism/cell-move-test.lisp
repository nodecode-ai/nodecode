;;;; cell-move-test.lisp --- what the layer already keeps, moved into a made cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: MOVE-TO-CELL takes every layer file of the names it
;;;; is given into a made cell, making the cell when there is none, in the
;;;; layer's load order; both folders' .asd and git agree with the move;
;;;; nothing live changes; a later redefinition from any session lands in the
;;;; cell; a file that also defines a name left out stays whole and is named;
;;;; a name no layer file defines is named; and a folder of that name the
;;;; organism did not make is refused before anything moves.

(in-package #:nodecode.test)

(defun move-test-output (thunk)
  "=> (values WHAT-THUNK-RETURNED WHAT-IT-PRINTED), under session s-cell."
  (let* ((result nil)
         (printed (with-output-to-string (*standard-output*)
                    (let ((nlk:*scribe-session-id* "s-cell"))
                      (setf result (funcall thunk))))))
    (values result printed)))

(deftest cell-move-takes-named-layer-files-in-load-order (with-made-cell-fixture ())
  (keep-definition-text "(defstruct mv-reading celsius)" :session "s-build")
  (keep-definition-text "(defparameter *mv-city* \"Tokyo\")" :session "s-build")
  (keep-definition-text "(defun mv-forecast () (mv-reading-celsius (make-mv-reading :celsius 21)))" :session "s-build")
  (keep-definition-text "(defun mv-unrelated () :stays)" :session "s-build")
  (let (moved printed)
    (multiple-value-setq (moved printed)
      (move-test-output (lambda ()
                          (nlk:move-to-cell "weather" (find-symbol "MV-FORECAST" '#:nodecode.evolved)
                                            "mv-reading" (find-symbol "*MV-CITY*" '#:nodecode.evolved)))))
    (is (equal '("mv-reading" "*mv-city*" "mv-forecast") moved) "the labels moved, in the layer's load order")
    (is (search "[layer: moved mv-reading, *mv-city*, mv-forecast to tester-weather" printed)))
  (dolist (name '("mv-reading" "_mv-city_" "mv-forecast"))
    (is (null (layer-file name)))
    (is (cell-file "weather" name)))
  (is (layer-file "mv-unrelated") "what was not named stays")
  (let ((asd (nlk:read-text (merge-pathnames "tester-weather/tester-weather.asd" nlk::*cells-directory*))))
    (is (< (search "\"mv-reading\"" asd) (search "\"_mv-city_\"" asd) (search "\"mv-forecast\"" asd)) "the struct still loads before its users"))
  (is (null (search "mv-forecast" (nlk:read-text (nlk::layer-system-file)))) "the layer's .asd lists it no more")
  (is (string= "" (knowledge-git "tester" "status" "--porcelain")) "the layer committed the move")
  (is (string= "" (knowledge-git "tester-weather" "status" "--porcelain")) "and so did the cell")
  (is (search "moved mv-reading" (knowledge-git "tester" "log" "--format=%s" "-1")))
  (is (eql 21 (funcall (find-symbol "MV-FORECAST" '#:nodecode.evolved))) "nothing live changed")
  ;; a later redefinition, from any session, is the cell file's to edit: never a layer copy
  (is (search "mv-forecast.lisp holds it, so edit it there"
              (keep-definition-text "(defun mv-forecast () (mv-reading-celsius (make-mv-reading :celsius 22)))"
                                    :session "s-later")))
  (is (null (layer-file "mv-forecast"))))

(deftest cell-move-leaves-a-shared-file-whole-and-names-what-it-did-not-find (with-made-cell-fixture ())
  (keep-definition-text "(defun mv-alone () 0)" :session "s-build")
  (let ((shared (merge-pathnames "mv-shared.lisp" (nlk:layer-directory))))
    (nlk:write-file-atomically shared "(in-package :nodecode.evolved)
(defun mv-half-a () 1)
(defun mv-half-b () 2)
")
    (nlk::write-layer-system (nlk::layer-system-file) (nlk::layer-name) "mv-shared")
    (multiple-value-bind (moved printed)
        (move-test-output (lambda () (nlk:move-to-cell "halves" "mv-half-a" "mv-alone" "mv-nowhere")))
      (is (equal '("mv-alone") moved))
      (is (search "[layer: mv-shared.lisp defines more than you named; left whole in the layer]" printed))
      (is (search "[layer: no layer file defines mv-nowhere]" printed)))
    (is (probe-file shared) "a hand file is never split")
    (is (null (cell-file "halves" "mv-shared")))))

(deftest cell-move-refuses-a-folder-it-did-not-make (with-made-cell-fixture ())
  (keep-definition-text "(defun mv-kept () 0)" :session "s-build")
  (ensure-directories-exist (merge-pathnames "tester-handmade/notes.txt" nlk::*cells-directory*))
  (is (search "did not make"
              (handler-case (progn (move-test-output (lambda () (nlk:move-to-cell "handmade" "mv-kept"))) "")
                (error (condition) (princ-to-string condition)))))
  (is (layer-file "mv-kept") "nothing moved"))
