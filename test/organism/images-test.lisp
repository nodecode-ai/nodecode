;;;; images-test.lisp --- saved heaps: where they live, the seal, the one-shot restore (images.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermetic: the home is a scratch directory of the test's own, a heap is a
;;;; file whose completeness is stubbed, and the exec is recorded, never made.
;;;; The dump itself -- a running organism frozen -- is proved end to end by
;;;; the packaged binary, outside this suite.

(in-package #:nodecode.test)

(defmacro with-image-home ((home) &body body)
  "BODY with the home a fresh scratch directory HOME, an image complete
whenever it has bytes in it, and this build labelled 0.1.8+aaaa."
  `(with-temp-trees ((,home "images-home"))
     (ensure-directories-exist (uiop:ensure-directory-pathname ,home))
     (with-saved-globals ((nlk:*home* (uiop:ensure-directory-pathname ,home)))
       (with-stubbed-fdefinitions ((nlk::image-complete-p (image)
                                    (and (probe-file image)
                                         (plusp (with-open-file (in image :element-type '(unsigned-byte 8))
                                                  (file-length in)))))
                                   (nle::effective-version () "0.1.8+aaaa"))
         ,@body))))

(defun save-fake-heap (name &key (build "0.1.8+aaaa") (bytes "heap"))
  "A heap NAME as a save leaves it: the dump written where the save writes,
then sealed under BUILD."
  (write-temp-file (nle::prepare-saved-image name) bytes)
  (nle::seal-saved-image name :build build :generation 3 :folder-key "k1"))

(deftest a-saved-image-name-is-a-plain-directory-name ()
  (dolist (name '("todo-demo" "v0.1.8_before" "A1"))
    (is (null (nle::image-name-refusal name)) name))
  (dolist (name (list "" ".hidden" "-x" "a/b" "../up" "ü" "restore" (make-string 65 :initial-element #\a)))
    (is (stringp (nle::image-name-refusal name)) name)))

(deftest a-sealed-heap-is-private-and-says-which-build-saved-it (with-image-home (home))
  (let ((manifest (save-fake-heap "before")))
    (is (probe-file (nle::saved-image-path "before")) "the heap stands where a restore looks")
    (is (not (probe-file (nle::saved-image-dump-path "before"))) "and the dump is gone")
    (is (= #o700 (file-mode (nle::images-directory))) "images/ is the owner's alone")
    (is (= #o700 (file-mode (nle::saved-image-directory "before"))))
    (is (= #o600 (file-mode (nle::saved-image-manifest-path "before"))) "the manifest too")
    (is (equal "0.1.8+aaaa" (nlk:json-value manifest :string "build")))
    (is (eql 3 (nlk:json-value manifest :integer "generation")))
    (is (eql 4 (nlk:json-value manifest :integer "bytes")))
    (is (equal "0.1.8+aaaa" (nlk:json-value (nle::saved-image-manifest "before") :string "build")) "what is read back is what was written")))

(deftest a-save-cut-short-replaces-nothing (with-image-home (home))
  (save-fake-heap "before" :bytes "good")
  (nle::prepare-saved-image "before")
  (is (handler-case (progn (nle::seal-saved-image "before") nil)
        (error (condition) (search "missing" (princ-to-string condition)))) "a dump that never landed is refused")
  (is (equal "good" (uiop:read-file-string (nle::saved-image-path "before"))) "the heap saved earlier still stands"))

(deftest a-restore-is-one-shot-and-boots-the-heap-in-place (with-image-home (home))
  (save-fake-heap "before")
  (is (search "next launch" (nle::restore-image "before" :restart nil)))
  (is (equal "before" (nle::restore-pointer)))
  (is (search "<- the next launch boots it" (nle::saved-images-text)) "the list says which")
  (is (= #o600 (file-mode (nle::restore-pointer-path))))
  (let ((execs '()))
    (with-stubbed-fdefinition (nlk::exec-program (path argv) (push (list path argv) execs) "stubbed")
      (nle::hand-over-to-saved-image '("nodecode" "serve"))
      (is (equal (list (list (nle::saved-image-path "before") '("nodecode" "serve"))) execs) "the heap is exec'd with the launch's own argv")
      (is (not (probe-file (nle::restore-pointer-path))) "the pointer is taken before the exec")
      (is (null (nle::hand-over-to-saved-image)) "so the launch after it boots the folder")
      (is (= 1 (length execs))))))

(deftest a-heap-of-another-build-is-refused-by-name (with-image-home (home))
  (save-fake-heap "old" :build "0.1.7+bbbb")
  (is-present (refusal (nle::saved-image-refusal "old")) "a refusal"
    (is (search "saved by 0.1.7+bbbb" refusal))
    (is (search "this home runs 0.1.8+aaaa" refusal))
    (is (search "nodecode update bbbb" refusal) "and the way to its build"))
  (is (handler-case (progn (nle::request-image-restore "old") nil) (error () t)) "restore leaves no pointer to it")
  (is (not (probe-file (nle::restore-pointer-path))))
  ;; a pointer left before an update moved the build is said once and taken away
  (write-temp-file (nle::restore-pointer-path) "old")
  (with-stubbed-fdefinition (nlk::exec-program (path argv) (declare (ignore path argv)) (error "no exec"))
    (is (search "saved by 0.1.7" (nle::hand-over-to-saved-image '("nodecode"))))
    (is (not (probe-file (nle::restore-pointer-path))))))

(deftest deleting-a-heap-takes-its-pointer-with-it (with-image-home (home))
  (save-fake-heap "gone")
  (save-fake-heap "kept")
  (nle::request-image-restore "gone")
  (is (search "deleted" (nle::delete-saved-image "gone")))
  (is (not (probe-file (nle::saved-image-directory "gone"))))
  (is (null (nle::restore-pointer)) "a pointer to it goes with it")
  (is (equal '("kept") (nle::saved-image-names))))

(deftest no-home-entry-carries-the-saved-heaps ()
  (is-present (row (find "images/" nlk::+home-entries+ :key #'first :test #'equal)) "the row"
    (is (null (second row)) "no clone carries it")
    (is (null (fourth row)) "no archive ships it")))
