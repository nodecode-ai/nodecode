;;;; cell-drift-test.lisp --- what a made cell runs live that no file keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: CELL-DRIFT names a function, a variable, a method
;;;; and a hook that were only evaluated -- each live, each in no file -- and
;;;; stops naming each once the cell's file defines it, though what a write
;;;; made live from text carries no pathname either; and it names what a file
;;;; defines that the image lacks. Its labels are checked one by one: a
;;;; suite image holds unfiled definitions of other tests too.

(in-package #:nodecode.test)

(defun drift-fixture (name)
  "A throwaway made-cell folder NAME/ with cell.lisp (its package) and
NAME.lisp (one function, one variable, one generic), both loaded from disk.
=> (values DIRECTORY CODE-FILE PACKAGE)."
  (let* ((directory (uiop:ensure-directory-pathname
                     (format nil "/tmp/cell-drift-~d/~a/" (random 1000000) name)))
         (cell (merge-pathnames "cell.lisp" directory))
         (code (merge-pathnames (format nil "~a.lisp" name) directory)))
    (ensure-directories-exist directory)
    (nlk::write-file-atomically cell (format nil "(defpackage #:~a (:use #:cl))~%" name))
    (nlk::write-file-atomically
     code
     (format nil "(in-package #:~a)~%(defun kept-fn () 1)~%(defvar *kept-var* 2)~%(defgeneric kept-gf (x))~%(defmethod kept-gf ((x integer)) x)~%" name))
    (load cell)
    (load code)
    (values directory code (find-package (string-upcase name)))))

(defun drift-live-labels (drift)
  "The labels of DRIFT's :LIVE-ONLY."
  (mapcar #'second (getf drift :live-only)))

(defun drift-eval-in (package text)
  "Evaluate TEXT read in PACKAGE, as an eval in the cell would."
  (let ((*package* package)) (eval (read-from-string text))))

(defun drift-append (file text)
  "Append TEXT to FILE, as a write into the cell's file would."
  (with-open-file (stream file :direction :output :if-exists :append)
    (write-string text stream)))

(deftest cell-drift-names-an-eval-the-file-lacks ()
  (let ((name (format nil "drift-a-~d" (random 1000000))))
    (multiple-value-bind (directory code package) (drift-fixture name)
      (unwind-protect
           (progn
             (let ((labels (drift-live-labels (nlk:cell-drift directory))))
               (is (notany (lambda (l) (search "kept-" l)) labels) "what the cell's file defined and the image loaded from it is not drift"))
             (drift-eval-in package "(defun only-live () 3)")
             (drift-eval-in package "(defvar *only-live-var* 4)")
             (drift-eval-in package "(defmethod kept-gf ((x string)) x)")
             (let ((labels (drift-live-labels (nlk:cell-drift directory))))
               (is (member "defun only-live" labels :test #'string=) "an evaluated function in no file is named")
               (is (member "defvar *only-live-var*" labels :test #'string=) "an evaluated variable in no file is named")
               (is (find-if (lambda (l) (search "defmethod kept-gf (string)" l)) labels) "an evaluated method in no file is named, by its specializers")
               (is (not (find-if (lambda (l) (search "defmethod kept-gf (integer)" l)) labels)) "the method the file loaded is not"))
             (drift-append code (format nil "(defun only-live () 3)~%(defvar *only-live-var* 4)~%(defmethod kept-gf ((x string)) x)~%"))
             (let ((drift (nlk:cell-drift directory)))
               (is (notany (lambda (l) (or (search "only-live" l) (search "kept-gf" l))) (drift-live-labels drift)) "once the file defines them, though they were made live from text, nothing is named")
               (is (null (getf drift :not-live)) "and everything the file defines is live")))
        (ignore-errors (delete-package package))
        (uiop:delete-directory-tree (uiop:pathname-parent-directory-pathname directory) :validate t :if-does-not-exist :ignore)))))

(deftest cell-drift-names-an-evaluated-hook-and-a-file-ahead ()
  (let* ((name (format nil "drift-b-~d" (random 1000000)))
         (point (intern (string-upcase (format nil "~a-point" name)) :keyword)))
    (multiple-value-bind (directory code package) (drift-fixture name)
      (unwind-protect
           (progn
             (drift-eval-in package (format nil "(nodecode.kernel:define-hook ~s \"k\" (op next) (funcall next op))" point))
             (let ((labels (drift-live-labels (nlk:cell-drift directory))))
               (is (find-if (lambda (l) (and (search "define-hook" l) (search (string-downcase (symbol-name point)) l))) labels) "an evaluated hook in no file is named"))
             (drift-append code (format nil "(nodecode.kernel:define-hook ~s \"k\" (op next) (funcall next op))~%(defun ahead () 5)~%" point))
             (let ((drift (nlk:cell-drift directory)))
               (is (notany (lambda (l) (search (string-downcase (symbol-name point)) l)) (drift-live-labels drift)) "once the file defines the hook it is not named")
               (is (member "ahead" (getf drift :not-live) :test #'string-equal) "a function the file defines and the image lacks is named under :not-live")))
        (ignore-errors (nlk::unhook point "k"))
        (ignore-errors (delete-package package))
        (uiop:delete-directory-tree (uiop:pathname-parent-directory-pathname directory) :validate t :if-does-not-exist :ignore)))))
