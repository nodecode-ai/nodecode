;;;; cell-reach-test.lisp --- which of a made cell's definitions it still uses.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: CELL-REACH, on a folder shaped like the stress-tested
;;;; todo cell, names the leftover that still loads as dead and keeps a
;;;; variable only a helper reads; counts a symbol handed over quoted as a use;
;;;; takes a method on a kernel generic, written in EVOLVED, as a root; names a
;;;; reached method calling a function nothing defines, and an .asd component
;;;; with no file; and interns nothing in NODECODE.EVOLVED while it reads.

(in-package #:nodecode.test)

(defun write-reach-fixture (directory files components)
  "Write FILES, an alist (NAME . TEXT), into DIRECTORY as NAME.lisp, and an
.asd of the folder's name listing COMPONENTS in that order. => DIRECTORY."
  (let ((name (car (last (pathname-directory directory)))))
    (ensure-directories-exist directory)
    (loop for (file . text) in files
          do (nlk::write-file-atomically (merge-pathnames (format nil "~a.lisp" file) directory) text))
    (nlk::write-file-atomically
     (merge-pathnames (format nil "~a.asd" name) directory)
     (format nil "(defsystem ~s :class nodecode.kernel:layer :depends-on (\"nodecode\") :serial t~%  :components (~{(:file ~s)~^ ~}))~%"
             name components))
    directory))

(deftest cell-reach-names-what-the-cell-no-longer-uses ()
  (let ((directory (merge-pathnames "tester-mini/" (uiop:ensure-directory-pathname
                                                    (format nil "/tmp/cell-reach-~d/" (random 1000000))))))
    (unwind-protect
         (progn
           (write-reach-fixture
            directory
            '(("cell" . "(defpackage #:tester-mini (:use #:cl #:nodecode.kernel #:nodecode.evolved) (:export #:api))
(in-package #:tester-mini)
(define-peripheral mini :not-running t)")
              ("_store_" . "(in-package :tester-mini) (defvar *store* nil)")
              ("_last_" . "(in-package :tester-mini) (defvar *last* nil)")
              ("_old_" . "(in-package :tester-mini) (defvar *old* (make-hash-table))")
              ("sid" . "(in-package :tester-mini) (defun sid () (or *last* \"default\"))")
              ("api" . "(in-package :tester-mini) (defun api (x) (push x *store*))")
              ("cmd" . "(in-package :tester-mini) (defun cmd (args session-id) (let ((s (sid))) (list s args session-id)))")
              ("watch" . "(in-package :tester-mini) (defun watch () (dolist (s '(api)) (print s)))")
              ("mini-define-cell" . "(in-package :tester-mini) (define-cell mini (:command \"mini\" #'cmd) (:start #'watch))")
              ("_zz-reach-seen_" . "(in-package :nodecode.evolved) (defvar *zz-reach-seen* nil)")
              ("zz-reach-poll" . "(in-package :nodecode.evolved) (defun zz-reach-poll (m) (push m *zz-reach-seen*))")
              ("print-object-after-zz-reach-thing-t" . "(in-package :nodecode.evolved)
(defmethod print-object :after ((m zz-reach-thing) stream) (declare (ignore stream)) (ignore-errors (zz-reach-poll m)))")
              ("print-object-before-zz-reach-thing-t" . "(in-package :nodecode.evolved)
(defmethod print-object :before ((m zz-reach-thing) stream) (ignore-errors (zz-reach-log \"~s\" stream)))"))
            '("cell" "_store_" "_last_" "_old_" "sid" "api" "cmd" "watch" "mini-define-cell" "gone"
              "_zz-reach-seen_" "zz-reach-poll" "print-object-after-zz-reach-thing-t" "print-object-before-zz-reach-thing-t"))
           (let ((reach (nlk:cell-reach (namestring directory))))
             (is (equal '("*old*") (getf reach :dead)) "the leftover that loads is dead; *LAST*, read by SID, is not")
             (is (member "api" (getf reach :kept) :test #'string=) "an export is a root")
             (is (member "watch" (getf reach :kept) :test #'string=) "the :start function is reached through the define-cell")
             (is (member "zz-reach-poll" (getf reach :kept) :test #'string=) "a method on a kernel generic reaches what it calls")
             (is (equal '(("print-object before (zz-reach-thing t)" "zz-reach-log")) (getf reach :broken))
                 "a reached call nothing defines is broken, and a binding written (S ...) is not a call")
             (is (equal '("gone") (getf reach :missing)) "a component with no file is named"))
           (is (null (find-symbol "ZZ-REACH-POLL" '#:nodecode.evolved)) "the read interned nothing in EVOLVED")
           (is (null (find-package "TESTER-MINI")) "nor made the cell's package"))
      (uiop:delete-directory-tree (uiop:pathname-parent-directory-pathname directory) :validate t :if-does-not-exist :ignore))))

(deftest cell-reach-guides-the-forget-on-a-one-file-cell (with-made-cell-fixture ())
  ;; The workflow the manual asks for: build, read cell-reach, forget what it
  ;; names the way the layer forgets, read it again. On a cell kept as
  ;; cell.lisp + todo.lisp a file names nothing, so the answer is labels.
  (flet ((keep (text) (keep-definition-text text :session "s-cell" :package "TESTER-TODO")))
    (make-test-cell "todo")
    (keep "(defvar *lists* (make-hash-table :test 'equal))")
    (keep "(defvar *seen* (make-hash-table :test 'equal))")
    (keep "(defun %old-poll () (gethash :x *seen*))")
    (keep "(defun todo (args session-id) (declare (ignore args)) (format nil \"~d\" (hash-table-count (gethash session-id *lists*))))")
    (keep "(define-cell todo (:section (\"tester-todo-section\")) (:command \"tester-todo\" #'todo :description \"Todos.\"))")
    (is (equal '("*seen*" "%old-poll") (getf (nlk:cell-reach "todo") :dead)) "a first try's leftovers, by label")
    (keep "(fmakunbound '%old-poll)")
    (keep "(makunbound '*seen*)")
    (let ((todo (nlk:read-text (merge-pathnames "todo.lisp" (nlk:cell-directory "todo")))))
      (is (null (search "*seen*" todo)) "makunbound cuts the variable out of todo.lisp")
      (is (null (search "%old-poll" todo)) "fmakunbound cuts the function")
      (is (search "(defvar *lists*" todo) "what the cell uses stays"))
    (is (null (getf (nlk:cell-reach "todo") :dead)) "read again, nothing is left over")
    (is (string= "" (knowledge-git "tester-todo" "status" "--porcelain")) "each forget committed")))
