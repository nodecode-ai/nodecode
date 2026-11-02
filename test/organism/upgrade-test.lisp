;;;; upgrade-test.lisp --- the upgrade classifier (upgrade.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two scratch trees laid out as a release stages them -- src/nodecode.asd
;;;; beside src/src/ -- and one classification each. Nothing is evaluated:
;;;; the fixture's definitions are text the scanner reads, never code the
;;;; image runs.

(in-package #:nodecode.test)

(defun upgrade-source (package &rest forms)
  "A file's text: an in-package of PACKAGE, none when it is NIL, then each of
FORMS on a line of its own."
  (format nil "~@[(in-package #:~a)~%~]~{~a~%~}" package forms))

(defparameter *upgrade-old-a*
  (upgrade-source "nodecode.evolved"
                  "(defun upg-f () 1)"
                  "(defmacro upg-m (x) `(list ,x))"
                  "(defun upg-g () (upg-m 1))"
                  "(defun upg-h () 2)"
                  "(defstruct upg-s a)"
                  "(defparameter *upg-p* 1)"
                  "(defgeneric upg-gen (a))"
                  "(defconstant +upg-k+ 1)")
  "The old tree's one file.")

(defparameter *upgrade-old-tree*
  (list (cons "src/src/a.lisp" *upgrade-old-a*)
        (cons "src/nodecode.asd"
              "(defsystem \"upg\" :depends-on (\"uiop\") :components ((:file \"a\")))"))
  "The old tree: (RELATIVE-PATH . TEXT) ...")

(defun upgrade-tree (&rest replacements)
  "*UPGRADE-OLD-TREE* with REPLACEMENTS -- PATH TEXT pairs -- written last, so
each wins over the base entry of the same path. A TEXT that is a list is
UPGRADE-SOURCE's arguments."
  (append *upgrade-old-tree*
          (loop for (path text) on replacements by #'cddr
                collect (cons path (if (listp text) (apply #'upgrade-source text) text)))))

(defun upgrade-a-with (old new &rest lines)
  "UPGRADE-TREE with *UPGRADE-OLD-A* the line OLD replaced by NEW, then each
of LINES on a line of its own after it; an OLD of \"\" replaces nothing."
  (is (search old *upgrade-old-a*) (format nil "fixture line present: ~a" old))
  (upgrade-tree "src/src/a.lisp"
                (format nil "~a~{~a~%~}" (replace-once *upgrade-old-a* old new) lines)))

(defun write-upgrade-tree (root files)
  "Write FILES — (RELATIVE-PATH . TEXT) ... — under ROOT; answers ROOT."
  (dolist (file files root)
    (write-temp-file (merge-pathnames (car file) root) (cdr file))))

(deftests upgrade (old-tree new-tree expected)
  ;; A row is a change between two trees and what the plan carries for it:
  ;; EXPECTED is plan keys and the values they must hold, :LINE the plan's
  ;; whole line and :SAYS a needle in it. An OLD-TREE of NIL is
  ;; *UPGRADE-OLD-TREE*.
  (with-temp-trees ((old "upgrade-old") (new "upgrade-new"))
    (write-upgrade-tree old (or old-tree *upgrade-old-tree*))
    (write-upgrade-tree new new-tree)
    (loop with plan = (nlk::classify-upgrade (merge-pathnames "src/src/" old)
                                             (merge-pathnames "src/src/" new))
          for (key value) on expected by #'cddr
          for read = (if (member key '(:line :says)) (nlk::upgrade-plan-line plan) (getf plan key))
          do (is (if (eq key :says) (search value read) (equal value read))
                 (format nil "~(~a~): ~s, not ~s" key read value))))
  (same-tree-is-nothing nil (upgrade-tree)
   '(:verdict :same :hot nil :line "same tree: nothing to do"))
  (changed-defun-is-hot nil
   (upgrade-a-with "(defun upg-f () 1)" "(defun upg-f () 3)")
   '(:verdict :hot :hot (("a.lisp" ("DEFUN" . "upg-f"))) :dependents nil
     :files (:changed 1 :added 0 :removed 0 :unloaded 0) :line "hot: 1 definition across 1 file"))
  ;; the macro, then its one user, in text order; upg-h names nothing and stays
  (changed-macro-carries-its-users nil
   (upgrade-a-with "(defmacro upg-m (x) `(list ,x))" "(defmacro upg-m (x) `(vector ,x))")
   '(:verdict :hot :hot (("a.lisp" ("DEFMACRO" . "upg-m") ("DEFUN" . "upg-g")))
     :dependents ("upg-g") :says "1 dependent"))
  (changed-defstruct-restarts nil
   (upgrade-a-with "(defstruct upg-s a)" "(defstruct upg-s a b)")
   '(:verdict :restart :held (("upg-s" . "defstruct leaves every live instance obsolete"))
     :line
     "restart: upg-s — defstruct leaves every live instance obsolete; 0 definitions would be hot"))
  ;; a DEFSTRUCT under the record's name would be sent hot; SBCL would
  ;; refuse the new slot and the upgrade would stay on the old definition.
  (changed-define-record-restarts
   (upgrade-tree "src/src/r.lisp" '("nodecode.evolved" "(define-record upg-r a)"))
   (upgrade-tree "src/src/r.lisp" '("nodecode.evolved" "(define-record upg-r a b)"))
   '(:verdict :restart :hot nil
     :held (("upg-r" . "define-record is a defstruct: every live instance goes obsolete"))))
  (kernel-name-is-under-the-floor
   (upgrade-tree "src/src/k.lisp" '("nodecode.kernel" "(defun store-open-p () :old)"))
   (upgrade-tree "src/src/k.lisp" '("nodecode.kernel" "(defun store-open-p () :upgraded)"))
   '(:verdict :hot :floor ("store-open-p") :says "1 under the floor"))
  (changed-inline-function-carries-its-callers
   (upgrade-a-with "" "" "(declaim (inline upg-h))" "(defun upg-i () (upg-h))")
   (upgrade-a-with "(defun upg-h () 2)" "(defun upg-h () 3)"
                   "(declaim (inline upg-h))" "(defun upg-i () (upg-h))")
   '(:verdict :hot :hot (("a.lisp" ("DEFUN" . "upg-h") ("DEFUN" . "upg-i")))
     :dependents ("upg-i")))
  (new-defstruct-in-new-file-is-hot nil
   (upgrade-tree "src/src/b.lisp"
                 '("nodecode.evolved" "(defstruct upg-t a)" "(defconstant +upg-j+ 1)"))
   '(:verdict :hot :hot (("b.lisp" ("DEFSTRUCT" . "upg-t") ("DEFCONSTANT" . "+upg-j+")))
     :files (:changed 0 :added 1 :removed 0 :unloaded 0)))
  (toplevel-form-restarts nil
   (upgrade-a-with "" "" "(setf *upg-p* 2)")
   '(:verdict :restart :held (("a.lisp" . "changes outside any definition are boot state"))))
  (new-file-with-toplevel-form-restarts nil
   (upgrade-tree "src/src/b.lisp" '("nodecode.evolved" "(defun upg-u () 1)" "(push 1 *upg-list*)"))
   '(:verdict :restart :held (("b.lisp" . "changes outside any definition are boot state"))))
  (new-dependency-restarts nil
   (upgrade-tree "src/nodecode.asd"
                 "(defsystem \"upg\" :depends-on (\"uiop\" \"no-such-system-upg\") :components ((:file \"a\")))")
   '(:verdict :restart :held (("no-such-system-upg" . "a system this image never loaded"))))
  ;; cl-ppcre is in the image whether or not the old .asd named it; and an
  ;; .asd that moved while no .lisp did is nothing to do
  (dependency-this-image-loaded-is-not-held nil
   (upgrade-tree "src/nodecode.asd"
                 "(defsystem \"upg\" :depends-on (\"uiop\" \"cl-ppcre\") :components ((:file \"a\")))")
   '(:verdict :same :held nil))
  (gone-definition-is-noted-not-held nil
   (upgrade-a-with (format nil "(defun upg-f () 1)~%") "")
   '(:verdict :hot :hot nil :gone ("upg-f")))
  (removed-file-is-gone
   (upgrade-tree "src/src/b.lisp" '("nodecode.evolved" "(defun upg-v () 1)"))
   (upgrade-tree)
   '(:verdict :hot :gone ("upg-v") :files (:changed 0 :added 0 :removed 1 :unloaded 0)))
  (defgeneric-lambda-list-move-restarts nil
   (upgrade-a-with "(defgeneric upg-gen (a))" "(defgeneric upg-gen (a b))")
   '(:verdict :restart :held (("upg-gen" . "a generic's lambda list moved under its methods"))))
  (defgeneric-option-change-is-hot nil
   (upgrade-a-with "(defgeneric upg-gen (a))" "(defgeneric upg-gen (a) (:documentation \"d\"))")
   '(:verdict :hot :hot (("a.lisp" ("DEFGENERIC" . "upg-gen")))))
  (changed-constant-restarts nil
   (upgrade-a-with "(defconstant +upg-k+ 1)" "(defconstant +upg-k+ 2)")
   '(:verdict :restart :held (("+upg-k+" . "a constant's redefinition is a continuable error"))))
  (unknown-package-is-held nil
   (upgrade-tree "src/src/b.lisp" '("no-such-package-upg" "(defun upg-x () 1)"))
   '(:verdict :restart :held (("upg-x" . "package no-such-package-upg is not in this image"))))
  (defpackage-export-add-is-hot
   (upgrade-tree "src/src/p.lisp" '(nil "(defpackage #:upg-pkg (:use #:cl) (:export #:a))"))
   (upgrade-tree "src/src/p.lisp" '(nil "(defpackage #:upg-pkg (:use #:cl) (:export #:a #:b))"))
   '(:verdict :hot :hot (("p.lisp" ("DEFPACKAGE" . "upg-pkg")))))
  (defpackage-use-change-restarts
   (upgrade-tree "src/src/p.lisp" '(nil "(defpackage #:upg-pkg (:use #:cl) (:export #:a))"))
   (upgrade-tree "src/src/p.lisp" '(nil "(defpackage #:upg-pkg (:use #:cl #:uiop) (:export #:a))"))
   '(:verdict :restart :held (("upg-pkg" . "defpackage moves symbols under running code"))))
  ;; a cell this image did not enable: its package is absent from the
  ;; image, declared by an old file, defined by nothing in the new tree
  (files-of-a-package-never-loaded-are-left-alone
   (upgrade-tree "src/src/cell.lisp" '("upg-absent-cell" "(defun z () 1)"))
   (upgrade-tree "src/src/cell.lisp" '("upg-absent-cell" "(defun z () 2)" "(setf *boot* t)")
                 "src/src/cell2.lisp" '("upg-absent-cell" "(defstruct zz a)"))
   '(:verdict :same :held nil :unloaded ("cell.lisp" "cell2.lisp")
     :files (:changed 0 :added 0 :removed 0 :unloaded 2))))

;;; --- converge: the transaction ---------------------------------------------

(defmacro with-converge-trees ((old-root new-root) new-text &body body)
  "Two scratch trees, each one live.lisp: the old one defining UPG-LIVE-F as 1,
the new one NEW-TEXT. UPG-LIVE-F is defined as the old tree has it, and
unbound again with UPG-LIVE-G after; this image's self-view is moved to the old
tree, its shipped index forgotten and its effective version cleared for BODY."
  `(nlk:with-cleanup ((fmakunbound 'nodecode.evolved::upg-live-f)
                      (fmakunbound 'nodecode.evolved::upg-live-g))
     (eval '(defun nodecode.evolved::upg-live-f () 1))
     (with-temp-trees ((old "converge-old") (new "converge-new"))
       (write-upgrade-tree old (list (cons "src/src/live.lisp"
                                           (evolved-source "(defun upg-live-f () 1)"))))
       (write-upgrade-tree new (list (cons "src/src/live.lisp" ,new-text)))
       (let* ((,old-root (truename (merge-pathnames "src/src/" old)))
              (,new-root (truename (merge-pathnames "src/src/" new)))
              (nlk::*source-root* ,old-root)
              (nlk::*shipped-definitions* nil)
              (nlk::*effective-version* nil)
              (nlk::*staged-source-root* nil))
         (declare (ignorable ,new-root))
         ,@body))))

(deftest upgrade-converge-makes-the-new-tree-live ()
  (with-converge-trees (old-root new-root) (evolved-source "(defun upg-live-f () 2)")
    (let ((plan (nlk::classify-upgrade old-root new-root)))
      (is-shape plan (:verdict eq :hot) (:symbols '(("NODECODE.EVOLVED" . "UPG-LIVE-F"))))
      (is-values (count files) (nlk::converge (list (cons plan new-root)) :version "t1")
        (count = 1) (files = 1))
      (is (eql 2 (funcall 'nodecode.evolved::upg-live-f)) "the new definition is live")
      (is (equal (namestring new-root) (namestring nlk::*source-root*)))
      (is (equal "t1" nlk::*effective-version*)))))

(deftest upgrade-converge-defines-what-the-release-adds ()
  "A structure and a constant the old tree never had are what the classifier
sends hot, and converge makes them live. Without this, a release adding
catalog-model would be refused and the auto-update would stick behind it."
  (with-converge-trees (old-root new-root)
      (evolved-source "(defun upg-live-f () 2)" "(defstruct upg-live-rec a)"
                      "(defconstant +upg-live-k+ 7)"
                      "(defun upg-live-g (r) (+ (upg-live-rec-a r) +upg-live-k+))")
    (let ((plan (nlk::classify-upgrade old-root new-root)))
      (is (eq :hot (getf plan :verdict)) "nothing the old tree had is redefined")
      (nlk::converge (list (cons plan new-root)) :version "t3")
      (let ((rec (funcall 'nodecode.evolved::make-upg-live-rec :a 5)))
        (is (eql 12 (funcall 'nodecode.evolved::upg-live-g rec)) "the struct, constant and user are live"))
      (is (equal "t3" nlk::*effective-version*)))))

(deftest upgrade-converge-under-a-locked-floor (with-floor-restored)
  (with-converge-trees (old-root new-root) (evolved-source "(defun upg-live-f () 3)")
    (nlk::lock-floor)
    (nlk::converge (list (cons (nlk::classify-upgrade old-root new-root) new-root)))
    (is (eql 3 (funcall 'nodecode.evolved::upg-live-f)))
    (is nlk::*floor-locked* "the floor is raised again after")))

(deftest upgrade-converge-is-all-or-nothing ()
  ;; f moves to 2, m is made, then g -- a method on a class this image lacks
  ;; -- will not evaluate: f is 1 again, m is gone, nothing about the image
  ;; moved
  (with-converge-trees (old-root new-root)
      (evolved-source "(defun upg-live-f () 2)" "(defmacro upg-live-m () 1)"
                      "(defmethod upg-live-g ((x upg-no-such-class)) x)")
    (let ((plan (nlk::classify-upgrade old-root new-root)))
      (is (eq :hot (getf plan :verdict)))
      (is-carrying (refused (refusal-text nlk:upgrade-refusal
                              (nlk::converge (list (cons plan new-root)) :version "t2")))
        ("upg-live-g" "the refusal names the definition"))
      (is (eql 1 (funcall 'nodecode.evolved::upg-live-f)) "f is back to 1")
      (is (null (macro-function 'nodecode.evolved::upg-live-m)) "m is unbound again")
      (is (equal (namestring old-root) (namestring nlk::*source-root*)))
      (is (null nlk::*effective-version*)))))

(deftest upgrade-converge-takes-a-new-export-before-its-user ()
  "A release exporting a symbol from its package file and reading it in a
file that sorts before that one goes live: the plan walks the .asd's load
order. Without this, sorted, engine/exec.lisp would read nlk:utf-8-octets
before package.lisp exported it."
  (flet ((tree (exports body)
           (list (cons "src/nodecode.asd"
                       "(defsystem \"upg-order\" :components ((:module \"src\" :serial t :components ((:file \"zpkg\") (:file \"a\")))))")
                 (cons "src/src/zpkg.lisp"
                       (format nil "(defpackage #:upg-order-pkg (:use #:cl) (:export ~{#:~a~^ ~}))~%" exports))
                 (cons "src/src/a.lisp" (evolved-source body)))))
    (nlk:with-cleanup ((fmakunbound 'nodecode.evolved::upg-order-f)
                       (when (find-package "UPG-ORDER-PKG") (delete-package "UPG-ORDER-PKG")))
      (eval '(defpackage #:upg-order-pkg (:use #:cl) (:export #:old)))
      (eval '(defun nodecode.evolved::upg-order-f () 1))
      (with-temp-trees ((old "order-old") (new "order-new"))
        (write-upgrade-tree old (tree '("old") "(defun upg-order-f () 1)"))
        (write-upgrade-tree new (tree '("old" "new") "(defun upg-order-f () 'upg-order-pkg:new)"))
        (let* ((old-root (truename (merge-pathnames "src/src/" old)))
               (new-root (truename (merge-pathnames "src/src/" new)))
               (nlk::*source-root* old-root)
               (nlk::*shipped-definitions* nil)
               (nlk::*effective-version* nil)
               (nlk::*staged-source-root* nil)
               (plan (nlk::classify-upgrade old-root new-root)))
          (is (equal '("zpkg.lisp" "a.lisp") (mapcar #'first (getf plan :hot))) "the package file goes first, as the .asd loads it")
          (nlk::converge (list (cons plan new-root)) :version "t4")
          (is (eq (find-symbol "NEW" "UPG-ORDER-PKG") (funcall 'nodecode.evolved::upg-order-f)) "the user reads the new export")
          (is (equal "t4" nlk::*effective-version*)))))))
