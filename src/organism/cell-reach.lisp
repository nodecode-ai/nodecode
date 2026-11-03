;;;; cell-reach.lisp --- which of a made cell's kept definitions the cell
;;;; still uses.
;;;;
;;;; A cell made by talking keeps every definition the model evaluated in it.
;;;; One it replaced under a new name stays beside the replacement and still
;;;; loads, so no load proves it dead: the stress-tested todo cell kept
;;;; *SEEN*, *CHECKING* and *LAST-CHECK* from its first attempt, each loading
;;;; cleanly, while *LAST-SID*, which looked like one of them, is read by
;;;; %SID. CELL-REACH walks from what the cell is -- its DEFINE-CELL, a hook,
;;;; a method on a generic it does not define, its package's exports --
;;;; through every name each reached form mentions, quoted ones too, since a
;;;; list of symbols handed to the kernel is a use. What no walk reaches is a
;;;; leftover. A reached form calling a name nothing defines is broken: it
;;;; answers now only because the image still holds what the folder lost.
;;;; It reads and loads nothing: the files are read in scratch packages that
;;;; stand in for the cell's and the image's, so a read interns nothing in
;;;; NODECODE.EVOLVED, and they are deleted before it answers.

(in-package #:nodecode.kernel)

(defun reach-packages (cell-name)
  "Scratch packages to read a made cell's files in: => (values CELL MIRRORS),
CELL standing in for the package CELL-NAME, MIRRORS an alist (MIRROR . REAL)
for the kernel and EVOLVED. Each sees what its real one exports, and every
one of them reads the real names and nicknames as the scratch ones."
  (let* ((stamp (random most-positive-fixnum))
         (use '(#:cl #:nodecode.kernel #:nodecode.evolved))
         (cell (make-package (format nil "REACH-CELL-~d" stamp) :use use))
         (mirrors (loop for real in (list (find-package '#:nodecode.kernel) (find-package '#:nodecode.evolved))
                        collect (cons (make-package (format nil "REACH-~a-~d" (package-name real) stamp) :use use)
                                      real))))
    (loop for (mirror . real) in mirrors
          do (do-external-symbols (symbol real) (export (list symbol) mirror)))
    (dolist (reader (cons cell (mapcar #'car mirrors)))
      (sb-ext:add-package-local-nickname (string-upcase cell-name) cell reader)
      (loop for (mirror . real) in mirrors
            do (dolist (name (cons (package-name real) (package-nicknames real)))
                 (sb-ext:add-package-local-nickname name mirror reader))))
    (values cell mirrors)))

(defun reach-forms (directory cell)
  "Each top-level form of the made cell DIRECTORY, in its .asd's load order,
read in CELL (REACH-PACKAGES) and whatever IN-PACKAGE names under it:
=> (values ((FILE . FORM) ...) EXPORTS MISSING), EXPORTS the symbols
cell.lisp's DEFPACKAGE exports, MISSING the components with no file."
  (let* ((name (folder-name directory))
         (system (read-defsystem-form (merge-pathnames (format nil "~a.asd" name) directory) name))
         forms exports missing)
    (dolist (component (getf (cddr system) :components))
      (when (file-component-p component)
        (let ((path (merge-pathnames (format nil "~a.lisp" (second component)) directory)))
          (if (not (probe-file path))
              (push (second component) missing)
              (with-open-file (stream path)
                (let ((*package* cell) (*read-eval* nil))
                  (loop for form = (read stream nil stream) until (eq form stream)
                        do (cond ((named-form-p form "IN-PACKAGE")
                                  (setf *package* (or (find-package (second form)) cell)))
                                 ((named-form-p form "DEFPACKAGE")
                                  (dolist (option (cddr form))
                                    (when (and (consp option) (eq (first option) :export))
                                      (dolist (x (rest option)) (push (intern (string x) cell) exports)))))
                                 ((consp form) (push (cons (second component) form) forms))))))))))
    (values (nreverse forms) exports (nreverse missing))))

(defun reach-symbols (tree &optional found)
  "Every symbol but NIL and the keywords inside TREE, quoted data included."
  (cond ((consp tree) (reach-symbols (cdr tree) (reach-symbols (car tree) found)))
        ((and tree (symbolp tree) (not (keywordp tree))) (cons tree found))
        (t found)))

(defun reach-calls (tree &optional bound calls)
  "(values CALLS BOUND): every symbol TREE puts in operator position, and every
one it binds -- a lambda list, a LET-like binding list, a DOLIST / DOTIMES
variable, a LOOP FOR / WITH -- so a binding written (S ...) is not a call."
  (when (consp tree)
    (let ((head (and (symbolp (first tree)) (first tree))))
      (when head
        (push head calls)
        (flet ((binds (part) (setf bound (reach-symbols part bound))))
          (let ((name (symbol-name head)))
            (cond ((member name '("LET" "LET*" "LAMBDA" "MULTIPLE-VALUE-BIND" "DESTRUCTURING-BIND"
                                  "WHEN-LET" "IF-LET" "BIND" "DOLIST" "DOTIMES" "FLET" "LABELS"
                                  "HANDLER-CASE" "HANDLER-BIND")
                           :test #'string=)
                   (when (consp (rest tree)) (binds (second tree)))
                   (when (string= name "HANDLER-CASE") (binds (cddr tree))))
                  ((member name '("DEFUN" "DEFMACRO") :test #'string=) (binds (third tree)))
                  ((string= name "DEFMETHOD") (binds (find-if #'listp (cddr tree))))
                  ((string= name "LOOP")
                   (loop for (word next) on (rest tree)
                         when (and (symbolp word)
                                   (member (symbol-name word) '("FOR" "WITH" "AND" "AS") :test #'string=))
                           do (binds next)))))))
      (loop for part on tree while (consp part)
            do (multiple-value-setq (calls bound) (reach-calls (car part) bound calls)))))
  (values calls bound))

(defun cell-reach (name)
  "Which of the made cell NAME's kept definitions the cell reaches -- NAME a
cell's name, or a folder holding a made cell's .asd. Answers a plist:
:DEAD the files of the definitions no root reaches, in load order; :BROKEN
(FILE NAME) for each reached form calling a function nothing in the folder
or the image defines; :MISSING the components with no file; :ROOTS and
:KEPT the files reached. Roots: DEFINE-CELL, DEFINE-HOOK, HOOK,
DEFINE-PERIPHERAL, a DEFMETHOD on a generic the cell does not define, and
the package's exports. Reads only."
  (let* ((directory (if (or (pathnamep name) (find #\/ name))
                        (uiop:ensure-directory-pathname name)
                        (cell-directory (made-cell-name name)))))
    (multiple-value-bind (cell mirrors) (reach-packages (folder-name directory))
      (unwind-protect
           (multiple-value-bind (forms exports missing) (reach-forms directory cell)
             (let ((defined (make-hash-table :test 'eq))
                   (reached (make-hash-table :test 'eq))
                   (scratch (cons cell (mapcar #'car mirrors)))
                   roots queue broken)
               (dolist (entry forms)
                 (let ((key (definition-key (rest entry))))
                   (when (and key (not (named-form-p (rest entry) "DEFMETHOD")))
                     (let ((what (second (rest entry))))
                       (push entry (gethash (if (consp what) (first what) what) defined))))))
               (dolist (entry forms)
                 (let ((form (rest entry)))
                   (when (or (some (lambda (op) (named-form-p form op)) '("DEFINE-CELL" "DEFINE-HOOK" "HOOK" "DEFINE-PERIPHERAL"))
                             (and (named-form-p form "DEFMETHOD") (not (gethash (second form) defined))))
                     (push entry roots))))
               (setf queue (append roots (loop for symbol in exports append (gethash symbol defined))))
               (loop while queue
                     do (let ((entry (pop queue)))
                          (unless (gethash entry reached)
                            (setf (gethash entry reached) t)
                            (dolist (symbol (reach-symbols (cddr entry)))
                              (setf queue (append (gethash symbol defined) queue))))))
               (flet ((real (symbol)
                        ;; a scratch name stands for the real package's, when it has one
                        (let ((mirror (assoc (symbol-package symbol) mirrors)))
                          (cond (mirror (find-symbol (symbol-name symbol) (cdr mirror)))
                                ((eq (symbol-package symbol) cell) nil)
                                (t symbol)))))
                 (dolist (entry forms)
                   (when (gethash entry reached)
                     (multiple-value-bind (calls bound) (reach-calls (rest entry))
                       (dolist (call (remove-duplicates calls))
                         (let ((real (real call)))
                           (unless (or (gethash call defined) (member call bound)
                                       (not (member (symbol-package call) scratch))
                                       (and real (or (fboundp real) (special-operator-p real))))
                             (push (list (first entry) (string-downcase (symbol-name call))) broken))))))))
               (flet ((files (entries) (remove-duplicates (mapcar #'first entries) :test #'string= :from-end t)))
                 (list :dead (files (remove-if (lambda (entry) (gethash entry reached)) forms))
                       :broken (remove-duplicates (nreverse broken) :test #'equal)
                       :missing missing
                       :roots (files (reverse roots))
                       :kept (files (remove-if-not (lambda (entry) (gethash entry reached)) forms))))))
        (dolist (package (cons cell (mapcar #'car mirrors)))
          (delete-package package))))))
