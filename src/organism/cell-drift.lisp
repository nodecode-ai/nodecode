;;;; cell-drift.lisp --- what a made cell holds live that no file keeps.
;;;;
;;;; Once a cell's files are its record and an eval in it keeps nothing, a
;;;; definition the model only evaluated answers now and is gone at the next
;;;; boot: the cell passes every check made in this image and breaks after a
;;;; restart. CELL-DRIFT reads the image against the files. A definition
;;;; loaded from a file carries the file's pathname; one evaluated carries
;;;; none -- and so does one a write made live from its file's text
;;;; (LOAD-SOURCE), which is why a definition with no pathname is named only
;;;; when no kept file defines it: the cell's own, the layer, every folder
;;;; under the cells folder, and the shipped source. It reads and loads
;;;; nothing into the image.

(in-package #:nodecode.kernel)

(defun drift-key-string (namespace name &optional qualifiers specializers)
  "One kept or live definition as the text DRIFT-KEPT-INDEX keys on."
  (format nil "~a|~a|~{~a~^ ~}|~{~a~^ ~}" namespace name qualifiers specializers))

(defun drift-entry-strings (key)
  "The index strings one DEFINITION-KEY stands for: its namespace's, and the
bare name under :ANY, so what a defining macro expands into is forgiven by
the name it was written under."
  (let* ((operator (getf key :operator))
         (name (getf key :name))
         (namespace (definition-namespace operator)))
    (list* (drift-key-string :any name)
           (cond ((member operator '("DEFMETHOD" "DEFINE-HOOK") :test #'string=)
                  (list (drift-key-string operator name (getf key :qualifiers) (getf key :specializers))))
                 ((keywordp namespace) (list (drift-key-string namespace name)))))))

(defun drift-roots (directory)
  "The folders whose files count as kept: DIRECTORY, the layer, every folder
under the cells folder (through its links)."
  (remove-duplicates
   (remove nil (list* (and directory (uiop:ensure-directory-pathname directory))
                      (ignore-errors (layer-directory))
                      (and *cells-directory* (ignore-errors (uiop:subdirectories *cells-directory*)))))
   :test #'equal :key (lambda (d) (ignore-errors (namestring (truename d))))))

(defun drift-lisp-files (directory)
  "Every .lisp file under DIRECTORY, a .git folder skipped."
  (let (files)
    (uiop:collect-sub*directories
     directory
     (lambda (d) (not (equal (car (last (pathname-directory d))) ".git")))
     (lambda (d) (not (equal (car (last (pathname-directory d))) ".git")))
     (lambda (d) (dolist (f (uiop:directory-files d "*.lisp")) (push f files))))
    files))

(defun drift-expanded-keys (form)
  "The DEFINITION-KEYs of what FORM, a toplevel form, defines once its macros
are expanded: a DEFINE-CELL's START-CELL and SETTING, a DEFINE-PERIPHERAL's
*STOP* and its condition's reader -- what a file defines though it never
writes the name down."
  ;; Walks special forms and expands macros; a call's arguments are not
  ;; walked, nor anything quoted. Bounded, and NIL for whatever does not
  ;; expand here: the index only ever gains names.
  (let ((budget 2000) found)
    (labels ((walk (form depth)
               (when (and (consp form) (symbolp (first form)) (plusp (decf budget)) (< depth 24))
                 (let* ((head (first form))
                        (operator (symbol-name head)))
                   (cond ((member head '(quote function)))
                         ((member operator *defining-operators* :test #'string=)
                          (when-let (key (definition-key form)) (push key found))
                          (walk-all (cddr form) depth))
                         ((special-operator-p head) (walk-all (rest form) depth))
                         ((macro-function head)
                          (multiple-value-bind (expansion expanded) (ignore-errors (macroexpand-1 form))
                            (when expanded
                              (when-let (key (definition-key form)) (push key found))
                              (walk expansion (1+ depth)))))))))
             (walk-all (forms depth)
               (loop for tail on forms while (consp tail)
                     do (walk (car tail) (1+ depth)))))
      (walk form 0))
    found))

(defun drift-file-expansion-keys (file entries)
  "The DEFINITION-KEYs that ENTRIES, FILE's (KEY START END PACKAGE), define
through their macros' expansions; only an entry whose operator is no standard
definition is read again."
  (let ((text nil))
    (loop for (key start end package) in entries
          when (and (packagep package)
                    (not (member (getf key :operator) *defining-operators* :test #'string=)))
            append (ignore-errors
                    (let ((form (let ((*package* package) (*read-eval* nil))
                                  (read-from-string (or text (setf text (uiop:read-file-string file)))
                                                    t nil :start start :end end))))
                      (drift-expanded-keys form))))))

(defun drift-kept-index (roots)
  "Every definition a file under ROOTS or the shipped source defines, as a
hash of DRIFT-KEY-STRINGs -- what a file's defining macros expand into
included."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (root roots)
      (dolist (file (ignore-errors (drift-lisp-files root)))
        (let ((entries (ignore-errors (file-definitions file))))
          (dolist (key (append (mapcar #'first entries) (drift-file-expansion-keys file entries)))
            (dolist (s (drift-entry-strings key)) (setf (gethash s index) t))))))
    (let ((shipped (ignore-errors (shipped-definitions))))
      (when (hash-table-p shipped)
        (maphash (lambda (name entries)
                   (declare (ignore name))
                   (dolist (entry entries)
                     (dolist (s (drift-entry-strings (first entry))) (setf (gethash s index) t))))
                 shipped)))
    index))

(defun drift-unfiled-p (object-or-sources)
  "Whether a live definition came from no file: its definition sources, or
OBJECT's, carry no pathname."
  (let ((sources (if (listp object-or-sources)
                     object-or-sources
                     (ignore-errors (list (sb-introspect:find-definition-source object-or-sources))))))
    (notany (lambda (source) (and source (sb-introspect:definition-source-pathname source))) sources)))

(defun drift-specializer-string (specializer)
  "A live method's SPECIALIZER as SPECIALIZER-STRING keys the written one."
  (if (typep specializer 'sb-mop:eql-specializer)
      (let ((*package* (find-package '#:nodecode.evolved)) (*print-case* :upcase))
        (format nil "EQL-~a" (prin1-to-string (sb-mop:eql-specializer-object specializer))))
      (let ((name (ignore-errors (class-name specializer))))
        (if name (symbol-name name) (princ-to-string specializer)))))

(defun drift-method-label (generic method)
  "=> (values KEY-STRING LABEL) for METHOD of GENERIC."
  (let* ((name (name-designator-string (sb-mop:generic-function-name generic)))
         (qualifiers (mapcar #'head-element-string (method-qualifiers method)))
         (specializers (mapcar #'drift-specializer-string (sb-mop:method-specializers method))))
    (values (drift-key-string "DEFMETHOD" name qualifiers specializers)
            (string-downcase (format nil "~a~{ ~a~} (~{~a~^ ~})" name qualifiers specializers)))))

(defun drift-live-only (packages index)
  "Each definition this image holds that no kept file defines and no file
loaded: (PACKAGE-NAME LABEL) for the functions, macros and variables homed
in PACKAGES, and for every method and hook the image holds."
  (let (found (seen (make-hash-table :test 'equal)))
    (flet ((note (package label key)
             (unless (or (gethash key index) (gethash key seen))
               (setf (gethash key seen) t)
               (push (list package label) found))))
      (dolist (package packages)
        (do-symbols (symbol package)
          (when (eq (symbol-package symbol) package)
            (let ((name (symbol-name symbol))
                  (where (package-name package)))
              (when (and (fboundp symbol) (not (special-operator-p symbol))
                         (not (typep (fdefinition symbol) 'generic-function))
                         (drift-unfiled-p (sb-introspect:find-definition-sources-by-name
                                           symbol (if (macro-function symbol) :macro :function)))
                         (not (gethash (drift-key-string :any name) index)))
                (note where (format nil "~a ~(~a~)" (if (macro-function symbol) "defmacro" "defun") name)
                      (drift-key-string :function name)))
              (when (and (boundp symbol) (not (constantp symbol))
                         (drift-unfiled-p (sb-introspect:find-definition-sources-by-name symbol :variable))
                         (not (gethash (drift-key-string :any name) index)))
                (note where (format nil "defvar ~(~a~)" name) (drift-key-string :variable name)))))))
      (sb-pcl::map-all-generic-functions
       (lambda (generic)
         (dolist (method (sb-mop:generic-function-methods generic))
           (unless (or (typep method 'sb-mop:standard-accessor-method)
                       (drift-system-generic-p generic))
             (when (drift-unfiled-p method)
               (multiple-value-bind (key label) (drift-method-label generic method)
                 (let ((home (ignore-errors (symbol-package (let ((n (sb-mop:generic-function-name generic)))
                                                              (if (consp n) (second n) n))))))
                   (note (if home (package-name home) "?") (format nil "defmethod ~a" label) key))))))))
      (loop for (name . chain) in *hooks*
            do (loop for (hook-key . fn) in chain
                     for key = (drift-key-string "DEFINE-HOOK" (name-designator-string name) (list hook-key))
                     when (drift-unfiled-p fn)
                       do (note "hook" (format nil "define-hook ~(~a~) ~s" (name-designator-string name) hook-key) key))))
    (nreverse found)))

(defun drift-system-generic-p (generic)
  "Whether GENERIC is the Lisp's own machinery -- named by nothing, a keyword,
or a symbol of an SB- package -- whose methods no cell writes."
  (let* ((name (sb-mop:generic-function-name generic))
         (symbol (if (consp name) (second name) name))
         (home (and symbol (symbolp symbol) (symbol-package symbol))))
    (or (null symbol) (not (symbolp symbol)) (keywordp symbol)
        (and home (uiop:string-prefix-p "SB-" (package-name home))))))

(defun drift-file-not-live (entries)
  "The labels of ENTRIES, (KEY START END PACKAGE) of a cell's files, that this
image does not hold: a function, macro, variable or hook the file defines and
the image lacks."
  (loop for (key nil nil package) in entries
        for operator = (getf key :operator)
        for name = (getf key :name)
        for symbol = (and (packagep package) (find-symbol name package))
        for namespace = (definition-namespace operator)
        when (cond ((eq namespace :function) (not (and symbol (fboundp symbol))))
                   ((eq namespace :variable) (not (and symbol (boundp symbol))))
                   ((string= operator "DEFINE-HOOK")
                    (not (loop for (hooked . chain) in *hooks*
                               thereis (and (equal (name-designator-string hooked) name)
                                            (assoc (first (getf key :qualifiers)) chain :test #'string=))))))
          collect (definition-key-label key)))

(defun cell-drift (name)
  "Whether the made cell NAME's files hold what this image runs for it --
NAME a cell's name, or a folder holding a made cell's .asd. Once a cell's
files are its record, a definition only evaluated works now and is gone at
the next boot. => a plist: :LIVE-ONLY ((PACKAGE LABEL) ...), what the image
holds that came from no file and that no kept file defines -- the cell's
package first, then the kernel's and the layer's, where a definition that
was meant for the cell may have landed -- every method and hook included;
:NOT-LIVE the labels the cell's files define and the image lacks, a file
ahead of the image; :CLEAN T when both are empty. Reads only."
  (let* ((directory (if (or (pathnamep name) (find #\/ name))
                        (uiop:ensure-directory-pathname name)
                        (cell-directory (made-cell-name name))))
         (entries (loop for file in (drift-lisp-files directory)
                        append (ignore-errors (file-definitions file))))
         (floor (list (find-package '#:nodecode.kernel) (find-package '#:nodecode.evolved)))
         (own (remove-if (lambda (p) (or (not (packagep p)) (member p floor)
                                         (member (package-name p) '("COMMON-LISP-USER" "ASDF-USER") :test #'string=)))
                         (remove-duplicates (mapcar #'fourth entries))))
         (index (drift-kept-index (drift-roots directory)))
         (live-only (drift-live-only (append own floor) index))
         (own-names (mapcar #'package-name own))
         (live-only (stable-sort live-only (lambda (a b) (and (member (first a) own-names :test #'string=)
                                                              (not (member (first b) own-names :test #'string=))))))
         (not-live (drift-file-not-live entries)))
    (list :live-only live-only :not-live not-live :clean (and (null live-only) (null not-live)))))
