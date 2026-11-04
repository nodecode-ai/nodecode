;;;; cell-fold.lisp --- a made cell as two files: cell.lisp, the package it
;;;; reads in, and NAME.lisp, everything else the conversation kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell made by talking is written the way the scribe keeps every
;;;; definition: one file each (made-cells.lisp). That is what lets a
;;;; conversation redefine one function without touching the rest, but what
;;;; it leaves when the conversation ends is a folder of thirty one-line files
;;;; for a todo list, with every first attempt still in it. FOLD-CELL is the
;;;; cell's last step: it reads the folder in its .asd's load order and writes
;;;; the definitions it keeps into NAME.lisp, each with its own text, so the
;;;; folder is cell.lisp and NAME.lisp. What it leaves out -- a component
;;;; whose file is gone, a name DROP lists (CELL-REACH's dead definitions,
;;;; one the last fresh load skipped) -- the commit names, and the history
;;;; before the fold keeps every one of them. A file holding anything but
;;;; definitions is not folded: what it runs at load is the human's, and it
;;;; stays where it is, listed after NAME.lisp.

(in-package #:nodecode.kernel)

(defun made-cell-folder (cell &aux (root *cells-directory*))
  "CELL -- a made cell's name, \"todo\", or its folder's, \"<identity>-todo\" --
as the folder of kept code it names. Refused for the layer, a knowledge
cell, or a folder the organism did not make."
  (unless root (error "this organism keeps nothing: it has no cells folder"))
  (let ((directory (find-if (lambda (directory)
                              (and directory (probe-file directory)
                                   (eq (kept-folder-kind directory) :code)))
                            (list (merge-pathnames (format nil "~a/" cell) root)
                                  (ignore-errors (cell-directory cell))))))
    (cond ((null directory) (error "no cell ~a was made here" cell))
          ((string= (folder-name directory) (layer-name))
           (error "~a is the layer: a made cell folds, the layer keeps one file per definition" cell))
          (t directory))))

(defun cell-component-names (directory &aux (name (folder-name directory)))
  "The file names DIRECTORY's .asd lists, in its load order."
  (loop for component in (getf (cddr (read-defsystem-form
                                      (merge-pathnames (format nil "~a.asd" name) directory) name))
                               :components)
        when (file-component-p component) collect (second component)))

(defun file-override-line (path)
  "PATH's override marker line, the header comment the loader judges a stale
override by (LAYER-FILE-OVERRIDE), or NIL."
  (with-open-file (in path :external-format :utf-8)
    (loop for line = (read-line in nil nil)
          while (and line (plusp (length line)) (char= (char line 0) #\;))
          when (uiop:string-prefix-p +override-marker+ line) return line)))

(defun cell-file-package-name (text)
  "The package TEXT's first IN-PACKAGE names, as its name reads upcased:
#:FOO, :foo and \"FOO\" alike; NODECODE.EVOLVED when it names none."
  (or (ppcre:register-groups-bind (name)
          ("(?im)^\\(in-package\\s+(?:#?:|\")?([^\\s\")]+)\"?\\s*\\)" text)
        (string-upcase name))
      "NODECODE.EVOLVED"))

(defun cell-main-name (directory definitions)
  "The name of DIRECTORY's main file: its DEFINE-CELL's name, else the
folder's after the organism's own prefix."
  (let ((cell (find "DEFINE-CELL" definitions :key (lambda (entry) (getf (first entry) :operator))
                                              :test #'string=))
        (folder (folder-name directory))
        (prefix (format nil "~a-" (ignore-errors (layer-name)))))
    (string-downcase
     (cond (cell (getf (first cell) :name))
           ((uiop:string-prefix-p prefix folder) (subseq folder (length prefix)))
           (t folder)))))

(defun fold-cell (cell &key drop main (commit t) &aux (directory (made-cell-folder cell))
                                                      (session-id *scribe-session-id*))
  "Fold the made cell CELL into two files: cell.lisp, its package, and
MAIN.lisp -- its DEFINE-CELL's name, todo.lisp -- holding every other
definition it keeps, in the .asd's load order, each its own text under the
package it reads in. DROP names definitions to leave out, symbols or
strings: a dead list (CELL-REACH), or what the last fresh load skipped. A
component whose file is gone leaves too; a file that holds more than
definitions stays as it is. The .asd lists cell and MAIN after it, the
README says so, and one commit carries the fold and names what it left
out. => a plist: :file, :kept and :dropped labels, :kept-files."
  (let* ((order (cell-component-names directory))
         (entries '()) (dropped '()) (folded '()) (kept-files '()))
    (flet ((dropped-p (key &aux (label (definition-key-label key)))
             ;; a label names one definition, a method's among them; a name every one of it
             (some (lambda (designator)
                     (if (and (stringp designator) (find #\Space designator))
                         (string-equal designator label)
                         (string= (designated-definition-name designator) (getf key :name))))
                   drop)))
    (bt2:with-lock-held (*layer-lock*)
      (dolist (component (remove "cell" order :test #'string=))
        (let ((path (merge-pathnames (format nil "~a.lisp" component) directory)))
          (if (not (probe-file path))
              (push (format nil "~a (its file is gone)" component) dropped)
              (let ((hits (file-definitions path))
                    (text (read-text path)))
                (if (or (null hits) (not (file-only-definitions-p path hits)))
                    (push component kept-files)
                    (let ((header (and (= 1 (length hits)) (file-override-line path))))
                      (push path folded)
                      (dolist (hit hits)
                        (destructuring-bind (key start end &optional home) hit
                          (if (dropped-p key)
                              (push (definition-key-label key) dropped)
                              (push (list key (if (packagep home) (package-name home) (cell-file-package-name text))
                                          (or (definition-leading-comment text start) header)
                                          (subseq text start end))
                                    entries))))))))))
      (setf entries (nreverse entries) dropped (nreverse dropped) kept-files (nreverse kept-files))
      (let* ((main (or main (cell-main-name directory entries)))
             (target (merge-pathnames (format nil "~a.lisp" main) directory))
             (name (folder-name directory))
             (system (merge-pathnames (format nil "~a.asd" name) directory))
             (form (read-defsystem-form system name))
             (description (getf (cddr form) :description))
             (package nil))
        (when (string= main "cell") (error "the main file cannot be cell.lisp, the package's"))
        (write-file-atomically
         target
         (with-output-to-string (out)
           (format out ";;;; ~a.lisp --- ~a~%;;;;~%;;;; The cell's code, in the order it loads: cell.lisp declares the package~%;;;; it reads in. Folded by fold-cell from one file per definition.~%"
                   main (or description name))
           (dolist (entry entries)
             (destructuring-bind (key home override text) entry
               (declare (ignore key))
               (terpri out)
               (unless (equal home package)
                 (format out "(in-package #:~(~a~))~%~%" home)
                 (setf package home))
               (when override (format out "~a~%" override))
               (write-string (string-trim +scribe-whitespace+ text) out)
               (terpri out)))))
        ;; every folded file but the target goes; the .asd then lists what is left
        (dolist (path folded)
          (unless (equal (namestring path) (namestring target)) (delete-file path)))
        (let ((plist (cddr form)))
          (setf (getf plist :components)
                (append '((:file "cell")) (list (list :file main))
                        (mapcar (lambda (file) (list :file file))
                                (remove main kept-files :test #'string=))))
          (write-system-file system name "a cell the organism made: cell.lisp, its package, then ~
                                          its code; edit freely, the component list is the load order."
                             (list* (first form) (second form) plist)))
        (let ((readme (merge-pathnames "README.md" directory)))
          (when (probe-file readme)
            (write-file-atomically
             readme (ppcre:regex-replace "A nodecode cell: its definitions one per file,(?s:.*)$" (read-text readme)
                                         (format nil "A nodecode cell: cell.lisp is the package it reads in, ~a.lisp its code, ~
                                                      loaded in the order ~a.asd lists them.~%" main name)))))
        (let ((labels (mapcar (lambda (entry) (definition-key-label (first entry))) entries)))
          ;; a fold of a folded cell with nothing to leave out writes the same bytes: no commit
          (when (and commit (git-changes-p directory))
            (git-commit-all directory
                            (format nil "fold: ~d definition~:p into ~a.lisp~@[; left out: ~{~a~^, ~}~]~@[ - session ~a~]"
                                    (length labels) main dropped session-id)
                            :notice (lambda (line) (format t "~&[~a: ~a]~%" name line))))
          (list :file (namestring target) :kept labels :dropped dropped :kept-files kept-files)))))))

(defun git-changes-p (directory)
  "Whether DIRECTORY's repository has a change to commit; T when it is no
repository git reads, so the commit says why."
  (multiple-value-bind (out _ status)
      (uiop:run-program (list "git" "-C" (sb-ext:native-namestring directory) "status" "--porcelain")
                        :output :string :ignore-error-status t)
    (declare (ignore _))
    (or (/= 0 status) (plusp (length (string-trim +scribe-whitespace+ out))))))
