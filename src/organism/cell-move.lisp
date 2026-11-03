;;;; cell-move.lisp --- "make what we just built a cell": what the layer
;;;; already keeps, moved into a made cell after the fact.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A conversation that built something before anyone said "cell" left its
;;;; definitions in the layer, among everything else the organism keeps.
;;;; MOVE-TO-CELL takes the files of the names it is given out of the layer
;;;; into a made cell (made-cells.lisp), NEW-CELL making it when there is none,
;;;; in the layer's own load order, and commits the move in both folders. Nothing
;;;; live changes: the definitions stand in this image as they were, and the
;;;; next boot loads them from the cell. A file is moved whole or not at all: a
;;;; file that also defines a name the call leaves out stays in the layer, and
;;;; the call says so.

(in-package #:nodecode.kernel)

(defun designated-definition-name (designator)
  "DESIGNATOR, a symbol, a string or (SETF NAME), as the scanner's :NAME of
the definitions it names: a string upcased, as the reader would."
  (typecase designator
    (symbol (symbol-name designator))
    (string (string-upcase designator))
    ((cons (eql setf)) (format nil "(SETF ~a)" (symbol-name (second designator))))
    (t (error "~s names no definition: give a symbol, a string or (setf name)" designator))))

(defun folder-load-order (directory &aux (name (folder-name directory)))
  "The file names DIRECTORY's .asd lists, in its load order."
  (loop for component in (getf (cddr (read-defsystem-form (layer-system-file name) name)) :components)
        when (file-component-p component) collect (second component)))

(defun move-to-cell (cell &rest names &aux (session-id *scribe-session-id*))
  "Move every layer file defining one of NAMES -- a function, its methods, a
variable, a hook on it -- into the made cell CELL, which NEW-CELL makes when
there is none, and commit the move in the layer and in the cell. => the
definitions moved, by label. Nothing live changes; the next boot loads them
from the cell. A file that defines a name NAMES leaves out stays whole."
  (unless names
    (error "name what to move: (move-to-cell ~s 'my-function '*my-variable* ...)" cell))
  (let* ((directory (cell-directory (made-cell-name cell)))
         (layer (layer-directory))
         (wanted (mapcar #'designated-definition-name names))
         (moving '()) (shared '()) (found '()) (labels '()))
    (unless (eq (kept-folder-kind directory) :code)
      (when (probe-file directory)
        (error "~a is a folder this organism did not make: pick another name" (namestring directory)))
      (new-cell cell :session-id session-id))
    (bt2:with-lock-held (*layer-lock*)
      (let ((order (folder-load-order layer)))
        (dolist (path (directory (merge-pathnames "*.lisp" layer)))
          (let* ((keys (mapcar #'first (file-definitions path)))
                 (hits (remove-if-not (lambda (key) (member (getf key :name) wanted :test #'string=)) keys)))
            (when hits
              (dolist (key hits) (pushnew (getf key :name) found :test #'string=))
              (if (= (length hits) (length keys))
                  (push (cons path keys) moving)
                  (push path shared)))))
        ;; the cell loads them in the order the layer did: a DEFSTRUCT before its readers' callers
        (setf moving (sort moving #'< :key (lambda (move)
                                             (or (position (pathname-name (car move)) order :test #'string=)
                                                 most-positive-fixnum)))))
      (dolist (move moving)
        (when (probe-file (merge-pathnames (file-namestring (car move)) directory))
          (error "~a holds ~a already: nothing was moved" (folder-name directory) (file-namestring (car move)))))
      (when moving
        (let ((system (layer-system-file (folder-name directory)))
              (from '()) (to '()))
          (dolist (move moving)
            (let ((target (merge-pathnames (file-namestring (car move)) directory)))
              (rename-file (car move) target)
              (push (car move) from)
              (push target to)
              (write-layer-system system (folder-name directory) (pathname-name target))))
          (write-layer-system (layer-system-file) (layer-name))
          (setf labels (distinct (loop for move in moving append (mapcar #'definition-key-label (cdr move)))))
          (flet ((notice (line) (format t "~&[~a: ~a]~%" (folder-name directory) line)))
            (git-commit-all layer (format nil "moved ~{~a~^, ~} to ~a~@[ — session ~a~]"
                                          labels (folder-name directory) session-id)
                            :only (cons (layer-system-file) from) :notice #'notice)
            (git-commit-all directory (format nil "~{~a~^, ~}, moved from the layer~@[ — session ~a~]"
                                              labels session-id)
                            :only (cons system to) :notice #'notice)))))
    (dolist (path shared)
      (format t "~&[layer: ~a defines more than you named; left whole in the layer]~%" (file-namestring path)))
    (when-let (missing (set-difference wanted found :test #'string=))
      (format t "~&[layer: no layer file defines ~{~(~a~)~^, ~}]~%" missing))
    (when labels
      (format t "~&[layer: moved ~{~a~^, ~} to ~a; nothing live changed, the next boot loads them from there]~%"
              labels (folder-name directory)))
    labels))
