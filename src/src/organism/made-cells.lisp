;;;; made-cells.lisp --- the cells the organism makes when it is asked for
;;;; one: a folder of its own code beside the layer, written by talking.
;;;;
;;;; What the organism keeps is the layer's unless it says otherwise. A made
;;;; cell is a second folder of the same kind -- a LAYER system, one
;;;; definition per file, loaded from source at every boot (cells.lisp) --
;;;; under a name of its own, <identity>-NAME, so what one conversation built
;;;; can be read, kept in a repository of its own and shared on its own.
;;;; NEW-CELL writes one, loads it and makes it the session's current cell:
;;;; every definition the session keeps from then on is written there
;;;; (DEFINITION-FOLDER), and a name kept there leaves the layer, so no name
;;;; loads from two folders (RECORD-DEFINITIONS). A name a made cell holds
;;;; stays in it from any session, so a later fix lands beside the code it
;;;; fixes. The current cell is the session's until LEAVE-CELL; a restart
;;;; forgets it, and ENTER-CELL takes it up again.

(in-package #:nodecode.kernel)

(defvar *session-cells* (make-hash-table :test 'equal :synchronized t)
  "Session id -> the folder name of the made cell its kept code goes to.")

(defun cell-directory (name)
  "The made cell NAME's folder under *CELLS-DIRECTORY*: <identity>-NAME/, as
the organism's own knowledge cell is <identity>-memory. Nothing is created
here."
  (merge-pathnames (format nil "~a-~a/" (layer-name) name) *cells-directory*))

(defun made-cell-name (name)
  "NAME, a made cell's name, checked: lowercase letters and digits in words
joined by hyphens, and not `memory'. => NAME."
  (unless (and (stringp name) (ppcre:scan "^[a-z0-9]+(?:-[a-z0-9]+)*$" name))
    (error "a cell's name is lowercase letters, digits and hyphens, like \"tokyo-time\": not ~s" name))
  (when (string= name "memory")
    (error "memory names the organism's own knowledge cell; pick another name"))
  name)

(defun current-cell-directory (&optional (session-id *scribe-session-id*))
  "The folder of the made cell SESSION-ID keeps its code in, while it stands;
NIL when its code is the layer's."
  (when-let (folder (and *cells-directory* (gethash session-id *session-cells*)))
    (let ((directory (merge-pathnames (format nil "~a/" folder) *cells-directory*)))
      (and (eq (kept-folder-kind directory) :code) directory))))

(defun current-cell (&optional (session-id *scribe-session-id*))
  "The name of the made cell this session keeps its definitions in, or NIL
when they go to the layer."
  (when-let (directory (current-cell-directory session-id))
    (folder-label directory)))

(defun enter-cell (name &optional (session-id *scribe-session-id*))
  "Make the made cell NAME this session's current cell -- after a restart, or
from another conversation -- so what it keeps from now on is written there.
=> its folder."
  (let ((directory (cell-directory (made-cell-name name))))
    (unless (eq (kept-folder-kind directory) :code)
      (error "no cell ~a was made here: (new-cell ~s) makes it" name name))
    (setf (gethash session-id *session-cells*) (folder-name directory))
    directory))

(defun leave-cell (&optional (session-id *scribe-session-id*))
  "Stop keeping this session's definitions in its current cell: they go to
the layer again. => the cell's name, or NIL when there was none."
  (prog1 (current-cell session-id)
    (remhash session-id *session-cells*)))

(defun add-cell-component (directory file-name)
  "Put FILE-NAME, a file of the made cell DIRECTORY named without its .lisp,
at the end of the cell's .asd component list, the load order, unless it is
listed; a listed file that is gone leaves it. Takes *LAYER-LOCK*. => the
.asd's path."
  (let* ((name (folder-name directory))
         (path (layer-system-file name)))
    (bt2:with-lock-held (*layer-lock*)
      (write-layer-system path name file-name))
    path))

(defun new-cell (name &key description (session-id *scribe-session-id*))
  "Make the cell NAME, which DESCRIPTION says the use of: write its folder,
<identity>-NAME/ under *CELLS-DIRECTORY* (CELL-DIRECTORY), an .asd declaring
a LAYER system and a README; commit it as a repository of its own; load it;
and make it this session's current cell, so every definition the session
keeps from now on is written there and not to the layer. Refused when the
folder is there already: (enter-cell NAME) works on in a made one. => the
folder."
  (made-cell-name name)
  (unless (or (null description) (stringp description))
    (error "a cell's description is a string, not ~s" description))
  (cond ((null *cells-directory*) (error "this organism keeps nothing: it has no cells folder"))
        (*pristine* (error "pristine: nothing is kept; relaunch without --pristine")))
  (let* ((directory (cell-directory name))
         (system (folder-name directory))
         (description (or description "")))
    ;; a folder /cells would list under the same name -- one made by hand, a
    ;; shipped or hub cell's nodecode-NAME -- is a clash too
    (when-let (taken (find-if #'probe-file
                              (list directory
                                    (merge-pathnames (format nil "~a/" name) *cells-directory*)
                                    (merge-pathnames (format nil "nodecode-~a/" name) *cells-directory*))))
      (error "~a is there already~:[; pick another name~;, a cell this organism made: (enter-cell ~s) works on in it~]"
             (namestring taken) (and (eq taken directory) (eq (kept-folder-kind taken) :code)) name))
    (ensure-directories-exist directory)
    (write-system-file (layer-system-file system) system (layer-system-blurb system)
                       (fresh-layer-system system :description description))
    (write-file-atomically (merge-pathnames "cell.lisp" directory) (made-cell-package-text name system))
    (write-layer-system (layer-system-file system) system "cell")
    (write-file-atomically (merge-pathnames "README.md" directory)
                           (format nil "# ~a~%~@[~%~a~%~]~%A nodecode cell: its definitions one per file, loaded from ~
                                        source at every boot in the order ~a.asd lists them, cell.lisp ~
                                        first: the package ~(~a~) they read in.~%"
                                   name (and (plusp (length description)) description) system system))
    (git-commit-all directory (format nil "new cell ~a~@[ - session ~a~]" name session-id)
                    :notice (lambda (line) (format t "~&[~a: ~a]~%" name line)))
    (git-exclude-dot-directories directory)
    (load-cell system)
    (setf (gethash session-id *session-cells*) system)
    (format t "~&[~a: made ~a; what this session keeps goes there now, (leave-cell) for the layer. ~
               A snippet working on it opens with (in-package #:~(~a~)), (help :cells) says the rest]~%"
            name (namestring directory) system)
    directory))

(defun made-cell-package-text (name system)
  "cell.lisp of the made cell NAME, whose system is SYSTEM: the package its
definitions read in, named after the system as the loader asks, and the
peripheral mark, so a DEFINE-CELL kept beside it is a cell that starts."
  ;; The package uses the kernel and EVOLVED's exports (DEFINE-CELL, NOTICE,
  ;; ON-STOP ...), which clash with neither, so a snippet reads the same in
  ;; it as in EVOLVED; EVOLVED's internal names are written nle::name.
  (format nil ";;;; cell.lisp --- ~a: the package this cell's definitions read in.~%~
               ;;;;~%~
               ;;;; Written by new-cell. Every other file here is one definition, kept~%~
               ;;;; by a session working on the cell; a snippet doing that opens with~%~
               ;;;; (in-package #:~(~a~)). A (define-cell ~(~a~) ...) kept beside this~%~
               ;;;; declares its commands, tools and settings: (help :cells).~%~%~
               (defpackage #:~(~a~)~%  (:use #:cl #:nodecode.kernel #:nodecode.evolved))~%~%~
               (in-package #:~(~a~))~%~%~
               (define-peripheral ~(~a~) :not-running t)~%"
          name system name system system name))
