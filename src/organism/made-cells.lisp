;;;; made-cells.lisp --- the cells the organism makes when it is asked for
;;;; one: a folder of its own code beside the layer, written by talking.
;;;;
;;;; What the organism keeps is the layer's unless it says otherwise. A made
;;;; cell is a folder of its own beside the layer, <identity>-NAME, loaded
;;;; from source at every boot (cells.lisp), so what one conversation built
;;;; can be read, kept in a repository of its own and shared on its own. Its
;;;; files are its record. NEW-CELL writes cell.lisp (the package) and
;;;; NAME.lisp (the code) with an .asd listing the two, loads it and makes it
;;;; the session's current cell; with :SOURCE the folder lives in a
;;;; repository of the user's and cells/ links to it (LINKED-CELL-FOLDER).
;;;; The model writes the code with write-file and edit: LOAD-SOURCE makes
;;;; what changed live, takes a cut definition out of the image and commits
;;;; the folder (cells.lisp). An eval in that session is a try, live and
;;;; never written (CELL-SESSION-RECORD), and a name a made cell's file holds
;;;; is never copied into the layer from any session, so a later fix lands
;;;; beside the code it fixes. A cell made before this keeps each definition
;;;; in a file of its own until FOLD-CELL folds it into the two. The current
;;;; cell is the session's until LEAVE-CELL; a restart forgets it, and
;;;; ENTER-CELL takes it up again.

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

(defun cell-main-file (directory)
  "The made cell DIRECTORY's code file: the first its .asd lists after
cell.lisp, else NAME.lisp after the folder's name less the organism's own
prefix."
  (let* ((name (folder-name directory))
         (listed (ignore-errors
                  (loop for component in (getf (cddr (read-defsystem-form (layer-system-file name) name))
                                               :components)
                        when (and (file-component-p component) (string/= (second component) "cell"))
                          return (second component))))
         (prefix (format nil "~a-" (layer-name))))
    (merge-pathnames (format nil "~a.lisp" (or listed (if (uiop:string-prefix-p prefix name)
                                                          (subseq name (length prefix))
                                                          name)))
                     directory)))

(defun made-cell-holding (key)
  "The file of a made cell -- a folder of code the organism made, never the
layer -- that defines KEY, or NIL."
  (dolist (directory (definition-folders))
    (when (and (string/= (folder-name directory) (layer-name))
               (eq (kept-folder-kind directory) :code))
      (dolist (path (directory (merge-pathnames "*.lisp" directory)))
        (when (member key (mapcar #'first (file-definitions path)) :test #'equal)
          (return-from made-cell-holding path))))))

(defun cell-session-record (form session-id &aux (directory (current-cell-directory session-id)))
  "Whether FORM, evaluated in SESSION-ID, is code RECORD-DEFINITIONS leaves
unwritten because a made cell's files are its record. In a session working
in a made cell every code form is: the model writes the cell's code with
write-file or edit (LOAD-SOURCE makes what changed live and commits it), and
an eval there is a try -- live in this image, gone at the next boot -- so the
folder holds what was written and nothing else. In any session, so is a form
every definition of which a made cell's file holds: a fix lands in that file,
never beside it in the layer. => T, the eval told so in one line, when FORM
defines or forgets code and keeps no memory or skill; NIL otherwise."
  (let ((acts '()) (knowledge nil))
    (walk-toplevel-acts form (lambda (sub)
                               (if (knowledge-form-p sub) (setf knowledge t) (push sub acts))))
    (setf acts (nreverse acts))
    (unless (or (null acts) knowledge)
      (let* ((keys (mapcar #'definition-key acts))
             ;; outside a cell: the one file of a made cell holding every definition
             (held (and (null directory) (every #'identity keys)
                        (let ((files (remove-duplicates (mapcar #'made-cell-holding keys)
                                                        :key (lambda (path) (and path (namestring path)))
                                                        :test #'equal)))
                          (and (null (rest files)) (first files)))))
             (labels (loop for sub in acts
                           for key in keys
                           collect (if key
                                       (definition-key-label key)
                                       (format nil "~(~a~) ~(~a~)" (first sub)
                                               (or (name-designator-string (literal-designator (second sub))) ""))))))
        (when (or directory held)
          (format t "~&[~a: ~{~a~^, ~} ~:[is~;are~] live in this image and not kept: ~a with write-file or ~
                     edit, which makes it live and commits it]~%"
                  (folder-label (or directory (linked-cell-folder held) (uiop:pathname-directory-pathname held)))
                  labels (rest labels)
                  (if directory
                      (format nil "a cell's code is its files, so write it into ~a"
                              (sb-ext:native-namestring (cell-main-file directory)))
                      (format nil "~a holds ~:[it~;them~], so edit it there"
                              (sb-ext:native-namestring held) (rest labels))))
          t)))))

(defun linked-cell-folder (path &aux (root *cells-directory*))
  "The folder of the cells folder that holds PATH through a link -- a made
cell whose folder lives elsewhere (NEW-CELL :SOURCE) -- named as the cells
folder names it, cells/NAME/; NIL for a file no link there leads to."
  (when (and root (probe-file root))
    (let ((file (ignore-errors (namestring (truename path)))))
      (when file
        (dolist (entry (directory (merge-pathnames "*/" root) :resolve-symlinks nil))
          (let ((target (ignore-errors (namestring (truename entry)))))
            (when (and target (string/= target (namestring entry)) (uiop:string-prefix-p target file))
              (return entry))))))))

(defun commit-written-cell-file (path live &aux (session-id *scribe-session-id*))
  "Commit a made cell's folder after PATH, one of its files, was written:
what a write makes live is the cell's record, so the cell's history is its
writes. LIVE names what went live. => the short hash, \"\" when nothing
changed, or NIL when PATH is no made cell's."
  (let ((directory (or (linked-cell-folder path) (uiop:pathname-directory-pathname path))))
    (when (and *cells-directory*
               (eq (kept-folder-kind directory) :code)
               (string/= (folder-name directory) (layer-name))
               (equal (ignore-errors (truename (uiop:pathname-parent-directory-pathname directory)))
                      (ignore-errors (truename *cells-directory*))))
      (git-commit-all directory (format nil "~a~@[: ~{~a~^, ~}~]~@[ - session ~a~]"
                                        (file-namestring path) live session-id)
                      :notice (lambda (line) (format t "~&[~a: ~a]~%" (folder-label directory) line))))))

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

(defun list-written-cell-file (path)
  "When PATH, a Lisp file just written, sits in a folder of kept code under
*CELLS-DIRECTORY* -- the layer or a made cell -- whose .asd does not list it,
list it there, last: a file written by hand loads at the next start, as the
definitions kept beside it do. => the folder's name when it listed it."
  ;; Before this the write loaded the file live and the next start never
  ;; read it: the cell's command answered until a restart, then was gone.
  (let* ((directory (or (linked-cell-folder path) (uiop:pathname-directory-pathname path)))
         (name (folder-name directory))
         (system (and *cells-directory* (layer-system-file name))))
    (when (and (equal (pathname-type path) "lisp")
               *cells-directory*
               (equal (ignore-errors (truename (uiop:pathname-parent-directory-pathname directory)))
                      (ignore-errors (truename *cells-directory*)))
               (eq (kept-folder-kind directory) :code)
               (probe-file system)
               (not (search (format nil "(:file ~s)" (pathname-name path)) (read-text system))))
      (add-cell-component directory (pathname-name path))
      name)))

(defun new-cell (name &key description source (session-id *scribe-session-id*))
  "Make the cell NAME, which DESCRIPTION says the use of: write its folder,
<identity>-NAME/ under *CELLS-DIRECTORY* (CELL-DIRECTORY) -- cell.lisp, its
package, NAME.lisp, its code, an .asd declaring a LAYER system that lists
the two, and a README; commit it as a repository of its own; load it; and
make it this session's current cell, whose code is what is written into its
files: an eval there defines live and keeps nothing (CELL-SESSION-RECORD).
SOURCE, a directory that is absent or empty, is where the folder lives
instead -- a repository of the operator's -- and <identity>-NAME under
*CELLS-DIRECTORY* a link to it, as a shipped cell's is. Refused when the
folder is there already: (enter-cell NAME) works on in a made one. => the
folder, as the cells folder names it."
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
    (let ((home (if source (uiop:ensure-directory-pathname (merge-pathnames source)) directory)))
      (when (and source (or (uiop:directory-files home) (uiop:subdirectories home)))
        (error "~a holds files already: a cell's source starts in an empty folder" (namestring home)))
      (ensure-directories-exist home)
      (write-system-file (merge-pathnames (format nil "~a.asd" system) home) system (layer-system-blurb system)
                         (fresh-layer-system system :description description))
      (write-file-atomically (merge-pathnames "cell.lisp" home) (made-cell-package-text name system))
      (write-file-atomically (merge-pathnames (format nil "~a.lisp" name) home)
                             (format nil ";;;; ~a.lisp --- ~a~%;;;;~%;;;; The cell's code, in the package cell.lisp makes. Written as a file~%~
                                          ;;;; (write-file, edit, or any editor): a write makes what changed live~%~
                                          ;;;; and commits it. An eval defines live and keeps nothing.~%~%~
                                          (in-package #:~(~a~))~%"
                                     name (if (plusp (length description)) description name) system))
      (write-file-atomically (merge-pathnames "README.md" home)
                             (format nil "# ~a~%~@[~%~a~%~]~%A nodecode cell: cell.lisp is the package ~(~a~), ~a.lisp its code, ~
                                          loaded from source at every boot in the order ~a.asd lists them.~%"
                                     name (and (plusp (length description)) description) system name system))
      (when source
        (ensure-directories-exist *cells-directory*)
        (unless (make-symlink (truename home) (merge-pathnames system *cells-directory*))
          (error "this platform links no folder: make the cell without :source"))))
    (write-layer-system (layer-system-file system) system "cell")
    (write-layer-system (layer-system-file system) system name)
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
               ;;;; Written by new-cell. The cell's code is ~a.lisp beside it, in~%~
               ;;;; (in-package #:~(~a~)); a (define-cell ~(~a~) ...) there declares its~%~
               ;;;; commands, tools and settings: (help :cells).~%~%~
               (defpackage #:~(~a~)~%  (:use #:cl #:nodecode.kernel #:nodecode.evolved))~%~%~
               (in-package #:~(~a~))~%~%~
               (define-peripheral ~(~a~) :not-running t)~%"
          name name system name system system name))
;;; --- off, and on again ------------------------------------------------------------
;;; A folder the installer put in the cells folder goes when /cells turns it
;;; off: another copy is a click away. The operator's own -- a cell made here
;;; or written by hand -- holds the only copy of its work, so off parks it
;;; instead: the off marker (+OFF-MARKER+) goes into the folder, the boot
;;; passes over it (CELL-FOLDERS), /cells lists it off, and on takes the
;;; marker out and loads it again. Its code stays loaded in this image until a
;;; restart, as every cell's does. The layer the organism writes to and a
;;; knowledge cell, which loads nothing, are never parked.

(defun operator-folder (name &optional (root *cells-directory*))
  "The folder NAME under ROOT when it is a cell folder the installer did not
put there, else NIL."
  (let ((directory (and root (merge-pathnames (format nil "~a/" name) root))))
    (and directory (uiop:directory-exists-p directory) (asd-files directory)
         (not (installer-put-p name root))
         directory)))

(defun cell-parkable-p (name &optional (root *cells-directory*))
  "Whether /cells turns the folder NAME under ROOT off by parking it: the
operator's own folder (OPERATOR-FOLDER), neither the layer the organism
writes to nor a knowledge cell."
  (let ((directory (operator-folder name root)))
    (and directory
         (string/= name (layer-name))
         (not (eq (kept-folder-kind directory) :knowledge)))))

(defun cell-parked-p (name &optional (root *cells-directory*))
  "Whether the operator's own folder NAME under ROOT is parked: there, and
passed over by every boot until /cells turns it on."
  (let ((directory (operator-folder name root)))
    (and directory (folder-parked-p directory))))

(defun park-cell (name &optional (root *cells-directory*))
  "Turn the operator's own folder NAME off without touching its work: the off
marker in it, kept out of its repository's history, so the next boot passes
over it; a session whose current cell it was keeps to the layer again.
=> the folder. Refused for a folder CELL-PARKABLE-P refuses."
  (unless (cell-parkable-p name root)
    (error "~a is not a folder of your own /cells can turn off" name))
  (let ((directory (operator-folder name root)))
    (write-file-atomically (merge-pathnames +off-marker+ directory)
                           (format nil "Turned off in /cells. Its folder stays as it is; no boot loads it ~
                                        until /cells turns it on again.~%"))
    (git-exclude-line directory +off-marker+)
    (maphash (lambda (session folder)
               (when (string= folder name)
                 (remhash session *session-cells*)))
             *session-cells*)
    directory))

(defun unpark-cell (name &optional (root *cells-directory*))
  "Take the off marker out of the operator's own folder NAME, so the boot
loads it again. => the folder, or NIL when it was not parked."
  (let ((directory (operator-folder name root)))
    (when (and directory (folder-parked-p directory))
      (delete-file (merge-pathnames +off-marker+ directory))
      directory)))
