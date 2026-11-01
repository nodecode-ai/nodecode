;;;; scribe.lisp --- definitions: recognize them, read the shipped source,
;;;; write the layer. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The organism develops image-first: define through eval, exercise the
;;;; live definition, keep what works; keeping is implicit. Every
;;;; defining form the eval seam evaluates successfully is written into the
;;;; organism's own LAYER — ~/.nodecode/cells/<git-identity>/ — as ONE FILE
;;;; PER DEFINITION in the EVOLVED package, so the model's own bytes mean the
;;;; same thing in the file as they did in the eval, and committed to the
;;;; layer's git repository with the session in the message. There is no keep
;;;; verb, no pending list, no persist step: defining something IS making a
;;;; cell, and the folder loader (cells.lisp) loads the layer over the
;;;; shipped source at the next boot. Forgetting is the same act in reverse:
;;;; FMAKUNBOUND, MAKUNBOUND, UNHOOK, REMOVE-METHOD or UNINTERN evaluated as
;;;; a toplevel form — Lisp's own vocabulary, nothing new to learn — takes
;;;; the name out of the image, and the scribe takes its file out of the
;;;; layer and commits the drop; a name the release defines too gets the
;;;; release's definition back. Undo of a whole change is a generation:
;;;; (rollback N) and a restart (generation.lisp).
;;;;
;;;; One file per definition is not cosmetic: in Lisp the unit of change is
;;;; the toplevel definition, not the text hunk. It makes git's line diff
;;;; coincide with the semantic unit, gives per-definition log, blame and
;;;; revert, and lets two people's changes never touch the same file.
;;;;
;;;; The shipped source is read-only to the organism: humans
;;;; edit every file under src/src; the organism writes only its layer. The
;;;; read side here — DEFINITION-LOCATION, DEFINITION-TEXT — is how the
;;;; organism inspects itself on any machine, and the SHIPPED-DEFINITIONS
;;;; index is what tells an upgrade which overridden definitions changed
;;;; underneath a layer: each layer file records the hash of the shipped text
;;;; it overrode.
;;;;
;;;; Three pieces, one discipline:
;;;;
;;;;   - RECOGNITION: what counts as a defining form, and the DEFINITION-KEY
;;;;     that names one — operator, name, and for a method its qualifiers and
;;;;     specializers, since a DEFMETHOD displaces one method, not the generic.
;;;;   - The SCANNER (TOPLEVEL-DEFINITIONS) is the single owner of "where does
;;;;     this definition live in this text", over shipped files and layer
;;;;     files alike.
;;;;   - RECORD-DEFINITIONS is the eval-seam write: file, .asd, commit, fact.
;;;;     It never signals — a scribe defect must not turn a successful eval
;;;;     into a turn error — and says what went wrong on the eval's own output
;;;;     so the model learns at once that a definition did not persist.

(in-package #:nodecode.kernel)

;;; --- recognizing defining forms -------------------------------------------

(defparameter +scribe-whitespace+ '(#\Space #\Tab #\Newline #\Return #\Page)
  "What the scribe trims and skips between forms.")

;;; Matched BY SYMBOL NAME — the evolved package is rebuilt under test
;;; (CLEAR-EVOLVED-PACKAGE), so symbol identity goes stale where names do not.
;;; The DEFINE-* families match by prefix in DEFINITION-KEY; bare DEFINE is
;;; the definer of every Lisp that is not Common Lisp, and of the units below.
(defparameter *defining-operators*
  '("DEFUN" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT" "DEFMACRO" "DEFGENERIC"
    "DEFMETHOD" "DEFCLASS" "DEFSTRUCT" "DEFTYPE" "DEFPACKAGE" "DEFSETF"
    "DEFINE")
  "Operator names that define a toplevel name.")

;;; A PROGN is split into one file per definition; a unit is filed
;;; WHOLE, once under every name it defines, so each name can be found,
;;; forgotten and redefined on its own while every file that survives still
;;; loads the unit its sibling needed. Matched by symbol name like
;;; *DEFINING-OPERATORS*. A cell that brings such a language pushes its
;;; operator here when it loads.
(defparameter *unit-operators* '()
  "Operator names whose body forms are definitions that only mean something
together — a typed language's compilation unit, where a definition may
refer to a sibling not yet defined and a type is used by the function
beside it.")

(defun named-form-p (form name)
  "Whether FORM is a list whose operator is a symbol named NAME, in any package."
  (and (consp form) (symbolp (first form)) (string= name (symbol-name (first form)))))

(defun name-designator-string (raw)
  "RAW — a symbol, a (SETF symbol) function name, or anything else — as the
string the scribe keys on, or NIL when RAW names nothing it can file."
  (cond ((and raw (symbolp raw)) (symbol-name raw))
        ((and (named-form-p raw "SETF") (= 2 (length raw)) (second raw) (symbolp (second raw)))
         (format nil "(SETF ~a)" (symbol-name (second raw))))))

(defun head-element-string (element)
  "ELEMENT of a definition's head as identity text, package-free so the key
reads the same wherever the text is read back: a symbol by name, a list as
its elements in parens, anything else printed."
  (cond ((and element (symbolp element)) (symbol-name element))
        ((consp element)
         (format nil "(~{~a~^ ~})" (mapcar #'head-element-string element)))
        (t (princ-to-string element))))

(defun specializer-string (specializer)
  "A DEFMETHOD specializer as written, keyed as text: a class name, T, or
EQL-<printed object>."
  (if (named-form-p specializer "EQL")
      (let ((*package* (find-package '#:nodecode.evolved))
            (*print-case* :upcase))
        (format nil "EQL-~a" (prin1-to-string (second specializer))))
      (head-element-string specializer)))

(defun definition-key (form)
  "The key naming what FORM defines: (:operator OP :name NAME) plus, for a
DEFMETHOD, :qualifiers and :specializers, and for a head written as a list
the rest of the head as :qualifiers — all strings, so two
keys compare with EQUAL across package rebuilds."
  ;; NIL when FORM defines nothing the scribe files.
  (when (and (consp form) (first form) (symbolp (first form)))
    (let* ((operator (symbol-name (first form)))
           ;; DEFINE-PERIPHERAL's DEFINE-VERB is a DEFUN with the running guard in
           ;; front, keyed as one: a layer DEFUN of a verb overrides it by name.
           (operator (if (string= operator "DEFINE-VERB") "DEFUN" operator))
           ;; a symbol, a (SETF reader), or the first element of a list head
           (name (and (or (member operator *defining-operators* :test #'string=)
                          (and (> (length operator) 7) (uiop:string-prefix-p "DEFINE-" operator)))
                      (let ((raw (second form)))
                        (or (name-designator-string raw)
                            (and (consp raw) (first raw) (symbolp (first raw))
                                 (symbol-name (first raw))))))))
      (when name
        (list* :operator operator :name name
               (cond
                 ((string= operator "DEFMETHOD")
                  ;; the qualifier symbols' names, and each required parameter's
                  ;; specializer, T when unspecialized
                  (let* ((rest (cddr form))
                         (qualifiers (loop while (and rest (not (listp (first rest))))
                                           collect (symbol-name (pop rest)))))
                    (list :qualifiers qualifiers
                          :specializers (loop for parameter in (first rest)
                                              until (member parameter lambda-list-keywords)
                                              collect (if (consp parameter)
                                                          (specializer-string (second parameter))
                                                          "T")))))
                 ;; A hook's key plays a qualifier's role: it tells one of several
                 ;; definitions on the same name apart, so two hooks on one
                 ;; function get two files and a re-evaluation keeps its slot.
                 ((and (string= operator "DEFINE-HOOK") (stringp (third form)))
                  (list :qualifiers (list (third form))))
                 (t (let ((raw (second form)))
                      ;; DEFINE's (name arg...) and DEFSTRUCT's (name option...) only describe
                      (and (consp raw) (rest raw)
                           (not (name-designator-string raw))
                           (not (member operator '("DEFINE" "DEFSTRUCT") :test #'string=))
                           (list :qualifiers (mapcar #'head-element-string (rest raw))))))))))))

(defun floor-key-p (key &optional (package *package*))
  "Whether KEY, read in PACKAGE, would define a kernel-owned name — what the
scribe refuses to record: the name is floor material and PACKAGE inherits
the kernel's names, as EVOLVED does."
  ;; A DEFINE-HOOK's name is the function or point it advises, not one it
  ;; defines: advice on the floor is exactly what the primitive is for, and
  ;; the package lock is untouched by it. A definition read in a package that
  ;; does not see the kernel — a typed language's own, where EQ names its
  ;; class — touches no floor.
  (and (not (string= (getf key :operator) "DEFINE-HOOK"))
       (kernel-owned-name-p (getf key :name))
       (let ((package (find-package package))
             (kernel (find-package '#:nodecode.kernel)))
         (or (null package)
             (eq package kernel)
             (and (member kernel (package-use-list package)) t)))))

(defun literal-designator (form)
  "What FORM names when it is written as a literal — (QUOTE X) answers X, a
keyword answers itself (a hook point is written bare) — else NIL."
  ;; A name computed at run time is not an act the scribe can see, the same
  ;; asymmetry as a DEFUN built inside a LET.
  (cond ((and (named-form-p form "QUOTE") (= 2 (length form)))
         (second form))
        ((keywordp form) form)))

(defun walk-toplevel-acts (form function)
  "Call FUNCTION with each act inside FORM, in order: FORM itself when its
operator defines or forgets a name, each body form of a PROGN / EVAL-WHEN,
recursively, or each definition a unit (*UNIT-OPERATORS*) holds directly."
  ;; Order is the act's meaning — a PROGN that forgets a name and defines it
  ;; again is a rewrite, not a deletion.
  (when (and (consp form) (first form) (symbolp (first form)))
    (let ((operator (symbol-name (first form))))
      (cond ((member operator '("PROGN" "EVAL-WHEN") :test #'string=)
             (dolist (sub (if (string= operator "PROGN") (rest form) (cddr form)))
               (walk-toplevel-acts sub function)))
            ((member operator *unit-operators* :test #'string=)
             (dolist (sub (rest form))
               (when (definition-key sub) (funcall function sub))))
            ((or (definition-key form)
                 (member operator '("FMAKUNBOUND" "MAKUNBOUND" "UNHOOK" "REMOVE-METHOD" "UNINTERN")
                         :test #'string=))
             (funcall function form))))))

(defun definition-key-label (key)
  "KEY as the one-line human label: NAME, or NAME (QUALIFIERS) SPECIALIZERS
for a method."
  (format nil "~(~a~)~@[ ~(~{~a~^ ~}~)~]~@[ (~(~{~a~^ ~}~))~]"
          (getf key :name)
          (getf key :qualifiers)
          (getf key :specializers)))

(defun definition-file-name (key)
  "The layer file a definition lives in: its name downcased, with every
character outside [a-z0-9-] turned to an underscore — *SYSTEM-PROMPT* is
_system-prompt_, (SETF FOO) is setf-foo — and a method's qualifiers and
specializers joined on, so each method has its own file."
  ;; Deterministic and invertible enough that two different keys never share a
  ;; file in practice.
  (flet ((slug (text)
           (substitute-if-not #\_ (lambda (character)
                                    (or (alphanumericp character) (char= character #\-)))
                              (string-downcase text))))
    (format nil "~a~{-~a~}~{-~a~}"
            (slug (ppcre:regex-replace "^\\(SETF (.*)\\)$" (getf key :name) "setf-\\1"))
            (mapcar #'slug (getf key :qualifiers))
            (mapcar #'slug (getf key :specializers)))))

(defun definition-path (key directory)
  "The file in DIRECTORY a definition of KEY is kept in (DEFINITION-FILE-NAME)."
  (merge-pathnames (format nil "~a.lisp" (definition-file-name key)) directory))

;;; --- the scanner ----------------------------------------------------------

(defun scribe-skip-blank (text position end)
  "The index of the next character in TEXT at or after POSITION that is not
whitespace, a semicolon comment, or a (nested) block comment; END when none."
  ;; An unterminated block comment reads as blank to the end — the reader
  ;; proper is what signals on malformed input.
  (flet ((at-p (pair) (string= pair text :start2 position :end2 (min end (+ position 2)))))
    (loop
      (cond ((>= position end) (return end))
            ((member (char text position) +scribe-whitespace+) (incf position))
            ((char= (char text position) #\;)
             (let ((eol (position #\Newline text :start position :end end)))
               (setf position (if eol (1+ eol) end))))
            ((at-p "#\|")
             (incf position 2)
             (loop with depth = 1
                   while (plusp depth)
                   do (cond ((>= (1+ position) end)
                             (return-from scribe-skip-blank end))
                            ((at-p "|#") (decf depth) (incf position 2))
                            ((at-p "#\|") (incf depth) (incf position 2))
                            (t (incf position)))))
            (t (return position))))))

(defun toplevel-definitions (text &key name read-eval &aux found)
  "=> ((KEY START END PACKAGE) ...) in text order — KEY a DEFINITION-KEY,
START/END a character span of TEXT, PACKAGE the package the form reads in
(the package object, or the designator string when the text names one this
image lacks) — for every toplevel defining form, descending
into PROGN and EVAL-WHEN by reading sub-forms individually so each carries
its own span, and into a unit (*UNIT-OPERATORS*) whose every definition
spans the unit: the file that holds one holds them all."
  ;; NAME, when given, keeps only definitions of that name. The single owner
  ;; of "where does a definition live in this text".
  ;;
  ;; Tracks IN-PACKAGE forms so symbols read where the file says they live,
  ;; and carries that package on every entry: a file's definitions are
  ;; evaluated back into the image under the package the file itself declares
  ;; (LOAD-SOURCE). READ-EVAL stays NIL for image-origin text (untrusted-input
  ;; discipline — and the eval snippet reads with *READ-EVAL* NIL, so such text
  ;; cannot carry #. anyway); shipped source files pass T, because the scan
  ;; must read exactly what ASDF loads and the live tree already carries #.
  ;; (provider.lisp).
  ;;
  ;; A form that does not read in this image — its body names a package this
  ;; image lacks — still yields its key from the elements that read before the
  ;; failure (operator, name, a method's qualifiers and lambda list, a hook's
  ;; key all sit ahead of the body), spanning to the end of TEXT, and ends the
  ;; scan: without the form's end nothing after it can be placed. A layer file
  ;; is one definition, so that is the file; and forgetting it must not need
  ;; its body to read. Other errors propagate.
  (let ((*package* (find-package '#:nodecode.evolved))
        (*read-eval* (and read-eval t))
        (home (find-package '#:nodecode.evolved)))
    (labels
        ((read-at (start end)
           (read-from-string text t nil :start start :end end :preserve-whitespace t))
         (region-forms (start end)
           ;; => where the region stops: the `)' closing it, or END
           (loop
             (let ((form-start (scribe-skip-blank text start end)))
               (when (>= form-start end) (return end))
               (when (char= (char text form-start) #\)) (return form-start))
               (let ((members (members-start form-start end)))
                 (if members
                     ;; A PROGN or EVAL-WHEN is read member by member, never
                     ;; whole first: without that, a large PROGN would be read
                     ;; twice over.
                     (let ((close (region-forms members end)))
                       (when (>= close end) (return end))
                       (setf start (1+ close)))
                     (multiple-value-bind (form next)
                         (handler-case
                             (read-at form-start end)
                           ((or reader-error end-of-file) ()
                             (when-let (head (head-form form-start end))
                               (consider head form-start end))
                             (return end)))
                       (consider form form-start (min next end))
                       (setf start next)))))))
         (members-start (form-start end)
           ;; Where the members of the PROGN or EVAL-WHEN at FORM-START begin,
           ;; past its operator and EVAL-WHEN's situations; NIL for any other form.
           (when (char= (char text form-start) #\()
             (with-handlers (((or reader-error end-of-file) () nil))
               (multiple-value-bind (operator next)
                   (read-at (1+ form-start) end)
                 (when (and operator (symbolp operator))
                   (cond ((string= (symbol-name operator) "PROGN") next)
                         ((string= (symbol-name operator) "EVAL-WHEN")
                          (nth-value 1 (read-at next end)))))))))
         (head-form (form-start end)
           ;; The elements of the list form at FORM-START that read, up to
           ;; the first that does not. NIL when the form does not open with
           ;; a paren or nothing reads.
           (when (char= (char text form-start) #\()
             (let ((position (1+ form-start)))
               (loop for element-start = (scribe-skip-blank text position end)
                     until (or (>= element-start end)
                               (char= (char text element-start) #\)))
                     collect (multiple-value-bind (element next)
                                 (with-handlers (((or reader-error end-of-file) () (loop-finish)))
                                   (read-at element-start end))
                               (setf position next)
                               element)))))
         (consider (form start end)
           (when (and (consp form) (first form) (symbolp (first form)))
             (let ((operator (symbol-name (first form))))
               (cond
                 ((string= operator "IN-PACKAGE")
                  ;; HOME follows the file's own declaration even when this
                  ;; image lacks the package: the reader stays where it was
                  ;; (a missing package cannot be read in), and the load side
                  ;; refuses the entries by name instead of defining them
                  ;; somewhere they do not belong.
                  (let* ((designator (and (rest form) (second form)))
                         (package (and designator
                                       (ignore-errors (find-package designator)))))
                    (cond (package (setf *package* package home package))
                          (designator (setf home (string designator))))))
                 ((member operator *unit-operators* :test #'string=)
                  (dolist (sub (rest form))
                    (note (definition-key sub) start end)))
                 (t (note (definition-key form) start end))))))
         (note (key start end)
           ;; One definition found at [START, END), kept when NAME allows it.
           (when (and key (or (null name) (string= (getf key :name) name)))
             (push (list key start end home) found))))
      (region-forms 0 (length text))
      (nreverse found))))

(defun file-definitions (path &key name read-eval)
  "TOPLEVEL-DEFINITIONS over PATH's text, or NIL when the file cannot be read
or does not scan: one owner for the rule that a walk over a tree of
definitions must not go blind over one broken file."
  ;; NAME and READ-EVAL are the scanner's own.
  (ignore-errors (toplevel-definitions (uiop:read-file-string path)
                                       :name name :read-eval read-eval)))

;;; --- the read side: the shipped source --------------------------------------

;;; Built on first use and kept for the process: shipped source does not
;;; change under a running image, and one full read of the tree is what it
;;; costs to answer "what did this layer override" at every splice and every
;;; boot.
(defvar *shipped-definitions* nil
  "(ROOT . TABLE): the index of every definition in the shipped source under
ROOT — NAME string -> ((KEY PATH START END) ...) in file order.")

(defun shipped-definitions (&optional (root *source-root*) &aux (memo *shipped-definitions*))
  "The shipped-definition index for ROOT (see *SHIPPED-DEFINITIONS*), or an
empty table when no source travels with this image."
  ;; A file that does not read is skipped: the organism must not go blind over
  ;; one broken file.
  (cond ((null root) (make-hash-table :test 'equal))
        ((equal (car memo) root) (cdr memo))
        (t (let ((table (make-hash-table :test 'equal)))
             (dolist (path (sort (directory (merge-pathnames "**/*.lisp" root))
                                 #'string< :key #'namestring))
               (dolist (hit (file-definitions path :read-eval t))
                 (alexandria:appendf (gethash (getf (first hit) :name) table)
                                     (list (list (first hit) path (second hit) (third hit))))))
             (setf *shipped-definitions* (cons root table))
             table))))

(defun forget-shipped-definitions ()
  "Drop the index so the next read rebuilds it — after the shipped tree was
edited by a human while this image runs, or between tests."
  (setf *shipped-definitions* nil))

(defun definition-name-key (name)
  "NAME as an index key: a symbol's name, or a string upcased."
  (etypecase name
    (symbol (symbol-name name))
    (string (string-upcase name))))

(defun definition-key-matches-p (shipped key)
  "Whether the definition key SHIPPED is KEY's: the same operator, and for
a method the same signature."
  ;; A name-only key (a DEFUN) matches by name and operator; a method key
  ;; matches its signature too.
  (and (string= (getf shipped :operator) (getf key :operator))
       (equal (getf shipped :qualifiers) (getf key :qualifiers))
       (equal (getf shipped :specializers) (getf key :specializers))))

(defun shipped-definition (key)
  "The shipped residence matching KEY exactly — (KEY PATH START END) — or
NIL."
  (find-if (lambda (hit) (definition-key-matches-p (first hit) key))
           (gethash (getf key :name) (shipped-definitions))))

(defun shipped-definition-in (relative key &optional (root *source-root*))
  "KEY's shipped residence in the one file RELATIVE names under ROOT —
(KEY PATH START END) — or NIL when that file is gone or holds no such
definition."
  ;; The stale-override check recorded which file it read, so that file is
  ;; scanned alone before the whole-tree index is built: without this, the
  ;; index would read the shipped tree on every boot that carries an
  ;; override. NIL sends the caller on to the index, which still answers a
  ;; definition that moved file since it was recorded.
  (bind ((path (and root (probe-file (merge-pathnames relative root))))
         (hit (and path (find key (file-definitions path :read-eval t :name (getf key :name))
                              :key #'first :test #'definition-key-matches-p))))
    (and hit (list (first hit) path (second hit) (third hit)))))

(defun definition-location (name &aux (key (definition-name-key name)))
  "Where NAME's definitions live: ((PATH START END) ...), the layer's own
file first when the layer carries one — that is what is live — then every
shipped residence in file order."
  ;; NIL when NAME is defined nowhere on disk: an eval that was never a
  ;; defining form, or a name the image has not defined at all.
  (append (layer-files-for-name key) (mapcar #'rest (gethash key (shipped-definitions)))))

(defun definition-text (name &aux (key (definition-name-key name))
                                  (found (or (layer-files-for-name key)
                                             (mapcar #'rest (gethash key (shipped-definitions))))))
  "The text of NAME's definition as it is on disk: the layer's when the
layer carries exactly one definition of NAME, else the single shipped
residence, else NIL — several residences (methods) are disambiguated by
reading DEFINITION-LOCATION."
  (when (and found (null (rest found)))
    (destructuring-bind (path start end) (first found)
      (subseq (uiop:read-file-string path) start end))))

(defun definition-prose (name)
  "The manual past NAME's contract sentence: the comment lines right after its docstring."
  ;; A definition keeps its first sentence as the docstring and the rest of
  ;; its prose as ;; lines inside the form, so DEFINITION-TEXT still carries
  ;; every word and HELP prints them. NIL when the definition or its
  ;; docstring cannot be found, or when nothing follows the docstring.
  (when-let (text (definition-text name))
    (let* ((doc (or (documentation name 'function) (documentation name 'variable)))
           (quoted (and doc (let ((*print-pretty* nil)) (prin1-to-string doc))))
           (at (and quoted (search quoted text)))
           (prose (loop for line in (and at (rest (lines (subseq text (+ at (length quoted))))))
                        while (ppcre:scan "^ *;;(?!;)" line)
                        collect (ppcre:regex-replace "^ *;; ?" line ""))))
      (when prose (format nil "~{~a~^~%~}" prose)))))

;;; --- the layer -------------------------------------------------------------
;;; ~/.nodecode/cells/<identity>/: the organism's own folder, a LAYER system
;;; (cells.lisp) and a git repository. Created on first need.

;;; Resolved once per process on first need; tests bind it.
(defvar *layer-name* nil
  "The layer folder's name: the operator's git identity, slugged.")

(defvar *layer-lock* (bt2:make-lock :name "layer")
  "Splices serialize: background evals record from their own threads, and
the .asd rewrite is a read-modify-write.")

(defun layer-name ()
  (or *layer-name*
      (setf *layer-name*
            ;; git config user.name, or the login, or "you": a cloned folder keeps its author's name
            (let* ((user (ignore-errors
                          (string-trim '(#\Space #\Newline)
                                       (run-bounded
                                        (list *git-program* "config" "--get" "user.name")
                                        :seconds 10 :error-output nil))))
                   ;; slugged: each run outside [a-z0-9-] one hyphen, trimmed ("Mike Smith" is mike-smith)
                   (login (if (plusp (length user)) user (or (uiop:getenv "USER") "you")))
                   (name (string-trim "-" (ppcre:regex-replace-all "(?:[^\\w-]|_)+"
                                                                   (string-downcase login) "-"))))
              (if (plusp (length name)) name "you")))))

(defun layer-directory (&optional (name (layer-name)))
  "The layer folder under *CELLS-DIRECTORY*, a directory pathname."
  ;; Nothing is created here.
  (merge-pathnames (format nil "~a/" name) *cells-directory*))

(defun layer-system-file (&optional (name (layer-name)))
  (merge-pathnames (format nil "~a.asd" name) (layer-directory name)))

(defun layer-files-for-name (name &aux (directory (and *cells-directory* (layer-directory))))
  "((PATH START END) ...) for every file of the layer whose definition is
NAME — the scanner over each file, so a method file and a function file of
one name both answer — then of every knowledge cell, as the index read it."
  ;; A DEFINE-HOOK file on NAME does not: it advises the name, it does not
  ;; define it, and a definition read must keep answering the body the advice
  ;; wraps. A knowledge cell is one file per definition, read through the
  ;; index's memo rather than scanned whole at every call.
  (append
   (when (and directory (probe-file directory))
     (loop for path in (sort (directory (merge-pathnames "*.lisp" directory))
                             #'string< :key #'namestring)
           nconc (loop for hit in (file-definitions path :name name)
                       unless (string= "DEFINE-HOOK" (getf (first hit) :operator))
                         collect (list path (second hit) (third hit)))))
   (when directory
     (loop for entry in (knowledge-entries)
           when (and (string= name (getf (knowledge-entry-key entry) :name))
                     (string/= (layer-name) (folder-name (knowledge-entry-folder entry))))
             collect (list (knowledge-entry-path entry) (knowledge-entry-start entry)
                           (knowledge-entry-end entry))))))

(defun forgotten-layer-files (form directory &key (paths :all) &aux files shared)
  "=> (values FILES SHARED): the layer files a forgetting FORM takes out of
DIRECTORY — ((PATH . KEYS) ...), every definition of each such file matched
by FORM's filter, or the file named for a matched definition with the
definitions the act takes from it — and SHARED, the files FORM reaches only
partly: a human's file holding several definitions, a unit's file named
for a sibling, which the scribe leaves whole rather than drop a sibling
definition with the forgotten one. PATHS, when given, are the only files
looked at: a knowledge cell's one file per name."
  (let ((filter
          ;; FORM's literal act as a predicate over keys; a DEFINE-HOOK file is UNHOOK's alone
          (let ((name (name-designator-string (literal-designator (second form)))))
            (flet ((family (operators)
                     (and name
                          (lambda (key)
                            (and (string= name (getf key :name))
                                 (member (getf key :operator) operators
                                         :test #'string=))))))
              (dispatch (symbol-name (first form)) string=
                ("FMAKUNBOUND" (family '("DEFUN" "DEFINE" "DEFMACRO" "DEFGENERIC" "DEFMETHOD"
                                         "DEFINE-SKILL")))
                ("MAKUNBOUND" (family '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT")))
                ("UNINTERN" (and name (lambda (key)
                                        (and (string= name (getf key :name))
                                             (not (string= "DEFINE-HOOK" (getf key :operator)))))))
                ("UNHOOK" (let ((hook-key (third form)))
                            (and name (stringp hook-key)
                                 (lambda (key)
                                   (and (string= "DEFINE-HOOK" (getf key :operator))
                                        (string= name (getf key :name))
                                        (equal (list hook-key) (getf key :qualifiers)))))))
                ("REMOVE-METHOD" (let ((finder (third form)))
                                   (when (and (named-form-p finder "FIND-METHOD")
                                              (= 4 (length finder))
                                              (named-form-p (second finder) "FUNCTION"))
                                     (let ((gf (name-designator-string (second (second finder))))
                                           (qualifiers (literal-designator (third finder)))
                                           (specializers (literal-designator (fourth finder))))
                                       (and gf (listp qualifiers) (listp specializers)
                                            (every #'symbolp qualifiers)
                                            (lambda (key)
                                              (and (string= "DEFMETHOD" (getf key :operator))
                                                   (string= gf (getf key :name))
                                                   (equal (mapcar #'symbol-name qualifiers)
                                                          (getf key :qualifiers))
                                                   (equal (mapcar #'specializer-string specializers)
                                                          (getf key :specializers))))))))))))))
    (when filter
      ;; Every file and the keys the scanner finds in it, in name order. A file
      ;; that does not read contributes nothing -- the loader names it when it
      ;; fails to load.
      (loop for path in (if (eq paths :all)
                            (and (probe-file directory)
                                 (sort (directory (merge-pathnames "*.lisp" directory))
                                       #'string< :key #'namestring))
                            (remove-if-not #'probe-file paths))
            for keys = (mapcar #'first (file-definitions path))
            for matched = (remove-if-not filter keys)
            when matched
              do (cond ((= (length matched) (length keys))
                        (push (cons path keys) files))
                       ;; A unit's file is named for one of its definitions and
                       ;; holds its siblings beside it: forgetting that name
                       ;; takes the file, and the siblings keep their own.
                       ((member (pathname-name path) matched
                                :key #'definition-file-name :test #'string=)
                        (push (cons path matched) files))
                       (t (push path shared)))))
    (values (nreverse files) (nreverse shared))))

;;; --- writing one definition --------------------------------------------------

;;; The loader reads it back to skip, and name, the overrides the shipped
;;; source has moved under since they were taken.
(defparameter +override-marker+ ";;; overrides "
  "The header line a layer file carries when its definition displaces a
shipped one: `;;; overrides tui/ui.lisp@<sha256 prefix of the shipped
text>`.")

(defun shipped-text-digest (hit)
  ;; the text of a shipped residence HIT — (KEY PATH START END) — as it reads now
  (destructuring-bind (path start end) (rest hit)
    (subseq (sha256-text (subseq (uiop:read-file-string path) start end)) 0 12)))

(defun write-file-atomically (path content &key mode directory-mode (parents t) &aux fd renamed)
  "CONTENT — a string, the octets one encodes to, or a function writing them
to the octet stream it is handed — to PATH through a temp claimed beside it,
synced, and one rename, so a crash mid-write, or off Windows a power cut, leaves
the old file or none, never a torn one.
PATH's directory is made first, unless PARENTS is NIL and a missing one is an
error. MODE is the mode the file carries across the rename: an integer,
:KEEP for the mode PATH already has, since adopting a fresh temp file's mode
would quietly widen an operator's 0600, or :PRIVATE for a file that holds
secrets: PATH's own owner bits and none for the group or others, 0600 when
PATH is new. DIRECTORY-MODE is the directory's mode."
  ;; The temp is claimed exclusively (CLAIM-TEMP-BESIDE), so a name already
  ;; taken — by a file, or by a link waiting to be followed — is a retry, never
  ;; a write through it; whichever way the write ends, the temp is gone.
  ;; Windows walks `/' as it walks `\', and the temp's directory is PATH's
  ;; last `/' (PATH-PARENT).
  (let* ((native (if (stringp path) path (sb-ext:native-namestring path)))
         #+win32 (native (substitute #\/ #\\ native))
         (bits (case mode
                 (:keep (ignore-errors (logand (sb-posix:stat-mode (sb-posix:stat native)) #o7777)))
                 (:private (logand #o700 (or (ignore-errors (sb-posix:stat-mode (sb-posix:stat native)))
                                             #o600)))
                 (t mode)))
         (temp (flet ((claim ()
                        (claim-temp-beside
                         native (lambda (temp)
                                  (setf fd (sb-posix:open temp (logior sb-posix:o-creat sb-posix:o-excl
                                                                       sb-posix:o-wronly)
                                                          #o666))))))
                 ;; The directory is made when the claim finds none: made first
                 ;; every time, its every parent was probed for each file.
                 (prog1 (with-handlers ((sb-posix:syscall-error (condition)
                                          (unless (and parents (= (sb-posix:syscall-errno condition)
                                                                  sb-posix:enoent))
                                            (error condition))
                                          (ensure-directories-exist path)
                                          (claim)))
                          (claim))
                   (when directory-mode
                     (ignore-errors (sb-posix:chmod (path-parent native) directory-mode)))))))
    (with-cleanup ((unless renamed (ignore-errors (sb-posix:unlink temp))))
      ;; Closing the stream closes FD, so the bits are set while it is open.
      ;; On Windows an fd-stream is over a HANDLE, never a C runtime
      ;; descriptor. Without reopening by name, writes over FD would fail
      ;; with "the handle is invalid" and no Windows build could dump; so the
      ;; temp, claimed and nobody else's, is reopened by name.
      (with-open-stream (out #-win32 (sb-sys:make-fd-stream fd :output t :buffering :full
                                                                :element-type '(unsigned-byte 8))
                             #+win32 (progn (sb-posix:close fd)
                                            (open (sb-ext:parse-native-namestring temp)
                                                  :direction :output :if-exists :overwrite
                                                  :element-type '(unsigned-byte 8))))
        (cond ((functionp content) (funcall content out))
              ((stringp content)
               (write-sequence (sb-ext:string-to-octets content :external-format :utf-8) out))
              (t (write-sequence content out)))
        (finish-output out)
        ;; FCHMOD holds the descriptor; SBCL's sb-posix has none on Windows,
        ;; where the bits are set by name on a temp nothing else can see yet.
        (when bits
          #-win32 (sb-posix:fchmod fd bits)
          #+win32 (sb-posix:chmod temp bits))
        ;; The bytes on the disk before the name moves: unsynced, a power cut
        ;; could keep the rename and lose the data, leaving an empty file in
        ;; the old one's place -- auth.json, a secret, the config. Not on
        ;; Windows: OUT there is over a HANDLE, and FLUSH-FILE asks the C
        ;; runtime for the handle of a descriptor.
        #-win32 (flush-file out))
      ;; rename(2), MoveFileEx with replacement on Windows (REPLACE-FILE's call)
      (multiple-value-bind (ok errno) (sb-unix:unix-rename temp native)
        (unless ok (error "cannot replace ~a: ~a" native (sb-int:strerror errno))))
      (setf renamed t))
    path))

(defun file-component-p (component)
  "Whether COMPONENT, an .asd's component read as data, is (:file NAME)."
  (and (named-form-p component "FILE") (stringp (second component))))

(defun read-defsystem-form (path name)
  "The DEFSYSTEM form declaring NAME in PATH, read as ASDF reads it — in the
ASDF-USER package, no read-time evaluation — and never evaluated: what the
folder loader asks a .asd before anything loads, and what the scribe
rewrites."
  ;; NIL when PATH is absent or declares no such system; a file that does not
  ;; read signals.
  (when (probe-file path)
    (find-if (lambda (form)
               (and (named-form-p form "DEFSYSTEM")
                    (typep (rest form) '(cons (or string symbol)))
                    (string-equal name (string (second form)))))
             (let ((*package* (find-package :asdf-user)) (*read-eval* nil))
               (uiop:read-file-forms path)))))

(defun fresh-layer-system (name &key knowledge &aux (*package* (find-package :asdf-user)))
  ;; A knowledge cell is a layer that says so in :properties and lists no
  ;; component: the loader records it and never loads it, and a build from
  ;; before knowledge cells loads an empty layer, quietly.
  (read-from-string
   (if knowledge
       (format nil "(defsystem ~s :description ~s :class nodecode.kernel:layer :depends-on (\"nodecode\") :properties ((:knowledge . t)) :components ())"
               name
               (format nil "~a: memories and skills, one definition per file, read by the index and never loaded." name))
       (format nil "(defsystem ~s :description ~s :class nodecode.kernel:layer :depends-on (\"nodecode\") :serial t :components ())"
               name
               (format nil "~a's layer: the organism's own definitions, one per file, over the shipped source." name)))))

(defun knowledge-system-file (directory)
  (merge-pathnames (format nil "~a.asd" (folder-name directory)) directory))

(defun write-system-file (path name blurb form &aux (*package* (find-package :asdf-user))
                                                    (*print-case* :downcase)
                                                    (*print-pretty* t)
                                                    (*print-right-margin* 78))
  "PATH, NAME's .asd: a header line saying BLURB, a FORMAT control string that
takes no arguments, then FORM printed as ASDF reads it."
  (write-file-atomically path (format nil ";;;; ~a.asd --- ~?~%~%~s~%" name blurb nil form)))

(defun ensure-knowledge-system (directory &aux (path (knowledge-system-file directory))
                                               (name (folder-name directory)))
  "DIRECTORY's knowledge .asd, written when it has none and left as it is
otherwise: it lists nothing, so a keep never changes it. => its path."
  (unless (probe-file path)
    (write-system-file path name "a knowledge cell: what the organism keeps and does not run, ~
                                  one definition per file. The index reads it; nothing loads it."
                       (fresh-layer-system name :knowledge t)))
  path)

(defun write-layer-system (path name &optional file-name)
  "Regenerate the layer's .asd under NAME from what the folder holds: every
component whose file is gone is pruned — a file deleted by hand or
forgotten at the seam leaves no entry behind to fail the next boot — and
FILE-NAME, when given, is added if absent."
  ;; Everything else a human may have written is kept, and so is the component
  ;; order: a redefinition keeps its slot, a new definition appends, and the
  ;; list stays the load order. What a layer IS is not an option: the LAYER
  ;; class and the dependency on the core are asserted on every write, so an
  ;; .asd from before the class existed becomes a layer at the next definition
  ;; instead of loading as a compiled library forever. Rewrites the file.
  (let* ((form (or (read-defsystem-form path name) (fresh-layer-system name)))
         (plist (cddr form))
         (fresh (cddr (fresh-layer-system name)))
         (directory (make-pathname :name nil :type nil :defaults path))
         ;; A component stays when it names a file that exists under DIRECTORY.
         ;; Anything that is not a (:file NAME) entry is taken as present: the
         ;; scribe prunes only what it can see missing.
         (components (remove-if-not
                      (lambda (component)
                        (or (not (file-component-p component))
                            (probe-file (merge-pathnames (format nil "~a.lisp" (second component))
                                                         directory))))
                      (getf plist :components)))
         (present (find file-name components :test #'equal
                                             :key (lambda (c) (and (consp c) (second c))))))
    (setf (getf plist :class) (getf fresh :class)
          (getf plist :depends-on) (getf fresh :depends-on)
          (getf plist :components) (append components (and file-name (not present)
                                                             (list (list :file file-name)))))
    (write-system-file path name "the organism's own layer. Generated at each definition; ~
                                  edit freely, the component list is the load order."
                       (list* (first form) (second form) plist))))

(defun git-in-layer (directory &rest args)
  "Run git ARGS inside DIRECTORY."
  ;; => (values OUTPUT EXIT-CODE STDERR): the
  ;; stdout and stderr without their final newlines, and git's exit code. A
  ;; git that could not run at all, or ran past two minutes and was killed,
  ;; answers the reason on both streams and -1, so a caller that reports
  ;; either one has it.
  (flet ((stripped (text) (string-right-trim '(#\Newline #\Return) text))
         (failed (text) (values text -1 text)))
    (with-handlers ((error (condition) (failed (princ-to-string condition))))
      (multiple-value-bind (out err code)
          (run-bounded (list* *git-program* "-C" (namestring directory) args) :seconds 120)
        (if (eq code :timeout)
            (failed (format nil "git ~{~a~^ ~} ran past 120 s and was killed" args))
            (values (stripped out) code (stripped err)))))))

(defun git-exclude-dot-directories (directory &aux (git (merge-pathnames ".git/" directory)))
  "DIRECTORY's .git/info/exclude keeps every dot-directory out of its
history -- another cell's ledger, an editor's state -- the line appended
once and never twice. Nothing without a repository. Best effort."
  (when (probe-file git)
    (ignore-errors
     (let* ((path (merge-pathnames "info/exclude" git))
            (text (or (read-text path) "")))
       ;; the line after TEXT's last, on a line of its own (~&)
       (unless (search ".*/" text)
         (write-file-atomically path (format nil "~a~&.*/~%" text) :mode :keep))))))

(defun git-commit-all (directory message &key notice (only nil only-p))
  "Stage DIRECTORY whole, or ONLY the files a caller wrote or deleted in it,
and commit it as MESSAGE, making the repository on first need."
  ;; => the short hash, "" when nothing changed, or NIL when git could not
  ;; (NOTICE, a function of one line naming the step and git's words, is told).
  ;; Of ONLY, a file that is gone and git never tracked is nothing to stage
  ;; (UPDATE-INDEX --REMOVE passes it by).
  (flet ((run (&rest args)
           (multiple-value-bind (out code err) (apply #'git-in-layer directory args)
             (unless (zerop code)
               (when notice
                 (funcall notice (format nil "git ~a in ~a: ~a"
                                         (first args) (namestring directory)
                                         (if (plusp (length err)) err out))))
               (return-from git-commit-all nil))
             out)))
    (unless (probe-file (merge-pathnames ".git/" directory))
      (run "init" "-q")
      (git-exclude-dot-directories directory))
    (let* ((names (mapcar #'file-namestring only))
           (ours (string-set names)))
      (unless (and only-p (null names))
        ;; Without writing objects by eight processes at once, a keep of
        ;; hundreds would wait on git deflating and writing each object in
        ;; turn; written first, the index finds them there.
        ;; An operator's uncommitted edits ride along, unless ONLY names the files the
        ;; commit takes; a git that cannot run costs the caller nothing but the commit,
        ;; the files being written already.
        (when (> (length names) 256)
          (let* ((present (string-set (directory-names (sb-ext:native-namestring directory))))
                 (blobs (remove-if-not (lambda (name) (gethash name present)) names))
                 (size (ceiling (length blobs) 8)))
            (parallel-map (lambda (chunk)
                            (apply #'git-in-layer directory "hash-object" "-w" "--" chunk))
                          (loop for rest = blobs then (nthcdr size rest)
                                while rest
                                collect (subseq rest 0 (min size (length rest))))
                          :lanes 8)))
        ;; ONLY's files as they stand, by name: each a pathspec of `add'.
        (if only-p
            (apply #'run "update-index" "--add" "--remove" "--" names)
            (run "add" "-A")))
      ;; Named, a commit takes ONLY's files from the tree again, hashing each
      ;; twice. Unnamed when nothing else waits in the index, which is then the
      ;; same commit.
      (let* ((staged (and (not (and only-p (null names)))
                          (remove "" (uiop:split-string (run "diff" "--cached" "--name-only" "-z")
                                                        :separator (string (code-char 0)))
                                  :test #'string=)))
             (mine (if only-p (remove-if-not (lambda (path) (gethash path ours)) staged) staged)))
        (cond ((null mine) "")
              (t (apply #'run (append (git-identity directory)
                                      (list "commit" "-q" "-m" message "--")
                                      (and (/= (length mine) (length staged)) mine)))
                 (run "rev-parse" "--short" "HEAD")))))))

(defun git-identity (directory)
  "The arguments a git that writes a commit in DIRECTORY takes: nodecode's
name and address when the repository and the operator's config name none."
  (unless (plusp (length (git-in-layer directory "config" "--get" "user.email")))
    '("-c" "user.name=nodecode" "-c" "user.email=nodecode@localhost")))

(defun git-head-sha (directory &aux (git (merge-pathnames ".git/" directory)))
  "The commit DIRECTORY's git HEAD names, read from the repository's own
files — HEAD, the ref it points at, packed-refs — so the answer costs a few
file reads and no git process: this is asked on the shell's first frame,
where forking git was 65 ms of the operator's wait."
  ;; NIL when there is no repository or no commit yet.
  (flet ((first-line (path)
           (when-let (line (ignore-errors (uiop:read-file-line path)))
             (string-trim '(#\Space #\Tab #\Return) line))))
    (let* ((head (first-line (merge-pathnames "HEAD" git)))
           (sha (cond ((uiop:string-prefix-p "ref: " head)
                       (let ((ref (string-trim '(#\Space) (subseq head 5))))
                         (or (first-line (merge-pathnames ref git))
                             ;; packed-refs: `<sha> <ref>' lines, `#' and peeled `^' lines skipped
                             (loop for line in (ignore-errors
                                                (uiop:read-file-lines
                                                 (merge-pathnames "packed-refs" git)))
                                   for space = (position #\Space line)
                                   when (and space (not (member (char line 0) '(#\# #\^)))
                                             (string= ref (string-right-trim '(#\Return) line)
                                                      :start2 (1+ space)))
                                     return (subseq line 0 space)))))
                      (t head))))
      (and (stringp sha)
           (ppcre:scan "^(?:[0-9a-fA-F]{40}|[0-9a-fA-F]{64})\\z" sha)
           sha))))

(defvar *layer-revision* nil
  "The layer's git HEAD as last observed — read once per process on first
need, advanced by every commit RECORD-DEFINITIONS lands. NIL until asked.")

(defun forget-scribe-memos ()
  "Drop what this file memoized about the machine — the shipped-definition
index, the layer name, the observed revision — so a dumped image (the
release build, a warm launch image) re-reads them where it wakes instead
of carrying the dumping machine's answers."
  (forget-shipped-definitions)
  (setf *layer-name* nil
        *layer-revision* nil))

(pushnew 'forget-scribe-memos sb-ext:*save-hooks*)

(defvar *scribe-session-id* nil
  "The session whose evaluation is currently running, bound by the eval snippet
(EVAL-FORM-PRINTED) around each form: the layer commit names it, and the
durable fact is attributed to it.")

;;; --- definitions the layer has taken ---------------------------------------
;;; Every keep stamps its name here: the objects the layer now carries and the
;;; moment, counted. A form's window (DEFINITIONS-BEFORE) reads the count, and
;;; REPORT-UNKEPT-CHANGES subtracts names taken after it -- the image runs many
;;; forms at once (lanes, forks, background evals). Without the subtraction, a
;;; definition another form keeps while this one runs would be reported as this
;;; form's unkept change.
(defvar *kept-definitions* (make-hash-table :test #'eq)
  "Symbol -> plist (:SEQ N :FUNCTION F :SETF SF :METHODS M): the objects
DEFINITION-SNAPSHOT records, as of the layer's last take of the name.")
(defvar *kept-sequence* 0
  "Monotonic count of definitions the layer has taken.")

(defun definition-name-symbol (name package)
  "The symbol NAME -- a definition-key name string, \"NAME\" or \"(SETF NAME)\" --
names in PACKAGE, or NIL when it names none."
  (find-symbol (string-upcase (ppcre:regex-replace "(?i)^\\(SETF (.*)\\)$" name "\\1")) package))

(defun sole-definition-symbol (key package &aux (name (getf key :name)))
  "=> (values SYMBOL BOUND-P): the symbol KEY names in PACKAGE when KEY is
its name's one definition, and whether this image binds it as KEY's
operator would — a value, a class, else a function. NIL for a method or a
hook, which share their name with siblings a bound name cannot tell apart,
and for a name PACKAGE lacks."
  (when-let (symbol (and (packagep package)
                         (null (getf key :qualifiers))
                         (null (getf key :specializers))
                         (definition-name-symbol name package)))
    (values symbol
            (let ((operator (getf key :operator)))
              (cond ((member operator '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT") :test #'string=)
                     (boundp symbol))
                    ((member operator '("DEFCLASS" "DEFSTRUCT") :test #'string=)
                     (and (find-class symbol nil) t))
                    (t (fboundp (if (uiop:string-prefix-p "(SETF " name)
                                    (list 'setf symbol)
                                    symbol))))))))

(defun restore-shipped-definition (key package)
  "Evaluate the release's definition of KEY, forgotten from the layer, back
into this image when the forgetting left its name, read in PACKAGE,
unbound: the layer's definition stood over the release's, or in place of
it when a boot skipped the file, and taking it away leaves the release's.
Says so on the eval's output. => whether it did."
  ;; A name bound still was defined again by the same form, which is the
  ;; form's to say; a method or a hook is left as the act left it.
  (multiple-value-bind (symbol bound) (sole-definition-symbol key (find-package package))
    (when-let (shipped (and symbol (not bound) (shipped-definition key)))
      (let ((label (definition-key-label key)))
        (when (member label (nth-value 1 (load-source (second shipped)
                                                      :only (list (cons (getf key :operator) label))))
                      :test #'string=)
          (format t "~&[layer: the release's ~a stands again (~a)]~%"
                  label (enough-namestring (second shipped) *source-root*))
          t)))))

(defun note-definition-kept (symbol &aux (objects (definition-objects symbol)))
  "Stamp SYMBOL as taken by the layer just now: the objects the layer now
carries -- the same three DEFINITION-SNAPSHOT records -- under the next
sequence."
  ;; A window can then tell a definition some form kept from one no form kept
  ;; (REPORT-UNKEPT-CHANGES).
  (sb-ext:with-locked-hash-table (*kept-definitions*)
    (setf (gethash symbol *kept-definitions*) (list* :seq (incf *kept-sequence*) objects))))

(defstruct (kept-folder (:copier nil))
  "What one RECORD-DEFINITIONS call did to one folder: its DIRECTORY,
whether it is a KNOWLEDGE cell, the files WRITTEN and REMOVED, the keys
DEFINED and FORGOTTEN, and MOVED -- (KEY . LABEL), a name another folder
now keeps -- each in act order once the acts are done."
  directory knowledge written removed defined forgotten moved operator)

(access (kept kept-folder))

(defun definition-folder (key form &optional (folders (definition-folders)))
  "=> (values DIRECTORY KNOWLEDGE-P): the folder the definition KEY, FORM's
own, is kept in. Code is the layer's. A memory or a prose skill
(KNOWLEDGE-FORM-P) is the :cell FORM names, else the knowledge cell of
FOLDERS already holding KEY's file, else the organism's own
(KNOWLEDGE-DIRECTORY)."
  (if (not (knowledge-form-p form))
      (values (layer-directory) nil)
      (let ((cell (getf (knowledge-form-options form) :cell)))
        (values (cond ((stringp cell) (knowledge-directory cell))
                      ((find-if (lambda (directory)
                                  (and (string/= (folder-name directory) (layer-name))
                                       (probe-file (definition-path key directory))))
                                folders))
                      (t (knowledge-directory)))
                t))))

(defun spans-by-key (text &aux (table (make-hash-table :test #'equal)))
  "TOPLEVEL-DEFINITIONS of TEXT by key, the first span of a key kept."
  ;; Without the hash table, checking member by member down the list would
  ;; compare keys quadratically.
  (dolist (hit (toplevel-definitions text) table)
    (alexandria:ensure-gethash (first hit) table hit)))

(defvar *kept-locks* '()
  "The knowledge folders whose lock the RECORD-DEFINITIONS running on this
thread holds until it returns.")

(defun call-with-kept-lock (knowledge directory thunk)
  "THUNK under DIRECTORY's folder lock when it is a KNOWLEDGE cell -- a
folder profiles share, which *LAYER-LOCK* cannot serialize across homes --
else as it is."
  (if (and knowledge (not (find (namestring directory) *kept-locks* :key #'namestring :test #'string=)))
      (with-folder-lock (directory) (funcall thunk))
      (funcall thunk)))

(defun record-definitions (form form-string end
                           &key (session-id *scribe-session-id*)
                           (package (package-name *package*))
                           (ledger t) &aux (acts '()) folders defined)
  "Write FORM's defining subforms into the folders they are kept in at the
eval seam — the layer, a knowledge cell — and take its forgetting subforms
out of every one."
  ;; FORM is the object the eval snippet read from FORM-STRING; END is the
  ;; reader's end position, so the recorded text is the model's own bytes —
  ;; never a re-print, comments and formatting survive by construction.
  ;; PACKAGE names the package the snippet read FORM in — the IN-PACKAGE its
  ;; layer file opens with, and the package the floor is judged against —
  ;; *PACKAGE* at the call unless the caller kept it from the read (a held
  ;; recording replays on another thread). Forms that neither define nor
  ;; forget — the overwhelming majority of evals — write nothing. Returns the
  ;; recorded keys.
  ;;
  ;; One file per definition (a PROGN of two defuns is two files; a unit's
  ;; definitions are each a file holding the unit whole), the layer's .asd
  ;; extended in first-definition order, one commit per folder naming every
  ;; definition and the session, and one durable ORGANISM.DEFINITION_RECORDED
  ;; fact per folder when the store is open. Floor names are refused — the
  ;; kernel is edited by humans, not spliced by the organism — and say so on
  ;; the eval's output.
  ;;
  ;; Code is the layer's. A memory and a prose skill are knowledge
  ;; (DEFINITION-FOLDER): written to a knowledge cell under its folder lock,
  ;; never sent to the shells and never the layer's revision, since nothing
  ;; loads them; and a name kept in one folder leaves every other one, so a
  ;; name has one home. Every name kept or forgotten is a line in the use
  ;; ledger (knowledge.lisp) unless LEDGER is NIL — an import seeds its own.
  ;;
  ;; Forgetting is the act in reverse (FORGOTTEN-LAYER-FILES): every file of
  ;; the layer and the knowledge cells whose definitions the act names is
  ;; deleted, its folder's .asd loses their entries, the same commit carries
  ;; the drop as `forgot NAME`, and one durable ORGANISM.DEFINITION_FORGOTTEN
  ;; fact tombstones the names for the context engine's defined: clause.
  ;; Acts apply in FORM's order, so forget-then-define in one PROGN is a
  ;; rewrite. A name with no file forgets nothing and says nothing: the image
  ;; act stands on its own, as Lisp has it. A layer name the release defines
  ;; too goes back to the release's definition (RESTORE-SHIPPED-DEFINITION):
  ;; the layer's was an edit of it, and the act took both out of the image.
  ;;
  ;; Never signals: a scribe defect must not turn a successful eval into a turn
  ;; error. A failure prints one bracketed line to *STANDARD-OUTPUT* — inside the
  ;; eval snippet that is the tool result the model reads — and returns NIL. A NIL
  ;; *CELLS-DIRECTORY* — the hermetic test posture — records nothing.
  ;;
  ;; The floor keeps nothing (generation.lisp): a pristine image runs the
  ;; binary alone, and a definition kept there would land in a folder nothing
  ;; loaded and no one proved. The image act stands, as Lisp has it.
  ;;
  ;; Nor does a fork: what it defines stays live, and the parent keeps what
  ;; it chooses (FORK-RECORD, fork.lisp).
  (when (fork-record form session-id)
    (return-from record-definitions nil))
  (when *pristine*
    (format t "~&[pristine: nothing is kept; (rollback N) restores a sealed generation, ~
               or relaunch without --pristine]~%")
    (return-from record-definitions nil))
  (with-handlers ((error (condition)
                    (format t "~&[layer: not recorded — ~a]~%"
                            (handler-case (princ-to-string condition)
                              (error () (type-of condition))))
                    nil))
    (when *cells-directory*
      (walk-toplevel-acts form (lambda (sub &aux (key (definition-key sub)))
                                 (push (list (if key :define :forget) key sub) acts))))
    (setf acts (nreverse acts))
    (when acts
      (when-let (floor (loop for (kind key) in acts
                             when (and (eq kind :define) (floor-key-p key package))
                               collect key))
        (format t "~&[layer: ~{~a~^, ~} ~:[is~;are~] floor material; the kernel is edited by humans, not recorded]~%"
                (mapcar #'definition-key-label floor) (rest floor))
        ;; a forget act carries no key
        (setf acts (remove-if (lambda (act) (and (second act) (member (second act) floor :test #'equal)))
                              acts)))
      (when acts
        (let ((text (string-trim +scribe-whitespace+
                                 (subseq form-string 0 (min end (length form-string)))))
              ;; the members' spans and the definition folders, each read
              ;; once: without this, a PROGN with many definitions would
              ;; read its text once per definition and list every folder
              ;; more than once.
              (spans :unread)
              (listed :unread)
              ;; the knowledge folders this call holds the lock of (FOLDER)
              (*kept-locks* '())
              ;; the knowledge files it has yet to write (HOLD), and their names
              (holding '())
              (held-names (make-hash-table :test #'equal)))
          (labels ((touched (directory)
                     (find (namestring directory) folders
                           :key (lambda (kept) (namestring kept.directory)) :test #'string=))
                   (folder (directory knowledge)
                     ;; the batch of DIRECTORY, first touched first; a knowledge
                     ;; cell's lock taken then, and held to the end of the call --
                     ;; taken a definition at a time.
                     (or (touched directory)
                         (progn
                           (when knowledge
                             (unless (lock-folder directory)
                               (error "~a is held by another process writing to that folder; retry in a moment"
                                      (namestring (folder-lock-path directory))))
                             (push directory *kept-locks*))
                           (car (push (make-kept-folder :directory directory :knowledge knowledge)
                                      folders)))))
                   (listed-folders ()
                     (if (eq listed :unread) (setf listed (definition-folders)) listed))
                   (hold (path content)
                     ;; a knowledge file, written with the call's others
                     ;; (WRITE-HELD); those first when one carries its
                     ;; name, which the next member's folder and its move
                     ;; look for on disk
                     (when (gethash (file-namestring path) held-names)
                       (write-held))
                     (setf (gethash (file-namestring path) held-names) t)
                     (push (cons path content) holding))
                   (write-held ()
                     ;; eight at a time
                     (when holding
                       (parallel-map (lambda (write) (write-file-atomically (car write) (cdr write)))
                                     (reverse (shiftf holding '())))
                       (clrhash held-names)))
                   (forget-file (kept path keys)
                     (call-with-kept-lock kept.knowledge kept.directory
                                          (lambda ()
                                            (delete-file path)
                                            (unless kept.knowledge
                                              (write-layer-system (layer-system-file) (layer-name)))))
                     (forget-knowledge-file path)
                     (push path kept.removed)
                     keys))
            (with-cleanup ((mapc #'unlock-folder *kept-locks*))
              (bt2:with-lock-held (*layer-lock*)
                (dolist (act acts)
                  (destructuring-bind (kind key sub) act
                    ;; a forget reads the folders as they stand
                    (unless (eq kind :define) (write-held))
                    (if (eq kind :define)
                        (multiple-value-bind (directory knowledge) (definition-folder key sub (listed-folders))
                          ;; a folder this call already wrote to has passed
                          (if (and knowledge (not (touched directory))
                                   (asd-files directory) (not (knowledge-folder-p directory)))
                              (format t "~&[~a: a cell of code, not of knowledge; ~a not recorded]~%"
                                      (folder-name directory) (definition-key-label key))
                              (let* ((kept (folder directory knowledge))
                                     (path (definition-path key directory))
                                     ;; A lone definition is the whole text. A member
                                     ;; of a PROGN or EVAL-WHEN gets the scanner's own
                                     ;; sub-span — never the enclosing form, whose
                                     ;; other members (a call, a SETF) would run
                                     ;; again at every boot.
                                     (own (if (eq sub form)
                                              (list key 0 (length text))
                                              (or (gethash key (if (eq spans :unread)
                                                                   (setf spans (spans-by-key text))
                                                                   spans))
                                                  (error "cannot isolate ~a inside the evaluated form"
                                                         (definition-key-label key))))))
                                (call-with-kept-lock
                                 knowledge directory
                                 (lambda ()
                                   ;; the IN-PACKAGE after the header makes OWN mean what it meant at the eval
                                   ;; OWN encoded from TEXT where it stands: copied out
                                   ;; and printed into the header's string first
                                   (let ((content
                                      (let* ((hit (and (not knowledge) (shipped-definition key)))
                                             (head (format nil ";;; ~a — ~a~@[ recorded by session ~a~], ~a~%~@[~a~%~](in-package ~a)~%~%"
                                                           (definition-key-label key)
                                                           (string-downcase (getf key :operator))
                                                           session-id
                                                           (iso-time (unix-now) :millis nil)
                                                           (and hit
                                                                (format nil "~a~a@~a"
                                                                        +override-marker+
                                                                        (enough-namestring (second hit) *source-root*)
                                                                        (shipped-text-digest hit)))
                                                           (if (string= package (string-upcase package))
                                                               (format nil ":~(~a~)" package)
                                                               (format nil "~s" package)))))
                                        (lambda (out)
                                          (write-sequence (sb-ext:string-to-octets head :external-format :utf-8) out)
                                          (write-sequence (sb-ext:string-to-octets text :external-format :utf-8
                                                                                        :start (second own) :end (third own))
                                                          out)
                                          (write-byte 10 out)))))
                                     (cond (knowledge
                                            ;; once a call: it lists nothing, so only its absence matters
                                            (unless kept.written (ensure-knowledge-system directory))
                                            (hold path content))
                                           (t (write-file-atomically path content)
                                              (write-layer-system (layer-system-file) (layer-name)
                                                                  (definition-file-name key)))))))
                                ;; a folder this write made is one the next member may move a name out of
                                (unless (find (folder-name directory) (listed-folders)
                                              :key #'folder-name :test #'string=)
                                  (setf listed :unread))
                                (forget-knowledge-file path)
                                (push path kept.written)
                                (push key kept.defined)
                                (push key defined)
                                ;; one home per name: a memory or a skill kept here leaves every other folder
                                (when (knowledge-operator-p (getf key :operator))
                                  (dolist (other (listed-folders))
                                    (let ((file (definition-path key other)))
                                      (when (and (string/= (folder-name other) (folder-name directory))
                                                 (probe-file file)
                                                 (find-if (lambda (found)
                                                            (and (knowledge-operator-p (getf found :operator))
                                                                 (string= (getf found :name) (getf key :name))))
                                                          (mapcar #'first (file-definitions file))))
                                        (let ((away (folder other (string/= (folder-name other) (layer-name)))))
                                          (forget-file away file '())
                                          (push (cons key (if knowledge (folder-label directory) "layer"))
                                                (kept-folder-moved away))))))))))
                        ;; every folder a definition is kept in; a knowledge cell's one file per name
                        (let ((name (name-designator-string (literal-designator (second sub)))))
                          (dolist (directory (listed-folders))
                            (let ((knowledge (string/= (folder-name directory) (layer-name))))
                              (multiple-value-bind (files shared)
                                  (forgotten-layer-files
                                   sub directory
                                   :paths (if knowledge
                                              (and name (list (definition-path
                                                               (list :name name) directory)))
                                              :all))
                                (dolist (path shared)
                                  (format t "~&[layer: ~a defines more than the act forgets; kept whole]~%"
                                          (file-namestring path)))
                                (when files
                                  (let ((kept (folder directory knowledge)))
                                    (setf kept.operator (or kept.operator (symbol-name (first sub))))
                                    (dolist (file files)
                                      (dolist (key (forget-file kept (car file) (cdr file)))
                                        (push key kept.forgotten))))))))))))
              (write-held)
              (setf folders (nreverse folders) defined (nreverse defined))
              (dolist (kept folders)
                (setf kept.written (nreverse kept.written) kept.removed (nreverse kept.removed)
                      kept.defined (nreverse kept.defined) kept.forgotten (nreverse kept.forgotten)
                      kept.moved (nreverse kept.moved))
                ;; Every layer definition written is taken now: stamp it, so a
                ;; window that spans this write can tell it from an unkept
                ;; change (REPORT-UNKEPT-CHANGES).
                (unless kept.knowledge
                  (dolist (key kept.defined)
                    (when-let (symbol (definition-name-symbol (getf key :name) package))
                      (note-definition-kept symbol)))))
              (dolist (kept folders)
                (when (or kept.written kept.removed)
                  (record-kept-folder kept form text session-id ledger)))
              ;; a name the layer gives up is the release's again
              (dolist (kept folders)
                (unless kept.knowledge
                  (dolist (key kept.forgotten)
                    (restore-shipped-definition key package)))))))
          ;; a knowledge cell this keep made is on the report from now on
          (dolist (kept folders)
            (when (and kept.knowledge (not (find-cell (folder-name kept.directory))))
              (ignore-errors (load-cell (folder-name kept.directory)))))
          defined)))))

(defun record-kept-folder (kept form text session-id ledger)
  "Commit what one RECORD-DEFINITIONS call wrote and removed in KEPT's
folder, say so on the eval's output, and record it: the durable facts, the
ledger lines unless LEDGER is NIL, the layer's revision."
  (let* ((directory kept.directory)
         (name (folder-name directory))
         (label (if kept.knowledge (folder-label directory) "layer"))
         (forgot (distinct (mapcar #'definition-key-label kept.forgotten)))
         (moved (loop for (key . to) in kept.moved
                      collect (format nil "~a to ~a" (definition-key-label key) to)))
         ;; what the act took elsewhere, in the words the commit and the eval's output share
         (notes (loop for (verb names) in `(("forgot" ,forgot) ("moved" ,moved))
                      when names collect (format nil "~a ~{~a~^, ~}" verb names)))
         (commit
           ;; a step git refuses is the eval's line: live, unfiled
           (call-with-kept-lock
            kept.knowledge directory
            (lambda ()
              (git-commit-all
               directory
               (format nil "~{~a~^, ~}~@[ — session ~a~]"
                       (append (mapcar #'definition-key-label kept.defined) notes)
                       session-id)
               :only (cons (if kept.knowledge (knowledge-system-file directory) (layer-system-file name))
                           (distinct (append kept.written kept.removed)))
               :notice (lambda (line) (error "~a" line)))))))
    (when (and (not kept.knowledge) commit (plusp (length commit)))
      (setf *layer-revision* (format nil "~a:~a" name commit)))
    ;; The result says what happened — the rules of the layer, each at the
    ;; moment it applies. The names alone: where a folder lives and how it
    ;; loads are (help :layer)'s, read when wanted, not on every keep.
    (when kept.defined
      (let ((labels (mapcar #'definition-key-label kept.defined)))
        (if kept.knowledge
            (format t "~&[~a: kept ~{~a~^, ~}]~%" label labels)
            ;; Every attached shell in another process is told which
            ;; definition each layer file now holds, and evaluates it from
            ;; the file (SOURCE-LOADED): the shells share the home, so the
            ;; snippet's definition is live where the operator types, not
            ;; only here.
            (let ((told (loop for path in kept.written
                              for key in kept.defined
                              maximize (with-handlers ((error () 0))
                                         (source-loaded
                                          path
                                          (list (cons (getf key :operator)
                                                      (definition-key-label key))))))))
              (format t "~&[layer: kept ~{~a~^, ~}~@[; sent to ~d shell~:p~]]~%"
                      labels (and (plusp told) told))))))
    (dolist (note notes)
      (format t "~&[~a: ~a]~%" label note))
    (when ledger
      (dolist (key kept.defined)
        (note-knowledge-use (getf key :name) "keep" :session session-id))
      (dolist (gone (distinct (mapcar (lambda (key) (getf key :name)) kept.forgotten)))
        (note-knowledge-use gone "forget" :session session-id)))
    ;; A label that names no session -- an import's `hermes import' -- records no fact.
    (when (and session-id (store-open-p) (ignore-errors (session-exists-p session-id)))
      (flet ((fact (kind action names operator)
               (record-organism-fact
                session-id kind action
                (list :names names
                      :operator operator
                      :text text
                      :layer name
                      :commit commit)
                'record-definitions)))
        (when kept.defined
          (fact +kind-definition-recorded+ "organism.definition_record"
                (map 'vector (lambda (key) (getf key :name)) kept.defined)
                (symbol-name (first form))))
        (when kept.forgotten
          (fact +kind-definition-forgotten+ "organism.definition_forget"
                (coerce (distinct (mapcar (lambda (key) (getf key :name)) kept.forgotten))
                        'vector)
                kept.operator))))))

;;; --- what the loader asks --------------------------------------------------

(defun layer-file-override (path)
  "PATH's override of a shipped definition: (values RELATIVE-FILE
RECORDED-DIGEST CURRENT-DIGEST), CURRENT-DIGEST NIL when the shipped
definition is gone, RECORDED-DIGEST NIL when the header records none — the
layer's own definition when it was recorded, which the shipped source
defines since. NIL for a file that overrides nothing."
  ;; An unmarked file is looked up in the shipped source only when its name
  ;; already means something in this image — at boot, that the release
  ;; defined it before the layer loaded — so a layer of its own definitions
  ;; never pays for the whole-tree index. A method or a hook is one of
  ;; several definitions on its name, which a bound name cannot tell apart:
  ;; those load as recorded, and a kernel name loads to meet the lock.
  (multiple-value-bind (relative recorded)
      (with-open-file (in path)
        (loop for line = (read-line in nil nil)
              while (and line (plusp (length line)) (char= (char line 0) #\;))
              when (uiop:string-prefix-p +override-marker+ line)
                do (ppcre:register-groups-bind (relative recorded)
                       ("^(.*)@(.*)$" (subseq line (length +override-marker+)))
                     (return (values relative recorded)))))
    (let ((hit (first (file-definitions path))))
      (if relative
          (let ((shipped (and hit (or (shipped-definition-in relative (first hit))
                                      (shipped-definition (first hit))))))
            (values relative recorded (and shipped (shipped-text-digest shipped))))
          (bind (((key _ _ home) (values-list hit))
                 ((symbol bound) (sole-definition-symbol key home))
                 (shipped (and symbol bound
                               (not (floor-key-p key home))
                               (shipped-definition key))))
            (when shipped
              (values (enough-namestring (second shipped) *source-root*) nil
                      (shipped-text-digest shipped))))))))

(defun stale-layer-file (path)
  "PATH's entry on the upgrade list when the shipped definition it overrides
has changed — or vanished — since the override was recorded, or when one
the layer recorded as its own is shipped since: (FILE-NAME RELATIVE-FILE
RECORDED CURRENT), RECORDED NIL for the last; NIL for a file that overrides
nothing, or whose shipped text still reads as recorded."
  ;; The loader skips a stale file — the override was an edit of that one
  ;; text, and the helpers it names may have moved with it — so the shipped
  ;; definition stands until the organism records the definition again over
  ;; the new text, or forgets it.
  (multiple-value-bind (relative recorded current)
      (handler-case (layer-file-override path) (error () nil))
    (and relative (not (equal recorded current))
         (list (file-namestring path) relative recorded current))))
