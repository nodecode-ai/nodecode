;;;; knowledge.lisp --- what the organism keeps and does not run: the
;;;; knowledge cells, the one index over its definitions, the use ledger.
;;;; Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A memory is a definition. (define-memory name "one sentence" :type
;;;; :feedback ;; why, how to apply ...) evaluated at the eval seam is written
;;;; by the scribe as one file in a KNOWLEDGE CELL --
;;;; ~/.nodecode/cells/<identity>-memory/, or the topic cell its :cell names
;;;; -- a git repository like the layer, whose .asd declares a LAYER system
;;;; carrying (:knowledge . t) in its :properties: the loader records the
;;;; folder and never loads it. A prose skill, (define-skill name "one
;;;; sentence" ;; the steps ...), is the same shape; a skill with a lambda
;;;; list is code, and lives in the layer like any defun. Nothing here is
;;;; evaluated: the index reads the files as data, (help 'name) reads one,
;;;; and a keep costs neither a boot nor the warm image.
;;;;
;;;; THE INDEX is the harness section `index', pinned on a conversation's
;;;; root at its first request and taken again only when the prefix breaks
;;;; anyway -- an eviction, /index reload -- never at a keep: a reflection
;;;; keeps under the conversation it reflects on, and a pin it marked would
;;;; spend a re-read of that conversation on every keep. It lists every
;;;; documented definition of the layer and of the knowledge cells, ranked by
;;;; the ledger and laid out within hot_budget, and counts the rest. A memory
;;;; never cools by time -- a standing preference is not stale because it
;;;; held -- and ranks by its evidence; a skill or a layer definition is hot
;;;; while it was used, or written, inside stale_days.
;;;;
;;;; THE LEDGER is one append-only file under the home, usage.jsonl: a line
;;;; per view, keep, patch, forget, call, helped and harm, each naming the
;;;; definition and the session. The index ranks by its running totals, each
;;;; line folded in once as it is appended (FOLD-LEDGER); a reader of single
;;;; uses reads the lines afresh (KNOWLEDGE-LEDGER-LINES).

(in-package #:nodecode.kernel)

;;; --- the folders --------------------------------------------------------------

(defun knowledge-folder-p (directory)
  "Whether DIRECTORY is a knowledge cell: its .asd declares a LAYER system
carrying (:knowledge . t) in :properties."
  (and directory (uiop:directory-exists-p directory)
       (cell-folder-knowledge-p (make-cell-folder (uiop:ensure-directory-pathname directory)))))

(defun knowledge-directory (&optional (name (format nil "~a-memory" (layer-name))))
  "The knowledge cell NAME under *CELLS-DIRECTORY*, the organism's own by
default: <identity>-memory. Nothing is created here."
  (merge-pathnames (format nil "~a/" name) *cells-directory*))

(defun definition-folders ()
  "The folders the organism's definitions are kept in: the layer when it is
there, then every knowledge cell, by name."
  (when *cells-directory*
    (let ((layer (layer-directory)))
      (append (and (uiop:directory-exists-p layer) (list layer))
              (remove-if (lambda (directory)
                           (or (string= (folder-name directory) (layer-name))
                               (not (knowledge-folder-p directory))))
                         (cell-folders))))))

(defun folder-label (directory &aux (name (folder-name directory))
                                    (own (format nil "~a-" (layer-name))))
  "DIRECTORY as the index names it: a cell of the organism's own without its
<identity>- prefix (`memory'), any other folder by its name."
  (if (and (uiop:string-prefix-p own name) (> (length name) (length own)))
      (subseq name (length own))
      name))

;;; --- a definition read as data -------------------------------------------------

(define-record (knowledge-entry (:copier nil)
                                (:export name operator description type project
                                         path label folder start end written origin))
  "One definition of a definition folder, read from its file and never
evaluated: what the index lists and (help 'name) reads."
  ;; NAME is the definition's name downcased, as the index prints it;
  ;; OPERATOR its defining operator's name (\"DEFINE-MEMORY\"); DESCRIPTION
  ;; its docstring, NIL when it has none -- and an undocumented definition is
  ;; not listed; TYPE and PROJECT a knowledge form's :type keyword and
  ;; :project root; START and END its span in PATH; WRITTEN the file's write
  ;; date as a universal time; ORIGIN its :origin, the world another harness
  ;; kept it in, NIL for the organism's own.
  (key nil)
  (name "" :type string)
  (operator "" :type string)
  (description nil)
  (type nil)
  (project nil)
  (body nil)
  (path nil)
  (label "" :type string)
  (folder nil)
  (start 0)
  (end 0)
  (written 0)
  (origin nil))

(access (definition knowledge-entry))

(defun knowledge-operator-p (operator)
  "Whether OPERATOR, a defining operator's name, keeps knowledge or a skill."
  (member operator '("DEFINE-MEMORY" "DEFINE-SKILL") :test #'string=))

(defun data-entry-p (definition)
  "Whether DEFINITION is knowledge the image does not run: a memory, or a skill in
prose."
  (and (knowledge-operator-p definition.operator) (not (eq definition.body :code))))

(defun knowledge-form-p (form)
  "Whether FORM is knowledge, kept in a knowledge cell and never loaded: a
DEFINE-MEMORY, or a DEFINE-SKILL with no lambda list."
  (and (consp form) (first form) (symbolp (first form)) (consp (rest form))
       (let ((operator (symbol-name (first form))))
         (or (string= operator "DEFINE-MEMORY")
             (and (string= operator "DEFINE-SKILL") (consp (cddr form))
                  (not (listp (third form))))))))

(defun knowledge-form-options (form)
  "=> (values OPTIONS BODY): the keyword pairs after a knowledge FORM's
description, and the string after them, when one closes the form."
  (loop with rest = (cdddr form)
        while (and (keywordp (first rest)) (rest rest))
        append (list (pop rest) (pop rest)) into options
        finally (return (values options (and (stringp (first rest)) (first rest))))))

(defun definition-docstring (form &aux (operator (symbol-name (first form))))
  "The documentation string FORM, a defining form read as data, carries, or
NIL: a knowledge form's description, a function's or a macro's first body
string with a body after it, a variable's fourth element, a class's or a
generic's (:documentation ...) option."
  (flet ((nth-string (n &key followed &aux (element (nth n form)))
           (and (stringp element) (or (not followed) (nthcdr (1+ n) form)) element)))
    (cond ((knowledge-form-p form) (nth-string 2))
          ((member operator '("DEFUN" "DEFMACRO" "DEFINE-VERB" "DEFINE" "DEFINE-SKILL")
                   :test #'string=)
           (nth-string 3 :followed t))
          ((member operator '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT" "DEFTYPE") :test #'string=)
           (nth-string 3))
          ((string= operator "DEFSTRUCT") (nth-string 2))
          ((member operator '("DEFCLASS" "DEFGENERIC") :test #'string=)
           (loop for option in (cddr form)
                 when (and (consp option) (eq (first option) :documentation)
                           (stringp (second option)))
                   return (second option)))
          ((string= operator "DEFINE-HOOK") nil)
          ((uiop:string-prefix-p "DEFINE-" operator)
           (or (nth-string 2) (nth-string 3))))))

(defun file-knowledge-entries (path folder label written)
  "Every definition in PATH, read as data -- *READ-EVAL* off, in a package of
its own made for the read and deleted after, so nothing it names is interned
anywhere that lives -- as KNOWLEDGE-ENTRYs, in file order. NIL when PATH does
not read."
  ;; A form that does not read ends the file, as the scanner's does
  ;; (TOPLEVEL-DEFINITIONS): nothing after it can be placed.
  (when-let (text (read-text path))
    (let ((package (make-package (symbol-name (gensym "NODECODE-KNOWLEDGE-READER-"))
                                 :use '(#:cl)))
          (end (length text)))
      (with-cleanup ((delete-package package))
        (let ((*package* package)
              (*read-eval* nil))
          (loop with position = 0
                for start = (scribe-skip-blank text position end)
                while (< start end)
                for (form next) = (multiple-value-list
                                   (with-handlers ((error () (values nil end)))
                                     (read-from-string text t nil :start start
                                                                   :preserve-whitespace t)))
                for key = (and (not (named-form-p form "IN-PACKAGE")) (definition-key form))
                for (options body) = (and key (knowledge-form-p form)
                                          (multiple-value-list (knowledge-form-options form)))
                for type = (getf options :type)
                for project = (getf options :project)
                for origin = (getf options :origin)
                do (setf position next)
                when key
                  collect (make-knowledge-entry
                           :key key
                           :name (string-downcase (getf key :name))
                           :operator (symbol-name (first form))
                           :description (definition-docstring form)
                           :type (and (keywordp type) type)
                           :project (and (stringp project) (string-right-trim "/" project))
                           :body (if (and (string= (symbol-name (first form)) "DEFINE-SKILL")
                                          (not (knowledge-form-p form)))
                                     :code
                                     body)
                           :path path :label label :folder folder
                           :start start :end next :written written
                           :origin (and (stringp origin) origin))))))))

;;; --- every definition, memoized by file ------------------------------------------
;;; The index is read at a conversation's pin, by (index), by (help 'name) and
;;; by the shell's picker; the files change only when something is kept. Each
;;; file is read again only when its stat moved -- write date and size, a
;;; second's resolution between them -- and the scribe drops the files it
;;; writes (FORGET-KNOWLEDGE-FILE), so a keep and a keep again inside one
;;; second still read as two.

(defvar *knowledge-memo* (make-hash-table :test 'equal :synchronized t)
  "Native namestring -> (STAMP . ENTRIES): every definition file as last read.")

(defun file-stamp (path)
  "PATH's write date and size, (MTIME . SIZE), or NIL when it cannot be stat'ed."
  (ignore-errors
   (let ((stat (sb-posix:stat (sb-ext:native-namestring path))))
     (cons (sb-posix:stat-mtime stat) (sb-posix:stat-size stat)))))

(defun forget-knowledge-file (path)
  "Drop PATH from the memo, so the next read reads it."
  ;; Keyed as the walk lists it: by truename, a linked folder resolved -- asked
  ;; only of a memo holding anything, as a first import's never does.
  (when (plusp (hash-table-count *knowledge-memo*))
    (remhash (sb-ext:native-namestring (or (ignore-errors (truename path)) path)) *knowledge-memo*)))

(defun knowledge-entries (&aux (seen (make-hash-table :test 'equal)))
  "Every definition the organism keeps, read as data: the layer's, then each
knowledge cell's, each folder's files by name."
  (prog1 (loop for folder in (definition-folders)
               for label = (folder-label folder)
               append (loop for path in (sort (directory (merge-pathnames "*.lisp" folder))
                                              #'string< :key #'namestring)
                            for native = (sb-ext:native-namestring path)
                            for stamp = (file-stamp path)
                            for hit = (gethash native *knowledge-memo*)
                            do (unless (and hit stamp (equal (car hit) stamp))
                                 (setf hit (cons stamp (file-knowledge-entries
                                                        path folder label
                                                        (if stamp (+ (car stamp) +unix-epoch+) 0)))
                                       (gethash native *knowledge-memo*) hit))
                               (setf (gethash native seen) t)
                            append (cdr hit)))
    ;; a file that is gone is forgotten with it
    (sb-ext:with-locked-hash-table (*knowledge-memo*)
      (loop for native being the hash-keys of *knowledge-memo*
            unless (gethash native seen) do (remhash native *knowledge-memo*)))))

(defun visible-entry-p (definition root)
  "Whether DEFINITION reads in a session whose project is ROOT: everywhere, unless
its :project names another root."
  (or (null definition.project) (equal definition.project root)))

(defun visible-entries (session-id &aux (root (and session-id (session-project-root session-id))))
  "The documented definitions a session reads: every definition with a
description whose project, if it names one, is SESSION-ID's."
  (remove-if-not (lambda (definition) (and definition.description (visible-entry-p definition root)))
                 (knowledge-entries)))

(defun knowledge-entry-named (name session-id &key data)
  "The definition NAME -- a symbol or a string -- as SESSION-ID reads it, or
NIL; DATA keeps a memory or a prose skill alone. The layer's first, then the
knowledge cells' by name."
  (let ((name (string-downcase (string name)))
        (root (and session-id (session-project-root session-id))))
    (find-if (lambda (definition)
               (and (string= name definition.name)
                    (visible-entry-p definition root)
                    (or (not data) (data-entry-p definition))))
             (knowledge-entries))))

(defun knowledge-text (definition &aux (text (or (read-text definition.path) "")))
  "DEFINITION's definition as it is written in its file."
  (subseq text (min definition.start (length text)) (min definition.end (length text))))

(defun knowledge-prose (definition &aux (text (knowledge-text definition)))
  "The manual DEFINITION carries past its description: the ;; lines inside its
form after the docstring, then a closing string body, as one text; NIL when
there is none."
  (let* ((quoted (and definition.description
                      (let ((*print-pretty* nil)) (prin1-to-string definition.description))))
         (at (and quoted (search quoted text)))
         (prose (loop for line in (and at (lines (subseq text (+ at (length quoted)))))
                      when (ppcre:scan "^\\s*;;(?!;)" line)
                        collect (ppcre:regex-replace "^\\s*;; ?" line ""))))
    (when (or prose (stringp definition.body))
      (format nil "~{~a~^~%~}~@[~%~a~]" prose (and (stringp definition.body) definition.body)))))

;;; --- the ledger ---------------------------------------------------------------------

(defvar *ledger-lock* (bt2:make-lock :name "knowledge-ledger"))

(defvar *ledger-totals* nil
  "The ledger as running totals: (OFFSET HEAD TAIL TABLE PATH) -- the byte its
fold has read to, the bytes the file opened on and the last ones folded,
name -> (LAST VOUCHED . HOURS) (FOLD-LEDGER-LINE), and the file's name.
Nothing of a line is kept past its fold.")

(defun knowledge-ledger-path ()
  (home "usage.jsonl"))

(defun note-knowledge-use (name kind &key session (at (iso-now)) extra)
  "Append one line to the use ledger: AT, NAME (downcased), KIND -- view,
keep, patch, forget, call, helped, harm -- SESSION, and EXTRA's (KEY VALUE
...) members where a value is given. => the line's object."
  (let ((object (json-object "at" at "name" (string-downcase (string name)) "kind" kind
                             :opt "session" session)))
    (loop for (key value) on extra by #'cddr
          do (when value (setf (gethash key object) value)))
    (bt2:with-lock-held (*ledger-lock*)
      (append-json-line (knowledge-ledger-path) object))))

(defun knowledge-ledger-lines ()
  "The ledger's lines as objects, oldest first, read afresh: its reader holds
them while it reads, and the index ranks by the running totals instead
(KNOWLEDGE-STATS)."
  ;; Kept, every line stood as its own table for the image's life -- the
  ;; ledger grows a line per view -- and any append made the copy stale anyway.
  (read-json-lines (knowledge-ledger-path)))

(defun ledger-weight (kind session)
  "What one ledger line of KIND, made by SESSION, adds to a definition's score:
a view, a keep, a call and a helped sighting one each -- a reflection's view
a tenth -- harm minus one, anything else nothing."
  (dispatch kind equal
    ("view" (if (and (stringp session) (uiop:string-prefix-p "experience-" session)) 1/10 1))
    ("keep" 1) ("call" 1) ("helped" 1)
    ("harm" -1)
    (t 0)))

(defun import-session (worlds)
  "The session an import of WORLDS, a list of world names, keeps under: its
commit's, and each ledger line it seeds."
  (format nil "~{~a~^, ~} import" worlds))

(defun import-use-p (line &aux (session (json-value line :string "session")))
  "Whether ledger LINE is an import's own: one it seeded under its session
(IMPORT-SESSION), or a use it brought from a home's ledger (\"imported\")."
  (or (and (json-value line :string "imported") t)
      (and session (uiop:string-suffix-p session " import"))))

(defun fold-ledger-line (table line)
  "One ledger LINE into TABLE, name -> (LAST VOUCHED . HOURS): LAST the
universal time of the name's newest line of any kind, VOUCHED true once a
keep or a helped sighting came from this organism's own use rather than an
import's (IMPORT-USE-P), HOURS hour number -> what that hour's lines weigh
(LEDGER-WEIGHT)."
  (let ((name (json-value line :string "name"))
        (kind (json-value line :string "kind"))
        (at (iso-universal (json-value line :string "at"))))
    (when (and name kind at)
      (let ((total (alexandria:ensure-gethash (string-downcase name) table (list 0 nil)))
            (weight (ledger-weight kind (json-value line :string "session")))
            (hour (floor at 3600)))
        (setf (first total) (max (first total) at))
        (when (and (member kind '("keep" "helped") :test #'string=) (not (import-use-p line)))
          (setf (second total) t))
        (unless (zerop weight)
          (if-let (cell (assoc hour (cddr total)))
            (incf (cdr cell) weight)
            (push (cons hour weight) (cddr total))))))))

(defun fold-ledger (&aux (path (knowledge-ledger-path)))
  "The ledger's running totals, *LEDGER-TOTALS*'s TABLE, with every line
appended since the last fold folded in first (FOLD-LEDGER-LINE). A ledger
that is not the one folded -- another name, gone, shorter, or its first bytes
or the last ones folded not what they were -- is folded again from its first line; a
torn last line waits for its newline. The caller holds *LEDGER-LOCK*."
  ;; A ledger deleted and written again can take the old file's inode, and a
  ;; fixture's first line can repeat byte for byte: both ends of what was
  ;; folded are compared, and a file equal at both is the folded one grown.
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (flet ((octets (from to)
             (let ((buffer (make-array (- to from) :element-type '(unsigned-byte 8))))
               (when in
                 (file-position in from)
                 (setf buffer (subseq buffer 0 (read-sequence buffer in))))
               buffer)))
      (let ((size (if in (file-length in) 0))
            (memo *ledger-totals*))
        (unless (and memo (equal (fifth memo) (namestring path)) (<= (first memo) size)
                     (equalp (second memo) (octets 0 (length (second memo))))
                     (equalp (third memo) (octets (- (first memo) (length (third memo))) (first memo))))
          (setf memo (setf *ledger-totals*
                           (list 0 (octets 0 0) (octets 0 0) (make-hash-table :test 'equal)
                                 (namestring path)))))
        (when (< (length (second memo)) (min size 256))
          (setf (second memo) (octets 0 (min size 256))))
        (when (< (first memo) size)
          (let* ((new (octets (first memo) size))
                 (end (position 10 new :from-end t)))
            (when end
              (loop for start = 0 then (1+ stop)
                    for stop = (position 10 new :start start :end (1+ end))
                    while stop
                    do (let ((line (ignore-errors (decode-json (subseq new start stop)))))
                         (when (hash-table-p line) (fold-ledger-line (fourth memo) line))))
              (incf (first memo) (1+ end))
              (setf (third memo) (octets (max 0 (- (first memo) 256)) (first memo))))))
        (fourth memo)))))

(defun knowledge-stats (&key (stale-days 30) (now (get-universal-time)))
  "Name -> (SCORE LAST VOUCHED) from the ledger's running totals (FOLD-LEDGER):
SCORE what its lines of the last STALE-DAYS days add up to, counted by the
hour (LEDGER-WEIGHT), LAST the universal time of its newest line of any kind,
VOUCHED true once a keep or a helped sighting came from this organism's own
use rather than an import's (IMPORT-USE-P)."
  (let ((table (make-hash-table :test 'equal))
        (since (floor (- now (* stale-days 86400)) 3600)))
    (bt2:with-lock-held (*ledger-lock*)
      (maphash (lambda (name total)
                 (destructuring-bind (last vouched . hours) total
                   (setf (gethash name table)
                         (list (loop for (hour . weight) in hours
                                     when (>= hour since) sum weight)
                               last vouched))))
               (fold-ledger)))
    table))

;;; --- hot and cold --------------------------------------------------------------------

(defun hot-knowledge (entries stats stale-days &optional (now (get-universal-time)))
  "(values HOT COLD): ENTRIES split by STATS, HOT best first. The organism's
own memory is always hot -- what cools it is the budget, never time -- and
anything else is hot while its newest activity is inside STALE-DAYS, but an
import's rule of the operator's word (:feedback, :user) is never hot until
vouched for: another harness's words are not this operator's. Best is the
organism's own, or an import its own use vouched for (KNOWLEDGE-STATS),
before an import nothing vouched for; then the higher score, then a memory
of the operator's own word, then the newer activity, then the name, so two
never tie."
  (let ((hot '()) (cold '()))
    (dolist (definition entries)
      ;; its rank: 1 for its own or vouched else 0, the ledger's score, 1 for its own memory of
      ;; the operator's word else 0, the universal time of its newest activity -- the ledger's
      ;; newest line, else its file's write date, so an import that wrote every file today ranks
      ;; each by the date its own history carries -- and its name
      (let* ((stat (gethash definition.name stats))
             (activity (if stat (second stat) definition.written))
             (own (or (null definition.origin) (and stat (third stat))))
             (word (member definition.type '(:feedback :user)))
             (fresh (< (- now activity) (* stale-days 86400))))
        (if (cond (own (or fresh (string= definition.operator "DEFINE-MEMORY")))
                  (word nil)
                  (t fresh))
            (push (cons (list (if own 1 0)
                              (if stat (first stat) 0)
                              (if (and own word) 1 0)
                              activity definition.name)
                        definition)
                  hot)
            (push definition cold))))
    (values (mapcar #'cdr (sort hot (lambda (a b)
                                      (loop for x in (car a) for y in (car b)
                                            unless (equal x y)
                                              return (if (stringp x) (string< x y) (> x y))))))
            (sort cold #'string< :key #'knowledge-entry-name))))

(defun entry-gist (definition &optional (limit 80))
  "DEFINITION's description as one index line carries it: its first sentence on
one line, clipped to LIMIT."
  (let* ((flat (one-line (or definition.description "")))
         ;; a period closing a word ends the sentence; " . " is a dotted pair
         (end (nth-value 1 (ppcre:scan "[^ ]\\.(?= |$)" flat))))
    (clip (string-left-trim " " (subseq flat 0 end)) limit)))

(defun entry-label (definition stats &aux (stat (gethash definition.name stats)))
  "DEFINITION's folder label as the index heads it, and the world another
harness kept it in while nothing in STATS vouched for it (KNOWLEDGE-STATS):
`memory from hermes'."
  (format nil "~a~@[ from ~a~]" definition.label (and (not (third stat)) definition.origin)))

(defun described-line (definition)
  "DEFINITION as the index describes it: `name — gist'."
  (format nil "~a — ~a" definition.name (entry-gist definition)))

(defun knowledge-tiers (entries budget)
  "(values DESCRIBED NAMED): ENTRIES, best first, as an index of BUDGET
characters holds them -- the first described, the next named on the `also:'
line; the rest do not fit."
  ;; Every one described when all fit. Else the first described within half of
  ;; BUDGET and the next named while the whole fits; and when that names every
  ;; one with room to spare, names become described lines again, best first,
  ;; while the whole still fits -- so one more definition never costs the
  ;; rest their descriptions. A line counts its newline; the also: line is
  ;; "also:" and its newline, then each name with the space before it.
  (let* ((count (length entries))
         (lines (map 'vector (lambda (definition) (1+ (length (described-line definition)))) entries))
         (names (map 'vector (lambda (definition) (1+ (length (knowledge-entry-name definition)))) entries)))
    ;; The characters of the first UPTO described and the rest up to THROUGH named.
    (flet ((size (upto through)
             (+ (reduce #'+ lines :end upto)
                (if (< upto through) (+ 6 (reduce #'+ names :start upto :end through)) 0)))
           (grow (n fits)
             (loop while (and (< n count) (funcall fits (1+ n))) do (incf n) finally (return n))))
      (bind ((all (<= (size count count) budget))
             (described (if all count (grow 0 (lambda (d) (<= (size d d) (/ budget 2))))))
             (listed (if all count (grow described (lambda (l) (<= (size described l) budget)))))
             (described (if (and (not all) (= listed count))
                            (grow described (lambda (d) (<= (size d count) budget)))
                            described)))
        (values (subseq entries 0 described) (subseq entries described listed))))))

;;; --- what a session reads ---------------------------------------------------------------

(defun ranked-knowledge (session-id settings &aux (stale (getf settings :stale-days 30))
                                                  (stats (knowledge-stats :stale-days stale)))
  "=> (values HOT COLD STATS) of what SESSION-ID reads, under SETTINGS
(:stale-days), STATS the ledger's (KNOWLEDGE-STATS) they were ranked by."
  (multiple-value-bind (hot cold) (hot-knowledge (visible-entries session-id) stats stale)
    (values hot cold stats)))

(defun knowledge-surface (session-id settings)
  "The index SESSION-ID reads: a first line saying how to read one and how
many more (index \"text\") finds, then the hot definitions as KNOWLEDGE-TIERS
lays them out within SETTINGS' :hot-budget, described under their folder's
label (ENTRY-LABEL) in rank order, the rest named on the `also:' line; \"\" when there is
nothing to read."
  ;; A label's header is paid out of the budget before the tiers are laid:
  ;; one per folder a hot definition lives in.
  (bind (((hot cold stats) (ranked-knowledge session-id settings))
         (labels (distinct (mapcar (lambda (definition) (entry-label definition stats)) hot)))
         (budget (- (getf settings :hot-budget 8192)
                    (reduce #'+ labels :key (lambda (label) (+ 3 (length label))))))
         ((described named) (knowledge-tiers hot (max 0 budget)))
         (unlisted (- (+ (length hot) (length cold)) (length described) (length named))))
    (if (and (null hot) (null cold))
        ""
        (format nil "(help 'name) reads one before you follow it, as a $name in a request asks~@[; (index \"text\") finds ~d more~]; (help :layer) says how to keep one.~{~%~a~}~@[~%also: ~{~a~^ ~}~]"
                (and (plusp unlisted) unlisted)
                (loop for label in (distinct (mapcar (lambda (definition) (entry-label definition stats))
                                                     described))
                      append (cons (format nil "[~a]" label)
                                   (loop for definition in described
                                         when (string= label (entry-label definition stats))
                                           collect (described-line definition))))
                (mapcar #'knowledge-entry-name named)))))

(defun knowledge-meter (session-id settings)
  "`N hot · M cold · K cells': what /index answers before its picker."
  (bind (((hot cold) (ranked-knowledge session-id settings)))
    (format nil "~d hot · ~d cold · ~d cell~:p" (length hot) (length cold)
            (length (distinct (mapcar #'knowledge-entry-label (append hot cold)))))))

(defun knowledge-rows (session-id settings)
  "The /index picker's rows: every definition SESSION-ID reads, hot first in
rank order and then cold by name, one row each -- the name, its first
sentence, a dot when it is hot, and its folder's label as the row's bucket,
which the dialog's tab strip says once."
  (bind (((hot cold) (ranked-knowledge session-id settings)))
    (flet ((row (definition mark)
             (list :key definition.label :label definition.name :detail (entry-gist definition)
                   :value (format nil "$~a " definition.name) :mark mark :mark-tone :success)))
      (append (mapcar (lambda (definition) (row definition "• ")) hot)
              (mapcar (lambda (definition) (row definition "  ")) cold)))))

(defun knowledge-search (text session-id settings &key (limit 20))
  "Every definition SESSION-ID reads whose name, description or file text
holds TEXT, case folded: this project's first, then the hot in rank order,
then the cold; at most LIMIT lines, the rest counted. => the text."
  (bind (((hot cold) (ranked-knowledge session-id settings))
         (needle (string-trim " " (or text "")))
         (hits (remove-if-not (lambda (definition)
                                (or (search needle definition.name :test #'char-equal)
                                    (search needle definition.description :test #'char-equal)
                                    (search needle (knowledge-text definition) :test #'char-equal)))
                              (append hot cold)))
         (hits (stable-sort hits #'> :key (lambda (definition) (if definition.project 1 0)))))
    (if (null hits)
        (format nil "nothing kept mentions ~s; (help :layer) says how to keep it" needle)
        (format nil "~{~a~^~%~}~@[~%(~d more: a longer text narrows it)~]"
                (loop for definition in hits
                      repeat limit
                      collect (format nil "~a [~a~:[~;, cold~]] — ~a" definition.name definition.label
                                      (member definition cold) (entry-gist definition)))
                (and (> (length hits) limit) (- (length hits) limit))))))

(defun knowledge-help-text (definition)
  "What (help 'name) answers for DEFINITION, a memory or a prose skill: the name,
what it is and where it lives, its description, and the prose after it."
  (format nil "~a — ~:[skill~;memory~]~@[, ~(~a~)~]~@[, for ~a~], in ~a~%~a~@[~%~a~]"
          definition.name (string= definition.operator "DEFINE-MEMORY") definition.type definition.project
          (sb-ext:native-namestring definition.path) definition.description (knowledge-prose definition)))

;;; --- the pin ------------------------------------------------------------------------
;;; A pinned durable harness section, written out for the core: the pin rides
;;; the chain's root (SESSION-STANDING-OWNER), so every fork of one room
;;; sends the same standing block and inherits the prefix its chain paid for.

(defparameter +index-section+ "index"
  "The durable harness key the index rides under.")

(defvar *index-refresh* (make-hash-table :test 'equal :synchronized t)
  "Conversation roots whose next request pins the index again: marked when
the floor advances and on /index reload, never by a keep.")

(defun mark-index-refresh (session-id)
  "Mark SESSION-ID's conversation to pin the index again at its next request."
  (when (stringp session-id)
    (setf (gethash (session-standing-owner session-id) *index-refresh*) t)))

(defun pin-index-section (session-id text &aux (owner (session-standing-owner session-id))
                                               (current (get-harness-section owner +index-section+)))
  "Pin the index on SESSION-ID's standing owner when it has none, or again
when marked: TEXT, a function of the session id, answers it, and an
unchanged text costs no write (every harness put is a durable event)."
  (when (or (null current) (gethash owner *index-refresh*))
    (let ((text (funcall text session-id)))
      (unless (equal text current)
        (set-harness-section owner +index-section+ text)))
    (remhash owner *index-refresh*)))
