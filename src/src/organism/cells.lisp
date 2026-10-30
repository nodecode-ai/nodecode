;;;; cells.lisp --- one folder of cells: load, report, start. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The user touches three things — the binary with its source,
;;;; one folder, git. This file is the folder. ~/.nodecode/cells/<name>/ is
;;;; a cell when it carries an .asd; presence is consent, there is no list
;;;; in config to keep in step. Every process loads the folder at the top of
;;;; MAIN — EVOLVED spans the TUI and the gateway, and an attached TUI is a
;;;; separate process — so an override of COMPOSE-FRAME is live wherever a
;;;; frame is composed.
;;;;
;;;; Two shapes, one convention. A LAYER is the organism's own accumulated
;;;; definitions (scribe.lisp writes it, one definition per file, under git);
;;;; its .asd names the LAYER system class and it loads FROM SOURCE at every
;;;; boot — ASDF keys its fasl cache on the SBCL version, not on this image,
;;;; so a layer compiled against last release's macros would keep stale
;;;; expansions across an upgrade; the layer cache (*LAYER-CACHE-DIRECTORY*)
;;;; keeps that contract by keying each file's compiled form on the image
;;;; stamp and the file's text, so the compile is paid once per build, not
;;;; at every launch. A PERIPHERAL is any other system: bigger,
;;;; dependency-heavy, rarely changing, compiled once and cached. A
;;;; peripheral that runs threads exports START-CELL (config) => stop-thunk,
;;;; which the gateway calls after recovery and funcalls on the way down.
;;;;
;;;; The loader answers the question an upgrade raises — what did this folder
;;;; change? — by snapshotting EVOLVED's definitions before each folder
;;;; loads: a shipped name redefined is an OVERRIDE, a name another folder
;;;; defined is a COLLISION, a method added to a shipped generic is recorded,
;;;; and a folder that fails to load is one record with the condition text
;;;; while its siblings load — a layer finer still: one file per definition,
;;;; so a file that fails is named on the record and the files after it
;;;; load. A layer file that would not run against this image is SKIPPED
;;;; before it loads, and the boot line says why: an override whose shipped
;;;; text moved since it was recorded (the file carries that text's digest),
;;;; or a definition naming a function or variable that neither the image
;;;; nor the layer's later files define — what its compile found missing; a
;;;; file naming what a later one defines loads behind it, once it has.
;;;; Without the skip, loading either file anyway would leave a helper an
;;;; upgrade has inlined undefined at the shell's first frame; skipped, the
;;;; shipped definition stands until the organism records the definition
;;;; again over the new source, or forgets it. (CELLS) is the whole report,
;;;; one function the agent reaches through eval.
;;;;
;;;; Order is deterministic without a manifest: every folder is registered
;;;; with ASDF before any loads, so a folder's :depends-on on a sibling
;;;; resolves; layers load after every peripheral, since a layer is the
;;;; organism's own definitions over everything else — its advice on a
;;;; function a peripheral provides must find that function fbound and its
;;;; package readable; and folders load alphabetically otherwise.

(in-package #:nodecode.kernel)

(defclass layer (asdf:system) ()
  ;; The generated .asd names this class; that is the whole difference between
  ;; a layer and a peripheral.
  (:documentation
   "An ASDF system the organism wrote for itself: one definition per file,
in the EVOLVED package, loaded from source at every boot."))

;;; --- the record -----------------------------------------------------------

(define-record (cell (:copier nil) (:export :readers))
  "What one folder did at boot."
  ;; STATE is :loaded or :failed. FAILURES says
  ;; what did not load, in order: (FILE . TEXT) where FILE names a layer file
  ;; ("_dm-rev_.lisp") and TEXT is the condition with SBCL's load context, or
  ;; FILE is NIL when the folder failed as a unit — its bundle, a compiled
  ;; system, the .asd. A layer's remaining files load past a failed one, so a
  ;; :failed layer still carries DEFINED. KIND is :layer
  ;; (a LAYER system), :peripheral (exports START-CELL), :library (loads,
  ;; nothing to start), or :knowledge (a knowledge cell, never loaded). DEFINED / OVERRIDES / COLLISIONS / METHODS-ADDED name
  ;; EVOLVED definitions as (KIND . SYMBOL) keys; a collision is
  ;; ((KIND . SYMBOL) . OTHER-CELL-NAME). SKIPPED lists the layer files that
  ;; did not load because they would not run against this image, in load order:
  ;; (FILE-NAME . REASON), REASON the text the boot line prints (LOAD-LAYER-FILE)
  ;; — the upgrade question. TIMINGS is ((FILE-NAME . MILLISECONDS) ...) per
  ;; layer file in load order, what each cost this boot: a file whose load
  ;; stalls — an initform that sleeps or waits — is what a slow direct launch
  ;; is made of, and nothing else names it (NOTE-SLOW-LAYER-FILES). START is
  ;; the START-CELL function or NIL; STOP the thunk
  ;; it returned once started; STARTED-P whether it is running now; REFUSAL the
  ;; text of the condition its last start signalled, or NIL -- the start state
  ;; the gateway's lifecycle keeps and DESCRIBE-CELLS reports.
  (name "" :type string)
  (path nil)
  (systems '())
  (state :loaded)
  (kind :library)
  (failures '())
  (defined '())
  (overrides '())
  (collisions '())
  (methods-added '())
  (skipped '())
  (timings '())
  (start nil)
  (stop nil)
  (started-p nil)
  (refusal nil))

(access (cell cell) (layer cell))

(defvar *cells* '()
  "Every folder the last LOAD-CELLS saw, in load order.")

(defvar *cell-load-lock* (bt2:make-recursive-lock :name "cell-load")
  "Held while a folder loads, by LOAD-CELLS and LOAD-CELL alike, and while
anything reads ASDF (UNAVAILABLE-SYSTEMS): ASDF's tables and SBCL's class and
structure definitions belong to one loader at a time, and a folder asked for
by two threads is loaded by the first, the second finding it on the report.")

;;; --- the standing board -----------------------------------------------------
;;; What the organism's parts say about themselves right now, keyed and
;;; live-only: NLE:NOTICE with a KEY replaces the entry under that key, a
;;; NIL text clears it. The board reaches every shell that attaches later
;;; (the gateway pushes it after the first replay) and the model every
;;; round (the live tail, context.lisp READ-LIVE-SECTIONS) -- a refusal at boot is
;;; not lost to whoever was not looking at that moment. Nothing here is
;;; durable, so a restart starts from an empty board.

;;; Rebuilt on every change under the lock, never mutated in place; readers
;;; take a copy.
(defvar *notices* '()
  "((KEY TEXT LEVEL) ...) in first-posting order.")

(defvar *notices-lock* (bt2:make-lock :name "notices"))

(defun note-standing (key text level)
  "Make TEXT the standing notice under KEY at LEVEL, or clear KEY when TEXT
is NIL. => the entry, or NIL when cleared."
  (check-type key string)
  (bt2:with-lock-held (*notices-lock*)
    ;; Replace in place: a notice that updates keeps its position,
    ;; so the board reads in the order its parts first spoke.
    (setf *notices* (if text
                        (put-in-place *notices* key (list text level) #'string=)
                        (remove key *notices* :key #'first :test #'string=)))
    (and text (list key text level))))

(defun standing-notices ()
  "A copy of the board: ((KEY TEXT LEVEL) ...)."
  (bt2:with-lock-held (*notices-lock*)
    (copy-list *notices*)))

;;; --- the log of what was said -----------------------------------------------
;;; THE BOARD STANDS; THE LOG REMEMBERS. A notice WITHOUT a key (gateway.lisp
;;; NOTICE) is said once: it reaches the shells attached at that moment, a
;;; shell attaching later replays no board entry for it, and the prompt's
;;; notices block -- which is the board -- never carries it. Without the log,
;;; a keyless notice would leave no answer to whether that cell said anything.
;;; Every notice is recorded here as it is said, keyed or not, so
;;; (NOTICE-LOG) answers that question. In-process, like the board: a restart
;;; starts with an empty log.
(defparameter +notice-log-size+ 64
  "How many notices the log keeps; the oldest past it falls off.")

(defvar *notice-log* '()
  "The last +NOTICE-LOG-SIZE+ notices, newest first: (TEXT LEVEL TIME).")

(defvar *notice-log-lock* (bt2:make-lock :name "notice-log"))

(defun note-said (text level &optional (time (get-universal-time)))
  "Record TEXT said at LEVEL as the newest notice. => TEXT."
  ;; Called from NOTICE for every notice, keyed or not, so the log is what was
  ;; said and not what still stands.
  (bt2:with-lock-held (*notice-log-lock*)
    (let ((log (cons (list text level time) *notice-log*)))
      (setf *notice-log* (subseq log 0 (min +notice-log-size+ (length log))))))
  text)

(defun notice-log (&key (limit 20))
  "The last LIMIT notices said, newest first: (TEXT LEVEL TIME)."
  (bt2:with-lock-held (*notice-log-lock*)
    (loop for entry in *notice-log*
          for i from 0 below limit
          collect (copy-list entry))))

(defun cells ()
  "The cell report: one CELL record per folder, in load order."
  ;; Read it through eval after a boot or an upgrade; nothing else summarizes
  ;; what the folder did to this image.
  *cells*)

(defun find-cell (name)
  "The record for folder NAME, or NIL."
  (find name *cells* :key #'cell-name :test #'string=))

(defun system-cell (system)
  "The record of the folder that declares SYSTEM, or NIL: how a cell names
its own folder to RESTART-CELLS, whatever name it was installed under."
  (find system *cells* :key #'cell-systems
                        :test (lambda (system systems) (member system systems :test #'string-equal))))

;;; --- definition snapshots -------------------------------------------------

(defun definition-objects (symbol &aux (fn (and (fboundp symbol) (unadvised-function symbol))))
  "SYMBOL's definitions as a snapshot records them, (:FUNCTION F :SETF S :METHODS N), each
NIL when absent: the function under any advice, the setf function, a generic's method count."
  (list :function fn
        :setf (and (fboundp `(setf ,symbol)) (unadvised-function `(setf ,symbol)))
        :methods (and (typep fn 'generic-function) (length (sb-mop:generic-function-methods fn)))))

(defun definition-snapshot (&aux (table (make-hash-table :test 'equal))
                                 (package (find-package '#:nodecode.evolved)))
  "EVOLVED's definitions right now: a table of (KIND . SYMBOL) -> object,
KIND one of :function (the fdefinition under any advice), :setf (the setf
function), :value (the symbol's value), :methods (a generic function's
method count)."
  ;; Only symbols EVOLVED itself owns are snapshotted: inherited kernel names
  ;; are locked, and a folder that touches one fails its load outright.
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (loop for (kind object) on (definition-objects symbol) by #'cddr
            when object do (setf (gethash (cons kind symbol) table) object))
      (when (boundp symbol)
        (setf (gethash (cons :value symbol) table) (symbol-value symbol)))))
  table)

(defun snapshot-delta (before after &aux (new '()) (changed '()))
  "=> (values NEW CHANGED): keys present only in AFTER, and keys whose object
differs between the two — a method count that grew, anything else not EQ."
  (maphash (lambda (key object)
             (multiple-value-bind (old present) (gethash key before)
               (cond ((not present) (push key new))
                     ((eq (car key) :methods)
                      (when (> object old) (push key changed)))
                     ((not (eq object old)) (push key changed)))))
           after)
  (values (sort new #'string< :key (lambda (key) (symbol-name (cdr key))))
          (sort changed #'string< :key (lambda (key) (symbol-name (cdr key))))))

(defun definitions-before ()
  "The eval seam's window from before a form runs -- (SNAPSHOT . TAKE-COUNT),
the definition snapshot and the layer's take count at that moment -- or NIL
when nothing records here (no layer) -- the gate DEFINITION-ESCROW keeps, so
the hermetic posture compares nothing."
  ;; REPORT-UNKEPT-CHANGES reads both: a name taken after TAKE-COUNT is some
  ;; form's keep, not this form's unkept change.
  (when *cells-directory*
    (cons (definition-snapshot)
          (sb-ext:with-locked-hash-table (*kept-definitions*) *kept-sequence*))))

;;; --- the load side: a file back into this image ----------------------------
;;;
;;; The eval seam's act is whole: a definition evaluated here is written to
;;; the layer, and REPORT-UNKEPT-CHANGES names whatever changed in the image
;;; that the layer will not carry. The file seam's act is half of one. EDIT
;;; and WRITE-FILE (exec.lisp) write the bytes and drop the shipped memo but
;;; tell the image nothing. Without this, a definition the organism has just
;;; rewritten in its own source would go on running its old body until a
;;; release rebuild and a relaunch, and the model, reading its own behaviour
;;; back, would read the old one. The discipline that answers it — edit, then
;;; evaluate the same form back — is the missing half of one act, and an act
;;; is not a thing to remember.
;;;
;;; LOAD-SOURCE is that half: the definitions a file gained, or whose text
;;; changed, are read under the package the file itself declares and evaluated
;;; here, in file order.
;;;
;;; The floor is not in the way — measured, not assumed. SBCL refuses a DEFUN
;;; on a kernel symbol from an evolved snippet (SYMBOL-PACKAGE-LOCKED-ERROR: the
;;; floor doing exactly its work) and allows the same DEFUN read with *PACKAGE*
;;; bound to the locked package itself, which is its own exemption for a
;;; package's own code and is precisely what loading kernel source is. A
;;; DEFMETHOD violates no lock at all. So this path lowers nothing and
;;; records no lineage: it loads shipped source the way the boot loads it.
;;;
;;; What it will not do, each named on the receipt rather than done quietly:
;;; evaluate a form that is not a definition (a toplevel SETF or PUSH belongs
;;; to boot, and running it again mid-life is a state change nobody asked
;;; for), reshape what is already live (DEFSTRUCT, DEFCLASS), redefine a
;;; package or a constant, take away a definition the file lost (FORGET's act,
;;; not a write's), or put one byte in the layer — the file the definition
;;; came from IS its durable home, and a layer copy would shadow it with an
;;; override that goes stale at the next edit.

(defvar *staged-source-root* nil
  "A tree LOAD-SOURCE may read from besides the image's own: the staged
release CONVERGE (upgrade.lisp) binds while it evaluates the upgrade.")

;;; REPORT-UNKEPT-CHANGES subtracts them: a name whose durable home is the
;;; file that was just written is not an unkept change, and the layer's advice
;;; would be wrong for it.
(defvar *source-loaded-names* '()
  "The definition names LOAD-SOURCE made live from a file during the eval snippet
now running — turn.lisp binds it per snippet.")

(defun source-residue (text entries)
  "What TEXT says outside its definitions: every definition span removed,
every semicolon comment dropped, the rest joined on single spaces."
  ;; Two revisions whose residue differs carry a change no definition holds —
  ;; a toplevel form, an IN-PACKAGE, a reader conditional — and the receipt
  ;; says so rather than leaving the model to read the whole file as live.
  (bind ((position 0)
         (gaps (with-output-to-string (out)
                 (dolist (entry (sort (copy-list entries) #'< :key #'second))
                   (when (< position (second entry))
                     (write-string text out :start position :end (second entry)))
                   (setf position (max position (third entry))))
                 (write-string text out :start position))))
    (format nil "~{~a~^ ~}"
            (loop for line in (lines gaps)
                  for code = (string-trim +scribe-whitespace+ (subseq line 0 (position #\; line)))
                  when (plusp (length code)) collect code))))

(defun definition-changes (old-text new-text)
  "=> (values ENTRIES GONE RESIDUE AFTER): ENTRIES the (ENTRY . OLD-BODY) of
NEW-TEXT's definitions that are new (OLD-BODY NIL) or whose text moved, ENTRY
as TOPLEVEL-DEFINITIONS lists it; GONE the labels OLD-TEXT defined and
NEW-TEXT does not; RESIDUE true when text outside every definition differs
(for a new file, NIL OLD-TEXT: when it holds any beyond IN-PACKAGE); AFTER
every definition of NEW-TEXT."
  (let* ((after (toplevel-definitions new-text :read-eval t))
         (old (and old-text (toplevel-definitions old-text :read-eval t)))
         (was (make-hash-table :test 'equal)))
    (dolist (entry old)
      (setf (gethash (first entry) was) (subseq old-text (second entry) (third entry))))
    (values (loop for entry in after
                  for previous = (gethash (first entry) was)
                  for body = (subseq new-text (second entry) (third entry))
                  unless (and previous (string= previous body))
                    collect (cons entry previous))
            (loop for key being the hash-keys of was
                  unless (assoc key after :test #'equal) collect (definition-key-label key))
            (if old-text
                (string/= (source-residue old-text old) (source-residue new-text after))
                (plusp (length (string-trim +scribe-whitespace+
                                            (ppcre:regex-replace-all "(?i)\\(in-package[^)]*\\)"
                                                                     (source-residue new-text after)
                                                                     "")))))
            after)))

(defun source-loaded (path definitions)
  "This image evaluated DEFINITIONS — ((OPERATOR . LABEL) ...), LABEL a
DEFINITION-KEY-LABEL — from the Lisp file at PATH, a file of its own trees:
the one seam that says so, called by LOAD-SOURCE for an edited file and by
RECORD-DEFINITIONS for a layer file just written."
  ;; => how many shells in
  ;; other processes were told: 0 here, where nothing listens. While the gateway
  ;; serves it advises this function (START-GATEWAY; ANNOUNCE-LOADED in
  ;; gateway.lisp): every attached shell in another process is sent an
  ;; organism.loaded frame naming PATH and DEFINITIONS, and each evaluates the
  ;; same definitions from the same file into its own image (LOAD-SOURCE :ONLY,
  ;; folded in client.lisp). Only the names cross the wire: a shell on another
  ;; disk finds no file of its trees and loads nothing. Live-only by
  ;; construction — a shell that starts later reads the file itself.
  (declare (ignore path definitions))
  0)

(defun unresolved-names (form &aux (missing '()))
  "The functions and variables FORM names that this image defines nowhere, as
(KIND PACKAGE NAME SETF-P): what FORM's compile, in a unit of its own, notes
as undefined. FORM is compiled, silently, and never run."
  ;; The compiler is the authority on what a form names: the same notes
  ;; COMPILE-LAYER-FILE keeps for a layer file (MISSING-NAME-ENTRY).
  (ignore-errors
   (handler-bind ((warning (lambda (condition)
                             (when-let (entry (missing-name-entry condition))
                               (when (member (first entry) '(:function :variable))
                                 (pushnew entry missing :test #'equal)))
                             (muffle-warning condition)))
                  (sb-ext:compiler-note #'muffle-warning))
     (with-compilation-unit (:override t)
       (compile nil `(lambda () ,form)))))
  (remove-if #'missing-name-defined-p (nreverse missing)))

(defun definition-package (entry)
  "The package the file declares for ENTRY, a definition as
TOPLEVEL-DEFINITIONS lists it; NIL when this image lacks that package."
  (find-package (fourth entry)))

(defun defined-here-p (entry)
  "Whether this image already gives the name ENTRY defines -- a definition as
TOPLEVEL-DEFINITIONS lists it -- a function, a value or a class."
  (when-let (symbol (and (definition-package entry)
                         (definition-name-symbol (getf (first entry) :name)
                                                 (definition-package entry))))
    (and (or (fboundp symbol) (fboundp (list 'setf symbol)) (boundp symbol)
             (find-class symbol nil)
             (member (sb-int:info :variable :kind symbol) '(:special :global :constant))))))

(defun definition-defines-p (entry form missing)
  "Whether ENTRY, a definition as TOPLEVEL-DEFINITIONS lists it, read as FORM,
defines what a compile noted MISSING, (KIND PACKAGE NAME SETF-P): by its own
name, or -- a structure, a class, a condition, a record -- by a symbol its
form carries or a name its constructor, copier, predicate or accessors take."
  (bind (((_ package name setf-p) (values-list missing)))
    (let* ((key (first entry))
           (operator (getf key :operator))
           (own (and (definition-package entry)
                     (definition-name-symbol (getf key :name) (definition-package entry))))
           (symbol (and (find-package package) (find-symbol name package))))
      (and own symbol
           (if (member operator '("DEFSTRUCT" "DEFCLASS" "DEFINE-CONDITION" "DEFINE-RECORD")
                       :test #'string=)
               (or (uiop:string-prefix-p (format nil "~a-" (symbol-name own)) name)
                   (member name (list (format nil "MAKE-~a" (symbol-name own))
                                      (format nil "COPY-~a" (symbol-name own)))
                           :test #'string=)
                   (and (member symbol (alexandria:flatten form)) t))
               ;; a DEFUN of NAME is not one of (SETF NAME); a definer of its
               ;; own -- DEFSETF, a DEFINE-* table -- may give both
               (and (eq symbol own)
                    (or (not (member operator '("DEFUN" "DEFGENERIC" "DEFMETHOD") :test #'string=))
                        (eq (and setf-p t) (uiop:string-prefix-p "(SETF " (getf key :name))))))))))

(defun make-definitions-live (text after wanted redefined-p verify
                              &aux (done '()) (live '()) (held '()) (sent '()) (taken '())
                              (pending '()) (undo '()) (forms (make-hash-table :test 'equal)))
  "Evaluate WANTED, definitions of TEXT as TOPLEVEL-DEFINITIONS lists them --
AFTER is every one TEXT holds -- into this image under the package each
declares, then call VERIFY, when given: => (values LIVE HELD SENT) as
LOAD-SOURCE answers them."
  ;; A definition is evaluated only once its compile resolves every function
  ;; and variable it names (UNRESOLVED-NAMES): one naming what this image
  ;; lacks would stand in it failing at every call. So a definition waits for
  ;; the ones in the load that define what it names; a name only TEXT defines,
  ;; in a definition this image lacks outright, is taken from TEXT into the
  ;; load; definitions left naming each other alone go live on each other's
  ;; word; the rest is held, the names it lacks its reason. REDEFINED-P says
  ;; which definitions redefine what the image holds: what the operators
  ;; REFUSAL names cannot take. A compile cannot see everything a definition
  ;; breaks -- a caller passing a callee arguments it no longer takes, a value
  ;; its declared type refuses -- so VERIFY, a function of no arguments, runs
  ;; once the load is live, and when it signals every definition the load made
  ;; live is put back as it was (SNAPSHOT-DEFINITIONS, taken before each
  ;; evaluates) and held with its error. A shell passes its own frame.
  (labels ((span (entry) (cons (second entry) (third entry)))
           (label (entry) (definition-key-label (first entry)))
           (first-line (condition)
             (string-right-trim '(#\Space #\Return)
                                (or (first (lines (safe-condition-report condition))) "")))
           (form (entry)
             ;; read once per span, under the file's package; NIL when it does not read
             (alexandria:ensure-gethash
              (span entry) forms
              (ignore-errors
               (let ((*package* (definition-package entry))
                     (*read-eval* t))
                 (read-from-string text t nil :start (second entry) :end (third entry))))))
           (refusal (entry &aux (operator (getf (first entry) :operator))
                                (redefined (funcall redefined-p entry)))
             ;; what no compile decides: the package, and the operators a redefinition breaks
             (cond ((null (definition-package entry))
                    (format nil "package ~(~a~) is not in this image" (fourth entry)))
                   ((and redefined (member operator '("DEFSTRUCT" "DEFCLASS") :test #'string=))
                    (format nil "~(~a~) leaves every live instance obsolete" operator))
                   ((and redefined (string= operator "DEFPACKAGE"))
                    "defpackage moves symbols under running code")
                   ((and redefined (string= operator "DEFCONSTANT"))
                    "a constant's redefinition is a continuable error")))
           (take (entry)
             (push entry taken)
             (if-let (reason (refusal entry))
               (push (cons (label entry) reason) held)
               (push entry pending)))
           (go-live (entry &aux (key (first entry)) (package (definition-package entry)))
             ;; A unit yields one entry per name it defines over one span: the
             ;; unit evaluates once and the names after the first ride it.
             (let ((reason (unless (member (span entry) done :test #'equal)
                             (when verify
                               (setf undo (append (snapshot-definitions
                                                   (loop for other in after
                                                         when (equal (span other) (span entry))
                                                           collect (cons (package-name package)
                                                                         (getf (first other) :name))))
                                                  undo)))
                             (handler-case
                                 (let ((*package* package)
                                       (*read-eval* t))
                                   (with-compilation-unit (:override t)
                                     (handler-bind ((warning #'muffle-warning))
                                       (eval (read-from-string text t nil :start (second entry)
                                                                          :end (third entry)))))
                                   nil)
                               (error (condition) (first-line condition))))))
               (cond (reason (push (cons (label entry) reason) held))
                     (t (pushnew (span entry) done :test #'equal)
                        (push (label entry) live)
                        (push (cons (getf key :operator) (label entry)) sent)
                        (pushnew (getf key :name) *source-loaded-names* :test #'string-equal)
                        ;; A file's definitions are the layer's take too: stamp
                        ;; them for windows this load may span (REPORT-UNKEPT-CHANGES).
                        (when-let (symbol (definition-name-symbol (getf key :name) package))
                          (note-definition-kept symbol))))))
           (unresolved (entry &aux (*package* (definition-package entry)))
             ;; compiled where it is read: a kernel file's package locks
             ;; exempt only the kernel's own code
             (unresolved-names (form entry)))
           (defines-p (entry name) (definition-defines-p entry (form entry) name)))
    (mapc #'take wanted)
    (setf pending (nreverse pending))
    (loop
      (let ((waiting '()) (moved nil))
        (dolist (entry pending)
          (if-let (missing (and (not (member (span entry) done :test #'equal))
                                (form entry)
                                (unresolved entry)))
            (push (cons entry missing) waiting)
            (progn (go-live entry) (setf moved t))))
        (setf waiting (nreverse waiting)
              pending (mapcar #'car waiting))
        (cond ((null waiting) (return))
              ;; another pass: what went live may be what the others name
              (moved)
              ;; nothing moved: take from TEXT what it alone defines
              ((let ((found (remove-duplicates
                             (loop for (nil . missing) in waiting
                                   nconc (loop for name in missing
                                               for provider = (find-if (lambda (entry)
                                                                         (and (not (member entry taken :test #'eq))
                                                                              (not (defined-here-p entry))
                                                                              (defines-p entry name)))
                                                                       after)
                                               when provider collect provider)))))
                 (mapc #'take found)
                 found))
              (t
               ;; Held, one at a time, whatever names what no waiting
               ;; definition gives; what is left names only each other.
               (loop
                 (let* ((stuck nil)
                        (lacks (loop for (entry . missing) in waiting
                                     for lacking = (remove-if (lambda (name)
                                                                (some (lambda (other) (defines-p (car other) name))
                                                                      waiting))
                                                              missing)
                                     when lacking
                                       do (setf stuck entry)
                                       and return lacking)))
                   (unless stuck (return))
                   (push (cons (label stuck)
                               (format nil "names ~{~a~^, ~}, which this image does not define"
                                       (mapcar #'missing-name-label lacks)))
                         held)
                   (setf waiting (remove stuck waiting :key #'car))))
               (dolist (wait waiting) (go-live (car wait)))
               (return)))))
    (let ((live (nreverse live)) (held (nreverse held)) (sent (nreverse sent)))
      (when-let (failure (and verify live
                              (handler-case (progn (funcall verify) nil)
                                (error (condition) (first-line condition)))))
        ;; newest snapshot first, so a name taken twice ends as it began
        (restore-definitions undo)
        (setf held (append (mapcar (lambda (label)
                                     (cons label (format nil "put back: with it live here, ~a" failure)))
                                   live)
                           held)
              live '()
              sent '()))
      (values live held sent))))

(defun load-source (path &key before only verify &aux (truename (ignore-errors (truename path))))
  "Make this image agree with the Lisp file at PATH: every definition the file
gained, and every one whose text differs from BEFORE — the file's text before
the write, NIL meaning every definition the file holds — is evaluated back
into this image, under the package the file declares, once everything it
names is defined here (MAKE-DEFINITIONS-LIVE)."
  ;; ONLY, a list of (OPERATOR . LABEL) pairs, names the definitions to
  ;; evaluate instead, whatever BEFORE says: what a shell folds when the
  ;; organism's image told it which definitions of PATH it just made live
  ;; (SOURCE-LOADED). VERIFY, a function of no arguments, is what must still
  ;; run once they are live: when it signals they are put back and held.
  ;;
  ;; => (values LINE LIVE HELD GONE): LINE the clause a receipt carries, NIL when
  ;; there is nothing to say; LIVE the labels now live here; HELD ((LABEL . REASON)
  ;; ...), what was named instead of evaluated; GONE the definitions BEFORE carried
  ;; and the file no longer does — still in the image, since taking a definition
  ;; away is FORGET's act, not a write's.
  ;;
  ;; What went live here is announced through SOURCE-LOADED, and LINE counts
  ;; the shells told — unless ONLY named it: a shell evaluating what it was
  ;; told is not the origin, and says nothing on.
  ;;
  ;; Never signals. A file outside the trees this image loads from, a file
  ;; that does not read, a package this image lacks: each answers, none stops
  ;; the write that called it.
  (with-handlers ((error () nil))
    ;; PATH, through its links, under the shipped source, the cells beside it or the folder
    (when (and truename
               (some (lambda (root)
                       (when-let (directory (ignore-errors (namestring (truename root))))
                         (uiop:string-prefix-p directory (namestring truename))))
                     (list (ignore-errors *source-root*)
                           (ignore-errors (shipped-cells-directory))
                           *staged-source-root*
                           (ignore-errors (probe-file *cells-directory*)))))
      (bind ((text (uiop:read-file-string truename :external-format :utf-8))
             (before (and (not only) before))
             ;; the same diff the upgrade classifier reads: CHANGES the definitions
             ;; new or moved since BEFORE, GONE the ones the file lost, DIFFERS
             ;; whether anything outside a definition moved, AFTER every definition
             ;; the file holds — which is what :ONLY walks, changed or not.
             ((changes gone differs after) (definition-changes before text))
             ((live held sent)
              (make-definitions-live
               text after
               (if only
                   (remove-if-not (lambda (entry)
                                    (member (cons (getf (first entry) :operator)
                                                  (definition-key-label (first entry)))
                                            only :test #'equal))
                                  after)
                   (mapcar #'car changes))
               ;; What a definition's operator cannot take is a REdefinition --
               ;; the upgrade classifier's rule, which holds a changed one and
               ;; sends a new one hot. Under ONLY that plan already decided; a
               ;; write knows its definition is new only when BEFORE lacks it.
               (lambda (entry)
                 (and (not only) (or (null before) (cdr (assoc entry changes :test #'eq)))))
               verify)))
        (let ((told (if (or only (null sent))
                        0
                        (handler-case (source-loaded truename sent) (error () 0)))))
          ;; the receipt's clause: each list shows eight labels, then a count (a receipt is a sentence)
          (values (flet ((listed (labels &aux (rest (- (length labels) 8)))
                           (format nil "~{~a~^, ~}~@[ +~a more~]"
                                   (subseq labels 0 (min 8 (length labels)))
                                   (and (plusp rest) rest))))
                    (when-let (parts (remove nil
                                             (list (when live (format nil "live here: ~a" (listed live)))
                                                   (when (plusp told)
                                                     (format nil "sent to ~d shell~:p" told))
                                                   (when held
                                                     (format nil "not live until a relaunch: ~a"
                                                             (listed (mapcar (lambda (entry)
                                                                               (format nil "~a — ~a"
                                                                                       (car entry) (cdr entry)))
                                                                             held))))
                                                   (when gone
                                                     (format nil "gone from the file, still in the image: ~a"
                                                             (listed gone)))
                                                   (when (and before differs)
                                                     "changes outside any definition are not live"))))
                      (format nil "~{~a~^; ~}" parts)))
                  live held gone))))))

(defun report-unkept-changes (before form kept)
  "Name, on the eval's own output, what FORM changed in the image that the
layer will not carry to the next boot."
  ;; KEPT is the names the scribe recorded or held for this form: a form that
  ;; kept anything replays whole at boot, by-products included (a DEFSTRUCT's
  ;; accessors, a DEFCLASS's readers), so nothing is said; a form that kept
  ;; nothing and still changed a function, a setf function or a method set
  ;; defined it some way the layer does not see -- built as a string and
  ;; evaled inside another form, or through (setf fdefinition) -- and is told
  ;; so. A (hook ...) call at toplevel is named apart: it is the one live-only
  ;; act that looks like the kept one. And a name the layer took while the
  ;; window was open -- another form's keep, another session's snippet, a file's
  ;; load -- is durable too and is subtracted with them: not this form's
  ;; unkept change, whoever's form carried it away; so is a name held for its
  ;; first traversal (HOLD-DEFINITIONS), a keep pending. Values are left alone -- a
  ;; setf of a variable is state, not a definition, and nagging on every
  ;; counter would drown the line that matters. BEFORE is DEFINITIONS-BEFORE's
  ;; window, (SNAPSHOT . TAKE-COUNT); NIL means nothing records here. Never
  ;; signals.
  (with-handlers ((error () nil))
    ;; a fork's eval is told nothing, and a fork's names are no other eval's
    ;; change (FORK-UNKEPT, fork.lisp)
    (when (and before (not (fork-unkept before)))
      (let* ((after (definition-snapshot))
             ;; every key new or changed since the window opened
             (delta (unless kept
                      (multiple-value-call #'append (snapshot-delta (car before) after))))
             ;; what waits on its first traversal (HOLD-DEFINITIONS)
             (held (and delta (mapcar (lambda (entry) (getf entry :name)) (held-definitions))))
             (names (remove-duplicates
                     (loop for key in delta
                           unless (or (eq (car key) :value)
                                      ;; A name a file just made live is
                                      ;; not an unkept change: its durable
                                      ;; home is that file (LOAD-SOURCE).
                                      (member (symbol-name (cdr key))
                                              *source-loaded-names*
                                              :test #'string-equal)
                                      ;; Nor is a name held for its first
                                      ;; traversal: some form's keep,
                                      ;; pending -- a hold armed inside
                                      ;; another form, a race's swap.
                                      (member (symbol-name (cdr key)) held
                                              :test #'string=)
                                      ;; And a name the layer took while
                                      ;; this window was open, still
                                      ;; standing as it was taken, is
                                      ;; some form's keep: carried, not
                                      ;; this form's unkept change.
                                      (when-let (kept (sb-ext:with-locked-hash-table
                                                          (*kept-definitions*)
                                                        (gethash (cdr key) *kept-definitions*)))
                                        (and (> (getf kept :seq) (cdr before))
                                             (eql (getf kept (car key)) (gethash key after)))))
                             collect (string-downcase (symbol-name (cdr key))))
                     :test #'string=)))
        (when names
          ;; a form that (load ...)ed files names the process it reached and what differs from HEAD
          (if-let (paths (labels ((walk (sub)
                                    (when (consp sub)
                                      (cond ((and (eq (first sub) 'load) (stringp (second sub)))
                                             (list (second sub)))
                                            ((member (first sub) '(progn prog1 prog2 when unless let let*))
                                             (mapcan #'walk (rest sub)))))))
                           (walk form)))
            (dolist (path paths)
              (let* ((truename (ignore-errors (truename path)))
                     (directory (and truename (directory-namestring truename)))
                     (committed (and directory
                                     (ignore-errors
                                      (run-bounded
                                       (list "git" "-C" directory "show"
                                             (format nil "HEAD:./~a" (file-namestring truename)))
                                       :seconds 30 :error-output nil))))
                     ;; definitions new or changed since git HEAD (DEFINITION-CHANGES, the
                     ;; upgrade's own diff), whitespace counted; :untracked when unknown
                     (differing
                       (with-handlers ((error () :untracked))
                         (if (or (null committed) (string= committed ""))
                             :untracked
                             (mapcar (lambda (change) (definition-key-label (first (car change))))
                                     (definition-changes committed (read-text truename)))))))
                (format t "~&[load: ~a reached this process (pid ~d) only — ~d definition~:p re-evaluated~a; nothing kept, the file is shipped source, and a shell in another process shows the change after a release rebuild and relaunch]~%"
                        path (sb-posix:getpid) (length names)
                        (cond ((eq differing :untracked) "")
                              ((null differing) ", none differing from the committed file")
                              (t (format nil ", ~d differing from the committed file: ~{~a~^, ~}~:[~; ...~]"
                                         (length differing)
                                         (subseq differing 0 (min 12 (length differing)))
                                         (> (length differing) 12)))))))
            (format t "~&[layer: ~{~a~^, ~} changed but not kept — only a toplevel defining form is recorded; eval the (defun ...) itself. (help :layer) has the rest]~%"
                    names)))
        (when (labels ((hook-form-p (sub)
                         (or (named-form-p sub "HOOK")
                             (and (named-form-p sub "PROGN")
                                  (some #'hook-form-p (rest sub))))))
                (hook-form-p form))
          (format t "~&[layer: (hook ...) advice is live-only; (define-hook name \"key\" (next &rest args) ...) is the kept form]~%"))))))

;;; --- the folder -----------------------------------------------------------

;;; A layer loads from source at every boot by contract — ASDF's fasl cache is
;;; keyed on the SBCL version, not on this image, so a layer compiled against
;;; last release's macros would keep stale expansions across an upgrade — but
;;; compiling 28 definitions at every launch was 100 ms of the operator's
;;; wait. Keying the compiled file on the image stamp keeps the contract (a
;;; new build is a new stamp, its directory empty) and pays each compile once.
;;; NIL — the hermetic test posture — compiles into a throwaway directory for
;;; the one load (LOAD-CELL-FOLDERS): a layer file is judged by what its
;;; compile found missing whether or not a build keeps the compilation.
(define-startup-parameter *layer-cache-directory*
  (cache-path "nodecode/layers/")
  "Where a layer's files compile to: one directory per *IMAGE-STAMP*, one
fasl per file named by the file's text.")

(define-startup-parameter *peripheral-cache-directory*
  (cache-path "nodecode/peripherals/")
  "Where a peripheral outside the shipped tree compiles to:
<stamp>/<folder>/, so a new build compiles it again
(CELL-OUTPUT-TRANSLATIONS). NIL leaves it to ASDF's user cache.")

(defun asd-files (directory &aux (native (uiop:native-namestring
                                            (uiop:ensure-directory-pathname directory))))
  "The .asd files at DIRECTORY's top level: a folder carrying one is a cell."
  ;; Matched by name, the matches alone made pathnames and resolved: through
  ;; DIRECTORY, every entry was, and a knowledge cell holds a file a
  ;; definition.
  (loop for name in (directory-names native)
        when (and (> (length name) 4) (uiop:string-suffix-p name ".asd"))
          when (probe-file (uiop:parse-native-namestring (concatenate 'string native name)))
            collect it))

(defun cell-folders (&optional (root *cells-directory*))
  "Subdirectories of ROOT carrying at least one .asd at their top level,
sorted by name. A missing ROOT is an empty folder, not an error."
  (when (and root (probe-file root))
    (sort (remove-if-not #'asd-files (uiop:subdirectories root)) #'string<
          :key #'folder-name)))

(defun folder-name (directory)
  "The last name on DIRECTORY's path: `b' for /a/b/, and for /a/b."
  (car (last (pathname-directory (uiop:ensure-directory-pathname directory)))))

(defun broken-cell-links (&optional (root *cells-directory*))
  "The names of entries directly under ROOT that are symlinks resolving to
nothing: a cell installed once whose folder has moved or been deleted."
  ;; Sorted.
  ;;
  ;; CELL-FOLDERS cannot see these at all — a link to nothing carries no
  ;; .asd, so the walk passes over it — and a cell that leaves in silence
  ;; is how a whole folder's worth of them once vanished from an operator's
  ;; boot report without a word. LOAD-CELLS reports each as a failed folder
  ;; instead, naming the path it still points at, so the line says what to
  ;; repair.
  ;;
  ;; The predicate is the POSIX pair, not PROBE-FILE: SBCL truenames the link
  ;; itself, so PROBE-FILE answers true for a dangling one. LSTAT says it is a
  ;; link, and STAT — which follows it — fails exactly when it resolves to
  ;; nothing. Nothing to do where the platform has no links: the installer
  ;; copies there.
  (declare (ignorable root))
  #+win32 '()
  #-win32
  (when (and root (uiop:directory-exists-p root))
    (let ((native (sb-ext:native-namestring root)))
      (remove-if-not (lambda (name &aux (path (concatenate 'string native name)))
                       (and (ignore-errors (symlink-target (sb-ext:parse-native-namestring path)))
                            (not (ignore-errors (sb-posix:stat path)))))
                     (entry-names root)))))

(defun entry-names (root)
  "The names directly under the directory ROOT, whatever each is — a folder,
a link, a link that resolves to nothing — read off the directory itself, so
a dangling link is among them: a walk through pathnames truenames each entry
and passes over one that leads nowhere."
  ;; Sorted. Where the platform has no links the installer copies, and the
  ;; subdirectories are the entries.
  #+win32 (sort (mapcar #'folder-name (uiop:subdirectories root)) #'string<)
  #-win32 (sort (directory-names (sb-ext:native-namestring root)) #'string<))

(defstruct (declared-system (:copier nil)
                            (:constructor make-declared-system
                                (name &key layer knowledge depends-on files problem description serves)))
  "What one top-level .asd declares, read as data."
  ;; NAME is the system, LAYER whether its :class is the LAYER class,
  ;; KNOWLEDGE whether that layer carries (:knowledge . t) in :properties --
  ;; a knowledge cell, recorded and never loaded (knowledge.lisp),
  ;; DEPENDS-ON the system names it names (downcased strings), FILES a layer's
  ;; (:file NAME) components in order, and PROBLEM a text when a layer
  ;; declares what a layer cannot — a :pathname, a component that is not
  ;; (:file NAME) — which the load records as the folder's failure rather than
  ;; loading nothing quietly. DESCRIPTION is its :description, "" when it has
  ;; none. SERVES is what it says it serves, its :properties' (:serves . PLISTS)
  ;; as SERVES-ENTRIES shapes it, NIL when it says nothing.
  (name "" :type string)
  (layer nil)
  (knowledge nil)
  (depends-on '())
  (files '())
  (problem nil)
  (description "" :type string)
  (serves '()))

(defun serves-entries (entries)
  "ENTRIES, what a folder says it serves -- the plists of its .asd's (:serves
. PLISTS) property, or the objects of an index entry's serves array -- in one
shape, the one both are compared in: a list of alists, each (KEY . TEXT)
sorted by KEY, a downcased string with - read as _. The core reads no key:
what a kind of entry means is its consumer's (the /models picker reads
provider)."
  (loop for entry in entries
        collect (sort (loop for (key . value) in (if (hash-table-p entry)
                                                     (alexandria:hash-table-alist entry)
                                                     (alexandria:plist-alist entry))
                            collect (cons (substitute #\_ #\- (string-downcase (string key))) value))
                      #'string< :key #'car)))

(access (declared declared-system) (primary declared-system))

(defstruct (cell-folder (:copier nil) (:constructor %make-cell-folder))
  "One folder before it loads: its DIRECTORY, its NAME, and what its
top-level .asd files declare — DECLARED-SYSTEMs, one per file, by name.
SOURCE is where its code is read from (OWN-CELL-DIRECTORY): DIRECTORY
itself, but for an install of a folder this image ships."
  (directory nil)
  (source nil)
  (name "" :type string)
  (systems '() :type list))

(access (folder cell-folder))

(defun cell-folder-knowledge-p (folder)
  "Whether FOLDER is a knowledge cell: one of its systems says so."
  (some #'declared-system-knowledge folder.systems))

(defun folder-depends-on (folder &aux (own (mapcar #'declared-system-name folder.systems)))
  "The systems FOLDER's .asd files depend on, its own systems excluded."
  (remove-duplicates (remove-if (lambda (name) (member name own :test #'string-equal))
                                (loop for declared in folder.systems append declared.depends-on))
                     :test #'string-equal))

;;; The entry under the one folder says which cell is installed; the build
;;; says which code. An install leads into whichever tree the last build to
;;; boot the home pointed it at (REFRESH-INSTALLED-CELLS), and ASDF reads the
;;; link again at every search: a channel's LOAD-SYSTEM at its start, minutes
;;; after the boot. A process of another build booting the same home in
;;; between -- a second unit, the gateway an update is replacing -- turned
;;; that search into the other build's source compiled against this core: the
;;; channel kit's DEFPACKAGE clashed with the one already loaded, and every
;;; channel was refused until a restart. Read through this image's own folder,
;;; a load never depends on where a link leads at that moment.
(defun own-cell-directory (directory &optional (shipped-root (shipped-cells-directory)))
  "The folder this image reads DIRECTORY's code from, DIRECTORY an entry of
the one folder: this image's own shipped folder (SHIPPED-COUNTERPART) in
place of an install of one -- a link wherever it leads now, a copy another
build made -- else DIRECTORY itself."
  ;; The rules are REFRESH-INSTALLED-CELLS', which points the same entries
  ;; at the same folders: a link to the operator's own folder, to a folder
  ;; this image does not ship or to nothing, a copy this image made and a
  ;; folder of the operator's own read where they stand.
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (parent (uiop:pathname-parent-directory-pathname directory))
         (target (and shipped-root (symlink-target (merge-pathnames (folder-name directory) parent))))
         (marker (and shipped-root (not target) (probe-file (merge-pathnames +installed-marker+ directory)))))
    (or (cond (target
               (shipped-counterpart (uiop:merge-pathnames*
                                     (uiop:parse-native-namestring target :ensure-directory t)
                                     parent)
                                    shipped-root))
              (marker
               (let* ((lines (ignore-errors (marker-lines marker)))
                      (from (first lines))
                      (stamp (second lines)))
                 (and (plusp (length from))
                      (not (equal stamp *image-stamp*))
                      (shipped-counterpart (uiop:parse-native-namestring from :ensure-directory t)
                                           shipped-root)))))
        directory)))

(defun make-cell-folder (directory &aux (source (own-cell-directory directory)))
  "DIRECTORY as a CELL-FOLDER, its .asd files read as data from where its
code is read (OWN-CELL-DIRECTORY)."
  (%make-cell-folder
   :directory directory
   :source source
   :name (folder-name directory)
   :systems (mapcar
             (lambda (name)
               ;; read, never loaded; a file that does not read is a bare record ASDF fails at load
               (if-let (form (ignore-errors
                              (read-defsystem-form
                               (merge-pathnames (format nil "~a.asd" name) source) name)))
                 (let* ((plist (cddr form))
                        (layer (eq (getf plist :class) 'layer))
                        (components (getf plist :components))
                        (odd (and layer
                                  (listp components)
                                  (find-if-not #'file-component-p components))))
                   (make-declared-system
                    name
                    :layer layer
                    :knowledge (let ((properties (getf plist :properties)))
                                 (and layer (listp properties) (every #'consp properties)
                                      (eq t (cdr (assoc :knowledge properties)))))
                    :description (let ((text (getf plist :description)))
                                   (if (stringp text) text ""))
                    :serves (let ((properties (getf plist :properties)))
                              (and (listp properties) (every #'consp properties)
                                   (let ((serves (cdr (assoc :serves properties))))
                                     (and (listp serves) (every #'listp serves)
                                          (serves-entries serves)))))
                    :depends-on (loop for dependency in (let ((names (getf plist :depends-on)))
                                                          (and (listp names) names))
                                      when (or (stringp dependency) (symbolp dependency))
                                        collect (string-downcase (string dependency)))
                    :files (and layer (listp components) (not odd)
                                (mapcar #'second components))
                    :problem (cond ((not layer) nil)
                                   ((getf plist :pathname)
                                    "a layer's files sit beside its .asd; :pathname is not honoured")
                                   ((not (listp components))
                                    "a layer's :components is a list of (:file NAME)")
                                   (odd
                                    (format nil "a layer lists (:file NAME) components only; ~s is not one"
                                            odd)))))
                 (make-declared-system name)))
             ;; a primary system is named by its .asd file; sorted for a stable load
             (sort (mapcar #'pathname-name (asd-files source))
                   #'string<))))

(defun muffled-load (thunk &aux (*read-eval* t)
                                (*compile-verbose* nil)
                                (*compile-print* nil)
                                (*load-verbose* nil)
                                (*load-print* nil))
  "Run trusted cell source in THUNK with read-time evaluation enabled and
style-warnings, compiler notes, and compile/load chatter muffled: a layer
loading at boot must not paint notes about its own unused variables."
  ;; Cell source already executes code. Its reader must not inherit a tool
  ;; caller's restriction; the caller's policy is restored on return or unwind.
  ;; Real warnings and errors pass.
  (handler-bind ((style-warning #'muffle-warning)
                 (sb-ext:compiler-note #'muffle-warning))
    (funcall thunk)))

(defun missing-names-path (fasl)
  (make-pathname :type "missing" :defaults fasl))

(defun prune-stamp-cache (&optional (stamp *image-stamp*) (cache *layer-cache-directory*))
  "Keep STAMP's compiled directory under CACHE — the layers', the
peripherals' — and the newest one beside it; delete the rest."
  ;; Two builds are alive at once on a machine that warmed an update — the one
  ;; running and the one its successor boots — and a compile under either would
  ;; delete the other's whole directory, so the next launch of whichever lost
  ;; would recompile the layer. The newest two are what PRUNE-WARM-CACHE keeps
  ;; of the images, for the same reason.
  (when (and cache stamp)
    (let* ((directories (sort (ignore-errors (uiop:subdirectories cache))
                              #'> :key (lambda (path)
                                         (or (ignore-errors (file-write-date path)) 0))))
           (others (remove stamp directories :key #'folder-name :test #'equal)))
      (dolist (path (if (member stamp directories :key #'folder-name :test #'equal)
                        (cdr others)
                        (cddr others)))
        (ignore-errors (uiop:delete-directory-tree path :validate t))))))

(defvar *layer-compiled* nil
  "A table NAME -> the name of its fasl, of the layer files compiled while one
layer loads, whose earlier texts SWEEP-LAYER-CACHE deletes after it; NIL
outside a load.")

(defun sweep-layer-cache (compiled folder)
  "Delete what the fasls COMPILED names supersede in FOLDER's directory of the
layer cache — each file's earlier texts — and every stamp directory but this
one and the newest beside it. Never signals."
  ;; Once per load, never once per fasl: without this, every publish would
  ;; prune and list the directory again, so the listing would grow with the
  ;; layer and loads would slow. One readdir here, no truenames; a file whose
  ;; compile published nothing keeps its earlier texts.
  (ignore-errors
   (prune-stamp-cache)
   (sb-ext:map-directory
    (lambda (old &aux (candidate (or (pathname-name old) "")))
      ;; <name>-<twelve hex>.fasl, so `start-1.fasl' is not taken for `start''s
      (ppcre:register-groups-bind (base) ("^(.+)-[0-9a-fA-F]{12}\\z" candidate)
        (let ((current (gethash base compiled)))
          (when (and current
                     (member (pathname-type old) '("fasl" "missing") :test #'equal)
                     (string/= candidate current)
                     (probe-file (make-pathname :name current :type "fasl" :defaults old)))
            (ignore-errors (delete-file old))))))
    (merge-pathnames (format nil "~a/~a/" *image-stamp* folder) *layer-cache-directory*)
    :directories nil)))

(defun missing-name-entry (condition)
  "What CONDITION, a warning a compile signalled, says the compile could not
resolve, as (KIND PACKAGE NAME SETF-P): its end-of-unit note `undefined
function: NAME' (:FUNCTION) or `undefined variable: NAME' (:VARIABLE), or
`using the lexical binding of the symbol (NAME)' (:BINDING); NIL for any
other warning."
  (let* ((binding-p (typep condition 'sb-kernel:asterisks-around-lexical-variable-name))
         (arguments (and (typep condition 'simple-condition)
                         (simple-condition-format-arguments condition))))
    (when binding-p (push :binding arguments))
    (when (and (listp arguments) (= 2 (length arguments))
               (or binding-p
                   (and (member (first arguments) '(:function :variable))
                        (uiop:string-prefix-p "undefined " (princ-to-string condition)))))
      (let* ((name (second arguments))
             (setf-p (and (consp name) (eq 'setf (first name))))
             (symbol (if setf-p (second name) name)))
        (when (and symbol (symbolp symbol) (symbol-package symbol))
          (list (first arguments) (package-name (symbol-package symbol))
                (symbol-name symbol) setf-p))))))

(defun compile-layer-file (source fasl)
  "COMPILE-FILE SOURCE to FASL through a temporary name and one rename, so
two launches compiling the same file at once each publish a whole fasl."
  ;; => (values FASL MISSING). MISSING is every function and variable the
  ;; compile found no definition for, (KIND PACKAGE NAME SETF-P), and every
  ;; *NAME* it bound lexically, (:BINDING PACKAGE NAME NIL), counted in a
  ;; compilation unit of its own so a load inside another — a test-op — still
  ;; hears it, and written beside FASL before the fasl is published, so a later
  ;; launch reads the list back instead of compiling. FASL is NIL when the
  ;; compile signals or SBCL reports failure, leaving no fasl behind: the caller
  ;; loads the source, whose LOAD compiles the same forms again and signals what
  ;; this could not. MISSING is NIL when the compile signalled. It deletes
  ;; nothing: a published FASL is noted in *LAYER-COMPILED*, and the load
  ;; sweeps what it supersedes once, at its end.
  (let ((tmp (make-pathname :type (format nil "fasl-~d" (sb-posix:getpid))
                            :defaults fasl))
        (missing '()))
    (with-handlers ((error ()
                      (ignore-errors (delete-file tmp))
                      nil))
      (bind (((output _ failure-p)
              (handler-bind ((warning
                               (lambda (condition)
                                 (when-let (entry (missing-name-entry condition))
                                   (pushnew entry missing :test #'equal)
                                   (muffle-warning condition)))))
                (with-compilation-unit (:override t)
                  (compile-file source :output-file tmp)))))
        (setf missing (nreverse missing))
        (cond ((and output (not failure-p))
               (write-file-atomically (missing-names-path fasl)
                                      (with-standard-io-syntax (prin1-to-string missing)))
               (uiop:rename-file-overwriting-target output fasl)
               (when *layer-compiled*
                 (setf (gethash (pathname-name source) *layer-compiled*) (pathname-name fasl)))
               (values fasl missing))
              (t
               (when output (ignore-errors (delete-file output)))
               (values nil missing)))))))

(defvar *layer-fasl-paths* nil
  "A table PATH -> its fasl under the layer cache while one layer loads, so
the text is read and digested once per launch; NIL outside a load.")

(defun layer-fasl-path (path)
  "Where PATH's compiled form lives: <cache>/<stamp>/<folder>/<name>-<twelve
hex of the text's sha256>.fasl — the text is the key, the stamp directory
this build's alone."
  (values (alexandria:ensure-gethash
           path (or *layer-fasl-paths* (make-hash-table :test 'equal))
           (merge-pathnames (format nil "~a/~a/~a-~a.fasl"
                                    *image-stamp*
                                    (folder-name (uiop:pathname-directory-pathname path))
                                    (pathname-name path)
                                    (subseq (sha256-text (uiop:read-file-string path)) 7 19))
                            *layer-cache-directory*))))

;;; --- compiling a layer ahead, in a child ------------------------------------
;;; A layer file compiles in ~8 ms and a layer has hundreds, all of them at
;;; once when the stamp is new — a rebuild, an empty cache: 2.0 s of the
;;; launch (258 files), serial, because SBCL's compiler
;;; holds one lock per image. The files are independent as compilation
;;; units — each is one defining form, and what one file's compile takes
;;; from another it takes by name — so the missing fasls are dealt among
;;; forks of one fresh process of this organism: a `trial' child booted on a
;;; scratch home that links this home's folders but its layers, whose
;;; workers compile into the same cache while the launch loads behind them,
;;; in order, as it always has. One boot, not one per worker: eight booted
;;; children each loaded the peripherals (0.2 s of CPU) beside the launch,
;;; nine processes on six cores, so a boot took 0.46 s before its ~0.2 s of
;;; compiling; eight forks of one booted child, each compiling the heaviest
;;; file, were done in 226 ms. LAUNCH.LISP forks
;;; nothing because the launcher is threaded; a fresh `trial' child is not,
;;; and when it is, SB-POSIX:FORK refuses and the child compiles alone. The
;;; child starts before the launch's first folder loads, not at the layer's
;;; turn: its boot (0.30-0.45 s) loads the peripherals the launch loads
;;; first (~0.27 s), and the two ran one after the other (245 files).
;;; A stamp with no directory for the layer has every file missing, as many
;;; as its .asd declares, so the child launches before the launch probes
;;; and digests the texts, its stdin left open for the form written after:
;;; its `main' came 27-36 ms after the launch's `load-direct', not 51
;;; (245 files). A directory the stamp has is scanned first, and a child
;;; launched only for enough missing fasls.
;;; Each worker walks the load order and compiles
;;; the files it claims — a claim is one MKDIR of <claims>/<index>, which one
;;; process wins — so a worker done with a light file takes the next: static
;;; shares, worker K of N taking files K, K+N ..., left two heavy files (161
;;; and 117 ms) in one worker, done at 0.80 s where the other seven were done
;;; by 0.60 s, and the launch loaded nothing until the child exited, 0.45 s
;;; after the first fasls (the same layer). The launch
;;; waits for each file just before it loads it (AWAIT-LAYER-FASL), and a
;;; file no worker has claimed yet it claims and compiles itself, the one it
;;; needs next. A fasl that exists has its `.missing' list, renamed into
;;; place first. A worker killed mid-file leaves a claim with neither fasl
;;; nor `failed': the launch waits for that file until the child exits, then
;;; compiles it. A child killed while its orphaned forks compile can leave a
;;; file compiling in a fork and in the launch at once: each publishes a
;;; whole file through a temporary name and one rename, the same text under
;;; the same name. The child first raises its
;;; nursery to 64 MB: the organism's 16 MB cap cost the compile 115 ms of
;;; 1063 ms in collections, and eight forks at 256 MB each would hold too
;;; much transient memory. The child loads the peripherals at the moment the
;;; launch does, so on an empty ASDF cache an operator's own peripheral — in
;;; a checkout just rebuilt, every shipped one — compiles in two processes
;;; at once, not nine: ASDF writes through a temporary name and renames, so
;;; each publishes a whole fasl; only the work is repeated. What a worker
;;; cannot know is a macro, a setf expander or a symbol macro an EARLIER
;;; layer file defines: it compiles the use as a call and notes the name as
;;; unresolved, and LOAD-LAYER-FILE, meeting such a name that IS a macro by
;;; the time the file loads, compiles the file again here
;;; (COMPILED-PAST-A-MACRO-P). Nor a variable an earlier file proclaims
;;; special: a LET of *A* there compiles as a lexical binding, with no free
;;; reference to note, so the compile notes SBCL's style-warning about it as
;;; a :BINDING and the file compiles again here if *A* is special by the time
;;; it loads. A child or a worker that dies, overruns or cannot be spawned
;;; leaves its files to the serial path.

(defparameter +layer-compile-workers+ 8
  "Processes a layer's missing fasls are split among: the child and its forks.")

(defparameter +layer-compile-ahead-minimum+ 24
  "Fewer missing fasls than this compile serially: a child's boot (~0.4 s)
outweighs what it would save.")

(defun compile-layer-shares (pairs workers claims)
  "COMPILE-LAYER-FILE each of PAIRS, (SOURCE . FASL) namestrings, here and in
WORKERS-1 forks, each worker walking PAIRS in order and taking the ones it
claims under the directory CLAIMS; returns once all exit."
  ;; Pair I is claimed by making CLAIMS/I: one MKDIR, which one process wins
  ;; and every other loses to EEXIST. A compile that published no fasl makes
  ;; CLAIMS/I/failed, so the launch waiting for the fasl stops. A refused fork
  ;; — SB-POSIX:FORK signals while another thread runs — leaves nothing
  ;; behind: this process walks every pair too. A fork leaves by EXIT :ABORT
  ;; T: it unwinds into nothing and flushes nothing it shares with this process.
  ;; The compile's nursery is the compile's: the process's own comes back
  ;; after, or a gateway that compiled its layer at boot kept a 64 MB nursery
  ;; for its whole life -- and a shell that did came back to the organism's.
  (flet ((walk ()
           (loop for (source . fasl) in pairs
                 for index from 0
                 for claim = (format nil "~a~d" claims index)
                 when (ignore-errors (sb-posix:mkdir claim #o700) t)
                   do (unless (ignore-errors (compile-layer-file (pathname source) (pathname fasl)))
                        (ignore-errors (sb-posix:mkdir (format nil "~a/failed" claim) #o700))))))
    (with-nursery ((* 64 1024 1024))
      (let ((forks (loop repeat (1- workers)
                         for pid = (or #-win32 (ignore-errors (sb-posix:fork)))
                         when (eql pid 0)
                           do (unwind-protect (walk) (sb-ext:exit :code 0 :abort t))
                         collect pid)))
        (walk)
        #-win32 (dolist (pid (remove nil forks)) (ignore-errors (sb-posix:waitpid pid 0)))))))

(defstruct (layer-child (:copier nil) (:predicate nil)
                        (:constructor make-layer-child (pid process input)))
  "The `trial' child a layer compiles in: PID and PROCESS as SPAWN-PROGRAM
answered them, INPUT the stdin its form is written down, and STATUS its exit
status once seen."
  pid process input (status nil))

(access (child layer-child))

(defun layer-child-exited (child)
  "CHILD's exit status once it has exited, NIL while it runs."
  ;; The first sight of the exit reaps the child (CHILD-EXIT), so it is kept.
  (or child.status
      (setf child.status (child-exit child.pid child.process))))

(defun start-layer-compile (scan files folders)
  "Start compiling the files among the paths SCAN answers that have no fasl yet
in a child of this organism — booted on a scratch home linking FOLDERS (the
layer's siblings), forking its workers — when there are enough of them to be
worth it, and return at once; FILES, the layer's count when its cache directory
for this stamp is absent, launches the child before SCAN runs. => the handle
AWAIT-LAYER-FASL and FINISH-LAYER-COMPILE wait on, NIL when nothing started,
and the paths. Signals only what SCAN signals."
  (let ((command (ignore-errors (organism-command "trial"))))
    (flet ((launch (count)
             ;; its stdin stays open for the form, written once the paths are scanned
             (when (and command #+win32 nil (>= count +layer-compile-ahead-minimum+))
               (let* ((home (uiop:ensure-directory-pathname
                             (merge-pathnames (format nil "nodecode-layer-home-~d-~d"
                                                      (sb-posix:getpid) (get-internal-real-time))
                                              (uiop:temporary-directory))))
                      (cells (merge-pathnames "cells/" home)))
                 (ensure-directories-exist cells)
                 (ensure-directories-exist (merge-pathnames "claims/" home))
                 (dolist (folder folders)
                   (ignore-errors
                    (make-symlink (truename folder) (merge-pathnames (folder-name folder) cells))))
                 ;; (CHILDREN HOME STARTED PENDING), one child; nothing reads its output
                 (list (list (ignore-errors
                              (bind (((pid _ _ process input)
                                      (spawn-program (list* "/usr/bin/env"
                                                            (format nil "NODECODE_HOME=~a" (namestring home))
                                                            command)
                                                     :input :stream :output nil :error-output nil)))
                                (make-layer-child pid process input))))
                       home (get-internal-real-time) '())))))
      (let ((handle (and files (ignore-errors (launch files)))))
        ;; its stdin closes on every path: a `trial' child that reads no form exits 70
        (with-cleanup ((ignore-errors (close (layer-child-input (first (first handle))))))
          (let ((paths (funcall scan)))
            (ignore-errors
             (let ((pending (remove-if (lambda (path) (probe-file (layer-fasl-path path))) paths)))
               (setf handle (or handle (launch (length pending))))
               (when handle
                 (setf (fourth handle) pending)
                 ;; one per directory, not one per file: a folder's files share it
                 (mapc #'ensure-directories-exist
                       (remove-duplicates (mapcar #'layer-fasl-path pending)
                                          :key #'pathname-directory :test #'equal))
                 (let ((in (layer-child-input (first (first handle)))))
                   ;; printed as the child reads it, in EVOLVED: kernel names carry their
                   ;; package; the form reads under *READ-EVAL* NIL in the child: strings only
                   (write-line
                    (with-standard-io-syntax
                      (let ((*package* (find-package '#:nodecode.evolved)))
                        (prin1-to-string
                         `(let ((*package* (find-package ,(package-name *package*))))
                            (compile-layer-shares
                             ',(mapcar (lambda (path)
                                         (cons (namestring path)
                                               (namestring (layer-fasl-path path))))
                                       pending)
                             ,(min +layer-compile-workers+ (length pending))
                             ,(namestring (merge-pathnames "claims/" (second handle))))
                            1))))
                    in)
                   (finish-output in)))))
            (values handle paths)))))))

(defun await-layer-fasl (handle path)
  "Return once HANDLE's workers are done with PATH: its fasl exists, its
compile failed, the child exited, or 120 s passed since the child started — at
once for a PATH not among HANDLE's files, and for one no worker has claimed
yet, which it claims for this process to compile. Never signals."
  (ignore-errors
   (destructuring-bind (&optional children home started pending) handle
     (let ((index (position path pending :test #'equal)))
       (when (and (first children) index)
         (let ((fasl (layer-fasl-path path))
               (claim (format nil "~aclaims/~d" (namestring home) index)))
           (unless (or (probe-file fasl) (ignore-errors (sb-posix:mkdir claim #o700) t))
             (wait-until (lambda () (or (probe-file fasl)
                                        (probe-file (format nil "~a/failed" claim))
                                        (layer-child-exited (first children))))
                         (+ started (* 120 internal-time-units-per-second)) 0.002))))))))

(defun finish-layer-compile (handle)
  "Wait for HANDLE's children, each until 120 s after they started, delete
their scratch home and note their files in *LAYER-COMPILED*. Once per HANDLE:
a second call does nothing, and so does NIL. Never signals."
  (ignore-errors
   (destructuring-bind (&optional children home started pending) handle
     (when children
       (setf (first handle) nil)
       (with-cleanup ((ignore-errors (remove-tree home)))
         (dolist (child children)
           (when child
             (wait-until (lambda () (layer-child-exited child))
                         (+ started (* 120 internal-time-units-per-second)))
             (unless child.status
               ;; killed at the deadline, the child orphans its forks: each finishes its share,
               ;; exits -- the child alone, not its group
               #-win32 (ignore-errors (sb-posix:kill child.pid sb-posix:sigkill))
               (setf child.status (wait-for-child child.pid child.process)))
             (ignore-errors (close (layer-child-input child) :abort t))
             (when child.process (ignore-errors (sb-ext:process-close child.process))))))
       ;; a child sweeps nothing: what its fasls supersede is the load's to delete
       (when *layer-compiled*
         (dolist (path pending)
           (setf (gethash (pathname-name path) *layer-compiled*)
                 (pathname-name (layer-fasl-path path)))))))))

(defun compiled-past-a-macro-p (entry)
  "Whether the name a compile could not resolve — ENTRY, (KIND PACKAGE NAME
SETF-P) — is a macro, a setf expander or a symbol macro now, or the variable a
:BINDING entry bound lexically is special now: the fasl calls what must
expand, or binds lexically what must be rebound, and stands to be compiled
again."
  (destructuring-bind (kind package name setf-p) entry
    (when-let (symbol (and (find-package package) (find-symbol name package)))
      (case kind
        (:function (if setf-p
                       (sb-int:info :setf :expander symbol)
                       (macro-function symbol)))
        (:variable (eq (sb-int:info :variable :kind symbol) :macro))
        (:binding (eq (sb-int:info :variable :kind symbol) :special))))))

(defun missing-name-label (entry)
  "ENTRY as the boot line names it, lowercase: NAME in EVOLVED, where layer
files live, PACKAGE::NAME elsewhere, (SETF ...) around a setf function."
  (destructuring-bind (package name setf-p) (rest entry)
    (let ((qualified (if (string= package "NODECODE.EVOLVED")
                         name
                         (format nil "~a::~a" package name))))
      (string-downcase (if setf-p (format nil "(setf ~a)" qualified) qualified)))))

(defun missing-name-defined-p (entry)
  "Whether this image defines the name a compile could not resolve — ENTRY,
(KIND PACKAGE NAME SETF-P) — now: fbound; bound or proclaimed special."
  (destructuring-bind (kind package name setf-p) entry
    (when-let (symbol (and (find-package package) (find-symbol name package)))
      (if (eq kind :function)
          (fboundp (if setf-p (list 'setf symbol) symbol))
          (or (boundp symbol)
              (member (sb-int:info :variable :kind symbol)
                      '(:special :global :constant)))))))

(defun load-layer-file (path others provides &optional promised)
  "LOAD PATH's definition unless it would not run against this image => NIL
when it loaded, the reason it was skipped, as the boot line prints it, or
:WAIT and the names it waits for: what OTHERS may yet define."
  ;; Two reasons. An override whose shipped definition changed or vanished since
  ;; it was recorded (STALE-LAYER-FILE) is an edit of a text that is gone, and a
  ;; definition recorded as the layer's own that the shipped source took over
  ;; since is an older text of the shipped one. A file
  ;; whose compile found a function or variable missing that this image does not
  ;; define now and that no file in OTHERS — the layer's files not loaded yet —
  ;; defines (what it may define, memoized in PROVIDES) names what nothing will
  ;; give it: one naming what an upgrade took away does not load, and neither
  ;; does one naming a skipped file's definitions, missing by the time it is
  ;; judged — a skip takes back what the compile noted. A caller recorded ahead
  ;; of its helper WAITS for it and loads behind it, never on its word: loaded
  ;; ahead, it would stand in the image naming what the helper, skipped or failed
  ;; in its turn, never defined. PROMISED
  ;; loads on that word after all: for the files left waiting on each other
  ;; alone, which no order satisfies. What loads is the fasl this
  ;; image compiled from PATH's text, else the source itself — a compile that
  ;; failed, whose LOAD compiles the same forms again and signals what the
  ;; compile could not, so a file that cannot compile fails exactly as it always
  ;; has. A layer file is a defining form, which compiles and loads to the same
  ;; definition; a form that only LOAD's evaluation runs, an (EVAL-WHEN
  ;; (:EXECUTE)), is not what a layer records.
  ;; what LATER-PATH may define, for a caller compiled ahead of it: (EXACT LOOSE PREFIXES)
  (flet ((provided (later-path)
           (alexandria:ensure-gethash later-path provides
                     (let ((text (ignore-errors (uiop:read-file-string later-path)))
                           (exact '())
                           (loose '())
                           (prefixes '()))
                       (flet ((walk (form)
                                (dolist (leaf (alexandria:flatten form))
                                  (when (symbolp leaf)
                                    (let ((name (symbol-name leaf)))
                                      (push name loose)
                                      ;; a name of two characters or more ending in a hyphen
                                      (when (ppcre:scan ".-$" name) (push name prefixes)))))))
                         (dolist (hit (and text (file-definitions later-path)))
                           (destructuring-bind (key start end &optional home) hit
                             (let ((operator (getf key :operator))
                                   (name (getf key :name)))
                               (cond ((string= operator "DEFINE-HOOK"))
                                     ((and (member operator *defining-operators* :test #'string=)
                                           (not (member operator '("DEFCLASS" "DEFSTRUCT")
                                                        :test #'string=)))
                                      (push name exact))
                                     (t
                                      (push (format nil "~a-" name) prefixes)
                                      (push (format nil "MAKE-~a" name) loose)
                                      (push (format nil "COPY-~a" name) loose)
                                      (walk (ignore-errors
                                             (let ((*package* (if (packagep home)
                                                                  home
                                                                  (find-package '#:nodecode.evolved)))
                                                   (*read-eval* nil))
                                               (read-from-string text t nil
                                                                 :start start :end end)))))))))
                         (list exact loose prefixes))))))
    (if-let (stale (stale-layer-file path))
      (destructuring-bind (relative recorded current) (rest stale)
        (format nil "~:[the shipped source took it over~*~;the shipped definition it overrides ~
                     ~:[is gone~;changed~]~] (~a)"
                recorded current relative))
      (multiple-value-bind (fasl missing)
          ;; the fasl compiled from this text before, with its missing names, else a compile now
          (handler-case
              ;; <cache>/<stamp>/<folder>/<name>-<twelve hex of the text's sha256>.fasl:
              ;; the text is the key, the stamp directory this build's alone
              (let* ((fasl (layer-fasl-path path))
                     ;; (LIST) when the fasl and the list beside it both read, else NIL
                     (read-back (and (probe-file fasl)
                                     (ignore-errors
                                      (list (uiop:safe-read-file-form (missing-names-path fasl))))))
                     (names (first read-back)))
                ;; The list reads back whole or the file compiles again: a torn or
                ;; foreign list must not decide what loads.
                (if (and read-back
                         (listp names)
                         (every (lambda (entry)
                                  (and (listp entry) (= 4 (length entry))
                                       (member (first entry) '(:function :variable :binding))
                                       (stringp (second entry)) (stringp (third entry))))
                                names))
                    (values fasl names)
                    (progn (ensure-directories-exist fasl)
                           (compile-layer-file path fasl))))
            (error () nil))
        ;; a name the compile took for a function or a variable that is a macro
        ;; now: compiled in a child ahead of the layer file defining it
        ;; (START-LAYER-COMPILE), or turned into one since — again, here, in order
        (when (and fasl (some #'compiled-past-a-macro-p missing))
          (multiple-value-setq (fasl missing)
            (handler-case (compile-layer-file path fasl)
              (error () (values nil missing)))))
        (let* ((absent (remove-if #'missing-name-defined-p
                                  ;; a lexical binding names nothing the image lacks
                                  (remove :binding missing :key #'first)))
               ;; what no file of OTHERS may define either
               (orphans
                 (remove-if
                  (lambda (entry)
                    (destructuring-bind (name setf-p) (cddr entry)
                      (some (lambda (other)
                              ;; names compare without their package, the way definition keys do
                              (destructuring-bind (exact loose prefixes) (provided other)
                                (or (member (if setf-p (format nil "(SETF ~a)" name) name) exact
                                            :test #'string=)
                                    (member name loose :test #'string=)
                                    (some (lambda (prefix) (uiop:string-prefix-p prefix name))
                                          prefixes))))
                            others)))
                  absent)))
          (cond ((and absent (or orphans (not promised)))
                 ;; take back what the compile noted: a name still unbound goes back to unknown
                 (dolist (hit (file-definitions path))
                   (bind (((key _ _ home) (values-list hit))
                          (operator (getf key :operator))
                          (name (getf key :name))
                          (setf-p (uiop:string-prefix-p "(SETF " name))
                          (symbol (and (packagep home) (definition-name-symbol name home))))
                     (when symbol
                       (cond ((member operator '("DEFVAR" "DEFPARAMETER") :test #'string=)
                              (unless (boundp symbol)
                                (sb-int:clear-info :variable :kind symbol)
                                (sb-int:clear-info :variable :where-from symbol)))
                             ((member operator '("DEFUN" "DEFGENERIC" "DEFMETHOD" "DEFINE")
                                      :test #'string=)
                              (let ((function-name (if setf-p (list 'setf symbol) symbol)))
                                (unless (fboundp function-name)
                                  (ignore-errors (fmakunbound function-name)))))))))
                 (if orphans
                     (format nil "names ~{~a~^, ~}, which this image does not define"
                             (mapcar #'missing-name-label orphans))
                     (values :wait absent)))
                (t (load (or fasl path))
                   nil)))))))

(defparameter +load-summary-line+ "^[ ;]*(?:$|compilation unit |caught (?:.* )?condition)"
  "A line of SBCL's load chatter that says nothing of the failure: blank, or a unit summary.")

(defun call-recording-failure (cell file thunk &aux (diagnostics (make-string-output-stream)))
  "Run THUNK with its load diagnostics captured; an error becomes (FILE .
TEXT) on CELL's FAILURES and the record's state :failed, and NIL is
answered where success answers T. Never signals: a failure is the record's
fact, not the boot's."
  (handler-case
      (let ((*error-output* diagnostics))
        (funcall thunk)
        t)
    (error (condition)
      (setf cell.state :failed
            cell.failures
            (append cell.failures
                    ;; TEXT: CONDITION, then the load context SBCL wrote, minus unit summaries
                    (let ((context (remove-if (lambda (line) (ppcre:scan +load-summary-line+ line))
                                              (lines (get-output-stream-string diagnostics)))))
                      (list (cons file
                                  ;; minus every object's heap address: the text rides the prompt
                                  ;; prefix (the cells section), and a reader error names its
                                  ;; stream as #<... {ADDRESS}>, another address at every boot.
                                  ;; Without this, a failing layer file would be re-read every
                                  ;; conversation in full after each relaunch.
                                  (ppcre:regex-replace-all
                                   " ?\\{[0-9A-F]{6,}\\}"
                                   (format nil "~a~{~%~a~}"
                                           (handler-case (princ-to-string condition)
                                             (error () (string (type-of condition))))
                                           (mapcar (lambda (line) (string-right-trim '(#\Space) line))
                                                   context))
                                   ""))))))
      nil)))

(defvar *compile-lock-held* nil
  "Whether this thread holds the compile lock (CALL-WITH-COMPILE-LOCK); one
thread at a time can, holding the load lock.")

(defun call-with-compile-lock (thunk)
  "THUNK's values, called holding the lock every process of this user takes
around an ASDF compile and load under the cache *PERIPHERAL-CACHE-DIRECTORY*
lives in: one process compiles a folder's files at a time, and the next finds
them compiled."
  ;; Without this lock, ASDF publishes a fasl by deleting the one there,
  ;; renaming its own into place and taking the new one's truename, and loads
  ;; a fasl it judged current a moment before: two processes compiling one
  ;; folder into one cache lose each other's files in those gaps.
  ;; *CELL-LOAD-LOCK* is this process's; this one is the cache's (LOCK-FILE,
  ;; waiting): the kernel drops it with a process that dies holding it, and
  ;; the wait is the kernel's, never a poll. A record lock is the process's,
  ;; not a thread's, and closing any descriptor of it drops it: a nested load
  ;; holds it already, and the load lock keeps a second thread from opening one
  ;; beside. A wait is a boot phase. A NIL cache -- the hermetic posture --
  ;; takes none, and neither does a cache whose folder cannot be made (nothing
  ;; compiles into it) or Windows, where LOCK-FILE cannot wait.
  (bt2:with-recursive-lock-held (*cell-load-lock*)
    (let* ((path (and *peripheral-cache-directory* (not *compile-lock-held*)
                      (not (uiop:os-windows-p))
                      (ignore-errors (ensure-directories-exist
                                      (merge-pathnames "compile.lock" *peripheral-cache-directory*)))))
           (lock (and path (ignore-errors
                            (or (lock-file path)
                                (progn (note-boot-phase "compile-lock-wait" (namestring path))
                                       (lock-file path :wait t)))))))
      (with-cleanup ((when lock (unlock-file lock)))
        (let ((*compile-lock-held* (or *compile-lock-held* (and lock t))))
          (funcall thunk))))))

(defun load-layer-system (cell folder entry)
  "Load the layer system ENTRY -- (DECLARED SCRATCH PATHS FASL-PATHS HANDLE), as
LOAD-CELL-FOLDERS started it -- of FOLDER into CELL, one file at a time."
  (bind (((_ scratch paths fasl-paths handle) (values-list entry))
         (*layer-cache-directory* (or scratch *layer-cache-directory*))
         (provides (make-hash-table :test 'equal))
         (*layer-fasl-paths* fasl-paths)
         (*layer-compiled* (make-hash-table :test 'equal))
         ;; ((PATH . NAMES) ...): the files waiting for NAMES, which
         ;; a file not loaded yet may define (LOAD-LAYER-FILE)
         (waiting '()))
    (flet ((settle (path later &optional promised
                    &aux (started (get-internal-real-time)) (wait nil))
             ;; load PATH, skip it or leave it waiting => whether it is settled
             (call-recording-failure
              cell (file-namestring path)
              (lambda ()
                (multiple-value-bind (reason names)
                    (muffled-load
                     (lambda ()
                       (load-layer-file path (append (remove path (mapcar #'car waiting)) later)
                                        provides promised)))
                  (cond ((eq reason :wait) (setf wait (cons path names)))
                        (reason
                         (setf cell.skipped
                               (append cell.skipped
                                       (list (cons (file-namestring path) reason)))))))))
             (setf waiting (append (remove path waiting :key #'car) (and wait (list wait))))
             (unless wait
               ;; what the file cost, so a stall is named
               (setf cell.timings (append cell.timings (list (cons (file-namestring path)
                                                                   (elapsed-ms started)))))
               t)))
      (with-cleanup (;; the child, with nothing left to claim, exits; its files join the sweep
                     (finish-layer-compile handle)
                     (if scratch
                         (ignore-errors (remove-tree scratch))
                         ;; what this load's compiles superseded, in one listing
                         (when (plusp (hash-table-count *layer-compiled*))
                           (sweep-layer-cache *layer-compiled* folder.name))))
        (loop for (path . later) on paths
              ;; the fasl a new stamp lacks, from the children
              ;; started ahead: the wait is not the file's cost
              do (await-layer-fasl handle path)
                 (settle path later)
                 ;; a load may be what a waiting file waited for
                 (loop for ready = (find-if (lambda (names) (every #'missing-name-defined-p names))
                                            waiting :key #'cdr)
                       while ready do (settle (car ready) later)))
        ;; nothing more is coming: a waiting file has what it
        ;; names from the others waiting, or it is skipped
        (loop while (some (lambda (entry) (settle (car entry) '())) (copy-list waiting)))
        ;; and those name each other alone: on their word, in file order
        (dolist (path paths)
          (when (assoc path waiting) (settle path '() t)))))))

(defun load-cell-folders (folders owners before)
  "Freeze the image (every live system immutable), register FOLDERS with ASDF
and load them dependencies first, each judged from the snapshot the one before
left (BEFORE for the first) against OWNERS. => the records, in load order."
  (dolist (loaded (asdf:already-loaded-systems))
    (asdf:register-immutable-system loaded))
  (note-image-systems)
  ;; ASDF's output configuration is process state a dump hook clears, so it
  ;; is set at every load, not once at boot.
  (when-let (translations (cell-output-translations folders))
    (asdf:initialize-output-translations translations))
  (ignore-errors (prune-stamp-cache *image-stamp* *peripheral-cache-directory*))
  ;; Registered by where the code is read from, never by the entry: ASDF
  ;; searches these again at every LOAD-SYSTEM (OWN-CELL-DIRECTORY).
  (dolist (folder folders) (pushnew folder.source asdf:*central-registry* :test #'equal))
  ;; load order: peripherals before layers, by name, a folder after the siblings it :depends-on
  (let* ((folders (stable-sort (copy-list folders) #'string<
                               :key (lambda (folder)
                                      (if (some #'declared-system-layer folder.systems) "1" "0"))))
         (homes (loop for folder in folders
                      append (mapcar (lambda (declared) (cons declared.name folder.name))
                                     folder.systems)))
         ;; (FOLDER . the sibling folders its systems depend on)
         (pending (mapcar (lambda (folder)
                            (cons folder (remove-duplicates
                                          (loop for need in (folder-depends-on folder)
                                                for other = (cdr (assoc need homes :test #'string=))
                                                when other collect other)
                                          :test #'string=)))
                          folders))
         (loaded '())
         (ordered '())
         (records '())
         ;; (DECLARED SCRATCH PATHS FASL-PATHS HANDLE) per layer system
         (ahead '()))
    (loop while pending
          do (let ((ready (or (find-if (lambda (entry) (subsetp (cdr entry) loaded :test #'string=))
                                       pending)
                              (first pending))))
               (setf pending (remove ready pending))
               (push (cell-folder-name (car ready)) loaded)
               (push (car ready) ordered)))
    ;; A layer's missing fasls start compiling in children here, while the
    ;; folders before it load; its texts are read and digested once. It
    ;; compiles into a cache it can write, else into a throwaway one deleted
    ;; after, without children.
    (dolist (folder ordered)
      (dolist (declared folder.systems)
        (when declared.layer
          (let* ((scratch (unless (and *layer-cache-directory*
                                       (ignore-errors
                                        (ensure-directories-exist
                                         (folder-under *image-stamp* *layer-cache-directory*))))
                            (uiop:ensure-directory-pathname
                             (merge-pathnames (format nil "nodecode-layer-~d-~d"
                                                      (sb-posix:getpid) (get-internal-real-time))
                                              (uiop:temporary-directory)))))
                 (*layer-fasl-paths* (make-hash-table :test 'equal))
                 (scan (lambda ()
                         (remove-if-not
                          #'probe-file
                          (mapcar (lambda (file)
                                    (merge-pathnames (format nil "~a.lisp" file) folder.source))
                                  declared.files)))))
            (multiple-value-bind (handle paths)
                (if (or scratch declared.problem)
                    (values nil (funcall scan))
                    (start-layer-compile
                     scan
                     ;; no directory for the layer under this stamp: none of its fasls exists
                     (unless (probe-file (merge-pathnames (format nil "~a/~a/" *image-stamp* folder.name)
                                                          *layer-cache-directory*))
                       (length declared.files))
                     (mapcar #'cell-folder-source
                             (remove-if (lambda (other)
                                          (some #'declared-system-layer
                                                (cell-folder-systems other)))
                                        folders))))
              (push (list declared scratch paths *layer-fasl-paths* handle) ahead))))))
    ;; one record per folder; one failing as a unit is a (NIL . TEXT) failure and keeps BEFORE
    (with-cleanup ((dolist (entry ahead) (finish-layer-compile (fifth entry))))
      (dolist (folder (nreverse ordered) (nreverse records))
        (let ((cell (make-cell :name folder.name
                               :path folder.directory
                               :systems (mapcar #'declared-system-name folder.systems))))
          ;; Diagnostics a failing load prints — SBCL's "while evaluating the form
          ;; at line N" context, a compile-time error's body — belong on the record
          ;; beside the condition, not scattered across a boot terminal the TUI is
          ;; about to clear.
          (when (call-recording-failure
                 cell nil
                 (lambda (&aux (bundle (merge-pathnames "vendor/bundle.lisp" folder.source)))
                   ;; ql:bundle-systems' contract: load bundle.lisp and every
                   ;; vendored system becomes findable. Peripherals carry their
                   ;; dependencies this way, so the operator's box needs no
                   ;; quicklisp.
                   (when (probe-file bundle)
                     (muffled-load (lambda () (load bundle))))
                   (dolist (declared folder.systems)
                     (let ((name declared.name))
                       (when declared.layer
                         (setf cell.kind :layer))
                       ;; a layer file by file, never an ASDF op; the rest through ASDF, cleared first
                       (cond
                         (declared.problem (error "~a.asd: ~a" name declared.problem))
                         ((declared-system-layer declared)
                          (load-layer-system cell folder (assoc declared ahead)))
                         (t
                          (ignore-errors (asdf:clear-system name))
                          (call-with-compile-lock
                           (lambda () (muffled-load (lambda () (asdf:load-system name)))))))
                       ;; the peripheral mark: its package exports a fbound START-CELL
                       (bind ((package (find-package (string-upcase name)))
                              ((symbol status) (and package (find-symbol "START-CELL" package))))
                         (when (and symbol (eq status :external) (fboundp symbol))
                           (setf cell.start (fdefinition symbol))
                           (when (eq cell.kind :library) (setf cell.kind :peripheral))))))))
            (let ((after (definition-snapshot)))
              ;; fill CELL's lists and advance OWNERS: the next folder is judged against this one
              ;; a changed method count adds methods; a changed definition nobody here owns
              ;; overrides a shipped one, and one another folder owns collides with it
              (bind (((new changed) (snapshot-delta before after))
                     (redefined (remove :methods changed :key #'car)))
                (setf cell.defined new
                      cell.methods-added (remove-if-not (lambda (key) (eq (car key) :methods))
                                                         changed)
                      cell.overrides (remove-if (lambda (key) (gethash key owners)) redefined)
                      cell.collisions (loop for key in redefined
                                             for owner = (gethash key owners)
                                             when owner collect (cons key owner)))
                (dolist (key (append new redefined))
                  (setf (gethash key owners) cell.name)))
              (setf before after)))
          (push cell records))))))

(defun folder-records (folders owners before)
  "The records of FOLDERS: those LOAD-CELL-FOLDERS loads, from BEFORE against OWNERS, then
one per knowledge cell: on the report, never loaded -- nothing in it is evaluated (knowledge.lisp)."
  (append (when-let (loaded (remove-if #'cell-folder-knowledge-p folders))
            (load-cell-folders loaded owners before))
          (loop for folder in folders
                when (cell-folder-knowledge-p folder)
                  collect (make-cell :name folder.name :path folder.directory :kind :knowledge
                                     :systems (mapcar #'declared-system-name folder.systems)))))

(defun load-cells (&key (root *cells-directory*))
  "Load every cell folder under ROOT and return the records, which are
also (CELLS) from now on."
  ;; Runs at the top of MAIN in every process.
  ;;
  ;; The image is frozen first: every system already alive is registered
  ;; immutable. Without this, a folder's :depends-on on the core would
  ;; recompile or reload it. Every folder's .asd is read as data once, for
  ;; the order and for a layer's file list; all folders are registered with
  ;; ASDF before any loads, so cross-folder dependencies resolve; then
  ;; each loads in name order — dependencies first — under its own error
  ;; boundary.
  (bt2:with-recursive-lock-held (*cell-load-lock*)
    (let ((folders (mapcar #'make-cell-folder (cell-folders root)))
          ;; The pristine floor: EVOLVED as the image booted, read before a
          ;; single layer file has loaded. Held for the life of the image so
          ;; REVERT can put a definition back with one SETF — no reader, no
          ;; compiler, no source tree (provisional.lisp). Only the first load
          ;; of a process sets it; a later reload would be reading a composed
          ;; image and calling it pristine.
          (pristine (definition-snapshot)))
      (setf *cells* '())
      (unless *pristine-definitions*
        (setf *pristine-definitions* pristine))
      (setf *cells* (folder-records folders (make-hash-table :test 'equal) pristine))
      ;; A cell whose folder is gone is a failure of this load, said out
      ;; loud, whether or not any folder loaded — the walk above cannot see
      ;; one at all.
      (setf *cells*
            (append *cells*
                    ;; each failed as a unit, its one line naming where the link still points
                    (mapcar (lambda (name &aux (path (merge-pathnames name root)))
                              (make-cell :name name
                                          :path path
                                          :state :failed
                                          :failures
                                          (list (cons nil
                                                      (format nil "the installed folder is gone~
                                                                   ~@[ (the link points at ~a)~]; relink it, ~
                                                                   or (install-cell ~s) to put the shipped ~
                                                                   one back"
                                                              (ignore-errors (symlink-target path))
                                                              name)))))
                            (broken-cell-links root))))
      *cells*)))

(defun layer-revision ()
  "The revision of the organism's definitions for provenance: `<layer>:<short
sha>` for the layer the cell report names (the one layer
loaded, or the first of several) when it has a commit, `base` when the image
runs on the shipped source alone."
  ;; The seven-character short sha is read from the repository files, not
  ;; asked of git: this runs under the shell's first frame.
  (or *layer-revision*
      (setf *layer-revision*
            (let* ((layer (find :layer *cells* :key #'cell-kind))
                   (sha (and layer (git-head-sha layer.path))))
              (if sha
                  (format nil "~a:~a" layer.name (subseq sha 0 7))
                  "base")))))

;;; --- the boot line --------------------------------------------------------

(defun snapshot-key-label (key)
  "A snapshot key — (KIND . SYMBOL) — as a boot-line word."
  (format nil "~(~a~)~:[~; (~(~a~))~]"
          (cdr key) (not (eq (car key) :function)) (car key)))

(defparameter +slow-layer-file-ms+ 250
  "A layer file whose load cost this many milliseconds or more is named on
the board and in the report: every direct launch pays it again.")

(defun describe-cells (&optional (cells *cells*) (stream *standard-output*)
                        &aux (skipped '()) (slow '()))
  "One line per folder — what loaded, what it overrides, what collided — one
SKIPPED line per layer file that would not run against this image, with the
reason, and one FAILED line per failure, naming the layer file when there
is one, for
the serve log, /cells, and the operator's terminal at boot."
  ;; A folder that failed as a unit has no summary line: its kind and count
  ;; would describe nothing. Prints nothing when the folder is empty.
  ;;
  ;; The layer files a build skips, and those whose load cost is
  ;; +SLOW-LAYER-FILE-MS+ or more in a boot, are also two standing notices
  ;; (`layer' and `layer-slow'), each cleared when it names no file: the board
  ;; every shell and `gateway status' reads, not only the boot line. A defvar
  ;; whose initform sleeps, waits on a socket or reads the world would stall
  ;; every direct launch, and nothing else names the file.
  (dolist (cell cells)
    (unless (assoc nil cell.failures)
      (format stream "~&cell ~a: ~(~a~), ~d definition~:p~
                        ~@[, overrides ~{~a~^ ~}~]~
                        ~@[, adds methods on ~{~a~^ ~}~]~
                        ~@[, COLLIDES ~{~a~^ ~}~]~
                        ~:[~;, started~]~:[~;, REFUSED~]~@[, ~a~]~%"
                cell.name cell.kind
                (if (eq cell.kind :knowledge)
                    (length (directory (merge-pathnames "*.lisp" cell.path)))
                    (length cell.defined))
                (mapcar #'snapshot-key-label cell.overrides)
                (mapcar #'snapshot-key-label cell.methods-added)
                (loop for (key . owner) in cell.collisions
                      collect (format nil "~a with ~a" (snapshot-key-label key) owner))
                cell.started-p
                cell.refusal
                ;; the generation the folder stands at, said once, on the layer's line
                (and (eq cell.kind :layer) (ignore-errors (generation-label)))))
    (loop for (file . reason) in cell.skipped
          do (format stream "~&cell ~a: SKIPPED ~a: ~a~%" cell.name file reason)
             (push (format nil "~a/~a (~a)" cell.name file reason) skipped))
    (loop for (file . ms) in cell.timings
          when (>= ms +slow-layer-file-ms+)
            do (format stream "~&cell ~a: SLOW ~a: ~d ms to load~%" cell.name file ms)
               (push (format nil "~a/~a (~d ms)" cell.name file ms) slow))
    (dolist (failure cell.failures)
      (when (car failure)
        (push (format nil "~a/~a (~a)" cell.name (car failure) (failure-line (cdr failure)))
              skipped))
      (format stream "~&cell ~a: FAILED ~@[~a: ~]~a~@[ — ~a~]~%"
              cell.name (car failure) (cdr failure)
              ;; a folder from before the rename names `nodecode-lisp': relink shipped, rewrite owned
              (let ((text (princ-to-string (cdr failure))))
                (when (search "nodecode-lisp" text)
                  (let* ((shipped (ignore-errors (find-offer cell.name)))
                         (link (and *cells-directory*
                                    (ignore-errors
                                      (namestring (merge-pathnames cell.name *cells-directory*)))))
                         (path (ignore-errors (and cell.path (namestring cell.path)))))
                    (cond ((and shipped (offer-directory shipped) link)
                           (format nil "this folder predates the rename; relink it at the shipped one: ~
                                        ln -sfn ~a ~a"
                                   (namestring (offer-directory shipped)) link))
                          (path
                           (format nil "this folder predates the rename; rewrite its names where it ~
                                        stands and it keeps every definition: ~
                                        sed -i 's/nodecode-lisp\\./nodecode./g; ~
                                        s/\"nodecode-lisp\"/\"nodecode\"/g' ~a*.lisp ~a*.asd"
                                   path path))
                          (t "this folder predates the rename: its .asd names the system ~
                  `nodecode-lisp', which this image does not provide"))))))))
  (flet ((note (key control rows)
           (note-standing key (and rows (format nil control (length rows) rows)) :warning)))
    (note "layer" "this build did not load ~d layer file~:p: ~{~a~^, ~}" (reverse skipped))
    (note "layer-slow" "~d layer file~:p stalled this launch: ~{~a~^, ~}" (reverse slow)))
  (length cells))

(defun failure-line (text)
  "A layer file's failure TEXT as one line short enough to name beside it:
the condition and what SBCL said of where, never the stream it read."
  (one-line (subseq text 0 (search "Stream:" text)) :cap 240))

(defun broken-layer-files (&optional (cells *cells*))
  "Every file of a layer this image did not load, with why: ((PATH . REASON)
...), PATH the file's native namestring and REASON one line, the skipped in
load order and then the failed."
  ;; What the model is handed to mend (the gateway's HAND-BROKEN-LAYER):
  ;; the layer is its own definitions, and a release that moves the core
  ;; under one leaves it standing in no image until it is defined again or
  ;; forgotten.
  (loop for cell in cells
        when (eq cell.kind :layer)
          append (flet ((path (file)
                          (sb-ext:native-namestring (merge-pathnames file cell.path))))
                   (append (loop for (file . reason) in cell.skipped
                                 collect (cons (path file) reason))
                           (loop for (file . text) in cell.failures
                                 when file collect (cons (path file) (failure-line text)))))))

(defun cells-folder-text (&optional names)
  "DESCRIBE-CELLS' lines for every record (or the folders NAMES) as one
text, NIL when the folder is empty."
  ;; Static for the life of a load -- it changes at RESTART-CELLS and nowhere
  ;; else -- which is what lets it ride the prompt prefix (the cells harness
  ;; section).
  (when-let (records (if names
                         (remove-if-not (lambda (cell) (member cell.name names :test #'string=))
                                        *cells*)
                         *cells*))
    (string-right-trim '(#\Newline)
                       (with-output-to-string (out)
                         (describe-cells records out)))))

(defun standing-notices-text ()
  "The board as one text, one notice per line, NIL when it is empty."
  ;; Live: it changes whenever a peripheral speaks, so it rides the tail of
  ;; the prompt (context.lisp READ-LIVE-SECTIONS), never the cached prefix.
  (when-let (board (standing-notices)) (format nil "~{~a~^~%~}" (mapcar #'second board))))

(defun cells-report-text (&optional names &aux (folder (cells-folder-text names))
                                                (board (standing-notices-text)))
  "The folder and the board as one text: CELLS-FOLDER-TEXT, then
STANDING-NOTICES-TEXT."
  ;; NIL when there is nothing to say -- no folder, empty board. What
  ;; RESTART-CELLS answers and what /cells renders after the lines; the
  ;; model reads the two halves apart, each where its rate of change belongs.
  (when (or folder board)
    (format nil "~@[~a~]~:[~;~%~]~@[~a~]" folder (and folder board) board)))

;;; --- the shipped catalog, and putting a shipped folder in the one folder -----
;;; The binary travels with its source (paths.lisp SOURCE-ROOT) and, beside
;;; that source, with every cell folder the tree ships. Installing one is
;;; putting it under *CELLS-DIRECTORY* — a symlink where the platform has
;;; them, a copy where it does not — and presence does the rest at the next
;;; boot, or now through LOAD-CELL. Nothing here names a cell: the
;;; catalog is whatever carries an .asd beside the source.

(defun shipped-cells-directory (&optional (source *source-root*))
  "The cell folders shipped beside the source tree — <source>/../cells/
— as a directory pathname, or NIL when this image carries no source or the
tree ships none."
  (when source
    (probe-file (merge-pathnames "../cells/" source))))

(defun shipped-fasl-directory (&optional (shipped (shipped-cells-directory)))
  "Where the shipped folders' compiled files live once a release laid the
directory down: <source>/../fasl/, beside the folders they compile from."
  ;; `just tarball' compiles every shipped folder into it (pack.sh), so a
  ;; launch where the directory exists loads and never compiles. NIL when no
  ;; such directory exists — a checkout, a Windows install, a tree that shipped
  ;; none — and the folders compile into ASDF's user cache as they always have.
  (when shipped
    (probe-file (merge-pathnames "../fasl/" shipped))))

(defun cell-output-translations (folders &aux (shipped (shipped-cells-directory))
                                                (fasl (and shipped (shipped-fasl-directory shipped))))
  "ASDF's output configuration for a load of FOLDERS: every source under the
shipped folders compiles beside them (SHIPPED-FASL-DIRECTORY), every other
peripheral into a directory of its own under this build's stamp
(*PERIPHERAL-CACHE-DIRECTORY*), and everything else into the user cache as
before. NIL when neither applies."
  ;; ASDF judges a fasl by its date against the core's load, which a dumped
  ;; image carries from its build. Without a directory per stamp, a fasl
  ;; compiled after the image was dumped would read as current and keep that
  ;; build's macro expansions: a checkout's image beside the installed
  ;; release, or a release built before it was installed. A directory per stamp
  ;; is the layer cache's answer to that hazard. A folder is matched by its
  ;; truename, as ASDF resolves it: a link into the shipped tree takes the
  ;; first rule, a link to the operator's own checkout its own.
  (when-let (rules (append
                    (when fasl
                      (list (list (namestring shipped) (namestring fasl))))
                    (when *peripheral-cache-directory*
                      (loop for folder in folders
                            for directory = (ignore-errors (truename folder.source))
                            when (and directory
                                      (notevery #'declared-system-layer folder.systems)
                                      (not (and shipped (uiop:subpathp directory shipped))))
                              collect (list (namestring directory)
                                            (namestring
                                             (merge-pathnames
                                              (format nil "~a/~a/" *image-stamp* folder.name)
                                              *peripheral-cache-directory*)))))))
    (list* :output-translations (append rules (list :inherit-configuration)))))

(define-record (offer (:copier nil) (:export :readers))
  "One cell this image can install: a folder shipped beside the source, or
an entry of one of the hub's two indexes at a pinned commit."
  ;; NAME is the folder's name once installed — its primary system's name,
  ;; ASDF's rule that a system is named by its .asd; DIRECTORY where a shipped
  ;; one ships from, NIL for a hub entry; GIT and COMMIT the https URL a hub
  ;; entry clones and the full commit id it is pinned at, NIL for a shipped
  ;; one; PATH the folder inside that repository the entry is, NIL for its
  ;; top; SYSTEMS what its .asd files declare; DEPENDS-ON the system names
  ;; those declare, own systems excluded; MISSING the dependencies this image
  ;; can neither find loaded, nor shipped, nor on its registry — a folder with
  ;; any is offered as unavailable, with the reason, rather than installed
  ;; blind and failed at boot. DESCRIPTION is the primary system's
  ;; :description, what a chooser shows beside the name. SERVES is what it says
  ;; it serves (SERVES-ENTRIES), which a consumer may offer it by before it is
  ;; installed: the /models picker, its providers. INDEX is which of the
  ;; hub's indexes listed it, "hub" for Nodecode's own and "community" for
  ;; the one people submit to; NIL for a shipped folder. ARCHIVE and SHA256
  ;; are the https URL of a .tar.gz of a PATH entry's folder at its commit
  ;; and that file's sha256, when its index carries them (HUB-ARCHIVE-CHECKOUT).
  (name "" :type string)
  (directory nil)
  (git nil)
  (commit nil)
  (path nil)
  (systems '() :type list)
  (depends-on '() :type list)
  (missing '() :type list)
  (description "" :type string)
  (serves '() :type list)
  (index nil)
  (archive nil)
  (sha256 nil))

(access (entry offer) (provider offer))

(defvar *image-systems* '()
  "(NAME LOADED FILE) for every system ASDF knew when this image was saved or
last froze (LOAD-CELL-FOLDERS): whether it was loaded, and the file that
defines it. What UNAVAILABLE-SYSTEMS answers from without asking ASDF.")

(defun note-image-systems (&aux (loaded (asdf:already-loaded-systems)))
  "Take *IMAGE-SYSTEMS* off ASDF, at a moment nothing loads beside: a save,
one thread left, or a freeze, holding the load lock."
  (setf *image-systems*
        (mapcar (lambda (name &aux (system (asdf:registered-system name)))
                  (list name
                        (and (member name loaded :test #'string=) t)
                        (and system (asdf:system-source-file system))))
                (asdf:registered-systems))))

(pushnew 'note-image-systems sb-ext:*save-hooks*)

(defun unavailable-systems (names &optional (shipped-systems '()))
  "The system NAMES this image can find neither among SHIPPED-SYSTEMS, nor
loaded, nor defined by a file still there."
  ;; ASDF is read under the load lock, like every load: a load defines its
  ;; system again, and ASDF's redefinition changes the class of the very
  ;; object this walk reads, through a class with no times. Reading ASDF
  ;; beside such a load would fail with `The slot OPERATION-TIMES is
  ;; unbound' and `No defined method for COMPONENT-OPERATION-TIME'.
  ;;
  ;; But a load holds that lock for as long as it compiles -- seconds cold,
  ;; longer waiting on another process's compile into the cache -- and
  ;; /cells, the walk's Cells step and its summary list on the shell's
  ;; own thread would freeze behind a held lock. So a shipped name asks
  ;; nothing, and a name the image carries -- the core, its libraries,
  ;; every name a shipped folder stands on -- is answered off
  ;; *IMAGE-SYSTEMS*, its file probed now. ASDF is asked, under the lock,
  ;; only for a name the image never knew.
  (remove-if (lambda (name &aux (known (assoc name *image-systems* :test #'string-equal)))
               (or (member name shipped-systems :test #'string-equal)
                   (if known
                       (destructuring-bind (loaded file) (rest known)
                         (or loaded (null file) (probe-file file)))
                       (bt2:with-recursive-lock-held (*cell-load-lock*)
                         (or (find name (asdf:already-loaded-systems) :test #'string-equal)
                             ;; defined by a file still there: a dumped image keeps
                             ;; its build's systems, source paths included
                             (when-let (system (ignore-errors (asdf:find-system name nil)))
                               (let ((file (asdf:system-source-file system)))
                                 (or (null file) (probe-file file)))))))))
             names))

(defun shipped-cells (&key (root (shipped-cells-directory)))
  "Every cell folder shipped under ROOT as OFFER records, by name."
  ;; Empty when nothing ships.
  (when (and root (probe-file root))
    (let* ((entries
             (mapcar (lambda (directory)
                       (let* ((folder (make-cell-folder directory))
                              (systems (mapcar #'declared-system-name folder.systems)))
                         (make-offer :name (or (first systems) (folder-name directory))
                                     :directory directory
                                     :systems systems
                                     :depends-on (folder-depends-on folder)
                                     :description
                                     (if-let (primary (first folder.systems))
                                       primary.description "")
                                     :serves (when-let (primary (first folder.systems))
                                               primary.serves))))
                     ;; ROOT's directories with an .asd, one level of grouping allowed (channels/)
                     (loop for directory in (uiop:subdirectories root)
                           ;; template/ is what a new peripheral is copied from, not a cell
                           unless (string= (folder-name directory) "template")
                             append (if (asd-files directory)
                                        (list directory)
                                        (remove-if-not #'asd-files
                                                       (uiop:subdirectories directory))))))
           (shipped-systems (loop for entry in entries append entry.systems)))
      (dolist (entry entries)
        (setf entry.missing (unavailable-systems entry.depends-on shipped-systems)))
      (sort entries #'string< :key #'offer-name))))

(defun missing-shipped-cells (&key (root *cells-directory*) (shipped (shipped-cells)))
  "The shipped cells absent from ROOT, in catalog order."
  ;; NIL when ROOT is not present, as in the hermetic posture. Cell presence
  ;; remains the operator's consent; this only reports what an explicit /cells
  ;; could turn on.
  (when root
    (remove-if (lambda (entry)
                (installed-cell-p entry.name root))
              shipped)))

(defun refresh-shipped-cell-notice (&key (root *cells-directory*)
                                          (shipped (shipped-cells)))
  "Synchronize the standing notice for shipped folders absent from ROOT."
  ;; The notice is live-only and names /cells as the explicit installation
  ;; path; clearing it after the last folder lands keeps the report truthful.
  ;; It names a folder once per home, the names kept in cells-named; without
  ;; this, a home that answers Minimal would read all shipped cells on every
  ;; launch. The board is the model's notices block too. A folder a later
  ;; release ships is named when it first boots.
  (let* ((named (ignore-errors (uiop:read-file-lines (home "cells-named"))))
         (missing (remove-if (lambda (entry) (member (offer-name entry) named :test #'string=))
                             (missing-shipped-cells :root root :shipped shipped))))
    (when missing
      (ignore-errors (write-file-atomically (home "cells-named")
                                            (format nil "~{~a~%~}" (append named (mapcar #'offer-name missing))))))
    ;; Short names and an info tone: four wrapped warning lines of
    ;; nodecode-* package names. Nothing is wrong while a folder waits to
    ;; be chosen.
    (note-standing
     "nodecode-shipped-cells"
     (and missing
          (format nil "~d cell~:p not installed yet: ~{~a~^, ~} · /cells turns any of them on"
                  (length missing)
                  (mapcar (lambda (entry &aux (name (offer-name entry)))
                            (if (uiop:string-prefix-p "nodecode-" name) (subseq name 9) name))
                          missing)))
     :info)))

(defun find-offer (name &optional (offers (shipped-cells)))
  (find name offers :key #'offer-name :test #'string=))

(defun offer-closure (names &optional (offers (shipped-cells)) &aux (ordered '()))
  "NAMES with every offered folder their systems depend on, transitively,
dependencies first and without duplicates: choosing an adapter brings its
kit, choosing a folder that stands on another brings that one."
  (labels ((provider-of (system)
             (find-if (lambda (entry) (member system entry.systems :test #'string-equal)) offers))
           (visit (name &aux (entry (find-offer name offers)))
             (when (and entry (not (member name ordered :test #'string=)))
               (dolist (dependency entry.depends-on)
                 (when-let (provider (provider-of dependency))
                   (visit provider.name)))
               (push name ordered))))
    (dolist (name names) (visit name)))
  (nreverse ordered))

(defun cell-dependents (name &optional (root *cells-directory*))
  "The folders under ROOT, NAME's aside, whose systems depend on one NAME's
folder declares: what would fail at the next boot were NAME taken out."
  ;; Read off the folders themselves, not the offers: a hub install stands on
  ;; a shipped folder as its own .asd says, with no index at hand.
  (let* ((folders (mapcar #'make-cell-folder (cell-folders root)))
         (own (find name folders :key #'cell-folder-name :test #'string=))
         (systems (and own (mapcar #'declared-system-name (cell-folder-systems own)))))
    (loop for folder in folders
          when (and (not (eq folder own))
                    (loop for declared in folder.systems
                            thereis (intersection systems declared.depends-on :test #'string-equal)))
            collect folder.name)))

;;; Its presence tells an uninstall the tree is the installer's to remove and
;;; not the operator's own folder; its stamp tells a boot of any other build
;;; the copy is not its own.
(defparameter +installed-marker+ ".shipped"
  "The file a copied install carries: the folder it was copied from on its
first line, the stamp of the image that copied it (*IMAGE-STAMP*) on its
second.")

(defun marker-lines (path)
  "The lines of the install marker at PATH, each trimmed of blanks and returns."
  (mapcar (lambda (line) (string-trim '(#\Return #\Space) line)) (uiop:read-file-lines path)))

(defun installed-cell-p (name &optional (root *cells-directory*))
  "Whether the folder NAME is present under ROOT and resolves."
  (and root
       (probe-file (merge-pathnames (format nil "~a/" name) root))
       t))

(defun copy-install (from target)
  "Copy the shipped folder FROM to TARGET and mark the copy the installer's,
FROM and this image's stamp in the marker. => TARGET."
  (copy-tree-preserving from target)
  (with-output-file (out (merge-pathnames +installed-marker+ target))
    (write-line (sb-ext:native-namestring from) out)
    (write-line *image-stamp* out))
  target)

;;; --- the hub: folders beyond the release, each at a commit a person read ------
;;; Two indexes list them, in one format. Nodecode's own cells are in
;;; index.json in nodecode-ai/cells, and arrive there by pull request; the
;;; ones other people publish are submitted at nodecode.ai, where the site pins
;;; the commit and a person at Nodecode reads it before it is listed in the
;;; community index the site serves. A name Nodecode's index lists is never
;;; the community's. Each index is one JSON file, an entry per folder: its
;;; name, the https URL it clones from, the full commit id it is pinned at,
;;; the folder inside that repository when it is not the whole of it (one
;;; repository may carry several), its primary system's :description and
;;; :depends-on, and the platforms it runs on when not all three. A person reads an entry's code before it
;;; merges; installing one clones that commit, and nothing here moves it. An
;;; entry stands on the core, the shipped folders and what it vendors, never
;;; on another entry.

(define-startup-parameter *hub-index-url*
  (or (json-value (uiop:getenv "NODECODE_HUB_INDEX") :text)
      "https://nodecode.ai/dl/cells/index.json")
  "Where Nodecode's own index is read from; NIL leaves it out.
$NODECODE_HUB_INDEX names another: a fork's, a local one.")

(define-startup-parameter *community-index-url*
  (or (json-value (uiop:getenv "NODECODE_COMMUNITY_INDEX") :text)
      "https://nodecode.ai/cells/community.json")
  "Where the community's index is read from: the cells people submitted at
nodecode.ai and a person there read. NIL leaves it out; with both indexes
NIL the shipped folders are offered alone. $NODECODE_COMMUNITY_INDEX names
another.")

(defparameter +hub-platform+ #+win32 "windows" #+darwin "macos" #-(or win32 darwin) "linux"
  "This image's word in an index entry's platforms.")

(defparameter +hub-marker+ ".hub"
  "The file a hub install carries: the URL it was cloned from on its first
line, the commit it is pinned at on its second, and the folder inside that
repository it is on a third when it is not the whole of it.")

(defun read-hub-index (source &key (shipped (shipped-cells)) ((:index listed-by) "hub"))
  "The entries of the hub index SOURCE — JSON text or octets — this platform
runs, as OFFER records by name, each marked as listed by INDEX."
  ;; Refuses an index that is not format 1 and an entry that breaks it, by
  ;; name: a name that could not be a folder, a URL that is not https://, a
  ;; commit that is not a full id — a branch or a short id is a moving target,
  ;; and the pin is what a review read — a path that climbs out of its
  ;; repository or into a dot folder, and an archive that is not https:// or
  ;; comes without its sha256. An index that lies is refused whole,
  ;; never read around.
  (let ((index (decode-json source :whole t))
        (shipped-systems (loop for entry in shipped append entry.systems)))
    (unless (eql 1 (json-value index :integer "format"))
      (error "the hub index is not format 1"))
    (sort
     (loop for item across (json-array index "cells")
           for name = (json-value item :text "name")
           for git = (json-value item :text "git")
           for commit = (json-value item :text "commit")
           for path = (json-value item :text "path")
           for archive = (json-value item :text "archive")
           for sha256 = (json-value item :text "sha256")
           for platforms = (json-value item :array "platforms")
           for depends-on = (loop for system across (json-array item "depends_on")
                                  when (stringp system) collect (string-downcase system))
           for serves = (multiple-value-bind (array present) (gethash "serves" item)
                          (cond ((not present) '())
                                ((and (vectorp array) (not (stringp array))
                                      (every (lambda (each)
                                               (and (hash-table-p each) (plusp (hash-table-count each))
                                                    (loop for value being the hash-values of each
                                                          always (stringp value))))
                                             array))
                                 (serves-entries (coerce array 'list)))
                                (t (error "hub entry ~a: serves is an array of objects whose values are text"
                                          name))))
           do (unless (and name (ppcre:scan "^[a-z0-9][a-z0-9._-]*\\z" name))
                (error "hub entry ~s: a name is a-z, 0-9, . _ and -, first a letter or digit" name))
              (unless (and git (uiop:string-prefix-p "https://" git))
                (error "hub entry ~a: git is an https:// URL, not ~s" name git))
              (unless (and commit (ppcre:scan "^[0-9a-f]{40}\\z" commit))
                (error "hub entry ~a: commit is a full 40-hex id, not ~s" name commit))
              (unless (or (null path) (ppcre:scan "^[A-Za-z0-9_][A-Za-z0-9._-]*(/[A-Za-z0-9_][A-Za-z0-9._-]*)*\\z" path))
                (error "hub entry ~a: path is a folder inside the repository, a/b, not ~s" name path))
              (unless (or (and (null archive) (null sha256))
                          (and archive (uiop:string-prefix-p "https://" archive)
                               sha256 (ppcre:scan "^[0-9a-f]{64}\\z" sha256)))
                (error "hub entry ~a: archive is an https:// URL beside its 64-hex sha256" name))
           ;; a name the release ships is the shipped folder's: the hub adds, never replaces
           when (and (or (null platforms) (find +hub-platform+ platforms :test #'equal))
                     (not (find name shipped :key #'offer-name :test #'string=)))
             collect (make-offer :name name :git git :commit commit :path path :index listed-by
                                 :archive archive :sha256 sha256
                                 :systems (list name)
                                 :depends-on depends-on
                                 :serves serves
                                 :missing (unavailable-systems depends-on shipped-systems)
                                 :description (or (json-value item :string "description") "")))
     #'string< :key #'offer-name)))

(defun hub-cells (&key (url *hub-index-url*) (community *community-index-url*)
                        (shipped (shipped-cells)) max-age)
  "The hub's entries this platform runs, as OFFER records by name: Nodecode's
own (the index at URL), then the community's (at COMMUNITY) less any name
Nodecode's lists; and as a second value why an index was read from its kept
copy, or NIL. NIL when both are NIL; a signal naming the reason when no
index asked for can be read. With MAX-AGE, an index whose kept copy a read
wrote under MAX-AGE seconds ago is that copy, unread."
  ;; Each index answers for itself: one that cannot be read leaves the other's
  ;; entries on offer rather than hiding them, and only when every index asked
  ;; for fails is there nothing to offer and a reason to say so.
  (let ((asked 0) (failed '()) (stale '()))
    (flet ((entries (source index)
             ;; The index at SOURCE, read as READ-HUB-INDEX reads it. When it cannot be, the
             ;; copy last read from SOURCE stands in, and why goes to STALE; with no copy,
             ;; the reason goes to FAILED.
             (when source
               (incf asked)
               (let ((kept (home (format nil "hub/~a.json" (short-digest source 16)))))
                 (flet ((offers (text) (read-hub-index text :shipped shipped :index index)))
                   (or
                    (and max-age (probe-file kept)
                         (< (- (get-universal-time) (file-write-date kept)) max-age)
                         (ignore-errors (offers (alexandria:read-file-into-byte-vector kept))))
                    (handler-case
                        (bind (((body status _ truncated)
                                (http :get source :max-bytes (* 1024 1024) :timeout 10)))
                          (unless (eql status 200)
                            (error "the hub index at ~a answered ~a" source status))
                          (when truncated
                            (error "the hub index at ~a is over a megabyte" source))
                          (prog1 (offers body)
                            (write-file-atomically kept body)))
                      (error (condition)
                        (if-let (copy (and (probe-file kept)
                                           (ignore-errors
                                            (offers (alexandria:read-file-into-byte-vector kept)))))
                          (progn (push (princ-to-string condition) stale) copy)
                          (progn (push condition failed) '()))))))))))
      (let* ((own (entries url "hub"))
             (theirs (remove-if (lambda (entry) (find-offer (offer-name entry) own))
                                (entries community "community"))))
        (when (and (plusp asked) (= (length failed) asked))
          (error (first (last failed))))
        (values (append own theirs)
                (and stale (format nil "~{~a~^; ~}" (reverse stale))))))))

(defun repository-links (directory)
  "The symbolic links under DIRECTORY, .git/ passed over, as `find' prints
them. NIL on Windows, where a clone makes none."
  ;; Walked in this process (TREE-ENTRIES), `find' only for a tree the walk
  ;; cannot read: a child cost ~90 ms of a hub install on macOS.
  #+win32 (progn directory nil)
  #-win32
  (let ((native (string-right-trim "/" (uiop:native-namestring directory))))
    (labels ((walk (path)
               (loop for (child . kind) in (tree-entries path)
                     unless (string= ".git" (subseq child (1+ (position #\/ child :from-end t))))
                       if (eq kind :link) collect child
                       else if (eq kind :directory) append (walk child))))
      (handler-case (walk native)
        (error ()
          (remove "" (lines (run-bounded (list "find" native "-name" ".git" "-prune" "-o" "-type" "l" "-print")
                                         :seconds 120))
                  :test #'string=))))))

(defun repository-head (directory)
  "The commit the repository at DIRECTORY has checked out detached, as its
.git/HEAD names it; NIL when HEAD names a branch or cannot be read."
  (let ((line (ignore-errors
               (string-trim '(#\Newline #\Return #\Space)
                            (uiop:read-file-string (merge-pathnames ".git/HEAD" directory))))))
    (and line (ppcre:scan "^[0-9a-f]{40}\\z" line) line)))

;;; --- the copy of a hub repository a home keeps ---------------------------------
;;; Every folder Nodecode's index lists is a folder of one repository at one
;;; commit. A clone per install fetched all of it again for each: 0.7 s on a
;;; near network, 5-25 s on a far one. A home keeps one bare copy per
;;; repository instead, holding each commit it was asked for one commit deep;
;;; the first pick at a commit fetches it, and every folder after checks
;;; out of it with no network.

(defvar *hub-repository-lock* (bt2:make-lock :name "hub repositories")
  "Held while a copy of a hub repository fetches, so two picks at once
fetch one commit once.")

(defun hub-git (arguments &optional environment)
  "Git run with ARGUMENTS, never prompting and never collecting on its own,
ENVIRONMENT (NAME=value strings) laid over: => its output. Signals its error
output when it fails."
  (multiple-value-bind (out err status)
      (run-bounded (list* *git-program* "-c" "gc.auto=0" arguments) :seconds 600
                   :environment (list* "GIT_TERMINAL_PROMPT=0" environment))
    (unless (eql status 0)
      (error "git ~a failed: ~a" (first arguments) (string-trim '(#\Newline) (or err ""))))
    out))

(defun hub-repository (entry home)
  "The copy HOME keeps of hub ENTRY's repository, holding ENTRY's commit:
made bare and fetched that one commit deep when it lacks it. => its
directory; signals when git cannot."
  (let* ((directory (merge-pathnames (format nil "hub/git/~a/" (short-digest (offer-git entry) 16)) home))
         (native (string-right-trim "/\\" (uiop:native-namestring directory)))
         (commit (offer-commit entry))
         (ref (format nil "refs/pins/~a" commit)))
    (bt2:with-lock-held (*hub-repository-lock*)
      (unless (or (probe-file (merge-pathnames ref directory))
                  (let ((packed (merge-pathnames "packed-refs" directory)))
                    (and (probe-file packed)
                         (search (format nil "~a ~a" commit ref) (uiop:read-file-string packed)))))
        (unless (probe-file (merge-pathnames "HEAD" directory))
          (ensure-directories-exist directory)
          (hub-git (list "init" "--quiet" "--bare" native)))
        (hub-git (list "-C" native "fetch" "--quiet" "--depth" "1" "--no-tags" "--" (offer-git entry)
                       (format nil "+~a:~a" commit ref)))))
    directory))

;;; A PATH entry's index may name an archive of its folder at its commit, on
;;; nodecode.ai's /dl beside the index, which Cloudflare keeps at the edge:
;;; one cell, 8-70 KB, from the nearest edge instead of the repository from
;;; GitHub. The index pins the archive's sha256 as it pins the commit, so the
;;; folder unpacked is the pin's as surely as a checkout's.

(defun hub-archive-checkout (entry staging)
  "Write hub ENTRY's folder, its path at its commit, into STAGING out of the
archive its index names, when the bytes are the sha256 the index carries:
=> T, or NIL, with STAGING gone, when there is none or it cannot be had,
and the caller fetches the repository."
  (when (and entry.archive entry.sha256)
    (or (ignore-errors
         (multiple-value-bind (body status)
             (http :get entry.archive :binary t :max-bytes (* 16 1024 1024) :timeout 20)
           (when (and (eql status 200) (typep body '(vector (unsigned-byte 8)))
                      (string= (sha256-text body) (format nil "sha256:~a" entry.sha256)))
             (ensure-directories-exist staging)
             (with-open-file (out (merge-pathnames ".archive.tar.gz" staging) :direction :output
                                  :element-type '(unsigned-byte 8) :if-exists :supersede)
               (write-sequence body out))
             ;; a name relative to the working directory: Git for Windows' tar
             ;; reads the colon of a drive as a host to fetch the archive from
             (multiple-value-bind (out err status)
                 (run-bounded (list "tar" "-xzf" ".archive.tar.gz") :seconds 60 :directory staging)
               (declare (ignore out))
               (unless (eql status 0)
                 (error "~a: tar failed: ~a" entry.name err)))
             (delete-file (merge-pathnames ".archive.tar.gz" staging))
             t)))
        (progn (when (probe-file staging) (remove-tree staging)) nil))))

(defun hub-repository-checkout (entry staging home)
  "Write hub ENTRY's folder, its path at its commit, into STAGING out of the
copy HOME keeps of its repository (HUB-REPOSITORY): => T, or NIL when that
copy cannot give it, and the caller clones."
  ;; Git writes the pin out of objects named by their own hashes: the folder
  ;; is the commit's as surely as a clone's HEAD. The index is a throwaway in
  ;; STAGING, so the copy, which has no work tree, keeps no state of a checkout.
  (ignore-errors
   (let ((repository (hub-repository entry home)))
     (ensure-directories-exist staging)
     (hub-git (list (format nil "--git-dir=~a" (string-right-trim "/\\" (uiop:native-namestring repository)))
                    (format nil "--work-tree=~a" (string-right-trim "/\\" (uiop:native-namestring staging)))
                    "checkout" "--quiet" (offer-commit entry) "--" (offer-path entry))
              (list (format nil "GIT_INDEX_FILE=~a"
                            (uiop:native-namestring (merge-pathnames ".git-index" staging)))))
     t)))

(defun clone-install (entry target)
  "Clone the hub ENTRY at its commit and move it to TARGET, marked the
installer's. => TARGET."
  ;; The clone is proven before it moves: HEAD is the pinned commit (a folder
  ;; checked out of the kept copy is the pin's by its objects' hashes, one
  ;; unpacked from its index's archive by the archive's sha256), the
  ;; primary system is the entry's name and, for an entry an index listed,
  ;; its description, depends_on and serves are the index's, no symbolic link reaches
  ;; out of it, and every system it declares it needs is here. It is made in
  ;; .cell-staging/ beside the root and renamed in, one filesystem and one
  ;; rename, because a boot loads every folder under the root that carries an
  ;; .asd, dot-named ones too: a clone cut short there would load. An entry
  ;; that is the whole repository keeps its .git, for an update to diff
  ;; against; one that is a folder of it (its PATH) moves in alone, and the
  ;; rest of the clone goes. A clone runs no hook and fetches no submodule,
  ;; so nothing in it runs before the boot loader loads it.
  (let* ((home (uiop:pathname-parent-directory-pathname (uiop:pathname-parent-directory-pathname target)))
         (staging (merge-pathnames (format nil ".cell-staging/~a/" entry.name) home))
         (native (string-right-trim "/\\" (uiop:native-namestring staging)))
         (source (if entry.path (merge-pathnames (format nil "~a/" entry.path) staging) staging)))
    (flet ((git (verb &rest args)
             ;; every verb but the clone runs in the clone
             (multiple-value-bind (out err status)
                 (run-bounded (list* *git-program*
                                     (if (string= verb "clone") (cons verb args) (list* "-C" native verb args)))
                              :seconds 600 :environment '("GIT_TERMINAL_PROMPT=0"))
               (unless (eql status 0)
                 (error "~a: git ~a failed: ~a" entry.name verb (string-trim '(#\Newline) (or err ""))))
               (string-trim '(#\Newline #\Space) (or out "")))))
      (when (probe-file staging) (remove-tree staging))
      (ensure-directories-exist (merge-pathnames "../" staging))
      (with-cleanup ((ignore-errors (when (probe-file staging) (remove-tree staging)))
                     (ignore-errors (uiop:delete-empty-directory
                                     (uiop:pathname-parent-directory-pathname staging))))
        ;; A folder of a repository unpacks out of the archive its index names
        ;; (HUB-ARCHIVE-CHECKOUT), else checks out of the copy this home keeps
        ;; of it (HUB-REPOSITORY-CHECKOUT); a whole repository, which keeps its
        ;; .git, and a folder neither can give, clone.
        (unless (and entry.path (or (hub-archive-checkout entry staging)
                                    (hub-repository-checkout entry staging home)))
          (when (probe-file staging) (remove-tree staging))
          (git "clone" "--quiet" "--no-checkout" "--" entry.git native)
          (git "checkout" "--quiet" "--detach" entry.commit)
          (unless (equal entry.commit (repository-head staging))
            (error "~a: HEAD is not ~a" entry.name entry.commit)))
        ;; a folder the path reaches through a link would move from elsewhere
        (unless (and (probe-file source) (uiop:subpathp (truename source) (truename staging)))
          (error "~a: the repository has no folder ~a" entry.name entry.path))
        (let* ((folder (make-cell-folder source))
               (primary (first folder.systems))
               (declared (folder-depends-on folder))
               (missing (unavailable-systems declared (loop for each in (shipped-cells)
                                                            append (offer-systems each)))))
          (unless (and primary (string= entry.name primary.name))
            (error "~a: the repository's primary system is ~a" entry.name
                   (if primary primary.name "missing: no .asd at its top")))
          ;; the index's copy of the .asd is what the chooser showed and judged;
          ;; an entry no index listed -- a generation's row, put back at the pin
          ;; its seal recorded (ROW-OFFER) -- carries no copy to hold it to (T-347)
          (when entry.index
            (unless (string= entry.description primary.description)
              (error "~a: the index's description is not the .asd's, ~s" entry.name primary.description))
            (unless (null (set-exclusive-or declared entry.depends-on :test #'string-equal))
              (error "~a: the index's depends_on ~s is not the .asd's ~s"
                     entry.name entry.depends-on declared))
            ;; what a picker offered the folder by is what the folder says it serves
            (unless (null (set-exclusive-or primary.serves entry.serves :test #'equal))
              (error "~a: the index's serves ~s is not the .asd's ~s"
                     entry.name entry.serves primary.serves)))
          (when-let (links (repository-links source))
            (error "~a carries symbolic link~p (~{~a~^, ~}); a cell may not"
                   entry.name (length links) links))
          (when missing
            (error "~a needs ~{~a~^, ~}, which this image cannot find" entry.name missing)))
        (with-output-file (out (merge-pathnames +hub-marker+ source))
          (write-line entry.git out)
          (write-line entry.commit out)
          (when entry.path (write-line entry.path out)))
        (sb-posix:rename (string-right-trim "/\\" (uiop:native-namestring source))
                         (string-right-trim "/\\" (uiop:native-namestring target)))))
    target))

(defun hub-pin (name &optional (root *cells-directory*))
  "What the hub install NAME under ROOT was cloned from, as its marker says:
(GIT COMMIT PATH), PATH NIL for a whole repository; NIL for any other folder."
  (when-let (marker (and root (probe-file (merge-pathnames (format nil "~a/~a" name +hub-marker+)
                                                          root))))
    (subseq (append (marker-lines marker) (list nil nil nil)) 0 3)))

(defun install-cell (name &key (root *cells-directory*) (offers (shipped-cells)))
  "Put the offered folder NAME under ROOT: a shipped one as a symlink to where
it ships from (a copy on a platform without links), a hub entry cloned at
its commit (CLONE-INSTALL)."
  ;; Already present is left as it is —
  ;; one another image made is pointed back at this image's at boot
  ;; (REFRESH-INSTALLED-CELLS). => the folder's directory. Refuses a name not
  ;; on offer, a folder whose dependencies this image cannot find
  ;; (OFFER-MISSING), and a ROOT of NIL (the hermetic posture).
  (let ((entry (find-offer name offers)))
    (unless root (error "no cell folder: *cells-directory* is NIL"))
    (unless entry (error "no cell named ~a on offer" name))
    (when entry.missing
      (error "~a needs ~{~a~^, ~}, which this image cannot find" name entry.missing))
    (let ((target (merge-pathnames (format nil "~a/" name) root)))
      (ensure-directories-exist root)
      (unless (probe-file target)
        ;; A link that no longer resolves is replaced, not tripped over —
        ;; MAKE-SYMLINK's own rule.
        (cond (entry.git (clone-install entry target))
              #-win32 (t (make-symlink entry.directory (merge-pathnames name root)))
              #+win32 (t (copy-install entry.directory target))))
      (refresh-shipped-cell-notice :root root :shipped (remove-if #'offer-git offers))
      (truename target))))

(defun shipped-counterpart (path &optional (root (shipped-cells-directory)))
  "This image's own folder in place of PATH, a shipped cell folder of some
other tree — a release's, a checkout's: ROOT/<rel>/, <rel> being whatever
follows PATH's last `cells' directory, when that folder is here and
carries an .asd."
  ;; NIL when PATH is not shaped like one or ROOT ships no such folder.
  (let ((rel (shipped-relative path)))
    (when (and root rel)
      (let ((here (merge-pathnames (make-pathname :directory (cons :relative rel)) root)))
        (and (asd-files here)
             (truename here))))))

(defun shipped-relative (path &aux (directory (uiop:ensure-directory-pathname path)))
  "PATH, a folder some tree ships, by its place there: (values REL CELLS),
REL the components after its last `cells' directory and CELLS that
directory; NIL when PATH is not shaped like one."
  (let* ((components (pathname-directory directory))
         (at (position "cells" components :test #'equal :from-end t))
         (rel (and at (nthcdr (1+ at) components))))
    (when (and rel (every #'stringp rel))
      (values rel (make-pathname :directory (subseq components 0 (1+ at)) :defaults directory)))))

(defun release-folder-p (path)
  "Whether PATH is a folder of a release's tree as the installer lays one
down: the organism's own system definition beside its cells/, and no
repository around the tree, which a checkout has."
  (when-let (cells (nth-value 1 (shipped-relative path)))
    (and (probe-file (merge-pathnames "../nodecode.asd" cells))
         (not (probe-file (merge-pathnames "../../.git" cells)))
         t)))

(defun refresh-installed-cells (&key (root *cells-directory*)
                                      (shipped-root (shipped-cells-directory)))
  "Point every install under ROOT that another image made back at this
image's own folder, and take out one of a folder this image no longer ships."
  ;; => the names refreshed in folder order, ((NAME . TEXT) ...) for the ones
  ;; that failed, and the names taken out.
  ;;
  ;; A release installs beside the one before it and never touches the home,
  ;; so a link the installer made would keep leading into the old release's
  ;; tree — or a checkout's, when another build runs — and would load that
  ;; tree's cell source against this core; a copy another build made would
  ;; keep its files, so rerunning the installer would update the executable
  ;; and none of its cells. An install is the installer's by UNINSTALL-CELL's
  ;; rule, a link or a copy carrying the marker; its folder here is
  ;; SHIPPED-COUNTERPART of the one it came from, so the entry keeps its name.
  ;; A link is pointed at it in place — a link to nothing too, so a release
  ;; deleted by hand costs nothing — and a copy is made again, everything but
  ;; the marker going first, so a copy cut short still carries the other
  ;; build's stamp and the next boot finishes it.
  ;;
  ;; An install of a folder this image does not ship, made from another
  ;; release's tree (RELEASE-FOLDER-P), is taken out: without this, a release
  ;; that stopped shipping it would leave the link leading into the tree it
  ;; came from, and every boot would load that tree's source against this
  ;; core.
  ;;
  ;; Left as they stand: a link that resolves under SHIPPED-ROOT, a copy whose
  ;; marker carries this image's stamp, the operator's own folder (no link, no
  ;; marker), and an install of a folder this image does not ship out of a
  ;; checkout -- a newer build's, booting the same home -- or out of a tree
  ;; that is gone, which cannot say whose it was. Each entry is its own error
  ;; boundary: two boots racing for one entry leave it pointed at the same
  ;; folder, and a loss never refuses a boot.
  (let ((refreshed '())
        (failed '())
        (retired '()))
    (when (and root shipped-root
               (uiop:directory-exists-p root)
               (probe-file shipped-root))
      (let ((here (namestring (truename shipped-root))))
        (dolist (name (entry-names root))
          (with-handlers ((error (condition)
                            (push (cons name (princ-to-string condition)) failed)))
            (let* ((link (merge-pathnames name root))
                   (directory (merge-pathnames (format nil "~a/" name) root))
                   (marker (merge-pathnames +installed-marker+ directory))
                   (target (symlink-target link)))
              (cond
                (target
                 (let* ((path (uiop:merge-pathnames*
                               (uiop:parse-native-namestring target :ensure-directory t)
                               root))
                        (resolved (probe-file path)))
                   (unless (and resolved
                                (uiop:string-prefix-p here (namestring resolved)))
                     (if-let (own (shipped-counterpart path shipped-root))
                       (progn (make-symlink own link)
                              (push name refreshed))
                       (when (and resolved (release-folder-p resolved))
                         #-win32 (sb-posix:unlink (sb-ext:native-namestring link))
                         (push name retired))))))
                ((probe-file marker)
                 (bind (((from stamp) (values-list (marker-lines marker))))
                   (let ((source (and (not (equal stamp *image-stamp*))
                                      (plusp (length from))
                                      (uiop:parse-native-namestring from :ensure-directory t))))
                     (if-let (own (and source (shipped-counterpart source shipped-root)))
                       (progn
                         (dolist (sub (uiop:subdirectories directory))
                           (uiop:delete-directory-tree sub :validate t))
                         (dolist (file (uiop:directory-files directory))
                           (unless (string= (file-namestring file) +installed-marker+)
                             (delete-file file)))
                         (copy-install own directory)
                         (push name refreshed))
                       (when (and source (probe-file source) (release-folder-p source))
                         (uiop:delete-directory-tree directory :validate t)
                         (push name retired))))))))))))
    (values (nreverse refreshed) (nreverse failed) (nreverse retired))))

(defun installer-put-p (name &optional (root *cells-directory*))
  "Whether the folder NAME under ROOT is one this installer put there — a
link, a copy carrying its marker, a hub clone carrying its own — and so one
UNINSTALL-CELL takes out."
  ;; a marker is only found inside a folder that is there
  (and root
       (let ((target (merge-pathnames (format nil "~a/" name) root)))
         (or #-win32 (symlink-target (merge-pathnames name root))
             (probe-file (merge-pathnames +installed-marker+ target))
             (probe-file (merge-pathnames +hub-marker+ target))))))

(defun uninstall-cell (name &key (root *cells-directory*))
  "Remove the folder NAME from under ROOT when it is one this installer put
there (INSTALLER-PUT-P)."
  ;; The operator's own folders are never touched: => T when removed, NIL when
  ;; nothing was. The shipped-cell notice is refreshed after a removal.
  (when (installer-put-p name root)
    (let ((link (merge-pathnames name root)))
      (cond #-win32
            ((symlink-target link) (sb-posix:unlink (sb-ext:native-namestring link)))
            (t (uiop:delete-directory-tree (merge-pathnames (format nil "~a/" name) root) :validate t))))
    (refresh-shipped-cell-notice :root root)
    t))

(defun load-cell (&rest names)
  "Load the folders NAMES under *CELLS-DIRECTORY* into this image now — the
per-folder load LOAD-CELLS runs at boot, dependencies first — and put
their records on (CELLS)."
  ;; A name already on the report keeps its record and is not loaded twice; a
  ;; name with no folder is skipped. => the records loaded, in load order.
  ;; The look at the report, the load and the append are one turn of the lock,
  ;; so a folder the boot thread is loading is on the report when this gets
  ;; its turn: the first shell frame and the boot both ask for the import
  ;; folder, and two loads of one system at once tore SBCL's class tables.
  ;; What starts is the gateway's business: RESTART-CELLS runs START-CELL
  ;; for a peripheral this put on the report.
  (bt2:with-recursive-lock-held (*cell-load-lock*)
    (let* ((root *cells-directory*)
           ;; The folder is named by its entry under ROOT — the link's name,
           ;; exactly as CELL-FOLDERS names it at boot — never by where the
           ;; link resolves, so the record the wizard makes is the record the
           ;; next boot makes, and START-CELLS finds it by that name.
           (folders (loop for name in names
                          for directory = (and root (folder-under name root))
                          when (and directory
                                    (not (find-cell name))
                                    (probe-file directory)
                                    (asd-files directory))
                            collect (make-cell-folder directory)))
           (owners (make-hash-table :test 'equal)))
      (when folders
        ;; Every earlier folder's definitions are its own: a collision with
        ;; one of them is attributed to it, exactly as at boot.
        (dolist (cell *cells*)
          (dolist (key (append cell.defined cell.overrides)) (setf (gethash key owners) cell.name)))
        ;; The load's nursery is the load's, as a compile's is
        ;; (COMPILE-LAYER-SHARES): the import folder's switch-on consed 25 MB,
        ;; and under the organism's 2 MB that was 86 ms of its 189 in
        ;; collections.
        (with-nursery ((* 64 1024 1024))
          (let ((records (folder-records folders owners (definition-snapshot))))
            (setf *cells* (append *cells* records))
            records))))))

;;; --- what a peripheral is made of, declared once ------------------------------
;;; Eight folders had each written the same six forms: the condition every
;;; refusal signals, the function that signals it, the settings variable, its
;;; reader, the lock and the macro over it. One declaration here; the names
;;; land in the folder's own package, the way DEFINE-ERROR interns its readers.

;;; NAME-ERROR is the one condition every refusal signals, DETAIL its whole
;;; report; FAIL signals it with a formatted detail, passed through FILTER (a
;;; function name) when one is given. NOT-RUNNING is the refusal
;;; RUNNING-SETTINGS answers while the settings variable *NAME* is NIL, said
;;; from NAME -- where to look, and the way back; LOCK names the lock
;;; WITH-NAME-LOCK holds, recursive when RECURSIVE.
(defmacro define-peripheral (name &key not-running lock recursive filter)
  "Declare in NAME's package NAME-ERROR and FAIL; with NOT-RUNNING, *NAME*,
RUNNING-SETTINGS and DEFINE-VERB; with LOCK, *LOCK* and WITH-NAME-LOCK."
  (let* ((package (symbol-package name))
         (text (symbol-name name))
         (condition (intern (format nil "~a-ERROR" text) package))
         (detail (intern (format nil "~a-ERROR-DETAIL" text) package))
         ;; The slot is named in NAME's package, so a DEFINE-ERROR child's :report there reaches it.
         (slot (intern "DETAIL" package))
         (fail (intern "FAIL" package))
         (settings (intern (format nil "*~a*" text) package))
         (running (intern "RUNNING-SETTINGS" package))
         (verb (intern "DEFINE-VERB" package))
         (lock-var (intern "*LOCK*" package))
         (with-lock (intern (format nil "WITH-~a-LOCK" text) package)))
    `(progn
       (define-error ,condition (error) ((,slot :reader ,detail))
         (:report "~a" ,slot)
         (:documentation "Every refusal the model sees; DETAIL is the whole report."))
       (defun ,fail (format-control &rest args)
         ,(format nil "Signal ~a with a formatted detail." condition)
         (error ',condition
                :detail (,@(if filter (list filter) '(identity))
                         (apply #'format nil format-control args))))
       ,@(when not-running
           ;; SETTING comes from the NLE:DEFINE-CELL declaration, which loads
           ;; last; the cell's files between call it.
           `((declaim (ftype (function (keyword) t) ,(intern "SETTING" package)))
             (defvar ,settings nil
               "The running cell's SETTINGS, or NIL while it is not started.")
             (defun ,running ()
               "The settings, or a refusal the model can read."
               (or ,settings
                   (,fail ,(format nil "the ~(~a~) cell is not running: check (cells) and the ~
                                        ~(~:*~a~) section of ~~~~/.nodecode/config.jsonc, ~
                                        then (restart-cells \"~(~a~)\")"
                                   text (package-name package)))))
             (defmacro ,verb (name lambda-list documentation &body body)
               "A DEFUN whose body opens with the refusal RUNNING-SETTINGS says while the
cell is not running."
               (list* 'defun name lambda-list documentation '(,running) body))))
       ,@(when lock
           `((defvar ,lock-var (,(if recursive 'bt2:make-recursive-lock 'bt2:make-lock)
                                :name ,lock))
             (defmacro ,with-lock (&body body)
               (list* ',(if recursive 'bt2:with-recursive-lock-held 'bt2:with-lock-held)
                      (list ',lock-var) body)))))))

;;; --- a worker: one thread, poked, stopped ---------------------------------------
;;; The thread shape every peripheral that runs something had written for
;;; itself: a semaphore waited on with a timeout, a stop flag read after
;;; every wait, a bounded join on the way out.

(define-record (worker (:copier nil) (:predicate nil) (:constructor %make-worker) (:export thread))
  "One thread running a step whenever it is poked or its wake passes, until
stopped."
  ;; THREAD is the bt2 thread; SEMAPHORE what a poke signals; STOPPING the
  ;; flag the thread reads after every wait.
  (thread nil)
  (semaphore (bt2:make-semaphore) :read-only t)
  (stopping nil))

(defun worker-start (name step &key (wake 60) prime &aux (worker (%make-worker)))
  "Start a thread NAME running STEP (no arguments) whenever poked or WAKE
seconds (a number, or a thunk answering one) pass, warning STEP's errors
under NAME, until stopped or STEP answers :STOP; PRIME runs the first step
at once. => the WORKER."
  (setf worker.thread
        (spawn name
          (loop
            (bt2:wait-on-semaphore worker.semaphore
                                   :timeout (if (functionp wake) (funcall wake) wake))
            (when (or worker.stopping
                      (eq :stop (handler-case (funcall step)
                                  (error (condition) (warn "~a: ~a" name condition)))))
              (return)))))
  (when prime
    (bt2:signal-semaphore worker.semaphore))
  worker)

(defun worker-poke (worker)
  "Wake WORKER now; nothing when there is none."
  (when worker
    (bt2:signal-semaphore worker.semaphore))
  worker)

(defun worker-stop (worker &key (timeout 5))
  "Stop WORKER and wait, at most TIMEOUT seconds, for its thread to leave."
  ;; => NIL either way, so (setf *worker* (worker-stop *worker*)) reads right.
  (when worker
    (setf worker.stopping t)
    (bt2:signal-semaphore worker.semaphore)
    (%join-bounded worker.thread timeout))
  nil)

;;; --- a session's place ----------------------------------------------------------

(defun find-session-cwd (session-id &aux (session (and (stringp session-id) (store-open-p)
                                                       (ignore-errors (find-session session-id)))))
  "The durable directory of SESSION-ID, or NIL without a store or a session."
  (and session session.cwd))

(defun session-project-root (session-id)
  "The repository SESSION-ID's directory sits in -- PROJECT-ROOT of its
durable cwd -- or NIL."
  (project-root (find-session-cwd session-id)))
