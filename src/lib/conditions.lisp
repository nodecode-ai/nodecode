;;;; conditions.lisp --- DEFINE-ERROR and DEFINE-RECORD, the declaration waist,
;;;; and the binding and thread macros every file below compiles against.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every refusal here is an ERROR subtype whose slots exist to be read back by
;;;; one FORMAT call. Hand-written, that spells the same name four times per
;;;; slot — slot, initarg, reader, and the reader again inside the report — so a
;;;; report naming the wrong ancestor's reader still compiled. DEFINE-ERROR
;;;; states each name once:
;;;;
;;;;   (define-error invalid-session-anchor (session-identity-error)
;;;;       (session-id anchor)
;;;;     (:report "anchor turn ~a is not a turn of session ~a's parent"
;;;;              anchor session-id))
;;;;
;;;; Report arguments are slot names, not reader calls; an inherited slot
;;;; resolves to the reader its ancestor declared, because the expander carries
;;;; a compile-time registry of which condition owns which slot. This file loads
;;;; first: refusals are the first contract each layer below declares.

(in-package #:nodecode.kernel)

;;; --- binding macros ------------------------------------------------------
;;; Bind a name, then act only when the value arrived. Written out, that is a
;;; LET whose whole body is a WHEN on the name just bound — three lines and
;;; two nesting levels to say one thing, at three hundred call sites. They sit
;;; in this file because it is the one that loads before every other: a macro
;;; is only available to the files compiled after it.

(defmacro when-let ((variable form) &body body)
  "Bind VARIABLE to FORM and evaluate BODY when the value is true, else NIL."
  `(let ((,variable ,form))
     (when ,variable ,@body)))

(defmacro if-let ((variable form) then &optional else)
  "Bind VARIABLE to FORM and take THEN when the value is true, else ELSE."
  `(let ((,variable ,form))
     (if ,variable ,then ,else)))

(defmacro lambda-ignoring (params &body body)
  "A lambda over PARAMS, none of which BODY has to read."
  `(lambda ,params
     (declare (ignorable ,@(remove-if #'consp (set-difference params lambda-list-keywords))))
     ,@body))

(defmacro bind (specs &body body)
  "BODY with each of SPECS bound in turn: (NAME FORM) for the one value FORM
answers, ((NAME ...) FORM) for the several it answers. A target named _ is a
value the body does not want."
  ;; One binder where Common Lisp has three. A chain of LET, LET* and
  ;; MULTIPLE-VALUE-BIND spends a line and a level of nesting on every link,
  ;; though every link says the same one thing: this name now stands for that
  ;; value. Written as one list of specs the chain reads as what it is, and
  ;; the body sits at one indentation however many names it took to get
  ;; there. Sequential, like LET*: every spec sees the specs above it, which
  ;; is what a chain already was.
  ;;
  ;; _ is the name for a value that has to be received and does not have to
  ;; be read -- the second value of a lookup whose first is the answer -- and
  ;; it is bound to a fresh symbol and declared ignored, so the body says
  ;; nothing about it and the compiler does not ask.
  (if (null specs)
      `(progn ,@body)
      (destructuring-bind ((target form) . rest) specs
        (let ((inner (if rest `((bind ,rest ,@body)) body)))
          (if (listp target)
              (let* ((names (loop for name in target
                                  collect (if (string= name "_") (gensym "IGNORED") name)))
                     (ignored (set-difference names target)))
                `(multiple-value-bind ,names ,form
                   ,@(when ignored `((declare (ignore ,@ignored))))
                   ,@inner))
              `(let ((,target ,form)) ,@inner))))))

(defmacro with-handlers ((&body clauses) &body body)
  "BODY, with CLAUSES — (CONDITION-TYPE (VARIABLE) FORM...) as HANDLER-CASE
writes them — answering for it instead when one of them is signalled."
  ;; HANDLER-CASE's sibling problem to UNWIND-PROTECT's: the protected form
  ;; is one form, so the body of more than one is a PROGN, and the clauses
  ;; that say what the body is guarded against are written past it, where a
  ;; reader meets them only after the code they cover. Here the guard is the
  ;; head and the body is a body, the same shape as WITH-CLEANUP.
  `(handler-case (progn ,@body) ,@clauses))

(defmacro with-cleanup ((&body cleanup) &body body)
  "BODY, and then CLEANUP however BODY leaves it — by answering, by a
transfer out, or on the way out of a signal."
  ;; UNWIND-PROTECT takes one protected form, so a body of more than one form
  ;; is written (UNWIND-PROTECT (PROGN ...) ...) and spends two lines saying
  ;; where the body starts before any of it is written. Here the cleanup is
  ;; the head, as the stream is in WITH-OPEN-FILE, and the body is a body.
  `(unwind-protect (progn ,@body) ,@cleanup))

(defmacro dispatch (subject test &body arms &aux (value (gensym "SUBJECT")))
  "Run the first ARM whose literal SUBJECT passes TEST; SUBJECT is evaluated
once. An arm is (LITERAL form*); a final (t form*) or (otherwise form*) takes
what TEST missed. No arm matching answers NIL, as COND does."
  ;; Written out, that is a COND whose every clause says the subject again --
  ;; ((STRING= VERB "list") ...), ((STRING= VERB "use") ...) -- a line per arm
  ;; spent restating it, and a subject mistyped in arm seven still compiles.
  ;; The predicate stays the one the clauses used: STRING=, EQUAL, EQL, =.
  `(let ((,value ,subject))
     (cond ,@(loop for arm in arms
                   collect (destructuring-bind (pattern . body) arm
                             (if (member pattern '(t otherwise))
                                 `(t ,@body)
                                 `((,test ,value ,pattern) ,@body)))))))

(defmacro spawn (name &body body)
  "Run BODY on a new thread named NAME and answer the thread."
  ;; The one thread shape the organism uses: a body and a name. Written out
  ;; it is a LAMBDA wrapped in MAKE-THREAD with :NAME trailing, at eighty
  ;; call sites.
  ;;
  ;; The thread prints as a REPL does. bordeaux-threads starts one under
  ;; standard syntax, where *PRINT-READABLY* is T, and the JSON decoder keeps
  ;; ASCII text as base-strings (NARROW-STRING): ~S of a decoded name would
  ;; print #A((18) BASE-CHAR . "three-minute check").
  `(bt2:make-thread (lambda () (let ((*print-readably* nil)) ,@body)) :name ,name))

(defun parallel-map (function list &key (lanes 8) &aux (items (coerce list 'simple-vector))
                                                       (results (make-array (length items)))
                                                       (next 0)
                                                       (failure nil)
                                                       (lock (bt2:make-lock :name "parallel-map")))
  "FUNCTION over each element of LIST, on up to LANES threads at once of
which the caller is one: => the results, in LIST's order. The first error a
call signals stops the lanes taking more, and is signalled here once they
are done."
  ;; FUNCTION runs where the caller's dynamic bindings do not reach -- a
  ;; lane sees a special's global value -- so what it reads of them, it
  ;; closes over.
  (flet ((lane ()
           (loop for index = (bt2:with-lock-held (lock)
                               (and (not failure) (< next (length items)) (shiftf next (1+ next))))
                 while index
                 do (handler-case (setf (svref results index) (funcall function (svref items index)))
                      (error (condition)
                        (bt2:with-lock-held (lock) (setf failure (or failure condition))))))))
    (mapc #'bt2:join-thread
          (prog1 (loop repeat (1- (min lanes (length items)))
                       collect (spawn "parallel-map" (lane)))
            (lane))))
  (when failure (error failure))
  (coerce results 'list))

;;; --- state hung beside an object -----------------------------------------
;;; A running image cannot grow a structure a slot, so state an object owns
;;; but its type does not declare lives in a table beside it. Written out that
;;; is a DEFVAR, a reader, a writer and a forgetter per table, with the "no
;;; entry means this default" rule restated at every reader, at forty-three
;;; tables.

(defmacro define-side-table (name (&key (test '#'eq) weakness synchronized (key 'identity))
                            &body documentation)
  "Define NAME as a table hung beside objects: (NAME OBJECT) reads, (SETF (NAME
OBJECT) VALUE) writes, (FORGET-NAME OBJECT) drops the entry."
  ;; KEY names the function turning the object into the table's key, so a table
  ;; keyed by an id rather than by identity says so once instead of at every
  ;; accessor. A missing entry reads as NIL. WEAKNESS reaches MAKE-HASH-TABLE, so a table can
  ;; hold its keys weakly and let the collector reclaim an object the rest of
  ;; the image has finished with. The variable keeps its own name, *NAME*, so
  ;; code that must map or count the whole table still can.
  (let ((variable (intern (format nil "*~a*" (symbol-name name))))
        (forget (intern (format nil "FORGET-~a" (symbol-name name))))
        (object (gensym "OBJECT"))
        (value (gensym "VALUE")))
    `(progn
       (defvar ,variable
         (make-hash-table :test ,test
                          ,@(when weakness (list :weakness weakness))
                          ,@(when synchronized '(:synchronized t)))
         ,@documentation)
       (defun ,name (,object)
         (values (gethash (,key ,object) ,variable)))
       (defun (setf ,name) (,value ,object)
         (setf (gethash (,key ,object) ,variable) ,value))
       (defun ,forget (,object)
         (remhash (,key ,object) ,variable)
         nil)
       ',name)))

;;; --- the slot registry ---------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Populated by DEFINE-ERROR as each form expands, which is why a :report
  ;; clause can name a slot and get the reader that actually reads it
  ;; whichever ancestor declared it. Conditions defined with bare
  ;; DEFINE-CONDITION are absent, so a report naming their slots must call the
  ;; reader itself.
  (defvar *error-slot-readers* (make-hash-table :test #'eq)
    "Condition name -> alist of (SLOT-NAME . READER), ancestors included.")

  (defun parse-error-slot (condition-name spec)
    "SPEC -> (values DEFCLASS-SLOT-FORM SLOT-NAME READER)."
    ;; SPEC is a bare slot name or (NAME . OPTIONS). Options: :INITFORM as
    ;; usual, :INITARG to override the derived keyword (NIL omits it
    ;; entirely), :READER to override the derived name, :WRITABLE T to emit an
    ;; accessor instead of a reader.
    (destructuring-bind (slot-name &rest options) (alexandria:ensure-list spec)
      (let ((initarg (if (member :initarg options)
                         (getf options :initarg)
                         (intern (symbol-name slot-name) :keyword)))
            (reader (or (getf options :reader)
                        (alexandria:format-symbol (symbol-package condition-name) "~a-~a"
                                                  condition-name slot-name))))
        (values (append (list slot-name)
                        (when initarg (list :initarg initarg))
                        (when (member :initform options)
                          (list :initform (getf options :initform)))
                        (list (if (getf options :writable) :accessor :reader) reader))
                slot-name
                reader))))

  (defun error-report-argument (condition-name argument condition-var)
    "A :report argument."
    ;; A registered slot name becomes its reader call; anything else is left
    ;; to evaluate where it stands, which is how a report reaches a constant
    ;; such as +MAX-PAGE-SIZE+.
    (if-let (reader (and (symbolp argument)
                         (cdr (assoc argument (gethash condition-name *error-slot-readers*)
                                     :test #'eq))))
      (list reader condition-var) argument)))

;;; --- the declaration -----------------------------------------------------

(defmacro define-error (name parents slots &body clauses)
  "Declare one refusal: NAME, its PARENTS, its SLOTS, and how it reports."
  ;; SLOTS is a list of slot specs (see PARSE-ERROR-SLOT). Each derives its
  ;; own :INITARG and :READER from the slot name, so neither is restated.
  ;;
  ;; CLAUSES are DEFINE-CONDITION options, with two rewritten forms:
  ;;
  ;;   (:report FORMAT-STRING ARGUMENT...)
  ;;
  ;; which expands to the report lambda. Each ARGUMENT that names a slot of
  ;; NAME — including a slot an ancestor declared — becomes that slot's reader
  ;; call; every other argument is evaluated verbatim. A report with no
  ;; arguments needs no IGNORE declaration.
  ;;
  ;;   (:export)
  ;;
  ;; which exports NAME and the readers this form declares, so a refusal that
  ;; is part of a package's boundary says so where it is declared rather than
  ;; in a hand-kept mirror. Opt-in, because the refusals the evolved package
  ;; declares about itself are internal.
  ;;
  ;;   (:signal FUNCTION)
  ;;
  ;; which defines FUNCTION of (CONTROL &rest ARGS), signalling NAME with the
  ;; formatted text as its DETAIL: a refusal's one-line signaller, said where
  ;; the refusal is.
  ;;
  ;; Every other clause (:DOCUMENTATION, :DEFAULT-INITARGS, or a hand-written
  ;; :REPORT lambda) passes through untouched.
  (let* ((condition-var (gensym "CONDITION"))
         (stream-var (gensym "STREAM"))
         (export (assoc :export clauses))
         (signaller (assoc :signal clauses))
         (parsed (mapcar (lambda (spec) (multiple-value-list (parse-error-slot name spec)))
                         slots))
         (slot-forms (mapcar #'first parsed))
         (registry (mapcar (lambda (slot) (cons (second slot) (third slot))) parsed))
         (readers (mapcar #'cdr registry)))
    (setf clauses (remove signaller (remove export clauses)))
    ;; Inherited slots resolve too: a child's report may name a slot its
    ;; parent declared, and must reach the parent's reader.
    (dolist (parent parents)
      (dolist (entry (gethash parent *error-slot-readers*))
        (unless (assoc (car entry) registry :test #'eq)
          (setf registry (append registry (list entry))))))
    ;; The registry is compile-time material, so the expansion carries its own
    ;; population: a later file compiled against this one's fasl must still be
    ;; able to resolve a slot it inherits. Registering here rather than in the
    ;; macro body is what makes the two orders equivalent.
    (setf (gethash name *error-slot-readers*) registry)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *error-slot-readers*) ',registry))
       ,@(when export `((export '(,name ,@readers))))
       (define-condition ,name ,parents ,slot-forms
         ,@(mapcar
            (lambda (clause)
              (if (and (consp clause) (eq (first clause) :report)
                       (stringp (second clause)))
                  `(:report
                    (lambda (,condition-var ,stream-var)
                      (declare (ignorable ,condition-var))
                      (format ,stream-var ,(second clause)
                              ,@(mapcar (lambda (argument)
                                          (error-report-argument
                                           name argument condition-var))
                                        (cddr clause)))))
                  clause))
            clauses))
       ,@(when signaller
           `((defun ,(second signaller) (control &rest args)
               ,(format nil "Signal ~a with the formatted text as its DETAIL." name)
               (error ',name :detail (apply #'format nil control args))))))))

(defmacro define-errors (parents (&rest clauses) &body rows)
  "A family of refusals as a table: one DEFINE-ERROR under PARENTS with
CLAUSES per row (NAME SLOTS CONTROL ARGUMENT...), CONTROL and ARGUMENTs its
(:report CONTROL ARGUMENT...)."
  `(progn ,@(loop for (name slots . report) in rows
                  collect `(define-error ,name ,parents ,slots ,@clauses (:report ,@report)))))

(defmacro define-refusal (name signal report documentation &key export)
  "A refusal whose one slot is DETAIL: NAME, reported as REPORT over DETAIL, signalled by SIGNAL."
  `(define-error ,name (error) (detail)
     (:documentation ,documentation) (:report ,report detail)
     ,@(and export '((:export))) (:signal ,signal)))

;;; --- the record declaration ----------------------------------------------

(defmacro define-record (name-and-options &body slots)
  "DEFSTRUCT with one more option, (:export WHAT...), naming which of the
names the structure derives are part of the package's boundary — said at the
declaration, as DEFINE-ERROR does, instead of in a hand-kept mirror."
  ;; A variable named for the record reads it through ACCESS: JOB.ID is
  ;; (JOB-ID JOB), with no ACCESS form restating it.
  ;; WHAT is T for the constructor, the predicate and every reader; else any
  ;; of :CONSTRUCTOR, :PREDICATE, :READERS, and slot names for single readers.
  ;; The record's own name is always exported; :CONC-NAME, :CONSTRUCTOR and
  ;; :PREDICATE are read for the names they set.
  (let* ((name (if (consp name-and-options) (first name-and-options) name-and-options))
         (options (if (consp name-and-options) (rest name-and-options) '()))
         (given (lambda (key) (find key options :key (lambda (option) (and (consp option) (first option))))))
         (export (funcall given :export))
         (derived (lambda (&rest parts)
                    (alexandria:format-symbol (symbol-package name) "~{~a~}" parts)))
         (constructor (if-let (option (funcall given :constructor))
                        (second option) (funcall derived "MAKE-" name)))
         (predicate (if-let (option (funcall given :predicate))
                      (second option) (funcall derived name "-P")))
         (conc-name (let ((option (funcall given :conc-name)))
                      (cond ((null option) (concatenate 'string (symbol-name name) "-"))
                            ((null (second option)) "")
                            (t (second option)))))
         (reader (lambda (slot) (funcall derived conc-name slot)))
         (readers (loop for slot in slots
                        unless (stringp slot)
                          collect (funcall reader (if (consp slot) (first slot) slot))))
         (names (cons name
                      (loop for what in (rest export)
                            append (case what
                                     ((t) (list* constructor predicate readers))
                                     (:constructor (list constructor))
                                     (:predicate (list predicate))
                                     (:readers readers)
                                     (otherwise (list (funcall reader what))))))))
    `(progn
       ,@(when export `((export ',names)))
       (defstruct (,name ,@(remove export options))
         ,@slots)
       (access (,name ,name)))))

;;; --- dotted readers ------------------------------------------------------
;;; A reader call spends the record's name at every site although the variable
;;; already carries it: (CHAT-MODEL-DIALOG MODEL) says `chat-model' twice. The
;;; name a reader adds over the variable is its slot, so ACCESS gives a
;;; variable's slots to the reader as MODEL.DIALOG — a plain symbol under the
;;; standard reader, and a symbol macro standing for exactly the reader call it
;;; replaces, so it is a place, chains, and costs nothing at run time.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun record-readers (record)
    "(SLOT . READER) for every slot RECORD declares a reader for."
    ;; A structure carries its readers in the compile-time environment, so a
    ;; record and the ACCESS that names it may share a file. A class does not:
    ;; DEFCLASS leaves only :FORTHCOMING-DEFCLASS-TYPE behind until load, so an
    ;; ACCESS over one must sit in a file compiled after the file defining it,
    ;; and says so rather than expanding into readers that do not exist.
    ;; A structure is read off its description whether or not its class exists
    ;; yet, so the description is asked first.
    (let ((description (sb-kernel:find-defstruct-description record nil))
          (class (find-class record nil)))
      (cond (description
             (loop for slot in (sb-kernel:dd-slots description)
                   for reader = (sb-kernel:dsd-accessor-name slot)
                   when reader collect (cons (sb-kernel:dsd-name slot) reader)))
            (class
             (unless (sb-mop:class-finalized-p class)
               (sb-mop:finalize-inheritance class))
             (loop for ancestor in (sb-mop:class-precedence-list class)
                   append (loop for slot in (sb-mop:class-direct-slots ancestor)
                                for reader = (first (sb-mop:slot-definition-readers slot))
                                when reader
                                  collect (cons (sb-mop:slot-definition-name slot) reader))))
            (t (error "ACCESS: ~s names no record this file can read. A class is ~
only readable from a file compiled after the one defining it." record))))))

(defmacro access (&body clauses)
  "Give each (VARIABLE RECORD...) clause's VARIABLE dotted readers: the symbol
VARIABLE.SLOT stands for the reader RECORD declares for SLOT, read on
VARIABLE. The first RECORD naming a slot owns it."
  ;; The dotted symbol is interned in the package being compiled, not the
  ;; variable's own — a variable named for a Common Lisp symbol (SIGNAL) would
  ;; otherwise intern its dotted readers in a locked package. Each package
  ;; declares the variables it spells and no name crosses a boundary. A clause
  ;; naming several records gives one variable the union of their slots, which
  ;; is what a variable holding either a record or its subclass wants — and two
  ;; records answering one slot with different readers is a refusal, because
  ;; the dotted name could only stand for one of them.
  `(progn
     ,@(loop for (variable . records) in clauses
             append (let ((seen (make-hash-table :test #'eq)))
                      (loop for record in records
                            append (loop for (slot . reader) in (record-readers record)
                                         for already = (gethash slot seen)
                                         when (and already (not (eq already reader)))
                                           do (error "ACCESS: ~s.~(~a~) is both ~s and ~s"
                                                     variable slot already reader)
                                         unless already
                                           do (setf (gethash slot seen) reader)
                                           and collect
                                           `(define-symbol-macro
                                                ,(alexandria:format-symbol *package* "~a.~a"
                                                                           variable slot)
                                              (,reader ,variable))))))))
