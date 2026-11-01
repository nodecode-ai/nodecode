;;;; contract.lisp --- DEFINE-CONTRACT: one boundary payload, declared once.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; JSON.LISP is the lenient dialect, for data a peer produced: a wrong-typed
;;;; value reads NIL and the caller decides what that meant. PROTOCOL.LISP is
;;;; the strict one for the gateway envelope, written a field at a time
;;;; (REQUIRED-STRING, OPTIONAL-INTEGER, PROTOCOL-OBJECT). This file is the
;;;; strict dialect made declarative, for the boundaries that are neither: a
;;;; model tool's arguments, an HTTP control request's body -- anywhere a
;;;; foreign JSON object becomes the value a Lisp owner runs on.
;;;;
;;;; The failure it removes is one shape stated twice. A registered tool
;;;; carries a hand-written JSON-schema object for the provider AND reads its
;;;; arguments again with WITH-JSON in the body, and nothing makes the two
;;;; agree: qa's report_issue advertised `required: tool, symptom' while its
;;;; body defaulted both to "unknown"/"other". TOOL-CALL-PREFLIGHT was a third
;;;; authority over the same shape -- a partial JSON-Schema reader that knows
;;;; `type' and `required' and ignores `enum'. A gateway request is the same
;;;; story with the producer and the consumer in one image: the client built
;;;; {package_id, method, params_json, session_id} by hand and gateway.lisp
;;;; read the four members back by hand.
;;;;
;;;; DEFINE-CONTRACT states the payload once and derives all of it: the record
;;;; the owner runs on (DEFINE-RECORD, its readers exported at the
;;;; declaration), the decoder that materializes it, the encoder that is the
;;;; decoder's inverse, and the JSON-Schema projection a catalog advertises.
;;;; The type vocabulary is JSON-VALUE's own +JSON-TYPES+ -- the same eight
;;;; keywords DEFINE-EVENT hydrates slots with -- so this is not a second type
;;;; language. Refusal is one condition, CONTRACT-REFUSAL, carrying the
;;;; contract, the member's wire path, what the declaration wanted and what
;;;; arrived.
;;;;
;;;; What it is not: a static type checker, a foreign IDL, or a replacement
;;;; for DEFINE-RECORD / DEFINE-ERROR / DEFINE-EVENT / DEFINE-SECTION /
;;;; DEFINE-CELL. It types ONE thing -- the value crossing a boundary -- and
;;;; everything above and below that line stays exactly as dynamic as it was.
;;;; A contract is live-only material: it carries no version and no hash,
;;;; because nothing durable is written from it (the event envelope's
;;;; CONTRACT-HASH-FOR is the durable side and stays events.lisp's), and a
;;;; redefinition replaces the declaration in place, which is how a boundary
;;;; already registered against one follows a live edit.

(in-package #:nodecode.kernel)

;;; --- the refusal ----------------------------------------------------------

(define-error contract-refusal (error)
    (contract path expected received)
  ;; CONTRACT names the
  ;; declaration, PATH the member's wire path from the payload's root
  ;; ("session_id", "providers[0].name"), EXPECTED what the declaration asked
  ;; for in the words the schema projection uses, RECEIVED what arrived. Signalled
  ;; by CONTRACT-DECODE before the boundary's owner runs, so a malformed payload
  ;; is refused at the boundary and never half-applied inside it.
  (:documentation
   "One member of a declared contract did not satisfy it.")
  (:report "~(~a~): ~a must be ~a, got ~a" contract path expected received)
  (:export))

(defun contract-refuse (contract path expected received)
  "Signal CONTRACT-REFUSAL naming the member and both sides of the mismatch."
  (error 'contract-refusal :contract contract :path path
                           :expected expected :received received))

;;; --- the declaration, as data ---------------------------------------------

(define-record (contract-field
                (:copier nil) (:export :constructor :readers)
                (:constructor make-contract-field
                    (key type &key slot initarg reader required default options of doc)))
  "One declared member of a contract."
  ;; KEY is the literal wire key; TYPE one of +JSON-TYPES+; SLOT, INITARG and
  ;; READER the names the materialized record carries it under; REQUIRED
  ;; whether the payload must carry it; DEFAULT what an absent member takes;
  ;; OPTIONS the values it may take; OF, on an :OBJECT, the contract it nests,
  ;; and on an :ARRAY the element contract or the element type; DOC its line
  ;; in the schema a catalog advertises.
  (key "" :type string :read-only t)
  (type :any :type keyword :read-only t)
  (slot nil :type symbol :read-only t)
  (initarg nil :type symbol :read-only t)
  (reader nil :type symbol :read-only t)
  (required nil :read-only t)
  (default nil :read-only t)
  (options '() :type list :read-only t)
  (of nil :type symbol :read-only t)
  (doc "" :type string :read-only t))

(access (field contract-field))

(define-record (contract
                (:copier nil) (:export :predicate :readers)
                (:constructor make-contract
                    (name &key doc fields unknown constructor)))
  "One declared boundary payload: NAME the declaration, DOC what the payload
is for, FIELDS its members in declaration order, UNKNOWN what a member the
declaration does not carry costs (:IGNORE or :REFUSE), CONSTRUCTOR the
record's own constructor, which CONTRACT-DECODE applies the materialized
members to."
  ;; Everything a boundary needs is derived from FIELDS, so the shape
  ;; a consumer is told (CONTRACT-SCHEMA) and the shape it is held to
  ;; (CONTRACT-DECODE) cannot be two shapes.
  (name nil :type symbol :read-only t)
  (doc "" :type string :read-only t)
  (fields '() :type list :read-only t)
  (unknown :ignore :type keyword :read-only t)
  (constructor nil :read-only t))

;;; DEFVAR, not DEFPARAMETER: this file reloading in a serving image must not
;;; drop the declarations its live boundaries resolve through.
(defvar *contracts* (make-hash-table :test #'eq)
  "Declared contracts by name.")

(defun declare-contract (contract)
  "Put CONTRACT on the registry under its name, replacing a declaration of the
same name in place -- which is how a redefined contract reaches the boundaries
already registered against it, with no second authority and no restart."
  (setf (gethash contract.name *contracts*) contract))

(defun contract (name)
  "The contract NAME declares."
  ;; Refuses an undeclared name rather than answering NIL: a boundary
  ;; registered against a contract that does not exist would advertise an
  ;; empty schema and then accept anything.
  (or (gethash name *contracts*)
      (error "no contract ~s; this image declares ~{~s~^ ~}"
             name (sort (alexandria:hash-table-keys *contracts*) #'string< :key #'symbol-name))))

;;; --- what a refusal says --------------------------------------------------

;;; EVAL-WHEN because DEFINE-CONTRACT reads the Lisp types while it expands.
;;; :ANY has no JSON Schema type on purpose: a member of that type is
;;; advertised with no `type' constraint, which is exactly what it is.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +contract-types+
    '((:any "a value" nil t) (:string "a string" "string" string)
      (:text "a non-empty string" "string" string) (:integer "an integer" "integer" integer)
      (:number "a number" "number" real) (:boolean "a boolean" "boolean" boolean)
      (:object "an object" "object" hash-table) (:array "an array" "array" vector))
    "Each of +JSON-TYPES+ as (TYPE NOUN SCHEMA-TYPE LISP-TYPE): what a refusal
calls it, so a model repairing a call and an operator reading a 400 see the
same word; how JSON Schema spells it; the Lisp type a member materializes as."))

(defparameter +contract-received-limit+ 48
  "How much of a wrong-typed string a refusal quotes back: enough to
recognize, never the payload.")

(defun contract-received (value)
  "VALUE as a refusal says it arrived -- named by JSON kind, a string quoted
and clipped -- so a refusal reads without the payload beside it."
  (cond ((cdr (assoc value '((:null . "null") (t . "true") (nil . "false")))))
        ((stringp value)
         (if (zerop (length value))
             "an empty string"
             (format nil "the string ~s" (clip value +contract-received-limit+))))
        ((integerp value) (format nil "the integer ~a" value))
        ((realp value) (format nil "the number ~a" value))
        ((hash-table-p value) "an object")
        ((vectorp value) "an array")
        (t (format nil "~s" value))))

(defun contract-path (prefix key)
  "KEY's wire path under PREFIX: its own key at the payload's root, dotted
under a nested one."
  (if (plusp (length prefix)) (concatenate 'string prefix "." key) key))

;;; --- decoding -------------------------------------------------------------

(defun contract-coerce (name type of value path)
  "VALUE as TYPE at PATH, or CONTRACT-REFUSAL under contract NAME."
  ;; OF, on an :OBJECT, names the contract the value nests; on an :ARRAY it
  ;; names the element contract, or an element type keyword for an array of
  ;; scalars.
  ;;
  ;; Nothing coerces across a type here -- the string "3" is not an integer --
  ;; because a coercion at a boundary is a second dialect with no owner.
  ;; JSON-VALUE is the lenient reader for a caller that wants one.
  (flet ((refuse (expected)
           (contract-refuse name path expected (contract-received value))))
    (case type
      (:any value)
      (:boolean (if (or (eq value t) (null value)) value (refuse "a boolean")))
      (:object
       (let ((table (or (json-value value :object) (refuse "an object"))))
         (if of (contract-decode (contract of) table path) table)))
      (:array
       (let ((items (or (json-value value :array) (refuse "an array"))))
         (if (null of)
             items
             (let ((index -1))
               (map 'vector
                    (lambda (item &aux (at (format nil "~a[~d]" path (incf index))))
                      (contract-coerce name (if (keywordp of) of :object) (unless (keywordp of) of)
                                       item at))
                    items)))))
      (t (or (json-value value type) (refuse (second (assoc type +contract-types+))))))))

(defun decode-contract-field (contract field object prefix &aux (key field.key)
                                                                (path (contract-path prefix key)))
  "FIELD's materialized value out of OBJECT, or CONTRACT-REFUSAL."
  ;; A member whose value is JSON null reads as ABSENT -- the collapse
  ;; SECTION-VALUE already makes image-wide -- so null and absent take the
  ;; same default and refuse the same way. JSON false is PRESENT and
  ;; materializes NIL on a :BOOLEAN, which is why presence is read off
  ;; GETHASH's second value and not off the coercion.
  (bind (((value present) (section-value object key)))
    (cond
      ((not present)
       (when field.required (contract-refuse contract.name path "present" "absent"))
       (contract-field-default field))
      (t
       (let ((materialized (contract-coerce contract.name field.type field.of value path))
             (options (contract-field-options field)))
         (when (and options (not (member materialized options :test #'equal)))
           (contract-refuse contract.name path
                            (format nil "one of ~{~a~^, ~}" options)
                            (contract-received value)))
         materialized)))))

(defun contract-decode (contract object &optional (prefix ""))
  "OBJECT, a decoded JSON object, as the record CONTRACT declares -- every
member typed, defaulted and checked before the boundary's owner sees it -- or
CONTRACT-REFUSAL naming the first member that did not satisfy the declaration."
  ;; PREFIX is the wire path OBJECT sits at, which nesting fills in; a caller
  ;; at the payload's root leaves it.
  (let ((table (json-value object :object)))
    (unless table
      (contract-refuse contract.name
                       (if (plusp (length prefix)) prefix "the payload")
                       "an object" (contract-received object)))
    (when (eq contract.unknown :refuse)
      (let ((declared (mapcar #'contract-field-key contract.fields)))
        (loop for key being the hash-keys of table
              unless (member key declared :test #'equal)
                do (contract-refuse
                    contract.name (contract-path prefix key)
                    (format nil "absent; this contract declares ~{~a~^, ~}" declared)
                    "a member"))))
    (apply contract.constructor
           (loop for field in (contract-fields contract)
                 collect field.initarg
                 collect (decode-contract-field contract field table prefix)))))

;;; --- encoding, the declared inverse ---------------------------------------

(defun encode-contract-value (type of value)
  "One member's value as the wire carries it: a nested contract encodes
through its own declaration, everything else passes."
  (cond ((null of) value)
        ((eq type :object) (contract-encode (contract of) value))
        ((and (eq type :array) (not (keywordp of)))
         (map 'vector (lambda (item) (contract-encode (contract of) item)) value))
        (t value)))

(defun contract-encode (contract value &aux (object (make-hash-table :test #'equal)))
  "VALUE, a record CONTRACT materialized, back as the JSON object it decoded
from -- so a producer in this image builds the payload the consumer decodes
through the same declaration, and the two ends of one boundary cannot drift."
  ;; A member whose value is NIL is absent, EXCEPT a :BOOLEAN, which always
  ;; writes: NIL is JSON false there and the record cannot hold `absent'. That
  ;; is the one place the round trip is not the identity, and it is the rule
  ;; CONFIG-BOOLEAN already keeps.
  (dolist (field contract.fields object)
    (let ((member (funcall field.reader value)))
      (when (or (eq (contract-field-type field) :boolean) member)
        (setf (gethash field.key object) (encode-contract-value field.type field.of member))))))

;;; --- the schema a catalog advertises --------------------------------------

(defun contract-field-schema (field)
  "One member's JSON-Schema description, derived from the declaration alone."
  (let* ((type (contract-field-type field))
         (of field.of)
         (base (if (and (eq type :object) of)
                   (contract-schema (contract of))
                   (json-object
                    :opt "type" (third (assoc type +contract-types+))
                    ;; :TEXT is a string the decoder refuses empty; a reader
                    ;; that speaks JSON Schema is told the same thing.
                    :when (eq type :text) "minLength" 1
                    :opt "enum" (let ((options (contract-field-options field)))
                                  (and options (coerce options 'vector)))
                    :when (and (eq type :array) of) "items"
                    (if (keywordp of)
                        (json-object :opt "type" (third (assoc of +contract-types+)))
                        (contract-schema (contract of)))))))
    (if (plusp (length field.doc)) (copy-json-object base "description" field.doc) base)))

(defun contract-schema (contract &aux (properties (make-hash-table :test #'equal))
                                      (required (loop for field in (contract-fields contract)
                                                      when (contract-field-required field)
                                                        collect field.key)))
  "CONTRACT as the JSON-Schema object a catalog advertises: one property per
declared member with its type, its enum and its line of documentation, and the
required list."
  ;; The projection and the decoder read the same FIELDS, which is what makes
  ;; the advertised shape and the dispatched shape one shape.
  (dolist (field contract.fields)
    (setf (gethash field.key properties)
          (contract-field-schema field)))
  (json-object "type" "object"
               "properties" properties
               :when required "required" (coerce required 'vector)))

;;; --- the declaration ------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter +contract-field-options+ '(:slot :required :default :options :of :doc)
    "Keywords a DEFINE-CONTRACT member spec may carry after its type.")

  (defun contract-lisp-type (type of)
    "The Lisp type a member of TYPE materializes as, for the record's own slot
declaration."
    ;; This is the RUNTIME type -- what the record checks on construction; the
    ;; wire type is the contract's, and what the decoder checks. A nested
    ;; :OBJECT materializes another record, whose class the declaration may
    ;; not have seen yet, so it declares T rather than a name that does not
    ;; resolve.
    (if (and of (eq type :object)) t (fourth (assoc type +contract-types+))))

  (defun parse-contract-field (name spec package)
    "One DEFINE-CONTRACT member spec as (KEY SLOT FIELD): its wire key, the
record's slot declaration for it, and the form that makes its CONTRACT-FIELD."
    ;; Every check a declaration can fail lands here, at macroexpansion: a
    ;; non-literal wire key, a type outside +JSON-TYPES+, an unknown option, a
    ;; member both :REQUIRED and :DEFAULT, :OF where nothing nests, :OPTIONS
    ;; on a member that has no values to choose between, and a derived slot
    ;; name that would bind a COMMON-LISP symbol.
    (unless (and (consp spec) (rest spec))
      (error "DEFINE-CONTRACT ~s: ~s is not a member (\"key\" TYPE OPTION...)" name spec))
    (destructuring-bind (key type &rest options) spec
      (unless (and (stringp key) (plusp (length key)))
        (error "DEFINE-CONTRACT ~s: ~s is not a literal wire key" name key))
      (flet ((refuse (control &rest arguments)
               (error "DEFINE-CONTRACT ~s: member ~s ~?" name key control arguments)))
        (unless (member type +json-types+)
          (refuse "declares type ~s (want one of ~{~s~^ ~})" type +json-types+))
        (unless (evenp (length options))
          (refuse "has an option with no value"))
        (loop for option in options by #'cddr
              unless (member option +contract-field-options+)
                do (refuse "has unknown option ~s (want one of ~{~s~^ ~})"
                           option +contract-field-options+))
        (unless (member (getf options :required) '(nil t))
          (refuse "declares :REQUIRED ~s; it is a literal T or NIL, because the record's ~
slot is declared from it" (getf options :required)))
        (when (and (getf options :required) (member :default options))
          (refuse "is :REQUIRED and :DEFAULT at once; a required member can never take a default"))
        (when (and (member :of options) (not (member type '(:object :array))))
          (refuse "declares :OF on a ~s; :OF nests a contract inside an :OBJECT or an :ARRAY"
                  type))
        (when (and (member :options options)
                   (not (member type '(:string :text :integer :number))))
          (refuse "declares :OPTIONS on a ~s; a fixed set of values is a scalar's constraint"
                  type))
        (let ((slot (or (getf options :slot) (wire-symbol key package))))
          (unless (and slot (symbolp slot) (not (keywordp slot)))
            (refuse "declares :SLOT ~s, which is not a slot name" slot))
          (when (eq (symbol-package slot) (find-package '#:common-lisp))
            (refuse "derives the slot name ~s, which is COMMON-LISP's and cannot be bound; ~
name another with :SLOT" slot))
          (destructuring-bind (&key required default options of doc &allow-other-keys) options
            (let ((lisp-type (contract-lisp-type type of)))
              (list key
                    ;; A required member has no sensible initform: the record
                    ;; is built by the decoder, which always passes every
                    ;; member, and a hand-made one missing a required member is
                    ;; the bug this says out loud (the SYNC-MESSAGE idiom).
                    `(,slot ,(and required `(error ,(format nil "~(~a~): ~a is required" name key)))
                      :type ,(if (or required (member lisp-type '(t boolean)))
                                 lisp-type
                                 `(or null ,lisp-type))
                      :read-only t)
                    `(make-contract-field
                      ,key ,type :slot ',slot :initarg ,(intern (symbol-name slot) :keyword)
                      :reader ',(alexandria:format-symbol package "~a-~a" name slot)
                      :required ,required :default ,default :options (or ,options '())
                      :of ',of :doc (or ,doc ""))))))))))

(defmacro define-contract (name &body clauses &aux (package (symbol-package name))
                                                   (doc nil)
                                                   (unknown :ignore)
                                                   (specs '()))
  "Declare the boundary payload NAME: its members with their literal wire keys
and their types, and -- derived from that, never restated -- the record it
materializes as, the decoder that materializes it, the encoder that is the
decoder's inverse, and the JSON-Schema projection a catalog advertises."
  ;; Clauses, in any order:
  ;;
  ;;   (:doc TEXT)                  what this payload is, for the schema a catalog
  ;;                                and a model read.
  ;;   (:unknown :IGNORE | :REFUSE) what a member the declaration does not carry
  ;;                                costs. :IGNORE by default -- a peer that adds a
  ;;                                key must not lose the call it rode in.
  ;;   ("key" TYPE OPTION...)       one member. KEY is the literal wire key,
  ;;                                checked at macroexpansion as WITH-JSON and
  ;;                                JSON-OBJECT check theirs; TYPE is one of
  ;;                                +JSON-TYPES+, the eight keywords JSON-VALUE
  ;;                                coerces by and DEFINE-EVENT hydrates with.
  ;;
  ;; Member options:
  ;;
  ;;   :required T     the payload must carry it; absent is a refusal.
  ;;   :default FORM   what an absent member takes, evaluated once where the
  ;;                   declaration stands. Refused beside :REQUIRED.
  ;;   :options FORM   the values it may take; one outside them is a refusal naming
  ;;                   the set, and the schema advertises it as `enum'.
  ;;   :of NAME        on an :OBJECT, the contract the member nests; on an :ARRAY,
  ;;                   the element contract, or an element type keyword for an
  ;;                   array of scalars.
  ;;   :slot SYMBOL    the record's slot name, when the derived one
  ;;                   (max_results -> MAX-RESULTS) will not do.
  ;;   :doc FORM       the member's line in the schema.
  ;;
  ;; Defines, in NAME's package and exported there: the record NAME with one
  ;; read-only slot per member, MAKE-NAME, NAME-P and one reader per member;
  ;; DECODE-NAME, which materializes a decoded JSON object into that record or
  ;; signals CONTRACT-REFUSAL; and ENCODE-NAME, its inverse. Registers the
  ;; declaration under NAME, reachable as (CONTRACT 'NAME), which is what a
  ;; boundary registers against. Answers NAME.
  ;;
  ;;   (nlk:define-contract issue-report
  ;;     (:doc "A tool that misbehaved, as the model reports it.")
  ;;     ("tool"    :text :required t :doc "the tool, as you called it")
  ;;     ("symptom" :text :required t :options +symptoms+ :doc "what went wrong")
  ;;     ("note"    :text :default ""  :doc "one sentence, at most 280 characters"))
  (unless (and name (symbolp name) (not (keywordp name)))
    (error "DEFINE-CONTRACT: ~s is not a contract name" name))
  (dolist (clause clauses)
    (case (and (consp clause) (if (stringp (first clause)) :member (first clause)))
      (:doc (setf doc (second clause)))
      (:unknown (setf unknown (second clause))
                (unless (member unknown '(:ignore :refuse))
                  (error "DEFINE-CONTRACT ~s: (:unknown ~s) is :IGNORE or :REFUSE"
                         name unknown)))
      (:member (push (parse-contract-field name clause package) specs))
      (t (error "DEFINE-CONTRACT ~s: ~s is neither a clause nor a member ~
(\"key\" TYPE OPTION...)"
                name clause))))
  (setf specs (nreverse specs))
  (unless specs
    (error "DEFINE-CONTRACT ~s: a contract with no members types nothing" name))
  (when-let (duplicate (loop for (spec . rest) on specs
                             when (find (first spec) rest :key #'first :test #'equal)
                               return (first spec)))
    (error "DEFINE-CONTRACT ~s: member ~s is declared twice" name duplicate))
  (let ((constructor (intern (concatenate 'string "MAKE-" (symbol-name name)) package))
        (decoder (intern (concatenate 'string "DECODE-" (symbol-name name)) package))
        (encoder (intern (concatenate 'string "ENCODE-" (symbol-name name)) package)))
    `(progn
       (define-record (,name (:copier nil) (:export t))
         ,(or doc (format nil "The value ~(~a~) materializes at its boundary." name))
         ,@(mapcar #'second specs))
       (defun ,decoder (object)
         ,(format nil "OBJECT, a decoded JSON object, as a ~(~a~) -- every ~
member typed, defaulted and checked -- or NLK:CONTRACT-REFUSAL naming the ~
member that did not satisfy the declaration."
                  name)
         (contract-decode (contract ',name) object))
       (defun ,encoder (value)
         ,(format nil "VALUE, a ~(~a~), back as the JSON object it decoded ~
from: the declared inverse of ~(~a~)."
                  name decoder)
         (contract-encode (contract ',name) value))
       (export '(,decoder ,encoder) ,(package-name package))
       (declare-contract
        (make-contract
         ',name
         :doc ,(or doc "")
         :unknown ,unknown
         :constructor #',constructor
         :fields (list ,@(mapcar #'third specs))))
       ',name)))
