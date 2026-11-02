;;;; contract-test.lisp --- DEFINE-CONTRACT, the boundary declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What these gates are about is the one property the declaration exists for:
;;;; the shape a consumer is TOLD (CONTRACT-SCHEMA) and the shape it is HELD TO
;;;; (CONTRACT-DECODE) are two projections of one FIELDS list, so they cannot be
;;;; two shapes. Everything else here is the table that makes that projection
;;;; honest: absent versus JSON false versus JSON null, required, defaulted,
;;;; enumerated, nested, the unknown-member policy, and the refusal that names
;;;; the member's wire path.
;;;;
;;;; The declarations below are the test's own fixtures. They are declared at
;;;; load time, like every other contract in the image, because the registry is
;;;; what a boundary resolves through and a fixture that bypassed it would not
;;;; be testing the same thing.

(in-package #:nodecode.test)

(nlk:access (contract nlk::contract) (field nlk::contract-field))

;;; --- the fixtures ----------------------------------------------------------

(nlk:define-contract contract-probe-endpoint
  (:doc "A host and the port it answers on.")
  ("host" :text :required t :doc "the host")
  ("port" :integer :default 443 :doc "the port"))

(nlk:define-contract contract-probe
  (:doc "Every shape the declaration can carry, in one payload.")
  ("id" :text :required t :doc "the id")
  ;; COUNT is COMMON-LISP's; the wire key is untouched and :SLOT names the
  ;; record's slot, which is the escape the macroexpansion refusal points at.
  ("count" :integer :default 3 :slot how-many :doc "how many")
  ("live" :boolean :doc "whether it is live")
  ("mode" :string :default "fast" :options '("fast" "slow") :doc "the mode")
  ("note" :string :doc "anything at all")
  ("anything" :any :doc "whatever arrived")
  ("endpoint" :object :of contract-probe-endpoint :doc "where it answers")
  ("tags" :array :of :string :doc "its tags")
  ("mirrors" :array :of contract-probe-endpoint :doc "where else it answers"))

(nlk:define-contract contract-probe-closed
  (:doc "A payload whose members are all of them.")
  (:unknown :refuse)
  ("only" :text :required t :doc "the one member"))

(defun contract-probe-of (json)
  "One CONTRACT-PROBE decoded from JSON text."
  (decode-contract-probe (nlk:decode-json json)))

(defun contract-probe-refusal (json)
  "The CONTRACT-REFUSAL decoding JSON text as a CONTRACT-PROBE signals."
  (signals-error nlk:contract-refusal (contract-probe-of json)))

(defmacro is-refused (json description &body checks)
  "Assert decoding JSON as a CONTRACT-PROBE refuses, under DESCRIPTION, and each
CHECK, (SLOT ...), as IS-SHAPE's check of the refusal's reader for SLOT."
  `(is-present (refusal (contract-probe-refusal ,json)) ,description
     (is-shape refusal ,@(loop for (slot . check) in checks
                               collect `(,(alexandria:format-symbol :nlk "CONTRACT-REFUSAL-~a" slot)
                                         ,@check)))))

;;; --- the declaration's own names -------------------------------------------

(deftest contract-declaration-derives-its-boundary-names ()
  ;; The record, its readers, the decoder and the encoder are the declaration's
  ;; own names, exported where it stands — no package mirror to fall behind a
  ;; member, which is DEFINE-RECORD's and DEFINE-ERROR's rule.
  (is-table (name) (eq :external (nth-value 1 (find-symbol name '#:nodecode.test)))
    ("CONTRACT-PROBE" "the record")
    ("MAKE-CONTRACT-PROBE" "the constructor")
    ("CONTRACT-PROBE-P" "the predicate")
    ("CONTRACT-PROBE-ID" "a reader per member")
    ("CONTRACT-PROBE-HOW-MANY" ":slot names the reader, the wire key does not")
    ("DECODE-CONTRACT-PROBE" "the decoder")
    ("ENCODE-CONTRACT-PROBE" "the encoder"))
  (is (nlk:contract-p (nlk:contract 'contract-probe)) "registered under its name")
  (is (equal '("id" "count" "live" "mode" "note" "anything" "endpoint" "tags" "mirrors")
             (mapcar #'nlk:contract-field-key
                     (nlk:contract-fields (nlk:contract 'contract-probe)))))
  (is (search "Every shape" (nlk:contract-doc (nlk:contract 'contract-probe))))
  (signals-error error (nlk:contract 'no-such-contract-anywhere)))

(deftest contract-record-is-immutable-and-typed (let ((probe (contract-probe-of "{\"id\":\"x\"}"))))
  (is (equal "x" (contract-probe-id probe)))
  ;; The slots are read-only: a materialized value is what crossed the
  ;; boundary, not a place a later owner edits.
  (is (null (fboundp '(setf contract-probe-id))) "no writer is generated")
  ;; A required member has no initform, so a construction site that leaves it
  ;; out is a compile-time warning before it is a run-time refusal — the
  ;; static half of a gradual contract, and all of it there is.
  (is (nth-value 1 (let ((*error-output* (make-broadcast-stream)))
                     (compile nil '(lambda () (make-contract-probe :how-many 1))))))
  (signals-error error (funcall 'make-contract-probe :how-many 1)
                "and refuses at run time")
  (signals-error type-error (funcall 'make-contract-probe :id 7)
                "the slot carries the runtime type the wire type became"))

;;; --- malformed declarations, refused where they are written ----------------

(deftest contract-declaration-refuses-at-macroexpansion ()
  ;; Every one of these is a bug in a declaration, and every one of them costs
  ;; the author a macroexpansion error rather than a boundary that quietly
  ;; types nothing.
  (is-table (form) (signals-error error (macroexpand-1 form))
    ('(nlk:define-contract bad ("k" :strng)) "a type outside +JSON-TYPES+")
    ('(nlk:define-contract bad (k :string)) "a wire key that is not a literal string")
    ('(nlk:define-contract bad ("" :string)) "an empty wire key")
    ('(nlk:define-contract bad ("k" :string :requried t)) "an unknown member option")
    ('(nlk:define-contract bad ("k" :string :default)) "an option with no value")
    ('(nlk:define-contract bad ("k" :string :required t :default "x"))
     "required and defaulted at once")
    ('(nlk:define-contract bad ("k" :string :required :maybe)) "a non-literal :required")
    ('(nlk:define-contract bad ("k" :string :of other)) ":of where nothing nests")
    ('(nlk:define-contract bad ("k" :object :options '("a"))) ":options on an object")
    ('(nlk:define-contract bad ("k" :string) ("k" :integer)) "one key declared twice")
    ('(nlk:define-contract bad (:doc "d")) "a contract with no members")
    ('(nlk:define-contract bad (:unknown :maybe) ("k" :string))
     "an unknown-member policy that is neither")
    ('(nlk:define-contract bad (:whatever) ("k" :string)) "a clause that is not one")
    ('(nlk:define-contract bad ("count" :integer))
     "a derived slot name COMMON-LISP owns; :SLOT is the escape")))

;;; --- required, optional, defaulted, nested ---------------------------------

(deftest contract-decode-materializes-every-member-shape ()
  (let ((probe (contract-probe-of
                "{\"id\":\"x\",\"count\":7,\"live\":true,\"mode\":\"slow\",
                  \"note\":\"\",\"anything\":[1,2],
                  \"endpoint\":{\"host\":\"h\"},\"tags\":[\"a\",\"b\"],
                  \"mirrors\":[{\"host\":\"m\",\"port\":80}]}")))
    (is-shape probe (contract-probe-id "x" "a required member")
      (contract-probe-how-many eql 7 "an integer under its own slot")
      (contract-probe-live eq t "JSON true") (contract-probe-mode "slow" "one of the options")
      (contract-probe-note "" "a :string may be empty")
      (contract-probe-anything equalp #(1 2) ":any passes what arrived")
      (contract-probe-tags equalp #("a" "b") "an array of scalars"))
    (is-present (endpoint (contract-probe-endpoint probe)) "a nested object materializes"
      (is-shape endpoint (contract-probe-endpoint-host "h") (contract-probe-endpoint-port eql 443)))
    (is-present (mirrors (contract-probe-mirrors probe)) "an array of nested records"
      (is (= 1 (length mirrors)))
      (is (equal "m" (contract-probe-endpoint-host (aref mirrors 0))))
      (is (eql 80 (contract-probe-endpoint-port (aref mirrors 0)))))))

(deftest contract-decode-defaults-what-the-payload-left-out ()
  (let ((probe (contract-probe-of "{\"id\":\"x\"}")))
    (is-shape probe (contract-probe-how-many eql 3 "an absent member takes its default")
      (contract-probe-mode "fast")
      (contract-probe-note null "an absent member with no default is NIL")
      (contract-probe-endpoint null) (contract-probe-tags null))))

;;; --- absent vs JSON false vs JSON null -------------------------------------

(deftest contract-decode-separates-absent-false-and-null ()
  ;; The three shasht decodings every hand-written reader in this image used to
  ;; re-derive, decided once by the declaration.
  (is (null (contract-probe-live (contract-probe-of "{\"id\":\"x\"}"))))
  (is (null (contract-probe-live (contract-probe-of "{\"id\":\"x\",\"live\":false}"))))
  (is (eq t (contract-probe-live (contract-probe-of "{\"id\":\"x\",\"live\":true}"))))
  ;; false is PRESENT, so it does not take a default; null is ABSENT, so it
  ;; does. That is the whole difference, and it is only visible on a defaulted
  ;; member.
  (is (eql 3 (contract-probe-how-many (contract-probe-of "{\"id\":\"x\",\"count\":null}"))))
  (is (equal "fast" (contract-probe-mode (contract-probe-of "{\"id\":\"x\",\"mode\":null}"))))
  (is-refused "{\"id\":null}" "a required member sent as null is absent, and refuses"
    (path "id") (received "absent")))

;;; --- refusal: the type, the path, the options ------------------------------

(deftest contract-refusal-names-the-member-and-both-sides ()
  (is-present (refusal (contract-probe-refusal "{}")) "a missing required member"
    (is-shape refusal (nlk:contract-refusal-contract eq 'contract-probe)
      (nlk:contract-refusal-path "id") (nlk:contract-refusal-expected "present")
      (nlk:contract-refusal-received "absent"))
    (is-carrying (text (princ-to-string refusal))
      ("contract-probe" "the report names the declaration")
      ("id must be present" "and the member")))
  (is-refused "{\"id\":\"x\",\"count\":\"7\"}" "a wrong-typed member is refused, never coerced"
    (path "count" "the wire key, not the slot") (expected "an integer")
    (received "the string \"7\""))
  (is-refused "{\"id\":\"\"}" "a :text member may not be empty"
    (expected "a non-empty string") (received "an empty string"))
  (is-refused "{\"id\":\"x\",\"mode\":\"medium\"}" "a value outside the options"
    (expected "one of fast, slow"))
  (is-refused "{\"id\":\"x\",\"live\":\"yes\"}" "a boolean is T or NIL and nothing else"
    (expected "a boolean"))
  (is-present (refusal (signals-error nlk:contract-refusal
                         (nlk:contract-decode (nlk:contract 'contract-probe) 7)))
      "a payload that is not an object at all"
      (is (equal "the payload" (nlk:contract-refusal-path refusal)))))

(deftest contract-refusal-paths-reach-into-nested-values ()
  ;; The path is the reason a nested declaration is worth having: a refusal
  ;; two levels in still says which member of which element.
  (is-refused "{\"id\":\"x\",\"endpoint\":{}}" "a member of a nested object"
    (path "endpoint.host") (contract eq 'contract-probe-endpoint))
  (is-refused "{\"id\":\"x\",\"mirrors\":[{\"host\":\"a\"},{\"port\":1}]}"
      "a member of an element of an array" (path "mirrors[1].host"))
  (is-refused "{\"id\":\"x\",\"tags\":[\"a\",3]}" "an element of an array of scalars"
    (path "tags[1]") (expected "a string"))
  (is-refused "{\"id\":\"x\",\"endpoint\":3}" "a nested member that is not an object"
    (path "endpoint") (expected "an object")))

;;; --- the unknown-member policy ---------------------------------------------

(deftest contract-unknown-members-are-ignored-unless-declared-otherwise ()
  ;; :IGNORE is the default because a peer that adds a key must not lose the
  ;; call it rode in — the posture every wire in this image already had.
  (is (equal "x" (contract-probe-id (contract-probe-of "{\"id\":\"x\",\"surprise\":1}"))))
  (is (equal "x" (contract-probe-closed-only
                  (decode-contract-probe-closed (nlk:decode-json "{\"only\":\"x\"}")))))
  (is-present (refusal (signals-error nlk:contract-refusal
                         (decode-contract-probe-closed
                          (nlk:decode-json "{\"only\":\"x\",\"extra\":1}"))))
      "a closed contract refuses what it does not"
      (is (equal "extra" (nlk:contract-refusal-path refusal)))
      (is (search "this contract declares only"
                  (nlk:contract-refusal-expected refusal)))))

;;; --- the encoder, the declared inverse -------------------------------------

(deftest contract-encode-round-trips-every-member-it-carried ()
  (let* ((text "{\"id\":\"x\",\"count\":7,\"live\":true,\"mode\":\"slow\",
                 \"note\":\"n\",\"anything\":\"a\",
                 \"endpoint\":{\"host\":\"h\",\"port\":80},
                 \"tags\":[\"a\"],\"mirrors\":[{\"host\":\"m\",\"port\":81}]}")
         (once (contract-probe-of text))
         (encoded (encode-contract-probe once))
         (twice (nlk:contract-decode (nlk:contract 'contract-probe) encoded)))
    (is-table (reader) (equalp (reader once) (reader twice))
      (contract-probe-id) (contract-probe-how-many) (contract-probe-live)
      (contract-probe-mode) (contract-probe-note) (contract-probe-anything)
      (contract-probe-tags))
    (is (equal "h" (contract-probe-endpoint-host (contract-probe-endpoint twice))))
    (is (eql 81 (contract-probe-endpoint-port (aref (contract-probe-mirrors twice) 0))))
    (is-shape encoded ((:text "id") "x" "under the literal wire key") ((:integer "count") eql 7))))

(deftest contract-encode-omits-absence-and-always-writes-a-boolean ()
  ;; The one place the round trip is not the identity, and the reason: a record
  ;; can hold NIL, and NIL on a :BOOLEAN means JSON false, not `absent'.
  (let ((encoded (encode-contract-probe (contract-probe-of "{\"id\":\"x\"}"))))
    (is (null (nth-value 1 (gethash "note" encoded))))
    (is (null (nth-value 1 (gethash "endpoint" encoded))))
    (is (nth-value 1 (gethash "live" encoded)) "a boolean is always written")
    (is-shape encoded ("live" null "and an absent one writes false") ("count" eql 3))))

;;; --- the projection a catalog advertises -----------------------------------

(deftest contract-schema-is-the-same-fields-the-decoder-reads ()
  ;; The gate the whole declaration exists for: the advertised shape and the
  ;; dispatched shape are derived from one list, so a member cannot be in one
  ;; and not the other, and required cannot mean two things.
  (let* ((contract (nlk:contract 'contract-probe))
         (schema (nlk:contract-schema contract))
         (properties (nlk:json-value schema :object "properties")))
    (is (equal "object" (nlk:json-value schema :text "type")))
    (is (alexandria:set-equal (mapcar #'nlk:contract-field-key contract.fields)
                              (alexandria:hash-table-keys properties) :test #'equal))
    (is (equal (loop for field in (nlk:contract-fields contract)
                     when field.required
                       collect field.key)
               (coerce (nlk:json-value schema :array "required") 'list)))
    ;; And what a member is advertised as is what it is decoded as.
    (is-shape properties ((:text "count" "type") "integer") ((:text "id" "type") "string")
      ((:integer "id" "minLength") eql 1) ((:array "mode" "enum") equalp #("fast" "slow"))
      ((:text "count" "description") "how many") ((:any "anything" "type") null)
      ((:text "tags" "items" "type") "string"))
    (is-present (nested (nlk:json-value properties :object "endpoint"))
        "a nested object advertises the nested declaration whole"
        (is-shape nested ((:text "type") "object") ((:array "required") equalp #("host"))
          ((:text "properties" "port" "type") "integer")))))

(deftest contract-redefinition-reaches-the-boundary-that-resolves-it ()
  ;; The organism edits itself: a contract redefined in a live image replaces
  ;; the declaration in place, and every boundary that resolves it by name
  ;; follows without a restart. There is no version, no hash and no second copy
  ;; to go stale — a boundary contract is live-only material, which is why
  ;; nothing durable is ever written from one.
  (let ((before (nlk:contract 'contract-probe-closed))
        (payload (nlk:decode-json "{\"only\":\"x\",\"extra\":1}")))
    (nlk:with-cleanup ((nlk:declare-contract before))
      (is (signals-error nlk:contract-refusal (decode-contract-probe-closed payload)))
      ;; A declaration is evaluated where it stands, under its file's
      ;; IN-PACKAGE; EVAL here has to say so for the record's exports.
      (let ((*package* (find-package '#:nodecode.test)))
        (eval '(nlk:define-contract contract-probe-closed
                (:doc "A payload whose members are all of them.")
                (:unknown :ignore)
                ("only" :text :required t :doc "the one member"))))
      (is (equal "x" (contract-probe-closed-only
                      (decode-contract-probe-closed payload))))
      (is (eq :ignore (nlk:contract-unknown (nlk:contract 'contract-probe-closed)))))
    (is (signals-error nlk:contract-refusal (decode-contract-probe-closed payload)))))
