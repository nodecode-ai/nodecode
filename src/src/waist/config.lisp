;;;; config.lisp --- typed section accessors over decoded shared config.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The core parses ~/.nodecode/config.jsonc once and hands the whole decoded
;;;; object to every consumer (the gateway's cell hook passes it through
;;;; verbatim); this file owns reading a section out of it with types,
;;;; defaults, and refusals. It exists because the same accessor family was
;;;; line-identical in two cells (channel kit and guard) with only the
;;;; condition class differing — the waist is the one owner now.
;;;;
;;;; shasht reads JSON false as NIL and null as :NULL, so boolean reads need
;;;; GETHASH's second value to tell false from missing, and every read
;;;; collapses :NULL to absent.
;;;;
;;;; PROVIDER.LISP'S TRIMMED-CONFIG-STRING IS EXEMPT, deliberately: it is the
;;;; Zig-parity lenient reader (coerces numbers to strings, never refuses) for
;;;; the provider table, a different contract from the refusing accessors here.

(in-package #:nodecode.kernel)

(define-refusal config-refusal config-error "config refusal: ~a"
  ;; Permanent (config class): the consumer stays absent with one loud
  ;; warning; the gateway boots regardless.
  "A config section refused to validate." :export t)

(defun section-value (section key)
  "(values VALUE PRESENT-P) with JSON null collapsed to absent."
  (multiple-value-bind (value present) (and (hash-table-p section) (gethash key section))
    (if (and present (not (eq value :null)))
        (values value t)
        (values nil nil))))

(defun config-string (section key &optional default)
  (multiple-value-bind (value present) (section-value section key)
    (cond ((not present) default)
          ((stringp value)
           ;; shasht builds fill-pointered strings; callers hand these to
           ;; simple-string consumers (uiop:getenv), so normalize here.
           (let ((trimmed (coerce (string-trim '(#\Space #\Tab) value)
                                  'simple-string)))
             (if (plusp (length trimmed)) trimmed default)))
          (t (config-error "~a must be a string, got ~s" key value)))))

(defun config-integer (section key default &key min)
  (multiple-value-bind (value present) (section-value section key)
    (cond ((not present) default)
          ((integerp value)
           (when (and min (< value min))
             (config-error "~a must be >= ~a, got ~a" key min value))
           value)
          (t (config-error "~a must be an integer, got ~s" key value)))))

(defun config-boolean (section key default)
  "Absence keys the default; an explicit JSON false stays false."
  (multiple-value-bind (value present) (section-value section key)
    (if present (and value t) default)))

(defun config-enum (section key default values &aux (value (config-string section key default)))
  (unless (member value values :test #'equal)
    (config-error "~a must be one of ~{~a~^|~}, got ~s" key values value))
  value)

(defun config-string-list (section key)
  "A JSON array of strings as a list; absent is '()."
  ;; Entries are trimmed and empties dropped, so a padded allowlist id still
  ;; matches the wire id.
  (multiple-value-bind (value present) (section-value section key)
    (cond ((not present) '())
          ((vectorp value)
           (loop for entry across value
                 for trimmed = (if (stringp entry)
                                   (string-trim '(#\Space #\Tab) entry)
                                   (config-error "~a entries must be strings, got ~s" key entry))
                 when (plusp (length trimmed))
                   collect trimmed))
          (t (config-error "~a must be an array of strings, got ~s"
                           key value)))))

;;; --- declared sections --------------------------------------------------------
;;; A folder says what its section of the shared config is made of, once, as
;;; data: its fields with their types and docs, the constraints between them,
;;; a guide paragraph for the operator, a check to run once the section is
;;; complete, and the reading its owner adds where a declaration cannot say
;;; enough. Five readers derive from that one declaration and agree by
;;; construction: the folder's own refusal at start (SECTION-PROBLEMS), the
;;; settings a page and /settings draw (the fields, in order), the model's setup primer
;;; (SECTION-TEXT), the cells report's one line naming what is missing
;;; (SECTION-SUMMARY), and the typed immutable values the folder runs on
;;; (SECTION-SETTINGS) -- the struct and parser every folder used to write
;;; for itself. SECTION-SETTINGS is also what a writer asks before it writes,
;;; so a value the folder would refuse at its start is refused where it is
;;; typed. Nothing here knows a channel from a search provider: a
;;; declaration is config vocabulary only.
;;;
;;; The declaration is the authority; a folder's config.example.jsonc, where
;;; one still exists, is an illustration of it.

(define-record (section-field
                (:copier nil) (:export :constructor :readers)
                (:constructor make-section-field
                    (name type &key required default doc options choices min)))
  "One member of a declared section."
  ;; TYPE is :string, :secret (a value written inline), :env (the NAME of an
  ;; environment variable holding a secret), :path (a file path), :directory (a
  ;; folder: ~/ expanded, a relative DEFAULT under the home), :boolean,
  ;; :integer, :list (an array of strings) or :choice (one of OPTIONS). MIN,
  ;; on an :integer, is the smallest value it may take. CHOICES, on a :list,
  ;; is a function of the section's table answering ((id . label) ...) — how a
  ;; panel offers ids by name — or signalling CONFIG-REFUSAL with the reason
  ;; it cannot. A second value, a plist, may say more: :WAIT TEXT, there is
  ;; nothing to pick from until the operator does what TEXT says on the
  ;; platform (a walk waits and asks again); :FOUND TEXT, one line on what
  ;; there is to pick from (a walk's wait ends on it); :DEFAULT IDS, what an
  ;; unset field takes when the operator just presses Enter.
  (name "" :type string)
  (type :string :type keyword)
  (required nil)
  (default nil)
  (doc "" :type string)
  (options '() :type list)
  (choices nil)
  (min nil))

(define-record (section (:copier nil) (:export :constructor :readers))
  "A declared section of the shared config."
  ;; PATH is the member path from the root ("channels" "discord"); OWNER the
  ;; folder or system that declared it, the name the cells report files it
  ;; under. ONE-OF and ANY-OF are lists of field-name lists: exactly one
  ;; member of a ONE-OF group must be set, at least one member of an ANY-OF
  ;; group must be non-empty. GUIDE is the operator's walk in prose; CHECK,
  ;; when present, is a function of the section's table answering a text once
  ;; the section is complete (and, as a second value, one line saying what it
  ;; saw, which a setup walk folds the step into), or signalling
  ;; CONFIG-REFUSAL. READER, when
  ;; present, is a function of the values the fields derive and the section's
  ;; table, answering the plist the owner runs on or signalling
  ;; CONFIG-REFUSAL: a nested object, a bound between members, a value read
  ;; off the environment.
  (path '() :type list)
  (owner "" :type string)
  (fields '() :type list)
  (one-of '() :type list)
  (any-of '() :type list)
  (guide "" :type string)
  (check nil)
  (reader nil))

(defvar *sections* '()
  "Every declared section, in declaration order; a later declaration of
the same path replaces the earlier one in place.")

(defun section-key (path)
  "PATH as the dotted key the reports and the refusals spell it with."
  (format nil "~{~a~^.~}" path))

(defun find-section (path)
  "The declared section at PATH (a list of member keys), or NIL."
  (find path *sections* :key #'section-path :test #'equal))

(defun sections ()
  "Every declared section, in declaration order."
  (copy-list *sections*))

(defun declare-section (section)
  "Put SECTION on the registry, replacing a declaration of the same path."
  (let ((existing (find-section section.path)))
    (setf *sections* (if existing
                         (substitute section existing *sections*)
                         (append *sections* (list section)))))
  section)

(defmacro define-section ((&rest path) &body clauses)
  "Declare the section at PATH."
  ;; Each clause is either an option — (:owner NAME) (:guide TEXT) (:check
  ;; FORM) (:reader FORM) (:one-of NAME...) (:any-of NAME...) — or a field, (NAME TYPE &key
  ;; required default doc options choices), whose keyword arguments are forms.
  ;; OWNER defaults to the declaring package's name, downcased, which by the
  ;; folder convention is the folder's name.
  (flet ((option (key default &aux (clause (find key clauses :key #'first :from-end t)))
           (if clause (second clause) default))
         (groups (key)
           (loop for (head . names) in clauses when (eq head key) collect names)))
    `(declare-section
      (make-section
       :path ',path
       :owner ,(option :owner (string-downcase (package-name *package*)))
       :fields (list ,@(loop for (name . field) in clauses
                             unless (member name '(:owner :guide :check :reader :one-of :any-of))
                               collect (if (and (stringp name) field)
                                           `(make-section-field ,name ,@field)
                                           (error "define-section: unknown clause ~s"
                                                  (cons name field)))))
       :one-of ',(groups :one-of)
       :any-of ',(groups :any-of)
       :guide ,(option :guide "")
       :check ,(option :check nil)
       :reader ,(option :reader nil)))))

(defun section-table (config path)
  "The object at PATH in CONFIG — the whole decoded shared config — as a
hash table, or NIL when any step of the path is absent or not an object."
  (apply #'json-value config :object path))

(defun section-find-field (section name)
  (find name section.fields :key #'section-field-name :test #'string=))

(defun field-present-p (table field)
  "Whether FIELD is set in TABLE, by its type's notion of set: a :list is
set when non-empty, a :boolean or an :integer when the member is there, a
string-valued field when its trimmed value is non-empty."
  ;; A value of the wrong shape counts as set — SECTION-PROBLEMS names it.
  (multiple-value-bind (value present) (section-value table (section-field-name field))
    (flet ((filled-p (entry)
             (and (stringp entry) (plusp (length (string-trim '(#\Space #\Tab) entry))))))
      (and present
           (case (section-field-type field)
             (:list (or (not (vectorp value)) (some #'filled-p value)))
             ((:boolean :integer) t)
             (t (or (not (stringp value)) (filled-p value))))))))

(defun section-gaps (section table &aux (pair (section-secret-pair section)) (gaps '()))
  "What keeps SECTION from being complete over TABLE (its object in the
config, or NIL when absent), in declaration order: each a (PROBLEM . NEED),
the refusal's text and the operator's words for the member it misses, NEED
NIL for a problem that is not a missing member."
  ;; A member of the wrong shape is what SECTION-FIELD-VALUE refuses.
  (flet ((present-p (name)
           (when-let (field (section-find-field section name)) (field-present-p table field)))
         (gap (problem &optional need) (push (cons problem need) gaps)))
    (dolist (field section.fields)
      (let* ((key (section-field-name field))
             (problem (handler-case (progn (section-field-value table field) nil)
                        (config-refusal (condition)
                          (princ-to-string condition))))
             (missing (and (section-field-required field)
                           (not (field-present-p table field)))))
        (cond (problem (gap problem (and missing key)))
              (missing (gap (format nil "~a is required" key) key)))))
    (dolist (group section.one-of)
      (let ((set (remove-if-not #'present-p group)))
        (cond ((null set)
               (gap (format nil "one of ~{~a~^ / ~} is required" group)
                    (if (and pair (= 2 (length group))
                             (member (section-field-name (car pair)) group :test #'string=)
                             (member (section-field-name (cdr pair)) group :test #'string=))
                        (secret-pair-noun pair)
                        (format nil "~{~a~^ or ~}" group))))
              ((rest set)
               (gap (format nil "exactly one of ~{~a~^ / ~} may be set, ~d are"
                            group (length set)))))))
    (dolist (group section.any-of)
      (unless (some #'present-p group)
        (gap (format nil "at least one of ~{~a~^ / ~} must be non-empty" group)
             (format nil "~{~a~^ or ~}" group)))))
  (nreverse gaps))

(defun section-problems (section table)
  "What keeps SECTION from being complete over TABLE (its object in the
config, or NIL when absent), as texts in declaration order: a required
field absent, a member of the wrong shape, a ONE-OF group with none or
several set, an ANY-OF group with none non-empty. Empty means complete."
  (mapcar #'car (section-gaps section table)))

(defun section-settings (section table)
  "SECTION's members read out of TABLE -- its object in the config, or NIL
when the section is absent -- as an immutable plist in declaration order,
each value typed and defaulted exactly as the declaration says
(WIRE-SYMBOL names it), then through the section's READER when it
declares one."
  ;; The fifth reader off the one declaration, and the settings struct with
  ;; its parser that every folder used to write twice.
  ;;
  ;; A member of the wrong shape, a :choice outside its options, an :integer
  ;; under its MIN all refuse (CONFIG-REFUSAL) -- which is the folder's start
  ;; refusal, landing on its record and the board. A REQUIRED member that is
  ;; merely absent does NOT refuse here: that is what a section still needs
  ;; (SECTION-PROBLEMS), and a folder whose section is unanswered still runs
  ;; whatever half of itself needs no answer.
  (let ((values (loop for field in (section-fields section)
                      append (list (wire-symbol (section-field-name field))
                                   (section-field-value table field)))))
    (if section.reader (funcall section.reader values table) values)))

(defun section-field-value (table field &aux (name (section-field-name field))
                                            (default (section-field-default field)))
  "FIELD's member of TABLE, typed and defaulted as its declaration says, or
CONFIG-REFUSAL for a member of the wrong shape."
  (ecase (section-field-type field)
    ((:string :secret :env :path) (config-string table name default))
    (:directory (if-let (text (config-string table name))
                  (uiop:ensure-directory-pathname (expand-home text))
                  (and default (home default))))
    (:boolean (config-boolean table name (and default (not (eq default :false)))))
    (:integer (config-integer table name default :min (section-field-min field)))
    ;; an absent member is its default; a present one, an empty array
    ;; included, is what it says
    (:list (if (nth-value 1 (section-value table name))
               (config-string-list table name)
               (coerce default 'list)))
    ;; absent and undefaulted is NIL, not a refusal: an unanswered choice is
    ;; a state, and CONFIG-ENUM would refuse NIL for not being one of the
    ;; options.
    (:choice (and (config-string table name default)
                  (config-enum table name default (section-field-options field))))))

(defun section-enabled-p (table)
  "The presence rule every section shares: a present section is enabled
unless it says \"enabled\": false; an absent one is not vetoed."
  (config-boolean table "enabled" t))

(defun section-needs-setup-p (section config &aux (table (section-table config section.path)))
  "Whether SECTION, read from CONFIG, is neither vetoed nor complete."
  ;; A vetoed section ("enabled": false) needs nothing; a section with no
  ;; constraints is complete however absent.
  (and (or (null table) (section-enabled-p table))
       (not (null (section-problems section table)))))

(defun section-secret-pair (section)
  "The (ENV-FIELD . PATH-FIELD) of a ONE-OF group made of an :env and a
:path field — a secret taken by reference — or NIL."
  (dolist (group section.one-of)
    (let* ((fields (mapcar (lambda (name) (section-find-field section name)) group))
           (env (find :env fields :key #'section-field-type))
           (path (find :path fields :key #'section-field-type)))
      (when (and env path (= 2 (length fields)))
        (return (cons env path))))))

(defun secret-pair-noun (pair)
  "What a secret PAIR holds, from its two field names' common stem:
bot_token_env and bot_token_file hold the `bot token'."
  (let* ((a (section-field-name (car pair)))
         (b (section-field-name (cdr pair)))
         (stem (string-right-trim "_" (subseq a 0 (or (mismatch a b) (length a))))))
    (if (plusp (length stem)) (substitute #\Space #\_ stem) "secret")))

(defun section-needs (section table &aux (gaps (section-gaps section table)))
  "What SECTION still needs over TABLE, in the operator's words: `still
needed: ' and one entry per missing member — a required field's name, the
secret pair's noun, `a or b' for any other group — in SECTION-PROBLEMS'
order, or `complete'."
  ;; A problem of another kind — a member of the wrong shape, a one-of with
  ;; two set — has only the declaration's words, so any of those says every
  ;; problem in them. /settings says this over the section, and SECTION-SUMMARY
  ;; after the section's key.
  (cond ((null gaps) "complete")
        ((every #'cdr gaps) (format nil "still needed: ~{~a~^ · ~}" (mapcar #'cdr gaps)))
        (t (format nil "~{~a~^; ~}" (mapcar #'car gaps)))))

(defun section-summary (section table)
  "One line for the cells report and a refusal: the key, then what the
section still needs (SECTION-NEEDS) — the words /settings shows."
  (format nil "~a: ~a" (section-key section.path)
          (section-needs section table)))

(defun section-text (section)
  "The primer paragraph a model reads to configure SECTION: the key, one
line per field — its type, whether it is required, its default, its doc —
the constraints, then the guide."
  (with-output-to-string (out)
    (format out "~a:" (section-key section.path))
    (dolist (field section.fields)
      (format out "~%  ~a (~a~:[~;, required~]~@[, default ~a~])~@[: ~a~]"
              (section-field-name field)
              (ecase (section-field-type field)
                (:string "string")
                (:secret "secret, written inline")
                (:env "name of an environment variable")
                (:path "file path")
                (:directory "directory, ~/ expanded; a relative default is under the home")
                (:boolean "boolean")
                (:integer (if (section-field-min field)
                              (format nil "integer, at least ~a" (section-field-min field))
                              "integer"))
                (:list "list of strings")
                (:choice (format nil "one of ~{~a~^ | ~}" (section-field-options field))))
              (section-field-required field)
              (let ((default (section-field-default field)))
                (or (cdr (assoc default '((t . "true") (:false . "false"))))
                    (and default (princ-to-string default))))
              (let ((doc (section-field-doc field)))
                (and (plusp (length doc)) doc))))
    (format out "~{~%  exactly one of ~{~a~^ / ~}~}" section.one-of)
    (format out "~{~%  at least one of ~{~a~^ / ~} non-empty~}" section.any-of)
    (let ((guide section.guide))
      (when (plusp (length guide))
        (format out "~%~a" guide)))))
