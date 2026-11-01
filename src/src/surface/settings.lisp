;;;; settings.lisp --- every declared setting read and written from the web
;;;; page, and config.jsonc whole for the engineer. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The config describes itself once: each folder, the core's own readers
;;;; among them, declares its section (NLK:DEFINE-SECTION), and the model's
;;;; primer, the cells report and the values a folder runs on all read that
;;;; declaration. The page's Settings tab and the shell's /settings are two
;;;; more readers of it, over one route: every section with its fields and
;;;; what it still needs, a write that is CONFIG-SET -- the one writer, which
;;;; keeps comments and refuses a secret's sentinel -- once the section it
;;;; lands in still reads as its owner's start reads it, a secret taken by
;;;; reference pasted into a 0600 file the section then names, and a field's
;;;; CHOICES and a section's CHECK run here, in the organism. Nothing here
;;;; knows what a section is for; a cell's own route serves what only it knows.
;;;;
;;;; The raw document is the engineer's: config.jsonc as it stands with each
;;;; secret's value read as *REDACTED-SECRET*, and a save that puts every
;;;; secret still reading so back from the file, refuses a document that
;;;; does not parse or that a declared reader would now refuse, and writes
;;;; the operator's text as it came, comments and all.

(in-package #:nodecode.evolved)

(nlk:access (field nlk::section-field) (section nlk::section))

;;; --- the words a field is said in ------------------------------------------------

;;; The core never reads it; the folder that declared the field reads the file
;;; at its start.
(define-startup-parameter *secrets-directory* (nlk:home "secrets/")
  "Where a secret the operator pastes rather than names is kept: one 0600
file per field, referenced from the section by its <base>_file member.")

(defun json-true-p (value)
  "Whether a decoded boolean — a member's value or a declared default —
reads true: a declaration spells false :FALSE, which is not NIL."
  (and value (not (eq value :false))))

(defun section-field-value-text (field table &aux (name field.name) (default field.default))
  "A field row's words: the value as a row spells it, `unset', or the
default the field falls back to."
  (multiple-value-bind (value present) (nlk:section-value table name)
    (ecase field.type
      (:env (cond ((not present) "unset")
                  ((and (stringp value) (uiop:getenv value))
                   (format nil "~a (set)" value))
                  (t (format nil "~a (not set in this environment)" value))))
      (:boolean (cond (present (if (json-true-p value) "true" "false"))
                      (t (format nil "default ~a" (if (json-true-p default) "true" "false")))))
      (:list (let ((items (ignore-errors (nlk:config-string-list table name))))
               (cond ((null items) "none")
                     ((rest items) (format nil "~d entries" (length items)))
                     (t (first items)))))
      (:secret (if present "set" "unset"))
      ((:string :path :directory :integer :choice)
       (cond (present (princ-to-string value))
             (default (format nil "default ~a" default))
             (t "unset"))))))

(defun section-essential-p (section field table &aux (pair (nlk:section-secret-pair section)))
  "Whether SECTION cannot do without FIELD, over TABLE, so a page shows it
unfolded: a required field, a member of a ONE-OF or ANY-OF group -- the
secret pair's two only once one is set, the paste standing for both until
then."
  (cond ((nlk:section-field-required field) t)
        ((and pair (or (eq field (car pair)) (eq field (cdr pair))))
         (nlk:field-present-p table field))
        ;; A boolean on the wire, never MEMBER's tail: a reader asks for one.
        (t (and (some (lambda (group) (member field.name group :test #'string=))
                      (append section.one-of section.any-of))
                t))))

(defun section-secret-file (section)
  "The file a secret SECTION takes by reference is kept in once pasted: one
0600 file under *SECRETS-DIRECTORY*, named for the section and the pair's
path member, which then names it."
  (merge-pathnames (format nil "~a.~a" (nlk:section-key section.path)
                           (nlk:section-field-name (cdr (nlk:section-secret-pair section))))
                   *secrets-directory*))

(defun write-secret-file (file text)
  "TEXT into FILE, created 0600 under a 0700 directory; an existing file is
replaced, its mode kept at 0600."
  (nlk:write-file-atomically file (format nil "~a~%" text) :mode #o600 :directory-mode #o700))

;;; --- the sections ----------------------------------------------------------------

(defun section-applies (section &aux (record (nlk:find-cell section.owner)))
  "When a write to SECTION takes effect: :NOW for a section of the core's,
which the core reads at every ask; (values :CELL NAME) for one its
peripheral folder NAME reads when it starts; :RESTART for one a folder
declares that another folder's start reads, which a restart of the organism
is the start sure to reach."
  (cond ((null record) :now)
        ((eq record.kind :peripheral) (values :cell record.name))
        (t :restart)))

(defun section-provider-p (section)
  "Whether SECTION is a provider's: its owner resolves a provider's credential
(a hook on :CREDENTIAL under the owner's key, the key a cell's hooks carry)."
  (and (assoc section.owner (cdr (assoc :credential nlk:*hooks*)) :test #'equal) t))

(defun field-json (section field table &aux (name field.name)
                                            (secret (or (eq field.type :secret) (secret-key-p name))))
  "FIELD of SECTION over TABLE as the page draws it: its declaration,
whether it is set, the value -- never a secret's, which says only set or
unset -- and the words a row says it in."
  (multiple-value-bind (value present) (nlk:section-value table name)
    (json-object "name" name
                 "type" (string-downcase field.type)
                 "doc" field.doc
                 "set" (nlk:field-present-p table field)
                 "essential" (section-essential-p section field table)
                 "text" (cond ((not secret) (section-field-value-text field table))
                              (present "set")
                              (t "unset"))
                 :when secret "secret" t
                 :when (and present (not secret)) "value" value
                 :when field.default "default" (if (eq field.default :false) nil field.default)
                 :when field.options "options" (coerce field.options 'vector)
                 :opt "min" field.min
                 :when field.choices "choices" t)))

(defun section-json (section config &aux (table (nlk:section-table config section.path))
                                         (pair (nlk:section-secret-pair section))
                                         (record (nlk:find-cell section.owner)))
  "SECTION over CONFIG as the page draws it: its key, its guide, what it
still needs, when a write to it applies, why its cell refused to start if
it did, whether it is a provider's, where a pasted secret goes, and every
field."
  (multiple-value-bind (applies cell) (section-applies section)
    (json-object "key" (nlk:section-key section.path)
                 "path" (coerce section.path 'vector)
                 "guide" section.guide
                 "present" (and table t)
                 "enabled" (nlk:section-enabled-p table)
                 "needs" (nlk:section-needs section table)
                 "check" (and section.check t)
                 "applies" (string-downcase applies)
                 :opt "cell" cell
                 :opt "provider" (section-provider-p section)
                 :opt "start_refusal" (and record record.refusal)
                 :when pair "secret" (json-object "noun" (nlk:secret-pair-noun pair)
                                                  "env" (nlk:section-field-name (car pair))
                                                  "file" (nlk:section-field-name (cdr pair))
                                                  "to" (namestring (section-secret-file section)))
                 "fields" (map 'vector (lambda (field) (field-json section field table))
                               section.fields))))

(defun settings-json (&optional said)
  "Every declared section as the config reads right now, the file it reads,
why it does not read if it does not, and SAID, what a write just did."
  (multiple-value-bind (config error) (read-shared-config)
    (json-object :opt "file" (and *shared-config-path* (namestring *shared-config-path*))
                 :opt "error" error
                 :opt "said" said
                 "sections" (map 'vector (lambda (section) (section-json section config))
                                 (nlk:sections)))))

(defun request-path (request &aux (path (coerce (json-value request :array "path") 'list)))
  "REQUEST's `path', the member keys from the config's root, refused unless
it is a non-empty array of non-empty strings."
  (require-non-empty (or path '(nil)) "path must be a non-empty array of member keys"
                     :required t)
  path)

(defun request-section (request &aux (path (request-path request)))
  "The declared section REQUEST's `path' names, refused by name when none is."
  (or (nlk:find-section path)
      (nlk::protocol-refuse :unknown_section "no section ~a is declared" (nlk:section-key path))))

(defun keep-section-secret (section text &aux (pair (nlk:section-secret-pair section))
                                              (file (section-secret-file section)))
  "TEXT, the secret SECTION takes by reference, kept in its 0600 file under
*SECRETS-DIRECTORY* and named by the pair's path member: answers what
CONFIG-SET did and where it went."
  (require-non-empty (list (nlk:trimmed text)) "the secret is empty" :required t)
  (write-secret-file file (nlk:trimmed text))
  (format nil "~a; the ~a is in ~a, mode 0600"
          (config-set (append section.path (list (nlk:section-field-name (cdr pair))))
                      (namestring file))
          (nlk:secret-pair-noun pair) (namestring file)))

(defun written-falses (value)
  "VALUE as the request's JSON decoded it, every false in it -- NIL, which
CONFIG-SET refuses as ambiguous -- spelled :FALSE, at any depth."
  (typecase value
    (null :false)
    (hash-table (alexandria:copy-hash-table value :key #'written-falses))
    ((and vector (not string)) (map 'vector #'written-falses value))
    (t value)))

(defun section-refusal (section config)
  "What SECTION's reader refuses of CONFIG -- a member of the wrong shape, or
what its owner refuses at its start (NLK:SECTION-SETTINGS) -- in the
refusal's own words, or NIL."
  (handler-case (progn (nlk:section-settings section (nlk:section-table config section.path)) nil)
    (nlk:config-refusal (condition) (nlk:config-refusal-detail condition))))

;;; SECTION is the declared section PATH lies in: the one at PATH, else the nearest
;;; that holds it; NIL outside every section.
(defun refuse-section-write (path value
                             &aux (section (loop for end from (length path) downto 1
                                                 thereis (nlk:find-section (subseq path 0 end)))))
  "Refuse writing VALUE at PATH when the section it lands in would read
refused where it does not now -- a value its cell would refuse at its next
start -- in the section's own words, naming the member the refusal is about:
the one an object VALUE carries whose write alone is refused, else the
member PATH names. Nothing is written."
  ;; The raw editor's rule, over one section: a refusal the file already has
  ;; is not this write's, so a section broken by hand can still be mended a
  ;; member at a time.
  (when (and section *shared-config-path*)
    (let ((old (or (nlk:read-text *shared-config-path*) ""))
          (was (section-refusal section (read-shared-config))))
      (flet ((refused (path value &aux (now (section-refusal section (config-document
                                                                      (nlk:set-jsonc-path old path value)))))
               (and now (not (equal now was)) now)))
        (when-let (refusal (refused path value))
          (error 'nlk:protocol-error
                 :code :refused :message (format nil "nothing was saved: ~a" refusal)
                 :field (if (hash-table-p value)
                            (loop for key being the hash-keys of value using (hash-value member)
                                  when (refused (append path (list key)) member) return key)
                            (nth (length section.path) path))))))))

(define-gateway-route handle-gateway-config-http (env)
  "GET /api/gateway/config — every declared section with each field's
value, a secret only as set or unset (SETTINGS-JSON). POST {path, value}
writes the member at PATH through CONFIG-SET (an object merges), {path,
remove: true} takes it out, {path, secret} keeps the secret the section at
PATH takes by reference in its 0600 file and names the file; each answers
the listing, with what the write did as `said'. A value or a removal the
section's own reader would refuse is refused first (REFUSE-SECTION-WRITE),
400 `refused' naming the member as `field'."
  (:route nil "/api/gateway/config")
  (settings-json
   (when (eq (getf env :request-method) :post)
     (let* ((request (gateway-request-json env))
            (path (request-path request)))
       (multiple-value-bind (value present) (gethash "value" request)
         (cond ((stringp (gethash "secret" request))
                (let ((section (request-section request)))
                  (require-request (nlk:section-secret-pair section)
                                   "~a takes no secret by reference" (nlk:section-key path))
                  (keep-section-secret section (gethash "secret" request))))
               ((eq t (gethash "remove" request))
                (refuse-section-write path :remove)
                (config-set path :remove))
               ;; A false is a member's own, or one inside a section's form.
               (present (let ((value (written-falses value)))
                          (refuse-section-write path value)
                          (config-set path value)))
               (t (nlk::protocol-refuse :invalid_request
                                        "a write carries value, remove: true, or secret"))))))))

(define-gateway-route handle-gateway-config-probe-http (env)
  "POST /api/gateway/config/probe {path, field?} — what the section at PATH
declares it can ask, over the section as config.jsonc says it now: with
FIELD, that field's CHOICES as {choices: [{id, label}]}, beside `wait' when
there is nothing to pick from until the operator acts on the platform (the
text says what to do) and `default', the ids an unset field takes when the
operator just presses Enter; without, the section's CHECK as {text}. A
probe's refusal answers 400 in its own words."
  (:route :post "/api/gateway/config/probe" "probe")
  (let* ((request (gateway-request-json env))
         (section (request-section request))
         (name (json-value request :text "field"))
         (field (and name (nlk::section-find-field section name)))
         (table (nlk:section-table (read-shared-config) section.path)))
    (require-request (or (not name) (and field field.choices)) "~a.~a offers no choices"
                     (nlk:section-key section.path) name)
    (require-request (or name section.check) "~a declares no check" (nlk:section-key section.path))
    (handler-case
        (if field
            (multiple-value-bind (choices offer) (funcall field.choices table)
              (json-object "choices" (map 'vector (lambda (choice)
                                                    (json-object "id" (car choice) "label" (cdr choice)))
                                          choices)
                           :opt "wait" (getf offer :wait)
                           :opt "default" (and (getf offer :default) (coerce (getf offer :default) 'vector))))
            (json-object "text" (princ-to-string (funcall section.check table))))
      (nlk:config-refusal (condition)
        (nlk::protocol-refuse :refused "~a" (nlk:config-refusal-detail condition))))))

;;; --- the document whole ------------------------------------------------------------

(defun secret-spans (text)
  "Each string of the JSONC document TEXT that holds a secret (SECRET-PATH-P:
its own name, or for a list's element the list's) and is not empty, as (PATH
START END) -- what CONFIG-GET reads as *REDACTED-SECRET*."
  (remove-if-not (lambda (leaf)
                   (and (secret-path-p (reverse (first leaf)))
                        (> (- (third leaf) (second leaf)) 2)))
                 (nlk::jsonc-string-leaves text)))

(defun splice-spans (text spans)
  "TEXT with each (START END NEW) of SPANS, ascending and apart, standing
in place of what lay between START and END."
  (with-output-to-string (out)
    (let ((cursor 0))
      (loop for (start end new) in spans
            do (write-string text out :start cursor :end start)
               (write-string new out)
               (setf cursor end))
      (write-string text out :start cursor))))

(defun config-document (text)
  "TEXT, a JSONC document, decoded; refused in the reader's words when it is
not one, or its root is not an object."
  (let ((value (handler-case (nlk:decode-json (nlk::normalize-jsonc text))
                 (error (condition)
                   (nlk::protocol-refuse :invalid_config "config.jsonc would not read: ~a" condition)))))
    (unless (hash-table-p value)
      (nlk::protocol-refuse :invalid_config "config.jsonc would not read: its root must be an object"))
    value))

(defun config-refusals (config &aux (general (json-value config :object "general")))
  "What the config's declared readers would refuse of CONFIG, as texts: a
section's member of the wrong shape (NLK:SECTION-SETTINGS), a model pin
that is not one (CONFIG-MODEL-PIN)."
  (append (loop for section in (nlk:sections)
                for why = (and (nlk:section-table config section.path)
                               (section-refusal section config))
                when why collect (format nil "~a: ~a" (nlk:section-key section.path) why))
          (loop for key in '("default_model" "auxiliary_model" "decider_model")
                when (handler-case (progn (config-model-pin general key) nil)
                       (nlk:config-refusal (condition) (nlk:config-refusal-detail condition)))
                  collect it)))

(defun config-text-json (text &optional said)
  "The raw editor's read of TEXT, config.jsonc as it stands: the document
with each secret read as *REDACTED-SECRET*, the stamp a save must bring
back, and SAID."
  (json-object "file" (namestring *shared-config-path*)
               "text" (splice-spans text (loop for (nil start end) in (secret-spans text)
                                               collect (list start end (nlk::json-string-literal
                                                                        *redacted-secret*))))
               "stamp" (nlk::sha256-text text)
               :opt "said" said))

(defun restore-secrets (text old &aux (kept (secret-spans old)) (spans '())
                                       (sentinel (nlk::json-string-literal *redacted-secret*)))
  "TEXT, an edit of OLD as the raw editor read it, with each secret still
reading *REDACTED-SECRET* given OLD's own value at the same path back:
(values TEXT COUNT). A sentinel where OLD holds no secret is refused."
  (loop for (path start end) in (secret-spans text)
        when (string= sentinel text :start2 start :end2 end)
          do (let ((was (or (find path kept :key #'first :test #'equal)
                            (nlk::protocol-refuse
                             :invalid_config "~{~a~^.~} reads ~a, and config.jsonc holds no ~
                                              secret there to keep: write the secret itself, ~
                                              or take the member out"
                             path sentinel))))
               (push (list start end (subseq old (second was) (third was))) spans)))
  (let ((spans (nreverse spans)))
    (values (splice-spans text spans) (length spans))))

(defun save-config-text (text stamp &aux (file *shared-config-path*))
  "Write TEXT, the raw editor's document, as config.jsonc once the file
still reads as it did at STAMP: its secrets put back (RESTORE-SECRETS), the
document read, and nothing a declared reader would refuse that it did not
already refuse in the file. Every other byte is the operator's, comments
included. Answers the editor's read of what was written."
  (unless file
    (error "the shared config is not writable in this process (no config path)"))
  (bt2:with-lock-held (*shared-config-write-lock*)
    (let ((old (or (nlk:read-text file) "")))
      (unless (equal stamp (nlk::sha256-text old))
        (nlk::protocol-refuse :changed "config.jsonc changed since it was read: reload it and edit again"))
      ;; Read first: a document that does not read is refused in the reader's
      ;; words, before anything walks its members.
      (config-document text)
      (multiple-value-bind (text secrets) (restore-secrets text old)
        (when-let (refused (set-difference (config-refusals (config-document text))
                                           (ignore-errors (config-refusals (config-document old)))
                                           :test #'string=))
          (nlk::protocol-refuse :invalid_config "nothing was saved: ~{~a~^; ~}" refused))
        (nlk:write-file-atomically file text :mode :private)
        ;; The memo keys on size and mtime, second-granular.
        (setf *shared-config-memo* nil)
        (config-text-json text (format nil "config.jsonc saved~[~:;, ~:*~d secret~:p kept from the file~]"
                                       secrets))))))

(define-gateway-route handle-gateway-config-raw-http (env)
  "GET /api/gateway/config/raw — config.jsonc as it stands, each secret's
value read as the redaction sentinel, with the stamp a save brings back.
POST {text, stamp} saves the document whole (SAVE-CONFIG-TEXT) and answers
the same read of what was written."
  (:route nil "/api/gateway/config/raw")
  (unless *shared-config-path*
    (nlk::protocol-refuse :refused "this process reads no config.jsonc"))
  (if (eq (getf env :request-method) :post)
      (let ((request (gateway-request-json env)))
        (save-config-text (or (json-value request :string "text")
                              (nlk::protocol-refuse :invalid_request "text must be a string"))
                          (json-value request :string "stamp")))
      (config-text-json (or (nlk:read-text *shared-config-path*) ""))))
