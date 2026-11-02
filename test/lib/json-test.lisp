;;;; json-test.lisp --- the decoded-wire-value waist.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The point of these tests is the decoding table, not the plumbing. shasht's
;;;; JSON null is the truthy keyword :NULL and its JSON false is NIL; every
;;;; reader in the image used to re-derive that inline, and one of them got it
;;;; wrong in a way that crashed a live turn. These cases pin the table once.

(in-package #:nodecode.test)

(deftest json-value-coerces-by-declared-type ()
  (let ((object (nlk:decode-json "{\"s\":\"hi\",\"n\":7,\"f\":1.5,\"b\":true,
                                 \"o\":{\"k\":1},\"a\":[1,2],\"e\":\"\"}")))
    (is-shape object ((:string "s") "hi" "string reads back")
      ((:text "s") "hi" "non-empty string reads back")
      ((:text "e") null "empty string is not :text") ((:string "e") "" "empty string is a :string")
      ((:integer "n") eql 7 "integer reads back") ((:number "n") eql 7 "an integer is a :number")
      ((:integer "f") null "a float is not an :integer")
      ((:number "f") eql 1.5 "float reads back as :number")
      ((:boolean "b") eq t "JSON true is :boolean T") ((:object "o") is "object reads back")
      ((:array "a") is "array reads back") ((:array "s") null) ((:string "n") null))))

(deftest json-value-folds-shasht-null-and-false ()
  (let ((object (nlk:decode-json "{\"null\":null,\"false\":false,\"true\":true}")))
    (is-shape object ((:any "null") null) ((:string "null") null "JSON null is not a string")
      ((:object "null") null "JSON null is not an object")
      ((:boolean "false") null "JSON false is :boolean NIL")
      ((:boolean "null") null "JSON null is :boolean NIL")
      ((:boolean "absent") null "an absent key is :boolean NIL")
      ((:boolean "true") eq t "only JSON true is :boolean T"))))

(deftest json-value-walks-paths-and-stops-safely ()
  (let ((object (nlk:decode-json "{\"m\":{\"s\":{\"id\":3}},\"flat\":\"x\"}")))
    (is-shape object ((:integer "m" "s" "id") eql 3 "a full path reads the leaf")
      ((:integer "m" "missing" "id") null) ((:integer "flat" "id") null))
    (is (null (nlk:json-value nil :string "m" "s")) "a NIL source reads NIL")
    (is (null (nlk:json-value "not-an-object" :string "m")) "a scalar source reads NIL")))

(deftest json-value-coerces-a-value-already-in-hand ()
  (is (equal "x" (nlk:json-value "x" :string)) "no path coerces the argument itself")
  (is (null (nlk:json-value :null :any)) "a bare :NULL folds to NIL")
  (is (null (nlk:json-value 3 :string)) "a wrong-typed argument reads NIL"))

(deftest json-value-reports-key-presence ()
  (let ((object (nlk:decode-json "{\"null\":null,\"false\":false}")))
    (is (nth-value 1 (nlk:json-value object :any "null")))
    (is (nth-value 1 (nlk:json-value object :boolean "false")))
    (is (null (nth-value 1 (nlk:json-value object :any "absent"))))
    (is (null (nth-value 1 (nlk:json-value object :any "null" "deeper"))))))

(deftest json-value-refuses-an-unknown-type ()
  (is (signals-error error (nlk:json-value (nlk:decode-json "{}") :symbol "k"))))

(deftest with-json-binds-sequentially-and-evaluates-source-once ()
  (let ((calls 0)
        (object (nlk:decode-json "{\"a\":\"one\",\"n\":2,\"o\":{\"b\":true}}")))
    (flet ((source () (incf calls) object))
      (nlk:with-json ((a :string "a")
                      (n :integer "n")
                      (nested :boolean "o" "b")
                      (missing :string "gone" :default "fallback")
                      (derived :string "a" :default a))
          (source)
        (is (equal "one" a) "a string field binds")
        (is (eql 2 n) "an integer field binds")
        (is (eq t nested) "a path field binds")
        (is (equal "fallback" missing) ":DEFAULT substitutes on a NIL read")
        (is (equal "one" derived) "a later :DEFAULT may name an earlier binding")))
    (is (eql 1 calls) "the source form is evaluated exactly once")))

(deftest with-json-defaults-do-not-mask-a-present-empty-string ()
  (nlk:with-json ((as-string :string "cmd" :default "fallback")
                  (as-text :text "cmd" :default "fallback"))
      (nlk:decode-json "{\"cmd\":\"\"}")
    (is (equal "" as-string) ":STRING keeps a present empty string")
    (is (equal "fallback" as-text) ":TEXT treats an empty string as absent")))

(deftest with-json-interleaves-wire-reads-and-ordinary-bindings ()
  ;; The shape the call sites have: a read, a value derived from it, another
  ;; read. A LET* that could not hold both would force every cluster to split.
  (let ((object (nlk:decode-json "{\"status\":\"running\",\"n\":3}")))
    (nlk:with-json ((status :string "status")
                    (live-p (and status (string= status "running")))
                    (n      :integer "n")
                    (doubled (* 2 n))
                    (blank)
                    bare)
        object
      (is (equal "running" status) "a wire read binds")
      (is live-p "an ordinary binding may name the read before it")
      (is (eql 6 doubled) "ordinary bindings interleave with later reads")
      (is (null blank) "(VAR) binds NIL")
      (is (null bare) "a bare symbol binds NIL")))
  ;; A wire type keyword with no key after it cannot be a read, so it must not
  ;; be silently taken as an ordinary binding of that keyword.
  (is (signals-error error (macroexpand-1 '(nlk:with-json ((v :integer)) nil nil)))))

(deftest with-json-rejects-malformed-specs-at-macroexpansion ()
  (is-table (spec) (signals-error error (macroexpand-1 '(nlk:with-json (spec) nil nil)))
    ((v :nope "k") "a three-element spec whose middle is not a wire type is a compile-time error")
    ((v :string key) "a non-literal key is a compile-time error")
    ((v :boolean "k" :default t) ":DEFAULT on :BOOLEAN could never answer false, so it is refused")
    ((v :string "k" :default) ":DEFAULT without a form is a compile-time error")))

;;; --- the encode side ---------------------------------------------------

(deftest json-object-builds-literal-key-objects ()
  (let ((object (nlk:json-object "s" "hi" "n" 7 "nested" (nlk:json-object))))
    (is-shape object (hash-table-p is "an object is an EQUAL hash table")
      (hash-table-test eq 'equal "keyed by EQUAL, the wire test") ("s" "hi" "a pair sets its key")
      ("n" eql 7 "pairs accumulate"))
    (is (zerop (hash-table-count (gethash "nested" object)))))
  (let ((round (shasht:read-json
                (nlk:encode-json-object
                 (nlk:json-object "t" t "f" :false "null" :null)))))
    (is (eq t (nlk:json-value round :boolean "t")) "T writes JSON true")
    (is (null (nlk:json-value round :any "f")) "shasht :FALSE writes JSON false")
    (is (nth-value 1 (nlk:json-value round :any "null")) ":NULL writes JSON null, key present")))

(deftest json-object-opt-folds-nil-to-key-absence ()
  (let ((object (nlk:json-object "always" 1
                                 :opt "present" "x"
                                 :opt "absent" nil)))
    (is (equal "x" (gethash "present" object)) ":OPT with a value sets the key")
    (is (null (nth-value 1 (gethash "absent" object))))))

(deftest json-object-when-writes-an-explicit-false ()
  ;; The navigate_checkpoint restore_files shape: the key rides only when the
  ;; condition holds, and the deliberate value is NIL (JSON false on the wire).
  (let ((object (nlk:json-object :when t "sent" nil
                                 :when nil "skipped" 9)))
    (multiple-value-bind (value present) (gethash "sent" object)
      (is (and present (null value)) ":WHEN true writes the value, even NIL"))
    (is (null (nth-value 1 (gethash "skipped" object))))))

(deftest json-object-evaluates-specs-in-order-and-once (let ((order '())))
  (flet ((tap (tag value) (push tag order) value))
    (nlk:json-object "a" (tap :a 1)
                     :opt "b" (tap :b nil)
                     :when (tap :test t) "c" (tap :c 2))
    (is (equal '(:a :b :test :c) (reverse order)))))

(deftest json-object-rejects-malformed-specs-at-macroexpansion ()
  (is-table (specs) (signals-error error (macroexpand-1 (cons 'nlk:json-object 'specs)))
    (("key") "a key without a value form is a compile-time error")
    ((key 1) "a computed key is a compile-time error; those go through MAKE-JSON-OBJECT")
    ((:opt key 1) ":OPT requires a literal string key")
    ((:when t key 1) ":WHEN requires a literal string key")))

;;; --- the JSONC dialect ------------------------------------------------------
;;; NORMALIZE-JSONC is the read path for the operator config and for every theme
;;; document, and it carried no tests at all. These pin its behaviour so the
;;; string/escape/comment primitives can be lifted out of it for the edit side
;;; below without the lift being taken on faith.

(defun jsonc-decoded (text)
  (shasht:read-json (nlk::normalize-jsonc text)))

(deftest normalize-jsonc-strips-both-comment-forms ()
  (is (equal "{\"a\": 1} " (nlk::normalize-jsonc "{\"a\": 1} // trailing")))
  (is (equal "
{\"a\": 1}" (nlk::normalize-jsonc "// leading
{\"a\": 1}")))
  (is (equal "{\"a\":  1}" (nlk::normalize-jsonc "{\"a\": /* mid */ 1}")))
  (is (eql 1 (gethash "a" (jsonc-decoded "{
  // } a close brace inside a line comment
  /* { and an open one inside a block */
  \"a\": 1
}")))))

(deftest normalize-jsonc-keeps-comment-syntax-inside-strings ()
  (let ((object (jsonc-decoded "{\"u\": \"https://example.com/a\", \"b\": \"/* not */\"}")))
    (is-shape object ("u" "https://example.com/a") ("b" "/* not */")))
  (is (equal "}{" (gethash "n" (jsonc-decoded "{\"n\": \"}{\"}"))))
  (is (equal "he said \"x\" and a trailing \\"
             (gethash "n" (jsonc-decoded "{\"n\": \"he said \\\"x\\\" and a trailing \\\\\"}")))))

(deftest normalize-jsonc-drops-trailing-commas ()
  (is (equal 1 (hash-table-count (jsonc-decoded "{\"a\": 1,}"))))
  (is (equalp #(1 2) (jsonc-decoded "[1, 2,]")))
  (is (equal 1 (hash-table-count (jsonc-decoded "{\"a\": 1, // why
}"))))
  (is (equal 2 (hash-table-count (jsonc-decoded "{\"a\": 1, /* keep */ \"b\": 2}")))))

(deftest normalize-jsonc-counts-page-as-whitespace ()
  ;; Asserted through the held comma rather than through shasht, which does not
  ;; accept #\Page as whitespace itself: the comma only goes if the #\Page that
  ;; followed it was routed into the gap.
  (is (equal (format nil "{\"a\": 1~c}" #\Page)
             (nlk::normalize-jsonc (format nil "{\"a\": 1,~c}" #\Page)))))

(deftest normalize-jsonc-refuses-an-unterminated-block-comment ()
  (is (signals-error error (nlk::normalize-jsonc "{ /* never closed"))))

;;; --- the JSONC edit side ----------------------------------------------------
;;; The writer these replace located an object's end with (POSITION #\}), knew
;;; nothing about comments, and could only overwrite a key that already existed
;;; -- silently doing nothing otherwise. Each case below is one of those, and
;;; then the path writer's own: a nested path, a value of any JSON type, an
;;; object that merges instead of replacing, and the diff it accounts for.

(defun jsonc-lines (&rest lines)
  (format nil "~{~a~^~%~}" lines))

(defun general-document (&rest members)
  "A JSONC document of one general section holding MEMBERS, one line each."
  (apply #'jsonc-lines "{" "  \"general\": {" (append members (list "  }" "}"))))

(defun jsonc-set (source path value)
  "The written text alone."
  (values (nlk:set-jsonc-path source path value)))

(defmacro is-jsonc-edit ((source path value) (from to) description &body body)
  "Write VALUE at PATH in SOURCE and assert the text it wrote is SOURCE with
its first FROM replaced by TO — the expected document spelled as the fixture
plus one substitution, so it cannot drift from the fixture — under
DESCRIPTION; then BODY with TEXT, ADDED, REPLACED and REMOVED bound to the
write's four values."
  (let ((document (gensym "SOURCE")))
    `(let ((,document ,source))
       (multiple-value-bind (text added replaced removed)
           (nlk:set-jsonc-path ,document ,path ,value)
         (declare (ignorable added replaced removed))
         (is (equal (replace-once ,document ,from ,to) text) ,description)
         ,@body))))

(deftest jsonc-edit-replaces-past-a-nested-object ()
  (let ((source (general-document "    \"context_window_policy\": { \"evict_trigger_pct\": 85 },"
                                  "    \"default_model\": \"old-model\","
                                  "    \"theme\": \"Tungstene\"")))
    (is-jsonc-edit (source '("general") '(:default_model "new-model"))
        ("\"old-model\"" "\"new-model\"")
        "the nested policy object does not truncate the search window"
        (is (null added) "nothing was added")
        (is (equal '("general.default_model") replaced) "the one member was replaced"))))

(deftest jsonc-edit-inserts-in-the-sections-own-style ()
  (is-jsonc-edit ((general-document "    \"theme\": \"T\"") '("general") '(:default_model "m1"))
      ("\"T\"" (jsonc-lines "\"T\"," "    \"default_model\": \"m1\""))
      "the new member takes the last member's indent"
      (is (equal '("general.default_model") added) "an absent key is inserted, not silently skipped")
      (is (null replaced)))
  (is-jsonc-edit ((general-document "    \"theme\": \"T\",") '("general") '(:default_model "m1"))
      ("\"T\"," (jsonc-lines "\"T\"," "    \"default_model\": \"m1\","))
      "a section already carrying a trailing comma keeps exactly one")
  (is-jsonc-edit ("{ \"general\": { \"theme\": \"T\" } }" '("general") '(:default_model "m1"))
      ("\"T\"" "\"T\", \"default_model\": \"m1\"")
      "a one-line section stays on one line"))

(deftest jsonc-edit-creates-the-missing-section ()
  (is-jsonc-edit ((jsonc-lines "{" "  \"providers\": {"
                               "    \"p\": { \"sdk\": \"openai-responses\" }" "  }" "}")
                  '("general") '(:default_provider "p" :default_model "m1"))
      ("  }" (jsonc-lines "  }," "  \"general\": {" "    \"default_provider\": \"p\","
                          "    \"default_model\": \"m1\"" "  }"))
      "an absent section is built at the root's own member indent")
  (is (equal (general-document "    \"default_model\": \"m1\"")
             (jsonc-set (jsonc-lines "{" "}") '("general") '(:default_model "m1"))))
  (is (equal "{ \"general\": { \"default_model\": \"m1\" } }"
             (jsonc-set "{}" '("general") '(:default_model "m1")))))

(deftest jsonc-edit-fills-an-empty-section ()
  (is-jsonc-edit ((jsonc-lines "{" "  \"general\": {}" "}") '("general") '(:default_model "m1"))
      ("{}" (jsonc-lines "{" "    \"default_model\": \"m1\"" "  }"))
      "an empty section expands to the document's own style")
  (is-jsonc-edit ((jsonc-lines "{" "  \"general\": { /* keep */ }" "}")
                  '("general") '(:default_model "m1"))
      ("{ /* keep */ }" (jsonc-lines "{ /* keep */" "    \"default_model\": \"m1\"" "  }"))
      "a comment living between the braces survives the fill"))

(deftest jsonc-edit-creates-a-fresh-document ()
  (let ((expected (jsonc-lines "{" "  \"general\": {"
                               "    \"default_provider\": \"p\","
                               "    \"default_model\": \"m1\"" "  }" "}" "")))
    (dolist (empty (list "" "   " (format nil "  ~%  ") (format nil "// nothing here~%")))
      (is-values (text added)
          (nlk:set-jsonc-path empty '("general")
                              '(:default_provider "p" :default_model "m1"))
        (text expected) (added '("general.default_provider" "general.default_model"))))
    (is (equal "m1" (nlk:json-value (jsonc-decoded expected) :string
                                    "general" "default_model")))))

(deftest jsonc-edit-ignores-braces-in-comments-and-strings ()
  (is-jsonc-edit ((jsonc-lines
                   "{"
                   "  // } this line comment holds a close brace"
                   "  \"general\": {"
                   "    /* } and { inside a block comment */"
                   "    \"note\": \"}{\","
                   "    \"theme\": \"T\", // a trailing } comment"
                   "    \"default_model\": \"old\""
                   "  }"
                   "}")
                  '("general" "default_model") "new")
      ("\"old\"" "\"new\"")
      "every comment and every braced string survives byte for byte"))

(deftest jsonc-edit-survives-escaped-quotes ()
  ;; The trailing backslash immediately before a closing quote is the killer: a
  ;; scanner reading \\" as an escaped quote desynchronizes and eats the rest.
  (is-jsonc-edit ((general-document "    \"note\": \"he said \\\"}\\\" and a trailing \\\\\","
                                    "    \"default_model\": \"old\"")
                  '("general" "default_model") "new")
      ("\"old\"" "\"new\"")
      "the escaped sibling is untouched and the target is replaced")
  (let ((text (jsonc-set (general-document "    \"default_model\": \"old\"")
                         '("general" "default_model") "a\"b\\c")))
    (is (search "\"a\\\"b\\\\c\"" text) "the written value is escaped on the way out")
    (is (equal "a\"b\\c"
               (nlk:json-value (jsonc-decoded text) :string "general" "default_model")))))

(deftest jsonc-edit-is-idempotent ()
  (let ((source (general-document "    \"default_model\": \"m1\"")))
    (is-values (text added replaced) (nlk:set-jsonc-path source '("general") '(:default_model "m1"))
      ((and (null added) (null replaced)) is "a document that already says it did not change")
      (text eq source "and the very same string comes back, so no file moves"))
    (is-jsonc-edit (source '("general") '(:default_model "m1" :default_provider "p"))
        ("\"m1\"" (jsonc-lines "\"m1\"," "    \"default_provider\": \"p\""))
        "only the absent member is written"
        (is (equal '("general.default_provider") added) "one satisfied key and one absent one is one addition")
        (is (null replaced)))))

(deftest jsonc-edit-refuses-rather-than-guesses ()
  (dolist (case (list "[1, 2]"
                      "{ \"general\": { \"default_model\": \"x\" "
                      "{ \"general\": { \"k\": \"unterminated "
                      "{ /* never closed"))
    (is (signals-error nlk:jsonc-refusal
          (jsonc-set case '("general") '(:default_model "m")))
        (format nil "~s is refused rather than spliced into" case)))
  (is (signals-error nlk:jsonc-refusal
        (jsonc-set "{ \"general\": { \"theme\": \"T\" } }" '("general" "theme" "deeper") "x")))
  (is (equal "{ \"general\": { \"default_model\": \"m\" } }"
             (jsonc-set "{ \"general\": 5 }" '("general") '(:default_model "m"))))
  (is-table (path value) (signals-error nlk:jsonc-refusal (jsonc-set "{}" path value))
    ('("a") nil "NIL is refused: it is false, null and the empty array at once")
    ('() "x" "an empty path names nothing")
    ('("a") (make-instance 'standard-object) "a value that is not JSON is refused")))

(deftest jsonc-edit-agrees-with-the-reader ()
  ;; The cross-check that the write side and NORMALIZE-JSONC really do share one
  ;; scanner: whatever the splice did, the document still decodes, the target
  ;; moved, and nothing else did.
  (dolist (source (list (jsonc-lines "{" "  \"general\": {"
                                     "    \"context_window_policy\": { \"evict_trigger_pct\": 85 },"
                                     "    \"default_model\": \"old\"" "  },"
                                     "  \"providers\": { \"p\": {} }" "}")
                        (jsonc-lines "{" "  // lead } comment"
                                     "  \"providers\": { \"p\": {} }," "}")
                        "{ \"providers\": { \"p\": {} } }"))
    (let ((after (jsonc-decoded (jsonc-set source '("general" "default_model") "new"))))
      (is (equal "new" (nlk:json-value after :string "general" "default_model")))
      (is-present (providers (nlk:json-value after :object "providers"))
          "the untouched section is still there"
          (is (equalp (nlk:json-value (jsonc-decoded source) :object "providers") providers))))))

;;; --- the path writer's own cases --------------------------------------------

(deftest jsonc-path-write-creates-every-missing-object-along-the-path ()
  (let ((source (jsonc-lines "{" "  // the operator's file"
                             "  \"general\": { \"theme\": \"T\" }" "}")))
    (is-jsonc-edit (source '("channels" "discord")
                           '(:bot_token_env "DISCORD_BOT_TOKEN" :require_mention t))
        ("  \"general\": { \"theme\": \"T\" }"
         (jsonc-lines "  \"general\": { \"theme\": \"T\" },"
                      "  \"channels\": {"
                      "    \"discord\": {"
                      "      \"bot_token_env\": \"DISCORD_BOT_TOKEN\","
                      "      \"require_mention\": true"
                      "    }"
                      "  }"))
        "the nest is built one indent step per level, the comment kept"
        (is (equal '("channels.discord.bot_token_env" "channels.discord.require_mention") added))
        (is (null replaced))
        (is (eq t (nlk:json-value (jsonc-decoded text) :any "channels" "discord" "require_mention"))))))

(deftest jsonc-path-write-merges-an-object-and-replaces-a-scalar ()
  (let ((source (jsonc-lines "{"
                             "  \"channels\": {"
                             "    \"discord\": {"
                             "      \"bot_token_env\": \"OLD\", // the reference, never the value"
                             "      \"allowed_users\": [\"1\"],"
                             "      \"require_mention\": true"
                             "    }"
                             "  }"
                             "}")))
    (is-values (text added replaced)
        (nlk:set-jsonc-path source '("channels" "discord")
                            '(:bot_token_env "DISCORD_BOT_TOKEN"
                              :allowed_channels ("42" "43")
                              :require_mention t))
      (added '("channels.discord.allowed_channels")) (replaced '("channels.discord.bot_token_env"))
      ((search "\"bot_token_env\": \"DISCORD_BOT_TOKEN\", // the reference, never the value" text) is)
      ((search "\"allowed_users\": [\"1\"]," text) is)
      ((search "\"allowed_channels\": [\"42\", \"43\"]" text) is)
      ((hash-table-count (nlk:json-value (jsonc-decoded text) :object "channels" "discord")) 4))
    (is-values (text added replaced)
        (nlk:set-jsonc-path source '("channels" "discord" "require_mention") :false)
      (added null) (replaced '("channels.discord.require_mention"))
      ((search "\"require_mention\": false" text) is ":false writes false"))
    (nlk:bind (((text _ replaced)
                (nlk:set-jsonc-path source '("channels" "discord" "allowed_users") #("1" "2"))))
      (is (equal '("channels.discord.allowed_users") replaced))
      (is (search "\"allowed_users\": [\"1\", \"2\"]," text)))))

(deftest jsonc-path-write-accepts-every-json-type ()
  (let ((text (jsonc-set "{}" '("t") '(:s "x" :n 7 :f 1.5 :yes t :no :false :nothing :null
                                       :list (1 2) :object (:inner "v")))))
    (let ((object (nlk:json-value (jsonc-decoded text) :object "t")))
      (is-shape object ("s" "x") ("n" eql 7) ("f" = 1.5) ("yes" eq t))
      (is (and (null (gethash "no" object)) (nth-value 1 (gethash "no" object))) "false is present and false")
      (is-shape object ("nothing" eq :null) ("list" equalp #(1 2))
        ((:string "object" "inner") "v"))))
  (is (equal "{ \"a\": { \"b\": 3 } }" (jsonc-set "{}" '("a" "b") 3))))

(deftest jsonc-path-write-deep-merge-keeps-siblings ()
  (let ((source (jsonc-lines "{"
                             "  \"mcp\": {"
                             "    \"servers\": {"
                             "      \"files\": { \"command\": \"a\" }"
                             "    }"
                             "  }"
                             "}")))
    (is-values (text added replaced)
        (nlk:set-jsonc-path source '("mcp") '(:servers (:web (:url "https://x"))))
      (added '("mcp.servers.web.url") "the new server is the one addition") (replaced null)
      ((search "\"files\": { \"command\": \"a\" }" text) is)
      ((nlk:json-value (jsonc-decoded text) :string "mcp" "servers" "web" "url") "https://x"))))

(deftest jsonc-path-write-removes-a-member-with-its-separator ()
  ;; :REMOVE is the only way a member leaves. The three positions a member can
  ;; sit in each need a different comma taken with it, and the operator's
  ;; comments are what a naive splice eats on the way out.
  (let ((source (jsonc-lines "{"
                             "  \"providers\": {"
                             "    \"a\": { \"sdk\": \"openai-responses\" },"
                             "    \"b\": { \"sdk\": \"anthropic\" }, // the one to go"
                             "    \"c\": { \"sdk\": \"openai-responses\" }"
                             "  }"
                             "}")))
    (is-jsonc-edit (source '("providers" "b") :remove)
        ((format nil "    \"b\": { \"sdk\": \"anthropic\" }, // the one to go~%") "")
        "the member, its line and the comma that joined it are gone; the
           trailing comment went with the member it annotated"
        (is (null added) "a removal adds nothing")
        (is (null replaced) "and replaces nothing")
        (is (equal '("providers.b") removed) "it reports the member it took out")
        (is (equal 2 (hash-table-count (nlk:json-value (jsonc-decoded text) :object "providers")))))
    (is-jsonc-edit (source '("providers" "a") :remove)
        ((format nil "    \"a\": { \"sdk\": \"openai-responses\" },~%") "")
        "the first member takes the comma that follows it")
    (is-jsonc-edit (source '("providers" "c") :remove)
        ((jsonc-lines "    \"b\": { \"sdk\": \"anthropic\" }, // the one to go"
                      "    \"c\": { \"sdk\": \"openai-responses\" }")
         "    \"b\": { \"sdk\": \"anthropic\" } // the one to go")
        "the last takes the comma that precedes it, leaving no trailing one"))
  (is (equal (jsonc-lines "{" "}")
             (jsonc-set (jsonc-lines "{" "  \"only\": 1" "}") '("only") :remove)))
  (is (equal "{\"a\": 1 }" (jsonc-set "{\"a\": 1, \"b\": 2}" '("b") :remove))))

(deftest jsonc-path-write-removes-nothing-it-was-not-asked-to ()
  ;; A removal never creates, and removing what is absent is what the document
  ;; already says -- so it must leave the file, and the reader's memo over its
  ;; mtime, alone.
  (let ((source (jsonc-lines "{" "  // kept" "  \"general\": { \"theme\": \"T\" }" "}")))
    (dolist (path '(("providers" "quiai") ("providers") ("general" "absent")))
      (multiple-value-bind (text added replaced removed)
          (nlk:set-jsonc-path source path :remove)
        (is (eq source text)
            (format nil "removing the absent ~{~a~^.~} returns the source itself" path))
        (is (and (null added) (null replaced) (null removed)))))
    (is (search "general.theme is not an object"
                (refusal-text nlk:jsonc-refusal
                  (nlk:set-jsonc-path source '("general" "theme" "deeper") :remove)))))
  (is (equal "" (jsonc-set "" '("a") :remove)))
  (is (equal "  // only a note" (jsonc-set "  // only a note" '("a") :remove))))

(deftest jsonc-plist-converters-round-trip ()
  (let* ((decoded (jsonc-decoded "{\"id\": \"1\", \"author\": {\"name\": \"a\"}, \"tags\": [\"x\"], \"gone\": null}"))
         (plist (nlk:json-plist decoded)))
    (is (equal "1" (getf plist :id)))
    (is (equal "a" (getf (getf plist :author) :name)) "objects nest as plists")
    (is-shape plist (:tags equalp #("x") "arrays stay vectors") (:gone eq :null "null stays :NULL")
      (nlk:plist-json equalp decoded "and the plist reads back as the object"))))

;;; --- the reasoning-field waist ----------------------------------------------
;;; Endpoints disagree on which key carries a thinking trace: llama.cpp streams
;;; reasoning_content, vLLM streams reasoning, and some send both holding the
;;; same text. The lane boundary folds all of that to one dialect, because the
;;; event log stores exactly one.

(deftest wire-reasoning-spans-the-field-names-endpoints-disagree-on ()
  (is-table (json) (equal "t" (nlk:wire-reasoning (nlk:decode-json json)))
    ("{\"reasoning_content\":\"t\"}" "llama.cpp's reasoning_content reads")
    ("{\"reasoning\":\"t\"}" "vLLM's reasoning reads")
    ("{\"reasoning_text\":\"t\"}" "reasoning_text reads"))
  (is-values (text present field) (nlk:wire-reasoning (nlk:decode-json "{\"content\":\"hi\"}"))
    (text null "no thinking reads no text") (present null "and is not present")
    (field null "and names no field")))

(deftest wire-reasoning-counts-a-doubled-trace-once ()
  ;; chutes.ai sends reasoning_content and reasoning holding the same text;
  ;; taking both would render the thinking twice.
  (is-values (text present field)
      (nlk:wire-reasoning
       (nlk:decode-json "{\"reasoning_content\":\"once\",\"reasoning\":\"once\"}"))
    (text "once" "the doubled trace yields one copy") (present is "and registers as present")
    (field "reasoning_content" "precedence picks the first field"))
  (is (equal "vllm" (nlk:wire-reasoning
                     (nlk:decode-json "{\"reasoning_content\":\"\",\"reasoning\":\"vllm\"}")))))

(deftest wire-reasoning-reports-presence-apart-from-text (multiple-value-bind (text present))
  ;; The chat lane replays the key's PRESENCE, not its text, so an empty
  ;; reasoning chunk still has to register or replay drops the field.
  (nlk:wire-reasoning (nlk:decode-json "{\"reasoning_content\":\"\"}"))
(is (equal "" text) "an empty trace reads as the empty string")
(is present "and is still present"))

;;; --- decode-json: the one reader, what it reads and what it costs ----------

(defun json-same-p (one two)
  "Whether two decoded values say the same thing."
  ;; Not EQUALP: that reads strings without case and calls 1 and 1.0 the same,
  ;; and both distinctions are exactly what a reader must not lose.
  (string= (nlk:encode-json-object one) (nlk:encode-json-object two)))

(deftest decode-json-reads-what-shasht-read ()
  ;; The reader replaced shasht's. Every value in the corpus must decode to
  ;; the same thing it decoded to before, numeric types and string contents
  ;; included; the shapes those values are held in are the only difference,
  ;; and the tests below are what assert those.
  (dolist (text (list "{}" "[]" "\"\"" "0" "-0" "true" "false" "null"
                      "  {  \"a\" : 1 , \"b\" : [ 2 , 3 ] }  "
                      "{\"nested\":{\"deep\":{\"deeper\":[{\"x\":null}]}}}"
                      "[1,-2,1.5,-1.25,2e3,2E3,1e-2,-3.5e+2,0.125]"
                      "[9007199254740993,-9007199254740993]"
                      "\"plain text\""
                      "\"esc \\\" \\\\ \\/ \\b \\f \\n \\r \\t\""
                      "\"unicode \\u00e9 \\u4e2d\\u6587\""
                      "\"astral \\ud83d\\ude00\""
                      "{\"dup\":1,\"dup\":2}"
                      "[[[[[1]]]]]"
                      "{\"empty-object\":{},\"empty-array\":[],\"empty-string\":\"\"}"))
    (is (json-same-p (nlk:decode-json text) (shasht:read-json text))
        (format nil "~a reads the same as it did" text))))

(deftest decode-json-holds-its-shapes-tightly ()
  (let* ((text "{\"models\":[{\"type\":\"text\",\"name\":\"a\"},{\"type\":\"text\",\"name\":\"b\"}],\"type\":\"list\",\"n\":null,\"f\":false,\"t\":true}")
         (root (nlk:decode-json text))
         (models (gethash "models" root)))
    (is (hash-table-p root) "an object is a hash table")
    (is (eq 'equal (hash-table-test root)) "with the EQUAL test the image reads by")
    (is (= 5 (hash-table-count root)) "every member present")
    (is (simple-vector-p models) "an array is a simple vector")
    (is (= 2 (length models)) "of its own length")
    (is (typep (gethash "type" root) 'simple-string) "a string is a simple string")
    (is (= 4 (length (gethash "type" root))) "of its own length")
    (is-shape root ("n" eq :null "null is :NULL") ("f" null "false is NIL") ("t" eq t "true is T"))
    (let ((first (svref models 0)))
      (is-shape first ("type" eq (gethash "type" (svref models 1))) ("name" "a" "values survive"))
      (is (every (lambda (key) (typep key 'simple-string)) (alexandria:hash-table-keys first))))))

(deftest decode-json-narrows-a-string-to-what-it-holds ()
  ;; Wire text is overwhelmingly ASCII, and a wide character string costs
  ;; four bytes a character to say so.
  (is-let* ((narrow (svref (nlk:decode-json "[\"openai-completions\"]") 0))
            (wide (svref (nlk:decode-json "[\"caf\\u00e9 \\u4e2d\\u6587\"]") 0)))
    ((typep narrow 'simple-base-string) is "an ASCII string is a base string")
    (narrow string= "openai-completions" "and says what it said")
    ((typep wide '(simple-array character (*))) is) ((length wide) = 7 "of its own length")
    ((char= #\é (char wide 3)) is "the character itself")
    (narrow (concatenate 'string "openai-" "completions"))))

(deftest decode-json-reads-every-escape ()
  (let ((simple (svref (nlk:decode-json "[\"a\\nb\\tc\\\"d\\\\e\\/f\\bg\\fh\\ri\"]") 0))
        (astral (svref (nlk:decode-json "[\"\\ud83d\\ude00\"]") 0)))
    (is (equal (format nil "a~cb~cc\"d\\e/f~cg~ch~ci"
                       #\Newline #\Tab #\Backspace #\Page #\Return)
               simple))
    (is (= 1 (length astral)) "a surrogate pair is one character")
    (is (= #x1f600 (char-code (char astral 0))) "the character it spells")
    (is-table (text) (signals-error nlk:json-refusal (nlk:decode-json text))
      ("\"\\ud83d\"" "a leading surrogate without its pair is a refusal")
      ("\"\\ud800\\u0041\"" "a leading surrogate before a plain escape is a refusal")
      ("\"\\ud800\\ud800\"" "a leading surrogate before another is a refusal")
      ("\"\\udc00\"" "a lone trailing surrogate is a refusal")
      ("\"\\q\"" "an escape that is not one is a refusal")
      ((format nil "\"a~cb\"" #\Newline) "a raw control character inside a string is a refusal"))))

(deftest decode-json-refuses-what-is-not-json ()
  (dolist (text '("" "{" "[1,]" "{\"a\"}" "{\"a\":}" "{a:1}" "[1 2]"
                  "tru" "nul" "-" "1." "1e" "\"unclosed"
                  "01" "-01" "[00]" "1e400" "1e999999999999999999999"))
    (is (signals-error nlk:json-refusal (nlk:decode-json text))
        (format nil "~s is refused" text)))
  (is (signals-error nlk:json-refusal
                     (nlk:decode-json (format nil "~v@{[~}1~v@{]~}"
                                              300 300 nil))))
  (let ((most (make-string 4300 :initial-element #\7)))
    (is (= (parse-integer most) (nlk:decode-json most)))
    (is (signals-error nlk:json-refusal (nlk:decode-json (format nil "~a7" most))))))

(deftest decode-json-reads-the-numbers-a-leading-zero-allows ()
  (is (eql 0 (nlk:decode-json "0")))
  (is (eql 0 (nlk:decode-json "-0")))
  (is (= 0.5d0 (nlk:decode-json "0.5")))
  (is (= 10 (nlk:decode-json "10")))
  (is (= 0 (nlk:decode-json "0e1")))
  (is (= 0 (nlk:decode-json "1e-400")) "a number too small for a float reads as zero"))

(deftest decode-json-reads-streams-and-refuses-a-trailing-value-when-asked ()
  (is (equal 7 (gethash "n" (nlk:decode-json (make-string-input-stream "{\"n\":7}")))))
  (is (equal 1 (gethash "a" (nlk:decode-json "{\"a\":1} {\"b\":2}"))))
  (is (signals-error nlk:json-refusal (nlk:decode-json "{\"a\":1} {\"b\":2}" :whole t)))
  (is (equal 1 (gethash "a" (nlk:decode-json "  {\"a\":1}  " :whole t)))))

(deftest decode-json-shares-short-strings-across-documents ()
  ;; Shared per document, a session's events kept one copy of each key and
  ;; enumerated value per event.
  (let ((one (nlk:decode-json "{\"role\":\"assistant\"}"))
        (two (nlk:decode-json "{\"role\":\"assistant\"}")))
    (flet ((key (table) (loop for key being the hash-keys of table return key)))
      (is (eq (key one) (key two)) "a key")
      (is (eq (gethash "role" one) (gethash "role" two)) "a value")))
  (let* ((long (make-string (1+ nlk::+json-shared-string-length+) :initial-element #\x))
         (decoded (nlk:decode-json (format nil "[~s,~s]" long long))))
    (is (not (eq (svref decoded 0) (svref decoded 1))) "a longer string is its own")
    (is (typep (svref decoded 0) 'simple-string) "and simple all the same"))
  (let ((nlk::*json-shared-strings* (make-hash-table :test #'equal :synchronized t))
        (nlk::+json-shared-string-count+ 2))
    (is (equalp #("a" "b" "c" "d" "e") (nlk:decode-json "[\"a\",\"b\",\"c\",\"d\",\"e\"]")) "a start-over loses no value")
    (is (<= (hash-table-count nlk::*json-shared-strings*) 2) "and what sharing pins stays within +JSON-SHARED-STRING-COUNT+")))

(deftest decode-json-reads-octets-as-it-reads-text ()
  ;; The wire and the disk hand over bytes; the reader takes them as they
  ;; are, and a string's own bytes are the only ones ever decoded.
  (dolist (text (list "{}" "[1,-2,1.5,2e3]" "\"esc \\\" \\\\ \\n\"" "\"unicode \\u00e9\""
                      "{\"nested\":{\"deep\":[{\"x\":null,\"t\":true,\"f\":false}]}}"))
    (is (json-same-p (nlk:decode-json (sb-ext:string-to-octets text :external-format :utf-8))
                     (nlk:decode-json text))
        (format nil "~a reads the same from its bytes" text)))
  (is-let* ((wide (svref (nlk:decode-json (nlk:utf-8-octets "[\"café 中文\"]")) 0))
            (narrow (svref (nlk:decode-json (nlk:utf-8-octets "[\"plain\"]")) 0))
            (mixed (svref (nlk:decode-json (nlk:utf-8-octets "[\"a\\né\\tb\"]")) 0)))
    ((typep wide '(simple-array character (*))) is "raw UTF-8 reads wide")
    ((length wide) = 7 "of its own length")
    ((char= #\é (char wide 3)) is "the character the bytes spell")
    ((typep narrow 'simple-base-string) is "ASCII bytes read as a base string")
    (narrow string= "plain") (mixed (format nil "a~cé~cb" #\Newline #\Tab))))

(deftest decode-json-refuses-bytes-that-are-not-utf-8 ()
  (is-table (in) (signals-error nlk:json-refusal in) ((nlk:decode-json (octets 34 #xff 34)))
    ((nlk:decode-json (octets 34 92 110 #xc3 34))) ((nlk:decode-json (octets 91 #xc3 #xa9 93))))
  ;; Well-formed only: an overlong form, a surrogate, a code past #x10FFFF.
  (dolist (codes '((#xc0 #xaf) (#xe0 #x80 #xaf) (#xed #xa0 #x80) (#xf4 #x90 #x80 #x80)))
    (is (signals-error nlk:json-refusal (nlk:decode-json (apply #'octets 34 92 110 (append codes '(34)))))))
  (is (= #x10ffff (char-code (char (nlk:decode-json (octets 34 #xf4 #x8f #xbf #xbf 34)) 0)))))

(deftest encode-json-octets-writes-the-body-as-utf-8 ()
  (let* ((object (nlk:json-object "k" "café" "n" 1))
         (octets (nlk:encode-json-octets object)))
    (is (typep octets 'nlk:octets) "octets, the form a body leaves in")
    (is (= (length octets)
           (length (nlk:utf-8-octets (nlk:encode-json-object object)))))
    (is (json-same-p object (nlk:decode-json octets)) "and reads back")))

(deftest encode-json-octets-writes-the-bytes-shasht-writes ()
  ;; Two encoders, one text: the octet writer's bytes are the UTF-8 of what
  ;; ENCODE-JSON-OBJECT (shasht) writes, for what it owns and what it hands
  ;; to shasht alike.
  (dolist (value (list (nlk:json-object "a" (vector 1 -20 "x" nil t :null (vector))
                                        "b" (nlk:json-object) "c" (list 1 (list 2)))
                       (coerce (loop for code below 300 collect (code-char code)) 'string)
                       (coerce (list (code-char #x7ff) (code-char #xffff) (code-char #x1f600)) 'string)
                       (coerce "base \\ \" /" 'simple-base-string)
                       (make-array 3 :element-type 'character :initial-contents "a\"b"
                                     :fill-pointer 2)
                       most-negative-fixnum 0 12345678901234567890 1.5 -0.25d0 1/3
                       :keyword #\x (list :array 1 2) (make-array '(2 2) :initial-element 0)))
    (is (equalp (sb-ext:string-to-octets (nlk:encode-json-object value) :external-format :utf-8)
                (nlk:encode-json-octets value))
        (prin1-to-string value))))

;;; --- a value already encoded, and the memo that keeps it -------------------

(deftest encode-json-octets-lays-a-verbatim-in-as-its-bytes ()
  ;; A verbatim of 512 bytes or more is spliced in, not copied.
  (let* ((inner (nlk:json-object "k" "café" "n" (vector 1 2)
                                 "long" (make-string 600 :initial-element #\é)))
         (whole (nlk:json-object "a" inner "b" (vector inner "x" inner) "c" 3)))
    (flet ((verbatim () (nlk:json-verbatim (nlk:encode-json-octets inner))))
      (let ((spliced (nlk:json-object "a" (verbatim)
                                      "b" (vector (verbatim) "x" (verbatim))
                                      "c" 3)))
        (is (typep (nlk:encode-json-octets spliced) 'nlk:octets))
        (is (equalp (nlk:encode-json-octets whole) (nlk:encode-json-octets spliced)))
        (is (equalp (nlk:encode-json-octets inner) (nlk:encode-json-octets (verbatim))))
        (is (equal (nlk:encode-json-object whole) (nlk:encode-json-object spliced)))))))

(deftest json-memo-hands-out-verbatims-once-the-body-was-encoded ()
  (let* ((memo (nlk:make-json-memo))
         (one (nlk:json-object "role" "user" "content" "one"))
         (two (nlk:json-object "role" "assistant" "content" "two"))
         (items (lambda (message) (list (nlk:copy-json-object message "seen" t))))
         (first (nlk:json-memo-items memo (vector one two) :test items)))
    (is (= 2 (length first)))
    (is (every #'hash-table-p first) "the first build hands out the builder's items")
    (is (every #'hash-table-p (nlk:json-memo-items memo (vector one two) :test items)))
    (let* ((body (nlk:json-object "m" (nlk:json-memo-items memo (vector one two) :test items)
                                  "z" 1))
           (octets (nlk:encode-json-body body "m" memo))
           (again (nlk:json-memo-items memo (vector one two) :test items)))
      (is (every #'nlk:json-verbatim-p (gethash "m" body)))
      (is (every #'nlk:json-verbatim-p again) "noted items come back as verbatims")
      (is (equalp octets (nlk:encode-json-octets (nlk:json-object "m" again "z" 1))))
      (is (equalp octets
                  (nlk:encode-json-octets
                   (nlk:json-object "m" (coerce (append (funcall items one) (funcall items two))
                                                'vector)
                                    "z" 1))))
      (is (every #'hash-table-p
                 (nlk:json-memo-items memo (vector (nlk:copy-json-object one)) :test items)))
      (is (every #'hash-table-p (nlk:json-memo-items memo (vector one) :other items))))))

(deftest json-memo-keeps-an-object-s-item-count-and-order ()
  (let* ((memo (nlk:make-json-memo))
         (none (nlk:json-object "n" 0))
         (pair (nlk:json-object "n" 2))
         (items (lambda (object)
                  (if (zerop (gethash "n" object))
                      '()
                      (list (nlk:json-object "i" "first") (nlk:json-object "i" "second")))))
         (body (nlk:json-object "m" (nlk:json-memo-items memo (vector none pair) :test items))))
    (is (= 2 (length (gethash "m" body))) "nothing for NONE, two items for PAIR")
    (let ((octets (nlk:encode-json-body body "m" memo))
          (again (nlk:json-memo-items memo (vector none pair) :test items)))
      (is (= 2 (length again)))
      (is (every #'nlk:json-verbatim-p again) "an object with no items is complete at once")
      (is (equalp octets (nlk:encode-json-octets (nlk:json-object "m" again)))))))

(deftest json-memo-measures-once-and-sums-an-array-through-its-elements ()
  (let* ((memo (nlk:make-json-memo))
         (one (nlk:json-object "k" "café"))
         (two (nlk:json-object "k" (vector 1 2 3)))
         (length-one (length (nlk:encode-json-object one)))
         (length-two (length (nlk:encode-json-object two))))
    (is (= length-one (nlk:json-memo-chars memo one)))
    (is (= (+ 2 1 length-one length-two) (nlk:json-memo-chars memo (vector one two))))
    (is (= (length (nlk:encode-json-object (vector one two)))
           (nlk:json-memo-chars memo (vector one two))))
    (is (= 2 (nlk:json-memo-chars memo (vector))))
    (setf (gethash "k" one) "changed since")
    (is (= length-one (nlk:json-memo-chars memo one)))))

(deftest a-kept-decode-leaves-out-what-its-reader-refuses ()
  ;; What KEEP refuses is passed over, strings with escaped quotes and
  ;; brackets inside included, and what it keeps decodes as a whole decode
  ;; would; the path it is asked of runs innermost key first.
  (let* ((seen '())
         (value (nlk:decode-json
                 "{\"a\": {\"keep\": [1, {\"x\": 2}], \"drop\": {\"s\": \"q\\\"}]{\", \"n\": [1, [2]]}},
                   \"b\": -1.5e2, \"c\": \"gone\", \"d\": true}"
                 :whole t
                 :keep (lambda (path)
                         (push path seen)
                         (not (member (first path) '("drop" "c") :test #'string=))))))
    (is (equalp '(1 2) (list (aref (nlk:json-value value :array "a" "keep") 0)
                             (nlk:json-value (aref (nlk:json-value value :array "a" "keep") 1) :integer "x"))))
    (is (null (nth-value 1 (gethash "drop" (gethash "a" value)))) "the refused member is left out")
    (is (null (nth-value 1 (gethash "c" value))) "a refused string too")
    (is-shape value ("d" eq t "and the members after it read") ("b" = -150))
    (is (member '("x" "keep" "a") seen :test #'equal) "an array's elements share its path")
    (is (not (member "s" seen :key #'first :test #'string=)) "nothing inside a refused value is asked")))

(deftest a-revived-decode-puts-the-reader-s-record-in-each-member-s-place ()
  ;; JSON.parse's reviver: each kept member's value as it closes, innermost
  ;; first, and the answer stands in its place.
  (let* ((order '())
         (value (nlk:decode-json "{\"m\": {\"a\": {\"n\": 1}, \"b\": {\"n\": 2}}}"
                                 :revive (lambda (path value)
                                           (push (first path) order)
                                           (if (= 2 (length path)) (gethash "n" value) value)))))
    (is (equal '(1 2) (list (gethash "a" (gethash "m" value)) (gethash "b" (gethash "m" value)))))
    (is (equal '("n" "a" "n" "b" "m") (reverse order)) "inner members close first")))
