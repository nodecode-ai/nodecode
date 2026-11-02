;;;; config-test.lisp --- the typed config-section accessors.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Moved with the accessors from the channel kit (which now re-exports
;;;; them); the channel-only vocabulary — secrets, the allowlist floor —
;;;; stays proved in channels/kit/test/config-test.lisp.

(in-package #:nodecode.test)

(deftest config-boolean-false-vs-absent ()
  ;; shasht reads JSON false as NIL and null as :NULL, so the boolean read
  ;; must use GETHASH's second value to tell false from missing.
  (is-let* ((section (shasht:read-json "{\"enabled\": false, \"other\": true,
                                       \"nulled\": null}")))
    ((nlk:config-boolean section "enabled" t) null) ((nlk:config-boolean section "missing" t) eq t)
    ((nlk:config-boolean section "nulled" t) eq t) ((nlk:config-boolean section "other" nil) eq t)))

(deftest config-typed-accessors-read-refuse-and-default ()
  (let ((section (shasht:read-json
                  "{\"n\": 7, \"s\": \" padded \", \"bad\": \"x\",
                    \"list\": [\" a \", \"\", \"b\"],
                    \"mode\": \"draft\"}")))
    (is (= 7 (nlk:config-integer section "n" 1 :min 1)))
    (is (= 5 (nlk:config-integer section "missing" 5)))
    (is (signals-error nlk:config-refusal
          (nlk:config-integer section "n" 1 :min 10)))
    (is (signals-error nlk:config-refusal
          (nlk:config-integer section "s" 1)))
    (is (equal "padded" (nlk:config-string section "s")))
    (is (equal '("a" "b") (nlk:config-string-list section "list")))
    (is (equal '() (nlk:config-string-list section "missing")))
    (is (equal "draft" (nlk:config-enum section "mode" "off"
                                        '("off" "draft"))))
    (is (signals-error nlk:config-refusal
          (nlk:config-enum section "bad" "off" '("off" "draft"))))))

(deftest config-section-value-collapses-null-and-non-tables ()
  (dolist (case '(("{\"k\": null}" nil nil "null value reads as nothing" "and as absent")
                  (nil nil nil "a missing section reads as nothing" "for every key")
                  ("{\"k\": 3}" 3 t "a present value comes back" "with its presence")))
    (destructuring-bind (json expected-value expected-present value-why present-why) case
      (is-values (value present) (nlk:section-value (and json (shasht:read-json json)) "k")
        (value eql expected-value value-why) ((and present t) eq expected-present present-why)))))

(deftest config-refusal-reports-its-detail ()
  (nlk:when-let (condition (signals-error nlk:config-refusal
                             (nlk:config-string (shasht:read-json "{\"k\": 3}") "k")))
    (is (search "must be a string" (nlk:config-refusal-detail condition)))
    (is (search "config refusal" (princ-to-string condition)))))

;;; --- declared sections ---------------------------------------------------------

(deftest config-define-section-registers-and-derives-problems-summary-and-text
    (with-saved-globals (nlk::*sections*))
  ;; One declaration, four readers: the problems a folder refuses on, the
  ;; gate a setup panel opens on, the report's one line, the model's primer.
  (nlk:define-section ("nl-gadget" "probe")
    (:owner "nl-gadget-owner")
    (:guide "walk this way")
    (:one-of "token_env" "token_file")
    (:any-of "rooms" "users")
    ("token_env" :env :doc "the variable")
    ("token_file" :path)
    ("rooms" :list :doc "ids")
    ("users" :list)
    ("loud" :boolean :default t :doc "noise")
    ("mode" :choice :options '("a" "b") :default "a")
    ("depth" :integer :required t)
    ("name" :string :required t))
  (let ((section (nlk:find-section '("nl-gadget" "probe"))))
    (is-present section "the declaration registers under its path"
      (is (equal "nl-gadget-owner" (nlk:section-owner section)))
      (is (equal '(("token_env" "token_file")) (nlk:section-one-of section)))
      (is (equal '(("rooms" "users")) (nlk:section-any-of section)))
      (is (= 8 (length (nlk:section-fields section))))
      (is (equal "nl-gadget.probe" (nlk:section-key (nlk:section-path section))))
      ;; absent: every constraint unmet, in declaration order
      (is (equal '("depth is required" "name is required"
                   "one of token_env / token_file is required"
                   "at least one of rooms / users must be non-empty")
                 (nlk:section-problems section nil)))
      (is (nlk:section-needs-setup-p section (shasht:read-json "{}")))
      (is (equal "still needed: depth · name · token · rooms or users"
                 (nlk:section-needs section nil)))
      (is (equal "nl-gadget.probe: still needed: depth · name · token · rooms or users"
                 (nlk:section-summary section nil)))
      ;; complete
      (let* ((config (shasht:read-json
                      "{\"nl-gadget\": {\"probe\": {\"token_env\": \"T\", \"rooms\": [\"1\"],
                                                          \"depth\": 3, \"name\": \"x\"}}}"))
             (table (nlk:section-table config '("nl-gadget" "probe"))))
        (is (hash-table-p table) "section-table walks the path")
        (is (null (nlk:section-problems section table)) "complete")
        (is (not (nlk:section-needs-setup-p section config)))
        (is (equal "nl-gadget.probe: complete" (nlk:section-summary section table))))
      (is (null (nlk:section-table (shasht:read-json "{\"nl-gadget\": 3}")
                                   '("nl-gadget" "probe"))))
      ;; both of a one-of, a wrong shape, a bad choice
      (let* ((table (nlk:section-table
                     (shasht:read-json
                      "{\"nl-gadget\": {\"probe\": {\"token_env\": \"T\", \"token_file\": \"f\",
                                                          \"rooms\": [\"1\"], \"depth\": \"deep\",
                                                          \"name\": \"x\", \"mode\": \"z\"}}}")
                     '("nl-gadget" "probe")))
             (problems (nlk:section-problems section table)))
        (is (find "exactly one of token_env / token_file may be set, 2 are"
                  problems :test #'string=))
        (is (row-carrying "depth must be an integer" problems))
        (is (row-carrying "mode must be one of a|b" problems))
        (is (search "exactly one of token_env / token_file may be set, 2 are"
                    (nlk:section-needs section table))))
      ;; vetoed needs nothing
      (is (not (nlk:section-needs-setup-p
                section (shasht:read-json "{\"nl-gadget\": {\"probe\": {\"enabled\": false}}}"))))
      ;; presence by type
      (let ((table (shasht:read-json "{\"rooms\": [\" \"], \"loud\": false, \"name\": \"  \"}")))
        (is (not (nlk:field-present-p table (nlk:section-find-field section "rooms"))))
        (is (nlk:field-present-p table (nlk:section-find-field section "loud")))
        (is (not (nlk:field-present-p table (nlk:section-find-field section "name")))))
      ;; the primer
      (let ((text (nlk:section-text section)))
        (is (uiop:string-prefix-p "nl-gadget.probe:" text))
        (is-carrying text "token_env (name of an environment variable): the variable"
          "loud (boolean, default true): noise" "mode (one of a | b, default a)"
          "depth (integer, required)" "exactly one of token_env / token_file"
          "at least one of rooms / users non-empty" "walk this way"))
      ;; a second declaration of the path replaces the first
      (nlk:declare-section (nlk:make-section :path '("nl-gadget" "probe") :owner "other"))
      (is (equal "other" (nlk:section-owner (nlk:find-section '("nl-gadget" "probe")))))
      (is (= 1 (count '("nl-gadget" "probe") nlk::*sections*
                      :key #'nlk:section-path :test #'equal))))))

(deftest config-section-settings-reads-a-list-default-only-when-absent
    (with-saved-globals (nlk::*sections*))
  ;; An absent :list member reads as its :default; a present one, an empty
  ;; array included, is what it says.  Without this, a declared list would
  ;; read NIL rather than its :default.
  (nlk:define-section ("nl-gadget" "lists")
    ("kinds" :list :default (vector "a" "b"))
    ("bare" :list))
  (flet ((settings (json)
           (nlk:section-settings (nlk:find-section '("nl-gadget" "lists"))
                                 (and json (shasht:read-json json)))))
    (is (equal '(:kinds ("a" "b") :bare nil) (settings nil)))
    (is (equal '(:kinds ("a" "b") :bare nil) (settings "{}")))
    (is (equal '(:kinds ("c") :bare ("d")) (settings "{\"kinds\": [\"c\"], \"bare\": [\"d\"]}")))
    (is (equal '(:kinds nil :bare nil) (settings "{\"kinds\": []}")))))
