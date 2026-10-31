;;;; yaml-test.lisp --- config.yaml and .env read as JSON reads.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest import-yaml-reads-the-block-dialect ()
  ;; The block style PyYAML writes: nested maps, a sequence at its key's own
  ;; column, empty flow forms, quoted and bare scalars, numbers, booleans,
  ;; null, comments, a list of maps, a block scalar.
  (let ((config (nik:read-yaml
                 "# written by hermes
model:
  default: gpt-5.5   # the pick
  provider: custom:subrouter-1
providers: {}
fallback_providers: []
toolsets:
- hermes-cli
- 'quoted item'
max_concurrent_sessions: null
agent:
  max_turns: 90
  environment_hint: ''
  ratio: 0.5
  flag: true
  other: false
  version: 0.21.1
  url: https://ai.example.test/v1
display:
  runtime_footer:
    fields:
    - model
    - context_pct
custom_providers:
- name: subrouter-1
  base_url: https://ai.example.test/v1
  models:
    gpt-5.5:
      name: gpt-5.5
- name: second
  api_key: \"with \\\"quotes\\\"\"
args: [\"-y\", \"@scope/pkg\", plain]
inline: {a: 1, b: two}
note: |
  first line
  second line
folded: >-
  one
  two
paragraphs: >
  first
  line

  second
")))
    (is-shape config (hash-table-p is) ((:string "model" "default") "gpt-5.5")
      ((:string "model" "provider") "custom:subrouter-1")
      ("providers" equalp (nlk:make-json-object) "{} is an empty object")
      ("fallback_providers" equalp #() "[] is an empty array")
      ("toolsets" equalp #("hermes-cli" "quoted item")) ("max_concurrent_sessions" eq :null)
      ((:integer "agent" "max_turns") eql 90)
      ((:string "agent" "environment_hint") "" "'' is the empty string")
      ((:any "agent" "ratio") = 0.5d0) ((:any "agent" "flag") eq t)
      ((:any "agent" "other") eq :false)
      ((:string "agent" "version") "0.21.1" "a dotted version is a string")
      ((:string "agent" "url") "https://ai.example.test/v1")
      ((:any "display" "runtime_footer" "fields") equalp #("model" "context_pct")))
    (let ((custom (gethash "custom_providers" config)))
      (is (and (vectorp custom) (= 2 (length custom))) "a list of maps")
      (is (equal "subrouter-1" (nlk:json-value (aref custom 0) :string "name")))
      (is (equal "gpt-5.5" (nlk:json-value (aref custom 0) :string "models" "gpt-5.5" "name")))
      (is (equal "with \"quotes\"" (nlk:json-value (aref custom 1) :string "api_key"))))
    (is-shape config ("args" equalp #("-y" "@scope/pkg" "plain") "a flow sequence")
      ((:integer "inline" "a") eql 1 "a flow mapping") ((:string "inline" "b") "two")
      ("note" (format nil "first line~%second line~%") "a literal block")
      ("folded" "one two" "a folded block, stripped")
      ("paragraphs" (format nil "first line~%second~%") "a blank line is a newline, with no space after it")))
  (is (zerop (hash-table-count (nik:read-yaml ""))) "an empty document is an empty object")
  (is (search "anchors" (refusal-text nik:import-error (nik:read-yaml "a: &x 1")))))

(deftest import-yaml-loses-only-the-section-it-cannot-read ()
  ;; An operator's config.yaml was refused ENTIRE over one wrapped line in a
  ;; block of persona prompts, and the refusal took their providers, their
  ;; custom endpoint and their default model — none of which the unreadable
  ;; part had anything to do with. A migration that loses everything because
  ;; it could not read one thing is worse than one that says which thing.
  (multiple-value-bind (config refusals)
      (nik:read-yaml
       "providers:
  openai:
    api_key: kept
display:
  personalities:
    a line that is no key and no item
model:
  default: gpt-5.5
")
    (is-shape config ((:string "providers" "openai" "api_key") "kept")
      ((:string "model" "default") "gpt-5.5") ("display" null))
    (is (equal '("display") (mapcar #'car refusals)))
    (is (search "no key" (cdr (first refusals)))))
  ;; Recovery is for damage, not for constructs this reader will not follow:
  ;; an alias means some other section's value is defined by something it
  ;; does not read, so carrying on would be guessing at settings elsewhere.
  (is (search "anchors" (refusal-text nik:import-error (nik:read-yaml "good: 1
bad: *ref
after: 2")))))

(deftest import-yaml-folds-a-wrapped-plain-scalar (let ((config (nik:read-yaml
                                                                 "agent:
  personalities:
    creative: You are a creative assistant. Think outside the box and offer innovative
      solutions.
    kawaii: You are a kawaii assistant! Use cute expressions,
      add sparkles, and be enthusiastic about everything! Every response
      should feel warm and adorable.
    concise: Keep responses brief.
    pirate: 'Arrr! Ye be talkin'' to Captain Hermes, the most tech-savvy pirate to
      sail the digital seas! Yo ho ho!'
  quoted: \"stays one line\"
  spoken: \"one two
    three\"
toolsets:
- a plain item that runs on
  past the end of its line
- second
providers:
  openai:
    api_key: kept
"))))
  ;; PyYAML wraps a long value over deeper-indented lines and reads it back as
  ;; one scalar with the breaks folded to single spaces. Left unread, the
  ;; continuation survived to the end of the document and READ-YAML refused the
  ;; whole file as `unexpected solutions.' — which is how one wrapped line in a
  ;; `personalities:' block, three sections away from anything that mattered,
  ;; cost an operator every provider in their config.yaml on a real migration.
  (is-shape config
    ((:string "agent" "personalities" "creative")
     "You are a creative assistant. Think outside the box and offer innovative solutions.")
    ((:string "agent" "personalities" "kawaii")
     "You are a kawaii assistant! Use cute expressions, add sparkles, and be enthusiastic about everything! Every response should feel warm and adorable.")
    ((:string "agent" "personalities" "concise") "Keep responses brief.")
    ((:string "agent" "quoted") "stays one line")
    ((:string "agent" "personalities" "pirate")
     "Arrr! Ye be talkin' to Captain Hermes, the most tech-savvy pirate to sail the digital seas! Yo ho ho!")
    ((:string "agent" "spoken") "one two three")
    ("toolsets" equalp #("a plain item that runs on past the end of its line" "second"))
    ((:string "providers" "openai" "api_key") "kept")))

(deftest import-env-file-reads-names-and-values (let ((env (nik:read-env-file
                                                            "# keys
OPENROUTER_API_KEY=or-1
export DISCORD_BOT_TOKEN=\"disc-1\"
TELEGRAM_ALLOWED_USERS='1,2'
EMPTY=
BROKEN LINE
OPENROUTER_API_KEY=or-2
"))))
  (is (equal '(("DISCORD_BOT_TOKEN" . "disc-1") ("TELEGRAM_ALLOWED_USERS" . "1,2") ("EMPTY" . "")
               ("OPENROUTER_API_KEY" . "or-2")) env) "export and quotes stripped, later line wins"))
