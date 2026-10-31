;;;; toml-test.lisp --- the TOML reader, against the shapes a home writes.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest import-toml-reads-a-config-into-the-json-shape ()
  ;; The reader answers the shape NLK:JSON-VALUE walks, so one shape
  ;; classifier reads a .toml tree exactly as it reads a .json one.
  (let ((config (nik:read-toml "# a codex-shaped config
model = \"gpt-5.6\"
model_provider = \"acmeprivate\"
model_reasoning_effort = \"high\"
disable_response_storage = true
tools = [\"apply_patch\", \"shell\"]

[model_providers.acmeprivate]
name = \"Acme Private\"
base_url = \"https://private.example.test/v1\"
env_key = \"ACMEPRIVATE_API_KEY\"
wire_api = \"chat\"
query_params = { api-version = \"2026-01-01\" }

[mcp_servers.docs]
command = \"uvx\"
args = [\"docs-mcp\", \"--quiet\"]
startup_timeout_sec = 20
")))
    (is-shape config ((:text "model") "gpt-5.6") ((:text "model_provider") "acmeprivate")
      ((:any "disable_response_storage") eq t) ((:any "tools") equalp #("apply_patch" "shell"))
      ((:text "model_providers" "acmeprivate" "base_url") "https://private.example.test/v1")
      ((:text "model_providers" "acmeprivate" "query_params" "api-version") "2026-01-01")
      ((:text "mcp_servers" "docs" "command") "uvx")
      ((:any "mcp_servers" "docs" "startup_timeout_sec") eql 20))))

(deftest import-toml-reads-every-scalar-form (let ((config (nik:read-toml "int = 42
negative = -7
hex = 0xff
under = 1_000
float = 1.5
exp = 2e3
yes = true
no = false
when = 1979-05-27T07:32:00Z
plain = 'literal \\n not an escape'
escaped = \"one\\ttwo\"
multi = \"\"\"
first
second\"\"\"
empty = []
[[rows]]
id = 1
[[rows]]
id = 2
"))))
  (is-shape config ((:any "int") eql 42) ((:any "negative") eql -7) ((:any "hex") eql 255)
    ((:any "under") eql 1000) ((:any "float") = 1.5d0) ((:any "exp") = 2000d0) ((:any "yes") eq t)
    ((:any "no") eq :false) ((:text "when") "1979-05-27T07:32:00Z")
    ((:text "plain") "literal \\n not an escape") ((:text "escaped") (format nil "one~atwo" #\Tab))
    ((:text "multi") (format nil "first~%second")) ((:any "empty") equalp #()))
  (let ((rows (nlk:json-value config :any "rows")))
    (is (= 2 (length rows)) "an array of tables collects its members")
    (is (eql 2 (nlk:json-value (aref rows 1) :any "id")))))

(deftest import-toml-refuses-a-document-by-its-line ()
  (is (search "line 2"
              (refusal-text nik:import-error (nik:read-toml (format nil "good = 1~%broken~%")))))
  (is (search "set twice"
              (refusal-text nik:import-error (nik:read-toml (format nil "a = 1~%a = 2~%")))))
  (is (zerop (hash-table-count (nik:read-toml ""))))
  (dolist (escape '("\\u+041" "\\u-001" "\\ud800" "\\U00110000"))
    (is (search "line 1"
                (refusal-text nik:import-error
                              (nik:read-toml (format nil "a = \"~a\"" escape))))
        (format nil "~a is a refusal naming its line" escape)))
  (is (equal (format nil "~c~c~c" (code-char #xe9) (code-char #x1f600) #\A)
             (nlk:json-value (nik:read-toml "a = \"\\u00e9\\U0001F600\\u0041\"") :string "a"))
      "a well-formed escape still reads"))
