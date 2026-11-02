;;;; shape-test.lisp --- the artifact shapes, and what must NOT read as one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The classifier is the whole architecture: readers per shape, worlds as
;;;; data. Half of these gates are therefore about false positives, because
;;;; a shape reader that is too eager is worse than a world table — it
;;;; writes a settings value into auth.json and calls it a provider. Both
;;;; refusals below are ones a prototype actually made on a real box.

(in-package #:nodecode.test)

(nlk:access (fact nik::fact))

(defun classify (text &key (source "config.json"))
  (nik::classify-tree (nlk:decode-json text) :source source :world "fixture"))

(deftest import-shape-reads-a-credential-under-any-spelling-and-any-depth ()
  ;; api_key, apiKey and API-KEY are one member under three spellings, and
  ;; the interesting ones are rarely at the top: a first prototype that
  ;; classified top-level members only missed a whole home's auth profiles.
  (let ((facts (classify (format nil "{\"agents\": {\"main\": {\"auth\": {\"profiles\": {
  \"anthropic:default\": {\"type\": \"api\", \"key\": \"~a\"},
  \"openai\": {\"apiKey\": \"~a\"}}}}}}" +fixture-key+ +fixture-key-2+))))
    (is (equal '("anthropic" "openai") (fact-ids facts :provider)))))

(deftest import-shape-keeps-a-grant-out-and-names-it ()
  (let ((facts (classify (format nil "{\"claudeAiOauth\": {\"accessToken\": \"~a\", \"refreshToken\": \"~a\", \"expiresAt\": 1}}"
                                 +fixture-key+ +fixture-refresh+))))
    (is (null (fact-ids facts :provider)) "a grant is never a key")
    (is (equal '("claudeaioauth") (fact-ids facts :oauth))))
  (let ((facts (classify (format nil "{\"profiles\": {\"anthropic:max\": {\"type\": \"oauth\", \"refresh\": \"~a\"}}}"
                                 +fixture-refresh+))))
    (is (equal '("anthropic") (fact-ids facts :oauth)) "an explicit type says so too")))

(deftest import-shape-refuses-a-settings-value-that-only-looks-like-a-key ()
  ;; `.claude.json' carries a usage breakdown whose rows each have a `key'.
  ;; A loose rule read nine providers out of it. A credential needs both a
  ;; credential CONTEXT — a credential file, or a key that says so — and a
  ;; value shaped like a secret.
  (let ((facts (classify "{\"cachedUsageUtilization\": {\"utilization\": {\"seven_day_breakdown\": {
  \"rows\": [{\"key\": \"seven_day\", \"value\": 12}]}}}}")))
    (is (null (fact-ids facts :provider)) "no context, and the value is not a secret"))
  (let ((facts (classify (format nil "{\"settings\": {\"key\": \"~a\"}}" +fixture-key+))))
    (is (null (fact-ids facts :provider)) "a secret-shaped value in a settings file is still not one"))
  (let ((facts (classify (format nil "{\"anthropic\": {\"type\": \"api\", \"key\": \"~a\"}}" +fixture-key+)
                         :source "auth.json")))
    (is (equal '("anthropic") (fact-ids facts :provider)))))

(deftest import-shape-refuses-a-workflow-permission-as-a-grant ()
  ;; A GitHub workflow inside a plugin checkout says `permissions: id-token:
  ;; write'. A grant reader that matched the NAME read that as somebody's
  ;; refresh token, under the provider id `permissions'.
  (let ((facts (classify "{\"permissions\": {\"contents\": \"read\", \"id-token\": \"write\"}}")))
    (is (null (fact-ids facts :oauth)) "`write' is not a token")
    (is (null (fact-ids facts :provider)))))

(deftest import-shape-reads-an-environment-name-map ()
  (let ((facts (classify (format nil "{\"ACMEROUTER_API_KEY\": \"~a\", \"auth_mode\": \"apikey\"}"
                                 +fixture-key+)
                         :source "auth.json")))
    (is (equal '("acmerouter") (fact-ids facts :provider))))
  (let ((facts (classify (format nil "{\"TELEGRAM_BOT_TOKEN\": \"~a\"}" +fixture-token+)
                         :source ".env")))
    (is (null (fact-ids facts :provider)))))

(deftest import-shape-mcp-map-is-strict-about-what-a-server-is ()
  (let ((facts (classify "{\"mcpServers\": {\"weather\": {\"command\": \"uvx\", \"args\": [\"weather-mcp\"]}}}")))
    (is (equal '("weather") (fact-ids facts :mcp)) "one server under a wrapper key is a map"))
  (let ((facts (classify "{\"mcp\": {\"docs\": {\"type\": \"local\", \"command\": [\"opencode\", \"x\", \"mcp\"], \"enabled\": true}}}")))
    (is (equal '("docs") (fact-ids facts :mcp)) "a command written as a list is a command")
    (let ((object (getf (nik:fact-value (first (remove :mcp facts :key #'nik:fact-kind :test-not #'eq)))
                        :object)))
      (is (equal "opencode" (gethash "command" object)) "its head is the program")
      (is (equalp #("x" "mcp") (gethash "args" object)) "and the rest are the arguments")))
  (let ((facts (classify "{\"validation_lanes\": {\"one\": {\"command\": \"just test\", \"owner\": \"me\"},
                                                  \"two\": {\"command\": \"just lint\", \"owner\": \"me\"}}}")))
    (is (null (fact-ids facts :mcp))))
  (let ((facts (classify "{\"connection\": {\"host\": \"db.example.test\", \"port\": 5432}}")))
    (is (null (fact-ids facts :mcp)) "and neither is an agent's own config")))

(deftest import-shape-reads-a-model-pick-however-it-is-written ()
  (flet ((pick (text)
           (let ((fact (first (remove :model (classify text) :key #'nik:fact-kind :test-not #'eq))))
             (and fact (list (getf fact.value :provider) (getf fact.value :model))))))
    (is (equal '("anthropic" "claude-opus-5") (pick "{\"model\": \"anthropic/claude-opus-5\"}")))
    (is (equal '("acmeprivate" "gpt-5.6")
               (pick "{\"model\": \"gpt-5.6\", \"model_provider\": \"acmeprivate\"}")))
    (is (equal '("subrouter-1" "gpt-5.5")
               (pick "{\"model\": {\"default\": \"gpt-5.5\", \"provider\": \"custom:subrouter-1\"}}")))
    (is (equal '(nil "gpt-5.5") (pick "{\"defaultModel\": \"gpt-5.5\"}")))
    ;; A provider named beside a model keeps the model whole, unless the model
    ;; leads with that same provider (am-07: Hermes on openrouter).
    (is (equal '("openrouter" "anthropic/claude-sonnet-4.5")
               (pick "{\"model\": {\"default\": \"anthropic/claude-sonnet-4.5\", \"provider\": \"openrouter\"}}")))
    (is (equal '("openrouter" "anthropic/claude-sonnet-4.5")
               (pick "{\"model\": {\"default\": \"openrouter/anthropic/claude-sonnet-4.5\", \"provider\": \"openrouter\"}}")))
    (is (equal '("ollama" "llama3:8b") (pick "{\"model\": \"llama3:8b\", \"model_provider\": \"ollama\"}")))))

(deftest import-shape-reads-a-cron-job-by-its-schedule-and-prompt ()
  (let* ((jobs "{\"jobs\": [
  {\"name\": \"morning\", \"prompt\": \"Summarize mail\", \"schedule\": {\"kind\": \"cron\", \"expr\": \"0 9 * * 1-5\"}},
  {\"name\": \"tick\", \"prompt\": \"Check the queue\", \"schedule\": {\"kind\": \"interval\", \"minutes\": 30}},
  {\"name\": \"script-only\", \"script\": \"echo hi\", \"schedule\": {\"kind\": \"cron\", \"expr\": \"* * * * *\"}}]}")
         (facts (classify jobs)))
    (flet ((job (id)
             (find id (remove :cron facts :key #'nik:fact-kind :test-not #'eq)
                   :key #'nik:fact-id :test #'equal)))
      (is (equal '("morning" "script-only" "tick") (fact-ids facts :cron)))
      (is (null (getf (nik:fact-value (job "script-only")) :prompt)))
      (is (equal "every 30m" (getf (nik:fact-value (job "tick")) :schedule))))))

(deftest import-shape-files-a-foreign-endpoint-under-an-id-of-its-own (with-import-runtime (home box))
  ;; The key a home files an endpoint under can be a wire's name (`openai')
  ;; and not a vendor's; a built-in's id never takes an endpoint that is not
  ;; the built-in's own.
  (flet ((entry (base)
           (first (remove :provider
                          (classify (format nil "{\"modelProviders\": {\"openai\": [{\"id\": \"m\", \"baseUrl\": ~s, \"envKey\": \"SOME_KEY\"}]}}" base))
                          :key #'nik:fact-kind :test-not #'eq))))
    (let ((fact (entry "https://dashscope.aliyuncs.com/compatible-mode/v1")))
      (is (equal "dashscope-aliyuncs" (nik:fact-id fact)) "named by its host")
      (is (equal "openai" (getf (nik:fact-value fact) :claimed)) "remembering the name the home used")
      (is (equal "openai-completions" (getf (nik:fact-value fact) :sdk)) "and not the built-in's wire"))
    (is (equal "openrouter" (nik:fact-id (entry "https://openrouter.ai/api/v1"))) "a catalog provider's own endpoint is that provider")
    (is (equal "openai" (nik:fact-id (entry "https://api.openai.com/v1"))) "and the built-in's own host is the built-in")
    (is (equal "localhost-11434" (nik:fact-id (entry "http://localhost:11434/v1"))))
    (is (equal "127-0-0-1-18954" (nik:fact-id (entry "http://127.0.0.1:18954/v1"))))))

(deftest import-shape-tells-a-secret-member-by-its-name-or-its-value ()
  (is-table (in in2) (nik::secret-member-p in in2) ("GITHUB_PERSONAL_ACCESS_TOKEN" "x")
    ("Authorization" "Bearer x") ("X-Api-Key" "x")
    ("SESSION" "ghp_0123456789abcdefABCDEF" "a credential under a plain name"))
  (is-table (in in2) (not (nik::secret-member-p in in2)) ("LOG_LEVEL" "info") ("X-Client" "hermes")
    ("MCP_TRANSPORT" "streamable-http" "long, but not a credential")
    ("ROOT" "/home/op/projects/some-long-directory-name")
    ("API_KEY" "" "an empty one leaks nothing")))
