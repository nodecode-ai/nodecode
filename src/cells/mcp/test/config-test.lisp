;;;; config-test.lisp --- the mcp section, entry by entry.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (both mcp::server-spec) (files mcp::server-spec) (remote mcp::server-spec))

(defun mcp-spec (section name)
  (find name (mcp::parse-mcp-section section) :key #'mcp::server-spec-name :test #'string=))

(deftest mcp-cell-config-infers-the-transport ()
  (let* ((section (mcp-section
                   "files" (mcp-entry "command" "mcp-files" "args" (vector "--root" "/tmp"))
                   "remote" (mcp-entry "url" "https://host/mcp"
                                       "headers" (mcp-entry "Authorization" "Bearer secret-1"))
                   "both" (mcp-entry "command" "x" "url" "http://h/")
                   "explicit" (mcp-entry "url" "http://h/" "transport" "streamable-http")
                   "object" (mcp-entry "url" "http://h/" "transport" (mcp-entry "type" "http"))))
         (files (mcp-spec section "files"))
         (remote (mcp-spec section "remote"))
         (both (mcp-spec section "both")))
    (is-shape files (.transport eq :stdio "a command is stdio")
      (.args '("--root" "/tmp") "args as a list") (.timeout-ms = 30000 "the Zig default timeout"))
    (is (eq :http remote.transport) "a url alone is http")
    (is (equal '(("Authorization" . "Bearer secret-1")) remote.headers))
    (is (eq :stdio both.transport) "command plus url is stdio (the Zig rule)")
    (is (eq :http (mcp::server-spec-transport (mcp-spec section "explicit"))))
    (is (eq :http (mcp::server-spec-transport (mcp-spec section "object"))))
    (is (equal '("both" "explicit" "files" "object" "remote")
               (mapcar #'mcp::server-spec-name (mcp::parse-mcp-section section))))))

(deftest mcp-cell-config-refusal-is-per-entry ()
  (let* ((section (mcp-section
                   "good" (mcp-entry "command" "ok")
                   "bad name!" (mcp-entry "command" "x")
                   "nourl" (mcp-entry "transport" "http")
                   "badenv" (mcp-entry "command" "x" "env" (mcp-entry "K" 5))
                   "negative" (mcp-entry "command" "x" "timeout_ms" -1)
                   "off" (mcp-entry "command" "x" "enabled" nil)
                   "zero" (mcp-entry "command" "x" "timeout_ms" 0)))
         (specs (mcp::parse-mcp-section section)))
    (flet ((refusal (name) (mcp::server-spec-refusal (mcp-spec section name))))
      (is (= 7 (length specs)) "every entry yields a spec")
      (is (null (refusal "good")) "the good one is clean")
      (is (search "only letters" (refusal "bad name!")) "a bad name is refused with its rule")
      (is (search "needs an http(s) url" (refusal "nourl")) "http without a url is refused")
      (is (search "must be a string" (refusal "badenv")) "a non-string env value is refused")
      (is (search "positive" (refusal "negative")) "a negative timeout is refused")
      (is (not (mcp::server-spec-enabled-p (mcp-spec section "off"))) "enabled: false is kept")
      (is (null (refusal "off")) "and not a refusal")
      (is (= 30000 (mcp::server-spec-timeout-ms (mcp-spec section "zero")))))))

(deftest mcp-cell-config-child-environment ()
  (let* ((spec (mcp-spec (mcp-section "s" (mcp-entry "command" "x"
                                                     "env" (mcp-entry "MCP_TEST_OVERRIDE" "two"
                                                                      "MCP_TEST_EXTRA" "yes")))
                         "s"))
         (environment (progn
                        (sb-posix:setenv "MCP_TEST_OVERRIDE" "one" 1)
                        (sb-posix:setenv "MCP_TEST_FN" "() { echo hi; }" 1)
                        (sb-posix:setenv "MCP_TEST_PLAIN" "plain" 1)
                        (mcp::child-environment
                         spec '("MCP_TEST_OVERRIDE" "MCP_TEST_FN" "MCP_TEST_PLAIN"
                                "MCP_TEST_ABSENT")))))
    (is (member "MCP_TEST_PLAIN=plain" environment :test #'string=))
    (is (member "MCP_TEST_OVERRIDE=two" environment :test #'string=))
    (is (not (member "MCP_TEST_OVERRIDE=one" environment :test #'string=)))
    (is (member "MCP_TEST_EXTRA=yes" environment :test #'string=))
    (is (notany (lambda (entry) (uiop:string-prefix-p "MCP_TEST_FN=" entry)) environment))
    (is (notany (lambda (entry) (uiop:string-prefix-p "MCP_TEST_ABSENT=" entry)) environment))))

(deftest mcp-cell-config-allowlist-and-defaults ()
  (multiple-value-bind (specs allowlist) (mcp::parse-mcp-section nil)
    (is (null specs) "no section is no servers")
    (is (equal mcp::+default-inherit-env+ allowlist) "and the default allowlist"))
  (let ((section (mcp-section "s" (mcp-entry "command" "x"))))
    (setf (gethash "inherit_env_allowlist" section) (vector "PATH" "HOME"))
    (is (equal '("PATH" "HOME") (nth-value 1 (mcp::parse-mcp-section section))))
    (setf (gethash "inherit_env_allowlist" section) (vector))
    (is (null (nth-value 1 (mcp::parse-mcp-section section))))))

(deftest mcp-cell-config-transport-text-hides-headers ()
  (let ((remote (mcp-spec (mcp-section
                           "remote" (mcp-entry "url" "https://host/mcp"
                                               "headers" (mcp-entry "Authorization" "Bearer secret-1"
                                                                    "X-Other" "secret-2")))
                          "remote"))
        (files (mcp-spec (mcp-section "files" (mcp-entry "command" "mcp-files"
                                                         "args" (vector "--root")))
                         "files")))
    (is (equal "http https://host/mcp (2 headers)" (mcp::transport-text remote)))
    (is (null (search "secret" (mcp::transport-text remote))) "never a header value")
    (is (equal "stdio mcp-files --root" (mcp::transport-text files)))))
