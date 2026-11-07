;;;; cell-test.lisp --- START-CELL, its config, and the /mcp entrypoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader contract end to end: START-CELL (config) connects the
;;;; servers and registers /mcp (nle:register-command); the stop thunk takes
;;;; it all down. The command is driven exactly as SLASH drives it for every
;;;; caller.

(in-package #:nodecode.test)

(deftest mcp-cell-start-and-stop-round-trip (with-mcp-cell ("probe" (mcp-fixture-entry)))
  (is (functionp stop) "start-cell returns a stop thunk")
  (is-present (entry (nle::find-registered-command "mcp")) "/mcp is registered"
    (is (equal "nodecode-mcp" (nle::slash-command-owner entry))))
  (is (eq :ready (mcp-wait-ready "probe")) "the server connects")
  (is (assoc :mcp nle::*help-topics*) "(help :mcp) is on the manual")
  (funcall stop)
  (setf stop nil)
  (is (null mcp::*registry*) "stop clears the registry")
  (is (null (assoc :mcp nle::*help-topics*)) "and the manual")
  (is (null (nle::find-registered-command "mcp")) "and /mcp"))

(deftest mcp-cell-stop-closes-every-server-at-once
    (with-mcp-cell ("a" (mcp-fixture-entry "--linger") "b" (mcp-fixture-entry "--linger")
                    "c" (mcp-fixture-entry "--linger")))
  ;; Each lingers past stdin and SIGTERM until the kill. Closed one after
  ;; another they would hold an organism's stop -- and its store -- for the
  ;; sum of the per-server times; at once, for about one close.
  (dolist (name '("a" "b" "c")) (is (eq :ready (mcp-wait-ready name))))
  (let ((pids (mapcar (lambda (server)
                        (mcp::stdio-transport-pid (mcp::client-transport (mcp::server-client server))))
                      (mcp::registry-servers mcp::*registry*)))
        (began (get-internal-real-time)))
    (funcall stop)
    (setf stop nil)
    (is (< (nle::elapsed-ms began) 1200) "the stop takes about one close")
    (is (notany #'nlk::pid-live-p pids) "and every server is gone")))

(deftest mcp-cell-disabled-and-second-start (with-mcp-log-dir)
  (let ((config (mcp-config "probe" (mcp-fixture-entry))))
    (setf (gethash "enabled" (gethash "mcp" config)) nil)
    (let ((stop (mcp:start-cell config)))
      (is (null mcp::*registry*) "enabled: false connects nothing")
      (is (nle::routed "/api/mcp") "and still serves the page's route")
      (funcall stop)
      (is (null (nle::routed "/api/mcp")) "which its stop takes back")))
  (let ((first-stop (mcp:start-cell (mcp-config "probe" (mcp-fixture-entry)))))
    (mcp-wait-ready "probe")
    (let ((registry mcp::*registry*)
          (second-stop (mcp:start-cell (mcp-config "probe" (mcp-fixture-entry)))))
      (is (not (eq registry mcp::*registry*)) "a second start replaces the registry")
      (is (eq :stopped (mcp::server-state (first (mcp::registry-servers registry)))))
      (mcp-wait-ready "probe")
      (is (equal "echo: two" (subseq (mcp:call "probe" "echo" :message "two") 0 9)))
      (funcall second-stop)
      (funcall second-stop)
      (funcall first-stop)
      (is (null mcp::*registry*) "stop is idempotent"))))

(deftest mcp-cell-slash-status-lines ()
  (with-mcp-cell ("probe" (mcp-fixture-entry)
                   "dead" (mcp-entry "command" "/nonexistent/mcp-server" "timeout_ms" 500)
                   "off" (mcp-entry "command" "x" "enabled" nil)
                   "bad" (mcp-entry "transport" "http"))
    (mcp-wait-ready "probe")
    (mcp-wait-ready "dead")
    (let ((line (mcp-slash "")))
      ;; bare /mcp lists every server after what to do: a window that clips
      ;; the line clips the servers
      (is (uiop:string-prefix-p "MCP: /mcp on NAME | restart NAME | tools NAME -- bad refused (" line))
      (is (search "dead error (cannot launch /nonexistent/mcp-server" line))
      (is (search "off off; probe ready 10 tools" line)))
    (is (equal (mcp-slash "") (mcp-slash "status")) "/mcp status is the bare form")
    (let ((line (mcp-slash "tools probe")))
      (is (uiop:string-prefix-p "MCP: probe (10 tools): echo, fail, late" line))
      ;; the operator reads words, never a form to eval
      (is (null (search "(mcp:" line))))
    (let ((line (mcp-slash "tools dead")))
      (is (uiop:string-prefix-p "MCP: dead is error (" line) "a server down says so"))
    ;; off as /mcp status says it, and the way on
    (is (equal "MCP: off is off -- /mcp on off starts it" (mcp-slash "tools off")))
    (let ((line (mcp-slash "tools nope")))
      (is (equal "MCP: no server named nope; configured: bad, dead, off, probe" line)))
    (let ((line (mcp-slash "restart")))
      (is (equal "MCP: usage /mcp [status | on NAME | off NAME | restart NAME | tools NAME]" line)))
    (let ((line (mcp-slash "restart off")))
      (is (search "disabled in config" line) "a disabled server cannot be restarted"))
    (let ((line (mcp-slash "frobnicate")))
      (is (uiop:string-prefix-p "MCP: unknown subcommand frobnicate; usage" line)))))

(deftest mcp-cell-slash-status-leads-with-the-way-on ()
  ;; Five servers an import brought over, all off: the line ran past 120
  ;; columns and the shell's clip fell in `/mcp on NAME', the one pointer to
  ;; starting one.
  (with-mcp-cell ("context7" (mcp-entry "command" "npx" "enabled" nil)
                   "docs" (mcp-entry "command" "uvx" "enabled" nil)
                   "linear" (mcp-entry "url" "https://mcp.linear.app/mcp" "enabled" nil)
                   "pg" (mcp-entry "command" "npx" "enabled" nil)
                   "remote" (mcp-entry "url" "https://mcp.example.invalid/mcp" "enabled" nil))
    (let ((line (mcp-slash "")))
      (is (uiop:string-prefix-p "MCP: /mcp on NAME -- " line) "the way on leads, alone")
      (is (search "context7 off; docs off; linear off; pg off; remote off" line) "then every server"))))

(deftest mcp-cell-slash-restart-is-asynchronous (with-ready-mcp ("probe" (mcp-fixture-entry)))
  (let ((before (mcp::server-connected-at (mcp-server "probe"))))
    (sleep 1.1)
    (let ((line (mcp-slash "restart probe")))
      (is (equal "MCP: restarting probe; /mcp status to follow" line) "restart answers at once"))
    (is (await (:timeout 10) (nlk:when-let (at (mcp::server-connected-at (mcp-server "probe")))
                               (> at before))))
    (is (equal "echo: after" (subseq (mcp:call "probe" "echo" :message "after") 0 11)))))

(deftest mcp-cell-slash-with-no-servers-says-so ()
  (with-mcp-log-dir
    (setf mcp::*registry* (mcp::make-registry))
    (mcp::register-mcp-command)
    (is (equal "MCP: no servers configured (mcp.servers in ~/.nodecode/config.jsonc)"
               (mcp-slash "")))))

(deftest mcp-cell-status-and-tools-text ()
  (with-mcp-cell ("probe" (mcp-fixture-entry)
                   "remote" (mcp-entry "url" "http://127.0.0.1:9/mcp"
                                       "headers" (mcp-entry "Authorization" "Bearer secret-9")
                                       "timeout_ms" 500))
    (mcp-wait-ready "probe")
    (mcp-wait-ready "remote")
    (let ((status (mcp:status)))
      (is (search "probe: ready, 10 tools, stdio python3" status) "status names the stdio server")
      (is (search "server fixture 0.0.0" status) "with its serverInfo")
      (is (search "stderr: " status) "and its stderr log")
      (is (search "remote: error (" status) "the refused http server is in error")
      (is (search "http http://127.0.0.1:9/mcp (1 header)" status) "with a header count")
      (is (null (search "secret-9" status)) "never a header value"))
    (let ((tools (mcp:tools "probe")))
      (is (uiop:string-prefix-p "probe: ready, 10 tools" tools) "tools leads with the state line")
      (is (search (format nil "~%  (mcp:probe/echo :message* :per-page :include-snapshot) - Echo a message back") tools)))
    (is (search "\"required\": [" (mcp:schema "probe" "echo")) "schema prints the input schema")
    (is (signals-error mcp:mcp-unknown-tool (mcp:schema "probe" "ghost")))))
