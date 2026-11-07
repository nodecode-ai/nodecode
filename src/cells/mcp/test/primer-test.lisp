;;;; primer-test.lisp --- (help :mcp) and the line every request carries.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest mcp-cell-primer-lists-the-catalog ()
  (with-mcp-cell ("probe" (mcp-fixture-entry)
                   "off" (mcp-entry "command" "x" "enabled" nil))
    (mcp-wait-ready "probe")
    (let ((text (mcp::primer-text)))
      (is (uiop:string-prefix-p "MCP servers configured by the user" text))
      (is (search "(mcp:probe/echo :message* :per-page :include-snapshot) - Echo a message back" text))
      (is (search (format nil "~%  off: disabled") text) "a disabled server is one line")
      (is (equal text (progn (mcp::refresh-primer) (mcp::primer-text)))))))

(deftest mcp-cell-primer-says-what-a-server-offers-never-how-it-is-doing ()
  ;; A state in the text moved the prompt prefix for every session each time
  ;; a server came up or dropped while it rode every request.
  (with-mcp-cell ("probe" (mcp-fixture-entry)
                   "dead" (mcp-entry "command" "/nonexistent/mcp-server-binary" "timeout_ms" 3000))
    (mcp-wait-ready "probe")
    (mcp-wait-ready "dead")
    (let ((text (mcp::primer-text)))
      (is (search (format nil "~%  dead: no tools listed - (mcp:status)") text) text)
      (is (search "(mcp:probe/echo" text))
      (dolist (state '(:connecting :error :ready :disconnected))
        (mcp::set-state (mcp-server "probe") state)
        (mcp::set-state (mcp-server "dead") state)
        (mcp::refresh-primer)
        (is (string= text (mcp::primer-text)) (format nil "the same bytes with both ~(~a~)" state))))))

(deftest mcp-cell-primer-degrades-under-the-budget ()
  (with-ready-mcp ("probe" (mcp-fixture-entry "--many" "40"))
    (is (search "(mcp:probe/gen-3 :value)" (mcp::primer-text)) "tier 1 lists every tool")
    (with-saved-globals (mcp::*primer-budget*)
      (setf mcp::*primer-budget* (+ (length mcp::+preamble+) 1200))
      (mcp::refresh-primer)
      (let ((text (mcp::primer-text)))
        (is (search "probe: echo fail late" text) "tier 2 lists names per server")
        (is (search "(mcp:tools \"probe\") lists the arguments" text) "and points at (mcp:tools)")
        (is (null (search "(mcp:probe/gen-3" text)) "without the call forms"))
      (setf mcp::*primer-budget* (+ (length mcp::+preamble+) 10))
      (mcp::refresh-primer)
      (is (search "probe: 50 tools - (mcp:tools \"probe\")" (mcp::primer-text))))))

(deftest mcp-cell-manual-is-a-topic-and-every-session-carries-its-line (with-temp-store ())
  (ensure-durable-session "mcp-primer")
  (nlk:set-harness-section "mcp-primer" "soul" "be kind")
  (with-ready-mcp ("probe" (mcp-fixture-entry))
    (is (search "(mcp:probe/echo" (nle:help :mcp)) "(help :mcp) carries the catalog")
    (let* ((sections (nle::read-harness-sections "mcp-primer"))
           (prompt (nle::harness-overlay-prompt "BASE" sections)))
      (is (equal "be kind" (cdr (assoc "soul" sections :test #'string=))) "the durable rows stand")
      ;; the help section names the servers
      (is (search "  :mcp - mcp:SERVER/TOOL calls a tool of a configured MCP server: probe"
                  (cdr (assoc "help" sections :test #'string=))))
      (is (null (search "(mcp:probe/echo" prompt)) "and never carries the catalog"))
    (funcall stop)
    (setf stop nil)
    (is (null (assoc :mcp nle::*help-topics*)) "stop takes the topic off")))

(deftest mcp-cell-primer-absent-without-servers (with-mcp-log-dir)
  (with-cell-stop ((mcp:start-cell (mcp-config)))
    (is (null mcp::*registry*) "no servers, no registry")
    (is (null (mcp::primer-text)) "no manual")
    (is (null (assoc :mcp nle::*help-topics*)) "no topic")))
