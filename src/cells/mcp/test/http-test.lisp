;;;; http-test.lisp --- streamable HTTP against a local clack fixture.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defmacro with-mcp-http-fixture ((var &rest options) &body body)
  `(let ((,var (mcp-start-http-fixture ,@options)))
     (unwind-protect (progn ,@body)
       (nle:stop-clack-handler (mcp-http-fixture-handler ,var)))))

(deftest mcp-cell-http-json-round-trip (with-mcp-http-fixture (fixture))
  (with-mcp-cell ("remote" (mcp-http-entry fixture))
    (is (eq :ready (mcp-wait-ready "remote")) "the http server connects")
    (is (equal "echo: hi" (mcp:call "remote" "echo" :message "hi")))
    (let ((requests (mcp-fixture-requests fixture)))
      (is (equal '("initialize" "notifications/initialized" "tools/list" "tools/call")
                 (mapcar (lambda (request) (gethash "method" (third request))) requests)))
      (let ((headers (second (fourth requests))))
        (is (equal "2025-06-18" (gethash "mcp-protocol-version" headers)))
        (is (search "text/event-stream" (gethash "accept" headers)))))))

(deftest mcp-cell-http-sse-reply-after-a-notification (with-mcp-http-fixture (fixture :sse-p t))
  (with-ready-mcp ("remote" (mcp-http-entry fixture))
    (is (equal "echo: streamed" (mcp:call "remote" "echo" :message "streamed")))))

(deftest mcp-cell-http-session-id-is-echoed-and-deleted ()
  (with-mcp-http-fixture (fixture :session-id "sess-42")
    (with-ready-mcp ("remote" (mcp-http-entry fixture))
      (is (equal "echo: s" (mcp:call "remote" "echo" :message "s")) "the session call answers")
      (let ((requests (mcp-fixture-requests fixture)))
        (is (null (gethash "mcp-session-id" (second (first requests)))))
        (is (equal "sess-42" (gethash "mcp-session-id" (second (third requests))))))
      (funcall stop)
      (setf stop nil)
      (is (= 1 (mcp-http-fixture-deletes fixture)) "stop DELETEs the session"))))

(deftest mcp-cell-http-refused-status-names-only-the-code ()
  (with-mcp-http-fixture (fixture :require-header '("authorization" . "Bearer right"))
    (with-mcp-cell ("remote" (mcp-http-entry fixture
                                              "headers" (mcp-entry "Authorization" "Bearer wrong-secret")))
      (is (eq :error (mcp-wait-ready "remote")) "a 401 fails the connect")
      (let ((text (mcp::server-error-text (mcp-server "remote"))))
        (is (search "HTTP 401" text) "the status is named")
        (is (null (search "wrong-secret" text)) "the header value is not")))
    (with-mcp-cell ("remote" (mcp-http-entry fixture
                                              "headers" (mcp-entry "Authorization" "Bearer right")))
      (is (eq :ready (mcp-wait-ready "remote")) "the right header connects"))))

(deftest mcp-cell-http-expired-session-reconnects ()
  (with-mcp-http-fixture (fixture :session-id "one")
    (with-ready-mcp ("remote" (mcp-http-entry fixture))
      (setf (mcp-http-fixture-session-id fixture) "two")
      (is (search "session expired"
                  (refusal-text mcp:mcp-offline (mcp:call "remote" "echo" :message "x"))))
      (is (equal "echo: y" (mcp:call "remote" "echo" :message "y"))))))

(deftest mcp-cell-mcp-test-a-post-that-awaits-no-answer-reads-no-stream ()
  ;; DeepWiki's 202 to the initialized notification drops its TLS without a
  ;; close_notify, and a streamed body read fails the handshake.
  (is (mcp::answer-awaited-p "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}") "a request awaits its answer")
  (is (not (mcp::answer-awaited-p "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")) "a notification does not")
  (is (not (mcp::answer-awaited-p "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{}}")) "nor a reply to the server")
  (with-mcp-http-fixture (fixture :session-id "sess-9")
    (with-ready-mcp ("remote" (mcp-http-entry fixture))
      (is (equal "echo: after" (mcp:call "remote" "echo" :message "after")) "the session carries on past the notification"))))
