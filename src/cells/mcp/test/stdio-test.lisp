;;;; stdio-test.lisp --- the process transport against the python fixture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The whole cell is started over one fixture server per test; the
;;;; generated function is reached through its symbol name because the
;;;; symbol is interned only when the catalog arrives.

(in-package #:nodecode.test)

(defun mcp-probe-process (&aux (client (mcp::server-client (mcp-server "probe"))))
  "The fixture child behind the probe server, or NIL."
  (and client (mcp::stdio-transport-pid (mcp::client-transport client))))

(deftest mcp-cell-stdio-connects-lists-and-calls (with-mcp-cell ("probe" (mcp-fixture-entry)))
  (is (functionp stop) "start-cell returns a stop thunk")
  (is (eq :ready (mcp-wait-ready "probe")) "the fixture connects")
  (let ((snapshot (mcp::server-snapshot (mcp-server "probe"))))
    (is (= 10 (getf snapshot :tool-count)) "the catalog is listed")
    (is (equal "fixture" (nlk:json-value (getf snapshot :server-info) :string "name"))))
  (let ((answer (mcp:call "probe" "echo" :message "hi")))
    (is (uiop:string-prefix-p "echo: hi" answer) "a call answers the text blocks"))
  (let ((symbol (find-symbol "PROBE/ECHO" :nodecode-mcp)))
    (is (and symbol (fboundp symbol)) "the tool is a function")
    (is (eq :external (nth-value 1 (find-symbol "PROBE/ECHO" :nodecode-mcp))))
    (let ((answer (funcall symbol :message "x" :per-page 5 :include-snapshot t)))
      (is (search "\"per_page\": 5" answer) ":per-page folds to per_page")
      (is (search "\"includeSnapshot\": true" answer) ":include-snapshot folds to includeSnapshot")
      (is (search "\"message\": \"x\"" answer) "and the message rides")))
  (let ((answer (mcp:call "probe" "echo" "message" "s" "odd_key" 1)))
    (is (search "\"odd_key\": 1" answer) "string keys pass verbatim"))
  (is (search "echo: folded" (mcp:call "probe" "Echo" :message "folded"))))

(deftest mcp-cell-stdio-tool-error-and-unknown-tool (with-ready-mcp ("probe" (mcp-fixture-entry)))
  (let ((condition (signals-error mcp:mcp-tool-error (mcp:call "probe" "fail"))))
    (is (search "probe/fail: echo refused: fail requested" (princ-to-string condition))))
  (is (search "echo" (refusal-text mcp:mcp-unknown-tool (mcp:call "probe" "nonesuch"))))
  (is (signals-error mcp:mcp-unknown-server (mcp:call "ghost" "echo")))
  (is (eq :ready (mcp::server-state (mcp-server "probe"))) "none of that cost the connection"))

(deftest mcp-cell-stdio-timeout-keeps-the-connection-and-skips-the-late-reply ()
  (with-ready-mcp ("probe" (mcp-fixture-entry))
    (is (search "connection is kept"
                (refusal-text mcp:mcp-timeout (mcp:call "probe" "late" :ms 800 :timeout 0.3))))
    (is (eq :ready (mcp::server-state (mcp-server "probe"))) "the server stays ready")
    (is (uiop:string-prefix-p "echo: again" (mcp:call "probe" "echo" :message "again")))
    (is (= 1 (mcp::client-stale-count (mcp::server-client (mcp-server "probe")))))))

(deftest mcp-cell-stdio-death-then-lazy-reconnect (with-ready-mcp ("probe" (mcp-fixture-entry)))
  (let ((first-process (mcp-probe-process))
        (condition (signals-error mcp:mcp-offline (mcp:call "probe" "die"))))
    (is (search ".stderr.log" (princ-to-string condition)))
    (is (eq :error (mcp::server-state (mcp-server "probe"))) "the server is in error")
    (is (uiop:string-prefix-p "echo: back" (mcp:call "probe" "echo" :message "back")))
    (is (eq :ready (mcp::server-state (mcp-server "probe"))) "and the server is ready again")
    (is (not (eq first-process (mcp-probe-process))) "on a fresh process")
    (is (search "dying on request"
                (uiop:read-file-string (mcp::stderr-log-path "probe"))))))

(deftest mcp-cell-stdio-garbage-line-server-requests-and-list-changed ()
  (with-ready-mcp ("probe" (mcp-fixture-entry))
    (is (equal "exploded and survived"
               (handler-bind ((warning #'muffle-warning))
                 (mcp:call "probe" "explode"))))
    (is (equal "roots answered: -32601" (mcp:call "probe" "roots")))
    (is (equal "ping answered: {}" (mcp:call "probe" "ping_me")))
    (is (equal "changed" (mcp:call "probe" "changed")) "the changed tool answers")
    (is (= 11 (length (mcp::server-tools (mcp-server "probe")))))
    (let ((symbol (find-symbol "PROBE/ECHO2" :nodecode-mcp)))
      (is (and symbol (fboundp symbol)) "the new tool is a function"))
    (is (search "probe/echo2" (mcp::primer-text)) "and the primer names it")))

(deftest mcp-cell-stdio-handshake-timeout-fails-the-server ()
  (with-mcp-cell ("slow" (let ((entry (mcp-fixture-entry "--slow-init")))
                            (setf (gethash "timeout_ms" entry) 500)
                            entry))
    (is (eq :error (mcp-wait-ready "slow" :timeout 10)) "a slow initialize ends in error")
    (is (search "initialize" (mcp::server-error-text (mcp-server "slow"))))
    (is (null (mcp::server-client (mcp-server "slow"))) "with the child closed")))

(deftest mcp-cell-stdio-pages-and-stderr-and-stop ()
  (with-ready-mcp ("probe" (mcp-fixture-entry "--pages" "--stderr-noise"))
    (is (= 10 (length (mcp::server-tools (mcp-server "probe")))))
    (is (search "hello from stderr"
                (uiop:read-file-string (mcp::stderr-log-path "probe"))))
    (let ((pid (mcp-probe-process)))
      (is (nlk:pid-live-p pid) "the child is alive while running")
      (funcall stop)
      (setf stop nil)
      (is (not (nlk:pid-live-p pid)) "stop leaves no live child, reaped")
      (is (null mcp::*registry*) "and no registry")
      (is (not (fboundp (find-symbol "PROBE/ECHO" :nodecode-mcp)))))))
