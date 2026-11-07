;;;; support.lisp --- MCP cell test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; MCP tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under an MCP-CELL-
;;;; name prefix; RUN-MCP-TESTS runs exactly that slice, so this system's
;;;; test-op never re-runs the core suite and `just test' never runs
;;;; these. Helper names carry an MCP- prefix: the registry package is
;;;; shared with every other cell's tests.
;;;;
;;;; Three fixtures: an in-memory transport (scripted inbound messages, a
;;;; responder function, captured outbound frames) for the JSON-RPC client;
;;;; the python stdio server in this directory for the process path; and a
;;;; clack app on an ephemeral port for streamable HTTP. Every test that
;;;; starts the cell points *LOG-DIRECTORY* at a temp dir (SETF, not LET:
;;;; connect threads read it) and restores the registry variables on unwind.

(in-package #:nodecode.test)

(nlk:access (server mcp::server))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(define-test-slice "mcp" "MCP-CELL-")

;;; --- small helpers --------------------------------------------------------

(defun mcp-entry (&rest pairs)
  "One mcp.servers entry as shasht would decode it."
  (apply #'nlk:make-json-object pairs))

(defun mcp-section (&rest name-entry-pairs)
  "An mcp section holding the named entries."
  (nlk:json-object "servers" (apply #'nlk:make-json-object name-entry-pairs)))

(defun mcp-config (&rest name-entry-pairs)
  "A whole shared config with one mcp section."
  (nlk:json-object "mcp" (apply #'mcp-section name-entry-pairs)))

(defun mcp-fixture-entry (&rest flags)
  "A stdio entry running the fixture with FLAGS and a short timeout."
  (mcp-entry "command" "python3"
             "args" (coerce (cons (namestring (asdf:system-relative-pathname
                                               "nodecode-mcp" "test/fixture-server.py"))
                                  flags)
                            'vector)
             "timeout_ms" 3000))

(defmacro with-mcp-log-dir (&body body)
  "BODY with the stderr logs under a fresh temp directory; registry variables restored."
  `(with-temp-directory (directory "mcp-logs")
     (with-saved-globals (mcp::*log-directory* mcp::*registry* nle::*help-topics*
                          nle::*registered-commands* nle:*hooks*)
       (setf mcp::*log-directory* directory)
       ,@body)))

(defmacro with-mcp-cell ((&rest config-forms) &body body)
  "START-CELL over a config built from CONFIG-FORMS (name entry pairs);
stop on unwind; registry variables restored. STOP is bound to the thunk."
  `(with-mcp-log-dir
     (with-cell-stop ((mcp:start-cell (mcp-config ,@config-forms)))
       ,@body)))

(defmacro with-ready-mcp ((name entry) &body body)
  "WITH-MCP-CELL over the one server NAME configured by ENTRY, BODY run
once its connect has settled."
  `(with-mcp-cell (,name ,entry)
     (mcp-wait-ready ,name)
     ,@body))

(defun mcp-server (name)
  (mcp::find-server name))

(defun mcp-wait-ready (name &key (timeout 10) &aux (server (mcp-server name)))
  "Wait for NAME's connect to settle (state published, functions and
primer in place); its final state."
  (await (:timeout timeout)
    (let ((state server.state))
      (and (not (member state '(:connecting :disconnected))) server.settled-p state))))

(defun mcp-slash (args &optional (session-id "s1"))
  "Drive /mcp ARGS as every caller does: the answer's text."
  (values (cell-entry "nodecode-mcp" "mcp" args session-id)))

;;; --- the in-memory transport ----------------------------------------------

(defstruct (mcp-memory-transport (:copier nil))
  "Scripted inbound (messages, or :CLOSED), a RESPONDER called with each
decoded outbound message to produce further inbound ones, and every frame
sent, oldest first."
  (inbox '() :type list)
  (responder nil)
  (sent '() :type list)
  (closed-p nil))

(nlk:access (transport mcp-memory-transport))

(defmethod mcp::transport-send ((transport mcp-memory-transport) text deadline)
  (declare (ignore deadline))
  (when transport.closed-p (error 'mcp::transport-closed :detail "memory transport closed"))
  (setf transport.sent (append transport.sent (list text)))
  (nlk:when-let (responder transport.responder)
    (let ((replies (funcall responder (cell-json text))))
      (setf transport.inbox (append transport.inbox (if (listp replies) replies (list replies)))))))

(defmethod mcp::transport-receive ((transport mcp-memory-transport) deadline)
  (declare (ignore deadline))
  (cond ((mcp-memory-transport-closed-p transport) :closed)
        ((mcp-memory-transport-inbox transport)
         (let ((item (pop transport.inbox))) (if (stringp item) (cell-json item) item)))
        (t :timeout)))

(defmethod mcp::transport-close ((transport mcp-memory-transport)) (setf transport.closed-p t))

(defun mcp-sent-messages (transport)
  (mapcar #'cell-json transport.sent))

(defun mcp-fake-answer (method name tools)
  "What a fake server answers METHOD with, as (KEY VALUE): initialize as the
server NAME, tools/list over TOOLS, anything else method-not-found."
  (cond ((equal method "initialize")
         (list "result" (nlk:json-object
                         "protocolVersion" "2025-06-18"
                         "serverInfo" (nlk:json-object "name" name "version" "1"))))
        ((equal method "tools/list") (list "result" (nlk:json-object "tools" tools)))
        (t (list "error" (nlk:json-object "code" -32601 "message" "nope")))))

(defun mcp-memory-client (&key (tools #()))
  "(values CLIENT TRANSPORT) over a fresh memory transport whose responder
implements initialize and tools/list over TOOLS (a vector of tool objects)."
  (let* ((transport
           (make-mcp-memory-transport
            :responder (lambda (message &aux (id (gethash "id" message)))
                         (when id
                           (list (apply #'nlk:make-json-object "jsonrpc" "2.0" "id" id
                                        (mcp-fake-answer (nlk:json-value message :string "method")
                                                         "memory" tools)))))))
         (client (mcp::make-client :transport transport)))
    (values client transport)))

;;; --- the HTTP fixture -------------------------------------------------------

(defstruct (mcp-http-fixture (:copier nil))
  port
  handler
  (requests '() :type list)             ; (method headers message) newest first
  (lock (bt2:make-lock :name "mcp-http-fixture"))
  require-header                        ; (name . value) the POST must carry
  session-id                            ; issued on initialize, required after
  (sse-p nil)                           ; answer tools/call with an event stream
  (deletes 0 :type integer))

(nlk:access (fixture mcp-http-fixture))

(defun mcp-fixture-requests (fixture)
  (bt2:with-lock-held ((mcp-http-fixture-lock fixture)) (reverse fixture.requests)))

(defun mcp-fixture-reply (fixture env message)
  "The clack response for one decoded POST MESSAGE."
  (let* ((id (gethash "id" message))
         (method (nlk:json-value message :string "method"))
         (headers (getf env :headers))
         (session fixture.session-id)
         (session-headers (when session (list :mcp-session-id session))))
    (flet ((reply (key result)
             (list 200 (append (list :content-type "application/json") session-headers)
                   (list (nlk:encode-json-object
                          (nlk:make-json-object "jsonrpc" "2.0" "id" id key result))))))
      (cond
        ((null id) (list 202 '(:content-type "text/plain") '("")))
        ((and session (not (equal method "initialize"))
              (not (equal (gethash "mcp-session-id" headers) session)))
         (list 404 '(:content-type "text/plain") '("no such session")))
        ((equal method "tools/call")
         (let* ((params (nlk:json-value message :object "params"))
                (text (format nil "echo: ~a"
                              (nlk:json-value params :string "arguments" "message")))
                (result (nlk:json-object
                         "content" (vector (nlk:json-object "type" "text" "text" text)))))
           (if fixture.sse-p
               (list 200
                     (append (list :content-type "text/event-stream") session-headers)
                     (list (format nil "event: message~%data: ~a~%~%event: message~%data: ~a~%~%"
                                   (nlk:encode-json-object
                                    (nlk:json-object "jsonrpc" "2.0"
                                                     "method" "notifications/message"
                                                     "params" (nlk:json-object "level" "info"
                                                                               "data" "first")))
                                   (nlk:encode-json-object
                                    (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))))
               (reply "result" result))))
        (t (apply #'reply (mcp-fake-answer
                           method "http-fixture"
                           (vector (nlk:json-object
                                    "name" "echo"
                                    "description" "Echo over HTTP"
                                    "inputSchema" (nlk:json-object
                                                   "type" "object"
                                                   "properties" (nlk:json-object
                                                                 "message" (nlk:json-object "type" "string"))
                                                   "required" (vector "message")))))))))))

(defun mcp-fixture-app (fixture)
  (lambda (env &aux (method (getf env :request-method))
                    (headers (getf env :headers)))
    (handler-case
        (cond
          ((eq method :delete)
           (bt2:with-lock-held ((mcp-http-fixture-lock fixture)) (incf fixture.deletes))
           (list 200 '(:content-type "text/plain") '("bye")))
          ((not (eq method :post))
           (list 405 '(:content-type "text/plain") '("post only")))
          (t
           (let* ((length (or (getf env :content-length) 0))
                  (buffer (make-array length :element-type '(unsigned-byte 8)))
                  (message (progn (when (and (getf env :raw-body) (plusp length))
                                    (read-sequence buffer (getf env :raw-body)))
                                  (cell-json (flexi-streams:octets-to-string
                                               buffer :external-format :utf-8))))
                  (required fixture.require-header))
             (bt2:with-lock-held ((mcp-http-fixture-lock fixture))
               (push (list method headers message) fixture.requests))
             (if (and required
                      (not (equal (gethash (car required) headers) (cdr required))))
                 (list 401 '(:content-type "text/plain") '("unauthorized"))
                 (mcp-fixture-reply fixture env message)))))
      (error (condition)
        (list 500 '(:content-type "text/plain")
              (list (format nil "fixture error: ~a" condition)))))))

(defun mcp-start-http-fixture (&key require-header session-id sse-p)
  "A clack app on an ephemeral port speaking enough streamable HTTP for the
tests; NLE:STOP-CLACK-HANDLER on the fixture's handler takes it down."
  (let ((fixture (make-mcp-http-fixture :require-header require-header
                                        :session-id session-id
                                        :sse-p sse-p)))
    (setf (values fixture.handler fixture.port) (nle:serve-local (mcp-fixture-app fixture)))
    fixture))

(defun mcp-http-entry (fixture &rest pairs)
  "An http entry pointing at FIXTURE, with extra PAIRS."
  (apply #'mcp-entry "url" (format nil "http://127.0.0.1:~d/mcp" fixture.port)
         "timeout_ms" 3000 pairs))
