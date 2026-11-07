;;;; rpc-test.lisp --- the JSON-RPC client over the in-memory transport.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun mcp-reply (id result-json)
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"result\":~a}" id result-json))

(deftest mcp-cell-rpc-request-matches-its-id ()
  (multiple-value-bind (client transport) (mcp-memory-client)
    (let ((result (mcp::rpc-request client "initialize" nil (mcp::deadline-after 5000))))
      (is (equal "2025-06-18" (nlk:json-value result :string "protocolVersion")))
      (let ((sent (first (mcp-sent-messages transport))))
        (is-shape sent ("id" = 1 "ids start at 1") ("method" "initialize" "the method rides")
          ("jsonrpc" "2.0" "as JSON-RPC 2.0"))))
    (mcp::rpc-request client "tools/list" nil (mcp::deadline-after 5000))
    (is (= 2 (gethash "id" (second (mcp-sent-messages transport)))) "ids are monotonic")))

(deftest mcp-cell-rpc-skips-stale-replies-and-counts-them ()
  (let* ((transport (make-mcp-memory-transport
                     :inbox (list (mcp-reply 7 "{\"late\":true}")
                                  (mcp-reply 1 "{\"mine\":true}"))))
         (client (mcp::make-client :transport transport)))
    (let ((result (mcp::rpc-request client "x" nil (mcp::deadline-after 5000))))
      (is (eq t (nlk:json-value result :boolean "mine")) "the matching reply wins")
      (is (= 1 (mcp::client-stale-count client)) "the late one was counted"))))

(deftest mcp-cell-rpc-answers-server-requests-inline ()
  (let* ((transport (make-mcp-memory-transport
                     :inbox (list "{\"jsonrpc\":\"2.0\",\"id\":\"s1\",\"method\":\"ping\"}"
                                  "{\"jsonrpc\":\"2.0\",\"id\":\"s2\",\"method\":\"roots/list\"}"
                                  "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\",\"params\":{}}"
                                  "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/tools/list_changed\"}"
                                  (mcp-reply 1 "{}"))))
         (client (mcp::make-client :transport transport)))
    (mcp::rpc-request client "x" nil (mcp::deadline-after 5000))
    (let ((sent (mcp-sent-messages transport)))
      (is (= 3 (length sent)) "the request, a pong, a -32601")
      (let ((pong (second sent)) (refusal (third sent)))
        (is (equal "s1" (gethash "id" pong)) "ping answered by id")
        (is (hash-table-p (gethash "result" pong)) "with an empty result")
        (is (equal "s2" (gethash "id" refusal)) "roots/list answered by id")
        (is (= -32601 (nlk:json-value refusal :number "error" "code")))))
    (is (mcp::client-list-changed-p client) "list_changed marked the catalog stale")))

(deftest mcp-cell-rpc-error-reply-and-timeout-and-close ()
  (let* ((transport (make-mcp-memory-transport
                     :inbox (list "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"bad params\"}}")))
         (client (mcp::make-client :transport transport)))
    (let ((condition (signals-error mcp::rpc-error
                       (mcp::rpc-request client "x" nil (mcp::deadline-after 5000)))))
      (is (= -32602 (mcp::rpc-error-code condition)) "an error member carries the code")
      (is (search "bad params" (princ-to-string condition)) "and the message"))
    (is (signals-error mcp::rpc-timeout
          (mcp::rpc-request client "x" nil (mcp::deadline-after 5000))))
    (setf (mcp-memory-transport-inbox transport) (list :closed))
    (is (signals-error mcp::transport-closed
          (mcp::rpc-request client "x" nil (mcp::deadline-after 5000))))))

(deftest mcp-cell-rpc-handshake-and-catalog (multiple-value-bind (client transport))
  (mcp-memory-client :tools (vector (nlk:json-object "name" "echo"
                                                     "description" "d"
                                                     "inputSchema" (nlk:json-object "type" "object"))
                                    (nlk:json-object "noname" t)))
(mcp::handshake client (mcp::deadline-after 5000))
(is (equal "memory" (nlk:json-value (mcp::client-server-info client) :string "name")))
(is (equal "2025-06-18" (mcp::client-protocol-version client)) "so is the protocol version")
(let ((sent (mcp-sent-messages transport)))
  (is (equal "notifications/initialized" (gethash "method" (second sent))))
  (is (null (nth-value 1 (gethash "id" (second sent)))) "as a notification")
  (is (equal "nodecode"
             (nlk:json-value (first sent) :string "params" "clientInfo" "name"))))
(let ((tools (mcp::list-tools client (mcp::deadline-after 5000))))
  (is (= 1 (length tools)) "a tool without a name is dropped")
  (is (equal "echo" (getf (first tools) :name)) "the named one is kept")
  (is (hash-table-p (getf (first tools) :schema)) "with its schema")))

(deftest mcp-cell-rpc-pagination-and-caps ()
  (let* ((pages 0)
         (transport (make-mcp-memory-transport
                     :responder
                     (lambda (message)
                       (let ((id (gethash "id" message))
                             (cursor (nlk:json-value message :string "params" "cursor")))
                         (incf pages)
                         (list (nlk:json-object
                                "jsonrpc" "2.0" "id" id
                                "result" (nlk:json-object
                                          "tools" (vector (nlk:json-object
                                                           "name" (format nil "t~d" pages)))
                                          :opt "nextCursor" (and (null cursor) "page2"))))))))
         (client (mcp::make-client :transport transport))
         (tools (mcp::list-tools client (mcp::deadline-after 5000))))
    (is (= 2 pages) "nextCursor was followed once")
    (is (equal '("t1" "t2") (mapcar (lambda (tool) (getf tool :name)) tools))))
  (let* ((transport (make-mcp-memory-transport
                     :responder
                     (lambda (message)
                       (list (nlk:json-object
                              "jsonrpc" "2.0" "id" (gethash "id" message)
                              "result" (nlk:json-object
                                        "tools" (coerce (loop for n below 70
                                                              collect (nlk:json-object
                                                                       "name" (format nil "g~d" n)))
                                                        'vector)))))))
         (client (mcp::make-client :transport transport))
         (tools (let ((*error-output* (make-broadcast-stream)))
                  (handler-bind ((warning #'muffle-warning))
                    (mcp::list-tools client (mcp::deadline-after 5000))))))
    (is (= mcp::+max-tools-per-server+ (length tools)) "the catalog is capped")))

(deftest mcp-cell-rpc-frame-limit-on-send (let* ((transport (make-mcp-memory-transport))
                                                  (client (mcp::make-client :transport transport))))
  (is (signals-error mcp::transport-closed
        (mcp::send-message client (nlk:json-object "blob" (make-string (* 1024 1100)
                                                                       :initial-element #\a))
                           (mcp::deadline-after 5000)))))
