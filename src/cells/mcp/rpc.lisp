;;;; rpc.lisp --- the JSON-RPC client, neutral over its transport.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; MCP is JSON-RPC 2.0 with one request in flight per connection from this
;;;; side. A transport is three generics: send a frame, receive the next
;;;; decoded message before a deadline, close. stdio.lisp and http.lisp
;;;; implement them; the tests add an in-memory one.
;;;;
;;;; Synchronous by design, no reader thread: a request writes its frame and
;;;; then reads on the calling thread until its own id answers. What arrives
;;;; meanwhile is handled inline — a server-initiated request (ping, and the
;;;; roots/sampling/elicitation family this client does not offer) is
;;;; answered at once, a notification is noted or dropped, and a reply to
;;;; some earlier id is a late answer to a request that already timed out,
;;;; skipped. Ids are monotonic per connection, so "not my id" is exactly
;;;; "stale". The cost, documented: between calls nothing reads, so a server
;;;; ping or a tools/list_changed is only seen while a call is reading.
;;;;
;;;; Deadlines, not timeouts: every wait takes the absolute deadline of the
;;;; call it serves, so a call that skips three stale replies still ends
;;;; when its caller said. A deadline that passes keeps the connection (the
;;;; late reply will be skipped by the next call); a closed transport does
;;;; not, and the registry reconnects once on the next call.
;;;;
;;;; Conditions here are internal; surface.lisp maps them to the MCP-*
;;;; family the model reads. Every handler in this file catches ERROR only:
;;;; (eval-interrupt) lands on the evaluating thread as a SERIOUS-CONDITION
;;;; that must pass through to its own handler.

(in-package #:nodecode-mcp)

(defparameter +protocol-version+ "2025-06-18")
(defparameter +frame-limit+ (* 1024 1024)
  "Largest frame read or posted, in characters (the Zig frame_limit).")
(defparameter +max-list-pages+ 16
  "tools/list pages followed before the catalog is taken as complete.")
(defparameter +max-tools-per-server+ 64
  "Tools kept per server; the rest are dropped with one warning.")

;;; --- internal conditions --------------------------------------------------

(nlk:define-error rpc-error (error) (code message (data :initform nil))
  (:report "code ~a: ~a" code message)
  ;; The connection is fine; the call is not.
  (:documentation "The server answered with a JSON-RPC error object."))

(nlk:define-error rpc-timeout (error) (detail)
  (:report "~a" detail)
  (:documentation "The deadline passed while waiting; connection kept."))

(nlk:define-error transport-closed (error) (detail)
  (:report "~a" detail)
  (:documentation "The transport is unusable: EOF, a dead child, a refused
socket, an oversize frame. The registry drops the connection."))

;;; --- deadlines ------------------------------------------------------------

(defun now-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun deadline-after (milliseconds)
  "An absolute deadline MILLISECONDS from now."
  (+ (now-seconds) (/ milliseconds 1000)))

(defun seconds-remaining (deadline)
  "Seconds until DEADLINE as a real, never negative."
  (max 0 (- deadline (now-seconds))))

;;; --- the transport protocol -----------------------------------------------

(defgeneric transport-send (transport text deadline)
  ;; Signals TRANSPORT-CLOSED when the
  ;; transport cannot carry it. DEADLINE bounds a transport whose send blocks
  ;; (HTTP: the whole exchange).
  (:documentation "Write one frame TEXT."))

(defgeneric transport-receive (transport deadline)
  ;; Signals TRANSPORT-CLOSED on a frame it cannot carry.
  (:documentation "The next decoded message (an EQUAL hash table), or
:TIMEOUT when DEADLINE passes first, or :CLOSED at EOF."))

(defgeneric transport-close (transport)
  (:documentation "Release everything; idempotent; never blocks past a few
seconds."))

;;; --- the client -----------------------------------------------------------

(nlk:define-record (client (:copier nil))
  "One connection: its transport and the request counter."
  transport
  (next-id 0 :type integer)
  (list-changed-p nil)
  (stale-count 0 :type integer)
  server-info
  protocol-version)

(defun check-frame (text)
  "Refuse TEXT when it is past +FRAME-LIMIT+: the frame is not sent."
  (when (> (length text) +frame-limit+)
    (error 'transport-closed
           :detail (format nil "frame of ~:d characters exceeds the 1 MiB limit"
                           (length text)))))

(defun send-message (client message deadline &aux (text (nlk:encode-json-object message)))
  (check-frame text)
  (transport-send client.transport text deadline))

(defun rpc-send-request (client method params deadline &aux (id (incf client.next-id)))
  "Write METHOD's request frame; returns the id to await."
  (send-message client
                (nlk:json-object "jsonrpc" "2.0"
                                 "id" id
                                 "method" method
                                 "params" (or params (nlk:make-json-object)))
                deadline)
  id)

(defun rpc-await-reply (client id method deadline)
  "Read until ID answers; its result. METHOD names the wait in a timeout."
  (loop
    (let ((message (transport-receive client.transport deadline)))
      (case message
        (:closed (error 'transport-closed :detail "connection closed"))
        (:timeout (error 'rpc-timeout
                         :detail (format nil "no answer to ~a within the deadline"
                                         method)))
        (t
         (let ((inbound (nlk:json-value message :string "method"))
               (reply-id (gethash "id" message)))
           (cond
             ((and inbound (nth-value 1 (gethash "id" message)))
              ;; A server request: ping answers empty, anything else -32601.
              (send-message
               client
               (nlk:json-object "jsonrpc" "2.0" "id" reply-id
                                :when (equal inbound "ping") "result" (nlk:make-json-object)
                                :when (not (equal inbound "ping"))
                                "error" (nlk:json-object
                                         "code" -32601
                                         "message" (format nil "client method ~a not supported" inbound)))
               deadline))
             (inbound
              ;; tools/list_changed marks the catalog stale; other notifications drop.
              (when (equal inbound "notifications/tools/list_changed")
                (setf client.list-changed-p t)))
             ((not (and (numberp reply-id) (= reply-id id)))
              (incf client.stale-count))
             ((nth-value 1 (gethash "error" message))
              (let ((error (gethash "error" message)))
                (error 'rpc-error
                       :code (or (nlk:json-value error :number "code") 0)
                       :message (or (nlk:json-value error :string "message")
                                    "unspecified error")
                       :data (and (hash-table-p error) (gethash "data" error)))))
             (t (return (gethash "result" message))))))))))

(defun rpc-request (client method params deadline)
  "Send METHOD with PARAMS and return its result, reading until DEADLINE."
  (rpc-await-reply client (rpc-send-request client method params deadline)
                   method deadline))

;;; --- the MCP lifecycle ----------------------------------------------------

(defun handshake (client deadline)
  "initialize, then notifications/initialized."
  ;; Returns the initialize result; the server's info and protocol version are
  ;; kept on the client.
  (let ((result (rpc-request
                 client "initialize"
                 (nlk:json-object
                  "protocolVersion" +protocol-version+
                  "capabilities" (nlk:make-json-object)
                  "clientInfo" (nlk:json-object "name" "nodecode"
                                                "version" "0.1.0"))
                 deadline)))
    (setf client.server-info (nlk:json-value result :object "serverInfo")
          client.protocol-version
          (nlk:json-value result :string "protocolVersion"))
    (send-message client (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized"
                                          "params" (nlk:make-json-object))
                  deadline)
    result))

(defun list-tools (client deadline)
  "The server's catalog as a list of tool plists, following nextCursor up
to +MAX-LIST-PAGES+ pages and keeping +MAX-TOOLS-PER-SERVER+ entries."
  (let ((tools
          (loop for pages from 1
                for cursor = nil then next
                for result = (rpc-request client "tools/list"
                                          (and cursor (nlk:json-object "cursor" cursor)) deadline)
                for next = (nlk:json-value result :text "nextCursor")
                ;; Each entry as (:name :description :schema); one without a name is skipped.
                append (loop for object across (nlk:json-array result "tools")
                             for name = (nlk:json-value object :text "name")
                             for description = (or (nlk:json-value object :string "description") "")
                             for schema = (or (nlk:json-value object :object "inputSchema")
                                              (nlk:json-value object :object "input_schema"))
                             when name
                               collect (list :name name :description description
                                             :schema (or schema (nlk:make-json-object))))
                until (or (null next) (>= pages +max-list-pages+)))))
    (when (> (length tools) +max-tools-per-server+)
      (warn "mcp: keeping ~d of ~d tools (the cap)"
            +max-tools-per-server+ (length tools))
      (setf tools (subseq tools 0 +max-tools-per-server+)))
    (setf client.list-changed-p nil)
    tools))
