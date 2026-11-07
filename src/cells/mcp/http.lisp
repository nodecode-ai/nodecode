;;;; http.lisp --- the streamable HTTP transport.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; MCP's streamable HTTP: every JSON-RPC message is POSTed to the one
;;;; endpoint URL; the server answers a request's POST with either one JSON
;;;; message or an SSE body that eventually carries the reply (notifications
;;;; and server requests may precede it); a notification's POST gets 202 and
;;;; no body. The Mcp-Session-Id header the initialize response issues is
;;;; echoed on every later request and DELETEd on close. The legacy
;;;; two-endpoint SSE transport is not implemented.
;;;;
;;;; Shape over the transport protocol: a send does the whole exchange and
;;;; queues whatever messages the response carried; a receive pops the
;;;; queue. So a request's reply is already waiting when RPC-REQUEST reads,
;;;; and an empty queue is "no answer in the response" — the deadline
;;;; condition. The SSE body is read on this thread under the socket's read
;;;; timeout (what remains of the deadline) and abandoned as soon as the
;;;; reply is in hand, so a server that keeps the stream open costs nothing.
;;;; A notification's or a reply's POST, which awaits no answer, reads its
;;;; response whole by its length instead. Without this, an empty 202 with
;;;; Content-Length 0, Connection: close, and a TLS drop without
;;;; close_notify would fail as "unexpected eof while reading", because
;;;; dexador's stream fills a buffer from the socket before a byte is asked
;;;; for.
;;;;
;;;; Header values are secrets and never appear in a condition: a failed
;;;; status reports the code only.

(in-package #:nodecode-mcp)

(defparameter *close-timeout-seconds* 5)

(defstruct (http-transport (:copier nil))
  url
  (headers '() :type list)              ; configured, ((name . value) ...)
  session-id
  protocol-version
  (pending '() :type list)              ; decoded messages not yet received
  (closed-p nil))

(nlk:access (transport http-transport))

(defun request-headers (transport)
  (append `(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . ,(or (http-transport-protocol-version transport)
                                           +protocol-version+)))
          (nlk:when-let (session transport.session-id) `(("mcp-session-id" . ,session)))
          transport.headers))

(defun header-value (headers name)
  "NAME's value from a dexador header table: both its backends lowercase every name."
  (and (hash-table-p headers) (gethash name headers)))

(defun note-session-id (transport headers &aux (session (header-value headers "mcp-session-id")))
  (when (and (stringp session) (plusp (length session))) (setf transport.session-id session)))

(defun character-stream (stream)
  "STREAM as a character stream: dexador hands back octets for some content
types; utf-8 is what MCP speaks."
  (if (subtypep (stream-element-type stream) 'character)
      stream
      (flexi-streams:make-flexi-stream stream :external-format :utf-8)))

(defun decode-message (text &aux (value (handler-case (nlk:decode-json text) (error () nil))))
  "TEXT as a message object, or NIL when it is not a JSON object."
  (and (hash-table-p value) value))

(defun queue-message (transport message)
  (when message (setf transport.pending (append transport.pending (list message)))))

(defun slurp-bounded (stream)
  "STREAM's remaining text, refusing past the frame limit."
  (multiple-value-bind (text cut-p) (nle:slurp-bounded stream (1+ +frame-limit+))
    (when cut-p (error 'transport-closed :detail "response body over the 1 MiB limit"))
    text))

(defun consume-response (transport status headers stream)
  "Queue what one successful response carries."
  (note-session-id transport headers)
  (let ((content-type (or (header-value headers "content-type") "")))
    (cond
      ((member status '(202 204)) nil)
      ((uiop:string-prefix-p "application/json" content-type)
       ;; A whole JSON body: one message, or an array of them (a batch).
       (let* ((text (slurp-bounded (character-stream stream)))
              (value (handler-case (nlk:decode-json text) (error () nil))))
         (cond ((hash-table-p value) (queue-message transport value))
               ((vectorp value)
                (loop for entry across value
                      when (hash-table-p entry) do (queue-message transport entry))))))
      ((uiop:string-prefix-p "text/event-stream" content-type)
       ;; The data: events into the queue, up to the first reply (the server should close after it).
       (nlk:read-events stream (lambda (text &aux (message (decode-message text)))
                                 (queue-message transport message)
                                 ;; A reply answers a request: it has an id and names no method.
                                 (and message
                                      (nth-value 1 (gethash "id" message))
                                      (null (nlk:json-value message :string "method"))))
                        :limit +frame-limit+))
      (t (error 'transport-closed
                :detail (format nil "HTTP ~a with content-type ~s"
                                status content-type))))))

(defun answer-awaited-p (text &aux (message (decode-message text)))
  "Whether the frame TEXT is a request -- a method and an id -- whose POST's
response carries its answer; a notification's or a reply's gets 202 and no body."
  (and message (nth-value 1 (gethash "id" message)) (gethash "method" message) t))

(defmethod transport-send ((transport http-transport) text deadline)
  (when (http-transport-closed-p transport)
    (error 'transport-closed :detail "transport closed"))
  (check-frame text)
  (let ((timeout (max 1 (ceiling (seconds-remaining deadline))))
        (awaited (answer-awaited-p text))
        (stream nil))
    (nlk:with-cleanup ((when (and stream (streamp stream))
                         (ignore-errors (close stream :abort t))))
      (handler-case
          (multiple-value-bind (body status headers)
              (dex:post transport.url
                        :headers (request-headers transport)
                        :content text
                        :want-stream awaited
                        :use-connection-pool nil
                        :keep-alive nil
                        :connect-timeout (min 10 timeout)
                        :read-timeout timeout)
            (setf stream body)
            (if awaited
                (consume-response transport status headers body)
                (note-session-id transport headers)))
        (dex:http-request-failed (condition)
          ;; A JSON-RPC error body is a per-call answer; else a failure named by status only.
          (let* ((status (dex:response-status condition))
                 (body (ignore-errors (dex:response-body condition)))
                 (text (cond ((stringp body) body)
                             ((streamp body)
                              (ignore-errors (slurp-bounded (character-stream body))))
                             ((vectorp body) (ignore-errors (flexi-streams:octets-to-string
                                                             body :external-format :utf-8)))))
                 (message (and text (decode-message text))))
            (note-session-id transport (ignore-errors (dex:response-headers condition)))
            (cond
              ((and message (nth-value 1 (gethash "error" message)))
               (queue-message transport message))
              ((and (eql status 404) (http-transport-session-id transport))
               (setf transport.session-id nil)
               (error 'transport-closed :detail "HTTP 404: the session expired"))
              (t (error 'transport-closed :detail (format nil "HTTP ~a" status))))))
        (transport-closed (condition) (error condition))
        (error (condition)
          (if (<= deadline (now-seconds))
              (error 'rpc-timeout
                     :detail (format nil "no answer from ~a within the deadline" transport.url))
              (error 'transport-closed
                     :detail (format nil "request to ~a failed: ~a" transport.url condition))))))))

(defmethod transport-receive ((transport http-transport) deadline)
  (declare (ignore deadline))
  (cond ((http-transport-closed-p transport) :closed)
        ((http-transport-pending transport) (pop transport.pending))
        (t :timeout)))

(defmethod transport-close ((transport http-transport))
  (unless (http-transport-closed-p transport)
    (setf (http-transport-closed-p transport) t)
    (nlk:when-let (session transport.session-id)
      (ignore-errors
       (dex:delete transport.url
                   :headers (request-headers transport)
                   :use-connection-pool nil
                   :keep-alive nil
                   :connect-timeout *close-timeout-seconds*
                   :read-timeout *close-timeout-seconds*)))
    (setf transport.pending '())
    t))
