;;;; provider-websocket-test.lisp --- nodecode.ai's chat lane over its WebSocket.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A loopback mock of nodecode.ai's gateway (its /v1/ws protocol, and
;;;; /v1/chat/completions over HTTP with SSE) stands behind each gate. They
;;;; lock: a session's next round sends only its new messages and the server
;;;; rebuilds the very body HTTP would have sent; anything else about the body
;;;; that changed sends it whole; the walk folds a socket answer as it folds
;;;; an HTTP one; and every way the socket is not there -- an upgrade refused,
;;;; a base the server lost, a socket gone before the answer, one closed by a
;;;; frame that leaves its stream open, one gone deaf -- still answers, on a
;;;; fresh socket or over HTTP.

(in-package #:nodecode.test)

(defstruct (ws-mock (:copier nil))
  "A loopback gateway: MODE says how its socket behaves, the rest is what it saw."
  (mode :ok)
  port
  handler
  (lock (bt2:make-lock :name "ws mock"))
  (seen '())
  (upgrades 0)
  (http 0)
  (bases (make-hash-table :test 'equal))
  (tricked nil))

(defun ws-mock-note (mock &rest entry)
  (bt2:with-lock-held ((ws-mock-lock mock))
    (setf (ws-mock-seen mock) (append (ws-mock-seen mock) (list entry)))))

(defun ws-mock-requests (mock)
  "The request frames MOCK took, in order: plists (:id :extends :bytes :whole :refused)."
  (remove :request (ws-mock-seen mock) :key #'car :test-not #'eq))

(defun ws-mock-extend (base delta &aux (out (alexandria:copy-hash-table base)))
  "DELTA laid over BASE, its messages appended, as the gateway's own extend."
  (maphash (lambda (key value)
             (setf (gethash key out)
                   (if (equal key "messages")
                       (concatenate 'vector (gethash key base) value)
                       value)))
           delta)
  out)

(defun ws-object (&rest fields &aux (object (make-hash-table :test 'equal)))
  "A JSON object of FIELDS, keys and values alternating, in their order."
  (loop for (key value) on fields by #'cddr
        do (setf (gethash key object) value))
  object)

(defun ws-mock-send (ws &rest fields)
  (wsd:send ws (nle::encode-json-object (apply #'ws-object fields))))

(defun ws-mock-request (mock ws frame text)
  (let* ((id (gethash "id" frame))
         (extends (gethash "extends" frame))
         (base (and extends (gethash extends (ws-mock-bases mock)))))
    (cond
      ;; :FORGET loses the first base asked for; :CLOSE drops the first socket
      ;; a request comes on
      ((and extends (or (null base) (and (eq (ws-mock-mode mock) :forget)
                                         (not (ws-mock-tricked mock)))))
       (setf (ws-mock-tricked mock) t)
       (ws-mock-note mock :request :id id :extends extends :bytes (length text) :refused t)
       (ws-mock-send ws "type" "error" "id" id
                     "error" (nlk:json-object "code" "unknown_base" "message" "gone")))
      ((and (eq (ws-mock-mode mock) :close) (not (ws-mock-tricked mock)))
       (setf (ws-mock-tricked mock) t)
       (ws-mock-note mock :request :id id :bytes (length text) :refused t)
       (wsd:close-connection ws))
      (t
       (let ((whole (if base
                        (ws-mock-extend base (gethash "body" frame))
                        (gethash "body" frame))))
         (multiple-value-bind (keep present) (gethash "keep" frame)
           (when (or keep (not present))
             (setf (gethash id (ws-mock-bases mock)) whole)))
         (ws-mock-note mock :request :id id :extends extends :bytes (length text)
                            :whole (nle::encode-json-object whole))
         (ws-mock-send ws "type" "start" "id" id "status" 200)
         (dolist (word '("hi" " there"))
           (ws-mock-send ws "type" "event" "id" id "event" :null
                         "data" (nlk:json-object
                                 "choices" (vector (nlk:json-object
                                                    "index" 0
                                                    "delta" (nlk:json-object "content" word))))))
         (if (eq (ws-mock-mode mock) :hold)
             (sleep 3)
             (progn (ws-mock-send ws "type" "event" "id" id "event" :null "data" "[DONE]")
                    (ws-mock-send ws "type" "end" "id" id))))))))

(defun ws-mock-app (mock)
  (lambda (env &aux (path (getf env :path-info)))
    (cond
      ((string= path "/v1/ws")
       (bt2:with-lock-held ((ws-mock-lock mock)) (incf (ws-mock-upgrades mock)))
       (if (eq (ws-mock-mode mock) :refuse)
           '(403 (:content-type "application/json") ("{\"error\":{\"code\":\"websocket_paid_only\"}}"))
           (let ((ws (wsd:make-server env)))
             (wsd:on :message ws
                     (lambda (text &aux (frame (nlk:decode-json text)))
                       (let ((type (gethash "type" frame)))
                         (if (equal type "request")
                             (nlk:spawn "ws-mock-request" (ws-mock-request mock ws frame text))
                             (ws-mock-note mock (intern (string-upcase type) :keyword)
                                           :id (gethash "id" frame))))))
             (lambda (responder)
               (declare (ignore responder))
               (wsd:start-connection ws)))))
      (t
       (bt2:with-lock-held ((ws-mock-lock mock)) (incf (ws-mock-http mock)))
       (list 200 '(:content-type "text/event-stream")
             (list (format nil "~a~adata: [DONE]~%~%" (chat-delta-line "hi") (chat-delta-line " there"))))))))

(defmacro with-ws-mock ((mock &key (mode :ok)) &body body)
  "BODY with MOCK a loopback gateway in MODE, the lane's WebSocket host, and
no line, base or down mark from before."
  `(let* ((,mock (make-ws-mock :mode ,mode :port (closed-loopback-port)))
          (nle::*websocket-hosts* '("127.0.0.1"))
          (nle::*websocket-lines* (make-hash-table :test 'equal))
          (nle::*websocket-bases* (make-hash-table :test 'equal))
          (nle::*websocket-down* (make-hash-table :test 'equal)))
     (setf (ws-mock-handler ,mock)
           (clack:clackup (ws-mock-app ,mock) :port (ws-mock-port ,mock) :silent t :debug nil))
     (nle::await-port (ws-mock-port ,mock))
     (unwind-protect (progn ,@body)
       (nle::close-websocket-lines)
       (clack:stop (ws-mock-handler ,mock)))))

(defun deaf-websocket-answer (socket close-frame &aux (stream (usocket:socket-stream socket)))
  "Take SOCKET's upgrade, then read whatever comes and answer nothing; with
CLOSE-FRAME, a close frame to the first frame, and still the socket kept."
  (ignore-errors
   (let ((head (loop with octets = (make-array 0 :element-type '(unsigned-byte 8)
                                                 :adjustable t :fill-pointer 0)
                     for octet = (read-byte stream nil nil)
                     while octet
                     do (vector-push-extend octet octets)
                     until (and (>= (length octets) 4)
                                (equalp (subseq octets (- (length octets) 4)) #(13 10 13 10)))
                     finally (return (sb-ext:octets-to-string octets :external-format :latin-1)))))
     (write-sequence
      (sb-ext:string-to-octets
       (format nil "HTTP/1.1 101 Switching Protocols~c~cUpgrade: websocket~c~cConnection: Upgrade~c~cSec-WebSocket-Accept: ~a~c~c~c~c"
               #\Return #\Newline #\Return #\Newline #\Return #\Newline
               (websocket-driver.util:generate-accept
                (string-trim '(#\Space #\Return)
                             (aref (nth-value 1 (ppcre:scan-to-strings "(?i)sec-websocket-key:([^\\n]*)" head)) 0)))
               #\Return #\Newline #\Return #\Newline)
       :external-format :latin-1)
      stream)
     (force-output stream)
     (when (and close-frame (read-byte stream nil nil))
       (write-sequence (coerce #(#x88 2 3 232) '(vector (unsigned-byte 8))) stream)
       (force-output stream))
     (loop while (read-byte stream nil nil)))))

(defun deaf-websocket-server (port &key close-frame &aux (listener (usocket:socket-listen "127.0.0.1" port :reuse-address t
                                                                           :element-type '(unsigned-byte 8)))
                                        (accepted (list 0)) (sockets '()) (stop nil))
  "A loopback server on PORT that takes the WebSocket upgrade and then is
deaf: it reads every frame, pings among them, answers none and never
closes => (values STOP ACCEPTED), STOP a function that ends it, ACCEPTED a
cons whose car counts the sockets it took."
  (nlk:spawn "deaf websocket server"
    (loop until stop
          do (when (ignore-errors (usocket:wait-for-input listener :timeout 0.1 :ready-only t))
               (let ((socket (ignore-errors (usocket:socket-accept listener))))
                 (when socket
                   (push socket sockets)
                   (incf (car accepted))
                   (nlk:spawn "deaf websocket" (deaf-websocket-answer socket close-frame)))))
          finally (ignore-errors (usocket:socket-close listener))))
  (values (lambda ()
            (setf stop t)
            (dolist (socket sockets) (ignore-errors (usocket:socket-close socket))))
          accepted))

(defun ws-mock-endpoint (mock)
  (format nil "http://127.0.0.1:~d/v1/chat/completions" (ws-mock-port mock)))

(defun ws-config (endpoint)
  (nle::make-effective-provider-config :lane "openai-completions" :endpoint endpoint
                                       :model "m" :api-key "k" :request-timeout 5))

(defun ws-body (messages &rest fields)
  "A chat body as the lane builds it: model, MESSAGES, then FIELDS."
  (nle::encode-json-object
   (apply #'ws-object "model" "m"
          "messages" (map 'vector (lambda (text) (nlk:json-object "role" "user" "content" text))
                          messages)
          (or fields (list "stream" t)))))

(defun ws-round (mock body &optional (session "s1"))
  "BODY over MOCK's socket as SESSION's round => the SSE text that came back,
or NIL when the socket did not take it."
  (let ((stream (nle::websocket-post (ws-mock-endpoint mock) body '(("authorization" . "Bearer k"))
                                     (ws-config (ws-mock-endpoint mock)) session)))
    (when stream
      (unwind-protect
           (sb-ext:octets-to-string
            (coerce (loop for octet = (read-byte stream nil nil) while octet collect octet)
                    '(vector (unsigned-byte 8)))
            :external-format :utf-8)
        (close stream)))))

(defun ws-walk (mock)
  "One walk of MOCK's endpoint, as the chat lane walks it => the frames folded."
  (let ((frames '()))
    (nle::walk-provider-stream (lambda (frame finish record)
                                 (declare (ignore finish record))
                                 (push frame frames))
                               :config (ws-config (ws-mock-endpoint mock))
                               :request-json (ws-body '("hi"))
                               :headers '(("authorization" . "Bearer k"))
                               :finish-on-sentinel t)
    (nreverse frames)))

(deftest websocket-url-is-beside-nodecode-ais-chat-route-only
    (let ((nle::*websocket-hosts* '("api.nodecode.ai"))))
  (is (equal '("wss://api.nodecode.ai/v1/ws" "/v1/chat/completions")
             (multiple-value-list (nle::websocket-url "https://api.nodecode.ai/v1/chat/completions"))) "nodecode.ai's chat route has its socket beside it")
  (is (null (nle::websocket-url "https://api.openai.com/v1/chat/completions")) "another host rides HTTP")
  (is (null (nle::websocket-url "https://api.nodecode.ai/v1/images/generations")) "so does another route"))

(deftest messages-span-skips-brackets-inside-strings ()
  (let* ((body "{\"model\":\"m\",\"messages\":[{\"content\":\"a ] \\\" [ b\"},{\"x\":[1]}],\"tools\":[]}")
         (octets (sb-ext:string-to-octets body :external-format :utf-8)))
    (multiple-value-bind (start end) (nle::messages-span octets)
      (is (equal "{\"content\":\"a ] \\\" [ b\"},{\"x\":[1]}"
                 (sb-ext:octets-to-string (subseq octets start end))) "the list's inside, whatever brackets its strings hold"))))

(deftest a-session-sends-only-its-new-messages-and-the-server-rebuilds-its-body ()
  (with-ws-mock (mock)
    (let ((bodies (list (ws-body (list (make-string 4000 :initial-element #\a) "one"))
                        (ws-body (list (make-string 4000 :initial-element #\a) "one" "two"))
                        (ws-body (list (make-string 4000 :initial-element #\a) "one" "two" "three")))))
      (dolist (body bodies)
        (let ((answer (ws-round mock body)))
          (is (search "data: [DONE]" answer) "each round's answer arrives whole, as its SSE body")
          (is (search "\"content\":\" there\"" answer) "with its content")))
      (let ((seen (ws-mock-requests mock)))
        (is (equal '(nil "r1" "r2") (mapcar (lambda (entry) (getf (cdr entry) :extends)) seen)) "the first round goes whole, each next one extends the one before")
        (is (every (lambda (entry body) (equal body (getf (cdr entry) :whole))) seen bodies) "and the server rebuilds the very body HTTP would have sent")
        (is (< (getf (cdr (second seen)) :bytes) 200) "a round past the first uploads only what is new")
        (is (member '(:forget :id "r1") (ws-mock-seen mock) :test #'equal) "a base a later round replaced is forgotten on the server")
        (is (= 1 (ws-mock-upgrades mock)) "all on one socket")))))

(deftest a-body-that-changed-beside-its-messages-goes-whole ()
  (with-ws-mock (mock)
    (ws-round mock (ws-body '("one") "stream" t "max_tokens" 10))
    (ws-round mock (ws-body '("one" "two") "stream" t "max_tokens" 20))
    (ws-round mock (ws-body '("other") "stream" t "max_tokens" 20))
    (is (equal '(nil nil nil) (mapcar (lambda (entry) (getf (cdr entry) :extends)) (ws-mock-requests mock))) "a changed field, or messages that do not continue the base, send the body whole")))

(deftest a-round-with-no-session-is-not-kept-on-the-server ()
  (with-ws-mock (mock)
    (ws-round mock (ws-body '("one")) nil)
    (ws-round mock (ws-body '("one" "two")) nil)
    (is (equal '(nil nil) (mapcar (lambda (entry) (getf (cdr entry) :extends)) (ws-mock-requests mock))) "a side call outside a session goes whole")
    (is (zerop (hash-table-count (ws-mock-bases mock))) "and asks the server to keep nothing")))

(deftest the-walk-folds-a-socket-answer-as-an-http-one ()
  (with-ws-mock (mock)
    (let ((frames (ws-walk mock)))
      (is (equal '("hi" " there")
                 (mapcar (lambda (frame) (gethash "content" (gethash "delta" (aref (gethash "choices" frame) 0)))) frames)) "the walk folds the socket's chunks in order")
      (is (zerop (ws-mock-http mock)) "and nothing went over HTTP"))))

(deftest a-refused-upgrade-answers-over-http-and-is-not-dialled-again ()
  (with-ws-mock (mock :mode :refuse)
    (is (= 2 (length (ws-walk mock))) "a key the socket refuses is answered over HTTP")
    (is (= 2 (length (ws-walk mock))) "and again")
    (is (equal '(1 2) (list (ws-mock-upgrades mock) (ws-mock-http mock))) "with one upgrade asked for, never a second")))

(deftest a-base-the-server-lost-goes-whole-on-the-same-socket ()
  (with-ws-mock (mock :mode :forget)
    (ws-round mock (ws-body '("one")))
    (is (search "data: [DONE]" (ws-round mock (ws-body '("one" "two")))) "the round the server could not extend is answered")
    (let ((seen (ws-mock-requests mock)))
      (is (equal '((nil nil) ("r1" t) (nil nil))
                 (mapcar (lambda (entry) (list (getf (cdr entry) :extends) (getf (cdr entry) :refused))) seen)) "its extends was refused, and the body went again whole")
      (is (= 1 (ws-mock-upgrades mock)) "on the same socket"))))

(deftest a-socket-gone-before-its-answer-goes-again-on-a-fresh-one ()
  (with-ws-mock (mock :mode :close)
    (is (search "data: [DONE]" (ws-round mock (ws-body '("one")))) "the round is answered")
    (is (= 2 (ws-mock-upgrades mock)) "on a second socket")
    (is (zerop (ws-mock-http mock)) "not over HTTP")))

(defun deaf-websocket-round (&key close-frame (quiet nle::*websocket-quiet-seconds*))
  "One round against a deaf server => (values STREAM SOCKETS SECONDS)."
  (let ((port (closed-loopback-port))
        (start (get-internal-real-time)))
    (multiple-value-bind (stop accepted) (deaf-websocket-server port :close-frame close-frame)
      (unwind-protect
           (let ((nle::*websocket-hosts* '("127.0.0.1"))
                 (nle::*websocket-lines* (make-hash-table :test 'equal))
                 (nle::*websocket-bases* (make-hash-table :test 'equal))
                 (nle::*websocket-down* (make-hash-table :test 'equal))
                 (nle::*websocket-quiet-seconds* quiet)
                 (nle::*websocket-pong-seconds* 0.3)
                 (endpoint (format nil "http://127.0.0.1:~d/v1/chat/completions" port)))
             (unwind-protect
                  (values (nle::websocket-post endpoint (ws-body '("one")) '(("authorization" . "Bearer k"))
                                               (ws-config endpoint) "s1")
                          (car accepted)
                          (/ (- (get-internal-real-time) start) internal-time-units-per-second))
               (nle::close-websocket-lines)))
        (funcall stop)))))

(deftest a-socket-that-goes-deaf-is-closed-and-the-round-goes-over-http ()
  (multiple-value-bind (stream sockets seconds) (deaf-websocket-round :quiet 0.2)
    (is (null stream) "a round no socket answers is left to HTTP")
    (is (= 3 sockets) "after three sockets, each found deaf by its unanswered ping and closed")
    (is (< seconds 15) "in seconds, not at the turn's deadline")))

(deftest a-close-frame-closes-the-line-though-the-socket-stays ()
  (multiple-value-bind (stream sockets seconds) (deaf-websocket-round :close-frame t)
    (is (null stream) "a round each socket closed under is left to HTTP")
    (is (= 3 sockets) "after three sockets")
    (is (< seconds 10) "each closed at its close frame, with no ping to wait on")))

(deftest a-stream-closed-before-its-end-cancels-it-on-the-server ()
  (with-ws-mock (mock :mode :hold)
    (let ((stream (nle::websocket-post (ws-mock-endpoint mock) (ws-body '("one"))
                                       '(("authorization" . "Bearer k"))
                                       (ws-config (ws-mock-endpoint mock)) "s1")))
      (is (read-byte stream nil nil) "the answer started")
      (close stream)
      (is (await (:tries 40 :rest 0.05) (member '(:cancel :id "r1") (ws-mock-seen mock) :test #'equal)) "closing it early tells the server to stop"))))
