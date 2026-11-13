;;;; provider-websocket.lisp --- nodecode.ai's chat lane over one WebSocket.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; nodecode.ai serves its API over one WebSocket as well as over HTTP
;;;; (https://nodecode.ai/docs/websocket): the same routes, each request a
;;;; frame, each answer's SSE events frames back, many requests on one socket.
;;;; What it buys a long agent loop is the upload: a request may name an
;;;; earlier one on the same socket in `extends' and send only its new
;;;; messages, where HTTP sends the whole conversation every round (an
;;;; 818k-token chat on a 20 Mbit/s link: 1.5 s to first token against 3.4 s).
;;;;
;;;; So a round to a host in *WEBSOCKET-HOSTS* goes over the socket by
;;;; default, and over HTTP with SSE whenever the socket is not there for it:
;;;; a host or key that refuses the upgrade (a free key does, with 403), a
;;;; dial that fails, a socket that closes or refuses the frame before the
;;;; answer starts, a proxy in the way, or NODECODE_WEBSOCKET=0.
;;;;
;;;; WEBSOCKET-POST is the one seam: WALK-PROVIDER-STREAM asks it before its
;;;; POST, and takes what it answers -- a binary stream yielding the same SSE
;;;; lines the POST's body would -- through the same walk, so the idle
;;;; deadline, the content clock, cancellation, evidence and retries are the
;;;; HTTP path's own. A stream closed before its end sends `cancel', so the
;;;; server stops the answer as a closed HTTP connection stops it.
;;;;
;;;; Only what is new: each session's last request that started is its base,
;;;; kept by byte offsets into its body. The next body goes as a delta when
;;;; everything but the messages is byte-for-byte the base's and the base's
;;;; messages are a prefix of its own (the history memo encodes a message the
;;;; same way every round), and whole otherwise. The server rebuilds the
;;;; whole body from the delta; one it no longer holds is refused as
;;;; `unknown_base' and the body goes again whole.
;;;;
;;;; A socket can go without its end of the stream ever coming: a close frame
;;;; from a peer that then never shuts its side (SBCL's own server on
;;;; Windows), or a link cut where neither end saw it. So a request never
;;;; waits on a line blind: a line that left the open state, or that heard
;;;; nothing for a while and then left its ping unanswered, is closed, and
;;;; what was waiting on it goes again on a fresh one.

(in-package #:nodecode.evolved)

(defparameter *websocket-hosts* '("api.nodecode.ai")
  "Hosts whose chat lane rides the WebSocket API by default.")

(defparameter *websocket-down-seconds* 60
  "How long a socket that failed to open is not dialled again for its key.")

(defparameter *websocket-quiet-seconds* 15
  "How long a line with a request waiting on it may hear nothing before it is
pinged.")

(defparameter *websocket-pong-seconds* 10
  "How long a ping may go unanswered before its line is taken for gone: a
socket half closed, or cut where neither end saw it close.")

(defparameter *websocket-bases-limit* 64
  "How many sessions' bases stay: past it the longest unused goes, and that
session's next body goes whole.")

(defvar *websocket-lock* (bt2:make-lock :name "provider websockets")
  "Guards the tables below and the dial of a line.")

(defvar *websocket-lines* (make-hash-table :test 'equal)
  "A key's (ws URL and headers) current line.")

(defvar *websocket-down* (make-hash-table :test 'equal)
  "A key whose socket did not open => the internal real time until it is
dialled again, or T for a key the server refused: never again this process.")

(defvar *websocket-bases* (make-hash-table :test 'equal)
  "A session's base: (LINE ID OCTETS INNER-START INNER-END LAST-USED).")

(defstruct (ws-line (:copier nil))
  "One socket and the requests running on it."
  client
  (lock (bt2:make-lock :name "provider websocket line"))
  (requests (make-hash-table :test 'equal))
  (next-id 0 :type integer)
  (closing nil)
  (closed nil)
  (idle-seconds 60)
  (idle-since (get-internal-real-time))
  ;; when it last heard a frame or a pong, and when a ping still unanswered went
  (heard (get-internal-real-time))
  (pinged nil))

(defstruct (ws-request (:copier nil))
  "One request on a line: its frames as SSE octets, in arrival order."
  (id "" :type string)
  line
  (lock (bt2:make-lock :name "provider websocket request"))
  (semaphore (sb-thread:make-semaphore :name "provider websocket request"))
  (chunks '())
  ;; :WAITING until the answer starts, then :STARTED, then :ENDED; or :WHOLE
  ;; (an answer not streamed), :REFUSED (an error frame), :CLOSED (the socket
  ;; went before the answer started)
  (state :waiting)
  (status nil)
  (body nil)
  (code nil))

(defun websocket-disabled-p ()
  (member (credential-env "NODECODE_WEBSOCKET") '("0" "off" "false" "no") :test #'string-equal))

(defun websocket-url (endpoint &aux (uri (ignore-errors (quri:uri endpoint))))
  "The WebSocket API's URL beside chat ENDPOINT, and the frame's path for it,
when ENDPOINT is a chat route on a host in *WEBSOCKET-HOSTS*."
  (let ((host (and uri (quri:uri-host uri)))
        (path (and uri (quri:uri-path uri))))
    (when (and host path
               (member host *websocket-hosts* :test #'string-equal)
               (uiop:string-suffix-p path "/chat/completions")
               (uiop:string-suffix-p endpoint path))
      (values (concatenate 'string
                           (if (string-equal (quri:uri-scheme uri) "http") "ws" "wss")
                           (subseq endpoint (search "://" endpoint)
                                   (- (length endpoint) (length "/chat/completions")))
                           "/ws")
              path))))

;;; --- the frames ---------------------------------------------------------------

(defun ws-request-push (request chunk &optional state)
  "Queue CHUNK (octets, or NIL) on REQUEST and move it to STATE, waking its reader."
  (bt2:with-lock-held ((ws-request-lock request))
    (when chunk
      (setf (ws-request-chunks request) (nconc (ws-request-chunks request) (list chunk))))
    (when state
      (setf (ws-request-state request) state)))
  (sb-thread:signal-semaphore (ws-request-semaphore request)))

(defun ws-request-done (line request state)
  "REQUEST no longer runs on LINE: it ended in STATE."
  (bt2:with-lock-held ((ws-line-lock line))
    (remhash (ws-request-id request) (ws-line-requests line))
    (when (zerop (hash-table-count (ws-line-requests line)))
      (setf (ws-line-idle-since line) (get-internal-real-time))))
  (ws-request-push request nil state))

(defun sse-octets (event data)
  "One SSE event, named EVENT (or not), carrying DATA, as the octets HTTP sends."
  (sb-ext:string-to-octets
   (format nil "~@[event: ~a~%~]data: ~a~%~%"
           (and (stringp event) event)
           (if (stringp data) data (encode-json-object data)))
   :external-format :utf-8))

(defun websocket-frame (line text &aux (frame (ignore-errors (nlk:decode-json text))))
  "Take one frame TEXT off LINE, on its reader thread."
  (when (hash-table-p frame)
    (let* ((type (json-value frame :string "type"))
           (id (json-value frame :string "id"))
           (request (and id (bt2:with-lock-held ((ws-line-lock line))
                              (gethash id (ws-line-requests line))))))
      (cond
        ((equal type "open")
         (setf (ws-line-idle-seconds line)
               (or (json-value frame :integer "idle_secs") (ws-line-idle-seconds line))))
        ((equal type "closing")
         (setf (ws-line-closing line) t))
        ((null request))
        ((equal type "start")
         (setf (ws-request-status request) (json-value frame :integer "status"))
         (ws-request-push request nil :started))
        ((equal type "event")
         (ws-request-push request (sse-octets (gethash "event" frame) (gethash "data" frame))))
        ((equal type "end")
         (ws-request-done line request :ended))
        ((equal type "response")
         (setf (ws-request-status request) (json-value frame :integer "status")
               (ws-request-body request) (let ((body (gethash "body" frame)))
                                           (if (stringp body) body (encode-json-object body))))
         (ws-request-done line request :whole))
        ((equal type "error")
         (setf (ws-request-code request) (json-value frame :string "error" "code")
               (ws-request-body request) (json-value frame :string "error" "message"))
         (ws-request-done line request :refused))))))

(defun websocket-line-closed (line)
  "LINE's socket is gone: a request still waiting for its start is :CLOSED, one
streaming ends where it stopped (the walk reads that as a cut stream)."
  (setf (ws-line-closed line) t)
  (let ((requests (bt2:with-lock-held ((ws-line-lock line))
                    (prog1 (alexandria:hash-table-values (ws-line-requests line))
                      (clrhash (ws-line-requests line))))))
    (dolist (request requests)
      (ws-request-push request nil (if (eq (ws-request-state request) :waiting) :closed :ended)))))

(defun seconds-since (time)
  (/ (- (get-internal-real-time) time) internal-time-units-per-second))

(defun websocket-line-watch (line &aux (client (ws-line-client line)))
  "Look after LINE while a request waits on it: a socket that left the open
state (the peer's close frame came, its end of the stream has not) or that
left a ping unanswered is closed now, its requests with it; one quiet too
long is pinged."
  (unless (ws-line-closed line)
    (let ((pinged (ws-line-pinged line)))
      (cond
        ((or (not (eq (wsd:ready-state client) :open))
             (and pinged (> (seconds-since pinged) *websocket-pong-seconds*)))
         (note-phase "ws gone state=~(~a~)~:[~; unanswered ping~]" (wsd:ready-state client) pinged)
         (websocket-line-closed line)
         (ignore-errors (nlk:sever-websocket client :grace-seconds 1)))
        ((and (null pinged) (> (seconds-since (ws-line-heard line)) *websocket-quiet-seconds*))
         (setf (ws-line-pinged line) (get-internal-real-time))
         (bt2:with-lock-held ((ws-line-lock line))
           (ignore-errors
            (wsd:send-ping client nil (lambda ()
                                        (setf (ws-line-heard line) (get-internal-real-time)
                                              (ws-line-pinged line) nil))))))))))

(defun websocket-wait (request)
  "Park until REQUEST has news, its line looked after every second meanwhile."
  (unless (sb-thread:wait-on-semaphore (ws-request-semaphore request) :timeout 1)
    (websocket-line-watch (ws-request-line request))))

(defun websocket-send (line octets)
  "Send OCTETS, one JSON frame, on LINE as text => true when it went."
  (and (not (ws-line-closed line))
       (bt2:with-lock-held ((ws-line-lock line))
         (handler-case (progn (wsd:send (ws-line-client line) octets :type :text) t)
           (error () nil)))))

;;; --- the lines ----------------------------------------------------------------

(defun websocket-refusal-status (condition)
  "The HTTP status an upgrade was refused with, read off CONDITION, or NIL."
  (let ((match (nth-value 1 (ppcre:scan-to-strings "response code: (\\d{3})"
                                                   (princ-to-string condition)))))
    (and match (parse-integer (aref match 0)))))

(defun dial-websocket-line (key url headers &aux (line (make-ws-line)))
  "Open a line to URL with HEADERS => the line, or NIL with KEY marked down."
  (let ((client (wsd:make-client url :additional-headers headers)))
    (setf (ws-line-client line) client)
    (wsd:on :message client (lambda (text)
                              (setf (ws-line-heard line) (get-internal-real-time))
                              (when (stringp text)
                                (ignore-errors (websocket-frame line text)))))
    (wsd:on :close client (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (websocket-line-closed line)))
    (handler-case (progn (wsd:start-connection client) line)
      (error (condition)
        (let ((status (websocket-refusal-status condition)))
          (note-phase "ws down status=~a ~a ~a" status url
                      (let ((text (remove #\Newline (princ-to-string condition))))
                        (subseq text 0 (min 160 (length text)))))
          (setf (gethash key *websocket-down*)
                (if (member status '(401 403 404))
                    t
                    (+ (get-internal-real-time)
                       (* *websocket-down-seconds* internal-time-units-per-second))))
          (ignore-errors (nlk:sever-websocket client))
          nil)))))

(defun websocket-line-usable-p (line)
  "Whether LINE takes a new request: open, not closing, not about to be closed
idle by the server, and with room."
  (and line
       (not (ws-line-closed line))
       (not (ws-line-closing line))
       (bt2:with-lock-held ((ws-line-lock line))
         (let ((running (hash-table-count (ws-line-requests line))))
           (and (< running 16)
                (or (plusp running)
                    (< (/ (- (get-internal-real-time) (ws-line-idle-since line))
                          internal-time-units-per-second)
                       (- (ws-line-idle-seconds line) 5))))))))

(defun websocket-line (key url headers &key fresh)
  "KEY's line to take a request on: the current one while it is usable and
FRESH is false, else a new one => the line, or NIL when none opens."
  (bt2:with-lock-held (*websocket-lock*)
    (let ((down (gethash key *websocket-down*))
          (current (gethash key *websocket-lines*)))
      (cond
        ((eq down t) nil)
        ((and down (< (get-internal-real-time) down)) nil)
        ((and (not fresh) (websocket-line-usable-p current)) current)
        (t
         (remhash key *websocket-down*)
         (when (and current
                    (zerop (bt2:with-lock-held ((ws-line-lock current))
                             (hash-table-count (ws-line-requests current)))))
           (ignore-errors (nlk:sever-websocket (ws-line-client current))))
         (let ((line (dial-websocket-line key url headers)))
           (if line
               (setf (gethash key *websocket-lines*) line)
               (remhash key *websocket-lines*))
           line))))))

(defun close-websocket-lines ()
  "Sever every line: an image saved or a process leaving holds no socket thread."
  (let ((lines (bt2:with-lock-held (*websocket-lock*)
                 (prog1 (alexandria:hash-table-values *websocket-lines*)
                   (clrhash *websocket-lines*)
                   (clrhash *websocket-bases*)))))
    (dolist (line lines)
      (ignore-errors (nlk:sever-websocket (ws-line-client line) :grace-seconds 1)))))

(defun reset-websocket-state ()
  "A restored image starts with no line, no base and nothing marked down."
  (clrhash *websocket-lines*)
  (clrhash *websocket-bases*)
  (clrhash *websocket-down*))

(pushnew 'close-websocket-lines sb-ext:*save-hooks*)
(pushnew 'close-websocket-lines sb-ext:*exit-hooks*)
(pushnew 'reset-websocket-state sb-ext:*init-hooks*)

;;; --- only what is new ------------------------------------------------------

(defun messages-span (octets &aux (key #.(sb-ext:string-to-octets "\"messages\":[")))
  "The bounds of the top-level messages list's inside in body OCTETS =>
(values START END), START just past its [ and END at its ], or NIL."
  ;; A quote inside a JSON string is escaped, so the key's bytes occur
  ;; unescaped only as a key; the body puts model before it and nothing that
  ;; holds such a key.
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (let ((start (search key octets)))
    (when start
      (let ((depth 1) (in-string nil) (escaped nil))
        (declare (type fixnum depth))
        (loop for i of-type fixnum from (+ start (length key)) below (length octets)
              for octet = (aref octets i)
              do (cond (escaped (setf escaped nil))
                       (in-string (case octet
                                    (92 (setf escaped t))
                                    (34 (setf in-string nil))))
                       (t (case octet
                            (34 (setf in-string t))
                            ((91 123) (incf depth))
                            ((93 125) (when (zerop (decf depth))
                                        (return-from messages-span
                                          (values (+ start (length key)) i)))))))))))
  nil)

(defun octets-equal (a a-start a-end b b-start b-end)
  (and (= (- a-end a-start) (- b-end b-start))
       (null (mismatch a b :start1 a-start :end1 a-end :start2 b-start :end2 b-end))))

(defun websocket-delta (base octets start end)
  "The body that extends BASE (a session's base) to OCTETS, whose messages
list's inside is START to END: only its new messages => octets, or NIL when
OCTETS differs from BASE elsewhere or does not continue its messages."
  (destructuring-bind (line id old old-start old-end &rest rest) base
    (declare (ignore line id rest))
    (when (and (octets-equal octets 0 start old 0 old-start)
               (octets-equal octets end (length octets) old old-end (length old))
               (<= (- old-end old-start) (- end start))
               (octets-equal octets start (+ start (- old-end old-start)) old old-start old-end))
      (let ((new-start (+ start (- old-end old-start))))
        ;; past the base's last message, the comma before the first new one
        (cond ((= new-start end))
              ((and (> old-end old-start) (= 44 (aref octets new-start))) (incf new-start))
              ((= old-end old-start))
              (t (return-from websocket-delta nil)))
        (concatenate '(vector (unsigned-byte 8))
                     #.(sb-ext:string-to-octets "{\"messages\":[")
                     (subseq octets new-start end)
                     #.(sb-ext:string-to-octets "]}"))))))

(defun websocket-request-octets (id path body &key extends (keep t))
  (concatenate '(vector (unsigned-byte 8))
               (sb-ext:string-to-octets
                (format nil "{\"type\":\"request\",\"id\":~s,\"path\":~s~@[,\"extends\":~s~]~:[,\"keep\":false~;~],\"body\":"
                        id path extends keep)
                :external-format :utf-8)
               body
               #.(sb-ext:string-to-octets "}")))

(defun note-websocket-base (session line id octets start end)
  "SESSION's next body extends request ID on LINE: OCTETS, its messages
list's inside START to END. The base it replaces on the same line is
forgotten there."
  (when (and session start)
    (let ((old (bt2:with-lock-held (*websocket-lock*)
                 (prog1 (gethash session *websocket-bases*)
                   (setf (gethash session *websocket-bases*)
                         (list line id octets start end (get-internal-real-time)))
                   (when (> (hash-table-count *websocket-bases*) *websocket-bases-limit*)
                     (remhash (alexandria:extremum (alexandria:hash-table-keys *websocket-bases*) #'<
                                                   :key (lambda (key) (sixth (gethash key *websocket-bases*))))
                              *websocket-bases*))))))
      (when (and old (eq (first old) line) (not (equal (second old) id)))
        (websocket-send line (sb-ext:string-to-octets
                              (format nil "{\"type\":\"forget\",\"id\":~s}" (second old))
                              :external-format :utf-8))))))

;;; --- the answer as the walk reads it -----------------------------------------

(defclass websocket-sse-stream (sb-gray:fundamental-binary-input-stream)
  ((request :initarg :request :reader websocket-sse-request)
   (buffer :initform nil)
   (position :initform 0)
   (open :initform t))
  (:documentation "One request's answer as the octets of its SSE body."))

(defmethod stream-element-type ((stream websocket-sse-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream websocket-sse-stream))
  (slot-value stream 'open))

(defmethod sb-gray:stream-read-byte ((stream websocket-sse-stream))
  (with-slots (request buffer position) stream
    (loop
      (when (and buffer (< position (length buffer)))
        (return (prog1 (aref buffer position) (incf position))))
      (let ((next nil) (state nil))
        (bt2:with-lock-held ((ws-request-lock request))
          (setf next (pop (ws-request-chunks request))
                state (ws-request-state request)))
        (cond (next (setf buffer next position 0))
              ((member state '(:ended :closed)) (return :eof))
              ;; a park the walk's deadline and a cancel both end
              (t (websocket-wait request)))))))

(defmethod close ((stream websocket-sse-stream) &key abort)
  (declare (ignore abort))
  (when (slot-value stream 'open)
    (setf (slot-value stream 'open) nil)
    (let* ((request (websocket-sse-request stream))
           (line (ws-request-line request)))
      ;; Closed before its end: the server stops the answer.
      (unless (member (ws-request-state request) '(:ended :closed))
        (websocket-send line (sb-ext:string-to-octets
                              (format nil "{\"type\":\"cancel\",\"id\":~s}" (ws-request-id request))
                              :external-format :utf-8))
        (bt2:with-lock-held ((ws-line-lock line))
          (remhash (ws-request-id request) (ws-line-requests line))))))
  t)

(defun websocket-start (line octets &key extends keep path)
  "Send body OCTETS on LINE and wait for its answer to start => the request,
in the state it reached."
  (let* ((id (bt2:with-lock-held ((ws-line-lock line))
               (format nil "r~d" (incf (ws-line-next-id line)))))
         (request (make-ws-request :id id :line line))
         (started nil))
    (bt2:with-lock-held ((ws-line-lock line))
      (setf (gethash id (ws-line-requests line)) request))
    (unwind-protect
         (progn
           (unless (websocket-send line (websocket-request-octets id path octets
                                                                  :extends extends :keep keep))
             (ws-request-done line request :closed))
           (loop while (eq (ws-request-state request) :waiting)
                 do (nlk:with-cancellable-wait (*current-durable-turn*)
                      (websocket-wait request)))
           (setf started t)
           request)
      ;; A deadline or a cancel while it waited: the server stops it.
      (unless started
        (close (make-instance 'websocket-sse-stream :request request))))))

(defun websocket-post (endpoint request-json headers config session)
  "The answer to REQUEST-JSON as WALK-PROVIDER-STREAM reads a POST's, over
the WebSocket API beside ENDPOINT => (values STREAM 200 HEADERS), or NIL
when it does not go that way and HTTP takes it."
  (multiple-value-bind (url path) (websocket-url endpoint)
    (when (and url
               (not (websocket-disabled-p))
               (null (nlk:proxy-for endpoint (effective-provider-config-proxy config))))
      (let* ((key (cons url (nlk:sha256-text (format nil "~s" headers))))
             (octets (coerce (request-octets request-json) '(simple-array (unsigned-byte 8) (*))))
             (fresh nil)
             (whole nil))
        (multiple-value-bind (start end) (messages-span octets)
          (loop for attempt below 3
                for line = (websocket-line key url headers :fresh fresh)
                do (unless line
                     (return nil))
                   (let* ((base (and session start (not whole)
                                     (bt2:with-lock-held (*websocket-lock*)
                                       (gethash session *websocket-bases*))))
                          (delta (and base (eq (first base) line)
                                      (websocket-delta base octets start end)))
                          (request (websocket-start line (or delta octets)
                                                    :extends (and delta (second base))
                                                    :keep (and session t) :path path)))
                     (note-phase "ws ~(~a~) ~a req-bytes=~d~@[ extends=~a~]" (ws-request-state request)
                                 (ws-request-id request) (length (or delta octets)) (and delta (second base)))
                     (case (ws-request-state request)
                       ((:started :ended)
                        (note-websocket-base session line (ws-request-id request) octets start end)
                        (unless (eql 200 (ws-request-status request))
                          (error 'provider-error :status (ws-request-status request)
                                                 :detail "non-200 response" :scope :request
                                                 :evidence-request-body (bounded-evidence request-json)))
                        (return (values (make-instance 'websocket-sse-stream :request request)
                                        200
                                        (alexandria:plist-hash-table
                                         '("content-type" "text/event-stream") :test 'equal))))
                       (:whole
                        ;; an answer whole is a refusal, as HTTP's status failure reads
                        (let ((body (or (ws-request-body request) "")))
                          (error 'provider-error
                                 :status (ws-request-status request)
                                 :detail (subseq body 0 (min 400 (length body)))
                                 :scope :request
                                 :evidence-body (bounded-evidence body)
                                 :evidence-request-body (bounded-evidence request-json)
                                 :evidence-content-type "application/json"
                                 :stream-complete-p t)))
                       (:refused
                        (cond ((member (ws-request-code request) '("unknown_base" "extends_path")
                                       :test #'equal)
                               (setf whole t))
                              ((equal (ws-request-code request) "closing")
                               (setf fresh t))
                              (t (return nil))))
                       ;; the socket went before the answer started: a fresh one, whole
                       (:closed (setf fresh t whole t))))))))))
