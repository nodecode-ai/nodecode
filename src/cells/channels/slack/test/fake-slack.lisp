;;;; fake-slack.lisp --- a Slack that answers on 127.0.0.1, for tests.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The adapter's live executors and its Socket Mode lap run unchanged
;;;; against this: the Web API under /api/ (the methods called,
;;;; answered in Slack's own envelope and recorded), apps.connections.open
;;;; answering a ws:// URL on the same port, and /link, the socket, which
;;;; says hello, records every acknowledgement and carries what a test pushes.
;;;; A slash command's response_url lands under /response/. Shapes follow
;;;; docs.slack.dev.

(in-package #:nodecode.test)

(define-test-slice "slack" "CHANNEL-SLACK-" :recipe "channels")

(defstruct (fake-slack (:copier nil))
  (lock (bt2:make-lock :name "fake-slack"))
  (handler nil)
  (port 0 :type integer)
  ;; (METHOD . BODY) per Web API call, newest first; BODY a decoded object,
  ;; or the query string of a GET.
  (calls '())
  ;; Envelope ids the lane acknowledged, newest first.
  (acks '())
  ;; The sockets open now, and how many apps.connections.open answered.
  (sockets '())
  (opened 0 :type integer)
  (ts 100 :type integer)
  ;; What conversations.history and conversations.replies answer: message
  ;; objects, history's newest first as Slack orders it.
  (history '())
  (replies '()))

(nlk:access (fake fake-slack))

(defun fake-slack-body (env &aux (length (getf env :content-length)))
  "The request's body decoded as JSON, or NIL for none."
  (when (and length (plusp length))
    (let ((octets (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence octets (getf env :raw-body))
      (ignore-errors (nlk:decode-json (sb-ext:octets-to-string octets :external-format :utf-8))))))

(defun fake-slack-answer (fake method body query)
  "The object Slack answers METHOD with."
  (flet ((ok (&rest pairs) (apply #'nlk:make-json-object "ok" t pairs)))
    (cond
      ((string= method "auth.test")
       (ok "user_id" "UBOT" "bot_id" "BBOT" "team" "Fake" "team_id" "T1"
           "url" "https://fake.slack.com/"))
      ((string= method "apps.connections.open")
       (ok "url" (format nil "ws://127.0.0.1:~d/link?ticket=~d&app_id=A1"
                         fake.port (bt2:with-lock-held (fake.lock) (incf fake.opened)))))
      ((string= method "users.info")
       (let ((id (or (ppcre:register-groups-bind (id) ("user=([^&]+)" (or query "")) id) "?")))
         (ok "user" (nlk:json-object "id" id "name" (string-downcase id)
                                     "profile" (nlk:json-object "display_name"
                                                                (if (string= id "U1") "kim" ""))))))
      ((string= method "chat.postMessage")
       (let ((ts (format nil "1700000000.~6,'0d" (bt2:with-lock-held (fake.lock) (incf fake.ts)))))
         (ok "channel" (gethash "channel" body) "ts" ts
             "message" (nlk:json-object "ts" ts "text" (gethash "text" body)))))
      ((string= method "chat.getPermalink")
       (or (ppcre:register-groups-bind (channel ts) ("channel=([^&]+)&message_ts=([^&]+)" (or query ""))
             (ok "channel" channel "permalink" (format nil "https://fake.slack.com/archives/~a/p~a"
                                                      channel (remove #\. ts))))
           (nlk:make-json-object "ok" nil "error" "message_not_found")))
      ((string= method "conversations.history")
       (ok "messages" (coerce fake.history 'vector)))
      ((string= method "conversations.replies")
       (ok "messages" (coerce fake.replies 'vector)))
      ((string= method "users.conversations")
       (ok "channels" (vector (nlk:json-object "id" "C1" "name" "general" "is_private" nil)
                              (nlk:json-object "id" "G2" "name" "ops" "is_private" t)
                              (nlk:json-object "id" "D1" "is_im" t "user" "U1"))))
      ((string= method "users.list")
       (ok "members" (vector (nlk:json-object "id" "U1" "name" "kim"
                                              "profile" (nlk:json-object "display_name" "kim"))
                             (nlk:json-object "id" "UBOT" "name" "nodecode" "is_bot" t)
                             (nlk:json-object "id" "U3" "name" "gone" "deleted" t))))
      (t (ok)))))

(defun fake-slack-socket (fake env)
  "The Socket Mode link: hello on open, every frame the lane sends read as an
acknowledgement."
  (let ((ws (wsd:make-server env)))
    (wsd:on :open ws
            (lambda ()
              (bt2:with-lock-held (fake.lock) (push ws fake.sockets))
              (wsd:send ws (nlk:encode-json-object
                            (nlk:json-object "type" "hello" "num_connections" 1)))))
    (wsd:on :message ws
            (lambda (message &aux (frame (ignore-errors (nlk:decode-json message))))
              (nlk:when-let (id (nlk:json-value frame :string "envelope_id"))
                (bt2:with-lock-held (fake.lock) (push id fake.acks)))))
    (wsd:on :close ws
            (lambda (&key code reason)
              (declare (ignore code reason))
              (bt2:with-lock-held (fake.lock)
                (setf fake.sockets (remove ws fake.sockets)))))
    (lambda (responder)
      (declare (ignore responder))
      (wsd:start-connection ws))))

(defun fake-slack-app (fake)
  (lambda (env &aux (path (getf env :path-info)))
    (cond
      ((string= path "/link") (fake-slack-socket fake env))
      ((uiop:string-prefix-p "/api/" path)
       (let* ((method (subseq path 5))
              (query (getf env :query-string))
              (body (fake-slack-body env)))
         (bt2:with-lock-held (fake.lock) (push (cons method (or body query)) fake.calls))
         (list 200 '(:content-type "application/json")
               (list (nlk:encode-json-object (fake-slack-answer fake method body query))))))
      ((uiop:string-prefix-p "/response/" path)
       (let ((body (fake-slack-body env)))
         (bt2:with-lock-held (fake.lock) (push (cons "response" body) fake.calls))
         (list 200 '(:content-type "text/plain") (list "ok"))))
      (t (list 200 '(:content-type "text/plain") (list "fake slack"))))))

(defmacro with-fake-slack ((var) &body body)
  "Run BODY with VAR a fake Slack serving on 127.0.0.1, stopped on unwind."
  `(let ((,var (make-fake-slack)))
     (multiple-value-bind (handler port) (nle::serve-local (fake-slack-app ,var))
       (setf (fake-slack-handler ,var) handler
             (fake-slack-port ,var) port)
       (nlk:with-cleanup ((nle::stop-clack-handler handler))
         ,@body))))

(defun fake-slack-api-base (fake)
  (format nil "http://127.0.0.1:~d/api" fake.port))

(defun slack-calls (fake method)
  "The bodies of every METHOD call so far, oldest first."
  (bt2:with-lock-held (fake.lock)
    (reverse (loop for (name . body) in fake.calls when (string= name method) collect body))))

(defun fake-slack-acked-p (fake envelope-id)
  (bt2:with-lock-held (fake.lock) (and (member envelope-id fake.acks :test #'equal) t)))

(defun fake-slack-push (fake frame)
  "Send FRAME, a JSON object, on every open socket; => how many carried it."
  (let ((sockets (bt2:with-lock-held (fake.lock) (copy-list fake.sockets))))
    (dolist (ws sockets (length sockets))
      (wsd:send ws (nlk:encode-json-object frame)))))

(defun slack-envelope (id type payload)
  "A Socket Mode envelope, as Slack sends one."
  (nlk:json-object "envelope_id" id "type" type "payload" payload
                   "accepts_response_payload" (equal type "slash_commands")))

(defun slack-event-envelope (id event)
  "An events_api envelope carrying EVENT in its event_callback."
  (slack-envelope id "events_api"
                  (nlk:json-object "type" "event_callback" "team_id" "T1"
                                   "event_id" (format nil "Ev~a" id) "event" event)))

(defun slack-message (channel ts text &key (user "U1") thread (channel-type "channel")
                                           subtype bot-id parent-user files)
  "A message event as Slack delivers one."
  (nlk:json-object "type" "message" "channel" channel "user" user "text" text "ts" ts
                   "channel_type" channel-type "team" "T1"
                   :opt "thread_ts" thread
                   :opt "subtype" subtype
                   :opt "bot_id" bot-id
                   :opt "parent_user_id" parent-user
                   :opt "files" files))
