;;;; cell-test.lisp --- a browser's requests, down the line and back.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defmacro with-link-routes (&body body)
  "BODY with two operator routes on the gateway: /t-link-own answers who it
was asked by, /t-link-echo the body it was sent."
  `(nlk:with-cleanup ((nle:route "/t-link-own" nil) (nle:route "/t-link-echo" nil))
     (nle:route "/t-link-own" (lambda (env)
                                (declare (ignore env))
                                (nlk:json-object "own" t)))
     (nle:route "/t-link-echo" (lambda (env &aux (text (make-string (getf env :content-length))))
                                 (read-sequence text (getf env :raw-body))
                                 (list 200 '(:content-type "text/plain") (list text))))
     ,@body))

(deftest link-cell-lets-in-an-allowed-browser-and-no-other (with-temp-gateway (port))
  (with-link ()
    (with-link-routes
      (let ((cookie (nodecode-link::allow-browser "Chrome on Android" "Zagreb, HR")))
        ;; No cookie: the page leads to the pairing page, anything else is refused.
        (multiple-value-bind (status headers) (link-request "GET" "/web/" :headers '(("accept" . "text/html")))
          (is (= 302 status))
          (is (equal "/_link/pair" (cdr (assoc "location" headers :test #'string=)))))
        (is (= 401 (link-request "GET" "/t-link-own")))
        (is (= 401 (link-request "GET" "/t-link-own" :headers (list (link-cookie "0000")))))
        ;; The cookie: replayed on loopback with the operator's Bearer, which
        ;; the browser never held.
        (multiple-value-bind (status headers body) (link-request "GET" "/t-link-own"
                                                                 :headers (list (link-cookie cookie)))
          (is (= 200 status))
          (is (search "\"own\":true" body))
          (is (search "application/json" (cdr (assoc "content-type" headers :test #'string=)))))
        ;; Something that changes a thing must come from this machine's own page.
        (is (= 403 (link-request "POST" "/t-link-echo" :body "hi"
                                 :headers (list (link-cookie cookie) '("origin" . "https://evil.example")))))
        (multiple-value-bind (status headers body)
            (link-request "POST" "/t-link-echo" :body "hello down the line"
                          :headers (list (link-cookie cookie) (link-own-origin)
                                         '("content-type" . "text/plain")))
          (declare (ignore headers))
          (is (= 200 status))
          (is (equal "hello down the line" body)))
        (multiple-value-bind (status headers) (link-request "GET" "/" :headers (list (link-cookie cookie)))
          (is (= 302 status))
          (is (equal "/web/" (cdr (assoc "location" headers :test #'string=)))))
        ;; Removed, the same cookie is a stranger's.
        (let ((id (gethash "id" (first (nodecode-link::browsers)))))
          (is (search "can no longer" (nodecode-link::run-slash (format nil "remove ~a" id) nil))))
        (is (= 401 (link-request "GET" "/t-link-own" :headers (list (link-cookie cookie)))))))))

(defun link-message (id)
  "The next whole WebSocket message the machine sent on stream ID, as a string."
  (let ((pieces '()))
    (loop for read = (link-next id)
          while (and read (eq (first read) :data))
          do (push (fourth read) pieces)
          unless (logtest nodecode-link::+more+ (third read))
            return (sb-ext:octets-to-string
                    (apply #'concatenate '(vector (unsigned-byte 8)) (reverse pieces))
                    :external-format :utf-8))))

(defun link-say (id text)
  "The browser's message TEXT on stream ID, as the relay hands it on."
  (nodecode-link::on-data (nodecode-link::data-frame
                           id nodecode-link::+text+
                           (sb-ext:string-to-octets text :external-format :utf-8))))

(deftest link-cell-bridges-the-page-socket-without-the-token (with-temp-gateway (port))
  (with-link ()
    (let ((cookie (nodecode-link::allow-browser "Firefox on Linux" "")))
      (flet ((upgrade (&rest headers)
               (let ((id (link-open "GET" "/gateway" :kind "ws" :headers headers)))
                 (values (gethash "status" (second (link-next id))) id))))
        (is (= 401 (upgrade (link-own-origin))) "no cookie")
        (is (= 403 (upgrade (link-cookie cookie) '("origin" . "https://evil.example"))) "another page")
        (multiple-value-bind (status id) (upgrade (link-cookie cookie) (link-own-origin))
          (is (= 101 status))
          ;; The gateway's challenge, answered with no token at all: the Bearer
          ;; the machine put on the upgrade is the operator at the door.
          (let ((challenge (nlk:decode-json (link-message id))))
            (is (equal "gateway_connect_challenge" (gethash "type" challenge)))
            (link-say id (nlk:encode-json-object
                          (nlk:make-json-object
                           "type" "gateway_connect"
                           "protocol_min" 1 "protocol_max" 1
                           "challenge_nonce" (gethash "challenge_nonce" challenge)
                           "client" (nlk:make-json-object "id" "nodecode-web" "version" "0.1.0"
                                                          "mode" "web" "platform" "browser"
                                                          "instance_id" "web-test")
                           "requested_scopes" (vector "sync.read" "sync.write")))))
          (is (equal "gateway_connect_ack" (gethash "type" (nlk:decode-json (link-message id)))))
          ;; The browser goes: the loopback socket goes with it, and nothing is said back.
          (nodecode-link::on-end id)
          (is (await (:timeout 5) (zerop (hash-table-count nodecode-link::*flows*)))))
        ;; Taken off the list, a browser's open socket is closed at once.
        (multiple-value-bind (status id) (upgrade (link-cookie cookie) (link-own-origin))
          (is (= 101 status))
          (is (equal "gateway_connect_challenge" (gethash "type" (nlk:decode-json (link-message id)))))
          (nodecode-link::run-slash (format nil "remove ~a" (gethash "id" (first (nodecode-link::browsers)))) nil)
          (is (equal "end" (gethash "t" (second (link-next id)))))
          (is (zerop (hash-table-count nodecode-link::*flows*))))))))

(deftest link-cell-pairs-the-browser-the-operator-allows (with-temp-gateway (port))
  (with-link ()
    (with-link-routes
      (let ((phone '(("user-agent" . "Mozilla/5.0 (Linux; Android 15) AppleWebKit/537.36 Chrome/140.0 Mobile Safari/537.36"))))
        (is (= 200 (link-request "GET" "/_link/pair")) "the pairing page is anyone's")
        (is (= 403 (link-request "POST" "/_link/ask" :headers phone)) "an ask comes from the page")
        (multiple-value-bind (status headers body)
            (link-request "POST" "/_link/ask" :headers (cons (link-own-origin) phone))
          (declare (ignore headers))
          (is (= 200 status))
          (let* ((ask (nlk:decode-json body))
                 (code (remove #\Space (gethash "code" ask))))
            (is (= 6 (length code)))
            (is (equal "m-test" (gethash "machine" ask)))
            ;; One ask at a time.
            (is (= 429 (link-request "POST" "/_link/ask" :headers (list (link-own-origin)))))
            (is (search "Chrome on Android" (getf nodecode-link::*ask* :agent)) "never its text")
            (is (search "a browser is asking, but not with 000000x — check the code on its screen"
                        (nodecode-link::run-slash "allow 000000x" nil)))
            ;; The code as both screens group it, 482 913, is the code.
            (is (search "allowed Chrome on Android in Zagreb, HR"
                        (nodecode-link::run-slash (format nil "allow ~a" (gethash "code" ask)) nil)))
            (multiple-value-bind (status headers body)
                (link-request "GET" (format nil "/_link/ask?id=~a" (gethash "id" ask)))
              (is (= 200 status))
              (is (search "allowed" body))
              (let* ((set (cdr (assoc "set-cookie" headers :test #'string=)))
                     (cookie (subseq set (1+ (position #\= set)) (position #\; set))))
                (is (search "Secure; HttpOnly; SameSite=Lax" set))
                ;; The cookie it was handed lets it in, and past the pairing page.
                (is (= 200 (link-request "GET" "/t-link-own" :headers (list (link-cookie cookie)))))
                (is (= 302 (link-request "GET" "/_link/pair" :headers (list (link-cookie cookie)))))))))
        ;; A second browser asks, and the operator says no.
        (let ((ask (nlk:decode-json (nth-value 2 (link-request "POST" "/_link/ask"
                                                               :headers (list (link-own-origin))
                                                               :ip "198.51.100.7")))))
          (is (search "denied" (nodecode-link::run-slash "deny" nil)))
          (is (search "denied" (nth-value 2 (link-request "GET" (format nil "/_link/ask?id=~a"
                                                                           (gethash "id" ask)))))))
        ;; The state keeps the digest, never the cookie, and only the operator reads it.
        (let ((file (nlk:home "link/state.json")))
          (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat file)))))
          (is (= 1 (length (nodecode-link::browsers)))))))))

(deftest link-cell-the-link-route-shows-and-moves-what-the-panel-does (with-temp-gateway (port))
  ;; /api/link answers what /link's panel shows and does what the slash does,
  ;; through the same verbs; a request that came down the line names the
  ;; browser it came through, which is how the page knows a verb would cut it.
  (with-link ()
    (flet ((ask (&rest headers)
             (nlk:decode-json (nth-value 2 (apply #'link-request "POST" "/_link/ask" :headers
                                                  (cons (link-own-origin) headers)
                                                  (and (null headers) '(:ip "198.51.100.7")))))))
      (is-route (port :get "/api/link" :token nil) 401 "the operator's route")
      (with-gateway-http (port :get "/api/link")
        (is (equal "off" (nlk:json-value body :string "state")))
        (is (equal +link-origin+ (nlk:json-value body :string "address")))
        (is (equal "ws://127.0.0.1:9/line" (nlk:json-value body :string "relay")))
        (is (zerop (length (nlk:json-value body :array "browsers"))))
        (is (null (nlk:json-value body :any "ask")))
        (is (null (nlk:json-value body :any "here")) "asked on this machine, not down the line"))
      (let* ((asked (ask '("user-agent" . "Mozilla/5.0 (Linux; Android 15) Chrome/140.0 Mobile Safari/537.36")))
             (code (gethash "code" asked)))
        (with-gateway-http (port :get "/api/link")
          (is-present (shown (nlk:json-value body :object "ask")) "the ask the panel shows"
            (is (equal code (nlk:json-value shown :string "code")) "the code as the asking page shows it")
            (is (equal "Chrome on Android" (nlk:json-value shown :string "agent")))
            (is (equal "Zagreb, HR" (nlk:json-value shown :string "place")))
            (is (<= 110 (nlk:json-value shown :integer "seconds_left") 120))))
        (with-gateway-http (port :post "/api/link?op=allow&code=000000")
          (is (search "a browser is asking, but not with 000 000" (nlk:json-value body :string "text"))))
        (with-gateway-http (port :post (format nil "/api/link?op=allow&code=~a" (quri:url-encode code)))
          (is (equal "link: allowed Chrome on Android in Zagreb, HR" (nlk:json-value body :string "text")))
          (is (null (nlk:json-value body :any "ask")))
          (is-present (browser (first (coerce (nlk:json-value body :array "browsers") 'list))) "the allowed browser"
            (is (equal "Chrome on Android" (nlk:json-value browser :string "agent")))
            (is (null (gethash "hash" browser)) "never its cookie's digest")))
        ;; The browser the operator let in asks for the same, down the line.
        (let* ((set (cdr (assoc "set-cookie" (nth-value 1 (link-request "GET" (format nil "/_link/ask?id=~a"
                                                                                    (gethash "id" asked))))
                                :test #'string=)))
               (cookie (subseq set (1+ (position #\= set)) (position #\; set)))
               (id (gethash "id" (first (nodecode-link::browsers)))))
          (multiple-value-bind (status headers body) (link-request "GET" "/api/link" :headers (list (link-cookie cookie)))
            (declare (ignore headers))
            (is (= 200 status))
            (is (equal id (gethash "here" (nlk:decode-json body))) "the page came through this browser"))
          ;; Another asks, and is denied; then the first is removed.
          (ask)
          (with-gateway-http (port :post "/api/link?op=deny")
            (is (equal "link: denied" (nlk:json-value body :string "text")))
            (is (null (nlk:json-value body :any "ask"))))
          (with-gateway-http (port :post "/api/link?op=remove&id=nobody")
            (is (search "no allowed browser nobody" (nlk:json-value body :string "text"))))
          (with-gateway-http (port :post (format nil "/api/link?op=remove&id=~a" id))
            (is (search "can no longer open this machine" (nlk:json-value body :string "text")))
            (is (zerop (length (nlk:json-value body :array "browsers")))))))
      ;; The switch: on with nothing listening at the relay's port is failing,
      ;; and says why in words; off is off.
      (with-gateway-http (port :post "/api/link?op=on")
        (is (search "link: on · no line yet — nothing listens at 127.0.0.1:9" (nlk:json-value body :string "text")))
        (is (equal "failing" (nlk:json-value body :string "state"))))
      (with-gateway-http (port :post "/api/link?op=off")
        (is (search "link: off" (nlk:json-value body :string "text")))
        (is (equal "off" (nlk:json-value body :string "state"))))
      (is-route (port :post "/api/link?op=nope") 500 "an op /link has no verb for" "route_failed"))))

(deftest link-cell-qr-code-is-the-standard-symbol ()
  ;; Known answers: the error correction of the standard's own worked example
  ;; (ISO/IEC 18004, annex I: 01234567 at 1-M), and two whole symbols that
  ;; qrencode 4.1.1 (-l M -8) draws module for module the same -- version 1,
  ;; and version 13, which adds the version bits and nine interleaved blocks.
  (is (equalp #(165 36 212 193 237 54 199 135 44 85)
              (nodecode-link::rs-remainder #(16 32 12 86 97 128 236 17 236 17 236 17 236 17 236 17)
                                           (nodecode-link::rs-divisor 10))))
  (flet ((symbol-digest (text)
           (nodecode-link::digest (format nil "~{~a~%~}" (nodecode-link::qr-rows text :margin 0)))))
    (is (equal "b0b09bb15298d7c3e024c7f0193f0c90f8311f7244cc72884d453e04157289e1" (symbol-digest "hi")))
    (is (equal "42b3b0265bed77e5b04a0d9fd6eed892a9172af6d313a31e2ae08f3cab13cec0"
               (symbol-digest (format nil "~v@{~a~:*~}" 60 "wxyz1")))))
  (let ((rows (nodecode-link::qr-rows "https://archlinux-4p8a94.nodecode.ai")))
    (is (= 33 (length rows)) "an address is version 3, 29 modules, and two light ones either side")
    (is (every (lambda (row) (= 33 (length row))) rows))
    (is (every (lambda (row) (string= "00" row :end2 2)) rows) "the margin is light")
    (is (string= "0011111110" (third rows) :end2 10) "and the finder's top edge is dark")))

(deftest link-cell-shows-the-address-as-a-qr-code-while-on (with-temp-gateway (port))
  ;; The panel and the page's route carry the address's code while the link
  ;; is on, and nothing while it is off: an address with no line behind it
  ;; answers no browser.
  (with-link ()
    (is (null (getf (nodecode-link::panel) :picture)) "off, no code")
    (with-gateway-http (port :get "/api/link")
      (is (null (nlk:json-value body :any "qr"))))
    (with-saved-globals ((nodecode-link::*line* (nodecode-link::make-line :state :up))
                         (nodecode-link::*address-code* nil))
      (let ((picture (getf (nodecode-link::panel) :picture)))
        (is (equal (nodecode-link::qr-rows +link-origin+) (getf picture :rows)) "on, the address's code")
        (is (equal "The address's QR code" (getf picture :label))))
      (with-gateway-http (port :get "/api/link")
        (is (equal "on" (nlk:json-value body :string "state")))
        (let ((drawn (coerce (nodecode-link::qr-rows +link-origin+) 'vector)))
          (is (equalp drawn (nlk:json-value body :array "qr")) "the page draws the same code"))))))

(defvar *relay-query* nil
  "The query the last line the fake relay took was dialled with.")

(defun fake-relay (inbox)
  "A relay's line door: every socket that opens posts (SOCKET . MESSAGE) for
each message it sends to INBOX."
  (lambda (env)
    (if (string-equal "websocket" (gethash "upgrade" (getf env :headers) ""))
        (let ((socket (wsd:make-server env)))
          (setf *relay-query* (getf env :query-string))
          (wsd:on :message socket (lambda (message)
                                    (sb-concurrency:send-message inbox (cons socket message))))
          (lambda (responder)
            (declare (ignore responder))
            (wsd:start-connection socket)))
        ;; SERVE-LOCAL's probe, asking whether anything answers.
        '(200 (:content-type "text/plain") ("relay")))))

(deftest link-cell-keeps-a-line-to-its-relay (with-temp-gateway (port))
  (let ((inbox (sb-concurrency:make-mailbox :name "relay")))
    (multiple-value-bind (relay relay-port) (nle:serve-local (fake-relay inbox))
      (nlk:with-cleanup ((nle:stop-clack-handler relay)
                         (nle:route "/t-link-page" nil))
        (nle:route "/t-link-page" (lambda (env) (declare (ignore env)) (nlk:json-object "page" t))
                   :public t)
        (with-cell-stop ((link-start "relay" (format nil "ws://127.0.0.1:~d/line" relay-port)))
          (flet ((heard ()
                   (let ((got (sb-concurrency:receive-message inbox :timeout 10)))
                     (values (car got) (cdr got))))
                 (welcome (socket &rest pairs)
                   (wsd:send socket (nlk:encode-json-object
                                     (apply #'nlk:make-json-object "t" "welcome" "machine" "m-line"
                                            "origin" "https://m-line.example.test"
                                            "address" "https://nodecode.ai/uplink/m-line" pairs)))))
            ;; Off, nothing is dialled.
            (is (null (sb-concurrency:receive-message inbox :timeout 1)))
            (nodecode-link::keep "on" t)
            (nlk:worker-poke nodecode-link::*worker*)
            (multiple-value-bind (socket hello) (heard)
              (let ((hello (nlk:decode-json hello)))
                (is (equal "hello" (gethash "t" hello)))
                (is (null (gethash "machine" hello)) "the first hello asks for a name")
                (is (uiop:string-prefix-p "hint=" *relay-query*) "and the line says what to call it"))
              (welcome socket "secret" "s3cret")
              (is (nodecode-link::await-line 5))
              (is (equal "s3cret" (nodecode-link::kept "secret")) "the minted secret is kept")
              ;; A request down the real line, answered from the gateway.
              (wsd:send socket (nlk:encode-json-object
                                (nlk:make-json-object "t" "open" "s" 7 "kind" "http" "method" "GET"
                                                      "path" "/_link/pair" "headers" (vector)
                                                      "ip" "203.0.113.1" "place" "")))
              (wsd:send socket "{\"t\":\"end\",\"s\":7}")
              (let ((head (nlk:decode-json (nth-value 1 (heard)))))
                (is (equal "head" (gethash "t" head)))
                (is (= 200 (gethash "status" head))))
              (let ((data (nth-value 1 (heard))))
                (is (= 7 (nodecode-link::frame-stream data)))
                (is (search "Open this machine" (sb-ext:octets-to-string (nodecode-link::frame-payload data)
                                                                         :external-format :utf-8))))
              (is (equal "end" (gethash "t" (nlk:decode-json (nth-value 1 (heard))))))
              ;; The line drops: the machine dials again, as itself.
              (nodecode-link::sever nodecode-link::*line*))
            (multiple-value-bind (socket hello) (heard)
              (let ((hello (nlk:decode-json hello)))
                (is (equal "m-line" (gethash "machine" hello)))
                (is (equal "s3cret" (gethash "secret" hello)))
                (is (equal "machine=m-line" *relay-query*)))
              (welcome socket)
              (is (nodecode-link::await-line 5))
              ;; And off: the line goes, and is not dialled again.
              (is (search "link: off" (nodecode-link::run-slash "off" nil)))
              (is (await (:timeout 5) (null nodecode-link::*line*)))
              (is (null (sb-concurrency:receive-message inbox :timeout 2))))))))))

(deftest link-cell-a-bare-link-shows-it-and-switches-nothing (with-temp-gateway (port))
  ;; The asking phone's page sends its operator to /link to compare the code:
  ;; a bare /link, and /link status, answer the state and the panel -- the
  ;; browser asking with its code and seconds among them -- and leave the link
  ;; as it was. /link on and /link off are the switch.
  (with-link ()
    (nodecode-link::keep "on" t)
    (let* ((asked (nlk:decode-json (nth-value 2 (link-request "POST" "/_link/ask"
                                                              :headers (list (link-own-origin)
                                                                             '("user-agent" . "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) Version/18.0 Mobile/15E148 Safari/604.1"))))))
           (code (gethash "code" asked)))
      (dolist (line '("" "status"))
        (multiple-value-bind (said dialog) (nodecode-link::run-slash line nil)
          (is (uiop:string-prefix-p "link: on" said) line)
          ;; The status names the browser asking, its code as its screen shows it.
          (is (search (format nil "a browser is asking: ~a (Safari on iPhone in Zagreb, HR, " code) said))
          (is (search (format nil "s left) — /link allow ~a" (remove #\Space code)) said))
          (is (equal "asking" (getf (first (getf dialog :rows)) :key)) "the panel leads with the ask")
          (is (nodecode-link::kept "on") "and the link is still on")))
      (is (search "link: off" (nodecode-link::run-slash "off" nil)))
      (is (null (nodecode-link::kept "on")))
      (is (search "link: off" (nodecode-link::run-slash "" nil)) "bare, off stays off")
      (is (null (nodecode-link::kept "on")))
      (is (search "Off · /link on turns it on" (getf (nodecode-link::panel) :empty-label)))
      (is (search "link: denied" (nodecode-link::run-slash "deny" nil)) "the ask still waited: no switch answered it")
      (is (search "usage /link [status | on | off | retry" (nodecode-link::run-slash "toggle" nil))))))

(deftest link-cell-a-relay-nothing-answers-reads-as-failing-in-words (with-temp-gateway (port))
  ;; A dial that fails says why the way the operator reads it -- nothing
  ;; listens there, the relay refused this machine -- never a condition's
  ;; printed self; the link reads as failing, with the tries and the seconds
  ;; to the next, the second failure is said to the shells, and retry dials
  ;; at once.
  (with-link ()
    (is (equal "nothing listens at 127.0.0.1:9"
               (nodecode-link::failure-words (make-condition 'usocket:connection-refused-error))))
    (is (equal "the relay refused this machine: wrong secret"
               (nodecode-link::failure-words (make-condition 'nodecode-link::line-refused
                                                             :words "the relay refused this machine: wrong secret"))))
    (is (equal "the relay's address answered, but not with a line"
               (nodecode-link::failure-words (make-condition 'simple-error :format-control "Error during WebSocket handshake:~%  Unexpected response code: 404"))))
    (with-gateway-http (port :post "/api/link?op=on")
      (is (equal "failing" (nlk:json-value body :string "state")))
      (is-present (failure (nlk:json-value body :object "failure")) "why there is no line"
        (is (equal "nothing listens at 127.0.0.1:9" (nlk:json-value failure :string "why")))
        (is (<= 1 (nlk:json-value failure :integer "tries")))
        (is (<= 0 (nlk:json-value failure :integer "next_try_seconds") 60))))
    (let ((said (await (:timeout 10)
                  (first (find-if (lambda (entry) (search "link: no line to the relay" (first entry)))
                                  (nlk:notice-log))))))
      (is (search "link: no line to the relay at ws://127.0.0.1:9/line — nothing listens at 127.0.0.1:9; trying again in" said))
      (is (not (search "Condition" said)) "no condition text")
      (is (not (search "USOCKET" said))))
    (is (search "link: on, failing · " (nodecode-link::status)))
    (is (search "no line: nothing listens at 127.0.0.1:9; trying again in" (nodecode-link::status)))
    (with-gateway-http (port :post "/api/link?op=retry")
      (is (search "link: still no line — nothing listens at 127.0.0.1:9; trying again in" (nlk:json-value body :string "text"))))
    (with-gateway-http (port :post "/api/link?op=off")
      (is (equal "off" (nlk:json-value body :string "state")))
      (is (null (nlk:json-value body :any "failure")) "off, nothing is failing"))))

(deftest link-cell-tells-the-account-its-name-once-a-name ()
  (with-link ()
    (let ((told '()) (answer nil) (old (fdefinition 'nle::machine-update)))
      (unwind-protect
           (with-saved-globals ((nodecode-link::*reported* nil) (nodecode-link::*report-after* 0))
             (setf (fdefinition 'nle::machine-update)
                   (lambda (facts &rest keys)
                     (declare (ignore keys))
                     (push (nlk:json-value facts :string "relay") told)
                     (if (eq answer :refuse) (error "wrong secret") answer)))
             ;; Never linked: asked each lap, nothing kept, nothing said.
             (nodecode-link::report-address 1000)
             (nodecode-link::report-address 1005)
             (is (equal '("m-test" "m-test") told))
             (is (null nodecode-link::*reported*))
             ;; Linked: told once, then quiet until the name moves or the hour is up.
             (setf answer (nlk:json-object "name" "m" "address" "https://m-test.nodecode.ai") told '())
             (nodecode-link::report-address 1010)
             (nodecode-link::report-address 1015)
             (is (equal '("m-test") told))
             (nodecode-link::keep "machine" "m-other")
             (nodecode-link::report-address 1020)
             (nodecode-link::report-address (+ 1020 nodecode-link::+report-seconds+))
             (is (equal '("m-other" "m-other" "m-test") told))
             ;; Refused: a warning, and no asking again for ten minutes.
             (setf answer :refuse told '() nodecode-link::*reported* nil)
             (nodecode-link::report-address 9000)
             (nodecode-link::report-address 9300)
             (is (equal '("m-other") told))
             (is (= 9600 nodecode-link::*report-after*)))
        (setf (fdefinition 'nle::machine-update) old)))))
