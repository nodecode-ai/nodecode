;;;; bridge-test.lisp --- the HTTP bridge against a fake extension.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test here drives a REAL acceptor on an ephemeral port with a Lisp
;;;; thread playing the extension's poll loop, so the wire contract the
;;;; vendored extension depends on — long-poll shape, version header, CORS,
;;;; result correlation, origin refusal — is proved byte for byte.

(in-package #:nodecode.test)

(nlk:access (bridge chrome::bridge))

(defun echo-handler (command)
  "The fake extension answering every command with its own action/params."
  (values t (nlk:json-object "action" (gethash "action" command)
                             "params" (gethash "params" command))))

(deftest chrome-cell-bridge-status-answers-while-idle ()
  (with-chrome-bridge (port bridge :version "0.15.46")
    (multiple-value-bind (status body) (bridge-http port :get "/status")
      (is (eql status 200))
      (is (equal (nlk:json-value body :text "version") "0.15.46"))
      (is (not (nlk:json-value body :boolean "connected")) "idle bridge is not connected")
      (is (eql (nlk:json-value body :integer "queuedCommands") 0))
      (is (equal (nlk:json-value body :text "url") (chrome::bridge-url bridge))))))

(deftest chrome-cell-bridge-long-poll-delivers-command-with-version ()
  (with-chrome-bridge (port bridge :version "0.15.46")
    (with-fake-extension (port #'echo-handler)
      (let ((result (chrome::bridge-send bridge "tab.version"
                                         (nlk:json-object "x" 1)
                                         :timeout 10)))
        (is (equal (nlk:json-value result :text "action") "tab.version"))
        (is (eql (nlk:json-value result :integer "params" "x") 1))
        (is (zerop (hash-table-count bridge.pending)))
        (is (null bridge.queue))
        (is (chrome::bridge-connected-p bridge) "a poll marks the bridge connected")
        (is (equal bridge.client-name "Nodecode Chrome Connector fake"))))
    ;; The poll response itself: header and body both carry the version.
    (with-saved-globals ((chrome::*long-poll-seconds* 0.2))
      (multiple-value-bind (status body headers)
          (bridge-http port :get "/next?name=Nodecode%20Chrome%20Connector%20probe" :origin +fake-origin+)
        (is (eql status 200))
        (is (equal (nlk:json-value body :text "type") "none"))
        (is (equal (nlk:json-value body :text "expectedExtensionVersion") "0.15.46"))
        (is (equal (gethash "x-pi-chrome-version" headers) "0.15.46"))
        (is (equal (gethash "access-control-allow-origin" headers) +fake-origin+))
        (is (equal (gethash "access-control-expose-headers" headers) "x-pi-chrome-version"))
        (is (equal (gethash "vary" headers) "origin"))))))

(deftest chrome-cell-bridge-refuses-foreign-origins (with-chrome-bridge (port bridge))
  (multiple-value-bind (status body) (bridge-http port :get "/next?name=x"
                                                  :origin "https://evil.example")
    (is (eql status 403))
    (is (equal (nlk:json-value body :text "error") "browser origin not allowed")))
  (is (eql 403 (bridge-http port :options "/result" :origin "https://evil.example")))
  (multiple-value-bind (status body headers)
      (bridge-http port :options "/result" :origin +fake-origin+)
    (is (eql status 200))
    (is (nlk:json-value body :boolean "ok"))
    (is (equal (gethash "access-control-allow-methods" headers) "GET,POST,OPTIONS"))
    (is (equal (gethash "access-control-allow-headers" headers) "content-type")))
  ;; A page's fetch with sec-fetch-site cross-site and no Origin is refused too.
  (is (eql 403 (bridge-http port :get "/next" :headers '(("sec-fetch-site" . "cross-site")))))
  ;; POST /command refuses anything that looks like a browser.
  (multiple-value-bind (status) (bridge-http port :post "/command" :origin +fake-origin+
                                             :body (nlk:json-object "action" "tab.list"))
    (is (eql status 403))))

(deftest chrome-cell-bridge-results-correlate-by-id (with-chrome-bridge (port bridge))
  ;; The fake extension answers each command with its own params so two
  ;; concurrent sends can be told apart; it also delays the first so the
  ;; second's result lands first.
  (with-fake-extension (port (lambda (command)
                               (let ((n (nlk:json-value command :integer "params" "n")))
                                 (when (eql n 1) (sleep 0.3))
                                 (values t (nlk:json-object "n" n)))))
    (let* ((results (make-array 2 :initial-element nil))
           (threads (loop for n from 1 to 2
                          collect (let ((n n))
                                    (nlk:spawn (format nil "sender-~d" n)
                                      (setf (aref results (1- n))
                                            (chrome::bridge-send
                                             bridge "page.probe"
                                             (nlk:json-object "n" n) :timeout 10)))))))
      (dolist (thread threads) (bt2:join-thread thread))
      (is (eql (nlk:json-value (aref results 0) :integer "n") 1))
      (is (eql (nlk:json-value (aref results 1) :integer "n") 2)))))

(deftest chrome-cell-bridge-unknown-result-id-is-404 (with-chrome-bridge (port bridge))
  (multiple-value-bind (status body)
      (bridge-http port :post "/result" :origin +fake-origin+
                   :body (nlk:json-object "id" "chrome-nope" "ok" t "result" 1))
    (is (eql status 404))
    (is (equal (nlk:json-value body :text "error") "unknown command id"))))

(deftest chrome-cell-bridge-error-result-signals-command-failed (with-chrome-bridge (port bridge))
  (with-fake-extension (port (lambda (command)
                               (declare (ignore command))
                               (values nil "boom")))
    (is-present (condition (signals-error chrome:chrome-command-failed
                             (chrome::bridge-send bridge "page.click"
                                                  (nlk:make-json-object)
                                                  :timeout 10)))
      "ok:false signals CHROME-COMMAND-FAILED"
      (is (search "boom" (princ-to-string condition)))
      (is (search "page.click" (princ-to-string condition))))))

(deftest chrome-cell-bridge-send-times-out-and-cleans-up (with-chrome-bridge (port bridge))
  (is-present (condition (signals-error chrome:chrome-timeout
                           (chrome::bridge-send bridge "tab.list" nil :timeout 0.3)))
    "no extension: CHROME-TIMEOUT"
    (is (search "not polling" (princ-to-string condition)))
    (is (search "last seen never" (princ-to-string condition))))
  (is (zerop (hash-table-count bridge.pending)) "timeout drains pending")
  (is (null bridge.queue) "timeout drains the queue")
  ;; Delivered but never answered: the other classification.
  (with-fake-extension (port (lambda (command)
                               (declare (ignore command))
                               (sleep 2)
                               (values t 1)))
    (let ((condition (signals-error chrome:chrome-timeout
                       (chrome::bridge-send bridge "tab.list" nil :timeout 0.5))))
      (is (and condition (search "received the command" (princ-to-string condition)))))))

(deftest chrome-cell-bridge-a-dead-poll-leaves-the-command-for-a-live-one
    (with-chrome-bridge (port bridge))
  ;; A Chrome killed or restarted mid-poll leaves its GET /next parked here up
  ;; to the long-poll deadline. The next command must wait for a live poll,
  ;; not go down the dead connection and time out as "received the command".
  (let* ((dead (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream dead)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "GET /next?name=Nodecode%20Chrome%20Connector%20dead HTTP/1.1~a~aHost: 127.0.0.1~a~a~a~a"
                             #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                     :external-format :latin-1)
                    stream)
    (finish-output stream)
    (is (await () bridge.last-seen) "the dead poll reached the bridge")
    (usocket:socket-close dead))
  (let* ((result nil)
         (sender (nlk:spawn "chrome-sender"
                   (setf result (ignore-errors
                                 (chrome::bridge-send bridge "tab.list" nil :timeout 5))))))
    ;; Only the dead poll is parked when the command is queued, so it is woken first.
    (sleep 0.3)
    (with-fake-extension (port #'echo-handler)
      (bt2:join-thread sender))
    ;; The live poll ran the command the dead one was woken for.
    (is (equal (nlk:json-value result :text "action") "tab.list"))))

(deftest chrome-cell-bridge-a-foreign-poll-never-takes-a-command (with-chrome-bridge (port bridge))
  ;; Another extension on the port -- an old build still loaded from a release
  ;; folder, or pi's own -- is parked and named, never handed a command.
  (with-saved-globals ((chrome::*long-poll-seconds* 0.5))
    (with-fake-extension (port (lambda (command)
                                 (declare (ignore command))
                                 (values t "the foreign extension ran it"))
                          :name "Pi Chrome Connector abc")
      (is (await () (chrome::foreign-polling-p bridge)))
      (is (equal "Pi Chrome Connector abc" bridge.foreign-name))
      (is (equal "Pi Chrome Connector abc"
                 (nlk:json-value (nth-value 1 (bridge-http port :get "/status")) :text "foreignClient")))
      (is-present (condition (signals-error chrome:chrome-timeout
                               (chrome::bridge-send bridge "tab.list" nil :timeout 1)))
        "only a foreign extension polls: the send times out naming it"
        (is (search "'Pi Chrome Connector abc' polling instead of 'Nodecode Chrome Connector'"
                    (princ-to-string condition))))
      ;; Ours, polling beside it, gets the command.
      (with-fake-extension (port #'echo-handler)
        (is (equal "tab.list" (nlk:json-value (chrome::bridge-send bridge "tab.list" nil :timeout 5)
                                              :text "action")))))))

(deftest chrome-cell-bridge-body-is-utf8-and-capped (with-chrome-bridge (port bridge))
  (let ((label (format nil "caf~c ~c" (code-char 233) (code-char #x4e2d))))
    (with-fake-extension (port (lambda (command)
                                 (declare (ignore command))
                                 (values t (nlk:json-object "label" label))))
      (let ((result (chrome::bridge-send bridge "page.snapshot" nil :timeout 10)))
        (is (equal (nlk:json-value result :text "label") label)))))
  (multiple-value-bind (status)
      (bridge-http port :post "/result" :origin +fake-origin+
                   :headers (list (cons "content-length"
                                        (princ-to-string (+ chrome::+max-body-bytes+ 1))))
                   :body (nlk:json-object "id" "x"))
    ;; dexador may refuse to send a mismatched content-length itself; either
    ;; way the bridge never accepts the declared size.
    (is (or (null status) (member status '(413 400))))))

(deftest chrome-cell-bridge-command-route-round-trips-for-local-processes ()
  (with-chrome-bridge (port bridge)
    (with-fake-extension (port #'echo-handler)
      (multiple-value-bind (status body)
          (bridge-http port :post "/command"
                       :body (nlk:json-object "action" "tab.list"
                                              "params" (nlk:json-object "k" "v")
                                              "timeoutMs" 5000))
        (is (eql status 200))
        (is (nlk:json-value body :boolean "ok"))
        (is (equal (nlk:json-value body :text "result" "action") "tab.list"))
        (is (equal (nlk:json-value body :text "result" "params" "k") "v"))))
    (multiple-value-bind (status body) (bridge-http port :post "/command"
                                                    :body (nlk:json-object "params" 1))
      (is (eql status 400))
      (is (equal (nlk:json-value body :text "error") "Missing command action")))))

(deftest chrome-cell-bridge-stop-fails-pending-and-closes-port (let ((outcome nil)))
  (with-chrome-bridge (port bridge)
    (let ((sender (bt2:make-thread
                   (lambda ()
                     (setf outcome
                           (handler-case
                               (progn (chrome::bridge-send bridge "tab.list" nil
                                                           :timeout 10)
                                      :returned)
                             (chrome:chrome-command-failed () :failed)
                             (chrome:chrome-timeout () :timeout)))))))
      (is (await () (plusp (hash-table-count
                             (chrome::bridge-pending bridge)))))
      (let ((started (get-internal-real-time)))
        (chrome::stop-bridge bridge)
        (bt2:join-thread sender)
        (is (< (- (get-internal-real-time) started)
               (* 3 internal-time-units-per-second))))
      (is (eq outcome :failed) "a pending send fails on stop")
      (setf bridge nil)
      (is (not (nle:await-port port :attempts 1)) "port closed after stop"))))

(deftest chrome-cell-bridge-probe-refuses-a-bound-port ()
  (let* ((port (temp-gateway-port))
         (holder (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
    (nlk:with-cleanup ((usocket:socket-close holder))
      (is-present (condition (signals-error error (chrome::start-bridge :port port)))
        "start-bridge signals, never dies, on a bound port"
        (is (search (princ-to-string port) (princ-to-string condition)))
        (is (search "already bound" (princ-to-string condition)))))))

(defun listener-threads-for-port (port &aux (suffix (format nil ":~d" port)))
  "Every live hunchentoot listener thread bound to PORT — the thread that
outlives CLACK:STOP when nobody joins the handler that owns it."
  (remove-if-not (lambda (thread &aux (name (bt2:thread-name thread)))
                   (and (stringp name)
                        (uiop:string-prefix-p "hunchentoot-listener-" name)
                        (uiop:string-suffix-p name suffix)
                        (bt2:thread-alive-p thread)))
                 (bt2:all-threads)))

(deftest chrome-cell-bridge-stop-joins-the-acceptor ()
  ;; CLACK:STOP returns after a fixed sleep while hunchentoot's stop runs in
  ;; the dying handler thread; a process exit in that window reaps the
  ;; listener and handler in arbitrary order and prints a JOIN-THREAD-ERROR
  ;; backtrace (the Ctrl-C incident). STOP-BRIDGE must return only once the
  ;; acceptor is gone — proved by the threads, not by a settle sleep.
  (let* ((port (temp-gateway-port))
         (bridge (chrome::start-bridge :port port)))
    (is (= 1 (length (listener-threads-for-port port))) "one listener while live")
    (chrome::stop-bridge bridge)
    (is (null (listener-threads-for-port port)) "no listener the instant stop returns")
    (is (not (nle:await-port port :attempts 1)) "port refuses the instant stop returns")))

(deftest chrome-cell-bridge-start-latch-unwind-stops-the-acceptor ()
  ;; A Ctrl-C during START-CELLS lands inside START-BRIDGE's acceptance
  ;; latch: the cell is not yet recorded as started, so STOP-CELLS never
  ;; sees this acceptor. The non-local exit must take the acceptor with it
  ;; instead of leaving it for image exit to reap.
  (let ((port (temp-gateway-port)))
    (with-stubbed-fdefinition (nle::http-answers-p (port &key attempts)
                               ;; The acceptor is live before the latch is abandoned, exactly
                               ;; as an interrupt mid-poll would find it.
                               (funcall original port)
                               (throw 'interrupted :interrupted))
      (is (eq :interrupted (catch 'interrupted (chrome::start-bridge :port port)))))
    (is (null (listener-threads-for-port port)) "no orphaned listener after the unwind")
    (is (not (nle:await-port port :attempts 1)) "port refuses after the unwind")))
