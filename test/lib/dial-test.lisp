;;;; dial-test.lisp --- reaching a peer by name: the order, the race, the
;;;; fallback, the stagger, the deadline, the advice on usocket's connect,
;;;; and a refusal under its timeout.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The race is exercised against a connect function the test writes -- an
;;;; address answers at once, fails at once, or answers late -- so every
;;;; branch is deterministic and no network is touched; the advice itself is
;;;; proved on loopback, where "localhost" carries both families on any
;;;; host whose /etc/hosts names ::1.

(in-package #:nodecode.test)

(defclass dial-test-socket (usocket:usocket)
  ((label :initarg :label :reader dial-test-socket-label)
   (closed :initform nil :accessor dial-test-socket-closed)))

(defmethod usocket:socket-close ((socket dial-test-socket))
  (setf (dial-test-socket-closed socket) t))

(defparameter *dial-six* (make-array 16 :element-type '(unsigned-byte 8) :initial-element 1))
(defparameter *dial-four* (make-array 4 :element-type '(unsigned-byte 8) :initial-element 10))

(defun dial-socket (label)
  "A fake socket named LABEL, for a connect to answer with."
  (make-instance 'dial-test-socket :label label))

(defmacro dial-race (six four)
  "NLK:DIAL over *DIAL-SIX* and *DIAL-FOUR*, the connect to each answering what
its form does -- at once, late, or by signalling."
  `(nlk:dial (lambda (address) (if (equalp address *dial-six*) ,six ,four))
             (list *dial-six* *dial-four*)))

(defmacro with-dialled-listener ((socket listen-host dial-host) &body body)
  "BODY with SOCKET dialled by name to DIAL-HOST at the port a fresh listener on
LISTEN-HOST took, both closed on the way out."
  `(let ((listener (usocket:socket-listen ,listen-host 0 :reuse-address t)))
     (nlk:with-cleanup ((usocket:socket-close listener))
       (let ((,socket (usocket:socket-connect ,dial-host (usocket:get-local-port listener))))
         (nlk:with-cleanup ((usocket:socket-close ,socket))
           ,@body)))))

(deftest dial-order-puts-ipv6-first-and-interleaves ()
  (let ((four-b (make-array 4 :element-type '(unsigned-byte 8) :initial-element 20))
        (six-b (make-array 16 :element-type '(unsigned-byte 8) :initial-element 2)))
    (is (equalp (list *dial-six* *dial-four* six-b four-b)
                (nlk:dial-order (list *dial-four* four-b *dial-six* six-b))))
    (is (equalp (list *dial-four* four-b) (nlk:dial-order (list *dial-four* four-b))))
    (is (null (nlk:dial-order '())))))

(deftest dial-answers-the-first-address-when-it-connects-at-once
    (let ((nlk:*dial-stagger-seconds* 0.1) (tried-four nil)))
  (let ((socket (dial-race (dial-socket "six") (progn (setf tried-four t) (dial-socket "four")))))
    (is (equal "six" (dial-test-socket-label socket)))
    (sleep 0.3)
    (is (not tried-four) "the second address is never tried once the first answers")))

(deftest dial-falls-back-at-once-when-the-first-address-fails
    (let ((nlk:*dial-stagger-seconds* 0.5) (start (get-internal-real-time))))
  (let ((socket (dial-race (error 'usocket:host-unreachable-error) (dial-socket "four"))))
    (is (equal "four" (dial-test-socket-label socket)))
    (is (< (nlk:elapsed-ms start) 400) "a failure starts the next address before the stagger")))

(deftest dial-starts-the-next-address-after-the-stagger-and-closes-the-late-one
    (let ((nlk:*dial-stagger-seconds* 0.1) (start (get-internal-real-time)) (late nil)))
  (let ((socket (dial-race (progn (sleep 0.5) (setf late (dial-socket "six")))
                           (dial-socket "four"))))
    (is (equal "four" (dial-test-socket-label socket)))
    (let ((elapsed (nlk:elapsed-ms start)))
      (is (and (<= 80 elapsed) (< elapsed 450)) (format nil "won at the stagger, not before or at the late answer: ~d ms" elapsed)))
    (sleep 0.6)
    (is-present (late "the late attempt still completed")
      (is (dial-test-socket-closed late) "the late socket is closed, not leaked"))))

(deftest dial-signals-the-last-addresss-failure-when-every-attempt-fails
    (let ((nlk:*dial-stagger-seconds* 0.1)))
  (is (signals-error usocket:connection-refused-error
        (dial-race (error 'usocket:host-unreachable-error)
                   (error 'usocket:connection-refused-error)))))

(deftest dial-honours-the-callers-deadline-and-closes-what-lands-after
    (let ((nlk:*dial-stagger-seconds* 0.1) (start (get-internal-real-time)) (landed nil)))
  (is (signals-error sb-sys:deadline-timeout
        (sb-sys:with-deadline (:seconds 0.15)
          (dial-race (progn (sleep 0.5) (setf landed (dial-socket "six")))
                     (progn (sleep 0.5) (dial-socket "four"))))))
  (is (< (nlk:elapsed-ms start) 400) "the deadline cut the wait")
  (sleep 0.7)
  (is-present (landed "the abandoned attempt still completed")
    (is (dial-test-socket-closed landed) "a socket landing after the caller left is closed")))

(deftest dial-advice-passes-a-literal-address-straight-through ()
  (with-dialled-listener (socket "127.0.0.1" "127.0.0.1")
    (is-shape socket (usocket:usocket-p is) (usocket:get-peer-address equalp #(127 0 0 1)))))

(deftest dial-advice-races-a-dual-stack-name-ipv6-first-and-falls-back ()
  ;; Only where "localhost" carries both families; a single-stack host has
  ;; nothing to race and the advice passes the name through.
  (let ((addresses (usocket:get-hosts-by-name "localhost")))
    (cond ((and (find 16 addresses :key #'length) (find 4 addresses :key #'length))
           (with-dialled-listener (socket "::1" "localhost")
             (is (= 16 (length (usocket:get-peer-address socket))) "a listener on ::1 alone is reached: IPv6 is tried, not skipped"))
           (with-dialled-listener (socket "127.0.0.1" "localhost")
             (is (= 4 (length (usocket:get-peer-address socket))) "a listener on 127.0.0.1 alone is reached: IPv6 refused, IPv4 taken")))
          (t (is (= 1 (length (remove-duplicates addresses :test #'equalp))) "single-stack localhost: nothing to race")))))

(defun closed-loopback-port (&aux (listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
  "A loopback port a listener just held and gave back: nothing listens there."
  (prog1 (usocket:get-local-port listener)
    (usocket:socket-close listener)))

(deftest dial-a-port-nothing-listens-on-is-refused-at-once-under-a-timeout
    (let ((port (closed-loopback-port)) (start (get-internal-real-time))))
  ;; Every client here dials with a timeout, and usocket's timed connect
  ;; polls getpeername, which answers a refused connect `not connected' --
  ;; the answer of one still in flight -- until the timeout.
  (is (signals-error usocket:connection-refused-error
        (usocket:socket-connect "127.0.0.1" port :timeout 10)))
  (is (< (nlk:elapsed-ms start) 2000) "the refusal ends the dial, not the 10 s timeout")
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (nlk:with-cleanup ((sb-bsd-sockets:socket-close socket))
      (is (signals-error sb-bsd-sockets:not-connected-error
            (sb-bsd-sockets:socket-peername socket)) "a socket that never dialed is still just not connected"))))

(deftest dial-a-failed-tls-handshake-closes-its-socket ()
  ;; A peer that answers a TLS hello in plain text fails dexador's handshake;
  ;; the socket dexador connected must close with it, which the peer reads as
  ;; the end of the stream, or a reset when the reply sat unread. A socket left
  ;; open holds the read to its deadline.
  (let ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8))))
    (nlk:with-cleanup ((usocket:socket-close listener))
      (let* ((url (format nil "https://127.0.0.1:~d/" (usocket:get-local-port listener)))
             (client (bt2:make-thread (lambda () (handler-case (progn (dex:get url :connect-timeout 5 :read-timeout 5 :use-connection-pool nil :keep-alive nil) :answered) (error () :failed)))
                                      :name "tls to a plain peer"))
             (peer (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (nlk:with-cleanup ((usocket:socket-close peer))
          (let ((stream (usocket:socket-stream peer)))
            (write-sequence (sb-ext:string-to-octets (format nil "HTTP/1.1 400 Bad Request~C~C~C~C" #\Return #\Newline #\Return #\Newline)) stream)
            (finish-output stream)
            (is (eq :failed (bt2:join-thread client)) "the handshake fails")
            (is (eq :closed (handler-case (sb-sys:with-deadline (:seconds 5) (loop while (read-byte stream nil nil)) :closed) (sb-sys:deadline-timeout () :still-open) (stream-error () :closed))) "the client's socket closed (or reset) with its handshake")))))))

(deftest dial-http-without-a-pool-leaves-no-connection-open ()
  ;; A peer that answers keep-alive and then waits: NLK:HTTP without POOL must
  ;; end the connection once the answer is in, for a 200 and for a refused
  ;; status alike, which the peer reads as the end of the stream. A connection
  ;; left open is a descriptor held until the peer gives up, and then one in
  ;; CLOSE-WAIT for as long as the process lives.
  (dolist (status '(200 404))
    (let ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8))))
      (nlk:with-cleanup ((usocket:socket-close listener))
        (let* ((url (format nil "http://127.0.0.1:~d/" (usocket:get-local-port listener)))
               (client (bt2:make-thread (lambda () (handler-case (nth-value 1 (nlk:http :get url :timeout 5)) (error () :failed)))
                                        :name "http without a pool"))
               (peer (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
          (nlk:with-cleanup ((usocket:socket-close peer))
            (let ((stream (usocket:socket-stream peer)))
              (loop with tail = '() for byte = (read-byte stream nil nil)
                    while byte
                    do (setf tail (last (append tail (list byte)) 4))
                    until (equal tail '(13 10 13 10)))
              (write-sequence (sb-ext:string-to-octets (format nil "HTTP/1.1 ~d X~C~CContent-Length: 2~C~C~C~Cok" status #\Return #\Newline #\Return #\Newline #\Return #\Newline) :external-format :latin-1) stream)
              (finish-output stream)
              (is (eql status (bt2:join-thread client)) (format nil "the ~d answer comes back as a value" status))
              (is (eq :closed (handler-case (sb-sys:with-deadline (:seconds 3) (loop while (read-byte stream nil nil)) :closed) (sb-sys:deadline-timeout () :still-open) (stream-error () :closed))) (format nil "after a ~d the client's connection is closed, not kept" status)))))))))

(deftest dial-http-is-cut-at-its-timeout-however-slowly-its-peer-answers ()
  ;; A peer that trickles its headers a line at a time never lets one read
  ;; wait long, so a per-read timeout -- all dexador has, and over TLS not
  ;; even that -- never fires. TIMEOUT is the whole exchange's wall clock,
  ;; capped body or not, and what it signals any handler of a deadline or of
  ;; an error takes.
  (dolist (cap '(nil 1000))
    (let ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8))))
      (nlk:with-cleanup ((usocket:socket-close listener))
        (let* ((url (format nil "http://127.0.0.1:~d/" (usocket:get-local-port listener)))
               (start (get-internal-real-time))
               (client (bt2:make-thread (lambda ()
                                          (handler-case (progn (nlk:http :get url :timeout 1 :max-bytes cap) :answered)
                                            (error (condition) condition)))
                                        :name "http to a trickling peer"))
               (peer (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
          (nlk:with-cleanup ((usocket:socket-close peer))
            (let ((stream (usocket:socket-stream peer)))
              (ignore-errors
               (write-sequence (sb-ext:string-to-octets (format nil "HTTP/1.1 200 OK~C~C" #\Return #\Newline)) stream)
               (loop repeat 20 while (bt2:thread-alive-p client)
                     do (finish-output stream)
                        (sleep 0.25)
                        (write-sequence (sb-ext:string-to-octets (format nil "X-Slow: a~C~C" #\Return #\Newline)) stream)))
              (let ((outcome (bt2:join-thread client)))
                (is (typep outcome 'nlk::http-timeout) (format nil "cap ~a: ~a" cap outcome))
                (is (typep outcome 'sb-sys:deadline-timeout) "a deadline's handler takes it too")
                (is (< (nlk:elapsed-ms start) 3000) "cut at its one second, not the peer's five")))))))))

(deftest dial-client-contexts-share-one-store-per-verify-mode ()
  ;; Every HTTPS connection dexador opens asks for a context: at the default
  ;; location it is one per verify mode, and a caller's free leaves it whole.
  (cl+ssl:ensure-initialized)
  (with-saved-globals ((nlk::*client-contexts* '()))
    (flet ((ask (mode) (cl+ssl:make-context :verify-mode mode :verify-location :default)))
      (let ((a (ask cl+ssl:+ssl-verify-peer+)) (b (ask cl+ssl:+ssl-verify-peer+))
            (none (ask cl+ssl:+ssl-verify-none+)))
        (is (cffi:pointer-eq a b) "one store for every verifying connection")
        (is (not (cffi:pointer-eq a none)) "another mode, another context")
        (cl+ssl::ssl-ctx-free a)
        (cl+ssl::ssl-ctx-free b)
        (let ((ssl (cl+ssl::ssl-new (ask cl+ssl:+ssl-verify-peer+))))
          (is (not (cffi:null-pointer-p ssl)) "both callers freed theirs and it still serves")
          (cl+ssl::ssl-free ssl))))
    (is-present (bundle (find-if #'probe-file '("/etc/ssl/certs/ca-certificates.crt"
                                                "/etc/pki/tls/certs/ca-bundle.crt"
                                                "/etc/ssl/ca-bundle.pem" "/etc/ssl/cert.pem")))
      (let ((one (cl+ssl:make-context :verify-location bundle))
            (two (cl+ssl:make-context :verify-location bundle)))
        (is (not (cffi:pointer-eq one two)) "a named location is fresh: its file may change")))))
