;;;; sandbox-egress-test.lisp --- the jail's way out: the keys' stand-ins,
;;;; the verdict on a host, the jail's brokered dial, and the egress itself
;;;; answering on a real socket.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun egress-test-octets (text)
  "TEXT as Latin-1 octets."
  (sb-ext:string-to-octets text :external-format :latin-1))

(defun egress-test-address (text)
  "TEXT, a literal address, as its octet vector."
  (coerce (first (usocket:get-hosts-by-name text)) '(vector (unsigned-byte 8))))

(deftest sandbox-egress-stand-ins-name-their-provider ()
  (let ((openai (nle::sandbox-key-stand-in "openai"))
        (other (nle::sandbox-key-stand-in "my.provider-2")))
    (is (uiop:string-prefix-p nlk:+jail-key-prefix+ openai) "a stand-in starts with the jail's prefix")
    (is (notany (lambda (c) (find c " :/?&=%")) other) "and sits in a header or a query as written")
    (is (equal '("openai" "my.provider-2")
               (nle::sandbox-key-stand-in-providers (format nil "Bearer ~a&key=~a&again=~a" openai other openai))) "each provider a text carries, once")
    (is (null (nle::sandbox-key-stand-in-providers (format nil "~a6f" nlk:+jail-key-prefix+))) "a prefix without its end names nothing")
    (is (string= "Bearer sk-real" (nle::sandbox-swap-stand-ins (format nil "Bearer ~a" openai) '(("openai" . "sk-real")))) "the swap puts the key where its stand-in was")))

#-win32
(deftest sandbox-egress-private-addresses ()
  (dolist (text '("127.0.0.1" "10.1.2.3" "172.16.0.1" "172.31.255.255" "192.168.1.1" "169.254.169.254"
                  "100.64.0.1" "0.0.0.0" "224.0.0.1" "::1" "::" "fd00::1" "fe80::1" "::ffff:127.0.0.1"))
    (is (nle::sandbox-private-address-p (egress-test-address text)) (format nil "~a is no public address" text)))
  (dolist (text '("1.1.1.1" "140.82.112.3" "172.32.0.1" "2606:4700::1111"))
    (is (not (nle::sandbox-private-address-p (egress-test-address text))) (format nil "~a is public" text))))

(deftest sandbox-egress-host-names ()
  (let ((hosts '("github.com" "*.discord.gg" ".example.org")))
    (is (nle::sandbox-host-allowed-p "GitHub.com" hosts) "a name, any case")
    (is (not (nle::sandbox-host-allowed-p "evil-github.com" hosts)) "never a name that only ends like one")
    (is (not (nle::sandbox-host-allowed-p "api.github.com" hosts)) "nor a name under one written bare")
    (is (nle::sandbox-host-allowed-p "gateway.discord.gg" hosts) "*.NAME admits the names under it")
    (is (not (nle::sandbox-host-allowed-p "discord.gg" hosts)) "and not NAME itself")
    (is (and (nle::sandbox-host-allowed-p "example.org" hosts) (nle::sandbox-host-allowed-p "a.example.org" hosts)) ".NAME admits NAME and the names under it")))

(deftest sandbox-egress-verdict-refuses-the-host-and-its-networks ()
  (let* ((public (egress-test-address "140.82.112.3"))
         (loopback (egress-test-address "127.0.0.1"))
         (resolve (lambda (host) (if (search "rebound" host) (list loopback) (list public))))
         (egress (list :hosts '("api.github.com" "rebound.example")
                       :providers '((:id "local" :origins ("http://127.0.0.1:11434"))))))
    (is (equalp (list public) (nle::sandbox-egress-verdict "api.github.com" 443 egress :resolve resolve)) "an allowed host is dialled at the address checked")
    (multiple-value-bind (addresses reason) (nle::sandbox-egress-verdict "example.com" 443 egress :resolve resolve)
      (is (and (null addresses) (search "not a host this jail may reach" reason)) "any other host is refused"))
    (multiple-value-bind (addresses reason) (nle::sandbox-egress-verdict "rebound.example" 443 egress :resolve resolve)
      (is (and (null addresses) (search "private network" reason)) "an allowed name that resolves to the host's loopback is refused"))
    (is (nle::sandbox-egress-verdict "127.0.0.1" 11434 egress :resolve resolve) "a configured provider is reached on the host's loopback")
    (is (not (nle::sandbox-egress-verdict "127.0.0.1" 22 egress :resolve resolve)) "another port there is not")
    (let ((internet (list* :internet t egress)))
      (is (nle::sandbox-egress-verdict "example.com" 443 internet :resolve resolve) "the internet setting admits any public host")
      (is (not (nle::sandbox-egress-verdict "rebound.example" 443 internet :resolve resolve)) "and still no private address"))))

(deftest sandbox-egress-jailed-dial-goes-to-the-broker ()
  (let ((saved-port (uiop:getenv "NODECODE_SANDBOX_EGRESS_PORT"))
        (saved-origins (uiop:getenv "NODECODE_SANDBOX_HOST_ORIGINS"))
        (stand-in (nle::sandbox-key-stand-in "openai")))
    (unwind-protect
         (progn
           (nlk::set-environment-variable "NODECODE_SANDBOX_EGRESS_PORT" "")
           (is (string= "https://api.openai.com/v1/x"
                        (nlk:jail-brokered-url "https://api.openai.com/v1/x" (list (cons "Authorization" stand-in)))) "outside a jail a dial is its own")
           (nlk::set-environment-variable "NODECODE_SANDBOX_EGRESS_PORT" "3128")
           (nlk::set-environment-variable "NODECODE_SANDBOX_HOST_ORIGINS" "http://127.0.0.1:11434")
           (is (string= "https://models.dev/api.json" (nlk:jail-brokered-url "https://models.dev/api.json")) "in a jail a dial with no stand-in takes the proxy as before")
           (let ((url (nlk:jail-brokered-url "https://api.openai.com/v1/chat/completions"
                                             (list (cons "Authorization" (format nil "Bearer ~a" stand-in))))))
             (is (string= "http://127.0.0.1:3128/.nodecode-broker/https/api.openai.com:443/v1/chat/completions" url) "a stand-in in a header sends the dial to the broker")
             (is (equal "https://api.openai.com:443/v1/chat/completions"
                        (nle::sandbox-broker-target (subseq url (length "http://127.0.0.1:3128")))) "which reads the address back"))
           (is (search "/.nodecode-broker/https/g.example:443/v1/m?key="
                       (nlk:jail-brokered-url (format nil "https://g.example/v1/m?key=~a" stand-in))) "so does one in the query, kept in it")
           (is (string= "http://127.0.0.1:3128/.nodecode-broker/http/127.0.0.1:11434/api/chat"
                        (nlk:jail-brokered-url "http://127.0.0.1:11434/api/chat")) "a provider on the host's loopback is the broker's to reach")
           (is (string= "http://127.0.0.1:41234/x" (nlk:jail-brokered-url "http://127.0.0.1:41234/x")) "the jail's own loopback stays its own"))
      (nlk::set-environment-variable "NODECODE_SANDBOX_EGRESS_PORT" (or saved-port ""))
      (nlk::set-environment-variable "NODECODE_SANDBOX_HOST_ORIGINS" (or saved-origins "")))))

(deftest sandbox-egress-stand-in-files-hold-no-key ()
  (let* ((auth (nlk:decode-json "{\"api_keys\": {\"openai\": {\"key\": \"sk-real-1\"}, \"cf\": {\"key\": \"sk-real-2\", \"vars\": {\"ACCOUNT\": \"acc-9\"}}}, \"machine_link\": {\"token\": \"link-secret\"}}"))
         (providers '((:id "openai" :origins ("https://api.openai.com:443") :keyed t)
                      (:id "cf" :origins ("https://api.cf.example:443") :keyed t)
                      (:id "local" :origins ("http://127.0.0.1:11434") :keyed nil)))
         (text (nle::sandbox-stand-in-auth auth providers))
         (read (nlk:decode-json text)))
    (is (notany (lambda (secret) (search secret text)) '("sk-real" "link-secret")) "the jail's auth.json holds no key and no link")
    (is (string= (nle::sandbox-key-stand-in "openai") (nle::json-value read :text "api_keys" "openai" "key")) "a stand-in where the key was")
    (is (string= "acc-9" (nle::json-value read :text "api_keys" "cf" "vars" "ACCOUNT")) "and the vars of its address")
    (is (null (nle::json-value read :object "api_keys" "local")) "a provider with no key gets none"))
  (let ((config (nle::sandbox-stand-in-config
                 (format nil "// mine~%{\"providers\": {\"fake\": {\"base_url\": \"http://h/v1\", \"api_key\": \"sk-cfg\"}, \"cmd\": {\"api_key\": \"!pass show k\"}}}"))))
    (is (and config (not (search "sk-cfg" config)) (not (search "pass show" config))) "config.jsonc's keys and key commands are stand-ins")
    (is (string= (nle::sandbox-key-stand-in "fake") (nle::json-value (nlk:decode-json config) :text "providers" "fake" "api_key"))))
  (is (null (nle::sandbox-stand-in-config "{\"providers\": {\"x\": {\"base_url\": \"http://h\"}}}")) "a config.jsonc with no key is the jail's as it is"))

#-win32
(deftest sandbox-egress-argv-mounts-the-stand-ins ()
  (let ((argv (nle::sandbox-bwrap-argv '("/opt/nc/nodecode" "serve")
                                       :home "/h/.nodecode/" :folder "/w/proj/" :binary "/opt/nc/nodecode"
                                       :hidden '("/h/.nodecode/auth.json")
                                       :stand-ins '(("/h/.nodecode/auth.json" . "/h/.nodecode/sandbox/stand-in/auth.json")
                                                    ("/h/.nodecode/config.jsonc" . "/h/.nodecode/sandbox/stand-in/config.jsonc"))
                                       :jail-environment '("OPENAI_API_KEY" "stand-in")
                                       :run "/h/.nodecode/sandbox/run/" :cache "/h/.nodecode/sandbox/cache/"
                                       :temporary "/h/.nodecode/sandbox/tmp/" :port 41234 :egress-port 3128
                                       :environment '("PATH=/usr/bin" "OPENAI_API_KEY=sk-real") :exists (constantly t)
                                       :link (constantly nil) :directory-p (constantly nil) :homes '("/home/u/"))))
    (is (search '("--ro-bind" "/h/.nodecode/sandbox/stand-in/auth.json" "/h/.nodecode/auth.json") argv :test #'equal) "auth.json is its stand-in")
    (is (not (search '("--ro-bind" "/dev/null" "/h/.nodecode/auth.json") argv :test #'equal)) "not the null device")
    (is (search '("--ro-bind" "/h/.nodecode/sandbox/stand-in/config.jsonc" "/h/.nodecode/config.jsonc") argv :test #'equal) "config.jsonc is its stand-in")
    (is (search '("--ro-bind" "/h/.nodecode/sandbox/egress.log" "/h/.nodecode/sandbox/egress.log") argv :test #'equal) "the egress log is read-only in the jail")
    (is (string= "stand-in" (sandbox-test-setenv argv "OPENAI_API_KEY")) "the environment's key is a stand-in")
    (is (notany (lambda (word) (search "sk-real" word)) argv) "and no key is in the argv")))

;;; --- the egress on a real socket ---------------------------------------------------

(defun egress-test-listener ()
  "A socket listening on an ephemeral port of the loopback, and its port."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 16)
    (values socket (nth-value 1 (sb-bsd-sockets:socket-name socket)))))

(defun egress-test-exchange (port request)
  "Send REQUEST, text, to the loopback PORT and answer all it sends back
until it closes, as text."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
        (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (unwind-protect
         (sb-sys:with-deadline (:seconds 20)
           (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
           (sb-bsd-sockets:socket-send socket (egress-test-octets request) nil)
           (loop (let ((got (nth-value 1 (sb-bsd-sockets:socket-receive socket buffer nil))))
                   (when (or (null got) (zerop got)) (return))
                   (loop for i below got do (vector-push-extend (aref buffer i) out)))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))
    (sb-ext:octets-to-string out :external-format :latin-1)))

(defun egress-test-upstream (seen)
  "A provider on an ephemeral loopback port that answers one request with an
event stream and pushes the request's text onto the cons SEEN. => its port."
  (multiple-value-bind (listener port) (egress-test-listener)
    (nlk::spawn "egress-test-upstream"
      (unwind-protect
           (let ((client (sb-bsd-sockets:socket-accept listener)))
             (unwind-protect
                  (multiple-value-bind (head rest) (nle::sandbox-read-head client)
                    (let* ((length (parse-integer (or (nle::sandbox-header (third (nle::sandbox-parse-head head)) "content-length") "0")))
                           (body (make-array length :element-type '(unsigned-byte 8))))
                      (replace body rest)
                      (loop with at = (min length (length rest)) while (< at length)
                            do (multiple-value-bind (got count) (sb-bsd-sockets:socket-receive client nil (- length at)
                                                                                               :element-type '(unsigned-byte 8))
                                 (replace body got :start1 at :end2 count)
                                 (incf at count)))
                      (push (format nil "~a~%~%~a" head (sb-ext:octets-to-string body :external-format :latin-1)) (car seen))
                      (nle::sandbox-send-text
                       client (format nil "HTTP/1.1 200 OK~c~cContent-Type: text/event-stream~c~cTransfer-Encoding: chunked~c~c~c~c~a"
                                      #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                      (format nil "c~c~cdata: PONG~c~c~c~c0~c~c~c~c"
                                              #\Return #\Newline #\Newline #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline)))))
               (ignore-errors (sb-bsd-sockets:socket-close client))))
        (ignore-errors (sb-bsd-sockets:socket-close listener))))
    port))

(deftest sandbox-egress-answers-the-jail ()
  (let* ((home (temp-path "egress"))
         (auth (merge-pathnames "auth.json" home))
         (log (merge-pathnames "egress.log" home))
         (seen (list nil))
         (upstream (egress-test-upstream seen))
         (stand-in (nle::sandbox-key-stand-in "openai")))
    (ensure-directories-exist home)
    (with-open-file (out auth :direction :output) (write-string "{\"api_keys\": {\"openai\": {\"key\": \"sk-real-outside\"}}}" out))
    (let ((nle::*auth-file-path* auth)
          (nle::*secrets-directory* (merge-pathnames "secrets/" home))
          (nle::*api-key* nil))
      (is (string= "sk-real-outside" (nle::sandbox-egress-key "openai" "http://127.0.0.1:1/v1")) "the broker's key is the outside's own resolution"))
    (multiple-value-bind (listener port) (egress-test-listener)
      (unwind-protect
           (let ((nle::*auth-file-path* auth))
             (nle::sandbox-egress-start
              :listener listener
              :egress (list :hosts '("localhost") :log (uiop:native-namestring log)
                            :credential (lambda (id url)
                                          ;; this home alone: its auth.json, no secrets/, no flag's key
                                          (let ((nle::*auth-file-path* auth)
                                                (nle::*secrets-directory* (merge-pathnames "secrets/" home))
                                                (nle::*api-key* nil))
                                            (nle::sandbox-egress-key id url)))
                            :providers (list (list :id "openai" :keyed t
                                                   :origins (list (format nil "http://127.0.0.1:~d" upstream))))))
             (let ((answer (egress-test-exchange
                            port (format nil "POST /.nodecode-broker/http/127.0.0.1:~d/v1/chat/completions HTTP/1.1~c~cHost: 127.0.0.1:3128~c~cAuthorization: Bearer ~a~c~cContent-Length: 2~c~c~c~c{}"
                                         upstream #\Return #\Newline #\Return #\Newline stand-in #\Return #\Newline
                                         #\Return #\Newline #\Return #\Newline))))
               (is (uiop:string-prefix-p "HTTP/1.1 200" answer) "the broker answers with the provider's status")
               (is (search "data: PONG" answer) "and its stream")
               (is (search "Connection: close" answer) "and closes after it")
               (let ((request (first (car seen))))
                 (is (and request (search "Bearer sk-real-outside" request)) "the provider got the key from outside the jail")
                 (is (and request (not (search nlk:+jail-key-prefix+ request))) "and no stand-in")
                 (is (and request (uiop:string-suffix-p request "{}")) "and the body")))
             (let ((answer (egress-test-exchange
                            port (format nil "GET /.nodecode-broker/http/127.0.0.1:9/x HTTP/1.1~c~cAuthorization: Bearer ~a~c~c~c~c"
                                         #\Return #\Newline stand-in #\Return #\Newline #\Return #\Newline))))
               (is (uiop:string-prefix-p "HTTP/1.1 403" answer) "a key's stand-in toward another address is refused"))
             (let ((answer (egress-test-exchange
                            port (format nil "CONNECT example.com:443 HTTP/1.1~c~c~c~c" #\Return #\Newline #\Return #\Newline))))
               (is (and (uiop:string-prefix-p "HTTP/1.1 403" answer) (search "not a host this jail may reach" answer)) "a tunnel to a host not allowed is refused"))
             (let ((answer (egress-test-exchange
                            port (format nil "CONNECT localhost:22 HTTP/1.1~c~c~c~c" #\Return #\Newline #\Return #\Newline))))
               (is (and (uiop:string-prefix-p "HTTP/1.1 403" answer) (search "private network" answer)) "an allowed name on the host's loopback is refused"))
             (let ((text (uiop:read-file-string log)))
               (is (search "allow broker POST" text) "each verdict is logged")
               (is (search "refuse CONNECT example.com:443" text))
               (is (not (search "sk-real-outside" text)) "and never a key")))
        (ignore-errors (sb-bsd-sockets:socket-close listener))
        (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)))))
