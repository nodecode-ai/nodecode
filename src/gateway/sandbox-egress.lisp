;;;; sandbox-egress.lisp --- the jail's one way out: a proxy outside the
;;;; jail, and the broker that adds the keys. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A jail has a network namespace of its own: its loopback and nothing else.
;;;; Its one way out is the listener it hands the launcher
;;;; (SANDBOX-HAND-OUT-EGRESS), which the launcher answers on here, outside the
;;;; jail, with the host's network and the home's keys. Three kinds of request
;;;; come in on it:
;;;;  - CONNECT HOST:PORT, the tunnel a TLS client takes through HTTPS_PROXY
;;;;    (git, curl, pip, nodecode's own hub and catalog fetches): opened when
;;;;    the jail may reach HOST (SANDBOX-EGRESS-VERDICT), its bytes carried as
;;;;    they are -- the TLS is the client's own, end to end;
;;;;  - an absolute-form http:// request, HTTP_PROXY's plain kind: the same
;;;;    verdict, the request passed on in origin form;
;;;;  - /.nodecode-broker/SCHEME/HOST:PORT/PATH, the broker, for a provider
;;;;    call the jailed organism makes (NLK:JAIL-BROKERED-URL). The jail holds
;;;;    no key, only stand-ins (SANDBOX-KEY-STAND-IN) where its auth.json,
;;;;    config.jsonc and environment had one; the broker swaps each for the
;;;;    key outside, and only toward the address that key's own provider is
;;;;    served at, and makes the call itself, TLS and all. It carries provider
;;;;    calls alone: one the operator configured, at its own address.
;;;; A tunnel or a plain request never reaches the host's loopback or a private
;;;; network -- nor does a name that resolves there; the address the verdict
;;;; checked is the one dialled -- unless a provider the operator configured is
;;;; served there. Each verdict is a line of <home>/sandbox/egress.log, which
;;;; the jail reads but cannot write; a key is never in it.

(in-package #:nodecode.evolved)

;;; --- stand-ins ----------------------------------------------------------------

(defun sandbox-key-stand-in (provider)
  "What a jail reads in place of PROVIDER's key: +JAIL-KEY-PREFIX+, the id's
UTF-8 octets in hexadecimal, then -x."
  (format nil "~a~(~{~2,'0x~}~)-x" nlk:+jail-key-prefix+
          (coerce (sb-ext:string-to-octets provider :external-format :utf-8) 'list)))

(defun sandbox-key-stand-in-providers (text)
  "The provider ids whose stand-ins TEXT carries, each once, in order."
  (let ((ids '()) (prefix nlk:+jail-key-prefix+))
    (loop for at = (search prefix text) then (search prefix text :start2 (1+ at))
          while at
          do (let* ((start (+ at (length prefix)))
                    (end (or (position-if-not (lambda (c) (digit-char-p c 16)) text :start start) (length text))))
               (when (and (evenp (- end start)) (< start end)
                          (uiop:string-prefix-p "-x" (subseq text end)))
                 (let ((id (ignore-errors
                            (sb-ext:octets-to-string
                             (coerce (loop for i from start below end by 2
                                           collect (parse-integer text :start i :end (+ i 2) :radix 16))
                                     '(vector (unsigned-byte 8)))
                             :external-format :utf-8))))
                   (when id (pushnew id ids :test #'string=))))))
    (nreverse ids)))

(defun sandbox-swap-stand-ins (text keys)
  "TEXT with each stand-in of KEYS, an alist (PROVIDER . KEY), replaced by
its key."
  (loop for (provider . key) in keys
        for stand-in = (sandbox-key-stand-in provider)
        do (loop for at = (search stand-in text)
                 while at
                 do (setf text (concatenate 'string (subseq text 0 at) key
                                            (subseq text (+ at (length stand-in))))))
        finally (return text)))

;;; --- what the jail may reach ----------------------------------------------------

(defun sandbox-private-address-p (address)
  "Whether ADDRESS, an octet vector of 4 (IPv4) or 16 (IPv6), is no public
address: this host's own (loopback, unspecified), a private or shared
network's (RFC 1918, 100.64/10, fc00::/7), link-local, benchmark, multicast or
reserved. An IPv4 address mapped into IPv6 is read as the IPv4 one."
  (case (length address)
    (4 (destructuring-bind (a b &rest _) (coerce address 'list)
         (declare (ignore _))
         (or (= a 0) (= a 10) (= a 127) (>= a 224)
             (and (= a 100) (<= 64 b 127))
             (and (= a 169) (= b 254))
             (and (= a 172) (<= 16 b 31))
             (and (= a 192) (= b 168))
             (and (= a 192) (= b 0) (zerop (aref address 2)))
             (and (= a 198) (<= 18 b 19)))))
    (16 (let ((head (aref address 0)))
          (or (every #'zerop (subseq address 0 15)) ; :: and ::1
              (and (every #'zerop (subseq address 0 10))
                   (= #xff (aref address 10) (aref address 11))
                   (sandbox-private-address-p (subseq address 12 16)))
              (= #xfc (logand head #xfe))                          ; fc00::/7
              (and (= head #xfe) (= #x80 (logand (aref address 1) #xc0))) ; fe80::/10
              (= head #xff))))                                      ; multicast
    (t t)))

(defun sandbox-host-allowed-p (host hosts)
  "Whether HOSTS, the jail's allowed names, admit HOST: a name equal to one,
any case, or under one written *.NAME or .NAME (.NAME admits NAME itself)."
  (let ((host (string-downcase (string-right-trim "." (string-trim "[]" host)))))
    (some (lambda (entry)
            (let ((entry (string-downcase (nlk:trimmed entry))))
              (cond ((uiop:string-prefix-p "*." entry)
                     (uiop:string-suffix-p host (subseq entry 1)))
                    ((uiop:string-prefix-p "." entry)
                     (or (string= host (subseq entry 1)) (uiop:string-suffix-p host entry)))
                    (t (string= host entry)))))
          hosts)))

(defun sandbox-egress-providers-at (egress &key origin host port)
  "The providers of EGRESS (its :PROVIDERS, plists with :ID and :ORIGINS)
served at ORIGIN, or at HOST and PORT whatever the scheme."
  (loop for provider in (getf egress :providers)
        when (some (lambda (served)
                     (if origin
                         (string= served origin)
                         (let ((at (search "://" served)))
                           (and at (string= (subseq served (+ at 3))
                                            (format nil "~:[~a~;[~a]~]:~d" (find #\: host) (string-downcase host) port))))))
                   (getf provider :origins))
          collect provider))

(defun sandbox-resolve (host)
  "HOST's addresses as octet vectors, a literal address as itself; NIL when
it does not resolve."
  (let ((host (string-trim "[]" host)))
    (mapcar (lambda (address) (coerce address '(vector (unsigned-byte 8))))
            (ignore-errors (usocket:get-hosts-by-name host)))))

(defun sandbox-egress-verdict (host port egress &key (resolve #'sandbox-resolve))
  "Whether the jail may reach HOST at PORT, under EGRESS (:HOSTS the names it
may reach, :INTERNET true to admit every public one, :PROVIDERS the
configured providers' addresses). => (values ADDRESSES NIL), the addresses to
dial, or (values NIL REASON). A provider's own address is reached wherever
it is; any other never on this host's loopback or a private network, so a
name that resolves there is refused too."
  (let ((provider (sandbox-egress-providers-at egress :host host :port port)))
    (if (not (or provider (getf egress :internet) (sandbox-host-allowed-p host (getf egress :hosts))))
        (values nil (format nil "~a is not a host this jail may reach" host))
        (let ((addresses (funcall resolve host)))
          (cond ((null addresses) (values nil (format nil "~a does not resolve" host)))
                ((and (not provider) (some #'sandbox-private-address-p addresses))
                 (values nil (format nil "~a is this host's own or a private network's address" host)))
                (t (values addresses nil)))))))

;;; --- the wire -------------------------------------------------------------------

(defparameter +sandbox-egress-head-limit+ 65536
  "The most bytes a request's head may take before it is refused.")

(defun sandbox-send-octets (socket octets &key (start 0) (end (length octets)))
  "Send OCTETS from START to END on SOCKET, whole."
  (loop while (< start end)
        do (let ((sent (sb-bsd-sockets:socket-send socket (subseq octets start end) (- end start) :nosignal t)))
             (unless (and sent (plusp sent)) (error "the peer stopped reading"))
             (incf start sent))))

(defun sandbox-send-text (socket text)
  "Send TEXT, Latin-1, on SOCKET."
  (sandbox-send-octets socket (sb-ext:string-to-octets text :external-format :latin-1)))

(defun sandbox-read-head (socket)
  "Read a request's head from SOCKET, through its blank line. => (values
HEAD REST): HEAD the text before the blank line (Latin-1, so it goes back
out byte for byte), REST the octets read past it; NIL when the peer closed
first or the head passed +SANDBOX-EGRESS-HEAD-LIMIT+."
  (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8)))
        (seen (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (blank (coerce #(13 10 13 10) '(vector (unsigned-byte 8)))))
    (loop
      (let ((length (nth-value 1 (sb-bsd-sockets:socket-receive socket buffer nil))))
        (when (or (null length) (zerop length)) (return nil))
        (loop for i below length do (vector-push-extend (aref buffer i) seen))
        (let ((at (search blank seen)))
          (when at
            (return (values (sb-ext:octets-to-string (subseq seen 0 at) :external-format :latin-1)
                            (subseq seen (+ at 4))))))
        (when (> (length seen) +sandbox-egress-head-limit+) (return nil))))))

(defun sandbox-parse-head (head)
  "HEAD, a request's head, as (METHOD TARGET HEADERS): HEADERS an alist of
(NAME . VALUE), names as written, in order; NIL when its first line is no
request line."
  (let* ((lines (uiop:split-string head :separator '(#\Newline)))
         (lines (mapcar (lambda (line) (string-right-trim '(#\Return) line)) lines))
         (words (uiop:split-string (first lines) :separator '(#\Space))))
    (when (and (= 3 (length words)) (uiop:string-prefix-p "HTTP/" (third words)))
      (list (first words) (second words)
            (loop for line in (rest lines)
                  for colon = (position #\: line)
                  when colon
                    collect (cons (subseq line 0 colon) (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))))))

(defun sandbox-header (headers name)
  "The value of the header NAME in HEADERS, any case, or NIL."
  (cdr (assoc name headers :test #'string-equal)))

(defun sandbox-host-port (authority default-port)
  "AUTHORITY, host[:port] or [v6][:port], as (values HOST PORT), or NIL."
  (let* ((close (and (plusp (length authority)) (char= (char authority 0) #\[) (position #\] authority)))
         (colon (if close
                    (and (< (1+ close) (length authority)) (char= (char authority (1+ close)) #\:) (1+ close))
                    (let ((at (position #\: authority :from-end t)))
                      (and at (= 1 (count #\: authority)) at))))
         (host (cond (close (subseq authority 1 close)) (colon (subseq authority 0 colon)) (t authority)))
         (port (if colon (ignore-errors (parse-integer authority :start (1+ colon))) default-port)))
    (when (and (plusp (length host)) (integerp port) (< 0 port 65536))
      (values host port))))

(defun sandbox-egress-token-p (text token)
  "Whether TEXT is TOKEN, compared in a time that does not tell where they
differ."
  (and (stringp text) (stringp token) (= (length text) (length token))
       (zerop (loop for a across text for b across token
                    sum (logxor (char-code a) (char-code b))))))

(defun sandbox-proxy-authorization-token (value)
  "The password of VALUE, a Proxy-Authorization header's Basic credentials,
or NIL."
  (let* ((value (and value (nlk:trimmed value)))
         (space (and value (position #\Space value)))
         (decoded (and space (string-equal "basic" (subseq value 0 space))
                       (ignore-errors (cl-base64:base64-string-to-string (nlk:trimmed (subseq value space))))))
         (colon (and decoded (position #\: decoded))))
    (and colon (subseq decoded (1+ colon)))))

(defun sandbox-egress-admitted (target headers egress)
  "TARGET as the egress serves it, or NIL when it may not be served. Under an
EGRESS with no :TOKEN -- a jail with a loopback of its own, which nothing
else reaches -- every request, TARGET as it is. With one -- a jail on the
host's loopback (macOS), where every program of the machine reaches the
port -- only the requests the jail was handed the token for: a broker path
that begins with it, TARGET with it taken out, and a proxy request whose
Proxy-Authorization carries it."
  (let ((token (getf egress :token)))
    (cond ((null token) target)
          ((uiop:string-prefix-p nlk:+jail-broker-path+ target)
           (let* ((after (subseq target (length nlk:+jail-broker-path+)))
                  (slash (position #\/ after)))
             (and slash (sandbox-egress-token-p (subseq after 0 slash) token)
                  (concatenate 'string nlk:+jail-broker-path+ (subseq after (1+ slash))))))
          ((sandbox-egress-token-p
            (sandbox-proxy-authorization-token (sandbox-header headers "proxy-authorization")) token)
           target))))

(defun sandbox-egress-refuse (socket status reason)
  "Answer the jail's request on SOCKET with STATUS and REASON, and close."
  (let ((body (format nil "nodecode --sandbox: ~a~%" reason)))
    (ignore-errors
     (sandbox-send-text socket (format nil "HTTP/1.1 ~d ~a~c~cContent-Type: text/plain; charset=utf-8~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c~a"
                                       status (case status (400 "Bad Request") (403 "Forbidden") (407 "Proxy Authentication Required")
                                         (411 "Length Required") (t "Bad Gateway"))
                                       #\Return #\Newline #\Return #\Newline
                                       (length (sb-ext:string-to-octets body :external-format :utf-8))
                                       #\Return #\Newline #\Return #\Newline #\Return #\Newline body)))))

(defvar *sandbox-egress-log-lock* (bt2:make-lock :name "sandbox egress log"))

(defun sandbox-egress-log (egress control &rest arguments)
  "Append a line, the time and CONTROL applied to ARGUMENTS, to EGRESS's
:LOG file."
  (when-let (path (getf egress :log))
    (bt2:with-lock-held (*sandbox-egress-log-lock*)
      (ignore-errors
       (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create
                                 :external-format :utf-8)
         (format out "~a ~?~%" (nlk::iso-now) control arguments))))))

(defun sandbox-egress-dial (addresses port)
  "A socket connected to the first of ADDRESSES that answers at PORT."
  (let ((last nil))
    (dolist (address addresses (error (or last "no address answered")))
      (handler-case (return (usocket:socket-connect address port :element-type '(unsigned-byte 8)
                                                                 :timeout 15 :nodelay t))
        (error (condition) (setf last condition))))))

(defun sandbox-egress-carry (client upstream rest)
  "Send REST to UPSTREAM, a usocket, then carry bytes both ways between it
and CLIENT until both ends close."
  (let ((socket (usocket:socket upstream)))
    (unwind-protect
         (progn
           (when (plusp (length rest)) (sandbox-send-octets socket rest))
           (let ((back (spawn "sandbox-egress-back" (sandbox-pump socket client))))
             (sandbox-pump client socket)
             (ignore-errors (bt2:join-thread back))))
      (ignore-errors (usocket:socket-close upstream)))))

;;; --- the three ways out -----------------------------------------------------------

(defun sandbox-egress-tunnel (client target rest egress)
  "CONNECT TARGET: open the tunnel when the verdict allows, else refuse."
  (multiple-value-bind (host port) (sandbox-host-port target nil)
    (if (null host)
        (sandbox-egress-refuse client 400 (format nil "CONNECT ~a names no host and port" target))
        (multiple-value-bind (addresses reason) (sandbox-egress-verdict host port egress)
          (sandbox-egress-log egress "~:[refuse~;allow~] CONNECT ~a:~d~@[ (~a)~]" addresses host port reason)
          (if (null addresses)
              (sandbox-egress-refuse client 403 reason)
              (let ((upstream (handler-case (sandbox-egress-dial addresses port)
                                (error (condition)
                                  (sandbox-egress-refuse client 502 (format nil "~a:~d: ~a" host port condition))
                                  nil))))
                (when upstream
                  (sandbox-send-text client (format nil "HTTP/1.1 200 Connection established~c~c~c~c"
                                                    #\Return #\Newline #\Return #\Newline))
                  (sandbox-egress-carry client upstream rest))))))))

(defparameter +sandbox-hop-headers+
  '("connection" "keep-alive" "proxy-connection" "proxy-authorization" "proxy-authenticate"
    "te" "trailer" "transfer-encoding" "upgrade")
  "The headers that are one hop's, never passed on.")

(defun sandbox-egress-forward (client method target headers rest egress)
  "An absolute-form http:// request: passed on in origin form when the
verdict allows, with the connection closed after it; else refused."
  (let* ((uri (ignore-errors (quri:uri target)))
         (host (and uri (quri:uri-host uri)))
         (port (and uri (or (quri:uri-port uri) 80))))
    (if (or (null host) (zerop (length host)))
        (sandbox-egress-refuse client 400 (format nil "~a names no host" target))
        (multiple-value-bind (addresses reason) (sandbox-egress-verdict host port egress)
          (sandbox-egress-log egress "~:[refuse~;allow~] ~a http://~a:~d~@[ (~a)~]" addresses method host port reason)
          (if (null addresses)
              (sandbox-egress-refuse client 403 reason)
              (let ((upstream (handler-case (sandbox-egress-dial addresses port)
                                (error (condition)
                                  (sandbox-egress-refuse client 502 (format nil "~a:~d: ~a" host port condition))
                                  nil))))
                (when upstream
                  (let ((path (format nil "~a~@[?~a~]" (let ((p (quri:uri-path uri))) (if (plusp (length p)) p "/"))
                                      (quri:uri-query uri))))
                    (sandbox-send-octets
                     (usocket:socket upstream)
                     (sb-ext:string-to-octets
                      (with-output-to-string (out)
                        (format out "~a ~a HTTP/1.1~c~c" method path #\Return #\Newline)
                        (loop for (name . value) in headers
                              unless (member name +sandbox-hop-headers+ :test #'string-equal)
                                do (format out "~a: ~a~c~c" name value #\Return #\Newline))
                        (format out "Connection: close~c~c~c~c" #\Return #\Newline #\Return #\Newline))
                      :external-format :latin-1))
                    (sandbox-egress-carry client upstream rest)))))))))

(defun sandbox-broker-target (target)
  "TARGET, a broker path, as (values URL ORIGIN HOST PORT), or NIL."
  (let* ((after (subseq target (length nlk:+jail-broker-path+)))
         (slash (position #\/ after))
         (scheme (and slash (string-downcase (subseq after 0 slash))))
         (rest (and slash (subseq after (1+ slash))))
         (end (and rest (or (position-if (lambda (c) (member c '(#\/ #\?))) rest) (length rest))))
         (authority (and end (subseq rest 0 end))))
    (when (member scheme '("http" "https") :test #'string=)
      (multiple-value-bind (host port) (sandbox-host-port authority nil)
        (when host
          (let ((url (format nil "~a://~:[~a~;[~a]~]:~d~a" scheme (find #\: host) host port (subseq rest end))))
            (values url (nlk:url-origin url) host port)))))))

(defun sandbox-broker-body (client headers rest)
  "The request body: REST and what more CLIENT sends, Content-Length's worth;
:CHUNKED when the body is chunked, which the broker does not take."
  (cond ((sandbox-header headers "transfer-encoding") :chunked)
        (t (let ((length (or (ignore-errors (parse-integer (or (sandbox-header headers "content-length") "0"))) 0))
                 (body (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
                 (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
             (loop for byte across rest while (< (length body) length) do (vector-push-extend byte body))
             (loop while (< (length body) length)
                   do (let ((got (nth-value 1 (sb-bsd-sockets:socket-receive client buffer nil))))
                        (when (or (null got) (zerop got)) (error "the jail closed before its request's body"))
                        (loop for i below (min got (- length (length body))) do (vector-push-extend (aref buffer i) body))))
             (coerce body '(simple-array (unsigned-byte 8) (*)))))))

(defun sandbox-broker-copy (body client)
  "Copy BODY, the provider's answer as an octet stream, to CLIENT, each line
as soon as it ends (an event stream's tokens wait for nothing)."
  (let ((buffer (make-array 16384 :element-type '(unsigned-byte 8))) (fill 0))
    (loop for byte = (read-byte body nil nil)
          while byte
          do (setf (aref buffer fill) byte)
             (incf fill)
             (when (or (= byte 10) (= fill (length buffer)))
               (sandbox-send-octets client buffer :end fill)
               (setf fill 0)))
    (when (plusp fill) (sandbox-send-octets client buffer :end fill))))

(defun sandbox-egress-key (provider url)
  "PROVIDER's key toward URL as this process resolves it, outside the jail --
config.jsonc, secrets/, auth.json, the environment -- or NIL. A provider whose
lane only a cell knows (the launcher loads no cell) still finds the key the
tiers no lane decides keep for it: secrets/, auth.json, <ID>_API_KEY."
  (let ((key (ignore-errors (credential-key (resolve-provider-credential provider :endpoint url)))))
    (if (and key (string/= key "public"))
        key
        (or (ignore-errors (provider-secret-key provider))
            (ignore-errors (auth-api-key (read-auth-file *auth-file-path*) provider))
            (provider-env-key provider)))))

(defun sandbox-egress-broker (client method target headers rest egress)
  "A provider call from the jail: refused unless its address is a
configured provider's and each stand-in it carries is a provider's served
there; else made outside, each stand-in swapped for the key, and the answer
carried back as it streams, the connection closed after it."
  (multiple-value-bind (url origin) (sandbox-broker-target target)
    (let* ((carried (sandbox-key-stand-in-providers
                     (format nil "~a~{ ~a~}" target (mapcar #'cdr headers))))
           ;; the providers served there: the plan's, and each one the call
           ;; carries a stand-in of read afresh -- a catalog that loaded after
           ;; the launch names an address the plan could not
           (served (and origin
                        (mapcar (lambda (provider) (getf provider :id))
                                (append (sandbox-egress-providers-at egress :origin origin)
                                        (sandbox-egress-providers-at
                                         (list :providers (ignore-errors (sandbox-provider-table carried)))
                                         :origin origin)))))
           (stray (remove-if (lambda (id) (member id served :test #'string=)) carried))
           ;; the address as the jail wrote it: a stand-in, never a key
           (shown (and url (format nil "~a ~a" method url))))
      (flet ((refuse (status reason)
               (sandbox-egress-log egress "refuse broker ~a (~a)" (or shown target) reason)
               (return-from sandbox-egress-broker (sandbox-egress-refuse client status reason))))
        (cond ((null url) (refuse 400 (format nil "~a is no provider address" target)))
              ((null served) (refuse 403 (format nil "~a is no configured provider's address" origin)))
              (stray (refuse 403 (format nil "the key of ~{~a~^, ~} goes only to its own provider's address, never ~a"
                                         stray origin))))
        (let* ((body (sandbox-broker-body client headers rest))
               (keys (loop for id in carried
                           for key = (ignore-errors (funcall (getf egress :credential 'sandbox-egress-key) id url))
                           unless (and key (string/= key "public"))
                             do (refuse 502 (format nil "no key for ~a outside the jail" id))
                           collect (cons id key)))
               (sent (append (loop for (name . value) in headers
                                   unless (or (member name +sandbox-hop-headers+ :test #'string-equal)
                                              (member name '("host" "content-length" "accept-encoding") :test #'string-equal))
                                     collect (cons name (sandbox-swap-stand-ins value keys)))
                             '(("Accept-Encoding" . "identity"))))
               (upstream-url (sandbox-swap-stand-ins url keys)))
          (when (eq body :chunked) (refuse 411 "a chunked body is not carried; send its length"))
          (sandbox-egress-log egress "allow broker ~a~@[ key ~{~a~^, ~}~]" shown carried)
          (sandbox-broker-answer client method upstream-url sent body))))))

(defun sandbox-broker-answer (client method url headers body)
  "Make the call METHOD URL with HEADERS and BODY outside the jail and carry
its answer to CLIENT: the status, the headers but the hop's own and the
length, then the body as it streams, the connection closed after it. CLIENT
closing first cuts the call."
  (let ((stream nil))
    (multiple-value-bind (answer status response-headers)
        (handler-case
            (multiple-value-bind (answer status response-headers)
                (dex:request url :method (intern (string-upcase method) :keyword)
                                 :headers headers :content (and (plusp (length body)) body)
                                 :want-stream t :force-binary t :max-redirects 0
                                 :use-connection-pool nil :keep-alive nil
                                 :connect-timeout 30 :read-timeout 600
                                 :proxy (nlk:proxy-for url))
              (values answer status response-headers))
          (dex:http-request-failed (condition)
            (values (dex:response-body condition) (dex:response-status condition)
                    (dex:response-headers condition)))
          (error (condition)
            (return-from sandbox-broker-answer
              (sandbox-egress-refuse client 502 (format nil "~a" condition)))))
      (setf stream (and (streamp answer) answer))
      (let ((watch (spawn "sandbox-broker-watch"
                     ;; the jail sends nothing more; its end closing cuts the call
                     (let ((octets (make-array 1024 :element-type '(unsigned-byte 8))))
                       (ignore-errors
                        (loop (let ((got (nth-value 1 (sb-bsd-sockets:socket-receive client octets nil))))
                                (when (or (null got) (zerop got)) (return)))))
                       (when stream (ignore-errors (close stream :abort t)))))))
        (declare (ignore watch))
        (unwind-protect
             (progn
               (sandbox-send-text
                client
                (with-output-to-string (out)
                  (format out "HTTP/1.1 ~d ~a~c~c" status (if (< status 400) "OK" "Error") #\Return #\Newline)
                  (when (hash-table-p response-headers)
                    (maphash (lambda (name value)
                               (unless (or (member name +sandbox-hop-headers+ :test #'string-equal)
                                           (member name '("content-length" "content-encoding") :test #'string-equal))
                                 (format out "~a: ~a~c~c" name value #\Return #\Newline)))
                             response-headers))
                  (format out "Connection: close~c~c~c~c" #\Return #\Newline #\Return #\Newline)))
               (cond (stream (sandbox-broker-copy stream client))
                     ((stringp answer) (sandbox-send-octets client (sb-ext:string-to-octets answer :external-format :utf-8)))
                     ((typep answer '(vector (unsigned-byte 8))) (sandbox-send-octets client answer))))
          (when stream (ignore-errors (close stream :abort t))))))))

;;; --- the listener ---------------------------------------------------------------

(defun sandbox-egress-serve (client egress)
  "Answer one connection from the jail, CLIENT, then close it."
  (unwind-protect
       (handler-case
           (multiple-value-bind (head rest)
               (sb-sys:with-deadline (:seconds 60) (sandbox-read-head client))
             (let ((request (and head (sandbox-parse-head head))))
               (cond ((null head))
                     ((null request) (sandbox-egress-refuse client 400 "no request line"))
                     (t (destructuring-bind (method sent headers) request
                          ;; a jail on the host's loopback is answered by its token alone
                          (let ((target (sandbox-egress-admitted sent headers egress)))
                            (cond ((null target)
                                   (sandbox-egress-log egress "refuse ~a: no token" method)
                                   (sandbox-egress-refuse client 407 "this proxy answers its own jail alone"))
                                  ((string-equal method "CONNECT")
                                   (sandbox-egress-tunnel client target rest egress))
                                  ((uiop:string-prefix-p nlk:+jail-broker-path+ target)
                                   (sandbox-egress-broker client method target headers rest egress))
                                  ((uiop:string-prefix-p "http://" (string-downcase target))
                                   (sandbox-egress-forward client method target headers rest egress))
                                  (t (sandbox-egress-refuse client 400 (format nil "~a ~a is no proxy request" method target))))))))))
         (error (condition)
           (sandbox-egress-log egress "error ~a" condition)))
    ;; shut down first: a thread still reading it (the broker's watch) holds
    ;; the descriptor, and a close alone would leave the jail's end open
    (ignore-errors (sb-bsd-sockets:socket-shutdown client :direction :io))
    (ignore-errors (sb-bsd-sockets:socket-close client))))

(defun sandbox-egress-start (&key listener egress)
  "Answer the jail's way out on LISTENER, a listening socket on the jail's
own loopback, under EGRESS (SANDBOX-EGRESS-VERDICT's plist, and :LOG), each
connection on a thread of its own, until the listener fails for good."
  (sandbox-egress-log egress "start: ~:[only the hosts allowed~;the public internet~], ~d provider~:p"
                      (getf egress :internet) (length (getf egress :providers)))
  (spawn "sandbox-egress"
    (unwind-protect
         (loop with failures = 0
               while (< failures 100)
               do (let ((client (handler-case (sb-bsd-sockets:socket-accept listener)
                                  (sb-bsd-sockets:interrupted-error () nil)
                                  (error () (incf failures) (sleep 0.05) nil))))
                    (when client
                      (setf failures 0)
                      (spawn "sandbox-egress-connection" (sandbox-egress-serve client egress)))))
      (ignore-errors (sb-bsd-sockets:socket-close listener)))))
