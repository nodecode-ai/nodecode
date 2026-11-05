;;;; dial.lisp --- how the organism reaches a peer by name: every address the
;;;; name resolves to, IPv6 first, raced with a stagger; the first to connect
;;;; wins.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; usocket connects to the first address a name resolves to, and that is
;;;; always an IPv4 one (GET-HOSTS-BY-NAME answers the A records, then the
;;;; AAAA). Every outbound client in the image dials through
;;;; USOCKET:SOCKET-CONNECT with a hostname -- dexador for the provider POST,
;;;; the listing GET, the update poller, websearch, MCP over HTTP;
;;;; websocket-driver for a channel's gateway socket -- so the whole organism was
;;;; IPv4-only on a dual-stack host, while curl and every browser prefer IPv6
;;;; and fall back (RFC 8305, happy eyeballs).
;;;;
;;;; Without this, on a 464XLAT carrier (NAT64 prefix 64:ff9b::/96): the
;;;; IPv4 side would drop every packet over 1280 bytes with no ICMP back, so
;;;; a request body longer than one segment would die at the server as a TLS
;;;; `bad record mac' alert -- the "tls failure" every turn would show --
;;;; while the same 400 KB body over IPv6 would go through every time,
;;;; seconds apart. On wifi both families would be fine.
;;;;
;;;; The policy is the race the RFC describes, at the one seam every client
;;;; already passes through: USOCKET:SOCKET-CONNECT is encapsulated (the
;;;; idiom hook.lisp and store.lisp use) so a hostname with two or more
;;;; addresses is dialed as a race -- IPv6 first, the next address started
;;;; *DIAL-STAGGER-SECONDS* later unless one has answered, the first socket
;;;; wins and a later one is closed -- and everything else (a literal
;;;; address, a unix path, a name with one address) passes straight through.

(in-package #:nodecode.kernel)

(defparameter *dial-stagger-seconds* 0.25
  "How long the race gives one connection attempt before the next address
is started beside it: RFC 8305's connection attempt delay.")

(defun encapsulate-afresh (name type advice)
  "ADVICE around NAME as its TYPE encapsulation, in place of one a reload left."
  (when (sb-int:encapsulated-p name type)
    (sb-int:unencapsulate name type))
  (sb-int:encapsulate name type advice))

(defun dial-order (addresses)
  "ADDRESSES (octet vectors, 16 long for IPv6, 4 for IPv4) interleaved by
family, IPv6 first: v6, v4, v6, v4, ..."
  (let ((six (remove-if-not (lambda (address) (= (length address) 16)) addresses))
        (four (remove-if (lambda (address) (= (length address) 16)) addresses)))
    (loop while (or six four)
          when six collect (pop six)
          when four collect (pop four))))

(defstruct (dial-race (:constructor make-dial-race (count)))
  "One race: the winner, the failures so far, and the semaphore each
attempt signals when it is over."
  (winner nil)
  (failures '())
  (count 0)
  (done (bt2:make-semaphore :name "dial")))

(access (race dial-race))

(defun start-dial-attempt (race connect index address)
  "Run (CONNECT ADDRESS) on its own thread: a socket takes the race if no
one has, or is closed; a failure is recorded under INDEX; either way the
race's semaphore is signalled once."
  (spawn "dial"
    (with-cleanup ((bt2:signal-semaphore (dial-race-done race)))
      (handler-case
          (let ((socket (funcall connect address)))
            (unless (null (sb-ext:compare-and-swap race.winner nil socket))
              (ignore-errors (usocket:socket-close socket))))
        (error (condition)
          (sb-ext:atomic-push (cons index condition) race.failures))))))

(defun abandon-dial (race &aux (landed (sb-ext:compare-and-swap race.winner nil :abandoned)))
  "Mark RACE as left by its caller, closing a socket that landed meanwhile."
  (when (typep landed 'usocket:usocket)
    (ignore-errors (usocket:socket-close landed))))

(defun dial (connect addresses &aux (pending (coerce addresses 'list))
                                    (race (make-dial-race (length pending)))
                                    (won nil))
  "The first socket (CONNECT ADDRESS) answers for one of ADDRESSES, tried in
order: each attempt runs on its own thread, the next starts
*DIAL-STAGGER-SECONDS* after the last unless one has already answered, a
socket that arrives after the winner is closed, and when every attempt has
failed the failure of the last address is what the caller sees."
  ;; A caller that leaves early -- a deadline, an interrupt -- abandons the
  ;; race, and an attempt that lands after that closes what it opened.
  (with-cleanup ((unless won (abandon-dial race)))
    (loop
      (when pending
        (start-dial-attempt race connect (- race.count (length pending)) (pop pending)))
      (bt2:wait-on-semaphore race.done
                             :timeout (and pending *dial-stagger-seconds*))
      (let ((winner race.winner))
        (when (typep winner 'usocket:usocket)
          (setf won t)
          (return winner)))
      (when (and (null pending)
                 (= (length race.failures) race.count))
        (error (cdr (first (sort (copy-list race.failures) #'> :key #'car))))))))

(defun dial-through (next host port &rest args)
  "USOCKET:SOCKET-CONNECT's advice: a hostname that resolves to two or more
addresses is raced through DIAL; everything else goes straight to NEXT."
  ;; A literal address and a name with one address resolve to a single
  ;; vector and pass through; a unix path is not a string. The addresses are
  ;; handed to NEXT as vectors, so no attempt resolves the name again.
  (let ((addresses (and (stringp host) (usocket:get-hosts-by-name host))))
    (if (< (length addresses) 2)
        (apply next host port args)
        (dial (lambda (address) (apply next address port args))
              (dial-order addresses)))))

(encapsulate-afresh 'usocket:socket-connect 'dial #'dial-through)

;;; --- a refused dial answers at once -------------------------------------------
;;; usocket's connect with a timeout -- every client here passes one -- starts
;;; the connect non-blocking and polls SB-BSD-SOCKETS:SOCKET-PEERNAME until the
;;; socket has a peer or the timeout passes. A connect the peer refused leaves
;;; getpeername answering ENOTCONN, which is also what a connect still in flight
;;; answers, and the refusal waits unread in SO_ERROR. Without this, a port
;;; nothing listens on would wait for the whole timeout, where curl says
;;; `connection refused' at once.

(defun peername-or-connect-failure (next socket)
  "SB-BSD-SOCKETS:SOCKET-PEERNAME's advice: a socket whose connect failed
signals that failure, read from SO_ERROR (ECONNREFUSED, EHOSTUNREACH, ...),
in place of the `not connected' getpeername answers for it; a socket with
nothing pending fails as NEXT does."
  (handler-bind ((sb-bsd-sockets:socket-error
                   (lambda-ignoring (condition)
                     (let ((errno (ignore-errors (sb-bsd-sockets:sockopt-error socket))))
                       (when (and errno (plusp errno))
                         (sb-bsd-sockets:socket-error "connect" errno))))))
    (funcall next socket)))

(encapsulate-afresh 'sb-bsd-sockets:socket-peername 'connect-failure #'peername-or-connect-failure)

;;; --- a handshake that fails closes its socket ---------------------------------
;;; dexador connects the socket and then shakes hands over it (MAKE-SSL-STREAM)
;;; before its request's own cleanup holds either. A handshake that signals --
;;; a reset, an SSL error, a caller's deadline -- left the connected socket
;;; open with nothing referring to it, a descriptor until exit. Every retry on
;;; a flaky link leaked one; a process whose descriptors are spent resolves no
;;; name, so it hears of no release and reaches no provider (a launchd agent
;;; on macOS has 256).

(defun close-unless-secured (next stream &rest args)
  "DEXADOR's MAKE-SSL-STREAM advice: STREAM, the connected socket's, is closed
when the handshake does not answer -- an error, a deadline, an interrupt."
  (let ((secured nil))
    (unwind-protect (multiple-value-prog1 (apply next stream args) (setf secured t))
      (unless secured (ignore-errors (close stream :abort t))))))

(encapsulate-afresh 'dexador.backend.usocket::make-ssl-stream 'close-unless-secured #'close-unless-secured)

;;; --- one client TLS context ---------------------------------------------------
;;; dexador makes a context for every HTTPS connection it opens, and a
;;; context's :DEFAULT verify location is the system's whole CA bundle parsed
;;; into a store of its own -- some 140 certificates of small allocations,
;;; parsed again for every request and held for as long as the connection
;;; lives, a pooled one included. OpenSSL shares
;;; one context between connections and threads, so each verify mode gets one,
;;; and a caller's free drops the reference handed to it, never the store.

(defvar *client-contexts* '()
  "(VERIFY-MODE . SSL_CTX) the shared client contexts, one per verify mode.")

(defvar *client-contexts-lock* (bt2:make-lock :name "tls client contexts"))

;;; --- Windows: the system's chain engine judges a peer -----------------------
;;; OpenSSL on Windows has no store to verify a peer against (cl-plus-ssl#54,
;;; https://github.com/cl-plus-ssl/cl-plus-ssl/issues/54): its default
;;; location is a directory nothing installs. A context that verifies at that
;;; default location hands the chain check to Windows instead: the chain the
;;; handshake presented is built by CertGetCertificateChain and judged by the
;;; SSL policy of CertVerifyCertificateChainPolicy -- the trust every other
;;; program on the machine is held to, its enterprise roots included, and the
;;; roots Windows fetches the first time a chain needs one (automatic root
;;; update), which a copy of the system store would not hold yet. cl+ssl
;;; still checks the host's name against the certificate after the handshake.

#+win32
(progn
  (defun windows-entry (name)
    "The address of crypt32's entry NAME, as a pointer to call."
    (sb-sys:int-sap
     (or (sb-sys:find-dynamic-foreign-symbol-address name)
         (progn (sb-alien:load-shared-object "crypt32.dll" :dont-save t)
                (sb-sys:find-dynamic-foreign-symbol-address name))
         (error "crypt32.dll has no ~a" name))))

  (defmacro windows-call (name &rest arguments-and-return)
    `(cffi:foreign-funcall-pointer (windows-entry ,name) (:convention :stdcall) ,@arguments-and-return))

  (defun zero-foreign (pointer size)
    (dotimes (i size pointer) (setf (cffi:mem-aref pointer :uint8 i) 0)))

  (defun x509-octets (x509)
    "X509's DER encoding, as octets."
    (let ((length (cffi:foreign-funcall "i2d_X509" :pointer x509 :pointer (cffi:null-pointer) :int)))
      (unless (plusp length) (error "a presented certificate does not encode"))
      (cffi:with-foreign-objects ((buffer :uint8 length) (cursor :pointer))
        (setf (cffi:mem-ref cursor :pointer) buffer)
        (cffi:foreign-funcall "i2d_X509" :pointer x509 :pointer cursor :int)
        (let ((octets (make-array length :element-type '(unsigned-byte 8))))
          (dotimes (i length octets) (setf (aref octets i) (cffi:mem-aref buffer :uint8 i)))))))

  (defun presented-chain (store-context)
    "What the handshake presented, as DER octets: the peer's certificate first."
    (let ((leaf (cffi:foreign-funcall "X509_STORE_CTX_get0_cert" :pointer store-context :pointer))
          (others (cffi:foreign-funcall "X509_STORE_CTX_get0_untrusted" :pointer store-context :pointer)))
      (cons (x509-octets leaf)
            (unless (cffi:null-pointer-p others)
              (loop for i below (cffi:foreign-funcall "OPENSSL_sk_num" :pointer others :int)
                    for x509 = (cffi:foreign-funcall "OPENSSL_sk_value" :pointer others :int i :pointer)
                    unless (cffi:pointer-eq x509 leaf) collect (x509-octets x509))))))

  (defun presented-server-name (store-context)
    "The name the handshake asked the server for (SNI), or NIL."
    (let ((ssl (cffi:foreign-funcall "X509_STORE_CTX_get_ex_data" :pointer store-context
                                     :int (cffi:foreign-funcall "SSL_get_ex_data_X509_STORE_CTX_idx" :int)
                                     :pointer)))
      (unless (cffi:null-pointer-p ssl)
        (let ((name (cffi:foreign-funcall "SSL_get_servername" :pointer ssl :int 0 :pointer)))
          (unless (cffi:null-pointer-p name) (cffi:foreign-string-to-lisp name))))))

  (defun chain-policy-verdict (leaf store server-name)
    "Windows' verdict on the chain it builds from LEAF, with STORE's certificates
to build through, for a TLS server named SERVER-NAME (any name when NIL): 0
when it is trusted for server authentication, else the CERT_E_ code."
    (cffi:with-foreign-objects ((parameters :uint8 32) (usages :pointer) (chain :pointer)
                                (extra :uint8 24) (policy :uint8 16) (status :uint8 24))
      (cffi:with-foreign-string (server-auth "1.3.6.1.5.5.7.3.1")
        ;; CERT_CHAIN_PARA: server authentication is the usage asked for.
        (zero-foreign parameters 32)
        (setf (cffi:mem-ref parameters :uint32 0) 32
              (cffi:mem-ref parameters :uint32 16) 1
              (cffi:mem-ref parameters :pointer 24) usages
              (cffi:mem-ref usages :pointer) server-auth)
        (when (zerop (windows-call "CertGetCertificateChain" :pointer (cffi:null-pointer) :pointer leaf
                                   :pointer (cffi:null-pointer) :pointer store :pointer parameters
                                   :uint32 0 :pointer (cffi:null-pointer) :pointer chain :int32))
          (return-from chain-policy-verdict (logior #x80000000 (sb-win32:get-last-error))))
        (unwind-protect
             (flet ((judge (name)
                      ;; SSL_EXTRA_CERT_CHAIN_POLICY_PARA, CERT_CHAIN_POLICY_PARA, CERT_CHAIN_POLICY_STATUS
                      (zero-foreign extra 24) (zero-foreign policy 16) (zero-foreign status 24)
                      (setf (cffi:mem-ref extra :uint32 0) 24
                            (cffi:mem-ref extra :uint32 4) 2 ; AUTHTYPE_SERVER
                            (cffi:mem-ref extra :pointer 16) name
                            (cffi:mem-ref policy :uint32 0) 16
                            (cffi:mem-ref policy :pointer 8) extra
                            (cffi:mem-ref status :uint32 0) 24)
                      (if (zerop (windows-call "CertVerifyCertificateChainPolicy" :pointer (cffi:make-pointer 4) ; CERT_CHAIN_POLICY_SSL
                                               :pointer (cffi:mem-ref chain :pointer) :pointer policy :pointer status :int32))
                          (logior #x80000000 (sb-win32:get-last-error))
                          (cffi:mem-ref status :uint32 4))))
               (if server-name
                   (cffi:with-foreign-string (name server-name :encoding :utf-16le) (judge name))
                   (judge (cffi:null-pointer))))
          (windows-call "CertFreeCertificateChain" :pointer (cffi:mem-ref chain :pointer) :void)))))

  (defun windows-chain-verdict (chain server-name)
    "Windows' verdict on CHAIN, DER octets with the peer's certificate first,
for a TLS server named SERVER-NAME: 0 when trusted, else the CERT_E_ code."
    (let ((store (windows-call "CertOpenStore" :pointer (cffi:make-pointer 2) ; CERT_STORE_PROV_MEMORY
                               :uint32 0 :pointer (cffi:null-pointer) :uint32 0 :pointer (cffi:null-pointer) :pointer)))
      (when (cffi:null-pointer-p store) (error "CertOpenStore refused a memory store"))
      (unwind-protect
           (let ((leaf (cffi:null-pointer)))
             (cffi:with-foreign-object (added :pointer)
               (loop for octets in chain
                     for first = t then nil
                     do (setf (cffi:mem-ref added :pointer) (cffi:null-pointer))
                        (cffi:with-pointer-to-vector-data (data octets)
                          ;; X509_ASN_ENCODING | PKCS_7_ASN_ENCODING, CERT_STORE_ADD_ALWAYS
                          (windows-call "CertAddEncodedCertificateToStore" :pointer store :uint32 #x10001
                                        :pointer data :uint32 (length octets) :uint32 4
                                        :pointer (if first added (cffi:null-pointer)) :int32))
                        (when first (setf leaf (cffi:mem-ref added :pointer)))))
             (when (cffi:null-pointer-p leaf) (error "the peer's certificate does not parse"))
             (unwind-protect (chain-policy-verdict leaf store server-name)
               (windows-call "CertFreeCertificateContext" :pointer leaf :int32)))
        (windows-call "CertCloseStore" :pointer store :uint32 0 :int32))))

  (defun x509-error-for (verdict)
    "The X509_V_ERR_ code OpenSSL reports for Windows' VERDICT."
    (case verdict
      (0 0)
      (#x800B0101 10)               ; CERT_E_EXPIRED: X509_V_ERR_CERT_HAS_EXPIRED
      ((#x800B0109 #x800B010A) 20)  ; untrusted root, broken chain: unable to get local issuer
      (#x800B010F 62)               ; CERT_E_CN_NO_MATCH: X509_V_ERR_HOSTNAME_MISMATCH
      (#x80092010 23)               ; CRYPT_E_REVOKED: X509_V_ERR_CERT_REVOKED
      (#x800B0110 26)               ; CERT_E_WRONG_USAGE: X509_V_ERR_INVALID_PURPOSE
      (t 27)))                      ; X509_V_ERR_CERT_UNTRUSTED

  (cffi:defcallback verify-through-windows :int ((store-context :pointer) (argument :pointer))
    (declare (ignore argument))
    (let ((verdict (handler-case (windows-chain-verdict (presented-chain store-context)
                                                        (presented-server-name store-context))
                     (error () #x800B0109))))
      (cffi:foreign-funcall "X509_STORE_CTX_set_error" :pointer store-context :int (x509-error-for verdict) :void)
      (if (zerop verdict) 1 0))))

(defun platform-verified (context verify-mode)
  "CONTEXT, which verifies its peers at the default location, made to verify
them through the platform's own trust where OpenSSL has none (Windows)."
  #+win32
  (when (logtest (or verify-mode cl+ssl:+ssl-verify-peer+) cl+ssl:+ssl-verify-peer+)
    (cffi:foreign-funcall "SSL_CTX_set_cert_verify_callback" :pointer context
                          :pointer (cffi:callback verify-through-windows) :pointer (cffi:null-pointer) :void))
  #-win32 (declare (ignore verify-mode))
  context)

(defun shared-client-context (next &rest args &key (verify-location :default) verify-mode &allow-other-keys)
  "CL+SSL:MAKE-CONTEXT's advice: a context asked for by verify mode and the
system's default location alone is that mode's shared one, with a reference
of its own for the caller to free; any other ask is NEXT's, a fresh context.
Every context at the default location verifies through the platform's trust."
  ;; A named location stays fresh: the file a caller names may be rewritten
  ;; between asks, and a cached store would keep the one before.
  (cond ((not (eq verify-location :default)) (apply next args))
        ((loop for key in args by #'cddr
               thereis (not (member key '(:verify-mode :verify-location))))
         (platform-verified (apply next args) verify-mode))
        (t (bt2:with-lock-held (*client-contexts-lock*)
             (let ((context (or (cdr (assoc verify-mode *client-contexts*))
                                (cdar (push (cons verify-mode (platform-verified (apply next args) verify-mode))
                                            *client-contexts*)))))
               (cffi:foreign-funcall "SSL_CTX_up_ref" :pointer context :int)
               context)))))

(defun forget-client-contexts ()
  "Drop the shared contexts before a dump: a foreign pointer outlives nothing."
  (setf *client-contexts* '()))

(encapsulate-afresh 'cl+ssl:make-context 'shared #'shared-client-context)
(pushnew 'forget-client-contexts sb-ext:*save-hooks*)

;;; --- one HTTP exchange ------------------------------------------------------
;;; Every client that reads a status as a fact -- a 4xx/5xx the caller words,
;;; never a signal -- goes through HTTP; only a transport failure (DNS, TLS, a
;;; timeout) signals. BODY-TEXT is the one way a body of any shape becomes text.

(defun decode-octets (octets &optional content-type)
  "OCTETS as text: utf-8 with replacement, or latin-1 when CONTENT-TYPE says
so. Nothing here signals on a bad byte."
  (sb-ext:octets-to-string
   (coerce octets '(simple-array (unsigned-byte 8) (*)))
   :external-format (if (and content-type
                             (cl-ppcre:scan "(?i)charset=\"?(iso-8859-1|latin-?1|windows-1252)"
                                            content-type))
                        :latin-1
                        '(:utf-8 :replacement #\?))))

(defun body-text (body &optional (limit 65536))
  "BODY -- a string, octets, a character or binary stream, or NIL -- as text,
a stream read for at most LIMIT elements and kept as far as it arrived when
the connection dies mid-read: this feeds failure copy, so a prefix always
beats nothing."
  (typecase body
    (null "")
    (string body)
    (vector (decode-octets body))
    (stream
     (let* ((binary (ignore-errors (subtypep (stream-element-type body) 'integer)))
            (type (if binary '(unsigned-byte 8) 'character))
            (chunk (make-array (min limit 65536) :element-type type))
            (whole (make-array 0 :element-type type :adjustable t :fill-pointer 0)))
       (with-handlers ((error () nil))
         (loop for end = (read-sequence chunk body :end (min (length chunk)
                                                            (- limit (length whole))))
               while (plusp end)
               do (loop for i below end do (vector-push-extend (aref chunk i) whole))))
       (if binary (decode-octets whole) (coerce whole 'string))))
    (t (princ-to-string body))))

(defun read-capped (stream max-bytes deadline)
  "Read binary STREAM into an octet vector of at most MAX-BYTES. => (values
OCTETS TRUNCATED-P); past DEADLINE, an internal-real-time, it signals."
  (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8)))
        (octets (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop
      (let ((end (read-sequence buffer stream)))
        (when (zerop end) (return (values octets nil)))
        (let* ((start (fill-pointer octets))
               (take (min end (- max-bytes start))))
          (adjust-array octets (+ start take) :fill-pointer (+ start take))
          (replace octets buffer :start1 start :end2 take)
          (when (> end take)
            (return (values octets t))))
        (when (> (get-internal-real-time) deadline)
          (error "fetch exceeded its time limit"))))))

(define-condition http-timeout (sb-sys:deadline-timeout error)
  ((host :initarg :host :reader http-timeout-host)
   (limit :initarg :limit :reader http-timeout-limit))
  (:report (lambda (condition stream)
             (format stream "~a did not answer within ~a second~:p"
                     (http-timeout-host condition) (http-timeout-limit condition))))
  (:documentation "An HTTP exchange past its TIMEOUT: a deadline's timeout
that is an error too, so a caller's handler for either takes it."))

;;; --- the proxy a dial goes through ------------------------------------------
;;; dexador reads HTTPS_PROXY once, when it loads, into DEX:*DEFAULT-PROXY*:
;;; in a dumped binary that is the build machine's environment, never the
;;; launch's. A process run behind a proxy -- a sandbox that leaves it no
;;; network of its own but one proxy -- names it in its environment, so every
;;; dial asks PROXY-FOR, which reads the environment at the call.

(defun proxy-environment-value (&rest names)
  "The first of the environment variables NAMES that is set and not blank,
trimmed; NIL when none is."
  (loop for name in names
        for value = (string-trim '(#\Space #\Tab #\Newline #\Return) (or (uiop:getenv name) ""))
        when (plusp (length value)) return value))

(defun ipv4-octets (text)
  "TEXT's four octets as a list when it is a dotted IPv4 literal, else NIL."
  (let ((parts (uiop:split-string text :separator ".")))
    (when (= 4 (length parts))
      (loop for part in parts
            for octet = (and (plusp (length part)) (<= (length part) 3)
                             (every #'digit-char-p part) (parse-integer part))
            unless (and octet (<= octet 255)) return nil
            collect octet))))

(defun ipv4-integer (octets)
  "OCTETS, four of them, as one 32-bit integer."
  (reduce (lambda (sum octet) (+ (* sum 256) octet)) octets :initial-value 0))

(defun loopback-host-p (host)
  "Whether HOST names this machine's loopback: no host at all, localhost or a
name under it, 127.0.0.0/8, or ::1."
  (let ((host (string-downcase (string-trim "[]" (or host "")))))
    (or (zerop (length host))
        (string= host "localhost")
        (uiop:string-suffix-p host ".localhost")
        (member host '("::1" "0:0:0:0:0:0:0:1") :test #'string=)
        (eql 127 (first (ipv4-octets host))))))

(defun no-proxy-entry-covers-p (entry host port)
  "Whether ENTRY, one item of a NO_PROXY list, covers HOST (lower case, no
brackets) at PORT: `*'; an IPv4 literal or CIDR; a bracketed or bare IPv6
literal; or a name, which covers itself and every name under it, a leading
dot or `*.' allowed. A `:PORT' after an IPv4 or a name, or after a
bracketed IPv6 literal, limits the entry to that port."
  (let* ((entry (string-downcase entry))
         (colons (count #\: entry))
         (bracket-end (and (plusp (length entry)) (char= #\[ (char entry 0)) (position #\] entry)))
         (split (cond (bracket-end (and (< (1+ bracket-end) (length entry))
                                        (char= #\: (char entry (1+ bracket-end)))
                                        (1+ bracket-end)))
                      ((= colons 1) (position #\: entry))))
         (name (string-trim "[]" (if split (subseq entry 0 split) entry)))
         (only-port (and split (parse-integer entry :start (1+ split) :junk-allowed t))))
    (when (or (null only-port) (eql only-port port))
      (let ((slash (position #\/ name)))
        (cond ((string= name "*") t)
              (slash
               (let ((network (ipv4-octets (subseq name 0 slash)))
                     (bits (parse-integer name :start (1+ slash) :junk-allowed t))
                     (address (ipv4-octets host)))
                 (when (and network address bits (<= 0 bits 32))
                   (let ((shift (- 32 bits)))
                     (= (ash (ipv4-integer network) (- shift))
                        (ash (ipv4-integer address) (- shift)))))))
              ((or (ipv4-octets name) (find #\: name)) (string= name host))
              (t
               (let ((name (string-left-trim "." (if (uiop:string-prefix-p "*." name)
                                                    (subseq name 2)
                                                    name))))
                 (and (plusp (length name))
                      (or (string= name host)
                          (uiop:string-suffix-p host (concatenate 'string "." name)))))))))))

(defun proxy-for (url &optional explicit)
  "The proxy a dial to URL goes through: EXPLICIT when given; else none for
a loopback host, or for a host the environment's NO_PROXY (no_proxy) list
covers; else HTTPS_PROXY (https_proxy) for https and wss, HTTP_PROXY
(http_proxy) for http and ws, then ALL_PROXY (all_proxy); NIL when none is
set. The environment is read at every call."
  (or explicit
      (let* ((uri (ignore-errors (quri:uri url)))
             (scheme (string-downcase (or (and uri (quri:uri-scheme uri)) "")))
             (host (string-downcase (string-trim "[]" (or (and uri (quri:uri-host uri)) ""))))
             (port (and uri (quri:uri-port uri)))
             (exempt (proxy-environment-value "NO_PROXY" "no_proxy")))
        (unless (or (loopback-host-p host)
                    (and exempt
                         (some (lambda (entry)
                                 (let ((entry (string-trim '(#\Space #\Tab) entry)))
                                   (and (plusp (length entry))
                                        (no-proxy-entry-covers-p entry host port))))
                               (uiop:split-string exempt :separator ","))))
          (if (member scheme '("https" "wss") :test #'string=)
              (proxy-environment-value "HTTPS_PROXY" "https_proxy" "ALL_PROXY" "all_proxy")
              (proxy-environment-value "HTTP_PROXY" "http_proxy" "ALL_PROXY" "all_proxy"))))))

(defun forget-build-proxy ()
  "Drop the proxy dexador read from the build machine's environment: every
dial in this image asks PROXY-FOR instead."
  (setf dex:*default-proxy* nil))

(forget-build-proxy)
(pushnew 'forget-build-proxy sb-ext:*init-hooks*)

(defun http (method url &key headers content (timeout 30) (connect-timeout (min 10 timeout))
                              max-bytes binary stream pool proxy &aux clipped)
  "One HTTP exchange that answers every status as a value. => (values BODY
STATUS HEADERS TRUNCATED-P). TIMEOUT seconds bound the whole exchange on a
wall clock -- the connect, the TLS handshake, the headers and, unless
STREAM, the body; past them it signals HTTP-TIMEOUT."
  ;; METHOD a keyword, HEADERS an alist, CONTENT what dexador sends (a string,
  ;; octets, a multipart alist). BODY is dexador's -- text, or octets with
  ;; BINARY -- or with STREAM the open response stream the caller reads and
  ;; closes. With MAX-BYTES (an integer, or a function of the response headers
  ;; answering one) the body is streamed under that cap and a TIMEOUT-second
  ;; wall clock: BODY is an octet vector, TRUNCATED-P true when the wire had
  ;; more. A failed status's streamed BODY is its text, the first 2 KiB of it.
  ;; POOL keeps the connection (localhost control and platform REST reuse
  ;; theirs), PROXY names one. Without POOL the exchange asks the peer to
  ;; close: dexador hands a kept-alive connection back to its caller to close
  ;; or pool, and before a refused status it hands it to nobody, so one kept
  ;; alive here would sit in CLOSE-WAIT, a descriptor per exchange, until the
  ;; process can open no file at all. The wall clock is SBCL's deadline, which
  ;; every fd wait feels: dexador's :read-timeout is SO_RCVTIMEO, which a TLS
  ;; stream on SBCL never feels, so without it a peer that stalls mid-handshake
  ;; or mid-headers held a hub read for minutes.
  ;; Under a deadline the exchange is given what is left of it (WAIT-SECONDS),
  ;; and running out of that spends it (STEP-RAN-OUT).
  (multiple-value-setq (timeout clipped) (wait-seconds timeout))
  (setf connect-timeout (min connect-timeout timeout))
  (handler-bind ((sb-sys:deadline-timeout
                   (lambda (condition)
                     (unless (typep condition 'http-timeout)
                       (step-ran-out clipped)
                       (error 'http-timeout :host (or (ignore-errors (quri:uri-host (quri:uri url))) url)
                                            :limit timeout)))))
    (sb-sys:with-deadline (:seconds timeout)
      (handler-case
          (bind (((body status response-headers _ socket)
                  (dex:request url :method method :headers headers :content content
                                   :connect-timeout connect-timeout :read-timeout timeout
                                   :max-redirects 5 :use-connection-pool pool :keep-alive (and pool t)
                                   :proxy (proxy-for url proxy)
                                   :want-stream (or stream (and max-bytes t))
                                   :force-binary (or binary (and max-bytes t)))))
            (if max-bytes
                (with-cleanup ((when (streamp body) (ignore-errors (close body :abort t)))
                               (when (streamp socket) (ignore-errors (close socket :abort t))))
                  (multiple-value-bind (octets truncated)
                      (read-capped body
                                   (if (functionp max-bytes)
                                       (funcall max-bytes response-headers)
                                       max-bytes)
                                   (deadline-after timeout))
                    (values octets status response-headers truncated)))
                (values body status response-headers nil)))
        (dex:http-request-failed (condition)
          (let ((body (dex:response-body condition)))
            (with-cleanup ((when (streamp body) (ignore-errors (close body :abort t))))
              (values (if (streamp body) (body-text body 2048) body) (dex:response-status condition)
                      (dex:response-headers condition) nil))))))))

;;; --- server-sent events -------------------------------------------------------

(defun read-events (stream on-event &key limit idle)
  "Read the server-sent events on STREAM -- characters, or octets read as
UTF-8 one at a time -- calling ON-EVENT with each event's `data:' lines
joined by newlines, until it answers true or the stream ends; other fields
and comments are skipped, past LIMIT characters the read signals, and so
does IDLE seconds without a line -- a comment is a line, so a server's ping
keeps the stream alive."
  (let ((stream (if (ignore-errors (subtypep (stream-element-type stream) 'integer))
                    (flexi-streams:make-flexi-stream stream :external-format :utf-8)
                    stream))
        (data '())
        (count 0))
    (flet ((dispatch ()
             (and data (funcall on-event (format nil "~{~a~^~%~}" (reverse (shiftf data '()))))))
           (next-line ()
             ;; A deadline, not dexador's :read-timeout: that is SO_RCVTIMEO,
             ;; which a TLS stream on SBCL never feels -- cl+ssl sets the fd
             ;; non-blocking and parks in WAIT-UNTIL-FD-USABLE, which only a
             ;; deadline bounds. Without this, a half-open connection would hold
             ;; the read open indefinitely. The read alone is under it: ON-EVENT
             ;; may run for as long as it needs.
             (if idle
                 (handler-case (sb-sys:with-deadline (:seconds idle) (read-line stream nil nil))
                   (sb-sys:deadline-timeout ()
                     (error "event stream silent for ~a seconds" idle)))
                 (read-line stream nil nil))))
      (loop for line = (next-line)
            while line
            do (when (and limit (> (incf count (1+ (length line))) limit))
                 (error "event stream over ~:d characters" limit))
               (let ((line (string-right-trim '(#\Return) line)))
                 (cond ((zerop (length line)) (when (dispatch) (return)))
                       ((uiop:string-prefix-p "data:" line)
                        (push (string-left-trim " " (subseq line 5)) data))))
            finally (dispatch)))))

;;; --- a client socket ended from another thread ----------------------------------
;;; A websocket-driver client reads on a thread of its own, and anything that
;;; drops a socket -- a stop, a supervisor that heard nothing, a replaced
;;; line -- runs somewhere else. Every client that dials out over TLS needs
;;; the one safe way to do that: the channels' platform sockets and the link
;;; cell's line to its relay.

(defun sever-websocket (ws &key (grace-seconds 3))
  "Disconnect WS from ANY thread without touching its stream state."
  ;;
  ;; WSD:CLOSE-CONNECTION is reader-thread-only: called from anywhere else it
  ;; CLOSEs the transport stream under the parked reader — on a TLS client that
  ;; is SSL_free racing SSL_read (a libssl memory fault that
  ;; would corrupt malloc/OpenSSL locks and wedge process exit) — and then
  ;; DESTROY-THREADs the reader. This sever instead shutdown(2)s the socket fd:
  ;; the parked read wakes with EOF while every stream/SSL structure stays
  ;; valid, and the reader runs wsd's own close path on itself, tearing the
  ;; stream down exactly once on the thread that uses it. We wait up to
  ;; GRACE-SECONDS for that self-close.
  ;;
  ;; With no live reader (never connected, reader already self-closed, or
  ;; called ON the reader) no concurrent stream user exists, so the plain
  ;; WSD:CLOSE-CONNECTION is safe and reclaims whatever the handshake opened.
  (when ws
    (let ((reader (ignore-errors
                    (websocket-driver.ws.client::read-thread ws))))
      (cond
        ((eq (wsd:ready-state ws) :closed))
        ((or (null reader)
             (not (bt2:thread-alive-p reader))
             (eq reader (bt2:current-thread)))
         (ignore-errors (wsd:close-connection ws)))
        (t
         ;; The raw socket fd under the transport stream, TLS or plain.
         (let* ((stream (ignore-errors (wsd:socket ws)))
                (raw (if (typep stream 'cl+ssl::ssl-stream)
                         (cl+ssl::ssl-stream-socket stream)
                         stream))
                (fd (and (streamp raw) (cl+ssl:stream-fd raw))))
           (when (integerp fd)
             (sb-alien:alien-funcall
              (sb-alien:extern-alien
               "shutdown" (function sb-alien:int sb-alien:int sb-alien:int))
              fd 2)))
         (wait-until (lambda () (eq (wsd:ready-state ws) :closed))
                     (deadline-after grace-seconds) 0.05)
         (unless (eq (wsd:ready-state ws) :closed)
           (warn "ws sever: reader did not close within ~as; abandoning"
                 grace-seconds))))))
  t)
