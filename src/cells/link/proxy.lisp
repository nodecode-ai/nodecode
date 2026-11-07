;;;; proxy.lisp --- one browser request, arrived down the line, answered here.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A stream (a FLOW here: CL owns the other word) is one browser request:
;;;; an HTTP exchange, or the page's socket to /gateway. Every one is decided
;;;; on this machine. A browser without an allowed cookie reaches the pairing
;;;; page and its two calls and nothing else; an allowed one is replayed on
;;;; loopback against this organism's own gateway, with the operator's Bearer
;;;; added and the browser's cookie taken off, exactly as a shell would ask --
;;;; so the gateway, its routes and the web page are what they are for
;;;; `nodecode web', and the socket needs no token of its own (the Bearer at
;;;; the upgrade is the operator at the door, surface/gateway.lisp). The one
;;;; header it adds names the browser (+VIA+), so the link's route can tell
;;;; the page it came through.
;;;;
;;;; A page of another site can make a browser send its cookie along (it is
;;;; SameSite=Lax, so a link someone followed arrives signed in), so a socket
;;;; and every request that changes something must also come from this
;;;; machine's own origin -- the one the relay named at welcome.

(in-package #:nodecode-link)

(nlk:define-record (flow (:copier nil) (:predicate nil))
  "One browser request on the line."
  (id 0)
  (kind "http")
  (method "GET")
  (path "/")
  (headers '())
  (ip "")
  (place "")
  (body (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  ;; A socket's loopback leg, the browser it was let in as, the frames of a
  ;; message still arriving, and what the gateway said before the relay heard
  ;; the upgrade was taken.
  (socket nil)
  (browser nil)
  (pieces '())
  (early '())
  (ready nil))

(nlk:access (flow flow))

(defvar *flows* (make-hash-table)
  "The line's open flows by stream number. Guarded by *LOCK*.")

(defparameter +body-max+ (* 64 1024 1024)
  "The largest request body a browser may send down the line.")

(defparameter *page-directory* (asdf:system-relative-pathname "nodecode-link" "page/"))

(defparameter +forwarded+
  '("accept" "accept-language" "content-type" "cache-control" "pragma"
    "if-none-match" "if-modified-since" "if-range" "range")
  "The browser's headers the gateway is shown. Not its cookie, not its
authorization, not its origin: on the loopback leg the request is the
operator's.")

(defparameter +via+ "x-nodecode-link"
  "The header the loopback leg names the allowed browser in, so a route can
tell a request that came down the line from one made on this machine: the
link's own route marks the page's browser with it. A browser cannot send it,
since +FORWARDED+ is all of its headers the gateway sees.")

(defparameter +hop+
  '("connection" "keep-alive" "transfer-encoding" "upgrade" "te" "trailer"
    "content-length" "proxy-connection")
  "What never crosses back from the gateway: this leg's own framing.")

(defun header (flow name)
  (cdr (assoc name flow.headers :test #'string=)))

(defun path-only (flow)
  (subseq flow.path 0 (position #\? flow.path)))

(defun query-value (flow name)
  (let ((query (subseq flow.path (min (length flow.path) (1+ (or (position #\? flow.path)
                                                                  (length flow.path)))))))
    (loop for pair in (uiop:split-string query :separator "&")
          for (key value) = (uiop:split-string pair :separator "=" :max 2)
          when (string= key name) return value)))

(defun own-origin-p (flow)
  "Whether FLOW came from a page of this machine's own address."
  (let ((origin (kept "origin")))
    (and origin (equal (header flow "origin") origin))))

;;; --- answering ------------------------------------------------------------------------

(defun forget-flow (flow)
  "Take FLOW off the line's table => T when it was still there."
  (with-link-lock (and (eq flow (gethash flow.id *flows*)) (remhash flow.id *flows*))))

(defun flow-live-p (flow)
  (with-link-lock (eq flow (gethash flow.id *flows*))))

(defun respond (flow status headers &optional (body ""))
  "Answer FLOW whole: STATUS, HEADERS ((NAME . VALUE) ...) and BODY."
  (when (forget-flow flow)
    (let ((octets (if (stringp body) (sb-ext:string-to-octets body :external-format :utf-8) body)))
      (send-control "head" "s" flow.id "status" status
                    "headers" (map 'vector (lambda (pair) (vector (car pair) (cdr pair)))
                                   (list* '("cache-control" . "no-store")
                                          '("x-content-type-options" . "nosniff")
                                          '("referrer-policy" . "no-referrer")
                                          headers)))
      (when (plusp (length octets)) (send-data flow.id octets))
      (send-control "end" "s" flow.id))))

(defun respond-json (flow status object &rest headers)
  (respond flow status (list* '("content-type" . "application/json") headers)
           (nlk:encode-json-object object)))

(defun refuse (flow status code message)
  (respond-json flow status (nlk:make-json-object
                             "error" (nlk:make-json-object "code" code "message" message))))

(defun redirect (flow location)
  (respond flow 302 (list (cons "location" location))))

(defun reset (flow why)
  (when (forget-flow flow)
    (send-control "reset" "s" flow.id "why" why)))

;;; --- the pairing page and its two calls -------------------------------------------

(defparameter +page-types+
  '(("html" . "text/html; charset=utf-8") ("js" . "text/javascript; charset=utf-8")
    ("css" . "text/css; charset=utf-8")))

(defparameter +page-policy+
  "default-src 'self'; connect-src 'self'; style-src 'self'; script-src 'self'; img-src 'self' data:; base-uri 'none'; form-action 'none'; frame-ancestors 'none'")

(defun serve-page-file (flow name)
  (let ((file (merge-pathnames name *page-directory*)))
    (respond flow 200 (list (cons "content-type" (cdr (assoc (pathname-type file) +page-types+
                                                            :test #'string=)))
                            (cons "content-security-policy" +page-policy+))
             (nlk:read-text file))))

(defun announce-ask (ask)
  "Tell the attached shells a browser is asking."
  ;; Said once, never on the board: the board rides into the model's prompt.
  (nle:notice (format nil "link: ~a~@[ in ~a~] asks to open this machine.~%~
                           If its page shows ~a, let it in: /link allow ~a  (or /link deny)"
                      (getf ask :agent) (and (plusp (length (getf ask :place))) (getf ask :place))
                      (code-text (getf ask :code)) (getf ask :code))))

(defun answer-pairing (flow browser path)
  (cond
    ((and browser (string= path "/_link/pair")) (redirect flow "/web/"))
    ((member path '("/_link/pair" "/_link/pair.js" "/_link/pair.css") :test #'string=)
     (serve-page-file flow (if (string= path "/_link/pair") "pair.html" (subseq path 7))))
    ((not (string= path "/_link/ask")) (refuse flow 404 "not_found" "unknown route"))
    ((string= flow.method "GET")
     (multiple-value-bind (state cookie) (await-ask (query-value flow "id") 25)
       (apply #'respond-json flow 200 (nlk:make-json-object "state" (string-downcase state))
              (and cookie
                   (list (cons "set-cookie"
                               (format nil "~a=~a; Path=/; Secure; HttpOnly; SameSite=Lax; Max-Age=31536000"
                                       +cookie+ cookie)))))))
    ((not (string= flow.method "POST")) (refuse flow 405 "method_not_allowed" "GET or POST"))
    ((not (own-origin-p flow)) (refuse flow 403 "origin_refused" "an ask from a page this machine did not serve"))
    (browser (respond-json flow 200 (nlk:make-json-object "state" "allowed")))
    (t
     (let ((ask (start-ask flow.ip (agent-name (header flow "user-agent")) (place-name flow.place))))
       (if (stringp ask)
           (refuse flow 429 "ask_refused" ask)
           (progn
             (announce-ask ask)
             (respond-json flow 200 (nlk:make-json-object
                                     "id" (getf ask :id) "code" (code-text (getf ask :code))
                                     "seconds" *ask-seconds* "machine" (or (kept "machine") "")))))))))

;;; --- the loopback leg --------------------------------------------------------------------

(defun replay (flow browser)
  "Answer FLOW, a request of the allowed BROWSER's (its id), with what this
organism's gateway answers the same request."
  (multiple-value-bind (url token) (nle:gateway-endpoint)
    (let ((method (find (string-upcase flow.method) '(:get :head :post :put :patch :delete :options)
                        :test #'string=)))
      (cond
        ((null url) (refuse flow 503 "gateway_down" "this machine's gateway is not listening"))
        ((null method) (refuse flow 405 "method_not_allowed" "not a method the gateway answers"))
        (t
         (multiple-value-bind (body status headers)
             (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
               (dex:request (format nil "~a~a" url flow.path)
                            :method method
                            :headers (list* (cons "authorization" (format nil "Bearer ~a" token))
                                            (cons +via+ browser)
                                            (remove-if-not (lambda (pair) (member (car pair) +forwarded+
                                                                                  :test #'string=))
                                                           flow.headers))
                            :content (and (plusp (length flow.body))
                                          (coerce flow.body '(simple-array (unsigned-byte 8) (*))))
                            ;; The browser follows a redirect itself, to its own origin.
                            :max-redirects 0 :want-stream t :force-binary t
                            :keep-alive nil :use-connection-pool nil
                            :connect-timeout 5 :read-timeout 300))
           (nlk:with-cleanup ((when (streamp body) (ignore-errors (close body))))
             (when (flow-live-p flow)
               (send-control "head" "s" flow.id "status" status
                             "headers" (let ((pairs '()))
                                         (maphash (lambda (name value)
                                                    (unless (member name +hop+ :test #'string-equal)
                                                      (push (vector (string-downcase name) value) pairs)))
                                                  headers)
                                         (coerce pairs 'vector)))
               (when (streamp body)
                 (loop with buffer = (make-array 65536 :element-type '(unsigned-byte 8))
                       for count = (read-sequence buffer body)
                       while (and (plusp count) (flow-live-p flow))
                       do (send-data flow.id (subseq buffer 0 count))))
               (when (forget-flow flow)
                 (send-control "end" "s" flow.id))))))))))

(defun bridge (flow)
  "Open FLOW's loopback socket to /gateway and carry messages both ways."
  (multiple-value-bind (url token) (nle:gateway-endpoint)
    (unless url
      (return-from bridge (upgrade-refused flow 503)))
    (let ((socket (wsd:make-client (format nil "ws~a/gateway" (subseq url 4))
                                   :additional-headers
                                   (list (cons "authorization" (format nil "Bearer ~a" token))))))
      (flet ((carry (message)
               (let ((octets (if (stringp message)
                                 (sb-ext:string-to-octets message :external-format :utf-8)
                                 message)))
                 (with-link-lock
                   (if flow.ready
                       (send-data flow.id octets :text (stringp message))
                       (push (cons octets (stringp message)) flow.early))))))
        (wsd:on :message socket #'carry)
        (wsd:on :close socket (lambda (&key code reason)
                                (declare (ignore code reason))
                                (when (forget-flow flow)
                                  (send-control "end" "s" flow.id))))
        (setf flow.socket socket)
        (handler-case (wsd:start-connection socket)
          (error () (return-from bridge (upgrade-refused flow 502))))
        ;; The gateway speaks first (its challenge), and it may have spoken
        ;; before the relay heard the upgrade was taken: that waits here.
        (with-link-lock
          (send-control "head" "s" flow.id "status" 101 "headers" (vector))
          (loop for (octets . text) in (reverse (shiftf flow.early '()))
                do (send-data flow.id octets :text text))
          (setf flow.ready t))))))

(defun upgrade-refused (flow status)
  (when (forget-flow flow)
    (send-control "head" "s" flow.id "status" status "headers" (vector))))

(defun close-socket (flow)
  (nlk:when-let (socket flow.socket)
    (ignore-errors (nlk:sever-websocket socket :grace-seconds 1))))

;;; --- what arrives down the line ----------------------------------------------------------
;;; These run on the line's reader thread and never wait: a request is answered
;;; on a thread of its own once its body is in, and a socket is dialled on one.

(defun navigation-p (flow)
  (and (string= flow.method "GET") (search "text/html" (or (header flow "accept") ""))))

(defun answer (flow)
  "Decide FLOW, an HTTP request whose body is in, and answer it."
  (let ((browser (allowed-browser (header flow "cookie")))
        (path (path-only flow)))
    (cond ((uiop:string-prefix-p "/_link/" path) (answer-pairing flow browser path))
          ((null browser)
           (if (navigation-p flow)
               (redirect flow "/_link/pair")
               (refuse flow 401 "link_not_allowed" "this browser is not allowed on this machine")))
          ((and (not (member flow.method '("GET" "HEAD") :test #'string=)) (not (own-origin-p flow)))
           (refuse flow 403 "origin_refused" "a request from a page this machine did not serve"))
          ((string= path "/") (redirect flow "/web/"))
          (t (replay flow (gethash "id" browser))))))

(defun admit-socket (flow &aux (browser (allowed-browser (header flow "cookie"))))
  (cond ((null browser) (upgrade-refused flow 401))
        ((not (own-origin-p flow)) (upgrade-refused flow 403))
        ((not (string= (path-only flow) "/gateway")) (upgrade-refused flow 404))
        (t (setf flow.browser (gethash "id" browser))
           (bridge flow))))

(defun cut-browser (id)
  "Close every socket the browser ID holds: taken off the list, it is out now,
not at its next request."
  (dolist (flow (with-link-lock
                  (loop for flow being the hash-values of *flows*
                        when (equal (flow-browser flow) id) collect flow)))
    (when (forget-flow flow)
      (close-socket flow)
      (send-control "end" "s" (flow-id flow)))))

(defun work (flow function)
  "Run (FUNCTION FLOW) on a thread of its own; a failure resets the flow."
  (nlk:spawn (format nil "link-flow-~d" flow.id)
    (handler-case (funcall function flow)
      (error (condition)
        (close-socket flow)
        (reset flow (nlk:one-line (princ-to-string condition) :cap 200))))))

(defun on-open (object)
  "A browser request began."
  (nlk:with-json ((id :integer "s") (kind :text "kind") (method :text "method")
                  (path :text "path") (ip :string "ip") (place :string "place"))
      object
    (let ((flow (make-flow :id id :kind kind :method (or method "GET") :path (or path "/")
                           :ip (or ip "") :place (or place "")
                           :headers (loop for pair across (or (nlk:json-array object "headers") #())
                                          collect (cons (string-downcase (aref pair 0)) (aref pair 1))))))
      (with-link-lock (setf (gethash id *flows*) flow))
      (when (equal kind "ws")
        (work flow #'admit-socket)))))

(defun on-data (frame)
  "A piece of a request body, or a frame of a browser's message."
  (let ((flow (with-link-lock (gethash (frame-stream frame) *flows*)))
        (payload (frame-payload frame)))
    (cond ((null flow))
          ((equal flow.kind "http")
           (let* ((body flow.body)
                  (start (fill-pointer body))
                  (end (+ start (length payload))))
             (cond ((> end +body-max+) (reset flow "request body over 64 MiB"))
                   (t (when (> end (array-dimension body 0))
                        (adjust-array body (max end (* 2 (array-dimension body 0)))))
                      (setf (fill-pointer body) end)
                      (replace body payload :start1 start)))))
          ((logtest +more+ (frame-flags frame))
           (push payload flow.pieces))
          (flow.socket
           (let ((whole (apply #'concatenate '(vector (unsigned-byte 8))
                               (reverse (cons payload (shiftf flow.pieces '()))))))
             (if (logtest +text+ (frame-flags frame))
                 (wsd:send flow.socket (sb-ext:octets-to-string whole :external-format :utf-8))
                 (wsd:send flow.socket whole :type :binary)))))))

(defun on-end (id)
  "The browser finished: a request's body is in, or its socket closed."
  (nlk:when-let (flow (with-link-lock (gethash id *flows*)))
    (if (equal flow.kind "http")
        (work flow #'answer)
        (when (forget-flow flow) (close-socket flow)))))

(defun on-reset (id)
  (nlk:when-let (flow (with-link-lock (gethash id *flows*)))
    (forget-flow flow)
    (close-socket flow)))

(defun drop-flows ()
  "The line is gone, and every flow with it."
  (dolist (flow (with-link-lock
                  (prog1 (loop for flow being the hash-values of *flows* collect flow)
                    (clrhash *flows*))))
    (close-socket flow)))
