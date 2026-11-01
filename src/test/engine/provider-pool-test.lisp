;;;; provider-pool-test.lisp --- provider connection reuse invariants.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The pool itself is dexador's; what this suite locks is the safety
;;;; contract provider.lisp layers over it: heredity hooks are registered by
;;;; construction, a flush actually empties the pool, and an aborted stream
;;;; can never hand its socket back for reuse.

(in-package #:nodecode.test)

(deftest provider-pool-heredity-hooks-are-registered ()
  ;; SAVE-LISP-AND-DIE runs *SAVE-HOOKS* itself, so pool flushing cannot be
  ;; skipped by any save path; the init hook rebuilds an empty pool on
  ;; restore. Both must be present in the loaded image, not documented.
  (is-table (hook hooks) (member hook hooks)
    ('nodecode.evolved::flush-provider-connection-pool sb-ext:*save-hooks*)
    ('nodecode.evolved::reset-provider-connection-pool sb-ext:*init-hooks*)))

(deftest provider-pool-flush-empties-the-pool (let ((key "https://pool-flush.test")
                                                    (stream (make-string-input-stream "cached"))))
  (dexador.connection-cache:push-connection key stream #'close)
  (nodecode.evolved::flush-provider-connection-pool)
  (is (null (dexador.connection-cache:steal-connection key)))
  (is (not (open-stream-p stream))))

(defun keep-alive-body (raw end on-live)
  "A dexador keep-alive body over RAW ending at END, whose close-action calls
ON-LIVE on the underlying stream only while it is open, as dexador's own does."
  (dexador.keep-alive-stream:make-keep-alive-stream
   raw :end end :on-close-or-eof (nlk:lambda-ignoring (underlying abort)
                                   (when (and underlying
                                              (open-stream-p underlying))
                                     (funcall on-live underlying)))))

(deftest aborted-stream-body-never-reaches-the-pool ()
  ;; Mirror of dexador's pooled close-action: it only pushes when the
  ;; underlying socket is still open. CLOSE-PROVIDER-STREAM-BODY must
  ;; hard-close the raw socket before that action runs on an aborted body.
  (let* ((pushed nil)
         (raw (make-string-input-stream "half-read response"))
         (body (keep-alive-body raw 18 (lambda (underlying) (declare (ignore underlying))
                                         (setf pushed t)))))
    (nodecode.evolved::close-provider-stream-body body)
    (is (not (open-stream-p raw)) "the aborted raw socket is closed")
    (is (null pushed) "the close-action saw a dead socket and did not pool it")))

(deftest drained-stream-body-close-is-idempotent ()
  ;; After EOF the wrapper has already run its close-action and dropped the
  ;; socket; the helper's second close must be a quiet no-op. :END 0 makes
  ;; the first read hit EOF without touching the underlying stream, and the
  ;; action mirrors dexador's guard: it only counts a live socket.
  (let* ((actions 0)
         (raw (make-string-input-stream "x"))
         (body (keep-alive-body raw 0 (lambda (underlying) (incf actions) (close underlying)))))
    (is (null (read-byte body nil nil)) "the wrapper reports EOF at once")
    (nodecode.evolved::close-provider-stream-body body)
    (is (= 1 actions) "EOF ran the close-action once; the close did not rerun it")))

(deftest pooled-connection-count-reads-the-host-key ()
  ;; The connect phase note says whether a round reuses a socket or dials:
  ;; the count is keyed the way dexador keys the pool, scheme://authority.
  (is-table (key endpoint) (equal key (nodecode.evolved::provider-connection-key endpoint))
    ("https://api.deepseek.com" "https://api.deepseek.com/v1/responses")
    ("http://127.0.0.1:8089" "http://127.0.0.1:8089/v1/responses"))
  (let ((endpoint "https://pool-count.test/v1/x")
        (stream (make-string-input-stream "idle")))
    (is (= 0 (nodecode.evolved::pooled-connection-count endpoint)))
    (dexador.connection-cache:push-connection "https://pool-count.test" stream #'close)
    (is (= 1 (nodecode.evolved::pooled-connection-count endpoint)))
    (dexador.connection-cache:steal-connection "https://pool-count.test")
    (is (= 0 (nodecode.evolved::pooled-connection-count endpoint)))))

;;; --- a body sent as a delta ---------------------------------------------------
;;; To an endpoint whose answers carry X-Body-Delta (src/tools/probes/body-relay.py),
;;; a body goes as a delta against the one its session sent there before;
;;; a relay that no longer holds that base answers 412 and gets the body
;;; again, against nothing.

(defun header-table (&rest pairs)
  "A response-header table of PAIRS, names lowercased as dexador keeps them."
  (alexandria:plist-hash-table pairs :test 'equal))

(defun delta-rebuilds (delta base length)
  "The body DELTA rebuilds from BASE, LENGTH bytes long."
  (let ((out (make-array length :element-type '(unsigned-byte 8))))
    (subseq out 0 (nlk::apply-delta delta base (length base) out))))

(deftest a-body-goes-as-a-delta-once-the-endpoint-says-it-takes-one
    (let ((nle::*delta-endpoints* (make-hash-table :test 'equal :synchronized t))
          (nle::*sent-bodies* (make-hash-table :test 'equal :synchronized t))
          (endpoint "http://relay.test:8081/v1/chat/completions")
          (one (sb-ext:string-to-octets "{\"messages\":[\"a long first message\",2,3],\"tools\":[]}"))
          (two (sb-ext:string-to-octets "{\"messages\":[\"a long first message\",2,3,4],\"tools\":[]}"))))
  (flet ((header (headers name) (cdr (assoc name headers :test #'string=))))
    (multiple-value-bind (content headers) (nle::body-wire endpoint one "s1")
      (is (eq one content) "an endpoint that said nothing takes the JSON as it is")
      (is (equal "application/json" (header headers "content-type"))))
    (nle::note-body-wire endpoint one "s1" (header-table "content-type" "text/event-stream"))
    (is (eq one (nle::body-wire endpoint one "s1")) "an answer without the header says nothing")
    (nle::note-body-wire endpoint one "s1" (header-table "x-body-delta" "1"))
    (multiple-value-bind (content headers) (nle::body-wire endpoint two "s1")
      (is (equal "application/octet-stream" (header headers "content-type")))
      (is (equal (subseq (nlk:sha256-text one) 7) (header headers "x-body-delta")) "against s1's last body")
      (is (equal (subseq (nlk:sha256-text two) 7) (header headers "x-body-sha256")))
      (is (< (length content) (length two)))
      (is (equalp two (delta-rebuilds content one (length two)))))
    (multiple-value-bind (content headers) (nle::body-wire endpoint two "s2")
      (is (equal "empty" (header headers "x-body-delta")) "a session with no body there goes against nothing")
      (is (equalp two (delta-rebuilds content (make-array 0 :element-type (quote (unsigned-byte 8))) (length two)))))
    ;; and WHOLE sends against nothing
    (is (equal "empty" (header (nth-value 1 (nle::body-wire endpoint two "s1" :whole t)) "x-body-delta")))))

(deftest a-relay-without-the-base-takes-the-body-again-against-nothing (with-temp-store ())
  (ensure-durable-session "relay-412")
  (with-durable-turn (turn "relay-412" :current t)
    (let* ((nle::*delta-endpoints* (make-hash-table :test 'equal :synchronized t))
           (nle::*sent-bodies* (make-hash-table :test 'equal :synchronized t))
           (context (user-context))
           (key (nle::provider-connection-key
                 (nle::effective-provider-config-endpoint
                  (nle::compiled-turn-context-provider-config context))))
           (sent '()))
      (setf (gethash key nle::*delta-endpoints*) t
            (gethash "relay-412" nle::*sent-bodies*)
            (list* "0000stale" (sb-ext:string-to-octets "{}") 0))
      (with-stubbed-fdefinition (dex:post (endpoint &rest args)
                                 (let ((base (cdr (assoc "x-body-delta" (getf args :headers)
                                                         :test #'string-equal))))
                                   (push base sent)
                                   (if (equal base "0000stale")
                                       (error 'dex:http-request-failed
                                              :body "{\"error\":{\"type\":\"body_delta_base_missing\"}}"
                                              :status 412 :headers (header-table "x-body-delta" "1")
                                              :uri (quri:uri endpoint) :method :post)
                                       (values (make-sse-stream (chunk-json "ok")) 200
                                               (header-table "content-type" "text/event-stream"
                                                             "x-body-delta" "1")))))
        (is (equal "ok" (gethash "content" (nle::call-provider-streaming context)))))
      (is (equal '("0000stale" "empty") (reverse sent)) "the stale base, then nothing")
      (let ((kept (gethash "relay-412" nle::*sent-bodies*)))
        ;; and the body that went is the session's base from now on
        (is (equal (first kept) (subseq (nlk:sha256-text (second kept)) 7)))
        (is (search "\"messages\"" (sb-ext:octets-to-string (second kept) :external-format :utf-8)))))))

(deftest provider-env-controls-are-read-again-at-image-start ()
  ;; A DEFPARAMETER over the environment keeps what the build's environment
  ;; held: every release sent no temperature whatever NODECODE_TEMPERATURE said.
  (let ((startup (mapcar #'car nlk::*startup-parameters*)))
    (dolist (name '(nle::*provider* nle::*endpoint* nle::*model* nle::*api-key*
                    nle::*temperature* nle::*top-p* nle::*max-completion-tokens*
                    nle::*tool-choice* nle::*reasoning-effort*
                    nle::*context-tokens-env-override*))
      (is (member name startup)
          (format nil "~s is read again when an image starts" name))))
  (sb-posix:setenv "NODECODE_TEMPERATURE" "0.6" 1)
  (nlk:with-cleanup ((sb-posix:unsetenv "NODECODE_TEMPERATURE"))
    (is (= 0.6 (funcall (cdr (assoc 'nle::*temperature* nlk::*startup-parameters*)))))))

(deftest a-rounds-ttft-and-stream-duration-add-up-to-the-round ()
  ;; A queued request's head comes at once and its first token seconds later:
  ;; the wait is TTFT's, and the stream runs from the first token.
  (let ((queued (nle::make-stream-cadence :sent-ms 0 :first-part-ms 150
                                          :first-token-ms 5000 :last-part-ms 6000))
        (silent (nle::make-stream-cadence :sent-ms 0 :first-part-ms 150 :last-part-ms 400)))
    (is-shape queued (nle::stream-cadence-ttft-ms = 5000) (nle::stream-cadence-duration-ms = 1000))
    ;; a stream with no token runs from its first part
    (is (= 250 (nle::stream-cadence-duration-ms silent)))))
