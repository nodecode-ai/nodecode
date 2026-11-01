;;;; provider-watchdog-test.lisp --- the idle watchdog on a provider attempt.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; dexador's :READ-TIMEOUT never reaches a TLS socket (provider.lisp, the
;;;; idle deadline), so the attempt keeps a deadline of its own. These gates dial a
;;;; loopback TLS server — the transport the bug lives on, and the one where
;;;; nothing but the watchdog can fire — with the configured timeout at 1 s,
;;;; and lock: the verdict's scope and wording by phase, that a lively stream
;;;; is never cut, and that a cut socket is closed and not pooled. A port
;;;; nothing listens on is no silence: its refusal ends the attempt at once.
;;;;
;;;; The server's certificate is loopback-ca.pem beside this file, its key
;;;; loopback-ca-key.pem: self-signed for IP 127.0.0.1, a hundred years long,
;;;; and trusted by the client through dexador's CA bundle for the walk.
;;;;
;;;; The server frames each event as one chunk and writes the sentinel with
;;;; the terminal chunk in a single flush, as a provider does: the walk's
;;;; drain of a finished body reads only what has already arrived.

(in-package #:nodecode.test)

(defun crlf-lines (&rest lines)
  "LINES, each ended by a CR LF, as one string."
  (format nil "~{~a~c~c~}" (loop for line in lines append (list line #\Return #\Linefeed))))

(defvar *paced-flush-at* nil
  "When the :PACED server flushed its first line, in internal real time; the
client side of the same process reads the same clock.")

(defun loopback-ca-path (name)
  (uiop:native-namestring
   (asdf:system-relative-pathname :nodecode (format nil "test/engine/~a" name))))

(defun serve-silence (mode)
  "Listen on a loopback port; take one TLS connection; read its request
head; then by MODE: :NEVER writes nothing; :HEAD writes a 200
text/event-stream chunked head and nothing more; :LINE writes the head and
one comment line, then nothing more; :DRIP writes the head, then one data
chunk every 0.2 s for 3 s, then the sentinel. The content modes stream chat
deltas after the head: :STALL one word, then an empty delta and a keep-alive
comment every 0.1 s for 10 s; :CHATTY a word every 0.2 s for 3 s, then the
sentinel; :WARM keep-alive comments and empty deltas every 0.1 s for 1.5 s,
then a word and the sentinel; :COLD the same keep-alives for 10 s and never
a word."
  ;; The head declares keep-alive, so a body that ended cleanly WOULD be
  ;; pooled: the pool gate is not trivially green. The thread then reads until
  ;; the client closes, so its end proves the socket was closed. => (values
  ;; URL THREAD).
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (thread
           (nlk:spawn "silent-provider-fixture"
             (nlk:with-handlers ((error () nil))
               (nlk:with-cleanup ((usocket:socket-close listener))
                 (let ((socket (usocket:socket-accept listener)))
                   (nlk:with-cleanup ((usocket:socket-close socket))
                     (let ((stream (cl+ssl:make-ssl-server-stream
                                    (usocket:socket-stream socket)
                                    :certificate (loopback-ca-path "loopback-ca.pem")
                                    :key (loopback-ca-path "loopback-ca-key.pem"))))
                       (labels ((put (text)
                                  (write-sequence (sb-ext:string-to-octets
                                                   text :external-format :latin-1)
                                                  stream)
                                  (finish-output stream))
                                (chunk (data)
                                  (put (crlf-lines (format nil "~x" (length data)) data)))
                                (last-chunk (data)
                                  ;; the sentinel and the terminal chunk in
                                  ;; one write, as a provider sends them
                                  (put (crlf-lines (format nil "~x" (length data)) data "0" ""))))
                           (loop with tail = 0
                                 for octet = (read-byte stream nil)
                                 while octet
                                 do (setf tail (logand #xFFFFFFFF
                                                       (logior (ash tail 8) octet)))
                                 until (= tail #x0D0A0D0A))
                           (unless (eq mode :never)
                             (put (crlf-lines "HTTP/1.1 200 OK" "content-type: text/event-stream"
                                              "transfer-encoding: chunked" "")))
                           (when (eq mode :line)
                             (chunk (format nil ": ~a~%"
                                            (make-string 160 :initial-element #\-))))
                           (when (eq mode :drip)
                             (loop repeat 15
                                   do (sleep 0.2)
                                      (chunk (format nil "data: {\"n\": 1, \"pad\": ~s}~%~%"
                                                     (make-string 130 :initial-element #\-))))
                             (last-chunk (format nil "data: [DONE]~%~%")))
                           (when (eq mode :paced)
                             ;; one 20-byte data: line, flushed, then a
                             ;; second's silence before the next
                             (chunk (format nil "data: {\"n\": 1234567}~%~%"))
                             (setf *paced-flush-at* (get-internal-real-time))
                             (sleep 1)
                             (chunk (format nil "data: {\"n\": 2}~%~%"))
                             (last-chunk (format nil "data: [DONE]~%~%")))
                           (flet ((word () (chunk (chat-delta-line "hi")))
                                  (warm ()
                                    (chunk (chat-delta-line ""))
                                    (chunk (format nil ": keep-alive~%~%"))))
                             (case mode
                               (:stall (word) (loop repeat 100 do (sleep 0.1) (warm)))
                               (:chatty (loop repeat 15 do (sleep 0.2) (word))
                                (last-chunk (format nil "data: [DONE]~%~%")))
                               (:warm (loop repeat 15 do (sleep 0.1) (warm))
                                (word)
                                (last-chunk (format nil "data: [DONE]~%~%")))
                               (:cold (loop repeat 100 do (sleep 0.1) (warm)))))
                           ;; hold the socket until the client lets go
                           (ignore-errors (loop while (read-byte stream nil))))))))))))
    (values (format nil "https://127.0.0.1:~d/v1/chat" port) thread)))

(defun thread-ends-within (thread seconds)
  (await (:tries (round (* seconds 20)) :rest 0.05) (not (bt2:thread-alive-p thread))))

(defun walk-attempt (endpoint timeout &optional (on-frame #'identity))
  "One attempt at ENDPOINT under a TIMEOUT-second request timeout, each frame
the fold receives handed to ON-FRAME => the provider error signalled, or :DONE."
  (handler-case (progn (nle::walk-provider-stream
                        (nlk:lambda-ignoring (frame finish record)
                          (funcall on-frame frame))
                        :config (nle::make-effective-provider-config
                                 :lane "openai-completions" :endpoint endpoint
                                 :model "silent" :api-key "none" :request-timeout timeout)
                        :request-json "{}" :headers '() :finish-on-sentinel t)
                       :done)
    (nle::provider-error (e) e)))

(defun walk-silence (mode &key on-frame)
  "One attempt against a SERVE-SILENCE server, the loopback CA trusted."
  ;; ON-FRAME, when given, sees each decoded frame as the fold receives it.
  ;; => (values OUTCOME SECONDS FRAMES URL SERVER), OUTCOME the condition
  ;; signalled or :DONE.
  (nlk:bind (((url server) (serve-silence mode)) (frames 0) (t0 (get-internal-real-time))
             (outcome (let ((dexador.backend.usocket:*ca-bundle*
                              (loopback-ca-path "loopback-ca.pem")))
                        (walk-attempt url 1 (lambda (frame)
                                              (incf frames)
                                              (when on-frame (funcall on-frame frame)))))))
    (values outcome
            (/ (- (get-internal-real-time) t0) internal-time-units-per-second)
            frames url server)))

(deftests watchdog-cuts (mode scope silence truncation)
  ;; The cut's shared invariants: within about 2 s, nothing folded, the socket
  ;; closed and not pooled -- and the verdict's scope and wording by phase.
  (multiple-value-bind (outcome seconds frames url server) (walk-silence mode)
    (is (typep outcome 'nle::provider-error) "the attempt failed")
    (is (eq truncation (and (typep outcome 'nle::provider-stream-incomplete) t))
        (format nil "after headers, and only then, the cut is stream truncation: ~a" outcome))
    (when (typep outcome 'nle::provider-error)
      (is (eq scope (nle::provider-error-scope outcome)))
      (is (search silence (nle::provider-error-detail outcome))
          (format nil "the detail names the silence: ~a"
                  (nle::provider-error-detail outcome))))
    (is (= 0 frames) "nothing folded")
    (is (< seconds 3) (format nil "cut in ~,1f s, within about 2" seconds))
    (is (= 0 (nle::pooled-connection-count url)))
    (is (thread-ends-within server 3) "the server saw the socket close"))
  (a-server-that-never-answers-as-a-request-failure :never :request "no response for 1 s" nil)
  ;; The first read of the stream phase is the decoder's own fill: a head
  ;; with no body bytes behind it is cut there, as truncation.
  (a-head-and-then-silence-as-incomplete :head :stream "no byte for 1 s" t)
  ;; A comment line folds nothing.
  (a-stream-silent-after-its-first-line-as-incomplete :line :stream "no byte for 1 s" t))

(deftest a-refused-dial-ends-the-attempt-at-once-naming-the-address
    (let ((port (closed-loopback-port)) (t0 (get-internal-real-time))))
  ;; A stopped Ollama: the host refuses in milliseconds, and without this
  ;; the attempt would wait out its request timeout and say `no response
  ;; for 120 s'. Here the timeout is 10 s.
  (let ((outcome (walk-attempt (format nil "http://127.0.0.1:~d/v1/chat/completions" port) 10)))
    (is (typep outcome 'nle::provider-connection-refused) (format nil "a refusal, named as one: ~a" outcome))
    (is (< (/ (- (get-internal-real-time) t0) internal-time-units-per-second) 2) "at once, not at the 10 s timeout")
    (when (typep outcome 'nle::provider-error)
      (is (eq :request (nle::provider-error-scope outcome)))
      (is (equal (format nil "nothing listens at 127.0.0.1:~d — is the server running?" port)
                 (nle::provider-error-detail outcome))))))

(deftest a-line-reaches-the-fold-the-instant-its-newline-does (let ((folded-at '())))
  ;; The walk splits SSE lines from the socket's octets itself and decodes
  ;; each as its newline arrives. Through dexador's character decoder a
  ;; 20-byte line sat until 128 octets had accumulated: here, until the
  ;; second line and the sentinel a second later. The server stamps its
  ;; flush; the fold stamps its receipt; the same process clock reads both.
  (setf *paced-flush-at* nil)
  (multiple-value-bind (outcome seconds frames url server)
      (walk-silence :paced :on-frame (nlk:lambda-ignoring (frame)
                                       (push (get-internal-real-time) folded-at)))
    (is (eq :done outcome) (format nil "the walk completed: ~a" outcome))
    (is (= 2 frames) "both lines folded")
    (is-present (first-at (and *paced-flush-at* (car (last folded-at)))) "both stamped"
      (let ((lag (/ (- first-at *paced-flush-at*) internal-time-units-per-second)))
        (is (< lag 0.3) (format nil "the first line folded ~,3f s after its flush" lag))))
    (is (>= seconds 0.9) (format nil "the stream took its second: ~,1f" seconds))
    (is (= 1 (nle::pooled-connection-count url)) "a clean stream pools its socket")
    (nle::flush-provider-connection-pool)
    (is (thread-ends-within server 3) "the flush closed it: the server saw the socket close")))

(deftest watchdog-never-cuts-a-stream-that-keeps-dripping ()
  ;; dexador's own :READ-TIMEOUT rides along at four times the configured
  ;; number: over plain http it is the fd-stream's timeout and, at the same
  ;; number, raced the watchdog to the verdict with its own IO-TIMEOUT
  ;; wording; as a backstop it fires only if the watchdog itself is gone.
  ;; The drip's 0.2 s cadence is well inside both.
  (multiple-value-bind (outcome seconds frames url server) (walk-silence :drip)
    (is (eq :done outcome) (format nil "the walk completed: ~a" outcome))
    (is (= 15 frames) "every dripped frame folded")
    (is (>= seconds 2.9) (format nil "the stream took its 3 s: ~,1f" seconds))
    ;; The contrast with a cut: a stream that ended cleanly hands its socket
    ;; to the pool, so the next round reuses it.
    (is (= 1 (nle::pooled-connection-count url)) "a clean stream pools its socket")
    (nle::flush-provider-connection-pool)
    (is (thread-ends-within server 3) "the flush closed it: the server saw the socket close")))

;;; --- the first token's wait ------------------------------------------------
;;; A watched attempt (ATTEMPT-WATCH) under a 5 s idle clock: its deadline or
;;; a skip cuts the wait long before that clock would, as a slow start whose
;;; scope is the phase it waited in; a watch whose token came leaves the idle
;;; clock alone.

(defun walk-watched (mode watch timeout)
  "One attempt against a SERVE-SILENCE server of MODE under a TIMEOUT-second
idle clock, WATCH the thread's attempt watch => (values OUTCOME SECONDS SERVER)."
  (multiple-value-bind (url server) (serve-silence mode)
    (let* ((t0 (get-internal-real-time))
           (outcome (let ((dexador.backend.usocket:*ca-bundle* (loopback-ca-path "loopback-ca.pem"))
                          (nle::*attempt-watch* watch))
                      (walk-attempt url timeout))))
      (values outcome (/ (- (get-internal-real-time) t0) internal-time-units-per-second) server))))

(defun watch-due-in (seconds)
  "An attempt watch, a candidate behind it, whose wait is cut SECONDS from now."
  (nle::make-attempt-watch :seconds seconds :next '(:provider "alt" :model "alt-model")
                           :deadline (+ (get-internal-real-time)
                                        (round (* seconds internal-time-units-per-second)))))

(deftests a-watched-wait-past-its-deadline-is-a-slow-start (mode scope)
  (multiple-value-bind (outcome seconds server) (walk-watched mode (watch-due-in 1) 5)
    (is (typep outcome 'nle::provider-slow-start) (format nil "a slow start: ~a" outcome))
    (when (typep outcome 'nle::provider-error)
      (is (eq scope (nle::provider-error-scope outcome)))
      (is (equal "no first token in 1 s" (nle::provider-error-detail outcome))))
    (is (< seconds 3) (format nil "cut in ~,1f s, not at the 5 s idle clock" seconds))
    (is (thread-ends-within server 3) "the server saw the socket close"))
  (before-the-response-head :never :request)
  ;; A comment line is no token: the wait goes on past it.
  (after-a-comment-line :line :stream))

(deftest a-skip-cuts-a-watched-wait-within-a-second ()
  (let ((watch (nle::make-attempt-watch :next '(:provider "alt" :model "alt-model"))))
    (nlk:spawn "skip-press-fixture"
      (sleep 0.5)
      (setf (nle::attempt-watch-skip watch) "skipped from the room"))
    (multiple-value-bind (outcome seconds server) (walk-watched :line watch 5)
      (is (typep outcome 'nle::provider-slow-start) (format nil "a slow start: ~a" outcome))
      (when (typep outcome 'nle::provider-error)
        (is (equal "skipped from the room" (nle::provider-error-detail outcome)) "the skip's words are the detail"))
      (is (< seconds 2.5) (format nil "cut ~,1f s in, within a second of the skip" seconds))
      (is (thread-ends-within server 3) "the server saw the socket close"))))

(defun chat-delta-line (text)
  "One SSE data line and its blank: a chat chunk whose delta content is TEXT."
  (format nil "data: {\"choices\": [{\"index\": 0, \"delta\": {\"content\": ~s}}]}~%~%" text))

;;; --- the content clock -------------------------------------------------------
;;; Bytes are not progress: past its first content an attempt is cut when its
;;; window passes with no more, however lively its socket, and before its first
;;; content when the first window does. Each frame's delta text streams through a lane assembly as
;;; the chat lane streams it, so an empty delta is what the lanes make of one.

(defun frame-delta-text (frame)
  "FRAME's choices[0].delta.content, or NIL."
  (ignore-errors (gethash "content" (gethash "delta" (aref (gethash "choices" frame) 0)))))

(defun walk-content (mode &key (windows 1) (first-windows 15))
  "One attempt against a SERVE-SILENCE server of MODE under a 1 s idle clock
and a content clock of WINDOWS idle timeouts, FIRST-WINDOWS before the first
content => (values OUTCOME SECONDS TEXT SERVER URL), OUTCOME the condition
signalled or :DONE, TEXT what the assembly gathered."
  (multiple-value-bind (url server) (serve-silence mode)
    (let* ((asm (nle::make-lane-assembly))
           (t0 (get-internal-real-time))
           (outcome
             (let ((dexador.backend.usocket:*ca-bundle* (loopback-ca-path "loopback-ca.pem"))
                   (nle::*content-idle-windows* windows)
                   (nle::*first-content-idle-windows* first-windows))
               (handler-case
                   (progn (nle::walk-provider-stream
                           (nlk:lambda-ignoring (frame finish record)
                             (let ((text (frame-delta-text frame)))
                               (when (stringp text) (nle::assembly-text-delta asm "text-0" text))))
                           :config (nle::make-effective-provider-config
                                    :lane "openai-completions" :endpoint url :model "stalling"
                                    :api-key "none" :request-timeout 1)
                           :request-json "{}" :headers '() :finish-on-sentinel t)
                          :done)
                 (nle::provider-error (e) e)))))
      (values outcome
              (/ (- (get-internal-real-time) t0) internal-time-units-per-second)
              (get-output-stream-string (nle::lane-assembly-content asm))
              server url))))

(deftest an-attempt-whose-content-stops-is-cut-however-lively-its-bytes ()
  ;; One word, then an empty delta and a keep-alive comment every 0.1 s: the
  ;; idle clock never fires on this, and before the content clock the turn
  ;; waited on it until someone cancelled.
  (multiple-value-bind (outcome seconds text server url) (walk-content :stall)
    (is (typep outcome 'nle::provider-stream-incomplete) (format nil "truncation: ~a" outcome))
    (when (typep outcome 'nle::provider-error)
      (is (eq :stream (nle::provider-error-scope outcome)))
      (is (equal "no content for 1 s" (nle::provider-error-detail outcome))
          (format nil "the detail names the content's silence: ~a" (nle::provider-error-detail outcome))))
    (is (equal "hi" text) "the word before the stall folded")
    (is (< 0.9 seconds 2.5) (format nil "cut in ~,1f s, at the 1 s window" seconds))
    (is (= 0 (nle::pooled-connection-count url)) "a cut socket is not pooled")
    (is (thread-ends-within server 3) "the server saw the socket close")))

(deftest content-that-keeps-coming-is-never-cut-by-the-content-clock ()
  ;; A word every 0.2 s for 3 s: three windows long, and no ceiling on it.
  (multiple-value-bind (outcome seconds text server url) (walk-content :chatty)
    (is (eq :done outcome) (format nil "the walk completed: ~a" outcome))
    (is (= 30 (length text)) "every word folded")
    (is (>= seconds 2.9) (format nil "the stream took its 3 s: ~,1f" seconds))
    (is (= 1 (nle::pooled-connection-count url)) "a clean stream pools its socket")
    (nle::flush-provider-connection-pool)
    (is (thread-ends-within server 3) "the flush closed it: the server saw the socket close")))

(deftest keep-alives-before-the-first-content-wait-out-only-the-first-window ()
  ;; With no failover candidate, nothing but the idle clock bounded the wait
  ;; for a first token, and keep-alives satisfy it: the turn waited for as long
  ;; as they came (proofs/RetryLadder.lean, firstTokenMain).
  (multiple-value-bind (outcome seconds text server url) (walk-content :cold :first-windows 2)
    (is (typep outcome 'nle::provider-stream-incomplete) (format nil "truncation: ~a" outcome))
    (when (typep outcome 'nle::provider-error)
      (is (eq :stream (nle::provider-error-scope outcome)) "the stream budget retries it")
      (is (equal "no first token for 2 s" (nle::provider-error-detail outcome))
          (format nil "the detail names the first token's silence: ~a" (nle::provider-error-detail outcome))))
    (is (equal "" text) "nothing folded")
    (is (< 1.9 seconds 3.5) (format nil "cut in ~,1f s, at the 2 s first window" seconds))
    (is (= 0 (nle::pooled-connection-count url)) "a cut socket is not pooled")
    (is (thread-ends-within server 3) "the server saw the socket close")))

(deftest a-warm-up-inside-the-first-window-is-never-cut ()
  ;; A cold prompt keeping its socket warm for 1.5 s, past the 1 s window,
  ;; before its first word: the first window is fifteen idle timeouts.
  (multiple-value-bind (outcome seconds text server) (walk-content :warm)
    (is (eq :done outcome) (format nil "the walk completed: ~a" outcome))
    (is (equal "hi" text) "the word after the warm-up folded")
    (is (>= seconds 1.4) (format nil "the warm-up took its 1.5 s: ~,1f" seconds))
    (nle::flush-provider-connection-pool)
    (is (thread-ends-within server 3) "the flush closed it: the server saw the socket close")))

(deftest the-content-window-counts-idle-timeouts ()
  (flet ((clock (request)
           (nle::content-clock-for
            (nle::make-effective-provider-config
             :lane "openai-completions" :endpoint "https://clock.example/v1" :model "m"
             :api-key "k" :request-timeout request))))
    (is (= 360 (nle::content-clock-seconds (clock 120))) "three idle timeouts at the default")
    (is (= 90 (nle::content-clock-seconds (clock 30))) "a provider's own idle timeout scales it")
    (let ((nle::*content-idle-windows* 1/2))
      (is (= 120 (nle::content-clock-seconds (clock 120))) "never under one idle timeout"))
    (is (= 1800 (nle::content-clock-first-seconds (clock 120))) "fifteen before the first content")
    (is (= 450 (nle::content-clock-first-seconds (clock 30))))
    (let ((nle::*content-idle-windows* nil))
      (is (null (nle::content-clock-seconds (clock 120))) "no window after content")
      (is (= 1800 (nle::content-clock-first-seconds (clock 120))) "and the first one stands"))
    (let ((nle::*content-idle-windows* nil) (nle::*first-content-idle-windows* nil))
      (is (null (clock 120)) "both off, no clock"))
    (is (not (nle::content-part-p :text-delta '(:id "t" :text ""))) "an empty fragment is no content")
    (is (nle::content-part-p :tool-input-start '(:id "c" :tool-name "read")) "a tool call's start is content")
    (is (not (nle::content-part-p :response-metadata '(:id "r"))) "metadata is no content")))

(deftest a-watch-whose-token-came-leaves-the-idle-clock-alone ()
  ;; Past its deadline, but a token already streamed: the cut is the idle
  ;; clock's own, truncation and not a slow start.
  (let ((watch (watch-due-in 0)))
    (setf (nle::attempt-watch-token-p watch) t)
    (let ((outcome (walk-watched :line watch 1)))
      (is (typep outcome 'nle::provider-stream-incomplete) (format nil "truncation: ~a" outcome))
      (is (not (typep outcome 'nle::provider-slow-start)) "not a slow start"))))
