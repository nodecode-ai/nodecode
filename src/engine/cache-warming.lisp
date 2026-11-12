;;;; cache-warming.lisp --- a prompt cache kept alive while its session waits.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A request that places cache_control breakpoints writes its prompt into
;;;; the provider's cache for that marker's lifetime: five minutes, or an
;;;; hour. Every read refreshes the entry; nothing else does. A session that
;;;; waits past the lifetime -- an operator reading the answer, a tool that
;;;; runs for ten minutes, a turn waiting on a background job -- sends its
;;;; next round into an expired entry, and the whole prompt is written again
;;;; at the write price (1.25x input on Anthropic, against 0.1x for a read).
;;;;
;;;; So each round that went out with breakpoints arms one warmer for its
;;;; session. At nine tenths of the lifetime the warmer sends the round's own
;;;; request again, byte for byte, with max_tokens 1: the provider reads the
;;;; cached prefix, which refreshes it, and answers one token. The next round
;;;; replaces the warmer, so a session never holds more than one.
;;;;
;;;; A refresh is sent only when it pays: its price is a cache read of the
;;;; prompt and one output token, against the write the next round saves,
;;;; weighed by how likely that round is to come before the entry expires --
;;;; certain while the turn that sent the request is still open (its tool is
;;;; running), 0.15 once the turn settled and the session sits idle. Warming
;;;; stops 60 minutes after the round while the turn is open, 30 after it
;;;; settled, when the session moved to another model, when a refresh fails,
;;;; and when the expected saving drops under five cents. A round in flight
;;;; reads the prefix itself, so a warmer that comes due then waits.
;;;;
;;;; What a refresh cost is a side call of its session, purpose
;;;; cache_warming: the usage rollup counts it beside the rounds.

(in-package #:nodecode.evolved)

(nlk:define-section ("cache_warming")
  (:guide "Keeps the prompt cache of a waiting session alive: a request with cache breakpoints is sent again with one output token before its cache entry expires, when that is cheaper than writing the prompt again")
  ("mode" :choice :options '("idle" "streaming" "off") :default "idle"
          :doc "idle warms while a turn runs a tool and after it ends; streaming only while the turn is still open; off never"))

(defparameter *cache-warming-delay-fraction* 0.9
  "How far into the cache entry's lifetime a refresh is sent.")

(defparameter *cache-warming-margin-seconds* 10
  "The least time a refresh leaves before the entry would expire.")

(defparameter *cache-warming-max-age-seconds* '(:streaming 3600 :idle 1800)
  "How long after the round that armed it a warmer keeps refreshing, by phase:
:STREAMING while the round's turn is still open, :IDLE once it settled.")

(defparameter *cache-warming-continuation* '(:streaming 1 :idle 0.15)
  "The chance, by phase, that the next round comes before the entry expires.")

(defparameter *cache-warming-minimum-saving* 0.05
  "US dollars a refresh must be expected to save to be sent.")

(defstruct (cache-warmer (:conc-name warmer-))
  "One session's armed refresh: the round it repeats and when it went."
  session-id
  ;; the durable turn the round belonged to: open is :STREAMING, settled :IDLE
  turn
  ;; the lane function and the compiled context the round was sent with
  lane context
  ;; the round's exact request octets, max_tokens already 1
  body
  provider model
  ;; seconds the shortest cache entry the request wrote lives
  ttl
  ;; prompt tokens the round reported: fresh, read and written
  prompt-tokens
  ;; GET-UNIVERSAL-TIME of the round, and of the latest refresh
  armed-at refreshed-at
  (refreshes 0)
  timer)

(defvar *cache-warmers* (make-hash-table :test #'equal :synchronized t)
  "Session id -> its armed CACHE-WARMER.")

(defvar *rounds-in-flight* (make-hash-table :test #'equal :synchronized t)
  "Session id -> how many of its provider rounds are on the wire.")

(defvar *cache-warming-lock* (bt2:make-lock :name "nodecode-cache-warming"))

;;; --- the request ----------------------------------------------------------

(defun cache-control-ttls (octets)
  "The lifetime in seconds of every cache_control marker in the request
OCTETS, a JSON object's UTF-8 bytes: 3600 for a marker whose ttl is 1h, 300
for any other. A scan for the key, as JSON-PATH-AT scans: no parse."
  (let ((needle (sb-ext:string-to-octets "\"cache_control\"" :external-format :utf-8))
        (one-hour (sb-ext:string-to-octets "\"1h\"" :external-format :utf-8)))
    (loop for start = (search needle octets) then (search needle octets :start2 (1+ start))
          while start
          collect (let ((close (or (position (char-code #\}) octets :start start) (length octets))))
                    (if (search one-hour octets :start2 start :end2 close) 3600 300)))))

(defun top-level-value-span (octets key &aux (depth 0) (in-string nil) (escaped nil)
                                             (string-start 0) (pending nil))
  "=> (values START END): the bytes of KEY's value in the JSON object OCTETS,
KEY a member of the object itself, never of one nested in it; NIL when it
has none. The value must be a number, a literal or a string."
  (loop for i from 0 below (length octets)
        for char = (code-char (aref octets i))
        do (if in-string
               (cond (escaped (setf escaped nil))
                     ((char= char #\\) (setf escaped t))
                     ((char= char #\")
                      (setf in-string nil)
                      (when (= depth 1)
                        (setf pending (sb-ext:octets-to-string octets :start (1+ string-start) :end i
                                                                      :external-format :utf-8)))))
               (case char
                 (#\" (setf in-string t string-start i))
                 ((#\{ #\[) (incf depth) (setf pending nil))
                 ((#\} #\]) (decf depth) (setf pending nil))
                 (#\, (setf pending nil))
                 (#\: (when (and (= depth 1) (equal pending key))
                        (let* ((start (position-if-not (lambda (byte) (member byte '(32 9 10 13)))
                                                       octets :start (1+ i)))
                               (end (and start
                                         (if (= (aref octets start) (char-code #\"))
                                             (1+ (top-level-string-end octets (1+ start)))
                                             (or (position-if (lambda (byte)
                                                                (member byte (mapcar #'char-code
                                                                                     '(#\, #\} #\Space #\Tab #\Newline #\Return))))
                                                              octets :start start)
                                                 (length octets))))))
                          (return (values start end))))
                    (setf pending nil))))))

(defun top-level-string-end (octets start &aux (escaped nil))
  "The index of the quote that closes the JSON string whose text begins at START."
  (loop for i from start below (length octets)
        for byte = (aref octets i)
        do (cond (escaped (setf escaped nil))
                 ((= byte (char-code #\\)) (setf escaped t))
                 ((= byte (char-code #\")) (return i)))
        finally (return (length octets))))

(defun budget-thinking-p (octets)
  "Whether the request OCTETS think on a token budget. Such a request cannot
be repeated with one output token: the budget must stay under max_tokens,
and the cache is keyed on it."
  (multiple-value-bind (start end) (top-level-value-span octets "thinking")
    (declare (ignore end))
    ;; thinking's value is an object, which the span does not take: look for
    ;; its budget_tokens member instead, right after the key
    (and start
         (search (sb-ext:string-to-octets "\"budget_tokens\"" :external-format :utf-8) octets
                 :start2 start :end2 (or (position (char-code #\}) octets :start start) start))
         t)))

(defun warming-request (octets)
  "The request OCTETS with max_tokens 1, everything else byte for byte, or
NIL when a refresh cannot repeat it: no cache_control marker, no top-level
max_tokens, or thinking on a token budget."
  (when (and (cache-control-ttls octets) (not (budget-thinking-p octets)))
    (multiple-value-bind (start end) (top-level-value-span octets "max_tokens")
      (when (and start end)
        (concatenate 'nlk:octets (subseq octets 0 start)
                     (sb-ext:string-to-octets "1" :external-format :utf-8)
                     (subseq octets end))))))

;;; --- the policy -----------------------------------------------------------

(defun cache-warming-mode ()
  "The shared config's cache_warming.mode: \"idle\", \"streaming\" or \"off\"."
  (or (ignore-errors
       (getf (nlk:section-settings (nlk:find-section '("cache_warming"))
                                   (json-value (read-shared-config) :object "cache_warming"))
             :mode))
      "idle"))

(defun cache-warming-delay (ttl)
  "Seconds after a read that a cache entry living TTL seconds is refreshed, or
NIL when it lives too briefly to."
  (when (> ttl *cache-warming-margin-seconds*)
    (max 1 (floor (min (* ttl *cache-warming-delay-fraction*)
                       (- ttl *cache-warming-margin-seconds*))))))

(defun cache-warming-saving (price prompt-tokens phase)
  "The US dollars a refresh of a PROMPT-TOKENS prompt is expected to save in
PHASE, under PRICE (MODEL-PRICE): the write the next round avoids, weighed by
the chance it comes in time, less the refresh's own read and output token.
NIL without a price or a prompt."
  (when (and price (plusp (or prompt-tokens 0)))
    (let ((read (round-cost price :cached-input-tokens prompt-tokens))
          (write (round-cost price :cache-write-tokens prompt-tokens))
          (refresh (round-cost price :cached-input-tokens prompt-tokens :output-tokens 1)))
      (- (* (getf *cache-warming-continuation* phase) (max 0 (- write read))) refresh))))

;;; --- arming ---------------------------------------------------------------

(defun disarm-cache-warmer (session-id &optional warmer)
  "Take SESSION-ID's warmer down -- only WARMER, when given and still the
session's -- its timer unscheduled."
  (bt2:with-lock-held (*cache-warming-lock*)
    (let ((armed (gethash session-id *cache-warmers*)))
      (when (and armed (or (null warmer) (eq armed warmer)))
        (when (warmer-timer armed)
          (ignore-errors (sb-ext:unschedule-timer (warmer-timer armed))))
        (remhash session-id *cache-warmers*)))))

(defun schedule-cache-warmer (warmer seconds)
  "Fire WARMER SECONDS from now, on a thread of its own."
  (let ((timer (sb-ext:make-timer (lambda () (fire-cache-warmer warmer))
                                  :thread t :name "nodecode-cache-warming")))
    (setf (warmer-timer warmer) timer)
    (sb-ext:schedule-timer timer seconds)))

(defun arm-cache-warming (&key session-id turn lane context request-json usage provider model)
  "After a round's request went out and answered: arm SESSION-ID's warmer to
repeat REQUEST-JSON on LANE with CONTEXT before its cache entry expires,
replacing the one an earlier round armed. A request that placed no
breakpoint, or that cannot be repeated (WARMING-REQUEST), leaves the session
unarmed."
  (when session-id
    (let* ((body (and (typep request-json 'nlk:octets) usage lane (warming-request request-json)))
           (ttl (and body (reduce #'min (cache-control-ttls request-json))))
           (prompt (and usage (+ (or (provider-usage-input-tokens usage) 0)
                                 (or (provider-usage-cached-input-tokens usage) 0)
                                 (or (provider-usage-cache-write-tokens usage) 0))))
           (delay (and ttl (cache-warming-delay ttl))))
      (disarm-cache-warmer session-id)
      (when (and body delay (plusp prompt))
        (let ((now (get-universal-time)))
          (bt2:with-lock-held (*cache-warming-lock*)
            (let ((warmer (make-cache-warmer :session-id session-id :turn turn :lane lane :context context
                                             :body body :provider provider :model model :ttl ttl
                                             :prompt-tokens prompt :armed-at now :refreshed-at now)))
              (setf (gethash session-id *cache-warmers*) warmer)
              (schedule-cache-warmer warmer delay)
              warmer)))))))

(defmacro with-round-in-flight ((session-id) &body body)
  "Run BODY counted as one of SESSION-ID's rounds on the wire: a warmer that
comes due meanwhile waits, since the round reads the cached prefix itself."
  (let ((id (gensym "SESSION")))
    `(let ((,id ,session-id))
       (when ,id (bt2:with-lock-held (*cache-warming-lock*) (incf (gethash ,id *rounds-in-flight* 0))))
       (unwind-protect (progn ,@body)
         (when ,id
           (bt2:with-lock-held (*cache-warming-lock*)
             (when (<= (decf (gethash ,id *rounds-in-flight* 1)) 0)
               (remhash ,id *rounds-in-flight*))))))))

;;; --- firing ---------------------------------------------------------------

(defun cache-warming-phase (warmer)
  ":STREAMING while the turn that sent WARMER's round is still open, else :IDLE."
  (if (ignore-errors (turn-still-open-p (warmer-turn warmer))) :streaming :idle))

(defun cache-warming-verdict (warmer &optional (now (get-universal-time)))
  "=> (values ACTION PHASE SAVING): what WARMER does now -- :WARM, :WAIT (a
round is on the wire) or :STOP -- in PHASE, expecting SAVING dollars."
  (let* ((phase (cache-warming-phase warmer))
         (mode (cache-warming-mode))
         (saving (cache-warming-saving (ignore-errors (model-price (warmer-model warmer) (warmer-provider warmer)))
                                       (warmer-prompt-tokens warmer) phase)))
    (values
     (cond ((or (equal mode "off") (and (equal mode "streaming") (eq phase :idle))) :stop)
           ((> (- now (warmer-armed-at warmer)) (getf *cache-warming-max-age-seconds* phase)) :stop)
           ((plusp (gethash (warmer-session-id warmer) *rounds-in-flight* 0)) :wait)
           ((multiple-value-bind (provider model)
                (ignore-errors (nlk:session-model-selection (warmer-session-id warmer)))
              (and model (not (and (equal provider (warmer-provider warmer))
                                   (equal model (warmer-model warmer))))))
            :stop)
           ((or (null saving) (< saving *cache-warming-minimum-saving*)) :stop)
           (t :warm))
     phase saving)))

(defun refresh-prompt-cache (warmer)
  "Send WARMER's request once, max_tokens 1, on its own lane and context, and
record what it cost as a side call of its session, purpose cache_warming.
=> (values USAGE FINISH-REASON)."
  (multiple-value-bind (message usage finish-reason)
      (let ((*replay-request-json* (warmer-body warmer)))
        (funcall (warmer-lane warmer) (warmer-context warmer) :on-part nil))
    (declare (ignore message))
    (record-side-call "cache_warming" (warmer-session-id warmer)
                      (compiled-turn-context-provider-config (warmer-context warmer))
                      usage finish-reason nil)
    (values usage finish-reason)))

(defun fire-cache-warmer (warmer)
  "WARMER's timer: refresh its session's prompt cache when that pays, then
schedule the next refresh; stop when it no longer does."
  (handler-case
      (when (eq warmer (gethash (warmer-session-id warmer) *cache-warmers*))
        (let ((action (cache-warming-verdict warmer)))
          (ecase action
            (:stop (disarm-cache-warmer (warmer-session-id warmer) warmer))
            ;; the round on the wire reads the prefix; look again a lifetime's
            ;; refresh later, unless that round arms a warmer of its own first
            (:wait (schedule-cache-warmer warmer (cache-warming-delay (warmer-ttl warmer))))
            (:warm
             (progn
               (refresh-prompt-cache warmer)
               (incf (warmer-refreshes warmer))
               (setf (warmer-refreshed-at warmer) (get-universal-time))
               (bt2:with-lock-held (*cache-warming-lock*)
                 (when (eq warmer (gethash (warmer-session-id warmer) *cache-warmers*))
                   (schedule-cache-warmer warmer (cache-warming-delay (warmer-ttl warmer))))))))))
    ;; a refresh that fails is not retried: the next real round arms again
    (error () (disarm-cache-warmer (warmer-session-id warmer) warmer))))
