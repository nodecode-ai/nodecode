;;;; observe.lisp --- where the time and money went: one session's turns,
;;;; each model call and step on the turn's own clock with the flags worth a
;;;; look (the web page's Run view), the bytes one call sent (its inspector),
;;;; and the usage across every session over a range (its Usage view).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Projections, never sources: every number is a fact the turn loop already
;;;; recorded -- turn.usage (tokens, price), turn.provider_request (the wait
;;;; for the first token, the stream), turn.tool_result (a step's time), the
;;;; retries, the model switches, the evictions and each turn's own start and
;;;; end -- so both rebuild from the log alone, and a number no fact carries is
;;;; absent from the answer: NIL is not zero. What counts as worth a look is
;;;; decided here and nowhere else; the page renders what it is sent.
;;;;
;;;; A call's start is placed back from its request fact, recorded as the
;;;; stream ended, by its wait and its stream; a step's from its result, by
;;;; its measured time. The usage read never hydrates a fact: SQLite pulls the few fields it sums
;;;; out of each payload, so a month of rounds is one scan.
;;;;
;;;; Retention: persisted-history; a call's bytes are debug-only (bodies.lisp).

(in-package #:nodecode.evolved)

(nlk:access (used nlk:turn-usage-event) (sent nlk:provider-request-event)
            (switch nlk:provider-fallback-event) (result nlk:tool-result-event)
            (call nlk:tool-call-event) (retry nlk:provider-retry-event))

(defmacro loop-vector (&body clauses)
  "LOOP over CLAUSES, what it answers as a vector: the JSON array a list of rows is sent as."
  `(coerce (loop ,@clauses) 'vector))

(defparameter +timeline-kinds+
  (list nlk::+kind-turn-started+ nlk::+kind-turn-completed+ nlk::+kind-turn-failed+
        nlk::+kind-turn-cancelled+ nlk::+kind-turn-usage+ nlk::+kind-provider-request+
        nlk::+kind-tool-call-started+ nlk::+kind-tool-result+ nlk::+kind-provider-retry+
        nlk::+kind-provider-fallback+ nlk::+kind-context-evicted+
        nlk::+kind-turn-context-evicted+)
  "The facts a session's timeline is folded from.")

(defstruct (timeline-turn (:conc-name tt-) (:copier nil))
  "One turn as the fold holds it."
  id prompt started-at started ended status detail evicted-p
  ;; NLK:TURN-ORIGIN: what began a turn no one asked, NIL for one someone did.
  origin
  (calls '()) (steps '()) (retries '()) (fallbacks '())
  ;; The request fact waiting for its round's usage, as (AT . EVENT).
  pending
  ;; What voided attempts billed: spent, though no round came of it.
  (voided-usd 0)
  (tool-calls (make-hash-table :test #'equal)))

(defun fold-timeline (events &aux (turns '()) (by-id (make-hash-table :test #'equal)))
  "EVENTS, a session's +TIMELINE-KINDS+ facts oldest first, as its
TIMELINE-TURNs, oldest first."
  (dolist (event events (nreverse turns))
    (let ((at (iso-8601-ms (nlk:event-occurred-at event)))
          ;; A fact of no turn (a session-wide eviction) is the open turn's.
          (turn (or (gethash (nlk:event-turn-id event) by-id) (first turns))))
      (when (typep event 'nlk:turn-started)
        (let ((new (make-timeline-turn :id (nlk:event-turn-id event)
                                       :prompt (nlk:turn-started-input event)
                                       :origin (nlk:turn-origin (nlk:turn-started-wake event) nil)
                                       :started-at (nlk:event-occurred-at event)
                                       :started at :status "running")))
          (setf (gethash (tt-id new) by-id) new)
          (push new turns)))
      (when turn
        (typecase event
          (nlk:turn-completed
           (setf (tt-ended turn) at (tt-status turn) "completed")
           ;; A recorded exchange says who recorded it on its completion.
           (unless (tt-origin turn)
             (setf (tt-origin turn) (nlk:turn-origin nil (nlk:turn-completed-recorder event))))
           ;; A recorded exchange is written whole once it settled: its
           ;; recorder's elapsed time is when the work behind it began.
           (when-let (elapsed (and at (nlk:turn-completed-elapsed-ms event)))
             (setf (tt-started turn) (- at elapsed))))
          (nlk:turn-failed (setf (tt-ended turn) at (tt-status turn) "failed"
                                 (tt-detail turn) (nlk:turn-failed-detail event)))
          (nlk:turn-cancelled (setf (tt-ended turn) at (tt-status turn) "cancelled"))
          (nlk:provider-request-event (setf (tt-pending turn) (cons at event)))
          (nlk:turn-usage-event
           (if (nlk:turn-usage-event-voided event)
               (when (nlk:turn-usage-event-cost-known event)
                 (incf (tt-voided-usd turn) (nlk:turn-usage-event-cost-usd event)))
               (destructuring-bind (&optional request-at . request) (tt-pending turn)
                 (setf (tt-pending turn) nil)
                 (push (list :usage event :request request :end (or request-at at))
                       (tt-calls turn)))))
          (nlk:tool-call-event
           (setf (gethash (nlk:tool-call-event-call-id event) (tt-tool-calls turn)) event))
          (nlk:tool-result-event (push (cons at event) (tt-steps turn)))
          (nlk:provider-retry-event (push (cons at event) (tt-retries turn)))
          (nlk:provider-fallback-event (push (cons at event) (tt-fallbacks turn)))
          ((or nlk:context-evicted-event nlk:turn-context-evicted-event)
           (setf (tt-evicted-p turn) t)))))))

(defun median-integer (values)
  "The middle of VALUES, integers, or NIL for none."
  (when values
    (let ((sorted (sort (copy-list values) #'<)))
      (nth (floor (length sorted) 2) sorted))))

(defun timeline-call-flags (used sent usual-wait first-p)
  "What is worth a look about one model call: \"slow_start\", \"cache_miss\"."
  (let* ((wait (and sent sent.ttft-ms))
         (cached (or used.cached-input-tokens 0))
         (fresh (+ (or used.input-tokens 0) (or used.cache-write-tokens 0))))
    ;; A slow start waited three times the session's usual wait, and at least ten
    ;; seconds; a cache miss sent a prompt less than half of which was reused,
    ;; once the part paid in full (8192 tokens) is worth a line.
    (append
     (and wait usual-wait
          (>= wait (max 10000 (* 3 usual-wait)))
          (list "slow_start"))
     ;; A session's first call has nothing to reuse yet.
     (and (not first-p) (>= fresh 8192)
          (< cached (* 1/2 (+ cached fresh)))
          (list "cache_miss")))))

(defun timeline-call-json (call turn-start usual-wait first-p)
  "One model call of a turn: its place on the turn's clock, what it sent and
wrote, what it cost, and its flags."
  (destructuring-bind (&key ((:usage used)) ((:request sent)) end) call
    (let* ((wait (and sent sent.ttft-ms))
           (stream (and sent sent.stream-duration-ms))
           (start (and end (- end (or wait 0) (or stream 0))))
           (diverged (and sent sent.prefix-diverged-in))
           ;; The message index a path like "input[17]" names.
           (bracket (and (stringp diverged) (position #\[ diverged))))
      (json-object
       :opt "at_ms" (and start turn-start (max 0 (- start turn-start)))
       :opt "wait_ms" wait
       :opt "stream_ms" stream
       "model" used.model
       "provider" used.provider
       :opt "effort" used.reasoning-effort
       :opt "input" used.input-tokens
       :opt "cached" used.cached-input-tokens
       :opt "cache_write" used.cache-write-tokens
       :opt "output" used.output-tokens
       :opt "reasoning" used.reasoning-tokens
       :opt "cost_usd" (and used.cost-known used.cost-usd)
       :opt "context" (and sent sent.estimated-context-tokens)
       :opt "messages" (and sent sent.provider-message-count)
       :opt "diverged_in" diverged
       :opt "diverged_at" (and bracket (parse-integer diverged :start (1+ bracket) :junk-allowed t))
       :opt "attempts" (and sent sent.transport-attempts)
       ;; What the request read (NLK:KEPT-REQUEST-BODY) finds the call's bytes by.
       :opt "round" (and sent sent.provider-round)
       :opt "finish" used.finish-reason
       :when used.estimated "estimated" t
       "flags" (coerce (timeline-call-flags used sent usual-wait first-p) 'vector)))))

(defun timeline-step-json (step turn turn-start)
  "One step of a turn: its place on the turn's clock, its time and its title."
  (destructuring-bind (at . result) step
    (let* ((ms result.duration-ms)
           (call (gethash result.call-id (tt-tool-calls turn)))
           (title (and call
                       (with-handlers ((error () nil))
                         (when-let (activity (tool-arguments-activity call.tool-name call.arguments
                                                                      :metadata result.metadata))
                           (activity-title activity))))))
      (json-object
       :opt "at_ms" (and at ms turn-start (max 0 (- at ms turn-start)))
       :opt "ms" ms
       "tool" result.tool-name
       "call_id" result.call-id
       :opt "title" title))))

(defun prompt-line (text &optional (limit 200))
  "TEXT on one line, runs of whitespace one space, at most LIMIT characters."
  (clip (one-line text) limit :ellipsis ""))

(defun sum-known (values)
  "The sum of VALUES' numbers, or NIL when none is one."
  (when-let (numbers (remove-if-not #'numberp values))
    (reduce #'+ numbers)))

(defun session-timeline-json (session-id &aux (now (iso-8601-ms (nlk::iso-now))))
  "SESSION-ID's turns as where their time and money went, with the totals."
  ;; A reflection's recap ran nothing in this session -- its work ran in the
  ;; session the reflection was forked into -- so it is no turn here. An exit's
  ;; wake did run a round of its own, and stays: named by its ORIGIN, numbered by
  ;; no one. N places a turn in the read (the address of it); NUMBER counts the
  ;; asks, the number the operator knows a turn by.
  (let* ((turns (remove "reflection" (fold-timeline (nlk:events :session-id session-id :kind +timeline-kinds+))
                        :key #'tt-origin :test #'equal))
         (calls (loop for turn in turns append (reverse (tt-calls turn))))
         ;; The middle wait for the first token over the calls, or NIL.
         (usual-wait (median-integer (loop for call in calls
                                           for sent = (getf call :request)
                                           for wait = (and sent sent.ttft-ms)
                                           when wait collect wait)))
         (used (getf (car (last calls)) :usage))
         (turn-objects
           (loop with asked = 0
                 for turn in turns
                 for n from 1
                 for number = (and (null (tt-origin turn)) (incf asked))
                 for start = (tt-started turn)
                 for turn-calls = (reverse (tt-calls turn))
                 for call-objects = (loop for call in turn-calls
                                          collect (timeline-call-json call start usual-wait
                                                                      (eq call (first calls))))
                 for step-objects = (loop for step in (reverse (tt-steps turn))
                                          collect (timeline-step-json step turn start))
                 for costs = (mapcar (lambda (call) (gethash "cost_usd" call)) call-objects)
                 for flags = (remove-duplicates
                              (append (loop for call in call-objects
                                            append (coerce (gethash "flags" call) 'list))
                                      (and (tt-retries turn) (list "retried"))
                                      (and (tt-fallbacks turn) (list "switched_model"))
                                      (and (tt-evicted-p turn) (list "memory_trimmed"))
                                      (and (equal "failed" (tt-status turn)) (list "failed")))
                              :test #'string= :from-end t)
                 collect
                 (json-object
                  "turn_id" (tt-id turn)
                  "n" n
                  :opt "number" number
                  :opt "origin" (tt-origin turn)
                  "prompt" (prompt-line (tt-prompt turn))
                  "status" (tt-status turn)
                  :opt "detail" (tt-detail turn)
                  :opt "started_at" (tt-started-at turn)
                  :opt "ms" (and start (- (or (tt-ended turn) now) start))
                  :opt "cost_usd" (sum-known (cons (and (plusp (tt-voided-usd turn))
                                                        (tt-voided-usd turn))
                                                   costs))
                  :when turn-calls "cost_complete" (every #'numberp costs)
                  "calls" (coerce call-objects 'vector)
                  "steps" (coerce step-objects 'vector)
                  "retries" (loop-vector for (at . retry) in (reverse (tt-retries turn))
                                         collect (json-object
                                                  :opt "at_ms" (and at start (max 0 (- at start)))
                                                  :opt "status" retry.status "detail" retry.detail
                                                  "delay_s" retry.delay-seconds))
                  "fallbacks" (loop-vector for (at . switch) in (reverse (tt-fallbacks turn))
                                           collect (json-object
                                                    :opt "at_ms" (and at start (max 0 (- at start)))
                                                    "from" (format nil "~a/~a" switch.from-provider
                                                                   switch.from-model)
                                                    "to" (format nil "~a/~a" switch.to-provider
                                                                 switch.to-model)
                                                    "reason" switch.reason))
                  "flags" (coerce flags 'vector)))))
    (flet ((total (key &optional (objects (loop for turn in turn-objects
                                                append (coerce (gethash "calls" turn) 'list))))
             (sum-known (mapcar (lambda (object) (gethash key object)) objects))))
      (json-object
       "session_id" session-id
       "turns" (coerce turn-objects 'vector)
       "totals"
       (json-object
        "turns" (count-if-not #'tt-origin turns)
        :opt "ms" (total "ms" turn-objects)
        :opt "cost_usd" (total "cost_usd" turn-objects)
        "cost_complete" (every (lambda (turn) (gethash "cost_complete" turn t)) turn-objects)
        "calls" (length calls)
        "steps" (loop for turn in turn-objects sum (length (gethash "steps" turn)))
        :opt "input" (total "input")
        :opt "cached" (total "cached")
        :opt "cache_write" (total "cache_write")
        :opt "output" (total "output")
        :opt "reasoning" (total "reasoning")
        :opt "wait_ms" (total "wait_ms")
        :opt "stream_ms" (total "stream_ms")
        :opt "step_ms" (total "ms" (loop for turn in turn-objects
                                         append (coerce (gethash "steps" turn) 'list)))
        :opt "usual_wait_ms" usual-wait
        :opt "peak_prompt" (and calls
                                (loop for call in calls
                                      for usage = (getf call :usage)
                                      maximize (+ (or (nlk:turn-usage-event-input-tokens usage) 0)
                                                  (or (nlk:turn-usage-event-cached-input-tokens usage) 0)
                                                  (or (nlk:turn-usage-event-cache-write-tokens usage) 0))))
        :opt "window" (and used (model-capability-context-tokens
                                 (resolve-model-capability used.model used.provider))))))))

(define-gateway-route handle-gateway-timeline-http (env session-id)
  "GET /api/gateway/sessions/<id>/timeline — where the session's time and
money went, turn by turn (SESSION-TIMELINE-JSON)."
  (:route :get "timeline" "session timeline")
  (session-timeline-json session-id))

;;; --- what a call sent: its request body, byte for byte -----------------------
;;; The bytes as they went out (NLK:KEPT-REQUEST-BODY), and beside them, in
;;; headers, how much the request kept before them already sent and where
;;; they first differ: the request fact's own diagnosis (REQUEST-PREFIX-
;;; DIAGNOSIS), which a session's first request lacks -- it had nothing
;;; before it -- and the kept chain does not.

(define-gateway-route handle-gateway-request-http (env session-id)
  "GET /api/gateway/sessions/<id>/request?turn=<turn_id>&round=<n> — the
exact body that model call sent, application/json, byte for byte; beside it
X-Shared-Bytes and X-Diverged-In, what it shares with the request kept before
it and where they part, and X-Previous, that request as `SESSION TURN ROUND'.
404 not_kept when it was not kept: recorded before bodies were, or dropped
past request_bodies.keep_mb."
  (:route :get "request" "request body")
  (let* ((query (gateway-query-parameters env))
         (turn (cdr (assoc "turn" query :test #'string=)))
         (round (parse-integer (or (cdr (assoc "round" query :test #'string=)) "") :junk-allowed t)))
    (multiple-value-bind (body previous from) (and turn round (nlk:kept-request-body session-id turn round))
      (if body
          (multiple-value-bind (shared diverged) (request-prefix-diagnosis previous body)
            (list 200 (list* :content-type "application/json" :content-length (length body)
                             :cache-control "private, no-cache"
                             (append (and shared (list :x-shared-bytes (princ-to-string shared)
                                                       :x-diverged-in diverged))
                                     (and from (list :x-previous (format nil "~{~a~^ ~}" from)))))
                  body))
          (gateway-error-response 404 "not_kept" "that call's request body was not kept")))))

;;; --- usage: every session over a range ---------------------------------------

(defun payload-columns (&rest keys)
  "The SQL column reading each of KEYS out of a fact's payload, a hyphenated key quoted."
  (loop for key in keys
        collect (format nil "json_extract(payload, '$.~:[~a~;\"~a\"~]')" (find #\- key) key)))

(defparameter +usage-columns+
  (list* "session_id" "occurred_at"
         (payload-columns "model" "provider" "input-tokens" "cached-input-tokens"
                          "cache-write-tokens" "output-tokens" "cost_known" "cost_usd" "voided"))
  "What a usage read takes out of each turn.usage fact: its session, clock, model,
provider, counts, price and whether it was voided.")

(defun usage-range-start (range &optional (now (get-universal-time)))
  "The universal time RANGE starts at: local midnight of the first of its
days, today counting as one -- \"today\", \"30d\", \"all\" (NIL), else seven."
  (when-let (days (dispatch range equal ("today" 1) ("30d" 30) ("all" nil) (t 7)))
    (multiple-value-bind (second minute hour) (decode-universal-time now)
      (- now second (* 60 minute) (* 3600 hour) (* 86400 (1- days))))))

(defun local-day (universal)
  "UNIVERSAL's local date as YYYY-MM-DD."
  (bind (((_ _ _ day month year) (decode-universal-time universal)))
    (format nil "~d-~2,'0d-~2,'0d" year month day)))

(defstruct (usage-sum (:conc-name us-) (:copier nil))
  "What a group of rounds adds up to."
  (calls 0) (cost 0) (priced 0) (input 0) (cached 0) (write 0) (output 0) (ms 0)
  ;; The turns that ended, and the ones of them that failed.
  (turns 0) (failed 0)
  ;; How many sessions the rounds were charged to, and the status each of a
  ;; model's failed attempts met.
  (sessions 0) (statuses '()))

(nlk:access (sum usage-sum))

(defun usage-sum-json (sum &rest more &aux (prompt (+ sum.input sum.cached sum.write)))
  "SUM's numbers, then MORE's keys and values."
  (let ((object (json-object
                 "calls" sum.calls :opt "cost_usd" (and (plusp sum.priced) sum.cost)
                 "unpriced" (- sum.calls sum.priced) "sessions" sum.sessions
                 "input" sum.input "cached" sum.cached "cache_write" sum.write "output" sum.output
                 :opt "cached_share" (and (plusp prompt) (/ sum.cached prompt 1d0))
                 :when (plusp sum.ms) "ms" sum.ms
                 :when (plusp sum.turns) "turns" sum.turns
                 :when (plusp sum.turns) "failed_turns" sum.failed
                 :when (plusp sum.turns) "error_rate" (/ sum.failed sum.turns 1d0))))
    (apply #'copy-json-object object more)))

(defun descending (&rest keys)
  "A sort predicate: the greater by the first of KEYS first, the next breaking a tie."
  (lambda (a b)
    (loop for key in keys
          for x = (funcall key a) and y = (funcall key b)
          unless (= x y) return (> x y))))

(defun ranked (table &optional limit)
  "TABLE's (NAME . USAGE-SUM) entries, costliest first, then busiest; the
first LIMIT of them when LIMIT is given."
  (let ((entries (sort (nreverse (alexandria:hash-table-alist table))
                       (descending #'us-cost #'us-calls) :key #'cdr)))
    (if (and limit (> (length entries) limit)) (subseq entries 0 limit) entries)))

(defun asked-where (where &key row literal)
  "WHERE narrowed to operator turns, excluding note sessions and reflection
recaps. ROW scopes the exclusions to a non-null event's identifiers;
LITERAL emits the same policy without parameters, for durable triggers."
  ;; One policy owns both live reads and the rebuild/incremental projection.
  ;; Scoped exclusions seek the two identifiers instead of copying every
  ;; reflection's identifier into a temporary set on each new usage fact.
  (flet ((value (text) (if literal (format nil "'~a'" text) "?")))
    (let ((prefix (if row (format nil "~a." row) "")))
      (cons (format nil "~@[~a and ~]~asession_id not in
                          (select session_id from session_directory where source = ~a~
                           ~@[ and session_id = ~a.session_id~])
                        and ~aturn_id not in
                          (select r.turn_id from events r where r.kind = ~a
                            and r.turn_id is not null
                            and json_extract(r.payload, '$.recorder') = 'reflection'~
                            ~@[ and r.turn_id = ~a.turn_id~])"
                    (first where) prefix (value nlk::+note-source+) row
                    prefix (value nlk::+kind-turn-completed+) row)
            (append (rest where)
                    (unless literal (list nlk::+note-source+ nlk::+kind-turn-completed+)))))))

(defun model-history-fill-sql (&optional scope)
  "Upsert eligible retained usage facts in SCOPE (or the log) into the
model history. One row per provider/model/turn/session; rounds only advance
recency, and ASKED-WHERE owns the exclusions."
  (format nil "insert into model_history (provider, model, turn_id, session_id, log_position)
      select json_extract(payload, '$.provider'), json_extract(payload, '$.model'),
             turn_id, session_id, max(log_position)
        from events where ~a
       group by 1, 2, 3, 4
      on conflict (provider, model, turn_id, session_id) do update
         set log_position = max(model_history.log_position, excluded.log_position)"
          (first (asked-where
                  (list (format nil "~@[~a and ~]kind = '~a' and turn_id is not null
                          and session_id is not null
                          and coalesce(json_extract(payload, '$.voided'), 0) = 0
                          and length(json_extract(payload, '$.provider')) > 0
                          and length(json_extract(payload, '$.model')) > 0"
                                scope nlk::+kind-turn-usage+))
                  :row (and scope "events") :literal t))))

(defparameter *model-history-schema*
  (list
   (list "table" "model_history"
         "create table model_history (
            provider not null, model not null, turn_id text not null,
            session_id text not null, log_position integer not null,
            primary key (provider, model, turn_id, session_id)) without rowid")
   (list "index" "model_history_by_turn"
         "create index model_history_by_turn on model_history (turn_id)")
   (list "index" "model_history_by_session"
         "create index model_history_by_session on model_history (session_id)")
   (list "trigger" "model_history_follows_usage"
         (format nil "create trigger model_history_follows_usage
            after insert on events when new.kind = '~a'
            begin ~a; end"
                 nlk::+kind-turn-usage+ (model-history-fill-sql "log_position = new.log_position")))
   (list "trigger" "model_history_follows_completion"
         (format nil "create trigger model_history_follows_completion
            after insert on events when new.kind = '~a'
            begin
              delete from model_history where turn_id = new.turn_id;
              ~a;
            end"
                 nlk::+kind-turn-completed+ (model-history-fill-sql "turn_id = new.turn_id")))
   (list "trigger" "model_history_follows_source"
         (format nil "create trigger model_history_follows_source
            after update of source on session_directory when new.source is not old.source
            begin
              delete from model_history where session_id = new.session_id;
              ~a;
            end"
                 (model-history-fill-sql "session_id = new.session_id"))))
  "Rebuildable persisted-history, never authority. Triggers keep it inside the
fact's transaction, including late reflection/source facts and rollback.")

(defun projection-current-p (handle schema)
  "True when every entry of SCHEMA stands in HANDLE's store as SCHEMA says it."
  (every (lambda (entry)
           (destructuring-bind (type name sql) entry
             (equalp sql (sqlite:execute-single
                          handle "select sql from sqlite_master where type = ? and name = ?"
                          type name))))
         schema))

(defun drop-stale-projection (handle schema)
  "Drop SCHEMA's entries, the last first, unless every one stands as SCHEMA says it."
  (unless (projection-current-p handle schema)
    (dolist (entry (reverse schema))
      (sqlite:execute-non-query handle (format nil "drop ~a if exists ~a" (first entry) (second entry))))))

(defun install-projection (handle schema)
  "Create SCHEMA's entries unless its first, a table, stands: true when it did."
  ;; DROP-STALE-PROJECTION ran first in the same transaction, so the entries
  ;; stand all together or not at all.
  (unless (sqlite:execute-single handle "select 1 from sqlite_master where type = ? and name = ?"
                                 (first (first schema)) (second (first schema)))
    (dolist (entry schema)
      (sqlite:execute-non-query handle (third entry)))
    t))

(defun prepare-model-history (handle)
  "Discard a stale model projection shape before its schema is applied."
  ;; EXECUTE-SINGLE finalizes before DDL. Compare every trigger, not just the
  ;; table: IF NOT EXISTS would preserve stale eligibility or recency policy.
  (drop-stale-projection handle *model-history-schema*)
  ;; The displaced full-usage covering index has no remaining reader.
  (sqlite:execute-non-query handle "drop index if exists events_usage_by_model"))

(defun open-model-history ()
  "Install the model projection and derive it once from retained facts."
  (nlk::with-store-transaction (handle)
    ;; Recheck after the kernel schema: replacing events/the directory also
    ;; drops their triggers, so a surviving derived table must be rebuilt.
    (prepare-model-history handle)
    (when (install-projection handle *model-history-schema*)
      (sqlite:execute-non-query handle (model-history-fill-sql)))))

(pushnew 'prepare-model-history nlk::*schema-prepare-hooks*)
(pushnew 'open-model-history nlk::*store-open-hooks*)

(defun frequent-models-json ()
  "Past models ranked by distinct operator turns, newest usage first on a
 tie. Retained facts own history; the transactional projection avoids
 reading every round on each uncached picker open."
  (let ((rows (nlk::with-store-handle (handle)
                (nlk::sql-rows handle
                               "select provider, model, count(distinct turn_id), max(log_position)
                                  from model_history group by provider, model"))))
    (loop-vector for (provider model turns position)
                 in (sort rows (descending #'third #'fourth))
                 collect (json-object "provider" provider "model" model "turns" turns))))

;;; --- the usage projection -------------------------------------------------
;;; What /usage and the Dashboard add up, kept by triggers inside each fact's
;;; own transaction and derived again whenever its shape changes: rebuildable,
;;; never authority. Read straight off the log, a week's rounds cost a JSON
;;; parse of every usage fact the store holds (the log has no index on its
;;; clock) and a Lisp list per round: 2.4 s for 7 days, 5.4 s for all, and the
;;; four tabs /usage reads at once took 7.5 s to show the first (asked
;;; 2026-10-26: "it takes forever", then "sub 1s target for all").
;;;
;;; Rounds are summed as they land, per UTC quarter hour -- every zone's
;;; offset is a whole number of them, so each sum falls in one local day
;;; wherever the store is read -- and per session, model, provider, whether
;;; voided, whether priced, and prompt band: how many of usage_tier_sizes the
;;; round's prompt is over, so one band's rounds bill at one tier of each
;;; price. The sizes are every tier size the models in the log price at,
;;; grown (and the sums derived again) when a read meets a model whose price
;;; names one more. Requests stay one row each, for exact waits: an index
;;; walks them in order of wait.

(defun json-number-sql (key &optional (default "null"))
  "The SQL reading KEY out of a fact's payload when it is a number, else DEFAULT."
  (let ((value (first (payload-columns key))))
    (format nil "case when typeof(~a) in ('integer', 'real') then ~a else ~a end" value value default)))

(defun usage-rollup-fill-sql (source)
  "Add the turn.usage facts SOURCE names -- a FROM-clause term with columns
OCCURRED_AT, SESSION_ID and PAYLOAD -- into usage_rollup, each number as
USAGE-JSON counts it: a token count that is no number counts 0, a cost only
when it is known."
  (format nil "insert into usage_rollup
                 (bucket, session_id, model, provider, voided, priced, band,
                  rounds, input, cached, write, output, cost)
      select bucket, session_id, model, provider, voided, cost is not null,
             (select count(*) from usage_tier_sizes where size < input + cached + write),
             count(*), sum(input), sum(cached), sum(write), sum(output), sum(cost)
        from (select substr(occurred_at, 1, 14)
                       || printf('%02d', cast(substr(occurred_at, 15, 2) as integer) / 15 * 15) bucket,
                     coalesce(session_id, '') session_id,
                     coalesce(~a, '') model, coalesce(~a, '') provider,
                     ~a input, ~a cached, ~a write, ~a output,
                     case when json_extract(payload, '$.cost_known') = 1 then ~a end cost,
                     coalesce(json_extract(payload, '$.voided') = 1, 0) voided
                from ~a)
       where true group by 1, 2, 3, 4, 5, 6, 7
      on conflict (bucket, session_id, model, provider, voided, priced, band) do update
         set rounds = rounds + excluded.rounds, input = input + excluded.input,
             cached = cached + excluded.cached, write = write + excluded.write,
             output = output + excluded.output, cost = cost + excluded.cost"
          (first (payload-columns "model")) (first (payload-columns "provider"))
          (json-number-sql "input-tokens" "0") (json-number-sql "cached-input-tokens" "0")
          (json-number-sql "cache-write-tokens" "0") (json-number-sql "output-tokens" "0")
          (json-number-sql "cost_usd") source))

(defparameter +turn-end-kinds+
  (list nlk::+kind-turn-completed+ nlk::+kind-turn-failed+ nlk::+kind-turn-cancelled+)
  "The facts a turn ends on.")

(defun turn-starts-fill-sql (source)
  "Record the turn starts SOURCE names -- a FROM-clause term with columns
TURN_ID, SESSION_ID and OCCURRED_AT -- in turn_spans, the first start kept."
  (format nil "insert into turn_spans (turn_id, session_id, started_at)
      select turn_id, session_id, occurred_at from ~a where true
      on conflict (turn_id) do update
         set started_at = coalesce(started_at, excluded.started_at)" source))

(defun turn-ends-fill-sql (source)
  "Record the turn ends SOURCE names -- a FROM-clause term with columns
TURN_ID, SESSION_ID, OCCURRED_AT, KIND and PAYLOAD -- in turn_spans: when,
how, and whether the turn was a reflection (ASKED-WHERE's policy)."
  (format nil "insert into turn_spans (turn_id, session_id, ended_at, kind, reflection)
      select turn_id, session_id, occurred_at, kind,
             coalesce(kind = '~a' and json_extract(payload, '$.recorder') = 'reflection', 0)
        from ~a where true
      on conflict (turn_id) do update
         set ended_at = excluded.ended_at, kind = excluded.kind, reflection = excluded.reflection"
          nlk::+kind-turn-completed+ source))

(defun request-timings-fill-sql (&optional scope)
  "Insert SCOPE's (or the log's) provider.request facts into request_timings:
each request's wait for its first token and the time it spent writing."
  (format nil "insert or replace into request_timings
                 (occurred_at, log_position, model, provider, ttft, streamed)
      select occurred_at, log_position, ~{~a~^, ~}, ~a, ~a
        from events where kind = '~a'~@[ and ~a~]"
          (payload-columns "model" "provider") (json-number-sql "ttft-ms")
          (json-number-sql "stream-duration-ms") nlk::+kind-provider-request+ scope))

(defparameter *usage-rounds-schema*
  (list
   (list "table" "usage_tier_sizes"
         "create table usage_tier_sizes (size integer primary key)")
   (list "table" "usage_rollup"
         "create table usage_rollup (
            bucket text not null, session_id text not null, model text not null,
            provider text not null, voided integer not null, priced integer not null,
            band integer not null, rounds integer not null, input integer not null,
            cached integer not null, write integer not null, output integer not null, cost,
            primary key (bucket, session_id, model, provider, voided, priced, band)) without rowid")
   (list "table" "request_timings"
         "create table request_timings (
            occurred_at text not null, log_position integer not null,
            model, provider, ttft, streamed,
            primary key (occurred_at, log_position)) without rowid")
   (list "index" "request_timings_by_wait"
         "create index request_timings_by_wait on request_timings (ttft) where ttft is not null")
   (list "index" "request_timings_by_model"
         "create index request_timings_by_model on request_timings (model, provider, ttft, streamed)")
   (list "trigger" "usage_rollup_follows_the_log"
         (format nil "create trigger usage_rollup_follows_the_log
            after insert on events when new.kind = '~a'
            begin ~a; end"
                 nlk::+kind-turn-usage+
                 (usage-rollup-fill-sql "(select new.occurred_at occurred_at, new.session_id session_id,
                                                 new.payload payload)")))
   (list "trigger" "request_timings_follow_the_log"
         (format nil "create trigger request_timings_follow_the_log
            after insert on events when new.kind = '~a'
            begin ~a; end"
                 nlk::+kind-provider-request+
                 (request-timings-fill-sql "log_position = new.log_position")))
   ;; Each turn's start and end, and whether it was a reflection: what the
   ;; folder table's turns, failures and time and the totals' counts read.
   (list "table" "turn_spans"
         "create table turn_spans (
            turn_id text primary key, session_id, started_at, ended_at, kind,
            reflection integer not null default 0) without rowid")
   (list "trigger" "turn_spans_follow_starts"
         (format nil "create trigger turn_spans_follow_starts
            after insert on events when new.kind = '~a' and new.turn_id is not null
            begin ~a; end"
                 nlk::+kind-turn-started+
                 (turn-starts-fill-sql "(select new.turn_id turn_id, new.session_id session_id,
                                                new.occurred_at occurred_at)")))
   (list "trigger" "turn_spans_follow_ends"
         (format nil "create trigger turn_spans_follow_ends
            after insert on events when new.kind in (~{'~a'~^, ~}) and new.turn_id is not null
            begin ~a; end"
                 +turn-end-kinds+
                 (turn-ends-fill-sql "(select new.turn_id turn_id, new.session_id session_id,
                                              new.occurred_at occurred_at, new.kind kind,
                                              new.payload payload)"))))
  "The usage projection.")

(defun prepare-usage-rounds (handle)
  "Discard a usage projection of another shape before the schema is applied."
  (drop-stale-projection handle *usage-rounds-schema*))

(defun tier-sizes-of (pairs &optional (prices (make-hash-table :test #'equal)))
  "Every tier size the prices of PAIRS, (MODEL PROVIDER) lists, name,
ascending; PRICES memoizes MODEL-PRICE by (MODEL . PROVIDER)."
  (sort (remove-duplicates
         (loop for (model provider) in pairs
               append (price-tier-sizes (alexandria:ensure-gethash (cons model provider) prices
                                                                   (model-price model provider)))))
        #'<))

(defun derive-usage-rollup (handle sizes)
  "Sum every round in the log into usage_rollup again, banded by SIZES."
  (sqlite:execute-non-query handle "delete from usage_tier_sizes")
  (dolist (size sizes)
    (sqlite:execute-non-query handle "insert into usage_tier_sizes (size) values (?)" size))
  (sqlite:execute-non-query handle "delete from usage_rollup")
  (sqlite:execute-non-query handle (usage-rollup-fill-sql
                                    (format nil "(select occurred_at, session_id, payload from events
                                                   where kind = '~a')"
                                            nlk::+kind-turn-usage+))))

(defun open-usage-rounds ()
  "Install the usage projection and derive it once from the log."
  (nlk::with-store-transaction (handle)
    ;; Rechecked after the kernel schema, as OPEN-MODEL-HISTORY is: a
    ;; replaced events table took the triggers with it.
    (prepare-usage-rounds handle)
    (when (install-projection handle *usage-rounds-schema*)
      (derive-usage-rollup handle (tier-sizes-of
                                   (nlk::sql-rows handle (format nil "select distinct ~{~a~^, ~} from events
                                                                       where kind = '~a'"
                                                                 (payload-columns "model" "provider")
                                                                 nlk::+kind-turn-usage+))))
      (sqlite:execute-non-query handle (request-timings-fill-sql))
      (sqlite:execute-non-query handle (turn-starts-fill-sql
                                        (format nil "(select turn_id, session_id, occurred_at from events
                                                       where kind = '~a' and turn_id is not null
                                                       order by log_position)"
                                                nlk::+kind-turn-started+)))
      (sqlite:execute-non-query handle (turn-ends-fill-sql
                                        (format nil "(select turn_id, session_id, occurred_at, kind, payload
                                                        from events where kind in (~{'~a'~^, ~})
                                                         and turn_id is not null order by log_position)"
                                                +turn-end-kinds+))))))

(pushnew 'prepare-usage-rounds nlk::*schema-prepare-hooks*)
(pushnew 'open-usage-rounds nlk::*store-open-hooks*)

(defun usage-tier-sizes ()
  "The sizes usage_rollup's bands are bounded by, ascending."
  (nlk::with-store-handle (handle)
    (mapcar #'first (nlk::sql-rows handle "select size from usage_tier_sizes order by size"))))

(defun usage-band-sizes (since prices)
  "The sizes usage_rollup's bands are bounded by, ascending, once they bound
every tier the prices of the models that spent since the clock SINCE name;
PRICES memoizes MODEL-PRICE as TIER-SIZES-OF does."
  (let ((sizes (usage-tier-sizes))
        (needed (tier-sizes-of (nlk::with-store-handle (handle)
                                 (nlk::sql-rows handle "select distinct model, provider from usage_rollup
                                                         where bucket >= substr(?, 1, 16)"
                                                since))
                               prices)))
    (if (subsetp needed sizes)
        sizes
        (progn
          ;; A model whose price names a size no band is bounded by: band
          ;; every round by that size too. Rechecked under the write lock,
          ;; so tabs read at once derive it once.
          (nlk::with-store-transaction (handle)
            (let ((now (mapcar #'first (nlk::sql-rows handle "select size from usage_tier_sizes"))))
              (unless (subsetp needed now)
                (derive-usage-rollup handle (sort (union needed now) #'<)))))
          (usage-tier-sizes)))))

(defparameter +usage-sums-sql+
  "sum(rounds), sum(priced * rounds), sum(cost), sum(input), sum(cached), sum(write),
   sum(output), count(distinct root)"
  "What a group of ROUNDS (USAGE-SPEND-SQL) adds up to, in USAGE-SUM-OF's order.")

(defun usage-spend-sql (body)
  "BODY, a select over ROUNDS, behind the terms that define it: each rollup
row since the clock ?2 that was no voided attempt, with the ROOT its spend is
charged to -- a note session's, the session it reflected on, through every
note between. ?1 is the note source."
  ;; SQL sums every table of the read, so a few hundred rows reach Lisp
  ;; rather than a row per session, day and model: carried across one by
  ;; one, those rows were most of a long range's time.
  (format nil "with recursive up (session_id, root, depth) as (
         select session_id, parent, 1 from session_directory
          where created_at is not null and source = ?1 and typeof(parent) = 'text'
         union all
         select up.session_id, d.parent, up.depth + 1
           from up join session_directory d on d.session_id = up.root
          where d.created_at is not null and d.source = ?1 and typeof(d.parent) = 'text'
            and up.depth < 64),
       roots (session_id, root) as (
         select session_id, root from (select session_id, root, max(depth) from up group by session_id)),
       rounds as materialized (
         select coalesce(roots.root, g.session_id) root, g.*
           from usage_rollup g left join roots on roots.session_id = g.session_id
          where g.bucket >= substr(?2, 1, 16) and g.voided = 0),
       spent (root, calls, priced, cost, input, cached, write, output) as materialized (
         select root, sum(rounds), sum(priced * rounds), sum(cost), sum(input), sum(cached),
                sum(write), sum(output)
           from rounds group by root)
     ~a" body))

(defun asked-turns-sql (&optional (table "turn_spans"))
  "TABLE's turns narrowed as ASKED-WHERE narrows the log, ?1 the note source:
no note session's turn, no reflection."
  (format nil "~a.reflection = 0 and ~:*~a.session_id not in
                 (select session_id from session_directory where source = ?1)" table))

(defparameter +turn-ms-sql+
  "cast(round((julianday(ended_at) - julianday(started_at)) * 86400000) as integer)"
  "A turn's time, its end's clock less its start's, in milliseconds.")

(defun usage-spend (since)
  "What every session spent since the clock SINCE (all when \"\"), summed in
SQL in one statement: a plist of :TOTAL (a row of +USAGE-SUMS-SQL+), :VOIDED
(the voided attempts' cost), :MODELS (each MODEL PROVIDER, its sums, and its
rounds the nodecode.ai account paid), :BANDS (each MODEL PROVIDER BAND INPUT
CACHED WRITE), :DAYS (each local DAY and its sums), :FOLDERS (each FOLDER and
its sums) and :SESSIONS (the costliest eight, then the busiest: each ID TITLE
FOLDER, its sums and its turns' time, a session that only ran turns ranked
last)."
  ;; One statement reads ROUNDS once: each table a member of one UNION ALL,
  ;; its row TAG K1 K2 K3, eight numbers, and one more.
  (let ((rows (nlk::with-store-handle (handle)
                (nlk::sql-rows
                 handle
                 (usage-spend-sql
                  (format nil "select 'total', null, null, null, ~a, null from rounds
                     union all
                     select 'voided', null, null, null, sum(cost), null, null, null, null, null, null, null, null
                       from usage_rollup where bucket >= substr(?2, 1, 16) and voided = 1
                     union all
                     select 'model', model, provider, null, ~:*~a,
                            sum(case when priced = 0 and provider = 'nodecode' then rounds else 0 end)
                       from rounds group by model, provider
                     union all
                     select 'band', model, provider, band, sum(input), sum(cached), sum(write),
                            null, null, null, null, null, null
                       from rounds where cached > 0 or write > 0 group by model, provider, band
                     union all
                     select 'day', date(bucket, 'localtime'), null, null, ~:*~a, null from rounds group by 2
                     union all
                     select 'folder', coalesce(d.cwd, ''), null, null, sum(s.calls), sum(s.priced), sum(s.cost),
                            sum(s.input), sum(s.cached), sum(s.write), sum(s.output), count(*), null
                       from spent s left join session_directory d
                         on d.session_id = s.root and d.created_at is not null
                      group by 2
                     union all
                     select * from (
                       with timed (session_id, ms) as (
                              select session_id, sum(~a) from turn_spans
                               where kind is not null and ended_at >= ?2 and ~a
                               group by 1 having sum(~:*~:*~a) is not null),
                            ranked (root, calls, priced, cost, input, cached, write, output, sessions) as (
                              select root, calls, priced, cost, input, cached, write, output, 1 from spent
                              union all
                              select session_id, 0, 0, null, 0, 0, 0, 0, 0 from timed
                               where session_id not in (select root from spent)),
                            top as (select * from ranked order by coalesce(cost, 0) desc, calls desc limit 8)
                       select 'session', top.root, coalesce(d.title, d.prompt, ''), coalesce(d.cwd, ''),
                              top.calls, top.priced, top.cost, top.input, top.cached, top.write,
                              top.output, top.sessions, timed.ms
                         from top left join timed on timed.session_id = top.root
                              left join session_directory d
                                on d.session_id = top.root and d.created_at is not null)"
                          +usage-sums-sql+ +turn-ms-sql+ (asked-turns-sql)))
                 nlk::+note-source+ since)))
        (spend (list :total nil :voided nil :models '() :bands '() :days '() :folders '()
                     :sessions '())))
    (loop for (tag k1 k2 k3 . numbers) in rows
          do (dispatch tag equal
               ("total" (setf (getf spend :total) (butlast numbers)))
               ("voided" (setf (getf spend :voided) (first numbers)))
               ("model" (push (list* k1 k2 numbers) (getf spend :models)))
               ("band" (push (list* k1 k2 k3 (subseq numbers 0 3)) (getf spend :bands)))
               ("day" (push (cons k1 (butlast numbers)) (getf spend :days)))
               ("folder" (push (cons k1 (butlast numbers)) (getf spend :folders)))
               ("session" (push (list* k1 k2 k3 numbers) (getf spend :sessions)))))
    ;; The costliest first, then the busiest: a compound statement keeps no
    ;; member's order.
    (setf (getf spend :sessions)
          (sort (getf spend :sessions) (descending (lambda (row) (or (nth 5 row) 0)) #'fourth)))
    spend))

(defun request-waits (where)
  "Over WHERE, the requests' waits for a first token: => (values P50 P90
MEDIANS STREAMED), P50 and P90 every request's, MEDIANS each model's
middle wait as ((MODEL PROVIDER) . MS), STREAMED each model's time spent
writing as ((MODEL PROVIDER) . MS). Each is MEDIAN-INTEGER's pick: the
wait at that share of the count, rounded down, read as one row of the
waits in order -- a window over every request sorted them twice."
  (nlk::with-store-handle (handle)
    (let* ((clause (format nil "~@[ and ~a~]" (first where)))
           (models (apply #'nlk::sql-rows handle
                          (format nil "select model, provider, count(ttft), sum(streamed)
                                         from request_timings where 1~a group by 1, 2" clause)
                          (rest where)))
           (count (reduce #'+ models :key #'third)))
      (flet ((pick (at &optional (scope "") &rest keys)
               (apply #'sqlite:execute-single handle
                      (format nil "select ttft from request_timings where ttft is not null~a~a
                                    order by ttft limit 1 offset ?" scope clause)
                      (append keys (rest where) (list at)))))
        (values (and (plusp count) (pick (floor count 2)))
                (and (plusp count) (pick (floor (* 9 count) 10)))
                (loop for (model provider waits) in models
                      when (plusp waits)
                        collect (cons (list model provider)
                                      (pick (floor waits 2) " and model is ? and provider is ?"
                                            model provider)))
                (loop for (model provider nil writing) in models
                      when writing collect (cons (list model provider) writing)))))))

(defun usage-turns (since)
  "The turns the operator asked (ASKED-TURNS-SQL) since the clock SINCE (all
when \"\"): => (values FOLDERS STARTED), FOLDERS each (FOLDER TURNS FAILED
MS) of the turns that ended, MS their timed turns' sum or NIL, STARTED how
many began."
  (let ((parameters (list nlk::+note-source+ since)))
    (nlk::with-store-handle (handle)
      (values (apply #'nlk::sql-rows handle
                     (format nil "with ends (session_id, turns, failed, ms) as (
                                    select session_id, count(*), sum(kind = '~a'), sum(~a) from turn_spans
                                     where kind is not null and ended_at >= ?2 and ~a group by 1)
                                  select coalesce(d.cwd, ''), sum(turns), sum(failed), sum(ms)
                                    from ends e left join session_directory d
                                      on d.session_id = e.session_id and d.created_at is not null
                                   group by 1"
                             nlk::+kind-turn-failed+ +turn-ms-sql+ (asked-turns-sql))
                     parameters)
              (apply #'sqlite:execute-single handle
                     (format nil "select count(*) from turn_spans where started_at >= ?2 and ~a"
                             (asked-turns-sql))
                     parameters)))))

(defun usage-sum-of (row)
  "The usage sum ROW says, its numbers in +USAGE-SUMS-SQL+'s order."
  (destructuring-bind (calls priced cost input cached write output sessions)
      (mapcar (lambda (value) (or value 0)) row)
    (make-usage-sum :calls calls :priced priced :cost cost :input input :cached cached
                    :write write :output output :sessions sessions)))

(defun usage-json (range &aux (now (get-universal-time)))
  "What every session spent over RANGE (USAGE-RANGE-START): the totals, each
day, each model with its failed attempts, each folder with its failed turns,
and the costliest sessions."
  (nlk::with-store-reader ()
    (let* ((start (usage-range-start range now))
           (where (and start (list "occurred_at >= ?" (iso-time (- start +unix-epoch+)))))
           (since (if where (second where) ""))
           (prices (make-hash-table :test #'equal))
           (sizes (usage-band-sizes since prices))
           (spend (usage-spend since))
           (total (usage-sum-of (getf spend :total)))
           (days (make-hash-table :test #'equal))
           (models (make-hash-table :test #'equal)) (folders (make-hash-table :test #'equal))
           (saved 0)
           ;; Rounds the nodecode.ai account paid -- the plan's requests or its
           ;; credit -- that have no API price here: counted, never `unpriced'
           ;; (a buyer's /usage said `1 call has no price', checkout audit
           ;; 2026-10-22).
           (account 0)
           (failed 0))
      (flet ((sum-of (table key) (alexandria:ensure-gethash key table (make-usage-sum))))
        ;; A voided attempt is no round, but its spend was real.
        (incf (us-cost total) (or (getf spend :voided) 0))
        (loop for (model provider . row) in (getf spend :models)
              do (setf (gethash (list model provider) models) (usage-sum-of (butlast row)))
                 (incf account (first (last row))))
        ;; One band's rounds bill at one tier of the price: its four rates,
        ;; handed every round's counts at once.
        (loop for (model provider band input cached write) in (getf spend :bands)
              for rates = (alexandria:ensure-gethash (cons model provider) prices
                                                     (model-price model provider))
              when rates
                do (incf saved (round-cache-saving
                                (price-tier rates (if (plusp band) (1+ (nth (1- band) sizes)) 0))
                                :input-tokens input :cached-input-tokens cached
                                :cache-write-tokens write)))
        (loop for (day . row) in (getf spend :days)
              do (setf (gethash day days) (usage-sum-of row)))
        (loop for (folder . row) in (getf spend :folders)
              do (setf (gethash folder folders) (usage-sum-of row)))
        (multiple-value-bind (ends started) (usage-turns since)
          (loop for (folder turns failures ms) in ends
                for sum = (sum-of folders folder)
                do (incf (us-turns sum) turns)
                   (incf (us-failed sum) failures)
                   (incf failed failures)
                   (when ms (incf (us-ms sum) ms)))
          ;; A model's attempt failed when it was retried, when the turn moved off
          ;; it (the switch's from-model), or when its provider failure ended the
          ;; turn; the rest of its attempts are its rounds. A failed turn names
          ;; the model its request targeted; one recorded before it did is charged
          ;; to the model its last attempt went to.
          (loop for (turn model provider status)
                  in (nlk:events :kind (list nlk::+kind-provider-retry+ nlk::+kind-provider-fallback+
                                             nlk::+kind-turn-failed+)
                                 :where (cons (format nil "~@[~a and ~](kind <> 'turn.failed' or ~
                                                       json_extract(payload, '$.\"condition-type\"') like 'PROVIDER-%')"
                                                      (first where))
                                              (rest where))
                                 :as :rows
                                 :columns '("turn_id"
                                            "coalesce(json_extract(payload, '$.\"from-model\"'), json_extract(payload, '$.model'))"
                                            "coalesce(json_extract(payload, '$.\"from-provider\"'), json_extract(payload, '$.provider'))"
                                            "json_extract(payload, '$.status')"))
                for key = (if (plusp (length model))
                              (list model provider)
                              (nlk:events :turn-id turn :order :newest :as :row
                                          :kind (list nlk::+kind-provider-request+ nlk::+kind-provider-retry+
                                                      nlk::+kind-provider-fallback+)
                                          :columns '("coalesce(json_extract(payload, '$.\"to-model\"'), json_extract(payload, '$.model'))"
                                                     "coalesce(json_extract(payload, '$.\"to-provider\"'), json_extract(payload, '$.provider'))")))
                when (plusp (length (first key)))
                  do (push status (us-statuses (sum-of models key))))
          (multiple-value-bind (wait-p50 wait-p90 medians streamed) (request-waits where)
            (json-object
             "range" range
             :opt "from" (and start (iso-time (- start +unix-epoch+)))
             "totals" (usage-sum-json total
                                      "unpriced" (- (us-calls total) (us-priced total) account)
                                      "account_calls" account
                                      "saved_usd" (float saved 1d0)
                                      "turns" started
                                      "failed_turns" failed)
             :opt "wait_p50_ms" wait-p50
             :opt "wait_p90_ms" wait-p90
             "days" (loop-vector for day in (if start
                                                (loop for universal from start to now by 86400
                                                      collect (local-day universal))
                                                (sort (alexandria:hash-table-keys days) #'string<))
                                 for sum = (sum-of days day)
                                 collect (usage-sum-json sum "day" day))
             "models" (loop-vector for ((model provider) . sum) in (ranked models)
                                   ;; Each model's waits and the time it spent
                                   ;; writing, for its typical wait and its speed.
                                   for wait = (cdr (assoc (list model provider) medians :test #'equal))
                                   for writing = (or (cdr (assoc (list model provider) streamed :test #'equal)) 0)
                                   for failures = sum.statuses
                                   for attempts = (+ sum.calls (length failures))
                                   ;; Each status the failures met, the commonest first.
                                   for statuses = (sort (remove-duplicates (copy-list failures)) #'>
                                                        :key (lambda (status) (count status failures)))
                                   collect (apply #'usage-sum-json sum "model" model "provider" provider
                                                  "attempts" attempts "failed" (length failures)
                                                  "error_rate" (/ (length failures) attempts 1d0)
                                                  "statuses" (loop-vector for status in statuses
                                                                          collect (json-object :opt "status" status
                                                                                               "count" (count status failures)))
                                                  (append
                                                   (and wait (list "wait_p50_ms" wait))
                                                   ;; Output over the time spent writing it.
                                                   (and (plusp writing) (plusp sum.output)
                                                        (list "speed" (round sum.output
                                                                             (/ writing 1000)))))))
             "folders" (loop-vector for (name . sum) in (ranked folders 8)
                                    collect (usage-sum-json sum "folder" name))
             "sessions" (loop-vector for (id title folder . row) in (getf spend :sessions)
                                     for sum = (usage-sum-of (butlast row))
                                     do (setf (us-ms sum) (or (first (last row)) 0))
                                     collect (usage-sum-json sum "id" id "title" title "folder" folder)))))))))

(define-gateway-route handle-gateway-usage-http (env)
  "GET /api/gateway/usage?range=today|7d|30d|all — what every session spent
over the range (USAGE-JSON); seven days when unsaid."
  (:route :get "/api/gateway/usage" "usage")
  (usage-json (or (cdr (assoc "range" (gateway-query-parameters env) :test #'string=)) "7d")))

;;; --- activity: what the agent did over a range, and what went wrong -----------
;;; The Dashboard's second read, beside usage: each step sorted into the
;;; families the transcript titles it by (ACTIVITY-TITLE's own reader, so a
;;; step counts as `Read' here exactly where its card says `Read'), what each
;;; family cost, the hours the turns ran, the provider's failures, the turns
;;; that failed, and the newest model calls. A step failed when its result is
;;; the ERROR: text every reader of a tool result already keys on.

(defun step-families (name arguments)
  "What one call of tool NAME with ARGUMENTS did, as (FAMILY . ITEMS) in the
order it did them, or NIL when no activity family names it."
  (when-let (activity (with-handlers ((error () nil))
                        (tool-arguments-activity name (if (stringp arguments) arguments ""))))
    (if-let (entries (activity-entries activity))
      (loop for family in (remove-duplicates (mapcar #'first entries) :from-end t)
            collect (cons family (count family entries :key #'first)))
      (list (cons (activity-family activity) 1)))))

(defun step-title (name arguments)
  "One call of tool NAME in words, as its card titles it, else NAME."
  (or (with-handlers ((error () nil))
        (when-let (activity (tool-arguments-activity name (if (stringp arguments) arguments "")))
          (prompt-line (activity-title activity) 120)))
      name))

(defun by-count (table)
  "TABLE's (KEY COUNT . MORE) entries in its own order, then the commonest first."
  (sort (nreverse (alexandria:hash-table-alist table)) #'> :key #'second))

(defun spend-by-step-json (rounds)
  "Where ROUNDS' money went, each (POSITION TURN PRICE VOIDED .
WHAT-EACH-CALL-DID) with PRICE NIL for none: a round's price split evenly
over the calls it asked for, a call's share evenly over the families it did
(STEP-FAMILIES, else its tool's name), a round that asked for none writing
the reply, and what the voided attempts billed beside them. The parts sum to
the priced total; an unpriced round is counted, never summed as zero, and
still shares out the model calls. BY says what the parts are weighed by:
the money when most rounds have a price, else the model calls."
  (let ((parts (make-hash-table :test #'equal)) (spent nil) (answered 0) (unpriced 0))
    (flet ((charge (key cost share &aux (part (alexandria:ensure-gethash key parts (list nil 0 0))))
             ;; KEY's (COST ROUNDS CALLS): one more call, COST more spent
             ;; when known, and SHARE more of a round.
             (when cost (setf (first part) (+ (or (first part) 0) cost)))
             (incf (second part) share)
             (incf (third part))))
      (loop for (nil nil price voided . calls) in rounds
            do (when price (setf spent (+ (or spent 0) price)))
               (cond (voided (when price (charge :voided price 0)))
                     (t (incf answered)
                        (unless price (incf unpriced))
                        (if calls
                            (dolist (keys calls)
                              (dolist (key keys)
                                (let ((share (/ 1d0 (length calls) (length keys))))
                                  (charge key (and price (* price share)) share))))
                            (charge :reply price 1))))))
    (let ((money (and spent (>= (- answered unpriced) unpriced))))
      (flet ((weight (entry) (if money (or (second entry) 0) (third entry))))
        (json-object
         :opt "cost_usd" spent
         "rounds" answered
         "unpriced" unpriced
         "by" (if money "cost" "calls")
         "parts" (loop-vector for (key cost share calls)
                              in (sort (nreverse (alexandria:hash-table-alist parts))
                                       ;; The weightiest first, then the most model calls.
                                       (descending #'weight #'third))
                              for plist = (and (keywordp key) (activity-family-plist key))
                              collect (json-object :when plist "family" (string-downcase key)
                                                   :when plist "verb" (getf plist :past)
                                                   :when plist "many" (getf plist :many)
                                                   :when (stringp key) "tool" key
                                                   :when (eq key :reply) "reply" t
                                                   :when (eq key :voided) "voided" t
                                                   :opt "cost_usd" cost
                                                   "rounds" (float share 1d0)
                                                   "calls" calls)))))))

(defun activity-json (range &aux (now (get-universal-time)))
  "What the agent did over RANGE (USAGE-RANGE-START) and what went wrong: its
steps by family, the slowest of them, where the money went by step
(SPEND-BY-STEP-JSON), the hour each turn started, the provider's retries and
switches, the failed turns, and the newest calls."
  (nlk::with-store-reader ()
    (let* ((start (usage-range-start range now))
           (where (and start (list "occurred_at >= ?" (iso-time (- start +unix-epoch+)))))
           (results (make-hash-table :test #'equal))
           (families (make-hash-table :test #'eq))
           (others (make-hash-table :test #'equal))
           (hours (make-array '(7 24) :initial-element 0))
           (steps 0) (failed-steps 0) (step-ms 0) (slowest '())
           ;; The range's rounds, oldest first, as (POSITION TURN PRICE VOIDED .
           ;; WHAT-EACH-CALL-DID): a call belongs to the round its turn logged
           ;; last before it, and a voided attempt is no turn's round.
           (rounds (loop for (position turn cost voided)
                           in (nlk:events :kind nlk::+kind-turn-usage+ :where where :as :rows
                                          ;; The price is read only where it is known.
                                          :columns '("log_position" "turn_id"
                                                     "case when json_extract(payload, '$.cost_known')
                                                        then json_extract(payload, '$.cost_usd') end"
                                                     "json_extract(payload, '$.voided')"))
                         collect (list position turn (and (realp cost) cost) (eql voided 1))))
           (waiting rounds)
           (open-rounds (make-hash-table :test #'equal)))
      ;; The calls a page at a time: a month of arguments is tens of megabytes,
      ;; and only the six slowest are kept past their page. The results are
      ;; read once the first page says where the range's calls begin: a result
      ;; is logged after its call, so none before that is ever looked up, and
      ;; a week's read skips the older results' rows.
      (loop for after = nil then (first (car (last page)))
            for page = (nlk:events :kind nlk::+kind-tool-call-started+ :where where :after after
                                   :limit 2000 :as :rows
                                   :columns (list* "log_position" "session_id" "turn_id"
                                                   (payload-columns "call-id" "tool-name"
                                                                    "arguments")))
            while page
            do (unless after
                 (loop for (turn call ms error)
                         in (nlk:events :kind nlk::+kind-tool-result+ :where where
                                        :after (first (first page)) :as :rows
                                        :columns '("turn_id" "json_extract(payload, '$.\"call-id\"')"
                                                   "json_extract(payload, '$.\"duration-ms\"')"
                                                   "substr(json_extract(payload, '$.result'), 1, 6) = 'ERROR:'"))
                       do (setf (gethash (cons turn call) results) (cons ms (eql error 1)))))
               (loop for (position session turn call name arguments) in page
                     for (ms . failed) = (gethash (cons turn call) results)
                     for did = (step-families name arguments)
                     do (loop for round = (first waiting)
                              while (and round (< (first round) position))
                              do (pop waiting)
                                 (unless (fourth round) (setf (gethash (second round) open-rounds) round)))
                        (when-let (round (gethash turn open-rounds))
                          (push (if did (mapcar #'car did) (list name)) (cddddr round)))
                        (incf steps)
                        (when failed (incf failed-steps))
                        (when (realp ms)
                          (incf step-ms ms)
                          (when (or (< (length slowest) 6) (> ms (first (car (last slowest)))))
                            (setf slowest (merge 'list (list (list ms session turn name arguments))
                                                 slowest #'> :key #'first))
                            (when (> (length slowest) 6) (setf slowest (subseq slowest 0 6)))))
                        (if did
                            (loop for (family . items) in did
                                  for sum = (alexandria:ensure-gethash family families (list 0 0 0))
                                  do (incf (first sum)) (incf (second sum) items)
                                     (when failed (incf (third sum))))
                            (let ((sum (alexandria:ensure-gethash name others (list 0 0))))
                              (incf (first sum))
                              (when failed (incf (second sum)))))))
      (loop for (at) in (nlk:events :kind nlk::+kind-turn-started+ :where (asked-where where) :as :rows
                                    :columns '("occurred_at"))
            for universal = (iso-universal at)
            when universal
              do (bind (((_ _ hour _ _ _ weekday) (decode-universal-time universal)))
                   (incf (aref hours weekday hour))))
      (let ((retries (make-hash-table :test #'equal))
            (fallbacks (make-hash-table :test #'equal))
            (calls '()) (waiting (make-hash-table :test #'equal)))
        (loop for (status detail)
                in (nlk:events :kind nlk::+kind-provider-retry+ :where where :as :rows
                               :columns (payload-columns "status" "detail"))
              for sum = (alexandria:ensure-gethash status retries (list 0 nil))
              do (incf (first sum))
                 (setf (second sum) detail))
        (loop for (from to reason)
                in (nlk:events :kind nlk::+kind-provider-fallback+ :where where :as :rows
                               :columns (payload-columns "from-model" "to-model" "reason"))
              do (incf (gethash (list from to reason) fallbacks 0)))
        ;; Newest first, a round's usage comes just before its request: the
        ;; request that follows a usage in the same turn is that round's.
        (loop for (kind session turn at model provider input cached write output known cost voided wait stream)
                in (nlk:events :kind (list nlk::+kind-turn-usage+ nlk::+kind-provider-request+)
                               :where where :order :newest :limit 80 :as :rows
                               :columns (list* "kind" "session_id" "turn_id" "occurred_at"
                                               (append (subseq +usage-columns+ 2)
                                                       (payload-columns "ttft-ms" "stream-duration-ms"))))
              do (cond ((and (equal kind nlk::+kind-turn-usage+) (not (eql voided 1)))
                        (let ((call (json-object "at" at "session_id" session
                                                 "title" (nlk::session-title session)
                                                 "turn_id" turn "model" model "provider" provider
                                                 :opt "input" input :opt "cached" cached
                                                 :opt "cache_write" write :opt "output" output
                                                 :opt "cost_usd" (and (eql known 1) (realp cost) cost))))
                          (push call calls)
                          (setf (gethash turn waiting) call)))
                       ((equal kind nlk::+kind-provider-request+)
                        (when-let (call (gethash turn waiting))
                          (remhash turn waiting)
                          (when (realp wait) (setf (gethash "wait_ms" call) wait))
                          (when (realp stream) (setf (gethash "stream_ms" call) stream))))))
        (json-object
         "range" range
         :opt "from" (and start (iso-time (- start +unix-epoch+)))
         "steps" (json-object "count" steps "failed" failed-steps :when (plusp step-ms) "ms" step-ms)
         "families" (loop-vector for (family used items failed) in (by-count families)
                                 for plist = (activity-family-plist family)
                                 collect (json-object "family" (string-downcase family)
                                                      "verb" (getf plist :past)
                                                      "one" (getf plist :one) "many" (getf plist :many)
                                                      "calls" used "items" items "failed" failed))
         "other_tools" (loop-vector for (name used failed) in (by-count others)
                                    collect (json-object "tool" name "calls" used "failed" failed))
         "spend" (spend-by-step-json rounds)
         "slowest" (loop-vector for (ms session turn name arguments) in slowest
                                collect (json-object "ms" ms "session_id" session
                                                     "title" (nlk::session-title session)
                                                     "turn_id" turn "tool" name
                                                     "step" (step-title name arguments)))
         "hours" (loop-vector for day below 7
                              collect (loop-vector for hour below 24 collect (aref hours day hour)))
         "retries" (loop-vector for (status count detail) in (by-count retries)
                                collect (json-object :opt "status" status "count" count
                                                     "detail" (prompt-line detail 160)))
         "fallbacks" (loop-vector for (from to reason) being the hash-keys of fallbacks using (hash-value count)
                                  collect (json-object "from" from "to" to "reason" reason "count" count))
         "failed_turns"
         (json-object
          "count" (nlk:events :kind nlk::+kind-turn-failed+ :where (asked-where where) :as :count)
          "cancelled" (nlk:events :kind nlk::+kind-turn-cancelled+ :where (asked-where where) :as :count)
          "latest" (loop-vector for (session turn at detail status)
                                in (nlk:events :kind nlk::+kind-turn-failed+ :where (asked-where where)
                                               :order :newest :limit 6 :as :rows
                                               :columns (list* "session_id" "turn_id" "occurred_at"
                                                               (payload-columns "detail" "status")))
                                collect (json-object "session_id" session
                                                     "title" (nlk::session-title session)
                                                     "turn_id" turn "at" at
                                                     "detail" (prompt-line detail 200)
                                                     :opt "status" status)))
         "calls" (loop-vector for call in (nreverse calls) repeat 30 collect call))))))

(define-gateway-route handle-gateway-activity-http (env)
  "GET /api/gateway/activity?range=today|7d|30d|all — what the agent did over
the range and what went wrong (ACTIVITY-JSON); seven days when unsaid."
  (:route :get "/api/gateway/activity" "activity")
  ;; Sorting a month of steps conses a gigabyte and more in passing:
  ;; under the organism's 2 MB nursery that is a collection every two
  ;; megabytes. The read's nursery is the read's, as a compile's is
  ;; (COMPILE-LAYER-SHARES), and the cap comes back after.
  (nlk:with-nursery ((* 64 1024 1024))
    (activity-json (or (cdr (assoc "range" (gateway-query-parameters env) :test #'string=)) "7d"))))

;;; --- /usage: the usage read as text ---------------------------------------------
;;; One rendering for every text surface: the shell's /usage asks its gateway
;;; for the read, a channel's or the web page's `/' runs it in the gateway's own
;;; image (HEADLESS-USAGE), and both print USAGE-REPORT-TEXT of the same object.

(defun usage-range-word (args &aux (word (string-trim '(#\Space #\Tab) (or args ""))))
  "The range ARGS names for /usage -- seven days when empty -- or NIL."
  ;; What /usage takes after its name, and the range each word means.
  (dispatch word string-equal ("" "7d") ("today" "today") ("7d" "7d") ("week" "7d")
    ("30d" "30d") ("month" "30d") ("all" "all")))

(defun usage-field (object key &aux (value (and (hash-table-p object) (gethash key object))))
  "KEY of OBJECT, a usage read's object in memory or off the wire, NIL for
absent, null or false."
  (if (member value '(:null :false)) nil value))

(defun usd-text (value)
  "VALUE in dollars as a reader counts them, a dash for no price."
  (cond ((not (realp value)) "-")
        ((< value 0.01) (format nil "$~,3f" value))
        (t (format nil "$~,2f" value))))

(defun ms-text (ms)
  "MS as a wait reads: milliseconds, seconds to a tenth, then DURATION-TEXT;
a dash for none."
  (cond ((not (realp ms)) "-")
        ((< ms 1000) (format nil "~d ms" (round ms)))
        ((< ms 10000) (format nil "~,1f s" (/ ms 1000)))
        (t (duration-text (/ ms 1000)))))

(defun share-text (share)
  "SHARE, a fraction, as a whole percent; a dash for none."
  (if (realp share) (format nil "~d%" (round (* 100 share))) "-"))

(defun folder-tail (dir)
  "DIR's last two segments: what tells one project from another."
  (let ((parts (remove "" (uiop:split-string (or dir "") :separator "/") :test #'string=)))
    (if parts (format nil "~{~a~^/~}" (last parts 2)) "-")))

(defun counted (n word)
  "N WORDs, the count grouped by thousands."
  (format nil "~:d ~a~:[s~;~]" n word (eql n 1)))

(defun usage-range-phrase (range)
  "RANGE as /usage says it: today, these 30 days, all told, else this week."
  (dispatch range equal
    ("today" "today") ("30d" "these 30 days") ("all" "all told") (t "this week")))

(defun usage-headline (usage &aux (totals (usage-field usage "totals"))
                                  (cost (usage-field totals "cost_usd")))
  "The sentence the Usage view leads with: the range, the spend or the calls,
and the sessions."
  (format nil "~@(~a~), ~a across ~a." (usage-range-phrase (usage-field usage "range"))
          (if cost (usd-text cost) (counted (or (usage-field totals "calls") 0) "model call"))
          (counted (or (usage-field totals "sessions") 0) "session")))

(defun usage-since (usage)
  "The first local day USAGE counts, as \"Sep 20\", or NIL for everything."
  (when-let (universal (iso-universal (usage-field usage "from")))
    (bind (((_ _ _ day month) (decode-universal-time universal)))
      (format nil "~a ~d" (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                                           "Aug" "Sep" "Oct" "Nov" "Dec"))
              day))))

(defun usage-sentences (usage &aux (totals (usage-field usage "totals"))
                                   (cost (usage-field totals "cost_usd"))
                                   (calls (or (usage-field totals "calls") 0))
                                   (unpriced (or (usage-field totals "unpriced") 0))
                                   (saved (usage-field totals "saved_usd"))
                                   (turns (or (usage-field totals "turns") 0)))
  "What follows the headline: the calls, the cache, the unpriced calls, the
failed turns -- each only when it says something."
  (remove nil
          (list (and cost (format nil "~a." (counted calls "model call")))
                (cond ((and (realp saved) (> saved 0.005))
                       (format nil "Reusing the prompt from cache saved about ~a." (usd-text saved)))
                      ((and (realp saved) (< saved -0.005))
                       (format nil "Writing the prompt to cache cost about ~a more than reading it back saved."
                               (usd-text (- saved))))
                      ((usage-field totals "cached_share")
                       (format nil "~a of the prompt came from cache."
                               (share-text (usage-field totals "cached_share")))))
                (let ((account (or (usage-field totals "account_calls") 0)))
                  (and (plusp account)
                       (format nil "~a on your nodecode.ai account, paid in its plan's requests or its credit."
                               (if (eql account 1) "1 call ran" (format nil "~:d calls ran" account)))))
                (and (plusp unpriced)
                     (format nil "~a no price~:[~;, so the spend is a floor~]."
                             (if (eql unpriced 1) "1 call has" (format nil "~:d calls have" unpriced))
                             cost))
                (and (plusp turns)
                     (format nil "~:d of ~a failed." (or (usage-field totals "failed_turns") 0)
                             (counted turns "turn"))))))

(defun usage-report-text (usage)
  "USAGE, the usage read (USAGE-JSON, or the copy its route sent), as the
lines a text surface prints: the Usage view's sentence and its tables."
  (flet ((clip (text width) (stub-clip (or text "") width "~"))
         (took (ms) (if (and (realp ms) (plusp ms)) (ms-text ms) "-")))
    (let ((cost (usage-field (usage-field usage "totals") "cost_usd")))
      (with-output-to-string (out)
        (format out "Usage · ~a~@[ (since ~a)~] · API-equivalent prices~%"
                (usage-range-phrase (usage-field usage "range")) (usage-since usage))
        (format out "~a~%~{~a~^ ~}~%" (usage-headline usage) (usage-sentences usage))
        (when (usage-field usage "wait_p50_ms")
          (format out "First word ~a typical (TTFT p50), ~a at p90.~%"
                  (ms-text (usage-field usage "wait_p50_ms")) (ms-text (usage-field usage "wait_p90_ms"))))
        (when-let (models (coerce (or (usage-field usage "models") #()) 'list))
          (format out "~%~32a ~8@a ~6@a ~9@a~%" "model" "calls" "cache" "spend")
          (dolist (model (subseq models 0 (min 6 (length models))))
            (format out "~32a ~8:d ~6@a ~9@a~%" (clip (usage-field model "model") 32)
                    (usage-field model "calls")
                    (share-text (usage-field model "cached_share"))
                    (usd-text (usage-field model "cost_usd")))))
        (when-let (rows (coerce (or (usage-field usage "sessions") #()) 'list))
          (format out "~%~a~%" (if cost "costliest sessions" "busiest sessions"))
          (dolist (row (subseq rows 0 (min 5 (length rows))))
            (format out "~32a ~16a ~8@a ~9@a~%"
                    (clip (let ((title (usage-field row "title")))
                            (if (plusp (length title)) title (usage-field row "id")))
                          32)
                    (clip (folder-tail (usage-field row "folder")) 16) (took (usage-field row "ms"))
                    (if (usage-field row "cost_usd") (usd-text (usage-field row "cost_usd"))
                        (format nil "~:d calls" (usage-field row "calls"))))))
        (when-let (folders (coerce (or (usage-field usage "folders") #()) 'list))
          (format out "~%~32a ~16@a ~8@a ~9@a~%" "folder" "sessions" "time" "spend")
          (dolist (folder (subseq folders 0 (min 5 (length folders))))
            (format out "~32a ~16:d ~8@a ~9@a~%" (clip (folder-tail (usage-field folder "folder")) 32)
                    (usage-field folder "sessions") (took (usage-field folder "ms"))
                    (usd-text (usage-field folder "cost_usd")))))))))

(defun headless-usage (args session-id)
  "The /usage answer without a shell: the usage read over the range ARGS
names, in this image, as text."
  (declare (ignore session-id))
  (if-let (range (usage-range-word args))
    (string-right-trim '(#\Newline) (usage-report-text (usage-json range)))
    "Usage: /usage [today | 7d | 30d | all]"))
