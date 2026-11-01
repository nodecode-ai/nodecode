;;;; ingest.lisp --- ingest: the streaming and durable facts, and their methods. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- ingest: streaming --------------------------------------------------

(defun end-thinking (row)
  "Stop ROW's thinking clock: a :REASONING row that opened on a live delta
records how long it thought, once."
  ;; A row with no clock — replayed, or already stopped — is left as it is.
  (when (and row (eq :reasoning row.kind) row.started-at (null row.thought-ms))
    (setf row.thought-ms (since-ms row.started-at))))

(defun hydrate-row (state row kind text turn-id &key voided)
  "Seat the durable TEXT on ROW, the stream row it confirms — its thinking
clock stopped, persisted history now, stamped with TURN-ID when it carries
none — or, with no ROW, append a hydrated assistant KIND row carrying it."
  ;; VOIDED marks the row voided either way. => the row.
  (cond (row
         (end-thinking row)
         (setf row.text text
               row.retention :persisted-history
               row.hydrated t
               row.turn-id (or row.turn-id turn-id))
         (when voided (setf row.voided t))
         row)
        (t (tail-row state :kind kind :role "assistant" :turn-id turn-id
                     :hydrated t :voided voided :text text))))

(define-ingest ingest-stream-body (st kind text &key turn-id)
  "Coalesce a streamed assistant delta of KIND into the adjacent tail row of
that kind, or open one."
  ;; Adjacency is strict — only the newest tail row may accept an id-less
  ;; delta — so a tool round between two text rounds, or a content round
  ;; between two thinking spans, starts a new row rather than merging across
  ;; it (the Zig adjacent-row coalescing rule).
  (let ((tail (last-tail-row st)))
    ;; Stream progress retires any retry countdown.
    (remhash "retry_status" st)
    (cond ((and tail (eq tail (open-tail-row st kind turn-id :role "assistant")))
           ;; Append in place, O(delta), and bump TEXT-REV for the render caches.
           (let ((buf (transcript-row-text tail)))
             (unless (and (adjustable-array-p buf) (array-has-fill-pointer-p buf))
               (setf buf (stream-text-buffer buf (max 256 (length text)))
                     (transcript-row-text tail) buf))
             (loop for ch across text do (vector-push-extend ch buf)))
           (incf (transcript-row-text-rev tail)))
          (t (end-thinking tail)
             (tail-row st :kind kind :role "assistant" :retention :live-only
                       :turn-id turn-id :text (stream-text-buffer text)
                       :started-at (and (eq kind :reasoning)
                                        (get-internal-real-time)))))))

(defun retry-status (retry &optional (now (get-internal-real-time)))
  "The retry a stream_reset's RETRY object announced at NOW, as the live bar
keeps it: (:attempt :limit :due :continuation :status :provider :reason), DUE
the internal time its delay ends."
  ;; A deadline, not words: format `retrying 2/4 in 10s' from the
  ;; deadline at each read. Without this, a stale string would keep
  ;; promising a retry that already went out.
  (when retry
    (list :attempt (json-value retry :integer "attempt")
          :limit (json-value retry :integer "limit")
          :due (when-let (delay (json-value retry :number "delay_seconds"))
                 (+ now (round (* delay internal-time-units-per-second))))
          :continuation (json-value retry :boolean "continuation")
          :status (json-value retry :integer "status")
          :provider (json-value retry :text "provider")
          :reason (json-value retry :text "reason"))))

(defun retry-status-text (retry &optional now)
  "RETRY-STATUS's words => (values HEAD SOURCE REASON). HEAD is `retry 2 of 4
in 6s' -- `continue ...' when the retry carries the cut attempt's output back
rather than re-running it -- its countdown read at NOW, internal time, and
`sent' once the delay is over; with no NOW, for a reader that does not redraw,
it has none. SOURCE is the provider and its status, `deepseek 503'; REASON
what the provider said, whole. Either is NIL when unknown."
  ;; Absent numbers drop their segment rather than inventing zeros. The
  ;; provider's words are never cut down to a summary: its first sentence alone
  ;; dropped `We advise users to temporarily switch to alternative services.'
  ;; Always output the error literally.
  (destructuring-bind (&key attempt limit due continuation status provider reason &allow-other-keys) retry
    (let ((source (format nil "~@[~a~]~:[~; ~]~@[~d~]" provider (and provider status) status)))
      (values (format nil "~:[retry~;continue~]~@[ ~a~]~@[~a~]"
                      continuation
                      (and attempt (format nil "~d~@[ of ~d~]" attempt limit))
                      (and now due (if (> due now)
                                       (format nil " in ~ds" (ceiling (- due now)
                                                                      internal-time-units-per-second))
                                       " sent")))
              (and (plusp (length source)) source)
              (and reason (plusp (length (nlk:trimmed reason))) (nlk:trimmed reason))))))

(defun retry-said (retry)
  "Who said what of RETRY, on one line: `deepseek 503: Service is too busy.
We advise users to temporarily switch to alternative services.', the
provider's words whole with their line breaks as spaces -- or NIL."
  (bind (((_ source reason) (retry-status-text retry))
         (reason (and reason (substitute #\Space #\Newline reason))))
    (if (and source reason) (format nil "~a: ~a" source reason) (or reason source))))

(define-ingest ingest-stream-reset (st &key turn-id retry-status)
  "Void the unhydrated stream rows for TURN-ID: the attempt that produced
them failed and is being retried or allowed to stand."
  ;; What it streamed is kept, not wiped — its text and thinking rows are
  ;; marked voided so the next attempt opens fresh rows below them instead of
  ;; coalescing onto them — while its in-flight tool rows, calls that never
  ;; ran, are dropped. The durable turn.attempt_voided fact seals the kept
  ;; rows (on the gateway stream it lands before this reset, so they are
  ;; already hydrated; the local fold seals them at settle); a voided row no
  ;; fact ever confirms is dropped at the settle like any other stream
  ;; ephemera. Durable-fact rows are untouched — the log is the authority on
  ;; what actually happened. RETRY-STATUS (RETRY-STATUS's plist), when known,
  ;; heads the live bar until stream progress clears it — a retrying provider
  ;; must look different from a stalled one.
  (setf (gethash "tail_rows" st)
        (loop for row in (gethash "tail_rows" st '())
              when (or (transcript-row-hydrated row)
                       (not (row-turn-matches-p row turn-id)))
                collect row
              else when (member (transcript-row-kind row) '(:text :reasoning))
                do (setf (transcript-row-voided row) (or (transcript-row-voided row) t))
                and collect row))
  (if retry-status
      (setf (gethash "retry_status" st) retry-status)
      (remhash "retry_status" st)))

(define-ingest ingest-provider-retry-fact (st &key turn-id delay-seconds status provider detail)
  "Fold one turn.provider_retry fact into its turn's tally: how many retries,
the backoff they waited, and the newest STATUS and PROVIDER with what it said
in DETAIL. The turn leaves it as one line when it ends (RETRY-TALLY-TEXT)."
  ;; The bar's countdown is live-only and leaves with the stream's next
  ;; delta: without this, a turn that sits on 503s and then answers would
  ;; rest like any other, with nothing saying why it was slow.
  (let* ((tally (gethash "turn_retries" st))
         (same (equal turn-id (getf tally :turn-id))))
    (setf (gethash "turn_retries" st)
          (list :turn-id turn-id
                :count (1+ (if same (getf tally :count) 0))
                :waited (+ (or delay-seconds 0) (if same (getf tally :waited) 0))
                :status status
                :provider (and (plusp (length provider)) provider)
                :reason (and detail (provider-said detail))))))

(defun retry-tally-text (tally)
  "INGEST-PROVIDER-RETRY-FACT's TALLY as the line its turn leaves:
`• Waited 21.3s through 2 retries · deepseek 503: Service is too busy. We
advise users to temporarily switch to alternative services.'"
  ;; The mark stands in the gutter, as the eviction line's does, so the words
  ;; start on the column the messages above them read from.
  (format nil "• Waited ~a through ~d retr~:@p~@[ · ~a~]"
          (format-turn-elapsed (round (* 1000 (getf tally :waited))))
          (getf tally :count)
          (retry-said tally)))

(define-ingest ingest-attempt-voided-fact (st &key attempt reasoning text
                                              detail stream-duration-ms
                                              disposition turn-id)
  "Fold one turn.attempt_voided fact: seal the failed attempt's streamed
thinking and text rows as persisted history."
  ;; Each row pairs with the oldest
  ;; unhydrated row of its kind for the turn — the one the deltas built, whether
  ;; the live reset has marked it voided yet (local fold) or not (gateway stream,
  ;; where the fact lands first) — or opens a hydrated row when nothing streamed
  ;; (replay, a reconnect that missed the deltas). The label rests on the last
  ;; kept row; every sealed row is voided so nothing coalesces onto it.
  ;; The seal line: which attempt, how long it streamed, why it failed, and
  ;; what happened next, in the operator's words. A stream shorter than a
  ;; tenth of a second has no length worth saying.
  (let ((label (format nil "Attempt ~d cut off~@[ after ~a~]~@[: ~a~]. ~a"
                       (if (integerp attempt) attempt 1)
                       (and (integerp stream-duration-ms) (>= stream-duration-ms 100)
                            (format-turn-elapsed stream-duration-ms))
                       (and (stringp detail) (plusp (length detail)) detail)
                       (cond ((equal disposition "rerun") "Trying again.")
                             ((equal disposition "continued") "Continued below.")
                             (t "Not retried."))))
        (sealed '()))
    (flet ((seal (kind content)
             (when (and (stringp content) (plusp (length content)))
               (push (hydrate-row st (open-tail-row st kind turn-id
                                                    :role "assistant"
                                                    :oldest t :voided :any)
                                  kind content turn-id :voided t)
                     sealed))))
      (seal :reasoning reasoning)
      (seal :text text))
    (when sealed
      (setf (transcript-row-voided (first sealed)) label))))

(define-ingest ingest-stream-tool (st index name arguments &key turn-id)
  "Upsert the in-flight tool call at stream INDEX with the accumulated
NAME and ARGUMENTS so far."
  ;; Only an unhydrated row can be upserted: provider stream indices restart
  ;; at 0 every tool round, so a later round's deltas would otherwise adopt an
  ;; earlier round's completed row and overwrite its recorded arguments under
  ;; its result.
  (let ((row (open-tail-row st :tool-call turn-id :index index)))
    ;; Stream progress retires any retry countdown.
    (remhash "retry_status" st)
    (unless row
      (end-thinking (last-tail-row st))
      (setf row (tail-row st :kind :tool-call :role "assistant"
                          :retention :live-only :turn-id turn-id
                          :stream-index index :tool-status "running")))
    (setf (transcript-row-tool-name row) (or name "")
          (transcript-row-tool-args row) (or arguments ""))))

;;; --- ingest: durable facts ------------------------------------------------

(define-ingest ingest-message-fact (st role content &key turn-id reasoning prompt-id
                                       command-id recorded steer metadata)
  "Record one durable conversational message."
  ;; An assistant fact first hydrates the newest streamed text row for the turn —
  ;; the same row the deltas built — so the durable content replaces the stream
  ;; instead of appearing twice. An assistant fact with no content and no streamed
  ;; row is a tool-only round and contributes no row. REASONING, when the round
  ;; produced one, hydrates that round's thinking row the same way — it precedes
  ;; the content row, so it is folded first. A user fact carrying COMMAND-ID (the
  ;; start_turn that committed it, from the fact's metadata) hydrates the prompt
  ;; row the submitting shell painted under that id (INGEST-SUBMITTED-PROMPT) the
  ;; same way, so the prompt never renders twice; with no such row it appends.
  ;; RECORDED marks a user fact that opened a recorded exchange
  ;; (NLK:RECORD-EXCHANGE-TURN: a reflection's recap, a room's write-back): the
  ;; turn ran no provider round, and its finish divider says so instead of timing
  ;; it. METADATA is the fact's display envelope -- the pictures the answer
  ;; names, or those sent with the prompt -- seated on its text row, the live
  ;; stream's and replay's alike.
  ;; STEER marks the input a promoted steer started: the turn the operator cut in with.
  (cond (recorded (setf (gethash "turn_recorded" st) t))
        ((equal role "user") (remhash "turn_recorded" st)))
  (let ((content (or content "")))
    (when (and (equal role "assistant") (json-value reasoning :text))
      (hydrate-row st
                   (open-tail-row st :reasoning turn-id
                                  ;; Oldest unhydrated thinking row first, not
                                  ;; newest: a multi-round turn streams one thinking
                                  ;; span per round and commits one fact per round in
                                  ;; the same order, so oldest-first pairs each fact
                                  ;; with the span it came from instead of
                                  ;; transposing them.
                                  :oldest t)
                   :reasoning reasoning turn-id))
    (if (equal role "assistant")
        (let ((row (open-tail-row st :text turn-id :role "assistant")))
          (when (or row (plusp (length content)))
            (setf (transcript-row-metadata (hydrate-row st row :text content turn-id)) metadata)))
        (let ((row (if-let (row (and command-id
                                     (open-tail-row st :text turn-id :role "user"
                                                    :prompt-id command-id)))
                     (hydrate-row st row :text content turn-id)
                     (tail-row st :kind :text :role role :turn-id turn-id :hydrated t
                                  :text content))))
          (setf (transcript-row-prompt-id row) prompt-id
                (transcript-row-recorded row) (and recorded t)
                (transcript-row-metadata row) metadata
                (transcript-row-steer-p row) (and steer t))))))

(define-ingest ingest-submitted-prompt (st text &key prompt-id)
  "Paint the prompt the composer just sent, before the gateway has said a
word: a user :TEXT row at the tail — after every row history holds, exactly
where the durable input fact will rest — unhydrated, carrying the
start_turn's command id as its PROMPT-ID."
  ;; Retention: live-only. The durable turn.input_committed fact whose
  ;; metadata names the same command id hydrates this row in place
  ;; (INGEST-MESSAGE-FACT), so the prompt never renders twice; a rejection, a
  ;; transport refusal, a queued start, or a retraction takes it back
  ;; (DROP-SUBMITTED-PROMPT); a replay rebuilds state from durable frames
  ;; alone, so the row lives only while this shell waits to hear back.
  (tail-row st :kind :text :role "user" :retention :live-only
            :prompt-id prompt-id :text (or text "")))

(defun drop-submitted-prompt (state prompt-id)
  "Take back the unhydrated prompt row painted under PROMPT-ID
(INGEST-SUBMITTED-PROMPT): the start it announced was rejected, never
delivered, parked behind another turn, or retracted."
  ;; A row the durable fact already hydrated is history and stays. => STATE.
  (remove-transcript-rows state (lambda (row)
                                  (and prompt-id
                                       (eq :text row.kind)
                                       (equal "user" row.role)
                                       (not row.hydrated)
                                       (equal prompt-id row.prompt-id)))))

(define-ingest ingest-pending-input-fact (st content &key prompt-id turn-id
                                            steer)
  "Record a queued prompt — a follow-up awaiting promotion, or a steer
awaiting the same promotion once the running turn ends at its next round
boundary — as a :PENDING-INPUT row: queue state, not conversation."
  ;; The status block renders it under the live turn bar
  ;; (PENDING-STEER-LINES), never as a transcript block. The started
  ;; INPUT_COMMITTED that promotes the prompt carries the same prompt id and
  ;; clears this row; folding both as user messages rendered every queued
  ;; prompt twice. STEER marks a parked steer; once marked, a row stays a
  ;; steer through every snapshot that re-names it.
  ;; One row per prompt: the submitting shell paints its steer the moment it
  ;; sends it, and the queue snapshot then names the same prompt id on every
  ;; snapshot until promotion, so a prompt already in the band is updated in
  ;; place. Identity is the prompt id alone — two steers reading the same
  ;; text are two rows.
  (let ((row (or (and prompt-id
                      (find-if
                       (lambda (row)
                         (and (eq :pending-input row.kind)
                              (equal prompt-id row.prompt-id)))
                       (transcript-rows st)))
                 (tail-row st :kind :pending-input :role "user"
                           :prompt-id prompt-id :hydrated t))))
    (setf (transcript-row-text row) (or content "")
          (transcript-row-turn-id row) (or turn-id
                                           (transcript-row-turn-id row))
          (transcript-row-steer-p row) (or (and steer t)
                                           (transcript-row-steer-p row)))))

(defun clear-pending-input (state prompt-id content)
  "Remove the queued :PENDING-INPUT row that PROMPT-ID (or, for logs that
predate durable prompt identity, matching CONTENT) promoted into a started
turn. A no-op when no pending row matches."
  (when (hash-table-p state)
    (flet ((clear-from (key &aux (rows (gethash key state '()))
                                 (match (find-if
                                         (lambda (row)
                                           (and (eq :pending-input row.kind)
                                                (or (and prompt-id (equal prompt-id row.prompt-id))
                                                    (equal (or content "")
                                                           row.text))))
                                         rows)))
             (when match
               (setf (gethash key state) (remove match rows :count 1))
               t)))
      (or (clear-from "tail_rows") (clear-from "history_rows"))))
  state)

(define-ingest ingest-tool-call-fact (st call-id name arguments &key turn-id)
  "Record a durable tool call, hydrating its streamed row when one exists."
  ;; A round's calls stream in model order and their facts land in model
  ;; order (RUN-TOOL-CALL), so a fact takes the oldest streamed row of its
  ;; name: without this, the newest row would pair a round's first call with
  ;; the second call's row, and an edit's card would sit under the ls after it.
  (let ((row (or (open-tail-row st :tool-call turn-id :name (or name "") :oldest t)
                 (tail-row st :kind :tool-call :role "assistant"
                           :turn-id turn-id :tool-name (or name "")))))
    (setf (transcript-row-tool-call-id row) call-id
          (transcript-row-tool-args row) (or arguments "")
          (transcript-row-tool-status row) "running"
          (transcript-row-retention row) :persisted-history
          (transcript-row-hydrated row) t)))

(define-ingest ingest-tool-result-fact (st call-id name result
                                        &key turn-id error-p metadata)
  "Record a tool result on the row its call opened, failed when ERROR-P —
the fact's own \"failed\", read by the live stream and replay alike —
stopped when its METADATA says who stopped it (STOP-DOCUMENT), and in the
background when it handed its job off (until the exit's reducer closes it)."
  ;; The result REPLACES any streamed output rather than appending (the Zig
  ;; setTailRowText rule), so the final output never concatenates onto the
  ;; argument preview. METADATA is the fact's optional display envelope
  ;; (image cards), a JSON-object hash — carried identically by
  ;; the live stream and replay.
  ;; Deliberately NOT the shared OPEN-TAIL-ROW match: a result lands on the
  ;; row its call opened whether or not that row was already hydrated, and it
  ;; matches on call id first, falling back to the tool name only when no row
  ;; carries the id -- a later call of the round still streaming has none yet,
  ;; and it is not this one.
  (let ((row (flet ((newest (predicate)
                      (find-if (lambda (row)
                                 (and (eq :tool-call (transcript-row-kind row))
                                      (row-turn-matches-p row turn-id)
                                      (funcall predicate row)))
                               (gethash "tail_rows" st '()) :from-end t)))
               (or (and call-id
                        (newest (lambda (row) (equal call-id (transcript-row-tool-call-id row)))))
                   (newest (lambda (row)
                             (and (null (transcript-row-tool-call-id row))
                                  (equal (or name "") (transcript-row-tool-name row)))))
                   (tail-row st :kind :tool-call :role "assistant"
                             :turn-id turn-id :tool-name (or name ""))))))
    (setf (transcript-row-tool-call-id row) (or (transcript-row-tool-call-id row) call-id)
          (transcript-row-text row) (or result "")
          (transcript-row-tool-status row) (cond ((json-value metadata :object "stopped") "stopped")
                                                 (error-p "error")
                                                 ;; handed off: the job runs on, its box below
                                                 ((equal "running" (json-value metadata :string
                                                                               "background_shell" "status"))
                                                  "background")
                                                 (t "complete"))
          (transcript-row-error-p row) (and error-p t)
          (transcript-row-metadata row) (and (hash-table-p metadata) metadata)
          (transcript-row-retention row) :persisted-history
          (transcript-row-hydrated row) t)))

(define-ingest ingest-system-notice (st text &key turn-id error-p outcome metadata
                                                (retention :persisted-history))
  "Append a system notice with declared RETENTION and optional semantic METADATA.
TEXT is its full receipt; OUTCOME is the turn's end when it rested here."
  (tail-row st :kind :system-notice :role "system" :turn-id turn-id
            :hydrated t :error-p (and error-p t) :outcome outcome :text text
            :metadata metadata :retention retention))

(defun iso-8601-ms (value)
  "Milliseconds since the CL epoch for an ISO-8601 UTC string, or NIL."
  ;; Only the shape ISO-NOW writes is parsed; anything else declines.
  (when-let (seconds (iso-universal value))
    (ignore-errors
      (+ (* 1000 seconds)
         (if (and (> (length value) 22) (char= (char value 19) #\.))
             (parse-integer value :start 20 :end 23)
             0)))))

(defun format-clock-minute (epoch-ms &optional zone)
  "EPOCH-MS as the wall clock the operator reads — `11:29 AM' — this
machine's zone unless ZONE, hours west of Greenwich as DECODE-UNIVERSAL-TIME
takes it."
  ;; The minute his own wall clock reads: a terminal turn fact is stamped in
  ;; the log's zone (UTC), and a status line that answers "when did this
  ;; finish?" has to answer it in his — the twelve-hour face with its AM/PM,
  ;; never a bare 24-hour number he has to convert.
  (bind (((_ minute hour) (decode-universal-time (+ nlk:+unix-epoch+ (floor epoch-ms 1000)) zone))
         (hour12 (mod hour 12)))
    (format nil "~d:~2,'0d ~a"
            (if (zerop hour12) 12 hour12)
            minute
            (if (< hour 12) "AM" "PM"))))

(defparameter +weekday-names+ #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter +month-names+ #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun local-date (universal)
  "=> (values KEY WORDS): UNIVERSAL's date on the operator's clock, as (YEAR
MONTH DATE) and as `Wed 14 Oct'."
  (multiple-value-bind (second minute hour date month year weekday) (decode-universal-time universal)
    (declare (ignore second minute hour))
    (values (list year month date)
            (format nil "~a ~d ~a" (aref +weekday-names+ weekday) date (aref +month-names+ (1- month))))))

(defun since-ms (start &optional (now (get-internal-real-time)))
  "Milliseconds from the internal-time stamp START to NOW, never negative."
  ;; What every live clock in this shell measures: a stamp taken on one frame
  ;; against the frame asking. The durable clocks are ISO-8601-MS's instead —
  ;; elapsed there is the difference between two logged facts.
  (max 0 (truncate (* 1000 (- now start)) internal-time-units-per-second)))

(defun format-turn-elapsed (elapsed-ms &aux (total-seconds (floor elapsed-ms 1000)))
  "An elapse in units: `N.Ns` under a minute, `Nm SSs` under an hour, `Nh MMm`
beyond."
  ;; `11:05' is the Zig MM:SS, and beside the transcript's `[10:40 AM]' it
  ;; would read as a time of day.
  (cond
    ((< total-seconds 60)
     (format nil "~d.~ds" total-seconds (floor (mod elapsed-ms 1000) 100)))
    ((< total-seconds 3600)
     (format nil "~dm ~2,'0ds" (floor total-seconds 60)
             (mod total-seconds 60)))
    (t (format nil "~dh ~2,'0dm" (floor total-seconds 3600)
               (floor (mod total-seconds 3600) 60)))))

(defun turn-rest-text (outcome elapsed-ms ended-at-ms)
  "How a turn came to rest: `24.8s completed (11:29 AM)', each fact only when known."
  (format nil "~@[~a ~]~a~@[ (~a)~]" (and elapsed-ms (format-turn-elapsed elapsed-ms))
          outcome (and ended-at-ms (format-clock-minute ended-at-ms))))

(defun format-cache-hit-percent (input cached)
  "The cache hit share of one prompt bill as the meter prints it — cache
reads over input plus cache reads, half-up — at the precision the number
earns: one decimal below 99% (`93.9%' while the prefix is still growing),
two at or above (`99.42%', where the first decimal is the whole story and
the second is the news)."
  ;; The count follows the value it prints, so no `99' ever rests on one
  ;; decimal: a value whose tenth rounds up to 99.0 takes the second
  ;; (`98.99%'), and the bound holds at 100.00%.
  (let* ((cached (or cached 0))
         (prompt (+ (or input 0) cached))
         (centi (if (zerop prompt)
                    0
                    (min 10000 (floor (+ (* cached 10000) (floor prompt 2))
                                      prompt))))
         (tenths (floor (+ centi 5) 10)))
    (if (>= tenths 990)
        (format nil "~d.~2,'0d%" (floor centi 100) (mod centi 100))
        (format nil "~d.~d%" (floor tenths 10) (mod tenths 10)))))

(defun turn-finish-divider-text (outcome &key usage (elapsed-ms (getf usage :elapsed-ms))
                                              (ended-at-ms (getf usage :ended-at-ms))
                                              (provider (getf usage :provider))
                                              (model (getf usage :model))
                                              (effort (getf usage :effort))
                                              (response-model (getf usage :response-model))
                                              (finish-reason (getf usage :finish-reason)))
  "The Zig writeTurnFinishDividerText contract, scoped to the facts a resting
turn is read for — the lane it ran on, how long it took, how it ended, and the
wall clock it ended at — each read off USAGE unless given:
`• provider/model→served (high) · 24.8s completed (11:29 AM) (length)`."
  ;;
  ;; Every segment joins only when its durable fact exists — a turn with no usage
  ;; and no model collapses toward `• 24.8s completed (11:29 AM)`. The line is the
  ;; live bar at rest: the identity is STATUS-IDENTITY-LABEL plus the ` (effort)`
  ;; the turn bound (the bar's STATUS-MODEL-LABEL shape), the elapse is
  ;; FORMAT-TURN-ELAPSED over the two logged facts, and the clock is
  ;; FORMAT-CLOCK-MINUTE of the terminal fact's own occurrence — the operator's
  ;; own twelve-hour face.
  ;;
  ;; The token meter is deliberately not on it: the session's bill at session
  ;; scope is the header's right lane, and one number in two lanes is one number
  ;; too many.
  ;;
  ;; USAGE still names the lane (PROVIDER / MODEL / EFFORT) and what the provider's
  ;; own stream reported about the stop:
  ;;
  ;;   RESPONSE-MODEL   the id the stream named, shown as `requested→served` only
  ;;                    when a relay served something other than the ask
  ;;   FINISH-REASON    shown unless it is a clean stop: a truncated answer must
  ;;                    not rest as an unqualified `completed`
  (format nil "• ~@[~a · ~]~a~@[ (~a)~]"
          (usage-identity-text (list :provider provider :model model :effort effort
                                     :response-model response-model))
          ;; The clock rides in parentheses: the outcome word is what the line says,
          ;; and the moment it happened is its note.
          (turn-rest-text outcome elapsed-ms ended-at-ms)
          (and (json-value finish-reason :text)
               (not (member finish-reason '("stop" "tool_calls") :test #'string-equal))
               finish-reason)))

(defun usage-identity-text (usage &aux (requested (json-value (getf usage :model) :text))
                                       (served (json-value (getf usage :response-model) :text)))
  "What one turn's USAGE accumulator ran on, as the finish divider names it —
`provider/model→served (effort)' — or NIL when no model was named."
  ;; A stream that named a model the request did not is still an identity:
  ;; the divider would otherwise render nothing at all. The turn's reasoning
  ;; effort rides the identity — what the round ran under, provider-reported
  ;; when the stream said so. A channel card's footer reads the same words.
  (when (or requested served)
    (format nil "~a~@[→~a~]~@[ (~a)~]"
            (status-identity-label (getf usage :provider) (or requested served))
            (and requested served (not (string-equal requested served)) served)
            (json-value (getf usage :effort) :text))))

(defun usage-meter-text (usage &aux (input (getf usage :input))
                                    (output (getf usage :output))
                                    (cached (getf usage :cached))
                                    (writes (getf usage :cache-write))
                                    (reasoning (getf usage :reasoning))
                                    ;; Estimated counts carry the `~` mark: the organism's own
                                    ;; arithmetic on a cut stream, not the provider's report.
                                    (mark (if (getf usage :estimated) "~" "")))
  "The ` · ↑1.2k c93.9% w800 ↓42 r30` token meter of one turn's USAGE
accumulator, or NIL when no count exists yet."
  ;; This is the ONE meter authority: the live bar shows it while the turn
  ;; runs, and /context reads it back. The resting divider carries the lane, the
  ;; outcome and the clock, never the counts — the header's right lane holds the
  ;; session's own bill. Each note joins only when the provider reported it
  ;; (`c` cache reads as a hit percentage, FORMAT-CACHE-HIT-PERCENT deciding
  ;; the decimals — one below 99%, two at or above, so the top of the range
  ;; keeps its resolution; `w` cache writes, `r` reasoning tokens), and an
  ;; estimated accumulator marks every count `~`.
  (when (or input output)
    (format nil " · ↑~a~a~@[ c~a~]~@[ w~a~] ↓~a~a~@[ r~a~]"
            mark (format-token-count (or input 0))
            (and cached (plusp cached) (format-cache-hit-percent input cached))
            (and writes (plusp writes) (format-token-count writes))
            mark (format-token-count (or output 0))
            (and reasoning (plusp reasoning) (uiop:strcat mark (format-token-count reasoning))))))

(define-ingest ingest-turn-terminal (st kind &key turn-id detail ended-at-ms facts)
  "Fold one terminal turn fact: emit its resting row — the finish divider
carrying the durable facts the turn left (model, elapse, outcome, the wall clock
it ended at) — then settle the tail, dropping stream rows no durable fact
confirmed."
  ;; KIND is "completed", "steered" (a completion a parked steer cut short),
  ;; "failed", or "cancelled". FACTS are what a recorded exchange's completion
  ;; says about the work behind it (INGEST-TURN-COMPLETED reads them).
  ;; Elapsed is the difference between two logged facts — the turn start and
  ;; this terminal fact — never a client clock; ENDED-AT-MS is that terminal
  ;; fact's own occurrence, which the divider shows in the operator's zone.
  ;; A turn its provider was retried through leaves one muted line under its
  ;; work, ahead of how it ended; the divider rests hidden
  ;; (REDUNDANT-FINISH-DIVIDER-P), and this line carries no outcome.
  (let ((tally (gethash "turn_retries" st)))
    (when (and tally (equal turn-id (getf tally :turn-id)))
      (tail-row st :kind :finish-divider :role "system" :turn-id turn-id :hydrated t
                   :text (retry-tally-text tally))))
  (let* ((started (gethash "turn_started_at_ms" st))
         (elapsed (and started ended-at-ms (<= started ended-at-ms)
                       (- ended-at-ms started)))
         (divider (cond ((equal kind "failed")
                        ;; Rendering the durable failure is what separates "provider
                        ;; refused" from the silent stall the operator otherwise stares at.
                        ;; The text is content only: the display policy's diagnostic
                        ;; chrome ("■ Error: ") owns the framing.
                        (ingest-system-notice
                         st (format nil "turn failed: ~a" (or detail "provider error"))
                         :turn-id turn-id :error-p t :outcome kind)
                        ;; A failed turn persists no finish divider (the Zig
                        ;; transcriptStatusSummary rule: only completed and cancelled
                        ;; rest into divider rows). Its resting summary is the live bar
                        ;; in the interrupted shape — `× model · 45.2s failed (18:21)` —
                        ;; which stays until the next turn starts.
                        (setf (gethash "turn_resting_status" st)
                              (list :outcome "failed" :elapsed-ms elapsed
                                    :ended-at-ms ended-at-ms))
                        nil)
                       ((gethash "turn_recorded" st)
                        ;; A recorded exchange: the pair was written whole, with no
                        ;; provider round between its facts (RECORD-EXCHANGE-TURN).
                        (remhash "turn_recorded" st)
                        (remhash "turn_resting_status" st)
                        ;; The pair's input row learns who recorded it.
                        (when-let (row (and (getf facts :recorder)
                                            (find-if (lambda (row)
                                                       (and row.recorded (equal "user" row.role)
                                                            (row-turn-matches-p row turn-id)))
                                                     (gethash "tail_rows" st '()) :from-end t)))
                          (setf row.recorded (getf facts :recorder)))
                        ;; FACTS name the work behind the row: the divider rests on
                        ;; `recorded' and the clock the fact landed at, where
                        ;; `completed' would claim a provider round that never ran.
                        (let ((line (turn-finish-divider-text "recorded" :usage facts
                                                              :ended-at-ms ended-at-ms)))
                          (if (getf facts :recorder)
                              (format nil "• ~a · ~a" (getf facts :recorder) (subseq line 2))
                              line)))
                       (t
                        (remhash "turn_resting_status" st)
                        (turn-finish-divider-text
                         (if (member kind '("cancelled" "steered") :test #'equal) kind "completed")
                         :usage (gethash "usage" st)
                         :elapsed-ms elapsed :ended-at-ms ended-at-ms)))))
    ;; The persisted resting row a finished turn leaves behind — the Zig finish_divider.
    (when divider
      (tail-row st :kind :finish-divider :role "system" :turn-id turn-id
                :hydrated t :outcome kind :text divider))
    (remhash "turn_started_at_ms" st)
    (remhash "retry_status" st)
    (remhash "turn_retries" st)
    (settle-tail-rows st)))

;;; --- durable event methods ------------------------------------------------
;;; Each method is a one-line delegation to a named ingest function so a
;;; mutation (or a test) can redefine and faithfully restore a reducer
;;; without hand-copying its body.

(define-ingest ingest-context-eviction (st &key boundary-position log-position
                                           turn-count message-count
                                           retained-message-count)
  "Fold one session.context.evicted fact: dim every row from the evicted
range (log positions at or below BOUNDARY-POSITION), rest the Zig fold-seam
divider at the boundary — between the evicted rows and the retained tail,
never at the fact's own arrival position — and append the fact's own counts
divider row (the Zig writeContextEvictionDividerText copy, one line per
eviction fact, resting at the fact's arrival position) from TURN-COUNT /
MESSAGE-COUNT / RETAINED-MESSAGE-COUNT."
  (let ((divider (make-transcript-row
                  :kind :eviction-divider :role "system" :hydrated t
                  :log-position log-position
                  :text "• context retained from here · dimmed messages above were evicted")))
    (flet ((evicted-p (row)
             (and row.log-position boundary-position
                  (<= row.log-position boundary-position))))
      (dolist (row (transcript-rows st))
        (when (evicted-p row)
          (setf (transcript-row-evicted-p row) t)))
      ;; Retire any earlier divider: the floor is monotone, one boundary.
      ;; The new one rests just past a lane's last evicted row, the tail
      ;; lane's before the history lane's.
      (remove-transcript-rows st (lambda (row) (eq :eviction-divider row.kind)))
      (flet ((rest-divider (key &aux (rows (gethash key st '()))
                                     (index (position-if #'evicted-p rows :from-end t)))
               (when index
                 (setf (gethash key st)
                       (append (subseq rows 0 (1+ index)) (list divider)
                               (subseq rows (1+ index)))))))
        (or (rest-divider "tail_rows")
            (rest-divider "history_rows")
            ;; No folded row precedes the boundary (a fresh attach): the
            ;; divider still records that older context exists above.
            (push divider (gethash "history_rows" st)))))
    ;; The durable per-fact counts divider (Zig appendContextEvictionDividerRow):
    ;; one system line per eviction fact at its arrival position, kept across
    ;; later evictions — only the fold-seam divider above is retired and
    ;; re-rested when the monotone floor advances.
    (tail-row st :kind :finish-divider :role "system" :hydrated t
              :log-position log-position
              :text (format nil "• evicted ~d turn~:p · ~d message~:p · ~d retained · recall: see log"
                            (or turn-count 0) (or message-count 0)
                            (or retained-message-count 0)))))

(defun stamp-unpositioned-rows (state position)
  "Stamp POSITION onto every row the fold has not yet attributed — called
after each event folds, so rows carry the durable position that produced
them (the eviction divider's placement input)."
  ;; The lanes are walked where they stand: TRANSCRIPT-ROWS conses a copy of
  ;; the whole history, and this runs once per folded fact.
  (when (and (hash-table-p state) (integerp position))
    (dolist (lane '("history_rows" "tail_rows"))
      (dolist (row (gethash lane state '()))
        (unless row.log-position
          (setf row.log-position position)))))
  state)

(defmacro define-fold (class ingest (&rest arguments) &rest fields)
  "Define the APPLY-EVENT method for CLASS: fold the event into STATE through
INGEST, called with ARGUMENTS positionally and FIELDS as keyword pairs."
  ;; A symbol is read off the event by CLASS's own reader prefix -- or by the
  ;; prefix string CLASS was written with, (CLASS "PREFIX"), for the events
  ;; whose readers drop the -EVENT tail -- and anything else is a form, read
  ;; with EVENT bound. The reducer entry points are one shape: unpack the
  ;; event through its readers, hand the values to the ingest function that
  ;; owns the fact. Written out, the plumbing ran longer than the ingest
  ;; bodies it feeds. A bare symbol among FIELDS is its own pair: TEXT stands
  ;; for (:TEXT TEXT), the keyword the ingest takes named like the reader.
  (let* ((name (if (consp class) (first class) class))
         (prefix (if (consp class) (second class) (symbol-name name))))
    (flet ((reader (spec)
             (if (symbolp spec)
                 (list (intern (format nil "~a-~a" prefix (symbol-name spec))
                               (symbol-package name))
                       'event)
                 spec)))
      `(defmethod apply-event (state (event ,name))
         (,ingest state ,@(mapcar #'reader arguments)
                  ,@(loop for (key spec) in (mapcar #'alexandria:ensure-list fields)
                          collect (if spec key (alexandria:make-keyword key))
                          collect (reader (or spec key))))))))

(define-fold (nlk:context-evicted-event "CONTEXT-EVICTED") ingest-context-eviction ()
  (:boundary-position end-sequence) (:log-position (nlk:event-log-position event))
  turn-count message-count retained-message-count)

(defun context-occupancy-tokens (usage)
  "Prompt tokens the newest provider round placed in the context window:
input + cache reads + cache creation (the Zig contextUsedTokens rule)."
  ;; Cache-creation tokens are prompt tokens too — providers report them
  ;; disjoint from input, but they occupy the window all the same.
  (+ (or (getf usage :input) 0)
     (or (getf usage :cached) 0)
     (or (getf usage :cache-write) 0)))

(defun usage-plus (totals counts)
  "The plist COUNTS with each count added to TOTALS' count under its key."
  ;; NIL is "the provider did not say": it adds nothing, never zeroes.
  (loop for (key addend) on counts by #'cddr
        nconc (list key (let ((sum (getf totals key)))
                          (cond ((not (integerp addend)) sum)
                                ((integerp sum) (+ sum addend))
                                (t addend))))))

(define-ingest ingest-turn-usage (st &key input output cached total model provider
                                       effort reasoning cache-write
                                       response-model finish-reason
                                       cost-known cost-usd estimated)
  "Fold ONE PROVIDER ROUND's usage into the three slots a session's cost is
read from."
  ;; A turn is a loop: every tool round bills its own prompt and its own
  ;; completion, so the last round's numbers are not what the turn cost, and
  ;; the turn's summed prompt is not what occupies the context. Each slot
  ;; answers exactly one question:
  ;;
  ;;   "usage"          this turn, summed over its rounds — the finish divider
  ;;                    and the live token meter. TURN-STARTED retires it, so a
  ;;                    turn never inherits the previous turn's bill.
  ;;   "session_usage"  every round of the session, summed — the header's global
  ;;                    ↑/↓ lane. Nothing inside a session resets it.
  ;;   "context_usage"  the newest round alone — what actually sits in the
  ;;                    context window, which the summed prompt would overstate
  ;;                    once per round (the Zig final_round_* rule).
  ;;
  ;; Identity facts (PROVIDER, MODEL, EFFORT, and what the stream itself
  ;; reported in RESPONSE-MODEL / FINISH-REASON) are last-fact-wins on the
  ;; turn slot: they describe the round that just ended, and the round that
  ;; ends last is the one the divider reports. A round silent about an
  ;; identity keeps the previous round's. Absent numbers stay NIL (never
  ;; invent a zero).
  ;;
  ;; ESTIMATED marks a round whose counts the organism estimated (a cancel cut
  ;; the stream before the provider reported). It is sticky on the turn slot —
  ;; one estimated round makes the turn's sum an estimate — and the renderers
  ;; mark such sums (`↑~1.1k`). The session bill still folds the numbers: the
  ;; spend was real, and an estimate is a better bill than a zero.
  (flet ((plus-round (totals)
           "TOTALS (a usage plist) plus this round's counts; the cost joins
only when the gateway supplied authoritative pricing."
           (append (usage-plus totals (list :input input :output output :cached cached
                                            :total total :reasoning reasoning
                                            :cache-write cache-write))
                   (list :cost-known (or (getf totals :cost-known) cost-known)
                         :cost (let ((cost (getf totals :cost)))
                                 (if (and cost-known (numberp cost-usd))
                                     (+ (if (numberp cost) cost 0.0) cost-usd)
                                     cost))))))
    (let ((turn (gethash "usage" st)))
      (setf (gethash "usage" st)
            (append (plus-round turn)
                    (list :rounds (1+ (or (getf turn :rounds) 0))
                          :estimated (or (getf turn :estimated)
                                         (and estimated t)))
                    (loop for (key value) on (list :model model :provider provider
                                                   :response-model response-model
                                                   :finish-reason finish-reason :effort effort)
                            by #'cddr
                          nconc (list key (or (json-value value :text) (getf turn key)))))))
    (setf (gethash "session_usage" st)
          (plus-round (gethash "session_usage" st)))
    (setf (gethash "context_usage" st)
          (list :input input :cached cached :cache-write cache-write
                :model (or (json-value model :text)
                           (getf (gethash "context_usage" st) :model)))))
  ;; The round's thinking row takes its reasoning tokens: the newest tail
  ;; row past the round's own answer text, else the round thought nothing.
  (let ((row (find-if-not #'assistant-text-p (gethash "tail_rows" st '()) :from-end t)))
    (when (and (integerp reasoning) row (eq :reasoning row.kind) (null row.reasoning-tokens))
      (setf row.reasoning-tokens reasoning))))

(define-ingest begin-turn-usage (st)
  "Retire the previous turn's accumulator so the token meter and the finish
divider count THIS turn's rounds."
  ;; The session bill and the context snapshot survive: one is what the
  ;; session has cost, the other is what still occupies the window — neither
  ;; is a property of the turn that just started.
  (remhash "usage" st))

(define-ingest ingest-turn-completed (st done)
  "Count completed turns, track the last message count, and settle the turn."
  (incf (gethash "turns_completed" st 0))
  (setf (gethash "last_message_count" st)
        done.message-count)
  (ingest-turn-terminal st (if done.steered "steered" "completed")
                        :turn-id (nlk::event-turn-id done)
                        :ended-at-ms (iso-8601-ms
                                      (nlk:event-occurred-at done))
                        :facts (list :recorder done.recorder :provider done.provider
                                     :model done.model :elapsed-ms done.elapsed-ms
                                     :input done.input-tokens :cached done.cached-input-tokens
                                     :output done.output-tokens :reasoning done.reasoning-tokens)))

(defun continued-part (text chars)
  "TEXT from CHARS on — the continuation's own part of a merged round
string — or TEXT whole when CHARS is not a usable offset."
  ;; The sealed prefix already rests in the transcript as the voided attempt's
  ;; block.
  (if (and (stringp text) (integerp chars) (< 0 chars (length text)))
      (subseq text chars)
      text))

(defun ingest-message-event (state event &aux (payload (nlk::message-event-payload event)))
  "Fold one conversational message fact: an input by the disposition that
admitted it, or a round's answer."
  ;; CONTENT is the text of a bare string or of the wire message's content
  ;; parts, so a multi-part input projects its text rather than "".
  ;;
  ;; A follow-up disposition is queue state, not conversation: its promotion
  ;; commits a second INPUT_COMMITTED (disposition "started") for the same
  ;; prompt. It folds as a pending row the promotion clears — never as a
  ;; second user message. A promotion is the conversation row for a prompt
  ;; the pending band showed — a follow-up's or a parked steer's: the band
  ;; row retires and the message lands at the tail, as the first input of
  ;; the turn it starts. The input's command id names the start_turn that
  ;; committed it: the submitting shell painted its prompt row under it at
  ;; Enter.
  ;;
  ;; A round that continued a cut attempt carries the merged message; the
  ;; sealed prefix already rests in the transcript as the voided block, so
  ;; only the continuation hydrates.
  (let* ((disposition (json-value payload :string "disposition"))
         (prompt-id (json-value payload :string "prompt-id"))
         (content (or (nlk:fact-message-content payload) ""))
         (answer (typep event 'nlk::assistant-message))
         (role (if answer "assistant" (or (json-value payload :string "role") "user"))))
    (when (equal disposition "follow-up")
      (return-from ingest-message-event
        (ingest-pending-input-fact state content :prompt-id prompt-id :turn-id event.turn-id)))
    (when (equal disposition "started")
      (setf state (clear-pending-input state prompt-id content)))
    (ingest-message-fact state role
                         (continued-part content
                                         (and answer (nlk::assistant-message-continued-content-chars
                                                      event)))
                         :reasoning (continued-part
                                     (nlk::message-event-reasoning event)
                                     (and answer
                                          (nlk::assistant-message-continued-reasoning-chars event)))
                         :turn-id event.turn-id :prompt-id prompt-id
                         :command-id (nlk:event-command-id event)
                         :recorded (equal disposition "recorded")
                         :steer (and (equal disposition "started")
                                     (json-value payload :boolean "steer"))
                         :metadata (nlk::message-event-metadata event))))

(defmethod apply-event (state (event nlk:turn-completed))
  (ingest-turn-completed state event))

(define-fold nlk:turn-failed ingest-turn-terminal ("failed")
  (:turn-id (nlk::event-turn-id event)) detail
  (:ended-at-ms (iso-8601-ms (nlk:event-occurred-at event))))

(define-fold nlk:turn-cancelled ingest-turn-terminal ("cancelled")
  (:turn-id (nlk::event-turn-id event))
  (:ended-at-ms (iso-8601-ms (nlk:event-occurred-at event))))

(define-ingest ingest-turn-started (st occurred-at &key turn-id wake)
  "Stamp the durable start time OCCURRED-AT: the finish divider's elapsed
segment is the difference between two logged facts, never a client clock.
A turn no one typed opens on a notice naming WAKE, the exits that woke it."
  ;; A new turn also retires the previous turn's resting failed summary — the
  ;; Zig live bar reflects one lifecycle at a time — and its usage
  ;; accumulator, so this turn's divider bills this turn's rounds.
  (remhash "turn_resting_status" st)
  (begin-turn-usage st)
  (setf (gethash "turn_started_at_ms" st) (iso-8601-ms occurred-at))
  (when (plusp (length wake))
    (ingest-wake-notice st wake turn-id)))

(define-ingest ingest-wake-notice (st wake turn-id)
  "Open TURN-ID, a turn an exit woke, on the line that says which: `woke:
eval 1005 exited 2'. The row's metadata carries WAKE's entries, each with
how it ended (EXEC-END), for a surface that draws the words itself."
  ;; Its own row, not an operator's line: the turn has no input fact, and no
  ;; reader may show one the operator did not write (NLK:ADMIT-WAKE-TURN).
  (let ((entries (map 'vector (lambda (entry) (copy-json-object entry "end" (exit-end entry)))
                      (remove-if-not #'hash-table-p wake))))
    (tail-row st :kind :system-notice :role "system" :turn-id turn-id :hydrated t
                 :metadata (make-json-object "wake" entries)
                 :text (format nil "woke: ~{~a~^, ~}"
                               (loop for entry across entries
                                     collect (format nil "eval ~a ~a"
                                                     (json-value entry :integer "eval_id")
                                                     (exit-end entry t)))))))

(define-fold nlk:turn-started ingest-turn-started ((nlk:event-occurred-at event))
  (:turn-id (nlk::event-turn-id event)) wake)

(defmethod apply-event (state (event nlk::message-event))
  (ingest-message-event state event))

(define-fold nlk:tool-call-event ingest-tool-call-fact (call-id tool-name arguments)
  (:turn-id (nlk::event-turn-id event)))

(define-fold nlk:turn-usage-event ingest-turn-usage ()
  (:input input-tokens) (:output output-tokens) (:cached cached-input-tokens)
  (:total total-tokens) (:reasoning reasoning-tokens)
  (:cache-write cache-write-tokens) model provider response-model finish-reason
  (:effort reasoning-effort) cost-known cost-usd
  estimated)

(define-fold nlk:tool-result-event ingest-tool-result-fact (call-id tool-name result)
  (:turn-id (nlk::event-turn-id event)) (:error-p failed) metadata)

(define-fold nlk::attempt-voided-event ingest-attempt-voided-fact ()
  attempt reasoning text detail stream-duration-ms disposition
  (:turn-id (nlk::event-turn-id event)))

(define-fold nlk:provider-retry-event ingest-provider-retry-fact ()
  delay-seconds status provider detail (:turn-id (nlk::event-turn-id event)))

;;; --- fold ---------------------------------------------------------------
;;; fold-log walks the event log page by page and reduces it through apply-event,
;;; so the fold is off-heap regardless of log length.

(defun fold-log (&key session-id (after 0) (limit 100) seed visible-set &aux (state seed)
                                                                             (cursor after))
  "Fold all matching events through APPLY-EVENT, starting from SEED."
  ;;
  ;; Walks the log page by page so the fold is off-heap regardless of log length.
  ;; VISIBLE-SET, when supplied, restricts turn-bearing events to that head chain
  ;; (NLK:EVENT-VISIBLE-P); without it the whole log folds, as before.
  ;; Returns the final state.
  (loop
    (let ((page (nlk:page-events :session-id session-id :after cursor :limit limit)))
      (when (null page) (return state))
      (dolist (event page)
        (when (or (null visible-set)
                  (nlk:event-visible-p event visible-set))
          (setf state (stamp-unpositioned-rows (apply-event state event) event.log-position))))
      (setf cursor (nlk:event-log-position (first (last page)))))))

;;; --- projection ---------------------------------------------------------
;;; project-state converts reduced state into display blocks. Each block declares
;;; its retention horizon: persisted-history, live-only, or debug-only.

(define-record (display-block (:export lines retention))
  "One rendered transcript unit with a declared retention horizon and its row
identity, which drives the transcript's vertical rhythm."
  ;; PLAIN-LINES is the style-free text parallel to LINES (mouse
  ;; hit-test/selection source); ROW is the TRANSCRIPT-ROW that produced the
  ;; block, a TOOL-GROUP for a folded run of calls, or NIL for synthetic
  ;; blocks. REFS, when non-NIL, is parallel to LINES and names the owner of
  ;; each line where one block holds several — an open group's member cards.
  ;; FILL is the background tone the block paints behind itself — non-NIL only
  ;; for the filled block styles, whose backgrounds must not touch across a
  ;; boundary.
  (lines nil :type list)
  (retention :live-only :type (member :persisted-history :live-only :debug-only))
  (kind :text)
  (plain-lines nil :type list)
  (row nil)
  (refs nil :type list)
  (fill nil))

(nlk:access (block display-block) (object display-block))

(defmethod print-object ((object display-block) stream)
  "Identity, never the graph: ROW can be a TOOL-GROUP whose memo holds this
block, so the default structure print never ends."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~d line~:p" object.kind (length object.lines))))

(defun trim-block-text (text)
  "TEXT with outer whitespace stripped — the Zig transcriptMessageBody outer
trim (render_plan.zig), widened here to every block kind: body content that
arrives wrapped in blank lines or padding spaces must not out-pad the block
padding policy that owns the transcript's vertical rhythm."
  (nlk:trimmed (or text "")))

