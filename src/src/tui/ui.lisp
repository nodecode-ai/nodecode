;;;; ui.lisp --- mutable reducer, projection, and viewport. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The mutable material: apply-event dispatch, fold, projection to display
;;;; blocks, and viewport. The log is kernel-owned and locked; this layer
;;;; interprets it and may be rewritten by a mutation without touching the log.
;;;;
;;;; Why apply-event stays out of the kernel: package locks do not protect
;;;; generics. A locked generic can still receive new methods via defmethod, so
;;;; placing the reducer in the kernel would look protected and not be. The log
;;;; writers are plain functions (protected), and the reducer is a mutable
;;;; generic (explicitly rewritable).

(in-package #:nodecode.evolved)

(nlk:access (image nlt::terminal-image) (msg nlt::mouse-event) (program nlt::program)
            (record nlk::cell) (ta editor) (token syntax-token) (verb nlk::verb)
            (done nlk:turn-completed) (band bg-band-row))

;;; --- reducer ------------------------------------------------------------
;;; apply-event is a CLOS generic dispatching on event class. The default method
;;; is identity, so unknown kinds are safe: a mutation introducing a new event
;;; kind does not crash the organism during replay.

(defgeneric apply-event (state event)
  ;; The default method is identity, so unknown event kinds are safe.
  (:documentation
   "Update STATE with the contribution of EVENT and return the new state.")
  (:method (state event)
    (declare (ignore event))
    state))

;;; --- transcript rows ------------------------------------------------------
;;; The chronology contract, distilled from the Zig TUI's three-layer
;;; DisplayBlock flow (src/tui/model/transcript_row.zig): a semantic row with
;;; kind/role/retention identity, a two-lane store (bounded mutable tail
;;; window + promoted history), and coalescing rules that give every streamed
;;; item and its later durable fact one row instead of two. Reducers ingest
;;; rows and the display policy renders them. One row per tool call: the
;;; streamed arguments, the durable call fact, and the result all reduce onto
;;; the same row, keyed by call id, exactly as the Zig model coalesces tool
;;; phases.
;;;
;;; Rows grow oldest-last, matching the event log's forward order. State is
;;; rebuilt per fold rather than shared: FOLD-IS-DETERMINISTIC folds one log
;;; twice and compares, and a shared tail would make the second fold observe
;;; the first one's accumulation.

(defstruct transcript-row
  "One transcript item with declared identity and retention."
  (kind :text :type (member :text :reasoning :tool-call :system-notice
                            :finish-divider :pending-input
                            :eviction-divider))
  (role "assistant" :type string)
  (retention :persisted-history
             :type (member :persisted-history :live-only :debug-only))
  (turn-id nil)
  ;; Queued-prompt identity: a :PENDING-INPUT row carries its prompt id so
  ;; the promotion fact (same prompt, disposition "started") can clear it.
  (prompt-id nil)
  ;; Queued-prompt disposition: T for a parked steer, which promotes ahead
  ;; of every follow-up queued before it (NLK:DEQUEUE-NEXT-TURN) — so the
  ;; band's Esc hint sits on the row that would actually run next. On the
  ;; user row a promoted steer opened, T too: that turn is the steer's.
  (steer-p nil)
  ;; On the user row that opened a recorded exchange (NLK:RECORD-EXCHANGE-TURN),
  ;; whose input was not asked here: T from its input fact, then the recorder
  ;; its completion names -- a reflection, a frame, the agent an import read.
  (recorded nil)
  ;; The durable log position that produced this row, stamped by the fold —
  ;; the eviction divider's placement and dimming discriminator.
  (log-position nil)
  ;; T once a context eviction dropped this row's turn from the provider
  ;; context; it renders dimmed above the eviction divider.
  (evicted-p nil)
  ;; Provider tool-call index while streaming; the wire carries no call id
  ;; until the durable turn.tool_call_started fact hydrates the row.
  (stream-index nil)
  (tool-call-id nil)
  (tool-name nil)
  (tool-args nil)
  (tool-status nil)
  ;; The exited background-shell lifecycle code stamped by the shared exit
  ;; reducer (live watcher frame and durable exit fact alike); NIL for calls
  ;; that never backgrounded.
  (tool-exit-code nil)
  ;; The durable tool_result display-fact envelope (a JSON-object hash, or
  ;; NIL): "image" marks image results. A message's envelope rides its text
  ;; row: "images", the pictures an answer names or a prompt was sent with.
  ;; One slot for both the live stream and replay — the same fact carries it
  ;; on both paths.
  (metadata nil)
  ;; T once a durable fact owns this row's content. Unhydrated rows are
  ;; stream ephemera: they render live but are dropped when the turn settles,
  ;; because the event log is the only authority on history.
  (hydrated nil)
  ;; Non-NIL once the attempt that streamed this row failed: what the
  ;; operator watched arrive is kept, dimmed, never coalesced onto again.
  ;; T while only the live reset has said so; the label string once the
  ;; durable turn.attempt_voided fact has sealed the row (the fact owns the
  ;; content, so the row survives the settle).
  (voided nil)
  (error-p nil)
  ;; On the row a turn rested on -- its finish divider, or the notice its
  ;; failure left -- the terminal fact's kind: "completed", "steered", "failed"
  ;; or "cancelled". A client reads how the turn ended here, never off the
  ;; row's words.
  (outcome nil)
  ;; The live thinking clock of a streamed :REASONING row: the internal real
  ;; time its first delta opened it, and how long it thought once the stream
  ;; moved past it. Live-only — a replayed row has neither, since the log
  ;; does not time a thought.
  (started-at nil)
  (thought-ms nil)
  ;; The reasoning tokens the round that produced a :REASONING row reported
  ;; on its turn.usage fact, or NIL when the provider did not say. Replay
  ;; reads the same fact, so the count survives the fold.
  (reasoning-tokens nil)
  ;; Streaming text-growth revision. Stream deltas append into TEXT in place
  ;; (an adjustable fill-pointer string) and bump this counter — the one
  ;; sanctioned exception to the reducers-replace-field-values rule, so the
  ;; render caches key on (text identity, text-rev) instead of text identity
  ;; alone. Hydration and every non-streaming path still replace TEXT whole.
  (text-rev 0 :type fixnum)
  (text "" :type string))

(nlk:access (first-user transcript-row) (item transcript-row) (ref transcript-row)
            (row transcript-row))

(defun stream-text-buffer (seed &optional (extra 256))
  "A fresh adjustable fill-pointer string holding SEED with room to grow —
the in-place append buffer streaming rows carry as their text."
  (let ((buf (make-array (max 128 (+ (length seed) extra))
                         :element-type 'character
                         :adjustable t :fill-pointer (length seed))))
    (replace buf seed)
    buf))

(defun transcript-rows (state)
  "All rows in display order: promoted history first, then the live tail."
  (when (hash-table-p state)
    (append (gethash "history_rows" state '())
            (gethash "tail_rows" state '()))))

(defun find-transcript-row (predicate state)
  "The first row in display order PREDICATE answers, or NIL: TRANSCRIPT-ROWS
read in place, for a question asked every frame -- the copy it hands out is
the whole history."
  (when (hash-table-p state)
    (or (find-if predicate (gethash "history_rows" state '()))
        (find-if predicate (gethash "tail_rows" state '())))))

(defun remove-transcript-rows (state predicate)
  "Drop every row PREDICATE answers from both of STATE's lanes. => STATE."
  (when (hash-table-p state)
    (dolist (key '("tail_rows" "history_rows"))
      (setf (gethash key state) (remove-if predicate (gethash key state '())))))
  state)

(defun append-tail-row (state row)
  "Append ROW to the tail window, promoting the oldest row on overflow."
  ;; History grows in place: the lane is the state's alone (TRANSCRIPT-ROWS
  ;; hands out a copy), and a fresh copy per promoted row made a long
  ;; replay's fold quadratic in its rows.
  (let ((tail (append (gethash "tail_rows" state '()) (list row))))
    (loop while (> (length tail) 16) ; the Zig max_tail_rows bound
          do (setf (gethash "history_rows" state)
                   (nconc (gethash "history_rows" state '())
                          (list (pop tail)))))
    (setf (gethash "tail_rows" state) tail))
  row)

(defun tail-row (state &rest fields)
  "A fresh transcript row of FIELDS, appended to STATE's tail window. => the row."
  ;; Opening a row is one call: every fact that has something new to show says
  ;; what the row IS, never that it is built and then appended.
  (append-tail-row state (apply #'make-transcript-row fields)))

(defun last-tail-row (state)
  "STATE's newest tail row of the conversation. A queued prompt's
:PENDING-INPUT row is queue state: the answer streaming when it was queued
goes on in the row it was streaming into, not in a second one."
  (find :pending-input (gethash "tail_rows" state '())
        :key #'transcript-row-kind :test-not #'eq :from-end t))

(defun assistant-text-p (row)
  "Whether ROW is the assistant's own words, an answer: not a thought, a call or a notice."
  (and (transcript-row-p row) (eq :text row.kind) (equal "assistant" row.role)))

(defun row-turn-matches-p (row turn-id)
  "Whether ROW belongs to TURN-ID."
  ;; A row or a caller without a turn id matches anything: the local
  ;; in-process stream has no turn id until the fold hydrates it.
  (or (null turn-id) (null row.turn-id) (equal turn-id row.turn-id)))

(defun settle-tail-rows (state)
  "Promote hydrated tail rows into history and drop stream rows no durable
fact confirmed."
  ;; Runs when a turn reaches a terminal fact, and when the local UI observes
  ;; completion outside the fold.
  (when (hash-table-p state)
    (setf (gethash "history_rows" state)
          (nconc (gethash "history_rows" state '())
                 (remove-if-not #'transcript-row-hydrated
                                (copy-list (gethash "tail_rows" state '()))))
          (gethash "tail_rows" state) '()))
  state)

(defmacro define-ingest (name (st &rest args) &body body)
  "Define the reducer NAME."
  ;; ST is bound to STATE, or to a fresh reducer table when the caller had
  ;; none; BODY runs for effect and ST is the return value — the three-line
  ;; envelope every ingest function otherwise writes by hand.
  (let ((state (gensym "STATE"))
        (doc (when (and (stringp (first body)) (rest body)) (pop body))))
    `(defun ,name (,state ,@args)
       ,@(when doc (list doc))
       (let ((,st (or ,state (make-hash-table :test #'equal))))
         ,@body
         ,st))))

(defun open-tail-row (state kind turn-id &key role name index prompt-id oldest
                                              voided)
  "The unhydrated tail row of KIND that a delta or fact for TURN-ID coalesces
onto — newest first, or OLDEST first where the pairing rule needs it."
  ;; ROLE, NAME, INDEX and PROMPT-ID narrow the match to a row's speaker, its
  ;; accumulated tool name, the provider stream index the deltas carried, or
  ;; the prompt identity a submitted prompt row was painted under. A voided
  ;; row is claimable only by the attempt_voided fact that seals it: VOIDED
  ;; :ANY does not care, and the default skips them.
  (flet ((match (row)
           (and (eq kind row.kind)
                (not row.hydrated)
                (or (eq voided :any) (not row.voided))
                (or (null role) (equal role row.role))
                (or (null name) (equal name row.tool-name))
                (or (null index) (eql index (transcript-row-stream-index row)))
                (or (null prompt-id) (equal prompt-id row.prompt-id))
                (row-turn-matches-p row turn-id))))
    (find-if #'match (gethash "tail_rows" state '()) :from-end (not oldest))))

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

;;; --- display policy: logical lines ----------------------------------------
;;; Row semantics to renderable rows in two steps, mirroring the Zig layers:
;;; a row becomes LOGICAL LINES (text + styled spans + gutter role), and the
;;; layout pass wraps each logical line at the frame width through the one
;;; line-break authority (theme.lisp BREAK-FRAGMENTS), painting gutters and
;;; hanging indents exactly as src/tui/body_layout.zig and transcript.zig do.

(defstruct (mspan (:constructor mspan (start end &key tone bold italic dim
                                            underline strike hidden link ground glint)))
  "A styled span over a logical line, offsets in characters."
  ;; HIDDEN spans are markdown marker bytes that stay in the text but never
  ;; paint, nor count where the line breaks. LINK, when set, is the URI the
  ;; painted span wraps in an OSC-8 hyperlink and a click on it opens
  ;; (OPEN-TARGET): an http(s) page, or a file: target naming a file or a
  ;; folder. GROUND is a tone whose background the span stands on, a strong
  ;; run's chip; GLINT, the internal time it arrived at when a glint crosses
  ;; it then (GLINT-OVERLAY).
  start end tone bold italic dim underline strike hidden link ground glint)

(nlk:access (moved mspan) (span mspan))

(defmacro copy-with (type object &rest slots &aux (copy (gensym "COPY")))
  "A copy of OBJECT, a TYPE structure, with SLOTS -- slot keywords and value
forms, evaluated in order -- stored on the copy. => the copy."
  ;; The painted lines and spans are shared across frames, so a variant of
  ;; one is a copy carrying the change, never the shared one written over.
  `(let ((,copy (,(alexandria:symbolicate 'copy- type) ,object)))
     (setf ,@(loop for (slot value) on slots by #'cddr
                   collect `(,(alexandria:symbolicate type '- slot) ,copy) collect value))
     ,copy))

(defun shifted-span (span offset)
  "A copy of SPAN moved OFFSET characters along its line."
  (copy-with mspan span :start (+ offset span.start) :end (+ offset span.end)))

(defstruct lline
  "One logical line ready for layout: body text, base style, spans, gutter."
  (text "" :type string)
  (tone :foreground)
  (bold nil) (italic nil) (dim nil) (underline nil)
  (spans '())
  (role :prose)
  (gutter-tone nil)
  (gutter-italic nil)
  (hang 0)
  ;; A SEG, or a list of them, each continuation row paints at the head of
  ;; its hang, over as many of the hang's blanks as it is wide: a quote's
  ;; bars run down every row the line wraps onto, a code block's edge.
  (hang-mark nil)
  ;; What the line's rows stand on, (TONE FROM TO FADE): TONE's background
  ;; from column FROM below TO -- an integer the first row's, :TEXT two past
  ;; each row's words, :EDGE the text's right edge -- then FADE cells of it
  ;; going out into the canvas (GROUND-SEGS).
  (ground nil)
  ;; A SEG standing at the right end of the line's first row, which wraps
  ;; short of it by a two-column gap: a message's clock (LLINE-ASIDE-SHOWN).
  (aside nil)
  (fill nil)
  ;; Painted under a shadow: every glyph of the row — text, spans, gutter —
  ;; at *SHADOW-PERCENT* of the light, the canvas beneath them untouched
  ;; (SHADOW-STYLE). The Zig cell-level shadow pass, carried on the lline so
  ;; LLINE-RENDER keys on it.
  (shadow nil)
  (rule nil)
  ;; A table's row: its columns are laid out to the row, so a row cut short
  ;; for an aside would break them (STAMP-TRANSCRIPT-LLINES).
  (grid nil)
  ;; On the first row of an inline image band: the image fact drawn over it
  ;; (IMAGE-BAND-LLINES), which is how the placement finds the band.
  (image nil))

(nlk:access (b lline) (fresh lline) (line lline) (title lline))

;;; Developer preserves grouped routine calls and output previews. Standard
;;; keeps live calls as name-plus-comment disclosures. At a terminal fact,
;;; its tool/thought trail folds to one clickable `Ran N commands' summary.
;;; Both keep thoughts folded; there is no global open-all mode.
(defparameter *tool-detail* :standard
  "Render-scope viewport mode, :DEVELOPER or :STANDARD.")

;;; RENDER-STATE binds it from the model's EXPANDED-TOOL-CALL slot (the Zig
;;; expandedToolCallId accordion). It opens what it names whatever the detail
;;; level.
(defvar *expanded-tool-call* nil
  "Render-scope accordion state: the expand key of the one clicked-open
card, group or thought, or NIL.")

;;; --- fades -------------------------------------------------------------
;;; A place on the screen that shows one thing at a time -- a turn's words
;;; under its run, the finished thought under the live bar -- hands it to a
;;; newer thing by a fade: what stood there goes out into the canvas, then
;;; the newcomer comes up out of it. The light is the frame's paint
;;; (FADE-OVERLAY), cell by cell over the rows the frame shows, so no row
;;; is laid out again while it moves; the swap, once the old thing has gone
;;; out, is a render (FADE-DUE-P).

(defvar *fades* nil
  "T while rows lay out for a shell painting on a terminal (WITH-RENDER-SCOPE):
places fade there, and nowhere else -- a test or the image's warm-up lays
out what the state holds now.")

(defstruct (fade (:constructor make-fade (shown out in &aux (at (get-internal-real-time)))))
  "One place's fade: SHOWN the thing that holds it, LEAVING what stood there
before it, going out from AT from FROM percent of its light, at the pace
that takes all of it out in OUT milliseconds, SHOWN then coming up over IN
-- all at once, or with a STEP, its TOKENS one after another, each over IN,
a STEP behind the one before it: in reading order, or in ORDER's, a vector
giving each token the slot it comes up in."
  shown (leaving nil) (from 100) at out in (step 0) (tokens 1) (order nil))

(nlk:access (fade fade))

(defun fade-fall (fade)
  "Milliseconds what is leaving FADE's place takes to go out: OUT from all
of its light, less from less, so a thing overtaken before it was up goes at
once and its newcomer never waits on the dark."
  (if fade.leaving (ceiling (* fade.out fade.from) 100) 0))

(defun fade-on (fade &optional (now (get-internal-real-time)))
  "What FADE's place shows at NOW: what is leaving, while it goes out, then
what holds it."
  (if (and fade.leaving (< (since-ms fade.at now) (fade-fall fade))) fade.leaving fade.shown))

(defun fade-rise (fade)
  "Milliseconds what holds FADE's place takes to come all the way up."
  (+ fade.in (* fade.step (max 0 (1- fade.tokens)))))

(defun fade-light (fade item &optional (now (get-internal-real-time)) token)
  "How much of its light, in percent, ITEM stands at in FADE's place at NOW:
the whole of it, or its TOKENth token when its tokens come up one by one."
  (let ((ms (since-ms fade.at now)))
    (cond ((and fade.leaving (equal item fade.leaving))
           (max 0 (- fade.from (floor (* 100 ms) (max 1 fade.out)))))
          (t (let ((up (max 0 (- ms (fade-fall fade)))))
               (min 100 (if token
                            (floor (* 100 (max 0 (- up (* (if fade.order
                                                              (aref fade.order (mod token (length fade.order)))
                                                              token)
                                                          fade.step))))
                                   (max 1 fade.in))
                            (floor (* 100 up) (max 1 (fade-rise fade))))))))))

(defun fade-settled-p (fade &optional (now (get-internal-real-time)))
  "Whether FADE's place stands still at NOW: what holds it is all the way up."
  (>= (since-ms fade.at now) (+ (fade-fall fade) (fade-rise fade))))

(defun fade-to (fade item &optional (now (get-internal-real-time)))
  "Hand FADE's place to ITEM at NOW: what it shows goes out first, from the
light it had come up to; what is already going out keeps going, and ITEM
waits behind it. => FADE."
  (unless (equal item fade.shown)
    (let ((on (fade-on fade now)))
      (unless (and fade.leaving (equal on fade.leaving))
        (setf fade.from (fade-light fade on now) fade.leaving on fade.at now))
      (setf fade.shown item)))
  fade)

(nlk:define-side-table fade-line (:weakness :key)
  "A painted transcript row -> (FADE . ITEM): the place it stands in and what
of that place it paints, so the frame's FADE-OVERLAY finds it on screen.")

(defun mark-fading-rows (rows fade item)
  "ROWS, the painted rows of ITEM in FADE's place, marked for the overlay. => ROWS."
  (dolist (row rows rows)
    (setf (fade-line row) (cons fade item))))

(defun tool-row-expand-key (row)
  "Accordion identity for a tool card: the durable call id once the row
is hydrated, else the row object itself (a streaming row the wire has
not stamped yet). A thought has no id: its key is the row."
  (or (transcript-row-tool-call-id row) row))

(defun open-key (&optional (key *expanded-tool-call*))
  "What accordion KEY opens: a run's call, or one of its files, clicked open
under the run's shut title is keyed (:FOLD KEY), which opens KEY and leaves
the run shut (TOOL-GROUP-OPEN-P)."
  (if (and (consp key) (eq :fold (first key))) (second key) key))

(defun detail-open-p (key)
  "Whether the one clicked-open card, group or thought is KEY."
  (and key (equal (open-key) key)))

;;; RENDER-STATE binds it from the model; the hovered block paints on the
;;; :hover background (the Zig setHoveredToolCall/UserPrompt states).
(defvar *hover-row* nil
  "Render-scope hover state: the TRANSCRIPT-ROW under the pointer, or NIL.")

;;; --- clickable targets --------------------------------------------------------
;;; What a click on the transcript opens, in one currency: a URI string on the
;;; span (MSPAN-LINK), which is also exactly what the OSC-8 wrap hands the
;;; terminal — so a terminal that opens hyperlinks itself opens these without
;;; asking this program at all. Two schemes reach the opener: http(s), a page,
;;; and file:, a file or a folder. A span carrying neither never arms.
;;;
;;; Two kinds of target, and they are not built the same way. A FACT names its
;;; own address — the file a call read, the file a diff changed, the picture a
;;; LOOK named — and becomes a target directly. PROSE only ever looks like one,
;;; so it passes PATH-SHAPED-P first, which is deliberately narrow: an anchor
;;; a path alone has, or a relative path whose last segment carries an
;;; extension.
;;;
;;; Nothing here touches the filesystem. A golden frame is the same frame on
;;; every box, and whether the path is still there is the click's question
;;; (OPEN-TARGET), asked once, at the moment it is asked.

(defun percent-encode (text)
  "TEXT's UTF-8 octets as a URI path: each octet written %XX but RFC 3986's
unreserved set and the three a path carries as themselves -- the separator,
the drive colon and @."
  ;; Every other octet percent-encodes, so a space, or a name in any script,
  ;; survives both this program's parser and the terminal's. The scanners are
  ;; built once: a pattern handed over as data is compiled at every call, three
  ;; a path, and every tool card names one.
  (ppcre:regex-replace-all (load-time-value (ppcre:create-scanner "%(?:2F|3A|40)"))
                           (quri:url-encode text :encoding :utf-8)
                           #'percent-decode :simple-calls t))

(defun percent-decode (text)
  "The string TEXT's %XX escapes decode to, the octets read back as UTF-8."
  ;; A run of escapes is one character's octets or several whole ones — an
  ;; escape never shares a character with a literal — so each run decodes alone.
  (ppcre:regex-replace-all "(?:%[0-9A-Fa-f]{2})+" text
                           (lambda (run) (quri:url-decode run :encoding :utf-8)) :simple-calls t))

;;; A launch fact like the footer's label, and dropped by the same save hook,
;;; so no dumped image carries the build box's directory into an operator's
;;; session.
(defvar *workspace-directory* nil
  "The shell's own directory, memoized: what a relative path in the
transcript resolves against.")

(defun workspace-directory ()
  "The shell's directory, without its trailing separator."
  (or *workspace-directory*
      (setf *workspace-directory*
            (string-right-trim "/\\" (uiop:native-namestring (uiop:getcwd))))))

(defun absolute-path-p (path &aux (len (length path)))
  "Whether PATH names its own root already: a leading separator, or a drive
letter and a colon (the Windows shape)."
  (or (and (plusp len) (find (char path 0) "/\\"))
      (and (> len 2) (alpha-char-p (char path 0)) (char= #\: (char path 1))
           (find (char path 2) "/\\"))))

(defun absolute-path (path)
  "PATH as an absolute native path: one that names its own root untouched,
`~' against the operator's home, anything else against the shell's own
directory. NIL for nothing to resolve."
  (let ((path (string-trim '(#\Space #\Tab) (or path ""))))
    (cond ((zerop (length path)) nil)
          ((absolute-path-p path) path)
          ((or (string= "~" path) (uiop:string-prefix-p "~/" path))
           (let ((home (ignore-errors (uiop:native-namestring (user-homedir-pathname)))))
             (and home (plusp (length home))
                  (concatenate 'string (string-right-trim "/\\" home) (subseq path 1)))))
          (t (concatenate 'string (workspace-directory) "/" path)))))

(defun file-uri (path &optional place)
  "PATH as a file: URI — the currency a path travels in from the span that
names it to the click that opens it, and what a terminal that follows OSC-8
hyperlinks itself is handed. PLACE, a line cited in the file, rides as the
fragment."
  ;; A numeric fragment is the line, the shape kitty's hyperlinked grep
  ;; writes and its open actions read; the opener itself drops it.
  (when-let (absolute (absolute-path path))
    (let ((slashed (substitute #\/ #\\ absolute)))
      (concatenate 'string "file://"
                   (percent-encode (if (char= #\/ (char slashed 0))
                                       slashed
                                       (concatenate 'string "/" slashed)))
                   (if (plusp (length place)) (concatenate 'string "#" (percent-encode place)) "")))))

(defun cited-place (text)
  "TEXT, a path an answer may cite a place in, as (values PATH PLACE): a
`#' opens the place as written (`x.lisp#L12'), and a trailing `:12',
`:12:5' or `:10-20' cites line 12, 12 or 10 (PLACE \"12\"). PLACE is NIL for
a bare path."
  ;; The place is where in the file, never part of its name: left on the
  ;; name, `ui.lisp:1269' opened nothing and the click said it was gone.
  (if-let (hash (position #\# text))
    (values (subseq text 0 hash) (subseq text (1+ hash)))
    (multiple-value-bind (start end starts ends)
        (ppcre:scan (load-time-value (ppcre:create-scanner ":(\\d+)(?:[:-]\\d+)?\\z")) text)
      (declare (ignore end))
      (if start
          (values (subseq text 0 start) (subseq text (aref starts 0) (aref ends 0)))
          (values text nil)))))

(defun path-link-spans (path)
  "The link span over PATH for a line that opens with that fact's path, or NIL."
  (when-let (uri (and path (file-uri path)))
    (list (mspan 0 (length path) :link uri))))

(defun file-uri-path (uri)
  "The native path a file: URI names, or NIL when URI is not one."
  (when (and (stringp uri) (uiop:string-prefix-p "file://" uri))
    (let ((path (percent-decode (subseq uri (length "file://") (position #\# uri)))))
      #+win32 (substitute #\\ #\/
                          (if (and (> (length path) 2) (char= #\/ (char path 0))
                                   (char= #\: (char path 2)))
                              (subseq path 1)
                              path))
      #-win32 path)))

(defun web-url-p (text)
  "Whether TEXT is an http(s) address — the only scheme a model may write
that a click hands to the operating system."
  (and (stringp text) (ppcre:scan "^(?i)https?://" text) t))

(defun openable-target-p (uri)
  "Whether a click on URI opens anything: a page or a file, and nothing
else."
  ;; A markdown link's target is the model's to write, so a scheme this does
  ;; not name never arms.
  (or (web-url-p uri)
      (and (stringp uri) (uiop:string-prefix-p "file://" uri))))

(defun span-target-uri (target)
  "The URI for a fact's TARGET — (:PATH . STRING) or (:URL . STRING), the
shape activity.lisp hands a title — or NIL."
  ;; A fact names its own address, so a path here is taken at its word; only
  ;; prose has to look like one first.
  (when (consp target)
    (case (car target)
      (:path (file-uri (cdr target)))
      (:url (and (web-url-p (cdr target)) (cdr target))))))

(defun path-shaped-p (text &key in-code)
  "Whether TEXT reads as a path rather than as prose: no whitespace and no
shell metacharacter in it, no scheme, not a run of digits and separators (a
date, a fraction), and either an anchor a path alone has (`/', `~/', `./',
`../', a drive letter) or a relative path whose last segment carries a name
and an extension."
  ;; `src/tui/ui.lisp' is one and `and/or' is not, which is the whole point of
  ;; the rule.
  ;;
  ;; IN-CODE drops the extension requirement for a run the answer put in
  ;; backticks. Inside code marks, `src/tui' is a folder and nothing else, and
  ;; the marks are the author's own — the middle rung between a fact, which
  ;; names its own address, and bare prose, where only an anchor will do.
  (let ((text (string-trim '(#\Space #\Tab) (or text ""))))
    (and (> (length text) 1)
         (not (search "://" text))
         (not (ppcre:scan "[ \\t\\n\\r*?\\[\\]{}|<>\"'`$;&=]" text))
         (find-if #'alpha-char-p text)
         (or (absolute-path-p text)
             (ppcre:scan "^(?:~|\\.\\.?)/" text)
             ;; A relative path names a file (a name, a dot, an extension),
             ;; ends at a folder, or stands inside code marks. Without that
             ;; `and/or' would paint as something to click.
             (and (find #\/ text)
                  (or in-code (ppcre:scan "/(?:[^./]+\\.[^/]+)?\\z" text))))
         t)))

(defun path-target-uri (text &key in-code)
  "The file: URI for TEXT when TEXT reads as a path, else NIL — what a run
of prose or of inline code carries as its target."
  (and (path-shaped-p text :in-code in-code)
       (multiple-value-bind (path place) (cited-place text)
         (file-uri path place))))

(defun markdown-target-uri (destination)
  "An explicit Markdown DESTINATION as an openable URI, or NIL."
  ;; Unlike a bare prose run, an explicit destination names its target:
  ;; report.md and paths with spaces need no PATH-SHAPED-P heuristic.
  ;; Normalize once here so OSC8 and the mouse read exactly the same URI.
  ;; A literal U+FEFF at the Markdown boundary is an invisible formatting
  ;; mark, not a relative-path prefix. Trim before wrapper/scheme/root checks,
  ;; and again inside angle wrappers. Do NOT trim after percent decoding:
  ;; %EF%BB%BF and an interior U+FEFF can name real filename characters.
  (let* ((boundary (list #\Space #\Tab #\Newline #\Return (code-char #xfeff)))
         (target (string-trim boundary (or destination "")))
         (length (length target)))
    (when (and (> length 1) (char= #\< (char target 0))
               (char= #\> (char target (1- length))))
      (setf target (string-trim boundary (subseq target 1 (1- length)))))
    (cond ((openable-target-p target) target)
          ((or (zerop (length target)) (char= #\# (char target 0))
               (and (ppcre:scan "^[A-Za-z][A-Za-z0-9+.-]*:" target)
                    (not (absolute-path-p target))))
           nil)
          ;; Bad percent-encoded UTF-8 is inert, not a renderer error.
          ;; A literal `#' is the fragment; an encoded one (%23) is the name's.
          (t (multiple-value-bind (path place) (cited-place target)
               (ignore-errors (file-uri (percent-decode path) place)))))))

(defun bare-run-end (text pos floor)
  "The end of the bare run opening at POS in TEXT: whitespace, a quote, an
angle bracket or a backquote closes it, less what a sentence puts after a
run — a trailing . , : ; ! ? or ' and a closing ), ] or } the run itself did
not open."
  ;; Never trimmed past FLOOR, the first index that could still end a run
  ;; worth having.
  (let ((end (or (ppcre:scan "[ \\t\\n\\r\"<>`]" text :start pos) (length text))))
    (loop while (> end floor)
          do (let* ((ch (char text (1- end)))
                    (opener (case ch (#\) #\() (#\] #\[) (#\} #\{))))
               (if (or (find ch ".,:;!?'")
                       (and opener
                            (> (count ch text :start pos :end end)
                               (count opener text :start pos :end end))))
                   (decf end)
                   (return))))
    end))

(defun bare-path-end (text pos start)
  "The end of the bare path anchored at POS in TEXT, or NIL when none opens
there."
  ;; Only an anchor a path alone has opens one — `/', `~/', `./', `../' — and
  ;; only at a word's start, so a slash inside a word, a date or a fraction is
  ;; never a path. The run ends where BARE-RUN-END says it does.
  (when (and (or (= pos start) (not (alphanumericp (char text (1- pos)))))
             (ppcre:scan "^(?:~|\\.\\.?)?/" text :start pos))
    (let ((end (bare-run-end text pos (1+ pos))))
      (and (> end (1+ pos))
           (path-shaped-p (subseq text pos end))
           end))))

;;; --- markdown classification (Zig render_plan.zig subset) ------------------
;;; Fences, headings, thematic breaks, list markers, blockquotes, and the
;;; inline pass (code/strong/emphasis/strike). A list's marker bytes stay in
;;; the text, in the keyword tone; the other blocks draw what their markers
;;; say (a heading its text alone, a quote a bar per level, a task its
;;; box); inline markers are hidden, and a fence's marker lines are not
;;; drawn at all. Fence bodies carry syntax-token spans from the theme.lisp
;;; tokenizer; a fence's language is what its body is tokenized as (```mermaid
;;; included — it renders as the plain code block).
;;; The block markers are read by anchored CL-PPCRE patterns, compiled once.

(defun fence-open-info (line)
  "When LINE opens a fenced code block: (marker-char run-length language
indent), else NIL."
  ;; The language is the info's first word, a `{...' attribute read from past
  ;; its first dot and a leading `.' dropped (`{.python}', `.python'). INDENT
  ;; is the spaces before the marker run.
  (ppcre:register-groups-bind (spaces run info) ("^( {0,3})(`{3,}|~{3,})(.*)" line)
    (let ((info (ppcre:regex-replace "^(?:\\{[^.]*\\.|\\.)" (nlk:trimmed info) "")))
      (list (char run 0) (length run) (ppcre:scan-to-strings "^[^ \\t\\r\\n\\f,}\\[\\]]*" info)
            (length spaces)))))

(defun fence-close-p (line fence)
  "Close requires the same marker char, a run at least as long, blank rest."
  (ppcre:register-groups-bind (run) ("^ {0,3}(`+|~+) *\\z" line)
    (and (char= (char run 0) (first fence)) (>= (length run) (second fence)))))

(defun heading-marker-end (line)
  "When LINE is an ATX heading: (values marker-end level), else NIL."
  ;; The marker is the #-run plus its trailing space; the heading's text
  ;; starts past it.
  (multiple-value-bind (start end) (ppcre:scan "^#{1,6}(?: |\\z)" line)
    (and start (values end (count #\# line :end end)))))

(defun list-marker-end (line)
  "Character end of a list-marker prefix (≤8 space indent, bullet or 1-9
digits + ./) + space, optional task box), else NIL. Becomes the hang indent.
Second value: whether the item is a task, :done for `[x]' and :open for `[ ]'."
  ;; ASCII digits, as the Zig renderer and CommonMark read an ordered marker.
  (multiple-value-bind (start end boxes) (ppcre:scan "^ {0,8}(?:[-*+]|\\d{1,9}[.)]) (\\[[ xX]\\] )?" line)
    (and start (values end (let ((box (aref boxes 0)))
                             (and box (if (char= #\Space (char line (1+ box))) :open :done)))))))

(defun blockquote-prefix-end (line)
  "End of a blockquote prefix: runs of > each optionally followed by a space."
  (nth-value 1 (ppcre:scan "^ {0,3}(?:> ?)+" line)))

(defun bare-url-end (text pos start)
  "The end of the bare http(s) URL at POS in TEXT, or NIL when none opens
there."
  ;; The scheme must open a word — POS is START, or the character before it is
  ;; not alphanumeric — and the URL runs to where BARE-RUN-END says it does,
  ;; never trimmed back into its own scheme.
  (let ((scheme-end (nth-value 1 (ppcre:scan "^(?i)https?://" text :start pos))))
    (when (and scheme-end
               (or (= pos start)
                   (not (alphanumericp (char text (1- pos))))))
      (let ((end (bare-run-end text pos scheme-end)))
        (and (> end scheme-end) end)))))

(defun inline-color-open (text pos)
  "End and RGB of a strict foreground-only color opener at POS, else NIL."
  (multiple-value-bind (match end starts ends)
      (ppcre:scan "^<span style=\"color: *(#[0-9a-fA-F]{6})\">" text :start pos)
    (when match
      (let ((hex (subseq text (aref starts 0) (aref ends 0))))
        (values end (loop for at in '(1 3 5)
                          collect (parse-integer hex :start at :end (+ at 2) :radix 16)))))))

(defun inline-color-run (text pos &aux (depth 1))
  "A complete single-line color region: (body-start body-end end RGB), else NIL.
Nested regions balance; escapes and inline code cannot close a region."
  ;; The body reads as tokens -- an escape, a code span, a closer, an opener,
  ;; any one character -- so a closer inside the first two never counts.
  (multiple-value-bind (body-start rgb) (inline-color-open text pos)
    (when body-start
      (ppcre:do-matches (start end
                         "(?s)\\\\.?|`[^`]+`|</span>|<span style=\"color: *#[0-9a-fA-F]{6}\">|."
                         text nil :start body-start)
        (cond ((prefix-at-p text start "</span>")
               (when (zerop (decf depth))
                 (return-from inline-color-run (list body-start start end rgb))))
              ((and (char= #\< (char text start)) (> end (1+ start)) (> (incf depth) 16))
               (return-from inline-color-run nil)))))))

(defun compact-inline-text (text spans &aux (hidden (remove-if-not #'mspan-hidden spans)))
  "TEXT and SPANS with hidden markers removed before measuring or attaching metadata."
  (flet ((shift (offset)
           (- offset (loop for span in hidden sum (max 0 (- (min offset span.end) span.start))))))
    (values
     (with-output-to-string (out)
       (loop for from = 0 then (mspan-end span)
             for span in hidden
             do (write-string text out :start from :end (mspan-start span))
             finally (write-string text out :start from)))
     (loop for span in spans
           unless (mspan-hidden span)
             collect (copy-with mspan span :start (shift span.start) :end (shift span.end))))))

(defun code-spans (text start end &aux (body (subseq text start end))
                                       (quiet (make-array (length body) :element-type 'bit
                                                                        :initial-element 0)))
  "The spans over the inline code run [START,END) of TEXT, two tones of the
cool hue: an id all quiet, a path's folders quiet under its last segment,
a name bright but for its package and its punctuation. Each carries the
file the run names, when it names one (PATH-TARGET-URI)."
  ;; The bright part is what a reader looks for -- the file, the function --
  ;; and the rest stands back.
  (let ((tone (cond ((ppcre:scan "^(?:[0-9a-f]{7,40}|\\d{3,}|[0-9A-Z]{8,}(?:/[0-9a-f]+…?)?)\\z" body)
                     (fill quiet 1)
                     :inline-code)
                    ((and (not (find-if #'blank-char-p body))
                          (or (ppcre:scan "^(?:~|\\.\\.?/)|[^/]/" body)
                              (ppcre:scan "\\.(?:lisp|asd|py|json|jsonc|md|js|ts|sh|txt|toml|ya?ml|rs|go|zig|c|h|html|css)\\z"
                                          body)))
                     (let* ((last (if (alexandria:ends-with #\/ body) (1- (length body)) (length body)))
                            (cut (position #\/ body :end last :from-end t)))
                       (when cut (fill quiet 1 :end (1+ cut)))
                       (when (< last (length body)) (setf (sbit quiet last) 1)))
                     :code-path)
                    (t (fill quiet 1 :end (or (let ((at (search "::" body :from-end t))) (and at (+ at 2)))
                                              (and (ppcre:scan "^[a-z][a-z0-9.-]*:[^:\\s]" body)
                                                   (1+ (position #\: body)))
                                              0))
                       (loop for i from 0 below (length body)
                             when (find (char body i) "*+{},…") do (setf (sbit quiet i) 1))
                       :inline-code)))
        (link (path-target-uri body :in-code t)))
    (loop with spans = '()
          for i from 0 below (length body)
          for piece = (if (= 1 (sbit quiet i)) :code-quiet tone)
          do (if (and spans (eq piece (mspan-tone (first spans))))
                 (setf (mspan-end (first spans)) (+ start i 1))
                 (push (mspan (+ start i) (+ start i 1) :tone piece :link link) spans))
          finally (return (nreverse spans)))))

(defun inline-spans (text start &optional (color-depth 0) &aux (spans '())
                                     (pos start)
                                     (len (length text)))
  "Inline markdown spans from START: `code`, **strong**, *em* and _em_,
***both***, ~~strike~~, backslash escapes, [text](url) links, bare http(s)
URLs, paths and strict <span style=\"color:#RRGGBB\"> regions."
  ;; Markers hide; code and strong take their palette tones; emphasis is
  ;; italic only, and _ never toggles intra-word (Zig). A link paints in the
  ;; link tone, underlined, and carries its target (MSPAN-LINK) for the OSC-8
  ;; wrap and the click that opens it; a path carries the same target and no
  ;; styling of its own, since a page is a place to go and a path is just the
  ;; file being talked about.
  ;; A backslash before ASCII punctuation (CommonMark) hides and the character
  ;; after it prints as itself, never opening or closing a mark: \* \_ \` \|
  ;; \\ \# read as the one character. Without this, the backslash would print
  ;; and the mark would style what follows. Inside `code' a backslash is
  ;; literal: a code run's text is kept whole (CODE-SPANS).
  (labels ((escape-p (at end)
             (and (< (1+ at) end)
                  (char= (char text at) #\\)
                  (find (char text (1+ at)) "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")))
           (paired (open-len close-pos close-len &rest style)
             ;; The run's STYLE is laid in pieces around the backslashes of
             ;; its own escapes, which hide.
             (push (mspan pos (+ pos open-len) :hidden t) spans)
             (let* ((piece (+ pos open-len))
                    (inner (and (< color-depth 16)
                                (search "<span style=" text :start2 piece :end2 close-pos)
                                (inline-spans (subseq text piece close-pos) 0 (1+ color-depth))))
                    (colored (some (lambda (span) (visual-style-p (mspan-tone span))) inner)))
               (cond (colored
                      (lay inner piece close-pos style
                           (lambda (moved)
                             (setf moved.tone (or moved.tone (getf style :tone))
                                   moved.ground (or moved.ground (getf style :ground))
                                   moved.bold (or moved.bold (getf style :bold))
                                   moved.italic (or moved.italic (getf style :italic))
                                   moved.strike (or moved.strike (getf style :strike))
                                   moved.underline (or moved.underline (getf style :underline))
                                   moved.link (or moved.link (getf style :link))))))
                     (t (loop for at from piece below close-pos
                              when (and (>= at piece) (escape-p at close-pos))
                                do (when (< piece at)
                                     (push (apply #'mspan piece at style) spans))
                                   (push (mspan at (1+ at) :hidden t) spans)
                                   (setf piece (+ at 2)))
                        (when (< piece close-pos)
                          (push (apply #'mspan piece close-pos style) spans)))))
             (push (mspan close-pos (+ close-pos close-len) :hidden t) spans)
             (setf pos (+ close-pos close-len)))
           (colored (run)
             (destructuring-bind (body-start body-end end rgb) run
               (let ((tone (vstyle :fg rgb)))
                 (push (mspan pos body-start :hidden t :tone tone) spans)
                 (lay (inline-spans (subseq text body-start body-end) 0 (1+ color-depth))
                      body-start body-end (list :tone tone)
                      (lambda (moved) (unless (visual-style-p moved.tone) (setf moved.tone tone))))
                 (push (mspan body-end end :hidden t) spans)
                 (setf pos end))))
           (lay (inner base end style adopt &aux (at 0))
             ;; INNER's spans moved on to BASE, ADOPT styling each one that is
             ;; not hidden, and the gaps between them up to END laid in STYLE.
             (dolist (span inner)
               (when (< at span.start)
                 (push (apply #'mspan (+ base at) (+ base span.start) style) spans))
               (let ((moved (shifted-span span base)))
                 (unless moved.hidden (funcall adopt moved))
                 (push moved spans))
               (setf at span.end))
             (when (< (+ base at) end)
               (push (apply #'mspan (+ base at) end style) spans)))
           (fenced (marker &rest style)
             ;; MARKER, then a non-empty run up to the next MARKER no backslash
             ;; escapes; else one char on.
             (let ((close (loop for at = (search marker text :start2 (+ pos (length marker)))
                                  then (search marker text :start2 (1+ at))
                                while (and at (char= (char text (1- at)) #\\))
                                finally (return at))))
               (if (and close (> close (+ pos (length marker))))
                   (apply #'paired (length marker) close (length marker) style)
                   (incf pos)))))
    (loop while (< pos len)
          do (let* ((ch (char text pos))
                    (color-run (and (< color-depth 16) (char= ch #\<)
                                    (inline-color-run text pos))))
               (cond
                 ((escape-p pos len)
                  (push (mspan pos (1+ pos) :hidden t) spans)
                  (incf pos 2))
                 (color-run (colored color-run))
                 ((char= ch #\`)
                  ;; Inline code is where an answer names a file —
                  ;; `src/tui/ui.lisp' — so the run carries that path as
                  ;; its target when it reads as one (CODE-SPANS). The
                  ;; span a reader already sees as a file is the span a
                  ;; click opens; no second scan, no second rule.
                  (let ((close (position #\` text :start (1+ pos))))
                    (cond ((and close (> close (1+ pos)))
                           (push (mspan pos (1+ pos) :hidden t) spans)
                           (dolist (span (code-spans text (1+ pos) close))
                             (push span spans))
                           (push (mspan close (1+ close) :hidden t) spans)
                           (setf pos (1+ close)))
                          (t (incf pos)))))
                 ;; ***both*** is strong and emphasis at once; without this
                 ;; it would read as ** and leave a stray * on each side of
                 ;; a bold run. A strong run stands on a chip.
                 ((and (prefix-at-p text pos "***")
                       (search "***" text :start2 (+ pos 3)))
                  (fenced "***" :tone :inline-strong :bold t :italic t :ground :chip))
                 ((prefix-at-p text pos "**")
                  (fenced "**" :tone :inline-strong :bold t :ground :chip))
                 ((char= ch #\*) (fenced "*" :italic t))
                 ;; Struck through, the web page's <del>; without this, the
                 ;; tildes would print.
                 ((prefix-at-p text pos "~~") (fenced "~~" :strike t))
                 ((and (char= ch #\_)
                       (or (= pos start)
                           (not (alphanumericp (char text (1- pos))))))
                  (let ((close (position #\_ text :start (1+ pos))))
                    (if (and close (> close (1+ pos))
                             (or (= (1+ close) len)
                                 (not (alphanumericp (char text (1+ close))))))
                        (paired 1 close 1 :italic t)
                        (incf pos))))
                 ((char= ch #\[)
                  ;; [text](url): the text underlines in the link tone,
                  ;; carries the url as an OSC-8 hyperlink (zero width —
                  ;; the layout math never sees it), and the raw url hides.
                  (multiple-value-bind (match end url)
                      (ppcre:scan "^\\[[^\\]]+\\]\\(([^)]*)\\)" text :start pos)
                    (if match
                        (let ((close (- (aref url 0) 2)))
                          ;; An image, ![alt](url), reads as its alt text.
                          (when (and (> pos start) (char= #\! (char text (1- pos))))
                            (push (mspan (1- pos) pos :hidden t) spans))
                          (paired 1 close (- end close)
                                  :tone :link :underline t
                                  :link (markdown-target-uri
                                         (subseq text (aref url 0) (1- end)))))
                        (incf pos))))
                 (t
                  (let ((end (and (char-equal ch #\h)
                                  (bare-url-end text pos start)))
                        (path-end (and (find ch "/~.") (bare-path-end text pos start))))
                    (cond (end
                           (push (mspan pos end :tone :link :underline t
                                        :link (subseq text pos end))
                                 spans)
                           (setf pos end))
                          ;; A bare path keeps the prose's own colour: a
                          ;; transcript naming files everywhere would read
                          ;; as a link farm if each one underlined. What
                          ;; marks it is the pointer (*HOVER-TARGET*).
                          (path-end
                           (when-let (uri (path-target-uri (subseq text pos path-end)))
                             (push (mspan pos path-end :link uri) spans))
                           (setf pos path-end))
                          (t (incf pos)))))))))
  (nreverse spans))

(defun token-spans (tokens)
  "Styled spans for syntax TOKENS."
  ;; Plain runs carry no span, so they paint in the line's base style; a
  ;; dimmed base dims the token colors through the span pipeline exactly as
  ;; the Zig dimColor pass does.
  (loop for token in tokens
        for tone = (token-tone token.kind)
        when tone
          collect (mspan token.start token.end
                         :tone tone)))

(defun command-token-spans (command)
  "Syntax spans for an executable command."
  ;; Both completed tool headers and the live background band use this pass so
  ;; a call keeps the same syntax colors when it moves between surfaces.
  (token-spans (tokenize-shell command)))

(defun table-row-cells (line &aux (text (string-trim " " line)) (cells (list '())) (closed nil))
  "Split one | row into trimmed cell strings, outer pipes stripped. A pipe
escaped as \\| or standing inside a `code span' stays in its cell, the
escape's backslash dropped (GFM, which drops it inside code too)."
  ;; Without this, a | inside an escaped pair or a closed code span would be
  ;; read as a cell boundary and push the row's last cells off the grid.
  ;; Any other escaped pair passes on whole for the inline pass, so an escaped
  ;; backtick never opens a code span here either. The row reads as tokens --
  ;; an escaped pair, a closed code span, a run of plain text, any one
  ;; character -- and only a bare | token parts cells.
  (ppcre:do-matches-as-strings (token "\\\\.|`[^`]*`|[^\\\\`|]+|." text)
    (if (setf closed (string= token "|"))
        (push '() cells)
        (push token (first cells))))
  ;; The outer pipes open and close the row rather than part cells: a leading
  ;; one leaves an empty first cell, a closing one an empty last. An empty row
  ;; still has its one empty cell.
  (when closed
    (pop cells))
  (let ((cells (nreverse cells)))
    (when (and (plusp (length text)) (char= (char text 0) #\|))
      (pop cells))
    (mapcar (lambda (tokens)
              (string-trim " " (ppcre:regex-replace-all
                                "\\\\\\|" (apply #'concatenate 'string (reverse tokens)) "|")))
            (or cells (list '())))))

(defun table-cell (cell)
  "CELL as the grid paints it: (TEXT . SPANS), hidden markers removed."
  (multiple-value-bind (text spans) (compact-inline-text cell (inline-spans cell 0))
    (cons text spans)))

(defparameter *min-table-key-value-width* 8
  "Minimum value-column budget for transposed key/value table records (Zig
min_table_key_value_width).")

(defun table-column-widths (all-rows &optional columns)
  "The display width of each markdown table column over ALL-ROWS."
  ;; COLUMNS caps how many columns are measured — the rendered grid keeps only
  ;; the header's columns — and defaults to the widest row's.
  (loop for column below (or columns (reduce #'max all-rows :key #'length))
        collect (loop for cells in all-rows
                      maximize (text-display-width (or (nth column cells) "")))))

(defun pack-lines (items width &optional (joiner " ") &aux (lines '()) (current nil))
  "ITEMS joined by JOINER into lines of at most WIDTH display columns,
greedily and in order; an item wider than WIDTH stands alone on its line."
  ;; The Zig emitMarkdownTableKeyValueRow tokenize-and-fill, and the home
  ;; screen's cell lane: NIL for no items.
  (dolist (item items)
    (let ((candidate (and current (concatenate 'string current joiner item))))
      (if (and candidate (> (text-display-width candidate) width))
          (setf lines (cons current lines) current item)
          (setf current (or candidate item)))))
  (nreverse (if current (cons current lines) lines)))

(defun table-separator-p (line)
  "Whether LINE is the separator row under a table's header: only | - : and
spaces, a | and a - among them, three characters or more between its outer
spaces."
  (ppcre:scan "^(?=.*\\|)(?=.*-) *[|:-][|:\\- ]+[|:-] *\\z" line))

(defun settled-line-count (lines &aux (count (length lines)))
  "How many of LINES, the lines of a message still arriving, are drawn yet:
all but the pipe rows it ends on when no separator has made them a table
yet, or, once one has, all but the last row."
  ;; A table lays its columns out over every row it holds, so a row drawn
  ;; while its cells arrived widened them token by token and the whole table
  ;; jumped. Holding back what is still arriving lays the table
  ;; out once per row: a row lands whole when the line after it begins, and a
  ;; header waits for its separator instead of showing first as a line of
  ;; pipes. Only a line opening with a pipe waits, so prose never stalls; the
  ;; separator may stand just above those lines, written without an outer
  ;; pipe.
  (let* ((run (1+ (or (position-if-not (lambda (line) (ppcre:scan "^ {0,3}\\|" line))
                                       lines :from-end t)
                      -1)))
         (separator (position-if #'table-separator-p lines :start (max 0 (1- run)))))
    (cond ((= run count) count)
          ((null separator) run)
          ((= separator (1- count)) count)
          (t (1- count)))))

(defun table-alignments (separator-line)
  "Each column's alignment as SEPARATOR-LINE, the |---| row under a table's
header, says it: :CENTER for a colon at both ends of its cell, :RIGHT for one
at the end, else :LEFT (GFM, md.js's ALIGNMENTS)."
  (mapcar (lambda (cell)
            (cond ((ppcre:scan "^:.*:\\z" cell) :center)
                  ((ppcre:scan ":\\z" cell) :right)
                  (t :left)))
          (table-row-cells separator-line)))

(defun markdown-table-llines (header-line separator-line row-lines
                              &key content-width)
  "An aligned markdown table: padded columns behind │ separators, the
header bold on a heading's band, each column standing left, right or
centred as SEPARATOR-LINE says."
  ;; When CONTENT-WIDTH is supplied and the padded grid overflows it (the Zig
  ;; markdownTableColumnsForBlock gate), the table transposes to key/value
  ;; records instead.
  ;; A cell is (TEXT . SPANS) (TABLE-CELL); ALL is every row's texts.
  ;; Without this, the alignment row's colons would be read as nothing, so a
  ;; column of figures marked --: would stand ragged at the left like prose.
  (let* ((header (mapcar #'table-cell (table-row-cells header-line)))
         (rows (mapcar (lambda (line) (mapcar #'table-cell (table-row-cells line)))
                       row-lines))
         (all (mapcar (lambda (cells) (mapcar #'car cells)) (cons header rows))))
    (if (and content-width
             (not (and (every (lambda (cells) (<= (length cells) 12)) all)
                       (let ((widths (table-column-widths all)))
                         (<= (+ 1 (reduce #'+ widths) (* 3 (length widths)))
                             content-width)))))
        ;; Key/value records (Zig emitMarkdownTableKeyValueRow), a dim rule
        ;; between; their values are the cells' texts, unstyled.
        (let* ((label-width (loop for label in (first all) maximize (text-display-width label)))
               (value-indent (min (+ label-width 3)
                                  (max 0 (- content-width
                                            *min-table-key-value-width*))))
               (value-width (max (- content-width value-indent)
                                 *min-table-key-value-width*)))
          (loop for cells in (rest all)
                for first-record = t then nil
                unless first-record
                  collect (make-lline :text "" :tone :markdown-line :rule t)
                nconc (loop for label in (first all)
                            for i from 0
                            for words = (split-words (nth i cells))
                            nconc (loop for fragment in (or (pack-lines words value-width)
                                                            (list ""))
                                        for labeled = t then nil
                                        for lead = (if labeled (concatenate 'string " " label) "")
                                        for pad = (max 0 (- value-indent (text-display-width lead)))
                                        for text = (format nil "~a~va~a" lead pad "" fragment)
                                        for bold = (and labeled (mspan 1 (length lead) :bold t))
                                        collect (make-lline :text (string-right-trim " " text)
                                                            :spans (and bold (list bold)))))))
        (let ((widths (table-column-widths all (length header)))
              (alignments (table-alignments separator-line)))
          ;; One grid row: each column's cell padded to its width with FILL on
          ;; the side its alignment leaves open (both, split, for a centred
          ;; one), the columns joined by JOINT in the lines' tone and each
          ;; cell's spans moved to where its text lands.
          (flet ((grid-lline (cells joint fill &rest style &aux (spans '()))
                   (apply #'make-lline
                          :text (with-output-to-string (out)
                                  (loop for width in widths
                                        for column from 0
                                        for (text . cell-spans) = (or (pop cells) '(""))
                                        for pad = (- width (text-display-width text))
                                        for before = (case (nth column alignments)
                                                       (:right pad)
                                                       (:center (floor pad 2))
                                                       (t 0))
                                        for first = t then nil
                                        do (unless first
                                             (push (mspan (file-position out)
                                                          (+ (file-position out) (length joint))
                                                          :tone :markdown-line)
                                                   spans)
                                             (write-string joint out))
                                           (format out "~v,,,v@a" before fill "")
                                           ;; The stream's position is where the cell's text lands.
                                           (dolist (span cell-spans)
                                             (push (shifted-span span (file-position out)) spans))
                                           (format out "~a~v,,,v@a" text (- pad before) fill "")))
                          :spans (nreverse spans)
                          :grid t
                          style)))
            ;; The header stands on a heading's band, which says header where
            ;; a ─┼─ rule under it would.
            (let ((head (grid-lline header " │ " #\Space :tone :heading :bold t)))
              (setf (lline-ground head) (list :heading-band 3 (+ 3 (text-display-width (lline-text head))) nil))
              (cons head (mapcar (lambda (cells)
                                   (grid-lline cells " │ " #\Space))
                                 rows))))))))

(defun lead-spans (line spans)
  "SPANS, the inline spans of paragraph LINE, with the strong run LINE
opens on made its lead -- the heading's tone, on no chip -- and the lead's
width as a second value, NIL when LINE opens on no strong run."
  (values spans
          (when (and (prefix-at-p line 0 "**") (not (prefix-at-p line 0 "***")))
            (loop with width = 0
                  for at = 0 then span.end
                  for span in spans
                  while (and (= span.start at) (or span.hidden (eq span.tone :inline-strong)))
                  unless span.hidden
                    do (setf span.tone :heading span.ground nil)
                       (incf width (text-display-width (subseq line span.start span.end)))
                  finally (return (and (plusp width) width))))))

(defun markdown-llines (text &key shadow width arriving)
  "Classify TEXT's logical lines as the Zig transcript does for assistant and
reasoning bodies."
  ;; Every body word-wraps — a word the row cannot hold moves
  ;; whole to the next row — a fence skips the inline pass and carries
  ;; syntax-token spans for the fence's language. Pipe tables
  ;; (a | header over a |---| separator) render aligned; ARRIVING says TEXT
  ;; is a message still streaming, whose table rows wait until they are
  ;; whole (SETTLED-LINE-COUNT). SHADOW marks
  ;; every lline for the shadow (the reasoning block): the same lines in the same
  ;; tones, the text painted at less light on the same canvas.
  ;; Second and third values: the character offset of the last top-level blank
  ;; line (the incremental-reparse checkpoint — nothing before it can re-render
  ;; differently once more text is appended, because no construct in this
  ;; dialect looks across a blank line outside a fence) and the count of llines
  ;; emitted before that line; both NIL without a usable checkpoint.
  ;; Empty TEXT classifies to no llines at all, SPLIT-LINES' policy: a body with
  ;; no content contributes nothing, not a blank line the block padding would
  ;; then pad on both sides.
  (let* ((fence nil)
         (slab-ended nil)
         (lines (coerce (split-lines text) 'vector))
         (line-count (if arriving (settled-line-count lines) (length lines)))
         (index 0)
         (result '())
         (cp-line nil)
         (cp-tail nil)
         ;; The prose body width (Zig transcriptBodyWidthForLine: the frame minus
         ;; the 3-column prose indent and the right padding column).
         (content-width (and width (max 1 (- width (nth-value 2 (body-gutter :prose))
                                              *transcript-right-padding*)))))
   (flet ((emit (&rest lline-args)
            (push (apply #'make-lline lline-args) result))
          (slab (&rest lline-args)
            ;; A code block's row: on the slab from its edge to the text's
            ;; right one, a wrapped line going on under the edge and a ↦.
            (push (apply #'make-lline :role :code :gutter-tone :markdown-bar :ground '(:slab 3 :edge nil)
                         :hang 2 :hang-mark (list (make-seg :text "▌" :tone :markdown-bar)
                                                  (make-seg :text "↦" :tone :muted))
                         lline-args)
                  result))
          (gap ()
            ;; A code block stands a blank row off whatever touches it --
            ;; prose, a heading, a list, another block -- without a blank
            ;; line between them, its ground would meet the line above it
            ;; edge to edge. A blank already there is the gap, and a block
            ;; opening the body has the body's padding.
            (let ((last (first result)))
              (when (and last (or (lline-rule last) (not (eq :prose (lline-role last)))
                                  (position-if-not #'blank-char-p (lline-text last))))
                (push (make-lline :text "") result))))
          (prose (text start &aux (spans (inline-spans text start)) (at (1- (length text))))
            ;; The inline pass, and CommonMark's hard break: an unescaped
            ;; backslash closing a line the paragraph goes on after hides, the
            ;; row ending there anyway. A paragraph's last line keeps its
            ;; backslash, as CommonMark has it.
            (if (and (>= at start)
                     (< index line-count)
                     (position-if-not #'blank-char-p (aref lines index))
                     (oddp (- at (or (position #\\ text :test-not #'char= :from-end t) -1)))
                     (notany (lambda (span) (> span.end at)) spans))
                (append spans (list (mspan at (1+ at) :hidden t)))
                spans)))
    (loop while (< index line-count)
     do
     (let ((line (aref lines index)))
       ;; The line after a code block's closing fence stands off it (GAP).
       (when (and (shiftf slab-ended nil) (position-if-not #'blank-char-p line))
         (gap))
       ;; Checkpoint: a blank line reached by this loop with no open fence,
       ;; and fence bodies never get here with FENCE set, so a blank recorded
       ;; here is a true top-level block boundary.
       (when (and (not fence) (zerop (length line)))
         (setf cp-line index
               cp-tail result))
       (incf index)
       (if (and (not fence)
                (find #\| line)
                (< index line-count)
                (table-separator-p (aref lines index))
                (>= (length (table-row-cells line)) 2))
           ;; A table block: header, separator, then every following | row.
           (let ((separator (aref lines index))
                 (rows (loop for i from (1+ index) below line-count
                             while (find #\| (aref lines i)) collect (aref lines i))))
             (incf index (1+ (length rows)))
             (setf result (revappend (markdown-table-llines line separator rows
                                                             :content-width content-width)
                                     result)))
        (cond
         ;; A fence's body stands on a slab, a ground a step off the canvas
         ;; with an edge in the gutter, its first row the language and its
         ;; last a blank: with only an indent saying code, a block would read
         ;; as prose. A ```diff body takes the diff tones by its lines' first
         ;; column past the fence's own indent.
         ((and fence (not (fence-close-p line fence)))
          (bind (((_ _ language indent) (values-list fence))
                 (mark (subseq line (min indent (or (position #\Space line :test-not #'char=)
                                                    (length line))))))
            (if (member language '("diff" "patch") :test #'string-equal)
                (slab :text line
                      :tone (cond ((some (lambda (head) (prefix-at-p mark 0 head))
                                         '("@@" "+++" "---"))
                                   :muted)
                                  ((prefix-at-p mark 0 "+") :diff-added)
                                  ((prefix-at-p mark 0 "-") :diff-removed)
                                  (t :foreground)))
                (slab :text line :spans (token-spans (tokenize-line line language))))))
         ;; The markers hide (the literal-code rule): a closing line clears
         ;; FENCE and is the slab's blank last row, an opening line sets it
         ;; and is its first, the language quiet at the fence's indent; each
         ;; stands a blank row off what touches the block (GAP).
         ((shiftf fence nil) (slab :text "") (setf slab-ended t))
         ((setf fence (fence-open-info line))
          (gap)
          (bind (((_ _ language indent) (values-list fence)))
            (slab :text (if (plusp (length language)) (format nil "~va~a" indent "" language) "")
                  :tone :muted)))
         ;; A thematic break: at most 3 spaces indent, then 3+ of one of -*_ and spaces.
         ((ppcre:scan "^ {0,3}([-*_])(?: *\\1){2,} *\\z" line)
          (emit :text line :tone :markdown-line :rule t))
         ;; A heading draws its text alone, its #s hidden (a closing run
         ;; too). # and ## stand on a band of the cool hue from the gutter,
         ;; solid under their words and going out into the canvas after them,
         ;; # on the deeper band and underlined; ### and deeper are their
         ;; words alone in the cool hue, ### bold. Every level drew bold behind
         ;; its hashes, and in the prose's colour until the D board.
         ;; A thought's stand on no band.
         ((heading-marker-end line)
          (multiple-value-bind (marker-end level) (heading-marker-end line)
            (let ((text (ppcre:regex-replace "(?:^| +)#+ *\\z" (subseq line marker-end) ""))
                  (banded (and (< level 3) (not shadow))))
              (emit :text text :tone (if banded :heading :subheading)
                    :bold (< level 4) :underline (= level 1)
                    :role (if banded :heading :prose) :gutter-tone :subheading
                    :hang (if banded 2 0) :hang-mark (and banded (make-seg :text "▌" :tone :subheading))
                    :ground (and banded (list (if (= level 1) :title-band :heading-band) 1 :text 36))
                    :spans (inline-spans text 0)))))
         ((list-marker-end line)
          (multiple-value-bind (end task) (list-marker-end line)
            (let ((indent (position #\Space line :test-not #'char=)))
              (cond
                ;; A task draws a box where its marker stood, □ open in the
                ;; marks' tone and ▣ done in the lines', its text dim: the web
                ;; page's checkbox, Codex's ☐ ☑ being no glyphs of Noto Sans
                ;; Mono. Without this, a task would draw `- [x]' as typed.
                (task
                 (let* ((done (eq task :done))
                        (head (format nil "~va~a " indent "" (if done "▣" "□")))
                        (text (concatenate 'string head (subseq line end))))
                   (emit :text text :dim done
                         :hang (min (length head) *max-markdown-hang*)
                         :spans (cons (mspan indent (1+ indent) :tone (if done :markdown-line :list-mark)
                                                                :bold (not done))
                                      (prose text (length head))))))
                ;; A number stands right-aligned to the widest of its list
                ;; (the :ORDINAL inset, set below), so a list reaching 10
                ;; keeps its words in one column.
                ((digit-char-p (char line indent))
                 (emit :text line :role (cons :ordinal 0)
                       :hang (min end *max-markdown-hang*)
                       :spans (cons (mspan indent (position #\Space line :start indent) :tone :list-mark)
                                    (prose line end))))
                ;; A bullet is a dot where its marker stood, hollow nested.
                (t (let ((text (concatenate 'string (subseq line 0 indent) (if (< indent 2) "•" "◦")
                                            (subseq line (1+ indent)))))
                     (emit :text text :hang (min end *max-markdown-hang*)
                           :spans (cons (mspan indent (1+ indent) :tone :list-mark)
                                        (prose text end)))))))))
         ;; A quote draws a bar per level in place of its > marks and its
         ;; text in the muted tone, the web page's border and colour; the
         ;; bars run down every row a long line wraps onto.
         ((blockquote-prefix-end line)
          (let* ((end (blockquote-prefix-end line))
                 (bars (apply #'concatenate 'string
                              (make-list (count #\> line :end end) :initial-element "▌ ")))
                 (text (concatenate 'string bars (subseq line end))))
            (emit :text text :tone :muted
                  :hang (min (length bars) *max-markdown-hang*)
                  :hang-mark (make-seg :text bars :tone :markdown-bar)
                  :spans (cons (mspan 0 (length bars) :tone :markdown-bar)
                               (prose text (length bars))))))
         ;; A paragraph that opens on a strong run leads with it: the run
         ;; in the heading's tone on a heading's band (LEAD-SPANS).
         (t
          (multiple-value-bind (spans lead) (if shadow (prose line 0) (lead-spans line (prose line 0)))
            (if lead
                (emit :text line :role :heading :hang 2 :gutter-tone :subheading
                      :ground (list :heading-band 1 (+ 3 lead 2) 36) :spans spans)
                (emit :text line :spans spans)))))))))
   ;; Explicit colors carry long hidden tags. Finalize their visible text
   ;; before wrapping, memoization and EQ-keyed link/copy metadata. Other
   ;; Markdown keeps its established representation.
   (dolist (line result)
     (when (some (lambda (span) (visual-style-p (mspan-tone span))) (lline-spans line))
       (multiple-value-bind (text spans) (compact-inline-text (lline-text line) (lline-spans line))
         (setf (lline-text line) text (lline-spans line) spans))))
   (when shadow
     (dolist (line result)
       (setf line.shadow t)
       (dolist (span (lline-spans line))
         (when (eq (mspan-tone span) :inline-strong)
           (setf (mspan-tone span) :thought-strong
                 (mspan-ground span) nil)))))
   ;; A run of numbered items takes the widest number's columns at each
   ;; depth, every number right-aligned in them: its last digit and dot
   ;; stand where a bullet would, the words a column past.
   (loop with run = '()
         for line in (append result (list nil))
         for (role . nil) = (and line (consp line.role) line.role)
         do (if (eq role :ordinal)
                (let* ((text line.text)
                       (indent (position #\Space text :test-not #'char=)))
                  (push (list line indent (- (position #\Space text :start indent) indent)) run))
                (loop for (numbered indent width) in run
                      do (setf (lline-role numbered)
                               (cons :ordinal (- (loop for (nil at most) in run
                                                       when (= at indent) maximize most)
                                                 width)))
                      finally (setf run '()))))
   ;; CP-TAIL is the result list as it stood on reaching the checkpoint
   ;; line, so its length IS the head lline count.
   (let ((cp-count (and cp-line (length cp-tail))))
     (values (nreverse result)
             (and cp-line (loop for i below cp-line sum (1+ (length (aref lines i)))))
             cp-count))))

;;; --- layout: logical lines to painted rows ---------------------------------

(defstruct (seg (:constructor make-seg (&key (text "") tone bold italic dim
                                             underline strike link ground glint)))
  "One paint run: text plus tone and attribute overrides."
  ;; LINK wraps the painted run in an OSC-8 hyperlink under a painting profile
  ;; — so a terminal that follows hyperlinks opens the run itself, whether it
  ;; names a page or a file. GROUND, a tone or a style, is the background the
  ;; run stands on (GROUNDS-SHOWN-P); GLINT its span's (MSPAN-GLINT).
  text tone bold italic dim underline strike link ground glint)

(nlk:access (s seg) (seg seg))

(defun osc8-wrap (content url)
  "CONTENT wrapped in the OSC-8 hyperlink pair for URL."
  ;; The renderer decodes the sequence into cell links and treats it as
  ;; zero-width, so wrap math and diffing survive it. URL is content too (a
  ;; markdown link's target is the model's to write): a control codepoint in
  ;; it would end the OSC early — at the renderer's parser or at the
  ;; terminal's — and paint the rest of the URL as cells the row never
  ;; measured, so those are dropped, not pictured.
  (format nil "~c]8;;~a~c\\~a~c]8;;~c\\"
          #\Escape
          (remove-if (lambda (ch) (control-codepoint-p (char-code ch))) url)
          #\Escape content #\Escape #\Escape))

(defun canvas-fill-bg ()
  "The canvas RGB, or NIL for a transparent theme or the :none profile."
  ;; Missing canvas RGB means terminal-default background. Own panel fills
  ;; still paint under a color profile; transparency is not colorlessness.
  (unless (eq (effective-color-profile) :none)
    (visual-style-bg (style-for-tone :background))))

(defun canvas-rgb ()
  "The rgb a cell with no ground of its own shows: the theme's canvas, or a
transparent theme's terminal's own background as it answered OSC 11 -- NIL
while the terminal has not said, which nothing guesses at."
  ;; What a fade goes out into and a wash lies over. A transparent theme's
  ;; nominal canvas is not what is on the screen: without this, a fade would
  ;; end on a black box and a word would come up out of it.
  (or (visual-style-bg (style-for-tone :background)) *terminal-canvas*))

(defun grounds-shown-p ()
  "Whether an answer's grounds paint -- its chips, bands and slabs: under
256 colours and more, where a fill a step off the canvas still is one. In
16 it would land on black or white."
  (member (effective-color-profile) '(:truecolor :ansi256)))

(defun seg-render (seg fill-bg &key force-bg shadow)
  "Paint SEG, including the session-id's opacity dim over the title palette."
  ;; SHADOW lays the shadow over the finished style: the text at less of the
  ;; light, the fill and every background left as they are.
  (let* ((tone (or seg.tone :foreground))
         (session-id-p (eq tone :session-id))
         (style (style-for-tone
                 (if session-id-p :session-title tone))))
    (when session-id-p
      (let ((fg style.fg) (bg (or style.bg (visual-style-bg (style-for-tone :background)))))
        (when (and fg bg)
          (setf style (style-with
                       style :fg (blend-rgb
                                  fg bg 63)))))) ; Zig's 4.5:1 contrast floor
    (when seg.dim
      (setf style (dim-style style)))
    (when (or seg.bold seg.italic seg.underline seg.strike)
      (setf style (style-with style :bold (or seg.bold style.bold)
                                    :italic (or seg.italic style.italic)
                                    :underline (or seg.underline style.underline)
                                    :strike (or seg.strike style.strike))))
    ;; A shadowed or dimmed row is its words at less light, standing on
    ;; nothing of its own.
    (when (and seg.ground (not seg.dim) (not shadow) (grounds-shown-p))
      (setf style (style-with style :bg (visual-style-bg (style-for-tone seg.ground)))))
    (when (and fill-bg (or force-bg (null style.bg))) (setf style (style-with style :bg fill-bg)))
    (when (and shadow (not (eq tone :thought)))
      ;; A thought's disclosure label is chrome, not its shadowed prose.
      (setf style (shadow-style style)))
    (let ((painted (styled seg.text style)))
      (if (and (stringp seg.link)
               (not (eq (effective-color-profile) :none)))
          (osc8-wrap painted seg.link)
          painted))))

(defun segs-text (segs)
  "The plain text SEGS paint."
  ;; The hit-test and copy lane derives from the painted segments through here
  ;; rather than being rebuilt beside them, so the two lanes cannot disagree
  ;; about what a row says.
  (apply #'concatenate 'string (mapcar #'seg-text segs)))

(defun segs-display-width (segs)
  "Columns SEGS occupy."
  (loop for s in segs sum (text-display-width s.text)))

(defun blank-frame-row (width bg &aux (spaces (make-string width :initial-element #\Space)))
  "A full-WIDTH canvas row: painted spaces under a profile, plain under :none."
  (if bg (styled spaces (vstyle :bg bg)) spaces))

(defun clip-segs (segs width &key mark)
  "SEGS when they paint within WIDTH columns; otherwise the longest prefix
of them that does."
  ;; MARK, when given, is the elision text painted in the cut seg's tone at
  ;; the edge (the chrome … of CLIP-TEXT-TO-WIDTH, the docked row's
  ;; gap-dot-pad ` … '); its own display width comes out of the budget, so a
  ;; mark wider than WIDTH is dropped and the cut stays bare. Without it the
  ;; cut is bare too, the Zig `.wrap = .none' panel truncation. The one row
  ;; painter's overflow guard: a row never exceeds the width it was asked for,
  ;; so a lane that cannot fit — the live turn bar naming a long model on a
  ;; narrow window — elides instead of spilling past the edge into the
  ;; implicit wrap that would shift every row below.
  (if (<= (segs-display-width segs) width)
      segs
      (let* ((mark (and mark (>= width (text-display-width mark)) mark))
             (budget (max 0 (- width (if mark (text-display-width mark) 0))))
             (used 0)
             (kept '()))
        (dolist (seg segs)
          (let ((w (text-display-width seg.text)))
            (cond ((<= (+ used w) budget)
                   (push seg kept)
                   (incf used w))
                  (t
                   (let ((head (take-display-prefix seg.text
                                                    (- budget used))))
                     (when (plusp (length head))
                       (push (copy-with seg seg :text head) kept))
                     (when mark
                       (push (copy-with seg seg :text mark :link nil) kept)))
                   (return)))))
        (nreverse kept))))

(defun render-row (prefix-segs body-segs fill-bg width &key shadow)
  "One painted viewport row, never wider than WIDTH."
  ;; A fill paints the row background to WIDTH; bare rows end at their content
  ;; (Zig lineWithChrome). SHADOW paints the row's text at less light; the
  ;; fill is not text and keeps its tone.
  (let* ((segs (clip-segs (append prefix-segs body-segs) width :mark "…"))
         (content (apply #'concatenate 'string
                         (mapcar (lambda (s) (seg-render s fill-bg :shadow shadow))
                                 segs))))
    (if fill-bg
        (pad-frame-row content (segs-display-width segs) width fill-bg)
        content)))

(defun fragment-segs (line s e &aux (text (lline-text line))
                                    (segs '())
                                    (pos s))
  "Paint segments for the fragment [S,E) of LINE: spans styled, hidden marker
runs skipped, unspanned runs in the line's base style."
  ;; An unspanned run paints as a span that sets nothing: the line's own style.
  (flet ((run (from to &optional (span (load-time-value (mspan 0 0))))
           (when (< from to)
             (push (make-seg :text (subseq text from to)
                             :tone (or span.tone line.tone)
                             :bold (or span.bold line.bold)
                             :italic (or span.italic line.italic)
                             :dim (or span.dim line.dim)
                             :underline (or span.underline line.underline
                                            (and *hover-target* (equal span.link *hover-target*) t))
                             :strike span.strike
                             :link span.link
                             :ground span.ground
                             :glint span.glint)
                   segs))))
    (dolist (span line.spans)
      (let ((ss (max s span.start))
            (se (min e span.end)))
        (when (< ss se)
          (run pos ss)
          (unless span.hidden (run ss se span))
          (setf pos (max pos se)))))
    (run pos e))
  (nreverse segs))

(defparameter *aside-min-width* 40
  "Narrowest frame a line's aside stands in; below it the words take the row.")

(defun lline-aside-shown (line width)
  "LINE's aside SEG where WIDTH has room for it, else NIL."
  (and line.aside (>= width *aside-min-width*) line.aside))

(defun lline-widths (line width)
  "(values first-row-width continuation-width) for LINE at frame WIDTH."
  (bind (((_ _ first-indent cont-indent)
          (body-gutter line.role))
         (aside (lline-aside-shown line width)))
    (values (max 1 (- width first-indent *transcript-right-padding*
                      (if aside (+ 2 (text-display-width (seg-text aside))) 0)))
            (max 1 (- width cont-indent line.hang
                      *transcript-right-padding*)))))

(defun lline-fragments (line width &aux (text (lline-text line)))
  "LINE's rows at frame WIDTH, (START . END) each: broken where its painted
words fit, its hidden markers counted as nothing."
  (multiple-value-bind (first-width cont-width) (lline-widths line width)
    (break-fragments text first-width cont-width
                     :soft t
                     :hidden (loop with hidden = nil
                                   for span in line.spans
                                   when span.hidden
                                     do (fill (or hidden (setf hidden (make-array (length text)
                                                                                  :element-type 'bit
                                                                                  :initial-element 0)))
                                              1 :start span.start :end (min span.end (length text)))
                                   finally (return hidden)))))

;;; Every span carrying the same URI underlines while it holds — the
;;; terminal-shaped answer to a cursor that changes shape, and the one thing
;;; that tells a reader a path is a thing to click before they click it. Read
;;; in FRAGMENT-SEGS and revalidated by the render memo, so the only rows that
;;; repaint under a moving pointer are the rows with targets.
(defvar *hover-target* nil
  "The target the pointer is resting on this render (RENDER-STATE binds it
from the model), or NIL.")

;;; LLINE-ROWS records them in the walk that builds the row, so a click finds
;;; the link under it (LINK-AT-POSITION) whatever the color profile paints —
;;; the OSC-8 wrap only rides a painting profile. Keyed on the row string
;;; itself, which the render memo keeps EQ-stable across frames and every lane
;;; after it carries untouched; weak, so a row the transcript dropped takes
;;; its entry with it.
(nlk:define-side-table plain-row-links (:weakness :key :synchronized t)
  "Plain transcript row or final chrome row -> ((START END TARGET) ...),
character ranges in its style-free text. TARGET is a URI or (:COMMAND . NAME).")

(defun link-plain-row (row links)
  "ROW, with LINKS recorded as its PLAIN-ROW-LINKS when there are any."
  (when links (setf (plain-row-links row) links))
  row)

(defun seg-links (segs start)
  "The (START END TARGET) ranges of SEGS' linked segs, SEGS laid from column START."
  (loop for seg in segs
        for from = start then end
        for end = (+ from (length (seg-text seg)))
        when (seg-link seg) collect (list from end (seg-link seg))))

(defparameter +ground-fade-floor+ 8
  "The fewest cells a ground's fade takes, short of a ground too short to
hold them.")

(defun ground-segs (segs ground text-end width)
  "SEGS, a row's paint, standing on GROUND, (TONE FROM TO FADE) (LLINE-GROUND):
TONE's background from column FROM below TO -- two past TEXT-END, the
words' end, for :TEXT, the text's right edge for :EDGE -- and no further,
nor past the edge. Under truecolor, over the canvas the terminal shows
(CANVAS-RGB), the last half of that span (at least +GROUND-FADE-FLOOR+
cells, at most FADE) goes out into it, under the last words; while that
canvas is unknown the span stands solid. A seg standing on its own ground
keeps it; a row short of the ground's end is padded to it."
  ;; A band is as long as its words: a fixed 36-cell fade past them runs a
  ;; short heading's band four times its length and a lead run's under the
  ;; prose after it. A fade toward a canvas the terminal does not show would
  ;; end on a box of the wrong colour: so it goes out into the canvas that
  ;; shows or not at all, and always all the way.
  (destructuring-bind (tone from to fade) ground
    (let* ((edge (- width *transcript-right-padding*))
           (canvas (canvas-rgb))
           (want (case to (:text (+ text-end 2)) (:edge edge) (t to)))
           (end (max (min from edge) (min want edge)))
           (fade (if (and fade canvas (eq (effective-color-profile) :truecolor))
                     (max 0 (min fade (- end from 1) (max +ground-fade-floor+ (ceiling (- end from) 2))))
                     0))
           (to (- end fade))
           (fill (visual-style-bg (style-for-tone tone)))
           (shown (segs-display-width segs))
           (column 0))
      ;; A cell's key says what it stands on: nothing, the solid ground, or
      ;; -- each its own -- a cell of the fade.
      (flet ((key (at) (cond ((or (< at from) (>= at end)) :none) ((< at to) :solid) (t at)))
             (ground (key) (case key
                             (:none nil)
                             (:solid tone)
                             (t (vstyle :bg (blend-rgb fill canvas (round (* 100 (- end key 1)) fade)))))))
        (loop for seg in (if (< shown end)
                             (append segs (list (make-seg :text (make-string (- end shown) :initial-element #\Space))))
                             segs)
              nconc (loop with text = seg.text
                          with start = 0
                          with key = (key column)
                          for i from 0 below (length text)
                          for here = (key column)
                          unless (eql here key)
                            collect (copy-with seg seg :text (subseq text start i) :ground (or seg.ground (ground key)))
                              into pieces
                            and do (setf start i key here)
                          do (incf column (char-display-width (char text i)))
                          finally (return (nconc pieces (list (copy-with seg seg :text (subseq text start)
                                                                             :ground (or seg.ground (ground key))))))))))))

(defun lline-rows (line width)
  "Wrap LINE at WIDTH => (values PAINTED PLAIN GLINTS): the painted rows,
the style-free text of each one, which is the mouse hit-test and
selection-copy source, and each row's glints, ((X0 X1 AT) ...) the cells a
glint crosses and the time it set off (GLINT-OVERLAY)."
  ;; One walk over one set of fragment segs feeds both lanes, so the hit-test
  ;; columns cannot drift away from the paint; the same walk records a plain
  ;; row's links on it (*PLAIN-ROW-LINKS*). The gutter mark paints on the
  ;; first row; continuations indent to the role's continuation column plus
  ;; the hang, the line's HANG-MARK painted over the head of the hang. A row
  ;; stands on the line's ground, a heading's band or a code block's slab:
  ;; every row of it, or, for a ground ending at a column, the first.
  (bind (((lead mark first-indent cont-indent)
          (body-gutter line.role)))
    (let ((fill-bg (or (and line.fill
                            (visual-style-bg (style-for-tone line.fill)))
                       ;; Unfilled rows still paint the canvas to full width:
                       ;; the Zig frame memset means no row ever ends on the
                       ;; terminal's default background (NIL under :none, so
                       ;; plain-text goldens keep their ragged right edge).
                       (canvas-fill-bg)))
          (ground (and line.ground (not line.shadow) (not line.dim) (grounds-shown-p) line.ground))
          (gutter (list (make-seg :text lead)
                        (make-seg :text mark
                                  :tone (or line.gutter-tone
                                            line.tone)
                                  :italic line.gutter-italic)))
          (cont (flet ((blanks (count)
                         (make-seg :text (make-string (max 0 count) :initial-element #\Space))))
                  (let ((marks (alexandria:ensure-list line.hang-mark)))
                    (list* (blanks cont-indent)
                           (append marks (list (blanks (- line.hang (segs-display-width marks))))))))))
      (if line.rule
          ;; A full-width thematic-break rule (Zig markdown_rule_fill).
          (let ((rule (make-string (lline-widths line width) :initial-element #\─)))
            (values (list (render-row (list (make-seg :text lead))
                                      (list (make-seg :text rule :tone line.tone :dim line.dim))
                                      fill-bg width
                                      :shadow line.shadow))
                    (list (concatenate 'string lead rule))
                    (list nil)))
          (loop with aside = (lline-aside-shown line width)
                for (s . e) in (lline-fragments line width)
                for firstp = t then nil
                for prefix = (if firstp gutter cont)
                for words = (fragment-segs line s e)
                for segs = (if (and firstp aside)
                               ;; The aside ends where the words may: the gap
                               ;; before it is the rest of the first row.
                               (append words
                                       (list (make-seg :text (make-string
                                                              (max 2 (- width first-indent *transcript-right-padding*
                                                                        (segs-display-width words)
                                                                        (text-display-width (seg-text aside))))
                                                              :initial-element #\Space))
                                             aside))
                               words)
                for paint = (if (and ground (or firstp (not (integerp (third ground)))))
                                (ground-segs (append prefix segs) ground
                                             (+ (segs-display-width prefix) (segs-display-width words))
                                             width)
                                (append prefix segs))
                collect (render-row nil paint fill-bg width :shadow line.shadow)
                  into painted
                collect (let ((prefix (if firstp
                                          (concatenate 'string lead mark)
                                          (segs-text cont))))
                          ;; Narrow before attaching EQ-keyed link ranges:
                          ;; the string returned is the click lane's key.
                          (link-plain-row (nlk:narrow-string (uiop:strcat prefix (segs-text segs)))
                                          (seg-links segs (length prefix))))
                  into plains
                collect (loop for seg in paint
                              for from = 0 then to
                              for to = (+ from (text-display-width seg.text))
                              when (and seg.glint (not seg.dim) (not line.shadow) (grounds-shown-p))
                                collect (list from to seg.glint))
                  into glints
                finally (return (values painted plains glints)))))))

(defun gutter-mark-p (char)
  "Whether CHAR is a mark the transcript's gutter paints rather than a word:
a role's mark (*BODY-GUTTERS*: a disclosure's ▸, a notice's ■, tool
output's ⎿ ...), or the ▌ edge and the ↦ a wrapped line goes on under."
  (or (find char "▌↦")
      (loop for (nil nil mark) in *body-gutters*
            thereis (and (plusp (length mark)) (char= char (char mark 0))))))

(defun without-gutter-marks (row)
  "ROW, one frame row's plain text, with the gutter marks it opens on
blanked (GUTTER-MARK-P): a disclosure's ▸, a notice's ■, tool output's ⎿,
a code block's and a quote's edges and the ↦ a wrapped line goes on under
are the gutter's, not the words', and a copy leaves them behind."
  ;; The copy reads what was painted (FRAME-SELECTION-TEXT), so the marks come
  ;; out there, a blank for each column they held, and the columns a selection
  ;; is cut at stay where they were. Prose that opened on one of these glyphs
  ;; would lose it the same way; a model writes none.
  (let ((row (copy-seq row)))
    (loop for i from 0 below (length row)
          for ch = (char row i)
          while (or (char= ch #\Space) (gutter-mark-p ch))
          do (setf (char row i) #\Space))
    row))

(defun llines-rows (llines width)
  "=> (values PAINTED PLAIN) for a whole list of llines wrapped at WIDTH."
  (loop for line in llines
        for (rows texts) = (multiple-value-list (lline-render line width))
        append rows into painted
        append texts into plains
        finally (return (values painted plains))))

;;; ROW-BODY-LLINES keeps a row's unchanged logical lines EQ-stable across
;;; re-renders, so during streaming only the tail lline of the growing row
;;; pays wrap and paint; everything above it is a fixnum-and-EQ probe. Weak
;;; keys, so llines dropped with their row release their rendered rows.
(nlk:define-side-table lline-render-cache (:weakness :key)
  "Weak per-lline memo: lline -> #(width palette profile hovered-span
(rows plain-rows)).")

;;; The memo holds a line's painted rows for as long as the line lives. A
;;; line dies with its row's memos, which a full render keeps only for what
;;; it projects (FORGET-UNPROJECTED-ROWS); what the memo keeps is kept
;;; narrow, and past this many lines -- a line held some other way -- it
;;; starts over. Without the limit, a line held some other way would keep
;;; its painted rows indefinitely.
(defparameter *lline-render-cache-limit* 4096
  "Logical lines whose painted rows LLINE-RENDER keeps before it starts over.")

(defparameter +glint-ms+ 800
  "Milliseconds a glint takes to cross a chip that has just arrived.")

(nlk:define-side-table row-glints (:weakness :key)
  "A painted row -> the glints it carries, ((X0 X1 AT) ...): the cells of a
chip, past the row's own start, and the time it arrived (LLINE-ROWS), so the
frame's GLINT-OVERLAY finds it on screen.")

(defun lline-render (line width)
  "(values ROWS PLAIN-ROWS) for LINE at WIDTH, memoized on the lline's
identity."
  ;; The entry revalidates on width, theme palette, color profile and the
  ;; pointer: the render inputs LLINE-ROWS reads outside the lline itself. The
  ;; pointer only ever changes a line that paints the target it moved on or
  ;; off of, so the entry keys on the one span of the line's own that the
  ;; pointer rests on (usually none), and a pointer that never touched the
  ;; line leaves the entry standing -- crossing a path in one card does not
  ;; repaint every other card's. Callers must not mutate LINE or the returned
  ;; lists: the memo shares both across frames (the same discipline
  ;; ROW-DISPLAY-BLOCK already imposes).
  (when (> (hash-table-count *lline-render-cache*) *lline-render-cache-limit*)
    (clrhash *lline-render-cache*))
  (values-list
   (memoized (lline-render-cache line)
     (width *palette* *color-profile*
      (and *hover-target* (find *hover-target* line.spans :key #'mspan-link :test #'equal)))
     (multiple-value-bind (rows plain glints) (lline-rows line width)
       ;; Plain rows already carry their final representation and link ranges.
       ;; Copying them here would lose the identity the mouse lane reads.
       (let ((rows (mapcar #'nlk:narrow-string rows)))
         (loop for row in rows
               for glint in glints
               when glint do (setf (row-glints row) glint))
         (list rows plain))))))

;;; --- row chrome: transcript rows to logical lines --------------------------
;;; The per-kind table from src/tui/transcript.zig: assistant prose in plain
;;; foreground (NOT the assistant green — that tone marks identity chrome),
;;; reasoning as that same prose under a shadow (the text at less light
;;; on the same canvas, not a quieter tone and not a darker band), user
;;; prompts as filled blocks, tool headers bold-named
;;; with muted args,
;;; dimmed tool output behind a "⎿" gutter, notices as "■" system blocks, and
;;; failures as "■ Error:" diagnostic blocks.

(defun block-pad-lline (tone &aux (cache (load-time-value (make-hash-table :test 'eq))))
  "One blank padding row — the Zig block_padding viewport line every
transcript block owns above and below its body (render_plan.zig
collectTranscriptRows)."
  ;; TONE, when non-NIL, extends a filled block's background through the pad
  ;; so the block reads as a card rather than a bare painted line; NIL leaves
  ;; the pad on the canvas, which is what the unfilled styles (assistant
  ;; prose, reasoning, tool blocks) render. The :status role keeps the
  ;; plain-text projection at the Zig writer's single space. Interned per
  ;; tone: pads are immutable like every rendered lline (the hover pass copies
  ;; before it writes), so every block shares the same pad object and
  ;; LLINE-RENDER memoizes its paint once per theme.
  (alexandria:ensure-gethash tone cache (make-lline :text "" :role :status :fill tone)))

(defun notice-url-spans (text)
  "Link spans for every bare http(s) URL in TEXT."
  ;; A notice is not markdown -- nothing in it is parsed, and an organism
  ;; saying `**` means `**` -- so this is the one run of INLINE-SPANS a
  ;; notice gets: the scanner over BARE-URL-END, the link tone, and the
  ;; target the OSC-8 wrap and the click both read (MSPAN-LINK). Without it a
  ;; notice carries no spans at all, and the address a cell says -- the
  ;; one line an operator is there to copy -- paints as prose.
  (loop with position = 0
        for start = (position #\h text :start position :test #'char-equal)
        for end = (and start (bare-url-end text start 0))
        while start
        when end
          collect (mspan start end :tone :link :underline t :link (subseq text start end))
        do (setf position (or end (1+ start)))))

(defun system-notice-llines (text error-p &aux (tone (if error-p :diagnostic-block :system-block)))
  (loop for line in (or (split-lines text) (list ""))
        for firstp = t then nil
        collect (make-lline :text line :tone tone :italic t
                            :role (if firstp (if error-p :diagnostic :system) :system-cont)
                            :gutter-tone tone :gutter-italic t
                            :spans (notice-url-spans line)
                            :hang (or (list-marker-end line)
                                      (nth-value 1 (ppcre:scan "^ *(?:\\S+ {2,}|[•·] )?" line)))
                            :fill tone)))

;;; The group pass classifies every tool row on every projection, so a row's
;;; words are computed once per change of the fields they read.
(nlk:define-side-table row-activity-cache (:weakness :key)
  "Weak per-row memo for ROW-ACTIVITY: row -> #(name args metadata error-p
activity).")

(defun row-activity (row)
  "ROW's ACTIVITY (activity.lisp): what its tool call did, in words — or NIL
for a row that is no tool call, or a tool that module has no words for."
  ;; The reducers replace these fields rather than mutate them, so identity is
  ;; the change signal.
  (when (eq :tool-call row.kind)
    (let ((name row.tool-name) (args row.tool-args) (metadata row.metadata) (error-p row.error-p))
      (memoized (row-activity-cache row) (name args metadata error-p)
        (tool-arguments-activity name args :metadata metadata :error-p error-p)))))

(nlk:define-side-table row-load-errors (:weakness :key)
  "Live-only shell-load failures on their originating Eval row, never durable tool metadata.")

(defun ingest-tool-load-error (state call-id turn-id text)
  "Attach a shell-only load failure to its exact Eval call. Return T if seated, NIL otherwise."
  (when (and (stringp call-id) (plusp (length call-id)))
    (when-let (row (find-if (lambda (row)
                             (and (eq :tool-call (transcript-row-kind row))
                                  (equal "eval" (transcript-row-tool-name row))
                                  (equal call-id (transcript-row-tool-call-id row))
                                  (equal turn-id (transcript-row-turn-id row))))
                           (transcript-rows state)))
      (setf (row-load-errors row) (append (row-load-errors row) (list text)))
      t)))

(defun tool-failure-text (row)
  "A tool's status suffix plus its explicit error code; shell-load errors remain live-only."
  ;; Only a code the result carries rides the word: without this, a stand-in
  ;; `EVAL-FAILED' behind `failed' would say the same thing twice.
  (let* ((shell-error (row-load-errors row))
         (status (if shell-error "error" (transcript-row-tool-status row)))
         (code (and (equal "eval" (transcript-row-tool-name row))
                    (if shell-error "SHELL-LOAD-FAILED"
                        (json-value (transcript-row-metadata row) :text "error" "code")))))
    (format nil "~a~@[ [~a]~]"
            (tool-status-text status (row-exit-document row)) code)))

(defun exit-end (exited &optional words)
  "EXEC-END of EXITED, an exit's document -- with WORDS, EXEC-END-WORDS."
  (funcall (if words #'exec-end-words #'exec-end)
           (json-value exited :integer "exit_code") (json-value exited :integer "shell_exit")))

(defun tool-status-text (status &optional exited)
  "The status word a tool card's header ends with, for the row's STATUS —
for a call that ran on in the background, how it ended by EXITED, the exit's
background_shell document: `exited 2', `failed', `interrupted', and nothing
for one that ended done, as a foreground call that finished says nothing."
  ;; The bare `exited' says that it ended and never how.
  (if (and (equal status "exited") (hash-table-p exited))
      (let ((words (exit-end exited t)))
        (if (equal words "done") "" (format nil " ~a" words)))
      (or (cdr (assoc status '(("running" . " running") ("error" . " failed") ("exited" . " exited")
                               ("stopped" . " stopped") ("background" . " in the background"))
                      :test #'equal))
          "")))

(defun tool-command-llines (row &key (status-text ""))
  "The call's own text as the card showed it before it had words: the
decoded eval snippet one viewport line per snippet line, a command on one line,
or the tool name bold with its raw arguments muted — STATUS-TEXT after the
last line."
  ;; The command is TOOL-COMMAND-STRING over the row's arguments, the kernel's
  ;; decode, shared with the checkpoint listing. A command line carries
  ;; SOURCE's syntax spans over the same-length TEXT: the flat branch
  ;; tokenizes the unflattened command — the newlines are what tell the lexer
  ;; where a comment ends. The status suffix is the one every card title ends
  ;; with (APPEND-STATUS-TEXT).
  (let* ((name (or (json-value row.tool-name :text) "…"))
         (command (tool-command-string row.tool-args))
         (args (substitute #\Space #\Newline (or row.tool-args "")))
         (llines (cond ((null command)
                        (list (append-lline-run
                               (make-lline :text name :tone :muted
                                           :spans (list (mspan 0 (length name)
                                                               :tone :tool :bold t)))
                               (if (plusp (length args)) (concatenate 'string " " args) "")
                               :tone :muted)))
                       ((equal name "eval") (command-llines command))
                       (t (list (make-lline :text (substitute #\Space #\Newline command)
                                            :tone :syntax-function
                                            :spans (command-token-spans command)))))))
    (append-status-text (car (last llines))
                        (if (row-load-errors row) "error" row.tool-status)
                        status-text
                        (json-value row.metadata :object "background_shell"))
    llines))

(defun command-llines (command &key dim)
  "COMMAND one lline per line, each painted by its own syntax spans: an eval
snippet as its card shows it, and an exec's command in the band and its view."
  (mapcar (lambda (line)
            (make-lline :text line :tone :syntax-function :dim dim
                        :spans (command-token-spans line)))
          (or (split-lines command) (list ""))))

(defun title-phrases-lline (phrases &key (verb-tone :tool)
                                 &aux (title (make-lline :text "" :tone :foreground)))
  "One title lline over PHRASES, (VERB TEXT DETAIL TARGET) each joined by
commas: the verbs bold in VERB-TONE, what they acted on in the
foreground, the aside after it, and — where the phrase names an address
(ACTIVITY-SOURCE-TARGET) — a span over exactly those words carrying what a
click opens."
  ;; The activity classifier decided what the source IS; this writes the words
  ;; and spans each run as it lands (APPEND-LLINE-RUN), so no path is ever read
  ;; back out of a title.
  (loop for (verb text detail target) in phrases
        for first = t then nil
        do (unless first (append-lline-run title ", "))
           (when verb
             (append-lline-run title verb :tone verb-tone :bold t)
             (when (plusp (length text)) (append-lline-run title " ")))
           (append-lline-run title text :link (span-target-uri target))
           (when detail (append-lline-run title (format nil " (~a)" detail))))
  title)

(defun append-lline-run (line text &rest style)
  "Write TEXT onto LINE's end as a span of its own under STYLE. => LINE."
  ;; The one way an lline grows: a title's phrases, a card's status word, a
  ;; diff header's stats. An empty TEXT leaves LINE exactly as it was, and a
  ;; STYLE that sets nothing adds the text in the line's own style, unspanned.
  (when (plusp (length text))
    (let ((start (length (lline-text line))))
      (setf (lline-text line) (concatenate 'string (lline-text line) text))
      (when (loop for (nil value) on style by #'cddr thereis value)
        (alexandria:appendf line.spans (list (apply #'mspan start (length line.text) style))))))
  line)

(defun append-title-detail (line text)
  "Append a muted detail in the shared transcript content column.
Compact labels hang under their detail; compound activity titles keep their layout."
  ;; TEXT is a note, a comment or a summary as its source holds it, folded to
  ;; one line here; a source with none (NIL) leaves LINE as it was.
  (when-let (detail (and text (nlk:one-line text)))
    (let* ((width (text-display-width (lline-text line)))
           (compact (and (eq *tool-detail* :standard) (<= width 6)))
           (hang (- *transcript-detail-column* (nth-value 2 (body-gutter :prose))))
           (pad (if compact (- hang 1 width) 0))
           (separator (if compact " " " · ")))
      (append-lline-run line (format nil "~va~a~a" pad "" separator detail) :tone :muted)
      (when (and compact (zerop (lline-hang line)))
        (setf (lline-hang line) hang))))
  line)

(defun append-status-text (title status status-text &optional exited)
  "Write STATUS-TEXT onto TITLE's end, spanned in the tone the tool STATUS
earns — :danger for an error or for a call that ran on in the background and
ended any way but done or stopped by EXITED, its exit's background_shell
document; :tool for a call still running, here or in the background; :muted
otherwise."
  ;; A card that is simply complete has no status text and TITLE is left
  ;; alone. Answers TITLE. A background `exited 2' read grey beside a
  ;; foreground `failed' in red; a grey `running' read as the rest of its
  ;; grey line, the one live word on it.
  (append-lline-run title status-text
                    :tone (cond ((or (equal status "error")
                                     (and (equal status "exited")
                                          (hash-table-p exited)
                                          (not (member (exit-end exited)
                                                       '("done" "stopped") :test #'equal))))
                                 :danger)
                                ((member status '("running" "background") :test #'equal) :tool)
                                (t :muted))))

(defun tool-note (row)
  "The call's comment: eval's leading ;; note, or an explicit tool argument."
  (or (when-let (activity (row-activity row)) (activity-note activity))
      (when-let (args (ignore-errors (shasht:read-json row.tool-args)))
        (or (json-value args :text "description") (json-value args :text "comment")))))

(defun disclosure-hover-lline (line)
  "A title accented on hover, with its shared line and spans left untouched."
  (copy-with lline line :tone :disclosure-hover :shadow nil
             :spans (mapcar (lambda (span) (copy-with mspan span :tone :disclosure-hover))
                            (lline-spans line))))

(defun row-edited-p (row)
  "Whether this call's durable result carries captured file changes."
  (and (eq :tool-call (transcript-row-kind row))
       (plusp (length (json-array (transcript-row-metadata row) "workspace_diff" "files")))
       t))

(defun tool-summary-words (row &aux (activity (row-activity row)))
  "What the call did, in words: its note, the files it edited, or its
activity's title. NIL for a call with none."
  (or (tool-note row)
      (when (row-edited-p row)
        (let ((files (json-array row.metadata "workspace_diff" "files")))
          (if (= 1 (length files))
              (json-value (aref files 0) :string "path")
              (format nil "~d files" (length files)))))
      (and activity (activity-title activity))))

(defun tool-summary-lline (row)
  "The tool's own identity, note and status, even when its result includes edits."
  (let* ((name (or (json-value row.tool-name :text) "…"))
         (title (title-phrases-lline (list (list (if (equal name "eval") "Eval" name) "" nil nil))))
         (summary (tool-summary-words row)))
    (append-title-detail title summary)
    (append-status-text title (if (row-load-errors row) "error" row.tool-status)
                        (tool-failure-text row)
                        (row-exit-document row))
    title))

(defun fork-tree-llines (row width &key (indent 0))
  "Call ROW's forks, one line each on the rails /sessions draws, while the
gateway says how they stand (session_fork_progress, client.lisp): a running
fork's calls and its newest note, an ended one's mark, time and the first
line of its answer. NIL for a call that forked nothing."
  ;; Without these, a call that forked stands as one line for minutes while
  ;; its forks work in sessions of their own, and nothing says what they do.
  (when-let (forks (and (hash-table-p row.metadata) (json-value row.metadata :array "forks")))
    (let* ((last (1- (length forks)))
           (names (map 'list (lambda (fork) (or (json-value fork :text "label") "")) forks))
           (room (min 24 (reduce #'max names :key #'text-display-width :initial-value 0)))
           ;; Each fork's calls, time and -- for one that did not complete --
           ;; how it ended: one cell, as wide as the widest, so every note
           ;; starts in one column (`1 call' and `12 calls' moved it).
           (stats (map 'list (lambda (fork &aux (status (json-value fork :string "status"))
                                                 (ms (json-value fork :integer "ms")))
                               (cons (format nil "  ~2d call~:p~@[ · ~a~]" (or (json-value fork :integer "calls") 0)
                                             (and ms (format-turn-elapsed ms)))
                                     (unless (member status '("running" "completed") :test #'equal)
                                       (format nil " · ~a" status))))
                       forks))
           (stat-room (reduce #'max stats :key (lambda (cell) (+ (text-display-width (car cell))
                                                                 (text-display-width (or (cdr cell) ""))))
                                          :initial-value 0)))
      (loop for fork across forks
            for name in names
            for (counts . ending) in stats
            for index from 0
            collect (let* ((status (json-value fork :string "status"))
                           (running (equal status "running"))
                           (done (equal status "completed"))
                           (words (json-value fork :text (if running "note" "answer")))
                           (shown (clip-text-to-width name room))
                           (line (make-lline :text (make-string indent :initial-element #\Space)
                                             :tone :muted)))
                      (append-lline-run line (if (= index last) "╰ " "├ ") :tone :muted)
                      (append-lline-run line (cond (running "○") (done "✓") (t "✗"))
                                        :tone (cond (running :tool) (done :success) (t :danger)))
                      (append-lline-run line (format nil " fork ~d  " (json-value fork :integer "n")) :tone :muted)
                      (append-lline-run line (concatenate 'string shown
                                                          (make-string (max 0 (- room (text-display-width shown)))
                                                                       :initial-element #\Space))
                                        :tone :foreground)
                      (append-lline-run line counts :tone :muted)
                      (when ending
                        (append-lline-run line ending :tone :danger))
                      (when words
                        (append-lline-run line (concatenate 'string
                                                            (make-string (max 0 (- stat-room (text-display-width counts)
                                                                                   (text-display-width (or ending ""))))
                                                                         :initial-element #\Space)
                                                            "  " (nlk:one-line words))
                                          :tone (if running :muted :foreground) :italic running))
                      (clip-lline line width))))))

(defun tool-header-llines (row &aux (activity (row-activity row))
                                    (status (if (row-load-errors row) "error" row.tool-status))
                                    (exited (json-value row.metadata :object "background_shell"))
                                    (status-text (tool-failure-text row)))
  "The tool card's header."
  ;; A call this image has words for (ROW-ACTIVITY) is titled by them, its
  ;; commands stand under the title while the title could not carry them
  ;; whole, and its snippet shows underneath only while the card is open
  ;; (DETAIL-OPEN-P) — the Lisp is one level down, not the first thing read. A
  ;; tool with no words keeps its name and raw arguments.
  (if (null activity)
      (if (tool-note row)
          (append (list (tool-summary-lline row))
                  (when (detail-open-p (tool-row-expand-key row))
                    (tool-command-llines row)))
          (tool-command-llines row :status-text status-text))
      (let* ((title (if (activity-code-title-p activity)
                        (make-lline :text activity.head :tone :syntax-function
                                    :spans (command-token-spans activity.head))
                        (if (activity-entries activity)
                            (title-phrases-lline
                             (activity-entries-phrases (activity-entries activity)
                                                      :running (member status '("running" "background")
                                                                       :test #'equal)))
                            (tool-summary-lline row)))))
        (when (activity-entries activity)
          (append-title-detail title (tool-note row)))
        ;; A code title has no entries (ACTIVITY-CODE-TITLE-P), so either arm, never both.
        (when (or (activity-entries activity) (activity-code-title-p activity))
          (append-status-text title status status-text exited))
        (append-row-diff-stats title row)
        (append
         (list title)
         ;; `$ <command>' lines for the commands a closed card's title could not carry whole.
         (when (and (not (detail-open-p (tool-row-expand-key row))) (eq :command activity.family))
           (loop for entry in (activity-entries activity)
                 for source = (second entry)
                 when (and (eq :command (first entry))
                           (stringp source)
                           (plusp (length (string-trim '(#\Space #\Tab #\Newline)
                                                       source)))
                           (not (search source title.text)))
                   ;; The `$ ' line is the source's first line, trimmed — the head
                   ;; names what the call runs, and a heredoc's body is not a
                   ;; source (the receipt's own rule). Tabs become spaces so the
                   ;; painted row cannot jump.
                   collect (make-lline
                            :text (format nil "$ ~a"
                                          (string-trim
                                           '(#\Space #\Tab)
                                           (substitute #\Space #\Tab (first-line source))))
                            :tone :muted :dim t :role :tool-cont)))
         (when (detail-open-p (tool-row-expand-key row))
           (let ((snippet (tool-command-llines row)))
             ;; A one-line snippet already stands as its own title.
             (unless (and (activity-code-title-p activity)
                          (= 1 (length snippet))
                          (equal (lline-text (first snippet))
                                 activity.head))
               (mapcar (lambda (line) (copy-with lline line :role :tool-cont :dim t))
                       snippet))))))))

(defun cut-lline (line &key (start 0) (end (length (lline-text line))) role)
  "A copy of LINE carrying only the [START,END) slice of its text and of its
spans, shifted to the slice, with ROLE replacing the copy's role when
supplied — the head and tail cuts the tool output window and the thinking
tail make at a fragment boundary."
  (copy-with lline line
             :text (subseq (lline-text line) start end)
             :spans (loop for span in (lline-spans line)
                          for from = (max start span.start)
                          for to = (min end span.end)
                          when (< from to)
                            collect (copy-with mspan span :start (- from start) :end (- to start)))
             :role (or role line.role)))

(defun llines-window (llines width budget &key from-end role &aux (rows 0) (kept '()))
  "The first BUDGET wrapped rows of LLINES at WIDTH — the last BUDGET with
FROM-END — the line that crosses the budget cut at a fragment boundary
(CUT-LLINE, the cut taking ROLE when given)."
  ;; The one windowing walk: the tool output window's head and tail and the
  ;; thinking tail are each a window of it.
  (dolist (line (if from-end (reverse llines) llines))
    (when (>= rows budget) (return))
    (let ((fragments (lline-fragments line width))
          (keep (- budget rows)))
      (push (cond ((<= (length fragments) keep) line)
                  (from-end (cut-lline line :role role
                                            :start (car (nth (- (length fragments) keep)
                                                             fragments))))
                  (t (cut-lline line :end (cdr (nth (1- keep) fragments)) :role role)))
            kept)
      (incf rows (min keep (length fragments)))))
  (if from-end kept (nreverse kept)))

(defun omission-text (hidden)
  "The five-row window's omission count and its per-card click control."
  (format nil "+~d line~:p (click to expand)" hidden))

(defun command-rows (command width &key open)
  "COMMAND's own lines as rows of segs WIDTH columns wide, painted as a card
paints an eval snippet: every row when OPEN or when they are five or fewer, else
the five-row window a closed card's output shows -- two head rows, the
omission line, two tail rows. => (values ROWS ALL), ALL the rows it takes
whole."
  ;; The background band and the exec view show a pending exec's command as
  ;; the call wrote it; flattened to one line it would be unreadable past a
  ;; few forms.
  (let ((rows (loop for line in (command-llines command :dim t)
                    append (loop for (start . end) in (break-fragments (lline-text line) width width)
                                 collect (fragment-segs line start end)))))
    (values (if (or open (<= (length rows) 5))
                rows
                (append (subseq rows 0 2)
                        (list (list (make-seg :text (omission-text (- (length rows) 4)) :tone :muted)))
                        (last rows 2)))
            (length rows))))

(defun stop-words (stop &aux (by (json-value stop :string "by"))
                             (after (json-value stop :integer "after_ms")))
  "The line a stopped call's card ends on, from its STOP document
(STOP-DOCUMENT): who stopped it and after how long."
  (format nil "~a~@[ after ~a~]"
          (cond ((equal by "operator") "stopped by you")
                ((equal by "shutdown") "stopped: the gateway shut down while this ran")
                ((equal by "model") "stopped by the model")
                (t (format nil "stopped~@[: ~a~]" (json-value stop :string "reason"))))
          (and after (not (equal by "shutdown")) (format-turn-elapsed after))))

(defun tool-output-llines (row width &aux (exited (row-exit-document row))
                                          (stop (or (json-value row.metadata :object "stopped")
                                                    (json-value exited :object "stopped")
                                                    (json-value row.metadata :object
                                                                "background_read" "stopped"))))
  "Tool output behind the ⎿ gutter, dimmed, collapsed to the Zig 5-visual-row
window: two head rows, one \"⋮ +N lines (click to expand)\" clause,
two tail rows. A
call that ran on in the background and exited shows the last lines its exit
kept instead, in the danger tone unless it ended done, and the report of the
condition it failed on after them, whole."
  ;; The budget counts wrapped visual rows at the tool body width, so one long
  ;; line cannot inflate the preview (Zig toolOutputPreviewForWidth). The
  ;; result reads as TOOL-RESULT-SHOWN has it, the page's reading too: a
  ;; run/write_stdin envelope as its stream, an eval result as the
  ;; text it produced without the harness's notes to the model, a background
  ;; job's read as what the job wrote; any other text unchanged. An exited
  ;; background call's result is its handoff, the output as it stood at the
  ;; yield: the card says nothing of how the exec went on.
  ;; A stopped call shows what it had written and who stopped it, never the
  ;; model's text of the abort.
  (let* ((lines (split-lines
                 (trim-block-text
                  (if-let (kept (or exited stop))
                    (kept-tail-text (or (json-value kept :string "tail") "")
                                    (json-value kept :boolean "tail_cut"))
                    (tool-result-shown row.tool-name row.text row.metadata)))))
         (report (and exited (split-lines (json-value exited :string "error"))))
         ;; A stop is no failure: it reads in the ordinary tone.
         (error-p (if exited
                      (not (member (exit-end exited) '("done" "stopped") :test #'equal))
                      row.error-p))
         (llines (loop for line in lines
                       for firstp = t then nil
                       collect (make-lline
                                :text line :tone (if error-p :danger :foreground) :dim t
                                :role (if firstp :tool :tool-cont)
                                :gutter-tone (if error-p :danger :muted))))
         (total (reduce #'+ llines :key (lambda (ll) (length (lline-fragments ll width))))))
    (append
     (if (or (detail-open-p (tool-row-expand-key row))
             (<= total 5))              ; the Zig max_output_preview_lines budget
         llines
         ;; Two visual rows each side of the omission line.
         (append
          (llines-window llines width 2)
          (list (make-lline :text (omission-text (- total 4))
                            :tone :muted :role :tool-omission
                            :gutter-tone :muted))
          (llines-window llines width 2 :from-end t :role :tool-cont)))
     (loop for line in report
           for firstp = (null llines) then nil
           for text = (if (eq line (first report)) (format nil "error: ~a" line) line)
           collect (make-lline :text text :tone :danger
                               :role (if firstp :tool :tool-cont) :gutter-tone :danger))
     (when stop
       (list (make-lline :text (stop-words stop) :tone :muted
                         :role (if llines :tool-cont :tool) :gutter-tone :muted)))
     (loop for line in (mapcan #'split-lines (row-load-errors row))
           collect (make-lline :text line :tone :danger :dim t
                               :role :tool-cont :gutter-tone :danger)))))

(defun row-exit-document (row &aux (shell (json-value row.metadata :object "background_shell")))
  "The exit's background_shell document on ROW, a call that ran on in the
background and exited (APPLY-BACKGROUND-SHELL-EXIT), else NIL."
  (and (equal "exited" (json-value shell :string "status")) shell))

;;; --- workspace diff documents (tool_result workspace_diff metadata) ---------
;;; The line-diff document over a tool call's workspace bracket. The producer
;;; (snapshot.lisp WORKSPACE-DIFF-DOCUMENT) diffs the call's pre/post capture
;;; trees and ships bounded per-file unified patches in the result's display
;;; metadata; this projection parses them back into the edit_diff.zig
;;; document shape — right-aligned numbered gutters, removed-before-added
;;; hunks (git's own ordering), word-level emphasis on index-paired lines,
;;; "..." elision rows between hunks, per-file path headers carrying the
;;; " +N -M" stats. Both the live stream and replay read the same fact, so
;;; the document renders identically on both paths.

(defstruct (diff-line (:constructor diff-line (sign number text)))
  "One diff document line: SIGN is :added, :removed, :context, or :meta;
NUMBER the file line number (NIL renders a blank number column); RANGES
the word-level emphasis (START . END) char ranges into TEXT, ascending."
  sign number (text "" :type string) (ranges '()))

(nlk:access (a diff-line) (r diff-line))

(defun diff-word-tokens (line &aux (tokens '()))
  "LINE split into alternating whitespace and word runs as (START END
WORD-P) triples — a space is ' ' or tab only (Zig tokenizeWords)."
  ;; NIL past the token cap: the pair keeps whole-line coloring.
  (ppcre:do-matches (start end "[ \\t]+|[^ \\t]+" line (nreverse tokens))
    (when (>= (length tokens) 128)      ; the Zig max_word_tokens cap
      (return-from diff-word-tokens nil))
    (push (list start end (not (member (char line start) '(#\Space #\Tab)))) tokens)))

(defun diff-word-ranges (line counterpart &aux (tokens-a (diff-word-tokens line))
                                               (tokens-b (diff-word-tokens counterpart)))
  "Word-level change ranges of LINE against its index-paired COUNTERPART:
(START . END) char ranges with byte-adjacent unmatched tokens merged into
one emphasis span."
  ;; NIL when the pair shares no common word token — the Zig
  ;; appendWordHighlights similarity gate, so fully rewritten lines keep plain
  ;; whole-line coloring instead of an all-emphasis smear.
  (when (and tokens-a tokens-b)
    (let* ((va (coerce tokens-a 'vector))
           (vb (coerce tokens-b 'vector))
           (token= (lambda (ta tb)
                     (string= line counterpart
                              :start1 (first ta) :end1 (second ta)
                              :start2 (first tb) :end2 (second tb))))
           (n (length va))
           (m (length vb))
           ;; Suffix-LCS, the Zig edit_diff shape: (aref table i j) is VA[i..] vs VB[j..].
           (table (make-array (list (1+ n) (1+ m)) :initial-element 0))
           (ranges '())
           (matched-word nil)
           (i 0)
           (j 0))
      (loop for i from (1- n) downto 0
            do (loop for j from (1- m) downto 0
                     do (setf (aref table i j)
                              (if (funcall token= (aref va i) (aref vb j))
                                  (1+ (aref table (1+ i) (1+ j)))
                                  (max (aref table (1+ i) j)
                                       (aref table i (1+ j)))))))
      (flet ((collect (token)
               (if (and ranges (= (cdr (first ranges)) (first token)))
                   (setf (cdr (first ranges)) (second token))
                   (push (cons (first token) (second token)) ranges))))
        (loop while (and (< i n) (< j m))
              do (cond ((funcall token= (aref va i) (aref vb j))
                        (when (third (aref va i)) (setf matched-word t))
                        (incf i)
                        (incf j))
                       ((>= (aref table (1+ i) j) (aref table i (1+ j)))
                        (collect (aref va i))
                        (incf i))
                       (t (incf j))))
        (loop for rest from i below n do (collect (aref va rest)))
        (when (and matched-word ranges)
          (nreverse ranges))))))

(defun parse-unified-patch (patch &aux (result '())
                                       (pending-removed '())
                                       (pending-added '())
                                       (old-num 0)
                                       (new-num 0)
                                       (in-hunk nil)
                                       (first-hunk t))
  "One \"diff --git\" section's unified patch text as diff document lines."
  ;; File headers before the first hunk drop; later hunks separate with a
  ;; "..." elision row; each hunk's contiguous removed-then-added run is
  ;; index-paired for word emphasis (the retired hunk-flush pairing). Removed
  ;; and context lines number old-side, added lines new-side.
  (labels ((flush (&aux (removed (nreverse pending-removed))
                        (added (nreverse pending-added)))
             (setf pending-removed '() pending-added '())
             (loop for r in removed
                   for a in added
                   do (setf r.ranges
                            (diff-word-ranges r.text
                                              (diff-line-text a))
                            a.ranges
                            (diff-word-ranges (diff-line-text a)
                                              r.text)))
             (setf result (revappend added (revappend removed result)))))
    (dolist (line (or (split-lines patch) '()))
      ;; A "@@ -A[,B] +C[,D] @@" hunk header: OLD-START only when both numbers read.
      (let* ((plus (and (>= (length line) 4) (string= "@@ -" line :end2 4)
                        (position #\+ line)))
             (new-start (and plus (parse-integer line :start (1+ plus) :junk-allowed t)))
             (old-start (and new-start (parse-integer line :start 4 :junk-allowed t))))
        (cond
          (old-start
           (flush)
           (unless first-hunk
             (push (diff-line :context nil "...") result))
           (setf first-hunk nil
                 in-hunk t
                 old-num old-start
                 new-num new-start))
          ((not in-hunk))
          ((zerop (length line)))
          (t
           (case (char line 0)
             (#\Space
              (flush)
              (push (diff-line :context old-num (subseq line 1)) result)
              (incf old-num)
              (incf new-num))
             (#\-
              ;; A removal after additions opens a new change run; keep
              ;; the pairing per run rather than smearing across runs.
              (when pending-added (flush))
              (push (diff-line :removed old-num (subseq line 1))
                    pending-removed)
              (incf old-num))
             (#\+
              (push (diff-line :added new-num (subseq line 1))
                    pending-added)
              (incf new-num))
             ;; "\ No newline at end of file" annotates, never renders.
             (#\\ nil)
             (t (flush)
                (setf in-hunk nil)))))))
    (flush)
    (nreverse result)))

(defun diff-body-lline (line gutter-width)
  "One diff document line as a logical line (transcript.zig
appendDiffBodySegments): added/removed rows tint the full row background
and keep the normal foreground for content — diff meaning rides on the bg
— with the gutter alone in the diff foreground tone and word-level ranges
on the emphasis background; context rows are the dim foreground base; meta
rows dim muted."
  (let* ((prefix-len (if (plusp gutter-width) (+ gutter-width 3) 2))
         (sign (diff-line-sign line))
         (added (eq sign :added)))
    (if (eq sign :meta)
        (make-lline :text (diff-line-text line) :tone :muted :dim t
                    :role :tool-cont)
        (make-lline
         ;; The Zig DocumentBuilder.appendPrefixed gutter: number column, sign, space.
         :text (concatenate 'string
                            (if (plusp gutter-width)
                                (format nil "~v@a " gutter-width (or (diff-line-number line) ""))
                                "")
                            (ecase sign (:added "+ ") (:removed "- ") (:context "  "))
                            (diff-line-text line))
         :tone :foreground :role :tool-cont
         :dim (eq sign :context)
         :fill (case sign (:added :diff-added-line) (:removed :diff-removed-line))
         :spans (unless (eq sign :context)
                  (cons (mspan 0 prefix-len
                               :tone (if added :diff-added :diff-removed))
                        (loop for (start . end) in (diff-line-ranges line)
                              collect (mspan (+ prefix-len start)
                                             (+ prefix-len end)
                                             :tone (if added
                                                       :diff-added-word
                                                       :diff-removed-word)))))))))

(defun append-row-diff-stats (lline row &aux (files (json-array row.metadata "workspace_diff" "files")))
  "The Zig \" +N -M\" header stats over the lines every file call ROW changed
added and removed, onto LLINE -- nothing for a call that changed none."
  (when (plusp (length files))
    (loop for file across files
          sum (or (json-value file :integer "added") 0) into added
          sum (or (json-value file :integer "removed") 0) into removed
          finally (append-lline-run lline (format nil " +~d" added) :tone :diff-added)
                  (append-lline-run lline (format nil " -~d" removed) :tone :diff-removed))))

(defparameter *diff-preview-lines* 12
  "How many lines of a changed file's diff stand under its header before
the header is clicked open: an edit reads as an edit at a glance, and a
long one does not bury the turn.")

(nlk:define-side-table parsed-patch (:weakness :key)
  "Weak memo: a diff document file -> PARSE-UNIFIED-PATCH of its patch, so
a run listing its edits on every frame parses each patch once.")

(defun file-diff-llines (file key &aux (open (detail-open-p key)))
  "The diff of FILE, a diff document file, as lines under its header: all
of it while the header (KEY) is clicked open, else its first
*DIFF-PREVIEW-LINES* and a line counting the rest, which opens it too."
  (let* ((patch (json-value file :string "patch"))
         (entries (and patch (memoized (parsed-patch file) (patch) (parse-unified-patch patch))))
         (maximum (reduce #'max entries :key (lambda (entry) (or (diff-line-number entry) 0)) :initial-value 0))
         (gutter (if (plusp maximum) (length (format nil "~d" maximum)) 0))
         (shown (if open entries (subseq entries 0 (min (length entries) *diff-preview-lines*))))
         (hidden (- (length entries) (length shown))))
    (append (mapcar (lambda (entry) (diff-body-lline entry gutter)) shown)
            (when (plusp hidden)
              (let ((more (make-lline :text (make-string (if (plusp gutter) (+ gutter 3) 2)
                                                         :initial-element #\Space)
                                      :tone :muted :role :tool-cont)))
                (append-lline-run more (format nil "… +~d line~:p" hidden) :tone :muted)
                (dolist (span (lline-spans more)) (setf (mspan-link span) key))
                (list more))))))

(defun workspace-diff-llines (row)
  "File disclosures immediately below their tool, each showing the head of
its diff and opening its own full diff (FILE-DIFF-LLINES)."
  (when-let (document (json-value row.metadata :object "workspace_diff"))
    (let ((truncated (or (json-value document :integer "truncated") 0)))
      (append
       (loop for file across (json-array document "files")
             when (hash-table-p file)
               nconc (let* ((path (or (json-value file :string "path") ""))
                            (key (list :diff (tool-row-expand-key row) path))
                            (added (format nil "+~d" (or (json-value file :integer "added") 0)))
                            (removed (format nil "-~d" (or (json-value file :integer "removed") 0)))
                            ;; The counts right-align against the content rail,
                            ;; so every path starts on it, under the words of the
                            ;; call that wrote it.
                            (hang (- *transcript-detail-column* (nth-value 2 (body-gutter :prose))))
                            (header (make-lline :text (make-string (max 0 (- hang (length added) (length removed) 2))
                                                                   :initial-element #\Space)
                                                :tone :muted :hang hang)))
                       (append-lline-run header added :tone :diff-added)
                       (append-lline-run header (format nil " ~a" removed) :tone :diff-removed)
                       (append-lline-run header (format nil " ~a~a" path (if (json-value file :boolean "binary") " (binary)" "")) :tone :muted)
                       ;; All glyphs on the header toggle this file, never the
                       ;; OS opener or another file in the same Eval.
                       (dolist (span (lline-spans header)) (setf (mspan-link span) key))
                       (cons header (file-diff-llines file key))))
       (when (plusp truncated)
         (list (make-lline :text (format nil "... +~d more file~:p" truncated)
                           :tone :muted :dim t :role :tool-cont)))))))

;;; --- image-result cards -----------------------------------------------------
;;; The tool_result display fact (metadata envelope) the Zig transcript draws
;;; as a card: a generated image (writeTranscriptToolHeader's image arm plus
;;; the kitty preview band). It derives from the SAME durable fact on the live
;;; stream and in replay, so the card renders identically on both paths.

(defun image-fact-plist (image &optional text)
  "IMAGE, an image display fact, as (:payload-kind :source :path :target
:media-type :width :height :bytes :png-path), payload kind inferred from
TEXT, the result beside it, when the fact omits it (the Zig
inferredImagePayloadKind chain)."
  (list* :payload-kind (or (json-value image :text "payload_kind") (inferred-image-payload-kind text))
         :source (or (json-value image :text "source") "inferred")
         (append (json-members image :text "path" "target" "media_type")
                 (json-members image :integer "width" "height" "bytes")
                 (json-members image :text "png_path"))))

(defun row-image-plist (row)
  "The image display fact on the tool ROW, or NIL (IMAGE-FACT-PLIST)."
  ;; Two shapes reach here and both draw one card. A payload fact carries the
  ;; image in the result text as base64, which is what a provider-executed
  ;; image generator returns. A file fact — a LOOK's (exec.lisp) — names the
  ;; file instead: its path, media type and pixel size, plus the PNG a
  ;; terminal can be given, which is the file itself or a cached transcode of
  ;; it. The fact never carries bytes, so the preview is read from the named
  ;; path at the moment it is drawn.
  (when (eq :tool-call row.kind)
    (when-let (image (json-value row.metadata :object "image"))
      (image-fact-plist image row.text))))

(defun image-preview-payload (image row)
  "What the terminal would be given for the IMAGE fact on ROW: the PNG file
it names, or the base64 in the row's result text."
  ;; NIL when the fact names no PNG the protocol can carry — a JPEG on a box
  ;; with no transcoder — which is also what keeps the card from reserving a
  ;; band nothing can fill.
  (or (getf image :png-path) (and (null (getf image :path)) (inline-image-base64 row.text))))

(defun image-card-name (image row)
  "What the card calls the image: the base name of the file the fact names —
what a person reads to know which picture this is — and the tool that
produced it for a fact that carries bytes and no path."
  ;; Cut at the last separator by hand rather than through a pathname: a path
  ;; is a string here, and a `*' or a `[' in one is a character, never a
  ;; wildcard.
  (let* ((path (getf image :path))
         (cut (and path (position-if (lambda (ch) (find ch "/\\")) path :from-end t)))
         (name (and path (subseq path (if cut (1+ cut) 0)))))
    (or (and name (plusp (length name)) name) path (json-value row.tool-name :text) "image")))

(defun image-card-title-lline (image row)
  "The card's title, in the shape every other tool card's has: the :IMAGE
family's own verb (activity.lisp *ACTIVITY-FAMILIES* — `Viewed', `Viewing'
while the call runs) over the image's name, then the status word a card ends
with when it is not simply complete."
  ;; The whole path this is the name of lives in the caption under the
  ;; picture.
  (let ((title (title-phrases-lline
                (list (list (getf (activity-family-plist :image)
                                  (if (equal "running" row.tool-status) :present :past))
                            (image-card-name image row)
                            nil
                            ;; The name reads as the picture, so the name is
                            ;; what a click on the picture lands on.
                            (when-let (path (getf image :path)) (cons :path path)))))))
    (append-title-detail title (tool-note row))
    (append-status-text title row.tool-status (tool-status-text row.tool-status))))

(defun image-card-detail (image)
  "The card's caption, under the picture: the whole path, then what the image
is and how big — `/home/me/shot.png · image/png · 1920x1080 · 412.0 KB' —
and the payload wording for a fact that carries its bytes in the result text
instead."
  ;; A file the graphics protocol cannot carry says so where the size would
  ;; be, which is why no band is reserved under it.
  (if (getf image :path)
      (let ((width (getf image :width))
            (height (getf image :height))
            (bytes (getf image :bytes)))
        (format nil "~{~a~^ · ~}"
                (remove nil
                        (list (getf image :path)
                              (or (getf image :media-type) "image")
                              (and width height (format nil "~dx~d" width height))
                              (and bytes (size-text bytes))
                              (unless (getf image :png-path)
                                "no inline preview: the graphics protocol carries PNG and no transcoder is installed")))))
      (or (cdr (assoc (getf image :payload-kind)
                      '(("base64" . "base64 image payload")
                        ("data_url" . "image data URL")
                        ("url" . "image URL"))
                      :test #'equal))
          "image result")))

(defun inferred-image-payload-kind (text)
  "The Zig inferredImagePayloadKind: data URL, URL, base64, else unknown."
  (let ((text (or text "")))
    (cond ((uiop:string-prefix-p "data:image/" text) "data_url")
          ((web-url-p text) "url")
          ((inline-image-base64 text) "base64")
          (t "unknown"))))

(defun inline-image-base64 (payload)
  "The bare base64 body of an inline PNG payload, or NIL when PAYLOAD is
not inline-renderable (the Zig normalizeInlineImagePayload gate): a bare
base64 string or a data:image/png;base64, URL, base64-alphabet only,
bounded at 8MB."
  ;; The URL's head is everything before its first comma: it names PNG and
  ;; says base64, and the body after it is at least eight alphabet characters.
  ;; A head that says anything else stays on, and no body carries a colon.
  (let* ((text (nlk:trimmed (or payload "")))
         (body (ppcre:regex-replace "^data:image/png[^,]*;base64[^,]*," text "")))
    (and (<= (length text) (* 8 1024 1024))
         (ppcre:scan "^[A-Za-z0-9+/=]{8,}\\z" body)
         body)))

;;; RENDER-STATE binds it from the model's KITTY-GRAPHICS flag.
(defvar *kitty-images-enabled* nil
  "Render-scope switch: T reserves an inline preview band under image
cards (the terminal answered the kitty graphics query).")

(defparameter *image-preview-rows* 8
  "Rows the inline kitty preview band reserves under an image card whose
fact never measured the image (the Zig image_preview_rows viewport input).")

;;; An image inside a transcript is a glance at what the organism saw, never a
;;; page of it.
(defparameter *image-preview-max-rows* 16
  "Ceiling on a measured preview band.")

(defun image-preview-rows (image width &aux (image-width (getf image :width))
                                            (image-height (getf image :height))
                                            (columns (max 8 (- width 4))))
  "Rows the inline preview band reserves for the IMAGE fact at frame WIDTH."
  ;; A fact that measured the file gets the image's own aspect at the nominal
  ;; cell aspect, never taller than the image's pixels ask for, never taller
  ;; than *IMAGE-PREVIEW-MAX-ROWS*, and never so tall that the columns kitty
  ;; derives would run past the frame. Deterministic from the fact alone,
  ;; which is what lets the reserved band and the placement agree without
  ;; sharing state — and lets every attached shell reserve exactly what it
  ;; places.
  ;;
  ;; The terminal is never asked: kitty is given a row count and derives the
  ;; columns from the image's own aspect ratio, so the guess only has to be close
  ;; enough to keep an image off its own scale. A cell is nominally 18 pixels tall
  ;; (near enough) and 2 times taller than wide (near enough), which is what
  ;; keeps a wide image inside the frame.
  (if (and (integerp image-width) (plusp image-width)
           (integerp image-height) (plusp image-height))
      (max 1 (min *image-preview-max-rows*
                  (ceiling image-height 18)
                  (floor (* columns image-height)
                         (* 2 image-width))))
      *image-preview-rows*))

(defun image-band-llines (image row width)
  "What stands under the line naming IMAGE, a fact of ROW's: the band a
terminal that speaks the kitty graphics protocol draws it over, its first row
carrying the fact for the placement, then the caption -- alone where the
terminal has no graphics or the fact names no PNG it could be given."
  (append
   (when (and *kitty-images-enabled* (image-preview-payload image row))
     (loop for fact = image then nil
           repeat (image-preview-rows image width)
           collect (make-lline :text "" :role :image-slot :image fact)))
   ;; The caption opens with the whole path, which is the other half of the
   ;; click: the name above says which picture, this says where it is, and
   ;; both open it.
   (list (make-lline :text (image-card-detail image)
                     :tone :foreground :dim t :role :tool
                     :gutter-tone :muted
                     :spans (path-link-spans (getf image :path))))))

(defun answer-llines (row llines width)
  "LLINES, the prose of the assistant text ROW, with each picture its facts
name drawn under the line that names it (IMAGE-BAND-LLINES), and one no line
names -- a table's cell -- under the last."
  ;; The fact decides what is a picture; the line only says where it stands,
  ;; read by the rule the engine found the target with (MARKDOWN-IMAGE-TARGETS).
  ;; LLINES is the parse cache's and is not touched.
  ;; IMAGES: the file facts of the pictures the answer names (exec.lisp
  ;; ANSWER-IMAGES), in its order, each as IMAGE-FACT-PLIST shapes one.
  (let ((images (loop for image across (json-array row.metadata "images")
                      when (hash-table-p image) collect (image-fact-plist image))))
    (flet ((band (image) (image-band-llines image row width)))
      (if (null images)
          llines
          (append (loop for line in llines
                        collect line
                        unless (eq :code line.role)
                          append (loop for target in (markdown-image-targets line.text)
                                       for image = (find target images :test #'equal
                                                                       :key (lambda (image) (getf image :target)))
                                       when image
                                         do (setf images (remove image images))
                                         and append (band image)))
                  (mapcan #'band images))))))

(defstruct md-cache
  "One row's previous markdown parse plus its incremental checkpoint: the
trimmed-relative character offset of the last top-level blank line and the
count of llines emitted before it."
  ;; RAW is the text object the parse read — streaming rows grow their text in
  ;; place through the append buffer, so the same object with a larger fill
  ;; pointer means everything before the checkpoint re-parses to the same
  ;; llines by construction and the parse can restart at the checkpoint.
  raw len shadow width llines cp-offset cp-count)

(nlk:access (cache md-cache))

(nlk:define-side-table row-md-cache (:weakness :key)
  "Weak per-row memo for ROW-MARKDOWN-LLINES: row -> MD-CACHE.")

(defun row-markdown-llines (row raw &key shadow width)
  "MARKDOWN-LLINES over the outer-trimmed RAW (the TRIM-BLOCK-TEXT policy,
applied by bounds so an unchanged prefix is never re-copied), incremental
across streaming appends: when RAW is the same append buffer the cache
already parsed and a top-level blank-line checkpoint exists, the head
llines are reused as-is and only the text from the checkpoint on
re-parses."
  ;; The head llines stay EQ-stable, which is what feeds the LLINE-RENDER memo
  ;; during streaming; the cached llines are shared across frames, so callers
  ;; must treat them as immutable (copy before dimming or hover-painting).
  (let* ((cache (row-md-cache row))
         (start (position-if-not #'blank-char-p raw))
         (end (if start
                  (1+ (position-if-not #'blank-char-p raw :from-end t))
                  0))
         (usable
           (and cache start
                ;; Same buffer object, append-only growth: the parsed prefix
                ;; is intact without a character compare.
                (eq cache.raw raw)
                (>= (length raw) cache.len)
                (eql cache.shadow shadow)
                (eql cache.width width)
                cache.cp-offset
                (plusp cache.cp-offset)
                (< (+ start cache.cp-offset) end))))
    (multiple-value-bind (llines cp-offset cp-count)
        (flet ((parse (text)
                 ;; Kept for as long as the row is projected, so kept narrow:
                 ;; a line cut from the streaming buffer is four bytes a
                 ;; character. Only what parses fresh; a head line reused is
                 ;; narrowed already.
                 (multiple-value-bind (llines cp count)
                     (markdown-llines text :shadow shadow :width width
                                           ;; A row no fact owns yet, and no failure
                                           ;; sealed, is a stream still arriving.
                                           :arriving (not (or row.hydrated row.voided)))
                   (dolist (line llines)
                     (setf line.text (nlk:narrow-string line.text)))
                   (values llines cp count))))
          (if usable
              (bind ((off cache.cp-offset) (head-count cache.cp-count)
                     ((tail tail-cp tail-count) (parse (subseq raw (+ start off) end))))
                (values (nconc (subseq cache.llines 0 head-count)
                               tail)
                        ;; No blank line in the tail yet: the old checkpoint
                        ;; still bounds the stable prefix of the new text.
                        (if tail-cp (+ off tail-cp) off)
                        (if tail-cp (+ head-count tail-count) head-count)))
              (parse (if start (subseq raw start end) ""))))
      (setf (row-md-cache row)
            (make-md-cache :raw raw :len (length raw)
                           :shadow shadow :width width
                           :llines llines
                           :cp-offset cp-offset :cp-count cp-count))
      (unless shadow
        (stamp-glints row llines))
      llines)))

(nlk:define-side-table row-glint-stamps (:weakness :key)
  "A message row -> the internal times its chips arrived at, in their
order: NIL for one the row held when this shell first laid it out.")

(defun stamp-glints (row llines &aux (stamps (or (row-glint-stamps row)
                                                 (setf (row-glint-stamps row)
                                                       (make-array 4 :adjustable t :fill-pointer 0))))
                                     (count 0))
  "Give each chip of message ROW's LLINES the time it arrived (MSPAN-GLINT):
one first laid out while ROW is arriving arrived then, and a glint crosses
it once; one a finished row held, never."
  ;; A chip is known by its place among ROW's chips, which a stream only ever
  ;; adds to, so every parse of a growing row finds each chip's own time.
  (let ((now (and (not (or row.hydrated row.voided)) (get-internal-real-time))))
    (dolist (line llines)
      (dolist (span (lline-spans line))
        (when (eq :chip (mspan-ground span))
          (when (>= count (length stamps))
            (vector-push-extend now stamps))
          (setf (mspan-glint span) (aref stamps count))
          (incf count))))))

(defparameter *thinking-tail-rows* 4
  "How many wrapped rows of a thought still streaming a folded transcript
shows under its `Thinking…' line: enough to follow the sentence it is on,
not the minutes of it — with the line above, the height of a collapsed tool
card's output window (five rows).")

(defun thinking-p (row)
  "Whether ROW is a thought still arriving: streamed, not yet confirmed or
voided, its clock still running."
  (and (not row.hydrated) (not row.voided) (null row.thought-ms)))

(defun row-notice-summary (row)
  "A successful system notice's explicit disclosure title metadata, or NIL."
  (and (transcript-row-p row) (eq :system-notice (transcript-row-kind row))
       (not (transcript-row-error-p row))
       (json-value (transcript-row-metadata row) :object "summary")))

(defun row-notice-label (row)
  "A successful system notice's label metadata, the word its row stands
under in the label lane, or NIL."
  (and (transcript-row-p row) (eq :system-notice (transcript-row-kind row))
       (not (transcript-row-error-p row))
       (json-value (transcript-row-metadata row) :text "label")))

(defun row-notice-bare-p (row)
  "Whether ROW is a notice that stands as its words alone, no band and no
mark: its metadata says `bare'."
  (and (transcript-row-p row) (eq :system-notice (transcript-row-kind row))
       (json-value (transcript-row-metadata row) :boolean "bare")))

(defun row-llines (row width)
  "The logical lines for ROW under the Zig row-chrome table."
  (unless (redundant-finish-divider-p row)
    (let ((lines
           (ecase row.kind
             (:text
              (cond ((equal row.role "assistant")
                     ;; Raw text: ROW-MARKDOWN-LLINES applies the outer trim by
                     ;; bounds so a streaming frame never re-copies the whole body.
                     (answer-llines row (row-markdown-llines row row.text :width width) width))
                    ((equal row.role "user")
                     (mapcar (lambda (line)
                               (make-lline :text line :tone :user-block :fill :user-block))
                             (or (split-lines
                                  (trim-block-text row.text))
                                 (list ""))))
                    (t (system-notice-llines
                        (trim-block-text row.text) nil))))
             (:reasoning
              ;; Open: the prose under a shadow, no WIDTH. Folded: one line, a tail while it streams.
              ;; A folded thought that has finished is one line, so its prose is never laid out.
              (let* ((open (detail-open-p row))
                     (thinking (thinking-p row))
                     (prose (and (or open (and thinking (eq *tool-detail* :developer))) (row-markdown-llines row row.text :shadow t))))
                (cond ((not (position-if-not #'blank-char-p (or row.text ""))) nil)
                      (open prose)
                      ((eq *tool-detail* :standard)
                       ;; Parse only the displayed line, not the full closed thought.
                       ;; Markdown owns its text/style; the Think prefix only shifts
                       ;; copied spans, before the renderer attaches link metadata.
                       ;; BODY: the one line a folded thought shows, as Markdown: the newest
                       ;; line while it streams, its first once it is done; NIL for a
                       ;; marker-only line (the text is not blank here).
                       (let* ((raw row.text)
                              (end (1+ (position-if-not #'blank-char-p raw :from-end t)))
                              (start (if thinking
                                         (1+ (or (position #\Newline raw :end end :from-end t) -1))
                                         (position-if-not #'blank-char-p raw)))
                              (stop (if thinking end (position #\Newline raw :start start)))
                              ;; A marker-only excerpt has no Markdown body to preview.
                              (body (or (first (markdown-llines (subseq raw start stop) :shadow t))
                                        (make-lline :tone :muted :italic t)))
                              (prefix (lline-text (append-title-detail (title-phrases-lline (list (list "Think" "" nil nil))) ""))))
                         (list (copy-with lline body
                                 :text (concatenate 'string prefix
                                                    (if (lline-rule body) "─" (lline-text body)))
                                 :role :prose :hang (text-display-width prefix)
                                 :hang-mark nil :rule nil
                                 :spans (list* (mspan 0 5 :tone :thought :bold t)
                                               (mspan 5 (length prefix) :tone :muted :italic t)
                                               (mapcar (lambda (span)
                                                         (shifted-span span (length prefix)))
                                                       (lline-spans body)))))))
                      (t (cons (make-lline :text (if thinking
                                                     "Thinking…"
                                                     (let ((ms row.thought-ms) (tokens row.reasoning-tokens))
                                                       (format nil "Thought~@[ for ~a~]~@[ · ~a tokens~]"
                                                               (and ms (format-turn-elapsed ms))
                                                               (and tokens (plusp tokens)
                                                                    (format-token-count tokens)))))
                                           :tone :muted :italic t
                                           :spans (list (mspan 0 (if thinking 9 7) :tone :thought :bold t)))
                               ;; The last rows of a thought still streaming, blank lines skipped.
                               (and thinking
                                    (llines-window (remove-if (lambda (line)
                                                                (zerop (length (string-trim '(#\Space #\Tab)
                                                                                            line.text))))
                                                              prose)
                                                   width *thinking-tail-rows* :from-end t)))))))
             (:tool-call
              (cond
                ;; File disclosures belong to the Eval that changed them. Their
                ;; headers always show; each file alone owns its expanded diff.
                ((row-edited-p row)
                 (cons (tool-summary-lline row)
                       (append (workspace-diff-llines row)
                               (when (and (run-member-open-p row)
                                          (or row.error-p (row-load-errors row)
                                              (member row.tool-status '("stopped" "exited" "background") :test #'equal)))
                                 (tool-output-llines row width))
                               (when-let (image (row-image-plist row))
                                 (cons (image-card-title-lline image row) (image-band-llines image row width))))))
                ((and (eq *tool-detail* :standard) (not (detail-open-p (tool-row-expand-key row))))
                 (cons (tool-summary-lline row)
                       (fork-tree-llines row width :indent (- *transcript-detail-column*
                                                              (nth-value 2 (body-gutter :prose))))))
                ((when-let (image (row-image-plist row))
                   (cons (image-card-title-lline image row) (image-band-llines image row width))))
                (t
                 (append (if (eq *tool-detail* :standard)
                             (cons (tool-summary-lline row) (tool-command-llines row))
                             (tool-header-llines row))
                         (fork-tree-llines row width :indent 2)
                         (tool-output-llines row width)))))
             (:system-notice
              (if-let (summary (row-notice-summary row))
                (let ((title (title-phrases-lline (list (list (json-value summary :string "name") "" nil nil))
                                                  :verb-tone :success)))
                  (append-title-detail title (json-value summary :text "comment"))
                  (cons title (when (detail-open-p row)
                                (system-notice-llines (trim-block-text row.text) nil))))
                (let ((text (trim-block-text row.text)))
                  (if-let (label (row-notice-label row))
                    ;; A labelled notice's line: the label bold in the label lane where
                    ;; Eval and Think stand, the text on the content rail, on the canvas.
                    ;; Without this, the ■ and its words would sit between the rails,
                    ;; off the canvas, so the band would read as blank rows.
                    (let* ((hang (- *transcript-detail-column* (nth-value 2 (body-gutter :prose))))
                           (line (make-lline :text "" :tone :system :hang hang))
                           (gap (make-string (max 1 (- hang (text-display-width label)))
                                             :initial-element #\Space)))
                      (append-lline-run line label :bold t)
                      (append-lline-run line gap)
                      (list (append-lline-run line (nlk:one-line text))))
                    ;; A bare notice's lines: its words in the system tone on the canvas, on
                    ;; the label lane where a notice's words stand, with no mark in the
                    ;; gutter. Without this, a bare notice would read as a box the system
                    ;; put up rather than the organism speaking.
                    (if (row-notice-bare-p row)
                        (loop for line in (or (split-lines text) (list ""))
                              collect (make-lline :text line :tone :system :italic t
                                                  :role :system-cont
                                                  :spans (notice-url-spans line)))
                        (system-notice-llines text row.error-p))))))
             ;; The two divider rows are one row shape; the evicted side of the fold
             ;; seam is the dimmed one.
             ;; A wrapped row goes on under its words, past the gutter's mark.
             ((:finish-divider :eviction-divider)
              (list (make-lline :text (trim-block-text row.text)
                                :tone :muted :role :status :hang 2
                                :dim (eq :eviction-divider row.kind)))))))
      ;; Both speakers' words start on the prose gutter, where a run's title
      ;; and the live line's words stand, the clock at the first row's right
      ;; end.
      (if (message-timestamp-row-p row)
          (stamp-transcript-llines lines (transcript-clock row) width)
          lines))))
(defun row-llines-with-eviction (row width)
  "ROW-LLINES with the evicted-region dimming applied on top: every line of
a row a context eviction folded away renders dim, whatever its kind, and so
does a voided attempt's row — kept, but not the answer."
  ;; Dims copies: the markdown llines are shared with the per-row parse cache,
  ;; and a mutation there would outlive the eviction that asked for it. A
  ;; voided row whose seal label has landed closes with that label as a status
  ;; line. The model's words keep the foreground however much work followed
  ;; them: greyed, every message but a turn's answer read as spent.
  (let* ((lines (row-llines row width))
         (voided row.voided)
         (lines (if (or row.evicted-p voided)
                    (mapcar (lambda (line) (copy-with lline line :dim t)) lines)
                    lines)))
    (if (stringp voided)
        (append lines
                (list (make-lline :text voided :tone :muted :role :status
                                  :dim t)))
        lines)))

(defun row-block-fill (item)
  "The background tone ITEM's block paints behind its body and its padding
rows, NIL when the block sits on the canvas."
  ;; ITEM is a projection item: a TRANSCRIPT-ROW, or a TOOL-GROUP, whose
  ;; folded run is a tool block and sits on the canvas like one (the
  ;; viewport's pad arm reads the last line's owner, and that owner is the
  ;; group while the model works through routine calls). The Zig
  ;; Filled transcript blocks: system notices and error diagnostics fill;
  ;; user prompts keep their own fill; assistant prose, reasoning and tool
  ;; blocks stay on canvas (reasoning's shadow is over text, not a fill).
  (when (transcript-row-p item)
    (cond ((or (row-notice-summary item) (row-notice-label item) (row-notice-bare-p item)) nil)
          ((eq item.kind :system-notice) (if item.error-p :diagnostic-block :system-block))
          ((or (not (eq item.kind :text)) (equal item.role "assistant")) nil)
          ((equal item.role "user") :user-block)
          (t :system-block))))

;;; Weak keys, so rows a fold or settle drops release their blocks with them.
(nlk:define-side-table row-display-cache (:weakness :key)
  "Weak per-row memo for ROW-DISPLAY-BLOCK: row -> its render key and block.")

(defun row-block-padding-p (row &optional (mode *tool-detail*))
  "Whether ROW owns padding: thoughts, compact notices and Standard tools own none."
  (not (or (redundant-finish-divider-p row)
           (row-notice-summary row)
           (and (transcript-row-p row)
                (or (eq :reasoning row.kind) (and (eq mode :standard) (eq :tool-call row.kind)))))))

(defun row-hover-llines (row llines)
  "Hover paint for ROW, inline or nested: Standard disclosure titles, otherwise block surfaces."
  (cond
    ((not (eq row *hover-row*)) llines)
    ((and (eq :text row.kind) (equal "user" row.role))
     ;; The prompt keeps its block fill, including both padding rows.
     (mapcar (lambda (line)
               (copy-with lline line :fill :user-block-hover
                          :tone (if (eq :user-block line.tone) :user-block-hover line.tone)))
             llines))
    ((and (eq *tool-detail* :standard)
          (or (member row.kind '(:tool-call :reasoning)) (row-notice-summary row)))
     ;; An open thought is prose only: it has no title to accent.
     (if (and llines (not (and (eq row.kind :reasoning) (detail-open-p row))))
         (cons (disclosure-hover-lline (first llines)) (rest llines))
         llines))
    (t
     ;; Copy-on-hover: the stabilized llines and interned pads are shared.
     (mapcar (lambda (line) (copy-with lline line :fill (or line.fill :hover))) llines))))

(defun row-display-block (row &key (width 80))
  "Render one TRANSCRIPT-ROW into a DISPLAY-BLOCK of painted rows, the
block's padding included when its viewport policy owns any."
  ;; Memoized per row, so a streaming frame re-renders only the rows whose
  ;; content or modifiers moved and full re-projection cost tracks the delta,
  ;; not the transcript. Consumers already must not mutate a block's lines —
  ;; the projection cache shares them across animation ticks — and the memo
  ;; extends that same discipline across full renders.
  ;;
  ;; The fields are everything the block is a function of: the row fields the
  ;; display policy reads (reducers replace field values rather than mutating
  ;; them in place, so object identity is the change signal), the render width,
  ;; the theme, and the render modifiers. Hover folds to a per-row boolean so
  ;; pointer motion invalidates the two rows it crosses, not the whole
  ;; transcript; streaming appends grow text in place, so the text revision is
  ;; the change signal identity alone cannot carry.
  (when (bg-band-row-p row)
    (return-from row-display-block
      (multiple-value-bind (lines plains refs) (background-band-lines row width)
        (make-display-block :retention :live-only :kind :tool-call :row row
                            :lines lines :plain-lines plains :refs refs))))
  (memoized (row-display-cache row)
    (width *palette* *color-profile* *kitty-images-enabled*
     *tool-detail* *expanded-tool-call*
     (eq row *hover-row*)
     row.kind row.role
     row.retention row.text
     row.tool-name row.tool-args
     row.tool-status row.tool-exit-code
     (transcript-row-tool-call-id row) row.metadata (row-load-errors row)
     row.hydrated row.error-p
     row.evicted-p row.voided
     row.thought-ms row.reasoning-tokens
     row.text-rev)
    (let* ((fill (row-block-fill row))
           (body (row-body-llines row width))
           ;; The block's own padding rows. Padding is a property of the block, not of the
           ;; gap between blocks: the Zig walk emits one block_padding line above and one
           ;; below every block unconditionally, so two adjacent blocks are separated by
           ;; both of their pads and the rhythm never collapses to a single row.
           ;;
           ;; Padding belongs to a body, so an empty one pads to nothing: a row whose
           ;; content renders to no logical lines -- a provider round that reported
           ;; whitespace-only `reasoning_content', a text row still waiting on its first
           ;; delta -- is not a block and must not widen the gap between the blocks
           ;; around it. Nor is a body of blank lines: without this, one empty line
           ;; padded on both sides would draw three blank rows where the rhythm keeps one.
           (llines (cond ((not (row-block-padding-p row)) body)
                         ((some (lambda (line)
                                  (or (lline-image line) (lline-rule line)
                                      (position-if-not #'blank-char-p (lline-text line))))
                                body)
                          (append (list (block-pad-lline fill)) body
                                  (list (block-pad-lline fill)))))))
      (setf llines (row-hover-llines row llines))
      (multiple-value-bind (lines plains) (llines-rows llines width)
        (make-display-block :retention row.retention
                            :kind row.kind
                            :row row
                            :fill fill
                            :lines lines
                            :plain-lines plains)))))

(defun lline-content-equal (a b)
  "Field-wise LLINE equality: two logical lines that lay out and paint
identically, so one may substitute for the other in the render caches."
  ;; EQUALP walks every slot of both lines and of each span, but folds case,
  ;; so the two strings a row shows as written -- its text and each span's
  ;; link target -- are compared exactly as well.
  (or (eq a b)
      (and (string= (lline-text a) b.text)
           (equalp a b)
           (every (lambda (x y) (equal (mspan-link x) (mspan-link y))) (lline-spans a) b.spans))))

;;; Row chrome rebuilds every logical line from source text on each re-render;
;;; during streaming the tail row re-renders every frame while all but its
;;; last line come out identical, so keeping the unchanged ones EQ-stable is
;;; what lets LLINE-RENDER skip their wrap+paint.
(nlk:define-side-table row-llines-cache (:weakness :key)
  "Weak per-row memo for ROW-BODY-LLINES: row -> the lline list of the
row's previous render.")

(nlk:define-side-table row-image-bands (:weakness :key)
  "Weak memo: a row -> ((OFFSET . IMAGE) ...), the inline image bands of its
last render: the painted row each opens on, counted from the row's first, and
the fact drawn there.")

(defun row-body-llines (row width)
  "ROW's logical lines at WIDTH, EQ-stable across renders (the ROW-LLINES-CACHE),
its inline image bands noted for the placement (IMAGE-BAND-AT)."
  ;; Each fresh lline is replaced by ROW's previous content-identical lline at the
  ;; same position, so unchanged lines stay EQ across re-renders and hit the
  ;; LLINE-RENDER memo. The merged list becomes the new cache entry. Positional
  ;; lockstep is the right shape: streaming appends only extend or add trailing
  ;; lines, and any reflow (tables re-align, a fence closes) changes the affected
  ;; lines' content, which is exactly what the equality probe detects.
  (let ((llines (setf (row-llines-cache row)
                      (loop with prev = (row-llines-cache row)
                            for new in (row-llines-with-eviction row width)
                            for old = (pop prev)
                            collect (if (and old (lline-content-equal new old)) old new)))))
    (when (or (find-if #'lline-image llines) (row-image-bands row))
      (setf (row-image-bands row)
            (loop with at = 0
                  for line in llines
                  when line.image collect (cons at line.image)
                  do (incf at (length (lline-render line width))))))
    llines))

(defun visible-transcript-state (model &aux (preview (chat-model-transcript-preview model)))
  "The state the transcript projects this frame: an open dialog's preview
while one is armed, else MODEL's live folded state (the Zig
sessionsDialogPreview substitution at the render-plan seam)."
  ;; The preview never replaces the live state — it substitutes here only, and
  ;; only while a dialog is open, so closing the dialog restores the live
  ;; transcript.
  (if (and preview (chat-model-dialog model))
      (cdr preview)
      (chat-model-state model)))

;;; --- the group pass ----------------------------------------------------------
;;; Routine calls fold. A run of calls that only read, search, list, fetch,
;;; recall, check or wait — ROW-ACTIVITY says so — becomes one TOOL-GROUP
;;; that renders as one line, `Read 4 files, Searched 'tool-row''; while
;;; thoughts are folded the thoughts between those calls join the run, so a
;;; turn that read its way to an answer reads as one line and the answer. The
;;; pass is pure over the rows: the rows are never touched, and replay folds
;;; the same groups the live stream does because both build the same rows.
;;;
;;; A reflection's recap folds the same way. The experience cell records it
;;; behind the turn it recaps, a pair written whole (NLK:RECORD-EXCHANGE-TURN)
;;; whose completion names its recorder `reflection': a question the operator
;;; never asked, its answer and its divider, nine rows after every tool turn.
;;; It is the machine's note to itself, so it draws as one quiet line under
;;; that turn, `Reflection recap · ' and its first words, which opens on the
;;; answer and its divider as a group opens on its calls -- the web page's
;;; drawRecap. Other recorded pairs draw as they are.

(defstruct (tool-group (:constructor make-tool-group (key)))
  "A routine run, completed Standard work trail, or recap projected as one item."
  ;; MEMBERS are its rows in order, the thoughts between its calls included;
  ;; a recap's are its prompt row and the rows of its turn after it. KEY is
  ;; its accordion identity, (:GROUP call-id) or (:RECAP turn-id), stable
  ;; while its first member is. LINE and BLOCK memoize its folded rendering;
  ;; TITLED-FROM is what LINE was worded from, (RUNNING . ACTIVITIES). TAIL
  ;; is what stands under its folded title a line each: a running turn's
  ;; newest round (NEWEST-ROUND) and any call still running here or as a
  ;; live exec, and THOUGHT a running turn's newest thought, the paragraph
  ;; under the live bar (LIVE-THOUGHT-ROWS).
  key
  (members '())
  (tail '())
  (thought nil)
  (line nil)
  (titled-from nil)
  (block nil))

(nlk:access (group tool-group) (pressed tool-group) (recap tool-group))

(defmethod print-object ((group tool-group) stream)
  "Identity, never the memo: BLOCK holds the DISPLAY-BLOCK whose row is this
group, so the default structure print never ends."
  (print-unreadable-object (group stream :type t :identity t)
    (format stream "~s ~d member~:p" group.key
            (length group.members))))

(nlk:define-side-table tool-groups (:weakness :key)
  "Weak memo: a group's first call row -> its TOOL-GROUP, so a group keeps
its identity across projections — the hover and the accordion hold it by
identity and by key while its run grows.")

(nlk:define-side-table row-groups (:weakness :key)
  "Weak memo: a row -> the TOOL-GROUP the last projection folded it into, so
a click on an open group's member knows which group it belongs to.")

(defun routine-call-p (row)
  "Whether ROW is a call that joins a run of routine calls (GROUP-ROWS): its
words group, and no background process of it ran on past it."
  (and (eq :tool-call row.kind) (not (row-edited-p row))
       (not (member row.tool-status '("exited" "background") :test #'equal))
       (when-let (activity (row-activity row)) (activity-groupable-p activity))))

(defun reflection-recap-p (row)
  "Whether ROW opened a reflection's recap: the input row of a recorded pair
whose completion named its recorder `reflection'."
  ;; The name lands on the input row with the completion (INGEST-TURN-TERMINAL);
  ;; until then the row is a recorded pair's like any other.
  (and (transcript-row-p row) (equal "reflection" row.recorded) row.turn-id t))

(defun row-mutation-path (row &aux (summary (row-notice-summary row))
                                   (path (and (equal "Mutate" (json-value summary :string "name"))
                                              (json-value summary :string "path"))))
  "A successful Mutate receipt's explicit nonempty affected-file path, or NIL."
  (and path (plusp (length path)) path))

(nlk:define-side-table mutation-groups (:weakness :key)
  "Live-only first file receipt -> its stable mutation disclosure.")

(defun group-mutation-rows (rows &aux (items '()) (run '()) (turn nil))
  "Adjacent successful file receipts of one turn as one Mutate disclosure."
  (labels ((flush ()
             (when run
               ;; Retain the first receipt for every affected file. Deduplicating
               ;; only PATHS left repeated file blocks in MEMBERS; dropping the
               ;; whole run is not deduplication: one file still needs its receipt.
               (let ((members (remove-duplicates (nreverse run)
                                                 :key #'row-mutation-path
                                                 :test #'equal :from-end t)))
                 (if (rest members)
                     (let* ((anchor (first members))
                            (group (or (mutation-groups anchor)
                                       (setf (mutation-groups anchor)
                                             (make-tool-group (list :mutation anchor))))))
                       (setf (tool-group-members group) members)
                       (forget-row-groups group)
                       (dolist (row members) (setf (row-groups row) group))
                       (push group items))
                     (dolist (row members)
                       (forget-row-groups row)
                       (push row items))))
               (setf run nil))))
    (dolist (row rows)
      (cond ((row-mutation-path row)
             (when (and run (not (equal turn (transcript-row-turn-id row)))) (flush))
             (setf turn (transcript-row-turn-id row))
             (push row run))
            (t (flush)
               (push row items))))
    (flush))
  (nreverse items))
(defun transcript-background-row (row backgrounds)
  "The live exec owned by ROW's call id, or by its unadopted provider index.
Adopted calls never match a later round's reused stream index."
  (when (and (transcript-row-p row) (eq :tool-call (transcript-row-kind row)))
    (find-if (lambda (exec &aux (call (bg-band-row-tool-call-id exec)))
               (if (and (stringp call) (plusp (length call)))
                   (equal call (transcript-row-tool-call-id row))
                   (and (null (transcript-row-tool-call-id row))
                        (transcript-row-stream-index row)
                        (eql (transcript-row-stream-index row) (bg-band-row-stream-index exec)))))
             backgrounds)))


(nlk:define-side-table live-run-rows (:weakness :key)
  "A running turn's newest run -> the most rows it and the canvas row over the
live bar have taken this turn (RUN-LIFT). GROUP-ROWS opens the entry while
the turn runs and drops it when the turn ends.")

(nlk:define-side-table held-window (:weakness :key)
  "A listed call -> (WIDTH . BLOCK): the output window its exec last showed
under it (LISTED-OUTPUT), which it keeps once the exec is gone while its
round stands under the run.")

(defun newest-round (members &aux (round '()) (receipts '()) (later nil))
  "The calls and receipts of run MEMBERS' newest tool round, in order: back
from the newest call to the thought before it or to the call its round
opened with."
  ;; Provider stream indices restart at 0 every round (INGEST-STREAM-TOOL),
  ;; so a call is of the round of the call after it only while its index is
  ;; below that one's; a call without one is a round of its own. A receipt
  ;; follows its call. A thought after the round is the next round's, still
  ;; arriving: the round stands until that one lists a call.
  (flet ((done () (return-from newest-round (or round receipts))))
    (dolist (row (reverse members) (or round receipts))
      (cond ((eq :reasoning (transcript-row-kind row))
             (when (or round receipts) (done)))
            ((eq :tool-call (transcript-row-kind row))
             (if (or (null later)
                     (let ((index (transcript-row-stream-index row))
                           (next (transcript-row-stream-index later)))
                       (and index next (< index next))))
                 (setf round (cons row (append receipts round)) receipts '() later row)
                 (done)))
            ((row-notice-summary row) (push row receipts))))))

(defun group-rows (rows &optional (context rows) backgrounds)
  "ROWS as projection items, in order: a row, or a TOOL-GROUP for each run
of routine calls. Reflection recaps appear only in the Developer viewport."
  ;; A Developer run holds consecutive calls of one turn that group
  ;; (ACTIVITY-GROUPABLE-P) and, while thoughts are folded, the thoughts between them;
  ;; a thought after a run's last call stays its own item. Anything else breaks
  ;; the run: an operator prompt, an answer with text, a plan, a notice, a call
  ;; that changed something or failed, a thought opened on its own, a new turn.
  ;; A recap's group holds its prompt row and every row of its turn after it.
  ;; Consume them before the viewport branches: Standard emits no recap item
  ;; (including padding), while Developer keeps its existing accordion.
  ;; A Standard run is a turn's whole work trail, live or done, under one
  ;; `Ran N commands' that counts up as calls land: its calls, thoughts and
  ;; receipts. Only an unsummarized notice splits it. A run stays folded,
  ;; live or done, until a click opens it to its
  ;; calls' names, one line each; under the folded title stands the newest
  ;; round of a running turn, finished calls too, until the next round lists
  ;; its first call, and then goes into the count. A call handed off to the
  ;; background stays where it ran while its exec lives, its output streaming
  ;; under it. It stood under the run as the exec's code block. /execs stops it.
  ;; Developer keeps that block in the call's place. The model's words never
  ;; fold, and a turn's words have one place, under its run: the newest
  ;; takes it from the one before, which fades out of it (TURN-WORDS-ON). While
  ;; the turn runs, its newest finished thought stands under the live bar
  ;; (the THOUGHT the run holds, LIVE-THOUGHT-ROWS). CONTEXT carries the
  ;; turns' ends and words even for a partial projection.
  (let ((standard (eq *tool-detail* :standard))
        (items '())
        (run '())
        (held '())
        (aside '())
        (recap nil)
        ;; ENDED: the turns that have. NEWEST: each turn's newest words.
        (ended (make-hash-table :test #'equal))
        (newest (make-hash-table :test #'equal)))
    (when standard
      (dolist (row context)
        (when-let (turn row.turn-id)
          (cond (row.outcome (setf (gethash turn ended) t))
                ((and (assistant-text-p row) (position-if-not #'blank-char-p row.text))
                 (setf (gethash turn newest) row))))))
    (labels ((alone (row) (close-run) (forget-row-groups row) (push row items))
             (close-run ()
               (when run
                 (let* ((members (reverse run))
                        (first-call (find :tool-call members :key #'transcript-row-kind))
                        (anchor (if standard (first members) first-call))
                        (group (or (tool-groups anchor)
                                   (setf (tool-groups anchor) (make-tool-group nil))))
                        (key (if standard
                                 (list :process (or (and first-call (transcript-row-tool-call-id first-call))
                                                    (transcript-row-turn-id anchor)))
                                 (list :group (tool-row-expand-key anchor)))))
                   (unless (equal key group.key)
                     (setf group.line nil group.titled-from nil group.block nil))
                   (setf group.key key group.members members)
                   (dolist (member members)
                     (setf (row-groups member) group))
                   (push group items)))
               (dolist (row (append (reverse aside) (reverse held)))
                 (forget-row-groups row)
                 (push row items))
               (setf run '() held '() aside '())))
      (dolist (row rows)
        (cond
          ;; A Standard call the band holds is a call like any other.
          ((and (not standard) (transcript-background-row row backgrounds))
           (alone row))
          ;; Associate recap members in both modes without removing facts;
          ;; only Developer emits the group into the visible projection.
          ((and recap (equal row.turn-id (transcript-row-turn-id (first recap.members))))
           (setf recap.members (append recap.members (list row))
                 (row-groups row) recap))
          ((reflection-recap-p row)
           (close-run)
           (setf recap (or (tool-groups row) (setf (tool-groups row) (make-tool-group nil)))
                 recap.key (list :recap row.turn-id)
                 recap.members (list row)
                 (row-groups row) recap)
           (unless standard (push recap items)))
          ((and standard row.turn-id
                (not row.outcome) (not (transcript-user-prompt-row-p row)))
           (let ((open (or run aside)))
             (when (and open (not (row-turn-matches-p row (transcript-row-turn-id (first open)))))
               (close-run)))
           (cond ((or (member row.kind '(:tool-call :reasoning)) (row-notice-summary row))
                  (setf run (cons row (append held run)) held '()))
                 ;; Unsummarized notices and diagnostics must remain visible.
                 ((eq :system-notice row.kind) (alone row))
                 ;; The turn's words stand under its run, in the one place
                 ;; the newest holds; the others give it up.
                 ((assistant-text-p row)
                  (forget-row-groups row)
                  (when-let (on (and (eq row (gethash row.turn-id newest))
                                     (turn-words-on row (not (gethash row.turn-id ended)))))
                    (push on aside)))
                 ((or run aside) (push row held))
                 (t (forget-row-groups row) (push row items))))
          (standard (alone row))
          ((routine-call-p row)
           (when (and run (not (row-turn-matches-p row (transcript-row-turn-id (first run)))))
             (close-run))
           (setf run (cons row (append held run))
                 held '()))
          ;; Assistant text with nothing to say yet renders no block and breaks no run.
          ((and run
                (or (and (eq :reasoning (transcript-row-kind row))
                         (not (detail-open-p row))
                         (row-turn-matches-p row (transcript-row-turn-id (first run))))
                    (and (assistant-text-p row) (zerop (length (trim-block-text row.text))))))
           (push row held))
          (t (alone row))))
      (close-run))
    (let ((items (nreverse items))
          (seen '()))
      (when standard
        ;; The newest run of a running turn holds its thought.
        (dolist (item (reverse items))
          (when (and (tool-group-p item) (eq :process (first (tool-group-key item))))
            (let* ((members (tool-group-members item))
                   (turn (transcript-row-turn-id (first members)))
                   ;; The turn's newest run, while the turn runs.
                   (live (and (not (gethash turn ended)) (not (member turn seen :test #'equal))))
                   (round (and live (newest-round members))))
              (setf (tool-group-tail item)
                    ;; A turn that ended never left a call running.
                    (remove-if-not (lambda (row) (or (member row round :test #'eq)
                                                     (and (equal "running" (transcript-row-tool-status row))
                                                          (not (gethash turn ended)))
                                                     (transcript-background-row row backgrounds)))
                                   members)
                    (tool-group-thought item)
                    (and live
                         (find-if (lambda (row) (and (eq :reasoning (transcript-row-kind row))
                                                     (position-if-not #'blank-char-p (transcript-row-text row))))
                                  members :from-end t)))
              (cond (live (unless (live-run-rows item) (setf (live-run-rows item) 0)))
                    ((live-run-rows item)
                     ;; Its turn over or its place taken, what it held goes.
                     (forget-live-run-rows item)
                     (dolist (row members) (forget-held-window row))))
              (push turn seen)))))
      (group-mutation-rows items))))

(defparameter +words-fade-ms+ '(150 250)
  "How quickly, out and then in, a turn's words give their place to newer
ones: quickly enough to read a fast model. Without this, a slow out and in
would leave words a model sends a moment apart never all the way up.")

(defvar *turn-words* (make-hash-table :test 'equal)
  "A turn id -> the FADE of the place its words stand in, while the turn
runs and until that place stands still after it has ended.")

(defun turn-words-on (row live)
  "What the place of the words of ROW's turn shows, ROW being the newest of
them: ROW, or while it fades in behind them the words it took the place from.
LIVE: the turn is running, the only time its words fade."
  ;; History lays out as it stands: a turn met ended never fades, and one
  ;; whose place stood still once it ended lets the place go. A newest row
  ;; that only changed hands -- the durable fact seating a streamed row's
  ;; words, or a turn that ended -- takes the place with no fade.
  (let* ((turn row.turn-id)
         (fade (and *fades* (gethash turn *turn-words*))))
    (cond ((not *fades*) row)
          ((null fade)
           (when live
             (setf (gethash turn *turn-words*) (apply #'make-fade row +words-fade-ms+)))
           row)
          (t (unless (eq row (fade-shown fade))
               (if (and live (string/= (transcript-row-text (fade-shown fade)) row.text))
                   (fade-to fade row)
                   (setf (fade-shown fade) row)))
             (prog1 (fade-on fade)
               (when (and (not live) (fade-settled-p fade))
                 (remhash turn *turn-words*)))))))

(defun tool-group-open-p (group &optional (key *expanded-tool-call*))
  "Whether the accordion owns GROUP or one of its members. A member clicked
open under the run's shut title, (:FOLD KEY) (OPEN-KEY), leaves it shut."
  ;; A run opened while it was all thought is keyed by its turn; its first
  ;; call rekeys it, and the run stays open.
  (and key (or (equal key (tool-group-key group))
               (and (eq :process (first (tool-group-key group)))
                    (equal key (list :process (transcript-row-turn-id (first (tool-group-members group))))))
               (member key (tool-group-members group) :key #'tool-row-expand-key :test #'equal)
               (and (consp key)
                    (case (first key)
                      (:diff (member (second key) (tool-group-members group) :key #'tool-row-expand-key :test #'equal))
                      (:mutation (member (second key) (tool-group-members group) :test #'eq))))) t))

(defun run-trail-row-p (row)
  "Whether ROW is one a run lists under its title, a line each: a call or a
receipt."
  (or (eq :tool-call (transcript-row-kind row)) (and (row-notice-summary row) t)))

(nlk:define-side-table step-in (:weakness :key)
  "Weak memo: an lline -> STEPPED-IN's copy of it, one object while the source
stands, so LLINE-RENDER's memo holds.")

(defun stepped-in (line)
  "LINE two columns in, its spans and hang with it -- but for a line hooked
in the gutter (⎿, ⋮), whose words already stand two in from the hook."
  (cond ((member line.role '(:tool :tool-omission)) line)
        ((step-in line))
        (t (setf (step-in line)
                 (copy-with lline line :text (concatenate 'string "  " line.text)
                                       :spans (mapcar (lambda (span) (shifted-span span 2)) line.spans)
                                       :hang (+ 2 line.hang))))))

(defun without-note (row llines)
  "LLINES, what listed call ROW holds, without the note heading its code:
the line it is listed on says those words already."
  (if (and (rest llines) (equal (tool-summary-words row) (string-left-trim "; " (lline-text (first llines)))))
      (rest llines)
      llines))

(defun run-member-open-p (row &aux (key (tool-row-expand-key row)) (open (open-key)))
  "Whether a run's listed ROW shows what it holds under its line -- the
accordion holds it, or one of the files it changed, under the run's title
shut or open."
  (or (detail-open-p key)
      (and (consp open) (eq :diff (first open)) (equal (second open) key))))

(defun clip-lline (line width)
  "LINE cut to its first row at WIDTH, its spans with it. => LINE."
  (let ((end (length (setf line.text (clip-text-to-width line.text (lline-widths line width))))))
    (setf line.spans (loop for span in line.spans
                           when (< span.start end)
                             collect (copy-with mspan span :end (min end span.end))))
    line))

(nlk:define-side-table tail-llines (:weakness :key)
  "Weak memo: a call or receipt row -> RUN-TAIL-LLINE's line, one object while
what the row says and the width stand, so LLINE-RENDER's memo holds.")

(defun ember-call-p (row)
  "Whether call ROW still runs, here or in the background: its listed line
burns amber and the frame's ember warms it."
  (and (member row.tool-status '("running" "background") :test #'equal) t))

(nlk:define-side-table ember-line (:weakness :key)
  "A painted transcript row -> the running call whose listed line it is, so
the frame's ember (EMBER-OVERLAY) finds that line on screen.")

(defun run-tail-lline (row width)
  "A call or receipt ROW as one line under its folded run, on the column of
the run title's words: no label, a call's words grey, amber while it runs
here or in the background, and a Mutate's teal."
  ;; Two columns in from the title they read as a step off it.
  ;; A running call in grey would read as stopped; its amber is the
  ;; ground the ember warms (EMBER-OVERLAY).
  (memoized (tail-llines row)
    (width row.tool-name row.tool-args (length (or row.tool-args "")) row.tool-status row.tool-exit-code
     row.metadata row.error-p (row-load-errors row) row.text row.text-rev)
    ;; Opened, a run lists these lines where it showed Eval and Mutate cards
    ;; a receipt names its file after its comment, and a call
    ;; that changed files counts the lines.
    (clip-lline
     (if-let (summary (row-notice-summary row))
       (let* ((comment (json-value summary :text "comment"))
              (path (json-value summary :text "path"))
              (line (make-lline :text (nlk:one-line (or comment path (json-value summary :string "name")))
                                :tone :success)))
         (when (and comment path)
           (append-lline-run line (format nil " · ~a" (nlk:one-line path)) :tone :muted))
         line)
       (let ((line (make-lline :text (nlk:one-line (or (tool-summary-words row) (json-value row.tool-name :text) "…"))
                               :tone (if (ember-call-p row) :tool :muted))))
         (append-row-diff-stats line row)
         (append-status-text line (if (row-load-errors row) "error" row.tool-status)
                             (tool-failure-text row) (row-exit-document row))))
     width)))

(defun paragraph-start (text end &aux (line-end end))
  "Where the paragraph of TEXT that ends at END starts: the line after the
nearest blank line above END, else 0."
  (loop (let* ((newline (position #\Newline text :end line-end :from-end t))
               (line-start (if newline (1+ newline) 0)))
          (when (and (< line-end end)
                     (not (position-if-not #'blank-char-p text :start line-start :end line-end)))
            (return (1+ line-end)))
          (if newline (setf line-end newline) (return 0)))))

(defun thought-heading-p (text start end)
  "Whether the paragraph of TEXT from START to END is a heading: one line, a
`# ...' or a `**...**'."
  (let ((title (string-trim '(#\Space #\Tab #\Return) (subseq text start end))))
    (and (not (find #\Newline title))
         (or (alexandria:starts-with #\# title)
             (and (alexandria:starts-with-subseq "**" title) (alexandria:ends-with-subseq "**" title))))))

(defun thought-heading-start (text start)
  "Where the heading right above the paragraph of TEXT at START starts, or
NIL when no heading stands there."
  (when-let (above (position-if-not #'blank-char-p text :end start :from-end t))
    (let ((heading (paragraph-start text (1+ above))))
      (and (thought-heading-p text heading (1+ above)) heading))))

(defun finished-thought-end (row &aux (text row.text))
  "Where the finished thinking of thought ROW ends: its whole text once it
is done; while it arrives, before the paragraph being written -- a paragraph
is done once a blank line closes it -- and the heading over that paragraph.
NIL while nothing of it is finished."
  (when-let (at (position-if-not #'blank-char-p text :from-end t))
    (let ((start (paragraph-start text (1+ at))))
      (cond ((not (thinking-p row)) (1+ at))
            ;; Closed, and no heading opening the next one.
            ((and (>= (count #\Newline text :start at) 2)
                  (not (thought-heading-p text start (1+ at))))
             (1+ at))
            (t (when-let (done (position-if-not #'blank-char-p text
                                                :end (if (>= (count #\Newline text :start at) 2)
                                                         start
                                                         (or (thought-heading-start text start) start))
                                                :from-end t))
                 (1+ done)))))))

(defun thought-paragraph-llines (text end &aux (start (paragraph-start text end)))
  "The paragraph of thought TEXT ending at END, whole, as Markdown lines
under the heading that opens it when one does, blank and rule lines dropped:
the newest thinking the model has finished, until a newer paragraph is done
and takes its place."
  ;; A summary's `**Finding the label**' is a paragraph of its own, and the
  ;; words under it say nothing alone. Under the reasoning's shadow, a step
  ;; quieter than the calls' grey: without this, at the same tone it would
  ;; read as another call.
  (setf start (or (thought-heading-start text start) start))
  (loop for line in (markdown-llines (subseq text start end) :shadow t)
        unless (or line.rule (not (position-if-not #'blank-char-p line.text)))
          collect (copy-with lline line :tone :muted :italic t)))

(nlk:define-side-table thought-llines (:weakness :key)
  "Weak memo: a thought row -> THOUGHT-PARAGRAPH-LLINES' lines, the same
objects while the row's words and where its finished part ends stand, so
LLINE-RENDER's memo holds across ticks.")

(defun finished-thought (group)
  "The newest finished thinking of run GROUP, (ROW . END), or NIL: a thought
still arriving shows only the paragraphs it has closed, and one with none
leaves the place to the thought before it."
  (loop for row in (reverse (tool-group-members group))
        when (eq :reasoning (transcript-row-kind row))
          do (when-let (end (finished-thought-end row))
               (return (cons row end)))))

(defparameter +thought-fade-ms+ '(100 150)
  "How quickly, out and then in, a finished thought gives the place under
the live bar to the next: the first comes up, a newer one takes the place
quickly -- IN the time each of its tokens takes.  A fast model's thoughts
come a moment apart, each read, not watched rising.")

(defparameter +thought-token-step-ms+ 15
  "Milliseconds between one token of a finished thought starting to come up
and the next: the thought arrives a word at a time, in reading order
(fade every token), faster than it is read.")

(defparameter +thought-sweep-ms+ 450
  "The longest the tokens of a thought take to start coming up, first to
last: a long one steps faster rather than taking longer.")

(nlk:define-side-table thought-fades (:weakness :key)
  "A model -> the FADE of the place under its live bar where the newest
finished thought stands.")

(defun live-thought-rows (model width)
  "The newest finished thought of the running turn MODEL's transcript shows
-- in the run that holds its THOUGHT (GROUP-ROWS) -- as its newest closed
paragraph's rows under the live bar at WIDTH => (values PAINTED-LIST
PLAIN-LIST), NIL when there is none."
  ;; Without this, a thought row would stand above the bar while a run has
  ;; calls, and the operator would read it as the wrong side of the line
  ;; saying the model is thinking. Only what the model has finished
  ;; thinking shows, faded in, never the words still arriving. The
  ;; projection the frame shows holds the run; under a dialog that is the
  ;; previewed session's, whose bar the frame shows too.
  (when (or (chat-model-turn-in-flight model)
            (and (chat-model-dialog model) (chat-model-transcript-preview model)))
    (let* ((block (find-if (lambda (row) (and (tool-group-p row) (tool-group-thought row)))
                           (seventh (chat-model-projection-cache model))
                           :key #'display-block-row :from-end t))
           (group (and block (display-block-row block)))
           (thought (and group (finished-thought group)))
           (fade (thought-fades model)))
      ;; The place fades only between thoughts of one run: a thought of a run
      ;; gone, or of a turn before, never goes out under this one.
      (cond ((not (and *fades* thought)) (setf fade nil (thought-fades model) nil))
            ((or (null fade) (not (member (car (fade-on fade)) (tool-group-members group))))
             (setf fade (setf (thought-fades model) (apply #'make-fade thought +thought-fade-ms+))))
            (t (fade-to fade thought)))
      (when-let (on (if fade (fade-on fade) thought))
        (destructuring-bind (row . end) on
          (multiple-value-bind (rows plains)
              (llines-rows (memoized (thought-llines row) (row.text end)
                             (thought-paragraph-llines row.text end))
                           width)
            ;; What holds the place comes up a token at a time.
            (when (and fade (equal on (fade-shown fade)))
              (let ((tokens (loop for plain in plains
                                  sum (count-if #'plusp (uiop:split-string plain :separator " ") :key #'length))))
                (setf (fade-tokens fade) (max 1 tokens)
                      (fade-step fade) (min +thought-token-step-ms+
                                            (floor +thought-sweep-ms+ (max 1 tokens))))))
            (values (if fade (mark-fading-rows rows fade on) rows) plains)))))))

(defun listed-output (row width backgrounds &aux (held (held-window row)))
  "What call ROW's exec has written, as the list of the one block a run lists
under the call's line at WIDTH; once the exec is gone, the block it last
showed (HELD-WINDOW), so a finished call of the standing round keeps it."
  (if-let (exec (transcript-background-row row backgrounds))
    (multiple-value-bind (rows plains) (exec-output-rows exec width 3 :expandable nil)
      (when rows
        (list (cdr (setf (held-window row)
                         (cons width (make-display-block :retention :live-only :kind :tool-call
                                                         :row exec :lines rows
                                                         :plain-lines plains)))))))
    (and held (eql width (car held)) (list (cdr held)))))

(defun tool-group-display-block (group width &optional backgrounds)
  "GROUP rendered: its title, then a run's listing (LISTING) or another
group's open members, or while closed its TAIL and any call clicked open under
it, with their canonical padding. A process title has one gap before its
first visible member. A listed call still running as one of BACKGROUNDS,
the state's live execs, shows what it has written under it."
  ;; The closed block is memoized on the group while its line, the width, the
  ;; theme and the hover stand; an open group, or one listing its calls,
  ;; composes on every frame, because the bodies it shows may still be moving
  ;; (a listed call's line is memoized on its row, RUN-TAIL-LLINE).
  ;; OPEN: the accordion holds the group or one of its members.
  (let* ((open (tool-group-open-p group))
         ;; The one line, present tense while a call runs: `Reading 2 files…'.
         (calls (remove :tool-call group.members :key #'transcript-row-kind :test-not #'eq))
         (running (some (lambda (row) (equal "running" row.tool-status))
                        calls))
         (activities (mapcar #'row-activity calls))
         ;; Worded again only when a call's activity or the tense moved: the
         ;; words are a function of those alone (ROW-ACTIVITY is memoized per
         ;; row), and wording a group -- its file links percent-encoded -- was
         ;; two fifths of a full render of a long session. A
         ;; recap has no calls and a line of its own.
         (process (eq :process (first group.key)))
         (mutation (eq :mutation (first group.key)))
         (disclosure (or process mutation))
         ;; A run lists its trail a line each (LISTING), the one view live
         ;; and opened: the Eval, Think and Mutate cards it opened to were the
         ;; view before it.
         (listing (cond (open (and process (remove-if-not #'run-trail-row-p group.members)))
                        ;; A call clicked open under the shut title stays
                        ;; listed there past its round, until a click
                        ;; closes it.
                        ;; So does a call that changed files: what an edit
                        ;; changed is the turn's work, and shut away behind
                        ;; `Ran 2 commands' it read as commands run.
                        ((and process (some (lambda (row) (or (run-member-open-p row) (row-edited-p row)))
                                            group.members))
                         (remove-if-not (lambda (row) (or (member row group.tail :test #'eq)
                                                          (run-member-open-p row)
                                                          (row-edited-p row)))
                                        group.members))
                        (t group.tail)))
         ;; A disclosure's state rides the gutter: ▸ shut, ▾ open.
         (role (if open :disclosure-open :disclosure))
         (words (cond
                  (mutation
                   ;; GROUP-MUTATION-ROWS keeps one receipt per file, so the members count them.
                   (let ((fresh (title-phrases-lline (list (list "Mutate" "" nil nil)) :verb-tone :success)))
                     (setf fresh.role role fresh.gutter-tone :muted)
                     (append-title-detail fresh (format nil "~d files" (length group.members)))
                     fresh))
                  (process
                   ;; A live run counts up as its calls land, `…' while one runs.
                   ;; Without this, one that has run nothing yet would say `Ran 0
                   ;; commands' and would read as nothing happening.
                   ;; A call that changed files counts as the files it
                   ;; edited, not as a command: `Edited 2 files +4 -2'.
                   (let* ((mutations (count-if #'row-notice-summary group.members))
                          (mutated (if (plusp mutations) (format nil ", Mutated ~d time~:p" mutations) ""))
                          (edited (remove-if-not #'row-edited-p calls))
                          (files (loop for row in edited
                                       append (loop for file across (json-array row.metadata "workspace_diff" "files")
                                                    collect (json-value file :text "path"))))
                          (commands (- (length calls) (length edited)))
                          (line (make-lline :text (cond ((and edited files)
                                                         (format nil "~:[Edited~;Ran ~:*~d command~:p, edited~] ~d file~:p~a~:[~;…~]"
                                                                 (and (plusp commands) commands)
                                                                 (length (remove-duplicates files :test #'equal))
                                                                 mutated running))
                                                        ((or calls (plusp mutations))
                                                         (format nil "Ran ~d command~:p~a~:[~;…~]" (length calls)
                                                                 mutated running))
                                                        ((some (lambda (row) (and (eq :reasoning row.kind) (thinking-p row)))
                                                               group.members)
                                                         "Thinking…")
                                                        (t "Thought"))
                                            :tone :muted :role role :gutter-tone :muted)))
                     (when (and edited files (not running))
                       (loop for row in edited
                             sum (loop for file across (json-array row.metadata "workspace_diff" "files")
                                       sum (or (json-value file :integer "added") 0))
                               into added
                             sum (loop for file across (json-array row.metadata "workspace_diff" "files")
                                       sum (or (json-value file :integer "removed") 0))
                               into removed
                             finally (append-lline-run line (format nil " +~d" added) :tone :diff-added)
                                     (append-lline-run line (format nil " -~d" removed) :tone :diff-removed)))
                     line))
                  ;; A reflection's recap as its one quiet line: `Reflection recap', and
                  ;; while it is closed the first words of its answer, cut to WIDTH so the
                  ;; line stays one row. The web page's recap summary, whose words hide
                  ;; while it is open: the answer is right under it then.
                  ((eq :recap (first group.key))
                   (let* ((answer (find-if #'assistant-text-p group.members))
                          (fresh (make-lline :text (if (and answer (not open))
                                                       (format nil "Reflection recap · ~a"
                                                               (nlk:one-line
                                                                (transcript-row-text answer)))
                                                       "Reflection recap")
                                             :tone :muted)))
                     (setf fresh.text (clip-text-to-width fresh.text (lline-widths fresh width)))
                     fresh))
                  ((equal (cons running activities) group.titled-from) group.line)
                  (t
                   (let ((fresh (title-phrases-lline
                                 (activity-entries-phrases
                                  (distinct-entries (loop for activity in activities
                                                          when activity append activity.entries))
                                  :running running))))
                     (dolist (call calls) (append-title-detail fresh (tool-note call)))
                     (when running (setf fresh.text (concatenate 'string fresh.text "…")))
                     (setf group.titled-from (cons running activities))
                     fresh))))
         ;; The title is WORDS, the line GROUP already holds kept while the two lay out
         ;; the same: one lline object per unchanged title, so LLINE-RENDER's memo holds.
         (title (if (and group.line (lline-content-equal group.line words))
                    group.line
                    (setf group.line words)))
         (hover (eq group *hover-row*)))
    (flet ((compose (&aux (llines (if disclosure (list (cons title group))
                                      (list (cons title group) (cons (block-pad-lline nil) group))))
                          ;; How many lines the member listed before this one took, NIL before
                          ;; the first.
                          (previous nil)
                          ;; The listed lines of calls still running, whose rows the ember warms.
                          (embers '()))
             (when (or listing (and open (not process)))
               ;; An open recap shows its answer and its divider; its
               ;; question, which no one here asked, stays in the log.
               (loop for member in (or listing group.members)
                     for body = (cond
                                  (listing
                                   (cons (let ((line (run-tail-lline member width)))
                                           ;; A click opens it, so the pointer accents
                                           ;; it as it does the title.
                                           (cond ((eq member *hover-row*) (disclosure-hover-lline line))
                                                 (t (when (ember-call-p member) (push line embers))
                                                    line)))
                                         ;; A listed line clicked open shows what its
                                         ;; card held, two columns in under it, the
                                         ;; card's title aside.
                                         (append
                                          ;; A call that forked lists its forks under it,
                                          ;; the rail under the call's first word: stepped
                                          ;; in, it stood two columns right of everything.
                                          (fork-tree-llines member width)
                                          (cond ((run-member-open-p member)
                                                 (mapcar #'stepped-in (without-note member (rest (row-body-llines member width)))))
                                                ;; Shut, a call that changed files
                                                ;; still shows them, each diff's head.
                                                ((row-edited-p member)
                                                 (mapcar #'stepped-in (workspace-diff-llines member))))
                                          (listed-output member width backgrounds))))
                                  ((tool-group-p member) (list (tool-group-display-block member width backgrounds)))
                                  (mutation
                                   (cons (make-lline :text (format nil "  ~a" (row-mutation-path member))
                                                     :tone :muted)
                                         (when (detail-open-p member)
                                           (system-notice-llines (trim-block-text (transcript-row-text member)) nil))))
                                  ((not (reflection-recap-p member))
                                   (if (eq *tool-detail* :standard)
                                       (list (row-display-block member :width width))
                                       (row-body-llines member width))))
                     when (and body (not (and (display-block-p (first body))
                                              (null (display-block-lines (first body))))))
                       ;; A canvas row between the title and its first call,
                       ;; a listed run's too.
                       do (when (and process (null previous)
                                     (not (and (display-block-p (first body))
                                               (transcript-row-p (display-block-row (first body)))
                                               (row-block-padding-p (display-block-row (first body))))))
                            (push (cons (block-pad-lline nil) group) llines))
                          ;; A canvas row between two listed calls when either
                          ;; shows more than its line: run together, the next
                          ;; call's line read as more of the last one's output.
                          (when (and listing previous (or (> previous 1) (rest body)))
                            (push (cons (block-pad-lline nil) group) llines))
                          (unless disclosure (push (cons (block-pad-lline nil) group) llines))
                          (setf previous (length body))
                          (dolist (line body) (push (cons line member) llines))))
             ;; Process boundaries are spaced by TRANSCRIPT-LINE-TABLE,
             ;; reusing a neighbouring assistant's canonical canvas pad.
             (when (not disclosure)
               (push (cons (block-pad-lline nil) group) llines))
             (setf llines (nreverse llines))
             (when hover
               ;; Completed work is a text disclosure, not a filled card.
               ;; Accent only its title; keep the shared line and members intact.
               (setf llines (loop for (line . owner) in llines
                                  collect (cons (cond ((not disclosure)
                                                       (copy-with lline line
                                                                  :fill (or line.fill :hover)))
                                                      ((eq line title)
                                                       (disclosure-hover-lline title))
                                                      (t line))
                                                owner))))
             (loop for (line . owner) in llines
                   for block-p = (display-block-p line)
                   for (rows plain-rows) = (if block-p
                                               (list (display-block-lines line)
                                                     (display-block-plain-lines line))
                                               (multiple-value-list (lline-render line width)))
                   do (when (member line embers :test #'eq)
                        (dolist (row rows) (setf (ember-line row) owner)))
                   append rows into lines
                   append plain-rows into plains
                   append (or (and block-p (display-block-refs line))
                              (make-list (length rows) :initial-element owner))
                     into refs
                   finally (return (make-display-block
                                    :retention (if mutation :live-only :persisted-history)
                                    :kind :tool-call
                                    :row group
                                    :refs refs
                                    :lines lines
                                    :plain-lines plains)))))
      ;; A shut run listing only settled calls that changed files shows the
      ;; same lines until the pointer or the accordion moves: those two ride
      ;; its memo, NIL for a run listing nothing so they never wake it.
      (if (or open (some (lambda (row) (or (member row group.tail :test #'eq) (run-member-open-p row)))
                         listing))
          (compose)
          (memoized (tool-group-block group) (title width *palette* *color-profile* hover disclosure
                                                    (and listing *hover-row*) (and listing *expanded-tool-call*))
            (compose))))))

(defun project-state (state &key (width 80) start end prompt)
  "Project conversation in chronological order. Pending execs replace their
own call's display block, never the fixed composer status. Subscribe snapshots
without a call row remain live-only blocks, in their registry order."
  (let* ((all (transcript-rows state))
         (backgrounds (state-background-rows state))
         (tail (or (and start (member start all :test #'eq)) all))
         (selected (if (and prompt start (notany #'transcript-user-prompt-row-p tail))
                       (let ((question (find-if #'transcript-user-prompt-row-p (ldiff all tail) :from-end t)))
                         (if question (cons question tail) tail))
                       tail))
         (rows (loop for row in selected until (eq row end)
                     unless (eq :pending-input (transcript-row-kind row)) collect row)))
    (append
     (loop for item in (group-rows rows all backgrounds)
           for exec = (transcript-background-row item backgrounds)
           for block = (if (tool-group-p item) (tool-group-display-block item width backgrounds)
                           (row-display-block (or exec item) :width width))
           ;; A turn's words in a place still fading (TURN-WORDS-ON).
           do (when-let (fade (and *fades* (assistant-text-p item)
                                   (gethash item.turn-id *turn-words*)))
                (when block (mark-fading-rows (display-block-lines block) fade item)))
           when (and block (display-block-lines block)) collect block)
     (unless end
       (loop for exec in backgrounds
             unless (some (lambda (row) (eq exec (transcript-background-row row backgrounds))) all)
               collect (row-display-block exec :width width))))))

;;; --- the projected tail ---------------------------------------------------
;;; A transcript projects from the row its window has grown to, not from its
;;; first: the window shows a few dozen lines of what can be thousands of rows.
;;; A state met for the first time — the replay an attach or a resume lands —
;;; projects only enough of its tail to fill *TAIL-PROJECTION-SCREENS* screens,
;;; and a session previewed one window (TRANSCRIPT-PROJECTION-START). Without
;;; this, a first projection of a large transcript would lay out every row
;;; before its first frame, stalling the UI thread. Scrolling to within a
;;; window of where the projection starts grows it (GROW-TRANSCRIPT-PROJECTION);
;;; Home and a found-block pin project it whole.

(defparameter *tail-projection-screens* 3
  "How many terminal heights of lines a state's first projection covers.")

(defun run-breaking-row-p (row)
  "Whether ROW closes any run of routine calls before it (GROUP-ROWS), so a
projection that starts at it groups every row after it as the whole one does."
  ;; A call closes one when it joins none -- it changed something, or failed.
  ;; A Standard call is taken as a boundary too: PROJECTION-GROUP-OPENING
  ;; moves any boundary inside a Standard run, live or completed, back to the
  ;; run's first row.
  (case row.kind
    (:tool-call (or (eq *tool-detail* :standard) (not (routine-call-p row))))
    ((:reasoning :pending-input) nil)
    (:text (not (and (equal "assistant" row.role)
                     (zerop (length (trim-block-text row.text))))))
    (t t)))

(defun projection-group-opening (rows index &aux (turn (transcript-row-turn-id (aref rows index))))
  "INDEX moved to its group's first row: a mutation, Standard process, or recap."
  (or (when (and turn (eq *tool-detail* :standard))
        ;; A Standard run is its turn's work trail, live or done (GROUP-ROWS).
        (flet ((work-p (row) (and (equal turn row.turn-id)
                                 (or (member row.kind '(:tool-call :reasoning)) (row-notice-summary row)))))
          (let ((first (position-if #'work-p rows))
                (last (position-if #'work-p rows :from-end t)))
            (and first last (<= first index last) first))))
      (when (row-mutation-path (aref rows index))
        (loop while (and (plusp index) (row-mutation-path (aref rows (1- index)))
                         (equal turn (transcript-row-turn-id (aref rows (1- index)))))
              do (decf index)
              finally (return index)))
      (and turn (position-if (lambda (row) (and (reflection-recap-p row) (equal turn row.turn-id)))
                             rows :start (max 0 (- index 2)) :end (1+ index)))
      index))

(defun tail-projection-start (state width lines &optional from)
  "The latest row of STATE a projection can start at whose rows before row
FROM -- where a growing projection starts now; the end when NIL -- project
at WIDTH to LINES lines, or NIL for the first row."
  ;; The look walks back from FROM a run at a time -- a row that closes the
  ;; runs before it (RUN-BREAKING-ROW-P) and the calls after it, which group
  ;; as the whole projection groups them, and never a row inside a recap
  ;; (PROJECTION-GROUP-OPENING) -- and lays out only that run, so a projection takes in
  ;; at most one run past what it asked for. Without this, walking back
  ;; a row count at a time would lay out rows of every height many times
  ;; the lines asked for, and walking from the end would project the whole
  ;; tail at every look.
  (let* ((rows (coerce (transcript-rows state) 'vector))
         (end (or (and from (position from rows)) (length rows)))
         (total 0))
    (when (plusp end)
      (loop for index = (projection-group-opening rows (or (position-if #'run-breaking-row-p rows :end end :from-end t) 0))
            do (incf total (reduce #'+ (project-state state :width width :start (aref rows index)
                                                            :end (and (< end (length rows)) (aref rows end)))
                                   :key (lambda (block) (length block.lines))))
            until (or (zerop index) (>= total lines))
            do (setf end index)
            finally (return (and (plusp index) (aref rows index)))))))

;;; --- transcript rhythm -----------------------------------------------------

(defun transcript-line-table (blocks &aux (lines '())
                                          (plains '())
                                          (refs '())
                                          (previous-fill nil)
                                          (previous-heavy nil))
  "Flatten BLOCKS into parallel paint, copy and hit-test lanes."
  ;; Blocks stand one canvas row apart: two touching canvas pads share one
  ;; row, a canvas pad on either side is the row, and a user, filled or
  ;; process block with none gets an unowned one. Without this, two messages
  ;; would stand two rows apart and a message would sit one row off a run.
  ;; Never filter body-owned blank rows.
  (flet ((emit (painted plain ref)
           (push painted lines) (push plain plains) (push ref refs))
         (canvas-pad-p (plain owner)
           (and plain (zerop (length (string-trim '(#\Space) plain)))
                (transcript-row-p owner)
                (row-block-padding-p owner)
                (null (row-block-fill owner)))))
    (dolist (block blocks)
      (when (display-block-lines block)
        (let* ((fill block.fill)
               (heavy (or (and (transcript-row-p block.row)
                               (eq :text (transcript-row-kind block.row))
                               (equal "user" (transcript-row-role block.row)))
                          (and (tool-group-p block.row)
                               (eq :process (first (tool-group-key block.row))))))
               (opening-pad-p (canvas-pad-p (first block.plain-lines)
                                           (if block.refs (first block.refs) block.row)))
               (closing-pad-p (canvas-pad-p (first plains) (first refs))))
          (when lines
            (cond ((and closing-pad-p opening-pad-p)
                   ;; The earlier block gives its closing pad up: a block's
                   ;; own first row stays its pad (IMAGE-BAND-AT).
                   (pop lines) (pop plains) (pop refs))
                  ((or closing-pad-p opening-pad-p))
                  ((or (and previous-fill fill) previous-heavy heavy)
                   (emit "" "" nil))))
          (loop with refs = block.refs
                for line in (display-block-lines block)
                for plain-tail = block.plain-lines then (rest plain-tail)
                for owner = (if refs (pop refs) block.row)
                do (emit line (or (first plain-tail) "") owner))
          (setf previous-fill fill previous-heavy heavy)))))
  (values (nreverse lines) (nreverse plains) (nreverse refs)))

;;;; C2 additions to ui.lisp --- the live program adapter

;;; --- TUI model and generics ---------------------------------------------
;;; view and update are the terminal core's CLOS generics (term/message.lisp),
;;; so the organism can redefine how it presents itself and how it interprets
;;; input.

(defclass chat-model ()
  ((session-id :initarg :session-id :reader chat-model-session-id)
   (band :initform nil :accessor chat-model-band)
   (editor :initform nil :accessor chat-model-editor)
   (state :initform nil :accessor chat-model-state)
   ;; Where the terminal's hardware cursor parks this frame: (col . row),
   ;; zero-based frame cells, or NIL to hide it. COMPOSE-FRAME writes it
   ;; while stacking rows (the composer caret cell); SHELL-VIEW reads it
   ;; into the view-state so the renderer emits the real cursor — the Zig
   ;; composer.zig cursor_visible/cursor contract on vaxis.
   (parked-cursor :initform nil :accessor chat-model-parked-cursor)
   (term-width :initform 80 :accessor chat-model-term-width)
   (term-height :initform 24 :accessor chat-model-term-height)
   (turn-in-flight :initform nil :accessor chat-model-turn-in-flight)
   ;; Live status clock: when the in-flight turn started (internal time) and
   ;; the latest activity as (verb-string . internal-time). Chrome inputs for
   ;; the live turn bar; never durable.
   (turn-started-at :initform nil :accessor chat-model-turn-started-at)
   ;; Per-turn memo for the live bar's model label: (started-at-stamp . label).
   ;; The gateway lane's label reads the durable session pin from the store;
   ;; caching per turn keeps that read off the ~150ms animation path.
   (live-label-cache :initform nil :accessor chat-model-live-label-cache)
   ;; Frame-clock support. PROJECTION-CACHE holds the last full transcript
   ;; projection as (width palette profile lines plains refs) so an
   ;; animation tick can recompose the live chrome without re-projecting the
   ;; whole transcript (RENDER-STATUS-FRAME). The palette and profile ride
   ;; along because the projected rows are SGR-baked strings, so they are
   ;; stale the moment either moves (PROJECTION-CACHE-LIVE-P). TRANSCRIPT-DIRTY marks state mutated by a
   ;; stream reducer without an immediate render: the next frame-clock tick
   ;; folds it into one full render — the Zig stream_framerate coalescing
   ;; rule, which bounds re-projection to the frame cadence instead of the
   ;; stream's arrival rate. Both live-only.
   (projection-cache :initform nil :accessor chat-model-projection-cache)
   ;; The live chrome as STATUS-CHROME-KEY read it at the last compose; a
   ;; tick composes again only when it reads differently. Live-only.
   (status-chrome-key :initform nil :accessor chat-model-status-chrome-key)
   ;; (STATE . ROW): the transcript row the projection of STATE starts at,
   ;; NIL for its first. A state the window has not grown into projects its
   ;; tail (TRANSCRIPT-PROJECTION-START). Live-only.
   (projection-start :initform nil :accessor chat-model-projection-start)
   ;; A boot's composer catalog refresh, put off until the frame the attach
   ;; is for (REFRESH-COMPOSER-CATALOGS-BEHIND-THE-FRAME). Live-only.
   (catalog-refresh-due :initform nil :accessor chat-model-catalog-refresh-due)
   ;; The home mark's glow (PLAY-MARK-GLOW): NIL until a launch (NLT:INIT
   ;; with a program) or a click starts one, (KIND . START) while it plays,
   ;; :DONE once it has (MARK-GLOWING-P); the times of the clicks that may
   ;; yet make five (CLICK-MARK); how far the keys have come through the
   ;; Konami code (KONAMI-KEY). Live-only.
   (mark-glow :initform nil :accessor chat-model-mark-glow)
   ;; Whether the band the last compose stacked was the home screen, :NEVER
   ;; before the first (COMPOSE-TRANSCRIPT-VIEWPORT). Live-only.
   (home-painted :initform :never :accessor chat-model-home-painted)
   (mark-clicks :initform nil :accessor chat-model-mark-clicks)
   (konami-progress :initform 0 :accessor chat-model-konami-progress)
   (transcript-dirty :initform nil :accessor chat-model-transcript-dirty)
   (turn-activity :initform nil :accessor chat-model-turn-activity)
   ;; The live bar's count of the running turn's tool calls — `12 actions'
   ;; — counted at the full render from the rows, read by every animation
   ;; tick. Live-only.
   (turn-tally :initform nil :accessor chat-model-turn-tally)
   ;; The dropdown selection index. The dropdown itself is derived state:
   ;; active exactly while SLASH-SUGGESTIONS of the live composer text is
   ;; non-empty, so only the selection needs storing (clamped at read).
   (slash-selection :initform 0 :accessor chat-model-slash-selection)
   ;; Prompt history (Zig ComposerState history): newest-first entries,
   ;; the index while navigating (NIL when editing), and the stashed draft
   ;; the walk started from — restored when Down passes the newest match.
   (prompt-history :initform nil :accessor chat-model-prompt-history)
   (history-index :initform nil :accessor chat-model-history-index)
   (history-stash :initform "" :accessor chat-model-history-stash)
   (history-loaded :initform nil :accessor chat-model-history-loaded)
   ;; Whether the transcript follows its tail. T until the operator scrolls
   ;; up; re-pinned when scrolling returns to the bottom and on submit.
   ;; While pinned, every render snaps to the newest rows (the streaming
   ;; follow); unpinned, new content leaves the window where it is.
   (follow-bottom :initform t :accessor chat-model-follow-bottom)
   ;; The open modal picker (PICKER-DIALOG), or NIL. While open it owns
   ;; every key; the frame overlays it above the composer like the slash
   ;; dropdown (the Zig dialog family: /tree, /sessions, pickers).
   (dialog :initform nil :accessor chat-model-dialog)
   ;; `nodecode setup' asked for the setup wizard on the first frame; the
   ;; first-run detector (SETUP-REQUIRED-P, setup.lisp) opens it unasked.
   (setup-requested :initarg :setup-requested :initform nil
                    :accessor chat-model-setup-requested)
   ;; The setup walk in progress (setup.lisp SETUP-RUN), or NIL: set where a
   ;; walk starts, cleared where it ends. Live-only. While it is on screen a
   ;; footer with no model configured names the walk, not the way to start
   ;; one (FOOTER-FRAME-ROW).
   (setup-walk :initform nil :accessor chat-model-setup-walk)
   ;; The landing screen's tip (HOME-TIP), picked at the first landing
   ;; screen and kept for the shell's life. Live-only.
   (home-tip :initform nil :accessor chat-model-home-tip)
   ;; Ctrl+F over an open dialog (TOGGLE-DIALOG-FULLSCREEN): T sizes the
   ;; docked panel to every row under the frame head, over the Zig docked
   ;; height and over a dialog's own BODY-ROWS. One preference for every
   ;; dialog, kept for the shell's life and never persisted (live-only): a
   ;; picker closed and reopened comes back at the size the operator chose.
   (dialog-fullscreen :initform nil :accessor chat-model-dialog-fullscreen)
   ;; The open dialog's transcript substitution (the Zig
   ;; sessions_dialog_preview lane): (SESSION-ID . STATE) while a picker
   ;; previews another session's folded history, NIL otherwise. Live-only
   ;; and detached — the live state stays resident underneath, so closing
   ;; the dialog is the restore, by construction.
   (transcript-preview :initform nil :accessor chat-model-transcript-preview)
   ;; Ctrl+O: Developer or Standard viewport. Live-only, shell-local.
   (tool-detail :initform :standard :accessor chat-model-tool-detail)
   ;; The one clicked-open card, group or thought's expand key (the Zig
   ;; expandedToolCallId accordion), or NIL.
   (expanded-tool-call :initform nil :accessor chat-model-expanded-tool-call)
   ;; The resolved keymap: action keyword -> KEY-CHORD list, defaults
   ;; overlaid with the ~/.nodecode/keybinds.json overrides once at
   ;; model creation (keys.lisp RESOLVE-KEYBINDS).
   (keymap :initform (resolve-keybinds) :accessor chat-model-keymap)
   ;; Collapsed pastes: (id . text) newest-last, expanded into the outgoing
   ;; prompt at submit (the Zig ComposerState.pastes).
   (pastes :initform nil :accessor chat-model-pastes)
   (paste-counter :initform 0 :accessor chat-model-paste-counter)
   ;; Images staged for the next prompt: (media-type . base64) — drained at
   ;; submit (the Zig ComposerState.images pending-attachment slot).
   (pending-images :initform nil :accessor chat-model-pending-images)
   ;; The composer's @ catalog, walked on a thread of its own and never on a
   ;; trigger's keystroke (START-COMPOSER-CATALOG-WALK); the -LOADED flag
   ;; means "the walk answered", so a trigger that finds it NIL asks for the
   ;; walk again rather than waiting on it.
   (file-catalog :initform nil :accessor chat-model-file-catalog)
   (file-catalog-loaded :initform nil :accessor chat-model-file-catalog-loaded)
   ;; Mouse state: the transcript line table (parallel painted/plain/row
   ;; vectors stamped by RENDER-STATE), hover row, drag-selection state,
   ;; the armed scrollbar drag, the armed tool-block press, and the armed   ;; link press (its URL).
   (line-plain :initform #() :accessor chat-model-line-plain)
   (line-rows :initform #() :accessor chat-model-line-rows)
   (hover-row :initform nil :accessor chat-model-hover-row)
   ;; The target under the pointer, so the span carrying it underlines while
   ;; the pointer is there and a reader can see what a click would reach: a
   ;; URI, or (:STOP . EXEC-ID) for the band's ✕, which lights instead.
   (hover-target :initform nil :accessor chat-model-hover-target)
   (selection :initform nil :accessor chat-model-selection)
   (selection-anchor :initform nil :accessor chat-model-selection-anchor)
   (selection-dragged :initform nil :accessor chat-model-selection-dragged)
   ;; The armed scrollbar drag, (SURFACE . GRAB): SURFACE :transcript for the
   ;; band's gutter or :dialog for the open picker's own bar; GRAB the rows
   ;; between the pointer and the thumb's top. One slot — one bar at a time.
   (scrollbar-drag :initform nil :accessor chat-model-scrollbar-drag)
   ;; Whether the pointer rests on the transcript's scroll rail, which lights
   ;; its thumb (RAIL-HOT-P).
   (rail-hover :initform nil :accessor chat-model-rail-hover)
   (pressed-tool-row :initform nil :accessor chat-model-pressed-tool-row)
   (pressed-link :initform nil :accessor chat-model-pressed-link)
   ;; The armed press on a band row's ✕: the exec id its release interrupts.
   (pressed-stop :initform nil :accessor chat-model-pressed-stop)
   ;; The armed press anywhere else on an exec's band lines: the exec id
   ;; its release opens or closes in place.
   (pressed-exec :initform nil :accessor chat-model-pressed-exec)
   ;; What the shell says about what the operator just did or asked, in the
   ;; composer's placeholder: a COMPOSER-STATUS, or NIL (SHOW-STATUS).
   (composer-status :initform nil :accessor chat-model-composer-status)
   ;; The toasts up on the frame, newest first (TOAST); what each notice
   ;; key last said, so a keyed notice toasts once for its words; the toast
   ;; under the pointer; and where the last frame placed them (PLACE-TOASTS).
   ;; Live-only.
   (toasts :initform nil :accessor chat-model-toasts)
   (toasts-seen :initform (make-hash-table :test #'equal) :accessor chat-model-toasts-seen)
   (toast-hover :initform nil :accessor chat-model-toast-hover)
   (toast-layout :initform nil :accessor chat-model-toast-layout)
   ;; Kitty graphics: T once the terminal answered the capability query
   ;; (init sends NLT:REQUEST-KITTY-GRAPHICS; absence of a reply leaves the
   ;; fallback rendering), plus the transmitted-image cache — payload text
   ;; -> NLT:TERMINAL-IMAGE (or :FAILED), bounded like the Zig AssetCache.
   (kitty-graphics :initform nil :accessor chat-model-kitty-graphics)
   (kitty-image-cache :initform (make-hash-table :test #'equal)
                      :accessor chat-model-kitty-image-cache)
   ;; The open /term line-projection panel (exec.lisp TERMINAL-PANEL), or
   ;; NIL. Live-only chrome fed by the same background-process frames the
   ;; band consumes; a session switch or resync starts panel-less.
   (program :initform nil :accessor chat-model-program))
  (:documentation "The live TUI model holding folded state and UI components."))

;;; --- initialization -----------------------------------------------------

(defun transcript-text-width (width)
  "Columns transcript rows wrap at: the frame width minus the 2-column left
inset and the 2 the scroll gutter keeps at the right edge, a blank and the
rail, so the margins match (the one width authority for transcript content)."
  ;; The Zig gutter kept 5 (2 blank, the bar, 2 blank): the text stopped five
  ;; columns short of the edge and the strip read as an empty pane beside it.
  (max 1 (- width 4)))

(defun composer-box-width (width)
  "Columns the composer box spans: the transcript's own, so the box ends where
the message boxes do and the scroll gutter's columns stay clear beside it."
  ;; Without this, the Zig box would span WIDTH - 4 and stick out three
  ;; columns past every message box above it.
  (transcript-text-width width))

(defun composer-text-inset ()
  "Columns between the composer box's left edge and its text: the prose
gutter a sent prompt's words start on, so typed text stays put on Enter."
  ;; The box and the transcript start on the same column. The text sits 1 in,
  ;; on the gutter rail. Without this, every sent prompt would jump right into
  ;; its band.
  (nth-value 2 (body-gutter :prose)))

(defun composer-text-width (width)
  "Columns the composer's text wraps at — the one width authority for
composer content, and a different number from TRANSCRIPT-TEXT-WIDTH even
where they happen to agree."
  ;; The text starts at COMPOSER-TEXT-INSET. TWO trailing columns stay
  ;; reserved: the parked hardware cursor takes the first when the caret sits
  ;; at the end of a full row, and the second stays padding — one whole cell
  ;; of it — so neither the text nor the caret ever touches the box's right
  ;; edge.
  (max 1 (- (composer-box-width width) (composer-text-inset) 2)))

(defun shell-footer-p (height)
  "Whether the shell carries the footer band and the boxed composer; short
windows drop both (Zig shellFooterRow returns null below 12 rows and the
composer falls back to its single-line mode)."
  (>= height 12))

(defun shell-overhead (height &optional (composer-rows 1))
  "Rows the shell chrome owns around the transcript, the Zig ChatShell
stack: blank row 0, the header row, the chat top-padding row (3), the
transcript/composer gap (1), then either the composer box — COMPOSER-ROWS
text rows between two pad rows — the composer/footer gap, the footer, and
the final blank, or, below 12 rows, the single-line composer and two
trailing blank rows (3). The box grows with its content, so the transcript
band yields exactly the extra text rows."
  (if (shell-footer-p height)
      (+ 9 (max 1 composer-rows))
      7))

(defun composer-max-rows (height)
  "The composer growth cap (Zig composerMaxVisibleTextRows, pi-mono editor
parity): 30% of the window, at least 5 rows, never past HEIGHT - 10 — the
nine chrome rows SHELL-OVERHEAD stacks around the box plus the one
transcript row the band keeps at minimum (Zig's HEIGHT - 8 assumes its own
stack; here it let a 12-row window compose 14). The short shell mode stays
single-line."
  (if (shell-footer-p height)
      (max 1 (min (max 5 (floor (* height 3) 10)) (- height 10)))
      1))

;;; --- the transcript window --------------------------------------------------

(defstruct (band-window (:constructor make-band-window (&key (height 1))))
  "The transcript band's scroll model: LINES the rows RENDER-STATE has already
painted and wrapped at the band's text width, HEIGHT the rows the band shows,
TOP the requested first visible row."
  ;; Read the offset through BAND-WINDOW-OFFSET, which clamps TOP to the
  ;; content, so a window that shrinks or a transcript that is cleared never
  ;; scrolls past the end. Nothing here wraps or paints: TRANSCRIPT-BAND-ROWS
  ;; paints, SCROLLBAR-THUMB derives the gutter, and the mouse maps frame rows
  ;; through TRANSCRIPT-BAND-METRICS.
  ;;
  ;; The content is two lists and one count, not one list: LINES is the
  ;; projected transcript and TAIL the live chrome composed under it, with
  ;; COUNT their rows. A frame that moves only the chrome appends to the tail
  ;; and leaves the projection alone; the paint used to rebuild the whole
  ;; content list, three lanes at a time, for every chrome region on every
  ;; frame.
  (lines '() :type list)
  (count 0 :type fixnum)
  (height 1 :type fixnum)
  (top 0 :type fixnum)
  ;; Appended after TOP: a slot added anywhere but the end moves every
  ;; accessor below it, and this structure is live in a running image.
  (base-count 0 :type fixnum)
  (tail '() :type list))

(nlk:access (window band-window))

;;; Written by BAND-WINDOW-SET-LINES, read by the scroll range, dropped when
;;; the content grows back to it or the tail is pinned (PIN-TRANSCRIPT-BOTTOM).
(nlk:define-side-table band-window-hold (:weakness :key)
  "Weak memo: band window -> the offset it holds past the last its content
reaches: the place of a window read off the tail that the content shrank under.")

(defun band-window-max-offset (window &optional (hold (band-window-hold window)))
  "The last scroll offset, including the pinned prompt's separator budget and
the place HOLD keeps (BAND-WINDOW-SET-LINES)."
  ;; A truncated prompt needs an upward step even when its answer fits:
  ;; scrolling off bottom exposes the retained inline prompt below the pin.
  ;; The prompt budgeted is the one governing the tail's top row, never the
  ;; newest: lifting a new prompt or steer to the top would clear the screen
  ;; above it.
  (bind ((end (max 0 (- window.count window.height)))
         (span (find-if (lambda (span) (<= (car span) end)) (band-window-prompt-spans window) :from-end t))
         ((_ _ spacer omitted) (if span (band-window-prompt-header window span) (values 0 0 0 0))))
    (max 0 (+ (- window.count window.height) spacer)
         (+ (or (car span) 0) (if (plusp omitted) 1 0))
         (or hold 0))))

(defun band-window-offset (window)
  "The first source row; a negative TOP is canvas inset, not history."
  (max 0 (min window.top (band-window-max-offset window))))

(defun (setf band-window-offset) (offset window)
  (setf window.top
        (max 0 (min offset (band-window-max-offset window)))))

(defun band-window-set-lines (window lines &optional tail hold &aux (base (length lines))
                                                                    (held (band-window-hold window))
                                                                    (was (and (or hold held) (copy-band-window window))))
  "Replace the content with LINES followed by TAIL; an offset past the new
end clamps, and the clamp is written through so a later height change starts
from it. HOLD says the operator reads the window off the tail: when the content
shrinks under rows that stand where they stood, the window keeps its place,
the rows the content left standing empty (BAND-WINDOW-HOLD) until it grows
back into them, they move, or the tail is pinned."
  ;; The two stay apart rather than concatenated: LINES is the projected
  ;; transcript, the same list object for every animation frame, and the
  ;; chrome in TAIL is all a frame moves.
  (setf window.lines lines
        window.tail tail
        window.base-count base
        window.count (+ base (length tail)))
  ;; A running Standard turn shows its newest words alone, so the next
  ;; message takes the place of every row the last one had: without this,
  ;; the operator, scrolled up during a turn, would be clamped back onto
  ;; the new bottom. The offset counts from the top, so rows above the
  ;; shrink stand still on their own; only the clamp moves them. The place
  ;; is held while the rows the window shows from its top down to its
  ;; first with text are the same rows at the same index -- a window whose
  ;; own rows left lands on the new bottom as before, never on a blank
  ;; band. Once held, the place is a floor of the scroll range: the wheel
  ;; comes back down to it and locks the follow there, with no jump, until
  ;; the content reaches it again.
  (when was
    (let* ((top window.top)
           (end (band-window-max-offset window nil))
           (place (max (or held 0) (if hold top 0)))
           (lead (and (> place end) (< -1 top (band-window-count was))
                      (loop for line in (band-window-range was top (min (band-window-count was) (+ top window.height)))
                            collect line until (not (blank-band-row-p line))))))
      (if (and lead (not (blank-band-row-p (car (last lead))))
               (<= (+ top (length lead)) window.count)
               (every #'equal lead (band-window-range window top (+ top (length lead)))))
          (setf (band-window-hold window) place)
          (forget-band-window-hold window))))
  ;; A disclosure click can preserve leading canvas slack. Ordinary scroll
  ;; setters clear it; recomposing live chrome must not spend that anchor.
  (unless (minusp window.top)
    (setf (band-window-offset window) window.top))
  window)

(defun band-window-scroll (window delta)
  "Move the window DELTA rows, negative for up, clamped to the content."
  (setf (band-window-offset window) (+ (band-window-offset window) delta))
  window)

(defun band-window-goto-top (window)
  (setf (band-window-offset window) 0)
  window)

(defun band-window-goto-bottom (window)
  (setf (band-window-offset window) (band-window-max-offset window))
  window)

(defun band-window-at-bottom-p (window)
  "Whether the window shows the last row."
  (>= (band-window-offset window) (band-window-max-offset window)))

;;; --- the pinned prompt band --------------------------------------------------
;;; The governing operator prompt stays at the top at every scroll offset,
;;; at rest, while streaming and without overflow. The response scrolls below
;;; it. BAND-WINDOW-PIECES is the single authority for paint and pointer/copy
;;; mapping; a short pinned transcript leaves its unused canvas below it.
(defparameter +max-pinned-prompt-rows+ 4
  "Rows of a user prompt block the pinned band paints while the rows under it
fill the band. A truncated block keeps its opening pad, first body line,
omission marker and final wrapped body line; rows the band has spare go to
more of its head.")

;;; Written by COMPOSE-TRANSCRIPT-VIEWPORT, the one place holding both the
;;; painted rows and the refs lane that names each line's owner, and read at
;;; slice time by BAND-WINDOW-PINNED: whether a prompt pins is a function of
;;; the scroll offset, so it cannot be baked into the projection itself.
(nlk:define-side-table band-prompt-spans (:weakness :key)
  "Weak memo: a projection's row list -> the content-index spans of its
user prompt blocks, in order.")

(nlk:define-side-table band-disclosure-spans (:weakness :key)
  "Live-only projection rows -> expanded process spans (START END TITLE-ROWS).")

(defun refs-disclosure-spans (refs key &optional (width 80)
                                     &aux (spans '()) (owner nil)
                                     (start nil) (title-rows 0) (index 0))
  "Open output spans (START END TITLE-ROWS), read from projected owners."
  ;; A process owns its nested file/member lines. Otherwise an open tool or
  ;; receipt pins its first logical title, including wrapping and opening pad.
  ;; No parsing painted words, and no invented header for titleless prose.
  (flet ((seal ()
           (when (and start (plusp title-rows))
             (push (list start index title-rows) spans))
           (setf owner nil start nil title-rows 0)))
    (dolist (ref refs)
      (let* ((group (cond ((tool-group-p ref) ref)
                          ((transcript-row-p ref) (row-groups ref))))
             (group (if (and group (eq :mutation (first group.key))) (row-groups group) group))
             ;; A run shut over a call open under its title owns it the same way.
             (group (and group (eq :process (first group.key)) (tool-group-open-p group (open-key key)) group))
             (row (and (not group) (transcript-row-p ref)
                       (or (and (eq :tool-call ref.kind) ref.tool-name
                                (equal key (tool-row-expand-key ref)))
                           (and (row-notice-summary ref) (eq key ref)))
                       (first (row-llines-cache ref)) ref))
             (exec (and (bg-band-row-p ref) (equal key (band-row-expand-key ref)) ref))
             (next (or group row exec)))
        (unless (eq next owner)
          (seal)
          (when next
            (setf owner next start index
                  title-rows (if row
                                 (+ (if (row-block-padding-p row) 1 0)
                                    (length (lline-render (first (row-llines-cache row)) width)))
                                 (if exec 1 0)))))
        (when (and group (eq ref group)) (incf title-rows)))
      (incf index))
    (seal))
  (nreverse spans))

(defun transcript-user-prompt-row-p (row)
  "Whether ROW is a prompt submitted here: the block the pinned band stands
for. A recorded pair's input (NLK:RECORD-EXCHANGE-TURN) is not one."
  (and (transcript-row-p row) (eq :text row.kind) (equal "user" row.role)
       (not row.recorded)))

(defun refs-prompt-spans (refs &aux (owner nil)
                                    (start 0))
  "REFS — a projection's per-line owners — as the content-index spans of its
user prompt blocks: one (START . END) cons per run of consecutive lines owned
by one prompt row."
  ;; A block owns its padding rows and the blank spacer TRANSCRIPT-LINE-TABLE
  ;; leaves between two filled blocks belongs to nobody, so a span is exactly
  ;; the rows the prompt's block paints.
  (nconc (loop for ref in refs
               for index from 0
               for prompt = (and (transcript-user-prompt-row-p ref) ref)
               unless (eq prompt owner)
                 append (prog1 (and owner (list (cons start index)))
                          (setf owner prompt start index)))
         (and owner (list (cons start (length refs))))))

(defun band-window-prompt-spans (window)
  "The prompt spans of the projection WINDOW shows, or NIL — the home screen's
rows are no projection, and a row list with no entry here pins none."
  (band-prompt-spans window.lines))

(defun band-window-after-prompt (window span &aux (end (cdr span)))
  "The first content row past SPAN's prompt block: past the blank row after
it — the unowned separator or the next block's canvas pad — which a pin's own
canvas gap stands for."
  (let ((next (and (< end window.count)
                   (first (band-window-range window end (1+ end))))))
    (+ end (if (and next (zerop (length (string-trim " " (plain-frame-row next))))) 1 0))))

(defun band-window-prompt-header (window span)
  "The prompt header's START, pinned ROWS, SPACER and omitted body rows."
  ;; Both ends own padding, not prompt text. If body text would be cut,
  ;; reserve the last two pinned rows for the marker and final wrapped body
  ;; line. Count only hidden body rows, once here for every consumer.
  (let* ((height (band-window-height window))
         (start (+ (car span) (if (= height 1) 1 0)))
         (full (- (cdr span) start))
         ;; The rows under the prompt, read only when they could leave the
         ;; band room: the scroll range asks on every offset read, and the
         ;; separator lookup walks the row list.
         (below (when (<= (- window.count (cdr span)) height)
                  (- window.count (band-window-after-prompt window span))))
         ;; The cap is for a band the rows under the prompt fill. Rows they
         ;; leave go to the prompt: whole when it fits over them with its
         ;; gap, else as many as fit over them with the closing pad and gap.
         ;; A capped pin over a short answer left the band half empty with
         ;; `[+4 lines omitted]' at its top.
         (rows (min full height
                    (cond ((null below) +max-pinned-prompt-rows+)
                          ((<= (+ full 1 below) height) full)
                          (t (max +max-pinned-prompt-rows+ (- height 2 below))))))
         ;; A truncated pin keeps its filled closing pad and a canvas gap.
         (spacer (if (plusp rows)
                     (min (- height rows) (if (< (+ start rows) (cdr span)) 2 1)) 0))
         ;; A band the pin would fill lets it go while live chrome -- the
         ;; bar -- ends the content: the work going on outranks the question.
         (rows (if (and window.tail (>= (+ rows spacer) height)) 0 rows))
         (omitted (if (and (plusp rows) (> (1- (cdr span)) (+ start rows)))
                      (- (- (cdr span) (car span) 2)
                         (+ (max 0 (- rows 2 (if (= start (car span)) 1 0)))
                            (if (>= rows 2) 1 0))) 0)))
    ;; This one budget feeds the scroll range, painted pieces and pointer lane.
    (values start rows (if (plusp rows) spacer 0) omitted)))

(defun blank-band-row-p (row)
  "Whether the painted band ROW shows no character, whatever it fills."
  (zerop (length (string-trim " " (plain-frame-row row)))))

(defun band-window-answer-passed-p (window from to start rows)
  "Whether the answer in content rows [FROM, TO) has text, all of it above
the ROWS shown from START, and the next prompt, at TO, shows: an answer
scrolled wholly under its pinned prompt."
  (and (< from (min to start))
       (< to (+ start rows))
       (or (<= to start) (every #'blank-band-row-p (band-window-range window start to)))
       (notevery #'blank-band-row-p (band-window-range window from (min to start)))))

(defun band-window-pinned (window &aux (height window.height)
                                       (top (band-window-offset window)))
  "The pinned prompt's START, ROWS, CONTENT-START and CONTENT-ROWS,
  or NIL when its original block is scrolling inline."
  ;; A prompt has one representation. Upward intent within a truncated
  ;; prompt exposes its original block instead of retaining its summary.
  ;; At bottom, or past the block, the pin excludes the entire inline owner.
  ;; A prompt whose answer has all gone under it lets go. Without this,
  ;; a prompt pinned over the answer's closing blank rows and the next
  ;; prompt would read as a question never answered.
  (when (plusp height)
    (let* ((spans (band-window-prompt-spans window))
           (bottom (band-window-at-bottom-p window))
           (span (find-if (lambda (candidate) (<= (car candidate) top)) spans :from-end t)))
      (when span
        (multiple-value-bind (start pinned spacer omitted) (band-window-prompt-header window span)
          (when (and (plusp pinned)
                     (not (and (plusp omitted) (not bottom) (< top (cdr span)))))
            (let* (;; The pin already budgets its canvas row, so content
                   ;; starts past the row table's own.
                   (after-prompt (band-window-after-prompt window span))
                   (content-start (min window.count (max after-prompt (+ top pinned))))
                   (content-rows (min (- height pinned spacer) (- window.count content-start)))
                   (answer-end (car (second (member span spans)))))
              (unless (and answer-end
                           (band-window-answer-passed-p window after-prompt answer-end content-start content-rows))
                (values start pinned content-start content-rows)))))))))

(defun band-window-top-padding (window)
  "Leading canvas slack: a click anchor, else promptless short-band alignment."
  ;; Negative TOP encodes an explicit inset as -1 - rows, including zero.
  ;; A collapse at a pinned title can therefore stay top-aligned too.
  (min window.height
       (cond ((minusp window.top) (- -1 window.top))
             ((band-window-prompt-spans window) 0)
             (t (- window.height (min window.count window.height))))))

(defun band-window-range (window start end &aux (base window.base-count)
                                                (head (min end base))
                                                (tail-start (max 0 (- start base))))
  "Content rows [START, END) of WINDOW, across the projection/chrome join: the
tail rows count from BASE-COUNT."
  ;; A range lies in the projection, in the chrome, or across the join — a
  ;; band whose content is all chrome (a shell with no transcript yet) asks
  ;; for the last two.
  (cond ((<= end base) (subseq window.lines start head))
        ((>= start base) (subseq window.tail tail-start (- end base)))
        (t (append (subseq window.lines start head) (subseq window.tail tail-start (- end base))))))

(defun band-window-pieces (window)
  "WINDOW's paint and pointer/copy runs, with prompt and disclosure headers."
  ;; Synthetic omission/spacer rows own no source index. The omission lives
  ;; in this shared piece lane, never in the durable message or projection.
  (bind (((start pinned content-start) (band-window-pinned window))
         (prompt (when pinned
                   (find-if (lambda (span) (<= (car span) start (1- (cdr span))))
                            (band-window-prompt-spans window))))
         ((_ _ spacer omitted)
          (if prompt (band-window-prompt-header window prompt) (values 0 0 0 0)))
         (head (if (plusp omitted) (max 0 (- pinned 2)) (or pinned 0)))
         (prefix (when pinned
                   (append (when (plusp head) (list (list start head)))
                           (when (plusp omitted)
                             (append (list (list :omitted omitted (car prompt)))
                                     (when (>= pinned 2)
                                       (list (list (- (cdr prompt) 2) 1)))))
                           (when (> spacer 1)
                             (list (list (1- (cdr prompt)) 1)))
                           (when (plusp spacer) (list :spacer)))))
         (top (if pinned content-start (band-window-offset window)))
         (room (- window.height (band-window-top-padding window)
                  (if pinned (+ pinned spacer) 0)))
         (span (find-if (lambda (span)
                          (and (<= (+ (first span) (third span)) top) (< top (second span))))
                        (band-disclosure-spans window.lines)))
         (title-rows (if span (min room (third span)) 0)))
    (when (plusp title-rows)
      (setf prefix (append prefix (list (list (first span) title-rows)))
            top (min window.count (+ top title-rows)) room (- room title-rows)))
    (append prefix
            (when (and (plusp room) (< top window.count))
              (list (list top (min room (- window.count top))))))))

(defun band-window-visible (window &optional (width 80) hover-index)
  "Visible rows at WIDTH; omitted prompt lines retain the prompt's fill."
  (loop for piece in (band-window-pieces window)
        if (eq piece :spacer) collect ""
        else if (eq (first piece) :omitted)
          append (let* ((hover (eql hover-index (third piece)))
                        (line (make-lline :tone (if hover :user-block-hover :muted) :dim (not hover)
                                          :fill (if hover :user-block-hover :user-block))))
                   (setf (lline-text line)
                         (clip-text-to-width (format nil "[+~d lines omitted]" (second piece))
                                             (nth-value 0 (lline-widths line width))))
                   (nth-value 0 (lline-render line width)))
        else append (destructuring-bind (start count) piece
                      (band-window-range window start (+ start count)))))

(defun band-window-row-index (window row &optional owner-p)
  "Source index at visible ROW; OWNER-P gives synthetic rows' hover owner."
  ;; Never map the omission marker to hidden text, links or image placement.
  ;; Copy still reads the frame's actual painted plain lane.
  (when (minusp row) (return-from band-window-row-index nil))
  (dolist (piece (band-window-pieces window))
    (if (or (eq piece :spacer) (eq (first piece) :omitted))
        (if (zerop row)
            (return-from band-window-row-index
              (and owner-p (not (eq piece :spacer)) (third piece)))
            (decf row))
        (destructuring-bind (start count) piece
          (if (< row count) (return-from band-window-row-index (+ start row)) (incf row (- count)))))))

(defun composer-status-lines (model width &aux (status (active-status model))
                                               (empty (zerop (length (composer-draft model)))))
  "=> (values LINES UNDER STATUS): the composer status's lines as the box
shows them at WIDTH text columns, and whether they stand under a draft --
every status over an empty composer, under a draft only a held one, since
news hides under what is typed while a failure or an answer stays in sight
-- or NIL with none to show."
  (when (and status (or empty (composer-status-held status)))
    (values (status-text-lines (composer-status-text status) width) (not empty) status)))

(defun composer-visible-rows (model &aux (height (chat-model-term-height model)))
  "Text rows the composer box currently shows: its text's, and the composer
status's under it or in its place (COMPOSER-STATUS-LINES), to the growth cap."
  (if (shell-footer-p height)
      (let ((text (max 1 (editor-height (chat-model-editor model))))
            (cap (composer-max-rows height)))
        (multiple-value-bind (lines under)
            (composer-status-lines model (composer-text-width (chat-model-term-width model)))
          (cond ((null lines) text)
                (under (+ text (min (length lines) (max 1 (- cap text)))))
                (t (max 1 (min cap (length lines)))))))
      1))

(nlk:define-side-table status-blocks (:weakness :key)
  "Model -> (painted plain refs), the live block's three lanes at the band's tail.")

(defun sync-viewport-geometry (model &aux (height (chat-model-term-height model))
                                          (rows (composer-visible-rows model)))
  "Fit the transcript viewport under the composer's current height and under
an open dialog's docked panel."
  ;; The composer grows and shrinks as the operator edits and the panel docks
  ;; over the composer; the transcript band yields the rows (the Zig ChatShell
  ;; reflow plus reserveTranscriptAboveDialog).
  (setf (band-window-height (chat-model-band model))
        (max 1 (- height (shell-overhead height rows)
                  (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
                    (if (null panel)
                        0
                        (let ((height (max 4 height)))
                          (max 0 (- (max 1 (- height (shell-overhead height rows)))
                                    (nth-value 2 (docked-dialog-above-bands above)))))))
                  ;; The dropdown panel's rows below the composer (the
                  ;; Zig composer lift: the band yields them so the
                  ;; frame keeps its height).
                  (or (composer-dropdown-geometry model) 0)))))

(defun initialize-chat-model-ui (model)
  "Initialize terminal widgets without choosing the model's state owner."
  ;; Read real terminal dimensions so the alt-screen layout fills the window.
  (multiple-value-bind (term-w term-h) (nlt:terminal-size)
    (setf (chat-model-term-width model) term-w)
    (setf (chat-model-term-height model) term-h)
    ;; The composer is the Zig box: text rows inside a filled rectangle,
    ;; no prompt glyph, the "Ready" placeholder (render_plan.zig
    ;; composer_ready_placeholder). The box grows with its content to the
    ;; pi-mono cap and then scrolls internally. The editor owns editing and
    ;; scrolling; COMPOSER-ROWS owns the paint; no painted caret — the
    ;; shell parks the terminal's hardware cursor at the caret cell (the
    ;; Zig composer cursor contract, which COMPOSE-FRAME keeps).
    ;;
    ;; The editor wraps at the composer's own width, unlike the transcript
    ;; viewport below: it holds live, unstyled text whose caret must map to
    ;; a visual cell, so the engine owns the break — through
    ;; BREAK-FRAGMENTS, the one line-break authority — word-aware like the
    ;; transcript's, over the raw text the caret maps into.
    (setf (chat-model-editor model)
          (make-editor :width (composer-text-width term-w)
                       :max-height (composer-max-rows term-h)
                       :placeholder "Ready"
                       :char-limit 100000))
    ;; The band window holds rows the display policy has already wrapped
    ;; through BREAK-FRAGMENTS, the one line-break authority, at the
    ;; transcript text width: inset 2 from the left, 2 kept at the right.
    (setf (chat-model-band model)
          (make-band-window :height (max 1 (- term-h (shell-overhead term-h))))))
  (render-state model))

;;; --- frame clock ---------------------------------------------------------
;;; The one animation pacing authority (the Zig frame loop, which renders
;;; every animating frame regardless of stream traffic). Pacing must NOT
;;; ride the program's worker: it is one thread (the single-writer
;;; discipline) and a local turn occupies it for its whole duration, so a
;;; tick queued as a command would wait behind the turn and the spinner
;;; would freeze until the turn ends. This thread sends tick messages
;;; straight into the program's mailbox — the same NLT:SEND wake path the
;;; stream delta-fn uses — so frames keep flowing while the worker is busy.

;;; The terminal core owns the one frame period. The animation clock and the
;;; physical event-loop paint share it so animation boundaries do not outrun
;;; the renderer or create a second pacing authority.


(defun live-chrome-animating-p (model &key (dialog-ticks t))
  "Whether chrome needs frames between input and stream events: a running
turn (spinner, elapsed clock, waiting sway — the Zig
transcriptStatusAnimating predicate), live background rows, or a transient
hint still on screen (its five-second decay only lands if something
repaints after it expires)."
  ;; With DIALOG-TICKS, an open dialog carrying a tick handler counts too:
  ;; that is the frame clock's question (keep ticking, the dialog's clocks
  ;; pace themselves). The tick's own render decision asks without it, because
  ;; the handler answers for the dialog whether the tick moved anything, and a
  ;; status frame per tick over a still dialog was the shell's whole idle
  ;; cost.
  (or (chat-model-turn-in-flight model)
      (state-background-rows (chat-model-state model))
      ;; news in the composer status goes after five seconds, and a docked
      ;; panel's row gives a held one back to its keys after as many
      (let ((status (chat-model-composer-status model)))
        (and status (or (not (composer-status-held status))
                        (< (since-ms (composer-status-born status)) 5500))))
      ;; A dialog whose rows carry clocks asks for ticks by having a
      ;; handler for them.
      (and dialog-ticks (picker-dialog-p (chat-model-dialog model))
           (picker-dialog-on-tick (chat-model-dialog model)) t)
      ;; The turn the bar shows in flight, a previewed one's included: its
      ;; mark and clock move.
      (and (nth-value 1 (bar-lane model)) t)))

(nlt:defmessage command-catalog-msg
    ((commands :initarg :commands :reader command-catalog-msg-commands))
  :documentation "The cell commands the gateway serves, read off the loop
thread: INIT queues the read as a command so the first frame never waits on
it — it is an HTTP round trip — and UPDATE installs what came back.")

(defun read-command-catalog-cmd (model)
  "A command that reads the cell commands MODEL's gateway serves and answers
COMMAND-CATALOG-MSG, or nothing when the read fails: the commands in hand
stay (builtins keep working, and the next connection or COMMANDS_CHANGED
reads again)."
  (lambda ()
    (with-handlers ((error () nil))
      (make-instance 'command-catalog-msg :commands (gateway-command-entries model)))))

(defmethod nlt:update ((model chat-model) (msg command-catalog-msg))
  (set-gateway-commands (command-catalog-msg-commands msg))
  (values model nil))

;;; --- the composer's @ catalog -----------------------------------------------
;;; A filesystem walk over up to 512 workspace paths costs more than a frame.
;;; Without the cache, the first `@' would paint its rows late while later
;;; presses land inside the frame. The walk is taken off the trigger's
;;; keystroke: it runs on a thread of its own. INIT starts it with the first
;;; frame, a trigger that finds the catalog unloaded asks for it, and what it
;;; answers waits in *COMPOSER-CATALOG-RESULTS* for the loop's tick branch, so
;;; the loop stays the model's one writer and the tick is what paints the rows.
;;; The answer is kept in *COMPOSER-CATALOG-CACHE-PATH*, read back on the boot
;;; thread before the first frame. A shell's boot is loaded work (a fresh
;;; `--ephemeral' launch boots an organism, its cells and its channels beside
;;; the shell), so the walk is not reliably done before the operator's first
;;; `@'. The cache is what makes that press the same frame as the second one;
;;; the walk that follows replaces it within the second.

(define-startup-parameter *composer-catalog-cache-path*
  (home "composer-catalogs.sexp")
  "Where the composer catalogs the last walk answered are kept, so a process
paints its first `@' from what the last one learned instead of paying the walk
under the boot's own load. NIL disables the cache; tests bind.")

(defparameter *composer-catalog-cache-workspaces* 4
  "Workspaces the catalog cache keeps: the current one first, then the most
recent others. The catalogs depend on the directory the shell runs in.")

(defun composer-catalog-cache-entries (path)
  "PATH's entries as an alist (WORKSPACE-STRING . RESULT-PLIST), newest first,
or NIL."
  ;; Anything unreadable — another shape, a half write, no file — reads as no
  ;; cache at all, and the walk fills the lanes as it always did. The read is
  ;; UIOP's safe one (standard syntax, *READ-EVAL* off), in CL-USER, not
  ;; KEYWORD: the file carries T and NIL too, and the keyword package reads
  ;; those as :T and :NIL.
  (when path
    (let ((entries (ignore-errors (uiop:safe-read-file-form path :package :cl-user))))
      (and (listp entries) entries))))

(defun read-composer-catalog-cache (workspace)
  "The walk's result for WORKSPACE as the plist it answers with, or NIL."
  (cdr (assoc (namestring workspace)
              (composer-catalog-cache-entries *composer-catalog-cache-path*)
              :test #'equal)))

(defun write-composer-catalog-cache (workspace result)
  "Remember RESULT — a walk's own plist — for WORKSPACE, newest first and
bounded."
  ;; Written beside itself and renamed over, so a reader never sees a half
  ;; file. Answers the path written, or NIL when the cache is off or the write
  ;; failed: a cache that cannot be written is not a walk that cannot finish.
  (when-let (path *composer-catalog-cache-path*)
    (ignore-errors
     (let* ((key (namestring workspace))
            (kept (remove key (composer-catalog-cache-entries path)
                          :key #'car :test #'equal))
            ;; The failures are a live diagnostic, not an answer.
            (entries (acons key (alexandria:remove-from-plist result :failures) kept)))
       ;; Readably, an SBCL base-string prints as #A((29) BASE-CHAR . "...")
       ;; — twice the bytes and a syntax only this implementation reads.
       (let ((*print-length* nil) (*print-level* nil)
             (*print-circle* nil) (*print-pretty* nil)
             (*print-readably* nil))
         (nlk:write-file-atomically
          path (prin1-to-string (subseq entries 0 (min (length entries)
                                                        *composer-catalog-cache-workspaces*)))))))))

(defun install-composer-catalogs (model result)
  "Install a walk's RESULT — its plist — into MODEL: an answer takes its list
and its LOADED flag (an empty answer is an answer), a failed walk keeps the
flag NIL so the next trigger retries it."
  ;; Answers whether anything moved.
  (when (getf result :files-loaded)
    (setf (chat-model-file-catalog model) (getf result :files)
          (chat-model-file-catalog-loaded model) t)))

;;; A trigger may ask on any keystroke; the walk still runs one at a time per
;;; model.
(defvar *composer-catalog-walks* (make-hash-table :test #'eq)
  "The models whose composer-catalog walk is in flight.")

(defvar *composer-catalog-results* (make-hash-table :test #'eq)
  "The walks that finished and have not landed yet: MODEL -> a plist of
:FILES :FILES-LOADED :FAILURES, written by the walk
thread and drained by the loop's tick branch.")

(defvar *composer-catalog-walks-lock*
  (bt2:make-lock :name "composer-catalog-walks")
  "One lock for both tables: they are read and written together.")

(defun read-composer-catalogs (workspace)
  "WORKSPACE's composer catalog, as the plist the tables carry."
  ;; A walk that signals is carried as text, never swallowed: the old
  ;; (ignore-errors ...) left the lane silently empty for the rest of the
  ;; process, which is what a dropdown nothing can explain looks like.
  (handler-case (list :files (workspace-file-catalog workspace) :files-loaded t)
    (error (e)
      (list :files-loaded nil
            :failures (list (format nil "file catalog unavailable — ~a" e))))))

;;; With the cached catalogs already in the model, nothing the operator sees
;;; waits on the walk, and its allocation no longer competes with the
;;; keystrokes that follow the frame (a first press measured 17-60 ms
;;; on this machine, the walk running beside it). A trigger's request is not
;;; delayed: it is asked for because nothing is in hand.
(defparameter *composer-catalog-walk-delay* 1/2
  "Seconds a boot's own refresh walk waits past the frame its attach is for
before it starts — the warm-spawn rule, one process over: the folder's builder
lands behind the first paint, never inside it.")

(defun start-composer-catalog-walk (model &key delay &aux (program (chat-model-program model)))
  "Walk MODEL's @ catalog on a thread of its own if none is already in flight,
and tick the loop when it lands — DELAY seconds from now, so a boot's refresh
lands behind the first frame."
  ;; NIL without a program: no loop means no frame to paint. The keystroke
  ;; path only ever asks here — it never waits on the filesystem.
  (when (and program
             (bt2:with-lock-held (*composer-catalog-walks-lock*)
               (unless (gethash model *composer-catalog-walks*)
                 (setf (gethash model *composer-catalog-walks*) t))))
    (let ((workspace (uiop:getcwd)))
      (spawn "nodecode-composer-catalogs"
        (when (and delay (plusp delay)) (sleep delay))
        (with-cleanup ((bt2:with-lock-held (*composer-catalog-walks-lock*)
                         (remhash model *composer-catalog-walks*)))
          (let ((result (read-composer-catalogs workspace)))
            ;; The cache is what the next process paints its first `@'
            ;; from; written here, on the walk's own thread.
            (write-composer-catalog-cache workspace result)
            (bt2:with-lock-held (*composer-catalog-walks-lock*)
              (setf (gethash model *composer-catalog-results*) result))
            (nlt:send program (nlt:make-tick-msg))))))
    t))

(defun refresh-composer-catalogs-behind-the-frame (model)
  "Start the catalog refresh MODEL's boot put off, now that the frame its
attach was for is on screen."
  ;; A fixed half-second delay would still put the catalog walk, with its
  ;; SKILL.md reads and workspace listing, inside a resumed session's replay.
  (when (shiftf (chat-model-catalog-refresh-due model) nil)
    (start-composer-catalog-walk model :delay *composer-catalog-walk-delay*)))

(defun land-composer-catalog-walks (model)
  "Install the composer catalogs a walk landed since the last frame, on the
loop thread, and put each failure up as an error toast (INSTALL-COMPOSER-
CATALOGS owns what an answered lane and a failed one each take)."
  (when-let (result (bt2:with-lock-held (*composer-catalog-walks-lock*)
                      (prog1 (gethash model *composer-catalog-results*)
                        (remhash model *composer-catalog-results*))))
    (install-composer-catalogs model result)
    (dolist (failure (getf result :failures))
      (toast model failure :error-p t))
    t))

;;; A streaming turn's transcript is projected on this tick, so this is how
;;; often new text reaches the screen. 1/60 is three ticks per 50 ms, so the
;;; spinner's 150 ms step and the elapsed clock's 100 ms digit flip on a tick.
;;; Frames the operator asks for -- the pointer, a key, the wheel -- paint at
;;; the loop's own ceiling (NLT:*FRAME-PERIOD-SECONDS*), which is faster.
(defparameter *frame-clock-period-seconds* 1/60
  "How often the frame clock posts ticks; the loop suppresses idle paint.")

(defun run-frame-clock (program)
  "Post paced ticks until PROGRAM stops; the loop owns every UI read."
  ;; A live source fold can temporarily replace a CLOS accessor's methods.
  ;; Reading the model here killed the clock during that window; streaming
  ;; then only painted when input arrived. The clock knows only its program
  ;; and time. UPDATE owns animation, dirty projection, expiry and QUIET, so
  ;; idle ticks still owe no compose or physical paint.
  (loop while (nlt:program-running program)
        do (let* ((period (max 1 (round (* *frame-clock-period-seconds*
                                           internal-time-units-per-second))))
                  (now (get-internal-real-time)))
             ;; Stay on the monotonic grid without ratios or sleep drift.
             (sleep (/ (- period (mod now period)) (float internal-time-units-per-second 1d0)))
             (nlt:send program (nlt:make-tick-msg)))))

(defmethod nlt:init ((model chat-model))
  "Initialize local widgets over the empty state the gateway's replay fills."
  ;; Commands
  ;; ride along, for the worker: the kitty graphics query — a terminal that
  ;; answers flips the model into inline-image mode (KITTY-GRAPHICS-MSG below);
  ;; silence keeps the fallback — the truecolor probe when the environment
  ;; guessed a quantized profile — a terminal that confirms upgrades the paint
  ;; (TRUECOLOR-MSG below); silence keeps the guess — and the read of the
  ;; gateway's cell commands, so the dropdown, ghost text and /help know them
  ;; without the first frame waiting on the read.
  ;; The frame clock starts here — INIT runs on the live program's update loop,
  ;; after NLT:RUN has marked the program running.
  (initialize-chat-model-ui model)
  (setf (chat-model-state model) (initial-chat-state))
  (render-state model)
  ;; What the last walk answered for this workspace paints the first `@': read
  ;; here, on the boot thread, so no frame can race it. A model with no program
  ;; takes it too — the rows are in hand wherever a frame paints — and the walk
  ;; that refreshes them starts with the frame, below.
  (install-composer-catalogs model
                             (read-composer-catalog-cache (uiop:getcwd)))
  ;; The frame clock; without a program handle (tests drive INIT) no thread starts.
  (when-let (program (chat-model-program model))
    ;; The launch glint's clock starts with the frame clock that carries it.
    (play-mark-glow model :shine :launch t)
    (spawn "nodecode-frame-clock" (run-frame-clock program))
    ;; With the catalog in hand the refresh waits for the frame the attach
    ;; is for; without it, the walk is all that stands between the operator
    ;; and an empty dropdown, so it starts now.
    (if (chat-model-file-catalog-loaded model)
        (setf (chat-model-catalog-refresh-due model) t)
        (start-composer-catalog-walk model))
    ;; The /models catalog — a 4.7 MB models.dev parse folded with every
    ;; fetched listing — is built on a thread of its own, so the first press
    ;; of /models paints from the memo instead of paying the build. The
    ;; answer is a memo, not UI state: nothing lands it on a frame.
    (start-models-picker-warm model))
  (append (list (nlt:terminal-io-cmd #'nlt:request-kitty-graphics)
                (nlt:terminal-io-cmd #'nlt:request-background))
          (when (color-profile-upgradable-p)
            (list (nlt:terminal-io-cmd #'nlt:request-truecolor)))
          (list (read-command-catalog-cmd model))))

;;; --- rendering ----------------------------------------------------------

(defun note-turn-activity (model verb kind &optional (now (get-internal-real-time)))
  "Stamp VERB, of KIND, as what MODEL's running turn is on — the live bar's
verb and its mark read the stamp, (VERB KIND NOW)."
  ;; A round's wait for its first token is the durable turn.provider_request
  ;; fact's (ttft-ms), read by the usage reports; no bar shows it.
  (setf (chat-model-turn-activity model) (list verb kind now)))

(defun toggle-tool-detail (model &optional (next (if (eq :developer (chat-model-tool-detail model))
                                                     :standard :developer)))
  "Set MODEL's viewport mode, by default the other of Developer and Standard."
  (check-type next (member :developer :standard))
  (setf (chat-model-tool-detail model) next)
  (show-status model (format nil "Switched to ~:(~a~) viewport" next))
  next)

(defun tool-detail-hint (model)
  "The live bar's viewport control — `ctrl+o viewport' — under the
chord MODEL's keymap binds, or NIL when it is unbound."
  (when-let (chord (first (gethash :toggle-viewport (chat-model-keymap model))))
    (format nil "~(~a~) viewport" (key-chord-label chord))))

(defun slash-details-op (model args)
  "The /details op: set Developer/Standard viewport mode, or report it."
  (let ((arg (string-downcase (string-trim '(#\Space #\Tab) (or args "")))))
    (cond ((zerop (length arg))
           (format nil "viewport ~:(~a~)" (chat-model-tool-detail model)))
          ((member arg '("developer" "standard") :test #'string=)
           (toggle-tool-detail model (if (equal arg "developer") :developer :standard))
           nil)
          (t (slash-usage "details")))))

(defun usage-meter-seg (state &aux (usage (and (hash-table-p state) (gethash "usage" state)))
                                   (text (and usage (usage-meter-text usage))))
  "The token-meter segment for the turn in the bar — its rounds so far,
summed — or NIL when no tokens exist."
  ;; The running bar's live meter (the Zig appendTranscriptStatusTokenMeter
  ;; shape), and the only lane a turn's own counts sit in: the resting divider
  ;; names the lane, the outcome and the clock instead of repeating them, and
  ;; the header's right lane carries the session's total, with room to spell
  ;; its long-form notes.
  ;;
  ;; The text is USAGE-METER-TEXT, the one spelling of a turn's counts, so the
  ;; bar and /context can never disagree about the numbers.
  (when text
    (make-seg :text text :tone :muted :dim t)))

(defun live-usage-meter-segs (model)
  "Developer's live token meter, plus context-pressure warnings in either viewport."
  ;; The meter is the turn's own accumulator (its rounds so far, summed); the
  ;; pressure clause is the newest round's occupancy, because the window holds
  ;; one prompt at a time, not the sum of every prompt the loop resent. NIL
  ;; before any usage exists. Standard hides the numeric meter, not the
  ;; actionable pressure warning or the retained usage read by /context. The
  ;; usage is the shown turn's (BAR-LANE), a previewed one's under a dialog.
  (let* ((state (bar-lane model))
         (usage (and (hash-table-p state) (gethash "usage" state)))
         (context (and (hash-table-p state) (gethash "context_usage" state))))
    (when usage
      (let* ((meter (and (eq :developer (chat-model-tool-detail model))
                         (usage-meter-seg state)))
             ;; Context pressure earns a bar slot only once it predicts an
             ;; imminent eviction stall; danger escalates the tone.
             (used (context-occupancy-tokens context))
             (window (and (getf context :model)
                          (ignore-errors
                            (model-context-tokens (getf context :model)))))
             (percent (and window (plusp used)
                           (if (zerop window)
                               100
                               (min 999 (floor (* used 100) window))))))
        (append (and meter (list meter))
                (when (and percent (>= percent 75)) ; Zig ctx warn 75, danger 90
                  (let ((danger (>= percent 90)))
                    (list (make-seg :text " · ")
                          (make-seg :text (format nil "ctx ~d%" percent)
                                    :tone (if danger :danger :muted) :dim (not danger))))))))))

(defun model-names-provider-p (provider model-id &aux (end (length provider)))
  "Whether MODEL-ID starts with PROVIDER's name and a separator --
`deepseek-flash' on deepseek -- so a label that names both says it once."
  (and (plusp end) (< end (length model-id))
       (string-equal provider model-id :end2 end)
       (find (char model-id end) "-/._:")
       t))

(defun status-identity-label (provider model-id)
  "The status bar's provider/model label (Zig
appendTranscriptStatusPlanModelLabel): both absent contributes nothing
rather than the literal word \"unknown\"; a provider that is empty,
\"unknown\", already spelled inside a slashed model id, or leading the model
id (MODEL-NAMES-PROVIDER-P) collapses to the model alone; otherwise
`provider/model'."
  ;; The local is not named TRIMMED: NODECODE.KERNEL exports a TRIMMED of
  ;; its own, and an image whose package locks are on refuses that binding —
  ;; the shipped file compiles under the build's own policy, but a live
  ;; re-evaluation of this definition (an edit made live, a layer override)
  ;; compiled it with an error stored in the function, so every call raised
  ;; "Execution of a form compiled with errors" and the live turn bar lost
  ;; its model label. The name is the fix; the body is unchanged.
  (flet ((trim-field (value)
           (json-value (and (stringp value) (string-trim '(#\Space #\Tab) value))
                       :text)))
    (let ((provider (trim-field provider))
          (model-id (trim-field model-id)))
      (cond ((null model-id) provider)
            ((or (null provider)
                 (string-equal provider "unknown")
                 (find #\/ model-id)
                 (model-names-provider-p provider model-id))
             model-id)
            (t (concatenate 'string provider "/" model-id))))))

(defun status-identity-with-effort (provider model-id effort)
  "STATUS-IDENTITY-LABEL plus the ` (effort)` suffix the turn will bind —
the live/resting bar's identity, resolved through the same
FOOTER-REASONING-EFFORT seam the footer reads, so the two lanes can never
name different efforts for one target."
  (let ((label (status-identity-label provider model-id)))
    (and label (format nil "~a~@[ (~a)~]" label (json-value effort :text)))))

(defun bar-retry (model)
  "The retry MODEL's live bar counts down (RETRY-STATUS), or NIL: the shown
turn's (BAR-LANE), while it runs unstopped."
  (multiple-value-bind (state in-flight started activity own) (bar-lane model)
    (declare (ignore started activity))
    (and in-flight
         (not (and own (turn-stopping-p model)))
         (hash-table-p state)
         (gethash "retry_status" state))))

(defun retry-reason-lines (model width)
  "What the provider said of the retry the live bar counts, whole and with its
own line breaks, as status-block rows on the bar's words' column at WIDTH
=> (values PAINTED-LIST PLAIN-LIST), NIL when no retry stands or it said
nothing."
  ;; Wrapped, never cut: the bar's one row cut the advice off the 503 that
  ;; carried it (always output the error literally).
  (when-let (reason (when-let (retry (bar-retry model)) (nth-value 2 (retry-status-text retry))))
    (llines-rows (mapcar (lambda (line) (make-lline :text line :tone :foreground :role :prose))
                         (split-lines reason))
                 width)))

(defmacro seg (text &optional tone &rest options)
  "A MAKE-SEG of TEXT in TONE, OPTIONS its other keys: the positional spelling
for the many bars and rows that paint one run of text in one tone."
  `(make-seg :text ,text :tone ,tone ,@options))

(defun run-lift (model over &aux (block (find-if (lambda (row) (and (tool-group-p row) (live-run-rows row)))
                                                 (seventh (chat-model-projection-cache model))
                                                 :key #'display-block-row :from-end t)))
  "The canvas rows over the live bar that keep the running turn's run in
MODEL's projection, with the OVER rows between it and the bar, as tall as
they have stood this turn."
  ;; Without this, a round shorter than the one before would leave canvas
  ;; over the bar instead of pulling it up. Counted here, where the canvas
  ;; row over the bar is decided: a transcript ending on canvas takes that
  ;; row's place. A run clicked open, or a call under its shut title, is
  ;; read in full and counts nothing.
  (let ((group (and block (display-block-row block))))
    (if (or (null group) (tool-group-open-p group) (some #'run-member-open-p (tool-group-members group)))
        0
        (let* ((rows (+ over (length (display-block-lines block))))
               (most (max rows (live-run-rows group))))
          (setf (live-run-rows group) most)
          (- most rows)))))

(nlk:define-side-table live-marks (:weakness :key)
  "A model -> (TURN . PLIST): the most its live TURN's block has taken under
each of LIVE-MARK's keys, so the bar and what stands under it hold still.")

(defun live-mark (model turn key value &aux (marks (live-marks model)))
  "The most VALUE has been under KEY in MODEL's live TURN (BAR-LANE's
STARTED), this VALUE included: the rows under the bar, :UNDER."
  (unless (and marks (eql turn (car marks)))
    (setf marks (setf (live-marks model) (list turn))))
  (setf (getf (cdr marks) key) (max value (getf (cdr marks) key 0))))

(defun live-status-line (model width)
  "The live turn bar (Zig transcriptStatusSummaryForKind .started): spinner
mark, the verb -- a tool's own, `evaluating', its line saying what the call does --, the elapsed clock, how long a quiet stream has waited, the
live token meter, the context-pressure clause, then esc on a filled cap: the
key that stops the turn, on the line it stops. The turn is the shown
transcript's (BAR-LANE): a previewed session's under a dialog, painted as
attaching to it would paint it."
  ;; A turn out of flight falls back to the resting interrupted bar when the
  ;; fold left one. Live-only chrome — it never enters the row lanes. Rendered
  ;; through RENDER-ROW so a painting profile carries the canvas to WIDTH.
  (labels ((mark-and-verb (elapsed-ms activity activity-ms tool-activity-p)
             ;; The mark is the verb's own (*STATUS-MARKS*): a stream quiet
             ;; past the threshold — or one that never spoke — reads
             ;; "waiting" under the sway (the Zig
             ;; transcriptStatusActivityStale rule), and every verb the bar
             ;; names wears its kind's mark beside it. A tool in flight
             ;; names itself regardless of stream freshness — tools stream
             ;; nothing while they work, and that silence is exactly when
             ;; the bar should talk. The third value is how long a stream
             ;; that did speak has been quiet: without it, `waiting' alone
             ;; would come on two seconds into any pause, so it could not
             ;; tell a long think from a stall.
             (if (and activity
                      (or tool-activity-p
                          (<= (- elapsed-ms activity-ms) *status-waiting-after-ms*)))
                 (values (status-mark elapsed-ms (second activity)) (first activity) nil)
                 (values (status-mark elapsed-ms :wait) "waiting on the model"
                         (and activity (- elapsed-ms activity-ms)))))
           (quiet-segs (quiet-ms)
             (and quiet-ms
                  (list (make-seg :text (format nil " · quiet ~a" (format-turn-elapsed quiet-ms))
                                  :tone :muted))))
           (elapsed-segs (elapsed-ms)
             ;; After the verb: the bar's first word is what the turn is
             ;; doing, and `11:05' at its head read as a time of day.
             (and elapsed-ms
                  (list (make-seg :text (format nil " · ~a" (format-turn-elapsed elapsed-ms))
                                  :tone :muted))))
           (bar-segs (mark mark-tone head head-tone &rest tails)
             ;; The mark in the gutter where ▸ and ■ stand, set flush against
             ;; the one space before the label lane, since a spinner frame is
             ;; two cells; then in the lane HEAD, then each of TAILS' segs: a run title's shape, so the bar
             ;; reads as the next line of the trail. Its mark sat in the label
             ;; lane and its words on the content rail, nine cells apart.
             (apply #'append
                    (list (make-seg :text (make-string (max 0 (- (nth-value 2 (body-gutter :disclosure))
                                                                 1 (text-display-width mark)))
                                                       :initial-element #\Space))
                          (make-seg :text mark :tone mark-tone)
                          (make-seg :text " "))
                    (list (make-seg :text head :tone head-tone))
                    tails))
           (retry-segs (retry verb-seg elapsed-ms &rest tails)
             ;; A retry in the tool amber, mark and head, then who answered it,
             ;; then the way out; what the provider said stands whole on the
             ;; rows under the bar (RETRY-REASON-LINES). It wore the foreground
             ;; and the plain sway, as any wait does, and a 72-character cut
             ;; left `switch to altern...'.
             (multiple-value-bind (head source) (retry-status-text retry (get-internal-real-time))
               (let ((status (getf retry :status)))
                 (apply #'bar-segs (status-mark elapsed-ms :wait) :tool "" :foreground
                        (list (funcall verb-seg head :tool))
                        (and source (list (make-seg :text " · " :tone :muted)
                                          (make-seg :text source :tone :foreground)))
                        (append tails
                                (list (and status (or (= status 429) (>= status 500))
                                           (list (make-seg :text " · " :tone :muted)
                                                 (hover-seg model "/models to switch"
                                                            (cons :command "models"))))))))))
           (stop-key (segs)
             ;; Esc, the fixed stop key, as a filled cap three columns after
             ;; the bar's own words. Whole or not at all: a bar too narrow
             ;; for it keeps its own words.
             (let ((key (list (make-seg :text "   ")
                              (make-seg :text " esc " :tone :key-chip)
                              (make-seg :text " stop" :tone :muted))))
               (if (<= (+ (segs-display-width segs) (segs-display-width key)) width)
                   (append segs key)
                   segs)))
           (bar-row (segs)
             ;; Attach ranges to the final string, after clipping/narrowing.
             (let ((painted (clip-segs segs width :mark "…")))
               (link-plain-row (nlk:narrow-string (render-row nil painted (canvas-fill-bg) width))
                               (seg-links painted 0)))))
    ;; One bar, whichever session it speaks for: under a dialog previewing
    ;; another session, that session's turn as attaching to it would paint
    ;; it. Without this, the preview would paint a bar of its own, wearing
    ;; the session's name at its head and losing the retry, the meter, the
    ;; resting shape and the tool's silence; the two would drift.
    (multiple-value-bind (state in-flight started activity own) (bar-lane model)
      (if (not in-flight)
          (let ((resting (and (hash-table-p state)
                              (gethash "turn_resting_status" state))))
            (when resting
              (let* (;; The interrupted shape is the resting divider's own: the
                     ;; elapse, the outcome word and the wall clock (in its
                     ;; parentheses) the turn died at, with no meter to repeat
                     ;; what the header's right lane carries.
                     (head (turn-rest-text (or (getf resting :outcome) "failed")
                                           (getf resting :elapsed-ms) (getf resting :ended-at-ms))))
                (bar-row (bar-segs *status-mark-interrupted* :danger head :muted)))))
          (let ((elapsed-ms (if started (since-ms started) 0)))
            (multiple-value-bind (mark verb quiet)
                (mark-and-verb elapsed-ms activity
                               (if (and started activity)
                                   (since-ms started (third activity))
                                   elapsed-ms)
                               ;; A tool call still running as the newest row is the one
                               ;; silence the bar keeps its mark through: a tool streams
                               ;; nothing while it works.
                               (and activity
                                    (hash-table-p state)
                                    (when-let (row (last-tail-row state))
                                      (and (eq :tool-call row.kind)
                                           (equal "running" row.tool-status)))))
              (let* ((stopping (and own (turn-stopping-p model)))
                     (retry (bar-retry model))
                     (words (if stopping "stopping…" verb))
                     (verb-seg (lambda (text tone)
                                 (make-seg :text text :tone tone :link :status-hint
                                           :underline (eq :status-hint (chat-model-hover-target model)))))
                     (elapsed (elapsed-segs (and started elapsed-ms))))
                (bar-row
                 ;; Esc stops the turn only while no dialog holds the keys:
                 ;; under one it closes the dialog, and a previewed turn is
                 ;; never this shell's to stop from there.
                 (funcall (if (or stopping (chat-model-dialog model)) #'identity #'stop-key)
                          (if retry
                              (retry-segs retry verb-seg elapsed-ms elapsed (live-usage-meter-segs model))
                              (bar-segs mark :foreground "" :foreground
                                        (cons (funcall verb-seg words :foreground) elapsed)
                                        (quiet-segs (and (not stopping) quiet))
                                        (live-usage-meter-segs model))))))))))))

;;; --- the background-process band ---------------------------------------------
;;; The status-block band (src/tui/render_plan.zig collectBackgroundProcessLines
;;; + transcript.zig band rows + band_layout.zig): one line per running or
;;; preparing shell, newest first, budgeted by BACKGROUND-BAND-BUDGET, with the
;;; counted overflow row and the frozen-head/marker/live-tail output window.
;;; Geometry (band_layout.zig): lead = 4 spaces, the odometer mark at column 4
;;; (0-based), body text at column 6; the output hangs under the command,
;;; EXEC-OUTPUT-ROWS: the ⌎ hook and the omission's ⋮ on column 6, its lines
;;; two in. Overflow "… ".
;;; Live-only chrome — rows come from STATE-BACKGROUND-ROWS, which replay
;;; never seeds.

(defparameter +band-lead+ "    ")

(defun chrome-row (lead segs bg width)
  "One chrome row behind the LEAD indent => (values PAINTED PLAIN)."
  ;; The plain lane is derived from the very segs that were painted
  ;; (SEGS-TEXT) — clipped to WIDTH first, so both lanes see the same cut —
  ;; instead of being re-concatenated beside them, so it cannot become a
  ;; second authority on what the row says.
  (let ((segs (clip-segs (cons (make-seg :text lead) segs) width
                          :mark "…")))
    (values (render-row '() segs bg width)
            (segs-text segs))))

(defun background-band-mark (row now)
  "The mark a band row's lead carries at NOW (internal time, as every chrome
clock is)."
  ;; Zig's is an activity odometer — it steps when the process speaks — which
  ;; is the honest answer for a row that is still being described, and a
  ;; frozen dot for a command that has said nothing for ten minutes, which is
  ;; exactly the row an operator watches to know the work out of flight is
  ;; still moving. So a RUNNING row's mark is the clock's: the odometer's own
  ;; eight frames, stepped on the turn bar's grid. One column wide either way,
  ;; so the command beside it never shifts. A row that was already live when
  ;; the clock arrived has no stamp: it is born on the frame that first asks,
  ;; so the command an operator is watching starts moving rather than parking.
  (if (eq :running (bg-band-row-status row))
      (let ((born (or (bg-row-born row) (setf (bg-row-born row) now))))
        (aref +background-odometer-frames+
              (mod (floor (since-ms born now) *status-running-frame-ms*)
                   (length +background-odometer-frames+))))
      (background-odometer-mark (bg-band-row-activity-events row))))
(defun band-row-tail-segs (row stop-hovered &optional width)
  "A pending eval's live-only elapsed clock and its preparing/stop state."
  (if (eq :preparing (bg-band-row-status row))
      (list (make-seg :text "  — preparing" :tone :muted :dim t))
      (let* ((now (get-internal-real-time))
             (born (or (bg-row-born row) (setf (bg-row-born row) now))))
        (cons (seg (format nil "~a~a"
                           (cond ((and width (< width 32)) "  ")
                                 ((and width (< width 60)) "  pending · ")
                                 (t "  pending eval · "))
                           (format-turn-elapsed (since-ms born now)))
                     :muted :dim t)
              (cond ((bg-row-stopping row)
                     (list (make-seg :text "  stopping…" :tone :muted :dim t)))
                    ((band-row-stoppable-p row)
                     (list (make-seg :text "  ")
                           (seg "✕" (if stop-hovered :danger :muted) :dim (not stop-hovered)))))))))
(defun band-first-row (row segs room width bg stop-hovered)
  "An exec's first band row: the lead, the mark, SEGS -- the command -- padded
to ROOM columns, then BAND-ROW-TAIL-SEGS. => (values PAINTED PLAIN)."
  (chrome-row +band-lead+
              (append (list (seg (background-band-mark row (get-internal-real-time)) :muted)
                            (make-seg :text " "))
                      segs
                      (list (seg (format nil "~va" (max 0 (- room (segs-display-width segs))) "")))
                      (band-row-tail-segs row stop-hovered width))
              bg width))

(defun band-command-width (row width)
  "The columns a band row's command lines take at WIDTH: all of it but the
lead and mark (6), what ends the first line (BAND-ROW-TAIL-SEGS) and the
last column."
  ;; The ✕'s three columns are kept for a row that has none, so every exec's
  ;; command wraps at one column and a row's ✕ never moves its text.
  (max 4 (- width 6 (max 3 (segs-display-width (band-row-tail-segs row nil width))) 1)))

(defun band-row-expand-key (row)
  "Accordion identity for a band exec: its call's id, the key its card
takes once the exec exits (TOOL-ROW-EXPAND-KEY), so it ends open as it ran;
else the row itself."
  (or (bg-band-row-tool-call-id row) row))

(defun band-exec-row-at (model line &aux (refs (chat-model-line-rows model)))
  "The BG-BAND-ROW whose lines the band paints at transcript LINE, or NIL."
  (let ((row (and (integerp line) (< -1 line (length refs)) (aref refs line))))
    (and (bg-band-row-p row) row)))

(defun band-exec-at (model line)
  "The exec whose lines the band paints at transcript LINE -- its id, what a
click there opens or closes -- or NIL."
  (when-let (row (band-exec-row-at model line))
    (bg-band-row-shell-session-id row)))

(defun band-stop-at (model line column &aux (refs (chat-model-line-rows model)))
  "The exec whose ✕ the band paints at transcript LINE, display COLUMN — its
id — or NIL."
  ;; Every line of an exec's rows refs its BG-BAND-ROW, and the ✕ is its first
  ;; line's last glyph -- never one its command wrote on a line under it; the
  ;; cell either side of it counts, so the press need not be exact.
  (let* ((row (band-exec-row-at model line))
         (plain (and row (band-row-stoppable-p row)
                     (not (and (plusp line) (eq row (aref refs (1- line)))))
                     (aref (chat-model-line-plain model) line)))
         (at (and plain (position #\✕ plain :from-end t))))
    (when (and at (<= (abs (- column (text-display-width (subseq plain 0 at)))) 1))
      (bg-band-row-shell-session-id row))))

(defun exec-output-rows (row width column &key open (expandable t))
  "What exec ROW has written, hung under the words at COLUMN as it streams:
the ⌎ hook on that column and the lines two in, every kept line when OPEN,
else the five-row window, its omission behind ⋮ saying a click opens it
when EXPANDABLE. => (values PAINTED-LIST PLAIN-LIST), NIL before any output."
  ;; Standard hid a running exec's output behind a click;
  ;; the operator wants it streaming right under the command, behind the
  ;; hook a queued prompt hangs under the live line.
  (let ((window (background-output-window row (if open most-positive-fixnum 5)
                                          (if open most-positive-fixnum 5)))
        (bg (canvas-fill-bg))
        (painted '()) (plains '()) (first-p t))
    (flet ((emit (gutter text)
             ;; The gutter undimmed muted -- the zig marker_style -- the body dim.
             (multiple-value-bind (line plain)
                 (chrome-row (make-string (if gutter column (+ column 2)) :initial-element #\Space)
                             (append (and gutter (list (make-seg :text gutter :tone :muted)))
                                     (list (make-seg :text (take-display-prefix text (max 1 (- width column 2)))
                                                     :tone :muted :dim t)))
                             bg width)
               (push line painted)
               (push plain plains))
             (setf first-p nil)))
      (when window
        (let ((omitted (getf window :omitted)) (collapse-p (getf window :collapse-p)))
          (dolist (line (getf window :head)) (emit (and first-p "⌎ ") line))
          ;; A live tail has no head, and its count comes first: the hook
          ;; hangs on it, so it never leaves the command.
          (when (and (plusp omitted) (not collapse-p))
            (emit (if first-p "⌎ " "⋮ ") (cond (open (format nil "+~d line~:p not kept" omitted))
                             (expandable (omission-text omitted))
                             (t (format nil "+~d line~:p" omitted)))))
          (dolist (line (getf window :tail))
            (emit (cond ((and collapse-p (plusp omitted)) "⋮ ") (first-p "⌎ ")) line))))
      (values (nreverse painted) (nreverse plains)))))

(defun background-band-lines (row width)
  "One pending exec's chronological transcript block, with command and retained output.
=> (values PAINTED-LIST PLAIN-LIST REF-LIST). Every line refs ROW, including
its expandable output. No viewport budget may omit a pending call."
  (let* ((open (detail-open-p (band-row-expand-key row)))
         (bg (canvas-fill-bg))
         (stop-hovered (equal *hover-target* (cons :stop (bg-band-row-shell-session-id row))))
         (painted '()) (plains '()) (refs '()))
    (flet ((add (p pl) (push p painted) (push pl plains) (push row refs)))
      ;; The command's lines (COMMAND-ROWS), every one when open, else the
      ;; five-row window: the first behind the lead and the mark, ended by
      ;; BAND-ROW-TAIL-SEGS, the rest under it at the command's column.
      (loop with room = (band-command-width row width)
            for segs in (command-rows (bg-band-row-command row) room :open open)
            for first = t then nil
            do (multiple-value-call #'add (if first
                                              (band-first-row row segs room width bg stop-hovered)
                                              (chrome-row "      " segs bg width))))
      (multiple-value-bind (rows row-plains) (exec-output-rows row width 6 :open open)
        (mapc #'add rows row-plains))
      (values (nreverse painted) (nreverse plains) (nreverse refs)))));;; --- queued steer prompts -----------------------------------------------------
;;; render_plan.zig collectPendingSteerLines: a queued follow-up prompt is
;;; queue state, rendered inside the status block — under the live turn bar
;;; and the background band, never above them as a transcript row. Live-only
;;; chrome: promotion (the same prompt's disposition-"started" fact) clears
;;; the row and the started turn renders it as a real user message.

(defun pending-steer-rows (state)
  "The queued :PENDING-INPUT rows in queue order, or NIL."
  (loop for lane in '("history_rows" "tail_rows")
        append (remove-if-not (lambda (row) (eq :pending-input row.kind))
                              (json-value state :any lane))))

;;; --- the held prompt (the Up peel) -----------------------------------------
;;; Up on a queued prompt takes it back out of the queue and into the
;;; composer: the shell sends the withdrawal, the row leaves the band with
;;; it, and its text sits in the composer for editing. The hold is this
;;; shell's live-only state — one key of the transcript state — and it is
;;; what the withdrawal's answer reads to hand the text back when the
;;; prompt promoted before the withdrawal reached the gateway.

(defun held-prompt (state)
  "This shell's held prompt as (:prompt-id ID :text TEXT), or NIL."
  (and (hash-table-p state) (gethash "held_prompt" state)))

(defun peelable-queued-steer (model)
  "The prompt Up may peel into the composer, or NIL: the newest queued
prompt, a steer or a follow-up, while nothing is held, the turn it waits on
is still running, and the composer is empty — a draft keeps Up's other
meaning (prompt history)."
  (when (and (chat-model-turn-in-flight model)
             (null (held-prompt (chat-model-state model)))
             (zerop (length (editor-value (chat-model-editor model)))))
    (find-if #'transcript-row-prompt-id (last (pending-steer-rows (chat-model-state model))))))

(defun hold-queued-prompt (model row &aux (state (chat-model-state model)) (text row.text))
  "Put ROW's queued prompt into the composer and take the row out of the
band with it — the peel: the prompt is neither queued nor shown once Up
has it."
  ;; Paint first: the composer holding the text and the row gone are what
  ;; the operator sees before the round trip. The withdrawal follows (the
  ;; durable queue loses the seat), and a refusal hands the text back
  ;; (NOTE-WITHDRAW-PROMPT-RESULT).
  (replace-composer-text model text)
  (when (hash-table-p state)
    (setf (gethash "held_prompt" state) (list :prompt-id row.prompt-id
                                              :text text)))
  (clear-pending-input state row.prompt-id text))

(defun release-held-prompt (model)
  "End the hold: the peeled prompt is neither queued nor shown."
  (when (hash-table-p (chat-model-state model)) (remhash "held_prompt" (chat-model-state model))))

(defun prune-pending-steer-rows (state ids)
  "Drop queued rows, steers and follow-ups alike, the newest queue snapshot
no longer names — a withdrawal, usually another shell's."
  ;; A follow-up's admission fact folds as a row a replay rebuilds, so the
  ;; snapshot a subscription is sent after it is what takes a withdrawn one
  ;; back out; a promoted one its started input already cleared.
  (remove-transcript-rows state (lambda (row)
                                  (and (eq :pending-input row.kind)
                                       row.prompt-id
                                       (not (member row.prompt-id ids
                                                    :test #'equal))))))

(defun pending-steer-lines (model width)
  "The queued steer prompts as status-block rows at WIDTH — each prompt's
first line behind the ⌎ hook gutter, continuations at the body column,
capped at five lines with the counted omitted
clause."
  ;; While a turn runs, the row Esc would dispatch carries
  ;; `esc dispatches now' on its first line, and one dim hint line under the
  ;; block names the key that takes the newest prompt back into the composer —
  ;; the pending-input preview's edit hint (codex tui
  ;; bottom_pane/pending_input_preview.rs: `<binding> edit last queued
  ;; message' on its own dim line, never woven into a row). The rows are
  ;; composed inside the transcript band, whose two-column outer inset the
  ;; :STEER-BAND roles account for: the resulting absolute hook/body columns
  ;; are 5/7, under the live line's words.
  ;; => (values PAINTED-LIST PLAIN-LIST), NIL while nothing is queued.
  (let* ((rows (pending-steer-rows (chat-model-state model)))
         ;; Between the turn's end and the promotion there is nothing to cut.
         ;; The row Esc would dispatch, in NLK:DEQUEUE-NEXT-TURN's own
         ;; order: the oldest parked steer when one waits, else the oldest
         ;; prompt.
         (next (and (chat-model-turn-in-flight model)
                    (or (find-if #'transcript-row-steer-p rows) (first rows))))
         (llines
           (append
            (loop for row in rows
                  for all = (or (split-lines (trim-block-text row.text))
                                (list ""))
                  ;; Five lines a prompt at most (Zig max_pending_steer_prompt_lines).
                  for shown = (subseq all 0 (min (length all) 5))
                  for omitted = (- (length all) (length shown))
                  nconc (loop for line in shown
                              for firstp = t then nil
                              for dispatch-p = (and firstp (eq row next))
                              for text = (format nil "~a~:[~;  — esc dispatches now~]"
                                                 line dispatch-p)
                              collect (make-lline
                                       :text text :tone :steer-preview :dim t
                                       :role (if firstp
                                                 :steer-band
                                                 :steer-band-cont)
                                       :gutter-tone :muted
                                       :spans (and dispatch-p
                                                   (list (mspan (+ (length line) 2) (length text)
                                                                :tone :muted :dim t)))))
                  when (plusp omitted)
                    collect (make-lline
                             :text (format nil "[~d lines omitted]" omitted)
                             :tone :muted :dim t :role :steer-band-cont))
            ;; The block's own hint line, last: a dim continuation row, so it
            ;; aligns with the text above it and reads as the block's footer,
            ;; never as another queued prompt. It appears exactly while Up would
            ;; really take the newest prompt back, by PEELABLE-QUEUED-STEER's own
            ;; reading (a draft in the composer keeps Up on prompt history, and
            ;; the hint with it).
            (when (peelable-queued-steer model)
              (list (make-lline :text "up edits the last queued prompt"
                                :tone :steer-preview :dim t
                                :role :steer-band-cont))))))
    (when llines
      (llines-rows llines width))))

;;; --- the home / logo screen -------------------------------------------------
;;; The Zig landing surface (screens.zig + home.zig): an empty idle shell
;;; shows the logo and the tip line in the transcript
;;; band, and the composer placeholder swaps to the gateway-state prompt.
;;; It vanishes the moment any row, notice, turn, dialog, or submit exists —
;;; there is no home flag to fall out of sync, only the predicate, and a
;;; frame whose predicate moved since the band was composed composes it again.

(defparameter +home-meta-label-width+ 14
  "Columns the fact block's label lane owns — the longest label,
`/generations', plus its gutter — so every value in the block starts on one
column.")

;;; The lane is a column, not a fraction of the window: a list of folder names
;;; that re-flowed with the window swept a wide terminal instead of reading as
;;; the block it belongs to. A band too narrow to give 56 gives its room,
;;; floored at 12.
(defparameter +home-meta-value-width+ 56
  "Columns the fact block's value lane owns on every terminal — the
80-column answer the block was drawn around.")

(defun home-logo-start-row (height &optional (extra 0))
  "The absolute terminal row the logo starts at."
  ;; EXTRA is the number of rows the fact block adds under the tip. Six
  ;; content rows plus five of chrome centre the logo, version and tip;
  ;; a growing fact block lifts it rather than crowding the composer.
  (if (<= height 12) 2 (max 2 (floor (- height 11 extra) 2))))

;;; The NC mark glows. Once a launch an ember glint crosses it on the
;;; diagonal (column plus row); a click plays the glint again, and the fifth
;;; click inside two seconds — or the Konami code typed on the home screen —
;;; burns the whole mark, a flicker running through it as it cools. The
;;; colours are the nodecode.ai ramp. The frame clock carries both: the tick
;;; repaints while one plays, and the mark is still otherwise.
(defparameter +home-mark+ '("▄  ▄  ▄▄▄"
                            "█▀▄█ █   "
                            "▀  ▀  ▀▀▀")
  "The NC mark the home screen stands on, one string a row.")

(defparameter +mark-ramp+ '((242 87 15) (250 139 62) (255 171 104) (255 210 171))
  "The glow's colours from coolest to hottest; below the first, the mark's own.")

(defparameter +mark-glow-seconds+ '(:shine 0.55 :burn 1.2)
  "How long each glow the mark plays lasts.")

(defparameter +mark-launch-delay+ 0.15
  "Seconds the mark stands still after launch before the glint sets off.")

(defparameter +mark-shine-half-width+ 3.5
  "Diagonal cells from the glint's centre to where the mark is plain again.")

(defparameter +konami-code+ '(:up :up :down :down :left :right :left :right #\b #\a)
  "The keys that burn the mark from the home screen, in order.")

(defun play-mark-glow (model kind &key launch)
  "Start KIND, :SHINE or :BURN, across the mark: now, or for a LAUNCH, a beat
after it."
  (setf (chat-model-mark-glow model)
        (cons kind (+ (get-internal-real-time)
                      (if launch (round (* +mark-launch-delay+ internal-time-units-per-second)) 0)))))

(defun mark-glow-elapsed (glow)
  "Seconds GLOW has played; negative while it waits."
  (/ (- (get-internal-real-time) (cdr glow)) internal-time-units-per-second))

(defun mark-glowing-p (model &aux (glow (chat-model-mark-glow model)))
  "Whether the mark's glow owes this tick a paint: it is playing on a home
screen that stands, or has just ended and the still mark is owed."
  ;; A launch whose first screen is not home ends it there, and a home that
  ;; comes later plays its own (COMPOSE-TRANSCRIPT-VIEWPORT); a home too
  ;; cramped to paint the mark ends it by the clock.
  (when (consp glow)
    (unless (and (home-screen-p model)
                 (< (mark-glow-elapsed glow) (getf +mark-glow-seconds+ (car glow))))
      (setf (chat-model-mark-glow model) :done))
    t))

(defun mark-glow-heat (model &aux (glow (chat-model-mark-glow model)))
  "The mark's heat this frame as a function of a cell's column and row, 0
its own colour to 1 the ramp's core, or NIL when it is still."
  (when (consp glow)
    (let* ((elapsed (max 0 (mark-glow-elapsed glow)))
           (s (/ elapsed (getf +mark-glow-seconds+ (car glow)))))
      (when (< s 1)
        (ecase (car glow)
          ;; A band crossing the diagonal, eased so it is quickest over the mark.
          (:shine
           (let* ((w +mark-shine-half-width+)
                  (span (+ (length (first +home-mark+)) (length +home-mark+) -2))
                  (centre (+ (- w) (* (/ (- 1 (cos (* pi s))) 2) (+ span w w)))))
             (lambda (x row) (max 0 (- 1 (/ (abs (- (+ x row) centre)) w))))))
          ;; A flare, a hold, a cool; a wave runs down the diagonal throughout.
          (:burn
           (let ((level (cond ((< s 0.1) (/ s 0.1))
                              ((< s 0.4) 1)
                              (t (/ (+ 1 (cos (* pi (/ (- s 0.4) 0.6)))) 2))))
                 (phase (* 6 pi elapsed)))
             (lambda (x row)
               (* level (+ 0.7 (* 0.2 (sin (- phase (/ (+ x row) 1.5))))))))))))))

(defun mark-line-segs (line row heat)
  "LINE, row ROW of the mark, as segs: the title tone, each cell HEAT warms
painted up the ramp."
  (if (null heat)
      (list (make-seg :text line :tone :title))
      (loop with ramp = (cons (visual-style-fg (style-for-tone :title)) +mark-ramp+)
            for ch across line
            for x from 0
            for at = (* (funcall heat x row) (1- (length ramp)))
            collect (make-seg :text (string ch)
                              :tone (if (<= at 0)
                                        :title
                                        (multiple-value-bind (step part) (floor at)
                                          (vstyle :fg (blend-rgb (or (nth (1+ step) ramp) (nth step ramp))
                                                                 (nth step ramp)
                                                                 (round (* 100 part)))
                                                  :bold t)))))))

(defun click-mark (model &aux (now (get-internal-real-time)))
  "A click on the mark: the glint again, or on the fifth inside two seconds,
the burn."
  (let ((clicks (cons now (remove-if (lambda (at) (> (- now at) (* 2 internal-time-units-per-second)))
                                     (chat-model-mark-clicks model)))))
    (setf (chat-model-mark-clicks model) (if (>= (length clicks) 5) nil clicks))
    (play-mark-glow model (if (>= (length clicks) 5) :burn :shine))
    (render-state model)))

(defun konami-key (model key plain &aux (at (chat-model-konami-progress model)))
  "Advance MODEL's run through the Konami code by KEY, PLAIN when no modifier
is held; T when the code spends KEY: its Left and Right on the home screen,
and its last key when that burns the mark."
  ;; Up and Down walk prompt history and back to the empty draft, and the
  ;; `b' types, so those pass through; Left on an empty composer would leave
  ;; the session (LEAVE-SESSION-OP), so the code keeps its own.
  (let ((next (cond ((and plain (eql key (nth at +konami-code+))) (1+ at))
                    ;; Up Up Up still stands two keys in.
                    ((and plain (eq key :up)) (if (= at 2) 2 1))
                    (t 0))))
    (setf (chat-model-konami-progress model) (mod next (length +konami-code+)))
    (cond ((= next (length +konami-code+)) (konami-burn model))
          ((<= 5 next 8) (home-screen-p model)))))

(defun konami-burn (model &aux (editor (chat-model-editor model)))
  "Burn the mark when the code's `b' is all the composer holds and, with it
gone, the home screen stands; T when it did, so the `a' is spent."
  (when (equal (editor-value editor) "b")
    (editor-reset editor)
    (cond ((home-screen-p model)
           (play-mark-glow model :burn)
           (render-state model)
           t)
          (t (editor-insert editor "b") nil))))

;;; --- the fact block -------------------------------------------------------
;;;
;;; What the landing screen is for is orientation, and the facts worth
;;; orienting by are the ones nothing else on the screen carries: which home
;;; this is, how many cell folders this process holds, and how many sealed
;;; generations the home can restore. Load diagnostics stay in logs. Each
;;; row is labelled by the command whose catalog holds the rest -- /profile,
;;; /cells, /generations -- so the row teaches the command without a tail
;;; spelling out what it does.
;;;
;;; Row order is shedding order. A short band drops rows from the tail, and
;;; the two facts the footer still carries — the model identity and the
;;; workspace — are the right ones to lose.

(defun cell-label (name)
  "A folder name as a list of them says it, on the home screen or in setup:
the shipped `nodecode-` prefix is noise once five of them line up."
  (if (and (> (length name) 9) (uiop:string-prefix-p "nodecode-" name))
      (subseq name 9)
      name))

(defun home-meta-value-width (width)
  "Columns a fact value may occupy before it folds: the fixed lane, or what
the band can hold when the band is narrower than that."
  ;; The width is a constant and not a fraction of WIDTH, so the fact block
  ;; reads the same on every terminal instead of stretching with the window it
  ;; happens to sit in.
  (max 12 (min (- width +home-meta-label-width+ 4) +home-meta-value-width+)))

(defun clipped-path (path width &aux (head (subseq path 0 (1+ (or (position #\/ path) -1))))
                                     (room (- width (text-display-width head) 1)) (tail nil))
  "PATH in at most WIDTH columns: whole when it fits, else its head (`~/' or
`/'), an … and as many whole trailing folders as the room holds, so the last
folder always shows — cut from its front only when it alone is wider."
  (cond ((<= (text-display-width path) width) path)
        (t (loop for slash = (position #\/ path :from-end t)
                   then (position #\/ path :from-end t :end slash)
                 while (and slash (>= slash (length head))
                            (<= (text-display-width (subseq path slash)) room))
                 do (setf tail (subseq path slash)))
           (concatenate 'string head "…"
                        (or tail (subseq path (max (length head)
                                                   (- (length path) (max 0 room)))))))))

(defun home-meta-rows (width &aux (records (or (ignore-errors (nlk:cells)) '()))
                                  ;; The layer is part of the composed self, not an installed cell.
                                  ;; Knowledge cells and folders whose load failed still count.
                                  (folders (remove :layer records :key #'nlk:cell-kind))
                                  (lane (home-meta-value-width width))
                                  (rows '()))
  "The landing screen's fact block: an ordered list of (LABEL VALUE TONE
COMMAND), COMMAND the slash command a click on the row runs."
  ;; A folded value's continuation rows carry LABEL NIL, so the value lane
  ;; stays one column.
  ;;
  ;; Only facts the chrome cannot already give: the model identity and the
  ;; workspace sit in the footer of every frame, so repeating them here would
  ;; be two rows that say nothing new the moment the operator's eye has moved
  ;; once.
  (flet ((row (label value tone command)
           (when (and value (plusp (length value)))
             (push (list label value tone command) rows))))
    ;; Which organism this is. Every home has a name to say, the default
    ;; one included: identity by absence reads as unknown, not as default,
    ;; and an operator with two shells open is asking the question this
    ;; row answers.
    ;; The home is written as the operator reads it (`~/...') and kept to
    ;; the value lane from the left, so its last folder, the one that
    ;; names it, is the part that shows: the whole path cut at the
    ;; window's edge would lose exactly that. A long name leaves the
    ;; path eight columns.
    (let ((name (clip-text-to-width (nlk:profile-label) (max 1 (- lane 3 8)))))
      (row "/profile"
           (format nil "~a · ~a" name
                   (clipped-path (home-abbreviated (namestring (nlk:home)))
                                 (max 8 (- lane (text-display-width name) 3))))
           :chrome "profile"))
    (row "/cells" (format nil "~d installed" (length folders)) :chrome "cells")
    ;; Failed/skipped cell details stay in DESCRIBE-CELLS's boot log and
    ;; explicit inspection (/cells), never the landing screen.
    ;; Sealed generations are the organism's restore points, counted from
    ;; the canonical manifest roster -- by its stat, since this runs every
    ;; frame (NLK:MANIFEST-COUNT). The row stands before the first seal,
    ;; just as /cells stands before the first installed cell.
    (row "/generations" (format nil "~d sealed" (nlk:manifest-count)) :chrome "generations")
    ;; What is MISSING, which the rows above cannot say: they describe what
    ;; the folder did, and an operator whose model pin was refused or whose
    ;; channel never started reads a screen full of healthy cells and
    ;; concludes everything is fine. Cell load diagnostics stay in the
    ;; boot log and explicit inspection, so they are excluded here;
    ;; everything else — including a cell's own, which reach
    ;; HEALTH-FACTS through the hook — lands on the landing screen because
    ;; this is the frame an operator is looking at when they wonder.
    ;; :DANGER only, and the distinction is load-bearing. A bare image has
    ;; no config and no model pin, which HEALTH-FACTS reports as :warn —
    ;; true, and not a fault: the operator is standing in the wizard that
    ;; sets it. A landing screen that announced it would be telling a
    ;; first-time user their fresh install is broken, and would cost the
    ;; block its rule that a bare image shows no rows at all. What reaches
    ;; here is what is actually WRONG — a refused pin, a folder that failed
    ;; — and the rest waits for /doctor, which is asked rather than shown.
    ;; Folded to the value lane like the cells, whole: a refused pin's
    ;; reason is what to do, and one row cut at the window's edge ended
    ;; before it said so (xm-203).
    (dolist (fact (remove-if-not (lambda (fact) (eq (health-fact-tone fact) :danger))
                                 (or (ignore-errors (health-facts)) '())))
      (unless (member (health-fact-key fact) '(:cells :cell))
        (let ((text (format nil "~a~@[ — ~a~]" (health-fact-detail fact) (health-fact-remedy fact))))
          (loop for line in (wrapped-lines text lane)
                for first = t then nil
                do (row (and first (health-fact-label fact)) line :danger "doctor"))))))
  (nreverse rows))

(nlk:define-side-table home-row-actions (:weakness :key :synchronized t)
  "Live-only plain Home row -> ((START END TARGET) ...), from painted segments.")

(defun home-command-at-position (model line column &aux (plains (chat-model-line-plain model)))
  "The trusted Home action on the painted row at LINE/COLUMN, or NIL."
  (when (and (home-screen-p model) (integerp line) (< -1 line (length plains)) column)
    (let ((plain (aref plains line)))
      (range-target (home-row-actions plain) (plain-column-index plain column)))))

;;; One tip a shell. A line that never changed was read once and then not at
;;; all, so each launch picks its own. A home that has sealed no generation
;;; is on its first launch, and gets the first: what only a self that
;;; rewrites itself can do. The pick holds for the shell's life, /clear
;;; included, so the line never changes under the operator's eye.
(defparameter +home-tips+
  '(("Try \"" "you can't read PDFs. teach yourself." "\"")
    ("Type " "/" " to browse commands; /help shows shortcuts.")
    ("" "/generations" " restores any earlier self.")
    ("" "/tree" " rewinds to any turn, or continues from it.")
    ("" "/think" " sets how hard the model reasons."))
  "The landing screen's tips, each (BEFORE TYPED AFTER): the words around the
one thing to type.")

(defun home-tip (model)
  "MODEL's tip from +HOME-TIPS+, picked at its first landing screen."
  (or (chat-model-home-tip model)
      (setf (chat-model-home-tip model)
            (if (zerop (nlk:manifest-count))
                (first +home-tips+)
                (nth (random (length +home-tips+) (make-random-state t)) +home-tips+)))))

(defun home-band-lines (model width &aux (height (chat-model-term-height model)))
  "The home band as (values LINES PLAINS): painted rows sized to fill the
transcript band exactly, so the band's bottom-anchoring becomes the Zig
absolute-row placement."
  ;; NIL on cramped windows — home.zig draws nothing below 12 rows or 48
  ;; columns; row 1 belongs to the shared header.
  (when (and (>= height 12) (>= (chat-model-term-width model) 48))
    (let* ((full (max 1 (- height (shell-overhead height (composer-visible-rows model)))))
           ;; The setup walk's dialog docks under the landing screen and takes
           ;; its rows from the band: the screen centres in what is left, as
           ;; on a window that much shorter.
           (band-height (if (and (chat-model-dialog model) (chat-model-band model))
                            (progn (sync-viewport-geometry model)
                                   (min full (band-window-height (chat-model-band model))))
                            full))
           (height (- height (- full band-height)))
           ;; How many folded lines the cell lane may spend: what the
           ;; band can hold once the logo, the tip and their
           ;; blanks have taken theirs. `+N more` is then a last resort on
           ;; a genuinely full screen, not a fixed cut at three.
           ;; A screen that still does not fit folds the lane to one line
           ;; first: the rows under it are what is wrong, a refused pin's
           ;; reason ending on what to do, and the tail went first (xm-203).
           ;; The rows then shed from the tail until the whole landing
           ;; screen fits; the anchor lifts as the block grows, so each drop
           ;; is re-measured.
           ;; The blank that separates the block from the tip belongs to
           ;; the block: an empty block costs no row at all.
           (extra (lambda (rows) (if rows (1+ (length rows)) 0)))
           (pad (lambda (rows) (max 0 (- (home-logo-start-row height (funcall extra rows)) 3))))
           (meta (let ((rows (home-meta-rows width)))
                   (loop until (or (null rows)
                                   ;; logo, version, blank, tip, blocks
                                   (<= (+ (funcall pad rows) 6 (funcall extra rows)) band-height))
                         ;; Lower-priority orientation never cuts off a
                         ;; diagnostic's remedy. Generations and profile
                         ;; yield before the diagnostic tail.
                         for spare = (and (find :danger rows :key #'third)
                                          (or (find "generations" rows :key #'fourth :test #'equal)
                                              (find "profile" rows :key #'fourth :test #'equal)))
                         do (setf rows (if spare (remove spare rows :test #'eq)
                                           (butlast rows))))
                   rows))
           (top-pad (funcall pad meta))
           (bg (canvas-fill-bg))
           (lines '())
           (plains '()))
      ;; A row's plain lane is the text its segs paint (SEGS-TEXT), so the two
      ;; lanes cannot disagree about what the row says.
      (labels ((emit-at (pad segs &optional command
                        &aux (spaces (make-string pad :initial-element #\Space))
                        (plain (concatenate 'string spaces (segs-text segs))))
                 ;; Ranges are character indexes into this final plain object;
                 ;; the hit reader converts painted columns (Unicode included).
                 ;; A Home command covers its painted segments.
                 (let ((target (and command (cons :home command))) (offset pad) (actions nil))
                   (loop for seg in segs
                         for text = (seg-text seg)
                         for end = (+ offset (length (string-right-trim " " text)))
                         do (when (and target (< offset end))
                              (push (list offset end target) actions)
                              ;; The mark answers with its glint, not a rule
                              ;; through its glyphs.
                              (when (and (stringp command)
                                         (equal (chat-model-hover-target model) target))
                                (setf (seg-underline seg) t)))
                            (incf offset (length text)))
                   (when actions (setf (home-row-actions plain) (nreverse actions))))
                 (push (render-row (list (make-seg :text spaces)) segs bg width) lines)
                 (push plain plains))
               (blank ()
                 (push "" lines)
                 (push "" plains))
               (emit (&rest segs)
                 (emit-at (max 0 (floor (- width (segs-display-width segs)) 2)) segs)))
          (dotimes (i top-pad) (blank))
          (loop with heat = (mark-glow-heat model)
                for line in +home-mark+
                for row from 0
                for segs = (mark-line-segs line row heat)
                do (emit-at (max 0 (floor (- width (segs-display-width segs)) 2)) segs :mark))
          ;; The semantic tag belongs directly under the logo. Commit metadata
          ;; remains in `nodecode --version`; both read the same build identity.
          (emit (make-seg :text (format nil "v~a" (subseq *user-agent-version* 0
                                                         (position #\+ *user-agent-version*)))
                          :tone :muted))
          (blank)
          ;; The shell's tip (HOME-TIP), the one thing to type in it brighter
          ;; than the words around it.
          (destructuring-bind (before typed after) (home-tip model)
            (emit (make-seg :text "Tip " :tone :tool)
                  (make-seg :text before :tone :muted)
                  (make-seg :text typed :tone :chrome)
                  (make-seg :text after :tone :muted)))
          ;; The fact block centres as one block, not line by line: a
          ;; column of labels only reads as a column when every row shares
          ;; one left edge.
          (when meta
            (blank)
            (let ((pad (max 0 (floor (- width
                                        +home-meta-label-width+
                                        (loop for row in meta
                                              maximize (text-display-width (second row))))
                                     2))))
              (loop for (label value tone command) in meta
                    do (emit-at pad
                                (list (make-seg :text (format nil "~va" +home-meta-label-width+ (or label ""))
                                                :tone (if (eq tone :danger) :danger :muted))
                                      (make-seg :text value :tone tone))
                                command)))))
      ;; Fill the band exactly: bottom-anchored full content is
      ;; top-anchored content, and rows land on their Zig absolute rows.
      ;; A band shorter than the content truncates from the tail, the
      ;; home.zig row >= content_bottom_row break.
      (flet ((fit (pushed &aux (rows (nreverse pushed)))
               (loop repeat band-height collect (or (pop rows) ""))))
        (values (fit lines) (fit plains))))))

(defun projection-width (model)
  "The width the transcript projection is built at."
  ;; One expression, because RENDER-STATE, the cache guard, and the animation
  ;; tick must agree on it — a guard reading a width the renderer never uses
  ;; either re-projects every frame or never re-projects at all.
  (transcript-text-width (max 20 (chat-model-term-width model))))

(defun projection-cache-live-p (model &aux (cache (chat-model-projection-cache model)))
  "Whether MODEL's cached transcript projection was built under the render
inputs now in force: the width it was laid out at, and the palette and color
profile it was PAINTED under."
  ;;
  ;; The paint inputs belong in this guard because a projected row is an
  ;; SGR-baked string, not a description of one — the viewport holds finished
  ;; bytes. Every render cache below this one already revalidates on *PALETTE*
  ;; (LLINE-RENDER, ROW-DISPLAY-KEY, the band-row epoch); the projection cache is
  ;; the last one above them, and it is where a theme engage used to survive.
  (and cache
       (eql (first cache) (projection-width model))
       (eq (second cache) *palette*)
       (eq (third cache) (effective-color-profile))))

(defmacro with-render-scope ((model) &body body)
  "BODY under the render-scope specials MODEL's rows are laid out under --
the detail level and its chord, the open card, the hover, the image mode."
  ;; Every look at the rows lays them out, and the row memos key on these, so
  ;; a look taken outside them would lay out rows the render then lays out
  ;; again -- or, for an input no memo keys on (the chord), keep them as the
  ;; look saw them.
  `(let ((*tool-detail* (chat-model-tool-detail ,model))
         (*fades* (and (chat-model-program ,model) t))
         (*expanded-tool-call* (chat-model-expanded-tool-call ,model))
         (*hover-row* (chat-model-hover-row ,model))
         (*hover-target* (chat-model-hover-target ,model))
         (*kitty-images-enabled* (chat-model-kitty-graphics ,model)))
     ,@body))

(defun transcript-projection-start (model state width)
  "The row MODEL projects STATE from: the tail while the window follows it;
where the window stood or grew to while it reads STATE's history; the first
under a found-block pin, which rests on a line of the whole table."
  ;; The tail is chosen again at every following render — the rows are
  ;; cached, so it is cheap — rather than kept: a render landing mid-replay
  ;; would otherwise pin the start of the few rows folded so far.
  (let ((entry (chat-model-projection-start model)))
    (cond ((transcript-pins model)
           (setf (chat-model-projection-start model) (cons state nil))
           nil)
          ((and entry (eq (car entry) state) (not (chat-model-follow-bottom model)))
           (cdr entry))
          ;; A preview under a dialog is looked at rather than read back, and a
          ;; held arrow lays one out a row: its first projection is one window.
          ;; Any other state's tail is measured in screens of the terminal, not
          ;; of the band: a dialog docked over the band leaves it a row or a
          ;; few, and a tail chosen under it outlives the dialog.
          (t (let ((band (chat-model-band model))
                   (preview (chat-model-transcript-preview model)))
               (cdr (setf (chat-model-projection-start model)
                          (cons state
                                (tail-projection-start
                                 state width
                                 (if (and preview (eq state (cdr preview)))
                                     (if band (band-window-height band) (chat-model-term-height model))
                                     (* *tail-projection-screens* (chat-model-term-height model))))))))))))

(defun grow-transcript-projection (model &key whole)
  "Project MODEL's transcript further back — WHOLE, or by another window once
its window is within a window of where the projection starts — keeping the
window on the rows it shows."
  ;; The window's offset counts from the top, so the rows grown in above
  ;; push what it shows down by exactly their lines. A window at a time,
  ;; because every line grown in is laid out for the first time (about 0.1
  ;; ms of layout and collection each): three windows made a hitch of
  ;; several frames every few dozen notches of the wheel.
  (let ((window (chat-model-band model))
        (entry (chat-model-projection-start model)))
    (when (and window entry (cdr entry)
               (eq (car entry) (visible-transcript-state model))
               (not (chat-model-follow-bottom model))
               (or whole (< (band-window-offset window) window.height)))
      (let ((before window.count))
        ;; The look lays out rows as the render will, so the render reuses
        ;; every block it memoized.
        (setf (cdr entry)
              (and (not whole)
                   (with-render-scope (model)
                     (tail-projection-start (car entry) (projection-width model)
                                            window.height (cdr entry)))))
        (render-state model)
        (band-window-scroll window (- window.count before))))))

;;; The per-row render memos are weak on the row, and a transcript keeps its
;;; rows: without this, every row ever projected would keep its logical lines,
;;; its block and its paint -- and through that paint the band rows keyed on
;;; it -- for the life of the shell. A full render keeps them for what it
;;; projects; a row scrolled back into the projection is built again once.
(defun forget-unprojected-rows (blocks &aux (projected (make-hash-table :test 'eq)))
  "Drop the render memos of every transcript row BLOCKS does not project."
  (flet ((keep (row) (when row (setf (gethash row projected) t))))
    (dolist (block blocks)
      (let ((row (display-block-row block)))
        (keep row)
        (when (tool-group-p row) (mapc #'keep (tool-group-members row))))
      (mapc #'keep (display-block-refs block))))
  (dolist (table (list *row-display-cache* *row-llines-cache* *row-md-cache*
                       *row-activity-cache*))
    (loop for row being the hash-keys of table
          unless (gethash row projected) do (remhash row table))))

(defun render-state (model)
  "Render the folded rows into the viewport at the transcript text width."
  ;; Streaming lives in the row lanes themselves — the reducers coalesce
  ;; deltas into live-only tail rows — so rendering is projection plus rhythm.
  ;; A transport NOTICE, when set, renders as one live-only system block at
  ;; the tail; the live turn bar follows it when a turn is in flight.
  ;;
  ;; This is the full render: it re-projects the whole transcript, refreshes
  ;; the projection cache, and clears the stream-coalescing dirty flag.
  ;; Animation ticks go through RENDER-STATUS-FRAME instead, which recomposes
  ;; only the live chrome over the cached projection.
  (with-render-scope (model)
    (bind ((width (projection-width model))
           (state (visible-transcript-state model))
           (blocks (project-state state :width width
                                        :start (transcript-projection-start model state width) :prompt t))
           ((lines plains refs) (transcript-line-table blocks)))
      (forget-unprojected-rows blocks)
      (setf (chat-model-projection-cache model)
            (list width *palette* (effective-color-profile)
                  lines plains refs blocks)
            (chat-model-transcript-dirty model) nil)
      (compose-transcript-viewport model width lines plains refs))))

(defun render-state-at-paint (model)
  "Leave MODEL's transcript for the next paint to project: the frame renders
a projection that is no longer live before it composes (the view's settle),
so any number of changes before it lay the rows out once."
  (setf (chat-model-projection-cache model) nil))

(defun status-chrome-key (model &optional (width (projection-width model)))
  "What the live chrome shows that only the clock moves, as it reads now:
the turn bar at WIDTH (its mark, elapsed clock and verb -- or the previewed
turn's), each band row's mark, and the transient hint while it stands. Two
composes with EQUAL keys paint the same chrome."
  ;; The bar is the bar's own render (LIVE-STATUS-LINE), never a second
  ;; derivation of its clocks: a clock the bar grows later moves the key by
  ;; construction. One row's render, against the frame compose it stands in
  ;; for, is what a tick that moved nothing costs.
  (let ((now (get-internal-real-time)))
    (list (live-status-line model width)
          (loop for row in (state-background-rows (visible-transcript-state model))
                collect (list (background-band-mark row now) (segs-text (band-row-tail-segs row nil))))
          (status-said model))))

(defun status-chrome-moved-p (model)
  "Whether the live chrome reads differently now than at the last compose: a
spinner step, an elapsed digit, a verb going to `waiting', a band mark, a
hint running out."
  ;; The frame clock ticks 60 times a second; the spinner holds a frame
  ;; 150 ms and the elapsed clock a digit 100 ms, so most ticks move nothing.
  ;; Without the moved-p check, every tick would compose the whole frame; that
  ;; composition would be a busy shell's entire cost.
  (not (equal (status-chrome-key model) (chat-model-status-chrome-key model))))
(defun render-status-frame (model)
  "Refresh live clocks over cached transcript blocks, without re-grouping or
re-projecting history. Only pending exec blocks get new command/elapsed paint."
  (if (and (projection-cache-live-p model)
           (or (seventh (chat-model-projection-cache model))
               (null (state-background-rows (visible-transcript-state model)))))
      (with-render-scope (model)
        (let* ((cache (chat-model-projection-cache model))
               (blocks (seventh cache)))
          (when (some (lambda (block) (bg-band-row-p (display-block-row block))) blocks)
            (setf blocks (mapcar (lambda (block &aux (row (display-block-row block)))
                                  (if (bg-band-row-p row) (row-display-block row :width (first cache)) block))
                                blocks))
            (multiple-value-bind (lines plains refs) (transcript-line-table blocks)
              (setf (cdddr cache) (list lines plains refs blocks))))
          (compose-transcript-viewport model (first cache) (fourth cache) (fifth cache) (sixth cache))))
      (render-state model)))
(defun fill-line-vector (vector rows count tail &aux (needed (+ count (length tail))))
  "VECTOR holding the first COUNT rows of ROWS followed by TAIL."
  ;; The lane the mouse and the kitty placements index has to be a vector, and
  ;; rebuilding it with COERCE copied the projected transcript twice per
  ;; frame. It is reset and refilled instead, and grows only when the
  ;; transcript does -- and shrinks when a projection that was far longer is
  ;; not coming back.
  ;;
  ;; A slot past the new fill pointer is out of the lane's reach, not the
  ;; collector's: it still names a row of the longer projection. Without
  ;; clearing it, a lane that shrinks would keep that row alive. Those slots
  ;; are emptied.
  (when (or (not (array-has-fill-pointer-p vector))
            (< (array-dimension vector 0) needed)
            (> (array-dimension vector 0) (max 1024 (* 4 needed))))
    (setf vector (make-array (max needed 64) :adjustable t :fill-pointer 0)))
  (let ((was (fill-pointer vector)))
    (setf (fill-pointer vector) needed)
    (replace vector rows :end1 count)
    (replace vector tail :start1 count)
    (loop for index from needed below was
          do (setf (aref vector index) nil)))
  vector)

;;; --- the found-block pin ------------------------------------------------------
;;; The conversation search's Enter lands on a block, not on a tail: each hit
;;; carries the durable log position of the fact its block was built from,
;;; the fold stamps every transcript row it builds with that position
;;; (STAMP-UNPOSITIONED-ROWS), and the pin is the promise to rest the window
;;; on it — now, and again when the replay that follows an attach replaces
;;; the folded rows. The two row lists are different lengths (the preview page
;;; ends near the block; the replay carries the whole session), so the line
;;; index is recomputed, never remembered.

;;; Written where a search hit is chosen, read by APPLY-TRANSCRIPT-PIN, and
;;; dropped by every gesture that moves the window off it on purpose — a
;;; scroll, a submit, a session switch.
(nlk:define-side-table transcript-pins (:weakness :key)
  "Weak memo: model -> (SESSION-ID . LOG-POSITION), the block the model's
transcript is resting on, or absent.")

(defun transcript-pin-p (model session-id &aux (pin (transcript-pins model)))
  "Whether MODEL's transcript is pinned to a block of SESSION-ID."
  (and pin (equal (car pin) session-id) t))

(defun transcript-shown-session-id (model &aux (preview (chat-model-transcript-preview model)))
  "The session MODEL's transcript shows right now: the armed preview's while
a dialog holds one, the attached session's otherwise — VISIBLE-TRANSCRIPT-STATE's
substitution, said as an id."
  ;; A pin is honored against the transcript the operator is looking at, which
  ;; is the previewed session for as long as the dialog previews one.
  (if (and preview (chat-model-dialog model))
      (car preview)
      (chat-model-session-id model)))

(defun transcript-line-for-position (model position &aux (refs (chat-model-line-rows model)))
  "The projection line MODEL's block at durable log POSITION starts on — the
first line whose owning row carries that position, else the first whose row
sits past it, because a fold that landed the block's row one lane over still
lands the operator in the right place."
  ;; NIL when the transcript has not folded that far: no line is invented for
  ;; a block that is not there.
  (position-if (lambda (ref)
                 (and (transcript-row-p ref) ref.log-position (>= ref.log-position position)))
               refs))

(defun apply-transcript-pin (model &aux (pin (transcript-pins model)))
  "Rest MODEL's transcript window on its pinned block: the block's first line
becomes the window's top row and the follow is off, because reading a found
block is not reading the tail."
  ;; Called from COMPOSE-TRANSCRIPT-VIEWPORT — the one pass that holds both
  ;; the folded rows and the line table they paint as — so every render
  ;; re-seats the window while the pin stands: a live frame moves the session,
  ;; never the promise. => T when the block was found and the window is there.
  (when (and pin (chat-model-band model)
             (equal (car pin) (transcript-shown-session-id model)))
    (when-let (line (transcript-line-for-position model (cdr pin)))
      (setf (band-window-offset (chat-model-band model)) line
            (chat-model-follow-bottom model) nil)
      t)))

(defun pin-transcript-position (model session-id position)
  "Pin MODEL's transcript on SESSION-ID's block at durable POSITION — a
conversation-search hit — and repaint, which lands the window on it
(APPLY-TRANSCRIPT-PIN runs inside that render and again in every later one,
the replay that follows an attach included). => T when the window moved."
  (setf (transcript-pins model) (cons session-id position))
  (render-state model)
  (transcript-pin-p model session-id))

(defun clear-transcript-pin (model)
  "Drop MODEL's viewport pin: the window is the operator's again."
  (forget-transcript-pins model)
  model)

(defun compose-status-block (model width &optional blank-above)
  "The live block the transcript ends on: the bar, what a retried provider
said (RETRY-REASON-LINES), queued prompts, and a row under them the newest
thought of its run (LIVE-THOUGHT-ROWS) -- no rows while nothing runs or
waits. BLANK-ABOVE: the transcript's last row is canvas already. Pending
execs belong to history."
  ;; The block scrolls with the transcript and the scroll gutter runs down
  ;; through it, one canvas row off the composer. Without this, a block
  ;; fixed between them would leave the gutter short of the box. A window
  ;; with the rows holds the bar off the work above by one canvas row; a
  ;; short window and the home screen's logo keep theirs.
  (bind ((status (first (chat-model-status-chrome-key model)))
         (height (chat-model-term-height model))
         (free (- height (shell-overhead height (composer-visible-rows model))
                  (or (composer-dropdown-geometry model) 0)))
         (spaced (and (shell-footer-p height) (>= free 8) (not (home-screen-p model))))
         (limit (max 1 (ceiling (- free (if spaced 1 0)) 2)))
         ;; A retry's words under the bar, then the queued prompts.
         ((said said-plains) (retry-reason-lines model width))
         ((steers steer-plains) (pending-steer-lines model width))
         (lines (append said steers)) (plains (append said-plains steer-plains))
         (count (length lines)) (room (max 0 (- limit 2)))
         (kept (if (> count room) (max 0 (1- room)) count))
         ;; On the column of the queued prompts it counts.
         ;; A painted row of the block like the ones it counts, so it fills
         ;; the band's width on its canvas and is cut where they are.
         ((omission omission-plain)
          (when (and (> count room) (plusp room))
            (bind (((rows plains)
                    (llines-rows (list (make-lline :text (format nil "… ~d status lines omitted" (- count kept))
                                                   :tone :muted :dim t :role :steer-band-cont))
                                 width)))
              (values (first rows) (first plains)))))
         ;; The thought stands a row off the bar and its lines, in the room
         ;; they leave it: a paragraph taller than that keeps its newest
         ;; rows, where its words arrive.
         (budget (max 0 (- room kept (if omission 1 0) 1)))
         ((thought thought-lines) (and status (plusp budget) (live-thought-rows model width)))
         (thought-rows (last thought budget)) (thought-plains (last thought-lines budget))
         ;; Under the bar the block keeps the most rows the turn's thoughts
         ;; have taken, in the room it has: a shorter paragraph after a
         ;; longer one leaves canvas, so a window following the tail holds
         ;; still.
         (beneath (if thought-rows (1+ (length thought-rows)) 0))
         ;; Over the bar, canvas keeps the running turn's run and the row
         ;; off it as tall as they have stood (RUN-LIFT).
         (lift (if status (run-lift model (if (and spaced (not blank-above)) 1 0)) 0))
         (pad (if (and status (plusp budget))
                  (max 0 (- (min (1+ budget) (live-mark model (nth-value 2 (bar-lane model)) :under beneath))
                            beneath))
                  0)))
    (flet ((lane (bar rows more blank under fill)
             (append (and spaced (not blank-above) (list blank))
                     (make-list lift :initial-element fill) (and status (list bar))
                     rows (and omission (list more))
                     (and under (list blank)) under
                     (make-list pad :initial-element fill))))
      (if (or status (plusp kept))
          (list (lane status (subseq lines 0 kept) omission "" thought-rows "")
                (lane (and status (plain-frame-row status)) (subseq plains 0 kept)
                      omission-plain "" thought-plains "")
                (lane :status (make-list kept :initial-element :status) :status nil
                      (make-list (length thought-rows) :initial-element :status) nil))
          (list nil nil nil)))))

(defun compose-transcript-viewport (model width lines plains refs &aux (base-count (length lines))
                                                                       (tail-lines '())
                                                                       (tail-plains '())
                                                                       (tail-refs '()))
  "Stack the live chrome under the projected transcript rows and hand the
result to the viewport."
  ;; Never mutates LINES/PLAINS/REFS — the projection cache shares them across
  ;; animation ticks.
  ;;
  ;; The chrome accumulates in lanes of its own rather than onto the
  ;; projection: the base is the same list object for every animation frame,
  ;; so appending a handful of chrome rows to it copied every transcript row
  ;; three times, once per chrome region, on every frame — and the copy grew
  ;; with the conversation.
  ;; The live block is the transcript's last rows.
  (setf (chat-model-status-chrome-key model) (status-chrome-key model width)
        (values tail-lines tail-plains tail-refs)
        (values-list (setf (status-blocks model)
                           (compose-status-block
                            model width
                            (let ((last (car (last lines))))
                              (and last (blank-band-line-p last width (canvas-fill-bg))))))))
 ;; An empty band is the home screen when the predicate holds (Zig
 ;; activeScreen): logo and tip. Otherwise it stays blank — the
 ;; connection narrative lives in the composer placeholder now. A home that
 ;; comes back -- a dialog closed over it, /clear -- is the launch's again,
 ;; glint and all; the launch plays its own (NLT:INIT).
 (let ((home (home-screen-p model)))
   (when (and home (null (chat-model-home-painted model)))
     (play-mark-glow model :shine :launch t))
   (setf (chat-model-home-painted model) home)
   (when (and (null lines) home)
     (setf (values lines plains) (home-band-lines model width)
           refs (make-list (length lines))
           base-count (length lines))))
 ;; Stamp the transcript's own hit-test lane — its rows, links and hover
 ;; targets. The selection wash is NOT applied here: a mark is a screen
 ;; region, so the frame's overlay (SELECTION-OVERLAY) washes the cells the
 ;; frame paints, whichever region painted the row.
 (setf (chat-model-line-plain model)
       (fill-line-vector (chat-model-line-plain model) plains base-count tail-plains)
       (chat-model-line-rows model)
       (fill-line-vector (chat-model-line-rows model) refs base-count tail-refs))
 (sync-viewport-geometry model)
 ;; The window takes the two row lists as they are, no join and re-split: the
 ;; rows keep the identity the band-row cache keys on, and the projected base
 ;; stays one list object across animation frames.
 ;; The pinned prompt band reads the projection's prompt spans, never its painted
 ;; rows: the refs lane is the one place that says which lines the operator's
 ;; prompts own, and it is rebuilt with the projection. Keyed by the base list
 ;; object, so a window showing a cached projection finds that projection's
 ;; spans and no other's.
 (setf (band-prompt-spans lines) (refs-prompt-spans refs)
       (band-disclosure-spans lines)
       (refs-disclosure-spans refs (chat-model-expanded-tool-call model) width))
 (band-window-set-lines (chat-model-band model) lines tail-lines (not (chat-model-follow-bottom model)))
 ;; A found-block pin is honored HERE, the pass that holds both the folded
 ;; rows and the line table they paint as: the window rests on the block
 ;; while the pin stands — a live frame moves the session, never the promise
 ;; — and the follow below must not snap a pinned window to the tail. After
 ;; the window has taken the lines: an offset is clamped to the content it
 ;; is set against, and set against the tail projected before the pin; without
 ;; that clamp, a block above that tail would land the window at the tail's top.
 (apply-transcript-pin model)
 ;; Follow the tail only while pinned: an operator reading history keeps
 ;; their place as new rows stream in below.
 (when (chat-model-follow-bottom model)
   (band-window-goto-bottom (chat-model-band model))))

;;; --- transcript scrolling ---------------------------------------------------
;;; PgUp/PgDn and the mouse wheel move the transcript window; every other
;;; key stays with the composer. Scrolling to the bottom re-pins the follow;
;;; submitting always re-pins, so the reply is visible.

(defun scroll-transcript (model action &optional (count 1))
  "Apply one scroll ACTION to the transcript window and update the
follow-bottom pin from where the window landed."
  ;; A page is the window's height. An explicit scroll is the operator taking
  ;; the window back from a found-block pin: the pin is spent the moment they
  ;; move it.
  (clear-transcript-pin model)
  (let ((window (chat-model-band model))
        (up (member action '(:page-up :line-up :home))))
    ;; Up is reading intent, even when a retained tool trail just collapsed
    ;; to less than one screen. Grow first, then spend the gesture on those
    ;; rows; a zero local range must not re-pin and strand older history.
    (when up
      (setf (chat-model-follow-bottom model) nil)
      (grow-transcript-projection model :whole (eq action :home)))
    (ecase action
      (:page-up (band-window-scroll window (- window.height)))
      (:page-down (band-window-scroll window window.height))
      (:line-up (band-window-scroll window (- count)))
      (:line-down (band-window-scroll window count))
      (:home (band-window-goto-top window))
      ;; End asks for the newest rows, as pinning the tail does: it spends a
      ;; place held over shrunk content (BAND-WINDOW-HOLD), or End would
      ;; land on the held offset with the rows below it blank, and the next
      ;; notch up would stop short of the history above it.
      (:end (forget-band-window-hold window)
            (band-window-goto-bottom window)))
    (setf (chat-model-follow-bottom model)
          (band-window-at-bottom-p window))
    (unless up (grow-transcript-projection model))))

(defun pin-transcript-bottom (model)
  "Re-pin the follow and snap to the tail."
  ;; Coming home to the tail spends a found-block pin: the operator asked for
  ;; the newest rows.
  (clear-transcript-pin model)
  (setf (chat-model-follow-bottom model) t)
  (forget-band-window-hold (chat-model-band model))
  (band-window-goto-bottom (chat-model-band model)))

;;; --- keyboard list surfaces ------------------------------------------------
;;; The three primitives every keyboard-driven list or field surface below
;;; shares — the composer's two dropdown lanes, the docked picker, and the
;;; /models wizard. Each surface keeps its own bindings and its own refusal
;;; copy; only the arithmetic is shared.

(defun wrap-selection (selection count delta)
  "SELECTION moved by DELTA over COUNT rows with wraparound."
  ;; SELECTION is clamped first, so an index left behind by a list that shrank
  ;; still lands in range; an empty list selects 0.
  (if (plusp count)
      (mod (+ (min selection (1- count)) delta) count)
      0))

(defun selected-of (rows selection)
  "The row SELECTION points at, clamped to ROWS, or NIL when ROWS is empty."
  (when rows
    (nth (min selection (1- (length rows))) rows)))

(defun past-headings (rows at delta &aux (count (length rows)))
  "AT, or the first index from it that is no heading row (PICKER-ROW-HEADING),
stepping DELTA through ROWS with wraparound."
  (loop repeat count
        while (picker-row-heading (nth at rows))
        do (setf at (wrap-selection at count delta)))
  at)

(defun edit-text-field (text key &key ctrl alt max)
  "One keystroke against a one-line text field => the field's new TEXT, or NIL
when the key is none of the field's own."
  ;; The shared edit ladder: Ctrl+U clears, Ctrl+W and Ctrl/Alt+Backspace
  ;; delete the trailing word, Backspace deletes one character, and a bare
  ;; printable character appends while the field is shorter than MAX (NIL for
  ;; no cap). Any other key — and Backspace on an empty field, which must not
  ;; disturb the surface's selection — declines, so the caller falls through
  ;; to its own bindings instead of the field swallowing them. The cleared
  ;; field answers "", which is a field's answer like any other.
  (cond ((and ctrl (characterp key) (char-equal key #\u)) "")
        ((or (and ctrl (characterp key) (char-equal key #\w))
             (and (eq key :backspace) (or ctrl alt)))
         ;; The trailing word goes, and the blanks after it.
         (values (ppcre:regex-replace "[^ \\t]*[ \\t]*\\z" text "")))
        ((eq key :backspace)
         (when (plusp (length text)) (subseq text 0 (1- (length text)))))
        ((and (characterp key) (not ctrl) (not alt) (graphic-char-p key))
         (if (or (null max) (< (length text) max))
             (concatenate 'string text (string key))
             text))))

;;; --- slash dropdown + dispatch --------------------------------------------
;;; The composer's command lane (slash.lisp owns the catalog/parser). The
;;; dropdown is derived from the live composer text every frame — no open/
;;; closed flag to fall out of sync — and only the selection index is stored.
;;; Both shells share these: the gateway model inherits the navigation keys
;;; through CALL-NEXT-METHOD and calls SUBMIT-SLASH from its own Enter branch.

;;; --- keybind consultation ---------------------------------------------------
;;; The update function dispatches through the resolved keymap (keys.lisp)
;;; instead of hardcoded characters, the Zig keybinds.zig resolution.

(defun model-key-matches-p (model action code mod)
  "Whether the pressed CODE+MOD dispatches ACTION under MODEL's keymap."
  (keybind-matches-p (chat-model-keymap model) action code mod))

(defun refresh-model-keymap (model)
  "Re-resolve MODEL's keymap when it was built from another action table than
the one now in force — what makes a keybind row (or an override) defined
into a running image live at the next keystroke."
  ;; One identity check per keystroke otherwise: the resolved keymap carries
  ;; its own source table.
  (unless (eq (gethash +keybind-source-key+ (chat-model-keymap model))
              *keybind-actions*)
    (setf (chat-model-keymap model) (resolve-keybinds))))

(defun composer-empty-p (model)
  "The Zig ComposerState.isEmpty gate: trimmed text empty."
  (zerop (length (nlk:trimmed (editor-value (chat-model-editor model))))))

;;; --- the composer status ------------------------------------------------------
;;; Everything the shell says about what the operator just did or asked -- a
;;; pick landed, a key saved, a command's answer, a prompt the gateway
;;; refused -- stands in the composer's placeholder, where the eye already is,
;;; and nowhere else. A lane of its own above the composer would take the home
;;; screen away and stand until the next prompt, and a failure that toasted
;;; would not show while popups were off. A line said once is news and goes
;;; after five seconds; a status that wraps, and a failure, stand until the
;;; composer is edited, a prompt is sent or another status takes the place. A
;;; failure paints in the danger tone. The status wraps rather than clips, so
;;; a failure is shown whole; one taller than the composer can grow opens as a
;;; list instead (SHOW-STATUS).

(defstruct (composer-status (:constructor make-composer-status (text error-p held draft)))
  "One composer status: its TEXT; ERROR-P, whether it says a failure; HELD,
whether it stands until the composer moves, and DRAFT, the composer's text
when it was said, which an edit moves off; BORN, when it was said."
  text error-p held draft (born (get-internal-real-time)))

(defparameter +selection-copied-hint+ "Copied selection to clipboard"
  "Zig selection_copied_notice, verbatim: said only once a clipboard tool took
the text (NLT:WRITE-CLIPBOARD's (:COMMAND NAME)).")

(defparameter +selection-osc52-hint+
  "Sent to the terminal as OSC 52: no clipboard tool here took it. Paste empty? Turn on OSC 52 in the terminal, or Shift+drag"
  "What a copy says when only the terminal's OSC 52 was left: the sequence was
written, and the terminal may have it off, so it never says `copied'.")

(defparameter +selection-copy-failed-hint+
  "Copy failed: no clipboard path worked here. Shift+drag selects with the terminal's own selection"
  "What a copy says when neither a clipboard tool nor the terminal took it.")

(defparameter +selection-empty-hint+ "Nothing selected to copy"
  "What a copy says with no marked words.")

(defun status-text-lines (text width)
  "TEXT as the composer status shows it at WIDTH columns: its lines, each
wrapped (WRAPPED-LINES), a blank line kept."
  (loop for line in (or (split-lines text) (list ""))
        append (or (wrapped-lines line (max 1 width)) (list ""))))

(defun show-status (model text &key error-p (title "Answer"))
  "Say TEXT in MODEL's composer status, as a failure with ERROR-P; NIL or an
empty TEXT takes the status down. A TEXT taller than the composer can grow
opens as a list titled TITLE while no dialog is open. => NIL."
  (let* ((text (and (stringp text) (string-right-trim '(#\Space #\Tab #\Return #\Newline) text)))
         (lines (and text (plusp (length text))
                     (status-text-lines text (composer-text-width (or (chat-model-term-width model) 80))))))
    (cond ((null lines)
           (setf (chat-model-composer-status model) nil))
          ((and (> (length lines) (composer-max-rows (or (chat-model-term-height model) 24)))
                (null (chat-model-dialog model)))
           (setf (chat-model-composer-status model) nil)
           (open-picker-dialog model title
                               (mapcar (lambda (line) (make-picker-row :label line)) lines)
                               nil
                               :searchable t :search-placeholder "Search"
                               :hint "esc close  ↑↓ navigate  type search"))
          (t
           (setf (chat-model-composer-status model)
                 (make-composer-status text (and error-p t) (and (or error-p (rest lines)) t)
                                       (composer-draft model))))))
  nil)

(defun composer-draft (model)
  "The text in MODEL's composer, or NIL for a model that has none."
  (when-let (editor (chat-model-editor model)) (editor-value editor)))

(defun active-status (model)
  "MODEL's composer status while it stands, else NIL: news for five seconds,
a held one until the composer's text moves off the draft it was said over."
  (when-let (status (chat-model-composer-status model))
    (if (if (composer-status-held status)
            (equal (composer-status-draft status) (composer-draft model))
            (< (since-ms (composer-status-born status)) 5000)) ; Zig composer_notice_visible_ms
        status
        (setf (chat-model-composer-status model) nil))))

(defun status-said (model)
  "The words MODEL's composer status says while it stands, else NIL."
  (when-let (status (active-status model)) (composer-status-text status)))

;;; --- composer autocomplete: the slash and @file lanes -----------------------
;;; The Zig composer_suggestions.zig triggers beside the slash dropdown: `/`
;;; completes catalog commands, `@` workspace paths. Both produce the same
;;; row, so the panel, the ghost, the navigation and the accept below are each
;;; written once. Every list is derived from the live composer text every
;;; frame — no open/closed flag to fall out of sync with — and only the
;;; selection index (the shared SLASH-SELECTION slot) is stored.

(defstruct (composer-suggestion
            (:constructor composer-suggestion (text remainder source
                                               &key detail alias entry)))
  "One dropdown row, whatever lane produced it."
  ;; TEXT is the completion
  ;; target as the row paints it, REMAINDER what the ghost shows after the cursor
  ;; (and what a token lane inserts there), SOURCE :slash or :file, DETAIL the
  ;; description column, ALIAS the command's other triggers painted beside the
  ;; label (slash rows only, empty elsewhere), and ENTRY the catalog
  ;; SLASH-COMMAND the row dispatches on Enter.
  (text "" :type string)
  (remainder "" :type string)
  (source nil)
  (detail "" :type string)
  (alias "" :type string)
  (entry nil))

(nlk:access (suggestion composer-suggestion))

(defun slash-active-suggestions (model &aux (text (editor-value (chat-model-editor model)))
                                            (prefix (subseq text (min 1 (length text)))))
  "The slash lane's rows: every catalog entry the live composer text matches."
  ;; TEXT is the canonical `/name` the label paints and Tab writes; REMAINDER
  ;; completes the trigger the operator is ACTUALLY typing, so a command
  ;; reached through an alias ghosts that alias while accepting it still
  ;; writes the canonical name — the Zig ghost/accept split, kept.
  (mapcar (lambda (entry)
            (let* ((name (slash-command-name entry))
                   (aliases entry.aliases)
                   (target (find-if (lambda (trigger) (slash-name-prefix-p prefix trigger))
                                    (cons name aliases))))
              (composer-suggestion
               (format nil "/~a" name)
               (if target (subseq target (length prefix)) "")
               :slash
               :detail (slash-command-description entry)
               :alias (format nil "~{/~a~^ ~}" aliases)
               :entry entry)))
          (slash-suggestions text)))

(defun submit-slash (model text)
  "Execute the slash command TEXT names."
  ;; The dropdown selection wins while open — Enter on a bare prefix runs the
  ;; highlighted command — and a literal name (typically with arguments)
  ;; parses directly. A known command resets the composer; an unknown name
  ;; keeps the text for editing and explains itself in the composer status.
  ;; Returns (values handled cmd).
  (multiple-value-bind (name args) (parse-slash-input text)
    (let ((entry (or (let ((row (selected-of (slash-active-suggestions model)
                                             (chat-model-slash-selection model))))
                       (and row (composer-suggestion-entry row)))
                     (find-slash-command name))))
      (setf (chat-model-slash-selection model) 0)
      (cond (entry
             (editor-reset (chat-model-editor model))
             ;; What stood there before is over; an op that says its own status
             ;; (/fork, /themes NAME) answers NIL, and its line stands.
             (show-status model nil)
             (multiple-value-bind (notice cmd)
                 (execute-slash-command model (slash-command-name entry) args)
               ;; The answer stands in the composer status.
               (when notice
                 (show-status model notice :title (format nil "/~a~@[ ~a~]" (slash-command-name entry)
                                                          (json-value args :text))))
               (render-state model)
               (values t cmd)))
            (t (show-status model (format nil "unknown command /~a — /help lists commands" name)
                            :error-p t)
               (render-state model)
               (values t nil))))))

(defun composer-token-filter (text trigger)
  "The bare token after the last TRIGGER character, or NIL once whitespace
follows it (Zig composerFileMentionFilterAtCursor with the cursor at end of
text)."
  (when-let (pos (position trigger text :from-end t))
    (let ((filter (subseq text (1+ pos))))
      (unless (find-if #'blank-char-p filter)
        filter))))

(defun token-suggestion (filter target source)
  "One suggestion when FILTER prefixes TARGET (Zig prefix + case rules:
strict case-insensitive prefix, or full-length case-insensitive equality)."
  ;; The detail column is the lane's own name, which is what the Zig
  ;; ComposerDropdownItem prints for a catalog entry carrying no description
  ;; of its own; a token row leaves the alias column empty.
  (when (slash-name-prefix-p filter target)
    (composer-suggestion target (subseq target (length filter)) source
                         :detail (string-downcase source))))

(defun token-suggestions-for (model &aux (text (editor-value (chat-model-editor model))))
  "The active @file suggestion list, or NIL."
  ;; The slash lane owns `/` inputs outright (Zig
  ;; composerSuggestionsForComposerText order); the list exists only while the
  ;; caret sits at the end of the text (the Zig composerSuggestionList cursor
  ;; gate), so completions always land on the trigger token. The catalog is
  ;; walked off this path: an unloaded one asks for the walk
  ;; (START-COMPOSER-CATALOG-WALK) and this reads whatever has landed, so no
  ;; keystroke ever waits on the filesystem.
  (unless (or (slash-input-p text)
              (not (composer-cursor-at-end-p (chat-model-editor model))))
    (when-let (filter (composer-token-filter text #\@))
      (unless (chat-model-file-catalog-loaded model)
        (start-composer-catalog-walk model))
      (loop for path in (chat-model-file-catalog model)
            when (token-suggestion filter path :file)
              collect it))))

(defun composer-trigger-command (model msg &aux (editor (chat-model-editor model))
                                                (key (nlt:key-event-code msg)))
  "The cell command whose TRIGGER the key MSG just typed at a word's
start, or NIL."
  ;; The core's `$' opens /index here, so a definition is named from the
  ;; index's own list. Only at the end of a draft that is no slash command,
  ;; and only where the character starts a word: `a$b' and `/x $' stay text.
  (when (and (characterp key) (composer-cursor-at-end-p editor))
    (let* ((text (editor-value editor))
           (end (length text)))
      (when (and (plusp end) (char= key (char text (1- end)))
                 (not (slash-input-p text))
                 (or (= end 1) (blank-char-p (char text (- end 2)))))
        (find (string key) *gateway-commands*
              :key #'slash-command-trigger :test #'equal)))))

(defun composer-suggestions-for (model)
  "The live dropdown's rows, from whichever lane matches: the slash catalog
owns `/` inputs outright (Zig composerSuggestionsForComposerText order) and
the token lanes take everything else."
  ;; NIL is also every caller's reading of `no dropdown is open`.
  (or (slash-active-suggestions model)
      (token-suggestions-for model)))

(defun selected-composer-suggestion (model)
  "The highlighted row of whichever lane is live, else NIL."
  (selected-of (composer-suggestions-for model)
               (chat-model-slash-selection model)))

(defun accept-suggestion (model)
  "Complete the highlighted row (Zig acceptComposerInlineSuggestion)."
  ;; A slash row rewrites the whole composer with the canonical name plus a
  ;; trailing space, so the caret lands at the end and the whitespace closes
  ;; the dropdown (whitespace ends name matching); a token row inserts its
  ;; remainder plus a space at the cursor. => T when a completion landed.
  (when-let (suggestion (selected-composer-suggestion model))
    (let ((editor (chat-model-editor model)) (slash (eq :slash suggestion.source)))
      (when slash (editor-reset editor))
      (editor-insert editor (uiop:strcat (if slash suggestion.text suggestion.remainder) " ")))
    (setf (chat-model-slash-selection model) 0)
    t))

(defun accept-token-suggestion-for-submit (model)
  "Enter over an open TOKEN dropdown completes instead of submitting, but
only when the completion would actually extend the text (Zig
acceptComposerTokenSuggestionForSubmit)."
  ;; The slash lane is not this lane: Enter there dispatches the command.
  (let ((suggestion (selected-composer-suggestion model)))
    (when (and suggestion (not (eq :slash suggestion.source)) (plusp (length suggestion.remainder)))
      (accept-suggestion model))))

(defun composer-dropdown-key (model key &aux (count (length (composer-suggestions-for model)))
                                             (selection (chat-model-slash-selection model)))
  "One dropdown navigation key over whichever completion lane is live."
  ;; Every lane shares the selection slot and the row type, so they share the
  ;; whole ladder: :up/:down move the selection with wraparound and :tab
  ;; accepts the highlighted row. => T when a live lane took the key.
  (when (plusp count)
    (if (eq key :tab)
        (accept-suggestion model)
        (setf (chat-model-slash-selection model)
              (wrap-selection selection count (ecase key (:up -1) (:down 1)))))
    t))

(defun composer-ghost-remainder (model &aux (ta (chat-model-editor model)))
  "The inline dim completion after the cursor: the selected row's remainder,
only while the caret sits at the end of the text (the Zig
composerSuggestionList cursor-at-end gate). NIL when nothing completes."
  (when (and (plusp (length (editor-value ta)))
             (composer-cursor-at-end-p ta))
    (let ((suggestion (selected-composer-suggestion model)))
      (and suggestion (json-value suggestion.remainder :text)))))

;;; --- the composer dropdown panel ---------------------------------------------
;;; The Zig composer dropdown (composer.zig renderDropdownBody): one row
;;; shape for the slash and @file lanes, docked BELOW the composer
;;; box — the composer lifts and the transcript band yields the rows — on
;;; the composer's own content surface with the box's two-column canvas
;;; margins. Rows have no selection bar: the completion target always
;;; paints in the composer token style (composerMarkerStyle) and selection
;;; brightens the description column from muted to the surface foreground.
;;; The token style is :syntax-command, not the Zig :syntax-builtin: every
;;; bundled theme keeps builtin green while the rest of the palette runs
;;; warm (Tungstene: builtin #65a30d against accents #fca864/#fa8b3e), so
;;; the green lane read as off-theme chrome. :syntax-command is the warm
;;; command-word tone the shell lexer already paints (#fdba74), and a
;;; completion target is a command word. A list longer than the window grows
;;; the Zig scrollbar lane at the rect's right edge, and the panel ends in
;;; one surface-filled padding row (composer_dropdown_rows = visible + 1).

(defun composer-dropdown-geometry (model)
  "=> (values PANEL-ROWS VISIBLE-COUNT TOTAL-COUNT) for the active
dropdown, or NIL."
  ;; PANEL-ROWS counts the visible entries plus the bottom padding row (Zig
  ;; composer_dropdown_rows = visible + 1), clamped so the transcript band
  ;; keeps a row. NIL while no lane matches or while a docked dialog owns the
  ;; bottom of the shell — a dialog outranks the composer for keys, so a stale
  ;; dropdown under it would be chrome without a gesture.
  (unless (chat-model-dialog model)
    (let ((count (length (composer-suggestions-for model))))
      (when (plusp count)
        (let* ((height (chat-model-term-height model))
               (room (max 0 (- height (shell-overhead height (composer-visible-rows model)) 1)))
               (panel (min (1+ (min count 8)) room))) ; 8 rows windowed on the selection
          (when (>= panel 2)
            (values panel (1- panel) count)))))))

(defun dropdown-label-width (suggestion)
  "Display columns SUGGESTION spends before the detail column: the
completion target plus the parenthesized alias (Zig
dropdownLabelWidthForItem)."
  (+ (text-display-width suggestion.text)
     (if (plusp (length suggestion.alias))
         (+ 2 (text-display-width suggestion.alias) 1)
         0)))

(defun composer-suggestion-hotkey (model suggestion)
  "The chord SUGGESTION's command also answers to, or NIL when it has none —
the / dropdown's hotkey lane (SLASH-COMMAND-KEYBIND)."
  ;; Read through MODEL's own resolved keymap, so an override in keybinds.json
  ;; shows the chord the operator actually has and a command whose action was
  ;; unbound shows nothing: a hotkey exists here only when a key really fires
  ;; it. NIL for a lane that is no command at all (@file), which is
  ;; every row without a catalog entry.
  (when-let (action (and suggestion.entry (slash-command-keybind suggestion.entry)))
    (keybind-chords-label (chat-model-keymap model) action :unbound nil)))

(defun dropdown-hotkey-lane (model suggestion key-width)
  "The row's right lane for SUGGESTION, or NIL when its command has no
chord: the label padded to KEY-WIDTH so every chord in the window starts at
the same column."
  ;; The lane is painted flush right, so that shared start falls out of the
  ;; lane's own width — one column for the whole window, never one per row.
  (when-let (label (composer-suggestion-hotkey model suggestion))
    (list (seg (pad-frame-row label (text-display-width label) key-width nil) :muted))))

(defun dropdown-panel-row (segs width bg surface &key scrollbar right)
  "One full-WIDTH dropdown row on the composer box's columns: its canvas
margins either side — painted with the box's own bg, not the dropdown surface —
around a DOCKED-PANEL-ROW with no side padding and the 2 + 1 + 2 scrollbar lane
the dropdown reserves (*SCROLLBAR-RESERVED-COLUMNS*, Zig dropdownItemWindow +
renderDropdownScrollbar)."
  ;; A description too long for the row ends in `…' at the cut; without this
  ;; it would stop mid-word at the edge.
  (let* ((box (composer-box-width width))
         (inner (docked-panel-row segs box surface
                                  :col 0 :pad-left 0 :pad-right 0
                                  :bar-gap 2 :scrollbar scrollbar
                                  :right right :mark "…"))
         (row (concatenate 'string
                           (blank-frame-row 2 bg)
                           inner
                           (blank-frame-row (- width 2 box) bg))))
    ;; The panel row's targets stand two columns in on the frame's row.
    (if-let (links (plain-row-links inner))
      (link-plain-row (nlk:narrow-string row)
                      (loop for (start end target) in links
                            collect (list (+ start 2) (+ end 2) target)))
      row)))

(defun composer-dropdown-panel-rows (model width bg)
  "The active dropdown's panel rows: the windowed entries plus the bottom
padding row, all on the composer surface (Zig renderDropdownBody fills the
whole dropdown rect). NIL while no lane matches or the window has no room."
  (multiple-value-bind (panel visible count)
      (composer-dropdown-geometry model)
    (when panel
      (bind ((entries (composer-suggestions-for model))
             (selection (min (chat-model-slash-selection model) (1- count)))
             (top (max 0 (min (- count visible) (1+ (- selection visible)))))
             (window (subseq entries top (+ top visible)))
             (label-width (reduce #'max window
                                  :key #'dropdown-label-width
                                  :initial-value 0))
             ;; The hotkey column: the chords of the commands in this window
             ;; ("/sessions Ctrl+S"), one shared width so every chord starts
             ;; at the same column and the description column reads the same
             ;; on every row. A window with no chords spends no columns —
             ;; the lane disappears rather than reserving blank space.
             (hotkey-width (loop for entry in window
                                 for label = (composer-suggestion-hotkey model entry)
                                 maximize (if label (text-display-width label) 0)))
             ;; The dropdown paints on the composer's content surface —
             ;; :user-block, the tone the box itself flips to once the
             ;; operator types, and a dropdown only exists over typed text
             ;; (composer.zig composerContentTone) — or NIL under :none.
             (surface (unless (eq (effective-color-profile) :none)
                        (visual-style-bg (style-for-tone :user-block))))
             (bar-p (> count visible))
             ;; The dropdown's bar spans its own window, so the track is
             ;; also the visible count (Zig dropdownScrollbarThumb).
             ((thumb-top thumb-height) (when bar-p (scrollbar-thumb visible visible count top))))
        (append
         (loop for entry in window
               for row from 0
               for selected = (= (+ top row) selection)
               for alias = (composer-suggestion-alias entry)
               for gap = (+ 2 (- label-width (dropdown-label-width entry)))
               ;; A row is the pointer's to choose, as Enter chooses the
               ;; highlighted one (HANDLE-MOUSE-RELEASE).
               for target = (cons :suggestion (+ top row))
               collect (dropdown-panel-row
                        (append
                         (list (make-seg :text (if (and (null surface) selected) "> " "  ")
                                         :tone :user-block :link target)
                               (make-seg :text (composer-suggestion-text entry)
                                         :tone :syntax-command :link target))
                         (when (plusp (length alias))
                           (list (make-seg :text (format nil " (~a)" alias) :tone :muted :link target)))
                         ;; Two columns past the window's widest label, the
                         ;; docked rows' separator: at one, the widest row's
                         ;; alias would run into its description, `/tree
                         ;; (/checkpoints) Browse'.
                         (list (make-seg :text (format nil "~va" gap "") :tone :user-block :link target)
                               (make-seg :text (composer-suggestion-detail entry)
                                         :tone (if selected :user-block :muted) :link target)))
                        width bg surface
                        ;; The chord this command also answers to, parked
                        ;; at the row's right edge — past the description,
                        ;; the way a menu names a command's key.
                        :right (dropdown-hotkey-lane model entry hotkey-width)
                        :scrollbar (scrollbar-cell thumb-top thumb-height row)))
         ;; The panel's own bottom padding row: surface fill, no
         ;; scrollbar cell (Zig draws the bar on the row window only).
         (list (dropdown-panel-row '() width bg surface)))))))

;;; --- paste capture and image attachments -------------------------------------
;;; The Zig attachComposerPaste / attachComposerImage pair: large pastes
;;; collapse to a [Paste #N] marker re-expanded at submit; Ctrl+V probes the
;;; system clipboard for an image and stages it as a pending attachment.

(defun attach-composer-paste (model text &aux (normalized (normalize-pasted-text (or text ""))))
  "Insert pasted TEXT: verbatim when small, collapsed to a `[Paste #N] `
marker with the content stashed for submit-time expansion when large."
  ;; => T when anything landed.
  (when (plusp (length normalized))
    ;; Past either bound the paste collapses (Zig
    ;; shouldCollapseComposerPaste).
    (if (or (> (length normalized) *paste-collapse-max-chars*)
            (> (1+ (count #\Newline normalized)) *paste-collapse-max-lines*))
        (let ((id (incf (chat-model-paste-counter model))))
          (editor-insert
           (chat-model-editor model)
           (format nil "[Paste #~d] " id))
          (push (cons id normalized) (chat-model-pastes model)))
        (editor-insert
         (chat-model-editor model) normalized))
    t))

(defun expand-composer-pastes (model prompt)
  "PROMPT with every stored `[Paste #N]` marker replaced by its captured
content; markers without a store pass through (Zig
expandComposerPasteMarkers)."
  ;; A marker matches a store entry only as that entry spells it, so
  ;; `[Paste #03]' is text, not paste 3.
  (ppcre:regex-replace-all "\\[Paste #\\d+\\]" prompt
                           (lambda (marker)
                             (or (cdr (find marker (chat-model-pastes model)
                                            :key (lambda (paste)
                                                   (format nil "[Paste #~d]" (car paste)))
                                            :test #'string=))
                                 marker))
                           :simple-calls t))

(defun clear-composer-attachments (model)
  "Drop stored pastes and staged images after a submit consumed them."
  (setf (chat-model-pastes model) nil
        (chat-model-pending-images model) nil))

(defun command-output (argvs &key stripped)
  "The output of the first of ARGVS that exits 0 with some, or NIL: each an
argv run through RUN-BOUNDED, its output without its final newline when
STRIPPED."
  ;; The clipboard readers' ladder: the first helper this box has answers.
  (dolist (argv argvs)
    (bind (((output _ status)
            (ignore-errors (run-bounded argv :seconds 10 :error-output nil))))
      (let ((output (and (eql status 0) (stringp output)
                         (if stripped (string-right-trim '(#\Newline #\Return) output) output))))
        (when (plusp (length output))
          (return output))))))

(defparameter *mac-clipboard-png*
  "ObjC.import('AppKit');
var p = $.NSPasteboard.generalPasteboard, d = p.dataForType('public.png');
if (d.isNil()) { var t = p.dataForType('public.tiff');
  if (!t.isNil()) d = $.NSBitmapImageRep.imageRepWithData(t).representationUsingTypeProperties($.NSBitmapImageFileTypePNG, $({})); }
d.isNil() ? '' : d.base64EncodedStringWithOptions(0).js"
  "The Mac pasteboard's image as base64 PNG, through osascript's JavaScript:
a screenshot lands there as PNG, an image copied from Preview or a page as
TIFF, which is re-encoded.")

(defparameter *windows-clipboard-image*
  "$ProgressPreference = 'SilentlyContinue'
$ErrorActionPreference = 'Stop'
[void][Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms')
[void][Reflection.Assembly]::LoadWithPartialName('System.Drawing')
$types = @{ '.png' = 'image/png'; '.jpg' = 'image/jpeg'; '.jpeg' = 'image/jpeg'; '.gif' = 'image/gif'; '.webp' = 'image/webp' }
$d = [Windows.Forms.Clipboard]::GetDataObject()
if ($d -eq $null) { exit 1 }
$png = $d.GetData('PNG')
if ($png -is [IO.MemoryStream]) { 'image/png ' + [Convert]::ToBase64String($png.ToArray()); exit 0 }
if ($d.GetDataPresent([Windows.Forms.DataFormats]::FileDrop)) {
  foreach ($f in $d.GetData([Windows.Forms.DataFormats]::FileDrop)) {
    $t = $types[[IO.Path]::GetExtension($f).ToLowerInvariant()]
    if ($t) { $t + ' ' + [Convert]::ToBase64String([IO.File]::ReadAllBytes($f)); exit 0 }
  }
}
$i = [Windows.Forms.Clipboard]::GetImage()
if ($i -ne $null) { $m = [IO.MemoryStream]::new(); $i.Save($m, [Drawing.Imaging.ImageFormat]::Png); 'image/png ' + [Convert]::ToBase64String($m.ToArray()); exit 0 }
exit 1"
  "The Windows clipboard's image as one `MEDIA-TYPE BASE64' line, through
Windows PowerShell: the PNG a browser or the snipping tool puts there, else
the first image file copied in Explorer, else the bitmap a screenshot key
leaves, re-encoded as PNG. Nothing is printed, and the exit is 1, when the
clipboard holds no image. No cmdlet runs, so no module loads: a cold run
is the .NET start alone, about a fifth of a second.")

(defun windows-clipboard-image-argv ()
  "The argv that runs *WINDOWS-CLIPBOARD-IMAGE*: Windows PowerShell, which
every Windows box carries, single-threaded as the clipboard requires, the
script passed as -EncodedCommand (UTF-16LE base64) so no quote of it meets
the command line."
  (list "powershell.exe" "-NoProfile" "-NonInteractive" "-STA" "-EncodedCommand"
        (cl-base64:usb8-array-to-base64-string
         (sb-ext:string-to-octets *windows-clipboard-image* :external-format :utf-16le))))

(defun clipboard-image-line (line)
  "LINE as *WINDOWS-CLIPBOARD-IMAGE* prints it => (values MEDIA-TYPE BASE64),
or NIL when it names no image type or carries no payload."
  (when-let (space (and line (position #\Space line)))
    (let ((media-type (subseq line 0 space))
          (base64 (subseq line (1+ space))))
      (when (and (member media-type '("image/png" "image/jpeg" "image/webp" "image/gif")
                         :test #'string=)
                 (plusp (length base64)))
        (values media-type base64)))))

(defun read-clipboard-image-base64 ()
  "=> (values MEDIA-TYPE BASE64) for the system clipboard's image, or NIL."
  ;; Probes the same Wayland/X11 helpers as the Zig readImageCommand,
  ;; capturing through `base64` so the payload never transits a character
  ;; decode; a Mac has neither, and answers PNG from its pasteboard; Windows
  ;; answers the type and the payload together, in one PowerShell run.
  #+win32
  (clipboard-image-line (command-output (list (windows-clipboard-image-argv)) :stripped t))
  #-win32
  (dolist (media-type '("image/png" "image/jpeg" "image/webp" "image/gif")) ; Zig image_media_types
    (when-let (output (command-output
                       #+darwin (and (equal media-type "image/png")
                                     (list (list "osascript" "-l" "JavaScript"
                                                 "-e" *mac-clipboard-png*)))
                       #-(or win32 darwin)
                       (list (shell-argv (format nil "wl-paste --type ~a | base64 -w0" media-type))
                             (shell-argv (format nil "xclip -selection clipboard -t ~a -o ~
                                                      | base64 -w0"
                                                 media-type)))
                       :stripped t))
      (return (values media-type output)))))

(defun stage-composer-image (model media-type base64)
  "Stage one image as a pending attachment of MODEL's next submit, behind an
`[Image #N] ' marker typed where the cursor is. => T."
  (editor-insert (chat-model-editor model)
                 (format nil "[Image #~d] "
                         (1+ (length (chat-model-pending-images model)))))
  (alexandria:appendf (chat-model-pending-images model)
                      (list (cons media-type base64)))
  t)

(defun paste-clipboard-image (model)
  "Stage the system clipboard's image in MODEL's composer. => T when there
was one."
  (multiple-value-bind (media-type base64) (read-clipboard-image-base64)
    (when media-type
      (stage-composer-image model media-type base64))))

(defun pasted-image-file (text)
  "TEXT, a paste, when it is nothing but the path of an image file on this
machine => (values MEDIA-TYPE BASE64), else NIL."
  ;; What Windows Terminal pastes for a snip (Win+Shift+S, PrintScreen) or an
  ;; image copied in Explorer -- the clipboard carries the file too, and the
  ;; terminal pastes files as their paths -- and what a file dragged onto a
  ;; terminal types. The path may come quoted; dragged onto a POSIX terminal,
  ;; its spaces may come backslashed. The type is the file's own bytes, and a
  ;; file past the provider's ceiling stays a path.
  (let* ((path (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (path (if (and (>= (length path) 2)
                        (find (char path 0) "\"'")
                        (char= (char path 0) (char path (1- (length path)))))
                   (subseq path 1 (1- (length path)))
                   path)))
    (when (and (< 0 (length path) 4096)
               (not (find #\Newline path))
               (some (lambda (extension)
                       (let ((end (- (length path) (length extension))))
                         (and (plusp end) (string-equal extension path :start2 end))))
                     '(".png" ".jpg" ".jpeg" ".gif" ".webp")))
      (dolist (candidate (remove-duplicates
                          (list path #-win32 (ppcre:regex-replace-all "\\\\(.)" path "\\1"))
                          :test #'equal))
        (let* ((octets (ignore-errors
                        (nlt:file-octets (uiop:parse-native-namestring candidate) *image-max-bytes*)))
               (media-type (nlt:image-media-type octets)))
          (when media-type
            (return (values media-type (cl-base64:usb8-array-to-base64-string octets)))))))))

;;; --- the docked dialog panel -------------------------------------------------
;;; One chrome for every dialog, mirroring src/tui/dialog.zig
;;; computeDockedDialog plus the dialogs.zig renderDockedPanel family. The
;;; panel docks at the bottom of the chat content area — over the composer,
;;; not above it, because Zig draws it after the composer onto the same rows
;;; (ui.zig ChatShell.render order) — and paints one composer-toned surface:
;;; content inset 3 columns from the left and 4 from the right, a four-row
;;; header (the tab row, a blank, the search-or-summary row, a blank), the
;;; list body with its scrollbar in the padded rect's last column, then a
;;; three-row footer whose middle row carries the key hints. The area's own
;;; last row stays canvas: it is the gap Zig leaves between the docked panel
;;; and the shell footer.
;;;
;;; Selection is tone, not a marker and not a highlight bar: the selected row
;;; prints in the composer tone and every other row in the system tone, and
;;; every tone is forced onto the panel surface (Zig shortcutPickerStyle), so
;;; the panel reads as a single surface.
;;;
;;; The dialog is model state (unlike the derived dropdown) because its rows
;;; come from a control-op response, not from the live composer text.

(defparameter *dialog-frame-max-height* 20
  "Tallest docked panel (Zig dialog_frame_max_height).")

(defparameter *docked-dialog-footer-rows* 3
  "Footer rows a docked panel reserves (Zig docked_dialog_footer_rows).")

(defparameter *docked-panel-pad-left* 3
  "Columns of panel fill left of the content (Zig computeDockedDialog padding).")

(defparameter *docked-panel-pad-right* 4
  "Columns of panel fill right of the content.")

;;; The tab row alone prints at column 0.
(defparameter *docked-panel-content-col* 1
  "Content column inside the padded rect (Zig docked_panel_content_col).")

(defparameter *docked-panel-search-row* 2
  "Header row carrying the search or summary text (Zig
docked_panel_search_row).")

(defparameter *dialog-row-overflow-mark* " … "
  "What a dialog entry too long for its row wears at the cut: a 1-width gap,
the triple dot (one character and one column wide, U+2026), then a 1-width
pad, so a row that runs into the right edge says it continues without the
text or the dots sitting flush against the edge.")

(defstruct picker-row
  "One selectable dialog entry."
  ;; KEY is the optional leading column (the Zig DialogListItem
  ;; key/label/detail triple); empty
  ;; for the ordinary two-column rows. SUFFIX is selected-row-only chrome
  ;; appended after DETAIL — the Zig /tree files preview, which only the
  ;; highlighted row carries. MARK is a status glyph painted ahead of
  ;; everything in its own MARK-TONE — the one column a row colors by what it
  ;; says rather than by whether it is selected. A HEADING row names the
  ;; rows under it and is never selected: the arrows, a page, the wheel and the
  ;; pointer pass over it, and a search hides it. PATH is selected-row-only
  ;; too: the folder the row is about (a session's working directory), which
  ;; the paint writes as the operator reads it and clips from the middle to
  ;; the room the row leaves, so its last folder shows. TONE, when given,
  ;; paints the row's label whether or not it is selected: a line that says
  ;; something went wrong reads so on either (the exec view's error).
  (key "" :type string)
  (label "" :type string)
  (detail "" :type string)
  (suffix "" :type string)
  (path "" :type string)
  (mark "" :type string)
  (mark-tone :muted)
  (tone nil)
  (value nil)
  (heading nil))

(nlk:access (row picker-row))

(nlk:define-side-table picker-row-prefix (:weakness :key)
  "A row's live-only prefix: (FIELD OFFSET TEXT), drawn muted before that
field's character at OFFSET. The row's raw label/key remains unchanged.")

(nlk:define-side-table picker-row-paint (:weakness :key)
  "A row that lays itself: ROW -> a function of (SELECTED ROOM) answering the
row's segments, at most ROOM columns from the panel's content edge, and the
tone whose ground the row stands on, or NIL for the panel's. Its slots stay
what a search reads and a test names; the function is only the look.")

(defun prefix-picker-row (row prefix &key (field :label) (offset 0))
  "Give ROW an explicit muted PREFIX in FIELD at OFFSET, returning ROW."
  (let ((text (ecase field (:label row.label) (:key row.key))))
    (assert (<= 0 offset (length text)))
    (setf (picker-row-prefix row) (list field offset (concatenate 'string prefix "  "))))
  row)

(defun picker-field-segs (row field text tone &aux (prefix (picker-row-prefix row)))
  "TEXT's segments in TONE, with ROW's explicit prefix in the muted tone."
  (if (and prefix (eq field (first prefix)))
      (let ((offset (second prefix)))
        (append (when (plusp offset) (list (seg (subseq text 0 offset) tone)))
                (list (seg (third prefix) :muted) (seg (subseq text offset) tone))))
      (list (make-seg :text text :tone tone))))

(defstruct picker-dialog
  "A docked list dialog."
  ;; ON-SELECT is called with (MODEL VALUE) for the chosen row and returns a
  ;; notice string or NIL; the dialog closes first so a handler can
  ;; immediately open a successor dialog. ON-DETAIL is the same contract on
  ;; the detail gesture (Right / i, the Zig isDialogDetailKey) — the cells
  ;; dialog's drill into one cell's facts.
  ;;
  ;; TITLE is the tab label the header row highlights and CONTEXT the muted
  ;; label beside it (the Zig renderDockedTabRow pair: " Sessions " plus "
  ;; Gateway"). SUBTITLE is the header's third row on a dialog with no search
  ;; field, the slot the search input owns otherwise. SUMMARY-ROWS, painted
  ;; rows (lists of segs) already fitted to the panel, stand in that row's
  ;; place as many rows as they are: the exec view's command, its lines kept.
  (title "" :type string)
  (context "" :type string)
  (subtitle "" :type string)
  (summary-rows '() :type list)
  (rows '() :type list)
  (selection 0 :type integer)
  ;; The list window's first visible row. The window used to be derived from
  ;; SELECTION alone, which cannot express a mouse scroll: the wheel moves the
  ;; window while the highlight stays on the row the pointer is on, and one
  ;; index cannot say both. DIALOG-BODY-VIEWPORT settles this on every paint
  ;; and every hit-test, so the two can never disagree about which rows show
  ;; — the Zig activeDialogViewportOffset, which its paged dialogs carry per
  ;; items view.
  (view-offset 0 :type integer)
  (on-select nil)
  ;; ON-HIGHLIGHT is called with (MODEL ROW) whenever the selection lands
  ;; on a row — the Zig selection-change lazy-fetch seam (the /tree
  ;; checkpoint files arm). Handlers own their idempotence. Delivery is
  ;; deferred: selection moves stage the row in PENDING-HIGHLIGHT and the
  ;; frame-clock tick delivers it (PICKER-FLUSH-HIGHLIGHT); only the
  ;; open-time initial highlight runs synchronously.
  (on-highlight nil)
  ;; The row awaiting ON-HIGHLIGHT delivery, or NIL. A burst of moves —
  ;; arrow repeat, search narrowing, hover sweep — overwrites this slot and
  ;; the tick delivers only the final row, once it has rested
  ;; *PICKER-HIGHLIGHT-REST* (SINCE is the stamp of the staged row's
  ;; arrival; restaging the same row keeps its clock running).
  (pending-highlight nil)
  (pending-highlight-since 0 :type integer)
  ;; When the last highlight actually delivered — the throttle's anchor:
  ;; during continuous movement the pending row still delivers once
  ;; *PICKER-HIGHLIGHT-THROTTLE* has elapsed since this stamp, so previews
  ;; follow the selection instead of waiting for a full stop.
  (last-highlight-at 0 :type integer)
  ;; ON-CANCEL is called with (MODEL) when Esc dismisses the dialog without
  ;; a selection — the Zig cancelThemePreviewSession seam: a dialog whose
  ;; highlight handler previews live state restores it here.
  (on-cancel nil)
  (on-detail nil)
  ;; HINT overrides the derived footer line while rows exist — dialogs
  ;; with bespoke key copy (the restore-checkpoint confirm stage). It is the
  ;; Zig renderDockedFooterText flavour: one muted line.
  (hint nil)
  ;; FOOTER-HINTS is the other Zig footer flavour (renderDockedKeyHintFooter):
  ;; a dialog kind whose DIALOG-PANEL-ROWS (KEY . LABEL) conses paint as
  ;; alternating system/muted segments, or those conses themselves, for a
  ;; dialog whose keys change while it is open (the exec view). It outranks
  ;; HINT and the derived line when present.
  (footer-hints nil :type (or symbol cons))
  ;; Type-to-filter: SEARCHABLE dialogs route printable keys into SEARCH,
  ;; and the visible rows are the subset that answers it, ranked best-first
  ;; (PICKER-VISIBLE-ROWS over the fuzzy matcher in keys.lisp).
  (searchable nil)
  (search "" :type string)
  (search-placeholder "" :type string)
  ;; The body's own copy while nothing matches (Zig DialogListView
  ;; empty_label), not a footer line: the hint row keeps its key copy.
  (empty-label "No items" :type string)
  ;; The distinguished copy while a live query hid every row (the Zig
  ;; "No matching sessions" / "No checkpoints match this filter" swap —
  ;; a dead end and a keystroke are different situations). NIL keeps
  ;; EMPTY-LABEL for both.
  (no-match-label nil)
  ;; Rows carry the two-column `> ` gutter on every profile, not only under
  ;; :none — the Zig option-row look (renderCheckpointRestoreOptionRow).
  (option-marker nil)
  ;; The DockedDialogOptions override: a three-row header drops the
  ;; blank under the summary row, which is the shape the answer-demanding
  ;; panels use (Zig's sudo prompt).
  (header-rows nil)
  ;; The Zig renderTitledDockedHeader tone pair. A :composer title is the
  ;; tab-row look and drops bold to match the chrome; an :approval title is
  ;; the answer-demanding look and keeps it. The summary tone is :muted on a
  ;; picker and :system where the third row is a live prompt, not a label.
  (title-tone :dialog-accent)
  (summary-tone :muted)
  ;; What the dialog is, for a handler that must find its own dialog after
  ;; the fact (the /models refresh repaint) without matching the title.
  (kind nil)
  ;; ON-TICK is called with (MODEL DIALOG) on every frame-clock tick while
  ;; the dialog is open, and its presence is what keeps the clock running
  ;; over an idle shell: a dialog whose rows carry clocks (the /sessions
  ;; running clock) repaints them here. Handlers own their own pacing.
  (on-tick nil)
  ;; ON-DELETE is called with (MODEL VALUE) for the highlighted row on the
  ;; delete gesture (Delete, Ctrl+X); the dialog stays open, so the handler
  ;; that removed the row refreshes it. NIL leaves the gesture unbound.
  (on-delete nil)
  ;; --- the checklist mode ---
  ;; ON-TOGGLE makes the dialog a checklist: Space toggles the highlighted
  ;; row's value in MARKS and every row's label carries its mark (`[x] ',
  ;; `[ ] '), and Enter hands ON-SELECT the whole MARKS list instead of one
  ;; row's value. ON-TOGGLE is called with (MODEL VALUE MARKED-P) after
  ;; each toggle; the dialog stays open.
  (on-toggle nil)
  (marks '() :type list)
  ;; --- the input mode ---
  ;; INPUT-LABEL non-NIL puts one text field in the body in place of the
  ;; list (the Zig renderConnectDockedInput row): INPUT is its text,
  ;; INPUT-SECRET-P masks it, INPUT-CONTEXT is the muted rule line under
  ;; it. Printable keys edit INPUT uncapped; Enter calls ON-INPUT with
  ;; (MODEL TEXT), Esc with (MODEL NIL) — a dialog without ON-INPUT leaves
  ;; the field on Esc. The /models wizard's two text steps ride this.
  (input-label nil)
  (input "" :type string)
  (input-secret-p nil)
  (input-context "" :type string)
  (on-input nil))

(nlk:access (dialog picker-dialog))

;;; The wizard IS a picker dialog. It occupies the same model slot, shows the
;;; same list body, and owns the same title/rows/search/selection state, so it
;;; INCLUDES the picker rather than restating those slots beside it — the panel
;;; below reads them through the picker's own accessors, and the wizard reads
;;; the very same slots through its own conc-name. What is genuinely the
;;; wizard's own is the step ladder in the /models section — and even the
;;; chrome that moves with the step is a picker slot, written from
;;; +CONNECT-STEPS+ on every transition (CONNECT-ENTER-STEP) rather than
;;; re-derived by a panel that would have to know a wizard from a picker.
(defstruct (connect-dialog (:include picker-dialog))
  "The /models wizard state."
  ;; STEP is :provider, :custom-provider, :api-key, or :result; the provider
  ;; PICKER-ROWs live in the inherited ROWS slot (values are plists (:provider
  ;; id) or (:custom t)), which PROVIDER-ROWS, a function of no
  ;; arguments, answers anew at open and after each saved key; the two text
  ;; steps ride the picker's own input mode (INPUT-LABEL / INPUT), cleared
  ;; whenever the flow leaves the step — the Zig zero-on-clear discipline,
  ;; minus the byte scrubbing CL cannot promise. CUSTOM-P is also the result's
  ;; return route: a typed provider id returns to its own step, a picked one
  ;; to the key step. The result is a failure's: a saved key returns to the
  ;; list, and Enter on a connected provider runs ON-SUCCESS.
  (step :provider)
  (provider-rows nil)
  (provider-id "" :type string)
  (provider-label "" :type string)
  (custom-p nil)
  (result-message "" :type string)
  (on-save nil)
  (on-success nil)
  ;; The context label every step wears instead of its own while a walk
  ;; runs the wizard (`Setup · step 1/3'), and the page that makes the
  ;; picked provider's key, when the rows know one.
  (walk nil)
  (key-page nil)
  ;; When the checking step began (internal real time), for its clock; and
  ;; the key a result keeps while nothing listened where it was checked,
  ;; so Enter checks the same key again once the server is up.
  (checking-since nil)
  (retry nil))

;;; A sheet stands in the middle of the frame over the shell as it is -- the
;;; transcript and the composer behind it, dimmed -- where a docked dialog
;;; takes the composer's place: /settings. It IS a picker dialog, so every
;;; chord, the Esc that closes it and the footer's toggle treat it as one;
;;; its body is its own. PAINT, called with (MODEL SHEET WIDTH HEIGHT) for
;;; the frame, answers the window: (values ROWS X Y CURSOR), the painted
;;; rows, the frame cell the first one starts at, and the cell (X . Y) the
;;; hardware cursor parks on, or NIL. KEY takes (MODEL SHEET KEY &key CTRL
;;; ALT SHIFT), every key the shell's own chords leave; PASTE (MODEL SHEET
;;; TEXT); CLICK (MODEL SHEET X Y), a left click on the 0-based frame cell X,
;;; Y in the window; WHEEL (MODEL SHEET DELTA X Y), a notch there; HOVER
;;; (MODEL SHEET X Y), the pointer moved onto that cell with no button held,
;;; answering whether that moved anything. Its canvas
;;; and the helpers every sheet paints and calls with are sheet.lisp's.
(defstruct (sheet-dialog (:include picker-dialog))
  "A dialog drawn in the middle of the frame."
  paint key paste click wheel hover
  ;; What the last frame painted, (ROWS X Y WIDTH): the overlay lays it and
  ;; the pointer measures against it; the cells a click or the pointer
  ;; resting there acts on, ((ROW FROM TO ACTION HOVER) ...); the line on the key row, (TEXT TONE . TIME);
  ;; and whether a call is under way.
  (layout nil) (hits '()) (flash nil) (busy nil))

(nlk:access (sheet sheet-dialog))

(defun sheet-box (width height want-width want-height &key whole)
  "=> (values X Y W H EDGES): where a sheet asking WANT-WIDTH x WANT-HEIGHT
cells stands on a WIDTH x HEIGHT frame -- centred over the rows above the
shell footer, its margins narrowing with the window, EDGES true -- or, under
50 columns or 16 rows above the footer, or WHOLE, every one of those rows
from the top, with no edges."
  (let ((bottom (if (shell-footer-p height) (- height 3) height)))
    (if (or whole (< width 50) (< bottom 16))
        (values 0 0 width bottom nil)
        (let ((w (min want-width (- width (* 2 (cond ((>= width 100) 6) ((>= width 70) 4) (t 2))))))
              (h (min want-height (- bottom 2))))
          (values (floor (- width w) 2) (max 1 (floor (- bottom h) 2)) w h t)))))

(defun settle-sheet (model width height &aux (sheet (chat-model-dialog model)))
  "Paint the open sheet's window for this WIDTH x HEIGHT frame, keep it for
the overlay and the pointer, and park the cursor where it says. => whether a
sheet is open."
  (when (sheet-dialog-p sheet)
    (multiple-value-bind (rows x y cursor) (funcall sheet.paint model sheet width height)
      (setf sheet.layout (list rows x y (reduce #'max rows :key #'text-display-width
                                                           :initial-value 0))
            (chat-model-parked-cursor model) cursor))
    t))

(defun sheet-at (model x y &aux (sheet (chat-model-dialog model)))
  "=> (values COLUMN ROW), the 0-based frame cell under mouse X, Y (1-based),
when it lies in the open sheet's window as the last frame painted it; NIL
otherwise."
  (when (and (sheet-dialog-p sheet) sheet.layout)
    (destructuring-bind (rows left top width) sheet.layout
      (let ((column (1- x)) (row (1- y)))
        (when (and (<= left column (+ left width -1))
                   (<= top row (+ top (length rows) -1)))
          (values column row))))))

(defparameter +sheet-backdrop-light+ 40
  "Percent of its own ink the frame behind an open sheet keeps, the rest its
ground's: what is behind reads as behind.")

(defun sheet-overlay (model &aux (sheet (chat-model-dialog model))
                                 (profile (effective-color-profile)))
  "The open sheet's window as a paint over the frame's cells, every cell
outside it dimmed toward its ground, or NIL while no sheet is open."
  ;; A basic colour's code names no rgb to blend, so the 16-colour profile
  ;; dims nothing; :none has nothing to dim.
  (when (and (sheet-dialog-p sheet) sheet.layout)
    (destructuring-bind (rows left top width) sheet.layout
      (let ((canvas (or (canvas-rgb) (visual-style-bg (style-for-tone :selection-ground))))
            (ink (visual-style-fg (style-for-tone :foreground)))
            ;; The parser hands every cell of one colour the same string, so
            ;; a frame blends each pair once.
            (dimmed (make-hash-table :test 'eq)))
        (flet ((rgb (sgr)
                 ;; An extended colour's only: `38;2;R;G;B' or `38;5;N'.
                 (and sgr (> (length sgr) 3) (sgr-rgb sgr))))
          (lambda (buffer)
            (when (and canvas (member profile '(:truecolor :ansi256)))
              (dotimes (row (nlt:screen-buffer-height buffer))
                (dotimes (column (nlt:screen-buffer-width buffer))
                  (unless (and (<= top row (+ top (length rows) -1))
                               (<= left column (+ left width -1)))
                    (let* ((cell (nlt:screen-buffer-ref buffer column row))
                           (grounds (alexandria:ensure-gethash (nlt:cell-fg cell) dimmed
                                                               (make-hash-table :test 'eq))))
                      (setf (nlt:cell-fg cell)
                            (alexandria:ensure-gethash (nlt:cell-bg cell) grounds
                                (color-sgr (blend-rgb (or (rgb (nlt:cell-fg cell)) ink)
                                                      (or (rgb (nlt:cell-bg cell)) canvas)
                                                      +sheet-backdrop-light+)
                                           t profile))))))))
            (nlt:paint-rows buffer rows :x left :y top)))))))

;;; The first deferral round moved handler cost off the keystroke but still
;;; delivered nearly one hydrate per move (measured: 140 picker keys -> 89
;;; flushes at ~21ms each): 20Hz ticks outpace human navigation, so coalescing
;;; alone barely coalesced. A row being arrowed past deserves no preview at
;;; all; only the row the operator rests on does. Two ticks' worth, so a
;;; delivery costs at most one preview per pause. The suite binds this to 0 —
;;; tests drive the flush by hand and assert delivery, not pacing.
(defparameter *picker-highlight-rest* 0.08
  "Seconds a staged highlight must rest before the tick delivers it.")

;;; Paces every staged lane — a held arrow and search narrowing alike. While
;;; the selection keeps moving, the pending row still delivers at this
;;; cadence, so the preview follows the scrub without paying a hydrate per
;;; landing; the final row lands via the rest window when the operator stops.
(defparameter *picker-highlight-throttle* 0.3
  "Seconds between tick deliveries while a STAGED highlight keeps moving.")

;;; Weakly keyed, so a dismissed dialog takes its entry with it; a table and
;;; not a slot for the reason PICKER-ARROW-HANDLERS gives.
(nlk:define-side-table picker-live-highlights (:weakness :key)
  "The open dialogs whose every landing delivers in the pass that makes it
(OPEN-PICKER-DIALOG's :HIGHLIGHT-DELIVERY :LIVE): PICKER-DIALOG -> T.")

(defun picker-notify-highlight (model dialog &key (instant (picker-live-highlights dialog)))
  "Deliver or stage the highlight for the currently selected visible row —
the Zig fetch-on-selection-change seam."
  ;; Highlight handlers do real work: the sessions dialog hydrates a whole
  ;; preview over HTTP, the tree dialog fetches checkpoint files, the themes
  ;; dialog rebuilds the palette. Two regimes, chosen by the caller:
  ;;
  ;; INSTANT — a landing the caller wants shown at once: a hover landing, the
  ;; search box emptied back to the listing, a listing arriving under a dialog
  ;; already open — delivers SYNCHRONOUSLY in the caller's own pass, one
  ;; delivery for one landing. A live dialog's landings are all instant: its
  ;; handler never blocks the loop, so a held arrow shows every row it crosses.
  ;;
  ;; Every other key stages — arrows and their chords (held repeat included), Tab,
  ;; search narrowing: the row is parked in PENDING-HIGHLIGHT and the
  ;; frame-clock tick delivers the survivor (PICKER-FLUSH-HIGHLIGHT) once it
  ;; rests or the throttle reopens, so a burst never pays a hydrate per
  ;; landing. The rows arrowed past were never chosen; only the row the
  ;; operator rests on, and one per throttle window while scrubbing, earn a
  ;; preview. Restaging the row already staged keeps its rest clock: a search
  ;; whose keystrokes leave the same top row selected must not postpone
  ;; that row's preview forever. No-op without a handler or without rows.
  (when-let (on-highlight dialog.on-highlight)
    (let ((row (selected-of (picker-visible-rows dialog)
                            dialog.selection))
          (now (get-internal-real-time)))
      (cond
        ;; A heading is never chosen, so it is never highlighted either: a
        ;; list that opens on one hands its handler nothing.
        ((or (null row) (picker-row-heading row)) nil)
        (instant
         (setf dialog.pending-highlight nil dialog.last-highlight-at now)
         (funcall on-highlight model row))
        ((not (eq row (picker-dialog-pending-highlight dialog)))
         (setf dialog.pending-highlight row dialog.pending-highlight-since now))))))

(defun picker-flush-highlight (model &aux (dialog (chat-model-dialog model)))
  "Deliver the open dialog's pending highlight when either pacing gate
opens: the staged row has rested *PICKER-HIGHLIGHT-REST* (the pause case),
or *PICKER-HIGHLIGHT-THROTTLE* has elapsed since the last delivery (the
live-follow case — previews keep ticking while the selection moves)."
  ;; The tick-branch flush: the pending row clears before the handler runs, so
  ;; a handler that opens a successor dialog or signals cannot wedge a stale
  ;; delivery. The span label moves with the work — the cost shows in /gc as
  ;; this seam, not as an anonymous tick. => T when a highlight was delivered.
  (when (picker-dialog-p dialog)
    (let ((row dialog.pending-highlight)
          (on-highlight dialog.on-highlight)
          (now (get-internal-real-time)))
      (when (and row on-highlight
                 (or (>= (- now dialog.pending-highlight-since)
                         (* *picker-highlight-rest*
                            internal-time-units-per-second))
                     (>= (- now dialog.last-highlight-at)
                         (* *picker-highlight-throttle*
                            internal-time-units-per-second))))
        (setf dialog.pending-highlight nil dialog.last-highlight-at now)
        (setf nlk:*attribution-label* 'picker-highlight-flush)
        (funcall on-highlight model row)
        t))))

(defparameter +checklist-marks+ '("[x] " "[ ] ")
  "The mark column a checklist row's label leads with, chosen and not.")

(defun picker-mark-rows (dialog)
  "Repaint every row's mark from MARKS — the one place a checklist row's
label says whether its value is chosen — over the label less whichever
mark it already carries."
  (dolist (row dialog.rows)
    (setf row.label
          (concatenate 'string
                       (nth (if (member row.value dialog.marks :test #'equal) 0 1)
                            +checklist-marks+)
                       (ppcre:regex-replace "^\\[[x ]\\] " row.label ""))))
  ;; This is the one handler that rewrites rows in place under a live query:
  ;; the memo a ranking left behind now describes labels that no longer exist.
  (forget-picker-visible-memo dialog))

;;; Weakly keyed, so a dismissed dialog takes its handler with it. A table
;;; rather than a dialog slot on purpose: a live image cannot grow a structure
;;; a slot, and the fold must reach the running shell without a relaunch.
(nlk:define-side-table picker-arrow-handlers (:weakness :key)
  "The open dialogs that carry a fold gesture: PICKER-DIALOG -> the handler
OPEN-PICKER-DIALOG's :ON-ARROW registered, called with (MODEL DIALOG
DIRECTION) for the two arrow keys the selection does not spend and answering
whether it took the key.")

(nlk:define-side-table picker-rename-handlers (:weakness :key)
  "The open dialogs whose rows take a name: PICKER-DIALOG -> the handler
OPEN-PICKER-DIALOG's :ON-RENAME registered, called with (MODEL DIALOG) on
the rename chord (F2), which opens the dialog's field on the highlighted
row.")

(nlk:define-side-table picker-space-handlers (:weakness :key)
  "The open dialogs, no checklist, whose rows answer Space: PICKER-DIALOG ->
the handler OPEN-PICKER-DIALOG's :ON-SPACE registered, called with (MODEL
DIALOG) and answering whether it took the key; declined, the Space is the
query's.")

;;; TABS is a list of (:LABEL the segment's own text, :COUNT the rows behind
;;; it, :VALUE the PICKER-ROW-KEY it selects — NIL for the tab that filters
;;; nothing, :MATCH an optional (ROW VALUE) predicate instead of key equality).
;;; A tabbed dialog says the bucket once, in the tab row, instead of once per
;;; row: the selected tab shows the rows its match or key equality accepts.
;;;
;;; Weakly keyed, and a table rather than a dialog slot on purpose: a live
;;; image cannot grow a structure a slot, and the strip must reach the running
;;; shell without a relaunch.
(nlk:define-side-table picker-tabs (:weakness :key)
  "The tab strip of a tabbed dialog: PICKER-DIALOG -> (TABS . ACTIVE-INDEX).")

(defun register-picker-tabs (dialog tabs &key (active 0))
  "Give DIALOG the tab strip TABS (plists as above), its ACTIVE-th tab
selected. => DIALOG. A dialog registered with no tabs has no strip."
  (when tabs (setf (picker-tabs dialog) (cons tabs active)))
  dialog)

;;; A picture is (:ROWS ROWS :LABEL LABEL), ROWS strings of `1' ink and `0'
;;; paper: a small image the body paints above the list -- LIST-DIALOG's
;;; :PICTURE, the link's QR code. With a :PALETTE of tone names each pixel is
;;; the base-36 digit of the tone it paints in (/context's grid), and :BESIDE
;;; stands it left of the list on a panel wide enough for both. A table for
;;; the reason the strip is one.
(nlk:define-side-table picker-pictures (:weakness :key)
  "The dialogs that carry a picture: PICKER-DIALOG -> its picture plist.")

(defun register-picker-picture (dialog picture)
  "Give DIALOG the PICTURE (a plist as above) its body paints above the list.
=> DIALOG. A dialog registered with none has none."
  (when (getf picture :rows) (setf (picker-pictures dialog) picture))
  dialog)

;;; A table for the reason the strip is one.
(nlk:define-side-table picker-body-floor (:weakness :key)
  "The body rows a dialog's panel held when last measured: PICKER-DIALOG ->
the rows a query in its box keeps it from shrinking under (DIALOG-BODY-HEIGHT).")

(defun picker-dialog-tabs (dialog)
  "DIALOG's tab strip as a list of tab plists, or NIL for a one-list dialog."
  (car (picker-tabs dialog)))

(defun picker-dialog-tab (dialog)
  "DIALOG's selected tab index — 0 for a dialog with no strip."
  (or (cdr (picker-tabs dialog)) 0))

(defun picker-dialog-tab-value (dialog)
  "The row key DIALOG's selected tab selects, or NIL when it filters nothing:
a dialog with no strip, or the all-rows tab of one."
  (when-let (tab (nth (picker-dialog-tab dialog) (picker-dialog-tabs dialog))) (getf tab :value)))

(defun picker-tab-mutate (dialog delta &aux (state (picker-tabs dialog))
                                            (tabs (car state)))
  "Move DIALOG's strip DELTA tabs, wrapping, and land the list at its top:
another tab is another list, so the cursor and the window come home, past
the headings it opens on."
  ;; => the
  ;; new index, or NIL for a dialog with no strip.
  (when tabs
    (setf (cdr state) (mod (+ (cdr state) delta) (length tabs)))
    ;; The ranking's memo describes the rows of the tab we left.
    (forget-picker-visible-memo dialog)
    (picker-seat-top dialog)
    (cdr state)))

(defun picker-tab-pick (model index &aux (dialog (chat-model-dialog model)))
  "Show the tab at INDEX of the open dialog's strip, as Tab walking there
would: the list at its top, its highlight delivered. => T when INDEX is a
tab of the strip."
  ;; A click on a tab's chip (DOCKED-DIALOG-TAB-SEGS): one landing, so the
  ;; highlight goes at once, as a hover's does.
  (when (and (picker-dialog-p dialog) (< -1 index (length (picker-dialog-tabs dialog))))
    (unless (= index (picker-dialog-tab dialog))
      (picker-tab-mutate dialog (- index (picker-dialog-tab dialog)))
      (picker-notify-highlight model dialog :instant t))
    t))

(defun open-picker-dialog (model title rows on-select &rest options
                           &key on-arrow on-search on-space on-rename
                           (highlight-delivery :instant) &allow-other-keys)
  "Open a dialog over MODEL."
  ;; Empty ROWS still open — the tab row plus the body's empty label tell the
  ;; operator there is nothing to pick. SEARCHABLE dialogs (the Zig dialogSpec
  ;; searchable kinds) own the header's third row with the shared
  ;; type-to-filter input; every other dialog shows SUBTITLE there.
  ;; ON-HIGHLIGHT fires immediately for the initially selected row.
  ;;
  ;; OPTIONS are MAKE-PICKER-DIALOG's own keywords, forwarded verbatim —
  ;; except :ON-ARROW, :ON-SPACE, :ON-RENAME and :HIGHLIGHT-DELIVERY, which
  ;; are this function's own:
  ;; the fold handler stays out of the dialog record because a live image
  ;; cannot grow a structure a slot, and it is registered in
  ;; *PICKER-ARROW-HANDLERS* instead. The defstruct already carries every
  ;; other default, so restating them here would be a second place for them to
  ;; drift.
  ;;
  ;; HIGHLIGHT-DELIVERY :STAGED opens without the synchronous first highlight:
  ;; the dialog paints in the caller's own pass and the row's highlight lands
  ;; on the frame clock -- the /tree picker, whose handler is a gateway read
  ;; paid on the loop. :LIVE delivers every landing at once, the open's
  ;; included (PICKER-LIVE-HIGHLIGHTS): a handler that never blocks the loop
  ;; -- the /sessions preview, whose reads run off it -- owes no stage.
  (let ((dialog (apply #'make-picker-dialog
                       :title title :rows rows :on-select on-select
                       (alexandria:remove-from-plist options :on-arrow :on-space :on-rename
                                                     :highlight-delivery :on-search))))
    ;; A checklist's rows carry their marks from the first paint.
    (when dialog.on-toggle (picker-mark-rows dialog))
    (when on-arrow (setf (picker-arrow-handlers dialog) on-arrow))
    (when on-search (setf (picker-search-handlers dialog) on-search))
    (when on-space (setf (picker-space-handlers dialog) on-space))
    (when on-rename (setf (picker-rename-handlers dialog) on-rename))
    (when (eq highlight-delivery :live) (setf (picker-live-highlights dialog) t))
    (setf (chat-model-dialog model) dialog)
    (picker-notify-highlight model dialog :instant (not (eq highlight-delivery :staged))))
  model)

;;; Weakly keyed, so a dismissed dialog takes its entry with it.
;;;
;;; The memo exists because the ranking is asked for twice per keystroke —
;;; once by the key ladder, once by the paint that follows it — and a dialog
;;; over a thousand rows (the prompt history) pays ~25 ms a pass. It is keyed
;;; on the rows LIST and the query, so any handler that swaps rows in place
;;; under a live query must say so (PICKER-MARK-ROWS is the one that does).
(nlk:define-side-table picker-search-handlers (:weakness :key)
  "An instance's query delivery policy, called with (DIALOG QUERY) before ranking.")

(nlk:define-side-table picker-visible-memo (:weakness :key)
  "The last ranking PICKER-VISIBLE-ROWS made, per dialog: (ROWS SEARCH .
VISIBLE).")

;;; The /sessions conversation search fills its rows from the gateway's grep
;;; over every session; PICKER-VISIBLE-ROWS shows them verbatim while the box
;;; still holds that query, because the dialog's own matcher would drop a row
;;; the store matched for a token the row's snippet window does not happen to
;;; carry.
(nlk:define-side-table picker-filtered-rows (:weakness :key)
  "Weak memo: dialog -> the query the dialog's rows already answer, when
something other than the dialog did the filtering.")

(defun picker-rows-filtered-for (dialog query)
  "Declare DIALOG's rows the answer to QUERY — NIL hands them back to the
dialog's own matcher. => DIALOG."
  (if query
      (setf (picker-filtered-rows dialog) query)
      (forget-picker-filtered-rows dialog))
  dialog)

(defun picker-visible-rows (dialog &aux (rows dialog.rows)
                                        (search dialog.search)
                                        (tab (picker-dialog-tab-value dialog)))
  "The rows the dialog currently shows: every row the search query answers,
best answer first."
  ;; SEARCH-SCORE is the matcher — tokenized, fuzzy, tiered — read over the
  ;; fields a row exposes in the order the eye reads them (label, key,
  ;; detail), and it both rejects a row and says where it belongs.
  ;;
  ;; The sort is STABLE, so rows of equal standing keep the dialog's own
  ;; order: the ranking lifts the better answers to the top without ever
  ;; scrambling a list whose order already carries meaning — a session's
  ;; recency, a provider's grouping, an alias ahead of the models it names.
  ;;
  ;; The answer is memoized per (rows, query, tab) — see
  ;; *PICKER-VISIBLE-MEMO*.
  ;;
  ;; A TABBED dialog narrows first to the selected tab's rows — the ones whose
  ;; KEY is its VALUE — and the query then filters what that tab shows. A tab
  ;; whose VALUE is NIL filters nothing, so the all-rows tab of a strip is the
  ;; plain list.
  ;; Filtered abroad: the rows under this dialog came from somewhere that
  ;; already matched and ordered them (*PICKER-FILTERED-ROWS*), and the box
  ;; still holds the query they answer — show them as they are.
  (when (equal (picker-filtered-rows dialog) search)
    (return-from picker-visible-rows rows))
  (memoized (picker-visible-memo dialog) (rows search tab)
    (let ((shown (if tab
                     (remove-if-not (lambda (row)
                                      (let ((match (getf (nth (picker-dialog-tab dialog)
                                                              (picker-dialog-tabs dialog)) :match)))
                                        (if match (funcall match row tab) (equal row.key tab))))
                                    rows)
                     rows)))
      (if (zerop (length search))
          shown
          (mapcar #'cdr (stable-sort (loop for row in shown
                                           for score = (and (not row.heading)
                                                            (search-score
                                                             search
                                                             (list row.label row.key row.detail)))
                                           when score collect (cons score row))
                                     #'> :key #'car))))))

(defun picker-reseat-selection (dialog value)
  "Seat DIALOG's selection after its rows were swapped in place: the row
carrying VALUE takes the cursor, and a value the new rows no longer hold
leaves the cursor on the seat it already had — clamped to the last row —
so a deletion hands the cursor to the row that slid up into the deleted
row's place rather than throwing it home -- or past it, when that is a
heading, which is never chosen. => the seated index."
  (let* ((visible (picker-visible-rows dialog))
         (index (and value
                     (position value visible
                               :key #'picker-row-value :test #'equal))))
    (setf dialog.selection
          (past-headings visible
                         (cond (index index) ((null visible) 0) (t (min dialog.selection (1- (length visible)))))
                         1))))

(defun picker-highlighted-value (dialog)
  "The value of the visible row DIALOG's selection points at, or NIL."
  (let ((row (selected-of (picker-visible-rows dialog) dialog.selection)))
    (and row (picker-row-value row))))

(defun picker-seat-top (dialog)
  "Seat DIALOG's highlight on its first row that is no heading, the window
at the top."
  (setf dialog.selection (past-headings (picker-visible-rows dialog) 0 1)
        dialog.view-offset 0))

(defun picker-search-mutate (dialog new-search)
  (setf dialog.search new-search)
  (when-let (handler (picker-search-handlers dialog))
    (funcall handler dialog new-search))
  ;; The window comes home with the selection: a query that shrinks the list
  ;; must not paint from the offset the old one scrolled to. A cleared query
  ;; shows the headings again, and the /models list opens on one.
  (picker-seat-top dialog))

(defun picker-page-move (model dialog action &aux (total (length (picker-visible-rows dialog))))
  "One PgUp/PgDn/Home/End on the open picker: ACTION moves the list's window
and takes the highlight with it — a page is as many rows as the panel's body
shows (DIALOG-BODY-BAND, the measure the wheel and the hit-test share) — and
Home/End seat the highlight on the list's first and last row with the window
at that end. => T when the list moved, NIL for an empty list."
  ;; A page past either end moves the window no further and lets the highlight
  ;; finish the trip on the row at that end, so the key never spends itself on
  ;; a scroll with nothing to show for it.
  (when (plusp total)
    (bind ((height (or (nth-value 1 (dialog-body-band model dialog)) total))
           (visible (min total (max 1 height))) (max-offset (max 0 (- total visible)))
           (page (max 1 visible)) (selection dialog.selection)
           (offset dialog.view-offset)
           ((seat window) (ecase action
                            (:page-up (values (max 0 (- selection page))
                                              (max 0 (- offset page))))
                            (:page-down (values (min (1- total) (+ selection page))
                                                (min max-offset (+ offset page))))
                            (:scroll-home (values 0 0))
                            (:scroll-end (values (1- total) max-offset)))))
      (setf dialog.selection (past-headings (picker-visible-rows dialog) seat 1)
            dialog.view-offset window)
      ;; The window then settles by the one rule paint and hit-test share,
      ;; so the highlighted row is on screen the moment the key lands.
      (dialog-body-viewport dialog visible)
      t)))

(nlk:define-side-table picker-type-through (:weakness :key)
  "Live-only: a picker that opened by itself after another pick -- the
reasoning effort /models offers once a model lands -- => T. Typing was never
meant for it: a printable key closes it, keeping what stands, and lands in the
composer (UPDATE).")

(defun dismiss-dialog (model handler)
  "Close MODEL's dialog, then call HANDLER — its ON-CANCEL or ON-SUCCESS, or NIL — on MODEL."
  (setf (chat-model-dialog model) nil)
  (when handler (funcall handler model)))

(defun picker-invoke-row (model dialog handler &key close)
  "Hand the highlighted visible row's value to HANDLER — one of DIALOG's
row handlers, or NIL — closing DIALOG first when CLOSE, and land the notice
it answers. No row, or no handler: nothing happens."
  (let ((row (selected-of (picker-visible-rows dialog)
                          dialog.selection)))
    (when (and row handler)
      (when close (setf (chat-model-dialog model) nil))
      (when-let (notice (funcall handler model row.value)) (show-status model notice)))))

(defun picker-dialog-key (model key &key ctrl alt shift (mod 0))
  "Route KEY to the open dialog; => T when the dialog consumed it."
  ;; A modal swallows every key except the quit chords its caller checks
  ;; first. Searchable dialogs edit the query: printable keys filter live,
  ;; Backspace deletes, Ctrl+W/Ctrl+Backspace deletes a word, Ctrl+U clears,
  ;; and Esc clears the query first, closing only once it is empty.
  ;; Refine the span label: a key routed here re-filters the dialog, which
  ;; is a different cost class from a composer keystroke, and the /gc
  ;; by-label table should say so.
  (setf nlk:*attribution-label* 'picker-dialog-key)
  (let ((dialog (chat-model-dialog model)))
    (when (and (connect-dialog-p dialog)
               (connect-dialog-key model dialog key :ctrl ctrl :alt alt))
      (return-from picker-dialog-key t))
    ;; A sheet works its own body: every key the shell's chords left is its.
    (when (sheet-dialog-p dialog)
      (funcall (sheet-dialog-key dialog) model dialog key :ctrl ctrl :alt alt :shift shift)
      (return-from picker-dialog-key t))
    (when dialog
      (let* ((rows (picker-visible-rows dialog))
             (count (length rows))
             ;; Ctrl+P / Ctrl+N walk the list alongside the arrows (the Zig
             ;; isPreviousDialogItemKey / isNextDialogItemKey chords). They
             ;; carry ctrl, so they never reach the search-input branch below.
             (delta (cond ((or (eq key :up) (and ctrl (characterp key) (char-equal key #\p)))
                           -1)
                          ((or (eq key :down) (and ctrl (characterp key) (char-equal key #\n)))
                           1))))
        (cond
          ;; Input mode: the field owns every key. Enter hands the text to
          ;; ON-INPUT and Esc hands it NIL — a dialog without a handler
          ;; leaves the field on Esc — and the rest edits the field,
          ;; uncapped: a path or a secret is not a search query.
          ((picker-dialog-input-label dialog)
           (let ((on-input dialog.on-input))
             (cond ((eq key :enter) (when on-input (funcall on-input model dialog.input)))
                   ((eq key :escape)
                    (if on-input (funcall on-input model nil) (setf dialog.input-label nil)))
                   (t
                    (when-let (new (edit-text-field dialog.input key :ctrl ctrl :alt alt))
                      (setf dialog.input new))))))
          (delta
           (when (plusp count)
             (setf dialog.selection
                   (past-headings rows (wrap-selection dialog.selection count delta) delta))))
          ;; PgUp/PgDn page the list and Home/End jump it to either end. These
          ;; are the transcript's own chords (the keymap's :page-up,
          ;; :page-down, :scroll-home and :scroll-end) spent on the dialog
          ;; while one is open, so the transcript behind the panel never
          ;; moves; this ladder runs only while a dialog is open, so the same
          ;; keys are the transcript's again the moment it closes.
          ((when-let (edge (find-if (lambda (action) (model-key-matches-p model action key mod))
                                    '(:page-up :page-down :scroll-home :scroll-end)))
             (picker-page-move model dialog edge)
             t))
          ;; The fold gesture (the sessions picker's Left/Right): consulted
          ;; on the arrow keys the selection does not spend, before the
          ;; detail gesture — and only a consumed answer ends the ladder
          ;; here, so a declined Right still reaches ON-DETAIL below.
          ((and (picker-arrow-handlers dialog)
                (or (eq key :left) (eq key :right))
                (funcall (picker-arrow-handlers dialog) model dialog key)))
          ;; Space on a list whose rows answer it -- /cells turns the
          ;; highlighted cell on or off -- ahead of the query, which would
          ;; take it as a character.
          ((and (eql key #\Space) (not ctrl) (not alt)
                (picker-space-handlers dialog)
                (funcall (picker-space-handlers dialog) model dialog)))
          ((eq key :escape)
           (if (and dialog.searchable (plusp (length dialog.search)))
               (picker-search-mutate dialog "")
               (dismiss-dialog model dialog.on-cancel)))
          ((eq key :enter)
           (setf (chat-model-dialog model) nil)
           ;; A checklist confirms its marks as one value; a picker
           ;; selects the highlighted row.
           (if dialog.on-toggle
               (when-let (notice (and dialog.on-select
                                      (funcall dialog.on-select model dialog.marks)))
                 (show-status model notice))
               (picker-invoke-row model dialog dialog.on-select)))
          ;; Space on a checklist toggles the highlighted row's mark; the
          ;; dialog stays open.
          ((and (picker-dialog-on-toggle dialog)
                (eql key #\Space) (not ctrl) (not alt))
           (when-let (row (selected-of rows dialog.selection))
             (let* ((value row.value)
                    (marked (not (member value dialog.marks
                                         :test #'equal))))
               (setf dialog.marks
                     (if marked
                         (append dialog.marks (list value))
                         (remove value dialog.marks :test #'equal)))
               (picker-mark-rows dialog)
               (funcall dialog.on-toggle model value marked))))
          ;; The detail gesture (Zig isDialogDetailKey: Right, i, I) on
          ;; dialogs that carry a detail handler — the cells dialog's
          ;; drill into one cell's facts. Bare letters stay search input on
          ;; searchable dialogs; with no selected row the key is a
          ;; consumed no-op, exactly like the Zig empty-list case.
          ((and (picker-dialog-on-detail dialog)
                (or (eq key :right)
                    (and (not dialog.searchable) (not ctrl) (not alt)
                         (characterp key) (char-equal key #\i))))
           (picker-invoke-row model dialog dialog.on-detail
                              :close t))
          ;; The delete gesture on dialogs that carry a handler for it --
          ;; /execs' interrupt. Ctrl+X never reaches the search input;
          ;; Delete is consumed here before it could.
          ((and (picker-dialog-on-delete dialog)
                (or (eq key :delete)
                    (and ctrl (characterp key) (char-equal key #\x))))
           (picker-invoke-row model dialog dialog.on-delete))
          ;; The rename chord on a dialog whose rows take a name: the
          ;; handler puts the dialog in input mode on the highlighted row.
          ((and (picker-rename-handlers dialog)
                (model-key-matches-p model :rename key mod))
           (funcall (picker-rename-handlers dialog) model dialog))
          ;; Tab walks a tabbed dialog's strip: the scope tabs of one list
          ;; whose rows are bucketed, where the Zig keeps one dialog per
          ;; filter. Another tab is another list, so the cursor comes home
          ;; with it (PICKER-TAB-MUTATE), and Shift+Tab walks back.
          ((and (eq key :tab) (picker-dialog-tabs dialog))
           (picker-tab-mutate dialog (if shift -1 1)))
          ;; Left and Right walk the strip too, stopping at its ends: the
          ;; arrows across are a strip's as Up and Down are the list's. A
          ;; dialog's own Left/Right outranks them: the fold above
          ;; (/sessions' forks) takes an arrow it can use, and one it
          ;; declines -- a row with no forks -- walks the strip here.
          ((and (or (eq key :left) (eq key :right)) (not ctrl) (not alt)
                (picker-dialog-tabs dialog))
           (let ((delta (if (eq key :left) -1 1)))
             (when (< -1 (+ (picker-dialog-tab dialog) delta) (length (picker-dialog-tabs dialog)))
               (picker-tab-mutate dialog delta))))
          (dialog.searchable
           (when-let (new (edit-text-field dialog.search key :ctrl ctrl :alt alt
                                           :max *max-dialog-search-chars*))
             (picker-search-mutate dialog new))))
        ;; The selection may have landed on a new row (movement, filter
        ;; edits). Re-notify while this dialog is still the open one —
        ;; highlight handlers are idempotent, and a failed lazy fetch gets
        ;; its retry exactly here (the Zig cleared-target re-selection).
        ;; Every key stages: a held arrow is a burst, and the tick delivers
        ;; the row the operator rests on (one per throttle window while the
        ;; selection keeps moving) -- never a hydrate per landing. A live
        ;; dialog delivers each one here instead.
        (when (eq dialog (chat-model-dialog model))
          (picker-notify-highlight model dialog)))
      t)))

;;; --- a paste into a dialog ---------------------------------------------------
;;; A paste lands where typing would. The terminal's own paste (Ctrl+Shift+V)
;;; arrives as one bracketed message; Ctrl+V asks the system clipboard for its
;;; text. Both reach the field through PICKER-DIALOG-PASTE, and neither walks
;;; the key ladder, so no pasted character can select, toggle or submit.

(defun picker-dialog-paste (model text)
  "Route pasted TEXT to the open dialog => T when it landed."
  ;; It goes where a typed character goes: the field in input mode, uncapped
  ;; as typing is there; the query of a searchable list, capped as typing is.
  ;; The /models result step and a list with no query own no field, and let
  ;; it go.
  ;; TEXT lands as one line the field could have been typed: the characters
  ;; EDIT-TEXT-FIELD takes from a keystroke (the graphic ones), the blanks
  ;; around them trimmed. A key copied with its trailing newline arrives as
  ;; the key, and a line break inside a paste cannot split a one-line field.
  (let ((dialog (chat-model-dialog model))
        (line (string-trim " " (remove-if-not #'graphic-char-p (or text "")))))
    (when (and dialog (plusp (length line)))
      (cond
        ((sheet-dialog-p dialog)
         (funcall (sheet-dialog-paste dialog) model dialog line)
         t)
        (dialog.input-label (setf dialog.input (concatenate 'string dialog.input line)) t)
        ((and (connect-dialog-p dialog)
              (member (connect-dialog-step dialog) '(:result :checking :installing :signing-in)))
         nil)
        ((picker-dialog-searchable dialog)
         (picker-search-mutate dialog (clip (concatenate 'string dialog.search line)
                                            *max-dialog-search-chars* :ellipsis ""))
         (picker-notify-highlight model dialog)
         t)))))

(defparameter *clipboard-paste-commands*
  #+win32 '()
  #+darwin '(("pbpaste"))
  #-(or win32 darwin) '(("wl-paste" "--no-newline")
                        ("xclip" "-selection" "clipboard" "-o")
                        ("xsel" "--clipboard" "--output"))
  "The readers Ctrl+V over a dialog tries in order: the read half of the
chain clipboard writes take — macOS's pasteboard, else wl-copy, xclip,
xsel.")

(defun docked-panel-surface-bg ()
  "The panel's fill: the :composer surface every tone is forced onto (Zig
shortcutPickerStyle), or NIL under the :none profile."
  ;; SEG-RENDER already backs a tone without its own bg with the fill it is
  ;; handed, and :composer — the one panel tone that carries a bg — carries
  ;; exactly this one, so the forcing needs no per-segment override.
  (unless (eq (effective-color-profile) :none)
    (visual-style-bg (style-for-tone :composer))))

(defparameter *docked-panel-lane-gap* 2
  "The columns between a panel row's left flow and its right lane — a tab,
so a row's own text and the fact parked at its right edge never read as one
run. Only spent when the row was given a right lane.")

(defun scrollbar-cell (thumb-top thumb-height row)
  "What a panel row's scrollbar lane paints at ROW: :THUMB inside the thumb
SCROLLBAR-THUMB answered, :TRACK beside it, NIL when it answered no bar."
  ;; The one reading of that pair, so the two docked lanes — the picker's list
  ;; and the composer's dropdown — cannot disagree about where a thumb ends.
  (when thumb-height
    (if (<= thumb-top row (+ thumb-top thumb-height -1)) :thumb :track)))

(defun docked-panel-row (segs width surface
                         &key (col *docked-panel-content-col*) scrollbar
                         (pad-left *docked-panel-pad-left*)
                         (pad-right *docked-panel-pad-right*)
                         (bar-gap 0) mark right lead (lead-width 0) (ground surface))
  "One full-WIDTH panel row — the one row primitive every docked panel paints
through: PAD-LEFT columns of panel fill, SEGS printed at COL inside the
padded rect and clipped at its right edge, the slack filled, then SCROLLBAR
(:thumb or :track) in the rect's last columns with BAR-GAP of fill either
side of the bar cell, then the right padding. GROUND, an rgb, is the fill
from COL to the bar -- a chosen row's lift -- and the panel's SURFACE
otherwise."
  ;; The dialog panel takes the defaults; the composer dropdown passes its own
  ;; zero pads and its 2-column gaps. Under the :none profile the geometry is
  ;; identical in plain text, so the goldens stay byte-stable. MARK is what a
  ;; row too long for its rect wears at the cut — the dialog entry's ` … ', a
  ;; 1-width gap either side of the one-column triple dot, so a row hitting
  ;; the right edge says it continues instead of ending flush against it; NIL
  ;; cuts bare, the Zig `.wrap = .none' truncation. RIGHT, when given, is a
  ;; second lane painted flush against the rect's right edge, with
  ;; *DOCKED-PANEL-LANE-GAP* columns of fill between it and SEGS: the row's
  ;; own text keeps the left lane and is what truncates when the two would
  ;; meet, so a fact parked right — the dropdown's hotkey column — is never
  ;; walked off the row by a long description beside it. LEAD is a lane of
  ;; LEAD-WIDTH columns between the left padding and the row's own column,
  ;; its segments clipped to it -- a picture standing beside a list -- and a
  ;; segment whose tone is a VISUAL-STYLE keeps its own ground there.
  ;; The decomposition is exact by construction — pad-left + col + room + the
  ;; scrollbar lane + pad-right always sums to WIDTH — so a narrow window
  ;; loses padding and content rather than pushing the row past the frame.
  ;; The scrollbar only claims its lane when one is left for it, which is the
  ;; same guard Zig keeps (a scrollbar writes inside its window, never past
  ;; its right edge).
  (let* ((pad-left (min pad-left (max 0 width)))
         (pad-right (min pad-right (- width pad-left)))
         (lead-width (max 0 (min lead-width (- width pad-left pad-right))))
         (lead (and (plusp lead-width) (clip-segs lead lead-width)))
         (inner (- width pad-left pad-right lead-width))
         (col (min col inner))
         (lane (+ 1 (* 2 bar-gap)))
         (bar (and scrollbar (> (- inner col) (if (plusp bar-gap) lane 0))))
         (room (- inner col (if bar lane 0)))
         ;; The right lane is measured and cut first: its columns come out of
         ;; the left lane's room, so the two can never overrun the rect and
         ;; the left text — never the parked fact — is what clips.
         (right (clip-segs right room))
         (right-width (segs-display-width right))
         (gap (if (plusp right-width) *docked-panel-lane-gap* 0))
         (kept (clip-segs segs (max 0 (- room right-width gap)) :mark mark))
         ;; The targets its segs carry -- a tab's chip, a key's hint, a link --
         ;; as ranges of the characters they stand on in the row, the shape
         ;; FRAME-TARGET-AT-POSITION reads under the pointer: KEPT from just
         ;; past the pads, the lead lane and the content column, RIGHT past
         ;; KEPT and the slack between them.
         (start (+ pad-left col (if (plusp lead-width)
                                    (+ (length (segs-text lead)) (- lead-width (segs-display-width lead)))
                                    0)))
         (links (append (seg-links kept start)
                        (seg-links right (+ start (length (segs-text kept))
                                            (- room (segs-display-width kept) right-width))))))
    (let ((row
            (flet ((fill-run (count &optional (fill surface)) (blank-frame-row (max 0 count) fill))
                   (paint (lane &optional (fill surface))
                     (format nil "~{~a~}"
                             (mapcar (lambda (seg)
                                       ;; Every tone the panel paints sits on the
                                       ;; panel surface, tones with a bg of their
                                       ;; own included (Zig shortcutPickerStyle);
                                       ;; a style given whole keeps its own.
                                       (seg-render seg fill
                                                   :force-bg (not (visual-style-p seg.tone))))
                                     lane))))
              (concatenate 'string
                           ;; The column before the text is one run with the pads
                           ;; when the row stands on the panel, so a plain row's
                           ;; bytes are what they always were.
                           (let ((lift (if (equal ground surface) 0 col)))
                             (concatenate 'string
                                          (if (plusp lead-width)
                                              (concatenate 'string
                                                           (fill-run pad-left)
                                                           (paint lead)
                                                           (fill-run (+ (- lead-width (segs-display-width lead))
                                                                        (- col lift))))
                                              (fill-run (+ pad-left (- col lift))))
                                          (if (plusp lift) (fill-run lift ground) "")))
                           (paint kept ground)
                           ;; The slack is what neither lane took: the gap is
                           ;; spent in KEPT's budget above, never here, so a row
                           ;; with a right lane still fills its rect exactly.
                           (fill-run (- room (segs-display-width kept) right-width) ground)
                           (paint right ground)
                           (when bar
                             ;; The bar cell: the thumb glyph or a track blank,
                             ;; toned for the surface, bare under :none.
                             (let ((thumb-p (eq scrollbar :thumb)))
                               (concatenate 'string
                                            (fill-run bar-gap)
                                            (if surface
                                                (styled (if thumb-p "█" " ")
                                                        (style-for-tone (if thumb-p
                                                                            :scrollbar-thumb
                                                                            :scrollbar-track)))
                                                (if thumb-p "█" " "))
                                            (fill-run bar-gap))))
                           (fill-run pad-right)))))
      ;; Linked whole: the row the frame keeps is the one its ranges are on.
      (if links (link-plain-row (nlk:narrow-string row) links) row))))

(defun docked-panel-room (width &optional scrollbar)
  "The columns a dialog's DOCKED-PANEL-ROW gives its SEGS at WIDTH: the
panel's pads and content column out, and the bar's lane when SCROLLBAR."
  ;; What a row fits to before the row cuts it: the footer's hints drop whole
  ;; to it, and a selected row's path clips its middle to what is left of it.
  (max 0 (- width *docked-panel-pad-left* *docked-panel-content-col*
            *docked-panel-pad-right* (if scrollbar 1 0))))

(defun dialog-list-viewport (total selected offset height)
  "The Zig computeDialogListViewport contract: => (values SELECTED OFFSET
VISIBLE-COUNT) for TOTAL rows shown in a HEIGHT-row body, with the window
pulled just far enough to keep SELECTED inside it."
  (if (or (zerop total) (zerop height))
      (values 0 0 0)
      (let ((visible (min total height))
            (selected (min selected (1- total))))
        (values selected
                (alexandria:clamp (min offset (- total visible)) (- selected (1- visible)) selected)
                visible))))

(defun dialog-body-viewport (dialog height)
  "=> (values SELECTION OFFSET VISIBLE) for DIALOG's list in a HEIGHT-row
body, and settle VIEW-OFFSET to what it answered."
  ;; The window is the dialog's own stored one, pulled just far enough to keep
  ;; the selection inside it, which is what lets the two gestures coexist: a
  ;; wheel scroll moves the window and leaves the highlight under the pointer,
  ;; while keyboard navigation moves the highlight and drags the window after
  ;; it. Paint and hit-test both come through here, so they read the same
  ;; settled number — the row the operator sees highlighted is the row the
  ;; pointer is on.
  ;;
  ;; A heading stays with the rows under it: when the selection drags the
  ;; window up onto the first row under one, the heading comes into view with
  ;; it. Only that drag -- a window the wheel put somewhere stays there.
  (let ((rows (picker-visible-rows dialog)))
    (multiple-value-bind (selection offset visible)
        (dialog-list-viewport (length rows) dialog.selection dialog.view-offset height)
      (when (> dialog.view-offset selection)
        (loop while (and (plusp offset)
                         (picker-row-heading (nth (1- offset) rows))
                         (< (- selection (1- offset)) visible))
              do (decf offset)))
      (setf dialog.view-offset offset)
      (values selection offset visible))))

(defun dialog-entry-segs (row selected &key marker tabbed room columns)
  "One docked list row (the Zig renderDockedRow family): the primary column
in the dialog accent tone while selected and the system tone otherwise, every
trailing field muted, two spaces between columns."
  ;; The accent is :dialog-accent rather than Zig's :composer — same choose
  ;; chain, but :composer belongs to the composer box, and a layer that
  ;; neutralizes that box would otherwise leave the selected row the same grey
  ;; as every other one. A row carrying KEY reads as the Zig label/value shape
  ;; (renderLabelValueDockedRow) — the field name leads and its value follows
  ;; muted. SUFFIX is selected-row-only chrome, the Zig files preview that
  ;; would drown the list on every row.
  ;;
  ;; MARKER adds the two-column `> ` gutter Zig spends on its option rows
  ;; (renderCheckpointRestoreOptionRow). The panel asks for it only under a
  ;; profile that paints no color, where tone alone cannot say which row is
  ;; selected — Zig degrades the same way, but a shell with no colors is still
  ;; expected to be usable. Every row in the list gets the gutter, so the
  ;; columns stay aligned within a profile.
  ;;
  ;; TABBED says the dialog's tab row already names the row's bucket, so the
  ;; row does not spend a column saying it again (PICKER-DIALOG-TABS).
  ;;
  ;; ROOM is the columns the panel row gives the entry (DOCKED-PANEL-ROOM).
  ;; The selected row's PATH takes what the rest leaves of it, cut from the
  ;; middle (CLIPPED-PATH): cut at the edge, a session's folder would lose its
  ;; tail, which is the part that names it. Under eight columns left it is not
  ;; drawn.
  (let* ((key (if tabbed "" row.key))
         (label row.label)
         (detail (concatenate 'string row.detail (if selected row.suffix "")))
         (primary (cond (row.heading :muted) (row.tone) (selected :dialog-accent) (t :system)))
         (key-segs (picker-field-segs row :key key primary))
         (label-segs (picker-field-segs row :label label (if (plusp (length key)) :muted primary)))
         (key-segs (if columns (clip-segs key-segs (first columns) :mark "…") key-segs))
         (label-segs (if columns (clip-segs label-segs (second columns) :mark "…") label-segs))
         (key-width (if columns (first columns) (segs-display-width key-segs)))
         (label-width (if columns (second columns) (segs-display-width label-segs)))
         (mark-width (if columns (third columns) (text-display-width row.mark)))
         (segs (append
                (when marker
                  (list (make-seg :text (if selected "> " "  ") :tone primary)))
                (when (plusp mark-width)
                  (list (make-seg :text row.mark :tone row.mark-tone)
                        (make-seg :text (make-string
                                         (+ 1 (max 0 (- mark-width (text-display-width row.mark))))
                                         :initial-element #\Space) :tone primary)))
                (when (plusp key-width)
                  (append key-segs
                          (list (make-seg :text (make-string (+ 2 (max 0 (- key-width (segs-display-width key-segs))))
                                                            :initial-element #\Space) :tone primary))))
                label-segs
                (when (plusp (length detail))
                  (list (make-seg :text (concatenate 'string
                                         (make-string (+ 2 (max 0 (- label-width (segs-display-width label-segs))))
                                                      :initial-element #\Space)
                                         detail) :tone :muted)))))
         (left (and selected room (plusp (length row.path)) (- room (segs-display-width segs) 2))))
    (if (and left (>= left 8))
        (append segs (list (make-seg :text (concatenate 'string "  "
                                                        (clipped-path (home-abbreviated row.path)
                                                                      left))
                                     :tone :muted)))
        segs)))

;;; The two panel reads that are still derived. Everything else the chrome
;;; needs — searchable, the tab context, the summary row, the footer copy —
;;; IS a picker slot: the /models wizard is a picker whose steps write those
;;; slots as the flow moves (CONNECT-ENTER-STEP), so no read below branches
;;; on which kind of dialog is open.

(defun dialog-header-row-count (dialog)
  "Header rows the panel reserves (Zig DockedDialogOptions header_rows): the
tab row, a blank, the summary -- one row, or its SUMMARY-ROWS -- and a blank."
  (or dialog.header-rows
      (+ 3 (max 1 (length dialog.summary-rows))))) 
(defun dialog-footer-row-count (dialog)
  "Footer reservation; :NONE removes both hint copy and its space."
  (if (eq :none (picker-dialog-footer-hints dialog)) 0 *docked-dialog-footer-rows*))
   ; Zig keymap_dialog_header_rows

;;; Panel geometry. The Zig docked area spans from its own top edge down to
;;; shellContentBottomRow, and the transcript yields the rows it covers
;;; (reserveTranscriptAboveDialog). Its height is the dialog's content, under
;;; the cap; without the cap, Zig's full-height panel would leave a short list
;;; blank rows the transcript could have had, the /sessions preview above it
;;; first of all.

(defun dialog-body-height (dialog width)
  "The body rows DIALOG's panel holds at WIDTH: the field rows of an input or
result step, else one for each row its list shows, or one for its empty
label -- and while a query is in its box, never fewer than it held as the
query began."
  ;; The panel follows its rows both ways: a fold that opens grows it and the
  ;; fold closing gives the rows back. A query alone holds it: the search row is
  ;; the panel's top edge, so a query narrowing the list would move the caret
  ;; under the typing. Cleared, the box lets the panel follow its rows again.
  (let ((rows (if-let (field (dialog-field-rows dialog width nil))
                (length field)
                (max 1 (length (picker-visible-rows dialog)))))
        (held (and (plusp (length dialog.search)) (picker-body-floor dialog))))
    (setf (picker-body-floor dialog) (max rows (or held 0)))))

(defun docked-dialog-frame-rows (model)
  "=> (values PANEL-HEIGHT ROWS-ABOVE ROWS-BELOW) for the open dialog, or NIL."
  ;; ROWS-ABOVE counts the frame rows before the panel's first painted row and
  ;; ROWS-BELOW those after its last, so the three always stack to TERM-HEIGHT
  ;; with the panel's painted rows between them.
  ;;
  ;; The area's last row is the canvas gap above the shell footer, so the
  ;; panel paints one row fewer than it spans.
  ;;
  ;; The area's bottom edge is the Zig shellContentBottomRow — the footer row
  ;; on a window tall enough to carry a footer, the window's own bottom
  ;; otherwise — and the frame head above it — the blank row, the header row
  ;; and the chat top-padding row — is out of the panel's reach (Zig's
  ;; full-height panel covers the padding row; here it stays the head's, so a
  ;; panel never touches the header). Under the shell's DIALOG-FULLSCREEN flag
  ;; the panel spans that whole reach, whatever the dialog's own rows say;
  ;; otherwise it is its chrome and its body (DIALOG-BODY-HEIGHT) under
  ;; *DIALOG-FRAME-MAX-HEIGHT*. A sheet docks nothing: its window stands
  ;; over the frame (SHEET-OVERLAY), which keeps its composer.
  (when-let (dialog (and (not (sheet-dialog-p (chat-model-dialog model)))
                         (chat-model-dialog model)))
    (let* ((height (max 4 (chat-model-term-height model)))
           ;; Every row under the frame head down to the shell footer, the
           ;; whole of a fullscreen panel. The head is three rows — the
           ;; blank, the header, the chat top-padding row — and the panel
           ;; reaches none of them: Zig's full-height panel covers the
           ;; padding row, so a fullscreen panel sits one canvas row under
           ;; the header the way a docked one sits one under the transcript.
           (reach (if (shell-footer-p height) (- height 5) (- height 3)))
           ;; The padding row and the canvas gap beside the header and footer.
           (chrome (+ (dialog-header-row-count dialog) (dialog-footer-row-count dialog) 2))
           (width (frame-width model))
           (panel (if (chat-model-dialog-fullscreen model)
                      reach
                      ;; A picture is painted whole or not at all, so the
                      ;; panel grows past the list's cap to hold it whole
                      ;; and the list under it; the line standing in for it
                      ;; is two rows of the list's panel.
                      ;; A picture beside the list holds the body to its
                      ;; own lines however short the list.
                      (multiple-value-bind (picture whole lead)
                          (dialog-picture-rows dialog (- reach chrome) width)
                        (min reach (+ chrome picture
                                      (max (dialog-body-height dialog width)
                                           (if lead
                                               (ceiling (length (getf (picker-pictures dialog) :rows)) 2)
                                               0)))
                             ;; /status and the privacy box are read whole, never scrolled.
                             (if (or whole (member (picker-dialog-kind dialog) '(:status :privacy)))
                                 reach *dialog-frame-max-height*))))))
      ;; Under 5 rows no panel: Zig's header-only strip has no shape in this shell.
      (when (>= panel 5)
        (if (shell-footer-p height)
            ;; …the panel's own canvas gap row, the footer, the final blank.
            (values panel (- height 2 panel) 3)
            (values panel (- height panel) 1))))))

(defun docked-dialog-above-bands (above &aux (slack (max 0 (- above 2)))
                                             (pad (min 1 slack))
                                             (gap (min 1 (- slack pad))))
  "=> (values PAD GAP BAND): how the ABOVE rows of a docked frame divide,
after the blank and header rows every frame owns."
  ;; PAD is the chat top-padding row (always present: the panel's reach stops
  ;; above it), GAP the one canvas row Zig leaves between the transcript and
  ;; the panel (reserveTranscriptAboveDialog stops the band at `dialog.area.y
  ;; - 1`, so that row paints neither transcript nor panel surface), and BAND
  ;; the transcript rows that remain. Both the frame stack and the viewport
  ;; reservation read this one seam, so they cannot disagree about which row
  ;; the separator is.
  (values pad gap (- slack pad gap)))

(defun docked-dialog-third-row-segs (dialog)
  "The header's third row: the live search query, or the placeholder, on a
searchable dialog; the summary line otherwise."
  ;; No caret glyph paints here — the hardware cursor parks just past the
  ;; query instead (Zig dockedDialogSearchFocus; COMPOSE-FRAME parks it).
  (cond ((not dialog.searchable)
         (when (plusp (length dialog.subtitle)) (list (seg dialog.subtitle dialog.summary-tone))))
        ((plusp (length dialog.search)) (list (seg dialog.search :composer)))
        (t (list (seg (or (json-value dialog.search-placeholder :text) "Type to search") :muted)))))

(defun docked-dialog-tab-segs (dialog &aux (none (eq (effective-color-profile) :none)))
  "DIALOG's tab strip as header segments, or NIL for a dialog with none:
every tab a filled chip, ` label  count `, two columns apart -- the one
selected in the sheet's held chip, the rest in its plain one -- the chips
/settings' options are, so a tab reads as one thing to press, and a click on
one shows it (PICKER-TAB-PICK)."
  ;; A chip keeps its own ground on the panel (a style given whole); under
  ;; the profile that paints no color the held one stands in brackets, as a
  ;; held option does on /settings.
  (loop for tab in (picker-dialog-tabs dialog)
        for index from 0
        for on = (= index (picker-dialog-tab dialog))
        for text = (format nil "~a~@[  ~:d~]" (getf tab :label) (getf tab :count))
        when (plusp index) collect (make-seg :text "  " :tone :muted)
        collect (make-seg :text (if (and on none) (format nil "[~a]" text) (format nil " ~a " text))
                          :tone (style-for-tone (if on :sheet-chip-on :sheet-chip))
                          :link (cons :dialog-tab index))))

(defun docked-dialog-header-rows (dialog width surface count)
  "The panel header, truncated to COUNT rows: the tab row at column 0, a
blank, the search-or-summary row at *DOCKED-PANEL-SEARCH-ROW* -- or the
dialog's SUMMARY-ROWS from there -- a blank."
  ;; A :dialog-accent title reads as the tab row and drops bold to match the
  ;; chrome (Zig renderTitledDockedHeader, which spends :composer there); any
  ;; other tone keeps it.
  (let* ((tabs (picker-dialog-tabs dialog))
         ;; A tabbed dialog's title is chrome, not the highlighted label: the
         ;; strip's selected tab says which list is on screen, and two
         ;; highlighted labels would each claim the row.
         (title-tone (if tabs :muted dialog.title-tone))
         (bold (and (not tabs) (not (eq dialog.title-tone :dialog-accent))))
         ;; A tabbed dialog's context is no tab: it stands flush right of the
         ;; strip, apart from the chips (`⠹ 1 running', `12 matches').
         (context (string-trim " " dialog.context))
         (rows (append
                (list (docked-panel-row
                       (append (when (or (not tabs) (plusp (length dialog.title)))
                                 (list (make-seg :text (format nil " ~a ~:[~; ~]" dialog.title tabs)
                                                 :tone title-tone :bold bold)))
                               (docked-dialog-tab-segs dialog)
                               (when (and (not tabs) (plusp (length dialog.context)))
                                 (list (make-seg :text dialog.context :tone :muted))))
                       width surface :col 0
                       :right (when (and tabs (plusp (length context)))
                                (list (make-seg :text context :tone :muted))))
                      (docked-panel-row '() width surface))
                (or (loop for segs in dialog.summary-rows
                          collect (docked-panel-row segs width surface :mark *dialog-row-overflow-mark*))
                    ;; A summary too long for the row wears the list rows' cut
                    ;; mark, where it stops at the edge. A live query stays
                    ;; bare: the caret parks past its end.
                    (list (docked-panel-row (docked-dialog-third-row-segs dialog)
                                            width surface
                                            :mark (and (not dialog.searchable)
                                                       *dialog-row-overflow-mark*))))
                (list (docked-panel-row '() width surface)))))
    (subseq rows 0 (min count (length rows)))))

(defparameter *dialog-input-field-gap* 2
  "Columns between an input field's label and its value (Zig
connectDialogInputFocus field_col).")

(defun dialog-input-field (dialog)
  "=> (values LABEL SHOWN-VALUE) for a dialog in input mode (the Zig
connectInputFieldLabel pair), or NIL for every list body: the text, or one
* per byte of a secret — never the secret."
  ;; One seam: the field row paints from it and the hardware cursor parks from
  ;; it.
  (when (and dialog dialog.input-label)
    (values dialog.input-label
            (if dialog.input-secret-p
                (make-string (length dialog.input) :initial-element #\*)
                dialog.input))))

(defparameter *picture-beside-gap* 4
  "Columns between a picture standing beside the list and the list.")

(defparameter *picture-beside-list-room* 36
  "The fewest columns a list keeps beside a picture; a narrower panel stands
the picture over it.")

(defun dialog-picture-rows (dialog body width &aux (picture (picker-pictures dialog)))
  "=> (values ROWS WHOLE LEAD): the rows DIALOG's picture takes at the top of
a BODY-row panel body WIDTH columns wide, the blank under it counted, and
whether it is painted whole -- else one line in its place says what it is
and that a larger window shows it. 0 for a dialog with none, or a body too
short to say even that. LEAD is the columns a picture standing beside the
list takes left of it, its gap counted: then ROWS is 0, nothing stands over
the list."
  ;; One reading for the paint and the hit-test (DIALOG-BODY-BAND): the list
  ;; starts where this says the picture ends.
  (if (null picture)
      0
      (let* ((rows (getf picture :rows))
             (lines (ceiling (length rows) 2))
             (wide (reduce #'max rows :key #'length))
             (room (docked-panel-room width)))
        (cond ((and (getf picture :beside)
                    (<= lines body)
                    (<= (+ wide *picture-beside-gap* *picture-beside-list-room*) room))
               (values 0 t (+ wide *picture-beside-gap*)))
              ((and (<= (+ lines 2) body) (<= wide room))
               (values (1+ lines) t nil))
              ((>= body 3) (values 2 nil nil))
              (t 0)))))

(defun picture-lines (rows paper-lit &aux (wide (reduce #'max rows :key #'length)))
  "ROWS (strings of `1' ink and `0' paper) as text, two pixel rows to a line
in half blocks. PAPER-LIT draws the paper as the glyph, for a panel whose
text is lighter than its ground, and the ink otherwise: paper reads light and
ink dark on either, which is what a camera reading a code needs."
  (flet ((lit-p (row x)
           ;; Past a short row is paper; past the last row is the ground.
           (and row (eq paper-lit (not (and (< x (length row)) (char= #\1 (char row x))))))))
    (loop for (top bottom) on rows by #'cddr
          collect (coerce (loop for x below wide
                                for up = (lit-p top x)
                                for down = (lit-p bottom x)
                                collect (char "█▀▄ " (+ (if up 0 2) (if down 0 1))))
                          'string))))

(defun picture-palette-segs (rows palette surface)
  "ROWS of PALETTE digits as segment lines, two pixel rows to a line: each
cell an upper half block whose ink is the top pixel's tone and whose ground
is the bottom one's, an empty or missing tone the panel's SURFACE."
  ;; A tone names its color by its foreground, or its ground when it has
  ;; none. Runs of one pair of colors paint as one segment.
  (let ((colors (map 'vector (lambda (name)
                               (and (plusp (length name))
                                    (let ((style (style-for-tone (intern (string-upcase name) :keyword))))
                                      (or (visual-style-fg style) (visual-style-bg style)))))
                     palette))
        (wide (reduce #'max rows :key #'length)))
    (flet ((color (row x &aux (index (and row (< x (length row)) (digit-char-p (char row x) 36))))
             (or (and index (< index (length colors)) (aref colors index)) surface)))
      (loop for (top bottom) on rows by #'cddr
            collect (loop for pairs = (loop for x below wide
                                            collect (cons (color top x) (color bottom x)))
                            then (nthcdr run pairs)
                          for pair = (first pairs)
                          for run = (or (position pair pairs :test-not #'equal) (length pairs))
                          while pairs
                          collect (make-seg :text (make-string run :initial-element #\▀)
                                            :tone (vstyle :fg (car pair) :bg (cdr pair))))))))

(defun picture-segs (picture surface &aux (rows (getf picture :rows)))
  "PICTURE's lines as segment lists on a panel of SURFACE: in its palette's
tones, or -- with no palette, or under a profile that paints no color -- in
ink and paper (PICTURE-LINES)."
  (if (and surface (getf picture :palette))
      (picture-palette-segs rows (getf picture :palette) surface)
      (let* ((fg (visual-style-fg (style-for-tone :foreground)))
             ;; No surface is the :none profile, painted as a dark terminal's.
             (paper-lit (or (null surface) (null fg) (> (reduce #'+ fg) (reduce #'+ surface))))
             (ink (if (getf picture :palette)
                      ;; Ink and paper of a palette picture: a tone is ink, its
                      ;; first entry (the ground) paper.
                      (mapcar (lambda (row) (substitute-if #\1 (lambda (char) (char/= char #\0)) row))
                              rows)
                      rows)))
        (mapcar (lambda (line) (list (seg line :foreground))) (picture-lines ink paper-lit)))))

(defun docked-dialog-picture-rows (dialog width surface whole
                                   &aux (picture (picker-pictures dialog)))
  "DIALOG's picture as panel rows and a blank under it: the picture when
WHOLE, else the one line that stands in for it."
  (append
   (if whole
       (mapcar (lambda (segs) (docked-panel-row segs width surface))
               (picture-segs picture surface))
       (list (docked-panel-row
              (list (seg (format nil "~a: a larger window shows it"
                                 (or (json-value (getf picture :label) :text) "The picture"))
                         :muted))
              width surface)))
   (list (docked-panel-row '() width surface))))

(defun docked-dialog-body-rows (dialog width surface count)
  "The panel body, always exactly COUNT rows: the dialog's picture, when it
carries one, then the list under it -- or, standing beside the list, the
list with the picture in its lead lane."
  (multiple-value-bind (picture whole lead) (dialog-picture-rows dialog count width)
    (if lead
        (docked-dialog-list-rows dialog width surface count
                                 :lead (picture-segs (picker-pictures dialog) surface)
                                 :lead-width lead)
        (append (and (plusp picture) (docked-dialog-picture-rows dialog width surface whole))
                (docked-dialog-list-rows dialog width surface (- count picture))))))

(defun dialog-field-rows (dialog width surface)
  "The rows DIALOG's body shows in place of a list: the input mode's field,
or the /models wizard's check and its result -- the lead row, the lines a
result breaks to, a blank, the muted context -- or NIL for a list body."
  (multiple-value-bind (lead context more)
      (cond
        ((picker-dialog-input-label dialog)
         ;; The field name, the gap, then the value — `(empty)' while
         ;; nothing is typed; the hardware cursor is the caret.
         (multiple-value-bind (label value) (dialog-input-field dialog)
           (values (list (seg label :system)
                         (seg (format nil "~va" *dialog-input-field-gap* "") :system)
                         (if (plusp (length value)) (seg value :composer) (seg "(empty)" :muted)))
                   dialog.input-context)))
        ((and (connect-dialog-p dialog)
              (member (connect-dialog-step dialog) '(:result :checking :installing :signing-in)))
         ;; A result's words break to the panel's width, as a toast's
         ;; do, and MORE carries the lines after the first: a refusal
         ;; ran its remedy off the edge, past column 72 at 80 columns.
         (let* ((checking (member (connect-dialog-step dialog) '(:checking :installing :signing-in)))
                (room (max 8 (docked-panel-room width)))
                (lines (if checking
                           (list (connect-checking-clock dialog))
                           (loop for line in (split-lines (connect-dialog-result-message dialog))
                                 nconc (wrapped-lines line room)))))
           (values (list (seg (or (first lines) "")
                              (if checking :muted :danger)))
                   (connect-provider-name dialog)
                   (if (eq :signing-in (connect-dialog-step dialog))
                       ;; under the clock, where to sign in -- the page a link,
                       ;; its query cut, then the whole address verbatim and
                       ;; dimmed when it carries more than the page -- and the
                       ;; device code when there is one; a command that
                       ;; answered no page says its words
                       (multiple-value-bind (url code) (sign-in-facts (connect-dialog-result-message dialog))
                         (if url
                             ;; a blank row ahead of each part, so the clock,
                             ;; the page, the address and the code stand apart
                             (loop for part in (list (list (list (make-seg :text (format nil "Sign in at ~a" (sign-in-page-label url))
                                                                           :tone :foreground :link url)))
                                                     (when (find-if (lambda (c) (member c '(#\? #\#))) url)
                                                       (loop for piece in (wrapped-lines url room)
                                                             collect (list (make-seg :text piece :tone :muted :link url))))
                                                     (and code (list (list (make-seg :text (format nil "Code ~a" code)
                                                                                     :tone :foreground :bold t)))))
                                   when part nconc (cons '() part))
                             (loop for line in (split-lines (connect-dialog-result-message dialog))
                                   nconc (wrapped-lines line room))))
                       (rest lines))))))
    (when lead
      (append (list (docked-panel-row lead width surface))
              (loop for line in more
                    collect (docked-panel-row (if (stringp line) (list (seg line :danger)) line)
                                              width surface))
              (list (docked-panel-row '() width surface))
              (when (plusp (length context))
                (list (docked-panel-row (list (seg context :muted)) width surface)))))))

(defun docked-dialog-list-rows (dialog width surface count &key lead (lead-width 0))
  "The panel body's list: the windowed list rows with the scrollbar column,
the wizard's input body, or the empty label — always exactly COUNT rows.
LEAD, segment lines, stands in a LEAD-WIDTH lane left of the list, a line a
row (DOCKED-PANEL-ROW's LEAD): a picture beside it."
  (when (plusp count)
    ;; Clamped to COUNT and blank-padded up to it: the body always fills its height.
    (let ((rows (or (dialog-field-rows dialog width surface)
                  (let* ((rows (picker-visible-rows dialog))
                         (total (length rows))
                         (tabbed (and (picker-dialog-tabs dialog) t))
                         (columns (flet ((segs-width (row field text)
                                           (segs-display-width
                                            (picker-field-segs row field text :system))))
                                    (loop for row in (picker-dialog-rows dialog)
                                          maximize (segs-width row :key row.key) into key
                                          maximize (segs-width row :label row.label) into label
                                          maximize (text-display-width row.mark) into mark
                                          finally (return (list (if tabbed 0 (or key 0))
                                                                (or label 0) (or mark 0))))))
                         (room (max 1 (- (docked-panel-room width t)
                                         lead-width
                                         (if (or (null surface) dialog.option-marker) 2 0)
                                         (if (plusp (third columns)) (1+ (third columns)) 0)))))
                    ;; Leave a readable description lane on narrow panels; clip
                    ;; overlong labels before padding, not through their detail.
                    (when (some (lambda (row) (plusp (length row.detail))) rows)
                      (setf (first columns) (min (first columns) (floor room 3))
                            (second columns) (min (second columns)
                                                  (max 1 (- room (if (plusp (first columns)) (+ 2 (first columns)) 0)
                                                            (max 14 (floor room 2)))))))
                    (if (zerop total)
                        ;; The body's own copy while no row shows. A live query that
                        ;; hid a non-empty list reads differently from a list that
                        ;; was empty to begin with (the Zig empty_label swap); a
                        ;; dialog that declares no distinguished copy — the wizard's
                        ;; non-searching steps, whose plain copy is the field's rule
                        ;; line — falls through to the plain one.
                        (list (docked-panel-row
                               (let ((label (or (and dialog.searchable
                                                     (plusp (length dialog.search))
                                                     (plusp (length dialog.rows))
                                                     dialog.no-match-label)
                                                dialog.empty-label)))
                                 (when (plusp (length label))
                                   (list (make-seg :text label :tone :muted))))
                               width surface :lead (first lead) :lead-width lead-width))
                        (bind (((selection offset visible) (dialog-body-viewport dialog count))
                               ((thumb-top thumb-height)
                                (scrollbar-thumb count visible total offset
                                                 :round-fn #'ceiling :fits-nil t)))
                          (loop for row from 0 below count
                                for index = (+ offset row)
                                for entry = (and (< index (+ offset visible)) (nth index rows))
                                for paint = (and entry (picker-row-paint entry))
                                for room = (- (docked-panel-room width thumb-height) lead-width)
                                collect (if paint
                                            ;; A row that lays itself starts
                                            ;; at the content column's own
                                            ;; cell, its edge there, on the
                                            ;; ground it names, and keeps one
                                            ;; cell of the panel's between it
                                            ;; and the bar.
                                            (let ((lane (- (+ room *docked-panel-content-col*)
                                                           (if thumb-height 1 0))))
                                              (multiple-value-bind (segs ground)
                                                  (funcall paint (= index selection) lane)
                                                (docked-panel-row
                                                 (if thumb-height
                                                     (let ((kept (clip-segs segs lane :mark *dialog-row-overflow-mark*)))
                                                       (append kept
                                                               (list (make-seg :text (make-string (max 0 (- lane (segs-display-width kept)))
                                                                                                  :initial-element #\Space)
                                                                               :tone :muted)
                                                                     (make-seg :text " " :tone (if surface (vstyle :bg surface) :muted)))))
                                                     segs)
                                                 width surface
                                                 :col 0
                                                 :ground (if (and surface ground)
                                                             (visual-style-bg (style-for-tone ground))
                                                             surface)
                                                 :scrollbar (scrollbar-cell thumb-top thumb-height row)
                                                 :mark *dialog-row-overflow-mark*
                                                 :lead (nth row lead) :lead-width lead-width)))
                                            (docked-panel-row
                                             (when entry
                                               (dialog-entry-segs
                                                entry
                                                (= index selection)
                                                ;; A tabbed dialog spends no
                                                ;; column on the bucket its tab
                                                ;; row says (PICKER-DIALOG-TABS).
                                                :tabbed tabbed :columns columns
                                                :marker (or (null surface) dialog.option-marker)
                                                :room room))
                                             width surface
                                             :scrollbar (scrollbar-cell thumb-top thumb-height row)
                                             :mark *dialog-row-overflow-mark*
                                             :lead (nth row lead) :lead-width lead-width)))))))))
      (loop for row below count
            collect (or (pop rows)
                        (docked-panel-row '() width surface
                                          :lead (nth row lead) :lead-width lead-width))))))

(defun dialog-panel-rows (model width bg &aux (dialog (chat-model-dialog model))
                                              (panel (docked-dialog-frame-rows model)))
  "The open dialog's docked panel: the painted rows COMPOSE-FRAME stacks over
the band's tail, the transcript/composer gap, and the composer."
  ;; NIL while no dialog is open. The footer is a blank, the hint row, a
  ;; blank, truncated to its band; a one-row footer keeps only the blank —
  ;; Zig's renderer prints its hint at row 1 and returns early below two rows.
  (when (and dialog panel)
    (let* ((surface (docked-panel-surface-bg))
           (inner (- panel 2))
           (header (min (dialog-header-row-count dialog) inner))
           (footer (min (dialog-footer-row-count dialog) (- inner header)))
           ;; The key-hint sets by dialog kind, verbatim from the Zig renderDockedKeyHintFooter
           ;; call sites — keys and their trailing spacing exactly as Zig writes them,
           ;; minus chords this shell does not bind (the session tree's ctrl+f fork and
           ;; tab filter, both Zig-only, would name keys that do nothing here; ctrl+f
           ;; is the fullscreen toggle every dialog shares, appended once below).
           ;; /sessions says only what its rows do -- Esc, the arrows and typing
           ;; are every list's -- so its row holds five caps.
           (hints (if (consp dialog.footer-hints)
                      dialog.footer-hints
                      (cdr (assoc dialog.footer-hints
                              '((:sessions ("enter" . "open") ("tab" . "this folder/all")
                                 ("→" . "forks") ("f2" . "rename"))
                                (:session-tree ("enter" . "navigate") ("esc" . "close")
                                 ("up/down" . "select")
                                 ("left/right" . "fold"))
                                (:checkpoint ("enter" . "select") ("esc" . "close")
                                 ("up/down" . "navigate"))
                                (:help ("esc" . "close") ("up/down" . "navigate")))))))
           (verb (if (chat-model-dialog-fullscreen model) "restore" "full screen"))
           ;; A docked panel stands where the composer was, so its key row says
           ;; the composer status for its first five seconds: a pick refused, a
           ;; key saved, a row's line run, while the panel stays or comes back.
           (said (let ((status (active-status model)))
                   (and status (< (since-ms (composer-status-born status)) 5000) status)))
           ;; Without key hints: the picker's own HINT, else the Zig default copy.
           (hint (or dialog.hint
                     (cond ((picker-dialog-searchable dialog)
                            "esc close  up/down navigate  type search")
                           ((picker-dialog-on-detail dialog)
                            "enter select  i details  esc close")
                           (t "esc close  up/down navigate")))))
      ;; One padding row above, then the three bands; the area's bottom
      ;; padding row is the canvas gap and belongs to the frame, not here.
      (append (list (docked-panel-row '() width surface))
              (docked-dialog-header-rows dialog width surface header)
              (docked-dialog-body-rows dialog width surface (- inner header footer))
              (subseq (list (docked-panel-row '() width surface)
                            ;; One unit per hint: a key-hint set's key and
                            ;; label, or a HINT's two-space-separated runs.
                            (if said
                                (docked-panel-row
                                 (list (make-seg :text (substitute #\Space #\Newline (composer-status-text said))
                                                 :tone (if (composer-status-error-p said) :danger :muted)))
                                 width surface :mark *dialog-row-overflow-mark*)
                            (docked-panel-row
                             (fitted-hint-segs
                              ;; A key-hint set's keys are filled caps, as a
                              ;; sheet's are (PAINT-KEYS). A hint naming one
                              ;; key is that key to click (HINT-KEY-PRESS), its
                              ;; words lit under the pointer as a link's are.
                              (flet ((target (key)
                                       (and (hint-key-press key) (cons :key key)))
                                     (tone (target)
                                       (if (and target (equal target (chat-model-hover-target model)))
                                           :disclosure-hover
                                           :muted)))
                                (if hints
                                    (loop for (key . label) in (append hints `(("ctrl+f" . ,verb)))
                                          for target = (target key)
                                          collect (list (make-seg :text (format nil " ~a " key)
                                                                  :tone (style-for-tone :key-chip)
                                                                  :link target)
                                                        (make-seg :text (format nil " ~a" label)
                                                                  :tone (tone target) :link target)))
                                    (loop for run in (ppcre:split "  +" (format nil "~a  ctrl+f ~a"
                                                                                hint verb))
                                          for target = (target (subseq run 0 (position #\Space run)))
                                          collect (list (make-seg :text run :tone (tone target)
                                                                  :link target)))))
                              (docked-panel-room width))
                             width surface :mark *dialog-row-overflow-mark*))
                            (docked-panel-row '() width surface))
                      0 (min footer 3))))))

(defun fitted-hint-segs (units room &aux (used 0) (segs '()))
  "UNITS -- each one key hint's segments -- two columns apart, as many of them
from the first as ROOM columns hold whole: the list's order is its priority,
so the hints that do not fit drop from its end."
  ;; The first hint stands whatever its width -- a HINT that is one long
  ;; sentence -- and the row's own cut mark says where it stops.
  (dolist (unit units)
    (let ((width (+ (segs-display-width unit) (if segs 2 0))))
      (when (and segs (> (+ used width) room))
        (return))
      (when segs
        (push (make-seg :text "  " :tone :muted) segs))
      (dolist (seg unit)
        (push seg segs))
      (incf used width)))
  (nreverse segs))

(defun dialog-list-selectable-p (dialog)
  "Whether DIALOG draws selectable list rows: a picker outside input mode,
including /models's provider step (also used by /setup), but not the
wizard's credential fields, check or result body."
  ;; The hover, the wheel, the bar and row clicks share this Boolean gate.
  (and (picker-dialog-p dialog)
       (not (sheet-dialog-p dialog))
       (null dialog.input-label)
       (or (not (connect-dialog-p dialog))
           (eq :provider (connect-dialog-step dialog)))))

(defun dialog-body-band (model dialog)
  "=> (values FIRST-ROW HEIGHT): the 1-based frame row DIALOG's list body
starts on and how many rows it spans, or NIL while no panel fits."
  ;; The decomposition is DIALOG-PANEL-ROWS's — the panel's padding row, the
  ;; header band, then the body — and this is the one authority for that band:
  ;; the hover hit-test and the wheel both measure the body through here.
  (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
    (when panel
      (let* ((inner (- panel 2))
             (header (min (dialog-header-row-count dialog) inner))
             (body (- inner header (min (dialog-footer-row-count dialog) (- inner header))))
             ;; A picture above the list is not rows a pointer picks.
             (picture (dialog-picture-rows dialog body (max 8 (chat-model-term-width model)))))
        (values (+ above 2 header picture) (- body picture))))))

(defun dialog-body-row-at (model y &aux (dialog (chat-model-dialog model)))
  "=> (values BODY-ROW HEIGHT): the 0-based row of the open dialog's list body
under frame mouse Y (1-based), and that body's height; NIL when Y is outside
the body or the panel owns no list."
  (when (dialog-list-selectable-p dialog)
    (multiple-value-bind (first height) (dialog-body-band model dialog)
      (when (and first (<= first y (+ first (1- height))))
        (values (- y first) height)))))

(defun dialog-hover-index (model y)
  "Frame mouse Y (1-based) to the visible-row index under the pointer in
the open picker's list body, or NIL outside the body or past its last
occupied row — the Zig handleActiveDialogHover hit-test
(dialogBodySelectionIndex through the shared list viewport)."
  ;; The window it indexes into is the one the paint settled
  ;; (DIALOG-BODY-VIEWPORT), so the row under the pointer is the row the
  ;; operator sees highlighted.
  (bind (((row height) (dialog-body-row-at model y)))
    (when row
      (bind (((_ offset visible) (dialog-body-viewport (chat-model-dialog model) height)))
        (when (< row visible)
          (+ offset row))))))

(defun dialog-apply-hover (model y &aux (dialog (chat-model-dialog model)))
  "Pull the open picker's selection onto the row under the pointer — the
Zig handleActiveDialogHover seam shared by every list dialog."
  ;; => T when Y
  ;; lands on an occupied body row. A selection move re-fires the highlight
  ;; seam, so hover previews exactly like keyboard navigation.
  ;; DIALOG-HOVER-INDEX answers only over a picker's selectable list.
  ;; A heading row takes no hover, so a click on one opens nothing.
  (when-let (index (dialog-hover-index model y))
    (unless (picker-row-heading (nth index (picker-visible-rows dialog)))
      (unless (= index dialog.selection)
        (setf dialog.selection index)
        ;; A hover landing is a selection move: deliver instantly, the
        ;; same scrub-through contract as a held arrow.
        (picker-notify-highlight model dialog :instant t))
      t)))

;;; --- the list dialog's own scrollbar ---------------------------------------
;;; The picker paints a scrollbar (renderShortcutPickerScrollbar) like the
;;; transcript's gutter, so it answers the pointer like one: the bar is
;;; grabbable and the grab drags the window. The wheel (HANDLE-MOUSE-WHEEL)
;;; moves the window under a stationary pointer; the bar moves it under a held
;;; button, and the highlight rides the window's edge where the wheel leaves
;;; it under the pointer. One predicate decides which panels follow the mouse
;;; at all (DIALOG-LIST-SELECTABLE-P), so the hover, the wheel and the bar
;;; cannot disagree about the body they measure.

(defun dialog-scrollbar-metrics (model &aux (dialog (chat-model-dialog model)))
  "=> (values FIRST HEIGHT TOTAL VISIBLE), the open dialog's list bar: the
1-based frame row its track starts on, the track's height in rows, the row
total and the rows the window shows — the band DIALOG-BODY-BAND hands the
hover and the wheel, measured once more for the bar."
  ;; NIL while no list dialog is open or its panel holds no body.
  (when (dialog-list-selectable-p dialog)
    (multiple-value-bind (first height) (dialog-body-band model dialog)
      (when (and first (plusp height))
        (let ((total (length (picker-visible-rows dialog))))
          (values first height total (min total height)))))))

(defun apply-dialog-scrollbar-track-row (model track-row grab-offset)
  "Move the open dialog's list so its thumb top lands on TRACK-ROW minus
GRAB-OFFSET — the dialog's tail of APPLY-SCROLLBAR-TRACK-ROW, inverting the
picker's own ceiling thumb."
  ;;
  ;; The list carries a highlight where the transcript carries none, and the
  ;; settled window is the one the paint pulls the highlight into: the drag takes
  ;; the highlight with the window rather than leaving the paint to pull the
  ;; window back, so a row dragged out of the window keeps its place in the list
  ;; and only the window's edge moves it. Nothing happens while the list fits
  ;; (no bar is painted) or the track row is the only one there is.
  (bind (((_ height total visible) (dialog-scrollbar-metrics model)))
    (when (and height (plusp total) (plusp (- total visible)))
      (let ((offset (scroll-for-thumb-top
                     height visible total
                     (max 0 (- (max 0 (min track-row (1- height)))
                               grab-offset))
                     :round-fn #'ceiling))
            (dialog (chat-model-dialog model)))
        (setf dialog.view-offset offset
              dialog.selection
              (max offset (min dialog.selection
                               (+ offset (1- visible)))))))))

;;; --- the /models wizard -----------------------------------------------------
;;; src/tui/model/models_dialog.zig distilled onto the dialog slot: a
;;; four-step flow (provider list -> optional custom id -> API key -> a
;;; failure's result) with the runtime.zig handleConnectDialogKey key ladder.
;;; A saved key returns to the list, which then opens on the connected
;;; providers, so one open connects as many providers as the operator has
;;; keys for. The dialog is transport-free: the shells inject the rows,
;;; ON-SAVE (persist the credential, => (values message error-p)) and
;;; ON-SUCCESS (what Enter on a connected provider opens — the models
;;; dialog, per the Zig flow).

;;; On the two input steps the :EMPTY-LABEL string is the field's own rule
;;; line rather than an empty-list label, which is the slot the Zig body
;;; prints it in. Verbatim minus the connect_setup_required arms — the Lisp
;;; shell has no startup-forced connect flow, so it has no `ctrl+c twice
;;; quits` state to describe.
(defparameter +connect-steps+
  '((:provider
     ;; no context: /models' tab strip says where the list is, and a walk its step
     :context "" :searchable t
     :subtitle "Choose a provider from models.dev."
     ;; /models' own, under its Main and Auxiliary tabs (a walk has no strip)
     :hint "enter select  ←→ main/auxiliary/decider/backup  ctrl+e new key  esc close  ↑↓ navigate"
     ;; a walk's Esc moves it on rather than closing it
     :walk-hint "enter select  ctrl+e new key  esc skip  ←→ steps  ↑↓ navigate  type search"
     :empty-label "Other provider" :no-match-label "No matching providers")
    (:custom-provider
     :context "Provider id" :input-label "Provider id"
     :subtitle "Enter a models.dev provider id."
     :hint "enter continue  esc back  ctrl+u clear  type provider id"
     :empty-label "Provider id must be lowercase letters, numbers, hyphen, or underscore.")
    (:api-key
     :context "API key" :input-label "API key" :input-secret-p t
     ;; a control string: CONNECT-ENTER-STEP fills in the provider and the
     ;; page that makes its key
     :subtitle "Paste your ~a API key.~@[ ~a makes one.~]"
     :hint "enter save  esc back  ctrl+u clear  type secret"
     :empty-label "No secret entered")
    (:checking
     :context "API key"
     ;; a control string: CONNECT-ENTER-STEP fills in the provider and the
     ;; seconds PROVIDER-KEY-CHECK waits
     :subtitle "Asking ~a whether it takes this key, waiting up to ~d seconds."
     :hint "esc back"
     :empty-label "")
    (:installing
     :context "Install"
     ;; a control string: CONNECT-INSTALL fills in the folder and its pin
     :subtitle "Installing ~a~@[ at ~a~], the cell that serves this provider."
     :hint "esc back"
     :empty-label "")
    (:signing-in
     :context "Sign in"
     ;; none: the panel names the provider under the page already
     :subtitle ""
     :hint "esc back"
     :empty-label "")
    (:result
     :context "Errors"
     :subtitle "Credential setup needs attention."
     :hint "enter retry  esc back"
     :empty-label ""))
  "The Zig connectDialogView switch, one row per step, keyed by the PICKER
slot each string lands in: the muted context label beside the Connect tab
(connectHeaderStepLabel), the summary the header's third row carries
wherever the step is not searching, the footer copy, the body's empty /
no-match labels, and the input mode's field label and masking on the two
text steps.")

(defun connect-enter-step (dialog step &aux (spec (rest (assoc step +connect-steps+))))
  "Move DIALOG onto STEP and paint the chrome the step owns onto the picker
slots the panel already reads."
  ;; STEP names a +CONNECT-STEPS+ row, so the two result flavours say which
  ;; they are (:RESULT-OK / :RESULT-ERROR) and both land on the single :RESULT
  ;; the flow tests. Writing the chrome here — rather than deriving it at
  ;; every paint — is what keeps one panel shape: past this line the wizard is
  ;; an ordinary picker whose header and footer move.
  (setf (connect-dialog-step dialog) step)
  (dolist (slot '(searchable context subtitle hint empty-label no-match-label
                  input-label input-secret-p))
    (setf (slot-value dialog slot) (getf spec (intern (string slot) :keyword))))
  ;; The sign-in step has no summary: its header is the title row and a
  ;; blank, so the clock sits one blank under the title, not three.
  (setf dialog.header-rows (and (eq step :signing-in) 2))
  (when-let (walk (connect-dialog-walk dialog))
    (setf dialog.context walk
          dialog.hint (or (getf spec :walk-hint) dialog.hint)))
  ;; The key step asks for the provider's key by name (`Paste your Google API
  ;; key.'), so the name no longer sits under the field.
  (when (eq step :api-key)
    (setf dialog.subtitle (format nil dialog.subtitle (connect-provider-name dialog)
                                  (connect-dialog-key-page dialog))))
  ;; The check says how long it waits and counts up to it, as the import's
  ;; does: a static `checking...' would read as a hang. The tick repaints
  ;; when the tenth shown moves.
  (setf (connect-dialog-checking-since dialog) (and (member step '(:checking :installing :signing-in))
                                                    (get-internal-real-time))
        dialog.on-tick (and (member step '(:checking :installing :signing-in))
                            (let ((shown nil))
                              (lambda (model dialog &aux (now (connect-checking-clock dialog)))
                                (declare (ignore model))
                                (unless (equal now shown)
                                  (setf shown now))))))
  (when (eq step :checking)
    (setf dialog.subtitle (format nil dialog.subtitle (connect-provider-name dialog)
                                  *provider-models-fetch-timeout-seconds*)))
  ;; The two text steps are the picker's input mode (a step with no
  ;; :INPUT-LABEL leaves it): the id step reopens on the id a failed save
  ;; returned from, under its rule line; the key step starts empty, under
  ;; the provider it is for.
  (setf dialog.input (if (and (eq step :custom-provider) (connect-dialog-custom-p dialog))
                         (connect-dialog-provider-id dialog)
                         "")
        dialog.input-context (if (eq step :api-key) "" (getf spec :empty-label))))

(defun connect-checking-clock (dialog &aux (since (connect-dialog-checking-since dialog)))
  "`checking… 1.3s', the checking step's body line, counting from when the
step began; `installing… 1.3s' on the installing step, `waiting for the
sign-in… 1.3s' on the sign-in step."
  (format nil "~a… ~a" (case (connect-dialog-step dialog)
                         (:installing "installing")
                         (:signing-in "waiting for the sign-in")
                         (t "checking"))
          (format-turn-elapsed (if since (since-ms since) 0))))

(defun connect-provider-name (dialog)
  "The provider as the wizard names it: the catalog label, else the id."
  (or (json-value (connect-dialog-provider-label dialog) :text)
      (connect-dialog-provider-id dialog)))

(defun connect-clear-credential (dialog)
  "Drop everything the flow has gathered about a provider."
  ;; Backing out to the list and picking a fresh entry both start from
  ;; nothing, and the secret goes with the rest (the Zig zero-on-clear
  ;; discipline).
  (setf (connect-dialog-provider-id dialog) ""
        (connect-dialog-provider-label dialog) ""
        (connect-dialog-custom-p dialog) nil
        (connect-dialog-key-page dialog) nil
        (connect-dialog-retry dialog) nil
        dialog.input ""))

(defun connect-ask-key (dialog row &aux (value (and row (picker-row-value row))))
  "Move DIALOG from the provider list to what ROW asks for: Other
provider's id, a provider's key. Any other row moves nothing."
  (when (or (getf value :custom) (getf value :provider))
    (connect-clear-credential dialog)
    (if (getf value :custom)
        (setf (connect-dialog-custom-p dialog) t)
        (setf (connect-dialog-provider-id dialog) (getf value :provider)
              (connect-dialog-provider-label dialog) (picker-row-label row)
              (connect-dialog-key-page dialog) (getf value :key-page)))
    (connect-enter-step dialog (if (getf value :custom) :custom-provider :api-key))))

(defun connect-install (model dialog row &key key &aux (value (picker-row-value row))
                                                     (offer (getf value :install))
                                                     (id (getf value :provider)))
  "Enter on ROW, a provider a folder not installed here serves
(PICKER-PROVIDER-OFFERS): install that folder through the organism, off the
loop (INSTALL-OFFERED-CELL), then go on as the provider asks -- the command
its folder signs in with, a pick when it answers already (a key it has, or a
login the folder brings: Claude Code's is the claude CLI's), else its key
step. KEY, Ctrl+E's, asks for a key whatever the folder signs in with. A
refused install says why over the list, and nothing else moved."
  (connect-clear-credential dialog)
  (setf (connect-dialog-provider-id dialog) id
        (connect-dialog-provider-label dialog) (picker-row-label row))
  (flet ((go-on (model commands &aux (how (getf offer :sign-in)))
           ;; the folder is in: on as the provider asks
           (when commands (set-gateway-commands commands))
           (setf dialog.rows (funcall (connect-dialog-provider-rows dialog)))
           (cond
             ;; the folder signs in with a command of its own:
             ;; it runs, and the wizard waits on its words
             ((and (not key) (uiop:string-prefix-p "/" how))
              (connect-sign-in model dialog how))
             ;; it answers already -- a key it had, or a login
             ;; its folder brings, which the reread rows say: a
             ;; pick, as Enter on a connected provider is
             ((and (not key)
                   (or (getf value :connected)
                       (find-if (lambda (row &aux (now (picker-row-value row)))
                                  (and (equal id (getf now :provider))
                                       (getf now :connected)))
                                dialog.rows)))
              (let ((on-success (connect-dialog-on-success dialog)))
                (dismiss-dialog model (and on-success
                                           (lambda (model) (funcall on-success model id))))))
             (t (connect-ask-key dialog row)))))
    (if (getf offer :installed)
        ;; in already, its provider not answering yet -- a sign-in Esc left
        ;; unfinished: nothing to install, on to it
        (go-on model nil)
        (progn
          (connect-enter-step dialog :installing)
          (setf dialog.subtitle (format nil dialog.subtitle (getf offer :cell)
                                        (let ((pin (getf offer :commit)))
                                          (and (stringp pin) (subseq pin 0 (min 7 (length pin)))))))
          (off-loop model
                    (lambda () (install-offered-cell model offer))
                    (lambda (model answer)
                      ;; Esc, or another dialog, moved the wizard on: the answer moves nothing
                      (if (not (and (eq dialog (chat-model-dialog model))
                                    (eq :installing (connect-dialog-step dialog))))
                          :quiet
                          (let ((refused (or (off-loop-failure answer) (getf answer :refused))))
                            (if refused
                                (progn
                                  (connect-clear-credential dialog)
                                  (connect-enter-step dialog :provider)
                                  (show-status model (format nil "~a was not installed: ~a" (getf offer :cell) refused)
                                               :error-p t))
                                (go-on model (getf answer :commands)))
                            (render-state model))))))))
  t)

(defun sign-in-facts (text &aux (text (or text "")))
  "The page TEXT, what a folder's sign-in command answered, sends the
operator to, and the device code it asks for there when it names one: =>
(values URL CODE), either NIL."
  ;; The code is the word after `code' that reads as one -- a digit in it, or
  ;; capitals around a dash: `enter the code ABCD-1234' or `NFGT-NHBJ', never
  ;; `send /openai-codex code ADDRESS' nor `Kimi Code sign-in'.
  (let ((words (loop with start = nil
                     for i from 0 to (length text)
                     for blank = (or (= i (length text))
                                     (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
                     if (and blank start) collect (subseq text start i) and do (setf start nil)
                     else if (and (not blank) (null start)) do (setf start i))))
    (values (let ((url (find-if (lambda (word &aux (host (search "://" word)))
                                  ;; a loopback address is where the browser
                                  ;; comes back to, never where to sign in
                                  (and host (< host 6)
                                       (member (subseq word 0 host) '("http" "https") :test #'string-equal)
                                       (notany (lambda (loopback)
                                                 (uiop:string-prefix-p loopback (subseq word (+ host 3))))
                                               '("localhost" "127." "[::1]"))))
                                words)))
              (and url (string-right-trim ".,;)" url)))
            (loop for (word next) on words
                  for code = (and next (string-trim ".,;:()'\"" next))
                  when (and code (string-equal (string-trim ":" word) "code")
                            (<= 4 (length code) 16)
                            (every (lambda (c) (or (alphanumericp c) (char= c #\-))) code)
                            (or (some #'digit-char-p code)
                                (and (find #\- code) (notany #'lower-case-p code))))
                    return code))))

(defun sign-in-page-label (url &aux (from (+ 3 (or (search "://" url) -3))))
  "URL as the sign-in panel names it: no scheme, no query."
  (subseq url from (or (position-if (lambda (c) (member c '(#\? #\#))) url :start from)
                       (length url))))

(defparameter *connect-sign-in-poll-seconds* 2
  "How long CONNECT-SIGN-IN waits between two reads of whether the provider
answers yet.")

(defun connect-sign-in (model dialog how &aux (id (connect-dialog-provider-id dialog)))
  "Run HOW, the command a folder just installed signs in with, and wait on
DIALOG for its provider to answer: what the command said -- the address to
open, a device code -- stands in the panel, and once the gateway's list reads
the provider connected the pick goes on to its models, as Enter on a
connected provider does. Esc goes back to the list (CONNECT-BACK), the
folder's sign-in still waiting without it."
  ;; Each read lands on the loop, which decides whether to read again: the
  ;; worker never looks at the dialog.
  (labels ((ours (model) (and (eq dialog (chat-model-dialog model))
                              (eq :signing-in (connect-dialog-step dialog))))
           (wait (model)
             (off-loop model
                       (lambda ()
                         (sleep *connect-sign-in-poll-seconds*)
                         (fetch-gateway-models model)
                         (and (member id (picker-connected-provider-ids) :test #'string-equal) t))
                       (lambda (model connected)
                         (cond ((not (ours model)) :quiet)
                               ((eq t connected)
                                (let ((on-success (connect-dialog-on-success dialog)))
                                  (dismiss-dialog model (and on-success
                                                             (lambda (model) (funcall on-success model id)))))
                                (render-state model))
                               (t (wait model) :quiet))))))
    (connect-enter-step dialog :signing-in)
    (setf dialog.subtitle (format nil dialog.subtitle (connect-provider-name dialog)))
    (off-loop model
              (lambda () (json-value (gateway-slash-answer model how) :string "text"))
              (lambda (model text &aux (failure (off-loop-failure text))
                                       (url (and (not failure) (sign-in-facts text))))
                (cond ((not (ours model)) :quiet)
                      ;; no page to sign in at: the command said why not --
                      ;; over the list, as a refused install says it
                      ((null url)
                       (setf (connect-dialog-result-message dialog) "")
                       (connect-clear-credential dialog)
                       (connect-enter-step dialog :provider)
                       (show-status model (if failure (format nil "~a: ~a" how failure) (or text how))
                                    :error-p t)
                       (render-state model))
                      (t (setf (connect-dialog-result-message dialog) text)
                         ;; the page opens where there is a browser to open it
                         (ignore-errors (open-target url))
                         (wait model)
                         (render-state model)))))))

(defun connect-back (model dialog)
  "One step back (the Zig backConnectDialog): from the provider step the
flow closes; either input step drops the half-gathered credential and
returns to the list; the result -- a failure's -- returns to the step it
interrupted, which is the custom-id step exactly when the id was typed
rather than picked, or to the list when nothing listened where the key was
checked."
  ;; Closing runs the dialog's
  ;; ON-CANCEL: a caller that chains the wizard — the setup walk — moves on by
  ;; it, from a saved key as from the list.
  (ecase (connect-dialog-step dialog)
    (:provider (dismiss-dialog model dialog.on-cancel))
    ((:custom-provider :api-key)
     (connect-clear-credential dialog)
     (connect-enter-step dialog :provider)
     (when (eq :model-providers (picker-dialog-kind dialog))
       (install-frequent-models model dialog
                                (json-object "models" (gethash dialog *models-frequent-by-dialog* #())))))
    ;; the answer, when it lands, finds the dialog moved on and takes nothing
    (:checking (connect-enter-step dialog :api-key))
    ;; the install goes on in the organism; its answer finds the list
    (:installing
     (connect-clear-credential dialog)
     (connect-enter-step dialog :provider))
    ;; the folder's sign-in goes on waiting; the list takes nothing from it
    (:signing-in
     (setf (connect-dialog-result-message dialog) "")
     (connect-clear-credential dialog)
     (connect-enter-step dialog :provider))
    (:result
     (setf (connect-dialog-result-message dialog) "")
     (cond
       ;; nothing listened where the key was checked: another key would meet
       ;; the same stopped server, so Esc goes back to the providers
       ((connect-dialog-retry dialog)
        (connect-clear-credential dialog)
        (connect-enter-step dialog :provider))
       (t (connect-enter-step dialog (if (connect-dialog-custom-p dialog) :custom-provider :api-key)))))))

(defun connect-dialog-key (model dialog key &key ctrl alt &aux (step (connect-dialog-step dialog)))
  "The keys the /models wizard takes away from the shared picker ladder
(runtime.zig handleConnectDialogKey): Esc steps back through the flow
instead of closing it, Enter advances the step instead of selecting a row,
and past the provider list every key belongs to the step, because no list is
showing for one to mean anything else."
  ;; => T when the wizard owned the key;
  ;; NIL hands it down to the picker ladder, where the provider list's navigation
  ;; and type-to-filter already live and need no second copy here.
  (cond
    ((and (eq key :escape)
          (or (not (eq step :provider))
              (zerop (length (connect-dialog-search dialog)))))
     (connect-back model dialog)
     t)
    ((and (eq step :provider) ctrl (characterp key) (char-equal key #\e))
     (let ((row (selected-of (picker-visible-rows dialog) (connect-dialog-selection dialog))))
       ;; a provider no installed folder serves yet is installed first
       (if (getf (and row (picker-row-value row)) :install)
           (connect-install model dialog row :key t)
           (connect-ask-key dialog row)))
     t)
    ((eq key :enter)
     ;; The Zig submit + persist ladder; a failed save is the result step's message.
     (ecase step
       (:provider
        (let* ((row (selected-of (picker-visible-rows dialog)
                                 (connect-dialog-selection dialog)))
               (value (and row (picker-row-value row)))
               (on-success (connect-dialog-on-success dialog)))
          (flet ((pick (choice)
                   (dismiss-dialog model (and on-success
                                              (lambda (model) (funcall on-success model choice))))))
            (cond ((getf value :model) (pick (getf value :model)))
                  ;; A provider a folder not installed here serves: the
                  ;; folder first, then what the provider asks for.
                  ((getf value :install) (connect-install model dialog row))
                  ;; A provider whose key answers is a pick, not a key to paste
                  ;; again: Enter picks it. Ctrl+E pastes a new one.
                  ((getf value :connected) (pick (getf value :provider)))
                  (t (connect-ask-key dialog row))))))
       (:custom-provider
        (let ((input dialog.input))
          ;; The Zig validProviderId rule; surrounding whitespace rejects, never trims.
          (when (ppcre:scan "^[a-z0-9][a-z0-9_-]*\\z" input)
            (setf (connect-dialog-provider-id dialog) input
                  (connect-dialog-provider-label dialog) input
                  (connect-dialog-custom-p dialog) t)
            ;; A provider no lane drives is refused before a key is asked
            ;; for: nothing could use the secret. Esc returns to the id.
            (if-let (refusal (provider-id-refusal input))
              (progn (setf (connect-dialog-result-message dialog)
                           (format nil "~a Nothing was saved." refusal))
                     (connect-enter-step dialog :result))
              (connect-enter-step dialog :api-key)))))
       (:api-key
        ;; The sink answers DONE once the provider has had its say on the
        ;; key, off the loop in a live shell; the step waits on it.
        (let ((on-save (connect-dialog-on-save dialog))
              (secret dialog.input))
          (setf dialog.input ""
                (connect-dialog-retry dialog) nil)
          (connect-enter-step dialog :checking)
          ;; A saved key needs no screen of its own: its line is the
          ;; notice over the list, rebuilt so the provider reads
          ;; connected, Suggested, and selected.
          (flet ((done (model message error-p)
                   (when (and (eq dialog (chat-model-dialog model))
                              (eq :checking (connect-dialog-step dialog)))
                     (cond (error-p
                            (setf (connect-dialog-result-message dialog) (or message ""))
                            (connect-enter-step dialog :result)
                            ;; nothing listened where the key was checked: the
                            ;; result keeps it, and Enter checks it again
                            (when (eq error-p :retry)
                              (setf (connect-dialog-retry dialog) secret
                                    dialog.hint "enter check again  esc other provider")))
                           (t
                            (show-status model message)
                            (connect-clear-credential dialog)
                            (setf dialog.rows (funcall (connect-dialog-provider-rows dialog))
                                  dialog.search "")
                            (picker-seat-top dialog)
                            (connect-enter-step dialog :provider)
                            ;; The answer lands off the loop, where no key's
                            ;; update repaints the band the notice joins.
                            (when (chat-model-band model)
                              (render-state model)))))))
            (if on-save
                (funcall on-save model (connect-dialog-provider-id dialog) secret #'done)
                (done model "no credential sink is wired" t)))))
       ((:checking :installing) nil)
       ;; a check nothing listened for is checked again, the same key
       (:result (if-let (secret (connect-dialog-retry dialog))
                  (progn (connect-enter-step dialog :api-key)
                         (setf dialog.input secret)
                         (connect-dialog-key model dialog :enter))
                  (connect-back model dialog))))
     t)
    ;; The two input steps hand every other key down to the picker's
    ;; input arm, which edits the field uncapped — a provider id or an
    ;; API key is not a search query, and Zig caps neither.
    ;; The result step has no field of its own: a stray character is
    ;; swallowed exactly as Zig swallows it, and the edit keys still reach
    ;; the provider query underneath, which is what backing out returns to.
    ((eq step :result)
     (unless (and (characterp key) (not ctrl) (not alt)
                  (graphic-char-p key))
       (when-let (new (edit-text-field (connect-dialog-search dialog) key
                                       :ctrl ctrl :alt alt))
         (picker-search-mutate dialog new)))
     t)))

(defparameter +loading-providers+ "Loading providers from models.dev…"
  "The provider list's search row while its first catalog fetch runs.")

(defun open-connect-dialog (model rows on-save on-success &key (title "Connect") kind)
  "Open the /models wizard over MODEL."
  ;; ROWS, a function of no arguments, answers the provider picker rows, at
  ;; open and after each saved key; ON-SAVE persists an entered key;
  ;; ON-SUCCESS is what Enter on a connected provider opens. A box
  ;; without the models.dev cache fetches it behind the list
  ;; (INSTALL-FETCHED-CATALOG), and the search row says so meanwhile: the list
  ;; holds the Other-provider entry alone until it lands.
  (let ((dialog (make-connect-dialog :title title :kind kind :rows (funcall rows) :provider-rows rows
                                     :search-placeholder (if (start-models-catalog-fetch model)
                                                             +loading-providers+
                                                             "Search providers")
                                     :on-save on-save
                                     :on-success on-success)))
    ;; The provider step's own chrome is not a defstruct default: it is the
    ;; first thing the flow enters, through the one seam every later step
    ;; enters through.
    (connect-enter-step dialog :provider)
    (picker-seat-top dialog)
    (setf (chat-model-dialog model) dialog))
  nil)

;;; The slash dropdown's rows render through the shared composer dropdown
;;; panel above (COMPOSER-DROPDOWN-PANEL-ROWS): one Zig row shape for every
;;; completion lane.

;;; --- help dialog -------------------------------------------------------------
;;; The Zig help_dialog.zig distilled onto the picker: the slash-command
;;; catalog (label + description) followed by the keybind listing, each
;;; keybind row showing the currently configured chord from the resolved
;;; keymap. Searchable, like every Zig help-dialog view.

(defun help-dialog-rows (model)
  "The help dialog's rows: every slash command (the gateway's cell commands
included), then every keybind action with its resolved key label."
  ;; A command carrying the Zig help_detail copy shows it; the dropdown
  ;; description covers the rest.
  (append
   (loop for entry in (append *slash-commands* *gateway-commands*)
         collect (make-picker-row
                  :label (slash-command-label entry)
                  :detail (or (json-value entry.help-detail :text) entry.description)))
   (loop for (action label) in *keybind-actions*
         collect (make-picker-row
                  :label (keybind-chords-label (chat-model-keymap model)
                                               action)
                  :detail label))))

(defun open-help-dialog (model)
  "Open the keyboard help dialog (the `?` trigger and the /help command)."
  ;; The Zig shortcut picker counts its entries inside the tab label
  ;; (shortcuts_tab_label).
  (let ((rows (help-dialog-rows model)))
    (open-picker-dialog model (format nil "Shortcuts (~d)" (length rows))
                        rows
                        nil
                        :searchable t
                        :search-placeholder "Type to search shortcuts…"
                        :empty-label "No shortcuts found."
                        :footer-hints :help))
  nil)

;;; --- prompt history --------------------------------------------------------
;;; The Zig composer history, distilled (src/tui/model/composer.zig +
;;; root.zig navigateComposerHistoryPrevious/Next): newest-first entries,
;;; dedupe-to-front on submit, a 1000-entry cap, prefix-filtered walking —
;;; the draft the walk started from filters which entries Up/Down visit and
;;; is restored when Down passes the newest match. Durable across restarts
;;; as one JSON-encoded prompt per line, appended on submit, loaded lazily
;;; on first recall.

(defparameter *max-composer-history-entries* 1000
  "Bound on recallable prompts (Zig max_composer_history_entries).")

(define-startup-parameter *composer-history-path*
  (home "composer-history")
  "Where submitted prompts persist, one JSON string per line. Tests rebind.")

(defun ensure-composer-history (model)
  (unless (chat-model-history-loaded model)
    ;; An unreadable line skips; a NIL path (the test runner's) persists nothing.
    (let ((entries (and *composer-history-path*
                        (read-json-lines *composer-history-path*
                                         :test (lambda (value) (json-value value :text))))))
      (setf (chat-model-prompt-history model)
            (subseq (reverse entries) 0 (min (length entries) *max-composer-history-entries*))
            (chat-model-history-loaded model) t))))

(defun reset-history-navigation (model)
  (setf (chat-model-history-index model) nil
        (chat-model-history-stash model) ""))

(defun note-submitted-prompt (model text)
  "Record TEXT on submit: dedupe to the front, cap, persist, and leave
navigation reset (Zig appendComposerHistory + history.appendToPath)."
  (ensure-composer-history model)
  (let ((entries (cons text (remove text (chat-model-prompt-history model)
                                    :test #'string=))))
    (setf (chat-model-prompt-history model)
          (subseq entries 0 (min (length entries)
                                 *max-composer-history-entries*))))
  (when *composer-history-path*
    (ignore-errors (append-json-line *composer-history-path* text)))
  (reset-history-navigation model))

(defun composer-cursor-at-end-p (ta &aux (value (editor-value ta))
                                         (newline (position #\Newline value :from-end t)))
  "Whether the caret sits at the very end of the composer text — the Zig
gate on entering history navigation."
  (and (= (editor-row ta)
          (1- (editor-line-count ta)))
       (= (editor-col ta) (- (length value) (if newline (1+ newline) 0)))))

(defun replace-composer-text (model text &aux (ta (chat-model-editor model)))
  "Install TEXT with the caret at its end: reset, then insert."
  (editor-reset ta)
  (editor-insert ta text))

(defun composer-history-previous (model)
  "Walk one entry older; T when the key was consumed."
  ;; Declines — so the cursor moves instead — when the caret is mid-text,
  ;; nothing matches, or history is empty (Zig
  ;; navigateComposerHistoryPrevious).
  (ensure-composer-history model)
  (let ((ta (chat-model-editor model)))
    (when (and (chat-model-prompt-history model)
               (composer-cursor-at-end-p ta))
      (let* ((index (chat-model-history-index model))
             (prefix (if index
                         (chat-model-history-stash model)
                         (editor-value ta)))
             (entries (chat-model-prompt-history model)))
        (loop for candidate from (if index (1+ index) 0)
                below (length entries)
              for entry = (nth candidate entries)
              when (and (uiop:string-prefix-p prefix entry)
                        ;; A fresh walk skips the entry equal to the draft.
                        (not (and (null index)
                                  (plusp (length prefix))
                                  (string= entry prefix))))
                do (when (null index)
                     (setf (chat-model-history-stash model) prefix))
                   (replace-composer-text model entry)
                   (setf (chat-model-history-index model) candidate)
                   (return t))))))

(defun composer-history-next (model)
  "Walk one entry newer, restoring the stashed draft past the newest match;
T when the key was consumed. Declines while not navigating."
  (when-let (index (chat-model-history-index model))
    (let ((prefix (chat-model-history-stash model))
          (entries (chat-model-prompt-history model)))
      (loop for candidate from (1- index) downto 0
            for entry = (nth candidate entries)
            when (uiop:string-prefix-p prefix entry)
              do (replace-composer-text model entry)
                 (setf (chat-model-history-index model) candidate)
                 (return-from composer-history-next t))
      (replace-composer-text model prefix)
      (reset-history-navigation model)
      t)))

(defparameter *max-prompt-history-rows* 5000
  "Bound on the prompts the history dialog reads out of the store in one
read, newest first — a bound on the read, not on the surface: a read that
hit it says so in the title.")

(defun prompt-history-origin (session-id)
  "Which lane a prompt arrived on, read off the session id that carried it."
  ;; The channel lanes and the cron jobs name themselves in their ids;
  ;; anything else is a shell of this home's own — the TUI's session, an
  ;; agent's, a subagent's.
  (or (find-if (lambda (lane) (uiop:string-prefix-p lane (or session-id "")))
               '("discord" "telegram" "cron"))
      "tui"))

(defun prompt-history (&key (limit *max-prompt-history-rows*))
  "Every prompt committed to a turn on this store, newest first, as (LANE
TEXT): the lane it arrived on (PROMPT-HISTORY-ORIGIN) and the prompt verbatim.
The organism's side of a shell's history: GET /api/gateway/prompts serves it."
  ;; The store is the only record that spans every way a prompt reaches this
  ;; organism — the TUI composer, a Discord or Telegram lane, a cron fire — so
  ;; this is what `every prompt' means; the composer ring beside it holds one
  ;; home's typed drafts and only those. Two kinds of row are not a person's
  ;; prompt and are left out: a `follow-up' disposition (queue state, whose
  ;; promotion commits the same prompt a second time as `started'), and the
  ;; reflection fork's own words ([experience] ...), which the harness injects
  ;; into a session rather than anyone typing them.
  ;;
  ;; => NIL when no store is open or it holds none.
  (when (nlk:store-open-p)
    ;; Keep the bound on input events BEFORE exclusions. Read plain prompts
    ;; directly; only structured messages need the canonical JSON/content
    ;; reader. Fetching and decoding the entire envelope cost ~1 s here.
    (loop for (session-id disposition prompt-id type raw) in
            (nlk:events :kind "turn.input_committed" :order :newest :limit limit
                        :as :rows
                        :columns '("session_id" "json_extract(payload, '$.disposition')"
                                   "json_extract(payload, '$.prompt-id')"
                                   "json_type(payload, '$.message')"
                                   "json_extract(payload, '$.message')"))
          unless (or (equal disposition "follow-up")
                     (and (stringp prompt-id) (uiop:string-prefix-p "experience:" prompt-id))
                     (and session-id (uiop:string-prefix-p "experience" session-id)))
            append (let ((text (cond ((equal type "text") raw)
                                     ((equal type "object") (message-content (nlk:decode-json raw)))
                                     (t ""))))
                     (unless (or (zerop (length text)) (uiop:string-prefix-p "[experience]" text))
                       (list (list (prompt-history-origin session-id) text)))))))

(nlk:define-side-table prompt-history-read (:weakness :key)
  "Live-only history read per shell: cached rows, readiness, in-flight read
and the dialog that requested them. The store remains the history authority.")

(nlk:define-side-table prompt-history-search (:weakness :key)
  "Live-only full rows and the one coalesced query read per history dialog.")

(defun stage-prompt-history-search (dialog query &optional seat
                                    &aux (state (prompt-history-search dialog)))
  "Echo the query now; rank only its rested value off the UI loop."
  (setf (getf state :query) query (getf state :at) (get-internal-real-time)
        (getf state :seat) seat
        (picker-dialog-rows dialog) (unless (plusp (length query)) (getf state :rows))
        (picker-dialog-on-tick dialog) (when (and (getf state :ready) (plusp (length query)))
                                         #'flush-prompt-history-search)
        (picker-dialog-empty-label dialog) (cond ((not (getf state :ready)) "Reading prompt history…")
                                                ((plusp (length query)) "Searching prompts…")
                                                (t "No prompts submitted yet"))
        (picker-dialog-no-match-label dialog) (picker-dialog-empty-label dialog))
  (picker-rows-filtered-for dialog query))

(defun flush-prompt-history-search (model dialog &aux (state (prompt-history-search dialog)))
  "Start at most one ranking after 120 ms of query rest; the frame remains free."
  (when (and (eq dialog (chat-model-dialog model)) (not (getf state :reading))
             (>= (- (get-internal-real-time) (getf state :at))
                 (* 0.12 internal-time-units-per-second)))
    (let ((query (getf state :query)) (rows (getf state :rows)))
      (setf (getf state :reading) t (picker-dialog-on-tick dialog) nil)
      (off-loop model
                (lambda () (picker-visible-rows (make-picker-dialog :rows (copy-list rows) :search query)))
                (lambda (model answer)
                  (setf (getf state :reading) nil)
                  (cond
                    ((not (eq dialog (chat-model-dialog model))) :quiet)
                    ((or (not (equal query dialog.search)) (not (eq rows (getf state :rows))))
                     (when (plusp (length (picker-dialog-search dialog)))
                       (setf (picker-dialog-on-tick dialog) #'flush-prompt-history-search))
                     :quiet)
                    (t
                     (setf (picker-dialog-rows dialog) (unless (off-loop-failure answer) answer)
                           (picker-dialog-on-tick dialog) nil
                           (picker-dialog-empty-label dialog)
                           (or (off-loop-failure answer) "No prompts match this filter")
                           (picker-dialog-no-match-label dialog) (picker-dialog-empty-label dialog)
                           (picker-dialog-selection dialog)
                           (or (position (getf state :seat) (picker-dialog-rows dialog)
                                         :key #'picker-row-value :test #'equal) 0))
                     (picker-rows-filtered-for dialog query))))))))

(defun prompt-history-title (rows)
  "Count the loaded rows, disclosing the existing read bound with a plus."
  (format nil "History (~d~:[~;+~])" (length rows)
          (= (length rows) *max-prompt-history-rows*)))

(defun read-prompt-history-rows (model drafts &optional cached)
  "Read and prepare history off the loop; name a refusal when using drafts."
  (let* ((answer (handler-case (gateway-model-http-call model :get "/api/gateway/prompts" nil)
                   (error (condition) (list :error (nlk:safe-condition-report condition)))))
         (failure (off-loop-failure answer))
         (rows (unless failure
                 (loop for prompt across (json-array answer "prompts")
                       for text = (json-value prompt :string "text")
                       when text collect (make-picker-row :key (json-value prompt :string "lane")
                                                         :label (nlk:one-line text) :value text)))))
    (when (and rows (equalp rows cached)) (setf rows cached))
    (list :rows (or rows (loop for text in drafts
                              collect (make-picker-row :label (nlk:one-line text) :value text)))
          :failed failure
          :detail (cond (failure (format nil "Store history unavailable: ~a" failure))
                        ((and (null rows) drafts) "Store history is empty — showing typed prompts")))))

(defun install-prompt-history-rows (model answer &aux (state (prompt-history-read model))
                                                     (dialog (getf state :dialog)))
  "Land on the UI loop, preserving search and the highlighted prompt; never reopen."
  (setf (getf state :reading) nil)
  (let ((failure (off-loop-failure answer)))
    (unless (or failure (and (getf answer :failed)
                            (or (getf state :ready) (null (getf answer :rows)))))
      (setf (getf state :rows) (getf answer :rows) (getf state :ready) t))
    (unless (and dialog (eq dialog (chat-model-dialog model)))
      (return-from install-prompt-history-rows :quiet))
    (bind ((rows (getf state :rows))
           (ready (getf state :ready))
           (searched (prompt-history-search dialog)))
      (setf dialog.title (if ready (prompt-history-title rows) "History (unavailable)")
            dialog.context (or failure
                               (when (getf answer :failed)
                                 (format nil "~a — showing ~a" (getf answer :detail)
                                         (if (eq rows (getf answer :rows))
                                             "typed prompts" "previously loaded history")))
                               (getf answer :detail) "")
            dialog.subtitle dialog.context)
      (when (zerop (length dialog.search))
        (setf dialog.empty-label
              (if ready "No prompts submitted yet" "History unavailable — reopen to retry")))
      ;; The worker reuses the cached list when the store returned the
      ;; same facts. A refresh must not blank already-ranked search hits.
      (unless (and (eq (getf searched :rows) rows) (eq (getf searched :ready) ready))
        (let* ((pick (nth dialog.selection (picker-visible-rows dialog)))
               (value (and pick (picker-row-value pick))))
          (setf (getf searched :rows) rows (getf searched :ready) ready)
          (if (plusp (length dialog.search))
              (stage-prompt-history-search dialog dialog.search value)
              (setf dialog.rows rows
                    dialog.selection (or (and pick (position value rows :key #'picker-row-value
                                                                         :test #'equal))
                                         0))))))))

(defun open-prompt-history-dialog (model)
  "Ctrl+R and /history: paint first, refresh the store-backed rows off the loop."
  (ensure-composer-history model)
  (let* ((state (or (prompt-history-read model)
                    (setf (prompt-history-read model) (list :ready nil :rows nil :reading nil :dialog nil))))
         (rows (getf state :rows)))
    (open-picker-dialog model (if (getf state :ready) (prompt-history-title rows) "History (reading…)")
                        rows
                        (lambda (model text)
                          (replace-composer-text model text)
                          (reset-history-navigation model)
                          nil)
                        :kind :prompt-history :searchable t :on-search #'stage-prompt-history-search
                        :search-placeholder "Type to search prompts…"
                        :empty-label (if (getf state :ready) "No prompts submitted yet" "Reading prompt history…")
                        :no-match-label "No prompts match this filter"
                        :hint "Enter returns the pick to the composer - Esc or Ctrl+R closes")
    (setf (getf state :dialog) (chat-model-dialog model)
          (prompt-history-search (chat-model-dialog model))
          (list :rows rows :ready (getf state :ready) :query "" :at 0 :reading nil :seat nil))
    ;; Reopening while the read runs reuses it, and seats its answer in this
    ;; dialog only. Neither closing nor a different dialog can be undone.
    (unless (getf state :reading)
      (setf (getf state :reading) t)
      (let ((drafts (copy-list (chat-model-prompt-history model)))
            (cached (getf state :rows)))
        (off-loop model (lambda () (read-prompt-history-rows model drafts cached))
                  #'install-prompt-history-rows))))
  nil)

;;; --- mouse suite -------------------------------------------------------------
;;; The Zig runtime.zig handleMouse gestures on the composed frame: scrollbar
;;; hit-test and drag, click-to-expand on tool cards, click-to-restore on
;;; user prompts, hover states, and drag-select with copy-on-release. All
;;; coordinates arrive 1-based from the decoder; the frame stack is the Zig
;;; ChatShell: blank, header, blank, then the transcript band.

(defun transcript-band-metrics (model &aux (window (chat-model-band model))
                                           (vp-height window.height)
                                           (total window.count))
  "=> (values VP-HEIGHT TOTAL Y-OFFSET TOP-PAD TEXT-WIDTH): the composed
frame's transcript band geometry."
  ;; Paint and pointer mapping share the same slack policy: a pinned
  ;; question stays at the top; only a promptless short band anchors below.
  (values vp-height total
          (band-window-offset window)
          (band-window-top-padding window)
          (transcript-text-width (max 20 (chat-model-term-width model)))))
(defun frame-transcript-position (model x y &optional owner-p)
  "Frame mouse X,Y (1-based) to transcript LINE-INDEX and display COLUMN.
Fixed composer status has no transcript owner."
  (bind (((vp-height total _ top-pad text-width) (transcript-band-metrics model))
         (band-row (- y 4))
         (column (- x 3))
         (index (and (<= 0 band-row) (< band-row vp-height) (<= 0 column) (< column text-width)
                     (band-window-row-index (chat-model-band model) (- band-row top-pad) owner-p))))
    (when (and index (<= 0 index) (< index total))
      (values index column))))
(defun transcript-item-at-line (model line-index &aux (refs (chat-model-line-rows model)))
  "What owns visual line LINE-INDEX: a TRANSCRIPT-ROW, a TOOL-GROUP's own
line, or NIL."
  (when (and (integerp line-index) (<= 0 line-index)
             (< line-index (length refs)))
    (let ((ref (aref refs line-index)))
      (and (or (transcript-row-p ref) (tool-group-p ref)) ref))))

(defun togglable-item-p (model item)
  "Whether a click on ITEM opens or closes it: a tool card, a group, or a
thought the detail level folds."
  (or (tool-group-p item)
      (and (transcript-row-p item)
           (or (eq :tool-call item.kind)
               (eq :reasoning item.kind)
               ;; A predicate keeps its Boolean contract across live callers.
               (and (row-notice-summary item) t)))))

(defun restorable-prompt-p (model item)
  "Whether a click on ITEM opens the checkpoint tree on it: an operator
prompt of the attached session, never one a dialog's preview shows."
  (and (transcript-row-p item)
       (eq :text item.kind)
       (equal "user" item.role)
       (equal (transcript-shown-session-id model) (chat-model-session-id model))))

(defun scroll-for-thumb-top (track-height visible-rows total-rows thumb-top
                             &key (round-fn #'floor))
  "The Zig transcriptScrollbarScrollForThumbTop math verbatim: the ceiling
division that exactly inverts SCROLLBAR-THUMB's floor."
  ;;
  ;; ROUND-FN is the thumb rule the caller is inverting — SCROLLBAR-THUMB's own
  ;; two: the transcript's floor and the docked picker's ceiling
  ;; (renderShortcutPickerScrollbar). A drag inverts the geometry of the bar it
  ;; is dragging, or the same track row would land on two different offsets.
  (if (zerop track-height)
      0
      (let ((max-scroll (max 0 (- (max total-rows visible-rows) visible-rows)))
            ;; The thumb this inverts, measured by the one thumb geometry.
            (max-top (- track-height (nth-value 1 (scrollbar-thumb track-height visible-rows
                                                                   total-rows 0
                                                                   :round-fn round-fn)))))
        (cond ((zerop max-scroll) 0)
              ((zerop max-top) (if (plusp thumb-top) max-scroll 0))
              (t (floor (+ (* (min thumb-top max-top) max-scroll)
                           max-top -1)
                        max-top))))))

(defun transcript-rail-at-p (model x y &aux (band-row (- y 4)))
  "Whether frame mouse X, Y (1-based) is on the transcript's scroll gutter:
the frame's last two columns, the rail and the blank beside it, over the
band's rows."
  (and (>= (1- x) (- (max 8 (chat-model-term-width model)) 2))
       (<= 0 band-row) (< band-row (transcript-band-metrics model))))

(defun rail-hot-p (model)
  "Whether the rail's thumb is lit: the pointer rests on the gutter or drags
the thumb."
  (or (chat-model-rail-hover model)
      (eq :transcript (car (chat-model-scrollbar-drag model)))))

(defun apply-scrollbar-track-row (model track-row grab-offset)
  "Move the transcript so the thumb top lands at TRACK-ROW - GRAB-OFFSET."
  ;; Reaching the bottom re-sticks the follow, matching the wheel handoff.
  (let ((vp-height (transcript-band-metrics model))
        (max-scroll (band-window-max-offset (chat-model-band model))))
    (when (plusp max-scroll)
      (let ((target (scroll-for-thumb-top vp-height vp-height
                                          (+ vp-height max-scroll)
                                          (max 0 (- track-row
                                                    grab-offset)))))
        (cond ((>= target max-scroll) (pin-transcript-bottom model))
              (t (clear-transcript-pin model)
                 (setf (band-window-offset (chat-model-band model)) target
                       (chat-model-follow-bottom model) nil)
                 (grow-transcript-projection model)))))))

(defun grab-scrollbar (model surface apply track-row thumb-top thumb-height)
  "Arm a drag of SURFACE's bar pressed at TRACK-ROW: the thumb grabbed where
the pointer landed, or the track jumped through APPLY and the thumb grabbed
at its midpoint (Zig pressTranscriptScrollbar)."
  ;; SURFACE is :transcript or :dialog, the bar the drag leg then moves.
  (let* ((on-thumb (<= thumb-top track-row (+ thumb-top thumb-height -1)))
         (grab (if on-thumb
                   (- track-row thumb-top)
                   (floor thumb-height 2))))
    (setf (chat-model-scrollbar-drag model) (cons surface grab))
    (unless on-thumb
      (funcall apply model track-row grab))))

(defun plain-column-index (text column)
  "The character index in TEXT at display COLUMN (wide characters claim
both their cells)."
  (loop with acc = 0
        for i from 0 below (length text)
        when (>= acc column) return i
        do (incf acc (char-display-width (char text i)))
        finally (return (length text))))

(defun word-char-p (ch)
  "Whether CH belongs to a selectable word: ASCII alphanumerics and `_',
plus every non-ASCII character — the Zig IS-WORD-GRAPHEME contract, so a
Unicode word or an identifier stays whole."
  (or (alphanumericp ch) (char= ch #\_) (>= (char-code ch) 128)))

(defun word-span-at (text index)
  "=> (values START END) character indices of the word run around INDEX in
TEXT, END exclusive, or NIL when INDEX holds a separator or is past the
end — the Zig WORD-DISPLAY-COLUMN-BOUNDS walk."
  (when (and (integerp index) (<= 0 index) (< index (length text))
             (word-char-p (char text index)))
    (values (if-let (before (position-if-not #'word-char-p text
                                             :end index :from-end t))
              (1+ before) 0)
            (or (position-if-not #'word-char-p text :start index)
                (length text)))))

;;; --- the frame: the one selection region ---------------------------------
;;; A mark belongs to the FRAME, not to the transcript. Every row the shell
;;; paints — header, transcript, notice, turn bar, composer, dropdown,
;;; dialog panel, footer — is markable, and a region added later is markable
;;; the day it is painted, because the lane is stamped from the composed
;;; frame itself and nothing here names a region. Coordinates are screen
;;; cells: (ROW . COLUMN), both 0-based, ROW from the frame's top and COLUMN
;;; from the screen's left edge — so no region's inset, wrapped width, or
;;; scroll offset has to be known to mark its text, and a drag that crosses
;;; from the transcript into the composer is one span, not two.
;;;
;;; What the wash covers and what the copy reads are the same lane of the
;;; same frame, by construction: the wash marks the cells the guarded rows of
;;; COMPOSE-FRAME are laid out into (what is painted), and the copy reads the
;;; style-free lane derivable from exactly those rows.
;;;
;;; The lane is model-keyed and weak rather than a CHAT-MODEL slot: the live
;;; image runs its shells out of this file, and a slot added to CHAT-MODEL
;;; with instances alive leaves every one of them obsolete.
(defstruct (frame-lane (:constructor make-frame-lane (rows width height)))
  "One composed frame: ROWS as painted, the WIDTH and HEIGHT it was composed
at (a lane from another geometry is not this frame), and PLAIN — the
style-free lane derived from ROWS on first read, NIL until a gesture asks."
  (rows #() :type simple-vector)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (plain nil))

(nlk:define-side-table frame-lanes (:weakness :key)
  "The last frame each model composed, a FRAME-LANE.")

(defun frame-width (model)
  "The frame's width in columns: the one expression COMPOSE-FRAME paints at,
FRAME-POSITION clamps to, and the wash spans."
  (max 8 (chat-model-term-width model)))

(defun frame-height (model)
  "The frame's height in rows: the one expression COMPOSE-FRAME stacks to and
FRAME-POSITION clamps to."
  (max 4 (chat-model-term-height model)))

(defun transcript-lane (model)
  "=> (values TOP BOTTOM LEFT RIGHT), the frame cells the transcript's words
stand in, 0-based and inclusive: the band's rows, between its two-column
inset and the scroll gutter -- the one region a selection marks, washes and
copies."
  ;; The band opens under the frame head's three rows, the row the pointer
  ;; legs read as band row 0 (Y - 4, 1-based); its columns are COMPOSE-FRAME's.
  ;; The header, the composer, the footer, a docked panel, the inset and the
  ;; rail are the shell's, never the transcript's words ("we should not select
  ;; the TUI elements/artifacts").
  (values 3 (+ 2 (transcript-band-metrics model))
          2 (+ 1 (transcript-text-width (frame-width model)))))

(defun lane-mark (model anchor cell)
  "The mark a drag from ANCHOR to CELL, frame (ROW . COLUMN)s, lays on the
transcript's words: the two cells clamped into TRANSCRIPT-LANE, earlier
first -- a cell above the band its first cell, below it its last -- or NIL
when ANCHOR is off the band's rows, so a drag begun on the shell's chrome
marks nothing."
  (multiple-value-bind (top bottom left right) (transcript-lane model)
    (flet ((clamp (cell &aux (row (car cell)))
             (cond ((< row top) (cons top left))
                   ((> row bottom) (cons bottom right))
                   (t (cons row (max left (min right (cdr cell))))))))
      (when (<= top (car anchor) bottom)
        (let ((from (clamp anchor)) (to (clamp cell)))
          (if (or (< (car from) (car to))
                  (and (= (car from) (car to)) (<= (cdr from) (cdr to))))
              (cons from to)
              (cons to from)))))))

(defun plain-frame-row (row &aux (len (length row)))
  "ROW's painted text with the escape sequences this shell writes removed:
the characters a display column indexes, and what a copy carries."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i len)
          do (let* ((ch (char row i))
                    (end (and (char= ch #\Escape)
                              (own-sequence-end row i len))))
               (cond (end (setf i end))
                     (t (write-char ch out)
                        (incf i)))))))

(defun frame-plain-rows (model &aux (lane (frame-lanes model)))
  "The style-free text of every row of MODEL's frame, one string per screen
row — the selection's hit-test and copy source."
  ;; Built on first read and cached until the next frame; a frame that was
  ;; never painted, or was stamped at another geometry, is composed here,
  ;; because the frame a paint would produce IS the lane.
  (unless (and lane
               (= (frame-lane-width lane) (frame-width model))
               (= (frame-lane-height lane) (frame-height model)))
    (compose-frame model)
    (setf lane (frame-lanes model)))
  (or (frame-lane-plain lane)
      (setf (frame-lane-plain lane)
            (map 'vector #'plain-frame-row (frame-lane-rows lane)))))

(defun frame-position (model x y)
  "Frame mouse X,Y (1-based) as the screen cell (ROW . COLUMN), clamped
to the frame so a drag that leaves the window still marks the nearest cell."
  ;; No region geometry is consulted: this is the selection lane's own
  ;; coordinate system.
  (cons (max 0 (min (1- (frame-height model)) (1- (or y 1))))
        (max 0 (min (1- (frame-width model)) (1- (or x 1))))))

(defun word-selection-at (model row column &aux (plains (frame-plain-rows model)))
  "=> ((ROW . C1) . (ROW . C2)) covering the whole word at display COLUMN of
frame ROW, or NIL when that cell holds a separator, sits past the row, or no
row is there — the Zig TRANSCRIPT-WORD-SELECTION-AT behind the double-click
gesture, over the frame rather than over the transcript alone."
  ;; C2 is inclusive, the convention FRAME-SELECTION-TEXT and
  ;; SELECTION-OVERLAY share. Only the transcript's words select: a word on
  ;; the header, the composer or a panel is the shell's (TRANSCRIPT-LANE).
  (when (and (integerp row) column (< row (length plains))
             (multiple-value-bind (top bottom left right) (transcript-lane model)
               (and (<= top row bottom) (<= left column right))))
    (bind ((text (aref plains row))
           ((start end) (word-span-at text (plain-column-index text column))))
      (when start
        (cons (cons row (text-display-width (subseq text 0 start)))
              (cons row (1- (text-display-width (subseq text 0 end)))))))))

(defun selection-rows-text (rows)
  "ROWS -- (TEXT . EDGE) per marked row, TEXT the words cut at the mark and
EDGE whether the row opens on the lane's edge -- as the copy's text: the
gutter's indent the edge rows share taken off, and joined with newlines."
  ;; The rows that open on the lane's edge open on the gutter's indent too:
  ;; the fewest blanks any of them opens on go, so a code block keeps its own
  ;; indent under the prose around it. A row the mark opens inside keeps its
  ;; words, never blanks.
  (let ((indent (or (loop for (text . edge) in rows
                          when (and edge (plusp (length text)))
                            minimize (or (position #\Space text :test-not #'char=) 0))
                    0)))
    ;; The band's padding rows the mark crossed at either end are not
    ;; words; a blank row between paragraphs is.
    (string-trim '(#\Newline)
                 (format nil "~{~a~^~%~}"
                         (loop for (text . edge) in rows
                               collect (if edge
                                           (subseq text (min indent (length text)))
                                           (string-left-trim " " text)))))))

(defun cut-marked-row (text from to)
  "TEXT, a plain row, cut to display columns FROM..TO (inclusive) and
right-trimmed: the TEXT of a (TEXT . EDGE) row SELECTION-ROWS-TEXT takes."
  ;; PLAIN-COLUMN-INDEX never answers past the text, so both cuts land in it.
  (let ((start (plain-column-index text from))
        (end (plain-column-index text (1+ to))))
    (string-right-trim " " (subseq text start (max start end)))))

(defun frame-mark-rows (model selection &aux (plains (frame-plain-rows model)))
  "SELECTION's rows of MODEL's last frame, from its plain lane, as
SELECTION-ROWS-TEXT takes them: the mark's rows inside TRANSCRIPT-LANE, each
cut to the lane's columns and at the mark's endpoints."
  (when plains
    (multiple-value-bind (top bottom left right) (transcript-lane model)
      (destructuring-bind ((r1 . c1) . (r2 . c2)) selection
        (loop for row from (max top r1) to (min bottom r2 (1- (length plains)))
              for from = (if (= row r1) (max left c1) left)
              collect (cons (cut-marked-row
                             (without-message-clock model row (without-gutter-marks (aref plains row)))
                             from (if (= row r2) (min right c2) right))
                            (= from left)))))))

(defun span-mark-rows (model span &aux (plains (chat-model-line-plain model)))
  "SPAN's rows (MARK-SPAN) off the transcript's own lines, as
SELECTION-ROWS-TEXT takes them: every line between its ends, shown or
scrolled out of the window, cut at the ends' columns."
  (destructuring-bind ((l1 . c1) . (l2 . c2)) span
    (multiple-value-bind (top bottom left right) (transcript-lane model)
      (declare (ignore top bottom))
      (loop with end = (- right left)
            for line from l1 to (min l2 (1- (length plains)))
            for from = (if (= line l1) c1 0)
            collect (cons (cut-marked-row
                           (without-line-clock model line (without-gutter-marks (aref plains line)))
                           from (if (= line l2) (min end c2) end))
                          (zerop from))))))

(defun frame-selection-text (model &aux (selection (chat-model-selection model)))
  "The marked transcript words of MODEL: the mark's rows cut at its
endpoints (the release column inclusive), the gutter's marks and a message's
clock left behind, right-trimmed, the gutter's indent they share taken off,
and joined with newlines. A mark a drag laid is read off the transcript's
own lines (MARK-SPAN), so it holds the lines a scroll took out of the
window; any other off the last frame's plain lane."
  ;; Only the transcript's words copy: the header, the composer, a panel,
  ;; the band's inset and its rail are the shell's.
  (when selection
    (let ((span (mark-span model)))
      (selection-rows-text (if span
                               (span-mark-rows model span)
                               (frame-mark-rows model selection))))))

;;; A drag's mark is held on the transcript's lines, not on the screen: each
;;; end is a POINT -- (OWNER PLACE COLUMN LINE): the row that owns the line
;;; (or the nearest owned line above it), the line's place under that
;;; owner, its lane column, and the line index it had -- so the mark keeps
;;; its words while the window scrolls under it (the drag's own edge scroll,
;;; the wheel, a page key) and while the projection grows rows in above it.
;;; CHAT-MODEL-SELECTION stays the screen mark the gesture laid; the points
;;; ride beside it keyed on that very cons, so a mark replaced or cleared
;;; anywhere drops them with it.
(nlk:define-side-table mark-points (:weakness :key)
  "Weak memo: chat model -> (MARK . (FROM . TO)), the transcript points of
the screen MARK a drag laid.")

(nlk:define-side-table anchor-point (:weakness :key)
  "Weak memo: chat model -> (ANCHOR . POINT), a press's frame cell and the
transcript point it stood on when the drag left it.")

(nlk:define-side-table drag-edge (:weakness :key)
  "Weak memo: chat model -> (X Y . LAST): a held drag's pointer past the
band's first or last row, and when that last scrolled the window.")

(defun transcript-point (model line column &aux (refs (chat-model-line-rows model)))
  "The point at transcript LINE, lane COLUMN: (OWNER PLACE COLUMN LINE)."
  (let* ((owner (loop for i downfrom (min line (1- (length refs))) to 0
                      when (aref refs i) return (aref refs i)))
         (first (and owner (position owner refs))))
    (list owner (- line (or first 0)) column line)))

(defun point-line (model point &aux (refs (chat-model-line-rows model))
                                    (count (length (chat-model-line-plain model))))
  "POINT's transcript line now: its owner's first line plus its place, or
the line it had when the owner is no longer projected."
  (destructuring-bind (owner place column line) point
    (declare (ignore column))
    (let ((first (and owner (position owner refs))))
      (max 0 (min (1- count) (if first (+ first place) line))))))

(defun frame-cell-point (model cell)
  "The transcript point under frame CELL (ROW . COLUMN), clamped into
TRANSCRIPT-LANE: a cell above the band the first shown line's start, one
below it the last shown line's end, and a band row no line stands on
(padding, the spacer, an omission marker) the next line below it from its
start, else the last above it to its end. NIL when no line is shown."
  (multiple-value-bind (top bottom left right) (transcript-lane model)
    (let* ((window (chat-model-band model))
           (top-pad (nth-value 3 (transcript-band-metrics model)))
           (count (length (chat-model-line-plain model)))
           (end (- right left))
           (row (car cell))
           (column (- (max left (min right (cdr cell))) left)))
      (flet ((line-at (r &aux (index (band-window-row-index window (- r top top-pad))))
               (and index (< -1 index count) index)))
        (cond ((< row top) (setf row top column 0))
              ((> row bottom) (setf row bottom column end)))
        (let ((line (line-at row)))
          (unless line
            (loop for r from (1+ row) to bottom
                  when (line-at r) do (setf line (line-at r) column 0) (return))
            (unless line
              (loop for r downfrom (1- row) to top
                    when (line-at r) do (setf line (line-at r) column end) (return))))
          (and line (transcript-point model line column)))))))

(defun mark-span (model &aux (mark (chat-model-selection model))
                             (entry (mark-points model)))
  "=> ((LINE1 . COLUMN1) . (LINE2 . COLUMN2)), MODEL's mark on the
transcript's lines as they stand now, earlier end first, lane columns, the
last inclusive; NIL for a mark no drag laid over the transcript."
  (when (and mark entry (eq mark (car entry)))
    (flet ((at (point) (cons (point-line model point) (third point))))
      (let ((a (at (cadr entry))) (b (at (cddr entry))))
        (if (or (< (car a) (car b)) (and (= (car a) (car b)) (<= (cdr a) (cdr b))))
            (cons a b)
            (cons b a))))))

(defun hold-mark-points (model mark &optional (from (frame-cell-point model (car mark))))
  "Hold MARK, a screen mark on MODEL's frame, as the selection with its
transcript points: FROM (by default its first cell's) to its last cell's."
  (let ((to (frame-cell-point model (cdr mark))))
    (setf (chat-model-selection model) mark)
    (if (and from to)
        (setf (mark-points model) (cons mark (cons from to)))
        (forget-mark-points model))
    mark))

(defun extend-mark (model anchor cell &aux (old (chat-model-selection model))
                                           (before (mark-span model)))
  "Lay the mark a drag from ANCHOR to CELL makes (LANE-MARK) with its
transcript points, the anchor's taken where the drag left it, so a scroll
since cannot move that end. => whether the marked words moved."
  (let ((mark (lane-mark model anchor cell)))
    (when mark
      (let ((from (let ((entry (anchor-point model)))
                    (if (and entry (eq anchor (car entry)))
                        (cdr entry)
                        (cdr (setf (anchor-point model)
                                   (cons anchor (frame-cell-point model anchor))))))))
        ;; The points run anchor to pointer, not first to last: LANE-MARK's
        ;; order is the screen's, and the anchor may have scrolled off it.
        (setf (chat-model-selection model) mark)
        (let ((to (frame-cell-point model cell)))
          (if (and from to)
              (setf (mark-points model) (cons mark (cons from to)))
              (forget-mark-points model)))
        (let ((after (mark-span model)))
          (if after
              (not (equal before after))
              (not (equal old mark))))))))

(defparameter *drag-scroll-period-ms* 50
  "How often a drag held past the transcript band's edge scrolls it, in ms.")

(defun note-drag-edge (model x y)
  "Hold the drag's edge scroll while the pointer at frame X,Y (1-based)
stands above the band's first row or below its last; drop it otherwise."
  (multiple-value-bind (top bottom) (transcript-lane model)
    (let ((row (1- (or y 1))))
      (if (or (< row top) (> row bottom))
          (setf (drag-edge model) (list* x y (cddr (drag-edge model))))
          (forget-drag-edge model)))))

(defun drag-scroll-step (model &aux (held (drag-edge model)))
  "Scroll a drag held past the band's edge one step -- a row for each row
the pointer stands past it, up to three -- at most every
*DRAG-SCROLL-PERIOD-MS*, and extend the mark to the cell under the pointer.
The pointer's motion asks, and so does every tick while it holds still.
=> whether the window moved."
  (cond
    ((null held) nil)
    ((not (and (chat-model-selection-anchor model)
               (chat-model-selection-dragged model)
               (chat-model-selection model)))
     (forget-drag-edge model)
     nil)
    (t
     (destructuring-bind (x y . last) held
       (when (or (null last) (>= (since-ms last) *drag-scroll-period-ms*))
         (multiple-value-bind (top bottom) (transcript-lane model)
           (let* ((row (1- y))
                  (past (cond ((< row top) (- row top)) ((> row bottom) (- row bottom)) (t 0)))
                  (window (chat-model-band model))
                  (before (band-window-offset window)))
             (setf (drag-edge model) (list* x y (get-internal-real-time)))
             (unless (zerop past)
               (scroll-transcript model (if (minusp past) :line-up :line-down) (min 3 (abs past)))
               (unless (= before (band-window-offset window))
                 (extend-mark model (chat-model-selection-anchor model) (frame-position model x y))
                 t)))))))))

;;; The operator wants the selection faint, not solid: at the Zig
;;; design_system value of 55 the wash covered the canvas with a heavy burnt
;;; band. 35 keeps the theme's :selection colour as a light tint — selected
;;; content still keeps its own foreground, like a browser text selection.
(defparameter +selection-overlay-alpha+ 35
  "Opacity (percent) of the transcript selection wash.")

(defun sgr-rgb (sgr &aux (codes (mapcar (lambda (part) (parse-integer part :junk-allowed t))
                                        (uiop:split-string (or sgr "") :separator ";"))))
  "The rgb a cell's SGR colour text names — `48;2;R;G;B', `48;5;N', or a
basic 40-47/100-107 — or NIL for the terminal's default."
  (case (length codes)
    (5 (cddr codes))
    (3 (ansi256-rgb (third codes)))
    (1 (and (first codes)
            (nth (- (first codes) (if (< (first codes) 100) 40 92)) *ansi16-palette*)))))

(defun selection-overlay (model &aux (selection (chat-model-selection model))
                                      (profile (effective-color-profile))
                                      (selection-bg (visual-style-bg (style-for-tone :selection))))
  "The translucent drag-selection wash as the frame's overlay (NLT:MAKE-VIEW
:OVERLAY), or NIL while nothing is marked: every cell of the mark keeps its
glyph and foreground and gets the :selection background blended over its own
at +SELECTION-OVERLAY-ALPHA+ -- the Zig design_system.selectionOverlayStyle
cell loop."
  ;; Span semantics match FRAME-SELECTION-TEXT: screen cells inside
  ;; TRANSCRIPT-LANE, the release column inclusive, an interior row washed
  ;; across the lane's width, never the shell's chrome or the rail. A
  ;; cell with no background uses the canvas the terminal shows as its wash
  ;; ground (CANVAS-RGB), the theme's nominal one while the terminal has not
  ;; said what that is.
  ;; The wash reads the cells the painter laid out, so
  ;; it lands on whatever region painted the row and restyles no row: a
  ;; frame that moves only the mark lays out nothing twice. Colorless
  ;; frames (:none) carry no wash -- there is nothing to blend.
  (when (and selection selection-bg (not (eq profile :none)))
    (let ((canvas-bg (or (canvas-rgb) (visual-style-bg (style-for-tone :selection-ground))))
          ;; The parser hands every cell of one colour the same string, so a
          ;; frame blends each colour it washes once.
          (washes (make-hash-table :test 'eq)))
      (let ((cells (mark-frame-cells model selection)))
        (lambda (buffer &aux (width (nlt:screen-buffer-width buffer)))
          (loop for (row first last) in cells
                when (< row (nlt:screen-buffer-height buffer))
                do (loop for col from (max 0 first) below (min width (1+ last))
                         for cell = (nlt:screen-buffer-ref buffer col row)
                         for under = (nlt:cell-bg cell)
                         do (setf (nlt:cell-bg cell)
                                  (or (gethash under washes)
                                      (setf (gethash under washes)
                                            (color-sgr (blend-rgb selection-bg
                                                                  (or (sgr-rgb under) canvas-bg)
                                                                  +selection-overlay-alpha+)
                                                       nil profile)))))))))))

(defun handle-mouse-wheel (model msg)
  "One wheel notch, routed to what the pointer is on: the overlay under it — an
open dialog's list, the composer dropdown — else the transcript, three rows per
notch (the Zig transcript_mouse_wheel_rows) wherever the pointer sits, or the
/term panel while its takeover owns the band."
  (let* ((y (or msg.y 0))
         (delta (* (if (eq :up (nlt:mouse-wheel-direction msg)) -1 1)
                   (max 1 (or (nlt:mouse-wheel-count msg) 1))))
         (dialog (chat-model-dialog model)))
    (cond
      ;; An open sheet takes the wheel whole: a notch on its window scrolls
      ;; what it shows there, one anywhere else moves nothing behind it.
      ((sheet-dialog-p dialog)
       (multiple-value-bind (column row) (sheet-at model (or msg.x 0) y)
         (when column
           (funcall (sheet-dialog-wheel dialog) model dialog delta column row))))
      ;; A notch on a painted row of the docked panel — the Zig
      ;; pointInDialog test on the active dialog area. Rows are the whole
      ;; test: the panel spans the frame's full width. It paints one row
      ;; fewer than its area spans (its last row is the canvas gap the
      ;; frame owns), which is exactly what DOCKED-DIALOG-FRAME-ROWS stacks.
      ((and dialog
            (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
              (and panel (<= (1+ above) y (+ above (1- panel))))))
       (when (dialog-list-selectable-p dialog)
         (multiple-value-bind (body-row height) (dialog-body-row-at model y)
           ;; The window moves DELTA rows; the selection lands on the row under the pointer.
           (let* ((total (length (picker-visible-rows dialog)))
                  (visible (min total (max 1 (or height (nth-value 1 (dialog-body-band model dialog))
                                                 1))))
                  (offset (max 0 (min (+ dialog.view-offset delta) (max 0 (- total visible))))))
             (when (plusp total)
               (setf dialog.view-offset offset)
               (let* ((row (if body-row
                               (min body-row (1- visible))
                               (max 0 (min (- dialog.selection offset) (1- visible)))))
                      (target (past-headings (picker-visible-rows dialog)
                                             (max 0 (min (+ offset row) (1- total))) 1)))
                 (unless (= target dialog.selection)
                   (setf dialog.selection target)
                   (picker-notify-highlight model dialog :instant t))))))))
      ;; A notch on the dropdown's panel: the last band above the shell
      ;; tail (COMPOSE-FRAME stacks the composer, the dropdown, then the
      ;; footer rows), so it is measured back from the frame's bottom and
      ;; cannot drift with the transcript band.
      ((bind (((rows _ count) (composer-dropdown-geometry model))
              (height (frame-height model))
              (first (and rows (1+ (- height (if (shell-footer-p height) 3 2) rows)))))
         (when (and rows (<= first y (+ first (1- rows))))
           ;; The selection moves DELTA rows, clamped at both ends (the Zig
           ;; scrollComposerSuggestionBy: a scroll stops where :up/:down wrap).
           (setf (chat-model-slash-selection model)
                 (alexandria:clamp (+ (min (chat-model-slash-selection model) (1- count))
                                      delta)
                                   0 (1- count)))
           t)))
      (t (scroll-transcript model (if (minusp delta) :line-up :line-down) (* 3 (abs delta)))))))

;;; Model-keyed because one image can serve several shells, each with its own
;;; model — the Zig keeps the same pair on its runtime
;;; \(transcript_last_left_press_at, transcript_last_left_press_cell).
(nlk:define-side-table left-press-state (:weakness :key)
  "The last left press per chat model, for the double-click test: (LINE
COLUMN INTERNAL-TIME).")

(defun note-left-press (model line column)
  "Record a left press at LINE, COLUMN and answer whether it is the second
click of a double-click — the same cell within 400ms."
  ;; A match consumes the pair, so a triple click starts a fresh cycle (the
  ;; Zig NOTE-TRANSCRIPT-LEFT-PRESS).
  (let* ((now (get-internal-real-time))
         (previous (left-press-state model))
         (doublep (and previous
                       (= (first previous) line)
                       (= (second previous) column)
                       (<= (since-ms (third previous) now) 400)))) ; Zig double_click_timeout_ns
    (if doublep
        (forget-left-press-state model)
        (setf (left-press-state model) (list line column now)))
    doublep))

(defun disarm-pointer (model)
  "Drop every armed pointer gesture: the tool-card, link, ✕ and exec presses,
the selection anchor and its drag flag."
  (setf (chat-model-pressed-tool-row model) nil
        (chat-model-pressed-link model) nil
        (chat-model-pressed-stop model) nil
        (chat-model-pressed-exec model) nil
        (chat-model-selection-anchor model) nil
        (chat-model-selection-dragged model) nil)
  (forget-anchor-point model)
  (forget-drag-edge model))

(defun handle-mouse-press (model msg &aux (x msg.x)
                                          (y msg.y))
  "The Zig press ladder over the frame: an open dialog's panel owns the
press on its rows and the rows under it — the cell its scrollbar paints
takes the grab, anywhere else there the press arms and the release confirms
— and above it, as with no dialog open, the transcript's scrollbar, ✕
arming, tool-card arming, link arming, prompt click-to-restore, and the
selection anchor — a second left click on that cell within 400ms
instead selects the word under the pointer."
  ;; The anchor is a screen cell wherever the press lands, so a press on
  ;; the header, the composer, the footer or a panel that moves is a drag,
  ;; never a click; only one on the transcript's words marks anything
  ;; (LANE-MARK), and the transcript's own affordances stay gated on its lane.
  ;;
  ;; No leg paints: the message that got here is folded in a batch the loop
  ;; paints once, and the frame's overlay washes the mark at that paint.
  ;;
  ;; A sheet's window is over everything, the toasts too, and takes the
  ;; pointer whole: a click on it is its own, run on the release, and a press
  ;; anywhere else but a toast does nothing, the shell behind it waiting as
  ;; its keys do.
  (when (and (sheet-dialog-p (chat-model-dialog model)) (sheet-at model x y))
    (return-from handle-mouse-press t))
  ;; A toast is over everything, an open dialog too: a press on one is its
  ;; own — on the × it takes the toast down, anywhere else it does nothing —
  ;; and reaches nothing under it.
  (multiple-value-bind (toast part) (toast-at model x y)
    (when toast
      (when (and (eq part :close) (eq :left msg.button)) (close-toast model toast))
      (return-from handle-mouse-press t)))
  (when (sheet-dialog-p (chat-model-dialog model))
    (return-from handle-mouse-press t))
  ;; The frame above the panel stays the transcript's: a card toggles, a
  ;; target opens, an exec stops while the keys stay the dialog's. No
  ;; panel painted, the dialog keeps the whole frame.
  (when (and (chat-model-dialog model)
             (multiple-value-bind (panel above) (docked-dialog-frame-rows model)
               (or (null panel) (<= (1+ above) y (+ above (1- panel))))))
    ;; A click confirms the row it lands on by replaying Enter, so mouse
    ;; and keyboard share one confirm path (the Zig handleActiveDialogHover
    ;; press leg) — on RELEASE, not here: the press arms the selection
    ;; anchor first, so a drag from a panel row confirms nothing (and marks
    ;; nothing: the panel is the shell's) and only a click confirms.
    (return-from handle-mouse-press
      (when (eq :left (nlt:mouse-event-button msg))
        (if-let (target (frame-target-at-position model x y))
          ;; A tab's chip, a key's hint or a link on the panel arms as a
          ;; target over the transcript does: the release on it with no drag
          ;; between acts (HANDLE-MOUSE-RELEASE), a drag from it confirms nothing.
          (setf (chat-model-pressed-link model) target
                (chat-model-selection-anchor model) (frame-position model x y)
                (chat-model-selection-dragged model) nil)
          (multiple-value-bind (first height total visible) (dialog-scrollbar-metrics model)
            (let ((track-row (and first (- y first))))
              ;; The gutter is the bar's own cell and every column right of it:
              ;; DOCKED-PANEL-ROW paints the bar between its two gaps with the right
              ;; padding outside it, so the bar sits *DOCKED-PANEL-PAD-RIGHT* columns
              ;; in from the frame's right edge — and those columns are the bar and
              ;; panel fill, never a cell carrying a row's own text, so a press
              ;; there takes nothing the pointer could have confirmed.
              (multiple-value-bind (thumb-top thumb-height)
                  (when (and track-row (<= 0 track-row (1- height))
                             (>= x (- (frame-width model) *docked-panel-pad-right*)))
                    (scrollbar-thumb height visible total
                                     (picker-dialog-view-offset (chat-model-dialog model))
                                     :round-fn #'ceiling :fits-nil t))
                (if thumb-height
                    ;; The painted bar owns its own cell: the grab is the gesture,
                    ;; so no anchor arms here and the release confirms nothing.
                    (grab-scrollbar model :dialog #'apply-dialog-scrollbar-track-row
                                    track-row thumb-top thumb-height)
                    ;; The anchor is FRAME-POSITION's (ROW . COLUMN) cell, the
                    ;; shape the drag leg compares whole.
                    (setf (chat-model-selection-anchor model) (frame-position model x y)
                          (chat-model-selection-dragged model) nil)))))))))
  (when (eq :left (nlt:mouse-event-button msg))
    ;; A fresh press always discards a drag whose release was lost.
    (setf (chat-model-scrollbar-drag model) nil)
    (bind (((vp-height total y-offset) (transcript-band-metrics model)) (band-row (- y 4)))
      (cond
        ;; The scroll gutter over the band rows owns the press — still when
        ;; nothing can scroll, so it never falls through into text selection.
        ((transcript-rail-at-p model x y)
         (let ((max-scroll (band-window-max-offset (chat-model-band model))))
           (when (plusp max-scroll)
             (multiple-value-call #'grab-scrollbar model :transcript #'apply-scrollbar-track-row
               band-row (scrollbar-thumb vp-height vp-height (+ vp-height max-scroll) y-offset)))))
        (t
         (disarm-pointer model)
         (setf (chat-model-selection model) nil)
         (bind ((cell (frame-position model x y))
                ((line column) (frame-transcript-position model x y))
                (frame-target (frame-target-at-position model x y))
                (row (and line (transcript-item-at-line model line)))
                (home (and line (home-command-at-position model line column)))
                (link (and line (link-at-position model line column)))
                (stop (and line (band-stop-at model line column)))
                (exec (and line (band-exec-at model line))))
           (macrolet ((arm (place value)
                        `(setf ,place ,value (chat-model-selection-anchor model) cell)))
             (cond
               (frame-target (arm (chat-model-pressed-link model) frame-target))
               ;; A running exec's ✕ arms its interrupt the way a target
               ;; arms its open: the release on it with no drag between
               ;; sends it.
               (stop (arm (chat-model-pressed-stop model) stop))
               ;; Anywhere else on an exec's lines arms its open or close,
               ;; as a press on a card does: open, it shows all it wrote.
               (exec (arm (chat-model-pressed-exec model) exec))
               ;; A target under the pointer owns the press, inside a
               ;; card or outside one: the glyphs it covers are the only
               ;; ones whose click can mean `open this', and the whole
               ;; rest of the row still toggles. It arms like a tool
               ;; card — a release on it with no drag between opens it,
               ;; a drag from it selects — and only the two schemes an
               ;; opener answers for arm at all (OPENABLE-TARGET-P): a
               ;; markdown link's target is the model's to write.
               (home (arm (chat-model-pressed-link model) home))
               ((or (openable-target-p link) (and (consp link) (eq :diff (first link))))
                (arm (chat-model-pressed-link model) link))
               ((and row (togglable-item-p model row))
                ;; Don't toggle yet: arm a pending click so a drag from
                ;; this cell selects text over the block instead. The
                ;; toggle resolves on release.
                (arm (chat-model-pressed-tool-row model) row))
               ((restorable-prompt-p model row)
                ;; Click-to-restore: a press on a user prompt opens the
                ;; checkpoint picker (the Zig
                ;; openCheckpointRestoreDialogForPrompt affordance).
                (when-let (notice (slash-tree-op model))
                  (show-status model notice)))
               (t
                (if-let (word (and (note-left-press model (car cell) (cdr cell))
                                   (word-selection-at model (car cell) (cdr cell))))
                  (setf (chat-model-selection model) word
                        (chat-model-selection-anchor model)
                        (car word)
                        (chat-model-selection-dragged model) t)
                  (setf (chat-model-selection-anchor model) cell)))))))))))

(defun handle-mouse-motion (model msg &aux (x msg.x) (y msg.y) (button msg.button))
  "Drag legs (scrollbar, selection) while the button is held; hover state
on pure motion. => whether anything a frame shows moved."
  ;; Terminals report the pointer at every cell it crosses, and most
  ;; crossings move nothing on screen -- the same mark, the same hovered
  ;; card, the same picker row. Those folds are quiet (UPDATE's third value),
  ;; so the loop paints no frame for them.
  (let ((changed
         (cond
           ((and (chat-model-scrollbar-drag model) (eq button :left))
            ;; One slot, two bars: the surface the grab was taken on decides which
            ;; window the pointer is dragging, and each bar clamps its own track.
            (let ((drag (chat-model-scrollbar-drag model)))
              (if (eq :dialog (car drag))
                  (multiple-value-bind (first height) (dialog-scrollbar-metrics model)
                    (when height
                      (apply-dialog-scrollbar-track-row model (- y first) (cdr drag))))
                  (apply-scrollbar-track-row
                   model (max 0 (min (- y 4) (1- (transcript-band-metrics model)))) (cdr drag))))
            t)
           ((eq button :left)
            (when-let (anchor (chat-model-selection-anchor model))
              (let ((cell (frame-position model x y)))
                ;; A press that has not left its cell is not yet a drag: a
                ;; jittery click stays a click (the tool card still toggles,
                ;; the dialog row still confirms), and the mark appears when
                ;; the pointer actually moves. Clamped, so a drag past the
                ;; frame's edge marks the nearest cell rather than stopping.
                ;; The mark keeps its earlier endpoint first.
                ;; The mark is the transcript's words the drag covers
                ;; (LANE-MARK); one begun on the shell's chrome is still a
                ;; drag, so its release confirms nothing, but marks nothing.
                ;; Held past the band's first or last row, the drag scrolls
                ;; the transcript under it and the mark follows (DRAG-SCROLL-
                ;; STEP; the tick goes on scrolling while the pointer holds
                ;; still), and the mark is held on the transcript's lines, so
                ;; it keeps the rows that scroll out of the window.
                (unless (equal anchor cell)
                  (setf (chat-model-selection-dragged model) t)
                  (note-drag-edge model x y)
                  (let ((moved (extend-mark model anchor cell)))
                    (or (drag-scroll-step model) moved))))))
           ;; Nothing behind an open sheet hovers: its window is over it,
           ;; and the pointer in it chooses what the sheet's HOVER says.
           ((sheet-dialog-p (chat-model-dialog model))
            (let ((sheet (chat-model-dialog model)))
              (and (null button) sheet.hover
                   (multiple-value-bind (column row) (sheet-at model x y)
                     (and column (funcall sheet.hover model sheet column row))))))
           ;; A toast under the pointer holds its five seconds and lifts its ground,
           ;; and nothing under it hovers.
           ((and (null button) (toast-at model x y))
            (hover-toast model (toast-at model x y)))
           ((null button)
            ;; An open picker follows the pointer (the Zig hover_selectable
            ;; dialogs): motion over an occupied body row pulls the selection
            ;; there, and the highlight seam fires — the /themes live preview.
            (let* ((left (hover-toast model nil))
                   (dialog (chat-model-dialog model))
                   (selection (and (picker-dialog-p dialog) dialog.selection)))
              (if (dialog-apply-hover model y)
                  (or (not (eql selection dialog.selection)) left)
                  (multiple-value-bind (line column) (frame-transcript-position model x y)
                    ;; The pointer highlights tool cards, groups, folded thoughts
                    ;; and user prompts, and marks the one target it rests on: the
                    ;; terminal has no cursor to change shape, so the span that would
                    ;; answer a click underlines instead, and a reader can see the
                    ;; gesture before spending it.
                    (let* ((owner-line (or line (frame-transcript-position model x y t)))
                           (item (and owner-line (transcript-item-at-line model owner-line)))
                           (row (and item
                                     (or (togglable-item-p model item)
                                         (restorable-prompt-p model item))
                                     item))
                           (target (or (frame-target-at-position model x y)
                                       (and line
                                        (or (home-command-at-position model line column)
                                            (if-let (stop (band-stop-at model line column))
                                              (cons :stop stop)
                                              (let ((uri (link-at-position model line column)))
                                                (and (openable-target-p uri) uri))))))))
                      (or (unless (and (eq row (chat-model-hover-row model))
                                       (equal target (chat-model-hover-target model)))
                            (setf (chat-model-hover-row model) row
                                  (chat-model-hover-target model) target)
                            (render-state model)
                            t)
                          left)))))))))
    (let ((hover-changed (note-message-timestamp-hover model msg))
          (rail-changed (let ((on (transcript-rail-at-p model x y)))
                          (unless (eq on (chat-model-rail-hover model))
                            (setf (chat-model-rail-hover model) on)
                            ;; Only a painted rail shows the change.
                            (plusp (band-window-max-offset (chat-model-band model)))))))
      (or changed hover-changed rail-changed))))
(defun copy-frame-selection (model &aux (text (frame-selection-text model))
                                        (program (chat-model-program model)))
  "Hand MODEL's marked text to the program's clipboard writer, which never
waits behind the worker (a turn occupies it), and say in the hint lane what
became of it once the writer knows (CLIPBOARD-OUTCOME-NOTICE): copied only
when a clipboard tool took it. With nothing marked it says so, and with no
live program that nothing was written. The release that ends a drag and the
:COPY-SELECTION key both come here."
  (cond ((zerop (length text)) (show-status model +selection-empty-hint+))
        ((null program) (show-status model +selection-copy-failed-hint+ :error-p t))
        (t (nlt:write-clipboard program text
                                (lambda (outcome)
                                  (nlt:send program (clipboard-outcome-notice outcome)))))))

(defun clipboard-outcome-notice (outcome)
  "The composer status a clipboard write's OUTCOME (NLT:WRITE-CLIPBOARD)
says, as the SYSTEM-NOTICE-MSG that lands it on the loop thread: copied for
a clipboard tool, sent for OSC 52 alone, failed for neither."
  (multiple-value-bind (text error-p)
      (cond ((and (consp outcome) (eq :command (first outcome))) +selection-copied-hint+)
            ((eq :osc52 outcome) +selection-osc52-hint+)
            (t (values +selection-copy-failed-hint+ t)))
    (make-instance 'system-notice-msg :text text :error-p error-p :answer t)))

(defun mark-frame-cells (model selection)
  "=> ((ROW FIRST LAST) ...): the frame rows SELECTION washes on MODEL's
frame, each with its first and last column (inclusive), inside
TRANSCRIPT-LANE -- for a drag's mark the shown rows whose lines it holds
(MARK-SPAN), wherever the window has scrolled to."
  (multiple-value-bind (top bottom left right) (transcript-lane model)
    (let ((span (mark-span model)))
      (if span
          (destructuring-bind ((l1 . c1) . (l2 . c2)) span
            (let ((window (chat-model-band model))
                  (top-pad (nth-value 3 (transcript-band-metrics model))))
              (loop for row from top to bottom
                    for line = (band-window-row-index window (- row top top-pad))
                    when (and line (<= l1 line l2))
                      collect (list row
                                    (if (= line l1) (+ left c1) left)
                                    (if (= line l2) (min right (+ left c2)) right)))))
          (destructuring-bind ((r1 . c1) . (r2 . c2)) selection
            (loop for row from (max top r1) to (min bottom r2)
                  collect (list row
                                (if (= row r1) (max left c1) left)
                                (if (= row r2) (min right c2) right))))))))

(defun range-target (ranges index)
  "The TARGET of the (START END TARGET) in RANGES whose span holds INDEX, or NIL."
  (loop for (start end target) in ranges when (<= start index (1- end)) return target))

(defun frame-target-at-position (model x y)
  "The target at 1-based frame X,Y, recorded on that exact painted row.
Character ranges come from the painter's clipped segments; screen columns
are resolved through the frame's plain lane, including wide Unicode."
  (when (and (integerp x) (integerp y)
             (<= 1 x (frame-width model)) (<= 1 y (frame-height model)))
    ;; The plain lane first: reading it composes a frame that was never painted.
    (let ((plains (frame-plain-rows model)))
      (range-target (plain-row-links (aref (frame-lane-rows (frame-lanes model)) (1- y)))
                    (plain-column-index (aref plains (1- y)) (1- x))))))

(defun link-at-position (model line column &aux (plains (chat-model-line-plain model)))
  "The target painted at transcript visual LINE, display COLUMN, or NIL: the
ranges LLINE-ROWS recorded on that line's plain row."
  (when (and (integerp line) (< -1 line (length plains)) column)
    (let ((plain (aref plains line)))
      (range-target (plain-row-links plain) (plain-column-index plain column)))))

(defun open-target (uri &aux (path (file-uri-path uri)))
  "Hand URI — an http(s) page, or a file: target naming a file or a folder —
to the platform's own opener, detached with its output in the cache's
nodecode/open-url.log."
  ;; A folder reaches the operator's file manager and a file the program its
  ;; type is registered to; where the desktop answers a file with a chooser,
  ;; that chooser is what appears. The default handler decides, always: no
  ;; list of programs is kept here, and no host path is baked in. => T when
  ;; the opener started, else the keyword naming the refusal — :MISSING for a
  ;; file: target that is not there any more, :NO-OPENER for a POSIX shell
  ;; with neither WAYLAND_DISPLAY nor DISPLAY, which is an ssh session, or an
  ;; opener that would not start.
  ;;
  ;; A path is probed natively, never as a pathname: a `*' or a `[' in a file
  ;; name is a character here, the way it is everywhere else in this file.
  (if (and path
           (not (ignore-errors (uiop:probe-file* (uiop:parse-native-namestring path)))))
      :missing
      (let* ((target (or path uri))
             (command #+win32 (list "rundll32" "url.dll,FileProtocolHandler" target)
                      #+darwin (list "open" target)
                      #-(or win32 darwin)
                      (and (some (lambda (variable &aux (value (uiop:getenv variable)))
                                   (and value (plusp (length value))))
                                 '("WAYLAND_DISPLAY" "DISPLAY"))
                           (list "xdg-open" target))))
        (if (and command
                 (nlk:spawn-detached command (nlk:cache-path "nodecode/open-url.log")))
            t
            :no-opener))))

(defun open-refusal-hint (uri outcome &aux (path (file-uri-path uri)))
  "What a refused open says in the transient lane: which of the two things
it could not do, and why."
  ;; A click that did nothing has to say so — silence is what the operator
  ;; already reported as a broken click.
  (cond ((eq :missing outcome)
         (format nil "Not there any more — ~a" (home-abbreviated (or path uri))))
        (path "Nothing here opens files — drag across the path to copy it")
        (t "No browser to open links here — drag across one to copy it")))

(defun render-disclosure-at-cursor (model owner line y)
  "Reproject a toggled disclosure without moving its clicked owner at Y."
  (let* ((refs (chat-model-line-rows model))
         (relative (- line (or (position owner refs :test #'eq) line)))
         (window (chat-model-band model)))
    (clear-transcript-pin model)
    (setf (chat-model-follow-bottom model) nil)
    (render-state model)
    (let* ((refs (chat-model-line-rows model))
           (start (or (position owner refs :test #'eq)
                      (when-let (group (row-groups owner))
                        (position group refs :test #'eq))))
           (target (when start
                     (+ start (min relative
                                   (1- (loop for i from start below (length refs)
                                             while (eq (aref refs i) (aref refs start)) count t))))))
           (wanted (- y 4)))
      (when target
        ;; Invert the shared piece lane, including pinned prompt separators.
        ;; The first estimate puts the title inline; the correction accounts
        ;; for the prompt's synthetic rows without duplicating its policy.
        (loop with offset = (- target wanted)
              repeat 3
              do (setf (band-window-top window)
                       (if (<= offset 0) (1- offset)
                           (min offset (band-window-max-offset window))))
                 (let ((actual (loop with at = (band-window-top-padding window)
                                     for piece in (band-window-pieces window)
                                     for (start count) = (and (consp piece) piece)
                                     when (and (integerp start) (< -1 (- target start) count))
                                       return (+ at (- target start))
                                     do (incf at (if (integerp start) count 1)))))
                   (unless (and actual (/= actual wanted)) (return))
                   (incf offset (- actual wanted))))))))

(defun picker-dialog-command (kind)
  "The footer command a picker of KIND belongs to: a footer cell's repeat
click cancels the dialog its command opened. A kind is its command's name,
but /models walks three: its provider list, a provider's models and the
auxiliary model's."
  (case kind
    ((:model-providers :models :auxiliary-models) "models")
    (t (and kind (string-downcase (string kind))))))

(defun handle-mouse-release (model msg &aux (x msg.x)
                                            (y msg.y)
                                            (undragged (not (chat-model-selection-dragged model))))
  "Resolve the armed gestures: a scrollbar drag ends first, so it can never
confirm a row; an undragged tool-card press toggles the expand, an
undragged press on a band
row's ✕ interrupts that exec and one elsewhere on its lines opens or closes
it in place as a card's does,
an undragged press on a target opens it — a page in the browser, a file or a folder in whatever the
desktop opens it with —, an undragged click on an open dialog's row
confirms it, on a tab's chip shows that tab and on a key's hint answers that
key (=> its key press, which UPDATE runs as a typed one), and a dragged
selection copies to the clipboard through the program's own writer."
  (cond
    ;; A release that ends a scrollbar drag — the transcript's gutter or the
    ;; open dialog's own bar — clears the grab and confirms nothing: the
    ;; drag was the gesture, not the row the pointer came to rest on.
    ((chat-model-scrollbar-drag model)
     (setf (chat-model-scrollbar-drag model) nil))
    ;; A left click on an open sheet's window is the sheet's.
    ((sheet-dialog-p (chat-model-dialog model))
     (disarm-pointer model)
     (multiple-value-bind (column row) (sheet-at model x y)
       (when (and column (eq :left (nlt:mouse-event-button msg)))
         (funcall (sheet-dialog-click (chat-model-dialog model))
                  model (chat-model-dialog model) column row))))
    ((and (chat-model-pressed-tool-row model) undragged)
     (let ((pressed (chat-model-pressed-tool-row model)))
       (disarm-pointer model)
       (multiple-value-bind (line) (frame-transcript-position model x y)
         (when (and line (eq pressed (transcript-item-at-line model line)))
           ;; The Zig toggleExpandedToolCall accordion; a group's member closes to it.
           (let* ((key (cond ((tool-group-p pressed) pressed.key)
                             ((eq :tool-call (transcript-row-kind pressed))
                              (tool-row-expand-key pressed))
                             (t pressed)))
                  (group (row-groups pressed))
                  (current (chat-model-expanded-tool-call model)))
             (setf (chat-model-expanded-tool-call model)
                   (cond ((and (tool-group-p pressed) (eq :process (first key)))
                          ;; A run does what its arrow says: ▾ folds it, a
                          ;; member open in it too; ▸ opens its whole trail,
                          ;; a call open under its title still open in it.
                          (cond ((tool-group-open-p pressed current) nil)
                                ((tool-group-open-p pressed (open-key current)) (open-key current))
                                (t key)))
                         ((and group (not (tool-group-p pressed)) (eq :process (first group.key))
                               (not (tool-group-open-p group current)))
                          ;; A call under a shut run's title opens alone, the
                          ;; run still shut.
                          (unless (equal key (open-key current)) (list :fold key)))
                         ((and (tool-group-p pressed) (eq :mutation (first key))
                               (tool-group-open-p pressed current))
                          ;; Close the file list, not its enclosing work trail.
                          (and group (tool-group-key group)))
                         ((not (equal key current)) key)
                         ((and group (not (eq group pressed))) group.key))))
           (render-disclosure-at-cursor model pressed line y)))))
    ((and (chat-model-pressed-stop model) undragged)
     (let ((id (chat-model-pressed-stop model)))
       (disarm-pointer model)
       (multiple-value-bind (line column) (frame-transcript-position model x y)
         (when (and line (eql id (band-stop-at model line column)))
           (interrupt-band-exec model id)
           (render-status-frame model)))))
    ((and (chat-model-pressed-exec model) undragged)
     (let ((id (chat-model-pressed-exec model)))
       (disarm-pointer model)
       (let* ((line (frame-transcript-position model x y))
              (row (and line (band-exec-row-at model line))))
         ;; The view is /execs': a click here unfolds the lines.
         ;; Without this, a click would open a dialog instead.
         (when (and row (eql id (bg-band-row-shell-session-id row)))
           (let ((key (band-row-expand-key row)))
             (setf (chat-model-expanded-tool-call model)
                   (unless (equal key (chat-model-expanded-tool-call model)) key)))
           (render-disclosure-at-cursor model row line y)))))
    ((and (chat-model-pressed-link model) (eq :left (nlt:mouse-event-button msg)) undragged)
     (let ((uri (chat-model-pressed-link model)))
       (disarm-pointer model)
       (multiple-value-bind (line column) (frame-transcript-position model x y)
         (cond
           ((eq uri :status-hint) nil) ; hover-only chrome, not an action
           ((and line (consp uri) (eq :diff (first uri))
                 (equal uri (link-at-position model line column)))
            (let* ((row (transcript-item-at-line model line))
                   (group (row-groups row))
                   (current (chat-model-expanded-tool-call model))
                   (run (and group (eq :process (first group.key))))
                   (key (cond ((not (equal uri (open-key current))) uri)
                              ;; A run's call keeps its files listed under its line.
                              (run (tool-row-expand-key row))
                              (t (and group (tool-group-key group))))))
              ;; A file of a call open under a shut run's title keeps the run shut.
              (setf (chat-model-expanded-tool-call model)
                    (if (and run (not (tool-group-open-p group current))) (list :fold key) key))
              (render-disclosure-at-cursor model row line y)))
           ;; A tab's chip on the open panel shows its tab; a key's hint is
           ;; that key, handed back for UPDATE to run as a typed one.
           ((and (consp uri) (eq :dialog-tab (car uri))
                 (equal uri (frame-target-at-position model x y)))
            (picker-tab-pick model (cdr uri)))
           ((and (consp uri) (eq :key (car uri))
                 (equal uri (frame-target-at-position model x y)))
            (hint-key-press (cdr uri)))
           ;; A composer dropdown row is chosen as Enter chooses the
           ;; highlighted one: it takes the highlight, then Enter goes.
           ((and (consp uri) (eq :suggestion (car uri))
                 (equal uri (frame-target-at-position model x y)))
            (setf (chat-model-slash-selection model) (cdr uri))
            (nlt:make-key-press-msg :code :enter))
           ((equal uri (frame-target-at-position model x y))
            (if (and (consp uri) (eq :command (car uri)))
                (let* ((dialog (chat-model-dialog model))
                       (kind (and (picker-dialog-p dialog) (picker-dialog-kind dialog))))
                  ;; Every footer command toggles its canonical picker kind,
                  ;; not a whitelist of options. A repeat click cancels even
                  ;; with a query; it never selects or reruns the opener.
                  (if (and kind (string-equal (cdr uri) (picker-dialog-command kind)))
                      (dismiss-dialog model (picker-dialog-on-cancel dialog))
                      (multiple-value-bind (notice cmd) (execute-slash-command model (cdr uri) "")
                        (when notice (show-status model notice))
                        (when cmd (error "Footer command unexpectedly returned a program command"))))
                  (render-state model))
                (let ((outcome (open-target uri)))
                  (unless (eq t outcome)
                    (show-status model (open-refusal-hint uri outcome))
                    (render-state model)))))
           ((and (equal uri '(:home . :mark))
                 (equal uri (home-command-at-position model line column)))
            (click-mark model))
           ((and (consp uri) (eq :home (car uri))
                 (equal uri (home-command-at-position model line column)))
            (multiple-value-bind (notice cmd) (execute-slash-command model (cdr uri) "")
              (when notice (show-status model notice))
              (render-state model)
              (when cmd (error "Home view command unexpectedly returned a program command"))))
           ((and line (equal uri (link-at-position model line column)))
            (let ((outcome (open-target uri)))
              (unless (eq t outcome)
                (show-status model (open-refusal-hint uri outcome))
                (render-state model))))))))
    ;; An open dialog confirms on release: a press on its panel only armed
    ;; the anchor, so a click with no drag between it and this release
    ;; replays Enter on the row under the pointer (the Zig
    ;; handleActiveDialogHover press leg), while a drag confirms nothing.
    ;; The transcript's gestures above resolve first: a press over the
    ;; transcript armed them as with no dialog open.
    ((and (chat-model-dialog model) (eq :left (nlt:mouse-event-button msg)) undragged)
     (setf (chat-model-selection-anchor model) nil)
     (when (dialog-apply-hover model y)
       (picker-dialog-key model :enter)))
    ((and (chat-model-selection-anchor model)
          (chat-model-selection-dragged model)
          (chat-model-selection model))
     (disarm-pointer model)
     (copy-frame-selection model))
    (t
     (disarm-pointer model)
     (when (chat-model-selection model)
       (setf (chat-model-selection model) nil)
       (render-state model)))))

;;; --- update method ------------------------------------------------------

(defmethod update :around ((model chat-model) msg &aux (nlk:*attribution-label* (type-of msg)))
  "Attribution seam: every message pass — reduce plus any paint, local and
gateway shells both, since GATEWAY-CHAT-MODEL subclasses CHAT-MODEL — runs as
one attributed span, so /gc can split collections that stalled visible work
from the ones COLLECT-DURING-WAIT hid inside provider wait."
  ;; The label is the message's type, free of consing (TYPE-OF returns the
  ;; struct name); the gateway shell refines the binding to the sync payload
  ;; kind once it has decoded the frame, so a worst span reads 'turn_event',
  ;; not just 'gw-projection-msg'.
  (nlk:with-gc-attribution (call-next-method)))

(defmethod update ((model chat-model) msg)
  "Handle messages and update the model."
  ;; The editor sees only what nothing here claimed. Every branch below
  ;; consumes the message it acts on by saying nothing at all; the two that
  ;; deliberately hand one on — a key no branch names, and a submit with
  ;; nothing to submit — say so, once each.
  (let ((pass-to-editor nil)
        ;; Whether the fold changed nothing a frame would show (the tick
        ;; branch and pointer motion say so): UPDATE's third value.
        (quiet nil))

    ;; A key or pointer event is the operator working: the idle collector's
    ;; stop-the-world pause waits out *IDLE-INTERVAL* from it (gc.lisp).
    (when (or (nlt:key-press-msg-p msg) (nlt:paste-msg-p msg) (typep msg 'nlt::mouse-event))
      (nlk:note-operator-input))

    (cond
      ;; Key press - handle before passing to the editor
      ((nlt:key-press-msg-p msg)
       ;; A keybind row defined into this image since the model was built
       ;; (or a cell's override) reaches the keyboard here.
       (refresh-model-keymap model)
       (let* ((key (nlt:key-event-code msg))
              (mod (nlt:key-event-mod msg))
              (ctrl (nlt:mod-contains mod nlt:+mod-ctrl+))
              (alt (nlt:mod-contains mod nlt:+mod-alt+))
              (shift (nlt:mod-contains mod nlt:+mod-shift+))
              (dialog (chat-model-dialog model)))
         (cond
           ;; Quit dispatches through the keymap (defaults Ctrl+Q/Ctrl+D).
           ;; Ctrl+C clears a non-empty composer first (the Zig keybind
           ;; split), then stops a running turn the way Esc does
           ;; (INTERRUPT-TURN), and quits only when there is neither --
           ;; Codex's order. Without that order, Ctrl+C would quit a
           ;; running turn instead of stopping it. Read ahead of the dialog
           ;; ladder, so a picker open over a running turn stops it too.
           ;;
           ;; Ctrl+D keeps the unix EOF rule every shell and readline
           ;; program shares: it exits only on an empty composer. With text
           ;; in the box it stays the readline delete-at-cursor (the Zig
           ;; isDeleteAtCursorKey chord), which the editor below applies.
           ((and (model-key-matches-p model :quit key mod)
                 (not (and ctrl (characterp key) (char-equal key #\d)
                           (not (composer-empty-p model)))))
            (return-from update (values model (nlt:quit-cmd))))
           ((and ctrl (characterp key) (char= key #\c))
            (cond ((plusp (length (editor-value (chat-model-editor model))))
                   (editor-reset (chat-model-editor model)))
                  ((interrupt-turn model))
                  (t (return-from update (values model (nlt:quit-cmd))))))

           ;; Ctrl+F over an open dialog flips its panel between the Zig
           ;; docked height and every row the window can give it. It is
           ;; read ahead of the dialog's own ladder so every dialog — the
           ;; wizard's text steps included — answers it the same way; with
           ;; no dialog open the chord stays the editor's. The frame reads
           ;; the flag on its next compose, so nothing here repaints; the
           ;; flag outlives the dialog, so the next one opens at the same
           ;; size.
           ((and dialog (model-key-matches-p model :toggle-dialog-fullscreen key mod))
            (setf (chat-model-dialog-fullscreen model)
                  (not (chat-model-dialog-fullscreen model))))

           ;; Ctrl+R on the history panel and Ctrl+S on the sessions picker
           ;; are the toggle back: the chord that opened the panel takes it
           ;; away again, the way Esc closes it — one gesture, never a reach
           ;; for a second key. It closes outright rather than clearing the
           ;; query first the way Esc does: a toggle toggles. Gated on the
           ;; panel's own kind, so a chord never dismisses a dialog that is
           ;; not its own; read ahead of the modal ladder, so the panel never
           ;; swallows the chord that would dismiss it (the sessions picker's
           ;; query box would otherwise take the #\s).
           ((and dialog
                 (model-key-matches-p model (case (picker-dialog-kind dialog)
                                              (:prompt-history :history-dialog)
                                              (:sessions :sessions-dialog))
                                      key mod))
            (dismiss-dialog model (picker-dialog-on-cancel dialog)))

           ;; The image-paste chord over a dialog pastes the clipboard's text
           ;; into its field: a field holds no image, and Ctrl+V is what an
           ;; operator reaches for with a key to paste. Where nothing reads
           ;; a clipboard, the dialog's subtitle — the instruction row the
           ;; operator is reading — names the paste that does reach this
           ;; process: the terminal's own. Not the composer status: a docked
           ;; panel stands over the composer, and the subtitle is the line
           ;; under the field; over ssh it never showed.
           ((and dialog (model-key-matches-p model :paste-image key mod))
            (if-let (text (command-output *clipboard-paste-commands*))
              (picker-dialog-paste model text)
              (setf (picker-dialog-subtitle dialog)
                    "No clipboard to read here — paste with your terminal (Ctrl+Shift+V)")))

           ;; A picker nobody opened -- the reasoning effort /models offers
           ;; once a model lands -- is not where typing goes: a printable key
           ;; closes it, keeping what stands, and reaches the composer as if
           ;; it had never opened. Without this, a `yo' typed after the pick
           ;; vanished and its Enter picked a rung (Mac take, 2026-10-21).
           ((and dialog (picker-type-through dialog)
                 (characterp key) (graphic-char-p key) (not ctrl) (not alt))
            (dismiss-dialog model (picker-dialog-on-cancel dialog))
            (unless (chat-model-dialog model)
              (return-from update (update model msg))))

           ;; An open modal picker owns every remaining key.
           (dialog (picker-dialog-key model key :ctrl ctrl :alt alt :shift shift :mod mod))

           ;; Switch Developer / Standard viewport (Ctrl+O).
           ((model-key-matches-p model :toggle-viewport key mod)
            (toggle-tool-detail model)
            (render-state model))

           ;; `?` on an empty composer opens the keyboard help dialog (the
           ;; Zig handleHelpDialogKey composer-empty gate); with text it
           ;; stays a printable character.
           ((and (model-key-matches-p model :help key mod)
                 (composer-empty-p model))
            (open-help-dialog model))

           ;; Ctrl+R browses every prompt this shell submitted — the whole
           ;; recall ring in one searchable picker, where Up/Down walk it an
           ;; entry at a time. Deliberately no empty-composer gate: recalling
           ;; a past prompt is what to do when the box already holds a draft.
           ((model-key-matches-p model :history-dialog key mod)
            (open-prompt-history-dialog model))

           ;; Ctrl+S opens the sessions picker — the Zig
           ;; open_session_launcher chord (keySpec('s', .{ .ctrl = true })),
           ;; whose lisp surface is /sessions, so both reach the one op. No
           ;; empty-composer gate: switching session is what to do with a
           ;; draft in the box. A shell that cannot reach a gateway answers
           ;; the op's refusal as a notice, which lands in the composer status
           ;; exactly as the slash command's does (SUBMIT-SLASH's contract);
           ;; on the gateway shell the directory read runs on its own thread,
           ;; so no frame waits for it.
           ((model-key-matches-p model :sessions-dialog key mod)
            (when-let (notice (slash-sessions-op model))
              (show-status model notice)
              (render-state model)))

           ;; Ctrl+P on an empty composer opens the command palette: it
           ;; inserts "/" and the slash dropdown appears from the composer
           ;; text — the palette IS the slash dropdown under another
           ;; binding (Zig openComposerCommandPalette). With text the key
           ;; is previous-suggestion navigation while a dropdown is open
           ;; (Zig isPreviousSuggestionKey) and otherwise a consumed no-op
           ;; — never a raw #\p insert. The dialog branch above already
           ;; suppresses it while any dialog is open.
           ((model-key-matches-p model :open-command-palette key mod)
            (cond
              ((composer-empty-p model)
               (editor-insert
                (chat-model-editor model) "/"))
              ((composer-suggestions-for model)
               (composer-dropdown-key model :up))))

           ;; Image paste (default Ctrl+V / Ctrl+Alt+V): probe the system
           ;; clipboard for an image and stage it as a pending attachment.
           ((model-key-matches-p model :paste-image key mod)
            (paste-clipboard-image model))

           ;; Copy the marked text again (default Ctrl+Shift+C) — anywhere on
           ;; the frame, since the mark is a screen region.
           ((model-key-matches-p model :copy-selection key mod)
            (copy-frame-selection model))

           ;; Newline through the keymap (defaults Shift+Enter, Alt+Enter,
           ;; Ctrl+J). Plain Enter stays submit.
           ((model-key-matches-p model :insert-newline key mod)
            (editor-insert
             (chat-model-editor model) (string #\Newline)))

           ;; Dropdown navigation while any suggestion lane matches: the
           ;; slash lane, or the @file token lane. Accept-suggestion
           ;; (default Tab) completes the highlighted row. Ctrl+N walks the
           ;; list forward (the Zig isNextSuggestionKey chord); its backward
           ;; twin Ctrl+P is the command-palette branch above, which routes
           ;; to the same dropdown once one is open.
           ((and (composer-suggestions-for model)
                 (let ((dropdown-key
                         (cond ((model-key-matches-p model :history-previous key mod) :up)
                               ((or (model-key-matches-p model :history-next key mod)
                                    (and ctrl (characterp key) (char-equal key #\n)))
                                :down)
                               ((model-key-matches-p model :accept-suggestion key mod) :tab))))
                   (and dropdown-key (composer-dropdown-key model dropdown-key)))))

           ;; Prompt history recall: Up at the end of the text walks older
           ;; prompts, Down walks back toward the stashed draft. Each helper
           ;; declines — falling through to cursor movement — when it does
           ;; not apply.
           ;; While this composer holds a peeled prompt (the Up peel), both
           ;; history keys stay cursor keys: a walk would replace the text
           ;; the peels put there for editing.
           ((and (null (held-prompt (chat-model-state model)))
                 (model-key-matches-p model :history-previous key mod)
                 (composer-history-previous model)))
           ((and (null (held-prompt (chat-model-state model)))
                 (model-key-matches-p model :history-next key mod)
                 (composer-history-next model)))

           ;; Transcript paging. These belong to the chat window, not the
           ;; composer — the composer scrolls internally with the cursor.
           ((when-let (action (cond ((model-key-matches-p model :page-up key mod) :page-up)
                                    ((model-key-matches-p model :page-down key mod) :page-down)
                                    ;; Home/End jump the transcript window (Zig scroll_home/end)
                                    ;; while the composer is empty; with text they stay editing keys.
                                    ((plusp (length (editor-value (chat-model-editor model)))) nil)
                                    ((model-key-matches-p model :scroll-home key mod) :home)
                                    ((model-key-matches-p model :scroll-end key mod) :end)))
              (scroll-transcript model action)
              t))

           ;; Enter semantics belong to the keymap: the shell's own submit
           ;; arm (client.lisp) claims the submit chord before this update
           ;; runs, and an Enter chord matching neither submit nor
           ;; insert-newline (both rebound) must not reach the editor, whose
           ;; own default would insert a newline behind the operator's
           ;; configuration.
           ((eq key :enter))
           (t (setf pass-to-editor t)))))

      ;; Window resize
      ((nlt:window-size-msg-p msg)
       (let ((width (nlt:window-size-msg-width msg))
             (height (nlt:window-size-msg-height msg)))
         (setf (chat-model-term-width model) width
               (chat-model-term-height model) height
               ;; The band's rows are re-wrapped by the display policy at the new
               ;; width on the RENDER-STATE below; the composer takes its own
               ;; box-derived width and re-wraps its live text there.
               (editor-width (chat-model-editor model)) (composer-text-width width)
               ;; The composer growth cap tracks the window; the viewport takes
               ;; whatever the composer's current height leaves.
               (editor-max-height (chat-model-editor model)) (composer-max-rows height))
         (sync-viewport-geometry model)

         (render-state model)))

      ;; Bracketed paste arrives as one message, and it lands where typing
      ;; would: an open dialog's field when it has one — the API key the
      ;; setup wizard asks to have pasted — else the composer, through the
      ;; collapse rule ([Paste #N] past the Zig bounds).
      ;; An EMPTY paste is the terminal's own paste key over a clipboard that
      ;; holds no text: Windows Terminal keeps Ctrl+V for itself and, with only
      ;; an image copied, sends ESC [ 200 ~ ESC [ 201 ~ and nothing between --
      ;; so it asks for the image, as the paste-image key does. A paste that is
      ;; only an image file's path -- a snip, an image copied in Explorer, a
      ;; file dragged in -- stages that file.
      ((nlt:paste-msg-p msg)
       (cond ((chat-model-dialog model)
              (picker-dialog-paste model (nlt:paste-msg-text msg)))
             (t (let ((text (nlt:paste-msg-text msg)))
                  (if (zerop (length text))
                      (paste-clipboard-image model)
                      (multiple-value-bind (media-type base64) (pasted-image-file text)
                        (if media-type
                            (stage-composer-image model media-type base64)
                            (attach-composer-paste model text)))))
                (reset-history-navigation model)
                (setf (chat-model-slash-selection model) 0))))

      ;; The terminal answered the kitty graphics query: image cards gain
      ;; their inline preview band from the next render on.
      ((nlt:kitty-graphics-msg-p msg)
       (setf (chat-model-kitty-graphics model)
             (and (nlt:kitty-graphics-msg-ok-p msg) t))
       (render-state model))

      ;; The terminal answered the truecolor probe: its SGR honors 24-bit
      ;; color whatever the environment claimed. Upgrade and repaint — the
      ;; row caches key on the profile, so the quantized paint settles out.
      ;; A no never downgrades: the environment's guess stands.
      ((nlt:truecolor-msg-p msg)
       (when (and (nlt:truecolor-msg-ok-p msg) (color-profile-upgradable-p))
         (setf *color-profile* :truecolor)
         (render-state model)))

      ;; The terminal said what its background is: a theme that leaves the
      ;; canvas to it solves an answer's colours on that (ANSWER-TONES).
      ((nlt:background-msg-p msg)
       (unless (equal *terminal-canvas* (nlt:background-msg-rgb msg))
         (setf *terminal-canvas* (nlt:background-msg-rgb msg))
         (engage-theme)
         (render-state model)))

      ;; Mouse wheel: the overlay under the pointer owns the notch — an open
      ;; dialog's list scrolls with the highlight staying under the cursor, the
      ;; composer dropdown moves its selection — and otherwise the transcript
      ;; takes it wherever the pointer is, or the /term panel while its
      ;; takeover owns the band.
      ((nlt:mouse-wheel-msg-p msg)
       (handle-mouse-wheel model msg))

      ;; Mouse gestures: press arms (scrollbar grab, tool card, selection
      ;; anchor), motion drags (thumb, selection) or hovers, release
      ;; resolves (expand toggle, copy-on-release).
      ((nlt:mouse-click-msg-p msg)
       (handle-mouse-press model msg))
      ((nlt:mouse-motion-msg-p msg)
       (setf quiet (not (handle-mouse-motion model msg))))
      ;; A click on a key's hint answers its key press: the key goes the way
      ;; a typed one goes, every chord and dialog ladder included.
      ((nlt:mouse-release-msg-p msg)
       (let ((key (handle-mouse-release model msg)))
         (when (nlt:key-press-msg-p key)
           (return-from update (update model key)))))

      ;; Tick from the frame clock: state a stream reducer dirtied since
      ;; the last frame takes one full render (the coalesced projection of
      ;; every delta that landed this frame); otherwise, while live chrome
      ;; animates, only the chrome recomposes over the cached projection so
      ;; the spinner, elapsed clock, and waiting sway advance between
      ;; stream frames. Pacing lives in the frame-clock thread
      ;; (NLT:INIT starts it) — a tick command re-armed here would queue on
      ;; the size-1 command pool behind a running local turn and never fire
      ;; mid-turn. The tick must not fall through to the editor: its NIL
      ;; command would clobber any pending TA-CMD.
      ((nlt:tick-msg-p msg)
       ;; A composer-catalog walk that finished since the last frame lands
       ;; here, so the rows the operator is waiting for paint with this one
       ;; (START-COMPOSER-CATALOG-WALK — the walk thread ticks). Deferred
       ;; dialog-highlight delivery rides the tick next, before the render
       ;; decision: a handler that dirties the transcript (the sessions
       ;; preview hydrate) gets this frame's full render; then an open
       ;; dialog's ON-TICK gets the tick and answers whether it moved
       ;; anything. A tick that moved nothing is QUIET: the loop skips the
       ;; compose and its diff. The dialog's clocks keep the ticks coming
       ;; (LIVE-CHROME-ANIMATING-P); without the skip, an idle dialog would
       ;; pay that compose and diff on every tick, while its handler says
       ;; which matter.
       ;; A running turn's ticks are gated the same way: the chrome is
       ;; re-read on each (STATUS-CHROME-MOVED-P) and composed only when a
       ;; clock of its stepped, so a tick inside the spinner's 150 ms frame
       ;; and the elapsed clock's 100 ms digit costs one bar render, not a
       ;; frame compose and its diff.
       (let* ((landed (land-composer-catalog-walks model))
              ;; A drag held still past the band's edge scrolls on the clock.
              (dragged (drag-scroll-step model))
              ;; News whose five seconds ran out leaves on this frame, and a
              ;; countdown strip that moved a step since the last one shows
              ;; it; a tick that moved neither stays quiet.
              (expired (or (expire-toasts model) (toast-strips-moved-p model)))
              (flushed (picker-flush-highlight model))
              (dialog (chat-model-dialog model))
              (ticked (and (picker-dialog-p dialog)
                           dialog.on-tick
                           (funcall dialog.on-tick model dialog)))
              (painted (cond ((or (chat-model-transcript-dirty model)
                                  ;; A place whose old thing has gone out takes
                                  ;; the newcomer where it stood.
                                  (fade-due-p model))
                              (render-state model)
                              t)
                             ((and (live-chrome-animating-p model :dialog-ticks nil)
                                   (status-chrome-moved-p model))
                              (render-status-frame model)
                              t)
                             ((mark-glowing-p model)
                              (render-state model)
                              t)
                             ;; The ember, the fades and the glints are the
                             ;; frame's paint: the rows stand.
                             ((ember-moved-p model) t)
                             ((fade-moved-p model) t)
                             ((glint-rows model) t))))
         (setf quiet (not (or landed dragged expired flushed ticked painted))))))

    ;; Hand the key to the editor. The viewport deliberately gets no
    ;; messages: its own keymap (j/k/space/arrows) would fight the composer
    ;; for every keystroke, so the transcript moves only through the
    ;; explicit paging/wheel branches above. Any real edit ends a history
    ;; walk and re-homes the dropdown selection (the Zig every-text-mutation
    ;; reset); motion alone does not.
    (when (and pass-to-editor
               (editor-handle-key (chat-model-editor model) msg))
      (reset-history-navigation model)
      (setf (chat-model-slash-selection model) 0)
      ;; A command's trigger character opens its dialog over the draft.
      (when-let (entry (composer-trigger-command model msg))
        (when-let (notice (run-gateway-command model entry "" :trigger entry.trigger))
          (show-status model notice))))

    ;; The composer may have grown or shrunk under any of the branches
    ;; above; the transcript band always takes what it leaves.
    (sync-viewport-geometry model)

    ;; Every command a branch answers returns from that branch itself; the
    ;; shared tail answers none, and says whether the fold was quiet.
    (values model nil quiet)))

;;; --- frame composition ------------------------------------------------------
;;; The whole terminal frame, assembled row by row: the renderer
;;; paints any cell the view string does not cover as a reset-background
;;; blank, so canvas parity with the Zig full-frame memset means emitting
;;; exactly TERM-HEIGHT rows, each TERM-WIDTH columns, every one carrying
;;; the canvas background under a painting profile. The stack is the Zig
;;; ChatShell: blank, header, top padding, the transcript band with its
;;; scroll gutter, a gap, the composer, and the footer lane on tall windows.

(defun pad-frame-row (content used width bg &aux (pad (- width used)))
  "CONTENT, whose visible width is USED, padded to WIDTH with canvas cells."
  (if (plusp pad)
      (concatenate 'string content (blank-frame-row pad bg))
      content))

(defun format-token-count (value)
  "Compact token count (Zig FooterStatus.appendTokenCount, extended): plain
below 1000, tenth-precision k to a million, tenth-precision M past it, the
trailing .0 dropped."
  ;; The M tier exists for the header's bare-occupancy lane: a million-token
  ;; prompt spelled `1200k` reads as a count, `1.2M` reads as a window.
  (let ((value (or value 0)))
    (flet ((tenths-of (unit suffix &aux (tenths (floor (+ (* value 10) (floor unit 2)) unit)))
             (format nil "~d~@[.~d~]~a" (floor tenths 10)
                     (and (plusp (mod tenths 10)) (mod tenths 10)) suffix)))
      (cond ((< value 1000) (format nil "~d" value))
            ((< value 999950) (tenths-of 1000 "k"))
            (t (tenths-of 1000000 "M"))))))

(defun format-occupancy-percent (used window)
  "Window occupancy as a tenth-precision percent, half-up."
  ;; Whole percent is too coarse for the header: a 1.5k prompt against a 200k
  ;; window is 0.75%, which floors to a flat `0%` — indistinguishable from an
  ;; empty window — and every prompt below 2k reads the same. The tenth keeps
  ;; the early session moving. Capped at 999.9% so a mis-sized window cannot
  ;; widen the lane.
  (if (or (null window) (zerop window))
      "100.0%"
      (let ((tenths (min 9999 (floor (+ (* used 1000) (floor window 2)) window))))
        (format nil "~d.~d%" (floor tenths 10) (mod tenths 10)))))

(defun header-status-text (model &key (detail (chat-model-tool-detail model)))
  "The header's right lane (Zig chatShellHeaderStatus): what the SESSION has
cost and how full its context is; Standard keeps context and cache hit rate, Developer the bill."
  ;; Both are session-scoped on purpose — this lane is the standing readout
  ;; the operator glances at, so it must not shrink back to a single turn's
  ;; numbers every time a turn starts.
  ;;
  ;;   occupancy  the newest provider round's prompt over the window it was sent
  ;;              to, as `43.8%/200k`. Occupancy is a snapshot, never a sum: an
  ;;              agent loop resends a growing prefix each round, so summed
  ;;              prompts exceed the window many times over. When no source
  ;;              knows the window (no config entry, no catalog row — the
  ;;              capability seam answers NIL) the lane shows the prompt's
  ;;              absolute size instead, `ctx 87.6k`: a percent of nothing is
  ;;              meaningless, but the operator still wants to see the context
  ;;              grow. The `ctx` word is the live bar's pressure-clause label —
  ;;              a bare `87.6k` next to the arrows reads as one more count, the
  ;;              label says which one.
  ;;   ↑ / ↓      every round of every turn, summed — the session's running bill.
  ;;
  ;; The arrows carry the same two notes the live transcript bar and the finish
  ;; divider carry, reciprocated at session scope: `(cache N%)` is the share of
  ;; the session's whole prompt bill that was served from cache
  ;; (FORMAT-CACHE-HIT-PERCENT, the rule the turn meter reads), `(reasoning N)` is
  ;; what the session spent thinking inside its completion bill. Each note joins
  ;; only when the provider actually reported it — a lane that always printed
  ;; `(cache 0%)` would claim a fact no round ever sent.
  ;;
  ;; The window resolves through the capability seam from the model the newest
  ;; round itself named. NIL before any usage fact exists.
  (let ((parts (header-status-parts model :detail detail)))
    (and parts (format nil "~{~a~^ · ~}" (mapcar #'car parts)))))

(defun header-status-segs (model)
  "HEADER-STATUS-TEXT as the header row paints it: each part in its tone,
the gutters muted."
  (loop for ((text . tone) . more) on (header-status-parts model)
        collect (make-seg :text text :tone tone)
        when more collect (make-seg :text " · " :tone :muted)))

(defun header-status-parts (model &key (detail (chat-model-tool-detail model)))
  "The header's right lane as (TEXT . TONE) parts, NIL before any usage
fact: HEADER-STATUS-TEXT's words. The occupancy warms as it nears the
eviction line -- :WARN with the line's size from three quarters of the
window, :DANGER with what the next request does once past it -- and every
other part is muted."
  ;; One number, one place: the lane is the only meter of the window the
  ;; frame keeps standing, so the line it nears is said here rather than
  ;; drawn again beside it.
  (let* ((state (visible-transcript-state model))
         (session (json-value state :any "session_usage"))
         (context (json-value state :any "context_usage")))
    ;; A /sessions preview of a session longer than its page has summed only
    ;; the page: its lane waits for the gateway's numbers for the whole
    ;; session (USAGE_PENDING, PREVIEW-INSTALL-HEADER), never shows the page's.
    (when (and session (not (gethash "usage_pending" state)))
      (destructuring-bind (&key input output cached reasoning cost-known cost &allow-other-keys)
          session
        (let* ((used (context-occupancy-tokens context))
               (window (and (getf context :model)
                            (ignore-errors
                              (model-context-tokens (getf context :model)))))
               (share (and window (plusp window) (/ used window)))
               (past (and share (>= share *context-eviction-trigger-ratio*)))
               (near (and share (>= share 3/4)))
               (tone (cond (past :danger) (near :warn) (t :muted)))
               (hit (and cached (plusp cached) (format-cache-hit-percent input cached))))
          (remove nil
                  (list
                   (when (plusp used)
                     (cons (if window
                               (format nil "~a/~a"
                                       (format-occupancy-percent used window)
                                       (format-token-count window))
                               (format nil "ctx ~a" (format-token-count used)))
                           tone))
                   (when near
                     (cons (if past
                               "evicts next request"
                               (format nil "evicts at ~a"
                                       (format-token-count (* window *context-eviction-trigger-ratio*))))
                           tone))
                   (when-let (text (if (eq detail :developer)
                                       (format nil "↑ ~a~@[ (cache ~a)~]" (format-token-count input) hit)
                                       (and hit (format nil "cache ~a" hit))))
                     (cons text :muted))
                   (when (eq detail :developer)
                     (cons (format nil "↓ ~a~@[ (reasoning ~a)~]" (format-token-count output)
                                   (and reasoning (plusp reasoning) (format-token-count reasoning)))
                           :muted))
                   (when (and (eq detail :developer) cost-known)
                     (cons (format nil "$~,3f" (or cost 0.0)) :muted)))))))))

(defun clip-text-to-width (text max-width)
  "TEXT clipped to at most MAX-WIDTH display columns, an … marking the cut."
  ;; The chrome-row overflow guard: every frame row must render exactly the
  ;; frame width, so a lane that cannot fit elides instead of spilling past
  ;; the terminal edge.
  (if (<= (text-display-width text) (max 0 max-width))
      text
      (let ((budget (1- max-width)))
        (if (plusp budget)
            (concatenate 'string (take-display-prefix text budget) "…")
            ""))))

(defun bilateral-frame-row (left-text left-tone right-text width bg &optional left-segs right-segs)
  "One chrome row: the clipped segments own both paint and target ranges.
The left text starts on the gutter rail and the right one ends where the
message boxes do, the scroll gutter's columns left clear."
  (let* ((segments (clip-segs (or left-segs
                                (list (make-seg :text left-text :tone left-tone)))
                            (max 1 (- width 3)) :mark "…"))
         (left-width (segs-display-width segments))
         (right (or right-segs (and right-text (list (make-seg :text right-text :tone :muted)))))
         (right-width (segs-display-width right))
         (inset (- width 2 (transcript-text-width width)))
         (right-fits (and right (<= (+ 3 left-width 2 right-width inset) width)))
         (gap (if right-fits (- width 3 left-width right-width inset) 0))
         (painted (if right-fits
                      (append segments (list (make-seg :text (make-string gap :initial-element #\Space))) right)
                      segments))
         ;; Finalize string identity before attaching EQ-keyed metadata. The
         ;; frame guard keeps this exact-width row, and frame gestures read it.
         (row (nlk:narrow-string
               (pad-frame-row
                (concatenate 'string (blank-frame-row 3 bg)
                             (apply #'concatenate 'string
                                    (mapcar (lambda (seg) (seg-render seg bg)) painted)))
                (+ 3 left-width (if right-fits (+ gap right-width) 0)) width bg))))
    (link-plain-row row (seg-links painted 3))))

(defun session-display-title (model &key (fallback (format nil "~a" (transcript-shown-session-id model))))
  "The title of the session the transcript shows: the name its state carries
(SESSION_TITLE), else the first committed prompt's first line, or FALLBACK
before any prompt. Directory surfaces default to the raw id; Standard's header
supplies a blank."
  ;; A /sessions preview holds one page of a session's history, whose first
  ;; prompt need not be the session's: the gateway's name for it rides the
  ;; previewed state (PREVIEW-PAGE-ENTRY), a blank while it is not known
  ;; rather than a prompt from the middle of the log. An attached session
  ;; folds its whole history and carries no name here.
  (let* ((state (visible-transcript-state model))
         (named (gethash "session_title" state)))
    (cond
      ((stringp named) (if (plusp (length named)) (first-line named 48) fallback))
      (t (if-let (first-user (find-transcript-row (lambda (row)
                                                    (and (eq :text row.kind)
                                                         (equal "user" row.role)
                                                         (plusp (length row.text))))
                                                  state))
           (first-line first-user.text 48)
           fallback)))))

(defvar *workspace-footer-label* nil
  "Memoized WORKSPACE-FOOTER-LABEL: nothing in the organism ever chdirs, so
the cwd is a launch fact — and the footer read its GETCWD syscall plus
pathname parse on every rendered frame.")

(defun workspace-footer-label ()
  "The workspace identity, home-abbreviated (the Zig workspaceFooterLabel
rule): `~/src/thing`, bare `~` at home, the trimmed path elsewhere."
  (or *workspace-footer-label*
      (setf *workspace-footer-label*
            (home-abbreviated (namestring (uiop:getcwd))))))

(defun forget-workspace-memos ()
  "Drop both cwd memos — the footer's label and the directory a clicked
relative path resolves against — so no dumped image carries the BUILD directory's
working directory: warm.lisp renders a frame as the last act of the load,
which memoizes it, and `just release' runs from src/ — so without this every
shipped binary showed `~/nodecode/nodecode/src' (or a packer's throwaway
tree) in its footer on every box, forever."
  ;; The cwd is a launch fact, but the launch that matters is the operator's,
  ;; not the builder's. Rides *SAVE-HOOKS* beside THEME's FORGET-COLOR-PROFILE
  ;; and NLT's FORGET-TERMINAL.
  (setf *workspace-footer-label* nil
        *workspace-directory* nil))

(pushnew 'forget-workspace-memos sb-ext:*save-hooks*)

(defun hover-seg (model text target &optional (tone :muted))
  "TEXT linked to TARGET: the disclosure accent while MODEL's pointer is on it, else TONE."
  (make-seg :text text :link target
            :tone (if (equal target (chat-model-hover-target model)) :disclosure-hover tone)))

(defun footer-frame-row (model width bg)
  "The footer's model, effort and actions left; profile, cwd and gateway right.
Segments are the one authority for text, color, clipping and target ranges."
  (multiple-value-bind (raw-provider raw-model state) (footer-identity model)
    (let* ((provider (json-value raw-provider :text))
           (model-id (json-value raw-model :text))
           (placeholder (cond ((eq state :pending) "setting up…")
                              ((and (chat-model-setup-walk model) (chat-model-dialog model)
                                    (or (eq state :unconfigured) (not (or provider model-id))))
                               "setting up…")
                              ((eq state :unconfigured) "run /setup to add a model")))
           (identity (cond ((and provider model-id)
                            (let ((model-id (if (and (> (length model-id) (1+ (length provider)))
                                                     (prefix-at-p model-id 0 (format nil "~a/" provider)))
                                                (subseq model-id (1+ (length provider))) model-id)))
                              ;; `deepseek deepseek-flash' said the provider twice.
                              (if (model-names-provider-p provider model-id)
                                  model-id
                                  (format nil "~a ~a" provider model-id))))
                           (t (or model-id provider)))))
      (flet ((action (text name) (hover-seg model text (cons :command name))))
        (let* ((left (if placeholder
                         (list (make-seg :text placeholder :tone :muted))
                         ;; The name is green or red by who can read a prompt sent to it
                         ;; (CONNECTION-IDENTITY-TONE): the footer's whole privacy word.
                         (append (and identity (list (hover-seg model identity (cons :command "models")
                                                                (or (connection-identity-tone model) :muted))))
                                 (when-let (effort (and provider model-id (footer-reasoning-effort model)))
                                   (list (action (format nil " (~a)" effort) "think")))
                                 (list (make-seg :text " · " :tone :muted)
                                       (action (footer-gesture-hint model) "sessions")))))
               (right (find-if (lambda (candidate)
                                 (<= (+ 3 (segs-display-width left) 2
                                        (segs-display-width candidate)
                                        (- width 2 (transcript-text-width width)))
                                     width))
                               (footer-right-candidates model))))
          (bilateral-frame-row (segs-text left) :muted (and right (segs-text right))
                               width bg left right))))))

;;; The projected line strings are identity-stable across frames
;;; (ROW-DISPLAY-BLOCK memoizes per row), so the per-row inset+pad+gutter
;;; concatenate — a measured animation-tick heap hotspot — runs once per line,
;;; not once per frame. Weak keys age out with the projection cache.
(defvar *band-row-cache*
  (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key)
  "viewport line string -> (thumb-row . track-row) banded frame rows.")

(defvar *band-row-cache-epoch* nil
  "(text-width bg palette profile bar-p hot) the band-row cache was built
under; any drift flushes it wholesale.")

(defun transcript-band-rows (model width text-width bg)
  "The transcript band: each viewport row inset 2 columns from the left,
padded to TEXT-WIDTH, then one blank column and the scroll rail's cell in
the frame's last column, WIDTH - 1."
  ;; Submitted questions stay top-aligned, inline or pinned; only a
  ;; promptless short band paints its canvas slack above the content.
  (bind ((window (chat-model-band model))
         (vp-height window.height)
         ;; The visible rows ride straight out of the window's list, so the
         ;; line identity the band-row cache keys on survives the frame.
         (lines (let* ((visible (band-window-visible window text-width
                                            (when (chat-model-hover-row model)
                                              (position (chat-model-hover-row model)
                                                        (chat-model-line-rows model) :test #'eq))))
                       (top-pad (band-window-top-padding window)))
                  (append (make-list top-pad :initial-element "")
                          visible
                          (make-list (- vp-height top-pad (length visible)) :initial-element ""))))
         (inset (if bg (styled "  " (vstyle :bg bg)) "  "))
         (gap (if bg (styled " " (vstyle :bg bg)) " "))
         ;; A band with nothing to scroll draws no bar: a full-height thumb
         ;; would be the strongest mark on the screen and say nothing.
         (bar-p (plusp (band-window-max-offset window)))
         ;; The rail is its half-width thumb alone, lit while the pointer
         ;; rests on the gutter or drags it; off the thumb the gutter is
         ;; canvas. Without this, the Zig gutter's lit column and full-block
         ;; thumb would read as an empty pane beside the text, and a hairline
         ;; track under the thumb as a line down the whole terminal.
         (hot (rail-hot-p model))
         (thumb-cell (styled "▐" (vstyle :fg (visual-style-fg (style-for-tone (if hot :foreground :muted)))
                                          :bg bg)))
         (track-cell gap)
         (epoch (list text-width bg *palette* (effective-color-profile) bar-p hot))
         ((thumb-top thumb-height)
          (scrollbar-thumb vp-height vp-height (+ vp-height (band-window-max-offset window))
                           (band-window-offset window))))
    (unless (equal *band-row-cache-epoch* epoch)
      (setf *band-row-cache-epoch* epoch)
      (clrhash *band-row-cache*))
    ;; Where the running calls' listed lines stand this frame, for the ember:
    ;; under the frame head's three rows, past the two-column inset.
    (setf (ember-rows model)
          (loop for line in lines
                for y from 3
                for call = (ember-line line)
                for plain = (and call (plain-frame-row line))
                for start = (and plain (position #\Space plain :test-not #'char=))
                when start
                  collect (list y (+ 2 (text-display-width (subseq plain 0 start)))
                                (+ 2 (text-display-width (string-right-trim " " plain)))
                                call))
          ;; And where the rows of a place still fading stand, for its light.
          (fade-rows model)
          (loop for line in lines
                for y from 3
                for (fade . item) = (and (plusp (length line)) (fade-line line))
                for plain = (and fade (not (fade-settled-p fade)) (plain-frame-row line))
                for start = (and plain (position #\Space plain :test-not #'char=))
                when start
                  collect (list y (+ 2 (text-display-width (subseq plain 0 start)))
                                (+ 2 (text-display-width (string-right-trim " " plain)))
                                fade item))
          ;; And where the chips a glint still crosses stand.
          (glint-rows model)
          (loop with now = (get-internal-real-time)
                for line in lines
                for y from 3
                nconc (loop for (x0 x1 at) in (and (plusp (length line)) (row-glints line))
                            when (< (since-ms at now) +glint-ms+)
                              collect (list y (+ 2 x0) (+ 2 x1) at))))
    (flet ((banded (line thumb-p)
             (let* ((row (nlk:narrow-string
                          (concatenate
                           'string inset
                           ;; Padding measures painted text, never control bytes.
                           (if (zerop (length line))
                               (blank-frame-row text-width bg)
                               (if bg line (pad-frame-row line (text-display-width (plain-frame-row line))
                                                          text-width nil)))
                           gap (if thumb-p thumb-cell track-cell)))))
               ;; The band owns the final painted string; preserve its source
               ;; ranges after the inset, on both fresh renders and memo hits.
               (link-plain-row row (loop for (start end target) in (plain-row-links line)
                                         collect (list (+ 2 start) (+ 2 end) target))))))
      (loop for line in lines
            for row from 0
            for thumb-p = (and bar-p (<= thumb-top row (+ thumb-top thumb-height -1)))
            collect
            (let ((entry (alexandria:ensure-gethash line *band-row-cache* (cons nil nil))))
              (if thumb-p
                  (or (car entry) (setf (car entry) (banded line t)))
                  (or (cdr entry) (setf (cdr entry) (banded line nil)))))))))

(defun blank-band-line-p (line text-width bg)
  "Whether LINE, one of the band's visible rows, paints bare canvas: the
empty spacer row or a block's unfilled pad."
  (or (zerop (length line))
      (string= line (first (lline-render (block-pad-lline nil) text-width)))
      (and bg (string= line (blank-frame-row text-width bg)))))

(defun steer-key-segs (model &aux (keymap (chat-model-keymap model)))
  "The keys that answer a running turn once the composer holds text, each a
filled cap before its verb: Enter steers the turn (the client's steer leg),
the queue key starts the next one after it. Two columns follow each verb,
the last two the box's right margin."
  ;; Both are rebindable, so they are read from the keymap, and an unbound
  ;; one drops out. Esc stands on the live bar (LIVE-STATUS-LINE).
  (loop for (action verb) in '((:submit "steer") (:submit-turn-queue "queue"))
        for chord = (first (gethash action keymap))
        when chord
          append (list (make-seg :text (format nil " ~(~a~) " (key-chord-label chord))
                                 :tone :key-chip)
                       (make-seg :text (format nil " ~a  " verb) :tone :muted))))

(defun composer-rows (model width height bg)
  "The composer band."
  ;; Tall windows get the box: a borderless filled rectangle over the
  ;; transcript's columns (COMPOSER-BOX-WIDTH from column 2), one pad row
  ;; above and below the text rows, text inset 1 more column, no prompt
  ;; glyph. The box carries one row per visible editor row — the editor grows
  ;; to the pi-mono cap and then scrolls internally. The fill tone is :composer while empty and flips to
  ;; :user-block once the operator types (composer.zig composerContentTone).
  ;; Typed into while a turn runs, Enter steers that turn: the box warms
  ;; (:composer-steer), an amber half-block stands at its left edge in the
  ;; running line's tool amber, and the bottom pad row names the keys
  ;; (STEER-KEY-SEGS) -- the box tells what Enter does before any key is read.
  ;; Short windows get the single-line mode: one full-width filled row.
  (bind ((ta (chat-model-editor model))
         (value (editor-value ta))
         (typed (and value (plusp (length value))))
         (steering (and typed (chat-model-turn-in-flight model)))
         (tone (cond (steering :composer-steer) (typed :user-block) (t :composer)))
         (surface (unless (eq (effective-color-profile) :none)
                    (visual-style-bg (style-for-tone tone))))
         (ta-lines (editor-visible-lines ta))
         (columns (if (shell-footer-p height) (composer-text-width width) (max 1 (- width 2))))
         ;; The placeholder lines while the composer is empty resolve in
         ;; priority order (Zig composerPlaceholderView + the home swap):
         ;; the composer status outranks the home prompt; the widget's own
         ;; Ready placeholder is the floor.
         ;; Expire status even while typed text hides its placeholder.
         ((status-lines under status) (composer-status-lines model columns))
         (hint (and (zerop (length (or value ""))) (not status)
                    (or (and (home-screen-p model) (home-prompt-text model))
                        ;; The floor, the widget's own placeholder, reads
                        ;; muted like every other placeholder line: it is
                        ;; absence of input, not input. Taking it through
                        ;; the hint lane is what gives it a tone at all —
                        ;; a widget line paints in the box's fill style,
                        ;; the composer's own bright foreground. Without this,
                        ;; while a turn runs `Ready' would read as idle.
                        (if (chat-model-turn-in-flight model)
                            "Type to steer"
                            (json-value ta.placeholder :text)))))
         ;; A placeholder bypasses the editor's own wrapping, so it must
         ;; clip to the composer text columns itself or a long notice
         ;; spills past the box on narrow windows.
         (hint (when hint (clip-text-to-width hint columns)))
         ;; The status wraps at the text columns -- a failure is shown whole
         ;; -- and paints muted, or in the danger tone for a failure.
         (hint-tone (if (and status (composer-status-error-p status)) :danger :muted))
         ;; Over an empty composer the status is its lines; under a draft a
         ;; held one stands below the text, in the rows the text leaves.
         (ta-lines (cond ((and status (not under)) status-lines)
                         (hint (cons hint (rest ta-lines)))
                         (t ta-lines)))
         (under (and under (subseq status-lines 0 (min (length status-lines)
                                                        (max 1 (- (composer-max-rows height)
                                                                  (max 1 (editor-height ta))))))))
         (hint (or hint (and status (not under))))
         ;; Ghost text: the top suggestion's remainder, dim, after the
         ;; cursor (the caret sits at the end whenever a ghost exists).
         (ghost (composer-ghost-remainder model)))
    (labels ((row (cols &rest segs)
               ;; The one composer row painter: SEGS laid down left to right
               ;; over the box fill, then the fill out to COLS — the docked
               ;; panel's own row, every tone forced onto the box surface, so a
               ;; run never counts SGR bytes as columns nor drops the fill
               ;; behind the pad.
               (docked-panel-row segs cols surface :col 0 :pad-left 0 :pad-right 0))
             (keyed (cols segs keys &aux (room (- cols (segs-display-width keys))))
               ;; The bottom pad row with KEYS flush right. The docked row forces
               ;; its surface onto every tone, which would take the caps' own
               ;; ground, so the keys paint through RENDER-ROW; a row too narrow
               ;; to keep its own text beside them goes without them.
               (if (>= room (+ 2 (segs-display-width segs)))
                   (concatenate 'string
                                (apply #'row room segs)
                                (render-row nil keys surface (- cols room)))
                   (apply #'row cols segs)))
             (run (line &optional (run-tone tone))
               ;; A typed line, or the placeholder in its own tone, at the text
               ;; inset: the message column in the box, two in the single-line mode.
               (make-seg :text (concatenate 'string
                                            (make-string (if (shell-footer-p height) (composer-text-inset) 2)
                                                         :initial-element #\Space)
                                            line)
                         :tone run-tone)))
      (if (shell-footer-p height)
          (bind ((side (concatenate 'string (blank-frame-row 1 bg)
                                    (if steering
                                        (styled "▐" (style-with (style-for-tone :tool) :bg bg))
                                        (blank-frame-row 1 bg))))
                 (keys (and steering (steer-key-segs model)))
                 (right (blank-frame-row (- width 2 (composer-box-width width)) bg))
                 (visible-count (min (length ta-lines)
                                     (- (composer-visible-rows model) (length under))))
                 ;; Text taller than the box scrolls inside it, and the pad
                 ;; row on each side counts the lines out of view there, in
                 ;; the omission line's words. Without this, a paste taller
                 ;; than the box would show its last visible lines with
                 ;; nothing saying the rest was there.
                 ((above below) (editor-hidden-lines ta)))
            (flet ((pad (count where)
                     (when (plusp count)
                       (list (run (format nil "+~d line~:p ~a" count where) :muted)))))
              (loop for (segs . more)
                      on (append (list (pad above "above"))
                                 (loop for line in (subseq ta-lines 0 visible-count)
                                       for index from 0
                                       collect (cond ((and status (not under)) (list (run line hint-tone)))
                                                     ((and hint (zerop index))
                                                      (list (run line :muted)))
                                                     ;; The dim ghost after the typed text,
                                                     ;; cut to the room the row leaves.
                                                     ((and ghost
                                                           (= index (1- visible-count)))
                                                      (list (run line) (make-seg :text ghost :tone :muted
                                                                                 :dim t)))
                                                     (t (list (run line)))))
                                 ;; a held status under the draft
                                 (mapcar (lambda (line) (list (run line hint-tone))) under)
                                 (list (pad below "below")))
                    collect (concatenate 'string side
                                         (if (and keys (null more))
                                             (keyed (composer-box-width width) segs keys)
                                             (apply #'row (composer-box-width width) segs))
                                         right))))
          (list (row width (run (first ta-lines) (if hint hint-tone tone))))))))

;;; --- the frame guard --------------------------------------------------------
;;; The renderer boundary's check of the display authority's contract
;;; (theme.lisp, DISPLAY-GLYPH): a frame is exactly TERM-HEIGHT rows, every
;;; row paints exactly TERM-WIDTH cells, and the only ESC bytes in it are
;;; the SGR runs and OSC-8 pairs this file writes. The authority makes that
;;; true by construction wherever content passes STYLED; this is the belt
;;; to that brace — the :none composer row that never passes it, a
;;; concatenate that never measured, a lane added next year — and it bounds
;;; any breach to the row it is on: nothing in a frame that passed here can
;;; move, style, or link another row.
;;;
;;; Live, a breach is repaired the way the authority would have painted it
;;; (a foreign ESC or control becomes its Control Picture, an over-wide row
;;; is cut at the last whole cell, a short row is padded with canvas) and
;;; counted in *FRAME-GUARD-REPAIRS*, a live-only debug fact that reaches
;;; the operator once as a system notice. Under *FRAME-GUARD-STRICT* — the
;;; suite posture — a breach signals instead, so every gate that composes a
;;; frame asserts the contract for free.

;;; Live-only; nonzero means a lane reached the renderer around the display
;;; authority or mis-measured a row. Cross-thread: SETF'd, never LET-bound.
(defvar *frame-guard-repairs* 0
  "Rows the frame guard repaired since boot.")

(defvar *frame-guard-strict* nil
  "When true a breach signals FRAME-GUARD-VIOLATION instead of being
repaired: the suite posture (CALL-WITH-HERMETIC-MACHINE-STATE binds it).")

(defvar *frame-guard-noticed* nil
  "Whether this image has already surfaced the one repair notice.")

(define-error frame-guard-violation (error) (index reason row)
  (:report "frame guard: row ~a ~a: ~s" index reason row))

(defun own-sequence-end (row i len &aux (j (1+ i)))
  "The index just past the control sequence this file could have written
at I, an ESC in ROW: an SGR run `ESC [ params m' with digit/semicolon
params (SGR-PREFIX, *SGR-RESET*), or an OSC-8 pair
`ESC ] 8 ; ; uri ESC \\' whose body holds no ESC and no control (OSC8-WRAP
drops them)."
  ;; NIL for anything else — a foreign ESC, which must paint as a cell.
  (when (< j len)
    (case (char row j)
      (#\[
       (let ((k (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\;))) row
                                 :start (1+ j) :end len)))
         (and k (char= (char row k) #\m) (1+ k))))
      (#\]
       (and (< (+ j 3) len)
            (string= "8;;" row :start2 (1+ j) :end2 (+ j 4))
            (let ((st (search (load-time-value (format nil "~c\\" #\Escape))
                              row :start2 (+ j 4)))) ; ST, the OSC-8 terminator
              (and st
                   (not (find-if (lambda (ch)
                                   (or (char= ch #\Escape)
                                       (control-codepoint-p (char-code ch))))
                                 row :start (+ j 4) :end st))
                   (+ st 2))))))))

(defparameter *osc8-close*
  (format nil "~c]8;;~c\\" #\Escape #\Escape)
  "The OSC-8 pair's closing half: an empty target ends the link.")

(defun guard-frame-row (row width bg)
  "=> (values ROW' REASON): ROW itself with REASON NIL when it paints exactly
WIDTH cells through this file's own control sequences alone; otherwise the
repaired row and a keyword naming the first breach — :foreign-escape,
:control, :overflow, :short. One scan; the copy starts only at a breach."
  (let ((len (length row))
        (i 0) (cols 0)
        (out nil) (reason nil) (cut nil) (sgr-open nil) (link-open nil))
    (declare (type fixnum len i cols))
    (flet ((breach (kind)
             (unless reason (setf reason kind))
             (unless out
               (setf out (make-string-output-stream))
               (write-string row out :end i))))
      (loop while (< i len)
            do (let* ((ch (char row i))
                      (end (and (char= ch #\Escape)
                                (own-sequence-end row i len))))
                 (cond
                   (end
                    (if (char= (char row (1+ i)) #\[)
                        ;; An SGR run stays open unless its params are a
                        ;; reset: empty or `0'.
                        (setf sgr-open (and (/= (+ i 2) (1- end))
                                            (mismatch "0" row :start2 (+ i 2) :end2 (1- end))))
                        ;; `ESC ] 8 ; ; ESC \' (7 chars) closes; anything
                        ;; longer carries a target and opens.
                        (setf link-open (> end (+ i 7))))
                    (when out (write-string row out :start i :end end))
                    (setf i end))
                   (t
                    (let ((glyph ch) (w 0))
                      (declare (type (integer 0 2) w))
                      (cond ((or (char= ch #\Escape) (control-codepoint-p (char-code ch)))
                             (breach (if (char= ch #\Escape) :foreign-escape :control))
                             (setf glyph (display-glyph ch) w 1))
                            (t (setf w (char-display-width ch))))
                      (when (> (+ cols w) width)
                        (breach :overflow)
                        (setf cut t)
                        (return))
                      (when out (write-char glyph out))
                      (incf cols w)
                      (incf i))))))
      (when cut
        ;; Close what the cut left open, innermost first, so the rest of
        ;; the frame keeps its own style and link state.
        (when sgr-open (write-string *sgr-reset* out))
        (when link-open (write-string *osc8-close* out)))
      (when (< cols width)
        (breach :short)
        (write-string (blank-frame-row (- width cols) bg) out))
      (values (if out (get-output-stream-string out) row) reason))))

(defun guard-frame-rows (rows width height bg)
  "ROWS, the composed frame, checked against the display authority's
contract and repaired (live) or signalled (strict) per breach."
  ;; Always exactly HEIGHT rows of WIDTH painted cells.
  (unless (= (length rows) height)
    (frame-guard-breach nil :height
                        (format nil "~d rows for a ~d-row window" (length rows) height))
    (setf rows (subseq (append rows (make-list height :initial-element (blank-frame-row width bg)))
                       0 height)))
  (loop for tail on rows
        for index from 0
        do (multiple-value-bind (row reason)
               (guard-frame-row (car tail) width bg)
             (when reason
               (frame-guard-breach index reason (car tail))
               (setf (car tail) row))))
  rows)

(defun frame-guard-breach (index reason row)
  "Record one breach: signal under the strict posture, otherwise count it
and surface the one-per-image notice."
  (when *frame-guard-strict*
    (error 'frame-guard-violation :index index :reason reason :row row))
  (incf *frame-guard-repairs*)
  (unless *frame-guard-noticed*
    (setf *frame-guard-noticed* t)
    (surface-system-notice
     (format nil "The frame guard repaired a row (~(~a~)): content reached ~
                  the renderer around the display authority. The frame ~
                  stays aligned; the row may read short or cut."
             reason)
     :error-p nil)))

(defun compose-frame (model)
  "The full frame as a list of exactly TERM-HEIGHT rows (the Zig ChatShell
stack): blank, header, top padding, the transcript band, the gap, the
composer, and — on windows of 12+ rows — the composer/footer gap, the
footer, and the final blank; short windows end with two blank rows."
  (let* ((width (max 8 (chat-model-term-width model)))
         (height (max 4 (chat-model-term-height model)))
         (bg (canvas-fill-bg))
         (text-width (transcript-text-width width))
         (blank (blank-frame-row width bg))
         ;; The first row the toasts may not reach: the composer's gap, or
         ;; the rows under a docked panel.
         (toast-floor 0)
         ;; The header: title, Developer-only dimmed session id, usage lane.
         ;; Home and setup yield the identity; Standard has no raw-id fallback.
         (status (header-status-text model))
         (status-width (if status (text-display-width status) 0))
         (fits (and status (<= (+ 3 4 2 status-width 3) width)))
         (header-row
           (if (or (home-screen-p model) (chat-model-setup-walk model))
               (bilateral-frame-row "" :session-title (and fits status) width bg
                                    nil (and fits (header-status-segs model)))
               (let* ((developer-p (eq :developer (chat-model-tool-detail model)))
                      (session-id (format nil "~a" (transcript-shown-session-id model)))
                      (title (session-display-title model :fallback (if developer-p session-id "")))
                      (identity (if (or (not developer-p) (equal title session-id))
                                    title
                                    (format nil "~a · ~a" title session-id)))
                      ;; A column budget, not a character count: a wide glyph in
                      ;; the title costs two.
                      (max-cols (max 4 (- width 6 (if fits (+ 2 status-width) 0))))
                      (shown (clip-text-to-width identity max-cols))
                      (separator (and developer-p (not (equal title session-id))
                                      (search " · " shown)))
                      (left-segs (when separator
                                   (list (make-seg :text (subseq shown 0 separator)
                                                   :tone :session-title)
                                         (make-seg :text " · " :tone :muted :dim t)
                                         (make-seg :text (subseq shown (+ separator 3))
                                                   :tone :session-id)))))
                 (bilateral-frame-row shown :session-title (and fits status)
                                      width bg left-segs
                                      (and fits (header-status-segs model)))))))
    ;; Settle geometry before painting: branches that reset or fill the
    ;; composer outside the shared update tail (gateway submit, slash
    ;; dispatch) change its height without passing through a key message,
    ;; and the frame must stack to exactly TERM-HEIGHT rows. The editor's
    ;; height is computed from its content; the viewport is what settles.
    (sync-viewport-geometry model)
    ;; Settle paint the same way, and for the same reason: every band below
    ;; recomposes from live tones on this pass, but the transcript band reads
    ;; finished SGR strings out of the viewport, which only RENDER-STATE
    ;; refreshes. /themes moves *PALETTE* outside the render path — the
    ;; picker's live preview on every highlight, its Enter, its Esc restore —
    ;; so without this the whole shell re-themes except the one surface the
    ;; operator is reading. The guard is an EQ probe per frame; it re-projects
    ;; only when a paint input actually moved.
    ;;
    ;; The home screen is a compose input no projection carries: a dialog
    ;; closing or a notice going leaves the rows as they were, and without this
    ;; Esc off the /generations sheet would leave the band a frame under it
    ;; blanked. It moves only while the transcript is empty, so the projection
    ;; it costs is of nothing.
    (unless (and (projection-cache-live-p model)
                 (eq (home-screen-p model) (chat-model-home-painted model)))
      (render-state model))
    (let ((rows
           (guard-frame-rows
            (bind ((head (list blank header-row blank))
           (band (transcript-band-rows model width text-width bg))
           (panel (dialog-panel-rows model width bg))
           (foot (and (shell-footer-p height)
                      (list blank (footer-frame-row model width bg) blank))))
       (if panel
           ;; The frame head — blank, header, chat top padding — is all the
           ;; panel cannot reach; everything under it down to the shell
           ;; footer is its slot. The
           ;; budget is read back from the rows actually stacked rather than
           ;; from a closed form, so a surface another slice adds above the
           ;; composer keeps its rows.
           (multiple-value-bind (panel-height above below)
               (docked-dialog-frame-rows model)
             ;; The panel replaces the composer, so the composer caret is gone;
             ;; a panel that owns a text field takes the cursor with it (Zig
             ;; ui.zig: cursor_vis off, then the dialog's own focus puts it
             ;; back), and a panel that owns none leaves it hidden.
             (let* ((dialog (chat-model-dialog model))
                    (inner (- panel-height 2))
                    (header (min (dialog-header-row-count dialog) inner)))
               ;; Just past TEXT printed at COL inside the padded rect, the text
               ;; clipped to its room — DOCKED-PANEL-ROW's decomposition, whose
               ;; pads a frame at least 8 columns wide always holds whole.
               (flet ((cell (row col text &aux (inner (- width *docked-panel-pad-left*
                                                          *docked-panel-pad-right*)))
                        (when (< col inner)
                          (cons (+ *docked-panel-pad-left* col
                                   (min (text-display-width text) (- inner col)))
                                row))))
                 (setf (chat-model-parked-cursor model)
                       (if dialog.searchable
                           (when (> header *docked-panel-search-row*)
                             (cell (+ above 1 *docked-panel-search-row*)
                                   *docked-panel-content-col*
                                   dialog.search))
                           (multiple-value-bind (label value) (dialog-input-field dialog)
                             (when (and label
                                        (> (- inner header (dialog-footer-row-count dialog)) 0))
                               (cell (+ above 1 header)
                                     (+ *docked-panel-content-col*
                                        (text-display-width label)
                                        *dialog-input-field-gap*)
                                     value)))))))
             (let* ((tail (or foot (list blank)))
                    (tail (last tail (min below (length tail)))))
               (setf toast-floor (- height (length tail)))
               ;; Above the panel: the blank, the header, then the chat top
               ;; padding, the band, and the canvas separator row that keeps
               ;; the panel surface off the transcript's last line.
               (bind (((pad gap want) (docked-dialog-above-bands above))
                      (kept (min want (length band))))
                 (append (subseq head 0 (min 2 (length head)))
                         ;; The pad row, then — a band shorter than its slot pads
                         ;; above, so the transcript stays bottom-anchored against
                         ;; the gap — the band's slack.
                         (make-list (+ pad (- want kept)) :initial-element blank)
                         (last band kept)
                         (make-list gap :initial-element blank)
                         panel
                         tail))))
           ;; The composer dropdown docks BELOW the composer (the Zig
           ;; composer lift), so the band has already yielded its rows
           ;; through SYNC-VIEWPORT-GEOMETRY.
           (let ((dropdown (composer-dropdown-panel-rows model width bg)))
             ;; The composer paints, so its caret cell is where the hardware
             ;; cursor parks.
             (setf (chat-model-parked-cursor model)
                   (multiple-value-bind (x y)
                       (editor-cursor-screen-position (chat-model-editor model))
                     (when (and x (< y (composer-visible-rows model)))
                       (let* ((tall (shell-footer-p height))
                              ;; blank + header + blank, the band, the gap blank, and
                              ;; the box's top pad row on tall windows.
                              (row (+ 3 (length band) 1 (if tall 1 0) y))
                              ;; Tall windows inset the side gutter (2) plus the box's own
                              ;; text inset; the short single-line mode only its inset (2).
                              (col (+ (if tall (+ 2 (composer-text-inset)) 2) x)))
                         (when (< row height)
                           (cons (min col (1- width)) row))))))
             (setf toast-floor (+ (length head) (length band)))
             ;; The live block is the band's own tail, so the scroll gutter
             ;; ends one canvas row over the composer.
             (append head
                     band
                     (list blank)
                     ;; Under an open sheet the composer takes no keys, and its
                     ;; box would show round the window's lower edge: its rows
                     ;; stand blank.
                     (let ((composer (composer-rows model width height bg)))
                       (if (sheet-dialog-p (chat-model-dialog model))
                           (make-list (length composer) :initial-element blank)
                           composer))
                     dropdown
                     (or foot (list blank blank))))))
            width height bg)))
      ;; An open sheet stands over this frame: its window is painted for it
      ;; here, the overlay lays it, and the cursor parks where it says.
      (settle-sheet model width height)
      ;; The frame is the selection's region, so it is stamped here — once,
      ;; for every row, whatever painted it: the transcript band, the turn
      ;; bar, the composer, a docked panel, the footer. The plain lane the copy
      ;; and the word gesture read is derived from these guarded rows, and the
      ;; wash (SELECTION-OVERLAY) marks the cells the painter lays them out
      ;; into, so what the wash covers and what a copy carries are the same
      ;; text.
      (setf (frame-lanes model)
            (make-frame-lane (coerce rows 'vector)
                             (frame-width model) (frame-height model))
            ;; The toasts over these rows, which the view's overlay paints
            ;; and the pointer's hit test reads.
            (chat-model-toast-layout model) (place-toasts model width toast-floor))
      rows)))

;;; --- view method --------------------------------------------------------

(defun image-payload-base64 (payload)
  "The base64 PNG PAYLOAD stands for: its own body when PAYLOAD is an
inline image payload (a bare base64 string or a data:image/png URL), and
otherwise the bytes of the PNG file at that path."
  ;; NIL when it is neither, which is what marks a payload failed so nothing
  ;; retries it.
  (or (inline-image-base64 payload)
      (let ((octets (and (stringp payload) (plusp (length payload))
                         ;; Ceiling on a PNG file read for an inline preview: the
                         ;; whole of it goes to the terminal as base64, once per
                         ;; shell, so a file past 8 MiB simply shows no preview.
                         (nlt:file-octets payload (* 8 1024 1024)))))
        (when (equal "image/png" (nlt:image-media-type octets))
          (cl-base64:usb8-array-to-base64-string octets)))))

(defun ensure-kitty-image (model key payload &aux (cache (chat-model-kitty-image-cache model))
                                                  (cached (gethash key cache)))
  "The transmitted NLT:TERMINAL-IMAGE for PAYLOAD under KEY, transmitting
once through the program terminal and caching (the Zig AssetCache): NIL
when the payload names no PNG this can carry, transmission failed before,
the cache is full, or no terminal stream exists yet."
  ;; KEY is the call the image belongs to, so a second look at a file that
  ;; changed under it is a second transmission rather than the first one's
  ;; picture.
  (cond
    ((nlt:terminal-image-p cached) cached)
    ((or (eq cached :failed) (>= (hash-table-count cache) 16)) nil)   ; Zig max_cached_images
    (t
     (let* ((base64 (image-payload-base64 payload))
            (program (chat-model-program model))
            (stream (and program program.tty-stream)))
       ;; No stream yet is no verdict: the next frame asks again.
       (when (or stream (null base64))
         (let ((image (and base64 (ignore-errors (nlt:transmit-image base64 :stream stream)))))
           (setf (gethash key cache) (or image :failed))
           image))))))

(defun image-band-at (refs plains index &aux (ref (aref refs index)) (start index))
  "The image fact whose inline band opens on lane row INDEX, or NIL: a band
of the row that owns that lane row, as its last render noted them
(ROW-BODY-LLINES)."
  ;; A block paints as [pad] body [pad], every row of it under the same ref,
  ;; and a member card of an open group as its body alone: a blank first row is
  ;; the block's own padding, which no offset counts. Read off the whole
  ;; projection rather than the visible window: a row scrolled halfway out of
  ;; the band must answer where its image STARTS.
  (loop while (and (plusp start) (eq ref (aref refs (1- start))))
        do (decf start))
  (when (zerop (length (string-trim '(#\Space) (aref plains start))))
    (incf start))
  (cdr (assoc (- index start) (row-image-bands ref))))

(defun kitty-image-placements (model)
  "Declarative kitty placements for the pictures visible in the transcript
band, an image card's and an answer's alike: each anchors its transmitted
image at the first row of its own reserved band (IMAGE-BAND-AT), at the body
column."
  ;; Placement identity (image, id, x, y, rows) is deterministic, so unchanged
  ;; frames emit no kitty traffic — the renderer's placement-diff contract. A
  ;; band that begins above the viewport anchors nothing: the protocol
  ;; places a whole image at a cell, and showing part of one is a crop this
  ;; does not ask for. An image draws over the cells, a sheet's window
  ;; too, so none is placed while one is open.
  (when (and (chat-model-kitty-graphics model)
             (not (sheet-dialog-p (chat-model-dialog model))))
    (bind (((vp-height _ _ top-pad) (transcript-band-metrics model))
           (window (chat-model-band model))
           (refs (chat-model-line-rows model))
           (plains (chat-model-line-plain model))
           (width (projection-width model))
           ;; The band's two-column inset plus the body column, so the
           ;; picture starts where the text naming it does.
           (x (+ 2 (nth-value 2 (body-gutter :image-slot))))
           (counts (make-hash-table :test #'eql)))
      (loop for row from 0 below (min vp-height (length refs) (length plains))
            for index = (band-window-row-index window row)
            for ref = (and index (< index (min (length refs) (length plains))) (aref refs index))
            for fact = (and ref (transcript-row-p ref) (row-image-bands ref)
                            (image-band-at refs plains index))
            for payload = (and fact (image-preview-payload fact ref))
            ;; A card's own call owns the transmission: two
            ;; looks at one path are two images. An answer's
            ;; picture is a file named by its bytes.
            for image = (and payload
                             (ensure-kitty-image
                              model
                              (or ref.tool-call-id payload)
                              payload))
            for top = (+ 3 top-pad row)
            for rows = (and image (image-preview-rows fact width))
            ;; An image draws over the cells, so one sharing a row
            ;; with a toast would cover it: it waits for the toast
            ;; to go. A placement says no width, so a shared row is
            ;; the whole test.
            when (and image (not (toasts-cover-rows-p model top rows)))
              collect (nlt:make-image-placement
                       image
                       :x x
                       :y top
                       :rows rows
                       :placement-id
                       (incf (gethash image.id counts 0)))))))

(defun shell-window-title ()
  "The name this shell's window wears: the profile, then the workspace it
is standing in — `default · ~/src/thing'."
  ;; The same two facts the footer's right lane leads with, composed once more
  ;; for the one surface an operator reads without looking inside the frame.
  ;; Control characters are dropped: a title rides an escape sequence and a
  ;; stray one would end it early.
  (let* ((parts (remove nil (list (nlk:profile-label) (workspace-footer-label))))
         (text (format nil "~{~a~^ · ~}" (remove "" parts :test #'string=))))
    (when (plusp (length text))
      (remove-if (lambda (character) (< (char-code character) 32)) text))))

;;; --- toasts -----------------------------------------------------------------
;;; What the system says without being asked — an organism notice, the board
;;; handed to a shell as it attaches, this shell's own caught errors, the
;;; build and update lines — floats top right over the frame for a while: the
;;; web page's toast() (cells/web/page/app.js) in cells. News leaves after
;;; five seconds and waits while the pointer rests on it; an error, or a line
;;; naming something to do, stays until it is closed; the same words again
;;; count up (×N) on the one on screen and bring it back to the top; a keyed
;;; notice replaces the toast under its key, and toasts once in this shell
;;; for what it says; past three, the oldest news leaves first, then the
;;; oldest error. Live-only: never a transcript row and never replayed —
;;; the board (/cells) and NLK:NOTICE-LOG keep what was said. The frame's
;;; overlay paints them over the laid-out cells (NLT:PAINT-ROWS), so the
;;; transcript under a toast keeps its rows, its copy lane and its hit test.
;;; No line borders: a toast is its ground, two columns of it either side of
;;; the words, and a half-row edge of it above and below.

(nlk:define-section ("tui")
  (:guide "Terminal display preferences.")
  ("notification_popups" :boolean :default :false
                         :doc "Show notification popups in the top-right corner. Off by default; notices remain available in /cells."))

(defun notification-popups-p (&aux (section (nlk:find-section '("tui"))))
  "Whether this shell shows notification popups; the declared default is Off."
  (eq t (getf (nlk:section-settings section (nlk:section-table (read-shared-config) '("tui")))
              :notification-popups)))

(defparameter *toast-news-ms* 5000
  "How long a toast that does not stay is up, the web page's five seconds.")

(defparameter *toasts-kept* 3
  "How many toasts are up at once; a narrow window shows the newest two.")

(defparameter *toast-width* 44
  "Columns a toast takes on a window 60 or more wide.")

(defparameter *toast-text-lines* 4
  "Lines of words a toast shows; more are cut with an ellipsis.")

(defstruct (toast (:constructor make-toast (text &key error-p (stays error-p) key foot)))
  "One notice up on the frame."
  ;; STAYS: only a close takes it down. FOOT: words under TEXT that the cut
  ;; at *TOAST-TEXT-LINES* never takes — where the rest is — or NIL. BORN:
  ;; when its five seconds began — a repeat and the pointer leaving it start
  ;; them again. HELD: how many milliseconds of them had run when the pointer
  ;; came onto it, NIL while the pointer is elsewhere: its countdown stands
  ;; still there.
  text error-p stays key foot (count 1) (born (get-internal-real-time)) held)

(defstruct (placed-toast (:constructor place-toast (toast x y width rows margin strip)))
  "Where the last frame painted one toast: its box's first column X, its
first row Y, the box's WIDTH, and the ROWS painted there, each led by the
blank column left of the box when it has a MARGIN, with the countdown STRIP
those rows drew (TOAST-STRIP)."
  toast x y width rows margin strip)

(nlk:access (place placed-toast))

(defun toast (model text &key error-p stays key foot)
  "Put TEXT up as a toast on MODEL's frame: an error, or one that STAYS, until
it is closed, anything else for five seconds. KEY names a standing notice:
its toast replaces the one up under KEY. FOOT is said under TEXT, whole
however much of TEXT the toast cuts. => whether the toasts moved."
  (let ((text (string-right-trim '(#\Space #\Tab #\Return #\Newline) (or text "")))
        (seen (chat-model-toasts-seen model)))
    ;; A keyed notice toasts once for what it says: the board is handed to
    ;; every shell that attaches, and a reconnect is not news.
    (when (and (plusp (length text))
               (not (and key (equal text (gethash key seen)))))
      (when key (setf (gethash key seen) text))
      (let* ((toasts (chat-model-toasts model))
             (same (find text toasts :key #'toast-text :test #'string=)))
        (cond (same
               (incf (toast-count same))
               (setf (toast-born same) (get-internal-real-time)
                     toasts (cons same (remove same toasts)))
               ;; Under the pointer its countdown stands still, full again.
               (when (toast-held same) (setf (toast-held same) 0)))
              (t
               (setf toasts (cons (make-toast text :error-p error-p :stays (or stays error-p)
                                              :key key :foot foot)
                                  (if key (remove key toasts :key #'toast-key :test #'equal) toasts)))
               ;; Past the room the oldest news leaves first, then the oldest
               ;; that stays; the new one never.
               (loop while (> (length toasts) *toasts-kept*)
                     do (setf toasts (remove (or (find-if-not #'toast-stays (rest toasts) :from-end t)
                                                 (car (last toasts)))
                                             toasts)))))
        (setf (chat-model-toasts model) toasts)
        t))))

(defun close-toast (model toast)
  "Take TOAST down. => whether it was up."
  (when (member toast (chat-model-toasts model))
    (setf (chat-model-toasts model) (remove toast (chat-model-toasts model)))
    (when (eq toast (chat-model-toast-hover model))
      (setf (chat-model-toast-hover model) nil))
    t))

(defun expire-toasts (model &aux (now (get-internal-real-time)))
  "Take down the news whose five seconds ran out. => whether any left."
  (flet ((expired-p (toast)
           (and (not (toast-stays toast))
                (not (eq toast (chat-model-toast-hover model)))
                (>= (since-ms (toast-born toast) now) *toast-news-ms*))))
    (when (some #'expired-p (chat-model-toasts model))
      (setf (chat-model-toasts model) (remove-if #'expired-p (chat-model-toasts model)))
      t)))

(defun hover-toast (model toast &aux (was (chat-model-toast-hover model)))
  "Rest the pointer on TOAST, NIL for none. => whether that moved."
  ;; The toast the pointer leaves starts its five seconds again, as the web
  ;; page's does on pointerleave; the one it comes onto holds its countdown
  ;; where it stands.
  (unless (eq toast was)
    (when was (setf (toast-born was) (get-internal-real-time)
                    (toast-held was) nil))
    (when toast (setf (toast-held toast) (since-ms (toast-born toast))))
    (setf (chat-model-toast-hover model) toast)
    t))

(defparameter *toast-strip-steps* 8
  "Steps a countdown strip takes across one column: its last cell fades from
the strip's shade to the ground in this many.")

(defun toast-strip (toast width now)
  "Columns of TOAST's WIDTH-wide bottom edge its countdown spans at NOW, to
a *TOAST-STRIP-STEPS*th of one: all of them as its five seconds begin, none
as they end, standing still while the pointer holds it; NIL for a toast
that stays."
  ;; A cell cannot be cut: the strip's tip would need three colours in one —
  ;; the strip, the ground, and the cell behind the edge. The part of a
  ;; column left is its last cell's shade instead (TOAST-ROWS), so the strip
  ;; slides rather than stepping a whole column nine times a second.
  (unless (toast-stays toast)
    (let ((ran (or (toast-held toast) (since-ms (toast-born toast) now))))
      (max 0 (min width (/ (ceiling (* width *toast-strip-steps* (- *toast-news-ms* ran))
                                    *toast-news-ms*)
                           *toast-strip-steps*))))))

(defun toast-strips-moved-p (model &aux (now (get-internal-real-time)))
  "Whether a countdown strip the last frame painted has moved a step since."
  (some (lambda (place) (not (eql place.strip (toast-strip place.toast place.width now))))
        (chat-model-toast-layout model)))

(defun toast-text-lines (toast width &aux (count (toast-count toast)))
  "TOAST's words broken to WIDTH columns, at most *TOAST-TEXT-LINES* lines, the
last of its text cut with … when there were more, then its foot whole, and
its ×N when it was said again. => (values LINES TAIL), TAIL to follow the
last line's words."
  (flet ((broken (text)
           (loop for line in (split-lines text) nconc (wrapped-lines line width)))
         (cut (line room)
           (concatenate 'string (take-display-prefix line (1- room)) "…")))
    (let* ((foot (broken (toast-foot toast)))
           (room (max 1 (- *toast-text-lines* (length foot))))
           (lines (broken (toast-text toast)))
           (tail (and (> count 1) (format nil "×~d" count))))
      (setf lines (append (cap-lines lines room width) foot))
      (let ((last-line (car (last lines))))
        (cond ((null tail) (values lines nil))
              ((<= (+ (text-display-width last-line) 1 (length tail)) width)
               (values lines (format nil " ~a" tail)))
              ((< (length lines) *toast-text-lines*)
               (values (append lines (list "")) tail))
              (t
               (setf (car (last lines)) (cut last-line (- width 1 (length tail))))
               (values lines (format nil " ~a" tail))))))))

(defun toast-rows (toast width &key hovered margin strip)
  "TOAST painted WIDTH columns wide: a half-row edge of its ground above (▄)
and below (▀), whose other half shows the cell behind it, and between them
its words with two columns of ground either side and its × on the first
row. The bottom edge's first STRIP columns are its countdown (TOAST-STRIP),
in a shade between the ground and the ×'s, a row of ground under the words.
HOVERED lifts the ground a step and lights the ×; MARGIN leads every row
with one blank canvas cell."
  ;; Under the :none profile there is no ground to draw an edge in, so the
  ;; edges are blank rows, the words sit on blanks, and no strip shows. The
  ;; row of ground above the strip is its own: right under the words it read
  ;; as underlining them. A toast with no strip keeps its three-part shape.
  (let* ((tone (if hovered :toast-hover :toast))
         (ground (unless (eq (effective-color-profile) :none)
                   (visual-style-bg (style-for-tone tone))))
         (lit (if (and ground strip) strip 0))
         (shade (and (plusp lit)
                     (blend-rgb (visual-style-fg (dim-style (style-for-tone tone))) ground 40)))
         (lead (if margin (blank-frame-row 1 (canvas-fill-bg)) ""))
         (text-width (- width 7)))
    (flet ((edge (glyph &optional (lit 0))
             ;; LIT whole cells in the shade, the part of one left as its
             ;; share of the shade over the ground, the rest ground.
             (multiple-value-bind (whole part) (floor lit)
               (concatenate
                'string lead
                (if ground
                    (concatenate
                     'string
                     (styled (make-string whole :initial-element glyph) (vstyle :fg shade))
                     (if (plusp part)
                         (styled (string glyph)
                                 (vstyle :fg (blend-rgb shade ground (round (* 100 part)))))
                         "")
                     (styled (make-string (- width whole (if (plusp part) 1 0)) :initial-element glyph)
                             (vstyle :fg ground)))
                    (make-string width :initial-element #\Space))))))
      (multiple-value-bind (lines tail) (toast-text-lines toast text-width)
        (append
         (list (edge #\▄))
         (loop for (line . more) on lines
               for first = t then nil
               for count = (and (null more) tail)
               collect (let ((used (+ 2 (text-display-width line)
                                      (if count (text-display-width count) 0))))
                         (concatenate
                          'string lead
                          (render-row
                           nil
                           (append (list (make-seg :text "  " :tone tone))
                                   (fragment-segs
                                    (make-lline :text line :spans (notice-url-spans line)
                                                :tone (if (toast-error-p toast) :toast-danger tone))
                                    0 (length line))
                                   (and count (list (make-seg :text count :tone tone :dim t)))
                                   (list (make-seg :text (make-string (max 0 (- width 3 used))
                                                                      :initial-element #\Space)
                                                   :tone tone)
                                         (make-seg :text (if first "×" " ") :tone tone
                                                   :dim (not hovered))
                                         (make-seg :text "  " :tone tone)))
                           ground width))))
         (and ground strip (list (concatenate 'string lead (blank-frame-row width ground))))
         (list (edge #\▀ lit)))))))

(defun place-toasts (model width floor)
  "Where MODEL's toasts paint on a WIDTH-column frame, newest on top from
row 3 down to row FLOOR, which none reaches: *TOAST-WIDTH* columns ending
where the transcript's text does, one blank column short of the scroll rail,
with a blank canvas column to their left, or from the frame's two-column
gutter to that same edge below 60 columns, three at most (two there). => a
list of PLACED-TOAST."
  ;; Visibility only: notice data and direct command answers keep their owners.
  ;; Dropping the layout also releases hit tests, image occlusion and countdown paint.
  (when (and (chat-model-toasts model) (not (notification-popups-p)))
    (hover-toast model nil)
    (return-from place-toasts nil))
  ;; A toast that would reach FLOOR is not placed, nor any older one: the
  ;; stack never covers the composer or the rows under a docked panel. Its
  ;; right edge is the message boxes' own, so the blank column before the
  ;; rail keeps it from reading as one block with the thumb.
  (let* ((wide (>= width 60))
         (right (+ 2 (transcript-text-width width)))
         (box (if wide (min *toast-width* (- right 5)) (- right 2)))
         (x (if wide (- right box) 2))
         (y 3)
         (now (get-internal-real-time)))
    ;; Under 12 columns there is no room for words between the pads and the ×.
    (when (>= box 12)
      (loop for toast in (chat-model-toasts model)
            for shown below (if wide *toasts-kept* 2)
            for strip = (toast-strip toast box now)
            for rows = (toast-rows toast box :hovered (eq toast (chat-model-toast-hover model))
                                             :margin wide :strip strip)
            while (<= (+ y (length rows)) floor)
            collect (place-toast toast x y box rows wide strip)
            do (incf y (length rows))))))

(defun toast-at (model x y &aux (column (1- x)) (row (1- y)))
  "The toast the last frame painted under mouse X, Y (1-based), or NIL, and
:CLOSE as a second value when the cell is its × (or a column either side of
it)."
  (dolist (place (chat-model-toast-layout model))
    (let ((right (+ place.x place.width)))
      (when (and (<= place.x column (1- right))
                 (<= place.y row (+ place.y (length place.rows) -1)))
        (return (values place.toast
                        (and (= row (1+ place.y)) (<= (- right 4) column (- right 2)) :close)))))))

(defun toasts-cover-rows-p (model top count)
  "Whether a toast the last frame placed covers any of the COUNT rows from
row TOP."
  (some (lambda (place)
          (and (< place.y (+ top (or count 1)))
               (< top (+ place.y (length place.rows)))))
        (chat-model-toast-layout model)))

(defun toasts-overlay (model &aux (layout (chat-model-toast-layout model)))
  "The toasts COMPOSE-FRAME placed, as a paint over the frame's cells, or NIL
while none is up."
  (when layout
    (lambda (buffer)
      (dolist (place layout)
        (nlt:paint-rows buffer place.rows
                        :x (if place.margin (1- place.x) place.x)
                        :y place.y)))))

(defun tooltip-rows (texts width &aux (bg (visual-style-bg (style-for-tone :toast))))
  "TEXTS painted as a WIDTH-column tooltip on the toast ground: each wrapped
inside two-column margins, between a blank row above and one below."
  (append (list (blank-frame-row width bg))
          (loop for text in texts
                nconc (loop for line in (wrapped-lines text (max 1 (- width 4)))
                            collect (render-row nil (list (make-seg :text (format nil "  ~a  " line)
                                                                    :tone :foreground))
                                                bg width)))
          (list (blank-frame-row width bg))))

(defun status-hint-overlay (model)
  "Keyboard affordances over the active status verb, never inline."
  (when (and (chat-model-turn-in-flight model)
             (not (chat-model-dialog model))
             (not (chat-model-selection-anchor model))
             (eq :status-hint (chat-model-hover-target model)))
    (let* ((rows (frame-lane-rows (frame-lanes model)))
           (index (position-if (lambda (row)
                                 (find :status-hint (plain-row-links row) :key #'third))
                               rows))
           (hint (live-status-hint model)))
      (when (and index (plusp (length hint)))
        (let* ((row (aref rows index))
               (range (find :status-hint (plain-row-links row) :key #'third))
               (column (text-display-width (subseq (plain-frame-row row) 0 (first range))))
               (width (min (frame-width model) (+ 4 (text-display-width hint))))
               (tooltip (tooltip-rows (list hint) width))
               (x (max 0 (min column (- (frame-width model) width))))
               (y (max 0 (- index (length tooltip)))))
          (lambda (buffer) (nlt:paint-rows buffer tooltip :x x :y y)))))))

;;; --- the ember ---------------------------------------------------------------
;;; A running call's listed line burns amber (RUN-TAIL-LLINE): in grey it
;;; would read as a command that had stopped. The ember keeps
;;; it reading alive, and it says what the command does rather than sweeping
;;; on a loop: each line its exec writes flares the words white-hot -- toward
;;; the title's ink, the theme's strongest, so a light theme flares dark --
;;; cooling over +EMBER-FLARE-MS+, and while it is quiet it
;;; breathes on +EMBER-BREATH-MS+ -- a command that prints once a second
;;; beats once a second, and a silent one never reads as frozen. Heat is
;;; paint, never projection: the frame's overlay warms the cells the band laid
;;; out, so a tick that moves only the heat re-projects nothing.

;;; The fades' paint: where their rows stand on screen, and their light.

(nlk:define-side-table fade-rows (:weakness :key)
  "A model -> the rows its last frame showed in a place still fading,
(Y X0 X1 FADE ITEM) each: the screen row, the cells from X0 below X1 its
words span, the place and what of it the row paints.")

(nlk:define-side-table fade-painted (:weakness :key)
  "A model -> T once its last frame's fade overlay painted every fading row
all the way up.")

(defun fade-moved-p (model &aux (now (get-internal-real-time)))
  "Whether a fading row on screen is still moving, or stood still since the
last frame painted it lower: what a tick that moved nothing else repaints
for."
  (and (fade-rows model)
       (or (notevery (lambda (row) (fade-settled-p (fourth row) now)) (fade-rows model))
           (not (fade-painted model)))))

(defun fade-due-p (model &aux (now (get-internal-real-time)))
  "Whether a row on screen is one whose place has gone over to what holds it:
the thing that left is out, and a render puts the newcomer where it stood."
  (some (lambda (row &aux (fade (fourth row)))
          (and (not (equal (fifth row) (fade-on fade now)))
               (equal (fifth row) fade.leaving)))
        (fade-rows model)))

(defun sgr-ink-rgb (sgr)
  "The rgb a cell's foreground SGR text names -- `38;2;R;G;B', `38;5;N', or a
basic 30-37/90-97 -- or NIL for the terminal's default."
  (if-let (code (and sgr (not (find #\; sgr)) (parse-integer sgr :junk-allowed t)))
    (nth (- code (if (< code 90) 30 82)) *ansi16-palette*)
    (sgr-rgb sgr)))

(defun fade-overlay (model &aux (rows (fade-rows model)) (profile (effective-color-profile)))
  "The fading rows' light as a paint over the frame's cells, or NIL while
none shows: a cell's ink blended toward the ground under it, the canvas the
terminal shows (CANVAS-RGB) under a cell with none; over a canvas the
terminal has not said, such a cell stands as painted. A place whose tokens
come up one by one lights each token on its own, counted in reading order
down its rows."
  (when (and rows (not (eq profile :none)))
    (let* ((now (get-internal-real-time))
           (settled (every (lambda (row) (fade-settled-p (fourth row) now)) rows))
           (ink (visual-style-fg (style-for-tone :foreground)))
           (canvas (canvas-rgb))
           ;; The parser hands every cell of one colour the same string, so a
           ;; frame blends each colour pair at a light once.
           (blends (make-hash-table :test 'equal))
           ;; Each place's token count so far, across its rows.
           (counts (make-hash-table :test 'eq)))
      (setf (fade-painted model) settled)
      (unless settled
        (lambda (buffer &aux (height (nlt:screen-buffer-height buffer)) (width (nlt:screen-buffer-width buffer)))
          (flet ((paint (cell light)
                   (when (< light 100)
                     (let ((key (list (nlt:cell-fg cell) (nlt:cell-bg cell) light)))
                       (when-let (sgr (alexandria:ensure-gethash
                                       key blends
                                       (let ((ground (or (sgr-rgb (second key)) canvas)))
                                         (and ground
                                              (color-sgr (blend-rgb (or (sgr-ink-rgb (first key)) ink) ground light)
                                                         t profile)))))
                         (setf (nlt:cell-fg cell) sgr))))))
            (loop for (y x0 x1 fade item) in rows
                  when (< y height)
                    do (if (and (plusp (fade-step fade)) (equal item (fade-on fade now))
                                (not (equal item (fade-leaving fade))))
                           ;; A token starts on a glyph after a blank; the
                           ;; second cell of a wide glyph is neither.
                           (loop with token = (gethash fade counts -1)
                                 with blank = t
                                 for x from x0 below (min x1 width)
                                 for cell = (nlt:screen-buffer-ref buffer x y)
                                 for space = (and (char= #\Space (nlt:cell-char cell)) (plusp (nlt:cell-width cell)))
                                 do (when (and blank (not space)) (incf token))
                                    (setf blank space)
                                    (unless space (paint cell (fade-light fade item now (max 0 token))))
                                 finally (setf (gethash fade counts) token))
                           (loop with light = (fade-light fade item now)
                                 for x from x0 below (min x1 width)
                                 do (paint (nlt:screen-buffer-ref buffer x y) light))))))))))

(defparameter +ember-flare-ms+ 700
  "Milliseconds a flare takes to cool back to the line's amber.")

(defparameter +ember-breath-ms+ 2400
  "Milliseconds one breath of a quiet running line takes.")

(defparameter +ember-breath-heat+ 3/10
  "How hot a breath runs at its peak, a flare's being 1.")

(nlk:define-side-table ember-stamps (:weakness :key)
  "A running call -> (LINES . AT): the lines its exec had written when this
shell last saw the count move, and the internal time it did.")

(nlk:define-side-table ember-rows (:weakness :key)
  "A model -> the running listed lines its last frame showed, (Y X0 X1 CALL)
each: the screen row, the cells from X0 below X1 its words span, the call.")

(nlk:define-side-table ember-painted (:weakness :key)
  "A model -> the heats its last frame's ember painted.")

(defun ember-heat (call backgrounds &optional (now (get-internal-real-time)))
  "How hot CALL's listed line burns at NOW, in twentieths: 0 its amber, 20
white-hot. A flare when it is first seen and whenever its exec among
BACKGROUNDS writes a line, then a breath while it is quiet."
  (let* ((exec (transcript-background-row call backgrounds))
         (lines (if exec (bg-row-written-lines exec) 0))
         (stamp (ember-stamps call)))
    (unless (eql lines (car stamp))
      (setf stamp (setf (ember-stamps call) (cons lines now))))
    (let ((quiet (since-ms (cdr stamp) now)))
      (round (* 20 (if (< quiet +ember-flare-ms+)
                       (expt (- 1 (/ quiet +ember-flare-ms+)) 2)
                       (* +ember-breath-heat+
                          (/ (- 1 (cos (/ (* 2 pi (- quiet +ember-flare-ms+)) +ember-breath-ms+)))
                             2))))))))

(defun ember-heats (model &optional (now (get-internal-real-time)))
  "The heat of each running line MODEL's last frame showed, at NOW."
  (let ((backgrounds (state-background-rows (visible-transcript-state model))))
    (mapcar (lambda (row) (ember-heat (fourth row) backgrounds now)) (ember-rows model))))

(defun ember-moved-p (model)
  "Whether a running line on screen burns hotter or cooler than the last
frame painted it: what a tick that moved nothing else repaints for."
  (and (ember-rows model) (not (equal (ember-heats model) (ember-painted model)))))

(defun ember-overlay (model &aux (rows (ember-rows model)) (profile (effective-color-profile)))
  "The running lines' heat as a paint over the frame's cells, or NIL while
none shows: a warm cell's foreground is the line's amber blended toward its
white-hot, the title's ink with a quarter of the amber left in it."
  (when (and rows (not (eq profile :none)))
    (let* ((heats (setf (ember-painted model) (ember-heats model)))
           (amber (visual-style-fg (style-for-tone :tool)))
           (core (blend-rgb (visual-style-fg (style-for-tone :title)) amber 75)))
      (lambda (buffer)
        (loop for (y x0 x1) in rows
              for heat in heats
              when (and (plusp heat) (< y (nlt:screen-buffer-height buffer)))
                do (loop with fg = (color-sgr (blend-rgb core amber (* 5 heat)) t profile)
                         for x from x0 below (min x1 (nlt:screen-buffer-width buffer))
                         do (setf (nlt:cell-fg (nlt:screen-buffer-ref buffer x y)) fg)))))))

(nlk:define-side-table glint-rows (:weakness :key)
  "A model -> the chips its last frame showed a glint still crossing,
(Y X0 X1 AT) each: the screen row, the cells from X0 below X1, the time the
chip arrived.")

(defun glint-overlay (model &aux (rows (glint-rows model)))
  "The glints crossing chips that have just arrived as a paint over the
frame's cells, or NIL while none crosses: a band of the chip's ink laid into
its ground, eased in from the left and out past the right -- the home mark's
shine, on what an answer found. Truecolor only: quantized, the band jumps."
  (when (and rows (eq (effective-color-profile) :truecolor))
    (let ((now (get-internal-real-time))
          (glow (blend-rgb (visual-style-fg (style-for-tone :inline-strong))
                           (visual-style-bg (style-for-tone :chip)) 55)))
      (lambda (buffer)
        (loop for (y x0 x1 at) in rows
              for s = (min 1 (/ (since-ms at now) +glint-ms+))
              for half = (max 1.5d0 (* 0.45d0 (- x1 x0)))
              for centre = (+ x0 (- half) (* (/ (- 1 (cos (* pi s))) 2) (+ (- x1 x0) half half)))
              when (< y (nlt:screen-buffer-height buffer))
                do (loop for x from x0 below (min x1 (nlt:screen-buffer-width buffer))
                         for heat = (- 1 (/ (abs (- (+ x 1/2) centre)) half))
                         for cell = (nlt:screen-buffer-ref buffer x y)
                         when (plusp heat)
                           do (setf (nlt:cell-bg cell)
                                    (color-sgr (blend-rgb glow (or (sgr-rgb (nlt:cell-bg cell)) glow)
                                                          (round (* 100 heat)))
                                               nil :truecolor))))))))

(defun frame-overlay (model &aux (fade (fade-overlay model))
                                 (ember (ember-overlay model))
                                 (glint (glint-overlay model))
                                 (hint (status-hint-overlay model))
                                 (toasts (toasts-overlay model))
                                 (wash (selection-overlay model))
                                 (timestamp (message-timestamp-overlay model))
                                 (sheet (sheet-overlay model)))
  "The frame's overlays: the fading places' light, the running lines' ember,
the arriving chips' glints, verb hint, toasts, then the drag selection wash,
the message timestamps, and an open sheet's window over them all."
  (let ((overlays (remove nil (list fade ember glint hint toasts wash timestamp sheet))))
    (when overlays
      (lambda (buffer) (dolist (overlay overlays) (funcall overlay buffer))))))

(defun shell-view (model)
  "The composed frame as the tui view — the named body VIEW delegates to,
so a mutation (or a test) can redefine and faithfully restore the shell."
  ;; COMPOSE-FRAME settles PARKED-CURSOR, so it must run before the slot
  ;; is read into the view-state below. The rows go to the renderer as the
  ;; list they are: no join, no re-split.
  (nlt:make-view (compose-frame model)
                 :alt-screen t
                 ;; Any-event tracking: hover states need pure motion
                 ;; (:all-motion = 1003h+1006h).
                 :mouse-mode :all-motion
                 ;; Kitty keyboard, disambiguate-only (CSI >1u) — the
                 ;; flag the Zig TUI's vaxis init also carries. Legacy
                 ;; encodings cannot express Shift+Enter at all: the
                 ;; terminal sends a bare CR for it, so the default
                 ;; insert-newline chord was unreachable without this.
                 ;; With disambiguation on, modified and functional keys
                 ;; arrive as CSI-u (Shift+Enter = CSI 13;2u, Ctrl+J =
                 ;; CSI 106;5u, Escape = CSI 27u) while unmodified text
                 ;; keys keep arriving as plain UTF-8, so typing —
                 ;; capitals included — is untouched. Terminals without
                 ;; the protocol ignore the sequence and keep the legacy
                 ;; decoding, which now reads Ctrl+J correctly too.
                 :keyboard-enhancements t
                 ;; The hardware cursor parked on the composer caret cell,
                 ;; or hidden (?25l) while no cell owns it — the Zig
                 ;; vaxis cursor_visible contract. Steady block, matching
                 ;; the Zig TUI's cursor.
                 :cursor (when-let (park (chat-model-parked-cursor model))
                           (nlt:make-cursor :x (car park) :y (cdr park)))
                 ;; Inline kitty previews for visible image cards; empty
                 ;; (and therefore inert) when the terminal never answered
                 ;; the graphics query.
                 :images (kitty-image-placements model)
                 ;; The window wears what the footer's right lane says, so
                 ;; two shells are told apart by the window manager, the
                 ;; tab bar and the task switcher — where an operator with
                 ;; two of them open actually looks. Live-only: written to
                 ;; the terminal, never to the store, and given back on
                 ;; the way out.
                 :title (shell-window-title)
                 ;; The toasts and the drag selection's wash, over the cells
                 ;; these rows paint.
                 :overlay (frame-overlay model)))

(defmethod view ((model chat-model))
  (shell-view model))

;;; --- operator warnings as system notices --------------------------------
;;; WARN (and any other write to *ERROR-OUTPUT*) paints the tty. Inside the
;;; alt-screen that lands as raw text in the viewport. Capture those lines
;;; and put them up as toasts, never as inline viewport junk; an answer
;;; posted from another thread (/gateway, /web) takes the same road to the
;;; composer status.

;;; Cross-thread: SETF the global, never LET-bind, so channel workers and the
;;; TUI thread see the same variable.
(defvar *active-tui-program* nil
  "The live program, if any.")

(defvar *system-notice-hook* nil
  "Optional (lambda (text error-p)) for tests; also invoked when a program
is live so the same path is observable without draining the program's mailbox.")

(nlt:defmessage system-notice-msg
    ((text :initarg :text :reader system-notice-msg-text)
     (error-p :initarg :error-p :initform nil :reader system-notice-msg-error-p)
     (stays :initarg :stays :initform nil :reader system-notice-msg-stays)
     (answer :initarg :answer :initform nil :reader system-notice-msg-answer))
  :documentation
  "What this shell says from another thread (LAND-SYSTEM-NOTICE): a toast,
or with ANSWER the composer status — never raw text painted onto the
alt-screen.")

(nlt::define-message-vocabulary system-notice-msg)

(defun strip-warning-prefix (text)
  "Drop the CL WARN printer's 'WARNING:' / 'Style-Warning:' lead-in so the
toast's own chrome, or the composer status's tone, owns the framing."
  ;; The lead-in and the blanks around it go; any other line loses only its
  ;; leading blanks.
  (values (ppcre:regex-replace "(?i)^[ \\t]*(?:(?:style-)?warning:|compiler-note:)?[ \\t]*"
                               (or text "") "")))

;;; A diagnostic raised by that paint folds into the toasts and stops there: a
;;; report must be total, and repainting to announce a failed paint is how a
;;; shell spins instead of showing one.
(defvar *landing-system-notice* nil
  "True while the loop thread is landing a notice in place.")

(defun land-system-notice (model text &key error-p stays answer)
  "Land one thing this shell says on MODEL: TEXT as a toast (TOAST), or with
ANSWER in the composer status (SHOW-STATUS). => whether the frame moved."
  ;; The one writer on this path — the reducer below and a report landed on
  ;; the loop thread itself both come here — and the answer is what keeps a
  ;; repeated answer free: the same text arriving twice is the same fact, and
  ;; a fact that changes nothing must not cost a frame, least of all when the
  ;; frame is what produced it. A repeated toast counts up instead.
  (if answer
      (let ((before (chat-model-composer-status model)))
        (when (and (json-value text :text)
                   (not (and before (equal text (composer-status-text before)))))
          (show-status model text :error-p error-p)
          t))
      (toast model text :error-p error-p :stays stays)))

(defun render-live-chrome (model)
  "Recompose the frame for a change that touched only the live chrome."
  ;; The composer status, the turn bar and the band are the only rows such a
  ;; change can move, so the cached transcript projection stands unless a reducer already
  ;; dirtied it — re-projecting the whole transcript to show one warning is
  ;; both work the frame does not owe and a second chance for the projection
  ;; to fail on the one path that has to survive a failing projection.
  (when (chat-model-band model)
    (if (chat-model-transcript-dirty model)
        (render-state model)
        (render-status-frame model))))

;;; A notice raised on the loop thread itself lands in place: put up as a
;;; toast, the chrome recomposed, and painted — exactly what the
;;; reducer and the loop's own paint do for a posted message, minus the trip
;;; through the mailbox. The loop thread never posts this message to itself.
;;; A diagnostic raised while a batch is being handled — a failure inside
;;; the renderer above all — would otherwise become the next batch: the
;;; report re-enters the fold, the fold fails, the failure reports, and the
;;; mailbox never empties, so the drain's wait (the loop's only rate limit)
;;; never arms and the shell turns the cycle as fast as it reproduces, at a
;;; full core, painting nothing. So the fold there is unconditional and
;;; cannot fail, and the paint is attempted once, dropped on failure, and
;;; never reported: the shell keeps its last good frame and its input, and
;;; the notice appears on the first frame that paints.

(defun surface-system-notice (text &key (error-p t) stays answer)
  "Say TEXT on the live TUI: a toast — an error, or one that STAYS, until it
is closed — or with ANSWER the composer status."
  ;; Never writes to the terminal: a live program gets the notice through its
  ;; own loop — landed in place when this is that loop's thread, posted
  ;; through NLT:SEND (thread-safe) from any other — and with no program the
  ;; text is dropped rather than painted over the alt-screen.
  (let ((text (string-right-trim
               '(#\Space #\Tab #\Return #\Newline)
               (strip-warning-prefix text))))
    (when (plusp (length text))
      (when *system-notice-hook*
        (funcall *system-notice-hook* text error-p))
      (when-let (program *active-tui-program*)
        (if (nlt:on-loop-thread-p program)
            (let ((model program.model))
              (when (and (typep model 'chat-model)
                         (land-system-notice model text :error-p error-p :stays stays
                                                        :answer answer)
                         (not *landing-system-notice*))
                (let ((*landing-system-notice* t))
                  (ignore-errors
                   (render-live-chrome model)
                   (nlt:render program.renderer (nlt:view model))))))
            (nlt:send program (make-system-notice-msg :text text :error-p error-p
                                                      :stays stays :answer answer))))
      text)))

(defclass tui-error-stream (line-output-stream)
  ((lock :initform (bt2:make-lock :name "tui-error-stream")
         :reader tui-error-stream-lock)
   (fallback :initarg :fallback :initform nil
             :accessor tui-error-stream-fallback))
  ;; The OS-level stderr is severed to the debug log while a TUI is live.
  ;; Explicit notices have their own typed path; raw stderr never uses it.
  (:documentation
   "The TUI-era *ERROR-OUTPUT*: characters buffer per line and retain their
bytes in FALLBACK, the debug log, never user notifications."))

(defmethod line-output ((stream tui-error-stream) text)
  ;; Stderr is not a notice protocol. Compiler reports, continuation lines,
  ;; and caught runtime failures retain their bytes in the debug-only sink.
  (let ((fallback (or (tui-error-stream-fallback stream) sb-sys:*stderr*)))
    (write-string text fallback)
    (ignore-errors (force-output fallback))))

;;; Channel workers and the TUI thread write the one stream, so its buffer
;;; moves under the lock; a carriage return ends a line as a newline does.
(defmethod sb-gray:stream-write-char :around ((stream tui-error-stream) char)
  (bt2:with-lock-held ((tui-error-stream-lock stream))
    (prog1 (call-next-method)
      (when (char= char #\Return)
        (setf (line-output-stream-at-line-start stream) t)
        (flush-line-output stream)))))

(defmethod sb-gray:stream-force-output :around ((stream tui-error-stream))
  (bt2:with-lock-held ((tui-error-stream-lock stream))
    (call-next-method)))

(defvar *crash-report-hook* nil
  "The debugger hook CALL-WITH-TUI-ERROR-CAPTURE installed, so a nested
capture wraps the disabled debugger once, never itself.")

(defun call-with-tui-error-capture (program thunk)
  "While THUNK runs, keep process-wide *ERROR-OUTPUT* in the debug-only
fallback stream and route fatal debugger reports there too. PROGRAM owns
explicit notifications, never compiler or raw runtime output."
  ;; Restores both even if THUNK unwinds. Uses SYMBOL-GLOBAL-VALUE so channel
  ;; worker threads (which do not rebind *ERROR-OUTPUT*) follow the redirect.
  (let* ((saved-program *active-tui-program*)
         (saved-error (sb-ext:symbol-global-value '*error-output*))
         (fallback (if (typep saved-error 'tui-error-stream)
                       (or (tui-error-stream-fallback saved-error)
                           sb-sys:*stderr*)
                       saved-error))
         (saved-hook sb-ext:*invoke-debugger-hook*)
         (wrapped (eq saved-hook *crash-report-hook*)))
    ;; Replacing an installed TUI-ERROR-STREAM keeps its fallback: no nesting.
    (setf *active-tui-program* program
          (sb-ext:symbol-global-value '*error-output*)
          (make-instance 'tui-error-stream :fallback fallback))
    (unless wrapped
      ;; Report on FALLBACK, not a toast a quitting process never paints.
      (setf *crash-report-hook*
            (lambda (condition hook &aux (*error-output* fallback))
              (cond
                (saved-hook (funcall saved-hook condition hook))
                (t
                 (ignore-errors
                   (format fallback "~&Unhandled ~s in thread ~s: ~a~%"
                           (type-of condition) sb-thread:*current-thread* condition)
                   (sb-debug:print-backtrace :stream fallback :from :interrupted-frame
                                             :print-thread t :emergency-best-effort t)
                   (finish-output fallback))
                 (sb-ext:exit :code 1 :abort t))))
            sb-ext:*invoke-debugger-hook* *crash-report-hook*))
    (with-cleanup ((ignore-errors
                     (let ((current (sb-ext:symbol-global-value '*error-output*)))
                       (when (typep current 'tui-error-stream)
                         (finish-output current))))
                   (unless wrapped
                     (setf sb-ext:*invoke-debugger-hook* saved-hook
                           *crash-report-hook* nil))
                   (setf *active-tui-program* saved-program
                         (sb-ext:symbol-global-value '*error-output*) saved-error))
      (funcall thunk))))

;;; --- OS-level stdio severance ---------------------------------------------
;;; TUI-ERROR-CAPTURE above owns the Lisp-stream level: *ERROR-OUTPUT* writes
;;; stay in the debug log. The OS level underneath it — the descriptors 0, 1
;;; and 2 severed from the terminal while a TUI paints — is the kernel's
;;; CALL-WITH-SEVERED-STDIO; this shell only names the sidecar log.

(defun stdio-log-path (store-path)
  "The stdio sink, named next to the database whose TUI owns the terminal —
the same adjacency rule as RESURRECTION-LOG-PATH."
  (pathname (concatenate 'string (namestring store-path) ".stdio.log")))

(defmethod update ((model chat-model) (msg system-notice-msg))
  "Land one thing said from another thread — a toast, or an answer — not as
raw text painted onto the viewport, not as a transcript row, and not as a
full re-projection: neither can move a transcript row, so an answer
recomposes over the cached projection, and a toast is the frame's overlay."
  ;; A repeated answer is the same fact and costs no frame.
  (let* ((answer (system-notice-msg-answer msg))
         (moved (land-system-notice model (system-notice-msg-text msg)
                                    :error-p (system-notice-msg-error-p msg)
                                    :stays (system-notice-msg-stays msg)
                                    :answer answer)))
    (when (and moved answer)
      (render-live-chrome model))
    (values model nil (not moved))))

;;; --- entry point --------------------------------------------------------

(defun fresh-session-id ()
  "Return a new unique session identifier."
  ;; A session with no prior event log rows has no history: recall returns
  ;; nothing and fold-log finds nothing. The format matches the session
  ;; struct's own auto-generated id.
  (format nil "s-~36r" (random (expt 36 8) (make-random-state t))))

(defun call-with-profile-switch (program body &aux (performer nlk:*profile-switch-performer*))
  "Run BODY — a whole shell, its teardown included — with the switch
performer installed, then perform whatever switch BODY parked."
  ;; PROGRAM is the loop BODY runs, so a pick can end it. Both shells go
  ;; through here, and the order below is the contract:
  ;;
  ;;   1. the program loop returns, and its own UNWIND-PROTECT has restored
  ;;      every terminal mode, left raw mode and given the window back;
  ;;   2. the shell's teardown has stopped what it started and closed the store;
  ;;   3. the store is closed again here, idempotently, because an exec keeps
  ;;      open file descriptors and a lease still held would be inherited into
  ;;      the new image and deadlock it against itself;
  ;;   4. only then is this process replaced.
  ;;
  ;; Nothing execs from inside the loop or from a picker callback: a switch
  ;; reaches this function as a plan and waits for the terminal. The performer
  ;; that actually execs is the one the kernel installed (launch.lisp), saved
  ;; and called here, so there is one exec call shape and both shells use it.
  ;; A refused exec prints the kernel's words and the shell exits normally,
  ;; rather than leaving a half-torn-down process behind.
  ;; A shell starts with nothing parked, and the plan is consumed the
  ;; moment it is performed: a refusal leaves this image clean rather than
  ;; half-switched.
  (setf nlk:*profile-switch-parked* nil)
  (prog1 (with-cleanup ((setf nlk:*profile-switch-performer* performer))
           (setf nlk:*profile-switch-performer*
                 (lambda (name binary argv)
                   (setf nlk:*profile-switch-parked* (list name binary argv))
                   ;; The operator pressed Enter expecting to be in that
                   ;; home now, and the terminal does not come back at all:
                   ;; the pick sends the same quit /exit sends, from wherever
                   ;; it was made, and RUN leaves the screen — the notice
                   ;; rides out on the frame it already painted — for the
                   ;; image that replaces this one.
                   (setf nlt:*keep-terminal* t)
                   (nlt:send program (nlt:make-quit-msg))
                   (format nil "switching to ~a: this shell comes back in that home" name)))
           (funcall body))
    (let ((pending nlk:*profile-switch-parked*))
      (when (and pending performer)
        ;; Consumed: one plan, one exec. A refused exec leaves an image with
        ;; nothing parked rather than half-switched.
        (setf nlk:*profile-switch-parked* nil)
        (ignore-errors (nlk:close-store))
        ;; The screen stays (the successor paints over it), the input modes do
        ;; not: raw mode is the one mode that must not outlive the process
        ;; that set it — the successor saves whatever it finds as its own
        ;; original and would hand raw settings back to the shell at its exit.
        (ignore-errors (nlt:exit-raw-mode))
        (when-let (refusal (apply performer pending))
          (setf nlt:*keep-terminal* nil)
          (ignore-errors (nlt:release-terminal program))
          (format *error-output* "~&nodecode: ~a~%" refusal)
          (uiop:finish-outputs))))))

;;;; --- the startup owner ---------------------------------------------------
;;;;
;;;; One function owns the boot sequence. Nothing in it needs to fork any
;;;; more (a child execs a fresh process), so the order is the
;;;; plain one: collection policy, the cell folder, then the entry the
;;;; arguments name.

;;; --- executable entry ----------------------------------------------------

(define-refusal launch-usage-error launch-usage-error "~a"
  "A command line the executable refuses; MAIN prints it
with a pointer to `nodecode --help' and exits 2.")

(defun ephemeral-store-path (&optional (pid (sb-posix:getpid)))
  "A store of this process's own under the platform temp directory
(TMPDIR, TEMP, TMP, else /tmp): nodecode-ephemeral-<pid>.db, the name the
Zig launcher gives the same thing."
  (merge-pathnames (format nil "nodecode-ephemeral-~d.db" pid)
                   (uiop:temporary-directory)))

;;; --- the command line: one table, every reader ------------------------------
;;; The launch's words are data (NLK:VERB, waist/verbs.lisp): the parser
;;; resolves a word and its aliases through the two tables below, `nodecode
;;; --help' prints them, and a word they do not hold is refused with the
;;; nearest one they do. `nodecode profiles --help' once answered `unknown
;;; argument' and a hand-kept usage line: the parser had no table to read
;;; back. An internal verb is the organism's own child entry, a trial or the
;;; warm-image supervisor and its builder, spawned and never typed: the
;;; parser takes it, help never lists it, no suggestion offers it.

(defparameter *launch-verbs*
  (list
   (nlk:make-verb "serve" "run this home's organism headless: a gateway and no shell"
                  :arguments "[--store PATH]"
                  :detail "It owns the store and serves its gateway until stopped, or exits at once when another process already owns it. A reconnecting shell spawns one; it is also the way to run an organism with no terminal.")
   (nlk:make-verb "login" "sign in to your nodecode.ai account: a subscription or API credits"
                  :arguments "[--subscription | --api]"
                  :detail "Asks which one unless a flag says it, opens nodecode.ai in your browser with this machine's code in the page, and waits there for you to sign in with your nodecode.ai account and allow the machine; over ssh, open the printed page on any device. The key is saved locally and the account's meter printed: the plan's requests left and when the plan ends, the API credit. No terminal UI or session is opened. /login does the same from inside the shell; /models is for other providers.")
   (nlk:make-verb "machine" "link this machine to your nodecode.ai account with a code from its Machines page"
                  :label "machine link [CODE]"
                  :arguments "link [CODE]"
                  :detail "Bring the code from https://nodecode.ai/account/machines, or omit CODE to get one to type there. Approve the machine on the account page; its key is saved locally. No terminal UI or session is opened. nodecode login signs in with a browser instead.")
   (nlk:make-verb "setup" "open the setup walk on the first frame"
                  :terminal t
                  :detail "Connect a provider, pick a model and its reasoning effort. /setup does the same from inside the shell.")
   (nlk:make-verb "import" "read another coding agent's home into this one; a preview until --yes"
                  :label "import [WORLD]" :terminal t
                  :arguments "[WORLD] [--source PATH] [--only KINDS] [--dry-run] [--overwrite] [--keep-running] [--yes]"
                  :detail "Providers, standing instructions, memory, skills, MCP servers, channels, cron jobs, sessions and profiles, each through the cell that owns it, keys and tokens with them. WORLD names one harness to read; naming none reads every coding-agent home on this box and merges them, newest first, with an existing fact here always winning. --source reads a home at a path instead, or a repository's skills and MCP servers (OWNER/REPO, an https URL, #REF pinning a branch, tag or commit); --only names the kinds, comma-separated; --overwrite replaces what is already here. A foreign gateway polling the same bots is stopped and disabled so they answer from here (the report names the command that gives them back); --keep-running leaves it running, and its bots land off and come on by themselves once it stops. The preview opens in the setup walk; --yes applies it without asking.")
   (nlk:make-verb "add" "install a cell from the hub, or one this release ships, and turn it on"
                  :label "add nc://NAME"
                  :arguments "nc://NAME [--yes]"
                  :detail "NAME is the cell's name, its nodecode- prefix optional: nc://channel-slack. Code from the hub is shown first -- its repository, the commit it is pinned at, what it brings -- and installed on a yes; --yes says it on the line, and with no terminal to ask nothing is installed without it. With the organism running it is turned on there, live in every attached shell; with none running it loads at the next launch.")
   (nlk:make-verb "profile" "list, create, use, delete, export, import and install profiles"
                  :aliases '("profiles")
                  :label "profile [VERB]"
                  :arguments "[VERB] [ARGUMENTS]")
   (nlk:make-verb "gateway" "keep this home's organism running in the background, and at login"
                  :label "gateway [VERB]"
                  :arguments "[VERB]")
   (nlk:make-verb "update" "move this home's organism to the latest release, or to VERSION"
                  :label "update [VERSION]"
                  :arguments "[VERSION] [--check]"
                  :detail "The release is staged beside the running build and classified, definition by definition. When every changed definition can go live it is evaluated into the running organism and every attached shell, with no restart; otherwise the organism restarts itself onto the new build at the next idle moment. There is no watcher: a build that does not serve leaves the next launch pristine, generation 0, and `rollback N' restores a sealed generation. With no organism running, the next launch runs the release. --check stages and classifies without applying. Naming a VERSION holds this home there -- the way back after an update -- until a bare `update' lifts it. The gateway also looks at the release channel by itself: `update' in the config sets auto (the default), ask, or off. Its own look only moves forward, goes live while no turn can begin and once none runs, and undoes a release that leaves a provider it served unresolved.")
   (nlk:make-verb "generations" "list the sealed generations of this home's organism"
                  :detail "A generation is the organism as it stood once a fresh child proved its folder: the binary's version and every loaded folder at its revision, numbered from 1 and never pruned (~/.nodecode/generations/). The list marks the one the folder stands at, and says when it stands at none and why. (generations) inside the shell answers the same.")
   (nlk:make-verb "rollback" "restore this home's organism to generation N"
                  :label "rollback N" :arguments "N"
                  :detail "Every folder goes back to what generation N recorded: the layer takes one commit whose tree is N's (its history kept, so forward is the same verb), a hub cell is cloned at N's pin, the binary and its shipped folders stay on the release that runs -- updates keep coming, forward, over the restored folder -- folders N had are installed and ones it never had are taken out where the installer put them; a plain folder whose content differs refuses the whole restore by name. With the organism running it converges and exits once no turn is in flight, and the next launch runs N; with none running, the next launch does. (rollback N) inside the shell does the same.")
   (nlk:make-verb "web" "open this home's organism in a browser tab"
                  :detail "Prints the page's link and opens it where there is a display. The operator token rides after the link's #, which a browser never sends to a server: keep the link to yourself. Needs the organism running (nodecode, or nodecode gateway on) and the web cell: turn web on in /cells.")
   (nlk:make-verb "resume" "open the shell on that session" :arguments "SESSION-ID" :terminal t)
   (nlk:make-verb "rpc" "drive this home's organism from a program: JSON lines in and out"
                  :label "rpc [SESSION-ID]"
                  :arguments "[SESSION-ID [--cursor N]] [--store PATH | --ephemeral]"
                  :detail "Each line on stdin is one gateway command, the object a client_command frame carries: {\"type\":\"start_turn\",\"request\":{\"prompt\":\"...\"}}, which may also name the provider, model and effort a session not yet begun runs on (the default stays where it is), or steer_turn, cancel_turn, withdraw_prompt, interrupt_exec, navigate_checkpoint, get_state (the session as it stands, in its result), set_session_subscriptions. A command_id is minted when a line names none, and a command naming no session_id goes to SESSION-ID, or to a fresh session. Each line on stdout is one gateway frame as the gateway sent it: the session's replay first, then its facts as they land (session_stream_event; turn.completed, turn.failed or turn.cancelled ends a turn), each command's gateway_turn_command_result, and a sync_protocol_error for a line that is not a command. --cursor N, the newest cursor a program read, resumes: the replay carries only what landed past it and says resumed_after, unless the session's history changed under it (a checkpoint move), when the whole replay comes instead. Every shape both ways is src/src/wire/protocol.schema.json. With no organism running it starts one, which stops when stdin closes; one already running keeps running.")
   (nlk:make-verb "help" "this text, or one command's" :arguments "[COMMAND]")
   (nlk:make-verb "trial" "a fresh process of the folder that answers one form (launch.lisp)" :internal t)
   (nlk:make-verb "warm" "the warm-image supervisor (launch.lisp)" :internal t)
   (nlk:make-verb "warm-dump" "build a warm image at PATH (launch.lisp)"
                  :arguments "PATH" :internal t))
  "The executable's commands, in help's order: what PARSE-LAUNCH-ARGUMENTS
resolves, what `nodecode --help' prints, what a mistyped command is matched
against.")

(defparameter *launch-options*
  (list
   (nlk:make-verb "--profile" "run profile NAME: its own home and its own organism"
                  :aliases '("-p") :arguments "NAME" :label "-p, --profile NAME"
                  :detail "Read anywhere on the line. $NODECODE_HOME names a home the same way; with neither, the profile `nodecode profile use' chose boots, else ~/.nodecode/.")
   (nlk:make-verb "--store" "run on the store at PATH" :arguments "PATH")
   (nlk:make-verb "--ephemeral" "run on a throwaway store of this process's own"
                  :detail "The store only: config, the layer and the knowledge cells are still this profile's. HOME=$(mktemp -d) nodecode is a throwaway organism.")
   (nlk:make-verb "--pristine" "run generation 0: the binary alone, no folder loaded"
                  :detail "Nothing of the cell folder loads, no published image is followed, and keeps are refused while it runs: the floor to stand on when the folder itself is the problem. (generations) lists what was sealed and (rollback N) restores one, the next launch running it. A launch whose last self died before serving stands here by itself.")
   (nlk:make-verb "--help" "this text" :aliases '("-h") :label "-h, --help")
   (nlk:make-verb "--version" "the version, and the commit it was built from"))
  "The executable's options, in help's order.")

(defun launch-name (word &rest tables)
  "The name of the launch command or option WORD spells in TABLES, its
aliases resolved, or NIL."
  (let ((verb (some (lambda (table) (nlk:find-verb word table)) tables))) (and verb verb.name)))

(defun refuse-launch-word (word)
  "Refuse WORD, a command or an option the launch does not hold, naming the
nearest one it does."
  (if (and (plusp (length word)) (char= (char word 0) #\-))
      (launch-usage-error "~a" (nlk:unknown-word-text "option" word *launch-options*))
      (launch-usage-error "~a" (nlk:unknown-word-text "command" word *launch-verbs*))))

(defun import-request (args)
  "The setup walk's import request the words ARGS after `import' ask for --
(:import WORLD), T for every world, plus :source, :only and the T-valued
switches -- and the words past it: `nodecode import' and a shell's /import
read one line alike."
  (let* ((word (first args))
         (named (and word (not (uiop:string-prefix-p "--" word)) (pop args)))
         (request nil))
    (when (and named (not (nlk:find-agent-world named)))
      (launch-usage-error
       "~s is not a world this build reads (~{~a~^, ~}); ~
          name a home at a path with --source: nodecode import ~a"
       named (nlk:agent-world-names)
       (nlk:verb-arguments (nlk:find-verb "import" *launch-verbs*))))
    (setf request (list :import (or named t)))
    (flet ((value-for (refusal)
             (or (pop args) (launch-usage-error refusal))))
      (loop while (and args (member (first args)
                                    '("--source" "--only" "--dry-run"
                                      "--overwrite" "--keep-running" "--yes")
                                    :test #'string=))
            do (let ((key (intern (string-upcase (subseq (pop args) 2)) :keyword)))
                 (setf (getf request key)
                       (case key
                         (:source (value-for "--source needs a PATH"))
                         (:only (value-for "--only needs KINDS, comma-separated"))
                         (t t))))))
    (values request args)))

(defun parse-launch-arguments (args &aux (profile nil))
  "ARGS, the executable's command line, as the launch plan: (values SERVE
CHILD SESSION-ID STORE-PATH EPHEMERAL SETUP PROFILE PRISTINE)."
  ;; SERVE is the headless owner, :WAIT for `gateway run' — the gateway, which
  ;; waits for the store instead of losing it; CHILD what runs before any
  ;; folder loads, (:help TOPIC . MORE), (:version), (:profile WORDS) or
  ;; (:gateway WORDS), or a launch-cache or trial child, (:trial), (:warm) or
  ;; (:warm-dump PATH); SESSION-ID what `resume' named; STORE-PATH what
  ;; `--store' named; EPHEMERAL whether the shell boots a throwaway store of
  ;; its own, which excludes the other three ways of naming one; SETUP whether
  ;; the shell opens the setup wizard on its first frame, T for `setup' and
  ;; the import request plist (:import WORLD ...) for `import'; PROFILE the
  ;; profile `-p NAME' or `--profile NAME' named, anywhere on the line;
  ;; PRISTINE whether `--pristine' asked for generation 0, the binary alone.
  ;;
  ;; Words resolve through *LAUNCH-VERBS* and *LAUNCH-OPTIONS*, aliases
  ;; included. `help [COMMAND]', or --help or -h anywhere ahead of a `profile'
  ;; or a `gateway' (each reads its own), answers (:help TOPIC), TOPIC the
  ;; first command on the line; --version answers (:version); both outrank the
  ;; rest of the line. A refusal is a LAUNCH-USAGE-ERROR naming what was
  ;; wrong, and the nearest word the tables hold for one they do not.
  ;; `-p NAME' or `--profile NAME' comes out wherever it sits (Hermes's rule).
  (setf args (loop while args
                   for arg = (pop args)
                   if (member arg '("-p" "--profile") :test #'string=)
                     do (let ((value (or (pop args) (launch-usage-error "~a needs a NAME" arg))))
                          (when profile
                            (launch-usage-error
                             "~a names a second profile, ~a; a launch runs one" arg value))
                          (unless (nlk:profile-name-p value)
                            (launch-usage-error "~s is not a profile name: a-z, 0-9, _ and -, ~
                                                   64 at most, starting with a letter or digit"
                                                value))
                          (setf profile value))
                   else collect arg))
  (let ((head (subseq args 0 (position-if (lambda (word)
                                            (member (launch-name word *launch-verbs*)
                                                    '("profile" "gateway" "help")
                                                    :test #'equal))
                                          args))))
    (when-let (child (cond ((some #'nlk:help-flag-p head)
                            (list :help (some (lambda (word) (launch-name word *launch-verbs*))
                                              args)))
                           ((member "--version" head :test #'string=) (list :version))))
      (return-from parse-launch-arguments (values nil child nil nil nil nil profile nil))))
  (let (session-id store-path serve child ephemeral setup pristine)
    (flet ((value-for (refusal)
             ;; The word a flag or a verb takes, or REFUSAL when the line ends first.
             (or (pop args) (launch-usage-error refusal))))
      (loop while args
            for arg = (pop args)
            for name = (launch-name arg *launch-verbs* *launch-options*)
            do (cond
                 ((string= name "serve") (setf serve t))
                 ;; (:login MODE): what the sign-in is for, NIL to ask on the terminal.
                 ((string= name "login")
                  (let ((mode (cond ((equal (first args) "--subscription") (pop args) "subscription")
                                    ((equal (first args) "--api") (pop args) "api"))))
                    (when (and args (not (uiop:string-prefix-p "-" (first args))))
                      (launch-usage-error "login signs in with a browser and takes no CODE; a code from the Machines page is nodecode machine link ~a"
                                          (first args)))
                    (setf child (list :login mode))))
                 ;; (:machine-link CODE): the Machines page's link, NIL to be given a code.
                 ((string= name "machine")
                  (unless (equal (pop args) "link")
                    (launch-usage-error "machine takes one verb: nodecode machine link [CODE]"))
                  (let ((code (and args (not (uiop:string-prefix-p "-" (first args))) (pop args))))
                    (when (and args (not (uiop:string-prefix-p "-" (first args))))
                      (launch-usage-error "machine link takes at most one CODE"))
                    (setf child (list :machine-link code))))
                 ((string= name "setup") (setf setup t))
                 ((string= name "import")
                  (multiple-value-setq (setup args) (import-request args)))
                 ((string= name "trial") (setf child (list :trial)))
                 ((string= name "warm") (setf child (list :warm)))
                 ((string= name "warm-dump")
                  (setf child (list :warm-dump (value-for "warm-dump needs a PATH"))))
                 ((string= name "resume")
                  (setf session-id (value-for "resume needs a SESSION-ID")))
                 ;; (:rpc SESSION-ID CURSOR): the session a program's commands go
                 ;; to when they name none, NIL for a fresh one, and the log
                 ;; position it resumes past. A command word after it is never
                 ;; a session id: `rpc serve' is refused.
                 ((string= name "rpc")
                  (let* ((word (first args))
                         (session (and word (not (uiop:string-prefix-p "-" word))
                                       (not (launch-name word *launch-verbs*))
                                       (pop args)))
                         (cursor (when (equal (first args) "--cursor")
                                   (pop args)
                                   (let ((value (value-for "--cursor needs N, the newest cursor read")))
                                     (or (ignore-errors (parse-integer value))
                                         (launch-usage-error "--cursor takes a log position, not ~s"
                                                             value))))))
                    (when (and cursor (or (null session) (minusp cursor)))
                      (launch-usage-error "--cursor resumes a session past a log position: ~
                                             nodecode rpc SESSION-ID --cursor N"))
                    (setf child (list :rpc session cursor))))
                 ;; `gateway run' is the gateway itself: the headless owner
                 ;; that waits for the store instead of losing it.
                 ((and (string= name "gateway") (equal (first args) "run"))
                  (pop args)
                  (setf serve :wait))
                 ;; The rest of the line is the child's: its flags are its own
                 ;; and the launch parser has no business reading them — the
                 ;; verbs the operating system answers, which run before any
                 ;; folder loads, print, and exit.
                 ((member name '("add" "profile" "gateway" "update" "web" "generations" "rollback")
                          :test #'string=)
                  (setf child (list (intern (string-upcase name) :keyword) args)
                        args nil))
                 ((string= name "help")
                  (let ((word (pop args)))
                    (setf child (list* :help
                                       (and word
                                            (or (launch-name word *launch-verbs*
                                                             *launch-options*)
                                                (refuse-launch-word word)))
                                       args)
                          args nil)))
                 ((string= name "--store")
                  (setf store-path (value-for "--store needs a PATH")))
                 ((string= name "--ephemeral") (setf ephemeral t))
                 ((string= name "--pristine") (setf pristine t))
                 (t (refuse-launch-word arg)))))
    (when (and child (eq (first child) :help))
      (return-from parse-launch-arguments
        (values nil child nil nil nil nil profile nil)))
    (when ephemeral
      (when serve
        (launch-usage-error "--ephemeral is a shell's flag: a headless owner names its store with --store PATH"))
      (when session-id
        (launch-usage-error "--ephemeral cannot resume: a fresh store has no session; ~
                               resume one with --store PATH"))
      (when store-path
        (launch-usage-error "--ephemeral chooses the store; --store PATH names one yourself")))
    (when (and setup serve)
      (launch-usage-error "~a is a shell's verb: a headless owner has no wizard"
                          (if (consp setup) "import" "setup")))
    (when (and child (eq (first child) :rpc) (or serve session-id setup))
      (launch-usage-error "rpc is a command of its own: nodecode [-p NAME] rpc ~a"
                          (nlk:verb-arguments (nlk:find-verb "rpc" *launch-verbs*))))
    (when (and child (member (first child) '(:login :machine-link :add :profile :gateway :update :web :generations :rollback))
               (or serve session-id store-path ephemeral setup))
      (launch-usage-error "~a is a command of its own: nodecode [-p NAME] ~:*~a ..."
                          (substitute #\Space #\- (string-downcase (first child)))))
    (values serve child session-id store-path ephemeral setup profile pristine)))

(defun launch-help-text (&optional topic more)
  "`nodecode --help': the commands and the options, from the tables; or
TOPIC's own help, a command's or an option's."
  ;; The profile command's help is the profile verbs' own
  ;; (NLK:PROFILE-COMMAND), MORE naming one of them.
  (cond
    ((null topic)
     (format nil "usage: nodecode [-p NAME] [COMMAND] [OPTIONS]~%~%~a~%~%commands~%~a~%options~%~a~%Run \"nodecode help COMMAND\" to read one command.~%"
             "With no command, nodecode opens the shell on this home's organism, and starts the organism first when none is running."
             (nlk:verbs-text *launch-verbs*)
             (nlk:verbs-text *launch-options*)))
    ((equal topic "profile")
     (nlk:profile-command (cons "help" more) :command "nodecode profile"))
    ((equal topic "gateway")
     (nlk:gateway-text (cons "help" more) :command "nodecode gateway"))
    (t (nlk:verb-help-text (or (nlk:find-verb topic *launch-verbs*)
                               (nlk:find-verb topic *launch-options*))
                           :command "nodecode"))))

(defvar *handover-launch* nil
  "T in the successor a switch exec'd into, when that successor is the one
that opens a TUI: this image inherited a live screen from the image that
replaced itself with this one, and owes it two things — the boot says
nothing to it (the report and the boot line go to the store's log, and the
frame already painted stays until this image's first frame paints over
it), and an image that cannot serve must switch its modes off rather than
leave the operator on a screen nobody owns.")

(defun handover-launch-p (&aux (word (uiop:getenv nlk::+handover-variable+)))
  "Whether this launch is a switch's successor, by the environment word the
performer left (profile.lisp, +HANDOVER-VARIABLE+)."
  ;; The word is consumed here: it describes one exec, and nothing this
  ;; process spawns inherits it.
  (ignore-errors (sb-posix:unsetenv nlk::+handover-variable+))
  (and word (plusp (length word)) t))

(defun decide-folder-behind-the-frame (store-path)
  "The folder decision for a TUI launch, taken on the organism-boot thread:
by the time this runs the shell is painting, so nothing here can spend the
operator's first frame."
  ;; Boot-cells loads the folder, or hands this process to a published image
  ;; — the very exec a launcher would otherwise make before it paints, now made
  ;; from behind the frame (launch.lisp, HAND-OVER-TO-IMAGE).
  ;;
  ;; The report goes to the store's log, where the frame is not: boot lines
  ;; written to the terminal would be covered by the TUI. Everything this
  ;; thread goes on to boot still happens after this returns — the folder is
  ;; loaded before anything serves it.
  ;; A log that will not open leaves the streams as they were.
  (let ((log (ignore-errors
              (open (stdio-log-path (or store-path nlk::*default-store-path*))
                    :direction :output :if-exists :append
                    :if-does-not-exist :create :element-type 'character))))
    (with-cleanup ((when log (ignore-errors (close log))))
      (let ((*standard-output* (or log *standard-output*))
            (*error-output* (or log *error-output*)))
        ;; The hand-over waits a second for this process's first frame: a
        ;; successor is told it inherited a live screen, and that is true only
        ;; once one is on the terminal — a screen nothing has painted is not
        ;; one, and the successor would skip entering the alternate screen and
        ;; paint onto the operator's scrollback. A frame that never lands (a
        ;; terminal that refuses writes) is not a reason to exec — the folder
        ;; loads here instead.
        ;; The cells' report and the keeper of the next launch's image are the
        ;; organism's (serve's MAIN, SERVE-GATEWAY), which runs in a process of
        ;; its own: a shell that ran them too walked the folder twice over.
        (nlk:note-boot-phase "folder-decision")
        (nlk:boot-cells
         :spawn t
         :hand-over (lambda (image)
                      (if (nlt:wait-for-frames-past 0 1000)
                          (progn (nlk:note-boot-phase "exec-image")
                                 (nlk::hand-over-to-image image))
                          "the frame never landed")))))))

(defun handover-give-back ()
  "Switch off what this image inherited and never took over: written when a
successor dies before its first frame, the one case where nothing else is
left to restore the terminal."
  (when (and *handover-launch* nlt:*inherit-screen*)
    (setf *handover-launch* nil)
    (ignore-errors (nlt:release-inherited-modes))))

(defmacro answer-and-quit (form &rest clauses)
  "Print what FORM answers -- (values TEXT CODE STREAM) -- and exit on it
(PRINT-ANSWER-AND-QUIT): a refusal no handler CLAUSE answers is the
condition's report, exit 1."
  `(multiple-value-call #'print-answer-and-quit
     (handler-case ,form
       ,@clauses
       (error (condition) (values (princ-to-string condition) 1 :error)))))

(defun main ()
  "Toplevel for the dumped executable (asdf:make :nodecode/bin)."
  ;; The entry is ATTACH-OR-OWN-ORGANISM (client.lisp): the first launch owns
  ;; the store and serves it; later launches attach to the owner's published
  ;; gateway as WebSocket clients. `serve` is the headless owner and `trial`
  ;; the fresh-process child that answers one form (launch.lisp);
  ;; `--ephemeral` boots a throwaway store of its own under the temp
  ;; directory, so a shell can try the folder as it is now beside a live
  ;; organism; `help', `--help' and `--version' answer and exit before
  ;; anything else is read. Errors print and exit nonzero instead of dropping
  ;; an operator into the SBCL debugger.
  (nlk:note-boot-phase "main")
  ;; The debugger-disabled runtime otherwise exits the whole process when
  ;; any background thread reaches its debugger, including layer workers.
  (install-worker-error-guard)
  ;; Raw text (#>TAG) in the image's reader before the first thread exists,
  ;; so eval and every layer load read it (turn.lisp, INSTALL-RAW-TEXT-READER).
  (install-raw-text-reader)
  ;; Ordinary pages before the first thread exists: with huge pages on,
  ;; every thread the organism starts costs some 4 MB resident for a few
  ;; hundred bytes of stack (os.lisp, DISABLE-HUGE-PAGES). And two malloc
  ;; arenas for every thread, not one per contended thread (CAP-MALLOC-ARENAS).
  (nlk:disable-huge-pages)
  (nlk:cap-malloc-arenas)
  ;; Names from /etc/hosts and DNS, not systemd's plugins, which hold off a
  ;; collection for as long as a lookup waits (RESOLVE-NAMES-DIRECTLY).
  (nlk:resolve-names-directly)
  ;; And an exit that never waits on OpenSSL's cleanup (os.lisp).
  (nlk:skip-openssl-exit-cleanup)
  ;; Room for descriptors: macOS starts a process at 256 open files
  ;; (os.lisp, RAISE-FILE-LIMIT), inherited by every child spawned after.
  (nlk::raise-file-limit)
  ;; The gc.lisp collection policy, here, not in the boot ladder, so every
  ;; entry gets it -- the nursery once the arguments say which process this
  ;; is, below. The heap as a whole: garbage past a slack earns a full walk
  ;; (gc.lisp, CAP-GARBAGE), and the collector's card marks for heap not
  ;; yet reached go back -- 8 MB the runtime filled at startup
  ;; (RELEASE-CARD-MARKS).
  (nlk:cap-garbage)
  (nlk:release-card-marks)
  ;; And the other dead window: a generation that grew past its budget is
  ;; collected while the operator is not typing (gc.lisp, COLLECT-WHILE-IDLE).
  (nlk:collect-while-idle)
  (with-handlers ((sb-sys:interactive-interrupt ()
                    ;; The orderly unwind is already done by the time this clause runs:
                    ;; every UNWIND-PROTECT below (stop-gateway, close-store) fired on the
                    ;; way up. What remains is a plain process exit — no second unwind of
                    ;; the threads that are still alive, because SBCL reaps those in
                    ;; arbitrary order under the disabled debugger, and any of them with
                    ;; a join in its cleanup prints a backtrace as its last act.
                    (handover-give-back)
                    (uiop:finish-outputs)
                    (uiop:quit 130 nil))
                  (error (e)
                    (handover-give-back)
                    (format *error-output* "~&nodecode: ~a~%" e)
                    ;; The same plain exit: a thread still alive is reaped, not
                    ;; unwound. Without this, a thread still alive inside a load
                    ;; would print `compilation unit aborted'.
                    (uiop:finish-outputs)
                    (uiop:quit 1 nil)))
    (multiple-value-bind (serve child session-id store-path ephemeral setup profile pristine)
        (handler-case (parse-launch-arguments (uiop:command-line-arguments))
          (launch-usage-error (refusal)
            (usage-quit (princ-to-string refusal))))
      ;; The nursery before anything allocates in earnest: SBCL's ~50MB
      ;; default lets a nursery GC land mid-replay and promote the whole
      ;; decoded frame graph (the 328MB-RSS incident). The shell's is its
      ;; own: it paints every frame, and every collection lands in one
      ;; (*SHELL-NURSERY*).
      (if (and (null child) (not serve))
          (nlk:cap-nursery nlk:*shell-nursery*)
          (nlk:cap-nursery))
      ;; Help and the version read no home, so a profile that does not
      ;; exist yet never stands between an operator and the help.
      (when child
        (case (first child)
          (:help (print-answer-and-quit
                  (launch-help-text (second child) (cddr child)) 0 :output))
          (:version (print-answer-and-quit
                     (format nil "nodecode ~a" *user-agent-version*) 0 :output))))
      ;; A launch that opens the shell draws on the controlling terminal, and
      ;; a process with none (cron, ssh without -t, a shell tool) is refused
      ;; here, before the home, the store or the folder is touched.
      (when-let (refusal (terminal-refusal child serve (uiop:command-line-arguments)))
        (write-line refusal *error-output*)
        (uiop:finish-outputs)
        (uiop:quit 2))
      ;; The home is settled before anything reads it. A named profile
      ;; re-resolves every startup parameter under its folder (the init
      ;; hook ran before argv was read); then the home this process ends
      ;; up in — flag, $NODECODE_HOME, the sticky file or the default —
      ;; is published to its children as NODECODE_HOME, so a serve
      ;; candidate, a trial, a warm image or a shell the model runs
      ;; boots into this home and never into the sticky file's.
      (when profile
        (handler-case (nlk:require-profile profile)
          (nlk:profile-error (refusal)
            (usage-quit (format nil "~a \"nodecode profile create ~a\" makes one."
                                refusal profile))))
        (nlk:adopt-profile profile))
      (nlk:publish-home)
      (when ephemeral
        ;; A store of this process's own, and the fact that it is a
        ;; throwaway said before the folder loads: a cell holding an
        ;; external lease reads NLK:*EPHEMERAL* at its start and stands down.
        (setf store-path (ephemeral-store-path))
        (nlk:publish-ephemeral))
      ;; The floor, by hand or after a death: decided here, before a pointer
      ;; is followed or anything paints, by the entries that run an organism
      ;; or front one; a child judges the folder and never stands on it.
      (when pristine (nlk:publish-pristine))
      (when (or serve (null child)) (nlk:decide-pristine))
      ;; A launch that runs an organism asks once whether its home and store
      ;; can be used, before a shell paints or anything boots (HOME-REFUSAL).
      (when-let (refusal (and (or (null child) (eq (first child) :rpc))
                              (home-refusal store-path)))
        (print-answer-and-quit refusal 1 :error))
      ;; The launch-cache verbs run before any folder loads: the `warm`
      ;; supervisor loads nothing itself, and `warm-dump` must load the
      ;; folder directly — never through the cache it is building.
      (when child
        (ecase (first child)
          (:warm (nlk::warm-main))
          (:warm-dump (nlk::warm-dump-main (second child)))
          ;; The answer on stdout, a refusal on stderr, then its exit code.
          (:login (account-login-main (second child)))
          (:machine-link (machine-link-main (second child)))
          (:profile (multiple-value-call #'print-answer-and-quit (profile-answer (second child))))
          (:gateway (gateway-main (second child)))
          (:update (answer-and-quit (values (update-text (second child)) 0 :output)))
          (:generations (generations-main (second child)))
          (:rollback (rollback-main (second child)))
          (:web (answer-and-quit (web-text (second child))))
          (:add (answer-and-quit (add-text (second child))
                  (gateway-http-error (refusal)
                    (values (gateway-http-error-detail refusal) 1 :error))))
          (:rpc (rpc-main (second child) (third child) store-path))
          (:trial nil)))
      ;; The folder loads in EVERY process before anything else runs:
      ;; the owner, an attached TUI, a headless serve
      ;; candidate. An override of a shipped definition is live wherever
      ;; that definition runs; a folder that fails is one line here and
      ;; in (nlk:cells), never a refused boot. BOOT-CELLS may hand the
      ;; whole process to a published warm image instead of loading — the
      ;; exec re-enters this MAIN with the folder already loaded — and
      ;; only a top-level entry sends a builder after the first paint:
      ;; children take a hit but never spawn.
      ;; A successor a switch exec'd here opens onto a screen that is
      ;; already a TUI's — its predecessor's frame, on the alternate screen,
      ;; waiting for this image's first frame to paint over it. So the boot
      ;; says nothing to that terminal and the first frame takes the screen
      ;; instead of entering it. Only the path that runs a TUI: a headless
      ;; successor owns its stdout, and a launch-cache child answers on it.
      (labels ((launch ()
                 (cond ((and (null child) (not serve))
                        ;; A TUI paints FIRST: the shell the operator sees comes
                        ;; from code this image already holds, and the folder
                        ;; decision — load it, or hand the process to a
                        ;; published image — rides the boot thread behind that
                        ;; frame (DECIDE-FOLDER-BEHIND-THE-FRAME). Nothing in
                        ;; the first frame needs the folder, and the frame is
                        ;; what the operator is waiting for: measured on this
                        ;; machine, the old order spent ~74 ms of the launch in
                        ;; the launcher, ~22 ms on the image's key and notice,
                        ;; and only then painted.
                        ;;
                        ;; A published image for this folder answers the whole
                        ;; launch, so the hand-over is made BEFORE anything
                        ;; paints: the frame the operator then sees is the
                        ;; image's own -- from the folder's code, not the
                        ;; launcher's, which the successor's frame would paint
                        ;; over -- and the exec no longer waits for one.
                        (nlk::hand-over-to-published-image)
                        (attach-or-own-organism :session-id session-id
                                                :store-path store-path
                                                :setup setup
                                                :prelude (lambda ()
                                                           (decide-folder-behind-the-frame
                                                            store-path))))
                       (t
                        ;; a serve leaves its receipt before the load: a self that dies before serving is found by the launch after
                        (nlk:boot-cells :spawn (null child) :receipt (and serve t))
                        ;; A child prints its own report on stdout; the boot line
                        ;; would be noise the parent has to skip.
                        (when (eq (first child) :trial)
                          (nlk::trial-main))
                        (nlk:describe-cells)
                        (if serve
                            ;; Headless owner-or-nothing: the resurrection candidate a
                            ;; client's reconnect ladder spawns, and the manual way to run
                            ;; a gateway with no TUI. Losing the ownership election is a
                            ;; normal exit — the winner is serving, which is what the
                            ;; invoker wanted ensured. `gateway run' (:WAIT), what the
                            ;; gateway's unit runs, never loses: it waits for the store.
                            (progn
                              ;; Configured and effective, once, in the gateway's log.
                              (when-let (line (nlk::file-limit-line))
                                (format *error-output* "~a~%" line)
                                (finish-output *error-output*))
                              (serve-organism-candidate :store-path store-path :wait (eq serve :wait)))
                            (attach-or-own-organism :session-id session-id
                                                    :store-path store-path
                                                    :setup setup))))))
        (setf *handover-launch* (and (handover-launch-p) (null child) (not serve)))
        (cond (*handover-launch*
               (setf nlt:*inherit-screen* t)
               (call-with-severed-stdio
                (stdio-log-path (or store-path nlk::*default-store-path*))
                #'launch))
              (t (launch)))))))

(defun usage-refusal-text (detail)
  "What a refused command line prints: DETAIL, and where the commands are."
  ;; The whole help is `nodecode --help''s alone: a refusal that buried its
  ;; one line under every command made both harder to read.
  (format nil "nodecode: ~a~%Run \"nodecode --help\" to see every command.~%" detail))

(defun usage-quit (detail)
  "Print USAGE-REFUSAL-TEXT for DETAIL to stderr, then exit 2 (bad invocation)."
  (fresh-line *error-output*)
  (write-string (usage-refusal-text detail) *error-output*)
  (uiop:finish-outputs)
  (uiop:quit 2))

(defun terminal-refusal (child serve args)
  "What a launch says where the process has no terminal, or NIL when it can
go on: only a launch that opens the shell -- no CHILD, not a headless SERVE --
draws on one, and the sentence names the command ARGS gave whose table entry
says TERMINAL."
  (unless (or child serve (nlt:terminal-present-p))
    (format nil "nodecode ~@[~a ~]needs a terminal (no controlling tty); run it in one."
            (some (lambda (word &aux (verb (nlk:find-verb word *launch-verbs*)))
                    (and verb verb.terminal verb.name))
                  args))))

(defun print-answer-and-quit (text code stream)
  "Print TEXT — on stdout for an :OUTPUT answer, as a refusal on stderr
otherwise — then exit with CODE."
  (if (eq stream :output)
      (progn (write-string text *standard-output*)
             (fresh-line *standard-output*))
      (format *error-output* "~&nodecode: ~a~%" text))
  (uiop:finish-outputs)
  (uiop:quit code))

(defun terminal-answer (question &key (input *standard-input*) (output *error-output*)
                                      (interactive (interactive-stream-p input)))
  "Ask QUESTION on the terminal and read one line back, or NIL when there is
no terminal to ask: INPUT is not one (a pipe, a script), or this is
Windows, where a line read from the console under this process is unproven
(the TUI reads the console's input records itself)."
  ;; The question goes to OUTPUT, stderr, so an answer piped elsewhere never
  ;; carries it.
  #+win32 (progn question input output interactive nil)
  #-win32
  (when interactive
    (format output "~a " question)
    (finish-output output)
    (read-line input nil nil)))

(defun yes-answer-p (reply)
  "Whether REPLY, a line typed at a question, says yes."
  (and (stringp reply)
       (member (string-trim '(#\Space #\Tab #\Return) reply) '("y" "yes") :test #'string-equal)
       t))

(defun profile-answer (words &key (ask #'terminal-answer))
  "The `profile' command line as (values TEXT EXIT-CODE STREAM): the answer,
0 and :OUTPUT; a refusal, :ERROR and 2 for words the verbs cannot read, 1
otherwise."
  ;; A delete nobody said yes to is asked through ASK: a yes runs it again
  ;; with --yes, any other answer keeps the profile, and with no terminal to
  ;; ask the refusal says how to answer on the line.
  (flet ((run (words) (nlk:profile-command words :command "nodecode profile")))
    (with-handlers ((nlk:profile-usage-error (refusal) (values (princ-to-string refusal) 2 :error))
                    (nlk:profile-error (refusal) (values (princ-to-string refusal) 1 :error)))
      (values (handler-case (run words)
                (nlk:profile-needs-yes (pending)
                  (let ((reply (funcall ask (nlk:profile-needs-yes-question pending))))
                    (cond ((yes-answer-p reply) (run (append words (list "--yes"))))
                          (reply (return-from profile-answer
                                   (values "nothing was deleted" 1 :error)))
                          (t (error pending))))))
              0 :output))))

(defun gateway-main (words)
  "The `gateway' command: the verbs' answer printed, the answer on stdout and a
refusal on stderr, then its exit code; `logs --follow' hands the terminal to
journalctl until it is interrupted."
  ;; Runs before any folder loads: the gateway's lifecycle is the operating
  ;; system's and needs no organism, and `gateway run', the organism itself,
  ;; never comes here.
  ;; The answer, 0 and :OUTPUT; a refusal, :ERROR and 2 for unreadable words, 1 otherwise.
  (answer-and-quit
   (cond ((and (equal (first words) "logs")
               (intersection '("--follow" "-f") (rest words) :test #'string=))
          (handler-case (uiop:run-program (nlk:gateway-log-argv :follow t)
                                          :input :interactive :output :interactive
                                          :error-output :interactive :ignore-error-status t)
            (sb-sys:interactive-interrupt () nil))
          (values "" 0 :output))
         ;; `setup --help' is the verbs' help, like every verb's.
         ((and (equal (first words) "setup")
               (not (intersection '("--help" "-h") (rest words) :test #'string=)))
          (handler-case (gateway-setup-text (rest words))
            (sb-sys:interactive-interrupt ()
              (values "setup stopped; what it saved stays saved" 130 :error))))
         (t (values (nlk:gateway-text words :command "nodecode gateway" :live #'gateway-live-facts)
                    0 :output)))
   (nlk:gateway-usage-error (refusal) (values (princ-to-string refusal) 2 :error))))

;;; --- `nodecode gateway setup' ----------------------------------------------
;;; A terminal walk over the organism's own config surface, the calls the web
;;; page's Control > Channels > Set up and /settings make: switch the channel's
;;; cell on (POST /api/gateway/cells), keep a pasted token in the section's
;;; 0600 file (POST /api/gateway/config {secret}), check it against the
;;; platform (/config/probe), pick who it answers from what the token sees
;;; (/config/probe {field}), write those members (/config {path, value}) and
;;; start the channels again (/api/channels?op=restart). The walk reads and
;;; writes no file itself; every refusal is the organism's, in its words.

(define-condition gateway-setup-ended (error) ()
  (:report "the input ended")
  (:documentation "A question of the setup walk read no line: no terminal, or end of input."))

(defun terminal-secret-answer (question &key (input *standard-input*) (output *error-output*))
  "Ask QUESTION and read one line back with the terminal's echo off, or NIL
when there is no terminal to ask (TERMINAL-ANSWER's rule)."
  #+win32 (progn question input output nil)
  #-win32
  (when (interactive-stream-p input)
    (flet ((stty (word)
             (ignore-errors
              (uiop:run-program (list "stty" word) :input :interactive
                                                   :output nil :error-output nil)
              t)))
      (format output "~a " question)
      (finish-output output)
      (let ((quiet (stty "-echo")))
        (unwind-protect (read-line input nil nil)
          (when quiet (stty "echo"))
          (terpri output)
          (finish-output output))))))

(defun default-yes-p (reply)
  "Whether REPLY, a line typed at a [Y/n] question, says yes: an empty one does."
  (and (stringp reply)
       (or (string= "" (string-trim '(#\Space #\Tab #\Return) reply))
           (yes-answer-p reply))))

(defun gateway-setup-port (ask out &key (seconds 120))
  "The port of the organism serving this home; when none does, the gateway
turned on (NLK:GATEWAY-ON) once ASK says yes, and its port once it serves.
NIL when the operator says no or it does not serve within SECONDS."
  (or (organism-port)
      (progn
        (format out "~&A bot runs inside this home's organism, and none is running.~%")
        (unless (default-yes-p (funcall ask "Turn the gateway on: run it in the background now and at every login? [Y/n]"))
          (return-from gateway-setup-port nil))
        (nlk:gateway-on)
        (format out "Starting the gateway")
        (loop with deadline = (+ (get-universal-time) seconds)
              for port = (organism-port)
              when (and port (ignore-errors
                              (gateway-http-session-request port :get "/api/gateway/identity" nil
                                                            (gateway-configured-operator-token)
                                                            :connect-timeout 2 :read-timeout 3)))
                do (format out " serving.~%")
                   (return port)
              while (< (get-universal-time) deadline)
              do (format out ".")
                 (finish-output out)
                 (sleep 1)
              finally (format out "~%")
                      (return nil)))))

(defun gateway-setup-section (call id)
  "The channels.ID section as GET /api/gateway/config draws it, or NIL."
  (find (format nil "channels.~a" id)
        (json-array (funcall call :get "/api/gateway/config" nil) "sections")
        :key (lambda (section) (json-value section :string "key")) :test #'equal))

(defun gateway-setup-said (answer)
  "What a config write ANSWER says it did."
  (or (json-value answer :string "said") "saved"))

(defun gateway-setup-member-path (path name)
  "The member NAME of the section at PATH, as a write's path."
  (concatenate 'vector path (vector name)))

(defun gateway-setup-refusal (condition)
  "The organism's words for a 400 CONDITION; any other status is not the
operator's to answer, so it goes on up."
  (if (eql 400 (gateway-http-error-status condition))
      (gateway-http-error-detail condition)
      (error condition)))

(defun gateway-setup-picks (reply choices)
  "The ids REPLY names: each comma- or space-separated word a 1-based number
into CHOICES ((ID . LABEL) ...), or an id as typed."
  (loop for word in (uiop:split-string reply :separator '(#\, #\Space #\Tab))
        for number = (and (plusp (length word)) (every #'digit-char-p word)
                          (parse-integer word))
        unless (string= word "")
          collect (if (and number (<= 1 number (length choices)))
                      (car (nth (1- number) choices))
                      word)))

(defun gateway-setup-secret (call ask-secret out path section)
  "Keep the token SECTION takes by reference: a pasted one in its 0600 file,
or $NAME, the environment variable the organism reads it from. => :KEPT when
an Enter keeps a set one, :WRITTEN."
  (let* ((secret (json-value section :object "secret"))
         (noun (json-value secret :string "noun"))
         (env (json-value secret :string "env"))
         (file (json-value secret :string "file"))
         (set (loop for field across (json-array section "fields")
                    for name = (json-value field :string "name")
                    when (and (member name (list env file) :test #'equal)
                              (json-value field :boolean "set"))
                      return name)))
    (loop
      (let ((text (string-trim '(#\Space #\Tab #\Return)
                               (funcall ask-secret
                                        (if set
                                            (format nil "The ~a is set. Enter keeps it; paste a new one, or $NAME for the environment variable that holds it:" noun)
                                            (format nil "Paste the ~a (it is not shown), or $NAME for the environment variable that holds it:" noun))))))
        (cond ((string= text "")
               (if set (return :kept) (format out "The ~a is needed.~%" noun)))
              ((char= #\$ (char text 0))
               (let ((name (subseq text 1)))
                 (cond ((equal set file)
                        (format out "The ~a is kept in a file now: paste the token itself to replace it.~%" noun))
                       ((not (and (plusp (length name))
                                  (every (lambda (char) (or (alphanumericp char) (char= char #\_))) name)))
                        (format out "~s is not an environment variable's name.~%" name))
                       (t (format out "~a~%" (gateway-setup-said
                                              (funcall call :post "/api/gateway/config"
                                                       (json-object "path" (gateway-setup-member-path path env)
                                                                    "value" name))))
                          (return :written)))))
              (t (format out "~a~%" (gateway-setup-said
                                     (funcall call :post "/api/gateway/config"
                                              (json-object "path" path "secret" text))))
                 ;; The file names it now; the variable it replaces goes, so one of the two is set.
                 (when (equal set env)
                   (funcall call :post "/api/gateway/config"
                            (json-object "path" (gateway-setup-member-path path env) "remove" t)))
                 (return :written)))))))

(defun gateway-setup-check (call ask out path)
  "What the section at PATH's check says its token sees, printed, and
whether the operator goes on with that token: T, or NIL to paste another --
also when the check refused it, the refusal printed."
  ;; The check's text is a report (a wrong token is one of its lines), not a
  ;; verdict: the operator, who reads it, decides.
  (handler-case
      (let ((text (json-value (funcall call :post "/api/gateway/config/probe" (json-object "path" path))
                              :string "text")))
        (format out "~&~%What the token sees:~%~{  ~a~%~}" (uiop:split-string (or text "") :separator '(#\Newline)))
        (default-yes-p (funcall ask "Go on with this token? [Y/n]")))
    (gateway-http-error (condition)
      (format out "~&The check refused it: ~a~%" (gateway-setup-refusal condition))
      nil)))

(defun gateway-setup-field (call ask out path field &aux (name (json-value field :string "name"))
                                                       (list (equal "list" (json-value field :string "type"))))
  "Ask for FIELD of the section at PATH -- from the ids its token can see
when it offers choices -- and write what the answer names; an Enter keeps it."
  (let ((choices (and (json-value field :boolean "choices")
                      (handler-case
                          (map 'list (lambda (choice) (cons (json-value choice :string "id")
                                                            (json-value choice :string "label")))
                               (json-array (funcall call :post "/api/gateway/config/probe"
                                                    (json-object "path" path "field" name))
                                           "choices"))
                        (gateway-http-error (condition)
                          (format out "~&  (nothing to pick from: ~a)~%" (gateway-setup-refusal condition))
                          nil)))))
    (format out "~&~%~a: ~a~@[~%  now: ~a~]~%" name (json-value field :string "doc")
            (and (json-value field :boolean "set") (json-value field :string "text")))
    (loop for (id . label) in choices
          for number from 1
          do (format out "  ~3d  ~a~:[  ~a~;~]~%" number label (search id label) id))
    (loop
      (let ((text (string-trim '(#\Space #\Tab #\Return)
                               (funcall ask (cond (choices "Numbers or ids, comma-separated; Enter keeps it:")
                                                  (list "Ids, comma-separated; Enter keeps it:")
                                                  (t "Value; Enter keeps it:"))))))
        (when (string= text "") (return :kept))
        (handler-case
            (progn (format out "~a~%" (gateway-setup-said
                                       (funcall call :post "/api/gateway/config"
                                                (json-object "path" (gateway-setup-member-path path name)
                                                             "value" (if list
                                                                         (coerce (gateway-setup-picks text choices) 'vector)
                                                                         text)))))
                   (return :written))
          (gateway-http-error (condition)
            (format out "~a~%" (gateway-setup-refusal condition))))))))

(defun gateway-setup-lane (call id &key (seconds 30))
  "ID's row of GET /api/channels once it is running and connected, or has
settled anywhere else, after at most SECONDS."
  (loop with deadline = (+ (get-universal-time) seconds)
        for row = (find id (json-array (funcall call :get "/api/channels" nil) "channels")
                        :key (lambda (row) (json-value row :string "id")) :test #'equal)
        for state = (and row (json-value row :string "state"))
        when (or (and (equal state "running") (json-value row :boolean "connected"))
                 (member state '("refused" "failed" "unconfigured" "stopped" "done") :test #'equal)
                 (>= (get-universal-time) deadline))
          return row
        do (sleep 1)))

(defun gateway-setup-text (words &key (ask #'terminal-answer) (ask-secret #'terminal-secret-answer)
                                      (out *standard-output*) call (keep-running t))
  "`nodecode gateway setup [CHANNEL]': walk one channel from its bot's token
to a running lane, over the organism serving this home (GATEWAY-SETUP-PORT
turns the gateway on when none does). CALL, (METHOD PATH BODY) => the
decoded answer, is the organism's control surface -- a test's fake; ASK and
ASK-SECRET read a line. KEEP-RUNNING offers the gateway at the end when a
shell serves. => (values TEXT CODE STREAM)."
  (labels ((ask (question)
             (finish-output out)
             (or (funcall ask question) (error 'gateway-setup-ended)))
           (ask-secret (question)
             (finish-output out)
             (or (funcall ask-secret question) (error 'gateway-setup-ended)))
           (stop (text &optional (code 1))
             (return-from gateway-setup-text (values text code (if (zerop code) :output :error)))))
    (handler-case
        (let* ((call (or call
                         (let ((port (gateway-setup-port #'ask out)))
                           (unless port
                             (stop "nothing was set up: a bot needs a running organism -- `nodecode gateway on', or a `nodecode' shell -- then `nodecode gateway setup' again"))
                           (lambda (method path body)
                             (gateway-http-session-request port method path body
                                                           (gateway-configured-operator-token)
                                                           :read-timeout 60)))))
               (cells (loop for row across (json-array (funcall call :get "/api/gateway/cells" nil) "cells")
                            for name = (json-value row :string "name")
                            when (and (uiop:string-prefix-p "nodecode-channel-" name)
                                      (string/= name "nodecode-channel-kit"))
                              collect (cons (subseq name (length "nodecode-channel-")) row)))
               (lanes (ignore-errors (json-array (funcall call :get "/api/channels" nil) "channels")))
               (id (let ((named (first words)))
                     (if named
                         (let ((id (string-downcase
                                    (reduce (lambda (word prefix)
                                              (if (uiop:string-prefix-p prefix word) (subseq word (length prefix)) word))
                                            '("nodecode-" "channel-") :initial-value named))))
                           (unless (assoc id cells :test #'string=)
                             (stop (format nil "no channel ~a here; there are ~{~a~^, ~}" named (mapcar #'car cells)) 2))
                           id)
                         (progn
                           (unless cells (stop "no channel adapter is on offer here"))
                           (format out "~&Which channel?~%")
                           (loop for (id . row) in cells
                                 for number from 1
                                 for lane = (find id lanes :key (lambda (lane) (json-value lane :string "id")) :test #'equal)
                                 do (format out "  ~d  ~10a ~a~%" number id
                                            (cond (lane (json-value lane :string "state"))
                                                  ((json-value row :boolean "on") "installed")
                                                  (t "not installed"))))
                           (loop for reply = (string-trim '(#\Space #\Tab #\Return) (ask "Number or name:"))
                                 for number = (ignore-errors (parse-integer reply))
                                 for pick = (cond ((and number (<= 1 number (length cells))) (car (nth (1- number) cells)))
                                                  ((assoc (string-downcase reply) cells :test #'string=) (string-downcase reply)))
                                 when pick return pick
                                 do (format out "Pick 1 to ~d, or a name.~%" (length cells)))))))
               (cell (format nil "nodecode-channel-~a" id)))
          (unless (json-value (cdr (assoc id cells :test #'string=)) :boolean "on")
            (format out "~&Installing ~a...~%" cell)
            (format out "~a~%" (or (json-value (funcall call :post "/api/gateway/cells" (json-object "name" cell "on" t))
                                               :string "text")
                                   "installed")))
          (let* ((section (or (gateway-setup-section call id)
                              (stop (format nil "~a declares no channels.~a section to set up" cell id))))
                 (path (json-value section :array "path")))
            (format out "~&~%Setting up ~a.~%" id)
            (unless (equal "complete" (json-value section :string "needs"))
              (format out "On ~a's side first:~%~{  ~a~%~}" id
                      (loop for step in (uiop:split-string (or (json-value section :string "guide") "") :separator '(#\;))
                            for words = (string-trim '(#\Space) step)
                            unless (string= words "") collect words)))
            (when (json-value section :object "secret")
              (loop do (gateway-setup-secret call #'ask-secret out path section)
                    until (or (not (json-value section :boolean "check"))
                              (gateway-setup-check call #'ask out path))
                    do (setf section (gateway-setup-section call id))))
            (loop repeat 3
                  do (loop with pair = (let ((secret (json-value section :object "secret")))
                                         (and secret (list (json-value secret :string "env")
                                                           (json-value secret :string "file"))))
                           for field across (json-array (gateway-setup-section call id) "fields")
                           for name = (json-value field :string "name")
                           when (and (not (member name pair :test #'equal))
                                     (or (json-value field :boolean "essential") (equal name "owner")))
                             do (gateway-setup-field call #'ask out path field))
                     (setf section (gateway-setup-section call id))
                  until (equal "complete" (json-value section :string "needs"))
                  do (format out "~&~%~:(~a~) ~a.~%" id (json-value section :string "needs")))
            (unless (equal "complete" (json-value section :string "needs"))
              (stop (format nil "~a is not started: ~a; what was saved stays, and `nodecode gateway setup ~a' goes on from there"
                            id (json-value section :string "needs") id)))
            (format out "~&~%Starting the channels...~%")
            (funcall call :post "/api/channels?op=restart" nil)
            (let* ((lane (gateway-setup-lane call id))
                   (state (and lane (json-value lane :string "state")))
                   (connected (and lane (json-value lane :boolean "connected")))
                   (detail (and lane (or (json-value lane :string "detail") (json-value lane :string "last_rejection")))))
              (unless (and (equal state "running") connected)
                (stop (format nil "~a did not connect: ~a~@[ -- ~a~]; `nodecode gateway logs' has the rest"
                              id (or state "no lane") detail)))
              (let ((gateway (ignore-errors (nlk:gateway-state))))
                (when (and keep-running gateway (nlk:gateway-state-supported gateway)
                           (not (eq :ours (nlk:gateway-state-installed gateway)))
                           (default-yes-p (ask (format nil "~a runs while the shell serving it does. Turn the gateway on, so it keeps running and starts at login? [Y/n]" id))))
                  (nlk:gateway-on)))
              (values (format nil "~a is connected~@[: ~a~]~%Change it: `nodecode gateway setup ~a', /settings > Channels, or `nodecode web' > Control > Channels.~@[~%Someone it does not know who writes to it directly is given a pairing code; /channels pair CODE lets them in.~]"
                              id detail id
                              (find-if (lambda (field) (and (equal "pairing" (json-value field :string "name"))
                                                            (not (eq :false (json-value field :any "value")))
                                                            (not (eq :false (json-value field :any "default")))))
                                       (json-array section "fields")))
                      0 :output))))
      (gateway-setup-ended ()
        (stop "setup stopped: there was no line to read (no terminal, or the input ended); what it saved stays saved"))
      (gateway-http-error (condition)
        (stop (format nil "the organism refused: ~a" (gateway-http-error-detail condition)))))))

(defun web-text (words)
  "`nodecode web': the page's link for the organism serving this home, opened
where there is a display. => (values ANSWER CODE STREAM)."
  ;; The link carries the operator token after its #, which a browser never
  ;; sends to a server, so the terminal that asked is the one place it is
  ;; written: never a shell's transcript, never the log.
  (when (intersection '("--help" "-h" "help") words :test #'string=)
    (return-from web-text
      (values (nlk:verb-help-text (nlk:find-verb "web" *launch-verbs*) :command "nodecode")
              0 :output)))
  (let ((port (organism-port))
        (token (gateway-configured-operator-token)))
    (cond
      ((null port)
       (values "nodecode is not running for this home: start it with `nodecode' (or `nodecode gateway on' to keep it running), then run this again"
               1 :error))
      ((not (eql 200 (nth-value 1 (ignore-errors
                                   (nlk:http :head (format nil "http://127.0.0.1:~d/web/" port)
                                             :timeout 3)))))
       ;; A folder installed and never loaded waits for a restart; one not
       ;; installed, for /cells.
       (values (if (nlk:installed-cell-p "nodecode-web")
                   "the web cell is installed, and the nodecode running for this home has not loaded it: restart it (`nodecode gateway restart' when the gateway keeps it running), then run this again"
                   "the web cell is not installed: in nodecode, open /cells and turn web on with Space, then run this again")
               1 :error))
      ((null token) (values "this home has no operator token" 1 :error))
      (t
       (let ((link (format nil "http://127.0.0.1:~d/web/#t=~a" port token)))
         (values (format nil "~a~%~:[no display here: open the link in a browser on this machine~;opened in your browser~]; it carries the operator token, so keep it to yourself"
                         link (eq t (open-target link)))
                 0 :output))))))
