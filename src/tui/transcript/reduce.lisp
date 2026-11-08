;;;; reduce.lisp --- mutable reducer, projection, and viewport. Evolved package.
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

(nlk:define-side-table transcript-rows-memo (:weakness :key)
  "STATE's last (HISTORY-LANE TAIL-LANE . ALL): the two lanes' own list cells
as TRANSCRIPT-ROWS last appended them, and the append. A streamed delta
mutates the live tail's own last row in place and leaves both lanes EQ
(APPEND-TAIL-ROW only rebuilds TAIL_ROWS on a new row; history never moves
between renders of the same delta), so TRANSCRIPT-ROWS answers this ALL
again rather than pay to copy HISTORY_ROWS fresh for a frame nothing in it
changed.")

(defun transcript-rows (state)
  "All rows in display order: promoted history first, then the live tail."
  ;; APPEND always copies its first argument's conses fresh, so this was
  ;; O(history length) on every call, streamed delta or not — most of a
  ;; render's PROJECT-STATE cost for a transcript long past its tail window
  ;; (dfb3610's own gate run: a streamed word still n^0.43 after its fix,
  ;; bytes flat; this and GROUP-ROWS-MEMO are the two walks that were still
  ;; paying for history on every delta). Neither lane is ever mutated in
  ;; place once handed out (every writer SETFs the hash entry, never NCONCs
  ;; onto a cell this held), so the memo answers exactly what a fresh APPEND
  ;; would while both lanes stand as they were.
  (when (hash-table-p state)
    (let ((history (gethash "history_rows" state '()))
          (tail (gethash "tail_rows" state '()))
          (memo (transcript-rows-memo state)))
      (if (and memo (eq (first memo) history) (eq (second memo) tail))
          (cddr memo)
          (let ((all (append history tail)))
            (setf (transcript-rows-memo state) (list* history tail all))
            all)))))

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

;;; A long session's history lane is thousands of rows, and two things touch
;;; it on every fact a fold takes: a promoted row goes on its end, and the
;;; rows the fact produced get its log position (STAMP-UNPOSITIONED-ROWS).
;;; Walking the lane for either made opening a session quadratic in its
;;; length. The lane only ever grows at its end in place; any other change
;;; writes a fresh list (REMOVE, a divider rested inside it, a push), so a
;;; lane whose first cons is the one a mark was taken on still holds every
;;; cons the mark has seen, unchanged: the mark keeps where the lane ends
;;; and how far its rows are all stamped, and a fresh list is read from its
;;; head once.

(defstruct (lane-mark (:constructor make-lane-mark (head)))
  "Where a state's history lane, first cons HEAD, ends (LAST) and the cons
through which every row carries a log position (STAMPED)."
  head (last nil) (stamped nil))

(defvar *lane-marks* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Each fold state's LANE-MARK for its history lane, gone with the state.")

(defun history-lane-mark (state)
  "STATE's mark for the history lane it holds now: the one taken on this
lane, or a fresh one when the lane is a new list."
  (let ((head (gethash "history_rows" state '()))
        (mark (gethash state *lane-marks*)))
    (if (and mark (eq head (lane-mark-head mark)))
        mark
        (setf (gethash state *lane-marks*) (make-lane-mark head)))))

(defun append-history-rows (state rows)
  "Put ROWS, a fresh list, on the end of STATE's history lane, in place. => STATE."
  (when rows
    (let ((mark (history-lane-mark state)))
      (if (null (lane-mark-head mark))
          (setf (gethash "history_rows" state) rows
                (gethash state *lane-marks*) (make-lane-mark rows))
          ;; LAST from the mark's end walks only what was put on behind it.
          (setf (cdr (last (or (lane-mark-last mark) (lane-mark-head mark)))) rows))
      (setf (lane-mark-last (gethash state *lane-marks*)) (last rows))))
  state)

(defun append-tail-row (state row)
  "Append ROW to the tail window, promoting the oldest row on overflow."
  ;; History grows in place: the lane is the state's alone (TRANSCRIPT-ROWS
  ;; hands out a copy), and a fresh copy per promoted row made a long
  ;; replay's fold quadratic in its rows.
  (let ((tail (append (gethash "tail_rows" state '()) (list row))))
    (loop while (> (length tail) 16) ; the Zig max_tail_rows bound
          do (append-history-rows state (list (pop tail))))
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
    (append-history-rows state (remove-if-not #'transcript-row-hydrated
                                              (copy-list (gethash "tail_rows" state '()))))
    (setf (gethash "tail_rows" state) '()))
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

