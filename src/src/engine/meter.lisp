;;;; meter.lisp --- the running turn as a value: what it spent, what it may spend.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The shell's footer and a channel's status card both tally a running turn
;;;; -- `working · 73 actions · 25m05s' -- and the model answering tool results
;;;; saw neither: no clock, no count, no operator. A cell asking which turn
;;;; its code runs in reached into the engine's own specials for it (memory,
;;;; experience and skills each did). Without this record, a turn can outlive
;;;; the work the operator stops; every fix for that is a rule over this
;;;; record, and every such rule needs the record first.
;;;;
;;;; One LIVE-TURN per running turn, bound by RUN and rebound wherever the
;;;; turn's work runs -- a background evaluation. TURN answers
;;;; it as a plist any hook takes apart with GETF. TURN-BUDGET is the one
;;;; policy read: *TURN-BUDGET* here, NIL -- no turn is capped by default, it
;;;; sees its clock instead -- advised by whoever owns a session's turns and
;;;; read once when the turn opens. What a budget means is written once, below
;;;; and nowhere else: a call past it is refused, so the model answers with
;;;; what it has; a round that calls tools again after that refusal ends the
;;;; turn; a wait never outlasts it (exec.lisp CLAMP-EXEC-YIELD).
;;;;
;;;; The spend line is what every tool result carries to the model: the turn's
;;;; clock, its rounds, its calls, against the budget when one is set, the
;;;; context the newest round filled, and the wall clock -- no prompt gives
;;;; the model today's date, and a reader decoding message ids into dates can
;;;; read 1956 before catching its own arithmetic. It grows the ask for one
;;;; sentence once the operator has read nothing for a while. It rides the
;;;; result's durable metadata and is rendered into the model's copy alone
;;;; (TOOL-RESULT-TEXT), by the round and by every later fold of the log
;;;; alike, so the bytes never drift and the prompt cache holds, while the
;;;; transcript and a channel card keep reading what the tool said.

(in-package #:nodecode.evolved)

(nlk:access (usage provider-usage))

(defstruct (live-turn (:constructor %make-live-turn) (:copier nil))
  "The running turn's meter."
  ;; Live-only: the turn thread writes it, and any thread of the turn reads it
  ;; through TURN.
  (session-id "" :type string :read-only t)
  (turn-id "" :type string :read-only t)
  (parent nil :type (or null string) :read-only t)
  (started (get-internal-real-time) :type integer :read-only t)
  (rounds 0 :type integer)
  (calls 0 :type integer)
  (input-tokens 0 :type integer)
  (cached-input-tokens 0 :type integer)
  (output-tokens 0 :type integer)
  (reasoning-tokens 0 :type integer)
  ;; The prompt the newest round placed in the window, as the provider counted
  ;; it (input + cache reads + cache writes), and the window the context
  ;; budget believed then. NIL until a round reports.
  (context-tokens nil :type (or null integer))
  (context-window nil :type (or null integer))
  ;; The operator's last sentence from this turn: when. The turn's opening
  ;; until the first one.
  (text-at (get-internal-real-time) :type integer)
  ;; (:SECONDS N :CALLS N :TOKENS N), each key optional, or NIL.
  (budget nil :type list)
  ;; T once a round's calls were refused for the budget: the next round that
  ;; calls a tool is the turn's end.
  (closing nil :type boolean)
  ;; T once the window crossed the eviction line and the model was told so
  ;; (HYDRATE-SESSION): the next crossing's request evicts, and a request
  ;; under the line again re-arms the warning.
  (context-warned nil :type boolean))

(nlk:access (live live-turn))

(defvar *live-turn* nil
  "The running turn's LIVE-TURN, bound by RUN and rebound on every thread
its work runs on; NIL outside a turn.")

;;; NIL: no turn is capped by default, and a turn without a cap reads no spend
;;; line; a cap is opt-in, by a folder advising TURN-BUDGET for its sessions or
;;; by a layer setting this.
(defparameter *turn-budget* nil
  "The budget a turn runs under when no owner of its session says otherwise
(TURN-BUDGET): (:SECONDS N :CALLS N :TOKENS N), each key optional, or NIL
for none.")

(defparameter *budget-nearly-spent* 4/5
  "The fraction of a budget past which the spend line says to finish.")

;;; --- the value -------------------------------------------------------------

(defun turn ()
  "The turn this thread is working for, as a plist, or NIL outside one."
  ;; Identity: :SESSION-ID, :TURN-ID and :PARENT, the session this one was
  ;; forked from (NIL when it was not). Spend: :SECONDS since it opened,
  ;; provider :ROUNDS, tool :CALLS, the token counts its rounds reported
  ;; (:INPUT-TOKENS :CACHED-INPUT-TOKENS :OUTPUT-TOKENS :REASONING-TOKENS),
  ;; :CONTEXT-TOKENS the newest round placed in the window against
  ;; :CONTEXT-WINDOW (either NIL until known) and :QUIET-SECONDS since the
  ;; operator last read a sentence from it. :BUDGET, the limits it runs
  ;; under: (:SECONDS N :CALLS N :TOKENS N), each key optional, or NIL. A
  ;; fresh plist on every call.
  (when-let (live *live-turn*)
    ;; A key names the LIVE-TURN slot it reads, but for the two clocks and the budget's copy.
    (loop with now = (get-internal-real-time)
          for slot in '(session-id turn-id parent seconds rounds calls input-tokens
                        cached-input-tokens output-tokens reasoning-tokens context-tokens
                        context-window quiet-seconds budget)
          collect (alexandria:make-keyword slot)
          collect (case slot
                    (seconds (floor (elapsed-ms live.started now) 1000))
                    (quiet-seconds (floor (elapsed-ms live.text-at now) 1000))
                    (budget (copy-list live.budget))
                    (t (slot-value live slot))))))

(defun turn-budget (turn)
  "The limits TURN -- the TURN plist of a turn that is opening -- runs
under: (:SECONDS N :CALLS N :TOKENS N), each key optional, or NIL for none;
*TURN-BUDGET* here."
  ;; Whoever owns a session's turns advises this function for its own sessions
  ;; and calls NEXT for every other: a channel's lanes, a cron job's fires, a
  ;; reflection. Read once, as the
  ;; turn opens. What a budget means is not the advice's to decide: a call
  ;; past it is refused, a round that calls tools after the refusal ends the
  ;; turn, and a wait never outlasts it. :TOKENS are billable tokens -- input,
  ;; output and reasoning, cache reads free -- of the turn's rounds; past them
  ;; the turn closes the way it does past its calls.
  (declare (ignore turn))
  *turn-budget*)

(defun budget-limits (value)
  "VALUE, what TURN-BUDGET answered, as the budget a turn runs under: the
:SECONDS and :TOKENS it names with a positive number, the :CALLS with a
whole one, or NIL. (:CALLS 0) is a turn that may run no tool: its first
call is refused."
  (loop for (key type) in '((:seconds (real (0))) (:calls (integer 0)) (:tokens (integer 1)))
        for limit = (ignore-errors (getf value key))
        when (typep limit type) append (list key limit)))

(defun open-live-turn (session-id turn-id)
  "The LIVE-TURN of a turn opening on SESSION-ID, its budget read."
  ;; An advice of TURN-BUDGET that signals is warned about and leaves the turn
  ;; unbudgeted: a policy read is not a precondition of the turn.
  (let* ((parent (and (nlk:store-open-p)
                      (ignore-errors
                       (nlk:durable-session-parent (nlk:find-session session-id)))))
         (live (%make-live-turn :session-id session-id :turn-id turn-id
                                :parent (and (stringp parent) parent))))
    (setf live.budget
          (handler-case (budget-limits (let ((*live-turn* live)) (turn-budget (turn))))
            (error (condition)
              (warn "the budget of ~a's turn is unreadable: ~a" session-id condition)
              nil)))
    live))

;;; --- folding what the turn spends ---------------------------------------------

(defun note-round (text-p usage &optional window)
  "Fold one provider round into the running turn: the round, the counts
USAGE reported, the context it filled against WINDOW -- the context budget
the round ran under, NIL when nothing knows it -- and TEXT-P, whether the
round said something the operator reads."
  ;; A round without usage leaves the last measured context standing.
  (when-let (live *live-turn*)
    (incf live.rounds)
    (when usage
      ;; each count lands on the LIVE-TURN slot of its PROVIDER-USAGE name
      (dolist (slot '(input-tokens cached-input-tokens output-tokens reasoning-tokens))
        (incf (slot-value live slot) (or (slot-value usage slot) 0)))
      ;; The footer's rule (tui/transcript/ingest.lisp CONTEXT-OCCUPANCY-TOKENS): cache
      ;; writes are prompt tokens too, reported disjoint from input.
      (when-let (prompt (remove nil (list usage.input-tokens usage.cached-input-tokens
                                          usage.cache-write-tokens)))
        (setf live.context-tokens (reduce #'+ prompt))))
    (when (and (integerp window) (plusp window))
      (setf live.context-window window))
    (when text-p
      (setf live.text-at (get-internal-real-time)))))

(defun billable-tokens (live)
  "What LIVE has spent that bills: input, output and reasoning of its
rounds -- never cache reads."
  (+ live.input-tokens live.output-tokens live.reasoning-tokens))

(defun tokens-spent-text (live tokens)
  "LIVE's billable tokens against a budget of TOKENS, `120k of 100k tokens'."
  (format nil "~a of ~a tokens" (format-token-count (billable-tokens live))
          (format-token-count tokens)))

(defun budget-share (live &optional (now (get-internal-real-time)))
  "How much of LIVE's budget is spent, the largest of its shares, or NIL
without a budget."
  (when-let (budget live.budget)
    (destructuring-bind (&key seconds calls tokens) budget
      (max (if seconds (/ (elapsed-ms live.started now) (* 1000 seconds)) 0)
           (cond ((null calls) 0) ((zerop calls) 1) (t (/ live.calls calls)))
           (if tokens (/ (billable-tokens live) tokens) 0)))))

(defun budget-text (live &optional (now (get-internal-real-time)))
  "What LIVE spent against its budget, in words: `10m of 10m', `40 of 40
calls', `120k of 100k tokens'."
  (destructuring-bind (&key seconds calls tokens) live.budget
    (format nil "~{~a~^, ~}"
            (remove nil (list (and seconds
                                   (format nil "~a of ~a"
                                           (nlk::duration-text (/ (elapsed-ms live.started now)
                                                                  1000))
                                           (nlk::duration-text seconds)))
                              (and calls (format nil "~d of ~d calls" live.calls calls))
                              (and tokens (tokens-spent-text live tokens)))))))

(defun meter-call (&aux (live *live-turn*))
  "Count one tool call of the running turn's round, or refuse it: NIL when
it may run, else the text it answers instead."
  ;; A refusal marks the turn closing.
  (cond ((null live) nil)
        ((let ((share (budget-share live))) (and share (>= share 1)))
         (setf live.closing t)
         (format nil "ERROR: refused: this turn has spent its budget (~a). No tool ~
                        call runs from here: answer now with what is done and what is left."
                 (budget-text live)))
        (t (incf live.calls)
           nil)))

(defun turn-closing-p ()
  "True when the running turn already had calls refused for its budget."
  (and *live-turn* (live-turn-closing *live-turn*)))

(defun budget-remaining-ms (&aux (live *live-turn*)
                                 (seconds (and live (getf live.budget :seconds))))
  "Milliseconds the running turn's budget has left, or NIL when no turn is
running or its budget sets no time."
  (and seconds
       (max 0 (- (* 1000 seconds) (elapsed-ms live.started)))))

;;; --- the line a budgeted turn's results carry -------------------------------------

(defun clock-text (&optional (time (get-universal-time)) zone)
  "TIME as the spend line's wall clock, `Wed 2026-09-16 18:37 +02:00': the
weekday, the date, the minute and the UTC offset in effect."
  ;; ZONE, hours west of Greenwich as DECODE-UNIVERSAL-TIME takes it, fixes
  ;; the zone; NIL reads this machine's local time, daylight saving included.
  (bind (((_ minute hour date month year day daylight-p decoded-zone)
          (if zone (decode-universal-time time zone) (decode-universal-time time))))
    (let ((east (- (if (and daylight-p (null zone)) 1 0) decoded-zone)))
      (multiple-value-bind (hours minutes) (floor (round (* (abs east) 60)) 60)
        (format nil "~a ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d ~:[+~;-~]~2,'0d:~2,'0d"
                ;; DECODE-UNIVERSAL-TIME's day of the week, 0 being Monday
                (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day) year month date hour minute
                (minusp east) hours minutes)))))

(defun context-text (live &aux (tokens live.context-tokens) (window live.context-window))
  "The context the newest round of LIVE filled, `context 87k of 1M', the
window left out when nothing knows it, or NIL before a round reported."
  (and tokens
       (format nil "context ~a~@[ of ~a~]"
               (format-token-count tokens)
               (and window (format-token-count window)))))

(defun spend-line (live now &aux (share (budget-share live now)))
  "LIVE's spend as the bracketed line a result carries at NOW, and past
*BUDGET-NEARLY-SPENT* of the budget what to do about it."
  ;; Tokens ride the line only under a token budget.
  (destructuring-bind (&key seconds calls tokens) live.budget
    (format nil "[~{~a~^ · ~}~@[; ~a~]]"
            (remove nil
                    (list (format nil "turn ~a~@[ of ~a~]"
                                  (nlk::duration-text (/ (elapsed-ms live.started now) 1000))
                                  (and seconds (nlk::duration-text seconds)))
                          (format nil "~d round~:p" live.rounds)
                          (if calls
                              (format nil "~d of ~d calls" live.calls calls)
                              (format nil "~d call~:p" live.calls))
                          (and tokens (tokens-spent-text live tokens))
                          (context-text live)
                          (clock-text)))
            (cond ((null share) nil)
                  ((>= share 1)
                   "the budget is spent: the next tool call is refused, so answer with what is done and what is left")
                  ((>= share *budget-nearly-spent*)
                   "the budget is nearly spent: finish, then answer with what is done and what is left")))))

(defparameter *context-line-ratio* 0.5
  "How full the window is before a turn without a budget says so on every
result: `context 95k of 180k'.")

(defparameter *context-line-floor-tokens* 60000
  "The context a result says out loud from, whatever the window is: the
smaller of it and *CONTEXT-LINE-RATIO*'s share of the window binds. A floor
because half a 1M window is 500k, a number no session here reaches, while 60k
is where a session has talk worth dropping; a window small enough for the
ratio to speak first still gets the line there. Whether the line changes how
often a session edits its context was A/B'd and came back flat (T-294), so
what it buys is that a model can read its own fill at all. NIL leaves the
ratio alone.")

(defun context-line-threshold (live)
  "The context tokens past which LIVE's results say how full they are, or NIL
when nothing bounds them: *CONTEXT-LINE-FLOOR-TOKENS*, or the window's share
under *CONTEXT-LINE-RATIO* when that share is smaller."
  (let ((ratio (and (realp live.context-window) (plusp live.context-window)
                    (* *context-line-ratio* live.context-window)))
        (floor *context-line-floor-tokens*))
    (cond ((and ratio floor) (min ratio floor))
          (t (or ratio floor)))))

(defun context-line-due-p (live)
  "Whether LIVE's results say how full the window is: past
CONTEXT-LINE-THRESHOLD, which *CONTEXT-LINE-FLOOR-TOKENS* may lower."
  (let ((threshold (context-line-threshold live)))
    (and live.context-tokens threshold (>= live.context-tokens threshold))))

(defun spend-metadata (metadata &aux (live *live-turn*))
  "METADATA, a tool result's metadata object or NIL, carrying the running
turn's spend line under `spend' when the turn runs under a budget, or how
full the window is once it passes CONTEXT-LINE-THRESHOLD; METADATA itself
outside a turn or with neither."
  ;; The line rides the durable fact beside the result, never inside it: a
  ;; transcript and a channel card read the result, and only the model's copy
  ;; of it grows the line (TOOL-RESULT-TEXT), so every refold sends the bytes
  ;; this round did. A turn without a budget has one limit the line can warn
  ;; of, the window, and only past CONTEXT-LINE-THRESHOLD: a clock on every result
  ;; would cost tokens per call the model would re-read for nothing, and
  ;; a model cannot judge its own length without a hint
  ;; (arXiv 2609.37725, appendix G).
  (if-let (line (cond ((null live) nil)
                      (live.budget (spend-line live (get-internal-real-time)))
                      ((context-line-due-p live)
                       (format nil "[~a]" (context-text live)))))
    (let ((object (if (hash-table-p metadata) metadata (make-json-object))))
      (setf (gethash "spend" object) line)
      object)
    metadata))

(defparameter +seq-line-prefix+ "[seq "
  "What the last line of a tool result opens with: the log position of its
fact, the id a context edit names the result by.")

(defun seq-line (seq spend)
  "The line a tool result ends with on the wire: `[seq 57]', and under a
budget the SPEND line's facts after the seq, in the one bracket."
  (format nil "~a~d~@[ · ~a~]]" +seq-line-prefix+ seq
          (and (stringp spend) (> (length spend) 2) (subseq spend 1 (1- (length spend))))))

(defun tool-result-text (result metadata &key seq)
  "What the model reads for a tool result: RESULT, then on a line of its own
SEQ, the log position of its fact, with the spend line its METADATA carries
-- or, outside a store, that line alone."
  ;; The one rendering, read by the round that records the result and by every
  ;; later fold of the log, so both send the same bytes and the cached prefix
  ;; holds. The seq is what (context :drop ...) names a block by, so every
  ;; result says its own, and the file (RENDER-CONTEXT) reads the result
  ;; without the line: its header names the block already.
  (let ((spend (and (hash-table-p metadata) (gethash "spend" metadata))))
    (cond (seq (format nil "~a~&~a" result (seq-line seq spend)))
          ((and (stringp spend) (plusp (length spend))) (format nil "~a~&~a" result spend))
          (t result))))

(defun without-seq-line (text &aux (at (position #\Newline text :from-end t))
                                   (last (if at (subseq text (1+ at)) text)))
  "TEXT, a tool result as the model read it, without the seq line it ends with."
  (if (and (uiop:string-prefix-p +seq-line-prefix+ last) (uiop:string-suffix-p last "]"))
      (if at (subseq text 0 at) "")
      text))
