;;;; context.lisp --- the context engine: log to provider input, one owner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lisp analog of src/context_engine/ (the harness.md §6 doctrine):
;;;; everything between the durable event log and the provider request lives
;;;; here, staged as an explicit pipeline —
;;;;
;;;;   select   log -> head-visible events        (IO: the one store read)
;;;;   project  events -> wire-shaped messages    (pure fold)
;;;;   unitize  events -> whole-turn units        (pure fold + covered-prefix cache)
;;;;   policy   units -> retained boundary        (pure: hysteresis + forced plans)
;;;;   assemble request -> compiled-turn-context  (pure function of its argument)
;;;;
;;;; COMPILE-TURN-CONTEXT reads no ambient state: every policy fact — frozen
;;;; provider config, tool payloads, base prompt, harness sections — rides in
;;;; an explicit CONTEXT-REQUEST built by the caller (turn.lisp's
;;;; SNAPSHOT-CONTEXT-REQUEST is the one seam that reads the specials). The
;;;; two eviction ratios and the operator default below are this module's own
;;;; policy knobs, not caller state.
;;;;
;;;; Eviction, not compaction: history is dropped whole-turn with an
;;;; addressable stub, never summarized. The durable floor
;;;; (nlk:advance-history-floor) is monotone; the stub tells the model where
;;;; the log keeps what the context no longer carries.

(in-package #:nodecode.evolved)

(nlk:access (event nlk::event) (live-turn nlk::durable-turn))

;;; --- policy knobs ---------------------------------------------------------

(defparameter *context-eviction-trigger-ratio* 0.85
  "Start whole-turn eviction when the estimated context reaches this ratio.")

(defparameter *context-eviction-floor-ratio* 0.50
  "Evict whole turns until the estimated context reaches this ratio.")

(defparameter *live-whole-rounds* 8
  "How many of a live turn's newest rounds a relief leaves whole while
thinning the rounds older than them is enough.")

(defparameter *thinned-result-chars* 300
  "How much of a tool result a thinned round keeps in the window: its head,
up to this many characters. A result no longer than twice this stays whole.")

;;; The Zig default_evict_percent (src/gateway/routes.zig): it is the
;;; operator-command contract, not a tunable — EVICT-SESSION-CONTEXT is its
;;; single owner; the gateway route parse and the local slash op both pass NIL
;;; for "operator default" rather than restating the value.
(defparameter *default-evict-percent* 50
  "The bare-/evict share of retained history to drop.")

(defun context-pressure-decision (estimated budget &aux (estimate (max 0 (or estimated 0))))
  "Classify one request estimate without inventing a provider window."
  ;; Returns a plist with :STATUS (:UNKNOWN, :OK or :PRESSURE), the estimate
  ;; and the known budget/trigger. A NIL BUDGET is deliberately unknown:
  ;; callers may record that fact, but must not evict merely because the
  ;; estimate is large.
  (list :status (cond ((not (and (realp budget) (plusp budget))) :unknown)
                      ((> estimate (* budget *context-eviction-trigger-ratio*)) :pressure)
                      (t :ok))
        :estimated estimate
        :budget budget
        :trigger (and budget (* budget *context-eviction-trigger-ratio*))))

;;; --- JSON copy ------------------------------------------------------------

(defun copy-json-value (value)
  "JSON-shaped data copied all the way down: what a caller takes before it
writes into a value it does not own."
  ;; MAP-JSON-LEAVES is the copying walk; taking each leaf as it comes is the
  ;; whole of the copy, strings included.
  (map-json-leaves value (lambda (leaf path) (declare (ignore path)) leaf)))

;;; --- select: log -> events ------------------------------------------------
;;; One fold rebuilds the session working set. The provider request is that
;;; fold. Display fold-log stays a separate projection of the same log.

(defun collect-session-events (session-id &optional (after 0))
  "Every event for SESSION-ID past position AFTER, oldest first, paging until
the log ends."
  (loop for page = (nlk:page-events :session-id session-id
                                    :after after
                                    :limit nlk::*default-page-size*)
        while page
        append page
        do (setf after (nlk:event-log-position (car (last page))))))

(defun collect-visible-session-events (session-id &optional (after 0))
  "The composed, head-visible event history for SESSION-ID past AFTER."
  ;; Fork lineage contributes first (each ancestor cut at its frozen anchor
  ;; watermark), then the session's own events; both filtered to the current
  ;; head's ancestor chain, with turn-less facts always flowing. Log positions
  ;; are global and ancestor ranges strictly precede descendant ranges, so
  ;; plain concatenation is globally ordered.
  (let ((visible-set (nlk:session-visible-turn-set session-id)))
    (flet ((visible (events)
             (remove-if-not (lambda (event) (nlk:event-visible-p event visible-set)) events)))
      (nconc
       ;; an ancestor's own read stops at its anchor
       (loop for (ancestor nil anchor-position) in (nlk:session-lineage session-id)
             when (< after anchor-position)
               nconc (visible (nlk:events :session-id ancestor :after after
                                          :before (1+ anchor-position))))
       (visible (collect-session-events session-id after))))))

;;; --- project: events -> wire messages -------------------------------------

(defun wire-safe-tool-arguments (arguments)
  "ARGUMENTS when it already reads as one complete JSON value — returned
untouched, because any re-serialization would reorder keys or re-escape
strings and churn the prompt-cache prefix on every later round — otherwise
a deterministic parseable stand-in."
  ;; Strict template engines parse tool-call arguments while rendering
  ;; INCOMING history (llama.cpp routes them through nlohmann); without
  ;; this, a truncated argument left in the journal would 500 every later
  ;; request and brick the session.
  ;; The journal keeps the raw truth; only the wire projection is replaced.
  ;; Blank or missing arguments degrade to the empty object (a no-argument
  ;; call, not a truncation), the same degradation TOOL-CALL-FUNCTION-INPUT
  ;; applies on the lanes that need decoded objects.
  (cond
    ((or (not (stringp arguments))
         (zerop (length (nlk:trimmed arguments))))
     "{}")
    ;; Full-consumption parse: SINGLE-VALUE-P makes shasht error on any
    ;; non-whitespace trailing junk, matching what strict servers enforce.
    ((ignore-errors (nlk:decode-json arguments :whole t) t)
     arguments)
    (t
     (encode-json-object
      (json-object
       ;; bounded, so the stand-in never re-imports the payload an output-length cut truncated
       "truncated_arguments" (subseq arguments 0 (min (length arguments) 256))
       "note" "arguments did not parse as JSON; replaced for replay")))))

;;; Wire text only — never a durable fact: the log keeps the open call open,
;;; and the projection answers it so the wire stays well-formed.
(defparameter +orphan-tool-result+ "no result recorded (interrupted)"
  "The tool message the fold pairs with a call fact that has no result
fact.")

;;; --- thinned rounds --------------------------------------------------------
;;; A live turn that outgrows its window thins its older rounds before any of
;;; them leaves (LIVE-RELIEF-PLAN): each long tool result keeps its head and
;;; one line naming the entry that reads it whole. The call and the note it
;;; opened with stay, so the window keeps what was done in every round and
;;; what was seen in the newest ones. A result is spent in the round after it
;;; lands and dead weight in every round after that; it is cut when it ages,
;;; never when it is new -- a cut at first sight costs the round that re-reads
;;; it (the 8,000-character eval ceiling, raised for that, turn.lisp
;;; *EVAL-RESULT-LIMIT*). And no round a relief finds standing replays its
;;; reasoning past it: a signed thinking block is bound to the window it was
;;; signed in (provider.lisp ANTHROPIC-MESSAGES), every relief rewrites that
;;; window, and a request that drops every block signed before the rewrite and
;;; replays those signed after it is the shape the API was probed to accept.
;;; The log keeps every result and every thought whole; the cut is the
;;; fold's, by the turn's newest turn.context.evicted fact, so every refold
;;; sends the same bytes.

(defun thin-result-p (text)
  "Is TEXT, a tool result, long enough that thinning it saves the window
anything?"
  (> (length text) (* 2 *thinned-result-chars*)))

(defun thinned-result-text (text position)
  "What a thinned round keeps of TEXT, the tool result fact at POSITION: its
head -- cut at a line end inside *THINNED-RESULT-CHARS* when one falls past
the middle of it -- and the recall that reads the whole."
  (let* ((limit *thinned-result-chars*)
         (newline (position #\Newline text :end limit :from-end t))
         (end (if (and newline (> newline (floor limit 2))) newline limit)))
    (format nil "~a~:[…~;~]~%[thinned from ~:d characters; (getf (recall-entry ~d) :text) reads it whole]"
            (subseq text 0 end) (eql end newline) (length text) position)))

(defun without-reasoning (message)
  "MESSAGE, a wire message its caller owns, with every reasoning key gone:
a round older than its turn's newest relief replays what it did, not how it
thought."
  (dolist (key (loop for key being the hash-keys of message
                     when (and (stringp key) (eql 0 (cl:search "reasoning" key)))
                       collect key)
               message)
    (remhash key message)))

;;; Which fact each folded message stands for. The fold names every message it
;;; builds: by the log position of the fact that carried it, or, for a message
;;; the fold makes itself, by the position it answers for and a suffix -- c a
;;; call flush, o the answer to a call that recorded none, s a turn's eviction
;;; stub. A model's note (working-context.lisp) reads :NOTE. The working
;;; context names its blocks by these, and a note never opens a round
;;; (HISTORY-TURN-ROUND-SPLIT). Weak keys: an entry dies with its message.
(nlk:define-side-table message-source (:weakness :key :synchronized t)
  "MESSAGE -> the log position or derived id of what it stands for, or :NOTE.")

(defun background-shell-exit-fact-p (event)
  "Whether EVENT is a background shell's exit fact (RECORD-BACKGROUND-SHELL-EXIT):
display material, which the wire fold reads as nothing."
  (and (typep event 'nlk:tool-result-event)
       (zerop (length (nlk:tool-result-event-result event)))
       (json-value (nlk::tool-result-event-metadata event) :object "background_shell")
       t))

(defun follow-up-p (event)
  (equal "follow-up" (json-value (nlk::message-event-payload event) :string "disposition")))

(defun wire-messages-from-events (events &key (thinned-through 0) (unreasoned-before 0))
  "Reconstruct provider-shaped messages from durable conversation facts: the
results at or below THINNED-THROUGH thinned, and no reasoning on an assistant
message below UNREASONED-BEFORE."
  ;; Returns (values MESSAGES CLEAN-P): CLEAN-P is true when the fold ended
  ;; with no pending unflushed tool calls, which is what licenses a cache to
  ;; cut the log at this point and fold only the tail next time.
  ;;
  ;; A tool result folds into context only when this fold has already emitted
  ;; its pairing assistant tool call — inside a stored assistant message's
  ;; tool_calls or a flushed synthetic one. Tool dispatch journals whatever
  ;; call id it is handed (EXECUTE-TOOL-CALL invoked directly from a EVAL form
  ;; lands real call/result facts under an id no assistant message issued),
  ;; and an unpaired result would replay as an orphan function_call_output /
  ;; tool_result and be hard-rejected by providers on every later round — one
  ;; fabricated fact would otherwise brick the session for good.
  ;;
  ;; The converse holds with the same force: every call this fold emits is
  ;; paired with a tool message before anything else follows it. A call fact
  ;; whose result never recorded — Esc or a process death between the call
  ;; fact and the result fact, the +ORPHAN-TOOL-RESULT+ window — folds as a
  ;; tool message saying so; without that tool message, an assistant
  ;; tool_calls message with a missing tool reply would be rejected by every
  ;; provider on every later round. The pairing is the projection's contract,
  ;; not a repair: the log keeps the open call as the fact it is, and the wire
  ;; says what the log says — the call happened and no result was recorded.
  ;;
  ;; Tool-call arguments leave the fold wire-safe (WIRE-SAFE-TOOL-ARGUMENTS,
  ;; on both the synthetic flush and the stored-message copy): a journaled
  ;; call whose arguments do not parse as JSON is the same replay poison as an
  ;; unpaired result, just rejected by the template engine instead of the API.
  (let ((result '())
        (pending-calls '())
        (issued-call-ids (make-hash-table :test #'equal))
        ;; (CALL-ID . TOOL-NAME) of every issued call still awaiting its
        ;; tool message, newest first.
        (open-calls '())
        (stored-tool-calls-p nil)
        ;; Each call fact's position by call id: what an unanswered call's
        ;; answer is named by (MESSAGE-SOURCE).
        (call-positions (make-hash-table :test #'equal))
        ;; Reminders met while a call stood open, newest first, each with its
        ;; fact's position: each follows the tool messages that close it,
        ;; never splits a call from them.
        (held-reminders '())
        ;; Ids this fold named for calls journaled with an empty one, oldest
        ;; first: what the empty-id results that follow answer, in order.
        (blank-call-ids '()))
    (labels ((emit (msg source)
               (setf (message-source msg) source)
               (push msg result))
             (named-call-id (call-id position k)
               ;; A call journaled with an empty id -- a provider that sent
               ;; none, before the lane minted one (GH #32) -- would replay as
               ;; "", and a strict endpoint refuses the whole history: the
               ;; session bricked for good. It folds named by its fact's
               ;; position, the same name on every fold, so a cached prefix
               ;; holds; its result, journaled under "" too, answers it in order.
               (if (and (stringp call-id) (plusp (length call-id)))
                   call-id
                   (let ((named (format nil "call-~a-~d" position k)))
                     (setf blank-call-ids (append blank-call-ids (list named))
                           (gethash named call-positions) position)
                     named)))
             (note-issued (call-id tool-name)
               (when (stringp call-id)
                 (unless (shiftf (gethash call-id issued-call-ids) t)
                   (push (cons call-id (if (stringp tool-name) tool-name "unknown"))
                         open-calls))))
             (close-open-calls ()
               ;; Every issued call answers before the next message: a
               ;; call with no recorded result answers that it has none.
               (flush-pending-calls)
               (dolist (open (reverse open-calls))
                 (emit (message "tool" +orphan-tool-result+
                                :name (cdr open) :tool-call-id (car open))
                       (format nil "~ao" (gethash (car open) call-positions (car open)))))
               (setf open-calls '()
                     blank-call-ids '())
               (release-reminders))
             (release-reminders ()
               (loop for (text . position) in (reverse held-reminders)
                     do (emit (message "system" text) position))
               (setf held-reminders '()))
             (flush-pending-calls ()
               (when (and pending-calls (not stored-tool-calls-p))
                 (emit (message
                        "assistant" :null
                        :tool-calls
                        (map 'vector
                             (lambda (event &aux (call-id (named-call-id
                                                           (nlk:tool-call-event-call-id event)
                                                           (nlk:event-log-position event) 0))
                                                 (tool-name (nlk:tool-call-event-tool-name event)))
                               (note-issued call-id tool-name)
                               (chat-tool-call-object call-id tool-name
                                                      (wire-safe-tool-arguments
                                                       (nlk:tool-call-event-arguments event))))
                             (reverse pending-calls)))
                       (format nil "~dc" (nlk:event-log-position (car (last pending-calls))))))
               ;; Stored assistant wire messages already contain these calls.
               (setf pending-calls '())))
      (dolist (event events)
        (typecase event
          (nlk::user-message
           ;; a queued follow-up folds once DEQUEUE-NEXT-TURN replays it as its own turn, not twice
           (unless (follow-up-p event)
             (close-open-calls)
             (emit (message "user" (nlk::message-event-content event)) event.log-position)))
          (nlk::assistant-message
           (close-open-calls)
           (setf stored-tool-calls-p nil)
           (let* ((stored-msg (json-value (nlk::message-event-payload event)
                                          :object "message"))
                  (msg (if stored-msg
                           (let* ((copy (copy-json-value stored-msg))
                                  (calls (gethash "tool_calls" copy)))
                             (setf stored-tool-calls-p (vectorp calls))
                             (loop for call across (if (vectorp calls) calls #())
                                   for k from 0
                                   for function = (json-value call :object "function")
                                   when (hash-table-p call)
                                     do (note-issued
                                         (setf (gethash "id" call)
                                               (named-call-id (gethash "id" call)
                                                              event.log-position k))
                                         (json-value call :string "function" "name"))
                                   ;; the fold's own copy is made wire-safe in place
                                   when function
                                     do (setf (gethash "arguments" function)
                                              (wire-safe-tool-arguments (gethash "arguments" function))))
                             copy)
                           (let ((content (nlk::message-event-content event)))
                             (message "assistant"
                                      (if (string= content "") :null content))))))
             (when (< event.log-position unreasoned-before)
               (without-reasoning msg))
             (emit msg event.log-position)))
          (nlk:tool-call-event
           (setf (gethash (nlk:tool-call-event-call-id event) call-positions) event.log-position)
           (push event pending-calls))
          ;; A prompt change stands where it was noticed, as the harness's
          ;; own message: the turn's opening one, or behind the round a
          ;; resumed turn had reached (TURN-PROMPT).
          (nlk:turn-context-prompt-event
           (let ((text (nlk:turn-context-prompt-text event)))
             (when (and (stringp text) (plusp (length text)))
               (push (cons text event.log-position) held-reminders)
               (unless (or pending-calls open-calls)
                 (release-reminders)))))
          (nlk:tool-result-event
           ;; A background-shell exit fact (RECORD-BACKGROUND-SHELL-EXIT) is display
           ;; material: folded, it would pair the run call's tool_use id twice.
           (unless (background-shell-exit-fact-p event)
             (flush-pending-calls)
             (let ((call-id (let ((journaled (nlk:tool-result-event-call-id event)))
                              (if (and (equal journaled "") blank-call-ids)
                                  (pop blank-call-ids)
                                  journaled))))
               (when (gethash call-id issued-call-ids)
                 (setf open-calls (remove call-id open-calls
                                          :key #'car :test #'equal :count 1))
                 ;; The model's copy: the result, then the spend line its
                 ;; metadata carries -- the bytes the round that recorded
                 ;; it sent (meter.lisp TOOL-RESULT-TEXT) -- or, once its
                 ;; round is thinned, the head that names the whole.
                 (emit (message "tool"
                                (let ((text (nlk:tool-result-event-result event)))
                                  (if (and (<= event.log-position thinned-through)
                                           (thin-result-p text))
                                      (thinned-result-text text event.log-position)
                                      (tool-result-text
                                       text (nlk::tool-result-event-metadata event)
                                       :seq event.log-position)))
                                :name (nlk:tool-result-event-tool-name event)
                                :tool-call-id call-id)
                       event.log-position)
                 (unless open-calls (release-reminders))))))))
      ;; A fold that ends on an open call is not a cut point: the result
      ;; may still record, and a cached prefix closing the call would then
      ;; drop the real result as unpaired on the next tail fold.
      (let ((clean-p (and (null pending-calls) (null open-calls))))
        (close-open-calls)
        (values (nreverse result) clean-p)))))

;;; --- unitize: events -> whole-turn units ----------------------------------

(defstruct (history-turn (:constructor make-history-turn))
  (turn-id "" :type string)
  (start-sequence 0 :type (integer 0))
  (end-sequence 0 :type (integer 0))
  (messages '() :type list)
  (terminal-p nil :type boolean)
  ;; Within-turn eviction state (the F1 redesign): the chronological log
  ;; positions of the unit's RETAINED assistant messages — the round
  ;; boundaries a live-turn watermark advances across — plus the cumulative
  ;; counters the newest turn.context.evicted fact carried, so the next
  ;; advance extends them instead of re-deriving from evicted events.
  (round-starts '() :type list)
  ;; The newest fact's two watermarks: events at or below EVICTED-THROUGH
  ;; left the window, and those at or below THINNED-THROUGH fold thinned.
  (evicted-through 0 :type (integer 0))
  (thinned-through 0 :type (integer 0))
  (evicted-start-sequence 0 :type (integer 0))
  (evicted-round-count 0 :type (integer 0))
  (evicted-message-count 0 :type (integer 0))
  ;; (POSITION NAME DISPLAY) triples for the organism.definition_recorded
  ;; and definition_forgotten facts that landed during this turn's span,
  ;; oldest first — what an eviction stub projects as surviving state. A
  ;; forgotten name rides as a tombstone: DISPLAY NIL.
  (definitions '() :type list)
  ;; (POSITION LINE) pairs for the snippets whose result facts carry a record —
  ;; the note the snippet opened with — oldest first: what an eviction stub
  ;; carries of the evicted rounds' conclusions.
  (notes '() :type list)
  ;; (POSITION FORM DISPLAY) triples for what the watched snippets showed
  ;; (turn.lisp WATCHED-TEXT), oldest first, a closed one as DISPLAY NIL:
  ;; what an eviction stub carries of the state the model was watching.
  (watched '() :type list)
  ;; Lazily memoized message token estimate, and the character count it is
  ;; taken from. Turn units are immutable once grouped, and cached units
  ;; survive across rounds, so each closed turn serializes for estimation
  ;; once per image, ever — and an open unit extended round by round
  ;; (EXTEND-HISTORY-TURN) carries the count forward and measures only the
  ;; round that arrived.
  (token-estimate nil :type (or null integer))
  (message-chars nil :type (or null (integer 0)))
  ;; T when the fold that built this unit ended at a cut point: no tool call
  ;; still awaiting its result. Only such a unit may be pinned and extended —
  ;; an open call's result records later, and a pinned prefix that already
  ;; answered the call for it would drop the real result as unpaired.
  (clean-p nil :type boolean))

(nlk:access (last history-turn) (tail history-turn) (turn history-turn) (unit history-turn))

(defun json-array-chars (values)
  "The length of VALUES' JSON array text, measured value by value: the
brackets, a comma between neighbours, and each value's own text."
  ;; Equal to the length of the whole list's own encoding — the empty list
  ;; included, which shasht writes as the false literal rather than as an
  ;; empty array — and the only measure of a message list an extended unit can
  ;; carry forward.
  (if (null values)
      (length (encode-json-object values))
      (+ 2 (1- (length values))
         (loop for value in values sum (length (encode-json-object value))))))

(defun history-turn-chars (turn)
  "The length of TURN's messages as JSON array text, measured once per unit."
  (or turn.message-chars (setf turn.message-chars (json-array-chars turn.messages))))

(defun history-turn-tokens (turn)
  "TURN's memoized message token estimate."
  (or turn.token-estimate (setf turn.token-estimate (ceiling (history-turn-chars turn) 4))))

(defun terminal-turn-event-p (event)
  (not (null (member event.kind '("turn.completed" "turn.failed" "turn.cancelled") :test #'equal))))

;;; --- the within-turn eviction watermark -------------------------------------
;;; The session floor stays whole-turn; an oversized LIVE turn evicts its own
;;; oldest rounds instead, at round boundaries recorded as turn.context.evicted
;;; facts riding the same turn-id — so the watermark arrives with the events it
;;; thins, replay-deterministically, and a turn that outgrew its window once
;;; can never resurrect the evicted rounds by growing past the session floor
;;; (adversarial review F1). The opening user input always survives: a
;;; compiled request without any user message is a hard template rejection on
;;; strict chat templates (the Qwen no-user-query 500).

;;; --- what evicted rounds leave behind ----------------------------------------
;;; Eviction drops trajectory, never state: a definition the organism recorded
;;; while those rounds ran is still live in the image, and the stub that
;;; replaces the rounds names it. Without this the model would re-derive what
;;; it already built. The clause is a pure projection of
;;; organism.definition_recorded and definition_forgotten facts already in the
;;; log — no model call, no judgement of rounds — so the durable refold
;;; reproduces it byte-identically.

(defun definition-lambda-list (text)
  "The lambda-list substring of a defining form's exact source TEXT, or NIL:
the first balanced group after the operator, the name (a symbol or a
(SETF name) group), and any method qualifiers."
  ;; The reader under *READ-SUPPRESS* walks each form's syntax -- strings,
  ;; escapes, character names, nested groups -- and interns and evaluates
  ;; nothing, so the stub never does either; any text it cannot follow
  ;; degrades to NIL, the bare-name display.
  (ignore-errors
   (let ((*read-suppress* t))
     (flet ((after (start)
              (nth-value 1 (read-from-string text t nil :start start :preserve-whitespace t))))
       (when (char= (char text 0) #\()
         (loop with i = (after (after 1))           ; past the operator and the name
               for start = (position-if-not (lambda (c) (find c nlk::+scribe-whitespace+))
                                            text :start i)
               do (case (char text start)            ; qualifiers, then the group
                    (#\( (return (subseq text start (after start))))
                    (#\) (return nil))
                    (#\; (setf i (position #\Newline text :start start)))
                    (t (setf i (after start))))))))))

(defun definition-display (operator name text)
  "How an eviction stub names one recorded definition: function-defining
operators carry the lambda list scanned from the exact source, everything
else the bare name, all downcased — the shape the model would type."
  (let* ((name (string-downcase name))
         (group (and (member operator '("DEFUN" "DEFMACRO" "DEFGENERIC" "DEFMETHOD")
                             :test #'string-equal)
                     (definition-lambda-list text)))
         (inner (and group (>= (length group) 2)
                     (let* ((collapsed (nlk:one-line
                                        (subseq group 1 (1- (length group)))))
                            ;; &AUX names a definition's own bindings, not a
                            ;; parameter the caller passes; the stub says how
                            ;; the name is called, so the lambda list ends there.
                            (collapsed (ppcre:regex-replace "(?is)\\s*&aux.*" collapsed "")))
                       (and (plusp (length collapsed)) collapsed)))))
    (stub-clip (if group (format nil "(~a~@[ ~a~])" name inner) name) 120)))

(defun standing-clause (label entries)
  "The `LABEL: ...` clause naming what ENTRIES — (POSITION NAME DISPLAY)
triples, oldest first — left standing in the image: newest entry of each
name wins, newest first, a tombstone (DISPLAY NIL) as the newest entry drops
the name, capped with an explicit +N more. NIL when empty. The definitions
the evicted rounds recorded stand as `defined:', the values the snippets they
watched last showed as `watching:'."
  (when entries
    (let* ((newest (remove-duplicates (reverse entries) :key #'second :test #'equal :from-end t))
           (picked (remove nil (mapcar #'third newest)))
           (total (length picked))
           ;; 24 named, the rest disclosed: the no-silent-caps rule applied to the stub itself
           (shown (subseq picked 0 (min total 24))))
      (when shown
        (format nil "~a: ~{~a~^ · ~}~@[ +~d more~]"
                label shown
                (and (> total (length shown)) (- total (length shown))))))))

;;; --- what the evicted rounds concluded ---------------------------------------
;;; A snippet's result fact carries, under its metadata's "snippet" key, the note
;;; the snippet opened with (SNIPPET-RECORD, turn.lisp). Eviction drops the round
;;; that carried it; the stub keeps one line per such snippet, oldest first, so
;;; what the model concluded before it acted outlives the observation it
;;; concluded it from. PRO-LONG's log entry (the agent's plan, the action, the
;;; outcome) is a projection of facts already in the log — no model call,
;;; byte-stable on every refold. Bounded like the definitions clause: the
;;; newest lines stand, the older are counted with the recall that reaches
;;; them, and the seq on every line is what RECALL-ENTRY reads whole.

(defparameter *eviction-stub-note-limit* 100
  "How many snippet lines an eviction stub carries before disclosing the older
ones as an explicit +N earlier.")

(defparameter *eviction-stub-line-width* 320
  "How long one snippet line of an eviction stub may run.")

(defun stub-clip (text width &optional (mark "…"))
  "TEXT cut to WIDTH characters with a disclosing ellipsis, or TEXT whole."
  ;; MARK is the ellipsis: a plain-text table's is `~'.
  (if (> (length text) width)
      (concatenate 'string (subseq text 0 (1- width)) mark)
      text))

(defun snippet-digest-line (position record &aux (note (gethash "note" record)))
  "One stub line for the snippet whose result fact at POSITION carries RECORD:
the seq, then the note."
  (stub-clip (format nil "seq ~d~@[ · ~a~]" position (and (stringp note) (string/= note "") note))
             *eviction-stub-line-width*))

(defun notes-block (notes)
  "The block an eviction stub carries for NOTES — (POSITION LINE) entries,
oldest first — or NIL when there are none: the newest
*EVICTION-STUB-NOTE-LIMIT* lines, one per line, the older ones disclosed as
+N earlier with the recall that reaches them."
  (when notes
    (let* ((total (length notes))
           (shown (last notes (min total *eviction-stub-note-limit*)))
           (earlier (- total (length shown)))
           (disclosure (and (plusp earlier)
                            (format nil "; +~d earlier: (recall-log :before ~d)"
                                    earlier (first (first shown))))))
      (format nil "[the evicted snippets noted and judged, oldest first; (recall-entry seq) reads one whole~@[~a~]~{~% ~a~}]"
              disclosure (mapcar #'second shown)))))

;;; --- what the model wrote for itself before the rounds left -----------------
;;; The notes are a diary: one line per snippet, written before its outcome,
;;; capped at the newest lines. Still a pure projection of facts in the log --
;;; no model call in the fold -- so the refold is byte-identical.

(defun turn-eviction-stub (start-sequence watermark round-count
                           &optional defs notes watched)
  "The in-window stand-in for a turn's evicted rounds — same typography and
recall road sign as the whole-turn stub, and the same rule: never name a
tool that does not exist."
  ;; DEFS and NOTES come as the turn holds them; the stub takes the ones at
  ;; or below WATERMARK, the ones the evicted range owns.
  ;; DEFS (definition triples the evicted rounds recorded) surface as the
  ;; defined: clause — evicted trajectory, surviving state; WATCHED (what
  ;; their watched snippets showed) as the watching: clause, the state they
  ;; watched as it last read; and NOTES (the evicted snippets' record entries)
  ;; as the block under it: evicted observations, surviving conclusions.
  (flet ((evicted (entries) (remove watermark entries :test #'< :key #'first)))
    (message "system"
             (format nil
                     "[turn history evicted: ~d round~:p · seq ~d–~d~@[ · ~a~]~@[ · ~a~] · recall: ~a]~@[~%~a~]"
                     round-count start-sequence watermark
                     (standing-clause "defined" (evicted defs))
                     (standing-clause "watching" (evicted watched))
                     (recall-example (1- start-sequence) (1+ watermark))
                     (notes-block (evicted notes))))))

;;; --- the prefix invariant --------------------------------------------------
;;; A message the fold has sent once is never rewritten while it stays in
;;; context, except at a relief: one turn.context.evicted fact, which rewrites
;;; the window once and then holds still. reasoning_content rides with its
;;; assistant message under the message's own horizon, signed or not, until
;;; a relief finds the round standing (thinned rounds, above). Prompt caching
;;; is a prefix match over the rendered request, and the
;;; OpenAI-family upstreams render a tool-call chain as one unit: change one
;;; round's reasoning and the chain re-renders from the first user message.
;;; Without this, shedding unsigned reasoning one round after it streamed
;;; would make every request diverge at the first assistant message, so
;;; cached_tokens would stay at the system prompt, the tools and the ask
;;; while the prompt grows. Keeping the reasoning attached caches the whole
;;; previous request, and the budget policy sees that weight in the estimates
;;; it evicts against.

(defun make-history-turn-from-reversed-events (turn-id events definition-events)
  "Build TURN-ID's unit from EVENTS, newest first."
  ;; The newest turn.context.evicted fact shapes the fold: its watermark drops
  ;; the rounds at or below it, its thinned watermark thins the results at or
  ;; below that, and the rounds older than the fact replay no reasoning. The
  ;; cumulative counters ride the unit so a later advance
  ;; extends them. DEFINITION-EVENTS are the turn-less definition facts that
  ;; landed during this turn's span, oldest first — they ride the unit so
  ;; eviction stubs can project them.
  (bind ((chronological (reverse events))
         (terminal-p (some #'terminal-turn-event-p events))
         ;; (POSITION NAME DISPLAY) per recorded name, a tombstone (DISPLAY NIL) per forgotten one
         (defs (loop for event in definition-events
                     for position = event.log-position
                     append (typecase event
                              (nlk:definition-recorded-event
                               (let* ((names (nlk:definition-recorded-event-names event))
                                      (name (and (plusp (length names)) (aref names 0))))
                                 (when (stringp name)
                                   (list (list position name
                                               (definition-display
                                                (nlk:definition-recorded-event-operator event)
                                                name
                                                (nlk:definition-recorded-event-text event)))))))
                              (nlk:definition-forgotten-event
                               (loop for name across (nlk:definition-forgotten-event-names event)
                                     when (stringp name)
                                       collect (list position name nil))))))
         (notes (loop for event in chronological
                      for record = (and (typep event 'nlk:tool-result-event)
                                        (json-value (nlk::tool-result-event-metadata event)
                                                    :object "snippet"))
                      when record
                        collect (let ((position event.log-position))
                                  (list position (snippet-digest-line position record)))))
         ;; What each result's watched snippets showed, as the stub names it:
         ;; the form and its value, or a tombstone for one closed.
         (watched (loop for event in chronological
                        when (typep event 'nlk:tool-result-event)
                          append (loop for shown across (json-array (nlk::tool-result-event-metadata event)
                                                                    "watched")
                                       for form = (json-value shown :string "form")
                                       for value = (json-value shown :string "value")
                                       when form
                                         collect (list event.log-position form
                                                       (and value (stub-clip (format nil "~a = ~a" form value)
                                                                             *eviction-stub-line-width*))))))
         ;; EVENTS run newest first, so the first hit is the newest fact —
         ;; the one the stub renders from.
         (watermark-event (find-if (lambda (event)
                                     (typep event 'nlk:turn-context-evicted-event))
                                   events))
         (evicted (lambda (reader) (if watermark-event (funcall reader watermark-event) 0)))
         (watermark (funcall evicted #'nlk:turn-context-evicted-watermark))
         (thinned (funcall evicted #'nlk:turn-context-evicted-thinned))
         (start (funcall evicted #'nlk:turn-context-evicted-start-sequence))
         (round-count (funcall evicted #'nlk:turn-context-evicted-round-count))
         (folded (if (plusp watermark)
                     ;; events at or below WATERMARK drop, the opening user input excepted
                     (loop with anchor-kept = nil
                           for event in chronological
                           when (or (> event.log-position watermark)
                                    (typecase event
                                      (nlk::user-message
                                       (and (not (follow-up-p event))
                                            (not anchor-kept)
                                            (setf anchor-kept t)))
                                      ((or nlk::assistant-message nlk:tool-call-event
                                           nlk:tool-result-event)
                                       nil)
                                      (t t)))
                             collect event)
                     chronological))
         ((messages clean-p) (wire-messages-from-events
                              folded
                              :thinned-through thinned
                              :unreasoned-before (funcall evicted #'nlk:event-log-position))))
    ;; The stub stands for rounds that left; a fact that only thinned has
    ;; none, and each thinned result names its own way back.
    (when (plusp round-count)
      (let ((stub (turn-eviction-stub start watermark round-count defs notes watched)))
        ;; Named for what it stands behind: the anchor it is spliced after.
        (setf (message-source stub)
              (format nil "~as" (or (message-source (first messages))
                                    (nlk:event-log-position (first chronological))))
              ;; spliced after the leading user message, else prepended: the stub reads
              ;; as history annotation, never as the task
              messages (if (user-role-message-p (first messages))
                           (list* (first messages) stub (rest messages))
                           (cons stub messages)))))
    (make-history-turn
     :turn-id turn-id
     :start-sequence (nlk:event-log-position (first chronological))
     :end-sequence (nlk:event-log-position (car (last chronological)))
     :messages messages
     :round-starts (loop for event in folded
                         when (typep event 'nlk::assistant-message)
                           collect event.log-position)
     :evicted-through watermark
     :thinned-through thinned
     :evicted-start-sequence start
     :evicted-round-count round-count
     :evicted-message-count (funcall evicted #'nlk:turn-context-evicted-message-count)
     :definitions defs
     :notes notes
     :watched watched
     :clean-p clean-p
     :terminal-p terminal-p)))

(defun record-with (constructor record &rest changes)
  "RECORD built again by CONSTRUCTOR, its keyword constructor, with CHANGES --
slot keywords and their values -- in those slots, and every other slot as
RECORD holds it."
  ;; The first of two same keywords wins an argument list, so CHANGES, in
  ;; front, outrank RECORD's own slots behind them.
  (apply constructor
         (append changes
                 (loop for slot in (sb-mop:class-slots (class-of record))
                       for name = (sb-mop:slot-definition-name slot)
                       append (list (intern (string name) :keyword) (slot-value record name))))))

(defun extend-history-turn (unit tail)
  "UNIT extended by TAIL, the same turn's next batch of folded events: one
unit carrying UNIT's own message objects followed by TAIL's."
  ;; The identity is the point. A turn's earlier rounds are the bytes the
  ;; provider has already prefilled, and the memo remembers a message's
  ;; encoding by EQ (JSON-MEMO), so a round that rebuilds its predecessors as
  ;; fresh objects re-encodes the whole turn to send what it sent last round.
  ;; Extended here, round N pays for round N alone: the fold pages three new
  ;; facts, the measure adds three messages to a count it carried, and the
  ;; body build finds every earlier message's verbatim.
  ;;
  ;; Sound only at a cut point (HISTORY-TURN-CLEAN-P on UNIT, which
  ;; HISTORY-TURNS-FOR-SESSION checks before it pins): TAIL's fold pairs a
  ;; tool result with a call in its own batch, and a result whose call stood
  ;; open across the cut would fold as an orphan and be dropped. An eviction
  ;; inside the unit is not an extension at all — the watermark rewrites
  ;; messages already sent — and the caller refolds the unit whole instead.
  ;; The turn's identity and its eviction counters are UNIT's own.
  (record-with
   #'make-history-turn unit
   :end-sequence tail.end-sequence
   :messages (append unit.messages tail.messages)
   :round-starts (append unit.round-starts
                         tail.round-starts)
   :definitions (append unit.definitions
                        tail.definitions)
   :notes (append unit.notes tail.notes)
   :watched (append unit.watched tail.watched)
   :clean-p tail.clean-p
   :terminal-p tail.terminal-p
   ;; UNIT's estimate measured fewer messages: taken again when asked
   :token-estimate nil
   ;; The measure extends with the messages: the unit's own count plus
   ;; each new message's text and the comma before it -- TAIL's own array
   ;; text less the bracket the two arrays share. A side with no messages
   ;; leaves the other's measure, taken when asked.
   :message-chars (cond ((null tail.messages) unit.message-chars)
                        (unit.messages
                         (+ (history-turn-chars unit) (json-array-chars tail.messages) -1)))))

(defun history-turns-from-events (events &aux (groups '())
                                              (current '())
                                              (current-id nil)
                                              (pending-defs '()))
  "Group durable conversation facts into source-addressed complete turn units."
  ;; Turn-less definition facts attach to the turn whose span holds them — the
  ;; newest turn opened at or before the fact — so an eviction stub can name
  ;; what the dropped rounds defined; ones recorded before any turn ride the
  ;; first.
  (flet ((close-group ()
           (when current
             (push (make-history-turn-from-reversed-events
                    current-id current (nreverse pending-defs))
                   groups)
             (setf current '() pending-defs '()))))
    (dolist (event events)
      (cond
        ;; A scribe provenance fact — a definition recorded or forgotten —
        ;; rides beside the turn it landed in.
        ((typep event '(or nlk:definition-recorded-event
                        nlk:definition-forgotten-event))
         (push event pending-defs))
        ;; A background exit fact folds to nothing and lands whenever its shell
        ;; ends, often inside a later turn's tool round. Grouped, it would cut
        ;; that turn into two units folded apart: the second refolds a call the
        ;; first already issued as a fresh synthetic call, and the provider
        ;; refuses the duplicate call id on every later round of the session.
        ((background-shell-exit-fact-p event))
        (t
         (when-let (turn-id event.turn-id)
           (when (and current (not (equal turn-id current-id)))
             (close-group))
           (setf current-id turn-id)
           (push event current)))))
    (close-group))
  (nreverse groups))

;;; --- hydration cache -------------------------------------------------------
;;; The session_runtime_snapshots distillation, sized to this runtime: what
;;; hydration re-pays every round is re-paging and re-materializing what it
;;; folded last round, so the cache holds the turn units no later fact can
;;; change — every unit but the last, and the last once it is terminal or up
;;; to the last cut point its fold reached — pinned to a covered watermark,
;;; and each round folds only the facts that arrived since.
;;; Validity is the Zig covered-prefix recheck — one aggregate query whose
;;; count and max position must both match what the cache recorded; any
;;; mismatch (a different database, replayed history) drops the entry and
;;; refolds from zero. Cache only: the log remains the sole authority, and a
;;; cleared cache is always correct, merely slower.
;;;
;;; It holds the sessions a turn is running on and the few idle ones hydrated
;;; last. Without that bound, the cache would hold every session the image has
;;; hydrated for the life of the image, and session histories would grow the
;;; heap.

(defvar *hydration-cache* (make-hash-table :test #'equal :synchronized t)
  "session-id -> (WATERMARK EVENT-COUNT HEAD-STAMP PINNED-TURNS).")

;;; A session with a turn in flight stays whatever this says and is not
;;; counted; a session let go refolds its log at its next hydration.
(defparameter *warm-sessions* 2
  "How many idle sessions keep their covered prefix, the most recently
hydrated first.")

(defvar *hydration-order* '()
  "Session ids the cache was read for, the most recent first.")

(defun clear-hydration-cache ()
  (sb-ext:with-locked-hash-table (*hydration-cache*)
    (clrhash *hydration-cache*)
    (setf *hydration-order* '())))

(defun invalidate-session-hydration-cache (session-id)
  "Drop SESSION-ID's covered prefix."
  ;; Mandatory after a head move: the move appends ABOVE any pinned watermark,
  ;; so the coverage recheck alone still matches while the visible set below
  ;; it has changed — the HEAD-STAMP element catches that lazily, this catches
  ;; it eagerly.
  (remhash session-id *hydration-cache*))

(defun note-hydrated (session-id)
  "SESSION-ID was just hydrated: it leads *HYDRATION-ORDER*, and every idle
session behind the *WARM-SESSIONS* most recent leaves the cache."
  (flet ((in-flight-p (id &aux (session (sb-ext:with-locked-hash-table (*session-registry*)
                                          (gethash id *session-registry*))))
           (and session session.in-flight-p)))
    (sb-ext:with-locked-hash-table (*hydration-cache*)
      (let ((idle 0))
        (setf *hydration-order*
              (loop for id in (cons session-id
                                    (remove session-id *hydration-order* :test #'equal))
                    if (or (in-flight-p id) (<= (incf idle) *warm-sessions*))
                      collect id
                    else
                      do (remhash id *hydration-cache*)))))))

;; A reopened store is a different history. A named function under PUSHNEW
;; keeps image reloads from stacking duplicate hooks.
(pushnew 'clear-hydration-cache nlk::*store-open-hooks*)

(defun covered-prefix (session-id head-stamp &aux (entry (gethash session-id *hydration-cache*)))
  "SESSION-ID's still-valid cache entry, or NIL: the pinned head stamp still
stands, and one aggregate query's count and max position both match what the
entry recorded."
  ;; Any mismatch (a different database, replayed history) drops the entry,
  ;; and the caller refolds from zero.
  (and entry
       (= head-stamp (third entry))
       (multiple-value-bind (count max-position)
           (nlk:session-replay-coverage session-id (first entry))
         (and (= count (second entry)) (= max-position (first entry))))
       entry))

(defun pin-folded-prefix (session-id head-stamp watermark turns)
  "Pin the TURNS a later round may reuse as they stand: every unit another
turn's unit followed, and the last when it is terminal or its own fold ended
at a cut point (HISTORY-TURN-CLEAN-P)."
  ;; WATERMARK is what the cache already holds, so an unchanged prefix is not
  ;; re-pinned.
  ;;
  ;; The fold groups a turn's CONSECUTIVE facts, so a unit another turn's
  ;; followed is final whether or not it saw its turn's terminal fact: a later
  ;; fact of its turn opens a unit of its own, in a fold from zero too. Only
  ;; the last unit can still grow. Without this, a pin that stops at the first
  ;; unit without a terminal fact would stop for good wherever a turn's result
  ;; lands after the next turn begins (a backgrounded call's exit); every round
  ;; would refold the facts behind that result.
  (let* ((last (car (last turns)))
         (pinned (if (and last (or last.terminal-p last.clean-p)) turns (butlast turns)))
         (new-watermark (and pinned (history-turn-end-sequence (car (last pinned))))))
    (unless (or (null pinned) (= new-watermark watermark))
      ;; the coverage count COVERED-PREFIX rechecks the pin against
      (let ((count (nlk:session-replay-coverage session-id new-watermark)))
        (setf (gethash session-id *hydration-cache*)
              (list new-watermark count head-stamp pinned))))))

(defun history-turns-for-session (session &key (after 0) raw)
  "The session's whole-turn history units, through the hydration cache: its
covered prefix plus the log tail folded, so a warm round costs one coverage
recheck and one tail page instead of a full-log rescan. They read as the
session's newest context edit has them (WORKING-CONTEXT-TURNS), or with RAW
as the fold made them."
  ;; NIL with no store.
  ;;
  ;; The prefix reaches into the turn being run. A turn's own earlier rounds
  ;; are the largest thing a round re-reads — 93% of the operator's provider
  ;; rounds are round one or later of a multi-round turn, and its longest turn
  ;; ran 339 rounds against 746 messages — so the cache holds the open unit
  ;; too and EXTEND-HISTORY-TURN appends the round that arrived to the unit it
  ;; pinned. What that buys is not the paging alone but identity: the memo
  ;; remembers an encoding by EQ, so the messages the provider has already
  ;; been sent are never encoded, measured or shaped twice.
  (when (nlk:store-open-p)
    ;; A floor-bounded operator read need not hydrate the discarded raw
    ;; prefix. It still passes through the identical revision projection.
    (when (plusp after)
      (return-from history-turns-for-session
        (let ((turns (history-turns-from-events (collect-visible-session-events (session-id session) after))))
          (if raw turns (working-context-turns (session-id session) turns)))))
    (let* ((session-id (session-id session))
           (head-stamp (nlk:session-head-stamp session-id))
           (entry (covered-prefix session-id head-stamp))
           (cached (and entry (fourth entry)))
           (watermark (if entry (first entry) 0))
           (open (let ((last (car (last cached)))) (and last (not last.terminal-p) last)))
           (events (collect-visible-session-events session-id watermark)))
      ;; An eviction rewrites the open unit's own messages — the stub lands
      ;; behind the user anchor and the dropped rounds leave — so the unit
      ;; is refolded from the fact after the terminal prefix, not extended.
      (when (and open
                 (some (lambda (event) (typep event 'nlk:turn-context-evicted-event))
                       events))
        (setf cached (butlast cached)
              open nil
              watermark (if-let (last (car (last cached)))
                          last.end-sequence 0)
              events (collect-visible-session-events session-id watermark)))
      (let* ((tail (history-turns-from-events events))
             (turns (if (and open tail
                             (equal (history-turn-turn-id (first tail))
                                    (history-turn-turn-id open)))
                        (append (butlast cached)
                                (list (extend-history-turn open (first tail)))
                                (rest tail))
                        (append cached tail))))
        (pin-folded-prefix session-id head-stamp watermark turns)
        (note-hydrated session-id)
        (if raw turns (working-context-turns session-id turns))))))

(defun session-context-estimate (session-id provider model)
  "(values TOKENS PARTS): the estimated prompt tokens SESSION-ID's next
provider round would carry, computed from the durable record without
running a turn: the transcript retained at the session's floor, the prompt
the next request compiles (the conversation's pinned prompt, turn.lisp
TURN-PROMPT) and its tool payload, through
COMPILED-CONTEXT-ESTIMATED-TOKENS and the (PROVIDER, MODEL) calibration
ratio — the same corrected number the eviction policy weighs against the
budget, and the number a readout should report when no provider round has
ever reported a prompt."
  ;; A lane with a record is never zero.
  ;;
  ;; PARTS is what that number is made of, in the same units, its parts
  ;; summing to TOKENS: :PROMPT and :TOOLS (the standing prompt and the tool
  ;; payload, which no eviction reaches), :STANDING (their sum), :HISTORY (the
  ;; retained turns — the only evictable part), :KEPT and :TURNS (turns the
  ;; floor retains, of the record's total) and :EVICTABLE (whole turns the
  ;; default /evict would drop). A readout that shows the split answers the
  ;; operator who reads a large occupancy and finds /evict nothing: a number
  ;; that is nearly all standing prompt is not history.
  ;;
  ;; The rest of PARTS is what each of those is made of, for a readout that
  ;; opens one: :SECTIONS, the system prompt and each harness section as
  ;; (NAME . TOKENS); :TOOL-SIZES, each tool's schema as (NAME . TOKENS),
  ;; largest first; :UNITS, each retained history unit as HISTORY-UNIT-FACTS
  ;; says it, oldest first. Each is estimated alone, so they come near their
  ;; part's figure rather than summing to it.
  ;;
  ;; This is NIL when the store is closed, the session is unknown, or the
  ;; session holds no turn to send: the callers then report the absence they
  ;; have, rather than a zero they do not.
  (when (and (nlk:store-open-p)
             (stringp session-id)
             (nlk:session-exists-p session-id))
    (when-let (turns (history-turns-for-session (intern-session session-id)))
      (let* ((boundary (session-floor-boundary session-id turns))
             (context (compile-turn-context
                       (snapshot-context-request
                        session-id
                        :history-turns turns
                        :history-boundary boundary)))
             (ratio (context-estimate-ratio provider model))
             (head (if-let (head context.head-message) (estimate-json-tokens head) 0))
             (prompt (round (* ratio (+ head (estimate-json-tokens context.system-prompt)))))
             (tools (round (* ratio (estimate-json-tokens (or context.tools #())))))
             (total (round (* (compiled-context-estimated-tokens context)
                              ratio)))
             (numbers (turn-numbers turns)))
        (flet ((scaled (value) (round (* ratio (estimate-json-tokens value)))))
          (values total
                  (list :prompt prompt
                        :tools tools
                        :standing (+ prompt tools)
                        :history (max 0 (- total prompt tools))
                        :kept (- (length turns) boundary)
                        :turns (length turns)
                        :evictable (or (getf (plan-forced-eviction
                                              (nthcdr boundary turns)
                                              *default-evict-percent*)
                                             :evicted-turn-count)
                                       0)
                        :sections (cons (cons "system prompt" (scaled context.base-system-prompt))
                                        (loop for (name . text) in context.harness-sections
                                              unless (equal text "")
                                                collect (cons name (scaled (nlk:harness-section-block name text)))))
                        :tool-sizes (sort (loop for tool across (or context.tools #())
                                                collect (cons (or (json-value tool :string "function" "name") "")
                                                              (scaled tool)))
                                          #'> :key #'cdr)
                        :units (loop for unit in (nthcdr boundary turns)
                                     collect (history-unit-facts unit ratio numbers)))))))))

(defun turn-numbers (turns &aux (numbers (make-hash-table :test #'equal)) (next 0))
  "Each turn id among TURNS, history units oldest first, mapped to its turn's
number in the session, 1 the first: a turn spread over several units counts
once, at its first."
  (dolist (unit turns numbers)
    (unless (gethash unit.turn-id numbers)
      (setf (gethash unit.turn-id numbers) (incf next)))))

(defun history-unit-facts (unit ratio numbers)
  "What a readout says of the history UNIT, its estimates scaled by RATIO, as
a plist: :TURN-ID, :NUMBER (NUMBERS's for it), :START and :END (its log
positions), :TOKENS, :NOTES (each note the model wrote in it as (TEXT .
TOKENS)), :PROMPT (its first user message's text, \"\" for none) and
:TERMINAL (whether its turn has ended)."
  (flet ((scaled (value) (round (* ratio value))))
    (list :turn-id unit.turn-id
          :number (gethash unit.turn-id numbers)
          :start unit.start-sequence
          :end unit.end-sequence
          :tokens (scaled (history-turn-tokens unit))
          :notes (loop for message in unit.messages
                       when (eq (message-source message) :note)
                         collect (cons (message-content message)
                                       (scaled (estimate-json-tokens message))))
          :prompt (if-let (asked (find-if #'user-role-message-p unit.messages))
                    (message-content asked)
                    "")
          :terminal unit.terminal-p)))

;;; --- policy: units -> retained boundary -----------------------------------

;;; --- the evicted history as blocks ------------------------------------------
;;; Every eviction is a fact, and each moved the floor from where the last one
;;; left it: the evicted history is a sequence of blocks, one per eviction, and
;;; the stub lists them so a recall can be aimed at one instead of at the whole
;;; span. The room behind a Discord lane carried sixteen evictions under one
;;; line, `seq 15370–268030 · 1265 turns'. A pure projection of the floors the
;;; chain recorded (NLK:RETAINED-HISTORY-FLOORS) over the turns the fold holds:
;;; no model call, byte-stable on every refold, and the block an eviction adds
;;; this round is the same block next round, when its floor has become a fact.

(defparameter *eviction-stub-block-limit* 12
  "How many blocks an eviction stub lists before folding the oldest into one
line that counts the evictions it holds.")

(defun eviction-blocks (evicted floors &aux (blocks '()))
  "EVICTED — the turns a stub stands in for, oldest first — split where
FLOORS, ascending log positions, cut them: (START END TURNS EVICTIONS) per
block, oldest first."
  ;; A floor F evicted every turn ending below it, so it falls between the two
  ;; turns whose ends straddle it. A floor that cut no visible turn, or the one
  ;; at the boundary itself, splits nothing: the last block ends where the
  ;; evicted range does, whether or not its floor has been recorded yet.
  (dolist (turn evicted)
    (let ((cut (loop while (and floors (<= (first floors) turn.end-sequence))
                     do (pop floors) count t)))
      (if (or (plusp cut) (null blocks))
          (push (list turn.start-sequence turn.end-sequence 1 1) blocks)
          ;; a block is this function's own fresh list, so it grows in place
          (setf (second (first blocks)) turn.end-sequence
                (third (first blocks)) (1+ (third (first blocks)))))))
  (nreverse blocks))

(defun fold-eviction-blocks (blocks &optional (limit *eviction-stub-block-limit*))
  "BLOCKS with the oldest folded into one when there are more than LIMIT, so
LIMIT lines stand: the folded line spans them and counts their evictions."
  (if (<= (length blocks) limit)
      blocks
      (let ((folded (subseq blocks 0 (1+ (- (length blocks) limit)))))
        (cons (list (first (first folded))
                    (second (car (last folded)))
                    (reduce #'+ folded :key #'third)
                    (reduce #'+ folded :key #'fourth))
              (nthcdr (length folded) blocks)))))

(defun recall-example (after before)
  "The recall a stub spells for the seqs strictly between AFTER and BEFORE,
runnable as written once the model puts its own words in place of text."
  (format nil "(recall-log :match \"text\"~@[ :after ~d~] :before ~d)"
          (and (plusp after) after) before))

(defun eviction-blocks-block (blocks)
  "The block list an eviction stub carries for BLOCKS, or NIL when the
evicted history is one block: the header already names it."
  (when (rest blocks)
    (let* ((blocks (fold-eviction-blocks blocks))
           (newest (car (last blocks))))
      (format nil "[evicted in ~d blocks, oldest first; a block's seqs aim a recall: ~a~{~% ~a~}]"
              (length blocks)
              (recall-example (1- (first newest)) (1+ (second newest)))
              (loop for (start end count evictions) in blocks
                    collect (format nil "seq ~d–~d · ~d turns~:[~; · ~d evictions~]"
                                    start end count (> evictions 1) evictions))))))

(defparameter *eviction-stub-context-note-chars* 6000
  "How much of the model's own context notes a whole-turn eviction stub
carries: the newest whole, older ones while they fit.")

(defun context-notes-block (turns)
  "The block an eviction stub carries for the notes the model wrote into
TURNS' context ([context note] messages, working-context.lisp), oldest first,
or NIL when it wrote none."
  ;; A note rides the turn it was written in, so a whole-turn eviction took
  ;; it with the turn: the one summary the model wrote of what was leaving.
  ;; The stub is a projection of the same fact, so every refold carries it.
  (let ((notes (loop for turn in turns
                     append (loop for msg in turn.messages
                                  when (eq :note (message-source msg))
                                    collect (note-text msg))))
        (kept '())
        (room *eviction-stub-context-note-chars*))
    (when notes
      (dolist (note (reverse notes))
        (cond ((null kept) (push (stub-clip note room) kept))
              ((<= (length note) room) (push note kept))
              (t (return)))
        (decf room (length (first kept))))
      (format nil "[your own notes in the evicted turns, oldest first~@[; ~d earlier not shown~]]~{~%~a~}"
              (and (< (length kept) (length notes)) (- (length notes) (length kept)))
              kept))))

(defun history-eviction-stub (turns boundary &optional floors)
  "The provider-context stub standing in for the evicted prefix — the Zig
renderEvictionStub typography (en dash, middle dots) — with the blocks
FLOORS cut it into."
  ;; The recall clause names RECALL-LOG, the kernel read the lisp organism
  ;; actually ships (recall.lisp), spelled as a call the model can run — never
  ;; a tool that does not exist: a road sign pointing at nothing teaches the
  ;; model to stop reading road signs. The seq range is the :seq domain
  ;; RECALL-LOG and RECALL-ENTRY take.
  (when (plusp boundary)
    (let ((last (nth (1- boundary) turns))
          (evicted (subseq turns 0 boundary)))
      (flet ((gathered (reader)
               (mapcan (lambda (turn) (copy-list (funcall reader turn))) evicted)))
        (message "system"
                 (format nil
                         "[history evicted: seq ~d–~d · ~d turns~@[ · ~a~]~@[ · ~a~] · recall: ~a]~@[~%~a~]~@[~%~a~]~@[~%~a~]"
                         (history-turn-start-sequence (first turns))
                         last.end-sequence
                         boundary
                         (standing-clause "defined" (gathered #'history-turn-definitions))
                         (standing-clause "watching" (gathered #'history-turn-watched))
                         (recall-example 0 (1+ last.end-sequence))
                         (eviction-blocks-block (eviction-blocks evicted floors))
                         (notes-block (gathered #'history-turn-notes))
                         (context-notes-block evicted)))))))

(defun user-role-message-p (msg)
  (equal "user" (json-value msg :string "role")))

(defun messages-at-boundary (turns boundary &optional floors)
  "The retained tail behind its eviction stub, with a user anchor
guaranteed: strict chat templates hard-reject a conversation carrying no
user-role message (the Qwen no-user-query 500), so a tail that lost every
user message re-splices the newest evicted one behind the stub. FLOORS are
the chain's recorded floors, which the stub lists the evicted blocks by."
  ;; Defense in depth — the policy below never evicts the live turn whole, so
  ;; a userless tail takes a degenerate history to reach.
  (let* ((tail (mapcan (lambda (turn) (copy-list turn.messages)) (nthcdr boundary turns)))
         (stub (history-eviction-stub turns boundary floors)))
    (if (or (null stub) (find-if #'user-role-message-p tail))
        (if stub (cons stub tail) tail)
        (let ((anchor (loop for turn in (reverse (subseq turns 0 boundary))
                              thereis (find-if #'user-role-message-p turn.messages :from-end t))))
          (cons stub (if anchor (cons (copy-json-value anchor) tail) tail))))))

(defun floor-boundary (turns floor)
  "How many leading TURNS lie entirely below the durable FLOOR."
  ;; Counting, not position-tracking: a floor beyond every turn must yield
  ;; (LENGTH TURNS), and the stepped-variable idiom under-reported by one on
  ;; list exhaustion, silently retaining one already-evicted turn.
  (if (zerop floor) 0 (loop for turn in turns while (< turn.end-sequence floor) count 1)))

(defun session-floor-boundary (session-id turns)
  "How many leading TURNS of SESSION-ID its durable floor has already cut."
  (floor-boundary turns (nlk:retained-history-floor session-id)))

;;; --- within-turn round eviction ---------------------------------------------
;;; The live turn's own eviction lane: message-space rounds (each opening at
;;; an assistant message) aligned to event-space round boundaries
;;; (ROUND-STARTS). Alignment is a precondition, not an assumption — a
;;; synthetic assistant flush (tool facts journaled with no stored assistant
;;; message) breaks the 1:1 mapping, and a misaligned watermark would sever a
;;; call from its result — so both planners refuse to act when the counts
;;; disagree and the turn simply stays whole.

(defun history-turn-round-split (turn &aux (messages turn.messages))
  "(values PREFIX ROUNDS): TURN's messages before its first assistant
message — the user anchor, plus a prior eviction stub when one stands — and
the per-round message runs, each opening at an assistant message."
  ;; A model's note is no round: it rides the round before it, or the prefix,
  ;; so the rounds stay the assistant facts ROUND-STARTS counts.
  (let ((starts (loop for msg in messages
                      for index from 0
                      when (and (equal "assistant" (json-value msg :string "role"))
                                (not (eq :note (message-source msg))))
                        collect index)))
    (values (subseq messages 0 (first starts))
            (loop for (from to) on starts collect (subseq messages from to)))))

(defun relieved-round (round &key thin)
  "ROUND's messages as a relief leaves them, near enough to estimate: each
assistant message without its reasoning, and when THIN each long tool result
cut to its head and the recall line."
  (mapcar (lambda (message &aux (role (json-value message :string "role"))
                                (text (content-text (gethash "content" message))))
            (cond ((equal "assistant" role)
                   (without-reasoning (copy-json-object message)))
                  ((and thin (equal "tool" role) (thin-result-p text))
                   (copy-json-object message "content" (thinned-result-text text 0)))
                  (t message)))
          round))

(defun live-relief-plan (turn over calibration)
  "How live TURN sheds OVER estimated tokens, CALIBRATION the measured to
estimated ratio: (:THIN N :DROP D), its oldest N retained rounds thinned and
its oldest D gone, or NIL for a turn of fewer than two rounds or rounds not
aligned to their boundaries."
  ;; Any relief takes the reasoning of every round it finds standing (thinned
  ;; rounds, above). Thinning comes first and goes as far as it may: every
  ;; round but the newest *LIVE-WHOLE-ROUNDS* at once -- headroom, so the next
  ;; relief and the prefix it rewrites are far off -- and on toward the newest
  ;; round while the estimate is still over, one round at the least, so the
  ;; relief is a fact. Rounds leave, oldest first, only past that, and never
  ;; the newest.
  (let* ((rounds (coerce (nth-value 1 (history-turn-round-split turn)) 'vector))
         (count (length rounds))
         (starts turn.round-starts))
    (when (and (>= count 2) (= count (length starts)))
      (flet ((tokens (messages) (* calibration (estimate-json-tokens messages))))
        (let* ((whole (map 'vector #'tokens rounds))
               (bare (map 'vector (lambda (round) (tokens (relieved-round round))) rounds))
               (thin (map 'vector (lambda (round) (tokens (relieved-round round :thin t))) rounds))
               (thinned (count-if (lambda (start) (<= start turn.thinned-through)) starts))
               (thin-to (max thinned (- count *live-whole-rounds*)))
               (drop 0))
          (flet ((recovered ()
                   (loop for i below count
                         sum (- (aref whole i)
                                (cond ((< i drop) 0)
                                      ((< i thin-to) (aref thin i))
                                      (t (aref bare i)))))))
            ;; one round at the least, so the relief is a fact
            (loop while (and (< thin-to (1- count)) (or (= thin-to thinned) (< (recovered) over)))
                  do (incf thin-to))
            (loop while (and (< (recovered) over) (< drop (1- count)))
                  do (incf drop))
            ;; with nothing left to thin, the oldest round goes
            (when (and (= thin-to thinned) (zerop drop))
              (setf drop 1))
            (list :thin thin-to :drop drop)))))))

(defun live-relief-fields (turn thin drop)
  "The turn.context.evicted fact that leaves live TURN with its oldest THIN
retained rounds thinned and its oldest DROP gone: (values WATERMARK FIELDS),
FIELDS the keywords RECORD-TURN-CONTEXT-EVICTION takes beside WATERMARK, or
NIL when that moves neither watermark or the rounds do not align."
  ;; Each watermark is the position before the first round it keeps, the
  ;; counts are cumulative, and the thinned one never trails the other: the
  ;; newest fact alone says what the window holds, and the fold that reads
  ;; it is the relief (HISTORY-TURNS-FOR-SESSION refolds the turn).
  (let* ((rounds (nth-value 1 (history-turn-round-split turn)))
         (starts turn.round-starts)
         (count (length rounds)))
    (when (and (< thin count) (< drop count) (= count (length starts)))
      (let* ((watermark (if (plusp drop) (1- (nth drop starts)) turn.evicted-through))
             (thinned (max watermark turn.thinned-through
                           (if (plusp thin) (1- (nth thin starts)) 0))))
        (when (or (> watermark turn.evicted-through) (> thinned turn.thinned-through))
          (values watermark
                  (list :thinned thinned
                        :evicted-start-sequence (cond ((zerop drop) turn.evicted-start-sequence)
                                                      ((plusp turn.evicted-start-sequence)
                                                       turn.evicted-start-sequence)
                                                      (t turn.start-sequence))
                        :evicted-round-count (+ turn.evicted-round-count drop)
                        :evicted-message-count (+ turn.evicted-message-count
                                                  (loop for round in (subseq rounds 0 drop)
                                                        sum (length round))))))))))

(defun choose-history-boundary (turns durable-floor
                                &key (budget (model-context-tokens *model*))
                                tools system-prompt head (calibration 1.0d0)
                                floors)
  "Apply durable floor, then replay hysteresis to a stable whole-turn
boundary."
  ;; Returns (values BOUNDARY LIVE-PLAN OVER): LIVE-PLAN, when non-NIL, is how
  ;; the live turn relieves itself WITHIN the turn (LIVE-RELIEF-PLAN, (:THIN N
  ;; :DROP D)) — terminal turns exhausted with the estimate still over the
  ;; trigger — and OVER the estimate at the durable floor when it is past the
  ;; trigger, NIL under it: what HYDRATE-SESSION warns the model with before
  ;; it lets either move. The live turn is never evicted whole (the userless-request /
  ;; resurrection class); when it cannot be relieved either, the request goes
  ;; out as-is and the provider's own verdict drives the recovery arm.
  ;;
  ;; TOOLS, SYSTEM-PROMPT and HEAD (the head message, or NIL) are the payloads
  ;; the request will actually carry, and FLOORS the recorded floors the
  ;; stub it will carry lists its blocks by; HYDRATE-SESSION (turn.lisp) owns the
  ;; live-global default for round zero and external callers — this policy
  ;; function reads no ambient state beyond the capability budget default.
  ;; CALIBRATION is the measured/estimated token ratio for the turn's
  ;; (provider, model); estimates below are compared to the budget in the
  ;; server's own token scale.
  ;;
  ;; A NIL BUDGET is an unknown window, and unknown means no trigger: no
  ;; estimate can prove pressure against a window nobody measured, so only the
  ;; durable floor applies and the provider-rejection recovery arm owns
  ;; eviction until a learned ceiling gives the belief a number.
  ;;
  ;; Estimation cost: the fixed parts serialize once, each turn's messages
  ;; estimate through the per-turn memo, and a suffix sum gives every
  ;; candidate boundary in O(turns) — no whole-context serialization per
  ;; probe. Only the boundary-varying eviction stub still serializes per
  ;; candidate, and it is one short line.
  (let* ((boundary (floor-boundary turns durable-floor))
         (trigger (and budget (* budget *context-eviction-trigger-ratio*)))
         (floor (and budget (* budget *context-eviction-floor-ratio*)))
         (fixed (+ (estimate-json-tokens system-prompt)
                   (estimate-json-tokens (or tools #()))
                   (if head (estimate-json-tokens head) 0)))
         (units (coerce turns 'vector))
         (sums (turn-token-sums units)))
    (flet ((estimate (boundary)
             (* calibration
                (+ fixed (- (aref sums (length units)) (aref sums boundary))
                   (if-let (stub (history-eviction-stub turns boundary floors))
                     (estimate-json-tokens stub) 0)))))
      (let ((live-plan nil)
            (over (and trigger (> (estimate boundary) trigger) (estimate boundary))))
        (when over
          (loop while (> (estimate boundary) floor)
                for next = (and (< boundary (length turns))
                                (history-turn-terminal-p (nth boundary turns)) (1+ boundary))
                while next
                do (setf boundary next))
          ;; Terminal turns exhausted and still over the trigger: the live
          ;; turn relieves itself toward the floor -- its older rounds thinned
          ;; first, its oldest rounds gone only past that -- never its user
          ;; anchor and never its newest round.
          (when (and (> (estimate boundary) trigger)
                     (< boundary (length units)))
            (let ((live (aref units (1- (length units)))))
              (unless (history-turn-terminal-p live)
                (setf live-plan (live-relief-plan live (- (estimate boundary) floor)
                                                  calibration))))))
        (values boundary live-plan over)))))

(defun turn-token-sums (units)
  "The running token estimate over UNITS, a vector of turns: element I sums
the units before I, so the last is the whole."
  (coerce (cons 0 (loop for unit across units
                        sum (history-turn-tokens unit) into total collect total))
          'vector))

(defun eviction-split (turns from to)
  "What evicting TURNS FROM below TO moves, as NLK:ADVANCE-HISTORY-FLOOR's
keywords: the evicted range's first and last seq, and the turns and messages
on each side of TO."
  (flet ((messages (start end)
           (loop for turn in (subseq turns start end) sum (length turn.messages))))
    (list :evicted-start-sequence (history-turn-start-sequence (nth from turns))
          :evicted-end-sequence (history-turn-end-sequence (nth (1- to) turns))
          :evicted-turn-count (- to from)
          :retained-turn-count (- (length turns) to)
          :evicted-message-count (messages from to)
          :retained-message-count (messages to (length turns)))))

(defun plan-forced-eviction (turns percent &aux (units (coerce turns 'vector))
                                                (count (length units)))
  "Mirror the Zig planForcedEviction over the post-floor tail TURNS: drop
the oldest whole turns until at least PERCENT of the retained-history token
estimate is gone."
  ;; Whole turns are the unit, so the realized share is the first turn
  ;; boundary at or past the target; the newest turn is always retained, and
  ;; the contiguous non-terminal tail is never evictable (the live rows an
  ;; operator fold must not touch). Pure function of (TURNS, PERCENT); returns
  ;; NIL when the plan evicts nothing (the route's no_history_to_evict), else
  ;; a plist of the boundary and its realized token split.
  (when (plusp count)
    (let* ((cum (turn-token-sums units))
           (target (floor (* (aref cum count) percent) 100))
           (cap (eviction-cap units))
           (boundary (or (position target cum :test #'<= :end (1+ cap)) cap)))
      (when (plusp boundary)
        (eviction-plan turns units cum boundary)))))

(defun eviction-cap (units &aux (count (length units)))
  "The furthest boundary an operator eviction reaches over UNITS, a vector of
the post-floor tail: never the newest unit, and never into the contiguous
non-terminal tail (the live rows an operator fold must not touch)."
  (min (1- count)
       (1+ (or (position-if #'history-turn-terminal-p units :from-end t) -1))))

(defun eviction-plan (turns units cum boundary &aux (total (aref cum (length units))))
  "The plan that evicts TURNS (UNITS as a vector, CUM their TURN-TOKEN-SUMS)
below BOUNDARY: the retained start and the realized token split."
  (list* :retained-from-sequence
         (history-turn-start-sequence (aref units boundary))
         :evicted-estimated-tokens (aref cum boundary)
         :retained-estimated-tokens (- total (aref cum boundary))
         :realized-percent (if (zerop total)
                               0
                               (floor (* (aref cum boundary) 100) total))
         (eviction-split turns 0 boundary)))

(defun plan-eviction-through (turns turn-id &aux (units (coerce turns 'vector)))
  "The plan that evicts the post-floor tail TURNS through the last unit of
the turn TURN-ID, as PLAN-FORCED-EVICTION answers it, or NIL when no unit of
TURNS is that turn's or evicting it would cross EVICTION-CAP."
  ;; A turn's late fact (a background exit) can open a second unit with its
  ;; id: the last one is where the turn ends.
  (when-let (index (position turn-id units :key #'history-turn-turn-id
                                           :test #'equal :from-end t))
    (when (< index (eviction-cap units))
      (eviction-plan turns units (turn-token-sums units) (1+ index)))))

;;; --- durable floor --------------------------------------------------------

(defun persist-history-boundary (session-id turns previous-boundary boundary
                                 &key (reason "budget_fill") plan)
  "Advance SESSION-ID's durable floor to BOUNDARY over TURNS."
  ;; PLAN is the forced-eviction plist PLAN-FORCED-EVICTION already produced;
  ;; only its four percent/token fields ride through, because the counts and
  ;; sequences are re-derived from TURNS here — that is what lets the
  ;; budget-fill path, which has no plan and no percent contract, write the
  ;; identical fact shape.
  (when (and session-id (> boundary previous-boundary))
    ;; the prefix is rebuilt anyway: the index is pinned again at the next request
    (nlk:mark-index-refresh session-id)
    (apply #'nlk:advance-history-floor
           session-id
           (if (< boundary (length turns))
               (history-turn-start-sequence (nth boundary turns))
               (1+ (history-turn-end-sequence (car (last turns)))))
           :reason reason
           :requested-percent (getf plan :requested-percent)
           :realized-percent (getf plan :realized-percent)
           :evicted-estimated-tokens (getf plan :evicted-estimated-tokens)
           :retained-estimated-tokens (getf plan :retained-estimated-tokens)
           (eviction-split turns previous-boundary boundary))))

;;; --- the recovery eviction step ---------------------------------------------
;;; One owner for what the provider-rejection recovery arm evicts (turn.lisp
;;; CALL-PROVIDER used to restate the hydration policy here with its own
;;; allow-live escape — the escape that produced the userless request).

(defun evict-context-one-step (session-id turns boundary &key live-turn
                                                              (reason "provider_rejected"))
  "One durable eviction step after a provider context rejection: the oldest
retained terminal turn when one exists, else the live turn's rounds thinned
while any but its newest *LIVE-WHOLE-ROUNDS* is still whole, and half its
retained rounds once none is (LIVE-TURN is the executing durable turn,
required to journal the within-turn fact)."
  ;; Persists the step and returns
  ;; (values TURNS BOUNDARY); NIL when nothing further is evictable — the
  ;; caller's rejection then stands as the turn's honest outcome.
  ;;
  ;; REASON names who asked, and rides both facts. The default is the
  ;; rejection arm's; the pre-call governor passes its own, because a step it
  ;; took is not a provider verdict — without that, governor steps would be
  ;; journaled as "provider_rejected".
  ;; a terminal turn only: the live turn evicts within itself, never whole
  (let ((next (and (< boundary (length turns))
                   (history-turn-terminal-p (nth boundary turns)) (1+ boundary))))
    (cond
      (next
       (persist-history-boundary session-id turns boundary next :reason reason)
       (values turns next))
      ((and live-turn turns)
       (let ((turn (car (last turns))))
         (when (and (not turn.terminal-p)
                    (equal turn.turn-id live-turn.turn-id))
           ;; LIVE-RELIEF-FIELDS refuses a step that moves nothing, or rounds
           ;; not aligned to their boundaries.
           (let* ((count (length (nth-value 1 (history-turn-round-split turn))))
                  (thinned (count-if (lambda (start) (<= start turn.thinned-through))
                                     turn.round-starts))
                  (thin (max thinned (- count *live-whole-rounds*))))
             (multiple-value-bind (watermark fields)
                 (if (> thin thinned)
                     (live-relief-fields turn thin 0)
                     (live-relief-fields turn thinned (max 1 (floor count 2))))
               (when fields
                 (apply #'nlk:record-turn-context-eviction
                        live-turn watermark :reason reason fields)
                 ;; the fold that reads the fact is the step
                 (values (history-turns-for-session (intern-session session-id))
                         boundary))))))))))

;;; --- the forced eviction op -----------------------------------------------
;;; One executable seam for a forced eviction, whatever surface carries it:
;;; the gateway POST route, the local TUI slash op and the channel kit's room
;;; bound all land here. Planning over the HEAD-VISIBLE fold is load-bearing
;;; — planning over the raw log on a forked or head-moved session counts
;;; events the next compile never projects, so the realized percentages lie
;;; and the floor can jump past turns the operator still sees.

(defun evict-session-context (session-id &key percent keep over through
                                              (reason "operator_requested"))
  "Plan and persist a forced eviction over SESSION-ID's retained history:
PERCENT of it, or down to KEEP estimated tokens, and only once it holds more
than OVER when that is given -- or, given THROUGH, a turn id, every retained
turn up to and including that one (PLAN-EVICTION-THROUGH)."
  ;; NIL PERCENT and KEEP select *DEFAULT-EVICT-PERCENT* — this op is the
  ;; single owner of that default. KEEP is spelled as the percent that reaches
  ;; it, so the plan and the fact read alike whichever was asked. OVER is the
  ;; gate a caller that bounds a session nobody sends requests on holds it to
  ;; (the channel kit's room, which the window trigger never sees): a room cut
  ;; to half its bound each time it passes it keeps one prefix for many
  ;; exchanges, where a cut back to the bound itself would move the prefix
  ;; every exchange. Plans against exactly what the next compile would see:
  ;; the head-visible fold, cut at the existing durable floor (the Zig
  ;; evictSessionContext posture), read from that floor on: a floor opens a
  ;; turn, so what lies below it is already cut, and a room bounded after
  ;; every exchange would otherwise re-read its whole life each time (5,298
  ;; events, 0.9 s, in a replay of the Discord room). Returns NIL when there
  ;; is nothing to evict, else (values PLAN FLOOR EVENT-ID SOURCE-WATERMARK)
  ;; from the persisted boundary advance, with REASON; PLAN leads with the
  ;; resolved :REQUESTED-PERCENT so surfaces echo the effective request.
  (let* ((turns (history-turns-for-session
                 (intern-session session-id)
                 :after (max 0 (1- (nlk:retained-history-floor session-id)))))
         (previous-boundary (session-floor-boundary session-id turns))
         (retained (nthcdr previous-boundary turns))
         (total (reduce #'+ retained :key #'history-turn-tokens))
         (percent (cond (through nil)
                        ((null keep) (or percent *default-evict-percent*))
                        ((> total keep) (ceiling (* (- total keep) 100) total))))
         (forced (if through
                     (plan-eviction-through retained through)
                     (and percent (or (null over) (> total over))
                          (plan-forced-eviction retained percent))))
         ;; One plan value: what surfaces echo is what the durable fact rides.
         ;; A turn asked for is the share it realizes.
         (plan (and forced (list* :requested-percent (or percent (getf forced :realized-percent))
                                  forced))))
    (when plan
      (multiple-value-bind (floor event-id source-watermark)
          (persist-history-boundary
           session-id turns previous-boundary
           (+ previous-boundary (getf plan :evicted-turn-count))
           :reason reason :plan plan)
        (values plan floor event-id source-watermark)))))

(defun evict-decline-text (session-id &key through)
  "What /evict answers when EVICT-SESSION-CONTEXT plans nothing: the state
that leaves it nothing, so the refusal reads against the occupancy /context
reports instead of against silence — how many turns the durable floor has
already cut, and that the newest turn is always retained (the turn an ask
is being answered in must never leave the window, and a tail that lost its
user message is a hard template rejection). THROUGH is the turn id an
eviction was asked to run through."
  ;; NIL when the session holds no turn at all, or holds more than one and the
  ;; caller's plan still came back empty: the plain `nothing to evict' is that
  ;; case's honest copy, and the callers keep it.
  (when (and (nlk:store-open-p)
             (stringp session-id)
             (nlk:session-exists-p session-id))
    (let* ((turns (history-turns-for-session (intern-session session-id)))
           (boundary (and turns (session-floor-boundary session-id turns)))
           (kept (and turns (- (length turns) boundary))))
      (when through
        (return-from evict-decline-text
          (if (find through (nthcdr (or boundary 0) turns)
                    :key #'history-turn-turn-id :test #'equal)
              "nothing to evict — that turn is the newest or still running, and those are always kept"
              "nothing to evict — that turn is not in the window any more")))
      (when (and kept (<= kept 1))
        (if (plusp boundary)
            (format nil
                    "nothing to evict — ~d of ~d turns already evicted; the newest turn is always kept"
                    boundary (length turns))
            "nothing to evict — the newest turn is the only one, and it is always kept")))))

;;; --- assemble: request -> compiled context ---------------------------------

(defparameter *help-section*
  "The manual, read when a round needs it: (help :topic) answers one of these, the running cells' among them, and (help) indexes every verb.~%~a"
  "The help section every session reads, a format string over the topic
lines (turn.lisp HELP-TOPIC-LINES): what the image can do, a line a topic,
with the text left in the image. Mutable material, like the system prompt.")

;;; --- the working directory's instructions ----------------------------------
;;; A project leaves a file for the agents that work in it — AGENTS.md by
;;; the open convention, CLAUDE.md and .cursorrules by two others — and an
;;; operator who kept one for their last harness expects it to keep
;;; applying here. The first one found wins, looked for in the session's
;;; directory and then in each parent up to the repository root (the
;;; directory holding .git), and it rides as the `instructions' harness
;;; section: read at every round's request build like the durable
;;; sections, never written anywhere. The file is the one authority and the
;;; section its projection — live-only, the channel kit's SOUL.md posture —
;;; so an edit is live next round and a deleted file clears it. Nothing is
;;; trimmed to fit: a file over the harness budget is refused with one
;;; notice per path in RULES-REFUSAL's words, and an unreadable one is no
;;; section.

(defparameter *instructions-file-names* '("AGENTS.md" "CLAUDE.md" ".cursorrules")
  "The files a project leaves for its agents, in the order the first found
wins within one directory.")

(defparameter +instructions-byte-limit+ 65536
  "The harness budget SET-HARNESS-SECTION holds durable sections to, applied
to this projected one the same way.")

(defvar *instructions-refused* (make-hash-table :test #'equal)
  "A project's rules files already said to be over budget: one notice per
path per image.")

(defun rules-refusal (file text)
  "What the operator is told of FILE, a rules file whose text is TEXT: NIL
within +INSTRUCTIONS-BYTE-LIMIT+, past it that no session reads it, and what
to do."
  (and (> (length text) +instructions-byte-limit+)
       (format nil "~a is not read: ~a — trim to ~a"
               (home-abbreviated (uiop:native-namestring file)) (nlk:size-text (length text))
               (nlk:size-text +instructions-byte-limit+))))

(defun rules-section (name file text)
  "TEXT, what the rules FILE holds, as the section NAME, (NAME . TEXT): none
for a missing, blank or over-budget file."
  (and text (<= (length text) +instructions-byte-limit+) (plusp (length (nlk:trimmed text)))
       (cons name (format nil "From ~a:~%~%~a" (namestring file)
                          (string-right-trim '(#\Newline #\Return) text)))))

;;; The operator's own rules, for every session in any folder, live in the
;;; home: AGENTS.md beside config.jsonc (NLK:HOME, so a profile has its
;;; own), where an import lands another harness's CLAUDE.md, AGENTS.md or
;;; GEMINI.md. It rides as the `operator-rules' section, ahead of a
;;; project's `instructions', and is read the same way: at the request
;;; build, never stored, refused over the budget under a standing notice, no
;;; section when blank or unreadable.

(defun operator-rules-section (project-file)
  "The home's AGENTS.md as the `operator-rules' section, (NAME . TEXT), or
NIL when there is none to read or it is PROJECT-FILE, which the
`instructions' section carries."
  (let* ((path (nlk:home "AGENTS.md"))
         (file (probe-file path))
         (text (and file (nlk:read-text file)))
         ;; one home, one file: the key is the board row's label
         (key "rules")
         (said (rules-refusal path (or text ""))))
    ;; Keyed, so it stands where the operator can still find it -- on a shell
    ;; that attaches later, in /cells, and in the model's notices block --
    ;; where a warning was a five-second toast in whichever shell shared this
    ;; process, once per image (xh-201). Decided here for every session, the
    ;; one whose PROJECT-FILE it is too, and of a missing file, so a notice
    ;; that stood comes down; said again only when its words change.
    (unless (equal said (second (assoc key (nlk:standing-notices) :test #'string=)))
      (ignore-errors (notice said :level :warning :key key)))
    (unless (equal file project-file) (rules-section "operator-rules" file text))))

;;; The session's directory itself rides as the `folder' section. Nothing
;;; else in the prompt names it, while the `image' section names the home,
;;; so without it a model asked where it is would answer with the home.
;;; Standing, not live: a session's directory never changes and a fork
;;; inherits its parent's, so it holds the cached prefix.

(defparameter *folder-section*
  "This session's folder is ~a: the operator opened the session there, and it is what \"here\" means to them. sh runs in it, and the file verbs resolve a relative path against it. An image file you name in an answer as ![alt](path) is drawn in that answer for the operator. Your home, ~a, holds your own config, memory and cells, not the operator's work."
  "The `folder' section, a format string over the session's directory and
the home. Mutable material, like the system prompt.")

;;; --- the live sections ----------------------------------------------------
;;; What no durable row holds and each turn reads afresh: the notice board, a
;;; lane's `where', a nudge. They never pin: what one session or one moment
;;; holds would make the conversation's head its own, and a fork's pin the
;;; prefix no sibling shares. They reach the model as a <system-reminder> in
;;; the transcript at the turn that saw them change (TURN-PROMPT), a pin's
;;; own turn first among them, so a notice costs its own bytes once and
;;; never a re-read of the turns before it.

(defvar *resumed-turns* (make-hash-table :test #'equal :synchronized t)
  "Turn id -> what the `resumed' live section tells the model, for each turn
this boot picked up where a restart cut it off (gateway.lisp NOTE-BACK).")

(defun read-live-sections (session-id &aux (turn *current-durable-turn*))
  "SESSION-ID's live sections at this turn's start — (NAME . TEXT) — the
ones no durable row holds."
  ;; The core contributes the standing-notice board under `notices'; a
  ;; cell with live material of its own advises this function the same way
  ;; it advises READ-HARNESS-SECTIONS for a standing section, and takes the
  ;; session id here for the same reason: what is true of ONE session (the
  ;; channel kit's `where', which thread a lane runs in) is read for the
  ;; session asking. What is the same at every boot of this home belongs on
  ;; the standing side instead: the serving gateway's `image' section (what
  ;; this process owns, and the rule for a shell in another) advises
  ;; READ-HARNESS-SECTIONS (gateway.lisp IMAGE-HARNESS-SECTIONS). What one
  ;; process alone knows — a pid, a port, who is attached, a heap address in
  ;; a condition's text — belongs on neither side: the model reads it with a
  ;; verb when it wants it.
  ;;
  ;; A turn this boot picked up after a restart cut it off reads `resumed'
  ;; at the round it resumes at, and the model opens by saying it is back.
  (append (when-let (board (nlk:standing-notices-text)) (list (cons "notices" board)))
          (when-let (resumed (and turn (equal session-id (nlk:durable-turn-session-id turn))
                                  (gethash (nlk:durable-turn-turn-id turn) *resumed-turns*)))
            (list (cons "resumed" resumed)))))

(defun sections-message (sections &aux (overlay (harness-overlay-prompt "" sections)))
  "SECTIONS as one user-role message of <harness> blocks, or NIL when there
are none — the head the lanes join ahead of the history (provider.lisp
MESSAGES-WITH-HEAD): HARNESS-OVERLAY-PROMPT's analog for a message. An empty
section is no block."
  ;; an overlay opens each block on a blank line, which a message does not
  (when (plusp (length overlay))
    (message "user" (shared-prompt (string-left-trim '(#\Newline) overlay)))))

(defun read-harness-sections (session-id)
  "The select-stage read of the standing <harness> sections SESSION-ID runs
under — its own and every anchored ancestor's, nearest winning
(NLK:STANDING-HARNESS-SECTIONS: a fork inherits the standing block it forked
into, the way it inherits the record) — read
once per turn at its first round's request build (the standing block is
frozen for the turn — turn.lisp SNAPSHOT-CONTEXT-REQUEST — so an entry
written mid-turn is visible at the next turn's first round, and the cached
prefix holds across the turn's rounds), then the three sections the core
projects — the session's folder, the working directory's instructions file
and the manual's topics — each replacing a durable row of its name. The
index of what the organism keeps is pinned first (NLK:PIN-INDEX-SECTION),
a durable row of its own, and rides behind every other (INDEX-LAST)."
  ;; Failure to read the durable ones warns and degrades to none — standing
  ;; context is not a turn precondition (the turn_engine.zig error posture);
  ;; an index that cannot be pinned is a standing notice, and the round goes on.
  (when (and session-id (nlk:store-open-p))
    (handler-case (nlk:pin-index-section session-id #'index-text)
      (error (condition)
        (ignore-errors
         (notice (format nil "index: not pinned for ~a: ~a" session-id condition)
                 :level :warning :key "index")))))
  (let* ((sections (when (and session-id (nlk:store-open-p))
                     (handler-case (nlk:standing-harness-sections session-id)
                       (error (e)
                         (warn "harness sections unavailable for this round: ~a" e)))))
         ;; the instructions file: no section when unreadable, blank or over the budget
         (session (and session-id (nlk:store-open-p)
                       (ignore-errors (nlk:find-session session-id))))
         (cwd (and session (nlk:durable-session-cwd session)))
         ;; the first found at CWD or a parent, stopping after the repository root (.git) or /
         (file (loop with directory = (and cwd (ignore-errors (uiop:ensure-directory-pathname cwd)))
                     repeat 64
                     while directory
                     thereis (loop for name in *instructions-file-names*
                                   thereis (uiop:file-exists-p (merge-pathnames name directory)))
                     until (probe-file (merge-pathnames ".git" directory))
                     do (let ((parent (uiop:pathname-parent-directory-pathname directory)))
                          (setf directory (and (not (equal parent directory)) parent)))))
         (text (and file (nlk:read-text file)))
         ;; the home's own AGENTS.md, for a session in any folder: ahead of
         ;; the project's file, and replacing a durable row of its name
         (rules (operator-rules-section file)))
    ;; Past the budget, a project's file is said once; the home's own stands
    ;; on the board wherever it rides (OPERATOR-RULES-SECTION).
    (when (and text (> (length text) +instructions-byte-limit+)
               (not (equal file (probe-file (nlk:home "AGENTS.md")))))
      (let ((key (namestring file)))
        (unless (gethash key *instructions-refused*)
          (setf (gethash key *instructions-refused*) t)
          (ignore-errors (notice (rules-refusal file text) :level :warning)))))
    (dolist (projected (list rules
                             (and cwd (cons "folder" (format nil "~?~:[~; sh is Git's sh.exe: POSIX sh, not cmd or PowerShell.~]"
                                                              *folder-section*
                                                              (list cwd (string-right-trim "/" (namestring (nlk:home))))
                                                              (uiop:os-windows-p))))
                             (rules-section "instructions" file text)
                             ;; a line a topic of the manual: what the image and its
                             ;; running cells can do, the texts left to (help)
                             (cons "help" (format nil *help-section*
                                                  (string-right-trim '(#\Newline) (help-topic-lines)))))
                       (index-last sections))
      (when projected
        (setf sections
              (append (remove (car projected) sections :key #'car :test #'string=)
                      (list projected)))))))

(defun index-last (sections &aux (index (assoc nlk::+index-section+ sections :test #'string=)))
  "SECTIONS with the index, when one stands, moved behind every other."
  ;; Prompt caching matches a prefix, and two conversations share one only
  ;; up to their first differing byte. Every other standing section reads
  ;; the same in every conversation of a project; the index is pinned per
  ;; conversation and moves with every keep (its count of what it does not
  ;; list, its ranking). Without this, the index would sit before the
  ;; shared sections and its first byte would become the first difference,
  ;; cutting the prefix the conversations share. Behind the rest, it costs
  ;; only itself.
  (if index (append (remove index sections :test #'eq) (list index)) sections))

;;; --- the system message and the head ----------------------------------------
;;; The system message is the harness's own voice: the base, where the
;;; session is, and a line per topic of what it can do. Every other standing
;;; section is text the harness relays — a repository's instructions file,
;;; an index the model's own keeps grew, a room's contract and soul — and it
;;; rides the head of the conversation, one user message joined ahead of the
;;; first user message's own text, where Claude Code, Codex and DSH put the
;;; same material. Both are the cached prefix; only the rank differs, and
;;; the system message falls to a few lines.

(defparameter *system-sections* '("folder" "help")
  "The standing sections the system message carries — where the session is
and what it can do; every other one rides the head message
(PROMPT-MESSAGES). Mutable material, like the system prompt.")

(defvar *shared-prompts* (make-hash-table :test #'equal :weakness :key-and-value
                                                        :synchronized t)
  "The prompt texts compiled turns hold, each by its own text, for as long as
one holds it (SHARED-PROMPT).")

(defun shared-prompt (text)
  "The prompt text a compiled turn already holds EQUAL to TEXT, else TEXT,
shared from now on."
  ;; A conversation's system prompt and head are the same bytes turn after
  ;; turn, and the lanes of one room differ in a line or two: built afresh,
  ;; every turn in flight held its own tens of kilobytes of the same text.
  (or (gethash text *shared-prompts*)
      (setf (gethash text *shared-prompts*) text)))

(defun prompt-messages (base sections)
  "=> (values SYSTEM-PROMPT HEAD-MESSAGE): the system message's text — BASE
and the sections *SYSTEM-SECTIONS* names — and the head message of the
rest, as a request built from BASE and SECTIONS carries them."
  (flet ((system-p (section) (member (car section) *system-sections* :test #'string=)))
    (values (shared-prompt (harness-overlay-prompt base (remove-if-not #'system-p sections)))
            (sections-message (remove-if #'system-p sections)))))

(defun harness-overlay-prompt (base sections)
  "BASE plus SECTIONS as <harness> blocks — the pure assemble half of the
harness cadence; the base stays frozen beside the overlay so no round ever
double-appends. An empty section is no block: a cell pins the empty
string for nothing to show, which keeps its pin and adds nothing to the prompt."
  (format nil "~a~{~%~%~a~}" base (loop for (name . text) in sections
                                        unless (equal text "")
                                          collect (nlk:harness-section-block name text))))
;;; --- the prompt a conversation keeps ---------------------------------------
;;; Prompt caching is a prefix match over the rendered request, so the prompt
;;; belongs to the conversation, not to the turn: its first request pins the
;;; system prompt, the standing harness sections and the tools (a
;;; turn.context.prompt fact, turn.lisp TURN-PROMPT), and every later request
;;; sends those bytes again. The live sections, and what changes after — a
;;; section rewritten, a notice posted, the system prompt of a newer build, a
;;; tool unloaded — reach the model as a <system-reminder> its fact carries,
;;; standing in the transcript at the turn that saw it and never moving. A
;;; fork reads its ancestors' facts through the lineage, so it keeps the
;;; prefix its parent paid for; only an eviction re-pins, when the window's
;;; head moves and the prefix is rebuilt anyway.

(defparameter *reminder-removed-line-limit* 160
  "How much of a removed line a reminder quotes: enough to place it, since
the model was shown it whole.")

(defun kept-sections (sections)
  "SECTIONS as a prompt keeps them: an empty one dropped, as no block
renders it, and each name once, its first standing."
  (remove-duplicates (remove "" sections :key #'cdr :test #'equal)
                     :key #'car :test #'string= :from-end t))

(defstruct conversation-prompt
  "What a conversation's prompt facts leave standing: the prompt its pin
sends, and what the model was told since."
  (base "" :type string)
  (sections '() :type list)
  (tools #() :type vector)
  (floor 0 :type integer)
  ;; The system prompt and sections as the reminders since the pin left
  ;; them, and the pinned tools no longer bound.
  (told-base "" :type string)
  (told-sections '() :type list)
  (withdrawn '() :type list))

(nlk:access (prompt conversation-prompt))

(defun section-with (sections name text)
  "SECTIONS with NAME's text TEXT — in its place when it stands, last when it
is new — or without NAME when TEXT is NIL."
  (if text
      (nlk::put-in-place sections name text #'string=)
      (remove name sections :key #'car :test #'string=)))

(defun fold-prompt-facts (facts &aux (prompt nil))
  "The CONVERSATION-PROMPT FACTS leave standing — turn.context.prompt events,
oldest first, a pin leading (NLK:SESSION-PROMPT-FACTS) — or NIL when none
pins."
  (dolist (fact facts prompt)
    (let ((base (nlk:turn-context-prompt-base fact))
          (sections (loop for entry across (or (nlk:turn-context-prompt-sections fact) #())
                          when (and (vectorp entry) (stringp (aref entry 0)))
                            collect (cons (aref entry 0)
                                          (and (> (length entry) 1)
                                               (stringp (aref entry 1))
                                               (aref entry 1))))))
      (cond ((nlk:turn-context-prompt-pin fact)
             (setf prompt (make-conversation-prompt
                           :base (or base "")
                           :sections sections
                           :tools (or (nlk:turn-context-prompt-tools fact) #())
                           :floor (or (nlk:turn-context-prompt-floor-sequence fact) 0)
                           :told-base (or base "")
                           :told-sections sections)))
            (prompt
             (when base (setf prompt.told-base base))
             (loop for (name . text) in sections
                   do (setf prompt.told-sections (section-with prompt.told-sections name text)))
             (setf prompt.withdrawn
                   (loop for name across (or (nlk:turn-context-prompt-withdrawn fact) #())
                         when (stringp name) collect name)))))))

(defun tool-payload-name (tool)
  "The function name TOOL, one tool payload object, offers."
  (or (json-value tool :string "function" "name") (json-value tool :string "name")))

(defun unmatched-lines (lines against &aux (counts (make-hash-table :test #'equal)))
  "LINES that AGAINST does not hold, in order, a repeated line matched once
per occurrence."
  (dolist (line against) (incf (gethash line counts 0)))
  (loop for line in lines
        if (plusp (gethash line counts 0))
          do (decf (gethash line counts))
        else collect line))

(defun text-change (old new)
  "NEW as a reminder says it after OLD: the lines added (+) and removed (-),
a removed line clipped, or NEW whole when that is shorter. => (values TEXT
LINES-P)."
  (flet ((lines (text) (remove-if (lambda (line) (zerop (length (nlk:trimmed line))))
                                  (nlk:lines text))))
    (let* ((added (unmatched-lines (lines new) (lines old)))
           (removed (unmatched-lines (lines old) (lines new)))
           (diff (format nil "~{+ ~a~^~%~}~:[~;~%~]~{- ~a~^~%~}"
                         added (and added removed)
                         (mapcar (lambda (line) (nlk:clip line *reminder-removed-line-limit*))
                                 removed))))
      ;; A change of order alone has no line to name: the text goes whole.
      (if (< 0 (length diff) (length new))
          (values diff t)
          (values new nil)))))

(defun prompt-change (prompt base sections tools)
  "What changed between what PROMPT last told the model and BASE, SECTIONS
and TOOLS, the prompt it would be given now, as the keyword arguments of
NLK:RECORD-TURN-CONTEXT-PROMPT — the changed BASE, the SECTIONS changes,
the WITHDRAWN pinned tools and the reminder TEXT — or NIL when nothing did."
  ;; A tool the pin lacks is not offered until the next pin: the tools ride
  ;; ahead of everything, and one more would re-read the conversation.
  (let* ((sections (kept-sections sections))
         (base-change (and (string/= base prompt.told-base) base))
         (section-changes
           (append (loop for (name . text) in sections
                         unless (equal text (cdr (assoc name prompt.told-sections
                                                        :test #'string=)))
                           collect (cons name text))
                   (loop for (name) in prompt.told-sections
                         unless (assoc name sections :test #'string=)
                           collect (cons name nil))))
         (bound (map 'list #'tool-payload-name tools))
         (withdrawn (loop for tool across prompt.tools
                          for name = (tool-payload-name tool)
                          unless (member name bound :test #'equal)
                            collect name))
         (gone (set-difference withdrawn prompt.withdrawn :test #'equal))
         (back (set-difference prompt.withdrawn withdrawn :test #'equal)))
    (when (or base-change section-changes gone back)
      (list :base base-change
            :sections (map 'vector
                           (lambda (change) (remove nil (vector (car change) (cdr change))))
                           section-changes)
            :withdrawn (coerce withdrawn 'vector)
            :text (prompt-reminder prompt base-change section-changes gone back)))))

(defun prompt-reminder (prompt base changes gone back)
  "The <system-reminder> telling the model BASE, the new system prompt or
NIL, the section CHANGES, (NAME . TEXT) with TEXT NIL for a withdrawn one,
and the pinned tools GONE and BACK since PROMPT last told it."
  (with-output-to-string (out)
    (format out "<system-reminder>")
    (when (or base changes)
      (format out "~%Your standing context changed. Each block below replaces the one of its key you were shown before; one marked changed=\"lines\" lists only the lines added (+) and removed (-)."))
    (when base
      (multiple-value-bind (text lines-p) (text-change prompt.told-base base)
        (format out "~%~%<system-prompt~:[~; changed=\"lines\"~]>~%~a~%</system-prompt>"
                lines-p text)))
    (loop for (name . text) in changes
          for old = (cdr (assoc name prompt.told-sections :test #'string=))
          do (cond ((null text)
                    (format out "~%~%<harness key=\"~a\">(withdrawn)</harness>" name))
                   ((null old)
                    (format out "~%~%~a" (nlk:harness-section-block name text)))
                   (t
                    (multiple-value-bind (change lines-p) (text-change old text)
                      (format out "~%~%<harness key=\"~a\"~:[~; changed=\"lines\"~]>~%~a~%</harness>"
                              name lines-p change)))))
    (when gone
      (format out "~%~%No longer available: ~{~a~^, ~} — a call to one is refused." gone))
    (when back
      (format out "~%~%Available again: ~{~a~^, ~}." back))
    (format out "~%</system-reminder>")))

(nlk:access (request context-request))

;;; --- the visible turn's image attachments ---------------------------------
;;; Images belong to the message they arrived with, and to exactly one turn:
;;; the durable log keeps the text (the prompt's [Image #N] placeholders, a
;;; LOOK's one-line receipt), the attachments ride the live turn's rounds
;;; and stop there. That is the Zig visible_user_images horizon — a replayed
;;; history never re-sends megabytes of base64 the model already looked at.

(defun image-content-parts (images)
  "IMAGES as typed parts, in order: an image as a data: URI the lanes
decode; a withheld one — (:WITHHELD . NOTE), what WITHHOLD-IMAGE-GROUPS
leaves where a provider refused the image — as a text part carrying the
note, standing exactly where the image would have."
  (loop for (media-type . payload) in images
        collect (if (eq media-type :withheld)
                    (make-text-content-part payload)
                    (make-image-content-part
                     (format nil "data:~a;base64,~a" media-type payload)))))

(defun attach-visible-images (messages groups)
  "MESSAGES with GROUPS' images attached, each group to the message it
arrived with."
  ;; The NIL group — the turn's inputs — joins the newest user message, the
  ;; visible turn's input; a call-id group becomes one user message of image
  ;; parts behind the whole round of tool results its call's result sits in —
  ;; behind the round, not the one result, because the Anthropic lane merges
  ;; that round into one user turn and wants every tool_result block ahead of
  ;; any other content in it. A group with nothing to belong to is dropped —
  ;; no user message, or a call whose result is not in the window: inventing a
  ;; message would put images the model cannot place ahead of the history.
  (let* ((inserts (make-hash-table))
         (prompt-images (cdr (assoc nil groups)))
         (prompt-position (and prompt-images
                               (position-if #'user-role-message-p messages
                                            :from-end t))))
    (flet ((tool-p (message) (equal "tool" (json-value message :string "role"))))
      (loop for (anchor . images) in groups
            for at = (and anchor images
                          (position-if (lambda (message)
                                         (and (tool-p message)
                                              (equal anchor (gethash "tool_call_id" message))))
                                       messages))
            ;; one user message of image parts: the shape every lane carries
            when at
              do (push (message "user" (coerce (image-content-parts images) 'vector))
                       (gethash (or (position-if-not #'tool-p messages :start at)
                                    (length messages))
                                inserts))))
    (append (loop for message in messages
                  for index from 0
                  append (reverse (gethash index inserts))
                  collect (if (eql index prompt-position)
                              ;; the text part first, so [Image #N] placeholders read in order
                              (copy-json-object message "content"
                                                (apply #'vector
                                                       (make-text-content-part
                                                        (content-text (gethash "content" message)))
                                                       (image-content-parts prompt-images)))
                              message))
            (reverse (gethash (length messages) inserts)))))

(defun messages-with-visible-images (messages groups)
  "MESSAGES with GROUPS' images attached, or MESSAGES itself when there are
none: the one step between the retained window and the round's provider
messages, so a retarget mid-round re-derives them the way the compile did."
  (if groups (attach-visible-images messages groups) messages))

;;; --- an image the provider would not take ---------------------------------
;;; LOOK's models.dev gate refuses a model the catalog says reads text only;
;;; a model the catalog does not know — a relay's alias, a private
;;; deployment — is tried, and the provider's answer decides. That answer
;;; needs a place to land. Without a place to land, a turn carrying images
;;; the provider would not take dies of PROVIDER-ERROR. The request minus
;;; its images is the cheapest test of whether the images are the cause,
;;; and a note where each stood is how the model learns what happened. The
;;; note is live-turn state with the images' own horizon: every later round
;;; of the turn carries it, no round carries the refused bytes again, and
;;; the receipt is what history keeps.

(defun withhold-image-groups (groups note-fn &aux (count 0))
  "GROUPS with every group's images replaced by one (:WITHHELD . NOTE)
entry, NOTE the string NOTE-FN answers for that group's image count."
  ;; A note already there stands, ahead of the new one: what was staged after
  ;; a withholding is withheld in its turn. => (values GROUPS COUNT), COUNT
  ;; the images withheld — zero means nothing was left to withhold, and GROUPS
  ;; come back untouched. Pure.
  (flet ((withheld-p (image) (eq (car image) :withheld)))
    (values
     (loop for (anchor . images) in groups
           for real = (remove-if #'withheld-p images)
           collect (cond ((null real) (cons anchor images))
                         (t (incf count (length real))
                            (cons anchor
                                  (append (remove-if-not #'withheld-p images)
                                          (list (cons :withheld
                                                      (funcall note-fn (length real)))))))))
     count)))

(defun compile-turn-context (request)
  "Compile REQUEST into one round's immutable provider input."
  ;; Pure: every policy fact was frozen into REQUEST by the caller, so the
  ;; same request always compiles to the same context, and nothing here reads
  ;; a special, a session slot, or the store. The messages are the request's
  ;; own, not copies: a message is a value no seam writes to once it is made
  ;; (a writer copies the one it changes, REQUEST-MESSAGES), and the
  ;; identity is what the history memo (provider.lisp *HISTORY-MEMO*)
  ;; remembers encodings by, so a message the hydration cache keeps EQ is
  ;; encoded once for as long as it is kept.
  (let* ((history-turns request.history-turns)
         (turns (or history-turns
                    (list (make-history-turn :messages request.messages :terminal-p t))))
         (images request.visible-images)
         (retained (if history-turns
                       (messages-at-boundary turns request.history-boundary request.history-floors)
                       request.messages)))
    (multiple-value-bind (system-prompt head)
        (prompt-messages request.base-system-prompt request.harness-sections)
      ;; every other slot of the request rides into the context of its name as it is
      (record-with #'make-compiled-turn-context request
                   :system-prompt system-prompt
                   :head-message head
                   :messages (coerce (messages-with-visible-images retained images) 'simple-vector)
                   :history-turns turns))))

(defun context-with (context &key messages provider-config boundary history-turns
                               floors visible-images)
  "CONTEXT again with only the named slots replaced — the one retarget seam
(a recovery eviction re-aiming the window, a lane re-run re-freezing the
config, a continuation appending its nudge, a withholding replacing the
images a provider refused with their notes)."
  ;; COMPILED-TURN-CONTEXT is read-only through and through, so every change
  ;; is a fresh struct; each slot the caller leaves NIL is shared, and
  ;; MESSAGES, like every message in it, is taken as it is.
  (apply #'record-with #'make-compiled-turn-context context
         (nlk::present-plist :provider-config provider-config
                             :messages (and messages (coerce messages 'simple-vector))
                             :history-turns history-turns :history-boundary boundary
                             :history-floors floors :visible-images visible-images)))

