;;;; lifecycle.lisp --- kernel-owned durable turn admission and termination.
;;;;
;;;; The kernel owns lifecycle validity; evolved Lisp owns everything between
;;;; TURN.STARTED and one terminal fact. This file deliberately knows nothing
;;;; about providers, prompts, tools, or recursive control flow.

(in-package #:nodecode.kernel)

(define-condition turn-lifecycle-error (error) ())

(define-errors (turn-lifecycle-error) ()
  (duplicate-turn-command (session-id command-id)
   "command ~a was already admitted for session ~a" command-id session-id)
  (turn-already-active (session-id) "session ~a already has an active turn" session-id)
  (no-active-turn (session-id) "session ~a has no active turn" session-id)
  (expected-turn-mismatch (session-id expected actual)
   "expected turn ~a does not match active turn ~a" expected actual)
  (turn-already-terminal (turn-id) "turn ~a already has a terminal event" turn-id))

(define-record (active-input-admission (:copier nil) (:export :readers))
  "Durable result of active-turn input admission."
  (disposition (error "disposition required") :type keyword :read-only t)
  (turn nil :read-only t)
  (turn-id nil :type (or null turn-id) :read-only t)
  (prompt-id nil :type (or null string) :read-only t)
  (sequence nil :type (or null integer) :read-only t)
  (queue-depth 0 :type integer :read-only t))

(define-error turn-cancelled-condition (turn-lifecycle-error)
    (turn-id
     (reason :initform "cancelled")
     ;; Streamed-but-unrecorded assistant text at the moment the cancel
     ;; unwound the provider lane. CALL-PROVIDER's part observer fills this
     ;; via HANDLER-BIND as the signal passes, so RUN's cancel handler can
     ;; durably close the round with what streamed — the text was billed
     ;; whether recorded or not (the Zig
     ;; appendOpenAssistantMessageCompletionsLocked backstop).
     (partial-text :initarg nil :initform nil :writable t)
     ;; Streamed-but-unrecorded thinking text and in-flight tool-call fragments
     ;; at the same moment, filled the same way: the cancel must not destroy
     ;; blocks the stream already created — each persists as its durable fact
     ;; so the transcript keeps what the user watched arrive.
     (partial-reasoning :initarg nil :initform nil :writable t)
     ;; List of (:id ID :name NAME :arguments ACCUMULATED-JSON) plists in
     ;; stream index order.
     (partial-tool-calls :initarg nil :initform nil :writable t)
     ;; When the cut round was continuing an earlier cut attempt, PARTIAL-TEXT
     ;; and PARTIAL-REASONING are the merged strings (the sealed prefix plus
     ;; what streamed since) and this is the (:reasoning CHARS :content
     ;; CHARS) offset plist where the continuation begins — the
     ;; assistant-fact contract RECORD-ASSISTANT-MESSAGE's CONTINUED-FROM.
     (partial-continued-from :initarg nil :initform nil :writable t)
     ;; What the cut round can still say about itself, filled the same way:
     ;; (:provider :model :effort :response-model :input-tokens
     ;; :output-tokens :reasoning-tokens). The identity is what the round
     ;; ran under; the counts are estimates (the server never reported) and
     ;; are present only when the stream proved the server processed the
     ;; request. RUN records it as an ESTIMATED usage fact so the resting
     ;; divider of a cancelled turn reads like any other turn's.
     (partial-usage :initarg nil :initform nil :writable t))
  (:export)
  (:report "turn ~a was cancelled: ~a" turn-id reason))

(defstruct (cancellation-signal (:constructor make-cancellation-signal))
  (requested-p nil)
  (reason "cancelled")
  ;; Set once a cancel asked for the turn to be taken back as well as
  ;; stopped: if the turn ends cancelled having produced nothing, the
  ;; gateway retracts it from the visible chain. Set-only — a later plain
  ;; cancel of the same turn does not withdraw the intent.
  (retract-p nil)
  ;; Set once the operator asked from a surface (REQUEST-CANCEL-TURN's
  ;; :OPERATOR): the calls the cancel cuts are theirs to name as stopped by
  ;; the operator, where a room's stop names its reason.
  (operator-p nil)
  ;; The threads parked inside a cancellable wait for this turn — the turn
  ;; thread in its provider wait or its tool body, each background
  ;; evaluation's waiter — empty outside one. REQUEST-CANCEL-TURN interrupts
  ;; every one of them so a cancel unwinds the blocking HTTP exchange and the
  ;; bodies now instead of at the next stream chunk or the body's
  ;; return (the gateway wakeSession-on-cancel posture applied to the
  ;; provider wait).
  (waiting-threads '()))

(access (signal cancellation-signal))

(defvar *turn-cancellation-signals* (make-hash-table :test #'equal))

(defun cancellation-key (session-id turn-id)
  (cons session-id turn-id))

(defun turn-signal (turn)
  (durable-turn-cancellation-signal turn))

(defun release-turn-cancellation (turn)
  "Forget the settled turn's registry entry."
  ;; Terminal facts are the only callers: without this the registry grows by
  ;; one entry per turn for the life of the image. The signal itself stays
  ;; reachable from the turn struct, so a racer holding the turn still reads a
  ;; coherent (terminal) cancellation state.
  (sb-ext:with-locked-hash-table (*turn-cancellation-signals*)
    (remhash (cancellation-key (durable-turn-session-id turn)
                               (durable-turn-turn-id turn))
             *turn-cancellation-signals*))
  turn)

(defun turn-cancellation-requested-p (turn)
  (cancellation-signal-requested-p (turn-signal turn)))

(defun turn-cancel-cause (turn &aux (signal (turn-signal turn)))
  "Why TURN was asked to stop: (values REASON OPERATOR-P), OPERATOR-P true
when the operator asked from a surface."
  (values signal.reason signal.operator-p))

(defun turn-retract-requested-p (turn)
  "True when a cancel of TURN asked for retraction."
  ;; Read off the turn's own signal: a worker's turn struct IS the registry
  ;; entry, so no id matching.
  (cancellation-signal-retract-p (turn-signal turn)))

(defun ensure-turn-not-cancelled (turn)
  (when (turn-cancellation-requested-p turn)
    (error 'turn-cancelled-condition
           :turn-id (durable-turn-turn-id turn)
           :reason (cancellation-signal-reason (turn-signal turn)))))

;;; The interruption function's guard: a delivery that lands after the region
;;; exited (or before it was entered) must be a no-op, and this binding's
;;; dynamic extent IS the region.
(defvar *cancellable-wait-turn* nil
  "The turn whose cancellable wait region the current thread is inside, nil
elsewhere.")

(defun cancel-wait-interruption ()
  "Runs inside the waiting thread via BT2:INTERRUPT-THREAD."
  ;; Unwinds the enclosing cancellable-wait region by THROW (tag: the turn
  ;; struct) when the thread is still parked in one whose turn has a pending
  ;; cancel; every other delivery is a no-op.
  ;;
  ;; A THROW, never a signalled condition, deliberately: the interruption lands at
  ;; whatever blocking read the thread was parked in — often inside DEX:REQUEST,
  ;; whose WITH-RETRYING wraps the socket I/O in (HANDLER-BIND ((ERROR ...))) and
  ;; answers any ERROR on a reused pooled connection by redialing and resending
  ;; the whole request. A signalled cancel is such an ERROR: dexador ate it and
  ;; re-parked the thread inside this interruption's context, where interrupts
  ;; stay disabled — SBCL's "Starting a poll(2) without a timeout while
  ;; interrupts are disabled" wedge, unkillable until a second Esc landed on the
  ;; fresh (non-reused) retry connection. A THROW is invisible to every
  ;; HANDLER-BIND between the park and the region boundary; WITH-CANCELLABLE-WAIT
  ;; signals the ordinary cancel condition at that boundary, outside any foreign
  ;; handler's scope. The throw target is live exactly while
  ;; *CANCELLABLE-WAIT-TURN* is bound, because the CATCH encloses the binding.
  (when (and *cancellable-wait-turn* (turn-cancellation-requested-p *cancellable-wait-turn*))
    (throw *cancellable-wait-turn* t)))

(defun call-with-cancellable-wait (turn thunk &aux (signal (and turn (turn-signal turn))))
  "Run THUNK as TURN's interruptible waiter, or plain when TURN is nil."
  ;; A cancel that arrived before the region opened signals on entry, before
  ;; anything blocks.
  (if (null turn)
      (funcall thunk)
      (block done
        (catch turn
          (with-cleanup ((sb-ext:with-locked-hash-table (*turn-cancellation-signals*)
                           (setf signal.waiting-threads
                                 (remove (bt2:current-thread) signal.waiting-threads))))
            (sb-ext:with-locked-hash-table (*turn-cancellation-signals*)
              (ensure-turn-not-cancelled turn)
              (pushnew (bt2:current-thread) signal.waiting-threads))
            (return-from done (let ((*cancellable-wait-turn* turn)) (funcall thunk)))))
        ;; Reached only via the interruption's throw: the cancel is pending.
        (ensure-turn-not-cancelled turn))))

(defmacro with-cancellable-wait ((turn-form) &body body)
  "Run BODY — one blocking provider wait (HTTP POST, SSE read, retry sleep)
or one tool-call execution — as an interruptible region: REQUEST-CANCEL-TURN
interrupts the thread parked here so Esc unwinds the wait immediately instead
of at the next byte off the socket or the tool body's return."
  ;; TURN-FORM evaluating to nil (no durable turn executing) runs BODY plain.
  ;;
  ;; The interruption crosses BODY as a THROW — see CANCEL-WAIT-INTERRUPTION
  ;; for why a signalled condition cannot survive BODY's foreign handlers —
  ;; and the ordinary TURN-CANCELLED-CONDITION is signalled here at the region
  ;; boundary, so partial-text capture and durable closure behave exactly as a
  ;; between-chunks cancel while no HANDLER-BIND inside BODY ever sees a
  ;; condition it could swallow.
  `(call-with-cancellable-wait ,turn-form (lambda () ,@body)))

(defun turn-assistant-round-count (turn-id)
  "Count of durable assistant closures for TURN-ID — a resumed turn's
starting round index."
  ;; Boot resume re-enters the agent loop with this count so the round
  ;; progression continues where the pre-crash run left it instead of
  ;; re-arming MAX-TOOL-ROUNDS from zero.
  (check-type turn-id turn-id)
  (if (store-open-p)
      (events :turn-id turn-id :kind +kind-assistant-completed+ :as :count)
      0))

;;; The open turns are a set the log keeps (events.lisp, the open turns): a
;;; read seeks each member's start by its position, so it touches the set and
;;; the few starts in it, never the starts ever made.
(defparameter +non-terminal-turn-sql+
  (list "log_position in (select log_position from open_turns)")
  "\"This TURN.STARTED row carries no terminal fact\" — the :where tail every
active-turn read in this file hands EVENTS.")

;;; The readers below that scope by kind alone spell the kind into the
;;; statement beside it.
(defparameter +open-turn-sql+
  (list (format nil "kind = '~a' and ~a"
                +kind-turn-started+ (first +non-terminal-turn-sql+)))
  "The unscoped open-turn predicate — every TURN.STARTED row without a
terminal fact, across all sessions — with the kind spelled INTO the
statement.")

(defparameter +turn-identity-columns+
  '("turn_id" "command_id" "definition_revision" "event_id")
  "The TURN.STARTED columns that reconstruct a DURABLE-TURN, in the order
ROW-TURN destructures them.")

(defun row-turn (session-id row &key adopt)
  "The DURABLE-TURN of SESSION-ID a +TURN-IDENTITY-COLUMNS+ ROW names (columns
after those are ignored)."
  ;; ADOPT takes its cancellation signal from the registry
  ;; (installing one when absent) instead of a fresh one.
  (multiple-value-bind (turn-id command-id definition-revision started-event-id) (values-list row)
    (apply #'%make-durable-turn
           :session-id session-id :turn-id turn-id :command-id command-id
           :definition-revision (or definition-revision "unknown")
           :started-event-id started-event-id
           (and adopt
                ;; lookup-or-install in one step under the lock: the struct's slot IS the entry
                (list :cancellation-signal
                      (sb-ext:with-locked-hash-table (*turn-cancellation-signals*)
                        (alexandria:ensure-gethash (cancellation-key session-id turn-id)
                                                   *turn-cancellation-signals*
                                                   (make-cancellation-signal))))))))

(defun find-active-turn (session-id turn-id)
  "Reconstruct exact admitted non-terminal turn identified by both ids."
  (check-strings session-id turn-id)
  (when-let (row (active-turn-row session-id turn-id))
    (row-turn session-id row :adopt t)))

(defun request-cancel-turn (session-id turn-id &optional (reason "cancelled") retract)
  "Signal exactly SESSION-ID/TURN-ID. REASON is why, in words, or :OPERATOR
for the operator's own stop from a surface (TURN-CANCEL-CAUSE)."
  ;; Execution owns the terminal fact. RETRACT carries the take-it-back
  ;; intent.
  (when-let (turn (find-active-turn session-id turn-id))
    (let* ((signal (turn-signal turn))
           (operator (eq reason :operator))
           (waiting
             (sb-ext:with-locked-hash-table (*turn-cancellation-signals*)
               (setf signal.requested-p t
                     signal.reason (if operator "stopped by the operator" (princ-to-string reason))
                     signal.operator-p (or signal.operator-p operator)
                     signal.retract-p (or signal.retract-p (and retract t)))
               (copy-list signal.waiting-threads))))
      ;; Interrupt outside the lock: the interruption's own unwind re-takes it
      ;; in WITH-CANCELLABLE-WAIT's cleanup. A stale delivery is harmless — the
      ;; interruption function is a guarded no-op once the region is gone.
      (dolist (thread waiting)
        (ignore-errors (bt2:interrupt-thread thread #'cancel-wait-interruption))))
    turn))


(define-record (durable-command (:constructor make-command) (:export :constructor :readers))
  "The one command envelope accepted by every turn surface."
  (id (error "command id required") :type string :read-only t)
  (kind "turn.start" :type string :read-only t)
  (session-id (error "session id required") :type string :read-only t)
  (input (error "command input required") :type string :read-only t)
  (invocation nil :read-only t)
  (definition-revision (current-definition-revision) :type string :read-only t))

(access (command durable-command))

 (define-record (durable-turn (:constructor %make-durable-turn) (:export :readers))
   (session-id (error "session-id required") :type string :read-only t)
   (turn-id (error "turn-id required") :type string :read-only t)
   (command-id (error "command-id required") :type string :read-only t)
   (definition-revision (error "definition-revision required")
                        :type string :read-only t)
   (started-event-id nil :type (or null string) :read-only t)
   (cancellation-signal (make-cancellation-signal) :read-only t))

(access (turn durable-turn))

(defun current-definition-revision (&optional (policy-symbol 'nodecode.evolved:agent-loop))
  "Identify the evolved policy selected for the next admitted turn."
  ;; The layer revision — the organism's folder at its git HEAD, `base` when
  ;; it has none — is durable provenance for which definitions were live
  ;; (scribe.lisp's LAYER-REVISION). The symbol remains inspectable
  ;; and redefinable Lisp; the kernel does not capture or freeze its function
  ;; object.
  (format nil "~a@~a" policy-symbol (layer-revision)))

(defun active-turn-p (session-id)
  "True when SESSION-ID has an admitted turn without a terminal fact."
  ;; Inside a writer's transaction this is the durable single-active-turn
  ;; authority for every process and every admission surface; outside one it
  ;; answers NIL on a closed store.
  (check-type session-id session-id)
  (and (store-open-p)
       (events :session-id session-id :kind +kind-turn-started+
               :where +non-terminal-turn-sql+ :as :exists)))

(defun any-active-turn-p ()
  "True when any session has an admitted turn without a terminal fact: the
one durable answer to \"is the organism busy\" a restart waits on."
  ;; NIL on a closed store.
  (and (store-open-p)
       (events :where +open-turn-sql+ :as :exists)))

(defun active-turn-row (session-id &optional turn-id (active t))
  "The newest active turn's durable identity (+TURN-IDENTITY-COLUMNS+), or NIL:
TURN-ID's alone when given, active or not when ACTIVE is NIL."
  (events :session-id session-id :turn-id turn-id :kind +kind-turn-started+
          :where (and active +non-terminal-turn-sql+) :order :newest
          :as :row :columns +turn-identity-columns+))

(defun session-source-kind (session-id)
  "The invocation source kind of SESSION-ID's first turn -- the request that
began the session -- or NIL before its first turn."
  ;; A session's later turns can arrive by another road than the one that
  ;; began it: an exit wake is admitted in process, on whatever session it
  ;; wakes. What began the session stays with it.
  (when (and (stringp session-id) (store-open-p))
    (ignore-errors
     (json-value (decode-json (events :session-id session-id :kind +kind-turn-started+
                                      :as :value :columns '("invocation_json")))
                 :string "source" "kind"))))

(defun active-turn-starts ()
  "Every session with an admitted turn in flight and the log's own stamp on
its TURN.STARTED fact: ((SESSION-ID . OCCURRED-AT) ...), sessions in id
order."
  ;; The whole-organism answer to "who is running right now" from one query,
  ;; however many sessions the store holds — the durable authority a shell's
  ;; running-session column is a projection of; NIL on a closed store.
  (and (store-open-p)
       (sort (loop for (session-id at) in (events :where +open-turn-sql+
                                                   :as :rows
                                                   :columns '("session_id" "occurred_at"))
                   collect (cons session-id at))
             #'string< :key #'car)))

;;; A live projection of the active set watches these on the stream instead of
;;; re-reading the store per frame (activity.lisp).
(defparameter +turn-lifecycle-kinds+
  (list +kind-turn-started+ +kind-turn-completed+ +kind-turn-failed+
        +kind-turn-cancelled+)
  "The kinds whose arrival can change which sessions have a turn in flight:
one start and the three terminals (SUSPENDED is not here — a resumed turn
was already running).")

;;; --- turn tree ----------------------------------------------------------
;;; Turns form an append-only tree: each TURN.STARTED (schema-version 2)
;;; stamps the head turn that admitted it as PARENT-TURN-ID. The session's
;;; head is movable — SESSION.HEAD_MOVED re-points it without mutating
;;; history — and the visible transcript is exactly the head's ancestor
;;; chain. Version 1 rows predate the tree and chain linearly.

(defun session-head-turn-id (session-id)
  "The session's current head turn id, or NIL at root."
  ;; Head authority: the newest TURN.STARTED or SESSION.HEAD_MOVED row wins —
  ;; appending a turn implicitly moves the head; a head-move fact re-points
  ;; it. A freshly forked session with neither starts at its fork anchor, so
  ;; its first turn chains into the parent lineage; one still standing by
  ;; starts at the anchor its mint froze.
  (check-type session-id session-id)
  (if-let (row (events :session-id session-id
                       :kind (list +kind-turn-started+ +kind-head-moved+)
                       :order :newest
                       :as :row :columns '("kind" "turn_id" "payload")))
    (destructuring-bind (kind turn-id payload) row
      (if (string= kind +kind-turn-started+)
          turn-id
          (values (json-value (decode-event-payload-object payload) :string "target-turn-id"))))
    (or (with-json ((anchor :text "anchor-turn-id"))
            (events :session-id session-id :kind +kind-session-created+ :as :payload)
          anchor)
        (standby-session-head-turn-id session-id))))

(defun session-turn-parents (session-id &aux (parents (make-hash-table :test #'equal))
                                             (previous nil))
  "TURN-ID -> parent turn id (NIL = root) for every TURN.STARTED in SESSION-ID."
  ;; Schema-version 2 rows carry PARENT-TURN-ID in the payload, absent meaning
  ;; root. Version 1 rows predate the turn tree and chain linearly to the
  ;; immediately preceding TURN.STARTED, so a pure-linear legacy log folds to
  ;; exactly its historical shape.
  (loop for (id payload version) in
        (events :session-id session-id :kind +kind-turn-started+
                :as :rows :columns '("turn_id" "payload" "schema_version"))
        do (setf (gethash id parents)
                 (or (json-value (decode-event-payload-object payload) :string "parent-turn-id")
                     (and (< version 2) previous))
                 previous id))
  parents)

(defun chain-set-from (parents head)
  "The ancestor chain of HEAD as an EQUAL hash set (cycle-guarded walk)."
  ;; Lives beside the PARENTS reader because the walk is the only reading of
  ;; that table there is: every ancestor question in the organism — the
  ;; visible-turn set, a chain depth — is this set or its count.
  (loop with set = (make-hash-table :test #'equal)
        for turn-id = head then (gethash turn-id parents)
        while (and turn-id (not (gethash turn-id set)))
        do (setf (gethash turn-id set) t)
        finally (return set)))

(defun turn-chain-depth (session-id turn-id)
  "Count of turns on TURN-ID's ancestor chain, TURN-ID inclusive."
  ;; The chain set IS the chain, so its count is the depth.
  (hash-table-count
   (chain-set-from (session-turn-parents session-id) turn-id)))

(defun snapshot-pending-prompts (payload)
  "The pending prompt vector inside one decoded TURN.QUEUE_SNAPSHOT PAYLOAD,
#() when absent (a NIL payload included)."
  ;; Two payload shapes reach the readers — the enveloped one with a
  ;; "snapshot" object and the bare snapshot — so the unwrap happens at most
  ;; once, here, rather than at each of the three sites that need the prompts.
  (json-array (or (and (hash-table-p payload) (gethash "snapshot" payload)) payload)
              "pending_prompts"))

(defun queue-prompts (session-id &optional turn-id)
  "Return the latest durable pending prompt vector for TURN-ID, or SESSION-ID's
pending prompt vector: the newest queue snapshot's, whichever turn wrote it."
  (snapshot-pending-prompts
   (events :session-id session-id :turn-id turn-id
           :kind +kind-turn-queue-snapshot+ :order :newest :as :payload)))

(defun steer-prompt-p (prompt)
  "True for a steer parked among a queue snapshot's pending prompts."
  ;; A
  ;; follow-up carries no disposition: it waits for the running turn to finish,
  ;; then promotes into a turn of its own. A steer is the same wait made short:
  ;; the running turn ends at its next round boundary because the steer is parked
  ;; (STEER-PARKED-P), and the steer promotes ahead of every follow-up parked
  ;; before it (DEQUEUE-NEXT-TURN).
  (and (hash-table-p prompt) (equal "steer" (gethash "disposition" prompt))))

(defun follow-up-count (prompts)
  "How many of PROMPTS wait for promotion — the queue depth a client is told,
which never counts a steer (Zig pendingFollowUpCount)."
  (count-if-not #'steer-prompt-p prompts))

(defun queue-snapshot-payload (session-id turn-id prompts)
  "The session_input_queue_snapshot_updated payload: SESSION-ID's queue, PROMPTS
pending behind TURN-ID, as of the session aggregate's sequence as it now stands."
  (list :type +kind-turn-queue-snapshot+ :session_id session-id
        :snapshot (make-json-object "count" (follow-up-count prompts)
                                    "active_turn" turn-id
                                    "active_turn_steerable" t
                                    "version" (current-aggregate-sequence "session" session-id)
                                    "pending_prompts" prompts)))

(defun steer-parked-p (turn)
  "True when a steer waits in TURN's queue snapshot."
  ;; The running turn reads it as its cue: a steer is the operator asking the
  ;; turn to end at its next round boundary so the steer can run as the next
  ;; turn, promoted by DEQUEUE-NEXT-TURN ahead of every follow-up parked
  ;; before it. Admission parks it (ADMIT-ACTIVE-TURN-INPUT); nothing else
  ;; about it is written until that promotion.
  (check-type turn durable-turn)
  (and (store-open-p) (find-if #'steer-prompt-p (queue-prompts turn.session-id turn.turn-id)) t))

(defun append-turn-start-facts (handle session-id command-id input
                                &key definition-revision invocation prompt-id sequence
                                disposition steer announce occurred-at (user-message t) wake
                                metadata)
  "Mint one turn and stamp its INPUT_COMMITTED + TURN_STARTED pair under the
admit-turn write owner, register its cancellation signal, and return the
DURABLE-TURN."
  ;; OCCURRED-AT, when stated, is the pair's clock (APPEND-EVENT-ROW).
  ;;
  ;; The pair is one shape and lives here: TURN.STARTED is schema-version 2, it
  ;; carries the session's head turn as PARENT-TURN-ID, and the input fact is its
  ;; causation. The three admission surfaces — a fresh transport command, the first
  ;; input on an idle session, and a queued prompt's promotion — differ only in the
  ;; extra input-fact keys (PROMPT-ID/SEQUENCE/DISPOSITION/STEER/ANNOUNCE, each omitted when
  ;; NIL so a plain command's payload stays exactly `role'/`message') and whether an
  ;; INVOCATION rides both rows; the session is always the aggregate and the
  ;; subject. The with-event-owner wrapper is the write-authority assertion
  ;; (EVENT-KIND-OWNER, events.lisp), not decoration, so it stays inside.
  ;;
  ;; USER-MESSAGE NIL starts a turn nobody typed (ADMIT-WAKE-TURN): no input
  ;; fact at all, so no reader -- the provider context, a transcript -- ever
  ;; shows a message the operator did not write. WAKE, such a turn's cause,
  ;; rides its TURN.STARTED instead: what a transcript opens the turn on.
  ;;
  ;; METADATA is the input's display envelope, as an answer's is
  ;; (RECORD-ASSISTANT-MESSAGE): "images", the pictures sent with it, which no
  ;; provider is sent from here.
  (let* ((turn-id (make-durable-id "turn"))
         (input-event-id (make-durable-id "event"))
         (started-event-id (make-durable-id "event"))
         (head (session-head-turn-id session-id))
         (definition-revision (or definition-revision
                                  (current-definition-revision))))
    (flet ((append-row (kind payload &rest keys)
             (apply #'append-event-row handle session-id kind payload
                    :turn-id turn-id :command-id command-id :definition-revision definition-revision
                    :invocation invocation :correlation-id command-id
                    :occurred-at occurred-at keys)))
      (with-event-owner 'admit-turn
        (when user-message
          (append-row +kind-input-committed+
                      (list* :role "user" :message input
                             (present-plist :prompt-id prompt-id :sequence sequence
                                            :disposition disposition :steer (and steer t)
                                            :announce (and announce t)
                                            :metadata metadata))
                      :event-id input-event-id))
        (append-row +kind-turn-started+
                    (present-plist :input input :parent-turn-id head :wake wake)
                    :schema-version 2 :event-id started-event-id
                    :causation-id (and user-message input-event-id))))
    ;; Adopted: the fresh turn's signal is installed in the registry.
    (row-turn session-id (list turn-id command-id definition-revision started-event-id) :adopt t)))

(defmacro with-session-transaction ((handle session-id) &body body)
  "BODY in one store transaction on HANDLE that first makes SESSION-ID
durable -- a session standing by materializes there, with whatever waited on
it -- or refuses it as UNKNOWN-SESSION."
  (alexandria:once-only (session-id)
    `(with-store-transaction (,handle)
       (materialize-standby-session ,session-id :handle ,handle)
       (unless (session-created-fact-p ,session-id)
         (error 'unknown-session :session-id ,session-id))
       ,@body)))

(defun admit-wake-turn (session-id command-id &key wake)
  "Durably start a turn on idle SESSION-ID that no one typed -- the harness's
own, whose first round the harness fills (turn.lisp RUN :PREFILL): an exit
wake reads the exited evaluations for the model. WAKE is what woke it, the
exits its TURN.STARTED records (EXIT-WAKE-DOCUMENT). => the DURABLE-TURN, or
NIL when a turn is already running, which then collects the exits itself."
  (check-strings session-id command-id)
  (with-session-transaction (handle session-id)
    (unless (active-turn-row session-id)
      (append-turn-start-facts handle session-id command-id ""
                               :invocation (default-invocation "turn.start" command-id)
                               :user-message nil :wake wake))))

(defun make-active-input-turn (session-id row)
  (or (find-active-turn session-id (first row))
      (row-turn session-id row)))

(defun admit-active-turn-input (session-id command-id input
                                &key (disposition :follow-up) expected-turn-id
                                prompt-id definition-revision invocation
                                images metadata)
  "Durably classify input as STARTED, QUEUED, STEERED, or DUPLICATE."
  ;; IMAGES are the input's (MEDIA-TYPE . BASE64) attachments. The QUEUED and
  ;; STEERED dispositions record them: both wait in the durable snapshot and
  ;; must promote with the images they were staged with, while a started input
  ;; hands its images straight to the live turn (turn.lisp). METADATA, the
  ;; input's display envelope (APPEND-TURN-START-FACTS), rides every input
  ;; fact of it, and the snapshot for the one its promotion writes.
  (check-strings session-id command-id input)
  (unless (member disposition '(:follow-up :steer) :test #'eq)
    (error "unsupported active input disposition ~s" disposition))
  (let* ((prompt-id (or prompt-id command-id))
         (definition-revision (or definition-revision (current-definition-revision)))
         (invocation (or invocation
                         (default-invocation
                          (if (eq disposition :steer) "steer_turn" "turn.start")
                          command-id))))
    (validate-invocation invocation :command-id command-id)
    (with-session-transaction (handle session-id)
       ;; A session minted before its first block is standing by: this block
       ;; materializes the identity — and whatever waited on it — in this same
       ;; transaction, so the log never holds a block without one.
       (let* (;; An existing active-input fact for COMMAND-ID, if any.
              (duplicate (events :session-id session-id :command-id command-id
                                 :kind +kind-input-committed+
                                 :as :row :columns '("turn_id" "payload")))
              (active-row (and (not duplicate) (active-turn-row session-id)))
              (turn-id (first active-row))
              (steer-p (eq disposition :steer))
              (prompts (and active-row (queue-prompts session-id turn-id)))
              (parked (find command-id prompts
                            :key (lambda (prompt) (gethash "command_id" prompt))
                            :test #'equal)))
         (flet ((duplicate-of (row source)
                  ;; ROW's turn admitted again, under the prompt id and sequence SOURCE holds.
                  (make-active-input-admission
                   :disposition :duplicate
                   :turn (make-active-input-turn session-id row)
                   :turn-id (first row)
                   :prompt-id (gethash "prompt_id" source prompt-id)
                   :sequence (gethash "sequence" source 0))))
           (cond
             (duplicate
              (destructuring-bind (turn-id payload-json) duplicate
                (duplicate-of (or (active-turn-row session-id turn-id nil)
                                  (list turn-id nil nil nil))
                              (decode-event-payload-object payload-json))))
             ((and (null active-row) steer-p)
              (error 'no-active-turn :session-id session-id))
             ((null active-row)
              (let ((turn (append-turn-start-facts
                           handle session-id command-id input
                           :definition-revision definition-revision
                           :invocation invocation
                           :prompt-id prompt-id :sequence 1
                           :disposition "started" :metadata metadata)))
                (make-active-input-admission
                 :disposition :started :turn turn
                 :turn-id turn.turn-id
                 :prompt-id prompt-id :sequence 1)))
             ((and steer-p (not (equal expected-turn-id turn-id)))
              (error 'expected-turn-mismatch :session-id session-id
                     :expected expected-turn-id :actual turn-id))
             ;; A steer waits in the snapshot with no fact of its own yet, so
             ;; its re-send is caught here.
             (parked (duplicate-of active-row parked))
             (t
              (let* ((sequence (1+ (length prompts)))
                     (input-event-id (make-durable-id "event"))
                     (queued-prompt
                       (json-object "prompt_id" prompt-id
                                    "command_id" command-id
                                    "content" input
                                    "sequence" sequence
                                    :when steer-p "disposition" "steer"
                                    :when images
                                    "images" (coerce (image-content-items images) 'vector)
                                    :opt "metadata" metadata))
                     (next-prompts (concatenate 'vector prompts (vector queued-prompt)))
                     (started-event-id (fourth active-row))
                     ;; Each row under KIND's own write owner (the stamper's
                     ;; default): admit-turn for the input,
                     ;; admit-active-turn-input for the snapshot.
                     (writer (row-turn session-id (list turn-id command-id definition-revision
                                                        started-event-id))))
                ;; A follow-up's admission is on the record the moment it is
                ;; accepted: its INPUT_COMMITTED lands here and promotion replays
                ;; it as a turn of its own, unless a withdrawal takes it back
                ;; first (WITHDRAW-PENDING-PROMPT). A steer is queue state in the snapshot
                ;; alone — its images with it — until promotion starts it as the
                ;; next turn, and the running turn reads it parked there as its
                ;; cue to end at the next round boundary (STEER-PARKED-P). Its one
                ;; fact is the started input promotion writes, so the log reads
                ;; in the order the provider was asked.
                (unless steer-p
                  (append-turn-event-row
                   handle writer +kind-input-committed+
                   (list* :role "user" :message input
                          :prompt-id prompt-id :sequence sequence
                          :disposition "follow-up" (present-plist :metadata metadata))
                   :invocation invocation :event-id input-event-id))
                (append-turn-event-row
                 handle writer +kind-turn-queue-snapshot+
                 (queue-snapshot-payload session-id turn-id next-prompts)
                 :invocation invocation :event-id (make-durable-id "event")
                 :causation-id (if steer-p started-event-id input-event-id))
                (make-active-input-admission
                 :disposition (if steer-p :steered :queued)
                 :turn (make-active-input-turn session-id active-row)
                 :turn-id turn-id :prompt-id prompt-id
                 :sequence sequence
                 :queue-depth (follow-up-count next-prompts))))))))))

(defun dequeue-next-turn (session-id)
  "Atomically promote the oldest pending prompt into a fresh durable turn —
the oldest STEER first when one waits: the steer ended the turn before it so
it could run next, and the follow-ups parked earlier keep their place behind
it."
  (with-store-transaction (handle)
     (let* ((prompts (queue-prompts session-id)))
       ;; The same in-transaction single-active-turn authority ADMIT-TURN
       ;; consults: two promoters (worker cleanup, boot drain) can race here,
       ;; and the second must observe the first's TURN.STARTED and yield —
       ;; expressed as value flow because a RETURN-FROM would unwind past the
       ;; transaction's COMMIT/ROLLBACK.
       (when (and (plusp (length prompts))
                  (not (active-turn-p session-id)))
         (let* ((index (or (position-if #'steer-prompt-p prompts) 0))
                (prompt (aref prompts index))
                (input (or (gethash "content" prompt) ""))
                (command-id (or (gethash "command_id" prompt) (make-durable-id "command")))
                ;; The promoted input keeps the QUEUED prompt's identity:
                ;; the follow-up INPUT_COMMITTED already carries this
                ;; prompt id, and the transcript fold clears its pending
                ;; row by exactly this key.
                (prompt-id (or (gethash "prompt_id" prompt) command-id))
                (rest (remove-if (constantly t) prompts :start index :count 1))
                ;; A steer's started input says it was one: the turn it opens is
                ;; the operator cutting in, and a transcript marks it so.
                (turn (append-turn-start-facts
                       handle session-id command-id input
                       :prompt-id prompt-id :sequence 1 :disposition "started"
                       :steer (steer-prompt-p prompt)
                       :metadata (gethash "metadata" prompt))))
           ;; The pair above advanced the session aggregate twice, so the
           ;; snapshot's payload re-reads it and claims exactly that sequence —
           ;; one authority instead of arithmetic threaded through three appends.
           (append-turn-event-row
            handle turn +kind-turn-queue-snapshot+
            (queue-snapshot-payload session-id turn.turn-id rest))
           ;; The queued prompt's images promote with it: they were staged
           ;; with this input and belong to the turn it finally starts.
           (list turn input
                 (content-image-attachments (gethash "images" prompt))))))))

(defun withdraw-pending-prompt (session-id prompt-id)
  "Take SESSION-ID's parked prompt PROMPT-ID, a steer or a follow-up, out of
the queue, atomically.

=> (:content TEXT :images ATTACHMENTS :turn-id ID :queue-depth N) when the
newest queue snapshot still held it, NIL otherwise — the promotion won the
race, or no such id is parked.

The newest snapshot is the queue: DEQUEUE-NEXT-TURN promotes from it and
from nothing else, so one newer snapshot minus the prompt is the whole
write. A steer has no fact of its own until promotion
(ADMIT-ACTIVE-TURN-INPUT). A follow-up's admission fact (its INPUT_COMMITTED,
disposition follow-up) is queue state no provider context reads (context.lisp)
and no pending band shows once a snapshot stops naming it
(PRUNE-PENDING-STEER-ROWS); its started input, which is the conversation, is
never written. Nothing of the running turn changes here: a withdrawn steer
simply stops being seen parked and the turn runs to its own end
(STEER-PARKED-P). The snapshot is read and rewritten inside this
transaction, and DEQUEUE-NEXT-TURN reads the newest snapshot inside its
own, so the promoter either promotes from a queue that still holds the
prompt or from this one that does not — never from a stale snapshot, never
twice."
  (check-strings session-id prompt-id)
  (with-store-transaction (handle)
    (let* ((payload (events :session-id session-id
                            :kind +kind-turn-queue-snapshot+
                            :order :newest :as :payload))
           (prompts (snapshot-pending-prompts payload))
           (index (position prompt-id prompts
                            :key (lambda (prompt) (gethash "prompt_id" prompt))
                            :test #'equal))
           (prompt (and index (aref prompts index))))
      ;; A prompt the newest snapshot no longer names already promoted:
      ;; its turn and this withdrawal's intent would collide, so the caller
      ;; is told nothing was withdrawn rather than a fact being written.
      (when prompt
        (let* ((turn-id (json-value (or (gethash "snapshot" payload) payload)
                                    :string "active_turn"))
               (row (and turn-id (active-turn-row session-id turn-id nil)))
               (turn (and row (row-turn session-id row)))
               (rest (remove-if (constantly t) prompts :start index :count 1)))
          (when turn
            (append-turn-event-row
             handle turn +kind-turn-queue-snapshot+
             (queue-snapshot-payload session-id turn-id rest))
            (list :content (or (gethash "content" prompt) "")
                  :images (content-image-attachments (gethash "images" prompt))
                  :turn-id turn-id
                  :queue-depth (follow-up-count rest))))))))

(defun session-log-cursor (session-id)
  "The newest durable LOG-POSITION for SESSION-ID, or 0."
  (check-type session-id session-id)
  (if (store-open-p) (session-log-watermark session-id) 0))

(defun sessions-with-pending-prompts ()
  "Session ids whose newest durable queue snapshot still holds prompts."
  (loop for (session-id payload-json)
          in (events :kind +kind-turn-queue-snapshot+
                     :where (list "log_position = (select max(log_position)
                                                    from events as newest
                                                   where newest.session_id = events.session_id
                                                     and newest.kind = ?)"
                                  +kind-turn-queue-snapshot+)
                     :as :rows :columns '("session_id" "payload"))
        when (plusp (length (snapshot-pending-prompts
                             (decode-event-payload-object payload-json))))
          collect session-id))

(defun turn-terminal-kind (turn-id)
  "The kind string of TURN-ID's terminal fact, or NIL while it is still
open."
  ;; The durable answer to how a turn ended — a worker that caught (or
  ;; swallowed) the condition still asks the log, which also covers a turn
  ;; closed by another path.
  (check-type turn-id turn-id)
  (events :turn-id turn-id
          :kind (list +kind-turn-completed+ +kind-turn-failed+ +kind-turn-cancelled+)
          :as :value :columns '("kind")))

(defun turn-terminal-p (turn-id)
  (and (turn-terminal-kind turn-id) t))

(defun turn-answer (session-id &optional turn-id)
  "The text SESSION-ID answered with -- its newest assistant message's, of
TURN-ID alone when given -- or NIL when that says nothing: a last round that
only called tools, or no answer yet."
  ;; What a turn run in another session for this one says: a goal's judge or
  ;; attempt, a fork. A turn still running answers what it has said so far,
  ;; so a reader after the last word asks TURN-TERMINAL-KIND first.
  (check-type session-id session-id)
  (and (store-open-p)
       (fact-message-content
        (events :session-id session-id :turn-id turn-id
                :kind +kind-assistant-completed+ :order :newest :as :payload))))

(defun admit-command (command)
  "Atomically admit COMMAND and emit its input/start facts."
  (check-type command durable-command)
  (let* ((session-id command.session-id)
         (command-id command.id)
         (invocation (or command.invocation (default-invocation command.kind command-id))))
    (validate-invocation invocation :command-id command-id)
    (with-session-transaction (handle session-id)
       ;; A session minted before its first block is standing by: this block
       ;; materializes the identity — and whatever waited on it — in this same
       ;; transaction, so the log never holds a block without one.
       (when (events :session-id session-id :command-id command-id
                  :kind +kind-turn-started+ :as :exists)
         (error 'duplicate-turn-command :session-id session-id
                :command-id command-id))
       ;; This query runs inside the same IMMEDIATE transaction as the facts
       ;; below. It is the durable single-active-turn authority for every
       ;; process and every admission surface.
       (when (active-turn-p session-id)
         (error 'turn-already-active :session-id session-id))
       (append-turn-start-facts handle session-id command-id command.input
                                :definition-revision command.definition-revision
                                :invocation invocation))))
(defun admit-turn (session-id command-id input
                   &key invocation (definition-revision (current-definition-revision)))
  "Construct and admit the canonical turn.start command envelope."
  (admit-command
   (make-command :id command-id :kind "turn.start" :session-id session-id
                 :input input :invocation invocation :definition-revision definition-revision)))

(defun retained-history-floors (session-id)
  "Every provider-context floor SESSION-ID's window stands on, ascending and
distinct: the retained-from-sequence of each eviction fact in the events it
composes — its own, and each anchored ancestor's up to the anchor
(SESSION-CHAIN-SCOPE). NIL on a closed store; inside a writer's transaction,
the floors as that transaction sees them."
  ;; A fork's window is its ancestors' history, so it is their evictions too:
  ;; a Discord lane forks its room, the room is where the operator evicts, and
  ;; without those evictions a lane that reads only its own facts would stand
  ;; on floor 0 and send the room whole. The newest is the floor; the rest are
  ;; where the evicted history splits into the blocks its stub lists.
  (check-type session-id session-id)
  (when (store-open-p)
    (sort (remove-duplicates
           (remove nil
                   (mapcar #'first
                           (events :kind +kind-context-evicted+
                                   :where (session-chain-scope session-id)
                                   :index +session-kind-index+
                                   :as :rows
                                   :columns '("cast(json_extract(payload, '$.retained-from-sequence') as integer)")))))
          #'<)))

(defun session-prompt-facts (session-id)
  "SESSION-ID's turn.context.prompt facts from the newest pin on, oldest
first: its own and each anchored ancestor's up to the anchor
(SESSION-CHAIN-SCOPE), on turns its head can see. NIL on a closed store or
when nothing pins."
  ;; The scan reads two columns; only the facts from the standing pin on are
  ;; hydrated, so a room re-pinned at every eviction never decodes the pins
  ;; it left behind.
  (check-type session-id session-id)
  (when (store-open-p)
    (let* ((scope (session-chain-scope session-id))
           (visible (session-visible-turn-set session-id))
           (pin (loop for (position turn-id pin-p)
                        in (events :kind +kind-turn-context-prompt+
                                   :where scope
                                   :index +session-kind-index+
                                   :order :newest
                                   :as :rows
                                   :columns '("log_position" "turn_id"
                                              "json_extract(payload, '$.pin')"))
                      when (and (eql pin-p 1) (turn-visible-p turn-id visible))
                        return position)))
      (when pin
        (remove-if-not (lambda (event) (event-visible-p event visible))
                       (events :kind +kind-turn-context-prompt+
                               :where scope
                               :index +session-kind-index+
                               :after (1- pin)))))))

(defun retained-history-floor (session-id)
  "Return SESSION-ID's durable monotone provider-context sequence floor, the
newest of RETAINED-HISTORY-FLOORS — 0 on a closed store, and inside a
writer's transaction the floor as that transaction sees it."
  ;; Monotone over the chain: an ancestor's facts are frozen at the anchor and
  ;; the session's own only append.
  (or (car (last (retained-history-floors session-id))) 0))

(defun advance-history-floor (session-id retained-from-sequence
                              &key evicted-start-sequence evicted-end-sequence
                              (evicted-turn-count 0) (reason "budget_fill")
                              (retained-turn-count 0)
                              (evicted-message-count 0)
                              (retained-message-count 0)
                              requested-percent realized-percent
                              evicted-estimated-tokens
                              retained-estimated-tokens)
  "Advance SESSION-ID's durable provider-context floor, never backward."
  ;;
  ;; The fact names who asked (REASON: budget_fill for the automatic policy,
  ;; operator_requested for the evict control op), the source watermark the log
  ;; stood at when eviction was decided (a renderer dims rows against it), and
  ;; turn/message counts on both sides of the boundary so a divider renders
  ;; without re-folding the log. Operator evictions additionally record the
  ;; requested/realized percent and the token split the forced planner realized
  ;; (the Zig SessionContextEvictionEvent fields); the automatic policy omits
  ;; them — a budget fill has no percent contract.
  ;;
  ;; Returns (VALUES FLOOR EVENT-ID SOURCE-WATERMARK) when the floor advanced,
  ;; or just the standing floor on the monotone no-op.
  (check-type session-id session-id)
  (check-type retained-from-sequence (integer 1))
  (check-type reason string)
  (with-session-transaction (handle session-id)
     ;; A fork standing by carries context an operator can cut before its first
     ;; prompt, as its turns can be rewound (MOVE-SESSION-HEAD): the cut is its
     ;; first block.
     ;; The no-op branch returns normally: a RETURN-FROM here would unwind
     ;; past the transaction's COMMIT/ROLLBACK and leave it open.
     (let ((previous (retained-history-floor session-id)))
       (if (<= retained-from-sequence previous)
           previous
           (let ((source-watermark (session-log-watermark session-id)))
             (bind (((_ _ event-id)
                     (with-event-owner 'advance-history-floor
                       (append-event-row
                        handle session-id +kind-context-evicted+
                        (append
                         (list :retained-from-sequence retained-from-sequence
                               :evicted-start-sequence (or evicted-start-sequence
                                                           (if (plusp previous) previous 1))
                               :evicted-end-sequence (or evicted-end-sequence
                                                         (1- retained-from-sequence))
                               :evicted-turn-count evicted-turn-count
                               :reason reason
                               :source-watermark source-watermark
                               :retained-turn-count retained-turn-count
                               :evicted-message-count evicted-message-count
                               :retained-message-count retained-message-count)
                         (present-plist :requested-percent requested-percent
                                        :realized-percent realized-percent
                                        :evicted-estimated-tokens evicted-estimated-tokens
                                        :retained-estimated-tokens retained-estimated-tokens))))))
               (values retained-from-sequence event-id source-watermark)))))))

;;; --- facts deferred to the turn's next durable write ----------------------
;;; A boundary fact whose evidence is slow to gather need not hold the round.
;;; The workspace capture behind a tool result is two git spawns; without
;;; this, it would stand between every tool result and the next provider
;;; request. The evidence is gathered on a thread of its own; the FACT lands
;;; here, on the thread that appends the turn's next durable fact, ahead of
;;; it. Every turn-fact recorder in this file settles first, so the log reads
;;; exactly as if the walk held the round: the capture, then the round
;;; that followed it. The prompt fact alone does not (RECORD-TURN-FACT): it is
;;; written just before the request the walk overlaps, and holding it would
;;; hold that request for the whole walk.
;;; Settlement runs outside the recorder's store transaction — the thunk
;;; records its own fact in its own.

(defvar *deferred-turn-facts* (make-hash-table :test #'equal)
  "turn-id -> thunks, newest first, each of which records one durable fact
about that turn and must run before the turn's next durable fact is
appended.")

(defun defer-turn-fact (turn thunk)
  "Arrange for THUNK — which records one durable fact about TURN — to run
before TURN's next durable fact is appended, on whichever thread appends
it, or when SETTLE-TURN-FACTS is called outright."
  ;; The one seam for evidence gathered off the round's thread: the durable
  ;; order stays the order the round saw.
  (check-type turn durable-turn)
  (sb-ext:with-locked-hash-table (*deferred-turn-facts*)
    (push thunk (gethash turn.turn-id *deferred-turn-facts*))))

(defun settle-turn-facts (turn)
  "Run TURN's deferred fact thunks, oldest first, and forget them."
  ;; TURN is the durable turn or its id. A thunk that signals is reported and
  ;; dropped: the fact it stood for was never a turn precondition. => how many
  ;; ran.
  (let* ((turn-id (if (stringp turn) turn turn.turn-id))
         (thunks (sb-ext:with-locked-hash-table (*deferred-turn-facts*)
                   (prog1 (reverse (gethash turn-id *deferred-turn-facts*))
                     (remhash turn-id *deferred-turn-facts*)))))
    (dolist (thunk thunks (length thunks))
      (handler-case (funcall thunk)
        (error (condition)
          (warn "deferred turn fact skipped: ~a" condition))))))

(defun append-turn-event-row (handle turn kind payload
                             &key owner event-id invocation
                             (causation-id turn.started-event-id)
                             occurred-at &aux (command-id turn.command-id))
  "Append PAYLOAD as one KIND row against TURN, stamping the envelope every
turn-bearing durable write shares: the session as aggregate and subject, the
turn and command ids, the definition revision, the started fact as causation,
the command id as correlation."
  ;; Hand-written it was eight lines per writer, and a write that forgot a key
  ;; still compiled and still wrote a row that folds and replays wrong.
  ;;
  ;; OWNER defaults to KIND's registered write owner and is passed only where
  ;; a writer's authority is not its own name (RECORD-ASSISTANT-MESSAGE writes
  ;; under COMPLETE-TURN) — EVENT-KIND-OWNER (events.lisp) is a
  ;; write-authority table, not decoration, so the stamper takes the owner
  ;; rather than widening it. CAUSATION-ID likewise defaults to the start fact
  ;; and is passed as NIL by the one writer whose row the start did not cause.
  (with-event-owner (or owner (event-kind-owner kind))
    (append-event-row handle turn.session-id kind payload
                      :turn-id turn.turn-id
                      :command-id command-id
                      :definition-revision turn.definition-revision
                      :invocation invocation
                      :event-id event-id
                      :causation-id causation-id
                      :correlation-id command-id
                      :occurred-at occurred-at)))

(defun record-assistant-message (turn message &key continued-from metadata)
  "Durably append one complete provider assistant message in wire shape."
  ;; CONTINUED-FROM marks a round that continued a cut attempt: its
  ;; (:reasoning CHARS :content CHARS) offsets, where the continuation begins
  ;; in the merged strings, ride in the payload after the wire MESSAGE.
  ;; METADATA, a JSON object, is the message's display envelope, as a tool
  ;; result's is: what a transcript draws beside the text and no provider is
  ;; sent -- "images", the pictures the answer names. Retention: persisted
  ;; history, the live stream's and a replay's alike.
  (unless (hash-table-p message)
    (error "assistant message must be a JSON object"))
  (record-turn-fact turn +kind-assistant-completed+ 'complete-turn
                    (list* :role "assistant" :message message
                           (nconc (when continued-from
                                    (list :continued-reasoning-chars (getf continued-from :reasoning 0)
                                          :continued-content-chars (getf continued-from :content 0)))
                                  (when metadata (list :metadata metadata))))))

(defun append-turn-terminal-rows (handle turn kind payload assistant-message &optional occurred-at)
  "Append TURN's terminal KIND row in the caller's transaction, after (and caused
by) ASSISTANT-MESSAGE's completion row when there is one, else by the start fact."
  (let ((assistant-id (when assistant-message (make-durable-id "event"))))
    (when assistant-message
      (append-turn-event-row handle turn +kind-assistant-completed+
                             (list :role "assistant" :message assistant-message)
                             :owner 'complete-turn :event-id assistant-id :occurred-at occurred-at))
    (append-turn-event-row handle turn kind payload :occurred-at occurred-at
                           :causation-id (or assistant-id turn.started-event-id))))

(defun record-turn-terminal (turn kind payload &key assistant-message)
  (check-type turn durable-turn)
  (settle-turn-facts turn)
  (multiple-value-prog1
      (with-store-transaction (handle)
         (unless (string= kind +kind-turn-cancelled+)
           (ensure-turn-not-cancelled turn))
         (when (turn-terminal-p turn.turn-id) (error 'turn-already-terminal :turn-id turn.turn-id))
         ;; A steer still parked when the turn ends stays queue state: it is the
         ;; next turn, promoted by DEQUEUE-NEXT-TURN once the worker settles,
         ;; ahead of every follow-up parked before it.
         (append-turn-terminal-rows handle turn kind payload assistant-message))
    ;; Reclaim only after the terminal fact is durable: a failed commit keeps
    ;; the registration so cancellation still reaches the still-live turn.
    (release-turn-cancellation turn)))
 ;; cancellation authority is exact session+turn via REQUEST-CANCEL-TURN.

(defun complete-turn (turn assistant-message &key (message-count 0) steered
                                        background-pending)
  "TURN's completion."
  ;; STEERED marks a turn that ended at a round boundary it would otherwise
  ;; have continued past, because a steer was parked (turn.lisp AGENT-LOOP):
  ;; the divider it rests on reads `steered', not `completed'.
  ;;
  ;; BACKGROUND-PENDING, when positive, is the count of background work this
  ;; turn left in flight when it ended: the turn's last word is not its answer
  ;; — the result arrives in a later turn, carried by the exit wake — and the
  ;; fact says so for every consumer that reads turns.
  ;; Turn numbers are chain depth, not a completed-fact count: on a branch a
  ;; sibling takes the number of the turn it replaced instead of inflating.
  (record-turn-terminal turn +kind-turn-completed+
                        (list* :turn-number (turn-chain-depth turn.session-id turn.turn-id)
                               :message-count message-count
                               (present-plist :steered (and steered t)
                                              :background-pending
                                              (and (integerp background-pending)
                                                   (plusp background-pending)
                                                   background-pending)))
                        :assistant-message assistant-message))

(defun fail-turn (turn condition &key evidence)
  "TURN's terminal failure fact: the condition's type and rendering, plus
EVIDENCE, a payload plist the caller distilled from the condition (the
engine's PROVIDER-FAILURE-EVIDENCE: status, content type, request digest,
the head of the response body)."
  ;; Debug-only retention on the fact, so a session that failed and was never
  ;; opened in a debugger still says what the provider answered. The detail is
  ;; the condition's message, every line of it, without the `See also:' block
  ;; SBCL's printer appends: references to a manual's nodes, which every
  ;; surface showing the detail would print as if they were the error.
  (record-turn-terminal turn +kind-turn-failed+
                        (nconc (list :condition-type (princ-to-string (type-of condition))
                                     :detail (let ((sb-int:*print-condition-references* nil))
                                               (princ-to-string condition)))
                               (copy-list evidence))))

(defun cancel-turn (turn reason)
  (record-turn-terminal turn +kind-turn-cancelled+
                        (list :reason (princ-to-string reason))))

(defparameter +exchange-fact-keys+
  '((:recorder . string) (:provider . string) (:model . string)
    (:elapsed-ms . (integer 0)) (:input-tokens . (integer 0))
    (:cached-input-tokens . (integer 0)) (:output-tokens . (integer 0))
    (:reasoning-tokens . (integer 0)))
  "The facts a recorded exchange may carry about the work behind it, each
with its type: who recorded the pair, what that work ran, how long it took,
what it cost.")

(defun record-exchange-turn (session-id input answer
                             &key command-id facts occurred-at announce (source "in_process"))
  "Append one already-settled exchange to SESSION-ID as a complete turn:
INPUT_COMMITTED, TURN_STARTED, ASSISTANT_MESSAGE_COMPLETED, TURN_COMPLETED,
in one transaction, with no provider round between them."
  ;; Returns the turn id.
  ;;
  ;; OCCURRED-AT, when stated, is when the exchange settled — UTC ISO-8601 —
  ;; and all four facts carry it: a recorder folding in an exchange that
  ;; happened elsewhere, earlier, keeps the log in the order things happened.
  ;; Unstated, the facts are stamped now, as every live write is.
  ;;
  ;; FACTS, when the recorder has them, are what the work behind the pair was
  ;; (+EXCHANGE-FACT-KEYS+: :recorder, :provider, :model, :elapsed-ms, the
  ;; token counts), carried on the completion for the transcript's divider.
  ;;
  ;; ANNOUNCE, when true, marks the pair as spoken to whatever watches the
  ;; session rather than only filed in it: the input fact carries `announce',
  ;; so a reader that surfaces the session — a channel lane — may hand the
  ;; answer to the people there as a note. Absent (the default), the exchange
  ;; is history and no surface posts it.
  ;;
  ;; SOURCE is the invocation source kind the turn is recorded under, the
  ;; kernel's default provenance unless the recorder is a road of its own,
  ;; and its session reads so wherever a session's source is read
  ;; (SESSION-SOURCE-KIND).
  ;;
  ;; The write-back seam for a session that keeps a shared record but never
  ;; runs a turn of its own. A caller that produced ANSWER elsewhere — in a
  ;; fork of this session, in another process — folds the pair back in as
  ;; history. The turn chains onto the head like any other, so the session
  ;; stays forkable at the exchange and the next fork anchors above it; the
  ;; pair is written whole, so no reader ever observes a half-recorded
  ;; exchange or an active turn here.
  ;;
  ;; INPUT is one user message and ANSWER one assistant message: a recorder
  ;; that has several speakers' lines to contribute composes them into INPUT
  ;; rather than appending several user turns, because a strict chat template
  ;; rejects the consecutive-user shape that would produce.
  (check-strings session-id input answer)
  (let* ((command-id (or command-id (make-durable-id "command")))
         (invocation (default-invocation "turn.record_exchange" command-id :source source))
         ;; an absent fact is absent; an unknown key or mistyped value refuses before any write
         (extra (loop for (key value) on facts by #'cddr
                      for type = (or (cdr (assoc key +exchange-fact-keys+))
                                     (error 'type-error
                                            :datum key
                                            :expected-type
                                            (cons 'member (mapcar #'car +exchange-fact-keys+))))
                      when value
                        do (unless (typep value type)
                             (error 'type-error :datum value :expected-type type))
                        and append (list key value))))
    (with-session-transaction (handle session-id)
       ;; A session minted before its first block is standing by: this block
       ;; materializes the identity — and whatever waited on it — in this same
       ;; transaction, so the log never holds a block without one.
       (let* ((turn (append-turn-start-facts
                     handle session-id command-id input
                     :invocation invocation :prompt-id command-id :sequence 1
                     :disposition "recorded"
                     :announce announce
                     :occurred-at occurred-at))
              (turn-id turn.turn-id))
         (append-turn-terminal-rows
          handle turn +kind-turn-completed+
          (append (list :turn-number (turn-chain-depth session-id turn-id)
                        :message-count 1)
                  extra)
          (make-json-object "role" "assistant" "content" answer)
          occurred-at)
         ;; Registered by APPEND-TURN-START-FACTS for a turn that was already
         ;; terminal when it was written: nothing can ever cancel it.
         (release-turn-cancellation turn)
         turn-id))))

(defun turn-origin (wake recorder)
  "What began a turn no one asked, as the wire word the listings name it by, or
NIL for a turn someone did: \"wake\" for a turn an exit woke (WAKE, the cause
its TURN.STARTED records: ADMIT-WAKE-TURN), \"reflection\" for the recap a
reflection recorded behind the turn it reflected on (RECORDER, its
TURN.COMPLETED's: RECORD-EXCHANGE-TURN)."
  ;; The one rule of it: the checkpoint listing and the timeline both number
  ;; and word turns by it, so a turn the operator did not ask is never counted
  ;; as one they did. A pair another recorder wrote -- an import, a room's
  ;; write-back -- is somebody's ask, and stays one.
  (cond ((plusp (length wake)) "wake")
        ((equal recorder "reflection") "reflection")))

(defparameter +note-source+ "experience"
  "The source kind a session the organism keeps for itself begins with: a
reflection's fork of the turn it reflects on. What runs there is the
organism's note on the operator's work, not more of it -- the counts of what
the operator did leave it out, as the web page's session list does, and its
spend is the session's it hangs from.")

;;; --- non-terminal turn facts ---------------------------------------------
;;;
;;; Everything a running turn journals about itself — usage, provider rounds,
;;; retries, tool calls, tool results — is the same durable
;;; write with a different payload: refuse a terminal turn, open one
;;; transaction, name the write owner, and stamp the eight-field turn envelope
;;; (aggregate, subject, turn, command, definition revision, causation,
;;; correlation) onto the row. RECORD-TURN-FACT is that write; each recorder is
;;; declared where its fact is (events.lisp DEFINE-EVENT :RECORD), so the
;;; payload a recorder writes and the slots a reader hydrates are one list.

(defun record-turn-fact (turn kind owner payload)
  "Append PAYLOAD as one non-terminal KIND fact of TURN under OWNER, TURN's
deferred facts settled first (a prompt fact excepted) and a terminal TURN
refused."
  (check-type turn durable-turn)
  ;; A prompt fact is written just before the provider request it describes,
  ;; the request a deferred workspace walk is there to overlap: it does not
  ;; wait for that walk, and stands ahead of the capture fact. Every other
  ;; fact of the turn settles first.
  (unless (equal kind +kind-turn-context-prompt+)
    (settle-turn-facts turn))
  (with-store-transaction (handle)
    (when (turn-terminal-p turn.turn-id)
      (error 'turn-already-terminal :turn-id turn.turn-id))
    (append-turn-event-row handle turn kind payload :owner owner)))

(defun append-session-fact (session-id kind payload &rest options)
  "Durably append one KIND fact of PAYLOAD to SESSION-ID, under KIND's own
write owner, OPTIONS APPEND-EVENT-ROW's keywords. => (values LOG-POSITION ...)
as APPEND-EVENT-ROW answers."
  (check-strings session-id)
  (with-store-transaction (handle)
    (with-event-owner (event-kind-owner kind)
      (apply #'append-event-row handle session-id kind payload options))))

(defun exited-shell-document (shell-session-id command exit-code
                              &key shell-exit tail tail-cut error stopped)
  "The background_shell document of a shell that exited: what its durable
exit fact carries and what a live exited frame folds into the call's rows --
one shape for both. EXIT-CODE is the evaluation's, SHELL-EXIT its last (sh
...)'s status, TAIL the last lines it wrote and TAIL-CUT whether it wrote
more before them, ERROR the report of the condition it failed on, STOPPED
who stopped it when someone did (the engine's stop document); an unknown
one is omitted, never invented (the Zig shellMetadataJson null-omission
shape)."
  (json-object "shell_session_id" shell-session-id
               "status" "exited"
               "command" command
               :when (integerp exit-code) "exit_code" exit-code
               :when (integerp shell-exit) "shell_exit" shell-exit
               :opt "tail" tail
               :when tail-cut "tail_cut" t
               :opt "error" error
               :opt "stopped" stopped))

(defun record-background-shell-exit (session-id turn-id call-id command
                                     shell-session-id exit-code
                                     &key shell-exit tail tail-cut error stopped workspace-diff)
  "Durable close of one backgrounded shell lifecycle: a TURN.TOOL_RESULT fact
on the owning turn, keyed by the run call's CALL-ID and carrying the exited
background_shell metadata (the Zig recordBackgroundShellExitFact contract:
result text \"\", tool name \"snippet\" — protocol/background_exec.zig
exit_fact_tool_name)."
  ;; Retention: persisted-history — replay rebuilds the transcript row's final
  ;; state from this fact; without it the durable snapshot would report
  ;; running forever.
  ;;
  ;; Deliberately OMITS the terminal-turn check RECORD-TOOL-RESULT applies: a
  ;; background exit usually lands after its turn completed, and refusing it
  ;; would strand every backgrounded shell's durable state at running. No new
  ;; event kind: the fact folds through the existing tool-result replay path
  ;; under the sealed RECORD-TOOL-RESULT owner.
  ;;
  ;; SHELL-EXIT, TAIL, TAIL-CUT, ERROR and STOPPED are EXITED-SHELL-DOCUMENT's: how
  ;; the shell ended and its last lines, so a card closed on replay reads as
  ;; the one closed live.
  ;;
  ;; WORKSPACE-DIFF, when given, is the detached window's cumulative diff
  ;; document (CLOSE-DETACHED-WORKSPACE-WINDOW), carried under the metadata's
  ;; workspace_diff key exactly as the yield-time bracket carries it — the
  ;; fold replaces the row's metadata, so the completion document supersedes
  ;; the yield-window one. => the appended fact's log position.
  (check-strings session-id turn-id call-id command)
  (check-type shell-session-id (integer 0))
  (let ((metadata (json-object
                   "background_shell"
                   (exited-shell-document shell-session-id command exit-code
                                          :shell-exit shell-exit :tail tail :tail-cut tail-cut
                                          :error error :stopped stopped)
                   :opt "workspace_diff" workspace-diff)))
    (append-session-fact session-id +kind-tool-result+
                         (list :call-id call-id :tool-name "snippet" :result "" :metadata metadata)
                         :turn-id turn-id)))

(defparameter *recovery-boot-id* nil)

(defparameter *max-turn-resume-attempts* 3
  "Boots that may claim one suspended turn before it is failed as unrecoverable.")

(defun claim-turn-resumption (turn)
  "Atomically claim a suspended TURN, at most once per process boot."
  ;; A crash mid-resume must not tombstone the turn: a later boot claims it
  ;; again. After *MAX-TURN-RESUME-ATTEMPTS* boots the turn is terminalized as
  ;; TURN.FAILED instead, so a turn whose resume keeps dying cannot wedge
  ;; admission forever — and its adopted cancellation registration is released
  ;; only AFTER that terminal fact commits, mirroring RECORD-TURN-TERMINAL's
  ;; reclaim ordering. No RETURN-FROM crosses the transaction: a non-local
  ;; exit would skip both COMMIT and ROLLBACK and leave the store handle
  ;; inside an open transaction.
  (let* ((boot-id (or *recovery-boot-id*
                      (setf *recovery-boot-id* (make-durable-id "boot"))))
         (turn-id turn.turn-id)
         (verdict
           (with-store-transaction (handle)
              (let ((claims (events :turn-id turn-id
                                    :kind +kind-turn-resume-claimed+
                                    :order :newest :as :payloads)))
                (cond
                  ((turn-terminal-p turn-id) nil)
                  ((and claims (equal boot-id (gethash "boot_id" (first claims)))) nil)
                  ((>= (length claims) *max-turn-resume-attempts*)
                   (append-turn-event-row
                    handle turn +kind-turn-failed+
                    (list :condition-type "turn-resume-exhausted"
                          :detail (format nil "turn was not resumed after ~d boots"
                                          (length claims))))
                   :exhausted)
                  (t
                   ;; No causation: a claim is caused by this boot, not by the
                   ;; start fact the envelope would otherwise point at.
                   (append-turn-event-row
                    handle turn +kind-turn-resume-claimed+
                    (list :boot_id boot-id :attempt (1+ (length claims))
                          :claim_token boot-id)
                    :causation-id nil)
                   turn))))))
    (when (eq verdict :exhausted)
      (release-turn-cancellation turn))
    (and (typep verdict 'durable-turn) verdict)))

(defun recoverable-interrupted-turns ()
  "Return (TURN INPUT) pairs marked for resume by the durable recovery pass."
  ;; Each turn's cancellation signal is ADOPTED from the registry (installing
  ;; on absence), never a bare struct default: a resumed worker polls the
  ;; struct's own slot, so a bare signal would leave /cancel flagging a
  ;; registry orphan while replying "accepted" — the boot-resume
  ;; unkillable-turn hole. Adoption also inherits a cancel that raced in
  ;; before this pass listed the turn.
  ;; Each row is the session id, the turn's identity, then its payload.
  (loop for (session-id . identity)
          in (events :where (list (concatenate
                                   'string
                                   (first +open-turn-sql+)
                                   " and exists (select 1 from events as suspended
                                                where suspended.turn_id = events.turn_id
                                                  and suspended.kind = ?)")
                                  +kind-turn-suspended+)
                     :as :rows
                     :columns (list* "session_id" (append +turn-identity-columns+
                                                          '("payload"))))
        collect (list (row-turn session-id identity :adopt t)
                      (or (gethash "input" (decode-event-payload-object (fifth identity))) ""))))

(defparameter +restart-tool-result+ "interrupted by process restart"
  "The result boot recovery records for a tool call the dead process never
answered.")

(defun recover-interrupted-turns ()
  "Mark orphaned starts resumable; execution is owned by the gateway worker."
  ;; Every open tool call of an orphaned turn closes first, so the resumed
  ;; turn's wire is well-formed before its first round.
  (dolist (row (events :where +open-turn-sql+
                       :as :rows
                       :columns (cons "session_id" +turn-identity-columns+)))
    (let ((turn (row-turn (first row) (rest row))))
      ;; every unanswered call gets a result fact: a reply-less tool_calls round is refused
      (with-store-transaction (handle)
        (let ((answered (make-hash-table :test #'equal))
              (turn-id turn.turn-id))
          (dolist (payload (events :turn-id turn-id :kind +kind-tool-result+
                                   :as :payloads))
            (setf (gethash (gethash "call-id" payload) answered) t))
          (loop for payload in (events :turn-id turn-id :kind +kind-tool-call-started+
                                       :as :payloads)
                for call-id = (gethash "call-id" payload)
                unless (or (not (stringp call-id)) (gethash call-id answered))
                  do (setf (gethash call-id answered) t)
                     (append-turn-event-row
                      handle turn +kind-tool-result+
                      (list :call-id call-id
                            :tool-name (or (json-value payload :string "tool-name") "unknown")
                            :result +restart-tool-result+)))))
      ;; Then one idempotent non-terminal suspension marker.
      (with-store-transaction (handle)
         (unless (events :turn-id turn.turn-id :kind +kind-turn-suspended+ :as :exists)
           (append-turn-event-row handle turn +kind-turn-suspended+
                                  (list :reason "process restart" :resumable t)))))))

(pushnew #'recover-interrupted-turns *store-open-hooks*)
