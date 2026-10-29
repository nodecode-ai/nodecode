;;;; client.lisp --- WebSocket TUI client. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Connects to a running gateway, subscribes to a session, and presents
;;;; the same chat interface as run-chat but drives it through
;;;; gateway events instead of running turns in-process.
;;;;
;;;; Threading model:
;;;;   - Main thread: the program loop (NLT:RUN: the loop and the renderer).
;;;;   - WS read thread: started by wsd:start-connection; receives inbound
;;;;     frames and sends its own messages to the program via
;;;;     nlt:send.  No program state is touched from this thread beyond
;;;;     the send call, which is thread-safe.
;;;;   - On Enter key: instead of queueing a turn on the worker, the update
;;;;     method calls wsd:send to send a start_turn command to the gateway.

(in-package #:nodecode.evolved)

;;; --- gateway-chat-model -------------------------------------------------

;;; Defined at compile time too, so the ACCESS below gives MODEL its slots.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gateway-chat-model (chat-model)
    ((ws :initarg :ws :accessor gateway-model-ws)
     (last-sequence :initform nil :accessor gateway-model-last-sequence)
     (last-cursor :initform nil :accessor gateway-model-last-cursor)
     (active-turn-id :initform nil :accessor gateway-model-active-turn-id)
     ;; The turn Esc asked to stop, by id, until its terminal fact settles
     ;; the flight (TURN-STOPPING-P): the bar says so in the gesture's own
     ;; frame. Live-only.
     (stop-requested :initform nil :accessor gateway-model-stop-requested)
     (pending-command-id :initform nil :accessor gateway-model-pending-command-id)
     (pending-correlation-id :initform nil :accessor gateway-model-pending-correlation-id)
     (pending-prompt :initform nil :accessor gateway-model-pending-prompt)
     (replay-pending-turn-id :initform nil :accessor gateway-model-replay-pending-turn-id)
     ;; The state a replay in flight folds into, out of sight, until its last
     ;; chunk puts it in place (SHOW-REPLAYED-STATE).
     (replay-staging :initform nil :accessor gateway-model-replay-staging)
     (reconnect-required :initform nil :accessor gateway-model-reconnect-required)
     ;; The sync-lifecycle chrome state (the Zig SyncLifecycleKind subset the
     ;; lisp transport can actually be in): :connecting :connected
     ;; :reconnecting :failed. Footer suffix + empty-transcript text input.
     (sync-lifecycle :initform :connecting
                     :accessor gateway-model-sync-lifecycle)
     ;; Internal-time of the last bare Esc on an idle shell — the Esc-Esc
     ;; checkpoint-focus gesture's first half.
     (last-escape-at :initform nil :accessor gateway-model-last-escape-at)
     ;; The composer as it stood at this client's last start_turn — plist
     ;; (:text :pastes :images :command-id :turn-id) — so a retracted turn can
     ;; hand the prompt back exactly as typed (markers, pastes, staged images).
     ;; Also the ownership proof: Esc asks for retraction only on a turn this
     ;; composer started. Live-only; cleared when the turn completes or fails.
     (retract-stash :initform nil :accessor gateway-model-retract-stash)
     ;; Esc landed before the start_turn was acknowledged: the retracting
     ;; cancel waits for the turn id (NOTE-TURN-ADMITTED).
     (retract-requested :initform nil :accessor gateway-model-retract-requested)
     ;; A retraction this client asked for is being resynced: the head-move
     ;; notices stay quiet — the composer refill is the whole story.
     (quiet-resync :initform nil :accessor gateway-model-quiet-resync)
     ;; The HTTP control endpoint this client's slash commands call: the same
     ;; port and bearer the sync connection authenticated with.
     (http-port :initarg :http-port :initform nil
                :accessor gateway-model-http-port)
     (http-token :initarg :http-token :initform nil
                 :accessor gateway-model-http-token)
     ;; Cached gateway-owned model facts for this client's session — a plist
     ;; (:provider :model :pinned :effort), or :UNRESOLVED before the first
     ;; read. A client that merely ATTACHED to someone else's gateway owns no
     ;; store and no restored provider globals, so the gateway's target
     ;; (GET /api/gateway/target) is the only authority for the identity a
     ;; turn here would bind.
     ;; The footer renders every frame and must not call out each time; the
     ;; gateway fact stays the authority — a fresh connection, a session
     ;; switch, and every model or effort pick this client lands are
     ;; the refresh points (INVALIDATE-GATEWAY-MODEL-FACTS).
     (model-facts :initform :unresolved :accessor gateway-model-model-facts)
     ;; The session the preview lane follows live (PREVIEW-FOLLOW): a second,
     ;; live-only subscription beside the attached one, whose frames reduce
     ;; into the lane instead of the live state. NIL while no preview is
     ;; armed. Read on the reader thread by the dispatch filter.
     (preview-session-id :initform nil :accessor gateway-model-preview-session-id)
     ;; The (VERB KIND STAMPED) the previewed stream last named, the shape and
     ;; internal clock of the attached TURN-ACTIVITY, for the bar under the
     ;; dialog (BAR-LANE); NIL between turns.
     (preview-activity :initform nil :accessor gateway-model-preview-activity)
     ;; Monotonic guard for the facts cache: a refresh captures the epoch at
     ;; spawn and its answer installs only while still newest.
     (facts-epoch :initform 0 :accessor gateway-model-facts-epoch))
    (:documentation "Chat model backed by one UI-thread Gateway reducer.")))

(nlk:access (awaiting picker-dialog) (existing bg-band-row) (model gateway-chat-model)
            (prog nlt::program) (removed bg-band-row))

;;; --- typed messages from the WS reader --------------------------

(nlt:defmessage gw-projection-msg
    ((frame :initarg :frame :reader gw-projection-msg-frame)
     (release :initarg :release :initform nil :reader gw-projection-msg-release))
  :documentation "One validated sync projection delivered to the UI thread.")

(nlt:defmessage gw-transport-error-msg
    ((text :initarg :text :reader gw-transport-error-msg-text)
     ;; REPORT-TRANSPORT-ERROR's VERDICT: :PERMANENT (the lifecycle reads
     ;; :failed and the bar settles), NIL (:reconnecting), or an explicit
     ;; sync-lifecycle verdict from the reconnect ladder. :FAILED here is a
     ;; user-visible verdict, not a stop — the ladder keeps healing.
     (verdict :initarg :verdict :reader gw-transport-error-msg-verdict))
  :documentation "A transport/protocol error delivered to the UI thread.")

(defun report-transport-error (program verdict control &rest args)
  "Post one transport notice to PROGRAM's update loop."
  ;; The reader thread and the reconnect ladder both report through here, so
  ;; the notice's shape is a fact of this function rather than of eleven
  ;; hand-rolled MAKE-INSTANCE forms. VERDICT is :PERMANENT, an explicit
  ;; :FAILED or :RECONNECTING, or NIL; the notice's text is (FORMAT NIL
  ;; CONTROL ARGS...).
  (nlt:send program (make-instance 'gw-transport-error-msg
                                   :text (apply #'format nil control args)
                                   :verdict verdict)))

;;; --- off the loop --------------------------------------------------------
;;; A shell reads the gateway, the config and the machine on threads of their
;;; own: the loop thread paints, and a dial run on it would stall every
;;; frame. One primitive says the whole shape — run this off the loop,
;;; install what it answered back ON the loop — so a reader needs neither a
;;; message class nor an UPDATE method of its own.

(nlt:defmessage off-loop-msg
    ((value :initarg :value :reader off-loop-msg-value)
     (continuation :initarg :continuation :reader off-loop-msg-continuation))
  :documentation "A thunk finished off the loop; UPDATE hands its value to
the continuation on the loop.")

(defun off-loop (model thunk continuation &aux (program model.program))
  "Run THUNK off the loop and hand its value — or (:error TEXT) when it
signalled — to CONTINUATION as (MODEL VALUE) on the loop. A continuation
that answers :QUIET moved nothing on screen, and the loop owes no frame."
  ;; Without a live program (tests drive UPDATE directly and must not spawn
  ;; threads) it runs here, in order.
  (flet ((safely ()
           (handler-case (funcall thunk)
             ;; A refusal is shown by its detail, the words that name the key.
             (nlk:config-refusal (condition)
               (list :error (nlk:config-refusal-detail condition)))
             (error (condition) (list :error (princ-to-string condition))))))
    (if program
        (spawn "nodecode-off-loop"
          (nlt:send program (make-instance 'off-loop-msg
                                           :value (safely)
                                           :continuation continuation)))
        (funcall continuation model (safely)))))

(defun off-loop-failure (value)
  "The text an OFF-LOOP value carries when the thunk signalled, else NIL."
  (and (consp value) (eq (first value) :error) (second value)))

(defmethod update ((model chat-model) (msg off-loop-msg))
  (values model nil
          (eq :quiet (funcall (off-loop-msg-continuation msg) model (off-loop-msg-value msg)))))

(defun initial-chat-state ()
  "Return the authoritative seed for a shell's initialization: the empty
state the gateway's replay fills, since a shell never folds a store."
  (json-object "history_rows" '()
               "tail_rows" '()
               "facts" '()))

;;; --- typed command helpers ----------------------------------------------

(define-error gateway-not-connected (error) ()
  (:report "this shell is not connected to its gateway")
  (:documentation "A command asked of a shell whose link cannot carry it:
it never left this process."))

(defun gateway-link-open-p (ws)
  "Whether WS, a shell's link, carries a frame handed to it now: NIL before
the first attach, and once the gateway or the socket hung it up."
  ;; The send's own rule (GATEWAY-LINK-SEND): a dropped socket's send and a
  ;; hung-up in-process link's both take the frame and carry nothing.
  (cond ((null ws) nil)
        ((gw-loopback-client-p ws) (and (find-gw-peer-state ws) t))
        (t (eq (wsd:ready-state ws) :open))))

(defun send-gateway-command (ws kind request &key command-id correlation-id)
  "Serialize and send one canonical command; return its identifiers."
  ;; A shell has no link (WS NIL) until its first attach, and a command from
  ;; that window is refused here. Without this, it would be handed on, and the
  ;; socket's send would answer `There is no applicable method ... (NIL ...)'.
  (unless ws (error 'gateway-not-connected))
  (let ((command (make-gateway-command
                  :kind kind
                  :command-id (or command-id (nlk:make-durable-id "command"))
                  :correlation-id (or correlation-id command-id
                                        (nlk:make-durable-id "correlation"))
                  :request (or request (make-hash-table :test #'equal)))))
    ;; Measured before it is written: a frame the gateway would refuse
    ;; never leaves this process (NLK:FRAME-TOO-LARGE, before any socket).
    (gateway-link-send ws (nlk:check-sync-frame-size (encode-client-command command)))
    (values command.command-id
            command.correlation-id)))

(defun send-start-turn (ws session-id prompt &key command-id correlation-id
                                                  images)
  "Send start_turn, optionally reusing stable identifiers after reconnect."
  ;; IMAGES, when present, is a list of (MEDIA-TYPE . BASE64) staged
  ;; attachments; the codec (protocol.lisp) carries them as image items in the
  ;; wire content array, after the prompt's text item.
  (send-gateway-command ws :start-turn
                        (json-object "session_id" session-id
                                     "prompt" prompt
                                     "generate_response" t
                                     :opt "images" images)
                        :command-id command-id :correlation-id correlation-id))

(defun send-cancel-turn (ws session-id turn-id &key retract)
  "Cancel exactly TURN-ID, never merely the session's current turn."
  ;; RETRACT asks the gateway to also take the turn back should it end having
  ;; produced nothing (the prompt returns to this composer on the gateway's
  ;; say-so).
  (send-gateway-command ws :cancel-turn
                        (json-object "session_id" session-id
                                     "turn_id" turn-id
                                     :when retract "retract" t)))

(defun send-interrupt-exec (ws session-id exec-id)
  "Interrupt exactly the background exec EXEC-ID in SESSION-ID (the
pending-exec view's Ctrl+X). The gateway's answer is a receipt — the exec's
exit itself arrives as the ordinary exit fact, so this shell only asks."
  (send-gateway-command ws :interrupt-exec
                        (json-object "session_id" session-id
                                     "exec_id" exec-id)))

(defun send-steer-turn (ws session-id turn-id prompt &key command-id
                                                         images)
  "Steer exactly TURN-ID with PROMPT."
  ;; The gateway parks the text on the running turn, which ends at its next
  ;; round boundary; promotion then starts the text as the next turn, and the
  ;; result frame and the live stream drive the UI. IMAGES ride the wire like
  ;; start_turn's and wait in the queue snapshot beside the text, promoting
  ;; with it onto the turn it starts, never onto the turn it cut short).
  (send-gateway-command ws :steer-turn
                        (json-object "session_id" session-id
                                     "expected_turn_id" turn-id
                                     "prompt" prompt
                                     :opt "images" images)
                        :command-id command-id))

(defun send-withdraw-prompt (ws session-id prompt-id)
  "Take a queued prompt PROMPT-ID back out of the queue (the Up peel)."
  ;; Fire-and-forget, like the steer leg: the queue snapshot that follows is
  ;; the durable answer, while the correlated result frame is the
  ;; requester's own — it carries the prompt's images home
  ;; (NOTE-WITHDRAW-PROMPT-RESULT).
  (send-gateway-command ws :withdraw-prompt
                        (json-object "session_id" session-id
                                     "prompt_id" prompt-id)))

(defun peel-queued-steer (model)
  "Up's peel: take the newest queued prompt, a steer or a follow-up, back
into the composer. => T when the key was consumed."
  ;; The row leaves the band and the composer fills BEFORE the round trip
  ;; (paint first); the withdrawal itself is the gateway's to confirm, and a
  ;; prompt that promoted in the meantime is refused and says so.
  (when-let (row (peelable-queued-steer model))
    (hold-queued-prompt model row)
    (send-withdraw-prompt model.ws model.session-id row.prompt-id)
    t))

(defun note-withdraw-prompt-result (model status response &aux (held (held-prompt model.state)))
  "The withdrawal's answer for the prompt this composer holds.

ACCEPTED: the images come home with it (its text is already in the
composer) and the hold stands until the prompt is submitted again or
discarded. REFUSED: the prompt promoted before the gateway read
the withdrawal — the hold ends, the text goes back out of the composer
while it is still exactly what the peel wrote, and a hint says why."
  (when held
    (if (string= status "accepted")
        (when-let (images (nlk::content-image-attachments (json-value response :any "images")))
          (setf model.pending-images (append model.pending-images images)))
        (progn
          (release-held-prompt model)
          (when (equal (editor-value model.editor) (getf held :text)) (editor-reset model.editor))
          (show-status model "That prompt already started")))))

(defun send-navigate-checkpoint (ws session-id &key target-turn-id
                                                    boundary
                                                    direction
                                                    (restore-files t))
  "Send navigate_checkpoint."
  ;; DIRECTION ("undo"/"redo") lets the server resolve the target from its
  ;; tree; an explicit TARGET-TURN-ID + BOUNDARY navigates to a picked
  ;; checkpoint. RESTORE-FILES NIL is the confirm stage's "Restore
  ;; conversation" option (Zig UndoScope conversation_only): the field is sent
  ;; only as an explicit false — its absence keeps the gateway's
  ;; conversation_and_files default. Fire-and-forget: the result frame and the
  ;; session_resync_required broadcast drive the UI.
  (send-gateway-command
   ws :navigate-checkpoint
   (json-object "session_id" session-id
                :opt "target_turn_id" target-turn-id
                :opt "boundary" boundary
                :opt "direction" direction
                ;; shasht writes NIL as JSON false — exactly the wire
                ;; value wanted.
                :when (not restore-files) "restore_files" nil)))

(defun send-subscribe (ws session-ids)
  "Send a typed set_session_subscriptions command."
  ;; The shell's pid rides it: the gateway's image section names each attached
  ;; shell with the process it runs in, so the model knows which shells a
  ;; definition evaluated there can reach (gateway.lisp IMAGE-LIVE-SECTIONS).
  ;; An entry is a session id, or (ID :LIVE-ONLY T) for a session whose live
  ;; frames alone are wanted -- the preview lane's follow, which holds its own
  ;; bounded history and asks the gateway for no replay -- or (ID :CURSOR N)
  ;; for one whose shell holds its log up to N, resumed past it.
  (send-gateway-command
   ws :set-session-subscriptions
   (json-object "subscriptions"
                (map 'vector
                     (lambda (entry &aux (options (and (consp entry) (rest entry))))
                       (json-object "session_id" (if (consp entry) (first entry) entry)
                                    (:getf options :opt "live_only" "cursor")))
                     session-ids)
                "pid" (sb-posix:getpid))))

(defun subscription-entries (model &aux (followed model.preview-session-id))
  "What MODEL subscribes to: its session, and the session its preview lane
follows live when one is armed."
  (cons model.session-id
        (when followed (list (list followed :live-only t)))))

;;; --- which session this shell is on, for programs outside it ---------------
;;; A desktop restorer reopens each terminal tab on the conversation it held.
;;; It finds the agent by its pid and asks the agent's own record: Claude Code
;;; keeps ~/.claude/sessions/<pid>.json, Pi
;;; $XDG_RUNTIME_DIR/pi-session-presence/<pid>.json. The gateway's map of
;;; shells (ATTACHED-SHELLS) is readable only from inside the organism, and it
;;; cannot tell a shell's own session from the one its preview lane follows, so
;;; the shell, which knows, says it: $XDG_RUNTIME_DIR/nodecode/shells/<pid>.json,
;;; written whenever its own session is set or switched -- every subscribe --
;;; and removed when the shell ends. The runtime directory is the user's own
;;; and emptied at every boot; a record a killed shell left behind names a pid
;;; whose start time no longer matches, and the next shell sweeps it.

(defvar *shell-presence-path* nil
  "Where this process's terminal shell keeps its presence record while it
runs (RUN-CHAT-GATEWAY), or NIL: not a shell, or no runtime directory.")

(defun shell-presence-path (&optional (pid (sb-posix:getpid)))
  "$XDG_RUNTIME_DIR/nodecode/shells/PID.json, or NIL when the environment
names no absolute runtime directory."
  #+win32 (declare (ignore pid))
  #-win32 (let ((runtime (uiop:getenv "XDG_RUNTIME_DIR")))
            (when (and runtime (plusp (length runtime)) (char= #\/ (char runtime 0)))
              (format nil "~a/nodecode/shells/~d.json" (string-right-trim "/" runtime) pid))))

(defun process-start-ticks (&optional (pid (sb-posix:getpid)))
  "When PID started, in clock ticks since boot (/proc/PID/stat field 22), or
NIL where there is no /proc: what tells this process from a later one given
the same pid."
  (ignore-errors
   (let* ((stat (uiop:read-file-string (format nil "/proc/~d/stat" pid)))
          ;; The command name may hold spaces and parens; the fields after
          ;; its last paren start at field 3.
          (fields (uiop:split-string (subseq stat (+ 2 (position #\) stat :from-end t)))
                                     :separator " ")))
     (parse-integer (nth 19 fields)))))

(defun shell-resume-argv (session-id)
  "The command that opens a shell on SESSION-ID in this home again."
  (cond ((nlk:profile-named-p) (list "nodecode" "-p" (nlk:profile-name) "resume" session-id))
        ((equal (namestring (nlk:home)) (namestring (nlk:default-home)))
         (list "nodecode" "resume" session-id))
        ;; A custom $NODECODE_HOME is no profile -p can name.
        (t (list "env" (format nil "NODECODE_HOME=~a" (uiop:native-namestring (nlk:home)))
                 "nodecode" "resume" session-id))))

(defun sweep-shell-presence (path)
  "Delete the records beside PATH whose shell is gone: a shell killed before
its unwind leaves one."
  #+win32 (declare (ignore path))
  #-win32
  (dolist (file (ignore-errors (uiop:directory-files (uiop:pathname-directory-pathname path) "*.json")))
    (let ((pid (ignore-errors (parse-integer (pathname-name file)))))
      (when (and pid (/= pid (sb-posix:getpid))
                 (handler-case (progn (sb-posix:kill pid 0) nil)
                   (sb-posix:syscall-error (condition)
                     (= (sb-posix:syscall-errno condition) sb-posix:esrch))))
        (ignore-errors (delete-file file))))))

(defun note-shell-presence (session-id &aux (path *shell-presence-path*))
  "Say SESSION-ID is this shell's own session, in its presence record. A
session only the preview lane follows is never named here."
  ;; The record costs the shell nothing: a runtime directory it cannot write
  ;; is no reason to stop a subscribe.
  (when (and path session-id)
    (ignore-errors
     (write-file-atomically
      path
      (encode-json-object
       (json-object "version" 1
                    "kind" "nodecode-shell"
                    "pid" (sb-posix:getpid)
                    "start_ticks" (or (process-start-ticks) :null)
                    "session_id" session-id
                    "profile" (nlk:profile-name)
                    "home" (uiop:native-namestring (nlk:home))
                    "cwd" (workspace-directory)
                    "resume_argv" (coerce (shell-resume-argv session-id) 'vector)
                    "updated_at" (nlk:unix-now)))
      :mode #o600 :directory-mode #o700))))

(defun open-shell-presence ()
  "Make this process a shell that keeps a presence record, clearing the ones
dead shells left. => the record's path, or NIL."
  (when-let (path (shell-presence-path))
    (sweep-shell-presence path)
    (setf *shell-presence-path* path)))

(defun close-shell-presence (&aux (path (shiftf *shell-presence-path* nil)))
  "The shell ends: its record goes."
  (when path (ignore-errors (delete-file path))))

(defun preview-follow (model session-id)
  "Have the preview lane follow SESSION-ID live: a live-only subscription
beside the attached one, so the frames the session publishes from here on
reduce into the lane (the UPDATE arm) -- the agent typing, its tool
calls and answers as they land -- while the lane's bounded history, read
before the follow, is the base."
  ;; Idempotent for the session already followed.
  (unless (equal session-id model.preview-session-id)
    (setf model.preview-session-id session-id model.preview-activity nil)
    ;; A shell that has no socket yet subscribes when it connects, with
    ;; the follow in the list (SUBSCRIPTION-ENTRIES).
    (when model.ws (send-subscribe model.ws (subscription-entries model)))))

(defun gateway-event-payload (event)
  "Normalize wrappers and replay/live event shapes to one event object."
  ;;
  ;; A typed inner event — {"type","payload","metadata"}, the shape every
  ;; durable fact rides in replay accumulated_frames — passes through whole: its
  ;; envelope metadata (turn_id, log_position, occurred_at) is fold input, and
  ;; the flattened copy below discards it. That loss was the client half of the
  ;; boot-resume unkillable-turn hole: a shell that learned about a running turn
  ;; only from the subscribe replay folded turn_id NIL, armed ACTIVE-TURN-ID to
  ;; NIL while TURN-IN-FLIGHT went T, and Esc silently sent no cancel_turn.
  ;; Live projection envelopes never carry a top-level "payload" key
  ;; (ENCODE-PROJECTION-EVENT spreads the domain payload flat), so only bare
  ;; inner events pass through; a session_stream_event envelope answers the
  ;; event its stream frame carries, and every other frame IS its event.
  (if (json-value event :object "payload")
      event
      (or (json-value event :object "event")
          (when-let (frame (json-value event :object "frame")) (or (gethash "event" frame) frame))
          event)))

(defun gateway-append-fact (state event)
  (alexandria:appendf (gethash "facts" state) (list event))
  state)

;;; --- background-shell band reducer -----------------------------------------
;;; Port of the src/tui/model/root.zig background-process reducer cluster.
;;; Band rows are LIVE-ONLY state: replay never seeds them (*FOLDING-REPLAY-P*
;;; gates every seed), a fresh subscriber regains still-running shells from
;;; the gateway's subscribe-time registry snapshot frames, and dead shells
;;; replay their durable exit fact instead of a band row.

;;; Band seeding is gated on it: a replayed dead process would have no exit
;;; frame to retire its row — the rejected phantom-row hazard (root.zig
;;; applying_replay_frames).
(defvar *folding-replay-p* nil
  "T while the reducer folds replayed history (FOLD-ACCUMULATED-FRAMES, every
chunk of a resume included).")

(defun state-background-rows (state)
  (json-value state :any "background_processes"))

(defun (setf state-background-rows) (rows state)
  (setf (gethash "background_processes" state) rows))

(defun state-ended-band-rows (state)
  "The band rows STATE's execs ended on, newest first: what /execs reopens
once the band let them go. Live-only, as the band is."
  (json-value state :any "ended_background_processes"))

;;; A side table rather than
;;; another band-row slot because the row is structure an image cannot redefine
;;; under its own live rows: a shell picking up a definition through
;;; NLK:LOAD-SOURCE has to be able to run the new code against the struct it
;;; already holds. Entries die with their rows (weak keys), and the stamp is
;;; chrome — never on the wire, never durable.
(nlk:define-side-table bg-row-born (:weakness :key)
  "ROW -> internal time this shell first showed it.
When this shell first showed ROW — what a RUNNING row's mark spins from,
so a silent command still looks alive — or NIL for a row that arrived without
a stamp: it clocks nothing.")

(defun bg-note-born (row)
  "Stamp ROW as just shown: every row the band can hold is born with one, so
the mark of the command that then runs has a clock to step on."
  (setf (bg-row-born row) (get-internal-real-time))
  row)

;;; The same chrome discipline for the interrupt this shell asked for: the row
;;; says `stopping' from the gesture until its exit retires it, and a refusal
;;; takes the mark back so the gesture is offered again.
(nlk:define-side-table bg-row-stopping (:weakness :key)
  "ROW -> T once this shell asked the gateway to interrupt it.")

;;; And for the exit: the band forgets the row, a reader still holding it (the
;;; /execs view) reads how it ended.
(nlk:define-side-table bg-row-exited (:weakness :key)
  "ROW -> the exit's background_shell document (NLK:EXITED-SHELL-DOCUMENT)
once the band let ROW go.")

(defun band-row-stoppable-p (row)
  "Whether ROW offers the interrupt gesture: a running exec with an id this
shell has not already asked to stop."
  (and (eq :running (bg-band-row-status row))
       (bg-band-row-shell-session-id row)
       (not (bg-row-stopping row))))

(defun interrupt-band-exec (model exec-id)
  "Ask the gateway to interrupt EXEC-ID and mark its band row stopping —
the one path the band's ✕ and the pending-exec view's Ctrl+X share."
  (when-let (row (find-band-row model.state nil exec-id))
    (setf (bg-row-stopping row) t))
  (send-interrupt-exec model.ws model.session-id exec-id))

(defun upsert-preparing-band-row (state name arguments
                                  &key call-id stream-index)
  "Seed/refresh a preparing band row from streamed run-tool fragments (the
lisp dialect keys pre-call-id stream deltas by provider index; the durable
tool_call_started fact adopts the row by call id)."
  ;; No-op on replay and once a spawn handoff promoted the row to :RUNNING.
  (when *folding-replay-p* (return-from upsert-preparing-band-row state))
  (let* ((rows (state-background-rows state))
         (unowned (remove-if-not (lambda (row) (and (eq :preparing (bg-band-row-status row))
                                                    (null (bg-band-row-tool-call-id row))))
                                 rows))
         (row (or (and call-id (find call-id rows :key #'bg-band-row-tool-call-id :test #'equal))
                  ;; Index-keyed match only while no call id owns the row:
                  ;; stream indices restart each tool round, so a later
                  ;; round's deltas must not adopt an adopted row.
                  (and stream-index (find stream-index unowned :key #'bg-band-row-stream-index))
                  ;; The durable fact adopts the oldest index-keyed
                  ;; preparing row rather than opening a second one — one
                  ;; call never owns two rows. A round's facts land in model
                  ;; order (INGEST-TOOL-CALL-FACT): without this, the first
                  ;; call would take the second's row, and the band would show
                  ;; the first command twice while the second's was nowhere.
                  (and call-id (first unowned)))))
    ;; A promoted :RUNNING row is the process updates' now.
    (unless (and row (eq :running (bg-band-row-status row)))
      (unless row
        (setf row (bg-note-born (make-bg-band-row
                                 :status :preparing
                                 :stream-index stream-index
                                 :command (or name ""))))
        (alexandria:appendf (state-background-rows state) (list row)))
      (when call-id (setf (bg-band-row-tool-call-id row) call-id))
      (when (and (stringp arguments) (plusp (length arguments)))
        (setf (bg-band-row-streamed-args row) arguments)
        (incf (bg-band-row-activity-events row)))
      ;; The decoded command grows in place as the arguments accumulate.
      (let ((raw (and (plusp (length (bg-band-row-streamed-args row)))
                      (shell-command-preview-from-args
                       (bg-band-row-streamed-args row)))))
        (when (and raw (plusp (length raw)))
          (setf (bg-band-row-command row) raw))))
    state))

(defun find-band-row (state call-id shell-id)
  "STATE's band row that CALL-ID's call owns, when CALL-ID names one, or
that runs SHELL-ID's shell, else NIL."
  (find-if (lambda (row)
             (or (and (json-value call-id :text)
                      (equal call-id (bg-band-row-tool-call-id row)))
                 (and shell-id
                      (eql shell-id (bg-band-row-shell-session-id row)))))
           (state-background-rows state)))

(defun apply-background-shell-exit (state call-id shell-id shell
                                    &key require-live-row backgrounded)
  "One reducer for the background-shell exit semantic regardless of trigger
— the live watcher frame, a write_stdin-observed exit, or the durable exit
fact on replay: retire the band row (running or still preparing), and, for a
call that outlived its tool call, stamp its transcript rows in place. SHELL is the exit's background_shell document
(NLK:EXITED-SHELL-DOCUMENT), which becomes the rows': how the call ended and
the last lines it wrote, the completed card at the same chronological call site."
  ;; REQUIRE-LIVE-ROW: the live-frame path only acts when a band row exists (a
  ;; foreground command's completed tool row must not be touched); the durable
  ;; exit fact applies unconditionally — on replay no band row ever exists.
  ;; BACKGROUNDED: the call was handed off. A foreground eval's (sh ...) shows
  ;; in the band while it runs, and the watcher's frames for it can land after
  ;; the call's result and after the next round's rows; its card stays where
  ;; the call was made, as the replay has it.
  (let ((removed (find-band-row state call-id shell-id)))
    (when (and require-live-row (null removed))
      (return-from apply-background-shell-exit nil))
    (when removed
      ;; The band forgets the row; a reader still holding it (the /execs view)
      ;; reads how it ended and the last lines it wrote, the exit's.
      (setf (bg-row-exited removed) shell)
      (when-let (tail (json-value shell :string "tail"))
        (note-band-preview removed (nlk:utf-8-octets
                                    (kept-tail-text tail (json-value shell :boolean "tail_cut")))))
      (alexandria:removef (state-background-rows state) removed :count 1)
      ;; Kept, the newest eight, for /execs to reopen: /execs lists running execs only.
      ;; Without this, a finished exec's output would be out of reach unless its view is
      ;; already open when it ends.
      (when-let (id removed.shell-session-id)
        (setf (gethash "ended_background_processes" state)
              (let ((kept (cons removed (remove id (state-ended-band-rows state)
                                                :key #'bg-band-row-shell-session-id))))
                (subseq kept 0 (min (length kept) 8))))))
    (when-let (resolved (and backgrounded
                             (or (and removed (json-value removed.tool-call-id :text))
                                 (json-value call-id :text))))
      (flet ((matches (row) (and (eq :tool-call row.kind) (equal resolved row.tool-call-id))))
        ;; Every transcript row of the call takes the exited status and code,
        ;; and the exit's document in place of the handoff's running one.
        (dolist (row (remove-if-not #'matches (transcript-rows state)))
          (setf row.tool-status "exited")
          (when-let (exit-code (json-value shell :integer "exit_code"))
            (setf row.tool-exit-code exit-code))
          (when (hash-table-p shell)
            (let ((metadata (if (hash-table-p row.metadata)
                                (alexandria:copy-hash-table row.metadata)
                                (make-json-object))))
              (setf (gethash "background_shell" metadata) shell
                    row.metadata metadata)))) ))))
(defun bg-apply-chunk (row chunk)
  "Reduce one raw CHUNK through ROW's streaming sanitizer into the bounded
head/tail previews and the display-line counters."
  (let* ((clean (sanitize-octets (bg-band-row-sanitizer row) chunk))
         (head (appended-preview-head (bg-band-row-output-head row) clean)))
    (when head (setf (bg-band-row-output-head row) head))
    (note-preview-lines row clean)
    (setf (bg-band-row-output-tail row)
          (appended-preview-tail (bg-band-row-output-tail row) clean))
    (when (plusp (length chunk))
      (incf (bg-band-row-activity-events row)))
    row))

(defun note-band-preview (row preview)
  "Show PREVIEW, a running snippet's live output tail, as ROW's: it replaces
the tail rather than adding to it, since each preview is whole."
  (let ((clean (sanitize-octets (make-text-sanitizer) (last-redraws preview))))
    (setf (bg-band-row-output-tail row) (appended-preview-tail (make-array 0 :element-type '(unsigned-byte 8)) clean)
          (bg-band-row-output-seen-lines row) (count (char-code #\Newline) clean)
          (bg-band-row-output-line-dirty row) (and (plusp (length clean))
                                                   (/= (char-code #\Newline) (aref clean (1- (length clean))))))
    (incf (bg-band-row-activity-events row))
    row))

(defun apply-running-background-update (state shell-id call-id command chunk)
  "The three-way running upsert (root.zig applyBackgroundProcessUpdate):
match by shell id -> append chunk; match by tool call id -> spawn handoff
promotes the preparing row IN PLACE (the band slot never jumps, one call
never owns two rows); else a fresh running row."
  (let ((existing (find-band-row state nil shell-id))
        (handoff (find-band-row state call-id nil)))
    (cond
      (existing
       (bg-apply-chunk existing chunk)
       (setf existing.status :running))
      (handoff
       ;; The preparing row takes a fresh running row's state in place, and
       ;; keeps its call id, its stream index and its activity count.
       (let ((fresh (make-bg-band-row :command (or command "") :shell-session-id shell-id)))
         (dolist (slot '(command sanitizer output-tail output-head output-seen-lines
                         output-line-dirty streamed-args shell-session-id status))
           (setf (slot-value handoff slot) (slot-value fresh slot))))
       (bg-apply-chunk handoff chunk))
      (t
       (let ((fresh (bg-note-born (make-bg-band-row
                                   :shell-session-id shell-id
                                   :tool-call-id (json-value call-id :text)
                                   :command (or command "")
                                   :status :running))))
         (bg-apply-chunk fresh chunk)
         (alexandria:appendf (state-background-rows state) (list fresh))))))
  state)

(defun gateway-fact-event (kind payload event turn-id)
  "EVENT, one durable fact off the wire, as the typed instance KIND declares:
PAYLOAD fills its slots, TURN-ID its turn, the metadata the rest (NLK:+EVENT-ROW-COLUMNS+)."
  (apply #'nlk::materialize-event :kind kind :payload payload :turn-id turn-id
         (loop for (column initarg) in nlk::+event-row-columns+
               for value = (json-value event :any "metadata" column)
               when value append (list initarg value))))

(defun tool-call-verdict (name arguments)
  "The live bar's verdict for a call of tool NAME, its ARGUMENTS streaming
or whole: the tool's own verb -- `evaluating' an eval, `running NAME' any
other -- beside the mark of what the call does (TOOL-ACTIVITY-VERB's kind)."
  ;; What the call does is its own line's, right above the bar. The bar said
  ;; it again, then held only the clock for a tool -- told from the model's
  ;; verbs by its kind, and a call whose words are a wait wears the model's
  ;; wait kind: `(progn (sleep 60) (sh ...))' under `;; checking the canonical
  ;; lane's verdict so far' put the note back on the bar, in and out with the
  ;; kind as its fragments streamed (`evaluating').
  (list :activity (cond ((equal name "eval") "evaluating")
                        ((and (stringp name) (plusp (length name))) (format nil "running ~a" name))
                        (t "running tool"))
        :kind (nth-value 1 (tool-activity-verb name arguments))))

(defun apply-gateway-event (state event)
  "Single replay/live reducer for the sync stream: durable dotted facts and
streaming deltas both reduce into the shared transcript-row lanes (ui.lisp),
so a streamed item and its durable fact are one row, never two."
  ;;
  ;; => (values STATE VERDICT). VERDICT is what this fold already learned about
  ;; the live turn, so no caller classifies the frame a second time:
  ;; (:lifecycle :started :turn-id ID) for an admitted turn, (:lifecycle
  ;; :completed | :failed | :cancelled) for a terminal, and (:activity VERB
  ;; :kind KIND) for the frames the live bar names — reasoning deltas think,
  ;; tool fragments and the durable tool-call fact name the running tool, text
  ;; deltas stream, and a tool result returns the bar to waiting on the
  ;; provider. KIND picks the mark beside the verb (*STATUS-MARKS*).
  (let* ((st (or state (make-hash-table :test #'equal)))
         (verdict '())
         (event (gateway-event-payload event))
         (kind (json-value event :string "type"))
         (payload (or (json-value event :object "payload") event))
         (turn-id (or (json-value payload :string "turn_id")
                      (json-value event :string "metadata" "turn_id")))
         (log-position (json-value event :integer
                                   "metadata" "log_position")))
    (note-transcript-time log-position (json-value event :string "metadata" "occurred_at")
                          kind turn-id)
    (values
     (stamp-unpositioned-rows
      (cond
       ;; A fact whose fold is its declaration's (ui.lisp DEFINE-FOLD) folds
       ;; through it, as the typed instance its kind declares -- hydrated from
       ;; the payload and the metadata as a stored row is -- so the keys, the
       ;; types and the defaults are the declaration's and never a copy.
       ((member kind '("session.context.evicted" "turn.attempt_voided" "turn.usage"
                       "turn.provider_retry" "turn.input_committed"
                       "turn.assistant_message_completed")
                :test #'string=)
        (apply-event st (gateway-fact-event kind payload event turn-id)))
       ((string= kind "context.edited")
        ;; The model rewrote its own context (working-context.lisp): one
        ;; persisted row says how much it removed, changed and added.
        (ingest-system-notice st (context-edit-line payload) :turn-id turn-id
                              :metadata (json-object "label" "Context")))
       ((string= kind "session_background_process_updated")
        ;; Live-only shell output/lifecycle frames (subscribe snapshots
        ;; included): the band state's one chunk feed. Never replayed.
        (with-json ((shell-id     :integer "process" "shell_session_id")
                    (call-id      :string  "process" "tool_call_id")
                    (command      :string  "process" "command" :default "")
                    (status       :string  "process" "status")
                    (exit-code    :integer "process" "exit_code")
                    (shell-exit   :integer "process" "shell_exit")
                    (tail         :string  "process" "tail")
                    (tail-cut     :boolean "process" "tail_cut")
                    (error-report :string  "process" "error")
                    (stopped      :object  "process" "stopped")
                    (backgrounded :boolean "process" "backgrounded")
                    (b64          :text    "process" "output_b64")
                    (preview-b64  :text    "process" "preview_b64"))
            event
          (let ((chunk (or (and b64 (ignore-errors (cl-base64:base64-string-to-usb8-array b64)))
                           (make-array 0 :element-type '(unsigned-byte 8)))))
            (cond
              ((equal status "exited")
               ;; The document the durable exit fact carries, from the same
               ;; fields: the card closed live is the card replay closes.
               (apply-background-shell-exit st (or call-id "") shell-id
                                            (and shell-id
                                                 (nlk:exited-shell-document
                                                  shell-id command exit-code
                                                  :shell-exit shell-exit :tail tail
                                                  :tail-cut tail-cut :error error-report
                                                  :stopped stopped))
                                            :require-live-row t
                                            :backgrounded backgrounded))
              ((equal status "running")
               (apply-running-background-update st shell-id call-id command
                                                chunk)
               (when-let (preview (and preview-b64 (ignore-errors
                                                    (cl-base64:base64-string-to-usb8-array preview-b64))))
                 (when-let (row (find-band-row st nil shell-id))
                   (note-band-preview row preview)))))))
        st)
       ((string= kind "session_fork_progress")
        ;; Live-only: how the forks of a running call stand (fork.lisp
        ;; FORK-COLLECT-PROGRESS), set on the call's own rows as their
        ;; "forks" -- a fresh metadata object, so each row's memo sees the
        ;; change. Never replayed: each fork's session is the record.
        (let ((call-id (json-value payload :string "tool_call_id"))
              (forks (json-value payload :array "forks")))
          (when (and call-id forks)
            (dolist (row (transcript-rows st))
              (when (and (eq :tool-call row.kind) (equal call-id row.tool-call-id))
                (setf row.metadata (copy-json-object (and (hash-table-p row.metadata) row.metadata)
                                                     "forks" forks))))))
        st)
       ((string= kind "session_input_queue_snapshot_updated")
        ;; Queue state, and the band's one authority. A steer parks here,
        ;; with no fact of its own until promotion starts it as the next
        ;; turn, so this is the frame that puts it in the pending band for
        ;; every attached shell and on replay; the started INPUT_COMMITTED
        ;; above retires the row. A follow-up's admission fact paints its row
        ;; first, and the snapshot names it too.
        (let ((pending (json-array payload "snapshot" "pending_prompts")))
          ;; A snapshot that no longer names a queued prompt is a
          ;; withdrawal — this shell's own peel included — and the row
          ;; leaves with it.
          (setf st (prune-pending-steer-rows
                    st
                    (loop for prompt across pending
                          collect (json-value prompt :string "prompt_id"))))
          (loop for prompt across pending
                do (setf st (ingest-pending-input-fact
                             st (or (json-value prompt :string "content") "")
                             :prompt-id (json-value prompt :string "prompt_id")
                             :turn-id turn-id
                             :steer (equal "steer" (json-value prompt :string "disposition"))))
                finally (return st))))
       ((string= kind "turn.tool_call_started")
        (with-json ((call-id :string "call-id")
                    (name :string "tool-name" :default (json-value payload :string "name"))
                    (arguments :string "arguments" :default ""))
            payload
          ;; The verb is the live bar's, and a replay's verdicts are
          ;; discarded: naming every replayed call — its arguments decoded,
          ;; an eval's forms walked — would be wasted work on the UI thread.
          (unless *folding-replay-p*
            (setf verdict (tool-call-verdict name arguments)))
          ;; A shell-family call lives in the band from its first fragment;
          ;; the durable fact adopts (or opens) the preparing row by call id.
          ;; Live only — replayed history never seeds the band.
          (when (and (json-value call-id :text) (seeds-background-band-p name))
            (upsert-preparing-band-row st name arguments :call-id call-id))
          (ingest-tool-call-fact st call-id name arguments :turn-id turn-id)))
       ((string= kind "turn.tool_result")
        (with-json ((call-id :string "call-id")
                    (tool-name :string "tool-name")
                    (result :string "result" :default "")
                    (failed :boolean "failed")
                    (metadata :object "metadata")
                    (shell (json-value metadata :object "background_shell"))
                    (shell-status (json-value shell :string "status"))
                    (shell-id (json-value shell :integer "shell_session_id"))
                    (shell-command (json-value shell :string "command"))
                    ;; The standalone watcher exit fact: tool name "snippet"
                    ;; (protocol/background_exec.zig exit_fact_tool_name), empty
                    ;; result text.
                    (exit-fact-p (and (equal tool-name "snippet")
                                      (string= result ""))))
            payload
          (setf verdict (list :activity "waiting" :kind :wait))
          ;; A still-preparing row for the call retires: the tool finished
          ;; without a spawn — a denial, a validation error, a synchronous
          ;; shell tool — or the running row owns the slot already.
          (when (json-value call-id :text)
            (alexandria:removef (state-background-rows st) call-id :count 1
                                :test (lambda (id row)
                                        (and (eq :preparing (bg-band-row-status row))
                                             (equal id (bg-band-row-tool-call-id row))))))
          (when (equal shell-status "exited")
            ;; The exit fact restamps the run row and materializes no row
            ;; of its own — on replay no band row ever exists, so the
            ;; reducer applies unconditionally. A write_stdin result that
            ;; observed the exit retires the run call's band row.
            (apply-background-shell-exit st (or call-id "") shell-id shell
                                         :require-live-row (not exit-fact-p)
                                         :backgrounded t))
          (when (and (equal shell-status "running") (not *folding-replay-p*)
                     (not (find-band-row st call-id shell-id)))
            ;; The backgrounding handoff seeds the live band row when the
            ;; watcher's first chunk has not arrived yet (root.zig
            ;; seedBackgroundProcessFromToolResult).
            (apply-running-background-update
             st shell-id (or call-id "") (or shell-command "")
             (make-array 0 :element-type '(unsigned-byte 8))))
          (if (and (equal shell-status "exited") exit-fact-p)
              st
              (ingest-tool-result-fact st call-id tool-name result
                                       :turn-id turn-id
                                       :error-p failed
                                       :metadata metadata))))
       ((string= kind "item_delta")
        (let* ((delta (json-value payload :object "delta"))
               (delta-type (json-value delta :any "type")))
          (cond
            ((equal delta-type "tool_call")
             (with-json ((index :integer "index" :default 0)
                         (name :string "name")
                         (arguments :string "arguments"))
                 delta
               (setf verdict (tool-call-verdict name arguments))
               ;; Preparing band seed: the call lives in the status-bar band
               ;; from its first streamed fragment instead of teleporting
               ;; there at spawn. Keyed by provider index until the durable
               ;; tool_call_started fact adopts it (lisp-dialect: the lisp
               ;; gateway's deltas carry no call id).
               (when (seeds-background-band-p name)
                 (upsert-preparing-band-row st name arguments
                                            :stream-index index))
               (ingest-stream-tool st index name arguments
                                   :turn-id turn-id)))
            ((equal delta-type "reasoning")
             (setf verdict (list :activity "thinking" :kind :thinking))
             (if-let (text (json-value delta :string "text"))
               (ingest-stream-body st :reasoning text :turn-id turn-id)
               st))
            ;; fallback: The round moved to another model. The wording lives in the
            ;; shell's own lane (the durable turn.provider_fallback fact);
            ;; this delta is the live hint, and it is not assistant text.
            ((equal delta-type "fallback")
             st)
            ((equal delta-type "stream_reset")
             (ingest-stream-reset st :turn-id turn-id
                                     :retry-status (retry-status (json-value delta :object "retry"))))
            (t
             (setf verdict (list :activity "streaming" :kind :streaming))
             ;; The OR consults PAYLOAD only when DELTA carries no "text" key
             ;; at all — a present-but-not-a-string delta text is still the
             ;; delta's answer, so this stays a raw GETHASH chain.
             (let ((text (or (and (hash-table-p delta) (gethash "text" delta))
                             (gethash "text" payload))))
               (if (stringp text)
                   (ingest-stream-body st :text text :turn-id turn-id)
                   st))))))
       ((string= kind "turn.started")
        (setf (gethash "turn_id" st) turn-id
              verdict (list :lifecycle :started :turn-id turn-id))
        ;; The durable start time seeds the finish divider's elapsed; the
        ;; previous turn's resting summary and usage accumulator retire; a
        ;; turn an exit woke opens on the line that says so.
        (ingest-turn-started st (json-value event :string "metadata" "occurred_at")
                             :turn-id turn-id :wake (json-value payload :array "wake")))
       ((member kind '("turn.completed" "turn.failed" "turn.cancelled") :test #'string=)
        ;; A turn that ended can never dispatch a still-preparing call, so
        ;; every preparing preview drops (root.zig
        ;; dropPreparingBackgroundProcesses).
        (alexandria:removef (state-background-rows st) :preparing :key #'bg-band-row-status)
        ;; The kind's own word past `turn.': :COMPLETED, :FAILED or :CANCELLED,
        ;; and whether the completion rests on `steered' -- a parked steer cut
        ;; it short, so the promoted text starts the next turn.
        (setf verdict (list :lifecycle (intern (string-upcase (subseq kind 5)) :keyword)
                            :steered (and (equal kind "turn.completed")
                                          (json-value payload :boolean "steered"))))
        ;; The declared fold settles the turn (ui.lisp INGEST-TURN-TERMINAL): a
        ;; completion a parked steer cut short rests on `steered' while the
        ;; lifecycle verdict stays completed.
        (apply-event st (gateway-fact-event kind payload event (or turn-id (gethash "turn_id" st))))
        (setf (gethash "turn_id" st) nil)
        st)
       (t (gateway-append-fact st event)))
      log-position)
     verdict)))
(defun frames-events (frames)
  "The events canonical accumulated FRAMES carry, oldest first."
  (coerce (loop for frame across (if (vectorp frames) frames #())
                for event = (and (hash-table-p frame) (gethash "event" frame))
                when event collect event)
          'vector))

(defun fold-events (events &optional (state (initial-chat-state))
                    &aux (*folding-replay-p* t))
  "Fold EVENTS, oldest first, onto STATE, by default a fresh shell state."
  ;; Run under *FOLDING-REPLAY-P* so replayed history never seeds live-only
  ;; band rows (the durable exit fact still restamps tool rows).
  (loop for event across events
        do (setf state (apply-gateway-event state event)))
  state)

(defun fold-accumulated-frames (frames &optional (state (initial-chat-state)))
  "Fold canonical accumulated frames onto STATE, by default a fresh shell state."
  ;; The one fold owner behind every durable-history read this shell does —
  ;; the subscription replay, each later chunk of a resume (folded onto the
  ;; state the chunks before it built) and the /sessions preview page alike, because
  ;; both carry the same frame shape.
  (fold-events (frames-events frames) state))

(defun seat-session-title (state title)
  "Put TITLE, the name the gateway says the session wears, on STATE for the
header to read (SESSION-DISPLAY-TITLE). Nothing moves for no state or no name."
  (when (and (hash-table-p state) (stringp title) (plusp (length title)))
    (setf (gethash "session_title" state) title)))

(defun show-replayed-state (model)
  "Put the state MODEL's replay folded out of sight in place of the one shown."
  (setf model.state model.replay-staging
        model.replay-staging nil))

(defun keep-window-from-bottom (model render)
  "Call RENDER, keeping MODEL's transcript window as far from the bottom as
it stood when the operator is reading history: a replayed state takes the
place of one whose newest rows were the same, and the window counts from
the top."
  (let* ((window (chat-model-band model))
         (from-bottom (and window (not (chat-model-follow-bottom model))
                           (- (band-window-count window) (band-window-offset window)))))
    (funcall render)
    (when from-bottom
      (band-window-scroll window (- (- window.count (band-window-offset window)) from-bottom)))))

(defun bar-lane (model)
  "The turn the bar under the transcript speaks for => (values STATE IN-FLIGHT
STARTED ACTIVITY OWN): the previewed session's while a dialog previews one,
the attached session's otherwise -- VISIBLE-TRANSCRIPT-STATE's substitution,
so the bar is always the transcript's above it. STARTED is the internal time
the turn's clock counts from and ACTIVITY the (VERB KIND STAMPED) its stream
last named, on the one clock both lanes keep; OWN says the turn is the
attached session's, the one this shell's stop acts on."
  ;; A previewed turn is in flight while its folded state names one, and its
  ;; clock runs from the same durable turn.started fact the attached clock is
  ;; anchored on (TURN-CLOCK-ANCHOR).
  (let ((preview model.transcript-preview))
    (if (and preview model.dialog)
        (let ((state (cdr preview)))
          (values state
                  (and (hash-table-p state) (gethash "turn_id" state) t)
                  (and (hash-table-p state) (turn-clock-anchor state))
                  model.preview-activity
                  nil))
        (values model.state model.turn-in-flight model.turn-started-at
                model.turn-activity t))))

(defun dispatch-inbound-frame (program model json-text &key release &aux (dispatched nil))
  "Decode one projection and enqueue it; all state changes happen in UPDATE."
  ;; The session filter reads MODEL's session id at dispatch time — never a
  ;; connect-time snapshot — because /clear, /sessions, and /fork re-point the
  ;; model at another session over the same connection, and a filter frozen on
  ;; the boot session would silently drop every frame the switched-to session
  ;; sends (the new session's replay, its start_turn command results, its
  ;; whole stream: the submit-then-stall-forever shape).
  (with-cleanup ((when (and release (not dispatched)) (funcall release)))
    (handler-case
        (let* ((frame (decode-sync-message json-text))
               (payload (and (eq :projection frame.type) frame.payload)))
          ;; Session-less, this shell's own, or the preview lane's followed session.
          (when (and payload (member frame.session-id
                                     (list nil model.session-id model.preview-session-id)
                                     :test #'equal))
            (nlt:send program (make-instance 'gw-projection-msg :frame frame
                                              :release release))
            (setf dispatched t)))
      (nodecode.kernel:protocol-error (condition)
        (when program
          (report-transport-error program :permanent "~a" (protocol-error-message condition)))))))

(defun gateway-frame-accepted-p (model frame &aux (seq frame.sequence)
                                                  (cursor frame.cursor)
                                                  (last-seq model.last-sequence)
                                                  (last-cursor model.last-cursor))
  "Classify one ordinary live frame as apply, duplicate, or gap."
  (cond ((and cursor last-cursor (<= cursor last-cursor)) :duplicate)
        ((and seq last-seq (<= seq last-seq)) :duplicate)
        ((and seq last-seq (> seq (1+ last-seq))) :gap)
        (t :apply)))

(defun result-refusal (payload default)
  "What a refused command result or a protocol error says of itself, else DEFAULT."
  (or (gethash "message" payload) (gethash "code" payload) default))

(defun gateway-command-result (model payload)
  (let* ((status (gethash "status" payload))
         (command (gethash "command" payload))
         (response (gethash "response" payload))
         (command-id (gethash "command_id" payload))
         (turn-id (and (hash-table-p response) (gethash "turn_id" response))))
    ;; The peel's own result: keyed by the command name, never by the
    ;; pending start_turn slot a submit may hold at the same moment.
    (when (string= command "withdraw_prompt")
      (note-withdraw-prompt-result model status response)
      (return-from gateway-command-result nil))
    ;; The interrupt's receipt is its own lane: it belongs to the exec view's
    ;; gesture, never to the submit flow the pending-command guard below
    ;; tracks, and a submission parked at the same moment must not swallow it.
    (when (string= command "interrupt_exec")
      (if (string= status "accepted")
          (show-status model "Interrupt requested")
          (progn
            ;; The receipt names no exec, so a refusal clears every mark.
            (dolist (row (state-background-rows model.state))
              (setf (bg-row-stopping row) nil))
            (show-status model (format nil "Interrupt refused: ~a"
                                       (result-refusal payload "the exec is gone"))
                         :error-p t)))
      (return-from gateway-command-result nil))
    (when (or (null model.pending-command-id)
              (equal model.pending-command-id command-id)
              (equal model.pending-correlation-id (gethash "correlation_id" payload)))
      (cond
        ((string= status "accepted")
         (when (and (string= command "start_turn") turn-id)
           (note-turn-admitted model turn-id))
         ;; The Zig undo contract: the prompt the rewind undid returns
         ;; to the composer, ready to edit and resend.
         (when-let (undone (and (string= command "navigate_checkpoint")
                                (json-value response :text "undone_prompt")))
           (editor-reset model.editor)
           (editor-insert model.editor undone)))
        ((string= status "queued")
         ;; The gateway parked the prompt behind the active turn (the Zig
         ;; writeQueuedStartTurnCommandResultFrame path). Nothing turn-side
         ;; changes — the durable follow-up fact renders the pending row,
         ;; so the prompt row Enter painted yields to the band (the
         ;; promotion's own fact lands the conversation row later);
         ;; the transient hint surfaces the parked depth (live-only).
         (setf model.state (drop-submitted-prompt model.state command-id))
         (show-status
          model (format nil "Queued~@[ (~d waiting)~]" (json-value response :integer "queue_depth"))))
        ((string= status "rejected")
         (cond
           ((string= command "navigate_checkpoint")
            (show-status model (result-refusal payload "Navigation rejected") :error-p t))
           ;; A refused cancel is transient news (the turn it named already
           ;; ended, typically): the flight state follows the terminal
           ;; frame, not this result.
           ((string= command "cancel_turn")
            (show-status model (format nil "Cancel refused: ~a" (result-refusal payload "turn mismatch"))
                         :error-p t))
           (t
            (note-turn-flight-settled model)
            (clear-retract-stash model)
            ;; The prompt row Enter painted announced a turn that will
            ;; never exist: it goes, the notice says why.
            (setf model.state
                  (drop-submitted-prompt model.state (or command-id model.pending-command-id)))
            (show-status model (format nil "Turn rejected: ~a"
                                       (result-refusal payload "gateway rejected command"))
                         :error-p t)))))
      (when (member status '("accepted" "rejected") :test #'string=)
        (clear-pending-command model)))))

(defun clear-pending-command (model)
  "Forget the start_turn MODEL last sent and waits to see admitted."
  (setf model.pending-command-id nil
        model.pending-correlation-id nil
        model.pending-prompt nil))

(defun replay-command-turn-id (payload command-id)
  (and command-id
       (loop for frame across (json-array payload "accumulated_frames")
             for metadata = (json-value frame :object "event" "metadata")
             when (equal command-id (json-value metadata :any "command_id"))
               return (or (gethash "turn_id" frame) (gethash "turn_id" metadata) t))))

(defun note-resumed-watermark (model frame)
  "A resumed frame opens a fresh per-peer sequence epoch: REPLACE the
watermarks."
  ;; The gateway sequences each peer's stream independently, so a reconnect's
  ;; epoch can start below the previous connection's; folding it through MAX
  ;; would classify every subsequent live frame as a duplicate and silently
  ;; drop the stream.
  (setf model.sync-lifecycle :connected
        model.last-sequence
        (or frame.sequence
            model.last-sequence)
        model.last-cursor
        (or frame.cursor
            model.last-cursor)
        model.reconnect-required nil))

(defun note-turn-flight-started (model)
  "Enter the in-flight state for the live bar."
  ;; The elapsed clock stamps once per turn here (submit time, the best clock
  ;; before admission lands); ANCHOR-TURN-CLOCK then moves it onto the durable
  ;; turn.started fact, so the bar's clock never restarts at zero mid-turn.
  (unless model.turn-started-at (setf model.turn-started-at (get-internal-real-time)))
  (setf model.turn-in-flight t))

(defun anchor-turn-clock (model)
  "Anchor the live bar's elapsed clock on the durable turn.started fact the
state just folded (`turn_started_at_ms`)."
  ;; The finish divider's elapsed is the difference between two logged facts,
  ;; so the clock the bar runs must be the same clock: elapsed-so-far = now -
  ;; occurred_at, read through the one ISO clock the facts are stamped with,
  ;; then carried into the internal-time units the bar ticks in. A reconnect
  ;; replay onto an active turn re-derives the anchor from the same fact
  ;; instead of restarting at zero. A start in the future (clock skew across
  ;; hosts) clamps to a zero elapsed rather than running a negative clock. A
  ;; state without the fact leaves the submit stamp.
  (when-let (anchor (turn-clock-anchor model.state))
    (setf model.turn-started-at anchor)))

(defun turn-clock-anchor (state)
  "The internal time STATE's turn started at, carried over from its durable
turn.started fact (`turn_started_at_ms'), or NIL without one."
  (let* ((started-ms (json-value state :integer "turn_started_at_ms"))
         (now-ms (and started-ms (iso-8601-ms (nlk::iso-now)))))
    (when (integerp now-ms)
      (- (get-internal-real-time)
         (truncate (* (max 0 (- now-ms started-ms))
                      internal-time-units-per-second)
                   1000)))))

(defun note-turn-flight-settled (model &key handoff)
  "Settle the ended turn; a steer HANDOFF keeps flight and clock for its successor."
  ;; A live Standard turn rests closed, including a call/thought the
  ;; operator opened while it ran. Idle/replay settlements preserve choices.
  (when (and model.turn-in-flight (eq (chat-model-tool-detail model) :standard))
    (setf (chat-model-expanded-tool-call model) nil))
  (unless handoff
    (setf model.turn-in-flight nil model.turn-started-at nil))
  (setf model.turn-activity nil model.active-turn-id nil model.stop-requested nil))

(defun clear-retract-stash (model)
  (setf model.retract-stash nil
        model.retract-requested nil))

(defun stash-owns-turn-p (model turn-id &aux (stash model.retract-stash))
  "Whether TURN-ID is the turn this composer's stashed prompt started."
  (and stash turn-id (equal turn-id (getf stash :turn-id))))

(defun note-turn-admitted (model turn-id)
  "The gateway named the turn this client's start_turn became."
  ;; Stamp the stash with it, and if Esc already asked for the turn back while
  ;; the id was unknown, send that retracting cancel now — WS ordering puts it
  ;; behind the admission, so it always addresses a real turn.
  (setf model.active-turn-id (and (stringp turn-id) turn-id))
  (let ((stash model.retract-stash))
    (when (and stash (stringp turn-id) (null (getf stash :turn-id))
               (equal (getf stash :command-id)
                      model.pending-command-id))
      (setf (getf model.retract-stash :turn-id) turn-id))
    (when (and model.retract-requested (stringp turn-id))
      (setf model.retract-requested nil)
      (send-cancel-turn model.ws
                        model.session-id turn-id
                        :retract (stash-owns-turn-p model turn-id)))))

(defun interrupt-turn (model)
  "Stop the turn MODEL's shell is running: Esc's gesture, and Ctrl+C's on an
empty composer. => true when there was a turn to stop, NIL on an idle shell."
  (cond
    ;; A running turn: cancel it, and — when this composer started it — ask
    ;; for it back. The gateway decides on durable facts whether anything
    ;; was produced; a turn that streamed stays as a plain cancel, one that
    ;; did not comes home to the composer on the retract confirmation
    ;; (session_checkpoint_undo_applied). The bar says `stopping…' from this
    ;; frame: the terminal fact comes once the organism has unwound.
    (model.active-turn-id
     (setf model.stop-requested model.active-turn-id)
     (send-cancel-turn model.ws
                       model.session-id
                       model.active-turn-id
                       :retract (stash-owns-turn-p model model.active-turn-id))
     t)
    ;; Before the start was acknowledged: the turn id is unknown, but WS
    ;; ordering means the gateway admits before it reads any cancel — so the
    ;; retracting cancel is sent the moment the id arrives
    ;; (NOTE-TURN-ADMITTED). Never Esc's tree gesture here.
    ((and model.turn-in-flight
          model.pending-command-id
          (equal model.pending-command-id (getf model.retract-stash :command-id)))
     (setf model.retract-requested t
           model.last-escape-at nil)
     (show-status model "Cancelling")
     (render-state model)
     t)))

(defun restore-retracted-composer (model)
  "The retracted prompt goes home: text, pastes and staged images exactly as
they stood at submit, and the prompt row Enter painted leaves the transcript
(a row the durable fact already hydrated is history; the resync replay that
follows a retraction rebuilds the transcript without it). The stash is spent."
  (when-let (stash model.retract-stash)
    (replace-composer-text model (or (getf stash :text) ""))
    (setf model.pastes (getf stash :pastes)
          model.pending-images (getf stash :images)
          model.state
          (drop-submitted-prompt model.state
                                 (getf stash :command-id)))
    (clear-retract-stash model)
    (show-status model "Retracted")))

(defun retract-refused-start (model message &aux (stash model.retract-stash))
  "The gateway refused the start_turn this shell has pending before it became
a turn: nothing durable happened, so the prompt comes home whole (text,
pastes, images, from the stash Esc would have spent), the row Enter painted
leaves the transcript, the flight settles, and the notice says why."
  (setf model.state (drop-submitted-prompt model.state model.pending-command-id))
  (clear-pending-command model)
  (note-turn-flight-settled model)
  (when stash
    (replace-composer-text model (or (getf stash :text) ""))
    (setf model.pastes (getf stash :pastes) model.pending-images (getf stash :images)))
  (clear-retract-stash model)
  (show-status model (format nil "The gateway refused the prompt: ~a" message) :error-p t))

(defun resolve-pending-admission (model turn-id)
  (note-turn-flight-started model)
  (note-turn-admitted model turn-id)
  (clear-pending-command model)
  (setf model.replay-pending-turn-id nil))

(defun reconcile-pending-command (model payload)
  "Resolve uncertain start delivery from authoritative replay admission facts."
  (let* ((pending model.pending-command-id)
         (admission-state (gethash "admission_state" payload))
         (admitted-turn-id (or model.replay-pending-turn-id
                               (replay-command-turn-id payload pending))))
    (when pending
      (cond
        (admitted-turn-id
         (resolve-pending-admission model admitted-turn-id)
         (unless (equal admission-state "active")
           (note-turn-flight-settled model)))
        ;; Esc took the prompt back before the start was ever acknowledged
        ;; and the replay shows no admission for it: the prompt goes home
        ;; and the command is dropped, never re-sent. (If the gateway had
        ;; admitted and then retracted it, the turn is off-chain — the
        ;; replay carries no frame for it, yet a re-send would hit the
        ;; duplicate-command row and arm this shell on a dead turn.)
        ((and (equal admission-state "ready")
              model.retract-requested)
         (clear-pending-command model)
         (note-turn-flight-settled model)
         (restore-retracted-composer model))
        ((equal admission-state "ready")
         (note-turn-flight-started model)
         (send-start-turn model.ws
                          model.session-id
                          model.pending-prompt
                          :command-id pending
                          :correlation-id model.pending-correlation-id))))))

(defun send-composer-prompt (model text send)
  "The one composer-submit ritual, shared by Tab's mid-turn queue submit and
by Enter's start and steer legs: TEXT joins the prompt-recall ring, the
transcript pins to its bottom, the expanded prompt and the staged images are
captured BEFORE the composer is emptied, the composer is emptied, and SEND
is then called with those two."
  ;; A transport that refuses does NOT put the text back — the gateway may
  ;; well have taken it — so the refusal lands as the Zig delivery-uncertain
  ;; notice rather than as a failure, in plain words (PLAIN-ERROR-TEXT's, or
  ;; none of the condition's own). A frame the sync bound refuses
  ;; (NLK:FRAME-TOO-LARGE), or one for a link that cannot carry it now
  ;; (GATEWAY-NOT-CONNECTED: none yet, or one that dropped), never left this
  ;; process: text, pastes and images go back into the composer whole, and the
  ;; notice says why. A dropped socket's send takes the frame and carries
  ;; nothing, so the link is asked before any leg arms a turn. Each leg
  ;; undoes what it armed before sending where it armed it.
  (note-submitted-prompt model text)
  (pin-transcript-bottom model)
  (let ((prompt (expand-composer-pastes model text))
        (pastes model.pastes)
        (images model.pending-images))
    (clear-composer-attachments model)
    (editor-reset model.editor)
    (show-status model nil)
    (let ((sent t))
      (handler-case (progn (unless (gateway-link-open-p model.ws)
                             (error 'gateway-not-connected))
                           (funcall send prompt images))
        ((or nlk:frame-too-large gateway-not-connected) (condition)
          (setf sent nil)
          (replace-composer-text model text)
          (setf model.pastes pastes
                model.pending-images images)
          (show-status model (format nil "Prompt not sent: ~a~@[; ~d image~:p staged~]"
                                     (if (typep condition 'nlk:frame-too-large)
                                         (nlk:protocol-error-message condition)
                                         (format nil "~a; Enter sends it once it is" condition))
                                     (and images (length images)))
                       :error-p t))
        (error (condition)
          (show-status model (format nil "Gateway delivery uncertain: the link to the gateway failed ~
                                          while sending~@[ (~a)~]"
                                     (multiple-value-bind (words plain) (plain-error-text condition)
                                       (and plain words)))
                       :error-p t)))
      ;; The prompt left the composer for the gateway: any hold Up's peel
      ;; took ends here. A frame the bound refused never left — the hold
      ;; stands with the text.
      (when sent
        (release-held-prompt model)))))

;;; --- checkpoint listing prefetch ------------------------------------------
;;; The Esc-Esc checkpoint gesture spends its first half arming: the operator
;;; has announced intent, and the 500 ms window before the second press is
;;; dead time the shell used to spend blocked on the listing round trip with
;;; nothing to paint. The fetch starts on the arming press instead, on a
;;; thread of its own, and the picker consumes it through the one seam every
;;; control call passes: GATEWAY-MODEL-HTTP-CALL.

(defparameter *checkpoint-listing-prefetch* nil
  "The one in-flight-or-landed checkpoint listing this shell fetched ahead of
the operator's second Esc, as a plist:

  :PATH       the control path fetched — matched exactly against the request
              the tree picker later makes, so a listing for another session
              or another `limit' can never be served
  :UNTIL      the internal-real-time it may be served until (the freshness bound)
  :SEMAPHORE  signalled by the worker when the answer landed
  :BOX        a one-cons mailbox the worker writes the parsed response into

Live-only by nature (a prefetch outlives nothing) and
single-writer/single-reader: the worker thread writes :BOX and signals, the UI
thread reads it and clears this place.")

;;; The Esc-Esc gesture window is half a second (CLIENT.LISP's arming branch),
;;; so a prefetch armed by the first Esc is at most that old when the second
;;; consumes it; the bound here is the slack around it, and it is deliberately
;;; shorter than any interval in which a turn could land new checkpoints. Past
;;; it the picker fetches fresh.
(defparameter *checkpoint-listing-prefetch-window-seconds* 2
  "How long a prefetched checkpoint listing may be served.")

;;; --- update method for gateway-chat-model -------------------------------

(defun key-msg-matches-p (model action msg)
  "Whether the key press MSG dispatches ACTION under MODEL's keymap."
  (model-key-matches-p model action (nlt:key-event-code msg) (nlt:key-event-mod msg)))

(defmethod nlt:update ((model gateway-chat-model) msg)
  (cond
    ((typep msg 'gw-projection-msg)
     (let* ((frame (gw-projection-msg-frame msg))
            (payload frame.payload)
            (kind (gethash "type" payload)))
       ;; Refine the span label the UPDATE :around bound: the payload kind
       ;; names which projection pass a worst span was, and the string is
       ;; already decoded — no consing on the stream path.
       (when kind (setf nlk:*attribution-label* kind))
       (cond
         ((and (sync-message-session-id frame)
               (not (string= (sync-message-session-id frame)
                             model.session-id))
               (equal (sync-message-session-id frame)
                      model.preview-session-id))
          ;; The followed session's own stream: into the preview lane,
          ;; never the live state or its watermarks.
          (let ((preview model.transcript-preview))
            (when (and preview (equal (car preview) frame.session-id))
              (cond
                ;; The live reducer, whose verdict feeds the bar under the dialog.
                ((member kind '("session_stream_event" "item_delta") :test #'string=)
                 (multiple-value-bind (state verdict)
                     (apply-gateway-event (cdr preview) payload)
                   (setf (cdr preview) state)
                   (preview-keep-event preview payload)
                   (let ((verb (getf verdict :activity)))
                     (when (or verb (getf verdict :lifecycle))
                       (setf model.preview-activity
                             (and verb (list verb (getf verdict :kind)
                                             (get-internal-real-time))))))
                   ;; A found-block pin outranks the live follow: the
                   ;; operator asked to read that block, and a stream frame
                   ;; moves the session, never the promise.
                   (unless (transcript-pin-p model (car preview))
                     (pin-transcript-bottom model))
                   (if (live-chrome-animating-p model)
                       (setf model.transcript-dirty t)
                       (render-state model))))
                ((string= kind "session_resync_required")
                 (setf model.transcript-preview nil)
                 (sessions-preview-arm model (car preview)))))))
         ((or (null (sync-message-session-id frame))
              (string= (sync-message-session-id frame)
                       model.session-id))
          (cond
           ((string= kind "session_stream_resumed")
            ;; A replay folds out of sight and is put in place at its last
            ;; chunk, so whatever renders meanwhile -- a key, the clock --
            ;; shows the transcript that was, never a half-folded one. A long
            ;; session's replay runs twice (gateway.lisp SEND-REPLAY-PASS):
            ;; the first pass, its newest turns, paints at its own last chunk,
            ;; and the whole log behind it takes that one's place.
            (let ((matched (replay-command-turn-id payload model.pending-command-id))
                  (later (plusp (or (gethash "replay_chunk_index" payload) 0))))
              (when (or matched (not later))
                (setf model.replay-pending-turn-id matched))
              (setf model.replay-staging
                    (fold-accumulated-frames (gethash "accumulated_frames" payload)
                                             (if later model.replay-staging (initial-chat-state))))
              ;; The name rides the replay itself: no frame folds it.
              (seat-session-title model.replay-staging (gethash "title" payload)))
            (note-resumed-watermark model frame)
            ;; A replay renders once, at its last chunk. A render re-projects
            ;; the whole transcript, so one per chunk would be quadratic in the
            ;; session — and each would show the tail of a half-folded history:
            ;; without this, resuming a large session would scroll through old
            ;; turns before resting on the real bottom.
            (when (gethash "replay_chunk_final" payload)
              (show-replayed-state model)
              (cond ((equal "active" (gethash "admission_state" payload))
                     (note-turn-flight-started model)
                     (anchor-turn-clock model)
                     (setf model.active-turn-id (gethash "turn_id" model.state)))
                    (t (note-turn-flight-settled model)))
              (cond
                ;; The first pass: painted, and nothing else is settled on it.
                ((gethash "replay_tail" payload)
                 (keep-window-from-bottom model (lambda () (render-state model)))
                 (nlk:note-boot-phase "transcript-painted" frame.session-id))
                (t
                 (setf model.quiet-resync nil)
                 (reconcile-pending-command model payload)
                 (keep-window-from-bottom model (lambda () (render-state model)))
                 (nlk:note-boot-phase "transcript-replayed" frame.session-id)
                 ;; The frame the attach was for is on screen now. A replay
                 ;; paints once, at its end, so the screen stands still while it
                 ;; folds; a trim armed at the attach would take that stillness
                 ;; for a settled shell and run its stop-the-world collection
                 ;; inside the replay it was meant to wait out.
                 (trim-behind-the-ready-frame)
                 (refresh-composer-catalogs-behind-the-frame model)))))
           ((string= kind "gateway_turn_command_result")
            (gateway-command-result model (gethash "result" payload))
            (render-state model))
           ((string= kind "sync_protocol_error")
            ;; Unsequenced (sequence 0): classified before the watermark
            ;; check, or the gateway's own diagnostic for a tombstoned peer
            ;; would be swallowed as a duplicate. A refusal names no command
            ;; -- the gateway refuses before it decodes -- but while this
            ;; shell's start_turn awaits admission it is the one frame of
            ;; ours a healthy gateway can be refusing, so the prompt comes
            ;; home instead of the shell sitting in flight on a turn that
            ;; never began. The permanent diagnostic is the tombstoned
            ;; peer's; the reconnect ladder settles that one through the
            ;; replay it earns.
            (let ((message (result-refusal payload "unknown")))
              (if (and (not (json-value payload :boolean "permanent"))
                       model.pending-command-id (null model.active-turn-id))
                  (retract-refused-start model message)
                  (show-status model (format nil "Gateway protocol error: ~a" message) :error-p t)))
            (render-state model))
           ((string= kind "organism.activity")
            ;; Which sessions are running a turn right now, and since when
            ;; (activity.lisp): session-less and unsequenced like a notice,
            ;; the whole map each time the set changes. Folded into this
            ;; shell's own table; an open /sessions picker draws its live
            ;; column from it again then. Live-only: never replayed.
            (when (nth-value 1 (ingest-session-activity model payload))
              (refresh-sessions-dialog model))
            (refresh-status-dialog model :activity t))
           ((string= kind "organism.notices")
            (refresh-status-dialog model :board payload))
           ((string= kind "organism.notice")
            ;; The organism speaking to every attached shell (NLE:NOTICE):
            ;; unsequenced and session-less, classified before the watermark
            ;; check like a protocol error, put up as a toast (TOAST) — an
            ;; error until it is closed, anything else for five seconds, a
            ;; keyed one in place of the toast under its key. Live-only:
            ;; never a row, never replayed. While the
            ;; setup walk is open the cell board line is the walk's own
            ;; question. Without this, the notice would stand over that
            ;; question as a list the walk contradicts.
            (with-json ((text :string "text") (level :string "level") (key :string "key")) payload
              (unless (and model.setup-walk (equal "nodecode-shipped-cells" key))
                (toast model text :error-p (equal "error" level) :key key))))
           ((string= kind "organism.loaded")
            ;; The organism's image made definitions live from a file of
            ;; its trees (NLK:SOURCE-LOADED): this shell evaluates the same
            ;; ones from its own copy, whichever session it shows -- the
            ;; image is one -- and says so as a system row only when the
            ;; payload names the session this shell shows, or names none (a
            ;; load outside any turn). Another session's edit changes this
            ;; image silently: that session's receipt already carries the
            ;; load, and a row for it here would read as its transcript
            ;; leaking into this one. Unsequenced and session-less like a
            ;; notice. Live-only: never replayed -- a shell that starts
            ;; later reads the file. :ONLY refuses files outside this
            ;; image's trees; synchronous, for the frames behind.
            (bind ((path (gethash "path" payload))
                   (definitions (gethash "definitions" payload))
                   (origin (gethash "session_id" payload))
                   ;; What the load must leave working is this shell's frame:
                   ;; a frame that composed before it and fails after is put
                   ;; back, never left failing at every message.
                   (paint (lambda ()
                            (render-state model)
                            (render-status-frame model)
                            (nlt:view model)))
                   (painted (and (stringp path) (ignore-errors (funcall paint) t)))
                   ((line _ held)
                    (and (stringp path) (vectorp definitions)
                         (nlk:load-source
                          path
                          :only (loop for entry across definitions
                                      when (hash-table-p entry)
                                        collect (cons (gethash "operator" entry)
                                                      (gethash "label" entry)))
                          :verify (and painted paint)))))
              (when (and line (or (not (stringp origin)) (string= origin model.session-id)))
                (let* ((text (format nil "[~a — this shell: ~a]" (file-namestring path) line))
                       (turn-id (json-value payload :string "turn_id")))
                  ;; A load on this shell can fail even though the organism's
                  ;; eval succeeded. Keep that live-only failure in its call,
                  ;; without rewriting the durable tool result or splitting
                  ;; Standard's completed work trail with a diagnostic card.
                  (unless (and held
                               (ingest-tool-load-error model.state
                                                      (json-value payload :text "call_id")
                                                      turn-id text))
                    (setf model.state
                          (ingest-system-notice
                           model.state text :turn-id turn-id
                           :retention :live-only :error-p (and held t)
                           :metadata (unless held
                                       (json-object "summary"
                                         (json-object "name" "Mutate" "path" path
                                                      "comment" (or (json-value payload :text "comment")
                                                                    (file-namestring path)))))))))
                (render-state model))))
           ((string= kind "organism.cells")
            ;; The organism turned folders on (SWITCH-CELL-ON): this shell
            ;; loads the same ones, as its boot would have, so its /cells
            ;; lists them and their code that runs in a shell runs here. It
            ;; starts no peripheral -- the organism runs those. Live-only.
            ;; Off the loop, as the boot's own load is. Without this, the
            ;; import folder's load would hold the setup walk's panel past
            ;; the import it waits for. A failed load is on the folder's
            ;; record, which /cells reads.
            (when-let (names (json-value payload :array "names"))
              (off-loop model
                        (lambda ()
                          (apply #'nlk:load-cell (remove-if-not #'stringp (coerce names 'list))))
                        (constantly :quiet))))
           ((string= kind "organism.upgraded")
            ;; The organism's image moved to a staged release and reads
            ;; itself from a new tree (ANNOUNCE-UPGRADED): this shell
            ;; converges its own image on the same tree and says what it
            ;; did as a toast, unasked news. Live-only, like organism.loaded.
            (with-json ((root :string "root") (version :string "version")) payload
              (when-let (line (and root version (converge-shell root version)))
                (toast model (format nil "update ~a — this shell: ~a" version line)))))
           ((string= kind "organism.note")
            ;; A note the organism says once, in the model's words (a boot
            ;; that is back, a release it runs for the first time), once per
            ;; text in this shell -- it comes again with every attach while
            ;; it is fresh. A chat that has begun shows it as a row: a
            ;; toast's four lines would cut it, and the row is its words
            ;; alone, no band (BARE). The home screen shows none: the
            ;; landing screen is the mark, the version and the tip. A note
            ;; that came to home is spent there, so no later attach puts it
            ;; into the chat that begins after. Live-only.
            (let ((text (json-value payload :string "text")))
              (unless (member text *organism-notes-shown* :test #'equal)
                (push text *organism-notes-shown*)
                (when (chat-begun-p model)
                  (setf model.state (ingest-system-notice model.state text
                                                          :metadata (json-object "bare" t)))
                  (render-state model)))))
           ((string= kind "model_changed")
            ;; A model or effort pick landed at the gateway from some
            ;; other origin — the organism's own SELECT-MODEL, another
            ;; client, a route. The cached footer facts are stale the
            ;; moment it lands; refetch off the loop thread (the frame
            ;; path never dials). Unsequenced like a notice, so consuming
            ;; it here leaves no watermark hole. A frame scoped to some
            ;; other session leaves this shell's facts alone; a
            ;; session-less one says the gateway default moved, which
            ;; this session may be following.
            (when (member (json-value payload :any "session_id")
                          (list nil model.session-id) :test #'equal)
              (invalidate-gateway-model-facts model)
              (refresh-gateway-model-facts model)))
           ((string= kind "session_titled")
            ;; The session this shell shows was named -- the operator's
            ;; /rename, a cell, the title a model wrote beside the first
            ;; turn -- and the header wears the name from here. A replay
            ;; still folding out of sight takes the same name, or its swap
            ;; into place would put the old one back.
            (when (equal (gethash "session_id" payload) model.session-id)
              (seat-session-title model.state (gethash "title" payload))
              (seat-session-title model.replay-staging (gethash "title" payload))
              (render-state model)))

           ((string= kind "commands_changed")
            ;; A cell started or stopped at the gateway: the cell
            ;; commands this shell read at attach are stale, and the boot
            ;; starts the peripherals after every shell has attached, so this
            ;; frame is the only cue that /cron, /web and their kin
            ;; exist. Refetch off the loop thread -- the frame path never
            ;; dials; the answer lands as the same message INIT's read
            ;; installs. Unsequenced, so consuming it here leaves no watermark
            ;; hole.
            (refresh-gateway-command-catalog model))
           ((string= kind "session_checkpoint_undo_applied")
            ;; Notice only; the paired session_resync_required drives the
            ;; actual re-replay. Handled before frame classification so the
            ;; unknown inner shape never reaches APPLY-GATEWAY-EVENT.
            ;; A retraction of this composer's own turn is the gateway's
            ;; confirmation that the prompt is off the chain: it goes home
            ;; now, quietly — any other peer sees the ordinary notice.
            (cond ((and (equal (gethash "reason" payload)
                               nlk:+head-move-reason-retract+)
                        (stash-owns-turn-p model (gethash "previous_turn_id" payload)))
                   (restore-retracted-composer model)
                   (setf model.quiet-resync t))
                  (t (show-status model (format nil "History ~a: the head moved"
                                                (or (gethash "reason" payload) "rewind")))))
            (render-state model))
           ((or (string= kind "session_resync_required")
                (eq (gateway-frame-accepted-p model frame) :gap))
            ;; A head move rewrote visible history server-side. Same epoch
            ;; guard as the gap path: one resubscribe per epoch; the replay
            ;; that answers it serves only the visible chain, and every
            ;; client-held KV CAS token is stale from here.
            ;; One resubscribe per gap epoch. The watermark stays put until the
            ;; resumed snapshot replaces it, so every in-flight frame queued
            ;; behind the first gap classifies :gap too; RECONNECT-REQUIRED is
            ;; the epoch guard — set here, cleared by NOTE-RESUMED-WATERMARK —
            ;; and without it each queued frame fires another full replay job.
            (unless model.reconnect-required
              (setf model.reconnect-required t model.sync-lifecycle :reconnecting)
              (cond ((string/= kind "session_resync_required")
                     (show-status model "Gateway resync required"))
                    ((not model.quiet-resync) (show-status model "History rewound; resyncing")))
              (send-subscribe model.ws (subscription-entries model))
              (note-shell-presence model.session-id)
              (render-state model)))
           ((eq (gateway-frame-accepted-p model frame) :apply)
            ;; A live frame the pending start_turn produced admits it: the
            ;; turn it names, or T when it names none.
            ;; The stream frame is under "frame", or PAYLOAD itself when it carries none.
            (let* ((stream-frame (json-value (or (json-value payload :any "frame") payload)
                                             :object))
                   (metadata (json-value stream-frame :object "event" "metadata"))
                   (command-id model.pending-command-id))
              (when (and command-id metadata
                         (equal command-id (gethash "command_id" metadata)))
                (resolve-pending-admission
                 model (or (gethash "turn_id" stream-frame)
                           (gethash "turn_id" metadata)
                           t))))
            (multiple-value-bind (state verdict)
                (apply-gateway-event model.state payload)
              (setf model.state state
                    model.last-sequence
                    (max (or model.last-sequence 0) (or frame.sequence 0))
                    model.last-cursor
                    (max (or model.last-cursor 0) (or frame.cursor 0)))
              (case (getf verdict :lifecycle)
                (:started
                 (note-turn-flight-started model)
                 (anchor-turn-clock model)
                 (setf model.active-turn-id
                       (getf verdict :turn-id)))
                ((:completed :failed :cancelled)
                 (let* ((ended (getf verdict :lifecycle))
                        ;; A completion a parked steer cut short is a handoff,
                        ;; not an end: the promoted text starts the next turn,
                        ;; so the flight crosses to it. The dead turn's id and
                        ;; stop mark go; the clock keeps running until the
                        ;; promoted start anchors it. Without this, settling the
                        ;; turn as an end would open an end-to-start gap that an
                        ;; Enter fills with a fresh start_turn; the chained
                        ;; steer would become a queued follow-up, one at a time.
                        (handoff (and (eq :completed ended) (getf verdict :steered))))
                   ;; The transient hint announces completion in the
                   ;; composer's placeholder slot.
                   (when (and (eq :completed ended) (not handoff))
                     (show-status model "Completed"))
                   ;; A turn that completed or failed produced its history; the
                   ;; stashed prompt has nothing to return to. A cancelled turn
                   ;; keeps it: the retraction's head move may still follow.
                   (when (and (not (eq :cancelled ended))
                              (stash-owns-turn-p model model.active-turn-id))
                     (clear-retract-stash model))
                   (note-turn-flight-settled model :handoff handoff)
                   ;; A turn may have spent the account's requests or credit.
                   (unless handoff (refresh-account-meter model)))))
              ;; Stream frames feed the live-bar activity stamp (the Zig
              ;; active_turn_phase): the verb and the mark's pulse/sway both
              ;; read it, so silence between frames decays into "waiting".
              (let ((verb (getf verdict :activity)))
                (when (and verb model.turn-in-flight)
                  (note-turn-activity model verb (getf verdict :kind)))))
            ;; Coalesce live-stream renders onto the frame clock: while
            ;; chrome animates (turn in flight or band rows live) the next
            ;; tick — at most one frame period away — projects every frame
            ;; that landed since the last render in one pass. A frame that
            ;; arrives with nothing animating renders immediately: no tick
            ;; is coming to flush it.
            (if (live-chrome-animating-p model)
                (setf model.transcript-dirty t)
                (render-state model)))))
         ((equal frame.session-id model.preview-session-id)
          ;; The followed session's own stream: into the preview lane,
          ;; never the live state or its watermarks.
          (let ((preview model.transcript-preview))
            (when (and preview (equal (car preview) frame.session-id))
              (cond
                ;; The live reducer, whose verdict feeds the bar under the dialog.
                ((member kind '("session_stream_event" "item_delta") :test #'string=)
                 (multiple-value-bind (state verdict)
                     (apply-gateway-event (cdr preview) payload)
                   (setf (cdr preview) state)
                   (let ((verb (getf verdict :activity)))
                     (when (or verb (getf verdict :lifecycle))
                       (setf model.preview-activity
                             (and verb (list verb (getf verdict :kind)
                                             (iso-8601-ms (nlk::iso-now)))))))
                   ;; A found-block pin outranks the live follow: the
                   ;; operator asked to read that block, and a stream frame
                   ;; moves the session, never the promise.
                   (unless (transcript-pin-p model (car preview))
                     (pin-transcript-bottom model))
                   (if (live-chrome-animating-p model)
                       (setf model.transcript-dirty t)
                       (render-state model))))
                ((string= kind "session_resync_required")
                 (setf model.transcript-preview nil)
                 (sessions-preview-arm model (car preview)))))))))
     (when (gw-projection-msg-release msg)
       (funcall (gw-projection-msg-release msg))))
    ((typep msg 'gw-connection-msg)
     (setf model.ws (gw-connection-msg-ws msg) model.sync-lifecycle :connected)
     (refresh-status-dialog model)
     (refresh-account-meter model)
     ;; A fresh connection is also this client's only cue that the gateway
     ;; may have moved its model config while we were away — and, on the
     ;; first one, the point at which the control surface is known reachable
     ;; at all. Queue the worker read rather than render a stale identity;
     ;; the frame path itself never dials. The read is a RE-read, not a
     ;; blanking: what the gateway already confirmed for this session stays
     ;; on screen until the new answer replaces it, and the footer reads
     ;; `setting up…' only while nothing has ever answered. (An invalidation
     ;; here repainted the placeholder for the length of one loopback round
     ;; trip on every attach.)
     ;; A fresh connection retires any "[gateway error: ...]" transport banner;
     ;; genuinely in-flight stream text is rebuilt by the replay that follows
     ;; the resubscribe, so clearing the live-only notice loses nothing durable.
     ;; the link's failure, said while it was down, is over
     (let ((status (chat-model-composer-status model)))
       (when (and status (composer-status-error-p status))
         (show-status model nil)))
     (refresh-gateway-model-facts model)
     ;; The gateway's cell commands ride the same moment: on a paint-first
     ;; boot INIT's read ran before any endpoint existed and refused
     ;; structurally; a fresh connection is when the control surface is
     ;; provably answering, so read it again (idempotent install).
     (return-from update (values model (read-command-catalog-cmd model))))
    ((typep msg 'gw-transport-error-msg)
     (let ((verdict (gw-transport-error-msg-verdict msg)))
       (show-status model (format nil "Gateway error: ~a" (gw-transport-error-msg-text msg)) :error-p t)
       (setf model.sync-lifecycle
             (case verdict ((:permanent) :failed) ((nil) :reconnecting) (t verdict)))
       (refresh-status-dialog model)
       ;; The bar settles; the turn id stays armed for the link that comes back.
       (when (eq verdict :permanent)
         (setf model.turn-in-flight nil model.turn-started-at nil model.turn-activity nil))
       (render-state model)))
    ;; An open modal picker owns its keys — the base update routes them. So
    ;; does every other message; the arms below are all key presses.
    ((or (not (nlt:key-press-msg-p msg)) model.dialog)
     (return-from update (call-next-method model msg)))
    ;; Every key advances or resets the Konami code's run, ahead of the arms
    ;; below: its Left is not the leave gesture (KONAMI-KEY).
    ((let ((mod (nlt:key-event-mod msg)))
       (konami-key model (nlt:key-event-code msg)
                   (not (or (nlt:mod-contains mod nlt:+mod-ctrl+)
                            (nlt:mod-contains mod nlt:+mod-alt+))))))
    ;; Up on a queued prompt takes it back out of the queue and into the
    ;; composer (the peel): the queue outranks prompt history, which keeps Up
    ;; whenever nothing is peelable — including while this composer already
    ;; holds one, so a stray Up can never stack a second peel.
    ((and (key-msg-matches-p model :history-previous msg) (peel-queued-steer model)))

    ;; Tab while a turn runs queue-submits (the Zig submit_turn_queue
    ;; gesture, isSubmitTurnQueueKey): an ordinary start_turn the gateway
    ;; parks behind the active turn — no dedicated queue command exists on
    ;; the wire. This branch outranks the dropdown, mirroring the Zig
    ;; handleComposerKey order (mid-turn Tab always queues, even with a
    ;; suggestion open); an empty composer is a consumed no-op (the Zig
    ;; trim-and-return-null rule). Idle Tab falls through to the base
    ;; update, where accept-suggestion keeps its meaning.
    ((and model.turn-in-flight (key-msg-matches-p model :submit-turn-queue msg))
     (unless (composer-empty-p model)
       ;; Fire-and-forget with a fresh command id: the pending-command
       ;; machinery stays reserved for the primary start's replay
       ;; reconciliation — a queued Tab submit registered there would
       ;; be re-sent as a fresh turn by RECONCILE-PENDING-COMMAND's
       ;; "ready" branch after a reconnect. The queued row itself is
       ;; the durable follow-up input_committed fact coming back on
       ;; the live stream (retention: persisted-history).
       (send-composer-prompt
        model (editor-value model.editor)
        (lambda (prompt images)
          (send-start-turn model.ws
                           model.session-id prompt
                           :images images)))
       (render-state model)))
    ((and (eq (nlt:key-event-code msg) :left)
          (not (nlt:mod-contains (nlt:key-event-mod msg) nlt:+mod-ctrl+))
          (not (nlt:mod-contains (nlt:key-event-mod msg) nlt:+mod-alt+))
          (zerop (length (editor-value model.editor))))
     ;; Left on an idle composer leaves the session (LEAVE-SESSION-OP): it
     ;; runs on without this shell, and the shell goes home with /sessions
     ;; open on it -- Claude Code's gesture.
     ;; With text in the composer Left is the cursor move it always was.
     ;; The home screen leaves too: the shell holds an attached session
     ;; whatever its transcript says, the same gate the footer hint dropped.
     (when-let (notice (leave-session-op model))
       (show-status model notice))
     (render-state model))
    ((eq (nlt:key-event-code msg) :escape)
     (cond
       ;; Esc on a running turn stops it (Ctrl+C on an empty composer
       ;; calls the same INTERRUPT-TURN).
       ((interrupt-turn model))
       ;; With nothing else to do — no turn, nothing marked, an empty
       ;; composer and so no dropdown; an open dialog took every key above
       ;; — Esc takes the newest toast down, as the × does.
       ((and (chat-model-toasts model)
             (null (chat-model-selection model))
             (composer-empty-p model))
        (close-toast model (first (chat-model-toasts model))))
       (t
        ;; Esc-Esc on an idle shell opens the checkpoint tree (the Zig
        ;; checkpoint-focus gesture); a lone Esc arms the gesture.
        (let ((now (get-internal-real-time))
              (last model.last-escape-at))
          (cond
            ((and last (< (- now last)
                          (truncate internal-time-units-per-second 2)))
             (setf model.last-escape-at nil)
             (when-let (notice (slash-tree-op model))
               (show-status model notice))
             (render-state model))
            (t
             ;; The listing the second press will open the picker on is
             ;; fetched now, off the loop thread: the round trip overlaps
             ;; the operator's own keystroke instead of the empty frame.
             ;; An unconsumed one inside its window stays: lone Escs cost one.
             (let* ((port model.http-port)
                    (path (checkpoint-listing-path model))
                    (entry *checkpoint-listing-prefetch*))
               (unless (or (null port) (null path)
                           (and entry (equal path (getf entry :path))
                                (< (get-internal-real-time) (getf entry :until))))
                 (let ((token model.http-token)
                       (box (list nil))
                       (semaphore (bt2:make-semaphore)))
                   (setf *checkpoint-listing-prefetch*
                         (list :path path :semaphore semaphore :box box
                               :until (deadline-after
                                       *checkpoint-listing-prefetch-window-seconds*)))
                   (spawn "checkpoint-listing-prefetch"
                     (with-cleanup ((bt2:signal-semaphore semaphore))
                       (setf (car box)
                             (ignore-errors
                              (gateway-http-session-request port :get path nil token))))))))
             (setf model.last-escape-at now)))))))
    ;; Submit dispatches through the resolved keymap; a chord bound
    ;; to insert-newline (Shift/Alt+Enter by default) belongs to the
    ;; composer via the inherited update.
    ((and (key-msg-matches-p model :submit msg) (not (key-msg-matches-p model :insert-newline msg)))
     ;; An open @file dropdown owns Enter: the highlighted token
     ;; completes instead of submitting the partial prompt.
     (when (accept-token-suggestion-for-submit model)
       (return-from update (values model nil)))
     (let* ((text (editor-value model.editor))
            (typed (and text (plusp (length (string-trim '(#\Space #\Tab #\Newline) text))))))
       ;; Enter on an empty composer while this shell holds a peeled prompt:
       ;; the hold is spent — the prompt is neither queued nor run.
       (when (and (null typed) (held-prompt model.state))
         (release-held-prompt model)
         (show-status model "Held prompt dropped")
         (return-from update (values model nil)))
       (when (slash-input-p text)
         ;; Slash commands dispatch through the shared submit seam (ui.lisp);
         ;; EXECUTE-SLASH-COMMAND below routes the gateway-backed ones over
         ;; the HTTP control ops.
         (return-from update (values model (nth-value 1 (submit-slash model text)))))
       (when (and typed (not model.turn-in-flight))
         (let ((command-id (nlk:make-durable-id "command"))
               (correlation-id (nlk:make-durable-id "correlation"))
               ;; Collapsed [Paste #N] markers expand into the outgoing
               ;; prompt; staged images ride the command. History keeps the
               ;; readable marker form, so the stash reads them here, before
               ;; the submit empties the composer.
               (pastes model.pastes))
           (send-composer-prompt
            model text
            (lambda (prompt images)
              (setf model.turn-in-flight t
                    ;; A fresh submit restarts the live-bar clock and retires
                    ;; the previous turn's activity stamp.
                    model.turn-started-at (get-internal-real-time)
                    model.turn-activity nil
                    model.pending-command-id command-id
                    model.pending-correlation-id correlation-id
                    model.pending-prompt prompt
                    ;; The composer as typed, kept until the turn settles: a
                    ;; retraction hands it back whole (RESTORE-RETRACTED-COMPOSER).
                    model.retract-stash
                    (list :text text
                          :pastes pastes
                          :images images
                          :command-id command-id
                          :turn-id nil)
                    model.retract-requested nil
                    ;; The prompt shows the moment Enter lands, at the
                    ;; tail where its durable fact will rest: a live-only
                    ;; row keyed by this command id, which that fact
                    ;; hydrates in place (INGEST-MESSAGE-FACT). Painted
                    ;; before the send, so the frame after Enter never
                    ;; waits on the loopback round trip.
                    model.state
                    (ingest-submitted-prompt model.state prompt
                                             :prompt-id command-id))
              ;; A transport that refuses takes the row back before the
              ;; refusal lands as the delivery-uncertain notice: a re-send
              ;; after reconnect appends its own fact row. A frame the sync
              ;; bound refuses never left: nothing is pending, nothing is
              ;; in flight, and there is no stash to hand back later.
              (handler-bind ((error (lambda (condition)
                                      (setf model.state
                                            (drop-submitted-prompt model.state command-id))
                                      (when (typep condition 'nlk:frame-too-large)
                                        (clear-pending-command model)
                                        ;; The flight settles; any turn id armed stays.
                                        (setf model.turn-in-flight nil model.turn-started-at nil
                                              model.turn-activity nil)
                                        (clear-retract-stash model)))))
                (send-start-turn model.ws
                                 model.session-id prompt
                                 :command-id command-id
                                 :correlation-id correlation-id
                                 :images images))))))
       ;; The steer leg: submitting while a turn runs steers exactly that
       ;; turn. Fire-and-forget — the pending-command machinery stays
       ;; reserved for start_turn, whose replay reconciliation would
       ;; otherwise re-send a steer as a fresh turn after reconnect.
       (when (and typed model.turn-in-flight model.active-turn-id)
         (let ((command-id (nlk:make-durable-id "command")))
          (send-composer-prompt
           model text
           (lambda (prompt images)
             ;; Paint the queue state before the round trip: the durable
             ;; queue snapshot names this prompt id when it arrives and
             ;; updates the same band row; the drained INPUT_COMMITTED
             ;; retires it into the transcript.
             (setf model.state
                   (ingest-pending-input-fact
                    model.state prompt
                    :prompt-id command-id
                    :turn-id model.active-turn-id
                    :steer t))
             ;; A frame the sync bound refuses never left: the band row
             ;; painted for it leaves with the refusal.
             (handler-bind ((nlk:frame-too-large
                              (lambda-ignoring (condition)
                                (setf model.state
                                      (clear-pending-input model.state command-id prompt)))))
               (send-steer-turn model.ws
                                model.session-id
                                model.active-turn-id
                                prompt
                                :command-id command-id
                                :images images))))))
       (render-state model)))
    (t (return-from update (call-next-method model msg))))
  ;; Every arm that did not return answers the model and no command.
  (values model nil))

(defun gateway-configured-operator-token ()
  "Resolve the local Gateway token without opening Gateway persistence: the
environment, the token BOOT-OWNED-ORGANISM materialized, the shared config."
  (or (some #'uiop:getenv '("NODECODE_GATEWAY_OPERATOR_TOKEN" "NODECODE_OPERATOR_TOKEN"))
      (with-open-file (stream (home "operator-token") :if-does-not-exist nil)
        (and stream (json-value (read-line stream nil nil) :text)))
      (ignore-errors (json-value (read-shared-config) :text "gateway" "operator_token"))))

(defun organism-port (&aux (state (ignore-errors (nlk:gateway-state))))
  "The port of the organism serving this home, or NIL when none is running."
  (and state (nlk:gateway-state-owner-port state)))

(define-error gateway-http-error (error)
    (status (code :initform nil) detail)
  (:report "Gateway HTTP ~d~@[ (~a)~]: ~a" status code detail))

(defun gateway-http-session-request (port method path body token
                                     &key (connect-timeout 5) (read-timeout 30) raw stream)
  "Call the Gateway HTTP control surface without opening its SQLite store.
RAW answers a 200 as octets; STREAM leaves a successful body open for its
caller. Both return headers. Refusals use the same closed, structured error lane."
  ;; NLK:HTTP answers every status, so each flows through the one
  ;; structured-error lane below. PORT NIL refuses
  ;; structurally before any dial: the shell paints before an endpoint exists,
  ;; and a control call from that window must read as the gateway being
  ;; unreachable, not as a malformed URL. CONNECT-TIMEOUT and READ-TIMEOUT
  ;; bound the call in seconds.
  (unless port
    (error 'gateway-http-error :status 503 :code "gateway_unreachable"
           :detail "no gateway endpoint is published yet"))
  (multiple-value-bind (response status headers)
      ;; Bounded, always: an endpoint that stops answering must cost a caller
      ;; seconds, never dexador's unbounded default read wait. The localhost
      ;; control connection is pooled; the save-hook guard in provider.lisp's
      ;; connection-reuse block keeps pooled sockets out of any dumped core.
      ;; The body is read as octets, the form the JSON reader walks: read as
      ;; text it was decoded to characters and encoded back before the walk.
      (nlk:http method (format nil "http://127.0.0.1:~d~a" port path)
                :headers `(("content-type" . "application/json")
                           ,@(and token `(("authorization" . ,(format nil "Bearer ~a" token)))))
                :content (if (hash-table-p body) (encode-json-object body) body)
                :connect-timeout connect-timeout :timeout read-timeout :pool t
                :binary t :stream stream)
    (when (and (or stream raw) (= status 200))
      (return-from gateway-http-session-request (values response headers)))
    (when (streamp response)
      (setf response (unwind-protect (nlk:body-text response)
                       (close response :abort t))))
    (let ((parsed (ignore-errors (nlk:decode-json response))))
      (unless (= status 200)
        (error 'gateway-http-error :status status
               :code (json-value parsed :any "error" "code")
               :detail (or (json-value parsed :any "error" "message")
                           (if (stringp response)
                               response
                               (and response (sb-ext:octets-to-string response :external-format :utf-8))))))
      ;; Object or array: list routes (/sessions)
      ;; answer a bare JSON array in the Zig lexicon. A decoded string is a
      ;; lisp vector too — exclude it, it is not a JSON collection.
      (unless (or (json-value parsed :object) (json-value parsed :array))
        (error 'gateway-http-error :status status :code "invalid_response"
               :detail "Gateway returned a non-object response"))
      parsed)))

(defun answer-text (answer)
  "What an organism's control op ANSWER says came of it, in words."
  (or (json-value answer :string "text") "the organism answered nothing"))

(defun ensure-gateway-session (sid port token)
  "Return SID when it exists; create exactly SID only on structured not-found."
  (handler-case
      (prog1 sid
        (gateway-http-session-request port :get
                                      (format nil "/api/gateway/sessions/~a" sid)
                                      nil token))
    (gateway-http-error (lookup-error)
      (unless (and (= 404 (gateway-http-error-status lookup-error))
                   (equal "unknown_session" (gateway-http-error-code lookup-error)))
        (error lookup-error))
      (unless (equal sid (gethash "session_id"
                                  (gateway-http-session-request
                                   port :post "/api/gateway/sessions"
                                   ;; The shell's own folder: a gateway in
                                   ;; another process resolves an unstated one
                                   ;; against its own.
                                   (make-json-object "session_id" sid
                                                     "title" "Lisp Gateway client"
                                                     "cwd" (workspace-directory))
                                   token)))
        (error 'gateway-http-error :status 200 :code "session_identity_mismatch"
               :detail "Gateway created a different session id"))
      sid)))


(nlt:defmessage gw-connection-msg
    ((ws :initarg :ws :reader gw-connection-msg-ws))
  :documentation "A fresh connected websocket installed by the UI thread.")

;;; The sync self-heal ladder (the src/tui/sync_client.zig SyncSelfHealPolicy
;;; + SyncReconnectConfig port): quiet retries first, an escalated
;;; :reconnecting verdict with the attempt count next, endpoint rediscovery
;;; through the store's holder sidecar after that, and a user-owed :failed
;;; verdict once the outage outlives the deadline — while the loop keeps
;;; healing (failed is a verdict, never a stop).

;;; The loop keeps healing past this point.
(defparameter *sync-failure-after-seconds* 30
  "Outage duration after which the lifecycle reports :failed.")

(defparameter *sync-backoff-base-seconds* 0.25
  "First retry delay (sync backoff_base_ms 250).")

;;;
;;; The gateway's acceptor carries a socket read timeout
;;; (*gateway-connection-read-timeout*, gateway.lisp), and before this ping
;;; existed the 20s Hunchentoot default closed an idle connection every cycle:
;;; close 1000, reconnect, cursorless full-session replay, ~16MB consed per lap.
;;; A ping inside the timeout window is the read activity that keeps the
;;; connection open, so this must stay well under the server-side timeout. The
;;; reconnector thread sends it from its lifetime park — no extra thread, which
;;; the fork-before-threads boot ordering requires.
(defparameter *sync-keepalive-seconds* 5.0
  "Idle interval between client WS pings.")

(defun rediscover-gateway-endpoint (store-path current-port)
  "The port a live gateway publishes in STORE-PATH's holder sidecar, when
it differs from CURRENT-PORT; NIL otherwise."
  ;; The sidecar {pid, port} is the same endpoint authority
  ;; ATTACH-OR-OWN-ORGANISM boots from — a gateway that restarted onto another
  ;; port republishes there, and no amount of retrying a dead port recovers
  ;; from that.
  (let ((live (live-published-endpoint store-path)))
    (and live (/= live current-port) live)))

(defun live-published-endpoint (store-path)
  "The holder sidecar's port, from a LIVE publisher only — the pid it
names is this process, or another process holds the store's fcntl lock —
else NIL."
  ;; The lock is the same ownership authority STORE-DEATH-VERDICT trusts and,
  ;; unlike a kill(2)-0 probe, immune to pid recycling and zombies. A dead
  ;; owner's stale sidecar must never seed a dial: without this, a shell could
  ;; park on a port nobody serves. This never errors; the paint-first ladder
  ;; polls it.
  ;;
  ;; A lock vouches for its holder alone, and the sidecar names that holder
  ;; only once it rewrites it: OPEN-STORE takes the lock a beat before it
  ;; replaces a dead owner's sidecar, and a shell polling in that beat would
  ;; dial the dead port and log failed reconnect attempts. A lock this process
  ;; holds vouches for no other pid, and one another process holds vouches only
  ;; for a pid that is alive.
  (multiple-value-bind (pid port)
      (ignore-errors (nlk::read-store-holder store-path))
    (and (integerp port)
         (integerp pid)
         (or (= pid (sb-posix:getpid))
             (and (not (nlk::store-lease-mine-p store-path))
                  (pid-live-p pid)
                  (ignore-errors (nlk::store-lease-held-p store-path))))
         port)))

;;; --- gateway resurrection ------------------------------------------------
;;; When the owning gateway dies dirty (lock freed by the kernel, holder
;;; sidecar left advertising a dead pid), a surviving client spawns a fresh
;;; DETACHED headless owner rather than promoting itself: a TUI client's
;;; image carries UI threads and half a session, and the owner is a separate
;;; process by design. Contention needs no new mechanism — every
;;; client may spawn a candidate, every candidate races for the store lease
;;; inside OPEN-STORE, exactly one wins and publishes its port in the holder
;;; sidecar, losers exit quietly, and the ladder's periodic rediscovery
;;; attaches everyone to whoever won.

;;; One attempt is enough: the verdict itself is the confirmation. A dirty
;;; death cannot be a transient blip — the kernel freed the lock, so the owner
;;; process is gone — and it cannot be a clean-quit race either, because
;;; orderly teardown releases the lock only AFTER deleting the sidecar
;;; (RELEASE-STORE-LEASE), making lock-free-with-sidecar unobservable there.
(defparameter *sync-resurrect-after-attempts* 1
  "Failed attempts before a dirty death licenses spawning a candidate.")

;;; Zero: a redundant candidate is harmless by construction — the fcntl
;;; election lets exactly one boot and the losers exit in milliseconds off the
;;; early lease probe — so waiting to give a supervisor respawn or another
;;; client's candidate a head start only adds dead time to every recovery.
;;; Raise this if a launcher supervisor ever owns primary respawn duty.
(defparameter *sync-resurrect-grace-seconds* 0.0
  "Outage age before the first spawn.")

(defparameter *sync-resurrect-rearm-seconds* 30.0
  "Minimum spacing between spawns: a booting candidate loads its folder and
recovers its store, and a second candidate mid-boot is pure election noise.")

;;; A store whose every resurrected owner dies at boot must not become a spawn
;;; loop; past the budget the ladder keeps retrying and reporting, but stops
;;; launching processes.
(defparameter *sync-resurrect-budget* 3
  "Spawns per outage.")

(defun resurrection-due-p (verdict &key failures elapsed attempts
                                   last-spawn-elapsed)
  "True when the reconnect ladder should spawn a resurrection candidate:
the owner died dirty, the outage is old enough that a supervisor respawn
would already have won, the spawn budget has room, and the previous
candidate had its boot window."
  (and (eq verdict :dirty-death)
       (>= failures *sync-resurrect-after-attempts*)
       (>= elapsed *sync-resurrect-grace-seconds*)
       (< attempts *sync-resurrect-budget*)
       (or (null last-spawn-elapsed)
           (>= last-spawn-elapsed *sync-resurrect-rearm-seconds*))))

(defparameter *serve-program* nil
  "Explicit argv prefix for starting a headless serve, or NIL to derive it
from the running executable. Tests inject a stub here.")

(defun serve-command (store-path)
  "The argv that runs STORE-PATH's organism as a headless serve -- the one a
shell starts and fronts (FRONT-ORGANISM), or a candidate that resurrects a
dead owner -- or NIL when this image cannot name a dumped executable to exec."
  ;; An interactive dev image runs under a bare sbcl runtime that cannot
  ;; replay this organism from the command line, so a serve is a
  ;; dumped-binary process; the dumped executable IS the runtime, so its own
  ;; path is the program. The home rides the environment (NLK:PUBLISH-HOME).
  (when-let (program (or *serve-program*
                         (let ((runtime (namestring (nlk::origin-pathname))))
                           (when (search "nodecode" (file-namestring runtime))
                             (list runtime)))))
    (append program (list "serve" "--store" (namestring store-path)))))

(defun gateway-supervises-p (store-path)
  "Whether this home's gateway is on and STORE-PATH is the store it serves:
then systemd, not a client, brings a dead owner back, and a clean stop is
a restart the gateway comes back from — not an organism left for a shell
to take. One file read (NLK:GATEWAY-INSTALLED-P); no process spawned."
  (and (equal (namestring (merge-pathnames store-path))
              (namestring (merge-pathnames nlk::*default-store-path*)))
       (nlk:gateway-installed-p)))

(defun gateway-returning-p (state)
  "Whether STATE's gateway serves, or is on its way back from an exit with
no fault in it: running, starting, stopping, or waiting out the restart its
unit schedules after a clean exit. A crash loop's restart is not one."
  ;; GATEWAY-STATE folds systemd's `success' and launchd's exit 0 into a NIL
  ;; RESULT. An update's restart (EXIT-FOR-RESTART) is such an exit: the unit
  ;; stops (deactivating), waits out its restart delay (auto-restart) and
  ;; starts again -- on a test box, 2026-10-21, eleven seconds without an owner.
  (and (null (nlk:gateway-state-result state))
       (or (member (nlk:gateway-state-active state) '("active" "activating" "deactivating")
                   :test #'equal)
           (equal (nlk:gateway-state-sub state) "auto-restart"))
       t))

(defparameter *gateway-return-seconds* 60
  "How long a shell leaves a returning gateway to take the store before it
starts an organism of its own.")

(defun await-gateway-store (path &key (state #'nlk:gateway-state)
                                      (held-p #'nlk::store-lease-held-p)
                                      (seconds *gateway-return-seconds*) (pause 0.1))
  "Wait while the gateway that supervises PATH is on its way to it: => T
once a process holds PATH, NIL at once where no gateway supervises it or none
is coming back, and NIL once SECONDS pass first."
  ;; What a shell asks before it starts an organism (ATTACH-OR-OWN-ORGANISM's
  ;; boot, which the reconnect ladder's takeover rungs run again). A shell that
  ;; started one in the unit's restart gap took the store from its own binary,
  ;; and the unit's gateway waited behind it for as long as that shell stayed
  ;; open (AWAIT-FREE-STORE): a test box, 2026-10-21, an update's restart, every open
  ;; shell starting an older organism, the new gateway idle for 15 minutes.
  ;; The unit is asked about once a second; the lock, every PAUSE.
  (when (gateway-supervises-p path)
    (loop with deadline = (deadline-after seconds)
          with asked = nil
          when (funcall held-p path) return t
          when (or (null asked) (>= (since-ms asked) 1000))
            do (setf asked (get-internal-real-time))
               (unless (gateway-returning-p (funcall state)) (return nil))
          when (>= (get-internal-real-time) deadline) return nil
          do (sleep pause))))

(defun spawn-resurrection-candidate (store-path)
  "Launch one detached headless serve candidate for STORE-PATH — one
NLK::SPAWN-DETACHED of the resurrection command, logged beside the store —
or, where the gateway supervises that store, ask systemd to start it: the
organism comes back under the unit `gateway off' stops, never beside it."
  ;; Values: the argv on launch, and :SYSTEMD after the gateway's start; NIL
  ;; plus a reason string when nothing was launched. Candidates are disposable
  ;; by design — the fcntl lease elects, losers exit 0 — so spawning is safe
  ;; to race against other clients and against a supervisor respawn.
  (if (gateway-supervises-p store-path)
      (if (nlk:gateway-start)
          (values (nlk:gateway-start-argv) :systemd)
          (values nil "the service manager would not start the gateway"))
      (let ((command (serve-command store-path)))
        (if (null command)
            (values nil "this image has no serve binary to exec")
            ;; The candidate's log lands next to the database it fought for.
            (nlk::spawn-detached
             command
             (pathname (format nil "~a.resurrect.log" (namestring store-path))))))))

;;; --- the organism a shell fronts -------------------------------------------
;;; A shell never runs the organism in its own process: a collection the
;;; organism's work causes stops every thread of the process it runs in, and a
;;; shell sharing it would freeze with it. The shell that finds the store free
;;; starts the organism as a headless serve in a process of its own and
;;; attaches over the socket, as every other shell does. Its stdin is a pipe
;;; from that shell, which is how it lives and dies with the shell, as it did
;;; inside it: a shell that quits writes why and lets go, and one that crashes
;;; or loses its terminal ends the pipe all the same.

(defparameter +front-variable+ "NODECODE_FRONT"
  "The environment word on an organism a shell started (FRONT-ORGANISM): the
shell's pid, and the sign that its stdin is that shell's pipe (WATCH-FRONT).")

(defvar *fronted* nil
  "True in an organism a shell started and holds by its stdin.")

(defun watch-front (&optional (pipe sb-sys:*stdin*))
  "When a shell started this headless owner (+FRONT-VARIABLE+), stop it once
that shell lets go of PIPE, its stdin, the line it wrote first, if any, kept
as the stop's reason for the next boot's note."
  ;; The word stays in the environment and the pipe on descriptor 0, so a
  ;; restart's exec (SERVE-NEXT :RESTART) still fronts for the same shell.
  (when (uiop:getenv +front-variable+)
    (setf *fronted* t)
    (spawn "front-watch"
      (let ((why (ignore-errors (read-line pipe nil nil))))
        (when (plusp (length why)) (setf *stop-reason* why))
        (serve-next :stop)))))

(define-error organism-exited (error) (status line)
  (:report "~:[the organism exited ~a before it served~;~:*~a~]" line status)
  (:documentation "The organism a shell started exited before it served, LINE
the last thing it said on its log, when it said anything."))

(defun log-line-after (path from)
  "The last line PATH's file holds past byte FROM, its `nodecode: ' lead off,
or NIL."
  (ignore-errors
   (with-open-file (in path :external-format :utf-8)
     (when (> (file-length in) from)
       (file-position in from)
       (let ((line (car (last (remove "" (mapcar (lambda (line) (string-trim '(#\Space #\Return) line))
                                                 (uiop:slurp-stream-lines in))
                                      :test #'string=)))))
         (if (and line (uiop:string-prefix-p "nodecode: " line))
             (subseq line (length "nodecode: "))
             line))))))

(defvar *front* nil
  "The organism this shell started, while it may run: (:PIPE STREAM :WAITER
THREAD), the pipe on its stdin and the thread that reaps it.")

(defun front-organism (store-path)
  "Start STORE-PATH's organism as a headless serve in a process of its own,
fronted by this shell, and return once it serves -- its gateway published,
which this shell's reconnector attaches to -- or once another process holds
the store, an election it lost. One that exits before either is a boot that
died: ORGANISM-EXITED, with the last line it wrote to its log."
  (let* ((command (or (serve-command store-path)
                      (error "this image names no nodecode binary to run the organism with")))
         (log (stdio-log-path store-path))
         (from (or (nlk:file-bytes log) 0))
         (word (format nil "~a=" +front-variable+))
         (exited (list nil)))
    (bind (((pid _ _ process pipe)
            (spawn-program (session-argv command)
                           :environment (cons (format nil "~a~d" word (sb-posix:getpid))
                                              (remove-if (lambda (entry) (uiop:string-prefix-p word entry))
                                                         (sb-ext:posix-environ)))
                           :output log :error-output :output :input :stream)))
      (nlk:note-boot-phase "organism-spawned" pid)
      (setf *front* (list :pipe pipe
                          :waiter (spawn "organism-front"
                                    (setf (car exited) (wait-for-child pid process)))))
      (loop until (or (live-published-endpoint store-path) (car exited))
            do (sleep 0.005))
      (when (car exited)
        (setf *front* nil)
        (ignore-errors (close pipe))
        (unless (nlk::store-lease-held-p store-path)
          (error 'organism-exited :status (car exited) :line (log-line-after log from)))))))

(defun stop-front (&key why (wait t))
  "Let go of the organism this shell fronts: WHY, when given, is the reason it
keeps for the next boot's note (`quit'), and with WAIT the shell waits for it
to let the store go -- bounded, so a wedged organism is left to end on its own."
  ;; The wait is what lets the next launch find the store free instead of
  ;; attaching to an organism on its way out.
  (when-let (front (shiftf *front* nil))
    (let ((pipe (getf front :pipe)))
      (ignore-errors (when why (write-line why pipe)) (finish-output pipe))
      (ignore-errors (close pipe)))
    (when wait
      (nlk::%join-bounded (getf front :waiter) 10))))

(defvar *organism-notes-shown* '()
  "The organism.note texts this shell has put in its transcript: the
organism's last note comes again with every attach while it is fresh.")

(defvar *organism-build-noted* nil
  "The other build this shell last said it attached to, so a reconnect to
the same organism says nothing new.")

(defun note-organism-build (program payload)
  "Say once, when the organism this shell attached to runs another build
than the shell — an update landed and the gateway still runs the build it
started with — which the two are, and how the gateway moves over."
  ;; PAYLOAD is the connect ack's. An ack carrying NO version is itself the
  ;; answer rather than nothing to say: the field arrives with the gateway
  ;; that knows to send it, so an organism that sends none predates that build
  ;; and is certainly older than this shell. Without this, a freshly installed
  ;; binary attached to a pre-upgrade organism would show no sign of the
  ;; mismatch.
  (let* ((version (and (hash-table-p payload) (gethash "version" payload)))
         (build (if (stringp version) version :older)))
    (unless (member build (list (effective-version) *organism-build-noted*) :test #'equal)
      (setf *organism-build-noted* build)
      (post-shell-notice
       program
       (format nil "the organism this shell attached to runs ~:[build ~a; this shell is ~a~;a ~
                    build older than this shell (~*~a) — it does not report its own~]~
                    ~:[~; · `nodecode gateway restart' moves the gateway to this build~]"
               (eq build :older) version (effective-version) (nlk:gateway-installed-p))
       ;; It names something to do, so it stays until it is closed.
       :error-p nil :stays t))))

(defun post-shell-notice (prog text &key (error-p t) stays answer)
  "Say TEXT on PROG's shell from any thread (SURFACE-SYSTEM-NOTICE: a toast,
or with ANSWER the composer status), waiting briefly for the
run loop to start accepting — NLT:SEND drops messages while the program is
not running, and a boot thread's failure can beat the first frame by
milliseconds."
  ;; Falls back to *ERROR-OUTPUT* (the severed-stdio log) when the shell never
  ;; comes up.
  (loop repeat 50 until (or *system-notice-hook* (and prog prog.running)) do (sleep 0.1))
  (if (or *system-notice-hook* (and prog prog.running))
      (let ((*active-tui-program* prog))
        (surface-system-notice text :error-p error-p :stays stays :answer answer))
      (format *error-output* "~&~a~%" text)))

;;; --- the home, asked once before anything boots -----------------------------
;;; Without HOME-REFUSAL, a launch over a home it cannot use would paint its
;;; shell, open the wizard over a dead organism, fail on a read-only mount or
;;; unparseable config.jsonc at every step, and end on a summary of what it
;;; had not saved. HOME-REFUSAL is the one question, answered from disk, that
;;; MAIN asks before a shell or an organism exists: the launch that gets a
;;; sentence back prints it and exits.

(defun plain-error-text (condition)
  "CONDITION as an operator reads it: the system's own words for a failed
system call (`read-only file system'), a store it cannot read named from
`~', else the condition's report. The second value is T for the first two,
words written for the operator, and NIL for a report, which may be Lisp's."
  (typecase condition
    (sb-posix:syscall-error
     (values (format nil "~(~a~)" (sb-int:strerror (sb-posix:syscall-errno condition))) t))
    ;; The store's sentence is the kernel's; only its path is written the way
    ;; the operator writes their own home.
    (nlk::store-unreadable
     (values (princ-to-string (make-condition 'nlk::store-unreadable
                                              :path (home-abbreviated (nlk::store-unreadable-path condition))
                                              :reason (nlk::store-unreadable-reason condition)))
             t))
    ;; The organism's own line, when its log ended on one.
    (organism-exited (values (princ-to-string condition) (and (organism-exited-line condition) t)))
    (t (values (princ-to-string condition) nil))))

(defmacro errno-of (&body body)
  "The errno the system call in BODY fails with, else NIL."
  `(with-handlers ((sb-posix:syscall-error (e) (sb-posix:syscall-errno e))) ,@body nil))

(defun folder-write-errno (folder)
  "NIL when a file can be made in FOLDER, else the errno that says why not."
  ;; A file made and removed, the one test a read-only mount, a permission and
  ;; a full disk all fail alike; an access(2) call would not see the last.
  (let ((probe (uiop:native-namestring
                (merge-pathnames (format nil ".nodecode-write-probe-~d" (sb-posix:getpid))
                                 folder))))
    (errno-of (sb-posix:close (sb-posix:open probe (logior sb-posix:o-creat sb-posix:o-excl
                                                           sb-posix:o-wronly)
                                             #o600))
              (ignore-errors (sb-posix:unlink probe)))))

(defun unwritable-words (errno)
  "What ERRNO, a refused write, says about the place: `read-only', `not
writable by this account', else `not writable (the system's words)'."
  (case errno
    ;; EROFS, then EPERM and EACCES: the numbers Linux, macOS and the Windows C
    ;; runtime agree on, which sb-posix names only where it has them.
    (30 "read-only")
    ((1 13) "not writable by this account")
    (t (format nil "not writable (~(~a~))" (sb-int:strerror errno)))))

(defun folder-refusal (folder hint)
  "A sentence saying why nodecode cannot write in FOLDER -- a file or a link
to nothing stands where it belongs, or the folder refuses the write -- ending
in what to do and HINT, or NIL when it can."
  ;; A folder that is not there yet is judged by the nearest thing on its path
  ;; that is: the folder it would be made in, or the file or the link to
  ;; nothing that no mkdir gets past (`Can't create directory').
  (let* ((folder (uiop:ensure-directory-pathname folder))
         (holder folder))
    (flet ((native (path) (string-right-trim "/\\" (uiop:native-namestring path))))
      (loop until (or (uiop:directory-exists-p holder)
                      (member (nlk::lstat-kind (native holder)) '(:file :link :other)))
            do (setf holder (uiop:pathname-parent-directory-pathname holder)))
      (let ((it (if (eq holder folder) "it" (home-abbreviated (native holder))))
            (target (nlk::symlink-target holder))
            (unfollowed (errno-of (sb-posix:stat (native holder)))))
        (flet ((sentence (why fix)
                 (if (eq holder folder)
                     (format nil "~a ~a; ~a ~a" (home-abbreviated (native folder)) why fix hint)
                     (format nil "~a does not exist and ~a ~a, so nodecode cannot make it; ~a ~a"
                             (home-abbreviated (native folder)) it why fix hint))))
          (cond ((uiop:directory-exists-p holder)
                 (when-let (errno (folder-write-errno holder))
                   (sentence (format nil "is ~a" (unwritable-words errno))
                             (if (eq holder folder)
                                 "nodecode needs to write there, so make it writable"
                                 (format nil "make ~a writable" it)))))
                ;; A link stat cannot follow (lstat answers the link, stat its
                ;; target): gone is ENOENT or ENOTDIR; a loop or a folder this
                ;; account cannot enter is there, and the system says which.
                ((and target unfollowed)
                 (if (member unfollowed (list sb-posix:enoent sb-posix:enotdir))
                     (sentence (format nil "links to ~a, which does not exist" (home-abbreviated target))
                               "create that folder or remove the link,")
                     (sentence (format nil "links to ~a, which nodecode cannot reach (~(~a~))"
                                       (home-abbreviated target) (sb-int:strerror unfollowed))
                               "fix the link or remove it,")))
                (t (sentence "is a file, not a folder" (format nil "move ~a aside" it)))))))))

(defun sqlite-file-p (path)
  "Whether the file at PATH is empty -- a store SQLite makes on its first open
-- or begins with the header every SQLite database begins with."
  ;; Anything else is what a crash or a full disk leaves, or no store at all:
  ;; SQLite refuses it (NOTADB) at the boot's first statement.
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((head (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
      (or (zerop (read-sequence head in))
          (equalp head (sb-ext:string-to-octets (format nil "SQLite format 3~c" (code-char 0))))))))

(defun store-refusal (store-path)
  "A sentence saying why the launch cannot use its store -- its path names no
one file, its folder cannot be written, or what is there is a folder, a pipe
or a device, refuses the write or is no SQLite database -- or NIL when it can.
STORE-PATH is the store the launch named, NIL for the home's own."
  (let* ((path (ignore-errors (merge-pathnames (or store-path nlk::*default-store-path*))))
         ;; SBCL reads *, ? and [ in a path as a pattern: no one file and no
         ;; native name, and the boot's first open died on it.
         (native (and path (not (wild-pathname-p path)) (uiop:native-namestring path)))
         (shown (and native (home-abbreviated native))))
    (or (unless native
          (format nil "--store ~a: nodecode cannot use a store path with *, ? or [ in it; name one ~
                       without them"
                  (home-abbreviated (princ-to-string store-path))))
        (folder-refusal (uiop:pathname-directory-pathname path)
                        "or name a store elsewhere with --store PATH")
        (cond ((uiop:directory-exists-p path)
               (if store-path
                   (format nil "~a is a folder, not a store file; name a file, such as --store ~a/history.db"
                           shown shown)
                   (format nil "~a is a folder, not a store file; move it aside and run nodecode again" shown)))
              ;; Not there yet, or a link to a file not there yet: the boot
              ;; makes it. Stat follows the link; PROBE-FILE answers the link.
              ((not (ignore-errors (sb-posix:stat native))) nil)
              ;; A pipe or a device, which the header's read below would wait
              ;; on for ever (`--store /dev/tty'): nothing is opened.
              ((not (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:stat native))))
               (format nil "~a is a device or a pipe, not a store file; ~:[move it aside and run nodecode ~
                            again~;name a file with --store PATH~]"
                       shown store-path))
              ;; Opened for reading and writing and closed at once, the way the
              ;; boot opens it: a 0444 file opened, then failed its first write.
              (t (or (when-let (errno (errno-of
                                       (sb-posix:close (sb-posix:open native sb-posix:o-rdwr))))
                       (format nil "~a is ~a; nodecode needs to write to it, so make it writable or name a ~
                                    store elsewhere with --store PATH"
                               shown (unwritable-words errno)))
                     ;; The words a boot says of what got past this check.
                     (unless (sqlite-file-p path)
                       (plain-error-text (make-condition 'nlk::store-unreadable :path native)))))))))

(defun shared-config-blank-p ()
  "Whether the shared config holds nothing but whitespace and comments."
  ;; The writer starts such a document over (NLK:SET-JSONC-PATH), so it is a
  ;; config to write; the reader's verdict on it is `the text ends where a
  ;; value belongs'.
  (ignore-errors
   (every #'nlk::jsonc-space-p
          (nlk::normalize-jsonc (uiop:read-file-string *shared-config-path*)))))

(defun shared-config-refusal ()
  "A phrase saying the shared config is there and cannot be read --
`~/.nodecode/config.jsonc is not valid JSONC (byte 77)' -- or NIL when it
reads, is absent, or is blank."
  (multiple-value-bind (config text) (read-shared-config)
    (when (and (null config) text (not (shared-config-blank-p)))
      (format nil "~a ~a" (home-abbreviated (uiop:native-namestring *shared-config-path*))
              (if-let (at (ppcre:register-groups-bind (at) ("at byte (\\d+)" text) at))
                (format nil "is not valid JSONC (byte ~a)" at)
                (format nil "cannot be read (~a)" text))))))

(defun home-refusal (store-path)
  "One plain sentence saying why this launch cannot use its home or its store
-- the home is read-only, a file or a link to nothing, config.jsonc does not
parse, the store is a folder, read-only or no SQLite database -- or NIL when
it can. STORE-PATH is the store the launch named, NIL for the home's own."
  ;; Answered from disk, before the shell paints or the organism boots, for
  ;; every launch that runs one: the shell, `setup', `import', `serve' and the
  ;; gateway's unit. `--ephemeral' takes it too: its store is in the temp
  ;; folder, but the operator token, the cell links and the config are the
  ;; home's, and a boot over a read-only home died on all of them.
  (or (folder-refusal (nlk:home) "or set NODECODE_HOME to another folder")
      (when-let (phrase (shared-config-refusal))
        (format nil "~a; fix it or move it aside, then run nodecode setup" phrase))
      (store-refusal store-path)))

(defvar *organism-boot-failure* nil
  "The line said of the organism this process's shell fronts when it died at
boot, or NIL while none has: what the launch exits on, and what a wait on
that organism answers at once instead of waiting for a gateway nothing will
serve.")

(defun organism-boot-failure-line (condition store-path &optional (what "could not start"))
  "What a shell says when its organism's boot died of CONDITION, in one line:
WHAT failed -- by default that nodecode could not start -- and why, in the
words PLAIN-ERROR-TEXT has for it -- the store's own sentence, the system's
words for a refused call -- or, for a condition with none, where to read why:
STORE-PATH's stdio log, which this writes it to, or why that log could not be
written."
  ;; The condition's own text is Lisp's -- `The slot ASDF/COMPONENT:OPERATION-TIMES
  ;; is unbound in the object #<SYSTEM ...>' -- so only the log holds it. Words
  ;; written for the operator are the line itself, never a pointer to the log that
  ;; holds them: a damaged store's `move it aside' would sit in the log behind `see
  ;; ...stdio.log'. Nothing promises a restart mends a cause that is still there (a
  ;; corrupt store, a home that is a file).
  (multiple-value-bind (text plain) (plain-error-text condition)
    (let* ((log (stdio-log-path (or store-path nlk::*default-store-path*)))
           ;; NIL when the log took the reason, else the system's words for
           ;; why it did not, or T when there are none. sb-posix's open only
           ;; asks, since its refusal carries the system's words (`is a
           ;; directory') where OPEN's is Lisp's; the write is by name, as an
           ;; fd-stream on Windows wraps a HANDLE, never a C runtime descriptor
           ;; (scribe.lisp, WRITE-FILE-ATOMICALLY).
           (refused (with-handlers ((error (condition)
                                      (multiple-value-bind (words plain)
                                          (plain-error-text condition)
                                        (if plain words t))))
                      (sb-posix:close (sb-posix:open (uiop:native-namestring log)
                                                     (logior sb-posix:o-wronly sb-posix:o-creat
                                                             sb-posix:o-append)
                                                     #o600))
                      (with-open-file (out log :direction :output :if-exists :append
                                               :if-does-not-exist :create
                                               :external-format :utf-8)
                        (format out "~&organism boot failed: ~a~%" text))
                      nil))
           (shown (home-abbreviated (uiop:native-namestring log))))
      (cond (plain (format nil "~a: ~a" what text))
            ((null refused) (format nil "~a; the reason is in ~a" what shown))
            (t (format nil "~a, and its reason could not be written to ~a~@[ (~a)~]"
                       what shown (and (stringp refused) refused)))))))

(defun call-organism-boot (boot shell store-path)
  "Run BOOT — the organism the shell fronts — with SHELL, a function of no
arguments answering that shell's program; STORE-PATH is the store it opens,
NIL for the home's own."
  ;; The boot starts before the program is built (the store, the listener and
  ;; the in-process attach overlap the frame's construction), so the program
  ;; is asked for where it is needed, never held. A boot that dies keeps its
  ;; line (*ORGANISM-BOOT-FAILURE*), which the setup walk's wait on this
  ;; organism's gateway ends on at once. A shell that never attached is ended
  ;; on it by its own reconnector, through the quit its loop reads -- what
  ;; /exit sends, so the loop gives the terminal back, never this thread --
  ;; and the launch exits 1 on it, as a refused launch does
  ;; (ATTACH-OR-OWN-ORGANISM): without this, a shell kept up over a dead
  ;; organism would walk the setup over it and take a task it could not send.
  ;; An attached shell is taking over an organism whose owner let the store go
  ;; (the clean-shutdown rung): it served a session, so it stays, and is told.
  (setf *organism-boot-failure* nil)
  (handler-case (funcall boot shell)
    (error (condition)
      (let* ((prog (funcall shell))
             (attached (gateway-model-ws prog.model)))
        (setf *organism-boot-failure*
              (organism-boot-failure-line condition store-path
                                          (if attached
                                              "could not take the store over"
                                              "could not start")))
        (when attached
          (post-shell-notice prog *organism-boot-failure*))))))

(defvar *boot-trim-done* nil
  "Whether this process already gave the boot's pages back after its shell's
ready frame: the trim runs once per process, on the first attach.")

(defun trim-behind-the-ready-frame ()
  "Give the boot's pages back once the shell has painted the frame its
attach produces — once per process, on the first attach."
  ;; TRIM-MEMORY forces a collection and unmaps the image's own pages:
  ;; measured 32 MB given back on a fresh warm boot, 33–291 ms of
  ;; stop-the-world. Before it, the pause was the last thing standing between
  ;; the operator and a usable shell; behind the ready frame nobody is waiting
  ;; inside it, and the pages go back within a couple of hundred milliseconds
  ;; of the shell coming up.
  (unless (sb-ext:compare-and-swap (symbol-value '*boot-trim-done*) nil t)
    (let ((at nlt:*frames-on-screen*))
      (spawn "boot-trim"
        (nlt:wait-for-frames-past at)
        ;; The pause lands on a still screen, never inside a repaint: a
        ;; frame arriving in the window means the operator is working,
        ;; and the trim waits for the next quiet quarter-second.
        (loop for seen = nlt:*frames-on-screen*
              do (sleep 0.25)
              until (= seen nlt:*frames-on-screen*))
        (nlk:note-boot-phase "trim-start")
        (nlk:trim-memory)
        (nlk:note-boot-phase "trim-done")))))

(defun note-shell-attached (program model link &key build-payload)
  "The one post-attach step both transports take: the shell is on the
gateway."
  ;; Said to the UI thread, which owns the lifecycle — its :connected is
  ;; what the composer's ready line follows — with the build it attached to, the
  ;; session directory warmed behind that paint and the model's subscription
  ;; sent; the boot's pages go back, and the folder's peripherals start,
  ;; behind the frame that subscription's replay paints. The
  ;; socket path (a shell in another process) and the owner's in-process attach
  ;; (gateway.lisp ATTACH-LOOPBACK-GATEWAY-PEER) must not drift: this is the
  ;; whole of what attaching means.
  (nlk:note-boot-phase "connected")
  (note-organism-build program build-payload)
  ;; Read the session directory once, behind the paint: the /sessions
  ;; picker then has rows for the first keystroke instead of only the
  ;; reading copy. One in flight per shell — a reconnect the cache already
  ;; answered for spawns nothing.
  (when (and (null (cached-session-directory model))
             (not (eq (getf (gethash model *session-directory*) :at) :reading)))
    (refresh-session-directory model))
  (nlt:send program (make-instance 'gw-connection-msg :ws link))
  ;; Resubscribe the model's *current* session: a reconnect after /clear
  ;; or /sessions must not re-attach the boot session -- and the session
  ;; its preview lane follows. The subscribe is what proves the link:
  ;; its frames carry the replay that paints the transcript.
  (send-subscribe link (subscription-entries model))
  (note-shell-presence model.session-id))

(defun gateway-client (port token)
  "A WebSocket client of the gateway at PORT, TOKEN its bearer when there is one."
  (wsd:make-client (format nil "ws://127.0.0.1:~d/gateway" port)
                   :additional-headers
                   (and token (list (cons "authorization" (format nil "Bearer ~a" token))))))

(defun answer-gateway-challenge (ws frame mode token
                                 &optional (instance (format nil "sbcl-~d" (sb-posix:getpid))))
  "Answer the connect challenge FRAME on WS as this nodecode's MODE client
INSTANCE, TOKEN its operator token."
  (wsd:send ws (encode-gateway-connect (gethash "challenge_nonce" (nlk:sync-message-payload frame))
                                       "nodecode" *user-agent-version* mode (string-downcase (nlk:platform))
                                       instance :operator-token token)))

(defun run-chat-gateway (&key session-id port
                              (store-path nlk::*default-store-path*)
                              boot setup)
  "Run the Gateway TUI with a reconnecting reader and UI-owned state. => the
session the shell ended on, or NIL when it never attached."
  ;; The shell paints FIRST. Nothing in the first frame needs a gateway — the
  ;; session id is client-chosen, the band is in-memory, the composer
  ;; placeholder is the :connecting lifecycle — so endpoint resolution and the
  ;; session ensure live on the reconnector thread, not in this prologue: PORT
  ;; NIL means the ladder polls STORE-PATH's holder sidecar (the one endpoint
  ;; authority for kernel-assigned ports) until an owner publishes, then
  ;; ensures the session and connects. BOOT, when given, is the organism this
  ;; shell fronts — a function called with the program, so its notices can
  ;; wait on the shell going live: it runs on its own organism-boot thread the
  ;; moment the program exists — the entry ladder starts the organism in a
  ;; process of its own through it (FRONT-ORGANISM) — its death ends a shell
  ;; that never attached (CALL-ORGANISM-BOOT), and the unwind lets go of the
  ;; organism it started (STOP-FRONT).
  ;; STORE-PATH also names the store whose sidecar the ladder re-resolves
  ;; after a sustained outage.
  (engage-theme)
  ;; Programs outside the organism read which session this shell is on
  ;; from its presence record (NOTE-SHELL-PRESENCE).
  (open-shell-presence)
  (let* ((pinned (and port t))
         (port (or port (live-published-endpoint store-path)))
         (token (gateway-configured-operator-token))
         (sid (or session-id (fresh-session-id)))
         (model (make-instance 'gateway-chat-model :session-id sid :ws nil
                                                   :http-port port
                                                   :http-token token
                                                   :setup-requested setup))
         (program nil)
         (stop nil)
         ;; The organism this shell took over after its owner let the store
         ;; go (the clean-shutdown rung), or NIL: joined on the way out like
         ;; the first boot.
         (takeover nil)
         (ingress-slots (bt2:make-semaphore :count 256
                                            :name "gateway-client-ingress"))
         ;; One inbound frame into the fold, bounded by the ingress slots: the
         ;; socket's projection arm.
         (release (lambda () (bt2:signal-semaphore ingress-slots)))
         (ingest (lambda (msg)
                   (bt2:wait-on-semaphore ingress-slots)
                   (dispatch-inbound-frame program model msg :release release)))
         ;; The shell program, answered to the boot's rare notices once it
         ;; exists.
         (shell (lambda ()
                  (loop until program do (sleep 0.001))
                  program))
         (start-organism (lambda ()
                           (spawn "organism-boot" (call-organism-boot boot shell store-path))))
         ;; And the organism this shell fronts boots NOW, on its own thread:
         ;; the store, the listener and the in-process attach overlap the
         ;; shell program's construction instead of following it. Nothing in
         ;; the boot before this shell's first frame needs the program.
         (boot-thread (and boot (funcall start-organism))))
    (let ((prog (nlt:make-program model)))
      (nlk:note-boot-phase "shell-program")
      (setf model.program prog program prog)
      (spawn "gateway-client-reconnector"
        (loop with delay = *sync-backoff-base-seconds*
              with failures = 0
              with outage-started = nil
              with failure-reported = nil
              with resurrect-attempts = 0
              with last-resurrect = nil
              with shutdown-reported = nil
              with stopping-reported = nil
              with stopping-since = nil
              ;; The session ensure of the lap in flight: (DONE . OUTCOME),
              ;; OUTCOME :ENSURED, the condition it failed with, or NIL
              ;; while it runs.
              with ensure = nil
              with ensure-reported = nil
              with unresolved-since = nil
              with unresolved-reported = nil
              ;; An attach ends the outage: the next one walks the ladder from its start.
              with reset-outage = (lambda ()
                                    (setf delay *sync-backoff-base-seconds* failures 0
                                          outage-started nil failure-reported nil
                                          resurrect-attempts 0 last-resurrect nil
                                          shutdown-reported nil stopping-reported nil
                                          stopping-since nil))
              ;; A spawn the ladder may make now: counted and stamped, => true.
              with resurrect = (lambda (failures elapsed)
                                 (when (resurrection-due-p
                                        :dirty-death :failures failures :elapsed elapsed
                                        :attempts resurrect-attempts :last-spawn-elapsed
                                        (and last-resurrect (/ (since-ms last-resurrect) 1000)))
                                   (incf resurrect-attempts)
                                   (setf last-resurrect (get-internal-real-time))))
              until stop
              do (block lap
                   ;; The organism this shell fronts died at boot before the
                   ;; shell ever attached: it ends on the boot's line
                   ;; (CALL-ORGANISM-BOOT). SEND drops the quit until the loop
                   ;; runs, so it goes again each lap until one lands.
                   (when (and boot *organism-boot-failure* (null model.ws))
                     (nlt:send program (nlt:make-quit-msg))
                     (sleep 0.025)
                     (return-from lap))
                   ;; Paint-first: the endpoint resolves HERE. The shell is
                   ;; already on screen; the owner booting behind it (or a
                   ;; sibling process) publishes the sidecar a beat later,
                   ;; and until then there is nothing to dial — a quiet
                   ;; fast poll, no failure accounting, no notices.
                   (unless port
                     (let ((published (live-published-endpoint store-path)))
                       (unless published
                         ;; Nobody holds the store: a shell that starts
                         ;; organisms starts one now, the moment the one
                         ;; before let go -- not after a dial against its
                         ;; dead port failed a backoff later
                         ;; (FRONT-ORGANISM). One start at a time, each
                         ;; spending the outage's budget as a resurrection
                         ;; does. One that holds it while it stops is
                         ;; waited out, and said once.
                         (when (and boot (not pinned)
                                    (not (and boot-thread (bt2:thread-alive-p boot-thread)))
                                    (not (and takeover (bt2:thread-alive-p takeover))))
                           (cond ((ignore-errors (nlk::store-lease-held-p store-path))
                                  ;; Said only past a second and a half: a stop
                                  ;; takes under one, and a notice for it only
                                  ;; flickered.
                                  (setf stopping-since (or stopping-since (get-internal-real-time)))
                                  (when (and (not stopping-reported)
                                             (> (since-ms stopping-since) 1500))
                                    (setf stopping-reported t)
                                    (post-shell-notice
                                     program
                                     "the nodecode before this one is still stopping; this shell starts its own once it lets go"
                                     :error-p nil)))
                                 ((funcall resurrect 1 0)
                                  (setf takeover (funcall start-organism)))))
                         ;; Quiet fast poll — but never SILENT forever:
                         ;; past the threshold (a shell fronting its own
                         ;; boot gets the outage window; a bare attach
                         ;; shell, which nothing in-process will ever
                         ;; publish for, gets seconds) say once what is
                         ;; being waited on, and keep watching. A boot
                         ;; that died has said so in its own line, and
                         ;; this one would only ask who serves.
                         (setf unresolved-since (or unresolved-since (get-internal-real-time)))
                         (when (and (not unresolved-reported)
                                    (not (and boot *organism-boot-failure*))
                                    (> (since-ms unresolved-since)
                                       (* 1000
                                          (if boot
                                              *sync-failure-after-seconds*
                                              5))))
                           (setf unresolved-reported t)
                           (report-transport-error
                            program :failed "store ~a advertises no gateway ~
                                          endpoint; still watching its ~
                                          sidecar — is an owner serving?"
                            (namestring store-path)))
                         ;; The organism publishes from a process of its
                         ;; own, which signals nothing here: a short lap
                         ;; dials within moments of the port landing.
                         (sleep 0.005)
                         (return-from lap))
                       (nlk:note-boot-phase "endpoint-seen" published)
                       (setf unresolved-since nil
                             unresolved-reported nil
                             port published
                             token (or token
                                       (gateway-configured-operator-token))
                             model.http-port port
                             model.http-token token)))
                   ;; The session ensure runs BESIDE the connect, not
                   ;; before it. Its only consumer is the subscribe that
                   ;; follows the ack, and the ack handler waits for it
                   ;; there — while the dial, the upgrade and the
                   ;; challenge, which need nothing from the session, go
                   ;; out at once.
                   ;;
                   ;; Every lap ensures, against the owner its connection
                   ;; reaches: a session not yet spoken in stands by in its
                   ;; owner's memory (standby.lisp), so an owner that dies
                   ;; takes it along and the one that replaces it never heard
                   ;; of it. Ensured once per shell; without that, a shell
                   ;; whose owner dies under its first prompt would subscribe
                   ;; the replacement to a session it does not hold: refused,
                   ;; no replay, and the prompt that waits on the replay to
                   ;; be re-sent would sit at `waiting' for good. An owner
                   ;; that holds the session answers one GET. Each lap's
                   ;; outcome is its own place, so an ensure still in flight
                   ;; against a dead owner never answers for the next one.
                   (let ((attempt (list (bt2:make-semaphore :name "shell-session-ensure")))
                         (port port))
                     (setf ensure attempt)
                     (spawn "shell-session-ensure"
                       (with-handlers ((error (condition)
                                         ;; The endpoint that refused may be stale or
                                         ;; gone: re-read the sidecar (unpinned ports
                                         ;; only) so the next lap ensures against
                                         ;; whoever actually serves.
                                         (setf (cdr attempt) condition)
                                         (unless pinned
                                           (setf model.http-port
                                                 (live-published-endpoint store-path)))))
                         (nlk:note-boot-phase "ensure-start")
                         (ensure-gateway-session sid port token)
                         (nlk:note-boot-phase "session-ensured")
                         (setf (cdr attempt) :ensured)
                         ;; The facts read goes out HERE, the moment
                         ;; the session it resolves against exists,
                         ;; not behind the connect ack: it runs
                         ;; beside the dial, the upgrade and the
                         ;; challenge instead of after all three,
                         ;; which puts the identity in the first
                         ;; frames
                         ;; The ack's own refresh stays as the
                         ;; reconciliation for a later connection
                         (refresh-gateway-model-facts model))
                       (bt2:signal-semaphore (car attempt))))
                   (let ((ws (gateway-client port token))
                       (handshake-complete nil)
                       ;; This lap's own ensure, which the ack waits on.
                       (attempt ensure)
                       (permanent nil)
                       ;; wsd client START-CONNECTION does not block for the
                       ;; connection's lifetime (its reader runs on a thread
                       ;; the driver owns); this semaphore is the lifetime.
                       (closed (bt2:make-semaphore :name "gateway-client-closed")))
                   (wsd:on :message ws
                           (lambda (msg)
                             (handler-case
                                 (let ((frame (decode-sync-message msg)))
                                   (case frame.type
                                     (:gateway-connect-challenge
                                      (nlk:note-boot-phase "ws-challenge")
                                      ;; The challenge completes before any command.
                                      (answer-gateway-challenge ws frame "tui" token))
                                     (:gateway-connect-ack
                                      ;; A successful attach ends the outage:
                                      ;; a long-lived client that drops later
                                      ;; gets the same ladder as one that
                                      ;; never attached.
                                      ;; The subscribe below needs the
                                      ;; session, and the ensure making one
                                      ;; has been in flight since the dial
                                      ;; began: the handshake waits for it
                                      ;; here, never before the dial. An
                                      ;; ensure that did not land drops
                                      ;; this connection — the ladder
                                      ;; re-laps it, exactly as the old
                                      ;; serial order did.
                                      (bt2:wait-on-semaphore (car attempt) :timeout 15)
                                      (setf handshake-complete (eq :ensured (cdr attempt)))
                                      (funcall reset-outage)
                                      ;; A takeover that finished — owning,
                                      ;; or attached to whoever won — may be
                                      ;; needed again on a later clean stop.
                                      (when (and takeover (not (bt2:thread-alive-p takeover)))
                                        (setf takeover nil))
                                      (cond ((not handshake-complete)
                                             (unless ensure-reported
                                               (setf ensure-reported t)
                                               (report-transport-error
                                                program nil "~a"
                                                (or (cdr attempt)
                                                    "the session ensure did not land")))
                                             (bt2:signal-semaphore closed))
                                            (t (note-shell-attached
                                                program model ws
                                                :build-payload frame.payload))))
                                     (:gateway-connect-rejected
                                      (setf permanent t)
                                      (report-transport-error
                                       program :permanent "~a"
                                       (or (gethash "message" frame.payload)
                                           "Gateway connection rejected"))
                                      (bt2:signal-semaphore closed))
                                     (:projection
                                      (when handshake-complete
                                        (funcall ingest msg)))))
                               (protocol-error (condition)
                                 (setf permanent t)
                                 (report-transport-error program :permanent "~a"
                                                         (protocol-error-message condition))))))
                   (wsd:on :close ws
                           (lambda (&key &allow-other-keys)
                             (unless permanent
                               (report-transport-error program nil "gateway disconnected"))
                             (bt2:signal-semaphore closed)))
                   (with-handlers ((error (condition)
                                     (unless permanent
                                       (report-transport-error program nil "~a" condition))))
                     (nlk:note-boot-phase "ws-dialing")
                     (wsd:start-connection ws)
                     (nlk:note-boot-phase "ws-driver-started")
                     ;; Park here for the connection's lifetime; the
                     ;; :close callback releases us. Closing immediately
                     ;; after START-CONNECTION returns would kill every
                     ;; connection milliseconds after the upgrade. Each
                     ;; timed-out wait sends a keepalive ping so the
                     ;; server's read timeout never fires on a healthy
                     ;; idle connection; SEND-PING no-ops unless the
                     ;; socket is :open, and a write on a dead socket
                     ;; signals into the reconnect ladder below.
                     (loop until (bt2:wait-on-semaphore
                                  closed :timeout *sync-keepalive-seconds*)
                           do (wsd:send-ping ws)))
                   (ignore-errors (wsd:close-connection ws))
                   (when permanent
                     (return))
                   (unless stop
                     ;; One failed attempt (connect refused, handshake never
                     ;; completed, or an established stream dropped): walk
                     ;; the self-heal ladder before backing off.
                     (incf failures)
                     (unless outage-started
                       (setf outage-started (get-internal-real-time)))
                     ;; An endpoint nobody serves any more -- its owner
                     ;; stopping or gone -- is not dialed again: the next lap
                     ;; watches the store instead, and starts or attaches to
                     ;; whatever comes next the moment it does.
                     (unless (or pinned (live-published-endpoint store-path))
                       (setf port nil
                             delay *sync-backoff-base-seconds*))
                     (let ((elapsed (/ (since-ms outage-started)
                                       1000)))
                       (unless failure-reported
                         (cond ((>= elapsed *sync-failure-after-seconds*)
                                (setf failure-reported t)
                                (report-transport-error program :failed "gateway unreachable ~
                                         for ~ds; still retrying"
                                                        (round elapsed)))
                               ;; Four failures escalate past a quiet reconnect notice.
                               ((>= failures 4)
                                (report-transport-error
                                 program :reconnecting "reconnect attempt ~d failed" failures))))
                       ;; Endpoint rediscovery, every lap past the threshold
                       ;; (and every lap once a candidate was spawned).
                       ;; REDISCOVER-GATEWAY-ENDPOINT answers only when the
                       ;; sidecar names a port that differs, so the notice
                       ;; fires exactly on movement. The old one-shot probe
                       ;; missed any owner that published after its single
                       ;; look — a respawned owner always does.
                       ;; Six failures: above the escalation, so the user sees the problem first.
                       (when (or (>= failures 6) (plusp resurrect-attempts))
                         (when-let (published (rediscover-gateway-endpoint
                                               store-path port))
                           (setf port published
                                 model.http-port published
                                 ;; A moved endpoint is a live gateway:
                                 ;; probe it now, not after the outage's
                                 ;; accumulated backoff.
                                 delay *sync-backoff-base-seconds*)
                           (report-transport-error
                            program :reconnecting "gateway endpoint moved; ~
                                            retrying on port ~d"
                            published)))
                       ;; Resurrection rung: only a DIRTY death (lock freed
                       ;; with the dead owner, sidecar still advertising it)
                       ;; licenses respawning. A clean shutdown deleted the
                       ;; sidecar deliberately; auto-respawning there would
                       ;; resurrect a server the operator just stopped.
                       (multiple-value-bind (verdict owner-pid)
                           (nlk::store-death-verdict store-path)
                         (case verdict
                           (:clean-shutdown
                            (cond
                              ;; The gateway supervises this store: a clean
                              ;; stop is its restart, and systemd brings
                              ;; the organism back. The dead endpoint goes,
                              ;; so the next lap watches the sidecar for
                              ;; the new one instead of dialing the old.
                              ((gateway-supervises-p store-path)
                               (unless shutdown-reported
                                 (setf shutdown-reported t)
                                 (report-transport-error
                                  program :reconnecting
                                  "the gateway stopped; attaching again when it is back"))
                               (unless pinned
                                 (setf port nil)))
                              ;; Nothing supervises it, and this shell
                              ;; fronts an organism: the owner let the store
                              ;; go on purpose — the gateway turned off, a
                              ;; sibling shell closed — and this shell takes
                              ;; the organism, the way a first launch does,
                              ;; then attaches to itself.
                              ((and boot (not pinned) (null takeover))
                               (setf takeover (funcall start-organism) port nil)
                               ;; Said as news, not as a fault: what ran it
                               ;; stopped on purpose, and this shell carries on.
                               (post-shell-notice
                                program "the organism stopped; this shell started it again"
                                :error-p nil))
                              ((not shutdown-reported)
                               (setf shutdown-reported t)
                               (report-transport-error
                                program :failed "gateway shut down and released ~
the store; not respawning — start another nodecode to take ownership"))))
                           (:dirty-death
                            (when (funcall resurrect failures elapsed)
                              (multiple-value-bind (launched reason)
                                  (cond ((and boot (not pinned))
                                         ;; A shell that starts organisms starts the
                                         ;; replacement too, fronted as the first was
                                         ;; (FRONT-ORGANISM); the dead endpoint goes.
                                         (unless (and takeover (bt2:thread-alive-p takeover))
                                           (setf takeover (funcall start-organism) port nil))
                                         t)
                                        (t (spawn-resurrection-candidate store-path)))
                                (cond (launched
                                       ;; The candidate boots in well under
                                       ;; a second; keep probing at the
                                       ;; base cadence instead of the
                                       ;; outage's accumulated backoff.
                                       (setf delay *sync-backoff-base-seconds*)
                                       (report-transport-error
                                        program :reconnecting "gateway (pid ~a) died ~
holding the store; ~:[spawned a replacement owner~;systemd restarts the gateway~] (attempt ~d/~d)"
                                        owner-pid (eq reason :systemd) resurrect-attempts
                                        *sync-resurrect-budget*))
                                      (t
                                       ;; Nothing launched: exhaust the
                                       ;; budget so this reports once,
                                       ;; while rediscovery keeps watching
                                       ;; for an owner started by hand.
                                       (setf resurrect-attempts
                                             *sync-resurrect-budget*)
                                       (report-transport-error
                                        program :failed "gateway (pid ~a) died ~
holding the store; could not spawn a replacement (~a) — restart ~
nodecode manually"
                                        owner-pid reason)))))))))
                     (sleep delay)
                     ;; Sync backoff_cap_ms 5000, factor 2.
                     (setf delay (min 5.0 (* 2 delay))))))))
      (call-with-profile-switch
       prog
       ;; The switch a /profile picker or `switch NAME' parked is performed
       ;; out here: after the program loop gave the terminal back and after
       ;; the teardown below let go of the organism this shell started.
       (lambda (&aux (quit nil))
         (with-cleanup ((setf stop t) (bt2:signal-semaphore ingress-slots :count 256)
                        (gateway-link-close model.ws)
                        ;; The organism this shell started goes with it, as it did
                        ;; when it ran inside it: join the boot (bounded -- it may
                        ;; still be starting it), then hand the organism the reason
                        ;; and let go of its pipe (STOP-FRONT). A parked switch hands
                        ;; the terminal to another home's organism, so it does not
                        ;; wait for this one to go; it lets go all the same, since an
                        ;; exec would keep the pipe open in the image that replaces
                        ;; this one. A no-op for a shell that attached instead.
                        (dolist (thread (remove nil (list boot-thread takeover)))
                          (nlk::%join-bounded thread 5))
                        (stop-front :why (and quit "quit")
                                    :wait (not nlk:*profile-switch-parked*))
                        (close-shell-presence))
           (call-with-severed-stdio (stdio-log-path store-path)
             (lambda ()
               (call-with-tui-error-capture prog
                 ;; A loop that returns is the operator's quit, which the
                 ;; next boot's note keeps quiet (gateway.lisp BACK-NOTE);
                 ;; a signal or a crash unwinds past it.
                 (lambda () (prog1 (nlt:run prog) (setf quit t)))))))))
      ;; The session the shell ended on, which a /sessions pick, a /fork or
      ;; Left may have moved off the one it launched with: the resume line a
      ;; caller prints names this one. A shell that never attached has none:
      ;; no gateway ever held a session of its to resume.
      (and model.ws model.session-id))))

(defun start-client-organism (&key session-id store-path port)
  "Boot a TUI client against a running Gateway without opening its store."
  ;; PORT NIL resolves from the default store's holder sidecar.
  ;;
  ;; STORE-PATH is rejected because the Gateway owns persistence and this
  ;; process must not acquire a competing SQLite lease.
  (when-let (ended (run-chat-gateway :session-id (or session-id (fresh-session-id)) :port port))
    (format t "~&To resume: (nle:start-client-organism :session-id ~s)~%" ended)))

;;; --- slash-command execution ----------------------------------------------
;;; One execution authority (slash.lisp owns the catalog and parser; ui.lisp
;;; owns the dropdown and the submit seam). Every shell is a GATEWAY-CHAT-MODEL
;;; -- the owner's own included -- so the gateway-backed commands go through
;;; the HTTP control ops, and the durable facts and live fanout happen where
;;; the store's owner runs. Every op returns notice text instead of
;;; signalling: a slash command's failure is chrome, never a crashed shell.

(defmacro with-slash-notice ((label &key (control "~a failed: ~a")) &body body)
  "BODY, with any error answered as notice text instead of signalling — the
one place that rule above is implemented — and T as a second value then, so a
caller that must tell a refusal from an answer need not read the words."
  ;; CONTROL is the FORMAT control the refusal is built with and LABEL its
  ;; first argument, the condition its second, so a lane whose copy is not the
  ;; default "<verb> failed: <why>" shape keeps its own Zig-parity wording at
  ;; the call site rather than losing it to the macro.
  (let ((reason (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       (error (,reason) (values (format nil ,control ,label ,reason) t)))))

(defun slash-themes-op (model args)
  "Open the theme picker (no ARGS) or engage ARGS as a theme name; the
choice persists across restarts."
  ;; Cosmetic op: resolution of a named theme stays strict inside
  ;; ENGAGE-THEME; the failure reports here rather than signalling.
  (with-slash-notice ("theme switch")
      (if (zerop (length args))
          (let* ((catalog (theme-catalog))
                 (current (saved-theme-selection))
                 ;; The Zig ThemePreviewSession original_theme capture: Esc
                 ;; restores exactly the palette the dialog opened over,
                 ;; however many rows were previewed in between.
                 (original-palette *palette*))
            (open-picker-dialog
             model "Themes"
             (mapcar (lambda (name)
                       (make-picker-row
                        :label name
                        ;; The Zig renderThemesDockedRow marker: the
                        ;; engaged theme names itself in the row, not
                        ;; only in the header.
                        :detail (if (equal name current) "current" "")
                        :value name))
                     catalog)
             #'slash-themes-op
             ;; Opening lands on the engaged theme (the Zig
             ;; themeDialogSelectionForCurrentTheme), so the open-time
             ;; highlight previews a no-op instead of the first row.
             :selection (or (position (or current (configured-theme-name)) catalog :test #'equal) 0)
             ;; Live hotload (the Zig previewSelectedThemesDialogItem):
             ;; every highlight move — keys, search edits, hover — engages
             ;; the row's theme without persisting. An unresolvable name
             ;; keeps the current palette; preview is cosmetic.
             :on-highlight (lambda-ignoring (model row)
                             (ignore-errors
                               (engage-theme
                                :name (picker-row-value row))))
             :on-cancel (lambda-ignoring (model)
                          (setf *palette* original-palette))
             :searchable t
             :context (if current (format nil "Current ~a" current) "")
             :search-placeholder "Search themes"
             :empty-label "No themes found"
             :hint "enter select  esc close  ↑↓ navigate  type search"))
          (progn (engage-theme :name args)
                 (save-theme-selection args)
                 (show-status model (format nil "Changed to ~a theme" args))))
      nil))

(defun slash-usage (name)
  (format nil "usage: ~a" (slash-command-usage (find-slash-command name))))

(defun auxiliary-model-summary (provider model effort)
  "One line naming the auxiliary model, or what stands in for it, in the
words /models' Auxiliary tab says it in."
  (if provider
      (format nil "auxiliary model: ~a/~a~@[ (~a)~]" provider model effort)
      "auxiliary model: same as main — naming, recaps and summaries run on each session's own model at its lightest effort"))

(defun decider-model-summary (provider model)
  "One line naming the decider general.decider_model picks, or that none is."
  (if provider
      (format nil "decider model: ~a/~a" provider model)
      "decider model: none — /models decider picks one"))

(defun auxiliary-target-summary ()
  "AUXILIARY-MODEL-SUMMARY of this image's auxiliary target as it stands."
  (multiple-value-bind (provider model-id effort configured) (auxiliary-model)
    (auxiliary-model-summary (and configured provider) model-id effort)))

(defun model-selection-summary ()
  "This image's selection the way a /models report opens: provider, model,
endpoint -- or why it has none -- then the auxiliary target."
  (format nil "provider: ~a~%model: ~a~%endpoint: ~a~%~a"
          *provider* *model*
          (handler-case (effective-endpoint)
            (provider-config-error (condition) (format nil "none: ~a" (provider-error-detail condition))))
          (auxiliary-target-summary)))

(defvar *serving* nil
  "T in the process SERVE-ORGANISM-CANDIDATE serves from: the one a stop's
deadline (SERVE-NEXT) and SIGTERM may end.")

(defvar *gateway-models* nil
  "The gateway's GET /api/gateway/models answer as this shell holds it:
(:rows PICKER-ROWS :connected IDS :listable IDS :refusals ((ID . WHY) ...)).
The gateway is the one
authority -- its image runs the cells whose lanes and inventory hooks add
models -- so a shell never builds the list itself.")

(defun gateway-models-facts (body)
  "BODY, a GET /api/gateway/models answer, as *GATEWAY-MODELS* holds it;
:UNREPORTED when the answer carries no providers field at all."
  (flet ((ids (key)
           (loop for provider across (or (gethash "providers" body) #())
                 when (gethash key provider) collect (gethash "id" provider))))
    (list :rows (build-models-catalog-picker-rows
                 (append
                  (loop for row across (or (gethash "models" body) #())
                        collect (list :provider (gethash "provider" row)
                                      :model (gethash "model" row)
                                      :name (gethash "name" row)
                                      :ctx (gethash "context" row)))
                  (loop for row across (or (gethash "unlisted" body) #())
                        collect (list :provider (gethash "provider" row)
                                      :placeholder (gethash "reason" row)))))
          :connected (ids "connected")
          :listable (ids "listable")
          :local (ids "local")
          ;; (ID . NAME) for each provider its folder names (OFFERED-PROVIDER-NAME)
          :names (loop for provider across (or (gethash "providers" body) #())
                       for name = (gethash "name" provider)
                       when (stringp name) collect (cons (gethash "id" provider) name))
          ;; (ID . WHY) for each provider no turn can run on there, WHY the
          ;; gateway's words (PROVIDER-TURN-REFUSAL reads them).
          :refusals (loop for provider across (or (gethash "providers" body) #())
                          for why = (gethash "refusal" provider)
                          when (stringp why) collect (cons (gethash "id" provider) why))
          ;; Each provider a folder not installed there serves, which picking
          ;; installs (PROVIDER-OFFERS' plists).
          :offers (loop for offer across (or (gethash "offers" body) #())
                        collect (append
                                 (list :cell (gethash "cell" offer) :commit (gethash "commit" offer)
                                       :provider (gethash "provider" offer) :name (gethash "name" offer)
                                       :sign-in (or (gethash "sign_in" offer) "key")
                                       :description (gethash "description" offer))
                                 ;; in already, its provider not answering yet
                                 (and (eq t (gethash "installed" offer)) (list :installed t))))
          ;; A gateway older than the providers field sends none: what it
          ;; has keys for is unknown here, not none.
          :unreported (not (nth-value 1 (gethash "providers" body))))))

(defun provider-turn-refusal (provider)
  "Why no turn can run on PROVIDER, in the words that say what to do, or
NIL. The serving image answers from its own lanes (SELECTION-REFUSAL). A
shell runs no cell, so a lane a cell registers exists in the gateway's image
only: a shell takes the gateway's answer (*GATEWAY-MODELS*' :REFUSALS) and
says nothing before that lands."
  ;; A shell asking its own lanes refused every pin a cell serves -- `provider
  ;; claude-code is not one this box knows' on the landing screen and on the
  ;; walk's step line, while every turn on it ran (the operator, 2026-10-22).
  (if *serving*
      (ignore-errors (selection-refusal provider))
      (cdr (assoc provider (getf *gateway-models* :refusals) :test #'string-equal))))

(defun fetch-gateway-models (model)
  "Read MODEL's gateway's model list into *GATEWAY-MODELS*; => the facts."
  (setf *gateway-models*
        (gateway-models-facts
         (gateway-model-http-call model :get "/api/gateway/models" nil))))

(defun gateway-models-landed (model value)
  "The loop's half of a /models list read (START-MODELS-PICKER-WARM): an
open picker restocks, and the landing screen, which says the gateway's
verdict on the pin (PROVIDER-TURN-REFUSAL), is laid out again."
  ;; The facts' read lands beside this one and repaints on its own: without
  ;; this, a list landing after it left the previous list's verdict painted --
  ;; a cold launch's `provider claude-code is not one this box knows', read
  ;; before the cells started, stood after they had (the operator, 2026-10-22).
  (cond ((off-loop-failure value) :quiet)
        ((home-screen-p model)
         (install-refreshed-models model)
         (render-state-at-paint model)
         nil)
        (t (install-refreshed-models model))))

(defun start-models-picker-warm (model)
  "Read the gateway's /models list off the loop, so the first press of
/models paints from it; an open picker restocks when it lands."
  (when (and model.program model.http-port)
    (off-loop model (lambda () (fetch-gateway-models model)) #'gateway-models-landed)))

(defun models-catalog-picker-rows ()
  "The /models picker's rows. The serving image -- the organism, whose cells
run -- builds them from MODEL-CHOICES; a shell reads them from it
(*GATEWAY-MODELS*), NIL before the first read lands."
  (if *serving*
      (build-models-catalog-picker-rows (model-choices))
      (getf *gateway-models* :rows)))

(defun picker-connected-provider-ids ()
  "CONNECTED-PROVIDER-IDS as the serving image answers it."
  (if *serving* (connected-provider-ids) (getf *gateway-models* :connected)))

(defun picker-listable-provider-ids ()
  "LISTABLE-PROVIDER-IDS as the serving image answers it."
  (if *serving* (listable-provider-ids) (getf *gateway-models* :listable)))

(defun picker-provider-offers ()
  "PROVIDER-OFFERS as the serving image answers it."
  (if *serving* (ignore-errors (provider-offers)) (getf *gateway-models* :offers)))

(defun picker-provider-name (id)
  "OFFERED-PROVIDER-NAME as the serving image answers it."
  (if *serving*
      (ignore-errors (offered-provider-name id))
      (cdr (assoc id (getf *gateway-models* :names) :test #'string-equal))))

(defun install-offered-cell (model offer)
  "Install OFFER's folder (PROVIDER-OFFERS) through MODEL's organism at the
pin the picker read, then read its models and its commands again, the
folder's own among them: => (:COMMANDS ENTRIES), or (:REFUSED WHY)."
  ;; Off the loop: a hub install clones before it loads.
  (handler-case
      (progn (gateway-model-http-call model :post "/api/gateway/cells"
                                      (json-object "name" (getf offer :cell) "on" t
                                                   :opt "commit" (getf offer :commit))
                                      :read-timeout 300)
             ;; the folder is in: a reread that fails leaves the lists in hand
             (ignore-errors (fetch-gateway-models model))
             (list :commands (ignore-errors (gateway-command-entries model))))
    (gateway-http-error (refusal) (list :refused (gateway-http-error-detail refusal)))))

(defun picker-local-provider-ids (rows)
  "The providers of ROWS, /models' model rows, served from a machine of the
operator's own (LOCAL-ENDPOINT-P) as the serving image answers it."
  (if *serving*
      (loop with ids = '()
            for row in rows
            for provider = (first (picker-row-value row))
            when (and (stringp provider) (local-endpoint-p (provider-base-url provider)))
              do (pushnew provider ids :test #'string-equal)
            finally (return ids))
      (getf *gateway-models* :local)))

(defun newer-version-p (a b &aux (i 0) (j 0))
  "Whether the model id A lists above B: where they first differ, a run of
digits lists the higher number first (z-10 above z-2, opus-4-6 above
opus-4-5), any other character in alphabetical order, case-insensitively;
an id lists above its own prefix (gpt-5.1 above gpt-5)."
  (loop
    (cond ((>= j (length b)) (return (< i (length a))))
          ((>= i (length a)) (return nil)))
    (let ((x (char a i)) (y (char b j)))
      (if (and (digit-char-p x) (digit-char-p y))
          (let* ((end-a (or (position-if-not #'digit-char-p a :start i) (length a)))
                 (end-b (or (position-if-not #'digit-char-p b :start j) (length b)))
                 (m (parse-integer a :start i :end end-a))
                 (n (parse-integer b :start j :end end-b)))
            (unless (= m n) (return (> m n)))
            (setf i end-a j end-b))
          (progn (unless (char-equal x y) (return (char-lessp x y)))
                 (incf i) (incf j))))))

(defun model-bare-id (id &aux (id (string-downcase id)))
  "ID as models.dev keys the model under any provider: lowercase, past its
last `/' (a router's vendor prefix) and short of its first `:' (a variant
such as `:batch' or `:free')."
  (let ((start (1+ (or (position #\/ id :from-end t) -1))))
    (subseq id start (or (position #\: id :start start) (length id)))))

(defvar *model-release-index* nil
  "(CATALOG . TABLE): per MODEL-BARE-ID, the earliest release date the parsed
models.dev CATALOG gives that model under any provider; rebuilt when the
catalog is reparsed.")

(defun model-release-date (catalog provider model-id)
  "When MODEL-ID was released, as models.dev dates it: its own entry under
PROVIDER, else the earliest date any provider gives its bare id -- a
router's listing (cline, zen) carries no dates of its own -- else NIL."
  (flet ((models-of (provider)
           (let ((entry (and (hash-table-p catalog) (gethash provider catalog))))
             (and (hash-table-p entry) (hash-table-p (gethash "models" entry))
                  (gethash "models" entry)))))
    (let ((own (gethash model-id (or (models-of provider) (make-hash-table)))))
      (or (and (typep own 'catalog-model) (catalog-model-release-date own))
          (let ((index *model-release-index*))
            (unless (and index (eq (car index) catalog))
              (let ((table (make-hash-table :test #'equal)))
                (when (hash-table-p catalog)
                  (loop for id being the hash-keys of catalog
                        do (loop for model being the hash-keys of (or (models-of id) (make-hash-table))
                                   using (hash-value entry)
                                 for date = (and (typep entry 'catalog-model)
                                                 (catalog-model-release-date entry))
                                 for bare = (and date (model-bare-id model))
                                 when (and date (or (null (gethash bare table))
                                                    (string< date (gethash bare table))))
                                   do (setf (gethash bare table) date))))
                (setf index (setf *model-release-index* (cons catalog table)))))
            (values (gethash (model-bare-id model-id) (cdr index))))))))

(defun newer-model-p (a b)
  "Whether the model A, as (DATE BARE-ID PROVIDER MODEL-ID), lists above B:
the later release date first, a dated model above an undated one; then by
the bare id (NEWER-VERSION-P), the provider, the id."
  (destructuring-bind (date-a bare-a provider-a id-a) a
    (destructuring-bind (date-b bare-b provider-b id-b) b
      (cond ((not (equal date-a date-b))
             (or (null date-b) (and date-a (string> date-a date-b))))
            ((string-not-equal bare-a bare-b) (newer-version-p bare-a bare-b))
            ((string-not-equal provider-a provider-b) (string-lessp provider-a provider-b))
            (t (newer-version-p id-a id-b))))))

(defparameter +own-provider+ "nodecode"
  "The provider id of nodecode.ai's own API, the one `nodecode login' wires:
its models lead every model list, and /models pins them (MODELS-OWN-ROWS).")

(defun model-choices ()
  "The models a turn can run on: the models.dev cache, the configured
provider models, and each configured provider's fetched listing
(PROVIDER-MODELS-CACHE-ENTRY), merged and — when any provider is connected (CONNECTED-PROVIDER-IDS) —
retained to the connected set (the Zig gateway_models_catalog pipeline:
mergeConfiguredProviderModels fills a known row's missing context budget and
appends unknown models; retainConfiguredProviders drops unconfigured lanes),
nodecode.ai's own (+OWN-PROVIDER+) first, then newest first (NEWER-MODEL-P)."
  ;; => plists (:provider :model :name :ctx), newest first, then one
  ;; (:provider :placeholder REASON) per provider whose listing failed.
  ;;
  ;; A provider whose listing failed, and whose last answer kept no fetched
  ;; row, trails one placeholder row — `provider/` with the failure as its
  ;; detail — beside whatever the config still types by hand, so the operator
  ;; sees why the listing is missing: an entry naming no sdk cannot be fetched
  ;; at all, and its hand-typed rows alone hid that (one model shown where the
  ;; listing carries ninety, and no reason). There is no unselectable row: its
  ;; value is NIL, and Enter on it reaches SLASH-MODELS-OP with no tokens,
  ;; which just reports the active selection.
  ;;
  ;; NIL when no source yields a row.
  (let ((configured-models (nth-value 1 (configured-provider-inventory))))
    (let* ((catalog (models-catalog-table))
           (listable (listable-provider-ids))
           (retained (union (connected-provider-ids) listable :test #'string-equal))
           ;; One place per (provider, model) pair, found by hash: the fold
           ;; over ~2,000 catalog rows used to ask every row built so far, and
           ;; that quadratic scan is what a rebuild paid.
           (by-pair (make-hash-table :test #'equal))
           (rows '())
           (placeholders '()))
      (flet ((fold (provider model-id display ctx)
               "Fold one (PROVIDER MODEL-ID) in: a known row only gains a
missing context budget and a missing name; an unknown pair joins ROWS.
DISPLAY is the model name when nonblank; provider names never fill it."
               (let* ((key (list (string-downcase provider) (string-downcase model-id)))
                      (name (and (stringp display) (plusp (length (nlk:trimmed display))) display))
                      (hit (gethash key by-pair)))
                 (if hit
                     (setf (getf hit :ctx) (or (getf hit :ctx) ctx)
                           (getf hit :name) (or (getf hit :name) name))
                     (setf (gethash key by-pair)
                           (car (push (list :provider provider :model model-id
                                            :name name :ctx ctx)
                                      rows)))))))
        ;; The models.dev cache's raw (:provider :model :name :ctx) rows first,
        ;; each a model a turn can run on (CATALOG-MODEL-TURN-P).
        (when (hash-table-p catalog)
          (loop for provider-id being the hash-keys of catalog using (hash-value provider)
                for models = (json-value provider :object "models")
                ;; A provider no lane drives lists nothing from the catalog: a
                ;; pick of one is refused at the gateway.
                when (and models (or (null retained)
                                     (member provider-id retained :test #'string-equal))
                          (configured-provider-lane provider-id))
                  do (loop for model-id being the hash-keys of models using (hash-value entry)
                           when (catalog-model-turn-p entry)
                             do (fold provider-id model-id
                                      (catalog-model-name entry)
                                      (let ((value (catalog-model-context entry)))
                                        (and value (plusp value) value))))))
        (dolist (configured configured-models)
          (fold
           ;; Configured rows carry a codename, not a model display name.
           (getf configured :provider-id)
           (getf configured :model-id)
           nil
           (getf configured :context-window)))
        (dolist (provider listable)
          (when-let (entry (provider-models-cache-entry provider))
            (dolist (model (getf entry :models))
              ;; A listing that names a model's kind offers a turn its chat
              ;; models alone: nodecode.ai's image model answers no turn.
              (when (let ((kind (getf model :kind))) (or (null kind) (string-equal kind "chat")))
                (fold provider (getf model :id)
                      (getf model :display)
                      (getf model :context-window))))
            (when (and (getf entry :error) (null (getf entry :models)))
              (push (list :provider provider :placeholder (getf entry :error))
                    placeholders))))
        ;; A row no source above named or sized -- a relay's bare id
        ;; (claude-code, cline) that its own namespace never carries -- takes
        ;; the name most catalog namespaces give that id (the first in
        ;; provider-id order on a tie: the resellers' consensus, as
        ;; MODEL-PRICE reads it) and the window a turn on it resolves
        ;; (MODEL-CONTEXT-TOKENS, the one capability seam, its cell hooks
        ;; included).
        (dolist (row rows)
          (let ((provider (getf row :provider)) (model (getf row :model)))
            (unless (getf row :name)
              (setf (getf row :name)
                    (let ((names (loop for (nil . entry) in (models-catalog-rows model)
                                       for name = (catalog-model-name entry)
                                       when (and (stringp name) (plusp (length (nlk:trimmed name))))
                                         collect name)))
                      (first (stable-sort (copy-list (remove-duplicates names :test #'equal :from-end t))
                                          #'> :key (lambda (name) (count name names :test #'equal)))))))
            (unless (getf row :ctx)
              (setf (getf row :ctx)
                    (let ((ctx (model-context-tokens model provider)))
                      (and (realp ctx) (plusp ctx) ctx)))))))
      (append
       ;; Every list a model is picked from reads this order, its search
       ;; boxes included: their ranking is stable over it. nodecode.ai's own
       ;; models lead it.
       (let ((newest (mapcar #'cdr
                             (sort (loop for row in rows
                                         collect (cons (list (model-release-date catalog (getf row :provider)
                                                                                 (getf row :model))
                                                             (model-bare-id (getf row :model))
                                                             (getf row :provider) (getf row :model))
                                                       row))
                                   #'newer-model-p :key #'car))))
         (flet ((own-p (row) (string-equal (getf row :provider) +own-provider+)))
           (append (remove-if-not #'own-p newest) (remove-if #'own-p newest))))
       ;; Placeholders trail the list, in provider order.
       (sort placeholders #'string-lessp
             :key (lambda (row) (getf row :provider)))))))

(defun build-models-catalog-picker-rows (choices)
  "CHOICES, rows shaped as MODEL-CHOICES answers them, as /models picker
rows; NIL when there are none, and the caller falls back to the textual
selection summary."
  ;; The row's value is the (PROVIDER MODEL) token pair, so Enter routes the
  ;; lane too.
  (mapcar (lambda (row)
            (if (getf row :placeholder)
                (make-picker-row
                 :label (format nil "~a/" (getf row :provider))
                 :detail (format nil "no listing (~a)" (getf row :placeholder))
                 :value nil)
                ;; The model name leads (codename when absent); the provider
                ;; follows in the shared picker's two-space detail column.
                ;; nodecode.ai's own models read as the pair a request names,
                ;; nodecode/<model>, in their own tone on every list.
                (make-picker-row
                 :label (if (string-equal (getf row :provider) +own-provider+)
                            (format nil "~a/~a" (getf row :provider) (getf row :model))
                            (or (getf row :name) (getf row :model)))
                 :tone (and (string-equal (getf row :provider) +own-provider+) :own-model)
                 :detail (getf row :provider)
                 :suffix (if (getf row :ctx)
                             (format nil "  • ctx ~a" (format-token-count (getf row :ctx)))
                             "")
                 :value (list (getf row :provider) (getf row :model)))))
          choices))

(defun models-picker-title (count refreshing-p &key auxiliary)
  "The Zig models header counts the catalog inside the tab label itself
(appendModelsDockedTab); a live fetch says so after it."
  ;; The Auxiliary tab's list names itself in the title, so the operator knows
  ;; which model Enter moves.
  (format nil "~:[Models~;Auxiliary model~] (~d)~:[~; · fetching~]"
          auxiliary count refreshing-p))

(defun start-provider-models-refresh (model)
  "Have the gateway refetch every stale or unlisted provider, on a background
thread, then rebuild an open /models picker from what it landed."
  ;; => T when a thread started. Nothing
  ;; starts without a live program (tests drive the install directly and must
  ;; not spawn threads), without a cache path, with nothing stale, or while a
  ;; refresh is already in flight — the one that lands repaints for both.
  ;;
  ;; The gateway fetches, never this shell: its image is the one whose cells
  ;; are started, so their hooks on a listing run.
  ;; The file it wrote moved under this image's memo, which is dropped here —
  ;; without dropping it, a rewrite of the same size in the same second would
  ;; keep the memo's identity.
  (let ((program model.program)
        (dialog model.dialog)
        (stale (and *provider-models-cache-path* (stale-listable-provider-ids))))
    ;; Every open rereads the gateway's list (its cells add models this
    ;; shell never sees); => T only when a stale listing is being fetched.
    (when (and program
               (let ((thread *provider-models-refresh-thread*))
                 (or (null thread) (not (bt2:thread-alive-p thread)))))
      (setf *provider-models-refresh-thread*
            (off-loop model
                      (lambda ()
                        (with-cleanup ((setf *provider-models-memo* nil
                                             *provider-models-refresh-thread* nil))
                          (when stale
                            (ignore-errors
                              (gateway-model-http-call
                               model :post "/api/gateway/provider-models/refresh"
                               (json-object "ids" (coerce stale 'vector)))))
                          (ignore-errors (fetch-gateway-models model))))
                      (lambda (model value)
                        (if (eq dialog model.dialog)
                            (install-refreshed-models model value)
                            :quiet))))
      (and stale t))))

;;; --- the models.dev catalog fetch ----------------------------------------------
;;; provider.lisp owns the models.dev cache the /models list is built from:
;;; MODELS-CATALOG-TABLE refetches an absent or stale file behind itself. A
;;; fresh box has none, so a connect list would hold the Other-provider entry
;;; alone until the fetch lands. The shell's half is only the wake: the open
;;; connect dialog takes the rows the moment they arrive.

(defun start-models-catalog-fetch (model &aux (program model.program))
  "On a box with no catalog, start REFRESH-MODELS-CATALOG and take its rows
into an open /models list when the fetch lands."
  ;; => T when a waiter started. Nothing starts without a live program
  ;; (tests drive the install directly), with a catalog already there (a stale
  ;; one refreshes behind its reader, and the list keeps the rows it opened
  ;; on), or with no fetch to wait on.
  (when (and program (null (models-catalog-table)))
    (when-let (fetch (refresh-models-catalog))
      (off-loop model (lambda () (bt2:join-thread fetch)) #'install-fetched-catalog)
      t)))

(defun install-fetched-catalog (model value &aux (dialog model.dialog))
  "An open /models list at its provider step takes the fetched catalog's
rows; a failed fetch is the step's own message, and a typed id needs the
catalog too (PROVIDER-ID-REFUSAL)."
  ;; VALUE is what the fetch thread answered — NIL, or why it failed — or the
  ;; OFF-LOOP failure when the join itself signalled.
  ;; The list searches, so its header row is the search row: the failure rides
  ;; the prompt, and names the keys saved here that wait on the list for an
  ;; endpoint. Written as the subtitle, it never showed, and an offline box
  ;; with a deepseek key read an empty list under `Search providers' (xm-204).
  (when (and (connect-dialog-p dialog)
             (eq (connect-dialog-step dialog) :provider))
    (let ((failure (or (off-loop-failure value) value))
          (waiting (sort (set-difference (configured-provider-inventory) (picker-connected-provider-ids)
                                         :test #'string-equal)
                         #'string<)))
      (unless failure
        (setf dialog.rows (funcall (connect-dialog-provider-rows dialog)))
        (picker-seat-top dialog))
      ;; A walk that wrote its own prompt over the loading one keeps it. The
      ;; way on comes before the row runs out at 80 columns.
      (when (or (member dialog.search-placeholder (list +loading-providers+ "Search providers" +search-models-root+)
                        :test #'equal)
                (uiop:string-prefix-p "no models.dev list" dialog.search-placeholder))
        (setf dialog.search-placeholder
              (cond (failure
                     (format nil "no models.dev list (~a) · ~:[reopen this list once online~;~:*~{~a~^ and ~} ~
                                  need~:[s~;~] it · reopen online~]"
                             (first-line failure) waiting (rest waiting)))
                    ((eq dialog.kind :model-providers) +search-models-root+)
                    (t "Search providers")))))
    (when model.band
      (render-state model))))

(defun models (&optional provider)
  "The catalog the shell's /models picker shows, as an answer instead of a
dialog."
  ;; Bare => an alist of (PROVIDER . MODEL-COUNT) over every provider the
  ;; picker would list. With PROVIDER => (values IDS LISTING-ERROR): that
  ;; provider's model ids and, when its fetched listing failed, why — the
  ;; configured models still count. Stale or absent fetched listings are
  ;; refreshed first (workers joined), so the answer is the picker's, not a
  ;; cold cache's. An unconfigured PROVIDER refuses, naming the configured
  ;; set.
  (check-type provider (or null provider-id))
  (let ((known (sort (copy-list (union (connected-provider-ids)
                                       (listable-provider-ids)
                                       :test #'string-equal))
                     #'string-lessp)))
    (when (and provider (not (member provider known :test #'string-equal)))
      (error "no provider ~s is configured~@[ (configured: ~{~a~^, ~})~]"
             provider known))
    (when-let (stale (intersection (if provider (list provider) known)
                                   (stale-listable-provider-ids)
                                   :test #'string-equal))
      (refresh-provider-models stale))
    (let ((rows (models-catalog-picker-rows)))
      (flet ((ids-for (id)
               (loop for row in rows
                     for value = (picker-row-value row)
                     when (and value (string-equal (first value) id))
                       collect (second value))))
        (if provider
            (values (ids-for provider)
                    (getf (provider-models-cache-entry provider) :error))
            (loop for id in known
                  collect (cons id (length (ids-for id)))))))))

(defvar *models-frequent-by-dialog* (make-hash-table :test #'eq :weakness :key)
  "Live-only ranking for the dialog that read it; retained usage owns history.")

(defparameter +frequently-used+ "Frequently used"
  "The heading over a models list's shortcuts from retained history.")

(defun models-frequent-picker-rows (rows history &key providers &aux (frequent '()))
  "Promote at most five available model pairs from retained HISTORY.
With PROVIDERS, the rest of the root is providers, never more model rows."
  (loop for entry across history
        for value = (list (gethash "provider" entry) (gethash "model" entry))
        for row = (find value rows :key #'picker-row-value :test #'equal)
        when (and row (not (member row frequent :test #'eq))) do (push row frequent)
        when (= (length frequent) 5) do (loop-finish))
  (if (null frequent)
      (or providers rows)
      (let ((others (or providers
                        (remove-if (lambda (row) (member row frequent :test #'eq)) rows))))
        (append
         (list (make-picker-row :label +frequently-used+ :heading t))
         (mapcar (lambda (row) (if providers (models-root-row row) row))
                 (reverse frequent))
         (when others
           (append (list (make-picker-row :heading t)
                         (make-picker-row :label (if providers "Providers" "Other models")
                                          :heading t))
                   others))))))

(defun models-dialog-catalog-rows (dialog)
  "The canonical model rows, narrowed by the dialog's existing tab strategy."
  (let* ((rows (models-catalog-picker-rows))
         (tab (nth (picker-dialog-tab dialog) (picker-dialog-tabs dialog)))
         (match (getf tab :match)))
    (if match (remove-if-not (lambda (row) (funcall match row (getf tab :value))) rows) rows)))

(defun models-provider-row-p (row provider)
  "Whether a model row, including a failed-listing row, belongs to PROVIDER."
  (if-let (value (picker-row-value row))
    (string-equal (first value) provider)
    (string-equal (picker-row-label row) (format nil "~a/" provider))))

(defparameter +search-models-root+ "Search providers and models"
  "/models' provider list's search row: its query finds models too
(SEARCH-MODELS-FROM-ROOT).")

(defun models-sign-in-rows ()
  "While this home is not signed in to nodecode.ai (+OWN-PROVIDER+ is not
connected), the one row /models' search offers for it: Enter opens /login;
NIL once it is."
  ;; The provider is on no models.dev list, so a home not signed in met
  ;; nothing of ours: searching `nodecode' in /models found nothing
  ;; (the operator, 2026-10-22).
  (unless (member +own-provider+ (picker-connected-provider-ids) :test #'string-equal)
    (list (make-picker-row :label "Sign in to nodecode.ai" :tone :own-model
                           :detail "nodecode's own models"
                           :value (list :model (list :login))))))

(defun search-models-from-root (dialog)
  "Have DIALOG, /models' provider list, find models as well as providers:
while its box holds a query, every model a turn can run on lists under the
list's own rows, newest first (MODEL-CHOICES), each picked the way a
Frequently used shortcut is (MODELS-ROOT-ROW), and -- while this home is not
signed in to nodecode.ai -- the row that signs it in (MODELS-SIGN-IN-ROWS).
=> DIALOG."
  ;; The rows go on through DIALOG's one rows function, so every restock -- a
  ;; refreshed listing, the history read, a saved key -- builds the list the
  ;; box asks for. SEARCHING is the kind it built last: a keystroke rebuilds
  ;; only when the box fills or empties, and the rest only rerank.
  (let ((root (connect-dialog-provider-rows dialog))
        (searching nil))
    (flet ((querying ()
             (and (eq :provider (connect-dialog-step dialog))
                  (plusp (length (picker-dialog-search dialog))))))
      (setf (connect-dialog-provider-rows dialog)
            (lambda (&aux (rows (funcall root)))
              (if (setf searching (querying))
                  ;; a model the list already offers on both tabs (Frequently
                  ;; used, Local) is not offered twice
                  (let ((held (loop for row in rows
                                    for pair = (getf (picker-row-value row) :model)
                                    when (and (consp pair) (stringp (first pair))
                                              (equal "" (picker-row-key row)))
                                      collect pair)))
                    (append rows
                            (loop for row in (models-catalog-picker-rows)
                                  for pair = (picker-row-value row)
                                  when (and pair (not (member pair held :test #'equal)))
                                    collect (models-root-row row))
                            (models-sign-in-rows)))
                  rows))
            (picker-search-handlers dialog)
            ;; a result step's edit keys reach the query too: they rebuild nothing
            (lambda (dialog query)
              (declare (ignore query))
              (when (and (eq :provider (connect-dialog-step dialog))
                         (not (eq searching (querying))))
                (setf (picker-dialog-rows dialog) (funcall (connect-dialog-provider-rows dialog)))))))
    (when (equal (picker-dialog-search-placeholder dialog) "Search providers")
      (setf (picker-dialog-search-placeholder dialog) +search-models-root+))
    dialog))

(defun models-root-row (row)
  "A model ROW as /models' provider list carries it: a copy whose value,
(:MODEL (PROVIDER MODEL)), the wizard picks as that model instead of asking
a provider's key."
  (let ((copy (copy-picker-row row)))
    (setf (picker-row-value copy) (list :model (picker-row-value row)))
    copy))

(defun restock-models-dialog (model dialog value &key top)
  "Refill DIALOG, MODEL's open /models list, in place: its provider step's rows, else the
catalog under its frequently used head, the cursor back on VALUE's row past the headings
-- or, with TOP, on the first row."
  (setf dialog.rows (if (connect-dialog-p dialog)
                        (funcall (connect-dialog-provider-rows dialog))
                        (models-frequent-picker-rows
                         (models-dialog-catalog-rows dialog)
                         (gethash dialog *models-frequent-by-dialog* #()))))
  (picker-reseat-selection dialog value)
  (setf dialog.selection (past-headings (picker-visible-rows dialog) dialog.selection 1))
  (when top
    (picker-seat-top dialog)
    ;; The Nodecode section stays on top in view; the cursor takes the
    ;; first shortcut, the model a reopen picks again.
    (let* ((visible (picker-visible-rows dialog))
           (at (position-if (lambda (row) (and (picker-row-heading row)
                                               (equal +frequently-used+ (picker-row-label row))))
                            visible)))
      (when at (setf dialog.selection (past-headings visible (1+ at) 1)))))
  (when model.band (render-state model)))

(defun install-frequent-models (model dialog value)
  "Install history only in the identical dialog that dispatched the read.
Closing, replacing, or reopening the picker discards its old completion."
  (unless (eq dialog model.dialog)
    (return-from install-frequent-models :quiet))
  (when-let (failure (off-loop-failure value))
    (show-status model (format nil "Couldn't read frequently used models: ~a" failure) :error-p t)
    (return-from install-frequent-models nil))
  (let* ((history (gethash "models" value))
         (selected (picker-highlighted-value dialog))
         (untouched (and (= dialog.selection (past-headings (picker-visible-rows dialog) 0 1))
                         (zerop (picker-dialog-view-offset dialog))
                         (zerop (length (picker-dialog-search dialog))))))
    (setf (gethash dialog *models-frequent-by-dialog*) history)
    ;; Retain the ranking while a credential is being entered, but never
    ;; replace that step's rows or selection with a late history answer.
    (when (and (eq :model-providers dialog.kind) (not (eq :provider (connect-dialog-step dialog))))
      (return-from install-frequent-models :quiet))
    (restock-models-dialog model dialog selected :top untouched)))

(defun start-models-frequent-read (model dialog)
  "Read retained model history off the UI loop, scoped to this dialog."
  (when model.program
    (off-loop model
              (lambda () (gateway-model-http-call model :get "/api/gateway/models/frequent" nil))
              (lambda (model value) (install-frequent-models model dialog value)))))

(defun open-models-picker (model &key (lands :session) then provider
                                 &aux (rows (models-catalog-picker-rows)) (auxiliary (eq lands :auxiliary)))
  "Open the model detail picker, narrowed to PROVIDER when given: a
provider picked on either of /models' tabs, and setup's model. Open from the
current caches, then refresh the fetched listings behind it."
  ;; LANDS says where Enter lands the row: :SESSION pins this shell's session
  ;; and makes it the default new sessions start on (SLASH-MODELS-OP);
  ;; :AUXILIARY, the Auxiliary tab's, lands the auxiliary model and pins no
  ;; session; :DEFAULT moves the gateway default
  ;; alone and pins no session (SLASH-DEFAULT-MODEL-OP), the setup walk's
  ;; model; a function, (MODEL TOKENS) => (values NOTICE FAILED), lands it
  ;; where its caller keeps a model of its own. THEN, when
  ;; given, is called once the
  ;; dialog is done, with the model and the picked (PROVIDER MODEL) pair — NIL
  ;; on Esc or a refused pick: the setup wizard's continuation, which reports a
  ;; model only when one was picked, and /models' way back to its providers.
  ;; A continuation always gets the list, empty until the refresh below lands
  ;; in it: without it, a walk whose list had not loaded yet went back to its
  ;; providers, the bare /models report standing over their key hints (Mac
  ;; take, 2026-10-21).
  ;; => (values notice cmd).
  (cond
    ((or rows provider then)
     (open-picker-dialog
      model (models-picker-title (length rows) nil :auxiliary auxiliary) rows
      ;; The row's value is the (PROVIDER MODEL) token pair, so Enter
      ;; routes the lane too — the Zig "select for session" semantics.
      (lambda (model tokens)
        (multiple-value-bind (notice failed)
            (if (functionp lands)
                (funcall lands model tokens)
                (ecase lands
                  (:session (slash-models-op model tokens))
                  (:auxiliary (slash-auxiliary-model-op model tokens))
                  (:default (slash-default-model-op model tokens))))
          ;; A pick the gateway refused says why in a toast, over whatever
          ;; comes next: its notice is set after the dialog closes, when no
          ;; frame carries it, and without this the pick would be silent. It
          ;; is no pick to the walk that chained this either.
          (cond (failed
                 (show-status model (or (and (stringp (first tokens)) (provider-id-refusal (first tokens)))
                                        notice)
                              :error-p t)
                 (when then (funcall then model nil))
                 nil)
                (t (when then (funcall then model tokens))
                   notice))))
      :kind (if auxiliary :auxiliary-models :models)
      :searchable t
      :search-placeholder "Search models"
      :empty-label (if rows "No models found" "Loading the model list…")
      ;; The Zig models footer, minus the ctrl+g default and tab
      ;; pagination chords this shell does not bind.
      :hint (case lands
              (:auxiliary "enter use as auxiliary  esc close  ↑↓ navigate")
              (:session "enter select for session  esc close  ↑↓ navigate")
              (t "enter select  esc close  ↑↓ navigate"))
      :on-cancel (and then (lambda (model) (funcall then model nil))))
     (when provider
       (register-picker-tabs model.dialog
                             (list (list :label provider :value provider :match #'models-provider-row-p)))
       (setf (picker-dialog-rows model.dialog) (models-dialog-catalog-rows model.dialog)
             ;; the title says which model Enter moves, the tab whose models
             ;; these are: a `Models' after `Auxiliary model' read as a third
             (picker-dialog-context model.dialog) ""
             (picker-dialog-hint model.dialog) (if auxiliary
                                                    "enter use as auxiliary  esc providers  ↑↓ navigate"
                                                    "enter select for session  esc providers  ↑↓ navigate"))
       (picker-seat-top model.dialog)
       (seat-models-picker model.dialog provider :auxiliary auxiliary))
     (start-models-frequent-read model model.dialog)
     (when (start-provider-models-refresh model)
       (setf (picker-dialog-title model.dialog)
             (models-picker-title (length rows) t :auxiliary auxiliary)))
     (values nil nil))
    (t
     ;; Nothing cached yet: fetch, and report the selection meanwhile —
     ;; reopening once the refresh lands shows the list.
     (start-provider-models-refresh model)
     (values (if auxiliary
                 (slash-auxiliary-model-op model nil)
                 (slash-models-op model nil))
             nil))))

(defun suggested-model (provider &optional word &aux (best nil)
                                                    (best-date nil))
  "The model id models.dev lists for PROVIDER that a turn can run on
(CATALOG-MODEL-TURN-P) and was released last, or NIL."
  ;; The organism works through tools, and release order is the one ranking
  ;; the catalog carries; a tie goes to the id sorting first. WORD, when
  ;; given, is one of the dash-separated words the id must carry.
  (maphash (lambda (id entry &aux (date (catalog-model-release-date entry)))
             (when (and date
                        (or (null word) (member word (uiop:split-string id :separator "-")
                                                :test #'string=))
                        (catalog-model-turn-p entry)
                        (or (null best-date)
                            (string> date best-date)
                            (and (string= date best-date) (string< id best))))
               (setf best id
                     best-date date)))
           (or (json-value (models-catalog-table) :object provider "models")
               (make-hash-table)))
  best)

(defun seat-models-picker (dialog provider &key auxiliary)
  "Start the open /models DIALOG on PROVIDER: on the model the shared
config's default -- with AUXILIARY, its auxiliary model -- already names
there, else, for the default, on its newest (SUGGESTED-MODEL), the search row
saying which. Neither listed, nothing moves."
  ;; The newest model is no suggestion for the auxiliary model, which wants a quick one.
  (let* ((kept (multiple-value-bind (pinned id)
                   (shared-model-pin (if auxiliary "auxiliary_model" "default_model"))
                 (and pinned (string-equal pinned provider) id)))
         (suggested (and (not auxiliary) (suggested-model provider))))
    (flet ((row-at (model-id)
             (and model-id
                  (position-if (lambda (row &aux (value (picker-row-value row)))
                                 (and (consp value)
                                      (string-equal (first value) provider)
                                      (string-equal (second value) model-id)))
                               (picker-visible-rows dialog)))))
      (let* ((kept-at (row-at kept))
             (at (or kept-at (row-at suggested))))
        (when at
          (setf dialog.selection at
                dialog.search-placeholder
                ;; a kept model the list does not carry is said, not passed over in silence
                (if kept-at
                    (format nil "~a, the ~:[model this home names~;auxiliary model now~] · type to search"
                            kept auxiliary)
                    (format nil "~@[~a, the model this home names, is not listed · ~]~
                                 suggested: ~a, the newest with tool calls · type to search"
                            kept suggested))))))))

(defun install-refreshed-models (model &optional landed &aux (dialog model.dialog))
  "Rebuild an open /models picker from the refreshed cache: rows swap in
place, the search text stays, and the selection follows its row value — a
value the refresh dropped leaves the cursor on its seat."
  (declare (ignore landed))
  (when (and (picker-dialog-p dialog)
             ;; The /models catalog in either mode, or a provider list: /models'
             ;; or the setup walk's, which has no kind and opens before the
             ;; gateway's list lands -- its offers (PROVIDER-OFFERS) came with
             ;; it, and a walk that kept its first rows never listed them.
             (or (member dialog.kind '(:models :auxiliary-models))
                 (and (connect-dialog-p dialog)
                      (eq :provider (connect-dialog-step dialog)))))
    (cond ((eq dialog.kind :model-providers) (setf dialog.title "Models"))
          ((not (connect-dialog-p dialog))
           (setf dialog.title (models-picker-title (length (models-dialog-catalog-rows dialog)) nil
                                                   :auxiliary (eq dialog.kind :auxiliary-models))
                 ;; a list opened before anything was cached said it was loading
                 dialog.empty-label "No models found")))
    (restock-models-dialog model dialog (picker-highlighted-value dialog))))

(defparameter +new-profile-action+ :new-profile
  "The roster's own action row, as its row value: ENTER on it opens the name
field rather than switching anywhere. No profile carries it — a name is a
string, and PROFILE-NAME-P refuses anything else — so one EQL on the value
separates the two kinds of row.")

(defun profile-picker-rows ()
  "One row per profile, over the facts NLK:PROFILE-RECORD already carries:
the name, a mark for the one this shell runs, and, as the detail, the
home with whatever else is true of it — a live organism's pid and port,
`boots by default', the description — and, under them all, the row that
makes a new home."
  ;; The create row rides last: the homes read first, the act after them, and
  ;; the search finds it by `new' or `create' like any other row's words.
  (append
   (loop for record in (nlk:profiles)
         for name = (nlk:profile-record-name record)
         for pid = (nlk:profile-record-pid record)
         for description = (nlk:profile-record-description record)
         collect (make-picker-row
                  :label name
                  :mark (if (nlk:profile-record-current-p record) "*" " ")
                  :mark-tone (if (nlk:profile-record-current-p record) :accent :muted)
                  :detail (format nil "~{~a~^  ·  ~}"
                                  (remove nil
                                          (list (string-right-trim
                                                 "/" (namestring (nlk:profile-record-home record)))
                                                (and pid
                                                     (format nil "serving pid ~d~@[ port ~d~]"
                                                             pid (nlk:profile-record-port record)))
                                                (and (nlk:profile-record-sticky-p record)
                                                     "boots by default")
                                                (and description (plusp (length description))
                                                     description))))
                  :value name))
   ;; The roster's last row: the gesture that makes a home instead of entering one. The
   ;; mark column says what the row does -- a plus, in the tone a row that acts gets --
   ;; and the detail says what lands on disk.
   (list (make-picker-row
          :label "New profile"
          :mark "+"
          :mark-tone :accent
          :detail "make a home of its own, this one's settings and secrets carried over"
          :value +new-profile-action+))))

(defun profile-create-context ()
  "The muted rule line under the create field: what ENTER is about to make. A
refusal replaces it on the open field (PROFILE-CREATE-PICKER-INPUT), which is
why the field is still open with the name still in it."
  (format nil "ENTER makes ~a<name>/ — sharing memory, skills and credentials; Esc goes back to the roster"
          (namestring nlk:*profiles-directory*)))

(defun open-profile-create-picker (model)
  "The roster's create row: one field for a new home's name, in the pane the
roster occupies. ENTER and ESC are PROFILE-CREATE-PICKER-INPUT's, which owns
what each gesture means."
  (open-picker-dialog
   model "Profiles" '() nil
   :kind :profiles
   :context " New"
   :subtitle "a-z, 0-9, `-' and `_' — a home of its own that reads your memory, skills and credentials where they are"
   :input-label "New profile"
   :input ""
   :input-context (profile-create-context)
   :hint "enter create  esc back to the roster"
   :on-input (lambda (model text) (profile-create-picker-input model text)))
  nil)

(defun profile-create-picker-input (model text &aux (dialog model.dialog)
                                                    (name (and text (nlk:trimmed text))))
  "ENTER or ESC on the create field. ESC (TEXT NIL) is the way back to the
roster, making nothing. ENTER makes the home the text names — the same
NLK:PROFILE-CREATE the `create' verb calls, so the clone default and every
refusal are that function's, not this gesture's — and reopens the roster
seated on the new row, where the next ENTER takes this shell there. A refusal
leaves the field open, the words in it, and says why underneath: the gesture
was right, the name needs fixing."
  (if (null text)
      (progn (open-profiles-picker model) nil)
      (handler-case
          (open-profiles-picker
           model :seat name
           :notice (format nil "created ~a at ~a — ENTER takes this shell there"
                           name (string-right-trim "/" (namestring (nlk:profile-create name)))))
        (nlk:profile-error (refusal)
          ;; The field and its words stay; the rule line carries the reason.
          (when dialog
            (setf dialog.input-context (format nil "~a" refusal)))
          nil))))

(defmacro with-profile-refusal (&body body)
  "BODY, with a profile refusal answered as the one line it is."
  `(with-handlers ((nlk:profile-error (refusal) (format nil "profile: ~a" refusal))) ,@body))

(defun profile-picker-delete (model value &aux (roster model.dialog))
  "The roster's Delete / Ctrl+X: confirm whole-home removal, then refresh
in place. The canonical owner refuses protected homes before asking and
rechecks them at confirmation; the create action is never a delete target."
  (when (stringp value)
    (labels ((back (&optional notice)
               (setf (picker-dialog-rows roster) (profile-picker-rows)
                     model.dialog roster)
               (picker-reseat-selection roster value)
               (when notice
                 (setf (picker-dialog-context roster) notice)
                 (show-status model notice))
               nil))
      (handler-case
          (nlk:profile-command (list "delete" value) :command "/profile")
        (nlk:profile-needs-yes (question)
          (declare (ignore question))
          (open-picker-dialog
           model "Delete profile"
           (list (make-picker-row :label "Cancel" :value :cancel)
                 (make-picker-row :label (format nil "Delete ~a" value) :value :delete))
           (lambda-ignoring (model answer)
             (if (eq answer :delete)
                 (back (with-profile-refusal
                         (nlk:profile-delete value)
                         (format nil "deleted ~a" value)))
                 (back)))
           :kind :profiles
           :context (format nil " ~a" value)
           :subtitle "Removes this profile's whole home: store, layer and secrets."
           :hint "enter choose  esc cancel"
           :on-cancel (lambda (model) (declare (ignore model)) (back))))
        (nlk:profile-error (refusal)
          (back (format nil "profile: ~a" refusal))))))
  nil)

(defun open-profiles-picker (model &key seat notice)
  "Bare /profile: the roster as a picker rather than a paragraph, on the
/sessions model."
  ;; Enter runs SWITCH-PROFILE-HERE: the kernel answers the plan
  ;; (PROFILE-SWITCH), the shell's performer carries it, and the shell's own
  ;; gesture ends the loop so CALL-WITH-PROFILE-SWITCH's tail can replace the
  ;; process once the terminal is back. Nothing here execs; the create row's
  ;; ENTER opens a field instead. SEAT is the row to leave the cursor on — the
  ;; home just made — and NOTICE the line to land with it.
  (with-slash-notice ("profile")
    (open-picker-dialog
     model "Profiles" (profile-picker-rows)
     ;; ENTER, one gesture per kind of row: the create row opens the name field, a
     ;; profile's name switches this shell into that home.
     (lambda (model value)
       (if (eql value +new-profile-action+)
           (open-profile-create-picker model)
           (switch-profile-here model.program value)))
     :kind :profiles
     :searchable t
     :context " Homes"
     :search-placeholder "Search profiles"
     :empty-label "No profiles"
     :no-match-label "No matching profiles"
     :on-delete #'profile-picker-delete
     :hint "enter switch/create  del/ctrl+x delete  esc close  up/down navigate  type search")
    (when seat
      (picker-reseat-selection model.dialog seat))
    (when notice
      (show-status model notice)))
  nil)

(defun profile-headless-text (args session-id &aux (tokens (split-words (or args ""))))
  "/profile with no shell to open a picker in — a channel lane, the model's
eval, the headless surface: PROFILE-COMMAND over the tail, and bare is the
roster text it always was, since Enter on a row is a gesture only a
terminal has."
  ;; `switch' is declined here rather than performed. The verb is the same one
  ;; either way; what differs is that this process is a gateway serving every
  ;; attached shell, and replacing its image would take all of them with it.
  ;; The decline says where the gesture lives and what moves a home from here.
  (declare (ignore session-id))
  (if (and tokens (string-equal (first tokens) "switch"))
      (format nil "profile: switch relaunches the terminal it is typed in, ~
                     and this organism has none. Run `/profile switch ~a' in ~
                     a shell, or `/profile use ~:*~a' to move the next launch."
              (or (second tokens) "NAME"))
      (with-profile-refusal (nlk:profile-command (or args "") :command "/profile"))))

(defun switch-profile-here (program name)
  "Run NAME in this shell, now: hand the plan to whatever performer this
shell installed — a TUI's parker records it for CALL-WITH-PROFILE-SWITCH's
tail to perform — then end the loop, because the terminal only comes back
when the loop ends and only a process that has given it back may be
replaced."
  ;; The gesture sends the quit as well as the parker does, and that is the
  ;; point rather than a duplicate: a parker is a closure created when the
  ;; shell started, so a shell whose image predates the parker's own send can
  ;; never be handed it — while a definition reaches a running shell live.
  ;; A refusal answers as text: nothing is parked and no loop
  ;; ends.
  (with-profile-refusal
    (multiple-value-bind (binary argv) (nlk:profile-switch name)
      (funcall nlk:*profile-switch-performer* name binary argv)
      (when program (nlt:send program (nlt:make-quit-msg)))
      (format nil "switching to ~a: this shell comes back in that home" name))))

(defun profile-slash-op (model args &aux (tokens (split-words (or args ""))))
  "/profile's answer."
  ;; Bare opens the picker — a roster you can walk and press Enter on, which
  ;; is the gesture the text roster never was; with a verb it is
  ;; PROFILE-COMMAND over the tail, a refusal as one line rather than a
  ;; condition. `switch NAME' is the one verb the shell performs itself,
  ;; through SWITCH-PROFILE-HERE, because moving this shell needs the loop
  ;; that paints it. Either way the machine's homes, not a session's state: it
  ;; runs in this shell's own image on both shells, like /cells.
  (cond
    ((null tokens) (open-profiles-picker model))
    ((and (string-equal (first tokens) "switch")
          (second tokens)
          (null (cddr tokens)))
     (switch-profile-here model.program (second tokens)))
    (t (with-profile-refusal (nlk:profile-command args :command "/profile")))))

(defun gateway-live-facts (state)
  "What the organism serving STATE's store says of itself — its build,
when it started, the lines on its standing board — as the plist
NLK:GATEWAY-REPORT reads, or NIL when it does not answer."
  ;; One GET of the identity route under this home's operator token, bounded
  ;; in seconds: a state printed on a wedged gateway must cost the operator a
  ;; moment, not half a minute.
  (when-let (port (nlk:gateway-state-owner-port state))
    (ignore-errors
     (let ((identity (gateway-http-session-request
                      port :get "/api/gateway/identity" nil
                      (gateway-configured-operator-token)
                      :connect-timeout 2 :read-timeout 3)))
       (list :version (json-value identity :string "version")
             :binary (json-value identity :string "build")
             :started-at (json-value identity :integer "started_at")
             :notices (loop for notice across (json-array identity "notices")
                            when (stringp notice) collect notice))))))

;;; --- /context gathering -----------------------------------------------------
;;; The one place the shells and the headless surface turn live state plus
;;; the window belief into /context's inputs; the renderer is slash.lisp's,
;;; pure.

(defun context-window-view (provider model-id)
  "The window belief for (PROVIDER, MODEL-ID) => (values WINDOW GRADE FLOOR):
GRADE `learned' means durable observations moved it; FLOOR, the largest
prompt proven. NIL when either is not a string."
  ;; /context and the rows a browser is sent both read it.
  (when (and (json-value provider :text) (json-value model-id :text))
    (bind ((prior (model-context-tokens model-id))
           (operator-p (and prior (model-context-tokens-operator-p model-id)))
           (window (context-window-belief provider model-id prior operator-p))
           ((floor ceiling) (and (nlk:store-open-p)
                                 (nlk:context-window-observation provider model-id))))
      (values window
              (cond ((null window) "unknown")
                    (operator-p "configured")
                    ((or (and floor (plusp floor)) (and ceiling (plusp ceiling)))
                     "learned")
                    (t "default"))
              floor))))

(defun context-report-text (provider model-id &rest report
                            &key session-id context-usage &allow-other-keys)
  "RENDER-CONTEXT-REPORT over REPORT — the :LABEL, :SESSION-ID and usage
accumulators a caller gathered — with the window belief for (PROVIDER,
MODEL-ID) and, while CONTEXT-USAGE holds no round, the occupancy estimated
from the session's durable record."
  ;; The shells and the headless surface both answer through it.
  (multiple-value-bind (window grade floor) (context-window-view provider model-id)
    (multiple-value-bind (context-estimate context-parts)
        (and (null context-usage)
             session-id
             (session-context-estimate session-id provider model-id))
      (apply #'render-context-report
             :context-estimate context-estimate
             :context-parts context-parts
             :window window
             :window-grade grade
             :window-floor floor
             report))))
(defun execute-slash-command (model name args &aux (entry (find-builtin-slash-command name))
                                                   (op (and entry entry.op)))
  "Execute the slash command NAME (canonical, slash-less) with raw ARGS on
MODEL's transport."
  ;; Returns (values notice cmd): NOTICE is the live-only result text for the
  ;; composer status (NIL for none), CMD a program command to run after the update
  ;; (NIL for none). The gateway-backed ops call the HTTP control ops.
  ;;
  ;; The catalog's DISPATCH slot says how the shell calls the command's op
  ;; (slash.lisp SLASH-COMMAND); a NIL slot is a command spelled out below.
  (case (and entry entry.dispatch)
    (:plain (values (funcall op model) nil))
    (:args (values (funcall op model args) nil))
    (:name (values (funcall op model name) nil))
    ;; The Zig usage_help argument gate answers before the op runs.
    (:guarded (values (or (slash-no-args-guard name args) (funcall op model)) nil))
    ;; Local-process introspection on either shell (/gc): the facts
    ;; describe THIS image — its collections — not a session's state.
    (:local (values (funcall op args model.session-id) nil))
    (t
     (cond
       ((string= name "exit") (values nil (nlt:quit-cmd)))
       ((string= name "models")
        ;; Bare /models opens the providers under the Main and Auxiliary
        ;; tabs; with no catalog it reports the active selection. /models
        ;; refresh forgets every fetched listing first, so the open
        ;; refetches behind the list, and /models list answers the
        ;; catalog as text — the shape a room gets, now on both shells.
        ;; `aux' leads the same forms for the auxiliary model: bare, the
        ;; list on its Auxiliary tab; a model lands this image's auxiliary
        ;; target and general.auxiliary_model together, pinning no session;
        ;; `default' takes it out of both.
        ;; `decider' leads the decider's: bare, its tab; a decider id pins
        ;; it in general.decider_model, `none' takes it out.
        (let* ((tokens (split-words args))
               (seat (find (first tokens) '("aux" "decider") :test #'equalp))
               (aux (equalp seat "aux"))
               (picks (if seat (rest tokens) tokens)))
          (cond
            ;; `backups' opens the Backup tab: the failover chain is set there.
            ((and (not seat) (member (first tokens) '("backup" "backups") :test #'equalp))
             (if (rest tokens)
                 (values (slash-usage name) nil)
                 (open-models-provider-picker model :tab "backup")))
            ((> (length picks) 2) (values (slash-usage name) nil))
            ((and (not seat) (equalp (first tokens) "list"))
             (values (model-catalog-list-text (second tokens)) nil))
            ((or (null picks) (and (not seat) (equalp picks '("refresh"))))
             (when picks (drop-provider-models-cache (listable-provider-ids)))
             (open-models-provider-picker model :tab (if seat (string-downcase seat) "main")))
            (aux (values (slash-auxiliary-model-op model picks) nil))
            (seat (values (slash-decider-model-op model picks) nil))
            (t (values (slash-models-op model picks) nil)))))
       ((string= name "evict")
        (multiple-value-bind (percent through valid-p) (evict-args args)
          (values (if valid-p (slash-evict-op model percent through) *evict-usage-hint*)
                  nil)))
       ((member name '("gateway" "web") :test #'string=)
        ;; The machine's gateway, not a session's state: local, like /profile.
        ;; Its verbs wait on systemd — an off waits on the gateway's own unwind
        ;; — so the answer comes from a thread of its own, and the shell keeps
        ;; painting meanwhile.
        ;; `nodecode web' on the shell: WEB-TEXT's answer in the composer
        ;; status, which is live-only, so the operator token in the link reaches this
        ;; terminal and never the transcript. Off the loop, like /gateway:
        ;; the answer asks the gateway, which may be this image's own.
        (let ((program model.program))
          (spawn (format nil "~a-slash" name)
            (handler-case
                (multiple-value-bind (text error-p)
                    (if (string= name "gateway")
                        (values (with-handlers ((nlk:gateway-error (refusal)
                                                  (format nil "gateway: ~a" refusal)))
                                  (nlk:gateway-text (or args "") :command "/gateway"
                                                                 :live #'gateway-live-facts))
                                nil)
                        (bind (((text _ stream) (web-text (split-words args))))
                          (values text (eq stream :error))))
                  (post-shell-notice program text :error-p error-p :answer t))
              (error (condition)
                (ignore-errors
                 (post-shell-notice program (format nil "~a: ~a" name condition) :answer t)))))
          (values nil nil)))
       ((string= name "context")
        ;; The organism answers it (its headless arm, over POST
        ;; /api/gateway/slash): the occupancy estimate and the learned
        ;; window read its store, which no shell's process holds.
        (values (run-gateway-command model (find-builtin-slash-command "context") args)
                nil))
       (t
        ;; A token no builtin claims may be a cell's command, which runs
        ;; at the gateway with its verbatim argument tail.
        (let ((entry (find-gateway-command name)))
          (values (if entry
                      (run-gateway-command model entry args)
                      (format nil "unknown command /~a" name))
                  nil)))))))

(defun evict-args (args)
  "The parse of /evict's ARGS => (values PERCENT THROUGH VALID-P), the Zig
executeEvictSlashCommand's and the turn form: bare /evict sends neither;
`40` and `40%` both read a percent (the trailing sign is what a user types),
0 and >100 are not valid; `through TURN' names the turn id eviction runs
through. What is not valid renders the usage hint rather than a request."
  (let* ((trimmed (string-trim " " args))
         (through (and (uiop:string-prefix-p "through " trimmed)
                       (string-trim " " (subseq trimmed 8))))
         (percent (and (null through)
                       (ignore-errors (parse-integer (string-right-trim "%" trimmed))))))
    (cond ((zerop (length trimmed)) (values nil nil t))
          (through (values nil through (plusp (length through))))
          (t (values percent nil (and (integerp percent) (< 0 percent 101)))))))

(defun footer-right-candidates (model &aux (port model.http-port))
  "Right-lane segment candidates, longest first. Shed workspace/profile
before the gateway endpoint; its lifecycle is color, never a text suffix. The
caveats a setup walk ended on (SETUP-ISSUE-SEGS) lead each and are shed last."
  (let* ((workspace (workspace-footer-label))
         (profile (nlk:profile-label))
         (separator (make-seg :text " · " :tone :muted))
         ;; what the last setup walk ended on, never the first thing shed
         (issues (setup-issue-segs model)))
    (flet ((part (text target) (hover-seg model text target)))
      (let* ((lead (append (when (and profile (plusp (length profile)))
                            (list (part profile (cons :command "profiles"))))
                          (when (plusp (length workspace))
                            (append (and profile (list separator))
                                    (list (part workspace (file-uri (workspace-directory))))))))
             ;; The setup walk names no gateway before an endpoint means anything.
             (candidates
               (if (and port (not model.setup-walk))
                   ;; The page as it opens, on Sessions (the operator, 2026-10-21:
                   ;; "Clicking gateway should ... go to the SESSIONS part").
                   (let* ((target (format nil "http://127.0.0.1:~d/web/~@[#t=~a~]"
                                          port (and model.http-token (quri:url-encode model.http-token))))
                          (endpoint (hover-seg model "Gateway" target
                                               (ecase model.sync-lifecycle
                                                 (:connected :success)
                                                 ((:connecting :reconnecting) :tool)
                                                 (:failed :danger)))))
                     (list (append lead (and lead (list separator)) (list endpoint))
                           (list endpoint)))
                   (and lead (list lead)))))
        ;; The nodecode.ai meter (account-login.lisp) leads while it fits: it is shed after the
        ;; profile and folder, before the gateway.
        (when-let (meter (account-meter-segs model))
          (setf candidates (append (loop for candidate in candidates
                                         collect (append meter (list separator) candidate))
                                   (last candidates))))
        (if issues
            (append (mapcar (lambda (candidate) (append issues (list separator) candidate)) candidates)
                    (list issues))
            candidates)))))


(defun live-status-hint (model)
  "Keyboard affordances for the live status verb's hover tooltip: the viewport
control, or the empty string when it is unbound. Esc stands on the bar itself
and the steer keys in the composer (COMPOSER-ROWS), so the tooltip no longer
repeats them."
  (or (tool-detail-hint model) ""))

(defun turn-stopping-p (model)
  "Whether Esc asked the turn in flight to stop and no terminal fact has
settled it yet."
  (and model.turn-in-flight model.stop-requested
       (equal model.stop-requested model.active-turn-id)))

(defun fetch-gateway-model-facts (model)
  "Dial the gateway for MODEL's session model facts => the plist, or NIL
(the empty plist) on any refusal."
  ;; One call answers identity, pin provenance and reasoning effort because
  ;; the gateway resolves all three against the same session-effective target;
  ;; an attached client has no store to ask instead. Never called on the loop
  ;; thread — the frame renders the cache; REFRESH-GATEWAY-MODEL-FACTS fills
  ;; it from a thread of its own.
  (ignore-errors (nth-value 3 (target-snapshot model))))

(defun gateway-model-facts (model &aux (facts model.model-facts))
  "The gateway's model facts for this client's session, read from the
MODEL-FACTS cache => a plist (:provider :model :pinned :effort), empty
until a fetch lands."
  ;; NEVER dials: the footer renders every frame on the loop thread, and a
  ;; dial there would stall the whole shell for the connect timeout whenever
  ;; the endpoint is dead. The connect ack queues READ-MODEL-FACTS-CMD on the
  ;; worker; the control ops that move the target refresh in place from their
  ;; own off-loop contexts.
  (if (eq facts :unresolved) '() facts))

(defun publish-gateway-model-facts (model facts epoch)
  "Land FACTS in MODEL's cache — newest EPOCH only — the moment a read
answered. => T when it landed."
  ;; One slot, set to a value the reader owns, read by the frame path: the
  ;; write is a single pointer and the epoch keeps an older read from landing
  ;; over a newer one. Everything a FRAME needs is here; everything a NOTICE
  ;; needs waits for the loop.
  (when (and facts (= epoch model.facts-epoch))
    (setf model.model-facts facts)
    t))

(defun refresh-gateway-model-facts (model)
  "Refetch MODEL's gateway facts on a thread of their own and install the
answer on the loop (the WRITE-CLIPBOARD shape: never the loop thread,
where a dial would stall every frame, and never the
worker, where a wedged gateway would park quit and every queued command
behind a 30 s read)."
  ;; The epoch captured at spawn keeps a slow fetch from overwriting newer
  ;; facts. A shell with no loop to install them on reads nothing at all.
  (let ((epoch (incf model.facts-epoch)))
    ;; The /models list rides the same moments: a port known, a model moved.
    (start-models-picker-warm model)
    (when model.program
      (off-loop model
                (lambda (&aux (facts (fetch-gateway-model-facts model)))
                  ;; PUBLISHED HERE, where the answer lands, not on the loop
                  ;; alone: the launch frame is painted before the loop drains
                  ;; its queue, so a value that only arrives as a message
                  ;; cannot be in the frame that first shows the footer.
                  ;; The loop keeps
                  ;; the other half — the notice it owes and the repaint
                  ;; (INSTALL-GATEWAY-MODEL-FACTS).
                  (publish-gateway-model-facts model facts epoch)
                  facts)
                (lambda (model facts) (install-gateway-model-facts model facts epoch)))))
  nil)

(defun install-gateway-model-facts (model facts epoch)
  "Install the facts a refresh read — newest EPOCH only — and repaint the
footer."
  (when (= epoch model.facts-epoch)
    ;; A NIL answer is a read that never landed — the control surface
    ;; refused, timed out, or the session did not exist yet — and that is no
    ;; evidence about the target: the cache keeps what it held, so
    ;; :UNRESOLVED still reads `setting up…' and an earlier answer stays the
    ;; last one the gateway confirmed. Installing the empty value instead
    ;; would render as a shell that has no model at all.
    (when facts
      (let ((before model.model-facts)
            (refusal (getf facts :refusal)))
        ;; Idempotent when the reader already published it.
        (publish-gateway-model-facts model facts epoch)
        ;; A target that cannot serve says so the moment its facts land —
        ;; at attach, and again whenever the target moves and is still
        ;; refused — never on a mere refresh of the same refused facts.
        (when (and refusal
                   (not (and (listp before) (equal refusal (getf before :refusal)))))
          (show-status model refusal :error-p t)))
      ;; The one landmark that says when the identity became knowable: an
      ;; attach whose facts land after the first frame is the case this path
      ;; exists to measure (NODECODE_PHASE_LOG).
      (nlk:note-boot-phase "model-facts" (getf facts :model))
      ;; A model chosen is measured once, before anyone prompts it (connection.lisp).
      (check-connection model)
      (render-state model))))

(defparameter *first-frame-facts-settle-ms* 25
  "How long the launch frame may wait for a model-facts read that is already
in flight (SETTLE-BEFORE-FIRST-FRAME). Bounded on purpose: the frame this
precedes is the operator's first screen, and a launcher's hand-over waits on
it.")

(defmethod settle-before-first-frame ((model gateway-chat-model))
  "Hold the launch frame for the facts read, briefly, when one is coming."
  ;; The footer's identity is the one fact this shell cannot resolve itself:
  ;; it belongs to the gateway, and the read that fetches it is already out
  ;; (the ladder's preload, which fires as soon as the session exists).
  ;; Waiting for it here keeps `setting up…' off the screen entirely; without
  ;; this, a frame painted before the identity arrives would be a flash the
  ;; operator reads as a stall. Three things bound the wait: only an
  ;; UNRESOLVED cache waits at all (a shell that already knows its target
  ;; paints at once), only a known endpoint waits (an owner still booting its
  ;; gateway has nothing to read from and paints at once), and the deadline
  ;; is 25 ms whatever the peer does — a gateway that never answers costs the
  ;; launch frame 25 ms and nothing more.
  (when (and model.http-port
             (eq model.model-facts :unresolved))
    (wait-until (lambda () (not (eq model.model-facts :unresolved)))
                (deadline-after (/ *first-frame-facts-settle-ms* 1000)) 0.001)))

(defun refresh-gateway-command-catalog (model)
  "Re-read the cell commands MODEL's gateway serves on a thread of its own
and post the answer to the shell (the model-facts shape: never the loop
thread, where a dial stalls every frame, and never the worker)."
  ;; GET /api/gateway/slash stays the authority; the COMMANDS_CHANGED frame
  ;; is the cue that it moved.
  (when-let (program model.program)
    (spawn "command-catalog-fetch"
      (when-let (msg (funcall (read-command-catalog-cmd model))) (nlt:send program msg)))))

(defun invalidate-gateway-model-facts (model)
  "Drop the cached model facts — and advance the epoch, so any fetch
already in flight cannot install what it read before the invalidation."
  ;; Called wherever this client changes the target or learns the gateway may
  ;; have moved under it: a fresh connection, a session switch, a landed model
  ;; selection, a landed effort pick.
  (incf model.facts-epoch)
  (setf model.model-facts :unresolved))

(defun footer-gesture-hint (model &aux (running (hash-table-count (session-activity-table model))))
  "A key hint the footer advertises beside the model, or NIL: the one gesture a
shell has that no slash command spells. `← sessions' whenever this shell
holds an attached session -- which for the gateway shell is every frame, home
screen included: Left answers LEAVE-SESSION-OP from home the same as from a
transcript, so the gesture is discoverable where the operator's eye already
rests, both screens."
  ;; With sessions running the hint counts them -- `← sessions · 2 running' --
  ;; read off the running-session map this shell holds (organism.activity):
  ;; what Left would show at work. The count rides beside the word, never in
  ;; its place: `← 1 running' left unsaid that the arrow is a key and the
  ;; count is sessions.
  (format nil "← sessions~[~:; · ~:*~d running~]" running))

(defun footer-identity (model)
  "The footer's provider/model identity => (values PROVIDER MODEL-ID STATE),
STATE one of :CONFIGURED, :UNCONFIGURED (nothing chose the target) or
:PENDING (this shell has not learned its target yet). The gateway footer
mirrors the turn binding (RUN's session-selection overlay)."
  ;; STATE is :UNCONFIGURED when nothing chose the target
  ;; (DEFAULT-MODEL-CONFIGURED-P): it is the image's build-time default, no
  ;; provider was connected for it, and the footer shows how to get a model
  ;; instead of naming it. :PENDING is no licence to guess: the slot says the
  ;; shell is still setting up rather than naming a lane no turn here would
  ;; bind. Without this, a paint-first launch would name the build-time lane
  ;; for its first frames. The durable per-session pin rides the overlay, so a
  ;; /models selection renders the lane the next turn will bind.
  ;;
  ;; The gateway computes that overlay — its own effective default with each
  ;; pin field on top, and whether anything chose it — and the client renders
  ;; exactly what it answered, so an attached client shows the running
  ;; organism's model rather than its own unrestored process defaults. Until an
  ;; answer lands the state is :PENDING: this cache is the one authority for
  ;; this shell's target, and neither the process globals nor a stale answer
  ;; may stand in for it.
  (if (eq model.model-facts :unresolved)
      (values nil nil :pending)
      (let ((facts (gateway-model-facts model)))
        (values (getf facts :provider) (getf facts :model)
                (if (getf facts :unconfigured) :unconfigured :configured)))))

(defun footer-reasoning-effort (model)
  "The reasoning-effort preference for the footer target, or NIL — the Zig
footerReasoningEffort input to appendReasoningEffort. The /think choice rides
the identity it belongs to: both came out of one target read resolved
against this session, so the gateway shell never asks a
store it may not own."
  (getf (gateway-model-facts model) :effort))

(defun chat-begun-p (model)
  "Whether MODEL's chat has begun: a transcript row, a live turn, or a submit
in flight (the Zig turn_command-projection-empty gate). Until it has, the
shell is on its home screen, a dialog over it or not."
  (or (find-transcript-row (constantly t) model.state)
      model.turn-in-flight
      model.pending-command-id))

(defun home-screen-p (model)
  "Whether the landing/home screen is active (Zig screens.zig isHomeScreen
distilled to the state this shell carries): no chat begun, no notice, and no
dialog but the setup walk's."
  ;; Without this, a fresh box's first frame would be the walk's dialog under a
  ;; blank band and a raw session id, not the landing screen. A draft is typed
  ;; on it, as on the launch's: the screen goes with the submit, not the first
  ;; key.
  (and (not (chat-begun-p model))
       (or (null model.dialog) model.setup-walk)))

(defun home-prompt-text (model)
  "The composer placeholder while home is active (Zig screens.zig
homePromptText): the connection narrative, the Zig table over the lifecycle
states this transport inhabits."
  ;; The Zig "Creating a Gateway session..." arm never arises: this client
  ;; always attaches with a resolved session. Connected is only the
  ;; invitation: the footer's Gateway, in its connected colour, says the rest.
  (case model.sync-lifecycle
    (:connected "Ask anything, or describe a task")
    ((:connecting :reconnecting) "Connecting to Gateway...")
    (:failed "Gateway sync failed - check the endpoint and retry")))

(defun checkpoint-listing-path (model)
  "The control path the tree picker fetches its rows from for MODEL's session,
or NIL when this shell has no session to ask about."
  ;; One writer of the string so the prefetch and the request it serves are
  ;; compared as equals — SLASH-TREE-OP composes the same path, and a shipped
  ;; change to it degrades to a prefetch miss (a fresh dial), never to a wrong
  ;; listing.
  ;;
  ;; No limit: the tree is the whole session. A row window read as a tree with
  ;; its oldest turns missing, which is what the operator saw (/tree beginning
  ;; at turn 11 while turn 1 sat 146 rows below the window's floor).
  ;;
  ;; The session id is read defensively: GATEWAY-MODEL-HTTP-CALL asks this of
  ;; every control call, and the inherited CHAT-MODEL slot can be genuinely
  ;; unbound on a client that has not finished attaching (the same unbound
  ;; slot that once crashed the shell header). A shell with no readable
  ;; session simply has no listing to prefetch.
  (when-let (session (json-value (ignore-errors model.session-id) :text))
    (format nil "/api/gateway/sessions/~a/checkpoints" session)))

(defun gateway-model-http-call (model method path body &key raw stream (read-timeout 30))
  "One control call for MODEL; RAW and READ-TIMEOUT as
GATEWAY-HTTP-SESSION-REQUEST takes them."
  ;; The tree picker's checkpoint listing is served from the prefetch the
  ;; arming Esc started when one is waiting for exactly this path; every other
  ;; call, and every miss, dials the gateway here.
  ;; Named here, once: every sender takes the shell's own gateway model, and
  ;; a model id handed in its place read as a NO-APPLICABLE-METHOD on the
  ;; port reader.
  (check-type model gateway-chat-model
              "an attached shell's gateway model; a snippet has none, and (help :operator) names the verbs that land the same facts")
  (or (and (eq method :get) (null body)
           (equal path (checkpoint-listing-path model))
           ;; Consumed fresh or stale; one in flight is this request, already started.
           (let ((entry *checkpoint-listing-prefetch*))
             (when (and entry (equal path (getf entry :path)))
               (setf *checkpoint-listing-prefetch* nil)
               (when (< (get-internal-real-time) (getf entry :until))
                 ;; Waiting on the started request beats dialing; 1 s bounds a slow gateway.
                 (let ((box (getf entry :box)))
                   (unless (car box)
                     (bt2:wait-on-semaphore (getf entry :semaphore) :timeout 1))
                   (car box))))))
      (gateway-http-session-request model.http-port
                                    method path body
                                    model.http-token :raw raw :stream stream
                                    :read-timeout read-timeout)))

;;; --- HTTP control senders ---------------------------------------------------
;;; Thin typed callers over the gateway's request-reply control ops; each
;;; returns the decoded JSON object (GATEWAY-HTTP-ERROR on refusal). A dialog
;;; or slash surface composes these; nothing here renders. Every one takes the
;;; MODEL its caller already holds and rides GATEWAY-MODEL-HTTP-CALL, so the
;;; port/token pair is named once, in that function, and never plumbed by
;;; hand at a call site.

(defun gateway-sentence (model path body)
  "POST BODY, an object, to PATH for MODEL => the sentence the organism
answered, or why it refused."
  (handler-case (answer-text (gateway-model-http-call model :post path body))
    (gateway-http-error (refusal) (gateway-http-error-detail refusal))))

(defun session-control-call (model method op &key body (session-id model.session-id))
  "One control call under a session's path: METHOD on
/api/gateway/sessions/<SESSION-ID>/OP — this shell's own session unless one
is named — with BODY (a JSON object, or NIL)."
  (gateway-model-http-call
   model method (format nil "/api/gateway/sessions/~a/~a" session-id op) body))

;;; --- the /models provider catalog ------------------------------------------
;;; The list the wizard opens on is the models.dev provider catalog, not the
;;; four SDK lanes this image happens to register: the Zig dialog fills it
;;; from the gateway's models catalog response
;;; (models_catalog.zig CONNECTDIALOGCATALOGFROMGATEWAY — one row per
;;; provider, the catalog display name as the label and `N models` as the
;;; detail), and this shell reads the models.dev cache file every process
;;; keeps current (provider.lisp MODELS-CATALOG-TABLE). No gateway round-trip is
;;; involved on either half: the save side is client-side too, entering
;;; SAVE-PROVIDER-API-KEY into the shared ~/.nodecode/auth.json exactly like
;;; the Zig TUI's persistConnectDialogApiKey.
;;;
;;; Two additions over the Zig rows: a provider this machine can already
;;; authenticate says so after the count, with its credential source, and
;;; once one is connected it heads the list under Suggested, so the list
;;; also answers "which of these am I connected to" and a saved key returns
;;; to it: an operator connects every provider they have a key for, then
;;; Enter on one goes on to its models. The count copy is the Zig one
;;; verbatim.

;;; --- /models ---------------------------------------------------------------
;;; The wizard state machine lives in ui.lisp (CONNECT-DIALOG); this op
;;; injects the transport-facing halves: the provider rows, the credential
;;; sink (SAVE-PROVIDER-API-KEY into the shared auth.json — client-side on
;;; both shells, exactly like the Zig persistConnectDialogApiKey), and what
;;; Enter on a connected provider opens (the models dialog, the Zig
;;; post-connect flow).

(defparameter +connect-pinned-providers+
  '(("anthropic" . "console.anthropic.com/settings/keys")
    ("openai" . "platform.openai.com/api-keys")
    ("openrouter" . "openrouter.ai/settings/keys")
    ("google" . "aistudio.google.com/apikey")
    ("deepseek" . "platform.deepseek.com/api_keys")
    ("moonshotai" . "platform.moonshot.ai/console/api-keys")
    ("zai" . "z.ai/manage-apikey/apikey-list"))
  "The providers the /models list opens on, in this order, each with the
page that makes its API key; the rest follow by name.")

(defun provider-id-refusal (id)
  "Why this build cannot serve the provider ID, in words the operator reads,
or NIL when a lane drives it."
  ;; Asked before a /models key is: a key saved for a provider no lane speaks
  ;; is a secret in auth.json that nothing can use; without a lane, a
  ;; /models pick would be refused. CONFIGURED-PROVIDER-LANE is the
  ;; resolution a turn runs, so what this accepts is what a turn can dial.
  ;; A key typed where the id goes is refused before any lane is asked, and
  ;; never echoed: a configured one has a lane.
  (if (key-shaped-p id)
      "That reads as an API key, not a provider id: pick the provider from the list, and the key step comes after it."
      (unless (configured-provider-lane id)
        (let ((catalog (models-catalog-table)))
          (cond ((configured-provider-entry id) (selection-refusal id))
                ((and (hash-table-p catalog) (json-value catalog :object id))
                 (format nil "~a is in models.dev, but this build has no lane for its API yet. ~
                              To use it, add providers.~a with an sdk and a base_url to config.jsonc."
                         id id))
                ((null catalog) (format nil "The models.dev list is not loaded yet, so ~a cannot be checked. ~
                                             Try again in a moment." id))
                (t (format nil "~a is not a models.dev provider and no providers.~a is configured. ~
                                Check the id at models.dev."
                           id id)))))))

(defvar *key-checks* (make-hash-table :test 'equalp)
  "By provider id, (VERDICT WORDS) when the key saved for it was checked and
did not answer: a PROVIDER-KEY-CHECK verdict -- :UNCHECKED, :REFUSED,
:UNREACHABLE -- of the key /models saved, or of the import's check of its
target (SETUP-IMPORT-VERIFY), and a sentence naming the provider and what
happened. A key that answered, or that nothing asked about, has no entry.")

;;; Live-only: the list and the walk's step line read it, and a restart knows
;;; what it checks again. Without this, a key saved over an unanswered check
;;; would read `connected' beside the notice that said so.
(defun key-check-state (provider)
  "How the connect list names PROVIDER: `needs attention' when no turn can
run on it (SELECTION-REFUSAL), whatever its key; else, when its key's last
check did not answer, `not checked', `key refused' or `not answering'; else
NIL."
  ;; Without this, a provider entry with no base_url would read `acme connected'
  ;; and `acme not checked' when a key typed for it is saved over the refusal.
  ;; A shell asks the gateway's answer: a cell's lane runs there only.
  (if (provider-turn-refusal provider)
      "needs attention"
      (case (first (gethash provider *key-checks*))
        (:unchecked "not checked")
        (:refused "key refused")
        (:unreachable "not answering"))))

(defun connect-picker-rows ()
  "The provider-step rows: once a provider is connected
(CONNECTED-PROVIDER-IDS), a Suggested heading over the connected
providers, then the pinned providers, then every other
models.dev provider the cached catalog carries that a lane can drive -- a
catalog package one serves, or a connected provider -- then the Zig `Other
provider` entry (setCatalog appends it after the gateway rows, so it closes
the list on both shells)."
  ;; A new operator meets 302.AI, Abacus and abliteration.ai first, and every
  ;; provider whose package no lane speaks, which a turn could only refuse.
  ;; Without a catalog cache only that entry survives; its id step needs the
  ;; catalog too, and says so (PROVIDER-ID-REFUSAL) rather than saving a key
  ;; no lane could use.
  ;; Sorted by display name then id: Zig walked a map order a Lisp hash table lacks.
  ;; A provider an uninstalled folder serves is listed too, and picking it
  ;; installs that folder (PICKER-PROVIDER-OFFERS).
  (let ((catalog (models-catalog-table))
        ;; The ids worth a PROVIDER-AUTH-STATE probe, which opens a file per
        ;; call.
        (connected (picker-connected-provider-ids))
        (listable (picker-listable-provider-ids))
        (model-rows (models-catalog-picker-rows))
        (rows '()))
    (labels ((member-p (id ids) (member id ids :test #'string-equal))
             (provider (row) (getf (picker-row-value row) :provider))
             (models (id)
               (count-if (lambda (row &aux (value (picker-row-value row)))
                           (and value (string-equal (first value) id)))
                         model-rows))
             (row (id label state detail)
               (push (make-picker-row
                      :label label :detail detail
                      ;; CONNECTED: the row reads `connected', and Enter
                      ;; on it goes on with the key it has (CONNECT-DIALOG-KEY)
                      :value (list :provider id
                                   :key-page (cdr (assoc id +connect-pinned-providers+
                                                         :test #'string-equal))
                                   :connected (and (null (key-check-state id))
                                                   (or (not (eq state :none))
                                                       (member-p id connected)
                                                       (member-p id listable)))))
                     rows))
             (pin (row)
               (position (provider row) +connect-pinned-providers+ :key #'car :test #'string-equal))
             (connected-p (row) (member-p (provider row) connected)))
      (when (hash-table-p catalog)
        (loop for provider-id being the hash-keys of catalog using (hash-value provider)
              for id = (and (stringp provider-id)
                            (nlk:trimmed provider-id))
              for listed = (or (member-p id connected) (member-p id listable))
              ;; The count is the models a turn can run on (CATALOG-MODEL-TURN-P),
              ;; the ones /models lists: the row said `OpenAI  52 models' over a
              ;; picker of 42, and a beta tester asked where ten went. A provider
              ;; with none leads nowhere, and is not offered unless it is already
              ;; connected.
              for count = (if listed
                              (models id)
                              (loop for model being the hash-values of
                                      (or (json-value provider :object "models") (make-hash-table))
                                    count (catalog-model-turn-p model)))
              when (and id (plusp (length id)) (hash-table-p provider)
                        (or listed
                            (and (catalog-npm-lane-name (json-value provider :text "npm"))
                                 (plusp count))))
                do (let ((state (if (member-p id connected) (provider-auth-state id) :none)))
                     (row id (or (json-value provider :text "name")
                                 (json-value provider :text "display_name")
                                 id)
                          state
                          ;; The Zig providerModelCountDetail copy, then
                          ;; where a connected provider's credential comes
                          ;; from: `connected (auth.json)', `connected (env)',
                          ;; or `not checked (auth.json)' for a key nothing
                          ;; answered (KEY-CHECK-STATE).
                          (if (eq state :none)
                              (format nil "~d model~:p" count)
                              (format nil "~d model~:p  ~a (~(~a~))" count
                                      (or (key-check-state id) "connected")
                                      (if (eq state :api-key) "auth.json" state)))))))
      ;; Configured and local providers need a row even without a models.dev entry.
      ;; Their models and listing failures still come from the one model catalog.
      (dolist (id (union connected listable :test #'string-equal))
        (unless (find id rows :test #'string-equal :key #'provider)
          (let ((state (if (configured-provider-lane id) (provider-auth-state id) :none)))
            (row id (or (json-value (configured-provider-entry id) :text "display_name")
                        (picker-provider-name id)
                        id)
                 state
                 (format nil "~d model~:p  ~a" (models id)
                         (or (key-check-state id)
                             (if (eq state :none) "configured" "connected")))))))
      ;; A provider a folder not installed here serves (PROVIDER-OFFERS):
      ;; Enter installs the folder first (CONNECT-INSTALL). Its models.dev row,
      ;; when it has one, becomes the folder's -- a key saved from that row
      ;; alone reached a lane that cannot talk to it (OpenCode Go answers 400
      ;; MissingSessionID) -- and one models.dev lacks (ChatGPT's Codex) is
      ;; listed by the name its folder gives it. The row says nothing of the
      ;; install: picking it is the whole of it ("less is more").
      (dolist (offer (picker-provider-offers))
        (let* ((id (getf offer :provider))
               (held (find-if (lambda (row)
                                (and (string-equal id (provider row))
                                     (not (getf (picker-row-value row) :install))))
                              rows)))
          (if held
              (setf (picker-row-value held) (list* :install offer (picker-row-value held)))
              (push (make-picker-row :label (getf offer :name)
                                     :value (list :provider id :install offer))
                    rows))))
      (let* ((sorted (stable-sort (sort rows #'string-lessp :key #'provider)
                                  (lambda (a b &aux (at (pin a)) (bt (pin b)))
                                    (cond ((and at bt) (< at bt))
                                          (at t)
                                          ((not bt) (string-lessp (picker-row-label a) (picker-row-label b)))))))
             (others (remove-if #'connected-p sorted))
             (pinned (count-if #'pin others)))
        (append
         ;; Without this row, a gateway that does not report its providers
         ;; would leave every saved key reading as gone: a key intact in
         ;; auth.json would be listed with a model count but nothing connected.
         (when (and (not *serving*) (getf *gateway-models* :unreported))
           (list (make-picker-row
                  :label (format nil "Saved keys unknown: the gateway runs an older build~:[~; · `nodecode gateway restart'~]"
                                 (ignore-errors (nlk:gateway-installed-p)))
                  :heading t)))
         ;; Providers with a saved key are listed under Suggested, and Enter on
         ;; one goes on to its models. A Continue row over them, on to every
         ;; connected provider's models at once, would be an extra row the list
         ;; does not need.
         (when connected
           (append (list (make-picker-row :label "Suggested" :heading t))
                   (remove-if-not #'connected-p sorted)
                   (list (make-picker-row :label "Connect another" :heading t))))
         (subseq others 0 pinned)
         ;; A heading between the pinned rows and the rest; without one, 302.AI
         ;; and abliteration.ai would read as picks beside Google.
         (when (< 0 pinned (length others))
           (list (make-picker-row :label "More providers, A to Z" :heading t)))
         (nthcdr pinned others)
         (list (make-picker-row :label "Other provider"
                                :detail "Enter a provider id from models.dev"
                                :value (list :custom t))))))))

(defun gateway-key-check (model provider key)
  "PROVIDER-KEY-CHECK of KEY for PROVIDER as MODEL's organism answers it
(POST /api/gateway/keys, check: true), as the list (VERDICT WORDS) the
/models sink reads: VERDICT NIL when nothing was asked."
  (let ((answer (gateway-model-http-call model :post "/api/gateway/keys"
                                         (json-object "provider" provider "key" (nlk:trimmed key) "check" t)
                                         :read-timeout (+ *provider-models-fetch-timeout-seconds* 15))))
    (list (cdr (assoc (json-value answer :string "verdict")
                      '(("works" . :works) ("refused" . :refused)
                        ("unreachable" . :unreachable) ("unchecked" . :unchecked))
                      :test #'equal))
          (json-value answer :string "words"))))

(defun connect-credential-sink (&optional saved)
  "The /models wizard's ON-SAVE: ask the provider whether it takes the key
(PROVIDER-KEY-CHECK, off the loop), then SAVE-PROVIDER-API-KEY into the
shared auth.json, answering DONE with (MODEL MESSAGE ERROR): ERROR NIL for a
save, :RETRY when nothing listens where the key is checked -- the server's to
start, and the same key is checked again -- else T."
  ;; SAVED, when given, is called with the normalized provider id after a
  ;; successful save — the setup wizard's record of what landed. A refused key
  ;; is not saved: the wizard returns to the key step. A key nothing answered
  ;; (a hang, a 5xx, a 404, or the check itself failing) may still be right:
  ;; it is saved, the message says it was not checked and why, and
  ;; *KEY-CHECKS* keeps it off the list's `connected'.
  ;; A shell asks its organism (GATEWAY-KEY-CHECK): the address a key is
  ;; checked at is the one a turn dials, and a cell that serves the provider
  ;; sets it there only -- OpenCode Go's, checked at models.dev's, answered 404.
  (lambda (model provider-id secret done)
    (off-loop
     model
     (lambda () (if *serving*
                    (multiple-value-list (provider-key-check provider-id (nlk:trimmed secret)))
                    (gateway-key-check model provider-id secret)))
     ;; VALUE is the check's two values as a list, or OFF-LOOP's failure,
     ;; which is said plainly: its text is a Lisp condition's.
     (lambda (model value)
       (destructuring-bind (&optional verdict words)
           (if (off-loop-failure value) (list :unchecked "the check itself failed") value)
         (case verdict
           (:refused
            (funcall done model (format nil "~a; nothing was saved. Enter pastes another." words) t))
           (:unreachable
            (funcall done model (format nil "~a; nothing was saved. Start the server, then Enter ~
                                             checks again; Esc picks another provider."
                                        words)
                     :retry))
           (t
            (handler-case
                (let* ((id (save-provider-api-key provider-id secret))
                       (name (or (json-value (models-catalog-table) :text id "name") id)))
                  (if (eq verdict :unchecked)
                      (setf (gethash id *key-checks*)
                            (list verdict (format nil "~a key was not checked: ~a" name words)))
                      (remhash id *key-checks*))
                  (when saved (funcall saved id))
                  ;; DEVIATION from the Zig materializationSummary copy: the lisp
                  ;; save writes auth.json only (no config.jsonc provider block),
                  ;; and the message says exactly what happened -- `works' only
                  ;; when the provider answered the key.
                  (let ((message (format nil "~a key ~:[saved~;works, saved~] to auth.json~@[, not checked: ~a~]"
                                         name (eq verdict :works) (and (eq verdict :unchecked) words))))
                    ;; A shell's list is its gateway's answer (*GATEWAY-MODELS*),
                    ;; read before this key existed: rebuilt from it, the provider
                    ;; just saved read unconnected and Enter on it asked for the
                    ;; key again. The gateway is read again first, off the loop.
                    (if *serving*
                        (funcall done model message nil)
                        (off-loop model (lambda () (fetch-gateway-models model) t)
                                  (lambda (model value)
                                    (funcall done model
                                             (if-let (failure (off-loop-failure value))
                                               (format nil "~a; the provider list was not read again: ~a"
                                                       message failure)
                                               message)
                                             nil)
                                    nil)))))
              (error (condition)
                (funcall done model
                         (format nil "Could not save the key to ~a (~a); nothing was saved. Enter pastes it again."
                                 (home-abbreviated (uiop:native-namestring (or *auth-file-path* "auth.json")))
                                 (plain-error-text condition))
                         t))))))
       nil))))

(defun open-models-reasoning-picker (model)
  "After /models lands a model, continue through its reasoning-effort view.
The target snapshot is read after the model selection so the ladder and current
rung belong to the new target, just as the setup walk's third stage does."
  ;; Nobody asked for this panel: typing goes past it to the composer
  ;; (PICKER-TYPE-THROUGH), keeping the effort that stands.
  (handler-case
      (progn (open-effort-view model (lambda (model effort) (apply-effort model effort))
                               (lambda (model value) (declare (ignore model value)) nil)
                               :hint "enter continue  esc close  ↑↓ navigate")
             (when-let (dialog (chat-model-dialog model))
               (setf (picker-type-through dialog) t)))
    (error (condition)
      (show-status model (format nil "Reasoning controls unavailable: ~a" condition)
                   :error-p t))))

;;; An Auxiliary pick asks its reasoning effort, as a Main pick asks its own:
;;; without this, a model chosen for side work with no rung chosen would run
;;; at whatever rung the last one had, or its lightest, and nothing would say
;;; which.

(defun open-auxiliary-effort-picker (model)
  "After an Auxiliary pick, the auxiliary model's reasoning-effort view: its
ladder as the gateway answers it (GET auxiliary-model's EFFORTS), the rung it
runs at marked, Default its lightest; a pick lands through the op the model
did. A model that does not reason asks nothing; a read that fails says so in
the composer status."
  (multiple-value-bind (said failed)
      (with-slash-notice ("Reasoning controls are unavailable" :control "~a: ~a")
        (let* ((object (gateway-model-http-call model :get "/api/gateway/auxiliary-model" nil))
               (provider (json-value object :string "provider"))
               (model-id (json-value object :string "model"))
               (ladder (coerce (json-array object "efforts") 'list)))
          (when (and provider ladder)
            (open-think-dialog model provider model-id ladder (json-value object :string "effort")
                               (lambda (model effort)
                                 (slash-auxiliary-model-op model (list provider model-id) :effort effort))
                               :default "The lightest it offers"
                               :subtitle (format nil "~a/~a, the auxiliary model" provider model-id))
            ;; as after a Main pick, typing goes past it (PICKER-TYPE-THROUGH)
            (setf (picker-type-through (chat-model-dialog model)) t))
          nil))
    (when failed (show-status model said :error-p t))))

;;; /models picks two models: Main, the one this session runs on and new
;;; sessions start on, and Auxiliary, the model a session's naming, a recap
;;; or a summary runs on. One list serves both under a strip of two tabs, Tab
;;; between them, and Enter lands its row on the tab in front: a second
;;; command for the auxiliary model was a second place to look, and nothing
;;; in /models said it was there.

(defparameter +models-tabs+
  (flet ((shown (row tab) (member (picker-row-key row) (list "" tab) :test #'string=))
         (own (row tab) (string= (picker-row-key row) tab)))
    (list (list :label "Main" :value "main" :match #'shown)
          (list :label "Auxiliary" :value "aux" :match #'shown)
          (list :label "Decider" :value "decider" :match #'own)
          (list :label "Backup" :value "backup" :match #'own)))
  "/models' strip: Main and Auxiliary show the rows they share, keyed \"\", and
their own; Decider, no chat model, its own alone.")

(defun models-decider-rows (&aux (in-use (decider-model)))
  "The rows only /models' Decider tab shows, keyed `decider': what a decider
answers, then each decider (DECIDER-CHOICES) with its model and where it
answers, and None -- the one in use first, so the tab opens on it."
  (flet ((own (here &rest options)
           (cons here (apply #'make-picker-row :key "decider" options))))
    (let ((choices (append (loop for row in (decider-choices)
                                 for here = (equalp (getf row :id) in-use)
                                 collect (own here :label (getf row :name)
                                              :detail (format nil "~:[~;in use · ~]~a · ~:[~;local · ~]~a"
                                                              here (getf row :model)
                                                              (local-endpoint-p (getf row :base))
                                                              (decider-address (getf row :base)))
                                              :value (list :model (list :decider (getf row :id)))))
                           (list (own (null in-use) :label "None"
                                      :detail (format nil "~:[~;in use · ~]nothing decides" (null in-use))
                                      :value (list :model (list :decider nil)))))))
      (append (list (make-picker-row :key "decider" :label "Judgments as probabilities" :heading t))
              (mapcar #'cdr (stable-sort choices (lambda (a b) (and (car a) (not (car b))))))
              (list (make-picker-row :key "decider" :heading t))))))

(defun decider-address (base)
  "BASE, a decider's base URL, as a row says where it answers: host:port."
  (let ((uri (ignore-errors (quri:uri base))))
    (if (and uri (quri:uri-host uri))
        (format nil "~a~@[:~a~]" (quri:uri-host uri) (quri:uri-port uri))
        base)))

(defun models-local-rows (rows)
  "/models' Local section: ROWS' models whose provider answers from a machine
of the operator's own (PICKER-LOCAL-PROVIDER-IDS), under their heading and
over a blank one; NIL when there are none. Each carries its (PROVIDER
MODEL) pair the way a Frequently used shortcut does."
  (let* ((local (picker-local-provider-ids rows))
         (mine (remove-if-not (lambda (row &aux (value (picker-row-value row)))
                                (and (stringp (first value))
                                     (member (first value) local :test #'string-equal)))
                              rows)))
    (when mine
      (append (list (make-picker-row :label "Local" :heading t))
              (mapcar #'models-root-row mine)
              (list (make-picker-row :heading t))))))

(defun models-own-rows (rows)
  "/models' Nodecode section, the top of its root: nodecode.ai's own models
among ROWS (+OWN-PROVIDER+), each picked the way a Frequently used shortcut
is, under their heading and over a blank one; NIL when ROWS hold none."
  (when-let (own (remove-if-not (lambda (row) (models-provider-row-p row +own-provider+)) rows))
    (append (list (make-picker-row :label "Nodecode" :heading t))
            (mapcar (lambda (row) (if (picker-row-value row) (models-root-row row) row)) own)
            (list (make-picker-row :heading t)))))

(defun models-root-rows (rows history)
  "/models' shared root under its tabs: nodecode.ai's own models
(MODELS-OWN-ROWS), Frequently used from HISTORY, the Local models of ROWS,
then the providers."
  (let* ((providers (connect-picker-rows))
         (root (models-frequent-picker-rows rows history :providers providers))
         (local (models-local-rows rows))
         (at (and local (position (first providers) root :test #'eq)))
         (own (models-own-rows rows))
         (shared (cond ((null at) (append root local))
                       ;; Frequently used closes on its Providers heading: Local goes above it.
                       ((and (plusp at) (picker-row-heading (nth (1- at) root)))
                        (append (subseq root 0 (1- at)) local (subseq root (1- at))))
                       (t (append local (list (make-picker-row :label "Providers" :heading t)) root)))))
    ;; Under the Nodecode section a bare provider list takes a heading of its
    ;; own, or its rows would read as ours.
    (append own
            (when (and own shared (not (picker-row-heading (first shared))))
              (list (make-picker-row :label "Providers" :heading t)))
            shared)))

(defun models-auxiliary-rows (rows)
  "The rows only /models' Auxiliary tab shows, keyed `aux': what the
auxiliary model runs, the one in use -- named as ROWS, the catalog's rows,
name it -- and Same as main, which takes it out. The one in use comes first,
so the tab opens on it."
  (multiple-value-bind (provider id) (shared-model-pin "auxiliary_model")
    (let ((row (and provider (find (list provider id) rows :key #'picker-row-value :test #'equalp))))
      (flet ((own (&rest options) (apply #'make-picker-row :key "aux" options)))
        (append
         ;; what it runs, short enough for the label column at 80 columns
         (list (own :label "Session names, recaps, summaries" :heading t))
         (when provider
           (list (own :label (if row (picker-row-label row) id)
                      :detail (format nil "in use · ~a" provider)
                      :value (list :model (list provider id)))))
         (list (own :label "Same as main"
                    :detail (format nil "~:[~;in use · ~]each session's model" (null provider))
                    :value (list :model :default))
               (own :heading t)))))))

;;; /models' Backup tab is the failover chain every session and room share:
;;; the backups in the order a failing turn tries them, each with what history
;;; says of it, Add, the triggers as rows Enter switches, and Test. The state
;;; is the gateway's (GET /api/gateway/failover), read off the loop for the
;;; dialog that asked and installed only in that identical dialog; a pick
;;; writes through POST and opens the tab again on the answer.

(defvar *models-backup-by-dialog* (make-hash-table :test #'eq :weakness :key)
  "/models dialog -> the failover state its read answered, the route's JSON.")

(defun backup-record-words (entry &aux (at (json-value entry :string "answered_at"))
                                       (tried (or (json-value entry :integer "tried") 0))
                                       (universal (and at (iso-universal at))))
  "What history says of ENTRY, one backup of the route's chain, in words short
enough for a picker's detail column: when it last answered, as how long ago,
and how many chain moves it rescued."
  (format nil "~:[never answered~;answered ~:*~a~]~:[~; · rescued ~d/~d~]"
          (and universal
               (let ((minutes (floor (max 0 (- (get-universal-time) universal)) 60)))
                 (cond ((< minutes 1) "just now")
                       ((< minutes 60) (format nil "~dm ago" minutes))
                       ((< minutes (* 48 60)) (format nil "~dh ago" (floor minutes 60)))
                       (t (format nil "~dd ago" (floor minutes (* 24 60)))))))
          (plusp tried) (or (json-value entry :integer "rescued") 0) tried))

(defun models-backup-rows (state)
  "The rows only /models' Backup tab shows, keyed `backup', over STATE -- the
failover route's answer, NIL while its read is out."
  (flet ((own (&rest options) (apply #'make-picker-row :key "backup" options))
         (act (&rest action) (list :model (list* :backup action))))
    (append
     (list (own :label "Tried in order · enter takes one out" :heading t))
     (cond
       ((null state) (list (own :label "Reading backups…" :detail "")))
       ((off-loop-failure state)
        (list (own :label "Could not read backups" :detail (off-loop-failure state))))
       (t
        (append
         (loop for entry across (or (json-value state :array "chain") #())
               for index from 0
               collect (own :label (format nil "~d. ~a" (1+ index) (json-value entry :string "model"))
                            :detail (format nil "~a · ~a"
                                            (json-value entry :string "provider") (backup-record-words entry))
                            :value (act :remove index)))
         (list (own :label "Add a backup…" :detail "pick a model; it goes last" :value (act :add))
               (own :label "Test backups" :detail "asks each backup once" :value (act :test))
               (own :heading t)
               (own :label "Switch when the model hits" :heading t))
         (loop for trigger across (or (json-value state :array "triggers") #())
               for on = (eq t (gethash "on" trigger))
               collect (own :label (format nil "~:[ ~;✓~] ~a" on (json-value trigger :string "label"))
                            :detail (if on "on · enter turns it off" "off · enter turns it on")
                            :value (act :toggle (json-value trigger :string "id")))))))
     (list (own :heading t)))))

(defun install-backup-state (model dialog value)
  "Install the failover state only in the identical dialog that dispatched the
read: closing, replacing, or reopening the picker discards its old completion."
  (unless (eq dialog model.dialog)
    (return-from install-backup-state :quiet))
  (setf (gethash dialog *models-backup-by-dialog*) value)
  ;; A credential being entered keeps its step's rows and selection.
  (unless (and (eq :model-providers dialog.kind) (eq :provider (connect-dialog-step dialog)))
    (return-from install-backup-state :quiet))
  (restock-models-dialog model dialog (picker-highlighted-value dialog)))

(defun start-models-backup-read (model dialog)
  "Read the failover state off the UI loop, scoped to DIALOG."
  (when model.program
    (off-loop model
              (lambda () (gateway-model-http-call model :get "/api/gateway/failover" nil))
              (lambda (model value) (install-backup-state model dialog value)))))

(defun backup-state-summary (state)
  "The notice a failover write answers: the chain, the triggers on, and a
config that was not saved."
  (let ((chain (coerce (or (json-value state :array "chain") #()) 'list))
        (on (loop for trigger across (or (json-value state :array "triggers") #())
                  when (eq t (gethash "on" trigger)) collect (json-value trigger :string "label"))))
    (format nil "backups: ~:[none~;~:*~{~a~^, ~}~] · switch on: ~:[nothing~;~:*~{~a~^, ~}~]~@[~%shared config: NOT saved — ~a~]"
            (loop for entry in chain
                  collect (format nil "~a/~a" (json-value entry :string "provider") (json-value entry :string "model")))
            on (shared-config-failure state))))

(defun models-backup-write (model body)
  "POST BODY to the failover route: (values NOTICE FAILED), and the state it answered."
  (let ((state nil))
    (multiple-value-bind (notice failed)
        (with-slash-notice ("backup models")
          (setf state (gateway-model-http-call model :post "/api/gateway/failover" body))
          (backup-state-summary state))
      (values notice failed state))))

(defun reopen-models-backup-tab (model state)
  "Open /models on its Backup tab again, STATE in it already, unless another
dialog took the screen meanwhile."
  (unless model.dialog
    (open-models-provider-picker model :tab "backup")
    (when (and state model.dialog)
      (install-backup-state model model.dialog state))))

(defun models-backup-act (model dialog action &aux (state (gethash dialog *models-backup-by-dialog*)))
  "Run ACTION, a Backup tab row's -- (:REMOVE INDEX), (:ADD), (:TOGGLE ID) or
(:TEST) -- over STATE, the failover state DIALOG was showing."
  (flet ((pairs (chain) (map 'list (lambda (entry) (json-object "provider" (json-value entry :string "provider")
                                                              "model" (json-value entry :string "model")))
                             chain))
         (post-state (body)
           (multiple-value-bind (notice failed answer) (models-backup-write model body)
             (show-status model notice :error-p failed)
             (reopen-models-backup-tab model (and (not failed) answer)))))
    (let ((chain (coerce (or (and (hash-table-p state) (json-value state :array "chain")) #()) 'list))
          (triggers (coerce (or (and (hash-table-p state) (json-value state :array "triggers")) #()) 'list)))
      (ecase (first action)
        (:remove
         (post-state (json-object "chain" (coerce (pairs (append (subseq chain 0 (second action))
                                                            (nthcdr (1+ (second action)) chain)))
                                             'vector))))
        (:toggle
         (post-state (json-object
                 "on" (coerce (loop for trigger in triggers
                                    for id = (json-value trigger :string "id")
                                    for on = (eq t (gethash "on" trigger))
                                    when (if (equal id (second action)) (not on) on) collect id)
                              'vector))))
        (:add
         (open-models-picker
          model
          :lands (lambda (model tokens)
                   (models-backup-write
                    model (json-object "chain" (coerce (append (pairs chain)
                                                               (list (json-object "provider" (first tokens)
                                                                                  "model" (second tokens))))
                                                       'vector))))
          :then (lambda (model picked)
                  (declare (ignore picked))
                  (reopen-models-backup-tab model nil))))
        (:test
         (show-status model "Testing backups…")
         (off-loop model
                   (lambda () (gateway-model-http-call model :post "/api/gateway/failover/test" (json-object)))
                   (lambda (model value)
                     (if (off-loop-failure value)
                         (show-status model (format nil "Couldn't test backups: ~a" (off-loop-failure value)) :error-p t)
                         (show-status model
                                      (format nil "backups: ~:[none to test~;~:*~{~a~^ · ~}~]"
                                              (loop for result across (or (json-value value :array "results") #())
                                                    collect (format nil "~a ~:[✗ ~a~;✓~*~] ~,1f s"
                                                                    (json-value result :string "model")
                                                                    (eq t (gethash "ok" result))
                                                                    (json-value result :string "reason")
                                                                    (or (gethash "seconds" result) 0)))))))))))))

(defun open-models-provider-picker (model &key (tab "main"))
  "Open /models on providers, with retained, model-specific shortcuts and
the local models above them, on TAB: main, aux or decider (+MODELS-TABS+)."
  (let ((dialog nil))
    (open-connect-dialog
     model
     (lambda (&aux (rows (models-catalog-picker-rows)))
       (append (models-auxiliary-rows rows)
               (models-decider-rows)
               (models-backup-rows (gethash dialog *models-backup-by-dialog*))
               (models-root-rows rows (gethash dialog *models-frequent-by-dialog* #()))))
     (connect-credential-sink)
     (lambda (model &optional choice &aux (aux (equal (picker-dialog-tab-value dialog) "aux")))
       (cond
         ;; A Backup row carries (:BACKUP ACTION ...).
         ((and (consp choice) (eq (first choice) :backup))
          (models-backup-act model dialog (rest choice)))
         ;; The Nodecode section's row of a home not signed in carries (:LOGIN).
         ((equal choice '(:login)) (slash-login-op model ""))
         ;; A Decider row carries (:DECIDER ID), None's ID NIL.
         ((and (consp choice) (eq (first choice) :decider))
          (multiple-value-bind (notice failed)
              (slash-decider-model-op model (list (or (second choice) "none")))
            (show-status model notice :error-p failed)))
         ;; A shortcut row carries the original (PROVIDER MODEL) pair; Same as
         ;; main carries :DEFAULT, and takes the auxiliary model out.
         ((or (eq choice :default) (and (consp choice) (stringp (first choice))))
          (multiple-value-bind (notice failed)
              (if aux
                  (slash-auxiliary-model-op model (if (eq choice :default) '("default") choice))
                  (slash-models-op model choice))
            (show-status model notice :error-p failed)
            (unless (or failed (eq choice :default))
              (if aux (open-auxiliary-effort-picker model) (open-models-reasoning-picker model)))))
         ;; A provider row hands back only its provider id.
         (choice
          (open-models-picker
           model :provider choice :lands (if aux :auxiliary :session)
           :then (lambda (model picked)
                   (cond ((null picked) (open-models-provider-picker model :tab (if aux "aux" "main")))
                         (aux (open-auxiliary-effort-picker model))
                         (t (open-models-reasoning-picker model))))))
         ;; Esc from the provider list closes the root.
         (t nil)))
     :title "Models" :kind :model-providers)
    (setf dialog (search-models-from-root (chat-model-dialog model)))
    (register-picker-tabs dialog +models-tabs+
                          :active (position tab +models-tabs+ :key (lambda (entry) (getf entry :value))
                                                              :test #'equal))
    (picker-seat-top dialog)
    (start-models-frequent-read model dialog)
    (start-models-backup-read model dialog)
    (start-provider-models-refresh model))
  (values nil nil))

;;; --- /usage ------------------------------------------------------------------
;;; On a shell /usage is a dialog: a tab per range, each filled by its own read
;;; of the gateway's usage (the store is the gateway's), off the loop, the one
;;; asked for first; Tab moves between them, Enter on a session opens it. A
;;; channel or the web page's `/' gets the same read as text (HEADLESS-USAGE).

(defparameter +usage-tabs+ '(("today" "Today") ("7d" "7 days") ("30d" "30 days") ("all" "All"))
  "The /usage dialog's tabs: each range and its label, in strip order.")

(defun usage-dialog-rows (range usage)
  "RANGE's rows in the /usage dialog, each keyed by RANGE so its tab shows
them: the sentence, the totals, spend by day, the models, the costliest
sessions -- Enter opens one -- and the folders. NIL USAGE is a read still
out; an OFF-LOOP failure is said in one row."
  (flet ((row (label detail &key value (mark "") (tone :muted))
           (make-picker-row :key range :label label :detail detail :value value
                            :mark mark :mark-tone tone))
         (pad (text) (format nil "~34a" (stub-clip (or text "") 34))))
    (cond
      ((null usage) (list (row "Adding it up…" "")))
      ((off-loop-failure usage)
       (list (row "Could not read usage" (off-loop-failure usage) :mark "×" :tone :danger)))
      (t
       (let* ((totals (usage-field usage "totals"))
              (cost (usage-field totals "cost_usd"))
              (days (coerce (or (usage-field usage "days") #()) 'list))
              (models (coerce (or (usage-field usage "models") #()) 'list))
              (sessions (coerce (or (usage-field usage "sessions") #()) 'list))
              (folders (coerce (or (usage-field usage "folders") #()) 'list)))
         (append
          ;; The nodecode.ai account first, as the footer's meter last read it.
          (account-slab-rows range)
          (list (row (usage-headline usage)
                     (format nil "~@[since ~a · ~]API-equivalent prices" (usage-since usage))
                     :mark "■" :tone :accent))
          (loop for sentence in (usage-sentences usage) collect (row sentence ""))
          (list (row (pad "Spend")
                     (format nil "~a  sum cost_usd~@[ · ~:d unpriced~]" (usd-text cost)
                             (let ((unpriced (usage-field totals "unpriced")))
                               (and unpriced (plusp unpriced) unpriced))))
                (row (pad "Model calls")
                     (format nil "~:d  provider requests" (or (usage-field totals "calls") 0)))
                (row (pad "Sessions")
                     (format nil "~:d  ~a" (or (usage-field totals "sessions") 0)
                             (counted (or (usage-field totals "turns") 0) "turn")))
                (row (pad "From cache")
                     (format nil "~a  cache hit~@[ · ~a~]"
                             (share-text (usage-field totals "cached_share"))
                             (let ((saved (usage-field totals "saved_usd")))
                               (cond ((not (realp saved)) nil)
                                     ((> saved 0.005) (format nil "saved about ~a" (usd-text saved)))
                                     ((< saved -0.005)
                                      (format nil "writes cost ~a more than reads saved" (usd-text (- saved))))))))
                (row (pad "First word, typical")
                     (format nil "~a  TTFT p50 · p90 ~a" (ms-text (usage-field usage "wait_p50_ms"))
                             (ms-text (usage-field usage "wait_p90_ms"))))
                (row (pad "Failed turns")
                     (format nil "~:d  turn.failed" (or (usage-field totals "failed_turns") 0))))
          (when (rest days)
            (let* ((priced (some (lambda (day) (usage-field day "cost_usd")) days))
                   (shown (last days 60))
                   (values (mapcar (lambda (day) (or (usage-field day (if priced "cost_usd" "calls")) 0))
                                   shown))
                   (top (reduce #'max values :initial-value 0)))
              (list (row (pad (if priced "Spend by day" "Model calls by day"))
                         (format nil "~a  ~a – ~a"
                                 (map 'string (lambda (value)
                                                (if (or (zerop top) (zerop value))
                                                    #\·
                                                    (char "▁▂▃▄▅▆▇█" (min 7 (floor (* 8 value) (* 1.0001 top))))))
                                      values)
                                 (subseq (usage-field (first shown) "day") 5)
                                 (subseq (usage-field (car (last shown)) "day") 5))))))
          (when models
            (cons (row (pad "Models") "calls · from cache · spend")
                  (loop for model in (subseq models 0 (min 8 (length models)))
                        collect (row (pad (format nil "  ~a" (usage-field model "model")))
                                     (format nil "~:d · ~a · ~a" (usage-field model "calls")
                                             (share-text (usage-field model "cached_share"))
                                             (usd-text (usage-field model "cost_usd")))))))
          (when sessions
            (cons (row (pad (if cost "Costliest sessions" "Busiest sessions")) "enter opens one")
                  (loop for session in sessions
                        for title = (usage-field session "title")
                        collect (row (pad (format nil "  ~a" (if (plusp (length title)) title
                                                                 (usage-field session "id"))))
                                     (format nil "~a · ~a · ~a" (folder-tail (usage-field session "folder"))
                                             (ms-text (usage-field session "ms"))
                                             (if (usage-field session "cost_usd")
                                                 (usd-text (usage-field session "cost_usd"))
                                                 (counted (usage-field session "calls") "call")))
                                     :value (list :session (usage-field session "id"))))))
          (when folders
            (cons (row (pad "Folders") "sessions · time · spend")
                  (loop for folder in folders
                        collect (row (pad (format nil "  ~a" (folder-tail (usage-field folder "folder"))))
                                     (format nil "~:d · ~a · ~a" (usage-field folder "sessions")
                                             (ms-text (usage-field folder "ms"))
                                             (usd-text (usage-field folder "cost_usd")))))))))))))

(defun dialog-read (model thunk continuation &aux (dialog model.dialog))
  "OFF-LOOP for the dialog open now: CONTINUATION takes (MODEL DIALOG VALUE)
while DIALOG is still the one open; one closed or replaced meanwhile takes
nothing, and owes no frame."
  (off-loop model thunk
            (lambda (model value)
              (if (eq dialog model.dialog) (funcall continuation model dialog value) :quiet))))

(defun usage-dialog-pick (model value)
  "Enter on a /usage row: a session's row opens that session."
  (when (and (consp value) (eq (first value) :session))
    (switch-gateway-session model (second value))))

(defun usage-dialog-account-rows (rows)
  "ROWS, the /usage dialog's, with each tab's nodecode.ai account rows (their
value (:ACCOUNT)) drawn again from *ACCOUNT-METER*; a tab whose read is still
out has none and takes them when it lands (USAGE-DIALOG-ROWS)."
  (loop for (id) in +usage-tabs+
        for own = (remove id rows :key #'picker-row-key :test-not #'equal)
        append (if (find '(:account) own :key #'picker-row-value :test #'equal)
                   (append (account-slab-rows id)
                           (remove '(:account) own :key #'picker-row-value :test #'equal))
                   own)))

(defun dialog-account-read (model redraw)
  "Read the nodecode.ai account's meter again from the gate for the dialog open now, off the loop:
it lands in *ACCOUNT-METER* and the dialog's rows become (REDRAW ROWS). The footer reads it at
connect and after a turn only, so a top-up bought while this shell was open read `API credit:
none' in the dialog until the next prompt (checkout audit, 2026-10-22)."
  (dialog-read model
               (lambda () (gateway-model-http-call model :get "/api/gateway/account?fresh=1" nil))
               (lambda-ignoring (model dialog value)
                 (unless (off-loop-failure value)
                   (setf *account-meter* (account-meter-of value)
                         dialog.rows (funcall redraw dialog.rows)))
                 nil)))

(defun slash-usage-op (model &optional args &aux (range (usage-range-word args)))
  "Open the /usage dialog on the range ARGS names, its reads off the loop."
  (cond
    ((null range) "Usage: /usage [today | 7d | 30d | all]")
    (t (open-picker-dialog model "Usage"
                           (loop for (id) in +usage-tabs+ append (usage-dialog-rows id nil))
                           #'usage-dialog-pick
                           :kind :usage
                           :context "every session"
                           :empty-label "Nothing ran in this range"
                           :hint "tab range  enter open session  esc close  ↑↓ navigate")
       (register-picker-tabs model.dialog (loop for (id label) in +usage-tabs+
                                                collect (list :label label :value id))
                             :active (position range +usage-tabs+ :key #'first :test #'equal))
       (dolist (id (cons range (remove range (mapcar #'first +usage-tabs+) :test #'equal)))
         (let ((id id))
           (dialog-read model
                        (lambda ()
                          (gateway-model-http-call
                           model :get (format nil "/api/gateway/usage?range=~a" id) nil))
                        (lambda-ignoring (model dialog value)
                          (setf dialog.rows (append (remove id dialog.rows :key #'picker-row-key
                                                                           :test #'equal)
                                                    (usage-dialog-rows id value)))
                          nil))))
       (dialog-account-read model #'usage-dialog-account-rows)
       nil)))

;;; --- /account ----------------------------------------------------------------
;;; What the footer's account badge opens: the nodecode.ai account alone, the
;;; plan's slab and the API credit's, no ranges and none of /usage's breakdown
;;; -- asked 2026-10-25: "just show the Nodecode.ai account and the API
;;; credit box, thats it". No headless arm: a room or a page shown this home's
;;; balance would read someone's account.

(defun slash-account-op (model)
  "Open the nodecode.ai account dialog: the plan's slab and the API credit's as the footer's meter
last read them, then read again from the gate (DIALOG-ACCOUNT-READ)."
  (flet ((rows (&optional stale)
           (declare (ignore stale))
           (account-slab-rows "")))
    (open-picker-dialog model "Usage" (rows) (constantly nil)
                        :kind :account
                        :empty-label "Not signed in to nodecode.ai: /login signs in"
                        :hint "esc close")
    (dialog-account-read model #'rows))
  nil)

;;; --- /think ------------------------------------------------------------------
;;; The picker and the footer read one answer, GET /api/gateway/target
;;; (TARGET-SNAPSHOT): the target this shell's session binds, its effort pick
;;; and the ladder that target admits. A pick lands through POST
;;; /api/gateway/sessions/<id>/effort (APPLY-EFFORT).

(defun target-snapshot (model)
  "=> (values PROVIDER MODEL-ID LADDER FACTS) from the gateway's target for
this shell's session: LADDER the effort rungs it admits, weakest first, FACTS
the footer's plist (:provider :model :unconfigured :pinned :pin-provider
:pin-model :effort :refusal :endpoint), and the default a new session starts on
(:default-provider :default-model)."
  ;; One dial answers the /think picker and the chrome, and an attached client
  ;; has no store to ask instead. The scope is what makes the answer address
  ;; the target the next turn on this session will bind rather than the
  ;; gateway's bare default.
  (let* ((target (gateway-model-http-call
                  model :get
                  (format nil "/api/gateway/target~@[?session_id=~a~]" model.session-id)
                  nil))
         (provider (json-value target :string "provider"))
         (model-id (json-value target :string "model"))
         (pin (json-value target :object "pin"))
         (default (json-value target :object "default")))
    (values provider model-id
            (coerce (json-array target "efforts") 'list)
            (list :provider provider
                  :model model-id
                  :unconfigured (json-value target :boolean "unconfigured")
                  :pinned (and pin t)
                  :pin-provider (json-value pin :text "provider")
                  :pin-model (json-value pin :text "model")
                  :effort (json-value target :text "effort")
                  :refusal (json-value target :text "refusal")
                  :endpoint (json-value target :text "endpoint")
                  :default-provider (json-value default :text "provider")
                  :default-model (json-value default :text "model")))))

(defun open-think-dialog (model provider model-id ladder effort on-select
                          &key (default "Use the provider default")
                          (subtitle (format nil "~a/~a" provider model-id)))
  ;; The items: Default -- the model's own reasoning, or what DEFAULT says
  ;; it is -- then every rung of the target's ladder weakest first, the
  ;; effort the target binds marked current and highlighted; the panel
  ;; shrinks to the rung count.
  (let ((rows (cons (make-picker-row :label "Default"
                                     :detail default
                                     :value nil)
                    (mapcar (lambda (rung)
                              (make-picker-row
                               :label rung
                               :detail (cond ((equal rung effort) "current")
                                             ((equal rung "off") "Answer without reasoning.")
                                             (t ""))
                               :value rung))
                            ladder))))
    (open-picker-dialog model
                        "Reasoning effort"
                        rows
                        on-select
                        :kind :think
                        :selection (if-let (index (position effort ladder :test #'equal))
                                     (1+ index) 0)
                        :subtitle subtitle
                        :empty-label "Reasoning controls are unavailable."
                        :hint "esc close  up/down navigate"))
  nil)

(defun open-effort-view (model on-effort on-continue &rest options)
  "The target's reasoning-effort view: its ladder's picker, ON-EFFORT taking the
pick, or -- a model with no controls -- one Continue row, ON-CONTINUE taking it."
  (multiple-value-bind (provider model-id ladder facts) (target-snapshot model)
    (if ladder
        (open-think-dialog model provider model-id ladder (getf facts :effort) on-effort)
        (apply #'open-picker-dialog model "Reasoning effort"
               (list (make-picker-row :label "Continue" :value :continue)) on-continue
               :kind :think :subtitle "This model has no reasoning effort controls." options))))

(defun apply-effort (model effort)
  "Land EFFORT — one rung of the target's ladder, or NIL for the model's own —
on this shell's session from its next turn, and as the default new sessions
start on: POST /api/gateway/sessions/<id>/effort (SET-SESSION-EFFORT)."
  ;; Returns NIL, so a picker's on-select can tail-call it.
  (session-control-call model :post "effort"
                        :body (json-object "effort" (or effort :null)))
  ;; The pick the footer renders came from the gateway with the identity it
  ;; belongs to; the landed pick makes that pair stale.
  (refresh-gateway-model-facts model)
  nil)

(defun slash-think-op (model &optional args)
  "Open the reasoning-effort picker, or apply one ARGS rung."
  ;; Returns notice text.
  (with-slash-notice ("Reasoning controls are unavailable"
                      :control "~a: ~a")
    (bind (((provider model-id ladder facts) (target-snapshot model))
           (tokens (split-words (or args "")))
           (value (unless (string-equal "default" (first tokens)) (first tokens))))
      (cond
        ((null ladder) "No reasoning control is available for this model.")
        ((> (length tokens) 1) (slash-usage "think"))
        (tokens
         (with-slash-notice ("reasoning control")
           (apply-effort model value)
           (format nil "reasoning effort for ~a/~a: ~a; applies from the next turn"
                   provider model-id (or value "provider default"))))
        (t
         (open-think-dialog
          model provider model-id ladder (getf facts :effort)
          (lambda (model value)
            ;; Enter is silent: the pick lands for the next turn and the
            ;; picker closes.
            (with-slash-notice ("reasoning control")
              (apply-effort model value)))))))))

;;; --- the gateway's cell commands -----------------------------------------
;;; A shell runs its builtins in its own image and a cell's command where
;;; the cell runs: it reads the catalog the gateway serves (GET
;;; /api/gateway/slash, whose builtins SET-GATEWAY-COMMANDS drops) and runs one
;;; through POST /api/gateway/slash, as the web page does. The answer's text is
;;; the notice; the panel it carries opens as this shell's picker.

(defun gateway-command-entries (model)
  "The commands MODEL's gateway serves (GET /api/gateway/slash), as the plists
SET-GATEWAY-COMMANDS takes."
  (loop for command across (json-array (gateway-model-http-call model :get "/api/gateway/slash" nil)
                                       "commands")
        collect (list :name (json-value command :string "name")
                      :description (or (json-value command :string "description") "")
                      :usage (or (json-value command :string "usage") "")
                      :session (json-value command :boolean "session")
                      :trigger (json-value command :text "trigger"))))

(defun gateway-slash-answer (model line)
  "The answer MODEL's gateway gives the slash LINE (POST /api/gateway/slash),
run in MODEL's session."
  (gateway-model-http-call
   model :post "/api/gateway/slash"
   (json-object "line" line :opt "session_id" (json-value model.session-id :text))))

(defun run-gateway-command (model entry args &key trigger)
  "Run ENTRY, a cell's command MODEL's gateway serves, with its verbatim
argument tail ARGS, and answer the notice: the answer's text, or NIL when the
panel it carries opened; TRIGGER when the composer's trigger character opened
it."
  (let* ((name (slash-command-name entry))
         (line (format nil "/~a~@[ ~a~]" name (json-value args :text))))
    (with-slash-notice ((format nil "/~a" name))
      (let ((answer (gateway-slash-answer model line)))
        (if (json-value answer :object "dialog")
            (progn (open-answer-dialog model answer :line line :trigger trigger) nil)
            (json-value answer :string "text"))))))

;;; --- the dialog a command's answer carries --------------------------------
;;; A command's answer may carry a docked picker (GATEWAY.LISP LIST-DIALOG):
;;; the shell that ran the command owns the panel, so a cell describes the
;;; list once and every shell paints it. The reader is the same on both
;;; transports -- the value's JSON -- so a local call and a gateway run open
;;; the same dialog.

(defun dialog-rows (dialog)
  "The picker rows DIALOG's JSON carries (LIST-DIALOG's ROWS), each row's own
JSON its value: what Enter inserts and the lines its keys run are read off it."
  (loop for row across (json-array dialog "rows")
        for tone = (json-value row :string "mark_tone")
        collect (make-picker-row
                 :key (or (json-value row :string "key") "")
                 :label (or (json-value row :string "label") "")
                 :detail (or (json-value row :string "detail") "")
                 :suffix (or (json-value row :string "suffix") "")
                 :value row
                 :mark (or (json-value row :string "mark") "")
                 :mark-tone (if tone (intern (string-upcase tone) :keyword) :muted))))

(defun dialog-tabs (dialog)
  "The tab strip DIALOG's JSON carries (LIST-DIALOG's TABS) as the plists
REGISTER-PICKER-TABS takes, or NIL for a dialog with no strip."
  (loop for tab across (json-array dialog "tabs")
        collect (list* :label (or (json-value tab :string "label") "")
                       (json-members tab :any "count" "value"))))

(defun dialog-picture (dialog)
  "The picture DIALOG's JSON carries (LIST-DIALOG's PICTURE) as the plist
REGISTER-PICKER-PICTURE takes, or NIL for a dialog with none."
  (when-let (picture (json-value dialog :object "picture"))
    (let ((palette (coerce (remove-if-not #'stringp (json-array picture "palette")) 'list)))
      (list* :rows (coerce (remove-if-not #'stringp (json-array picture "rows")) 'list)
             :label (or (json-value picture :string "label") "")
             (append (and palette (list :palette palette))
                     (and (json-value picture :boolean "beside") (list :beside t)))))))

(defun open-answer-dialog (model answer &key line trigger under (selection 0) label)
  "Open the dialog ANSWER -- the answer to the slash LINE -- carries as
MODEL's picker, on the row LABEL names, else row SELECTION, over UNDER: the
answer dialogs it was opened from, nearest first, as (LINE SELECTION LABEL).
A command whose answer is its text alone opens nothing."
  ;; The row's VALUE is what Enter hands the composer: a command that answers
  ;; with a list of things to name picks one into the draft at the caret,
  ;; which is how /index lands a `$name'. Opened by its TRIGGER, the
  ;; character the operator typed is still at the caret, and a value that
  ;; begins with it takes its place. A row's RUN lines outrank it: Enter and
  ;; Space run them (ANSWER-GESTURE), and Esc -- or Enter on a row that does
  ;; neither -- goes back to the dialog under this one, read again.
  (when-let (dialog (json-value answer :object "dialog"))
    (let* ((rows (dialog-rows dialog))
           (picker nil))
      (labels ((stack (&aux (row (selected-of (picker-visible-rows picker)
                                              (picker-dialog-selection picker))))
                 ;; This dialog atop UNDER, on the row the cursor is on: read
                 ;; again, the row keeps the cursor wherever the list moved it.
                 (cons (list line (or (position row rows) 0) (and row (picker-row-label row)))
                       under))
               (gesture (model value key)
                 (when-let (run (json-value value :text "run" key))
                   (setf (chat-model-dialog model) nil)
                   (answer-gesture model run (stack))
                   t)))
        (open-picker-dialog
         model
         (or (json-value dialog :string "title") "Pick one")
         rows
         (lambda (model value &aux (text (json-value value :text "value")))
           (cond ((gesture model value "enter"))
                 (text
                  (when (and trigger (uiop:string-prefix-p trigger text)
                             (uiop:string-suffix-p (editor-value model.editor) trigger))
                    (loop repeat (length trigger)
                          do (editor-handle-key model.editor (nlt:make-key-press-msg :code :backspace))))
                  (editor-insert model.editor text))
                 (t (reopen-answer-dialog model under)))
           nil)
         :selection (or (and label (position label rows :key #'picker-row-label :test #'equal))
                        (min selection (max 0 (1- (length rows)))))
         :on-cancel (lambda (model) (reopen-answer-dialog model under))
         :on-space (and (find-if (lambda (row) (json-value (picker-row-value row) :text "run" "space"))
                                 rows)
                        (lambda (model dialog)
                          (gesture model (picker-highlighted-value dialog) "space")))
         :searchable (json-value dialog :boolean "searchable")
         :search-placeholder (or (json-value dialog :string "search_placeholder") "")
         :empty-label (or (json-value dialog :string "empty_label") "Nothing to pick")
         :no-match-label (json-value dialog :string "no_match_label")
         :hint (json-value dialog :string "hint")
         :context (or (json-value dialog :string "context") "")
         :subtitle (or (json-value dialog :string "summary") "")
         :summary-tone (if-let (tone (json-value dialog :string "summary_tone"))
                         (intern (string-upcase tone) :keyword)
                         :muted)
         :kind :answer)
        (setf picker model.dialog)
        (register-picker-tabs picker (dialog-tabs dialog))
        (register-picker-picture picker (dialog-picture dialog))))))

(defun answer-gesture (model line stack)
  "Run LINE, the line a row's key names in the answer dialog atop STACK, where
the command ran: a dialog it answers opens over STACK; else what it says is a
toast, the one line the panel leaves in sight, and STACK is read again
(REOPEN-ANSWER-DIALOG)."
  (multiple-value-bind (answer failed) (with-slash-notice (line) (gateway-slash-answer model line))
    (if (and (not failed) (json-value answer :object "dialog"))
        (open-answer-dialog model answer :line line :under stack)
        (progn (show-status model (if failed answer (json-value answer :string "text")) :error-p failed)
               (reopen-answer-dialog model stack)))))

(defun reopen-answer-dialog (model stack)
  "Open the answer dialog atop STACK -- (LINE SELECTION LABEL) entries,
nearest first -- read again on its row; one whose line answers no dialog now,
what it showed gone, gives way to the one under it. An empty STACK opens
nothing."
  (loop for ((line selection label) . under) on stack
        for answer = (ignore-errors (gateway-slash-answer model line))
        when (json-value answer :object "dialog")
          return (open-answer-dialog model answer :line line :under under
                                                  :selection selection :label label)))

;;; --- headless slash execution ---------------------------------------------
;;; The same catalog, run without a shell: a channel cell answering a
;;; Telegram or Discord `/models', a cron job, an eval snippet. One primitive
;;; over the canonical surfaces the two shells already call (SELECT-MODEL,
;;; SELECT-EFFORT, EVICT-SESSION-CONTEXT, the head move, the cells'
;;; commands), answering text where a shell would post a notice. Commands
;;; whose whole meaning is a dialog or a shell move (the pickers, /fork,
;;; /exit) stay the shells' and answer that they need one.

(defun slash-catalog ()
  "Every command a headless caller can run, in /help order: the builtins
with a headless arm, then each cell's registered command, as plists (:name
:description :usage :autocomplete :session :aliases :trigger) — USAGE the
display form when the command takes arguments, \"\" when the bare name is the
whole usage, :AUTOCOMPLETE true for a command whose argument tail a caller can
complete while it is typed (SLASH-COMPLETIONS answers it), :SESSION NIL for
one that acts on no session, which a client with none open runs as it is,
:ALIASES the other names SLASH answers it by, and :TRIGGER the character a
shell's composer opens it on."
  ;; The menu a channel publishes to its platform is this list; a cell's
  ;; command that shadows a builtin name is left out, as SLASH never reaches
  ;; it.
  (loop for entry in (append (remove-if-not #'slash-command-headless *slash-commands*)
                             (remove-if #'find-builtin-slash-command *registered-commands*
                                        :key #'slash-command-name))
        collect (list :name (slash-command-name entry)
                      :description (slash-command-description entry)
                      :usage entry.usage
                      :autocomplete (and entry.completer t)
                      :session entry.session
                      :aliases entry.aliases
                      :trigger entry.trigger)))

(defun models-argument-choices (text session-id)
  "The model catalog as completion choices for the tail TEXT typed so far:
one (:name \"provider/model\" :value \"provider model\") per catalog row
whose `provider/model' contains TEXT, case-insensitively — an empty TEXT
matches every row — in catalog order, read from the caches as they stand
(no refresh: a keystroke costs a read). After `aux ', the same rows pick the
auxiliary model; before it, `aux' is a choice of its own."
  ;; => list of (:name :value) plists, possibly empty.
  (declare (ignore session-id))
  (let* ((tail (string-left-trim '(#\Space #\Tab) (or text "")))
         (aux (and (> (length tail) 3) (string-equal "aux " tail :end2 4)))
         (needle (string-downcase (string-trim '(#\Space #\Tab) (if aux (subseq tail 4) tail)))))
    (append
     (when (and (not aux) (search needle "aux"))
       (list (list :name "aux — the auxiliary model" :value "aux")))
     (if-let (seat (and (not aux) (> (length tail) 7) (string-equal "decider " tail :end2 8)
                        (string-downcase (string-trim '(#\Space #\Tab) (subseq tail 8)))))
       ;; after `decider ', its ids and none
       (loop for id in (append (mapcar #'first +deciders+) '("none"))
             when (search seat id)
               collect (list :name (format nil "decider ~a" id) :value (format nil "decider ~a" id)))
       (when (and (not aux) (search needle "decider"))
         (list (list :name "decider — the decider model" :value "decider"))))
     (when (and (not aux) (search needle "backups"))
       (list (list :name "backups — the backup models" :value "backups")))
     (loop for row in (models-catalog-picker-rows)
           for value = (picker-row-value row)
           for label = (and value
                            (format nil "~a/~a" (first value) (second value)))
           when (and label (search needle (string-downcase label)))
             collect (list :name label
                           :value (format nil "~:[~;aux ~]~a ~a"
                                          aux (first value) (second value)))))))

(defun slash-completions (name text &key session-id)
  "One command's argument completions for the tail TEXT typed so far, in
SESSION-ID, the session the command would act on — the headless seam a
channel's on-the-fly menu answers through, beside SLASH and SLASH-CATALOG:
what SLASH-CATALOG marks :AUTOCOMPLETE, this answers, a builtin's or a
cell's."
  ;; => (:name :value) plists, or NIL for a command that offers none. The
  ;; catalog's COMPLETER slot names the function; it takes the tail typed so far
  ;; and the session id, as a headless arm takes its argument, and returns the
  ;; plists.
  (let* ((entry (or (find-builtin-slash-command name) (find-registered-command name)))
         (completer (and entry entry.completer)))
    ;; A COMPLETER that is a list of words offers the ones holding the tail.
    (if (listp completer)
        (loop with needle = (string-trim '(#\Space #\Tab) (or text ""))
              for word in completer when (search needle word :test #'char-equal)
              collect (list :name word :value word))
        (funcall completer (or text "") session-id))))

(defun slash-help-text (args session-id)
  "The catalog as text, one command per line."
  (declare (ignore args session-id))
  (format nil "~{~a~^~%~}"
          (loop for entry in (slash-catalog)
                collect (format nil "~a — ~a"
                                (or (json-value (getf entry :usage) :text)
                                    (format nil "/~a" (getf entry :name)))
                                (getf entry :description)))))

(defun require-headless-session (session-id name)
  (unless (stringp session-id)
    (error "/~a has no ambient session here — pass :session-id \"s-...\"" name))
  (unless (and (nlk:store-open-p) (nlk:session-exists-p session-id))
    (error "no session ~s in the open store" session-id))
  session-id)

(defun headless-head-move (session-id new-head boundary reason)
  "The durable head move and the peer broadcast, as the WS navigation runs
them (EXECUTE-NAVIGATE-CHECKPOINT) minus the workspace restore: a headless
move is conversation-only."
  (when (nlk:active-turn-p session-id)
    (error "a turn is in flight on ~a; cancel or await it first" session-id))
  (let ((previous (nlk:session-head-turn-id session-id)))
    (move-session-head-durably session-id new-head boundary reason)
    (broadcast-head-moved session-id new-head previous boundary reason)))

(defun headless-undo (args session-id)
  (declare (ignore args))
  (require-headless-session session-id "undo")
  (let ((head (nlk:session-head-turn-id session-id)))
    (if (null head)
        "nothing to undo"
        (let* ((parent (gethash head (nlk:session-composed-parents session-id)))
               (prompt (turn-started-input-of head))
               ;; The prompt as one line of at most 120 characters.
               (flat (nlk:trimmed (substitute #\Space #\Newline (or prompt head)))))
          (headless-head-move session-id parent "user_message" "rewind")
          (format nil "rewound to before: ~a" (clip flat 120))))))

(defun headless-redo (args session-id)
  (declare (ignore args))
  (require-headless-session session-id "redo")
  ;; The newest head move remembers where the operator came FROM; when that
  ;; origin fell off the current chain it is the branch tip redo returns to
  ;; (the WS navigate direction rule).
  (if-let (origin (nlk::redo-origin session-id))
    (progn (headless-head-move session-id origin "turn" "redo")
           (format nil "redone: the head is back on ~a" origin))
    "nothing to redo"))

(defun headless-clear (args session-id)
  (declare (ignore args))
  (require-headless-session session-id "clear")
  (cond ((null (nlk:session-head-turn-id session-id))
         "nothing to clear: the session is at its start")
        (t (headless-head-move session-id nil "turn" "rewind")
           "cleared: the session is back at its start; /redo returns")))

(defun model-catalog-list-text (provider)
  "The catalog the picker shows, as text: every provider with its count —
each row naming its own drill-down — or one provider's ids with its
listing failure beside them. The /models list answer."
  (if provider
      (multiple-value-bind (ids listing-error) (models provider)
        (format nil "~a: ~d model~:p~@[ (listing: ~a)~]~{~%  ~a~}"
                provider (length ids) listing-error ids))
      (format nil "~{~a~^~%~}"
              (loop for (provider . count) in (models)
                    collect (format nil "~a: ~d model~:p — /models list ~a"
                                    provider count provider)))))

(defun headless-models (args session-id &aux (tokens (split-words args))
                                             (aux (equalp (first tokens) "aux"))
                                             (picks (if aux (rest tokens) tokens)))
  "The room's /models: the session target's read, the catalog as text, or a
pick landed on SESSION-ID; after `aux', the auxiliary model's read, or a
pick landed on it -- this image's auxiliary target and general.auxiliary_model
together, pinning no session -- `default' taking it out."
  (cond
    ((and (not aux) (equalp (first tokens) "list"))
     (model-catalog-list-text (second tokens)))
    ((and (equalp (first tokens) "decider") (<= (length tokens) 3))
     (bind ((picks (rest tokens))
            ((_ _ outcome detail)
             (cond ((null picks) (values))
                   ((equalp picks '("none")) (select-decider-model nil))
                   (t (select-decider-model (first picks) :model (second picks))))))
       (multiple-value-bind (id model) (decider-model)
         (format nil "~a~@[~%shared config: ~a~]~:[~;~%/models decider <id> picks one (~{~a~^, ~}); /models decider none takes it out~]"
                 (decider-model-summary id model)
                 (when (eq outcome :failed) (format nil "NOT saved — ~a" detail))
                 (null picks) (mapcar #'first +deciders+)))))
    ((> (length picks) 2)
     "usage: /models [provider] [model] | aux [provider] [model | default] | decider [id | none] | list [provider]")
    ((and aux (null picks))
     (format nil "~a~%/models aux <provider> <model> changes it; /models aux default gives its work back to each session's own model"
             (auxiliary-target-summary)))
    (aux
     (bind (((_ _ outcome detail)
             (if (equalp picks '("default"))
                 (select-auxiliary-model nil)
                 (select-auxiliary-model (first (last picks))
                                         :provider (when (= 2 (length picks)) (first picks))))))
       (format nil "~a~@[~%shared config: ~a~]" (auxiliary-target-summary)
               (when (eq outcome :failed)
                 (format nil "NOT saved — ~a" detail)))))
    ((null tokens)
     (multiple-value-bind (pin-provider pin-model)
         (and (stringp session-id) (nlk:session-model-selection session-id))
       (format nil "~a~@[~%session pin: ~a~]~%~
                      /models <provider> <model> changes it, /models aux <provider> <model> the auxiliary model; ~
                      /models list shows the catalog"
               (model-selection-summary)
               (and (or pin-provider pin-model)
                    (format nil "~a/~a" (or pin-provider "-")
                            (or pin-model "-"))))))
    (t
     (require-headless-session session-id "models")
     (select-model (first (last tokens))
                   :provider (when (= 2 (length tokens)) (first tokens))
                   :session-id session-id))))

(defun headless-think (args session-id &aux (rung (string-trim '(#\Space #\Tab) args)))
  (cond
    ((zerop (length rung))
     (bind (((provider model _ _ _ effort) (gateway-target (and (stringp session-id) session-id)))
            (ladder (effort-ladder provider model)))
       (if (null ladder)
           (format nil "~a/~a offers no reasoning control" provider model)
           (format nil "reasoning effort for ~a/~a: ~a~%ladder: ~{~a~^ ~}~%~
                            /think <rung> sets it for this session and every new one from the next turn; /think default picks the model's own"
                   provider model
                   (or (effective-effort effort provider model) "provider default")
                   ladder))))
    (t
     (require-headless-session session-id "think")
     (select-effort (if (string-equal rung "default") nil rung)
                    :session-id session-id))))

(defun headless-rename (args session-id)
  "The /rename answer: SESSION-ID's name, ARGS made its name, or with
`default' the name its first prompt makes given back (SET-SESSION-TITLE)."
  (require-headless-session session-id "rename")
  (cond
    ((nlk:on-standby-p session-id)
     "nothing to name yet: a session is named once it has a prompt")
    ((zerop (length args))
     (format nil "this session is named ~s~%~
                  /rename NAME renames it; /rename default gives back the name its first prompt makes"
             (nlk::session-title session-id)))
    (t (format nil "renamed: ~a"
               (set-session-title session-id (if (string-equal args "default") "" args))))))

(defun slash-rename-op (model args)
  "/rename on a shell: the gateway's HEADLESS-RENAME for this shell's session."
  ;; The name is a fact in the gateway's store, so that image answers, the
  ;; same text a room or the web page's composer gets.
  (with-slash-notice ("rename")
    (json-value (gateway-slash-answer model (format nil "/rename ~a" args)) :string "text")))

(defun markdown-export-path (args name)
  "Where /export ARGS writes the file the gateway named NAME: ARGS as a path
from this shell's folder, NAME inside it when it is a folder, NAME in this
shell's folder when ARGS is empty."
  (let* ((here (uiop:ensure-directory-pathname (workspace-directory)))
         (given (and (plusp (length args)) (merge-pathnames (nlk:expand-home args) here))))
    (cond ((null given) (merge-pathnames name here))
          ((or (uiop:directory-pathname-p given) (uiop:directory-exists-p given))
           (merge-pathnames name (uiop:ensure-directory-pathname given)))
          (t given))))

(defun slash-export-op (model args)
  "/export [PATH]: this shell's session as Markdown, in a file where this
shell runs."
  ;; The gateway writes the text (SESSION-MARKDOWN) and this shell the file,
  ;; so a shell attached from another machine keeps the file on its own.
  (if (home-screen-p model)
      "nothing to export yet: the session has no prompt"
      (notice-off-loop
       model "export: writing…"
       (lambda ()
         (with-slash-notice ("export")
           (multiple-value-bind (octets headers)
               (gateway-model-http-call
                model :get (format nil "/api/gateway/sessions/~a/export?format=markdown"
                                   model.session-id)
                nil :raw t)
             (let ((path (markdown-export-path
                          args (or (ppcre:register-groups-bind (name)
                                       ("filename=\"([^\"/\\\\]+)\"" (gethash "content-disposition" headers))
                                     name)
                                   "session.md"))))
               (ensure-directories-exist path)
               (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
               (format nil "exported to ~a" (uiop:native-namestring path)))))))))

(defun headless-evict (args session-id)
  (require-headless-session session-id "evict")
  ;; Why nothing went, or the Zig contextEvictMessage ack — the realized
  ;; eviction, not the request. A turn in flight refuses, as the route does:
  ;; its boundary would be planned over a moving log. What went is published,
  ;; so a shell attached to the session sees the divider whoever asked.
  (multiple-value-bind (percent through valid-p) (evict-args args)
    (let* ((busy (and valid-p (nlk:active-turn-p session-id)))
           (cursor (and valid-p (not busy) (session-cursor session-id)))
           (plan (and valid-p (not busy)
                      (evict-session-context session-id :percent percent :through through))))
      (when plan (publish-turn-facts session-id cursor))
      (cond ((not valid-p) *evict-usage-hint*)
            (busy "a turn is in flight; cancel or await it first")
            ((null plan) (or (evict-decline-text session-id :through through) "nothing to evict"))
            (t (format nil "Evicted ~d turn~:p (~d% of history context) · ~d message~:p retained"
                       (getf plan :evicted-turn-count)
                       (getf plan :realized-percent)
                       (getf plan :retained-message-count)))))))

(defun headless-cells (args session-id)
  "This process's folder report — the same local-process rule as /gc."
  (declare (ignore args session-id))
  (render-cells (nlk:cells)))

(defun headless-doctor (args session-id)
  "What is missing rather than what loaded, then what only a look can say
(DOCTOR-FACTS): the facts describe THIS image — its model pin, its folder,
its store."
  (declare (ignore args session-id))
  (health-report-text :facts (doctor-facts)))

(defun notice-off-loop (model placeholder thunk)
  "PLACEHOLDER as a shell's answer now, and the text THUNK answers put in the
composer status once it lands off the loop: a slash whose work takes seconds."
  ;; Without a loop (a test drives the op) THUNK's text is the answer itself.
  (cond ((null model.program) (funcall thunk))
        (t (off-loop model thunk
                     (lambda (model text)
                       (show-status model (or (off-loop-failure text) text)
                                    :error-p (and (off-loop-failure text) t))
                       (pin-transcript-bottom model)
                       (render-state model)))
           placeholder)))

(defun slash-doctor-op (model args)
  "/doctor on a shell: the gateway's HEADLESS-DOCTOR, asked for off the loop."
  ;; The organism a shell drives is its gateway's: the pin a turn binds, the
  ;; folder that started, the store it serves are that image's, and this is
  ;; what the web page's Check everything shows. An attached shell's own image
  ;; holds no store and binds no turn.
  (declare (ignore args))
  (notice-off-loop model "doctor: looking…"
                   (lambda () (json-value (gateway-slash-answer model "/doctor") :string "text"))))

(defun headless-gc (args session-id &aux (arg (string-trim " " args)))
  "The collection counters of this image — the one whose pauses the
operator feels — or, for `reset', a fresh attribution window."
  (declare (ignore session-id))
  (cond
    ((zerop (length arg))
     (render-collection-report (nlk:collection-stats)
                               (nlk:gc-attribution-stats)))
    ((string-equal arg "reset")
     (nlk:reset-gc-attribution)
     "gc attribution window reset")
    (t "Usage: /gc [reset]")))

;;; --- /context as a picture --------------------------------------------------
;;; A shell paints /context as a docked picker: the window as a grid of
;;; cells, each part of the prompt in its own tone, the free room and the
;;; stretch past the eviction line under them, and the parts as the list
;;; beside it, each opening to what it holds -- history to its turns, where
;;; Enter evicts through one. The parts are the record's estimate
;;; (SESSION-CONTEXT-ESTIMATE) scaled to the newest round's measured prompt
;;; when one exists, so the grid and the header lane read one occupancy.

(defparameter +context-parts+
  '((:prompt "standing prompt" "context-prompt")
    (:tools "tools" "context-tools")
    (:index "skills and memories" "context-index")
    (:notes "notes" "context-notes")
    (:history "history" "thought"))
  "The parts /context splits the window into, in the grid's order, as (KEY
LABEL TONE): TONE the theme tone a part's cells and chip paint in.")

(defparameter +context-usage+
  "Usage: /context [prompt | tools | index | notes | history | time]"
  "What /context answers an argument it does not open.")

(defparameter +context-grid-width+ 40
  "The grid's cells across; with +CONTEXT-GRID-HEIGHT+ down, one cell is
1/400 of the window.")

(defparameter +context-grid-height+ 10
  "The grid's cells down: two to a text row, so five rows.")

(defparameter +context-palette+
  (append '("context-free" "context-zone" "context-other" "disclosure-hover")
          (mapcar #'third +context-parts+))
  "The grid's palette (LIST-DIALOG's picture): the free cells, the free cells
past the eviction line, another part's under a part's own view, a turn an
eviction takes, then each part's tone in +CONTEXT-PARTS+ order.")

(defparameter +context-section-names+
  '(("operator-rules" . "operator rules · AGENTS.md")
    ("instructions" . "project instructions")
    ("help" . "help topics")
    ("resumed" . "resume note"))
  "The words /context's prompt view names a harness section by, where its
own name is not them.")

(defun context-palette-char (entry)
  "The picture character that paints +CONTEXT-PALETTE+'s ENTRY."
  (digit-char (position entry +context-palette+ :test #'equal) 36))

(defun context-tone (key)
  "The tone part KEY paints in, as the keyword a row's mark takes."
  (intern (string-upcase (third (assoc key +context-parts+))) :keyword))

(defun context-breakdown (session-id provider model-id context-usage)
  "SESSION-ID's window split into +CONTEXT-PARTS+ as a plist: :TOTAL the
occupancy (CONTEXT-USAGE's measured prompt, else the record's estimate),
:ESTIMATED whether it is the estimate, :PARTS each part as (:KEY :TOKENS
:ITEMS), ITEMS (TEXT TOKENS) pairs, and :UNITS the retained history units
(HISTORY-UNIT-FACTS) with :TOKENS scaled like the parts. NIL when the
record holds nothing to estimate."
  (multiple-value-bind (estimate parts) (session-context-estimate session-id provider model-id)
    (when (and estimate (plusp estimate))
      (let* ((measured (and context-usage (context-occupancy-tokens context-usage)))
             (total (if (and measured (plusp measured)) measured estimate))
             (factor (/ total estimate))
             (sections (getf parts :sections))
             (notes (loop for unit in (getf parts :units) append (getf unit :notes))))
        (labels ((scaled (tokens) (round (* factor tokens)))
                 (part (key tokens pairs)
                   (list :key key :tokens tokens
                         :items (loop for (text . n) in pairs collect (list text (scaled n))))))
          (let* ((index (scaled (or (cdr (assoc "index" sections :test #'string=)) 0)))
                 (prompt (max 0 (- (scaled (getf parts :prompt)) index)))
                 (tools (scaled (getf parts :tools)))
                 (noted (scaled (reduce #'+ notes :key #'cdr)))
                 (units (loop for unit in (getf parts :units)
                              collect (list* :tokens (scaled (getf unit :tokens)) unit))))
            (list :total total
                  :estimated (not (and measured (plusp measured)))
                  :units units
                  :parts (list (part :prompt prompt
                                     (remove "index" sections :key #'car :test #'string=))
                               (part :tools tools (getf parts :tool-sizes))
                               (part :index index '())
                               (part :notes noted notes)
                               (list :key :history
                                     :tokens (max 0 (- total prompt tools index noted))
                                     :items (loop for unit in units
                                                  collect (list (getf unit :prompt)
                                                                (getf unit :tokens))))))))))))

(defun context-scale (breakdown window)
  "The tokens the grid's whole spans: WINDOW, or BREAKDOWN's total when no
source knows the window."
  (or window (max 1 (getf breakdown :total))))

(defun context-share (tokens breakdown window)
  "TOKENS as a share of the grid's whole, `36.0%'."
  (format-occupancy-percent tokens (context-scale breakdown window)))

(defun context-grid-picture (breakdown window &key focus (width +context-grid-width+)
                                                     (height +context-grid-height+))
  "The grid LIST-DIALOG paints for BREAKDOWN, as rows of +CONTEXT-PALETTE+
characters: WIDTH cells across and HEIGHT down, each an equal share of
WINDOW, filled in +CONTEXT-PARTS+ order, then the free room, the zone's past
the eviction line. FOCUS, a part's key, paints the other parts' cells muted."
  (let* ((cells (* width height))
         (unit (/ (context-scale breakdown window) cells))
         (spans '())
         (start 0))
    ;; Each part a span of the window.
    (flet ((span (tokens entry)
             (push (list start (+ start tokens) (context-palette-char entry)) spans)
             (incf start tokens)))
      (dolist (part (getf breakdown :parts))
        (destructuring-bind (&key key tokens &allow-other-keys) part
          (span tokens (if (and focus (not (eq focus key)))
                           "context-other"
                           (third (assoc key +context-parts+)))))))
    (loop for row below height
          collect (let ((line (make-string width)))
                    (dotimes (column width line)
                      (let* ((middle (* unit (+ 0.5 column (* row width))))
                             (span (find-if (lambda (span) (and (<= (first span) middle)
                                                                (< middle (second span))))
                                            spans)))
                        (setf (char line column)
                              (cond (span (third span))
                                    ((and window (>= middle (* window *context-eviction-trigger-ratio*)))
                                     (context-palette-char "context-zone"))
                                    (t (context-palette-char "context-free"))))))))))

(defparameter +context-strip-cells+ 10
  "The cells of the grid's one-row rendition, a room's card's (LIST-DIALOG's
:STRIP): a tenth of the window each.")

(defun context-picture (rows &optional strip)
  "The picture LIST-DIALOG carries for the grid ROWS: beside the list where
the panel is wide enough; STRIP, its one-row rendition."
  (list :rows rows :palette +context-palette+ :beside t :strip strip
        :label "The window as a grid"))

(defun context-grid (breakdown window &key focus)
  "The picture of BREAKDOWN in WINDOW (CONTEXT-PICTURE): the grid, and the
same window in +CONTEXT-STRIP-CELLS+ cells; FOCUS as CONTEXT-GRID-PICTURE's."
  (context-picture (context-grid-picture breakdown window :focus focus)
                   (first (context-grid-picture breakdown window :focus focus
                                                                 :width +context-strip-cells+ :height 1))))

(defun context-occupancy-tone (tokens window)
  "The tone TOKENS in WINDOW warm to: :DANGER past the eviction line, :WARN
from three quarters of the window, else NIL -- and NIL with no window."
  (let ((share (and window (plusp window) (/ tokens window))))
    (cond ((null share) nil)
          ((>= share *context-eviction-trigger-ratio*) :danger)
          ((>= share 3/4) :warn))))

(defun context-summary (label grade window total)
  "The line under /context's title => (values TEXT TONE): what it runs on,
where the window came from and where eviction starts -- or, in the danger
tone, that the next request evicts."
  (let ((trigger (round (* 100 *context-eviction-trigger-ratio*))))
    (cond ((null window)
           (values (format nil "~@[~a · ~]window unknown · nothing evicts until a window is known" label)
                   :muted))
          ((>= total (* window *context-eviction-trigger-ratio*))
           (values (format nil "past the ~d% line: the next request evicts the oldest turns" trigger)
                   :danger))
          (t (values (format nil "~@[~a · ~]~a window · evicts at ~d% (~a), down to ~d%"
                             label grade trigger
                             (format-token-count (* window *context-eviction-trigger-ratio*))
                             (round (* 100 *context-eviction-floor-ratio*)))
                     :muted)))))

(defun context-title-context (breakdown window &aux (total (getf breakdown :total))
                                                    (mark (if (getf breakdown :estimated) "~" "")))
  "The muted words beside /context's title: `88.5k of 200k · 44.3%', `~'
before an estimate."
  (if window
      (format nil "~a~a of ~a · ~a" mark (format-token-count total)
              (format-token-count window) (format-occupancy-percent total window))
      (format nil "~a~a" mark (format-token-count total))))

(defun context-part-label (part breakdown)
  "PART's row label: its name, and how many it holds where that says more."
  (let ((name (second (assoc (getf part :key) +context-parts+))))
    (case (getf part :key)
      ((:tools :notes) (format nil "~a · ~d" name (length (getf part :items))))
      (:history (format nil "~a · ~d turn~:p" name (length (getf breakdown :units))))
      (t name))))

(defun context-amount (tokens breakdown window)
  "A row's detail: TOKENS and their share, right-aligned so rows line up."
  (format nil "~6@a  ~6@a" (format-token-count tokens) (context-share tokens breakdown window)))

(defun context-dialog (breakdown window summary tone rounds trailing)
  "The docked picker /context answers with: the grid beside the parts, each
opening its own view (/context KEY; the index, /index), the free room, and
the over-time row when ROUNDS (CONTEXT-ROUNDS, TRAILING its second value)
holds any."
  (list-dialog
   :title "Context"
   :context (context-title-context breakdown window)
   :summary summary :summary-tone tone
   ;; The state a room's card wears: a known window is fine until it warms.
   :tone (and window (or (context-occupancy-tone (getf breakdown :total) window) :success))
   :picture (context-grid breakdown window)
   :hint "enter open  esc close  up/down navigate"
   :rows (append
          ;; A part holding nothing is no row; history always is one.
          (loop for part in (getf breakdown :parts)
                for key = (getf part :key)
                when (or (eq key :history) (plusp (getf part :tokens)))
                collect (list :label (context-part-label part breakdown)
                              :detail (context-amount (getf part :tokens) breakdown window)
                              :mark "██" :mark-tone (context-tone key)
                              :run (list :enter (if (eq key :index)
                                                    "/index"
                                                    (format nil "/context ~(~a~)" key)))))
          ;; No window, no free room: the grid's whole is then what is in it,
          ;; and its free row would read a measured zero.
          (when window
            (let ((free (max 0 (- window (getf breakdown :total)))))
              (list (list :label "free" :detail (context-amount free breakdown window)
                          :mark "██" :mark-tone :context-free))))
          (when rounds
            (list (list :label "over time"
                        :detail (format nil "~d round~:p · ~d eviction~:p"
                                        (length rounds) (+ (count-if #'second rounds)
                                                           (if trailing 1 0)))
                        :mark "▸" :mark-tone :muted
                        :run (list :enter "/context time")))))))

(defun context-first-line (text)
  "TEXT's first non-blank line, trimmed: what a row names a prompt or note by."
  (find-if #'plusp (mapcar #'trimmed (lines text)) :key #'length))

(defun context-history-rows (breakdown window)
  "The history view's rows: each retained turn by its number and size, in
the order the turns end -- where an eviction through one cuts -- and, the
selected row's, what evicting through it frees, which Enter does (/evict
through TURN); the newest turn and a running one say why they stay."
  ;; A turn's late fact (a background exit) is a unit of its own after the
  ;; next turn began: the turn's row gathers its units and stands where its
  ;; last one does, since an eviction through it takes every unit up to there.
  (let* ((units (coerce (getf breakdown :units) 'vector))
         (count (length units))
         (cap (min (1- count) (1+ (or (position-if (lambda (unit) (getf unit :terminal)) units
                                                   :from-end t)
                                      -1))))
         (turns '())
         (total (getf breakdown :total)))
    (loop for unit across units
          for index from 0
          for id = (getf unit :turn-id)
          for turn = (or (find id turns :key (lambda (turn) (getf turn :turn-id)) :test #'equal)
                         ;; Every key a row reads is in place from the start, so
                         ;; the SETF GETFs below write this plist, not a copy.
                         (car (push (list :turn-id id :number (getf unit :number) :tokens 0
                                          :last 0 :terminal nil :prompt "")
                                    turns)))
          do (incf (getf turn :tokens) (getf unit :tokens))
             (setf (getf turn :last) index
                   (getf turn :terminal) (getf unit :terminal))
             (unless (context-first-line (getf turn :prompt))
               (setf (getf turn :prompt) (getf unit :prompt))))
    (loop for turn in (sort turns #'< :key (lambda (turn) (getf turn :last)))
          for evictable = (< (getf turn :last) cap)
          for freed = (reduce #'+ units :end (1+ (getf turn :last))
                                        :key (lambda (unit) (getf unit :tokens)))
          collect (list :key (format nil "t~d" (getf turn :number))
                        :label (or (context-first-line (getf turn :prompt)) "(no prompt in the window)")
                        ;; Beside the grid a row has some forty columns: the
                        ;; size, and on the selected row what Enter frees --
                        ;; the hint row names the key.
                        :detail (format nil "~6@a" (format-token-count (getf turn :tokens)))
                        :suffix (cond ((not evictable)
                                       (if (getf turn :terminal) "  · kept: newest" "  · kept: running"))
                                      (t (format nil "  −~a → ~a"
                                                 (format-token-count freed)
                                                 (context-share (max 0 (- total freed)) breakdown window))))
                        :run (and evictable
                                  (list :enter (format nil "/evict through ~a" (getf turn :turn-id))))))))

(defun context-part-answer (key breakdown window)
  "/context KEY => (values TEXT DIALOG): the part's items, largest first --
history's turns oldest first -- as lines, and as the picker that shows them
beside the grid with the other parts muted."
  (let* ((part (find key (getf breakdown :parts) :key (lambda (part) (getf part :key))))
         (rows (if (eq key :history)
                   (context-history-rows breakdown window)
                   (loop for (text tokens) in (sort (copy-list (getf part :items)) #'> :key #'second)
                         collect (list :label (or (cdr (assoc text +context-section-names+ :test #'string=))
                                                  (context-first-line text)
                                                  "(empty)")
                                       :detail (context-amount tokens breakdown window)))))
         (about (ecase key
                  (:prompt "sent with every request; no eviction reaches it")
                  (:tools "each tool's schema, sent with every request; /cells unloads a cell's")
                  (:index "the skills and memories index; /index opens it")
                  (:notes "what the model wrote under [[note]] in context.txt; they leave with their turn")
                  (:history "the conversation; the oldest turns leave first at the eviction line"))))
    (values (format nil "~a · ~a~%~{~a~%~}" (second (assoc key +context-parts+))
                    (context-amount (getf part :tokens) breakdown window)
                    (loop for row in rows
                          collect (format nil "  ~@[~a  ~]~a  ~a"
                                          (getf row :key) (getf row :label) (getf row :detail))))
            (dialog-json
             (list-dialog
              :title (format nil "Context / ~a" (second (assoc key +context-parts+)))
              :context (context-amount (getf part :tokens) breakdown window)
              :summary about :summary-tone :muted
              :picture (context-grid breakdown window :focus key)
              :hint (if (eq key :history)
                        "enter evict through  esc back  up/down navigate"
                        "esc back  up/down navigate")
              :empty-label "Nothing here"
              :rows rows)))))

(defun context-rounds (session-id &aux (rounds '()) (pending nil))
  "SESSION-ID's provider rounds, oldest first, each (TOKENS EVICTION): the
prompt the round placed in the window (CONTEXT-OCCUPANCY-TOKENS over its
turn.usage fact) and the session.context.evicted fact that moved the floor
since the round before, NIL for none. The second value is an eviction no
round has followed yet."
  (dolist (event (nlk:events :session-id session-id
                             :kind (list "turn.usage" "session.context.evicted")
                             :order :oldest :as :instances)
                 (values (nreverse rounds) pending))
    (typecase event
      (nlk:context-evicted-event (setf pending event))
      (nlk:turn-usage-event
       ;; A round that reported no prompt (a voided attempt, a side
       ;; call's empty usage) placed nothing in the window.
       (unless (nlk:turn-usage-event-voided event)
         (let ((state (make-hash-table :test #'equal)))
           (apply-event state event)
           (let ((tokens (context-occupancy-tokens (gethash "context_usage" state))))
             (when (plusp tokens)
               (push (list tokens pending) rounds)
               (setf pending nil)))))))))

(defun context-time-picture (rounds scale window)
  "ROUNDS (CONTEXT-ROUNDS) as a column chart in the grid's palette: each of
+CONTEXT-GRID-WIDTH+ columns the largest prompt of the rounds it spans,
over SCALE tokens, a column holding the first round after an eviction in
the eviction's tone."
  (let* ((count (length rounds))
         (columns (loop for column below +context-grid-width+
                        for from = (floor (* column count) +context-grid-width+)
                        for to = (max (1+ from) (floor (* (1+ column) count) +context-grid-width+))
                        for span = (subseq rounds (min from (1- count)) (min to count))
                        collect (cons (reduce #'max span :key #'first)
                                      (some #'second span)))))
    (loop for row below +context-grid-height+
          for level = (- +context-grid-height+ row)
          collect (coerce
                   (loop for (tokens . evicted) in columns
                         collect (context-palette-char
                                  (cond ((>= (* +context-grid-height+ (/ tokens scale)) (- level 0.5))
                                         (if evicted "disclosure-hover" "thought"))
                                        ((and window (>= (/ (- level 0.5) +context-grid-height+)
                                                         *context-eviction-trigger-ratio*))
                                         "context-zone")
                                        (t "context-free"))))
                   'string))))

(defun context-eviction-row (label eviction before after)
  "The over-time row of EVICTION, a session.context.evicted fact: LABEL (the
round it came before), the turns it took, the prompt BEFORE it and AFTER."
  (list :key "▾"
        :label (format nil "~a · ~d turn~:p" label (nlk:context-evicted-turn-count eviction))
        :detail (format nil "~a → ~a · ~a"
                        (format-token-count before)
                        (format-token-count after)
                        (let ((reason (nlk:context-evicted-reason eviction)))
                          (dispatch reason equal
                            ("operator_requested" "asked") ("budget_fill" "at the line")
                            ("provider_rejected" "refused by the provider") (t reason))))))

(defun context-time-answer (rounds trailing breakdown window)
  "/context time => (values TEXT DIALOG): each round's prompt from the log as
a chart, and each eviction as a row -- the round it came before, the turns it
took, the prompt before and after it; TRAILING, one no round has followed,
against the occupancy now."
  (if (null rounds)
      "no provider round in the log yet"
      (let* ((peak (reduce #'max rounds :key #'first))
             (scale (or window (max 1 peak)))
             (now (if breakdown (getf breakdown :total) (first (car (last rounds)))))
             (rows (append
                    (loop for (tokens eviction) in rounds
                          for previous = 0 then before
                          for before = tokens
                          for number from 1
                          when eviction
                            collect (context-eviction-row (format nil "round ~d" number)
                                                          eviction previous tokens))
                    (when trailing
                      (list (context-eviction-row "now" trailing (first (car (last rounds))) now)))
                    (list (list :label "peak" :detail (format-token-count peak))
                          (list :label "now" :detail (format-token-count now))))))
        (values (format nil "~d round~:p · peak ~a~%~{~a~%~}" (length rounds) (format-token-count peak)
                        (loop for row in rows
                              collect (format nil "  ~a  ~a" (getf row :label) (getf row :detail))))
                (dialog-json
                 (list-dialog
                  :title "Context / over time"
                  :context (format nil "~d round~:p · top: ~a" (length rounds) (format-token-count scale))
                  :summary "each round's prompt, read from the session log" :summary-tone :muted
                  :picture (context-picture (context-time-picture rounds scale window))
                  :hint "esc back  up/down navigate"
                  :rows rows))))))

(defun headless-context (args session-id)
  "The /context answer without a shell: SESSION-ID's usage facts folded
from its durable turn.usage events (the newest 512 rounds) and the window
belief for its resolved target -- the text, and the docked picker a shell
paints it as (CONTEXT-DIALOG). ARGS opens a part (+CONTEXT-PARTS+'s keys)
or `time', each round's prompt from the log."
  ;; A session with no round at all has its occupancy estimated from the
  ;; durable record (SESSION-CONTEXT-ESTIMATE), and a window no source knows
  ;; is reported with the floor the evidence proves.
  (require-headless-session session-id "context")
  (let* ((asked (string-downcase (string-trim " " (or args ""))))
         (part (find asked +context-parts+ :key #'first :test #'string-equal))
         (rows (nlk:events :session-id session-id :kind "turn.usage"
                           :order :newest :limit 512 :as :instances))
         (newest (first rows))
         (session-state (make-hash-table :test #'equal))
         (turn-state (make-hash-table :test #'equal))
         (context-state (make-hash-table :test #'equal)))
    (unless (or part (zerop (length asked)) (string= asked "time"))
      (return-from headless-context +context-usage+))
    ;; Each fact folds through the fold's own turn.usage reducer (APPLY-EVENT).
    (dolist (event rows) (apply-event session-state event))
    (dolist (event rows) (when (equal newest.turn-id event.turn-id) (apply-event turn-state event)))
    (when newest (apply-event context-state newest))
    ;; The window is the one the session's next turn binds (GATEWAY-TARGET:
    ;; its pin over the gateway's default), so a room that has run no round
    ;; yet still knows it; only an unconfigured default, which names no model
    ;; a person chose, gives way to the newest round's.
    (bind (((target-provider target-model _ _ unconfigured) (gateway-target session-id))
           (provider (if (and unconfigured newest)
                         (nlk:turn-usage-event-provider newest)
                         target-provider))
           (model-id (if (and unconfigured newest)
                         (nlk:turn-usage-event-model newest)
                         target-model))
           (effort (and newest (nlk:turn-usage-event-reasoning-effort newest)))
           (label (status-identity-with-effort provider model-id effort))
           (context-usage (gethash "context_usage" context-state))
           ;; A prompt measured before the newest eviction counts turns the
           ;; window no longer holds: the record's estimate is the occupancy
           ;; until the next round reports one.
           (evicted (and newest
                         (nlk:events :session-id session-id :kind "session.context.evicted"
                                     :order :newest :limit 1 :as :instance)))
           (measured (and context-usage
                          (not (and evicted (> (nlk:event-log-position evicted)
                                               (nlk:event-log-position newest))))
                          context-usage))
           ((window grade) (context-window-view provider model-id))
           (breakdown (context-breakdown session-id provider model-id measured)))
      (cond
        ((string= asked "time")
         (multiple-value-call #'context-time-answer (context-rounds session-id) breakdown window))
        (part
         (if breakdown
             (context-part-answer (first part) breakdown window)
             "nothing in the window yet"))
        (t
         (values (context-report-text provider model-id
                                      :label label
                                      :session-id session-id
                                      :usage (gethash "usage" turn-state)
                                      :session-usage (gethash "session_usage" session-state)
                                      :context-usage context-usage)
                 (and breakdown
                      (dialog-json
                       (multiple-value-bind (summary tone)
                           (context-summary label grade window (getf breakdown :total))
                         (multiple-value-call #'context-dialog breakdown window summary tone
                           (context-rounds session-id)))))))))))

(defun slash (line &key (session-id nlk:*scribe-session-id*))
  "Run one slash command LINE — \"/models grok-4.6\", \"/think high\",
\"/help\" — without a shell, against SESSION-ID (by default the session
this eval is running for), and answer the text a shell would show; the
second value is the panel a cell's command answered with (DIALOG-JSON),
for a caller that draws one."
  ;; The builtins with a headless arm (SLASH-COMMAND-HEADLESS) land on the
  ;; same canonical surfaces the shells' /models, /think, /undo, /redo,
  ;; /clear, /evict, /context, /cells and /gc do; a cell's command is an
  ;; entry of the same shape whose arm runs it (REGISTER-COMMAND);
  ;; (SLASH-CATALOG) lists them all and /help renders that list. A command
  ;; whose meaning is a dialog or a shell move answers that it needs an
  ;; interactive shell; an unknown name says so. Operation failures signal, as
  ;; the verbs they land on do — an unknown lane, an off-ladder effort, a turn
  ;; in flight.
  (check-type line string)
  (multiple-value-bind (name args) (parse-slash-input line)
    (unless name
      (error "not a slash command: ~s" line))
    (let ((entry (or (find-builtin-slash-command name) (find-registered-command name))))
      (cond
        ((null entry) (format nil "unknown command /~a; /help lists them" name))
        (entry.headless (funcall entry.headless args session-id))
        (t (format nil "/~a needs an interactive shell" entry.name))))))

(defun model-pick-body (tokens)
  "The body a pick of TOKENS posts to a model route: (MODEL) or (PROVIDER MODEL)."
  (json-object :opt "provider" (and (= 2 (length tokens)) (first tokens))
               "model" (first (last tokens))))

(defun shared-config-failure (object)
  "Why a model route's answer OBJECT says the shared config was not saved, or NIL."
  (and (equal "failed" (json-value object :string "shared_config"))
       (json-value object :string "shared_config_error")))

(defun slash-models-op (model tokens)
  "Show (no TOKENS) or change (1 token: model; 2 tokens: provider model)
the active selection. Returns notice text: the effective selection, or the
pick landed on THIS SESSION (SELECT-SESSION-MODEL): the durable per-session
fact outranks the gateway default for this session's turns and survives
restarts, and the pick is the default new sessions start on."
  (with-slash-notice ("model selection")
      (if (null tokens)
          (let ((object (gateway-model-http-call model :get
                                                 "/api/gateway/model" nil))
                (facts (gateway-model-facts model)))
            (format nil "provider: ~a~%model: ~a~%endpoint: ~a~%~a~
                         ~@[~%session override: ~a~]"
                    (gethash "provider" object)
                    (gethash "model" object)
                    (or (json-value object :string "endpoint")
                        (format nil "none: ~a" (json-value object :text "refusal")))
                    (apply #'auxiliary-model-summary
                           (mapcar (lambda (key) (json-value object :string "auxiliary" key))
                                   '("provider" "model" "effort")))
                    (when (getf facts :pinned)
                      (format nil "~@[~a ~]~a"
                              (getf facts :pin-provider)
                              (or (getf facts :pin-model) "(lane default)")))))
          (let ((object (session-control-call model :post "model" :body (model-pick-body tokens))))
            ;; The accepted selection moved the session's target, so the
            ;; cached facts — identity, pin provenance and the effort resolved
            ;; against the old target — are all stale together.
            (refresh-gateway-model-facts model)
            (let ((picked (json-value object :string "selected_model")))
              (format nil "session model: ~@[~a ~]~a~:[~; — new sessions start on it too~]~@[~%shared config: NOT saved — ~a~]"
                      (json-value object :string "selected_provider")
                      (or picked "(cleared)")
                      picked
                      (shared-config-failure object)))))))

(defun slash-default-model-op (model tokens)
  "Land TOKENS (1 token: model on the default's lane; 2 tokens: provider
model) as the gateway default the way Control's Settings does (POST
/api/gateway/model): every session not yet begun starts on it, and the
shared config keeps it. Returns notice text."
  ;; No session is pinned; a shell whose session has not begun learns from
  ;; the model_changed frame the op broadcasts.
  (with-slash-notice ("default model selection")
      (let ((object (gateway-model-http-call model :post "/api/gateway/model"
                                             (model-pick-body tokens))))
        (refresh-gateway-model-facts model)
        (format nil "default model: ~a/~a~@[~%shared config: NOT saved — ~a~]"
                (json-value object :string "provider")
                (json-value object :string "model")
                (shared-config-failure object)))))

(defun slash-auxiliary-model-op (model tokens &key (effort nil effort-p))
  "The arm behind the Auxiliary tab and `/models aux': show (no TOKENS) or
change (1 token: model; 2 tokens: provider model; `default': none) the
auxiliary model — the target a session's naming, a recap or a summary runs
on — with EFFORT, at that rung (NIL: the lightest). The gateway's
auxiliary-model op lands it (POST /api/gateway/auxiliary-model), or answers
it (GET)."
  ;; Lands the image's target and general.auxiliary_model together and pins no
  ;; session: no session facts move, so nothing is refreshed. Returns notice
  ;; text.
  (with-slash-notice ("auxiliary model selection")
      (let ((object (gateway-model-http-call
                     model (if tokens :post :get) "/api/gateway/auxiliary-model"
                     (cond ((equalp tokens '("default")) (json-object "model" :null))
                           (effort-p (json-object "provider" (first tokens) "model" (second tokens)
                                                  "effort" (or effort :null)))
                           (tokens (model-pick-body tokens))))))
        (format nil "~a~@[~%shared config: NOT saved — ~a~]"
                (auxiliary-model-summary (json-value object :string "provider")
                                         (json-value object :string "model")
                                         (json-value object :string "effort"))
                (shared-config-failure object)))))

(defun slash-decider-model-op (model tokens)
  "The arm behind the Decider tab and `/models decider': show (no TOKENS) or
pin (a decider id, then optionally its model; `none' takes it out) the
decider general.decider_model names, through the gateway's decider-model op
(GET / POST /api/gateway/decider-model). Returns notice text."
  (with-slash-notice ("decider model selection")
      (let ((object (gateway-model-http-call
                     model (if tokens :post :get) "/api/gateway/decider-model"
                     (cond ((equalp tokens '("none")) (json-object "provider" :null))
                           (tokens (json-object "provider" (first tokens)
                                                :opt "model" (second tokens)))))))
        (format nil "~a~@[~%shared config: NOT saved — ~a~]"
                (decider-model-summary (json-value object :string "provider")
                                       (json-value object :string "model"))
                (shared-config-failure object)))))

(defun switch-gateway-session (model session-id)
  "Attach MODEL to SESSION-ID: reset folded state and watermarks, then
resubscribe — the replay that answers rebuilds the transcript."
  ;; A /sessions preview already holding SESSION-ID's folded history seeds the
  ;; new state (the Zig adoptSessionsDialogPreview promotion), so the
  ;; transcript carries the entered session from this frame on rather than
  ;; reading empty until the replay lands. The replay still rebuilds it
  ;; authoritatively.
  ;;
  ;; An empty destination is the home screen (Zig create_session +
  ;; isHomeScreen): the landing band is its feedback. A destination with
  ;; history acknowledges the switch in the five-second composer hint, never
  ;; in transcript chrome. Return NIL so slash/picker callers cannot put the
  ;; acknowledgement back at the transcript tail.
  (let* ((preview model.transcript-preview)
         (adopted (and (equal session-id (car preview)) (cdr preview))))
    (invalidate-gateway-model-facts model)
    ;; A found-block pin belongs to the session it was found in; the caller
    ;; that means to keep one pins again right after the switch.
    (clear-transcript-pin model)
    (setf (slot-value model 'session-id) session-id
          model.state (or adopted (initial-chat-state))
          ;; The preview lane empties either way, and the switch resubscribes
          ;; to its destination alone: the follow ends with the lane.
          model.transcript-preview nil
          model.preview-session-id nil
          model.preview-activity nil
          model.last-sequence nil
          model.last-cursor nil
          model.reconnect-required nil
          model.replay-pending-turn-id nil
          model.quiet-resync nil)
    (note-turn-flight-settled model)
    (clear-retract-stash model)
    ;; The pending start_turn belongs to the session we just left.
    ;; Keeping it fails the gateway home-screen-p gate and leaves a
    ;; blank band after /clear mid-turn.
    (clear-pending-command model)
    (show-status model nil)
    (when adopted
      ;; The adopted rows are the transcript now; paint them before the
      ;; subscribe rather than leaving the previous session's frame up.
      (pin-transcript-bottom model)
      (render-state model))
    (send-subscribe model.ws (list session-id))
    (note-shell-presence session-id)
    ;; The facts this shell held were the session it left: its pin, its
    ;; effort. The switch blanked them (INVALIDATE-GATEWAY-MODEL-FACTS) and
    ;; nothing else refetches for a switch -- the gateway broadcasts
    ;; MODEL_CHANGED only when a target moves, and a subscribe is
    ;; no fresh connection -- so without this read the footer and the turn
    ;; bar would say `setting up…' for as long as the shell stays here. Scoped
    ;; to the destination: the slot above already names it.
    (refresh-gateway-model-facts model)
    (when (transcript-rows model.state)
      (show-status model (format nil "Switched to ~a" session-id)))
    nil))

(defun slash-navigate-op (model direction)
  "Fire an undo/redo navigation. Returns notice text."
  (send-navigate-checkpoint model.ws
                            model.session-id
                            :direction direction)
  nil)

;;; --- /tree files preview + restore confirm stage --------------------------
;;; The Zig checkpoint chrome: the selected /tree row lazily fetches and
;;; shows its changed files ("3 file changes: a.zig, b.zig, c.zig, +2
;;; more"), and picking a user-prompt row interposes the two-option
;;; "Restore checkpoint" dialog before any navigation fires.

(defun checkpoint-file-change-label (count)
  "\"N file change(s)\" — the Zig fileChangeLabel (singular drops the s)."
  (format nil "~d file change~:p" count))

(defun checkpoint-file-list-label (paths &key (max-paths 3))
  "Up to MAX-PATHS paths \", \"-joined with a \", +K more\" overflow tail
(the Zig session_dialogs fileListLabel)."
  (let* ((shown (subseq paths 0 (min max-paths (length paths))))
         (overflow (- (length paths) (length shown))))
    (format nil "~{~a~^, ~}~@[, +~d more~]"
            shown (and (plusp overflow) overflow))))

(defun checkpoint-files-suffix (count &optional paths)
  "The selected-row files chrome: \"  ·  N file changes\" bare until the
path list lands, then \": p1, p2, p3, +K more\" (Zig dialogs.zig selected
tree-row composition). Empty PATHS keep the bare count."
  (format nil "  ·  ~a~@[: ~a~]"
          (checkpoint-file-change-label count)
          (and paths (checkpoint-file-list-label paths))))

(defun checkpoint-row-ensure-files (row fetch &aux (value (picker-row-value row))
                                                   (checkpoint-id (getf value :checkpoint-id)))
  "The lazy files arm for one highlighted /tree ROW (Zig
pendingSessionTreeCheckpointFiles): rows without a capture or with zero
count never fetch; a loaded row never refetches; FETCH => (values files
ok) — a failed fetch (OK NIL) leaves the row bare so the next highlight
retries. => ROW."
  ;; A row with a capture is fetched whether or not the listing carried a
  ;; count: the files answer IS the count (|paths a rewind would touch|,
  ;; the same computation the listing used to run for every row), so this
  ;; one call lands the number and the list together and the confirm
  ;; stage's `(N files changed)' label reads what the rewind would do.
  (when (and checkpoint-id (not (getf value :files-loaded)))
    (multiple-value-bind (files ok) (funcall fetch checkpoint-id)
      (when ok
        (let ((count (length files)))
          (setf (getf (picker-row-value row) :files-loaded) t
                (getf (picker-row-value row) :files) files
                (getf (picker-row-value row) :file-change-count) count
                (picker-row-suffix row)
                ;; No files: no chrome (a bare `0 file changes' is chrome
                ;; the Zig never drew either).
                (if (plusp count)
                    (checkpoint-files-suffix count files)
                    ""))))))
  row)

(defun checkpoint-option-rows (value &aux (count (getf value :file-change-count)))
  "The moves a picked prompt row offers, in order: continue from the turn —
the head lands on it, its answer kept, so the next prompt branches from there
(absent on the head row: the session is already there) — then rewind to
before its message, the prompt back in the composer, and the same rewind
with the code changes when a capture recorded them (VALUE's
:FILE-CHANGE-COUNT pre-rendered, the static label while it is unknown)."
  (append
   (unless (getf value :head-p)
     (list (make-picker-row :label "Continue from this turn"
                            :detail "new turns branch from its answer"
                            :value (list :move :continue))))
   (list (make-picker-row :label "Rewind to before this message"
                          :detail "the prompt returns to the composer"
                          :value (list :move :rewind :restore-files nil)))
   (when (getf value :checkpoint-id)
     (list (make-picker-row
            :label (if (integerp count)
                       (format nil "Rewind to before this message + code changes (~d file~:p changed)"
                               count)
                       "Rewind to before this message + code changes")
            :value (list :move :rewind :restore-files t))))))

(defun open-checkpoint-dialog (model value move &key preview)
  "The /tree confirm stage over a picked prompt row: its moves as options,
Enter calling MOVE with the chosen kind (:CONTINUE or :REWIND) and its
RESTORE-FILES-P, Esc closing without moving. PREVIEW, when given, draws the
highlighted move: a function of the option's value answering the painted rows
that stand in the header's third row (TREE-MOVE-ROWS), drawn again as the
highlight moves."
  ;; VALUE is the picked row's value plist: its :DETAIL (`turn 3 · branch') is
  ;; the tab's context and its :SUMMARY the header's third row. => NIL.
  (let ((rows (checkpoint-option-rows value)))
    (apply #'open-picker-dialog
           model "Checkpoint" rows
           (lambda-ignoring (model option)
             (funcall move (getf option :move) (getf option :restore-files))
             nil)
           :context (format nil " ~a" (or (getf value :detail) ""))
           :subtitle (or (json-value (getf value :summary) :text)
                         "Continue from this turn, or rewind to before its message")
           ;; The option rows keep the `> ' gutter on every profile.
           :option-marker t
           :footer-hints :checkpoint
           (when preview
             ;; The picture is a pure read of the graph the tree opened on, so
             ;; every landing draws at once.
             (list :summary-rows (funcall preview (picker-row-value (first rows)))
                   :highlight-delivery :live
                   :on-highlight (lambda (model row)
                                   (when row
                                     (setf (picker-dialog-summary-rows (chat-model-dialog model))
                                           (funcall preview (picker-row-value row)))))))))
  nil)

(defun checkpoint-row-excerpt (text)
  "TEXT's first line cut at 48 columns — the /tree row's excerpt of a prompt
or of a tool command — and \"\" for anything that is not a string."
  (if (stringp text)
      (subseq text 0 (min (or (position #\Newline text) (length text)) 48))
      ""))

(defun tree-lane-prefix (fork lanes)
  "The tree glyphs in front of a /tree or /sessions row: one two-column cell
per lane the row sits inside (LANES, outermost first, T while that lane's
line has more forks below), `│ ' down an open lane and blank down a closed
one, except the innermost cell on a FORK row — where a later line leaves its
parent — which reads `├ ' with more forks below or `╰ ' as the last."
  ;; So off turn1: `├ yay', `│ no', `├ hello!', `│ bye!', `╰ no!' — three
  ;; lines, not five siblings; and off a session two forks were taken from, `├
  ;; fork', `╰ fork'.
  (format nil "~{~a~}"
          (loop for (open . more) on lanes
                collect (cond (more (if open "│ " "  "))
                              ((and fork open) "├ ")
                              (fork "╰ ")
                              (open "│ ")
                              (t "  ")))))

;;; --- the /tree metro ------------------------------------------------------
;;; The picture the /tree rows paint: the session's turns in the order they
;;; ran, one row each, on a graph of one rail per line -- the head's line the
;;; leftmost rail, a line branched off it a rail of its own to the right,
;;; joined to the turn it left by a fork -- with the turn's clock time ahead
;;; of the rails and, flush right, how it ended, the steers it took, its calls
;;; and how long it ran (no bar: a log-scaled one would draw short and long
;;; runs alike beside the time that already says how long it ran). A
;;; reflection's recap is its note on the turn it follows, never a turn of its
;;; own: it folds under that turn. The confirm stage draws the move it offers
;;; on the same graph (TREE-MOVE-ROWS). The rows keep their slots -- label,
;;; detail, value -- for the search, the tests and the moves; the paint
;;; (PICKER-ROW-PAINT) is only the look.

(defstruct metro-line
  "One line of the /tree graph: its LANE (0 is the head's line), whether it is
the head's line (MAIN), the node id it left (FROM), its node ids in run order
(NODES), and the rows it spans, START to END -- an END a later line's fork
stretches down to the row that line starts on."
  (lane 0) (main nil) (from nil) (nodes '()) (start 0) (end -1))

(defun metro-recap-p (entry)
  "Whether listing prompt ENTRY is a reflection's recap: a note on a turn."
  (equal "reflection" (gethash "origin" entry)))

(defun metro-cron (prompt)
  "A cron fire's (JOB . HH:MM) when PROMPT opens the way a cron prompt does --
the job's name quoted after `[cron', then `fired', a date and the minute --
else NIL."
  (when (and (stringp prompt) (alexandria:starts-with-subseq "[cron \"" prompt))
    (let* ((close (position #\" prompt :start 7))
           (clock (and close (+ close 19))))
      (when (and close
                 (eql close (search "\" fired " prompt :start2 close))
                 (< (+ clock 4) (length prompt))
                 (char= #\: (char prompt (+ clock 2))))
        (cons (subseq prompt 7 close) (subseq prompt clock (+ clock 5)))))))

(defun metro-clock (ms)
  "The local HH:MM of epoch milliseconds MS; NIL for NIL."
  (when ms
    (multiple-value-bind (second minute hour)
        (decode-universal-time (+ nlk:+unix-epoch+ (floor ms 1000)))
      (declare (ignore second))
      (format nil "~2,'0d:~2,'0d" hour minute))))

(defun metro-span-ms (entry)
  "Listing ENTRY's span in milliseconds, its STARTED_AT to its ENDED_AT; NIL
while it has no end (an absent end is no zero)."
  (let ((start (json-value entry :string "started_at"))
        (end (json-value entry :string "ended_at")))
    (let ((from (and start (iso-8601-ms start)))
          (to (and end (iso-8601-ms end))))
      (and from to (max 0 (- to from))))))

(defun metro-ask-key (node)
  "The opening of NODE's ask a retry repeats: its first 22 characters."
  (let ((prompt (or (json-value (getf node :entry) :string "prompt") "")))
    (subseq prompt 0 (min 22 (length prompt)))))

(defun metro-nodes (entries children &aux (prompts (make-hash-table :test #'equal))
                                          (by-id (make-hash-table :test #'equal))
                                          (nodes '()))
  "The /tree graph's turns over the listing ENTRIES, in the order they ran: a
plist per prompt row that is no recap -- :ID, :ENTRY, :PARENT (the nearest
listed turn above it that is no recap; NIL at a root, or under a turn the
listing's limit cut), :CALLS (CHILDREN's tool rows under it), :RECAPS (the
recap rows folded under it), :HEAD-P (it, or a recap of it, is the head),
:ON-CHAIN, :START (epoch ms), :SPAN (ms; NIL while it runs), :ENDING,
:STEERS, :CRON (METRO-CRON) and :RETRY (a cancelled ask its parent was asked
again after). A recap whose turn the listing does not carry stays a node.
=> (values NODES BY-ID), BY-ID the node of each id."
  (dolist (entry entries)
    (when (checkpoint-turn-entry-p entry)
      (setf (gethash (gethash "turn_id" entry) prompts) entry)))
  (flet ((up (entry)
           ;; The nearest listed ancestor that is no recap.
           (do ((parent (gethash "parent_turn_id" entry)
                        (gethash "parent_turn_id" (gethash parent prompts))))
               ((not (and (stringp parent) (gethash parent prompts))) nil)
             (unless (metro-recap-p (gethash parent prompts))
               (return parent)))))
    (dolist (entry entries)
      (when (checkpoint-turn-entry-p entry)
        (let ((id (gethash "turn_id" entry))
              (parent (up entry))
              (start (json-value entry :string "started_at")))
          (unless (and parent (metro-recap-p entry))
            (push (setf (gethash id by-id)
                        (list :id id :entry entry :parent parent
                              :calls (gethash id children) :recaps '()
                              :head-p (eq t (gethash "is_head" entry))
                              :on-chain (eq t (gethash "on_head_chain" entry))
                              :start (and start (iso-8601-ms start))
                              :span (metro-span-ms entry)
                              :ending (json-value entry :string "ending")
                              :steers (or (json-value entry :integer "steers") 0)
                              :cron (metro-cron (gethash "prompt" entry))
                              :retry nil
                              :position (or (json-value entry :integer "log_position") 0)))
                  nodes)))))
    ;; Each recap onto its turn, in the listing's order: a recap at the head
    ;; puts the head on its turn, the one the operator reads it under.
    (dolist (entry entries)
      (when (and (checkpoint-turn-entry-p entry) (metro-recap-p entry))
        (let ((node (gethash (up entry) by-id)))
          (when node
            (setf (getf node :recaps) (append (getf node :recaps) (list entry)))
            (when (eq t (gethash "is_head" entry))
              (setf (getf node :head-p) t)))))))
  (let ((nodes (stable-sort (nreverse nodes) #'< :key (lambda (node) (getf node :position)))))
    ;; A cancelled ask whose parent was asked the same again later says so.
    (dolist (node nodes)
      (when (and (equal "cancelled" (getf node :ending)) (getf node :parent)
                 (null (getf node :cron)))
        (let ((key (metro-ask-key node)))
          (setf (getf node :retry)
                (and (find-if (lambda (other)
                                (and (not (eq other node))
                                     (equal (getf other :parent) (getf node :parent))
                                     (> (getf other :position) (getf node :position))
                                     (equal key (metro-ask-key other))))
                              nodes)
                     t)))))
    (values nodes by-id)))

(defun metro-layout (nodes main-p &aux (index (make-hash-table :test #'equal))
                                       (holder (make-hash-table :test #'equal))
                                       (main (make-metro-line :main t))
                                       (lines (list main)))
  "NODES (METRO-NODES) on rails, a plist: :LINES (METRO-LINEs, the head's line
first), :HOLDER (node id -> its line) and :LANES (how many). A node MAIN-P
(called with its id) puts on the head's line stands on lane 0; any other
continues the line of the turn it left while that turn ends its line, else
starts a line of its own, in the first lane right of the head's whose line
ended above the row it starts on."
  (loop for node in nodes
        for i from 0
        for id = (getf node :id)
        do (setf (gethash id index) i)
           (let ((line (if (funcall main-p id)
                           main
                           (let* ((parent (getf node :parent))
                                  (up (and parent (gethash parent holder))))
                             (if (and up (not (metro-line-main up))
                                      (equal parent (first (metro-line-nodes up))))
                                 up
                                 (first (push (make-metro-line :from parent) lines)))))))
             (push id (metro-line-nodes line))
             (setf (gethash id holder) line)))
  (setf lines (nreverse lines))
  (dolist (line lines)
    (let ((ids (setf (metro-line-nodes line) (reverse (metro-line-nodes line)))))
      (when ids
        (setf (metro-line-start line) (gethash (first ids) index)
              (metro-line-end line) (gethash (car (last ids)) index)))))
  ;; The rail a line left runs on down to the row the line starts on.
  (dolist (line (rest lines))
    (let ((up (and (metro-line-from line) (gethash (metro-line-from line) holder))))
      (when up
        (setf (metro-line-end up) (max (metro-line-end up) (metro-line-start line))))))
  (let ((ends (make-array 1 :adjustable t :fill-pointer 1
                            :initial-element (metro-line-end main))))
    (dolist (line (rest lines))
      (let ((lane (loop for k from 1
                        while (and (< k (fill-pointer ends))
                                   (>= (aref ends k) (metro-line-start line)))
                        finally (return k))))
        (setf (metro-line-lane line) lane)
        (if (< lane (fill-pointer ends))
            (setf (aref ends lane) (metro-line-end line))
            (vector-push-extend (metro-line-end line) ends))))
    (list :lines lines :holder holder :lanes (fill-pointer ends))))

(defun metro-glyph (node main head)
  "NODE's mark on its rail, (GLYPH . TONE): the head's (its id HEAD), a failed
turn's, a cancelled one's, else one on the head's line (MAIN) or off it."
  (cond ((equal (getf node :id) head) (cons "◉" :tool))
        ((equal (getf node :ending) "failed") (cons "✕" :diff-removed))
        ((equal (getf node :ending) "cancelled") (cons "◦" :muted))
        (main (cons "●" :dialog-accent))
        (t (cons "○" :success))))

(defun metro-cells (layout i node &key sub head)
  "The graph cells of row I, NODE's, two columns a lane, as (TEXT . TONE): a
rail down every line spanning the row, NODE's mark on its own, and on the row
a line starts the fork joining it to the rail it left. SUB draws a row hung
under row I instead: the rails running on below it. HEAD is the head's id."
  (let* ((holder (getf layout :holder))
         (own (gethash (getf node :id) holder))
         (cells (make-array (* 2 (getf layout :lanes)) :initial-element (cons " " nil))))
    (flet ((rail (line) (if (metro-line-main line) :list-mark :scrollbar-rail)))
      (dolist (line (getf layout :lines))
        (when (and (metro-line-nodes line)
                   (<= (metro-line-start line) i (metro-line-end line)))
          (let ((column (* 2 (metro-line-lane line))))
            (cond ((and (not sub) (eq line own))
                   (setf (aref cells column) (metro-glyph node (metro-line-main line) head)))
                  ((if sub (< i (metro-line-end line)) (> i (metro-line-start line)))
                   (setf (aref cells column) (cons "│" (rail line))))))))
      (let ((up (and (not sub) own (not (metro-line-main own))
                     (equal (getf node :id) (first (metro-line-nodes own)))
                     (metro-line-from own)
                     (gethash (metro-line-from own) holder))))
        (when up
          (let ((from (* 2 (metro-line-lane up)))
                (to (* 2 (metro-line-lane own)))
                (last (>= i (metro-line-end up))))
            (setf (aref cells from)
                  (cons (if (> from to) (if last "╯" "┤") (if last "╰" "├")) (rail up)))
            (loop for x from (1+ (min from to)) below (max from to)
                  do (setf (aref cells x) (cons "─" :scrollbar-rail)))))))
    (coerce cells 'list)))

(defun metro-cell-segs (cells)
  "CELLS (METRO-CELLS) as segments."
  (loop for (text . tone) in cells
        collect (make-seg :text text :tone (or tone :muted))))

(defun metro-label-text (node)
  "NODE's words on a row: its ask's excerpt, or a cron fire's job and minute."
  (let ((cron (getf node :cron)))
    (if cron
        (format nil "cron ~a · fired ~a" (car cron) (cdr cron))
        (checkpoint-row-excerpt (gethash "prompt" (getf node :entry))))))

(defun metro-row (left label suffix words columns room selected)
  "One painted /tree row ROOM wide: LEFT, then LABEL -- SUFFIX after it, the
selected row's files chrome -- in the room the right lane leaves, then the right
lane flush right: WORDS (cells of segments, two columns apart) ahead of the
fixed COLUMNS. The WORDS give way first, then the COLUMNS, before the label goes
under twelve columns. => (values SEGS GROUND)."
  (let ((plain (if selected :system :muted))
        (lead (segs-display-width left)))
    (flet ((right-width ()
             (+ (if words (+ (lane-width words) 2) 0) (segs-display-width columns))))
      (loop while (and words (< (- room lead 1 (right-width)) 12))
            do (setf words (butlast words)))
      (when (< (- room lead 1 (right-width)) 12)
        (setf columns '()))
      (let* ((right (append (when words
                              (append (lane-segs words) (list (make-seg :text "  " :tone plain))))
                            columns))
             (open (max 0 (- room lead (segs-display-width right) (if right 1 0))))
             (shown (clip-segs label (if suffix
                                         (max (min 12 open) (- open (text-display-width suffix)))
                                         open)
                               :mark *dialog-row-overflow-mark*))
             (tail (and suffix (clip-text-to-width suffix (max 0 (- open (segs-display-width shown))))))
             (body (append left shown
                           (when (plusp (length tail)) (list (make-seg :text tail :tone :muted))))))
        (values (append body
                        (list (make-seg :text (make-string (max (if right 1 0)
                                                                (- room (segs-display-width body)
                                                                   (segs-display-width right)))
                                                           :initial-element #\Space)
                                        :tone plain))
                        right)
                (and selected :panel-lift))))))

(defun metro-node-paint (row node cells &key clock repeat (arrow "  ") head)
  "The painter (PICKER-ROW-PAINT) of NODE's /tree row ROW: the edge, its CLOCK
time (blank under a row of the same minute, REPEAT, unless selected), its graph
CELLS, the HEAD tag on the head's row, the fold's ARROW, its ask -- a cron
fire's job and minute -- and, flush right, how it ended, its steers, its calls,
how long it ran, and a pen when a recap folds under it. The
selected row carries ROW's files chrome after the ask."
  (lambda (selected room)
    (let* ((plain (if selected :system :muted))
           (quiet :sheet-undone)
           (ending (getf node :ending))
           (span (getf node :span))
           (calls (length (getf node :calls)))
           (steers (getf node :steers))
           (cron (getf node :cron))
           (left (append
                  (list (if selected
                            (make-seg :text "▌" :tone :dialog-accent)
                            (make-seg :text " " :tone plain))
                        (make-seg :text " " :tone plain)
                        (make-seg :text (if (and clock (or selected (not repeat))) clock "     ")
                                  :tone (if selected :system quiet))
                        (make-seg :text "  " :tone plain))
                  (metro-cell-segs cells)
                  (list (make-seg :text " " :tone plain))
                  (when head
                    ;; A chip stands on its own ground, so its tone is the
                    ;; resolved style (a keyword tone takes the row's).
                    (list (make-seg :text " HEAD " :tone (style-for-tone :sheet-chip-on) :bold t)
                          (make-seg :text " " :tone plain)))
                  (list (make-seg :text arrow :tone quiet))))
           (label (if cron
                      (list (make-seg :text " cron " :tone (style-for-tone :sheet-chip-set))
                            (make-seg :text (format nil " ~a" (car cron)) :tone :system)
                            (make-seg :text (format nil " · fired ~a" (cdr cron)) :tone quiet))
                      (list (make-seg :text (metro-label-text node)
                                      :tone (cond (head :title)
                                                  ((equal ending "cancelled") quiet)
                                                  ((getf node :on-chain) :foreground)
                                                  (t :muted))
                                      :bold head))))
           (suffix (let ((text (picker-row-suffix row)))
                     (and selected (plusp (length text)) text)))
           (words (remove nil
                          (list (cond ((getf node :retry)
                                       (list (make-seg :text "re-asked ↓" :tone quiet :italic t)))
                                      ((equal ending "failed")
                                       (list (make-seg :text "failed" :tone :diff-removed)))
                                      ((equal ending "cancelled")
                                       (list (make-seg :text "cancelled" :tone quiet)))
                                      ((and (getf node :start) (null ending))
                                       (list (make-seg :text "open" :tone quiet))))
                                (and (plusp steers)
                                     (list (make-seg :text (format nil "+~d steer~:p" steers)
                                                     :tone :syntax-command))))))
           (columns (list (make-seg :text (format nil "~8@a" (if (plusp calls) (format nil "~d call~:p" calls) ""))
                                    :tone quiet)
                          (make-seg :text (format nil "  ~7@a" (if span (format-turn-elapsed span) ""))
                                    :tone plain)
                          (make-seg :text (if (getf node :recaps) "  ✎" "   ") :tone quiet)
                          (make-seg :text " " :tone plain))))
      (metro-row left label suffix words columns room selected))))

(defun metro-call-paint (entry cells)
  "The painter (PICKER-ROW-PAINT) of a call's /tree row, listing ENTRY, hung
under its turn: the rails running on (CELLS), the tool's name and what it was
asked to do, and flush right how long the call ran."
  (lambda (selected room)
    (let* ((plain (if selected :system :muted))
           (span (metro-span-ms entry))
           (left (append (list (if selected
                                   (make-seg :text "▌" :tone :dialog-accent)
                                   (make-seg :text " " :tone plain))
                               (make-seg :text "        " :tone plain))
                         (metro-cell-segs cells)
                         (list (make-seg :text " " :tone plain)
                               (make-seg :text "    ┆ " :tone :scrollbar-rail))))
           (label (list (make-seg :text (format nil "~a  " (or (json-value entry :text "tool_name") "tool"))
                                  :tone :tool)
                        (make-seg :text (or (json-value entry :text "tool_command") "")
                                  :tone (if selected :foreground :muted))))
           (columns (list (make-seg :text (format nil "~7@a" (if span (format-turn-elapsed span) ""))
                                    :tone :sheet-undone)
                          (make-seg :text "    " :tone plain))))
      (metro-row left label nil nil columns room selected))))

(defun metro-recap-row (entry cells)
  "The /tree row of recap ENTRY folded under its turn: a heading -- the arrows
pass over it -- of the rails running on (CELLS), a pen, and what the recap
said, or when it ran while the listing carries no words of it."
  (let* ((said (or (json-value entry :text "answer")
                   (metro-clock (let ((start (json-value entry :string "started_at")))
                                  (and start (iso-8601-ms start))))
                   ""))
         (row (make-picker-row :label (format nil "recap ~a" said) :heading t)))
    (setf (picker-row-paint row)
          (lambda (selected room)
            (declare (ignore selected))
            (values (clip-segs (append (list (make-seg :text "         " :tone :muted))
                                       (metro-cell-segs cells)
                                       (list (make-seg :text " " :tone :muted)
                                             (make-seg :text "    ✎ recap  " :tone :sheet-undone)
                                             (make-seg :text said :tone :muted :italic t)))
                               room :mark *dialog-row-overflow-mark*)
                    nil)))
    row))

(defun metro-chain (by-id id &aux (chain (make-hash-table :test #'equal)))
  "ID and every turn above it by BY-ID's parents: the line a head on ID stands
on, as a set."
  (loop for node = (gethash id by-id) then (gethash (getf node :parent) by-id)
        while node
        do (setf (gethash (getf node :id) chain) t))
  chain)

(defun tree-move-name (node)
  "How a move names NODE's turn: `turn 4', `the 14:31 cron fire', `the wake'."
  (let ((entry (getf node :entry))
        (cron (getf node :cron)))
    (cond (cron (format nil "the ~a cron fire" (cdr cron)))
          ((gethash "turn_number" entry) (format nil "turn ~a" (gethash "turn_number" entry)))
          ((equal "wake" (gethash "origin" entry)) "the wake")
          (t "this turn"))))

(defun tree-move-sentence (node option value leaving joining)
  "OPTION's move on NODE, the turn of picked row VALUE, in a sentence: where
the head goes, the turns it takes off the head's line (LEAVING) and puts on it
(JOINING), and the files."
  (let ((name (tree-move-name node))
        (count (length leaving)))
    (if (eq (getf option :move) :continue)
        (format nil "Continue from ~a: the head moves here~@[; ~a~]~@[; ~a~]."
                name
                (when leaving
                  (let ((calls (reduce #'+ leaving :key (lambda (turn) (length (getf turn :calls)))))
                        (ms (reduce #'+ leaving :key (lambda (turn) (or (getf turn :span) 0)))))
                    (format nil "~d turn~:p (~d call~:p~@[, ~a~]) stay~:[s~;~] as a branch"
                            count calls (and (plusp ms) (format-turn-elapsed ms)) (/= count 1))))
                (when (> (length joining) 1)
                  (format nil "~d turns join the head line" (length joining))))
        (format nil "Rewind to before ~a: its ask returns to the composer; ~d turn~:p leave~:[s~;~] the head line (/redo returns); ~a."
                name count (/= count 1)
                (let ((files (getf value :file-change-count)))
                  (cond ((not (getf option :restore-files)) "files stay as they are")
                        ((integerp files) (format nil "~d file~:p restored" files))
                        (t "its files restored")))))))

(defun tree-move-rows (state value option room)
  "The confirm stage's picture of OPTION, a move on the turn whose row VALUE
was picked: up to five rows of STATE's /tree graph around that turn, relaid
with the head's line where the move leaves it -- the turn tagged `here'
(continue) or `before this' (rewind), each row the move takes off the head's
line `leaves head', each it puts on `joins head', the head it lands on `HEAD'
-- then a blank and the move in a sentence, two rows, so every option draws as
many rows. => painted rows, ROOM wide; NIL for a turn the graph does not hold."
  (let* ((nodes (getf state :nodes))
         (by-id (getf state :by-id))
         (node (gethash (getf value :turn-id) by-id)))
    (when node
      (let* ((continue (eq (getf option :move) :continue))
             (head (if continue (getf node :id) (getf node :parent)))
             (chain (metro-chain by-id head))
             (layout (metro-layout nodes (lambda (id) (gethash id chain))))
             (leaving (remove-if-not (lambda (turn) (and (getf turn :on-chain)
                                                         (not (gethash (getf turn :id) chain))))
                                     nodes))
             (joining (remove-if (lambda (turn) (or (getf turn :on-chain)
                                                    (not (gethash (getf turn :id) chain))))
                                 nodes))
             (at (position node nodes))
             ;; Five rows, the sentence's two and a blank: the panel's cap
             ;; (*DIALOG-FRAME-MAX-HEIGHT*) keeps every option in view under them.
             (from (max 0 (min (- at 2) (- (length nodes) 5))))
             (sentence (wrapped-lines (tree-move-sentence node option value leaving joining)
                                      (max 20 (1- room)))))
        (append
         (loop for i from from below (min (length nodes) (+ from 5))
               for turn = (nth i nodes)
               for id = (getf turn :id)
               for on = (gethash id chain)
               for leaves = (and (getf turn :on-chain) (not on))
               for tag = (cond ((eq turn node) (cons (if continue " here " " before this ") :sheet-chip-on))
                               (leaves (cons " leaves head " :sheet-chip))
                               ((and on (not (getf turn :on-chain))) (cons " joins head " :diff-added-emphasis))
                               ((equal id head) (cons " HEAD " :sheet-chip-on)))
               collect (clip-segs
                        (append (list (make-seg :text (format nil " ~5a  " (or (metro-clock (getf turn :start)) ""))
                                                :tone :sheet-undone))
                                (metro-cell-segs (metro-cells layout i turn :head head))
                                (list (make-seg :text " " :tone :muted))
                                (when tag
                                  (list (make-seg :text (car tag) :tone (style-for-tone (cdr tag)) :bold t)
                                        (make-seg :text " " :tone :muted)))
                                (list (make-seg :text (metro-label-text turn)
                                                :tone (cond ((equal id head) :title)
                                                            ((or leaves (equal (getf turn :ending) "cancelled")) :sheet-undone)
                                                            (on :foreground)
                                                            (t :muted)))))
                        room :mark *dialog-row-overflow-mark*))
         (list '())
         (loop for k below 2
               for line = (nth k sentence)
               collect (if line
                           (clip-segs (list (make-seg :text (format nil " ~a" line) :tone :tool))
                                      room :mark *dialog-row-overflow-mark*)
                           '())))))))

(defun tree-fold-context (state)
  "The /tree tab's context: how many turns, on how many lines, from the first
one's clock time to the last one's end."
  (let* ((nodes (getf state :nodes))
         (lines (count-if #'metro-line-nodes (getf (getf state :layout) :lines)))
         (starts (loop for node in nodes when (getf node :start) collect (getf node :start)))
         (ends (loop for node in nodes
                     when (getf node :start)
                       collect (+ (getf node :start) (or (getf node :span) 0)))))
    (format nil " ~d turn~:p · ~d line~:p~@[ · ~a~]"
            (length nodes) lines
            (and starts (format nil "~a → ~a"
                                (metro-clock (reduce #'min starts))
                                (metro-clock (reduce #'max ends)))))))

;;; --- the /tree turn fold --------------------------------------------------
;;; The listing is one row per checkpoint, and a session of 20 turns and 270
;;; tool calls is 290 of them: the turns drown in their own tool traffic. The
;;; dialog folds it the way /sessions folds forks — a turn is one row, its
;;; tool rows are the branch under it, drawn only while the turn is open —
;;; and every turn starts closed, so /tree opens on the turns themselves and
;;; one Right opens the turn the operator is looking at.
;;;
;;; The fold rides the dialog in a side table rather than a slot: a running
;;; image cannot grow a structure a slot, and the fold must reach a shell that
;;; is already up.

(nlk:define-side-table tree-fold (:weakness :key)
  "The open /tree dialog -> its fold, a plist: :ENTRIES the listing in its own
order, :TURNS the ids the listing names as turns, :CHILDREN the tool rows
filed under each turn id, :OPEN the turn ids whose tool rows are drawn — the
one fact the Left/Right gesture moves. Esc drops the dialog and the fold with
it, so a reopened picker starts with every turn closed.")

(defun checkpoint-turn-entry-p (entry)
  "Whether listing ENTRY is a turn's own row — its prompt, the row the fold
keeps while its tool rows hang under it."
  (equal "user_message" (gethash "kind" entry)))

(defun tree-fold-index (entries &aux (turns (make-hash-table :test #'equal))
                                     (children (make-hash-table :test #'equal)))
  "The /tree dialog's fold over the listing ENTRIES: the turns it names and,
under each, the tool rows that followed it — filed by their own TURN_ID,
against the turns seen so far, so a row can never hang under a turn the
listing did not list. Every turn starts closed."
  (dolist (entry entries)
    (let ((turn-id (gethash "turn_id" entry)))
      (when (stringp turn-id)
        (if (checkpoint-turn-entry-p entry)
            (setf (gethash turn-id turns) t)
            (when (gethash turn-id turns)
              (alexandria:appendf (gethash turn-id children) (list entry)))))))
  ;; The graph the rows paint: the turns in the order they ran (METRO-NODES)
  ;; on the rails of the head's line as the listing stands (METRO-LAYOUT).
  (multiple-value-bind (nodes by-id) (metro-nodes entries children)
    (list :entries entries :turns turns :children children
          :open (make-hash-table :test #'equal)
          :nodes nodes :by-id by-id
          :layout (metro-layout nodes (lambda (id) (getf (gethash id by-id) :on-chain))))))

(defun tree-fold-rows (state &aux (turns (getf state :turns))
                                  (open (getf state :open))
                                  (layout (getf state :layout))
                                  (nodes (getf state :nodes))
                                  (head (getf (find-if (lambda (node) (getf node :head-p)) nodes) :id))
                                  (last-clock nil)
                                  (rows '()))
  "STATE's fold laid out on its graph (the /tree metro): a turn's own row in
the order the turns ran, and while the turn is open its recaps and then its
tool rows under it, the rails running on beside them. A tool row whose turn the
listing never named stays a row of its own, ahead of the graph, so nothing the
listing carried is lost."
  (dolist (entry (getf state :entries))
    (let ((turn-id (gethash "turn_id" entry)))
      (unless (or (checkpoint-turn-entry-p entry)
                  (and (stringp turn-id) (gethash turn-id turns)))
        (push (checkpoint-picker-row entry :child t) rows))))
  (loop for node in nodes
        for i from 0
        for id = (getf node :id)
        for kids = (getf node :calls)
        for opened = (and (gethash id open) t)
        for row = (checkpoint-picker-row (getf node :entry)
                                         :children (and kids (length kids))
                                         :open opened)
        for clock = (metro-clock (getf node :start))
        do ;; A recap at the head puts the head on its turn: the turn's row
           ;; offers what the head's does.
           (when (getf node :head-p)
             (setf (getf (picker-row-value row) :head-p) t))
           (setf (picker-row-paint row)
                 (metro-node-paint row node (metro-cells layout i node :head head)
                                   :clock clock :repeat (equal clock last-clock)
                                   :head (equal id head)
                                   :arrow (cond ((not (or kids (getf node :recaps))) "  ")
                                                (opened "▾ ")
                                                (t "▸ "))))
           (setf last-clock clock)
           (push row rows)
           (when opened
             (let ((under (metro-cells layout i node :sub t)))
               (dolist (recap (getf node :recaps))
                 (push (metro-recap-row recap under) rows))
               (dolist (kid kids)
                 (let ((kid-row (checkpoint-picker-row kid :child t)))
                   (setf (picker-row-paint kid-row) (metro-call-paint kid under))
                   (push kid-row rows))))))
  (nreverse rows))

(defun tree-fold-turn-value (state turn-id)
  "The row value TURN-ID's own row carries in STATE's fold as it now stands —
the seat a gesture leaves the cursor on."
  (loop for row in (tree-fold-rows state)
        for value = (picker-row-value row)
        when (and (equal turn-id (getf value :turn-id))
                  (equal "user_message" (getf value :kind)))
          return value))

(defun fold-arrow (model direction id open children-p parent relay)
  "One Left/Right over a picker fold whose OPEN table holds the ids drawn
open: Right opens ID when it has children (CHILDREN-P) and is closed; Left
closes ID when it is open, else PARENT (NIL at a root) with the cursor parked
there. RELAY lays the dialog's rows out again, the cursor back on the id it is
handed, and MODEL repaints. => T when the fold moved; a gesture with nothing
to fold declines, leaving the key the no-op it was."
  (when-let (target (ecase direction
                      (:right (and children-p (not (gethash id open)) id))
                      (:left (if (gethash id open) id parent))))
    (if (eq direction :right) (setf (gethash target open) t) (remhash target open))
    (funcall relay target)
    (when model.band
      (render-state model))
    t))

(defun tree-fold-gesture (model dialog direction &aux (state (tree-fold dialog)))
  "One Left/Right on the open /tree picker — the fold the highlight drives.
Right opens the highlighted turn's tool rows under it; Left closes the
highlighted turn, or — on a tool row — the turn it hangs under, parking the
cursor on that turn. A gesture with nothing to fold declines, leaving the key
the no-op it was, and so does a box holding a query: the rows on screen are
the search's, not the fold's. => T when the fold moved."
  (when (and state
             (zerop (length (string-trim " " dialog.search))))
    (let* ((value (picker-highlighted-value dialog))
           (turn-id (and value (getf value :turn-id))))
      (when (and (stringp turn-id) (gethash turn-id (getf state :turns)))
        ;; A turn row whose tool rows are drawn closes them. A tool row's turn
        ;; is open (the row is on screen), so the climb from it closes that
        ;; turn -- its parent here -- and parks on it.
        (fold-arrow model direction turn-id (getf state :open)
                    ;; A turn opens over its calls and the recaps folded under it.
                    (let ((node (gethash turn-id (getf state :by-id))))
                      (and node (or (getf node :calls) (getf node :recaps)) t))
                    (unless (equal "user_message" (getf value :kind)) turn-id)
                    (lambda (seat-turn)
                      ;; The rows under the dialog, laid out again with the fold
                      ;; as it now stands, the cursor back on SEAT-TURN's row.
                      (setf dialog.rows (tree-fold-rows state))
                      (picker-reseat-selection dialog
                                               (tree-fold-turn-value state seat-turn))))))))

(defun checkpoint-picker-row (entry &key child children open)
  "One /tree picker row from a checkpoint listing entry."
  ;; A turn row leads with the fold's own column — `▸ ' while the tool rows
  ;; under it are folded away, `▾ ' while they are drawn, blank when the turn
  ;; holds none — then the chain glyph and the prompt; the detail beside it
  ;; says how many calls the fold is holding back. A tool row reads by what
  ;; the tool was asked to do — eval's form, a shell's command — and by the
  ;; tool's name only when the listing carried no command for it, hanging one
  ;; indent deeper while it is a turn's child (CHILD). The label opens with the
  ;; tree glyphs of the lanes the listing put it in (TREE-LANE-PREFIX). These
  ;; slots are what the search reads and the moves carry; the /tree dialog
  ;; paints the row as the metro (TREE-FOLD-ROWS). The count suffix sizes the
  ;; rewind to before the prompt, one of the moves every prompt row offers
  ;; (CHECKPOINT-OPTION-ROWS).
  (with-json ((kind (gethash "kind" entry))
              (user-p (equal kind "user_message"))
              (head-p :boolean "is_head")
              (on-chain :boolean "on_head_chain")
              (fork :boolean "fork")
              (lanes (coerce (json-array entry "lanes") 'list))
              (indent (tree-lane-prefix fork lanes))
              (prompt (checkpoint-row-excerpt (gethash "prompt" entry)))
              (command (checkpoint-row-excerpt (gethash "tool_command" entry)))
              (caret (if (and (not child) children) (if open "▾ " "▸ ") ""))
              (origin :text "origin")
              (label (if user-p
                         (format nil "~a~a~a ~a"
                                 indent caret
                                 (cond (head-p "●") (on-chain "○") (t "◇"))
                                 prompt)
                         (format nil "~a~a· ~a"
                                 indent
                                 (if child "    " "  ")
                                 (or (json-value command :text)
                                     (gethash "tool_name" entry)
                                     "tool"))))
              ;; The descriptor the picked row hands the confirm stage (`turn
              ;; 3 · branch') stays clean: the calls count is the row's own
              ;; display, never part of what a picked row IS.
              ;; A turn no one asked -- a reflection's recap, an exit's wake --
              ;; carries no number: it says what it is instead.
              (descriptor (if user-p
                              (format nil "~a~a~a"
                                      (dispatch origin equal ("reflection" "recap") ("wake" "woke")
                                        (t (format nil "turn ~a" (or (gethash "turn_number" entry) "?"))))
                                      (if on-chain "" " · branch")
                                      (if head-p " · head" ""))
                              (if on-chain "" "branch")))
              (detail (if (and user-p children (not open))
                          (format nil "~a · ~d call~:p" descriptor children)
                          descriptor))
              (checkpoint-id :text "checkpoint_id")
              (files :integer "file_change_count" :default 0))
      entry
    (make-picker-row
     :label label :detail detail
     ;; The bare count renders immediately; the path tail lands via the
     ;; highlight fetch.
     :suffix (if (plusp files) (checkpoint-files-suffix files) "")
     :value (list :kind kind
                  :turn-id (gethash "turn_id" entry)
                  :head-p head-p
                  :on-chain on-chain
                  ;; The confirm stage's tab context: `turn 3 · branch'.
                  :detail descriptor
                  :checkpoint-id checkpoint-id
                  ;; The prompt excerpt doubles as the restore dialog's
                  ;; third-row summary (the Zig item.summary hand-off).
                  :summary (if user-p prompt "")
                  :file-change-count files))))

(defun slash-tree-op (model)
  "Open the checkpoint tree picker."
  ;; Returns notice text. The mouse click-to-restore gesture (ui.lisp) calls
  ;; it too.
  (with-slash-notice ("tree")
      (let* ((response (session-control-call model :get "checkpoints"))
             ;; The fold lays the listing out as the metro: one row per turn
             ;; in the order the turns ran, on the rails of their lines, the
             ;; recaps and tool rows of a turn under it while it is open --
             ;; and the selection opens on the head wherever its line sits.
             (state (tree-fold-index
                     (coerce (gethash "checkpoints" response) 'list)))
             (rows (tree-fold-rows state)))
        (open-picker-dialog
         model "Session tree" rows
         (lambda (model value)
           (flet ((navigate (boundary &optional (restore-files t))
                    (send-navigate-checkpoint
                     model.ws
                     model.session-id
                     :target-turn-id (getf value :turn-id)
                     :boundary boundary
                     :restore-files restore-files)))
             (cond
               ((null (getf value :turn-id)) "checkpoint carries no turn")
               ((equal (getf value :kind) "user_message")
                ;; A prompt row is a turn, and the confirm stage offers its two
                ;; moves: continue from it — the turn boundary lands the head
                ;; on the turn, answer kept, conversation only, so the next
                ;; prompt branches from there (the operator reads
                ;; a row as being AT that turn) — or rewind to before its
                ;; message, the head on its parent and the prompt back in the
                ;; composer, with or without the code changes. The same menu
                ;; on the head chain and off it: a branch row's continue is
                ;; the return to that branch, and on the head row only the
                ;; rewind remains, which is /undo. Nothing fires until the
                ;; operator picks.
                (open-checkpoint-dialog
                 model value
                 (lambda (move restore-files)
                   (ecase move
                     (:continue (navigate "turn" nil))
                     (:rewind (navigate "user_message" restore-files))))
                 ;; The move drawn on the graph the tree opened on.
                 :preview (lambda (option)
                            (tree-move-rows state value option
                                            (docked-panel-room model.term-width)))))
               (t
                ;; Tool rows keep direct Enter-navigate — the head on the
                ;; turn, the workspace at that call — the Zig /tree dialog
                ;; had no confirm stage for them either.
                (navigate "turn")
                nil))))
         :searchable t
         :on-arrow #'tree-fold-gesture
         :selection (or (position-if (lambda (row) (getf (picker-row-value row) :head-p)) rows) 0)
         ;; The graph's measure: ` 18 turns · 2 lines · 13:22 → 14:31'.
         :context (tree-fold-context state)
         :search-placeholder "Search checkpoints"
         :empty-label "No checkpoints available"
         :no-match-label "No checkpoints match this filter"
         :footer-hints :session-tree
         ;; The highlight fetch is a whole staged-diff read on the gateway —
         ;; 25-60 ms measured, and a good deal more on a session whose
         ;; workspace is huge — so it is STAGED, never paid between the
         ;; keystroke and the rows: the picker paints from the listing it
         ;; just read, and the selected row's files chrome lands on the
         ;; frame clock (OPEN-PICKER-DIALOG :HIGHLIGHT-DELIVERY, the
         ;; /sessions picker's own shape).
         :highlight-delivery :staged
         :on-highlight
         (lambda (model row)
           (checkpoint-row-ensure-files
            row
            (lambda (checkpoint-id)
              (with-handlers ((error () (values nil nil)))
                (let ((object (session-control-call
                               model :get
                               (format nil "checkpoints/~a/files" checkpoint-id))))
                  (values (coerce (gethash "files" object) 'list) t)))))))
        ;; The fold the dialog's Left/Right gesture moves, filed beside the
        ;; dialog — Esc drops it, so a reopened picker starts folded.
        (setf (tree-fold model.dialog) state)
        nil)))
(defun relative-age-label (updated-at now-ms)
  "The Zig relativeAgeLabel copy: \"just now\", \"Nm ago\", \"Nh ago\",
\"Nd ago\"; a stamp that does not parse passes through verbatim."
  ;; The shell's ISO-8601-MS is this image's one reader for the stamp shape
  ;; the wire writes, and it declines the same garbage the Zig days-from-civil
  ;; parser declined.
  (let* ((updated (iso-8601-ms updated-at))
         (diff (and updated (floor (- now-ms updated) 1000))))
    (cond ((null updated) updated-at)
          ((< diff 60) "just now")
          (t (format nil "~a ago" (nlk:duration-text diff :coarse t))))))

;;; --- the running sessions ----------------------------------------------------
;;; Which sessions have a turn in flight right now, and since when: the
;;; gateway's organism.activity frames (activity.lisp) -- the whole map each
;;; time it changes, and once more as this shell subscribes -- folded into a
;;; table of this shell's own. The directory /sessions lists is a durable
;;; read of what the store holds; this is the one live column painted over
;;; it, so a session nobody here started -- a room's, a cron job's, another
;;; shell's -- reads as busy in the same place, under the spinner its turn
;;; is running. Live-only: never replayed, and nothing dials for it.

;;; An open picker's fold holds the table itself, so rebuilding rows after a
;;; frame reads the map as it now stands; the keys are weak.
(defvar *session-activity* (make-hash-table :test #'eq :weakness :key)
  "Weak memo: a shell's model -> its running-session table, session id ->
the ISO stamp the running turn's own durable fact carries.")

(defun session-activity-table (model)
  "MODEL's running-session table — every session with a turn in flight, and
when it started — made empty on first read: a shell that heard from nobody
has no session running."
  (alexandria:ensure-gethash model *session-activity* (make-hash-table :test #'equal)))

(defun ingest-session-activity (model payload &aux (table (session-activity-table model))
                                                   (next (make-hash-table :test #'equal)))
  "Fold one organism.activity frame into MODEL's table."
  ;; Every frame carries the whole map, so the table is rebuilt rather than
  ;; patched: a session whose turn settled leaves it here, and a row naming a
  ;; session twice keeps the last word. => (values TABLE MOVED-P): MOVED-P
  ;; says the map differs from the one the table held — the snapshot every
  ;; subscribe answers (the preview lane re-subscribes per highlight) mostly
  ;; carries the map the shell already holds, and re-laying a large directory
  ;; for it would cost time and a collection.
  (loop for row across (json-array payload "sessions")
        for id = (and (hash-table-p row) (gethash "id" row))
        for started = (and (hash-table-p row) (gethash "started_at" row))
        when (and (stringp id) (stringp started))
          do (setf (gethash id next) started))
  (let ((moved (not (equalp next table))))
    (when moved
      (clrhash table)
      (maphash (lambda (id started) (setf (gethash id table) started)) next))
    (values table moved)))

(defun refresh-sessions-dialog (model &key tick &aux (dialog model.dialog))
  "Draw an open /sessions picker again over the running-session table: the
directory rows re-laid from the fold, the running ones wearing the live
spinner and their clock, the age column re-read at the paint, and the
highlight reseated on the session it held."
  ;; On a TICK the paint lands only
  ;; when the spinner has moved to its next frame — the frame clock's own quantum
  ;; (STATUS-MARK) — so a dialog over a directory of settled sessions costs the
  ;; clock nothing, and the elapsed second rides the same paint.
  ;;
  ;; A box holding a query shows the gateway's conversation hits instead of
  ;; the directory: those rows are not a listing and carry no live column, so
  ;; this declines and leaves them as they are. => T when the rows were drawn
  ;; again.
  (when (and (picker-dialog-p dialog) (eq dialog.kind :sessions))
    (let ((state (gethash dialog *session-search*)))
      ;; A dialog whose directory read is still out holds no fold to lay
      ;; again: its rows are the read's to produce.
      (when (and state (getf state :fold) (string= "" (string-trim " " dialog.search)))
        (let* ((activity (session-activity-table model))
               (now-ms (iso-8601-ms (nlk::iso-now)))
               (frame (and (plusp (hash-table-count activity))
                           (floor now-ms *status-running-frame-ms*))))
          (unless (and tick (equal frame (getf state :activity-frame)))
            (let ((seat (picker-highlighted-value dialog)))
              (setf (getf state :activity-frame) frame
                    ;; The age column is a clock too: re-read it as the rows
                    ;; are laid again, or a dialog left open says just now
                    ;; hours later.
                    (sessions-fold-now-ms (getf state :fold)) now-ms
                    dialog.rows
                    (sessions-fold-rows (getf state :fold) activity)
                    ;; The running count's spinner turns with the rows'.
                    dialog.context
                    (sessions-dialog-context (getf state :fold) activity))
              (picker-reseat-selection dialog seat)
              ;; A tick's own frame repaints the chrome it asked for; the
              ;; rest of the time nothing else would.
              (when (and (not tick) model.band)
                (render-state model))
              t)))))))

(defun running-elapsed-ms (started now-ms &aux (started-ms (iso-8601-ms started)))
  "Milliseconds STARTED — the ISO stamp a running turn's own durable fact
carries — lies before NOW-MS, never negative: a start in the future (clock
skew across hosts) clamps to zero rather than running a clock backwards."
  ;; => the number, or NIL when STARTED does not parse.
  (and started-ms (max 0 (- now-ms started-ms))))

(defun running-clock-label (elapsed-ms &aux (seconds (floor elapsed-ms 1000)))
  "The running clock's text in the one dialect this shell's dialogs speak:
`42s', `3m 12s', `1h 04m'."
  ;; The /sessions directory's running row reads it, so a turn in flight is
  ;; clocked in this shell's words.
  (if (< seconds 60) (format nil "~ds" seconds) (format-turn-elapsed elapsed-ms)))

(defun session-picker-entry (entry)
  "One wire session row as the plist SESSION-TREE-ORDER walks and folds: the
three fields the walk reads, with the wire object itself kept under :ENTRY
for the row to read its title, directory, age and count from."
  (list :id (gethash "id" entry)
        :parent (json-value entry :string "parent_id")
        :updated-at (json-value entry :string "updated_at")
        :entry entry))

(defun session-picker-index (entries &aux (by-id (make-hash-table :test #'equal))
                                          (children (make-hash-table :test #'equal)))
  "The sessions dialog's fold indexes over the wire ENTRIES: session id ->
its entry, and session id -> the ids listing it as parent, as the listing
scanned them."
  ;; => (values BY-ID CHILDREN). A PARENT_ID no entry answers —
  ;; or a row parenting itself — indexes nothing; SESSION-TREE-ORDER reads such
  ;; a row as a root too.
  (dolist (entry entries)
    (setf (gethash (gethash "id" entry) by-id) entry))
  (dolist (entry entries)
    (when-let (parent (sessions-fold-parent by-id (gethash "id" entry)))
      (push (gethash "id" entry) (gethash parent children))))
  (values by-id children))

;;; --- a session and its nodes --------------------------------------------------
;;;
;;; One row per session, and the sessions that came off it hang under it
;;; exactly as the listing's own tree (SESSION-TREE-ORDER) lays them: its
;;; forks, its frames, and — while the team cell runs — the seats it
;;; started. The wire names the edge each node hangs by (RELATION):
;;; `inherited' for a fork that composes the session's history, `detached'
;;; for a node that began fresh. Nothing sits above a session: a session is
;;; the unit a person works in, and what came off it is its own branch.

(defun make-sessions-picker-fold (entries &optional (now-ms (iso-8601-ms (nlk::iso-now))))
  "The dialog's fold over a directory read: the listing, the index over its
PARENT_ID edges, and every session folded — the door state, one Right from
the nodes it holds. A reflection (source `experience', NLK::+NOTE-SOURCE+)
is the organism's own note on a turn, not a session anyone opens: it is
left out, as the web page leaves it, and counts as no fork."
  (setf entries (remove nlk::+note-source+ entries
                        :test #'equal :key (lambda (entry) (json-value entry :string "source"))))
  (multiple-value-bind (by-id children) (session-picker-index entries)
    (make-sessions-fold :entries entries :by-id by-id
                        :children children :now-ms now-ms)))

(defun session-picker-descendant-starts (children activity)
  "The running turns projected up their fork chains: id -> the ISO start of
the newest turn running anywhere UNDER it, for every session in ENTRIES
that is not running itself but has a running descendant — a fork or a spawn
at any depth."
  ;; A collapsed parent hides its branch whole, so this is how `▸ parent' says
  ;; that something inside it is live without the operator opening it
  ;; (SESSION-PICKER-ROW's dimmed mark). The walk reads the parent links off
  ;; CHILDREN, the listing's own PARENT_ID edges (SESSION-PICKER-INDEX), so a
  ;; listing a LIMIT cut short stops where its rows stop instead of inventing
  ;; ancestry, and a cycle ends at the first repeat. NIL or empty ACTIVITY is
  ;; no ancestors. => the table.
  (let ((table (make-hash-table :test #'equal)))
    (when (and activity (plusp (hash-table-count activity)))
      (let ((parent (make-hash-table :test #'equal)))
        (maphash (lambda (up kids) (dolist (kid kids) (setf (gethash kid parent) up))) children)
        (loop for id being the hash-keys of activity using (hash-value started)
              do (let ((seen (make-hash-table :test #'equal)))
                   (loop for up = (gethash id parent) then (gethash up parent)
                         while (and (stringp up) (not (gethash up seen)))
                         do (setf (gethash up seen) t)
                            (let ((held (gethash up table)))
                              (when (or (null held) (string< held started))
                                (setf (gethash up table) started))))))
        ;; A session running its own turn wears the live mark, never the dim
        ;; one: its own start is the clock its row paints.
        (loop for id being the hash-keys of activity do (remhash id table))))
    table))

(defun session-local-day (stamp &aux (ms (if (stringp stamp) (iso-8601-ms stamp) stamp)))
  "The local date STAMP -- an ISO instant, or epoch milliseconds -- falls on
(LOCAL-DATE's key), or NIL when it does not parse."
  (and ms (local-date (+ nlk:+unix-epoch+ (floor ms 1000)))))

(defun session-day-heading (day now-ms)
  "The heading over DAY's sessions: `Today', `Yesterday', else `Mon 13 Oct'.
It names the day and is never chosen; a rule runs from it to the edge."
  (let* ((now (+ nlk:+unix-epoch+ (floor now-ms 1000)))
         (label (cond ((equal day (local-date now)) "Today")
                      ((equal day (local-date (- now 86400))) "Yesterday")
                      (t (destructuring-bind (year month date) day
                           (nth-value 1 (local-date (encode-universal-time 0 0 12 date month year)))))))
         (row (make-picker-row :label label :heading t)))
    (setf (picker-row-paint row)
          (lambda (selected room)
            (declare (ignore selected))
            (list (make-seg :text " " :tone :muted)
                  (make-seg :text label :tone :muted :bold t)
                  (make-seg :text " " :tone :muted)
                  (make-seg :text (make-string (max 0 (- room (text-display-width label) 2))
                                               :initial-element #\─)
                            :tone :scrollbar-rail))))
    row))

(defun session-picker-rows (entries children open now-ms &optional activity)
  "The sessions dialog's rows: the listing's own fork tree
(SESSION-TREE-ORDER), laid out again from the wire ENTRIES with the dialog's
fold — a session's forks are drawn only while its id is in OPEN, so a
collapsed session hides its branch whole and every lane describes the rows
actually on screen — under a heading for each day the sessions moved on."
  ;; CHILDREN (id -> the ids under it) supplies the forks a row counts.
  ;; ACTIVITY is the running-session table this shell holds
  ;; (SESSION-PICKER-ROW's clock and spinner); NIL draws a directory of
  ;; settled rows. A fork lists under its session's day, whatever day it moved
  ;; on itself: the day is a top-level row's. => picker rows, in drawing order.
  (let ((underway (session-picker-descendant-starts children activity))
        (day nil)
        (rows '()))
    (loop for row in (nlk:session-tree-order
                      (mapcar #'session-picker-entry entries)
                      :expanded (lambda (id) (and (gethash id open) t)))
          for id = (getf row :id)
          for kids = (gethash id children)
          do (when (zerop (or (getf row :depth) 0))
               (let ((moved (session-local-day (getf row :updated-at))))
                 (when (and moved (not (equal moved day)))
                   (setf day moved)
                   (push (session-day-heading day now-ms) rows))))
             (push (session-picker-row row now-ms
                                       :children (and kids (length kids))
                                       :day day
                                       :started-at (and activity
                                                        (gethash id activity))
                                       :descendant-started-at (gethash id underway))
                   rows))
    (nreverse rows)))

(defun session-picker-prefix (row updated-at now-ms &key (field :label) (offset 0) prefix)
  "Prefix ROW with PREFIX when non-NIL, otherwise its relative update age,
muted by the shared picker renderer."
  ;; A search's rows: its hits and the names that answer the query.
  (prefix-picker-row row (or prefix
                             (if (iso-8601-ms updated-at)
                                 (relative-age-label updated-at now-ms)
                                 "time unknown"))
                     :field field :offset offset))

(defparameter +session-ledger-sources+ '("channel" "cron" "team")
  "The sources a /sessions row names in a chip: a session a room, a cron job
or a team seat began. A shell's and the organism's own say nothing.")

;;; A room's record line opens on its speaker and the ids it carries --
;;; `<speaker> [m<message> u<user>]: <words>' (the channels kit, room.lisp) --
;;; which a first prompt cut at 64 characters is mostly made of, often to the
;;; point that the cut leaves the bracket open. A lane no name was given for
;;; is named by that prompt.
(defparameter +session-room-line+
  (ppcre:create-scanner "^([^\\[\\n]{0,64}?) *\\[m[0-9]+(?: u[0-9]+)?[^\\]\\n]*(?:\\]: *|$)")
  "The speaker tag a room's line opens on: the speaker, then the ids.")

(defun session-spoken (text)
  "TEXT as a row says it: a room's line as its words, or -- when the cut left
none -- its speaker; any other text as it is."
  (multiple-value-bind (from to starts ends) (and text (ppcre:scan +session-room-line+ text))
    (if from
        (let ((words (string-trim " " (subseq text to)))
              (speaker (subseq text (aref starts 0) (aref ends 0))))
          (cond ((plusp (length words)) words)
                ((plusp (length speaker)) speaker)))
        text)))

(defun session-age-label (updated now-ms day)
  "A /sessions row's age: how long ago UPDATED was, on a row of today or one
whose own day is not the DAY it lists under; the clock time it moved at
under an earlier day's heading, which already says the day."
  (let ((moved (session-local-day updated)))
    (cond ((null moved) "unknown")
          ((and (equal moved day)
                (not (equal moved (session-local-day now-ms))))
           (format-clock-minute (iso-8601-ms updated)))
          (t (relative-age-label updated now-ms)))))

(defun session-ledger-paint (&key mark mark-tone age age-tone indent name forks count source)
  "The painter (PICKER-ROW-PAINT) of one /sessions row: an edge, the MARK and
the AGE in a column of their own, the tree's INDENT and the NAME in the room
the rest leaves, and flush right the FORKS it holds, its COUNT of messages
and the SOURCE chip -- each dropping, the source first, when the row is too
narrow for the name to keep a reading's width."
  (lambda (selected room)
    (let* ((plain (if selected :system :muted))
           (title (if selected :dialog-accent :system))
           (right (remove nil
                          (list (and forks
                                     (list (make-seg :text (format nil "~9@a" (format nil "~d fork~:p" forks))
                                                     :tone plain)))
                                (and count (plusp count)
                                     (list (make-seg :text (format nil "~9@a" (format nil "~d msg~:p" count))
                                                     :tone plain)))
                                (and source
                                     (list (make-seg :text (format nil " ~a " source)
                                                     :tone (style-for-tone :sheet-chip)))))))
           ;; The edge, a blank, the mark, a blank, the age, two blanks.
           (lead 14))
      (flet ((after (right) (if right (+ 2 (lane-width right)) 0)))
        ;; The right lane gives way before the name's reading width does, the
        ;; source first; then the name gives way, down to a word's width.
        (loop while (and right (< (- room lead 28 (after right)) 0))
              do (setf right (butlast right)))
        (let* ((right-width (lane-width right))
               (title-width (max 8 (- room lead (after right))))
               (left (append
                      (list (if selected
                                (make-seg :text "▌" :tone :dialog-accent)
                                (make-seg :text " " :tone plain))
                            (make-seg :text " " :tone plain)
                            (make-seg :text (if (plusp (length mark)) mark " ") :tone mark-tone)
                            (make-seg :text " " :tone plain)
                            (make-seg :text (format nil "~8@a" (clip-text-to-width age 8))
                                      :tone (or age-tone plain))
                            (make-seg :text "  " :tone plain)
                            (make-seg :text indent :tone :muted)
                            (let* ((room (max 0 (- title-width (text-display-width indent))))
                                   (shown (clip-text-to-width name room)))
                              (make-seg :text (concatenate 'string shown
                                                           (make-string (max 0 (- room (text-display-width shown)))
                                                                        :initial-element #\Space))
                                        :tone title :bold selected))))))
          (values (append left
                          (when right
                            (cons (make-seg :text (make-string (max 2 (- room (segs-display-width left) right-width))
                                                               :initial-element #\Space)
                                            :tone plain)
                                  (lane-segs right))))
                  (and selected :panel-lift)))))))

(defun lane-width (lane)
  "The columns LANE, cells of segments a row lays two columns apart, spans."
  (loop for cell in lane
        for first = t then nil
        sum (+ (segs-display-width cell) (if first 0 2))))

(defun lane-segs (lane)
  "LANE's cells as one run of segments, two columns apart."
  (loop for cell in lane
        for first = t then nil
        unless first collect (make-seg :text "  " :tone :muted)
        append cell))

(defun session-picker-row (row now-ms &key children day started-at descendant-started-at)
  "One /sessions row, a ledger line: its age -- a running turn's clock in its
place -- its name, and flush right the forks it holds, its messages and where
it came from. DAY is the date of the heading it lists under (SESSION-AGE-LABEL)."
  ;; The row's slots keep what the box's own matcher reads -- the name -- and
  ;; the mark a test reads; PICKER-ROW-PAINT lays it.
  (let ((entry (getf row :entry)))
    (with-json ((id (gethash "id" entry))
                (title :text "title")
                (wd :text "working_directory")
                (updated :text "updated_at")
                (source :string "source")
                (count :integer "message_count")
                (indent (tree-lane-prefix (getf row :fork) (getf row :lanes)))
                (name (or (session-spoken title) id))
                (elapsed (and started-at (running-elapsed-ms started-at now-ms)))
                (underway (and (null elapsed) descendant-started-at
                               (running-elapsed-ms descendant-started-at now-ms)))
                (mark (cond (elapsed (status-mark elapsed))
                            (underway (status-mark underway))
                            (t "")))
                (picker-row (make-picker-row :mark mark
                                             :mark-tone (if elapsed :tool :muted)
                                             :label (concatenate 'string indent name)
                                             :path (or wd "")
                                             :value id)))
      entry
      (setf (picker-row-paint picker-row)
            (session-ledger-paint :mark mark :mark-tone (if elapsed :tool :muted)
                                  :age (if elapsed
                                           (running-clock-label elapsed)
                                           (session-age-label updated now-ms day))
                                  :age-tone (and elapsed :tool)
                                  :indent indent :name name
                                  :forks (and children (plusp children) children)
                                  :count count
                                  :source (find source +session-ledger-sources+ :test #'equal)))
      picker-row)))

(defstruct sessions-fold
  "The open /sessions dialog's fold: the listing it folds over (ENTRIES, the
wire rows), the indexes into it (BY-ID, id -> entry; CHILDREN, id -> the
ids under it), OPEN — the session ids whose forks are drawn, the one fact
the gestures move — and NOW-MS, the age column's clock, read once at open."
  ;; The dialog owns it: Esc drops it, and a reopened picker starts with every
  ;; parent collapsed.
  entries
  by-id
  children
  (open (make-hash-table :test #'equal))
  now-ms)

(nlk:access (fold sessions-fold))

(defun sessions-fold-parent (by-id id &aux (parent (gethash "parent_id" (gethash id by-id))))
  "The session ID hangs under in the listing BY-ID indexes, or NIL when it lists as
a root: its PARENT_ID, one the listing's own rows answer to and not the id itself."
  (and (stringp parent)
       (not (equal parent id))
       (gethash parent by-id)
       parent))

(defun sessions-dialog-context (fold activity)
  "The /sessions dialog's context label: how many sessions are running right
now, under the live spinner, or nothing -- the tabs beside it already count
the ones listed."
  (let ((started (loop for entry in fold.entries
                       for at = (and activity (gethash (gethash "id" entry) activity))
                       when at collect at)))
    (if started
        (format nil "~a ~d running"
                (status-mark (or (running-elapsed-ms (first started) (iso-8601-ms (nlk::iso-now))) 0))
                (length started))
        "")))

(defun sessions-fold-directory (fold id &aux (seen (make-hash-table :test #'equal)))
  "The folder ID's tree was opened in: the working directory of the session
FOLD's listing hangs it from at the top, spelled as WORKSPACE-DIRECTORY
spells a shell's (no trailing separator), or the empty string."
  ;; A fork, a frame and a team seat belong to the folder their tree began in,
  ;; whatever directory they run in themselves, so a tree is listed whole or
  ;; not at all. A cycle ends at its first repeat.
  (loop for up = (sessions-fold-parent fold.by-id id)
        while (and up (not (gethash up seen)))
        do (setf (gethash id seen) t id up))
  (string-right-trim "/\\" (or (json-value (gethash id fold.by-id) :string "working_directory") "")))

(defun sessions-fold-rows (fold &optional activity)
  "FOLD's rows as the dialog holds them: SESSION-PICKER-ROWS over its
listing and the sessions opened so far — a session's own nodes under it —
the running sessions among them drawn live off ACTIVITY (the shell's
running-session table). Each row's KEY is the folder its tree was opened in
(SESSIONS-FOLD-DIRECTORY), the bucket the folder tab selects."
  (let ((rows (session-picker-rows fold.entries fold.children fold.open fold.now-ms activity))
        (heading nil))
    (dolist (row rows rows)
      (if (picker-row-heading row)
          (setf heading row)
          (let ((folder (sessions-fold-directory fold (picker-row-value row))))
            (setf (picker-row-key row) folder)
            (when heading (pushnew folder (day-heading-folders heading) :test #'equal)))))))

(nlk:define-side-table day-heading-folders (:weakness :key)
  "A /sessions day heading -> the folders the sessions listed under it were
opened in (SESSIONS-FOLD-ROWS): the folder tab keeps a heading over its own
rows alone.")

(defun sessions-folder-tabs (fold &aux (here (workspace-directory)))
  "The /sessions tab strip: the sessions opened in this shell's folder, then
every session — each counting FOLD's listing, and no count while the read
that brings it is out."
  (list (list :label "This folder"
              :count (and fold (count here fold.entries
                                      :test #'equal
                                      :key (lambda (entry)
                                             (sessions-fold-directory
                                              fold (gethash "id" entry)))))
              :value here
              ;; A day's heading stands while a row under it is this folder's.
              :match (lambda (row folder)
                       (if (picker-row-heading row)
                           (member folder (day-heading-folders row) :test #'equal)
                           (equal folder (picker-row-key row)))))
        (list :label "All" :count (and fold (length fold.entries)) :value nil)))

(defun sessions-fold-key (model dialog fold direction &aux (id (picker-highlighted-value dialog)))
  "One Left/Right on the open /sessions picker — the fold the highlight
drives."
  ;; Right opens the highlighted session, the nodes it holds splicing in
  ;; under it: its forks, its frames, and the seats a team it opened is
  ;; running. Left closes the highlighted session's own nodes, or — holding
  ;; none open — closes the session it hangs under and parks the cursor
  ;; there. A gesture with nothing to fold declines, leaving the key the
  ;; no-op it was. The rows lay out again and the cursor seats on the session
  ;; the gesture left it on. => T when the fold moved.
  (when id
    (fold-arrow model direction id fold.open (gethash id fold.children)
                (sessions-fold-parent fold.by-id id)
                (lambda (seat)
                  ;; The rows under the dialog, laid out again with the fold
                  ;; as it now stands, the cursor back on SEAT's session.
                  (setf dialog.rows (sessions-fold-rows fold (session-activity-table model)))
                  (picker-reseat-selection dialog seat)))))

;;; --- a picked session attached ------------------------------------------------

(defun attach-picked-session (model value &aux (session-id (if (consp value) (car value) value)))
  "Enter on a /sessions row, VALUE its session id or a hit's
(SESSION-ID . POSITION): attach to that session, or say MODEL already is."
  ;; POSITION, which a conversation-search row carries, is the durable log
  ;; position of the block the search grabbed: the attached session rests on
  ;; that block rather than on the tail, which is the whole point of searching
  ;; your own conversation. The pin is armed before the replay's rows land and
  ;; applied again there (the replay replaces the preview's page with the
  ;; whole session, so the line index that holds now is not the one that holds
  ;; in a moment). The acknowledgement is composer status; => NIL.
  (if (equal session-id model.session-id)
      (progn (sessions-preview-clear model)
             (show-status model "Already attached to this session"))
      (switch-gateway-session model session-id))
  (when (consp value)
    (pin-transcript-position model session-id (cdr value)))
  nil)

;;; --- the pending-exec view -------------------------------------------------
;;; The band shows a pending exec in a five-line window, all it kept once a
;;; click opens it there; /execs opens the same object — the row's own
;;; retained output — at panel height, live while it runs, with the interrupt gesture on the exec it is
;;; about. One reader of the row's buffer, so the band and the panel can never
;;; disagree about what the exec printed. The row's own bounds are the
;;; buffer's (1 KiB head + 2 KiB tail); the omission line says how many of the
;;; stream's lines those bounds dropped. Its command heads it as the call
;;; wrote it, in the five-row window a card's output takes until Ctrl+O, and
;;; an exec that failed ends on the report of the condition it failed on.

(defun bg-row-output-lines (row &aux (window (background-kept-window row))
                                     (omitted (getf window :omitted 0)))
  "ROW's retained output as display lines for the panel => (values LINES
OMITTED TOTAL): BACKGROUND-KEPT-WINDOW, what an open band exec shows, its
omission clause a line of its own."
  (values (append (getf window :head)
                  (when (plusp omitted)
                    (list (format nil "⋮ +~d line~:p hidden" omitted)))
                  (getf window :tail))
          omitted
          (bg-row-written-lines row)))

(defun exec-view-rows (exec-id lines &optional report)
  "The panel's rows — one picker row per output line, then REPORT, the
condition the exec failed on, `error: ' first and in the danger tone — every
row's VALUE the exec id, which is the interrupt gesture's argument."
  (let ((report (split-lines report)))
    (append (loop for line in (or lines (and (null report) (list "(no output yet)")))
                  collect (make-picker-row :label line :value exec-id))
            (loop for line in report
                  for first = t then nil
                  collect (make-picker-row :label (if first (format nil "error: ~a" line) line)
                                           :tone :danger :value exec-id)))))

(defun exec-view-context (band live)
  "The view's header for BAND, its exec: running and how much it wrote while
LIVE, else how it ended (the exit's document, EXEC-END-WORDS) -- `exited 2',
not only that it did."
  ;; The title beside it already names the exec (`Exec 1000'), so the context
  ;; says only its state: it read `Exec 1000  exec 1000 · exited 2'.
  (if live
      (bind (((_ omitted total) (bg-row-output-lines band)))
        (format nil " running · ~d line~:p~@[ · +~d hidden~] "
                total (and (plusp omitted) omitted)))
      (format nil " ~a "
              (if-let (exited (bg-row-exited band))
                (exit-end exited t)
                "exited"))))

(defun exec-view-command (model band)
  "=> ROWS: the explicitly opened exec view shows its whole command."
  (values (command-rows band.command (max 8 (docked-panel-room model.term-width)) :open t)))

(defun exec-view-hints (live)
  "The explicit exec view's interrupt, close and scroll controls."
  (append (when live '(("ctrl+x" . "interrupt")))
          '(("esc" . "close") ("up/down" . "scroll"))))

(defun refresh-exec-view (model dialog band &aux (rows dialog.rows)
                                                  (live (find band (state-background-rows model.state))))
  "Re-read BAND, DIALOG's exec, into its rows, header and hints: live while
the band holds it, and once it left, how it ended with the interrupt gesture
gone. => T when the panel moved."
  (bind ((context (exec-view-context band live))
         (command (exec-view-command model band))
         (hints (exec-view-hints live))
         (new (exec-view-rows (bg-band-row-shell-session-id band) (bg-row-output-lines band)
                              (and (not live) (json-value (bg-row-exited band) :string "error")))))
    (unless live
      (setf dialog.on-delete nil))
    (unless (and (equal (mapcar #'picker-row-label rows) (mapcar #'picker-row-label new))
                 (equal context dialog.context)
                 (equal (mapcar #'segs-text command) (mapcar #'segs-text dialog.summary-rows))
                 (equal hints dialog.footer-hints))
      ;; The tail stays pinned only while the operator was already
      ;; there; a scrolled-up reader is not yanked back by new output.
      (let ((at-tail (>= dialog.selection
                         (1- (length rows)))))
        (setf dialog.rows new
              dialog.context context
              dialog.summary-rows command
              dialog.footer-hints hints)
        (when at-tail
          (setf dialog.selection
                (1- (length dialog.rows)))))
      t)))

(defun open-exec-view (model exec-id &aux (live (find-band-row model.state nil exec-id))
                                          (band (or live (find exec-id (state-ended-band-rows model.state)
                                                                 :key #'bg-band-row-shell-session-id))))
  "Open the exec view on EXEC-ID: its command, its retained output as the
panel's rows, refreshed on every tick while it runs, and how it ended once it
exited -- an exec that ended before the view opened included."
  ;; The view holds the row itself, so it still reads it once the band let it
  ;; go; a row the band holds again under the id (a resubscribe) takes over.
  ;; The refresh fills it, so it opens as a tick finds it.
  (open-picker-dialog
   model (format nil "Exec ~d" exec-id) (exec-view-rows exec-id nil) nil
   :kind :exec
   :context " exited "
   :empty-label "No output yet"
   :footer-hints (exec-view-hints live)
   :on-tick (lambda (model dialog)
              (setf band (or (find-band-row model.state nil exec-id) band))
              (when (and band (refresh-exec-view model dialog band))
                (render-status-frame model)
                t))
   :on-delete (and live
                   (lambda (model value)
                     (interrupt-band-exec model value)
                     (format nil "Interrupt requested (exec ~d)" value))))
  (when band
    (refresh-exec-view model (chat-model-dialog model) band))
  nil)

(defun slash-execs-op (model)
  "/execs: open the exec view, a running exec's live output -- Ctrl+X there
interrupts it -- or how one that ended here ended, with its last lines."
  ;; The view is the band row's retained output at panel height, live, with
  ;; the interrupt gesture.
  ;; => notice string, or NIL. One exec opens its view directly; more open the
  ;; picker, running first and then the ended, newest first, and Enter on a
  ;; row opens that exec's view (the dialog closes first, so a handler may
  ;; open its successor).
  (with-slash-notice ("execs")
    (let* ((running (remove-if-not (lambda (row) (eq :running (bg-band-row-status row)))
                                   (state-background-rows model.state)))
           (rows (append running (state-ended-band-rows model.state))))
      (cond
        ((null rows) "no exec has run here yet")
        ((null (rest rows))
         (open-exec-view model (bg-band-row-shell-session-id (first rows))))
        (t
         (open-picker-dialog
          model (format nil "Execs (~d)" (length rows))
          (loop for row in rows
                collect (make-picker-row
                         :label (command-preview (bg-band-row-command row))
                         :detail (format nil "exec ~d · ~a"
                                         (bg-band-row-shell-session-id row)
                                         (if (member row running)
                                             "running"
                                             (exit-end (bg-row-exited row) t)))
                         :value (bg-band-row-shell-session-id row)))
          (lambda (model id) (open-exec-view model id))
          :kind :execs
          :hint "enter expand  esc close  ↑↓ navigate"
          :empty-label "No execs"))))))

(defun leave-session-op (model)
  "Leave the attached session running: go home on a fresh session the way
/clear does, and open /sessions there with the highlight on the session just
left, once the directory read lands."
  ;; Nothing is marked: a session runs on whoever is attached, and /sessions
  ;; shows which are running. => notice string, or NIL.
  (with-slash-notice ("leave")
    (let ((id model.session-id))
      (slash-clear-op model)
      (slash-sessions-op model :seat id))))

;;; --- the /sessions transcript preview ------------------------------------
;;; The Zig sessions_dialog_preview lane (model/root.zig
;;; pendingGatewaySessionsDialogHistory + sessionsDialogPreview): the
;;; highlighted row's durable history is read once over the history pager,
;;; folded through the same frame authority the subscription replay uses,
;;; and substituted for the transcript underneath the dialog. Enter promotes
;;; the already-folded rows into the live state, so the transcript flips the
;;; moment the session is chosen instead of waiting on the replay that
;;; rebuilds it authoritatively.

(defun sessions-preview-clear (model)
  "Drop the armed preview, stop following its session — the attached
session alone again — and repaint the live transcript."
  ;; The viewport pin goes with it: a preview that ends takes the found-block
  ;; promise with it.
  (clear-transcript-pin model)
  (when model.preview-session-id
    (setf model.preview-session-id nil
          model.preview-activity nil)
    (when model.ws (send-subscribe model.ws (subscription-entries model))))
  (when model.transcript-preview (setf model.transcript-preview nil) (render-state model)))

(defun sessions-preview-install (model session-id entry position &key (follow t))
  "Seat ENTRY — a (SESSION-ID . STATE) page — under the dialog and repaint:
the window rests on POSITION's block when the row named one, else on the
newest rows as a preview always did. FOLLOW has the lane follow the session
live from here."
  ;; The newest rows are laid out by the paint that shows them
  ;; (RENDER-STATE-AT-PAINT), so a held arrow that crosses three rows between
  ;; two frames lays out one.
  (setf model.transcript-preview entry)
  (cond (position (pin-transcript-position model session-id position))
        (t (pin-transcript-bottom model)
           (render-state-at-paint model)))
  (when follow (preview-follow model session-id))
  model)

(defun sessions-preview-arm (model value &key memo (follow t) (position (and (consp value) (cdr value)))
                             &aux (session-id (if (consp value) (car value) value)))
  "Fold the durable history of VALUE's session into MODEL's preview lane and
repaint; FOLLOW has the lane follow it live at once."
  ;; VALUE is a row's value: a session id, or a conversation-search hit's
  ;; (SESSION-ID . POSITION), whose POSITION is the default below.
  ;; The attached session previews nothing — the transcript underneath already
  ;; is it — and a view already under the dialog never refetches. POSITION, on a
  ;; conversation-search row, is the durable log position of the block the row
  ;; matched: the page ENDS 64 events past that block (READ-SESSION-PAGE) and
  ;; the window rests on it
  ;; (PIN-TRANSCRIPT-POSITION) rather than on the newest rows — reading from the
  ;; block is what makes an old hit previewable at all, since the newest page
  ;; would not hold it and the context under it is what the operator lands in.
  ;; MEMO, when the
  ;; dialog passes one, is its read-ahead (SESSIONS-PREVIEW-READ-AHEAD), keyed
  ;; on (session . position): a row it already folded installs in this pass, a
  ;; row whose read is out installs when the page lands. A first visit reads
  ;; its page off the loop thread (READ-SESSION-PAGE, seated by
  ;; INSTALL-SESSION-PAGE): the read is a gateway round trip — a history page,
  ;; its hash check, JSON both ways — and a shell that waits for it on the loop
  ;; thread would answer no key until it lands. A failed read leaves the lane
  ;; empty so the next highlight retries (the Zig cleared-target re-selection);
  ;; no failure is kept as a page.
  (let ((key (cons session-id position)))
    (cond
      ((or (null session-id)
           (equal session-id model.session-id))
       (sessions-preview-clear model))
      ((and (equal session-id (car model.transcript-preview))
            (if position
                (equal key (gethash model *transcript-pins*))
                (null (gethash model *transcript-pins*))))
       (when follow (preview-follow model session-id)))
      ((and memo (consp (gethash key memo)))
       (sessions-preview-install model session-id (gethash key memo) position
                                 :follow follow))
      ;; The dialog's reads are the read-ahead's, which starts this row's
      ;; first: a held arrow outrunning them crossed rows faster than pages
      ;; land, and a read a row queued the gateway past a second. A landing
      ;; on a row whose read failed reads it again.
      ((null memo)
       (sessions-preview-read model key nil :follow follow))
      ((eq :failed (gethash key memo))
       (remhash key memo)))
    (when memo (sessions-preview-read-ahead model memo :follow follow))
    model))

;;; A /sessions dialog reads ahead: every landing, and every page that lands,
;;; tops up the reads of the rows nearest the highlight, so a held arrow
;;; arrives on rows already folded and each one paints in the frame that
;;; highlights it. Its memo holds a row's fold, :READING while the row's read
;;; is out, or :FAILED once it failed -- read again only when a landing asks,
;;; never by the read-ahead, so a gateway refusing every read is not asked
;;; again the moment it refuses -- and lets go of the oldest folds outside
;;; the reach: a fold is
;;; ~350 KB, and a dialog that kept every row it passed held them all until
;;; it closed.
(defparameter *sessions-preview-reach* 3
  "Rows on either side of the /sessions highlight whose pages are read ahead.")

(defparameter *sessions-preview-reads* 3
  "Preview pages one /sessions dialog reads at once.")

(defparameter *sessions-preview-kept* 16
  "Folded previews one /sessions dialog keeps.")

(defun sessions-preview-read (model key memo &key follow &aux (dialog model.dialog))
  "Read KEY's page off the loop and seat it (INSTALL-SESSION-PAGE); MEMO, when
given, holds :READING for KEY meanwhile, and the landing reads further ahead."
  ;; A page for a row the highlight has moved off changes nothing on screen,
  ;; so its landing owes no frame.
  (when memo (setf (gethash key memo) :reading))
  (off-loop model
            ;; A hit's page ends 64 events past its block: the events
            ;; strictly below its position + 65.
            (lambda () (read-session-page model (car key) (and (cdr key) (+ (cdr key) 65))))
            (lambda (model page)
              (prog1 (if (install-session-page model key memo page :follow follow) t :quiet)
                (when (and memo (eq dialog model.dialog))
                  (sessions-preview-read-ahead model memo :follow follow))))))

(defun sessions-preview-read-ahead (model memo &key follow &aux (dialog model.dialog))
  "Start reads for the rows within *SESSIONS-PREVIEW-REACH* of the open
dialog's highlight that MEMO holds nothing for, nearest first, while fewer
than *SESSIONS-PREVIEW-READS* are out; then drop the oldest folds outside
that reach past *SESSIONS-PREVIEW-KEPT*."
  (when (picker-dialog-p dialog)
    (let* ((at dialog.selection)
           (from (max 0 (- at *sessions-preview-reach*)))
           (near (stable-sort
                  (loop for row in (nthcdr from (picker-visible-rows dialog))
                        for index from from to (+ at *sessions-preview-reach*)
                        for key = (preview-row-key (picker-row-value row))
                        when (and key (not (equal (car key) model.session-id)))
                          collect (cons (abs (- index at)) key))
                  #'< :key #'car))
           (entries (alexandria:hash-table-values memo))
           (out (count :reading entries))
           (excess (- (count-if #'consp entries) *sessions-preview-kept*)))
      (loop for (nil . key) in near
            while (< out *sessions-preview-reads*)
            unless (gethash key memo)
              do (sessions-preview-read model key memo :follow follow)
                 (incf out))
      ;; A table walks in the order its keys went in, oldest first.
      (loop for key being the hash-keys of memo using (hash-value entry)
            while (plusp excess)
            when (and (consp entry) (not (rassoc key near :test #'equal)))
              do (remhash key memo)
                 (decf excess)))))

(defun read-session-page (model session-id before)
  "GET SESSION-ID's history page for the preview lane: the 128 events strictly
below log position BEFORE, the newest when NIL. => (FRAMES . OLDER), OLDER the
position the page behind it ends under, NIL when this one starts the session."
  ;; 128 is the page a held arrow reads one of a row. Without this limit,
  ;; the session_history_max_page_limit of 256 would make the frames under
  ;; it wait. 128 is the first page only; a rested highlight reads the
  ;; pages behind it (SESSIONS-PREVIEW-OLDER), and the replay an Enter
  ;; starts reads the rest.
  (let ((page (session-control-call model :get
                                    (if before
                                        (format nil "history?limit=128&before=~d" before)
                                        "history?limit=128")
                                    :session-id session-id)))
    (cons (gethash "frames" page)
          (and (gethash "has_more" page) (gethash "next_before" page)))))

(defun preview-row-key (value)
  "The (SESSION-ID . POSITION) a row whose value is VALUE previews, or NIL: a
directory row's value is its session id, a conversation-search hit's is
(SESSION-ID . LOG-POSITION)."
  (cond ((consp value) (cons (car value) (cdr value)))
        ((stringp value) (cons value nil))))

(defun preview-highlight-key (model &aux (dialog model.dialog))
  "The (SESSION-ID . POSITION) the open dialog's highlighted row previews, or NIL."
  (when (picker-dialog-p dialog)
    (preview-row-key (picker-highlighted-value dialog))))

(defun sessions-preview-follow-at-rest (model dialog)
  "Have the preview lane follow the session shown under DIALOG live once
DIALOG's highlight has rested *PICKER-HIGHLIGHT-REST*. => NIL: the follow
itself moves nothing on screen."
  ;; A subscription per row crossed was a gateway command on the loop -- a
  ;; store read an in-process gateway answers under the lock the read-ahead
  ;; holds -- and the attached session's own subscription re-answered each
  ;; time; the rows a held arrow crosses are read, not watched.
  (let ((shown (car model.transcript-preview)))
    (when (and shown
               (not (equal shown model.preview-session-id))
               (picker-highlight-rested-p dialog))
      (preview-follow model shown))
    nil))

(defun picker-highlight-rested-p (dialog)
  "Whether DIALOG's highlight has rested *PICKER-HIGHLIGHT-REST*."
  (>= (- (get-internal-real-time) dialog.last-highlight-at)
      (* *picker-highlight-rest* internal-time-units-per-second)))

;;; A preview starts as the newest page, 128 events, with nothing behind it:
;;; a page is counted in events where the view counts lines, and Standard
;;; folds a turn's tool trail to one row. The route answers where the page
;;; behind a page ends (next_before, has_more); the preview keeps that and
;;; every event its fold has taken beside the entry (PREVIEW-PAGING), and once
;;; the highlight rests reads the page behind whenever the fold projects less
;;; than a state's first projection does (*TAIL-PROJECTION-SCREENS* screens) or
;;; the operator has scrolled to its top. Without that read, a preview whose
;;; fold projects less than a state's first projection would stop scrolling
;;; before the oldest ask. A held arrow never pays for it: the page a row gets
;;; first is the 128 it always was, and a row the highlight has moved off reads
;;; no further.

(nlk:define-side-table preview-paging (:weakness :key)
  "Preview entry (SESSION-ID . STATE) -> a plist: :EVENTS every event folded
into its state, oldest first, in a vector that grows with the live follow;
:BEFORE the log position the page behind them ends under, NIL at the
session's start; :READING T while that page is out; :CHECKED how many events
the last look at the fold's length saw.")

(defun preview-page-entry (session-id page &key title position)
  "The preview entry for SESSION-ID's PAGE, a READ-SESSION-PAGE answer: its
fold, with the events behind it and where the page behind them ends kept.
TITLE is the name the directory lists the session under, POSITION the log
position of the hit the page was read around, NIL for the newest page."
  ;; The header shows the session, not the page (SESSION-DISPLAY-TITLE,
  ;; HEADER-STATUS-PARTS): a fold that does not hold the session's whole
  ;; history -- a page with one behind it, or one around a hit -- carries the
  ;; session's name rather than the first prompt it happens to start at, and
  ;; its usage lane waits for the gateway's sums (PREVIEW-INSTALL-HEADER).
  (let* ((events (frames-events (car page)))
         (state (fold-events events))
         (entry (cons session-id state))
         (partial (or (cdr page) position)))
    (setf (preview-paging entry)
          (list :events (make-array (length events) :adjustable t :fill-pointer t
                                                    :initial-contents events)
                :before (cdr page) :newest (null position)
                :reading nil :checked nil :header nil))
    (when partial (setf (gethash "usage_pending" state) t))
    (cond (title (setf (gethash "session_title" state) title))
          (partial (setf (gethash "session_title" state) "")))
    entry))

(defun preview-carry-slots (from to landed)
  "The slots a preview's state keeps beside its rows, FROM the state a refold
replaces TO the one it made: the name and the wait for the gateway's usage,
and with LANDED those sums themselves, which no page of rows recomputes."
  (dolist (key (if landed
                   '("session_title" "usage_pending" "session_usage" "context_usage")
                   '("session_title" "usage_pending")))
    (multiple-value-bind (value present) (gethash key from)
      (if present (setf (gethash key to) value) (remhash key to))))
  to)

(defun preview-keep-event (entry event)
  "Keep EVENT, which the live follow just folded into ENTRY's state, beside
the events behind it, so a page read later folds onto the same history."
  (when-let (paging (preview-paging entry))
    (vector-push-extend event (getf paging :events))))

(defun preview-wants-older-p (model entry &aux (window model.band) (paging (preview-paging entry)))
  "Whether ENTRY, the preview under MODEL's dialog, should read the page
behind it: the operator reads at the top of its fold, or its fold projects
fewer than *TAIL-PROJECTION-SCREENS* terminal heights of lines."
  (let ((rows (length (getf paging :events)))
        (start model.projection-start))
    (and window
         (or (and (not model.follow-bottom)
                  (< (band-window-offset window) window.height)
                  ;; Rows above the window that are not yet projected are the
                  ;; scroll's to grow (GROW-TRANSCRIPT-PROJECTION), not a page's.
                  start (eq (car start) (cdr entry)) (null (cdr start)))
             ;; The look lays the fold out, so it is taken once for each
             ;; event count rather than at every tick.
             (and (/= rows (or (getf paging :checked) -1))
                  (progn (setf (getf paging :checked) rows)
                         (with-render-scope (model)
                           (null (tail-projection-start
                                  (cdr entry) (projection-width model)
                                  (* *tail-projection-screens* model.term-height))))))))))

(defun install-older-page (model entry page)
  "Fold PAGE, the read behind ENTRY's oldest events, in front of them and
repaint when ENTRY is still the preview. => T when that moved the screen."
  ;; The fold goes again from the oldest event: the reducer folds forward
  ;; only. The live follow's events are among those kept, so a session that
  ;; ran while it was previewed keeps what it said. A failed read ends the
  ;; reach for this entry, so a tick does not ask again.
  (let ((paging (preview-paging entry)))
    (setf (getf paging :reading) nil
          (getf paging :checked) nil)
    (cond
      ((off-loop-failure page)
       (setf (getf paging :before) nil)
       nil)
      (t
       (let* ((older (frames-events (car page)))
              (events (getf paging :events))
              (all (make-array (+ (length older) (length events)) :adjustable t :fill-pointer t)))
         (replace all older)
         (replace all events :start1 (length older))
         (setf (cdr entry) (preview-carry-slots (cdr entry) (fold-events all)
                                                (eq :landed (getf paging :header)))
               (getf paging :events) all
               (getf paging :before) (cdr page))
         ;; The newest page's history read to its start is the whole session:
         ;; its fold is exact, and its first prompt its name.
         (when (and (getf paging :newest) (null (cdr page)))
           (remhash "usage_pending" (cdr entry))
           (when (equal "" (gethash "session_title" (cdr entry)))
             (remhash "session_title" (cdr entry))))
         (when (eq entry model.transcript-preview)
           ;; A window that reads is on a fold projected whole (PREVIEW-WANTS-
           ;; OLDER-P), and the new fold's rows are new: it is projected whole
           ;; again, or the window would land at the top of one tail window.
           (keep-window-from-bottom
            model (lambda ()
                    (unless model.follow-bottom
                      (setf model.projection-start (cons (cdr entry) nil)))
                    (render-state model)))
           t))))))

(defun sessions-preview-older (model dialog &aux (entry model.transcript-preview)
                                                 (paging (and entry (preview-paging entry))))
  "Read the page behind the preview under DIALOG once its highlight rests, when
the preview wants it (PREVIEW-WANTS-OLDER-P), one page at a time. => NIL:
nothing moves on screen until the page lands."
  (when (and paging
             (getf paging :before)
             (not (getf paging :reading))
             (picker-highlight-rested-p dialog)
             (preview-wants-older-p model entry))
    (setf (getf paging :reading) t)
    (let ((before (getf paging :before)))
      (off-loop model
                (lambda () (read-session-page model (car entry) before))
                (lambda (model page)
                  (if (install-older-page model entry page) t :quiet)))))
  nil)

;;; The header of a shell shows the session it is on: its title, and its usage
;;; lane -- what the session has spent and how full its window is. A preview
;;; of a session that its page does not hold whole cannot sum that lane, so the
;;; gateway sums it (GET sessions/ID/header) once the highlight rests, and the
;;; numbers are seated on the previewed state, where the header reads them like
;;; an attached session's: the rounds the live follow folds past THROUGH, the
;;; newest round the gateway counted, add to them as they land.

(defun usage-slot-plist (object)
  "The usage plist a JSON usage slot (USAGE-SLOT-JSON's) stands for."
  (when (hash-table-p object)
    (loop for key being the hash-keys of object using (hash-value value)
          nconc (list (intern (string-upcase (substitute #\- #\_ key)) :keyword) value))))

(defun usage-event-position (event &aux (event (gateway-event-payload event)))
  "The log position of EVENT, one the reducer folds, when it is a turn.usage
fact; MOST-POSITIVE-FIXNUM for one that carries none, a fact not yet kept."
  (and (equal "turn.usage" (json-value event :any "type"))
       (or (json-value event :integer "metadata" "log_position") most-positive-fixnum)))

(defun preview-install-header (entry facts)
  "Seat FACTS, the gateway's answer for ENTRY's session, on its state: the
name, the session's usage through the newest round counted, and every round
the entry kept past that one folded on top."
  (let* ((state (cdr entry))
         (through (or (json-value facts :integer "through") 0))
         (sums (initial-chat-state))
         (title (json-value facts :string "title")))
    (setf (gethash "session_usage" sums) (usage-slot-plist (gethash "session_usage" facts))
          (gethash "context_usage" sums) (usage-slot-plist (gethash "context_usage" facts)))
    (loop for event across (getf (preview-paging entry) :events)
          for position = (usage-event-position event)
          when (and position (> position through))
            do (apply-gateway-event sums event))
    (setf (gethash "session_usage" state) (gethash "session_usage" sums)
          (gethash "context_usage" state) (gethash "context_usage" sums))
    (when (and title (plusp (length title)))
      (setf (gethash "session_title" state) title))
    (remhash "usage_pending" state)
    state))

(defun install-header-facts (model entry facts)
  "Take the gateway's header answer for ENTRY. => T when it moved the screen."
  ;; A failed read leaves the lane waiting, as a page that could not be read
  ;; leaves the preview empty: a wrong number is worse than none.
  (let ((paging (preview-paging entry)))
    (cond ((off-loop-failure facts) (setf (getf paging :header) :failed) nil)
          (t (preview-install-header entry facts)
             (setf (getf paging :header) :landed)
             (eq entry model.transcript-preview)))))

(defun sessions-preview-header (model dialog &aux (entry model.transcript-preview)
                                                  (paging (and entry (preview-paging entry))))
  "Ask the gateway for the header facts of the preview under DIALOG once its
highlight rests, when its fold cannot sum them. => NIL: nothing moves on screen
until they land."
  (when (and paging
             (gethash "usage_pending" (cdr entry))
             (null (getf paging :header))
             (picker-highlight-rested-p dialog))
    (setf (getf paging :header) :reading)
    (off-loop model
              (lambda () (session-control-call model :get "header" :session-id (car entry)))
              (lambda (model facts)
                (if (install-header-facts model entry facts) t :quiet))))
  nil)

(defun install-session-page (model key memo page &key (follow t))
  "Seat one preview page: fold it here — the loop thread's own fold, so the
reducer never runs beside a stream frame — keep it in MEMO, and install it
under the dialog when the highlight still rests on the row KEY was read for.
=> T when that moved the preview."
  ;; A page for a row the operator has moved off is memoized and otherwise
  ;; dropped, the way a search answer to a query the box has moved off is. A
  ;; failed read leaves the lane empty and MEMO holding no page for its row,
  ;; so the next highlight there reads it again.
  (cond
    ((off-loop-failure page)
     (when memo (setf (gethash key memo) :failed))
     (when (equal key (preview-highlight-key model))
       (sessions-preview-clear model)
       t))
    (t
     (let ((entry (preview-page-entry (car key) page :title (directory-title model (car key))
                                                     :position (cdr key))))
       (when memo (setf (gethash key memo) entry))
       (when (equal key (preview-highlight-key model))
         (sessions-preview-install model (car key) entry (cdr key) :follow follow)
         t)))))

(defun read-session-directory (model &aux (entries (coerce (gateway-model-http-call
                                                            model :get "/api/gateway/sessions" nil)
                                                           'list)))
  "GET the gateway's session directory. => its entries."
  ;; Every read that lands also replaces this shell's own copy
  ;; (*SESSION-DIRECTORY*), which is what the next keystroke opens on.
  (setf (gethash model *session-directory*)
        (list :entries entries :at (get-internal-real-time)))
  entries)

(defun slash-sessions-op (model &key seat)
  "/sessions, and the Ctrl+S chord over it: open the session picker. It opens
on the keystroke, and the directory read — a gateway round trip — happens on
a thread of its own and fills the dialog already on screen
(INSTALL-SESSION-DIRECTORY, OPEN-SESSIONS-PICKER-AWAITING). SEAT, a session
id, takes the highlight once its row is listed (Left's session just left)."
  ;; Nothing in the dialog's frame needs that answer, so nothing waits for it:
  ;; the notice a failed read owes lands in the composer status from the install
  ;; rather than as this op's answer. => NIL.
  (open-sessions-picker-now model :seat seat)
  ;; Closing or replacing this picker cancels its pending read's
  ;; UI effect. An old completion never reopens or fills another.
  (dialog-read model
               (lambda () (read-session-directory model))
               (lambda-ignoring (model dialog value)
                 (install-session-directory model value :seat seat)))
  nil)

;;; --- the /sessions conversation search --------------------------------------
;;; The picker's box is a title filter until the operator types in it; from
;;; there it is a grep over every session's conversation, answered by the
;;; gateway (POST /api/gateway/sessions/search). The box keeps one truth —
;;; what the operator typed — and the search hangs three facts off it: what is
;;; staged, what is in flight, and which query the rows on screen answer. Both
;;; live in a table keyed by the dialog rather than in slots, because a
;;; running image cannot grow a structure, and the dialog's own slots belong
;;; to the picker.
;;;
;;; A hit row's value is (SESSION-ID . LOG-POSITION) — the pair Enter needs to
;;; land on the block — while the directory's rows keep their plain session
;;; id. The two shapes are told apart where they are read, never guessed at.

;;; The dialog ranks nothing (the gateway answers newest first), so this is
;;; also how many hits a box shows, and a page that hit the bound says so with
;;; a `+'.
(defparameter *session-search-limit* 60
  "Rows one conversation search asks the gateway for.")

;;; The read is a store scan behind an HTTP round trip, and it runs on a
;;; thread of its own either way.
(defparameter *session-search-rest* 0.15
  "Seconds a typed query rests before the search fires: typing `docker' is one
search, not six.")

;;; :FOLD is the listing's fold (the rows an empty box shows), :STAGED the
;;; query the box holds now, :STAGED-AT when that query last changed, :FIRED
;;; the query a read is out for, :ROWS-QUERY the query the rows on screen
;;; answer (`' for the directory). The dialog takes its state with it: the
;;; keys are weak.
(defvar *session-search* (make-hash-table :test #'eq :weakness :key)
  "Weak memo: the open /sessions dialog -> its search state, a plist.")

(defun read-session-search (model query)
  "POST the gateway's conversation search for QUERY => its rows."
  (gethash "rows" (gateway-model-http-call
                   model :post "/api/gateway/sessions/search"
                   (json-object "query" query "limit" *session-search-limit*))))

(defun session-search-picker-rows (json-rows fold &aux (now-ms (iso-8601-ms (nlk::iso-now))))
  "The conversation-search rows: one per matching block, the session's title
leading, the matched line next, the speaker and the block's age last."
  ;; A snippet is ONE line (NLK:ONE-LINE) — the row is a row whatever the
  ;; message held. Each row's value is (SESSION-ID . LOG-POSITION), the pair
  ;; the attach and the viewport pin read.
  (loop for row across (if (vectorp json-rows) json-rows #())
        for session-id = (json-value row :string "session_id")
        for seq = (json-value row :integer "seq")
        for at = (json-value row :text "occurred_at")
        for snippet = (nlk:one-line (json-value row :text "snippet"))
        for role = (json-value row :string "role")
        when (and session-id seq)
          collect (session-picker-prefix
                   (make-picker-row
                   :key (or (json-value row :text "title") session-id)
                   :label (or snippet "")
                   ;; How a hit names its speaker: `you' for the operator, `agent'
                   ;; for the answer, the tool's own name for tool traffic.
                   :detail (format nil "~a~@[ · ~a~]"
                                   (dispatch role equal ("user" "you") ("assistant" "agent")
                                     (t (or (json-value row :text "tool") "log")))
                                   (and at (relative-age-label at now-ms)))
                   :value (cons session-id seq))
                   (when-let (entry (and fold (gethash session-id (sessions-fold-by-id fold))))
                     (json-value entry :text "updated_at"))
                   now-ms :field :key)))

(defun session-name-picker-rows (fold query hits &aux (now-ms (iso-8601-ms (nlk::iso-now))))
  "The sessions in FOLD's listing whose name holds every word of QUERY and
that no row of HITS already finds, as rows leading the search's: a name the
operator gave is found by its words, which no conversation holds."
  (let ((words (split-words (string-downcase query)))
        (found (mapcar (lambda (row) (car (picker-row-value row))) hits)))
    (loop for entry in (and fold words fold.entries)
          for id = (gethash "id" entry)
          for title = (json-value entry :text "title")
          when (and title (not (member id found :test #'equal))
                    (every (lambda (word) (search word (string-downcase title))) words))
            collect (session-picker-prefix
                     (make-picker-row
                     :label title
                     :detail "name"
                     :value id) (json-value entry :text "updated_at") now-ms))))

(defun sessions-search-tick (model dialog)
  "The /sessions dialog's frame-clock tick, and the search box's own clock:
stage what the box holds, and once that has rested *SESSION-SEARCH-REST* ask
the gateway."
  ;; An emptied box needs no round trip — the session directory comes back at
  ;; once, the rows the dialog opened with, the fold's own. One read is out
  ;; per query: a keystroke landing while a read is in flight stages behind
  ;; it, and the answer to a query the box has moved off is dropped where it
  ;; lands. A live shell reads on a thread of its own — the read is a store
  ;; scan behind an HTTP round trip, and on the loop thread every frame would
  ;; wait for it. With no live program (tests drive this tick directly, and
  ;; must not spawn threads) the read and the install run inline. => T when
  ;; the rows moved (the directory came back under an emptied box); a fired
  ;; read answers by its own message.
  (when-let (state (gethash dialog *session-search*))
    (let ((query (string-trim " " dialog.search)))
      (unless (equal query (getf state :staged))
        (setf (getf state :staged) query
              (getf state :staged-at) (get-internal-real-time)))
      (cond
        ((equal (getf state :staged) (getf state :rows-query)) nil)
        ((equal (getf state :staged) (getf state :fired)) nil)
        ((< (since-ms (getf state :staged-at)) (* 1000 *session-search-rest*))
         nil)
        ((zerop (length query))
         (let ((fold (getf state :fold)))
           (setf (getf state :rows-query) ""
                 dialog.rows (sessions-fold-rows
                                              fold (session-activity-table model))
                 dialog.context
                 (sessions-dialog-context fold (session-activity-table model)))
           (picker-seat-top dialog))
         (picker-rows-filtered-for dialog nil)
         (picker-notify-highlight model dialog :instant t)
         (render-state model)
         t)
        (t
         (setf (getf state :fired) query)
         (off-loop model
                   (lambda () (read-session-search model query))
                   (lambda (model value) (install-session-search model query value)))
         nil)))))

(defun install-session-search (model query value)
  "Install one conversation-search answer on the /sessions dialog QUERY was
read for."
  ;; An answer to a query the box has moved off is dropped: the box is the one
  ;; truth about what the operator is looking for. A failed read leaves the
  ;; rows where they were and says so beside the title.
  (let* ((dialog model.dialog)
         (state (and (picker-dialog-p dialog)
                     (eq :sessions dialog.kind)
                     (gethash dialog *session-search*))))
    (when (and state (equal query (getf state :fired)))
      (setf (getf state :fired) nil))
    (when (and state
               (equal query (string-trim " " dialog.search)))
      (if-let (failure (off-loop-failure value))
          (setf dialog.context
                (format nil " search failed: ~a" failure))
          (let* ((hits (session-search-picker-rows value (getf state :fold)))
                 (rows (append (session-name-picker-rows (getf state :fold) query hits) hits)))
            (setf dialog.rows rows
                  dialog.selection 0
                  dialog.view-offset 0
                  dialog.context
                  ;; `~:p' adds the `s' to the noun before it, so the noun
                  ;; has to be the singular that takes one (match, not matchs).
                  (format nil " ~d ~:[matches~;match~]~:[~;+~]"
                          (length rows) (= (length rows) 1)
                          (= (length hits) *session-search-limit*))
                  (getf state :rows-query) query)
            ;; The rows are the gateway's: the dialog shows them verbatim
            ;; (*PICKER-FILTERED-ROWS*) instead of re-filtering them.
            (picker-rows-filtered-for dialog query)
            ;; The first hit previews like any selection landing: the
            ;; transcript underneath rests on the block before Enter does.
            (picker-notify-highlight model dialog :instant t)))
      (render-state model))))

;;; Said on either tab: this folder's list is the one a picker opens on, and an
;;; empty store is the only way the other one is empty too.
(defparameter +sessions-empty-label+ "No sessions in this folder yet · tab shows all"
  "The /sessions body while its list holds no rows.")

(defun open-sessions-picker-dialog (model fold &key seat
                                              &aux (memo (make-hash-table :test #'equal)))
  "Open the /sessions dialog over FOLD's rows — none, under the reading copy,
while the directory read is still out and FOLD is NIL — and file its own
state beside it: FOLD, and MEMO, the preview read-ahead kept for its lifetime.
SEAT, a session id, takes the highlight when its row is listed."
  ;; Every landing is LIVE, the open's included: this picker's handler never
  ;; waits on the loop -- a row's history read (measured ~70 ms off a
  ;; 1600-session store) runs off it and was usually started rows before the
  ;; highlight arrived, and the fold it lands as is laid out by the paint
  ;; that shows it -- so a held arrow previews every row it crosses. Staged,
  ;; a scrub showed one row in three hundred milliseconds.
  ;; No title: the strip's chips say which list is on screen, and the
  ;; running count stands at the right of them.
  (open-picker-dialog
   model "" (if fold (sessions-fold-rows fold (session-activity-table model)) '())
   #'attach-picked-session
   :empty-label (if fold +sessions-empty-label+ "Reading the sessions…")
   :highlight-delivery :live
   :kind :sessions
   :searchable t
   ;; The fold: Left and Right open and close the highlighted session's forks
   ;; under it (the selection spends Up/Down alone). Every parent starts
   ;; collapsed — the door state — and the fold dies with the dialog. A box
   ;; holding a query shows the gateway's hits instead, which are no tree, so
   ;; the gesture declines there — and so does a fold that is not there yet
   ;; (the read still out: the rows arrive with it).
   :on-arrow (lambda (model dialog direction)
               (let ((fold (getf (gethash dialog *session-search*) :fold)))
                 (when (and fold (not (plusp (length (string-trim " " dialog.search)))))
                   (sessions-fold-key model dialog fold direction))))
   ;; Every selection landing — keys, search edits, hover — hydrates the
   ;; highlighted session into the transcript underneath, the way /themes
   ;; engages the highlighted palette; a hit also carries the block to rest
   ;; on. The memo lives as long as this dialog: scrubbing back through
   ;; the rows it holds costs render only, and closing the dialog drops it
   ;; so a reopened picker never shows a stale preview.
   :on-highlight (lambda (model row)
                   (sessions-preview-arm model (picker-row-value row) :memo memo :follow nil))
   :on-cancel #'sessions-preview-clear
   :on-rename #'sessions-rename-start
   ;; The dialog's clock, three readers: the box's own (the conversation search
   ;; fires once the query has rested), the running-session spinner, whose
   ;; paint lands each time it moves to its next frame, and the live follow of
   ;; the row the highlight rests on.
   :on-tick (lambda (model dialog &aux (moved (sessions-search-tick model dialog))
                                       (drawn (refresh-sessions-dialog model :tick t)))
              (sessions-preview-follow-at-rest model dialog)
              (sessions-preview-older model dialog)
              (sessions-preview-header model dialog)
              (or moved drawn))
   :search-placeholder "Search names and conversations"
   :no-match-label "No matching sessions"
   :footer-hints :sessions)
  ;; The context row names the directory's shape — the rows it holds and the
  ;; sessions running among them. A dialog the keystroke opened before its
  ;; read landed keeps the plain label until the read fills it
  ;; (OPEN-SESSIONS-PICKER).
  (let ((dialog model.dialog))
    (when (and fold (picker-dialog-p dialog))
      (setf dialog.context
            (sessions-dialog-context fold (session-activity-table model))))
    ;; The picker opens on this folder's sessions, and Tab reaches every one.
    (register-picker-tabs dialog (sessions-folder-tabs fold))
    ;; The seat is placed among the rows the open tab shows, and previewed:
    ;; SEAT's row, else the first session under the heading the list opens on.
    (when (and fold (picker-dialog-p dialog))
      (if (and seat (find seat (picker-visible-rows dialog) :key #'picker-row-value :test #'equal))
          (picker-reseat-selection dialog seat)
          (picker-seat-top dialog))
      (picker-notify-highlight model dialog)))
  ;; The dialog's own state, filed beside it — a table, not a slot: a running
  ;; image cannot grow a structure the picker already holds instances of.
  ;; Every key the dialog writes is born here: SETF of an absent plist key
  ;; conses onto a local binding nothing else holds, so a key added later
  ;; would never be seen again (the throttle below reads its own).
  (setf (gethash model.dialog *session-search*)
        (list :fold fold :staged "" :staged-at 0 :fired nil
              :rows-query "" :activity-frame nil))
  model)

;;; --- F2: a session renamed where it is listed ---------------------------------
;;; The field opens over the list on the highlighted row, directory row or
;;; search hit alike, holding its name; Enter posts it to the title route off
;;; the loop, and the name that route answers -- the one now in effect -- is
;;; written into the listing the dialog holds, which is also this shell's
;;; cached directory, so every row naming the session reads it at once.

(defun sessions-rename-start (model dialog)
  "F2 on /sessions: the highlighted session's name in the dialog's field."
  (declare (ignore model))
  (let* ((value (picker-highlighted-value dialog))
         ;; A directory row's value is its id, a search hit's (ID . SEQ).
         (id (if (consp value) (car value) value))
         (fold (getf (gethash dialog *session-search*) :fold))
         (entry (and fold (stringp id) (gethash id fold.by-id))))
    (when (stringp id)
      (setf dialog.input-label "Name"
            ;; A hit's KEY is its session's name; a directory row's is its folder.
            dialog.input (or (and entry (json-value entry :string "title"))
                             (and (consp value)
                                  (picker-row-key (selected-of (picker-visible-rows dialog)
                                                               dialog.selection)))
                             "")
            dialog.input-context "empty gives back the name its first prompt makes"
            ;; The sessions keys mean nothing to a field: its own take the footer.
            dialog.footer-hints nil
            dialog.hint "enter rename  esc keep the name  ctrl+u clear"
            dialog.on-input (lambda (model text) (sessions-rename-finish model dialog id text))))
    t))

(defun sessions-rename-finish (model dialog id text)
  "Enter (TEXT) or Esc (NIL) on the /sessions name field: back to the list,
and TEXT posted as ID's name off the loop."
  (setf dialog.input-label nil
        dialog.on-input nil
        dialog.hint nil
        dialog.footer-hints :sessions)
  (when text
    (off-loop model
              (lambda ()
                (json-value (session-control-call model :post "title" :session-id id
                                                  :body (json-object "title" text))
                            :string "title"))
              (lambda (model name) (install-session-name model dialog id name)))))

(defun install-session-name (model dialog id name)
  "Land the name the title route answered for ID in the /sessions DIALOG:
its listing's entry, its directory rows and the search hits on the session."
  (if-let (failure (off-loop-failure name))
      (show-status model (format nil "Rename failed: ~a" failure) :error-p t)
      (let* ((state (gethash dialog *session-search*))
             (fold (getf state :fold))
             (entry (and fold (gethash id fold.by-id)))
             (updated (and entry (json-value entry :text "updated_at")))
             (now-ms (iso-8601-ms (nlk::iso-now))))
        (when entry
          (setf (gethash "title" entry) name))
        (if (plusp (length (getf state :rows-query)))
            ;; The search's rows: a hit's KEY is its session's name, and a
            ;; name row is its name.
            (dolist (row dialog.rows)
              (let ((value (picker-row-value row)))
                (cond ((and (consp value) (equal id (car value)))
                       (setf (picker-row-key row) name)
                       (session-picker-prefix row updated now-ms :field :key))
                      ((equal id value)
                       (setf (picker-row-key row) "" (picker-row-label row) name)
                       (session-picker-prefix row updated now-ms)))))
            (when fold
              (let ((seat (picker-highlighted-value dialog)))
                (setf dialog.rows (sessions-fold-rows fold (session-activity-table model)))
                (picker-reseat-selection dialog seat))))))
  (render-state model))

;;; Every read of the directory lands here first (READ-SESSION-DIRECTORY), so
;;; the picker a keystroke opens has rows to draw at once — the fresh read is
;;; what catches it up, and the dialog is what it catches up in. The keys are
;;; weak: a switched-away shell takes its copy with it.
(defvar *session-directory* (make-hash-table :test #'eq :weakness :key)
  "Weak memo: an attached shell -> the session directory it last read, a
plist (:entries ... :at internal-time).")

(defun cached-session-directory (model)
  "The entries of MODEL's last directory read, or NIL when this shell has
never read one."
  (getf (gethash model *session-directory*) :entries))


(defun refresh-session-directory (model)
  "Warm the shared directory off-loop for the session picker.
One read in flight per shell, preserving cached rows until the answer lands."
  (unless (eq (getf (gethash model *session-directory*) :at) :reading)
    (setf (getf (gethash model *session-directory*) :at) :reading)
    (off-loop model
              (lambda () (read-session-directory model))
              (lambda (model value)
                (when-let (failure (off-loop-failure value))
                  (setf (getf (gethash model *session-directory*) :at) nil)
                  (show-status model (format nil "Sessions failed: ~a" failure) :error-p t))
                (if (off-loop-failure value) t :quiet)))))

(defun directory-title (model session-id)
  "SESSION-ID's name in MODEL's last directory read, or NIL."
  (json-value (find session-id (cached-session-directory model)
                    :key (lambda (entry) (json-value entry :any "id")) :test #'equal)
              :text "title"))

(defun open-sessions-picker-now (model &key seat &aux (entries (cached-session-directory model)))
  "Open the /sessions picker NOW, on the keystroke, and let the read catch it
up (INSTALL-SESSION-DIRECTORY)."
  ;; The read is a gateway round trip — ~150 ms against a store holding 1600
  ;; sessions — and the dialog is a frame nothing in that answer is needed
  ;; for: the tab row, the search box and the footer are this shell's own
  ;; code, and a directory this shell already read is rows on screen at once.
  ;; So the picker paints on the keystroke: with the last listing, or with the
  ;; reading copy when there is none yet, and the answer lands in the same
  ;; dialog either way.
  (open-sessions-picker-dialog model (and entries (make-sessions-picker-fold entries))
                               :seat seat)
  (setf (getf (gethash model.dialog *session-search*) :awaiting) t)
  model)

(defun sessions-picker-awaiting-p (model &aux (dialog model.dialog))
  "The /sessions dialog on screen, opened by a keystroke and waiting for the
directory read that fills it where it stands."
  (and (picker-dialog-p dialog)
       (eq dialog.kind :sessions)
       (getf (gethash dialog *session-search*) :awaiting)
       dialog))

(defun open-sessions-picker (model value &key seat)
  "Open the sessions picker on VALUE, the directory a read answered, the
highlight on SEAT's row when it is listed."
  ;; A picker the
  ;; keystroke already opened is filled where it stands — the dialog the operator
  ;; is looking at is the one the rows land in. Any other time (nothing open, or
  ;; a dialog that is not this picker) the picker opens here, as it always has.
  ;; => the notice when the directory could not be read or drawn, else NIL.
  (or (when-let (failure (off-loop-failure value))
        (format nil "Sessions failed: ~a" failure))
      (with-slash-notice ("sessions")
          (bind ((fold (make-sessions-picker-fold value))
                 (activity (session-activity-table model))
                 (awaiting (sessions-picker-awaiting-p model)))
            (if awaiting
                (let* ((state (gethash awaiting *session-search*))
                       ;; The row the dialog held: nothing while the reading
                       ;; copy showed, the operator's own seat when the
                       ;; listing on screen was this shell's last read --
                       ;; or the session the opener named.
                       (seat (or seat (picker-highlighted-value awaiting))))
                  (setf (getf state :fold) fold
                        (getf state :awaiting) nil
                        awaiting.rows
                        (sessions-fold-rows fold activity)
                        awaiting.context
                        (sessions-dialog-context fold activity)
                        awaiting.empty-label +sessions-empty-label+)
                  ;; The counts arrive with the rows, on the tab the operator
                  ;; may already have moved to while the read was out.
                  (register-picker-tabs awaiting (sessions-folder-tabs fold)
                                        :active (picker-dialog-tab awaiting))
                  (picker-reseat-selection awaiting seat)
                  ;; The open-time highlight, live like every landing: the
                  ;; highlighted session's history read starts off the loop
                  ;; here, and the rows paint without waiting on it.
                  (picker-notify-highlight model awaiting)
                  (when model.band (render-state model)))
                (open-sessions-picker-dialog model fold :seat seat))
            nil))))

(defun install-session-directory (model value &key seat)
  "Fill the pending /sessions dialog, or open one, on the directory VALUE a
read answered, the highlight on SEAT's row when it is listed."
  ;; its answer is only ever why the directory could not be read or drawn
  (when-let (notice (open-sessions-picker model value :seat seat))
    (show-status model notice :error-p t))
  (render-state model))

(defun switch-to-fresh-gateway-session (model &optional parent)
  "Create a fresh session at the gateway — a fork of PARENT when one is named,
which the server anchors at the parent's current head — and switch MODEL to
it. => NIL; the switch owns its composer status."
  ;; A fresh session opens in this shell's folder; a fork names none and works
  ;; where its parent does (NLK:STANDBY-SESSION).
  (switch-gateway-session
   model (gethash "session_id"
                  (gateway-model-http-call
                   model :post "/api/gateway/sessions"
                   (json-object "session_id" (fresh-session-id)
                                :opt "parent" parent
                                :opt "cwd" (and (null parent) (workspace-directory)))))))

(defun slash-clear-op (model)
  "Start a fresh session and switch to it; NIL on success, refusal text on failure."
  (with-slash-notice ("clear")
    (switch-to-fresh-gateway-session model)))

(defun slash-fork-op (model)
  "Fork a new session at the current head and switch to it."
  ;; Successful feedback is transient composer status; refusals still answer.
  (with-slash-notice ("fork")
    (let ((parent model.session-id))
      (switch-to-fresh-gateway-session model parent)
      (show-status model (format nil "Forked from ~a" parent))
      nil)))

(defun slash-evict-op (model percent &optional through)
  "Force a context eviction of PERCENT of the retained history, or of every
retained turn through the turn id THROUGH."
  ;; NIL PERCENT means the operator typed bare /evict: the route stays the
  ;; single owner of the 50% default. Returns notice text.
  (with-slash-notice ("evict")
      (let ((response (session-control-call
                       model :post "evict"
                       ;; Bare /evict posts an empty object: the route
                       ;; is the single owner of the 50% default.
                       :body (json-object :opt "percent" percent :opt "through" through))))
        ;; The Zig contextEvictMessage ack, counted in words: it reports
        ;; the realized eviction, not the request.
        (format nil "Evicted ~d turn~:p (~d% of history context) · ~d message~:p retained · recall: see log"
                (gethash "evicted_turn_count" response)
                (gethash "realized_percent" response)
                (gethash "retained_message_count" response)))))

;;; --- attach-or-own entry ------------------------------------------------
;;; Distilled from the Zig TUI's foreground-gateway ladder (src/tui/root.zig)
;;; and the database lifetime lease (src/db/lease.zig): exactly one process
;;; owns the store and serves it; every UI — including the owner's own — is a
;;; WebSocket client of that gateway. A second launch does not die on the
;;; ownership lock; it reads the endpoint the owner published in the store's
;;; holder sidecar and attaches.

(defun boot-owned-organism (&key session-id store-path)
  "The ownership boot spine shared by the foreground and headless serve
entries: take the store's ownership lock, seed the session, and raise the
floor."
  ;; The folder already loaded in MAIN, so the image is the composed self by
  ;; the time the lock is taken. Returns (VALUES SESSION-ID OPERATOR-TOKEN).
  ;; Signals NLK::STORE-IN-USE, store untouched, when a live process already
  ;; owns it.
  (let ((sid (or session-id (fresh-session-id)))
        ;; The token every local first-party surface shares, minted on first use.
        (token (or (gateway-configured-operator-token)
                   (let ((token (format nil "~(~{~2,'0x~}~)" (coerce (random-bytes 32) 'list))))
                     ;; The directory closes before the file exists, so no one else can
                     ;; open the token file while it is being written.
                     (write-secret-file (home "operator-token") token)
                     token))))
    (nlk:note-boot-phase "store-token")
    (nlk:open-store :path (or store-path nlk::*default-store-path*))
    (nlk:note-boot-phase "store-opened")
    ;; The boot session is minted standing by: it becomes durable with the
    ;; first block, so a boot nobody spoke in leaves no session at all. The
    ;; floor engagement below names it as the boot that engaged, and lands
    ;; either way (an organism fact asserts nothing about the session).
    (nlk:standby-session :id sid)
    (nlk:engage-floor :session-id sid)
    (values sid token)))

;;; Spelled the way FRESH-SESSION-ID spells the rest.
(defparameter +gateway-session-id+ "s-GATEWAY"
  "The session a gateway's boot names: one per home across every restart,
so a unit that comes up at each login and after each crash leaves one
session behind, not one a start.")

;; PAUSE bounds how long a shell's close leaves the store unowned, beside the boot.
(defun await-free-store (path &key (held-p #'nlk::store-lease-held-p) (pause 1) tick)
  "Return once no live process holds PATH's ownership lock — at once when
none does."
  ;; The gateway's patience: a shell that owns the store keeps it until it
  ;; closes, and the gateway takes the organism over then instead of losing an
  ;; election it was never meant to contest. Says once on stdout, the unit's
  ;; journal, whom it waits on. => how many looks it took.
  (loop for looks from 0
        while (funcall held-p path)
        do (when (zerop looks)
             (format t "~&waiting: pid ~a owns ~a; serving once it lets go~%"
                     (or (nlk::read-store-holder path) "?") (namestring path))
             (finish-output))
           (when tick (funcall tick))
           (sleep pause)
        finally (return looks)))

(defvar *gateway-run* nil
  "T in the process `nodecode gateway run' serves: the one the home's unit
runs, and brings back on `current' from any exit.")

;;; A headless owner has no shell to quit and, run by hand, no unit to restart
;;; it: the operator's stop and restart reach it here. The owner thread parks
;;; on the semaphore, so either one is taken within the moment, and both let
;;; the store go through the same cleanup a kill runs first.
(defvar *serve-next* nil
  "What the headless owner does once it lets the store go: NIL while it
serves; :STOP to exit, :RESTART to run its own command line again.")

(defvar *serve-wake* (bt2:make-semaphore :name "serve-wake")
  "What the headless owner parks on between watchdog beats.")

(defparameter *stop-seconds* 3
  "How long a headless owner's stop may run before it is cut short
(CUT-SHORT): a shell that relaunches waits on its store.")

(defvar *stopping* nil
  "T once this headless owner was told to stop.")

(defun cut-short ()
  "End this process now: everything it started killed, then gone without an
unwind -- the kernel frees the store's lock with it."
  ;; An exit that unwinds waits on every thread it cannot stop, up to SBCL's
  ;; exit timeout: a channel's read, an MCP server's pipe. Under a SIGTERM
  ;; that lands mid-stop, before CLOSE-STORE, that wait would hold the store
  ;; while a relaunched shell dials a gateway already gone.
  #-win32 (dolist (pid (proc-descendants (sb-posix:getpid)))
            (ignore-errors (sb-posix:kill pid sb-posix:sigkill)))
  (sb-ext:exit :code 143 :abort t))

(defun serve-next (what)
  "Have the headless owner WHAT (:STOP or :RESTART) once it lets the store go;
a stop that has not let it go within *STOP-SECONDS* is cut short (CUT-SHORT)."
  (when (and (eq what :stop) *serving* (not *stopping*))
    (setf *stopping* t)
    (spawn "stop-deadline" (sleep *stop-seconds*) (cut-short)))
  (setf *serve-next* what)
  (bt2:signal-semaphore *serve-wake*))

(defun stop-on-sigterm ()
  "SIGTERM stops this headless owner as a quit does (SERVE-NEXT :STOP), and
one that lands while it already stops cuts it short."
  ;; SBCL's own handler exits from wherever the signal lands, and from inside
  ;; STOP-GATEWAY that unwind skipped CLOSE-STORE. The work goes to a thread
  ;; of its own, never the handler's.
  #-win32 (sb-sys:enable-interrupt sb-unix:sigterm
                                   (lambda-ignoring (signal info context)
                                     (spawn "serve-sigterm"
                                       (if *stopping* (cut-short) (serve-next :stop))))))

(defun serve-organism-candidate (&key session-id store-path (port *gateway-port*) wait)
  "Boot the organism as a HEADLESS gateway owner, or lose the election
quietly."
  ;; This is the `nodecode serve` entry and the process a client's
  ;; resurrection rung spawns: probe the ownership lock first so a losing
  ;; candidate skips the boot, then boot and serve until killed. Returns :LOST
  ;; when another process owns the store — never an error, because losing the
  ;; election is the expected fate of most candidates.
  ;;
  ;; WAIT is the gateway's own — `nodecode gateway run', what its unit runs:
  ;; instead of losing, wait while another process owns the store (a shell the
  ;; operator keeps open) and take the organism over the moment that process
  ;; lets go (AWAIT-FREE-STORE); a race for it lost is waited out again, so it
  ;; never answers :LOST. Its boot names +GATEWAY-SESSION-ID+.
  ;;
  ;; The authoritative election is the fcntl lock inside OPEN-STORE; the probe
  ;; is only an economy. START-GATEWAY publishes the bound port in the holder
  ;; sidecar, which is how the clients that spawned this candidate find their
  ;; way back to it.
  (setf *gateway-run* (and wait t))
  (let ((path (merge-pathnames (or store-path nlk::*default-store-path*)))
        (session-id (or session-id (and wait +gateway-session-id+))))
    ;; Notify before waiting for a shell-owned store: the service is initialized
    ;; even while it is intentionally idle, and the same owner thread must keep
    ;; the watchdog alive in that wait.
    (nlk:systemd-ready)
    (format t "~&[CP-ISSUE-20260912-001] gateway ready~%")
    (let ((heartbeat-noted nil))
      (loop
        ;; A candidate that loses did not die: the receipt BOOT-CELLS left
        ;; before its load goes with it, or the launch after reads a self that
        ;; died before serving and runs pristine -- no cell, so no cell's
        ;; command. Most visible on Windows, where every serve loads directly
        ;; and so leaves one.
        (if wait
            (await-free-store path :tick #'nlk:systemd-watchdog)
            (when (nlk::store-lease-held-p path)
              (nlk:take-self-receipt)
              (return :lost)))
        (with-handlers ((nlk::store-in-use (refusal)
                          (format t "~&election lost: ~a~%" refusal)
                          (finish-output)
                          (unless wait
                            (nlk:take-self-receipt)
                            (return :lost))))
          ;; The models.dev catalog is parsed beside the boot, not by its
          ;; first reader: the first turn's model, /models and the setup
          ;; walk's import of keys all read it. A reader that comes while it
          ;; is in flight waits on its lock (READ-JSON-FILE-MEMO). The parse
          ;; conses 46 MB and keeps a fraction.
          (spawn "models-catalog"
            (nlk:with-nursery ((* 64 1024 1024))
              (ignore-errors (models-catalog-table))))
          (multiple-value-bind (sid token)
              (boot-owned-organism :session-id session-id :store-path path)
            (with-cleanup ((ignore-errors (stop-gateway))
                           (nlk:note-phase "stop store")
                           (ignore-errors (nlk:close-store))
                           (nlk:note-phase "stop done"))
              ;; The store's open and a warm image's folder walk overlapped;
              ;; a stale image does not serve (NLK:AWAIT-WARM-VERIFICATION).
              (nlk:await-warm-verification)
              ;; The boot's pages go back behind the peripherals, not here: a
              ;; trim's collection is up to 291 ms of stop-the-world, and here
              ;; it landed on the handshake of the shell attaching.
              (let ((bound (start-gateway :port port
                                          :operator-token token
                                          :trim t)))
                (format t "~&serving store ~a on port ~d (session ~s)~%"
                        (namestring path) bound sid)
                (finish-output)
                ;; What ends this process ends it within moments: a stop's
                ;; deadline, SIGTERM as a stop, and an exit that waits at most
                ;; a second on threads it cannot stop.
                (setf *serving* t
                      sb-ext:*exit-timeout* 1)
                (stop-on-sigterm)
                (watch-front)
                ;; Park; the gateway runs on hunchentoot threads and this
                ;; process lives until killed or asked (SERVE-NEXT). The
                ;; heartbeat is deliberately on this owner thread.
                (loop
                  (nlk:systemd-watchdog)
                  (unless heartbeat-noted
                    (setf heartbeat-noted t)
                    (format t "~&[CP-ISSUE-20260912-002] gateway heartbeat~%"))
                  (when *serve-next* (return))
                  (bt2:wait-on-semaphore *serve-wake*
                                         :timeout (/ nlk:*gateway-watchdog-seconds* 3)))))))
        ;; The store is let go by now: an exec keeps every open descriptor,
        ;; and a lease still held would be inherited into a deadlock. So
        ;; would a connection a page still holds, read by nobody after.
        (case *serve-next*
          (:stop (return :stopped))
          (:restart
           (dolist (name (nlk::directory-names #+linux "/proc/self/fd" #-linux "/dev/fd"))
             (let ((fd (parse-integer name :junk-allowed t)))
               (when (and fd (> fd 2)) (ignore-errors (sb-posix:close fd)))))
           (return (format t "~&restart refused: ~a~%"
                           (nlk::exec-program (nlk:organism-binary) sb-ext:*posix-argv*)))))))))

(defun attach-or-own-organism (&key session-id store-path setup prelude)
  "The binary's entry: paint the shell NOW, boot the organism BEHIND it."
  ;; Nothing in the first frame needs a gateway — the session id is
  ;; client-chosen, the band is in-memory, the composer placeholder is the
  ;; :connecting lifecycle — so the organism starts on the organism-boot
  ;; thread while the operator already sees and types into the shell. It runs
  ;; in a process of its own (FRONT-ORGANISM), and owner and attach are one
  ;; code path: whichever process wins the fcntl lock serves, and every shell —
  ;; the one that started it included — finds the endpoint through the holder
  ;; sidecar the reconnector polls.
  (let ((path (merge-pathnames (or store-path nlk::*default-store-path*))))
    ;; The line names the session the shell ended on, not the one it launched
    ;; with: a /sessions pick, a /fork or Left moves a shell off its first id.
    (let ((ended
            (run-chat-gateway
             :session-id (or session-id (fresh-session-id)) :store-path path :setup setup
             ;; Start the organism when the store is free, else attach to the live owner.
             :boot
             ;; The boot starts from the first moment, before the program is
             ;; built; SHELL, which answers the program once it exists, it has
             ;; no use for.
             (lambda-ignoring (shell)
               ;; PRELUDE is the folder decision when the caller has put it off until
               ;; the shell could paint (a TUI launch): it loads the folder here, or
               ;; replaces this process with a published image, or returns having
               ;; done one of them. The organism this thread goes on to boot is
               ;; still under the rule — nothing serves before the folder is
               ;; loaded — only the paint moved ahead of it. It is decided once:
               ;; the takeover rung boots this again in a process whose folder is
               ;; long loaded, and decided again, it hands a shell to a fresh image
               ;; from behind its frame, which then boots as a launch of its own.
               (when prelude (funcall (shiftf prelude nil)))
               ;; A warm image that vouched for itself answers before this shell
               ;; starts an organism from it: a stale one hands the process back.
               (nlk:await-warm-verification)
               (nlk:note-boot-phase "boot-start")
               ;; A gateway starting behind this launch — at login, after a crash,
               ;; in the gap an update's restart leaves — gets its moment to take the
               ;; store first: a shell that won that race would leave the gateway
               ;; waiting behind it, the wrong way round (AWAIT-GATEWAY-STORE).
               (await-gateway-store path)
               ;; A store nobody holds gets its organism from this shell, in a
               ;; process of its own (FRONT-ORGANISM); a held one is attached to
               ;; as it is, through the sidecar the reconnector polls.
               (unless (nlk::store-lease-held-p path)
                 (front-organism path)
                 (nlk:note-boot-phase "organism-serving"))))))
      ;; A shell that never attached has no session to resume (ENDED NIL),
      ;; and one whose own boot died was ended by it (CALL-ORGANISM-BOOT): the
      ;; launch is refused on its line -- MAIN prints `nodecode: ' and it, and
      ;; exits 1. A successor still runs inside the severance MAIN gave it, so
      ;; the line is said on the process's own stdio, not into the log.
      (cond (ended (nlk::call-with-unsevered-stdio
                    (lambda ()
                      (format t "~&To resume: nodecode resume ~a~@[ --store ~a~]~%"
                              ended (and store-path (namestring path))))))
            (*organism-boot-failure* (error "~a" *organism-boot-failure*))))))
