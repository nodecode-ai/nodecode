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
     ;; The target a launch named (`nodecode --provider P --model M --effort
     ;; E', #141): plist (:SESSION-ID :PROVIDER :MODEL :EFFORT), held until
     ;; that session's first turn is admitted. Every create of the session
     ;; carries it -- the re-create after an organism lost it standing by
     ;; too -- and so does every start_turn until then, so an organism whose
     ;; create reads no target still pins it at admission
     ;; (HELD-LAUNCH-TARGET). A pick in the shell replaces it.
     (launch-target :initform nil :accessor gateway-model-launch-target)
     ;; The launch's first prompt (`-- PROMPT', `--prompt-file FILE'),
     ;; plist (:SESSION-ID :COMPOSER): it waits in the composer, shown as
     ;; typed, and goes through Enter's start leg once the session's
     ;; replay is in place (SEND-LAUNCH-PROMPT). An edit makes it the
     ;; operator's: it is then never sent by itself.
     (launch-prompt :initform nil :accessor gateway-model-launch-prompt)
     ;; The organism's refusal of the launch's target, (CODE . DETAIL), from
     ;; the session's create; said once the shell is live, and nothing is
     ;; sent by itself while it stands.
     (launch-refusal :initform nil :accessor gateway-model-launch-refusal)
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
                                                  images target)
  "Send start_turn, optionally reusing stable identifiers after reconnect."
  ;; IMAGES, when present, is a list of (MEDIA-TYPE . BASE64) staged
  ;; attachments; the codec (protocol.lisp) carries them as image items in the
  ;; wire content array, after the prompt's text item. TARGET, a plist
  ;; (:PROVIDER :MODEL :EFFORT), names what a session not yet begun runs on
  ;; (the gateway's PIN-START-TURN-TARGET); a member it lacks is not sent.
  (send-gateway-command ws :start-turn
                        (json-object "session_id" session-id
                                     "prompt" prompt
                                     "generate_response" t
                                     :opt "images" images
                                     (:getf target :opt "provider" "model" "effort"))
                        :command-id command-id :correlation-id correlation-id))

(defun held-launch-target (model)
  "The target the launch named, as a plist (:PROVIDER :MODEL :EFFORT), while
MODEL's session is the one it launched on and that session's first turn is
not yet admitted; else NIL."
  (let ((held model.launch-target))
    (and held (equal (getf held :session-id) model.session-id) held)))

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
    (cond ((string= status "accepted")
           (when-let (images (nlk::content-image-attachments (json-value response :any "images")))
             (setf model.pending-images (append model.pending-images images))))
          (t (release-held-prompt model)
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
       ;; A fact whose fold is its declaration's (tui/transcript/ingest.lisp DEFINE-FOLD) folds
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
                    st (map 'list (lambda (prompt) (json-value prompt :string "prompt_id")) pending)))
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
        ;; The declared fold settles the turn (tui/transcript/ingest.lisp INGEST-TURN-TERMINAL): a
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
      (cond ((string= status "accepted") (show-status model "Interrupt requested"))
            ;; The receipt names no exec, so a refusal clears every mark.
            (t (dolist (row (state-background-rows model.state))
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
            ;; A refused start_turn never became a turn, so the prompt comes
            ;; home: back in the composer as typed (text, pastes, images)
            ;; when the composer is empty, else in the recall ring Up reads,
            ;; where every submit already put it. A launch prompt was never
            ;; typed, so without this it would be gone.
            (let ((stash model.retract-stash))
              (when (and stash (string= command "start_turn")
                         (equal (getf stash :command-id) (or command-id model.pending-command-id))
                         (zerop (length (string-trim '(#\Space #\Tab #\Newline)
                                                     (or (editor-value model.editor) "")))))
                (replace-composer-text model (or (getf stash :text) ""))
                (setf model.pastes (getf stash :pastes)
                      model.pending-images (getf stash :images))))
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

(defun command-turn-id (frame command-id
                        &aux (metadata (json-value frame :object "event" "metadata")))
  "The turn stream FRAME admits for COMMAND-ID: the turn it names, or T when it
names none; NIL when FRAME answers no such command."
  (and command-id (equal command-id (json-value metadata :any "command_id"))
       (or (gethash "turn_id" frame) (gethash "turn_id" metadata) t)))

(defun replay-command-turn-id (payload command-id)
  (loop for frame across (json-array payload "accumulated_frames")
          thereis (command-turn-id frame command-id)))

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
  ;; The session's first turn is admitted, and it pinned the launch's target:
  ;; the session keeps it from here (PIN-START-TURN-TARGET), so it is no
  ;; longer this shell's to carry.
  (when (and (stringp turn-id) (held-launch-target model))
    (setf model.launch-target nil))
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
                          :correlation-id model.pending-correlation-id
                          ;; The resend carries the target the first send
                          ;; did: an organism that restarted under it holds
                          ;; the session again unpinned.
                          :target (held-launch-target model)))))))

(defun start-composer-turn (model text)
  "Enter's start leg: send TEXT, the composer's prompt, as this session's
next turn (start_turn), its pastes expanded and its staged images riding,
with the target a launch named while the session's first turn is not yet
admitted (HELD-LAUNCH-TARGET). The prompt row paints at once; the stash keeps
the composer as typed for a retraction or a refusal to hand back."
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
                          :images images
                          :target (held-launch-target model)))))))

(defun send-launch-prompt (model)
  "The launch's first prompt, once MODEL's session stands replayed: sent
through Enter's start leg as if typed (START-COMPOSER-TURN), carrying the
launch's target. Spent the first time it is decided, sent or not."
  ;; Not sent, and said: a target the organism refused (the prompt waits in
  ;; the composer while a pick is made), and a turn already running on a
  ;; session resumed with a prompt (Enter steers it, Tab queues behind it).
  ;; Not sent, silently: a composer the operator edited -- the prompt is his
  ;; now -- and a shell that moved to another session first.
  (when-let (refusal (shiftf model.launch-refusal nil))
    (show-status model (format nil "The launch's model was refused: ~a. Nothing ran: /models picks another~:[~;, and the prompt waits in the composer~]"
                               (string-right-trim ". " (cdr refusal)) model.launch-prompt)
                 :error-p t)
    (setf model.launch-prompt nil))
  (when-let (launch (shiftf model.launch-prompt nil))
    (let ((text (getf launch :composer)))
      (cond ((not (equal (getf launch :session-id) model.session-id)))
            ((not (equal (editor-value model.editor) text)))
            ((or model.turn-in-flight model.pending-command-id)
             (show-status model "A turn is running: the prompt waits in the composer (Enter steers it, Tab queues it)"))
            (t (start-composer-turn model text))))))

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

