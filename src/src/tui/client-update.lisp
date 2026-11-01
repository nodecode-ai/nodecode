;;;; client-update.lisp --- the update method, checkpoint prefetch, gateway resurrection. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

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
            (kind (gethash "type" payload))
            (frame-session (sync-message-session-id frame)))
       ;; Refine the span label the UPDATE :around bound: the payload kind
       ;; names which projection pass a worst span was, and the string is
       ;; already decoded — no consing on the stream path.
       (when kind (setf nlk:*attribution-label* kind))
       (cond
         ((and frame-session
               (not (string= frame-session model.session-id))
               (equal frame-session model.preview-session-id))
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
         ((or (null frame-session) (string= frame-session model.session-id))
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
                 (send-launch-prompt model)
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
                       (turn-id (json-value payload :string "turn_id"))
                       (call-id (json-value payload :text "call_id")))
                  ;; A load on this shell can fail even though the organism's
                  ;; eval succeeded. Keep that live-only failure in its call,
                  ;; without rewriting the durable tool result or splitting
                  ;; Standard's completed work trail with a diagnostic card.
                  (unless (and held (ingest-tool-load-error model.state call-id turn-id text))
                    (setf model.state
                          (ingest-system-notice
                           model.state text :turn-id turn-id
                           :retention :live-only :error-p (and held t)
                           :metadata (unless held
                                       (json-object "summary"
                                         (json-object "name" "Mutate" "path" path
                                                      "comment" (or (json-value payload :text "comment")
                                                                    (file-namestring path))
                                                      ;; The call it came from, whose line
                                                      ;; under a run says it (RUN-TAIL-LLINE).
                                                      :opt "call_id" call-id)))))))
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
            (when-let (turn-id (command-turn-id
                                (json-value (or (json-value payload :any "frame") payload) :object)
                                model.pending-command-id))
              (resolve-pending-admission model turn-id))
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
                (render-state model)))))))
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
    ;; does every other message; the arms below are all key presses. So does
    ;; the keyboard selection over the transcript (TRANSCRIPT-SELECT-KEY):
    ;; while it runs, Left moves its cursor, Enter copies and Esc unmarks,
    ;; never leaving the session, submitting or stopping the turn.
    ((or (not (nlt:key-press-msg-p msg)) model.dialog (transcript-select model))
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
            ((and last (< (since-ms last now) 500))
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
         (start-composer-turn model text))
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

(defun organism-pid ()
  "The pid of the organism serving this home, as its store's lock testifies,
or NIL when none is running."
  (multiple-value-bind (verdict pid)
      (ignore-errors (nlk::store-death-verdict (nlk::home-store-path nlk:*home*)))
    (and (eq verdict :live) pid)))

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

(defparameter +launch-target-refusals+ '("unknown_provider" "effort_unsupported")
  "The codes a session's create answers a target it cannot run on with, the
ones a start_turn refuses its target by (the gateway's TARGET-REFUSAL).")

(defun ensure-gateway-session (sid port token &key target on-refusal on-target)
  "Return SID when it exists; create exactly SID only on structured not-found."
  ;; TARGET, a plist (:PROVIDER :MODEL :EFFORT), rides the create: the
  ;; organism pins the new session alone to it, and ON-TARGET is handed the
  ;; plist it read. One it refuses is handed to ON-REFUSAL, the
  ;; GATEWAY-HTTP-ERROR saying why, and the session is created without it --
  ;; a shell needs a session to stand on, and nothing in it runs until a
  ;; target is settled. An organism whose create reads no target creates the
  ;; session unpinned; the first start_turn pins it then.
  (handler-case
      (prog1 sid
        (gateway-http-session-request port :get
                                      (format nil "/api/gateway/sessions/~a" sid)
                                      nil token))
    (gateway-http-error (lookup-error)
      (unless (and (= 404 (gateway-http-error-status lookup-error))
                   (equal "unknown_session" (gateway-http-error-code lookup-error)))
        (error lookup-error))
      (flet ((create (target)
               (gateway-http-session-request
                port :post "/api/gateway/sessions"
                ;; The shell's own folder: a gateway in another process
                ;; resolves an unstated one against its own.
                (json-object "session_id" sid
                             "title" "Lisp Gateway client"
                             "cwd" (workspace-directory)
                             (:getf target :opt "provider" "model" "effort"))
                token)))
        (let* ((refused nil)
               (created (handler-case (create target)
                          (gateway-http-error (refusal)
                            (unless (and target
                                         (member (gateway-http-error-code refusal)
                                                 +launch-target-refusals+
                                                 :test #'equal))
                              (error refusal))
                            (setf refused t)
                            (when on-refusal (funcall on-refusal refusal))
                            (create nil))))
               (read (gethash "target" created)))
          (unless (equal sid (gethash "session_id" created))
            (error 'gateway-http-error :status 200 :code "session_identity_mismatch"
                   :detail "Gateway created a different session id"))
          ;; The target as the organism read it goes to ON-TARGET, its
          ;; members apart. An organism that says none back read none: its
          ;; first start_turn takes the members word for word, so a spec only
          ;; a reader splits (`openai-codex/gpt-6.1-sol:high' with no
          ;; provider or effort of its own) would run as one model id on the
          ;; default's lane -- refused here instead, before anything runs.
          (cond ((or refused (null target)))
                ((hash-table-p read)
                 (when on-target
                   (funcall on-target (list :provider (gethash "provider" read)
                                            :model (gethash "model" read)
                                            :effort (gethash "effort" read)))))
                ((and on-refusal (target-spec-p target))
                 (funcall on-refusal
                          (make-condition 'gateway-http-error
                                          :status 200 :code "target_unread"
                                          :detail (format nil "this organism predates the PROVIDER/MODEL:EFFORT form (~a); name --provider and --effort apart, or update it"
                                                          (getf target :model))))))))
      sid)))

(defun target-spec-p (target)
  "Whether TARGET, a plist (:PROVIDER :MODEL :EFFORT), holds a model only an
organism that reads the spec splits: a `PROVIDER/' prefix with no provider
named, or a `:EFFORT' suffix that names a rung with no effort named."
  (let ((model (getf target :model)))
    (and (stringp model)
         (or (and (null (getf target :provider)) (find #\/ model))
             (and (null (getf target :effort))
                  (let ((colon (position #\: model :from-end t)))
                    (and colon (effort-rank (subseq model (1+ colon))))))))))


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

(defun still-starting-text (seconds loading log)
  "What a shell says when its own boot has not served after SECONDS: the
layer file LOADING names -- (CELL FILE . STARTED), NLK::*LAYER-FILE-LOADING*
-- while one loads, else the organism's LOG."
  ;; A layer file whose initform waits (a network call, a sleep) is what a
  ;; launch this slow is made of, and it stalls every launch until forgotten.
  (if loading
      (destructuring-bind (cell file . started) loading
        (format nil "nodecode is still starting (~d s): ~a/~a has been loading for ~d s ~
                     -- a layer file that waits stalls every launch"
                seconds cell file (round (since-ms started) 1000)))
      (format nil "nodecode is still starting (~d s): the organism has not served yet; ~
                   its log is ~a"
              seconds (home-abbreviated (uiop:native-namestring log)))))

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
      (if-let (command (serve-command store-path))
        ;; The candidate's log lands next to the database it fought for.
        (nlk::spawn-detached
         command
         (pathname (format nil "~a.resurrect.log" (namestring store-path))))
        (values nil "this image has no serve binary to exec"))))

