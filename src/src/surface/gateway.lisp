;;;; gateway.lisp --- WebSocket gateway server. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Splits the organism into two processes: a gateway that owns sessions,
;;;; turn engine, and event log; and TUI clients that connect via WebSocket.
;;;;
;;;; Threading model:
;;;;   - Hunchentoot creates one thread per accepted connection.
;;;;   - Each WS connection thread calls wsd:start-connection, blocking while
;;;;     reading frames from the peer.
;;;;   - start_turn spawns a bt:make-thread so the WS reader stays live during
;;;;     the provider call.
;;;;   - One turn runs per session (the durable active-turn authority, SUBMIT):
;;;;     a start_turn while one is in flight is queued behind it (result
;;;;     status "queued"), and a steer_turn parks for the running turn's next
;;;;     round boundary.
;;;;   - SQLite writes: WAL + SQLite's own SQLITE_THREADSAFE serialization
;;;;     handles concurrent writes from different session turn threads.
;;;;
;;;; Gateway -> Client frames (JSON, sequence-numbered per peer stream —
;;;; the durable log cursor, not the sequence, is cross-peer fact identity):
;;;;   {"sequence":N,"session_id":"...","cursor":C,"payload":{"type":"session_stream_event",...}}
;;;; A subscription's replay is session_stream_resumed: accumulated_frames,
;;;; admission_state, and the cursor on its final chunk. The route is
;;;; /gateway; every frame and command shape is in wire/protocol.schema.json.
;;;;
(in-package #:nodecode.evolved)

(nlk:access (cell nlk::cell) (command gateway-command) (frame sync-message) (newest nlk::event)
            (update exec-update))
;;; --- gateway configuration ----------------------------------------------

;;; START-GATEWAY resolves 0 to a concrete free port and returns it, and
;;; either way advertises the bound port in the store's holder sidecar — the
;;; sidecar, never a well-known number, is how clients find the endpoint. Set
;;; a fixed port only to pin the listener for an external consumer.
(defparameter *gateway-port* 0
  "TCP port the gateway binds; 0 means a kernel-assigned ephemeral port.")

;;; DEFVAR, never DEFPARAMETER: START-GATEWAY materializes this for the
;;; process lifetime, and re-loading this file under a live gateway must not
;;; reset the auth every connected client depends on back to NIL.
(defvar *gateway-operator-token* nil
  "Materialized operator token for HTTP control and WebSocket handshake auth.")

;;; Two full frames: the largest frame the transport carries always fits behind
;;; a queue still draining, and a peer that stops reading is cut off at a
;;; bound rather than at the heap. No frame count stands beside it: a count is
;;; no measure of a stalled peer. A session running a turn publishes 100-200
;;; frames a second, a delta ~150 bytes each, and a writer inside a replay (a
;;; switch delivers the destination chunk by chunk) drains nothing until the
;;; replay ends -- so a 128-frame bound dropped a healthy shell 19 KB behind,
;;; a second into every switch to a running session.
(defconstant +gateway-max-peer-queue-bytes+ (* 2 nlk:+max-sync-frame-bytes+)
  "What one peer's writer FIFO may hold unsent before the peer is judged
stalled and dropped: the one bound, in bytes, the thing the heap is made
of.")

;;; A browser cannot send a ping: its socket answers the gateway's, and a pong
;;; is what keeps a quiet tab inside *GATEWAY-CONNECTION-READ-TIMEOUT*, which
;;; this must stay well under.
(defparameter *gateway-ping-seconds* 30
  "Seconds of silence on a socket peer's stream before its writer pings it.")

(defvar *gateway-peer-states* (make-hash-table :test #'eq))

(defstruct (gw-peer-state (:copier nil))
  ;; The registry key AND the transport: a socket peer's ws, or the
  ;; in-process client handle (GW-LOOPBACK-CLIENT), which carries its own
  ;; sink and hang-up. The handle owns those, never this state: a state is
  ;; born with its writer and dies with it, and nothing remakes one.
  (socket (error "socket required") :read-only t)
  (lock (bt2:make-lock :name "gateway-peer") :read-only t)
  (ready (bt2:make-semaphore :name "gateway-peer-ready") :read-only t)
  ;; The FIFO and its last cons: the bound is bytes, so the queue may run
  ;; long, and an append must not walk it.
  (queue '() :type list)
  (queue-tail '() :type list)
  (queued-bytes 0 :type integer)
  (closed-p nil :type boolean)
  (durable-cursors (make-hash-table :test #'equal) :read-only t)
  (session-sequences (make-hash-table :test #'equal) :read-only t)
  ;; Past the connect ack: the peer decodes projections now, so the
  ;; organism's notices may reach it.
  (attached-p nil :type boolean)
  ;; The process the shell runs in, from its subscribe command, or NIL for
  ;; a client that did not say (IMAGE-LIVE-SECTIONS).
  (pid nil)
  ;; The standing board went out once, after this peer's first replay --
  ;; a replay replaces the shell's transcript state, so rows sent before
  ;; it would be wiped.
  (board-sent-p nil :type boolean))

(nlk:access (state gw-peer-state))

(defstruct (gw-replay-job (:copier nil))
  (session-id (error "session-id required") :type string :read-only t)
  (sequence 0 :type integer)
  (cursor 0 :type integer :read-only t)
  (admission-state "ready" :type string :read-only t)
  ;; The session's name when the replay was queued (SESSION-TITLE), "" for
  ;; a session with no prompt yet: session.title flows on no stream, so the
  ;; replay is how an attaching shell learns the name.
  (title "" :type string :read-only t)
  ;; A resume's: the log position the peer already holds, the replay carrying
  ;; only what lies past it (SEND-REPLAY-UNDER-LOCK); NIL for the whole.
  (after nil :type (or null integer) :read-only t))

(nlk:access (job gw-replay-job))

(defstruct (gw-loopback-client (:copier nil)
             (:constructor make-gw-loopback-client (dispatch sink hang-up &optional subscriptions)))
  "The in-process client's handle (ATTACH-LOOPBACK-GATEWAY-PEER): it is
ALSO the peer key its registry state hangs from, and it carries that peer's
transport."
  ;; SINK takes each outbound frame's JSON text, on the writer thread; HANG-UP
  ;; -- a function of no arguments, or NIL -- is called once when the writer
  ;; ends, which is the socket's close for a peer with no socket. Both belong
  ;; to the attach, so no state can lose them: a state remade without its sink
  ;; sent through WSD:SEND on this handle and died on the first frame.
  ;; DISPATCH carries the shell's commands to
  ;; DISPATCH-COMMAND (the dispatcher a socket peer's route reaches the same
  ;; way); SUBSCRIPTIONS is that connection's subscription slot, which the
  ;; dispatcher rewrites in place.
  (dispatch (error "dispatch required"))
  (sink (error "sink required"))
  (hang-up nil)
  (subscriptions nil))

(nlk:access (client gw-loopback-client) (peer gw-loopback-client) (ws gw-loopback-client))

(defvar *gateway-sessions* (make-hash-table :test #'equal)
  "Maps session-id strings to GW-SESSION structs.")

(defvar *gateway-handler* nil
  "The live Clack handler, or NIL.")

;;; With kernel-assigned ports the number a caller asked for (0) and the
;;; number the listener owns differ; this variable holds the real one.
(defvar *gateway-bound-port* nil
  "The port the live gateway actually accepted on, or NIL when stopped.")

;;; --- per-session gateway state ------------------------------------------

(defstruct (gw-session (:copier nil))
  "Per-session state for the gateway."
  ;; The session lock is also the per-peer write serialization boundary: every
  ;; publication and replay write takes it, so a slow/failing peer cannot race
  ;; a later frame for this session.
  (id (error "id required") :type string)
  (lock (bt2:make-lock :name "gw-session") :read-only t)
  (peers '() :type list)
  (turn-active-p nil :type boolean)
  ;; What the running turn streamed that no fact holds yet (STREAM-PARTIAL).
  (partial nil))

(nlk:access (gws gw-session))
(nlk:access (stored nlk::stored-fact))

;;; --- session registry helpers -------------------------------------------

(defun find-or-create-gw-session (session-id)
  "Return process state for an already durable SESSION-ID."
  (unless (and (nlk:store-open-p) (nlk:session-exists-p session-id))
    (error 'nlk:unknown-session :session-id session-id))
  (sb-ext:with-locked-hash-table (*gateway-sessions*)
    (alexandria:ensure-gethash session-id *gateway-sessions* (make-gw-session :id session-id))))

(defun remove-peer (session-ids peer)
  "Remove PEER from the subscriber list of each of SESSION-IDS the gateway holds."
  (dolist (session-id session-ids)
    (when-let (gws (sb-ext:with-locked-hash-table (*gateway-sessions*)
                     (gethash session-id *gateway-sessions*)))
      (bt2:with-lock-held ((gw-session-lock gws)) (setf gws.peers (remove peer gws.peers))))))

(defun replay-job-frame (job frames index final-p &optional tail-p)
  (nlk:encode-session-stream-resumed
   job.session-id job.sequence frames
   :cursor (and final-p job.cursor)
   :admission-state job.admission-state
   :title job.title
   :replay-chunk-index index :replay-chunk-final-p final-p
   :replay-tail-p tail-p
   :resumed-after job.after
   :octets t))

;;; The folder's peripherals start behind the first shell's attach: their
;;; processes' first seconds are what that shell's handshake and the replay
;;; that paints its transcript would otherwise share. The shell runs in a
;;; process of its own, so the gateway says when it has handed one its
;;; transcript.
(defvar *replay-sent-semaphore* (bt2:make-semaphore :name "replay-sent")
  "Signalled each time a subscriber has been sent the last chunk of a
session's replay (SEND-REPLAY-CHUNK); a fresh one for each gateway's start
(START-CELLS-BEHIND-THE-LISTENER), so no replay an earlier one sent counts.")

(defun send-replay-chunk (state job frames index final-p &optional tail-p)
  "Send FRAMES as one bounded chunk, bisecting oversized chunks."
  ;; Returns the next chunk index. A chunk of one fact goes whole even past
  ;; the bound: the fact was admitted under the same bound and outgrew it only
  ;; by its envelope, the socket itself reads to 64 MiB, and refusing it would
  ;; leave the session unreadable to every shell that ever attaches.
  (let ((json (replay-job-frame job (coerce frames 'vector) index final-p tail-p)))
    (cond
      ((or (<= (length json) nlk:+max-sync-frame-bytes+)
           (<= (length frames) 1))
       (gw-peer-send-text state json)
       (when final-p (bt2:signal-semaphore *replay-sent-semaphore*))
       (1+ index))
      (t
       (let* ((half (floor (length frames) 2))
              (next (send-replay-chunk state job (subseq frames 0 half)
                                       index nil tail-p)))
         (send-replay-chunk state job (subseq frames half) next final-p tail-p))))))

;;; --- the first pass -----------------------------------------------------------
;;; A replay is the whole log, folded by the shell in order and painted at its
;;; last chunk. Without a first pass, a long session's first paint would wait on
;;; all of it. A long session's replay runs twice. The first pass carries its
;;; newest turns, from the prompt of the turn that holds the *REPLAY-TAIL-FACTS*th
;;; newest fact, behind the facts the shell sums across the whole session; the
;;; shell paints it. The second is the whole log, which the shell folds out of
;;; sight and puts in place at its last chunk. Both end at the job's cursor with
;;; its admission state, so the first paint already says whether a turn runs.

(defparameter *replay-tail-facts* 256
  "Roughly how many newest facts a long session's first pass carries: it
starts at the prompt of the turn holding the fact this far back.")

(defparameter *replay-tail-minimum* 1024
  "The facts of its own a session's log holds before its replay runs a
first pass: a shorter log replays whole about as fast.")

(defparameter +replay-seed-kinds+ (list nlk::+kind-turn-usage+)
  "The kinds a first pass carries from before its turns: the facts the
shell's fold sums across the whole session (ui.lisp INGEST-TURN-USAGE),
which its header reads beside the title the session's first prompt names
(SESSION-DISPLAY-TITLE), the one other fact it carries.")

(defun replay-tail-start (session-id cursor)
  "The log position a first pass of SESSION-ID's replay up to CURSOR
starts at, or NIL when it runs none: a log of fewer than
*REPLAY-TAIL-MINIMUM* facts, a turn that opens in the log's older half."
  (let ((newest (nlk:events :session-id session-id :before (1+ cursor) :order :newest
                            :limit *replay-tail-minimum* :columns '("log_position")
                            :as :rows)))
    (when (= (length newest) *replay-tail-minimum*)
      (when-let (start (nlk:events :session-id session-id :kind nlk::+kind-input-committed+
                                   :before (1+ (first (nth (1- *replay-tail-facts*) newest)))
                                   :order :newest :columns '("log_position") :as :value))
        (and (< (* 2 (nlk:events :session-id session-id :after (1- start) :before (1+ cursor)
                                 :as :count))
                (nlk:events :session-id session-id :before (1+ cursor) :as :count))
             start)))))

(defun send-replay-pass (state job visible-set &optional tail &aux (index 0))
  "Send JOB's replay as one pass of chunks (WALK-REPLAY), the last one final."
  (walk-replay job.session-id job.cursor visible-set
               (lambda (frames final-p)
                 (setf index (send-replay-chunk state job frames index final-p (and tail t))))
               tail job.after))

(defun session-frames (session-id &aux (cursor (session-cursor session-id)) (all '()))
  "Every frame a shell's replay of SESSION-ID folds, oldest first: the whole
pass WALK-REPLAY reads up to the log's newest fact."
  ;; The Markdown export reads the session through here, so it shows what a
  ;; shell attached to it shows: its fork's parent turns, never a turn the
  ;; head left.
  (when (plusp cursor)
    (walk-replay session-id cursor (nth-value 0 (nlk:session-visible-turn-set session-id))
                 (lambda-ignoring (frames final-p)
                   (setf all (revappend frames all)))))
  (nreverse all))

(defun walk-replay (session-id cursor visible-set send &optional tail resume)
  "Call SEND with (FRAMES FINAL-P) for SESSION-ID's replay up to CURSOR, the
last call final: each fork ancestor up to its anchor, then the session's own
facts; with TAIL, a REPLAY-TAIL-START position, the first pass — the first
prompt the session's head carries and every fact of +REPLAY-SEED-KINDS+
before TAIL, then the session's own from TAIL; with RESUME, a log position,
the session's own facts past it alone."
  (let ((after (cond (resume) (tail (1- tail)) (t 0)))
        (opening t))
    (labels ((visible (facts) (visible-event-frames session-id facts visible-set))
             (opening-prompt (id before)
               ;; The first the head carries: a retracted turn's is not the title.
               (loop for past = 0 then (nlk::stored-fact-log-position (car (last facts)))
                     for facts = (nlk:events :session-id id :kind nlk::+kind-input-committed+
                                             :after past :before before :limit 16 :as :facts)
                     while facts
                     do (when-let (frames (visible facts))
                          (return (list (first frames))))))
             (older (id before)
               ;; A segment whole, or, in a first pass, the facts it seeds:
               ;; every one of them follows the opening prompt, as its turns do.
               (if tail
                   (append (and opening (opening-prompt id before))
                           (visible (nlk:events :session-id id :kind +replay-seed-kinds+
                                                :before before :as :facts)))
                   (visible (nlk:events :session-id id :before before :as :facts)))))
      (unless resume
        (dolist (segment (nlk:session-lineage session-id))
          (when-let (frames (older (first segment) (1+ (third segment))))
            (funcall send frames nil))
          (setf opening nil)))
      (when-let (frames (and tail (older session-id tail)))
        (funcall send frames nil))
      ;; A fork standing by has no fact of its own: its replay ends on its
      ;; lineage, so a shell opens on the turns it carries. A resume at the
      ;; log's end has nothing to carry.
      (when (= after cursor)
        (funcall send '() t)
        (return-from walk-replay))
      ;; A replay snapshot pages; its cursor runs on unfiltered rows.
      (loop
        for facts = (remove-if (lambda (stored) (> stored.log-position cursor))
                               (nlk:page-events :session-id session-id
                                                :after after :limit 128
                                                :as :facts))
        do (unless facts
             (error "replay snapshot cursor ~d disappeared" cursor))
           (setf after (nlk::stored-fact-log-position (car (last facts))))
           (let ((final-p (= after cursor))
                 (frames (visible facts)))
             (when (or frames final-p)
               (funcall send frames final-p))
             (when final-p (return)))))))

(defun release-gw-peer-state (state)
  "Forget STATE's registry entry, but only while its socket still maps to it."
  ;; The eq guard keeps a stale release from evicting a successor state for
  ;; the same socket object.
  (sb-ext:with-locked-hash-table (*gateway-peer-states*)
    (when (eq state (gethash state.socket *gateway-peer-states*))
      (remhash state.socket *gateway-peer-states*))))

(defun tombstone-peer-queue (state &optional notice)
  "Close STATE and drop everything queued on it, keeping NOTICE -- the
diagnostic it was dropped with -- as the one frame left to send."
  ;; The caller holds STATE's lock. The one shape of a tombstone, so the
  ;; orderly stop, the writer's own failure and the queue-bound eviction cannot
  ;; disagree about what a dropped peer's FIFO holds afterwards.
  (setf state.closed-p t
        state.queue (and notice (list notice))
        state.queue-tail state.queue
        state.queued-bytes (if notice (length notice) 0)))

(defun peer-writer-loop (state)
  "Drain one peer FIFO; this is the only server-side socket writer."
  ;; The writer owns its registry entry's lifetime: however the loop ends —
  ;; orderly stop, write failure, or anything unwinding past it — the entry is
  ;; released and the connection hung up, so a tombstoned peer cannot pin its
  ;; socket in the registry and its client learns the stream is gone instead
  ;; of waiting on a link the gateway no longer serves. A close frame and a
  ;; closed socket are what put the client's own reconnect ladder (and the
  ;; replay it earns) in motion; an in-process peer — the image's own attached
  ;; shell, ATTACH-LOOPBACK-GATEWAY-PEER — has no socket, so its handle
  ;; carries the hang-up its attach gave it, and that is the same contract.
  ;; Left with its dead link instead, the shell stayed `connected' and painted
  ;; nothing while its turn ran on, until the process was restarted.
  (with-cleanup ((release-gw-peer-state state)
                 (let ((peer state.socket))
                   (ignore-errors
                    (if (gw-loopback-client-p peer)
                        (when-let (hang-up peer.hang-up) (funcall hang-up))
                        (wsd:close-connection peer)))))
    (loop
      (loop until (bt2:wait-on-semaphore state.ready :timeout *gateway-ping-seconds*)
            unless (gw-loopback-client-p state.socket)
              ;; A dead socket fails here and again at its reader, whose close stops the peer.
              do (ignore-errors (wsd:send-ping state.socket)))
      (let ((item nil))
        (bt2:with-lock-held ((gw-peer-state-lock state))
          (cond
            ((gw-peer-state-queue state)
             (setf item (pop state.queue))
             (unless state.queue
               (setf state.queue-tail '()))
             (decf state.queued-bytes
                   (if (stringp item) (length item) 1))
             ;; A dropped peer's queue holds exactly one last frame: the
             ;; diagnostic it was dropped with. Re-arm the semaphore so the
             ;; next wait returns at once and the loop ends, rather than
             ;; parking on a signal nobody owes it — which would pin the
             ;; connection open and its registry entry with it.
             (when (and state.closed-p (null state.queue)) (bt2:signal-semaphore state.ready)))
            ((gw-peer-state-closed-p state) (return))))
        (when item
          (with-handlers ((error (condition)
                            ;; Say WHY before tombstoning. A poisoned replay (a corrupt
                            ;; row refused by the read-side hash checks, a vanished
                            ;; snapshot cursor) leaves the socket healthy, and without
                            ;; this frame the client only sees its stream die — an
                            ;; undiagnosable resubscribe loop. Best-effort by design:
                            ;; when the failure IS the socket, the send fails again and
                            ;; there is nobody left to tell.
                            (ignore-errors
                             (gw-peer-send-text
                              state
                              (nlk::encode-sync-protocol-error
                               "peer_writer_failed"
                               (format nil "peer writer failed: ~a" condition)
                               :permanent t)))
                            (bt2:with-lock-held (state.lock) (tombstone-peer-queue state))
                            ;; Exit now rather than re-parking on the semaphore: the
                            ;; queue is gone and nothing is obliged to signal again.
                            (return)))
            (cond
              ;; Work that must write in this peer's order, done when its
              ;; turn comes (rows.lisp FLUSH-ROWS-VIEW).
              ((functionp item) (funcall item state))
              ((not (gw-replay-job-p item))
               (gw-peer-send-text state item))
              ;; A session standing by with no fork behind it has nothing to say.
              ((and (zerop (gw-replay-job-cursor item))
                    (null (nlk:session-lineage (gw-replay-job-session-id item))))
               (gw-peer-send-text state (replay-job-frame item #() 0 t)))
              (t
               (let* ((session-id (gw-replay-job-session-id item))
                      (visible-set (nth-value 0 (nlk:session-visible-turn-set session-id))))
                 (when-let (tail (and (null (gw-replay-job-after item))
                                      (replay-tail-start session-id (gw-replay-job-cursor item))))
                   (send-replay-pass state item visible-set tail))
                 (send-replay-pass state item visible-set)
                 (when (plusp (gw-replay-job-cursor item))
                   ;; Fold the history into the hydration cache behind the replay a shell
                   ;; is reading: the first prompt sent there then folds only what landed
                   ;; since, not the whole log in front of its request. A warm that fails
                   ;; is only a cold first prompt, which folds and says why.
                   (spawn "history-warm"
                     (ignore-errors
                      (history-turns-for-session (intern-session session-id))))))))))))))

(defun find-gw-peer-state (peer)
  "PEER's registry state, or NIL once its writer has ended: a peer the
gateway hung up is never remade behind its client's back."
  (sb-ext:with-locked-hash-table (*gateway-peer-states*)
    (gethash peer *gateway-peer-states*)))

(defun ensure-gw-peer-state (peer)
  "The registry state for PEER, made at its connection's birth -- the
socket's open, the in-process attach -- with the writer that owns it."
  ;; Every later touch finds (FIND-GW-PEER-STATE) and never remakes: a state
  ;; remade after its writer ended would run a second writer for a link the
  ;; first one hung up. The in-process shell measured exactly that: its
  ;; subscribe after an eviction remade its state without its sink, and the
  ;; new writer died on the first frame.
  (sb-ext:with-locked-hash-table (*gateway-peer-states*)
    (or (gethash peer *gateway-peer-states*)
        (let ((state (make-gw-peer-state :socket peer)))
          (setf (gethash peer *gateway-peer-states*) state)
          (spawn "gateway-peer-writer" (peer-writer-loop state))
          state))))

(defun stop-gw-peer (peer &aux (state (find-gw-peer-state peer)))
  (when state
    (bt2:with-lock-held ((gw-peer-state-lock state))
      (tombstone-peer-queue state))
    (bt2:signal-semaphore state.ready)
    ;; The writer thread releases the registry entry as it exits; releasing
    ;; here as well (eq-guarded, idempotent) covers states that never had a
    ;; writer, so a stop is always a full reclamation.
    (release-gw-peer-state state))
  state)

(defun gw-peer-send-text (state text &aux (peer state.socket))
  "One frame to STATE's peer — TEXT, JSON as a string or as its UTF-8 octets
(a replay chunk) — through its socket as a text message, or to the sink an
attached in-process shell's handle carries."
  (if (gw-loopback-client-p peer)
      (funcall peer.sink text)
      (wsd:send peer text :type :text)))

;;; --- the owner's own shell, attached without a socket ----------------------
;;; The shell that boots this gateway runs in this very image. Without this,
;;; it would dial its own listener over loopback to reach it: a socket, a
;;; client driver thread, a challenge round trip and an HTTP session ensure,
;;; all to talk to itself. The frames, the per-peer sequencing, the queue
;;; bounds and the backpressure are the socket path's, unchanged; only the
;;; transport is a call.

(defun attach-loopback-gateway-peer (sink &key hang-up)
  "Attach the shell that boots this image's gateway to it in-process: one
peer, the same frames and sequencing as a socket peer, with no socket, no
challenge and no HTTP session ensure between an image and itself — the
caller IS this image, so operator authority is a fact rather than a token
round trip."
  ;; SINK is called with each outbound frame's JSON text, on the peer's writer
  ;; thread. HANG-UP, when given, is called once when the gateway ends this
  ;; peer's writer -- dropped at the queue bound, a replay that failed --
  ;; exactly when a socket peer would see its connection close: the caller
  ;; drops its link and attaches again. => the client handle to pass as the
  ;; client's `ws' (GATEWAY-LINK-SEND, GATEWAY-LINK-CLOSE), or NIL when this
  ;; image serves no gateway.
  (unless (and *gateway-handler* *gateway-bound-port*)
    (return-from attach-loopback-gateway-peer nil))
  (let* ((subscriptions (list '()))
         (scopes (copy-list nlk::+gateway-default-scopes+))
         (client nil)
         (dispatch (lambda (message)
                     (dispatch-command client message subscriptions scopes))))
    ;; CLIENT is bound once and set after the closure that reads it. A second
    ;; CLIENT binding in this LET* shadows the first, and the dispatch lambda —
    ;; closed over the first, NIL — then hands DISPATCH-COMMAND NIL for every
    ;; command the boot shell sends: its subscribe registers NIL as the
    ;; subscriber, frames publish to a peer that cannot receive them, and the
    ;; window sits on `waiting' with an empty transcript while the turn runs.
    (setf client (make-gw-loopback-client dispatch sink hang-up subscriptions))
    (ensure-gw-peer-state client)
    (mark-peer-attached client)
    client))

(defun gateway-link-send (ws text)
  "One JSON text to the gateway: through the socket, or — for the shell that
owns this image's gateway — straight into its in-process dispatch."
  ;; A link the gateway hung up carries nothing, as a closed socket does.
  (if (gw-loopback-client-p ws)
      (when (find-gw-peer-state ws)
        (funcall ws.dispatch text))
      (wsd:send ws text)))

(defun gateway-link-close (ws)
  "Hang up WS: close the socket, or detach the in-process peer and forget
its subscriptions. NIL is quiet."
  (cond
    ((null ws) nil)
    ((gw-loopback-client-p ws) (stop-gw-peer ws) (remove-peer (car ws.subscriptions) ws))
    (t (ignore-errors (wsd:close-connection ws)))))

(defun enqueue-peer-frame (peer item &key session-id cursor cursor-present-p encoder &aux accepted)
  "Enqueue one bounded writer item, sequencing session frames per peer."
  ;;
  ;; ITEM is a pre-encoded string for unsequenced control frames, a replay job,
  ;; or a function of the writer's peer state, called when its turn comes.
  ;; Session-stream frames pass ENCODER instead: a function of this peer's next
  ;; per-session sequence, called only after the duplicate check so a suppressed
  ;; frame burns no sequence and the peer's stream stays dense. Live durable
  ;; frames at or below the peer's replayed cursor are suppressed; a replay job
  ;; always transmits (a gap-recovery resubscribe must not be swallowed by its
  ;; own watermark) and re-arms the watermark it carries. => whether the peer
  ;; took it. A peer with no registry state -- its writer ended and hung it up
  ;; -- takes nothing, and is never remade here.
  ;;
  ;; A peer past the queue bound is dropped for good, out loud: it keeps
  ;; exactly one last frame -- a permanent sync_protocol_error naming why --
  ;; and the writer sends it and then hangs the connection up, which the
  ;; client reads as its stream dying, walking its own reconnect ladder and
  ;; earning the replay that rebuilds what its fold missed. Without this final
  ;; frame, a silent tombstone would leave a shell painting a dead stream for
  ;; minutes after its turn completed, its bar still counting and its band row
  ;; still reading "preparing". A state already tombstoned (a writer failure,
  ;; an orderly stop) is left alone: never clear a queue its writer still owes
  ;; the wire.
  (when-let (state (find-gw-peer-state peer))
    (bt2:with-lock-held ((gw-peer-state-lock state))
      (let ((replay-p (gw-replay-job-p item)))
        (when (and (not replay-p) session-id cursor-present-p
                   (<= cursor (gethash session-id state.durable-cursors -1)))
          (return-from enqueue-peer-frame t))
        (unless state.closed-p
          (when (or replay-p encoder)
            ;; The peer's first frame seeds from the durable allocator; later ones increment.
            (let ((sequence (setf (gethash session-id state.session-sequences)
                                  (if-let (last (gethash session-id state.session-sequences))
                                    (1+ last)
                                    (nlk::allocate-gateway-sequence session-id)))))
              (if replay-p
                  (setf (gw-replay-job-sequence item) sequence)
                  (setf item (funcall encoder sequence)))))
          (let ((size (if (stringp item) (length item) 1)))
            (cond
              ((<= (+ state.queued-bytes size) +gateway-max-peer-queue-bytes+)
               (let ((link (list item)))
                 (if state.queue-tail (setf (cdr state.queue-tail) link) (setf state.queue link))
                 (setf state.queue-tail link))
               (incf state.queued-bytes size)
               (when (and session-id cursor-present-p)
                 (setf (gethash session-id state.durable-cursors)
                       cursor))
               (setf accepted t))
              (t (tombstone-peer-queue
                  state (nlk::encode-sync-protocol-error
                         "peer_evicted"
                         "peer fell behind its stream; the gateway dropped this subscriber"
                         :permanent t))))))))
    (bt2:signal-semaphore state.ready)
    accepted))

(defun send-frame-under-lock (gws peer kind payload &key cursor durable-p)
  "Enqueue one frame for PEER while GWS is locked, evicting failed peers."
  ;; The frame is encoded per target because each peer owns its stream
  ;; sequence.
  (let ((sent (enqueue-peer-frame
               peer nil
               :session-id gws.id
               :cursor (and durable-p cursor)
               :cursor-present-p (and durable-p (not (null cursor)))
               :encoder (lambda (sequence)
                          (nlk:encode-projection-event
                           gws.id sequence kind payload
                           :cursor cursor :durable-p durable-p)))))
    (unless sent
      (setf gws.peers (remove peer gws.peers)))
    sent))

(defun publish-event (session-id kind payload &key cursor durable-p)
  "Publish one frame to every subscribed peer, sequenced per peer at enqueue."
  ;; Live-only frames touch no durable state; each peer's stream is dense by
  ;; construction because its sequence is allocated after suppression.
  ;;
  ;; This is the :FRAME hook point: every live frame of every session passes
  ;; through (NLE:HOOK :FRAME KEY FN) chains here, on the publishing thread
  ;; and before the session lock, with OP (:session-id :kind :payload :cursor
  ;; :durable-p). An in-process observer (a channel, a metrics sink, a test
  ;; capture) hooks here instead of dialling the gateway back over loopback.
  ;; Isolated: a hook's own error is said and stepped over (NLK:CALL-ISOLATED);
  ;; an error out of the fan-out below is this publish's.
  ;;
  ;; The running-session map and the desktop notice read the frame once it
  ;; is out (NOTE-SESSION-ACTIVITY-FRAME in activity.lisp, NOTIFY-TURN-END in
  ;; notify.lisp): after the chain, so their frames are siblings of this one
  ;; at the point and never nest inside its hooks.
  (let* ((op (list :session-id session-id :kind kind :payload payload
                   :cursor cursor :durable-p durable-p))
         (delivered
           (call-hooked
            :frame op
            ;; The innermost continuation: fan OP out to the session's peers,
            ;; and into what the round in flight streamed (NOTE-STREAM-FRAME).
            (lambda (op)
              (destructuring-bind (&key session-id kind payload cursor durable-p) op
                (let ((gws (find-or-create-gw-session session-id)))
                  (bt2:with-lock-held ((gw-session-lock gws))
                    (note-stream-frame gws kind payload (and durable-p cursor))
                    (plusp (loop for target in (copy-list (gw-session-peers gws))
                                 count (send-frame-under-lock gws target kind payload
                                                              :cursor cursor :durable-p durable-p)))))))
            :isolate t)))
    ;; The activity map folds it into which sessions are running, and a
    ;; turn's end pops the notice when a shell received it. Neither nests
    ;; inside the other's hooks.
    (note-session-activity-frame op)
    (notify-turn-end op delivered)
    delivered))

;;; --- the organism's own voice ---------------------------------------------
;;; A peripheral that refuses its config, a channel that connects or dies, a
;;; boot that could not read the shared config: facts about the organism
;;; itself, with no session to ride. Without NOTICE, each would be a WARN --
;;; one line in the severed stdio log while a shell owns the terminal,
;;; invisible to the operator and to the model alike. NOTICE is the one
;;; generic way a part says something: every attached shell puts it up as a
;;; toast, and with a KEY it also stands on the
;;; board (cells.lisp) that late-attaching shells receive and the cells
;;; harness section shows the model each round.

(defun attached-shells (&aux (sessions (sb-ext:with-locked-hash-table (*gateway-sessions*)
                                         (loop for gws being the hash-values of *gateway-sessions*
                                               when gws.peers
                                                 collect (cons gws.id
                                                               (copy-list gws.peers))))))
  "Every session a shell is subscribed to, with the pids of the shells on
it: ((SESSION-ID PID-OR-NIL ...) ...), sessions in id order."
  (sort (loop for (id . peers) in sessions
              collect (cons id
                            (loop for peer in peers
                                  for state = (find-gw-peer-state peer)
                                  collect (and state state.pid))))
        #'string< :key #'first))

(defparameter *image-help-summary*
  "this process: its store and home, and the operator's shells, which may run in another process"
  "The line the help section carries for (help :image) while the gateway
serves. Mutable material, like the system prompt.")

(defun image-help-text ()
  "(help :image): what this process owns and serves, that the operator's
shells attach to it and may each run in another process, where to read which
do — and the rule that follows: a definition evaluated here is sent to every
attached shell in another process, which evaluates it from the same file
(SOURCE-LOADED), so the operator's shell on this machine changes with this
process, and a shell elsewhere does not."
  ;; A topic while the gateway serves (SERVE-GATEWAY sets it, STOP-GATEWAY
  ;; takes it off); its line rides every request's help section, so nothing
  ;; that moves stands in it: the pid, the port and the attached-shells list
  ;; would move at every relaunch and every shell attaching or leaving. What
  ;; moves is read when it is wanted, (attached-shells) and (sb-posix:getpid).
  ;; Without the live pids, a shell in another process running the shipped
  ;; binary would not be told apart from the process where a definition was
  ;; evaluated.
  (format nil "This image ~:[serves the gateway~;~:*owns the store ~a and serves its gateway~]. Its home is ~a (profile ~a): config.jsonc, secrets/, and under cells/ the layer and the knowledge cell of memories and skills live there; another profile is another home and another organism, run with `nodecode -p NAME'. The operator's shells attach to this gateway, and each may run in this process or in another: (attached-shells) answers every session a shell is on with the pids of its shells, and (sb-posix:getpid) is this process. A definition evaluated here — a snippet's, kept in the layer, or an edit of a file in this image's trees made live — is sent to every attached shell in another process, and each evaluates the same definitions from the same file into its own image, so the operator's shell on this machine changes with this process; the receipt counts the shells told. A structure or a class is held in both (load-source names it instead), and a shell elsewhere runs its own image from the binary it was launched with: a change to shipped source reaches it through a release rebuild and a relaunch."
          (and (nlk:store-open-p) (namestring (nlk:store-path nlk::*store*)))
          (string-right-trim "/" (namestring (nlk:home))) (nlk:profile-name)))

(defun mark-peer-attached (peer &aux (state (find-gw-peer-state peer)))
  (when state (bt2:with-lock-held ((gw-peer-state-lock state)) (setf state.attached-p t)))
  state)

(defun notice-payload (text level key)
  "The organism.notice wire payload: TEXT at LEVEL, keyed when KEY names a
standing notice."
  (json-object "text" text
               "level" (string-downcase (symbol-name level))
               :opt "key" key))

(defun standing-notices-payload ()
  "The complete typed standing board, including quiet entries; live-only."
  (json-object "notices"
               (map 'vector (lambda (notice)
                               (destructuring-bind (key text level) notice
                                 (notice-payload text level key)))
                    (nlk:standing-notices))))

(defvar *standing-notice-publication-lock* (bt2:make-lock :name "standing-notice-publication")
  "Order board writes and whole-board publications, without keeping another board.")

(defun fanout-frame (encoded)
  "ENCODED to every peer past its connect ack."
  ;; => how many peers took it.
  (loop for peer in (sb-ext:with-locked-hash-table (*gateway-peer-states*)
                      (loop for peer being the hash-keys of *gateway-peer-states*
                              using (hash-value state)
                            when (gw-peer-state-attached-p state)
                              collect peer))
        count (enqueue-peer-frame peer encoded)))

;;; Which kinds BROADCAST-ORGANISM carries is NLK:+UNSEQUENCED-FRAME-KINDS+
;;; (wire/protocol.lisp) and nothing here: the decoder at the far end admits
;;; that same list, and a second copy beside the producer is how a kind gets
;;; published that no shell will take.

(defun broadcast-organism (kind payload &key session-id)
  "One session-less unsequenced frame of KIND carrying PAYLOAD to every
attached peer, through the :FRAME point like every live frame."
  ;; => (values SENT-P COUNT): whether any peer took it, and how many did —
  ;; FANOUT-FRAME's count, folded back out of the hook chain, whose
  ;; answer a hook may have replaced with something else entirely. SESSION-ID
  ;; rides on the OP an observer
  ;; sees; the envelope stays session-less either way, so the frame reaches
  ;; every shell whichever session it shows.
  (unless (member kind nlk:+unsequenced-frame-kinds+ :test #'string=)
    (error "no unsequenced frame kind ~s" kind))
  (let ((told (call-hooked :frame
                           (list :session-id session-id :kind kind :payload payload
                                 :cursor nil :durable-p nil)
                           (lambda (op)
                             (fanout-frame
                              (nlk:encode-unsequenced-frame kind (getf op :payload))))
                           :isolate t)))
    (if (integerp told)
        (values (plusp told) told)
        (values told 0))))

(defun announce-loaded (path definitions)
  "NLK:SOURCE-LOADED while this gateway serves — START-GATEWAY advises the
seam with this: every attached shell is sent one
organism.loaded frame naming PATH and DEFINITIONS ((OPERATOR . LABEL)
...), and evaluates them from its own copy of the file (client.lisp's
organism.loaded arm, NLK:LOAD-SOURCE :ONLY)."
  ;; Only the names cross the wire. Rides the :FRAME point like a notice,
  ;; session-less, on the calling thread: the image is one, so the frame
  ;; reaches every shell whichever session it shows -- but the payload names
  ;; the session whose turn made the definitions live (EVAL-SESSION-ID, the
  ;; turn bound on this thread; absent for a load outside any turn), and a
  ;; shell draws its system row in that session alone. => how many shells
  ;; were told, what the receipt counts. The fold itself refuses a file it
  ;; does not have.
  (nth-value 1
             (broadcast-organism
              "organism.loaded"
              (json-object "path" (namestring path)
                           "definitions" (map 'vector
                                              (lambda (pair)
                                                (json-object "operator" (car pair)
                                                             "label" (cdr pair)))
                                              definitions)
                           :opt "session_id" (eval-session-id)
                           :opt "turn_id" (and *current-durable-turn*
                                               (nlk:durable-turn-turn-id *current-durable-turn*))
                           :opt "call_id" *executing-tool-call-id*
                           :opt "comment" *snippet-note*))))

(defun gateway-endpoint ()
  "Where this organism's own gateway answers and the token it takes: => (values
URL TOKEN), URL as http://127.0.0.1:PORT, or NIL while nothing listens."
  ;; For a cell that speaks to its own gateway as a client does -- over
  ;; loopback with the Bearer every route and socket takes -- rather than
  ;; into the gateway's internals.
  (when *gateway-bound-port*
    (values (format nil "http://127.0.0.1:~d" *gateway-bound-port*) *gateway-operator-token*)))

(defun keep-running (what)
  "WHAT — \"the Discord bot\", \"2 cron jobs\" — is work that should go on
while no shell is open."
  ;; The first time a cell says so, the background gateway comes on
  ;; (NLK:GATEWAY-ON :AUTO) and every shell is told: the organism a shell runs
  ;; now passes to the gateway when that shell closes, and comes up again at
  ;; every login. Nothing happens once the gateway is on, once the operator
  ;; turned it off, in an ephemeral organism, or on a machine with no
  ;; background gateway. A cell's call — the channel kit's when a bot
  ;; starts, cron's when a job is scheduled — and it answers at once: the
  ;; operating system is asked on a thread of its own, never on a cell's
  ;; start. => the thread, or NIL.
  (unless (or nlk:*ephemeral*
              (null (nlk:gateway-unit-path))
              (nlk:gateway-installed-p)
              (nlk:gateway-off-p))
    (spawn "gateway-on"
      (handler-case
          (when (nlk:gateway-state-p (nlk:gateway-on :auto t))
            (notice (format nil "nodecode's gateway keeps ~a running after this shell closes, ~
                                  and comes up at every login · /settings turns it off"
                            what)))
        (error (refusal)
          (ignore-errors
           (notice (format nil "gateway: ~a cannot keep running after this shell closes — ~a"
                           what refusal)
                   :level :warning)))))))

(defun notice (text &key (level :info) key)
  "Say TEXT to the operator: every attached shell puts it up as a toast
(LEVEL :info or :warning for five seconds, :error until it is closed).
A :quiet notice is said to no one and only stands under its KEY."
  ;; With KEY, the notice also STANDS: it replaces the board's entry under
  ;; KEY, so a shell attaching later still sees it and the model reads it in
  ;; the notices block at the tail of every round's prompt until a later
  ;; notice under KEY replaces it or a NIL TEXT clears it. Without a key it is
  ;; said once and gone -- and either way it goes into the log, so a reader
  ;; that was not there can hear it again (cells.lisp NOTICE-LOG).
  ;;
  ;; :QUIET is a state that is normal, not news: the board's readers -- the
  ;; model's notices block, /cells, (cells), the web page's cell list --
  ;; read it, and no shell draws it, now or when it attaches. A channel
  ;; adapter installed with no section said so as a card on every launch;
  ;; its lane refusing to start is still an error, and said.
  ;;
  ;; Rides the :FRAME point like every live frame (OP :kind "organism.notice",
  ;; :session-id NIL), on the calling thread, so an observer sees it; a
  ;; hook's own error is stepped over there. A text no attached shell took,
  ;; in a process with no shell of its own, goes to *ERROR-OUTPUT* -- the
  ;; headless serve log -- so nothing is ever said to nobody. Returns TEXT.
  (check-type key (or null string))
  (unless (member level (if key '(:quiet :info :warning :error) '(:info :warning :error)))
    (error "notice level is :info, :warning or :error, or :quiet under a key, not ~s" level))
  (when key
    ;; Every standing change has a board frame, even a quiet update or clear.
    ;; Spoken notices below retain their independent toast/log contract.
    (bt2:with-lock-held (*standing-notice-publication-lock*)
      (let ((before (nlk:standing-notices)))
        (nlk:note-standing key text level)
        (unless (equal before (nlk:standing-notices))
          (broadcast-organism "organism.notices" (standing-notices-payload))))))
  (when (and (stringp text) (plusp (length text)) (not (eq level :quiet)))
    ;; Every notice, keyed or not, is logged as it is said: the board answers
    ;; what still stands, the log answers what was said (cells.lisp
    ;; NOTICE-LOG). A clear says nothing, so NIL is never logged.
    (nlk:note-said text level)
    (let ((delivered (broadcast-organism "organism.notice"
                                         (notice-payload text level key))))
      (unless (or delivered *active-tui-program* *system-notice-hook*)
        (format *error-output* "~&~a~%" text))))
  text)

(defun command-result-payload (command-id correlation-id session-id command status code message
                                &optional turn-id (queue-depth 0) message-id sequence
                                extra-response)
  (json-object
   "command_id" command-id
   "correlation_id" correlation-id
   "session_id" session-id
   "command" command
   "status" status
   "message" message
   :when (and code (not (string= code "ok"))) "code" code
   :opt "response"
   (when (or turn-id extra-response)
     ;; queue_depth is integer-typed on the wire; an absent depth is zero,
     ;; never JSON null (cancel_turn results pass no depth). A read with no
     ;; turn running (get_state) answers with no turn_id.
     (apply #'copy-json-object (json-object :opt "turn_id" turn-id
                                            "queue_depth" (or queue-depth 0))
            (and (hash-table-p extra-response)
                 (loop for key being the hash-keys of extra-response using (hash-value value)
                       collect key collect value))))
   :opt "message_id" message-id
   :opt "sequence" sequence))

(defun publish-command-result (peer session-id command-id correlation-id &rest result)
  "Send a requester-only control result outside the session event sequence."
  ;; RESULT is COMMAND-RESULT-PAYLOAD's arguments from COMMAND on.
  (when peer
    (enqueue-peer-frame
     peer (nlk:encode-command-result
           0 (apply #'command-result-payload command-id correlation-id session-id result)
           :session-id (json-value session-id :text)))))

(defun accumulated-frame (session-id log-position turn-id kind payload
                          event-id command-id occurred-at)
  "One durable fact in the shared replay/live frame shape: the sole inner
event shape — KIND, PAYLOAD, the envelope as metadata — under the frame's
seq, session and turn."
  ;; PAYLOAD is the canonical stored payload, its fields unrenamed: the object
  ;; a committed instance was materialized from, or the logged text itself as
  ;; a JSON-VERBATIM. Read again here by event id, it was a query and a decode
  ;; per fact.
  (make-json-object
   "seq" log-position
   "session_id" session-id
   "turn_id" turn-id
   "event" (nlk::encode-inner-event
            kind payload
            :metadata (make-json-object
                       "event_id" event-id
                       "log_position" log-position
                       "turn_id" turn-id
                       "command_id" command-id
                       ;; Durable time rides the wire so clients derive elapsed
                       ;; (the finish divider, replay included) from the log,
                       ;; never from their own clock.
                       "occurred_at" occurred-at))))

(defun accumulated-event-frame (session-id event)
  "EVENT, a materialized instance, as its frame: the live publication."
  (accumulated-frame session-id event.log-position event.turn-id event.kind
                     event.payload event.event-id event.command-id event.occurred-at))

(defun visible-event-frames (session-id facts visible-set)
  "FACTS, STORED-FACTs, as the frames a client folds, less the
replay-suppressed evidence kinds and the turn-bearing facts off VISIBLE-SET's
chain."
  ;; The replay pages and the history pager filter through this one predicate,
  ;; and lay each payload into its frame as the log keeps it. Without this,
  ;; a payload would be decoded into an instance only to be encoded back.
  (loop for stored in facts
        unless (or (nlk:event-kind-replay-suppressed-p stored.kind)
                   (not (nlk::turn-visible-p stored.turn-id visible-set)))
          collect (accumulated-frame session-id stored.log-position stored.turn-id
                                     stored.kind (nlk:json-verbatim stored.payload)
                                     stored.event-id stored.command-id
                                     stored.occurred-at)))

(defun replay-admission-state (session-id)
  "Return the durable turn admission state at replay time."
  (if (and (nlk:store-open-p) (nlk:active-turn-p session-id)) "active" "ready"))

(defun session-cursor (session-id)
  (nlk::session-log-cursor session-id))

(defun resume-position (session-id after cursor)
  "AFTER, the log position a peer says it holds of SESSION-ID, when a
resume from it to CURSOR leaves that peer the transcript a whole replay
would; else NIL."
  ;; What it folded stands only while the head never moved since: a move
  ;; takes turns off the chain (SESSION.HEAD_MOVED, the one fact that can), a
  ;; position past the log names another log, and nothing is held before the
  ;; first fact.
  (and (integerp after) (<= 1 after cursor)
       (zerop (nlk:events :session-id session-id :kind nlk::+kind-head-moved+
                          :after after :as :count))
       after))

(defun send-replay-under-lock (gws peer &optional after &aux (session-id gws.id))
  "Queue one bounded replay job before subsequent live publications, and
behind it what the round in flight streamed that the log does not hold yet.
AFTER, the log position the peer holds, makes it a resume when one can be
(RESUME-POSITION), else the whole replay goes."
  ;; Runs under the session's lock so the job orders ahead of later live
  ;; frames; the job's per-peer sequence is stamped at enqueue time.
  (let* ((cursor (session-cursor session-id))
         (resume (resume-position session-id after cursor))
         (job (make-gw-replay-job :session-id session-id
                                   :cursor cursor
                                   :admission-state (replay-admission-state session-id)
                                   :title (nlk::session-title session-id)
                                   :after resume))
         (partial (stream-partial-payloads gws cursor)))
    ;; A resumed peer keeps what it folded of the round in flight, and the
    ;; partial carries each row whole: it voids its own first, as at a retry.
    (when (and resume partial)
      (push (make-json-object "turn_id" (gethash "turn_id" (first partial))
                              "delta" (make-json-object "type" "stream_reset"))
            partial))
    (prog1 (enqueue-peer-frame peer job :session-id session-id
                               :cursor cursor :cursor-present-p (plusp cursor))
      (dolist (payload partial)
        (send-frame-under-lock gws peer "item_delta" payload)))))

(defun session-facts-after (session-id cursor)
  "SESSION-ID's committed facts above log position CURSOR, oldest first: all
of them out of the store's memory of what it committed, or a page of the log
when that memory does not reach back to CURSOR."
  (multiple-value-bind (committed miss) (nlk:committed-events-after session-id cursor)
    (if miss
        (nlk:page-events :session-id session-id :after cursor :limit 128)
        committed)))

(defun publish-turn-facts (session-id previous-cursor &aux (cursor previous-cursor))
  "Publish committed durable facts after PREVIOUS-CURSOR in bounded pages."
  ;; The live stream carries exactly what a replay of the same log would:
  ;; replay-suppressed evidence kinds are dropped, and turn-bearing facts pass
  ;; the SAME visibility predicate replay uses (EVENT-VISIBLE-P against the
  ;; current head chain) — one predicate, so a continuously-connected peer and
  ;; a reconnecting peer fold the same transcript by construction. A fresh
  ;; TURN.STARTED is the head the instant it commits (newest-started-wins head
  ;; authority) and every queued/steer input fact carries the ACTIVE turn's
  ;; id, so nothing a live peer must see is ever off-chain at publish time;
  ;; only abandoned branches drop. The visible set is recomputed per page, so
  ;; a head move landing mid-publication is confined to the page that raced it
  ;; (the navigate broadcast resyncs every peer anyway). The cursor still
  ;; advances over dropped rows.
  ;;
  ;; The facts come out of the store's memory of what it committed
  ;; (NLK:COMMITTED-EVENTS-AFTER), which after any fact of a running turn is all
  ;; of them; the log is paged only when that memory does not reach back to the
  ;; cursor — a cursor from before the store opened, or further back than the
  ;; ring keeps. Either answer is the same instances in the same order; only
  ;; where they came from differs. This runs on the turn thread after every
  ;; committed fact, between a tool result and the next provider request.
  (loop
    for events = (session-facts-after session-id cursor)
    for visible-set = (when (some #'nlk:event-turn-id events)
                        (nlk:session-visible-turn-set session-id))
    while events
    do (dolist (event events)
         (setf cursor event.log-position)
         (unless (or (nlk:event-kind-replay-suppressed-p
                      event.kind)
                     (and visible-set
                          (not (nlk:event-visible-p event visible-set))))
           (publish-event session-id "session_stream_event"
                          (make-json-object
                           "frame" (accumulated-event-frame session-id event))
                          :cursor cursor :durable-p t))))
  cursor)

(defun plist-object (plist keys &rest pairs)
  "A JSON object of PAIRS, then each of KEYS' value in PLIST under the key's
snake_case wire name."
  (apply #'make-json-object
         (append pairs (loop for key in keys
                             collect (substitute #\_ #\- (string-downcase key))
                             collect (getf plist key)))))

(defun turn-part-projector (session-id turn-id)
  "The gateway's one stream-part reducer: project each STREAM-PART into the
item_delta wire shape."
  ;; A :stream-retry tells clients to void what the failed attempt's partial
  ;; stream buffered (stream_reset). A :tool-input-start opens the client's
  ;; tool row before any argument fragment arrives (same upsert shape, empty
  ;; arguments).
  (lambda (part &aux (kind part.kind)
                     (info part.payload))
    (flet ((delta (object)
             (publish-event session-id "item_delta"
                            (make-json-object
                             "turn_id" turn-id "delta" object))))
      (case kind
        ((:text-delta :reasoning-delta)
         (delta (make-json-object "type" (if (eq kind :text-delta) "text" "reasoning")
                                  "text" part.text)))
        ((:tool-input-start :tool-input-delta)
         (delta (make-json-object "type" "tool_call"
                                  "index" part.index
                                  "name" part.tool-name
                                  "arguments" (if (eq kind :tool-input-start) "" part.arguments))))
        (:stream-retry
         (delta (json-object "type" "stream_reset"
                             :when info "retry"
                             (plist-object info '(:attempt :limit :delay-seconds :status :provider :reason)
                                           "continuation" (and (getf info :continuation) t)))))
        (:stream-fallback
         ;; The round moved to another model: an attached shell may say so.
         ;; The durable turn.provider_fallback fact is the replayed truth.
         (delta (plist-object info '(:from-provider :from-model :to-provider :to-model
                                     :reason :status)
                              "type" "fallback")))
        ;; Deliberately not projected — pinned, not forgotten. Brackets and
        ;; per-round metadata are replay/durable-fact material: :tool-call
        ;; and :finish are covered by the turn.tool_call_started and
        ;; turn.usage durable facts, :reasoning-signature is provider replay
        ;; material, :error terminates in a durable turn terminal, and the
        ;; remaining brackets carry no client-visible state the accumulated
        ;; item_delta upserts do not already carry.
        ((:stream-start :response-metadata
          :text-start :text-end :reasoning-start :reasoning-end
          :reasoning-signature :tool-input-end :tool-call :error :finish)
         nil)))))

;;; --- the round in flight ----------------------------------------------------
;;; The item_delta frames above are live-only: the log holds a round's text,
;;; thinking and calls once the fact that closes the round commits, and not a
;;; word before. Without this, a shell or a page that subscribes mid-round --
;;; a reload, a reconnect, a second tab, a switch away and back -- would
;;; replay the log and open on whatever streamed next, mid-sentence, the
;;; paragraphs before it gone until the turn ended. So a session keeps what
;;; its peers were streamed of the round in flight, folded under its lock
;;; beside the fan-out (PUBLISH-EVENT), and a replay carries it
;;; (SEND-REPLAY-UNDER-LOCK): one item_delta per streamed row, the row whole
;;; so far, which the live deltas behind it append to.
;;;
;;; Retention: live-only. A row leaves when the fact that seats it is
;;; published -- the pairing a shell's fold makes (ui.lisp INGEST-MESSAGE-FACT,
;;; INGEST-TOOL-CALL-FACT, INGEST-ATTEMPT-VOIDED-FACT) -- and every row with
;;; the turn's end.

(defstruct (stream-partial (:copier nil))
  "What a session's peers were streamed of the round in flight."
  (turn-id nil)
  ;; Oldest first, as a shell's tail holds them: (:TEXT BUFFER),
  ;; (:REASONING BUFFER) or (:TOOL-CALL INDEX NAME ARGUMENTS).
  (rows '())
  ;; The stream_reset a retry sent, until the stream moves again: the
  ;; countdown a shell's bar reads, and the internal time it was sent.
  (reset nil)
  (reset-at 0)
  ;; The newest fact that seated a row: a fact is published by every
  ;; publisher that reaches it (PUBLISH-TURN-FACTS), and seats once.
  (seated 0))

(nlk:access (partial stream-partial))

(defparameter +stream-seating-kinds+
  (list nlk::+kind-assistant-completed+ nlk::+kind-attempt-voided+ nlk::+kind-tool-call-started+
        nlk::+kind-turn-completed+ nlk::+kind-turn-failed+ nlk::+kind-turn-cancelled+)
  "The facts that seat what a round streamed: its message, its voided
attempt, each of its calls, and the turn's end.")

(defun stream-partial-delta (partial turn-id delta)
  "Fold one item_delta's DELTA, of TURN-ID, into PARTIAL as a shell's fold
does (client.lisp APPLY-GATEWAY-EVENT): text and thinking onto the newest
row when it is of their kind, a call's fragments onto its stream index's."
  (unless (equal turn-id partial.turn-id)
    (setf partial.turn-id turn-id partial.rows '() partial.reset nil))
  (let ((type (json-value delta :string "type")))
    (cond
      ((member type '("text" "reasoning") :test #'equal)
       (let ((kind (if (equal type "text") :text :reasoning))
             (text (or (json-value delta :string "text") ""))
             (newest-row (car (last partial.rows))))
         (setf partial.reset nil)
         (if (eq kind (first newest-row))
             (loop for char across text do (vector-push-extend char (second newest-row)))
             (setf partial.rows (append partial.rows (list (list kind (stream-text-buffer text))))))))
      ((equal type "tool_call")
       (let ((index (or (json-value delta :integer "index") 0))
             (name (or (json-value delta :string "name") ""))
             (arguments (or (json-value delta :string "arguments") "")))
         (setf partial.reset nil)
         (if-let (row (find-if (lambda (row) (and (eq :tool-call (first row)) (eql index (second row))))
                               partial.rows :from-end t))
           (setf (third row) name (fourth row) arguments)
           (setf partial.rows (append partial.rows (list (list :tool-call index name arguments)))))))
      ;; The attempt's text and thinking were seated, voided, by its fact,
      ;; which lands first; its calls never ran.
      ((equal type "stream_reset")
       (setf partial.rows '()
             partial.reset (and (json-value delta :object "retry") delta)
             partial.reset-at (get-internal-real-time))))))

(defun stream-partial-replayed-reset (partial)
  "PARTIAL's stream_reset as a peer attaching now is sent it: its retry's
delay less what has passed since it was sent."
  ;; A shell switched back mid-backoff counted the whole delay down again.
  (let ((retry (alexandria:copy-hash-table (json-value partial.reset :object "retry")))
        (passed (/ (- (get-internal-real-time) partial.reset-at) internal-time-units-per-second)))
    (when-let (delay (json-value retry :number "delay_seconds"))
      (setf (gethash "delay_seconds" retry) (float (max 0 (- delay passed)))))
    (make-json-object "type" "stream_reset" "retry" retry)))

(defun stream-partial-seat (partial kind position payload)
  "Fold the fact of KIND at log POSITION, carrying PAYLOAD, into PARTIAL:
the rows it seats leave, once."
  (when (and (member kind +stream-seating-kinds+ :test #'equal)
             (> position partial.seated))
    (setf partial.seated position)
    (cond ((equal kind nlk::+kind-tool-call-started+)
           ;; The oldest call of its name: a round's calls stream and start in
           ;; the model's order.
           (let ((name (json-value payload :string "tool-name")))
             (setf partial.rows (remove-if (lambda (row) (and (eq :tool-call (first row))
                                                              (equal name (third row))))
                                           partial.rows :count 1))))
          ((member kind (list nlk::+kind-assistant-completed+ nlk::+kind-attempt-voided+)
                   :test #'equal)
           (setf partial.rows (remove-if (lambda (row) (member (first row) '(:text :reasoning)))
                                         partial.rows)))
          (t (setf partial.rows '() partial.reset nil)))))

(defun note-stream-frame (gws kind payload cursor)
  "Fold one frame of KIND, PAYLOAD, that GWS's peers are sent -- a durable
one at log position CURSOR -- into what the round in flight streamed."
  ;; The caller holds GWS's lock, as a replay takes the partial under it: a
  ;; peer that subscribes is sent each delta once, in the partial or live.
  (cond ((equal kind "item_delta")
         (stream-partial-delta (or gws.partial
                                   ;; Every fact that seats a row streamed before
                                   ;; this first delta was published before it.
                                   (setf gws.partial (make-stream-partial
                                                      :seated (session-cursor gws.id))))
                               (json-value payload :string "turn_id")
                               (json-value payload :object "delta")))
        ((and gws.partial cursor (equal kind "session_stream_event"))
         (stream-partial-seat gws.partial (json-value payload :string "frame" "event" "type") cursor
                              (json-value payload :object "frame" "event" "payload")))))

(defun stream-partial-payloads (gws cursor)
  "The item_delta payloads that hand a peer whose replay ends at log
position CURSOR what GWS's peers were streamed of the round in flight: a
retry's countdown, or each streamed row whole so far."
  ;; The caller holds GWS's lock. The replay reads the log, which may already
  ;; hold a fact its publisher has not reached: it is seated here first, as
  ;; its publication would seat it, so its row never reaches the peer twice.
  (when-let (partial gws.partial)
    (when (or partial.rows partial.reset)
      (loop with after = partial.seated
            while (< after cursor)
            do (let ((events (session-facts-after gws.id after)))
                 (unless events (return))
                 (dolist (event events)
                   (when (> event.log-position cursor) (return))
                   (stream-partial-seat partial event.kind event.log-position event.payload))
                 (setf after (min cursor (nlk:event-log-position (car (last events)))))))
      (flet ((payload (delta) (make-json-object "turn_id" partial.turn-id "delta" delta)))
        (if partial.reset
            (list (payload (stream-partial-replayed-reset partial)))
            ;; A row is (:TEXT BUFFER), (:REASONING BUFFER) or (:TOOL-CALL INDEX NAME ARGUMENTS).
            (loop for (kind buffer-or-index name arguments) in partial.rows
                  collect (payload
                           (if (eq kind :tool-call)
                               (make-json-object "type" "tool_call" "index" buffer-or-index
                                                 "name" name "arguments" arguments)
                               (make-json-object "type" (if (eq kind :text) "text" "reasoning")
                                                 "text" (copy-seq buffer-or-index))))))))))

;;; The latch is only ever held transiently — a finishing worker holds it
;;; between its terminal fact and its cleanup — so expiry means the invariant
;;; broke; the turn is then durably failed rather than silently orphaned.
(defparameter *turn-latch-timeout-seconds* 5
  "How long a durably admitted turn may wait for its session's worker latch.")

(define-error turn-worker-unavailable (error) (session-id)
  (:report "no turn worker became available for session ~a" session-id))

(defun acquire-turn-latch (gws)
  "Take GWS's worker latch, waiting up to *TURN-LATCH-TIMEOUT-SECONDS*."
  ;; By the time a fresh admission has committed :STARTED, the durable
  ;; single-active-turn authority has already ruled that no other turn is
  ;; active — a held latch is a finishing worker that has terminalized but not
  ;; yet cleaned up. So a busy latch is waited out, never treated as a
  ;; refusal: refusing here would orphan a durably admitted turn. Returns T on
  ;; acquisition, NIL on timeout.
  (wait-until (lambda ()
                (bt2:with-lock-held ((gw-session-lock gws))
                  (unless gws.turn-active-p
                    (setf gws.turn-active-p t))))
              (deadline-after *turn-latch-timeout-seconds*)))

(defun release-turn-latch (session-id gws)
  "Free GWS's worker latch, the one way out of ACQUIRE-TURN-LATCH whether a
worker ran or not, then promote SESSION-ID's next queued prompt."
  (bt2:with-lock-held ((gw-session-lock gws))
    (setf gws.turn-active-p nil))
  (ignore-errors (start-queued-turn session-id))
  ;; After the queued prompts: a promoted prompt keeps the session busy and
  ;; its own end wakes; nothing promoted, the exits wake the idle session now.
  (ignore-errors (wake-idle-session session-id)))

(defparameter *session-naming* :thread
  ":THREAD names a session from its first prompt (NAME-SESSION) on a thread
of its own, :INLINE inside the SUBMIT that admits it (a gate that covers
naming), NIL never (the test posture).")

(defun submit (session-id prompt &key steer images command-id invocation source source-id)
  "Put PROMPT into SESSION-ID: the one in-process ingress, which the
WebSocket start_turn/steer_turn commands are shells over."
  ;; Returns the
  ;; NLK:ACTIVE-INPUT-ADMISSION the kernel recorded — its disposition is
  ;; :STARTED (a worker thread now owns the turn), :QUEUED (parked behind the
  ;; active turn; dequeue promotion runs it), :STEERED (parked; the running turn
  ;; ends at its next round boundary and this prompt starts the next turn) or
  ;; :DUPLICATE (COMMAND-ID was already admitted; nothing spawned).
  ;;
  ;; STEER names the running turn the prompt cuts short — it must still be the
  ;; active one; without it the prompt starts a turn or queues behind the
  ;; active one. IMAGES are (MEDIA-TYPE . BASE64) attachments; COMMAND-ID is
  ;; the idempotency key (generated when absent); INVOCATION is the provenance
  ;; record, defaulting to the kernel's in-process one. An in-process submitter
  ;; with no record of its own names its SOURCE kind (`cron', `team') and
  ;; SOURCE-ID instead, and the record is built here, turn.start or turn.steer.
  ;;
  ;; Signals rather than returning a status: NLK:UNKNOWN-SESSION, the kernel's
  ;; NLK:TURN-LIFECYCLE-ERROR family for a steer that found no matching active
  ;; turn, and TURN-WORKER-UNAVAILABLE when the worker latch never cleared (the
  ;; turn is already failed durably by then). Every admission fact reaches the
  ;; live stream before this returns.
  ;;
  ;; Runs on the caller's thread and never blocks on the turn: the worker is
  ;; spawned, not joined.
  (check-type session-id string)
  (check-type prompt string)
  ;; The input cap is read here, at the one ingress every submitter comes
  ;; through, before anything is admitted: ADMIT's own check runs on the
  ;; worker after the durable admission, and without this an input past the
  ;; cap would be admitted, leaving a turn with no round and no terminal
  ;; fact. The refusal is the submitter's answer.
  (when (> (length prompt) *max-input-characters*)
    (error 'input-too-large :length (length prompt) :limit *max-input-characters*))
  (let* ((command-id (or command-id (nlk::make-durable-id "command")))
         ;; Captured BEFORE the admission facts commit: reading the cursor
         ;; afterwards would page past the input/turn-start facts and they
         ;; would never appear on the live stream.
         (pre-cursor (session-cursor session-id))
         ;; Read before the admission too: a session with no name -- none
         ;; given, and no admitted prompt that says something -- is named
         ;; from this one (NAME-SESSION).
         (unnamed (and *session-naming* (string= "" (nlk::session-title session-id))))
         ;; The pictures the operator's message shows, read once here, at
         ;; the one ingress: the admission records them with the input.
         (metadata (when-let (facts (prompt-images images))
                     (json-object "images" facts)))
         (admission (nlk:admit-active-turn-input
                     session-id command-id prompt
                     :disposition (if steer :steer :follow-up)
                     :expected-turn-id steer
                     :prompt-id command-id
                     :definition-revision (nlk:current-definition-revision)
                     :invocation (or invocation
                                     (and source (nlk:default-invocation
                                                  (if steer "turn.steer" "turn.start")
                                                  command-id :source source :id source-id)))
                     :images images
                     :metadata metadata)))
    (ecase admission.disposition
      ;; The admitted turn already has an owner (a live worker or the boot
      ;; resume pass); acknowledging must not spawn a second one.
      (:duplicate nil)
      ;; Both park in the durable queue snapshot, images included, and both
      ;; are delivered by dequeue promotion after the active turn ends — a
      ;; steer sooner, because the running turn ends at its next round
      ;; boundary on seeing it parked (NLK:STEER-PARKED-P), and first, ahead
      ;; of the follow-ups. The snapshot is evidence and never streams; its
      ;; live frame (PUBLISH-QUEUE-SNAPSHOT) is what a client's pending band
      ;; shows.
      ((:queued :steered)
       (publish-turn-facts session-id pre-cursor)
       (publish-queue-snapshot session-id))
      (:started (start-admitted-turn session-id admission.turn prompt images pre-cursor)
       (when unnamed
         ;; Beside the turn, never ahead of it. A title is a nicety: a model
         ;; that cannot be reached leaves the prompt's line, in the log alone.
         (flet ((name ()
                  (with-handlers ((error (condition)
                                    (warn "session ~a was not named: ~a" session-id condition)))
                    (name-session session-id prompt))))
           (if (eq *session-naming* :inline)
               (name)
               (spawn (format nil "name-~a" session-id) (name)))))))
    admission))

(defun newest-queue-snapshot (session-id)
  "SESSION-ID's newest durable queue snapshot payload, or NIL."
  (let ((payload (nlk:events :session-id session-id :kind nlk::+kind-turn-queue-snapshot+
                             :order :newest :as :payload)))
    (and (hash-table-p payload) payload)))

(defun publish-queue-snapshot (session-id)
  "Send SESSION-ID's queue as it now stands to every attached peer: the live
frame of a durable snapshot that is evidence and never streams itself."
  (when-let (payload (newest-queue-snapshot session-id))
    (publish-event session-id "session_input_queue_snapshot_updated" payload)))

(defun start-admitted-turn (session-id turn prompt images pre-cursor &key prefill)
  "Give the durably admitted TURN its worker thread and publish its
projections. PREFILL is a harness turn's first round (RUN)."
  ;; PRE-CURSOR is the session's durable cursor from before the admission
  ;; facts committed. The three entries are SUBMIT's :STARTED arm, boot
  ;; recovery, and dequeue promotion.
  ;;
  ;; The durable admission is authoritative; the latch only prevents duplicate
  ;; worker threads. A turn that was durably admitted MUST get a worker or a
  ;; durable terminal fact — never a silent return that leaves ACTIVE-TURN-P
  ;; wedged with no owner — so a latch that never clears fails the turn and
  ;; signals TURN-WORKER-UNAVAILABLE.
  (let ((gws (find-or-create-gw-session session-id))
        (turn-id (nlk:durable-turn-turn-id turn)))
    (unless (acquire-turn-latch gws)
      (let ((condition (make-condition 'turn-worker-unavailable
                                       :session-id session-id)))
        (ignore-errors (nlk:fail-turn turn condition))
        (publish-turn-facts session-id pre-cursor)
        (error condition)))
    ;; Admission is durable before it is the first live publication. That
    ;; publication runs the :FRAME chain here, before the worker exists: a
    ;; hook that signals fails the turn durably and frees the latch, as the
    ;; worker would, then signals to the caller like the latch timeout above
    ;; (a debug hook's SUBSEQ wedged two Discord sessions so).
    (let ((published-cursor
            (handler-case (publish-turn-facts session-id pre-cursor)
              (serious-condition (condition)
                (ignore-errors (nlk:fail-turn turn condition))
                (ignore-errors (publish-turn-facts session-id pre-cursor))
                (release-turn-latch session-id gws)
                (error condition)))))
      (spawn (format nil "turn-~a" turn-id)
        (flet ((flush-facts ()
                 (setf published-cursor
                       (publish-turn-facts session-id published-cursor))))
          (unwind-protect
               (let ((*turn-fact-committed-fn* #'flush-facts)
                     (*turn-part-fn*
                       (turn-part-projector session-id turn-id)))
                 ;; RUN commits the outcome once its own handlers are open, and
                 ;; they open after its first ADMIT: a signal before that point
                 ;; would leave the admitted turn with no owner, so it is
                 ;; failed here, durably -- the invariant above, held for RUN.
                 (handler-case
                     (run prompt :session (intern-session session-id)
                                 :admitted-turn turn
                                 :images images
                                 :prefill prefill)
                   (serious-condition (condition)
                     (ignore-errors
                      (unless (nlk:turn-terminal-kind turn-id)
                        (nlk:fail-turn turn condition)))))
                 ;; Nothing may escape this thread: under --disable-debugger
                 ;; an unhandled condition in a worker quits the whole image.
                 (ignore-errors (flush-facts))
                 ;; The terminal fact is on the live stream before the turn
                 ;; can leave the chain: a cancel that asked for retraction
                 ;; is honoured here, latch held, before the next turn is
                 ;; promoted onto the rewound head — and refused when input
                 ;; is parked on the turn, so the promotion below starts
                 ;; that input on top of the cut turn instead.
                 (ignore-errors
                  (when (and (nlk:turn-retract-requested-p turn)
                             (equal (nlk:turn-terminal-kind turn-id) nlk::+kind-turn-cancelled+)
                             (zerop (nlk:turn-assistant-round-count turn-id))
                             (zerop (length (nlk::queue-prompts session-id turn-id)))
                             (equal (nlk:session-head-turn-id session-id) turn-id))
                    (let ((parent (gethash turn-id (nlk:session-composed-parents session-id))))
                      ;; A START_TURN admitted since the cancel refuses the move: a plain cancel.
                      (with-handlers ((nlk:turn-already-active () nil))
                        (move-session-head-durably session-id parent "user_message"
                                                   nlk:+head-move-reason-retract+)
                        (broadcast-head-moved session-id parent turn-id "user_message"
                                              nlk:+head-move-reason-retract+))))))
            (release-turn-latch session-id gws))))
      turn)))

(defun start-queued-turn (session-id &aux (pre-cursor (session-cursor session-id)))
  "Promote SESSION-ID's next queued prompt to a turn, when one waits."
  (when-let (next (nlk::dequeue-next-turn session-id))
    (start-admitted-turn session-id (first next) (second next) (third next) pre-cursor)))
;;; --- checkpoint navigation ------------------------------------------------
;;; The pi-mono rewind: one durable head move, a KV fold to the anchor
;;; watermark, a hash-guarded workspace restore, and a resync broadcast.
;;; The conversation head is authoritative — a workspace restore failure
;;; degrades and reports, it never rolls the head move back.

(defun turn-started-input-of (turn-id)
  "The committed prompt of TURN-ID (turn ids are globally unique)."
  (json-value (nlk:events :turn-id turn-id :kind nlk::+kind-turn-started+ :as :payload)
              :string "input"))

(defun move-session-head-durably (session-id new-head boundary reason)
  "One transaction: the durable head move (re-checking the active-turn
authority) and the KV fold commit together — no crash window where the KV
is folded to a branch the head no longer points at — then the hydration
cache forgets the abandoned chain."
  (nlk:navigate-session-head session-id new-head
                             :reason reason :boundary-kind boundary)
  (invalidate-session-hydration-cache session-id))

(defun broadcast-head-moved (session-id new-head previous-head boundary reason)
  "Tell every peer the transcript changed; the resubscribe replay now
serves only the visible chain."
  (publish-event session-id "session_checkpoint_undo_applied"
             (make-json-object
              "session_id" session-id
              "target_turn_id" (or new-head "")
              "previous_turn_id" (or previous-head "")
              "reason" reason
              "boundary" boundary))
  (publish-event session-id "session_resync_required"
             (make-json-object "session_id" session-id)))

(defun execute-navigate-checkpoint (peer session-id target new-head boundary
                                    checkpoint-id undone-prompt redo-p
                                    restore-files command-id correlation-id)
  "The latch-held body of one navigation: durable head move, KV fold,
workspace restore, resync broadcast, command result."
  ;; RESTORE-FILES NIL is the Zig UndoScope conversation_only: the head move
  ;; and KV fold still run, the workspace restore is skipped.
  (let* ((session (nlk:find-session session-id))
         (workspace (and session (nlk:durable-session-cwd session)))
         (captures (nlk:session-workspace-captures session-id))
         (parents (nlk:session-composed-parents session-id))
         (previous-head (nlk:session-head-turn-id session-id))
         (head-tree (nlk:session-head-tree captures parents previous-head))
         ;; A capture postdates the write it must reverse: restores take the tree before.
         (target-tree
           (cond
             (checkpoint-id (nlk:restore-tree-for-checkpoint captures checkpoint-id parents))
             ((null target)
              (some (lambda (entry) (getf entry :tree-sha)) captures))
             ((string= boundary "user_message")
              (when-let (capture (find-if (lambda (entry)
                                            (and (equal (getf entry :turn-id) target)
                                                 (equal (getf entry :boundary-kind)
                                                        "user_message")))
                                          captures))
                (nlk:restore-tree-for-checkpoint captures (getf capture :checkpoint-id) parents)))
             (t
              (loop for capture in (reverse captures)
                    thereis (and (equal (getf capture :turn-id) target)
                                 (getf capture :tree-sha)))))))
    (move-session-head-durably session-id new-head boundary
                               (if redo-p "redo" "rewind"))
    ;; Workspace restore, outside every transaction. Failure degrades.
    (let* ((outcome (if (and workspace target-tree restore-files)
                        (nlk:restore-to-checkpoint target-tree head-tree
                                                   :workspace workspace
                                                   :session-id session-id)
                        (list :status :skipped)))
           (preimage (getf outcome :preimage-tree-sha)))
      ;; The redo preimage becomes the abandoned head's newest capture, so
      ;; navigating back restores it with no special-casing.
      (when (and preimage previous-head)
        (ignore-errors
          (nlk:record-workspace-capture session-id
                                        :turn-id previous-head
                                        :boundary-kind "preimage"
                                        :tree-sha preimage
                                        :anchor-position
                                        (nlk:turn-end-log-position previous-head))))
      (broadcast-head-moved session-id new-head previous-head boundary
                            (if redo-p "redo" "rewind"))
      (publish-command-result
       peer session-id command-id correlation-id "navigate_checkpoint"
       "accepted" "ok" "session head moved" (or new-head "") 0 nil nil
       (json-object
        "workspace" (string-downcase (symbol-name (or (getf outcome :status) :skipped)))
        "conflicts" (length (getf outcome :conflicts))
        ;; A skipped/degraded restore names WHY at the wire — the client
        ;; renders the refusal, not a bare status word.
        :when (getf outcome :degradation-reason) "degradation_reason"
        (string-downcase (getf outcome :degradation-reason))
        :opt "undone_prompt" undone-prompt)))))

;;; --- background-exec projection seam ---------------------------------------
;;; Live-only frames from the shell registry (src/gateway/server.zig
;;; runBackgroundExecWatcher + wire.zig writeBackgroundProcessUpdatedFrame).
;;; Retention: live-only — the frames (subscribe snapshots included) never
;;; touch the durable log; durable truth is the turn.tool_result pair (the
;;; backgrounding handoff and the watcher exit fact).

(defun background-process-update-payload (update)
  "The session_background_process_updated payload for one registry UPDATE."
  ;; Null/false optionals are omitted (the Zig wire_json shape): exit_code
  ;; only when known, backgrounded only when true, sudo fields only while
  ;; pending. rows/cols are omitted entirely — pipes have no geometry (decoder
  ;; default 0).
  (make-json-object
   "session_id" update.session-id
   "process"
   (json-object
    "shell_session_id" update.shell-session-id
    "tool_call_id" update.tool-call-id
    "command" update.command
    "status" (if (eq :exited update.status) "exited" "running")
    "output_b64" (cl-base64:usb8-array-to-base64-string
                  update.chunk)
    "offset" update.chunk-offset
    :when (integerp update.exit-code)
    "exit_code" update.exit-code
    :when (integerp update.shell-exit)
    "shell_exit" update.shell-exit
    :when update.backgrounded "backgrounded" t
    ;; A running snippet's live preview, whole each time: what its command wrote
    ;; lately, for a band -- not a chunk of the stream the offsets splice.
    :when update.preview
    "preview_b64" (cl-base64:usb8-array-to-base64-string update.preview)
    ;; An exited backgrounded snippet's last lines, as its exit fact keeps them.
    :opt "tail" update.tail
    :when update.tail-cut "tail_cut" t
    :opt "error" update.error
    :opt "stopped" update.stopped)))

(defun record-background-shell-exit-from-update (update &aux (turn-id update.turn-id))
  "Append the durable exit fact for a FINAL update (server.zig
recordBackgroundShellExitFact): skipped when the shell has no owning turn."
  ;; Not broadcast live — live peers already saw the watcher's exited frame;
  ;; replay serves it. The workspace window a detached call held open closes
  ;; here too: writes landing after the yield-time bracket ride the exit fact
  ;; as the cumulative workspace_diff document, and the completion tree stamps
  ;; its own capture so the late window stays durable for undo.
  (when (and (stringp turn-id) (nlk:store-open-p))
    (with-handlers ((error (condition)
                      (warn "background shell exit fact not recorded: ~a" condition)))
      ;; The yield-time capture this window closes on may still be
      ;; deferred to its turn's next fact: land it first, so the
      ;; lookup finds it and no second walk queues behind its index.
      (nlk:settle-turn-facts turn-id)
      (multiple-value-bind (document tree-sha yield-tree)
          (nlk:close-detached-workspace-window update.session-id update.tool-call-id)
        (let ((position
                (nlk:record-background-shell-exit
                 update.session-id
                 turn-id
                 update.tool-call-id
                 update.command
                 update.shell-session-id
                 update.exit-code
                 :shell-exit update.shell-exit
                 :tail update.tail
                 :tail-cut update.tail-cut
                 :error update.error
                 :stopped update.stopped
                 :workspace-diff document)))
          (when tree-sha
            (nlk:record-workspace-capture
             update.session-id
             :turn-id turn-id
             :boundary-kind "tool_result"
             :tool-call-id update.tool-call-id
             :tree-sha tree-sha
             :pre-tree-sha yield-tree
             :anchor-position position)))))))

(defvar *background-exec-watcher* nil)

(defvar *exit-wake-lock* (bt2:make-lock :name "exit-wake"))

(defun wake-idle-session (session-id)
  "Wake SESSION-ID once, with every exit the model never observed whose
spawning turn completed, and only while the session is idle: no turn running,
no prompt queued."
  ;; A busy session leaves its exits unclaimed for the running turn to read
  ;; at its next round boundary (turn.lisp AGENT-LOOP), or for the turn that
  ;; ends to collect, so exits that land together wake one turn. The lock holds
  ;; across SUBMIT, so the watcher and a finishing worker cannot both find the
  ;; session idle.
  (when (and (stringp session-id) (nlk:store-open-p))
    (bt2:with-lock-held (*exit-wake-lock*)
      (unless (or (nlk:active-turn-p session-id)
                  (plusp (length (nlk::queue-prompts session-id))))
        ;; A turn of the harness's own, with no user message, opening on an
        ;; (eval-status ID) call per exit (EXIT-WAKE-MESSAGE), its start fact
        ;; naming the exits that woke it (EXIT-WAKE-DOCUMENT).
        (when-let (ids (claim-exit-wakes (exec-registry-instance) session-id))
          (handler-case
              (let* ((pre-cursor (session-cursor session-id))
                     (turn (nlk:admit-wake-turn
                            session-id (nlk:make-durable-id "command")
                            :wake (exit-wake-document (exec-registry-instance) ids))))
                (if turn
                    (start-admitted-turn session-id turn "" nil pre-cursor
                                         :prefill (exit-wake-message ids))
                    (release-exit-wakes (exec-registry-instance) ids)))
            (error (condition)
              (warn "exit wake not started for ~a: ~a" session-id condition))))))))

(setf nlk:*session-wake*
      ;; The kernel's one way to put text into a session, wired to the one
      ;; in-process ingress. On a thread of its own: the callers inside the
      ;; kernel are error handlers running mid-turn, before the unwind, and
      ;; an ingress call from inside the failing turn's own stack would
      ;; re-enter admission underneath it. A wake that lands while the turn
      ;; is still dying queues, and the worker's cleanup promotes it.
      (lambda (session-id text)
        (spawn "session-wake"
          ;; A session the store cannot admit (a local eval's placeholder, a torn-down
          ;; session) is skipped, never an error.
          (handler-case (submit session-id text)
            (error (condition)
              (warn "session wake not submitted for ~a: ~a" session-id condition)
              nil)))))

(defun background-exec-watcher-tick (&key wake)
  (dolist (update (ignore-errors (exec-collect-updates (exec-registry-instance))))
    ;; One live-only chunk frame; sessions the store does not know (local
    ;; tool runs) are skipped, never an error.
    (ignore-errors
     (publish-event update.session-id
                    "session_background_process_updated"
                    (background-process-update-payload update)))
    (when update.final
      (record-background-shell-exit-from-update update)
      ;; The watcher edge of the exit wake: an idle session wakes now.
      (when wake
        (ignore-errors (wake-idle-session update.session-id))))))

(defun start-background-exec-watcher ()
  (unless *background-exec-watcher*
    (setf *background-exec-watcher*
          (worker-start "background-exec-watcher"
                        (lambda ()
                          (ignore-errors (scan-exec-child-stalls (exec-registry-instance)))
                          ;; The runaway probe is no longer driven from here: it has
                          ;; one owner, the eval guard, which starts with the first
                          ;; evaluation in any image and reaches synchronous snippets this
                          ;; watcher never listed (exec.lisp ENSURE-EVAL-GUARD).
                          (background-exec-watcher-tick :wake t))
                        :wake 0.2))))


;;; Filled by DEFINE-SYNC-COMMAND; DISPATCH-COMMAND routes every
;;; session-scoped client command through here.
(defvar *sync-command-handlers* (make-hash-table :test #'eq)
  "Command-kind keyword -> handler function.")

(defmacro define-sync-command (kind (&rest payload-specs) &body body)
  "Define the handler for one session-scoped WebSocket client command."
  ;; KIND is the command-kind keyword. The result frame's wire name always
  ;; comes from COMMAND-KIND-WIRE-NAME, so a handler can never publish a name
  ;; the envelope codec does not speak. PAYLOAD-SPECS are WITH-JSON specs read
  ;; from the request payload; SID is always bound first, as the payload's
  ;; session_id string (NIL when absent or wrong-typed — validation material,
  ;; not a crash).
  ;;
  ;; BODY runs with PEER, PAYLOAD, SID, COMMAND-ID, CORRELATION-ID, and
  ;; SUBSCRIPTION-LIST bound, plus a RESULT flet:
  ;;
  ;;   (RESULT STATUS CODE MESSAGE &REST TRAILING)
  ;;
  ;; that publishes the correlated command result to the requesting peer;
  ;; TRAILING are PUBLISH-COMMAND-RESULT's optional response fields (turn-id,
  ;; queue-depth, message-id, sequence, extra-response).
  ;;
  ;; Leading clauses run in written order, each a rejection when its test
  ;; fails:
  ;;
  ;;   (:require FORM MESSAGE)          invalid_request with MESSAGE unless FORM
  ;;   (:subscribed MESSAGE [EXTRA...])  not_subscribed unless SID is among the
  ;;                                     peer's accepted subscriptions; EXTRA
  ;;                                     forms ride as RESULT's trailing args
  ;;
  ;; A body of one (:admit ACTION SUBMIT-ARG...) clause is the turn-admission
  ;; shell: PROMPT and IMAGES submitted on SID under a websocket invocation
  ;; named ACTION, with SUBMIT-ARGS riding to SUBMIT, and the admission
  ;; relayed through RESULT. A (:target PROVIDER MODEL EFFORT) clause before
  ;; it pins SID to that target first (CALL-ADMISSION).
  ;;
  ;; The shared entry contract — decode, scope check, protocol-error mapping —
  ;; stays in DISPATCH-COMMAND; a declaration here owns only its own command.
  (let* ((clauses (loop while (and (consp (first body))
                                   (member (first (first body)) '(:require :subscribed)))
                        collect (pop body)))
         (target (when (and (consp (first body)) (eq (first (first body)) :target))
                   (rest (pop body)))))
    (when (and (consp (first body)) (eq (first (first body)) :admit))
      (destructuring-bind (action &rest submit-args) (rest (first body))
        (setf body
              ;; A shell: admission semantics live in SUBMIT, only its wire mapping here.
              `((apply
                 #'result
                 (call-admission
                  sid ,(and target `(list ,@target))
                  (lambda ()
                 (with-handlers ((turn-worker-unavailable ()
                                   (list "rejected" "worker_unavailable"
                                         "no turn worker became available; turn failed durably"))
                                 (nlk:unknown-session ()
                                   (list "rejected" "unknown_session" "session does not exist"))
                                 (error (condition)
                                   (list "rejected" "admission_failed"
                                         (princ-to-string condition))))
                   (let ((admission
                           (submit sid prompt ,@submit-args :command-id command-id
                                   :images images
                                   ;; Provenance: a WebSocket client command.
                                   :invocation (nlk:make-invocation
                                                :action ,action :command-id command-id
                                                :correlation-id correlation-id
                                                :source (nlk:make-invocation-source "gateway")
                                                :transport (nlk:make-invocation-transport
                                                            "websocket")))))
                     (ecase admission.disposition
                       (:started
                        (list "accepted" "ok" "turn admitted" admission.turn-id 0 command-id 1))
                       (:duplicate
                        (list "accepted" "ok" "turn already admitted" admission.turn-id 0
                              admission.prompt-id admission.sequence))
                       (:queued
                        (list "queued" "ok" "turn input queued" admission.turn-id
                              admission.queue-depth admission.prompt-id admission.sequence))
                       (:steered
                        (list "accepted" "ok" "turn steered" admission.turn-id 0
                              admission.prompt-id admission.sequence))))))))))))
    `(setf (gethash ,kind *sync-command-handlers*)
           (lambda-ignoring (peer payload command-id correlation-id subscription-list)
             (with-json ((sid :string "session_id") ,@payload-specs) payload
               (flet ((result (status code message &rest trailing)
                        (apply #'publish-command-result
                               peer (or sid "") command-id correlation-id
                               (nlk::command-kind-wire-name ,kind)
                               status code message trailing)))
                 (declare (ignorable #'result))
                 (cond
                   ,@(loop for (kind head . rest) in clauses
                           collect (ecase kind
                                     (:require `((not ,head)
                                                 (result "rejected" "invalid_request" ,@rest)))
                                     (:subscribed
                                      `((not (member sid (car subscription-list) :test #'string=))
                                        (result "rejected" "not_subscribed" ,head ,@rest)))))
                   (t ,@body))))))))

(defvar *start-turn-pin-lock* (bt2:make-lock :name "start_turn target pin")
  "Serializes PIN-START-TURN-TARGET's read and write, so two start_turns
racing on one session not yet begun cannot both find it unpinned.")

(defun pin-start-turn-target (session-id provider model effort)
  "Pin SESSION-ID to the target a start_turn's PROVIDER, MODEL and EFFORT
members name, before its turn is admitted: SELECT-SESSION-MODEL's pin
without its default half, so the shared config, this image's default and
every other session stay as they are. => (values REFUSAL UNDO): REFUSAL a
(CODE MESSAGE) list when the members are refused, nothing written; UNDO a
thunk that puts back the selection the pin replaced, NIL when nothing was
written."
  ;; Only a session not yet begun takes a target: its first turn pins what it
  ;; runs (RUN), and a switch mid-conversation would re-send the whole
  ;; prompt uncached. A begun session takes members naming its own target --
  ;; a reconnecting client resends its start_turn as it was -- and refuses
  ;; any other. A model alone keeps the lane the session would run on, as a
  ;; /models pick does. Written before admission, because the first turn's
  ;; RUN pins the default on a session it finds unpinned; under the lock,
  ;; because the read and the write are two store calls. A session this
  ;; image does not know is SUBMIT's to refuse, by name.
  (check-type session-id string)
  (unless (and (nlk:store-open-p)
               (handler-case (nlk:session-exists-p session-id) (type-error () nil)))
    (return-from pin-start-turn-target (values nil nil)))
  (bt2:with-lock-held (*start-turn-pin-lock*)
    (bind (((pinned-provider pinned-model) (nlk:session-model-selection session-id))
           (picked-effort (nlk:session-model-effort session-id))
           (target-provider (or provider pinned-provider *provider*))
           (target-model (or model pinned-model *model*)))
      (when (and pinned-provider pinned-model)
        (return-from pin-start-turn-target
          (if (and (equal target-provider pinned-provider) (equal target-model pinned-model)
                   (or (null effort) (equal effort picked-effort)))
              (values nil nil)
              (values (list "session_pinned"
                            (format nil "session ~a runs on ~a/~a~@[ at effort ~a~]: ~
                                         a session keeps the target it began on"
                                    session-id pinned-provider pinned-model picked-effort))
                      nil))))
      (handler-case
          (progn
            (check-model-id target-provider target-model)
            ;; SELECT-DEFAULT-MODEL's resolve half: the lane, and the address
            ;; it rides unless the process override already is one.
            (if (and *endpoint* (equal target-provider *provider*))
                (find-provider-lane target-provider)
                (provider-endpoint target-provider))
            (check-effort target-provider target-model effort))
        (provider-error (condition)
          (return-from pin-start-turn-target
            (values (list "unknown_provider" (provider-error-detail condition)) nil)))
        (effort-refusal (condition)
          (return-from pin-start-turn-target
            (values (list "effort_unsupported" (effort-refusal-detail condition)) nil))))
      (nlk:record-session-model-selection
       session-id :provider target-provider :model target-model
       :effort (or effort (admitted-effort picked-effort target-provider target-model)))
      (broadcast-model-changed session-id)
      (values nil
              (lambda ()
                (nlk:record-session-model-selection
                 session-id :provider pinned-provider :model pinned-model :effort picked-effort)
                (broadcast-model-changed session-id))))))

(defun call-admission (session-id target admit)
  "ADMIT's (STATUS CODE MESSAGE ...) answer for SESSION-ID, once TARGET -- a
start_turn's (PROVIDER MODEL EFFORT), NIL or all NIL when it names none --
has pinned the session (PIN-START-TURN-TARGET): a refused target is the
answer and nothing is admitted, and an admission refused after the pin puts
the session's selection back."
  (multiple-value-bind (refusal undo)
      (and (some #'identity target) (apply #'pin-start-turn-target session-id target))
    (if refusal
        (list* "rejected" refusal)
        (let ((outcome (funcall admit)))
          (when (and undo (equal "rejected" (first outcome)))
            (funcall undo))
          outcome))))

(define-sync-command :start-turn ((prompt :text "prompt")
                                  ;; :any — the codec normalizes wire content
                                  ;; to a Lisp list of staged attachments, not
                                  ;; a JSON array.
                                  (images :any "images")
                                  ;; The target a session not yet begun runs
                                  ;; on, all optional (PIN-START-TURN-TARGET).
                                  (provider :text "provider")
                                  (model :text "model")
                                  (effort :text "effort"))
  (:require (and sid prompt) "session_id and non-empty prompt are required")
  (:subscribed "subscribe to the session before starting a turn")
  (:target provider model effort)
  (:admit "turn.start"))

(define-sync-command :steer-turn ((prompt :text "prompt")
                                  (images :any "images")
                                  (expected :string "expected_turn_id"))
  (:require (and sid expected prompt)
            "session_id, expected_turn_id, and non-empty prompt are required")
  (:subscribed "subscribe to the session before steering a turn")
  (:admit "turn.steer" :steer expected))

(define-sync-command :withdraw-prompt ((prompt-id :string "prompt_id"))
  ;; Up on a queued prompt, a steer or a follow-up, from the shell that
  ;; holds it: the prompt leaves the durable queue
  ;; (NLK:WITHDRAW-PENDING-PROMPT) and comes home to the composer with its
  ;; images; the queue snapshot that follows is the durable answer, and this
  ;; result is the requester's own.
  (:require (and sid prompt-id) "session_id and prompt_id are required")
  (:subscribed "subscribe to the session before withdrawing a prompt")
  (if-let (withdrawn (nlk:withdraw-pending-prompt sid prompt-id))
    ;; The extra members ride the result so the shell that peeled the
    ;; prompt gets its images back ("images" joins only when there are
    ;; any — a vector of image items, the wire's own shape); every other
    ;; peer learns it left from the queue's live frame.
    (progn
     (publish-queue-snapshot sid)
     (result "accepted" "ok" "prompt withdrawn"
            (getf withdrawn :turn-id) (getf withdrawn :queue-depth)
            nil nil (json-object "prompt_id" prompt-id
                                 "content" (getf withdrawn :content)
                                 :opt "images" (when-let (images (getf withdrawn :images))
                                                 (coerce (nlk::image-content-items images)
                                                         'vector)))))
    (result "rejected" "prompt_not_pending"
            "no queued prompt with that prompt id")))

(define-sync-command :cancel-turn ((turn-id :string "turn_id")
                                   ;; Strategy input: stop the turn AND take
                                   ;; it back if it produced nothing
                                   ;; (START-ADMITTED-TURN). JSON true
                                   ;; only; absent is the plain cancel.
                                   (retract :boolean "retract"))
  (:require (and sid turn-id) "session_id and turn_id are required")
  (:subscribed "subscribe to the session before cancelling a turn" turn-id)
  ;; A surface's stop is the operator's own: the calls it cuts say so.
  (cond ((nlk:request-cancel-turn sid turn-id :operator retract)
         ;; Background-shell waits may be parked for minutes; the cancelled
         ;; turn's tool call must observe the cancel now, not when its yield
         ;; window expires (server.zig wakeSession-on-cancel).
         (wake-exec-session (exec-registry-instance) sid)
         (result "accepted" "ok" "cancellation requested" turn-id))
        (t (result "rejected" "turn_mismatch" "turn_id is not the active turn"
                   turn-id))))

(define-sync-command :interrupt-exec ((exec-id :integer "exec_id"))
  ;; The pending-exec view's gesture: the operator stops a background
  ;; evaluation from the surface they are watching, instead of asking a model
  ;; to call (eval-interrupt). The soft abort is exactly the condition
  ;; EVAL-INTERRUPT's default arm sends; the exit fact, the band row's
  ;; retirement and the wake all follow the ordinary path, so this command
  ;; only asks and answers.
  (:require (and sid exec-id) "session_id and exec_id are required")
  (:subscribed "subscribe to the session before interrupting an exec")
  (let ((process (find-background-eval (exec-registry-instance) exec-id)))
    (cond ((null process)
           (result "rejected" "unknown_exec"
                   "no background exec with that id"))
          ((not (equal sid process.session-id))
           (result "rejected" "exec_session_mismatch"
                   "that exec belongs to another session"))
          ((eq :exited process.status)
           (result "rejected" "exec_exited" "that exec already exited"))
          (t (exec-stop-process process :by :operator)
             (result "accepted" "ok" "interrupt requested")))))

(define-sync-command :navigate-checkpoint ((target :text "target_turn_id")
                                           (checkpoint-id :text "checkpoint_id")
                                           (boundary :string "boundary" :default "turn")
                                           ;; The confirm-stage scope switch (Zig UndoScope):
                                           ;; absent or null means the full
                                           ;; conversation_and_files restore; an explicit JSON
                                           ;; false (shasht reads it as NIL) selects
                                           ;; conversation_only. This is one of the two strict
                                           ;; parsers that must tell JSON null apart from JSON
                                           ;; false, so it reads GETHASH's own presence flag
                                           ;; (CONFIG-BOOLEAN) rather than the lenient
                                           ;; JSON-VALUE waist (json.lisp).
                                           (restore-files
                                            (nlk:config-boolean payload "restore_files" t))
                                           ;; DIRECTION lets thin shells say /undo and /redo
                                           ;; without computing targets: the server owns the
                                           ;; tree, so it resolves them.
                                           (direction :string "direction"))
  (:require sid "session_id is required")
  (:subscribed "subscribe to the session before navigating")
  (:require (member boundary '("turn" "user_message") :test #'string=)
            "boundary must be \"turn\" or \"user_message\"")
  (:require (member direction '(nil "undo" "redo") :test #'equal)
            "direction must be \"undo\" or \"redo\"")
  ;; Run off the WS reader thread under the session turn latch (restore does filesystem work).
  (let ((gws (find-or-create-gw-session sid)))
    (block navigate
      (flet ((reject (code message)
               (result "rejected" code message)
               (return-from navigate nil)))
        (cond
          ((equal direction "undo")
           (let ((head (nlk:session-head-turn-id sid)))
             (unless head (reject "noop" "nothing to undo"))
             (setf target head boundary "user_message")))
          ((equal direction "redo")
           ;; The newest head move remembers where the operator came FROM;
           ;; when that origin fell off the current chain it is the
           ;; abandoned branch tip redo returns to.
           (let ((origin (nlk::redo-origin sid)))
             (unless origin (reject "noop" "nothing to redo"))
             (setf target origin boundary "turn"))))
        (when (and target (not (nlk:session-known-turn-p sid target)))
          (reject "unknown_turn" "target turn is not in the session's tree"))
        (let* ((parents (nlk:session-composed-parents sid))
               (new-head (cond ((null target) nil)
                               ((string= boundary "user_message")
                                (gethash target parents))
                               (t target)))
               (undone-prompt (and target (string= boundary "user_message")
                                   (turn-started-input-of target)))
               (current-head (nlk:session-head-turn-id sid))
               (redo-p (and new-head
                            (or (null current-head)
                                (gethash current-head (nlk:chain-set-from parents new-head))))))
          (when (equal new-head current-head)
            (reject "noop" "target is already the session head"))
          (when (nlk:active-turn-p sid)
            (reject "turn_active" "a turn is in flight; cancel or await it first"))
          (unless (acquire-turn-latch gws)
            (reject "turn_active" "session worker latch is busy"))
          ;; Latch held from here; the worker owns release.
          (spawn (format nil "navigate-~a" sid)
            (with-cleanup ((bt2:with-lock-held ((gw-session-lock gws))
                             (setf (gw-session-turn-active-p gws) nil)))
              (with-handlers ((error (condition)
                                (result "rejected" "invalid_request" (princ-to-string condition))))
                (execute-navigate-checkpoint peer sid target
                                             new-head boundary
                                             checkpoint-id undone-prompt
                                             redo-p restore-files
                                             command-id correlation-id)))))))))

(defun session-state-json (session-id)
  "SESSION-ID as it stands, each part from its own read model: the session
detail (GATEWAY-SESSION-JSON, as GET /api/gateway/sessions/ID answers), its
title, whether a turn runs (the replay's admission_state), the newest log
position (the cursor a resume names), the target its next turn binds
(GATEWAY-TARGET-JSON), and the prompts parked behind the running turn."
  (json-object
   "session" (if-let (session (nlk:find-session session-id))
               (gateway-session-json session)
               (gateway-standby-session-json session-id))
   "title" (or (nlk::session-title session-id) :null)
   "admission_state" (replay-admission-state session-id)
   "cursor" (session-cursor session-id)
   "target" (gateway-target-json :session-id session-id)
   "pending_prompts" (nlk::queue-prompts session-id)))

(define-sync-command :get-state ()
  ;; A read, answered in the command's own result: what a program asks
  ;; before it acts (Pi's get_state). Any session the operator's store holds
  ;; answers; no subscription is needed to look.
  (:require sid "session_id is required")
  (if (nlk:session-exists-p sid)
      (result "accepted" "ok" "session state"
              (first (nlk::active-turn-row sid))
              (nlk::follow-up-count (nlk::queue-prompts sid))
              nil nil (session-state-json sid))
      (result "rejected" "unknown_session" "session does not exist")))

(define-sync-command :set-session-subscriptions ((subscriptions :array "subscriptions")
                                                 (pid :integer "pid"))
  ;; The decoder (NLK::DECODE-COMMAND) admits this command only with its
  ;; subscriptions array, and carries the pid only when it is an integer. An
  ;; entry names a session; a live_only entry (a preview lane's follow) gets no
  ;; replay and no band snapshot; a rows entry gets the session folded here and
  ;; sent as session_rows instead of its frames (rows.lisp), and stays among
  ;; the peer's subscriptions so its commands are admitted; an entry with a
  ;; cursor, the newest log position its shell holds, is resumed past it
  ;; (SEND-REPLAY-UNDER-LOCK).
  (multiple-value-bind (new-ids live-only rows cursors)
      (loop for entry across subscriptions
            for id = (json-value entry :string "session_id")
            for cursor = (json-value entry :integer "cursor")
            collect (if (hash-table-p entry) (gethash "session_id" entry) entry) into new-ids
            when (and id (json-value entry :boolean "live_only")) collect id into live-only
            when (and id (json-value entry :boolean "rows")) collect id into rows
            when cursor collect (cons id cursor) into cursors
            finally (return (values new-ids live-only rows cursors)))
    (labels ((tell (kind payload)
               (enqueue-peer-frame peer (nlk:encode-unsequenced-frame kind payload)))
             (board (&aux (state (find-gw-peer-state peer)))
               ;; The board follows the first replay, once: what the
               ;; organism's parts said before this shell attached -- a
               ;; :quiet entry was said to no one.
               (unless (or (null state) (shiftf state.board-sent-p t))
                 (bt2:with-lock-held (*standing-notice-publication-lock*)
                   (tell "organism.notices" (standing-notices-payload)))
                 (loop for (key text level) in (nlk:standing-notices)
                       unless (eq level :quiet)
                         do (tell "organism.notice" (notice-payload text level key)))
                 ;; And the note the organism last said while it is fresh
                 ;; (ORGANISM-NOTE): a boot's, in the model's words.
                 (when-let (words (organism-note))
                   (tell "organism.note" (json-object "text" words))))))
      (when-let (state (and pid (find-gw-peer-state peer)))
        (setf state.pid pid))
      (remove-peer (car subscription-list) peer)
      (close-rows-views peer rows)
      (setf (car subscription-list) new-ids)
      (dolist (sid new-ids)
        (let ((gws (find-or-create-gw-session sid)))
          (if (member sid rows :test #'string=)
              (progn (open-rows-view peer sid)
                     (board))
              (bt2:with-lock-held ((gw-session-lock gws))
                (pushnew peer gws.peers)
                (unless (member sid live-only :test #'string=)
                  (send-replay-under-lock gws peer (cdr (assoc sid cursors :test #'equal)))
                  (board)
                  ;; The band is a live-only projection: replay never seeds it.
                  ;; This peer regains the session's still-running shells from
                  ;; the registry authority instead -- one snapshot frame per
                  ;; process carrying the full retained output, which later
                  ;; watcher chunks splice behind by offset. The session lock
                  ;; guarantees replay < snapshots < live watcher frames.
                  (dolist (update (exec-collect-session-snapshots
                                   (exec-registry-instance) sid))
                    (send-frame-under-lock
                     gws peer "session_background_process_updated"
                     (background-process-update-payload update)))
                  ;; The queue is live-only too: a steer parked on the running
                  ;; turn reaches this peer as the queue's frame, not the replay,
                  ;; and so does the end of a follow-up the replay folded as
                  ;; pending that a withdrawal took back -- running or not.
                  (when-let (queue (newest-queue-snapshot sid))
                    (send-frame-under-lock gws peer "session_input_queue_snapshot_updated" queue)))))))
      ;; The running-session map rides the first subscription,
      ;; whole, once the peer is on its sessions so its own reads as
      ;; attached: a shell's copy is its own table, not transcript
      ;; state, so no replay wipes it.
      (send-session-activity-snapshot peer))))

(defun dispatch-command (peer message subscription-list scopes)
  "Decode one typed client command and route it with correlated structured outcomes."
  (with-handlers ((nlk:protocol-error (condition)
                    (enqueue-peer-frame
                     peer (nlk::encode-sync-protocol-error
                           (string-downcase (symbol-name (nlk:protocol-error-code condition)))
                           (nlk:protocol-error-message condition) :permanent nil))))
    (let* ((frame (nlk:decode-sync-message message))
           (command (and (eq :client-command frame.type) frame.payload)))
      (unless (typep command 'nlk:gateway-command)
        (nlk::protocol-refuse :invalid-envelope "post-handshake frame must be a client_command"))
      (let* ((kind command.kind)
             (sid (and (hash-table-p command.request) (gethash "session_id" command.request))))
        (unless (member (if (member kind '(:set-session-subscriptions :get-state))
                            "sync.read"
                            "sync.write")
                        scopes :test #'string=)
          (when (eq kind :set-session-subscriptions)
            (nlk::protocol-refuse :scope-denied "command is not authorized by accepted scopes"))
          (publish-command-result
           peer (or sid "") command.command-id command.correlation-id
           (nlk::command-kind-wire-name kind) "rejected" "scope_denied"
           "command is not authorized by accepted scopes")
          (return-from dispatch-command nil))
        (when-let (handler (gethash kind *sync-command-handlers*))
          (funcall handler peer command.request command.command-id command.correlation-id
                   subscription-list))))))
;;; --- HTTP control surface -------------------------------------------------

(defun gateway-json-response (status object &aux (body (encode-json-octets object)))
  ;; The body leaves as octets, which the server writes as they are: through
  ;; the string encoder a 400 KB history page would cons memory,
  ;; and the server would encode the string to octets again; this writer conses
  ;; about its own size.
  ;; Its length is known, so the reader takes it in one exactly sized read
  ;; instead of chunks into a buffer grown by doubling.
  (list status (list :content-type "application/json" :content-length (length body))
        body))

(defun gateway-error-response (status code message &optional field)
  (gateway-json-response status (json-object "error" (json-object "code" code "message" message
                                                                  :opt "field" field))))

(defun gateway-answer (value)
  "VALUE as a route's response: a response list as it is, a JSON value as a 200's body."
  (if (or (functionp value) (and (consp value) (integerp (first value))))
      value (gateway-json-response 200 value)))

(defun gateway-http-authorized-p (env)
  (let* ((headers (getf env :headers))
         (authorization (and (hash-table-p headers)
                             (or (gethash "authorization" headers)
                                 (gethash "Authorization" headers))))
         (prefix "Bearer "))
    (and (json-value *gateway-operator-token* :text)
         (stringp authorization)
         (string-equal prefix authorization :end2 (min (length prefix) (length authorization)))
         (string= *gateway-operator-token* authorization
                  :start2 (length prefix)))))

(defvar *gateway-routes* (make-hash-table :test #'equal)
  "What MAKE-SYNC-APP dispatches on: route key -> (FUNCTION . LITERALS). A
key is the request path for a path route and the subroute's own name — never
a path, which always leads with a slash — for a session subroute, whose
function takes the session id after the environment.")

(defmacro define-gateway-route (name lambda-list &body body)
  "Define one operator HTTP route."
  ;; Every route shares one entry contract, so a body is only its own logic:
  ;; an unauthorized request is refused before BODY runs, a signalled
  ;; PROTOCOL-ERROR maps through the status table below, and any other error
  ;; becomes a 400 rather than escaping into the server.
  ;;
  ;; Leading clauses, in written order before the body:
  ;;
  ;;   (:on ARM...)            further HANDLER-CASE arms for this route; an ARM
  ;;                           (TYPE STATUS CODE [MESSAGE]) answers TYPE with
  ;;                           STATUS, CODE and MESSAGE, else the condition's words
  ;;   (:route VERB KEY NOUN L...)  the route's own address: KEY is registered in
  ;;                           *GATEWAY-ROUTES* so the dispatcher needs no arm of
  ;;                           its own, the literals L ride after the path
  ;;                           arguments, and a non-NIL VERB is a 405
  ;;                           method_not_allowed unless the request verb is VERB
  ;;                           (or one of a list of them), reported as "NOUN is a
  ;;                           VERB control operation". A route that serves more
  ;;                           than one address declares one clause per address.
  ;;                           A KEY that is no path names a session subroute --
  ;;                           the segments after the session id, a `*' standing
  ;;                           for the one segment handed on after it
  ;;                           (MAKE-SYNC-APP) -- and is a 404 unknown_session
  ;;                           unless its session argument names a session that
  ;;                           exists.
  ;;
  ;; Guards run inside the handler-case, so a store that refuses mid-check
  ;; still maps through the status table rather than escaping.
  ;;
  ;; The first parameter of LAMBDA-LIST is the request environment; the rest
  ;; are path arguments the dispatcher already resolved. A body answers a
  ;; response list, or a JSON value that is the body of a 200 (GATEWAY-ANSWER).
  (let ((env (first lambda-list))
        (documentation (when (and (stringp (first body)) (rest body))
                         (pop body)))
        (extra '())
        (guards '())
        (registrations '()))
    (flet ((verb-guard (verb noun &aux (verbs (uiop:ensure-list verb)))
             `((not (member (getf ,env :request-method) ',verbs))
               (gateway-error-response
                405 "method_not_allowed"
                ,(format nil "~a is a ~{~a~^ or ~} control operation" noun verbs))))
           (session-guard (var)
             `((not (nlk:session-exists-p ,var))
               (gateway-error-response 404 "unknown_session" "session does not exist")))
           (arm (arm)
             (if (integerp (second arm))
                 (destructuring-bind (type status code &optional message) arm
                   `(,type (condition) (declare (ignorable condition))
                      (gateway-error-response ,status ,code
                                              ,(or message '(princ-to-string condition)))))
                 arm)))
      (loop while (and (consp (first body))
                       (member (first (first body)) '(:on :route)))
            do (destructuring-bind (key &rest args) (pop body)
                 (ecase key
                   (:on (setf extra (append extra (mapcar #'arm args))))
                   (:route
                    (destructuring-bind (verb address &optional noun &rest literals) args
                      (when verb (push (verb-guard verb noun) guards))
                      (unless (char= #\/ (char address 0))
                        (push (session-guard (second lambda-list)) guards))
                      (push `(setf (gethash ,address *gateway-routes*)
                                   (list ',name ,@literals))
                            registrations)))))))
    `(progn
       ,@registrations
       (defun ,name ,lambda-list
         ,@(when documentation (list documentation))
         (if (gateway-http-authorized-p ,env)
             (handler-case (gateway-answer (cond ,@(nreverse guards) (t ,@body)))
               ,@extra
               (nlk:protocol-error (condition)
                 ;; One status mapping for every route's protocol-error arm.
                 (gateway-error-response
                  (case (nlk:protocol-error-code condition)
                    (:request_too_large 413)
                    (:unsupported_media_type 415)
                    (t 400))
                  (string-downcase (symbol-name (nlk:protocol-error-code condition)))
                  (nlk:protocol-error-message condition)
                  (nlk:protocol-error-field condition)))
               (nlk:contract-refusal (condition)
                 ;; A route whose body is a declared contract (NLK:DEFINE-CONTRACT)
                 ;; refuses at the boundary, and the refusal already names the
                 ;; member's wire path, what the declaration wanted and what
                 ;; arrived. One arm, so every such route answers the same way.
                 (gateway-error-response 400 "contract_refused"
                                         (princ-to-string condition)))
               (provider-error (condition)
                 ;; A route that touches the selection refuses by the lane's own
                 ;; detail, which names the key to set (providers.<id>.sdk), not
                 ;; by the round-trip prefix PRINC-TO-STRING would add.
                 (gateway-error-response 400 "unknown_provider"
                                         (provider-error-detail condition)))
               (error (condition)
                 (gateway-error-response 400 "invalid_request"
                                         (princ-to-string condition))))
             (gateway-error-response 401 "operator_auth_failed"
                                     "valid Bearer token required"))))))

(defun require-non-empty (fields message &key required allow-null)
  "Refuse the request (PROTOCOL-ERROR :invalid_request carrying MESSAGE)
unless every one of FIELDS is a non-empty string; an absent field (NIL)
passes unless REQUIRED, a JSON null (:NULL) passes when ALLOW-NULL."
  (dolist (field fields)
    (unless (or (and (null field) (not required))
                (and allow-null (eq field :null))
                (and (stringp field) (plusp (length field))))
      (error 'nlk:protocol-error :code :invalid_request :message message))))

(defun require-request (test format-control &rest args)
  "Refuse the request (PROTOCOL-ERROR :invalid_request, FORMAT-CONTROL and
ARGS its message) unless TEST."
  (unless test (apply #'nlk::protocol-refuse :invalid_request format-control args)))

(defun request-media-type (env &aux (type (getf env :content-type)))
  "ENV's declared Content-Type without its parameters, or NIL."
  (and (stringp type) (string-trim " " (subseq type 0 (position #\; type)))))

(defun gateway-request-json (env &aux (length (or (getf env :content-length) 0)))
  (when (> length 65536)
    (nlk::protocol-refuse :request_too_large "HTTP request body exceeds 64 KiB"))
  (when (zerop length)
    (return-from gateway-request-json (make-hash-table :test #'equal)))
  ;; Bodies are parsed only when declared JSON: parsing whatever
  ;; bytes arrive regardless of declaration is a request-smuggling
  ;; and CSRF-adjacent hole, and the Zig gateway refuses it too.
  (unless (string-equal "application/json" (request-media-type env))
    (nlk::protocol-refuse :unsupported_media_type "request body must be application/json"))
  (let ((text (make-string length)))
    (unless (= length (read-sequence text (getf env :raw-body)))
      (nlk::protocol-refuse :truncated_request "HTTP request body ended early"))
    (let ((value (handler-case (nlk:decode-json text)
                   (error (condition)
                     (nlk::protocol-refuse :invalid_json "invalid JSON: ~a" condition)))))
      (require-request (hash-table-p value) "request body must be a JSON object")
      value)))

(defun gateway-session-json (session &aux (anchor (nlk:durable-session-anchor-turn-id session)))
  (json-object
   "session_id" (nlk:durable-session-id session)
   "cwd" (nlk:durable-session-cwd session)
   "parent" (nlk:durable-session-parent session)
   "created_at" (nlk:durable-session-created-at session)
   :opt "anchor_turn_id" anchor
   :when anchor "anchor_log_position"
   (nlk:durable-session-anchor-log-position session)))

(defun gateway-standby-session-json (session-id)
  "The session detail for an id that is standing by: what the mint knows,
and no fork when it named none."
  ;; The directory never draws one: a standby session is not in it — but the
  ;; shell that just minted the id asks for it here, and so does a shell
  ;; re-attaching.
  (let ((record (nlk:standby-session-record session-id)))
    (json-object
     "session_id" session-id
     "cwd" (or (and record (nlk:standby-record-cwd record)) "")
     "parent" (and record (nlk:standby-record-parent record))
     "standby" (and t t))))

(defun gateway-query-parameters (env &aux (uri (or (getf env :request-uri) ""))
                                          (query-start (position #\? uri)))
  "ENV's query string as an ordered list of (NAME . VALUE) conses; a key
written without an `=' carries NIL and an empty part is skipped."
  ;; One scanner
  ;; for every route, so no two of them can disagree about a missing value. The
  ;; only values that ride here are durable ids the gateway itself minted
  ;; (`session-<hex>`) and small integers, so they are read verbatim: no
  ;; percent-decoding lane exists to disagree with the path router's.
  (when query-start
    (loop for part in (uiop:split-string (subseq uri (1+ query-start))
                                         :separator "&")
          for eq-at = (position #\= part)
          when (plusp (length part))
            collect (cons (subseq part 0 eq-at) (and eq-at (subseq part (1+ eq-at)))))))

;;; --- operator history eviction op ----------------------------------------
;;; The automatic budget-fill policy lives in the context engine
;;; (choose-history-boundary); the operator producer of
;;; session.context.evicted is EVICT-SESSION-CONTEXT (context.lisp), shared
;;; with the local TUI slash op. This request-reply route parses, delegates,
;;; and publishes the fact live so every attached peer sees the divider
;;; without waiting for the next turn.

(defun checkpoint-entry-json (entry &aux (user-p (string= (getf entry :kind) "user_message")))
  (json-object
   (:getf entry "kind" "turn_id" "log_position")
   "on_head_chain" (and (getf entry :on-head-chain) t)
   ;; The tree shape (CHECKPOINT-TREE-ORDER): how many lanes
   ;; this row sits inside (0 on the trunk), one boolean per
   ;; lane — open while more lines fork off below — and
   ;; whether this row is where a later line forks off.
   "depth" (or (getf entry :depth) 0)
   "lanes" (map 'vector (lambda (open) (and open t)) (getf entry :lanes))
   "fork" (and (getf entry :fork) t)
   "tree_sha" (getf entry :tree-sha)
   ;; The files-preview seam (Zig checkpoint wire fields):
   ;; FILE_CHANGE_COUNT rides only when the row carries one — the listing
   ;; does not derive counts, so its absence says `ask the files route',
   ;; never `nothing changed' (an absence read as a zero is a claim the
   ;; listing cannot make). CHECKPOINT_ID appears only when a capture
   ;; recorded one — its absence tells the client there is nothing to
   ;; fetch files for.
   (:getf entry :opt "file_change_count" "checkpoint_id")
   ;; The row's span, the instants the log stamped: a turn's TURN.STARTED to
   ;; its terminal event, a call's TOOL_CALL_STARTED to its result. The end
   ;; and the ENDING (`completed', `failed', `cancelled') are absent while it
   ;; runs; STEERS counts the inputs committed into a turn after its first.
   (:getf entry :opt "started_at" "ended_at")
   :opt "ending" (getf entry :ending)
   :when user-p "steers" (or (getf entry :steers) 0)
   :when user-p "parent_turn_id" (getf entry :parent-turn-id)
   ;; The row the picker draws is an EXCERPT of the prompt — its first line
   ;; cut at CHECKPOINT-ROW-EXCERPT's 48 columns — and the confirm stage's
   ;; summary is that same excerpt. Shipping the whole prompt would put every
   ;; pasted document the session ever carried on the wire, and the encode of
   ;; it would cost more than the log read. The client still cuts what it is
   ;; given, so a gateway that sends more is drawn the same.
   :when user-p "prompt" (checkpoint-row-excerpt (getf entry :prompt))
   ;; A reflection's recap says what it said, under the same excerpt.
   :opt "answer" (and (getf entry :answer)
                      (json-value (checkpoint-row-excerpt (getf entry :answer)) :text))
   ;; A turn no one asked names what began it and takes no number: the asks
   ;; are numbered in a row (NLK:SESSION-CHECKPOINTS).
   :opt "origin" (getf entry :origin)
   :when (and user-p (getf entry :turn-number)) "turn_number" (getf entry :turn-number)
   :when user-p "is_head" (and (getf entry :is-head) t)
   :when (not user-p) "tool_call_id" (getf entry :tool-call-id)
   :when (not user-p) "tool_name" (getf entry :tool-name)
   ;; Lisp-only beside the Zig pair: the executable text the call's
   ;; arguments named — the same 48-column excerpt the row draws, for the
   ;; same reason the prompt is excerpted above. Absent, not empty, when
   ;; the call named none — the picker's fallback to the tool name is the
   ;; projection's call.
   ;; Absent, never empty: the picker's `(or command tool-name)'
   ;; fallback reads an empty string as a command, and would draw a
   ;; blank cell for a call that named none.
   :opt "tool_command"
   (and (not user-p) (json-value (checkpoint-row-excerpt (getf entry :tool-command)) :text))))

(define-gateway-route handle-gateway-checkpoints-http (env session-id)
  "GET /api/gateway/sessions/<id>/checkpoints — the tree picker projection."
  ;; Off-chain branches are included (they are the redo surface), marked by
  ;; on_head_chain, and the rows come in tree order with a depth each
  ;; (CHECKPOINT-TREE-ORDER): the first line of turns is the trunk, a turn
  ;; branched from later is a line of its own one lane deeper right under the
  ;; turn it branched from, and each row says which lanes it sits inside so the
  ;; picker can draw the lines.
  (:route :get "checkpoints" "checkpoint listing")
  ;; The Zig route contract: `limit' is the only key this route accepts, and
  ;; a limit it cannot use is a refusal rather than a silent full listing.
  (unless (every (lambda (parameter) (equal "limit" (car parameter)))
                 (gateway-query-parameters env))
    (nlk::protocol-refuse :invalid_request "limit must be a single positive integer"))
  ;; LIMIT keeps the NEWEST entries (the Zig list drops from the front) —
  ;; the picker wants the recent tail — and the read starts from that tail
  ;; (SESSION-CHECKPOINTS :LIMIT) rather than composing every checkpoint the
  ;; session ever made to throw all but 64 away: 446 ms of log scan and
  ;; payload decode on the widest session measured, against 35 ms for the
  ;; same answer read from the tail. The tree order is laid out after the
  ;; cut: a parent the cut removed leaves its children in the root group
  ;; rather than hiding them.
  ;;
  ;; No file-change COUNT is derived here. The counts annotate the rows the
  ;; OPERATOR highlights, and the picker paints a row's files chrome only on
  ;; the selected row — so the pass paid a whole-workspace staging to compute
  ;; 64 numbers, 63 of which were never drawn, on the keystroke the picker
  ;; opens from (measured 213 ms cold, 21 s on a session whose workspace is
  ;; /tmp). The count's owner is the files route below: the path list it
  ;; already answers IS the count, computed once per highlighted row, behind
  ;; the frame.
  (let* ((limit (gateway-query-integer env "limit"))
         (entries (nlk:checkpoint-tree-order
                   (nlk:session-checkpoints session-id :limit limit))))
    (make-json-object
     "session_id" session-id
     "head_turn_id" (or (nlk:session-head-turn-id session-id) "")
     "checkpoints" (coerce (mapcar #'checkpoint-entry-json entries) 'vector))))

(define-gateway-route handle-gateway-checkpoint-files-http (env session-id checkpoint-id)
  "GET /api/gateway/sessions/<id>/checkpoints/<checkpoint-id>/files — the
lazily-fetched files preview for one checkpoint (Zig checkpointFilesPayload)."
  ;; FILES is the worktree-vs-undo-target-tree diff minus the restore engine's
  ;; foreign-write conflict set — only paths a restore would actually touch;
  ;; snapshots unavailable means an empty list rather than a wrong one; an
  ;; unknown checkpoint is a 404.
  (:route :get "checkpoints/*/files" "checkpoint files listing")
  (multiple-value-bind (files known)
      (nlk:checkpoint-changed-files session-id checkpoint-id)
    (if (not known)
        (gateway-error-response 404 "unknown_checkpoint"
                                "checkpoint does not exist")
        (make-json-object
         "session_id" session-id
         "checkpoint_id" checkpoint-id
         "files" (coerce files 'vector)))))

(define-gateway-route handle-gateway-image-http (env session-id key)
  "GET /api/gateway/sessions/<id>/images/<key> — the picture an image fact of
the session names, as its bytes: KEY is the call whose result carries the fact
(a LOOK's), or the id of a picture a message shows -- one an answer names
(exec.lisp ANSWER-IMAGES) or one sent with a prompt (PROMPT-IMAGES). A page
cannot open the path the fact carries, where a terminal reads it itself."
  ;; The fact names a file and never holds bytes (exec.lisp LOOK), so this
  ;; reads the file as it is now, the moment the terminal would draw it; the
  ;; file gone, the PNG a terminal was given (a transcode in the cache) stands
  ;; in. A message's fact names the copy kept of its picture's bytes as well,
  ;; and that is served first: what the message showed, whatever the file
  ;; holds now. Only a path some image fact names is ever served.
  (:route :get "images/*" "image")
  (let* ((image (or (json-value (nlk:events :session-id session-id :kind nlk::+kind-tool-result+
                                            :where (list "json_extract(payload, '$.call-id') = ?" key)
                                            :order :newest :as :payload)
                                :object "metadata" "image")
                    (find key (json-array (nlk:events :session-id session-id
                                                      :kind (list nlk::+kind-input-committed+
                                                                  nlk::+kind-assistant-completed+)
                                                      :where (list "exists (select 1 from json_each(payload, '$.metadata.images') where json_extract(value, '$.id') = ?)" key)
                                                      :order :newest :as :payload)
                                          "metadata" "images")
                          :key (lambda (image) (json-value image :text "id")) :test #'equal)))
         (original (some (lambda (key) (when-let (path (json-value image :text key)) (probe-file path)))
                         '("copy_path" "path")))
         (file (or original (when-let (png (json-value image :text "png_path")) (probe-file png)))))
    (if file
        (list 200 (list :content-type (if original (json-value image :text "media_type") "image/png")
                        :x-content-type-options "nosniff" :cache-control "private, no-cache")
              file)
        (gateway-error-response 404 "no_image" "no image fact of that name has a picture still on disk"))))

(define-gateway-route handle-gateway-evict-http (env session-id)
  (:route :post "evict" "history eviction")
  (if (nlk:active-turn-p session-id)
      ;; Folding history under a streaming turn would compute the
      ;; eviction boundary from a moving log; the Zig wire code,
      ;; verbatim (http.zig EVICT_TURN_IN_PROGRESS_BODY).
      (gateway-error-response 409 "turn_in_progress"
                              "a turn is in flight; cancel or await it first")
      ;; The Zig parseSessionEvictRequest contract, strict: absent or JSON
      ;; null is the default (NIL), JSON false refuses — so GETHASH, never
      ;; the lenient JSON-VALUE, which folds the two together.
      (bind ((request (gateway-request-json env))
             ((value present) (gethash "percent" request))
             (percent (cond
                        ;; shasht reads JSON null back as the truthy keyword :NULL.
                        ((or (not present) (eq value :null)) nil)
                        ((and (integerp value) (< 0 value 101)) value)
                        (t (error 'nlk:protocol-error :code :invalid_request
                                  :message "percent must be an integer between 1 and 100"))))
             ;; THROUGH, a turn id, evicts every retained turn up to it instead.
             (through (bind (((value present) (gethash "through" request)))
                        (cond ((or (not present) (eq value :null)) nil)
                              ((and (stringp value) (plusp (length value))) value)
                              (t (error 'nlk:protocol-error :code :invalid_request
                                        :message "through must be a turn id")))))
             (pre-cursor (session-cursor session-id))
             ;; The one operator eviction seam (context.lisp): it plans against
             ;; the head-visible fold cut at the existing floor and persists the
             ;; boundary advance; this route only parses, publishes, and formats.
             ((plan floor event-id source-watermark)
              (evict-session-context session-id :percent percent :through through)))
        (cond ((null plan)
               (gateway-error-response 409 "no_history_to_evict"
                                       "the session has no retained history to evict"))
              (t
               (publish-turn-facts session-id pre-cursor)
               (plist-object plan '(:requested-percent :realized-percent :evicted-turn-count
                                    :evicted-message-count :retained-message-count
                                    :evicted-start-sequence :evicted-end-sequence
                                    :evicted-estimated-tokens :retained-estimated-tokens)
                             "session_id" session-id
                             "event_id" event-id
                             "retained_from_sequence" floor
                             "source_watermark" source-watermark))))))

;;; --- model control op ------------------------------------------------------
;;; Provider identity is process configuration materialized in provider.lisp
;;; globals; this request-reply op mutates it and SELECT-DEFAULT-MODEL persists
;;; it. Turns already admitted keep their frozen config; the next compile sees
;;; the change.
;;;
;;; The durable authority is general.default_provider + general.default_model in
;;; the shared operator config, and nothing else. There used to be a second one
;;; -- plain gateway_config rows in the store -- seeded from config at boot and
;;; then allowed to overwrite it, while only THIS route wrote them. A pick made
;;; anywhere else landed in config and was then discarded on the next boot by a
;;; row nobody had updated. One authority, written by every surface.

(defun gateway-auxiliary-model-json (&optional outcome detail)
  "The auxiliary target as the wire says it: the configured pin with its
effective effort (AUXILIARY-MODEL) and the rungs its model offers (EFFORTS,
weakest first: what /models asks for after an auxiliary pick), or provider
and model NULL when none is configured — side work then runs on each
session's own target."
  (multiple-value-bind (provider model effort configured)
      (auxiliary-model)
    (json-object "provider" (if configured provider :null)
                 "model" (if configured model :null)
                 "effort" (if (and configured effort) effort :null)
                 "efforts" (coerce (and configured (effort-ladder provider model)) 'vector)
                 :opt "shared_config" (and outcome (string-downcase outcome))
                 :opt "shared_config_error" detail)))

(defun gateway-decider-json (&optional outcome detail)
  "The decider general.decider_model picks (DECIDER-MODEL) as the wire says
it, provider and model NULL when none is, and every decider /models offers."
  (multiple-value-bind (id model base) (decider-model)
    (json-object "provider" (or id :null)
                 "model" (or model :null)
                 "base_url" (or base :null)
                 "deciders" (gateway-deciders-json)
                 :opt "shared_config" (and outcome (string-downcase outcome))
                 :opt "shared_config_error" detail)))

(defun gateway-deciders-json ()
  "DECIDER-CHOICES as the wire's array: id, name, model, base_url, local."
  (map 'vector (lambda (row)
                 (json-object "id" (getf row :id) "name" (getf row :name)
                              "model" (getf row :model) "base_url" (getf row :base)
                              "local" (local-endpoint-p (getf row :base))))
       (decider-choices)))

(defun gateway-model-json (&optional outcome detail &aux (refusal nil)
                                                         (endpoint (handler-case (effective-endpoint)
                                                                     (provider-config-error (condition)
                                                                       (setf refusal (provider-error-detail condition))
                                                                       :null))))
  (json-object "provider" *provider*
               "model" *model*
               ;; A selection with no address (PROVIDER-CONFIG-ERROR) is still
               ;; the selection: the page keeps the picker that can move it,
               ;; and says why this one cannot serve.
               "endpoint" endpoint
               "refusal" (or refusal :null)
               ;; The auxiliary target rides along so a shell's /models
               ;; summary can name it without a second dial.
               "auxiliary" (gateway-auxiliary-model-json)
               ;; Config is the only durability now, so a write that did not
               ;; land has to be visible rather than warned about into a log
               ;; nobody is reading.
               :opt "shared_config" (and outcome (string-downcase outcome))
               :opt "shared_config_error" detail))

(defun restore-model-config ()
  "Boot lane: the shared config's general defaults seed the selection, under
the operator's NODECODE_* overrides (the providers/root.zig ladder:
explicit override > general default)."
  ;; A configured default is seeded whether or not its lane resolves — the
  ;; snapshot's `refusal` then tells every shell why it cannot serve — and
  ;; only an unreadable config warns rather than failing the boot: config
  ;; restore is not a store-open precondition.
  (handler-case (seed-model-config-from-shared-config)
    (error (e)
      (warn "gateway model config restore failed: ~a" e))))

;; Runs at every OPEN-STORE; a named function under PUSHNEW so image reloads
;; do not stack duplicates. It reads no store state -- this is simply the one
;; boot seam every entry point already passes through.
(pushnew 'restore-model-config nlk::*store-open-hooks*)

(define-gateway-route handle-gateway-frequent-models-http (env)
  "GET /api/gateway/models/frequent — retained operator turn usage ranked
by frequency, then recency; no second model history is stored."
  (:route :get "/api/gateway/models/frequent" "frequently used models")
  (make-json-object "models" (frequent-models-json)))

(define-gateway-route handle-gateway-models-http (env)
  "GET /api/gateway/models — the models a turn can run on, the /models picker's
own list (MODEL-CHOICES), for a client that cannot read this home's
caches: each (provider, model) with its name and context budget when known,
each provider whose listing failed with why, and each provider this image
drives (connected: CONNECTED-PROVIDER-IDS, listable: LISTABLE-PROVIDER-IDS)
and the default model's, each with why no turn can run on it when none can
(SELECTION-REFUSAL), and each provider a folder not installed here serves
(PROVIDER-OFFERS), which picking installs -- the facts an attached shell
reads, since a cell's lanes and inventory hooks run here only."
  (:route :get "/api/gateway/models" "model catalog")
  (loop with connected = (connected-provider-ids)
        with listable = (listable-provider-ids)
        ;; The pin's provider, connected or not: a shell's landing screen and
        ;; walk ask after it, a typo there included (vr-101).
        with pinned = (shared-model-pin)
        for row in (model-choices)
        if (getf row :placeholder)
          collect (json-object "provider" (getf row :provider)
                               "reason" (princ-to-string (getf row :placeholder))) into unlisted
        else collect (json-object (:getf row "provider" "model")
                                  (:getf row :opt "name")
                                  :opt "context" (getf row :ctx)) into models
        finally (return (make-json-object
                         "models" (coerce models 'vector)
                         "unlisted" (coerce unlisted 'vector)
                         "providers"
                         (coerce (loop for id in (sort (copy-list (union (union connected listable
                                                                                :test #'string-equal)
                                                                         (and pinned (list pinned))
                                                                         :test #'string-equal))
                                                       #'string-lessp)
                                       for refusal = (ignore-errors (selection-refusal id))
                                       collect (json-object
                                                "id" id
                                                "connected" (and (member id connected :test #'string-equal) t)
                                                "listable" (and (member id listable :test #'string-equal) t)
                                                ;; Served from a machine of the operator's own (/models' Local).
                                                "local" (and (local-endpoint-p (provider-base-url id)) t)
                                                ;; Whether a turn can run on it here, and why not
                                                ;; in the words that say what to do.
                                                "ready" (not refusal)
                                                :opt "refusal" refusal))
                                 'vector)
                         "offers" (map 'vector
                                       (lambda (offer)
                                         (json-object "cell" (getf offer :cell)
                                                      :opt "commit" (getf offer :commit)
                                                      "provider" (getf offer :provider)
                                                      "name" (getf offer :name)
                                                      "sign_in" (getf offer :sign-in)
                                                      "description" (getf offer :description)
                                                      :opt "installed" (getf offer :installed)))
                                       (ignore-errors (provider-offers)))))))

;;; --- slash commands ---------------------------------------------------------
;;; The catalog a channel publishes as its platform's command menu, one line
;;; run the way a channel runs it (SLASH), and what an argument completes to,
;;; for a client with no image of its own -- the web page's `/' menu -- and
;;; for a shell in another process, which runs its builtins itself and a
;;; cell's commands here, where the cell runs. A line
;;; rides the body, as a search does: the query scanner reads values verbatim.

(define-gateway-route handle-gateway-slash-http (env)
  "GET /api/gateway/slash — the commands a client can run, in /help order
(SLASH-CATALOG), each saying whether it acts on a session; POST {line,
session_id?} runs one against that session and answers {text, dialog?}, what a
shell would show: the line, and the panel a command answers with. A failure is
the verb's own 400."
  (:route (:get :post) "/api/gateway/slash" "slash")
  (case (getf env :request-method)
    (:get (make-json-object
           "commands" (map 'vector
                           (lambda (entry)
                             (json-object (:getf entry "name" "description" "usage" "session")
                                          (:getf entry :opt "autocomplete" "trigger")))
                           (slash-catalog))))
    (:post (let* ((request (gateway-request-json env))
                  (line (gethash "line" request)))
             (require-non-empty (list line) "line must be a non-empty string" :required t)
             (multiple-value-bind (text dialog)
                 (slash line :session-id (json-value request :string "session_id"))
               (json-object "text" text :opt "dialog" dialog))))))

(define-gateway-route handle-gateway-slash-completions-http (env)
  "POST /api/gateway/slash/completions {name, text, session_id?} — what
NAME's argument completes to from the TEXT typed so far, in the session the
command would act on (SLASH-COMPLETIONS): {choices: [{name, value}]}, empty
for a command that offers none."
  (:route :post "/api/gateway/slash/completions" "slash completions")
  (let* ((request (gateway-request-json env))
         (name (gethash "name" request)))
    (require-non-empty (list name) "name must be a non-empty string" :required t)
    (make-json-object
     "choices" (map 'vector (lambda (choice) (json-object (:getf choice "name" "value")))
                    (slash-completions name (json-value request :string "text")
                                       :session-id (json-value request :string "session_id"))))))

(define-gateway-route handle-gateway-model-http (env)
  (:route (:get :post) "/api/gateway/model" "model")
  (case (getf env :request-method)
    (:get (gateway-model-json))
    (:post
     (let* ((request (gateway-request-json env))
            (provider (gethash "provider" request))
            (model (gethash "model" request))
            (endpoint (gethash "endpoint" request)))
       (require-non-empty (list provider model endpoint)
                          "provider, model, and endpoint must be non-empty strings")
       (unless (or provider model endpoint)
         (nlk::protocol-refuse :invalid_request "at least one of provider, model, endpoint is required"))
       ;; The lane check, the globals, an explicit endpoint and the config
       ;; write are all SELECT-DEFAULT-MODEL's: an unknown lane is refused here,
       ;; not at the next turn, and the pair reaches config together or not at
       ;; all.
       (bind (((_ _ outcome detail) (select-default-model :provider provider :model model :endpoint endpoint)))
         (when (eq outcome :failed)
           (warn "shared config not updated: ~a" detail))
         ;; The default moved: every attached shell whose session follows
         ;; it is rendering stale facts now.
         (broadcast-model-changed)
         (gateway-model-json outcome detail))))))

(define-gateway-route handle-gateway-auxiliary-model-http (env)
  "GET /api/gateway/auxiliary-model answers the auxiliary target; POST
{model, provider?, effort?} lands one the way SELECT-AUXILIARY-MODEL does —
this image's target and general.auxiliary_model together — refusing an
unknown lane or a rung its model does not offer before anything moves;
{model: null} takes it out of both, and effort null is the lightest rung."
  ;; No session is pinned: side work is nobody's turn, so no
  ;; session_model_selected fact and no footer moves.
  (:route (:get :post) "/api/gateway/auxiliary-model" "auxiliary model")
  (:on (effort-refusal 400 "effort_unsupported" (effort-refusal-detail condition)))
  (case (getf env :request-method)
    (:get (gateway-auxiliary-model-json))
    (:post
     (let* ((request (gateway-request-json env))
            (provider (gethash "provider" request))
            (model (gethash "model" request)))
       (require-non-empty (list provider model)
                          "provider and model must be non-empty strings"
                          :allow-null t)
       (require-request model "model is required")
       (multiple-value-bind (effort effort-p) (gethash "effort" request)
         (require-request (or (null effort) (eq effort :null) (stringp effort))
                          "effort must be a string or null")
         (bind (((_ _ outcome detail)
                 (apply #'select-auxiliary-model (and (stringp model) model)
                        :provider (and (stringp provider) provider)
                        (and effort-p (list :effort (and (stringp effort) effort))))))
           (when (eq outcome :failed)
             (warn "shared config not updated: ~a" detail))
           (gateway-auxiliary-model-json outcome detail)))))))

(define-gateway-route handle-gateway-decider-model-http (env)
  "GET /api/gateway/decider-model answers the decider general.decider_model
picks and the deciders /models offers; POST {provider, model?} pins one
(SELECT-DECIDER-MODEL), refusing an id no decider has before anything
moves; {provider: null} takes the pin out."
  (:route (:get :post) "/api/gateway/decider-model" "decider model")
  (case (getf env :request-method)
    (:get (gateway-decider-json))
    (:post
     (let* ((request (gateway-request-json env))
            (provider (gethash "provider" request))
            (model (gethash "model" request)))
       (require-non-empty (list provider model) "provider and model must be non-empty strings"
                          :allow-null t)
       (require-request provider "provider is required")
       (bind (((_ _ outcome detail)
               (select-decider-model (and (stringp provider) provider)
                                     :model (and (stringp model) model))))
         (when (eq outcome :failed)
           (warn "shared config not updated: ~a" detail))
         (gateway-decider-json outcome detail))))))

(defun gateway-failover-json (&optional outcome detail)
  "The failover chain as the wire says it -- each backup in order with what
history says of it (FAILOVER-RECORD) -- and every trigger with whether it is on."
  (let ((on (failover-triggers)))
    (json-object
     "chain" (map 'vector
                  (lambda (entry)
                    (destructuring-bind (&key provider model) entry
                      (destructuring-bind (&key answered-at tried rescued) (failover-record provider model)
                        (json-object "provider" provider "model" model
                                     "answered_at" (or answered-at :null)
                                     "tried" tried "rescued" rescued))))
                  (failover-chain))
     "triggers" (map 'vector
                     (lambda (trigger)
                       (json-object "id" (car trigger) "label" (cdr trigger)
                                    "on" (if (member (car trigger) on :test #'string=) t :false)))
                     +failover-triggers+)
     :opt "shared_config" (and outcome (string-downcase outcome))
     :opt "shared_config_error" detail)))

(define-gateway-route handle-gateway-failover-http (env)
  "GET /api/gateway/failover answers the failover chain every session and room
share, each backup with what history says of it, and the triggers; POST
{chain?: [{provider, model}], on?: [trigger id]} writes either through
SET-FAILOVER-CHAIN and SET-FAILOVER-TRIGGERS, refusing a malformed entry or an
id no trigger has before anything moves."
  (:route (:get :post) "/api/gateway/failover" "model failover")
  (case (getf env :request-method)
    (:get (gateway-failover-json))
    (:post
     (let* ((request (gateway-request-json env))
            (chain (gethash "chain" request))
            (on (gethash "on" request)))
       (require-request (or chain on) "chain or on is required")
       (when chain
         (require-request (and (vectorp chain)
                               (every (lambda (entry)
                                        (and (hash-table-p entry)
                                             (every (lambda (key)
                                                      (let ((value (gethash key entry)))
                                                        (and (stringp value) (plusp (length (nlk:trimmed value))))))
                                                    '("provider" "model"))))
                                      chain))
                          "chain must be an array of {provider, model}"))
       (when on
         (require-request (and (vectorp on)
                               (every (lambda (id) (and (stringp id) (assoc id +failover-triggers+ :test #'string=)))
                                      on))
                          "on must be an array of trigger ids: ~{~a~^, ~}" (mapcar #'car +failover-triggers+)))
       (let ((outcome :written) (detail nil))
         (flet ((wrote (written why)
                  (unless (eq written :written) (setf outcome written detail why))))
           (when chain
             (multiple-value-call #'wrote
               (set-failover-chain (map 'list (lambda (entry)
                                                (list (nlk:trimmed (gethash "provider" entry))
                                                      (nlk:trimmed (gethash "model" entry))))
                                        chain))))
           (when on
             (multiple-value-call #'wrote (set-failover-triggers (coerce on 'list)))))
         (when (eq outcome :failed)
           (warn "shared config not updated: ~a" detail))
         (gateway-failover-json outcome detail))))))

(define-gateway-route handle-gateway-failover-test-http (env)
  "POST /api/gateway/failover/test asks each backup once (PROBE-MODEL: no
retry, no chain, no turn's facts) and answers what each said, in chain order."
  (:route :post "/api/gateway/failover/test" "model failover test")
  (json-object "results"
               (map 'vector
                    (lambda (entry)
                      (destructuring-bind (&key provider model) entry
                        (multiple-value-bind (ok reason seconds) (probe-model provider model)
                          (json-object "provider" provider "model" model
                                       "ok" (if ok t :false)
                                       "reason" (or reason :null)
                                       "seconds" seconds))))
                    (failover-chain))))

(define-gateway-route handle-gateway-provider-models-refresh-http (env)
  "POST /api/gateway/provider-models/refresh {ids} fetches those providers'
listings into the shared cache file (REFRESH-PROVIDER-MODELS) and answers
the ids it wrote."
  ;; The fetch runs in this image, not in the shell that asked: this is the
  ;; image whose cells are started, so a hook on LIST-PROVIDER-MODELS, or on
  ;; the credential a listing sends, runs for every listing. An attached shell
  ;; loads the folder and starts nothing; it reads the file this writes.
  (:route :post "/api/gateway/provider-models/refresh" "provider listing refresh")
  (let ((ids (json-value (gateway-request-json env) :array "ids")))
    (unless (and ids (plusp (length ids)) (every #'stringp ids))
      (nlk::protocol-refuse :invalid_request "ids must be a non-empty array of provider ids"))
    (json-object "refreshed" (coerce (refresh-provider-models (coerce ids 'list)) 'vector))))

;;; --- the target a turn binds -------------------------------------------------
;;; What a turn on a session would run on: the session's pin over the gateway's
;;; process-effective (provider, model), field by field, with the reasoning
;;; effort it binds and the ladder that target admits (EFFORT-LADDER). An
;;; effort pick is carried on the session's model selection fact
;;; (NLK:RECORD-SESSION-MODEL-SELECTION), so it survives a restart and a
;;; later pick elsewhere, which moves only the default, does not move it; a
;;; session that picked none runs on the default effort (EFFECTIVE-EFFORT),
;;; as RUN binds it for each turn. GET /api/gateway/target answers it; a
;;; shell's footer, its /think picker and the web page's model chip read it.

(defun gateway-target (session-id)
  "=> (values PROVIDER MODEL PIN-PROVIDER PIN-MODEL UNCONFIGURED EFFORT):
the identity a turn on SESSION-ID would bind, which is RUN's selection
overlay — the durable per-session pin over the gateway's process-effective
default, field by field — and the effort the session picked."
  ;; A NIL SESSION-ID (or a closed store) resolves the gateway's own target
  ;; with no pin. UNCONFIGURED is true when neither a pin nor the
  ;; configuration chose that identity (DEFAULT-MODEL-CONFIGURED-P): it is the
  ;; image's build-time default, which no surface should present as a model.
  ;; EFFORT is the pick as stored; ADMITTED-EFFORT says whether the target
  ;; takes it.
  (multiple-value-bind (pin-provider pin-model)
      (if (and session-id (nlk:store-open-p))
          (nlk:session-model-selection session-id)
          (values nil nil))
    (values (or pin-provider *provider*)
            (or pin-model *model*)
            pin-provider
            pin-model
            (not (or pin-provider pin-model (default-model-configured-p)))
            (and session-id (nlk:session-model-effort session-id)))))

(defun gateway-target-json (&key session-id aim-provider aim-model)
  "The target SESSION-ID's next turn binds, as GET /api/gateway/target
answers it — the gateway default's when SESSION-ID is NIL, or, with
AIM-PROVIDER or AIM-MODEL and no session, that target over the gateway's."
  ;; The aim is a pick a client holds for a session not yet opened (the web
  ;; opener): the ladder it offers is that model's, not the default's, and
  ;; nothing is pinned until the session exists.
  (multiple-value-bind (provider model pin-provider pin-model unconfigured effort)
      (gateway-target session-id)
    (when (and (null session-id) (or aim-provider aim-model))
      (setf provider (or aim-provider provider)
            model (or aim-model model)
            unconfigured nil))
    (json-object
     "provider" provider
     "model" model
     ;; Nothing chose this target: a shell says how to get a model in its
     ;; place rather than naming it.
     "unconfigured" (if unconfigured t :false)
     ;; What a session that follows the default runs on: a client names it
     ;; beside a pick, so the pick's scope is never a guess.
     "default" (if (default-model-configured-p)
                   (json-object "provider" *provider* "model" *model*)
                   :null)
     ;; The session's own pin; NULL when it follows the default.
     "pin" (if (or pin-provider pin-model)
               (json-object "provider" (or pin-provider :null) "model" (or pin-model :null))
               :null)
     ;; The effort a turn here binds — the session's pick while this target
     ;; admits it, else the default — and the rungs it admits, weakest
     ;; first: empty for a model that does not reason.
     "effort" (or (effective-effort effort provider model) :null)
     "efforts" (coerce (effort-ladder provider model) 'vector)
     ;; Why the target cannot serve a turn, when it cannot (an entry
     ;; naming no sdk), else why the config's own pin was not applied
     ;; (the retired flat keys, a pin missing its lane): the attached
     ;; shell shows it the moment the facts land, instead of learning it
     ;; from the first turn's failure or from a footer naming a model
     ;; the file never chose.
     "refusal" (or (selection-refusal provider) *model-config-refusal* :null))))

(define-gateway-route handle-gateway-target-http (env)
  "GET /api/gateway/target — the target a turn would bind and its effort
(GATEWAY-TARGET-JSON): ?session_id= scopes it to that session; without one,
?provider= and ?model= aim it at a session not yet opened."
  (:route :get "/api/gateway/target" "turn target")
  (let ((query (lambda (name)
                 (json-value (cdr (assoc name (gateway-query-parameters env) :test #'string=))
                             :text))))
    (gateway-target-json :session-id (funcall query "session_id")
                         :aim-provider (funcall query "provider")
                         :aim-model (funcall query "model"))))

(define-gateway-route handle-gateway-prompts-http (env)
  "GET /api/gateway/prompts — every prompt this store committed to a turn,
newest first, each with the lane it arrived on (PROMPT-HISTORY): what a
shell's history dialog opens on."
  (:route :get "/api/gateway/prompts" "prompt history")
  (json-object "prompts" (coerce (loop for (lane text) in (prompt-history)
                                       collect (json-object "lane" lane "text" text))
                                 'vector)))

;;; --- cell slash commands ---------------------------------------------------
;;; A cell's slash command is a catalog entry like a builtin's whose HEADLESS
;;; arm runs it, so it runs the way every command runs: through SLASH, which
;;; POST /api/gateway/slash serves to a shell in another process, the web page
;;; and a channel's room alike, and SLASH-CATALOG lists it beside the builtins.
;;; Its answer is the text every caller shows, and the panel a shell opens when
;;; the answer carries one: the cell describes the list as data, the shell
;;; paints it, and a headless caller reads the text and never notices.

(defvar *registered-commands* '()
  "The slash commands cells registered (REGISTER-COMMAND), oldest first:
SLASH-COMMAND entries whose HEADLESS arm runs them and whose OWNER is the key
that registered them.")

(defun list-dialog (&key title (context "") rows tabs picture (searchable nil)
                         (search-placeholder "") (empty-label "")
                         no-match-label hint summary summary-tone tone)
  "A docked picker a command's answer carries: TITLE the tab
label, CONTEXT the muted label beside it, SEARCHABLE the type-to-filter
input (its placeholder is SEARCH-PLACEHOLDER), EMPTY-LABEL the body's copy
with no rows and NO-MATCH-LABEL its copy under a live query that hid every
row, HINT the footer line where the default copy will not do, SUMMARY the
line under the title of a dialog that does not search, in SUMMARY-TONE
(muted by default), TONE the state the whole dialog reports -- a theme tone
a surface that draws one wears (a room's card its edge), NIL for none."
  ;;
  ;; ROWS are plists in PICKER-ROW's own vocabulary --
  ;; (:key :label :detail :suffix :value :mark :mark-tone :verb): LABEL the
  ;; primary column, DETAIL the muted one after it, SUFFIX what the selected
  ;; row alone adds after DETAIL, VALUE what the shell's Enter hands back, VERB
  ;; the word a surface that gives the row its own button writes on it --
  ;; Leave, Change -- where the dialog's one Enter word will not do, KEY
  ;; a leading column -- or, in a tabbed dialog, the bucket the row belongs to --
  ;; and MARK a one-glyph status column painted in the theme's TONE. OPTIONS,
  ;; rows of their own (:label :detail :mark :run), are the picks the row's
  ;; line offers, the one in use marked: a surface that draws a menu draws
  ;; them as the row's, and one that does not runs the row's line. A row's
  ;; :RUN, (:enter LINE :space LINE), names the slash lines its keys run in
  ;; VALUE's place, where the command ran: a dialog the line answers opens
  ;; over this one, whose Esc comes back here, and otherwise its text is the
  ;; notice and this dialog is read again on the same row (the shell's
  ;; ANSWER-GESTURE) -- a list whose rows open their record and move it.
  ;;
  ;; TABS are the dialog's tab strip as plists (:label the tab's own text,
  ;; :count the rows behind it, :value the row KEY it selects — NIL for the
  ;; tab that filters nothing). The shell paints the strip in the tab row, Tab
  ;; walks it, and the rows the selected tab shows are the ones whose KEY is
  ;; its VALUE, so a bucket every row would repeat once reads once instead. A
  ;; dialog with no TABS is the plain one-list picker.
  ;;
  ;; PICTURE is a small image the shell paints above the rows, as a plist:
  ;; :ROWS, one string a row of pixels, `1' ink and `0' paper -- a QR code, a
  ;; chart -- and :LABEL, what the shell says in its place when the window
  ;; cannot hold it whole. The shell draws paper light and ink dark whatever
  ;; its theme, so a code a camera reads reads in either. A :PALETTE, a list
  ;; of theme tone names, makes each pixel the digit (base 36) of the tone it
  ;; paints in instead; an empty name, or none, is the panel's own ground.
  ;; :BESIDE stands the picture left of the rows, where the panel is wide
  ;; enough for both, instead of over them. :STRIP is the picture as one
  ;; string of its palette's digits, for a surface with no room to paint the
  ;; rows (a room's card).
  ;;
  ;; The answer a shell paints; the same command's text is what a room, a cron
  ;; job or a test reads. REGISTER-COMMAND encodes it through DIALOG-JSON for
  ;; the answer's JSON.
  (list :title (or title "") :context (or context "")
        :searchable (and searchable t)
        :search-placeholder (or search-placeholder "")
        :empty-label (or empty-label "")
        :no-match-label no-match-label
        :hint hint :summary summary :summary-tone summary-tone :tone tone
        :tabs tabs :picture picture :rows rows))

(defun dialog-json (dialog)
  "DIALOG (LIST-DIALOG) as the JSON object an answer carries."
  ;; A member the spec leaves NIL is absent rather than JSON false: an absent
  ;; hint and a false one read the same, and the reader's defaults live on its
  ;; side.
  (flet ((tone (name) (and name (string-downcase (string name)))))
    (json-object
     (:getf dialog "title" "context")
     "searchable" (and (getf dialog :searchable) t)
     (:getf dialog "search_placeholder" "empty_label")
     (:getf dialog :opt "no_match_label" "hint" "summary")
     :opt "summary_tone" (tone (getf dialog :summary-tone))
     :opt "tone" (tone (getf dialog :tone))
     :opt "tabs" (when-let (tabs (getf dialog :tabs))
                   (map 'vector
                        (lambda (tab)
                          (json-object
                           "label" (or (getf tab :label) "")
                           (:getf tab :opt "count" "value")))
                        tabs))
     :opt "picture" (when-let (picture (getf dialog :picture))
                      (json-object "rows" (coerce (getf picture :rows) 'vector)
                                   "label" (or (getf picture :label) "")
                                   :opt "palette" (and (getf picture :palette)
                                                       (coerce (getf picture :palette) 'vector))
                                   :opt "beside" (and (getf picture :beside) t)
                                   :opt "strip" (getf picture :strip)))
     "rows" (map 'vector
                 (lambda (row)
                   (json-object
                    "key" (or (getf row :key) "")
                    "label" (or (getf row :label) "")
                    "detail" (or (getf row :detail) "")
                    (:getf row :opt "suffix" "value" "verb")
                    :opt "run" (when-let (run (getf row :run))
                                 (json-object (:getf run :opt "enter" "space")))
                    "mark" (or (getf row :mark) "")
                    :opt "mark_tone" (tone (getf row :mark-tone))
                    :opt "options" (when-let (options (getf row :options))
                                     (map 'vector
                                          (lambda (option)
                                            (json-object
                                             "label" (or (getf option :label) "")
                                             "detail" (or (getf option :detail) "")
                                             "mark" (or (getf option :mark) "")
                                             :opt "run" (when-let (run (getf option :run))
                                                          (json-object (:getf run :opt "enter" "space")))))
                                          options))))
                 (getf dialog :rows)))))

(defun register-command (owner command run &key description argument-hint
                                                   (label command) (catches 'error)
                                                   (session t) trigger complete)
  "Put the slash command COMMAND, which RUN answers, in the catalog under the
key OWNER, replacing any command of that name; SESSION NIL says it acts on no
session, so a client with none open runs it as it is, TRIGGER, a
one-character string, is what the shell's composer opens it on when it is
typed at a word's start (the index's `$'), and COMPLETE completes the
argument while it is typed (SLASH-COMPLETIONS): it takes the tail typed so
far and the session id, as RUN takes its argument, and answers (:name
:value) plists. => COMMAND's entry."
  ;; RUN is a function designator, so a command whose whole implementation is
  ;; one defun passes the symbol and a reload of that file reaches the command
  ;; without a re-install. RUN takes the command's argument string and the
  ;; session id and answers (values TEXT DIALOG): TEXT what every caller shows,
  ;; DIALOG a LIST-DIALOG the shell that ran the command opens as its docked
  ;; picker. A CATCHES condition answers "LABEL: it"; any other keeps going up,
  ;; to the caller's own refusal.
  (check-type owner string)
  (unless (json-value command :text)
    (error "a registered command needs a non-empty name"))
  (let ((entry (make-slash-command
                command (or description "")
                :usage (if (json-value argument-hint :text)
                           (format nil "/~a ~a" command argument-hint)
                           "")
                :dispatch nil :owner owner :session session :trigger trigger
                :completer complete
                :headless (lambda (args session-id)
                            (multiple-value-bind (text dialog)
                                (handler-case (funcall run args session-id)
                                  (error (it)
                                    (if (typep it catches)
                                        (format nil "~a: ~a" label it)
                                        (error it))))
                              (values (or text (format nil "/~a done" command))
                                      (and dialog (dialog-json dialog))))))))
    (setf *registered-commands*
          (append (remove command *registered-commands*
                          :key #'slash-command-name :test #'string-equal)
                  (list entry)))
    entry))

(defun unregister-commands (owner &aux (before (length *registered-commands*)))
  "Take every command OWNER registered out of the catalog -- the cell that
registered them is stopping. => how many went."
  ;; The catalog every attached shell holds is refreshed by the lifecycle
  ;; (START-CELLS / STOP-CELLS / RESTART-CELLS), not here: a stop that
  ;; removes three commands pushes one cue, not three.
  (setf *registered-commands*
        (remove owner *registered-commands* :key #'slash-command-owner :test #'equal))
  (- before (length *registered-commands*)))

(defun find-registered-command (name)
  "The command a cell registered as NAME, else NIL."
  (when (stringp name)
    (find name *registered-commands* :key #'slash-command-name :test #'string-equal)))

;;; --- gateway service and directory routes ----------------------------------

(defun gateway-query-integer (env name)
  "Positive integer query parameter NAME, or NIL when absent."
  ;; Signals PARSE-ERROR on garbage so route handlers answer 400 instead of
  ;; guessing.
  (when-let (value (cdr (assoc name (gateway-query-parameters env)
                               :test #'string=)))
    (let ((number (parse-integer value)))
      (require-request (plusp number) "~a must be positive" name)
      number)))

(defvar *gateway-started-at* nil
  "When the running gateway started accepting, in unix seconds, or NIL:
the started_at its identity route answers, which `nodecode gateway' turns
into an uptime.")

(defun gateway-identity-json ()
  "Who serves: the identity route and the status overview share this reader."
  (make-json-object
   "service" "nodecode-gateway"
   "protocol_version" nlk::+gateway-protocol-version+
   "pid" (sb-posix:getpid)
   "store_open" (and (nlk:store-open-p) t)
   "tools" (coerce (mapcar #'first *tools*) 'vector)
   ;; Who serves, for a shell that asks without attaching: `nodecode
   ;; gateway' prints the build, the uptime and the standing board.
   "version" (effective-version)
   ;; The binary and the folder it runs from, apart: a hot upgrade moves
   ;; the definitions past the build, and the restart watcher reads the
   ;; folder to know which build came up.
   "build" *user-agent-version*
   "runtime" (if-let (directory (nlk:runtime-directory)) (namestring directory) :null)
   "profile" (nlk:profile-name)
   "started_at" (or *gateway-started-at* :null)
   "notices" (coerce (mapcar #'second (nlk:standing-notices)) 'vector)))

(define-gateway-route handle-gateway-identity-http (env)
  (:route :get "/api/gateway/identity" "identity")
  (gateway-identity-json))

;;; --- how a row hangs under its session -------------------------------------
;;;
;;; A session's edge is its created fact's: an anchor turn means the child
;;; composes that parent's history — an inherited fork, which is what a
;;; /fork and a reflection are — and a parent with no anchor is
;;; provenance alone: a node spawned from another session that begins fresh
;;; (a team seat, a frame). The listing answers that one question per row as
;;; the wire word `relation', read off the edge the store holds and never
;;; off an id's spelling, so the /sessions picker draws every node under the
;;; session it came off.

(defun session-relation (parent anchor-turn-id)
  "How a directory row hangs under its parent, as the wire word the
/sessions picker draws: \"inherited\" when the session composes that
parent's history (its created fact froze an anchor turn), \"detached\" when
the edge is provenance alone — a node spawned from another session that
begins fresh — and NIL for a session that began on its own."
  (cond ((null parent) nil)
        (anchor-turn-id "inherited")
        (t "detached")))

(defun gateway-session-entry-json (entry)
  "One session directory row in the Zig SessionInfoPayload lexicon, plus
how the row sits in the fork tree — the same DEPTH/LANES/FORK triple the
checkpoint listing carries (SESSION-TREE-ORDER), so one glyph renderer
draws the /tree picker and the /sessions picker alike: DEPTH 0 on a session
that began on its own, LANES one boolean per lane the row is inside, T
while that lane still has a later fork below it, FORK on the row that opens
a lane. RELATION beside them is how the row hangs under its parent — an
`inherited' fork or a `detached' node (SESSION-RELATION) — absent on a
session that began on its own. SOURCE is the source kind of the request that
began the session (NLK:SESSION-SOURCE-KIND), null before its first turn.
PROMPT is the first prompt's line that says something, null before one."
  (make-json-object
   "id" (getf entry :id)
   "relation" (session-relation (getf entry :parent) (getf entry :anchor-turn-id))
   "source" (or (getf entry :source) :null)
   "title" (getf entry :title)
   "prompt" (or (getf entry :prompt) :null)
   "semantic_summary" (make-json-object "title" (getf entry :title))
   "created_at" (or (getf entry :created-at) "")
   "updated_at" (or (getf entry :updated-at) "")
   "working_directory" (or (getf entry :working-directory) "")
   "parent_id" (getf entry :parent)
   "message_count" (getf entry :message-count)
   "depth" (or (getf entry :depth) 0)
   "lanes" (map 'vector (lambda (open) (and open t)) (getf entry :lanes))
   "fork" (and (getf entry :fork) t)
   "waits" (session-waits-json entry)))

(defun session-waits-json (entry &aux (waits (getf entry :waits)))
  "What the session of directory ENTRY waits on the person it answers for:
the mark its newest answer carries (ANSWER-WAITS), {\"why\": \"asked\",
\"question\": LINE, or null for the answer's own last line}; a newest turn that
failed, {\"why\": \"failed\"}; else null -- a turn someone stopped waits on
nobody."
  (cond (waits (json-object "why" "asked" "question" (if (plusp (length waits)) waits :null)))
        ((equal (getf entry :turn-kind) nlk::+kind-turn-failed+) (json-object "why" "failed"))
        (t :null)))

(defun session-directory (limit &aux (entries (nlk:list-sessions)))
  "The sessions a picker lists, newest activity first, at most LIMIT."
  (if (and limit (> (length entries) limit)) (subseq entries 0 limit) entries))

(defun session-shell-p (session-id &aux (links (session-rows-links session-id)))
  "Whether a shell holds SESSION-ID open: a peer on it other than a link a
web page's rows view subscribes in the page's place."
  (when-let (gws (sb-ext:with-locked-hash-table (*gateway-sessions*)
                   (gethash session-id *gateway-sessions*)))
    (bt2:with-lock-held (gws.lock) (some (lambda (peer) (not (member peer links))) gws.peers))))

(defun session-delete-refusal (ids)
  "Why the sessions IDS -- a session and every session under it -- cannot be
deleted now, as (values CODE MESSAGE), or NIL: a shell holds one open, or one
runs a background evaluation."
  ;; Who has a session open is the gateway's to know, never the store's: a
  ;; shell on it -- a terminal, the owner's own -- writes on to it, and so does
  ;; an evaluation still running. A web page's rows view is a projection held
  ;; for a tab, and simply closes. A turn running in one is the store's to
  ;; refuse (NLK:SESSION-RUNNING).
  (let ((open (find-if #'session-shell-p ids))
        (busy (find-if (lambda (id) (exec-collect-session-snapshots (exec-registry-instance) id))
                       ids)))
    (cond (open (values "session_open" (format nil "a shell has ~a open: close it there first" open)))
          (busy (values "session_busy"
                        (format nil "~a is running a background evaluation: stop it first" busy))))))

(defun delete-gateway-sessions (session-id &aux (plan (nlk:delete-session session-id))
                                                (ids (getf plan :sessions)))
  "NLK:DELETE-SESSION of SESSION-ID, and what this process holds of the
sessions that went dropped with them: their rows views and registry
entries. => the delete's plan."
  (close-session-rows-views ids)
  (dolist (table (list *gateway-sessions* *session-registry*))
    (sb-ext:with-locked-hash-table (table)
      (dolist (id ids) (remhash id table))))
  plan)

(defun delete-gateway-session (session-id dry-run)
  "NLK:DELETE-SESSION for the sessions route, DRY-RUN answering what would
go: refused while a shell holds one of the sessions open or one runs a
background evaluation (SESSION-DELETE-REFUSAL), and what this process holds
of them dropped once they are gone."
  (let ((plan (nlk:delete-session session-id :dry-run t)))
    (multiple-value-bind (code message) (session-delete-refusal (getf plan :sessions))
      (if code
          (gateway-error-response 409 code message)
          (let ((plan (if dry-run plan (delete-gateway-sessions session-id))))
            (json-object "sessions" (coerce (getf plan :sessions) 'vector)
                         (:getf plan "turns" "events")))))))

(defparameter *prune-titles* 5
  "How many of a prune's sessions its answer names by title.")

(defun prune-gateway-sessions (&key days source empty dry-run only)
  "Delete the sessions NLK:PRUNE-PICKS picks, each with every session under
it, the way the DELETE route deletes one (DELETE-GATEWAY-SESSIONS); with
DRY-RUN delete nothing and answer the same. ONLY, when given, is the ids a dry
run named: a pick it does not name stays, so a second press deletes no more
than the first one read. => the answer's JSON object."
  ;; A pick the delete path would refuse -- a turn running in its tree, a
  ;; shell holding one of them open, an evaluation running in one -- is
  ;; skipped and named with the reason, never the whole prune refused: a
  ;; sweep over many sessions does not stop at the one somebody is using.
  (multiple-value-bind (picks running) (nlk:prune-picks :days days :source source :empty empty)
    (let ((gone '()) (titles '()) (kept '()) (count 0) (turns 0))
      (flet ((keep (id reason)
               (push (json-object "id" id "title" (nlk::session-title id) "reason" reason) kept)))
        (dolist (id running) (keep id "a turn is running in it"))
        (dolist (id picks)
          (when (or (null only) (member id only :test #'equal))
            (handler-case
                (let ((plan (nlk:delete-session id :dry-run t)))
                  (if-let (why (nth-value 1 (session-delete-refusal (getf plan :sessions))))
                    (keep id why)
                    ;; The name is read before the delete takes it.
                    (let ((title (and (< (length titles) *prune-titles*) (nlk::session-title id)))
                          (plan (if dry-run plan (delete-gateway-sessions id))))
                      (push id gone)
                      (when title (push title titles))
                      (incf count (length (getf plan :sessions)))
                      (incf turns (getf plan :turns)))))
              (nlk:session-running (condition) (keep id (princ-to-string condition)))
              ;; Gone since the pick: nothing is left of it to delete.
              (nlk:unknown-session () nil)))))
      (json-object
       "dry_run" (and dry-run t)
       "sessions" (coerce (reverse gone) 'vector)
       "count" count
       "turns" turns
       "titles" (coerce (reverse titles) 'vector)
       "kept" (coerce (reverse kept) 'vector)))))

(define-gateway-route handle-gateway-sessions-http (env session-id)
  "The whole /api/gateway/sessions path family, one authority: GET with a
SESSION-ID answers the session detail, GET without one lists the directory
(the Zig list route returns a bare array), POST without one creates, DELETE
with one deletes it and every session under it (?dry_run=1: says what would
go and deletes nothing)."
  ;; Every other shape is the 405 — a POST addressed to a concrete session
  ;; included.
  ;;
  ;; The listing comes in the session fork tree (SESSION-TREE-ORDER): a child
  ;; session — a fork, or a session spawned from another — follows its parent
  ;; one lane deeper, and each row says which lanes it sits inside and whether
  ;; it opens one, the shape the checkpoint listing already sends. LIMIT cuts
  ;; the directory before the tree is laid out, so a parent the cut removed
  ;; leaves its children listed as roots.
  (:route nil "/api/gateway/sessions" nil nil)
  (:on (nlk:duplicate-session 409 "duplicate_session" "session already exists")
       (nlk:invalid-session-anchor 400 "invalid_anchor")
       (nlk:unknown-session 404 "unknown_session" "session does not exist")
       (nlk:session-running 409 "turn_running")
       (nlk:missing-session-folder 400 "no_folder"))
  (let ((method (getf env :request-method)))
    (cond
      ((and (eq method :delete) session-id)
       (delete-gateway-session
        session-id (member (cdr (assoc "dry_run" (gateway-query-parameters env) :test #'string=))
                           '("1" "true") :test #'equal)))
      ((and (eq method :get) session-id)
       (let ((session (nlk:find-session session-id)))
         (cond
           (session (gateway-session-json session))
           ;; An id minted and not yet spoken in: the shell that minted it,
           ;; and any re-attach, read the standing-by identity here instead
           ;; of a 404 for a session the store has every right not to hold.
           ((nlk:on-standby-p session-id)
            (gateway-standby-session-json session-id))
           (t (gateway-error-response 404 "unknown_session" "session does not exist")))))
      ((eq method :get)
       (map 'vector #'gateway-session-entry-json
            (nlk:session-tree-order (session-directory (gateway-query-integer env "limit")))))
      ((and (eq method :post) (null session-id))
       (let* ((request (gateway-request-json env))
              (id (gethash "session_id" request)))
         (require-non-empty (list id) "session_id must be a non-empty string"
                            :required t)
         ;; Creating an id that exists — durable or still standing by — is
         ;; the duplicate it always was: a client that means to continue a
         ;; session reads or attaches to it, never creates it again.
         (when (and (nlk:store-open-p) (nlk:session-exists-p id))
           (error 'nlk:duplicate-session :session-id id))
         ;; The folder it names -- or, naming none and forking nothing, the
         ;; gateway's own -- is there to work in, or the session is refused
         ;; saying so: one opened where nothing is failed each sh call later
         ;; on the spawn. A fork that names none works where its parent does.
         (when (or (gethash "cwd" request) (null (gethash "parent" request)))
           (nlk:check-session-cwd (gethash "cwd" request)))
         ;; A new session is minted standing by (standby.lisp): the shell's
         ;; /clear, /fork and first attach name an id here, and it is durable
         ;; once its first block lands — a shell opened and never typed in
         ;; leaves no session in the store at all.
         (gateway-standby-session-json
          (nlk:standby-session
           :id id :cwd (gethash "cwd" request)
           :parent (gethash "parent" request)
           :anchor-turn-id (json-value request :text "anchor_turn_id")))))
      (t (gateway-error-response 405 "method_not_allowed"
                                 "unsupported session control operation")))))

(defun export-file-name (session-id)
  "The name SESSION-ID's export downloads as: the id, anything but an ASCII
letter, digit, dot, dash or underscore made a dash."
  (format nil "nodecode-~a.jsonl" (ppcre:regex-replace-all "[^A-Za-z0-9._-]" session-id "-")))

(define-gateway-route handle-gateway-session-export-http (env session-id)
  "GET /api/gateway/sessions/<id>/export — the session, the sessions under it
and the sessions it composes as one file of JSON lines (NLK:EXPORT-SESSIONS),
sent to be saved; POST /api/gateway/sessions/import takes it back.
?format=markdown sends this session alone to be read (SESSION-MARKDOWN)."
  ;; The JSON lines copy is refused while a turn runs, a file import could
  ;; not finish; the Markdown is the settled turns, and says one is running.
  (:route :get "export" "session export")
  (:on (nlk:session-running 409 "turn_running")
       (nlk:unknown-session 404 "unknown_session" "session does not exist"))
  (let ((wanted (cdr (assoc "format" (gateway-query-parameters env) :test #'string=))))
    (multiple-value-bind (octets type name)
        (cond ((member wanted '(nil "jsonl") :test #'equal)
               (values (nlk:export-sessions session-id) "application/x-ndjson"
                       (export-file-name session-id)))
              ((equal wanted "markdown")
               (values (sb-ext:string-to-octets (session-markdown session-id) :external-format :utf-8)
                       "text/markdown; charset=utf-8" (markdown-file-name session-id)))
              (t (nlk::protocol-refuse :invalid_request "format is jsonl or markdown")))
      (list 200 (list :content-type type
                      :content-disposition (format nil "attachment; filename=\"~a\"" name)
                      :content-length (length octets))
            octets))))

(defparameter *session-import-bytes* (* 128 1024 1024)
  "The largest session file an import reads.")

(define-gateway-route handle-gateway-session-import-http (env)
  "POST /api/gateway/sessions/import — recreate the sessions of a file an
export wrote, the body (NLK:IMPORT-SESSIONS). => {sessions, kept, events}:
the sessions made, the composed sessions this store already held, the facts
that landed."
  ;; A file of JSON lines is no JSON body, and a long session's runs to
  ;; megabytes, past GATEWAY-REQUEST-JSON's bound: the body is read here as
  ;; octets, declared as the one type it is. A session the store already
  ;; holds refuses the whole file.
  (:route :post "/api/gateway/sessions/import" "session import")
  (:on (nlk:sessions-present 409 "session_exists") (nlk:session-file-invalid 400 "invalid_file"))
  (let ((length (or (getf env :content-length) 0)))
    (cond
      ((not (string-equal "application/x-ndjson" (request-media-type env)))
       (gateway-error-response 415 "unsupported_media_type" "a session file is application/x-ndjson"))
      ((zerop length) (gateway-error-response 400 "invalid_file" "the body is empty"))
      ((> length *session-import-bytes*)
       (gateway-error-response 413 "request_too_large"
                               (format nil "a session file this route reads is at most ~d MiB"
                                       (floor *session-import-bytes* (* 1024 1024)))))
      (t
       (let ((octets (make-array length :element-type '(unsigned-byte 8))))
         (unless (= length (read-sequence octets (getf env :raw-body)))
           (nlk::protocol-refuse :truncated_request "HTTP request body ended early"))
         (destructuring-bind (&key sessions kept events) (nlk:import-sessions octets)
           (json-object "sessions" (coerce sessions 'vector)
                        "kept" (coerce kept 'vector)
                        "events" events)))))))

(define-gateway-route handle-gateway-sessions-prune-http (env)
  "POST /api/gateway/sessions/prune — delete, each with every session under
it, the sessions whose last activity is older than DAYS days, begun by
SOURCE when one is named (\"\" those the log names none for), holding no
message when EMPTY (PRUNE-GATEWAY-SESSIONS). DRY_RUN answers what would go
and deletes nothing."
  ;; Body: {"days": N, "source": KIND?, "empty": BOOL?, "dry_run": BOOL?,
  ;; "sessions": [ID...]?} -- SESSIONS, when given, the ids a dry run named,
  ;; so the delete that follows it takes no session the confirmation did not.
  ;; Answer: {"dry_run", "sessions": the ids that go (or went), oldest
  ;; activity first, "count": every session with them, "turns", "titles":
  ;; the first few's names, "kept": [{"id", "title", "reason"}]} -- a pick a
  ;; turn, a shell or an evaluation holds, skipped and said why.
  (:route :post "/api/gateway/sessions/prune" "session prune")
  (let* ((request (gateway-request-json env))
         (days (gethash "days" request))
         (source (gethash "source" request :null))
         (only (gethash "sessions" request :null)))
    (unless (and (integerp days) (<= 0 days 36500))
      (nlk::protocol-refuse :invalid_request "days must be a whole number of days, 0 or more"))
    (unless (or (eq source :null) (stringp source))
      (nlk::protocol-refuse :invalid_request "source must be a source kind, \"\" or null"))
    (unless (or (eq only :null) (and (vectorp only) (not (stringp only)) (every #'stringp only)))
      (nlk::protocol-refuse :invalid_request "sessions must be an array of session ids"))
    (prune-gateway-sessions :days days
                            :source (and (stringp source) source)
                            :empty (json-value request :boolean "empty")
                            :dry-run (json-value request :boolean "dry_run")
                            :only (and (vectorp only) (coerce only 'list)))))

(define-gateway-route handle-gateway-session-search-http (env)
  "POST /api/gateway/sessions/search — grep every session's conversation at
once (NLK:RECALL-SEARCH): the /sessions picker's search box once the box
stops being a title filter."
  ;; The query rides the body, never the query string, because the one query
  ;; scanner reads its values verbatim (ids the gateway minted and small
  ;; integers) and a search box holds spaces, ampersands and quotes.
  ;;
  ;; Body: {"query": "...", "limit": N}. Answer: {"query": "...",
  ;; "rows": [...]} — one row per matching block, newest first, at most LIMIT
  ;; (default 60, capped 200). A row carries the session (id, title), the block
  ;; (SEQ, the durable log position a shell pins its viewport at), who spoke
  ;; (KIND, and ROLE for a message — `user' or `assistant' — or TOOL for tool
  ;; traffic) and the snippet window around the match.
  ;;
  ;; A hit on a turn the session's head no longer carries is dropped here, not
  ;; served: the replay and the history pager both fold through the same
  ;; visibility predicate, so a search that flew a shell to a hidden block
  ;; would land it on a transcript that cannot show it.
  (:route :post "/api/gateway/sessions/search" "session conversation search")
  (let* ((request (gateway-request-json env))
         (query (string-trim " " (or (gethash "query" request) "")))
         (limit (or (json-value request :integer "limit") 60))
         ;; Session id -> the turn ids its head chain carries, read once per
         ;; session a page of hits landed in.
         (visible (make-hash-table :test #'equal)))
    (require-request (plusp (length query)) "query must be a non-empty string")
    (require-request (and (integerp limit) (plusp limit)) "limit must be a positive integer")
    (flet ((visible-turn-set (session-id)
             (alexandria:ensure-gethash session-id visible
                                        (nth-value 0 (nlk:session-visible-turn-set session-id)))))
      (loop for row in (nlk:recall-search query :limit (min limit 200))
            for turn-id = (getf row :turn-id)
            when (or (null turn-id)
                     (gethash turn-id (visible-turn-set (getf row :session-id))))
              collect (json-object
                       "session_id" (getf row :session-id)
                       "title" (or (getf row :title) (getf row :session-id))
                       (:getf row "seq" "kind")
                       "role" (or (getf row :role) "")
                       "tool" (or (getf row :tool) "")
                       "occurred_at" (getf row :at)
                       "snippet" (getf row :text)) into rows
            finally (return (make-json-object "query" query "rows" (coerce rows 'vector)))))))

(define-gateway-route handle-gateway-history-http (env session-id)
  "GET /api/gateway/sessions/<id>/history?before=N&limit=N — the lazy
history pager."
  ;; Returns the LIMIT visible frames strictly below BEFORE
  ;; (default: above the newest position), oldest first, with the floor
  ;; position to pass as the next page's BEFORE.
  (:route :get "history" "history paging")
  (let* ((before (or (gateway-query-integer env "before")
                     (1+ (session-cursor session-id))))
         (limit (min (or (gateway-query-integer env "limit") 128) 512))
         (rows (nlk:page-events-before session-id before limit :as :facts))
         (floor-position (if rows
                             (nlk::stored-fact-log-position (first rows))
                             0)))
    (make-json-object
     "session_id" session-id
     "frames" (coerce (visible-event-frames session-id rows
                                            (nlk:session-visible-turn-set session-id))
                      'vector)
     "next_before" floor-position
     "has_more" (and rows
                     (nlk:events :session-id session-id :before floor-position
                                 :as :exists)
                     t))))

(defun usage-slot-json (usage)
  "A fold's usage slot, the plist ingest-turn-usage keeps, as a JSON object
whose keys are the plist's, lowercased with underscores; a count the provider
did not report is absent, never zero."
  (apply #'make-json-object
         (loop for (key value) on usage by #'cddr
               when value
                 append (list (substitute #\_ #\- (string-downcase (symbol-name key)))
                              value))))

(defun session-header-json (session-id)
  "What the header of a shell on SESSION-ID shows: its title (the newest name,
else the first prompt), what the whole session has spent (session_usage), what
its newest round put in the window (context_usage), and THROUGH, the log
position of the newest round they cover."
  ;; The session's turn.usage facts on its visible chain go through the same
  ;; reducer a shell's replay folds them with (APPLY-EVENT), so a shell that
  ;; holds only a page of the history -- a /sessions preview -- reads the
  ;; numbers the attached shell would sum, not the page's.
  (let ((state (make-hash-table :test #'equal))
        (visible (nlk:session-visible-turn-set session-id))
        (through 0))
    (dolist (event (nlk:events :session-id session-id :kind "turn.usage" :order :oldest
                               :as :instances))
      (when (nlk::turn-visible-p event.turn-id visible)
        (apply-event state event)
        (setf through (nlk:event-log-position event))))
    (make-json-object
     "session_id" session-id
     "title" (or (nlk::session-title session-id) "")
     "session_usage" (usage-slot-json (gethash "session_usage" state))
     "context_usage" (usage-slot-json (gethash "context_usage" state))
     "through" through)))

(define-gateway-route handle-gateway-session-header-http (env session-id)
  "GET /api/gateway/sessions/<id>/header -- SESSION-HEADER-JSON."
  (:route :get "header" "session header facts")
  (session-header-json session-id))

(defun broadcast-model-changed (&optional session-id)
  "Tell every attached shell the target a turn binds moved — its model or
its effort: SESSION-ID's when one is named, the gateway default's when NIL."
  ;; A shell's footer renders cached facts (GATEWAY-MODEL-FACTS never dials on
  ;; the frame path) and session.model_selected is replay-suppressed config
  ;; evidence, so without this frame a selection landed anywhere but the
  ;; shell's own slash op stayed invisible to every footer until its next
  ;; reconnect. Unsequenced like a notice — consumed by kind before the
  ;; watermark, so the dense per-peer stream keeps no hole — and it rides the
  ;; :FRAME point like every live frame.
  (broadcast-organism "model_changed"
                      (json-object :opt "session_id" session-id)
                      :session-id session-id))

(defun refresh-attached-command-catalogs ()
  "Tell every shell its copy of the cell commands moved: the cell
lifecycle just started or stopped peripherals, so the commands they register
entered or left the catalog."
  ;; The boot starts the peripherals AFTER the listener, on their own thread —
  ;; on purpose, so a shell's attach never waits on them — while every shell
  ;; reads the catalog at attach and again on a fresh connection: without this
  ;; cue a boot's commands (/index, /cron, /web, ...) are missing from the
  ;; composer with no notice and no retry until the shell's next attach.
  ;; Each shell gets the unsequenced COMMANDS_CHANGED frame and re-reads off
  ;; its own transport, so GET /api/gateway/slash stays the one authority.
  ;; Returns how many shells were told.
  (nth-value 1 (broadcast-organism "commands_changed" (json-object))))

(defun set-session-effort (session-id effort)
  "The one meaning of an effort pick, whoever makes it: resolve the target a
turn on SESSION-ID would bind (GATEWAY-TARGET, the session's pin over the
gateway default), check EFFORT against that target's own ladder
(CHECK-EFFORT: NIL is the model's own reasoning, a refusal lands before any
write), make it the default every session opened from now on starts on
(SELECT-DEFAULT-EFFORT: this image's and the shared config's), pin it on
SESSION-ID, and tell every attached shell (BROADCAST-MODEL-CHANGED)."
  ;; The HTTP effort route, the shell's /think and the model-facing
  ;; SELECT-EFFORT land here, as a model pick lands in SELECT-SESSION-MODEL.
  ;; A session that already ran keeps its effort when the default moves: its
  ;; first turn pinned what it ran at (RUN). A landed pick is part of the
  ;; facts every shell's footer caches: the mutating client refreshes
  ;; itself, the others learn from the frame, session-less since the default
  ;; moved. => (values PROVIDER MODEL OLD NEW OUTCOME DETAIL): OLD the effort
  ;; the session ran at, OUTCOME and DETAIL SELECT-DEFAULT-EFFORT's answer.
  (check-type session-id string)
  (unless (and (nlk:store-open-p) (nlk:session-exists-p session-id))
    (error "no session ~s in the open store" session-id))
  (bind (((provider model pin-provider pin-model _ picked) (gateway-target session-id))
         (old (effective-effort picked provider model)))
    (check-effort provider model effort)
    (multiple-value-bind (outcome detail) (select-default-effort effort)
      (when (eq outcome :failed)
        (warn "shared config not updated: ~a" detail))
      (nlk:record-session-model-selection session-id :provider pin-provider :model pin-model
                                                     :effort effort)
      (broadcast-model-changed)
      (values provider model old effort outcome detail))))

(defun select-session-model (session-id &key provider model)
  "The one meaning of a model pick, whoever makes it: resolve the pair and
its lane BEFORE mutating anything (an unknown lane refuses with no
half-moved state), make it the default every session opened from now on
starts on (SELECT-DEFAULT-MODEL: this image's and the shared config's),
durably pin SESSION-ID to it, and tell every attached shell
(BROADCAST-MODEL-CHANGED)."
  ;; The HTTP session-model route, the owner shell's /models, the web page's
  ;; drawer and the model-facing SELECT-MODEL all land here. A session that
  ;; already ran keeps its model when the default moves: its first turn pinned
  ;; what it ran on (RUN), so the move reaches only sessions not yet begun.
  ;;
  ;; The pin is the pair the pick RESOLVED to — a model-only pick keeps the
  ;; session's lane — so a later move of the default's lane cannot slide this
  ;; session under it. The session's effort pick rides along while the new
  ;; target admits it — the reasoning a session chose outlives a change of
  ;; model that admits it. PROVIDER and MODEL both NIL clears the model pin
  ;; and leaves the default alone; the pick stays.
  ;; => (values PROVIDER MODEL OUTCOME DETAIL): the pin and
  ;; PERSIST-SHARED-MODEL-PIN's answer (NIL OUTCOME on a clear).
  (check-type session-id string)
  (unless (and (nlk:store-open-p) (nlk:session-exists-p session-id))
    (error "no session ~s in the open store" session-id))
  (bind (((pinned-provider pinned-model) (nlk:session-model-selection session-id))
         (picked (or provider model))
         ((target-provider target-model outcome detail)
          (if picked
              (select-default-model :provider (or provider pinned-provider *provider*)
                                    :model (or model pinned-model *model*))
              (values *provider* *model*)))
         (effort (admitted-effort (nlk:session-model-effort session-id)
                                  target-provider target-model)))
    (when (eq outcome :failed)
      (warn "shared config not updated: ~a" detail))
    (nlk:record-session-model-selection session-id
                                        :provider (and picked target-provider)
                                        :model (and picked target-model)
                                        :effort effort)
    ;; A pick moved the default, which every shell on a session not yet begun
    ;; shows: the frame goes session-less. A clear moved this session alone.
    (broadcast-model-changed (and (not picked) session-id))
    (values (and picked target-provider) (and picked target-model) outcome detail)))

(defun select-model (model &key provider (session-id nlk:*scribe-session-id*))
  "Land a model selection the way a /models pick does: durably pin
SESSION-ID — by default the session this eval is running for — make it the
default new sessions start on, and every attached shell's footer follows
(SELECT-SESSION-MODEL)."
  ;; MODEL alone keeps the current lane; :PROVIDER names one. (select-model
  ;; nil) clears the pin so the session takes the default again at its next
  ;; turn. An unknown lane refuses before anything moves. Other sessions that
  ;; already ran keep their own model. => one line saying what landed.
  (check-type model (or null model-id))
  (check-type provider (or null provider-id))
  (unless (stringp session-id)
    (error "select-model has no ambient session here — pass :session-id \"s-...\""))
  (bind (((provider model outcome detail)
          (select-session-model session-id :provider provider :model model)))
    (if model
        (format nil "session ~a pinned to ~a/~a, the default for new sessions now~@[ (shared config NOT saved — ~a)~]; sessions already begun keep theirs; attached shells follow"
                session-id provider model (and (eq outcome :failed) detail))
        (format nil "session ~a takes the default again from its next turn (~a/~a)"
                session-id *provider* *model*))))

(defun select-effort (effort &key (session-id nlk:*scribe-session-id*))
  "Land a reasoning-effort pick the way the shell's /think dialog does: it
is pinned on SESSION-ID — by default the session this eval is running for —
checked against the ladder of the target a turn there binds, made the
default new sessions start on, and every attached shell's footer follows
(SET-SESSION-EFFORT)."
  ;; EFFORT is one rung of the target's own ladder, (select-effort "medium");
  ;; an off-ladder rung refuses before anything moves and names the ladder.
  ;; (select-effort nil) picks the model's own reasoning. A pick binds at
  ;; admission, so the turn already running keeps its effort and the pick
  ;; applies from the next turn; sessions already begun keep theirs. => one
  ;; line saying what landed.
  (check-type effort (or null string))
  (unless (stringp session-id)
    (error "select-effort has no ambient session here — pass :session-id \"s-...\""))
  (bind (((provider model old new outcome detail) (set-session-effort session-id effort)))
    (format nil "reasoning effort for ~a/~a: ~a -> ~a, the default for new sessions now~@[ (shared config NOT saved — ~a)~]; applies from the next turn; sessions already begun keep theirs"
            provider model
            (or old "provider default") (or new "provider default")
            (and (eq outcome :failed) detail))))

(define-gateway-route handle-gateway-session-model-http (env session-id)
  "POST /api/gateway/sessions/<id>/model — durable selection for this session,
and the default every session opened from now on starts on (the
SELECT-SESSION-MODEL contract); sessions already begun keep their own."
  ;; Both null clears the pin and leaves the default alone: the session takes
  ;; the default again at its next turn.
  (:route :post "model" "session model selection")
  (let* ((request (gateway-request-json env))
         (provider-raw (gethash "provider" request))
         (model-raw (gethash "model" request)))
    (require-non-empty (list provider-raw model-raw)
                       "provider and model must be non-empty strings"
                       :allow-null t)
    (multiple-value-bind (provider model outcome detail)
        (select-session-model session-id
                              :provider (and (stringp provider-raw) provider-raw)
                              :model (and (stringp model-raw) model-raw))
      (json-object "session_id" session-id
                   "selected_provider" provider
                   "selected_model" model
                   :opt "shared_config" (and outcome (string-downcase outcome))
                   :opt "shared_config_error" detail))))

(define-gateway-route handle-gateway-session-effort-http (env session-id)
  "POST /api/gateway/sessions/<id>/effort {\"effort\": rung | null} — the
session's reasoning effort from its next turn, and the default every new
session starts on (SET-SESSION-EFFORT); null is the model's own. => the session's target, as GET
/api/gateway/target answers it."
  (:route :post "effort" "session reasoning effort")
  (:on (effort-refusal 400 "effort_unsupported" (effort-refusal-detail condition)))
  (let ((effort (gethash "effort" (gateway-request-json env))))
    (unless (or (null effort) (eq effort :null) (stringp effort))
      (nlk::protocol-refuse :invalid_request "effort must be a string or null"))
    (set-session-effort session-id (and (stringp effort) effort))
    (gateway-target-json :session-id session-id)))

(defun set-session-title (session-id title)
  "Name SESSION-ID TITLE, as one line of at most 64 characters. The empty
string gives back the name its first prompt makes. => the name now in effect."
  ;; The one writer of the session.title fact: every operator path (the title
  ;; route, /rename), a cell naming its own sessions (cron) and a model's
  ;; title (NAME-SESSION) land here.
  ;; The newest fact is the name every directory surface reads; an empty one
  ;; reads as none (DIRECTORY-TITLE-SQL), so the prompt's line shows again.
  (nlk:record-event session-id nlk::+kind-session-title+
                    (list :title (clip (one-line title) 64 :ellipsis ""))
                    :aggregate-type "session" :aggregate-id session-id
                    :subject-id session-id)
  ;; The fact flows on no stream (config evidence), so every attached shell is
  ;; told the name now in effect: the one the header of the shell showing
  ;; SESSION-ID wears. A shell that attaches later reads it off the replay.
  (let ((named (nlk::session-title session-id)))
    (when (plusp (length named))
      (broadcast-organism "session_titled" (json-object "session_id" session-id "title" named)
                          :session-id session-id))
    named))

;;; --- what a session's answer waits on ------------------------------------------
;;; An answer that waits on the person it answers -- a question, a go or no-go,
;;; a call only they can make -- is marked so by its own turn, never guessed
;;; from its words: a fact in the session's log the directory keeps beside the
;;; session's newest turn (NLK:LIST-SESSIONS :WAITS), so every surface that
;;; lists sessions reads the one answer (SESSION-WAITS-JSON). A channel's
;;; answer is marked through the same call (NCK:ANSWER-WAITS names its room).

(defun answer-waits (&key question session turn)
  "Mark the answer of the running turn as waiting on the person it answers --
a question, a go or no-go, a call only they can make -- QUESTION the line
that says on what (the answer's own last line when omitted). SESSION and
TURN name the turn, the one this runs in by default. The mark stands once
that turn completes, until another turn begins there; a second call
replaces the first. => (:session-id S :turn-id T :waits QUESTION-OR-T)"
  (receipt "calls" (list :verb "answer-waits"))
  (unless (or (null question)
              (and (stringp question) (<= 1 (length (trimmed question)) 200)))
    (error "answer-waits takes a question of 1 to 200 characters, or none, got ~s" question))
  (let* ((running *current-durable-turn*)
         (session (or session (and running (durable-turn-session-id running))
                      (error "answer-waits runs in a turn, or names its session and turn")))
         (turn (or turn (and running (equal session (durable-turn-session-id running))
                             (durable-turn-turn-id running))
                   (error "answer-waits names no turn of ~a" session)))
         (question (and question (trimmed question))))
    (nlk:record-event session nlk::+kind-session-waits+ (and question (list :question question))
                      :aggregate-type "session" :aggregate-id session :subject-id session
                      :turn-id turn)
    (list :session-id session :turn-id turn :waits (or question t))))

;;; --- a session's name, written by a model ------------------------------------
;;; A session wears its first prompt's line from the moment that prompt is
;;; admitted; a model then writes it a title -- what the person wants done, in
;;; three to seven words -- in one side call (COMPLETE: the auxiliary target,
;;; else the session's own), run beside the turn so the name usually lands while
;;; the answer is still being written. SUBMIT starts it, once, for a session
;;; with no name yet. Whoever owns a session advises NAME-SESSION for it and
;;; calls NEXT: with the words its name is better written from than the prompt
;;; (a channel's ask, under the room's envelope), and to wear the title where
;;; it shows the session (a thread, a card). *SESSION-NAMING* says whether, and
;;; on which thread.

(defparameter +title-instruction+
  "You name conversations. Given the message that opened one, answer with a title of three to seven words, in sentence case, that names what the person wants done: the words they would look for it by in a list. Keep names, numbers and technical terms exact. Never answer the message: name it. Answer with the title alone: no quotes, no trailing period, no preamble."
  "The side call's whole instruction for a session's title (NAME-SESSION).")

(defparameter +title-max-tokens+ 512
  "The naming call's completion ceiling. A model that thinks spends it
reasoning before it names; too low a ceiling would yield no title.
Hermes gives its titles 512.")

(defun written-title (text &aux (bag '(#\Space #\Tab #\Return #\" #\' #\` #\*)))
  "The title a naming call's TEXT holds, or NIL when it holds none: its first
line that says something, any label, quotes and trailing period taken off,
cut at a word to the 64 characters a session's name keeps."
  ;; Past twelve words the model answered the message instead of naming it,
  ;; and the session keeps its prompt's line.
  (let* ((line (and (stringp text)
                    (find-if (lambda (line) (plusp (length (trimmed line))))
                             (uiop:split-string text :separator '(#\Newline)))))
         (line (and line (string-left-trim bag line)))
         (bare (and line (string-trim (cons #\. bag)
                                      (if (uiop:string-prefix-p "title:" (string-downcase line))
                                          (subseq line 6)
                                          line))))
         (words (split-words bare)))
    (when (<= 1 (length words) 12)
      (let ((title (format nil "~{~a~^ ~}" words)))
        (subseq title 0 (and (> (length title) 64)
                             (or (position #\Space title :from-end t :end 65) 64)))))))

(defun name-session (session-id text &aux (words (split-words text)))
  "Name SESSION-ID the title a model writes for TEXT -- TEXT's own words when
they are five or fewer, at no call -- unless the session was given a name
meanwhile. => the title it now wears, or NIL: TEXT says nothing, the answer
named nothing, or a name came first. A provider failure signals, as
COMPLETE's does."
  ;; Any name given while the call ran outranks it: the operator's /rename, a
  ;; cell's own (cron names a job's session as its fire is admitted).
  (let ((title (if (<= (length words) 5)
                   (and words (format nil "~{~a~^ ~}" words))
                   (written-title (complete +title-instruction+ (clip text 1000 :ellipsis "")
                                            :session-id session-id
                                            :max-tokens +title-max-tokens+)))))
    (when (and title (not (nlk:events :session-id session-id :kind nlk::+kind-session-title+
                                      :as :exists)))
      (set-session-title session-id title))))

(define-gateway-route handle-gateway-session-title-http (env session-id)
  "POST /api/gateway/sessions/<id>/title {title} — the operator names the
session (SET-SESSION-TITLE); the empty string gives back the name its first
prompt makes. => {session_id, title}: the name now in effect."
  ;; A session standing by is in no directory and has no fact to carry a
  ;; name: its first prompt makes it, and names it. A running turn is no
  ;; bar, model naming writes the same fact mid-turn.
  (:route :post "title" "session naming")
  (let ((title (gethash "title" (gateway-request-json env))))
    (require-request (stringp title) "title must be a string")
    (if (nlk:on-standby-p session-id)
        (gateway-error-response 409 "nothing_to_name"
                                "the session has no prompt yet: it is named once it has one")
        (json-object "session_id" session-id
                     "title" (set-session-title session-id title)))))

;;; --- WebSocket connection handler ---------------------------------------

(defun apply-gateway-route (route env &rest arguments)
  "Call ROUTE's handler with ENV, the ARGUMENTS the dispatcher resolved out of
the path, and the literals the declaration carried after them."
  (apply (first route) env (append arguments (rest route))))

(defvar *routes* (make-hash-table :test #'equal :synchronized t)
  "What ROUTE registered: a path -> (FUNCTION . PUBLIC).")

(defun route (path fn &key public)
  "Answer PATH on the gateway's port with FN, a function of the request's clack
environment returning a clack response or a JSON value (a 200's body). A PATH
ending in / answers every path under it; FN NIL takes PATH back. A request
without the operator token is refused before FN runs, unless PUBLIC. => PATH."
  ;; PUBLIC is for what cannot carry the token -- a browser page, a webhook, a
  ;; sign-in callback -- whose FN owns its own trust. Everything else a
  ;; cell serves is the operator's, as the gateway's own routes are: the
  ;; Bearer header is also what keeps another site's page from posting to it.
  ;; The gateway's own routes win every collision.
  (check-type path string)
  (unless (uiop:string-prefix-p "/" path)
    (error "route: ~s does not start with /" path))
  (if fn
      (setf (gethash path *routes*) (cons (the function fn) (and public t)))
      (remhash path *routes*))
  path)

(defun routed (uri)
  "What ROUTE registered for URI, (FUNCTION . PUBLIC): its exact path's, else
the longest directory's (a path ending in /) that URI lies under."
  (or (gethash uri *routes*)
      (sb-ext:with-locked-hash-table (*routes*)
        (loop with best = nil and best-length = 0
              for path being the hash-keys of *routes* using (hash-value entry)
              when (and (uiop:string-suffix-p path "/") (> (length path) best-length)
                        (uiop:string-prefix-p path uri))
                do (setf best entry best-length (length path))
              finally (return best)))))

(defun make-sync-app ()
  "Return the authenticated HTTP control and WebSocket Gateway application."
  (lambda (env)
    (nlk:note-boot-phase "http-entry" (getf env :request-uri))
    ;; URI is the request path with any query string stripped. Route
    ;; matching MUST use it, never the raw :REQUEST-URI: the real TUI sends
    ;; ?limit=N on GET routes, and matching against the raw URI silently
    ;; misses every query-carrying request.
    (let* ((uri (let ((raw (or (getf env :request-uri) ""))) (subseq raw 0 (position #\? raw))))
           ;; The non-empty segments after the session prefix: an id or an op
           ;; that would contain a slash simply lands in the next segment.
           (segments (let ((prefix "/api/gateway/sessions/"))
                       (when (uiop:string-prefix-p prefix uri)
                         (remove "" (uiop:split-string (subseq uri (length prefix))
                                                       :separator "/")
                                 :test #'string=))))
           (session-id (first segments))
           ;; OP names the subroute: the segments after the session id, the
           ;; third read as `*' -- a checkpoint id, an image key -- and handed
           ;; on after the session id. A path no declaration names falls
           ;; through to the uniform 404.
           (op (and (second segments)
                    (format nil "~a~:[~;/*~]~{/~a~}" (second segments) (third segments)
                            (cdddr segments))))
           (headers (getf env :headers))
           (origin (gethash "origin" headers))
           (host (gethash "host" headers))
           (routed (routed uri)))
      (cond
        ;; A page on another name that resolves here -- DNS rebinding -- is
        ;; the one way a browser reaches a loopback port as someone else.
        ((not (and (stringp host)
                   (member (subseq host 0 (position #\: host)) '("127.0.0.1" "localhost")
                           :test #'string-equal)))
         (gateway-error-response 403 "host_refused" "this gateway answers only at 127.0.0.1 or localhost"))
        ;; Unauthenticated liveness, the Zig /healthz body shape.
        ((string= uri "/healthz")
         (gateway-json-response 200 (make-json-object "ok" t "component" "gateway")))
        ;; The declared addresses (*GATEWAY-ROUTES*), exact path first: the
        ;; conversation search is looked up before the session-id branch
        ;; because `search' there is the route, not a session.
        ((gethash uri *gateway-routes*)
         (apply-gateway-route (gethash uri *gateway-routes*) env))
        ((and session-id (null (second segments)))
         (handle-gateway-sessions-http env session-id))
        ((and op (gethash op *gateway-routes*))
         (apply #'apply-gateway-route (gethash op *gateway-routes*) env session-id
                (and (third segments) (list (third segments)))))
        ;; A browser opens a socket to any origin it likes, and only says whose
        ;; page asked; a shell's socket says nothing. The page's own origin, which
        ;; the Host check above held to loopback (a tunnel's port is the page's too).
        ((and (string= uri "/gateway") origin
              (not (string-equal origin (format nil "http://~a" host))))
         (gateway-error-response 403 "origin_refused" "a socket from a page this gateway did not serve"))
        ((string= uri "/gateway")
         (let* ((ws (wsd:make-server env))
                ;; An upgrade that carries the operator's Bearer is the
                ;; operator at the door, as every HTTP route takes it.
                (door (gateway-http-authorized-p env))
                (subscription-ref (list '()))
                (nonce (nlk::make-durable-id "challenge"))
                (handshake-complete nil)
                (accepted-scopes '()))
           (wsd:on :open ws
                   (lambda ()
                     (nlk:note-boot-phase "ws-open")
                     (ensure-gw-peer-state ws)
                     (enqueue-peer-frame ws (nlk:encode-gateway-connect-challenge nonce))))
           (wsd:on :message ws
                   (lambda (msg)
                     (with-handlers
                         ((nlk:protocol-error (condition)
                            ;; Past the handshake a refusal is a recoverable
                            ;; protocol error; before it, the permanent rejection.
                            (enqueue-peer-frame
                             ws (funcall (if handshake-complete
                                             #'nlk::encode-sync-protocol-error
                                             #'nlk:encode-gateway-connect-rejected)
                                         (string-downcase (nlk:protocol-error-code condition))
                                         (nlk:protocol-error-message condition)
                                         :permanent (not handshake-complete)))))
                       (unless (stringp msg)
                         (nlk::protocol-refuse :invalid_envelope "sync frames are text"))
                       (nlk:check-sync-frame-size msg)
                       (if handshake-complete
                           (dispatch-command ws msg subscription-ref accepted-scopes)
                           (let ((frame (nlk:decode-sync-message msg)))
                             (unless (eq :gateway-connect frame.type)
                               (error 'nlk:protocol-error :code :connect_required
                                      :message "gateway_connect must complete before commands"))
                             (setf accepted-scopes
                                   (nlk::validate-gateway-connect
                                    frame.payload nonce
                                    *gateway-operator-token* :authorized door)
                                   handshake-complete t)
                             (enqueue-peer-frame
                              ws (nlk:encode-gateway-connect-ack accepted-scopes
                                                              :version (effective-version)))
                             (nlk:note-boot-phase "ws-acked")
                             (mark-peer-attached ws))))))
           (wsd:on :close ws
                   (lambda (&key &allow-other-keys)
                     (stop-gw-peer ws)
                     (remove-peer (car subscription-ref) ws)
                     (close-rows-views ws)))
           (lambda-ignoring (responder)
             (wsd:start-connection ws))))
        ((and routed (not (cdr routed)) (not (gateway-http-authorized-p env)))
         (gateway-error-response 401 "operator_auth_failed" "valid Bearer token required"))
        (routed
         (with-handlers ((error (condition)
                           (gateway-error-response 500 "route_failed" (princ-to-string condition))))
           (gateway-answer (funcall (car routed) env))))
        ;; Unknown routes 404 uniformly, authenticated or not: the old
        ;; catch-all 200 banner was both a service fingerprint and — since
        ;; ids with "/" never match a session route — a namespace oracle.
        (t (gateway-error-response 404 "not_found" "unknown route"))))))
;;; --- lifecycle ----------------------------------------------------------

;;; --- the note a boot says when it is back -------------------------------------
;;;
;;; A boot should brief the operator briefly that it is back. Restart -- an
;;; update's, a crash, a stop -- cuts off the turns it ran. Without NOTE-BACK,
;;; those turns would pick up at boot without a word: a shell reattached to a
;;; moving transcript and a channel's home room would hear "Back." alone, and
;;; an operator's quit and restart would leave nothing said. So a boot writes
;;; down what it has to tell (NOTE-BACK): how long it was gone, why, and the
;;; turns it picks up. Each turn it picks up opens with the model saying in a
;;; sentence that it is back and where it was (the `resumed' live section,
;;; context.lisp READ-LIVE-SECTIONS).
;;;
;;; The note is said at every return, including the operator's own quit.
;;; Without a note, a quit and restart would leave nothing said, since nothing
;;; had been running to write one. What this return was reads off the
;;; gateway's `run' stamp: when it started, and -- written at a clean stop --
;;; when it stopped and why. A stamp with no stop is a run that died (a
;;; crash, a kill -9); no stamp is a home's first boot, which has nothing to
;;; come back from.
;;;
;;; The words are the model's: it should respond to the note in a natural way,
;;; not a hardcoded "Back.". Once the shell is ready, the boot hands its note
;;; -- with a release it runs for the first time (update.lisp TAKE-RELEASE) --
;;; to the model in one side call, and says what the model writes (SAY-NOTE):
;;; a row on every shell, and on one that attaches in the next ten minutes,
;;; and a post in each channel host's home room, one that starts later
;;; included.

(defvar *stop-reason* nil
  "Why this gateway stops, for the next boot's note: \"quit\" for the
operator's own quit from the shell that owns it (CALL-WITH-SHELL's loop
returning), \"an update\" for an update's restart (update.lisp
EXIT-FOR-RESTART), NIL for anything else -- a signal, its unit stopping it.")

(defun note-run (&rest stamp)
  "Make STAMP, a json-object's keys and values, the gateway's `run' stamp."
  (when (nlk:store-open-p)
    (nlk:session-state-put "gateway" "run" (encode-json-object (apply #'make-json-object stamp)))))

(defun note-stopped ()
  "Keep when and why this gateway stops in its `run' stamp, for the next
boot's note."
  (let ((run (ignore-errors (nlk:decode-json (nlk:session-state-get "gateway" "run")))))
    (apply #'note-run "started" (json-value run :integer "started") "stopped" (get-universal-time)
           (and *stop-reason* (list "why" *stop-reason*)))))

(defun back-note (run turns &optional (now (get-universal-time)))
  "What a boot has to tell, for the model to say, from RUN -- the `run' stamp
the gateway before it left, NIL at a home's first boot -- and TURNS, how many
turns it picks up; NIL when it has nothing to tell."
  ;; A run that died left no stop, so its gap is not known. What stopped it
  ;; -- a quit, a kill, a crash -- is not told: it is nothing the user needs.
  ;; A restart for an update is told: that one changed what runs. The turns
  ;; are counted, never named: the words reach every channel's home room
  ;; (SAY-NOTE), and a session's title is its room's, not theirs.
  (let ((stopped (json-value run :integer "stopped"))
        (why (json-value run :string "why")))
    (when run
      (format nil "nodecode is running again~@[ after ~a away~]~@[: it restarted for ~a~].~@[ ~a~]"
              (and stopped (plusp (- now stopped)) (nlk:duration-text (- now stopped) :coarse t))
              (and (not (equal why "quit")) why)
              (case turns
                (0 nil)
                (1 "It picks up the turn the stop cut off.")
                (t (format nil "It picks up ~d turns the stop cut off." turns)))))))

(defun note-back (resumable)
  "This boot's note, from the `run' stamp the gateway before it left and
RESUMABLE, the (TURN INPUT) pairs it picks up; each of those turns is told it
at its next round. This boot's own stamp replaces the one it read. The note
carries nothing the organism keeps -- no memory, skill or session title: the
words the model writes of it are posted in every channel's home room, where
others read them (SAY-NOTE); a turn picked up knows its own work."
  (let ((run (ignore-errors (nlk:decode-json (nlk:session-state-get "gateway" "run")))))
    (note-run "started" (get-universal-time))
    (clrhash *resumed-turns*)
    (when-let (note (back-note run (length resumable)))
      (loop for (turn) in resumable
            do (setf (gethash (nlk:durable-turn-turn-id turn) *resumed-turns*)
                     (format nil "A restart cut this turn off, and it resumes where it stopped. ~a Open your next message with one short sentence telling the user you are back and what you were doing, then carry on."
                             note)))
      note)))

(nlk:define-section ("welcome")
  (:guide "What nodecode says when it comes back -- after a stop, a crash or an update: a short brief in the side-work model's words, as a row in a chat that has begun and in each channel's home room; the home screen shows none.")
  ("note" :boolean :default t
          :doc "say the brief; off, nodecode comes back without a word and makes no model call for it"))

(defun welcome-note-p ()
  "Whether the organism says its note (SAY-NOTE): the shared config's
`welcome.note', on unless it says."
  (getf (nlk:section-settings (nlk:find-section '("welcome"))
                              (json-value (read-shared-config) :object "welcome"))
        :note))

(defparameter +note-instruction+
  "You are nodecode, the user's agent. The message is a note your own runtime wrote about you: that you are running again after a stop, how long you were gone and whether an update restarted you, maybe a release you now run and what changed in it, or how many turns the stop cut off that you pick up. What you write is posted where other people read it, a channel's shared room among them. Brief the user, in your own words and as yourself, that you are back: one or two short sentences. It always carries humor: one dry, warm, wry line -- about being back or the time away -- never at the user's expense; a brief with no joke in it is not done. It is a brief, not a pitch: never offer, ask or suggest to continue, resume, pick up where you left off or carry anything on, and end without a question. Never claim work is running or left running. Never say why or how you stopped -- a quit, a kill, a crash, a service, from outside or from within: only that you are back. Say how long you were gone only broadly -- a moment, a few minutes, a while, overnight -- never an exact figure. Say that you were updated only when the note says so, never with a version number. Of a list of changes, name the few that matter to a user, each one meaning exactly what its line says: never turn one around. Plain text; no greeting, heading or emoji. Do not copy the note's sentences, and add nothing it does not say."
  "The side call's whole instruction for a note the organism says (SAY-NOTE).
Its words are posted in shared channel rooms, so it says what the note says
and nothing the organism keeps: a brief with a touch of humor and no offer
to carry the user's work on, never what stopped it; the time away said
broadly, a release without its version.")

(defparameter +note-max-tokens+ 1024
  "The side call's ceiling: a few sentences, and room for a thinking model to
reason before them.")

(defparameter *organism-note-seconds* 600
  "How long the note the organism last said is said to a shell that attaches.")

(defvar *organism-note* nil
  "(WORDS . UNIVERSAL-TIME): the note the organism last said, or NIL.")

(defun say-note (note)
  "Hand NOTE -- what the organism has to tell about itself -- to the model, and
say the words it writes as an organism.note: a row on every shell whose chat
has begun, and on one that attaches while they are fresh (the home screen
shows none), and a post in each channel host's home room. => the words, or NIL: NOTE empty, the operator's `welcome.note' off,
or no model answered."
  ;; One side call (COMPLETE), on the side-work model: no tools, no history --
  ;; the note is all it knows. A model that cannot be reached says nothing,
  ;; in the log alone.
  (let ((words (and (plusp (length note))
                    (welcome-note-p)
                    (with-handlers ((error (condition)
                                      (warn "the organism's note went unsaid: ~a" condition)
                                      nil))
                      (let ((written (complete +note-instruction+ note :max-tokens +note-max-tokens+)))
                        (and (stringp written) (nlk:trimmed written)))))))
    (when (plusp (length words))
      (setf *organism-note* (cons words (get-universal-time)))
      (broadcast-organism "organism.note" (json-object "text" words))
      words)))

(defun organism-note ()
  "The words the organism last said while they are fresh, else NIL."
  (let ((note *organism-note*))
    (and note (< (- (get-universal-time) (cdr note)) *organism-note-seconds*) (car note))))

;;; --- the layer's files a boot did not load, the model's to mend -------------
;;; The layer is the model's own definitions; without this, a release that
;;; moves the core under one would leave a file standing in no image: it
;;; would be skipped or FAILED at every boot, said on the boot line and the
;;; board, and acted on by nobody. A boot that finds one it had not handed
;;; before hands them all to the model, as a turn of their own, once: each
;;; is defined again against this build or forgotten.

(defparameter *layer-request*
  "[layer: this build did not load ~d file~:p of your layer (~a), definitions you kept, each with why:
~{- ~a: ~a~%~}Until you act they stand in no image, and every boot names them again. For each, read the file and decide. Still worth having: define it again so it loads against this build -- eval the corrected definition itself, a toplevel form, and the scribe writes it over the file; a name this build took away has a successor or none, which (apropos ...) and (documentation ...) find. Not worth it: forget it -- eval (fmakunbound 'name) or (makunbound '*name*) as a toplevel form, the name written out, and the scribe takes the file out; a forgetting computed inside another form, a loop over names, is not seen. Then look: the file reads anew or is gone. (help :layer) has the rest. Answer with one line per file: kept or forgotten, and why.]"
  "What the layer's session is handed at a boot that did not load a file of
the layer it had not handed before. Data, so a layer can reword it.")

(defun hand-broken-layer (&aux (broken (nlk:broken-layer-files)))
  "Hand the files of the layer this boot did not load to the model, in the
session `layer', when one of them was not handed before. => the files
handed, or NIL."
  ;; The gateway's state `layer' keeps the files this boot found broken, so a
  ;; file mended and broken again is handed again. Kept once the prompt is
  ;; admitted: a submit that signals is tried again at the next boot.
  (when (nlk:store-open-p)
    (let* ((files (mapcar #'car broken))
           (handed (ignore-errors
                    (coerce (json-array (nlk:decode-json (nlk:session-state-get "gateway" "layer"))
                                        "files")
                            'list))))
      (prog1 (when (set-difference files handed :test #'string=)
               (let ((folder (directory-namestring (first files))))
                 (unless (nlk:session-exists-p "layer")
                   (nlk:standby-session :id "layer" :cwd folder))
                 (submit "layer"
                         (format nil *layer-request* (length broken) folder
                                 (loop for (path . reason) in broken append (list path reason)))
                         :source "layer"))
               files)
        (when (or files handed)
          (nlk:session-state-put "gateway" "layer"
                                 (encode-json-object (json-object "files" (coerce files 'vector)))))))))

(defun start-gateway-recovery (resumable)
  "Dispatch durable suspended turns -- RESUMABLE, the (TURN INPUT) pairs the
recovery pass marked -- once the listener is live and the cells are up
(START-CELLS-BEHIND-THE-LISTENER), then drain queued prompts stranded in
sessions whose active turn already terminalized."
  (dolist (entry resumable)
    (destructuring-bind (turn input) entry
      ;; Captured before the claim fact commits so the live cursor stays
      ;; authoritative over it (the claim itself is evidence-horizon and is
      ;; suppressed from publication like every other evidence kind).
      (let* ((session-id (nlk:durable-turn-session-id turn))
             (pre-cursor (session-cursor session-id)))
        (when (nlk::claim-turn-resumption turn)
          (spawn (format nil "resume-~a" (nlk:durable-turn-turn-id turn))
            (ignore-errors (start-admitted-turn session-id turn input nil pre-cursor)))))))
  ;; Sessions with a live (resumed) turn drain via their worker's cleanup;
  ;; only sessions with no active turn need a boot-time kick.
  (dolist (session-id (ignore-errors (nlk::sessions-with-pending-prompts)))
    (unless (nlk:active-turn-p session-id)
      (ignore-errors (start-queued-turn session-id)))))

;;; --- cells: start, stop, restart -----------------------------------------
;;; The folder is loaded at the top of MAIN (kernel cells.lisp);
;;; what the gateway owns is the lifecycle of the peripherals in it. The
;;; contract is one convention — a folder's package <NAME> exports
;;; START-CELL (config) => stop-thunk — and carries no use case: channel
;;; hosts, bridges, schedulers all ride these same lines. The whole parsed
;;; config is passed through so the core parses once and each cell picks
;;; its own sections. Nothing else is handed over: a cell lives inside the
;;; image and reaches the organism through the in-process seams (NLE:HOOK,
;;; NLE:SUBMIT, the NLK session API), never by dialling the listener it runs
;;; beside — so it needs neither the port nor the operator token. Hermetic
;;; tests are safe by construction: the suite posture binds
;;; *SHARED-CONFIG-PATH* to NIL and the folder to nothing.
;;;
;;; A refusal is a fact, not a log line: it lands on the record (REFUSAL),
;;; on the board under the folder's name, and on every shell — and
;;; RESTART-CELLS is the operator's one form for "I fixed the config, try
;;; again", from the TUI or from a channel, without a relaunch.

(defvar *cells-lifecycle-lock* (bt2:make-lock :name "cells-lifecycle")
  "Start, stop and restart serialize: the boot thread starts after
recovery, a turn worker restarts from eval, the unwind stops.")

(defun cell-selected-p (cell names) (or (null names) (member cell.name names :test #'string=)))

(defun %start-cells (names)
  (multiple-value-bind (config error-text) (read-shared-config)
    (notice (and error-text
                 (format nil "cells started without shared config: ~a" error-text))
            :level :warning :key "shared config")
    (when (and (hash-table-p config) (nth-value 1 (gethash "addons" config)))
      (notice (format nil "config key `addons` is not read: a cell is a folder under ~a"
                      (namestring nlk::*cells-directory*))
              :level :warning))
    (dolist (cell (nlk:cells))
      (when (and (cell-selected-p cell names)
                 (eq cell.state :loaded)
                 cell.start
                 (not cell.started-p)
                 ;; A folder taken out of the one folder (control.lisp's off)
                 ;; keeps its code until a restart, never its start: presence
                 ;; is consent.
                 (or (null cell.path) (probe-file cell.path)))
        (let ((name cell.name))
          (handler-case
              (let ((stop (funcall cell.start config)))
                (setf cell.started-p t cell.stop (and (functionp stop) stop))
                (when cell.refusal
                  ;; The refusal is over: off the board, and said once.
                  (setf cell.refusal nil)
                  (notice nil :key name)
                  (notice (format nil "cell ~a: started" name))))
            (error (condition)
              ;; The record keeps the refusal in the words it was made in; the
              ;; board's line names the condition for whoever reads the log.
              (setf cell.refusal (if (typep condition 'nlk:config-refusal)
                                      (nlk:config-refusal-detail condition)
                                      (princ-to-string condition)))
              (notice (format nil "cell ~a not started: ~a" name condition)
                      :level :error :key name))))))))

(defun %stop-cells (names)
  (dolist (cell (reverse (nlk:cells)))
    (when (cell-selected-p cell names)
      (when cell.started-p
        (nlk:note-phase "stop cell ~a" cell.name)
        (handler-case (when cell.stop
                        (funcall cell.stop))
          (error (condition)
            (notice (format nil "cell ~a stop signalled: ~a" cell.name condition) :level :warning)))
        (setf cell.started-p nil
              cell.stop nil))
      (when cell.refusal (setf cell.refusal nil) (notice nil :key cell.name)))))

(defun start-cells (&optional names)
  "Start every loaded peripheral (or the folders NAMES): funcall its
START-CELL with the shared config after recovery, keeping the stop thunk
on its record."
  ;; Failure-class split: an error starting one cell — refused config, bad
  ;; entry — is that record's REFUSAL, one standing notice under the folder's
  ;; name, and that cell absent; the gateway always boots and siblings still
  ;; start. Transient transport trouble is the cell's own supervision
  ;; problem and never re-touches this hook.
  (bt2:with-lock-held (*cells-lifecycle-lock*)
    (%start-cells names))
  ;; The commands moved: every shell's copy of the catalog is stale.
  (refresh-attached-command-catalogs)
  ;; And the lanes: a shell that attached before them read `provider
  ;; claude-code is not one this box knows' for a pin a cell serves, and kept
  ;; it on its landing screen (the operator, 2026-10-22).
  (broadcast-model-changed))

(defun stop-cells (&optional names)
  "Stop every started cell (or the folders NAMES), newest first, and
clear their refusals."
  ;; Clients stop before their server, so this is the first step of
  ;; STOP-GATEWAY's unwind.
  (bt2:with-lock-held (*cells-lifecycle-lock*)
    (%stop-cells names))
  (refresh-attached-command-catalogs))

(defun restart-cells (&optional name &aux (names (and name (list name))))
  "Stop every started peripheral — or folder NAME alone — re-read the
shared config, and start again: what the operator or the model says after a
config change instead of relaunching."
  ;; The same stops and starts the gateway runs at its own boot and unwind;
  ;; nothing new in the lifecycle. Answers the report — the records' lines
  ;; with their start state, then the board — so the eval snippet shows what
  ;; happened. Blocks on the caller's thread for as long as the stops and
  ;; starts take.
  (when (and name (not (nlk:find-cell name)))
    (error "no cell folder named ~a" name))
  (bt2:with-lock-held (*cells-lifecycle-lock*)
    (%stop-cells names)
    ;; The reader's memo keys on size and mtime, second-granular: an edit
    ;; the operator made within the second the memo was taken must still
    ;; be what starts.
    (setf *shared-config-memo* nil)
    (%start-cells names))
  (refresh-attached-command-catalogs)
  (broadcast-model-changed)
  (or (nlk:cells-report-text names) "no cells"))

(defun stop-clack-handler (handler &key (timeout 5))
  "Stop HANDLER's acceptor and wait for it to be gone."
  ;; CLACK:STOP is
  ;; DESTROY-THREAD plus a fixed half-second sleep; hunchentoot's own STOP runs
  ;; inside the dying handler thread's unwind, so the call returns while the
  ;; listener can still be alive. A process that exits in that window lets SBCL
  ;; reap the two threads in arbitrary order: the listener aborts first, and the
  ;; handler's cleanup then joins an aborted thread — the JOIN-THREAD-ERROR
  ;; backtrace on Ctrl-C. Joining the handler thread (bounded by TIMEOUT
  ;; seconds) is the proof the acceptor is down, so the thread is destroyed
  ;; here and joined, without CLACK:STOP's half second: two of them were a
  ;; second of every organism's stop, the store held through it. Every error
  ;; is swallowed: a stop is best effort and this is unwind-time code.
  (let ((thread (ignore-errors (clack.handler::handler-acceptor handler))))
    (cond ((typep thread 'bt2:thread)
           (when (bt2:thread-alive-p thread) (ignore-errors (bt2:destroy-thread thread)))
           (nlk::%join-bounded thread timeout))
          (t (ignore-errors (clack:stop handler)))))
  t)

(defun mark-interactive-socket (socket)
  "TCP_NODELAY on SOCKET, a connection the gateway accepted."
  ;; Every connection here is interactive — a control request, a sync stream —
  ;; and hunchentoot writes a response as more than one segment; with Nagle
  ;; on, the second waits for the first's acknowledgement, which a client on a
  ;; reused connection delays by 40 ms (Linux's delayed ACK): every pooled
  ;; control request from an attaching shell measured exactly that, and a
  ;; fresh connection — curl, an unpooled request — measured 1 ms. Best
  ;; effort: a socket that refuses the option still serves.
  (ignore-errors (setf (usocket:socket-option socket :tcp-nodelay) t))
  socket)

(defmethod hunchentoot:process-connection :before
    ((acceptor clack.handler.hunchentoot::clack-acceptor) socket)
  "The gateway's acceptor marks every connection it processes before the
first byte is read: the HTTP control surface and the /gateway sync stream
both ride sockets that never wait on Nagle."
  (mark-interactive-socket socket))

(defmethod initialize-instance :after
    ((acceptor clack.handler.hunchentoot::clack-acceptor) &key)
  "The gateway's acceptor logs to the process's standard error, the
organism's log (LOG-SOURCE), never to the *ERROR-OUTPUT* it was made under."
  ;; Hunchentoot takes its message log from *ERROR-OUTPUT* when the acceptor
  ;; is made, and clack hands on the caller's: a shell's error stream, whose
  ;; every line is a toast. A warning signalled on a connection (a cold
  ;; cell compile's `undefined variable'), `too many request threads', a
  ;; body it cannot read -- each stood as a raw `[date [WARNING]] ...' toast.
  ;; Standard error is the severed-stdio log under a shell, the journal under
  ;; the gateway's unit.
  (setf (hunchentoot:acceptor-message-log-destination acceptor) sb-sys:*stderr*))

(defun await-port (port &key (attempts 100) (pause 0.002))
  "Poll until 127.0.0.1:PORT completes a TCP connect; => T, or NIL after
ATTEMPTS laps of PAUSE seconds."
  ;; The kernel answers a refused connect in microseconds, so a lap against a
  ;; listener that is not up yet costs one PAUSE — where an HTTP client's
  ;; connect timeout charges its full second for the same refusal. Generic on
  ;; purpose: anything that just started a local acceptor awaits it here.
  (loop repeat attempts
        thereis (with-handlers ((error () (sleep pause)))
                  (usocket:socket-close (usocket:socket-connect "127.0.0.1" port))
                  t)))

;;; STOP-GATEWAY joins it (bounded) before stopping the handler: the probe is
;;; the proof the acceptor is live, and a stop that outran it is the
;;; suite-teardown UNBOUND-SLOT flake.
(defvar *gateway-acceptance-probe* nil
  "The thread running the latest START-GATEWAY's application probe, or NIL.")

(defun await-gateway-acceptance (port &key (attempts 100))
  "Block until the freshly started acceptor ACCEPTS one request — the
start-confirmed latch."
  ;; clackup returns before the handler thread finishes hunchentoot's start.
  ;; Without this, a stop that arrives inside that window would die with
  ;; UNBOUND-SLOT on the taskmaster's ACCEPTOR-PROCESS slot *in the handler
  ;; thread*, where no caller-side handler can reach it. A completed
  ;; round-trip proves the accept loop is live, so a later stop can never
  ;; outrun startup. Any HTTP response counts — dexador signals on 4xx and an
  ;; unauthenticated status probe is a response all the same; only transport
  ;; errors mean the listener is not up yet.
  ;;
  ;; Two stages: AWAIT-PORT first — an HTTP probe would otherwise burn
  ;; dexador's whole connect timeout on a refusal the kernel answers in
  ;; microseconds. The raw poll finds the listener within milliseconds; the
  ;; HTTP round trip then completes on its first lap. ATTEMPTS keeps its
  ;; proportional meaning for callers that bound the wait small. The round
  ;; trip itself runs on its own thread, so the boot path never waits for it.
  (unless (await-port port :attempts (* 20 attempts))
    (return-from await-gateway-acceptance nil))
  (nlk:note-boot-phase "gateway-accepted-raw" port)
  (setf *gateway-acceptance-probe*
        (spawn "gateway-acceptance-probe"
          (http-answers-p port :attempts attempts)
          (nlk:note-boot-phase "gateway-accepted-http" port)))
  t)

(defun http-answers-p (port &key (attempts 100))
  "Whether 127.0.0.1:PORT answers one HTTP request within ATTEMPTS laps."
  ;; Any response counts -- NLK:HTTP answers a 4xx as a value, and an
  ;; unauthenticated probe is a response all the same; only a transport error
  ;; is another lap.
  (loop repeat attempts
        thereis (with-handlers ((error () (sleep 0.02)))
                  (nlk:http :head (format nil "http://127.0.0.1:~d/healthz" port) :timeout 1)
                  t)))

(defun local-port (port &aux (socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
  "PORT once 127.0.0.1 binds it -- 0 takes the kernel's ephemeral pick; a port
another listener holds signals USOCKET:ADDRESS-IN-USE-ERROR."
  ;; Bind and release: clackup binds on its own handler thread, where a bind
  ;; failure never reaches the caller, and a listener already on the port
  ;; would answer every probe in its place.
  (with-cleanup ((usocket:socket-close socket))
    (usocket:get-local-port socket)))

;;; A client that hangs up mid-answer -- a browser leaving a page, a shell
;;; quitting inside a history read -- fails the next write to its socket.
;;; Clack answers that failure by writing a 500 to the same socket, which fails
;;; inside hunchentoot's WITH-DEBUGGER, and with the debugger disabled that
;;; exits the whole process: the gateway, and every session it serves. The
;;; hook below ends that one connection's thread instead, and hands anything
;;; else on to whatever hook the process has. (ACCEPTOR T) is hunchentoot's
;;; own :around, so this one names the socket's class.
(defmethod hunchentoot:process-connection :around
    ((acceptor hunchentoot:acceptor) (socket usocket:stream-usocket))
  (let ((hook sb-ext:*invoke-debugger-hook*))
    (catch 'client-gone
      (let ((sb-ext:*invoke-debugger-hook*
              (lambda (condition next)
                (when (and (typep condition 'stream-error)
                           (eq (stream-error-stream condition) (usocket:socket-stream socket)))
                  (throw 'client-gone nil))
                (when hook (funcall hook condition next)))))
        (call-next-method)))))

(defun serve-local (app &key (port 0))
  "Serve the clack APP on 127.0.0.1:PORT (0: an ephemeral one) => (values
HANDLER PORT), once it has answered a request. A bound PORT signals
USOCKET:ADDRESS-IN-USE-ERROR."
  ;; An acceptor that never answers signals too, and no exit --
  ;; that one, or an interrupt while the probe waits -- leaves the acceptor
  ;; behind. STOP-CLACK-HANDLER takes it down.
  (let* ((bound (local-port port))
         (handler (clack:clackup app :address "127.0.0.1" :port bound :silent t :debug nil))
         (live nil))
    (with-cleanup ((unless live (stop-clack-handler handler)))
      (unless (and (await-port bound :attempts 2000) (http-answers-p bound))
        (error "nothing started accepting on 127.0.0.1:~d" bound))
      (setf live t)
      (values handler bound))))

;;;
;;; Hunchentoot's default is 20s, and clack passes no :read-timeout when it
;;; builds the acceptor, so the default silently becomes gateway policy — and it
;;; closed the gateway's own local WS peer at every 20s of idle, forcing a
;;; reconnect + full-session replay cycle that promoted ~15MB/min of floating
;;; garbage (the 328MB-RSS incident). The WS client now pings inside that window
;;; (*sync-keepalive-seconds*, client.lisp), so this value is a backstop that
;;; reaps genuinely dead peers and idle HTTP keep-alive connections, not the
;;; liveness mechanism. It must stay comfortably above the ping cadence.
(defparameter *gateway-connection-read-timeout* 75
  "Socket read timeout, in seconds, for every gateway connection.")

(defun start-gateway (&rest keys &key port operator-token workspace-captures trim)
  "Start the HTTP/WS gateway with one materialized operator token and return
the port it accepted on: the listener first (OPEN-GATEWAY-LISTENER), then
everything the endpoint lets other processes reach (SERVE-GATEWAY). TRIM gives
the boot's pages back once the peripherals are up (a headless owner's boot)."
  (declare (ignore port operator-token workspace-captures))
  (when *gateway-handler*
    (warn "gateway already running; call stop-gateway first")
    (return-from start-gateway *gateway-bound-port*))
  ;; Every key but TRIM rides to OPEN-GATEWAY-LISTENER only when the caller
  ;; said it. Without this, an unspecified WORKSPACE-CAPTURES would arrive
  ;; there as an explicit NIL and override its config-read default, so every
  ;; headless owner (`serve', `gateway run') would run with captures off while
  ;; a TUI owner, which opens the listener directly, would still run with them
  ;; on.
  (serve-gateway (apply #'open-gateway-listener (alexandria:remove-from-plist keys :trim))
                 :trim trim))

;;; The port the store's gateway bound last is taken again while nothing else
;;; holds it, so what that gateway served -- a browser tab, a bookmark -- finds
;;; it after a restart. The KV row keys on (session, key) and wants no session,
;;; so it never shows in a picker.
(defun remembered-port ()
  "The port this store's gateway bound last, while nothing else holds it; else NIL."
  (when-let (last (and (nlk:store-open-p)
                       (parse-integer (or (nlk:session-state-get "gateway" "port") "")
                                      :junk-allowed t)))
    (handler-case (local-port last)
      (usocket:socket-error () nil))))

(defun open-gateway-listener (&key (port *gateway-port*) operator-token
                      (workspace-captures
                       ;; An absent file, section or key, or an unreadable file, says nothing.
                       (with-handlers ((error () nlk::*workspace-capture-enabled*))
                         (nlk:config-boolean (json-value (read-shared-config) :object "workspace")
                                             "captures" nlk::*workspace-capture-enabled*))))
  "Bind the gateway's listener and wait for its first acceptance — one
materialized operator token, the bound port and a live acceptor — and
return the port it accepted on; the endpoint is published later, by
SERVE-GATEWAY."
  ;; PORT 0 (the default) binds the port the store's gateway had last
  ;; (REMEMBERED-PORT) when it is free, else a kernel-assigned one; either way
  ;; the bound port lands in the store's holder sidecar, so attachers never
  ;; depend on a fixed number. WORKSPACE-CAPTURES sets the boundary-capture
  ;; policy. The kill switch is a true global (SB-EXT:DEFGLOBAL): every thread
  ;; reads the same variable, and the SETF below is a real process-wide policy
  ;; change. It DEFAULTS to the shared config's workspace.captures, and when
  ;; the config says nothing, to the policy already in force — so a caller
  ;; that says nothing cannot re-arm captures the test runner switched off (a
  ;; fixture session's default cwd is the live repo, and captures would ADD -A
  ;; it into a shadow store). Only an explicit argument or an explicit config
  ;; key changes the policy.
  (setf nlk::*workspace-capture-enabled* workspace-captures
        *gateway-operator-token* (json-value (or operator-token
                                                 (uiop:getenv "NODECODE_GATEWAY_OPERATOR_TOKEN")
                                                 (uiop:getenv "NODECODE_OPERATOR_TOKEN"))
                                             :text))
  (unless *gateway-operator-token* (error "Gateway operator token is required"))
  ;; A global SETF, not a dynamic LET: clackup builds the acceptor on a
  ;; spawned handler thread, where a caller-side binding would be invisible.
  ;; The acceptor's read-timeout slot initform reads this special at
  ;; make-instance time, and thread creation orders the setf before it.
  (setf hunchentoot:*default-connection-timeout*
        *gateway-connection-read-timeout*)
  ;; Port 0: bind 127.0.0.1:0, read the kernel's assignment, release — the
  ;; ephemeral allocator never hands out a port still bound or lingering.
  (let ((bound (if (zerop port) (or (remembered-port) (local-port 0)) port)))
    (setf *gateway-handler*
          (clack:clackup (make-sync-app)
                         :port bound
                         :silent t
                         :debug nil))
    (nlk:note-boot-phase "gateway-clacked" bound)
    (unless (await-gateway-acceptance bound)
      ;; clackup binds on its own handler thread, so a bind failure never
      ;; signals here — a listener that never confirms is how it presents.
      ;; Fail the start loudly instead of returning a port nobody serves.
      (stop-clack-handler (shiftf *gateway-handler* nil))
      (error "gateway never started accepting on port ~d" bound))
    (nlk:note-boot-phase "gateway-listening" bound)
    (when (nlk:store-open-p)
      (nlk:session-state-put "gateway" "port" (princ-to-string bound)))
    (setf *gateway-bound-port* bound
          *gateway-started-at* (nlk::unix-now))
    bound))

(defun serve-gateway (bound &key trim &aux resumable back)
  "Make BOUND's listener reachable and live: the hooks, the endpoint in the
store's holder sidecar, the background watcher, recovery, the
update poller and the folder's peripherals, TRIM as START-GATEWAY takes it.
=> BOUND."
;; What this process is, on the manual while this gateway serves.
  (set-help-topic :image *image-help-summary* (image-help-text))
  ;; The index's command, ahead of every cell's: the composer's `$' opens the
  ;; first command that names it (COMPOSER-TRIGGER-COMMAND).
  (register-command "index" "index" 'index-slash
                    :description "Knowledge: the meter and the picker; reload re-pins; a text finds hot and cold"
                    :argument-hint "[reload | TEXT]" :trigger "$")
  ;; Another coding agent's home brought in (src/src/import/): the setup
  ;; walk's route, and the watch over any bot an earlier import held for a
  ;; foreign gateway still running. /import is a builtin (tui/slash.lisp).
  (route "/api/import" #'nik:import-route)
  (nik:start-watch)
  ;; What this image makes live from a file of its trees reaches every
  ;; attached shell in another process while this gateway serves: the
  ;; kernel's seam is advised here, the way the harness read is.
  (nlk:hook 'nlk:source-loaded "gateway-loaded"
            (lambda (next path definitions)
              (funcall next path definitions)
              (announce-loaded path definitions)))
  ;; A hook an isolated chain stepped over is said to every attached shell
  ;; and stands on the board until it is registered again or unhooked.
  (nlk:hook 'nlk::note-hook-failure "gateway-notice"
            (lambda (next name key condition)
              (declare (ignore next))
              (notice (and condition (nlk::hook-failure-text name key condition))
                      :level :error :key (nlk::hook-failure-key name key))))
  ;; What this boot picks up, and what it has to tell of it, before the
  ;; endpoint is published: the turns it picks up are told at their first
  ;; round, and the rest once the cells are up to hear it.
  (setf resumable (ignore-errors (nlk::recoverable-interrupted-turns)))
  (when (nlk:store-open-p)
    (setf back (ignore-errors (note-back resumable))))
  ;; The listener is bound: advertise the endpoint in the store's holder
  ;; sidecar so a contender that loses the ownership lock attaches here
  ;; instead of refusing. No-op when this process owns no open store
  ;; (gateway tests run against fixture state).
  (ignore-errors (nlk::publish-store-endpoint bound))
  (nlk:note-boot-phase "endpoint-published" bound)
  ;; The background-exec watcher: 200 ms tick projecting live-only shell
  ;; output frames and appending the durable exit fact per FINAL update.
  (start-background-exec-watcher)
  ;; The running-session map is read again from this log at its first use.
  (reset-session-activity)
  ;; The last restart's record on the board, then the channel polled on
  ;; the config's cadence (update.lisp).
  (start-update-poller)
  (nlk:note-boot-phase "update-polled")
  ;; And the launch image kept in step with the folder for as long as this owner
  ;; lives: a launch that meets a moved folder loads it in front of the
  ;; operator's first frame, and an owner outliving the edit is exactly the
  ;; process that can pay that build instead (launch.lisp, the keeper).
  (nlk::start-launch-image-keeper)
  (nlk:note-boot-phase "launch-image-keeper")
  ;; This self serves: the receipt it left before its load (generation.lisp)
  ;; is no evidence of a death.
  (nlk:take-self-receipt)
  ;; The turns it picks up are dispatched behind the cells.
  (start-cells-behind-the-listener back :trim trim :resumable resumable)
  bound)

(defun start-cells-behind-the-listener (back &key trim resumable)
  "Start the folder's peripherals on their own thread, now that the listener
is bound and its endpoint published, and say beside them what this boot has
to tell: BACK, how it came back (NOTE-BACK), and a release it runs for the
first time. RESUMABLE, the turns the recovery pass marked, are dispatched once
the peripherals are up (START-GATEWAY-RECOVERY). With TRIM the boot's pages go
back after them (NLK:TRIM-MEMORY)."
  ;; Nothing the operator's shell waits for is behind them — the shell ensures
  ;; its session, completes its handshake and is sent its replayed transcript
  ;; the moment the sidecar names the port (*REPLAY-SENT-SEMAPHORE*) — while
  ;; the peripherals spawn processes (MCP servers, channel hosts, a browser
  ;; bridge) whose startup would otherwise share that window with it. An
  ;; organism no shell attaches to waits out the timeout. The token check moves with them: a listener whose auth died during
  ;; cell startup is the worst failure shape — it serves on its published
  ;; port while rejecting every bearer — so the gateway is torn down and the
  ;; reason said here instead of refusing a boot that has already handed the
  ;; shell its frame. A gateway stopped, or replaced, while this waits owns
  ;; no peripherals: without this, they would start for a dead listener, and
  ;; a later gateway's token would read as a disturbed one and end the process.
  (nlk:note-boot-phase "cells-start")
  (let ((handler *gateway-handler*)
        (sent (setf *replay-sent-semaphore* (bt2:make-semaphore :name "replay-sent"))))
    (spawn "start-cells"
      (bt2:wait-on-semaphore sent :timeout 3)
      (when (eq handler *gateway-handler*)
        ;; One note, in the model's words, on a thread of its own: the shell
        ;; hears it once the model answers, not once every peripheral is up,
        ;; and a channel host that starts after it reads it at its start
        ;; (ORGANISM-NOTE).
        (spawn "boot-note"
          ;; a restore the last launch made is recorded here, marker taken, and said with the rest
          (say-note (format nil "~{~a~^~%~%~}"
                            (remove nil (list (ignore-errors (nlk:take-restoring-marker))
                                              back (ignore-errors (boot-release)))))))
        (let ((materialized *gateway-operator-token*))
          (start-cells)
          (nlk:note-boot-phase "cells-started")
          ;; The background gateway is on by default, a user service, as
          ;; Hermes' setup offers first: a home nodecode names comes up at
          ;; every login from its first boot, until the operator turns it
          ;; off -- /settings, `nodecode gateway off'.
          (when (nlk:gateway-by-default-p)
            (keep-running "your sessions"))
          ;; The same gateway's token moved under the cells; one stopped
          ;; meanwhile took its token with it, and is no fault of theirs.
          (when (and (eq handler *gateway-handler*)
                     (not (equal *gateway-operator-token* materialized)))
            (ignore-errors (stop-gateway))
            (error "cell startup disturbed the gateway operator token")))
        ;; The turns this boot picks up, and the prompts stranded behind
        ;; finished ones, run once the cells are up: a lane, tool or channel a
        ;; cell brings exists only after its START-CELL, and a turn pinned to
        ;; one (claude-code/...) resumed ahead of it failed as a config error
        ;; 2 s into its boot. Their sessions stay durably active
        ;; meanwhile (ACTIVE-TURN-P), so a prompt landing first queues behind.
        (with-handlers ((error (condition)
                          (warn "the turns this boot picks up were not dispatched: ~a"
                                condition)))
          (start-gateway-recovery resumable))
        (nlk:note-boot-phase "recovery-done")
        ;; Behind the peripherals: a definition being mended may call what
        ;; one of them makes as it starts (an MCP server's tools).
        (with-handlers ((error (condition)
                          (warn "the layer's files this boot did not load were not handed on: ~a"
                                condition)))
          (hand-broken-layer))
        (with-handlers ((error (condition)
                          (warn "the floor this boot stands on was not handed on: ~a" condition)))
          (hand-failed-self))
        (when trim (nlk:trim-memory))))))

(defun stop-gateway ()
  "Stop the running gateway, if any, and retract the published endpoint."
  (when *gateway-handler*
    ;; When and why it stopped, for the next boot's note.
    (ignore-errors (note-stopped))
    (setf *stop-reason* nil *organism-note* nil)
    ;; A stopping gateway advertises nothing first: a shell that launches
    ;; while the peripherals stop would attach to an organism on its way out,
    ;; and dial its port again after it went. The sidecar keeps the pid, so
    ;; the shell waits for the lock instead (RUN-CHAT-GATEWAY).
    (ignore-errors (nlk::publish-store-endpoint nil))
    ;; Cells are clients of this gateway: they stop before their server.
    (nlk:note-phase "stop cells")
    (ignore-errors (stop-cells))
    (ignore-errors (nik:stop-watch))
    (nlk:note-phase "stop execs")
    ;; Durable exit facts must outrun the watcher's death: kill every child
    ;; (none may outlive the image that owns its pipes), wait for the
    ;; drain-gated exits, run one final collection so every backgrounded
    ;; shell's exit fact lands, and only then stop the watcher and reclaim
    ;; the registry. Authority note: live Zig unwinds watcher-join before
    ;; Registry.deinit and so loses these facts — shells alive at shutdown
    ;; replay "running" forever, against protocol/background_exec.zig's
    ;; own replay contract. The contract outranks the Zig unwind order.
    (ignore-errors (terminate-all-execs (exec-registry-instance)))
    (ignore-errors (exec-await-all-exits (exec-registry-instance)))
    (ignore-errors (background-exec-watcher-tick))
    (setf *background-exec-watcher* (worker-stop *background-exec-watcher*))
    (ignore-errors (destroy-all-execs (exec-registry-instance)))
    ;; The application probe still in flight proves the acceptor live; a
    ;; stop must not outrun it.
    (when *gateway-acceptance-probe*
      (nlk::%join-bounded *gateway-acceptance-probe* 5)
      (setf *gateway-acceptance-probe* nil))
    (nlk:note-phase "stop listener")
    (stop-clack-handler *gateway-handler*)
    (nlk:note-phase "stop update-poller")
    (ignore-errors (stop-update-poller))
    (set-help-topic :image nil)
    (nlk:unhook 'nlk:source-loaded "gateway-loaded")
    (nlk:unhook 'nlk::note-hook-failure "gateway-notice")
    (setf *gateway-handler* nil
          *gateway-bound-port* nil
          *gateway-started-at* nil
          *gateway-operator-token* nil))
  t)

;;; The headless serve entry lives in client.lisp (SERVE-ORGANISM-CANDIDATE):
;;; it shares the ownership boot spine with the foreground entry and adds
;;; election etiquette (probe first, lose quietly on STORE-IN-USE), which the
;;; old START-GATEWAY-ORGANISM here lacked — that entry served without the
;;; shared operator token, so attaching first-party clients could not
;;; authenticate against it.

;;; --- the one idle exit, and the restore to a sealed generation ---------------
;;;
;;; One exit for everything that wants the next launch to run something else:
;;; a release whose boot state this image will not evaluate (update.lisp
;;; UPGRADE-ORGANISM) and a restore to a sealed generation (ROLLBACK, over
;;; generation.lisp RESTORE-GENERATION). It was the update's own, under the
;;; update's vocabulary; it is the gateway's because the gateway is what ends.
;;; A shell's organism runs the next boot state at its next launch, which the
;;; operator makes by reopening it; the gateway a unit runs has no operator
;;; to, so it exits once no turn is in flight and the unit -- systemd's
;;; Restart=always, launchd's KeepAlive through the launcher -- starts it
;;; again on `current'; a shell fronting its organism re-fronts a fresh one
;;; the same way (client.lisp, the reconnect ladder).

(defparameter *idle-poll-seconds* 5
  "How often a restart waiting for an idle moment asks whether a turn is in
flight; also its first wait, so the answer that asked for it is out before
the process goes.")

(defvar *restart-wait* nil
  "The thread waiting for an idle moment to exit this gateway, or NIL.")

(defvar *restart-reason* nil
  "Why the exit below is taken, for the next boot's note: the update's, or a
restore's.")

(defvar *restart-exit* 'exit-for-restart
  "What ends this gateway for the next launch, a function of what that launch
runs (a release label, `generation N'). A test binds a recorder.")

(defun exit-for-restart (to)
  (format t "~&restart: ~a runs at the next launch; exiting for it~%" to)
  (finish-output)
  (setf *stop-reason* (or *restart-reason* "a restart"))
  ;; Stopped and closed here, then gone at once. Left to the main thread's
  ;; unwind, SBCL's exit would end the listener thread first and the server's
  ;; own stop would fail joining it; a unit's stop by signal is no cleaner.
  (ignore-errors (stop-gateway))
  (ignore-errors (nlk:close-store))
  (sb-ext:exit :code 0 :abort t))

(defun restart-when-idle (to &key (reason "an update"))
  "Exit this gateway once no turn is in flight, for the next launch to run
TO, REASON why: one wait at a time, and a later request rides the same one."
  (unless (and *restart-wait* (bt2:thread-alive-p *restart-wait*))
    (setf *restart-reason* reason
          *restart-wait*
          (spawn "restart-wait"
            (loop do (sleep *idle-poll-seconds*)
                  while (ignore-errors (nlk:any-active-turn-p)))
            (funcall *restart-exit* to)))))

(defun rollback (number &key (restart (and *gateway-handler* t)))
  "Restore the folder to generation NUMBER -- the operator's side, every row
converged to its manifest (NLK:RESTORE-GENERATION), while the binary and its
shipped cells stay on the release this organism runs -- and, under RESTART
(the default while this organism serves), exit once no turn is in flight so
the next launch runs it. => what was done, in words: the model's and the
operator's (rollback N)."
  ;; A cell advises the restore with (hook 'rollback ...). Nothing holds the
  ;; channel: the poller moves only the kernel's side, and only forward
  ;; (update.lisp CHECK-FOR-UPDATE).
  (let ((lines (nlk:restore-generation number)))
    (when restart
      (restart-when-idle (format nil "generation ~d" number) :reason "a restore"))
    (format nil "generation ~d ~a~a" number
            (if lines
                (format nil "restored: ~{~a~^; ~}" lines)
                "is what the folder already stood at")
            (if restart
                "; this organism exits once no turn is in flight, and the next launch runs it"
                "; the next launch runs it"))))

(defun restore-refusal (steps)
  "Why the restore STEPS take can never work, every reason whole -- a folder
nothing can put back -- or NIL. Uncommitted edits hold a restore without
refusing it: keeping or discarding them mends it."
  (when-let (reasons (nlk:step-refusals steps))
    (format nil "~{~a~^; ~}" reasons)))

(defun edits-json (names)
  "The folders NAMES, whose uncommitted edits hold every restore, as the
generations routes carry them: each with how many paths it would keep."
  (map 'vector (lambda (name)
                 (json-object "name" name
                              "count" (length (nlk:edit-paths (nlk::folder-under name nlk::*cells-directory*)))))
       names))

(defun history-json (change refusal)
  "One change of the history (NLK:GENERATION-HISTORY) as the list route
carries it, REFUSAL why going back to it can never work, or NIL."
  (json-object (:getf change "kind" "from" "to" "first_at" "at" "title" "where" "version")
               :when (getf change :nodecode) "nodecode" t
               :opt "notes" (and (getf change :notes) (coerce (getf change :notes) 'vector))
               :opt "keeps" (and (getf change :keeps) (coerce (getf change :keeps) 'vector))
               :opt "files" (when-let (files (getf change :files))
                              (destructuring-bind (count added removed paths) files
                                (json-object "count" count "added" added "removed" removed
                                             "paths" (coerce paths 'vector))))
               "refusal" (or refusal :null)))

(defvar *generations-memo* nil
  "(KEY . CHANGES): the history GENERATIONS-ANSWER folded last, KEY the
manifests' own text and the shipped cells it was read against.")

(defun memo-generation-history (manifests builtin)
  "NLK:GENERATION-HISTORY of MANIFESTS against BUILTIN, folded once for the
same manifests: a sealed manifest never changes, so the same manifests fold
to the same changes -- six seconds of git reads on a home of 775 -- and a
room's card paged, or a sheet read again, folds them once until the next
seal, which folds them whole again. A
session renamed since keeps its old title in WHERE until the next seal."
  (let ((key (list (nlk:short-digest (nlk:encode-json-object (coerce manifests 'vector)) 16) builtin))
        (memo *generations-memo*))
    (if (and memo (equal key (car memo)))
        (cdr memo)
        (let ((changes (nlk:generation-history manifests :builtin builtin)))
          (setf *generations-memo* (cons key changes))
          changes))))

(defun generations-answer ()
  "The sealed generations as the changes they were (NLK:GENERATION-HISTORY),
newest first, each with why going back to it can never work when it cannot;
the generation the folder stands at, null when none; the binary's version,
and whether a restore moves nodecode itself -- it follows a release, never a
checkout; and the folders whose uncommitted edits hold every restore until
they are kept or discarded. The list route's answer, and the history a
/generations without a shell reads (HEADLESS-GENERATIONS)."
  ;; A folder that does not read (no cells directory) still lists: nothing
  ;; then says where it stands, or what going back would refuse.
  (let* ((manifests (nlk:manifests))
         (read (ignore-errors (list (nlk:self-rows))))
         (rows (first read))
         (current (and read (nlk:sealed-generation (nlk:self-identity rows) manifests)))
         (builtin (mapcar #'nlk:offer-name (nlk:shipped-cells)))
         (by-number (alexandria:alist-hash-table
                     (mapcar (lambda (m) (cons (nlk::manifest-number m) m)) manifests))))
    (json-object
     "now" (if current (nlk::manifest-number current) :null)
     "version" nlk::*binary-version*
     "nodecode_follows" (and (find-if (lambda (row)
                                        (and (equal "release" (getf row :origin))
                                             (member (getf row :name) builtin :test #'equal)))
                                      rows)
                             t)
     "edits" (edits-json (mapcar (lambda (row) (getf row :name)) (nlk:edits-rows rows)))
     "changes" (map 'vector
                    (lambda (change &aux (to (getf change :to)))
                      (history-json change
                                    (and read
                                         (restore-refusal
                                          (nlk:restore-steps to (nlk::manifest-rows (gethash to by-number)) rows)))))
                    (memo-generation-history manifests builtin)))))

(defun generation-plan-answer (to)
  "What restoring generation TO would do to the folder as it stands, read
before anything moves -- a line a folder that moves or stays
(NLK:PLAN-LINES), why it can never work when it cannot, and the folders
whose uncommitted edits hold it: the plan route's answer, and the question
a /generations N without a shell asks."
  (multiple-value-bind (steps manifest) (nlk:restore-plan to)
    (json-object "to" to
                 "sealed_at" (json-value manifest :string "sealed_at")
                 "lines" (coerce (nlk:plan-lines steps to :version (json-value manifest :string "binary" "version"))
                                 'vector)
                 "refusal" (or (restore-refusal steps) :null)
                 "edits" (edits-json (nlk:step-edits steps)))))

(defun generation-edits-text (name action)
  "The uncommitted edits of the folder NAME kept, for ACTION `keep' -- one
commit, the folder it leaves proved as the next generation (NLK:KEEP-EDITS)
-- or set aside into its repository's stash, for `discard'
(NLK:DISCARD-EDITS): the organism's sentence for it."
  (if (equal action "keep")
      (multiple-value-bind (sha verdict detail) (nlk:keep-edits name)
        (if (zerop (length sha))
            (format nil "~a had nothing uncommitted to keep; ~a" name (seal-words verdict detail))
            (format nil "Kept ~a's uncommitted edits as ~a; ~a" name sha (seal-words verdict detail))))
      (multiple-value-bind (count verdict detail) (nlk:discard-edits name)
        (format nil "Discarded ~d uncommitted edit~:p in ~a (git stash list there still has them); ~a"
                count name (seal-words verdict detail)))))

(define-gateway-route handle-gateway-generations-http (env)
  "GET /api/gateway/generations: the sealed generations as the changes they
were, where the folder stands and the edits that hold a restore
(GENERATIONS-ANSWER)."
  (:route :get "/api/gateway/generations" "generations")
  (generations-answer))

(define-gateway-route handle-gateway-generation-plan-http (env)
  "GET /api/gateway/generations/plan?to=N: what restoring generation N
would do to the folder as it stands, read before anything moves -- a line a
folder that moves or stays (NLK:PLAN-LINES), why it can never work when it
cannot, and the folders whose uncommitted edits hold it."
  (:route :get "/api/gateway/generations/plan" "plan")
  (let ((to (gateway-query-integer env "to")))
    (require-request to "plan takes ?to=N, a generation's number")
    (generation-plan-answer to)))

(defun seal-words (verdict detail)
  "What PROVE-SELF's VERDICT and DETAIL say of the folder a keep or a
discard left, as the end of its sentence."
  (case verdict
    (:sealed (format nil "sealed as generation ~d" detail))
    (:current "the folder stands at a generation already sealed")
    (:red (format nil "the folder did not prove, so no generation stands for it: ~{~a~^; ~}" detail))
    (t "it seals once the folder proves")))

(define-gateway-route handle-gateway-generation-edits-http (env)
  "POST /api/gateway/generations/edits {name, action}: the uncommitted edits
of the folder NAME kept -- one commit, the folder it leaves proved as the
next generation (NLK:KEEP-EDITS) -- or, for `discard', set aside into its
repository's stash (NLK:DISCARD-EDITS); the answer carries the sentence."
  (:route :post "/api/gateway/generations/edits" "edits")
  (let* ((body (gateway-request-json env))
         (name (json-value body :string "name"))
         (action (json-value body :string "action")))
    (require-request (and name (member action '("keep" "discard") :test #'equal))
                     "edits takes {\"name\": FOLDER, \"action\": \"keep\" or \"discard\"}")
    (json-object "text" (generation-edits-text name action))))

(define-gateway-route handle-gateway-rollback-http (env)
  "POST /api/gateway/generations/rollback {number}: the folder restored to
that generation, this organism exiting once idle for the next launch to run
it; the answer carries the sentence the verb prints."
  (:route :post "/api/gateway/generations/rollback" "rollback")
  (let* ((body (gateway-request-json env))
         (number (json-value body :integer "number")))
    (require-request number "rollback takes {\"number\": N}, the generation's number")
    (json-object "text" (rollback number))))

(defun generations-main (words)
  "The `generations' command: the home's sealed generations, from its
manifests. Runs before any folder loads, like `update'."
  (answer-and-quit (progn (when words (error "generations takes no argument: nodecode generations"))
                          (values (nlk:generations) 0 :output))))

(defun rollback-main (words)
  "The `rollback N' command: asked of the serving organism over its route when
one runs -- it converges and exits once idle -- else converged here, the next
launch running N."
  (answer-and-quit
    (let ((number (and (= 1 (length words)) (ignore-errors (parse-integer (first words)))))
          (port (organism-port)))
      (unless number
        (error "rollback takes one argument, the generation's number: nodecode rollback N"))
      (values (if port
                  (answer-text (gateway-http-session-request
                                port :post "/api/gateway/generations/rollback"
                                (json-object "number" number)
                                (gateway-configured-operator-token)
                                :connect-timeout 3 :read-timeout 600))
                  (rollback number :restart nil))
              0 :output))))

;;; --- the floor handed to the model -------------------------------------------

(defparameter *failed-self-request*
  "[self: ~a. This launch runs pristine: generation 0, the binary alone -- nothing of the folder (~a) loaded, no image followed, and nothing is kept while it runs. What was sealed:
~a
(rollback N) restores one of them, and the next launch runs it. Or mend the folder by hand -- the file that kills the load, or its .asd -- and the next launch retries it. Answer with what you chose.]"
  "What the layer's session is handed at a boot that runs pristine because
the self before it died before serving. Data, so a layer can reword it.")

(defun hand-failed-self ()
  "Hand the model the floor it stands on, in the session `layer', once per
death: this boot runs pristine because the self before it died before serving
(generation.lisp DECIDE-PRISTINE). => the marker's count when handed, else NIL."
  ;; The gateway's state `failed-self' keeps the count handed, so a boot that
  ;; holds the floor hands nothing twice and a new death is handed again.
  (when (and nlk:*pristine* (nlk:store-open-p))
    (let* ((marker (nlk:read-death-marker))
           (deaths (and marker (json-value marker :integer "deaths")))
           (handed (ignore-errors
                    (json-value (nlk:decode-json (nlk:session-state-get "gateway" "failed-self"))
                                :integer "deaths"))))
      (when (and deaths (not (eql deaths handed)))
        (let ((folder (namestring (or nlk::*cells-directory* (nlk:home)))))
          (unless (nlk:session-exists-p "layer")
            (nlk:standby-session :id "layer" :cwd folder))
          (submit "layer"
                  (format nil *failed-self-request*
                          (json-value marker :string "line") folder (nlk:generations))
                  :source "layer"))
        (nlk:session-state-put "gateway" "failed-self"
                               (encode-json-object (json-object "deaths" deaths)))
        deaths))))
