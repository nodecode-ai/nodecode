;;;; protocol.lisp --- typed Gateway sync protocol.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One wire contract, and this file owns it: the WebSocket Gateway's
;;;; versioned handshake, client-command envelopes, projection envelopes,
;;;; cursors, and refusal errors. Gateway callers deal in typed structs and
;;;; validated discriminators; only this file deals in JSON hash tables.

(in-package #:nodecode.kernel)

(defconstant +gateway-protocol-version+ 1
  "The only Gateway sync protocol version currently accepted.")

;;; FIELD, when a refusal is about one member of the request, names it: the
;;; field a form puts the hand back on.
(define-error protocol-error (error) (code message (field :initform nil)) (:export)
  (:report "~a: ~a" code message))

(defun protocol-refuse (code format-control &rest args)
  (error 'protocol-error
         :code code
         :message (apply #'format nil format-control args)))

;;; Inbound, the gateway refuses a larger command before decoding it;
;;; outbound, a replay chunk splits to stay under it. A prompt's staged images
;;; ride its start_turn as base64, so this is also what one Enter can carry:
;;; 16 MiB is two images at a provider's 5 MB ceiling, or a dozen screenshots.
;;; The socket's own bound (fast-websocket) is 64 MiB.
(defconstant +max-sync-frame-bytes+ 16777216
  "The largest WebSocket text frame the sync transport carries, in characters
of its encoded JSON (bytes, for the base64 that makes a frame large), either
direction.")

(define-error frame-too-large (protocol-error) (bytes bound)
  (:export)
  ;; A PROTOCOL-ERROR, so the gateway's refusal arm encodes it like any other;
  ;; a class of its own, so a client can tell a frame that never left its
  ;; process from a transport that may have taken one.
  (:documentation "One encoded sync frame of BYTES past the BOUND of
+MAX-SYNC-FRAME-BYTES+."))

(defun check-sync-frame-size (json &aux (bytes (length json)))
  "JSON, one encoded sync frame, when it fits +MAX-SYNC-FRAME-BYTES+; else
FRAME-TOO-LARGE naming both sizes."
  ;; The gateway asks before decoding what a peer sent; a client asks before
  ;; writing what it encoded, so a frame the gateway would refuse never leaves
  ;; the process that built it.
  (when (> bytes +max-sync-frame-bytes+)
    (error 'frame-too-large
           :code :frame_too_large :bytes bytes :bound +max-sync-frame-bytes+
           :message (format nil "a ~,1f MiB frame exceeds the ~d MiB sync frame bound"
                            (/ bytes 1048576.0)
                            (floor +max-sync-frame-bytes+ 1048576))))
  json)

(define-record (sync-message (:copier nil) (:export :readers))
  "Validated outer Gateway envelope."
  ;; PAYLOAD is a JSON object for projections or a GATEWAY-COMMAND for
  ;; client_command frames.
  (type (error "sync message type required") :type keyword :read-only t)
  (protocol-version nil :type (or null integer) :read-only t)
  (sequence nil :type (or null integer) :read-only t)
  (session-id nil :type (or null string) :read-only t)
  (cursor nil :type (or null integer) :read-only t)
  (payload nil :read-only t))

(define-record (gateway-command (:copier nil) (:export :constructor :readers))
  "Validated client command. Subscription commands carry no command identity."
  (kind (error "command kind required") :type keyword :read-only t)
  (command-id nil :type (or null string) :read-only t)
  (correlation-id nil :type (or null string) :read-only t)
  (request (error "command request required") :type hash-table :read-only t))

(defun encode-inner-event (type payload &key metadata)
  "Build a reducer-compatible inner event envelope as a JSON object."
  (required-string nil "event.type" type)
  ;; PAYLOAD may be a JSON-VERBATIM: a logged fact's payload as the log keeps
  ;; it, an object by the hash it was written under (events.lisp STORED-FACT).
  (make-json-object "type" type
                    "payload" (if (json-verbatim-p payload)
                                  payload
                                  (protocol-object (or payload (make-hash-table :test #'equal))
                                                   "event.payload"))
                    "metadata" (protocol-object
                                 (or metadata (make-hash-table :test #'equal))
                                 "event.metadata")))

(defun decode-inner-event (object)
  "Validate one inner projection event envelope and answer it."
  ;; Every producer in this image calls this for the refusal, never for a
  ;; carrier: the envelope must be an object, its "type" a non-empty string, a
  ;; present "payload" an object, and "metadata" an object when present. The
  ;; discriminator itself is deliberately NOT checked here — a foreign
  ;; gateway's newer fact must not cost the replay frame it rode in — and the
  ;; client folds the raw JSON below (client.lisp APPLY-GATEWAY-EVENT), so
  ;; nothing typed stands between the wire and the reducer.
  (protocol-object object "event")
  (required-string object "type")
  (multiple-value-bind (value present) (gethash "payload" object)
    (when present (protocol-object value "event.payload")))
  (when-let (metadata (gethash "metadata" object))
    (protocol-object metadata "event.metadata"))
  object)

(defun projection-events (payload &aux (frames (gethash "accumulated_frames" payload)))
  "Validate canonical accumulated replay frames and return their inner events."
  (unless (vectorp frames)
    (protocol-refuse :invalid-envelope "replay accumulated_frames must be an array"))
  (map 'vector (lambda (frame) (decode-stream-frame frame "accumulated replay frame")) frames))

(defun decode-stream-frame (frame what)
  "Validate FRAME, one sequenced frame WHAT names, and answer its inner event."
  (protocol-object frame what)
  (required-integer frame "seq")
  (decode-inner-event (gethash "event" frame)))

(defun validate-command-result (result)
  "Validate and return one canonical Gateway command result object."
  (let ((result (protocol-object result "result")))
    (dolist (field '("command_id" "correlation_id" "session_id" "command"
                     "status" "message"))
      (required-string result field))
    (optional-string result "code")
    (when-let (response (gethash "response" result))
      (protocol-object response "result.response")
      (optional-string response "turn_id")
      (optional-integer response "queue_depth"))
    result))

(defun encode-command-result (sequence result &key session-id)
  "Encode one validated command outcome in its canonical RESULT wrapper."
  (encode-json-object
   (json-object "sequence" sequence
                "payload" (json-object "type" "gateway_turn_command_result"
                                       "result" (validate-command-result result))
                :opt "session_id" session-id)))

(defun encode-sync-protocol-error (code message &key permanent)
  "Encode a post-handshake protocol error outside session sequencing."
  (encode-unsequenced-frame
   "sync_protocol_error" (json-object "code" code "message" message "permanent" (and permanent t))))

(defun encode-unsequenced-frame (type payload)
  "Encode one control frame of TYPE outside session sequencing: sequence 0,
so the client classifies it by kind before any watermark check and the
per-peer dense stream never sees a hole."
  ;; PAYLOAD's members ride beside the type.
  (encode-json-object
   (make-json-object "sequence" 0
                     "payload" (copy-json-object (and payload (protocol-object payload "payload"))
                                                 "type" type))))

(defun encode-session-stream-resumed (session-id sequence frames &key cursor admission-state
                                                replay-chunk-index replay-chunk-final-p
                                                replay-tail-p resumed-after title octets)
  "Encode canonical accumulated replay FRAMES at the authoritative watermark:
as JSON text, or with OCTETS as its UTF-8 octets."
  ;; OCTETS is the replay's own form: its frames carry each logged payload as
  ;; a JSON-VERBATIM, which octets take as they are and text would decode.
  ;; REPLAY-TAIL-P marks a chunk of the first pass a long session's replay
  ;; paints from — its newest turns, the whole replay following it
  ;; (gateway.lisp SEND-REPLAY-TAIL). RESUMED-AFTER marks a resume: FRAMES
  ;; are the facts past that log position alone, which the client folds onto
  ;; what it holds instead of replacing it (gateway.lisp
  ;; SEND-REPLAY-UNDER-LOCK). TITLE is the session's name as the replay finds
  ;; it, which no frame folds (session.title is config evidence); absent when
  ;; the session has none yet.
  (required-string nil "session_id" session-id)
  (required-integer nil "sequence" sequence)
  (funcall
   (if octets #'encode-json-octets #'encode-json-object)
   (json-object
    "sequence" sequence
    "session_id" session-id
    "payload" (json-object
               "type" "session_stream_resumed"
               "session_id" session-id
               "accumulated_frames" (coerce frames 'vector)
               :opt "admission_state" admission-state
               :opt "replay_chunk_index" replay-chunk-index
               :when replay-chunk-final-p "replay_chunk_final" t
               :when replay-tail-p "replay_tail" t
               :opt "resumed_after" resumed-after
               :opt "title" (and title (plusp (length title)) title))
    ;; An absent cursor must be absent on the wire: shasht encodes NIL as
    ;; JSON false, and a non-final chunk carrying "cursor": false breaks
    ;; every integer-typed cursor reader. Only the final chunk knows the
    ;; authoritative watermark.
    :opt "cursor" cursor)))

(defun protocol-object (value field)
  (unless (hash-table-p value)
    (protocol-refuse :invalid-envelope "~a must be an object" field))
  value)

(macrolet ((define-field-check (name test message)
             `(defun ,name (object field &optional (value (gethash field object)))
                (unless ,test
                  (protocol-refuse :invalid-envelope ,message field))
                value)))
  (define-field-check required-string (and (stringp value) (plusp (length value)))
    "~a must be a non-empty string")
  (define-field-check optional-string (or (null value) (stringp value))
    "~a must be a string or null")
  (define-field-check required-integer (and (integerp value) (<= 0 value))
    "~a must be a non-negative integer")
  (define-field-check optional-integer (or (null value) (and (integerp value) (<= 0 value)))
    "~a must be a non-negative integer or null")
  (define-field-check optional-text (or (null value) (and (stringp value) (plusp (length value))))
    "~a must be a non-empty string or null")
  (define-field-check required-array (vectorp value) "~a must be an array"))

;;; DECODE-COMMAND and ENCODE-CLIENT-COMMAND cover new kinds through their
;;; OTHERWISE arms (request keys pass through verbatim), so a command addition
;;; touches this table plus the DISPATCH-COMMAND arm only.
(defparameter +gateway-command-kinds+
  (mapcar (lambda (kind) (cons (substitute #\_ #\- (string-downcase kind)) kind))
          '(:set-session-subscriptions :start-turn :steer-turn :withdraw-prompt :cancel-turn
            :navigate-checkpoint
            ;; The pending-exec view's own gesture (client.lisp SEND-INTERRUPT-EXEC):
            ;; one more request-reply kind, decoded and answered like every other.
            :interrupt-exec
            ;; A read: the session as it stands, in its command result (gateway.lisp
            ;; SESSION-STATE-JSON), what a program asks before it acts.
            :get-state))
  "Wire name to command kind and, read backwards, kind to wire name: one
table, so the two directions cannot drift apart.")

(defun command-kind-wire-name (kind)
  (or (car (rassoc kind +gateway-command-kinds+))
      (protocol-refuse :unsupported-command "unsupported command kind ~s" kind)))

(defparameter +unsequenced-frame-kinds+
  '(;; Live-only, unsequenced, session-less: the organism
    ;; speaking to every attached shell at once (NLE:NOTICE,
    ;; gateway.lisp) -- a peripheral's refusal, a channel's
    ;; state change. Rendered as a system row; never replayed.
    "organism.notice"
    ;; Complete typed standing board, including quiet changes and clears.
    "organism.notices"
    ;; Live-only, unsequenced, session-less: which sessions
    ;; are running a turn right now, and since when
    ;; (activity.lisp) -- the whole map each time the set
    ;; changes, the durable read NLK:ACTIVE-TURN-STARTS
    ;; projected for the shells that paint a spinner at it.
    ;; Never replayed: a shell that starts later reads the
    ;; map again from the subscription snapshot.
    "organism.activity"
    ;; Live-only, unsequenced, session-less: the organism's
    ;; image evaluated definitions from a file of its trees
    ;; (NLK:SOURCE-LOADED, gateway.lisp ANNOUNCE-LOADED) --
    ;; the path and the definitions' names, never the code.
    ;; A shell in another process on the same disk
    ;; evaluates them from its own copy of the file
    ;; (client.lisp). Never replayed: a shell that starts
    ;; later reads the file itself.
    "organism.loaded"
    ;; Live-only, unsequenced, session-less: the organism's image moved to a
    ;; staged release and reads itself from a new tree (update.lisp
    ;; ANNOUNCE-UPGRADED); each attached shell converges its own image on the
    ;; same tree (CONVERGE-SHELL). Never replayed -- the durable truth is the
    ;; organism.upgraded fact in the log.
    "organism.upgraded"
    ;; Live-only, unsequenced, session-less: a note the organism says to its
    ;; operator once, in the model's words, too long for a toast -- a boot's,
    ;; of how it came back and the release it runs (gateway.lisp SAY-NOTE) --
    ;; which each shell shows as a row and a channel host posts in its home
    ;; room. Never replayed: whoever says one keeps whether it was said.
    "organism.note"
    ;; Live-only, unsequenced, session-less: the folders the organism just
    ;; turned on (control.lisp SWITCH-CELL-ON); each shell in another
    ;; process loads the same ones into its own image, as its boot loads the
    ;; folder. Never replayed -- a shell that starts later loads the folder.
    "organism.cells"
    ;; Live-only, unsequenced: a model or effort pick
    ;; landed at the gateway (SELECT-SESSION-MODEL,
    ;; SET-SESSION-EFFORT, the default's route), so every
    ;; attached shell's cached footer facts are stale; the
    ;; client refetches off the loop thread. Never replayed --
    ;; the durable truth is session.model_selected, config
    ;; evidence served by its read model.
    "model_changed"
    ;; Live-only, unsequenced: a session was named (gateway.lisp
    ;; SET-SESSION-TITLE -- the operator's /rename, a cell, the title a model
    ;; writes from the first prompt) and carries {session_id, title}, the
    ;; name now in effect; the shell showing that session wears it in its
    ;; header. Session.title is config evidence and flows on no stream, so
    ;; this frame is the live half; the resume replay carries the name for a
    ;; shell that attaches later (session_stream_resumed.title). Never
    ;; replayed as a frame.
    "session_titled"
    ;; Live-only, unsequenced, session-less: a cell started or stopped at
    ;; the gateway (cells.lisp START-CELLS and its kin), so the cell
    ;; commands every shell read at attach are stale; the client refetches
    ;; them off the loop thread. Never replayed -- GET /api/gateway/slash is
    ;; the one authority and a fresh shell reads it at attach.
    "commands_changed")
  "Every session-less frame kind carried outside session sequencing.
ONE LIST, BOTH DIRECTIONS: ENCODE-UNSEQUENCED-FRAME and the gateway's
BROADCAST-ORGANISM write exactly these kinds, and PROJECTION-KIND admits
exactly these at the far end. A producer naming a kind this list does not
carry refuses at the gateway; a kind that reaches a shell this list does not
carry is refused by the decoder and shown as `unsupported projection type'
-- which is why the two ends read one list and not two.")

(defparameter +session-frame-kinds+
  ;; Exactly the kinds with a live producer or consumer -- a new frame kind
  ;; earns its entry together with the code that emits or folds it, never as
  ;; a reservation. The underscore turn_* quartet is the ZIG gateway's wire
  ;; lexicon (protocol/sync.zig GatewayTurnFrameEventKind wireName): the lisp
  ;; client attaches to either gateway, so those kinds have a live foreign
  ;; producer even though the lisp gateway only ever nests dot-kinds inside
  ;; session_stream_event frames.
  '("session_stream_event" "session_stream_resumed"
    "session_checkpoint_undo_applied"
    "session_resync_required" "sync_protocol_error"
    "gateway_turn_command_result"
    ;; Live-only background-shell output/lifecycle frames
    ;; (Zig wire.zig writeBackgroundProcessUpdatedFrame),
    ;; including subscribe-time registry snapshots. Retention:
    ;; live-only — the durable truth is the turn.tool_result
    ;; pair (handoff + watcher exit fact); these frames never
    ;; replay (src/protocol/background_exec.zig retention
    ;; table).
    "session_background_process_updated"
    ;; Live-only: how the forks of a running call stand (organism/fork.lisp
    ;; FORK-COLLECT-PROGRESS), drawn under the call. Retention: live-only --
    ;; each fork's own session is the durable record; never replayed.
    "session_fork_progress"
    ;; Live-only queue state: the durable snapshot is
    ;; evidence and never streams, so this frame is how an
    ;; attached peer learns of a parked steer -- on admission,
    ;; on withdrawal, and on subscribe while a turn runs.
    "session_input_queue_snapshot_updated"
    ;; Live-only transcript rows for a client that renders
    ;; and does not fold (surface/rows.lisp): the rows that
    ;; changed, folded at the gateway by the shell's reducer.
    "session_rows"
    "turn_started" "turn_completed" "turn_failed"
    "turn_cancelled" "item_delta")
  "Every frame kind a session's stream or a command's answer carries, beside
the session-less +UNSEQUENCED-FRAME-KINDS+: what PROJECTION-KIND admits, and
what protocol.schema.json, beside this file, names (a test holds the two to
one list).")

(defun projection-kind (value)
  "Recognize stable projection/event discriminators without interning input."
  ;; The kinds are +SESSION-FRAME-KINDS+ and the session-less
  ;; +UNSEQUENCED-FRAME-KINDS+, which the producers read too.
  (unless (stringp value)
    (protocol-refuse :invalid-envelope "payload.type must be a string"))
  (unless (or (member value +session-frame-kinds+ :test #'string=)
              (member value +unsequenced-frame-kinds+ :test #'string=))
    ;; Printed with ~a between quotes and not with ~s: the value comes off the
    ;; decoder as an adjustable base-string, and a printer bound readably
    ;; renders that as #A((18) BASE-CHAR . "..."), which is what the operator
    ;; was shown.
    (protocol-refuse :unsupported-projection "unsupported projection type \"~a\"" value))
  value)

(defun encode-handshake-frame (type &rest pairs)
  "Encode one handshake frame of TYPE offering this protocol version alone, then
alternating key/value PAIRS."
  (encode-json-object (apply #'make-json-object "type" type
                             "protocol_min" +gateway-protocol-version+
                             "protocol_max" +gateway-protocol-version+ pairs)))

(defun encode-gateway-connect-challenge (nonce)
  (required-string nil "challenge nonce" nonce)
  (encode-handshake-frame "gateway_connect_challenge" "challenge_nonce" nonce))

(defun encode-gateway-connect (challenge client-id client-version client-mode
                               platform instance-id &key operator-token scopes)
  (encode-handshake-frame
    "gateway_connect"
    "challenge_nonce" (required-string nil "value" challenge)
    "client" (make-json-object "id" client-id "version" client-version
                                "mode" client-mode "platform" platform
                                "instance_id" instance-id)
    "auth" (make-json-object "operator_token" operator-token)
    "requested_scopes" (coerce (or scopes '("sync.read" "sync.write")) 'vector)
    "transport" (make-json-object "forwarded" nil
                                    "secure_channel_observed" nil)))

(defun encode-gateway-connect-ack (scopes &key version)
  "The answer to an accepted connect: the protocol version, the scopes
granted, and, when given, VERSION — the build the organism runs, so a shell
of another build can say so."
  (encode-json-object
   (json-object "type" "gateway_connect_ack"
                "protocol_version" +gateway-protocol-version+
                "accepted_scopes" (coerce scopes 'vector)
                :opt "version" version)))

(defun encode-gateway-connect-rejected (code message &key permanent)
  (encode-handshake-frame "gateway_connect_rejected"
                          "reason" code "message" message
                          "permanent" (if permanent t nil)))

(defparameter +gateway-default-scopes+ '("sync.read" "sync.write")
  "Scopes granted when a client requests the canonical default set.")

(defun validate-gateway-connect (payload challenge-nonce operator-token &key authorized)
  "Validate one client connect payload against this connection's challenge.
AUTHORIZED says the socket's upgrade already carried the operator's Bearer,
so the payload need not carry the token again."
  ;; Returns accepted scopes only after mandatory operator authentication:
  ;; the token in the payload, or the upgrade's own (AUTHORIZED) -- what a
  ;; proxy that adds the header to every request it forwards presents, the
  ;; browser behind it never holding the token at all.
  (protocol-object payload "gateway_connect")
  (let ((nonce (required-string payload "challenge_nonce"))
        (client (protocol-object (gethash "client" payload) "client"))
        (auth (gethash "auth" payload)))
    (unless (string= nonce challenge-nonce)
      (protocol-refuse :challenge-mismatch "gateway connect challenge does not match"))
    (dolist (field '("id" "version" "mode" "platform" "instance_id"))
      (required-string client field))
    (when auth (protocol-object auth "auth"))
    (unless (and (stringp operator-token) (plusp (length operator-token)))
      (protocol-refuse :operator-auth-unconfigured
                       "gateway operator token is not configured"))
    ;; a presented token is a string; OPERATOR-TOKEN is one, checked above
    (unless (or authorized (equal (and auth (gethash "operator_token" auth)) operator-token))
      (protocol-refuse :operator-auth-failed
                       "operator token required; connect with a valid operator token"))
    ;; the requested scopes as wire names, each one this gateway grants
    (let ((requested (gethash "requested_scopes" payload)))
      (unless (vectorp requested)
        (protocol-refuse :invalid-connect "requested_scopes must be an array"))
      (loop for value across requested
            unless (member value +gateway-default-scopes+ :test #'equal)
              do (protocol-refuse :scope-denied
                                  "unsupported requested scope ~s" value))
      (or (coerce requested 'list) (copy-list +gateway-default-scopes+)))))

;;; --- user image attachments -------------------------------------------------
;;; A turn's images ride inside the prompt's `content` array, after the text
;;; item (the Zig appendPromptContentItems / parseImageAttachments pair):
;;;
;;;   {"type":"image","source":{"type":"base64","media_type":..,"data":..}}
;;;
;;; Base64 is the only source this wire speaks, in both directions. A `file`
;;; source naming a path for the gateway to read was retired: no
;;; client in this image ever produced one, and resolving it turned an
;;; authenticated start_turn into an arbitrary-file read. Above the wire every
;;; lisp surface speaks one shape — a list of (MEDIA-TYPE . BASE64) conses in
;;; prompt order, which is what the `images` request field holds on both sides
;;; of the codec.

(defun image-content-items (images)
  "IMAGES as wire content items, in order."
  (loop for (media-type . base64) in images
        collect (make-json-object
                 "type" "image"
                 "source" (make-json-object "type" "base64"
                                            "media_type" media-type
                                            "data" base64))))

(defun content-image-attachments (content)
  "Every image CONTENT carries, in order, as (MEDIA-TYPE . BASE64) conses."
  ;; An item of any other shape — no source, a source this wire does not speak, a
  ;; base64 source missing its media type or its data — is skipped rather than
  ;; refused: one bad attachment must not cost the user the prompt it rode with
  ;; (the Zig warn-and-skip rule).
  (loop for item across (or (json-value content :array) #())
        for source = (json-value item :object "source")
        for media-type = (json-value source :text "media_type")
        for data = (json-value source :text "data")
        when (and (equal "image" (json-value item :string "type"))
                  (equal "base64" (json-value source :string "type"))
                  media-type data)
          collect (cons media-type data)))

(defun normalize-gateway-request (kind request)
  "Normalize Zig-compatible request content into Lisp gateway fields."
  (let* ((normalized (copy-json-object request))
         (content (and (member kind '(:start-turn :steer-turn))
                       (gethash "content" normalized)))
         (part (and (vectorp content) (> (length content) 0) (aref content 0)))
         (images (content-image-attachments content)))
    (when (and (hash-table-p part)
               (string= (gethash "type" part "") "text"))
      (setf (gethash "prompt" normalized) (gethash "text" part)))
    (when images (setf (gethash "images" normalized) images))
    normalized))

(defun encode-client-command (command)
  (unless (gateway-command-p command)
    (protocol-refuse :invalid-command "expected a gateway-command"))
  (let ((table (json-object
                "type" (command-kind-wire-name (gateway-command-kind command))
                :opt "command_id" (gateway-command-command-id command)
                :opt "correlation_id" (gateway-command-correlation-id command)))
        (request (copy-json-object (gateway-command-request command))))
    (case (gateway-command-kind command)
      ((:start-turn :steer-turn)
       (let ((prompt (gethash "prompt" request))
             (images (gethash "images" request)))
         (when (stringp prompt)
           ;; The text item first — always present, so the receiver can
           ;; recover the prompt even with no text — then one image item per
           ;; attachment, in order (Zig appendPromptContentItems).
           (setf (gethash "content" request)
                 (coerce (cons (make-json-object "type" "text" "text" prompt)
                               (image-content-items images))
                         'vector))
           (remhash "prompt" request))
         ;; Images live in `content` on the wire; the field is the internal
         ;; carrier on both sides of the codec, never a second wire shape.
         (remhash "images" request))
       (setf (gethash "request" table) request))
      (:set-session-subscriptions
       (setf (gethash "subscriptions" table) (gethash "subscriptions" request))
       ;; The shell's pid rides the subscription (client.lisp
       ;; SEND-SUBSCRIBE): the gateway names each attached shell's
       ;; process in the image section, and skips the shell in its own
       ;; process when a source load is sent out (SOURCE-LOADED).
       (when-let (pid (json-value request :integer "pid")) (setf (gethash "pid" table) pid)))
      (otherwise
       (maphash (lambda (key value) (setf (gethash key table) value)) request)))
    (encode-json-object (make-json-object "type" "client_command" "command" table))))

(defun encode-projection-event (session-id sequence kind payload &key cursor durable-p)
  (required-string nil "session_id" session-id)
  (required-integer nil "sequence" sequence)
  (projection-kind kind)
  (encode-json-object
   (json-object "sequence" sequence
                "session_id" session-id
                "payload" (copy-json-object (and payload (protocol-object payload "payload"))
                                            "type" kind)
                :opt "cursor" cursor
                :when durable-p "durable" t)))

(defun decode-command (command)
  (protocol-object command "command")
  (let* ((type (required-string command "type"))
         (kind (or (cdr (assoc type +gateway-command-kinds+ :test #'string=))
                   (protocol-refuse :unsupported-command "unsupported command type ~s" type)))
         (command-id (unless (eq kind :set-session-subscriptions)
                       (required-string command "command_id")))
         (correlation-id (unless (eq kind :set-session-subscriptions)
                           (required-string command "correlation_id")))
         (request (make-hash-table :test #'equal)))
    (case kind
      ((:start-turn :steer-turn)
       (setf request
             (normalize-gateway-request
              kind (protocol-object (gethash "request" command) "request"))))
      (:set-session-subscriptions
       (let ((subscriptions (gethash "subscriptions" command)))
         (unless (vectorp subscriptions)
           (protocol-refuse :invalid-command "subscriptions must be an array"))
         (setf (gethash "subscriptions" request) subscriptions)
         (when-let (pid (json-value command :integer "pid")) (setf (gethash "pid" request) pid))))
      (otherwise
       (maphash (lambda (key value)
                  (unless (member key '("type" "command_id" "correlation_id") :test #'string=)
                    (setf (gethash key request) value))) command)))
    (make-gateway-command :kind kind :command-id command-id :correlation-id correlation-id
                          :request request)))

(defun decode-sync-message (json-text)
  "Decode and validate one Gateway WebSocket message."
  (let* ((root (handler-case (nlk:decode-json json-text)
                 (error (e) (protocol-refuse :invalid-json "invalid JSON: ~a" e))))
         (root (protocol-object root "frame"))
         (type-name (gethash "type" root))
         (type (and type-name
                    (if (stringp type-name)
                        (find type-name '(:gateway-connect :gateway-connect-challenge :gateway-connect-ack
                                          :gateway-connect-rejected :client-command)
                              :key (lambda (kind) (substitute #\_ #\- (string-downcase kind))) :test #'string=)
                        (protocol-refuse :invalid-envelope "type must be a string")))))
    (when (and type-name (null type))
      (protocol-refuse :unsupported-frame "unsupported frame type ~s" type-name))
    (case type
      (:client-command
       (make-sync-message :type type :payload (decode-command (gethash "command" root))))
      ((:gateway-connect :gateway-connect-challenge)
       (let ((minimum (required-integer root "protocol_min")) (maximum (required-integer root "protocol_max")))
         (unless (<= minimum +gateway-protocol-version+ maximum)
           (protocol-refuse :unsupported-version "protocol range ~d..~d does not include version ~d" minimum maximum +gateway-protocol-version+))
         (make-sync-message :type type :protocol-version +gateway-protocol-version+ :payload root)))
      ((:gateway-connect-ack :gateway-connect-rejected)
       (let ((version (or (gethash "protocol_version" root) (gethash "protocol_min" root))))
         (unless (and (integerp version) (= version +gateway-protocol-version+))
           (protocol-refuse :unsupported-version "unsupported negotiated protocol version"))
         (make-sync-message :type type :protocol-version version :payload root)))
      (otherwise
       (let* ((sequence (required-integer root "sequence"))
              (session-id (optional-text root "session_id"))
              (cursor (optional-integer root "cursor"))
              (payload (protocol-object (gethash "payload" root) "payload"))
              (kind (projection-kind (required-string payload "type"))))
         (dispatch kind string=
           ("gateway_turn_command_result" (validate-command-result (gethash "result" payload)))
           ("session_stream_resumed"
            (projection-events payload)
            (optional-text payload "title")
            (unless (member (gethash "admission_state" payload)
                            '("ready" "active") :test #'string=)
              (protocol-refuse :invalid-envelope
                               "admission_state must be ready or active")))
           ("session_stream_event"
            (decode-stream-frame (or (gethash "frame" payload) payload) "session stream frame"))
           ("organism.notice" (required-string payload "text"))
           ("organism.notices" (required-array nil "organism.notices notices" (gethash "notices" payload)))
           ("organism.note" (required-string payload "text"))
           ("organism.cells" (required-array nil "organism.cells names" (gethash "names" payload)))
           ("organism.activity"
            (required-array nil "organism.activity sessions" (gethash "sessions" payload)))
           ("organism.loaded"
            (required-string payload "path")
            (optional-text payload "session_id")
            (optional-text payload "turn_id")
            (optional-text payload "call_id")
            (optional-text payload "comment")
            (required-array nil "organism.loaded definitions" (gethash "definitions" payload))))
         ;; The session-less half is +UNSEQUENCED-FRAME-KINDS+ and is read from
         ;; it, never listed again here: a kind published outside sequencing
         ;; and missing from a second copy of the list arrives session-less and
         ;; is refused for having no session.
         (when (and (null session-id)
                    (not (member kind +unsequenced-frame-kinds+ :test #'string=))
                    (not (member kind '("gateway_turn_command_result" "sync_protocol_error")
                                 :test #'string=)))
           (protocol-refuse :invalid-envelope "session_id is required for session projection ~s" kind))
         (make-sync-message :type :projection :sequence sequence :session-id session-id
                            :cursor cursor :payload payload))))))
