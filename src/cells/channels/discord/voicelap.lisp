;;;; voicelap.lisp --- the live voice connection: sockets, keys, workers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; voice.lisp is the protocol with nothing moving in it and dave.lisp is
;;;; the library; this file is the part that runs. One VOICE-LANE holds a
;;;; voice channel the operator asked the bot to sit in, and owns four
;;;; threads and nothing else:
;;;;
;;;;   lap       the supervised voice websocket: identify, heartbeat, the
;;;;             DAVE handshake, speaking state. Never blocks on audio.
;;;;   listen    the UDP socket, read packet by packet: transport-open,
;;;;             DAVE-open, hand the frame to that speaker's utterance.
;;;;   work      one utterance at a time: transcribe it, then submit it as
;;;;             an ordinary ask through the kit. Seconds per item, which is
;;;;             exactly why it is not the listener.
;;;;   speak     the playback queue: WAV in, paced Opus out, interruptible.
;;;;
;;;; The rules the issue draws, kept here so they are checkable:
;;;;   - The bot sits in voice only where it is asked to: beside whoever
;;;;     types /voice join, or where voice_autojoin and voice_follow seat it.
;;;;     Nothing else joins, and nothing is heard outside a seat.
;;;;   - Only an allowlisted human's speech opens a turn. Our own audio, a
;;;;     bot's, a stranger's, silence and a malformed packet open nothing.
;;;;   - An utterance is ONE ask through NCK:HANDLE-CANDIDATE, the same door
;;;;     a typed line uses, in the room the text channel already owns. The
;;;;     text channel keeps the transcript, the status card and the answer;
;;;;     the voice channel is a second way in and a second way out, never a
;;;;     second conversation.
;;;;   - An utterance that keeps going stops whatever is being said. The
;;;;     room's own cancellation is unchanged: this only silences the
;;;;     speaker.

(in-package #:nodecode-channel-discord)

(defparameter +voice-frames-per-second+ 50)
(defparameter +voice-udp-timeout-seconds+ 1
  "How long the listener waits on a quiet socket before looking at the
clock: an utterance ends in silence, and silence sends no packets.")

(deftype snowflake () "A Discord id: a decimal string, never an integer." 'string)

(nlk:define-record (voice-lane (:copier nil) (:export :readers)
                               (:constructor %make-voice-lane))
  "One voice channel the bot is sitting in."
  (guild-id "" :type snowflake)
  ;; The bot's own user id: what MLS knows this member as, and the one id a
  ;; frame from ourselves is recognised by.
  (self-user-id "" :type snowflake)
  (channel-id "" :type snowflake)
  ;; The text channel this voice channel talks through: the room that owns
  ;; the transcript, the status card and the answer.
  (room-channel-id "" :type snowflake)
  (speakers '() :type list)
  (session-id nil)
  (token nil)
  (endpoint nil)
  (heartbeat-interval nil)
  (ws nil)
  (udp nil)
  (ssrc nil)
  (secret-key nil)
  (dave-version nil)
  (dave-session nil)
  (encryptor nil)
  (decryptors (make-hash-table) :type hash-table)   ; ssrc -> libdave decryptor
  (ratchets (make-hash-table :test #'equal) :type hash-table)
  (speaker-ids (make-hash-table) :type hash-table)  ; ssrc -> user id
  (members '() :type list)
  (utterances (make-hash-table) :type hash-table)   ; ssrc -> utterance
  (binary-sequence nil)
  (mls-joined-p nil)
  (lock (bt2:make-lock :name "discord-voice") :type t)
  ;; playback
  (play-queue nil)
  (play-generation 0 :type integer)
  ;; work
  (work-queue nil)
  (stop-lap nil)
  ;; Turn ids whose answer has already been said out loud. The kit re-keys a
  ;; lane when it opens a thread for an ask, and both the thread's session and
  ;; the room's carry the same completed answer, so the turn is what dedupes.
  (spoken-turns (make-hash-table :test #'equal) :type hash-table)
  (last-error nil)
  (heard 0 :type integer)
  (spoken 0 :type integer)
  ;; When somebody last spoke to the bot or it last spoke: the idle clock
  ;; (VOICE-IDLE-P).
  (active-ms (nck:now-ms) :type integer)
  (leaving-p nil :type boolean)
  ;; /voice notes: what was said, newest first, as (MS USER-ID TEXT), while
  ;; NOTES-BY — the person who asked for them — is set (VOICE-STOP-NOTES).
  (notes '() :type list)
  (notes-by nil :type (or null snowflake))
  (notes-from-ms 0 :type integer))

(nlk:access (lane voice-lane))

;;; One at a time: a bot has one voice state per guild, and a second lane
;;; would be a second conversation, which is the thing this slice refuses to
;;; build.
(defvar *voice-lane* nil
  "The voice channel this image is sitting in, or NIL.")

(defvar *voice-section* nil
  "The channels.discord section voice started from, read live by the rules
that seat the bot (VOICE-PLACE); NIL while voice is not started.")

(defvar *voice-states* (make-hash-table :test #'equal :synchronized t)
  "Who sits in which voice channel of every server the bot is in: user id ->
(CHANNEL-ID . BOT-P), from GUILD_CREATE and every VOICE_STATE_UPDATE after
it. A person sits in one voice channel at a time, wherever it is.")

(defvar *voice-channel-guilds* (make-hash-table :test #'equal :synchronized t)
  "Voice channel id -> the server it is in, from GUILD_CREATE's channels and
every voice state: what a join tells Discord beside the channel.")

(defvar *voice-channel-names* (make-hash-table :test #'equal :synchronized t)
  "Channel id -> (NAME TYPE GUILD) for the text, voice and stage channels of
every server the bot is in, from GUILD_CREATE: what /voice calls a channel,
the voice channels it offers to sit in and the text channels to talk through.")

(defvar *voice-member-names* (make-hash-table :test #'equal :synchronized t)
  "User id -> the name its server shows for that person, from GUILD_CREATE's
members: whom /voice says it hears, and who sits in each voice channel.")

(defvar *voice-expecting-leave* nil
  "Whether the bot asked Discord to take it out of voice, so the
VOICE_STATE_UPDATE that says it is out is its own doing and ends no lane.")

(defvar *voice-seat-lock* (bt2:make-lock :name "discord-voice-seat")
  "One join, move or leave at a time.")

;;; --- configuration ------------------------------------------------------------------

(defun voice-room-channel (section channel)
  "The channel a voice lane sitting in CHANNEL talks through: the one the
section names, else CHANNEL's own chat -- every voice channel carries one,
where the people in the call already read and type."
  (or (config-string section "voice_text_channel_id" nil) channel))

(defun voice-speakers (section &optional asker)
  "Whose speech may open a turn: named, else the users allowed to drive the
bot, else its owner, else ASKER, who typed /voice join. Nobody refuses."
  ;; Empty is not 'everyone' here.
  (or (config-string-list section "voice_speakers")
      (config-string-list section "allowed_users")
      (config-string-list section "owner")
      (and asker (list asker))))

;;; --- the lane's own logging -----------------------------------------------------------

(defun voice-note (format &rest arguments)
  (nck:set-channel-status "discord" :voice (apply #'format nil format arguments)))

(defun voice-say-in-room (lane text)
  "One line in the text channel the lane talks through."
  ;; Every visible thing voice does — joined, left, a failure — is said there,
  ;; because that room is the surface the issue makes authoritative.
  (let ((host (and *discord-adapter* (discord-adapter-host *discord-adapter*))))
    (when (and host lane)
      (ignore-errors (nck:post-message host (list :channel-id lane.room-channel-id) text)))))

;;; --- DAVE, driven --------------------------------------------------------------------

(defun voice-send (lane payload)
  (nlk:when-let (ws lane.ws) (wsd:send ws (nlk:encode-json-object payload))))

(defun voice-send-binary (lane opcode body)
  (nlk:when-let (ws lane.ws)
    (wsd:send-binary ws (voice-binary-frame opcode body))))

(defun voice-start-dave (lane version)
  (load-dave)
  (let ((session (dave-session-create)))
    (dave-session-init session version (parse-integer lane.channel-id) lane.self-user-id)
    ;; A session description or a fresh epoch starts the group over. Anything
    ;; armed against the old one is gone with it, and saying otherwise is what
    ;; left the listener asking libdave for a ratchet forever.
    (setf lane.mls-joined-p nil)
    (clrhash lane.decryptors)
    (clrhash lane.ratchets)
    (setf lane.dave-session session lane.encryptor (dave-encryptor-create))
    (dave-encryptor-assign-codec lane.encryptor lane.ssrc +dave-codec-opus+)))

(defun voice-send-key-package (lane)
  (nlk:if-let (package (dave-session-key-package lane.dave-session))
    (progn (voice-send-binary lane +voice-op-dave-key-package+ package)
           (voice-send lane (voice-transition-ready-payload 0)))
    (warn "discord voice: libdave produced no key package")))

(defun voice-arm-self (lane)
  "Take this member's ratchet out of the group and arm the encryptor with
it. Until this happens nothing we send can be heard."
  (nlk:when-let (ratchet (dave-session-key-ratchet lane.dave-session lane.self-user-id))
    (dave-encryptor-set-ratchet lane.encryptor ratchet)
    (dave-encryptor-passthrough lane.encryptor nil)
    (setf (gethash "self" lane.ratchets) ratchet
          lane.mls-joined-p t)))

(defun voice-arm-speaker (lane ssrc &aux (user (gethash ssrc lane.speaker-ids)))
  "A decryptor for SSRC on that member's ratchet."
  ;; Nothing to arm before the group exists: libdave answers every such ask
  ;; with an error of its own, and the listener asks once per packet, so the
  ;; group check belongs here.
  (when (and user lane.dave-session lane.mls-joined-p (not (gethash ssrc lane.decryptors)))
    (nlk:when-let (ratchet (dave-session-key-ratchet lane.dave-session user))
      (let ((decryptor (dave-decryptor-create)))
        (dave-decryptor-arm decryptor ratchet -1) ; no expiry
        (setf (gethash ssrc lane.decryptors) decryptor (gethash user lane.ratchets) ratchet)))))

(defun voice-arm-all (lane)
  "This member's ratchet, then every speaker's decryptor on theirs."
  (voice-arm-self lane)
  (loop for ssrc being the hash-keys of lane.speaker-ids do (voice-arm-speaker lane ssrc)))

(defun voice-handle-binary (lane payload)
  (multiple-value-bind (sequence opcode body) (voice-binary-parts payload)
    (when sequence (setf lane.binary-sequence sequence))
    (let ((members (cons lane.self-user-id lane.members)))
      (cond
        ((null opcode))
        ((= opcode +voice-op-dave-external-sender+)
         (dave-session-set-external-sender lane.dave-session body)
         (voice-send-key-package lane))
        ((= opcode +voice-op-dave-proposals+)
         (nlk:when-let (commit (dave-session-process-proposals lane.dave-session body members))
           (voice-send-binary lane +voice-op-dave-commit-welcome+ commit)))
        ((member opcode (list +voice-op-dave-announce-commit+ +voice-op-dave-welcome+))
         (multiple-value-bind (transition rest) (transition-and-rest body)
           (if (= opcode +voice-op-dave-welcome+)
               (let ((result (dave-session-process-welcome lane.dave-session rest members)))
                 (unless (null-handle-p result) (dave-welcome-destroy result)))
               (let ((result (dave-session-process-commit lane.dave-session rest)))
                 (unless (null-handle-p result)
                   (when (dave-commit-failed-p result)
                     (warn "discord voice: MLS commit refused in transition ~a" transition))
                   (dave-commit-destroy result))))
           (voice-arm-all lane)
           (voice-send lane (voice-transition-ready-payload transition))))))))

;;; --- the voice websocket ----------------------------------------------------------------

(defun voice-handle-text (lane payload &aux (op (nlk:json-value payload :integer "op"))
                                            (data (nlk:json-value payload :object "d")))
  (cond
    ((eql op +voice-op-hello+)
     (setf lane.heartbeat-interval
           (/ (or (nlk:json-value data :integer "heartbeat_interval") 13750) 1000.0)))
    ((eql op +voice-op-ready+)
     (setf lane.ssrc (nlk:json-value data :integer "ssrc"))
     (voice-open-udp lane
                     (nlk:json-value data :string "ip")
                     (nlk:json-value data :integer "port")))
    ((eql op +voice-op-session-description+)
     (setf lane.secret-key
           (map '(vector (unsigned-byte 8)) #'identity
                (nlk:json-value data :array "secret_key"))
           lane.dave-version
           (nlk:json-value data :integer "dave_protocol_version"))
     (when (and lane.dave-version
                (plusp lane.dave-version))
       (voice-start-dave lane lane.dave-version))
     (voice-send lane (voice-speaking-payload lane.ssrc t)))
    ((eql op +voice-op-speaking+)
     (let ((user (nlk:json-value data :string "user_id"))
           (ssrc (nlk:json-value data :integer "ssrc")))
       (when (and user ssrc (not (equal user lane.self-user-id)))
         (setf (gethash ssrc lane.speaker-ids)
               (coerce user 'simple-string))
         (voice-arm-speaker lane ssrc))))
    ((eql op +voice-op-clients-connect+)
     (setf lane.members
           (union lane.members (coerce (nlk:json-array data "user_ids") 'list) :test #'equal)))
    ((eql op +voice-op-client-disconnect+)
     (setf lane.members
           (remove (nlk:json-value data :string "user_id") lane.members :test #'equal)))
    ((eql op +voice-op-dave-prepare-transition+)
     (voice-send lane (voice-transition-ready-payload
                       (nlk:json-value data :integer "transition_id"))))
    ((eql op +voice-op-dave-execute-transition+)
     (voice-arm-all lane))
    ((eql op +voice-op-dave-prepare-epoch+)
     (when (eql (nlk:json-value data :integer "epoch") 1)
       (voice-start-dave lane (or (nlk:json-value data :integer "protocol_version")
                                  +voice-dave-version+))
       (voice-send-key-package lane)))))

;;; --- the UDP socket ---------------------------------------------------------------------

(defun voice-open-udp (lane host port)
  "Open the media socket and tell the gateway where we are."
  ;; IP discovery is the only way to learn the address Discord will actually
  ;; see.
  (let ((socket (usocket:socket-connect (coerce host 'simple-string) port
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8))))
    (setf lane.udp socket)
    (usocket:socket-send socket (ip-discovery-packet lane.ssrc)
                         +ip-discovery-length+)
    (multiple-value-bind (packet size)
        (usocket:socket-receive socket
                                (make-array 128 :element-type '(unsigned-byte 8))
                                128)
      (multiple-value-bind (address external-port) (ip-discovery-answer packet size)
        (unless address
          (error "the voice server did not answer IP discovery"))
        (voice-send lane (voice-select-protocol-payload address external-port))))))

(defun voice-speaker-allowed-p (lane user-id &aux (speakers lane.speakers))
  ;; Notes are of everyone in the channel: a meeting is everyone's.
  (and user-id
       (or lane.notes-by
           (member "*" speakers :test #'equal)
           (member user-id speakers :test #'equal))
       t))

(defun voice-note-frame (lane ssrc frame &aux (user (gethash ssrc lane.speaker-ids)))
  "One decrypted Opus frame from SSRC."
  ;; Frames from anyone this lane does not listen to are dropped here, before
  ;; any state is made for them.
  (when (voice-speaker-allowed-p lane user)
    (let ((utterance (alexandria:ensure-gethash ssrc lane.utterances
                                                (make-utterance :user-id user
                                                                :last-ms (nck:now-ms)))))
      ;; Someone kept talking: stop talking over them. A breath, a keyboard,
      ;; a door and the room's own noise are a frame or two and say nothing
      ;; about wanting the floor, so only a run of speech cuts an answer.
      (unless (opus-silence-p frame)
        (when (>= (utterance-milliseconds utterance) +barge-in-ms+)
          (voice-stop-playback lane)))
      (utterance-note-frame utterance frame (nck:now-ms)))))

(defun voice-keep-armed (lane)
  "Take this member's ratchet again when the group has one and we do not."
  ;; An epoch change tears the old group down, and until the encryptor is
  ;; armed again nothing this bot says can be heard. Called from the
  ;; listener's own timeout tick, so it asks about once a second and never per
  ;; packet.
  ;; A speaker is armed only once the group is joined (VOICE-ARM-SPEAKER).
  (when (and lane.dave-session
             (not lane.mls-joined-p))
    (voice-arm-all lane)))

;;; --- hearing loss ----------------------------------------------------------------------

(defparameter +voice-deaf-window-packets+ 40
  "Packets from speakers this lane holds a key for that make one window of
audio: most of a second at fifty frames a second.")

(defparameter +voice-deaf-windows+ 15
  "Windows of audio in a row that arrive and do not open before the lane
gets up and sits down again — fifteen seconds of a mouth moving and nothing
said.")

(defvar *voice-deaf-windows* 0
  "Windows of audio that arrived and did not open, in a row, on the lane
seated now.")

(defvar *voice-deaf-retry-ms* 0
  "When this lane last got up to recover its hearing.")

(defun voice-hearing-windows (packets opened windows)
  "WINDOWS, after one window holding PACKETS from speakers this lane holds a
key for, of which OPENED opened.

Audio that arrives and never opens is the one symptom of a DAVE group gone
stale: Discord keys a fresh one whenever a member rejoins, and a lane that
missed the commit keeps ratchets for a group that no longer exists. It
decrypts to nothing, says nothing, and looks exactly like a quiet channel —
which is why it is counted here rather than left to be noticed."
  (if (and (>= packets +voice-deaf-window-packets+) (zerop opened))
      (1+ windows)
      0))

(defun voice-recover-hearing (lane &aux (section *voice-section*) (now (nck:now-ms)))
  "On the listener's tick: a lane that has sat through
+VOICE-DEAF-WINDOWS+ of its speakers' audio without opening one frame of it
has lost the group. Get up and sit down again, once a minute at most, and say
so: a lane that cannot hear anyone is a state the room can see."
  (when (and section
             (not lane.leaving-p)
             (>= *voice-deaf-windows* +voice-deaf-windows+)
             (>= (- now *voice-deaf-retry-ms*) 60000))
    (setf *voice-deaf-retry-ms* now
          *voice-deaf-windows* 0)
    (voice-say-in-room lane (format nil "voice: nothing from <#~a> has opened in ~d seconds — the encryption group went stale; sitting down again."
                                    lane.channel-id +voice-deaf-windows+))
    (nlk:spawn "channel-discord-voice-hearing"
      (bt2:with-lock-held (*voice-seat-lock*)
        (when (eq lane *voice-lane*)
          (nlk:with-handlers ((error (condition) (voice-note "~a" condition)))
            (voice-act section :move lane.channel-id "recovering my hearing")))))))

(defun voice-close-finished-utterances (lane &aux (now (nck:now-ms)))
  "Every speaker who has fallen quiet long enough hands their utterance to
the work queue. Runs on the listener, which is why it only enqueues."
  ;; Removing the entry being visited is the one change MAPHASH allows.
  (maphash (lambda (ssrc utterance)
             (when (or (utterance-complete-p utterance now)
                       ;; Audio that never opened made an entry and put no
                       ;; word in it: that is not a quiet speaker, it is no
                       ;; speaker at all, and an entry left standing is what a
                       ;; deaf lane looks like from the outside.
                       (utterance-stale-p utterance now))
               (remhash ssrc lane.utterances)
               ;; A cough is not an ask.
               (when (utterance-worth-hearing-p utterance)
                 (incf lane.heard)
                 (setf lane.active-ms now)
                 (nck:queue-push lane.work-queue utterance))))
           lane.utterances))

(defun voice-listen-lap (lane stop-p)
  "The UDP socket, read until the lane stops."
  ;; Everything here is cheap: a transport open, a DAVE open, a push onto a
  ;; list. The window counters are the one thing that is not about this
  ;; packet: a second of a speaker's audio that does not open is the shape of
  ;; a lane gone deaf, and VOICE-RECOVER-HEARING gets up from it.
  (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8)))
        (window-start (nck:now-ms))
        (window-packets 0)
        (window-opened 0))
    (loop until (funcall stop-p)
          do (handler-case
                 (multiple-value-bind (packet size)
                     (if (usocket:wait-for-input lane.udp
                                                 :timeout +voice-udp-timeout-seconds+
                                                 :ready-only t)
                         (usocket:socket-receive lane.udp buffer 2048))
                   (when (and packet (integerp size) (plusp size))
                     (nlk:when-let (frame (and lane.secret-key
                                               (voice-packet-frame lane.secret-key packet size)))
                       ;; VOICE-ARM-SPEAKER arms a speaker it has no decryptor for yet.
                       (let* ((ssrc (rtp-ssrc packet))
                              (decryptor (progn (voice-arm-speaker lane ssrc)
                                                (gethash ssrc lane.decryptors)))
                              (opus (when decryptor
                                      (if (opus-silence-p frame)
                                          frame
                                          (dave-decrypt decryptor frame)))))
                         ;; A stranger's audio is meant not to open, so only a
                         ;; speaker this lane holds a key for fills a window.
                         (when decryptor
                           (incf window-packets)
                           (when opus (incf window-opened)))
                         (when opus (voice-note-frame lane ssrc opus))))))
               (error (condition)
                 ;; A malformed packet is not an event. A dead socket ends the lap.
                 (unless (funcall stop-p)
                   (setf lane.last-error (princ-to-string condition))
                   (sleep 0.05))))
             (let ((now (nck:now-ms)))
               (when (>= (- now window-start) 1000)
                 (setf *voice-deaf-windows* (voice-hearing-windows window-packets
                                                                   window-opened
                                                                   *voice-deaf-windows*)
                       window-start now
                       window-packets 0
                       window-opened 0)))
             (voice-recover-hearing lane)
             (voice-keep-armed lane)
             (voice-close-finished-utterances lane)
             (voice-leave-if-idle lane))))

;;; --- an utterance becomes an ask ----------------------------------------------------------

(defun voice-candidate (lane utterance text message-id)
  "The utterance as the same shape a typed line normalizes into."
  ;; It is addressed by construction: someone spoke into a voice channel this
  ;; bot was asked to sit in, which is as direct as an address gets.
  ;; voice_origin and the speaker ride the source so the durable record says
  ;; where it came from.
  (nlk:json-object
   "text" text
   "source" (nlk:json-object
             "platform" "discord"
             "chat_kind" "channel"
             "channel_id" lane.room-channel-id
             "message_id" message-id
             "user_id" utterance.user-id
             "workspace_id" lane.guild-id
             "addressed" t
             "voice_origin" "discord_voice"
             "voice_channel_id" lane.channel-id
             "voice_seconds" (/ (utterance-milliseconds utterance) 1000.0))))

(defun voice-work-lap (lane stop-p)
  "One piece of slow work at a time: an utterance transcribed and submitted,
or an answer synthesized and queued to be spoken."
  ;; Both are seconds of CPU, which is exactly why neither runs on the
  ;; listener or the lap.
  (loop until (funcall stop-p)
        do (multiple-value-bind (item found) (nck:queue-pop lane.work-queue 0.5)
             (when found
               (handler-case
                   (if (and (consp item) (eq (first item) :speak))
                       (voice-speak-answer lane (second item))
                       (voice-submit-utterance lane item))
                 (error (condition)
                   (setf lane.last-error (princ-to-string condition))
                   (voice-say-in-room
                    lane (format nil "voice: ~a" (princ-to-string condition)))))))))

(defun voice-submit-utterance (lane utterance)
  "Transcribe UTTERANCE and hand it to the kit as one ordinary ask."
  ;; The transcript is POSTED in the text room first, and that message is the
  ;; ask. Three things follow from it and all three are wanted: the room holds
  ;; a readable record of what was said, the answer is a reply to it the way
  ;; every other answer is a reply, and the lane gets the unique id it is
  ;; keyed by — a lane is <room>-m<message>, so an ask with no message of its
  ;; own would share one lane with every other thing ever said aloud.
  (let* ((recording (utterance-recording utterance))
         (host (and *discord-adapter* (discord-adapter-host *discord-adapter*)))
         (text (string-trim '(#\Space #\Tab #\Newline)
                            (or (nck:transcribe-audio recording "ogg"
                                                      :seconds (/ (utterance-milliseconds
                                                                   utterance)
                                                                  1000.0))
                                ""))))
    (cond
      ((zerop (length text)))
      ;; Taking notes: written down, never answered.
      (lane.notes-by
       (bt2:with-lock-held ((voice-lane-lock lane))
         (push (list (nck:now-ms) utterance.user-id text) lane.notes)))
      (host
       (nlk:bind (((delivered message-ids)
                   (nck:post-message host (list :channel-id lane.room-channel-id)
                                     (format nil "🎙 <@~a>: ~a" utterance.user-id text)))
                  (message-id (first message-ids)))
         (if (and delivered message-id)
             (nck:handle-candidate host (voice-candidate lane utterance text message-id))
             ;; The room could not take the transcript. Saying it aloud
             ;; anyway would open a turn with no record, so it does not.
             (setf lane.last-error
                   "the text room refused the transcript; no turn was opened")))))))

;;; --- saying the answer out loud -------------------------------------------------------------

(defun voice-stop-playback (lane)
  "Silence whatever is being said, now."
  ;; The generation counter is the whole mechanism: the speaker thread checks
  ;; it between frames and drops what it was doing the moment it moves.
  (bt2:with-lock-held ((voice-lane-lock lane))
    (incf lane.play-generation)
    (setf lane.play-queue nil)))

(defun voice-wav-to-opus (wav)
  "WAV octets as a list of 20 ms Opus frames at 48 kHz stereo — what Discord
speaks."
  (nck:call-with-scratch-directory
   (lambda (directory)
     (opus-audio-frames
      (nck:read-octets (nck:wav-to-ogg-opus wav (merge-pathnames "answer.ogg" directory)))))))

(defun voice-play (lane frames)
  "Queue FRAMES to be spoken, replacing anything queued behind the current
generation. Returns the generation they belong to."
  (bt2:with-lock-held ((voice-lane-lock lane)) (setf lane.play-queue frames) lane.play-generation))

;;; Discord asks for five: without them the receiver's decoder interpolates
;;; from the last real frame and the answer trails off into a smear.
(defparameter +voice-tail-silence-frames+ 5
  "Frames of silence sent after speech stops.")

(defun voice-send-packet (lane frame sequence timestamp counter)
  "One media frame on the wire: DAVE-encrypted, then transport-sealed under
the RTP header that is its own AAD. => T when it went."
  (let ((sealed (and lane.mls-joined-p (dave-encrypt lane.encryptor lane.ssrc frame))))
    (when (and sealed lane.secret-key)
      (ignore-errors
       (let ((packet (voice-packet lane.secret-key counter sequence timestamp lane.ssrc sealed)))
         (usocket:socket-send lane.udp packet (length packet))
         t)))))

(defun voice-speak-lap (lane stop-p &aux (sequence (random 65536))
                                         (timestamp (random (expt 2 30)))
                                         (counter 1)
                                         (last-ms nil))
  "The paced sender: one 20 ms frame every 20 ms."
  ;; Two things here are not obvious. First, the RTP timestamp is a CLOCK,
  ;; not a counter: it is realigned from the wall clock at the head of every
  ;; burst. Without that, advancing it only per frame sent would leave it
  ;; behind real time by however long the bot was quiet, and a receiver
  ;; would drop packets that old. Second, speaking is announced again before
  ;; each burst and taken back after it, with the tail of silence Discord
  ;; asks for.
  ;;
  ;; Between frames the generation is re-read, which is how a new utterance
  ;; cuts the answer off mid-sentence.
  (flet ((send (frame)
           ;; FRAME as the stream's next packet: sequence, clock and counter move
           ;; whether or not it went out.
           (prog1 (voice-send-packet lane frame sequence timestamp counter)
             (setf sequence (mod (1+ sequence) 65536)
                   timestamp (mod (+ timestamp +opus-frame-samples+) (expt 2 32))
                   counter (1+ counter)
                   last-ms (nck:now-ms)))))
    (loop until (funcall stop-p)
          do (let ((frames nil) (generation nil))
               (bt2:with-lock-held ((voice-lane-lock lane))
                 (setf frames lane.play-queue generation lane.play-generation lane.play-queue nil))
               (if (null frames)
                   (sleep 0.02)
                   (progn
                     ;; The clock moved while we said nothing; the stream's
                     ;; timestamp has to move with it. SEND moves LAST-MS on.
                     (when last-ms
                       (setf timestamp (mod (+ timestamp (* 48 (max 0 (- (nck:now-ms) last-ms))))
                                            (expt 2 32))))
                     (voice-send lane (voice-speaking-payload lane.ssrc t))
                     (loop for frame in frames
                           for due = (+ (get-internal-real-time)
                                        (/ internal-time-units-per-second
                                           +voice-frames-per-second+))
                           until (funcall stop-p)
                           while (= generation lane.play-generation)
                           do (when (send frame) (incf lane.spoken))
                              (let ((wait (/ (- due (get-internal-real-time))
                                             internal-time-units-per-second)))
                                (when (plusp wait) (sleep wait))))
                     ;; The tail, then hand the floor back.
                     (loop repeat +voice-tail-silence-frames+
                           do (send +opus-silence-frame+)
                              (sleep 0.02))
                     (voice-send lane (voice-speaking-payload lane.ssrc nil))))))))

(defun voice-speak-answer (lane text)
  "Say TEXT in the voice channel."
  ;; Text stays the answer: a failure here costs one line in the room and
  ;; nothing else.
  (handler-case
      (nlk:bind (((wav _ cut-p) (nck:synthesize-speech text)))
        (setf lane.active-ms (nck:now-ms))
        (voice-stop-playback lane)
        (voice-play lane (voice-wav-to-opus wav))
        (when cut-p
          (voice-say-in-room lane "voice: said the first part out loud; the rest is above."))
        t)
    (error (condition)
      (voice-say-in-room lane (format nil "voice: could not say that out loud (~a). ~
                                           The answer above is the answer."
                                      (princ-to-string condition)))
      nil)))

;;; --- the voice websocket lap ----------------------------------------------------------------

(defun voice-ws-lap (lane stop-p)
  "One supervised lap against the voice gateway."
  ;; The DAVE handshake, the heartbeat and the speaking state live here; audio
  ;; never does.
  (multiple-value-bind (host port) (voice-endpoint-parts lane.endpoint)
    (let* ((url (format nil "wss://~a:~a/?v=8" host port))
           (inbound (nck:make-work-queue "channel-discord-voice-in" :cap 512))
           (ws (make-discord-socket url)))
      (setf lane.ws ws)
      (wsd:on :message ws
              (lambda (message)
                (nck:queue-push inbound
                                (if (stringp message)
                                    (list :text message)
                                    (list :binary message)))))
      (wsd:on :close ws (lambda (&key code reason)
                          (declare (ignore reason))
                          (nck:queue-push inbound (list :closed (socket-closed-code ws code 1006)))))
      (nlk:with-cleanup ((ignore-errors (nlk:sever-websocket ws))
                         (setf lane.ws nil))
        (block lap
          (wsd:start-connection ws)
          (voice-send lane (voice-identify-payload lane.guild-id
                                                   lane.self-user-id
                                                   lane.session-id
                                                   lane.token))
          (let ((next-beat nil))
            (loop
              (when (funcall stop-p) (return-from lap :stop))
              (nlk:when-let (interval lane.heartbeat-interval)
                (let ((now (nck:now-ms)))
                  (when (or (null next-beat) (>= now next-beat))
                    (setf next-beat (+ now (round (* 1000 interval))))
                    (voice-send lane (voice-heartbeat-payload
                                      lane.binary-sequence)))))
              (nlk:bind (((item found) (nck:queue-pop inbound 0.25)) (kind (first item)))
                ;; The gateway went away. A lane the operator still wants comes
                ;; back on the next lap with a fresh VOICE_SERVER_UPDATE; one
                ;; that was told to leave has already cleared the stop.
                (when (eq kind :closed) (return-from lap 2.0))
                (when found
                  (nlk:with-handlers ((error (condition)
                                        (setf lane.last-error (princ-to-string condition))
                                        (warn "discord voice~:[~; (dave)~]: ~a" (eq kind :binary)
                                              condition)))
                    (if (eq kind :text)
                        (let ((payload (ignore-errors (nlk:decode-json (second item)))))
                          (when (hash-table-p payload) (voice-handle-text lane payload)))
                        (voice-handle-binary
                         lane (coerce (second item) '(vector (unsigned-byte 8)))))))))))))))

;;; --- joining and leaving ----------------------------------------------------------------------

(defun voice-attach-server (lane &key session-id token endpoint)
  "What the main gateway learned about this voice connection."
  ;; Both halves present and the lane's threads start.
  (when session-id (setf lane.session-id session-id))
  (when token (setf lane.token token))
  (when endpoint (setf lane.endpoint endpoint))
  (when (and lane.session-id lane.token lane.endpoint (null lane.stop-lap))
    (voice-start-threads lane))
  lane)

(defun voice-start-threads (lane)
  (setf *voice-deaf-windows* 0
        *voice-deaf-retry-ms* 0
        lane.work-queue (nck:make-work-queue "channel-discord-voice-work"
                                                          :cap 64)
        lane.stop-lap
        (nck:start-supervised "channel-discord-voice"
                              (lambda (stop-p) (voice-ws-lap lane stop-p))
                              :on-degraded
                              (lambda (condition)
                                (setf lane.last-error
                                      (princ-to-string condition)))))
  ;; The listener waits for the socket the gateway's Ready opens.
  (nlk:spawn "channel-discord-voice-listen"
    (loop repeat 200
          until lane.udp
          do (sleep 0.05))
    (when lane.udp (voice-listen-lap lane (lambda () (null lane.stop-lap)))))
  (nlk:spawn "channel-discord-voice-work"
    (voice-work-lap lane (lambda () (null lane.stop-lap))))
  (nlk:spawn "channel-discord-voice-speak"
    (voice-speak-lap lane (lambda () (null lane.stop-lap))))
  lane)

(defun gateway-ready-p (&aux (adapter *discord-adapter*))
  "Whether the main gateway has finished identifying."
  ;; A voice state sent before READY is thrown away. Without this, the bot
  ;; would report itself in the channel while Discord's own voice-states/@me
  ;; says it is in none.
  (and adapter adapter.application-id t))

(defun join-voice (section &key channel room speakers)
  "Sit in CHANNEL, voice_channel_id by default, in the server it is in,
talking through ROOM, else the channel the section names or the call's own
chat (VOICE-ROOM-CHANNEL), and hearing SPEAKERS where it names nobody
(VOICE-SPEAKERS)."
  ;; => (values LANE MESSAGE): the lane, or NIL and the honest reason. Nothing
  ;; here listens yet — the main gateway answers the voice state with a
  ;; server to dial, and VOICE-ATTACH-SERVER starts the threads; whether
  ;; Discord seated the bot at all is VOICE-AWAIT-SEAT's to say.
  (when *voice-lane*
    (return-from join-voice
      (values *voice-lane*
              (format nil "already sitting in <#~a>" (voice-lane-channel-id *voice-lane*)))))
  (let* ((adapter *discord-adapter*)
         (channel (or channel (config-string section "voice_channel_id" nil)))
         (guild (and channel (gethash channel *voice-channel-guilds*)))
         (room (or room (voice-room-channel section channel)))
         (speakers (or speakers (voice-speakers section))))
    (nlk:when-let (refusal
                   (cond ((not (gateway-ready-p))
                          "the Discord gateway is still connecting; try again in a moment")
                         ((null channel) "sit in a voice channel first, then /voice join")
                         ((null guild)
                          (format nil "<#~a> is no voice channel of a server the bot is in" channel))
                         ((null speakers)
                          "nobody is allowed to speak (channels.discord.voice_speakers)")
                         ((null adapter.bot-user-id) "the bot does not know its own user id yet")))
      (return-from join-voice (values nil refusal)))
    (handler-case (load-dave)
      (error (condition)
        (return-from join-voice
          (values nil (format nil "Discord requires its end-to-end encryption library ~
                                   for voice, and it is not here: ~a"
                              (princ-to-string condition))))))
    (let ((lane (%make-voice-lane :guild-id guild :self-user-id adapter.bot-user-id
                                  :channel-id channel :room-channel-id room :speakers speakers)))
      (setf *voice-lane* lane)
      (discord-gateway-send adapter (voice-state-payload guild channel))
      (voice-note "joining <#~a>" channel)
      (values lane nil))))

(defvar *voice-seat-seconds* 10
  "How long a join waits for Discord to say the bot sits down.")

(defun voice-await-seat (lane &aux (until (+ (get-internal-real-time)
                                             (* *voice-seat-seconds* internal-time-units-per-second))))
  "Whether Discord seated LANE within *VOICE-SEAT-SECONDS*: its own voice
state carries the session. A lane it never seated is let go."
  ;; Discord answers a join it will not grant with nothing at all: a bot
  ;; without Connect, or a full channel, waits forever (discord.py's connect
  ;; times out the same way).
  (loop until (or lane.session-id (not (eq lane *voice-lane*))
                  (> (get-internal-real-time) until))
        do (sleep 0.05))
  (cond (lane.session-id t)
        ((eq lane *voice-lane*)
         (leave-voice :send nil)
         ;; Asked to leave a seat it may yet be given; expecting no echo, as
         ;; there may be none.
         (ignore-errors (discord-gateway-send *discord-adapter*
                                              (voice-state-payload lane.guild-id nil)))
         nil)))

(defun voice-unseated-text (lane &aux (adapter *discord-adapter*))
  "Why Discord left LANE standing, and what fixes it."
  (format nil "Discord did not seat me in <#~a>. The bot needs the Connect and Speak ~
               permissions there, and the channel needs room: a server admin can grant ~
               them to the bot's role~@[, or invite the bot again with ~
               https://discord.com/oauth2/authorize?client_id=~a&scope=bot+applications.commands&permissions=309240908864~]."
          lane.channel-id (and adapter adapter.application-id)))

(defun voice-joining-line (lane &optional why)
  "What the room is told when the bot sits down in LANE's channel."
  (format nil "voice: joining <#~a>~@[ (~a)~] — I will listen to ~
               ~:[~{<@~a>~^, ~}~;everyone there~*~] and answer here."
          lane.channel-id why (member "*" lane.speakers :test #'equal) lane.speakers))

(defun leave-voice (&key (send t) (notes t))
  "Stop listening, stop speaking, and clear the voice state Discord keeps."
  ;; => T when there was a lane to leave. SEND NIL leaves Discord's voice state
  ;; as it is: Discord already took the bot out, or a move's own voice state
  ;; replaces it. NOTES NIL leaves the lane's notes on it for the move to
  ;; carry; otherwise notes in progress are written up (VOICE-STOP-NOTES).
  (nlk:when-let (lane *voice-lane*)
    (setf *voice-lane* nil)
    (when notes (voice-stop-notes lane))
    (voice-stop-playback lane)
    (let ((stop lane.stop-lap)) (setf lane.stop-lap nil) (when stop (ignore-errors (funcall stop))))
    (when send
      (setf *voice-expecting-leave* t)
      (ignore-errors (discord-gateway-send *discord-adapter*
                                           (voice-state-payload lane.guild-id nil))))
    (ignore-errors (when lane.udp
                     (usocket:socket-close lane.udp)))
    (voice-free-dave lane)
    ;; Clear the voice line alone: CLEAR-CHANNEL-STATUS takes the whole
    ;; adapter's row down, and the text lane is still running.
    (nck:set-channel-status "discord" :voice nil)
    t))

(defun voice-free-dave (lane)
  "Give libdave back everything this lane took from it."
  (ignore-errors
   (loop for decryptor being the hash-values of lane.decryptors
         do (dave-decryptor-destroy decryptor))
   (clrhash lane.decryptors)
   (loop for ratchet being the hash-values of lane.ratchets do (dave-ratchet-destroy ratchet))
   (clrhash lane.ratchets)
   (when lane.encryptor (dave-encryptor-destroy lane.encryptor) (setf lane.encryptor nil))
   (when lane.dave-session (dave-session-destroy lane.dave-session) (setf lane.dave-session nil))))

;;; --- where the bot sits by its own rules ---------------------------------------------------------
;;; After OpenClaw (extensions/discord/src/voice at 7ba58abd93d): voice_follow
;;; seats the bot beside the people it names wherever they sit in the server
;;; (voice-following.ts), voice_autojoin seats it in its own channel while
;;; somebody is in it (autoJoin whenOccupied, voice-runtime.ts), and it gets
;;; up when the last person leaves. After Hermes (adapter.py's
;;; voice_channel_inactivity_timeout): a seat it took on /voice join is
;;; given up after voice_idle_minutes nobody spoke to it. A move is a
;;; fresh lane: the voice state names the new channel, and Discord answers
;;; with a new voice server to dial (discord.py's voice_state.py move_to).

(defun voice-humans-in (channel &optional (states *voice-states*))
  "The people, no bots, sitting in CHANNEL."
  (loop for user being the hash-keys of states using (hash-value (where . bot-p))
        when (and (equal where channel) (not bot-p)) collect user))

(defun voice-place (section &optional (states *voice-states*)
                    &aux (home (config-string section "voice_channel_id" nil)))
  "(values CHANNEL WHY): where the bot's rules seat it now — beside the first
person voice_follow names who is in voice, else in its own channel while
voice_autojoin is on and somebody is there — or NIL."
  (dolist (user (config-string-list section "voice_follow"))
    (nlk:when-let (channel (car (gethash user states)))
      (return-from voice-place (values channel (format nil "following <@~a>" user)))))
  (when (and home (config-boolean section "voice_autojoin" nil) (voice-humans-in home states))
    (values home "somebody is in it")))

(defun voice-reaction (section here user was &optional (states *voice-states*))
  "(values ACTION CHANNEL WHY) once USER, a person, has moved out of WAS —
STATES saying where everyone sits now, the bot in HERE (NIL: nowhere): :JOIN
or :MOVE to CHANNEL, :LEAVE, or NIL to stay. A NIL USER asks with nobody
having moved, as the boot does."
  (multiple-value-bind (place why) (voice-place section states)
    (cond
      ;; Somebody it follows moved: it goes where its rules seat it now, and
      ;; leaves the channel they left when no rule seats it anywhere.
      ((member user (config-string-list section "voice_follow") :test #'equal)
       (cond ((equal place here) nil)
             (place (values (if here :move :join) place why))
             ((and here (equal was here)) (values :leave nil (format nil "<@~a> left" user)))))
      ;; The last person left the channel it sits in.
      ((and here (equal was here) (null (voice-humans-in here states)))
       (if place (values :move place why) (values :leave nil "everyone left")))
      ((and (null here) place) (values :join place why)))))

(defun voice-act (section action channel why &aux (lane *voice-lane*))
  "Carry out what VOICE-REACTION decided, saying it in the room."
  (case action
    (:leave (when lane
              (voice-say-in-room lane (format nil "voice: leaving <#~a> — ~a." lane.channel-id why))
              (leave-voice)))
    ((:join :move)
     ;; A move carries the notes being taken to the next seat.
     (let ((notes (and lane (list lane.notes lane.notes-by lane.notes-from-ms))))
       (when (eq action :move) (leave-voice :send nil :notes nil))
       (multiple-value-bind (seated refusal)
           (join-voice section :channel channel :room (and lane lane.room-channel-id)
                               :speakers (and lane lane.speakers))
         (cond ((null seated)
                (voice-note "not joined: ~a" refusal)
                ;; A move that found no seat gets up from the old one too.
                (when (eq action :move)
                  (voice-stop-notes lane)
                  (setf *voice-expecting-leave* t)
                  (ignore-errors (discord-gateway-send *discord-adapter*
                                                       (voice-state-payload lane.guild-id nil)))))
               ((not (voice-await-seat seated))
                (voice-say-in-room seated (format nil "voice: ~a" (voice-unseated-text seated))))
               (t (when (second notes)
                    (setf (values (voice-lane-notes seated) (voice-lane-notes-by seated)
                                  (voice-lane-notes-from-ms seated))
                          (values-list notes)))
                  (voice-say-in-room seated (voice-joining-line seated why)))))))))

(defun voice-apply-rules (user was)
  "What the bot's rules make of USER having moved out of WAS, acted on off
the gateway's thread — a join loads libdave — one seat change at a time."
  (nlk:when-let (section *voice-section*)
    (nlk:spawn "channel-discord-voice-seat"
      (bt2:with-lock-held (*voice-seat-lock*)
        (nlk:with-handlers ((error (condition) (voice-note "~a" condition)))
          (let ((lane *voice-lane*))
            (multiple-value-call #'voice-act section
              (voice-reaction section (and lane lane.channel-id) user was))))))))

(defun voice-idle-p (lane section now)
  "Whether LANE has sat past voice_idle_minutes with nobody speaking to it,
in a seat it took on /voice join: one its rules hold is theirs to give up,
and a meeting being written down is not idle."
  (let ((idle (* 60000 (config-integer section "voice_idle_minutes" 5 :min 0))))
    (and (plusp idle)
         (null lane.notes-by)
         (not (equal lane.channel-id (voice-place section)))
         (>= (- now lane.active-ms) idle))))

(defun voice-leave-if-idle (lane &aux (section *voice-section*))
  "On the listener's tick: get up from an idle seat (VOICE-IDLE-P)."
  (when (and section (not lane.leaving-p) (voice-idle-p lane section (nck:now-ms)))
    (setf lane.leaving-p t)
    (nlk:spawn "channel-discord-voice-idle"
      (bt2:with-lock-held (*voice-seat-lock*)
        (when (eq lane *voice-lane*)
          (voice-act section :leave nil
                     (format nil "nobody has spoken to me for ~d minute~:p"
                             (config-integer section "voice_idle_minutes" 5))))))))

;;; --- meeting notes ---------------------------------------------------------------------------
;;; After OpenClaw's transcripts (src/transcripts at 7ba58abd93d), where the
;;; notes are a keyword summary written to disk: here what is said is kept
;;; with who said it, and when the notes stop the room gets the transcript as
;;; a file and the model writes the notes from it, as an ask in the name of
;;; whoever asked for them.

(defvar *voice-notes-asks* '()
  "The messages notes were asked on, newest first: their answer is read, not
said out loud (VOICE-OBSERVE-FRAME).")

(defun voice-notes-transcript (lines from-ms)
  "LINES, (MS USER-ID TEXT) oldest first, as one line each: minutes and
seconds from FROM-MS, who, what."
  (format nil "~{~a~^~%~}"
          (loop for (ms user text) in lines
                collect (multiple-value-bind (minutes seconds) (floor (round (- ms from-ms) 1000) 60)
                          (format nil "[~2,'0d:~2,'0d] <@~a>: ~a" minutes seconds user text)))))

(defun voice-notes-ask (channel minutes people transcript)
  "What the model is asked when notes stop."
  (format nil "Write up the notes of the meeting just held in the voice channel <#~a>: ~
               a few lines on what it was about, then what was decided, who is doing ~
               what, and what is still open — only what the transcript says, naming ~
               people as it does. ~d minute~:p, ~d ~:*~[people~;person~:;people~]; ~
               the transcript, one line per thing said, from the start:~%~%~a"
          channel minutes people transcript))

(defun voice-stop-notes (lane)
  "End LANE's notes. => how many lines were taken; the room gets them on a
thread of their own, the lane being on its way out or on with its work."
  (multiple-value-bind (lines by from)
      (bt2:with-lock-held ((voice-lane-lock lane))
        (values (reverse (shiftf lane.notes '())) (shiftf lane.notes-by nil) lane.notes-from-ms))
    (when (and by lines)
      (let ((room lane.room-channel-id) (channel lane.channel-id) (guild lane.guild-id))
        (nlk:spawn "channel-discord-voice-notes"
          (nlk:with-handlers ((error (condition)
                                (voice-note "the notes were not handed on: ~a" condition)))
            (voice-hand-notes room channel guild lines by from)))))
    (length lines)))

(defun voice-hand-notes (room channel guild lines by from-ms)
  "Post LINES, the notes taken in CHANNEL, in ROOM as a transcript file, and
ask the room in BY's name to write them up."
  ;; The file's message is the ask: the answer replies to it, and the lane is
  ;; keyed by it.
  (let ((host (or (and *discord-adapter* (discord-adapter-host *discord-adapter*))
                  (error "the Discord lane is not running")))
        (people (length (remove-duplicates (mapcar #'second lines) :test #'equal)))
        (minutes (max 1 (round (- (nck:now-ms) from-ms) 60000)))
        (transcript (voice-notes-transcript lines from-ms)))
    (nck:call-with-scratch-directory
     (lambda (directory)
       (let* ((file (nck:write-octets (sb-ext:string-to-octets transcript :external-format :utf-8)
                                      (merge-pathnames "meeting-transcript.txt" directory)))
              (result (nck:execute-plan
                       (host-executor host)
                       (discord-file-message-plan
                        (list :channel-id room) file
                        :content (format nil "voice: notes from <#~a> — ~d line~:p from ~d ~
                                              ~:*~[people~;person~:;people~] over ~d minute~:p."
                                         channel (length lines) people minutes))))
              (message-id (and (execution-ok-p result)
                               (nlk:json-value (execution-body result) :string "id"))))
         (unless message-id
           (error "the room refused the transcript: ~a" (execution-error result)))
         (push message-id *voice-notes-asks*)
         (setf *voice-notes-asks* (subseq *voice-notes-asks* 0 (min 8 (length *voice-notes-asks*))))
         (nck:handle-candidate
          host (nlk:json-object
                "text" (voice-notes-ask channel minutes people transcript)
                "source" (nlk:json-object "platform" "discord" "chat_kind" "channel"
                                          "channel_id" room "message_id" message-id
                                          "user_id" by "workspace_id" guild
                                          "addressed" t "voice_channel_id" channel))))))))

(defun voice-announce (lane line typed)
  "LINE, a /voice answer typed in the channel TYPED, said in LANE's room too
unless it was typed there, where the answer already says it. => LINE."
  (unless (equal typed lane.room-channel-id) (voice-say-in-room lane line))
  line)

(defun voice-notes-command (lane argument by &optional typed)
  "/voice notes [stop], typed in the channel TYPED."
  (cond
    ((equal argument "stop")
     (if lane.notes-by
         (format nil "voice: notes stopped — ~:[nobody said anything~;writing them up in <#~a>~]."
                 (plusp (voice-stop-notes lane)) lane.room-channel-id)
         "voice: no notes are being taken"))
    (argument (format nil "voice: notes, or notes stop — not ~s" argument))
    (lane.notes-by "voice: already taking notes; `/voice notes stop` writes them up")
    ;; The notes are written up as an ask, and an ask is somebody's.
    ((null by) "voice: ask for notes in Discord, so they have somebody to answer")
    (t (bt2:with-lock-held ((voice-lane-lock lane))
         (setf lane.notes '() lane.notes-by by lane.notes-from-ms (nck:now-ms)))
       (voice-announce lane (format nil "voice: taking notes in <#~a> — what everyone says is ~
                                         written down and nothing is answered until `/voice notes stop`."
                                    lane.channel-id)
                       typed))))

(defun voice-how-to-start (section lead)
  "LEAD, how to start while the bot sits nowhere, then where its rules would
seat it on their own."
  (format nil "~a~@[; voice_autojoin sits in <#~a> whenever somebody is there~]~
               ~@[; I follow ~{<@~a>~^, ~} into voice~]."
          lead
          (and (config-boolean section "voice_autojoin" nil)
               (config-string section "voice_channel_id" nil))
          (config-string-list section "voice_follow")))

(defparameter +voice-reply-labels+
  '(("off" . "Words only") ("on" . "Voice for voice notes") ("tts" . "Voice for every answer"))
  "What /voice calls each of +VOICE-REPLIES+.")

(defun voice-replies-label (room)
  "What ROOM's voice messages are, in /voice's words."
  (cdr (assoc (or (nck:room-voice-replies room) "off") +voice-reply-labels+ :test #'string=)))

(defun voice-channel-name (id &aux (entry (and id (gethash id *voice-channel-names*))))
  "Channel ID as /voice names it: #NAME where its server named it, else its
mention."
  (if entry (format nil "#~a" (first entry)) (format nil "<#~a>" id)))

(defun voice-person (user)
  "USER as /voice names them: their server name, else their mention."
  (or (gethash user *voice-member-names*) (format nil "<@~a>" user)))

(defun voice-channels (guild types)
  "(ID . NAME) of GUILD's channels of TYPES — every server's for GUILD NIL --
in name order."
  (sort (loop for id being the hash-keys of *voice-channel-names* using (hash-value (name type where))
              when (and (member type types) (or (null guild) (equal guild where)))
                collect (cons id name))
        #'string-lessp :key #'cdr))

(defun voice-whom (speakers)
  "SPEAKERS, a list of user ids where `*' is everyone, in words."
  (cond ((member "*" speakers :test #'equal) "everyone")
        (speakers (format nil "~{~a~^, ~}" (mapcar #'voice-person speakers)))
        (t "nobody")))

(defun voice-hearing (lane)
  "Whom LANE hears, in words."
  (voice-whom lane.speakers))

(defun voice-row (label detail verb line &rest more)
  "A /voice dialog row: LABEL and DETAIL, its button VERB, the slash LINE
its press runs, MORE its other members (:mark, :mark-tone)."
  (list* :label label :detail detail :verb verb :run (list :enter line) more))

(defun voice-dialog (section room &aux (lane *voice-lane*) (in-room (and *command-source* t)))
  "Bare /voice as a dialog (NLE:LIST-DIALOG): where the bot sits, the channel
it talks through, whom it hears, its notes and ROOM's voice messages, each
row naming its own press — Leave, Change, Take — and the line that does it.
A shell paints it as a picker, a room as a card (the kit's DIALOG-CARD):
one owner for both."
  ;; Voice messages and new notes are a room's — what a room hears, and an
  ;; ask somebody there answers for — so a shell offers neither.
  (nle:list-dialog
   :title "Voice"
   :context (if lane (format nil "in ~a" (voice-channel-name lane.channel-id)) "not seated")
   :tone (and lane :success)
   :summary (if lane
                (format nil "Heard ~a utterance~:p, sent ~a frame~:p~:[ — waiting for somebody else ~
                             to sit down~;~]."
                        lane.heard lane.spoken lane.mls-joined-p)
                (voice-how-to-start section "Not in a voice channel. Join sits beside you, or in the channel you pick"))
   ;; a shell's selected row says what Enter does there, as a room's button does
   :rows (mapcar (lambda (row) (list* :suffix (format nil "  · enter ~a" (getf row :verb)) row))
          (append
           (remove nil
                 (list (if lane
                           (voice-row "Sitting in" (voice-channel-name lane.channel-id) "leave" "/voice leave"
                                      :mark "●" :mark-tone :success)
                           (voice-row "Sitting in" "nowhere" "join" "/voice join" :mark "○" :mark-tone :muted))
                       (and lane (voice-row "Talking through" (voice-talk-through lane)
                                            "change" "/voice room"))
                       (and lane (voice-row "Hearing" (voice-hearing lane) "change" "/voice hear"))
                       (and lane (cond (lane.notes-by
                                        (voice-row "Notes" (format nil "~a line~:p so far" (length lane.notes))
                                                   "stop" "/voice notes stop"))
                                       (in-room (voice-row "Notes" "not taking" "take" "/voice notes"))))
                       (and in-room (voice-row "Voice messages here" (voice-replies-label room)
                                               "change" "/voice replies"))))
           (voice-model-rows (and in-room room))))))

(defun voice-pick-dialog (title context summary rows empty &optional (verb "use"))
  "A /voice sub-list: TITLE over CONTEXT, SUMMARY, ROWS to pick one of, EMPTY
its copy with none, VERB what a pick does."
  (nle:list-dialog :title title :context context :summary summary :rows rows
                   :empty-label empty :searchable (> (length rows) 8)
                   :hint (format nil "enter ~a  esc back" verb)))

(defun voice-join-dialog ()
  "/voice join with nobody to sit beside: the voice channels to sit in, and
who is in each."
  (voice-pick-dialog
   "Join" "voice channels" "Where to sit."
   (loop for (id . name) in (voice-channels nil '(2 13))
         for people = (voice-humans-in id)
         collect (voice-row (format nil "#~a" name)
                            (if people (format nil "~{~a~^, ~}" (mapcar #'voice-person people)) "empty")
                            "join" (format nil "/voice join ~a" id)
                            :mark (if people "●" "") :mark-tone :success))
   "No voice channels known yet: a server lists its own when the bot connects."
   "join"))

;;; --- what voice hears, speaks and thinks with ---------------------------------------------

(defparameter *voice-catalog-seconds* 300
  "How long what a model server lists stands before it is asked again.")

(defvar *voice-catalogs* (make-hash-table :test #'equal :synchronized t)
  "base_url -> (READ-AT MODELS VOICES): what an OpenAI-compatible server
listed at /models.")

(defun voice-server-catalog (base &aux (now (get-universal-time))
                                       (seen (gethash base *voice-catalogs*)))
  "(values MODELS VOICES): the model ids the server at BASE lists at /models
and the voices it names beside them, where it names any -- NIL for a server
that does not answer; asked again after *VOICE-CATALOG-SECONDS*."
  (if (and seen (< (- now (first seen)) *voice-catalog-seconds*))
      (values (second seen) (third seen))
      (let* ((json (ignore-errors
                    (multiple-value-bind (body status)
                        (nlk:http :get (format nil "~a/models" (string-right-trim "/" base))
                                  :connect-timeout 2 :timeout 3)
                      (and (< status 400) (nlk:decode-json (nlk:body-text body))))))
             (models (and (hash-table-p json)
                          (loop for entry across (or (nlk:json-value json :array "data") #())
                                for id = (nlk:json-value entry :string "id")
                                when id collect id)))
             (voices (and (hash-table-p json)
                          (remove-if-not #'stringp (coerce (or (nlk:json-value json :array "voices") #()) 'list)))))
        (setf (gethash base *voice-catalogs*) (list now models voices))
        (values models voices))))

(defun voice-where (base)
  "Where the engine at BASE runs, in words: built in for none, a server on
this machine for a loopback address, else the server's host."
  (let ((host (and base (ppcre:register-groups-bind (host) ("^[a-zA-Z]+://\\[?([^/\\]:]+)" base) host))))
    (cond ((zerop (length base)) "built in")
          ((member host '("127.0.0.1" "localhost" "::1") :test #'equal) "a server on this machine")
          (t (or host base)))))

(defun voice-engine (section)
  "(values BASE MODEL VOICE ON) of SECTION, the transcription or speech
section as the kit holds it: its server, \"\" for the built-in engine, the
model and voice it names, and whether it is on."
  (flet ((said (key) (and (hash-table-p section) (nlk:json-value section :string key))))
    (values (or (said "base_url") "") (said "model") (said "voice")
            (not (and (hash-table-p section) (eq (gethash "enabled" section) :false))))))

(defun voice-hearing-picks ()
  "(values PICKS CURRENT WHERE): the models speech can be heard with -- the
transcription server's, the one in use first -- and where they run."
  (multiple-value-bind (base model) (voice-engine nck:*transcription*)
    (if (zerop (length base))
        (values (list "Parakeet TDT 0.6B") "Parakeet TDT 0.6B" "built in")
        (values (remove-duplicates (cons model (voice-server-catalog base)) :test #'equal :from-end t)
                model (voice-where base)))))

(defun voice-speaking-picks ()
  "(values PICKS CURRENT WHERE MODEL): the voices an answer can be said in --
the speech server's, the one in use first, 25 at most -- where they run and
the model that says them."
  (multiple-value-bind (base model voice) (voice-engine nck:*speech*)
    (if (zerop (length base))
        (values (list "en_US lessac") "en_US lessac" "built in" "Piper")
        (let* ((voice (or voice "alloy"))
               (picks (remove-duplicates (cons voice (nth-value 1 (voice-server-catalog base)))
                                         :test #'equal :from-end t)))
          (values (subseq picks 0 (min 25 (length picks))) voice (voice-where base) model)))))

(defun voice-thinking-picks (room)
  "(values PICKS CURRENT): the models ROOM may run on, as (PROVIDER MODEL
WHAT) -- the one it runs on, the organism's default, its fallbacks -- and
the one in use."
  (flet ((pair (entry) (and entry (getf entry :model) (list (getf entry :provider) (getf entry :model)))))
    (let* ((default (pair (ignore-errors (nle:config-get '("general" "default_model")))))
           (current (or (multiple-value-bind (provider model) (and room (nck:room-lane-model room))
                          (and model (list provider model)))
                        default))
           (chain (ignore-errors (nle:config-get '("failover" "chain"))))
           (picks (remove-duplicates
                   (remove nil (append (list (and current (append current (list (if (equal current default) "default" "this room's"))))
                                             (and default (append default (list "default"))))
                                       (and (vectorp chain)
                                            (loop for entry across chain
                                                  for each = (pair entry)
                                                  when each collect (append each (list "fallback"))))))
                   :test (lambda (a b) (equal (subseq a 0 2) (subseq b 0 2))) :from-end t)))
      (values picks current))))

(defun voice-model-rows (room)
  "The rows saying what voice hears, speaks and thinks with, each offering
the picks its line makes (NLE:LIST-DIALOG's OPTIONS), the one in use
marked."
  (flet ((option (label detail current line)
           (list :label label :detail detail :mark (if current "●" "") :run (list :enter line))))
    (append
     (multiple-value-bind (picks current where) (voice-hearing-picks)
       (list (voice-row "Hears with" (if (nth-value 3 (voice-engine nck:*transcription*))
                                         (format nil "~a · ~a" current where)
                                         "off")
                        "pick" "/voice hears"
                        :options (loop for each in picks
                                       collect (option each where (equal each current)
                                                       (format nil "/voice hears ~a" each))))))
     (multiple-value-bind (picks current where model) (voice-speaking-picks)
       (list (voice-row "Speaks with" (if (nck:speech-enabled-p)
                                          (format nil "~@[~a ~]~a · ~a" model current where)
                                          "off")
                        "pick" "/voice speaks"
                        :options (loop for each in picks
                                       collect (option each (format nil "~@[~a · ~]~a" model where)
                                                       (equal each current)
                                                       (format nil "/voice speaks ~a" each))))))
     (multiple-value-bind (picks current) (voice-thinking-picks room)
       (and picks
            (list (voice-row "Thinks with" (format nil "~{~a/~a~}" current) "pick" "/models"
                             :options (loop for (provider model what) in picks
                                            collect (option model (format nil "~a · ~a" provider what)
                                                            (equal (list provider model) current)
                                                            (format nil "/models ~a ~a" provider model))))))))))

(defun voice-pick-model (kind word)
  "/voice hears|speaks [WORD], KIND :hears or :speaks: the
picks as a sub-list, or the one WORD names made the one in use -- written
to the shared config and to the section the kit speaks and hears with."
  (multiple-value-bind (picks current) (if (eq kind :hears) (voice-hearing-picks) (voice-speaking-picks))
    (let ((what (if (eq kind :hears) "hearing with" "speaking with"))
          (path (if (eq kind :hears) '("transcription" "model") '("speech" "voice")))
          (section (if (eq kind :hears) nck:*transcription* nck:*speech*)))
      (cond
        ((null word)
         (values (format nil "voice: ~a ~a" what current)
                 (voice-pick-dialog
                  (if (eq kind :hears) "Hears with" "Speaks with") ""
                  (if (eq kind :hears) "The model speech is heard with." "The voice answers are said in.")
                  (loop for each in picks
                        collect (voice-row each "" "use" (format nil "/voice ~(~a~) ~a" kind each)
                                           :mark (if (equal each current) "●" "") :mark-tone :accent))
                  "")))
        ((not (hash-table-p section))
         (format nil "voice: the built-in engine has only ~a" current))
        ((not (member word picks :test #'equal))
         (format nil "voice: ~a is none of ~{~a~^, ~}" word picks))
        (t (nle:config-set path word)
           (setf (gethash (second path) section) word)
           (format nil "voice: ~a ~a now." what word))))))

(defun voice-room-choices (lane section &aux (allowed (config-string-list section "allowed_channels")))
  "(ID . NAME) of the channels LANE may talk through: the call's own chat,
then its server's text channels -- those allowed_channels names where it
names any."
  (remove-if-not (lambda (choice) (or (null allowed) (member (car choice) allowed :test #'equal)))
                 (cons (cons lane.channel-id (or (first (gethash lane.channel-id *voice-channel-names*))
                                                 lane.channel-id))
                       (voice-channels lane.guild-id '(0 5)))))

(defun voice-talk-through (lane)
  "The channel LANE talks through, in words: the call's own chat said so."
  (if (equal lane.room-channel-id lane.channel-id)
      (format nil "~a · the call's own chat" (voice-channel-name lane.channel-id))
      (voice-channel-name lane.room-channel-id)))

(defun voice-room-command (lane channel section typed &aux (choices (voice-room-choices lane section)))
  "/voice room [CHANNEL]: the text channels LANE may talk through, or talk
through CHANNEL, typed in the channel TYPED."
  (cond
    ((null channel)
     (values (format nil "voice: talking through ~a" (voice-talk-through lane))
             (voice-pick-dialog
              "Talking through" (voice-channel-name lane.channel-id)
              "The text channel what is said in voice goes to, and answers come from."
              (loop for (id . name) in choices
                    collect (voice-row (format nil "#~a" name) (if (equal id lane.channel-id) "the call's own chat" "")
                                       "use" (format nil "/voice room ~a" id)
                                       :mark (if (equal id lane.room-channel-id) "●" "") :mark-tone :accent))
              "No text channels known yet: a server lists its own when the bot connects.")))
    ((not (assoc channel choices :test #'equal))
     (format nil "voice: ~a is no text channel I may talk through" (voice-channel-name channel)))
    (t (setf lane.room-channel-id channel)
       (voice-announce lane (format nil "voice: talking through ~a now." (voice-talk-through lane)) typed))))

(defun voice-hear-command (lane whom section by typed &aux (default (voice-speakers section by)))
  "/voice hear [everyone | default]: whom LANE could hear, or hear everyone in
its channel or the room's speakers (VOICE-SPEAKERS), typed by BY in TYPED."
  (let ((everyone (and (member "*" lane.speakers :test #'equal) t)))
    (cond
      ((null whom)
       (values (format nil "voice: hearing ~a" (voice-hearing lane))
               (voice-pick-dialog
                "Hearing" (voice-channel-name lane.channel-id)
                "Whose speech opens a turn. Notes hear everyone while they run."
                (list (voice-row "Everyone" "anybody in the voice channel" "hear" "/voice hear everyone"
                                 :mark (if everyone "●" "") :mark-tone :accent)
                      (voice-row "The room's speakers"
                                 (voice-whom default)
                                 "hear" "/voice hear default"
                                 :mark (if (and (not everyone) (equal lane.speakers default)) "●" "")
                                 :mark-tone :accent))
                "" "hear")))
      ((string-equal whom "everyone")
       (setf lane.speakers (list "*"))
       (voice-announce lane (format nil "voice: hearing everyone in ~a now." (voice-channel-name lane.channel-id))
                       typed))
      ((string-not-equal whom "default") (format nil "voice: hear everyone, or default — not ~s" whom))
      ((null default) "voice: nobody is allowed to speak (channels.discord.voice_speakers)")
      (t (setf lane.speakers default)
         (voice-announce lane (format nil "voice: hearing ~a now." (voice-hearing lane)) typed)))))

(defun voice-status-text (section &aux (lane *voice-lane*))
  "What /voice status says."
  (cond
    ((null lane)
     (voice-how-to-start section (concatenate 'string "voice: not in a voice channel. "
                                             "Sit in one and type `/voice join`, and I sit beside you")))
    (t
     (format nil "voice: in <#~a>, talking through <#~a>~%~
                    · encryption: ~a~%~
                    · speakers: ~{~a~^, ~}~%~
                    · heard ~a utterance~:p, sent ~a frame~:p~
                    ~@[~%· taking notes: ~a line~:p so far~]~@[~%· last trouble: ~a~]"
             lane.channel-id
             lane.room-channel-id
             (if lane.mls-joined-p
                 (format nil "end-to-end, DAVE v~a" (or lane.dave-version +voice-dave-version+))
                 "waiting for the group — nobody else is in the channel yet")
             (or lane.speakers (list "nobody"))
             lane.heard
             lane.spoken
             (and lane.notes-by (length lane.notes))
             lane.last-error))))

;;; --- the answer, said out loud ------------------------------------------------------------------

(defparameter +voice-hook-key+ "channel-discord-voice")

(defun voice-room-session-p (lane session-id)
  "Whether SESSION-ID is the voice lane's text room or anything forked off
it."
  ;; The kit names a room discord-<channel> and a lane below it, adding
  ;; -t<thread> when it opens a thread for the ask and -m<message> for the ask
  ;; itself; matching the room prefix is how this stays true whatever the kit
  ;; keys a lane by, which guessing the lane's own name did not.
  (and (stringp session-id)
       (uiop:string-prefix-p (format nil "discord-~a" lane.room-channel-id) session-id)))

(defun voice-observe-frame (op &aux (lane *voice-lane*))
  "An answer that lands in the voice lane's room is said out loud — the room
the bot is sitting in a voice channel for, whether the ask was spoken or
typed."
  ;; Every other frame is none of voice's business.
  ;;
  ;; Runs on whichever thread published the frame, so it only hands the words
  ;; to the work queue; synthesis is the worker's.
  (when (and lane lane.mls-joined-p (voice-room-session-p lane (nck:frame-session-id op)))
    (nlk:bind (((kind payload _ turn-id) (nck:frame-fact op)))
      (when (and (equal kind "turn.assistant_message_completed")
                 ;; A round that called tools is still working: what it said
                 ;; there is commentary, not the answer.
                 (not (nck:fact-message-tool-calls payload))
                 ;; One turn, one spoken answer: the thread's session and the
                 ;; room's both carry it.
                 (not (gethash turn-id lane.spoken-turns))
                 ;; Meeting notes are for reading.
                 (notany (lambda (id) (search id (nck:frame-session-id op))) *voice-notes-asks*))
        (let ((text (nck:fact-message-content payload)))
          (when (and text (plusp (length text)))
            (setf (gethash turn-id lane.spoken-turns) t)
            (nck:queue-push lane.work-queue
                            (list :speak text))))))))

;;; --- what the main gateway tells us -------------------------------------------------------------

(defun voice-seed-states (guild &optional (bot-user-id ""))
  "Who sits where in GUILD, a GUILD_CREATE's d, and which voice channels are
its: that server's part of the tables, the rest kept."
  ;; Its voice states carry no member; the members it lists say who is a bot.
  (let ((id (nlk:json-value guild :string "id"))
        (bots (cons bot-user-id
                    (loop for member across (or (nlk:json-value guild :array "members") #())
                          for user = (nlk:json-value member :object "user")
                          when (nlk:json-value user :boolean "bot")
                            collect (nlk:json-value user :string "id")))))
    (loop for member across (or (nlk:json-value guild :array "members") #())
          for user = (nlk:json-value member :object "user")
          for name = (or (nlk:json-value member :string "nick")
                         (nlk:json-value user :string "global_name")
                         (nlk:json-value user :string "username"))
          when name
            do (setf (gethash (nlk:json-value user :string "id") *voice-member-names*) name))
    (loop for channel across (or (nlk:json-value guild :array "channels") #())
          for channel-id = (nlk:json-value channel :string "id")
          for type = (nlk:json-value channel :integer "type")
          when (member type '(0 2 5 13)) ; text, voice, announcement, stage
            do (setf (gethash channel-id *voice-channel-names*)
                     (list (or (nlk:json-value channel :string "name") channel-id) type id))
          when (member type '(2 13))
            do (setf (gethash channel-id *voice-channel-guilds*) id))
    (loop for user being the hash-keys of *voice-states* using (hash-value (channel))
          when (equal id (gethash channel *voice-channel-guilds*))
            do (remhash user *voice-states*))
    (loop for state across (or (nlk:json-value guild :array "voice_states") #())
          for user = (nlk:json-value state :string "user_id")
          for channel = (nlk:json-value state :string "channel_id")
          when (and user channel)
            do (setf (gethash channel *voice-channel-guilds*) id
                     (gethash user *voice-states*)
                     (cons channel (and (member user bots :test #'equal) t))))))

(defun voice-own-state (channel session-id &aux (lane *voice-lane*))
  "The bot's own VOICE_STATE_UPDATE: in CHANNEL under SESSION-ID, or out."
  (cond
    ;; Out because it asked to be: that leave already ended the lane.
    ((and (null channel) *voice-expecting-leave*) (setf *voice-expecting-leave* nil))
    ;; Out because somebody disconnected it in Discord: the lane is over.
    ((null channel)
     (when lane
       (voice-say-in-room lane (format nil "voice: disconnected from <#~a> in Discord."
                                       lane.channel-id))
       (leave-voice :send nil)))
    ((null lane) nil)
    ;; Dragged into another channel by somebody: a fresh lane there, which
    ;; the server update that follows dials.
    ((and lane.session-id (not (equal channel lane.channel-id)))
     (let ((section *voice-section*)
           ;; a room picked apart from the call goes along; the call's own
           ;; chat stays behind, and the new call's is the one talked through
           (room (and (not (equal lane.room-channel-id lane.channel-id)) lane.room-channel-id)))
       (leave-voice :send nil :notes nil)
       (multiple-value-bind (seated refusal)
           (and section (join-voice section :channel channel :room room
                                            :speakers lane.speakers))
         (if seated
             (voice-attach-server seated :session-id session-id)
             (voice-note "not joined: ~a" refusal)))))
    (t (setf lane.channel-id channel)
       (voice-attach-server lane :session-id session-id))))

(defun route-voice-dispatch (adapter payload &aux (lane *voice-lane*)
                                                  (section *voice-section*)
                                                  (event (nlk:json-value payload :string "t"))
                                                  (data (nlk:json-value payload :object "d")))
  "GUILD_CREATE, VOICE_STATE_UPDATE and VOICE_SERVER_UPDATE: who sits where
in the voice channels, and the two dispatches that carry a voice
connection."
  ;; Everything else answers NIL and falls through to the ordinary routing,
  ;; GUILD_CREATE included: it seeds threads too. Runs on the lap thread: it
  ;; only records, and hands a seat change to a thread of its own.
  ;;
  ;; The bot's own VOICE_STATE_UPDATE carries the session id the voice gateway
  ;; identifies with, and VOICE_SERVER_UPDATE carries the token and the server
  ;; to dial. Both are needed and they arrive in either order.
  (when data
    (cond
      ((equal event "GUILD_CREATE")
       (voice-seed-states data adapter.bot-user-id)
       (when section (voice-apply-rules nil nil))
       nil)
      ((equal event "VOICE_STATE_UPDATE")
       (let ((user (nlk:json-value data :string "user_id"))
             (channel (nlk:json-value data :string "channel_id"))
             (guild (nlk:json-value data :string "guild_id")))
         (when (and channel guild)
           (setf (gethash channel *voice-channel-guilds*) guild))
         (cond
           ((equal user adapter.bot-user-id)
            (voice-own-state channel (nlk:json-value data :string "session_id")))
           (user
            (let ((was (car (gethash user *voice-states*)))
                  (bot-p (nlk:json-value data :boolean "member" "user" "bot")))
              (if channel
                  (setf (gethash user *voice-states*) (cons channel bot-p))
                  (remhash user *voice-states*))
              (unless (or bot-p (equal was channel))
                (voice-apply-rules user was))))))
       t)
      ((equal event "VOICE_SERVER_UPDATE")
       (when lane
         (voice-attach-server lane
                              :token (nlk:json-value data :string "token")
                              :endpoint (nlk:json-value data :string "endpoint")))
       t))))

;;; --- /voice ---------------------------------------------------------------------------------

(defun voice-command (argument section &optional room)
  "`/voice join [CHANNEL] | leave | status | room [CHANNEL] | hear [everyone |
default] | replies | on | tts | off | notes [stop] | say WORDS`, typed in
ROOM — bare, the status in words and the dialog (VOICE-DIALOG)."
  ;; => the line to show, and the dialog where the answer is one. The room
  ;; hears a seat change once: a voice channel the bot is sitting in is a
  ;; visible state, and the text room is where visible state belongs. Who
  ;; typed it (NCK:*COMMAND-SOURCE*) is where /voice join sits down, the
  ;; channel it was typed in where it talks, and whose name notes are asked
  ;; in; a shell, with nobody to sit beside, picks the channel from a list.
  (let* ((words (remove "" (uiop:split-string (or argument "") :separator '(#\Space #\Tab))
                       :test #'string=))
         (verb (string-downcase (or (first words) "")))
         (object (second words))
         (by (and *command-source* (nlk:json-value *command-source* :text "user_id")))
         (typed (and *command-source* (nlk:json-value *command-source* :text "channel_id")))
         (lane *voice-lane*))
    ;; A change pressed on a room's card answers the card again, the change
    ;; on it: words alone would take its buttons away.
    (multiple-value-bind (text dialog)
      (cond
        ((string= verb "") (values (voice-status-text section) (voice-dialog section room)))
        ((string= verb "hears") (voice-pick-model :hears object))
        ((string= verb "speaks") (voice-pick-model :speaks object))
        ((string= verb "status") (voice-status-text section))
        ((assoc verb +voice-replies+ :test #'string=)
         (nlk:with-handlers ((error (condition) (format nil "voice: ~a" condition)))
           (set-voice-replies room verb)))
        ((string= verb "replies")
         (if *command-source*
             (values (format nil "voice: voice messages here — ~(~a~)" (voice-replies-label room))
                     (voice-pick-dialog
                      "Voice messages here" "" "How this room's answers come back."
                      (loop with mode = (or (nck:room-voice-replies room) "off")
                            for (key . meaning) in +voice-replies+
                            collect (voice-row (cdr (assoc key +voice-reply-labels+ :test #'string=))
                                               meaning "use" (format nil "/voice ~a" key)
                                               :mark (if (equal key mode) "●" "") :mark-tone :accent))
                      ""))
             "voice: voice messages are a room's — pick them there, with `/voice replies`"))
        ((string= verb "join")
         ;; Beside the person who asked, wherever they sit in voice, or in the
         ;; channel named; talking through the channel it was typed in and
         ;; hearing them where no list names who is heard; answered once
         ;; Discord has seated the bot.
         (let ((channel (or object (and by (car (gethash by *voice-states*))))))
           (if (and (null channel) (null lane))
               (values "voice: sit in a voice channel first, or pick one" (voice-join-dialog))
               (bt2:with-lock-held (*voice-seat-lock*)
                 (multiple-value-bind (lane refusal)
                     (join-voice section :channel channel
                                         :speakers (voice-speakers section by))
                   (cond
                     ((or (null lane) refusal) (format nil "voice: ~a" refusal))
                     ((not (voice-await-seat lane)) (format nil "voice: ~a" (voice-unseated-text lane)))
                     (t (voice-announce lane (voice-joining-line lane) typed))))))))
        ((not (member verb '("say" "leave" "notes" "room" "hear") :test #'string=))
         (format nil "voice: no such verb ~s — join, leave, status, room, hear, replies, on, tts, off, ~
                      notes, hears, speaks, say <words>" verb))
        ((null lane) "voice: not in a voice channel")
        ((string= verb "room") (voice-room-command lane object section typed))
        ((string= verb "hear") (voice-hear-command lane object section by typed))
        ((string= verb "notes") (voice-notes-command lane object by typed))
        ((string= verb "say")
         ;; Say something now, with no turn behind it. The playback path is
         ;; otherwise only reachable by asking a question and waiting, which
         ;; makes a fault in it slow and expensive to see.
         (let ((words (string-trim '(#\Space #\Tab)
                                   (subseq (or argument "") (min (length (or argument "")) 4)))))
           (cond
             ((zerop (length words)) "voice: say what?")
             (t (nck:queue-push lane.work-queue (list :speak words))
                (format nil "voice: saying ~s out loud" words)))))
        (t (prog1 (voice-announce lane (format nil "voice: leaving ~a." (voice-channel-name lane.channel-id)) typed)
             (leave-voice))))
      (values text
              (or dialog
                  (and *command-source*
                       (or (member verb '("join" "leave" "notes" "hears" "speaks") :test #'string=)
                           (and object (member verb '("room" "hear") :test #'string=))
                           (assoc verb +voice-replies+ :test #'string=))
                       (voice-dialog section room)))))))

(defun voice-choices (text &aux (lane *voice-lane*))
  "The /voice argument completions for the tail TEXT: the verbs that would
do something now, each with what it does — join only while the bot sits
nowhere; leave, room, hear, notes and say only while it sits; notes stop
only while notes are taken."
  (loop with needle = (nlk:trimmed text)
        for (verb . what)
          in (append (if lane
                         (list '("leave" . "get up from the voice channel")
                               '("room" . "the text channel I talk through")
                               '("hear" . "whose speech opens a turn: everyone, or the room's speakers")
                               (if lane.notes-by
                                   '("notes stop" . "stop the notes and write them up")
                                   '("notes" . "write down what everyone says; answer nothing"))
                               '("say" . "speak the words after it, now"))
                         (list '("join" . "sit in the voice channel you are in, or one you pick")))
                     (list '("status" . "where the bot sits and what it hears")
                           '("hears" . "the model speech is heard with")
                           '("speaks" . "the voice answers are said in")
                           '("replies" . "how this room's answers come back: words or voice"))
                     +voice-replies+)
        when (search needle verb :test #'char-equal)
          collect (list :name (format nil "~a — ~a" verb what) :value verb)))

(defun register-voice-command (section)
  "Register /voice."
  ;; The section is closed over: the command reads the live configuration the
  ;; lane was started from, never a copy of it.
  (nle:register-command "nodecode-channel-discord" "voice"
                        (lambda (args session-id)
                          (voice-command args section session-id))
                        :description "Voice: join, leave, where it talks and whom it hears; answers as voice messages (on, tts, off); meeting notes"
                        :argument-hint "join [channel] | leave | status | room | hear | replies | on | tts | off | notes [stop] | hears [model] | speaks [voice] | say <words>"
                        :complete (lambda (text session-id)
                                    (declare (ignore session-id))
                                    (voice-choices text))))

;;; --- start and stop ----------------------------------------------------------------------------

(defun start-voice (section)
  "Wire voice into a started Discord lane: the /voice command, the frame
observer that speaks an answer, and the rules that seat the bot, which the
first GUILD_CREATE puts to work (ROUTE-VOICE-DISPATCH). => a thunk that
undoes all of it."
  (register-voice-command section)
  (nle:hook :frame +voice-hook-key+ (nlk:observer #'voice-observe-frame "discord-voice"))
  (setf *voice-section* section)
  (lambda ()
    (setf *voice-section* nil)
    (ignore-errors (leave-voice))
    (clrhash *voice-states*)
    (clrhash *voice-channel-guilds*)
    (clrhash *voice-channel-names*)
    (clrhash *voice-member-names*)
    (ignore-errors (nle:unhook :frame +voice-hook-key+))
    (ignore-errors (nle:unregister-commands "nodecode-channel-discord"))
    t))
