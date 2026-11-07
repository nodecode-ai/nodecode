;;;; voice.lisp --- Discord voice, everything of it that is pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The twin of gateway.lisp one layer down: the voice gateway's payloads,
;;;; the RTP packet, the transport cipher, the Ogg container an utterance is
;;;; handed to the transcriber in, and the rule that says when someone has
;;;; stopped talking. No I/O, no threads, no libdave — voicelap.lisp owns the
;;;; socket, the session and the workers, and dave.lisp owns the library.
;;;;
;;;;   - The voice endpoint's PORT is load-bearing. VOICE_SERVER_UPDATE
;;;;     answers a voice endpoint and the port varies by server (443,
;;;;     2087, 8443). Dialing the host on 443 regardless — which is what
;;;;     every older client did — is answered with close 4006, "Session is
;;;;     no longer valid", which reads like a stale session and is not one.
;;;;   - max_dave_protocol_version 0 is closed with 4017. There is no
;;;;     presence-only mode left to fall back to.
;;;;   - Binary frames are [seq uint16 BE][opcode uint8][payload] coming in
;;;;     and [opcode uint8][payload] going out.
;;;;   - Discord forms no MLS group while the bot is alone in the channel.
;;;;     No second member, no proposals, no ratchet, and so no media in
;;;;     either direction. That is normal and not a fault.
;;;;   - Under an -rtpsize mode the unencrypted head is 12 octets plus four
;;;;     per CSRC plus four more for the extension preamble when the
;;;;     extension bit is set; that head is the AAD. The 32-bit nonce is the
;;;;     packet's last four octets and the GCM IV is those four big-endian
;;;;     followed by eight zeros. The extension BODY is inside the
;;;;     ciphertext, so it is skipped after decryption, not before.

(in-package #:nodecode-channel-discord)

;;; --- the voice gateway's opcodes --------------------------------------------------

(defparameter +voice-op-identify+ 0)
(defparameter +voice-op-select-protocol+ 1)
(defparameter +voice-op-ready+ 2)
(defparameter +voice-op-heartbeat+ 3)
(defparameter +voice-op-session-description+ 4)
(defparameter +voice-op-speaking+ 5)
(defparameter +voice-op-hello+ 8)
(defparameter +voice-op-clients-connect+ 11)
(defparameter +voice-op-client-disconnect+ 13)
(defparameter +voice-op-dave-prepare-transition+ 21)
(defparameter +voice-op-dave-execute-transition+ 22)
(defparameter +voice-op-dave-transition-ready+ 23)
(defparameter +voice-op-dave-prepare-epoch+ 24)
(defparameter +voice-op-dave-external-sender+ 25)
(defparameter +voice-op-dave-key-package+ 26)
(defparameter +voice-op-dave-proposals+ 27)
(defparameter +voice-op-dave-commit-welcome+ 28)
(defparameter +voice-op-dave-announce-commit+ 29)
(defparameter +voice-op-dave-welcome+ 30)

;;; 0 is refused with close code 4017 and is not an option.
(defparameter +voice-dave-version+ 1
  "The DAVE protocol version this adapter identifies with.")

;;; Discord offers this and aead_xchacha20_poly1305_rtpsize; AES-256-GCM is
;;; the one ironclad carries as an AEAD, so it is the one asked for.
(defparameter +voice-transport-mode+ "aead_aes256_gcm_rtpsize"
  "The transport cipher asked for at Select Protocol.")

(define-payload voice-identify-payload (guild-id user-id session-id token) +voice-op-identify+
  "server_id" guild-id
  "user_id" user-id
  "session_id" session-id
  "token" token
  "max_dave_protocol_version" +voice-dave-version+)

(define-payload voice-select-protocol-payload (address port) +voice-op-select-protocol+
  "protocol" "udp"
  "data" (nlk:json-object "address" address
                          "port" port
                          "mode" +voice-transport-mode+))

(define-payload voice-heartbeat-payload (sequence) +voice-op-heartbeat+
  "t" (get-universal-time)
  "seq_ack" (or sequence -1))

(define-payload voice-speaking-payload (ssrc speaking) +voice-op-speaking+
  "speaking" (if speaking 1 0)
  "delay" 0 "ssrc" ssrc)

(define-payload voice-transition-ready-payload (transition-id) +voice-op-dave-transition-ready+
  "transition_id" transition-id)

(defun voice-state-payload (guild-id channel-id)
  "The main gateway's op 4. A NIL CHANNEL-ID leaves the voice channel — the
one way out, and the one that clears the voice state Discord keeps."
  (nlk:json-object "op" 4
                   "d" (nlk:json-object "guild_id" guild-id
                                        "channel_id" (or channel-id :null)
                                        "self_mute" nil
                                        "self_deaf" nil)))

(defun voice-endpoint-parts (endpoint)
  "(values HOST PORT) of a VOICE_SERVER_UPDATE endpoint."
  ;; The port is not optional — see this file's header.
  (nlk:if-let (colon (position #\: endpoint :from-end t))
    (values (subseq endpoint 0 colon)
            (or (ignore-errors (parse-integer (subseq endpoint (1+ colon)))) 443))
    (values endpoint 443)))

;;; --- binary frames ---------------------------------------------------------------

(defun voice-binary-parts (payload)
  "(values SEQUENCE OPCODE BODY) of a gateway-to-client DAVE frame."
  (when (>= (length payload) 3)
    (values (+ (* 256 (aref payload 0)) (aref payload 1))
            (aref payload 2)
            (subseq payload 3))))

(defun voice-binary-frame (opcode body)
  "A client-to-gateway DAVE frame: the opcode octet, then BODY."
  (concatenate '(vector (unsigned-byte 8)) (vector opcode) body))

(defun transition-and-rest (body)
  "(values TRANSITION-ID REST) of a welcome or announce-commit body."
  (values (+ (* 256 (aref body 0)) (aref body 1)) (subseq body 2)))

;;; --- big-endian helpers ------------------------------------------------------------

(defun be32-octets (value) (ironclad:integer-to-octets value :n-bits 32))

;;; --- IP discovery ------------------------------------------------------------------

(defparameter +ip-discovery-length+ 74)

(defun ip-discovery-packet (ssrc)
  "The type 0x1 request: two octets of type, two of length, the ssrc, then
room for the address and port the server writes back."
  (let ((packet (make-array +ip-discovery-length+ :element-type '(unsigned-byte 8)
                                                  :initial-element 0)))
    (setf (aref packet 1) 1 (aref packet 3) 70 (ironclad:ub32ref/be packet 4) ssrc)
    packet))

(defun ip-discovery-answer (packet size)
  "(values ADDRESS PORT) out of a type 0x2 answer, or NIL when it is not one."
  (when (and (>= size +ip-discovery-length+) (= (aref packet 1) 2))
    (let ((end (or (position 0 packet :start 8 :end 72) 72)))
      (values (coerce (sb-ext:octets-to-string (subseq packet 8 end)
                                               :external-format :latin-1)
                      'simple-string)
              (+ (* 256 (aref packet 72)) (aref packet 73))))))

;;; --- the RTP packet ------------------------------------------------------------------

(defparameter +rtp-version-flag+ #x80)
(defparameter +rtp-payload-type+ #x78)
(defparameter +opus-frame-samples+ 960
  "48 kHz for 20 ms: the frame size Discord speaks in.")

(defun rtp-audio-p (packet size)
  "Whether PACKET looks like an Opus RTP packet rather than RTCP or a
keepalive. Anything else is dropped without a word."
  (and (> size 16)
       (= (logand (aref packet 0) #xc0) +rtp-version-flag+)
       (= (aref packet 1) +rtp-payload-type+)))

(defun rtp-head-length (packet)
  "The unencrypted head under an -rtpsize mode, which is also the AAD."
  (+ 12
     (* 4 (logand (aref packet 0) #x0f))
     (if (plusp (logand (aref packet 0) #x10)) 4 0)))

(defun rtp-extension-octets (packet)
  "How many octets of extension body ride inside the ciphertext."
  ;; The preamble's second half word, after the head and its CSRCs, counts words.
  (if (plusp (logand (aref packet 0) #x10))
      (* 4 (ironclad:ub16ref/be packet (+ 14 (* 4 (logand (aref packet 0) #x0f)))))
      0))

(defun rtp-ssrc (packet) (ironclad:ub32ref/be packet 8))

(defun rtp-header (sequence timestamp ssrc)
  (let ((header (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref header 0) +rtp-version-flag+
          (aref header 1) +rtp-payload-type+
          (ironclad:ub16ref/be header 2) sequence
          (ironclad:ub32ref/be header 4) timestamp
          (ironclad:ub32ref/be header 8) ssrc)
    header))

;;; --- the transport cipher --------------------------------------------------------------

(defun transport-cipher (key counter-octets aad)
  "AES-256-GCM under KEY, AAD already taken in, its 12-octet IV the four
big-endian nonce octets COUNTER-OCTETS, then eight zero."
  (let ((cipher (ironclad:make-authenticated-encryption-mode
                 :gcm :cipher-name :aes :key key
                 :initialization-vector (concatenate '(vector (unsigned-byte 8)) counter-octets
                                                     (make-array 8 :initial-element 0)))))
    (ironclad:process-associated-data cipher aad)
    cipher))

(defun transport-seal (key counter aad plaintext
                       &aux (cipher (transport-cipher key (be32-octets counter) aad)))
  "PLAINTEXT under AES-256-GCM: ciphertext then its 16-octet tag."
  (concatenate '(vector (unsigned-byte 8))
               (ironclad:encrypt-message cipher plaintext) (ironclad:produce-tag cipher)))

(defun transport-open (key counter-octets aad sealed)
  "The plaintext under SEALED, or NIL when the tag does not verify."
  (nlk:with-handlers ((error () nil))
    (let* ((size (length sealed))
           (cipher (transport-cipher key counter-octets aad))
           (plain (ironclad:decrypt-message cipher (subseq sealed 0 (- size 16)))))
      (and (equalp (ironclad:produce-tag cipher) (subseq sealed (- size 16))) plain))))

(defun voice-packet (key counter sequence timestamp ssrc frame)
  "One wire packet: the RTP header, FRAME sealed under it, and the nonce."
  (let ((header (rtp-header sequence timestamp ssrc)))
    (concatenate '(vector (unsigned-byte 8))
                 header
                 (transport-seal key counter header frame)
                 (be32-octets counter))))

(defun voice-packet-frame (key packet size)
  "The media frame inside PACKET — transport-opened, extension body stepped
over — or NIL when it is not ours to read."
  ;; Still DAVE-encrypted: opening that is libdave's, and needs the sender's
  ;; ratchet.
  (when (rtp-audio-p packet size)
    (let* ((head (rtp-head-length packet))
           (aad (subseq packet 0 head))
           (sealed (subseq packet head (- size 4)))
           (counter (subseq packet (- size 4) size))
           (plain (transport-open key counter aad sealed)))
      (when plain
        (let ((skip (rtp-extension-octets packet)))
          (if (<= skip (length plain)) (subseq plain skip) plain))))))

;;; --- Ogg Opus --------------------------------------------------------------------------
;;;
;;; An utterance reaches the transcriber as a file, and the transcriber
;;; sniffs the container. A container is framing, not a codec: wrapping the
;;; Opus frames Discord already sent into Ogg pages needs no Opus
;;; implementation, and so this adapter carries no codec at all.

(defparameter +ogg-crc-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (index 256 table)
      (let ((remainder (ash index 24)))
        (dotimes (bit 8)
          (setf remainder
                (if (logbitp 31 remainder)
                    (logand #xffffffff (logxor (ash remainder 1) #x04c11db7))
                    (logand #xffffffff (ash remainder 1)))))
        (setf (aref table index) remainder))))
  "Ogg's CRC-32: the plain polynomial, no reflection and no final xor.")

(defun ogg-crc (octets &aux (crc 0))
  (loop for octet across octets
        do (setf crc (logand #xffffffff
                             (logxor (ash crc 8)
                                     (aref +ogg-crc-table+
                                           (logxor (logand (ash crc -24) #xff) octet))))))
  crc)

(defun le-octets (value count &aux (octets (make-array count :element-type '(unsigned-byte 8))))
  (dotimes (index count octets)
    (setf (aref octets index) (ldb (byte 8 (* 8 index)) value))))

(defun ogg-page (serial sequence granule packets flags)
  ;; Lacing: a packet is as many 255s as fit, then what is left, 0 included.
  (let* ((segments (loop for packet in packets
                         for size = (length packet)
                         append (make-list (floor size 255) :initial-element 255)
                         collect (mod size 255)))
         (body (apply #'concatenate '(vector (unsigned-byte 8)) packets))
         (head (concatenate '(vector (unsigned-byte 8))
                            (sb-ext:string-to-octets "OggS")
                            (vector 0 flags)
                            (le-octets granule 8)
                            (le-octets serial 4)
                            (le-octets sequence 4)
                            (vector 0 0 0 0)
                            (vector (length segments))
                            (coerce segments '(vector (unsigned-byte 8)))))
         (crc (ogg-crc (concatenate '(vector (unsigned-byte 8)) head body))))
    (concatenate '(vector (unsigned-byte 8))
                 (subseq head 0 22) (le-octets crc 4) (subseq head 26) body)))

(defun opus-head (channels)
  (concatenate '(vector (unsigned-byte 8))
               (sb-ext:string-to-octets "OpusHead")
               (vector 1 channels)
               (le-octets 312 2)            ; pre-skip
               (le-octets 48000 4)
               (vector 0 0)                 ; output gain
               (vector 0)))                 ; mapping family

(defun opus-tags ()
  (concatenate '(vector (unsigned-byte 8))
               (sb-ext:string-to-octets "OpusTags")
               (le-octets 8 4) (sb-ext:string-to-octets "nodecode")
               (le-octets 0 4)))

(defparameter +ogg-packets-per-page+ 20)

(defun ogg-opus-file (frames &key (channels 2) &aux (serial #x4e4f4445)
                                                    (granule 0))
  "FRAMES, a list of Opus packets in the order they were spoken, as the
octets of an Ogg Opus file."
  (apply #'concatenate '(vector (unsigned-byte 8))
         (ogg-page serial 0 0 (list (opus-head channels)) #x02)
         (ogg-page serial 1 0 (list (opus-tags)) 0)
         (loop for rest on frames by (lambda (list) (nthcdr +ogg-packets-per-page+ list))
               for batch = (subseq rest 0 (min +ogg-packets-per-page+ (length rest)))
               for sequence from 2
               do (incf granule (* +opus-frame-samples+ (length batch)))
               collect (ogg-page serial sequence granule batch
                                 (if (nthcdr +ogg-packets-per-page+ rest) 0 #x04)))))

(defun ogg-packets (octets &aux (packets '()) (offset 0))
  "Every packet in an Ogg stream, in order."
  ;; The inverse of OGG-OPUS-FILE, and how a WAV turned into Ogg Opus by
  ;; ffmpeg becomes frames to play.
  (loop while (and (<= (+ offset 27) (length octets))
                   (string= "OggS" (sb-ext:octets-to-string
                                    (subseq octets offset (+ offset 4))
                                    :external-format :latin-1)))
        do (let* ((count (aref octets (+ offset 26)))
                  (table (subseq octets (+ offset 27) (+ offset 27 count)))
                  (body (+ offset 27 count))
                  (cursor body)
                  (run 0))
             (loop for segment across table
                   do (incf run segment)
                      (when (< segment 255)
                        (push (subseq octets cursor (+ cursor run)) packets)
                        (incf cursor run)
                        (setf run 0)))
             (setf offset (+ body (reduce #'+ table)))))
  (nreverse packets))

(defun opus-audio-frames (octets)
  "The Opus frames of an Ogg Opus file, its two header packets dropped."
  (remove-if (lambda (packet)
               (and (>= (length packet) 8)
                    (member (sb-ext:octets-to-string (subseq packet 0 8)
                                                     :external-format :latin-1)
                            '("OpusHead" "OpusTags") :test #'string=)))
             (ogg-packets octets)))

;;; --- when somebody has stopped talking ---------------------------------------------------

;;; Short enough that a sentence does not wait, long enough to sit through the
;;; pause in the middle of a thought.
(defparameter +utterance-silence-ms+ 900
  "How long a speaker must be quiet before what they said is one ask.")

(defparameter +utterance-minimum-ms+ 400
  "Shorter than this is a cough, a door, a keyboard: no ask is opened.")

(defparameter +barge-in-ms+ 400
  "How long somebody must keep talking before they take the floor back. A
cough, a keyboard, a breath or a door is a frame or two and must not cut an
answer in half; a person who wants the floor keeps going past this.")

;;; Past it the words so far are one ask and the rest begins another: a
;;; monologue must not grow without bound in memory and must not wait forever
;;; to be answered.
(defparameter +utterance-maximum-ms+ 60000
  "The longest single utterance.")

;;; DAVE lets it through unencrypted by design, so it arrives as itself.
(defparameter +opus-silence-frame+
  (coerce #(#xF8 #xFF #xFE) '(vector (unsigned-byte 8)))
  "The frame a Discord client sends to say it has stopped.")

(defun opus-silence-p (frame)
  (equalp frame +opus-silence-frame+))

(nlk:define-record (utterance (:copier nil) (:export :constructor :readers))
  "What one speaker has said since they last fell quiet."
  ;; FRAMES is newest first — the list is reversed once, when the utterance is
  ;; closed.
  (user-id "" :type string)
  (frames '() :type list)
  (count 0 :type integer)
  (last-ms 0 :type integer))

(defun utterance-milliseconds (utterance)
  (* 20 utterance.count))

(defun utterance-note-frame (utterance frame now-ms)
  "FRAME added to UTTERANCE."
  ;; A silence frame carries no words and is not kept; it only moves nothing,
  ;; since the quiet is measured from the last frame that did carry words.
  (unless (opus-silence-p frame)
    (push frame utterance.frames)
    (incf utterance.count)
    (setf utterance.last-ms now-ms))
  utterance)

(defun utterance-complete-p (utterance now-ms)
  "Whether UTTERANCE is a finished thought: quiet long enough, or so long
that it must be cut."
  (and (plusp utterance.count)
       (or (>= (- now-ms utterance.last-ms) +utterance-silence-ms+)
           (>= (utterance-milliseconds utterance) +utterance-maximum-ms+))))

(defun utterance-stale-p (utterance now-ms)
  "Whether UTTERANCE has sat open without one word in it long enough to be
no utterance at all — the shape a lane that stopped decrypting leaves behind.
A speaker's entry is made by their first packet, so a group that went stale
held one for every mouth in the channel and closed none of them."
  (and (zerop utterance.count)
       (>= (- now-ms utterance.last-ms) +utterance-silence-ms+)))

(defun utterance-worth-hearing-p (utterance)
  (>= (utterance-milliseconds utterance) +utterance-minimum-ms+))

(defun utterance-recording (utterance)
  "UTTERANCE as Ogg Opus octets, ready for the transcriber."
  (ogg-opus-file (reverse utterance.frames)))
