;;;; package.lisp --- NODECODE-CHANNEL-DISCORD package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Discord channel adapter: a pure gateway-websocket protocol reducer
;;;; (gateway.lisp), REST request plans (rest.lisp), and the platform plus
;;;; the gateway lap the kit's host runs over (adapter.lisp). The room/lane
;;;; topology, the gate and the digest delivery are the kit's; the kit's
;;;; thread-topology rule (no network I/O in wsd callbacks) binds every
;;;; callback in this package.

(defpackage #:nodecode-channel-discord
  (:documentation "Discord channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:ncd)
  (:export
   ;; adapter.lisp --- the channel entry (kit host convention), the
   ;; model-facing REST call, the dispatch seam a layer advises to
   ;; observe gateway events, and the platform
   #:start-channel #:request #:discord-handle-dispatch #:discord-platform
   #:discord-strip-mention #:discord-where-text
   ;; probe.lisp --- what the token can see; the section declaration
   ;; (nlk:define-section) beside it is read through the kernel
   #:probe-channel #:probe-channel-choices
   ;; voice.lisp --- the pure half of Discord voice, exported for tests:
   ;; the payloads, the RTP packet, the transport cipher, Ogg Opus, and
   ;; the rule that decides when somebody has finished speaking
   #:voice-identify-payload #:voice-select-protocol-payload
   #:voice-heartbeat-payload #:voice-speaking-payload #:voice-state-payload
   #:voice-transition-ready-payload #:voice-endpoint-parts #:voice-binary-parts
   #:voice-binary-frame #:transition-and-rest #:ip-discovery-packet
   #:ip-discovery-answer #:rtp-audio-p #:rtp-head-length #:rtp-extension-octets
   #:rtp-ssrc #:rtp-header #:transport-seal #:transport-open #:voice-packet
   #:voice-packet-frame #:ogg-opus-file #:ogg-packets #:opus-audio-frames
   #:opus-silence-p #:+opus-silence-frame+ #:make-utterance #:utterance-frames
   #:utterance-count #:utterance-user-id #:utterance-milliseconds
   #:utterance-note-frame #:utterance-complete-p #:utterance-worth-hearing-p
   #:utterance-recording #:+voice-dave-version+ #:+voice-transport-mode+
   ;; dave.lisp --- libdave, fetched and bound
   #:install-dave #:load-dave #:dave-installed-p #:dave-library-path
   #:*dave-directory*
   ;; voicelap.lisp --- the live voice lane
   #:join-voice #:leave-voice #:start-voice #:voice-command #:voice-await-seat
   #:voice-speakers #:voice-room-channel
   #:voice-status-text #:voice-candidate #:voice-speaker-allowed-p
   #:voice-note-frame #:voice-close-finished-utterances #:route-voice-dispatch
   #:*voice-lane*
   ;; gateway.lisp --- pure reducer, exported for table-driven tests
   #:initial-gateway-session #:gateway-url-with-params #:heartbeat-tick
   #:reduce-gateway-payload #:reduce-gateway-close
   ;; ingress normalization
   #:route-discord-message #:route-discord-message-update
   #:route-discord-interaction #:route-discord-control
   #:route-discord-autocomplete #:route-discord-reaction
   #:interaction-autocomplete-plans #:discord-inbound-policy
   ;; what addresses the bot: its user id, and the roles it itself holds
   #:discord-self-role-ids #:note-self-roles #:*self-role-ids*
   ;; rest.lisp
   #:delete-message-plan #:discord-message-plan #:discord-file-message-plan
   #:edit-message-plan #:typing-plan #:reaction-plans #:read-back-path
   #:commands-plan #:discord-command-name-p #:interaction-response-plan
   #:interaction-ack-plans #:hydrate-self-roles #:thread-close-plan
   ;; board.lisp --- the thread board as Discord draws it (its semantics are
   ;; the kit's NCK:BOARD-ROW), exported for tests
   #:board-components #:board-list-line #:board-show-text #:board-pressed #:start-board #:*board*
   ;; a turn's card: its components, what its message holds of its
   ;; pictures, and the application emojis its marks wear
   #:card-components #:message-media #:*card-marks* #:application-emojis-plan
   #:create-application-emoji-plan
   #:emoji-spelling #:ensure-card-marks))
