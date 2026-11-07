;;;; package.lisp --- NODECODE-CHANNEL-KIT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Shared machinery for channel adapters (Discord, Telegram, ...). Adapters
;;;; are cells loaded INTO the gateway image and reach the organism through
;;;; its exported in-process seams only: (NLE:HOOK :FRAME ...) to observe
;;;; every live frame, NLE:SUBMIT to put a prompt into a session,
;;;; NLE:EVICT-SESSION-CONTEXT to hold a room to its budget, and the NLK
;;;; session API to make sessions durable. No room cell
;;;; reaches an nle:: internal, and nothing dials the gateway it runs inside.
;;;;
;;;; THREAD TOPOLOGY RULE: no network I/O on a thread that is not ours. A
;;;; :FRAME hook runs on the publishing thread (the turn worker) and a
;;;; platform wsd callback runs on that socket's reader, so both do parse +
;;;; classify + enqueue ONLY; all platform REST — typing indicators included
;;;; — runs on a per-adapter delivery worker thread behind a bounded queue.

(defpackage #:nodecode-channel-kit
  ;; The typed config accessors
  ;; are the core waist's (src/waist/config.lisp) — imported and re-exported so
  ;; adapters keep reading them off this package; a refused read signals
  ;; NLK:CONFIG-REFUSAL.
  (:documentation
   "Shared channel cell machinery: inbound admission, outbound
plans/executors, bounded delivery worker, supervision, the in-process
organism seams, the SOUL.md persona section every adapter applies before
it submits, the room/lane session topology and the channel host that runs
it over any platform, and the /channels status surface.")
  (:use #:cl)
  (:nicknames #:nck)
  (:import-from #:nodecode.kernel
                #:config-error
                #:config-string
                #:config-integer
                #:config-boolean
                #:config-enum
                #:config-string-list
                #:json-plist
                #:plist-json
                #:frame-fact
                #:fact-message-content)
  (:export
   ;; A record declared with NLK:DEFINE-RECORD exports its names there, not here.
   ;; cell.lisp --- the generic cell entry, the folder's presence, the
   ;; probe that turns names into ids
   #:start-cell #:present-adapters #:*unconfigured* #:probe
   ;; src/waist/config.lisp --- re-exported accessors
   #:config-error #:config-string #:config-integer #:config-boolean
   #:config-enum #:config-string-list #:json-plist #:plist-json
   ;; config.lisp
   #:resolve-channel-secret #:require-non-empty-allowlist
   ;; soul.lisp --- SOUL.md as the session's standing persona section
   #:+soul-section+ #:*soul-default-path* #:soul-path #:read-soul #:apply-soul
   ;; admission.lisp --- the gate, and the one mention rule it shares with
   ;; the adapters that ask whether an edit involves the bot
   #:decide-inbound #:set-other-addressees #:mention-involves-p
   #:candidate-addressed-p #:candidate-pressed-p #:candidate-joined-p #:discord-mention-p #:telegram-mention-p
   #:source-field #:handle-tag #:+channel-seams-primer+
   ;; outbound.lisp
   #:split-text-chunks #:execute-plan #:make-dexador-executor #:multipart-body-p
   #:make-recording-executor #:make-scripted-response #:execute-delivery
   #:probe-failure #:redact-text #:call #:rest-plan
   ;; delivery.lisp
   #:make-work-queue #:queue-push #:queue-pop #:queue-depth
   #:start-delivery-worker #:stop-delivery-worker #:delivery-worker-enqueue
   #:typing-note-started #:typing-note-stopped #:typing-due-p
   #:typing-note-sent #:make-lane-table #:intern-lane #:find-lane
   #:bind-lane-address #:lane-for-address #:remove-lane #:lane-count #:map-lanes
   #:lane-origin #:lane-thread-origin #:lane-reference
   #:forget-thread-origin #:now-ms
   ;; digest.lisp
   #:+digest-thinking-cap+ #:lane-turn-digest #:lane-open-digest
   #:digest-note-tool-call #:digest-note-thinking #:digest-note-round
   #:digest-note-terminal #:digest-note-queued #:digest-note-admitted
   #:digest-note-pending #:digest-note-visible #:digest-note-call-start
   #:digest-note-call-result #:digest-note-usage
   #:digest-note-said #:digest-note-writing #:digest-note-written
   #:digest-card #:card-text #:step-mark #:digest-details #:details-text #:thought-headline
   #:digest-note-line #:digest-note-lines #:+digest-note-cap+
   #:digest-status-plan #:digest-status-attempted #:digest-status-refused
   #:digest-status-delivered #:digest-final-text
   #:recorded-answer-text
   ;; room.lisp --- the session topology every channel shares
   #:+room-ambient-lines+ #:+lane-idle-reap-ms+ #:room-session-id #:lane-id
   #:channel-target #:session-target #:speaker-line #:note-ambient
   #:drain-ambient #:lane-contract #:write-back #:candidate-attachments
   #:claim-slot #:release-slot #:reap-lanes
   #:resolve-owners #:remove-all
   ;; fetch.lisp --- an archive too big to ship, pinned and fetched once
   #:install-pinned-archive #:archive-installed-p #:call-with-scratch-directory
   #:write-octets #:read-octets #:ffmpeg-present-p #:machine-build
   ;; speech.lisp --- an answer said out loud
   #:*speech* #:*speaker-directory* #:synthesize-speech #:speech-enabled-p
   #:install-speaker #:local-speaker #:spoken-text #:speakable-text
   #:+voice-replies+ #:wav-shape #:wav-to-ogg-opus #:voice-message
   ;; transcribe.lisp --- a recording read as the words it says
   #:*transcription* #:*transcriber-directory* #:audio-container
   #:transcribe-audio #:local-transcriber #:install-transcriber
   ;; host.lisp --- the lane interpreter over a platform
   #:platform-plan-control-ack #:platform-plan-autocomplete
   #:platform-plan-thread #:platform-plan-delete-thread #:platform-plan-close-thread #:platform-closed-note
   #:platform-tables
   #:platform-thread-id-of
   #:platform-thread-link #:platform-plan-file #:+reaction-seen+
   #:+reaction-done+ #:+reaction-failed+
   #:control-pressed #:completion-requested #:reaction-noticed
   #:make-host-from-section #:section-thread-behavior
   #:thread-title #:thread-ask-p #:message-target #:host-id
   #:post-message #:post-file #:answer-file #:answer-choices #:answer-waits #:answer-waiting #:choice #:menu-choice
   #:said-line #:press-token #:press-action #:candidate-press #:fetch-image #:schedule-flush #:*command-source* #:set-voice-replies
   #:home-target #:offer-card #:room-voice-replies #:room-lane-model
   #:flush-now #:deliver-answer #:on-delta #:on-fact-with-turn #:operator-p
   #:candidate-operator-p #:observe-candidate #:build-ask #:continue-lane
   #:intern-ask-lane #:sync-commands #:handle-candidate #:mark-joined-thread
   #:candidate-taken-p #:candidate-source
   #:start-host #:stop-host
   ;; supervise.lisp
   #:start-supervised
   ;; organism.lisp
   #:ensure-session #:frame-session-id #:frame-fact #:frame-delta
   #:fact-message-content #:fact-message-reasoning #:fact-message-tool-calls
   ;; status.lisp
   #:set-channel-status #:channel-status #:clear-channel-status
   #:clear-all-channel-status #:channels-status-report
   #:register-channel-commands
   ;; host.lisp --- a host and its transport lap, started and stopped as one
   #:run-host
   ;; board.lisp --- what a thread board says; the platform reads and draws
   #:+board-sections+ #:+board-rows+ #:+board-done-close-ms+ #:+board-quiet-close-ms+
   #:board-state #:board-close-p #:board-age #:board-when #:board-question #:board-row
   #:board-section-rows #:board-lane-phases #:board-list-text
   #:register-board #:unregister-board #:board-command))
