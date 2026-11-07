;;;; host.lisp --- the channel host: one lane interpreter, any platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A channel adapter is two things: a transport that turns platform events
;;;; into candidates (a Discord gateway websocket, a Telegram long poll) and
;;;; a PLATFORM — how a message, an edit, a delete and a typing beat are
;;;; spelled as request plans, how long a message may be, how a mention is
;;;; written, how a session id is prefixed. Everything between the two is
;;;; the same for every channel and lives here: admission into a room and a
;;;; lane, the concurrency gate, the per-turn digest folded off the :FRAME
;;;; point, the status line edited in place, the answer posted once as a
;;;; reply that pings its asker, the chrome retired, the exchange written
;;;; back, idle lanes reaped.
;;;;
;;;; Thread topology (the kit rule: no network I/O on a thread that is not
;;;; ours):
;;;;   - the adapter's transport thread: HANDLE-CANDIDATE — admission, room
;;;;     ensure, lane fork, NLE:SUBMIT. Owned by the adapter's supervised lap.
;;;;   - the organism's publishing threads, through one (NLE:HOOK :FRAME)
;;;;     chain: OBSERVE-FRAME folds facts and reasoning deltas into the
;;;;     lane's digest (lane lock, no I/O) and enqueues terminal jobs. The
;;;;     hook sees every session's frames; the lane table is the filter.
;;;;   - channel-<id>-deliver: a POOL of threads running every platform
;;;;     call — the live status line, the turn's answer, chrome deletion,
;;;;     typing beats, notices. A pool because lanes outnumber drainers: one
;;;;     call parked on its request timeout must not stall every other
;;;;     lane's chrome behind it. The first thread also owns the tick, which
;;;;     plans and enqueues but never calls out itself.
;;;;
;;;; UX SHAPE (room.lisp holds the session topology):
;;;;   - one answer per ask. A CARD appears only when the ask is queued,
;;;;     input is parked behind its turn, or the turn calls a tool or runs
;;;;     long enough to earn it; it posts as a SILENT reply to the ask, edits
;;;;     itself in place, carries the parked input as ⌎ rows, and once the
;;;;     answer lands settles above it as the turn's record (digest.lisp).
;;;;   - the answer is a reply to the ask and is the only message that ever
;;;;     pings; chrome never does.
;;;;   - opt-in (channels.<id>.reactions): the ask itself carries a reaction
;;;;     — seen when admitted, working while its turn runs, none once the
;;;;     answer has replaced both.
;;;;   - a reply to anything a lane produced continues that lane — steering
;;;;     it while it runs, opening its next turn once it has settled. Where
;;;;     the platform has threads, an ask typed in a plain channel opens one
;;;;     and runs inside it, answering there alone (THREADS below); a thread
;;;;     the bot takes part in and a DM are one conversation each, where every
;;;;     line talks to the surface's lane (CONVERSATION-LANE); everywhere else
;;;;     the reply chain is the branch.
;;;;   - a message that is a slash line (`/help', `/models grok-4.6') is a
;;;;     COMMAND, never a prompt: it runs through the organism's headless
;;;;     NLE:SLASH against the room on a delivery thread and answers as one
;;;;     reply, no lane, no turn, no chrome. The catalog those commands come
;;;;     from (NLE:SLASH-CATALOG) is published to the platform as its
;;;;     command menu — Telegram's setMyCommands, Discord's application
;;;;     commands — and re-published whenever the catalog changes shape,
;;;;     so a cell registering /memory after the channel started still
;;;;     reaches the menu. With operators declared, commands other than
;;;;     /help are theirs alone: they act on the shared room and on the
;;;;     organism's own configuration.

(in-package #:nodecode-channel-kit)

(nlk:access (chunk text-chunk) (existing channel-lane) (newest channel-lane) (other channel-lane)
            (table lane-table) (target-digest turn-digest))

;;; --- the platform ---------------------------------------------------------------

(nlk:define-record (platform (:copier nil) (:export :constructor id session-prefix text-limit
                                                    plan-reaction plan-commands plan-respond))
  "What one platform contributes to the host."
  ;; The four plan builders take the host's request timeout as
  ;; :TIMEOUT-SECONDS; the closures capture the adapter's own facts (its bot
  ;; identity, its api base) at start.
  (id (error "platform id required") :type string)      ; "discord"
  (name "" :type string)                                  ; "Discord"
  ;; The word the contract uses for the shared surface: room, chat.
  (noun "room" :type string)
  ;; What an operator id is called in the contract: "Discord user id".
  (owner-label "user id" :type string)
  ;; Session ids read <prefix>-<channel>[-t<thread>][-m<message>].
  (session-prefix "" :type string)
  ;; The harness section key the lane contract rides under.
  (contract-section "channel-room" :type string)
  (text-limit 2000 :type integer)
  ;; How often the typing indicator must be re-asserted to stay on.
  (typing-refresh-ms +typing-refresh-ms+ :type integer)
  ;; (target chunk &key reply-to ping controls files mentions card panel media
  ;;  timeout-seconds) => request-plan. CARD, a turn's card (DIGEST-CARD),
  ;; is the message where the platform draws one — Discord's components,
  ;; CONTROLS inside it and its pictures uploaded — and CHUNK its CARD-TEXT
  ;; where it does not; PANEL, a command's card (OFFER-CARD), likewise, CHUNK
  ;; its words. MEDIA is what the card's message holds of its pictures
  ;; already (MEDIA-OF).
  (plan-message (error "plan-message required") :type function)
  ;; (target message-id text &key retry controls card media timeout-seconds)
  ;; => request-plan; CARD and MEDIA as PLAN-MESSAGE's. A card's message is
  ;; edited as a whole card, its CONTROLS restated; without a CARD the
  ;; message is TEXT.
  (plan-edit (error "plan-edit required") :type function)
  ;; (target message-id &key timeout-seconds) => request-plan
  (plan-delete (error "plan-delete required") :type function)
  ;; (target &key timeout-seconds) => request-plan
  (plan-typing (error "plan-typing required") :type function)
  ;; (target message-id emoji &key previous timeout-seconds) => the request
  ;; plans, in order, that leave EMOJI as the bot's one reaction on the
  ;; message — NIL clears it; PREVIOUS is the emoji there now, for a
  ;; platform that removes by name. NIL when the platform has no reactions:
  ;; the host then never plans one.
  (plan-reaction nil :type (or null function))
  ;; (body) => the id of the message a plan-message execution created
  (message-id-of (error "message-id-of required") :type function)
  ;; (body) => what the message a card post or edit landed as holds of the
  ;; card's pictures, in the platform's own words — (NAME . ID) per file on
  ;; Discord — which the card's next edit keeps them by (MEDIA); NIL for a
  ;; platform whose card carries none.
  (media-of (constantly nil) :type function)
  ;; (target message-id) => the address-book key for one platform message.
  ;; Discord's ids are global; Telegram's are per chat, so its key carries
  ;; the chat.
  (address-of (lambda (target message-id)
                (declare (ignore target))
                message-id)
   :type function)
  ;; (text) => text with our own mention removed, or NIL for none.
  (strip-mention nil :type (or null function))
  ;; (text footer) => the answer's message body with its provenance line.
  (answer-body (lambda (text footer) (format nil "~a~%~%~a" text footer))
   :type function)
  ;; (candidate) => one sentence on where the room is and who the bot is.
  (where-text (constantly "unknown")
   :type function)
  ;; (entries &key timeout-seconds) => the request plans that publish
  ;; ENTRIES — (:name :description :usage) plists, NLE:SLASH-CATALOG's shape
  ;; — as the platform's command menu, or NIL while the platform cannot yet
  ;; (Discord before READY names its application). NIL when the platform
  ;; has no menu: the host then publishes nothing.
  (plan-commands nil :type (or null function))
  ;; (candidate text &key private controls panel timeout-seconds) => the plan that
  ;; answers CANDIDATE through its own return path — a Discord interaction —
  ;; or NIL to reply in the room. TEXT NIL asks for the plan that HOLDS the
  ;; path open while the answer is worked out (Discord's deferred response:
  ;; three seconds become fifteen minutes), PRIVATE for an answer only the
  ;; person who asked sees; TEXT then answers through the held path, CONTROLS
  ;; on it, drawn as PANEL (OFFER-CARD) where the platform draws one. A candidate a press made (source.pressed) is answered, unless
  ;; PRIVATE, by editing the message pressed. NIL when the platform has no
  ;; such path: every answer is a reply.
  (plan-respond nil :type (or null function))
  ;; Whether a message can carry choices (CHOICE): buttons and menus whose
  ;; press comes back as a candidate saying the choice's line, through the
  ;; adapter's own route (NCK:SAID-LINE). NIL: the kit hangs none, and a
  ;; card is its words alone.
  (choices nil :type boolean)
  ;; The platform's API primer and its own advisable seams, for the contract.
  (api-primer "" :type string)
  (seams-primer "" :type string)
  ;; The capabilities a platform may carry, each answering NIL unless the
  ;; adapter passes its own: a platform that has none of them — a DM, a chat
  ;; whose bot cannot open a thread or carry a file — keeps the surface it
  ;; always had.
  ;;
  ;; (payload &key text timeout-seconds) => the request plans that answer one
  ;; control press — a callback query, an interaction — so the platform's own
  ;; spinner stops; TEXT, when given, answers the presser alone in it (a
  ;; card's Details). PAYLOAD is the normal form the adapter relays (:id :data
  ;; :user-id :user-name :message-id :channel-id).
  (plan-control-ack (constantly nil) :type function)
  ;; (payload choices &key timeout-seconds) => the request plans that answer
  ;; one autocomplete request — the menu shown while a command's argument is
  ;; typed; PAYLOAD is (:id :token :command :text :user-id :channel-id) and
  ;; CHOICES the (:name :value) plists the catalog offers, possibly empty.
  (plan-autocomplete (constantly nil) :type function)
  ;; (target name &key message-id timeout-seconds) => the plan that opens a
  ;; THREAD named NAME for the ask that arrived on TARGET, hung off the ask's
  ;; own MESSAGE-ID where the platform works that way, or NIL when it cannot
  ;; open one: Telegram's forum topics are made by the person who runs the
  ;; group, and a flat chat or a DM has no threads at all — the ask then runs
  ;; in the channel it was typed in. A TARGET that names a :THREAD-ID is a
  ;; thread there already, and the plan names it NAME.
  (plan-thread (constantly nil) :type function)
  ;; (thread-id &key timeout-seconds) => the plan that removes the thread this
  ;; host opened, for the ask a silent turn answered: an empty room nobody
  ;; asked for. NIL when the platform cannot remove one; it then stays.
  (plan-delete-thread (constantly nil) :type function)
  ;; (body) => the id of the thread a PLAN-THREAD execution opened, read off
  ;; its response BODY, or NIL — the ask then falls back to its channel.
  (thread-id-of (constantly nil) :type function)
  ;; (thread-id) => the text that links a thread in a message — Discord's
  ;; `<#id>' — or NIL, and the host names the thread in its own words.
  (thread-link (constantly nil) :type function)
  ;; (user-id) => the text that mentions USER-ID in a message — Discord's
  ;; `<@id>' — or NIL where naming the id among a post's MENTIONS reaches
  ;; them unspelled (Telegram).
  (mention (constantly nil) :type function)
  ;; (target pathname &key content reply-to voice timeout-seconds) => the
  ;; plan that posts one FILE, CONTENT the caption above it and REPLY-TO the
  ;; message it answers, or NIL when the platform carries no file: a caption
  ;; naming a picture nobody can see is never posted, and POST-FILE answers
  ;; why. VOICE, (:seconds S :waveform OCTETS), posts an Ogg Opus FILE as the
  ;; platform's voice message, S seconds long and drawn as WAV-SHAPE draws it.
  (plan-file (constantly nil) :type function))

;;; --- the host ---------------------------------------------------------------------

(nlk:define-record (channel-host (:copier nil) (:conc-name host-)
                                 (:export :constructor executor lanes worker policy
                                          commands-published))
  (platform (error "platform required") :type platform)
  (executor nil)
  (lanes nil)
  (worker nil)
  (policy nil)
  ;; channels.<id>.owner: the user ids whose instructions are standing
  ;; policy. Everyone else the allowlist admits is a participant with the
  ;; same access (room.lisp, AUTHORITY).
  (owners '() :type list)
  ;; channels.<id>.soul_file, else ~/.nodecode/SOUL.md: the persona every
  ;; lane carries as its `soul` harness section (soul.lisp). NIL only on
  ;; hosts built bare in tests — APPLY-SOUL treats it as nothing to do.
  (soul-path nil :type (or null string))
  (request-timeout 30 :type real)
  (status-update-ms +digest-status-refresh-ms+ :type integer)
  ;; Lanes admitted at once across the whole host, rationed round-robin by
  ;; author. The gateway's own per-session queue cannot fire here — one lane
  ;; holds one turn — so this is the only backpressure, and it is live-only.
  (max-concurrent-turns 4 :type integer)
  ;; channels.<id>.turn_budget_minutes: the budget an ask's turn runs under
  ;; (NLE:TURN-BUDGET, advised in START-HOST). Past it the turn's tool calls
  ;; are refused and it answers with what it has. 0, the default, caps
  ;; nothing: the turn sees its clock on every result instead.
  (turn-budget-minutes 0 :type integer)
  ;; channels.<id>.room_tokens: the most estimated history tokens a room
  ;; keeps; past it the room drops its older half (BOUND-ROOM), so every
  ;; lane forked from it opens on at most this much history. 0 bounds
  ;; nothing.
  (room-tokens +room-tokens+ :type integer)
  ;; channels.<id>.reactions: mark each ask on the message itself — seen
  ;; when admitted, working while its turn runs, cleared when the answer
  ;; lands. Off by default: the status line is the surface, this is a
  ;; glance, and every step is one more platform call.
  (reactions-p nil :type boolean)
  (delivery-workers 4 :type integer)
  (text-limit 2000 :type integer)
  ;; The fingerprint of the command catalog last published to the platform
  ;; (SYNC-COMMANDS), NIL before the first; and whether a publish job is
  ;; in flight, so the tick never queues a second.
  (commands-published nil :type (or null string))
  (commands-in-flight-p nil :type boolean)
  (book (make-room-book))
  ;; channels.<id>.thread_behavior create_per_message: an ask typed in a plain
  ;; channel opens a thread of its own (THREADS below) — except in the channel
  ;; ids channels.<id>.flat_channels names. A bare host keeps the flat surface.
  (threads-p nil :type boolean)
  (flat-channels '() :type list)
  ;; channels.<id>.stream: the words a round writes show on its card as the
  ;; model writes them (digest.lisp). A bare host shows them once the round
  ;; ends.
  (stream-p nil :type boolean)
  ;; channels.<id>.voice_replies: what a room's answers carry besides their
  ;; words when /voice set nothing there (LANE-VOICE-REPLIES). A bare host
  ;; answers in words alone.
  (voice-replies "off" :type string)
  ;; channels.<id>.pairing: a direct message from someone the allowlists do
  ;; not name is answered with a pairing code the operator can approve
  ;; (operator.lisp).
  (pairing-p nil :type boolean)
  ;; channels.<id>.agents: what a room's asks may run as, as (NAME . PLIST);
  ;; channels.<id>.routes: which asks run as which, the most specific first
  ;; (operator.lisp, READ-AGENTS and READ-ROUTES).
  (agents '() :type list)
  (routes '() :type list))

(nlk:access (host channel-host))

(defun host-id (host)
  (platform-id host.platform))

(defun section-thread-behavior (section)
  "The section's channels.<id>.thread_behavior as one of :create-per-message
(the default), :reply-in-place or :disabled — the channel config's own
vocabulary for what the bot does about threads."
  ;; The keyword is the value's own name, hyphenated.
  (let ((value (config-enum section "thread_behavior" "create_per_message"
                            '("create_per_message" "reply_in_place"
                              "disabled"))))
    (intern (substitute #\- #\_ (string-upcase value)) :keyword)))

(defun make-host-from-section (section platform &key executor policy owners
                                                     soul-path
                                                     (request-timeout 30)
                                                     &aux (agents (read-agents platform.id section)))
  "A host over PLATFORM tuned by its channels.<id> SECTION: the request
timeout, the status refresh, the concurrency cap, the delivery pool and
the chunk limit are the same keys for every channel."
  ;; REQUEST-TIMEOUT is the platform's default when the section is silent. The
  ;; thread policy is recorded beside the host — channels.<id>.thread_behavior
  ;; in the channel config's own words: create_per_message (the default) opens
  ;; a thread per ask, and reply_in_place and disabled keep the flat surface.
  ;; channels.<id>.flat_channels narrows create_per_message to the channels
  ;; named flat.
  (make-channel-host
   :platform platform
   :executor executor
   :policy policy
   :owners owners
   :soul-path soul-path
   :lanes (make-lane-table (format nil "channel-~a-lanes"
                                   platform.id))
   :request-timeout (config-integer section "request_timeout_seconds"
                                    request-timeout :min 1)
   :status-update-ms (config-integer section "status_update_ms"
                                     +digest-status-refresh-ms+ :min 0)
   :max-concurrent-turns (config-integer section "max_concurrent_turns" 4
                                         :min 1)
   :turn-budget-minutes (config-integer section "turn_budget_minutes" 0
                                        :min 0)
   :room-tokens (config-integer section "room_tokens" +room-tokens+ :min 0)
   :reactions-p (config-boolean section "reactions" nil)
   :delivery-workers (config-integer section "delivery_workers" 4 :min 1)
   :text-limit (config-integer section "text_chunk_limit" platform.text-limit :min 1)
   :threads-p (eq (section-thread-behavior section) :create-per-message)
   :flat-channels (config-string-list section "flat_channels")
   :stream-p (config-boolean section "stream" t)
   :voice-replies (config-enum section "voice_replies" "off" (mapcar #'car +voice-replies+))
   :pairing-p (config-boolean section "pairing" t)
   :agents agents
   :routes (read-routes platform.id section agents)))

(defvar *hosts* '()
  "Every started channel host, newest first: the registry a room-wide
command walks. Mutated at start and stop, read from workers.")

;;; --- plans through the platform -------------------------------------------------------

(defun host-plan (host builder &rest arguments)
  "What BUILDER — a plan slot of HOST's platform, #'PLATFORM-PLAN-EDIT and its
kin — plans for ARGUMENTS under HOST's request timeout, or NIL when the
platform leaves that slot empty."
  (nlk:when-let (build (funcall builder host.platform))
    (apply build (append arguments (list :timeout-seconds host.request-timeout)))))

(defun address-of (host target message-id)
  (funcall (platform-address-of host.platform) target message-id))

;;; --- delivery side (worker pool) --------------------------------------------------------

(defun post-message (host target text &key reply-to ping controls files mentions what panel)
  "Worker thread: chunked post to TARGET; PANEL, a command's card (OFFER-CARD),
rides the first chunk, drawn where the platform draws one."
  ;; (values DELIVERED-P MESSAGE-IDS
  ;; ERROR) — MESSAGE-IDS in chunk order, so every message the lane produced can
  ;; be bound as an address that routes a reply back to it. FILES ride the first
  ;; chunk — the one that carries the reply — so an answer's picture shows in
  ;; the message that is the answer; CONTROLS ride the last, under the words
  ;; they answer. MENTIONS — the user ids the
  ;; post addresses — open the text in the platform's spelling (PLATFORM-MENTION)
  ;; before it is cut, so the spelling counts against the first chunk's limit.
  ;; Without this, a first chunk cut to the limit would exceed it once the
  ;; platform adds its mention spelling, and the platform would refuse the post
  ;; whole. Every chunk hands the ids on for the platform to let through: the
  ;; note that needs the operator reaches them. WHAT names the post in the one
  ;; warning a refused post is, when the caller has nothing else to do about it.
  (nlk:bind ((spelled (loop for id in mentions
                            for spelling = (funcall (platform-mention host.platform) id)
                            when spelling collect spelling))
             (chunks (split-text-chunks (format nil "~{~a ~}~a" spelled text) host.text-limit))
             ;; A platform whose media message holds fewer words than its text
             ;; message answers one chunk with several plans — the file, then
             ;; what its caption could not hold — so this is a flattening.
             (plans (loop for chunk in chunks
                          append (alexandria:ensure-list
                                  (host-plan host #'platform-plan-message target chunk
                                             :reply-to reply-to :ping ping
                                             :controls (and (= chunk.index chunk.total) controls)
                                             :mentions mentions
                                             :files (and (= 1 chunk.index) files)
                                             :panel (and (= 1 chunk.index) panel)))))
             (id-of (platform-message-id-of host.platform))
             ((delivered results error) (execute-delivery plans host.executor)))
    (unless (or delivered (null what))
      (warn "~a ~a failed: ~a" (host-id host) what error))
    (values delivered
            (loop for result in results
                  when result.ok-p
                    append (let ((id (funcall id-of result.body)))
                             (and id (list id))))
            error)))

(defun post-file (pathname &key platform channel thread content reply-to)
  "Post one FILE into one room as that room's bot."
  ;; PLATFORM names the running channel to post through, by the id its host
  ;; carries ("discord", "telegram"); CHANNEL and THREAD name where the file
  ;; lands — the thread when one is given, the channel otherwise. CONTENT is
  ;; the caption above it; REPLY-TO the message it answers.
  ;;
  ;; A file is how an answer shows a thing rather than tells it: an image
  ;; borrowed from the web (FETCH-IMAGE brings one down), or one annotated
  ;; when the marks are the point. Answers the run's shape, (:status N :body
  ;; VALUE [:error TEXT]) — the body is the message the platform created, its
  ;; id and its attachment among it, so the landing reads from the answer
  ;; itself. A platform that carries no file answers :status 0 with the reason
  ;; and sends nothing; a platform that is not running is an error, the way a
  ;; door call is. Blocks on the caller's thread.
  (unless (and (stringp platform) (plusp (length (string-trim " " platform))))
    (error "post-file names no platform"))
  (unless (and (stringp channel) (plusp (length (string-trim " " channel))))
    (error "post-file names no channel for ~a" platform))
  (let* ((host (or (find platform *hosts* :key #'host-id :test #'string-equal)
                   (error "no ~a channel is running" platform)))
         (plan (host-plan host #'platform-plan-file (list :channel-id channel :thread-id thread)
                          pathname :content content :reply-to reply-to)))
    (if (null plan)
        (list :status 0 :error (format nil "~a carries no files" (platform-name host.platform)))
        (call-answer (execute-plan host.executor plan)))))

(defun running-answer (what platform channel thread session)
  "(values HOST LANE TURN-ID): the running channel PLATFORM names, its lane
in the room CHANNEL and THREAD name — SESSION naming the lane itself — and
the turn running there, whose answer takes what WHAT hands it; an error
naming WHAT when there is none."
  ;; PLATFORM names the running channel by the id its host carries
  ;; ("discord", "telegram"); CHANNEL and THREAD name the room whose running
  ;; turn takes the hand — the thread when one is given, the flat room
  ;; otherwise — and SESSION names the lane itself, for a room several lanes
  ;; answer in.
  (unless (and (stringp platform) (plusp (length (string-trim " " platform))))
    (error "~a names no platform" what))
  (unless (or channel thread session)
    (error "~a names no room for ~a" what platform))
  (let ((host (or (find platform *hosts* :key #'host-id :test #'string-equal)
                  (error "no ~a channel is running" platform)))
        (matches '()))
    (map-lanes host.lanes
               (lambda (lane &aux (target lane.target))
                 (when (and (or (null session)
                                (equal lane.session-id session))
                            (if thread
                                (or (equal (getf target :thread-id) thread)
                                    (equal (getf target :channel-id) thread))
                                (and (null (getf target :thread-id))
                                     (equal (getf target :channel-id) channel))))
                   (push lane matches))))
    ;; one room can hold several lanes: the one running a turn now takes it.
    (when (and (rest matches) (null session))
      (let ((running (remove-if-not #'lane-active-turn-id matches)))
        (when (rest running)
          (error "several lanes are running in ~a — name the session"
                 (or thread channel)))
        (when running (setf matches running))))
    (when (rest matches)
      (error "several lanes answer in ~a — name the thread"
             (or thread channel)))
    (unless matches
      (error "no lane answers in ~a" (or thread channel)))
    (let* ((lane (first matches))
           (turn-id (bt2:with-lock-held ((lane-lock lane))
                      (or (and lane.digest (turn-digest-turn-id lane.digest))
                          lane.active-turn-id))))
      (unless turn-id
        (error "no turn is running in ~a" (or thread channel)))
      (values host lane turn-id))))

(defun answer-file (pathname &key platform channel thread session)
  "Hand one FILE to the answer the running turn is about to deliver: it
rides the answer's own message — in the thread the answer posts from, and
in the line the ask's room keeps — so a picture shows where the answer
shows and never beside it."
  ;; Nothing posts now: the file lands on the message that answers the ask,
  ;; and there is no answer before there is a turn. The room is named as
  ;; RUNNING-ANSWER reads it. Answers the turn the file was handed to,
  ;; (:session-id S :turn-id T :files (PATH ...)), in the order handed;
  ;; errors when no platform runs, no lane answers in that room, or no turn
  ;; is running there. The call stamps itself an act first: it changes the
  ;; answer, so the snippet calling it is never watched, and a watched one's
  ;; run again is refused before it hands anything (NLE:RECEIPT).
  (nle:receipt "calls" (list :verb "answer-file" :source (namestring pathname)))
  (multiple-value-bind (host lane turn-id)
      (running-answer "answer-file" platform channel thread session)
    (declare (ignore host))
    (list :session-id lane.session-id :turn-id turn-id
          :files (add-answer-files lane turn-id (list (pathname pathname))))))

(defparameter +answer-choice-cap+ 5
  "The most choices one answer carries: one row of buttons.")

(defun answer-choices (labels &key platform channel thread session)
  "Hand the answer the running turn is about to deliver LABELS, two to five
choices: they ride the answer's own message as buttons, and a press is the
presser's reply to it, its words the label — a clarifying question asked
with its answers on it."
  ;; Nothing posts now, as with ANSWER-FILE, and the room is named the same
  ;; way. A second call replaces the first: the answer carries the last set.
  ;; A label is at most 80 characters, what a button shows. Answers (:session-id S
  ;; :turn-id T :choices LABELS); errors on labels it would not show, or a
  ;; platform whose messages carry no choices. Stamped an act first, as
  ;; ANSWER-FILE is (watched as a read, it ran again after every
  ;; later snippet and put an earlier question's buttons on later answers).
  (nle:receipt "calls" (list :verb "answer-choices"))
  (unless (and (listp labels)
               (<= 2 (length labels) +answer-choice-cap+)
               (every (lambda (label) (and (stringp label) (<= 1 (length (nlk:trimmed label)) 80)))
                      labels)
               (= (length labels) (length (remove-duplicates labels :test #'string=))))
    (error "answer-choices takes 2 to ~d distinct labels of 1 to 80 characters, got ~s"
           +answer-choice-cap+ labels))
  (multiple-value-bind (host lane turn-id)
      (running-answer "answer-choices" platform channel thread session)
    (unless (platform-choices host.platform)
      (error "~a messages carry no choices" (platform-name host.platform)))
    (let ((labels (mapcar #'nlk:trimmed labels)))
      (setf (gethash lane.session-id *answer-choices*) (list :turn-id turn-id :labels labels))
      (list :session-id lane.session-id :turn-id turn-id :choices labels))))

(defun answer-waits (&key question platform channel thread session)
  "Mark the answer the running turn is about to deliver as waiting on the
person it answers -- a question, a go or no-go, a call only they can make --
QUESTION the line that says on what (the answer's own last line when
omitted). Choices handed with ANSWER-CHOICES mark it already; an answer
nobody marked waits on nobody, whatever its words end on."
  ;; Nothing posts now, as with ANSWER-FILE, and the room is named the same
  ;; way. The mark is the turn's own, in its session's log (NLE:ANSWER-WAITS,
  ;; which stamps the act, checks QUESTION and answers (:session-id S
  ;; :turn-id T :waits QUESTION-OR-T)), so every listing of sessions reads it;
  ;; once the answer posts it is written down beside its messages too
  ;; (DELIVER-ANSWER, ANSWER-WAITING). A second call replaces the first.
  (multiple-value-bind (host lane turn-id)
      (running-answer "answer-waits" platform channel thread session)
    (declare (ignore host))
    (nle:answer-waits :question question :session lane.session-id :turn turn-id)))

(defun bind-addresses (host lane target message-ids)
  (dolist (id message-ids)
    (bind-lane-address host.lanes (address-of host target id)
                       lane.session-id)))

;;; --- typing (per surface, not per lane) -------------------------------------------------

(defun typing-tick (host &aux (live (typing-surfaces host.lanes))
                              (refresh (platform-typing-refresh-ms host.platform)))
  "One typing beat per SURFACE that has a live lane, while the work keeps
showing something."
  ;; N lanes in a channel drive one indicator; driving it N times a window is
  ;; N times the rate limit for no additional pixels. The cap reads the
  ;; surface's newest sighting: a turn that keeps producing keeps its beat
  ;; past +TYPING-CAP-MS+; one that has shown nothing for that long stops.
  (loop for (key target seen) in live
        do (let ((state (surface-typing host key target))
                 (now (now-ms)))
             (unless (typing-state-active-p state)
               (typing-note-started state now))
             (when (typing-due-p state now :refresh-ms refresh
                                       :seen-at-ms seen)
               (typing-note-sent state now)
               (execute-plan host.executor (host-plan host #'platform-plan-typing target)))))
  ;; A surface with nothing live stops re-asserting; the platform expires it.
  (with-room-book (book host)
    (maphash (lambda (key entry)
               (unless (assoc key live :test #'equal)
                 (typing-note-stopped (car entry))))
             book.typing)))

(defmacro on-worker (host &body body)
  "Enqueue BODY as one job on HOST's delivery pool. => whether it queued."
  `(delivery-worker-enqueue (host-worker ,host) (lambda () ,@body)))

;;; --- reactions (per ask, opt-in) -----------------------------------------------------
;;; The one surface that sits on the operator's own message: the eye while
;;; the turn that answers it is active — admitted, queued, running — with no
;;; transition while it runs, and the turn's outcome once it ends: a check
;;; when its answer landed, a cross when it failed, nothing when it was
;;; stopped or chose silence. A turn a steer cut short hands its marks to the
;;; turn the steer opens, which answers for both. Discord takes any unicode
;;; emoji; Telegram's set is fixed, and its plans say these in its own. Data,
;;; so an operator's layer can reword them.

(defparameter +reaction-seen+ "👀"
  "The reaction on an ask while its turn is active.")

(defparameter +reaction-done+ "✅"
  "The reaction an ask keeps once its answer has landed.")

(defparameter +reaction-failed+ "❌"
  "The reaction an ask keeps once its turn has failed.")

(nlk:define-side-table terminal-reactions (:test #'eq :synchronized t)
  "Reaction snapshots keyed by terminal digest until delivery clears them.")

(defvar *reaction-owners* (make-hash-table :test #'equal :synchronized t)
  "The turn id that owns each live (lane, message-id) reaction, or NIL.")

(defun terminal-reaction-snapshot (lane turn-id &aux (selected '())
                                                     (remaining '()))
  "Claim LANE's reactions owned by TURN-ID."
  ;; The caller holds the lane lock.
  ;;
  ;; A queued follow-up has a NIL owner until its own TURN.STARTED fact, so it
  ;; stays visible while the prior turn's terminal delivery removes older
  ;; markers.
  (dolist (mark lane.reactions)
    (let* ((key (list lane (car mark)))
           (owner (gethash key *reaction-owners*)))
      (cond ((or (equal owner turn-id)
                 (and (null turn-id) (null owner)))
             (push mark selected)
             (remhash key *reaction-owners*))
            (t (push mark remaining)))))
  (setf lane.reactions (nreverse remaining))
  (nreverse selected))

(defun disown-reactions (lane turn-id)
  "Hand LANE's reactions owned by TURN-ID to the lane's next turn."
  ;; The caller holds the lane lock. A steer cut TURN-ID short, and the turn
  ;; it opens answers the ask too: its TURN.STARTED claims every unowned mark
  ;; (ADOPT-REACTIONS), so the eye holds through the handover and the outcome
  ;; lands on both messages.
  (dolist (mark lane.reactions)
    (let ((key (list lane (car mark))))
      (when (equal turn-id (gethash key *reaction-owners*))
        (remhash key *reaction-owners*))))
  '())

(defun adopt-reactions (lane turn-id)
  "Make TURN-ID the owner of every unowned reaction on LANE."
  ;; The caller holds the lane lock.
  (dolist (mark lane.reactions)
    (let ((key (list lane (car mark))))
      (unless (gethash key *reaction-owners*)
        (setf (gethash key *reaction-owners*) turn-id)))))

(defun settle-terminal-reactions (host lane reactions &optional outcome)
  "Worker thread: leave OUTCOME — a mark, or NIL for none — in place of every
reaction in REACTIONS after terminal delivery."
  ;; The lane's live reaction book has already been separated from this
  ;; snapshot, so a newer queued ask cannot be touched by an older turn's
  ;; finish job. The outcome is left behind, never recorded: it is no
  ;; turn's to clear.
  (loop for (message-id . emoji) in reactions
        do (perform-reaction-on-target host (message-target lane message-id)
                                       message-id outcome emoji lane)))

(defun perform-reaction-on-target (host target message-id emoji previous lane)
  "Worker thread: the platform calls that leave EMOJI as the bot's one
reaction on MESSAGE-ID in TARGET."
  ;; LANE is the lane that stops reacting when the platform refuses — the mark
  ;; a turn owns. A clear that finds the reaction already gone is success,
  ;; never a refusal.
  (nlk:bind ((plans (host-plan host #'platform-plan-reaction target message-id emoji
                              :previous previous))
             ((delivered results error) (execute-delivery plans host.executor)))
    (cond
      (delivered nil)
      ;; A clear that finds the reaction already gone (Discord's 400) holds.
      ((and (null emoji)
            results
            (every (lambda (result)
                     (or (execution-ok-p result)
                         (member (execution-status result) '(400 404))))
                   results))
       nil)
      (t
       (setf lane.reactions-refused-p t)
       (warn "~a reaction on ~a for ~a refused, the lane stops reacting: ~a"
             (host-id host) message-id lane.session-id error)))))

(defun schedule-reaction (host lane message-id emoji &optional turn-id)
  "Any thread: leave EMOJI (NIL to clear) on MESSAGE-ID when the host
reacts, the platform can, the lane has not been refused, and it is not
already there."
  ;; TURN-ID owns a newly recorded marker. Enqueues; never calls out.
  (when (and message-id
             host.reactions-p
             (platform-plan-reaction host.platform)
             (not lane.reactions-refused-p))
    (multiple-value-bind (changed previous)
        (bt2:with-lock-held ((lane-lock lane))
          (let* ((mark (assoc message-id lane.reactions :test #'equal)) (previous (cdr mark))
                 (key (list lane message-id))
                 (changed (not (equal previous emoji))))
            (when changed
              (setf lane.reactions
                    (let ((rest (remove mark lane.reactions)))
                      (if emoji (acons message-id emoji rest) rest))))
            ;; A mark already right moves only its owner — the promoted ask's eye
            ;; is claimed by the turn that will clear it; a new mark is its turn's.
            (cond ((and emoji (or changed turn-id))
                   (setf (gethash key *reaction-owners*) turn-id))
                  (changed (remhash key *reaction-owners*)))
            (values changed previous)))
      (when changed
        (on-worker host
          (perform-reaction-on-target host (message-target lane message-id)
                                      message-id emoji previous lane))))))

;;; --- controls (the buttons and menus a message carries) -----------------------------------
;;; The kit owns the vocabulary and the permission; the platform owns how a
;;; control renders, how a press arrives, and how the press is answered. A
;;; message's CONTROLS is a list, each element one row:
;;;
;;;   (LABEL DATA STYLE DISABLED)       a button alone on its row
;;;   ((LABEL DATA STYLE DISABLED) ...)  buttons side by side
;;;   (:MENU PLACEHOLDER OPTIONS)        a menu, each option
;;;                                      (LABEL DATA DESCRIPTION CURRENT)
;;;
;;; or :CLEAR, which takes a message's controls away. DATA is what a press
;;; carries back. A running card carries "stop" and "details",
;;; pressed as controls (CONTROL-PRESSED). A CHOICE's data says a line: its
;;; press comes back as a candidate — the presser saying that line in the
;;; room, a reply to the message pressed — admitted and answered as typed
;;; words are, so a clarifying question's answers and a picker's steps need
;;; no handler of their own.
;;;
;;; The data is a press: a short id for the line and the view of the card it
;;; opens (PRESS-TOKEN), kept in the store, so every platform's cap holds it
;;; -- Telegram's callback_data is 64 bytes, a Discord custom_id or option
;;; value 100, and a line like /models PROVIDER MODEL ran past both, its
;;; option left out -- and a card posted before a restart still answers.

(defparameter +press-prefix+ "nck:p:"
  "What a choice's data begins with; the rest is the id its press is kept
under (PRESS-ACTION).")

(defparameter +press-state+ "channel-presses"
  "The state namespace presses are kept in, one key a slot (PRESS-SLOT).")

(defparameter +press-slots+ 500
  "How many presses the store keeps: a ring of slots under the session
state's per-namespace key quota (512), so minting a press never fails a card.")

(defun press-slot (id)
  "The store key press ID is kept under: its digest folded onto the ring of
+PRESS-SLOTS+, or NIL for an id that is no digest."
  ;; A later press whose digest folds onto the same slot takes it; the one it
  ;; displaced then reads back from this image's memory only, and after a
  ;; restart says nothing -- never the other press's line (PRESS-ACTION).
  (nlk:when-let (n (ignore-errors (parse-integer id :radix 16)))
    (princ-to-string (mod n +press-slots+))))

(defvar *presses* (make-hash-table :test #'equal :synchronized t)
  "Press id -> its action, the JSON object PRESS-TOKEN made: read through
from the store, which outlives this image.")

(defun press-token (line &key tab page back back-tab back-page confirm)
  "The data a choice carries: the id of its press, saying LINE -- the view
the card its answer draws opens on beside it: TAB and PAGE of a dialog
(DIALOG-CARD), BACK the line of the dialog it was pressed on, at BACK-TAB
and BACK-PAGE, CONFIRM a line asked about before it runs."
  ;; The id is the action's digest: the same press is the same id on every
  ;; card and every boot, so the store holds one key per distinct press.
  (let* ((action (nlk:json-object "line" line
                                  :when tab "tab" tab :when page "page" page
                                  :when back "back" back :when back-tab "back_tab" back-tab
                                  :when back-page "back_page" back-page
                                  :when confirm "confirm" t))
         (json (nlk:encode-json-object action))
         (id (nlk:short-digest json 12)))
    (unless (gethash id *presses*)
      (setf (gethash id *presses*) action)
      (when (nlk:store-open-p)
        ;; the slot carries its press's id, so a press it displaced reads as none
        (nlk:session-state-put +press-state+ (press-slot id)
                               (nlk:encode-json-object (nlk:json-object "id" id "action" action)))))
    (concatenate 'string +press-prefix+ id)))

(defun press-action (data &aux (id (and (stringp data) (eql 0 (search +press-prefix+ data))
                                        (subseq data (length +press-prefix+)))))
  "The action the press DATA carries (PRESS-TOKEN's JSON object), or NIL for
data that is no press, or a press this home never made."
  (when (plusp (length id))
    (or (gethash id *presses*)
        (nlk:when-let (json (and (nlk:store-open-p) (press-slot id)
                                 (nlk:session-state-get +press-state+ (press-slot id))))
          (let* ((kept (ignore-errors (nlk:decode-json json)))
                 (action (and (hash-table-p kept) (equal id (nlk:json-value kept :text "id"))
                              (gethash "action" kept))))
            (when (hash-table-p action)
              (setf (gethash id *presses*) action)))))))

(defun choice (label line &rest view &key (style :secondary) disabled &allow-other-keys)
  "A button that says LINE when pressed, showing LABEL; VIEW, PRESS-TOKEN's
keys, what the card its answer draws opens on."
  (list label (apply #'press-token line (alexandria:remove-from-plist view :style :disabled))
        style disabled))

(defun menu-choice (label line &rest view &key description current &allow-other-keys)
  "A menu option that says LINE when picked, showing LABEL — DESCRIPTION
under it, CURRENT shown as the one picked; VIEW as CHOICE's."
  (list label (apply #'press-token line (alexandria:remove-from-plist view :description :current))
        description current))

(defun said-line (data)
  "The line a press carrying DATA says, or NIL when DATA is no choice's."
  (nlk:when-let (action (press-action data))
    (nlk:json-value action :text "line")))

(defun candidate-press (candidate)
  "The action of the press CANDIDATE is (PRESS-ACTION of its source's
press_data), or NIL for words typed."
  (press-action (source-field candidate "press_data")))

(defun switch-offered-p (digest now)
  "True while DIGEST's turn has shown nothing past the stall window and its
provider attempt still waits for a first token with a failover candidate
behind it (NLE:PROVIDER-ATTEMPT-NEXT)."
  (and (eq digest.phase :running)
       (not digest.background-pending)
       (null (digest-open-step digest))
       (> (- now (digest-seen-ms digest)) +digest-stall-ms+)
       digest.turn-id
       (nle:provider-attempt-next digest.turn-id)
       t))

(defun line-controls (lane digest)
  "The controls LANE's card carries for DIGEST's turn: while it waits, Raise
limit (LIFT-PRESSED); while it runs, Stop beside Details on one row, Switch
model between them while a silent provider has a failover candidate behind
it (SKIP-PRESSED); once it settled, Details alone, the card staying as the
turn's record. Each turn's press routes back through the lane's session id;
Raise limit is the host's."
  ;; (LABEL DATA STYLE): the kit names the button's weight, the platform
  ;; styles it.
  (let ((details (list "Details" (format nil "nck:details:~a" lane.session-id) :secondary)))
    (case digest.phase
      (:queued (list (list (list "Raise limit" "nck:lift" :primary))))
      (:running (list (append (list (list "Stop" (format nil "nck:stop:~a" lane.session-id) :danger))
                              (when (switch-offered-p digest (now-ms))
                                (list (list "Switch model" (format nil "nck:skip:~a" lane.session-id)
                                            :secondary)))
                              (list details))))
      ((:completed :failed :cancelled :paused) (list details)))))

(defun deliver-or-warn (host plans what)
  "Worker thread: run PLANS through HOST's executor; one that fails is a
warning naming WHAT. => whether they were delivered."
  (nlk:bind (((delivered _ error)
              (execute-delivery plans host.executor)))
    (unless delivered
      (warn "~a ~a: ~a" (host-id host) what error))
    delivered))

(defun control-pressed (host payload &aux (data (getf payload :data))
                                          (user-id (getf payload :user-id)))
  "Adapter thread: one control press — PAYLOAD is the normal form
(:id :data :user-id :user-name :message-id :channel-id)."
  ;; Returns at once.
  ;;
  ;; The STOP is acted on HERE, on the thread the press arrived on, before the
  ;; press is acknowledged. Cancelling is not a platform call — it sets the
  ;; turn's signal and wakes the threads parked on it — and the room's one
  ;; escape from a running turn must not queue behind the delivery pool's
  ;; posts and edits, nor wait out the acknowledgement's own round trip to a
  ;; platform that may be slow or failing. The engine cuts a running round at
  ;; its next stream line, so the press is felt as soon as the flag is set.
  ;;
  ;; Everything else is a worker's: the acknowledgement that stops the
  ;; platform's own spinner, which a Details press answers with the card's
  ;; details and a step's Output press with what the step answered.
  (when (and (stringp data) (stringp user-id)
             (eql 0 (search "nck:stop:" data)))
    (stop-pressed host user-id (subseq data (length "nck:stop:")) (getf payload :user-name)))
  ;; Switch model is a stop's kin: a flag the walking attempt looks at once
  ;; a second, set here; the worker only says where the round went.
  (let ((switched (and (stringp data) (stringp user-id)
                       (eql 0 (search "nck:skip:" data))
                       (skip-pressed host user-id (subseq data (length "nck:skip:"))
                                     (getf payload :user-name)))))
    (when switched
      (on-worker host
        (deliver-or-warn host (host-plan host #'platform-plan-control-ack payload :text switched)
                         (format nil "control ack for ~a failed" (getf payload :id))))
      (return-from control-pressed nil)))
  (on-worker host
    (flet ((after (prefix) (and (stringp data) (eql 0 (search prefix data)) (subseq data (length prefix)))))
      (let ((session-id (after "nck:details:"))
            (press (after +step-press-prefix+))
            (asked (after "nck:access:"))
            (closer (after "nck:close:"))
            (granted (after "nck:grant:")))
        (cond ((equal data "nck:lift") (lift-pressed host payload))
              (asked (access-pressed host payload asked))
              (closer (close-pressed host payload closer))
              (granted (grant-pressed host payload granted))
              (session-id (details-pressed host payload session-id))
              (press (step-pressed host payload press))
              ;; A choice whose press this home cannot read back (PRESS-ACTION)
              ;; arrives here, never as its line: the card is older than the
              ;; press vocabulary, or than the store it was kept in.
              (t (deliver-or-warn host (if (or (after "nck:stop:") (equal data "nck:page"))
                                           (host-plan host #'platform-plan-control-ack payload)
                                           (host-plan host #'platform-plan-control-ack payload
                                                      :text "This button is out of date: run the command again."))
                                  (format nil "control ack for ~a failed" (getf payload :id)))))))))

(defun skip-pressed (host user-id session-id &optional user-name
                     &aux (lane (find-lane host.lanes session-id)))
  "The Switch model control: cut SESSION-ID's provider attempt while it waits
for a first token, so its round leaves for the failover candidate, when
USER-ID owns the lane or operates the room. => what the presser is told."
  ;; Runs on the thread the press arrived on, as a stop does: it sets a flag
  ;; and makes no platform call.
  (cond
    ((or (null lane) (null (lane-active-turn-id lane)))
     "Nothing to switch: the turn has ended.")
    ((not (or (equal user-id (lane-owner-id lane)) (operator-p host user-id)))
     "Only whoever asked, or a room operator, can switch the model.")
    (t (let ((next (nle:skip-provider-attempt
                    (lane-active-turn-id lane)
                    (if user-name (format nil "switched by ~a" user-name) "switched from the room"))))
         (if next
             (format nil "Switching to ~a/~a." (getf next :provider) (getf next :model))
             "Nothing to switch: the model has started answering.")))))

(defun stop-pressed (host user-id session-id &optional user-name
                     &aux (lane (find-lane host.lanes session-id)))
  "The stop control — cancel SESSION-ID's active turn when USER-ID owns the
lane or operates the room; the card says who, by USER-NAME."
  ;; Runs on the thread the press arrived on
  ;; (CONTROL-PRESSED): it takes two short locks and signals, and makes no
  ;; platform call, so nothing here can park on a network.
  (cond
    ((null lane) nil)
    ((null (lane-active-turn-id lane))
     (unless (and (or (equal user-id (lane-owner-id lane))
                      (operator-p host user-id))
                  (stop-turnless-line host lane (if user-name
                                                    (format nil "stopped by ~a" user-name)
                                                    "stopped from the room")))
       (warn "~a: stop press found no active turn on ~a"
             (host-id host) lane.session-id)))
    ((or (equal user-id (lane-owner-id lane))
         (operator-p host user-id))
     (or (nlk:request-cancel-turn lane.session-id lane.active-turn-id
                                  (if user-name (format nil "stopped by ~a" user-name) "stopped from the room"))
         (warn "~a: stop of ~a found no active turn"
               (host-id host) lane.session-id)))
    (t (warn "~a: ~a may not stop ~a"
             (host-id host) user-id lane.session-id))))

;;; --- Raise limit (a queued card's way past the gate) -------------------------------
;;; A queued ask's card says it waits and carries Raise limit. A room
;;; operator's press doubles how many turns run at once across the host --
;;; live at once, and written to channels.<id>.max_concurrent_turns so a
;;; restart keeps it -- and lets the line through as far as the new limit
;;; reaches. Anyone else's press is answered, to them alone, with whose the
;;; limit is; it moves nothing.

(defun raise-limit (host &aux (was host.max-concurrent-turns) (limit (* 2 (max 1 was)))
                              (key (list "channels" (platform-id host.platform) "max_concurrent_turns")))
  "Double HOST's turns at once, keep it in the config, and let the line
through. => what the operator who raised it is told."
  ;; The live limit moves first: the config write can be refused (no config
  ;; in this process, a document the writer cannot read) and the line still
  ;; moves, the answer then saying the raise lasts until a restart.
  (setf host.max-concurrent-turns limit)
  (let ((refusal (handler-case (progn (nle:config-set key limit) nil)
                   (error (condition) (princ-to-string condition)))))
    (multiple-value-bind (admitted pending) (claim-waiting host limit)
      (dolist (ask admitted) (admit-ask host ask))
      (when (or admitted pending)
        (save-queued-asks host))
      (recount-queued host pending)
      (format nil "Raised to ~d turns at once (was ~d)~[~:;; ~:*~d waiting ask~:p started~]. ~:[Saved as ~{~a~^.~}.~;~:*Until a restart only: the config was not written (~a).~]"
              limit was (length admitted) refusal key))))

(defun lift-pressed (host payload &aux (user-id (getf payload :user-id)))
  "Worker thread: a queued card's Raise limit press, answered to the presser
alone -- a room operator's raises the limit (RAISE-LIMIT)."
  (deliver-or-warn host
                   (host-plan host #'platform-plan-control-ack payload
                              :text (if (operator-p host user-id)
                                        (raise-limit host)
                                        (format nil "Only the room's operators can raise the limit (~d turn~:p at once): ask one of them."
                                                host.max-concurrent-turns)))
                   (format nil "control ack for ~a failed" (getf payload :id))))

;;; --- Close (an answer's way to put its thread away) ---------------------------------
;;; An answer in a thread carries Close under its words. The asker's press, or a
;;; room operator's, closes the thread the press came from -- out of the room's
;;; open threads, kept, and open again once anyone writes in it -- unless a turn
;;; still runs there. Anyone else's press moves nothing. Every press is answered
;;; to the presser alone.

(nlk:define-side-table platform-plan-close-thread (:test #'eq :weakness :key :synchronized t)
  "A PLATFORM's (thread-id &key timeout-seconds) => the plan that CLOSES a
thread: puts it away, out of the room's open threads, kept and searchable,
and open again once anyone writes in it (Discord's archive). NIL when the
platform cannot close one: an answer in a thread then carries no Close.")
;; A table hung beside the platform rather than a slot of it: a running image
;; holds its platforms, and a slot added to the record would leave them
;; obsolete under it.

(nlk:define-side-table platform-closed-note (:test #'eq :weakness :key :synchronized t)
  "What a PLATFORM's Close press tells the presser once the thread is closed,
in its own terms -- a Telegram topic stays locked until an admin reopens it --
or NIL for the kit's words: open again once anyone writes in it.")

(defun answer-controls (host lane choices &aux (thread (getf lane.target :thread-id)))
  "The rows of buttons LANE's answer carries: its CHOICES, one row of buttons
each saying its label; then, in a thread the platform can close, Close
(CLOSE-PRESSED), naming who asked."
  (append (and choices
               (list (loop for label in choices
                           collect (choice label label :style :primary))))
          (and thread (platform-choices host.platform)
               (host-plan host #'platform-plan-close-thread thread)
               (list (list (list "Close thread" (format nil "nck:close:~a" (or lane.owner-id ""))
                                 :secondary))))))

(defun thread-running-p (host thread-id &aux (running nil))
  "Whether a turn runs, or waits to, in the thread THREAD-ID."
  (map-lanes host.lanes
             (lambda (lane)
               (when (and (equal thread-id (getf lane.target :thread-id))
                          (or lane.active-turn-id
                              (and lane.digest (eq :queued (turn-digest-phase lane.digest)))))
                 (setf running t))))
  running)

(defun close-pressed (host payload owner-id &aux (user-id (getf payload :user-id))
                                                 (thread (or (getf payload :thread-id)
                                                             (getf payload :channel-id))))
  "Worker thread: an answer's Close press -- the asker's (OWNER-ID) or a room
operator's closes the thread the press came from (PLATFORM-PLAN-CLOSE-THREAD):
the payload's :THREAD-ID where the platform's threads live inside a chat
(Telegram's topics), else its channel, which is the thread (Discord's)."
  (let* ((allowed (or (operator-p host user-id)
                      (and (plusp (length owner-id)) (equal user-id owner-id))))
         (plan (and (stringp thread) (host-plan host #'platform-plan-close-thread thread)))
         (busy (and plan (thread-running-p host thread)))
         (text (cond ((not allowed) "Only the person who asked or the room's operators can close this thread.")
                     ((null plan) "This room can't be closed.")
                     (busy "A turn still runs here: close the thread once it has answered, or stop it first.")
                     (t (or (platform-closed-note host.platform)
                            "Closed. Write in it to open it again.")))))
    ;; The answer goes first: a press is answered within the platform's few
    ;; seconds, and a closed thread takes no more words.
    (deliver-or-warn host (host-plan host #'platform-plan-control-ack payload :text text)
                     (format nil "control ack for ~a failed" (getf payload :id)))
    (when (and allowed plan (not busy))
      (nlk:bind (((closed _ error) (execute-delivery (list plan) host.executor)))
        ;; A close the platform refused leaves the thread open, and says so
        ;; where the press said it closed.
        (unless closed
          (post-message host (list :channel-id (or (getf payload :channel-id) thread) :thread-id thread)
                        (format nil "This thread did not close: ~a" error)
                        :what "close refusal"))
        closed))))

;;; --- Ask for access (an operator-only command's way past the gate) ------------------
;;; A refused command carries Ask for access. The refused person's press posts
;;; one request into the room, addressed to its operators, carrying Make
;;; operator -- at most once in ten minutes a person. An operator's press of
;;; that adds the person to the room's operators, live at once and written to
;;; channels.<id>.owner so a restart keeps it, and tells them in the room;
;;; anyone else's press moves nothing. Every press is answered to the presser
;;; alone.

(defvar *access-asked* (make-hash-table :test #'equal :synchronized t)
  "(host id . user id) -> when that person last asked the room's operators
for access (ACCESS-PRESSED).")

(defun access-pressed (host payload command &aux (user-id (getf payload :user-id))
                                                 (name (getf payload :user-name))
                                                 (key (cons (host-id host) user-id))
                                                 (now (now-ms)))
  "Worker thread: a refusal's Ask for access press for COMMAND -- one request
to the room's operators, carrying Make operator."
  (let* ((name (if (plusp (length name)) (subseq name 0 (min 40 (length name))) "Someone"))
         (last (gethash key *access-asked*))
         (text (cond ((operator-p host user-id)
                      "You're already one of this room's operators: run the command again.")
                     ((and last (< (- now last) 600000))
                      "Already asked: the room's operators have your request.")
                     (t (setf (gethash key *access-asked*) now)
                        (if (post-message host (list :channel-id (getf payload :channel-id))
                                          (format nil "🔑 ~a asks to run /~a, which only this room's operators can run. Make them an operator to let them run every command."
                                                  name command)
                                          :mentions host.owners
                                          :controls (list (list (list (format nil "Make ~a an operator" name)
                                                                      (format nil "nck:grant:~a" user-id)
                                                                      :primary)))
                                          :what "access request")
                            "Asked: the room's operators got your request, and can let you in with one press."
                            (progn (remhash key *access-asked*)
                                   "The request did not post: ask an operator directly."))))))
    (deliver-or-warn host (host-plan host #'platform-plan-control-ack payload :text text)
                     (format nil "control ack for ~a failed" (getf payload :id)))))

(defun make-operator (host user-id &aux (key (list "channels" (platform-id host.platform) "owner")))
  "Add USER-ID to HOST's operators, live and in the config. => what the
operator who pressed is told."
  ;; Live first, as RAISE-LIMIT: a refused config write still lets the person
  ;; in, the answer then saying it lasts until a restart.
  (setf host.owners (append host.owners (list user-id)))
  (let ((refusal (handler-case (progn (nle:config-set key (copy-list host.owners)) nil)
                   (error (condition) (princ-to-string condition)))))
    (format nil "Made an operator (~d now). ~:[Saved as ~{~a~^.~}.~;~:*Until a restart only: the config was not written (~a).~]"
            (length host.owners) refusal key)))

(defun grant-pressed (host payload user-id &aux (presser (getf payload :user-id)))
  "Worker thread: an access request's Make operator press for USER-ID -- a
room operator's makes them one and tells them in the room."
  (let* ((grant (and (operator-p host presser) (plusp (length user-id))
                     (not (operator-p host user-id))))
         (text (cond ((not (operator-p host presser))
                      "Only the room's operators can make someone an operator.")
                     ((not grant) "Already one of this room's operators.")
                     (t (make-operator host user-id)))))
    (deliver-or-warn host (host-plan host #'platform-plan-control-ack payload :text text)
                     (format nil "control ack for ~a failed" (getf payload :id)))
    (when grant
      (post-message host (list :channel-id (getf payload :channel-id))
                    "you're now one of this room's operators: run the command again."
                    :mentions (list user-id) :what "access granted"))))

;;; --- Details (everything a card leaves out, to whoever pressed it) -----------------
;;; A card shows the newest steps; its Details press answers the person who
;;; pressed it, and no one else, with all of them — each with the end of what
;;; it answered — and the newest thought in whole sentences (DIGEST-DETAILS).
;;; Private where the platform can be (a Discord ephemeral answer, a Telegram
;;; alert), so a room is never flooded with one person's curiosity: the toggle
;;; this replaced posted every call into the room for everyone. Who may press
;;; is Stop's rule: the person who asked, and the room's operators.

(defvar *card-details* (make-hash-table :test #'equal :synchronized t)
  "A settled card's address (ADDRESS-OF) -> (OWNER-ID . DETAILS): what a
Details press on a card whose turn is over shows. Live-only, as *HOSTS* is: a
press after a restart is told the steps are gone.")

(defvar *card-details-order* '()
  "The ids *CARD-DETAILS* holds, newest first, for dropping the oldest.")

(defparameter +card-details-kept+ 500
  "Settled cards whose details a press can still show.")

(defun keep-card-details (host lane digest)
  "Worker thread: DIGEST's turn settled its card — keep what a later Details
press on it shows."
  (multiple-value-bind (status-id details)
      (bt2:with-lock-held ((lane-lock lane))
        (values digest.status-id (digest-details digest (now-ms))))
    (when status-id
      (sb-ext:with-locked-hash-table (*card-details*)
        (let ((address (address-of host lane.target status-id)))
          (unless (gethash address *card-details*)
            (push address *card-details-order*))
          (setf (gethash address *card-details*) (cons lane.owner-id details)))
        (loop while (> (hash-table-count *card-details*) +card-details-kept+)
              do (remhash (car (last *card-details-order*)) *card-details*)
                 (setf *card-details-order* (butlast *card-details-order*)))))))

(defun pressed-card-details (host payload session-id &aux (card-id (getf payload :message-id))
                                                          (lane (find-lane host.lanes session-id)))
  "What the card a press PAYLOAD came from in SESSION-ID's room shows, (OWNER-ID
. DETAILS) — the running turn's from its digest, a settled one's from what its
card kept — or NIL once they are no longer held."
  (or (and lane
           (bt2:with-lock-held ((lane-lock lane))
             (let ((digest lane.digest))
               (and digest (equal card-id digest.status-id)
                    (member digest.phase '(:queued :running))
                    (cons lane.owner-id (digest-details digest (now-ms)))))))
      (gethash (address-of host (list :channel-id (getf payload :channel-id)) card-id)
               *card-details*)))

(defun card-press-text (host payload pressed answer &aux (user-id (getf payload :user-id)))
  "What a press on a card answers its presser: ANSWER called with the card's
details, PRESSED (PRESSED-CARD-DETAILS) — or why not."
  (destructuring-bind (&optional owner . details) pressed
    (cond ((null details) "This card's steps are no longer held.")
          ((not (or (equal user-id owner) (operator-p host user-id)))
           "Details are for the person who asked and the room's operators.")
          (t (funcall answer details)))))

(defun details-pressed (host payload session-id)
  "Worker thread: a card's Details press, answered to the presser alone."
  (deliver-or-warn host
                   (host-plan host #'platform-plan-control-ack payload
                              :text (card-press-text host payload (pressed-card-details host payload session-id)
                                                     (lambda (details) (details-text details host.text-limit))))
                   (format nil "details for ~a failed" (getf payload :id))))

(defun step-pressed (host payload press &aux (colon (position #\: press :from-end t)))
  "Worker thread: a step's Output press, PRESS its session id and number
(STEP-PRESS), answered to the presser alone with the end of what the step
answered; who may press is Details' rule."
  (let ((number (and colon (parse-integer press :start (1+ colon) :junk-allowed t))))
    (deliver-or-warn host
                     (host-plan host #'platform-plan-control-ack payload
                                :text (card-press-text
                                       host payload (and colon (pressed-card-details host payload (subseq press 0 colon)))
                                       (lambda (details)
                                         (nlk:if-let (step (and number (find number (getf details :steps) :key #'fifth)))
                                           (step-text step host.text-limit)
                                           "This step is no longer held."))))
                     (format nil "step output for ~a failed" (getf payload :id)))))

;;; --- completions (a command's argument, completed while it is typed) -----------------
;;; The platform asks for choices as the operator types an argument: the
;;; catalog owns what completes (NLE:SLASH-COMPLETIONS), the kit owns who
;;; may see it — the commands' own rule — and the platform owns how the
;;; menu is answered. One request, one answer, three seconds to give it.

(defun completion-allowed-p (host name user-id)
  "Whether USER-ID may see NAME's argument completions: the commands' own
rule — with operators declared, every command but /help is theirs."
  (or (null host.owners) (and (stringp name) (string= name "help")) (operator-p host user-id)))

(defun completion-requested (host payload)
  "Adapter thread: one autocomplete request — PAYLOAD is the normal form
(:id :token :command :text :user-id :channel-id :thread-id), its channel and
thread a target."
  ;; Answers on a worker; returns at once, the thread that runs this is the
  ;; connection's. The command completes in the room it is typed in, the
  ;; session it would act on, which completing never opens.
  (on-worker host
    ;; Choices only for an asker who may run the command.
    (let* ((name (getf payload :command))
           (choices (and (stringp name)
                         (completion-allowed-p host name (getf payload :user-id))
                         (nle:slash-completions
                          name (getf payload :text)
                          :session-id (target-room-id (platform-session-prefix host.platform)
                                                      payload)))))
      (deliver-or-warn host (host-plan host #'platform-plan-autocomplete payload choices)
                       (format nil "completions for ~a failed" name)))))

(defun stop-room-turn (room-session-id)
  "(values STOPPED-P TEXT): ask the engine to cancel the newest turn
running among ROOM-SESSION-ID's lanes, across every started host."
  ;; The /stop answer.
  (let ((newest nil)
        ;; A room's lanes read its session id plus a message suffix.
        (prefix (format nil "~a-m" room-session-id)))
    (dolist (host *hosts*)
      (map-lanes host.lanes
                 (lambda (lane)
                   (when (and lane.active-turn-id
                              (uiop:string-prefix-p prefix lane.session-id)
                              (or (null newest) (> lane.last-active-ms newest.last-active-ms)))
                     (setf newest lane)))))
    (cond ((null newest) (values nil "no turn is running in this room"))
          ((nlk:request-cancel-turn
            (lane-session-id newest) (lane-active-turn-id newest)
            "stopped from the room")
           ;; Said to the room: no session id, which reads as noise there.
           (values t "stopping the running turn…"))
          (t (values nil "the turn already settled")))))

;;; --- the status line ------------------------------------------------------------------------

(defmacro with-lane-digest ((digest lane) &body body)
  "BODY under LANE's lock with DIGEST bound to the lane's digest; nothing when
the lane has none."
  `(bt2:with-lock-held ((lane-lock ,lane))
     (let ((,digest (lane-digest ,lane)))
       (when ,digest ,@body))))

(defmacro with-turn-digest ((digest lane turn-id now) &body body)
  "BODY under LANE's lock with DIGEST bound to the lane's digest for TURN-ID,
opened at NOW when the lane holds none for it (LANE-TURN-DIGEST)."
  `(bt2:with-lock-held ((lane-lock ,lane))
     (let ((,digest (lane-turn-digest ,lane ,turn-id ,now))) ,@body)))

(defun warn-lane (host lane what detail)
  (warn "~a ~a for ~a failed: ~a" (host-id host) what lane.session-id detail))

(defun run-plan (host lane what plan &aux (result (execute-plan host.executor plan)))
  "Worker thread: run PLAN through HOST's executor, warn LANE's reader under
WHAT when the platform refused it, and answer the execution either way."
  ;; The execution is the answer because a refusal is not always the end of
  ;; the story: a terminal status edit records its text as settled anyway, so
  ;; the loop closes rather than replanning for the life of the lane.
  (unless result.ok-p
    (warn-lane host lane what result.error))
  result)

(defun note-lane-provider (lane &aux (digest (lane-digest lane)))
  "Tick thread, no lock held: tell LANE's running card whom its rounds wait
on, once, when it has heard of nobody -- the identity a turn of the lane's
session binds (NLE:GATEWAY-TARGET), a store read the lane lock is never held
over."
  (when (and digest
             (eq (turn-digest-phase digest) :running)
             (not (getf (round-stream digest) :asked))
             (null (getf (turn-digest-usage digest) :provider)))
    (digest-note-provider digest (ignore-errors (values (nle:gateway-target lane.session-id))))))

(defun schedule-flush (host lane &aux kind card status-id terminal-p reply-to controls media)
  "Tick thread: plan LANE's card under the lane lock (pure), and enqueue the
platform call it implies."
  ;; At most one flush per lane is ever outstanding — with a pool of drainers,
  ;; two jobs planned from the same digest would race to create two cards.
  ;; What the turn says on its way rides the card (digest.lisp).
  (note-lane-provider lane)
  (with-lane-digest (digest lane)
    (unless lane.flush-in-flight-p
      (setf controls (or (line-controls lane digest) :clear))
      ;; The card opens titled the ask's first words; the title its session
      ;; is named replaces them when it lands (NAMING-ADVICE).
      (unless (or digest.task (zerop (length lane.said)))
        (setf digest.task (first-words lane.said)))
      (multiple-value-setq (kind card)
        (digest-status-plan digest (now-ms) :min-update-ms host.status-update-ms :controls controls))
      (when (eq kind :skip) (setf kind nil))
      (when kind
        (digest-status-attempted digest (now-ms) :controls controls)
        (setf lane.flush-in-flight-p t
              status-id digest.status-id
              media digest.posted-media
              reply-to (lane-reference lane digest.ask-id)
              terminal-p (not (member digest.phase
                                      '(:queued :running)))))))
  (when kind
    (on-worker host
      (nlk:with-cleanup ((bt2:with-lock-held ((lane-lock lane))
                           (setf (lane-flush-in-flight-p lane) nil)))
        (perform-flush host lane kind card status-id terminal-p
                       :reply-to reply-to :controls controls :media media)))))

(defun post-card (host target card &key reply-to controls)
  "Worker thread: CARD posted on TARGET as one message, a silent reply to
REPLY-TO carrying CONTROLS. => (values DELIVERED MESSAGE-ID ERROR MEDIA
REFUSED), MEDIA what the message holds of the card's pictures (MEDIA-OF),
REFUSED whether the platform refused the request itself (a 4xx, not a rate
limit), so the same post would be refused again."
  ;; One message, never chunks: a card is a few lines, and the platform that
  ;; posts its words posts them whole — the first plan of what it answers.
  (let ((result (execute-plan host.executor
                              (first (alexandria:ensure-list
                                      (host-plan host #'platform-plan-message target
                                                 (make-text-chunk :text (nlk:clip (card-text card) (1- host.text-limit) :ellipsis "…"))
                                                 :reply-to reply-to :controls controls :card card))))))
    (values result.ok-p
            (and result.ok-p (funcall (platform-message-id-of host.platform) result.body))
            result.error
            (and result.ok-p (funcall (platform-media-of host.platform) result.body))
            (and (not result.ok-p) (<= 400 result.status 499) (/= result.status 429)))))

(defun edit-card (host lane status-id card what &key retry controls media)
  "Worker thread: LANE's card message STATUS-ID edited into CARD, CONTROLS on
it, the pictures it holds (MEDIA) kept. => (values EXECUTION LANDED), LANDED
what the message holds of the card's pictures once it took the edit."
  (let ((result (run-plan host lane what
                          (host-plan host #'platform-plan-edit lane.target status-id (card-text card)
                                     :retry retry :controls controls :card card :media media))))
    (values result (and result.ok-p (funcall (platform-media-of host.platform) result.body)))))

(defun perform-flush (host lane kind card status-id terminal-p &key reply-to
                                                         controls media &aux (target lane.target))
  "Worker thread: the one card post or edit the plan asked for, MEDIA what the
card's message holds of its pictures."
  ;; A post is a silent reply to REPLY-TO, the ask the card stands in for:
  ;; with several lanes running in one room, the anchor is what says whose
  ;; card this is, and the answer below it replies to the same message.
  (ecase kind
    (:post
     (multiple-value-bind (delivered message-id error landed refused)
         (post-card host target card :reply-to reply-to :controls controls)
       (cond ((not delivered)
              ;; A refused post would be refused again on every tick's
              ;; window for as long as the turn runs.
              (when refused
                (with-lane-digest (digest lane)
                  (digest-status-refused digest card)))
              (warn-lane host lane "card post" error))
             (t (bind-addresses host lane target (list message-id))
                (let ((settled-p nil))
                  (with-lane-digest (digest lane)
                    (digest-status-delivered digest card message-id landed)
                    ;; The settle can race this post: the finish job settles
                    ;; the card while it is still in flight — it reads no id
                    ;; — and the card lands after, a working card no turn
                    ;; watches. A post planned while the turn still ran whose
                    ;; turn has since ended retires itself.
                    (setf settled-p
                          (and (not terminal-p) (not (member digest.phase '(:queued :running))))))
                  (when (and settled-p message-id)
                    (retire-chrome host lane message-id)))))))
    (:edit
     (multiple-value-bind (result landed)
         (edit-card host lane status-id card "card edit" :retry terminal-p :controls controls :media media)
       ;; A running edit is superseded by the next tick, but a TERMINAL
       ;; one bypasses the refresh throttle by design — nothing follows
       ;; it — so a permanent failure (deleted message, lost access)
       ;; would replan on every tick for the life of the lane. The
       ;; plan's own retries already ran; record the card as settled so
       ;; the loop closes.
       (cond (result.ok-p (with-lane-digest (digest lane)
                            (digest-status-delivered digest card nil landed)))
             (terminal-p (with-lane-digest (digest lane)
                           (digest-status-delivered digest card))))))))

(defun wear-title (host lane digest title &aux (origin (lane-origin lane))
                                              (thread-id (getf origin :thread-id)))
  "Naming thread: LANE's first ask is titled TITLE -- its card, DIGEST, wears
it, and the thread the kit opened for it is renamed to it."
  ;; A title that lands while the turn runs rides the card's next edit, one
  ;; that lands before the card is up opens it, and one that lands after the
  ;; card settled retitles it (RETITLE-CARD), the tick no longer editing it. A
  ;; refused rename leaves the thread its first words, in the log alone.
  (when (and digest
             (bt2:with-lock-held ((lane-lock lane))
               (setf (turn-digest-task digest) title)
               (not (member (turn-digest-phase digest) '(:queued :running)))))
    (retitle-card host lane digest title))
  (let* ((plan (and thread-id
                    (host-plan host #'platform-plan-thread
                               (list :channel-id (getf origin :channel-id) :thread-id thread-id)
                               title)))
         (result (and plan (execute-plan host.executor plan))))
    (when (and result (not result.ok-p))
      (warn "~a: thread ~a keeps its first words, the rename was refused: ~a"
            (host-id host) thread-id result.error))))

(defun naming-advice (host)
  "Advice on NLE:NAME-SESSION: a session of one of HOST's lanes is named from
its ask's own words, not the room's envelope its prompt carries, and the
title it is given is its ask's (WEAR-TITLE). Every other session is NEXT's."
  ;; The kernel names a session once, from its first prompt: a lane's is the
  ;; ask that opened it, so its thread and that ask's card wear the one title,
  ;; and a later ask's card its own first words. An ask of five words or
  ;; fewer is its own name: it costs no call, and wears nothing new.
  (lambda (next session-id text &aux (lane (find-lane host.lanes session-id)) digest said)
    (if (null lane)
        (funcall next session-id text)
        (progn
          (bt2:with-lock-held ((lane-lock lane))
            (setf digest lane.digest said lane.said))
          (let ((title (funcall next session-id said)))
            (when (and title (string/= title (format nil "~{~a~^ ~}" (nlk:split-words said))))
              (wear-title host lane digest title))
            title)))))

(defun card-with (card &rest parts)
  "CARD with PARTS, (KEY VALUE)*, in place of its own."
  (append parts (loop for (key value) on card by #'cddr
                      unless (loop for (part) on parts by #'cddr thereis (eq part key))
                        append (list key value))))

(defun retitle-card (host lane digest title &aux status-id card controls media)
  "Give DIGEST's settled card TITLE: the card it last delivered, retitled."
  ;; Settled, the digest has forgotten its steps; the card it delivered is the
  ;; card, so the edit restates that one with the new title, its settled
  ;; controls and the pictures it holds. A card retired meanwhile is left alone.
  (bt2:with-lock-held ((lane-lock lane))
    (setf status-id (turn-digest-status-id digest)
          card (let ((posted (turn-digest-posted-card digest)))
                 (and posted (card-with posted :task title)))
          controls (line-controls lane digest)
          media (turn-digest-posted-media digest)))
  (when (and status-id card)
    (multiple-value-bind (result landed)
        (edit-card host lane status-id card "card retitle" :retry t :controls controls :media media)
      (when (execution-ok-p result)
        (bt2:with-lock-held ((lane-lock lane))
          (when (equal status-id (turn-digest-status-id digest))
            (digest-status-delivered digest card nil landed)))))))

(defun flush-now (host lane &aux kind card status-id reply-to controls media)
  "Plan and perform LANE's card on THIS thread."
  ;; The terminal settle: a failed turn's notice must land before the finish
  ;; path moves on, and the tick's next pass is up to half a second away.
  (with-lane-digest (digest lane)
    (setf controls (or (line-controls lane digest) :clear))
    (unless (or digest.task (zerop (length lane.said)))
      (setf digest.task (first-words lane.said)))
    (multiple-value-setq (kind card)
      (digest-status-plan digest (now-ms) :min-update-ms host.status-update-ms :controls controls))
    (cond ((eq kind :skip) (setf kind nil))
          (t (digest-status-attempted digest (now-ms) :controls controls)
             (setf status-id digest.status-id
                   media digest.posted-media
                   reply-to digest.ask-id))))
  (when kind
    (perform-flush host lane kind card status-id t :reply-to reply-to
                   :controls controls :media media)))

(defun retire-thread (host lane &aux (origin (lane-empty-thread lane))
                                     (thread-id (getf origin :thread-id))
                                     (plan (and thread-id
                                                (host-plan host #'platform-plan-delete-thread
                                                           thread-id))))
  "Worker thread: remove the thread the kit opened for an ask whose turn
answered in silence, and take the lane with it."
  ;; A silent answer posts nothing — no answer, no ping — so the thread it
  ;; opened to work in would stand empty in the room, named after a question
  ;; nobody was told the fate of.
  ;;
  ;; Only ever a thread THIS host opened for THIS lane and nothing but chrome
  ;; went into (LANE-EMPTY-THREAD): a thread carrying the turn's own words or
  ;; a line somebody typed is the room's, and stays.
  ;;
  ;; The working line lives inside the thread, so a removed thread takes its
  ;; chrome with it and the caller retires none. A refused removal leaves both
  ;; where they are — a stale line the reader can make sense of, rather than a
  ;; hole — and keeps the origin, because a lane still in its thread answers
  ;; without a reference (LANE-REFERENCE).
  ;;
  ;; => T when the thread is gone, and the chrome with it.
  (when (and plan (execution-ok-p (run-plan host lane "thread delete" plan)))
    ;; The room the lane lived in is gone: a later reply to the ask must
    ;; open a lane of its own rather than post into a deleted channel.
    ;; REMOVE-LANE drops the origin with the lane.
    (remove-lane host.lanes lane.session-id)
    t))

(defvar *replied-lines* (make-hash-table :test #'equal :synchronized t)
  "Status message ids someone replied to: retired by an edit, never deleted.")

(defparameter +replied-line-text+ "answered below"
  "What a status line someone replied to says once its turn is over.")

(defun note-replied-line (lane message-id)
  "Remember that a person replied to MESSAGE-ID, when it is LANE's status line."
  (when message-id
    (with-lane-digest (digest lane)
      (when (equal message-id digest.status-id)
        (setf (gethash message-id *replied-lines*) t)))))

(defun retire-chrome (host lane status-id &optional digest)
  "Delete the status line the answer replaced — or, when someone replied to
it, settle it in place."
  ;; Ordered strictly after the answer's post: a failed delete leaves a stale
  ;; line, which the reader can make sense of, where a failed post after a
  ;; delete would leave a hole. A deleted line someone replied to leaves
  ;; their reply pointing at "Original message was deleted".
  ;; A card's message is a card for good (PLAN-EDIT), so the settled line is
  ;; a card that says so.
  (when status-id
    (if (remhash status-id *replied-lines*)
        (edit-card host lane status-id (list :state :done :headline +replied-line-text+) "chrome settle"
                   :controls :clear)
        (run-plan host lane "chrome delete"
                  (host-plan host #'platform-plan-delete lane.target status-id)))
    (bt2:with-lock-held ((lane-lock lane))
      (let ((target-digest (or digest lane.digest)))
        (when (and target-digest
                   (equal status-id target-digest.status-id))
          (setf target-digest.status-id nil))))))

(defun settle-card (host lane digest &aux (now (now-ms)))
  "Worker thread: DIGEST's turn answered — its card, when it earned one,
settles above the answer to how long the work took over its last steps,
Details on it, and keeps what a Details press on it shows."
  ;; The record of the work stays beside what the work said; a card someone
  ;; replied to stays as it is replied to.
  (multiple-value-bind (status-id card controls media)
      (bt2:with-lock-held ((lane-lock lane))
        (values digest.status-id (digest-card digest now) (line-controls lane digest) digest.posted-media))
    (when (and status-id card)
      (remhash status-id *replied-lines*)
      (multiple-value-bind (result landed)
          (edit-card host lane status-id card "card settle" :retry t :controls controls :media media)
        (when (execution-ok-p result)
          (bt2:with-lock-held ((lane-lock lane))
            (digest-status-delivered digest card nil landed))))
      (keep-card-details host lane digest))))

;;; --- the turn's terminal delivery ------------------------------------------------------------

;;; A model that writes [SILENT] or NO_REPLY reaches the same silence as one
;;; that writes SILENT.
(defparameter +silence-spellings+ '("SILENT" "[SILENT]" "NO_REPLY" "NO REPLY")
  "Every spelling of the no-post sentinel the room reads — trimmed and
case-insensitive.")

(defun silent-answer-p (text)
  "Whether TEXT is the channel's no-post sentinel — one of +SILENCE-SPELLINGS+,
trimmed and case-insensitive."
  ;; The sentinel is an output control value, not user-visible answer content.
  ;; Trim transport whitespace so a model's final newline cannot turn silence
  ;; into a Discord message.
  (and (stringp text)
       (member (nlk:trimmed text)
               +silence-spellings+ :test #'string-equal)))

(defun deliver-answer (host lane digest)
  "Worker thread: post one turn's answer as a reply that pings its asker —
in a thread the kit opened, as a post naming the asker once — bind every
chunk as an address for the lane, then settle the card above it."
  ;; The reply goes to the ask THIS turn answered — a steer has already moved
  ;; the lane's trigger on to the next message by the time the cut turn's
  ;; answer lands.
  ;;
  ;; A lane the kit put in a thread of its own answers in the thread and
  ;; nowhere else: the ask's own message already shows the thread under it, so
  ;; a copy in the room the ask was typed in would be the same answer twice.
  ;; The message it answers lives in the parent channel rather than in the
  ;; thread, so the answer carries no reference; its FIRST answer names the
  ;; asker instead — the one ping the ask earns, since the thread opened
  ;; around a person who has not joined it.
  ;;
  ;; The files the turn handed to its answer with ANSWER-FILE ride the
  ;; answer's own message, so a picture shows where the answer shows; the
  ;; choices it handed with ANSWER-CHOICES ride it as one row of buttons.
  ;; An answer its turn marked waiting (ANSWER-WAITS), or that carries
  ;; choices, is written down as waiting beside the messages it posted as
  ;; (NOTE-ANSWER-WAITING), before its card settles.
  ;;
  ;; => the answer text; for a thread lane's first answer, the session id of
  ;; the room its ask was typed in: the second record the exchange settles
  ;; into (FINISH-TURN), so the next ask typed there forks above it; and
  ;; whether the room took the answer.
  (let* ((turn-id digest.turn-id)
         (assistant-facts (and turn-id (nlk:store-open-p)
                               (handler-case
                                   (reverse (nlk:events :turn-id turn-id
                                                        :kind "turn.assistant_message_completed"
                                                        :order :oldest :as :payloads))
                                 (error (condition)
                                   (warn "~a completed answer lookup for ~a failed: ~a"
                                         (host-id host) turn-id condition)
                                   nil))))
         (text (or (digest-final-text digest)
                   ;; The event log is the durable answer; an unreadable store is reported.
                   (some #'fact-message-content assistant-facts)))
         (wordless-completion-p (and assistant-facts (null text)))
         (status-id digest.status-id)
         (origin (lane-thread-origin lane))
         (reply-to (lane-reference lane (or digest.ask-id lane.trigger-message-id)))
         (target lane.target)
         (files (collect-answer-files lane turn-id))
         (choices (take-answer-choices lane turn-id))
         (waits (multiple-value-list (answer-mark lane turn-id))))
    (when (and text (null digest.answer))
      ;; A missed live frame is recoverable from the durable event log. Keep
      ;; the recovered text on the captured digest so write-back and tests read
      ;; the same answer the worker posted.
      (setf digest.answer text))
    (cond
      ((silent-answer-p text)
       ;; SILENT is a control value: do not post it, write it back, or leave
       ;; the temporary working line behind.
       (setf digest.answer nil)
       ;; The thread goes first: an empty one the kit opened for this ask is
       ;; removed whole, and the working line inside it goes with it.
       (unless (retire-thread host lane)
         (retire-chrome host lane status-id digest))
       nil)
      ((null text)
       ;; A completion that spoke and left no words the room can show is a
       ;; failure, never a choice, and never batched with the SILENT sentinel.
       ;; One line says so — and only for a turn that actually spoke; a
       ;; completion that never spoke at all keeps the pinned absence.
       (if wordless-completion-p
           (progn
             (post-message host target
                           "the turn ended without an answer — nothing was posted."
                           :reply-to reply-to :ping nil
                           :what (format nil "no-answer notice for ~a" turn-id))
             ;; The thread stays: the notice is its content now.
             (retire-chrome host lane status-id digest))
           (unless (retire-thread host lane)
             (retire-chrome host lane status-id digest)))
       nil)
      (t
       ;; The answer is the one message a turn posts: what it said on its way
       ;; stayed on its card, so nothing sits between the two to set it apart.
       (let ((landed nil))
         (multiple-value-bind (delivered message-ids error)
             ;; A post that signals is an answer the room never took, and says
             ;; so the way a refusal does. 2026-10-19: an answer's new choice
             ;; could not keep its press past the session state quota, the job
             ;; died in the worker's log, and the room heard nothing: the card
             ;; read working, the ask kept its eye.
             (handler-case
                 (post-message host target text
                               :reply-to reply-to :ping (null origin)
                               :mentions (and origin lane.owner-id (list lane.owner-id))
                               :files files
                               :controls (answer-controls host lane choices))
               (error (condition) (values nil nil (princ-to-string condition))))
           (bind-addresses host lane target message-ids)
           (note-delivery (host-id host) delivered error lane.session-id)
           (when (and delivered choices (not (first waits)))
             (mark-choices-waiting lane turn-id))
           (when (and delivered (or (first waits) choices))
             (note-answer-waiting host (or (getf target :thread-id) (getf target :channel-id))
                                  message-ids (second waits)))
           (if delivered
               (progn
                 (settle-card host lane digest)
                 (when origin
                   (forget-thread-origin lane)
                   (setf landed (getf origin :room))))
               (settle-unposted-answer host lane status-id digest reply-to error))
           (values text landed delivered)))))))

(defun voice-reply-due-p (mode voice)
  "Whether an answer carries a voice message too, in a room whose voice
replies are MODE (+VOICE-REPLIES+), to an ask said VOICE (ASK-VOICE)."
  ;; An ask said in a voice channel is answered out loud there.
  (and (not (eq voice :channel))
       (or (equal mode "tts") (and (equal mode "on") (eq voice :note)))))

(defun post-voice-reply (host target text reply-to)
  "Worker thread: TEXT, an answer that stands in words on TARGET, said as a
voice message there too, a reply to REPLY-TO."
  ;; The words are the answer: a voice that cannot speak costs one line that
  ;; says why, never the answer. A long answer's message says its first part
  ;; (speech.max_characters).
  (nlk:with-handlers ((error (condition)
                        (post-message host target
                                      (format nil "voice: no voice message (~a). ~
                                                   The answer above is the answer." condition)
                                      :what "voice message notice")))
    (call-with-scratch-directory
     (lambda (directory)
       (nlk:bind (((pathname seconds waveform) (voice-message text directory))
                  (plan (or (host-plan host #'platform-plan-file target pathname
                                       :reply-to reply-to
                                       :voice (list :seconds seconds :waveform waveform))
                            (error "~a carries no files" (platform-name host.platform))))
                  (result (execute-plan host.executor plan)))
         (unless result.ok-p
           (error "~a refused it: ~a" (platform-name host.platform) result.error)))))))

(defun settle-unposted-answer (host lane status-id digest reply-to error
                               &aux (text (format nil "the answer could not be posted~%> ~a"
                                                  (digest-note-line error +digest-note-cap+))))
  "Worker thread: the room refused a turn's answer — the card that stood in
for it settles failed and says so, its Stop gone, or, for a turn that earned
no card, one line says so."
  ;; Without this, the line would read `working' with a stop on a turn
  ;; already over, and every press would find nothing to stop. The card stays
  ;; the turn's record: its steps, and Details on it.
  (cond
    (status-id
     (remhash status-id *replied-lines*)
     (multiple-value-bind (card controls media)
         (bt2:with-lock-held ((lane-lock lane))
           (values (card-with (digest-card digest (now-ms)) :state :failed
                              :headline "The answer could not be posted"
                              :note (digest-note-line error +digest-note-cap+))
                   (line-controls lane digest) digest.posted-media))
       (edit-card host lane status-id card "answer refusal settle" :retry t :controls controls :media media))
     (keep-card-details host lane digest)
     (bt2:with-lock-held ((lane-lock lane))
       (when (equal status-id digest.status-id)
         (setf digest.status-id nil))))
    (t (post-message host lane.target text :reply-to reply-to
                     :what (format nil "answer refusal notice for ~a" lane.session-id)))))

(defun recorded-answer-text (digest answer)
  "The assistant half the room records for DIGEST's ask: what the model said
while the turn waited on its own background work, then the answer."
  ;; The turn that ends the work would otherwise record an exchange missing
  ;; everything spoken before the result arrived.
  (prog1 (format nil "~{~a~%~%~}~a" digest.waiting-words answer)
    (setf digest.waiting-words nil)))

(defun write-back-rooms (host lane text typed-room)
  "Fold LANE's exchange, TEXT its answer, into LANE's own room, and into
TYPED-ROOM too — the room the ask was typed in — when that is another room."
  ;; Skipped when TYPED-ROOM is the one the thread hangs in: the own-room
  ;; write-back already carried the exchange there.
  (write-back lane text :budget host.room-tokens)
  (when (and typed-room (not (equal typed-room (room-parent-room lane.parent-session-id))))
    (handler-case (progn (ensure-room host typed-room)
                         (write-back lane text :room typed-room :budget host.room-tokens))
      (error (condition)
        (warn "channel: room write-back to ~a failed: ~a" typed-room condition)))))

(defun finish-turn (host lane digest &aux (reactions (terminal-reactions digest))
                                          (outcome nil))
  "Worker thread: one turn's terminal delivery, then its reactions, room
write-back, and concurrency slot."
  ;; DIGEST is the object folded at terminal time, not a re-read of the lane:
  ;; a reply admitted in the window between the fold and this job replaces
  ;; LANE-DIGEST, and re-reading would deliver the wrong turn's answer or
  ;; none. It also carries the terminal phase and detail, so nothing has to be
  ;; threaded alongside it.
  ;;
  ;; A turn whose own background work is still in flight delivers no answer
  ;; and clears nothing: its last word is spoken as its own message, the line
  ;; the ask is watching stays up saying it is waiting, the working mark stays
  ;; on the ask, and the exit wake's turn answers for real.
  ;;
  ;; A turn a steer cut short keeps the lane's slot: the turn the steer opens
  ;; runs on it, and gives it back when it settles.
  ;;
  (nlk:with-cleanup ((forget-terminal-reactions digest) (digest-forget-activity digest)
                     (setf lane.last-active-ms (now-ms))
                     (unless (eq digest.phase :steered)
                       (promote-next host lane.owner-id)))
    (cond
      (digest.background-pending
       ;; Its word shows on the card the ask watches, the room's record keeps it.
       (let ((text digest.answer))
         (when (and (stringp text)
                    (plusp (length text))
                    (not (silent-answer-p text)))
           (bt2:with-lock-held ((lane-lock lane))
             (digest-note-waiting-words digest text)))))
      ((eq (turn-digest-phase digest) :completed)
       ;; A table in the answer rides it as a picture (TABLES.LISP);
       ;; a draw that cannot happen leaves the fence the transform
       ;; already answered. The room records what the model said, its
       ;; tables as it wrote them: a picture is nothing a later turn can
       ;; read, and one read its own answer back with the list gone.
       (let ((said digest.answer)
             ;; Where the answer's voice message goes is where the answer
             ;; goes, read before the answer moves the lane past its thread's
             ;; first exchange.
             (voice-to (and (voice-reply-due-p
                             (lane-voice-replies host lane.parent-session-id lane.target)
                             lane.voice)
                            (cons lane.target
                                  (lane-reference lane (or digest.ask-id
                                                           lane.trigger-message-id))))))
         (answer-tables-into-files lane digest host.platform)
         (multiple-value-bind (answer typed-room posted) (deliver-answer host lane digest)
           ;; A thread lane's first exchange settles in the room the ask was
           ;; typed in too (DELIVER-ANSWER), so the next ask typed there forks
           ;; above it. An answer the room refused leaves the cross: the ask
           ;; was never answered where it was asked.
           (when answer
             (setf outcome (if posted +reaction-done+ +reaction-failed+))
             (write-back-rooms host lane (recorded-answer-text digest (or said answer)) typed-room))
           ;; Seconds of synthesis, so a job of its own: the slot this turn
           ;; holds goes to the next ask meanwhile.
           (when (and posted voice-to)
             (on-worker host (post-voice-reply host (car voice-to) answer (cdr voice-to)))))))
      ((eq (turn-digest-phase digest) :steered)
       ;; A steer ended the turn at a round boundary, and the lane's next
       ;; turn takes its ask with the steer (NOTE-TURN-FACT): nothing posts —
       ;; not the notice a wordless completion earns, "the turn ended without
       ;; an answer". Its card goes, what it said on its way with it; the next
       ;; turn raises its own.
       (retire-chrome host lane digest.status-id digest))
      (t
       ;; A failed or stopped turn IS its card: the notice settles into
       ;; it, so nothing is posted beside it.
       ;; The ask stays in the record: the rooms its answer would have
       ;; surfaced in keep the exchange, the notice as the assistant
       ;; half, so the next lane forked in the flow sees it — an ask
       ;; that was stopped is still something the room said.
       (flush-now host lane)
       (keep-card-details host lane digest)
       (when (eq (turn-digest-phase digest) :failed)
         (setf outcome +reaction-failed+))
       (nlk:when-let (card (digest-card digest (now-ms)))
         (write-back-rooms host lane (card-text card) (getf (lane-thread-origin lane) :room)))))
    ;; The answer, or the notice, now says what the eye did, and the outcome
    ;; takes its place. A turn waiting on its own background work keeps the
    ;; ask's mark: what the reaction says — this is being worked on — is
    ;; still true.
    (unless digest.background-pending
      (settle-terminal-reactions host lane reactions outcome))))

;;; --- recorded exchanges (a lane's notes to itself) ------------------------------------------
;;; The experience cell folds a reflection's recap back into its session
;;; as a RECORDED exchange: a settled turn nobody asked, marked by
;;; disposition "recorded" on its input fact. A lane delivers the answers
;;; to asks, and a recorded exchange is history — no status line, no typing,
;;; no reaction, no post — while its pair still writes back into the room,
;;; so the note-to-self rides the shared record every later lane forks
;;; above. ANNOUNCED (the recorder said so on the input fact), the exchange
;;; is addressed to the room after all: its answer posts once as one note
;;; under +NOTE-HEAD+, addressing the room's operators — the recap that
;;; needs them @s them.

;;; Live-only fold state, like *HOSTS* — a table rather than a lane slot, so
;;; the kit redefines no structure a running image already holds instances of.
(defvar *recorded-turns* (make-hash-table :test #'eq :synchronized t)
  "The recorded exchange in flight on each lane (LANE -> RECORDED-TURN), if
any: from the input fact that opens it to the terminal fact that settles
it.")

(defstruct (recorded-turn (:copier nil) (:predicate nil)
                           (:constructor make-recorded-turn))
  "One recorded exchange folded on a lane: what identifies it, whether it
announced itself to the room, and its pair's newest assistant text."
  (turn-id nil :type (or null string))
  (input nil :type (or null string))
  (announce-p nil :type boolean)
  (text nil :type (or null string)))

(nlk:access (recorded recorded-turn))

(defparameter +note-head+ "⚠ needs your attention"
  "What a lane's note opens with: the announced recap that must reach the
people in the room.")

;;; --- fact folding (publishing thread) ------------------------------------------------------

(defun note-turn-fact (host lane kind payload turn-id &aux (now (now-ms))
                                                           (ask-id nil)
                                                           (took-over nil)
                                                           (steered nil))
  "Publishing thread: fold one in-flight turn fact into the lane's digest
under the lane lock — no I/O (kit topology rule); the one side effect is
enqueueing a reaction step."
  ;; T when KIND was a fold-only kind; the delivery tick turns the folded
  ;; state into at most one status post/edit.
  (nlk:dispatch kind equal
     ;; The lane's prompt is what THIS turn was asked, verbatim from the
     ;; fact: a steer parked on the lane promotes into a turn of its own,
     ;; and each turn's write-back names its own user half. A turn that
     ;; took an unanswered ask's digest over keeps that ask as the prompt:
     ;; the room records the ask and its answer, not the harness's wake
     ;; line between them.
     ;; The eye set at admission now belongs to this turn — the one that
     ;; settles it when the turn ends — and so do the marks a turn a steer
     ;; cut short handed on.
    ("turn.started" (bt2:with-lock-held ((lane-lock lane))
                      (let* ((previous (lane-digest lane))
                             (fresh-p (and previous
                                           (not (equal turn-id (turn-digest-turn-id previous)))))
                             (awaited (and previous
                                           (not (equal turn-id
                                                       (turn-digest-turn-id previous)))
                                           (turn-digest-background-pending previous)
                                           previous))
                             (digest (if awaited
                                         ;; The turn before this one ended with its own
                                         ;; background work still in flight: the ask is
                                         ;; unanswered and the line it is watching is
                                         ;; still its own, so this turn takes the digest
                                         ;; over rather than opening a second line for one
                                         ;; ask.
                                         (progn
                                           (setf took-over t
                                                 (turn-digest-turn-id awaited) turn-id)
                                           ;; => AWAITED, its background count forgotten.
                                           (setf (turn-digest-background-pending awaited) nil)
                                           awaited)
                                         (lane-turn-digest lane turn-id now))))
                        ;; The ask may have been holding a queued line; admission is what
                        ;; turns it into a running one, and elapsed starts here.
                        (when (eq (turn-digest-phase digest) :queued)
                          (digest-note-admitted digest now))
                        ;; The turn is now the thing that has shown nothing yet: the
                        ;; stall window counts from here.
                        (digest-note-visible digest now)
                        ;; A turn the gate did not admit — a steer promoted into its own
                        ;; turn, a prompt typed into the session from a TUI — answers
                        ;; the message that triggered the lane last.
                        (setf ask-id (or (turn-digest-ask-id digest) (lane-trigger-message-id lane))
                              (turn-digest-ask-id digest) ask-id
                              steered (and fresh-p (eq (turn-digest-phase previous) :steered)))))
                    ;; A turn a steer opened answers the ask the cut turn was
                    ;; given as well: the room records both lines as its ask.
                    (unless took-over
                      (nlk:when-let (input (nlk:json-value payload :string "input"))
                        (setf (lane-prompt lane)
                              (if (and steered (lane-prompt lane))
                                  (format nil "~a~%~a" (lane-prompt lane) input)
                                  input))))
                    (setf (lane-active-turn-id lane) turn-id
                          (lane-last-active-ms lane) now)
                    (when steered
                      (bt2:with-lock-held ((lane-lock lane)) (adopt-reactions lane turn-id)))
                    (schedule-reaction host lane ask-id +reaction-seen+ turn-id))
     ;; The session's parked input, whole — the TUI's pending band, folded
     ;; as ⌎ rows under the running line. The snapshot names its active
     ;; turn; one for a turn this lane is not running is dropped, as a
     ;; delta whose turn does not match is.
    ("session_input_queue_snapshot_updated" (with-lane-digest (digest lane)
                                              (when (equal turn-id (turn-digest-turn-id digest))
                                                (digest-note-pending
                                                 digest
                                                 (loop for prompt across (nlk:json-array payload "snapshot" "pending_prompts")
                                                       for (id text disposition)
                                                         = (mapcar (lambda (key) (nlk:json-value prompt :string key))
                                                                   '("prompt_id" "content" "disposition"))
                                                       collect (list :prompt-id id :text (or text "")
                                                                     :steer-p (equal "steer" disposition)))))))
    ("turn.tool_call_started" (let ((name (nlk:json-value payload :string "tool-name"))
                                    (call-id (nlk:json-value payload :string "call-id"))
                                    (arguments (nlk:json-value payload :string "arguments")))
                                (with-turn-digest (digest lane turn-id now)
                                  (digest-note-tool-call digest)
                                  ;; A step of the turn, its words classified from its
                                  ;; arguments: the card says what the call is doing,
                                  ;; not the tool that runs it.
                                  (digest-note-call-start digest call-id name arguments now))))
     ;; A call landed: its step settles — the result's metadata carries the
     ;; receipts that name what a snippet of free Lisp did, so the step takes the
     ;; words the transcript's row takes at the same moment, and the end of
     ;; what it answered is kept for the card's details.
    ("turn.tool_result" (let ((call-id (nlk:json-value payload :string "call-id"))
                              (metadata (nlk:json-value payload :object "metadata"))
                              (output (nlk:json-value payload :string "result"))
                              (failed (nlk:json-value payload :boolean "failed")))
                          (when call-id
                            (with-turn-digest (digest lane turn-id now)
                              (digest-note-call-result digest call-id now :metadata metadata
                                                                          :output output :failed failed)))))
     ;; The round moved to another model: the card says so, with the
     ;; reason, until the turn settles — the visible half of the durable
     ;; fact the engine's failover policy records.
    ("turn.provider_fallback" (let ((to (nlk:json-value payload :string "to-model"))
                                    (reason (nlk:json-value payload :string "reason")))
                                (when to
                                  (with-turn-digest (digest lane turn-id now)
                                    (setf (turn-digest-fallback-text digest)
                                          (format nil "fell back to ~a~@[ (~a)~]" to
                                                  (and reason (substitute #\Space #\_ reason))))
                                    ;; The round starts over on the candidate.
                                    (digest-note-provider
                                     digest (nlk:json-value payload :string "to-provider"))
                                    (digest-note-round-over digest)))))
     ;; The provider's own count of what the round read, cached, wrote and
     ;; thought, its price, and what it ran on: the card's footer, as the
     ;; TUI's meter and finish divider read the same fact.
    ("turn.usage" (flet ((field (type key) (nlk:json-value payload type key)))
                    (with-turn-digest (digest lane turn-id now)
                      ;; The round is over: the next one counts from nothing.
                      (digest-note-round-over digest)
                      (digest-note-usage
                       digest (list :input (field :integer "input-tokens")
                                    :output (field :integer "output-tokens")
                                    :cached (field :integer "cached-input-tokens")
                                    :cache-write (field :integer "cache-write-tokens")
                                    :reasoning (field :integer "reasoning-tokens")
                                    :cost-known (field :boolean "cost_known")
                                    :cost (field :number "cost_usd")
                                    :estimated (field :boolean "estimated")
                                    :provider (field :string "provider")
                                    :model (field :string "model")
                                    :response-model (field :string "response-model")
                                    :effort (field :string "reasoning-effort")
                                    :finish-reason (field :string "finish-reason"))))))
    ("turn.assistant_message_completed" (let ((text (fact-message-content payload))
                                              (reasoning (fact-message-reasoning payload)))
                                          (with-turn-digest (digest lane turn-id now)
                                            (digest-note-round digest text reasoning)
                                            ;; A round that called tools keeps the turn going: whatever it
                                            ;; said, it said while the work goes on, and its card shows it
                                            ;; (digest.lisp). The round that ENDS the turn carries no calls,
                                            ;; and its text is the answer the turn delivers. SILENT is the
                                            ;; answer's own sentinel and is never said mid-turn.
                                            (digest-note-written digest text
                                                                 (and (fact-message-tool-calls payload)
                                                                      (not (silent-answer-p text))))
                                            (digest-note-visible digest now))))
    (t (return-from note-turn-fact nil)))
  t)

(defun on-delta (host session-id payload &aux (lane (find-lane host.lanes session-id))
                                              (delta (nlk:json-value payload :object "delta"))
                                              (turn-id (nlk:json-value payload :string "turn_id")))
  "Publishing thread (:FRAME hook): reasoning deltas feed the digest's live
thinking tail and, while the section streams, text deltas the words its card
shows the round writing — string append under the lane lock, no I/O."
  ;; This is the lane's only stream ephemera, and it never CREATES turn state:
  ;; a delta whose turn does not match the lane's open digest is dropped,
  ;; leaving the durable facts as the sole authority on which turn a lane is
  ;; running.
  (when (and lane delta (gethash "type" delta))
    (with-lane-digest (digest lane)
      (when (equal turn-id digest.turn-id)
        ;; Any typed part is the turn showing something — text and
        ;; reasoning alike clear the stall window; reasoning also feeds
        ;; the note. A retried round voids what its failed attempt wrote.
        (let ((now (now-ms)))
          (flet ((chars (key)
                   (let ((value (gethash key delta)))
                     (if (stringp value) (length value) 0))))
            (digest-note-visible digest now)
            ;; Every streamed word counts on the card's flow (ROUND-FLOW).
            (nlk:dispatch (gethash "type" delta) equal
              ("reasoning" (digest-note-thinking digest (gethash "text" delta))
                           (digest-note-streamed digest (chars "text") now))
              ("text" (when (host-stream-p host) (digest-note-writing digest (gethash "text" delta)))
                      (digest-note-streamed digest (chars "text") now))
              ("tool_call" (let ((index (gethash "index" delta)))
                             (when (integerp index)
                               (digest-note-call-streamed digest index (chars "arguments") now))))
              ("stream_reset" (digest-note-written digest nil nil)
                              (digest-note-round-over digest)))))))))

(defun terminal-phase (kind payload)
  "(values PHASE DETAIL): the phase the terminal turn fact KIND settles a
digest into, and the detail its PAYLOAD carries — NIL for any other kind."
  ;; A completion a parked steer cut short at a round boundary is :STEERED:
  ;; the turn the steer opens next on the lane answers for both.
  (cond ((equal kind "turn.completed")
         (if (eq t (nlk:json-value payload :boolean "steered")) :steered :completed))
        ((equal kind "turn.failed")
         (values :failed (nlk:json-value payload :string "detail")))
        ((equal kind "turn.cancelled")
         (values :cancelled (nlk:json-value payload :string "reason")))))

(defun on-fact-with-turn (host session-id kind payload metadata
                          &optional turn-id)
  "Fact entry off the :FRAME hook."
  ;; TURN-ID rides the fact metadata / envelope; it keys the lane's per-turn
  ;; digest.
  (let* ((turn-id (or turn-id (nlk:json-value metadata :string "turn_id")))
         (lane (find-lane host.lanes session-id))
         (recorded (gethash lane *recorded-turns*)))
    (cond
      ;; A turn starting on a lane the reaper let go — a cron fire, a
      ;; background job's wake an hour on — is taken up where the lane's id
      ;; says it lives, and answered there, where before it answered nobody.
      ((and (null lane) (equal kind "turn.started") (adopt-lane host session-id))
       (on-fact-with-turn host session-id kind payload metadata turn-id))
      ;; A laneless turn.failed still surfaces where its session id says it belongs.
      ((null lane)
       (when (equal kind "turn.failed")
         (let ((target (session-target (platform-session-prefix
                                        host.platform)
                                       session-id))
               (detail (nlk:json-value payload :string "detail")))
           (when target
             (on-worker host
               (post-message host target (format nil "turn failed: ~a" (or detail "unknown error"))
                             :what (format nil "failure notice for ~a" session-id)))))))
      ;; A recorded exchange's opening input, or a later fact of its turn. No I/O —
      ;; the terminal fact enqueues the one job that settles it.
      ((or (and (equal kind "turn.input_committed")
                (equal "recorded" (nlk:json-value payload :string "disposition")))
           (and recorded turn-id
                (equal turn-id (recorded-turn-turn-id recorded))))
       (cond
         ((equal kind "turn.input_committed")
          (setf (gethash lane *recorded-turns*)
                (make-recorded-turn
                 :turn-id turn-id
                 :input (nlk:json-value payload :string "message")
                 :announce-p (eq t (nlk:json-value payload :boolean "announce")))))
         ((equal kind "turn.assistant_message_completed")
          (nlk:when-let (text (fact-message-content payload)) (setf recorded.text text)))
         ((member kind '("turn.completed" "turn.failed" "turn.cancelled") :test #'equal)
          (remhash lane *recorded-turns*)
          (when (equal kind "turn.completed")
            (on-worker host
              (nlk:when-let (text recorded.text)
                (write-back lane text :prompt recorded.input)
                (when recorded.announce-p
                  ;; The one post that addresses the room's operators: a recap
                  ;; that needs them @s them. No reply; nothing else pings.
                  (let ((target lane.target) (what (format nil "note for ~a" lane.session-id)))
                    (bind-addresses
                     host lane target
                     (nth-value 1 (post-message host target (format nil "~a~%~%~a" +note-head+ text)
                                                :mentions host.owners :what what)))))))))))
      ((note-turn-fact host lane kind payload turn-id))
      ;; A terminal fact enqueues the finish job, the only path that posts turn output.
      (t
       (multiple-value-bind (phase detail) (terminal-phase kind payload)
         (when phase
           (let* ((now (now-ms))
                  (background-pending
                    (and (eq phase :completed)
                         (nlk:json-value payload :integer "background-pending")))
                  (digest
                    (with-turn-digest (digest lane turn-id now)
                      (setf lane.active-turn-id nil)
                      (if background-pending
                          ;; The work it started is still going: the
                          ;; digest stays running, waiting on it.
                          (digest-note-background-pending digest background-pending)
                          (digest-note-terminal digest phase detail now))
                      (setf (terminal-reactions digest)
                            (if (eq phase :steered)
                                (disown-reactions lane turn-id)
                                (terminal-reaction-snapshot lane turn-id)))
                      digest)))
             (on-worker host (finish-turn host lane digest)))))))))

(defun observe-frame (host op &aux (session-id (frame-session-id op)))
  "The host's :FRAME hook body."
  ;; Every live frame of every session passes here on its publishing thread; a
  ;; frame whose session has no lane is a laneless notice at most. Fold +
  ;; enqueue only — never a platform call.
  (multiple-value-bind (kind payload metadata turn-id) (frame-fact op)
    (cond (kind (on-fact-with-turn host session-id kind payload metadata turn-id))
          ((equal "organism.note" (getf op :kind))
           ;; A note the organism says once to its operator, in the model's
           ;; words (a boot that is back, a release it runs for the first
           ;; time: NLE::SAY-NOTE), goes where the bot reports.
           (nlk:when-let (text (nlk:json-value (getf op :payload) :string "text"))
             (say-note-home host text)))
          ((frame-head-moved-p op)
           ;; The chatter held for the next ask goes with the record it
           ;; followed.
           (drain-ambient host session-id)
           (retire-lanes host (lambda (room) (equal room session-id))))
          (t (nlk:when-let (delta (frame-delta op)) (on-delta host session-id delta))))))

(defun retire-lanes (host room-p)
  "Retire every lane forked from a room ROOM-P takes: a settled one is
dropped now, one still running answers its turn and goes with the next reap."
  ;; Any head move counts, from any surface: a /new or an /undo typed in the
  ;; room, a rewind from a shell attached to the room's session. Without this a
  ;; cleared room would change nothing for up to +LANE-IDLE-REAP-MS+ wherever
  ;; every line talks to the surface's live lane (CONVERSATION-LANE) — a thread,
  ;; a DM: /new would answer "cleared" and the next line would run on the
  ;; old fork with everything it had just cleared. An ask still waiting at the
  ;; gate has not forked yet, and forks the room as it stands: it stays. A room
  ;; handed to another agent retires its lanes the same way (AGENT-COMMAND).
  ;; Lane table only — no I/O.
  (map-lanes host.lanes
             (lambda (lane)
               (when (and (funcall room-p lane.parent-session-id)
                          (nlk:session-exists-p lane.session-id))
                 (if (lane-live-p lane)
                     (setf (lane-retired lane) t)
                     (remove-lane host.lanes lane.session-id))))))

(defun bind-admitted-turn (lane admission
                           &aux (turn-id (nlk:active-input-admission-turn-id admission))
                             (stopped nil))
  "LANE's line watches the turn the engine says its ask started -- ADMISSION
names it -- from the admission on, not from that turn's first frame."
  ;; A line opened at admission learned its turn only from TURN.STARTED's
  ;; frame, and a line with no turn is one nothing ends: RECONCILE-TERMINAL-
  ;; DIGEST finds a missed ending by turn id, Stop cancels by turn id, and the
  ;; reaper skips a line that reads running. A frame already folded has bound
  ;; it, and this changes nothing. A line Stop settled while the ask was still
  ;; being read in (STOP-TURNLESS-LINE) cancels the turn it went on to start.
  (bt2:with-lock-held ((lane-lock lane))
    (let ((digest lane.digest))
      (when (and digest (null digest.turn-id) (null lane.active-turn-id))
        (case digest.phase
          (:running (setf digest.turn-id turn-id lane.active-turn-id turn-id))
          (:cancelled (setf stopped (or digest.detail "stopped from the room")))))))
  (when stopped
    (nlk:request-cancel-turn lane.session-id turn-id stopped)))

(defun stop-turnless-line (host lane detail &aux (reactions '()))
  "Settle LANE's line when it reads running with no turn behind it -- an
admission that started none -- DETAIL on the card, the ask's mark off and the
slot back. => T when there was such a line."
  ;; Stop is the room's one way to end a line, and it cancelled by turn id
  ;; alone: on a line with none it warned and left it running (2026-10-20,
  ;; the 'are you here?' card, pressed and still working an hour later).
  (when (bt2:with-lock-held ((lane-lock lane))
          (let ((digest lane.digest))
            (when (and digest (eq digest.phase :running) (null digest.turn-id)
                       (null lane.active-turn-id))
              (digest-note-terminal digest :cancelled detail (now-ms))
              (setf reactions (terminal-reaction-snapshot lane nil))
              t)))
    (setf lane.last-active-ms (now-ms))
    (flet ((settle ()
             (flush-now host lane)
             (settle-terminal-reactions host lane reactions nil)
             (promote-next host lane.owner-id)))
      (unless (and host.worker (on-worker host (settle)))
        (settle)))
    t))

(defun drop-duplicate-admission (host lane admission
                                 &aux (turn-id (nlk:active-input-admission-turn-id admission))
                                      (status-id nil) (reactions '()))
  "An ask the engine refused as one it had already admitted -- ADMISSION
names the turn its command id ran -- runs nothing: the line this delivery
opened settles unseen, a thread it opened goes with the lane, the ask's
working mark comes off, and the slot goes back."
  ;; 2026-10-20: a thread deleted under an answered ask made Discord restate
  ;; the ask after its lane was reaped, and the kit admitted it again under
  ;; its spent command id. Nothing read the engine's :DUPLICATE: the card
  ;; waited on the provider for five hours, Stop found no turn to stop, and
  ;; the reaper never takes a lane whose line still reads running. :COMPLETED
  ;; is the settle that draws nothing (DIGEST-STATUS-PLAN): the ask's own turn
  ;; already ended, and said so where it ran.
  (bt2:with-lock-held ((lane-lock lane))
    (let ((digest lane.digest))
      (when (and digest (null digest.turn-id) (member digest.phase '(:queued :running)))
        (digest-note-terminal digest :completed
                              (format nil "already admitted as ~a" turn-id) (now-ms))
        (setf status-id digest.status-id
              reactions (terminal-reaction-snapshot lane nil)))))
  (warn "~a: ~a was already admitted as ~a; nothing runs"
        (host-id host) lane.session-id turn-id)
  (flet ((settle ()
           (unless (retire-thread host lane)
             (retire-chrome host lane status-id))
           (settle-terminal-reactions host lane reactions nil)
           (setf lane.last-active-ms (now-ms))
           (promote-next host lane.owner-id)))
    (unless (and host.worker (on-worker host (settle)))
      (settle))))

(defun fail-lane (host lane message)
  "Worker job: a refused admission surfaces in the channel — fail clearly,
never silently — and gives the slot back."
  (let ((reactions
          (bt2:with-lock-held ((lane-lock lane))
            (let ((digest (or lane.digest
                              (lane-open-digest lane (now-ms)))))
              (setf digest.ask-id (or digest.ask-id lane.trigger-message-id))
              (digest-note-terminal digest :failed message (now-ms))
              (terminal-reaction-snapshot lane digest.turn-id)))))
    (flush-now host lane)
    (settle-terminal-reactions host lane reactions +reaction-failed+)
    (setf lane.last-active-ms (now-ms))
    (promote-next host lane.owner-id)))

;;; --- ingress (transport thread) ------------------------------------------------------------

(defun operator-p (host user-id)
  "Whether USER-ID is one of the room's operators."
  ;; NIL for a missing id: authority is never assumed.
  (and (stringp user-id)
       (member user-id host.owners :test #'string=)
       t))

(defun candidate-operator-p (host candidate)
  (operator-p host (source-field candidate "user_id")))

(defun candidate-said (host candidate &aux (strip (platform-strip-mention host.platform))
                                           (raw (or (gethash "text" candidate) "")))
  "CANDIDATE's text as the model reads it: our own mention stripped, trimmed."
  (nlk:trimmed (if strip (funcall strip raw) raw)))

(defun host-speaker-line (host candidate &key cap attachments reply-context)
  "(values TEXT ANCHORS): the candidate's room line."
  ;; REPLY-CONTEXT also
  ;; renders the message it answers under it — the ask must carry what was said
  ;; to it, and what it carried: a voice note's words, a picture, a file.
  ;; ANCHORS are (POSITION . ATTACHMENTS) within TEXT: ATTACHMENTS at the end of
  ;; the speaker's own line, above the reply context, and the files of the
  ;; message it answers at the end of that message's line.
  (let* ((line (speaker-line candidate
                             :cap cap
                             :attachments attachments
                             :operator-p (candidate-operator-p host candidate)
                             :strip (platform-strip-mention host.platform)))
         (reply (and reply-context (candidate-reply candidate)))
         (context (and reply
                       (reply-context-line
                        reply
                        :operator-p (operator-p host (gethash "user_id" reply)))))
         (text (if (and line context)
                   (format nil "~a~%~a" line context)
                   line))
         (answered (and line context (candidate-attachments reply))))
    (values text
            (append (and line attachments (list (cons (length line) attachments)))
                    (and answered (list (cons (length text) answered)))))))

(defun observe-candidate (host candidate)
  "Room chatter that was not addressed to us: buffered as context for the
next ask."
  ;; Never its own turn — one room turn is one user message and one assistant
  ;; message, and a strict chat template rejects the consecutive-user shape a
  ;; chatter turn would leave behind. A recording rides with its line, read in
  ;; only when an ask carries the chatter: a voice note is what a room says as
  ;; much as a typed line is, and a question about it comes after.
  ;; NOTE-AMBIENT buffers nothing for a candidate with no line.
  (let* ((recordings (remove-if-not #'recording-attachment-p
                                    (candidate-attachments candidate)))
         (line (host-speaker-line host candidate :cap +room-ambient-line-cap+
                                                 :attachments recordings)))
    (note-ambient host
                  (room-session-id (platform-session-prefix host.platform) candidate)
                  (if (and line recordings) (cons line recordings) line))))

(defun attachment-key (attachment)
  "What makes two attachment entries one file: the platform's id for it,
else its url."
  (or (gethash "id" attachment) (gethash "url" attachment)))

(defun ambient-without (ambient attachments)
  "AMBIENT with the recordings among ATTACHMENTS taken off its entries: a
voice note the ask itself carries — the message an edit made an ask, the
note a reply answers — is read in once, where the ask names it, and its
chatter line stays as the record that it was said."
  (let ((keys (remove nil (mapcar #'attachment-key attachments))))
    (if (null keys)
        ambient
        (loop for entry in ambient
              collect (if (consp entry)
                          (nlk:if-let (kept (remove-if (lambda (attachment)
                                                         (member (attachment-key attachment) keys
                                                                 :test #'equal))
                                                       (cdr entry)))
                            (cons (car entry) kept) (car entry))
                          entry)))))

(defun press-key (host candidate)
  "The submission key of a press on a choice (ASK-IDENTITY): the card it
answered and the line it said. NIL for a message, whose id is its own key."
  ;; A press is no message. Without a key of its own every press in a lane
  ;; would share one command id, which the engine dedupes on: each press
  ;; after the lane's first would read as that first one arriving twice --
  ;; admitted as a duplicate, no turn started, its card waiting on the
  ;; provider until a typed line came. A card is answered once (ANSWER-CARD
  ;; takes its choices away), so the card and the line name one press, and
  ;; the same press delivered twice is still one ask.
  (when (and (candidate-pressed-p candidate)
             (null (source-field candidate "message_id")))
    (format nil "press-~a-~a"
            (or (source-field candidate "reply_to_message_id") "")
            (nlk:short-digest (candidate-said host candidate) 8))))

(defun build-ask (host candidate lane-session-id continue-p
                  &key ambient-room)
  "The ask CANDIDATE opens, under LANE-SESSION-ID."
  ;; CONTINUE-P marks it as the next turn of a lane that exists. AMBIENT-ROOM
  ;; names where the chatter that rides in with the ask is taken from,
  ;; defaulting to its own room: a thread the kit just opened for an ask takes
  ;; the PARENT channel's, because the thread's own record starts with this
  ;; very ask. Nothing else rides in: the conversation the question was born
  ;; out of reaches the lane by the fork — the room chain composes it message
  ;; by message, each in its own user turn — never as text pasted into this
  ;; ask. The contract says how the room reaches the lane — every line, or
  ;; only a mentioned one — from the fact the mention gate turned on
  ;; (ROOM-SPEAKS-FREELY-P); the line itself is never marked. A
  ;; continuation runs as its lane does, whoever replied (AGENT-FOR names
  ;; the agent for a new one).
  (let* ((platform host.platform)
         (room (room-session-id platform.session-prefix candidate))
         (lane (and continue-p (find-lane host.lanes lane-session-id)))
         (agent (if lane
                    lane.agent
                    (values (agent-for host room (channel-target candidate) (candidate-source candidate)))))
         (ambient (drain-ambient host (or ambient-room room))))
    (multiple-value-bind (ask-line line-anchors)
        (host-speaker-line host candidate :attachments (candidate-attachments candidate)
                                          :reply-context t)
      (multiple-value-bind (prompt anchors)
          (compose-prompt (ambient-without ambient (reduce #'append line-anchors :key #'cdr))
                          (or ask-line "")
                          :anchors line-anchors)
        (make-ask
         :room room
         :lane lane-session-id
         :owner-id (source-field candidate "user_id")
         :message-id (source-field candidate "message_id")
         :key (press-key host candidate)
         :target (channel-target candidate)
         :attachments anchors
         :contract (lane-contract
                    :name platform.name
                    :noun platform.noun
                    :owner-label platform.owner-label
                    :owners host.owners
                    :speaks-freely (room-speaks-freely-p host.policy candidate)
                    :api-primer platform.api-primer
                    :seams-primer platform.seams-primer
                    :standing (agent-standing (agent-of host agent) platform.noun))
         :where (funcall platform.where-text candidate)
         :prompt prompt
         :continue-p continue-p
         :voice (cond ((source-field candidate "voice_origin") :channel)
                      ((some #'recording-attachment-p (candidate-attachments candidate)) :note))
         :agent agent
         :said (candidate-said host candidate))))))

;;; --- inbound attachments ---------------------------------------------
;;; What a person attaches reaches the lane as what it is: an image as an
;;; image, a recording as the words it says, a text file as its text, and any
;;; other file — a video, a PDF, an archive, a log too long to read whole —
;;; as a file on this machine, its path in the prompt for the lane's own tools
;;; to open. The
;;; candidate carries each file
;;; by reference — the url to fetch, or, where a platform reaches its files
;;; with the adapter's own credential, a thunk that returns the bytes — and
;;; the ask carries the references to its admission, anchored where the
;;; message that brought them sits in its prompt: the asker's own line, or a
;;; voice note in the room's chatter (COMPOSE-PROMPT). There each file is
;;; fetched and its bytes sniffed, never the platform's word taken: an image
;;; becomes the (MEDIA-TYPE . BASE64) part SUBMIT takes, named by the same
;;; [Image #N] marker the composer writes, so the marker and the pixels read
;;; in one round; a recording becomes its transcript (TRANSCRIBE-AUDIO); a
;;; file that is neither is saved and named, never described, and every
;;; failure is one honest bracketed note. The fetch is network I/O and a
;;; transcription seconds of CPU, and the threads that admit asks — a
;;; platform's reader, the websocket lap — are the ones that heartbeat, so it
;;; runs on the delivery pool (RUN-ASK-INGRESS).

(defparameter +ask-image-timeout-seconds+ 20
  "How long one attachment's fetch may take before the line that stands for
it says it did not arrive.")

(defparameter +ask-attachments-taken+ 4
  "Files read from one message: a message is not a gallery, and every image
rides every remaining round.")

(defparameter +ask-recordings-taken+ 4
  "Recordings transcribed for one ask, the chatter's voice notes and its own
together: each is seconds of CPU on the delivery pool.")

(defparameter +document-max-bytes+ (* 100 1024)
  "The largest text file an ask reads whole into its prompt — Hermes' ceiling:
past it a file is noted, and the lane can still fetch it over the platform's
API.")

(defparameter +file-max-bytes+ (* 32 1024 1024)
  "The largest file an ask brings down to this machine — Hermes' ceiling:
past it a file is noted, and never fetched.")

(nlk:define-startup-parameter *attachment-directory*
    (nlk:cache-path "nodecode/channel-files/")
  "Where an attached file the prompt cannot hold is saved for the lane's
tools: with the cache, apart from what must be kept; tests bind a scratch
folder.")

(defun save-attachment (attachment octets &aux (name (or (attachment-name attachment) "file")))
  "OCTETS written under *ATTACHMENT-DIRECTORY*, named for ATTACHMENT's id and
its own name made safe for a path. => the file's native path."
  (let ((path (merge-pathnames
               (format nil "~a-~a"
                       (or (gethash "id" attachment) (random (expt 2 40)))
                       (nlk:clip (ppcre:regex-replace-all "[^A-Za-z0-9._-]" name "_") 80
                                 :ellipsis ""))
               *attachment-directory*)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence octets out))
    (uiop:native-namestring path)))

(defun document-text-of (octets)
  "OCTETS as text when they decode as UTF-8 and hold no NUL — what a text
file is — with a leading byte-order mark dropped; NIL for anything else."
  (let ((text (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
                (error () nil))))
    (and text
         (not (find (code-char 0) text))
         (string-left-trim (list (code-char #xFEFF)) text))))

(defun document-prompt-text (attachment text)
  "The lines a text file leaves in the prompt: its name, then its content
fenced whole — the fence one backtick longer than any run inside, so the
file's own fences stay its own."
  (let* ((longest (loop with run = 0 and best = 0
                        for ch across text
                        do (setf run (if (char= ch #\`) (1+ run) 0)
                                 best (max best run))
                        finally (return best)))
         (fence (make-string (max 3 (1+ longest)) :initial-element #\`)))
    (format nil "[File~@[ \"~a\"~]]~%~a~%~a~%~a"
            (attachment-name attachment) fence (string-right-trim '(#\Newline) text) fence)))

(defun url-octets (url &key (timeout +ask-image-timeout-seconds+))
  "URL's bytes, fetched whole and binary — never decoded as text, never
sniffed into a character encoding, whatever the response calls itself."
  ;; The one fetch the kit makes outside a platform's own files: the inbound
  ;; attachment path and FETCH-IMAGE both come through here.
  (dex:get url :force-binary t :read-timeout timeout :connect-timeout 10))

(defun read-attachment (attachment)
  "(values KIND DATA NOTE): ATTACHMENT's bytes — fetched from its url, or
the bytes its fetch thunk returns — and what they are, sniffed from the
bytes themselves: :IMAGE with the (MEDIA-TYPE . BASE64) part one turn
carries, when it is under the ceiling one image may take; :AUDIO with
(OCTETS . CONTAINER), under the ceiling one transcription takes; :TEXT with
the text a file declared as one holds, whole under its ceiling; :FILE with
(PATH . BYTES), where any other file was saved; else NIL and the note saying
why none."
  ;; A size the platform declares over the ceiling a file may take is refused
  ;; before anything is fetched. Never signals: an attachment that did not
  ;; arrive or a file too large is one honest line in the prompt.
  (nlk:with-handlers ((error (condition)
                        (values nil nil (princ-to-string condition))))
    (let ((size (gethash "size" attachment)))
      (when (and (integerp size) (> size +file-max-bytes+))
        (error "the platform reports ~:d bytes, over the ~:d-byte ceiling a file may take"
               size +file-max-bytes+))
      (let ((octets (let ((url (gethash "url" attachment))
                          (fetch (gethash "fetch" attachment)))
                      (cond (url (url-octets url))
                            ((functionp fetch) (funcall fetch))
                            (t (error "the attachment carries nothing to fetch"))))))
        (if (not (typep octets '(vector (unsigned-byte 8))))
            (values nil nil "the platform did not answer with the file's bytes")
            (let* ((bytes (length octets))
                   (media-type (nle:image-media-type octets))
                   (container (audio-container octets))
                   (text (and (document-attachment-p attachment)
                              (<= bytes +document-max-bytes+)
                              (document-text-of octets))))
              (cond
                ((and media-type (<= bytes nle:*image-max-bytes*))
                 (values :image
                         (cons media-type (cl-base64:usb8-array-to-base64-string octets))
                         nil))
                ;; A video's container holds sound too; a video file is
                ;; opened, not heard.
                ((and container (<= bytes +audio-max-bytes+) (not (video-file-p attachment)))
                 (values :audio (cons octets container) nil))
                (text (values :text text nil))
                ((> bytes +file-max-bytes+)
                 (values nil nil (format nil "~:d bytes, over the ~:d-byte ceiling a file may take"
                                         bytes +file-max-bytes+)))
                (t (values :file (cons (save-attachment attachment octets) bytes) nil)))))))))

(defun attachment-name (attachment &aux (name (gethash "filename" attachment)))
  ;; The sender picked it, and it lands inside the room record: one line.
  (and (stringp name) (plusp (length (room-line name))) (room-line name)))

(defun recording-text (attachment data index &aux (declared (gethash "seconds" attachment)))
  "The text a recording leaves in the prompt: its transcript under a
[Audio #INDEX, length, transcribed] mark, or the one note saying why it has
none. DATA is READ-ATTACHMENT's (OCTETS . CONTAINER)."
  (nlk:with-handlers ((error (condition)
                        (format nil "[the recording ~@[\"~a\" ~]could not be transcribed: ~a]"
                                (attachment-name attachment) condition)))
    (nlk:bind (((text seconds) (transcribe-audio (car data) (cdr data)
                                                 :seconds (and (realp declared) declared)))
               (length (duration-text (or seconds declared))) (text (room-line text)))
      (if (plusp (length text))
          (format nil "[Audio #~d~@[, ~a~], transcribed] \"~a\"" index length text)
          (format nil "[Audio #~d~@[, ~a~], transcribed: no speech heard]"
                  index length)))))

(defun ask-prompt-and-attachments (ask &aux (anchors ask.attachments) (prompt ask.prompt))
  "(values PROMPT IMAGES): ASK's prompt with each file read in at its anchor
— an [Image #N] marker for an image, the transcript for a recording, a text
file's content fenced under its name, a bracketed note for a file that is
none of those or did not resolve — and the
(MEDIA-TYPE . BASE64) parts SUBMIT takes, in the order the images arrived."
  ;; A recording whose declared length is refused is noted before it is
  ;; fetched.
  (let ((parts '())
        (images 0)
        (recordings 0)
        (cursor 0))
    (flet ((texts (attachments &aux (taken (min (length attachments) +ask-attachments-taken+)))
             (append
              (loop for attachment in (subseq attachments 0 taken)
                    for name = (attachment-name attachment)
                    for refusal = (and (recording-attachment-p attachment)
                                       (transcription-refusal
                                        (gethash "seconds" attachment)))
                    collect
                    (if refusal
                        (format nil "[the recording ~@[\"~a\" ~]could not be transcribed: ~a]"
                                name refusal)
                        (multiple-value-bind (kind data note) (read-attachment attachment)
                          (case kind
                            (:image (push data parts)
                             (format nil "[Image #~d]" (incf images)))
                            (:text (document-prompt-text attachment data))
                            (:file
                             (format nil "[File~@[ \"~a\"~] (~@[~a, ~]~:d bytes) saved at ~a: not ~
                                          read in; open it with your tools]"
                                     name (gethash "media_type" attachment) (cdr data) (car data)))
                            (:audio
                             (if (< recordings +ask-recordings-taken+)
                                 (recording-text attachment data (incf recordings))
                                 (format nil "[the recording ~@[\"~a\" ~]was not transcribed: ~
                                                ~d already were for this ask]"
                                         name +ask-recordings-taken+)))
                            (t (format nil "[the attachment ~@[\"~a\" ~]could not be read: ~a]"
                                       name note))))))
              (and (> (length attachments) taken)
                   (list (format nil "[~d more attachments arrived and were not read]"
                                 (- (length attachments) taken)))))))
      (values (with-output-to-string (out)
                (dolist (anchor anchors)
                  (write-string prompt out :start cursor :end (car anchor))
                  (setf cursor (car anchor))
                  (format out "~{ ~a~}" (texts (cdr anchor))))
                (write-string prompt out :start cursor))
              (nreverse parts)))))

(defun run-ask-ingress (host ask thunk)
  "Run THUNK — which reads ASK's attachments in and admits its turn — on the
delivery pool when ASK carries attachments, inline when it carries none:
the fetch and the transcription are the only slow work on this path, and
the thread that admitted the ask owns a platform connection that heartbeats."
  ;; A pool that refuses the job runs it here instead, rather than dropping
  ;; the ask.
  (unless (and ask.attachments host.worker (delivery-worker-enqueue host.worker thunk))
    (funcall thunk)))

;;; --- borrowing an image from the web ------------------------------------
;;; An answer that illustrates borrows a real picture rather than describing
;;; one: FETCH-IMAGE brings the image at a URL down as a file a lane can
;;; post — its bytes sniffed, so a page or a video refuses with the reason,
;;; and under the ceiling one image may take. The posting half is POST-FILE's;
;;; this half only makes the file.

(nlk:define-startup-parameter *image-cache-directory*
    (nlk:cache-path "nodecode/channel-images/")
  "Where FETCH-IMAGE writes what it brings down: with the cache, apart from
what must be kept; tests bind a scratch folder.")

(defun fetch-image (url &key filename (timeout +ask-image-timeout-seconds+))
  "(values PATHNAME MEDIA-TYPE): the image at URL, brought down as a file
ready to post — its bytes whole and binary, sniffed rather than trusted to
the name, written under *IMAGE-CACHE-DIRECTORY* as FILENAME or the URL names
it, so a repeat call for one URL lands on one file."
  ;; Refuses, with the reason, what a picture must not be: a page, a video,
  ;; bytes this cannot open, an image over the ceiling. The borrow half of an
  ;; answer's picture: this brings it down, POST-FILE posts it.
  (let* ((octets (url-octets url :timeout timeout))
         (binary-p (typep octets '(vector (unsigned-byte 8))))
         (media-type (and binary-p (nle:image-media-type octets))))
    (unless media-type
      (error "~a did not answer with an image~@[ (~:d bytes)~]"
             url (and binary-p (length octets))))
    (when (> (length octets) nle:*image-max-bytes*)
      (error "the image at ~a is ~:d bytes, over the ~:d-byte ceiling one image may take"
             url (length octets) nle:*image-max-bytes*))
    (let* ((name
             ;; FILENAME, else the URL's own name (or "image") kept apart by the URL's hash.
             (if (and (stringp filename) (plusp (length (string-trim " " filename))))
                 filename
                 ;; MEDIA-TYPE is one of the four IMAGE-MEDIA-TYPE sniffs.
                 (let* ((extension (if (equal media-type "image/jpeg") "jpg" (subseq media-type 6)))
                        (tail (subseq url (1+ (or (position #\/ url :from-end t) -1))))
                        (raw (if (ppcre:scan "\\A[\\w.-]+\\z" tail) tail "image"))
                        (base (ppcre:regex-replace (format nil "(?i)\\.~a\\z" extension) raw "")))
                   (format nil "~a-~a.~a" base (nlk:short-digest url) extension))))
           (path (merge-pathnames name *image-cache-directory*)))
      (ensure-directories-exist path)
      (values (write-octets octets path) media-type))))

;;; --- the room a target hangs in --------------------------------------------------------

(defun room-parent-session-id (host target &aux (channel (getf target :channel-id)))
  "The room TARGET hangs in, named the way ROOM-SESSION-ID names rooms: the
channel a thread posts to, or NIL for a target that names no thread — a
channel room hangs in nothing and forks nothing."
  ;; A thread is a surface inside a channel wherever the platform opens them,
  ;; and this is the room its own record starts from.
  (and (getf target :thread-id) channel
       (format nil "~a-~a" (platform-session-prefix host.platform)
               channel)))

;;; --- commands (transport thread in, worker thread out) ---------------------------------

(defun slash-line-p (text)
  "Whether TEXT is a slash line: `/' then a letter."
  ;; `/etc/hosts is broken' reads as a command too and answers as an unknown
  ;; one, the shells' rule — a message meant as a prompt loses its leading
  ;; slash.
  (and (stringp text)
       (> (length text) 1)
       (char= #\/ (char text 0))
       (alpha-char-p (char text 1))))

(defvar *command-source* nil
  "While a room's slash command runs, the source of the message that ran it
— who and where, as the adapter normalized it (SOURCE-FIELD's hash); NIL for
a command run anywhere else.")

(defvar *command-card* nil
  "While a room's slash command runs, what its answer carries beside its
words (OFFER-CARD): (:CONTROLS CONTROLS :PANEL PANEL).")

(defun offer-card (&key controls panel)
  "Have the answer to the slash command running in a room carry CONTROLS —
rows of CHOICEs and menus of MENU-CHOICEs, whose presses say their lines —
and show as PANEL where the platform draws one: (:TITLE :TEXT :FIELDS
:TONE), a title, its words, (NAME . VALUE) fields and a tone of the card
colours (:DONE :STOPPED :WORKING :FAILED), in place of the command's words,
which stay the answer everywhere else. A command run anywhere else answers
in its words alone. => CONTROLS."
  (when *command-source* (setf *command-card* (list :controls controls :panel panel)))
  controls)

(defun run-command (host candidate line)
  "Worker thread: the answer to one slash LINE from CANDIDATE, who may run
it (COMMAND-MESSAGE asked), as (values TEXT CONTROLS PANEL) — CONTROLS and
PANEL when the answer is a card (OFFER-CARD)."
  ;; The room is ensured first, so a session-scoped command has its session;
  ;; every failure is an answer, never a signal — a channel never learns of a
  ;; refusal from a log. /models is the room's own: a pick sets this room's
  ;; model, never the organism's (ROOM-MODELS-COMMAND). Any other command's
  ;; answer is COMMAND-ANSWER's; a press that asks before its line runs
  ;; (CONFIRM-CARD) runs nothing yet.
  (multiple-value-bind (name args) (nle:parse-slash-input line)
    (nlk:with-handlers ((error (condition)
                          (format nil "/~a: ~a" name condition)))
      (let* ((target (channel-target candidate))
             (room (room-session-id (platform-session-prefix
                                     host.platform)
                                    candidate))
             (press (and (platform-choices host.platform) (candidate-press candidate))))
        (ensure-room host room :parent (room-parent-session-id host target))
        ;; The room's own session names the model its lanes run on, so what
        ;; a command reads of it -- /context's window, /status's model -- is
        ;; that model's before the room has run an ask.
        (pin-room-model host room room target
                        (agent-of host (agent-for host room target (candidate-source candidate))))
        (cond ((and press (nlk:json-value press :boolean "confirm"))
               (confirm-card line press))
              ((equal name "models")
               (room-models-command host candidate args room))
              ((member name '("think" "effort") :test #'equal)
               (room-effort-command host candidate args room))
              (t (command-answer host candidate line room press)))))))

(defun command-answer (host candidate line room press)
  "Worker thread: LINE run in ROOM for CANDIDATE, (values TEXT CONTROLS
PANEL) -- its words and the card it offered (OFFER-CARD); else, where the
platform's messages carry choices, the dialog it answers a shell with drawn
as a card (DIALOG-CARD) on the view PRESS -- the action of the press that
said LINE, or NIL -- opens, and words alone said over the card PRESS was
on, read again: the panel's :NOTICE."
  (let ((*command-source* (candidate-source candidate))
        (*command-card* nil)
        (cards (platform-choices host.platform)))
    (multiple-value-bind (text dialog) (nle:slash line :session-id room)
      (flet ((view (key) (and press (nlk:json-value press :integer key))))
        (let ((back (and press (nlk:json-value press :text "back"))))
          (cond (*command-card*
                 (values text (getf *command-card* :controls) (getf *command-card* :panel)))
                ((and cards (hash-table-p dialog))
                 (dialog-card dialog line :tab (view "tab") :page (or (view "page") 1)
                                          :back back :back-tab (view "back_tab") :back-page (view "back_page")))
                (back
                 (let ((under (nth-value 1 (ignore-errors (nle:slash back :session-id room)))))
                   (if (hash-table-p under)
                       (multiple-value-bind (card controls panel)
                           (dialog-card under back :tab (view "back_tab") :page (or (view "back_page") 1))
                         (values (format nil "~a~%~%~a" text card) controls (list* :notice text panel)))
                       (values text nil nil))))
                (t (values text nil nil))))))))

(defparameter +private-commands+ '("stop" "channels" "models" "think" "effort" "threads")
  "Commands whose answer only acknowledges what the room sees happen anyway —
the stopped turn's own line says it stopped — or is a picker the person who
ran it works alone, and so is said to that person alone, where the platform
can.")

(defvar *refusals-alerted* (make-hash-table :test #'equal :synchronized t)
  "(host id . user id) -> when the operator was last told of that user's
refused command.")

(defun command-message (host candidate line &aux (name (values (nle:parse-slash-input line)))
                                                 (user (source-field candidate "user_id")))
  "Worker thread: answer the slash LINE CANDIDATE typed — run, or refused."
  ;; With operators declared, every command but /help is theirs: the commands
  ;; act on the shared room and on the organism's own configuration, and the
  ;; room's contract already says whose word is policy.
  (if (completion-allowed-p host name user)
      ;; A press answers in place, on a card whose privacy was settled when it
      ;; was posted.
      (respond host candidate (lambda () (run-command host candidate line))
               :private (and (not (candidate-pressed-p candidate))
                             (member name +private-commands+ :test #'equal)))
      ;; The refusal says what to do next, and carries it: Ask for access sends
      ;; the room's operators a request they answer with one press.
      (refuse host candidate (format nil "🔒 Only this room's operators can run /~a. Press Ask for access to send them a request, or send /help to see what you can use." name)
              (format nil "/~a" name)
              :controls (list (list (list "Ask for access" (format nil "nck:access:~a" name) :primary))))))

(defun refuse (host candidate text what &key controls
                                       &aux
                                       (user (source-field candidate "user_id"))
                                       (key (cons (host-id host) user))
                                       (now (now-ms)))
  "Worker thread: TEXT, a refusal, to the person CANDIDATE is — to them alone
where the platform can, CONTROLS under it — and a word to the operator's
shells naming who was refused WHAT, at most once in ten minutes a person."
  ;; Said to the shells and the log, never put on the board, which rides into
  ;; the model's prompt, and to the home channel when the operator set one.
  (respond host candidate (lambda () (values text controls)) :private t)
  (when (let ((last (gethash key *refusals-alerted*))) (or (null last) (> (- now last) 600000)))
    (setf (gethash key *refusals-alerted*) now)
    (tell-operator host (format nil "~a: ~a (~a) was refused ~a in ~a"
                                (host-id host)
                                (or (source-field candidate "user_name") "someone") user what
                                (or (source-field candidate "channel_id") "a direct message"))
                   :level :warning)))

(defun respond (host candidate answer &key private)
  "Worker thread: answer CANDIDATE with what ANSWER — a function of no
arguments — returns, (values TEXT CONTROLS PANEL): through the platform's own
return path when it offers one for this candidate, held open while ANSWER
runs — PRIVATE, seen by the person who asked alone — else as a reply to the
message that asked, pinging its author."
  ;; The hold goes first, before any of the work: a command slower than the
  ;; platform's first window (Discord's three seconds — /doctor, /usage)
  ;; still lands where it was asked, where an answer sent after the window is
  ;; refused and lost. A hold the platform refused leaves no path to answer
  ;; through, and the answer is a reply in the room instead. A long answer's
  ;; later chunks are plain posts either way — a private one's are not sent:
  ;; an acknowledgement or a refusal is one line. CONTROLS ride the answer's
  ;; last message. A press answered in place (PLAN-RESPOND) turns the message
  ;; pressed into the answer, and its controls go unless the answer brings
  ;; its own. An answer drawn as a PANEL is one message: its words past the
  ;; first chunk are the panel's, and are not posted after it.
  (let* ((hold (host-plan host #'platform-plan-respond candidate nil :private private))
         (held (and hold (execution-ok-p (execute-plan host.executor hold)))))
    (multiple-value-bind (text controls panel) (funcall answer)
      (let* ((pressed (candidate-pressed-p candidate))
             (chunks (split-text-chunks text host.text-limit))
             (tail (if (or private panel) '() (rest chunks)))
             (controls (or controls (and held (not private) pressed :clear)))
             ;; The answer says PRIVATE as its hold did: a platform whose
             ;; privacy rides the answer, not the hold (Slack's ephemeral,
             ;; Telegram's own chat with the bot), reads it here.
             (plan (and held (host-plan host #'platform-plan-respond
                                        candidate (text-chunk-text (first chunks))
                                        :private private
                                        :controls (if tail (and pressed :clear) controls)
                                        :panel panel)))
             (rest (if plan tail chunks)))
        (when (and hold (not held))
          (warn "~a: the command's answer could not be held open; it replies in the room" (host-id host)))
        (when plan
          (let ((result (execute-plan host.executor plan)))
            (note-delivery (host-id host) result.ok-p
                           result.error "command")))
        (when rest
          (nlk:bind (((delivered _ error)
                      (post-message host (channel-target candidate)
                                    (format nil "~{~a~^~%~}"
                                            (mapcar #'text-chunk-text (if panel (list (first rest)) rest)))
                                    :reply-to (and (null plan) (source-field candidate "message_id"))
                                    :ping (null plan)
                                    :controls (and (consp controls) controls)
                                    :panel panel)))
            (note-delivery (host-id host) delivered error "command")))))))

(defun menu-entries (catalog &aux (names (mapcar (lambda (entry) (getf entry :name)) catalog)))
  "CATALOG as a platform's command menu: every command, then each of its
aliases as a command of its own — a name already in the menu stays the one
command it names."
  ;; A platform menu has no aliases, and a name missing from it reads as a
  ;; command the bot does not have. The interaction an alias sends back spells
  ;; the alias, which NLE:SLASH resolves.
  (append catalog
          (loop for entry in catalog
                append (loop for alias in (getf entry :aliases)
                             unless (member alias names :test #'string-equal)
                               collect (progn (push alias names)
                                              (list* :name alias
                                                     (alexandria:remove-from-plist entry :name :aliases)))))))

(defun sync-commands (host &aux (plan-commands (platform-plan-commands host.platform)))
  "Tick thread: publish the command catalog to the platform when its shape
— the names, the descriptions, and which argument tails complete —
differs from the one last published — at start, and again when a cell
registers a command later."
  ;; Planning only; the calls run as one queued job, and the fingerprint is
  ;; recorded whether they landed or not, so a platform that refuses the menu
  ;; is one warning per catalog shape, never a loop.
  (when (and plan-commands (not host.commands-in-flight-p))
    (let* ((entries (menu-entries (nle:slash-catalog)))
           (fingerprint (format nil "~{~a=~a~:[~;+~]~^|~}"
                                (loop for entry in entries
                                      append (list (getf entry :name)
                                                   (getf entry :description)
                                                   (getf entry :autocomplete))))))
      (unless (equal fingerprint host.commands-published)
        (nlk:when-let (plans (host-plan host #'platform-plan-commands entries))
          (setf host.commands-in-flight-p t)
          (on-worker host
            (nlk:with-cleanup ((setf (host-commands-in-flight-p host) nil))
              (deliver-or-warn host plans "command menu not published")
              (setf host.commands-published fingerprint))))))))

;;; --- one ask, one lane ------------------------------------------------------
;;; An ask is one message, and a message can be handed over twice: the
;;; gateway replays what it thinks a resume missed, a reconnect's first
;;; events overlap the last lap's. The lane id and the address book catch the
;;; redelivery that finds its lane already interned — but a lane the kit
;;; opened a THREAD for does not exist while the call to open it is out: that
;;; is platform I/O, and in that window nothing owns the message. Two
;;; deliveries both take the thread path; the platform refuses the loser as a
;;; duplicate thread, and the loser's fallback — every way the thread can
;;; fail to appear admits the ask flat — opens a second lane for words
;;; already being answered inside the thread. So the delivery CLAIMS the ask
;;; before any of its platform work goes out, and a redelivery meets the
;;; claim instead of the open window.

;;; Empty between admissions.
(defvar *ask-claims* (make-hash-table :test #'equal)
  "`(HOST-ID . MESSAGE-ID)' -> t while an ask's admission is in flight:
claimed at the delivery, released once the ask's lane exists to be found or
the admission has failed and a redelivery may have it.")

(defun release-ask-claim (host message-id)
  "Drop HOST's claim on MESSAGE-ID: the admission it guarded has settled —
a lane exists for the ask, or it failed and a redelivery may have it."
  (when message-id
    (sb-ext:with-locked-hash-table (*ask-claims*)
      (remhash (cons (host-id host) message-id) *ask-claims*))))

(defun lane-invocation (command-id lane-id action)
  "The provenance a room's turn carries: source kind \"channel\", the lane as
its id, and this image as the transport."
  ;; A room is an unattended lane the way cron is one: what admits a turn here
  ;; is a message somebody left in a channel, not an operator at a keyboard.
  ;; Saying so is what lets the roster show where a running turn came from.
  ;; ACTION is the gateway's own spelling, "turn.start" or
  ;; "turn.steer", because a room's ingress is the same admission a WebSocket
  ;; command makes.
  (nlk:default-invocation action command-id :source "channel" :id lane-id))

(defun submit-ask (ask action tag &rest keys)
  "Read ASK's attachments in and submit it to its lane as ACTION, the command
id tagged TAG; KEYS ride along to NLE:SUBMIT."
  (multiple-value-bind (prompt images) (ask-prompt-and-attachments ask)
    (let ((command-id (format nil "~a-~a-~a" ask.lane tag (ask-identity ask))))
      (apply #'nle:submit ask.lane prompt :command-id command-id :images images
             :invocation (lane-invocation command-id ask.lane action) keys))))

(defun continue-lane (host lane ask &aux (active-turn lane.active-turn-id)
                                         (message-id ask.message-id))
  "LANE's next turn from ASK, an ask that addresses a lane already open —
the one authority for what a continuation does, whatever gesture made it: a
reply typed at the lane, or a reaction left on something it posted."
  ;; A turn already running is STEERED: it ends at its next round boundary and
  ;; ASK runs as the lane's next turn, no slot changing hands and no fork
  ;; happening. A settled lane takes ASK through the gate as its next turn,
  ;; with its whole tool trace still in context.
  ;;
  ;; An ask no message carried leaves the lane's message anchors alone: the
  ;; eye, the reply reference and the address book all name messages, and a
  ;; gesture has none to give them.
  (cond
    ((null active-turn) (gate-ask host ask))
    (t (when message-id
         (setf lane.trigger-message-id message-id))
       (setf lane.target (lane-delivery-target lane ask.target) lane.last-active-ms (now-ms)
             lane.voice ask.voice)
       (bind-addresses host lane ask.target (list message-id))
       (schedule-reaction host lane message-id +reaction-seen+)
       ;; The steered text opens the lane's next turn, whose TURN.STARTED sets
       ;; the lane prompt (NOTE-TURN-FACT): each exchange the room records
       ;; names its own ask.
       (run-ask-ingress
        host ask
        (lambda ()
          (handler-case (submit-ask ask "turn.steer" "steer" :steer active-turn)
            ;; The one refusal that is a race rather than an error: the turn
            ;; settled between our read of the lane and the admission. That ask
            ;; is re-admitted as a fresh turn on the same lane, which is what the
            ;; person replying meant.
            (nlk:turn-lifecycle-error ()
              (gate-ask host ask))
            (error (condition)
              (warn "~a: steer ~a refused: ~a"
                    (host-id host) (ask-identity ask) condition)
              (if host.worker
                  (on-worker host (fail-lane host lane (princ-to-string condition)))
                  (fail-lane host lane (princ-to-string condition))))))))))

(defun conversation-lane (host candidate &aux (target (channel-target candidate)))
  "The live lane CANDIDATE talks to without a reply gesture, or NIL: on a
surface that is one conversation — a direct message, or a thread the adapter
marked as one the bot takes part in — the lane running a turn there, else
the one active there last."
  ;; So a line typed while the turn runs steers it, and a line typed after it
  ;; settles is that lane's next turn, its tool trace still in context — one
  ;; conversation per surface, the way a person reads a DM or a thread. A
  ;; channel is many conversations, and a new ask there opens its own lane. A
  ;; surface whose lane was reaped has none, nor one whose lane was retired —
  ;; its room's head moved under it (RETIRE-ROOM-LANES): the ask forks the
  ;; surface's room as it stands.
  (when (or (equal "direct_message" (source-field candidate "chat_kind"))
            (candidate-joined-p candidate))
    (let ((found '()))
      (map-lanes host.lanes
                 (lambda (lane &aux (at lane.target))
                   (when (and (not (lane-retired lane))
                              (equal (getf at :channel-id) (getf target :channel-id))
                              (equal (getf at :thread-id) (getf target :thread-id)))
                     (push lane found))))
      (or (find-if #'lane-active-turn-id found)
          (first (sort found #'> :key #'lane-last-active-ms))))))

(defun answer-message (host candidate)
  "An ask."
  ;; A slash line is a command and answers without a lane
  ;; (COMMAND-MESSAGE). An ask that opens a thread of its own (THREAD-ASK-P: a
  ;; plain channel where the section wants threads) takes that path whatever it
  ;; replies to — its own lane forked at the room's head, a thread hung off its
  ;; own message, and the reply target kept as context the ask carries: a
  ;; question is a question, and answering it inside the thread of the message it
  ;; replied to would file it under someone else's ask. Every other ask takes one
  ;; of three shapes, decided by the reply gesture alone — never by who is
  ;; asking:
  ;;
  ;;   a reply to a RUNNING lane  -> steer it, no new slot: the lane already
  ;;                                 holds one and the person is talking to a
  ;;                                 turn that is still going.
  ;;   a reply to a SETTLED lane  -> its next turn, through the gate. The lane's
  ;;                                 whole tool trace is still in context.
  ;;   anything else              -> a new lane forked at the room's head, so it
  ;;                                 sees everything the room has said since.
  ;;
  ;; A surface that is one conversation — a direct message, a thread the bot
  ;; takes part in — needs no reply gesture: its live lane is the one every
  ;; message there talks to (CONVERSATION-LANE).
  ;;
  ;; A press on a choice (CHOICE) is a reply to the card pressed, wherever the
  ;; room would open threads: it answers the lane that posted the card, and
  ;; the card says which answer it took (ANSWER-CARD). A card whose lane is
  ;; gone -- reaped after its idle window, or lost to a restart -- is
  ;; answered as a typed reply to it would be: a new lane forked at the
  ;; room's head, anchored on the card, so a button works for as long as the
  ;; card shows it.
  (let* ((reply-to (source-field candidate "reply_to_message_id"))
         (pressed (candidate-pressed-p candidate))
         (message-id (source-field candidate "message_id"))
         (target (channel-target candidate))
         ;; Where the ask opens a thread of its own, the reply gesture is
         ;; context, not a route: the lane it would have joined answers from
         ;; a thread, and this question belongs in one of its own
         ;; (START-THREADED-ASK).
         (own-thread-p (thread-ask-p host candidate))
         (existing (or (and reply-to
                            (or pressed (not own-thread-p))
                            (lane-for-address host.lanes
                                              (address-of host target reply-to)))
                       (conversation-lane host candidate)))
         ;; The text as the model would read it: our own mention stripped,
         ;; so `@bot /help' and Telegram's `/help@bot' are the command.
         (line (candidate-said host candidate))
         ;; An ask typed INSIDE an existing thread carries the parent
         ;; channel's live chatter, exactly as a thread-opening ask does; the
         ;; conversation itself is the fork's, never text read back. The
         ;; target's :CHANNEL-ID is that parent channel.
         (parent-room (room-parent-session-id host target)))
    (cond
      ((slash-line-p line)
       (on-worker host (command-message host candidate line)))
      ;; A message a lane already took — the gateway replays what it thinks a
      ;; resume missed — is not a second steer or a second turn: the first
      ;; delivery bound it as that lane's address.
      ((and existing message-id
            (lane-for-address host.lanes (address-of host target message-id)))
       nil)
      (existing
       (when pressed
         (on-worker host (answer-card host candidate line)))
       (note-replied-line existing reply-to)
       (continue-lane host existing (build-ask host candidate existing.session-id t)))
      (t
       ;; A press is no message: the card it answers anchors the new lane --
       ;; its id, its claim, the reply its answer carries -- and is answered
       ;; in place first, its buttons gone.
       (when pressed
         (on-worker host (answer-card host candidate line)))
       (let* ((anchor (or message-id (and pressed reply-to)))
              (lane-id (and anchor
                            (lane-id
                             (room-session-id (platform-session-prefix host.platform) candidate)
                             anchor))))
                  ;; The lane id IS the message id, so a redelivered event finds its own lane
         ;; already interned, and a lane that moved into a thread is addressed by that
         ;; message too (BIND-LANE-ADDRESS, at admission), so the address book catches
         ;; the redelivery the id cannot: the thread's lane is named for the thread,
         ;; and this id is the channel's. Neither exists while the thread call is out,
         ;; which is what the claim covers — and the gateway would dedupe the
         ;; start_turn on its command id, but the concurrency slot is ours to leak:
         ;; claiming a second one for the same ask is capacity nothing ever gives
         ;; back.
         (when (and lane-id
                    (null (find-lane host.lanes lane-id))
                    (null (lane-for-address
                           host.lanes
                           (address-of host target anchor)))
                    ;; The claim is this delivery's, unless another has the same ask in flight.
                    (sb-ext:with-locked-hash-table (*ask-claims*)
                      (let ((key (cons (host-id host) anchor)))
                        (unless (gethash key *ask-claims*)
                          (setf (gethash key *ask-claims*) t)))))
           ;; Opening a thread is platform I/O and is done off this
           ;; thread; the in-thread path takes the gate off it too, so a
           ;; queued slot never blocks the gateway. The claim rides with
           ;; the work and is released once the lane the ask opens exists.
           (let ((thunk
                   (lambda ()
                     (if own-thread-p
                         (start-threaded-ask host candidate lane-id)
                         (nlk:with-cleanup ((release-ask-claim host anchor))
                           (let ((ask (build-ask host candidate lane-id nil
                                                 :ambient-room parent-room)))
                             (setf (ask-message-id ask) anchor)
                             (gate-ask host ask)))))))
             (unless (and (or own-thread-p parent-room) host.worker
                          (delivery-worker-enqueue host.worker thunk))
               (funcall thunk)))))))))

(defun answer-card (host candidate line &aux (card (or (source-field candidate "card_text") ""))
                                              (mark (format nil "Answered: ~a" line)))
  "Worker thread: the card CANDIDATE pressed, answered with LINE — its
choices go, and a line under its words says which answer it took."
  ;; OpenClaw's mark on a question it asked: the question stays
  ;; readable, and the room sees the answer it took. The card's words are
  ;; cut, if they must be, to leave the mark room in one message.
  (respond host candidate
           (lambda ()
             (funcall (platform-answer-body host.platform)
                      (nlk:clip card (max 0 (- host.text-limit (length mark) 8)) :ellipsis "…")
                      mark))))

;;; --- threads (an ask gets a surface of its own) ------------------------------
;;; A question typed in a plain channel can open a THREAD for itself: the
;;; thread is created off the ask's own message and the lane runs inside it,
;;; answering there alone — the ask's message shows the thread under it, so
;;; the room it was typed in needs no copy. The topology costs nothing:
;;; room.lisp already keys a thread as its own room with its parent's id in
;;; the name, and every adapter already normalizes a message that arrives
;;; inside one — this half only decides WHICH asks open a thread.

(defparameter +thread-title-length+ 80
  "Characters the name an ask's thread opens with keeps — Discord allows
100, and the margin is for a word that would be cut in half.")

(defun thread-title (host candidate)
  "The name an ask's thread opens with: the ask's first words, our mention
stripped, cut to +THREAD-TITLE-LENGTH+."
  ;; A quick name the reader recognizes, until the title its lane's session
  ;; is named replaces it (WEAR-TITLE).
  (let* ((words (nlk:split-words (candidate-said host candidate)))
         (name (format nil "~{~a~^ ~}" (subseq words 0 (min (length words) 5)))))
    (if (zerop (length name))
        "a question"
        (string-right-trim '(#\Space) (nlk:clip name +thread-title-length+ :ellipsis "")))))

(defun first-words (said)
  "SAID's first five words, the name an ask carries until its title lands."
  (let ((words (nlk:split-words said)))
    (string-right-trim ",.;:" (format nil "~{~a~^ ~}" (subseq words 0 (min 5 (length words)))))))

(defun thread-ask-p (host candidate)
  "Whether CANDIDATE — an ask that has opened no lane yet — gets a thread of
its own instead of running in the surface it was typed on."
  ;; A channel or group message with an id, where the section wants threads
  ;; and does not name that channel flat: a DM has no threads to open, a
  ;; thread is one already, and a forum topic is the platform's own room for
  ;; the ask.
  (and host.threads-p
       (source-field candidate "message_id")
       (member (source-field candidate "chat_kind") '("channel" "group")
               :test #'equal)
       (source-field candidate "channel_id")
       (not (member (source-field candidate "channel_id") host.flat-channels
                    :test #'equal))))

(defun candidate-in-thread (candidate thread-id parent-channel-id)
  "CANDIDATE as the adapter would have normalized the same message had it
arrived inside THREAD-ID: the thread is the active channel, its parent beside
it, the kind a thread — one the bot takes part in, since the kit opened it
for this very ask. A copy — the adapter's own candidate is its event's."
  (nlk:copy-json-object
   candidate
   "source" (nlk:copy-json-object (candidate-source candidate)
                                  "chat_kind" "thread"
                                  "channel_id" thread-id
                                  "parent_channel_id" parent-channel-id
                                  "thread_id" thread-id
                                  "joined" (not (candidate-bot-p candidate)))))

(defun mark-joined-thread (host candidate)
  "CANDIDATE, marked source.joined when a person typed it inside a thread
HOST already takes part in; NIL stays NIL."
  ;; An adapter's opt-in, called on its own ingress before HANDLE-CANDIDATE:
  ;; a Discord thread is one conversation, so once the bot is in it — it
  ;; answered an ask there, or opened the thread for one — a follow-up needs
  ;; no mention, and a line typed while a turn runs steers it
  ;; (CONVERSATION-LANE). A platform whose threads are standing topics
  ;; (Telegram's forum topics) leaves it uncalled. Taking part is the thread's
  ;; room existing in the store — made by the first ask admitted there, or by
  ;; a command run there — so it outlives the lane and the image. A bot's line
  ;; is never marked: two bots that each answer every line of a thread would
  ;; answer each other forever.
  (let ((source (and candidate (candidate-source candidate))))
    (when (and source
               (source-field candidate "thread_id")
               (not (candidate-bot-p candidate))
               (nlk:session-exists-p (room-session-id (platform-session-prefix host.platform)
                                                      candidate)))
      (setf (gethash "joined" source) t)))
  candidate)

(defun candidate-taken-p (host candidate &aux (prefix (platform-session-prefix host.platform))
                                                (message (source-field candidate "message_id")))
  "Whether CANDIDATE's message already opened a lane — where it was typed,
or in the thread the kit opened off it — in this store or an earlier image's."
  ;; What a catch-up after the bot was away asks of a message it reads back:
  ;; one it answered before the gap is not asked again. A thread the kit
  ;; opens off a message carries the message's own id on Discord, the one
  ;; platform that reads back.
  (and message
       (or (nlk:session-exists-p (lane-id (room-session-id prefix candidate) message))
           (nlk:session-exists-p
            (lane-id (room-session-id prefix (candidate-in-thread candidate message
                                                                  (source-field candidate "channel_id")))
                     message)))))

(defun start-threaded-ask (host candidate lane-id)
  "Open a thread for CANDIDATE's ask — hung off the ask's own message and
named from its first words — and admit the ask inside it."
  ;; Runs on the delivery pool. LANE-ID is the lane the flat path would have
  ;; opened; the lane that runs in the thread is named for the thread, so a
  ;; reply to it, a /stop and a laneless notice all land in the room the work
  ;; is in.
  ;;
  ;; Every way the thread can fail to appear — no capability, a refused call,
  ;; an answer that names no thread — admits the ask flat in the room it was
  ;; typed in: a question never lands nowhere. The ask's claim
  ;; (ANSWER-MESSAGE's) is released on the way out either way: what it guards
  ;; is the window this call spends on the platform, and the lane that answers
  ;; the ask exists by the time this returns.
  (let* ((platform host.platform)
         (message-id (source-field candidate "message_id"))
         (typed (source-field candidate "channel_id"))
         (parent-room (room-session-id platform.session-prefix
                                       candidate)))
    ;; Everything this call spends on the platform rides inside the claim it
    ;; took: opening the thread happens there, so an error on the way gives
    ;; the claim back with it.
    (nlk:with-cleanup ((release-ask-claim host message-id))
      (let ((plan (host-plan host #'platform-plan-thread (list :channel-id typed)
                             (thread-title host candidate) :message-id message-id)))
        (if (null plan)
            (gate-ask host (build-ask host candidate lane-id nil))
            (let* ((result (execute-plan host.executor plan))
                   (thread-id (and result.ok-p (funcall platform.thread-id-of result.body))))
              (cond
                ((null thread-id)
                 (warn "~a: ask ~a runs in the channel, no thread opened: ~a"
                       (host-id host) message-id
                       (if result.ok-p "the response named no thread" result.error))
                 (gate-ask host (build-ask host candidate lane-id nil)))
                (t
                 ;; The ask now reads as one that arrived inside the
                 ;; thread, and carries the room-it-was-typed-in's live
                 ;; chatter: what the channel said since its last exchange
                 ;; that was never a turn. The conversation the question
                 ;; was born out of reaches the lane by the fork — the
                 ;; thread's room composes the channel's record message by
                 ;; message — never as text pasted into this ask. Its lane
                 ;; is the THREAD's (LANE-ID is the flat
                 ;; fallback's): an ask that moved into a thread is
                 ;; addressed there from the first message, so a later
                 ;; reply, a /stop, or a notice all land in the room the
                 ;; work is in. ORIGIN keeps the room the ask was typed in,
                 ;; where the answer's one line goes and where a call about
                 ;; the ask's own message is addressed — and its record
                 ;; (:ROOM), where the exchange settles beside the thread's
                 ;; once the answer lands there.
                 (let* ((inside (candidate-in-thread candidate thread-id typed))
                        (ask (build-ask
                              host inside
                              (lane-id (room-session-id platform.session-prefix inside) message-id)
                              nil :ambient-room parent-room))
                        (lane (intern-ask-lane host ask))
                        (origin (list :channel-id typed :message-id message-id
                                      :thread-id thread-id :room parent-room
                                      :landed nil)))
                   (setf (lane-origin lane) origin)
                   ;; The thread's room composes the room the ask was TYPED
                   ;; in: the conversation goes on where the person typed.
                   ;; First ensure wins; the ask's own admission then finds
                   ;; the room already made.
                   (ensure-room host ask.room :parent parent-room)
                   (gate-ask host ask))))))))))

(defun message-target (lane message-id &aux (origin (lane-origin lane)))
  "The target a call about MESSAGE-ID needs: the lane's own, except for the
ask a lane opened its thread for — that message lives in the parent channel,
and a call addressed to the thread would name a message it does not hold."
  (if (and origin (equal message-id (getf origin :message-id)))
      (list :channel-id (getf origin :channel-id))
      lane.target))

(defun gate-ask (host ask)
  "Claim a concurrency slot for ASK and admit it, or show it its place in
the line."
  ;; A queued ask opens its status line at once: the failure this replaces was
  ;; an invisible wait behind a typing indicator that expired after five
  ;; minutes into silence.
  ;; The wait is timed by the wall clock from its first moment, which a
  ;; restart does not reset (TAKE-BACK-QUEUED).
  (unless (queued-since ask)
    (setf (queued-since ask) (get-universal-time)))
  (multiple-value-bind (admitted position)
      (claim-slot host ask host.max-concurrent-turns)
    (if admitted
        (admit-ask host ask)
        (let ((lane (intern-ask-lane host ask)))
          (bt2:with-lock-held ((lane-lock lane))
            ;; A queued line already open -- the one a restart brought back --
            ;; is the one noted, as admission keeps it.
            (let ((digest lane.digest))
              (setf (turn-digest-ask-id
                     (digest-note-queued (if (and digest (eq digest.phase :queued))
                                             digest
                                             (lane-open-digest lane (now-ms)))
                                         position))
                    ask.message-id)))
          (schedule-reaction host lane ask.message-id +reaction-seen+)
          (schedule-flush host lane)
          (save-queued-asks host)))))

(defun intern-ask-lane (host ask)
  (let ((lane (intern-lane host.lanes
                           ask.lane
                           :target ask.target
                           :trigger-message-id ask.message-id
                           :parent-session-id ask.room
                           :owner-id ask.owner-id)))
    (bind-addresses host lane ask.target (list ask.message-id))
    (setf lane.prompt ask.prompt
          ;; Brought current on every ask, a fresh fork and a continuation
          ;; alike: a lane the kit moved into a thread learns its thread id
          ;; here, and the section is read live.
          lane.where ask.where
          lane.voice ask.voice
          lane.agent ask.agent
          lane.said ask.said)
    lane))

(defun recount-queued (host pending)
  "Everyone still in PENDING, the line in its order, counts down on their card."
  (loop for ask in pending
        for position from 0
        do (nlk:when-let (lane (find-lane host.lanes ask.lane))
             (with-lane-digest (digest lane)
               (when (eq digest.phase :queued)
                 (digest-note-queued digest position)))
             (schedule-flush host lane))))

(defun promote-next (host owner-id)
  "Give back a finished lane's slot and let the next ask through."
  (multiple-value-bind (next pending) (release-slot host owner-id)
    (when next (admit-ask host next))
    ;; The queue moved: what a stop or a crash would leave waiting.
    (when (or next pending)
      (save-queued-asks host))
    (recount-queued host pending)))

(defun admit-ask (host ask &aux (lane (intern-ask-lane host ask)))
  "Ensure the room, fork the lane off its current head, and open its turn."
  ;; Holds a concurrency slot on entry; every failure path gives it back, or
  ;; the gate leaks capacity one crash at a time. The submitting half —
  ;; resolving the ask's attachments and the submit itself — owns the slot
  ;; from the moment it starts: a turn in flight gives it back when it
  ;; settles, and a failure before or during admission gives it back here. A
  ;; refused admission is surfaced in the channel, never swallowed.
  (handler-case
      (progn
        (unless ask.continue-p
          (ensure-room host ask.room
                       :parent (room-parent-session-id host ask.target))
          ;; The lane works in its agent's folder, else where its room does.
          (fork-lane-session ask.room ask.lane
                             (platform-contract-section
                              host.platform)
                             ask.contract
                             :cwd (getf (agent-of host ask.agent) :folder)))
        ;; The persona is brought current on every ask — a fresh
        ;; fork and a continuation alike — so an edited SOUL.md is
        ;; live on the next message. Key order puts it after the
        ;; room contract.
        (apply-soul (host-id host) ask.lane
                    (or (getf (agent-of host ask.agent) :soul-path) host.soul-path))
        ;; The model too: a /models in the room since the last ask is live on
        ;; this one. Its reasoning effort is the same room-level overlay,
        ;; applied after the model so the provider ladder is authoritative.
        (apply-lane-model host ask)
        (apply-lane-effort host ask)
        (bt2:with-lock-held ((lane-lock lane))
          (let ((digest lane.digest))
            (setf (turn-digest-ask-id
                   (if (and digest
                            (eq digest.phase :queued))
                       (digest-note-admitted digest (now-ms))
                       (lane-open-digest lane (now-ms))))
                  ask.message-id)))
        (schedule-reaction host lane ask.message-id +reaction-seen+)
        ;; Typing starts here rather than waiting for the tick: the
        ;; indicator is what covers a turn quick enough never to earn
        ;; chrome, and half a second of nothing is the gap it exists
        ;; to fill.
        (when host.worker
          (on-worker host (typing-tick host)))
        ;; The submit is where an ask's attachments are read in — fetched,
        ;; transcribed: on the pool when the ask carries any, on this
        ;; thread when it does not.
        (run-ask-ingress
         host ask
         (lambda ()
           (nlk:with-handlers ((error (condition)
                                 (fail-lane host lane (princ-to-string condition))))
             (let ((admission (submit-ask ask "turn.start" "msg")))
               (case (nlk:active-input-admission-disposition admission)
                 (:started (bind-admitted-turn lane admission))
                 (:duplicate (drop-duplicate-admission host lane admission))))))))
    (error (condition)
      (warn "~a ingress for ~a failed: ~a"
            (host-id host) ask.lane condition)
      (fail-lane host lane (princ-to-string condition)))))

;;; --- reactions from the room (a gesture on a message of ours) ----------------------
;;; A reaction is a line the room said without typing one: it arrives as a
;;; candidate like any other, carrying the emoji as its text and the message
;;; it was left on as its reply gesture. It is a NOTICE, never an actionable
;;; trigger: it opens no turn and steers none, whoever left it and whether
;;; it was added or taken back. Turns start from words — a message, an edit
;;; that addresses the bot, a command — never from a gesture.
;;;
;;; So every accepted reaction becomes one line of room chatter, carried
;;; into whatever that room asks next. It posts nothing and pings nobody,
;;; so reaction spam cannot become notice spam, and it cannot loop with the
;;; eye the kit leaves on an ask because it makes no platform call at all.
;;; The emoji itself is never read here: what a room means by one is the
;;; model's to read, not a table's to decide.
;;;
;;; Only ever on a message the kit posted and still holds an address for:
;;; the address book is what says "mine", and a reaction on anything else —
;;; another person's message, another bot's, one whose lane has been reaped
;;; — is not ours to read.

(defun reaction-noticed (host candidate)
  "Adapter thread: one reaction CANDIDATE — the emoji as its text, the
message it was left on as its reply gesture, and source.reaction saying
whether it was added or taken back."
  ;; => what became of it: :OBSERVE,
  ;; :REJECT, or NIL for a reaction on nothing of ours. Never :ANSWER: a
  ;; gesture opens no turn and steers none, whoever left it.
  ;;
  ;; The lane is found by the reacted message's own address, which is what
  ;; makes that message ours. The read gates are the room's (DECIDE-INBOUND:
  ;; scope, channel, user, chat kind, bot authors).
  (let* ((emoji (string-trim '(#\Space) (or (gethash "text" candidate) "")))
         (added-p (equal "add" (source-field candidate "reaction")))
         (message-id (source-field candidate "reply_to_message_id"))
         (lane (and (plusp (length emoji))
                    message-id
                    (lane-for-address
                     host.lanes
                     (address-of host (channel-target candidate) message-id)))))
    (when lane
      (multiple-value-bind (action reason) (decide-inbound host.policy candidate)
        (cond
          ((eq action :reject)
           (set-channel-status (host-id host) :last-rejection reason)
           :reject)
          (t
           ;; Somebody acted inside the thread the kit opened for an ask, so
           ;; no silent answer may take that thread back with them in it.
           (note-thread-spoken (source-field candidate "thread_id"))
           ;; The kit owns the words a gesture reads as — the candidate
           ;; arrived carrying the emoji alone — and every line the room
           ;; records is written by SPEAKER-LINE, here as everywhere. The
           ;; emoji itself goes through untouched: what a room means by one
           ;; is the model's to read, not a table's to decide.
           (setf (gethash "text" candidate)
                 (format nil "~a ~a" (if added-p "reacted" "took back") emoji))
           (observe-candidate host candidate)
           :observe))))))

(defun handle-candidate (host candidate &aux (text (nlk:trimmed (or (gethash "text" candidate) "")))
                                             (attachments (candidate-attachments candidate)))
  "One normalized inbound CANDIDATE on the adapter's transport thread:
admitted through the policy, then observed as room chatter, answered as
an ask, or rejected with its reason on the status surface."
  ;; Returns the action taken, or NIL for a message carrying neither words nor
  ;; an attachment.
  (when (or (plusp (length text)) attachments)
    (set-channel-status (host-id host) :last-event-at-ms
                        (* 1000 (- (get-universal-time) nlk:+unix-epoch+)))
    (multiple-value-bind (action reason)
        (decide-inbound host.policy candidate)
      ;; Typed inside a thread the kit opened for an ask: that thread holds
      ;; a line of somebody's own now, and no silent answer may take it —
      ;; whether this line is an ask of its own or only chatter.
      (unless (eq action :reject)
        (note-thread-spoken (source-field candidate "thread_id")))
      (ecase action
        (:reject (set-channel-status (host-id host) :last-rejection reason)
                 (cond
                   ;; A candidate with its own return path hears the refusal.
                   ((host-plan host #'platform-plan-respond candidate reason)
                    (on-worker host
                      (refuse host candidate (format nil "not allowed here: ~a" reason)
                              (format nil "~a (~a)" (candidate-said host candidate) reason))))
                   ;; A stranger's direct message is answered with a way in.
                   ((pairing-candidate-p host candidate reason)
                    (on-worker host (offer-pairing host candidate)))))
        (:observe (observe-candidate host candidate))
        (:answer (answer-message host candidate)))
      action)))

;;; --- terminal reconciliation ------------------------------------------------------------

(defun reconcile-terminal-digest (host lane)
  "Fold a durable terminal fact missed by the live frame hook."
  ;;
  ;; => T when this call claimed the digest for terminal delivery. The caller is
  ;; a worker tick: all platform work remains on the delivery queue.
  (let* ((digest lane.digest)
         (turn-id (and digest digest.turn-id))
         (fact (and digest
                    (eq digest.phase :running)
                    (stringp turn-id)
                    (nlk:store-open-p)
                    (handler-case
                        (nlk:when-let (row (nlk:events
                                            :turn-id turn-id
                                            :kind '("turn.completed" "turn.failed" "turn.cancelled")
                                            :order :newest
                                            :as :row
                                            :columns '("kind" "payload")))
                          (list (first row) (nlk:decode-json (second row))))
                      (error (condition)
                        (warn "terminal recovery lookup for ~a failed: ~a" turn-id condition)
                        nil)))))
    ;; FACT is (KIND PAYLOAD), the terminal fact's own arguments.
    (multiple-value-bind (phase detail) (and fact (apply #'terminal-phase fact))
      (when (and phase
                 (bt2:with-lock-held ((lane-lock lane))
                   (when (and (eq digest lane.digest)
                              (eq digest.phase :running)
                              (equal turn-id digest.turn-id)
                              (equal turn-id lane.active-turn-id))
                     (setf lane.active-turn-id nil)
                     (digest-note-terminal digest phase detail (now-ms))
                     (setf (terminal-reactions digest) (terminal-reaction-snapshot lane turn-id))
                     t)))
        (on-worker host (finish-turn host lane digest))
        t))))

;;; --- a stop in the middle of a turn ------------------------------------------------------
;;; The organism resumes a turn a stop cut off when it boots again
;;; (RECOVER-INTERRUPTED-TURNS), but a lane lives in the image: the resumed
;;; turn found no lane, its answer landed nowhere, and its line said
;;; "working" for good. So a stopping host settles every live line — a
;;; running one says it is paused and picks up when the bot is back, a queued
;;; one that it never ran — and writes the running lanes down in the store;
;;; the next start takes them back, and the resumed turn answers where it was
;;; asked, on the line it had. Hermes warns the chat and asks for a message
;;; after the restart. A restart of the channels alone finds the turns still
;;; running and picks them straight up.

(defparameter +handoff-key+ "interrupted-lanes"
  "The state key a stopping host writes its running lanes under.")

(defparameter +paused-detail+
  "the bot stopped; this picks up where it left off when it is back"
  "What a running line says while its host is down.")

(defparameter +held-detail+
  "the bot stopped before this ran; it runs when the bot is back"
  "What a queued line says while its host is down: the ask waits on.")

(defparameter +stale-detail+
  "the bot was down too long for this to run: ask again"
  "What a queued line says when the next start finds it past +QUEUED-HORIZON+.")

(defparameter +queued-key+ "queued-asks"
  "The state key a host keeps its waiting asks under (SAVE-QUEUED-ASKS).")

(defparameter +queued-horizon+ 1800
  "Seconds an ask may have waited, a downtime included, and still run when its
host is back: an older one is dropped, its line saying so, so a restart never
resurrects yesterday's backlog.")

(defvar *queued-asks-lock* (bt2:make-lock :name "queued-asks")
  "Held over each write of a host's waiting asks: the snapshot is taken inside
it, so the store keeps the newest.")

(defun handoff-session (host)
  (format nil "channel-~a" (host-id host)))

(defun ask-handoff (ask status-id)
  "What the next start needs of the waiting ASK, whose line is the message
STATUS-ID, as JSON (HANDOFF-ASK reads it back)."
  (nlk:json-object "room" ask.room
                   "lane" ask.lane
                   :opt "owner" ask.owner-id
                   :opt "message" ask.message-id
                   "target" (nlk:plist-json ask.target)
                   "contract" ask.contract
                   "prompt" ask.prompt
                   :opt "continue" (and ask.continue-p t)
                   ;; (POSITION . ATTACHMENTS): each the adapter's table, the
                   ;; url to fetch at admission, never the bytes.
                   "attachments" (map 'vector (lambda (anchor)
                                                (nlk:json-object "at" (car anchor)
                                                                 "files" (coerce (cdr anchor) 'vector)))
                                      ask.attachments)
                   :opt "key" ask.key
                   :opt "where" ask.where
                   :opt "voice" (and ask.voice (string-downcase ask.voice))
                   :opt "agent" ask.agent
                   "said" ask.said
                   :opt "queued" (queued-since ask)
                   :opt "status" status-id))

(defun handoff-ask (entry)
  "The waiting ask ENTRY, an ASK-HANDOFF, describes, the time it first waited
back beside it (QUEUED-SINCE)."
  (let ((ask (make-ask :room (nlk:json-value entry :string "room")
                       :lane (nlk:json-value entry :string "lane")
                       :owner-id (nlk:json-value entry :string "owner")
                       :message-id (nlk:json-value entry :string "message")
                       :target (nlk:json-plist (nlk:json-value entry :object "target"))
                       :contract (or (nlk:json-value entry :string "contract") "")
                       :prompt (or (nlk:json-value entry :string "prompt") "")
                       :continue-p (eq t (gethash "continue" entry))
                       :attachments (loop for anchor across (or (nlk:json-value entry :array "attachments") #())
                                          collect (cons (nlk:json-value anchor :integer "at")
                                                        (coerce (nlk:json-value anchor :array "files") 'list)))
                       :key (nlk:json-value entry :string "key")
                       :where (nlk:json-value entry :string "where")
                       :voice (nlk:when-let (voice (nlk:json-value entry :string "voice"))
                                (find voice '(:note :channel) :test #'string-equal))
                       :agent (nlk:json-value entry :string "agent")
                       :said (or (nlk:json-value entry :string "said") ""))))
    (setf (queued-since ask) (nlk:json-value entry :integer "queued"))
    ask))

(defun save-queued-asks (host &aux (session (handoff-session host)))
  "Write HOST's waiting asks down, oldest first, each with the line it shows:
what the next start runs (TAKE-BACK-QUEUED). Called at every change of the
queue and at the stop, so a crash keeps them waiting as a stop does."
  ;; Lock order: this lock, then the book's, then a lane's, one at a time;
  ;; nothing holding either of the others takes this one.
  (when (nlk:store-open-p)
    (bt2:with-lock-held (*queued-asks-lock*)
      (let ((entries (loop for ask in (with-room-book (book host) (copy-list book.pending))
                           for lane = (find-lane host.lanes ask.lane)
                           collect (ask-handoff ask (and lane
                                                         (bt2:with-lock-held ((lane-lock lane))
                                                           (and lane.digest
                                                                (turn-digest-status-id lane.digest))))))))
        (if entries
            (nlk:session-state-put session +queued-key+
                                   (nlk:encode-json-object (coerce entries 'vector)))
            (nlk:session-state-delete session +queued-key+))))))

(defun take-back-queued (host &aux (session (handoff-session host))
                                   (now (get-universal-time)))
  "Starting: the asks a stop or a crash left waiting (SAVE-QUEUED-ASKS) wait
again, oldest first, each on the line it had, and run as slots free; one
that waited past +QUEUED-HORIZON+ is dropped, its line saying so. => how
many wait again."
  (let ((json (and (nlk:store-open-p) (nlk:session-state-get session +queued-key+))))
    (if (null json)
        0
        (progn
          (nlk:session-state-delete session +queued-key+)
          (loop for entry across (nlk:decode-json json)
                count (nlk:with-handlers ((error (condition)
                                            (warn "~a: an ask a stop left waiting was not taken back: ~a"
                                                  (host-id host) condition)
                                            nil))
                        (take-back-ask host entry now)))))))

(defun take-back-ask (host entry now &aux (ask (handoff-ask entry))
                                          (status-id (nlk:json-value entry :string "status"))
                                          (lane (intern-ask-lane host ask))
                                          (fresh (<= (- now (or (queued-since ask) now)) +queued-horizon+)))
  "One waiting ask from TAKE-BACK-QUEUED's ENTRY, at NOW: back at the gate on
the line it had, or, stale, that line saying so. => whether it waits again."
  ;; The line is the queued digest the gate notes, so the card the stop left
  ;; is the one edited, and admission keeps it (ADMIT-ASK).
  (when status-id
    (bind-addresses host lane ask.target (list status-id)))
  (bt2:with-lock-held ((lane-lock lane))
    (let ((digest (lane-open-digest lane (now-ms))))
      (setf digest.status-id status-id
            digest.ask-id ask.message-id)
      (if fresh
          (digest-note-queued digest 0)
          (digest-note-terminal digest :cancelled +stale-detail+ (now-ms)))))
  (if fresh
      (progn (gate-ask host ask) t)
      (progn (flush-now host lane) nil)))

(defun lane-handoff (lane digest)
  "What the next start needs of LANE, whose turn DIGEST watches, as JSON."
  ;; The caller holds the lane lock.
  (nlk:json-object "session" lane.session-id
                   "turn" digest.turn-id
                   "target" (nlk:plist-json lane.target)
                   "trigger" lane.trigger-message-id
                   "parent" lane.parent-session-id
                   "owner" lane.owner-id
                   "where" lane.where
                   "prompt" lane.prompt
                   "said" lane.said
                   "agent" lane.agent
                   ;; An origin is rewritten by pushing keys in front, and GETF
                   ;; reads the first: the first of each key is the one kept.
                   "origin" (nlk:when-let (origin (lane-origin lane))
                              (nlk:plist-json (loop with seen = '()
                                                    for (key value) on origin by #'cddr
                                                    unless (member key seen)
                                                      do (push key seen)
                                                      and append (list key value))))
                   "status" digest.status-id
                   "ask" digest.ask-id
                   ;; The title the card wears (WEAR-TITLE): the next start's card
                   ;; keeps it, not the ask's first words again.
                   "task" digest.task
                   "calls" digest.tool-calls
                   ;; Elapsed, not the start: NOW-MS counts from the image's own
                   ;; start, and the next one's is another clock.
                   "elapsed" (- (now-ms) digest.started-at-ms)
                   "reactions" (map 'vector (lambda (mark) (vector (car mark) (cdr mark)))
                                    lane.reactions)))

(defun running-handoff (lane digest)
  "LANE's hand-off (LANE-HANDOFF) when its turn is one the next boot resumes,
else NIL: running, and not waiting on its own background work -- that work
dies with the image, and its wake with it, so the turn keeps its line."
  ;; The caller holds the lane lock.
  (when (and (eq digest.phase :running) lane.active-turn-id (not digest.background-pending))
    (lane-handoff lane digest)))

(defparameter +handoff-beat+ 5000
  "How stale, in ms, the elapsed time a running lane is written down with may
grow (SAVE-RUNNING-LANES): the record is rewritten when its lanes change, and
once a beat while they run.")

(defun save-running-lanes (host previous &aux (entries '()))
  "Write HOST's running lanes down as a stop does (HAND-OFF-LANES), so a death
no stop runs under -- the power cut, a kill -- hands them on all the same: the
next start takes them back (TAKE-BACK-LANES) and each resumed turn answers on
the line it had. PREVIOUS is what the last call wrote; the same text is not
written twice. => the text this call stands on, NIL for no running lane."
  ;; The tick calls this, once the start took back what the last image left.
  ;; Each elapsed is floored to the beat, so a lane that only ran on rewrites
  ;; the record once a beat, not every tick; sorted by turn, since the lanes'
  ;; table walks in no fixed order and an unchanged record must read the same.
  ;; A turn that ends is gone from the record at the next tick: a death
  ;; inside that half second can answer it twice, or not at all.
  (map-lanes host.lanes
             (lambda (lane)
               (with-lane-digest (digest lane)
                 (nlk:when-let (entry (running-handoff lane digest))
                   (setf (gethash "elapsed" entry)
                         (* +handoff-beat+ (floor (gethash "elapsed" entry) +handoff-beat+)))
                   (push entry entries)))))
  (let ((text (and entries
                   (nlk:encode-json-object
                    (coerce (sort entries #'string< :key (lambda (entry) (or (gethash "turn" entry) "")))
                            'vector)))))
    (cond ((or (equal text previous) (not (nlk:store-open-p))) previous)
          (text (nlk:session-state-put (handoff-session host) +handoff-key+ text) text)
          (t (nlk:session-state-delete (handoff-session host) +handoff-key+) nil))))

(defun hand-off-lanes (host &aux (now (now-ms)) (entries '()))
  "Stopping: settle every live line of HOST, and write down the lanes whose
turns the next boot resumes, and the asks still waiting (SAVE-QUEUED-ASKS)."
  ;; Runs after the delivery pool stopped, on the stopping thread: each line
  ;; is one synchronous edit.
  (map-lanes host.lanes
             (lambda (lane)
               (let ((running
                       (with-lane-digest (digest lane)
                         (case digest.phase
                           (:running
                            (nlk:when-let (entry (running-handoff lane digest))
                              (push entry entries)
                              (digest-note-terminal digest :paused +paused-detail+ now)))
                           (:queued
                            (digest-note-terminal digest :paused +held-detail+ now))))))
                 (when running
                   (nlk:with-handlers ((error (condition) (warn-lane host lane "pause notice" condition)))
                     (flush-now host lane))))))
  ;; What runs now, or nothing: the tick's last write (SAVE-RUNNING-LANES)
  ;; may name a turn that has ended since.
  (when (nlk:store-open-p)
    (if entries
        (nlk:session-state-put (handoff-session host) +handoff-key+
                               (nlk:encode-json-object (coerce entries 'vector)))
        (nlk:session-state-delete (handoff-session host) +handoff-key+)))
  ;; The queue as it stands, each ask with the line just settled.
  (save-queued-asks host))

(defun take-back-lanes (host &aux (session (handoff-session host)))
  "Starting: take back the lanes a stop wrote down (HAND-OFF-LANES), each
watching the turn the boot resumed. => how many were taken back."
  (let ((json (and (nlk:store-open-p) (nlk:session-state-get session +handoff-key+))))
    (if (null json)
        0
        (progn
          (nlk:session-state-delete session +handoff-key+)
          (loop for entry across (nlk:decode-json json)
                count (nlk:with-handlers ((error (condition)
                                            (warn "~a: a lane a stop handed on was not taken back: ~a"
                                                  (host-id host) condition)
                                            nil))
                        (take-back-lane host entry)))))))

(defun take-back-lane (host entry &aux (turn (nlk:json-value entry :string "turn"))
                                       (owner (nlk:json-value entry :string "owner"))
                                       (status-id (nlk:json-value entry :string "status")))
  "One lane from HAND-OFF-LANES' ENTRY: its line, its marks and its slot,
watching the turn TURN."
  ;; The tick does the rest: the resumed turn's facts fold into the digest as
  ;; they come, and one that already ended while nothing watched is found in
  ;; the log (RECONCILE-TERMINAL-DIGEST) and answered.
  (let* ((target (nlk:json-plist (nlk:json-value entry :object "target")))
         (trigger (nlk:json-value entry :string "trigger"))
         (lane (intern-lane host.lanes (nlk:json-value entry :string "session")
                            :target target :trigger-message-id trigger
                            :parent-session-id (nlk:json-value entry :string "parent")
                            :owner-id owner)))
    (nlk:when-let (origin (nlk:json-plist (nlk:json-value entry :object "origin")))
      (setf (lane-origin lane) origin))
    (bt2:with-lock-held ((lane-lock lane))
      (let ((digest (lane-open-digest lane (- (now-ms) (or (nlk:json-value entry :integer "elapsed") 0)))))
        (setf digest.turn-id turn
              digest.status-id status-id
              digest.ask-id (nlk:json-value entry :string "ask")
              digest.task (nlk:json-value entry :string "task")
              digest.tool-calls (or (nlk:json-value entry :integer "calls") 0)
              lane.active-turn-id turn
              lane.where (nlk:json-value entry :string "where")
              lane.prompt (nlk:json-value entry :string "prompt")
              lane.said (or (nlk:json-value entry :string "said") "")
              lane.agent (nlk:json-value entry :string "agent")
              lane.reactions (loop for pair across (or (nlk:json-value entry :array "reactions") #())
                                   collect (cons (aref pair 0) (aref pair 1))))
        (dolist (mark lane.reactions)
          (setf (gethash (list lane (car mark)) *reaction-owners*) turn))))
    (bind-addresses host lane target (remove nil (list trigger status-id)))
    (with-room-book (book host) (take-slot book (make-ask :owner-id owner)))
    lane))

(defun adopt-lane (host session-id &aux (prefix (platform-session-prefix host.platform)))
  "Intern SESSION-ID — a lane of HOST's the reaper let go, running a turn
again — from what its id says: where it lives and the message it opened
on. => the lane, or NIL for an id that names no lane of HOST."
  ;; The turn is running already, so the lane holds a slot like any other
  ;; running lane. The ask of a lane the kit moved into a thread sits in the
  ;; channel, the thread named after it: that lane's line posts in the thread
  ;; replying to nothing.
  (ppcre:register-groups-bind (room message) ("(?s)^(.+)-m([^-]+)\\z" session-id)
    (let ((target (session-target prefix session-id)))
      (when (and target (nlk:session-exists-p session-id))
        (prog1 (intern-lane host.lanes session-id
                            :target target
                            :trigger-message-id (unless (equal message (getf target :thread-id)) message)
                            :parent-session-id room)
          (with-room-book (book host) (take-slot book (make-ask :owner-id nil))))))))

;;; --- the worker tick, start and stop ------------------------------------------------------

(defparameter +where-section+ "where"
  "The live section naming which surface a lane runs on.")

(defun lane-where-section (session-id)
  "SESSION-ID's `where' live section as ((NAME . TEXT)), or NIL when the id
names no lane of any started host."
  ;; Live, not standing: the sentence names
  ;; the lane's own thread, and a standing section sits ahead of the inherited
  ;; history, where one differing byte costs the whole conversation a re-read
  ;; (nc-private#35). A lane in a thread that opened from another room's
  ;; record is told where the thread begins in it (LANE-THREAD-NOTE).
  (let* ((host (and (stringp session-id)
                    (find-if (lambda (host) (find-lane host.lanes session-id)) *hosts*)))
         (lane (and host (find-lane host.lanes session-id)))
         (where (and lane lane.where (plusp (length lane.where))
                     (format nil "Where you are: ~a" lane.where)))
         (note (and lane (lane-thread-note lane (platform-noun host.platform))))
         (text (format nil "~{~a~^ ~}" (remove nil (list where note)))))
    (when (plusp (length text))
      (list (cons +where-section+ text)))))

(defun live-section-advice ()
  "Advice on NLE::READ-LIVE-SECTIONS appending the asking lane's `where'."
  (lambda (next session-id)
    (append (funcall next session-id)
            (handler-case (lane-where-section session-id) (error () nil)))))

(defun budget-advice (host)
  "Advice on NLE:TURN-BUDGET: a turn in one of HOST's lanes runs under the
channel's turn_budget_minutes -- none at all when that is 0 -- and every
other turn is NEXT's."
  (lambda (next turn)
    (if (find-lane host.lanes (getf turn :session-id))
        (let ((minutes host.turn-budget-minutes))
          (and (plusp minutes) (list :seconds (* 60 minutes))))
        (funcall next turn))))

(defun start-host (host &aux (reap-at (+ (now-ms) 30000))
                             (id (host-id host))
                             ;; Set once the lanes the last image left are back:
                             ;; until then the tick must not write over them.
                             (taken-back nil)
                             (handed :unwritten))
  "Start the delivery pool, observe the :FRAME point and give the lanes'
turns their budget."
  ;; The adapter starts its transport after this and stops it before
  ;; STOP-HOST.
  (setf host.worker
        (start-delivery-worker
         (format nil "channel-~a-deliver" id)
         :workers host.delivery-workers
         ;; The tick plans chrome, beats typing, reaps idle lanes; it never calls out.
         :tick-fn (lambda ()
                    (typing-tick host)
                    (sync-commands host)
                    (map-lanes host.lanes
                               (lambda (lane)
                                 ;; One lane's fold must not cost every other lane
                                 ;; its tick: a definition redefined under a
                                 ;; running host leaves state some lane cannot
                                 ;; read, and the tick that died on it took
                                 ;; every other lane's card, trail and reap
                                 ;; with it.
                                 (nlk:with-handlers ((error (condition)
                                                       (warn-lane host lane "tick fold" condition)))
                                   (unless (reconcile-terminal-digest host lane)
                                     (schedule-flush host lane)))))
                    (let ((now (now-ms)))
                      (when (>= now reap-at) (setf reap-at (+ now 30000)) (reap-lanes host.lanes)))
                    (when taken-back
                      (nlk:with-handlers ((error (condition)
                                            (warn "~a: the running lanes were not written down: ~a" id condition)))
                        (setf handed (save-running-lanes host handed)))))))
  ;; Observe, never veto: NEXT always runs, and a fold that signals is
  ;; contained here — a channel bug must not fail the publish for the TUI
  ;; peer watching the same turn.
  (nle:hook :frame (format nil "channel-~a" id)
            (lambda (op next)
              (handler-case (observe-frame host op)
                (error (condition)
                  (warn "~a frame fold failed: ~a" id condition)))
              (funcall next op)))
  (nle:hook 'nle:turn-budget (format nil "channel-~a" id) (budget-advice host))
  (nle:hook 'nle:name-session (format nil "channel-~a" id) (naming-advice host))
  (pushnew host *hosts*)
  (when (nlk:store-open-p)
    (note-paired host (paired host)))
  (nlk:with-handlers ((error (condition)
                        (warn "~a: the lanes a stop handed on were not taken back: ~a" id condition)))
    (take-back-lanes host))
  ;; Behind the running lanes, which hold their slots, the asks that waited.
  (nlk:with-handlers ((error (condition)
                        (warn "~a: the asks a stop left waiting were not taken back: ~a" id condition)))
    (take-back-queued host))
  (setf taken-back t)
  ;; The organism says it is back in the model's words, as a note this host
  ;; posts in its home room: a frame when it comes (OBSERVE-FRAME), or here,
  ;; when it came before this host started.
  (nlk:when-let (note (nle:organism-note))
    (say-note-home host note))
  host)

(defun stop-host (host)
  (setf *hosts* (remove host *hosts*))
  (nle:unhook :frame (format nil "channel-~a" (host-id host)))
  (nle:unhook 'nle:turn-budget (format nil "channel-~a" (host-id host)))
  (nle:unhook 'nle:name-session (format nil "channel-~a" (host-id host)))
  (when host.worker
    (stop-delivery-worker host.worker))
  (nlk:with-handlers ((error (condition)
                        (warn "~a: the running lanes were not handed on: ~a" (host-id host) condition)))
    (hand-off-lanes host))
  ;; An ask still queued on the stopped worker never runs the release its
  ;; claim waits on; without this, the claim would drop the same message
  ;; redelivered to the next host.
  (sb-ext:with-locked-hash-table (*ask-claims*)
    (loop for key being the hash-keys of *ask-claims*
          when (equal (car key) (host-id host))
            do (remhash key *ask-claims*)))
  t)

(defun run-host (host name lap &key on-start on-stop &aux (id (host-id host)))
  "Run HOST over the transport LAP: the channel reads :starting, ON-START runs,
the host starts, and LAP runs under START-SUPERVISED on the thread
channel-<id>-NAME, each signalled lap leaving the channel disconnected."
  ;; Returns the stop thunk: the lap stops, then the host, then ON-STOP runs
  ;; and the channel reads :stopped.
  (set-channel-status id :state :starting :connected nil
                      :soul (soul-status host.soul-path))
  (when on-start (funcall on-start))
  (start-host host)
  (let ((stop-lap (start-supervised
                   (format nil "channel-~a-~a" id name) lap
                   :on-degraded (lambda (condition)
                                  (set-channel-status
                                   id :connected nil
                                   :detail (princ-to-string condition))))))
    (lambda ()
      (funcall stop-lap)
      (stop-host host)
      (when on-stop (funcall on-stop))
      (set-channel-status id :state :stopped :connected nil)
      t)))
