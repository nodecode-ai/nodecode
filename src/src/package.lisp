;;;; package.lisp --- the terminal core, the kernel/evolved boundary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two packages, one boundary — and the terminal core beneath both. The kernel holds what must run before any
;;;; layer or survive any layer: transport, persistence, the lock, the folder
;;;; loader and its report, the hook primitive, the fresh-process seams. The
;;;; evolved package holds everything the organism is
;;;; expected to rewrite about itself: the prompt, the tool table, the turn
;;;; loop, projections, and UI methods.
;;;;
;;;; The turn loop lives in EVOLVED on purpose. It is the primary mutation
;;;; target — the whole reason this substrate is Lisp is that the agent loop is
;;;; reachable by EVAL at run time. Putting it in the kernel would protect the
;;;; one thing that most needs to change.
;;;;
;;;; sb-ext:lock-package is NOT applied here. Slice A6 owns the lock, so that
;;;; slices A1-A5 can develop against an open image. Symbols are exported ahead
;;;; of their definitions so the boundary is legible from one file — with one
;;;; exception: the names DEFINE-EVENT, DEFINE-ERROR and DEFINE-RECORD synthesize
;;;; by string concatenation are exported by those macros, at the declaration. A mirror
;;;; that has to restate a reader name it never wrote is a mirror that falls
;;;; behind the slot list, which is exactly how it drifted.

(in-package #:cl-user)

;;;; Three packages, one boundary and one floor beneath it. The terminal
;;;; core comes first: the program loop, the decoder, the cell painter and
;;;; the tty, a substrate both packages stand on and neither owns.

(defpackage #:nodecode.term
  ;; Derived from cl-tuition and reduced to what the shells run (see
  ;; src/term/NOTICE).
  (:documentation
   "The terminal core: messages and the model protocol, the width table, the
cell painter, the tty and its modes, the byte decoder, kitty graphics, the
renderer, the program loop.")
  (:use #:cl)
  (:nicknames #:nlt)
  (:export
   ;; --- the model protocol (message.lisp) ---
   #:init #:update #:view #:settle-before-first-frame
   #:wait-for-frames-past
   ;; --- messages ---
   #:message #:defmessage #:+mod-shift+ #:+mod-alt+ #:+mod-ctrl+ #:mod-contains
   #:key-event-code #:key-event-mod #:key-event-text #:key-press-msg
   #:key-press-msg-p #:make-key-press-msg #:paste-msg-p #:paste-msg-text
   #:make-paste-msg #:window-size-msg #:window-size-msg-p
   #:window-size-msg-width #:window-size-msg-height #:make-window-size-msg
   #:mouse-event-x #:mouse-event-y #:mouse-event-mod #:mouse-event-button
   #:mouse-click-msg-p #:make-mouse-click-msg #:mouse-release-msg-p
   #:make-mouse-release-msg #:mouse-motion-msg-p #:make-mouse-motion-msg
   #:mouse-wheel-msg-p #:make-mouse-wheel-msg #:mouse-wheel-direction
   #:mouse-wheel-count #:tick-msg-p #:make-tick-msg #:quit-msg-p #:make-quit-msg
   #:kitty-graphics-msg-p #:kitty-graphics-msg-ok-p #:make-kitty-graphics-msg
   #:truecolor-msg-p #:truecolor-msg-ok-p #:make-truecolor-msg
   #:background-msg-p #:background-msg-rgb #:make-background-msg
   #:terminal-io-msg-p #:terminal-io-msg-thunk #:make-terminal-io-msg
   ;; --- the declarative view: CURSOR, its constructor, predicate and
   ;; readers are exported by DEFINE-RECORD at the declaration ---
   ;; --- commands ---
   #:quit-cmd #:terminal-io-cmd
   ;; --- the width table (width.lisp) ---
   #:codepoint-width
   ;; --- cells (cells.lisp) ---
   #:+attr-bold+ #:+attr-italic+ #:+attr-underline+ #:+attr-faint+
   #:screen-buffer-ref #:paint-rows #:parse-styled-string #:render-diff
   ;; --- the tty and its modes (terminal.lisp) ---
   #:terminal-size #:window-size #:terminal-present-p #:enter-modes
   #:exit-modes #:enter-alt-screen #:exit-alt-screen
   #:enable-mouse #:disable-mouse #:push-kitty-keyboard #:pop-kitty-keyboard
   #:apply-terminal-transitions #:request-truecolor #:request-background
   #:*clipboard-use-command* #:copy-to-clipboard #:write-clipboard
   ;; --- the decoder (input.lisp) ---
   #:read-key #:ctrl-char-to-key #:parse-mouse-sequence
   ;; --- kitty graphics (image.lisp) ---
   #:image-media-type #:image-dimensions #:png-dimensions #:file-octets
   #:*png-transcoders* #:transcode-to-png #:transmit-image #:place-image
   #:request-kitty-graphics
   ;; --- the renderer (render.lisp) ---
   #:make-renderer #:render #:renderer-width #:renderer-height
   #:renderer-output-stream #:renderer-last-view-state
   ;; --- the program loop (program.lisp) ---
   #:program #:program-p #:make-program #:run #:send #:program-running
   #:*keep-terminal* #:release-terminal #:*inherit-screen*
   #:release-inherited-modes #:exit-raw-mode #:*frames-on-screen*
   #:program-tty-stream #:program-model
   #:program-renderer #:program-error-handler #:program-worker #:start-worker
   #:stop-worker #:program-loop-thread #:on-loop-thread-p #:run-command
   #:handle-messages #:event-loop #:coalesce-wheel #:*frame-period-seconds*
   #:report-error))

(defpackage #:nodecode.kernel
  (:documentation
   "The floor: the machinery every layer stands on and none may redefine. Locked at boot.")
  (:use #:cl)
  ;; FILE-OCTETS is defined in os.lisp and exported from the terminal core,
  ;; whose image seams read with it.
  (:import-from #:nodecode.term #:file-octets)
  (:nicknames #:nlk)
  (:export
   ;; --- the refusal-declaration waist (conditions.lisp) ---
   #:define-error #:define-record #:access #:when-let #:if-let #:bind #:with-cleanup
   #:with-handlers #:define-errors #:define-refusal #:lambda-ignoring
   #:dispatch #:spawn #:parallel-map #:define-side-table
   ;; --- startup-resolved paths (paths.lisp) ---
   #:home-path #:expand-home #:home-abbreviated
   ;; worlds: the other coding agents' homes, and the stat-only probe
   #:*agent-worlds* #:find-agent-world #:agent-world-names #:agent-home-path
   #:detect-agent-worlds #:cache-path
   #:define-startup-parameter #:resolve-startup-parameters #:runtime-directory
   #:*source-root* #:*ephemeral* #:publish-ephemeral #:*pristine* #:publish-pristine
   ;; the home, and the profile it is (paths.lisp)
   #:home #:*home* #:*profile-flag* #:profile-name #:profile-name-p
   #:profile-named-p #:profile-label #:profile-home
   #:default-home #:sticky-profile #:locate-home
   #:publish-home #:adopt-profile #:+reserved-profile-names+
   #:*profiles-directory* #:*active-profile-path* #:symlink-target
   #:make-symlink #:copy-tree-preserving #:remove-tree
   ;; --- a command line's words (verbs.lisp) ---
   #:find-verb #:help-flag-p #:edit-distance #:nearest-words #:unknown-word-text
   #:verbs-text #:verb-help-text
   ;; --- profiles: several homes, one binary (organism/profile.lisp) ---
   #:profiles #:find-profile #:profile-create #:profile-delete #:profile-use
   #:profile-switch #:*profile-switch-performer* #:*profile-switch-parked*
   #:export-home #:profile-export #:profile-import #:profile-install #:profile-update
   #:profile-command #:*profile-verbs* #:require-profile #:*wrapper-directory*
   #:organism-binary
   ;; --- the gateway: the organism kept running by the operating system
   ;; (organism/background.lisp) ---
   #:*service-manager* #:*unit-directory* #:*unit-runner*
   #:*gateway-watchdog-seconds* #:systemd-notify #:systemd-ready
   #:systemd-watchdog #:gateway-label #:gateway-unit-name #:gateway-unit-path
   #:gateway-unit-duplicates #:unit-removal-command
   #:gateway-log-path #:gateway-unit-text #:gateway-on #:gateway-off #:temporary-home-p
   #:gateway-off-p #:gateway-restart #:gateway-start #:gateway-start-argv
   #:gateway-installed-p #:gateway-by-default-p #:gateway-process-p #:gateway-log-argv
   #:gateway-log-text #:gateway-report #:gateway-text
   ;; --- the operating system beneath the organism (os.lisp) ---
   #:platform #:architecture #:disable-huge-pages #:cap-malloc-arenas #:resident-bytes
   #:skip-openssl-exit-cleanup #:resolve-names-directly
   #:mapped-resident-bytes #:release-mapped-pages #:trim-working-set #:*shell* #:shell-argv
   #:*session-wrapper* #:session-argv #:pid-live-p #:kill-tree #:spawn-detached
   #:exits-zero-p
   #:environment-with #:spawn-program #:wait-for-child #:child-exit #:run-bounded #:lock-file
   #:file-locked-p #:unlock-file #:folder-lock-path #:lock-folder #:unlock-folder #:file-bytes
   #:with-folder-lock #:replace-file #:random-bytes #:with-output-file
   #:call-with-severed-stdio
   ;; --- reaching a peer by name (dial.lisp) ---
   #:*dial-stagger-seconds* #:dial-order #:dial #:http #:body-text #:decode-octets #:read-events
   #:sever-websocket
   ;; --- wire-value waist, both directions (json.lisp) ---
   #:octets #:decode-json #:json-value #:json-array #:with-json
   #:wire-reasoning
   #:json-object #:make-json-object
   #:copy-json-object #:encode-json-object #:encode-json-octets #:json-verbatim
   #:json-verbatim-p #:json-verbatim-octets #:json-memo #:make-json-memo
   #:json-memo-chars #:json-memo-items #:json-memo-note #:encode-json-body
   #:set-jsonc-path #:json-plist #:plist-json #:pretty-json #:json-members
   ;; --- declared boundary contracts (contract.lisp) ---
   ;; CONTRACT, CONTRACT-P and the readers of both records, and
   ;; CONTRACT-REFUSAL with its readers, are exported by DEFINE-RECORD and
   ;; DEFINE-ERROR at their declarations.
   #:define-contract #:declare-contract #:contract-decode
   #:contract-encode #:contract-schema
   ;; --- typed config-section accessors (config.lisp) ---
   ;; CONFIG-REFUSAL and its reader are exported by DEFINE-ERROR at the
   ;; declaration.
   #:config-error #:section-value #:config-string #:config-integer
   #:config-boolean #:config-enum
   ;; declared sections (config.lisp): one declaration, four readers
   #:define-section #:declare-section #:sections #:find-section #:section-key
   #:section-find-field #:section-table #:field-present-p #:section-problems
   #:section-enabled-p #:section-settings
   #:section-needs-setup-p #:section-secret-pair #:secret-pair-noun
   #:section-needs #:section-summary #:section-text #:config-string-list
   ;; --- plain helpers every folder reaches for (text.lisp) ---
   #:one-line #:clip #:narrow-string #:safe-condition-report #:*condition-report-budget* #:lines
   #:distinct #:iso-universal #:read-text #:append-json-line #:read-json-lines #:trimmed
   #:first-line #:+unix-epoch+ #:unix-now #:iso-time #:parse-duration #:split-words #:utf-8-octets
   #:size-text #:duration-text #:deadline-after #:elapsed-ms #:unused-name #:wait-until #:string-set
   ;; --- compiled deny/allow pattern rules (textrules.lisp) ---
   #:compile-text-rules #:text-rules-refusal
   ;; --- the sync protocol (G0) ---
   #:decode-sync-message #:+max-sync-frame-bytes+ #:check-sync-frame-size
   #:encode-gateway-connect-challenge #:encode-gateway-connect
   #:encode-gateway-connect-ack #:encode-gateway-connect-rejected
   #:encode-client-command #:encode-command-result
   #:encode-session-stream-resumed #:encode-projection-event
   #:encode-unsequenced-frame
   #:+unsequenced-frame-kinds+
   ;; --- invocation provenance ---
   #:make-invocation #:make-invocation-source #:make-invocation-transport #:default-invocation
   #:validate-invocation #:invocation-json-object #:invocation-error
   ;; --- store lifecycle (A3) ---
   ;; History lives in the event log. The image holds the active turn only.
   #:open-store #:close-store #:store-path #:store-open-p #:after-commit
   ;; --- durable session-state KV + harness sections (C1) ---
   #:session-state-put #:session-state-get
   #:session-state-delete
   #:set-harness-section #:get-harness-section #:clear-harness-section
   #:list-harness-sections #:harness-section-block
   #:session-state-quota-exceeded #:state-value-too-large
   #:harness-budget-exceeded #:invalid-harness-key
   ;; --- the exact request bodies, kept as deltas (bodies.lisp) ---
   #:keep-request-body #:kept-request-body #:last-request-body #:request-bodies-size
   #:encode-delta
   ;; --- event log: canonical command/event envelope ---
   ;; EVENT and its envelope readers are exported at the declaration.
   #:unknown-event #:record-event #:adopt-event #:+event-envelope-columns+
   #:make-durable-id
   ;; the clock and the hash every folder stamps with (events.lisp)
   #:iso-now #:sha256-text #:short-digest #:events #:page-events #:page-events-before
   #:committed-events-after #:session-replay-coverage #:event-count
   #:event-kind-replay-suppressed-p
   ;; --- session identity ---
   ;; Create is the command. SESSION.CREATED is the fact. Intern is a cache.
   #:create-session #:find-session #:session-exists-p
   ;; The folder an operator names for a new session, there to work in.
   #:check-session-cwd
   ;; A session follows its folder when that moves: SESSION.MOVED is the fact.
   #:move-sessions
   ;; A session minted before its first block is standing by (standby.lisp):
   ;; the id lives in memory and the log stays quiet until a block lands.
   #:standby-session #:on-standby-p #:standby-session-record
   #:materialize-standby-session #:list-sessions #:session-waits
   #:session-tree-order #:record-session-model-selection
   #:session-model-selection #:session-model-effort
   ;; A session whole, its children with it (store/sessions.lisp).
   #:delete-session #:prune-picks #:export-sessions #:import-sessions
   ;; --- durable command and turn lifecycle ---
   #:current-definition-revision #:admit-command #:admit-turn #:admit-wake-turn
   #:admit-active-turn-input #:withdraw-pending-prompt
   #:turn-lifecycle-error #:active-turn-p
   ;; Where a turn was admitted from -- "websocket", "cron", a channel's
   ;; name: the roster shows it, the evaluator choice reads it, and reads
   ;; the session's first turn's too.
   #:session-source-kind
   #:active-turn-starts #:any-active-turn-p #:retained-history-floor #:retained-history-floors
   #:session-prompt-facts
   #:advance-history-floor #:complete-turn #:record-assistant-message
   #:record-exchange-turn #:turn-origin #:fail-turn #:request-cancel-turn
   ;; TURN-CANCELLED-CONDITION, with its readers, is exported by DEFINE-ERROR
   ;; at its declaration.
   #:turn-assistant-round-count
   #:ensure-turn-not-cancelled
   #:with-cancellable-wait #:turn-cancellation-requested-p
   #:turn-retract-requested-p #:turn-cancel-cause #:turn-terminal-kind #:turn-answer #:steer-parked-p
   ;; A fact's recorder (DEFINE-EVENT's :RECORD) is exported at its declaration.
   #:cancel-turn #:+tool-command-keys+
   #:tool-command-string #:deref-literal-refs #:record-background-shell-exit
   #:exited-shell-document
   #:observe-context-window-floor #:observe-context-window-ceiling
   #:context-window-observation #:duplicate-turn-command #:turn-already-active
   #:turn-already-terminal
   ;; --- collection scheduling (A3) ---
   ;; Full GC is forced where a pause is invisible: after a provider request is
   ;; dispatched, before the response lands.
   #:cap-nursery #:with-nursery #:*shell-nursery* #:cap-garbage #:release-card-marks #:trim-memory #:collect-while-idle
   #:note-operator-input
   ;; One line per boot landmark when NODECODE_PHASE_LOG names a file.
   #:note-boot-phase #:note-phase #:await-warm-verification
   ;; Store endpoint publication, and the wait a shell does for it.
   #:collect-during-wait
   #:gc-run-time #:collection-stats #:with-gc-attribution #:*attribution-label*
   #:gc-attribution-stats #:reset-gc-attribution
   ;; --- around-hooks at named points and around any function (hook.lisp) ---
   #:*hooks* #:hook #:define-hook #:unhook #:call-hooked
   ;; the shapes a hook takes, written once (hook.lisp)
   #:observer #:frame-fact #:fact-message-content
   ;; --- the folder: cell load, report, lifecycle (cells.lisp) ---
   #:layer #:cells #:load-cells #:boot-cells #:describe-cells #:find-cell
   #:note-standing #:standing-notices #:standing-notices-text
   #:note-said #:notice-log #:+notice-log-size+
   #:cells-report-text #:cells-folder-text #:broken-cell-links #:broken-layer-files
   ;; the shipped catalog, the hub's, and installing from them (cells.lisp)
   #:shipped-cells-directory #:shipped-cells #:find-offer #:offer-closure
   #:cell-dependents #:system-cell #:hub-pin
   #:hub-cells #:read-hub-index #:*hub-index-url* #:*community-index-url*
   #:missing-shipped-cells #:install-cell #:uninstall-cell
   #:installed-cell-p #:load-cell #:definitions-before #:report-unkept-changes #:folder-name
   ;; what a peripheral is made of, and what it runs on (cells.lisp)
   #:define-peripheral #:worker-start #:worker-poke
   #:worker-stop #:find-session-cwd #:session-project-root
   ;; --- the floor (evolve.lisp) ---
   #:lock-floor #:unlock-floor #:engage-floor #:lower-floor
   ;; --- the scribe: definitions recognized, read, recorded ---
   ;; The layer write at the eval seam and the reads over the live
   ;; definition. Plain functions the organism reaches via eval.
   #:record-definitions #:*scribe-session-id* #:definition-location
   #:definition-text #:definition-prose #:definition-key
   #:forget-shipped-definitions #:layer-directory #:toplevel-definitions
   #:load-source #:source-loaded #:*unit-operators*
   ;; the file write and the git a folder's repository is kept with (scribe.lisp)
   #:write-file-atomically #:git #:git-exclude-dot-directories #:git-commit-all
   ;; --- knowledge: the cells nothing loads, the one index, the use ledger
   ;; (knowledge.lisp); the entry's readers export at its declaration ---
   #:knowledge-folder-p #:knowledge-directory #:definition-folders #:folder-label
   #:knowledge-form-p #:definition-docstring #:knowledge-entries #:knowledge-entry-named
   #:knowledge-surface #:knowledge-search #:knowledge-rows #:knowledge-meter
   #:knowledge-help-text #:note-knowledge-use #:knowledge-ledger-lines #:knowledge-stats #:import-session
   #:mark-index-refresh #:pin-index-section
   ;; --- provisional definitions: the stack as evidence (provisional.lisp) ---
   ;; A redefinition of a name the caller is standing on is held until it
   ;; has run once; the boot snapshot is what a revert puts back.
   #:backtrace-names #:name-on-stack-p #:*caller-frames* #:*evaluating*
   #:*pristine-definitions* #:definition-escrow #:hold-definitions
   #:held-definitions #:revert #:*session-wake*
   ;; the battery the trial child runs over the whole folder (provisional.lisp)
   #:overrides-take-their-callers-calls #:probe-overrides
   ;; --- generations: the self sealed as a numbered manifest of the binary
   ;; and every loaded folder at its revision, proved by the keeper's trial
   ;; child before its image is built; the list (generation.lisp) ---
   #:generations #:generations-directory #:self-rows #:self-identity
   #:manifests #:manifest-count #:sealed-generation #:seal-generation #:prove-self #:refused-key-p
   ;; the restore (every row back to a manifest), the release pin it leaves,
   ;; and the marker the boot that runs a restore takes
   #:restore-plan #:restore-generation #:take-restoring-marker
   ;; ... a generation laid whole beside the folder, a move between two in
   ;; words, and a form run in a fresh process of the organism under a
   ;; throwaway home (launch.lisp): what a bisect over generations stands on
   #:build-generation #:manifest-number #:generation-changes #:trial
   ;; ... read ahead: each folder's step, why one can never restore, the
   ;; edits that hold it; the history the shell lists; the edits kept or set
   ;; aside
   #:restore-steps #:step-refusals #:step-edits #:plan-lines #:generation-history
   #:edits-rows #:edit-paths #:keep-edits #:discard-edits
   ;; the floor: the self receipt a serve leaves before its load, the death
   ;; marker a launch finds, and the decision to run generation 0
   #:write-self-receipt #:take-self-receipt #:read-death-marker #:decide-pristine
   #:pristine-reason
   ;; --- fork: a turn forks itself, each fork fenced to names of its own;
   ;; back through the generations to the step a check broke; a name's
   ;; versions raced on the image's own calls (fork.lisp) ---
   #:fork #:fork-error #:fork-error-detail #:fork-session-p
   #:fork-collect-progress #:fork-progress-snapshots
   #:*fork-task-input* #:*fork-earlier-input* #:*fork-keep-input* #:*fork-bisect-input*
   ;; --- history recall: the model-facing log read (recall.lisp) ---
   ;; The PRO-LONG read contract the eviction stub advertises: scan clipped,
   ;; fetch one entry whole. RECALL-UNSCOPED is exported at its declaration.
   #:recall-log #:recall-entry #:recall-search
   ;; --- turn tree, head navigation, fork composition (checkpoint.lisp) ---
   #:session-head-turn-id #:+head-move-reason-retract+ #:session-head-stamp
   #:session-lineage #:session-chain-scope #:session-composed-parents #:session-standing-chain
   #:session-standing-owner #:standing-harness-sections
   #:chain-set-from #:session-visible-turn-set
   #:event-visible-p #:navigate-session-head #:turn-end-log-position
   #:session-known-turn-p #:move-session-head #:session-workspace-captures
   #:restore-tree-for-checkpoint #:session-head-tree #:session-checkpoints
   #:checkpoint-tree-order #:checkpoint-changed-files
   #:turn-chain-depth #:session-log-cursor
   #:defer-turn-fact #:settle-turn-facts
   ;; --- workspace snapshots (snapshot.lisp) ---
   #:ensure-snapshot-store #:git-available-p #:capture-workspace-tree
   #:open-workspace-bracket #:close-workspace-bracket #:workspace-diff-document
   #:anchor-tree #:changed-paths #:restore-paths #:hash-in-tree #:repack-store
   #:record-workspace-capture #:checkpoint-capture-boundary
   #:close-detached-workspace-window #:turn-boundary-captured-p
   #:restore-to-checkpoint #:snapshot-git-failed))

(defpackage #:nodecode.evolved
  (:documentation
   "The mutable material. Prompt, tools, turn loop, projections, UI methods.")
  (:use #:cl #:nodecode.kernel)
  (:nicknames #:nle)
  ;; IMAGE-MEDIA-TYPE is the terminal core's (term/image.lisp): what bytes
  ;; are is one question with one answer, asked by LOOK, by the channel
  ;; adapters, and by the carrier that decides what a terminal can be given.
  (:import-from #:nodecode.term #:view #:update #:init #:settle-before-first-frame
                #:image-media-type)
  (:export
   ;; --- session and turn loop (A2) ---
   #:intern-session #:admit #:hydrate-session #:agent-loop #:run #:forget
   ;; --- the model-facing surface (A2) ---
   #:*system-prompt* #:help #:set-help-topic #:*tools* #:register-tool
   #:define-memory #:define-skill #:index
   #:unregister-tool #:eval-string
   #:sh #:$ #:edit #:write-file #:look
   ;; --- what a tool call did (turn.lisp): the record a verb stamps on the
   ;; result fact of the call it runs under — a cell's verb names itself
   ;; to the transcript this way (activity.lisp) — and whether it failed,
   ;; the mark a tool's own verdict or a hook's veto puts on that fact ---
   #:receipt #:failure
   ;; --- what a call did, in words: the classifier the transcript and a
   ;; channel card both read — a title (`Read src/x.lisp'), the live verb a
   ;; bar or card shows (`reading src/x.lisp'), the entries a group counts,
   ;; whether a call is routine enough to fold into a run, and the one
   ;; compact token count (activity.lisp) ---
   #:tool-arguments-activity #:tool-activity-verb #:activity-title
   #:format-token-count
   ;; --- what a turn ran on and spent, in the finish divider's words: a
   ;; usage accumulator's sum (NIL stays unknown), its identity and token
   ;; meter, the prompt its newest round put in the window, and the window
   ;; a model has (ui.lisp, provider.lisp) — the TUI and a channel card read
   ;; one line ---
   #:organism-note #:usage-plus #:usage-identity-text #:usage-meter-text
   #:context-occupancy-tokens #:model-context-tokens
   ;; --- a turn's provider attempt waiting for its first token: the failover
   ;; candidate a slow start would leave for, and the cut a room's press asks
   ;; for (provider.lisp, ATTEMPT-WATCH) ---
   #:provider-attempt-next #:skip-provider-attempt
   ;; --- what an image is, asked of bytes rather than of a filename: the
   ;; format they open, and the ceiling one image may take (exec.lisp). The
   ;; same two answers LOOK gives an operator's file, for a cell facing
   ;; an attachment that arrived over a platform. ---
   #:image-media-type #:*image-max-bytes* #:display-fact
   ;; --- around-hooks, inherited from the kernel (hook.lisp) ---
   #:*hooks* #:hook #:define-hook #:unhook #:observer
   ;; --- one cell, declared (organism/cell.lisp): the composition DSL
   ;; over the primitives, and the generic registration scope under it that
   ;; makes a stop complete, idempotent and free after a failed start ---
   #:define-cell #:starting #:on-stop
   ;; --- the running turn as a value (meter.lisp): which session and turn
   ;; a thread works for, what the turn has spent, and the one policy read
   ;; that gives a session's turns a budget ---
   #:turn #:turn-budget
   ;; --- in-process ingress (gateway.lisp) ---
   #:submit #:turn-worker-unavailable
   ;; --- a session's name (gateway.lisp): the one writer every naming path
   ;; lands on, the operator's and a cell's; and the title a model writes a
   ;; session from its first prompt, which a session's owner advises ---
   #:set-session-title #:name-session
   ;; --- a turn's mark that its answer waits on the person it answers
   ;; (gateway.lisp), which a channel's answer is marked through too ---
   #:answer-waits
   ;; --- a forced eviction (context.lisp): a session's oldest whole turns
   ;; dropped by share or down to a token count, once it holds more than a
   ;; bound — what /evict runs, and what bounds a session nobody sends
   ;; requests on ---
   #:evict-session-context
   ;; --- the organism's own voice (gateway.lisp): every attached shell, the
   ;; standing board the live tail of every round's prompt shows the model ---
   #:notice
   ;; --- work that outlives the shell (gateway.lisp): a cell's one word
   ;; for it, and the kernel turns the background gateway on, once ---
   #:keep-running
   ;; --- the shared operator config (provider.lisp): the one reader, the
   ;; one writer, and the model-facing pair over them ---
   #:read-shared-config #:config-get #:config-set
   ;; --- a failure below HTTP in words (provider.lisp): the one reading of
   ;; a refused port, a name not found, a TLS refusal, for any dialer ---
   #:transport-failure-label
   ;; --- one resolution seam for provider credentials (provider.lisp): what
   ;; a cell answers at the :CREDENTIAL point, where the credential came
   ;; from, and what it carries into a request ---
   #:make-credential #:credential-key #:credential-source #:credential-attributes
   #:credential-error
   ;; --- one bounded HTTP exchange (provider.lisp): what a cell asks a
   ;; host with, a failure answered as a value ---
   #:http-fetch
   ;; --- the model-facing selection verbs: the catalog the shell's /models
   ;; picker shows (client.lisp), everything a pick means, everything a
   ;; /think pick means, and the target a session's next turn binds
   ;; (gateway.lisp) ---
   #:models #:select-model #:select-effort #:gateway-target
   ;; --- the headless slash surface (client.lisp): one command line run
   ;; without a shell against a session, and the catalog it can run — what
   ;; a channel publishes as its menu, a cron job or an eval snippet calls --
   ;; and the one reading of a typed line as a command (slash.lisp) ---
   #:slash #:slash-catalog #:slash-completions #:parse-slash-input
   ;; --- the auxiliary target (provider.lisp): the model side work runs
   ;; on, the pick that moves it, the binding a layer or a cell runs its
   ;; own side call under, and the call itself ---
   #:auxiliary-model #:with-auxiliary-model #:complete
   ;; --- the folder report, inherited from the kernel (cells.lisp), and
   ;; the gateway's lifecycle over it (gateway.lisp) ---
   #:cells #:restart-cells
   ;; --- the sealed generations of the self, inherited from the kernel
   ;; (generation.lisp): what (generations) lists; and the restore to one,
   ;; this organism exiting once idle for the next launch to run it
   ;; (gateway.lisp) ---
   #:generations #:rollback
   ;; --- a local clack acceptor served once it answers, stopped and
   ;; proved gone; a path answered on the gateway's own port (gateway.lisp) ---
   #:serve-local #:stop-clack-handler #:route
   ;; --- this organism's own gateway as a client reaches it: its loopback
   ;; address and the token it takes (gateway.lisp) ---
   #:gateway-endpoint
   ;; --- this machine told to its nodecode.ai account (account-login.lisp) ---
   #:machine-update
   ;; --- a stream read whole up to a bound (exec.lisp) ---
   #:slurp-bounded
   ;; --- projections and UI (A7) ---
   ;; view/update stay generic so the organism can redefine its own interface.
   #:view #:update
   ;; --- reducer and projection (C1) ---
   #:apply-event #:fold-log #:project-state
   ;; --- design system (theme.lisp, R1/R3 render parity) ---
   #:*palette* #:*color-profile* #:style-for-tone #:dim-style #:shadow-style
   #:styled #:break-fragments #:body-gutter #:rgb-to-ansi256 #:rgb-to-ansi16
   #:scrollbar-thumb
   ;; --- theme catalog (theme.lisp, Tungstene overlay parity) ---
   #:default-palette #:theme-catalog #:theme-colors-for-name
   #:palette-from-theme-colors #:configured-theme-name #:engage-theme
   ;; --- syntax highlighting (theme.lisp, syntax_highlight.zig port) ---
   #:tokenize-line #:tokenize-shell #:token-tone
   ;; --- live TUI (C2) ---
   #:chat-model
   ;; --- session internals (C1 tests need these) ---
   #:message #:message-content #:append-message #:finish-turn
   ;; --- gateway server (G0) ---
   #:await-port #:start-gateway #:stop-gateway #:serve-organism-candidate
   ;; --- a cell's slash commands (SLASH-CATALOG, SLASH) ---
   #:register-command #:unregister-commands #:list-dialog
   ;; --- gateway TUI client (G1) ---
   #:gateway-chat-model #:run-chat-gateway #:start-client-organism))

(defpackage #:nodecode.import
  (:documentation "Another coding agent's home brought into this one: readers
per artifact shape, worlds as data (NLK:*AGENT-WORLDS*), a plan of items, and
each item landed through the seam that owns it (src/src/import/).")
  (:use #:cl)
  (:nicknames #:nik)
  (:shadow #:import #:search)
  (:export
   ;; model- and operator-facing verbs
   #:scan #:import #:worlds
   ;; the repositories held as sources: listed, and one taken back
   #:sources #:remove-source
   ;; the one condition
   #:import-error
   ;; the plan, for the wizard, a tool, or a test
   #:make-plan #:apply-plan #:report-text #:plan-json
   ;; the format readers
   #:read-yaml #:read-toml #:read-env-file
   ;; what the gateway serves while it runs: /import, POST /api/import, the watch
   #:run-slash #:import-route #:start-watch #:stop-watch))

(defpackage #:nodecode
  (:documentation "Operator facade. Re-exports the entry points, owns nothing.")
  (:use #:cl)
  (:import-from #:nodecode.evolved #:run #:forget)
  (:export #:run #:forget))

(defpackage #:nodecode.test
  (:documentation "Test runner. Each slice lands its own test file.")
  (:use #:cl)
  (:export #:run-all #:deftest #:is #:signals-error))
