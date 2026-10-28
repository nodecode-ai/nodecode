;;;; nodecode.asd --- system definition for the Lisp organism.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; This system is the product. The Zig tree it grew up beside was moved out
;;;; of this repository; nothing here reaches into it, and
;;;; the root justfile's plain recipes (build, test, release, lint, check) are
;;;; this system's.
;;;;
;;;; Layout: src/ groups one-owner-per-concern files into domain directories
;;;; (term, waist, wire, store, session, organism, engine, tui, surface); the
;;;; kernel/evolved boundary declaration stays at src/package.lisp. The
;;;; component lists below are the canonical load order — the directory is a
;;;; file's address, never its position. test/ mirrors the same grouping.
;;;; Components are named explicitly so a misplaced file fails the load
;;;; loudly instead of being silently skipped.

;;; --- the version this system declares -----------------------------------------
;;; The version carries the commit, not just the base: a checkout has git to
;;; ask — the short HEAD of the tree this file is loaded from — and a packed
;;; build is stamped with the revision it was cut from. src/tools/pack.sh
;;; builds a `git archive' of a revision in a throwaway tree with no .git and
;;; sets NODECODE_BUILD_COMMIT, which outranks git precisely because git cannot
;;; answer there. One rule, so the form every reader reaches for —
;;; `(asdf:component-version (asdf:find-system "nodecode"))' — and the image's
;;; *USER-AGENT-VERSION* (engine/provider.lisp) never disagree.

(defun nodecode-version (declared)
  "DECLARED — the base the defsystem form below declares — with the commit
this build names riding it: the build's own stamp first, then the short
commit of the tree git is asked in. A tree that can name neither reports the
bare base, which is the one honest thing left to say. Any +suffix DECLARED
already carries is dropped first, so reloading this file recomputes instead
of stacking."
  (flet ((word (text &aux (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
           (and (plusp (length trimmed)) trimmed)))
    (let ((base (subseq declared 0 (position #\+ declared)))
          (commit (or (word (uiop:getenv "NODECODE_BUILD_COMMIT"))
                      (ignore-errors
                       (word (uiop:run-program '("git" "rev-parse" "--short" "HEAD")
                                               :directory (or (ignore-errors
                                                               (asdf:system-source-directory "nodecode"))
                                                              (uiop:getcwd))
                                               :output :string
                                               :error-output nil
                                               :ignore-error-status t))))))
      (format nil "~a~@[+~a~]" base commit))))

(defsystem "nodecode"
  :description "A self-modifying agent organism in SBCL: gateway, session, turn loop, and heredity."
  :license "MIT"
  :version "0.1.3"
  ;; Dependency amendment seam.
  ;; A1 added: shasht (JSON codec) and two SBCL contribs.
  ;;
  ;; The contribs are deliberate: both load zero entries into
  ;; sb-sys:*shared-objects*, which keeps SAVE-LISP-AND-DIE (A6) and fork (A4)
  ;; free of foreign library state. sb-bsd-sockets is what the gateway suite
  ;; binds an ephemeral port through to learn a free one; sb-posix carries the
  ;; fcntl lease, chmod, and process seams.
  ;; A2 added: dexador (provider HTTP) and uiop (env lookup; already present as
  ;; an ASDF dependency, named for clarity).
  ;; A3 added: sqlite (cl-sqlite, durable transcript). It loads libsqlite3, and a
  ;; connection holds a C pointer in the heap, so store.lisp closes the handle on
  ;; SB-EXT:*SAVE-HOOKS* and disowns it after fork. See that file's header.
  ;; cl+ssl is named because the event log's SHA-256 is libcrypto's
  ;; EVP_Digest (store/events.lisp), which cl+ssl loads for TLS at every
  ;; start (it was already in the image through dexador). No ironclad system
  ;; is named: the cells that use one name it, and nodecode/bin carries
  ;; them.
  ;; G0 added: clack + clack-handler-hunchentoot (HTTP/WS server) and
  ;; websocket-driver (RFC-6455 framing). clack-handler-hunchentoot must be
  ;; named explicitly so clackup resolves :hunchentoot; websocket-driver
  ;; provides the wsd: nickname used by gateway.lisp and client.lisp.
  ;; textrules added: cl-ppcre. The waist's pattern-rule engine needs real
  ;; word boundaries and match POSITIONS — allow-overrides are span-scoped
  ;; rather than whole-text (see waist/textrules.lisp) — so SEARCH cannot
  ;; serve it.
  ;; term: bordeaux-threads is named because the terminal core's worker and
  ;; input thread call bt2 directly (it was already in the image through
  ;; clack, so nothing new is fetched). It loads no shared object; neither
  ;; does the core's one sb-alien ioctl. sb-concurrency is SBCL's own
  ;; mailbox, the queue under the terminal core's loop and the channel
  ;; kit's workers: a pure-Lisp contrib, no shared object.
  ;; The tuition dependency left with the cutover to src/term/, and with it
  ;; serapeum, trivial-channels and version-string, which nothing else in the
  ;; image pulls.
  ;; flexi-streams is named because READ-EVENTS decodes a binary event
  ;; stream through it (it was already in the image through hunchentoot, so
  ;; nothing new is fetched); alexandria because the waist and the cells
  ;; read and write whole files through it (it came in through dexador).
  :depends-on ("bordeaux-threads"
               "shasht"
               "cl-ppcre"
               "dexador"
               "flexi-streams"
               "alexandria"
               "sqlite"
               "cl+ssl"
               "cl-base64"
               "uiop"
               "clack"
               "clack-handler-hunchentoot"
               "websocket-driver"
               (:require "sb-bsd-sockets")
               (:require "sb-posix")
               (:require "sb-concurrency"))
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     ;; conditions precedes everything with a refusal to declare, which is
     ;; every file below: DEFINE-ERROR is the one place a condition's slot,
     ;; initarg, reader, and report argument agree by construction.
     (:file "waist/conditions")
     ;; paths: startup-resolved home and source roots. Every
     ;; file below that names a file under $HOME declares it through
     ;; DEFINE-STARTUP-PARAMETER.
     (:file "waist/paths")
     ;; os: the shell, processes, locks, files — one arm per platform,
     ;; behind names that say what is needed, never where.
     (:file "waist/os")
     ;; dial: how a peer is reached by name -- every address, IPv6 first,
     ;; raced -- installed on usocket's connect before any file below dials.
     (:file "waist/dial")
     ;; json precedes everything: the decoded-wire-value waist (JSON-VALUE,
     ;; WITH-JSON) is what every file below reads foreign payloads through,
     ;; starting with frame's envelope fields.
     (:file "waist/json")
     ;; contract: the strict dialect of the same wire types, declared.
     ;; DEFINE-CONTRACT states one boundary payload once and derives the
     ;; record it materializes, the decoder, the encoder and the JSON-Schema
     ;; projection a catalog advertises. After json, whose +JSON-TYPES+ is
     ;; its type vocabulary; before every boundary that declares one.
     (:file "waist/contract")
     ;; config: typed section accessors over the decoded shared config —
     ;; the one owner of the accessor family the channel kit and guard
     ;; cells previously each carried a copy of.
     (:file "waist/config")
     ;; textrules: compiled deny/allow pattern rules over one text, the
     ;; relocated guard policy engine with the vocabulary stripped.
     (:file "waist/textrules")
     (:file "waist/verbs")
     ;; worlds: where the other coding agents keep their homes, and the
     ;; stat-only probe over that table. Data the first frame reads before
     ;; any folder loads; the import folder's readers stand on it.
     (:file "waist/worlds")
     ;; text: the plain helpers every folder reaches for -- one line, a cut,
     ;; the lines, the distinct ones, a file's text, a project's root.
     (:file "waist/text")
     (:file "wire/invocation")
     (:file "wire/protocol")
     (:file "store/store")
     ;; standby precedes events: the append gate consults the registry of ids
     ;; minted but not yet durable, and the registry names nothing in events.
     (:file "store/standby")
     (:file "store/events")
     ;; state follows events: the KV appends its DDL after the envelope
     ;; schema and stamps log_seq from the events table.
     (:file "store/state")
     ;; bodies: the exact request bodies as deltas, its table appended to
     ;; the schema after the KV's.
     (:file "store/bodies")
     (:file "session/identity")
     ;; sessions follows identity: a session whole -- deleted, exported,
     ;; imported -- is every table's rows by its id, and asks whether an id
     ;; is a session the way identity answers it.
     (:file "store/sessions")
     (:file "organism/gc")
     ;; hook: the around-chain primitive — points and advice
     ;; on any function name. Kernel floor because peripherals stand on it.
     (:file "organism/hook")
     (:file "organism/evolve")
     (:file "session/lifecycle")
     ;; snapshot precedes checkpoint: the shadow-git store and its durable
     ;; capture fact are the mechanism the turn-tree navigation restores
     ;; through. Both precede session/turn, which hook captures and fold
     ;; visibility.
     (:file "session/snapshot")
     ;; scribe follows evolve and snapshot: the journal and the splice append
     ;; organism facts through heredity's owned path and ride snapshot's git
     ;; seam for the review diff. It precedes turn, whose eval snippet calls the
     ;; journal hook.
     (:file "organism/scribe")
     ;; provisional follows scribe and precedes cells: it defers the
     ;; scribe's own write for a definition that is running on the stack
     ;; that defined it, and holds the pristine snapshot cells takes
     ;; before the first layer file loads.
     (:file "organism/provisional")
     ;; cells: the one folder — load, snapshot, report. It follows
     ;; scribe, whose shipped-definition index answers which overridden
     ;; definitions changed underneath a layer.
     (:file "organism/cells")
     ;; knowledge after cells: a knowledge cell is a folder the loader
     ;; records and never loads, and the index reads the layer the scribe
     ;; writes.
     (:file "organism/knowledge")
     ;; upgrade: what a staged tree changes in this image, classified: reads
     ;; the scribe's definitions and the loader's residue rule, evaluates nothing.
     (:file "organism/upgrade")
     ;; background: the gateway kept running by the operating system — its
     ;; unit, its state, its verbs. It reads the store's owner verdict, and
     ;; precedes profile, whose delete turns a home's gateway off.
     (:file "organism/background")
     ;; profiles read the store's owner verdict, the layer's git, the
     ;; installer's marker: after every one of them, before the launch.
     (:file "organism/profile")
     ;; launch after cells: the warm-image cache execs published images,
     ;; spawns its builder and dump child as fresh processes, and keys on
     ;; what the loader reads.
     (:file "organism/launch")
     ;; generation after launch: the self sealed -- a manifest of the folder
     ;; the keeper's trial child proved, written before its image is built.
     (:file "organism/generation")
     ;; fork after generation: a way back lays generations whole
     ;; (BUILD-GENERATION) and asks them (TRIAL), a race's winner is HELD
     ;; (HOLD-DEFINITIONS); the escrow, the scribe and the unkept report ask
     ;; its fence about a fork's forms by name, at run time.
     (:file "organism/fork")
     ;; recall follows scribe: the model-facing log read defaults its session
     ;; scope to the ambient *scribe-session-id* the eval seam binds.
     (:file "session/recall")
     (:file "session/checkpoint")
     (:file "session/session")
     ;; provider precedes turn: the stream-part waist, lane registry, and
     ;; frozen-config structs are the layer the turn loop folds policy over.
     (:file "engine/provider")
     ;; meter precedes context and turn: the running turn as a value -- what
     ;; it spent, the budget it runs under, the spend line a tool result
     ;; carries -- is the record RUN binds and the loop folds, the line the
     ;; history fold renders, and the budget exec's waits read.
     (:file "engine/meter")
     ;; context precedes turn: the context engine
     ;; (log->events->units->boundary->compiled context) is a pure layer
     ;; between provider's frozen structs and the loop's ambient-state freeze.
     (:file "engine/context")
     ;; Revisions reuse context units; the request loop publishes their working copy.
     (:file "engine/working-context")
     (:file "engine/turn")
     ;; shell precedes exec: the grammar of what (sh ...) runs — a command
     ;; string as its simple commands, each with words, redirects and the
     ;; substitution mark — is one pure authority, read by exec's read-only
     ;; classifier and by activity's transcript words.
     (:file "engine/shell")
     ;; exec follows turn: the background-shell registry registers its tools
     ;; through turn's *tools* seam, and its pure band-preview material is
     ;; consumed by ui and client.
     (:file "engine/exec")
     ;; activity follows exec: the words a tool call's transcript card, the
     ;; live bar and the roster say, read from the snippet and the receipts its
     ;; verbs stamp; ui, client and agents consume it.
     (:file "engine/activity")
     ;; term: the terminal core — the message vocabulary and model protocol,
     ;; the width table, the cell painter, the tty and its modes, the byte
     ;; decoder, kitty graphics, the renderer, the program loop. It precedes
     ;; tui: theme measures through its width table and ui runs its loop.
     (:file "term/message")
     (:file "term/width")
     (:file "term/cells")
     (:file "term/terminal")
     (:file "term/input")
     (:file "term/image")
     (:file "term/render")
     (:file "term/program")
     ;; theme precedes ui: the design system (palette, wrap, gutters) is pure
     ;; material the display policy consumes.
     (:file "tui/theme")
     ;; editor follows theme: the composer's editing engine wraps through
     ;; BREAK-FRAGMENTS and measures through TEXT-DISPLAY-WIDTH, theme's
     ;; authorities; ui constructs it and paints its rows.
     (:file "tui/editor")
     ;; slash precedes ui: the command catalog/parser is pure material both
     ;; shells consume; the execution methods live with the shells.
     (:file "tui/slash")
     ;; keys precedes ui: dialog search, the keybind keymap, the composer
     ;; catalogs, and paste normalization are pure input material the update
     ;; function consults.
     (:file "tui/keys")
     (:file "tui/ui")
     (:file "surface/gateway")
     ;; One debugger boundary for raw, cell and product workers; gateway owns notices.
     (:file "tui/worker")
     ;; organism/cell: the cell composition DSL (STARTING/ON-STOP and
     ;; DEFINE-CELL). Its address is organism/ -- it is what one cell
     ;; declares about itself, beside the folder loader -- and its load
     ;; position is here, after gateway: the declaration composes
     ;; REGISTER-COMMAND and UNREGISTER-EXTENSION, which gateway owns.
     (:file "organism/cell")
     ;; activity: which sessions are running a turn right now, projected for
     ;; the shells -- it folds the frames PUBLISH-EVENT lets out.
     (:file "surface/activity")
     ;; notify: a desktop notice when a turn ends, the second reader of the
     ;; frames PUBLISH-EVENT lets out.
     (:file "surface/notify")
     (:file "surface/health")
     (:file "surface/client")
     (:file "tui/timestamps")
     ;; rpc: `nodecode rpc', a program's shell -- the gateway's commands on
     ;; its stdin, the gateway's frames on its stdout. After client: it
     ;; finds, starts and attaches to the organism the way a shell does.
     (:file "surface/rpc")
     ;; rows: a session's transcript folded here, by the shell's reducer, for
     ;; a client that renders and does not fold. After client: it folds with
     ;; APPLY-GATEWAY-EVENT and subscribes the way a shell does.
     (:file "surface/rows")
     ;; observe: where the time and money went -- one session's timeline
     ;; and the usage across sessions, folded from the facts the turn loop
     ;; records, for the web page; its routes are gateway routes.
     (:file "surface/observe")
     ;; files: a session's own folder for the web page's Files view --
     ;; listed, read, saved and added to, never past the folder; its route
     ;; is a gateway route.
     (:file "surface/files")
     ;; update: a shipped release reaches the running organism -- staged
     ;; through the installer, classified, converged or restarted; the
     ;; verb, the route, the poller and the watcher. After client: the
     ;; watcher and the verb call the gateway the way a shell does.
     (:file "surface/update")
     ;; setup follows client: the walk chains the shell's own dialogs
     ;; (provider/model/effort pickers) and the writers they already own.
     (:file "surface/setup")
     ;; settings: the declared sections read and written for the page's
     ;; Settings tab and the shell's /settings, a pasted secret kept in a
     ;; file of its own.
     (:file "surface/settings")
     ;; sheet: the canvas, inks, pointer targets and key-row line every
     ;; dialog in the middle of the frame paints and calls with.
     (:file "tui/sheet")
     ;; The TUI's quick config knobs use the same typed config route as the page.
     (:file "tui/settings")
     ;; /generations: the history of the self, a sheet over the generations
     ;; routes (gateway.lisp), and the way back to a change in it.
     (:file "tui/generations")
     ;; control: the operator's own routes for the web page's Control pane --
     ;; how the organism runs, its log, its keys, its cells -- each a shell
     ;; over the owner a shell's verb already calls; its cell listing is
     ;; what the page and the shell's /cells both read.
     (:file "surface/control")
     ;; /cells: every cell on a sheet over the cells routes (control.lisp),
     ;; the chosen one laid open beside the list.
     (:file "tui/cells")
     ;; status composes those readers; the shell and headless format one projection.
     (:file "surface/status")
     ;; account login is a CLI child over the HTTP account-link contract and canonical config/key writers.
     (:file "surface/account-login")
     ;; connection: the footer's green or red model name, one measured handshake a model pick
     ;; and the provider terms.
     (:file "surface/connection")
     ;; backup: this home kept whole and brought back as a profile -- the
     ;; store read through health's reader, the archive profile.lisp's shape.
     (:file "surface/backup")
     ;; import: another coding agent's home brought into this one. Readers
     ;; per artifact shape (yaml, toml, shape), the conventions that say
     ;; which files to open (home), a plan of items, and each landed through
     ;; the seam that owns it -- the config and auth writers, the scribe --
     ;; then the verbs: /import and POST /api/import, which SERVE-GATEWAY
     ;; registers as it serves.
     (:file "import/conditions")
     (:file "import/seams")
     (:file "import/yaml")
     (:file "import/toml")
     (:file "import/catalog")
     (:file "import/shape")
     (:file "import/home")
     (:file "import/sessions")
     (:file "import/tally")
     (:file "import/takeover")
     (:file "import/plan")
     (:file "import/source")
     (:file "import/verbs"))))
  :in-order-to ((test-op (test-op "nodecode/test"))))

;;; DEFSYSTEM stores :version literally — a form there is refused as an
;;; invalid specifier — so the commit rides the declared version here, where
;;; loading this file (and every reload of it) recomputes it:
;;; `(asdf:component-version (asdf:find-system "nodecode"))' => "0.0.1+<commit>".

(let ((system (asdf:registered-system "nodecode")))
  (setf (asdf:component-version system)
        (nodecode-version (asdf:component-version system))))

(defsystem "nodecode/bin"
  :description "Standalone executable: the organism dumped with SAVE-LISP-AND-DIE."
  :license "MIT"
  ;; The released image is the only Lisp available on a target machine: a
  ;; shipped cell's third-party system must already be in it. Without this,
  ;; ASDF would look for its source under the build host's Quicklisp and the
  ;; load would die on a missing TRUENAME. The core names no ironclad system
  ;; (see above); the channel kit's digest and Discord voice's cipher and
  ;; AEAD ride here, bin-only, so check and the developer image still skip
  ;; compiling them. All three must be present; without one, that system
  ;; would compile from Quicklisp at every launch on an empty cache. The
  ;; context skill is a built-in cell: even an empty home or --pristine
  ;; retains its protected standing instruction. No optional folder is required.
  :depends-on ("nodecode" "nodecode-context" "ironclad/digest/sha256" "ironclad/cipher/aes"
               "ironclad/aead/gcm")
  ;; program-op is the same save-lisp-and-die seam A6 (heredity) builds on; the
  ;; dependency comments on the main system track which systems keep that dump
  ;; free of foreign state. build-pathname is relative to this file — and
  ;; exempt from ASDF's cache output-translations, as all image dumps are — so
  ;; the binary lands at the repo root next to the Zig ./nodecode launcher.
  ;; Built via `just release`.
  :build-operation "program-op"
  :build-pathname "../nodecode"
  ;; warm.lisp runs the launch path once as the last act of the load, so
  ;; the dump carries warm dispatch caches instead of billing the
  ;; operator's first launch for them. Bin-only: the developer image and
  ;; the test image never load it.
  :components ((:file "src/warm"))
  :entry-point "nodecode.evolved::main")

(defsystem "nodecode/test"
  :description "Test system for nodecode. Directories mirror src/ domains; the list is the load order."
  :license "MIT"
  :depends-on ("nodecode")
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "support")
     ;; The cell lanes' shared runner and helpers (DEFINE-TEST-SLICE), one
     ;; copy for every cell test system that depends on this one.
     (:file "../tools/test-slice")
     ;; json-test first: the decoded-wire-value waist is what every later
     ;; suite's payload assertions read through.
     (:file "waist/json-test")
     ;; contract-test follows json-test: the declaration it gates stands on
     ;; the decoded-wire-value waist that suite locks.
     (:file "waist/conditions-test")
     (:file "waist/contract-test")
     (:file "waist/config-test")
     (:file "waist/os-test")
     (:file "waist/dial-test")
     (:file "waist/paths-test")
     (:file "waist/verbs-test")
     (:file "waist/textrules-test")
     (:file "waist/text-test")
     (:file "store/store-test")
     (:file "store/reader-test")
     (:file "store/events-test")
     (:file "store/state-test")
     (:file "store/bodies-test")
     (:file "store/sessions-test")
     (:file "wire/protocol-test")
     (:file "wire/invocation-test")
     (:file "session/identity-test")
     (:file "session/move-test")
     ;; standby-test follows identity-test: it drives the same session
     ;; fixtures, and asserts what a minted-but-unspoken id did NOT write.
     (:file "session/standby-test")
     (:file "session/lifecycle-test")
     (:file "session/session-admit-test")
     (:file "organism/evolve-test")
     (:file "organism/cells-test")
     (:file "organism/notice-log-test")
     (:file "organism/cell-test")
     (:file "organism/upgrade-test")
     (:file "organism/profile-test")
     (:file "organism/background-test")
     (:file "organism/warm-test")
     ;; generation-test follows warm-test: it seals the warm fixture's folder
     ;; through a shell fixture playing the trial child, and lends that
     ;; fixture to the keeper's gates below.
     (:file "organism/generation-test")
     (:file "organism/fork-test")
     (:file "organism/fork-progress-test")
     ;; launch-keeper-test follows warm-test: it drives the same warm fixtures
     ;; for the keeper and the layer cache's own pruning.
     (:file "organism/launch-keeper-test")
     ;; hook-test follows cells-test: its two-folder advice proof drives the
     ;; same folder fixtures.
     (:file "organism/hook-test")
     ;; scribe-test follows evolve-test: it drives the same genome fixtures
     ;; for staging/retirement.
     (:file "organism/scribe-test")
     ;; provisional-test follows scribe-test: it drives the same fixture
     ;; layer, and what it asserts is which of those writes did NOT happen.
     (:file "organism/provisional-test")
     ;; knowledge-test follows scribe-test: the keeps it drives are the
     ;; scribe's, into a fixture folder of its own (support's
     ;; WITH-KNOWLEDGE-FIXTURE).
     (:file "organism/knowledge-test")
     ;; recall-test follows scribe-test: it drives the store fixtures plus
     ;; support's seeded turns and binds the scribe ambient recall defaults
     ;; to.
     (:file "session/recall-test")
     ;; provider-pool-test precedes turn-test: it locks the connection-reuse
     ;; safety contract the turn loop's provider rounds rely on.
     (:file "engine/provider-pool-test")
     ;; provider-watchdog-test follows provider-pool-test: its cut-socket
     ;; gate reads the pool the same way.
     (:file "engine/provider-watchdog-test")
     (:file "engine/turn-test")
     ;; meter-test follows turn-test: its budget turns drive the tool-call
     ;; seam through turn-test's wire helpers and probe schema.
     (:file "engine/meter-test")
     ;; context-test follows turn-test: it drives the same store fixtures and
     ;; seed helpers.
     (:file "engine/context-test")
     ;; tool-call-id-test follows turn-test: it drives turn-test's canned SSE helpers.
     (:file "engine/tool-call-id-test")
     (:file "engine/working-context-proof")
     (:file "engine/working-context-test")
     ;; activity-test follows turn-test: its receipt proof drives the same
     ;; tool-call seam through turn-test's wire helpers.
     (:file "engine/activity-test")
     (:file "session/snapshot-test")
     (:file "term/input-test")
     (:file "term/terminal-test")
     (:file "term/cells-test")
     (:file "term/image-test")
     (:file "term/program-test")
     (:file "tui/theme-test")
     (:file "tui/editor-test")
     (:file "tui/ui-test")
     (:file "tui/activity-label-spacing-test")
     (:file "tui/fork-tree-test")
     (:file "tui/inline-color-test")
     (:file "tui/frame-clock-test")
     (:file "tui/history-test")
     ;; Raw worker isolation is proved in disposable debugger-disabled processes.
     (:file "tui/worker-test")
     (:file "tui/slash-test")
     ;; own-models-test reuses slash-test's listing fixtures.
     (:file "tui/own-models-test")
     (:file "tui/profile-delete-test")
     (:file "tui/composer-test")
     (:file "tui/setup-test")
     (:file "tui/setup-sign-in-test")
     (:file "tui/indentation-test")
     (:file "tui/launch-test")
     (:file "surface/account-login-test")
     (:file "surface/connection-test")
     (:file "tui/scroll-test")
     (:file "tui/chrome-test")
     (:file "tui/home-sessions-test")
     (:file "tui/footer-pending-test")
     ;; toast-test drives ui-test's frame-model fixture and mouse helper.
     (:file "tui/toast-test")
     ;; canvas-test too, and theme-test's THEME-PALETTE.
     (:file "tui/canvas-test")
     (:file "tui/timestamps-test")
     (:file "surface/gateway-test")
     (:file "surface/gateway-return-test")
     (:file "surface/think-off-test")
     ;; Uses gateway-test's detached shell and the real organism.loaded fold.
     (:file "tui/mutation-collapse-test")
     (:file "tui/eval-error-test")
     (:file "tui/tool-hover-test")
     (:file "tui/collapse-scroll-test")
     (:file "tui/disclosure-anchor-test")
     ;; steer-test and checkpoint-test follow gateway-test: both drive the WS
     ;; fixtures (with-temp-gateway, ws-collect-frames).
     (:file "surface/steer-test")
     ;; sessions-live-test drives the same WS fixtures and the client picker;
     ;; activity-test the same frames and the /sessions row's live column.
     (:file "surface/sessions-live-test")
     (:file "surface/activity-test")
     (:file "surface/notify-test")
     (:file "surface/rows-test")
     ;; rpc-test drives the same WS fixtures: a program's lines through
     ;; `nodecode rpc' to a gateway and its frames back.
     (:file "surface/rpc-test")
     (:file "surface/observe-test")
     (:file "surface/usage-projection-test")
     ;; sessions-nodes-test drives the same picker rows: a session's own
     ;; nodes under it, the edge each hangs by, and the store's detached
     ;; create that a team seat is minted with.
     (:file "surface/sessions-nodes-test")
     ;; sessions-files-test drives the sessions routes over the store
     ;; fixtures store/sessions-test seeds.
     (:file "surface/sessions-files-test")
     (:file "surface/health-test")
     (:file "surface/status-test")
     (:file "surface/update-test")
     (:file "surface/control-test")
     ;; control-cells-test follows cells-test: it writes its scratch
     ;; release with that file's folder and hub-index writers.
     (:file "surface/control-cells-test")
     ;; provider-offers-test follows control-cells-test: it clones with
     ;; cells-test's hub fixtures and opens ui-test's frame-model wizard.
     (:file "surface/provider-offers-test")
     (:file "surface/settings-test")
     ;; Quick TUI settings gates reuse the canonical route's isolated fixture.
     (:file "tui/settings-test")
     (:file "tui/generations-test")
     (:file "tui/cells-test")
     (:file "surface/backup-test")
     (:file "session/checkpoint-test")
     ;; exec-test drives the same WS fixtures plus the store fixtures, so it
     ;; loads late.
     (:file "engine/exec-test")
     ;; background-steer-test follows exec-test: it drives the drained
     ;; registry beside it, turn-test's wire helpers and the provider stub.
     (:file "engine/background-steer-test")
     ;; band-test follows exec-test: the surface gates reuse its golden
     ;; frames and octet helpers plus ui-test's frame-model fixture.
     (:file "tui/band-test")
     (:file "tui/pending-eval-test")
     (:file "tui/live-block-test")
     (:file "tui/status-tail-scroll-test")
     (:file "tui/scroll-hold-test")
     ;; frame-guard-test follows band-test: the property gate seeds every
     ;; content lane through ui-test's frame-model fixture and support's
     ;; fold, with the store fixtures the lanes fold against.
     (:file "tui/frame-guard-test")
     ;; withdraw-prompt-test follows steer-test and chrome-test: it drives the
     ;; same queue fixtures (admission, the WS client, the key helpers) for the
     ;; Up peel.
     (:file "organism/withdraw-prompt-test")
     ;; The import's gates load last: their scratch box stands on the
     ;; store, cell and config fixtures above.
     (:file "import/support")
     (:file "import/yaml-test")
     (:file "import/toml-test")
     (:file "import/shape-test")
     (:file "import/world-test")
     (:file "import/home-test")
     (:file "import/import-test")
     (:file "import/source-test")
     (:file "engine/model-id-test"))))
  ;; Resolved at run time: the package does not exist when this form is read.
  ;; RUN-ALL is defined by support, the first file above.
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-all)))
