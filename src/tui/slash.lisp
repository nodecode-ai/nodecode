;;;; slash.lisp --- the slash-command catalog and parser. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The composer's command surface, distilled from the Zig catalog
;;;; (src/tui/model/slash_commands.zig): one table declaring command identity
;;;; -- name, aliases, usage, description -- read by three consumers that must
;;;; agree: the dropdown (prefix filtering), the Enter dispatch (parsing), and
;;;; /help (the rendered catalog). Pure material only. Execution lives behind
;;;; the EXECUTE-SLASH-COMMAND generic, whose methods the shells own, because
;;;; the same command reaches its backend differently per transport: the local
;;;; organism mutates process state directly, the gateway client goes through
;;;; the HTTP control ops.

(in-package #:nodecode.evolved)

;;; --- catalog ---------------------------------------------------------------

(defstruct (slash-command (:constructor make-slash-command
                              (name description &key aliases
                               (help-detail "") owner
                               (dispatch :plain)
                               (op (and dispatch
                                        (intern (format nil "SLASH-~:@(~a~)-OP" name)
                                                :nodecode.evolved)))
                               (headless (and (eq dispatch :local) op))
                               keybind (session t) trigger)))
  "One catalog entry."
  ;; NAME carries no leading slash, and is the whole of what anyone types: a
  ;; command is its name, and what it once took after it is a row of the
  ;; dialog it answers typed alone (tui/commands/bare.lisp). ALIASES are
  ;; alternate names (also slash-less). HELP-DETAIL is what the help dialog
  ;; says of it, in words: what typed alone it offers, never a line to spell.
  ;; OWNER is the key of the cell that registered the command (gateway.lisp
  ;; REGISTER-COMMAND), NIL for a builtin and for a shell's copy of the
  ;; gateway's commands. The execution slots name functions defined later, so they are symbols: DISPATCH
  ;; is how the shell calls OP -- :PLAIN (OP MODEL), :ARGS (OP MODEL ARGS), :NAME
  ;; (OP MODEL NAME), :GUARDED (OP MODEL) behind the Zig usage_help gate, :LOCAL
  ;; (OP ARGS SESSION-ID) for a model-free command that answers alike with or
  ;; without a shell, NIL for one the shell arm spells out; OP defaults to
  ;; SLASH-<NAME>-OP; HEADLESS answers (ARGS SESSION-ID) without a shell (the op
  ;; for :LOCAL, NIL when a shell is needed).
  ;;
  ;; SESSION is NIL for a command that acts on no session -- /help, /cells,
  ;; /link -- so a client with no session open (the web page's opener) runs
  ;; it as it is instead of opening a session for it first.
  ;;
  ;; KEYBIND is the keymap action whose chord the command also answers to: a
  ;; hotkey is a fact about the command, so it is filed with the catalog, not
  ;; in the keymap, which only knows the actions. It is read through the
  ;; model's resolved keymap at paint time (COMPOSER-SUGGESTION-HOTKEY), so an
  ;; override in keybinds.json shows the chord the operator actually has, and
  ;; a command whose action was unbound shows none. A cell's command has
  ;; no hotkey.
  ;;
  ;; TRIGGER is a cell command's opener in the composer: the character,
  ;; typed at a word's start, opens the command's dialog and stays in the
  ;; draft until a pick replaces it (COMPOSER-TRIGGER-COMMAND). NIL for most.
  (name nil :type string) (description nil :type string)
  (aliases nil :type list) (help-detail nil :type string)
  owner dispatch op headless keybind session trigger)

(nlk:access (entry slash-command))

;;; The Zig table carries 20 commands; entries land here as their backends
;;; exist in the organism.
(defparameter *slash-commands*
  (list (make-slash-command "help" "Show commands and key bindings"
                            :op 'open-help-dialog :headless 'slash-help-text :keybind :help
                            :session nil)
        (make-slash-command "clear" "Start a fresh session"
                            :aliases '("new") :headless 'headless-clear)
        (make-slash-command "sessions" "Pick a session and switch to it" :keybind :sessions-dialog
                            :aliases '("agents"))
        (make-slash-command "rename" "Name this session, or show its name"
                            :dispatch :args :headless 'headless-rename
                            :help-detail
                            "The name /sessions and the web sidebar show: one line, at most 64 characters. It shows the name, asks for a new one, or gives back the name the first prompt makes. F2 on a row in /sessions renames that session")
        (make-slash-command "export" "Save this session as Markdown"
                            :dispatch :args
                            :help-detail
                            "The prompts, the answers, the plans and one line per call, as the transcript shows them, in a file named after the session: in this shell's folder, or in a folder or at a file it asks for. Reasoning and a call's output stay out; the web page's Markdown button saves the same file")
        (make-slash-command "execs" "Watch an exec's live output or how it ended, or interrupt it"
                            :help-detail
                            "The band shows a pending exec in a five-line window; /execs opens the exec's retained output at panel height, live while it runs. Ctrl+X there interrupts it (the soft abort), Delete the same; Esc closes. The last execs that ended here stay listed with how each ended and its last lines. With more than one exec, a picker opens first, running ones on top, and Enter opens one")
        (make-slash-command "tree" "Browse the session tree: continue from a turn, or rewind"
                            :aliases '("checkpoints"))
        (make-slash-command "undo" "Rewind the last turn"
                            :op 'slash-navigate-op :dispatch :name :headless 'headless-undo)
        (make-slash-command "redo" "Return to the branch the last rewind left"
                            :op 'slash-navigate-op :dispatch :name :headless 'headless-redo)
        (make-slash-command "fork" "Fork a new session at the current head")
        (make-slash-command "history" "Pick a past prompt into the composer"
                            :op 'open-prompt-history-dialog :keybind :history-dialog)
        (make-slash-command "themes" "List themes or switch the active theme"
                            :dispatch :args)
        (make-slash-command "settings" "Browse and change settings"
                            :dispatch :guarded :session nil
                            :help-detail
                            "Every setting on one sheet in the middle of the window: the sections beside the open one, or one at a time on a narrow window, each setting's value on its row. Right steps a value on where it stands, as Space and Enter step a switch or a choice, and Shift with an arrow steps it either way; Left goes back to the sections from any setting; Enter types a number, text, ids, or a secret, which is never shown, only whether it is set; Delete gives a setting its default back. Typing searches every setting; Esc goes back, then closes. A saved change says when it applies.")
        (make-slash-command "models" "Show or change the main, auxiliary and decider models"
                            :dispatch nil :headless 'headless-models
                            :help-detail
                            "Opens on providers, with Frequently used and Local shortcuts, under four tabs; Tab moves between them. Main is this session's model and the one new sessions start on; after choosing one, its reasoning effort view opens too. Local lists the models served from your own machines: localhost, your LAN or your tailnet. Auxiliary is the model session names, recaps and summaries run on; Same as main gives that work back to each session's own model. Decider is the model that answers judgments as probabilities (DJev); None picks none. Backup is the failover chain: the models a failing turn moves on to, in order, Add a backup, Test backups, and the triggers, which Enter switches; Enter on a backup takes it out. Pick a connected provider to browse its models, or connect a new one here; Ctrl+E replaces its key, Ctrl+R fetches every provider's list again. Esc from models returns to providers. In a room or on the web page, Main, Auxiliary and Decider as tabs: the frequently used models, each provider a row whose picks are its models, and a field for any other")
        (make-slash-command "evict"
                            "Drop the oldest turns from the provider context"
                            :aliases '("compact")
                            :dispatch nil :headless 'headless-evict
                            :help-detail
                            "Force a context eviction on the active Gateway session: half, a quarter, three quarters or all of its retained history, or every retained turn through one picked from /context's history list")
        (make-slash-command "context"
                            "Show this session's context window and usage"
                            :dispatch nil :headless 'headless-context :help-detail
                            "Occupancy (the newest provider round's prompt) over the effective window — configured, learned, or the model's own default — split into its parts: the standing prompt, the tools, the skills and memories index, the model's notes and the history. A shell paints it as a grid; a part opens to what it holds, history to its turns, where Enter evicts through one; time charts each round's prompt from the log")
        (make-slash-command "status"
                            "Nodecode right now: attention, runtime, services and today's usage"
                            :dispatch :guarded :headless 'headless-status
                            :session nil
                            :help-detail
                            "A read-only overview of the serving organism: warnings first, version/profile/uptime, default model, cell-owned standing notices, active turns and today's API-equivalent usage across this store. Enter drills in or refreshes snapshots; Esc closes. Activity and notices update from the runtime stream. No provider probes: /doctor runs diagnostics; /usage owns the full breakdown")
        (make-slash-command "usage"
                            "What every session spent: today, this week, 30 days or all"
                            :dispatch :args :headless 'headless-usage
                            :session nil
                            :help-detail
                            "Spend at catalog prices, model calls, the cache share and what it saved, first-token waits and failed turns across every session, then spend by day, the models, the costliest sessions and the folders. On a shell, a dialog: Tab moves between today, 7 days, 30 days and all, Enter on a session opens it; on a channel or the web page, the four ranges to pick, each answered as text")
        ;; What the footer's account badge opens (tui/commands/accounts.lisp SLASH-ACCOUNT-OP). No
        ;; headless arm: a room or a page would be shown this home's balance.
        (make-slash-command "account"
                            "Your nodecode.ai account: the plan's requests left and the API credit"
                            :session nil
                            :help-detail
                            "The plan's slab and the API credit's side by side, the one paying now lifted and badged, read again from nodecode.ai when it opens; the footer's account badge opens it. Not signed in, it says so: /login signs in")
        ;; Why the footer's model name is green or red (tui/connection.lisp). No headless arm:
        ;; the measurement is this shell's own pick.
        (make-slash-command "privacy"
                            "Who can read a prompt to the chosen model, and how it gets there"
                            :session nil
                            :help-detail
                            "Measured once when a model is chosen: TLS, post-quantum key exchange, interception and Cloudflare in front; then what the provider's terms say about storing, training and ZDR. Private only for NODECODE's own endpoint; a provider not on the list reads unknown. The footer's model name is green when private, red when not")
        ;; The account's sign-in from inside the shell (tui/account-login.lisp): the
        ;; gateway's organism claims, opens the page and waits, as `nodecode login' does. No
        ;; headless arm: a room or a page signing this home in to someone's account would
        ;; hand that account this machine's key.
        (make-slash-command "login"
                            "Sign in to your nodecode.ai account: a subscription or API credits"
                            :dispatch :args :op 'slash-login-op
                            :session nil
                            :help-detail
                            "It asks which one; either way nodecode.ai opens in your browser with this machine's code in the page, and you sign in with your nodecode.ai account and allow the machine. Over ssh, open the page the status line names on any device. The key is saved in this home's auth.json, and the footer shows the account's meter from then on: the plan's requests left, or the API credit; click it for /account")
        (make-slash-command "details"
                            "Set the Developer or Standard viewport"
                            :dispatch :args
                            :help-detail
                            "Ctrl+O switches viewport modes. Developer groups routine calls and previews output. Standard (default) shows each call as tool name plus comment, with input and output behind a click, like DSH normal web view. Both open individual cards on click; there is no open-all mode. It offers the two, the one in use marked")
        ;; The one folder: on a shell a sheet over the organism's
        ;; cells routes (tui/cells-sheet.lisp OPEN-CELLS-DIALOG), headless the
        ;; report's lines over (nlk:cells).
        (make-slash-command "cells"
                            "Every cell: turn one on or off, see why one failed, make a new one"
                            :dispatch :args :op 'slash-cells-op
                            :headless 'headless-cells
                            :session nil
                            :help-detail
                            "A sheet in the middle of the window: Home lists this machine's cells, those that need a look first, then the ones shipped with nodecode, from the hub, your own and the ones off; Hub lists the hub's. The chosen one is laid open beside the list: what it is for, every failure and skipped file whole, the commands it adds, its settings, whether it runs, its folder. Space turns it on or off; a hub cell says where it comes from first, and a second Space installs it. Enter opens its settings; Left and Right, or Tab, switch Home and Hub; typing searches both; Esc closes. Home's first row, + New cell, leaves “Make a cell that ” in the composer: finish the sentence and the agent asks what it still needs, then builds it as a cell of its own, on at once. In a room or on the web page, the same report as rows, a cell each, and + New cell, which says what to tell the agent.")
        ;; The sealed generations (organism/generation.lisp): on a shell the
        ;; history sheet over the gateway's routes (tui/generations.lisp
        ;; SLASH-GENERATIONS-OP), Enter asking in place before the restore;
        ;; headless the list, or the restore to N where the organism runs.
        (make-slash-command "generations"
                            "The history of this organism, and the way back to a change in it"
                            :dispatch :args
                            :op 'slash-generations-op :headless 'headless-generations
                            :session nil
                            :help-detail
                            "A generation is the organism as it stood once a fresh child proved its folder: the binary's version and every loaded folder at its revision, numbered from 1 and never pruned. A sheet of the changes they were, newest first by day -- an agent's keeps, a nodecode update, a cell installed -- each in words; the one you stand on is where going back leaves you, the newer ones it would undo greyed, and a red line marks how far back anything can come. Enter asks in place, then restores it -- every folder back to what it recorded, the layer by one commit forward, a checkout's folders left where they stand -- and the organism exits once no turn is in flight, the next launch running it. Uncommitted edits that hold a restore are kept as a commit or discarded into git stash from the sheet. Without a shell -- a room, eval -- the same changes are one card to page and pick from, the filters its tabs; a pick asks the same question, and its answer -- keep or discard, when edits hold it -- restores")

        ;; One place that answers `what is wrong with this organism right
        ;; now'. /cells says what the folder did; this says what is missing,
        ;; across the model pin, the folder and the store at once, and then
        ;; goes and looks (health.lisp DOCTOR-FACTS, what the web page's
        ;; Check everything shows) — and a cell's own facts, which reach it
        ;; by hooking HEALTH-FACTS. A shell asks its gateway, off its loop:
        ;; reading a large store back takes seconds.
        ;; Snapshots (organism/heap-image.lisp): the organism kept as it
        ;; stands, values and all, beside the folder's generations.
        (make-slash-command "snapshot"
                            "Take a snapshot: this organism frozen whole, values and all"
                            :dispatch :args
                            :op 'slash-snapshot-op :headless 'headless-snapshot
                            :session nil
                            :help-detail
                            "A snapshot is this organism's heap as it stands -- every value a session built up, closures mid-count, definitions that were only ever evaluated -- kept under ~/.nodecode/snapshots/NAME/ on this machine alone: it holds what this machine holds, keys among them, so it is never shared; the folder is what you share. /snapshot waits for no turn to be in flight, stops the organism for a moment, keeps its heap under a dated name and runs on from that snapshot in place: the same process, the shells attached reconnecting as after a restart. /snapshots switches between them")
        (make-slash-command "snapshots"
                            "Your snapshots: switch between them, rename, delete"
                            :dispatch :args
                            :op 'slash-snapshots-op :headless 'headless-snapshots
                            :session nil
                            :help-detail
                            "The snapshots this machine keeps, newest first, and Fresh from files -- the cells as their files say, what a plain launch runs. Enter switches the organism to a row in place, in about half a second; F2 renames a snapshot; Delete removes one after a confirm. A snapshot boots only on the build that took it, and a plain launch still loads the folder as it is now")
        (make-slash-command "doctor"
                            "What this organism is missing, and what to do about it"
                            :dispatch :args :headless 'headless-doctor
                            :session nil
                            :help-detail
                            "The model pin this shell will actually bind, every cell folder that failed and why, what the store holds; then it looks: the store read back whole, the build on disk, the free disk, config.jsonc and every section declared in it, the default provider's key, a release waiting — each with the remedy where there is one")
        ;; This home kept whole in one file (store/backup.lisp), what the
        ;; web page's Back up now makes; made off the loop, a large store
        ;; takes a minute.
        (make-slash-command "backup"
                            "Back this home up whole: sessions, config, keys, memory, skills, cells"
                            :dispatch :args :headless 'headless-backup
                            :session nil
                            :help-detail
                            "One archive under ~/.nodecode/backups/, readable by you alone: a copy of the store taken while it runs, config.jsonc, auth.json and secrets/, memory, skills, SOUL.md and the cell folders. It holds your keys: the archive `nodecode profile export NAME PATH --whole' writes. `nodecode profile import FILE --name NAME' brings one back as a profile beside this home; the web page's Settings pane lists, downloads and restores them")

        ;; Several homes, one binary: the roster and the lifecycle verbs, a
        ;; one-line shell over (nlk:profile-command).
        (make-slash-command "profile"
                            "Profiles: walk your homes and switch to one, or run a verb on them"
                            :aliases '("profiles")
                            :dispatch :args :op 'profile-slash-op
                            :headless 'profile-headless-text
                            :help-detail
                            "A profile is a home of its own — config, secrets, store, layer — under ~/.nodecode/profiles/<name>/, and it reads your memory, skills and credentials where they are rather than copying them. /profile opens the roster: Enter relaunches this shell in the home you picked; Right on a home offers the rest — boot it by default, export it whole or not, a new profile cloned from it, pull its distribution, delete it — and the rows under the homes make another: like this one, empty, a fork, brought in from an archive or installed from a repository. In a room or on the web page, the same homes and rows, a home's row opening what it takes; switching is a shell's alone")

        ;; The organism kept running by the operating system: where the
        ;; gateway stands and the verbs that turn it on and off, a one-line
        ;; shell over (nlk:gateway-text).
        (make-slash-command "gateway"
                            "The gateway: this organism kept running in the background, and at login"
                            :dispatch nil :help-detail
                            "Where it stands — serving, waiting for this shell to close, off — and what it keeps running; turning it on has the operating system run it (systemd on Linux, launchd on macOS) from now on and at every login, turning it off stops it and this shell takes the organism over; a restart puts it on this build, its log says what it did, and Discord, Telegram or Slack is set up step by step: the bot's token, a wait while it joins a server, then its channels and people picked from a checklist")

        ;; This organism in a browser tab: a shell over WEB-TEXT, what
        ;; `nodecode web' answers. The link carries the operator token, so
        ;; there is no headless arm: no room and no page ever offers it.
        (make-slash-command "web"
                            "Open this organism in a browser tab"
                            :dispatch nil :help-detail
                            "What `nodecode web' does: the page's link, opened where there is a display. The operator token rides after the link's #, which a browser never sends to a server: keep the link to yourself. Needs the web cell: turn web on in /cells")


        ;; The Zig table's control-surface commands, copy verbatim from
        ;; src/tui/model/slash_commands.zig:150-157 so the dropdown and the
        ;; help dialog agree with the Zig TUI.
        (make-slash-command "setup" "Set up the organism"
                            :dispatch :guarded :help-detail
                            "Connect a provider, pick a model and its reasoning effort")
        ;; A shell walks the import stage `nodecode import' opens, its plan read
        ;; off the loop over a pinged stream: as a gateway command it held the
        ;; loop, then failed at the 30 s read on a large home (T-350). A room
        ;; or the page gets the summary as text (NIK:RUN-SLASH).
        (make-slash-command "import"
                            "Bring another coding agent's home into this one: preview first, then apply"
                            :dispatch :args :headless 'headless-import :session nil
                            :help-detail
                            "Reads another harness's home -- providers, instructions, memory, skills, MCP servers, channels, cron, sessions, profiles -- and shows what would land before anything does. It asks where to read from first: every agent found here, one of them, a folder or a repository, or the repositories already held, each with a row that takes it back. The preview then lands it as planned, over what this home already has, or one kind of it alone; in a shell it is the setup walk's import stage with its progress, as nodecode import opens it. Skills are linked and read where they are")
        (make-slash-command "think" "Open provider-native reasoning controls"
                            :aliases '("effort") :dispatch :args :headless 'headless-think
                            :help-detail
                            "The model's reasoning rungs: the one picked applies from the next turn, here and in every new session; Default picks the model's own")
        (make-slash-command "gc"
                            "Show collection cost and stall attribution"
                            :dispatch :local :op 'headless-gc
                            :session nil
                            :help-detail
                            "The collection counters and the UI spans a collection stalled, or a fresh window to count them in")
        (make-slash-command "exit" "Exit the organism" :aliases '("quit") :dispatch nil))
  "The slash-command catalog, in /help display order.")

;;; --- the gateway's cell commands -----------------------------------------
;;; A cell's command runs where the cell runs: in the gateway's image,
;;; which a shell reads the commands of (tui/commands/cell-control.lisp GATEWAY-COMMAND-ENTRIES)
;;; and runs one at (RUN-GATEWAY-COMMAND). They parse, suggest, ghost and list
;;; in /help exactly like builtins, and never shadow one: the builtin catalog
;;; is consulted first, and a command named like a builtin or its alias is
;;; dropped as it is read.

(defvar *gateway-commands* '()
  "The cell commands the attached gateway serves, as SLASH-COMMAND entries
this shell runs there.")

(defun find-builtin-slash-command (name)
  "The builtin catalog entry NAME or one of its aliases identifies, else
NIL."
  ;; The one lookup every consumer of the builtin table makes: the shells'
  ;; dispatch, the shadowing rule, and the headless SLASH.
  (when (stringp name)
    (find-if (lambda (entry) (member name (cons entry.name entry.aliases) :test #'string-equal))
             *slash-commands*)))

(defun set-gateway-commands (entries)
  "Replace the shell's copy of the gateway's cell commands with ENTRIES,
plists (:name :description :session :trigger); a name a builtin answers to,
or a blank one, is dropped."
  (setf *gateway-commands*
        (loop for entry in entries
              for name = (getf entry :name)
              when (and (json-value name :text) (not (find-builtin-slash-command name)))
                collect (make-slash-command name (or (getf entry :description) "")
                                            :dispatch nil
                                            :session (getf entry :session t)
                                            :trigger (json-value (getf entry :trigger) :text)))))

(defun find-gateway-command (name)
  "The gateway's cell command NAME identifies, else NIL. Builtins outrank
these."
  (when (stringp name)
    (find name *gateway-commands* :key #'slash-command-name :test #'string-equal)))

(defun slash-command-label (entry)
  "The display form for ENTRY: its /name, the whole of what anyone types."
  (format nil "/~a" (slash-command-name entry)))

;;; --- parsing ---------------------------------------------------------------

(defun slash-input-p (text &aux (trimmed (string-left-trim '(#\Space #\Tab) (or text ""))))
  "Whether TEXT's first non-blank character is the command prefix."
  (and (plusp (length trimmed)) (char= (char trimmed 0) #\/)))

(defun parse-slash-input (text &aux (trimmed (nlk:trimmed (or text ""))))
  "When TEXT is a slash command: (values name args), NAME lowercased without
its slash, ARGS the trimmed verbatim tail (\"\" when absent). NIL otherwise."
  (when (and (plusp (length trimmed)) (char= (char trimmed 0) #\/))
    (let* ((rest (subseq trimmed 1))
           (end (or (position-if #'blank-char-p rest) (length rest))))
      (values (string-downcase (subseq rest 0 end))
              (nlk:trimmed (subseq rest end))))))

(defun find-slash-command (name)
  "The catalog entry NAME or one of its aliases identifies, else NIL."
  ;; Builtins win; the gateway's cell commands are consulted after.
  (or (find-builtin-slash-command name)
      (find-gateway-command name)))

(defun slash-no-args-guard (name args)
  "The Zig executeSlashCommandFromSpec argument gate for usage_help
commands: NIL when ARGS is empty (proceed); the Zig usage line for a help
arg (the Zig isSlashHelpArg rule: `help` case-insensitively, or the literal
flags --help / -h); the Zig rejection line otherwise."
  (when (plusp (length (or args "")))
    (if (or (string-equal args "help") (member args '("--help" "-h") :test #'string=))
        (format nil "Usage: /~a" name)
        (format nil "`/~a` does not take arguments." name))))

(defun slash-name-prefix-p (prefix name &aux (n (length prefix)))
  (and (<= n (length name)) (string-equal prefix name :end2 n)))

(defun slash-suggestions (text &aux (value (or text "")))
  "Catalog entries whose name or an alias starts with TEXT's bare /prefix."
  ;; NIL once the input carries whitespace: a command with arguments underway
  ;; is past choosing its name (the Zig composerSlashCommandPrefix rule).
  ;; The command a whole name names leads, so the name Tab completed stays the
  ;; highlighted row, and its Enter runs that command and not a longer one.
  (when (and (uiop:string-prefix-p "/" value) (notany #'blank-char-p value))
    (let ((prefix (subseq value 1)))
      (stable-sort
       (remove-if-not
        (lambda (entry)
          (some (lambda (name) (slash-name-prefix-p prefix name)) (cons entry.name entry.aliases)))
        (append *slash-commands* *gateway-commands*))
       (flet ((whole (entry) (member prefix (cons entry.name entry.aliases) :test #'string-equal)))
         (lambda (entry other) (and (whole entry) (not (whole other)))))))))

;;; --- execution seam --------------------------------------------------------
;;; /help renders through the help dialog (tui/history.lisp OPEN-HELP-DIALOG): the
;;; catalog plus the resolved keybind listing, searchable. The old notice-lane
;;; text body is gone — one help surface, not two.

;;; --- /gc rendering -----------------------------------------------------------
;;; Pure formatting over the kernel's collection telemetry (gc.lisp): the
;;; instant snapshot plus the attribution window, one readout for both shells.

(defun render-collection-report (stats attribution)
  "The /gc readout."
  ;; STATS is COLLECTION-STATS, ATTRIBUTION is GC-ATTRIBUTION-STATS; times
  ;; arrive in internal-time units and leave human.
  (flet ((ms (units) (/ (* units 1000.0) internal-time-units-per-second))
         (mb (bytes) (/ bytes 1048576.0)))
    (destructuring-bind (&key spans span-worst span-worst-label span-worst-cpu span-time gc-spans
                              gc-worst gc-worst-label gc-time by-label &allow-other-keys)
        attribution
      (destructuring-bind (&key forced-collections last-pause gc-run-time nursery heap-bytes
                                resident-bytes image-bytes last-trim idle-collections
                                image-releases &allow-other-keys)
          stats
        (with-output-to-string (out)
          (format out "ui spans: ~d" spans)
          (when (plusp spans)
            ;; Worst wall next to its CPU: wall >> cpu means the span was
            ;; preempted — system load, not this program's work.
            (format out " · worst ~,1fms~@[ (~(~a~))~] cpu ~,1fms · total ~,1fms"
                    (ms span-worst) span-worst-label (ms span-worst-cpu) (ms span-time)))
          (format out "~%collections inside spans: ~d" gc-spans)
          (when (plusp gc-spans)
            (format out " · worst ~,1fms~@[ (~(~a~))~] · total ~,1fms"
                    (ms gc-worst) gc-worst-label (ms gc-time)))
          (loop for row in by-label
                repeat 4
                do (destructuring-bind (label count wall gc) row
                     (format out "~%  ~(~a~): ~d span~:p · ~,1fms~@[ · gc ~,1fms~]"
                             (or label "unlabeled") count (ms wall)
                             (when (plusp gc) (ms gc)))))
          (format out "~%hidden collections: ~d~@[ · last pause ~,1fms~]"
                  forced-collections (and last-pause (* last-pause 1000.0)))
          (format out "~%gc time: ~,2fs · nursery ~,1fMB · live heap ~,1fMB"
                  (/ gc-run-time (float internal-time-units-per-second))
                  (mb nursery) (mb heap-bytes))
          ;; The process as the machine counts it, and the two idle-time
          ;; returns: collections of an overgrown generation, and the image's
          ;; own pages given back after a stretch of work.
          (format out "~%resident ~:[n/a~;~:*~,1fMB~]~@[ (image ~,1fMB)~] · idle collections ~d · image releases ~d~@[ · last trim ~a~]"
                  (and resident-bytes (mb resident-bytes)) (and image-bytes (mb image-bytes))
                  idle-collections image-releases
                  (and last-trim (getf last-trim :before) (getf last-trim :after)
                       (format nil "~,1f→~,1fMB" (mb (getf last-trim :before))
                               (mb (getf last-trim :after))))))))))

;;; --- /cells rendering --------------------------------------------------
;;; Pure formatting over the kernel's cell records. The shells
;;; call (nlk:cells) and hand the list here.

(defun render-cells (cells)
  "The /cells listing: DESCRIBE-CELLS' lines -- start state included --
then every standing notice this process's parts posted (NLE:NOTICE with a
key: a peripheral's refusal, a channel's state), or the empty-folder note."
  (if (null cells)
      (if nlk::*cells-directory*
          (format nil "no cells: ~a holds no folder with an .asd"
                  (namestring nlk::*cells-directory*))
          "no cells: the folder is disabled in this process")
      (string-right-trim '(#\Newline)
                         (with-output-to-string (out)
                           (nlk:describe-cells cells out)
                           (loop for (nil text) in (nlk:standing-notices)
                                 do (format out "~&~a~%" text))))))

;;; --- /context rendering -----------------------------------------------------
;;; Pure formatting over the usage accumulators the shells fold from the
;;; provider stream (tui/transcript/ingest.lisp INGEST-TURN-USAGE) and the window belief: one
;;; readout for the two shells and the headless surface, beside /gc.

(defun render-context-report (&key label session-id usage session-usage
                                   context-usage context-estimate context-parts
                                   window window-grade window-floor)
  "The /context readout."
  ;; USAGE, SESSION-USAGE and CONTEXT-USAGE are the
  ;; model state's accumulators — the turn's rounds, the session's bill, the
  ;; newest round's prompt.  CONTEXT-ESTIMATE is the estimated prompt of the
  ;; next request, folded from the session's own durable record
  ;; (SESSION-CONTEXT-ESTIMATE); it stands in whenever no provider round has
  ;; reported a prompt — a lane that holds a record is never reported as empty,
  ;; and a `~' marks the number as an estimate.  WINDOW is the effective
  ;; context budget and WINDOW-GRADE its provenance (`configured', `learned',
  ;; `default'), both NIL when no source knows the window; WINDOW-FLOOR is the
  ;; least window durable evidence proves (the largest prompt a round of this
  ;; target ever had processed), named when WINDOW itself is unknown so the
  ;; occupancy still reads against something the provider accepted.
  ;; CONTEXT-PARTS is the estimate's own breakdown (SESSION-CONTEXT-ESTIMATE's
  ;; second value), rendered as one `parts' line: how much of the estimate is
  ;; the standing prompt, how much is the history the floor retains (of the
  ;; record's turns), and how many turns /evict could still drop — so a large
  ;; occupancy that /evict refuses to touch reads as the standing prompt it is.
  (with-output-to-string (out)
    (flet ((meter (usage control &rest args)
             (let ((text (string-left-trim " ·" (or (and usage (usage-meter-text usage)) ""))))
               (when (plusp (length text))
                 (apply #'format out control text args)))))
      (format out "context~@[ · ~a~]~@[ · ~a~]" label session-id)
      (let* ((measured (and context-usage
                            (context-occupancy-tokens context-usage)))
             (estimated-p (and (null measured)
                               (typep context-estimate '(integer 1))))
             (occupied (if estimated-p context-estimate measured))
             (floor (and (typep window-floor '(integer 1))
                         window-floor))
             (mark (if estimated-p "~" ""))
             (source (if estimated-p
                         " · no provider round yet, estimated from the record"
                         "")))
        (cond
          ((null occupied)
           (format out "~&occupancy: no provider round yet~@[ · proven floor ~a~]"
                   (and floor (format-token-count floor))))
          ((and (integerp window) (plusp window))
           (format out "~&occupancy: ~a~a / ~a (~a) · free ~a~@[ (~a)~]~a"
                   mark (format-token-count occupied)
                   (format-token-count window)
                   (format-occupancy-percent occupied window)
                   (format-token-count (max 0 (- window occupied)))
                   window-grade source))
          (t
           ;; One argument for the floor's clause: a skipped ~@[ consumes
           ;; only its own, and the (at most) one left over printed as NIL.
           (format out "~&occupancy: ~a~a · window unknown~@[ · proven floor ~a~]~a"
                   mark (format-token-count occupied)
                   (and floor (format nil "~a~@[ (at most ~a)~]" (format-token-count floor)
                                      (and (<= occupied floor)
                                           (format-occupancy-percent occupied floor))))
                   source))))
      (destructuring-bind (&key standing prompt tools history kept turns evictable
                           &allow-other-keys)
          context-parts
        (when (plusp (or turns 0))
          (format out "~&parts: ~a standing (prompt ~a · tools ~a) · ~a history (newest ~d of ~d turns kept) · evictable: ~:[none~:;~:*~d turns~]"
                  (format-token-count standing) (format-token-count prompt)
                  (format-token-count tools) (format-token-count history) kept turns
                  (and evictable (plusp evictable) evictable))))
      (meter usage "~&turn: ~a~@[ · ~d round~:p~]" (getf usage :rounds))
      (meter session-usage "~&session: ~a~@[ · $~,4f~]"
             (let ((cost (getf session-usage :cost)))
               (and (getf session-usage :cost-known)
                    (numberp cost)
                    cost))))))

