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
                              (name description &key (usage "") aliases
                               (help-detail "") owner
                               (dispatch :plain)
                               (op (and dispatch
                                        (intern (format nil "SLASH-~:@(~a~)-OP" name)
                                                :nodecode.evolved)))
                               (headless (and (eq dispatch :local) op))
                               completer keybind (session t) trigger)))
  "One catalog entry."
  ;; NAME carries no leading slash; ALIASES are alternate
  ;; names (also slash-less); USAGE is the display form when the command takes
  ;; arguments, empty when the bare name is the whole usage. HELP-DETAIL carries
  ;; the Zig help_detail copy for the help dialog, whose label column is
  ;; SLASH-COMMAND-LABEL (the Zig help_label was a second label nothing rendered);
  ;; OWNER is the key of the cell that registered the command (gateway.lisp
  ;; REGISTER-COMMAND), NIL for a builtin and for a shell's copy of the
  ;; gateway's commands. A cell's argument hint is not a slot: it is spent
  ;; building USAGE. The execution slots name functions defined later, so they are symbols: DISPATCH
  ;; is how the shell calls OP -- :PLAIN (OP MODEL), :ARGS (OP MODEL ARGS), :NAME
  ;; (OP MODEL NAME), :GUARDED (OP MODEL) behind the Zig usage_help gate, :LOCAL
  ;; (OP ARGS SESSION-ID) for a model-free command that answers alike with or
  ;; without a shell, NIL for one the shell arm spells out; OP defaults to
  ;; SLASH-<NAME>-OP; HEADLESS answers (ARGS SESSION-ID) without a shell (the op
  ;; for :LOCAL, NIL when a shell is needed); COMPLETER completes the typed tail.
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
  (name nil :type string) (description nil :type string) (usage nil :type string)
  (aliases nil :type list) (help-detail nil :type string)
  owner dispatch op headless completer keybind session trigger)

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
                            :usage "/rename [name | default]"
                            :dispatch :args :headless 'headless-rename
                            :help-detail
                            "The name /sessions and the web sidebar show: one line, at most 64 characters. Bare /rename shows it; /rename default gives back the name the first prompt makes. F2 on a row in /sessions renames that session")
        (make-slash-command "export" "Save this session as Markdown"
                            :usage "/export [path]" :dispatch :args
                            :help-detail
                            "The prompts, the answers, the plans and one line per call, as the transcript shows them, in a file named after the session in this shell's folder, or at PATH (a folder or a file). Reasoning and a call's output stay out; the web page's Markdown button saves the same file")
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
                            :usage "/themes [name]" :dispatch :args)
        (make-slash-command "settings" "Browse and change settings"
                            :dispatch :guarded :session nil
                            :help-detail
                            "Every setting on one sheet in the middle of the window: the sections beside the open one, or one at a time on a narrow window, each setting's value on its row. Right steps a value on where it stands, as Space and Enter step a switch or a choice, and Shift with an arrow steps it either way; Left goes back to the sections from any setting; Enter types a number, text, ids, or a secret, which is never shown, only whether it is set; Delete gives a setting its default back. Typing searches every setting; Esc goes back, then closes. A saved change says when it applies.")
        (make-slash-command "models" "Show or change the main, auxiliary and decider models"
                            :usage "/models [provider] [model] | aux [provider] [model | default] | decider [id | none] | backups | list | refresh"
                            :dispatch nil :headless 'headless-models :completer 'models-argument-choices
                            :help-detail
                            "Bare opens providers with Frequently used and Local model shortcuts under four tabs; Tab moves between them. Main is this session's model and the one new sessions start on; after choosing one, its reasoning effort view opens too. Local lists the models served from your own machines: localhost, your LAN or your tailnet. Auxiliary is the model session names, recaps and summaries run on; Same as main gives that work back to each session's own model. Decider is the model that answers judgments as probabilities (DJev); None picks none. Backup is the failover chain: the models a failing turn moves on to, in order, Add a backup, Test backups, and the triggers, which Enter switches; Enter on a backup takes it out. Pick a connected provider to browse its models, or connect a new one here; Ctrl+E replaces its key. Esc from models returns to providers. Typed, /models aux [provider] model sets the auxiliary model and /models aux default takes it out; /models decider djev pins the decider and /models decider none takes it out; /models backups opens on Backup")
        (make-slash-command "evict"
                            "Drop the oldest turns from the provider context"
                            :aliases '("compact")
                            :usage "/evict [percent | through TURN]" :dispatch nil :headless 'headless-evict
                            :help-detail
                            "Force a context eviction on the active Gateway session: a percent of its retained history (default 50%), or every retained turn through TURN, a turn id (/context history runs it)")
        (make-slash-command "context"
                            "Show this session's context window and usage"
                            :usage "/context [prompt | tools | index | notes | history | time]"
                            :dispatch nil :headless 'headless-context :help-detail
                            "Occupancy (the newest provider round's prompt) over the effective window — configured, learned, or the model's own default — split into its parts: the standing prompt, the tools, the skills and memories index, the model's notes and the history. A shell paints it as a grid; a part opens to what it holds, history to its turns, where Enter evicts through one; time charts each round's prompt from the log")
        (make-slash-command "status"
                            "Nodecode right now: attention, runtime, services and today's usage"
                            :usage "/status" :dispatch :guarded :headless 'headless-status
                            :session nil
                            :help-detail
                            "A read-only overview of the serving organism: warnings first, version/profile/uptime, default model, cell-owned standing notices, active turns and today's API-equivalent usage across this store. Enter drills in or refreshes snapshots; Esc closes. Activity and notices update from the runtime stream. No provider probes: /doctor runs diagnostics; /usage owns the full breakdown")
        (make-slash-command "usage"
                            "What every session spent: today, this week, 30 days or all"
                            :usage "/usage [today | 7d | 30d | all]"
                            :dispatch :args :headless 'headless-usage :completer '("today" "7d" "30d" "all")
                            :session nil
                            :help-detail
                            "Spend at catalog prices, model calls, the cache share and what it saved, first-token waits and failed turns across every session, then spend by day, the models, the costliest sessions and the folders. On a shell, a dialog: Tab moves between today, 7 days, 30 days and all (seven days unless named), Enter on a session opens it; on a channel or the web page, the same as text")
        ;; What the footer's account badge opens (tui/commands/accounts.lisp SLASH-ACCOUNT-OP). No
        ;; headless arm: a room or a page would be shown this home's balance.
        (make-slash-command "account"
                            "Your nodecode.ai account: the plan's requests left and the API credit"
                            :session nil
                            :help-detail
                            "The plan's slab and the API credit's side by side, the one paying now lifted and badged, read again from nodecode.ai when it opens; the footer's account badge opens it. Not signed in, it says so: /login signs in")
        ;; Why the footer's model name is green or red (surface/connection.lisp). No headless arm:
        ;; the measurement is this shell's own pick.
        (make-slash-command "privacy"
                            "Who can read a prompt to the chosen model, and how it gets there"
                            :session nil
                            :help-detail
                            "Measured once when a model is chosen: TLS, post-quantum key exchange, interception and Cloudflare in front; then what the provider's terms say about storing, training and ZDR. Private only for NODECODE's own endpoint; a provider not on the list reads unknown. The footer's model name is green when private, red when not")
        ;; The account's sign-in from inside the shell (surface/account-login.lisp): the
        ;; gateway's organism claims, opens the page and waits, as `nodecode login' does. No
        ;; headless arm: a room or a page signing this home in to someone's account would
        ;; hand that account this machine's key.
        (make-slash-command "login"
                            "Sign in to your nodecode.ai account: a subscription or API credits"
                            :usage "/login [subscription | api]"
                            :dispatch :args :op 'slash-login-op :completer '("subscription" "api")
                            :session nil
                            :help-detail
                            "Bare asks which one; either way nodecode.ai opens in your browser with this machine's code in the page, and you sign in with your nodecode.ai account and allow the machine. Over ssh, open the page the status line names on any device. The key is saved in this home's auth.json, and the footer shows the account's meter from then on: the plan's requests left, or the API credit; click it for /account")
        (make-slash-command "details"
                            "Set the Developer or Standard viewport"
                            :usage "details developer|standard" :dispatch :args
                            :help-detail
                            "Ctrl+O switches viewport modes. Developer groups routine calls and previews output. Standard (default) shows each call as tool name plus comment, with input and output behind a click, like DSH normal web view. Both open individual cards on click; there is no open-all mode. Bare /details reports the viewport")
        ;; The one folder: on a shell a sheet over the organism's
        ;; cells routes (tui/cells-sheet.lisp OPEN-CELLS-DIALOG), headless the
        ;; report's lines over (nlk:cells).
        (make-slash-command "cells"
                            "Every cell: turn one on or off, see why one failed"
                            :usage "/cells" :op 'open-cells-dialog :headless 'headless-cells
                            :session nil
                            :help-detail
                            "A sheet in the middle of the window: Home lists this machine's cells, those that need a look first, then the ones shipped with nodecode, from the hub, your own and the ones off; Hub lists the hub's. The chosen one is laid open beside the list: what it is for, every failure and skipped file whole, the commands it adds, its settings, whether it runs, its folder. Space turns it on or off; a hub cell says where it comes from first, and a second Space installs it. Enter opens its settings; Left and Right, or Tab, switch Home and Hub; typing searches both; Esc closes.")
        ;; The sealed generations (organism/generation.lisp): on a shell the
        ;; history sheet over the gateway's routes (tui/generations.lisp
        ;; SLASH-GENERATIONS-OP), Enter asking in place before the restore;
        ;; headless the list, or the restore to N where the organism runs.
        (make-slash-command "generations"
                            "The history of this organism, and the way back to a change in it"
                            :usage "/generations [N]" :dispatch :args
                            :op 'slash-generations-op :headless 'headless-generations
                            :session nil
                            :help-detail
                            "A generation is the organism as it stood once a fresh child proved its folder: the binary's version and every loaded folder at its revision, numbered from 1 and never pruned. Bare, a sheet of the changes they were, newest first by day -- an agent's keeps, a nodecode update, a cell installed -- each in words; the one you stand on is where going back leaves you, the newer ones it would undo greyed, and a red line marks how far back anything can come. Enter asks in place, then restores it -- every folder back to what it recorded, the layer by one commit forward, a checkout's folders left where they stand -- and the organism exits once no turn is in flight, the next launch running it. Uncommitted edits that hold a restore are kept as a commit or discarded into git stash from the sheet. /generations N asks for N. Without a shell -- a room, eval -- the same changes are one card to page and pick from, the filters its tabs; a pick asks the same question, and /generations N go (keep or discard, when edits hold it) restores")

        ;; One place that answers `what is wrong with this organism right
        ;; now'. /cells says what the folder did; this says what is missing,
        ;; across the model pin, the folder and the store at once, and then
        ;; goes and looks (health.lisp DOCTOR-FACTS, what the web page's
        ;; Check everything shows) — and a cell's own facts, which reach it
        ;; by hooking HEALTH-FACTS. A shell asks its gateway, off its loop:
        ;; reading a large store back takes seconds.
        (make-slash-command "doctor"
                            "What this organism is missing, and what to do about it"
                            :usage "/doctor" :dispatch :args :headless 'headless-doctor
                            :session nil
                            :help-detail
                            "The model pin this shell will actually bind, every cell folder that failed and why, what the store holds; then it looks: the store read back whole, the build on disk, the free disk, config.jsonc and every section declared in it, the default provider's key, a release waiting — each with the remedy where there is one")
        ;; This home kept whole in one file (surface/backup.lisp), what the
        ;; web page's Back up now makes; made off the loop, a large store
        ;; takes a minute.
        (make-slash-command "backup"
                            "Back this home up whole: sessions, config, keys, memory, skills, cells"
                            :usage "/backup" :dispatch :args :headless 'headless-backup
                            :session nil
                            :help-detail
                            "One archive under ~/.nodecode/backups/, readable by you alone: a copy of the store taken while it runs, config.jsonc, auth.json and secrets/, memory, skills, SOUL.md and the cell folders. It holds your keys: the archive `nodecode profile export NAME PATH --whole' writes. `nodecode profile import FILE --name NAME' brings one back as a profile beside this home; the web page's Settings pane lists, downloads and restores them")

        ;; Several homes, one binary: the roster and the lifecycle verbs, a
        ;; one-line shell over (nlk:profile-command).
        (make-slash-command "profile"
                            "Profiles: walk your homes and switch to one, or run a verb on them"
                            :aliases '("profiles")
                            :usage "/profile [switch | list | use | create | delete | export | import | install | update | help]"
                            :dispatch :args :op 'profile-slash-op
                            :headless 'profile-headless-text
                            :help-detail
                            "A profile is a home of its own — config, secrets, store, layer — under ~/.nodecode/profiles/<name>/, and it reads your memory, skills and credentials where they are rather than copying them; bare /profile opens the roster — Enter relaunches this shell in the home you picked, and the `+ New profile' row under them opens a field that makes another — /profile switch NAME does it without the picker, /profile use NAME moves the next bare launch instead, /profile help names every verb's arguments")

        ;; The organism kept running by the operating system: where the
        ;; gateway stands and the verbs that turn it on and off, a one-line
        ;; shell over (nlk:gateway-text).
        (make-slash-command "gateway"
                            "The gateway: this organism kept running in the background, and at login"
                            :usage "/gateway [on | off | restart | logs | help]"
                            :dispatch nil :help-detail
                            "Bare says where it stands — serving, waiting for this shell to close, off — and what it keeps running; `on' has the operating system run it (systemd on Linux, launchd on macOS) from now on and at every login, `off' stops it and this shell takes the organism over")

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
                            "Bring another coding agent's home into this one: preview first, --yes applies"
                            :usage "/import [WORLD] [--source PATH] [--only KINDS] [--dry-run] [--overwrite] [--keep-running] [--yes]"
                            :dispatch :args :headless 'headless-import :session nil
                            :completer (nlk:agent-world-names)
                            :help-detail
                            "Reads another harness's home -- providers, instructions, memory, skills, MCP servers, channels, cron, sessions, profiles -- and shows what would land; --yes applies. In a shell it opens the setup walk's import stage with its progress, as nodecode import does; /import --sources lists the repositories held, /import --remove LABEL takes one back")
        (make-slash-command "think" "Open provider-native reasoning controls"
                            :aliases '("effort") :dispatch :args :headless 'headless-think
                            :usage "/think [rung]" :completer (cons "default" +effort-ladder+)
                            :help-detail
                            "Bare opens reasoning controls; /think <rung> applies it from the next turn, here and in every new session; /think default picks the model's own")
        (make-slash-command "gc"
                            "Show collection cost and stall attribution"
                            :usage "/gc [reset]" :dispatch :local :op 'headless-gc
                            :session nil
                            :help-detail
                            "Show collection counters and the UI spans a collection stalled; /gc reset restarts the window")
        (make-slash-command "exit" "Exit the organism" :aliases '("quit") :dispatch nil))
  "The slash-command catalog, in /help display order.")

(defparameter *evict-usage-hint*
  "Usage: /evict [percent | through TURN] — 1-100, default 50"
  "The feedback line for a malformed or out-of-range /evict argument: the
Zig evict_usage_hint (src/tui/model/slash_commands.zig) and the turn form
/context's history list runs.")

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
plists (:name :description :usage :session :trigger); a name a builtin
answers to, or a blank one, is dropped."
  (setf *gateway-commands*
        (loop for entry in entries
              for name = (getf entry :name)
              when (and (json-value name :text) (not (find-builtin-slash-command name)))
                collect (make-slash-command name (or (getf entry :description) "")
                                            :usage (or (getf entry :usage) "")
                                            :dispatch nil
                                            :session (getf entry :session t)
                                            :trigger (json-value (getf entry :trigger) :text)))))

(defun find-gateway-command (name)
  "The gateway's cell command NAME identifies, else NIL. Builtins outrank
these."
  (when (stringp name)
    (find name *gateway-commands* :key #'slash-command-name :test #'string-equal)))

(defun slash-command-label (entry)
  "The display form for ENTRY: its usage when it takes arguments, else the
bare /name."
  (if (plusp (length entry.usage)) entry.usage (format nil "/~a" (slash-command-name entry))))

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
  (when (and (uiop:string-prefix-p "/" value) (notany #'blank-char-p value))
    (let ((prefix (subseq value 1)))
      (remove-if-not
       (lambda (entry)
         (some (lambda (name) (slash-name-prefix-p prefix name)) (cons entry.name entry.aliases)))
       (append *slash-commands* *gateway-commands*)))))

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

