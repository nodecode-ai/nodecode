;;;; entry.lisp --- warnings, stdio severance, the entry point, the command line. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- operator warnings as system notices --------------------------------
;;; WARN (and any other write to *ERROR-OUTPUT*) paints the tty. Inside the
;;; alt-screen that lands as raw text in the viewport. Capture those lines
;;; and put them up as toasts, never as inline viewport junk; an answer
;;; posted from another thread (/gateway, /web) takes the same road to the
;;; composer status.

;;; Cross-thread: SETF the global, never LET-bind, so channel workers and the
;;; TUI thread see the same variable.
(defvar *active-tui-program* nil
  "The live program, if any.")

(defvar *system-notice-hook* nil
  "Optional (lambda (text error-p)) for tests; also invoked when a program
is live so the same path is observable without draining the program's mailbox.")

(nlt:defmessage system-notice-msg
    ((text :initarg :text :reader system-notice-msg-text)
     (error-p :initarg :error-p :initform nil :reader system-notice-msg-error-p)
     (stays :initarg :stays :initform nil :reader system-notice-msg-stays)
     (answer :initarg :answer :initform nil :reader system-notice-msg-answer))
  :documentation
  "What this shell says from another thread (LAND-SYSTEM-NOTICE): a toast,
or with ANSWER the composer status — never raw text painted onto the
alt-screen.")

(nlt::define-message-vocabulary system-notice-msg)

(defun strip-warning-prefix (text)
  "Drop the CL WARN printer's 'WARNING:' / 'Style-Warning:' lead-in so the
toast's own chrome, or the composer status's tone, owns the framing."
  ;; The lead-in and the blanks around it go; any other line loses only its
  ;; leading blanks.
  (values (ppcre:regex-replace "(?i)^[ \\t]*(?:(?:style-)?warning:|compiler-note:)?[ \\t]*"
                               (or text "") "")))

;;; A diagnostic raised by that paint folds into the toasts and stops there: a
;;; report must be total, and repainting to announce a failed paint is how a
;;; shell spins instead of showing one.
(defvar *landing-system-notice* nil
  "True while the loop thread is landing a notice in place.")

(defun land-system-notice (model text &key error-p stays answer)
  "Land one thing this shell says on MODEL: TEXT as a toast (TOAST), or with
ANSWER in the composer status (SHOW-STATUS). => whether the frame moved."
  ;; The one writer on this path — the reducer below and a report landed on
  ;; the loop thread itself both come here — and the answer is what keeps a
  ;; repeated answer free: the same text arriving twice is the same fact, and
  ;; a fact that changes nothing must not cost a frame, least of all when the
  ;; frame is what produced it. A repeated toast counts up instead.
  (if answer
      (let ((before (chat-model-composer-status model)))
        (when (and (json-value text :text)
                   (not (and before (equal text (composer-status-text before)))))
          (show-status model text :error-p error-p)
          t))
      (toast model text :error-p error-p :stays stays)))

(defun render-live-chrome (model)
  "Recompose the frame for a change that touched only the live chrome."
  ;; The composer status, the turn bar and the band are the only rows such a
  ;; change can move, so the cached transcript projection stands unless a reducer already
  ;; dirtied it — re-projecting the whole transcript to show one warning is
  ;; both work the frame does not owe and a second chance for the projection
  ;; to fail on the one path that has to survive a failing projection.
  (when (chat-model-band model)
    (if (chat-model-transcript-dirty model)
        (render-state model)
        (render-status-frame model))))

;;; A notice raised on the loop thread itself lands in place: put up as a
;;; toast, the chrome recomposed, and painted — exactly what the
;;; reducer and the loop's own paint do for a posted message, minus the trip
;;; through the mailbox. The loop thread never posts this message to itself.
;;; A diagnostic raised while a batch is being handled — a failure inside
;;; the renderer above all — would otherwise become the next batch: the
;;; report re-enters the fold, the fold fails, the failure reports, and the
;;; mailbox never empties, so the drain's wait (the loop's only rate limit)
;;; never arms and the shell turns the cycle as fast as it reproduces, at a
;;; full core, painting nothing. So the fold there is unconditional and
;;; cannot fail, and the paint is attempted once, dropped on failure, and
;;; never reported: the shell keeps its last good frame and its input, and
;;; the notice appears on the first frame that paints.

(defun surface-system-notice (text &key (error-p t) stays answer)
  "Say TEXT on the live TUI: a toast — an error, or one that STAYS, until it
is closed — or with ANSWER the composer status."
  ;; Never writes to the terminal: a live program gets the notice through its
  ;; own loop — landed in place when this is that loop's thread, posted
  ;; through NLT:SEND (thread-safe) from any other — and with no program the
  ;; text is dropped rather than painted over the alt-screen.
  (let ((text (string-right-trim
               '(#\Space #\Tab #\Return #\Newline)
               (strip-warning-prefix text))))
    (when (plusp (length text))
      (when *system-notice-hook*
        (funcall *system-notice-hook* text error-p))
      (when-let (program *active-tui-program*)
        (if (nlt:on-loop-thread-p program)
            (let ((model program.model))
              (when (and (typep model 'chat-model)
                         (land-system-notice model text :error-p error-p :stays stays
                                                        :answer answer)
                         (not *landing-system-notice*))
                (let ((*landing-system-notice* t))
                  (ignore-errors
                   (render-live-chrome model)
                   (nlt:render program.renderer (nlt:view model))))))
            (nlt:send program (make-system-notice-msg :text text :error-p error-p
                                                      :stays stays :answer answer))))
      text)))

(defclass tui-error-stream (line-output-stream)
  ((lock :initform (bt2:make-lock :name "tui-error-stream")
         :reader tui-error-stream-lock)
   (fallback :initarg :fallback :initform nil
             :accessor tui-error-stream-fallback))
  ;; The OS-level stderr is severed to the debug log while a TUI is live.
  ;; Explicit notices have their own typed path; raw stderr never uses it.
  (:documentation
   "The TUI-era *ERROR-OUTPUT*: characters buffer per line and retain their
bytes in FALLBACK, the debug log, never user notifications."))

(defmethod line-output ((stream tui-error-stream) text)
  ;; Stderr is not a notice protocol. Compiler reports, continuation lines,
  ;; and caught runtime failures retain their bytes in the debug-only sink.
  (let ((fallback (or (tui-error-stream-fallback stream) sb-sys:*stderr*)))
    (write-string text fallback)
    (ignore-errors (force-output fallback))))

;;; Channel workers and the TUI thread write the one stream, so its buffer
;;; moves under the lock; a carriage return ends a line as a newline does.
(defmethod sb-gray:stream-write-char :around ((stream tui-error-stream) char)
  (bt2:with-lock-held ((tui-error-stream-lock stream))
    (prog1 (call-next-method)
      (when (char= char #\Return)
        (setf (line-output-stream-at-line-start stream) t)
        (flush-line-output stream)))))

(defmethod sb-gray:stream-force-output :around ((stream tui-error-stream))
  (bt2:with-lock-held ((tui-error-stream-lock stream))
    (call-next-method)))

(defvar *crash-report-hook* nil
  "The debugger hook CALL-WITH-TUI-ERROR-CAPTURE installed, so a nested
capture wraps the disabled debugger once, never itself.")

(defun call-with-tui-error-capture (program thunk)
  "While THUNK runs, keep process-wide *ERROR-OUTPUT* in the debug-only
fallback stream and route fatal debugger reports there too. PROGRAM owns
explicit notifications, never compiler or raw runtime output."
  ;; Restores both even if THUNK unwinds. Uses SYMBOL-GLOBAL-VALUE so channel
  ;; worker threads (which do not rebind *ERROR-OUTPUT*) follow the redirect.
  (let* ((saved-program *active-tui-program*)
         (saved-error (sb-ext:symbol-global-value '*error-output*))
         (fallback (if (typep saved-error 'tui-error-stream)
                       (or (tui-error-stream-fallback saved-error)
                           sb-sys:*stderr*)
                       saved-error))
         (saved-hook sb-ext:*invoke-debugger-hook*)
         (wrapped (eq saved-hook *crash-report-hook*)))
    ;; Replacing an installed TUI-ERROR-STREAM keeps its fallback: no nesting.
    (setf *active-tui-program* program
          (sb-ext:symbol-global-value '*error-output*)
          (make-instance 'tui-error-stream :fallback fallback))
    (unless wrapped
      ;; Report on FALLBACK, not a toast a quitting process never paints.
      (setf *crash-report-hook*
            (lambda (condition hook &aux (*error-output* fallback))
              (cond
                (saved-hook (funcall saved-hook condition hook))
                (t
                 (ignore-errors
                   (format fallback "~&Unhandled ~s in thread ~s: ~a~%"
                           (type-of condition) sb-thread:*current-thread* condition)
                   (sb-debug:print-backtrace :stream fallback :from :interrupted-frame
                                             :print-thread t :emergency-best-effort t)
                   (finish-output fallback))
                 (sb-ext:exit :code 1 :abort t))))
            sb-ext:*invoke-debugger-hook* *crash-report-hook*))
    (with-cleanup ((ignore-errors
                     (let ((current (sb-ext:symbol-global-value '*error-output*)))
                       (when (typep current 'tui-error-stream)
                         (finish-output current))))
                   (unless wrapped
                     (setf sb-ext:*invoke-debugger-hook* saved-hook
                           *crash-report-hook* nil))
                   (setf *active-tui-program* saved-program
                         (sb-ext:symbol-global-value '*error-output*) saved-error))
      (funcall thunk))))

;;; --- OS-level stdio severance ---------------------------------------------
;;; TUI-ERROR-CAPTURE above owns the Lisp-stream level: *ERROR-OUTPUT* writes
;;; stay in the debug log. The OS level underneath it — the descriptors 0, 1
;;; and 2 severed from the terminal while a TUI paints — is the kernel's
;;; CALL-WITH-SEVERED-STDIO; this shell only names the sidecar log.

(defun stdio-log-path (store-path)
  "The stdio sink, named next to the database whose TUI owns the terminal —
the same adjacency rule as RESURRECTION-LOG-PATH."
  (pathname (concatenate 'string (namestring store-path) ".stdio.log")))

(defmethod update ((model chat-model) (msg system-notice-msg))
  "Land one thing said from another thread — a toast, or an answer — not as
raw text painted onto the viewport, not as a transcript row, and not as a
full re-projection: neither can move a transcript row, so an answer
recomposes over the cached projection, and a toast is the frame's overlay."
  ;; A repeated answer is the same fact and costs no frame.
  (let* ((answer (system-notice-msg-answer msg))
         (moved (land-system-notice model (system-notice-msg-text msg)
                                    :error-p (system-notice-msg-error-p msg)
                                    :stays (system-notice-msg-stays msg)
                                    :answer answer)))
    (when (and moved answer)
      (render-live-chrome model))
    (values model nil (not moved))))

;;; --- entry point --------------------------------------------------------

(defun fresh-session-id ()
  "Return a new unique session identifier."
  ;; A session with no prior event log rows has no history: recall returns
  ;; nothing and fold-log finds nothing. The format matches the session
  ;; struct's own auto-generated id.
  (format nil "s-~36r" (random (expt 36 8) (make-random-state t))))

(defun call-with-profile-switch (program body &aux (performer nlk:*profile-switch-performer*))
  "Run BODY — a whole shell, its teardown included — with the switch
performer installed, then perform whatever switch BODY parked."
  ;; PROGRAM is the loop BODY runs, so a pick can end it. Both shells go
  ;; through here, and the order below is the contract:
  ;;
  ;;   1. the program loop returns, and its own UNWIND-PROTECT has restored
  ;;      every terminal mode, left raw mode and given the window back;
  ;;   2. the shell's teardown has stopped what it started and closed the store;
  ;;   3. the store is closed again here, idempotently, because an exec keeps
  ;;      open file descriptors and a lease still held would be inherited into
  ;;      the new image and deadlock it against itself;
  ;;   4. only then is this process replaced.
  ;;
  ;; Nothing execs from inside the loop or from a picker callback: a switch
  ;; reaches this function as a plan and waits for the terminal. The performer
  ;; that actually execs is the one the kernel installed (launch.lisp), saved
  ;; and called here, so there is one exec call shape and both shells use it.
  ;; A refused exec prints the kernel's words and the shell exits normally,
  ;; rather than leaving a half-torn-down process behind.
  ;; A shell starts with nothing parked, and the plan is consumed the
  ;; moment it is performed: a refusal leaves this image clean rather than
  ;; half-switched.
  (setf nlk:*profile-switch-parked* nil)
  (prog1 (with-cleanup ((setf nlk:*profile-switch-performer* performer))
           (setf nlk:*profile-switch-performer*
                 (lambda (name binary argv)
                   (setf nlk:*profile-switch-parked* (list name binary argv))
                   ;; The operator pressed Enter expecting to be in that
                   ;; home now, and the terminal does not come back at all:
                   ;; the pick sends the same quit /exit sends, from wherever
                   ;; it was made, and RUN leaves the screen — the notice
                   ;; rides out on the frame it already painted — for the
                   ;; image that replaces this one.
                   (setf nlt:*keep-terminal* t)
                   (nlt:send program (nlt:make-quit-msg))
                   (format nil "switching to ~a: this shell comes back in that home" name)))
           (funcall body))
    (let ((pending nlk:*profile-switch-parked*))
      (when (and pending performer)
        ;; Consumed: one plan, one exec. A refused exec leaves an image with
        ;; nothing parked rather than half-switched.
        (setf nlk:*profile-switch-parked* nil)
        (ignore-errors (nlk:close-store))
        ;; The screen stays (the successor paints over it), the input modes do
        ;; not: raw mode is the one mode that must not outlive the process
        ;; that set it — the successor saves whatever it finds as its own
        ;; original and would hand raw settings back to the shell at its exit.
        (ignore-errors (nlt:exit-raw-mode))
        (when-let (refusal (apply performer pending))
          (setf nlt:*keep-terminal* nil)
          (ignore-errors (nlt:release-terminal program))
          (format *error-output* "~&nodecode: ~a~%" refusal)
          (uiop:finish-outputs))))))

;;;; --- the startup owner ---------------------------------------------------
;;;;
;;;; One function owns the boot sequence. Nothing in it needs to fork any
;;;; more (a child execs a fresh process), so the order is the
;;;; plain one: collection policy, the cell folder, then the entry the
;;;; arguments name.

;;; --- executable entry ----------------------------------------------------

(define-refusal launch-usage-error launch-usage-error "~a"
  "A command line the executable refuses; MAIN prints it
with a pointer to `nodecode --help' and exits 2.")

(defun ephemeral-store-path (&optional (pid (sb-posix:getpid)))
  "A store of this process's own under the platform temp directory
(TMPDIR, TEMP, TMP, else /tmp): nodecode-ephemeral-<pid>.db, the name the
Zig launcher gives the same thing."
  (merge-pathnames (format nil "nodecode-ephemeral-~d.db" pid)
                   (uiop:temporary-directory)))

;;; --- the command line: one table, every reader ------------------------------
;;; The launch's words are data (NLK:VERB, waist/verbs.lisp): the parser
;;; resolves a word and its aliases through the two tables below, `nodecode
;;; --help' prints them, and a word they do not hold is refused with the
;;; nearest one they do. `nodecode profiles --help' once answered `unknown
;;; argument' and a hand-kept usage line: the parser had no table to read
;;; back. An internal verb is the organism's own child entry, a trial or the
;;; warm-image supervisor and its builder, spawned and never typed: the
;;; parser takes it, help never lists it, no suggestion offers it.

(defparameter *launch-verbs*
  (list
   (nlk:make-verb "serve" "run this home's organism headless: a gateway and no shell"
                  :arguments "[--store PATH]"
                  :detail "It owns the store and serves its gateway until stopped, or exits at once when another process already owns it. A reconnecting shell spawns one; it is also the way to run an organism with no terminal.")
   (nlk:make-verb "login" "sign in to your nodecode.ai account: a subscription or API credits"
                  :arguments "[--subscription | --api]"
                  :detail "Asks which one unless a flag says it, opens nodecode.ai in your browser with this machine's code in the page, and waits there for you to sign in with your nodecode.ai account and allow the machine; over ssh, open the printed page on any device. The key is saved locally and the account's meter printed: the plan's requests left and when the plan ends, the API credit. No terminal UI or session is opened. /login does the same from inside the shell; /models is for other providers.")
   (nlk:make-verb "machine" "link this machine to your nodecode.ai account with a code from its Machines page"
                  :label "machine link [CODE]"
                  :arguments "link [CODE]"
                  :detail "Bring the code from https://nodecode.ai/account/machines, or omit CODE to get one to type there. Approve the machine on the account page; its key is saved locally. No terminal UI or session is opened. nodecode login signs in with a browser instead.")
   (nlk:make-verb "setup" "open the setup walk on the first frame"
                  :terminal t
                  :detail "Connect a provider, pick a model and its reasoning effort. /setup does the same from inside the shell.")
   (nlk:make-verb "import" "read another coding agent's home into this one; a preview until --yes"
                  :label "import [WORLD]" :terminal t
                  :arguments "[WORLD] [--source PATH] [--only KINDS] [--dry-run] [--overwrite] [--keep-running] [--yes]"
                  :detail "Providers, standing instructions, memory, skills, MCP servers, channels, cron jobs, sessions and profiles, each through the cell that owns it, keys and tokens with them. WORLD names one harness to read; naming none reads every coding-agent home on this box and merges them, newest first, with an existing fact here always winning. --source reads a home at a path instead, or a repository's skills and MCP servers (OWNER/REPO, an https URL, #REF pinning a branch, tag or commit); --only names the kinds, comma-separated; --overwrite replaces what is already here. A foreign gateway polling the same bots is stopped and disabled so they answer from here (the report names the command that gives them back); --keep-running leaves it running, and its bots land off and come on by themselves once it stops. The preview opens in the setup walk; --yes applies it without asking.")
   (nlk:make-verb "add" "install a cell from the hub, or one this release ships, and turn it on"
                  :label "add nc://NAME"
                  :arguments "nc://NAME [--yes] [--wait SECONDS]"
                  :detail "NAME is the cell's name, its nodecode- prefix optional: nc://channel-slack. Code from the hub is shown first -- its repository, the commit it is pinned at, what it brings -- and installed on a yes; --yes says it on the line, and with no terminal to ask nothing is installed without it. With the organism running it is turned on there, live in every attached shell; with none running it loads at the next launch. --wait is how long add waits, 600 s unless it says: the organism fits the turn-on into it, keeping up to 30 s for loading and answering, and when the rest runs out it stops and says so rather than go on after add has stopped waiting.")
   (nlk:make-verb "profile" "list, create, use, delete, export, import and install profiles"
                  :aliases '("profiles")
                  :label "profile [VERB]"
                  :arguments "[VERB] [ARGUMENTS]")
   (nlk:make-verb "gateway" "keep this home's organism running in the background, and at login"
                  :label "gateway [VERB]"
                  :arguments "[VERB]")
   (nlk:make-verb "update" "move this home's organism to the latest release, or to VERSION"
                  :label "update [VERSION]"
                  :arguments "[VERSION] [--check]"
                  :detail "The release is staged beside the running build and classified, definition by definition. When every changed definition can go live it is evaluated into the running organism and every attached shell, with no restart; otherwise the organism restarts itself onto the new build at the next idle moment. There is no watcher: a build that does not serve leaves the next launch pristine, generation 0, and `rollback N' restores a sealed generation. With no organism running, the next launch runs the release. --check stages and classifies without applying. Naming a VERSION holds this home there -- the way back after an update -- until a bare `update' lifts it. The gateway also looks at the release channel by itself: `update' in the config sets auto (the default), ask, or off. Its own look only moves forward, goes live while no turn can begin and once none runs, and undoes a release that leaves a provider it served unresolved.")
   (nlk:make-verb "generations" "list the sealed generations of this home's organism"
                  :detail "A generation is the organism as it stood once a fresh child proved its folder: the binary's version and every loaded folder at its revision, numbered from 1 and never pruned (~/.nodecode/generations/). The list marks the one the folder stands at, and says when it stands at none and why. (generations) inside the shell answers the same.")
   (nlk:make-verb "rollback" "restore this home's organism to generation N"
                  :label "rollback N" :arguments "N"
                  :detail "Every folder goes back to what generation N recorded: the layer takes one commit whose tree is N's (its history kept, so forward is the same verb), a hub cell is cloned at N's pin, the binary and its shipped folders stay on the release that runs -- updates keep coming, forward, over the restored folder -- folders N had are installed and ones it never had are taken out where the installer put them; a plain folder whose content differs refuses the whole restore by name. With the organism running it converges and exits once no turn is in flight, and the next launch runs N; with none running, the next launch does. (rollback N) inside the shell does the same.")
   (nlk:make-verb "web" "open this home's organism in a browser tab"
                  :detail "Prints the page's link and opens it where there is a display. The operator token rides after the link's #, which a browser never sends to a server: keep the link to yourself. Needs the organism running (nodecode, or nodecode gateway on) and the web cell: turn web on in /cells.")
   (nlk:make-verb "resume" "open the shell on that session" :arguments "SESSION-ID" :terminal t)
   (nlk:make-verb "rpc" "drive this home's organism from a program: JSON lines in and out"
                  :label "rpc [SESSION-ID]"
                  :arguments "[SESSION-ID [--cursor N]] [--store PATH | --ephemeral]"
                  :detail "Each line on stdin is one gateway command, the object a client_command frame carries: {\"type\":\"start_turn\",\"request\":{\"prompt\":\"...\"}}, which may also name the provider, model and effort a session not yet begun runs on (the default stays where it is), or steer_turn, cancel_turn, withdraw_prompt, interrupt_exec, navigate_checkpoint, get_state (the session as it stands, in its result), set_session_subscriptions. A command_id is minted when a line names none, and a command naming no session_id goes to SESSION-ID, or to a fresh session. Each line on stdout is one gateway frame as the gateway sent it: the session's replay first, then its facts as they land (session_stream_event; turn.completed, turn.failed or turn.cancelled ends a turn), each command's gateway_turn_command_result, and a sync_protocol_error for a line that is not a command. --cursor N, the newest cursor a program read, resumes: the replay carries only what landed past it and says resumed_after, unless the session's history changed under it (a checkpoint move), when the whole replay comes instead. Every shape both ways is src/src/wire/protocol.schema.json. With no organism running it starts one, which stops when stdin closes; one already running keeps running.")
   (nlk:make-verb "help" "this text, or one command's" :arguments "[COMMAND]")
   (nlk:make-verb "trial" "a fresh process of the folder that answers one form (launch.lisp)" :internal t)
   (nlk:make-verb "warm" "the warm-image supervisor (launch.lisp)" :internal t)
   (nlk:make-verb "warm-dump" "build a warm image at PATH (launch.lisp)"
                  :arguments "PATH" :internal t))
  "The executable's commands, in help's order: what PARSE-LAUNCH-ARGUMENTS
resolves, what `nodecode --help' prints, what a mistyped command is matched
against.")

(defparameter *launch-options*
  (list
   (nlk:make-verb "--profile" "run profile NAME: its own home and its own organism"
                  :aliases '("-p") :arguments "NAME" :label "-p, --profile NAME"
                  :detail "Read anywhere on the line. $NODECODE_HOME names a home the same way; with neither, the profile `nodecode profile use' chose boots, else ~/.nodecode/.")
   (nlk:make-verb "--store" "run on the store at PATH" :arguments "PATH")
   (nlk:make-verb "--ephemeral" "run on a throwaway store of this process's own"
                  :detail "The store only: config, the layer and the knowledge cells are still this profile's. HOME=$(mktemp -d) nodecode is a throwaway organism.")
   (nlk:make-verb "--pristine" "run generation 0: the binary alone, no folder loaded"
                  :detail "Nothing of the cell folder loads, no published image is followed, and keeps are refused while it runs: the floor to stand on when the folder itself is the problem. (generations) lists what was sealed and (rollback N) restores one, the next launch running it. A launch whose last self died before serving stands here by itself.")
   (nlk:make-verb "--provider" "open the shell's fresh session on provider P"
                  :arguments "P" :label "--provider P"
                  :detail "--model PROVIDER/MODEL says the same; this says it outright, for a model id whose own prefix names a provider.")
   (nlk:make-verb "--model" "open the shell's fresh session on model M: [PROVIDER/]MODEL[:EFFORT]"
                  :arguments "M" :label "--model M"
                  :detail "This session alone runs on it: the default and every other session stay where they are. M is a model id as /models lists it, behind PROVIDER/ for another provider and before :EFFORT for a reasoning effort: --model openai-codex/gpt-6.1-sol:high. A prefix is the provider only when it names one, and a suffix the effort only when it names a rung and the id without it is no listed model, so ids like openrouter's deepseek/deepseek-r1:free stay whole; --provider and --effort say either part outright. The organism checks the target before the session opens; one it refuses is said in the shell, and nothing runs on the default instead.")
   (nlk:make-verb "--effort" "open the shell's fresh session at reasoning effort E"
                  :arguments "E" :label "--effort E"
                  :detail "--model MODEL:E says the same; this says it outright.")
   (nlk:make-verb "--prompt-file" "send FILE's text as the shell's first prompt"
                  :arguments "FILE" :label "--prompt-file FILE"
                  :detail "Read before anything starts; one that cannot be read, or holds no text, refuses the launch. Its trailing newlines go, as $(cat FILE) drops them. Keeps a long prompt off the command line.")
   (nlk:make-verb "--" "send every word after it as the shell's first prompt"
                  :arguments "PROMPT..." :label "-- PROMPT..."
                  :detail "The words join with one space; quote the prompt to keep it exactly. Nothing after it is read as a command or an option, -p and --help included. The prompt sends once the session stands, as if typed; edited in the composer before then, it waits there instead. nodecode resume SESSION-ID -- PROMPT sends it in that session.")
   (nlk:make-verb "--help" "this text" :aliases '("-h") :label "-h, --help")
   (nlk:make-verb "--version" "the version, and the commit it was built from"))
  "The executable's options, in help's order.")

(defun launch-name (word &rest tables)
  "The name of the launch command or option WORD spells in TABLES, its
aliases resolved, or NIL."
  (let ((verb (some (lambda (table) (nlk:find-verb word table)) tables))) (and verb verb.name)))

(defun refuse-launch-word (word)
  "Refuse WORD, a command or an option the launch does not hold, naming the
nearest one it does."
  (if (and (plusp (length word)) (char= (char word 0) #\-))
      (launch-usage-error "~a" (nlk:unknown-word-text "option" word *launch-options*))
      (launch-usage-error "~a" (nlk:unknown-word-text "command" word *launch-verbs*))))

(defun import-request (args)
  "The setup walk's import request the words ARGS after `import' ask for --
(:import WORLD), T for every world, plus :source, :only and the T-valued
switches -- and the words past it: `nodecode import' and a shell's /import
read one line alike."
  (let* ((word (first args))
         (named (and word (not (uiop:string-prefix-p "--" word)) (pop args)))
         (request nil))
    (when (and named (not (nlk:find-agent-world named)))
      (launch-usage-error
       "~s is not a world this build reads (~{~a~^, ~}); ~
          name a home at a path with --source: nodecode import ~a"
       named (nlk:agent-world-names)
       (nlk:verb-arguments (nlk:find-verb "import" *launch-verbs*))))
    (setf request (list :import (or named t)))
    (flet ((value-for (refusal)
             (or (pop args) (launch-usage-error refusal))))
      (loop while (and args (member (first args)
                                    '("--source" "--only" "--dry-run"
                                      "--overwrite" "--keep-running" "--yes")
                                    :test #'string=))
            do (let ((key (intern (string-upcase (subseq (pop args) 2)) :keyword)))
                 (setf (getf request key)
                       (case key
                         (:source (value-for "--source needs a PATH"))
                         (:only (value-for "--only needs KINDS, comma-separated"))
                         (t t))))))
    (values request args)))

(defun parse-launch-arguments (args &aux (profile nil) (prompt nil))
  "ARGS, the executable's command line, as the launch plan: (values SERVE
CHILD SESSION-ID STORE-PATH EPHEMERAL SETUP PROFILE PRISTINE LAUNCH)."
  ;; SERVE is the headless owner, :WAIT for `gateway run' — the gateway, which
  ;; waits for the store instead of losing it; CHILD what runs before any
  ;; folder loads, (:help TOPIC . MORE), (:version), (:profile WORDS) or
  ;; (:gateway WORDS), or a launch-cache or trial child, (:trial), (:warm) or
  ;; (:warm-dump PATH); SESSION-ID what `resume' named; STORE-PATH what
  ;; `--store' named; EPHEMERAL whether the shell boots a throwaway store of
  ;; its own, which excludes the other three ways of naming one; SETUP whether
  ;; the shell opens the setup wizard on its first frame, T for `setup' and
  ;; the import request plist (:import WORLD ...) for `import', or a channel's
  ;; setup sheet, (:channel-setup CHANNEL) for `gateway setup [CHANNEL]'; PROFILE the
  ;; profile `-p NAME' or `--profile NAME' named, anywhere on the line;
  ;; PRISTINE whether `--pristine' asked for generation 0, the binary alone.
  ;;
  ;; Words resolve through *LAUNCH-VERBS* and *LAUNCH-OPTIONS*, aliases
  ;; included. `help [COMMAND]', or --help or -h anywhere ahead of a `profile'
  ;; or a `gateway' (each reads its own), answers (:help TOPIC), TOPIC the
  ;; first command on the line; --version answers (:version); both outrank the
  ;; rest of the line. A refusal is a LAUNCH-USAGE-ERROR naming what was
  ;; wrong, and the nearest word the tables hold for one they do not.
  ;; `-p NAME' or `--profile NAME' comes out wherever it sits (Hermes's rule).
  ;;
  ;; LAUNCH is what the line asks of the shell's session (#141), a plist
  ;; (:PROVIDER :MODEL :EFFORT :PROMPT) or NIL: --provider, --model and
  ;; --effort, and the first prompt -- every word after the first `--', which
  ;; is cut off before any pass reads the line, so no word of a prompt is
  ;; ever a profile, a help request or a command -- or --prompt-file's text.
  (let ((cut (position "--" args :test #'string=)))
    (when cut
      (setf prompt (format nil "~{~a~^ ~}" (nthcdr (1+ cut) args))
            args (subseq args 0 cut))
      (when (zerop (length (string-trim '(#\Space #\Tab #\Newline) prompt)))
        (launch-usage-error "-- needs the PROMPT after it: nodecode -- \"what the session should do\""))))
  (setf args (loop while args
                   for arg = (pop args)
                   if (member arg '("-p" "--profile") :test #'string=)
                     do (let ((value (or (pop args) (launch-usage-error "~a needs a NAME" arg))))
                          (when profile
                            (launch-usage-error
                             "~a names a second profile, ~a; a launch runs one" arg value))
                          (unless (nlk:profile-name-p value)
                            (launch-usage-error "~s is not a profile name: a-z, 0-9, _ and -, ~
                                                   64 at most, starting with a letter or digit"
                                                value))
                          (setf profile value))
                   else collect arg))
  (let ((head (subseq args 0 (position-if (lambda (word)
                                            (member (launch-name word *launch-verbs*)
                                                    '("profile" "gateway" "help")
                                                    :test #'equal))
                                          args))))
    (when-let (child (cond ((some #'nlk:help-flag-p head)
                            (list :help (some (lambda (word) (launch-name word *launch-verbs*))
                                              args)))
                           ((member "--version" head :test #'string=) (list :version))))
      (return-from parse-launch-arguments (values nil child nil nil nil nil profile nil nil))))
  (let (session-id store-path serve child ephemeral setup pristine target prompt-file)
    (flet ((value-for (refusal)
             ;; The word a flag or a verb takes, or REFUSAL when the line ends first.
             (or (pop args) (launch-usage-error refusal))))
      (loop while args
            for arg = (pop args)
            for name = (launch-name arg *launch-verbs* *launch-options*)
            do (cond
                 ((string= name "serve") (setf serve t))
                 ;; (:login MODE): what the sign-in is for, NIL to ask on the terminal.
                 ((string= name "login")
                  (let ((mode (cond ((equal (first args) "--subscription") (pop args) "subscription")
                                    ((equal (first args) "--api") (pop args) "api"))))
                    (when (and args (not (uiop:string-prefix-p "-" (first args))))
                      (launch-usage-error "login signs in with a browser and takes no CODE; a code from the Machines page is nodecode machine link ~a"
                                          (first args)))
                    (setf child (list :login mode))))
                 ;; (:machine-link CODE): the Machines page's link, NIL to be given a code.
                 ((string= name "machine")
                  (unless (equal (pop args) "link")
                    (launch-usage-error "machine takes one verb: nodecode machine link [CODE]"))
                  (let ((code (and args (not (uiop:string-prefix-p "-" (first args))) (pop args))))
                    (when (and args (not (uiop:string-prefix-p "-" (first args))))
                      (launch-usage-error "machine link takes at most one CODE"))
                    (setf child (list :machine-link code))))
                 ((string= name "setup") (setf setup t))
                 ((string= name "import")
                  (multiple-value-setq (setup args) (import-request args)))
                 ((string= name "trial") (setf child (list :trial)))
                 ((string= name "warm") (setf child (list :warm)))
                 ((string= name "warm-dump")
                  (setf child (list :warm-dump (value-for "warm-dump needs a PATH"))))
                 ((string= name "resume")
                  (setf session-id (value-for "resume needs a SESSION-ID")))
                 ;; (:rpc SESSION-ID CURSOR): the session a program's commands go
                 ;; to when they name none, NIL for a fresh one, and the log
                 ;; position it resumes past. A command word after it is never
                 ;; a session id: `rpc serve' is refused.
                 ((string= name "rpc")
                  (let* ((word (first args))
                         (session (and word (not (uiop:string-prefix-p "-" word))
                                       (not (launch-name word *launch-verbs*))
                                       (pop args)))
                         (cursor (when (equal (first args) "--cursor")
                                   (pop args)
                                   (let ((value (value-for "--cursor needs N, the newest cursor read")))
                                     (or (ignore-errors (parse-integer value))
                                         (launch-usage-error "--cursor takes a log position, not ~s"
                                                             value))))))
                    (when (and cursor (or (null session) (minusp cursor)))
                      (launch-usage-error "--cursor resumes a session past a log position: ~
                                             nodecode rpc SESSION-ID --cursor N"))
                    (setf child (list :rpc session cursor))))
                 ;; `gateway setup [CHANNEL]' is the shell, opened straight on
                 ;; the channel's setup sheet; `gateway setup --help' is the
                 ;; gateway verbs' help, like every verb's.
                 ((and (string= name "gateway") (equal (first args) "setup")
                       (notany #'nlk:help-flag-p (rest args)))
                  (pop args)
                  (setf setup (list :channel-setup
                                    (and args (not (uiop:string-prefix-p "-" (first args))) (pop args))))
                  (when (and args (not (uiop:string-prefix-p "-" (first args))))
                    (launch-usage-error "gateway setup takes one CHANNEL: nodecode gateway setup discord")))
                 ;; `gateway run' is the gateway itself: the headless owner
                 ;; that waits for the store instead of losing it.
                 ((and (string= name "gateway") (equal (first args) "run"))
                  (pop args)
                  (setf serve :wait))
                 ;; The rest of the line is the child's: its flags are its own
                 ;; and the launch parser has no business reading them — the
                 ;; verbs the operating system answers, which run before any
                 ;; folder loads, print, and exit.
                 ((member name '("add" "profile" "gateway" "update" "web" "generations" "rollback")
                          :test #'string=)
                  (setf child (list (intern (string-upcase name) :keyword) args)
                        args nil))
                 ((string= name "help")
                  (let ((word (pop args)))
                    (setf child (list* :help
                                       (and word
                                            (or (launch-name word *launch-verbs*
                                                             *launch-options*)
                                                (refuse-launch-word word)))
                                       args)
                          args nil)))
                 ((string= name "--store")
                  (setf store-path (value-for "--store needs a PATH")))
                 ((string= name "--ephemeral") (setf ephemeral t))
                 ((string= name "--pristine") (setf pristine t))
                 ((member name '("--provider" "--model" "--effort") :test #'string=)
                  (let* ((noun (string-upcase (subseq name 2)))
                         (key (intern noun :keyword))
                         (value (value-for (format nil "~a needs a ~a" name noun))))
                    (when (getf target key)
                      (launch-usage-error "~a names a second ~(~a~), ~a; a session runs on one"
                                          name noun value))
                    (when (or (zerop (length (string-trim " " value)))
                              (char= (char value 0) #\-))
                      (launch-usage-error "~a needs a ~a, not ~s" name noun value))
                    (setf (getf target key) value)))
                 ((string= name "--prompt-file")
                  (when prompt-file
                    (launch-usage-error "--prompt-file names a second FILE; a launch sends one first prompt"))
                  (setf prompt-file (value-for "--prompt-file needs a FILE")))
                 (t (refuse-launch-word arg)))))
    (when (and child (eq (first child) :help))
      (return-from parse-launch-arguments
        (values nil child nil nil nil nil profile nil nil)))
    (when (and prompt prompt-file)
      (launch-usage-error "a launch sends one first prompt: -- PROMPT or --prompt-file FILE"))
    (when (or target prompt prompt-file)
      (when (or serve child)
        (launch-usage-error "~a is the shell's: nodecode [-p NAME] [--provider P] [--model M] [--effort E] -- PROMPT"
                            (cond (target (format nil "--~(~a~)" (first target)))
                                  (prompt-file "--prompt-file")
                                  (t "a first prompt"))))
      (when (and target session-id)
        (launch-usage-error "resume keeps the model ~a began on: /models in the shell picks another"
                            session-id)))
    ;; Read now, before anything boots: a prompt that cannot be had stops
    ;; the launch here, with nothing started.
    (when prompt-file
      (setf prompt (launch-prompt-file prompt-file)))
    (when ephemeral
      (when serve
        (launch-usage-error "--ephemeral is a shell's flag: a headless owner names its store with --store PATH"))
      (when session-id
        (launch-usage-error "--ephemeral cannot resume: a fresh store has no session; ~
                               resume one with --store PATH"))
      (when store-path
        (launch-usage-error "--ephemeral chooses the store; --store PATH names one yourself")))
    (when (and setup serve)
      (launch-usage-error "~a is a shell's verb: a headless owner has no wizard"
                          (cond ((and (consp setup) (eq (first setup) :channel-setup)) "gateway setup")
                                ((consp setup) "import")
                                (t "setup"))))
    (when (and child (eq (first child) :rpc) (or serve session-id setup))
      (launch-usage-error "rpc is a command of its own: nodecode [-p NAME] rpc ~a"
                          (nlk:verb-arguments (nlk:find-verb "rpc" *launch-verbs*))))
    (when (and child (member (first child) '(:login :machine-link :add :profile :gateway :update :web :generations :rollback))
               (or serve session-id store-path ephemeral setup))
      (launch-usage-error "~a is a command of its own: nodecode [-p NAME] ~:*~a ..."
                          (substitute #\Space #\- (string-downcase (first child)))))
    (values serve child session-id store-path ephemeral setup profile pristine
            (and (or target prompt)
                 (loop for key in '(:provider :model :effort :prompt)
                       for value = (if (eq key :prompt) prompt (getf target key))
                       when value append (list key value))))))

(defun launch-prompt-file (file)
  "FILE's text as a first prompt (`--prompt-file FILE'), its trailing
newlines dropped as $(cat FILE) drops them; a file that cannot be read, or
holds no text, is a LAUNCH-USAGE-ERROR."
  ;; A relative FILE is the caller's: read against the working folder.
  (let* ((path (merge-pathnames (uiop:parse-native-namestring file)
                                (uiop:ensure-directory-pathname (uiop:getcwd))))
         (text (handler-case (uiop:read-file-string path :external-format :utf-8)
                 (error (condition)
                   (launch-usage-error "--prompt-file ~a cannot be read: ~a"
                                       file (or (ignore-errors (plain-error-text condition))
                                                (type-of condition)))))))
    (setf text (string-right-trim '(#\Newline #\Return) text))
    (when (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
      (launch-usage-error "--prompt-file ~a holds no prompt" file))
    text))

(defun launch-help-text (&optional topic more)
  "`nodecode --help': the commands and the options, from the tables; or
TOPIC's own help, a command's or an option's."
  ;; The profile command's help is the profile verbs' own
  ;; (NLK:PROFILE-COMMAND), MORE naming one of them.
  (cond
    ((null topic)
     (format nil "usage: nodecode [-p NAME] [COMMAND] [OPTIONS] [-- PROMPT]~%~%~a~%~%commands~%~a~%options~%~a~%Run \"nodecode help COMMAND\" to read one command.~%"
             "With no command, nodecode opens the shell on this home's organism, and starts the organism first when none is running. --model M -- PROMPT opens it on a fresh session running on M, its first prompt sent."
             (nlk:verbs-text *launch-verbs*)
             (nlk:verbs-text *launch-options*)))
    ((equal topic "profile")
     (nlk:profile-command (cons "help" more) :command "nodecode profile"))
    ((equal topic "gateway")
     (nlk:gateway-text (cons "help" more) :command "nodecode gateway"))
    (t (nlk:verb-help-text (or (nlk:find-verb topic *launch-verbs*)
                               (nlk:find-verb topic *launch-options*))
                           :command "nodecode"))))

(defvar *handover-launch* nil
  "T in the successor a switch exec'd into, when that successor is the one
that opens a TUI: this image inherited a live screen from the image that
replaced itself with this one, and owes it two things — the boot says
nothing to it (the report and the boot line go to the store's log, and the
frame already painted stays until this image's first frame paints over
it), and an image that cannot serve must switch its modes off rather than
leave the operator on a screen nobody owns.")

(defun handover-launch-p (&aux (word (uiop:getenv nlk::+handover-variable+)))
  "Whether this launch is a switch's successor, by the environment word the
performer left (profile.lisp, +HANDOVER-VARIABLE+)."
  ;; The word is consumed here: it describes one exec, and nothing this
  ;; process spawns inherits it.
  (ignore-errors (sb-posix:unsetenv nlk::+handover-variable+))
  (and word (plusp (length word)) t))

(defun decide-folder-behind-the-frame (store-path)
  "The folder decision for a TUI launch, taken on the organism-boot thread:
by the time this runs the shell is painting, so nothing here can spend the
operator's first frame."
  ;; Boot-cells loads the folder, or hands this process to a published image
  ;; — the very exec a launcher would otherwise make before it paints, now made
  ;; from behind the frame (launch.lisp, HAND-OVER-TO-IMAGE).
  ;;
  ;; The report goes to the store's log, where the frame is not: boot lines
  ;; written to the terminal would be covered by the TUI. Everything this
  ;; thread goes on to boot still happens after this returns — the folder is
  ;; loaded before anything serves it.
  ;; A log that will not open leaves the streams as they were.
  (let ((log (ignore-errors
              (open (stdio-log-path (or store-path nlk::*default-store-path*))
                    :direction :output :if-exists :append
                    :if-does-not-exist :create :element-type 'character))))
    (with-cleanup ((when log (ignore-errors (close log))))
      (let ((*standard-output* (or log *standard-output*))
            (*error-output* (or log *error-output*)))
        ;; The hand-over waits a second for this process's first frame: a
        ;; successor is told it inherited a live screen, and that is true only
        ;; once one is on the terminal — a screen nothing has painted is not
        ;; one, and the successor would skip entering the alternate screen and
        ;; paint onto the operator's scrollback. A frame that never lands (a
        ;; terminal that refuses writes) is not a reason to exec — the folder
        ;; loads here instead.
        ;; The cells' report and the keeper of the next launch's image are the
        ;; organism's (serve's MAIN, SERVE-GATEWAY), which runs in a process of
        ;; its own: a shell that ran them too walked the folder twice over.
        (nlk:note-boot-phase "folder-decision")
        (nlk:boot-cells
         :spawn t
         :hand-over (lambda (image)
                      (if (nlt:wait-for-frames-past 0 1000)
                          (progn (nlk:note-boot-phase "exec-image")
                                 (nlk::hand-over-to-image image))
                          "the frame never landed")))))))

(defun handover-give-back ()
  "Switch off what this image inherited and never took over: written when a
successor dies before its first frame, the one case where nothing else is
left to restore the terminal."
  (when (and *handover-launch* nlt:*inherit-screen*)
    (setf *handover-launch* nil)
    (ignore-errors (nlt:release-inherited-modes))))

(defmacro answer-and-quit (form &rest clauses)
  "Print what FORM answers -- (values TEXT CODE STREAM) -- and exit on it
(PRINT-ANSWER-AND-QUIT): a refusal no handler CLAUSE answers is the
condition's report, exit 1."
  `(multiple-value-call #'print-answer-and-quit
     (handler-case ,form
       ,@clauses
       (error (condition) (values (princ-to-string condition) 1 :error)))))

(defun main ()
  "Toplevel for the dumped executable (asdf:make :nodecode/bin)."
  ;; The entry is ATTACH-OR-OWN-ORGANISM (client.lisp): the first launch owns
  ;; the store and serves it; later launches attach to the owner's published
  ;; gateway as WebSocket clients. `serve` is the headless owner and `trial`
  ;; the fresh-process child that answers one form (launch.lisp);
  ;; `--ephemeral` boots a throwaway store of its own under the temp
  ;; directory, so a shell can try the folder as it is now beside a live
  ;; organism; `help', `--help' and `--version' answer and exit before
  ;; anything else is read. Errors print and exit nonzero instead of dropping
  ;; an operator into the SBCL debugger.
  (nlk:note-boot-phase "main")
  ;; The debugger-disabled runtime otherwise exits the whole process when
  ;; any background thread reaches its debugger, including layer workers.
  (install-worker-error-guard)
  ;; Raw text (#>TAG) in the image's reader before the first thread exists,
  ;; so eval and every layer load read it (turn.lisp, INSTALL-RAW-TEXT-READER).
  (install-raw-text-reader)
  ;; Ordinary pages before the first thread exists: with huge pages on,
  ;; every thread the organism starts costs some 4 MB resident for a few
  ;; hundred bytes of stack (os.lisp, DISABLE-HUGE-PAGES). And two malloc
  ;; arenas for every thread, not one per contended thread (CAP-MALLOC-ARENAS).
  (nlk:disable-huge-pages)
  (nlk:cap-malloc-arenas)
  ;; Names from /etc/hosts and DNS, not systemd's plugins, which hold off a
  ;; collection for as long as a lookup waits (RESOLVE-NAMES-DIRECTLY).
  (nlk:resolve-names-directly)
  ;; And an exit that never waits on OpenSSL's cleanup (os.lisp).
  (nlk:skip-openssl-exit-cleanup)
  ;; Room for descriptors: macOS starts a process at 256 open files
  ;; (os.lisp, RAISE-FILE-LIMIT), inherited by every child spawned after.
  (nlk::raise-file-limit)
  ;; The gc.lisp collection policy, here, not in the boot ladder, so every
  ;; entry gets it -- the nursery once the arguments say which process this
  ;; is, below. The heap as a whole: garbage past a slack earns a full walk
  ;; (gc.lisp, CAP-GARBAGE), and the collector's card marks for heap not
  ;; yet reached go back -- 8 MB the runtime filled at startup
  ;; (RELEASE-CARD-MARKS).
  (nlk:cap-garbage)
  (nlk:release-card-marks)
  ;; And the other dead window: a generation that grew past its budget is
  ;; collected while the operator is not typing (gc.lisp, COLLECT-WHILE-IDLE).
  (nlk:collect-while-idle)
  (with-handlers ((sb-sys:interactive-interrupt ()
                    ;; The orderly unwind is already done by the time this clause runs:
                    ;; every UNWIND-PROTECT below (stop-gateway, close-store) fired on the
                    ;; way up. What remains is a plain process exit — no second unwind of
                    ;; the threads that are still alive, because SBCL reaps those in
                    ;; arbitrary order under the disabled debugger, and any of them with
                    ;; a join in its cleanup prints a backtrace as its last act.
                    (handover-give-back)
                    (uiop:finish-outputs)
                    (uiop:quit 130 nil))
                  (error (e)
                    (handover-give-back)
                    (format *error-output* "~&nodecode: ~a~%" e)
                    ;; The same plain exit: a thread still alive is reaped, not
                    ;; unwound. Without this, a thread still alive inside a load
                    ;; would print `compilation unit aborted'.
                    (uiop:finish-outputs)
                    (uiop:quit 1 nil)))
    (multiple-value-bind (serve child session-id store-path ephemeral setup profile pristine launch)
        (handler-case (parse-launch-arguments (uiop:command-line-arguments))
          (launch-usage-error (refusal)
            (usage-quit (princ-to-string refusal))))
      ;; The nursery before anything allocates in earnest: SBCL's ~50MB
      ;; default lets a nursery GC land mid-replay and promote the whole
      ;; decoded frame graph (the 328MB-RSS incident). The shell's is its
      ;; own: it paints every frame, and every collection lands in one
      ;; (*SHELL-NURSERY*).
      (if (and (null child) (not serve))
          (nlk:cap-nursery nlk:*shell-nursery*)
          (nlk:cap-nursery))
      ;; Help and the version read no home, so a profile that does not
      ;; exist yet never stands between an operator and the help.
      (when child
        (case (first child)
          (:help (print-answer-and-quit
                  (launch-help-text (second child) (cddr child)) 0 :output))
          (:version (print-answer-and-quit
                     (format nil "nodecode ~a" *user-agent-version*) 0 :output))))
      ;; A launch that opens the shell draws on the controlling terminal, and
      ;; a process with none (cron, ssh without -t, a shell tool) is refused
      ;; here, before the home, the store or the folder is touched.
      (when-let (refusal (terminal-refusal child serve (uiop:command-line-arguments)))
        (write-line refusal *error-output*)
        (uiop:finish-outputs)
        (uiop:quit 2))
      ;; The home is settled before anything reads it. A named profile
      ;; re-resolves every startup parameter under its folder (the init
      ;; hook ran before argv was read); then the home this process ends
      ;; up in — flag, $NODECODE_HOME, the sticky file or the default —
      ;; is published to its children as NODECODE_HOME, so a serve
      ;; candidate, a trial, a warm image or a shell the model runs
      ;; boots into this home and never into the sticky file's.
      (when profile
        (handler-case (nlk:require-profile profile)
          (nlk:profile-error (refusal)
            (usage-quit (format nil "~a \"nodecode profile create ~a\" makes one."
                                refusal profile))))
        (nlk:adopt-profile profile))
      (nlk:publish-home)
      (when ephemeral
        ;; A store of this process's own, and the fact that it is a
        ;; throwaway said before the folder loads: a cell holding an
        ;; external lease reads NLK:*EPHEMERAL* at its start and stands down.
        (setf store-path (ephemeral-store-path))
        (nlk:publish-ephemeral))
      ;; The floor, by hand or after a death: decided here, before a pointer
      ;; is followed or anything paints, by the entries that run an organism
      ;; or front one; a child judges the folder and never stands on it.
      (when pristine (nlk:publish-pristine))
      (when (or serve (null child)) (nlk:decide-pristine))
      ;; A launch that runs an organism asks once whether its home and store
      ;; can be used, before a shell paints or anything boots (HOME-REFUSAL).
      (when-let (refusal (and (or (null child) (eq (first child) :rpc))
                              (home-refusal store-path)))
        (print-answer-and-quit refusal 1 :error))
      ;; The launch-cache verbs run before any folder loads: the `warm`
      ;; supervisor loads nothing itself, and `warm-dump` must load the
      ;; folder directly — never through the cache it is building.
      (when child
        (ecase (first child)
          (:warm (nlk::warm-main))
          (:warm-dump (nlk::warm-dump-main (second child)))
          ;; The answer on stdout, a refusal on stderr, then its exit code.
          (:login (account-login-main (second child)))
          (:machine-link (machine-link-main (second child)))
          (:profile (multiple-value-call #'print-answer-and-quit (profile-answer (second child))))
          (:gateway (gateway-main (second child)))
          (:update (answer-and-quit (values (update-text (second child)) 0 :output)))
          (:generations (generations-main (second child)))
          (:rollback (rollback-main (second child)))
          (:web (answer-and-quit (web-text (second child))))
          ;; what it waits on is said on stderr while it waits (CALL-SAYING-WAITS)
          (:add (answer-and-quit (call-saying-waits (lambda () (add-text (second child))))
                  (gateway-http-error (refusal)
                    (values (gateway-http-error-detail refusal) 1 :error))))
          (:rpc (rpc-main (second child) (third child) store-path))
          (:trial nil)))
      ;; The folder loads in EVERY process before anything else runs:
      ;; the owner, an attached TUI, a headless serve
      ;; candidate. An override of a shipped definition is live wherever
      ;; that definition runs; a folder that fails is one line here and
      ;; in (nlk:cells), never a refused boot. BOOT-CELLS may hand the
      ;; whole process to a published warm image instead of loading — the
      ;; exec re-enters this MAIN with the folder already loaded — and
      ;; only a top-level entry sends a builder after the first paint:
      ;; children take a hit but never spawn.
      ;; A successor a switch exec'd here opens onto a screen that is
      ;; already a TUI's — its predecessor's frame, on the alternate screen,
      ;; waiting for this image's first frame to paint over it. So the boot
      ;; says nothing to that terminal and the first frame takes the screen
      ;; instead of entering it. Only the path that runs a TUI: a headless
      ;; successor owns its stdout, and a launch-cache child answers on it.
      (labels ((launch ()
                 (cond ((and (null child) (not serve))
                        ;; A TUI paints FIRST: the shell the operator sees comes
                        ;; from code this image already holds, and the folder
                        ;; decision — load it, or hand the process to a
                        ;; published image — rides the boot thread behind that
                        ;; frame (DECIDE-FOLDER-BEHIND-THE-FRAME). Nothing in
                        ;; the first frame needs the folder, and the frame is
                        ;; what the operator is waiting for: measured on this
                        ;; machine, the old order spent ~74 ms of the launch in
                        ;; the launcher, ~22 ms on the image's key and notice,
                        ;; and only then painted.
                        ;;
                        ;; A published image for this folder answers the whole
                        ;; launch, so the hand-over is made BEFORE anything
                        ;; paints: the frame the operator then sees is the
                        ;; image's own -- from the folder's code, not the
                        ;; launcher's, which the successor's frame would paint
                        ;; over -- and the exec no longer waits for one.
                        (nlk::hand-over-to-published-image)
                        (attach-or-own-organism :session-id session-id
                                                :store-path store-path
                                                :setup setup
                                                :launch launch
                                                :prelude (lambda ()
                                                           (decide-folder-behind-the-frame
                                                            store-path))))
                       (t
                        ;; a serve leaves its receipt before the load: a self that dies before serving is found by the launch after
                        (nlk:boot-cells :spawn (null child) :receipt (and serve t))
                        ;; A child prints its own report on stdout; the boot line
                        ;; would be noise the parent has to skip.
                        (when (eq (first child) :trial)
                          (nlk::trial-main))
                        (nlk:describe-cells)
                        (if serve
                            ;; Headless owner-or-nothing: the resurrection candidate a
                            ;; client's reconnect ladder spawns, and the manual way to run
                            ;; a gateway with no TUI. Losing the ownership election is a
                            ;; normal exit — the winner is serving, which is what the
                            ;; invoker wanted ensured. `gateway run' (:WAIT), what the
                            ;; gateway's unit runs, never loses: it waits for the store.
                            (progn
                              ;; Configured and effective, once, in the gateway's log.
                              (when-let (line (nlk::file-limit-line))
                                (format *error-output* "~a~%" line)
                                (finish-output *error-output*))
                              (serve-organism-candidate :store-path store-path :wait (eq serve :wait)))
                            (attach-or-own-organism :session-id session-id
                                                    :store-path store-path
                                                    :setup setup))))))
        (setf *handover-launch* (and (handover-launch-p) (null child) (not serve)))
        (cond (*handover-launch*
               (setf nlt:*inherit-screen* t)
               (call-with-severed-stdio
                (stdio-log-path (or store-path nlk::*default-store-path*))
                #'launch))
              (t (launch)))))))

(defun usage-refusal-text (detail)
  "What a refused command line prints: DETAIL, and where the commands are."
  ;; The whole help is `nodecode --help''s alone: a refusal that buried its
  ;; one line under every command made both harder to read.
  (format nil "nodecode: ~a~%Run \"nodecode --help\" to see every command.~%" detail))

(defun usage-quit (detail)
  "Print USAGE-REFUSAL-TEXT for DETAIL to stderr, then exit 2 (bad invocation)."
  (fresh-line *error-output*)
  (write-string (usage-refusal-text detail) *error-output*)
  (uiop:finish-outputs)
  (uiop:quit 2))

(defun terminal-refusal (child serve args)
  "What a launch says where the process has no terminal, or NIL when it can
go on: only a launch that opens the shell -- no CHILD, not a headless SERVE --
draws on one, and the sentence names the command ARGS gave whose table entry
says TERMINAL."
  (unless (or child serve (nlt:terminal-present-p))
    (format nil "nodecode ~@[~a ~]needs a terminal (no controlling tty); run it in one."
            (some (lambda (word &aux (verb (nlk:find-verb word *launch-verbs*)))
                    (and verb verb.terminal verb.name))
                  args))))

(defun print-answer-and-quit (text code stream)
  "Print TEXT — on stdout for an :OUTPUT answer, as a refusal on stderr
otherwise — then exit with CODE."
  (if (eq stream :output)
      (progn (write-string text *standard-output*)
             (fresh-line *standard-output*))
      (format *error-output* "~&nodecode: ~a~%" text))
  (uiop:finish-outputs)
  (uiop:quit code))

(defun terminal-answer (question &key (input *standard-input*) (output *error-output*)
                                      (interactive (interactive-stream-p input)))
  "Ask QUESTION on the terminal and read one line back, or NIL when there is
no terminal to ask: INPUT is not one (a pipe, a script), or this is
Windows, where a line read from the console under this process is unproven
(the TUI reads the console's input records itself)."
  ;; The question goes to OUTPUT, stderr, so an answer piped elsewhere never
  ;; carries it.
  #+win32 (progn question input output interactive nil)
  #-win32
  (when interactive
    (format output "~a " question)
    (finish-output output)
    (read-line input nil nil)))

(defun yes-answer-p (reply)
  "Whether REPLY, a line typed at a question, says yes."
  (and (stringp reply)
       (member (string-trim '(#\Space #\Tab #\Return) reply) '("y" "yes") :test #'string-equal)
       t))

(defun profile-answer (words &key (ask #'terminal-answer))
  "The `profile' command line as (values TEXT EXIT-CODE STREAM): the answer,
0 and :OUTPUT; a refusal, :ERROR and 2 for words the verbs cannot read, 1
otherwise."
  ;; A delete nobody said yes to is asked through ASK: a yes runs it again
  ;; with --yes, any other answer keeps the profile, and with no terminal to
  ;; ask the refusal says how to answer on the line.
  (flet ((run (words) (nlk:profile-command words :command "nodecode profile")))
    (with-handlers ((nlk:profile-usage-error (refusal) (values (princ-to-string refusal) 2 :error))
                    (nlk:profile-error (refusal) (values (princ-to-string refusal) 1 :error)))
      (values (handler-case (run words)
                (nlk:profile-needs-yes (pending)
                  (let ((reply (funcall ask (nlk:profile-needs-yes-question pending))))
                    (cond ((yes-answer-p reply) (run (append words (list "--yes"))))
                          (reply (return-from profile-answer
                                   (values "nothing was deleted" 1 :error)))
                          (t (error pending))))))
              0 :output))))

(defun gateway-main (words)
  "The `gateway' command: the verbs' answer printed, the answer on stdout and a
refusal on stderr, then its exit code; `logs --follow' hands the terminal to
journalctl until it is interrupted."
  ;; Runs before any folder loads: the gateway's lifecycle is the operating
  ;; system's and needs no organism, and `gateway run', the organism itself,
  ;; never comes here.
  ;; The answer, 0 and :OUTPUT; a refusal, :ERROR and 2 for unreadable words, 1 otherwise.
  (answer-and-quit
   (cond ((and (equal (first words) "logs")
               (intersection '("--follow" "-f") (rest words) :test #'string=))
          (handler-case (uiop:run-program (nlk:gateway-log-argv :follow t)
                                          :input :interactive :output :interactive
                                          :error-output :interactive :ignore-error-status t)
            (sb-sys:interactive-interrupt () nil))
          (values "" 0 :output))
         (t (values (nlk:gateway-text words :command "nodecode gateway" :live #'gateway-live-facts)
                    0 :output)))
   (nlk:gateway-usage-error (refusal) (values (princ-to-string refusal) 2 :error))))

(defun web-text (words)
  "`nodecode web': the page's link for the organism serving this home, opened
where there is a display. => (values ANSWER CODE STREAM)."
  ;; The link carries the operator token after its #, which a browser never
  ;; sends to a server, so the terminal that asked is the one place it is
  ;; written: never a shell's transcript, never the log.
  (when (intersection '("--help" "-h" "help") words :test #'string=)
    (return-from web-text
      (values (nlk:verb-help-text (nlk:find-verb "web" *launch-verbs*) :command "nodecode")
              0 :output)))
  (let ((port (organism-port))
        (token (gateway-configured-operator-token)))
    (cond
      ((null port)
       (values "nodecode is not running for this home: start it with `nodecode' (or `nodecode gateway on' to keep it running), then run this again"
               1 :error))
      ((not (eql 200 (nth-value 1 (ignore-errors
                                   (nlk:http :head (format nil "http://127.0.0.1:~d/web/" port)
                                             :timeout 3)))))
       ;; A folder installed and never loaded waits for a restart; one not
       ;; installed, for /cells.
       (values (if (nlk:installed-cell-p "nodecode-web")
                   "the web cell is installed, and the nodecode running for this home has not loaded it: restart it (`nodecode gateway restart' when the gateway keeps it running), then run this again"
                   "the web cell is not installed: in nodecode, open /cells and turn web on with Space, then run this again")
               1 :error))
      ((null token) (values "this home has no operator token" 1 :error))
      (t
       (let ((link (format nil "http://127.0.0.1:~d/web/#t=~a" port token)))
         (values (format nil "~a~%~:[no display here: open the link in a browser on this machine~;opened in your browser~]; it carries the operator token, so keep it to yourself"
                         link (eq t (open-target link)))
                 0 :output))))))
