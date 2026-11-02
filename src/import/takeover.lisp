;;;; takeover.lisp --- the one thing a foreign harness running is allowed to mean.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything else in this folder reads files. This reads the box: whether
;;;; the harness whose home is being imported is RUNNING, and — because two
;;;; long-pollers on one bot token fight — stopping it so its bots answer
;;;; from here.
;;;;
;;;; It is still not a reader per world. The unit is a manifest field
;;;; (WORLD-TAKEOVER-UNIT); this code takes whatever unit it is handed. Every
;;;; world whose home can carry a bot declares one — Hermes and Openclaw run
;;;; bots of their own — and a world without one is never asked about, so its
;;;; bot would land on beside a gateway still polling it.
;;;;
;;;; Only a request somebody made stops it: `nodecode import WORLD' asks first,
;;;; and /import --yes is the answer given ahead. The setup walk's unasked first
;;;; frame applies with keep_running, so a live gateway is never stopped or
;;;; disabled for it; `nodecode import WORLD' moves its bots later.
;;;;
;;;; The watch is the other half. An import that leaves that gateway
;;;; running — --keep-running, a process outside its unit, a stop that
;;;; failed — lands each bot's section off and puts it on the held record
;;;; (plan.lisp); this watch, in the serving process, looks every few
;;;; seconds, and once the foreign gateway has been gone for two looks
;;;; running — a restart passes through `activating', which the probe counts
;;;; as up — turns the held sections on. One way: the held record is gone,
;;;; and a harness started again later finds its bots taken.

(in-package #:nodecode.import)

(defvar *running-probe* 'units-running
  "The function the plan and the watch ask whether a foreign gateway runs,
called with the list of units to look for; a test stubs it.")

(defvar *gateway-stop* 'stop-unit
  "The function a takeover calls to stop a foreign gateway, called with the
unit; a test stubs it.")

(defparameter +unit-up-states+
  '("active" "activating" "deactivating" "reloading" "refreshing")
  "The `systemctl is-active' words for a unit that is up or on its way
through a restart: only `inactive' and `failed' say it is down.")

(defun systemd-unit-state (unit)
  "UNIT's `systemctl --user is-active' word, or NIL where there is no
systemctl."
  (let ((out (ignore-errors
              (nlk:trimmed (nlk:run-bounded (list "systemctl" "--user" "is-active" unit)
                                            :seconds 30 :error-output nil)))))
    (and (stringp out) (plusp (length out)) out)))

(defun command-lines (programs &aux (own (ignore-errors (sb-posix:getpid)))
                                    (found '()))
  "((PID COMM ARGV) ...) for every process but this one: from /proc, ARGV its
words as the process holds them and COMM the name the kernel keeps for it --
or, on a box without /proc, from `pgrep -fl' for each of PROGRAMS, ARGV the
line it prints, the words joined by blanks, and COMM NIL."
  ;; Listed and read without a pathname or a stream a process: through UIOP,
  (dolist (entry (nlk::directory-names "/proc/"))
    (let ((pid (ignore-errors (parse-integer entry))))
      (when (and pid (not (eql pid own)))
        (flet ((proc-file (name)
                 (nlk:if-let (octets (nlt:file-octets (format nil "/proc/~d/~a" pid name)))
                   (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement #\?))
                   "")))
          (nlk:when-let (argv (remove "" (nlk::split-nul (proc-file "cmdline")) :test #'string=))
            (push (list pid (nlk:trimmed (proc-file "comm")) argv) found))))))
  (or found
      (loop for program in programs
            for out = (ignore-errors
                       (string-right-trim '(#\Newline)
                                          (nlk:run-bounded (list "pgrep" "-fl" program)
                                                           :seconds 30 :error-output nil)))
            when (stringp out)
              append (loop for line in (uiop:split-string out :separator '(#\Newline))
                           for space = (position #\Space line)
                           for pid = (and space (ignore-errors (parse-integer line :end space)))
                           when (and pid (not (eql pid own)))
                             collect (list pid nil (string-trim " " (subseq line space)))))))

(defun gateway-command-p (argv unit &optional comm)
  "Whether a process runs UNIT's gateway, read from its ARGV -- its words, or
one line of them joined by blanks as `pgrep' prints it -- and COMM, the name
the kernel keeps for it: named as the unit is (`openclaw-gateway', the title
that gateway gives itself, cut to 15 characters in COMM), or running the
unit's program -- the command itself, the script, `-m' module or `-c' code an
interpreter runs, or a process COMM names as the program -- whose first word
is the unit's verb, `gateway' in openclaw-gateway.service, with nothing after
the verb but an option or `run'."
  ;; The command decides, never a word anywhere in the line: a `journalctl -fu
  ;; openclaw-gateway', a `tail -f ~/.openclaw/logs/gateway.log', an editor on
  ;; a file of that name, `openclaw gateway status' or a `hermes chat' asked
  ;; about the gateway only look at it. Hermes' store
  ;; launcher runs `python3 -I -c CODE gateway run': the code names the
  ;; program, the words after it are the program's own, and COMM is `hermes'
  ;; (hermes_cli/_launchers.py runtime_command, main.py _set_process_title).
  (let* ((joined (stringp argv))
         (words (if joined (remove "" (ppcre:split "\\s+" argv) :test #'string=) argv))
         (stem (string-downcase (subseq unit 0 (or (cl:search ".service" unit) (length unit)))))
         (cut (position-if (lambda (ch) (find ch ".-")) stem))
         (program (subseq stem 0 cut))
         (verb (and cut (subseq stem (1+ cut))))
         (comm (and comm (string-downcase comm)))
         (base (and words (string-downcase (nle::shell-program-name (first words))))))
    (labels ((ours-p (parts)
               ;; The program, its package or its checkout: hermes, hermes_cli, hermes-agent.
               (some (lambda (part) (or (string= part program)
                                        (uiop:string-prefix-p (format nil "~a_" program) part)
                                        (uiop:string-prefix-p (format nil "~a-" program) part)))
                     parts))
             (verb-first-p (args)
               ;; Past options, `--profile NAME' among them, the verb; then an option, `run' or nothing.
               (loop with skip = nil
                     for (word next) on (mapcar #'string-downcase args)
                     do (cond (skip (setf skip nil))
                              ((member word '("-p" "--profile") :test #'string=) (setf skip t))
                              ((not (uiop:string-prefix-p "-" word))
                               (return (and (string= word verb)
                                            (or (null next) (uiop:string-prefix-p "-" next)
                                                (string= next "run"))))))))
             (interpreted (options)
               ;; => the words that name what an interpreter runs, and the words it runs with.
               (loop with skip = nil
                     for (word . after) on options
                     do (cond (skip (setf skip nil))
                              ((string= word "-m")
                               (return (values (ppcre:split "[/.]" (string-downcase (or (first after) "")))
                                               (rest after))))
                              ;; The code's words name the program; on a joined line they run on.
                              ((member word '("-c" "-e" "--eval") :test #'string=)
                               (let ((code (if joined after (list (first after)))))
                                 (return (values (ppcre:split "[^a-z0-9_-]+" (string-downcase (format nil "~{~a~^ ~}" code)))
                                                 (if joined after (rest after))
                                                 joined))))
                              ;; Python's -X dev and -W, Node's --require: an operand, not the program.
                              ((member word '("-X" "-W" "-Q" "-r" "--require" "--import" "--loader") :test #'string=)
                               (setf skip t))
                              ((not (uiop:string-prefix-p "-" word))
                               (return (values (ppcre:split "[/.]" (string-downcase word)) after)))))))
      (when base
        (or (string= base stem)
            (and comm (string= comm (subseq stem 0 (min 15 (length stem)))))
            (multiple-value-bind (parts args run-on)
                (if (ppcre:scan "\\A(?:node(?:js)?|bun|deno|python[0-9.]*|pypy[0-9.]*|sh|bash|dash|zsh)\\z" base)
                    (interpreted (rest words))
                    (values (list base) (rest words)))
              (and (or (ours-p parts) (and comm (string= comm program)))
                   (or (null verb)
                       (if run-on
                           ;; The code ends where a word closes it, as Hermes' own reader finds it.
                           (loop for (end . tail) on args
                                   thereis (and (uiop:string-suffix-p end ")") (verb-first-p tail)))
                           (verb-first-p args))
                       ;; A script the verb's folder runs: Hermes' gateway/run.py.
                       (cl:search (list verb "run") parts :test #'string=)))))))))

(defun units-running (units)
  "What says one of UNITS's harnesses is up on this box, one line each —
the systemd user unit, up or passing through a restart, and every process
that runs its gateway (GATEWAY-COMMAND-P) — or NIL."
  (append
   (loop for unit in units
         for state = (systemd-unit-state unit)
         when (member state +unit-up-states+ :test #'equal)
           collect (format nil "systemd user unit ~a is ~a" unit state))
   (when units
     ;; A unit's program is what the process table is searched by: hermes-gateway.service is `hermes'.
     (let ((programs (nlk:distinct
                      (mapcar (lambda (unit)
                                (string-downcase
                                 (subseq unit 0 (position-if (lambda (ch) (find ch ".-")) unit))))
                              units))))
       (loop for (pid comm argv) in (command-lines programs)
             when (some (lambda (unit) (gateway-command-p argv unit comm)) units)
               collect (format nil "pid ~d: ~a" pid (nlk:clip (if (stringp argv) argv (format nil "~{~a~^ ~}" argv))
                                                          80 :ellipsis "…")))))))

(defun systemctl (&rest args)
  "`systemctl --user ARGS' => (values EXIT-CODE STDERR-OR-NIL), the code
:TIMEOUT past two minutes; a systemctl that cannot run at all is a refusal."
  (nlk:with-handlers ((error (condition)
                        (fail "could not run systemctl: ~a" condition)))
    (nlk:bind (((_ err code)
                (nlk:run-bounded (list* "systemctl" "--user" args) :seconds 120 :output nil)))
      (let ((err (string-right-trim '(#\Newline) err)))
        (values code (and (plusp (length err)) err))))))

(defun stop-unit (unit)
  "Stop UNIT and check that it is down — the bots are free now — then
disable it, so nothing starts it again at the next login to fight for them."
  ;; The stop has to succeed; the disable is reported either way (a transient
  ;; unit has no file to disable). => one line saying what was done.
  (multiple-value-bind (code err) (systemctl "stop" unit)
    (unless (eql code 0)
      (fail "systemctl --user stop ~a failed (~a)~@[: ~a~]" unit code err)))
  (let ((state (systemd-unit-state unit)))
    (when (member state +unit-up-states+ :test #'equal)
      (fail "~a is still ~a after systemctl --user stop" unit state)))
  (multiple-value-bind (code err) (systemctl "disable" unit)
    (if (eql code 0)
        (format nil "~a stopped and disabled" unit)
        (format nil "~a stopped, but disabling it failed (~a)~@[: ~a~] — it may start again at the next login"
                unit code err))))

;;; --- the held record ---------------------------------------------------------
;;; The bots an import left off because another harness's gateway ran them,
;;; one platform per line in this folder's own directory. An import's action
;;; writes it; the watch below reads it and hands the bots over. The two run
;;; on different threads.

(defvar *held-lock* (bt2:make-lock :name "import-held")
  "Serializes the held record's read-modify-writes.")

(defun held-path (settings)
  (merge-pathnames "held" (merge-pathnames "import/" (getf settings :home))))

(defun held-platforms (settings)
  "The platforms on the held record, oldest first."
  (nlk:split-words (nlk:read-text (held-path settings))))

(defun write-held (settings platforms &aux (path (held-path settings)))
  "PLATFORMS as the held record; none deletes it."
  (cond (platforms
         (nlk:write-file-atomically path (format nil "~{~a~%~}" platforms)))
        ((probe-file path) (delete-file path)))
  platforms)

(defun set-held (settings platform held)
  "PLATFORM onto the held record when HELD, off it otherwise."
  (bt2:with-lock-held (*held-lock*)
    (let ((others (remove platform (held-platforms settings) :test #'string=)))
      (write-held settings (if held (append others (list platform)) others)))))

;;; --- the watch ---------------------------------------------------------------

(defparameter +absent-looks+ 2
  "Looks in a row that must find no foreign gateway before the bots move.")

(defvar *start-watch* 'start-watch
  "What an install and an applied import call to watch the held bots; a
test stubs it and drives WATCH-STEP itself.")

(defvar *watch-lock* (bt2:make-lock :name "import-watch")
  "Starting and stopping the watch serialize.")

(defvar *watch* nil
  "The watch's worker while it runs.")

(defvar *absent-looks* 0
  "Looks in a row that found no foreign gateway.")

(defun watch-step (settings)
  "One look at the held bots: none held → :IDLE; the foreign gateway up →
:WAITING; gone for fewer than +ABSENT-LOOKS+ looks in a row → :GONE; gone
that long → every held section handed over → :HANDED."
  (cond ((null (held-platforms settings))
         (setf *absent-looks* 0)
         :idle)
        ;; Every takeover unit this build knows: the held record says a bot is held, not by whom.
        ((funcall *running-probe*
                  (remove nil (mapcar #'nlk:agent-world-takeover-unit nlk:*agent-worlds*)))
         (setf *absent-looks* 0)
         :waiting)
        ((< (incf *absent-looks*) +absent-looks+)
         :gone)
        (t
         (setf *absent-looks* 0)
         ;; Held sections on (one changed meanwhile left as it is), the record cleared, one restart.
         (nlk:when-let (on (bt2:with-lock-held (*held-lock*)
                             (prog1 (loop with config = (nle:read-shared-config)
                                          for platform in (held-platforms settings)
                                          for section = (nlk:json-value config :any "channels" platform)
                                          when (and (hash-table-p section)
                                                    (not (nlk:config-boolean section "enabled" t)))
                                            do (nle:config-set (list "channels" platform "enabled") t)
                                            and collect platform)
                               (write-held settings '()))))
           (let ((names (format nil "~{~a~^ and ~}" (mapcar #'string-capitalize on))))
             (nlk:with-handlers ((error (condition)
                                   (nle:notice (format nil "the other gateway stopped and the ~a bot~p ~:[is~;are~] on in the config, but the channel kit did not restart: ~a — (restart-cells \"nodecode-channel-kit\") tries again"
                                                       names (length on) (rest on) condition)
                                               :level :warning)))
               (dolist (platform (rest on))
                 (need-cell (format nil "nodecode-channel-~a" platform)))
               (need-cell (format nil "nodecode-channel-~a" (first on)) :fresh t)
               (nle:notice (format nil "the other gateway stopped: the ~a bot~p now answer~:[s~;~] from nodecode"
                                   names (length on) (rest on))))))
         :handed)))

(defun start-watch ()
  "Watch the held bots from this process: nothing held, an organism that
holds no channel (--ephemeral), or a watch already running starts nothing."
  ;; => the worker, or NIL. The first look is at once: a gateway stopped
  ;; while nothing watched is owed its hand-over now, not a period later.
  (bt2:with-lock-held (*watch-lock*)
    (let ((settings (import-settings)))
      (when (and settings
                 (not nlk:*ephemeral*)
                 (held-platforms settings)
                 (not (and *watch* (bt2:thread-alive-p (nlk:worker-thread *watch*)))))
        (setf *absent-looks* 0
              *watch* (nlk:worker-start "import-watch"
                                        (lambda ()
                                          (and (member (watch-step settings) '(:idle :handed))
                                               :stop))
                                        :wake 5 :prime t))))
    *watch*))
