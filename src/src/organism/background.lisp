;;;; background.lisp --- the gateway: this home's organism, kept running by
;;;; the operating system.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An organism lives as long as the process that owns its store: the first
;;;; shell a launch opens, or `nodecode serve'. A bot, a cron job, the bots
;;;; an import took over from Hermes — each wants the organism up while no
;;;; shell is open: at boot, after a logout, after a crash. That is the
;;;; gateway: the organism served headless by the operating system's own
;;;; service manager, which starts it at login, restarts it when it dies and
;;;; stops it when asked. One unit per home — on Linux a systemd user unit,
;;;; nodecode-gateway.service for ~/.nodecode/, nodecode-gateway-NAME.service
;;;; for a profile; on macOS a LaunchAgent of the same name, plainly, under
;;;; ~/Library/LaunchAgents, through a launcher that never moves (below) —
;;;; whose process is `nodecode gateway run': the organism,
;;;; waiting while a shell owns the store and taking it over the moment that
;;;; shell lets go. A shell and the gateway never contend: a shell opened
;;;; while the gateway serves attaches to it, and a shell attached when the
;;;; gateway is turned off takes the organism back (client.lisp).
;;;;
;;;; This file is the mechanism, and the one text every shell speaks
;;;; (GATEWAY-TEXT): on, off, restart, the log, the state. Kernel, and
;;;; locked: what the operating system runs at every login is not material
;;;; the organism rewrites in itself. Every question to the operating system
;;;; goes through one runner (*UNIT-RUNNER*), and which manager is spoken to
;;;; is a value (*SERVICE-MANAGER*), so the gates script systemd and launchd
;;;; instead of driving them, each on whatever box runs the suite. A logon
;;;; task on Windows is the arm still to come; there the state says so.

(in-package #:nodecode.kernel)

(define-refusal gateway-error gateway-fail "~a"
  "A gateway verb refused; DETAIL is the whole report." :export t)

(define-error gateway-usage-error (gateway-error)
    ()
  ;; DETAIL ends by pointing at the help; a command line exits 2 on it, the
  ;; code for a bad invocation.
  (:documentation "A gateway request its verbs cannot read: an unknown verb,
a word left over.")
  (:export))

;;; --- where the unit lives, and who is asked ----------------------------------

;;; The gates SETF it beside a scripted *UNIT-RUNNER*, so the launchd arm is
;;; proved on a Linux box and the systemd arm on a Mac.
(define-startup-parameter *service-manager*
    ;; a user unit on Linux, a LaunchAgent on macOS, no arm yet on Windows
    #+linux :systemd #+darwin :launchd #-(or linux darwin) nil
  "Which service manager the gateway verbs speak to, resolved when the
process starts.")

;;; NIL where the background gateway has no arm yet, and under the hermetic
;;; test posture, whose gates bind a scratch folder and script *UNIT-RUNNER*.
(define-startup-parameter *unit-directory*
    (case *service-manager*
      (:systemd (merge-pathnames "systemd/user/"
                                 (or (environment-directory "XDG_CONFIG_HOME")
                                     (home-path ".config/"))))
      (:launchd (home-path "Library/LaunchAgents/")))
  "Where the gateway's unit file is written: systemd's or launchd's folder.")

;;; NIL off macOS, in a developer image and under the hermetic test posture;
;;; the launchd gates bind a scratch file.
(define-startup-parameter *shipped-launcher*
    (let ((directory (runtime-directory)))
      (and directory (probe-file (merge-pathnames "nodecode-launcher" directory))))
  "The gateway launcher this build carries beside its executable
(src/tools/launcher.c, compiled by a macOS release), or NIL.")

;;; The gates bind a script of systemd's or launchd's answers.
(defvar *unit-runner* 'run-unit-program
  "What every question to the operating system goes through: a function of
one argv answering (values OUTPUT ERROR-OUTPUT STATUS) as RUN-BOUNDED does,
STATUS :MISSING for a program that is not installed.")

(defun run-unit-program (argv)
  "ARGV run to completion, two minutes at most — a stop that waits on the
gateway's own orderly unwind included — under the user manager's XDG_RUNTIME_DIR; a
program that cannot be run answers STATUS :MISSING."
  (with-handlers ((error (condition)
                    (values "" (princ-to-string condition) :missing)))
    (run-bounded argv :seconds 120 :environment
                 ;; su and cron leave it unset; systemctl --user finds the user manager through it
                 #-win32 (unless (environment-directory "XDG_RUNTIME_DIR")
                           (let ((directory (format nil "/run/user/~d" (sb-posix:getuid))))
                             (when (uiop:directory-exists-p (concatenate 'string directory "/"))
                               (list (concatenate 'string "XDG_RUNTIME_DIR=" directory)))))
                 #+win32 nil)))

(defun unit-run (&rest argv)
  "ARGV through *UNIT-RUNNER*: (values OUTPUT ERROR-OUTPUT STATUS)."
  (funcall *unit-runner* argv))

;;; --- systemd supervision -------------------------------------------------

;;; The serve owner sends its heartbeat from the same thread that waits for or
;;; serves the store. A helper thread would be stopped by the same
;;; stop-the-world failure this watchdog is meant to catch.
(defparameter *gateway-watchdog-seconds* 30
  "The watchdog window in the generated unit, in seconds.")

(defvar *systemd-notify-failed* nil
  "Whether the last systemd notify send failed, so one broken socket does not
fill the journal once per heartbeat.")

(defun systemd-notify (message)
  "Send MESSAGE to systemd's NOTIFY_SOCKET, or NIL outside systemd."
  ;; The socket is a Unix datagram. Systemd uses a leading `@' to name an
  ;; abstract socket; SBCL exposes that namespace through
  ;; LOCAL-ABSTRACT-SOCKET. Notify is best effort so `nodecode gateway run'
  ;; remains usable in a foreground shell.
  #+linux
  (let ((address (uiop:getenv "NOTIFY_SOCKET")))
    (when (and (stringp address) (plusp (length address)))
      (handler-case
          (let* ((abstract (char= (char address 0) #\@))
                 (socket (make-instance
                          (if abstract
                              'sb-bsd-sockets:local-abstract-socket
                              'sb-bsd-sockets:local-socket)
                          :type :datagram))
                 (name (if abstract (subseq address 1) address)))
            (with-cleanup ((sb-bsd-sockets:socket-close socket))
              (sb-bsd-sockets:socket-connect socket name)
              (when (= (sb-bsd-sockets:socket-send socket message (length message))
                       (length message))
                (setf *systemd-notify-failed* nil)
                t)))
        (error ()
          (unless *systemd-notify-failed*
            (setf *systemd-notify-failed* t)
            (format *error-output* "~&[CP-ISSUE-20260912-003] systemd notify failed~%"))
          nil))))
  #-linux
  (declare (ignore message)))

(defun systemd-ready ()
  "Tell a systemd notify unit that the gateway owner is initialized."
  (systemd-notify "READY=1"))

(defun systemd-watchdog ()
  "Refresh a systemd watchdog from the gateway owner thread."
  (systemd-notify "WATCHDOG=1"))

(defun answer-line (&rest texts)
  "The first non-blank line of the first of TEXTS that has one, or NIL:
what a refusal quotes of a program's answer."
  (loop for text in texts
        thereis (find-if #'plusp (mapcar #'trimmed (lines text)) :key #'length)))

(defun manager-run (&rest arguments)
  "The service manager's own command with ARGUMENTS — systemctl --user, or
launchctl: (values OUTPUT ERROR-OUTPUT STATUS ARGV)."
  (bind ((argv (append (ecase *service-manager*
                         (:systemd (list "systemctl" "--user"))
                         (:launchd (list "launchctl")))
                       arguments)) ((out err status) (apply #'unit-run argv)))
    (values out err status argv)))

(defun manager-or-fail (&rest arguments)
  "The service manager's own command — systemctl --user, or launchctl — with
ARGUMENTS, or a GATEWAY-ERROR quoting its answer. => the output."
  (multiple-value-bind (out err status argv) (apply #'manager-run arguments)
    (unless (eql status 0)
      (gateway-fail "~{~a~^ ~} failed~@[: ~a~]" argv (answer-line err out)))
    out))

(defun platform-reason ()
  (format nil "no background gateway on ~a yet: it runs under systemd on Linux and ~
               launchd on macOS — `nodecode serve' in a terminal you keep open runs ~
               the organism headless"
          (software-type)))

;;; --- launchd supervision -------------------------------------------------
;;; macOS keeps an account's programs running through launchd: a LaunchAgent,
;;; a property list under ~/Library/LaunchAgents that launchd loads into the
;;; account's gui/<uid> domain at login and, bootstrapped by hand, at once.
;;; RunAtLoad starts it, KeepAlive brings it back from any exit, and there is
;;; no notify socket, no watchdog and no journal: stdout and stderr go to a
;;; file of the agent's own. An agent lives in the login session and ends
;;; with it — launchd has no linger; a program kept up with nobody logged in
;;; is a LaunchDaemon, root's, and not offered here.

(defun launchd-domain ()
  "This account's launchd domain, gui/<uid>: where a LaunchAgent lives."
  (format nil "gui/~d" #-win32 (sb-posix:getuid) #+win32 0))

(defun launchd-target (label)
  "The service LABEL names in this account's domain, as launchctl spells it."
  (format nil "~a/~a" (launchd-domain) label))

(defun agent-loaded-p (label)
  "Whether launchd holds the agent LABEL in this account's domain now."
  (eql 0 (nth-value 2 (manager-run "print" (launchd-target label)))))

(defun boot-out-agent (label)
  "Boot the agent LABEL out, then wait, ten seconds at most, for launchd to
let go of it: the unload is not instant, and a bootstrap that lands during it
is refused."
  (manager-run "bootout" (launchd-target label))
  (wait-until (lambda () (not (agent-loaded-p label))) (deadline-after 10) 0.2))

;;; macOS remembers a protected folder (Desktop, Documents, Downloads) granted
;;; to an agent against the file launchd started, its path and its code hash.
;;; The organism's file moves with every release and every warm launch image,
;;; so launchd starts a launcher that never moves instead, and the launcher
;;; runs the organism as its child: a child's access is asked on its parent's
;;; behalf (src/tools/launcher.c). Installed once and never rewritten after,
;;; since a grant is held against its bytes; a launcher that must change is a
;;; new generation, a new folder, and one new question to the operator.
(defparameter +launcher-generation+ 1
  "The launcher's generation: the folder GATEWAY-LAUNCHER-PATH installs it in.")

(defun gateway-launcher-path ()
  "Where launchd's launcher lives: Application Support/nodecode/launcher/
<generation>/nodecode beside the LaunchAgents folder, named as System
Settings shows it. NIL with no unit folder."
  (and *unit-directory*
       (merge-pathnames (format nil "Application Support/nodecode/launcher/~d/nodecode"
                                +launcher-generation+)
                        (uiop:pathname-parent-directory-pathname *unit-directory*))))

(defun install-gateway-launcher (&aux (path (gateway-launcher-path)))
  "The launcher at GATEWAY-LAUNCHER-PATH, copied from *SHIPPED-LAUNCHER* when
it is not there yet. => its native namestring."
  (unless (probe-file path)
    (unless *shipped-launcher*
      (gateway-fail "this build carries no gateway launcher (nodecode-launcher beside ~
                     the executable), and launchd runs the gateway through one"))
    (let ((temp (make-pathname :name ".nodecode" :type "tmp" :defaults path)))
      (ensure-directories-exist path)
      (uiop:copy-file *shipped-launcher* temp)
      #-win32 (sb-posix:chmod (uiop:native-namestring temp) #o755)
      (replace-file temp path)))
  (uiop:native-namestring path))

(defun launcher-child (pid)
  "The pid of the organism the launcher at PID runs, its one child, as
text; NIL before it has one."
  ;; -a: macOS's pgrep leaves its own ancestors out of what it matches, and the
  ;; organism that asks is the child asked after.
  (and pid
       (bind (((out _ status) (unit-run "pgrep" "-a" "-P" pid)))
         (and (eql status 0) (first (lines out))))))

(defun account-name ()
  "This process's login name: the password database's, else $USER."
  #-win32 (or (ignore-errors (sb-posix:passwd-name (sb-posix:getpwuid (sb-posix:getuid))))
              (uiop:getenv "USER"))
  #+win32 (uiop:getenv "USERNAME"))

;;; --- the unit ------------------------------------------------------------------

(defun own-account-home-p ()
  "Whether this process's $HOME is the account's own home, the folder the
password database names (ACCOUNT-HOME), compared by true name so a link to
it counts. True where there is no password entry to ask."
  ;; The service manager is the account's, one per user: systemd --user and
  ;; launchd's gui/<uid> domain hold ONE nodecode-gateway whatever $HOME a
  ;; process was handed. A borrowed $HOME -- a proof's scratch HOME -- has a
  ;; ~/.nodecode/ of its own that is not the account's default home, and
  ;; naming its gateway plainly took the account's (Sai's Mac, 2026-10-21:
  ;; a scratch HOME's first launch bootstrapped nodecode-gateway and the
  ;; account's own gateway exited 143).
  (let ((account (account-home)))
    (or (null account)
        (flet ((true (path) (namestring (or (ignore-errors (truename path))
                                            (uiop:ensure-directory-pathname path)))))
          (equal (true (user-homedir-pathname)) (true account))))))

(defun account-named-home-p (home)
  "Whether HOME is a home nodecode names for this account: the default one
or a named profile's, under the account's own $HOME (OWN-ACCOUNT-HOME-P)."
  (and (own-account-home-p)
       (or (equal (namestring home) (namestring (uiop:ensure-directory-pathname (default-home))))
           (profile-named-p home))))

(defun gateway-own-label (&optional (home *home*))
  "What HOME's gateway is called when no unit serves it yet, before any
suffix of the manager's own: nodecode-gateway for the account's default
home, nodecode-gateway-NAME for its named profile (a profile name is
already a unit name), and for any other home — a $NODECODE_HOME of the
operator's own, or the default home of a $HOME that is not the account's —
eight hex digits of its path's digest: two folders called the same are
still two organisms, and no borrowed $HOME takes the account's own name."
  (let ((home (uiop:ensure-directory-pathname home)))
    (cond ((not (account-named-home-p home))
           (format nil "nodecode-gateway-~a"
                   (subseq (sha256-text (namestring home)) 7 15)))
          ((profile-named-p home)
           (format nil "nodecode-gateway-~a" (profile-name home)))
          (t "nodecode-gateway"))))

(defun gateway-by-default-p (&optional (home *home*))
  "Whether HOME's gateway comes on by itself at a boot: the homes nodecode
names for this account, the default one and a named profile's. Any other
folder $NODECODE_HOME names -- a proof's scratch home among them -- and any
home under a $HOME that is not the account's own is the operator's to turn
on."
  (account-named-home-p (uiop:ensure-directory-pathname home)))

(defun gateway-unit-name (&optional (home *home*) (manager *service-manager*))
  "The unit HOME's gateway runs as under MANAGER: the label with systemd's
.service — nodecode-gateway.service, nodecode-gateway-NAME.service — or the
label itself, which is how launchd names an agent."
  (concatenate 'string (gateway-label home) (if (eq manager :systemd) ".service" "")))

(defun gateway-unit-path (&optional (home *home*))
  "Where HOME's unit file lives, or NIL where there is no unit folder: the
unit's own name under systemd's folder, <label>.plist under launchd's."
  (and *unit-directory*
       (merge-pathnames (concatenate 'string (gateway-unit-name home)
                                     (if (eq *service-manager* :launchd) ".plist" ""))
                        *unit-directory*)))

(defun gateway-log-path (&optional (home *home*))
  "Where HOME's gateway writes under launchd, which keeps no journal:
Library/Logs/nodecode/<label>.log beside the LaunchAgents folder, stdout
and stderr both. NIL with no unit folder."
  (and *unit-directory*
       (merge-pathnames (format nil "Logs/nodecode/~a.log" (gateway-label home))
                        (uiop:pathname-parent-directory-pathname *unit-directory*))))

;;; OFF removes only a unit that carries it, and ON never writes over one that
;;; does not: a unit of that name someone else wrote — an older build's
;;; hand-made nodecode-gateway.service — is theirs.
(defparameter +unit-marker+ "nodecode gateway: "
  "How every unit this file writes names the home it serves, in its first
two lines: `# nodecode gateway: <home>' opens a systemd unit, and `<!--
nodecode gateway: <home> -->' follows a plist's XML declaration.")

(defun unit-file-owner (path)
  "Who wrote the unit file at PATH: :OURS when it opens with +UNIT-MARKER+
(within two lines), :FOREIGN when anything else is there, NIL when nothing is."
  (when (and path (probe-file path))
    (loop for line in (lines (ignore-errors (uiop:read-file-string path))) repeat 2
          when (search +unit-marker+ line) return :ours
          finally (return :foreign))))

(defun xml-escaped (text)
  "TEXT as XML character data: the four characters a plist cannot carry
bare replaced by their references."
  (ppcre:regex-replace-all "[&<>\"]" text
                           (lambda (match)
                             (ecase (char match 0)
                               (#\& "&amp;") (#\< "&lt;") (#\> "&gt;") (#\" "&quot;")))
                           :simple-calls t))

(defun unit-quoted (text &key exec)
  "TEXT as one double-quoted word of a unit file: backslashes and quotes
escaped, `%' doubled — systemd's specifier sign — and, under EXEC, `$'
doubled too: ExecStart= expands variables, Environment= does not."
  (format nil "\"~a\"" (ppcre:regex-replace-all (if exec "[\\\\\"%$]" "[\\\\\"%]") text
                                                  (lambda (match)
                                                    (if (find match '("%" "$") :test #'string=)
                                                        (concatenate 'string match match)
                                                        (concatenate 'string "\\" match)))
                                                  :simple-calls t)))

;;; A home's own name hangs on the $HOME of the process asking: the same
;;; profile is nodecode-gateway-NAME from the account's own $HOME and a
;;; digest from a borrowed one. A `gateway on' asked from the other side
;;; therefore found no unit and wrote a second one for the same home; both
;;; ran, one serving and one waiting on the store, and an update's handoff
;;; raced between them. So the unit that already names a home -- by true
;;; name, through any link -- is that home's gateway, whatever it is called,
;;; and only a home no unit serves gets its own name.

(defun unit-file-path (label)
  "Where a unit called LABEL lives in the unit folder."
  (merge-pathnames (concatenate 'string label (case *service-manager*
                                                (:systemd ".service")
                                                (:launchd ".plist")
                                                (t "")))
                   *unit-directory*))

(defun true-home-name (folder)
  "FOLDER, a pathname or a native namestring, as a directory's true name
when it exists, else as written."
  (let ((path (uiop:ensure-directory-pathname
               (if (pathnamep folder) folder (uiop:parse-native-namestring folder)))))
    (namestring (or (ignore-errors (truename path)) path))))

(defun unit-unquoted (word)
  "WORD as UNIT-QUOTED wrote it, read back: its quotes off, `\\X' as X and
`%%' as `%'."
  (let ((bare (if (and (>= (length word) 2)
                       (char= #\" (char word 0) (char word (1- (length word)))))
                  (subseq word 1 (1- (length word)))
                  word)))
    (ppcre:regex-replace-all "\\\\(.)|%%" bare
                             (lambda (match escaped)
                               (declare (ignore match))
                               (or escaped "%"))
                             :simple-calls t)))

(defun xml-unescaped (text)
  "TEXT with the references XML-ESCAPED writes read back."
  (ppcre:regex-replace-all "&(amp|lt|gt|quot);" text
                           (lambda (match name)
                             (declare (ignore match))
                             (cdr (assoc name '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\""))
                                         :test #'string=)))
                           :simple-calls t))

(defun unit-file-home (path &aux (text (and path (ignore-errors (uiop:read-file-string path)))))
  "The home the gateway unit file at PATH serves, by true name: its
NODECODE_HOME, a systemd Environment= line or a plist's EnvironmentVariables
entry, else the folder its marker names. NIL when it names none."
  (let ((named
          (and text
               (or (loop for line in (lines text)
                         for word = (and (uiop:string-prefix-p "Environment=" line)
                                         (unit-unquoted (string-trim " " (subseq line 12))))
                         when (and word (uiop:string-prefix-p "NODECODE_HOME=" word))
                           return (subseq word 14))
                   (ppcre:register-groups-bind (value)
                       ("<key>NODECODE_HOME</key>\\s*<string>([^<]*)</string>" text)
                     (xml-unescaped value))
                   (loop for line in (lines text) repeat 2
                         for at = (search +unit-marker+ line)
                         when at
                           return (string-trim " " (ppcre:regex-replace "\\s*-->\\s*$" (subseq line (+ at (length +unit-marker+))) "")))))))
    (and (plusp (length named)) (true-home-name named))))

(defun gateway-unit-files ()
  "Every gateway unit file nodecode wrote in the unit folder, by name."
  (and *unit-directory* (member *service-manager* '(:systemd :launchd))
       (sort (remove-if-not (lambda (path) (eq :ours (unit-file-owner path)))
                            (directory (unit-file-path "nodecode-gateway*") :resolve-symlinks nil))
             #'string< :key #'file-namestring)))

(defun gateway-units-serving (&optional (home *home*) &aux (true (true-home-name home)))
  "The gateway unit files nodecode wrote that serve HOME, by name."
  (remove-if-not (lambda (path) (equal true (unit-file-home path))) (gateway-unit-files)))

(defun gateway-label (&optional (home *home*) &aux (own (gateway-own-label home)))
  "What HOME's gateway is called to the service manager, before any suffix
of the manager's own: its own name (GATEWAY-OWN-LABEL) when a unit has it or
none serves HOME, else the name of the unit nodecode wrote that does."
  ;; One look at the own name's file; the unit folder is read only when that
  ;; is not there.
  (or (and *unit-directory*
           (not (probe-file (unit-file-path own)))
           (when-let (serving (first (gateway-units-serving home)))
             (pathname-name serving)))
      own))

(defun unit-serves-store-p (path home)
  "Whether the systemd unit at PATH runs the process that owns HOME's store
now. NIL where the service manager cannot say."
  (and (eq *service-manager* :systemd)
       (multiple-value-bind (verdict owner) (ignore-errors (store-death-verdict (home-store-path home)))
         (and (eq verdict :live) owner
              (multiple-value-bind (out err status)
                  (ignore-errors (manager-run "show" (file-namestring path) "--property=MainPID"))
                (declare (ignore err))
                (let ((at (and (eql status 0) (stringp out) (search "MainPID=" out))))
                  (and at (eql owner (parse-integer out :start (+ at 8) :junk-allowed t)))))))))

(defun gateway-unit-duplicates ()
  "Every home two or more gateway units nodecode wrote serve: a list (HOME
KEPT . EXTRA) each, HOME its true name, KEPT the unit to keep -- the one
whose process serves the store now, else the one this process names the
home by (GATEWAY-OWN-LABEL), else the first -- and EXTRA the rest."
  ;; Asks the service manager, once a unit, only when a home has two.
  (let ((by-home (make-hash-table :test #'equal)))
    (dolist (path (gateway-unit-files))
      (when-let (home (unit-file-home path))
        (setf (gethash home by-home) (append (gethash home by-home) (list path)))))
    (sort (loop for home being the hash-keys of by-home using (hash-value paths)
                when (rest paths)
                  collect (let* ((name (file-namestring (unit-file-path (gateway-own-label (pathname home)))))
                                 (kept (or (find-if (lambda (path) (unit-serves-store-p path home)) paths)
                                           (find name paths :key #'file-namestring :test #'string=)
                                           (first paths))))
                            (list* home kept (remove kept paths))))
          #'string< :key #'first)))

(defun unit-removal-command (path &aux (label (pathname-name path)))
  "The command that stops the gateway unit at PATH and takes it away."
  (format nil "~a && rm ~a"
          (if (eq *service-manager* :launchd)
              (format nil "launchctl bootout ~a" (launchd-target label))
              (format nil "systemctl --user disable --now ~a.service" label))
          (uiop:escape-sh-token (uiop:native-namestring path))))

(defun gateway-unit-text (&key (manager *service-manager*) (home *home*) (program (organism-binary))
                            (launcher (and (eq manager :launchd) (gateway-launcher-path)))
                            (path (uiop:getenv "PATH")) (user-home (uiop:getenv "HOME"))
                            (working-directory (uiop:getcwd))
                            ;; the XDG folders set here: a gateway resolving others reads another machine
                            (carried (loop for name in '("XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME")
                                           for value = (uiop:getenv name)
                                           when (and (stringp value) (plusp (length value)))
                                             collect (cons name value))))
  "The unit that serves a home's gateway under MANAGER: a systemd user unit
or a LaunchAgent -- whose LAUNCHER runs PROGRAM -- handed the rest of these
keys, HOME as a folder."
  (let* ((home (uiop:ensure-directory-pathname home))
         (account (account-home))
         ;; USER-HOME is the gateway's HOME only when it is not the account's own
         (own-home (and user-home (plusp (length user-home))
                        (or (null account)
                            (not (equal (namestring (uiop:ensure-directory-pathname user-home))
                                        (namestring account))))))
         (variables (append (list (cons "NODECODE_HOME"
                                        (string-right-trim "/\\" (uiop:native-namestring home))))
                            (when own-home (list (cons "HOME" user-home)))
                            carried
                            (when (and path (plusp (length path))) (list (cons "PATH" path))))))
    (ecase manager
      ;; restarts 5 s after any exit, stops on SIGTERM; notify + watchdog show a stopped image
      (:systemd
       (with-output-to-string (out)
         (format out "# ~a~a~%" +unit-marker+ (uiop:native-namestring home))
         ;; No After=network-online.target: a user manager has no such unit, and
         ;; the line would order nothing. What talks out tries again.
         (format out "[Unit]~%Description=nodecode gateway (~a)~%" (profile-name home))
         (format out "StartLimitIntervalSec=0~%~%")
         (format out "[Service]~%Type=notify~%NotifyAccess=main~%WatchdogSec=~ds~%"
                 *gateway-watchdog-seconds*)
         (format out "ExecStart=~a gateway run~%" (unit-quoted program :exec t))
         ;; The gateway is a process, not a session: preserve the folder from
         ;; which its unit was written so image-level relative paths agree with
         ;; the shell that enabled it. Session tools still use their durable cwd.
         ;; Bare, `%' doubled: systemd takes the line as the path and never
         ;; unquotes it, so a quoted one is not absolute and the unit would not
         ;; load (bad-setting).
         (format out "WorkingDirectory=~a~%"
                 (ppcre:regex-replace-all "%" (string-right-trim "/\\"
                                                                 (uiop:native-namestring
                                                                  (uiop:ensure-directory-pathname working-directory)))
                                          "%%"))
         (loop for (name . value) in variables
               do (format out "Environment=~a~%"
                          (unit-quoted (format nil "~a=~a" name value))))
         (format out "Restart=always~%RestartSec=5~%KillMode=mixed~%")
         (format out "TimeoutStopSec=60~%~%")
         (format out "[Install]~%WantedBy=default.target~%")))
      ;; no journal under launchd: stdout and stderr go to GATEWAY-LOG-PATH; the marker is line two
      (:launchd
       (let ((log (uiop:native-namestring (gateway-log-path home)))
             (working (string-right-trim
                       "/" (uiop:native-namestring
                            (or (and own-home (uiop:ensure-directory-pathname user-home))
                                account
                                home)))))
         (with-output-to-string (out)
           (format out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
           ;; an XML comment may not hold two dashes in a row
           (format out "<!-- ~a~a -->~%" +unit-marker+
                   (ppcre:regex-replace-all "-(?=-)" (uiop:native-namestring home) "- "))
           (format out "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" ~
                        \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">~%")
           (format out "<plist version=\"1.0\">~%<dict>~%")
           (format out "  <key>Label</key><string>~a</string>~%" (xml-escaped (gateway-label home)))
           (format out "  <key>ProgramArguments</key>~%  <array>~%")
           (dolist (word (list (uiop:native-namestring launcher) program "gateway" "run"))
             (format out "    <string>~a</string>~%" (xml-escaped word)))
           (format out "  </array>~%")
           (format out "  <key>EnvironmentVariables</key>~%  <dict>~%")
           (loop for (name . value) in variables
                 do (format out "    <key>~a</key><string>~a</string>~%"
                            (xml-escaped name) (xml-escaped value)))
           (format out "  </dict>~%")
           (format out "  <key>WorkingDirectory</key><string>~a</string>~%" (xml-escaped working))
           (format out "  <key>RunAtLoad</key><true/>~%")
           (format out "  <key>KeepAlive</key><true/>~%")
           (format out "  <key>ThrottleInterval</key><integer>5</integer>~%")
           (format out "  <key>ExitTimeOut</key><integer>60</integer>~%")
           (format out "  <key>StandardOutPath</key><string>~a</string>~%" (xml-escaped log))
           (format out "  <key>StandardErrorPath</key><string>~a</string>~%" (xml-escaped log))
           (format out "</dict>~%</plist>~%")))))))

;;; --- the state -----------------------------------------------------------------

(define-record (gateway-state (:copier nil) (:export :predicate :readers))
  "One home's gateway as the operating system and the store report it,
read in one pass by GATEWAY-STATE."
  ;; SUPPORTED is NIL where there is no background gateway, REASON saying why.
  ;; UNIT is its name and FILE its unit file; INSTALLED :OURS, :FOREIGN (a
  ;; unit of that name nodecode did not write) or NIL. ENABLED: it starts at
  ;; login. ACTIVE and SUB are systemd's ActiveState and SubState ("active"
  ;; "running", "activating" "auto-restart", "failed" ...), RESULT its last
  ;; Result unless that was success, MAIN-PID the unit's process. OWNER-PID
  ;; and OWNER-PORT name the store's live owner: the gateway itself, or the
  ;; shell it waits on. LINGER: T when systemd keeps this account's units
  ;; running with no session open, NIL when it does not, :UNKNOWN. OFF: the
  ;; operator turned it off, and no cell turns it back on.
  (unit "" :type string)
  (file nil)
  (supported nil)
  (reason nil)
  (installed nil)
  (enabled nil)
  (active nil)
  (sub nil)
  (result nil)
  (main-pid nil)
  (owner-pid nil)
  (owner-port nil)
  (linger :unknown)
  (off nil))

(access (state gateway-state))

(defun linger-state (&optional (user (account-name)))
  "T when logind keeps USER's systemd instance running with no session
open — its units start at boot and outlive a logout — NIL when it does not,
:UNKNOWN when loginctl cannot say."
  ;; NIL under launchd, which has no linger: an agent lives in the login
  ;; session.
  (cond ((not (eq *service-manager* :systemd)) nil)
        ((null user) :unknown)
        (t (multiple-value-bind (out err status)
               (unit-run "loginctl" "show-user" user "--property=Linger")
             (cond ((eql status 0) (and (search "Linger=yes" out) t))
                   ;; No session and no linger: logind holds no record of USER.
                   ((search "not logged in or lingering" (or err "")) nil)
                   (t :unknown))))))

(defun gateway-off-path (&optional (home *home*))
  (merge-pathnames "gateway-off" (uiop:ensure-directory-pathname home)))

(defvar *temporary-roots* :system
  "The folders the system clears, whose homes no gateway comes up in at
login: :SYSTEM for the temporary directory and, off Windows, /tmp; a list
of folders otherwise. A test binds its own.")

(defun temporary-home-p (&optional (home *home*))
  "Whether HOME sits under a folder the system clears (*TEMPORARY-ROOTS*)."
  ;; A unit written for such a home outlives it: a test's or a script's
  ;; scratch home is gone at its cleanup or the next boot, and its unit goes
  ;; on starting a gateway on the binary `current' names, at every login,
  ;; in an empty folder. True names on both sides, so a link either way
  ;; counts.
  (flet ((real (path) (namestring (or (ignore-errors (truename path)) path))))
    (let ((home (real (uiop:ensure-directory-pathname home))))
      (some (lambda (root)
              (uiop:string-prefix-p (real (uiop:ensure-directory-pathname root)) home))
            (if (eq *temporary-roots* :system)
                (list* (uiop:temporary-directory) #-win32 '("/tmp/") #+win32 '())
                *temporary-roots*)))))

(defun gateway-off-p (&optional (home *home*))
  "Whether the operator turned HOME's gateway off: then nothing but the
operator's own `gateway on' turns it back on."
  (and (probe-file (gateway-off-path home)) t))

(defun gateway-state (&key (home *home*))
  "HOME's gateway as the operating system and the store report it now: a
GATEWAY-STATE."
  ;; Two questions to systemd (the unit, the linger) and one look at the
  ;; store's ownership lock; nothing is cached, so a state cannot go stale
  ;; against what it describes. Where there is no unit folder, nothing is
  ;; asked at all.
  (let* ((home (uiop:ensure-directory-pathname home))
         (file (gateway-unit-path home))
         (unit (gateway-unit-name home))
         (state (make-gateway-state :unit unit :file file
                                    :installed (unit-file-owner file)
                                    :off (gateway-off-p home))))
    (multiple-value-bind (verdict pid port) (store-death-verdict (home-store-path home))
      (when (eq verdict :live) (setf state.owner-pid pid state.owner-port port)))
    (unless file (setf state.reason (platform-reason)) (return-from gateway-state state))
    (multiple-value-bind (out err status argv)
        (if (eq *service-manager* :launchd)
            (manager-run "print" (launchd-target unit))
            (manager-run "show" unit
                         "--property=LoadState,ActiveState,SubState,UnitFileState,Result,MainPID"))
      (flet ((field (name)
               (loop for line in (lines (and (eql status 0) out))
                     for equals = (position #\= line)
                     when (and equals
                               (string= name (string-trim '(#\Space #\Tab) (subseq line 0 equals))))
                       return (string-trim '(#\Space #\Tab #\Return) (subseq line (1+ equals))))))
        (setf state.reason
              (cond
                ((eq status :missing)
                 (format nil "~a is not installed, and the background gateway runs under ~(~a~)"
                         (first argv) *service-manager*))
                ((and (eq *service-manager* :systemd) (not (eql status 0)))
                 (format nil "systemd has no user manager to talk to here~@[ (~a)~] — ~
                              `sudo loginctl enable-linger ~a' starts one"
                         (answer-line err out) (account-name)))
                ;; A systemctl that answers nothing leaves the state unsupported, and
                ;; says so and what runs the organism instead: a refusal quoted this
                ;; reason, and it read `NIL' (xa-204).
                ((and (eq *service-manager* :systemd) (not (find #\= out)))
                 (format nil "`~{~a~^ ~}' printed nothing readable — `nodecode serve' in a ~
                              terminal you keep open runs the organism headless"
                         (subseq argv 0 (min 4 (length argv)))))))
        (unless state.reason
          (multiple-value-bind (enabled active sub result pid)
              (if (eq *service-manager* :systemd)
                  (values (equal (field "UnitFileState") "enabled") (field "ActiveState")
                          (field "SubState")
                          (and (not (member (field "Result") '("" "success") :test #'equal))
                               (field "Result"))
                          (field "MainPID"))
                  ;; launchd's answer in systemd's words; an agent launchd holds starts at login
                  (let ((word (field "state"))
                        (exit (field "last exit code")))
                    (values (eql status 0)
                            (if (equal word "running") "active" "inactive")
                            (cond ((equal word "running") "running")
                                  ((member word '("waiting" "spawn scheduled") :test #'equal)
                                   "auto-restart")
                                  (t "dead"))
                            (and exit (not (member exit '("0" "(never exited)") :test #'equal))
                                 (format nil "exit ~a" exit))
                            ;; the agent's process is the launcher; the organism is its child
                            (launcher-child (field "pid")))))
            (let ((pid (ignore-errors (parse-integer (or pid "")))))
              (setf state.supported t
                    state.enabled enabled
                    state.active active
                    state.sub sub
                    state.result result
                    state.main-pid (and pid (plusp pid) pid)
                    state.linger (linger-state)))))))
    state))

(defun gateway-installed-p (&optional (home *home*))
  "Whether HOME's gateway is on: its unit file is there, and nodecode wrote
it."
  ;; One file read -- the unit folder's too when the home's own name has no
  ;; file (GATEWAY-LABEL) -- and no process spawned: what a reconnect lap, a
  ;; launch and a cell's start ask.
  (eq :ours (unit-file-owner (gateway-unit-path home))))

(defun gateway-process-p (pid)
  "Whether PID is a gateway — a nodecode serving `gateway run' — as far as
its command line says."
  ;; A warm launch image keeps the argv it was exec'd with, so a gateway reads
  ;; as one from its first instant.
  (and pid
       (search '("gateway" "run")
               ;; the command line off /proc on Linux, off ps on macOS (split on spaces)
               #+linux
               (let ((text (ignore-errors (uiop:read-file-string (format nil "/proc/~d/cmdline" pid)
                                                                 :external-format :latin-1))))
                 (and text (remove "" (split-nul text) :test #'string=)))
               #+darwin
               (multiple-value-bind (out err status)
                   (ignore-errors (run-bounded (list "ps" "-o" "command=" "-p" (princ-to-string pid))
                                               :seconds 5))
                 (declare (ignore err))
                 (and (eql status 0) (split-words out)))
               #-(or linux darwin) nil
               :test #'equal)
       t))

;;; --- on, off, restart ------------------------------------------------------------

(defun write-unit (file text)
  "FILE holding TEXT, written through a hidden sibling and a rename, and
only when it holds anything else. => T when it was written."
  (unless (equal text (and (probe-file file) (ignore-errors (uiop:read-file-string file))))
    (write-file-atomically file text)
    t))

(defun require-supported (state) (unless state.supported (gateway-fail "~a" state.reason)) state)

(defun gateway-on (&key (home *home*) auto (program (organism-binary)))
  "Run HOME's gateway in the background from now on and at every login:
write its unit — again whenever what it would run has changed — have the
service manager run it, and on Linux ask logind to keep it
running after a logout."
  ;; It starts at once; while a shell owns the store it waits, and takes the
  ;; organism over when that shell closes. AUTO is a cell asking
  ;; (KEEP-RUNNING), not the operator: it answers :OFF, touching nothing, once
  ;; the operator turned the gateway off, and leaves a gateway that is already
  ;; on as it is. PROGRAM is what the unit runs, ORGANISM-BINARY's answer by
  ;; default. => the GATEWAY-STATE after, or :OFF. A machine with no
  ;; background gateway, a unit of that name nodecode did not write, no binary
  ;; to run and a refusal from the service manager are each a GATEWAY-ERROR
  ;; saying so.
  (let ((state (require-supported (gateway-state :home home))))
    (when (eq state.installed :foreign)
      (gateway-fail "~a is a unit nodecode did not write, so it stays as it is; ~
                     move it aside, then turn the gateway on again"
                    (uiop:native-namestring state.file)))
    (cond
      ((and auto (gateway-state-off state)) :off)
      ((and auto (eq (gateway-state-installed state) :ours)) state)
      (t
       (unless program
         (gateway-fail "this image is not the nodecode executable, so there is no binary ~
                        for the unit to run"))
       (let ((text (gateway-unit-text :home home :program program)))
         (ecase *service-manager*
           (:systemd
            (when (write-unit state.file text)
              (manager-or-fail "daemon-reload"))
            (manager-or-fail "enable" "--now" state.unit))
           (:launchd
            ;; launchd opens its log file but never makes the folder; a loaded agent may be stopped
            (ensure-directories-exist (gateway-log-path home))
            (install-gateway-launcher)
            (let* ((loaded (agent-loaded-p state.unit))
                   (written (write-unit state.file text)))
              (when (and loaded written)
                (boot-out-agent state.unit))
              (manager-run "enable" (launchd-target state.unit))
              (unless (and loaded (not written))
                (manager-or-fail "bootstrap" (launchd-domain) (uiop:native-namestring state.file)))
              (manager-run "kickstart" (launchd-target state.unit))))))
       (ignore-errors (delete-file (gateway-off-path home)))
       ;; logind keeps the account's systemd up with no session: polkit grants most accounts that
       (let ((user (account-name)))
         (when (and (eq *service-manager* :systemd) user (not (eq t (linger-state user))))
           (unit-run "loginctl" "enable-linger" user)))
       (gateway-state :home home)))))

(defun gateway-off (&key (home *home*) (mark t) &aux (unit (gateway-unit-name home))
                                                     (file (gateway-unit-path home)))
  "Stop HOME's gateway and take it out of every login: its unit disabled,
its file removed, then its process stopped — the file first, because a
shell attached to this gateway reads the file to tell a stop from a
restart, and must find it gone by the time the gateway lets the store go
and the shell takes the organism over."
  ;; Under MARK — the operator's off —
  ;; the home keeps a note of it, and no cell turns the gateway back on
  ;; (GATEWAY-ON :AUTO); PROFILE-DELETE passes NIL, the home about to go. A unit
  ;; nodecode did not write is left alone. => the GATEWAY-STATE after.
  (when (eq :ours (unit-file-owner file))
    (ecase *service-manager*
      (:systemd
       (manager-run "disable" unit)
       (delete-file file)
       (manager-or-fail "stop" unit)
       (manager-run "daemon-reload")
       (manager-run "reset-failed" unit))
      (:launchd
       (delete-file file)
       (multiple-value-bind (out err status argv) (manager-run "bootout" (launchd-target unit))
         (unless (or (eql status 0) (not (agent-loaded-p unit)))
           (gateway-fail "~{~a~^ ~} failed~@[: ~a~]" argv (answer-line err out)))))))
  (when mark
    (with-output-file (out (gateway-off-path home))
      (format out "turned off at ~a~%" (iso-now))))
  (gateway-state :home home))

(defun gateway-restart (&key (home *home*) (program (organism-binary)))
  "Restart HOME's gateway on what its unit would run now: the unit
rewritten first when that changed — an update moved the binary, the PATH
moved — so it comes back on this build, with the config as it is now."
  ;; Refused while the gateway is off. => the GATEWAY-STATE after.
  (let ((state (require-supported (gateway-state :home home))))
    (unless (eq state.installed :ours)
      (gateway-fail "the gateway is off; `gateway on' starts it"))
    (let ((text (and program (gateway-unit-text :home home :program program))))
      ;; launchd reads the file once: a changed agent is booted out and bootstrapped again
      (ecase *service-manager*
        (:systemd
         (when (and text (write-unit state.file text))
           (manager-or-fail "daemon-reload"))
         (manager-or-fail "restart" state.unit))
        (:launchd
         (when text (install-gateway-launcher))
         (cond ((and text (write-unit state.file text))
                (boot-out-agent state.unit)
                (manager-or-fail "bootstrap" (launchd-domain) (uiop:native-namestring state.file)))
               (t (manager-or-fail "kickstart" "-k" (launchd-target state.unit)))))))
    (gateway-state :home home)))

(defun gateway-start-argv (&optional (home *home*))
  "The command that asks the service manager to start HOME's gateway —
what GATEWAY-START runs, for a shell's log line."
  (ecase *service-manager*
    (:systemd (list "systemctl" "--user" "start" (gateway-unit-name home)))
    (:launchd (list "launchctl" "kickstart" (launchd-target (gateway-unit-name home))))))

(defun gateway-start (&optional (home *home*))
  "Ask the service manager to start HOME's gateway — nothing, while it
runs."
  ;; The reconnect ladder's rung for a store the gateway serves: the operating
  ;; system brings the organism back, never a process detached beside it that
  ;; `gateway off' could not stop. => T when the manager took the job.
  (eql 0 (nth-value 2 (apply #'unit-run (gateway-start-argv home)))))

(defun gateway-log-argv (&key (home *home*) (lines 60) follow)
  "The command that reads HOME's gateway log — journalctl for a systemd
unit, tail for a launchd agent's file: the last LINES, and under FOLLOW
everything after them as it comes."
  (ecase *service-manager*
    (:systemd
     (append (list "journalctl" "--user" "--unit" (gateway-unit-name home)
                   "--lines" (princ-to-string lines) "--output" "short-iso")
             (if follow (list "--follow") (list "--no-pager"))))
    (:launchd
     (append (list "tail" "-n" (princ-to-string lines))
             (when follow (list "-f"))
             (list (uiop:native-namestring (gateway-log-path home)))))))

(defun gateway-log-text (&key (home *home*) (lines 60))
  "The last LINES of HOME's gateway log, as journalctl prints them — or as
its file holds them under launchd."
  (unless (gateway-unit-path home)
    (gateway-fail "~a" (platform-reason)))
  (let* ((argv (gateway-log-argv :home home :lines lines))
         (text (and (or (not (eq *service-manager* :launchd))
                        (probe-file (gateway-log-path home)))
                    (multiple-value-bind (out err status) (apply #'unit-run argv)
                      (unless (eql status 0)
                        (gateway-fail "~a could not read the gateway's log~@[: ~a~]"
                                      (first argv) (answer-line err out)))
                      (string-right-trim '(#\Newline #\Space) out)))))
    (if (plusp (length text))
        text
        (format nil "~a has written no log yet" (gateway-unit-name home)))))

;;; --- the shells' text --------------------------------------------------------------

(defun gateway-serving-p (state &aux (owner state.owner-pid))
  "Whether STATE's gateway is the store's owner: systemd's main process and
the pid the store's lock names are one."
  (and owner (eql owner state.main-pid)))

(defun gateway-summary (state &key live (command "gateway"))
  "The report's first line, after its label: where STATE's gateway stands,
and the one thing to do about it when there is one."
  (cond
    ((not (gateway-state-supported state)) (format nil "unavailable — ~a" state.reason))
    ((eq (gateway-state-installed state) :foreign)
     (format nil "off — ~a is a unit nodecode did not write, and it is left alone"
             (uiop:native-namestring state.file)))
    ((not (eq (gateway-state-installed state) :ours))
     (format nil "off~:[~; (you turned it off)~] — nothing keeps this organism running once ~
                  its last shell closes; `~a on' runs it in the background, and at every login"
             state.off command))
    ((equal state.active "failed")
     (format nil "on, but it failed~@[ (~a)~] — `~a logs' says why, `~a restart' tries again"
             state.result command command))
    ((equal (gateway-state-sub state) "auto-restart")
     (format nil "on, restarting after it exited~@[ (~a)~] — `~a logs' says why"
             state.result command))
    ((member state.active '("inactive" "deactivating") :test #'equal)
     (format nil "on, but stopped — `~a restart' starts it" command))
    ((gateway-serving-p state)
     (let ((started (getf live :started-at)))
       (format nil "on — serving: pid ~d~@[, port ~d~]~@[, up ~a~]~@[, build ~a~]~@[ (binary ~a)~]"
               state.owner-pid state.owner-port
               (and (integerp started) (duration-text (- (unix-now) started)))
               (getf live :version)
               ;; a hot upgrade moved the definitions past the binary: say both
               (let ((binary (getf live :binary)))
                 (and binary (not (equal binary (getf live :version))) binary)))))
    (state.owner-pid
     (format nil "on — waiting: pid ~d runs the organism now, and the gateway takes it over ~
                  when that closes"
             state.owner-pid))
    (t (format nil "on — starting~@[ (pid ~d)~]" state.main-pid))))

(defun gateway-report (&optional (state (gateway-state)) &key live (command "gateway"))
  "STATE as the text a shell prints: where the gateway stands — serving,
waiting on a shell, starting, stopped, failed, off — whether it starts at
login and outlives a logout, its unit, and, while it serves, the lines the
organism keeps standing (a bot's state among them)."
  ;; LIVE is what the serving organism says of itself, a plist of :VERSION,
  ;; :STARTED-AT (unix seconds) and :NOTICES, or NIL; COMMAND how the calling
  ;; shell spells the command, which every hint quotes.
  (let ((rows (list (list "gateway" (gateway-summary state :live live :command command)))))
    (when (eq state.installed :ours)
      (push (list "login"
                  (cond ((not (gateway-state-enabled state))
                         (format nil "does not start at login — `~a on' makes it" command))
                        ((eq *service-manager* :launchd)
                         (format nil "starts when you log in, and stops when you log out — ~
                                      an agent lives in your login session"))
                        ((eq t (gateway-state-linger state))
                         "starts at every login, and keeps running after you log out")
                        ((null (gateway-state-linger state))
                         (format nil "starts at every login, and stops when you log out — ~
                                      `sudo loginctl enable-linger ~a' keeps it running"
                                 (account-name)))
                        (t "starts at every login")))
            rows)
      (push (list (if (eq *service-manager* :launchd) "agent" "unit")
                  (format nil "~a — `~a logs' reads its log"
                          state.unit command))
            rows))
    (loop for notice in (getf live :notices)
          for label = "standing" then ""
          do (push (list label notice) rows))
    (let ((width (reduce #'max rows :key (lambda (row) (length (first row))))))
      (format nil "~:{~va  ~a~%~}" (mapcar (lambda (row) (cons width row)) (nreverse rows))))))

(defparameter *gateway-verbs*
  (list
   (make-verb "status" "whether it runs, what it serves, whether it starts at login"
              :detail "What the command prints with no verb: serving, waiting for a shell to close, starting, stopped or failed; its pid, port, uptime and build; whether it starts at login and keeps running after a logout; and the lines the organism keeps standing, a bot's state among them.")
   (make-verb "setup" "connect a bot to it: Discord, Telegram, Slack" :arguments "[CHANNEL]"
              :detail "Walks one channel in the terminal, over the organism serving this home, and offers to turn the gateway on when none serves: installs the channel's cell, keeps the pasted bot token in a 0600 file in this home's secrets folder (or names the environment variable that holds it), checks it against the platform, picks the rooms and people it answers from what the token can see, and starts the channel. CHANNEL skips the choice: `setup discord'. Run it again to change one; /settings > Channels and the web page's Control > Channels edit the same section.")
   (make-verb "on" "run it in the background from now on, and at every login"
              :detail "Writes the unit the operating system runs — a systemd user unit on Linux, a LaunchAgent on macOS; nodecode-gateway, nodecode-gateway-NAME for a profile — enables and starts it, and on Linux asks logind to keep it running after you log out. While a shell runs the organism the gateway waits, and takes over when that shell closes; every shell opened after attaches to it. A bot or a cron job turns it on by itself, once, unless you turned it off.")
   (make-verb "off" "stop it, and stop starting it at login"
              :detail "Disables and stops the unit and removes it; nothing turns it back on by itself after that. A shell attached to it takes the organism over, and bots and cron jobs run while a shell is open.")
   (make-verb "restart" "restart it on this build, with the config as it is now")
   (make-verb "logs" "its recent log" :arguments "[--follow]"
              :detail "The last 60 lines of its journal on Linux, of its log file on macOS; --follow, on the command line, keeps printing.")
   (make-verb "run" "the gateway in the foreground: what the unit runs"
              :detail "Serves this home's organism headless until stopped. While another process owns the store it waits, and takes the organism over when that one lets go.")
   (make-verb "help" "this text, or one verb's" :arguments "[VERB]"))
  "The gateway verbs, in help's order: what GATEWAY-TEXT reads, what
its help prints, what a mistyped verb is matched against.")

(defun gateway-text (args &key (command "gateway") live)
  "One gateway request as text: the shells' one entry — `nodecode gateway
...', /gateway — over *GATEWAY-VERBS*; with none, the state."
  ;; ARGS is the words after the command, one string or a list; COMMAND is how
  ;; the calling shell spells the command, which the help, the refusals and
  ;; every hint quote back. LIVE, a function of a GATEWAY-STATE answering what
  ;; the serving organism says of itself (GATEWAY-REPORT's LIVE), is the
  ;; shell's to give: asking the organism is a client's call, made only while
  ;; the gateway serves. `help [VERB]', or --help or -h anywhere, answers help
  ;; instead of acting. => the answer. A refusal is GATEWAY-ERROR;
  ;; GATEWAY-USAGE-ERROR for words the verbs cannot read, pointing at the
  ;; help.
  (with-verbs (args *gateway-verbs* :refusal gateway-usage-error
               :noun "gateway verb" :default "status" :command command
               :usage "usage: ~a [VERB]~%~%The gateway is this home's organism kept running by the operating system: in the background, at every login, after a crash. With no verb, ~a says where it stands.~%~%verbs~%~a")
    (flet ((report (state)
             (gateway-report state
                             :live (and live (gateway-serving-p state)
                                        (ignore-errors (funcall live state)))
                             :command command)))
      (unless (member verb '("logs" "run" "setup") :test #'string=)
        (nothing-more))
      (dispatch verb string=
        ("status" (report (gateway-state)))
        ("on" (format nil "the gateway is on~%~a" (report (gateway-on))))
        ("off" (format nil "the gateway is off~%~a" (report (gateway-off))))
        ("restart" (format nil "the gateway restarted~%~a" (report (gateway-restart))))
        ("logs" (when (member "--follow" words :test #'string=)
                  (usage-fail verb "--follow is the command line's: `nodecode gateway logs --follow'."))
                (nothing-more)
                (gateway-log-text))
        ("run" (usage-fail verb "run is the command line's own: `nodecode gateway run' serves in the foreground."))
        ("setup" (usage-fail verb "setup is the command line's own: `nodecode gateway setup' walks it in a terminal; here, /settings > Channels sets up the same section."))
        (t (error "the gateway verb ~a has no branch in GATEWAY-TEXT" verb))))))
