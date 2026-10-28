;;;; background-test.lisp --- the gateway: this home's organism, kept
;;;; running by the operating system.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hermetic posture leaves no unit folder, so no gate writes a unit
;;;; where the operator's systemd reads or asks the operator's systemd
;;;; anything. WITH-SCRIPTED-SYSTEMD points the unit folder under the test's
;;;; scratch home and hands *UNIT-RUNNER* a FAKE-SYSTEMD: systemctl --user,
;;;; loginctl and journalctl answered from a little state of units and
;;;; linger, every argv recorded. Both variables are SETF, never LET — a
;;;; cell's KEEP-RUNNING asks the operating system on a thread of its own
;;;; — and the posture restores them.

(in-package #:nodecode.test)

(nlk:access (state nlk::gateway-state))

(defstruct fake-unit
  (enabled nil)
  (active "inactive")
  (sub "dead")
  (pid nil))

(defstruct fake-systemd
  (units (make-hash-table :test #'equal))
  (linger nil)
  (linger-allowed t)
  (bus t)
  (calls '())
  (stop-saw-file :unasked))

(nlk:access (fake fake-systemd) (service fake-unit))

(defun fake-systemctl (fake arguments)
  "systemctl --user ARGUMENTS against FAKE: the unit's file is the real one
under *UNIT-DIRECTORY*, its enablement and its process FAKE's."
  (if (not fake.bus)
      (values "" "Failed to connect to bus: No medium found" 1)
      (let* ((verb (first arguments))
             (unit (find-if (lambda (word) (search ".service" word)) (rest arguments)))
             (service (and unit (alexandria:ensure-gethash unit fake.units (make-fake-unit)))))
        (flet ((file-p ()
                 (and (probe-file (merge-pathnames unit nlk:*unit-directory*)) t))
               (run ()
                 (setf service.active "active" service.sub "running" service.pid 4242))
               (halt ()
                 (setf service.active "inactive" service.sub "dead" service.pid nil)))
          (cond
            ((string= verb "show")
             (values (format nil "LoadState=~:[not-found~;loaded~]~%ActiveState=~a~%SubState=~a~%~
                             UnitFileState=~:[disabled~;enabled~]~%Result=success~%MainPID=~d~%"
                             (file-p) service.active service.sub service.enabled (or service.pid 0))
                     "" 0))
            ((member verb '("daemon-reload" "reset-failed") :test #'string=)
             (values "" "" 0))
            ((string= verb "enable")
             (cond ((file-p)
                    (setf service.enabled t)
                    (when (member "--now" arguments :test #'string=) (run))
                    (values "" "Created symlink." 0))
                   (t (values "" (format nil "Failed to enable unit: Unit file ~a does not exist." unit)
                              1))))
            ((string= verb "disable")
             (setf service.enabled nil)
             (values "" "Removed." 0))
            ((string= verb "stop") (setf fake.stop-saw-file (file-p)) (halt) (values "" "" 0))
            ((member verb '("start" "restart") :test #'string=)
             (run)
             (values "" "" 0))
            (t (values "" (format nil "Unknown command verb ~a." verb) 1)))))))

(defun fake-systemd-run (fake argv &aux (program (first argv)) (verb (second argv)))
  "ARGV as FAKE answers it, recorded: systemctl --user, loginctl and journalctl."
  (setf fake.calls (append fake.calls (list argv)))
  (cond ((string= program "systemctl") (fake-systemctl fake (cddr argv)))
        ((and (string= program "loginctl") (string= verb "show-user"))
         (values (format nil "Linger=~:[no~;yes~]~%" fake.linger) "" 0))
        ((and (string= program "loginctl") (string= verb "enable-linger"))
         (if fake.linger-allowed
             (progn (setf fake.linger t)
                    (values "" "" 0))
             (values "" "Could not enable linger: Access denied" 1)))
        ((string= program "loginctl") (values "" "Unknown command verb." 1))
        ((string= program "journalctl")
         (values (format nil "2026-09-11T21:00:00+0000 box nodecode[4242]: serving store~%") "" 0))
        (t (values "" (format nil "Couldn't find executable ~s" program) :missing))))

(defmacro with-scripted-systemd ((fake &rest options) &body body)
  "BODY with the unit folder under this test's scratch home and
*UNIT-RUNNER* answering from FAKE, a FAKE-SYSTEMD made from OPTIONS."
  `(let ((,fake (make-fake-systemd ,@options)))
     (declare (ignorable ,fake))
     (setf nlk:*service-manager* :systemd
           nlk:*unit-directory* (nlk:home "systemd/user/")
           nlk:*unit-runner* (lambda (argv) (fake-systemd-run ,fake argv)))
     ,@body))

(defun unit-verbs (fake)
  "The systemctl verbs FAKE was handed, in order."
  (loop for argv in (fake-systemd-calls fake)
        when (equal (first argv) "systemctl") collect (third argv)))

(defparameter +test-binary+ "/opt/nodecode/current/nodecode")

;;; --- the unit ---------------------------------------------------------------

(deftest gateway-unit-name-follows-the-home ()
  (is (equal "nodecode-gateway.service" (nlk:gateway-unit-name (nlk:default-home) :systemd)))
  (is (equal "nodecode-gateway-coder.service" (nlk:gateway-unit-name (nlk:profile-home "coder") :systemd)))
  (let ((one (nlk:gateway-unit-name #p"/srv/a/agent/" :systemd))
        (two (nlk:gateway-unit-name #p"/srv/b/agent/" :systemd)))
    (is (uiop:string-prefix-p "nodecode-gateway-" one))
    (is (= (length "nodecode-gateway-12345678.service") (length one)))
    (is (not (equal one two)) "two folders called the same are two organisms")))

(deftest a-borrowed-home-never-takes-the-accounts-gateway ()
  ;; Sai's Mac, 2026-10-21: a proof ran under a scratch $HOME whose
  ;; ~/.nodecode/ was its default home; its first launch bootstrapped
  ;; nodecode-gateway into the account's one launchd domain and the account's
  ;; own gateway exited 143. The service manager is the account's, whatever
  ;; $HOME a process was handed: only the account's own homes are named
  ;; plainly or come on by themselves.
  (let ((real-home (sb-posix:getenv "HOME"))
        (scratch (namestring (temp-path "home"))))
    (is (nlk::own-account-home-p) "the suite runs under the account's own $HOME")
    (is (equal "nodecode-gateway" (nlk:gateway-label (nlk:default-home))) "which keeps its name")
    (nlk:with-cleanup ((sb-posix:setenv "HOME" real-home 1)
                       (ignore-errors (uiop:delete-directory-tree (pathname scratch) :validate t)))
      (ensure-directories-exist scratch)
      (sb-posix:setenv "HOME" (string-right-trim "/" scratch) 1)
      (is (not (nlk::own-account-home-p)) "a scratch $HOME is not the account's")
      (let ((label (nlk:gateway-label (nlk:default-home))))
        (is (search scratch (namestring (nlk:default-home))) "the default home follows $HOME")
        (is (not (equal "nodecode-gateway" label)) "its gateway is not the account's")
        (is (= (length "nodecode-gateway-12345678") (length label)) "it is named by its path"))
      (is (not (equal "nodecode-gateway-coder" (nlk:gateway-label (nlk:profile-home "coder")))) "nor is a profile under it the account's profile")
      (is (not (nlk:gateway-by-default-p (nlk:default-home))) "and it is the operator's to turn on"))
    (is (equal "nodecode-gateway" (nlk:gateway-label (nlk:default-home))) "the account's name is back with its $HOME")))

(deftest gateway-unit-text-pins-the-home-and-quotes-for-systemd ()
  (let ((text (nlk:gateway-unit-text :manager :systemd :home #p"/tmp/h/"
                                     :program "/opt/node code/100%/nodecode$x"
                                     :path "/usr/bin:/bin"
                                     :user-home (namestring (nlk::account-home))
                                     :working-directory "/workspace/node code/100%/")))
    (is (uiop:string-prefix-p "# nodecode gateway: /tmp/h/" text))
    (is-carrying text "ExecStart=\"/opt/node code/100%%/nodecode$$x\" gateway run"
      ("Environment=\"NODECODE_HOME=/tmp/h\"" "the home pinned")
      ("Environment=\"PATH=/usr/bin:/bin\"" "the PATH of whoever turned it on")
      ("WorkingDirectory=/workspace/node code/100%%" "the enabling shell's folder is preserved, bare: systemd never unquotes it")
      (:absent "Environment=\"HOME=" "the account's own home is not carried") "Type=notify"
      "NotifyAccess=main" "WatchdogSec=30s" "Restart=always" "WantedBy=default.target")
    ;; A user manager has no network-online.target: the line ordered nothing.
    (is (not (search "network-online" text))))
  (is-carrying (text (nlk:gateway-unit-text :manager :systemd :home #p"/tmp/h/" :program "/n/nodecode" :path ""
                                            :user-home "/scratch/h"
                                            :carried '(("XDG_CONFIG_HOME" . "/scratch/h/.config"))))
    ("Environment=\"HOME=/scratch/h\"" "a HOME that is not the account's rides along")
    ("Environment=\"XDG_CONFIG_HOME=/scratch/h/.config\""
     "and so does the XDG folder the shell that turned it on had: a gateway that ~
         resolved another would read another machine — its own unit folder somewhere ~
         else — and write a second unit there")))

#+linux
(deftest systemd-notify-sends-a-datagram-to-the-unit-socket ()
  (let* ((path (format nil "/tmp/nodecode-notify-~d.sock" (sb-posix:getpid)))
         (saved (sb-posix:getenv "NOTIFY_SOCKET"))
         (socket (make-instance 'sb-bsd-sockets:local-socket :type :datagram)))
    (nlk:with-cleanup ((sb-bsd-sockets:socket-close socket) (ignore-errors (delete-file path))
                       (if saved
                           (sb-posix:setenv "NOTIFY_SOCKET" saved 1)
                           (sb-posix:unsetenv "NOTIFY_SOCKET")))
      (ignore-errors (delete-file path))
      (sb-bsd-sockets:socket-bind socket path)
      (sb-posix:setenv "NOTIFY_SOCKET" path 1)
      (is (nlk:systemd-notify "READY=1") "notify accepts the socket")
      (multiple-value-bind (bytes length)
          (sb-bsd-sockets:socket-receive socket nil 128)
        (is (= 7 length) "the complete payload arrives")
        (is (equal "READY=1" (subseq bytes 0 length)))))))

;;; --- on, off, restart -----------------------------------------------------

(deftest gateway-on-writes-its-unit-enables-it-and-asks-for-linger (with-scripted-systemd (fake))
  (let* ((unit (nlk:gateway-unit-name))
         (state (nlk:gateway-on :program +test-binary+)))
    (is-present (state state) "the state after"
      (is-shape state (.installed eq :ours "the unit is nodecode's") (.active "active" "it runs")
        (.enabled is "it starts at login") (.linger eq t "and outlives a logout")))
    (let ((text (uiop:read-file-string (nlk:gateway-unit-path))))
      (is (uiop:string-prefix-p "# nodecode gateway: " text))
      (is (search (format nil "ExecStart=\"~a\" gateway run" +test-binary+) text)))
    (flet ((called (&rest argv) (member argv (fake-systemd-calls fake) :test #'equal)))
      (is (called "systemctl" "--user" "daemon-reload"))
      (is (called "systemctl" "--user" "enable" "--now" unit))
      (is (called "loginctl" "enable-linger" (nlk::account-name))))
    (setf (fake-systemd-calls fake) '())
    (nlk:gateway-on :program +test-binary+)
    (is (not (member "daemon-reload" (unit-verbs fake) :test #'equal)))))

(deftest gateway-on-leaves-a-unit-it-did-not-write-alone (with-scripted-systemd (fake))
  ;; An older build left a hand-made nodecode-gateway.service on the
  ;; operator's machine: the name is taken, and the file is theirs.
  (let ((path (nlk:gateway-unit-path)))
    (write-temp-file path (format nil "[Unit]~%Description=Nodecode Gateway~%"))
    (is (search "did not write" (refusal-text nlk:gateway-error (nlk:gateway-on :program +test-binary+))))
    (is (search "Description=Nodecode Gateway" (uiop:read-file-string path)))
    (is (not (nlk:gateway-installed-p)) "and it does not read as the gateway")
    (is (eq :foreign (nlk:gateway-state-installed (nlk:gateway-state))))
    (is (search "did not write" (nlk:gateway-report (nlk:gateway-state))))
    (nlk:gateway-off)
    (is (probe-file path) "off leaves it too")
    (is (not (member "disable" (unit-verbs fake) :test #'equal)))))

(deftest gateway-off-goes-before-its-process-and-an-cell-never-undoes-it ()
  (with-scripted-systemd (fake)
    (nlk:gateway-on :program +test-binary+)
    (setf (fake-systemd-calls fake) '())
    (let ((state (nlk:gateway-off)))
      (is (null state.installed) "the unit is gone")
      (is (nlk:gateway-state-off state) "and the home remembers the operator said off"))
    (is (null (probe-file (nlk:gateway-unit-path))))
    (is (eq nil fake.stop-saw-file))
    (let ((verbs (unit-verbs fake)))
      (is (< (position "disable" verbs :test #'equal) (position "stop" verbs :test #'equal))))
    (setf (fake-systemd-calls fake) '())
    (is (eq :off (nlk:gateway-on :auto t :program +test-binary+)))
    (is (not (member "enable" (unit-verbs fake) :test #'equal)))
    (is (nlk:gateway-state-p (nlk:gateway-on :program +test-binary+)) "the operator's on does")
    (is (not (nlk:gateway-off-p)) "and clears the note")
    (setf (fake-systemd-calls fake) '())
    (is (nlk:gateway-state-p (nlk:gateway-on :auto t :program +test-binary+)))
    (is (not (member "enable" (unit-verbs fake) :test #'equal)))))

(deftest gateway-restart-moves-a-changed-unit-first (with-scripted-systemd (fake))
  (is (search "is off" (refusal-text nlk:gateway-error (nlk:gateway-restart :program "/a/nodecode"))))
  (nlk:gateway-on :program "/a/nodecode")
  (setf (fake-systemd-calls fake) '())
  (nlk:gateway-restart :program "/b/nodecode")
  (is (search "ExecStart=\"/b/nodecode\" gateway run" (uiop:read-file-string (nlk:gateway-unit-path))))
  (let ((verbs (unit-verbs fake)))
    (is (member "daemon-reload" verbs :test #'equal))
    (is (member "restart" verbs :test #'equal))))

(deftest gateway-linger-refused-names-the-sudo-that-grants-it ()
  (with-scripted-systemd (fake :linger-allowed nil)
    (let ((state (nlk:gateway-on :program +test-binary+)))
      (is (null state.linger) "logind said no")
      (is (search (format nil "sudo loginctl enable-linger ~a" (nlk::account-name))
                  (nlk:gateway-report state))))))

(deftest gateway-installed-p-reads-one-file-and-asks-nobody (with-scripted-systemd (fake))
  (is (not (nlk:gateway-installed-p)))
  (write-temp-file (nlk:gateway-unit-path) "[Unit]")
  (is (not (nlk:gateway-installed-p)) "a unit nodecode did not write is not the gateway")
  (write-temp-file (nlk:gateway-unit-path) (format nil "# nodecode gateway: /x/~%[Unit]~%"))
  (is (nlk:gateway-installed-p))
  (is (null (fake-systemd-calls fake)) "no process was asked"))

;;; --- the state and its text -----------------------------------------------

(deftest gateway-state-says-why-a-machine-has-none ()
  (let ((state (nlk:gateway-state)))
    (is (not state.supported) "the posture's machine has no unit folder")
    (is (search "no background gateway" state.reason))
    (is (search "unavailable" (nlk:gateway-report state))))
  (with-scripted-systemd (fake :bus nil)
    (let ((state (nlk:gateway-state)))
      (is (not state.supported))
      (is-carrying state.reason "no user manager" "enable-linger")
      (is (search "no user manager"
                  (refusal-text nlk:gateway-error (nlk:gateway-on :program +test-binary+)))))))

(deftest gateway-state-says-why-when-systemctl-answers-nothing (with-scripted-systemd (fake))
  ;; A systemctl that exits 0 and prints nothing -- a stub, a broken install --
  ;; left the state unsupported with no reason, and a cell's keep-running
  ;; refusal ended `... after this shell closes — NIL' (xa-204).
  (setf nlk:*unit-runner* (lambda (argv) (declare (ignore argv)) (values "" "" 0)))
  (let ((state (nlk:gateway-state)))
    (is (not state.supported))
    (is-carrying (text state.reason) "`systemctl --user show " "printed nothing readable"
      ;; and what to do instead, last
      "`nodecode serve'"))
  (is-carrying (text (refusal-text nlk:gateway-error (nlk:gateway-on :program +test-binary+)))
    "printed nothing readable" "`nodecode serve'" (:absent "NIL")))

(deftest gateway-report-says-where-it-stands ()
  (labels ((state (&rest fields)
             (apply #'nlk::make-gateway-state :unit "nodecode-gateway.service" :supported t fields))
           (says (text needle) (and (search needle text) t))
           (reports (needle &rest fields)
             (says (nlk:gateway-report (apply #'state fields)) needle)))
    (let ((serving (nlk:gateway-report
                    (state :installed :ours :enabled t :active "active" :sub "running"
                           :main-pid 42 :owner-pid 42 :owner-port 9100 :linger t)
                    :live (list :version "0.0.1+abc" :started-at (- (nlk::unix-now) 7260)
                                :notices '("discord: running, connected")))))
      (is (says serving "on — serving: pid 42, port 9100, up 2h 1m, build 0.0.1+abc"))
      (is (says serving "starts at every login, and keeps running after you log out"))
      (is (says serving "standing  discord: running, connected") "the board, while it serves"))
    (is (reports "waiting: pid 7 runs the organism now"
                 :installed :ours :enabled t :active "active" :main-pid 42 :owner-pid 7))
    (is (reports "on — starting (pid 42)" :installed :ours :enabled t :active "active" :main-pid 42))
    (is (says (nlk:gateway-report (state :installed :ours :enabled t :active "failed" :result "exit-code")
                                  :command "/gateway")
              "on, but it failed (exit-code) — `/gateway logs' says why"))
    (is (reports "restarting after it exited"
                 :installed :ours :enabled t :active "activating" :sub "auto-restart"))
    (is (reports "does not start at login" :installed :ours :enabled nil :active "active"))
    (is (reports "off — nothing keeps this organism running"))
    (is (reports "(you turned it off)" :off t))))

(deftest gateway-text-reads-its-verbs-and-refuses-the-rest ()
  (let ((help (nlk:gateway-text "help" :command "nodecode gateway")))
    (is (search "usage: nodecode gateway [VERB]" help))
    (dolist (verb '("status" "on" "off" "restart" "logs" "run"))
      (is (search (format nil "  ~a" verb) help) (format nil "help lists ~a" verb))))
  (is (search "usage: /gateway on" (nlk:gateway-text "help on" :command "/gateway")))
  (is (search "usage: /gateway logs [--follow]" (nlk:gateway-text '("logs" "--help") :command "/gateway")))
  (flet ((refusal (words)
           (refusal-text nlk:gateway-usage-error (nlk:gateway-text words :command "/gateway"))))
    (is (search "Did you mean \"on\"" (refusal "onn")) "a typo names the nearest verb")
    (is (search "does not take" (refusal "on now")))
    (is (search "--follow is the command line's" (refusal "logs --follow")))
    (is (search "run is the command line's own" (refusal "run"))))
  (is (search "unavailable" (nlk:gateway-text "" :command "/gateway")) "with no verb, the state")
  (with-scripted-systemd (fake)
    (let ((text (with-stubbed-fdefinition (nlk:organism-binary () +test-binary+)
                  (nlk:gateway-text "on" :command "/gateway"
                                    :live (lambda (state) (declare (ignore state))
                                            (list :version "v9" :notices '("telegram: running")))))))
      (is (uiop:string-prefix-p "the gateway is on" text))
      (is (search "on — starting (pid 4242)" text)))
    (is (search "serving store" (nlk:gateway-text "logs" :command "/gateway")) "the journal's lines")
    (is (uiop:string-prefix-p "the gateway is off" (nlk:gateway-text "off" :command "/gateway")))))

;;; --- beside the gateway: the binary it runs, the profile it serves ----------

#-win32
(deftest organism-binary-prefers-the-installers-current-link ()
  ;; install.sh lays a release out as <root>/<version>/nodecode beside a
  ;; <root>/current link: a unit or a wrapper that names the link runs the
  ;; next update's build on its next start.
  (let* ((root (nlk:home "install/"))
         (binary (merge-pathnames "v1/nodecode" root))
         (current (uiop:native-namestring (merge-pathnames "current/nodecode" root)))
         (loose (merge-pathnames "elsewhere/nodecode" (nlk:home "loose/"))))
    (flet ((from (origin) (let ((nlk::*image-origin* origin)) (nlk:organism-binary))))
      (write-temp-file binary "#!/bin/sh")
      (write-temp-file (merge-pathnames "v2/nodecode" root) "#!/bin/sh")
      (nlk:make-symlink (merge-pathnames "v1/" root) (merge-pathnames "current" root))
      (is (equal current (from binary)))
      ;; The link has moved on to v1 while this process still runs v2 -- an
      ;; update's restart rewrites the unit from inside the build it is
      ;; leaving -- and the answer is still the link: the unit must not pin
      ;; the folder being left.
      (is (equal current (from (merge-pathnames "v2/nodecode" root))))
      ;; No `current' beside the folder: not the installer's layout, the binary itself.
      (write-temp-file loose "#!/bin/sh")
      (is (equal (uiop:native-namestring loose) (from loose)))
      (is (null (from (merge-pathnames "v1/sbcl" root))) "not a nodecode executable, no binary"))))

#+linux
(deftest gateway-process-p-reads-the-command-line ()
  (is (not (nlk:gateway-process-p (sb-posix:getpid))) "this image is not a gateway")
  (let ((process (sb-ext:run-program "/bin/sh" '("-c" "sleep 30; true" "gateway" "run")
                                     :wait nil)))
    (nlk:with-cleanup ((ignore-errors (sb-ext:process-kill process 9))
                       (ignore-errors (sb-ext:process-wait process)))
      ;; RUN-PROGRAM answers once it has forked, before the child's exec lands:
      ;; until then /proc/PID/cmdline is the parent's argv or empty. Ask until
      ;; the exec is in, as a gateway's own pid is only ever read long after.
      (is (nlk:wait-until (lambda () (nlk:gateway-process-p (sb-ext:process-pid process)))
                          (nlk:deadline-after 5))))))

(deftest profile-delete-takes-its-gateway-with-it (with-scripted-systemd (fake))
  (nlk:profile-create "coder")
  (let* ((home (nlk:profile-home "coder"))
         (path (nlk:gateway-unit-path home)))
    (nlk:gateway-on :home home :program +test-binary+)
    (is (probe-file path) "the profile's own unit")
    (is (equal "nodecode-gateway-coder.service" (file-namestring path)))
    (nlk:profile-delete "coder")
    (is (null (probe-file path)) "the unit went with the home")
    (is (member "disable" (unit-verbs fake) :test #'equal))
    (is (not (uiop:directory-exists-p home)))))

;;; --- launchd: the same verbs on macOS -----------------------------------------
;;; WITH-SCRIPTED-LAUNCHD is the macOS twin of WITH-SCRIPTED-SYSTEMD: the
;;; LaunchAgents folder under the scratch home, the manager set to :LAUNCHD,
;;; and a FAKE-LAUNCHD answering launchctl — print, enable, bootstrap,
;;; bootout, kickstart in the account's gui domain — and tail for the log.

(defstruct fake-agent
  (loaded nil)
  (running nil)
  (pid nil))

(defstruct fake-launchd
  (agents (make-hash-table :test #'equal))
  (calls '())
  (bootstrap-saw-file :unasked))

(nlk:access (agent fake-agent))

(defun fake-agent-record (fake label)
  (alexandria:ensure-gethash label (fake-launchd-agents fake) (make-fake-agent)))

(defun fake-launchctl (fake arguments)
  "launchctl ARGUMENTS against FAKE, in this account's gui domain."
  (let* ((verb (first arguments))
         (domain (nlk::launchd-domain))
         (target (find-if (lambda (word) (uiop:string-prefix-p (concatenate 'string domain "/") word))
                          (rest arguments)))
         (label (and target (subseq target (1+ (length domain)))))
         (agent (and label (fake-agent-record fake label))))
    (flet ((not-found ()
             (values "" (format nil "Could not find service ~s in domain for uid: 501" label) 113)))
      (nlk:dispatch verb string=
        ("print" (if (and agent agent.loaded)
                     (values (format nil "~a = {~%	active count = 1~%	state = ~:[not running~;running~]~%~@[	pid = ~d~%~]	last exit code = 0~%}~%"
                                     target agent.running (and agent.running agent.pid))
                             "" 0)
                     (not-found)))
        ("enable" (values "" "" 0))
        ("bootstrap" (let* ((path (third arguments))
                            (agent (fake-agent-record fake (pathname-name (pathname path)))))
                       (setf (fake-launchd-bootstrap-saw-file fake) (and (probe-file path) t))
                       (cond ((not (probe-file path))
                              (values "" "Bootstrap failed: 2: No such file or directory" 5))
                             (agent.loaded
                              (values "" "Bootstrap failed: 5: Input/output error" 5))
                             (t (setf agent.loaded t agent.running t agent.pid 4242)
                                (values "" "" 0)))))
        ("bootout" (cond ((and agent agent.loaded)
                          (setf agent.loaded nil agent.running nil agent.pid nil)
                          (values "" "" 0))
                         (t (values "" "Boot-out failed: 3: No such process" 3))))
        ("kickstart" (cond ((and agent agent.loaded)
                            (setf agent.running t
                                  agent.pid (if (member "-k" arguments :test #'string=)
                                                4243
                                                (or agent.pid 4242)))
                            (values "" "" 0))
                           (t (not-found))))
        (t (values "" (format nil "Unrecognized subcommand: ~a" verb) 64))))))

(defparameter +test-launcher-bytes+ "the launcher this build carries"
  "What the scripted launchd's shipped launcher holds.")

(defmacro with-scripted-launchd ((fake &rest options) &body body)
  "BODY with the LaunchAgents folder under this test's scratch home, the
manager set to launchd and *UNIT-RUNNER* answering from FAKE, a
FAKE-LAUNCHD made from OPTIONS."
  `(let ((,fake (make-fake-launchd ,@options)))
     (declare (ignorable ,fake))
     (write-temp-file (nlk:home "shipped/nodecode-launcher") +test-launcher-bytes+)
     (setf nlk:*service-manager* :launchd
           nlk:*unit-directory* (nlk:home "Library/LaunchAgents/")
           nlk::*shipped-launcher* (nlk:home "shipped/nodecode-launcher")
           nlk:*unit-runner*
           (lambda (argv)
             (setf (fake-launchd-calls ,fake) (append (fake-launchd-calls ,fake) (list argv)))
             (let ((program (first argv)))
               (cond ((string= program "launchctl") (fake-launchctl ,fake (rest argv)))
                     ;; the launcher's one child: the organism, a thousand pids on.
                     ;; Asked without -a it answers nothing, as macOS's pgrep
                     ;; answers the child that asks after itself.
                     ((string= program "pgrep")
                      (let ((pid (parse-integer (second (member "-P" argv :test #'string=)))))
                        (if (and (member "-a" argv :test #'string=)
                                 (find pid (alexandria:hash-table-values (fake-launchd-agents ,fake))
                                       :key #'fake-agent-pid))
                            (values (format nil "~d~%" (+ pid 1000)) "" 0)
                            (values "" "" 1))))
                     ((string= program "tail")
                      (let ((path (car (last argv))))
                        (if (probe-file path)
                            (values (uiop:read-file-string path) "" 0)
                            (values "" (format nil "tail: ~a: No such file or directory" path) 1))))
                     (t (values "" (format nil "Couldn't find executable ~s" program) :missing))))))
     ,@body))

(defun agent-verbs (fake)
  "The launchctl verbs FAKE was handed, in order."
  (loop for argv in (fake-launchd-calls fake)
        when (equal (first argv) "launchctl") collect (second argv)))

(deftest launchd-names-the-agent-plainly (with-scripted-launchd (fake))
  (let ((coder (nlk:profile-home "coder")))
    (is (equal "nodecode-gateway" (nlk:gateway-unit-name (nlk:default-home))))
    (is (equal "nodecode-gateway-coder" (nlk:gateway-unit-name coder)))
    (is (equal "nodecode-gateway-coder.plist" (file-namestring (nlk:gateway-unit-path coder))))
    (is (equal "nodecode-gateway-coder.log" (file-namestring (nlk:gateway-log-path coder))))
    (is (search "Library/Logs/nodecode/" (namestring (nlk:gateway-log-path))))))

(deftest launchd-agent-text-is-a-plist-naming-the-home (with-scripted-launchd (fake))
  (is-carrying (text (nlk:gateway-unit-text :manager :launchd :home #p"/tmp/h--x/"
                                            :program "/opt/node <code>/nodecode&x"
                                            :path "/usr/bin:/bin"
                                            :user-home (namestring (nlk::account-home))))
    (is (uiop:string-prefix-p "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" text))
    (is (search (format nil "~%<!-- nodecode gateway: /tmp/h- -x/ -->~%") text))
    (let ((path (nlk:home "agent.plist")))
      (write-temp-file path text)
      (is (eq :ours (nlk::unit-file-owner path)) "which is what makes the file ours"))
    ("<key>Label</key><string>nodecode-gateway-" "the label")
    (is (search (format nil "<array>~%    <string>~a</string>~%    <string>/opt/node &lt;code&gt;/nodecode&amp;x</string>~%    <string>gateway</string>~%    <string>run</string>"
                        (uiop:native-namestring (nlk::gateway-launcher-path)))
                text) "launchd runs the launcher, and the launcher the program, XML-escaped")
    (is (search "/Library/Application Support/nodecode/launcher/1/nodecode"
                (uiop:native-namestring (nlk::gateway-launcher-path))) "one launcher for the account")
    ("<key>NODECODE_HOME</key><string>/tmp/h--x</string>" "the home pinned")
    ("<key>PATH</key><string>/usr/bin:/bin</string>" "the PATH of whoever turned it on")
    (:absent "<key>HOME</key>" "the account's own home is not carried")
    ("<key>RunAtLoad</key><true/>" "starts at login")
    ("<key>KeepAlive</key><true/>" "and comes back from any exit")
    ("<key>StandardOutPath</key><string>" "its log file, since launchd keeps no journal")
    "Library/Logs/nodecode/nodecode-gateway-")
  (is-carrying (text (nlk:gateway-unit-text :manager :launchd :home #p"/tmp/h/" :program "/n/nodecode"
                                            :path "" :user-home "/scratch/h"
                                            :carried '(("XDG_CONFIG_HOME" . "/scratch/h/.config"))))
    ("<key>HOME</key><string>/scratch/h</string>" "a HOME that is not the account's rides along")
    ("<key>XDG_CONFIG_HOME</key><string>/scratch/h/.config</string>"
     "and so does the folder the shell that turned it on had")
    ("<key>WorkingDirectory</key><string>/scratch/h</string>" "and it runs there")))

(deftest launchd-gateway-on-writes-the-agent-and-bootstraps-it (with-scripted-launchd (fake))
  (let ((state (nlk:gateway-on :program +test-binary+)))
    (is (equal (concatenate 'string (nlk:gateway-label) ".plist") (file-namestring state.file)))
    (is (uiop:string-prefix-p (namestring (nlk:home "Library/LaunchAgents/"))
                              (namestring state.file)))
    ;; The scratch home is neither the default home nor a profile, so its
    ;; label carries the digest suffix; the point is the shape.
    (is-shape state (.supported is "macOS has a background gateway") (.active "active")
      (.installed eq :ours "the agent is ours") (.sub "running")
      (.unit (nlk:gateway-label) "named plainly, as launchd names it")
      (.main-pid eql 5242 "the organism: the launcher's child, not the agent's own pid")
      (.enabled is "starts at login") (.linger null "and there is no linger to ask for"))
    (is (equal +test-launcher-bytes+ (uiop:read-file-string (nlk::gateway-launcher-path))) "installed")
    (is (search '("enable" "bootstrap" "kickstart") (agent-verbs fake) :test #'equal))
    (is (not (member "loginctl" (fake-launchd-calls fake) :key #'first :test #'equal)))
    (is (eq t (fake-launchd-bootstrap-saw-file fake)) "the file was there when launchd read it")
    (is (uiop:directory-exists-p (nlk:home "Library/Logs/nodecode/")))
    (let ((report (nlk:gateway-report state :command "/gateway")))
      (is (search "on — starting (pid 5242)" report))
      (is (search "stops when you log out" report) "an agent lives in the login session")
      (is (not (search "loginctl" report)) "and no sudo is offered")
      (is (search (format nil "agent    ~a" (nlk:gateway-label)) report) "the row names the agent")))
  (is (equal 1 (count "bootstrap" (agent-verbs fake) :test #'equal)))
  (nlk:gateway-on :program +test-binary+)
  (is (equal 1 (count "bootstrap" (agent-verbs fake) :test #'equal)))
  (write-temp-file nlk::*shipped-launcher* "the next build's launcher")
  (nlk:gateway-on :program "/opt/nodecode/v2/nodecode")
  ;; never rewritten after: the operator's folder grants are held against its bytes
  (is (equal +test-launcher-bytes+ (uiop:read-file-string (nlk::gateway-launcher-path))))
  (is (search '("bootout" "enable" "bootstrap")
              (remove "print" (agent-verbs fake) :test #'equal) :test #'equal))
  (is (search "/opt/nodecode/v2/nodecode" (uiop:read-file-string (nlk:gateway-unit-path)))))

(deftest launchd-gateway-on-refuses-a-build-with-no-launcher (with-scripted-launchd (fake))
  (setf nlk::*shipped-launcher* nil)
  (is-carrying (refusal (refusal-text nlk:gateway-error (nlk:gateway-on :program +test-binary+)))
    ("carries no gateway launcher" "saying what is missing"))
  (is (null (probe-file (nlk:gateway-unit-path))) "and no agent names a launcher that is not there")
  (is (not (member "bootstrap" (agent-verbs fake) :test #'equal))))

(deftest launchd-gateway-off-boots-the-agent-out-file-first (with-scripted-launchd (fake))
  (nlk:gateway-on :program +test-binary+)
  (let* ((file (nlk:gateway-unit-path))
         (state (nlk:gateway-off)))
    (is (null (probe-file file)) "the agent file is gone")
    (is (member "bootout" (agent-verbs fake) :test #'equal) "and launchd was told")
    (is-shape state (.installed null) (.active "inactive"))
    (is (nlk:gateway-off-p) "the operator's off is remembered")
    (is (uiop:string-prefix-p "off (you turned it off)" (nlk::gateway-summary state)))))

(deftest launchd-gateway-restart-kickstarts-or-reloads (with-scripted-launchd (fake))
  (nlk:gateway-on :program +test-binary+)
  (setf (fake-launchd-calls fake) '())
  (nlk:gateway-restart :program +test-binary+)
  (is (equal '("kickstart") (remove "print" (agent-verbs fake) :test #'equal)))
  (is (member "-k" (find "kickstart" (fake-launchd-calls fake) :key #'second :test #'equal)
              :test #'equal))
  (setf (fake-launchd-calls fake) '())
  (let ((state (nlk:gateway-restart :program "/opt/nodecode/v2/nodecode")))
    (is (equal '("bootout" "bootstrap") (remove "print" (agent-verbs fake) :test #'equal)))
    (is (search "/opt/nodecode/v2/nodecode"
                (uiop:read-file-string state.file))))
  (nlk:gateway-off)
  (is (signals-error nlk:gateway-error (nlk:gateway-restart :program +test-binary+))))

(deftest launchd-gateway-logs-read-the-agents-log-file (with-scripted-launchd (fake))
  (nlk:gateway-on :program +test-binary+)
  (is (search "has written no log yet" (nlk:gateway-log-text)) "no file, no log")
  (write-temp-file (nlk:gateway-log-path) (format nil "serving store /x on port 4~%"))
  (is (search "serving store" (nlk:gateway-log-text)) "the file's lines")
  (is (equal (list "tail" "-n" "60" "-f" (uiop:native-namestring (nlk:gateway-log-path)))
             (nlk:gateway-log-argv :follow t))))

(deftest launchd-gateway-start-kickstarts-for-the-reconnect-rung (with-scripted-launchd (fake))
  (nlk:gateway-on :program +test-binary+)
  (setf (fake-launchd-calls fake) '())
  (is (nlk:gateway-start) "launchd took the job")
  (is (equal (list "launchctl" "kickstart" (format nil "~a/~a" (nlk::launchd-domain) (nlk:gateway-label)))
             (nlk:gateway-start-argv))))

(deftest launchd-gateway-refuses-with-launchds-answer (with-scripted-launchd (fake))
  (setf nlk:*unit-runner*
        (lambda (argv)
          (if (equal (second argv) "print")
              (values "" "Could not find service" 113)
              (values "" "Bootstrap failed: 125: Domain does not support specified action" 125))))
  ;; A domain that will not take the agent is a refusal:
  (is-carrying (refusal (refusal-text nlk:gateway-error (nlk:gateway-on :program +test-binary+)))
    ("launchctl bootstrap" "naming the call")
    ("Domain does not support" "and quoting launchd")))

(deftest launchd-unit-text-of-a-manager-with-no-arm-is-refused (let ((nlk:*service-manager* nil)))
  (is (signals-error error (nlk:gateway-unit-text :home #p"/tmp/h/" :program "/n/nodecode"))))

(deftest gateway-by-default-is-a-named-homes-own ()
  (is (nlk:gateway-by-default-p (nlk:default-home)) "the default home's gateway comes on by itself")
  (is (nlk:gateway-by-default-p (nlk:profile-home "coder")) "and a named profile's")
  (is (not (nlk:gateway-by-default-p #p"/srv/a/agent/")) "a folder $NODECODE_HOME names is the operator's to turn on"))
