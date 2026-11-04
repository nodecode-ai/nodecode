;;;; sandbox-test.lisp --- `nodecode --sandbox': the srt settings, the wrap,
;;;; the flag and the port a jailed gateway is told.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun sandbox-test-settings (&key secret-files policy (exists (constantly t))) "The settings for a fixed home, folder and binary."
  (nle::sandbox-settings :home "/h/.nodecode/" :folder "/w/proj/" :binary "/opt/nc/nodecode"
                         :srt-dir "/opt/srt/" :policy policy :secret-files secret-files
                         :provider-hosts '("openrouter.ai") :exists exists
                         :run-directory "/run/user/1000"))

(deftest sandbox-settings-carry-every-key-srt-requires ()
  (let ((settings (sandbox-test-settings)))
    (dolist (path '(("network" "allowedDomains") ("network" "deniedDomains")
                    ("filesystem" "denyRead") ("filesystem" "allowRead")
                    ("filesystem" "allowWrite") ("filesystem" "denyWrite")))
      (is (vectorp (apply #'nlk:json-value settings :array path)) (format nil "~{~a~^.~} is an array" path)))
    (is (find "openrouter.ai" (nlk:json-value settings :array "network" "allowedDomains") :test #'string=) "a configured provider's host is reachable")
    (is (find "github.com" (nlk:json-value settings :array "network" "allowedDomains") :test #'string=) "the cells repository is reachable")
    (is (find "/w/proj/" (nlk:json-value settings :array "filesystem" "allowWrite") :test #'string=) "the folder is writable")
    (is (find "/opt/nc/nodecode" (nlk:json-value settings :array "filesystem" "denyWrite") :test #'string=) "the binary that runs the jail is not")
    (is (find "/h/.nodecode/secrets/" (nlk:json-value settings :array "filesystem" "denyWrite") :test #'string=) "nor the secrets")
    (is (find "/run/user/1000" (nlk:json-value settings :array "filesystem" "denyRead") :test #'string=) "the runtime directory's sockets are out of reach")
    (is (find "/tmp" (nlk:json-value settings :array "filesystem" "denyRead") :test #'string=) "so is the host's /tmp: the jail gets an empty one of its own")
    (is (not (find "/tmp" (nlk:json-value settings :array "filesystem" "allowWrite") :test #'string=)) "and never the host's to write")
    (is (find "/h/.nodecode/sandbox/srt/" (nlk:json-value settings :array "filesystem" "denyWrite") :test #'string=) "srt's sockets and stand-ins are not the jail's to write")
    (is (null (nth-value 1 (nlk:json-value settings :any "network" "tlsTerminate"))) "no masked secret, no TLS termination")))

(deftest sandbox-settings-mount-only-what-exists ()
  (let ((settings (sandbox-test-settings :exists (lambda (path) (search ".ssh" path)))))
    (is (equalp (vector (nle::sandbox-expand "~/.ssh"))
                (nlk:json-value settings :array "filesystem" "denyRead")) "a deny-read entry that is not on disk is left out")))

(deftest sandbox-settings-mask-each-secret-toward-its-host ()
  (let* ((settings (sandbox-test-settings
                    :secret-files '(("/h/.nodecode/secrets/providers.openrouter.api_key" . "openrouter.ai"))))
         (masks (nlk:json-value settings :array "credentials" "files")))
    (is (= 1 (length masks)))
    (is (equal "mask" (nlk:json-value (aref masks 0) :string "mode")))
    (is (equal "^\\s*(\\S+)" (nlk:json-value (aref masks 0) :string "extract")) "only the key's span is masked, never the newline after it")
    (is (equal "deny" (nlk:json-value (aref masks 0) :string "onExtractNoMatch")) "a file with no key in it is denied, not shown")
    (is (equalp #("openrouter.ai") (nlk:json-value (aref masks 0) :array "injectHosts")) "the real key goes only to its provider")
    (is (hash-table-p (nlk:json-value settings :any "network" "tlsTerminate")) "a mask needs TLS termination")))

(deftest sandbox-settings-merge-the-operators-policy ()
  (let ((settings (sandbox-test-settings
                   :policy '(:allowed-domains ("pypi.org") :deny-read ("/srv/keys") :allow-write ("/data")))))
    (is (find "pypi.org" (nlk:json-value settings :array "network" "allowedDomains") :test #'string=))
    (is (find "/srv/keys" (nlk:json-value settings :array "filesystem" "denyRead") :test #'string=))
    (is (find "/data" (nlk:json-value settings :array "filesystem" "allowWrite") :test #'string=))
    (is (not (search "allowAllUnixSockets" (nlk:encode-json-object settings))) "seccomp's unix-socket block stays on: the jail makes no unix socket, so it reaches none of the host's")))

(deftest sandbox-wrap-jails-the-serve-argv-whole ()
  (let ((argv (nle::sandbox-wrap '("/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
                                 :srt "/opt/srt/srt" :settings "/h/.nodecode/sandbox/srt-settings.json"
                                 :port 41234 :run "/h/.nodecode/sandbox/run/"
                                 :srt-temporary "/h/.nodecode/sandbox/srt/"
                                 :jail-temporary "/h/.nodecode/sandbox/tmp/")))
    (is (equal '("env" "-u" "NODECODE_PRISTINE" "NODECODE_SANDBOXED=1" "NODECODE_GATEWAY_PORT=41234"
                 "XDG_RUNTIME_DIR=/h/.nodecode/sandbox/run" "XDG_CACHE_HOME=/h/.nodecode/sandbox/cache"
                 "TMPDIR=/h/.nodecode/sandbox/srt" "CLAUDE_CODE_TMPDIR=/h/.nodecode/sandbox/tmp"
                 "/opt/srt/srt" "--settings" "/h/.nodecode/sandbox/srt-settings.json" "--" "sh" "-c")
               (subseq argv 0 15)) "env (srt's temporary directory out of /tmp, the jail's own), srt, its settings and the -- that keeps the words apart")
    (is (equal '("41234" "/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
               (subseq argv 16)) "the port as $0, then the serve argv word for word")
    (let ((script (nth 15 argv)))
      (is (search "exec 3>&1 1>&2" script) "the listener handed in on stdout moves to descriptor 3")
      (is (search "Seccomp:" script) "a jail without the seccomp filter stops before anything loads")
      (is (search "socat ACCEPT-FD:3,fork TCP4:127.0.0.1:\"$0\"" script) "a socat accepts on it toward the gateway's port")
      (is (not (search "UNIX-LISTEN" script)) "and makes no unix socket of its own")
      (is (search "exec 3>&- \"$@\"" script) "the serve runs with the listener closed"))))

#+linux
(deftest sandbox-jail-script-stops-a-jail-without-seccomp ()
  ;; Outside srt this process carries no seccomp filter, as a jail would not
  ;; on an architecture srt has no helper for: the script stops there, before
  ;; the serve runs. A runner that is itself under a filter skips the check.
  (when (search (format nil "Seccomp:~c0" #\Tab) (uiop:read-file-string "/proc/self/status"))
    (multiple-value-bind (output errors status)
        (uiop:run-program (list "sh" "-c" nle::+sandbox-jail-script+ "41234" "sh" "-c" "echo the-serve-ran")
                          :output :string :error-output :string :ignore-error-status t)
      (is (eql 78 status) "the jail script exits 78")
      (is (search "no seccomp filter" errors) "saying why, on the log")
      (is (not (search "the-serve-ran" (concatenate 'string output errors))) "and the serve never runs"))))

(deftest sandbox-launch-makes-the-files-the-launcher-reads ()
  ;; A home with no config.jsonc: srt would cover the absent path with a
  ;; placeholder, and the jailed organism died reading its config through it.
  (let ((home (sandbox-test-home)))
    (unwind-protect
         (progn
           (is (= 2 (length (nle::sandbox-ensure-launcher-files home))) "both made when absent")
           (dolist (name '("sandbox.jsonc" "config.jsonc"))
             (is (equal (format nil "{}~%") (uiop:read-file-string (merge-pathnames name home))) (format nil "~a is empty JSON" name)))
           (with-open-file (o (merge-pathnames "config.jsonc" home) :direction :output :if-exists :supersede)
             (write-line "{\"general\":{}}" o))
           (is (null (nle::sandbox-ensure-launcher-files home)) "a second launch makes none")
           (is (search "general" (uiop:read-file-string (merge-pathnames "config.jsonc" home))) "and never touches the operator's config"))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(defun sandbox-port-taken-p (port)
  "Whether a listener holds the loopback PORT."
  (handler-case (progn (nle::local-port port) nil)
    (usocket:address-in-use-error () t)))

(deftest sandbox-listener-is-the-jails-one-way-in ()
  ;; The shell listens on the host's loopback and hands the socket to the
  ;; jail as its stdout; what it answers is a descriptor a child can take.
  (let* ((port (nle::local-port 0))
         (socket (nle::sandbox-listener port)))
    (unwind-protect
         (progn
           (is (integerp (sb-bsd-sockets:socket-file-descriptor socket)) "a descriptor")
           (is (eql port (nth-value 1 (sb-bsd-sockets:socket-name socket))) "on the port the plan named")
           (is (sandbox-port-taken-p port) "and listening: nothing else binds the port while it stands"))
      (sb-bsd-sockets:socket-close socket))))

(deftest sandbox-stdout-is-the-listener-only-in-a-sandboxed-shell ()
  (let ((nle::*sandbox-plan* nil))
    (is (eq :plain (nle::call-with-sandbox-stdout (lambda (listener) (if listener :listener :plain)))) "outside a sandboxed shell there is none"))
  (let* ((port (nle::local-port 0))
         (nle::*sandbox-plan* (list :port port))
         (seen nil))
    (nle::call-with-sandbox-stdout (lambda (listener)
                                     (setf seen listener)
                                     (is (sandbox-port-taken-p port) "while the function runs the listener holds the port")))
    (is (integerp seen) "a sandboxed shell's serve gets a descriptor")
    (is (eql port (nle::local-port port)) "and the shell's copy is closed once the function returns: the jail alone holds it")))

(deftest sandbox-requires-a-socat-that-takes-a-descriptor ()
  ;; The jail's one way in is a listening socket on its stdout: the socat
  ;; inside must accept on a descriptor it is handed.
  (is (eq (and (nlk::executable-on-path "socat") t) (and (nle::socat-accepts-a-descriptor-p) t)) "the socat on this machine takes ACCEPT-FD, or there is none"))

(deftest sandbox-wrap-command-is-the-argv-itself-outside-a-sandbox ()
  (let ((nle::*sandbox-plan* nil))
    (is (equal '("nodecode" "serve") (nle::sandbox-wrap-command '("nodecode" "serve"))))))

(deftest sandbox-flags-are-for-a-launch-that-runs-an-organism ()
  (let ((nle::*sandbox-asked* nil) (nle::*sandbox-refused* nil))
    (nle::parse-launch-arguments '("--sandbox"))
    (is (eq t nle::*sandbox-asked*) "--sandbox is read"))
  (let ((nle::*sandbox-asked* nil) (nle::*sandbox-refused* nil))
    (nle::parse-launch-arguments '("--no-sandbox"))
    (is (eq t nle::*sandbox-refused*) "--no-sandbox is read"))
  (dolist (line '(("serve" "--sandbox") ("gateway" "run" "--no-sandbox") ("--no-sandbox" "rpc")))
    (let ((nle::*sandbox-asked* nil) (nle::*sandbox-refused* nil))
      (is (handler-case (progn (nle::parse-launch-arguments line) t)
            (nle::launch-usage-error () nil))
          (format nil "~{~a~^ ~} runs an organism, so it takes the flag" line))))
  (let ((nle::*sandbox-asked* nil) (nle::*sandbox-refused* nil))
    (is (search "opposite" (refusal-text nle::launch-usage-error
                             (nle::parse-launch-arguments '("--sandbox" "--no-sandbox")))) "both at once are refused")))

(deftest sandbox-srt-on-path-reaches-the-argv-as-a-string ()
  ;; npm installs srt as a link to its dist/cli.js; the PATH lookup answers
  ;; the link's truename, a pathname, which the spawn refused as an argv word.
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "nodecode-srt-~36r/" (random (expt 36 8)))
                                (uiop:temporary-directory))))
         (target (merge-pathnames "cli.js" dir))
         (saved-path (uiop:getenv "PATH"))
         (saved-srt (uiop:getenv "NODECODE_SRT")))
    (unwind-protect
         (progn
           (ensure-directories-exist dir)
           (with-open-file (s target :direction :output) (write-line "#!/bin/sh" s))
           (sb-posix:chmod (uiop:native-namestring target) #o755)
           (sb-posix:symlink (uiop:native-namestring target)
                             (uiop:native-namestring (merge-pathnames "srt" dir)))
           (nlk::set-environment-variable "NODECODE_SRT" "")
           (nlk::set-environment-variable "PATH" (string-right-trim "/" (uiop:native-namestring dir)))
           (let ((srt (nle::srt-program)))
             (is (stringp srt) "srt found on PATH is a namestring")
             (is (every #'stringp (nle::sandbox-wrap '("nodecode" "serve") :srt srt :settings #p"/h/sandbox/srt.json"
                                                                             :port 1 :run #p"/h/sandbox/run/")) "the jailed argv is words only")))
      (nlk::set-environment-variable "PATH" (or saved-path ""))
      (nlk::set-environment-variable "NODECODE_SRT" (or saved-srt ""))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(deftest sandbox-gateway-port-from-the-environment ()
  (let ((saved (uiop:getenv "NODECODE_GATEWAY_PORT")))
    (unwind-protect
         (progn
           (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" "41234")
           (is (eql 41234 (nle::gateway-port-from-environment)))
           (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" "70000")
           (is (null (nle::gateway-port-from-environment)) "past 65535 is no port")
           (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" "x")
           (is (null (nle::gateway-port-from-environment)) "nor is a word"))
      (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" (or saved "")))))

(deftest sandbox-settings-deny-the-home-and-read-back-only-the-work ()
  (let* ((settings (sandbox-test-settings))
         (deny (nlk:json-value settings :array "filesystem" "denyRead"))
         (allow (nlk:json-value settings :array "filesystem" "allowRead")))
    (is (find (uiop:native-namestring (user-homedir-pathname)) deny :test #'string=) "the user's home is read-denied whole")
    (dolist (path '("/w/proj/" "/h/.nodecode/" "/opt/nc/" "/opt/srt/"))
      (is (find path allow :test #'string=) (format nil "~a reads back" path)))
    (is (not (find (nle::sandbox-expand "~/.ssh") allow :test #'string=)) "and nothing of the credentials does")))

(deftest sandbox-settings-deny-the-accounts-own-home-under-another-home ()
  ;; $HOME pointed elsewhere (a wrapper, sudo, a CI user) moved the whole-home
  ;; deny there, and the account's real home -- ~/.ssh with it -- read open.
  (let ((saved (uiop:getenv "HOME"))
        (own (uiop:native-namestring
              (uiop:ensure-directory-pathname
               (sb-posix:passwd-dir (sb-posix:getpwuid (sb-posix:getuid)))))))
    (unwind-protect
         (progn
           (nlk::set-environment-variable "HOME" "/nonexistent-home-for-test/")
           (let ((deny (nlk:json-value (sandbox-test-settings) :array "filesystem" "denyRead")))
             (is (find "/nonexistent-home-for-test/" deny :test #'string=) "the home $HOME names is read-denied")
             (is (find own deny :test #'string=) "the account's own home is read-denied too")))
      (nlk::set-environment-variable "HOME" (or saved "")))))

(deftest sandbox-hides-the-homes-own-secrets ()
  (let* ((settings (nle::sandbox-settings :home "/h/.nodecode/" :folder "/w/proj/" :binary "/opt/nc/nodecode"
                                          :exists (constantly t) :run-directory nil :deny-home nil
                                          :hidden (list "/h/.nodecode/auth.json" "/h/.nodecode/operator-token")))
         (deny (nlk:json-value settings :array "filesystem" "denyRead")))
    (is (find "/h/.nodecode/auth.json" deny :test #'string=) "auth.json is not read in the jail")
    (is (find "/h/.nodecode/operator-token" deny :test #'string=) "nor the gateway's token")
    (is (zerop (length (nlk:json-value settings :array "filesystem" "allowRead"))) "without the home deny nothing needs reading back")))

(deftest sandbox-settings-deny-writing-what-the-launcher-reads ()
  (let ((deny (nlk:json-value (sandbox-test-settings) :array "filesystem" "denyWrite"))
        (allow (nlk:json-value (sandbox-test-settings) :array "filesystem" "allowWrite")))
    (dolist (path '("/h/.nodecode/config.jsonc" "/h/.nodecode/sandbox.jsonc" "/h/.nodecode/sandbox/jailed.json" "/h/.nodecode/secrets/"
                    "/h/.nodecode/sandbox/srt/"))
      (is (find path deny :test #'string=) (format nil "~a is the launcher's, not the jail's" path)))
    (is (notany (lambda (path) (search ".cache/nodecode" path)) allow) "the machine's shared cache is not the jail's to write")))

(defun sandbox-test-home ()
  "A scratch home with one cells/ folder under git, one commit in it."
  (let ((home (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "nc-sbx-~36r/" (random (expt 36 8))) (uiop:temporary-directory)))))
    (ensure-directories-exist (merge-pathnames "cells/vise/" home))
    (with-open-file (o (merge-pathnames "cells/vise/a.lisp" home) :direction :output :if-exists :supersede)
      (write-line "(defun a () 1)" o))
    (uiop:run-program (list "sh" "-c" "git init -q && git add -A && git -c user.email=t@t -c user.name=t commit -qm one")
                      :directory (merge-pathnames "cells/vise/" home))
    home))

(deftest sandbox-mark-holds-its-first-snapshot ()
  (let ((home (sandbox-test-home)))
    (unwind-protect
         (progn
           (is (null (nle::sandbox-jailed-refusal home :loads t)) "an unmarked home launches anywhere")
           (nle::sandbox-mark home)
           (let ((first (uiop:read-file-string (nle::sandbox-mark-path home))))
             (with-open-file (o (merge-pathnames "cells/vise/b.lisp" home) :direction :output :if-exists :supersede)
               (write-line "(defparameter *b* (sb-ext:run-program \"/bin/true\" nil))" o))
             (uiop:run-program (list "sh" "-c" "git add -A && git -c user.email=t@t -c user.name=t commit -qm escape")
                               :directory (merge-pathnames "cells/vise/" home))
             (nle::sandbox-mark home)
             (is (equal first (uiop:read-file-string (nle::sandbox-mark-path home))) "a later sandboxed launch never moves the mark")))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest sandbox-mark-refuses-an-unjailed-load ()
  (let ((home (sandbox-test-home)))
    (unwind-protect
         (progn
           (nle::sandbox-mark home)
           (is (search "has run under --sandbox" (nle::sandbox-jailed-refusal home :loads t)) "a plain launch, serve or rpc is refused")
           (is (null (nle::sandbox-jailed-refusal home :loads t :asked t)) "--sandbox runs")
           (is (null (nle::sandbox-jailed-refusal home :loads t :sandboxed "1")) "the organism inside the jail runs")
           (is (null (nle::sandbox-jailed-refusal home :loads nil)) "a verb that loads no folder runs"))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest sandbox-review-names-what-the-jail-wrote ()
  (let ((home (sandbox-test-home)))
    (unwind-protect
         (progn
           (nle::sandbox-mark home)
           (is (search "nothing: cells/ is as the mark found it" (nle::sandbox-review-text home)) "an untouched home has nothing to show")
           (with-open-file (o (merge-pathnames "cells/vise/b.lisp" home) :direction :output :if-exists :supersede)
             (write-line "(defun b () 2)" o))
           (uiop:run-program (list "sh" "-c" "git add -A && git -c user.email=t@t -c user.name=t commit -qm escape-commit")
                             :directory (merge-pathnames "cells/vise/" home))
           (ensure-directories-exist (merge-pathnames "cells/planted/" home))
           (let ((text (nle::sandbox-review-text home)))
             (is (search "cells/vise: 1 commit since the mark" text) "a new commit in the layer is counted")
             (is (search "escape-commit" text) "and named")
             (is (search "b.lisp" text) "with the files it touches")
             (is (search "cells/planted: a new folder" text) "a folder the jail planted is named")))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest sandbox-release-needs-yes-and-the-outside ()
  (let* ((home (sandbox-test-home))
         (nlk::*home* home)
         (saved (uiop:getenv "NODECODE_SANDBOXED")))
    (unwind-protect
         (progn
           (nle::sandbox-mark home)
           (is (= 2 (nth-value 1 (nle::sandbox-answer '("release")))) "release asks for --yes")
           (nlk::set-environment-variable "NODECODE_SANDBOXED" "1")
           (is (= 1 (nth-value 1 (nle::sandbox-answer '("release" "--yes")))) "never from inside the jail")
           (nlk::set-environment-variable "NODECODE_SANDBOXED" "")
           (is (probe-file (nle::sandbox-mark-path home)) "the mark still stands")
           (is (zerop (nth-value 1 (nle::sandbox-answer '("release" "--yes")))) "from outside, with --yes, it lifts")
           (is (null (probe-file (nle::sandbox-mark-path home))) "and the mark is gone"))
      (nlk::set-environment-variable "NODECODE_SANDBOXED" (or saved ""))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))


;;; --- on by default -----------------------------------------------------------

(defmacro with-sandbox-test-home ((home) &body body)
  "BODY with HOME a fresh test home that is NLK:*HOME*, outside any jail."
  `(let* ((,home (sandbox-test-home))
          (nlk::*home* ,home)
          (saved (uiop:getenv "NODECODE_SANDBOXED")))
     (unwind-protect
          (progn (nlk::set-environment-variable "NODECODE_SANDBOXED" "") ,@body)
       (nlk::set-environment-variable "NODECODE_SANDBOXED" (or saved ""))
       (uiop:delete-directory-tree ,home :validate t :if-does-not-exist :ignore))))

(deftest sandbox-is-on-unless-something-says-otherwise ()
  (with-sandbox-test-home (home)
    (flet ((why (&rest keys) (nth-value 1 (apply #'nle::sandbox-decision home keys))))
      (is (eq #+linux :default #-linux :no-jail (why)) "a fresh home is jailed where the OS has a jail")
      (is (eq :asked (why :asked t)) "--sandbox")
      (is (eq :refused (why :refused t)) "--no-sandbox")
      (is (eq :inside (why :sandboxed t :asked t)) "the organism inside the jail starts no jail of its own")
      (nle::sandbox-policy-write home '("jail") :false)
      (is (eq :off (why)) "the home's setting turns it off")
      (is (eq :asked (why :asked t)) "and --sandbox still jails one launch"))))

(deftest sandbox-off-asks-is-kept-and-lifts-the-mark ()
  (with-sandbox-test-home (home)
    (nle::sandbox-mark home)
    (let ((nle::*sandbox-terminal* nil))
      (is (= 2 (nth-value 1 (nle::sandbox-answer '("off")))) "no terminal and no --yes: refused")
      (is (eq :on (nle::sandbox-setting home)) "and nothing changed"))
    (let ((nle::*sandbox-terminal* t) (*error-output* (make-broadcast-stream)))
      (with-input-from-string (*standard-input* (format nil "n~%"))
        (is (= 1 (nth-value 1 (nle::sandbox-answer '("off")))) "a no keeps the jail"))
      (with-input-from-string (*standard-input* (format nil "y~%"))
        (is (zerop (nth-value 1 (nle::sandbox-answer '("off")))) "a yes turns it off")))
    (is (eq :off (nle::sandbox-setting home)) "kept in sandbox.jsonc")
    (is (null (probe-file (nle::sandbox-mark-path home))) "and the mark is lifted with it")
    (is (zerop (nth-value 1 (nle::sandbox-answer '("on")))) "on")
    (is (eq :on (nle::sandbox-setting home)) "puts it back")
    (nlk::set-environment-variable "NODECODE_SANDBOXED" "1")
    (dolist (words '(("off" "--yes") ("allow" "example.org") ("revoke" "example.org") ("on")))
      (is (= 1 (nth-value 1 (nle::sandbox-answer words))) (format nil "~{~a~^ ~} never runs inside the jail" words)))
    (nlk::set-environment-variable "NODECODE_SANDBOXED" "")
    (is (eq :on (nle::sandbox-setting home)) "the jail could not turn itself off")))

(deftest sandbox-no-sandbox-asks-on-a-terminal-only ()
  (with-sandbox-test-home (home)
    (let ((nle::*sandbox-terminal* nil))
      (multiple-value-bind (text code) (nle::sandbox-refused-launch home)
        (is (= 2 code) "no terminal: refused")
        (is (search "nodecode sandbox off --yes" text) "naming the way that does not ask")))
    (let ((nle::*sandbox-terminal* t) (*error-output* (make-broadcast-stream)))
      (with-input-from-string (*standard-input* (format nil "y~%"))
        (is (null (nle::sandbox-refused-launch home)) "a yes lets the launch go on")))
    (is (eq :off (nle::sandbox-setting home)) "and is kept")))

(deftest sandbox-channel-hosts-come-from-the-binary-and-the-config ()
  (let* ((config (nlk:decode-json "{\"channels\": {\"discord\": {\"bot_token_env\": \"T\"},
                                    \"matrix\": {\"homeserver\": \"https://Matrix.Example.org:8448/\"},
                                    \"email\": {\"imap_host\": \"imap.example.com\", \"smtp_host\": \"smtp.example.com:587\"},
                                    \"imessage\": {\"url\": \"http://127.0.0.1:1234\"},
                                    \"telegram\": {\"enabled\": false}}}"))
         (hosts (nle::sandbox-channel-hosts config)))
    (is (member "*.discord.gg" (cdr (assoc "discord" hosts :test #'string=)) :test #'string=) "discord's gateway hosts, from the binary")
    (is (equal '("matrix.example.org") (cdr (assoc "matrix" hosts :test #'string=))) "a homeserver the config names")
    (is (equal '("imap.example.com" "smtp.example.com") (cdr (assoc "email" hosts :test #'string=))) "the mail servers, ports dropped")
    (is (null (cdr (assoc "imessage" hosts :test #'string=))) "never this machine's loopback")
    (is (null (assoc "telegram" hosts :test #'string=)) "a channel turned off adds nothing")
    (is (null (nle::sandbox-channel-hosts (nlk:decode-json "{}"))) "no channels, no hosts")))

(deftest sandbox-grants-are-kept-and-taken-back ()
  (with-sandbox-test-home (home)
    (let ((folder (uiop:native-namestring (ensure-directories-exist (merge-pathnames "grant/x/" home)))))
      (with-open-file (o (nle::sandbox-policy-path home) :direction :output :if-exists :supersede)
        (write-line "{ // the operator's note" o) (write-line "}" o))
      (is (eq :refused (nle::sandbox-grant-target "data")) "a bare word is neither")
      (is (equal '(:host "docs.example.net") (multiple-value-list (nle::sandbox-grant-target "https://Docs.example.net/a"))) "a URL is its host")
      (is (eq :refused (nle::sandbox-grant-target "./no-such-folder" home)) "a path that is not there")
      (is (equal (string-right-trim "/" folder) (nth-value 1 (nle::sandbox-grant-target "./grant/x" home))) "a relative path, made absolute")
      (is (zerop (nth-value 1 (nle::sandbox-answer (list "allow" folder "--write")))) "allow --write")
      (is (zerop (nth-value 1 (nle::sandbox-answer '("allow" "docs.example.net")))) "allow a host")
      (let ((policy (nle::sandbox-policy home)))
        (is (equal (list (string-right-trim "/" folder)) (getf policy :allow-write)) "kept as a written folder")
        (is (equal '("docs.example.net") (getf policy :allowed-domains)) "and a site"))
      (is (search "the operator's note" (uiop:read-file-string (nle::sandbox-policy-path home))) "the file's comments stay")
      (is (zerop (nth-value 1 (nle::sandbox-answer (list "allow" folder)))) "allow without --write")
      (is (equal (list nil (list (string-right-trim "/" folder)))
                 (let ((policy (nle::sandbox-policy home))) (list (getf policy :allow-write) (getf policy :allow-read))))
          "moves it to read only")
      (nle::sandbox-answer (list "revoke" folder))
      (nle::sandbox-answer '("revoke" "docs.example.net"))
      (let ((policy (nle::sandbox-policy home)))
        (is (and (null (getf policy :allow-read)) (null (getf policy :allowed-domains))) "revoke takes each back")))))

(deftest sandbox-folder-is-never-the-home-or-above ()
  (let ((home (uiop:ensure-directory-pathname (user-homedir-pathname))))
    (is (null (nle::sandbox-work-folder #p"/")) "not the root")
    (is (null (nle::sandbox-work-folder home)) "not the user's home")
    (is (null (nle::sandbox-work-folder (uiop:pathname-parent-directory-pathname home))) "nor a folder above it")
    (is (equal (merge-pathnames "proj/" home) (nle::sandbox-work-folder (merge-pathnames "proj/" home))) "a project under it is the folder")
    (is (null (nle::sandbox-work-folder (merge-pathnames "proj/" home) :serve t)) "a serve or a gateway holds none")
    (let ((settings (nle::sandbox-settings :home "/h/.nodecode/" :folder nil :binary "/opt/nc/nodecode"
                                           :exists (constantly t) :run-directory "/run/user/1000")))
      (is (equalp #("/h/.nodecode/") (gethash "allowWrite" (gethash "filesystem" settings))) "no folder: only the home is written"))))

(deftest sandbox-serve-runs-itself-in-the-jail ()
  (let ((nle::*serve-program* '("/opt/nc/nodecode")))
    (is (equal '("/opt/nc/nodecode" "serve") (nle::sandbox-serve-argv nil t)) "a serve")
    (is (equal '("/opt/nc/nodecode" "gateway" "run" "--store" "/s/h.db") (nle::sandbox-serve-argv #p"/s/h.db" :wait))
        "the gateway's, on the store its line named")))
