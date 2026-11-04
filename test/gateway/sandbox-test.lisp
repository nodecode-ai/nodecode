;;;; sandbox-test.lisp --- `nodecode --sandbox': the bwrap jail's argv, its
;;;; seccomp filter, the descriptors handed in, the flag and the mark.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun sandbox-test-argv (&key policy hidden (environment '("PATH=/usr/bin:/home/u/bin" "HOME=/home/u" "NODECODE_HOME=/h/.nodecode"))
                               (folder "/w/proj/") (binary "/opt/nc/nodecode") (store "/h/.nodecode/history.db")
                               (directories '()))
  "The jail's argv for a fixed home, folder and binary, every path there."
  (nle::sandbox-bwrap-argv '("/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
                           :home "/h/.nodecode/" :folder folder :binary binary :store store
                           :policy policy :hidden hidden
                           :run "/h/.nodecode/sandbox/run/" :cache "/h/.nodecode/sandbox/cache/"
                           :temporary "/h/.nodecode/sandbox/tmp/" :port 41234 :egress-port 3128
                           :environment environment :exists (constantly t)
                           :link (lambda (name) (and (member name '("/bin" "/lib64") :test #'string=)
                                                     (format nil "usr~a" name)))
                           :directory-p (lambda (path) (member path directories :test #'string=))
                           :homes '("/home/u/")))

(defun sandbox-test-mount (argv flag path)
  "The index of the mount FLAG PATH in ARGV, or NIL."
  (loop for (a b c) on argv for i from 0
        when (and (string= a flag) (string= b path) (or (not (member flag '("--bind" "--ro-bind") :test #'string=)) (string= c path)))
          return i))

(defun sandbox-test-setenv (argv name)
  "The value ARGV's --setenv gives NAME, or NIL."
  (loop for (a b c) on argv when (and (string= a "--setenv") (string= b name)) return c))

(deftest sandbox-jail-has-namespaces-of-its-own ()
  (let ((argv (sandbox-test-argv)))
    (is (equal "bwrap" (first argv)) "bwrap, run by nodecode itself")
    (dolist (flag '("--unshare-user" "--unshare-pid" "--unshare-net" "--unshare-ipc" "--disable-userns" "--new-session"))
      (is (member flag argv :test #'string=) (format nil "~a" flag)))
    (is (member "--seccomp" argv :test #'string=) "under the filter")
    (is (equal "5" (second (member "--seccomp" argv :test #'string=))) "read from descriptor 5")
    (is (equal '("--" "/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db") (last argv 5)) "the serve argv word for word, a store path with a space one word")
    (is (not (find-if (lambda (word) (or (search "srt" word) (search "socat" word))) argv)) "no srt, no socat")))

(deftest sandbox-jail-mounts-the-work-and-nothing-of-the-home ()
  (let ((argv (sandbox-test-argv)))
    (is (sandbox-test-mount argv "--bind" "/w/proj") "the folder is written")
    (is (sandbox-test-mount argv "--bind" "/h/.nodecode") "so is the home")
    (is (sandbox-test-mount argv "--ro-bind" "/opt/nc") "the binary's tree is read only")
    (is (sandbox-test-mount argv "--ro-bind" "/usr") "the system is read only")
    (is (sandbox-test-mount argv "--symlink" "usr/bin") "a usr-merged /bin is a link")
    (is (sandbox-test-mount argv "--tmpfs" "/tmp") "the jail's /tmp is its own")
    (let ((mounted (remove-if (lambda (word) (uiop:string-prefix-p "PATH" word)) argv)))
      (is (not (find-if (lambda (word) (uiop:string-prefix-p "/home/u" word)) mounted)) "nothing of the user's home is mounted"))
    (is (not (find "/run" argv :test #'string=)) "nor /run, with the user's sockets")
    (let ((root (position "--remount-ro" argv :test #'string=)))
      (is (and root (equal "/" (nth (1+ root) argv))) "the root bwrap builds on is read-only")
      (is (< (sandbox-test-mount argv "--bind" "/w/proj") root) "after the mounts"))
    (is (equal "/w/proj" (second (member "--chdir" argv :test #'string=))) "it works in the folder")))

(deftest sandbox-jail-never-takes-a-home-as-its-folder ()
  (let ((homes '("/home/u/")))
    (is (null (nle::sandbox-work-folder "/home/u/" homes)) "the user's home is no work folder")
    (is (null (nle::sandbox-work-folder "/home/" homes)) "nor a directory holding it")
    (is (null (nle::sandbox-work-folder "/" homes)) "nor the root")
    (is (equal "/home/u/proj" (nle::sandbox-work-folder "/home/u/proj/" homes)) "a project in it is"))
  (let ((argv (sandbox-test-argv :folder "/home/u/")))
    (is (not (sandbox-test-mount argv "--bind" "/home/u")) "launched in the home, the jail gets no folder")
    (is (equal "/h/.nodecode" (second (member "--chdir" argv :test #'string=))) "and works in its own home")))

(deftest sandbox-jail-hides-the-keys-and-guards-the-launchers-files ()
  (let* ((argv (sandbox-test-argv :hidden '("/h/.nodecode/auth.json" "/h/.nodecode/secrets/"
                                            "/h/.nodecode/secrets/providers.x.api_key" "/h/.nodecode/backups/")
                                  :directories '("/h/.nodecode/secrets" "/h/.nodecode/backups")))
         (home (sandbox-test-mount argv "--bind" "/h/.nodecode")))
    (is (search '("--ro-bind" "/dev/null" "/h/.nodecode/auth.json") argv :test #'equal) "auth.json reads as nothing")
    (is (search '("--tmpfs" "/h/.nodecode/secrets" "--remount-ro" "/h/.nodecode/secrets") argv :test #'equal) "secrets/ is an empty read-only directory")
    (is (not (find "/h/.nodecode/secrets/providers.x.api_key" argv :test #'string=)) "a key inside it is left to that")
    (is (search '("--tmpfs" "/h/.nodecode/backups") argv :test #'equal) "the backups (each the home whole) are hidden")
    (dolist (file '("/h/.nodecode/config.jsonc" "/h/.nodecode/sandbox.jsonc" "/h/.nodecode/sandbox/jailed.json"))
      (let ((at (sandbox-test-mount argv "--ro-bind" file)))
        (is (and at (< home at)) (format nil "~a is read-only, over the written home" file))))
    (is (< home (search '("--ro-bind" "/dev/null" "/h/.nodecode/auth.json") argv :test #'equal)) "the hiding goes over the home")))

(deftest sandbox-jail-keeps-a-binary-in-its-folder-read-only ()
  ;; ./nodecode --sandbox in a checkout: the folder is written, the binary in
  ;; it is not, and the rest of the checkout stays the folder's.
  (let ((argv (sandbox-test-argv :folder "/w/proj/" :binary "/w/proj/nodecode")))
    (is (not (sandbox-test-mount argv "--ro-bind" "/w/proj")) "the folder is not made read-only")
    (is (< (sandbox-test-mount argv "--bind" "/w/proj") (sandbox-test-mount argv "--ro-bind" "/w/proj/nodecode")) "the binary alone is, after it")))

(deftest sandbox-jail-merges-the-operators-policy ()
  (let ((argv (sandbox-test-argv :policy '(:allow-write ("/data") :allow-read ("/srv/docs") :deny-read ("/w/proj/.env")))))
    (is (sandbox-test-mount argv "--bind" "/data") "allow_write is written")
    (is (sandbox-test-mount argv "--ro-bind" "/srv/docs") "allow_read is read")
    (is (search '("--ro-bind" "/dev/null" "/w/proj/.env") argv :test #'equal) "deny_read inside the folder is hidden")))

(deftest sandbox-jail-gets-only-the-safe-environment ()
  (let ((argv (sandbox-test-argv :environment '("PATH=/usr/bin:/home/u/bin:/opt/nc" "HOME=/home/u" "NODECODE_HOME=/h/.nodecode"
                                                "NODECODE_API_KEY=sk-x" "OPENAI_API_KEY=sk-y" "SSH_AUTH_SOCK=/run/user/1/ssh"
                                                "NODECODE_PRISTINE=1" "LC_ALL=C"))))
    (dolist (name '("NODECODE_API_KEY" "OPENAI_API_KEY" "SSH_AUTH_SOCK" "NODECODE_PRISTINE"))
      (is (search (list "--unsetenv" name) argv :test #'equal) (format nil "~a is unset" name)))
    (dolist (name '("NODECODE_HOME" "HOME" "LC_ALL"))
      (is (not (search (list "--unsetenv" name) argv :test #'equal)) (format nil "~a is kept" name)))
    (is (equal "/usr/bin:/opt/nc:/usr/local/bin:/bin:/usr/local/sbin:/usr/sbin" (sandbox-test-setenv argv "PATH")) "PATH keeps what is mounted")
    (is (equal "1" (sandbox-test-setenv argv "NODECODE_SANDBOXED")))
    (is (equal "41234" (sandbox-test-setenv argv "NODECODE_GATEWAY_PORT")) "the gateway's port on the jail's loopback")
    (is (equal "3" (sandbox-test-setenv argv "NODECODE_SANDBOX_INBOUND_FD")) "the way in")
    (is (equal "4" (sandbox-test-setenv argv "NODECODE_SANDBOX_EGRESS_FD")) "the way out")
    (is (equal "http://127.0.0.1:3128" (sandbox-test-setenv argv "HTTPS_PROXY")) "web traffic goes to the proxy")
    (is (search "127.0.0.1" (sandbox-test-setenv argv "NO_PROXY")) "the jail's own loopback does not")
    (is (equal "/h/.nodecode/sandbox/tmp" (sandbox-test-setenv argv "TMPDIR")))))

(deftest sandbox-bpf-assembles-jumps-by-label ()
  (let ((octets (nle::sandbox-bpf-assemble '((:ld 4) (:jeq 7 :yes nil) (:ret 0) :yes (:ret 1)))))
    (is (= 32 (length octets)) "four instructions, 8 octets each")
    (is (equalp #(#x20 0 0 0 4 0 0 0) (subseq octets 0 8)) "ld [4]")
    (is (equalp #(#x15 0 1 0 7 0 0 0) (subseq octets 8 16)) "jeq 7, then over the next one")
    (is (equalp #(6 0 0 0 1 0 0 0) (subseq octets 24 32)) "ret 1")))

(deftest sandbox-seccomp-program-for-each-machine ()
  (dolist (machine '(:x86-64 :arm64))
    (let ((octets (nle::sandbox-seccomp-program machine)))
      (is (zerop (mod (length octets) 8)) (format nil "~a: whole instructions" machine))
      (is (< (length octets) (* 8 64)) "short")))
  (is (equalp (nle::sandbox-seccomp-program) (nle::sandbox-seccomp-program (nle::sandbox-machine))) "this machine's by default"))

#+linux
(deftest sandbox-seccomp-refuses-unix-sockets-under-bwrap ()
  ;; The filter as the jail gets it, bwrap reading it from a descriptor this
  ;; process hands its child: a unix socket is refused, a TCP one is not.
  (when (and (nlk::executable-on-path "bwrap") (nlk::executable-on-path "python3")
             (nle::sandbox-bwrap-works-p (uiop:native-namestring (nlk::executable-on-path "bwrap"))))
    (let ((seccomp (nle::sandbox-seccomp-descriptor)))
      (unwind-protect
           (multiple-value-bind (pid output errors process)
               (nlk:spawn-program (list "bwrap" "--ro-bind" "/" "/" "--unshare-net" "--seccomp" "3" "python3" "-c"
                                        "import socket
try:
  socket.socket(socket.AF_UNIX); print('unix-made')
except OSError as e: print('unix-refused', e.errno)
socket.socket(socket.AF_INET); print('inet-made')")
                                  :output :stream :error-output :output :descriptors (list seccomp))
             (declare (ignore errors))
             (let ((text (uiop:slurp-stream-string output)))
               (is (eql 0 (nlk:wait-for-child pid process)) text)
               (is (search "unix-refused 1" text) "socket(AF_UNIX) answers EPERM")
               (is (search "inet-made" text) "socket(AF_INET) is made")))
        (ignore-errors (sb-posix:close seccomp))))))

#+linux
(deftest sandbox-descriptors-only-in-a-sandboxed-shell ()
  (let ((nle::*sandbox-plan* nil))
    (is (eq :plain (nle::call-with-sandbox-descriptors (lambda (descriptors) (if descriptors :jail :plain)))) "outside a sandboxed shell there are none"))
  (let* ((port (nle::local-port 0))
         (nle::*sandbox-plan* (list :port port :hosts '()))
         (seen nil))
    (nle::call-with-sandbox-descriptors (lambda (descriptors)
                                          (setf seen descriptors)
                                          (is (sandbox-port-taken-p port) "while the function runs the listener holds the port")))
    (is (and (= 3 (length seen)) (every #'integerp seen)) "the way in, the way out and the filter")
    (is (eql port (nle::local-port port)) "and the shell's copy is closed once the function returns: the jail alone holds it")))

(deftest sandbox-launch-makes-the-files-the-launcher-reads ()
  ;; A home with no config.jsonc: the jail could create it, and the next
  ;; launch would read the jail's providers; the launcher makes both first.
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

(deftest sandbox-wrap-command-is-the-argv-itself-outside-a-sandbox ()
  (let ((nle::*sandbox-plan* nil))
    (is (equal '("nodecode" "serve") (nle::sandbox-wrap-command '("nodecode" "serve"))))))

(deftest sandbox-flag-is-a-shells (let ((nle::*offer-sandbox* t)))
  (let ((nle::*sandbox-asked* nil))
    (nle::parse-launch-arguments '("--sandbox"))
    (is (eq t nle::*sandbox-asked*) "--sandbox is read"))
  (let ((nle::*sandbox-asked* nil))
    (is (search "shell's flag" (refusal-text nle::launch-usage-error
                                 (nle::parse-launch-arguments '("serve" "--sandbox")))) "a serve under --sandbox is refused"))
  (let ((nle::*sandbox-asked* nil))
    (is (search "shell's flag" (refusal-text nle::launch-usage-error
                                 (nle::parse-launch-arguments '("--sandbox" "rpc")))) "so is rpc")))

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

(defun sandbox-commit (home message &key init)
  "Commit everything in HOME's cells/vise/ as MESSAGE, a repository made first under INIT."
  ;; git's own argv, no shell around it: the test runs where git does.
  (let ((directory (merge-pathnames "cells/vise/" home)))
    (flet ((git (&rest arguments)
             (uiop:run-program (append (list "git") *fixture-git-config* arguments) :directory directory)))
      (when init (git "init" "-q"))
      (git "add" "-A")
      (git "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" message))))

(defun sandbox-test-home ()
  "A scratch home with one cells/ folder under git, one commit in it."
  (let ((home (temp-path "sbx")))
    (ensure-directories-exist (merge-pathnames "cells/vise/" home))
    (with-open-file (o (merge-pathnames "cells/vise/a.lisp" home) :direction :output :if-exists :supersede)
      (write-line "(defun a () 1)" o))
    (sandbox-commit home "one" :init t)
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
             (sandbox-commit home "escape")
             (nle::sandbox-mark home)
             (is (equal first (uiop:read-file-string (nle::sandbox-mark-path home))) "a later sandboxed launch never moves the mark")))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest a-fixture-commit-starts-no-git-maintenance-to-outlive-it ()
  ;; The test above deletes its home right after a commit: a detached `git
  ;; maintenance run' still holding objects/maintenance.lock failed that delete.
  (with-temp-trees ((home "sbx-home") (traces "sbx-trace"))
    (let ((trace (merge-pathnames "trace2.log" (ensure-directories-exist traces))))
      (ensure-directories-exist (merge-pathnames "cells/vise/" home))
      (alexandria:write-string-into-file "(defun a () 1)" (merge-pathnames "cells/vise/a.lisp" home))
      (with-env-var ("GIT_TRACE2" (uiop:native-namestring trace))
        (sandbox-commit home "one" :init t))
      (is (search "commit" (uiop:read-file-string trace)) "the commit ran under the trace")
      (is (not (git-trace-starts-maintenance-p trace)) "and started no maintenance run that outlives it"))))

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
           (sandbox-commit home "escape-commit")
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

