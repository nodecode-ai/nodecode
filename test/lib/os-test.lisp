;;;; os-test.lisp --- the operating-system seam: the shell, bounded runs,
;;;; locks, the atomic replace, pid liveness.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every primitive here has a POSIX arm and a Windows arm behind one name;
;;;; these gates pin the contract the callers rely on, and run on whichever
;;;; arm the suite's host has. The cross-process cases use a python child the
;;;; way the store lease tests do: a lock is a per-process fact, so only a
;;;; second process can hold one against this image.

(in-package #:nodecode.test)

(defun run-sh (command &rest keys)
  "RUN-BOUNDED over COMMAND through sh, KEYS ahead of a ten-second bound."
  (apply #'nlk:run-bounded (nlk:shell-argv command) (append keys '(:seconds 10))))

(deftest shell-argv-runs-a-command-string-through-sh ()
  (let ((argv (nlk:shell-argv "echo hi")))
    (is (= 3 (length argv)) "program, flag, command")
    (is (search "sh" (first argv)) "the program is a POSIX sh")
    (is (equal "-c" (second argv)) "a plain command is sh -c")
    (is (equal "echo hi" (third argv)) "the command rides verbatim"))
  (is (equal "-lc" (second (nlk:shell-argv "true" :login t))))
  (let ((nlk:*shell* nil))
    (is (signals-error error (nlk:shell-argv "true")))))

;;; Git for Windows past its redirectors (T-442). The layouts are scratch trees
;;; of empty files: what is under test is which file a redirector would start
;;; and the environment it would hand it, which a Linux suite reads as well as
;;; a Windows one; the Windows release smokes start the real programs.

(defun git-layout (root &key (toolchain "mingw64") (bash t))
  "Make ROOT a Git for Windows tree of empty files: its redirectors and, BASH
and TOOLCHAIN willing, the programs they start."
  (dolist (file (append (list "bin/sh.exe" "bin/bash.exe" "bin/git.exe" "cmd/git.exe")
                        (when toolchain (list (format nil "~a/bin/git.exe" toolchain)))
                        (when bash (list "usr/bin/bash.exe" "usr/bin/sh.exe"))))
    (write-temp-file (merge-pathnames file root)))
  root)

(defun environment-value (name environment) "NAME's value in ENVIRONMENT, \"NAME=value\" strings, or NIL."
  (let ((entry (find name environment :key #'nlk::environment-entry-name :test #'string-equal)))
    (and entry (subseq entry (1+ (length name))))))

(deftest git-redirector-names-the-program-a-redirector-starts ()
  (with-temp-workspace (dir)
    (let* ((root (nlk::real-path (git-layout (merge-pathnames "Git/" dir))))
           (bash (uiop:native-namestring (merge-pathnames "usr/bin/bash.exe" root)))
           (git (uiop:native-namestring (merge-pathnames "mingw64/bin/git.exe" root))))
      (dolist (shell '("bin/sh.exe" "bin/bash.exe"))
        (let ((found (nlk::git-redirector (merge-pathnames shell root))))
          (is (eq :shell (getf found :kind)) "bin's sh.exe and bash.exe are compat-bash")
          (is (equal bash (getf found :program)) "compat-bash starts usr/bin/bash.exe, not sh.exe")
          (is (equal "MINGW64" (getf found :msystem)))))
      (dolist (wrapper '("cmd/git.exe" "bin/git.exe"))
        (let ((found (nlk::git-redirector (merge-pathnames wrapper root))))
          (is (eq :git (getf found :kind)))
          (is (equal git (getf found :program)) "cmd/git.exe starts the toolchain's git.exe")))
      (dolist (real '("usr/bin/sh.exe" "usr/bin/bash.exe" "mingw64/bin/git.exe"))
        (is (null (nlk::git-redirector (merge-pathnames real root))) "a real program is no redirector: it starts as it is"))
      (is (null (nlk::git-redirector (merge-pathnames "bin/nothing.exe" root))))
      (is (null (nlk::git-redirector nil))))
    (let ((arm (git-layout (merge-pathnames "Arm/" dir) :toolchain "clangarm64")))
      (is (equal "CLANGARM64" (getf (nlk::git-redirector (merge-pathnames "cmd/git.exe" arm)) :msystem)) "an ARM64 Git names its own toolchain"))
    (let ((bare (git-layout (merge-pathnames "Bare/" dir) :bash nil)))
      (is (null (nlk::git-redirector (merge-pathnames "bin/sh.exe" bare))) "no usr/bin/bash.exe: the shell keeps its redirector")
      (is (eq :git (getf (nlk::git-redirector (merge-pathnames "cmd/git.exe" bare)) :kind)) "git needs only its toolchain"))
    (let ((none (git-layout (merge-pathnames "None/" dir) :toolchain nil)))
      (is (null (nlk::git-redirector (merge-pathnames "bin/sh.exe" none))) "no toolchain git: not a Git for Windows layout"))))

(deftest git-redirector-follows-a-scoop-shim ()
  (with-temp-workspace (dir)
    (let* ((root (git-layout (merge-pathnames "apps/git/current/" dir)))
           (target (uiop:native-namestring (merge-pathnames "bin/sh.exe" root)))
           (shim (merge-pathnames "shims/sh.exe" dir)))
      (write-temp-file shim)
      (write-temp-file (merge-pathnames "shims/sh.shim" dir)
                       (format nil "path = \"~a\"~c~%args = --login~%" target #\Return))
      (is (equal target (uiop:native-namestring (nlk::shim-target shim))) "the shim names its target on its path line")
      (let ((found (nlk::git-redirector shim)))
        (is (eq :shell (getf found :kind)))
        (is (equal target (getf found :redirector)) "the redirector is the shim's target, the path it names")
        (is (equal (uiop:native-namestring (merge-pathnames "usr/bin/bash.exe" (nlk::real-path root))) (getf found :program)) "three processes become one"))
      (is (null (nlk::shim-target (merge-pathnames "bin/sh.exe" root))) "a program with no .shim beside it is no shim")
      (write-temp-file (merge-pathnames "shims/gone.exe" dir))
      (write-temp-file (merge-pathnames "shims/gone.shim" dir) "path = C:\\nowhere\\bin\\sh.exe")
      (is (null (nlk::git-redirector (merge-pathnames "shims/gone.exe" dir))) "a shim whose target is missing changes nothing"))))

#-win32
(deftest git-redirector-reads-the-layout-where-it-really-lies ()
  ;; scoop's `current' is a junction to the version folder; the redirector
  ;; reads its own real path, so MSYS's / is the version folder, and the path
  ;; the shim names is what outlives that folder.
  (with-temp-workspace (dir)
    (let* ((version (git-layout (merge-pathnames "apps/git/2.51.0/" dir)))
           (current (merge-pathnames "apps/git/current" dir)))
      (sb-posix:symlink (uiop:native-namestring (nlk::native-folder version)) (uiop:native-namestring current))
      (let* ((named (merge-pathnames "apps/git/current/bin/sh.exe" dir))
             (found (nlk::git-redirector named))
             (real (nlk::real-path version)))
        (is (equal (uiop:native-namestring (merge-pathnames "usr/bin/bash.exe" real)) (getf found :program)) "the program is under the real root")
        (is (equal (uiop:native-namestring (merge-pathnames "bin/" real)) (uiop:native-namestring (getf found :folder))) "EXEPATH's folder is the real one")
        (is (equal (uiop:native-namestring named) (getf found :redirector)) "the redirector is the path it was named by")))))

(deftest git-redirector-environment-is-the-wrappers ()
  ;; setup_environment and maybe_read_config in git-wrapper.c, entry by entry.
  (with-temp-workspace (dir "Users/me/keep")
    (let* ((root (git-layout (merge-pathnames "Git/" dir)))
           (shell (nlk::git-redirector (merge-pathnames "bin/sh.exe" root)))
           (git (nlk::git-redirector (merge-pathnames "cmd/git.exe" root)))
           (top (nlk::native-folder (nlk::real-path root)))
           (profile (nlk::native-folder (merge-pathnames "Users/me/" dir)))
           (base (list "PATH=C:\\Windows" "MSYSTEM=MSYS" "USERPROFILE=C:\\Users\\me" "OTHER=1")))
      (let ((env (nlk::git-redirector-environment shell base)))
        (is (equal "MINGW64" (environment-value "MSYSTEM" env)) "MSYSTEM is set whatever was inherited")
        (is (equal "ssh" (environment-value "PLINK_PROTOCOL" env)))
        (is (equal "C:\\Users\\me" (environment-value "HOME" env)) "no HOMEPATH: HOME is USERPROFILE")
        (is (equal (format nil "~a\\mingw64\\bin;~a\\usr\\bin;C:\\Users\\me\\bin;C:\\Windows" top top)
                   (environment-value "PATH" env)) "PATH: the toolchain, usr/bin and HOME's bin lead what was there")
        (is (equal (nlk::native-folder (merge-pathnames "bin/" (nlk::real-path root))) (environment-value "EXEPATH" env)) "compat-bash sets EXEPATH to its own folder")
        (is (equal "1" (environment-value "OTHER" env)) "the rest rides along")
        (is (= 1 (count "MSYSTEM" env :key #'nlk::environment-entry-name :test #'string-equal))))
      (let ((env (nlk::git-redirector-environment git (list* "HOME=D:\\h\\" "PLINK_PROTOCOL=plink" base))))
        (is (equal "D:\\h\\" (environment-value "HOME" env)) "a HOME already set stays")
        (is (equal "plink" (environment-value "PLINK_PROTOCOL" env)))
        (is (search ";D:\\h\\bin;" (environment-value "PATH" env)) "HOME's bin with no doubled separator")
        (is (null (environment-value "EXEPATH" env)) "cmd/git.exe sets no EXEPATH"))
      (let ((env (nlk::git-redirector-environment
                  git (list* "HOMEDRIVE=" (format nil "HOMEPATH=~a" profile) base))))
        (is (equal profile (environment-value "HOME" env)) "HOMEDRIVE and HOMEPATH, when they name a folder"))
      (let ((env (nlk::git-redirector-environment
                  git (list* "HOMEDRIVE=" "HOMEPATH=/no/such/folder" base))))
        (is (equal "C:\\Users\\me" (environment-value "HOME" env)) "else USERPROFILE"))
      (write-temp-file (merge-pathnames "etc/git-bash.config" root)
                       (format nil "MSYS=winsymlinks:nativestrict~c~%OTHER=no~%" #\Return))
      (let ((env (nlk::git-redirector-environment shell (list* "MSYS=enable_pcon" base))))
        (is (equal "winsymlinks:nativestrict enable_pcon" (environment-value "MSYS" env)) "git-bash.config's MSYS leads the inherited one")))))

(deftest past-git-redirector-starts-the-real-program ()
  (with-temp-workspace (dir)
    (let* ((root (git-layout (merge-pathnames "Git/" dir)))
           (shell (nlk::git-redirector (merge-pathnames "bin/sh.exe" root)))
           (git (nlk::git-redirector (merge-pathnames "cmd/git.exe" root)))
           (base (list "PATH=C:\\Windows")))
      (let ((nlk::*git-shell* shell) (nlk::*git-direct* git))
        (multiple-value-bind (argv env) (nlk::past-git-redirector (list (getf shell :program) "-c" "true") base)
          (is (equal (list (getf shell :program) "-c" "true") argv) "the shell's argv stands")
          (is (equal "MINGW64" (environment-value "MSYSTEM" env)) "with the redirector's environment"))
        (multiple-value-bind (argv env) (nlk::past-git-redirector (list "git" "status") base)
          (is (equal (list (getf git :program) "status") argv) "a child named git is the toolchain's")
          (is (equal "MINGW64" (environment-value "MSYSTEM" env))))
        (multiple-value-bind (argv env) (nlk::past-git-redirector (list "powershell" "-c" "x") base)
          (is (equal (list "powershell" "-c" "x") argv))
          (is (eq base env) "anything else is untouched"))
        (delete-file (merge-pathnames "usr/bin/bash.exe" root))
        (delete-file (merge-pathnames "mingw64/bin/git.exe" root))
        (is (equal (list (getf shell :redirector) "-c" "true")
                   (nlk::past-git-redirector (list (getf shell :program) "-c" "true") base)) "a vanished bash: the redirector it was found by")
        (is (equal (list "git" "status") (nlk::past-git-redirector (list "git" "status") base)) "a vanished git: PATH's own"))
      (let ((nlk::*git-shell* nil) (nlk::*git-direct* nil))
        (is (equal (list "git" "status") (nlk::past-git-redirector (list "git" "status") base)) "no Git for Windows found: nothing changes")))))

(deftest session-argv-wraps-only-where-sessions-exist ()
  (let ((argv (nlk:session-argv (list "prog" "arg"))))
    #+linux (is (equal (list "setsid" "-w" "prog" "arg") argv))
    #+darwin (is (and (equal "/usr/bin/perl" (first argv))
                      (equal (list "--" "prog" "arg") (last argv 3))))
    #-(or linux darwin) (is (equal (list "prog" "arg") argv)))
  (let ((nlk:*session-wrapper* nil))
    (is (equal (list "prog" "arg") (nlk:session-argv (list "prog" "arg"))))))

#-win32
(deftest session-argv-child-leads-a-session-and-answers-its-status ()
  ;; Through whichever wrapper the host has: the child's output and exit
  ;; status come back unchanged, and `ps' reads its session as its own.
  (multiple-value-bind (output errors status)
      (nlk:run-bounded (nlk:session-argv (nlk:shell-argv "echo led; echo err 1>&2; exit 4"))
                       :seconds 20)
    (is (search "led" output) "stdout rides through the wrapper")
    (is (search "err" errors) "so does stderr")
    (is (eql 4 status) "and the exit status is the child's own"))
  (when nlk:*session-wrapper*
    ;; procps prints a session id, so on Linux the shell can see it leads
    ;; its own; BSD ps has none (its session pointer reads the same for
    ;; every process on a kernel that hides its addresses) and perl's
    ;; POSIX no getsid, so on macOS python3 reads it for the child.
    #+linux
    (nlk:bind (((output _ status)
                (nlk:run-bounded (nlk:session-argv
                                  (nlk:shell-argv
                                   "test \"$(ps -o sid= -p $$ | tr -d ' ')\" = \"$$\" && echo own || echo shared"))
                                 :seconds 20)))
      (is (and (eql 0 status) (search "own" output))))
    #-linux
    (if (nlk::executable-on-path "python3")
        (is (eql 0 (nth-value 2 (nlk:run-bounded
                                 (nlk:session-argv
                                  (nlk:shell-argv
                                   "python3 -c 'import os,sys; sys.exit(0 if os.getsid(0) == int(sys.argv[1]) else 3)' $$"))
                                 :seconds 20))))
        (is t "no python3 on this box to witness the session"))))

(deftest platform-is-one-word ()
  (is (eq (nlk:platform) #+win32 :windows #+darwin :darwin #+linux :linux
                         #-(or win32 darwin linux) :unix)))

(deftest architecture-is-one-word ()
  ;; uiop:architecture answers NIL on arm64 SBCL; the kernel's read must not.
  (is (eq (nlk:architecture) #+x86-64 :x64 #+arm64 :arm64)))

(deftest run-bounded-captures-both-streams-and-the-status ()
  (multiple-value-bind (output errors status) (run-sh "echo out; echo err 1>&2; exit 3")
    (is (equal (format nil "out~%") output) "stdout is the first value")
    (is (equal (format nil "err~%") errors) "stderr is the second")
    (is (eql 3 status) "the exit status is the third")))

(deftest a-childs-stderr-appends-to-a-file-and-its-exit-reads-without-a-wait
    (with-temp-workspace (dir))
  ;; The MCP cell's servers: stderr kept in a log that outlives each
  ;; launch, the exit asked for while the transport polls.
  (let ((log (merge-pathnames "child.log" dir)))
    (write-temp-file log (format nil "before~%"))
    (multiple-value-bind (pid output errors process)
        (nlk:spawn-program (nlk:shell-argv "echo out; echo err 1>&2; exit 3")
                           :error-output log)
      (nlk:with-cleanup ((close output))
        (is (null errors) "no stderr stream: it went to the file")
        (is (equal "out" (read-line output)))
        (is (eql 3 (await (:tries 500 :rest 0.01) (nlk:child-exit pid process))))
        (is (equal (format nil "before~%err~%") (uiop:read-file-string log)) "appended")))))

(deftest a-childs-output-lands-in-a-file-with-its-errors-joined (with-temp-workspace (dir))
  ;; SPAWN-DETACHED's wrapper: both streams in one log it creates.
  (let ((log (merge-pathnames "child.log" dir)))
    (multiple-value-bind (pid output errors process)
        (nlk:spawn-program (nlk:shell-argv "echo out; echo err 1>&2")
                           :output log :error-output :output)
      (is (and (null output) (null errors)) "no pipes: both went to the file")
      (is (eql 0 (nlk:wait-for-child pid process)))
      (is (equal (format nil "out~%err~%") (uiop:read-file-string log)) "created, in order"))))

#-win32
(deftest a-descriptor-of-this-process-can-be-a-childs-stdout (with-temp-workspace (dir))
  ;; A sandboxed shell hands its jailed serve a listening socket as stdout
  ;; (the one descriptor besides stdin and stderr srt passes through): the
  ;; child's stdout is the descriptor itself, its stderr the log alone.
  (let ((log (merge-pathnames "child.log" dir))
        (file (merge-pathnames "stdout.txt" dir)))
    (let ((descriptor (sb-posix:open (uiop:native-namestring file)
                                     (logior sb-posix:o-wronly sb-posix:o-creat) #o600)))
      (unwind-protect
           (multiple-value-bind (pid output errors process)
               (nlk:spawn-program (nlk:shell-argv "echo out; echo err 1>&2")
                                  :output descriptor :error-output log)
             (is (and (null output) (null errors)) "no pipes")
             (is (eql 0 (nlk:wait-for-child pid process))))
        (sb-posix:close descriptor)))
    (is (equal (format nil "out~%") (uiop:read-file-string file)) "stdout went down the descriptor")
    (is (equal (format nil "err~%") (uiop:read-file-string log)) "stderr to the log")
    (let ((descriptor (sb-posix:open (uiop:native-namestring file)
                                     (logior sb-posix:o-wronly sb-posix:o-trunc) #o600)))
      (unwind-protect
           (is (nlk:spawn-detached (nlk:shell-argv "echo detached-out; echo detached-err 1>&2") log :stdout descriptor))
        (sb-posix:close descriptor)))
    (is (await (:rest 0.05) (search "detached-out" (or (ignore-errors (uiop:read-file-string file)) ""))) "a detached child's stdout goes down the descriptor too")
    (is (search "detached-err" (uiop:read-file-string log)) "and its stderr to the log")))

(deftest a-spawn-whose-log-folder-is-missing-names-the-log (with-temp-workspace (dir))
  ;; Opening the log fails with ENOENT, the errno a missing program gives:
  ;; `nodecode rpc' on a home never made said `spawning setsid: No such file
  ;; or directory', naming the one thing that was there.
  (let* ((log (merge-pathnames "never-made/child.log" dir))
         (said (handler-case (progn (nlk:spawn-program (nlk:shell-argv "true")
                                                       :output log :error-output :output)
                                    nil)
                 (error (condition) (princ-to-string condition)))))
    (is (and said (search (uiop:native-namestring log) said)) (format nil "named: ~a" said))))

#-win32
(deftest a-child-that-leaves-a-daemon-behind-is-answered-at-its-own-exit ()
  ;; A clipboard helper forks the owner that serves the selection, then
  ;; exits. Neither output a pipe, the owner holds nothing this image reads,
  ;; and the call returns with the helper rather than waiting out a drain.
  (let ((started (get-internal-real-time)))
    (multiple-value-bind (output errors status)
        (run-sh "sleep 5 & echo gone" :output nil :error-output nil)
      (is (and (null output) (null errors)) "both streams went to the null device")
      (is (eql 0 status) "the helper's own exit"))
    (is (nlk:exits-zero-p (nlk:shell-argv "sleep 5 & exit 0")) "EXITS-ZERO-P reads neither")
    (is (< (- (get-internal-real-time) started) (* 3 internal-time-units-per-second)))))

(deftest run-bounded-kills-the-child-at-the-bound (let ((started (get-internal-real-time))))
  (nlk:bind (((output _ status) (run-sh "echo begun; sleep 20; echo never" :seconds 0.5)))
    (is (eq :timeout status) "the status names the bound")
    (is (search "begun" output) "output before the kill is kept")
    (is (not (search "never" output)) "nothing after it")
    (is (< (- (get-internal-real-time) started)
           (* 5 internal-time-units-per-second)))))

(deftest a-child-is-given-its-descriptors-and-nothing-else ()
  ;; The child gets a descriptor plan, never a copy of this image's table:
  ;; stdin the null device, stdout and stderr each its own pipe, and every
  ;; descriptor this image holds above stderr closed. A store handle or a
  ;; provider socket riding into a git call would hold the file open and
  ;; travel where it was never meant to.
  (with-open-file (held "/dev/null" :direction :input)
    (declare (ignorable held))
    #+linux
    (nlk:bind (((output _ status) (run-sh "ls /proc/self/fd | tr '\n' ' '; cat")))
      (is (eql 0 status) "the shell ran and its stdin ended at once")
      (let ((descriptors (nlk:split-words output)))
        ;; 0, 1, 2 and the descriptor `ls' itself opened on the directory.
        (is (<= (length descriptors) 4)
            (format nil "the child holds only its own: ~a" descriptors))))
    #-linux
    (nlk:bind (((output _ status) (run-sh "cat; echo ended")))
      (is (eql 0 status) "the child ran")
      (is (search "ended" output) "and its stdin was the null device, not a wait"))))

#-win32
(deftest a-bound-that-falls-takes-the-shells-background-work-with-it ()
  ;; The child leads a process group of its own, so the kill at the bound
  ;; takes the tree with it. A backgrounded sleep outliving the shell would
  ;; hold the pipe open, and the call would wait the drain's own bound out
  ;; instead of returning with the kill.
  (let ((started (get-internal-real-time)))
    (nlk:bind (((output _ status) (run-sh "sleep 20 & echo started; sleep 20" :seconds 0.5)))
      (is (eq :timeout status) "the bound fell")
      (is (search "started" output) "the shell had begun")
      (is (< (- (get-internal-real-time) started)
             (* 3 internal-time-units-per-second))))))

(deftest run-bounded-lays-the-environment-over-the-inherited-one ()
  (nlk:bind (((output _ status)
              (run-sh "printf '%s|%s' \"$NODECODE_OS_TEST\" \"$PATH\""
                      :environment (list "NODECODE_OS_TEST=laid over"))))
    (is (eql 0 status) "the shell ran")
    (is (uiop:string-prefix-p "laid over|" output) "the override is seen")
    (is (> (length output) (length "laid over|")) "PATH still rides along")))

(deftest run-bounded-runs-in-the-named-directory (with-temp-workspace (dir))
  (nlk:bind (((output _ status) (run-sh "pwd" :directory dir)))
    (is (eql 0 status) "pwd ran")
    (is (equal (namestring (truename dir))
               (namestring (uiop:ensure-directory-pathname
                            (string-right-trim '(#\Newline #\Return) output)))))))

(deftest environment-with-replaces-every-entry-of-an-overridden-name ()
  (let ((environment (nlk:environment-with (list "PATH=/nowhere" "NODECODE_OS_NEW=1"))))
    (is (equal "PATH=/nowhere" (find "PATH=" environment :test #'uiop:string-prefix-p)))
    (is (= 1 (count "PATH=" environment :test #'uiop:string-prefix-p)))
    (is (member "NODECODE_OS_NEW=1" environment :test #'equal) "a new name is added")
    (is (find "HOME=" environment :test #'uiop:string-prefix-p))))

(deftest environment-with-removes-a-bare-name ()
  (let ((environment (nlk:environment-with (list "NODECODE_OS_GONE")
                                           (list "PATH=/bin" "NODECODE_OS_GONE=1" "NODECODE_OS_GONE_NOT=2"))))
    (is (equal '("PATH=/bin" "NODECODE_OS_GONE_NOT=2") environment) "the name goes, a longer one stays, the bare name is not added")))

(defun child-read (name &rest keys)
  "NAME's value as a child RUN-BOUNDED starts with KEYS reads it."
  ;; On Windows as its UTF-16 code units, numbers no console code page can bend.
  (let ((output (string-right-trim
                 '(#\Return #\Newline)
                 (apply #'nlk:run-bounded
                        #-win32 (list "printenv" name)
                        #+win32 (list "powershell" "-NoProfile" "-Command"
                                      (format nil "[string]::Join(' ', [int[]][char[]]$env:~a)" name))
                        :seconds 30 keys))))
    #-win32 output
    #+win32 (map 'string (lambda (unit) (code-char (parse-integer unit)))
                 (uiop:split-string output :separator " "))))

;; A value outside ASCII, and outside every Windows ANSI code page, crosses into
;; a child exactly: inherited, or handed in the environment the child is given.
(deftest a-child-reads-a-value-outside-ascii-exactly ()
  (let ((value (map 'string #'code-char '(#x141 #x75 #x6B #x61 #x73 #x7A #x2D #x4A #xFC
                                          #x72 #x67 #x65 #x6E #x2D #x65E5))))
    (nlk::set-environment-variable "NODECODE_OS_WIDE_SET" value)
    (let ((entry (format nil "NODECODE_OS_WIDE_SET=~a" value))
          (handed (list (format nil "NODECODE_OS_WIDE=~a" value))))
      (is (member entry (nlk:process-environment) :test #'equal) "this process reads back what it set")
      (is (equal value (child-read "NODECODE_OS_WIDE_SET")) "a child that inherits it")
      (is (equal value (child-read "NODECODE_OS_WIDE_SET" :environment handed)) "a child handed this process's environment")
      (is (equal value (child-read "NODECODE_OS_WIDE" :environment handed)) "a child handed the value itself"))))

(defmacro with-lock-holder ((path &optional (text "")) &body body
                           &aux (holder (gensym "HOLDER")))
  "Run BODY while a python3 child holds an exclusive lock on PATH — the
foreign holder a per-process lock needs — after asserting it took the lock,
and end the child on unwind."
  ;; TEXT is written into the locked file first: what a holder says of itself.
  `(let ((,holder (uiop:launch-program
                   (list "python3" "-c"
                         "import fcntl,sys,time
f=open(sys.argv[1],'w')
f.write(sys.argv[2]); f.flush()
fcntl.lockf(f,fcntl.LOCK_EX|fcntl.LOCK_NB)
print('locked',flush=True)
time.sleep(30)"
                         (namestring ,path) ,text)
                   :output :stream)))
     (nlk:with-cleanup ((uiop:terminate-process ,holder) (uiop:wait-process ,holder))
       (is (equal "locked" (read-line (uiop:process-info-output ,holder) nil)))
       ,@body)))

(deftest a-lock-is-exclusive-across-processes-and-freed-with-its-holder (with-temp-workspace (dir))
  (let ((path (merge-pathnames "probe.lock" dir)))
    (is (not (nlk:file-locked-p path)) "a missing file is not locked")
    (with-lock-holder (path)
      (is (nlk:file-locked-p path) "the probe sees it held")
      (is (null (nlk:lock-file path)) "a claim against it answers NIL"))
    (is (not (nlk:file-locked-p path)) "the kernel freed it with the holder")
    (let ((lock (nlk:lock-file path)))
      (is lock "now the claim succeeds")
      (nlk:unlock-file lock)
      (is (not (nlk:file-locked-p path)) "and the release frees it"))))

(deftest a-lock-taken-waiting-is-had-once-its-holder-lets-go (with-temp-workspace (dir))
  ;; WAIT is the kernel's wait for the holder, not a refusal: the cell
  ;; loader waits so for another process compiling into its cache (ip-02).
  ;; And no lock outlives an exec: its descriptor closes there.
  #-win32
  (let* ((path (merge-pathnames "wait.lock" dir))
         (asking (bt2:make-semaphore :name "test-lock-asking"))
         (lock :waiting)
         (taker nil))
    (with-lock-holder (path)
      (setf taker (nlk:spawn "test-lock-taker"
                    (bt2:signal-semaphore asking)
                    (setf lock (nlk:lock-file path :wait t))))
      (is (bt2:wait-on-semaphore asking :timeout 10))
      (sleep 0.2)
      ;; it waits while another process holds the file
      (is (eq :waiting lock)))
    (bt2:join-thread taker)
    ;; and has it once that process is gone
    (is (integerp lock))
    (is (nlk:file-locked-p path))
    (is (logtest 1 (sb-posix:fcntl lock sb-posix:f-getfd)))
    (nlk:unlock-file lock)
    (is (not (nlk:file-locked-p path)))))

(deftest a-holder-probing-its-own-lock-keeps-it (with-temp-workspace (dir))
  ;; POSIX drops every record lock a process holds on a file the moment it
  ;; closes ANY descriptor for that file, so a probe that opened the file
  ;; would take the holder's own lock away. FILE-LOCKED-P answers a lock of
  ;; this process's own from *HELD-LOCKS*, opening nothing.
  (let* ((path (merge-pathnames "probe.lock" dir))
         (lock (nlk:lock-file path)))
    (is lock "the claim succeeds")
    (is (nlk:file-locked-p path) "the holder reads its own lock as held")
    (dotimes (i 5) (nlk:file-locked-p path))
    (is (equal (format nil "refused~%")
               (nlk:run-bounded (list "python3" "-c"
                                      "import fcntl,sys
f=open(sys.argv[1],'w')
try:
    fcntl.lockf(f,fcntl.LOCK_EX|fcntl.LOCK_NB)
    print('took',flush=True)
except OSError:
    print('refused',flush=True)"
                                      (namestring path)))))
    (nlk:unlock-file lock)
    (is (not (nlk:file-locked-p path)) "the release forgets it too")))

#-win32
(deftest a-folder-lock-is-one-file-for-two-homes-that-share-one-folder (with-temp-workspace (dir))
  ;; Profile-create links a shared folder into the new home rather than
  ;; copying it, so one folder is reached by two paths — and a lock that
  ;; named the path would be two locks. Resolved first, it is one file.
  (let* ((folder (ensure-directories-exist (merge-pathnames "memory/" dir)))
         (link (merge-pathnames "home/memory/" dir)))
    (ensure-directories-exist (merge-pathnames "home/" dir))
    (nlk:make-symlink folder link)
    (is (equal (namestring (nlk:folder-lock-path folder))
               (namestring (nlk:folder-lock-path link))) "one lock file, not two")
    (is (equal (namestring (merge-pathnames ".locks/memory.lock" dir))
               (namestring (nlk:folder-lock-path folder))) "beside it, in a dot directory")))

#-win32
(deftest a-folder-lock-waits-then-refuses-while-a-foreign-process-holds-it (with-temp-workspace (dir))
  (let* ((folder (merge-pathnames "memory/" dir))
         (path (nlk:folder-lock-path folder)))
    (ensure-directories-exist folder)
    ;; The holder is a foreign process and makes the file itself, so the
    ;; .locks directory has to be there first (the writer makes it).
    (ensure-directories-exist path)
    (with-lock-holder (path)
      (is (null (nlk:lock-folder folder :wait 0.2)) "the claim waits, then answers NIL")
      (is (signals-error error (nlk:with-folder-lock (folder :wait 0.2) (never))) "refused, never unlocked"))
    (let ((lock (nlk:lock-folder folder :wait 0.2)))
      (is lock "the holder gone, the claim succeeds")
      (nlk:unlock-folder folder)
      (is (not (nlk:file-locked-p path)) "and the release frees it"))))

(deftest a-folder-lock-counts-its-reentrance-and-releases-on-unwind (with-temp-workspace (dir))
  ;; POSIX would let this process take the lock twice and CLOSE the second
  ;; descriptor over it, dropping the first lock with it; the count is what
  ;; keeps a nested writer's body protected.
  (let* ((folder (merge-pathnames "skills/" dir))
         (path (nlk:folder-lock-path folder)))
    (ensure-directories-exist folder)
    (nlk:with-folder-lock (folder)
      (nlk:with-folder-lock (folder)
        (is (nlk:file-locked-p path) "the inner claim is the outer one"))
      (is (nlk:file-locked-p path) "the inner release leaves the outer held"))
    (is (not (nlk:file-locked-p path)) "the outer release frees it")
    (is (signals-error error (nlk:with-folder-lock (folder) (error "the body blew up"))) "and it rides out")
    (is (not (nlk:file-locked-p path)) "and the lock is not left behind")))

(deftest replace-file-renames-over-an-existing-target (with-temp-workspace (dir))
  (let ((from (merge-pathnames "from" dir))
        (to (merge-pathnames "to" dir)))
    (write-temp-file from "new")
    (write-temp-file to "old")
    (nlk:replace-file from to)
    (is (equal "new" (uiop:read-file-string to)) "the target carries the source's bytes")
    (is (not (probe-file from)) "the source is gone")
    (is (equal (namestring to)
               (namestring (nlk:replace-file (progn (write-temp-file from "x") from)
                                             to))))))

(deftest random-bytes-come-from-the-operating-system (let ((first (nlk:random-bytes 32))
                                                           (second (nlk:random-bytes 32))))
  (is (and (typep first '(vector (unsigned-byte 8))) (= 32 (length first))))
  (is (not (equalp first second)) "two draws differ")
  (is (> (length (remove-duplicates (coerce first 'list))) 8)))

(deftest pid-live-p-tells-a-live-process-from-a-gone-one ()
  (is (nlk:pid-live-p (sb-posix:getpid)) "this process is alive")
  (let* ((child (uiop:launch-program (list "true")))
         (pid (uiop:process-info-pid child)))
    (uiop:wait-process child)
    (is (not (nlk:pid-live-p pid)) "a reaped child is gone")))

(deftest spawn-detached-answers-the-command-and-logs-its-output
    (with-temp-trees ((dir "os-detached")))
  (let ((log (merge-pathnames "detached.log" dir))
        (marker (format nil "detached-~36r" (random (expt 36 8)))))
    (is (nlk:spawn-detached (nlk:shell-argv (format nil "echo ~a" marker)) log))
    (is (await (:rest 0.05) (and (probe-file log) (search marker (uiop:read-file-string log)))))))

;;; --- memory: the kernel's side of the organism's pages -----------------------

(deftest the-memory-asks-answer-per-platform ()
  ;; Linux answers residency, huge pages, arenas and the mapped release;
  ;; Windows residency and the working-set trim; anywhere else says so with
  ;; NIL rather than pretending, so a policy reads the answer and never the
  ;; platform.
  #+linux
  (let ((resident (nlk:resident-bytes)))
    (is (and (integerp resident) (plusp resident)) "resident bytes read from /proc")
    (is (nlk:disable-huge-pages) "the huge-page request is made")
    (is (nlk:cap-malloc-arenas) "the arena cap is made")
    (is (nlk:resolve-names-directly) "names resolve past systemd's plugins")
    (is (sb-bsd-sockets:get-host-by-name "localhost") "and /etc/hosts still answers")
    (is (let ((released (nlk:release-mapped-pages
                         (nlk::resolve-runtime sb-ext:*core-pathname*))))
          (and (integerp released) (plusp released))))
    (is (plusp (nlk:release-mapped-pages #p"/nonexistent/never-mapped")) "libraries go back with no image")
    (is (integerp (nlk:resident-bytes)) "the process still reports after the release")
    (is (null (nlk:trim-working-set)) "no whole-working-set trim"))
  #+win32
  (let ((resident (nlk:resident-bytes)))
    (is (and (integerp resident) (plusp resident)) "the working set read")
    (is (nlk:trim-working-set) "the trim is made")
    (is (< (nlk:resident-bytes) resident) "and the working set is smaller for it")
    (is (null (nlk:release-mapped-pages #p"C:/x")) "no release of one mapping"))
  #-(or linux win32)
  (progn
    (is (null (nlk:resident-bytes)) "no residency figure")
    (is (null (nlk:disable-huge-pages)) "no huge-page setting")
    (is (null (nlk:cap-malloc-arenas)) "no arena setting")
    (is (null (nlk:resolve-names-directly)) "no resolver setting")
    (is (null (nlk:release-mapped-pages #p"/tmp/x")) "no release call")
    (is (null (nlk:trim-working-set)) "no working-set trim")))

(deftest the-mapped-resident-read-sees-this-image ()
  #+linux
  (let ((core (nlk::resolve-runtime sb-ext:*core-pathname*)))
    (is (plusp (nlk:mapped-resident-bytes core)))
    (is (< (nlk:mapped-resident-bytes #p"/nonexistent/never-mapped")
           (nlk:mapped-resident-bytes core)) "the image's pages count only as the image")
    (let ((before (nlk:mapped-resident-bytes core)))
      (nlk:release-mapped-pages core)
      (is (< (nlk:mapped-resident-bytes core) before))))
  #-linux
  (is (null (nlk:mapped-resident-bytes #p"/tmp/x")) "no such read off Linux"))

;;; What a release may drop outright is what the file can hand back: a
;;; mapping this process cannot write, holding no page of its own.
(defun mapping-permissions (start)
  "The permission field /proc/self/maps shows for the mapping at START."
  (loop for line in (uiop:read-file-lines "/proc/self/maps")
        when (= start (parse-integer line :end (position #\- line) :radix 16))
          return (subseq line (1+ (position #\Space line)) (+ 5 (position #\Space line)))))

(deftest the-release-sorts-mappings-by-what-their-file-can-give-back ()
  #+linux
  (let ((kinds '()))
    (nlk::map-file-mappings (nlk:lambda-ignoring (start end kind rss)
                              (push (cons kind start) kinds))
                            (nlk::resolve-runtime sb-ext:*core-pathname*))
    (is (assoc :image kinds) "the image's mappings")
    (is (assoc :clean kinds) "a library's code")
    (is (every (lambda (entry)
                 (or (not (eq :clean (car entry)))
                     (char= #\- (char (mapping-permissions (cdr entry)) 1))))
               kinds) "nothing writable is dropped outright")))

;;; A library's RELRO — the GOT and the constants ld.so relocated, then made
;;; read-only — is a read-only file mapping every page of which is private.
;;; Swapped out, smaps counts those pages under Swap:, not Anonymous:, and a
;;; DONTNEED on it would hand back the FILE's page: unrelocated pointers, 0 or
;;; an offset like #xcab0, which the next call through them would jump to.
(defun private-library-mapping ()
  "START and END of a read-only library mapping that holds pages of this
process's own, or NIL."
  (loop with start and end and library
        for line in (uiop:read-file-lines "/proc/self/smaps")
        do (let ((dash (position #\- line))
                 (space (position #\Space line)))
             (cond ((and dash space (< dash space))
                    (setf start (parse-integer line :end dash :radix 16)
                          end (parse-integer line :start (1+ dash) :end space :radix 16)
                          library (and (search ".so" line)
                                       (char= #\- (char line (+ space 2))))))
                   ((and library (eql 0 (search "Anonymous:" line))
                         (plusp (parse-integer line :start 10 :junk-allowed t)))
                    (return (values start end)))))))

(deftest a-swapped-private-page-is-never-dropped-outright ()
  ;; MADV_PAGEOUT is what memory pressure does to a cold RELRO page. A host
  ;; without swap keeps the page resident, and the gate still holds.
  #+linux
  (multiple-value-bind (start end) (private-library-mapping)
    (is-present start "a library's relocated read-only mapping"
      (nlk::%madvise start (- end start) 21) ; MADV_PAGEOUT
      (let ((kind :unseen))
        (nlk::map-file-mappings (nlk:lambda-ignoring (from to seen rss)
                                  (when (= from start) (setf kind seen)))
                                (nlk::resolve-runtime sb-ext:*core-pathname*))
        (is (not (eq kind :clean)) "a private page in swap is still private")))))

(deftest the-mapped-resident-read-conses-almost-nothing ()
  ;; The idle collector reads it inside the interval it measures for quiet:
  ;; without this, a line string per smaps line would push a read past 64 KB.
  #+linux
  (let ((core (nlk::resolve-runtime sb-ext:*core-pathname*)))
    (nlk:mapped-resident-bytes core)
    (let ((before (sb-ext:get-bytes-consed)))
      (nlk:mapped-resident-bytes core)
      (is (< (- (sb-ext:get-bytes-consed) before) (* 64 1024)) "under 64 KB a read"))))

(deftest a-path-lies-within-a-folder-only-past-a-separator ()
  (is (nlk::path-within-p "/a/b" "/a/b") "the folder itself")
  (is (nlk::path-within-p "/a/b/c" "/a/b") "a step beneath it")
  (is (not (nlk::path-within-p "/a/bc" "/a/b")) "never a name that only starts like it")
  (is (not (nlk::path-within-p "/a" "/a/b")) "nor the folder above")
  (is (nlk::path-within-p "/a" "") "everything lies within the root")
  ;; REAL-PATH spells a Windows path with `\'.
  #+win32 (is (nlk::path-within-p "C:\\proj\\src" "C:\\proj") "Windows steps with \\")
  #+win32 (is (nlk::path-within-p "C:\\proj" "C:\\") "a drive's root keeps its own")
  #+win32 (is (not (nlk::path-within-p "C:\\project" "C:\\proj")) "nor there a name that only starts like it"))

(deftest lstat-kind-reads-a-folder-named-with-its-separator (with-temp-workspace (dir "inside/a.txt"))
  ;; A folder's native namestring ends in its separator; Windows' stat finds
  ;; no folder by such a name, so a folder named so read as absent there.
  (let ((folder (uiop:native-namestring (merge-pathnames "inside/" dir))))
    (is (eq :directory (nlk::lstat-kind folder)) "the folder, its separator at its end, is a folder")
    (is (eq :directory (nlk::lstat-kind (string-right-trim "/\\" folder))) "and without it")
    (is (eq :file (nlk::lstat-kind (uiop:native-namestring (merge-pathnames "inside/a.txt" dir)))) "a file a file")
    (is (eq :absent (nlk::lstat-kind (uiop:native-namestring (merge-pathnames "none/" dir)))) "nothing is absent")
    #+win32
    (is (eq :directory (nlk::lstat-kind (subseq folder 0 3))) "and a drive's root a folder")))

#-win32
#-win32
(deftest remove-tree-takes-a-link-as-a-link-and-leaves-what-it-points-at
    (with-temp-workspace (dir "tree/sub/f.txt" "outside/keep.txt"))
  ;; The walk lstat(2)s every entry: a link to a folder, and one to nothing,
  ;; go as links; a read-only file goes; what a link points at stays.
  (let ((tree (merge-pathnames "tree/" dir)))
    (nlk:make-symlink (merge-pathnames "outside/" dir) (merge-pathnames "sub/out" tree))
    (nlk:make-symlink (merge-pathnames "nowhere/" dir) (merge-pathnames "dangling" tree))
    (sb-posix:chmod (uiop:native-namestring (merge-pathnames "sub/f.txt" tree)) #o444)
    (is (nlk:remove-tree tree))
    (is (null (uiop:directory-exists-p tree)) "the tree is gone")
    (is (probe-file (merge-pathnames "outside/keep.txt" dir)) "and what its link reached is not")))

(deftest a-listing-says-what-each-name-is (with-temp-workspace (dir "a.txt" "sub/b.txt"))
  ;; The kind rides the listing, so a walk after folders stats no file.
  (sb-posix:symlink "a.txt" (namestring (merge-pathnames "link" dir)))
  (multiple-value-bind (names kinds) (nlk::directory-names (namestring dir))
    (let ((kind-of (mapcar #'cons names kinds)))
      (is (equal '("a.txt" "link" "sub") (sort (copy-list names) #'string<)))
      (is (eq :file (cdr (assoc "a.txt" kind-of :test #'string=))))
      (is (eq :directory (cdr (assoc "sub" kind-of :test #'string=))))
      (is (eq :link (cdr (assoc "link" kind-of :test #'string=))) "a link is a link, not what it reaches"))))

#+linux
(deftest file-octets-reads-a-file-that-says-no-size ()
  ;; /proc says 0 for every size: read to the end of the file all the same,
  ;; past the first buffer's worth.
  (is (zerop (sb-posix:stat-size (sb-posix:stat "/proc/self/maps"))))
  (let ((octets (nlt:file-octets "/proc/self/maps")))
    (is (> (length octets) 4096) "a map of an SBCL image is pages long")
    (is (= (char-code #\Newline) (aref octets (1- (length octets)))) "and read to its last line")))

(deftest raise-file-limit-raises-to-the-select-safe-cap-and-never-lowers ()
  ;; Sai's Mac, 2026-10-21: a gateway at macOS's 256 open files resolved no
  ;; name once leaked sockets filled them. MAIN raises the soft limit toward
  ;; the hard one, capped where select(2) still sees every descriptor.
  #-win32
  (multiple-value-bind (soft hard) (nlk::file-limit)
    (is (and soft hard (<= soft hard)) "the limits read back")
    (nlk:with-cleanup ((nlk::file-limit soft))
      (let ((saved nlk::*file-limit*))
        (nlk:with-cleanup ((setf nlk::*file-limit* saved))
          (is (= 256 (nlk::file-limit 256)) "the soft limit can be set to macOS's start")
          (let ((raised (nlk::raise-file-limit)))
            (is (= 256 (getf raised :was)))
            (is (= (min hard nlk::+select-safe-descriptors+) (getf raised :soft) (nlk::file-limit)) "raised to the cap, or the hard limit under it")
            (is (search "raised from 256" (nlk::file-limit-line raised)) "and said once"))
          (when (> hard 2048)
            (nlk::file-limit 2048)
            (is (= 2048 (getf (nlk::raise-file-limit) :soft)) "a higher limit is never lowered")))))))

(deftest a-named-wait-is-heard-from-its-start-to-its-end ()
  ;; With nobody listening the body only runs. A listener hears what the
  ;; body waits on and its bound as it starts, and NIL as it ends, however
  ;; it ends; the body's values pass through.
  (is (equal '(1 2) (multiple-value-list (nlk:with-named-wait (5 "nothing heard") (values 1 2)))))
  (let* ((heard '())
         (nlk:*wait-hook* (lambda (text bound) (push (list text bound) heard))))
    (is (equal '(1 2) (multiple-value-list
                       (nlk:with-named-wait (7 "fetching ~a from ~a" "x" "https://y") (values 1 2)))))
    (is (equal '(("fetching x from https://y" 7) (nil nil)) (reverse heard)))
    (setf heard '())
    (ignore-errors (nlk:with-named-wait (7 "a wait that throws") (error "cut short")))
    (is (equal '(("a wait that throws" 7) (nil nil)) (reverse heard)) "it ends when its body throws")))

(deftest wait-seconds-holds-to-adddeadlines-lean-traces ()
  ;; proofs/AddDeadlines.lean decides what WAIT-SECONDS gives each step of a
  ;; run under one deadline (givenTrace) and where the run ends (cut): the
  ;; real WAIT-SECONDS walks the same runs here on a clock of the test's own,
  ;; each step taking what it is given or its own time, the less, and the
  ;; step given nothing signals instead of starting. The owner's deadline for
  ;; a waiter's seconds is the proof's DEADLINE too.
  (flet ((walk (seconds wants &aux (units internal-time-units-per-second)
                                   (deadline (nlk::make-wait-deadline (* seconds units) seconds)) (now 0) (given '()))
           (dolist (want wants (values (reverse given) (/ now units) nil))
             (destructuring-bind (bound took) want
               (handler-case (let ((seconds (nlk:wait-seconds bound :deadline deadline :now now)))
                               (push seconds given)
                               (incf now (* units (min seconds took))))
                 (nlk:wait-deadline-passed ()
                   (return (values (reverse (cons 0 given)) (/ now units) t))))))))
    ;; trace100, trace100_ends: the indexes, a download cut at its own 20 s,
    ;; the lock held elsewhere past the deadline, and a clone that never starts
    (multiple-value-bind (given end signalled) (walk 100 '((10 3) (10 4) (20 25) (nil 1000) (600 5)))
      (is (equal '(10 10 20 73 0) given))
      (is (= 100 end))
      (is signalled "the clone, given nothing, signals instead of starting"))
    ;; deadline600, trace570, trace570_ends: add's 600 s leave the work 570,
    ;; and the fetch that would hang is given what is left of them
    (is (= 570 (nlk:cell-switch-deadline 600)))
    (multiple-value-bind (given end signalled)
        (walk (nlk:cell-switch-deadline 600) '((10 1) (10 1) (20 20) (nil 0) (600 1) (600 100000) (600 590)))
      (is (equal '(10 10 20 548 548 547 0) given))
      (is (= 570 end))
      (is signalled))
    ;; margins
    (is (equal '(570 30 6 1) (mapcar #'nlk:cell-switch-deadline '(600 60 12 1))))
    ;; with no deadline every step keeps its own bound, and the lock has none
    (is (= 600 (nlk:wait-seconds 600 :deadline nil)))
    (is (null (nlk:wait-seconds nil :deadline nil)))))

(deftest a-wait-deadline-holds-the-sooner-of-two-and-clips-what-runs-under-it ()
  ;; A deadline inside another ends no later than the outer one; a child of
  ;; RUN-BOUNDED under it is killed when it runs out, and none starts once it
  ;; has passed.
  (nlk:with-wait-deadline (1)
    (let ((outer nlk:*wait-deadline*))
      (nlk:with-wait-deadline (60)
        (is (eq outer nlk:*wait-deadline*) "a later deadline inside leaves the sooner one"))
      (nlk:with-wait-deadline (nil)
        (is (eq outer nlk:*wait-deadline*)))))
  #-win32
  (let ((started (get-internal-real-time)))
    (is (eq :timeout (nth-value 2 (nlk:with-wait-deadline (1)
                                    (nlk:run-bounded '("sleep" "30") :seconds 30)))))
    ;; the child is given what is left of the deadline, not its own 30 s
    (is (< (/ (- (get-internal-real-time) started) internal-time-units-per-second) 3)))
  ;; a child is not started past the deadline
  (is (search "ran out"
              (refusal-text nlk:wait-deadline-passed
                (nlk:with-wait-deadline (1/100)
                  (sleep 0.05)
                  (nlk:run-bounded '("true"))))))
  ;; The time is said to have run out in the step it cut short: a named
  ;; wait, or a wait for a lock that names itself.
  #-win32
  (let ((text (refusal-text nlk:wait-deadline-passed
                (nlk:with-wait-deadline (1/2)
                  (nlk:with-named-wait (30 "sleeping in a test")
                    (nlk:run-bounded '("sleep" "30") :seconds 30))
                  (nlk:run-bounded '("true"))))))
    (is (search "ran out while sleeping in a test" text) text))
  (let ((lock (bt2:make-lock :name "a test's lock"))
        (held (bt2:make-semaphore))
        (done (bt2:make-semaphore)))
    (bt2:make-thread (lambda ()
                       (bt2:with-lock-held (lock)
                         (bt2:signal-semaphore held)
                         (bt2:wait-on-semaphore done :timeout 5)))
                     :name "holds a test's lock")
    (bt2:wait-on-semaphore held)
    (unwind-protect
         (is (equal "the time ran out while waiting on a test's lock"
                    (nlk:wait-deadline-reason
                     (signals-error nlk:wait-deadline-passed
                       (nlk:with-wait-deadline (1/5)
                         (nlk:call-with-lock-in-time lock (constantly t) "waiting on a test's lock"))))))
      (bt2:signal-semaphore done)))
  ;; a step no wait named is said as none
  (is (null (nlk:wait-deadline-reason (make-condition 'nlk:wait-deadline-passed :seconds 1))))
  (is (equal "cut short" (nlk:wait-deadline-reason (make-condition 'simple-error :format-control "cut short")))))

;;; --- an interruption that unwinds lands where it strands nothing -----------

(defun c-malloc-and-free (size)
  "One malloc of SIZE bytes and its free, in C."
  (sb-alien:alien-funcall (sb-alien:extern-alien "free" (function sb-alien:void sb-sys:system-area-pointer))
                          (sb-alien:alien-funcall (sb-alien:extern-alien "malloc" (function sb-sys:system-area-pointer
                                                                                          sb-alien:unsigned-long))
                                                  size)))

(defun spin-in-malloc ()
  "Call malloc and free forever, at sizes past glibc's thread cache, so that
every call takes its arena's lock."
  (loop (c-malloc-and-free (+ 2048 (random 60000)))))

(defun every-arena-answers-p ()
  "Whether malloc_trim, which takes every arena's lock in turn, returns within
five seconds on a thread of its own. Windows' C library has no malloc_trim:
there a thousand mallocs and frees over the sizes the spinner asks return in
that time, each size class's lock taken."
  (eq :ok (nlk::%join-bounded
           (bt2:make-thread (lambda ()
                              #-win32
                              (sb-alien:alien-funcall (sb-alien:extern-alien "malloc_trim" (function sb-alien:int sb-alien:unsigned-long)) 0)
                              #+win32
                              (loop for size from 2048 below 62048 by 60 do (c-malloc-and-free size))
                              :ok)
                            :name "arena probe")
           5)))

(deftest an-unwinding-interruption-never-strands-a-malloc-lock ()
  ;; The frozen suite: a stop unwound a thread out of malloc, its arena's lock
  ;; stayed held, and the next free elsewhere waited on it for six hours. A
  ;; thread that spends most of its time inside malloc is stopped two hundred
  ;; times, each way a stop is sent; every arena still answers after. With
  ;; BT2:INTERRUPT-THREAD in place of the safe delivery this froze in its
  ;; first few dozen stops.
  (dolist (stop (list (lambda (thread) (nlk:interrupt-thread-safely thread (lambda () (throw 'stopped :stopped))))
                      (lambda (thread) (nlk:terminate-thread-safely thread))
                      (lambda (thread) (bt2:destroy-thread thread))))
    (let ((ended 0))
      (dotimes (i 200)
        (let* ((ready (bt2:make-semaphore))
               (thread (bt2:make-thread (lambda () (catch 'stopped
                                                     (bt2:signal-semaphore ready)
                                                     (spin-in-malloc)))
                                        :name "spins in malloc")))
          (bt2:wait-on-semaphore ready :timeout 5)
          (sleep (/ (random 2000) 1000000))
          (funcall stop thread)
          (nlk::%join-bounded thread 5)
          (unless (bt2:thread-alive-p thread)
            (incf ended))))
      (is (= 200 ended) "every stop ended its thread")
      (is (every-arena-answers-p) "no arena's lock was left held"))))

(deftest an-unwinding-interruption-runs-only-outside-other-c-code ()
  ;; Every run of the function happened where the classifier allows an
  ;; unwind; and a thread spinning in malloc was found inside C at least once,
  ;; the case the delivery sends again rather than unwinds there.
  (let* ((stop nil)
         (thread (bt2:make-thread (lambda () (loop until stop do (c-malloc-and-free (+ 2048 (random 60000)))))
                                  :name "spins in malloc"))
         (runs 0) (unsafe 0) (in-c 0))
    (unwind-protect
         (dotimes (i 300)
           (nlk:interrupt-thread-safely thread (lambda ()
                                                 (incf runs)
                                                 (unless (nlk:interruption-may-unwind-p) (incf unsafe))))
           ;; a plain delivery, which never unwinds, says where it landed
           (let ((landed (bt2:make-semaphore)) (where (list nil)))
             (bt2:interrupt-thread thread (lambda () (setf (car where) (nlk:interruption-may-unwind-p))
                                            (bt2:signal-semaphore landed)))
             (bt2:wait-on-semaphore landed :timeout 5)
             (unless (car where) (incf in-c))))
      (setf stop t)
      (nlk::%join-bounded thread 5))
    (is (= 300 runs) "each delivery ran")
    (is (zerop unsafe) "none ran inside C code")
    #+(and linux x86-64)
    (is (plusp in-c) "the spinning thread was found inside malloc")))

(deftest a-parked-thread-still-takes-an-unwinding-interruption ()
  ;; A stop is for a thread parked on something outside it, so the safe
  ;; delivery must still land in a system call that waits: a sleep, a read
  ;; on a pipe nobody writes, an accept nobody dials, a futex wait.
  (dolist (park (list (lambda () (sleep 30))
                      #-win32
                      (lambda () (multiple-value-bind (in out) (sb-posix:pipe)
                                   (declare (ignore out))
                                   (read-byte (sb-sys:make-fd-stream in :input t :element-type '(unsigned-byte 8)) nil)))
                      (lambda () (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                                   (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
                                   (sb-bsd-sockets:socket-listen socket 1)
                                   (unwind-protect (sb-bsd-sockets:socket-accept socket)
                                     (sb-bsd-sockets:socket-close socket))))
                      (lambda () (bt2:wait-on-semaphore (bt2:make-semaphore)))))
    (let ((thread (bt2:make-thread (lambda () (catch 'stopped (funcall park) :returned)) :name "parked")))
      (sleep 0.2)
      (is (eq :ran (nlk:interrupt-thread-safely thread (lambda () (throw 'stopped :stopped)))))
      (is (eq :stopped (nlk::%join-bounded thread 5)) "and unwound it out of its wait"))))
