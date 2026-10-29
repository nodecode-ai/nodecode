;;;; os.lisp --- what the organism asks of the operating system, in one place.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The organism runs on Linux, macOS and Windows from one source tree, and
;;;; every place the three disagree is a function here with one arm per
;;;; platform behind a reader conditional: the shell a command string runs
;;;; through, a child that must outlive its parent, a bounded run, whether a
;;;; pid is alive, the lock a store owner holds, the flush before a publish,
;;;; the rename that publishes, the stdio severance under a TUI. Callers
;;;; name the need and never the platform; outside this file only the
;;;; terminal core, which owns the console, reads a #+win32, and what a
;;;; caller must say of the platform it says through PLATFORM.
;;;;
;;;; macOS is POSIX with util-linux missing: no setsid(1), so the session a
;;;; child leads and the double fork a detached child needs are spelled with
;;;; what every Mac carries — perl for the session, sh for the fork — and
;;;; SBCL reports the executable by the path it was started through, links
;;;; unresolved (paths.lisp, RESOLVE-RUNTIME).
;;;;
;;;; Windows is bound through the SB-WIN32 package SBCL builds its own
;;;; RUN-PROGRAM and file layer from — CreateProcessW and the STARTUPINFO
;;;; layout, the descriptor-to-HANDLE translation — so the ABI facts live in
;;;; one upstream place; the few calls SBCL does not wrap (a process handle,
;;;; a flush, a std handle) are declared here. A descriptor on Windows is a C
;;;; runtime descriptor over a HANDLE, which is why the file layer speaks
;;;; descriptors to sb-posix and handles to the kernel. The one shell is
;;;; bash on every platform that has it: /bin/bash (else /bin/sh), and on
;;;; Windows the sh.exe Git for Windows carries, itself bash — git is already
;;;; required for layers, so the shell comes with it, and every command
;;;; string, heredoc and exit convention the tool text teaches holds on all
;;;; three.

(in-package #:nodecode.kernel)

;;; --- the platform ------------------------------------------------------------

(defun platform ()
  "The operating system this image runs on, as one word: :LINUX, :DARWIN,
:WINDOWS, or :UNIX for any other POSIX SBCL runs on."
  ;; What a shell says of itself on the wire, what a skill's `platforms' line
  ;; is matched against — the one read, so no caller keeps a reader
  ;; conditional of its own.
  #+win32 :windows
  #+darwin :darwin
  #+linux :linux
  #-(or win32 darwin linux) :unix)

(defun architecture ()
  "The CPU this image runs on, as one word: :X64 or :ARM64."
  ;; Read from the build's own features, like `platform'. Not
  ;; uiop:architecture: the UIOP SBCL ships (3.3.1) maps :x86-64 to :x64 and
  ;; knows 32-bit :arm, but has no row for :arm64, so on Apple Silicon it
  ;; answers NIL -- and a release pointer named for NIL is a 404.
  #+x86-64 :x64
  #+arm64 :arm64
  #-(or x86-64 arm64) (error "no release is built for this CPU"))

;;; --- Windows bindings SBCL does not carry ---------------------------------

(defmacro define-routines (&body routines)
  "A foreign routine for each row (C-NAME LISP-NAME RESULT (ARG TYPE)...): RESULT
and each TYPE are SB-ALIEN's, or SAP, INT or STR for the three most written."
  (let ((short '((sap . sb-sys:system-area-pointer) (int . sb-alien:int)
                 (str . sb-alien:c-string))))
    `(progn
       ,@(loop for (c-name name . types) in routines
               collect `(sb-alien:define-alien-routine (,c-name ,name) ,@(sublis short types))))))

#+win32
(progn
  (define-routines
    ("OpenProcess" %open-process sb-win32::handle (access sb-win32::dword) (inherit int)
     (pid sb-win32::dword))
    ("CreateFileW" %create-file sb-win32::handle (name sb-win32::system-string)
     (access sb-win32::dword) (share sb-win32::dword) (security (* t))
     (disposition sb-win32::dword) (attributes sb-win32::dword) (template sb-win32::handle))
    ("FlushFileBuffers" %flush-file-buffers int (handle sb-win32::handle))
    ("SetStdHandle" %set-std-handle int (which sb-win32::dword) (handle sb-win32::handle))
    ("SetEnvironmentVariableA" %set-environment-variable int (name str) (value str))
    ("SetProcessWorkingSetSize" %set-process-working-set-size int (process sb-win32::handle)
     (minimum (sb-alien:unsigned 64)) (maximum (sb-alien:unsigned 64)))
    ("K32GetProcessMemoryInfo" %get-process-memory-info int (process sb-win32::handle)
     (counters sap) (size sb-win32::dword))
    ("GetFinalPathNameByHandleW" %get-final-path-name sb-win32::dword
     (handle sb-win32::handle) (buffer sap) (size sb-win32::dword) (flags sb-win32::dword)))
  (defconstant +file-share-all+ 7)
  (defconstant +file-flag-backup-semantics+ #x02000000)
  (defconstant +process-query-limited-information+ #x1000)
  (defconstant +still-active+ 259)
  (defconstant +error-access-denied+ 5)
  (defconstant +error-sharing-violation+ 32)
  (defconstant +generic-read-write+ #xC0000000)
  (defconstant +open-existing+ 3)
  (defconstant +open-always+ 4)
  (defconstant +file-attribute-normal+ #x80)
  (defconstant +detached-process+ #x8)
  (defconstant +std-input-handle+ #xFFFFFFF6)
  (defconstant +std-output-handle+ #xFFFFFFF5)
  (defconstant +std-error-handle+ #xFFFFFFF4))

;;; --- the shell ---------------------------------------------------------------

(defun executable-on-path (name)
  "The first PATH directory holding NAME, as NAME's truename, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                            :separator #+win32 ";" #-win32 ":")
        thereis (and (plusp (length directory))
                     (ignore-errors
                      (probe-file
                       (merge-pathnames name (uiop:parse-native-namestring
                                              directory :ensure-directory t)))))))

(define-startup-parameter *shell*
    ;; Bash where the machine has it: the commands a model writes are bash --
    ;; `time', [[ ]], arrays, brace expansion -- and on Debian and Ubuntu
    ;; /bin/sh is dash, where `time' alone is "not found" (three of four
    ;; benchmark runs lost a call to it). Every script this image runs through
    ;; the shell is POSIX sh, which bash runs as written.
    ;; Windows: on PATH, beside the git on PATH (cmd/git.exe by bin/sh.exe), or Git's two homes.
    #-win32 (or (and (probe-file "/bin/bash") "/bin/bash")
                (executable-on-path "bash")
                "/bin/sh")
    #+win32
    (flet ((under (root &rest relative)
             (and root
                  (some (lambda (piece)
                          (ignore-errors
                           (probe-file (merge-pathnames piece root))))
                        relative))))
      (or (executable-on-path "sh.exe")
          (when-let (git (executable-on-path "git.exe"))
            (under (uiop:pathname-parent-directory-pathname
                    (uiop:pathname-directory-pathname git))
                   "bin/sh.exe" "usr/bin/sh.exe"))
          (some (lambda (variable &aux (root (uiop:getenv variable)))
                  (and (stringp root) (plusp (length root))
                       (under (uiop:parse-native-namestring root :ensure-directory t)
                              "Git/bin/sh.exe" "Programs/Git/bin/sh.exe")))
                '("ProgramFiles" "LOCALAPPDATA"))))
  "The shell SHELL-ARGV runs command strings through, resolved when the
process starts: bash (/bin/bash, else the first on PATH), else /bin/sh;
Git for Windows' sh.exe; or NIL when the machine has none.")

(defun shell-argv (command &key login)
  "COMMAND, a string, as the argv that runs it through the shell: sh -c, or
sh -lc under LOGIN — a login shell reads the operator's profile, which is
where a credential helper's PATH lives."
  ;; Signals when the machine has no shell; on Windows that means no Git for
  ;; Windows.
  (check-type command string)
  (unless *shell*
    (error "no shell to run a command through: ~a"
           #+win32 "install Git for Windows (winget install Git.Git); its sh.exe is the shell"
           #-win32 "/bin/sh is missing"))
  (list (if (stringp *shell*) *shell* (uiop:native-namestring *shell*))
        (if login "-lc" "-c")
        command))

(define-startup-parameter *session-wrapper*
    #+linux (list "setsid" "-w")
    ;; setsid(1) in perl: forking first, as `setsid -w' does, when this process leads a group.
    #+darwin (and (probe-file "/usr/bin/perl")
                  (list "/usr/bin/perl" "-e" "use POSIX ();
if (getpgrp() == $$) {
  my $pid = fork();
  defined($pid) or exit 127;
  if ($pid) { waitpid($pid, 0); my $s = $?; exit(($s & 127) ? 128 + ($s & 127) : $s >> 8); }
}
defined(POSIX::setsid()) or exit 127;
exec { $ARGV[0] } @ARGV;
exit 127;" "--"))
    #-(or linux darwin) nil
  "What SESSION-ARGV puts in front of a command, resolved when the process
starts: NIL where a child cannot be given a session of its own, and then
the command is itself.")

(defun session-argv (argv)
  "ARGV wrapped so the child leads a session of its own — no controlling
terminal, so a program that touches /dev/tty fails cleanly instead of being
stopped by job control from the TUI's terminal."
  ;; *SESSION-WRAPPER* is the
  ;; wrap: setsid on Linux, perl's setsid on macOS, nothing on Windows, where
  ;; ARGV is itself.
  (append *session-wrapper* argv))

;;; --- processes ---------------------------------------------------------------

(defun pid-live-p (pid)
  "Whether a process PID exists: kill(2) with signal 0, where a refusal other
than `no such process' (a pid owned by another user) still means alive; on
Windows a query handle that reports STILL_ACTIVE, or an access refusal,
which likewise names a process that is there."
  #-win32
  (handler-case (progn (sb-posix:kill pid 0) t)
    (sb-posix:syscall-error (condition)
      (/= (sb-posix:syscall-errno condition) sb-posix:esrch))
    (error () nil))
  #+win32
  (let ((process (%open-process +process-query-limited-information+ 0 pid)))
    (if (zerop process)
        (= (sb-win32::get-last-error) +error-access-denied+)
        (with-cleanup ((sb-win32::close-handle process))
          (multiple-value-bind (ok code) (sb-win32::get-exit-code-process process)
            (and (/= ok 0) (= code +still-active+)))))))

(defun running-executables (&aux (deleted " (deleted)"))
  "The path every process this account can see was started from, as the
kernel names it in /proc/<pid>/exe, less the ` (deleted)' it adds once that
file is gone. NIL where there is no /proc."
  ;; The path, not the file: a file put back where a running one was deleted
  ;; still reads `(deleted)' in its process's name, and the path is what a
  ;; fresh child of that process execs. Another user's process refuses the
  ;; readlink and is not listed.
  #+linux
  (loop for directory in (ignore-errors (directory #p"/proc/*/" :resolve-symlinks nil))
        for exe = (ignore-errors (sb-posix:readlink (uiop:strcat (namestring directory) "exe")))
        when exe
          collect (if (uiop:string-suffix-p exe deleted)
                      (subseq exe 0 (- (length exe) (length deleted)))
                      exe))
  #-linux nil)

;;; Linux: what a process tells the kernel about its own pages.
#+linux
(define-routines
  ("madvise" %madvise int (start sb-alien:unsigned-long) (length sb-alien:unsigned-long)
   (advice int))
  ("prctl" %prctl int (option int) (arg2 sb-alien:unsigned-long) (arg3 sb-alien:unsigned-long)
   (arg4 sb-alien:unsigned-long) (arg5 sb-alien:unsigned-long))
  ("mallopt" %mallopt int (param int) (value int))
  ("__nss_configure_lookup" %nss-configure-lookup int (database str) (service str)))

(defun spawn-detached (command log)
  "Launch COMMAND fully detached, output appended to LOG: the child survives
this process's exit and owns no terminal."
  ;; => COMMAND, or (values NIL
  ;; REASON). On POSIX the child is double-forked into a session of its own
  ;; (setsid -f, or macOS's wrapped shell), the wrapper started through
  ;; SPAWN-PROGRAM, which creates the log, and reaped here. On Windows one CreateProcessW with
  ;; DETACHED_PROCESS: no console, so the console that closes over the parent
  ;; takes nothing with it; stdin is NUL and both output handles the log,
  ;; inherited through the STARTUPINFO. Every inheritable handle of this
  ;; process rides along, as under SBCL's own RUN-PROGRAM there.
  (with-handlers ((error (condition)
                    (values nil (princ-to-string condition))))
    (ensure-directories-exist log)
    #-win32
    (bind (((pid _ _ process)
            (spawn-program
             ;; Without setsid(1), macOS's wrapper leads a shell that backgrounds it.
             #+linux (append (list "setsid" "-f") command)
             #-linux (append *session-wrapper*
                             (list "/bin/sh" "-c" "exec \"$0\" \"$@\" &")
                             command)
             :output log :error-output :output)))
      (wait-for-child pid process))
    #+win32
    (let* ((command-line
             ;; Quoted the way the C runtime unquotes, by SBCL's own RUN-PROGRAM
             ;; quoting: an argument empty or holding a space, a tab or a quote
             ;; is quoted, and the backslashes before a quote doubled.
             (sb-impl::prepare-args command t))
           (null-handle (sb-win32::duplicate-and-unwrap-fd
                         (sb-posix:open "NUL" sb-posix:o-rdonly) :inheritp t))
           (log-handle (sb-win32::duplicate-and-unwrap-fd
                        (sb-posix:open (uiop:native-namestring log)
                                       (logior sb-posix:o-wronly sb-posix:o-creat
                                               sb-posix:o-append)
                                       #o600)
                        :inheritp t)))
      (with-cleanup ((sb-win32::close-handle null-handle) (sb-win32::close-handle log-handle))
        (sb-alien:with-alien ((information sb-win32::process-information)
                              (startup sb-win32::startup-info))
          (sb-win32::zero-alien startup sb-win32::startup-info)
          (setf (sb-alien:slot startup 'sb-win32::cb)
                (sb-alien:alien-size sb-win32::startup-info :bytes)
                (sb-alien:slot startup 'sb-win32::flags)
                sb-win32::+startf-use-std-handles+
                (sb-alien:slot startup 'sb-win32::stdin) null-handle
                (sb-alien:slot startup 'sb-win32::stdout) log-handle
                (sb-alien:slot startup 'sb-win32::stderr) log-handle)
          (unless (sb-win32::create-process
                   nil command-line nil nil t
                   +detached-process+ nil nil
                   (sb-alien:alien-sap startup) (sb-alien:alien-sap information))
            (error "CreateProcess ~a: Windows error ~d"
                   (first command) (sb-win32::get-last-error)))
          (sb-win32::close-handle
           (sb-alien:slot information 'sb-win32::thread-handle))
          (sb-win32::close-handle
           (sb-alien:slot information 'sb-win32::process-handle)))))
    command))

(defun kill-tree (pid)
  "End PID and everything under it, now: SIGKILL to PID's process group
(RUN-BOUNDED's and SPAWN-DETACHED's children lead their own groups) and to
PID itself; on Windows `taskkill /T /F', which walks the tree the same
way. Never signals."
  #-win32 (ignore-errors (sb-posix:kill (- pid) sb-posix:sigkill))
  #-win32 (ignore-errors (sb-posix:kill pid sb-posix:sigkill))
  #+win32
  (ignore-errors
   (uiop:run-program (list "taskkill" "/PID" (princ-to-string pid) "/T" "/F")
                     :output nil :error-output nil :ignore-error-status t))
  t)

(defun %drain (in out)
  "Copy IN to OUT a character at a time, so what a child wrote before it
was killed is in OUT even when the pipe never closes."
  (ignore-errors
   (loop for char = (read-char in nil)
         while char
         do (write-char char out))))

(defun symlink-target (path)
  "What the symbolic link at PATH points to, as a native namestring, or NIL
when PATH is not a link (or, on Windows, always: an install there is a
copy carrying a marker, never a link)."
  #-win32
  (let ((native (string-right-trim "/" (sb-ext:native-namestring path))))
    (and (eq :link (lstat-kind native))
         (ignore-errors (sb-posix:readlink native))))
  #+win32
  (progn path nil))

(defun make-symlink (target link)
  "A symbolic link at LINK pointing to TARGET, replacing a link already
there. => LINK, or NIL on Windows, where the caller copies instead."
  #-win32
  (let ((native (string-right-trim "/" (sb-ext:native-namestring link))))
    (when (ignore-errors (sb-posix:lstat native))
      (sb-posix:unlink native))
    (sb-posix:symlink (sb-ext:native-namestring target) native)
    link)
  #+win32
  (progn target link nil))

(defun copy-tree-preserving (from to)
  "Everything under the directory FROM copied under TO — hidden entries,
permissions and symbolic links preserved as they are (a link is copied as
a link, never followed)."
  ;; `cp -a' on POSIX, robocopy on Windows; parents of TO are created. Signals
  ;; when the copy fails.
  (let ((from (uiop:native-namestring (uiop:ensure-directory-pathname from)))
        (to (uiop:native-namestring (uiop:ensure-directory-pathname to))))
    (ensure-directories-exist to)
    #-win32
    (run-checked (list "cp" "-a" (concatenate 'string from ".") to)
                 (lambda (err) (error "copying ~a to ~a failed: ~a" from to err)))
    #+win32
    (bind (((_ err status)
            (run-bounded (list "robocopy" (string-right-trim "\\" from) (string-right-trim "\\" to)
                               "/E" "/NFL" "/NDL" "/NJH" "/NJS" "/NP")
                         :seconds 600)))
      ;; robocopy: 0-7 are success codes, 8 and above failures.
      (unless (and (integerp status) (< status 8))
        (error "copying ~a to ~a failed: ~a" from to (string-trim '(#\Newline) err))))
    to))

#-win32
(defun tree-entries (native)
  "The entries of the directory NATIVE, a native path with no trailing
slash, as (PATH . KIND), KIND :DIRECTORY, :LINK or :FILE as lstat(2) reads
it: a link is never followed."
  (let ((names '()))
    (let ((directory (sb-posix:opendir native)))
      (unwind-protect
           (loop for entry = (sb-posix:readdir directory)
                 until (sb-alien:null-alien entry)
                 do (let ((name (sb-posix:dirent-name entry)))
                      (unless (member name '("." "..") :test #'string=)
                        (push name names))))
        (sb-posix:closedir directory)))
    (loop for name in (nreverse names)
          for path = (concatenate 'string native "/" name)
          for type = (logand (sb-posix:stat-mode (sb-posix:lstat path)) sb-posix:s-ifmt)
          collect (cons path (cond ((= type sb-posix:s-ifdir) :directory)
                                   ((= type sb-posix:s-iflnk) :link)
                                   (t :file))))))

(defun remove-tree (directory)
  "DIRECTORY and everything under it, gone — a symbolic link inside is
removed as a link, never followed into what it points at."
  ;; UIOP's DELETE-DIRECTORY-TREE follows links on SBCL: a home whose cells/
  ;; holds links into the shipped tree would take the shipped cells with it.
  ;; On POSIX a walk that lstat(2)s every entry, in this process: an `rm -rf'
  ;; child cost ~120 ms of a hub install on macOS. `rm -rf' still takes what
  ;; the walk cannot (a name that is not UTF-8); rmdir on Windows. Signals
  ;; when anything is left.
  (let ((native (string-right-trim "/\\" (uiop:native-namestring
                                            (uiop:ensure-directory-pathname directory)))))
    (when (uiop:directory-exists-p directory)
      #-win32
      (labels ((clear (path)
                 (loop for (child . kind) in (tree-entries path)
                       do (if (eq kind :directory) (clear child) (sb-posix:unlink child)))
                 (sb-posix:rmdir path)))
        (handler-case (clear native)
          (error ()
            (run-checked (list "rm" "-rf" native)
                         (lambda (err) (error "removing ~a failed: ~a" native err))))))
      #+win32
      (run-checked (list "cmd" "/c" "rmdir" "/s" "/q" native)
                   (lambda (err) (error "removing ~a failed: ~a" native err))))
    (when (uiop:directory-exists-p directory)
      (error "removing ~a left it in place" native))
    t))

(defun set-environment-variable (name value)
  "NAME=VALUE in this process's environment from now on: what every child
spawned after this inherits, and what this process's own reads answer."
  ;; POSIX setenv; the Win32 process block on Windows, which CreateProcess
  ;; hands a child that names no environment of its own.
  #-win32 (sb-posix:setenv name value 1)
  #+win32 (%set-environment-variable name value)
  value)

(defun environment-with (overrides)
  "This process's environment with OVERRIDES — \"NAME=value\" strings — laid
over it: a name in OVERRIDES replaces every inherited entry of that name
(case-blind on Windows, as the OS reads them), the rest ride along."
  (flet ((name (entry) (subseq entry 0 (or (position #\= entry) (length entry)))))
    (let ((names (mapcar #'name overrides)))
      (append overrides
              (remove-if (lambda (entry)
                           (member (name entry) names
                                   :test #+win32 #'string-equal #-win32 #'string=))
                         (sb-ext:posix-environ))))))

;;; --- starting a child without copying this image --------------------------
;;; SB-EXT:RUN-PROGRAM forks, and a fork copies the page tables of the whole
;;; address space before the exec throws them away. The cost is this image's,
;;; not the child's, and it grows with the heap: 2.6 ms at a 150 MB heap,
;;; 6.4 ms at 400 MB, 26 ms at 900 MB (SBCL 2.6.7, huge pages off, as the
;;; organism runs). A turn pays two of those for every workspace capture and
;;; one more for every command a tool runs, so the organism's own size was
;;; the price of asking the operating system for anything.
;;;
;;; posix_spawn hands the kernel the argv, the environment, the working
;;; directory and the descriptor plan in one call and never copies this
;;; address space: 0.4 ms, whatever the heap holds. What RUN-PROGRAM gave a
;;; child is given here explicitly — its own process group, so KILL-TREE
;;; reaches its descendants; every descriptor above stderr closed, so no
;;; store handle or provider socket rides along; stdin on the null device —
;;; and a libc without the two _np actions that spell the last two falls
;;; back to the fork.

#-win32
(define-routines
  ("posix_spawnp" %posix-spawnp int (pid (* int)) (file str) (actions sap) (attributes sap)
   (argv sap) (envp sap))
  ("posix_spawn_file_actions_init" %spawn-actions-init int (actions sap))
  ("posix_spawn_file_actions_destroy" %spawn-actions-destroy int (actions sap))
  ("posix_spawn_file_actions_adddup2" %spawn-adddup2 int (actions sap) (descriptor int) (as int))
  ("posix_spawn_file_actions_addopen" %spawn-addopen int (actions sap) (descriptor int)
   (path str) (flags int) (mode sb-alien:unsigned-int))
  ("posix_spawn_file_actions_addchdir_np" %spawn-addchdir int (actions sap) (path str))
  ("posix_spawn_file_actions_addclosefrom_np" %spawn-addclosefrom int (actions sap) (from int))
  ("posix_spawnattr_init" %spawnattr-init int (attributes sap))
  ("posix_spawnattr_destroy" %spawnattr-destroy int (attributes sap))
  ("posix_spawnattr_setflags" %spawnattr-setflags int (attributes sap) (flags sb-alien:short))
  ("posix_spawnattr_setpgroup" %spawnattr-setpgroup int (attributes sap) (group int)))

;;; 0x02 on glibc and on Apple's libc alike.
(defconstant +spawn-setpgroup+ 2
  "POSIX_SPAWN_SETPGROUP — the child leads a process group of its own.")

;;; Both are opaque, both are far smaller than this on every libc, and the
;;; init call that follows writes their real shape.
(defconstant +spawn-opaque-bytes+ 512
  "Room for a posix_spawn_file_actions_t and a posix_spawnattr_t.")

;;; Probed per process, never at build time: a dumped image must not carry the
;;; build machine's libc.
(define-startup-parameter *spawn-primitive*
    #+win32 :fork
    #-win32
    ;; The rest of the family is POSIX.1-2001 and comes with posix_spawnp;
    ;; the two _np actions are what a libc may lack.
    (if (every (lambda (name)
                 (ignore-errors (sb-sys:find-dynamic-foreign-symbol-address name)))
               '("posix_spawnp" "posix_spawn_file_actions_addchdir_np"
                 "posix_spawn_file_actions_addclosefrom_np"))
        :posix
        :fork)
  ":POSIX when this machine's libc carries posix_spawn and the actions a
child of this image needs, :FORK when it does not: how this process starts a
child, posix_spawn or SB-EXT:RUN-PROGRAM.")

#-win32
(defmacro with-c-string-vector ((sap strings) &body body &aux (block (gensym "BLOCK"))
                                                              (size (gensym "SIZE")))
  "SAP bound to a null-terminated C array of the STRINGS, freed on exit —
the argv and envp shape every exec call takes, built the way SBCL's own
RUN-PROGRAM builds it."
  `(multiple-value-bind (,block ,sap ,size)
       (sb-impl::string-list-to-c-strvec (coerce ,strings 'list))
     (with-cleanup ((sb-sys:deallocate-system-memory ,block ,size))
       ,@body)))

#-win32
(defun %spawn-posix (argv directory environment output errors input)
  "ARGV started through posix_spawn, its stdout as OUTPUT says -- :STREAM, a
pipe; a native namestring, appended to that file -- and its stderr as ERRORS
says: :STREAM, a pipe of its own; :OUTPUT, joined to stdout; a namestring.
=> (values PID OUTPUT-STREAM ERROR-STREAM NIL INPUT-STREAM)."
  (let (out-read out-write err-read err-write in-read in-write pid)
    (with-cleanup ((unless pid
                     (dolist (descriptor (list out-read out-write err-read err-write
                                               in-read in-write))
                       (when descriptor (ignore-errors (sb-posix:close descriptor))))))
      (let ((actions (make-array +spawn-opaque-bytes+ :element-type '(unsigned-byte 8)))
            (attributes (make-array +spawn-opaque-bytes+ :element-type '(unsigned-byte 8))))
        (setf (values out-read out-write) (and (eq output :stream) (sb-posix:pipe)))
        (setf (values err-read err-write) (and (eq errors :stream) (sb-posix:pipe)))
        (setf (values in-read in-write) (and (eq input :stream) (sb-posix:pipe)))
        ;; Lisp octet vectors, so they arrive zeroed and need no freeing; pinned
        ;; for as long as libc holds their addresses.
        (sb-sys:with-pinned-objects (actions attributes)
          (let ((actions-sap (sb-sys:vector-sap actions))
                (attributes-sap (sb-sys:vector-sap attributes)))
            (%spawn-actions-init actions-sap)
            (%spawnattr-init attributes-sap)
            (with-cleanup ((%spawn-actions-destroy actions-sap)
                           (%spawnattr-destroy attributes-sap))
              (sb-alien:with-alien ((child sb-alien:int))
                (%spawnattr-setpgroup attributes-sap 0)
                (%spawnattr-setflags attributes-sap +spawn-setpgroup+)
                ;; In order: the working directory, then stdout and
                ;; stderr -- a pipe, or a file opened for appending --
                ;; stderr joined by duplicating the stdout already in
                ;; place, then the input pipe onto stdin while its
                ;; descriptor is still open, then every other descriptor this
                ;; image holds closed, and the null device as stdin
                ;; when there was no pipe. The stdin dup2 has to
                ;; precede the closefrom: file actions run in the
                ;; order they were added, and the closefrom would
                ;; otherwise take the read end away first.
                (when directory (%spawn-addchdir actions-sap directory))
                (flet ((place (descriptor target)
                         (if (stringp target)
                             (%spawn-addopen actions-sap descriptor target
                                             (logior sb-posix:o-wronly sb-posix:o-creat
                                                     sb-posix:o-append)
                                             #o644)
                             (%spawn-adddup2 actions-sap target descriptor))))
                  (place 1 (or out-write output))
                  (place 2 (case errors (:output 1) (:stream err-write) (t errors))))
                (when in-read (%spawn-adddup2 actions-sap in-read 0))
                (%spawn-addclosefrom actions-sap 3)
                (unless in-read
                  (%spawn-addopen actions-sap 0 "/dev/null" sb-posix:o-rdonly 0))
                (with-c-string-vector (argv-sap argv)
                  (with-c-string-vector (envp-sap (or environment
                                                      (sb-ext:posix-environ)))
                    (let ((code (%posix-spawnp (sb-alien:addr child) (first argv)
                                               actions-sap attributes-sap
                                               argv-sap envp-sap)))
                      (unless (zerop code)
                        ;; A file action can be what failed -- a chdir into a
                        ;; directory that is gone returns ENOENT -- so name
                        ;; it, or a missing working directory reads as a
                        ;; missing program.
                        (error "spawning ~a~@[ in ~a~]: ~a" (first argv) directory
                               (sb-int:strerror code)))
                      (setf pid child))))))))
        (dolist (descriptor (list out-write err-write in-read))
          (when descriptor (sb-posix:close descriptor)))
        (flet ((child-stream (descriptor direction name)
                 (when descriptor
                   (sb-sys:make-fd-stream descriptor direction t :external-format :utf-8
                                                     :buffering :full :auto-close t :name name))))
          (values pid
                  (child-stream out-read :input "child stdout")
                  (child-stream err-read :input "child stderr")
                  nil
                  (child-stream in-write :output "child stdin")))))))

(defun spawn-program (argv &key directory environment (output :stream) (error-output :stream)
                                (input :null))
  "Start ARGV — the program first, searched on PATH — as a child of this
process: DIRECTORY its working directory when given, ENVIRONMENT
(\"NAME=value\" strings) its whole environment, this process's own when NIL."
  ;; OUTPUT is :STREAM for a stdout pipe, NIL for the null device, or a
  ;; pathname the child's stdout is appended to, created when absent.
  ;; ERROR-OUTPUT takes the same three, or :OUTPUT to join stderr to stdout in
  ;; the child, so one stream carries both in the order the child wrote them.
  ;; A child that leaves a daemon behind -- a clipboard owner, a browser --
  ;; wants NIL for both: the daemon inherits whatever pipe it was handed and
  ;; holds it open past its parent's exit. INPUT is :NULL
  ;; for the null device on stdin, or :STREAM for a pipe the parent writes the
  ;; child's stdin down — a child this image talks to rather than only reads.
  ;; The child leads its own process group, so KILL-TREE reaches what it starts.
  ;; => (values PID OUTPUT-STREAM ERROR-STREAM PROCESS INPUT-STREAM): each
  ;; stream NIL unless its pipe was asked for, PROCESS the SB-EXT:PROCESS when
  ;; the fork path started it and NIL when posix_spawn did. WAIT-FOR-CHILD takes PID and PROCESS and
  ;; the caller closes the streams.
  (flet ((target (value)
           (if (or (null value) (keywordp value)) value (uiop:native-namestring value))))
    (let ((directory (and directory
                          (uiop:native-namestring
                           (uiop:ensure-directory-pathname directory))))
          (output (target output))
          (errors (target error-output)))
      (if (eq *spawn-primitive* :posix)
          #-win32 (%spawn-posix argv directory environment (or output "/dev/null")
                                (or errors "/dev/null") input)
          #+win32 (error "no posix_spawn on this platform")
          (let ((process (apply #'sb-ext:run-program (first argv) (rest argv)
                                :search t :wait nil
                                :input (and (eq input :stream) :stream)
                                :output output :if-output-exists :append
                                :error errors :if-error-exists :append
                                :external-format :utf-8
                                :directory directory
                                (when environment
                                  (list :environment environment)))))
            (values (sb-ext:process-pid process)
                    (and (eq output :stream) (sb-ext:process-output process))
                    (and (eq errors :stream) (sb-ext:process-error process))
                    process
                    (and (eq input :stream) (sb-ext:process-input process))))))))

#-win32
(defun wait-status-code (status otherwise)
  "The waitpid STATUS as an exit status: the code the child exited with, 128
plus the signal that ended it, else OTHERWISE."
  (cond ((sb-posix:wifexited status) (sb-posix:wexitstatus status))
        ((sb-posix:wifsignaled status) (+ 128 (sb-posix:wtermsig status)))
        (t otherwise)))

(defun wait-for-child (pid process)
  "Block until the child exits — PID and PROCESS as SPAWN-PROGRAM answered
them."
  ;; => its exit status: the code it exited with, or 128 plus the signal
  ;; that ended it.
  (if process
      (progn (sb-ext:process-wait process)
             (child-exit pid process))
      #-win32
      (loop
        (handler-case
            (return (wait-status-code (nth-value 1 (sb-posix:waitpid pid 0)) 1))
          ;; A signal the image handles — SIGCHLD for a child RUN-PROGRAM
          ;; started, a timer — interrupts the wait, never ends it.
          (sb-posix:syscall-error (condition)
            (unless (eql (sb-posix:syscall-errno condition) sb-posix:eintr)
              (return 1)))))
      #+win32 1))

(defun child-exit (pid process)
  "The child's exit status once it has exited — PID and PROCESS as
SPAWN-PROGRAM answered them — or NIL while it runs: WAIT-FOR-CHILD's answer,
without the wait."
  ;; A child started without a PROCESS is reaped here the moment it is seen
  ;; to have exited, so ask this, or WAIT-FOR-CHILD, never both after it has.
  (if process
      (unless (sb-ext:process-alive-p process)
        (let ((code (sb-ext:process-exit-code process)))
          (if (eq (sb-ext:process-status process) :signaled) (+ 128 code) code)))
      #-win32
      (handler-case
          (multiple-value-bind (reaped status) (sb-posix:waitpid pid sb-posix:wnohang)
            (when (plusp reaped)
              (wait-status-code status 1)))
        ;; No such child any longer: whatever it was, it has ended.
        (sb-posix:syscall-error () 1))
      #+win32 1))

(defun %join-bounded (thread seconds)
  "THREAD's value, or :TIMEOUT when it is still running SECONDS from now."
  (with-handlers ((error () :timeout))
    (sb-thread:join-thread (bt2:thread-native-thread thread)
                           :default :timeout :timeout seconds)))

(defun %feed (out text)
  "Write TEXT down OUT and close it, so a child reading its stdin to the end
sees the end; a child that stopped reading first is not an error here."
  (unless (ignore-errors (write-string text out) (close out) t)
    (ignore-errors (close out :abort t))))

(defun run-bounded (argv &key (seconds 60) directory environment input
                             (output :string) (error-output :string))
  "Run ARGV — the program first, searched on PATH — to completion or SECONDS,
whichever comes first, stdin the string INPUT when given (written whole, then
closed) and empty otherwise, in DIRECTORY when given, under ENVIRONMENT
(\"NAME=value\" strings laid over this process's, see ENVIRONMENT-WITH)."
  ;; => (values OUTPUT ERROR-OUTPUT STATUS): the child's
  ;; stdout and stderr as strings — NIL for one whose OUTPUT or ERROR-OUTPUT
  ;; was NIL, sent to the null device (SPAWN-PROGRAM) — and its exit status — 128 plus the signal
  ;; that ended it when a signal did, :TIMEOUT when the bound fell first and the
  ;; child's whole tree was killed (KILL-TREE: a shell's sleeping grandchild
  ;; would otherwise hold the pipe open past the bound).
  ;;
  ;; The child is started without forking this image (SPAWN-PROGRAM). Nothing
  ;; here polls: both pipes drain on threads of their own, so a child that
  ;; fills one while the other is read cannot wedge, and a third thread parks
  ;; in wait for the exit so the bound is a timed join rather than a sleep
  ;; loop — the bound is enforced in the image, so no `timeout' binary (GNU,
  ;; absent on macOS and Windows) stands between the organism and its child.
  ;; INPUT is written on a thread of its own for the same reason: a string
  ;; longer than the pipe holds would otherwise block the write against a
  ;; child blocked writing its answer to a pipe nobody reads yet.
  (multiple-value-bind (pid output-stream error-stream process input-stream)
      (spawn-program argv :directory directory
                          :environment (and environment
                                            (environment-with environment))
                          :output (and output :stream)
                          :error-output (and error-output :stream)
                          :input (if input :stream :null))
    (let* ((output (and output-stream (make-string-output-stream)))
           (errors (and error-stream (make-string-output-stream)))
           (readers (append
                     (and output (list (spawn "run-bounded stdout" (%drain output-stream output))))
                     (and errors (list (spawn "run-bounded stderr" (%drain error-stream errors))))))
           (feeder (and input-stream (spawn "run-bounded stdin" (%feed input-stream input))))
           (waiter (spawn "run-bounded wait" (wait-for-child pid process)))
           (status (%join-bounded waiter seconds)))
      (with-cleanup ((when output-stream (ignore-errors (close output-stream)))
                     (when error-stream (ignore-errors (close error-stream)))
                     (when input-stream (ignore-errors (close input-stream :abort t)))
                     (when process (ignore-errors (sb-ext:process-close process))))
        (when (eq status :timeout)
          (kill-tree pid)
          (%join-bounded waiter 5))
        ;; A killed child's own children may hold the pipes open; the
        ;; drain is bounded so a stray grandchild cannot park the caller.
        (when feeder (%join-bounded feeder 5))
        (dolist (reader readers) (%join-bounded reader 5))
        (values (and output (get-output-stream-string output))
                (and errors (get-output-stream-string errors))
                status)))))

(defun exits-zero-p (argv &key input)
  "T when ARGV runs and exits 0 within a minute, INPUT (a string) on its
stdin; NIL when it fails or is not installed."
  ;; Neither output is read, so neither is a pipe: a clipboard helper
  ;; (wl-copy, xclip) forks the owner that serves the selection, and that
  ;; owner would hold a pipe open for as long as it serves.
  (ignore-errors
   (eql 0 (nth-value 2 (run-bounded argv :input input :output nil :error-output nil)))))

(defun run-checked (argv refuse &key (seconds 600))
  "ARGV run through RUN-BOUNDED, SECONDS at most."
  ;; => its output; unless it
  ;; exits 0, REFUSE is called first with its error output, newlines trimmed.
  (multiple-value-bind (out err status) (run-bounded argv :seconds seconds)
    (unless (eql status 0)
      (funcall refuse (string-trim '(#\Newline) err)))
    out))

;;; --- entropy -----------------------------------------------------------------

(defun random-bytes (count)
  "COUNT bytes from the operating system's entropy source, as an octet
vector: /dev/urandom, or on Windows the cryptographic provider SBCL seeds
its own random state from (CryptGenRandom)."
  ;; Signals when the source cannot answer; a token minted from anything less
  ;; is not a secret.
  #-win32
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (with-open-file (stream "/dev/urandom" :element-type '(unsigned-byte 8))
      (unless (= count (read-sequence bytes stream))
        (error "/dev/urandom answered fewer than ~d bytes" count)))
    bytes)
  #+win32
  (multiple-value-bind (bytes code) (sb-win32::crypt-gen-random count)
    (unless bytes
      (error "CryptGenRandom refused ~d bytes: Windows error ~d" count code))
    bytes))

;;; --- files -------------------------------------------------------------------

;;; Load-bearing, not bookkeeping: a POSIX record lock dies the moment ANY
;;; descriptor for that file is closed by the holding process, so a holder
;;; that merely PROBED its own lock would lose it — FILE-LOCKED-P answers from
;;; here instead of opening the file. Without this, a probe would let another
;;; process take the lock, leaving two holders on the file.
(defvar *held-locks* (make-hash-table :test #'eql :synchronized t)
  "LOCK -> the native path it holds, for every lock this process took.")

(defun note-held-lock (lock path)
  (setf (gethash lock *held-locks*) path)
  lock)

(defun lock-file (path &key wait &aux (native (uiop:native-namestring path)))
  "Take the exclusive lock on the file at PATH, creating it, and answer the
lock — a descriptor, a handle on Windows — or NIL when another live process
holds it. WAIT waits for that process to let go instead, NIL then only when
the kernel will not lock the file at all (POSIX: Windows answers at once)."
  (declare (ignorable wait))
  ;; The kernel releases a lock with its process, so a crash cannot
  ;; wedge the next taker: an fcntl record lock (per process, unshared by fork)
  ;; on POSIX; on Windows the file opened with no sharing, which is the same
  ;; promise spelled the other way round. The lock is remembered in
  ;; *HELD-LOCKS*, which is what keeps a later probe from closing it. Signals
  ;; when the file cannot be opened at all. The descriptor closes at an exec
  ;; (FD_CLOEXEC): a successor would hold a lock it does not know of for
  ;; life. A wait is the kernel's, never a poll, taken again when a signal
  ;; ends it early.
  #-win32
  (let ((fd (sb-posix:open native (logior sb-posix:o-creat sb-posix:o-rdwr) #o600)))
    (sb-posix:fcntl fd sb-posix:f-setfd 1)
    (loop (handler-case (progn (sb-posix:lockf fd (if wait sb-posix:f-lock sb-posix:f-tlock) 0)
                               (return (note-held-lock fd native)))
            (sb-posix:syscall-error (condition)
              (unless (and wait (eql (sb-posix:syscall-errno condition) sb-posix:eintr))
                (sb-posix:close fd)
                (return nil))))))
  #+win32
  (let ((handle (%create-file native +generic-read-write+ 0 nil
                              +open-always+ +file-attribute-normal+ 0)))
    (cond ((/= handle sb-win32::invalid-handle) (note-held-lock handle native))
          ((= (sb-win32::get-last-error) +error-sharing-violation+) nil)
          (t (error "cannot lock ~a: Windows error ~d"
                    path (sb-win32::get-last-error))))))

(defun file-locked-p (path &aux (native (uiop:native-namestring path)))
  "Whether a live process holds the lock on the file at PATH: a probe,
never a claim, so it cannot contend with the holder, and NIL when the file
does not exist."
  ;; A lock THIS process holds answers T from *HELD-LOCKS* without the file
  ;; being opened at all — opening it would be the probe that destroys it,
  ;; since POSIX releases every record lock a process holds on a file as soon
  ;; as it closes any descriptor for it. Another process's lock reads as held
  ;; on every platform.
  (or (sb-ext:with-locked-hash-table (*held-locks*)
        (and (member native (alexandria:hash-table-values *held-locks*) :test #'equal) t))
      #-win32
      (handler-case
          (let ((fd (sb-posix:open native sb-posix:o-rdwr)))
            (with-cleanup ((sb-posix:close fd))
              (handler-case (progn (sb-posix:lockf fd sb-posix:f-test 0) nil)
                (sb-posix:syscall-error () t))))
        (sb-posix:syscall-error () nil))
      #+win32
      (let ((handle (%create-file native +generic-read-write+ 0 nil
                                  +open-existing+ +file-attribute-normal+ 0)))
        (cond ((/= handle sb-win32::invalid-handle)
               (sb-win32::close-handle handle)
               nil)
              (t (= (sb-win32::get-last-error) +error-sharing-violation+))))))

(defun unlock-file (lock)
  "Release LOCK, a LOCK-FILE answer, by closing what holds it, and forget
it: the next probe of that path asks the file again."
  (remhash lock *held-locks*)
  #-win32 (sb-posix:close lock)
  #+win32 (sb-win32::close-handle lock)
  nil)

;;; --- a native path, as text -------------------------------------------------
;;; Joined and compared as strings and never parsed, so `*' and `[' are only
;;; characters in a name, which a pathname would read as wildcards.

(defparameter +path-separators+ #+win32 "/\\" #-win32 "/"
  "What separates the steps of a path: `/', and on Windows `\\' as well, which
Windows walks as it walks `/'.")

(defun native-path (path &aux (native (string-right-trim +path-separators+
                                                         (if (pathnamep path) (sb-ext:native-namestring path) path))))
  "PATH — a string or a pathname — as a native path with no trailing slash
but a Windows drive's root, which keeps its own."
  ;; Windows spells a directory with a closing `\', and its stat refuses one
  ;; that keeps it: a session's folder answered 404 at its own root.
  #+win32 (if (and (= (length native) 2) (char= #\: (char native 1))) (concatenate 'string native "\\") native)
  #-win32 native)

(defun path-under (root path)
  "PATH, relative and `/'-separated, under ROOT, a native directory with no
trailing slash: joined as text and never parsed, so `*' and `[' are only
characters in a name."
  (concatenate 'string root "/" path))

(defun path-parent (path &aux (slash (position #\/ path :from-end t)))
  "PATH without its last `/'-separated step, or NIL when it has only one."
  (and slash (subseq path 0 slash)))

(defun path-within-p (path directory)
  "Whether the native PATH is DIRECTORY or lies beneath it, both with no
trailing slash but a Windows drive's root, which keeps its own."
  ;; A native path on Windows is spelled with `\' (REAL-PATH's): held to `/'
  ;; alone, nothing beneath a folder there lay beneath it.
  (or (string= path directory)
      (and (< (length directory) (length path))
           (uiop:string-prefix-p directory path)
           (or (find (char path (length directory)) +path-separators+)
               (and (plusp (length directory))
                    (find (char directory (1- (length directory))) +path-separators+))))))

(defun directory-names (directory)
  "The names in the native DIRECTORY, `.' and `..' aside; NIL when it cannot
be read. The second value is what each one is, by the listing itself and in
the same order: :DIRECTORY, :FILE (a regular one), :LINK, :OTHER, or NIL
where the file system does not say."
  ;; The listing says it without a stat: a walk that wants the folders among
  ;; many names would otherwise pay a stat a name.
  (with-handlers ((error () nil))
    (let ((handle (sb-posix:opendir directory)))
      (with-cleanup ((sb-posix:closedir handle))
        (loop for entry = (sb-posix:readdir handle)
              until (sb-alien:null-alien entry)
              for name = (sb-posix:dirent-name entry)
              unless (member name '("." "..") :test #'string=)
                collect name into names
                and collect (dirent-kind entry) into kinds
              finally (return (values names kinds)))))))

(defun dirent-kind (entry)
  "The kind the directory ENTRY's d_type says (DIRECTORY-NAMES)."
  ;; d_type is the byte before d_name on Linux and macOS alike. Declared, the
  ;; slot is an offset; undeclared, a lookup per call.
  #+(or linux darwin)
  (declare (type (sb-alien:alien (* sb-posix::dirent)) entry))
  #+(or linux darwin)
  (let* ((sap (sb-alien:alien-sap entry))
         (name (- (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:slot entry 'sb-posix::name)))
                  (sb-sys:sap-int sap))))
    (case (sb-sys:sap-ref-8 sap (1- name))
      (0 nil)
      (4 :directory)
      (8 :file)
      (10 :link)
      (t :other)))
  #-(or linux darwin)
  (progn entry nil))

(defun lstat-kind (path)
  "What is at the native PATH, a last link not followed: :DIRECTORY, :FILE (a
regular one), :LINK, :OTHER or :ABSENT, or :UNKNOWN when the question itself
was refused. The stat is the second value."
  (handler-case
      (let* ((stat (sb-posix:lstat path))
             (kind (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt)))
        (values (cond ((= kind sb-posix:s-ifdir) :directory)
                      ((= kind sb-posix:s-ifreg) :file)
                      ((= kind sb-posix:s-iflnk) :link)
                      (t :other))
                stat))
    (sb-posix:syscall-error (condition)
      (if (member (sb-posix:syscall-errno condition) (list sb-posix:enoent sb-posix:enotdir))
          :absent
          :unknown))))

(defun claim-temp-beside (path claim)
  "Call CLAIM with fresh names beside PATH until one is not taken, CLAIM
signalling EEXIST for one that is. => the name claimed."
  ;; A name in PATH's own directory, for a temp renamed over PATH:
  ;; `.nodecode-<pid>-<random>': hidden, and of one length whatever PATH's
  ;; own name, so a name at the filesystem's limit still has a temp beside
  ;; it. Claimed exclusively, so a name already taken -- by a file, or by a
  ;; link waiting to be followed -- is a retry, never a write through it.
  (loop repeat 8
        for temp = (path-under (or (path-parent path) ".")
                               (format nil ".nodecode-~d-~36r" (sb-posix:getpid)
                                       (random (expt 36 8))))
        when (with-handlers ((sb-posix:syscall-error (condition)
                               (unless (= sb-posix:eexist (sb-posix:syscall-errno condition))
                                 (error condition))))
               (funcall claim temp)
               t)
          return temp
        finally (error "no free temp name beside ~a" path)))

;;; --- a folder's lock: what two homes that share one folder contend for ------
;;;
;;; A profile may share a folder rather than copy it (profile.lisp links
;;; secrets/ and the knowledge cells into a new home), which makes two
;;; processes writers of one directory. LOCK-FILE is the claim; what this section adds is where the
;;; lock file is, asked so that both homes answer one path -- the folder's
;;; symbolic links resolved first -- and a bounded wait, because a writer that
;;; cannot take the lock of the folder it shares must not write at all.

(defparameter *folder-lock-wait* 5
  "Seconds LOCK-FOLDER waits for another process to let go before answering
NIL: a write holds the lock for milliseconds, so anything longer is a writer
that is stuck.")

(defvar *folder-locks* (make-hash-table :test #'equal :synchronized t)
  "Native lock-file name -> (LOCK . COUNT) for every folder lock this process
holds. Re-entrancy lives here and nowhere else: POSIX takes a second record
lock from this process without a word, and CLOSING that second descriptor
releases the first lock with it.")

(defun folder-lock-path (directory)
  "The file that stands for the folder DIRECTORY: in a `.locks' directory
beside it, named after it -- ~/.nodecode/cells/you-memory/ and
~/.nodecode/cells/.locks/you-memory.lock -- with DIRECTORY's symbolic links resolved
first, so two homes sharing one folder contend for one lock rather than two.
A dot-directory, so the folder's own repository never carries it:
GIT-EXCLUDE-DOT-DIRECTORIES keeps those out of every commit."
  (let* ((resolved (uiop:ensure-directory-pathname
                    (or (ignore-errors (truename directory)) (pathname directory))))
         (name (car (last (pathname-directory resolved)))))
    (merge-pathnames (concatenate 'string ".locks/" name ".lock")
                     (uiop:pathname-parent-directory-pathname resolved))))

(defun lock-folder (directory &key (wait *folder-lock-wait*))
  "The exclusive lock on DIRECTORY's shared folder (FOLDER-LOCK-PATH), waited
for up to WAIT seconds. => the lock, or NIL while another process holds it.
Re-entrant within this process, counted in *FOLDER-LOCKS*."
  (let* ((path (folder-lock-path directory))
         (native (uiop:native-namestring path)))
    ;; The .locks directory is ours to make: LOCK-FILE opens with O_CREAT,
    ;; which fails on a folder that is not there yet (the first writer of a
    ;; folder nobody has written). Its value — the pathname — is not a lock,
    ;; so it must not stand in the OR below.
    (ensure-directories-exist path)
    (or (sb-ext:with-locked-hash-table (*folder-locks*)
          (when-let (held (gethash native *folder-locks*))
            (incf (cdr held))
            (car held)))
        ;; A writer stuck with the lock cannot hold this one for long; past
        ;; the deadline the answer is NIL, and WITH-FOLDER-LOCK turns that
        ;; into a refusal.
        ;; LOCK-FILE's answer, asked every 50 ms until it is a lock or WAIT is spent.
        (when-let (lock (wait-until (lambda () (lock-file path)) (deadline-after wait) 0.05))
          (setf (gethash native *folder-locks*) (cons lock 1))
          lock))))

(defun unlock-folder (directory &aux (path (folder-lock-path directory))
                                     (native (uiop:native-namestring path)))
  "Release this process's hold on DIRECTORY's folder lock, the last holder
out closing it. => T when the lock was forgotten."
  (sb-ext:with-locked-hash-table (*folder-locks*)
    (let ((held (gethash native *folder-locks*)))
      (cond ((null held) nil)
            ((> (cdr held) 1) (decf (cdr held)) t)
            (t (remhash native *folder-locks*)
               (unlock-file (car held))
               t)))))

(defmacro with-folder-lock ((directory &key (wait '*folder-lock-wait*)) &body body)
  "Run BODY holding DIRECTORY's folder lock, released however BODY ends.
DIRECTORY is an expression. Signals when another process has held it for WAIT
seconds: a writer that cannot take the lock of a folder it shares must not
write, or two homes write one index from two stale reads."
  (let ((lock (gensym "LOCK")) (dir (gensym "DIRECTORY")))
    `(let* ((,dir ,directory)
            (,lock (lock-folder ,dir :wait ,wait)))
       (unless ,lock
         (error "~a is held by another process writing to that folder; retry in a moment"
                (namestring (folder-lock-path ,dir))))
       (with-cleanup ((unlock-folder ,dir)) ,@body))))

(defmacro with-output-file ((stream path) &body body &aux (file (gensym "PATH")))
  "Run BODY with STREAM writing the file at PATH, created or replaced, its
directory made first."
  `(let ((,file ,path))
     (ensure-directories-exist ,file)
     (with-open-file (,stream ,file :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
       ,@body)))

(defun flush-file (stream)
  "Push STREAM's file — an fd-stream — through to the disk: fsync, or
FlushFileBuffers on Windows. Best effort; a refusal is not an error."
  (let ((fd (sb-sys:fd-stream-fd stream)))
    #-win32 (ignore-errors (sb-posix:fsync fd))
    #+win32 (ignore-errors (%flush-file-buffers (sb-win32::get-osfhandle fd)))))

(defun replace-file (from to)
  "Rename FROM over TO atomically, TO replaced if it exists: rename(2), and
on Windows MoveFileEx with replacement."
  ;; Not CL:RENAME-FILE, which merges the target's missing type from the
  ;; source — "nodecode" would quietly stay "nodecode.tmp-<pid>". => TO.
  (multiple-value-bind (ok errno)
      (sb-unix:unix-rename (uiop:native-namestring from) (uiop:native-namestring to))
    (unless ok
      (error "cannot replace ~a with ~a: ~a" to from
             (or (ignore-errors (sb-int:strerror errno)) errno)))
    to))

(defun real-path (path)
  "PATH with every link on the way to it followed, as a pathname, or NIL
when nothing is there."
  ;; TRUENAME follows a symbolic link, and on Windows leaves a junction -- the
  ;; installer's `current' -- as-is: a binary started through one would take
  ;; `current' for the release it is. The file's own handle says where it lies,
  ;; as \\?\C:\... or \\?\UNC\host\share\...
  #-win32 (ignore-errors (truename path))
  #+win32
  (let ((handle (%create-file (uiop:native-namestring path) 0 +file-share-all+ nil
                              +open-existing+ +file-flag-backup-semantics+ 0)))
    (unless (= handle sb-win32::invalid-handle)
      (with-cleanup ((sb-win32::close-handle handle))
        (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
          (sb-sys:with-pinned-objects (buffer)
            (let ((units (%get-final-path-name handle (sb-sys:vector-sap buffer) 1024 0)))
              (when (< 0 units 1024)
                (let ((name (sb-ext:octets-to-string buffer :external-format :utf-16le
                                                            :end (* 2 units))))
                  (uiop:parse-native-namestring
                   (cond ((uiop:string-prefix-p "\\\\?\\UNC\\" name)
                          (concatenate 'string "\\\\" (subseq name 8)))
                         ((uiop:string-prefix-p "\\\\?\\" name) (subseq name 4))
                         (t name))
                   :ensure-directory (uiop:directory-pathname-p path)))))))))))

(defun file-bytes (path)
  "The size of the file at PATH in bytes, or NIL when it cannot be opened."
  (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))

(defun claim-file (path stale-seconds)
  "Create the file at PATH exclusively, this process's pid in it, as a claim
another process honours: one there already is broken, once, when it is
STALE-SECONDS old or older, so a claimant that crashed holds nothing forever.
=> T when this process holds the claim."
  (flet ((create ()
           (with-open-file (out path :direction :output :if-exists nil
                                     :if-does-not-exist :create)
             (when out (format out "~d~%" (sb-posix:getpid)) t))))
    (or (create)
        (let ((date (ignore-errors (file-write-date path))))
          (when (and date (>= (- (get-universal-time) date) stale-seconds))
            (ignore-errors (delete-file path))
            (create))))))

(defun file-octets (path &optional limit)
  "The bytes of the file at PATH, or NIL when it cannot be read or is longer
than LIMIT: the one bounded read the image seams and the sidecars share."
  ;; Off Windows, the descriptor alone: OPEN would make a stream and its buffer
  ;; and ask access(2) first, adding overhead the descriptor path avoids.
  (with-handlers ((error () nil))
    #-win32
    (let ((fd (sb-posix:open (sb-ext:native-namestring (merge-pathnames path) :as-file t)
                             sb-posix:o-rdonly)))
      (with-cleanup ((sb-posix:close fd))
        (let ((size (sb-posix:stat-size (sb-posix:fstat fd))))
          (when (or (null limit) (<= size limit))
            (descriptor-octets fd size)))))
    #+win32
    (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (when (and in (or (null limit) (<= (file-length in) limit)))
        (alexandria:read-stream-content-into-byte-vector in :initial-size (file-length in))))))

#-win32
(defun descriptor-octets (fd size &aux (octets (make-array size :element-type '(unsigned-byte 8)))
                                       (end 0))
  "Every byte left to read from the descriptor FD, SIZE of them expected: more
when the file grew since, or says no size, as a file under /proc does."
  (flet ((into (vector start)
           (sb-sys:with-pinned-objects (vector)
             (sb-posix:read fd (sb-sys:sap+ (sb-sys:vector-sap vector) start) (- (length vector) start)))))
    (loop
      (if (< end (length octets))
          (let ((read (into octets end)))
            (when (zerop read)
              (return (if (= end (length octets)) octets (subseq octets 0 end))))
            (incf end read))
          ;; full: the end of the file, or a file longer than it said
          (let* ((probe (make-array 4096 :element-type '(unsigned-byte 8)))
                 (read (into probe 0)))
            (when (zerop read)
              (return octets))
            (setf octets (replace (make-array (+ (* 2 end) read) :element-type '(unsigned-byte 8))
                                  octets)
                  octets (replace octets probe :start1 end :end2 read)
                  end (+ end read)))))))

;;; --- the process's stdio -----------------------------------------------------
;;; A TUI paints through the terminal core, which opens the terminal itself;
;;; while it runs the process's own descriptors 0, 1 and 2 have no legitimate
;;; reader or writer, and anything that writes them directly — a child handed
;;; an fd-stream, foreign code's printf, an SBCL low-level report — paints
;;; over the viewport. Severing them removes the whole
;;; class: fd 0 reads the null device, fds 1 and 2 append to a sidecar log,
;;; and children inherit the severed set. Retention: the log is a debug-only
;;; sink; fd-level bytes cannot be attributed to a tool call, so no lane
;;; projects them into the transcript.

#+win32
(defun %std-handles-follow-fds ()
  "Point the process-wide std handles at the HANDLEs behind descriptors 0, 1
and 2, so a child that inherits the defaults and foreign code that asks
GetStdHandle see what the C runtime descriptors now see."
  (loop for which in (list +std-input-handle+ +std-output-handle+ +std-error-handle+)
        for fd from 0
        do (%set-std-handle which (sb-win32::get-osfhandle fd))))

(defun %stdio-onto (fds &key quietly)
  "Lisp's own buffered output flushed through, then the descriptors FDS moved
onto 0, 1 and 2 in turn, the std handles following on Windows; QUIETLY a
move that refuses is passed over."
  (ignore-errors (finish-output *standard-output*))
  (ignore-errors (finish-output *error-output*))
  (loop for fd in fds
        for target from 0
        do (if quietly (ignore-errors (sb-posix:dup2 fd target)) (sb-posix:dup2 fd target)))
  #+win32 (when fds
            (if quietly (ignore-errors (%std-handles-follow-fds)) (%std-handles-follow-fds))))

(defvar *severed-stdio* nil
  "Duplicates of the process's own descriptors 0, 1 and 2 while the outermost
CALL-WITH-SEVERED-STDIO has them severed, else NIL: what an exec hands its
successor back (CALL-WITH-UNSEVERED-STDIO).")

(defun call-with-severed-stdio (log-path thunk &aux null-fd log-fd saved outermost)
  "Run THUNK with the process's OS-level stdio severed from the terminal:
fd 0 reads the null device (children see EOF instead of stealing raw-mode
keystrokes), fds 1 and 2 append to LOG-PATH."
  ;; Lisp streams already wrapping
  ;; those fds keep working and land in the log; child processes inherit the
  ;; severed fds. The original fds are restored on unwind, so post-TUI output
  ;; (resume hints, crash reports) reaches the shell again. A severance inside
  ;; another one saves the severed set, so only the outermost names the
  ;; originals (*SEVERED-STDIO*).
  ;;
  ;; Severance is skipped — THUNK runs bare — when the terminal cannot be
  ;; opened or any syscall refuses: with no controlling terminal there is no
  ;; viewport to protect, and the log must never eat the only visible output.
  ;; On Windows the terminal is CONOUT$ and the null device NUL, and the three
  ;; std handles follow the descriptors both ways.
  (with-cleanup (;; Stray Lisp-stream buffers flush into whatever fd 1/2 point at now
                 ;; (the log when severed), then the fds move back. Restoring a saved
                 ;; fd onto an unmoved original (the partial-severance states) is a
                 ;; harmless self-assignment.
                 (when outermost (setf *severed-stdio* nil))
                 (%stdio-onto saved :quietly t)
                 (dolist (fd (list* null-fd log-fd saved))
                   (when fd (ignore-errors (sb-posix:close fd)))))
    (with-handlers ((sb-posix:syscall-error () nil))
      (sb-posix:close (sb-posix:open #-win32 "/dev/tty" #+win32 "CONOUT$"
                                     sb-posix:o-rdwr))
      (setf null-fd (sb-posix:open #-win32 "/dev/null" #+win32 "NUL"
                                   sb-posix:o-rdonly)
            log-fd (sb-posix:open (uiop:native-namestring log-path)
                                  (logior sb-posix:o-wronly
                                          sb-posix:o-creat
                                          sb-posix:o-append)
                                  #o600))
      ;; SAVED holds the originals of 0, 1 and 2 in that order, as many
      ;; as were taken before a refusal.
      (dotimes (fd 3) (setf saved (nconc saved (list (sb-posix:dup fd)))))
      ;; Output written so far belongs on the terminal, not in
      ;; the log; flush it through the fds before they move.
      (%stdio-onto (list null-fd log-fd log-fd))
      (unless *severed-stdio*
        (setf *severed-stdio* saved outermost t))
      ;; fd 1 is the log from here: stamp whose bytes follow, so
      ;; a shared append-only file stays attributable per run.
      (format *standard-output* ";; stdio severed: pid ~d ~a~%"
              (sb-posix:getpid) (iso-now))
      (ignore-errors (finish-output *standard-output*)))
    (funcall thunk)))

(defun call-with-unsevered-stdio (thunk &aux (severed '()))
  "THUNK with the process's own descriptors back on 0, 1 and 2 while
CALL-WITH-SEVERED-STDIO has them severed, and severed again after: what THUNK
writes reaches the terminal, and an exec inside it hands its successor the
terminal, never the log."
  ;; A successor behind the frame (a warm image, the origin a stale image hands
  ;; back to) inherited fds 1 and 2 on the log, saved them as its own, and put
  ;; them back at its exit: its last lines -- the resume line, why a launch
  ;; could not start -- went to the log and the terminal showed nothing.
  ;; The severed set is held close-on-exec (FD_CLOEXEC, 1), so
  ;; a successor inherits none of it.
  (with-cleanup ((when severed
                   (%stdio-onto severed :quietly t)
                   (dolist (fd severed) (ignore-errors (sb-posix:close fd)))))
    (when *severed-stdio*
      (dotimes (fd 3)
        (let ((copy (sb-posix:dup fd)))
          #-win32 (sb-posix:fcntl copy sb-posix:f-setfd 1)
          (setf severed (nconc severed (list copy)))))
      (%stdio-onto *severed-stdio*))
    (funcall thunk)))

;;; --- memory: what the kernel does with the organism's pages ------------------
;;; The organism's resident size is mostly not its live data: it is what the
;;; kernel and the C allocator chose to back a touched byte with, and what
;;; stays mapped after a phase that read widely has ended. The asks are
;;; Linux-only and NIL elsewhere, so a caller reads the answer and never the
;;; platform.

(defun disable-huge-pages ()
  "Ask the kernel to back this process with ordinary pages from here on."
  ;; => T when the request was made, NIL where the platform has no such setting.
  ;;
  ;; With transparent huge pages on (`always' on this kernel), the first touch
  ;; of any 2 MB-aligned range of anonymous memory is backed by one 2 MB page.
  ;; A thread's stack is touched at its top and its struct is memset at
  ;; creation, so every thread costs two huge pages — some 4 MB resident for a
  ;; thread that holds a few hundred bytes of stack — and the runtime's guard
  ;; mprotect then splits them into ordinary pages that stay resident. A huge
  ;; page repays its size on a large hot heap; this program's memory is many
  ;; small idle regions. Process-wide (PR_SET_THP_DISABLE): every mapping made
  ;; after the call, which is why MAIN makes it first. The flag is inherited
  ;; across fork and exec, so children the organism spawns run without huge
  ;; pages too — the posture most distributions ship as their default.
  #+linux
  (zerop (%prctl 41 1 0 0 0))
  #-linux nil)

(defun cap-malloc-arenas ()
  "Ask the C allocator to serve every thread from at most two arenas."
  ;; => T when the request was made, NIL where the platform has no such setting.
  ;;
  ;; glibc hands a thread that finds its arena contended an arena of its own,
  ;; up to eight per core, and memory one arena frees serves only the threads
  ;; bound to that arena. What mallocs here is SQLite, TLS and the runtime's
  ;; thread setup — small and bursty, from many threads — while Lisp allocation
  ;; never passes through malloc. Without this, those allocations would scatter
  ;; across many arenas and keep their freed memory resident. M_ARENA_MAX (-8)
  ;; bounds the arenas made after the call, which is why MAIN makes it first.
  #+linux
  (= 1 (%mallopt -8 2))
  #-linux nil)

(defun resolve-names-directly ()
  "Have the C library resolve host names from /etc/hosts and DNS itself, not
through the plugins nsswitch.conf lists."
  ;; => T when asked, NIL where the platform has no such setting.
  ;;
  ;; systemd's plugins (resolve, mymachines) block SIGUSR2 for the length of
  ;; a lookup, and SIGUSR2 is how SBCL stops a thread for a collection.
  ;; Without this, a collection that lands during a lookup would park every
  ;; other thread where it stands -- one of them inside malloc, holding one
  ;; of the two arenas every thread shares (CAP-MALLOC-ARENAS) -- and the
  ;; lookup would then wait on that arena and could not park, and the
  ;; collector would wait on the lookup: the whole process would stop, input
  ;; and SIGTERM with it. /models asks every provider for its listing at
  ;; once, so lookups run beside TLS handshakes. On a system running
  ;; systemd-resolved, resolv.conf most often names its stub, which still
  ;; answers; a name only systemd's plugins know (a .local host, a
  ;; container) does not resolve in this process. Before the first lookup,
  ;; which is why MAIN asks first.
  #+linux
  (zerop (%nss-configure-lookup "hosts" "files dns"))
  #-linux nil)

(defun skip-openssl-exit-cleanup ()
  "Ask OpenSSL not to register its cleanup at exit(3)."
  ;; => T when asked, NIL where no libcrypto is loaded.
  ;;
  ;; OPENSSL_cleanup runs from exit(3) and takes OpenSSL's own locks. An
  ;; HTTPS read on another thread when exit ends can leave one held; without
  ;; this flag, the process would hang in exit forever, SIGTERM included.
  ;; Cleaning up a process that is leaving buys nothing. OPENSSL_INIT_NO_ATEXIT
  ;; is honoured only by the first initialisation, and cl+ssl initialises at
  ;; its first TLS use, which is why MAIN asks first.
  (when-let (address (sb-sys:find-foreign-symbol-address "OPENSSL_init_crypto"))
    (/= 0 (sb-alien:alien-funcall
           (sb-alien:sap-alien (sb-sys:int-sap address)
                               (function sb-alien:int (sb-alien:unsigned 64)
                                         sb-alien:system-area-pointer))
           #x00080000 (sb-sys:int-sap 0)))))

;;; The open-file limit. Every descriptor the organism holds -- a socket, a
;;; pipe, the store, a log -- counts against its soft limit, and past it
;;; nothing opens: no connection, and no name resolves either. macOS starts
;;; every login process at 256 (Linux at 1024), and a gateway that leaked
;;; sockets for a day and a half reached it (Sai's Mac, 2026-10-21: 240 of
;;; 256 descriptors on sockets to the release host, then "host not found"
;;; for every request). The hard limit is the ceiling a process may raise
;;; itself to; SELECT-SAFE-DESCRIPTORS caps the raise.

(defconstant +select-safe-descriptors+ 1024
  "FD_SETSIZE: the highest descriptor count select(2) can watch. SBCL's own
waits poll (WAIT-UNTIL-FD-USABLE, SERVE-EVENT), but usocket's WAIT-FOR-INPUT
selects, and Hunchentoot's accept loop waits on the gateway's listener with
it: a listener numbered past this would be written outside its fd_set.")

(defun file-limit (&optional soft)
  "This process's open-file limits as (values SOFT HARD), after asking for
SOFT as the soft one when given; NIL on Windows, or where the system refuses
to say."
  ;; RLIMIT_NOFILE is 7 on Linux and 8 on the BSDs and macOS; rlim_t is 64
  ;; bits on every platform a release is built for. A refused raise leaves the
  ;; limit as it was, so the answer is always read back, never assumed.
  #-win32
  (let ((limits (sb-alien:make-alien (sb-alien:unsigned 64) 2))
        (resource #+linux 7 #-linux 8))
    (unwind-protect
         (flet ((get-limits ()
                  (zerop (sb-alien:alien-funcall
                          (sb-alien:extern-alien "getrlimit"
                                                 (function sb-alien:int sb-alien:int
                                                           (* (sb-alien:unsigned 64))))
                          resource limits))))
           (when (get-limits)
             (when soft
               (setf (sb-alien:deref limits 0) soft)
               (sb-alien:alien-funcall
                (sb-alien:extern-alien "setrlimit"
                                       (function sb-alien:int sb-alien:int
                                                 (* (sb-alien:unsigned 64))))
                resource limits)
               (get-limits))
             (values (sb-alien:deref limits 0) (sb-alien:deref limits 1))))
      (sb-alien:free-alien limits)))
  #+win32 (progn soft nil))

(defvar *file-limit* nil
  "What RAISE-FILE-LIMIT found and left: (:was SOFT :soft SOFT :hard HARD
:cap CAP), NIL before it ran and on Windows.")

(defun raise-file-limit (&optional (cap +select-safe-descriptors+))
  "Raise this process's soft open-file limit toward its hard limit, to CAP at
most, and never lower it. => (:was :soft :hard :cap), kept in *FILE-LIMIT*;
NIL where the platform has no limit to ask."
  ;; Process-wide and inherited across fork and exec, so the organism a shell
  ;; spawns, a warm image and every child start from it -- which is why MAIN
  ;; asks first. setrlimit past OPEN_MAX (10240) is refused on macOS; the cap
  ;; sits under it.
  (multiple-value-bind (was hard) (file-limit)
    (when was
      (let* ((want (min hard cap))
             (soft (if (< was want) (or (file-limit want) was) was)))
        (setf *file-limit* (list :was was :soft soft :hard hard :cap cap))))))

(defun file-limit-line (&optional (limit *file-limit*))
  "One line saying what the open-file limit was and is, for a log; NIL when
nothing was asked."
  (when limit
    (destructuring-bind (&key was soft hard cap) limit
      (format nil "open files: ~d~:[ (raised from ~d)~;~*~]; hard limit ~a, select-safe cap ~d"
              soft (= soft was) was
              (if (>= hard (expt 2 62)) "unlimited" hard) cap))))

(defun resident-bytes ()
  "Bytes of this process resident in memory right now — the RSS a process
monitor shows, the working set on Windows — or NIL where the platform does not
say."
  #+linux
  (let* ((line (uiop:read-file-line "/proc/self/statm" :external-format :latin-1))
         (space (position #\Space line)))
    (* (parse-integer line :start (1+ space) :junk-allowed t) (sb-posix:getpagesize)))
  ;; PROCESS_MEMORY_COUNTERS on x64: cb and PageFaultCount, two DWORDs, then
  ;; SIZE_Ts, WorkingSetSize the second of them.
  #+win32
  (let ((counters (make-array 72 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (counters)
      (let ((sap (sb-sys:vector-sap counters)))
        (setf (sb-sys:sap-ref-32 sap 0) 72)
        (and (/= 0 (%get-process-memory-info (sb-win32::get-current-process) sap 72))
             (sb-sys:sap-ref-64 sap 16)))))
  #-(or linux win32) nil)

#+linux
(defun map-file-mappings (function image)
  "Call FUNCTION with START, END, KIND and RSS for every mapping of a file
in /proc/self/smaps: KIND :IMAGE for a mapping of the file IMAGE, :CLEAN for
a read-only mapping of any other file that holds no page of this process's
own, resident or swapped, NIL for the rest; RSS its resident bytes."
  ;; Read as octets through two small buffers, never a string per line: the
  ;; idle collector asks this inside the interval it measures for quiet, and
  ;; READ-LINE over the operator's smaps would cons most of what an interval
  ;; may cons before it counts as work. A mapping is reported at the next
  ;; header, once its Rss:, Anonymous: and Swap: are read.
  (let ((name (sb-ext:string-to-octets (sb-ext:native-namestring image) :external-format :utf-8))
        (chunk (make-array 16384 :element-type '(unsigned-byte 8)))
        (line (make-array 1024 :element-type '(unsigned-byte 8)))
        (fill 0) (file nil) (kind nil) (start 0) (end 0) (rss 0) (own 0))
    (labels ((parse (from to radix)
               (loop with value = 0
                     for i from from below to
                     for weight = (digit-char-p (code-char (aref line i)) radix)
                     do (cond (weight (setf value (+ (* radix value) weight)))
                              ((plusp value) (return value)))
                     finally (return value)))
             (field-p (field end-at)
               (and (> end-at (length field)) (not (mismatch field line :end2 (length field)))))
             (report ()
               (when file
                 (funcall function start end (if (and (eq kind :clean) (plusp own)) nil kind) rss)))
             (end-line (&aux (end-at (min fill (length line))))
               (let ((dash (position 45 line :end end-at))    ; -
                     (space (position 32 line :end end-at))   ; SPC
                     (slash (position 47 line :end end-at)))  ; /
                 ;; A mapping's header: its range precedes its first space.
                 ;; Every field line below one (Rss:, VmFlags:) has none. A
                 ;; line past the buffer is a path too long to be the image.
                 (cond ((and dash space (< dash space))
                        (report)
                        (setf file slash
                              start (parse 0 dash 16)
                              end (parse (1+ dash) space 16)
                              rss 0
                              own 0
                              kind (cond ((null slash) nil)
                                         ((and (<= fill (length line))
                                               (= (- end-at slash) (length name))
                                               (not (mismatch name line :start2 slash :end2 end-at)))
                                          :image)
                                         ((/= (aref line (+ space 2)) 119) :clean)))) ; w
                       ((field-p #.(sb-ext:string-to-octets "Rss:") end-at)
                        (setf rss (* 1024 (parse 4 end-at 10))))
                       ;; A page of its own is resident (Anonymous:) or in
                       ;; swap (Swap:), and both are what DONTNEED would throw
                       ;; away: a library's RELRO swapped out reads
                       ;; Anonymous: 0, and dropping it would hand back the
                       ;; file's unrelocated GOT.
                       ((field-p #.(sb-ext:string-to-octets "Anonymous:") end-at)
                        (incf own (parse 10 end-at 10)))
                       ((field-p #.(sb-ext:string-to-octets "Swap:") end-at)
                        (incf own (parse 5 end-at 10)))))
               (setf fill 0)))
      (with-open-file (smaps "/proc/self/smaps" :element-type '(unsigned-byte 8))
        (loop for count = (read-sequence chunk smaps)
              while (plusp count)
              do (dotimes (i count)
                   (let ((octet (aref chunk i)))
                     (cond ((= octet 10) (end-line))
                           ((< fill (length line)) (setf (aref line fill) octet) (incf fill))
                           (t (setf fill (1+ (length line)))))))))
      (when (plusp fill) (end-line))
      (report)
      (values))))

(defun mapped-resident-bytes (image)
  "Bytes of file pages this process holds that RELEASE-MAPPED-PAGES gives
back: its mappings of the file IMAGE, and the read-only mappings of every
other file it never wrote to -- the libraries' code and constants."
  ;; => an integer, or NIL where the platform does not say.
  ;;
  ;; The measure a release acts on: how much this process has touched back
  ;; in since it last gave those pages up. Costs a walk of every mapping,
  ;; which is why a caller reads RESIDENT-BYTES — one short line — first.
  #+linux
  (let ((total 0))
    (map-file-mappings (lambda-ignoring (start end kind rss)
                         (when kind (incf total rss)))
                       image)
    total)
  #-linux
  (progn image nil))

(defun trim-working-set ()
  "Take every page this process holds out of its working set."
  ;; => T when the request was made, NIL where the platform has no such call.
  ;;
  ;; Windows gives a process no way to hand back one mapping's pages the way
  ;; MADV_PAGEOUT does, only its whole working set: SetProcessWorkingSetSize
  ;; with both sizes (SIZE_T)-1. The pages stay committed and come back on
  ;; their next touch, from the standby list while memory allows.
  #+win32
  (/= 0 (%set-process-working-set-size (sb-win32::get-current-process)
                                       (ldb (byte 64 0) -1) (ldb (byte 64 0) -1)))
  #-win32 nil)

(defun release-mapped-pages (image)
  "Give the kernel back every page this process maps from the file IMAGE,
and every page of a read-only mapping of another file it never wrote to (a
library's code and constants); stop the read-around on them all, so each
page comes back alone when next touched."
  ;; => bytes of mappings released, or NIL where the
  ;; platform has no such call.
  ;;
  ;; The contents are untouched. The image's mappings are written to --
  ;; heap the image carried, code the loader patched -- so they are paged
  ;; out (MADV_PAGEOUT): a clean page drops and rereads from the file, a
  ;; private page this process wrote goes to swap where there is one and
  ;; stays where there is none. A library's read-only mapping with no
  ;; private page, in memory or in swap, is dropped outright (MADV_DONTNEED):
  ;; the page cache keeps it for every other process mapping the file, and it
  ;; comes back to this one on its next touch without a read. PAGEOUT would
  ;; pass those over: it
  ;; leaves alone any page another process maps too -- libc's, libcrypto's.
  ;; What changes is accounting and locality — the pages count against the
  ;; organism again only as it uses them, one at a time instead of sixteen
  ;; around every fault (MADV_RANDOM).
  #+linux
  (let ((released 0))
    (map-file-mappings (lambda-ignoring (start end kind rss)
                         (when kind
                           (%madvise start (- end start) 1) ; MADV_RANDOM
                           (%madvise start (- end start) (if (eq kind :image) 21 4)) ; PAGEOUT, DONTNEED
                           (incf released (- end start))))
                       image)
    released)
  #-linux
  (progn image nil))
