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
    ("GetFileType" %get-file-type sb-win32::dword (handle sb-win32::handle))
    ("SetStdHandle" %set-std-handle int (which sb-win32::dword) (handle sb-win32::handle))
    ("SetEnvironmentVariableW" %set-environment-variable int (name sb-win32::system-string)
     (value sb-win32::system-string))
    ("GetEnvironmentStringsW" %get-environment-strings sap)
    ("FreeEnvironmentStringsW" %free-environment-strings int (block sap))
    ("SetProcessWorkingSetSize" %set-process-working-set-size int (process sb-win32::handle)
     (minimum (sb-alien:unsigned 64)) (maximum (sb-alien:unsigned 64)))
    ("K32GetProcessMemoryInfo" %get-process-memory-info int (process sb-win32::handle)
     (counters sap) (size sb-win32::dword))
    ("GetFinalPathNameByHandleW" %get-final-path-name sb-win32::dword
     (handle sb-win32::handle) (buffer sap) (size sb-win32::dword) (flags sb-win32::dword))
    ("GetFileInformationByHandle" %get-file-information-by-handle int
     (handle sb-win32::handle) (information sap))
    ("GetHandleInformation" %get-handle-information int (handle sb-win32::handle) (flags sap))
    ("InitializeProcThreadAttributeList" %initialize-attribute-list int (attributes sap)
     (count sb-win32::dword) (flags sb-win32::dword) (size sap))
    ("UpdateProcThreadAttribute" %update-attribute int (attributes sap) (flags sb-win32::dword)
     (attribute (sb-alien:unsigned 64)) (value sap) (size (sb-alien:unsigned 64))
     (previous sap) (returned sap))
    ("DeleteProcThreadAttributeList" %delete-attribute-list sb-alien:void (attributes sap))
    ("FindFirstFileW" %find-first-file sb-win32::handle (name sb-win32::system-string) (data sap))
    ("FindNextFileW" %find-next-file int (handle sb-win32::handle) (data sap))
    ("FindClose" %find-close int (handle sb-win32::handle))
    ("SetFileAttributesW" %set-file-attributes int (name sb-win32::system-string)
     (attributes sb-win32::dword))
    ("RemoveDirectoryW" %remove-directory int (name sb-win32::system-string))
    ("DeleteFileW" %delete-file int (name sb-win32::system-string)))
  (defconstant +handle-flag-inherit+ 1)
  (defconstant +proc-thread-attribute-handle-list+ #x20002)
  (defconstant +extended-startupinfo-present+ #x80000)
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
  (defconstant +file-attribute-readonly+ #x1)
  (defconstant +file-attribute-directory+ #x10)
  (defconstant +file-attribute-reparse-point+ #x400)
  (defconstant +invalid-file-attributes+ #xFFFFFFFF)
  (defconstant +detached-process+ #x8)
  (defconstant +create-unicode-environment+ #x400)
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

;;; --- Git for Windows, past its redirectors -----------------------------------
;;; What Git for Windows puts on PATH are redirectors (git-wrapper.c in
;;; git-for-windows/MINGW-packages): bin\sh.exe and bin\bash.exe are its
;;; compat-bash, which runs usr\bin\bash.exe, and cmd\git.exe (bin\git.exe a
;;; copy) runs mingw64\bin\git.exe. Each is a process of its own that lays
;;; Git's environment over the one it inherited, starts the real program and
;;; waits on it: a command string cost 31.5 ms through bin\sh.exe and 21 ms
;;; through usr\bin\bash.exe, `git --version' 18 ms through cmd\git.exe and
;;; 10 ms through mingw64\bin\git.exe (T-442), and a scoop shim in front is a
;;; third process. So this image starts the real program itself, with the
;;; environment the redirector would have handed it, and keeps today's program
;;; wherever anything of that layout is missing.

(defparameter +git-toolchains+
  '(("mingw64" . "MINGW64") ("clangarm64" . "CLANGARM64") ("mingw32" . "MINGW32"))
  "Git for Windows' toolchain folders, each with the MSYSTEM its redirectors set.")

(defun shim-target (program)
  "The program the scoop shim PROGRAM starts -- the `path =' line of the .shim
file beside it -- or NIL when PROGRAM is no shim."
  (with-handlers ((error () nil))
    (with-open-file (in (make-pathname :type "shim" :defaults program) :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil)
              while line
              for text = (string-trim '(#\Space #\Tab #\Return) line)
              for equals = (position #\= text)
              when (and equals (string-equal "path" (string-trim '(#\Space #\Tab) (subseq text 0 equals))))
                return (let ((value (string-trim '(#\Space #\Tab #\") (subseq text (1+ equals)))))
                         (and (plusp (length value)) (uiop:parse-native-namestring value))))))))

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

#+win32
(defun file-identity (path)
  "Which file PATH, a native namestring, is -- links followed -- as one
string, VOLUME:INDEX, what device:inode is on POSIX; NIL when it cannot be
opened."
  ;; Windows' stat answers 0 for every file's inode, so device:inode names
  ;; one file for the whole volume. The file's own handle carries the volume's
  ;; serial number and the file's 64-bit index on it
  ;; (BY_HANDLE_FILE_INFORMATION: the serial at byte 28, the index's high and
  ;; low halves at 44 and 48).
  (let ((handle (%create-file path 0 +file-share-all+ nil
                              +open-existing+ +file-flag-backup-semantics+ 0)))
    (unless (= handle sb-win32::invalid-handle)
      (with-cleanup ((sb-win32::close-handle handle))
        (let ((information (make-array 52 :element-type '(unsigned-byte 8))))
          (sb-sys:with-pinned-objects (information)
            (let ((sap (sb-sys:vector-sap information)))
              (when (/= 0 (%get-file-information-by-handle handle sap))
                (format nil "~d:~d" (sb-sys:sap-ref-32 sap 28)
                        (logior (ash (sb-sys:sap-ref-32 sap 44) 32)
                                (sb-sys:sap-ref-32 sap 48)))))))))))

(defun git-redirector (program)
  "When PROGRAM is one of Git for Windows' redirectors, itself or behind a scoop
shim: a plist -- :ROOT Git's top folder, :KIND :SHELL (bin\\sh.exe,
bin\\bash.exe) or :GIT (cmd\\git.exe, bin\\git.exe), :FOLDER the redirector's
own folder, :TOOLCHAIN and :MSYSTEM -- and its real program as :PROGRAM, a
native namestring. NIL for anything else, and for a Git whose real programs are
not where its redirectors would look."
  ;; The layout is read where the redirector really lies, as it reads its own
  ;; (find_exe_realpath): through scoop's `current' junction to the version
  ;; folder, the root MSYS maps to /. :REDIRECTOR keeps the path the shim
  ;; names, which an update re-points, for when that folder is retired.
  (let* ((found (and program (ignore-errors (probe-file program)) (pathname program)))
         (target (and found (shim-target found)))
         (named (if target (and (ignore-errors (probe-file target)) target) found))
         (program (and named (real-path named))))
    (when program
      (let* ((name (string-downcase (file-namestring program)))
             (folder (uiop:pathname-directory-pathname program))
             (folder-name (string-downcase (or (car (last (rest (pathname-directory folder)))) "")))
             (root (uiop:pathname-parent-directory-pathname folder))
             (kind (cond ((and (equal folder-name "bin") (member name '("sh.exe" "bash.exe") :test #'equal))
                          :shell)
                         ((and (member folder-name '("cmd" "bin") :test #'equal) (equal name "git.exe"))
                          :git))))
        (when kind
          (loop for (toolchain . msystem) in +git-toolchains+
                for git = (probe-file (merge-pathnames (format nil "~a/bin/git.exe" toolchain) root))
                for bash = (probe-file (merge-pathnames "usr/bin/bash.exe" root))
                when (and git (or bash (eq kind :git)))
                  return (list :root root :kind kind :folder folder
                               :toolchain toolchain :msystem msystem
                               :redirector (uiop:native-namestring named)
                               :program (uiop:native-namestring
                                         (merge-pathnames (if (eq kind :shell)
                                                              "usr/bin/bash.exe"
                                                              (format nil "~a/bin/git.exe" toolchain))
                                                          root)))))))))

(defun native-folder (pathname)
  "PATHNAME, a folder, as a native namestring with no separator at its end."
  (string-right-trim "\\/" (uiop:native-namestring pathname)))

(defun git-redirector-environment (redirector &optional (environment (process-environment)))
  "ENVIRONMENT as REDIRECTOR (GIT-REDIRECTOR's plist) lays it over what it
inherits before it starts its real program -- setup_environment and
maybe_read_config in git-wrapper.c: MSYSTEM; PLINK_PROTOCOL=ssh unless set;
HOME unless set, HOMEDRIVE and HOMEPATH when that names a folder other than
System32, else USERPROFILE; PATH led by the toolchain's bin, usr\\bin and
HOME's bin; for the shell's compat-bash, EXEPATH its own folder; and the MSYS=
lines of etc\\git-bash.config ahead of any MSYS already set."
  (flet ((value (name)
           (let ((entry (find name environment :key #'environment-entry-name :test #'string-equal)))
             (and entry (subseq entry (min (length entry) (1+ (length name))))))))
    (let* ((root (native-folder (getf redirector :root)))
           (home (or (value "HOME")
                     (when-let (homepath (value "HOMEPATH"))
                       (let ((candidate (concatenate 'string (or (value "HOMEDRIVE") "") homepath))
                             (system32 (when-let (windows (value "SystemRoot"))
                                         (concatenate 'string windows "\\system32"))))
                         (and (not (and system32 (string-equal candidate system32)))
                              (uiop:directory-exists-p
                               (uiop:parse-native-namestring candidate :ensure-directory t))
                              candidate)))
                     (value "USERPROFILE")))
           (path (format nil "~a\\~a\\bin;~a\\usr\\bin;~@[~a~]~:[~;bin;~]~a"
                         root (getf redirector :toolchain) root
                         (and home (if (or (zerop (length home)) (find (char home (1- (length home))) "\\"))
                                       home
                                       (concatenate 'string home "\\")))
                         home (or (value "PATH") "")))
           (msys (with-handlers ((error () nil))
                   (with-open-file (in (merge-pathnames "etc/git-bash.config" (getf redirector :root))
                                       :if-does-not-exist nil)
                     (and in (loop for line = (read-line in nil)
                                   while line
                                   for text = (string-right-trim '(#\Return) line)
                                   when (uiop:string-prefix-p "MSYS=" text)
                                     collect (subseq text 5))))))
           (msys-value (let ((old (value "MSYS")))
                         (dolist (piece msys old)
                           (setf old (if old (format nil "~a ~a" piece old) piece))))))
      (environment-with
       (append (list (format nil "MSYSTEM=~a" (getf redirector :msystem))
                     (format nil "PATH=~a" path))
               (unless (value "PLINK_PROTOCOL") (list "PLINK_PROTOCOL=ssh"))
               (when (and home (not (value "HOME"))) (list (format nil "HOME=~a" home)))
               (when (eq (getf redirector :kind) :shell)
                 (list (format nil "EXEPATH=~a" (native-folder (getf redirector :folder)))))
               (when msys (list (format nil "MSYS=~a" msys-value))))
       environment))))

(defun windows-shell-candidate ()
  "The sh.exe a Windows image finds: on PATH, beside the git on PATH
(cmd\\git.exe by bin\\sh.exe), or in Git's two homes; NIL when none."
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
              '("ProgramFiles" "LOCALAPPDATA")))))

(define-startup-parameter *git-shell*
    #-win32 nil
    #+win32 (let ((redirector (git-redirector (windows-shell-candidate))))
              (and redirector (eq (getf redirector :kind) :shell) redirector))
  "On Windows, when the sh.exe found is Git for Windows' compat-bash: its
GIT-REDIRECTOR plist, whose usr\\bin\\bash.exe *SHELL* then is, started with
the environment the redirector would have laid down; else NIL.")

(define-startup-parameter *git-direct*
    #-win32 nil
    #+win32 (let ((redirector (git-redirector (executable-on-path "git.exe"))))
              (and redirector (eq (getf redirector :kind) :git) redirector))
  "On Windows, when the git.exe on PATH is Git for Windows' cmd\\git.exe
redirector: its GIT-REDIRECTOR plist, whose toolchain git.exe a child named
`git' then is; else NIL.")

(define-startup-parameter *shell*
    ;; Bash where the machine has it: the commands a model writes are bash --
    ;; `time', [[ ]], arrays, brace expansion -- and on Debian and Ubuntu
    ;; /bin/sh is dash, where `time' alone is "not found" (three of four
    ;; benchmark runs lost a call to it). Every script this image runs through
    ;; the shell is POSIX sh, which bash runs as written.
    ;; Windows: Git's bash past its compat-bash redirector, else the sh.exe found.
    #-win32 (or (and (probe-file "/bin/bash") "/bin/bash")
                (executable-on-path "bash")
                "/bin/sh")
    #+win32 (if *git-shell* (getf *git-shell* :program) (windows-shell-candidate))
  "The shell SHELL-ARGV runs command strings through, resolved when the
process starts: bash (/bin/bash, else the first on PATH), else /bin/sh;
Git for Windows' usr\\bin\\bash.exe, which its bin\\sh.exe runs, else the
sh.exe found; or NIL when the machine has none.")

(defun past-git-redirector (argv environment)
  "ARGV and ENVIRONMENT (NIL: this process's) as Git for Windows' redirector
would have started its real program: the shell *GIT-SHELL* names, and a child
named `git' when *GIT-DIRECT* names one, each with the redirector's
environment; anything else unchanged."
  ;; A Git uninstalled or moved under a running image: the redirector it was
  ;; found by, or PATH's own `git', answers as it did before.
  (let ((program (first argv)))
    (cond ((and *git-shell* (stringp program) (string-equal program (getf *git-shell* :program)))
           (if (probe-file program)
               (values argv (git-redirector-environment *git-shell* (or environment (process-environment))))
               (values (cons (getf *git-shell* :redirector) (rest argv)) environment)))
          ((and *git-direct* (stringp program) (member program '("git" "git.exe") :test #'string-equal)
                (probe-file (getf *git-direct* :program)))
           (values (cons (getf *git-direct* :program) (rest argv))
                   (git-redirector-environment *git-direct* (or environment (process-environment)))))
          (t (values argv environment)))))

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

(defun spawn-detached (command log &key stdout)
  "Launch COMMAND fully detached, output appended to LOG: the child survives
this process's exit and owns no terminal. STDOUT, a descriptor of this
process's, is the child's stdout instead, LOG then its stderr alone."
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
             :output (or stdout log) :error-output (if stdout log :output))))
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

#+win32
(defun extended-path (native)
  "NATIVE, an absolute Windows path, in the \\\\?\\ form that reaches past
MAX_PATH's 260 characters: a git clone's objects nest deep."
  (let ((native (substitute #\\ #\/ native)))
    (cond ((uiop:string-prefix-p "\\\\?\\" native) native)
          ((uiop:string-prefix-p "\\\\" native) (concatenate 'string "\\\\?\\UNC\\" (subseq native 2)))
          ((and (> (length native) 2) (char= #\: (char native 1))) (concatenate 'string "\\\\?\\" native))
          (t native))))

#+win32
(defun directory-entries (native)
  "The entries of the directory NATIVE, a path with no trailing separator,
as (PATH . ATTRIBUTES), the attributes FindFirstFileW reads beside each
name: nothing is followed, so a junction or a link is an entry carrying
the reparse-point bit."
  ;; WIN32_FIND_DATAW: the attributes are its first DWORD, the name the
  ;; NUL-terminated UTF-16 at byte 44.
  (let ((data (make-array 592 :element-type '(unsigned-byte 8)))
        (entries '()))
    (sb-sys:with-pinned-objects (data)
      (let* ((sap (sb-sys:vector-sap data))
             (handle (%find-first-file (concatenate 'string native "\\*") sap)))
        (unless (= handle sb-win32:invalid-handle)
          (with-cleanup ((%find-close handle))
            (loop do (let* ((end (loop for at from 44 below 564 by 2
                                       when (zerop (sb-sys:sap-ref-16 sap at)) return at
                                       finally (return 564)))
                            (name (sb-ext:octets-to-string data :external-format :utf-16le
                                                                :start 44 :end end)))
                       (unless (member name '("." "..") :test #'string=)
                         (push (cons (concatenate 'string native "\\" name) (sb-sys:sap-ref-32 sap 0))
                               entries)))
                  while (/= 0 (%find-next-file handle sap)))))))
    (nreverse entries)))

(defun remove-tree (directory)
  "DIRECTORY and everything under it, gone — a symbolic link inside is
removed as a link, never followed into what it points at."
  ;; UIOP's DELETE-DIRECTORY-TREE follows links on SBCL: a home whose cells/
  ;; holds links into the shipped tree would take the shipped cells with it.
  ;; On POSIX a walk that lstat(2)s every entry, in this process: an `rm -rf'
  ;; child cost ~120 ms of a hub install on macOS. `rm -rf' still takes what
  ;; the walk cannot (a name that is not UTF-8). On Windows the walk reads
  ;; each entry's attributes as it lists it: a junction or link is removed as
  ;; itself, and a read-only file -- git keeps every object read-only, and
  ;; DeleteFileW refuses one, which is where UIOP stops -- is made writable
  ;; first. Signals when anything is left.
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
      (labels ((fail (path)
                 (error "removing ~a failed at ~a: ~a" native path
                        (string-trim '(#\Space #\Return #\Newline #\.)
                                     (sb-win32:format-system-message (sb-win32:get-last-error)))))
               (remove-entry (path attributes)
                 (when (and (logtest attributes +file-attribute-readonly+)
                            (zerop (%set-file-attributes path +file-attribute-normal+)))
                   (fail path))
                 (cond ((and (logtest attributes +file-attribute-directory+)
                             (not (logtest attributes +file-attribute-reparse-point+)))
                        (loop for (child . bits) in (directory-entries path)
                              do (remove-entry child bits))
                        (when (zerop (%remove-directory path)) (fail path)))
                       ((logtest attributes +file-attribute-directory+)
                        (when (zerop (%remove-directory path)) (fail path)))
                       ((zerop (%delete-file path)) (fail path)))))
        (let* ((path (extended-path native))
               (attributes (sb-win32:get-file-attributes path)))
          (unless (= attributes +invalid-file-attributes+)
            (remove-entry path attributes)))))
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

(defun process-environment ()
  "This process's environment, as \"NAME=value\" strings."
  ;; SB-EXT:POSIX-ENVIRON on Windows decodes the ANSI block (GetEnvironmentStrings:
  ;; the code page's bytes, a `?' for every character outside it) as UTF-8, so a
  ;; single value holding a `ü' signals. The wide block is the environment itself.
  #-win32 (sb-ext:posix-environ)
  #+win32
  (let ((block (%get-environment-strings)))
    (unless (zerop (sb-sys:sap-int block))
      (with-cleanup ((%free-environment-strings block))
        (loop with start = 0
              until (zerop (sb-sys:sap-ref-16 block start))
              collect (let* ((end (loop for at from start by 2
                                        until (zerop (sb-sys:sap-ref-16 block at))
                                        finally (return at)))
                             (octets (make-array (- end start) :element-type '(unsigned-byte 8))))
                        (dotimes (i (length octets))
                          (setf (aref octets i) (sb-sys:sap-ref-8 block (+ start i))))
                        (setf start (+ end 2))
                        (sb-ext:octets-to-string octets :external-format
                                                 '(:utf-16le :replacement #\?))))))))

(defun environment-entry-name (entry)
  "The NAME of ENTRY, a \"NAME=value\" string or a bare NAME: up to its first `=',
which a name may only begin with (Windows' hidden \"=C:=C:\\work\" entries)."
  (subseq entry 0 (or (position #\= entry :start (min 1 (length entry))) (length entry))))

(defun environment-with (overrides &optional (environment (process-environment)))
  "ENVIRONMENT, this process's by default, with OVERRIDES laid over it: a
\"NAME=value\" replaces every entry of that name (case-blind on Windows, as
the OS reads them), a bare \"NAME\" removes them, the rest ride along."
  (let ((names (mapcar #'environment-entry-name overrides)))
    (append (remove-if (lambda (entry) (string= entry (environment-entry-name entry))) overrides)
            (remove-if (lambda (entry)
                         (member (environment-entry-name entry) names
                                 :test #+win32 #'string-equal #-win32 #'string=))
                       environment))))

;;; --- a Windows child's environment, in Unicode ------------------------------
;;; SBCL's RUN-PROGRAM hands CreateProcessW an explicit environment as bytes in
;;; the C-string external format, UTF-8, with no CREATE_UNICODE_ENVIRONMENT, so
;;; Windows reads them in the ANSI code page: a child given its parent's own
;;; environment read C:\Users\JÃ¼rgen where an inheriting one reads
;;; C:\Users\Jürgen, and a character outside the code page was lost outright.
;;; SPAWN-PROGRAM gives RUN-PROGRAM no environment on Windows and binds the
;;; wide block here instead; SBCL's one CreateProcessW call, wrapped, hands it
;;; on flagged as Unicode, so the child reads every value as an heir would.

#+win32
(progn
  (defvar *unicode-environment* nil
    "The block SPAWN-PROGRAM is starting a child with on this thread, as
UNICODE-ENVIRONMENT-BLOCK makes it, or NIL for a child that inherits.")

  (defun unicode-environment-block (environment)
    "ENVIRONMENT, \"NAME=value\" strings, as the block CreateProcessW takes under
CREATE_UNICODE_ENVIRONMENT: each entry UTF-16LE and NUL-ended, sorted by
name case-blind as Windows keeps its own, one more NUL closing it."
    (sb-ext:string-to-octets
     (with-output-to-string (out)
       (dolist (entry (stable-sort (copy-list environment) #'string<
                                   :key (lambda (entry)
                                          (string-upcase (environment-entry-name entry)))))
         (write-string entry out)
         (write-char #\Nul out))
       (write-char #\Nul out)
       (unless environment (write-char #\Nul out)))
     :external-format :utf-16le))

  (defun %create-process-in-unicode (create-process application command-line
                                     process-security thread-security inheritp
                                     flags environment &rest more)
    "SB-WIN32::CREATE-PROCESS, given *UNICODE-ENVIRONMENT* when one is bound."
    (let ((block *unicode-environment*))
      (if block
          (sb-sys:with-pinned-objects (block)
            (apply create-process application command-line process-security
                   thread-security inheritp (logior flags +create-unicode-environment+)
                   (sb-sys:vector-sap block) more))
          (apply create-process application command-line process-security
                 thread-security inheritp flags environment more))))

  ;; By name, so a reload of the wrapper reaches the encapsulation already in place.
  (sb-ext:without-package-locks
    (sb-int:encapsulate 'sb-win32::create-process 'unicode-environment
                        '%create-process-in-unicode)))

;;; --- a Windows child inherits its three standard handles, no more ---------
;;; CreateProcess told to inherit hands the child every handle of this process
;;; marked inheritable, not only the three its STARTUPINFO names. SBCL's
;;; RUN-PROGRAM marks a child's pipe ends inheritable before it takes its spawn
;;; lock, and every Winsock socket is inheritable, so a child started while
;;; another spawn's pipes were open took those pipes, and the gateway's sockets,
;;; with it. On one gateway the WhatsApp driver, started beside a `git' run,
;;; kept that run's stdout open for as long as it lived: the drain never saw end
;;; of file, and the close RUN-BOUNDED made under it froze every close in the
;;; image (2026-11-05). A PROC_THREAD_ATTRIBUTE_HANDLE_LIST names the three
;;; standard handles as the only ones the child gets -- how Python's subprocess
;;; has started every Windows child since 3.7.

#+win32
(progn
  (defun inheritable-std-handles (startup)
    "The standard handles STARTUP, a STARTUPINFOW's address, gives a child,
each once and only those marked inheritable: the handle list refuses any
other."
    ;; GetHandleInformation reads the handle table, never the file object, so
    ;; it does not wait behind a read another thread has pending on the handle.
    (let ((info (sb-alien:deref (sb-alien:sap-alien startup (* sb-win32::startup-info))))
          (handles '()))
      (sb-alien:with-alien ((flags sb-win32::dword 0))
        (dolist (handle (list (sb-alien:slot info 'sb-win32::stdin)
                              (sb-alien:slot info 'sb-win32::stdout)
                              (sb-alien:slot info 'sb-win32::stderr))
                        (nreverse handles))
          (when (and (plusp handle)
                     ;; A console pseudo-handle ends in binary 11: no kernel handle.
                     (/= 3 (logand handle 3))
                     (not (member handle handles))
                     (/= 0 (%get-handle-information
                            handle (sb-alien:alien-sap (sb-alien:addr flags))))
                     (logtest flags +handle-flag-inherit+))
            (push handle handles))))))

  (defun call-with-handle-list (startup handles function)
    "FUNCTION called with the address of a STARTUPINFOEXW: STARTUP's fields and
an attribute list naming HANDLES as the only handles a child inherits."
    (let* ((size (sb-alien:alien-size sb-win32::startup-info :bytes))
           (array (sb-alien:make-alien (sb-alien:unsigned 64) (length handles)))
           (extended (sb-alien:make-alien (sb-alien:unsigned 8) (+ size 8)))
           (needed (sb-alien:make-alien (sb-alien:unsigned 64)))
           (attributes nil))
      (with-cleanup ((when attributes (sb-alien:free-alien attributes))
                     (sb-alien:free-alien needed)
                     (sb-alien:free-alien extended)
                     (sb-alien:free-alien array))
        (loop for handle in handles
              for index from 0
              do (setf (sb-alien:deref array index) handle))
        ;; Asked with no list, it answers the size one list of one attribute takes.
        (setf (sb-alien:deref needed) 0)
        (%initialize-attribute-list (sb-sys:int-sap 0) 1 0 (sb-alien:alien-sap needed))
        (setf attributes (sb-alien:make-alien (sb-alien:unsigned 8) (sb-alien:deref needed)))
        (when (zerop (%initialize-attribute-list (sb-alien:alien-sap attributes) 1 0
                                                 (sb-alien:alien-sap needed)))
          (error "InitializeProcThreadAttributeList: Windows error ~d"
                 (sb-win32::get-last-error)))
        (with-cleanup ((%delete-attribute-list (sb-alien:alien-sap attributes)))
          (when (zerop (%update-attribute (sb-alien:alien-sap attributes) 0
                                          +proc-thread-attribute-handle-list+
                                          (sb-alien:alien-sap array) (* 8 (length handles))
                                          (sb-sys:int-sap 0) (sb-sys:int-sap 0)))
            (error "UpdateProcThreadAttribute: Windows error ~d" (sb-win32::get-last-error)))
          (let ((sap (sb-alien:alien-sap extended)))
            (sb-kernel:system-area-ub8-copy startup 0 sap 0 size)
            (setf (sb-sys:sap-ref-32 sap 0) (+ size 8)
                  (sb-sys:sap-ref-sap sap size) (sb-alien:alien-sap attributes))
            (funcall function sap))))))

  (defun %create-process-inheriting-std-handles (create-process application command-line
                                                 process-security thread-security inheritp
                                                 flags environment directory startup
                                                 information)
    "SB-WIN32::CREATE-PROCESS, its child inheriting only the standard handles its
STARTUP names when it was to inherit at all."
    (flet ((create (inheritp flags startup)
             (funcall create-process application command-line process-security
                      thread-security inheritp flags environment directory startup
                      information)))
      (if (not (and inheritp
                    (sb-sys:system-area-pointer-p startup)
                    (logtest (sb-alien:slot (sb-alien:deref (sb-alien:sap-alien
                                                             startup (* sb-win32::startup-info)))
                                            'sb-win32::flags)
                             sb-win32::+startf-use-std-handles+)))
          ;; No standard handles named (RUN-PROGRAM's :PRESERVE-FDS alone): as asked.
          (create inheritp flags startup)
          (let ((handles (inheritable-std-handles startup)))
            (if handles
                (call-with-handle-list
                 startup handles
                 (lambda (extended)
                   (create t (logior flags +extended-startupinfo-present+) extended)))
                (create nil flags startup))))))

  (sb-ext:without-package-locks
    (sb-int:encapsulate 'sb-win32::create-process 'inherit-std-handles-only
                        '%create-process-inheriting-std-handles)))

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

;; A strict decoder made one stray byte in a command's output -- a Latin-1
;; file name, a binary's banner -- a decoding error in the middle of the
;; read: SH unwound, killed the child's tree, and the output and the exit
;; status were lost with it. PROVIDER.LISP and DIAL.LISP already decode
;; their bytes this lenient way.
(defparameter *child-external-format* '(:utf-8 :replacement #\?)
  "The external format of a child's pipes: UTF-8, a byte that is not UTF-8
read as ? rather than signalled.")

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
                        ;; directory that is gone, or an output file in a
                        ;; folder that is not there, returns ENOENT -- so name
                        ;; it, or a missing working directory or log folder
                        ;; reads as a missing program.
                        (error "spawning ~a~@[ in ~a~]~@[, its output to ~a~]: ~a" (first argv) directory
                               (find-if (lambda (target)
                                          (and (stringp target)
                                               (not (uiop:directory-exists-p
                                                     (uiop:pathname-directory-pathname
                                                      (uiop:parse-native-namestring target))))))
                                        (list output errors))
                               (sb-int:strerror code)))
                      (setf pid child))))))))
        (dolist (descriptor (list out-write err-write in-read))
          (when descriptor (sb-posix:close descriptor)))
        (flet ((child-stream (descriptor direction name)
                 (when descriptor
                   (sb-sys:make-fd-stream descriptor direction t
                                          :external-format *child-external-format*
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
  ;; OUTPUT is :STREAM for a stdout pipe, NIL for the null device, a
  ;; pathname the child's stdout is appended to, created when absent, or a
  ;; descriptor of this process's (an integer) the child's stdout is.
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
           (cond ((or (null value) (keywordp value)) value)
                 ;; A descriptor: posix_spawn duplicates it into place; the
                 ;; fork path's RUN-PROGRAM takes it as an fd-stream.
                 ((integerp value) (if (eq *spawn-primitive* :posix)
                                       value
                                       (sb-sys:make-fd-stream value :output t :auto-close nil)))
                 (t (uiop:native-namestring value)))))
    (let ((directory (and directory
                          (uiop:native-namestring
                           (uiop:ensure-directory-pathname directory))))
          (output (target output))
          (errors (target error-output)))
      (if (eq *spawn-primitive* :posix)
          #-win32 (%spawn-posix argv directory environment (or output "/dev/null")
                                (or errors "/dev/null") input)
          #+win32 (error "no posix_spawn on this platform")
          (let* (#+win32 (redirected (multiple-value-list (past-git-redirector argv environment)))
                 #+win32 (argv (first redirected))
                 #+win32 (environment (second redirected))
                 #+win32 (*unicode-environment*
                          (and environment (unicode-environment-block environment)))
                 #+win32 (environment nil)
                 (process (apply #'sb-ext:run-program (first argv) (rest argv)
                                 :search t :wait nil
                                 :input (and (eq input :stream) :stream)
                                 :output output :if-output-exists :append
                                 :error errors :if-error-exists :append
                                 :external-format *child-external-format*
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

;;; A wait on something outside this process -- an index, an archive, a
;;; repository, another nodecode -- is bounded, and while it lasts it is named
;;; to whoever listens. `nodecode add' listens (CALL-SAYING-WAITS) and says on
;;; stderr what it waits on once that has lasted a few seconds: a background
;;; shell that prints nothing for minutes reads as a hang.

(defvar *wait-hook* nil
  "NIL, or a function of TEXT and BOUND that this thread calls as it starts a
wait WITH-NAMED-WAIT names -- TEXT what it waits on, in words, BOUND the
most seconds it waits -- and of NIL and NIL when that wait ends.")

(defvar *wait-step* nil
  "The name of the innermost named wait this thread is in (WITH-NAMED-WAIT):
the name a deadline keeps for a step it cuts short (STEP-RAN-OUT).")

(defmacro with-named-wait ((bound control &rest arguments) &body body)
  "BODY, a wait of at most BOUND seconds that CONTROL and ARGUMENTS name as
FORMAT would, named to *WAIT-HOOK* while it lasts. => BODY's values."
  `(call-with-named-wait (lambda () ,@body) ,bound ,control ,@arguments))

(defun call-with-named-wait (function bound control &rest arguments)
  "FUNCTION's values, its wait named to *WAIT-HOOK* as WITH-NAMED-WAIT says,
and a step inside it that a deadline cuts short named so (*WAIT-STEP*)."
  (let* ((hook *wait-hook*)
         (name (apply #'format nil control arguments))
         (*wait-step* name))
    (if hook
        (progn (funcall hook name bound)
               (unwind-protect (funcall function)
                 (funcall hook nil nil)))
        (funcall function))))

;;; Work done for one waiter -- a cell turned on for `nodecode add', which
;;; holds a wall clock on the answer -- runs in steps each bounded on its own:
;;; an index read, a download, a fetch, a clone, a wait for a lock. Their
;;; bounds add up to far more than the waiter waits, so it could give up on
;;; work that went on and succeeded, and its retry ran beside it. Under
;;; WITH-WAIT-DEADLINE the work has one deadline instead: every step this
;;; thread starts gets what is left of it at most (WAIT-SECONDS) -- a child of
;;; RUN-BOUNDED, an exchange of HTTP, a lock taken through
;;; CALL-WITH-LOCK-IN-TIME -- and a step that finds nothing left signals
;;; WAIT-DEADLINE-PASSED instead of starting. proofs/AddDeadlines.lean proves
;;; such work ends by its deadline whatever each step takes within what it is
;;; given, the lock's waits included.

(defstruct (wait-deadline (:constructor make-wait-deadline (end seconds)) (:copier nil) (:predicate nil))
  "A deadline work for one waiter runs under (WITH-WAIT-DEADLINE): the
internal real time END by which it ends and the SECONDS it was given. SPENT
once a step it cut short ran out (STEP-RAN-OUT), and STEP then that step's
name -- the named wait (WITH-NAMED-WAIT) it ran in -- or NIL for an unnamed one."
  (end 0 :type integer :read-only t)
  (seconds 0 :type real :read-only t)
  (spent nil)
  (step nil :type (or null string)))

(defvar *wait-deadline* nil
  "NIL, or the WAIT-DEADLINE the work this thread does for one waiter runs
under (WITH-WAIT-DEADLINE).")

(define-error wait-deadline-passed (error) (seconds (step :initform nil)) (:export)
  (:report "the ~d s it was given ran out~@[ while ~a~]" seconds step))

(defun wait-deadline-reason (condition &optional (deadline *wait-deadline*))
  "Why work under DEADLINE failed once it passed, for a person: the time ran
out while the step it cut short ran, when that step had a name; else
CONDITION's report, and NIL for a WAIT-DEADLINE-PASSED, which says no more."
  (let ((step (if (typep condition 'wait-deadline-passed)
                  (wait-deadline-passed-step condition)
                  (and deadline (wait-deadline-step deadline)))))
    (cond (step (format nil "the time ran out while ~a" step))
          ((typep condition 'wait-deadline-passed) nil)
          (t (princ-to-string condition)))))

(defun step-ran-out (clipped &optional (deadline *wait-deadline*))
  "Say a step ran out of what it was given: when the deadline gave it that
(CLIPPED, WAIT-SECONDS' second value) the deadline is SPENT, named by the
step running now (*WAIT-STEP*)."
  ;; The clock can read a hair short of END when a timed wait wakes: spent,
  ;; the next step is refused rather than given that hair and failing as
  ;; though it were the one the time ran out in.
  (when (and clipped deadline (not (wait-deadline-spent deadline)))
    (setf (wait-deadline-spent deadline) t
          (wait-deadline-step deadline) *wait-step*)))

(defun wait-seconds (bound &key (deadline *wait-deadline*) (now (get-internal-real-time)))
  "The seconds a step bounded by BOUND -- seconds, or NIL for a step with no
bound of its own -- is given under DEADLINE (*WAIT-DEADLINE*): BOUND, or
what is left of DEADLINE when that is less or BOUND is NIL; BOUND when there
is no deadline. Signals WAIT-DEADLINE-PASSED when nothing is left -- the
clock is past it, or a step it cut short ran out -- so a step never starts
past the deadline."
  ;; GIVEN in proofs/AddDeadlines.lean, whose decided traces the test
  ;; wait-seconds-holds-to-adddeadlines-lean-traces pins here.
  ;; The second value is T when the deadline, not BOUND, gives the step its
  ;; time: a step that runs out of it then says so (STEP-RAN-OUT).
  (if (null deadline)
      (values bound nil)
      (let ((left (/ (- (wait-deadline-end deadline) now) internal-time-units-per-second)))
        (when (or (wait-deadline-spent deadline) (not (plusp left)))
          (error 'wait-deadline-passed :seconds (wait-deadline-seconds deadline)
                                       :step (wait-deadline-step deadline)))
        (if (and bound (< bound left))
            (values bound nil)
            (values left t)))))

(defun wait-deadline-passed-p (&optional (deadline *wait-deadline*) (now (get-internal-real-time)))
  "Whether the thread works under a deadline (*WAIT-DEADLINE*) that has passed."
  (and deadline (or (wait-deadline-spent deadline) (>= now (wait-deadline-end deadline)))))

(defmacro with-wait-deadline ((seconds) &body body)
  "BODY, work for one waiter that must end within SECONDS (WAIT-SECONDS), or
by the deadline it runs under already when that comes first. SECONDS NIL
leaves the deadline as it stands."
  `(call-with-wait-deadline (lambda () ,@body) ,seconds))

(defun call-with-wait-deadline (function seconds)
  "FUNCTION's values under WITH-WAIT-DEADLINE's deadline of SECONDS."
  (let* ((end (and seconds (deadline-after seconds)))
         (*wait-deadline* (if (and end (or (null *wait-deadline*) (< end (wait-deadline-end *wait-deadline*))))
                              (make-wait-deadline end seconds)
                              *wait-deadline*)))
    (funcall function)))

(defun call-with-lock-in-time (lock function &optional step)
  "FUNCTION's values, called holding LOCK: waited for as long as it takes
with no deadline, and under one for what is left of it at most
(WAIT-SECONDS), past which WAIT-DEADLINE-PASSED is signalled. STEP names
the wait for the lock, when it is one, to the deadline (*WAIT-STEP*)."
  (unless (bt2:acquire-lock lock :wait nil)
    (let ((*wait-step* (or step *wait-step*)))
      (multiple-value-bind (seconds clipped) (wait-seconds nil)
        (unless (bt2:acquire-lock lock :timeout seconds)
          (step-ran-out clipped)
          (error 'wait-deadline-passed :seconds (wait-deadline-seconds *wait-deadline*)
                                       :step (wait-deadline-step *wait-deadline*))))))
  (unwind-protect (funcall function)
    (bt2:release-lock lock)))

(defun %threads-released-p (threads)
  "T once none of THREADS -- RUN-BOUNDED's drains and its feeder -- is left
parked on a pipe, so closing the pipes cannot wait on one; NIL when one would
not let go, and the pipes stay open rather than freeze the image."
  ;; On Windows a close waits behind a read or a write another thread has
  ;; pending on the same pipe, and while it waits every other close in the
  ;; process waits behind it: a NUL close started 14 s in finished at 30 s, the
  ;; moment the pipe's last writer exited (2026-11-05, winbox). A thread still
  ;; parked when the bound fell -- a grandchild holds the pipe's other end -- is
  ;; ended first; SBCL's interrupt cancels its pending I/O
  ;; (CancelSynchronousIo). A POSIX close never waits, so there the pipes close
  ;; as they are.
  #-win32 (progn threads t)
  #+win32
  (dolist (thread threads t)
    (when (bt2:thread-alive-p thread)
      (ignore-errors (bt2:destroy-thread thread))
      (%join-bounded thread 5)
      (when (bt2:thread-alive-p thread)
        (return nil)))))

(defun run-bounded (argv &key (seconds 60) directory environment input
                             (output :string) (error-output :string) &aux clipped)
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
  ;; Under a deadline the child is given what is left of it (WAIT-SECONDS),
  ;; and none is started once it has passed.
  (multiple-value-setq (seconds clipped) (wait-seconds seconds))
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
      (with-cleanup ((when (%threads-released-p (if feeder (cons feeder readers) readers))
                       (when output-stream (ignore-errors (close output-stream)))
                       (when error-stream (ignore-errors (close error-stream)))
                       (when input-stream (ignore-errors (close input-stream :abort t)))
                       ;; Closes the same streams once more: never under a parked thread.
                       (when process (ignore-errors (sb-ext:process-close process)))))
        (when (eq status :timeout)
          (step-ran-out clipped)
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

(defun held-lock (path &aux (native (uiop:native-namestring path)))
  "The lock this process holds on the file at PATH (a LOCK-FILE answer), or
NIL when it holds none."
  (sb-ext:with-locked-hash-table (*held-locks*)
    (loop for lock being the hash-keys of *held-locks* using (hash-value held)
          when (equal held native) return lock)))

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

(defun gzip-file (from to)
  "The file at FROM gzipped into the file at TO, TO replaced if it exists,
in this process: Windows ships tar but no gzip program. => TO."
  ;; salza2, the deflater FIT-IMAGE already carries. Its fixed Huffman codes
  ;; run as fast as gzip -6 does and come out about a quarter larger.
  (salza2:gzip-file from to)
  to)

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
  ;; A file that says no size, as one under /proc does, is read to its end.
  (with-handlers ((error () nil))
    (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (when (and in (or (null limit) (<= (file-length in) limit)))
        (alexandria:read-stream-content-into-byte-vector in :initial-size (max 1 (file-length in)))))))


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

#-win32
(define-routines
  ("getrlimit" %getrlimit int (resource int) (limits (* (sb-alien:unsigned 64))))
  ("setrlimit" %setrlimit int (resource int) (limits (* (sb-alien:unsigned 64)))))

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
         (when (zerop (%getrlimit resource limits))
           (when soft
             (setf (sb-alien:deref limits 0) soft)
             (%setrlimit resource limits)
             (%getrlimit resource limits))
           (values (sb-alien:deref limits 0) (sb-alien:deref limits 1)))
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

;;; --- interrupting a thread: where an unwind may land ------------------------
;;; An interruption runs on top of whatever its thread was doing, C frames
;;; included: SBCL delivers it inside foreign code as readily as in Lisp. One
;;; that unwinds -- a THROW, an ERROR, ABORT-THREAD -- discards those C frames
;;; without their cleanup, and a C function that held a lock when it was
;;; interrupted holds it for good. malloc's arena lock is the one that bites:
;;; every thread shares two arenas (CAP-MALLOC-ARENAS), so the next malloc or
;;; free that misses its thread's cache, on any thread, waits on it forever,
;;; at 0% CPU and deaf to SIGINT. A full `just test' froze so for six hours:
;;; the main thread joining a thread, in free, on the main arena, and no live
;;; thread holding it. The test it froze in had done nothing wrong; an earlier
;;; stop had unwound a thread out of malloc.
;;;
;;; So an interruption that may unwind goes through INTERRUPT-THREAD-SAFELY.
;;; It runs where the interrupted pc is Lisp code, or where the thread is
;;; parked in a system call that waits -- a read, a poll, an accept, a sleep,
;;; a wait for a child or a futex the runtime itself sleeps on -- across which
;;; no C library lock is held. Landed anywhere else in C it does nothing, and
;;; is sent again a moment later, by when the thread is past the malloc it
;;; found. The parked case is what a stop is for (a turn waiting on its
;;; provider, an evaluation in SLEEP or SH), so it stays reachable; the
;;; classification reads the interrupted context's pc and result register.
;;; Where that reading is not written (only x86-64 Linux is), every landing
;;; counts as safe, as it did before; Windows interrupts at safepoints only.

#+(and linux x86-64)
(defparameter +waiting-syscalls+
  '(0 1 7 17 18 19 20 23 34 35 42 43 44 45 46 47 61 72 73 202 230 232 247 270 271 281 288)
  "x86-64 Linux system calls a thread parks in while it waits on something
outside it: read, write, poll, pread64, pwrite64, readv, writev, select,
pause, nanosleep, connect, accept, sendto, recvfrom, sendmsg, recvmsg, wait4,
fcntl, flock, futex, clock_nanosleep, epoll_wait, waitid, pselect6, ppoll,
epoll_pwait, accept4.")

#+(and linux x86-64)
(defun %pc-function-name (pc)
  "The name of the exported C function PC is in, or NIL."
  (sb-alien:with-alien ((info (sb-alien:array sb-sys:system-area-pointer 4)))
    (unless (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "dladdr" (function sb-alien:int sb-sys:system-area-pointer
                                                             (* (sb-alien:array sb-sys:system-area-pointer 4))))
                    pc (sb-alien:addr info)))
      (let ((name (sb-alien:deref info 2)))
        (unless (zerop (sb-sys:sap-int name))
          (sb-alien::c-string-to-string name :latin-1 'character))))))

(defun interruption-may-unwind-p (&aux (context sb-kernel:*current-internal-error-context*))
  "Inside an interruption, true when unwinding out of it strands no C
library's lock: its thread was running Lisp code, or was parked in a system
call that waits (+WAITING-SYSCALLS+)."
  ;; Nothing here may call malloc: this can run on top of a malloc that holds
  ;; its arena's lock, and the lock is not recursive. dladdr does not.
  #+(and linux x86-64)
  (or (null context)                    ; run outside a signal: Lisp
      (let* ((context (sb-alien:sap-alien context (* sb-sys:os-context-t)))
             (pc (sb-vm:context-pc context))
             (address (sb-sys:sap-int pc))
             (result (sb-vm:context-register context sb-vm::rax-offset)))
        (flet ((syscall-at (offset)
                 ;; the two bytes of `syscall' (0F 05), read only inside pc's page
                 (let ((byte (+ (logand address #xfff) offset)))
                   (and (<= 0 byte) (<= (+ byte 2) #x1000)
                        (= #x0f (sb-sys:sap-ref-8 pc offset))
                        (= #x05 (sb-sys:sap-ref-8 pc (1+ offset)))))))
          (or (and (sb-di::code-header-from-pc pc) t)
              ;; A wait the signal broke: the call returned -EINTR, pc past it.
              (and (= result (- (ash 1 64) 4)) (syscall-at -2))
              ;; A wait the kernel will restart: pc back on the call, and the
              ;; result register holding its number again. A futex only from
              ;; libc's syscall(), which is how the runtime sleeps; glibc's own
              ;; lock waits (__lll_lock_wait) may sit inside another lock.
              (and (member result +waiting-syscalls+)
                   (syscall-at 0)
                   (or (/= result 202)
                       (equal (%pc-function-name pc) "syscall")))))))
  #-(and linux x86-64)
  (progn context t))

(defparameter *interrupt-resend-seconds* 0.001
  "How long INTERRUPT-THREAD-SAFELY waits before sending again an
interruption that landed inside C code.")

(defun interrupt-thread-safely (thread function &key (within 2))
  "Run FUNCTION in THREAD as BT2:INTERRUPT-THREAD does, but only where an
unwind out of it strands nothing (INTERRUPTION-MAY-UNWIND-P): landed inside
other C code it is sent again, until it runs or WITHIN seconds pass.
=> :RAN; :GONE when THREAD has ended; :QUEUED when THREAD holds interruptions
back past WITHIN (it runs, in Lisp, once they are allowed again); NIL when
THREAD stayed inside C code for WITHIN seconds and FUNCTION did not run."
  ;; THREAD a bordeaux or an SBCL thread. The call waits for each delivery to
  ;; land, which is microseconds unless THREAD defers interruptions
  ;; (WITHOUT-INTERRUPTS, a guarded SQLite call): at least a twentieth of a
  ;; second, so WITHIN 0 makes one delivery and a probe that may skip a pass
  ;; asks that.
  (let ((native (if (typep thread 'sb-thread:thread) thread (bt2:thread-native-thread thread)))
        (deadline (+ (get-internal-real-time) (* within internal-time-units-per-second))))
    (loop
      (let ((landed (sb-thread:make-semaphore))
            (where (list nil)))
        (unless (ignore-errors
                 (sb-thread:interrupt-thread
                  native (lambda ()
                           (let ((safe (interruption-may-unwind-p)))
                             (setf (car where) (if safe :ran :deferred))
                             (sb-thread:signal-semaphore landed)
                             (when safe (funcall function)))))
                 t)
          (return :gone))
        (unless (sb-thread:wait-on-semaphore
                 landed :timeout (max 0.05 (/ (- deadline (get-internal-real-time))
                                              internal-time-units-per-second)))
          (return (if (sb-thread:thread-alive-p native) :queued :gone)))
        (when (eq (car where) :ran)
          (return :ran))
        (when (>= (get-internal-real-time) deadline)
          (return nil))
        (sleep *interrupt-resend-seconds*)))))

(defun terminate-thread-safely (thread &key (within 2))
  "End THREAD as SB-THREAD:TERMINATE-THREAD does, delivered where its unwind
strands nothing (INTERRUPT-THREAD-SAFELY, whose answer this is)."
  (interrupt-thread-safely thread (lambda () (sb-thread:abort-thread :allow-exit t))
                           :within within))

;;; Libraries end their own threads through BT2:DESTROY-THREAD -- the
;;; websocket client its reader on every close, clack its acceptor -- so the
;;; entry itself goes through TERMINATE-THREAD-SAFELY. Idempotent: a reload
;;; replaces the advice rather than stacking it.
(when (sb-int:encapsulated-p 'bt2:destroy-thread 'safe-point)
  (sb-int:unencapsulate 'bt2:destroy-thread 'safe-point))
(sb-int:encapsulate 'bt2:destroy-thread 'safe-point
                    (lambda (next thread)
                      (if (eq thread (bt2:current-thread))
                          (funcall next thread) ; its own refusal
                          (progn (terminate-thread-safely thread) thread))))
