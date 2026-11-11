;;;; sandbox-linux.lisp --- the Linux jail: bubblewrap, run by nodecode
;;;; itself, under a seccomp filter built here. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The jail is bwrap's, with nothing between nodecode and it: no srt, no
;;;; socat, no Node. It has user, pid, ipc, uts, cgroup and network
;;;; namespaces of its own -- so a loopback of its own and no other network,
;;;; and no abstract unix socket of the host's -- and a mount view built up
;;;; from an empty root: the system (/usr, /etc and the /bin and /lib links)
;;;; and the binary's tree read-only, the work folder and the home read-write,
;;;; the launcher's files read-only over them, the home's keys hidden. What is
;;;; not mounted is not there: ~/.ssh, other agents' homes, the host's /tmp and
;;;; /run with every socket in them. The seccomp filter refuses every unix
;;;; socket, the io_uring calls that would make one round it, and the user's
;;;; keyrings.
;;;;
;;;; Three descriptors cross into it (CALL-WITH-SANDBOX-DESCRIPTORS), at 3, 4
;;;; and 5 of bwrap's:
;;;;  3 the listener the shell bound on the host's loopback port, the jail's
;;;;    one way in: the jailed organism accepts on it and carries each
;;;;    connection to its gateway on the jail's own loopback;
;;;;  4 the jail's end of a socketpair, the way out: before its folder loads,
;;;;    the jailed organism binds the egress port on its own loopback and sends
;;;;    that listening socket back over it, so the launcher accepts on it and
;;;;    the proxy outside the jail (SANDBOX-EGRESS-START) answers -- the jail
;;;;    makes no unix socket, and holds no key;
;;;;  5 the seccomp program, which bwrap reads, applies and closes.
;;;; SANDBOX-JAIL-ENTER is what the jailed organism does with them, before
;;;; anything of its folder loads.

(in-package #:nodecode.evolved)

;;; --- the seccomp filter ---------------------------------------------------

(defparameter +sandbox-seccomp-machines+
  ;; audit arch, then the calls by name; io_uring's are the generic table's
  ;; on both.
  '((:x86-64 #xC000003E :socket 41 :add-key 248 :request-key 249 :keyctl 250
     :io-uring-setup 425 :io-uring-enter 426 :io-uring-register 427)
    (:arm64 #xC00000B7 :socket 198 :add-key 217 :request-key 218 :keyctl 219
     :io-uring-setup 425 :io-uring-enter 426 :io-uring-register 427))
  "Per machine the jail runs on: its AUDIT_ARCH and the numbers of the calls
the filter refuses.")

(defun sandbox-machine ()
  "This machine as +SANDBOX-SECCOMP-MACHINES+ names it, or NIL when the jail
has no filter for it."
  #+x86-64 :x86-64 #+arm64 :arm64 #-(or x86-64 arm64) nil)

(defun sandbox-bpf-assemble (program)
  "PROGRAM, a list of instructions and labels, as the octets of a classic
BPF program (struct sock_filter, little-endian): (:LD OFFSET) loads the
32-bit word at OFFSET of struct seccomp_data, (:JEQ K THEN ELSE) and (:JGE K
THEN ELSE) jump to the label THEN or ELSE (NIL: the next instruction), (:RET
K) answers K."
  (let ((labels (make-hash-table)) (code '()) (index 0))
    (dolist (item program)
      (if (symbolp item)
          (setf (gethash item labels) index)
          (progn (push item code) (incf index))))
    (setf code (nreverse code))
    (let ((octets (make-array (* 8 (length code)) :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for (op k then else) in code
            for n from 0
            for at = (* 8 n)
            do (flet ((jump (label)
                        (if (null label)
                            0
                            (let ((offset (- (or (gethash label labels) (error "no label ~a" label)) n 1)))
                              (assert (<= 0 offset 255))
                              offset)))
                      (put (value size offset)
                        (dotimes (i size) (setf (aref octets (+ at offset i)) (ldb (byte 8 (* 8 i)) value)))))
                 (put (ecase op (:ld #x20) (:jeq #x15) (:jge #x35) (:ret #x06)) 2 0)
                 (when (member op '(:jeq :jge))
                   (put (jump then) 1 2)
                   (put (jump else) 1 3))
                 (put k 4 4)))
      octets)))

(defun sandbox-seccomp-program (&optional (machine (sandbox-machine)))
  "The jail's seccomp filter for MACHINE, as BPF octets bwrap loads: a call
of another architecture ends the process; x32's calls, socket(AF_UNIX), the
io_uring calls (they make sockets without socket(2)) and the keyring calls
answer EPERM; everything else runs."
  (destructuring-bind (arch &rest calls)
      (or (rest (assoc machine +sandbox-seccomp-machines+))
          (error "the jail has no seccomp filter for ~a" machine))
    (sandbox-bpf-assemble
     `((:ld 4)                          ; seccomp_data.arch
       (:jeq ,arch nil :kill)
       (:ld 0)                          ; seccomp_data.nr
       (:jge #x40000000 :deny nil)      ; x32's calls
       ,@(loop for name in '(:keyctl :add-key :request-key
                             :io-uring-setup :io-uring-enter :io-uring-register)
               collect `(:jeq ,(getf calls name) :deny nil))
       (:jeq ,(getf calls :socket) nil :allow)
       (:ld 16)                         ; args[0], the domain's low word
       (:jeq 1 :deny :allow)            ; AF_UNIX
       :allow (:ret #x7FFF0000)         ; SECCOMP_RET_ALLOW
       :deny (:ret #x00050001)          ; SECCOMP_RET_ERRNO | EPERM
       :kill (:ret #x80000000)))))      ; SECCOMP_RET_KILL_PROCESS

;;; --- the mount view and the argv ------------------------------------------

(defparameter +sandbox-system-trees+ '("/usr" "/etc" "/opt" "/nix" "/gnu")
  "The system trees a jail reads, each mounted read-only when it is there.")

(defparameter +sandbox-system-links+ '("/bin" "/sbin" "/lib" "/lib32" "/lib64" "/libx32")
  "The top-level names a usr-merged system keeps as links into /usr; on one
that is not, the directories themselves, read-only.")

(defparameter +sandbox-environment-kept+
  '("PATH" "HOME" "USER" "LOGNAME" "SHELL" "LANG" "LANGUAGE" "TZ" "TERM" "COLORTERM")
  "The variables of the launcher's environment a jail keeps, besides LC_*
and NODECODE_*: everything else -- a key in an environment variable, an
agent's socket, a desktop's bus -- is unset in it.")

(defparameter +sandbox-environment-dropped+
  '("NODECODE_PRISTINE" "NODECODE_API_KEY" "NODECODE_OPERATOR_TOKEN" "NODECODE_SRT")
  "The NODECODE_ variables a jail never gets: the shell's pristine marker
(the organism loads its folder) and the keys, which the proxy outside adds.")

(defun sandbox-environment-kept-p (name)
  "Whether the variable NAME rides into the jail."
  (and (not (member name +sandbox-environment-dropped+ :test #'string=))
       (or (member name +sandbox-environment-kept+ :test #'string=)
           (uiop:string-prefix-p "LC_" name)
           (uiop:string-prefix-p "NODECODE_" name))))

(defun sandbox-directory-name (path)
  "PATH as a native directory name with no trailing slash (\"/\" stays)."
  (let ((name (uiop:native-namestring (uiop:ensure-directory-pathname path))))
    (if (string= name "/") name (string-right-trim "/" name))))

(defun sandbox-path-under-p (path tree)
  "Whether PATH is TREE or inside it, both native names."
  (let ((path (string-right-trim "/" path)) (tree (string-right-trim "/" tree)))
    (or (string= tree "")
        (string= path tree)
        (uiop:string-prefix-p (concatenate 'string tree "/") path))))

(defun sandbox-work-folder (folder &optional (homes (sandbox-user-homes)))
  "FOLDER as the jail's work folder, or NIL when it is / or holds a user's
home -- a jail given that would hold ~/.ssh and every other agent's home."
  (let ((name (sandbox-directory-name folder)))
    (unless (some (lambda (home) (sandbox-path-under-p (sandbox-directory-name home) name)) homes)
      name)))

(defun sandbox-proxy-environment (port &optional token)
  "The variables that send the jail's web traffic to the egress proxy on
loopback PORT, and the loopback around it. With TOKEN the proxy's address
carries it as its password, which every client sends as Proxy-Authorization
(a proxy on the host's loopback, SANDBOX-EGRESS-ADMITTED)."
  (let ((proxy (format nil "http://~@[nodecode:~a@~]127.0.0.1:~d" token port))
        (direct "localhost,127.0.0.1,::1"))
    (list "HTTP_PROXY" proxy "HTTPS_PROXY" proxy "http_proxy" proxy "https_proxy" proxy
          "NO_PROXY" direct "no_proxy" direct)))

(defun sandbox-jail-path (path trees)
  "The PATH variable's value inside the jail: the entries of PATH that lie in
the mounted TREES, then the system's own directories."
  (let ((entries (remove-if-not (lambda (entry) (some (lambda (tree) (sandbox-path-under-p entry tree)) trees))
                                (uiop:split-string (or path "") :separator ":"))))
    (format nil "~{~a~^:~}"
            (remove-duplicates (append entries '("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/sbin"))
                               :test #'string= :from-end t))))

(defun sandbox-bwrap-argv (argv &key (bwrap "bwrap") home folder binary store policy hidden
                                     run cache temporary port egress-port stand-ins jail-environment
                                     (environment (sb-ext:posix-environ))
                                     (exists #'probe-file) (link #'sandbox-link-target)
                                     (directory-p #'uiop:directory-exists-p)
                                     (homes (sandbox-user-homes)) (disable-userns t))
  "ARGV, the serve's own, as the command line that runs it in a bwrap jail:
HOME and the work FOLDER (SANDBOX-WORK-FOLDER; NIL for none) read-write,
the BINARY's directory read-only, STORE's directory read-write when it lies
outside HOME, POLICY's grants (SANDBOX-POLICY), HIDDEN's paths unreadable
(a directory an empty read-only one, a file the null device), the
launcher's files read-only; RUN, CACHE and TEMPORARY the jail's runtime
directory, cache and TMPDIR; PORT the gateway's port on the jail's loopback
and EGRESS-PORT the proxy's. STAND-INS, an alist (REAL . STAND-IN), puts
each STAND-IN read-only where REAL, a launcher's or a hidden file, would be
(SANDBOX-STAND-INS); JAIL-ENVIRONMENT, NAME VALUE ..., is set in the jail
last. ENVIRONMENT is the launcher's, which the jail keeps only the safe part
of; EXISTS decides which optional paths are
mounted, LINK answers a top-level link's target (or NIL for a directory).
The serve gets descriptors 3 and 4 (the way in and out), bwrap reads the
filter from 5."
  (let* ((home (sandbox-directory-name home))
         (folder (and folder (sandbox-work-folder folder homes)))
         (binary-dir (sandbox-directory-name (uiop:pathname-directory-pathname binary)))
         (store-dir (and store (sandbox-directory-name (uiop:pathname-directory-pathname store))))
         (store-dir (and store-dir (not (sandbox-path-under-p store-dir home))
                         (not (and folder (sandbox-path-under-p store-dir folder)))
                         store-dir))
         (present (lambda (path) (and path (ignore-errors (funcall exists path)) t)))
         (expand (lambda (entries) (mapcar (lambda (entry) (string-right-trim "/" (sandbox-expand entry))) entries)))
         (allow-write (remove-if-not present (funcall expand (getf policy :allow-write))))
         (allow-read (remove-if-not present (funcall expand (append +sandbox-home-read+ (getf policy :allow-read)))))
         (written (remove nil (list* home folder store-dir allow-write)))
         (mounted (append written allow-read (list binary-dir)))
         (masked (remove-if-not (lambda (path)
                                  (and (funcall present path)
                                       (some (lambda (tree) (sandbox-path-under-p (string-right-trim "/" path) tree)) mounted)))
                                (remove-duplicates
                                 (append hidden
                                         (funcall expand +sandbox-default-deny-read+)
                                         (funcall expand (getf policy :deny-read)))
                                 :test #'string=)))
         (launcher-files (remove-if-not present
                                        (mapcar (lambda (relative) (sandbox-home-path home relative))
                                                (list "config.jsonc" "sandbox.jsonc" "sandbox/egress.log"
                                                      (format nil "sandbox/~a" +sandbox-mark-name+)))))
         (variables (mapcar (lambda (entry)
                              (let ((at (position #\= entry)))
                                (if at (cons (subseq entry 0 at) (subseq entry (1+ at))) (cons entry ""))))
                            environment))
         (out '()))
    (labels ((emit (&rest words) (dolist (word words) (push word out)))
             (setenv (name value) (emit "--setenv" name value)))
      (emit bwrap "--unshare-user" "--unshare-ipc" "--unshare-pid" "--unshare-net" "--unshare-uts"
            "--unshare-cgroup-try")
      (when disable-userns (emit "--disable-userns"))
      (emit "--new-session")
      ;; the system, read-only
      (dolist (tree +sandbox-system-trees+)
        (when (funcall present tree) (emit "--ro-bind" tree tree)))
      (dolist (name +sandbox-system-links+)
        (when (funcall present name)
          (let ((target (funcall link name)))
            (if target (emit "--symlink" target name) (emit "--ro-bind" name name)))))
      (emit "--proc" "/proc" "--dev" "/dev" "--tmpfs" "/tmp" "--tmpfs" "/var/tmp")
      ;; What it runs and works on, shallowest first: a mount over a tree
      ;; hides what was mounted inside it before, so a read-only file in a
      ;; written folder goes after the folder, never before. At one path the
      ;; later kind wins: written, read, the launcher's, hidden.
      (dolist (mount (stable-sort (sandbox-mounts :written written :read allow-read
                                                  :binary binary :binary-dir binary-dir
                                                  :launcher launcher-files :hidden masked
                                                  :directory-p directory-p)
                                  #'< :key (lambda (mount) (count #\/ (second mount)))))
        (destructuring-bind (kind path) mount
          (let ((stand-in (and (member kind '(:read :hide-file))
                               (cdr (assoc path stand-ins :test #'string=)))))
            (if stand-in
                ;; the file the jail reads in its place: a key's stand-in, never the key
                (emit "--ro-bind" stand-in path)
                (ecase kind
                  (:write (emit "--bind" path path))
                  (:read (emit "--ro-bind" path path))
                  (:hide-directory (emit "--tmpfs" path "--remount-ro" path))
                  (:hide-file (emit "--ro-bind" "/dev/null" path)))))))
      ;; The empty root bwrap builds on -- and the parents it made for every
      ;; mount, a skeleton of the host's home among them -- is not the
      ;; jail's to write: only what was mounted written, and its own /tmp.
      (emit "--remount-ro" "/")
      (emit "--chdir" (or folder home))
      ;; the environment: the safe part of the launcher's, then the jail's own
      (loop for (name . nil) in variables
            unless (sandbox-environment-kept-p name) do (emit "--unsetenv" name))
      (setenv "PATH" (sandbox-jail-path (cdr (assoc "PATH" variables :test #'string=))
                                        (append +sandbox-system-trees+ mounted)))
      (setenv "NODECODE_SANDBOXED" "1")
      (setenv "NODECODE_GATEWAY_PORT" (princ-to-string port))
      (setenv "NODECODE_SANDBOX_INBOUND_FD" "3")
      (setenv "NODECODE_SANDBOX_EGRESS_FD" "4")
      (setenv "NODECODE_SANDBOX_EGRESS_PORT" (princ-to-string egress-port))
      (loop for (name value) on (sandbox-proxy-environment egress-port) by #'cddr do (setenv name value))
      (setenv "XDG_RUNTIME_DIR" (sandbox-directory-name run))
      (setenv "XDG_CACHE_HOME" (sandbox-directory-name cache))
      (setenv "TMPDIR" (sandbox-directory-name temporary))
      ;; the keys' stand-ins and the host-side providers' origins
      (loop for (name value) on jail-environment by #'cddr do (setenv name value))
      (emit "--seccomp" "5" "--")
      (append (nreverse out) argv))))

(defun sandbox-mounts (&key written read binary binary-dir launcher hidden (directory-p #'uiop:directory-exists-p))
  "The jail's mounts beyond the system's, as (KIND PATH) in the order they
win at one path: the WRITTEN trees, the READ ones, the binary -- its
directory, or the BINARY alone when that directory lies in a written tree
(the rest of it reads through that tree) -- the LAUNCHER's files, then the
HIDDEN paths, a directory as an empty read-only one, a file as the null
device; a hidden path inside another hidden directory is left to it."
  (let* ((binary-tree (if (some (lambda (tree) (sandbox-path-under-p binary-dir tree)) written)
                          (uiop:native-namestring binary)
                          binary-dir))
         (hidden (mapcar (lambda (path) (string-right-trim "/" path)) hidden))
         (hidden-directories (remove-if-not directory-p hidden)))
    (append (mapcar (lambda (path) (list :write path)) written)
            (mapcar (lambda (path) (list :read path)) (append read (list binary-tree) launcher))
            (loop for path in hidden
                  unless (some (lambda (tree) (and (string/= tree path) (sandbox-path-under-p path tree)))
                               hidden-directories)
                    collect (list (if (member path hidden-directories :test #'string=) :hide-directory :hide-file)
                                  path)))))

(defun sandbox-link-target (path)
  "The target of the link PATH, or NIL when PATH is no link."
  #-win32 (ignore-errors (sb-posix:readlink path))
  #+win32 (declare (ignore path)))

(defun sandbox-bwrap-requirements ()
  "Why the bwrap jail cannot run on this machine, as one sentence, or NIL."
  (let ((bwrap (nlk::executable-on-path "bwrap")))
    (cond ((null (sandbox-machine))
           "--sandbox has no seccomp filter for this machine's architecture yet (x86-64 and arm64 only)")
          ((null bwrap) "--sandbox needs bubblewrap: install bwrap (the bubblewrap package)")
          ((not (eq nlk::*spawn-primitive* :posix))
           "--sandbox needs a libc with posix_spawn's closefrom (glibc 2.34 or later): it hands the jail its descriptors that way")
          ((not (sandbox-bwrap-works-p (uiop:native-namestring bwrap)))
           "--sandbox needs user namespaces, and this system refuses bwrap one (on Ubuntu, an AppArmor profile for bwrap; elsewhere kernel.unprivileged_userns_clone=1)"))))

(defun sandbox-bwrap-works-p (bwrap)
  "Whether BWRAP can make the jail's namespaces here."
  (eql 0 (nth-value 2 (ignore-errors
                       (uiop:run-program (list bwrap "--unshare-all" "--ro-bind" "/" "/" "true")
                                         :output nil :error-output nil :ignore-error-status t)))))

(defun sandbox-bwrap-disables-userns-p (bwrap)
  "Whether BWRAP takes --disable-userns (bubblewrap 0.8 and later)."
  (search "--disable-userns" (or (ignore-errors (uiop:run-program (list bwrap "--help") :output :string
                                                                                           :error-output :output
                                                                                           :ignore-error-status t))
                                 "")))

;;; --- the descriptors handed in --------------------------------------------

#+linux
(defun sandbox-seccomp-descriptor ()
  "A pipe's read end with the jail's seccomp program in it, whole: bwrap
reads it from there."
  (multiple-value-bind (read write) (sb-posix:pipe)
    (let ((program (sandbox-seccomp-program)))
      (unwind-protect
           (sb-sys:with-pinned-objects (program)
             (loop with start = 0 while (< start (length program))
                   do (let ((wrote (sb-unix:unix-write write program start (- (length program) start))))
                        (unless (and wrote (plusp wrote))
                          (sb-posix:close read)
                          (error "writing the jail's seccomp program failed"))
                        (incf start wrote))))
        (sb-posix:close write)))
    read))

#+linux
(defun sandbox-receive-egress (channel egress)
  "Wait, on a thread of its own, for the listening socket the jail sends
over CHANNEL (SANDBOX-HAND-OUT-EGRESS), and answer the jail's way out on it
under EGRESS (SANDBOX-EGRESS-START); CHANNEL is closed once that is done, or
once the jail ends without one."
  (spawn "sandbox-egress-receive"
    (unwind-protect
         (when-let (descriptor (ignore-errors (nlk::receive-descriptor channel)))
           (sandbox-egress-start
            :listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp
                                                                 :descriptor descriptor)
            :egress egress))
      (ignore-errors (sb-posix:close channel)))))

;;; --- inside the jail ------------------------------------------------------

(defun sandbox-environment-descriptor (name)
  "The descriptor the variable NAME names, or NIL."
  (let ((value (ignore-errors (parse-integer (or (uiop:getenv name) "")))))
    (and (integerp value) (<= 3 value) value)))

#+linux
(defun sandbox-seccomp-on-p ()
  "Whether a seccomp filter is on this process."
  (search (format nil "Seccomp:~c2" #\Tab)
          (or (ignore-errors (uiop:read-file-string "/proc/self/status")) "")))

#+linux
(defun sandbox-hand-out-egress (channel port)
  "Bind the egress PORT on the jail's own loopback and send the listening
socket out over CHANNEL, then close both here: the launcher accepts on it."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
                (sb-bsd-sockets:socket-bind listener #(127 0 0 1) port)
                (sb-bsd-sockets:socket-listen listener 128)
                (nlk::send-descriptor channel (sb-bsd-sockets:socket-file-descriptor listener)))
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (ignore-errors (sb-posix:close channel)))))

(defun sandbox-pump (from to)
  "Copy what FROM reads to TO until FROM ends, then end TO's sending side."
  (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
    (ignore-errors
     (loop (let ((length (nth-value 1 (sb-bsd-sockets:socket-receive from buffer nil))))
             (when (or (null length) (zerop length)) (return))
             (loop with start = 0 while (< start length)
                   do (let ((sent (sb-bsd-sockets:socket-send
                                   to (if (zerop start) buffer (subseq buffer start length))
                                   (- length start) :nosignal t)))
                        (unless (and sent (plusp sent)) (return-from sandbox-pump))
                        (incf start sent))))))
    (ignore-errors (sb-bsd-sockets:socket-shutdown to :direction :output))))

(defun sandbox-carry (client port)
  "Carry the connection CLIENT, accepted on the jail's way in, to the
gateway on the jail's loopback PORT, both ways, until both ends close."
  (let ((gateway (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (when (ignore-errors (sb-bsd-sockets:socket-connect gateway #(127 0 0 1) port) t)
           (let ((back (spawn "sandbox-inbound-back" (sandbox-pump gateway client))))
             (sandbox-pump client gateway)
             (ignore-errors (bt2:join-thread back))))
      (ignore-errors (sb-bsd-sockets:socket-close gateway))
      (ignore-errors (sb-bsd-sockets:socket-close client)))))

(defun sandbox-bridge-inbound (descriptor port)
  "Accept, on a thread of its own, every connection on the listener
DESCRIPTOR -- the shell's, on the host's loopback -- and carry each to the
gateway on the jail's loopback PORT (SANDBOX-CARRY)."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp
                                                             :descriptor descriptor)))
    (spawn "sandbox-inbound"
      (loop (let ((client (handler-case (sb-bsd-sockets:socket-accept listener)
                            (sb-bsd-sockets:interrupted-error () nil)
                            (error () (sleep 0.05) nil))))
              (when client
                (spawn "sandbox-inbound-connection" (sandbox-carry client port))))))))

#+linux
(defun sandbox-jail-enter ()
  "The jailed organism's first acts, before anything of its folder loads: a
jail with no seccomp filter on it stops here (exit 78); the egress listener
goes out to the launcher; the way in is bridged to the gateway's port. The
variables naming the descriptors are taken out of the environment, so
nothing this process starts takes them for its own."
  (when (sandbox-environment-descriptor "NODECODE_SANDBOX_INBOUND_FD")
    (unless (sandbox-seccomp-on-p)
      (format *error-output* "nodecode: --sandbox found no seccomp filter on the jail, so it could reach the unix sockets of the host; it does not run without one~%")
      (finish-output *error-output*)
      (sb-ext:exit :code 78 :abort t))
    (let ((inbound (sandbox-environment-descriptor "NODECODE_SANDBOX_INBOUND_FD"))
          (egress (sandbox-environment-descriptor "NODECODE_SANDBOX_EGRESS_FD"))
          (egress-port (ignore-errors (parse-integer (or (uiop:getenv "NODECODE_SANDBOX_EGRESS_PORT") ""))))
          (port (gateway-port-from-environment)))
      (dolist (name '("NODECODE_SANDBOX_INBOUND_FD" "NODECODE_SANDBOX_EGRESS_FD"))
        (ignore-errors (sb-posix:unsetenv name)))
      (when (and egress egress-port)
        (handler-case (sandbox-hand-out-egress egress egress-port)
          (error (condition)
            (format *error-output* "nodecode: the jail's way out could not be handed out: ~a~%" condition))))
      (when (and inbound port)
        (sandbox-bridge-inbound inbound port)))))
