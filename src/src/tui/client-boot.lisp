;;;; client-boot.lisp --- the organism a shell fronts, and the home asked before anything boots. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; A shell never runs the organism in its own process: a collection the
;;; organism's work causes stops every thread of the process it runs in, and a
;;; shell sharing it would freeze with it. The shell that finds the store free
;;; starts the organism as a headless serve in a process of its own and
;;; attaches over the socket, as every other shell does. Its stdin is a pipe
;;; from that shell, which is how it lives and dies with the shell, as it did
;;; inside it: a shell that quits writes why and lets go, and one that crashes
;;; or loses its terminal ends the pipe all the same.

(defparameter +front-variable+ "NODECODE_FRONT"
  "The environment word on an organism a shell started (FRONT-ORGANISM): the
shell's pid, and the sign that its stdin is that shell's pipe (WATCH-FRONT).")

(defvar *fronted* nil
  "True in an organism a shell started and holds by its stdin.")

(defun watch-front (&optional (pipe sb-sys:*stdin*))
  "When a shell started this headless owner (+FRONT-VARIABLE+), stop it once
that shell lets go of PIPE, its stdin, the line it wrote first, if any, kept
as the stop's reason for the next boot's note."
  ;; The word stays in the environment and the pipe on descriptor 0, so a
  ;; restart's exec (SERVE-NEXT :RESTART) still fronts for the same shell.
  (when (uiop:getenv +front-variable+)
    (setf *fronted* t)
    (spawn "front-watch"
      (let ((why (ignore-errors (read-line pipe nil nil))))
        (when (plusp (length why)) (setf *stop-reason* why))
        (serve-next :stop)))))

(define-error organism-exited (error) (status line)
  (:report "~:[the organism exited ~a before it served~;~:*~a~]" line status)
  (:documentation "The organism a shell started exited before it served, LINE
the last thing it said on its log, when it said anything."))

(defun log-line-after (path from)
  "The last line PATH's file holds past byte FROM, its `nodecode: ' lead off,
or NIL."
  (ignore-errors
   (with-open-file (in path :external-format :utf-8)
     (when (> (file-length in) from)
       (file-position in from)
       (let ((line (car (last (remove "" (mapcar (lambda (line) (string-trim '(#\Space #\Return) line))
                                                 (uiop:slurp-stream-lines in))
                                      :test #'string=)))))
         (if (and line (uiop:string-prefix-p "nodecode: " line))
             (subseq line (length "nodecode: "))
             line))))))

(defvar *front* nil
  "The organism this shell started, while it may run: (:PIPE STREAM :WAITER
THREAD), the pipe on its stdin and the thread that reaps it.")

(defun front-organism (store-path)
  "Start STORE-PATH's organism as a headless serve in a process of its own,
fronted by this shell, and return once it serves -- its gateway published,
which this shell's reconnector attaches to -- or once another process holds
the store, an election it lost. One that exits before either is a boot that
died: ORGANISM-EXITED, with the last line it wrote to its log."
  (let* ((command (or (serve-command store-path)
                      (error "this image names no nodecode binary to run the organism with")))
         ;; A home never used has no folder yet, and the log is the first
         ;; thing put in it: without this, the spawn opening it fails.
         (log (ensure-directories-exist (stdio-log-path store-path)))
         (from (or (nlk:file-bytes log) 0))
         (word (format nil "~a=" +front-variable+))
         (exited (list nil)))
    (bind (((pid _ _ process pipe)
            (spawn-program (session-argv command)
                           :environment (cons (format nil "~a~d" word (sb-posix:getpid))
                                              (remove-if (lambda (entry) (uiop:string-prefix-p word entry))
                                                         (sb-ext:posix-environ)))
                           :output log :error-output :output :input :stream)))
      (nlk:note-boot-phase "organism-spawned" pid)
      (setf *front* (list :pipe pipe
                          :waiter (spawn "organism-front"
                                    (setf (car exited) (wait-for-child pid process)))))
      (loop until (or (live-published-endpoint store-path) (car exited))
            do (sleep 0.005))
      (when (car exited)
        (setf *front* nil)
        (ignore-errors (close pipe))
        (unless (nlk::store-lease-held-p store-path)
          (error 'organism-exited :status (car exited) :line (log-line-after log from)))))))

(defun stop-front (&key why (wait t))
  "Let go of the organism this shell fronts: WHY, when given, is the reason it
keeps for the next boot's note (`quit'), and with WAIT the shell waits for it
to let the store go -- bounded, so a wedged organism is left to end on its own."
  ;; The wait is what lets the next launch find the store free instead of
  ;; attaching to an organism on its way out.
  (when-let (front (shiftf *front* nil))
    (let ((pipe (getf front :pipe)))
      (ignore-errors (when why (write-line why pipe)) (finish-output pipe))
      (ignore-errors (close pipe)))
    (when wait
      (nlk::%join-bounded (getf front :waiter) 10))))

(defvar *organism-notes-shown* '()
  "The organism.note texts this shell has put in its transcript: the
organism's last note comes again with every attach while it is fresh.")

(defvar *organism-build-noted* nil
  "The other build this shell last said it attached to, so a reconnect to
the same organism says nothing new.")

(defvar *organism-restart-asked* nil
  "The organism this shell asked to restart onto its own build, as (BUILD .
STARTED-AT) off its identity route: one that comes back on that same build
is said, never asked again.")

(defun organism-build-stance (label)
  "How this shell stands to the organism it attached to, LABEL the release
label the connect ack names, NIL when it names none: :SAME; :ORGANISM-STALE
when this shell runs the release the home's `current' names and the organism
does not, so the organism is the one to move; :SHELL-STALE when the organism
runs `current' and this shell does not, so the shell is the one to reopen;
else :APART, neither of them on `current' (a checkout's shell, a home with no
installed release)."
  (let ((mine (effective-label)) (current (ignore-errors (current-label))))
    (cond ((equal label mine) :same)
          ((null current) :apart)
          ((and (installed-release-root) (equal mine current)) :organism-stale)
          ((equal label current) :shell-stale)
          (t :apart))))

(defun note-organism-build (program payload &optional model)
  "Say once, when the organism this shell attached to runs another build
than the shell, which the two are and what moves them together: an organism
behind the release the home installed last restarts onto it once no turn is
in flight (ASK-ORGANISM-RESTART, with MODEL the shell's gateway model)."
  ;; PAYLOAD is the connect ack's. An ack carrying NO version is itself the
  ;; answer rather than nothing to say: the field arrives with the gateway
  ;; that knows to send it, so an organism that sends none predates that build
  ;; and is certainly older than this shell. Without this, a freshly installed
  ;; binary attached to a pre-upgrade organism would show no sign of the
  ;; mismatch.
  ;;
  ;; A shell carrying on against an older organism runs every turn on that
  ;; build's code, which a newer shell's session can need and not find: an
  ;; update that build refused live left it there, and each turn failed until
  ;; it was restarted by hand. A notice was all a shell did about it.
  (let* ((version (and (hash-table-p payload) (gethash "version" payload)))
         (build (if (stringp version) version :older))
         (from (if (stringp version) version "an older build"))
         (stance (organism-build-stance (and (stringp version) (version-label version))))
         (moving (and model (eq stance :organism-stale))))
    ;; The restart this shell asked for landed: said, and the ask is done.
    (when (and (eq stance :same) *organism-restart-asked*)
      (setf *organism-restart-asked* nil)
      (post-shell-notice program (format nil "the organism restarted and now runs ~a, this shell's build"
                                         (effective-version))
                         :error-p nil))
    ;; Once asked, every attach to that build asks again whether it restarted.
    (unless (or (eq stance :same)
                (and (member build (list (effective-version) *organism-build-noted*) :test #'equal)
                     (not (and moving (equal from (car *organism-restart-asked*))))))
      (setf *organism-build-noted* build)
      (if moving
          (spawn "organism-restart" (ask-organism-restart program model from))
          (post-shell-notice
           program
           (if (eq stance :shell-stale)
               (format nil "this shell runs ~a, older than the organism it attached to (~a), which is what this home installed last: quit and open nodecode again to run it"
                       (effective-version) version)
               (format nil "the organism this shell attached to runs ~:[build ~a; this shell is ~a~;a ~
                            build older than this shell (~*~a) — it does not report its own~]~
                            ~:[~; · `nodecode gateway restart' moves the gateway to this build~]"
                       (eq build :older) version (effective-version) (nlk:gateway-installed-p)))
           ;; It names something to do, so it stays until it is closed.
           :error-p nil :stays t)))))

(defun ask-organism-restart (program model from)
  "Ask the organism MODEL attached to, running FROM, to restart onto this
shell's build -- the release its home installed last -- once no turn is in
flight, and say so on PROGRAM's shell; or say plainly why it cannot, that
turns may fail on it meanwhile, and what moves it."
  ;; Off the UI thread: three localhost calls, each bounded. The restart
  ;; ends this shell's link, and the reconnector attaches it again to
  ;; whatever serves next.
  (let ((mine (effective-version)))
    (flet ((say (control &rest args)
             (post-shell-notice program (apply #'format nil control args) :error-p nil :stays t)))
      (handler-case
          (let* ((identity (gateway-model-http-call model :get "/api/gateway/identity" nil :read-timeout 5))
                 (started (and (hash-table-p identity) (gethash "started_at" identity)))
                 (service (gateway-model-http-call model :get "/api/gateway/service" nil :read-timeout 5))
                 (runs (and (hash-table-p service) (gethash "runs" service)))
                 (restart (and (hash-table-p service) (gethash "restart" service)))
                 (fix (cond ((equal runs "unit") "`nodecode gateway restart' moves it")
                            ((equal runs "shell") "quitting the shell that started it moves it")
                            (t (format nil "stopping it~@[ (pid ~a)~] and opening nodecode again moves it"
                                       (and (hash-table-p service) (gethash "pid" service)))))))
            (cond ((equal (car *organism-restart-asked*) from)
                   ;; Asked already: still waiting for idle, or back on the same build.
                   (unless (equal (cdr *organism-restart-asked*) started)
                     (setf *organism-restart-asked* (cons from started))
                     (say "the organism restarted and still runs ~a, not this shell's ~a: what starts it names that build, and turns here may fail · ~a"
                          from mine fix)))
                  ((not (and (hash-table-p restart) (gethash "able" restart)))
                   (say "the organism this shell attached to runs ~a, not this shell's ~a, and cannot restart itself~@[ (~a)~]: turns here may fail · ~a"
                        from mine (and (hash-table-p restart) (gethash "text" restart)) fix))
                  ((not (gethash "restart_idle" service))
                   ;; A gateway that predates the idle hold would restart at
                   ;; once, stopping the turns it runs for other sessions.
                   (say "the organism this shell attached to runs ~a, older than this shell's ~a, and cannot wait for its turns to end before it restarts: turns here may fail · ~a, stopping any turn in flight"
                        from mine fix))
                  (t
                   (gateway-model-http-call model :post "/api/gateway/restart"
                                            (json-object "when" "idle") :read-timeout 5)
                   (setf *organism-restart-asked* (cons from started))
                   (say "the organism this shell attached to runs ~a; this shell runs ~a, what this home installed last: the organism restarts onto it once no turn is running, and this shell attaches again"
                        from mine))))
        (error (condition)
          (say "the organism this shell attached to runs ~a, not this shell's ~a, and could not be asked to restart (~a): turns here may fail"
               from mine condition))))))

(defun post-shell-notice (prog text &key (error-p t) stays answer)
  "Say TEXT on PROG's shell from any thread (SURFACE-SYSTEM-NOTICE: a toast,
or with ANSWER the composer status), waiting briefly for the
run loop to start accepting — NLT:SEND drops messages while the program is
not running, and a boot thread's failure can beat the first frame by
milliseconds."
  ;; Falls back to *ERROR-OUTPUT* (the severed-stdio log) when the shell never
  ;; comes up.
  (loop repeat 50 until (or *system-notice-hook* (and prog prog.running)) do (sleep 0.1))
  (if (or *system-notice-hook* (and prog prog.running))
      (let ((*active-tui-program* prog))
        (surface-system-notice text :error-p error-p :stays stays :answer answer))
      (format *error-output* "~&~a~%" text)))

;;; --- the home, asked once before anything boots -----------------------------
;;; Without HOME-REFUSAL, a launch over a home it cannot use would paint its
;;; shell, open the wizard over a dead organism, fail on a read-only mount or
;;; unparseable config.jsonc at every step, and end on a summary of what it
;;; had not saved. HOME-REFUSAL is the one question, answered from disk, that
;;; MAIN asks before a shell or an organism exists: the launch that gets a
;;; sentence back prints it and exits.

(defun plain-error-text (condition)
  "CONDITION as an operator reads it: the system's own words for a failed
system call (`read-only file system'), a store it cannot read named from
`~', else the condition's report. The second value is T for the first two,
words written for the operator, and NIL for a report, which may be Lisp's."
  (typecase condition
    (sb-posix:syscall-error
     (values (format nil "~(~a~)" (sb-int:strerror (sb-posix:syscall-errno condition))) t))
    ;; The store's sentence is the kernel's; only its path is written the way
    ;; the operator writes their own home.
    (nlk::store-unreadable
     (values (princ-to-string (make-condition 'nlk::store-unreadable
                                              :path (home-abbreviated (nlk::store-unreadable-path condition))
                                              :reason (nlk::store-unreadable-reason condition)))
             t))
    ;; The organism's own line, when its log ended on one.
    (organism-exited (values (princ-to-string condition) (and (organism-exited-line condition) t)))
    (t (values (princ-to-string condition) nil))))

(defmacro errno-of (&body body)
  "The errno the system call in BODY fails with, else NIL."
  `(with-handlers ((sb-posix:syscall-error (e) (sb-posix:syscall-errno e))) ,@body nil))

(defun folder-write-errno (folder)
  "NIL when a file can be made in FOLDER, else the errno that says why not."
  ;; A file made and removed, the one test a read-only mount, a permission and
  ;; a full disk all fail alike; an access(2) call would not see the last.
  (let ((probe (uiop:native-namestring
                (merge-pathnames (format nil ".nodecode-write-probe-~d" (sb-posix:getpid))
                                 folder))))
    (errno-of (sb-posix:close (sb-posix:open probe (logior sb-posix:o-creat sb-posix:o-excl
                                                           sb-posix:o-wronly)
                                             #o600))
              (ignore-errors (sb-posix:unlink probe)))))

(defun unwritable-words (errno)
  "What ERRNO, a refused write, says about the place: `read-only', `not
writable by this account', else `not writable (the system's words)'."
  (case errno
    ;; EROFS, then EPERM and EACCES: the numbers Linux, macOS and the Windows C
    ;; runtime agree on, which sb-posix names only where it has them.
    (30 "read-only")
    ((1 13) "not writable by this account")
    (t (format nil "not writable (~(~a~))" (sb-int:strerror errno)))))

(defun folder-refusal (folder hint)
  "A sentence saying why nodecode cannot write in FOLDER -- a file or a link
to nothing stands where it belongs, or the folder refuses the write -- ending
in what to do and HINT, or NIL when it can."
  ;; A folder that is not there yet is judged by the nearest thing on its path
  ;; that is: the folder it would be made in, or the file or the link to
  ;; nothing that no mkdir gets past (`Can't create directory').
  (let* ((folder (uiop:ensure-directory-pathname folder))
         (holder folder))
    (flet ((native (path) (string-right-trim "/\\" (uiop:native-namestring path))))
      (loop until (or (uiop:directory-exists-p holder)
                      (member (nlk::lstat-kind (native holder)) '(:file :link :other)))
            do (setf holder (uiop:pathname-parent-directory-pathname holder)))
      (let ((it (if (eq holder folder) "it" (home-abbreviated (native holder))))
            (target (nlk::symlink-target holder))
            (unfollowed (errno-of (sb-posix:stat (native holder)))))
        (flet ((sentence (why fix)
                 (if (eq holder folder)
                     (format nil "~a ~a; ~a ~a" (home-abbreviated (native folder)) why fix hint)
                     (format nil "~a does not exist and ~a ~a, so nodecode cannot make it; ~a ~a"
                             (home-abbreviated (native folder)) it why fix hint))))
          (cond ((uiop:directory-exists-p holder)
                 (when-let (errno (folder-write-errno holder))
                   (sentence (format nil "is ~a" (unwritable-words errno))
                             (if (eq holder folder)
                                 "nodecode needs to write there, so make it writable"
                                 (format nil "make ~a writable" it)))))
                ;; A link stat cannot follow (lstat answers the link, stat its
                ;; target): gone is ENOENT or ENOTDIR; a loop or a folder this
                ;; account cannot enter is there, and the system says which.
                ((and target unfollowed)
                 (if (member unfollowed (list sb-posix:enoent sb-posix:enotdir))
                     (sentence (format nil "links to ~a, which does not exist" (home-abbreviated target))
                               "create that folder or remove the link,")
                     (sentence (format nil "links to ~a, which nodecode cannot reach (~(~a~))"
                                       (home-abbreviated target) (sb-int:strerror unfollowed))
                               "fix the link or remove it,")))
                (t (sentence "is a file, not a folder" (format nil "move ~a aside" it)))))))))

(defun sqlite-file-p (path)
  "Whether the file at PATH is empty -- a store SQLite makes on its first open
-- or begins with the header every SQLite database begins with."
  ;; Anything else is what a crash or a full disk leaves, or no store at all:
  ;; SQLite refuses it (NOTADB) at the boot's first statement.
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((head (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
      (or (zerop (read-sequence head in))
          (equalp head (sb-ext:string-to-octets (format nil "SQLite format 3~c" (code-char 0))))))))

(defun store-refusal (store-path)
  "A sentence saying why the launch cannot use its store -- its path names no
one file, its folder cannot be written, or what is there is a folder, a pipe
or a device, refuses the write or is no SQLite database -- or NIL when it can.
STORE-PATH is the store the launch named, NIL for the home's own."
  (let* ((path (ignore-errors (merge-pathnames (or store-path nlk::*default-store-path*))))
         ;; SBCL reads *, ? and [ in a path as a pattern: no one file and no
         ;; native name, and the boot's first open died on it.
         (native (and path (not (wild-pathname-p path)) (uiop:native-namestring path)))
         (shown (and native (home-abbreviated native))))
    (or (unless native
          (format nil "--store ~a: nodecode cannot use a store path with *, ? or [ in it; name one ~
                       without them"
                  (home-abbreviated (princ-to-string store-path))))
        (folder-refusal (uiop:pathname-directory-pathname path)
                        "or name a store elsewhere with --store PATH")
        (cond ((uiop:directory-exists-p path)
               (if store-path
                   (format nil "~a is a folder, not a store file; name a file, such as --store ~a/history.db"
                           shown shown)
                   (format nil "~a is a folder, not a store file; move it aside and run nodecode again" shown)))
              ;; Not there yet, or a link to a file not there yet: the boot
              ;; makes it. Stat follows the link; PROBE-FILE answers the link.
              ((not (ignore-errors (sb-posix:stat native))) nil)
              ;; A pipe or a device, which the header's read below would wait
              ;; on for ever (`--store /dev/tty'): nothing is opened.
              ((not (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:stat native))))
               (format nil "~a is a device or a pipe, not a store file; ~:[move it aside and run nodecode ~
                            again~;name a file with --store PATH~]"
                       shown store-path))
              ;; Opened for reading and writing and closed at once, the way the
              ;; boot opens it: a 0444 file opened, then failed its first write.
              (t (or (when-let (errno (errno-of
                                       (sb-posix:close (sb-posix:open native sb-posix:o-rdwr))))
                       (format nil "~a is ~a; nodecode needs to write to it, so make it writable or name a ~
                                    store elsewhere with --store PATH"
                               shown (unwritable-words errno)))
                     ;; The words a boot says of what got past this check.
                     (unless (sqlite-file-p path)
                       (plain-error-text (make-condition 'nlk::store-unreadable :path native)))))))))

(defun shared-config-blank-p ()
  "Whether the shared config holds nothing but whitespace and comments."
  ;; The writer starts such a document over (NLK:SET-JSONC-PATH), so it is a
  ;; config to write; the reader's verdict on it is `the text ends where a
  ;; value belongs'.
  (ignore-errors
   (every #'nlk::jsonc-space-p
          (nlk::normalize-jsonc (uiop:read-file-string *shared-config-path*)))))

(defun shared-config-refusal ()
  "A phrase saying the shared config is there and cannot be read --
`~/.nodecode/config.jsonc is not valid JSONC (byte 77)' -- or NIL when it
reads, is absent, or is blank."
  (multiple-value-bind (config text) (read-shared-config)
    (when (and (null config) text (not (shared-config-blank-p)))
      (format nil "~a ~a" (home-abbreviated (uiop:native-namestring *shared-config-path*))
              (if-let (at (ppcre:register-groups-bind (at) ("at byte (\\d+)" text) at))
                (format nil "is not valid JSONC (byte ~a)" at)
                (format nil "cannot be read (~a)" text))))))

(defun home-refusal (store-path)
  "One plain sentence saying why this launch cannot use its home or its store
-- the home is read-only, a file or a link to nothing, config.jsonc does not
parse, the store is a folder, read-only or no SQLite database -- or NIL when
it can. STORE-PATH is the store the launch named, NIL for the home's own."
  ;; Answered from disk, before the shell paints or the organism boots, for
  ;; every launch that runs one: the shell, `setup', `import', `serve' and the
  ;; gateway's unit. `--ephemeral' takes it too: its store is in the temp
  ;; folder, but the operator token, the cell links and the config are the
  ;; home's, and a boot over a read-only home died on all of them.
  (or (folder-refusal (nlk:home) "or set NODECODE_HOME to another folder")
      (when-let (phrase (shared-config-refusal))
        (format nil "~a; fix it or move it aside, then run nodecode setup" phrase))
      (store-refusal store-path)))

(defvar *organism-boot-failure* nil
  "The line said of the organism this process's shell fronts when it died at
boot, or NIL while none has: what the launch exits on, and what a wait on
that organism answers at once instead of waiting for a gateway nothing will
serve.")

(defun organism-boot-failure-line (condition store-path &optional (what "could not start"))
  "What a shell says when its organism's boot died of CONDITION, in one line:
WHAT failed -- by default that nodecode could not start -- and why, in the
words PLAIN-ERROR-TEXT has for it -- the store's own sentence, the system's
words for a refused call -- or, for a condition with none, where to read why:
STORE-PATH's stdio log, which this writes it to, or why that log could not be
written."
  ;; The condition's own text is Lisp's -- `The slot ASDF/COMPONENT:OPERATION-TIMES
  ;; is unbound in the object #<SYSTEM ...>' -- so only the log holds it. Words
  ;; written for the operator are the line itself, never a pointer to the log that
  ;; holds them: a damaged store's `move it aside' would sit in the log behind `see
  ;; ...stdio.log'. Nothing promises a restart mends a cause that is still there (a
  ;; corrupt store, a home that is a file).
  (multiple-value-bind (text plain) (plain-error-text condition)
    (let* ((log (stdio-log-path (or store-path nlk::*default-store-path*)))
           ;; NIL when the log took the reason, else the system's words for
           ;; why it did not, or T when there are none. sb-posix's open only
           ;; asks, since its refusal carries the system's words (`is a
           ;; directory') where OPEN's is Lisp's; the write is by name, as an
           ;; fd-stream on Windows wraps a HANDLE, never a C runtime descriptor
           ;; (scribe.lisp, WRITE-FILE-ATOMICALLY).
           (refused (with-handlers ((error (condition)
                                      (multiple-value-bind (words plain)
                                          (plain-error-text condition)
                                        (if plain words t))))
                      (sb-posix:close (sb-posix:open (uiop:native-namestring log)
                                                     (logior sb-posix:o-wronly sb-posix:o-creat
                                                             sb-posix:o-append)
                                                     #o600))
                      (with-open-file (out log :direction :output :if-exists :append
                                               :if-does-not-exist :create
                                               :external-format :utf-8)
                        (format out "~&organism boot failed: ~a~%" text))
                      nil))
           (shown (home-abbreviated (uiop:native-namestring log))))
      (cond (plain (format nil "~a: ~a" what text))
            ((null refused) (format nil "~a; the reason is in ~a" what shown))
            (t (format nil "~a, and its reason could not be written to ~a~@[ (~a)~]"
                       what shown (and (stringp refused) refused)))))))

(defun call-organism-boot (boot shell store-path)
  "Run BOOT — the organism the shell fronts — with SHELL, a function of no
arguments answering that shell's program; STORE-PATH is the store it opens,
NIL for the home's own."
  ;; The boot starts before the program is built (the store, the listener and
  ;; the in-process attach overlap the frame's construction), so the program
  ;; is asked for where it is needed, never held. A boot that dies keeps its
  ;; line (*ORGANISM-BOOT-FAILURE*), which the setup walk's wait on this
  ;; organism's gateway ends on at once. A shell that never attached is ended
  ;; on it by its own reconnector, through the quit its loop reads -- what
  ;; /exit sends, so the loop gives the terminal back, never this thread --
  ;; and the launch exits 1 on it, as a refused launch does
  ;; (ATTACH-OR-OWN-ORGANISM): without this, a shell kept up over a dead
  ;; organism would walk the setup over it and take a task it could not send.
  ;; An attached shell is taking over an organism whose owner let the store go
  ;; (the clean-shutdown rung): it served a session, so it stays, and is told.
  (setf *organism-boot-failure* nil)
  (handler-case (funcall boot shell)
    (error (condition)
      (let* ((prog (funcall shell))
             (attached (gateway-model-ws prog.model)))
        (setf *organism-boot-failure*
              (organism-boot-failure-line condition store-path
                                          (if attached
                                              "could not take the store over"
                                              "could not start")))
        (when attached
          (post-shell-notice prog *organism-boot-failure*))))))

(defvar *boot-trim-done* nil
  "Whether this process already gave the boot's pages back after its shell's
ready frame: the trim runs once per process, on the first attach.")

(defun trim-behind-the-ready-frame ()
  "Give the boot's pages back once the shell has painted the frame its
attach produces — once per process, on the first attach."
  ;; TRIM-MEMORY forces a collection and unmaps the image's own pages:
  ;; measured 32 MB given back on a fresh warm boot, 33–291 ms of
  ;; stop-the-world. Before it, the pause was the last thing standing between
  ;; the operator and a usable shell; behind the ready frame nobody is waiting
  ;; inside it, and the pages go back within a couple of hundred milliseconds
  ;; of the shell coming up.
  (unless (sb-ext:compare-and-swap (symbol-value '*boot-trim-done*) nil t)
    (let ((at nlt:*frames-on-screen*))
      (spawn "boot-trim"
        (nlt:wait-for-frames-past at)
        ;; The pause lands on a still screen, never inside a repaint: a
        ;; frame arriving in the window means the operator is working,
        ;; and the trim waits for the next quiet quarter-second.
        (loop for seen = nlt:*frames-on-screen*
              do (sleep 0.25)
              until (= seen nlt:*frames-on-screen*))
        (nlk:note-boot-phase "trim-start")
        (nlk:trim-memory)
        (nlk:note-boot-phase "trim-done")))))

(defun note-shell-attached (program model link &key build-payload)
  "The one post-attach step both transports take: the shell is on the
gateway."
  ;; Said to the UI thread, which owns the lifecycle — its :connected is
  ;; what the composer's ready line follows — with the build it attached to, the
  ;; session directory warmed behind that paint and the model's subscription
  ;; sent; the boot's pages go back, and the folder's peripherals start,
  ;; behind the frame that subscription's replay paints. The
  ;; socket path (a shell in another process) and the owner's in-process attach
  ;; (gateway.lisp ATTACH-LOOPBACK-GATEWAY-PEER) must not drift: this is the
  ;; whole of what attaching means.
  (nlk:note-boot-phase "connected")
  (note-organism-build program build-payload model)
  ;; Read the session directory once, behind the paint: the /sessions
  ;; picker then has rows for the first keystroke instead of only the
  ;; reading copy. One in flight per shell — a reconnect the cache already
  ;; answered for spawns nothing.
  (when (and (null (cached-session-directory model))
             (not (eq (getf (gethash model *session-directory*) :at) :reading)))
    (refresh-session-directory model))
  (nlt:send program (make-instance 'gw-connection-msg :ws link))
  ;; Resubscribe the model's *current* session: a reconnect after /clear
  ;; or /sessions must not re-attach the boot session -- and the session
  ;; its preview lane follows. The subscribe is what proves the link:
  ;; its frames carry the replay that paints the transcript.
  (send-subscribe link (subscription-entries model))
  (note-shell-presence model.session-id))

(defun gateway-client (port token)
  "A WebSocket client of the gateway at PORT, TOKEN its bearer when there is one."
  (wsd:make-client (format nil "ws://127.0.0.1:~d/gateway" port)
                   :additional-headers
                   (and token (list (cons "authorization" (format nil "Bearer ~a" token))))))

(defun answer-gateway-challenge (ws frame mode token
                                 &optional (instance (format nil "sbcl-~d" (sb-posix:getpid))))
  "Answer the connect challenge FRAME on WS as this nodecode's MODE client
INSTANCE, TOKEN its operator token."
  (wsd:send ws (encode-gateway-connect (gethash "challenge_nonce" (nlk:sync-message-payload frame))
                                       "nodecode" *user-agent-version* mode (string-downcase (nlk:platform))
                                       instance :operator-token token)))

(defun run-chat-gateway (&key session-id port
                              (store-path nlk::*default-store-path*)
                              boot setup)
  "Run the Gateway TUI with a reconnecting reader and UI-owned state. => the
session the shell ended on, or NIL when it never attached."
  ;; The shell paints FIRST. Nothing in the first frame needs a gateway — the
  ;; session id is client-chosen, the band is in-memory, the composer
  ;; placeholder is the :connecting lifecycle — so endpoint resolution and the
  ;; session ensure live on the reconnector thread, not in this prologue: PORT
  ;; NIL means the ladder polls STORE-PATH's holder sidecar (the one endpoint
  ;; authority for kernel-assigned ports) until an owner publishes, then
  ;; ensures the session and connects. BOOT, when given, is the organism this
  ;; shell fronts — a function called with the program, so its notices can
  ;; wait on the shell going live: it runs on its own organism-boot thread the
  ;; moment the program exists — the entry ladder starts the organism in a
  ;; process of its own through it (FRONT-ORGANISM) — its death ends a shell
  ;; that never attached (CALL-ORGANISM-BOOT), and the unwind lets go of the
  ;; organism it started (STOP-FRONT).
  ;; STORE-PATH also names the store whose sidecar the ladder re-resolves
  ;; after a sustained outage.
  (engage-theme)
  ;; Programs outside the organism read which session this shell is on
  ;; from its presence record (NOTE-SHELL-PRESENCE).
  (open-shell-presence)
  (let* ((pinned (and port t))
         (port (or port (live-published-endpoint store-path)))
         (token (gateway-configured-operator-token))
         (sid (or session-id (fresh-session-id)))
         (model (make-instance 'gateway-chat-model :session-id sid :ws nil
                                                   :http-port port
                                                   :http-token token
                                                   :setup-requested setup))
         (program nil)
         (stop nil)
         ;; The organism this shell took over after its owner let the store
         ;; go (the clean-shutdown rung), or NIL: joined on the way out like
         ;; the first boot.
         (takeover nil)
         (ingress-slots (bt2:make-semaphore :count 256
                                            :name "gateway-client-ingress"))
         ;; One inbound frame into the fold, bounded by the ingress slots: the
         ;; socket's projection arm.
         (release (lambda () (bt2:signal-semaphore ingress-slots)))
         (ingest (lambda (msg)
                   (bt2:wait-on-semaphore ingress-slots)
                   (dispatch-inbound-frame program model msg :release release)))
         ;; The shell program, answered to the boot's rare notices once it
         ;; exists.
         (shell (lambda ()
                  (loop until program do (sleep 0.001))
                  program))
         (start-organism (lambda ()
                           (spawn "organism-boot" (call-organism-boot boot shell store-path))))
         ;; And the organism this shell fronts boots NOW, on its own thread:
         ;; the store, the listener and the in-process attach overlap the
         ;; shell program's construction instead of following it. Nothing in
         ;; the boot before this shell's first frame needs the program.
         (boot-thread (and boot (funcall start-organism))))
    (let ((prog (nlt:make-program model)))
      (nlk:note-boot-phase "shell-program")
      (setf model.program prog program prog)
      (spawn "gateway-client-reconnector"
        (loop with delay = *sync-backoff-base-seconds*
              with failures = 0
              with outage-started = nil
              with failure-reported = nil
              with resurrect-attempts = 0
              with last-resurrect = nil
              with shutdown-reported = nil
              with stopping-reported = nil
              with stopping-since = nil
              ;; The session ensure of the lap in flight: (DONE . OUTCOME),
              ;; OUTCOME :ENSURED, the condition it failed with, or NIL
              ;; while it runs.
              with ensure = nil
              with ensure-reported = nil
              with unresolved-since = nil
              with unresolved-reported = nil
              with starting-reported = nil
              ;; An attach ends the outage: the next one walks the ladder from its start.
              with reset-outage = (lambda ()
                                    (setf delay *sync-backoff-base-seconds* failures 0
                                          outage-started nil failure-reported nil
                                          resurrect-attempts 0 last-resurrect nil
                                          shutdown-reported nil stopping-reported nil
                                          stopping-since nil))
              ;; A spawn the ladder may make now: counted and stamped, => true.
              with resurrect = (lambda (failures elapsed)
                                 (when (resurrection-due-p
                                        :dirty-death :failures failures :elapsed elapsed
                                        :attempts resurrect-attempts :last-spawn-elapsed
                                        (and last-resurrect (/ (since-ms last-resurrect) 1000)))
                                   (incf resurrect-attempts)
                                   (setf last-resurrect (get-internal-real-time))))
              until stop
              do (block lap
                   ;; The organism this shell fronts died at boot before the
                   ;; shell ever attached: it ends on the boot's line
                   ;; (CALL-ORGANISM-BOOT). SEND drops the quit until the loop
                   ;; runs, so it goes again each lap until one lands.
                   (when (and boot *organism-boot-failure* (null model.ws))
                     (nlt:send program (nlt:make-quit-msg))
                     (sleep 0.025)
                     (return-from lap))
                   ;; Paint-first: the endpoint resolves HERE. The shell is
                   ;; already on screen; the owner booting behind it (or a
                   ;; sibling process) publishes the sidecar a beat later,
                   ;; and until then there is nothing to dial — a quiet
                   ;; fast poll, no failure accounting, no notices.
                   (unless port
                     (let ((published (live-published-endpoint store-path)))
                       (unless published
                         ;; Nobody holds the store: a shell that starts
                         ;; organisms starts one now, the moment the one
                         ;; before let go -- not after a dial against its
                         ;; dead port failed a backoff later
                         ;; (FRONT-ORGANISM). One start at a time, each
                         ;; spending the outage's budget as a resurrection
                         ;; does. One that holds it while it stops is
                         ;; waited out, and said once.
                         (when (and boot (not pinned)
                                    (not (and boot-thread (bt2:thread-alive-p boot-thread)))
                                    (not (and takeover (bt2:thread-alive-p takeover))))
                           (cond ((ignore-errors (nlk::store-lease-held-p store-path))
                                  ;; Said only past a second and a half: a stop
                                  ;; takes under one, and a notice for it only
                                  ;; flickered.
                                  (setf stopping-since (or stopping-since (get-internal-real-time)))
                                  (when (and (not stopping-reported)
                                             (> (since-ms stopping-since) 1500))
                                    (setf stopping-reported t)
                                    (post-shell-notice
                                     program
                                     "the nodecode before this one is still stopping; this shell starts its own once it lets go"
                                     :error-p nil)))
                                 ((funcall resurrect 1 0)
                                  (setf takeover (funcall start-organism)))))
                         ;; Quiet fast poll — but never SILENT forever:
                         ;; past the threshold (a shell fronting its own
                         ;; boot gets the outage window; a bare attach
                         ;; shell, which nothing in-process will ever
                         ;; publish for, gets seconds) say once what is
                         ;; being waited on, and keep watching. A boot
                         ;; that died has said so in its own line, and
                         ;; this one would only ask who serves.
                         ;; A boot of this shell's own that is still on its
                         ;; way is no missing owner: it is said as what it
                         ;; waits on -- the layer file loading now, else the
                         ;; organism's log -- and the failure stays for a
                         ;; boot that ended without one serving.
                         (setf unresolved-since (or unresolved-since (get-internal-real-time)))
                         (when (and (not (and boot *organism-boot-failure*))
                                    (> (since-ms unresolved-since)
                                       (* 1000
                                          (if boot
                                              *sync-failure-after-seconds*
                                              5))))
                           (cond ((or (and boot-thread (bt2:thread-alive-p boot-thread))
                                      (and takeover (bt2:thread-alive-p takeover)))
                                  (unless starting-reported
                                    (setf starting-reported t)
                                    (post-shell-notice
                                     program
                                     (still-starting-text (round (since-ms unresolved-since) 1000)
                                                          nlk::*layer-file-loading*
                                                          (stdio-log-path store-path))
                                     :error-p nil)))
                                 ((not unresolved-reported)
                                  (setf unresolved-reported t)
                                  (report-transport-error
                                   program :failed "store ~a advertises no gateway ~
                                                 endpoint; still watching its ~
                                                 sidecar — is an owner serving?"
                                   (namestring store-path)))))
                         ;; The organism publishes from a process of its
                         ;; own, which signals nothing here: a short lap
                         ;; dials within moments of the port landing.
                         (sleep 0.005)
                         (return-from lap))
                       (nlk:note-boot-phase "endpoint-seen" published)
                       (setf unresolved-since nil
                             unresolved-reported nil
                             starting-reported nil
                             port published
                             token (or token
                                       (gateway-configured-operator-token))
                             model.http-port port
                             model.http-token token)))
                   ;; The session ensure runs BESIDE the connect, not
                   ;; before it. Its only consumer is the subscribe that
                   ;; follows the ack, and the ack handler waits for it
                   ;; there — while the dial, the upgrade and the
                   ;; challenge, which need nothing from the session, go
                   ;; out at once.
                   ;;
                   ;; Every lap ensures, against the owner its connection
                   ;; reaches: a session not yet spoken in stands by in its
                   ;; owner's memory (standby.lisp), so an owner that dies
                   ;; takes it along and the one that replaces it never heard
                   ;; of it. Ensured once per shell; without that, a shell
                   ;; whose owner dies under its first prompt would subscribe
                   ;; the replacement to a session it does not hold: refused,
                   ;; no replay, and the prompt that waits on the replay to
                   ;; be re-sent would sit at `waiting' for good. An owner
                   ;; that holds the session answers one GET. Each lap's
                   ;; outcome is its own place, so an ensure still in flight
                   ;; against a dead owner never answers for the next one.
                   (let ((attempt (list (bt2:make-semaphore :name "shell-session-ensure")))
                         (port port))
                     (setf ensure attempt)
                     (spawn "shell-session-ensure"
                       (with-handlers ((error (condition)
                                         ;; The endpoint that refused may be stale or
                                         ;; gone: re-read the sidecar (unpinned ports
                                         ;; only) so the next lap ensures against
                                         ;; whoever actually serves.
                                         (setf (cdr attempt) condition)
                                         (unless pinned
                                           (setf model.http-port
                                                 (live-published-endpoint store-path)))))
                         (nlk:note-boot-phase "ensure-start")
                         (ensure-gateway-session sid port token)
                         (nlk:note-boot-phase "session-ensured")
                         (setf (cdr attempt) :ensured)
                         ;; The facts read goes out HERE, the moment
                         ;; the session it resolves against exists,
                         ;; not behind the connect ack: it runs
                         ;; beside the dial, the upgrade and the
                         ;; challenge instead of after all three,
                         ;; which puts the identity in the first
                         ;; frames
                         ;; The ack's own refresh stays as the
                         ;; reconciliation for a later connection
                         (refresh-gateway-model-facts model))
                       (bt2:signal-semaphore (car attempt))))
                   (let ((ws (gateway-client port token))
                       (handshake-complete nil)
                       ;; This lap's own ensure, which the ack waits on.
                       (attempt ensure)
                       (permanent nil)
                       ;; wsd client START-CONNECTION does not block for the
                       ;; connection's lifetime (its reader runs on a thread
                       ;; the driver owns); this semaphore is the lifetime.
                       (closed (bt2:make-semaphore :name "gateway-client-closed")))
                   (wsd:on :message ws
                           (lambda (msg)
                             (handler-case
                                 (let ((frame (decode-sync-message msg)))
                                   (case frame.type
                                     (:gateway-connect-challenge
                                      (nlk:note-boot-phase "ws-challenge")
                                      ;; The challenge completes before any command.
                                      (answer-gateway-challenge ws frame "tui" token))
                                     (:gateway-connect-ack
                                      ;; A successful attach ends the outage:
                                      ;; a long-lived client that drops later
                                      ;; gets the same ladder as one that
                                      ;; never attached.
                                      ;; The subscribe below needs the
                                      ;; session, and the ensure making one
                                      ;; has been in flight since the dial
                                      ;; began: the handshake waits for it
                                      ;; here, never before the dial. An
                                      ;; ensure that did not land drops
                                      ;; this connection — the ladder
                                      ;; re-laps it, exactly as the old
                                      ;; serial order did.
                                      (bt2:wait-on-semaphore (car attempt) :timeout 15)
                                      (setf handshake-complete (eq :ensured (cdr attempt)))
                                      (funcall reset-outage)
                                      ;; A takeover that finished — owning,
                                      ;; or attached to whoever won — may be
                                      ;; needed again on a later clean stop.
                                      (when (and takeover (not (bt2:thread-alive-p takeover)))
                                        (setf takeover nil))
                                      (cond ((not handshake-complete)
                                             (unless ensure-reported
                                               (setf ensure-reported t)
                                               (report-transport-error
                                                program nil "~a"
                                                (or (cdr attempt)
                                                    "the session ensure did not land")))
                                             (bt2:signal-semaphore closed))
                                            (t (note-shell-attached
                                                program model ws
                                                :build-payload frame.payload))))
                                     (:gateway-connect-rejected
                                      (setf permanent t)
                                      (report-transport-error
                                       program :permanent "~a"
                                       (or (gethash "message" frame.payload)
                                           "Gateway connection rejected"))
                                      (bt2:signal-semaphore closed))
                                     (:projection
                                      (when handshake-complete
                                        (funcall ingest msg)))))
                               (protocol-error (condition)
                                 (setf permanent t)
                                 (report-transport-error program :permanent "~a"
                                                         (protocol-error-message condition))))))
                   (wsd:on :close ws
                           (lambda (&key &allow-other-keys)
                             (unless permanent
                               (report-transport-error program nil "gateway disconnected"))
                             (bt2:signal-semaphore closed)))
                   (with-handlers ((error (condition)
                                     (unless permanent
                                       (report-transport-error program nil "~a" condition))))
                     (nlk:note-boot-phase "ws-dialing")
                     (wsd:start-connection ws)
                     (nlk:note-boot-phase "ws-driver-started")
                     ;; Park here for the connection's lifetime; the
                     ;; :close callback releases us. Closing immediately
                     ;; after START-CONNECTION returns would kill every
                     ;; connection milliseconds after the upgrade. Each
                     ;; timed-out wait sends a keepalive ping so the
                     ;; server's read timeout never fires on a healthy
                     ;; idle connection; SEND-PING no-ops unless the
                     ;; socket is :open, and a write on a dead socket
                     ;; signals into the reconnect ladder below.
                     (loop until (bt2:wait-on-semaphore
                                  closed :timeout *sync-keepalive-seconds*)
                           do (wsd:send-ping ws)))
                   (ignore-errors (wsd:close-connection ws))
                   (when permanent
                     (return))
                   (unless stop
                     ;; One failed attempt (connect refused, handshake never
                     ;; completed, or an established stream dropped): walk
                     ;; the self-heal ladder before backing off.
                     (incf failures)
                     (unless outage-started
                       (setf outage-started (get-internal-real-time)))
                     ;; An endpoint nobody serves any more -- its owner
                     ;; stopping or gone -- is not dialed again: the next lap
                     ;; watches the store instead, and starts or attaches to
                     ;; whatever comes next the moment it does.
                     (unless (or pinned (live-published-endpoint store-path))
                       (setf port nil
                             delay *sync-backoff-base-seconds*))
                     (let ((elapsed (/ (since-ms outage-started)
                                       1000)))
                       (unless failure-reported
                         (cond ((>= elapsed *sync-failure-after-seconds*)
                                (setf failure-reported t)
                                (report-transport-error program :failed "gateway unreachable ~
                                         for ~ds; still retrying"
                                                        (round elapsed)))
                               ;; Four failures escalate past a quiet reconnect notice.
                               ((>= failures 4)
                                (report-transport-error
                                 program :reconnecting "reconnect attempt ~d failed" failures))))
                       ;; Endpoint rediscovery, every lap past the threshold
                       ;; (and every lap once a candidate was spawned).
                       ;; REDISCOVER-GATEWAY-ENDPOINT answers only when the
                       ;; sidecar names a port that differs, so the notice
                       ;; fires exactly on movement. The old one-shot probe
                       ;; missed any owner that published after its single
                       ;; look — a respawned owner always does.
                       ;; Six failures: above the escalation, so the user sees the problem first.
                       (when (or (>= failures 6) (plusp resurrect-attempts))
                         (when-let (published (rediscover-gateway-endpoint
                                               store-path port))
                           (setf port published
                                 model.http-port published
                                 ;; A moved endpoint is a live gateway:
                                 ;; probe it now, not after the outage's
                                 ;; accumulated backoff.
                                 delay *sync-backoff-base-seconds*)
                           (report-transport-error
                            program :reconnecting "gateway endpoint moved; ~
                                            retrying on port ~d"
                            published)))
                       ;; Resurrection rung: only a DIRTY death (lock freed
                       ;; with the dead owner, sidecar still advertising it)
                       ;; licenses respawning. A clean shutdown deleted the
                       ;; sidecar deliberately; auto-respawning there would
                       ;; resurrect a server the operator just stopped.
                       (multiple-value-bind (verdict owner-pid)
                           (nlk::store-death-verdict store-path)
                         (case verdict
                           (:clean-shutdown
                            (cond
                              ;; The gateway supervises this store: a clean
                              ;; stop is its restart, and systemd brings
                              ;; the organism back. The dead endpoint goes,
                              ;; so the next lap watches the sidecar for
                              ;; the new one instead of dialing the old.
                              ((gateway-supervises-p store-path)
                               (unless shutdown-reported
                                 (setf shutdown-reported t)
                                 (report-transport-error
                                  program :reconnecting
                                  "the gateway stopped; attaching again when it is back"))
                               (unless pinned
                                 (setf port nil)))
                              ;; Nothing supervises it, and this shell
                              ;; fronts an organism: the owner let the store
                              ;; go on purpose — the gateway turned off, a
                              ;; sibling shell closed — and this shell takes
                              ;; the organism, the way a first launch does,
                              ;; then attaches to itself.
                              ((and boot (not pinned) (null takeover))
                               (setf takeover (funcall start-organism) port nil)
                               ;; Said as news, not as a fault: what ran it
                               ;; stopped on purpose, and this shell carries on.
                               (post-shell-notice
                                program "the organism stopped; this shell started it again"
                                :error-p nil))
                              ((not shutdown-reported)
                               (setf shutdown-reported t)
                               (report-transport-error
                                program :failed "gateway shut down and released ~
the store; not respawning — start another nodecode to take ownership"))))
                           (:dirty-death
                            (when (funcall resurrect failures elapsed)
                              (multiple-value-bind (launched reason)
                                  (cond ((and boot (not pinned))
                                         ;; A shell that starts organisms starts the
                                         ;; replacement too, fronted as the first was
                                         ;; (FRONT-ORGANISM); the dead endpoint goes.
                                         (unless (and takeover (bt2:thread-alive-p takeover))
                                           (setf takeover (funcall start-organism) port nil))
                                         t)
                                        (t (spawn-resurrection-candidate store-path)))
                                (cond (launched
                                       ;; The candidate boots in well under
                                       ;; a second; keep probing at the
                                       ;; base cadence instead of the
                                       ;; outage's accumulated backoff.
                                       (setf delay *sync-backoff-base-seconds*)
                                       (report-transport-error
                                        program :reconnecting "gateway (pid ~a) died ~
holding the store; ~:[spawned a replacement owner~;systemd restarts the gateway~] (attempt ~d/~d)"
                                        owner-pid (eq reason :systemd) resurrect-attempts
                                        *sync-resurrect-budget*))
                                      (t
                                       ;; Nothing launched: exhaust the
                                       ;; budget so this reports once,
                                       ;; while rediscovery keeps watching
                                       ;; for an owner started by hand.
                                       (setf resurrect-attempts
                                             *sync-resurrect-budget*)
                                       (report-transport-error
                                        program :failed "gateway (pid ~a) died ~
holding the store; could not spawn a replacement (~a) — restart ~
nodecode manually"
                                        owner-pid reason)))))))))
                     (sleep delay)
                     ;; Sync backoff_cap_ms 5000, factor 2.
                     (setf delay (min 5.0 (* 2 delay))))))))
      (call-with-profile-switch
       prog
       ;; The switch a /profile picker or `switch NAME' parked is performed
       ;; out here: after the program loop gave the terminal back and after
       ;; the teardown below let go of the organism this shell started.
       (lambda (&aux (quit nil))
         (with-cleanup ((setf stop t) (bt2:signal-semaphore ingress-slots :count 256)
                        (gateway-link-close model.ws)
                        ;; The organism this shell started goes with it, as it did
                        ;; when it ran inside it: join the boot (bounded -- it may
                        ;; still be starting it), then hand the organism the reason
                        ;; and let go of its pipe (STOP-FRONT). A parked switch hands
                        ;; the terminal to another home's organism, so it does not
                        ;; wait for this one to go; it lets go all the same, since an
                        ;; exec would keep the pipe open in the image that replaces
                        ;; this one. A no-op for a shell that attached instead.
                        (dolist (thread (remove nil (list boot-thread takeover)))
                          (nlk::%join-bounded thread 5))
                        (stop-front :why (and quit "quit")
                                    :wait (not nlk:*profile-switch-parked*))
                        (close-shell-presence))
           (call-with-severed-stdio (stdio-log-path store-path)
             (lambda ()
               (call-with-tui-error-capture prog
                 ;; A loop that returns is the operator's quit, which the
                 ;; next boot's note keeps quiet (gateway.lisp BACK-NOTE);
                 ;; a signal or a crash unwinds past it.
                 (lambda () (prog1 (nlt:run prog) (setf quit t)))))))))
      ;; The session the shell ended on, which a /sessions pick, a /fork or
      ;; Left may have moved off the one it launched with: the resume line a
      ;; caller prints names this one. A shell that never attached has none:
      ;; no gateway ever held a session of its to resume.
      (and model.ws model.session-id))))

(defun start-client-organism (&key session-id store-path port)
  "Boot a TUI client against a running Gateway without opening its store."
  ;; PORT NIL resolves from the default store's holder sidecar.
  ;;
  ;; STORE-PATH is rejected because the Gateway owns persistence and this
  ;; process must not acquire a competing SQLite lease.
  (when-let (ended (run-chat-gateway :session-id (or session-id (fresh-session-id)) :port port))
    (format t "~&To resume: (nle:start-client-organism :session-id ~s)~%" ended)))

;;; --- slash-command execution ----------------------------------------------