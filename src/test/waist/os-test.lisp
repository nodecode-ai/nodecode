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
