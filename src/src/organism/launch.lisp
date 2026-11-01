;;;; launch.lisp --- the warm-image launch cache. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; LOAD-CELLS runs at the top of MAIN in every process and costs the
;;;; operator a cold load on every launch: ASDF plans, fasl fop execution,
;;;; a layer's files, definition snapshots. None of that depends on
;;;; anything but bytes on disk — the binary and the folder — so the
;;;; result is cacheable as the strongest cache a Lisp has: a saved image.
;;;; After one launch loads the folder directly, a detached builder loads it
;;;; again in a fresh process and dumps that process as an executable under
;;;; a key derived from every byte the load reads; the next launch stats the
;;;; same bytes, finds the published image, and EXECs it — same argv, same
;;;; environ, same pid — landing in an image whose folder is already
;;;; loaded. A launch that cannot use the cache — no image yet, the folder
;;;; moved, a marker, an exec the kernel refused — loads directly, exactly
;;;; as it always has, and says why in one line when there is anything to
;;;; say. `rm -rf ~/.cache/nodecode/warm` is always safe.
;;;;
;;;; Reliability is the design's spine, in five refusals:
;;;;   - the KEY covers the origin executable, the shipped source tree (the
;;;;     stale-override check digests it), and every visible file under
;;;;     every cell folder, symlinks followed — one stat line each, so an
;;;;     edit, a rewrite (the scribe's rename is a new inode), a retargeted
;;;;     symlink, or an upgraded binary is a different key;
;;;;   - a folder any record of which FAILED to load is never frozen: the
;;;;     dump child refuses, and the direct path stays as loud as today;
;;;;   - a published image is VERIFIED before publish: the builder boots it
;;;;     with the `trial' verb and asks it for its own key;
;;;;   - a published image that dies before MAIN cannot loop: the launcher
;;;;     leaves a boot receipt the warm image deletes once it is up, and a
;;;;     receipt whose pid is gone poisons the key to a `failed' marker;
;;;;   - a `failed' marker stops both exec and rebuild until the key itself
;;;;     changes — fixing the folder is what clears it.
;;;;
;;;; No fork: SBCL refuses to fork a threaded image, and with the folder on
;;;; disk a fresh process IS the composed self. The builder is a fresh `warm'
;;;; process (spawned detached, after the first paint), its dump child a
;;;; fresh `warm-dump' process, the verify runs the new image itself, and
;;;; the only things that cross between them are files and one marked line
;;;; of stdout.

(in-package #:nodecode.kernel)

;;; Non-NIL is what makes a process a warm image: its folder is already
;;; loaded, and BOOT-CELLS loads nothing.
(defvar *image-folder-key* nil
  "The folder key this image was dumped under, or NIL in the origin binary
and every developer image.")

;;; --- boot phases -----------------------------------------------------------
;;; One append-only line per landmark of a launch, epoch-stamped, into the
;;; same NODECODE_PHASE_LOG the provider writes its rounds to: the launch's
;;; own phases — the folder decision, the exec, the store, the listener, the
;;; publish, the peripherals — and the shell's ready line then read off one
;;; file against one clock, and against the terminal's own bytes. Off unless
;;; the environment names a path at boot: a mark costs one GETENV.

(defvar *phase-log-lock* (bt2:make-lock :name "phase-log"))

(defun note-phase (control &rest arguments &aux (path (uiop:getenv "NODECODE_PHASE_LOG")))
  "Append one line to the phase log NODECODE_PHASE_LOG names, when it names
one: the epoch milliseconds, then CONTROL over ARGUMENTS. NIL and one GETENV
otherwise."
  (when (plusp (length path))
    (ignore-errors
      (bt2:with-lock-held (*phase-log-lock*)
        (with-open-file (out path :direction :output :if-exists :append
                                  :if-does-not-exist :create)
          (bind (((_ sec usec) (sb-unix:unix-gettimeofday)))
            (format out "t=~d ~?~%" (+ (* sec 1000) (round usec 1000)) control arguments)))))))

(defun note-boot-phase (label &optional detail)
  "One boot landmark on the phase log (NOTE-PHASE): LABEL, and DETAIL when given."
  (note-phase "boot ~a~@[ ~a~]" label detail))


;;; --- fresh processes of this organism ---------------------------------------

(defvar *organism-command* nil
  "Explicit command list for a fresh process of this organism, or NIL to
run the origin executable. Tests pin it.")

(defun organism-binary-p (&aux (runtime sb-ext:*runtime-pathname*)
                               (core sb-ext:*core-pathname*))
  "Whether this process is the dumped organism executable — the core IS the
runtime file — rather than a developer's SBCL that loaded the system."
  (and runtime core
       (ignore-errors (equal (truename runtime) (truename core)))))

(defun organism-command (verb &rest args)
  "The command that runs VERB (\"trial\", \"warm\", \"warm-dump\") with ARGS in
a fresh process of this organism: *ORGANISM-COMMAND* when pinned, else the
origin executable — a warm launch image spawns the binary it descends from,
never the cache file that may be pruned under it."
  ;; A developer SBCL has no organism to spawn and answers NIL. A warm launch
  ;; image names the binary it descends from (*IMAGE-ORIGIN*, baked at the
  ;; dump) whether or not its own cache file is still there: PRUNE-WARM-CACHE
  ;; keeps the newest two images and, only where /proc lists what runs, a
  ;; live process's own, so elsewhere a long-lived owner's file is routinely
  ;; gone while the process serves on. Without this, a process that cannot
  ;; name its binary would neither build the next launch's image nor run a
  ;; trial child.
  (cond (*organism-command* (append *organism-command* (list verb) args))
        ((or *image-origin* (organism-binary-p))
         (list* (namestring (origin-pathname)) verb args))))

;;; The form cannot forge it: its own output never reaches the real stdout.
(defparameter +score-marker+ "nodecode-trial-score "
  "The prefix of the one stdout line ASK-CHILD reads.")

(defun trial-main (&aux (out *standard-output*) (text (uiop:slurp-stream-string *standard-input*)))
  "The `trial' verb: the folder loaded like every process of this organism,
read one form from stdin, evaluate it with its output silenced, print the
number it answers as the marked last line, exit 0. Any error — a form that
does not read, a result that is not a number — exits 70 with no score line."
  ;; The release gate (pack.sh), the platform smoke lanes (the CI workflows)
  ;; and the warm verify (BUILD-WARM-IMAGE) all ask through it. Never returns.
  ;; Nothing a form does here is recorded: the folder already loaded.
  ;;
  ;; It leaves by _exit, its own output flushed: a warm image answers quickly
  ;; while the launch's SHA256-WARM thread may still be inside OpenSSL setup;
  ;; SBCL's exit unwinds such a thread out of foreign code with a libc lock
  ;; held -- exit(3) then waits on that lock forever in its stdio flush.
  ;; Without _exit, a hung verify would publish nothing, and every later
  ;; launch would boot cold.
  (setf *cells-directory* nil)
  (with-handlers ((error ()
                    (uiop:quit 70 nil)))
    (let ((score (let ((*standard-output* (make-broadcast-stream))
                       (*error-output* (make-broadcast-stream))
                       (*trace-output* (make-broadcast-stream))
                       (*package* (find-package '#:nodecode.evolved)))
                   ;; No #. while the form is read: evaluation is EVAL's job.
                   (eval (with-standard-io-syntax
                           (let ((*package* (find-package '#:nodecode.evolved))
                                 (*read-eval* nil))
                             (read-from-string text)))))))
      (unless (realp score)
        (error "the form answered ~s, which is not a number" (type-of score)))
      (let ((*read-default-float-format* 'double-float))
        (format out "~&~a~s~%" +score-marker+ score))
      (finish-output out)
      (uiop:quit 0 nil))))

(defun ask-child (command form deadline &key environment)
  "Run FORM through the `trial' verb of the process COMMAND starts — this
organism, or an image under test — and answer the number it printed.
ENVIRONMENT, \"NAME=value\" strings, is laid over this process's for it."
  ;; => (values NUMBER-OR-NIL DETAIL). The score line's presence is the whole
  ;; truth: SBCL exits 1 on a memory fault and can exit 0 after a Lisp error,
  ;; so a child that printed no number failed however it ended, and one still
  ;; running at DEADLINE seconds is killed, its whole tree (RUN-BOUNDED). A
  ;; program that cannot start signals.
  (bind (((out _ status)
          (run-bounded command :seconds deadline :input (format nil "~a~%" form)
                               :environment environment)))
    ;; the last marked line that reads as a number; a killed child's is not taken
    (let ((score (unless (eq status :timeout)
                   (loop for line in (reverse (lines out))
                         for value = (and (uiop:string-prefix-p +score-marker+ line)
                                          (ignore-errors
                                           (let ((*read-eval* nil)
                                                 (*read-default-float-format* 'double-float))
                                             (read-from-string
                                              (subseq line (length +score-marker+))))))
                         when (realp value) return value))))
      (values score
              (cond (score "scored")
                    ((eq status :timeout) "killed at deadline: no score")
                    (t (format nil "exited ~a without a score" status)))))))

(defun throwaway-environment (home)
  "The environment of a process whose whole world is HOME: NODECODE_HOME, HOME
and the XDG roots all under it, so it reads none of this home's files and
writes none."
  ;; HOME too: a fresh home imports another harness's credentials from the
  ;; user's own on its first launch, and a scratch NODECODE_HOME alone still
  ;; reads them.
  (let ((root (string-right-trim "/\\" (sb-ext:native-namestring (uiop:ensure-directory-pathname home)))))
    (list (format nil "NODECODE_HOME=~a/" root)
          (format nil "HOME=~a" root)
          (format nil "XDG_CACHE_HOME=~a/cache" root)
          (format nil "XDG_CONFIG_HOME=~a/config" root)
          (format nil "XDG_DATA_HOME=~a/data" root))))

(defun trial (form &key home (seconds 60))
  "Evaluate FORM, a form's text, in a fresh process of this organism -- the
`trial' verb: the folder loaded as every process loads it, the number the
form answers read back (ASK-CHILD). HOME, when given, is a throwaway home the
process runs under whole (THROWAWAY-ENVIRONMENT): it loads HOME's cells/,
reads and writes nothing of this home's, and runs with updates off unless
HOME's own config says otherwise. => (values NUMBER-OR-NIL DETAIL); NIL and
why when this image has no organism to spawn."
  (let ((command (organism-command "trial")))
    (cond ((null command)
           (values nil "no organism binary to spawn: a developer image runs no trial"))
          ((null home) (ask-child command form seconds))
          (t (let ((config (merge-pathnames "config.jsonc" (uiop:ensure-directory-pathname home))))
               (unless (probe-file config)
                 (write-file-atomically config (format nil "{\"update\": {\"mode\": \"off\"}}~%")))
               (ask-child command form seconds :environment (throwaway-environment home)))))))

;;; NIL disables the cache entirely — a developer image (whose runtime is a
;;; bare SBCL that cannot be exec'd back into the organism), the hermetic test
;;; posture, and Windows, where a process cannot replace itself with another
;;; image: every launch there loads the folder directly, the designed
;;; fallback.
(define-startup-parameter *warm-cache-directory*
  (and (organism-binary-p) #+win32 nil #-win32 (cache-path "nodecode/warm/"))
  "Where launch images live: <folder>/<stamp>/<key>/nodecode per published
image, beside its markers, in a part of its own per cell folder.")

;;; --- the key ---------------------------------------------------------------

(defun stat-line-of (label stat)
  "One manifest line for LABEL from an already-taken STAT: the witnesses of
a file's identity and its last change, at the whole-second granularity
sb-posix exposes."
  ;; One stat per file is the walk's whole bill, so the walker hands its own
  ;; stat here rather than paying for a second.
  (format nil "~a ~d ~d ~d ~d" label
          (sb-posix:stat-ino stat) (sb-posix:stat-size stat)
          (sb-posix:stat-mtime stat) (sb-posix:stat-ctime stat)))

(defun map-visible-file-tree (root fn &aux (seen (make-hash-table :test 'equal))
                                            (stat (make-instance 'sb-posix:stat)))
  "Call FN on the namestring of every visible FILE under ROOT and its STAT —
links followed, cycles cut by device:inode — one readdir per directory. The
STAT is refilled for the next file, so FN reads it before it returns."
  ;; The launch-image keeper takes this walk every few seconds for as long as
  ;; a shell lives, so what it conses is what an idle organism conses: through
  ;; SB-EXT:MAP-DIRECTORY, a pathname per entry -- more than the idle collector
  ;; allows a quiet interval. A name string per entry and one STAT refilled is
  ;; the floor.
  (labels ((walk (directory &aux (key (and (ignore-errors (sb-posix:stat directory stat))
                                           (format nil "~d:~d" (sb-posix:stat-dev stat)
                                                   (sb-posix:stat-ino stat)))))
             (when (and key (not (gethash key seen)))
               (setf (gethash key seen) t)
               (let ((handle (sb-posix:opendir directory)))
                 (with-cleanup ((sb-posix:closedir handle))
                   (loop for entry = (sb-posix:readdir handle)
                         until (sb-alien:null-alien entry)
                         do (let ((name (sb-posix:dirent-name entry)))
                              ;; dot entries: . and .., .git, .NAME.tmp
                              (when (and (plusp (length name)) (char/= #\. (char name 0)))
                                (let ((full (concatenate 'string directory name)))
                                  (when (ignore-errors (sb-posix:stat full stat))
                                    ;; The mode test, not S_ISDIR: SBCL's sb-posix
                                    ;; carries the mode constants on every platform
                                    ;; and the S_IS* calls only where there is a
                                    ;; POSIX libc, and this walk is compiled on all
                                    ;; three.
                                    (if (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt)
                                           sb-posix:s-ifdir)
                                        (walk (concatenate 'string full "/"))
                                        (funcall fn full stat))))))))))))
    (walk (namestring (uiop:ensure-directory-pathname root)))))

(defun folder-key (&aux (manifest '()))
  "The launch key: sha256 over a canonical manifest of everything the load
reads — the cell directory's name (it carries $HOME), the origin
executable's identity, the shipped source tree the stale-override check
digests, every visible file under every cell folder but a knowledge cell's,
and the names of the links that resolve to nothing."
  ;; One stat per file, no content read: ~2 ms.
  ;;
  ;; The broken links are in the manifest because they are in the report: a
  ;; link that appeared or was repaired changes what the boot says, and a key
  ;; blind to it would answer from a published image that says the opposite.
  (push (format nil "cells ~a" (namestring *cells-directory*)) manifest)
  (let ((origin (ignore-errors (namestring (truename (origin-pathname))))))
    ;; A file that cannot be stat'ed carries one line less, which is
    ;; itself a difference.
    (push (or (and origin
                   (ignore-errors (stat-line-of (format nil "origin ~a" origin)
                                                (sb-posix:stat origin))))
              "origin unknown")
          manifest))
  (flet ((walk (root &key (start 0) (keep (constantly t)) &aux (lines '()))
           ;; one stat line per file KEEP takes, its path from START on, sorted
           (map-visible-file-tree root (lambda (path stat)
                                         (when (funcall keep path)
                                           (push (stat-line-of (subseq path start) stat) lines))))
           (setf manifest (append (sort lines #'string>) manifest))))
    (let ((root *source-root*))
      (push (format nil "source ~a" (if root (namestring root) "none")) manifest)
      (when root
        (walk root :keep (lambda (path) (uiop:string-suffix-p path ".lisp")))))
    (nlk:note-boot-phase "key-source")
    (dolist (directory (cell-folders))
      (push (format nil "folder ~a ~a" (folder-name directory)
                    (or (ignore-errors (namestring (truename directory))) "?"))
            manifest)
      ;; every visible file, links followed, dot entries (.git, .NAME.tmp) skipped, cycles
      ;; cut -- but a knowledge cell's: nothing in it loads, so a keep moves no key
      (unless (knowledge-folder-p directory)
        (walk directory :start (length (namestring (uiop:ensure-directory-pathname directory)))))))
  (nlk:note-boot-phase "key-folders")
  (dolist (name (broken-cell-links))
    (push (format nil "broken ~a" name) manifest))
  (nlk:note-boot-phase "key-links")
  (let ((text (format nil "~{~a~%~}" (nreverse manifest))))
    (nlk:note-boot-phase "key-formatted")
    (prog1 (subseq (sha256-text text) 7 39)
      (nlk:note-boot-phase "key-hashed"))))

;;; --- the cache layout ------------------------------------------------------

;;; Every home on a machine shares the one cache, and an image answers one
;;; folder only, so each cell folder keeps a part of its own: its images, its
;;; pointer and its markers are never another folder's. With one shared
;;; newest-two prune, six gateways on one box (2026-11-01) deleted each
;;; other's images, and each rebuilt its own every minute, for ever.

(defun folder-cache-name (directory &aux (hash #xcbf29ce484222325))
  "The name of DIRECTORY's part of the cache: FNV-1a over its namestring, 16
hex digits -- the same in every process and every build, and no digest library
woken on a launch's fast path."
  (declare (type (unsigned-byte 64) hash))
  (loop for octet across (sb-ext:string-to-octets (if directory (namestring directory) "")
                                                  :external-format :utf-8)
        do (setf hash (ldb (byte 64 0) (* (logxor hash octet) #x100000001b3))))
  (format nil "~(~16,'0x~)" hash))

(defun warm-folder-directory ()
  "This cell folder's part of the cache: every build's images of it, its
pointers and its markers, apart from every other folder's."
  (merge-pathnames (format nil "~a/" (folder-cache-name *cells-directory*))
                   *warm-cache-directory*))

(defun warm-owner-path (&optional (part (warm-folder-directory)))
  "The file naming the folder PART belongs to, one line: what a prune asks
before it deletes a part that is not its own."
  (merge-pathnames "folder" part))

(defun warm-stamp-directory ()
  "Everything this build caches of this folder — images, markers, receipts,
its pointer."
  (merge-pathnames (format nil "~a/" *image-stamp*) (warm-folder-directory)))

(defun warm-key-directory (key)
  (merge-pathnames (format nil "~a/" key) (warm-stamp-directory)))

(defun warm-image-path (key)
  (merge-pathnames "nodecode" (warm-key-directory key)))

(defun warm-failed-path (key)
  (merge-pathnames "failed" (warm-key-directory key)))

(defun warm-sentinel-path (key)
  (merge-pathnames ".warming" (warm-key-directory key)))

(defun boot-receipt-path (key &optional (pid (sb-posix:getpid)))
  (merge-pathnames (format nil "booting.~d" pid) (warm-key-directory key)))

(defun dead-receipts (directory)
  "The booting.<pid> receipts in DIRECTORY whose pid is gone."
  (loop for path in (directory (make-pathname :name "booting" :type :wild :defaults directory))
        for pid = (parse-integer (or (pathname-type path) "") :junk-allowed t)
        when (and pid (not (pid-live-p pid))) collect path))

(defun image-complete-p (image)
  "An image that ends as SBCL ends an executable core, which it writes last:
the core's offset, then the core magic, which the core also opens with. A
truncated copy — a crashed writer, a full disk — is caught before an exec is
paid for it."
  ;; Not against the origin's size: an image of a folder with no cell in
  ;; it can be a heap page smaller than the executable it was dumped from --
  ;; and every launch of such a home booted cold. Words are little-endian on
  ;; every platform a release is built for.
  (flet ((word (in at &aux (octets (make-array 8 :element-type '(unsigned-byte 8))))
           (and (file-position in at)
                (= 8 (read-sequence octets in))
                (loop for i from 7 downto 0 sum (ash (aref octets i) (* 8 i))))))
    (ignore-errors
     (with-open-file (in image :element-type '(unsigned-byte 8))
       (let ((length (file-length in)))
         (and (> length 16)
              (eql #x5342434C (word in (- length 8))) ; "SBCL", CORE_MAGIC
              (let ((core (word in (- length 16))))
                (and (< core (- length 16))
                     (eql #x5342434C (word in core))))))))))

(defun mark-warm-failed (key reason)
  "Write KEY's `failed' marker: no exec and no rebuild for this key until
the folder itself changes — which computes a different key and starts
clean."
  (ignore-errors
   (with-output-file (out (warm-failed-path key))
     (format out "warm launch cache: ~a~%" reason)))
  nil)

;;; --- exec ------------------------------------------------------------------

#-win32
(defun swap-signal-mask (mask)
  "Set this thread's signal mask to MASK, a sigset_t's octets, and return the
one it replaces; a NIL MASK only reads it."
  ;; 128 octets: glibc's sigset_t, and more than any other libc's.
  (let ((old (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (mask old)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien
        "sigprocmask"
        (function sb-alien:int sb-alien:int
                  sb-alien:system-area-pointer
                  sb-alien:system-area-pointer))
       #+linux 2 #-linux 3              ; SIG_SETMASK
       (if mask (sb-sys:vector-sap mask) (sb-sys:int-sap 0))
       (sb-sys:vector-sap old)))
    old))

#-win32
(defun silence-alarm ()
  "Zero the kernel's alarm (ITIMER_REAL) and ignore SIGALRM, which discards
one already raised. The caller holds SBCL's scheduler lock, so no thread
re-arms it in between."
  (sb-unix:unix-setitimer :real 0 0 0 0)
  (sb-sys:enable-interrupt sb-unix:sigalrm :ignore))

#-win32
(defun call-as-last-act (thunk)
  "Call THUNK -- an exec -- with nothing left in this process to end its
successor: no collection starting, no alarm armed or pending, no signal
blocked. A THUNK that returns was refused by the kernel, and the process is
given back as it was. => THUNK's value."
  ;; A mask survives exec, and an inherited block would gag the image on
  ;; the far side.
  ;;
  ;; The runtime's GC lock is held across the exec: no collection starts
  ;; while the kernel replaces the process. SBCL stops every thread for a
  ;; collection with SIGUSR2; without the lock, one that reaches this
  ;; thread inside execve would stay pending through it, and the successor
  ;; -- its handlers reset to the default, which for SIGUSR2 is to
  ;; terminate -- would die of it before its first line. A collection
  ;; already under way stops this thread as usual, and the lock is taken
  ;; once it is done.
  ;;
  ;; And the alarm is zeroed: ITIMER_REAL survives execve too, and SIGALRM's
  ;; default is to terminate. MAIN arms the idle collector's 3 s timer before
  ;; a launch execs its published image, which arms its own before a stale
  ;; one hands back. Without the zero, a successor that has not yet blocked
  ;; its signals would die with exit 142 when the inherited alarm fires.
  ;;
  ;; Only the kernel's alarm crosses: SBCL's schedule is memory the exec
  ;; drops with the rest, so it stays whole, and a refusal re-arms from it.
  ;; That matters: a refused process keeps running (a refused image is a
  ;; cold boot in this very process), and would lose its idle collector, a
  ;; turn's collection timer and the slack walk for good. The scheduler's
  ;; lock is held throughout, so no thread re-arms the alarm before the
  ;; exec -- taken before the GC lock, never after: a thread inside it can be
  ;; waiting on a timer callback that collects, and no collection starts
  ;; while the GC lock is held. A SIGALRM raised but not yet taken would
  ;; cross as well; SIG_IGN discards it, and SBCL's handler goes straight
  ;; back.
  ;;
  ;; The mask is emptied last: a signal this thread takes inside the lock is
  ;; deferred, and SBCL blocks every deferrable one until the lock is left.
  ;; A refusal gets its mask back joined with any such block set since, which
  ;; the deferred handler lifts as the lock is left.
  (sb-int:with-system-mutex (sb-impl::*scheduler-lock*)
    (loop until (eql 1 (sb-alien:alien-funcall
                        (sb-alien:extern-alien "try_acquire_gc_lock" (function sb-alien:int))))
          do (sleep 0.001))
    (silence-alarm)
    (sb-sys:enable-interrupt sb-unix:sigalrm #'sb-unix::sigalrm-handler)
    (let ((mask (swap-signal-mask (make-array 128 :element-type '(unsigned-byte 8)
                                                  :initial-element 0))))
      (multiple-value-prog1 (funcall thunk)
        (swap-signal-mask (map-into mask #'logior mask (swap-signal-mask nil)))
        (sb-impl::set-system-timer)
        (sb-alien:alien-funcall (sb-alien:extern-alien "release_gc_lock" (function sb-alien:int)))))))

(defun exec-program (path argv)
  "Replace this process with PATH running ARGV — argv[0] included; environ,
pid, open fds and job control carry over."
  ;; On success never returns; answers the kernel's refusal as text. The
  ;; exec is the process's last act (CALL-AS-LAST-ACT): nothing it leaves
  ;; set can end the successor. Windows has no exec, and no cache to exec
  ;; from: the refusal is the answer.
  #+win32 (declare (ignore argv))
  (mapc #'finish-output (list *standard-output* *error-output*))
  #+win32 (format nil "exec ~a: not on Windows" (namestring path))
  #-win32
  (let ((file (namestring path)))
    (with-c-string-vector (vector argv)
      ;; Still here after the call: the kernel refused. Errno first — giving
      ;; the process back and freeing the vector would clobber it. The
      ;; successor gets the process's own stdio, never a TUI's severed set.
      (let ((errno (call-with-unsevered-stdio
                    (lambda ()
                      (call-as-last-act
                       (lambda ()
                         (sb-alien:alien-funcall
                          (sb-alien:extern-alien "execv"
                                                 (function sb-alien:int sb-alien:c-string
                                                           sb-alien:system-area-pointer))
                          file vector)
                         (sb-alien::get-errno)))))))
        (format nil "execv ~a: ~a" file (sb-int:strerror errno))))))

(defun warm-pointer-path ()
  "The file naming this build's published image: written when one is
published, read before anything else is walked on a launch."
  (merge-pathnames "current" (warm-stamp-directory)))

(defun read-warm-pointer (&aux (text (lines (read-text (warm-pointer-path)))))
  "=> the image path and the folder key the pointer names, or (values NIL
NIL) when this build has published nothing."
  (values (and (plusp (length (first text))) (first text)) (second text)))

(defun warm-the-digest ()
  "Start OpenSSL's own setup on a thread: the first SHA-256 in a fresh
process costs CPU, dead time a key walk's remaining
reads hide behind."
  (spawn "sha256-warm" (ignore-errors (sha256-text "warm"))))

(defvar *warm-verification* nil
  "The thread checking a receipt-less image's folder key, or NIL.")

(defvar *warm-verification-failure* nil
  "Why that check failed, or NIL while it has not.")

(defun await-warm-verification ()
  "Wait for a warm image's folder verification — taken while the store was
being opened — and serve on its answer: a folder that does not answer this
image's key hands the process back to the origin binary (which never
returns). NIL when this image has nothing to verify."
  (when *warm-verification*
    (%join-bounded *warm-verification* 30)
    (nlk:note-boot-phase "warm-verified")
    (when *warm-verification-failure*
      (hand-back-to-origin *warm-verification-failure*))
    (setf *warm-verification* nil))
  t)

(defun hand-back-to-origin (reason)
  "Leave a stale image: forget the pointer that named it, say REASON on the
log, and exec the origin binary — which loads the folder as it is now, or
builds the image that matches it."
  ;; Returns only when the kernel refused the exec, and that refusal is an
  ;; error: a stale image must not serve.
  (format *error-output* "~&warm: ~a; handing this process to ~a~%"
          reason (origin-pathname))
  (ignore-errors (delete-file (warm-pointer-path)))
  ;; An exec keeps open file descriptors: the store's lease would ride into
  ;; the successor and deadlock it against itself — the profile switch's
  ;; rule, the same way.
  (ignore-errors (nlk:close-store))
  (error "a stale warm image could not hand back to ~a: ~a"
         (origin-pathname) (exec-program (origin-pathname) sb-ext:*posix-argv*)))

(defun hand-over-to-published-image ()
  "The launch's fast path, tried before anything paints: follow the pointer
the builder left for this build and exec the image it names."
  ;; Nothing is walked and nothing is vouched for here — the pointer names the
  ;; image published last, not the folder's state — because the image keys the
  ;; folder itself as it boots (BOOT-CELLS' no-receipt path) and hands the
  ;; process back to this binary when the folder moved since it was dumped. A
  ;; stale pointer costs one exec, never a wrong boot.
  ;;
  ;; => never when an image answers (the exec replaces this process); else text
  ;; saying why not, with nothing changed.
  ;; A warm image never follows the pointer: it IS an image, and an image
  ;; following one would hand the process to itself for ever. And a build that
  ;; keeps no images -- Windows, a developer image -- has no pointer to read:
  ;; merging one under a NIL cache would break every bare `nodecode' there.
  (if *pristine*
      "pristine: generation 0, no image followed"
   (when (and (null *image-folder-key*) *warm-cache-directory*)
   (multiple-value-bind (image key) (read-warm-pointer)
    (cond
      ((null key) "this build has published no image pointer")
      ((not (probe-file image))
       (format nil "the image the pointer names is gone (~a)" key))
      ((probe-file (warm-failed-path key))
       (format nil "key ~a is marked failed" key))
      ((not (image-complete-p image))
       (format nil "the image for key ~a is incomplete" key))
      ((directory (make-pathname :name "booting" :type :wild
                                 :defaults (warm-key-directory key)))
       ;; A boot receipt still on disk: a sibling launch is on its way in.
       ;; Left to the painted path, which knows how to read the pids.
       "a sibling launch is booting this key")
      (t
       (nlk:note-boot-phase "exec-published-image")
       (format nil "the kernel refused the exec: ~a"
               (exec-program image sb-ext:*posix-argv*))))))))

(defun hand-over-to-image (image &optional (argv sb-ext:*posix-argv*))
  "Replace this process with the published IMAGE its launcher has just
painted over."
  ;; The word in the environment is the one a profile switch's successor reads
  ;; (profile.lisp, +HANDOVER-VARIABLE+): a live screen is waiting for it, so
  ;; its first frame paints over this frame instead of entering the alternate
  ;; screen again and blanking it. Answers the kernel's refusal, and only then
  ;; — on success this process is the image.
  ;;
  ;; The launcher reaches this from its boot thread, behind the frame it has
  ;; already painted: the folder decision (an exec, or a load) is the one
  ;; thing that used to stand between the operator and a first frame.
  (sb-posix:setenv +handover-variable+ "1" 1)
  (exec-program image argv))


(defun exec-profile-switch (name binary argv)
  "Perform a PROFILE-SWITCH plan here and now: the caller has no terminal
state to give back, so nothing is deferred."
  ;; On success this process is gone; a kernel refusal comes back as the text
  ;; the caller shows.
  ;; What this image leaves in the environment is what the successor
  ;; inherits: a live terminal. It is the one thing an exec carries that
  ;; says so (the environment), and the successor must know, because a
  ;; screen already painted is not one to enter and clear again.
  (or (hand-over-to-image binary argv) (format nil "switched to ~a" name)))

;; The default performer, so a command line, a channel lane and the model's
;; eval all reach the exec through the one call shape that owns it. A TUI
;; shell rebinds this for the length of its run: it has a terminal to
;; restore and a store to close first (tui/entry.lisp
;; CALL-WITH-PROFILE-SWITCH).
(setf *profile-switch-performer* #'exec-profile-switch)

;;; --- the boot entry --------------------------------------------------------

(defun fresh-sentinel-p (key &aux (date (ignore-errors (file-write-date (warm-sentinel-path key)))))
  (and date (< (- (get-universal-time) date) 300)))

(defparameter *warm-spawn-delay* 1
  "Seconds between boot and the builder spawn, so the detached process's
start lands behind the first paint, never inside it. Tests bind 0.")

(defun spawn-warm-builder (key)
  "One detached `warm' supervisor for KEY, *WARM-SPAWN-DELAY* from now —
unless an image, a fresh sentinel, or a failed marker already answers."
  ;; => the spawning thread, or NIL. The marker's reason is a boot phase, a
  ;; debug fact: the operator can do nothing with it, and the launch works
  ;; without an image. Said on *ERROR-OUTPUT* it was a toast over the first
  ;; frame -- a shell's keeper thread says it on the shell -- `warm
  ;; launch cache: the dump child exited 1' at every look. The marker and the
  ;; key's warm.log keep it.
  (cond ((probe-file (warm-image-path key)) nil)
        ((probe-file (warm-failed-path key))
         (nlk:note-boot-phase "warm-refused"
                              (ignore-errors (uiop:read-file-line (warm-failed-path key))))
         nil)
        ((fresh-sentinel-p key) nil)
        (t
         ;; Everything the thread needs is captured here, on the calling
         ;; thread: the specials behind these paths are startup parameters
         ;; a test LETs, and a LET is invisible from a fresh thread.
         (let ((log (merge-pathnames "warm.log" (warm-key-directory key)))
               (command (organism-command "warm"))
               (delay *warm-spawn-delay*))
           ;; A developer image has no organism to spawn.
           (when command
             (spawn "warm-spawn"
               (sleep delay)
               (ignore-errors
                (ensure-directories-exist log)
                (spawn-detached command log))))))))

;;; --- the keeper ------------------------------------------------------------

;;; A published image answers one folder and only that folder: its key moves
;;; with every file the load reads, and a launch that meets a moved folder
;;; would pay the whole load in front of its first frame. The launcher itself
;;; can only ever be the one that pays; the paying moves here instead, to a
;;; process that is going to live a while: it looks at the folder every few
;;; seconds and asks for the image the next launch will exec. The looking loads
;;; nothing (FOLDER-KEY is one stat per file), and the build it asks for runs
;;; in a child, one at a time (the sentinel), after the same delay a cold
;;; boot's builder waits.

(defparameter *launch-image-period* 5
  "Seconds between one look at the folder by the launch-image keeper and the
next, after a look that found it moved. A launch that lands inside the window —
this period, plus the build behind it — still loads the folder itself; a later
one execs the published image.")

;;; Each look walks every file of the folder and the source tree, and a
;;; process lives far longer quiet than editing: a look that finds the folder
;;; where the last one left it waits twice as long for the next, up to this.
(defparameter *launch-image-idle-period* 60
  "The longest the keeper waits between looks at a folder that has not moved.")

(defvar *launch-image-keeper* nil
  "This process's keeper thread, or NIL when it keeps none.")

(defun folder-stamp (&aux (count 0) (names 0) (sizes 0) (times 0) (inodes 0))
  "What FOLDER-KEY reads, witnessed without its manifest: sums over the stat of
the origin, of the cell directory itself, and of every visible file under the
source root and under the cell directory. A folder whose stamp has not moved
has not moved its key."
  ;; The keeper looks every *LAUNCH-IMAGE-PERIOD* for as long as a process
  ;; lives. Without the stamp, the key -- CELL-FOLDERS' pathname globs, a
  ;; formatted line per file, the text they make, its digest -- would cons
  ;; enough on each look that the idle collector would never see consecutive
  ;; quiet checks. The stamp is two walks. It covers what the key covers and
  ;; more: every file under the source root, not only its .lisp, and every
  ;; folder under the cell directory, .asd or not; the directory's own stat
  ;; moves with an entry added or removed there, a link to nothing included.
  ;; Its granularity is the key's own whole second.
  (flet ((note (path stat)
           (setf count (1+ count)
                 names (logand most-positive-fixnum (+ names (sxhash path)))
                 sizes (+ sizes (sb-posix:stat-size stat))
                 times (+ times (sb-posix:stat-mtime stat) (sb-posix:stat-ctime stat))
                 inodes (+ inodes (sb-posix:stat-ino stat)))))
    (dolist (path (list (ignore-errors (namestring (truename (origin-pathname))))
                        (namestring *cells-directory*)))
      (when-let (stat (and path (ignore-errors (sb-posix:stat path))))
        (note path stat)))
    (when *source-root*
      (map-visible-file-tree *source-root* #'note))
    (map-visible-file-tree *cells-directory* #'note)
    (list count names sizes times inodes)))

(defvar *launch-image-stamp* nil
  "(STAMP . KEY): the FOLDER-STAMP and FOLDER-KEY of the keeper's last key.")

(defvar *proved-key* nil
  "The key the keeper last handed the prover: one key is judged once a process.")

(defun tend-launch-image ()
  "One look at the folder on disk: a key the folder moved to is proved and
sealed first (generation.lisp PROVE-SELF); then, when no published image
answers it and it proved, one is asked for. => the key a build was asked for,
else NIL."
  ;; The key is taken again only when the stamp has moved, and proved once per
  ;; key; whether its image exists is asked at every look, so a build that
  ;; died unpublished is asked for again as before. The prove-and-seal half
  ;; runs wherever a folder loads -- a developer image and Windows seal
  ;; without a cache -- and only the build half needs one.
  (when-let (key (and *cells-directory* (not *pristine*)
                      (let ((stamp (ignore-errors (folder-stamp)))
                            (last *launch-image-stamp*))
                        (if (and stamp last (equal stamp (car last)))
                            (cdr last)
                            (let ((key (ignore-errors (folder-key))))
                              (setf *launch-image-stamp* (and stamp key (cons stamp key)))
                              key)))))
    (unless (equal key *proved-key*)
      (setf *proved-key* key)
      (when (eq :red (ignore-errors (prove-self key)))
        ;; No image stands for a self that does not prove: a launch loads the
        ;; folder cold, and the boot line says it is unsealed.
        (when *warm-cache-directory*
          (ignore-errors (delete-file (warm-image-path key)))
          (mark-warm-failed key "the folder did not prove; (generations) says why"))))
    (when (and *warm-cache-directory*
               (not (probe-file (warm-image-path key)))
               (not (refused-key-p key)))
      (spawn-warm-builder key)
      key)))

(defun start-launch-image-keeper ()
  "Keep this home's launch image in step with the folder, on a thread of its
own, for as long as this process lives. Idempotent per process."
  (or *launch-image-keeper*
      (setf *launch-image-keeper*
            (spawn "launch-image-keeper"
              (let ((period *launch-image-period*))
                (loop (sleep period)
                      (let ((seen *launch-image-stamp*))
                        (ignore-errors (tend-launch-image))
                        (setf period (if (eq seen *launch-image-stamp*)
                                         (min *launch-image-idle-period* (* 2 period))
                                         *launch-image-period*)))))))))

(defun boot-cells (&key spawn hand-over receipt)
  "Load the folder — through the warm-image cache when a published image
matches its current key — and answer the records. Under RECEIPT (a serve)
this process's receipt is left before the load, for the launch after to find
if this one dies before it serves."
  ;; MAIN's one boot entry.
  ;;
  ;; A fourth state first: the floor. *PRISTINE* (by hand, or a self that
  ;; died before serving: generation.lisp DECIDE-PRISTINE) loads nothing,
  ;; follows no image and spawns no builder; the boot line says so, and
  ;; (generations) says why.
  ;;
  ;; Three states. A WARM IMAGE (*IMAGE-FOLDER-KEY* is the key it was dumped
  ;; under) already holds the loaded folder: it deletes the boot receipt its
  ;; launcher wrote, says so once when the folder has moved since the dump, and
  ;; loads nothing. A LAUNCHER with a published image for the current key hands
  ;; the whole process over with one exec — same argv, same environ, same pid —
  ;; or, when the caller passes HAND-OVER, asks it to make that exec (a TUI
  ;; launcher does: it paints first and decides from behind its frame).
  ;; Everything else — no cache, no image yet, a marker, a poisoned receipt, an
  ;; exec the kernel refused, a HAND-OVER that could not make it — loads
  ;; directly, exactly as LOAD-CELLS always has, and (under SPAWN: the tui and
  ;; serve entries, never a child) sends one detached builder after the first
  ;; paint so the next launch hits. A build that has cached nothing here yet
  ;; (no stamp directory) has no image to find, so it walks no key in front of
  ;; its load. Only a launcher refreshes the folder first
  ;; (REFRESH-INSTALLED-CELLS): a warm image holds one its launcher already
  ;; refreshed.
  (cond
    (*pristine*
     (nlk:note-boot-phase "pristine")
     (setf *cells* '())
     (format *error-output* "~&pristine: generation 0, no folder loaded~@[ -- ~a~]~%"
             (ignore-errors (pristine-reason)))
     *cells*)
    (*image-folder-key*
     (nlk:note-boot-phase "warm-image")
     ;; The launcher that exec'd this image wrote its receipt under THIS key:
     ;; the walk this process would repeat already ran, in this pid, and
     ;; answered the key of the folder on disk — so the folder is current by
     ;; the launcher's own word, and the walk is skipped. A direct launch of
     ;; the cache file has no receipt and still checks, and still says so
     ;; when the folder has moved.
     (cond
       ((probe-file (boot-receipt-path *image-folder-key*))
        (ignore-errors (delete-file (boot-receipt-path *image-folder-key*)))
        (nlk:note-boot-phase "warm-receipted"))
       ;; No receipt vouched for this image (a pointer launch writes
       ;; none), so it vouches for itself: the folder is keyed and
       ;; compared. A folder that moved is a stale image, and a stale
       ;; image does not serve — it hands the process to the origin
       ;; binary, which loads the folder as it is now.
       (t (warm-the-digest)
          ;; The folder's answer is taken on a thread and awaited before
          ;; this image SERVES (AWAIT-WARM-VERIFICATION, the gateway start),
          ;; not before it takes the store: the store's open and the walk
          ;; overlap, and a stale image hands the process back with the
          ;; store closed behind it.
          (setf *warm-verification*
                (spawn "warm-verify"
                  (let ((current (and *cells-directory* *warm-cache-directory*
                                      (ignore-errors (folder-key)))))
                    (nlk:note-boot-phase "warm-key-checked")
                    (unless (and current (equal current *image-folder-key*))
                      (setf *warm-verification-failure*
                            (format nil "the cell folder does not answer ~
                                          key ~a (it answers ~a)"
                                    *image-folder-key* (or current "nothing")))))))
          (nlk:note-boot-phase "warm-verifying")))
     ;; The board's notice is not on the ready line's path: synchronized from
     ;; a thread of its own.
     (spawn "shipped-cell-notice" (ignore-errors (refresh-shipped-cell-notice)))
     *cells*)
    (t
     ;; Before the key reads the folder: an install another image made — a
     ;; link into the release installed before this one, a copy another
     ;; build left — points back at this image's own folder, so the key
     ;; digests and the load compiles the cells this core shipped with.
     (warm-the-digest)
     (nlk:note-boot-phase "refresh-start")
     (multiple-value-bind (refreshed failed retired)
         (handler-case (refresh-installed-cells) (error () (values nil nil nil)))
       (nlk:note-boot-phase "refresh-done")
       (format *error-output* "~@[~&cells: ~{~a~^, ~} now load from this build~%~]" refreshed)
       (format *error-output* "~@[~&cells: ~{~a~^, ~} taken out: this build no longer ships ~
                               ~:[it~;them~]~%~]"
               retired (rest retired))
       (loop for (name . text) in failed
             do (format *error-output* "~&cells: ~a still loads from another build: ~a~%"
                        name text)))
     ;; A hub clone of a folder this build ships gives way to the build's own:
     ;; a release never touches the home, so the clone would keep running the
     ;; code it was cloned at (nodecode #112). No index is read here, so a boot
     ;; never waits on the network; the update check moves the rest.
     (multiple-value-bind (moved failed)
         (handler-case (refresh-hub-clones) (error () (values nil nil)))
       (format *error-output* "~@[~&cells: ~{~a~^, ~} now load from this build in place of the hub clone~%~]" moved)
       (loop for (name . text) in failed
             do (format *error-output* "~&cells: ~a still loads from its hub clone: ~a~%" name text)))
     (let* ((warm (and *cells-directory* *warm-cache-directory*))
            ;; Every image, marker and receipt of this build lives under its
            ;; stamp directory: without one, the walk decides nothing.
            (cached (and warm (probe-file (warm-stamp-directory))))
            (key (and cached (ignore-errors (folder-key))))
            (image (and key (warm-image-path key)))
            ;; The builder's key is then taken beside the load (which writes
            ;; caches, not the folders the key stats), specials captured: a
            ;; test LETs them. A child owes no builder and leaves no thread:
            ;; the layer-compile trial forks after its boot, and
            ;; SB-POSIX:FORK refuses while another thread runs.
            (keying (and warm spawn (not cached)
                         (let ((directory *cells-directory*) (root *source-root*))
                           (spawn "warm-key"
                             (let ((*cells-directory* directory) (*source-root* root))
                               (ignore-errors (folder-key))))))))
       (when (and image
                  (probe-file image)
                  (not (probe-file (warm-failed-path key)))
                  (image-complete-p image)
                  ;; A receipt with a dead pid: the image died before MAIN. That poisons the key.
                  (let ((dead (dead-receipts (warm-key-directory key))))
                    (when dead
                      (mark-warm-failed key "published image died before boot")
                      (dolist (path dead) (ignore-errors (delete-file path))))
                    (null dead)))
         (ignore-errors
          (with-output-file (out (boot-receipt-path key))
            (format out "~d~%" (get-universal-time))))
         (nlk:note-boot-phase "exec-published-image")
         (let ((refusal (if hand-over
                            (funcall hand-over image)
                            (exec-program image sb-ext:*posix-argv*))))
           ;; Still here: the exec did not happen — the kernel refused it, or
           ;; the caller's HAND-OVER could not make it (it answers only when
           ;; it could not; on success this process is already the image).
           ;; The receipt is ours — delete it before it reads as an image
           ;; that died.
           (ignore-errors (delete-file (boot-receipt-path key)))
           (format *error-output* "~&warm: ~a; loading directly~%" refusal)))
       (nlk:note-boot-phase "load-direct")
       (when receipt (write-self-receipt key))
       (load-cells)
       (nlk:note-boot-phase "folder-loaded")
       (refresh-shipped-cell-notice)
       ;; Bounded like AWAIT-WARM-VERIFICATION: a key that failed or ran over
       ;; is no builder this launch, and the keeper asks again in 5 s.
       (when keying
         (setf key (let ((answer (%join-bounded keying 30))) (and (stringp answer) answer))))
       (when (and key spawn) (spawn-warm-builder key))
       *cells*))))

;;; --- the builder: two verbs, files between them ----------------------------

(defparameter *warm-abandoned-age* (* 24 3600)
  "Seconds a part of the cache no folder owns sits still before a prune
deletes it: a part being set up, or an older build at work in the layout
before parts, has moved within it.")

(defun newest-write-under (directory)
  "The latest write date of DIRECTORY and of every file and directory under it."
  (let ((newest (or (ignore-errors (file-write-date directory)) 0)))
    (uiop:collect-sub*directories
     directory t t
     (lambda (sub)
       (dolist (path (cons sub (ignore-errors (uiop:directory-files sub))))
         (setf newest (max newest (or (ignore-errors (file-write-date path)) 0))))))
    newest))

(defun abandoned-warm-part-p (part running)
  "Whether nobody comes back for PART, a part of the cache: the folder its
owner file names is gone -- or it names none, as the stamps the layout before
parts left at the top -- no live process runs an image in it, and nothing in
it has moved for *WARM-ABANDONED-AGE*."
  (let ((owner (ignore-errors (uiop:read-file-line (warm-owner-path part)))))
    (and (not (and (plusp (length owner)) (probe-file owner)))
         (notany (lambda (exe) (uiop:string-prefix-p (namestring part) exe)) running)
         (< (newest-write-under part) (- (get-universal-time) *warm-abandoned-age*)))))

(defun prune-warm-cache (&aux (running (running-executables))
                              (own (folder-cache-name *cells-directory*))
                              (root (namestring (or (ignore-errors (truename *warm-cache-directory*))
                                                    *warm-cache-directory*))))
  "Keep this folder's newest two published images across every stamp — one
folder runs under two organism binaries at most, a release and a developer
build — and every image a live process runs, and delete the rest of its key
directories whole. Then delete every other part of the cache nobody comes
back for (ABANDONED-WARM-PART-P)."
  ;; Without this, an image deleted under its process would strand it: the
  ;; process would serve on, but every fresh child it execs from that file
  ;; would fail.
  ;; The kernel's list is the authority, not a receipt the image writes, so
  ;; an organism of any build is kept, the one already running included.
  ;; DIRECTORY answers truenames, as the kernel's list does, so a cache
  ;; reached through a link is checked against where it really lies.
  (flet ((delete-part (directory)
           (ignore-errors
            (uiop:delete-directory-tree
             directory
             :validate (lambda (path) (uiop:string-prefix-p root (namestring path)))))))
    (let* ((images (sort (directory (merge-pathnames "*/*/nodecode" (warm-folder-directory)))
                         #'> :key (lambda (path)
                                    (or (ignore-errors (file-write-date path)) 0))))
           (stale (remove-if (lambda (image)
                               (member (namestring image) running :test #'string=))
                             (nthcdr 2 images))))
      (dolist (image stale)
        (delete-part (uiop:pathname-directory-pathname image))))
    ;; Another folder's part is that folder's own to prune; it goes here only
    ;; once nobody comes back for it -- a profile deleted, or a stamp of the
    ;; layout before parts that nothing runs any more.
    (dolist (part (ignore-errors (directory (merge-pathnames "*/" *warm-cache-directory*))))
      (unless (equal own (car (last (pathname-directory part))))
        (when (abandoned-warm-part-p part running)
          (delete-part part))))))

(defun build-warm-image (key &aux (directory (warm-key-directory key))
                                  (tmp (merge-pathnames (format nil "nodecode.tmp-~d"
                                                                (sb-posix:getpid))
                                                        directory)))
  "Dump, verify, publish KEY's image; on any refusal write the marker and
say so on stdout — the builder's log is the story, its exit is always 0."
  (ensure-directories-exist tmp)
  (dolist (path (directory (make-pathname :name "nodecode" :type :wild
                                          :defaults directory)))
    (when (and (uiop:string-prefix-p "tmp-" (or (pathname-type path) ""))
               (> (- (get-universal-time) (or (ignore-errors (file-write-date path)) 0)) 3600))
      (ignore-errors (delete-file path))))
  (flet ((fail (reason)
           (format t "~&warm: ~a~%" reason)
           (mark-warm-failed key reason)
           (ignore-errors (delete-file tmp))
           (return-from build-warm-image nil)))
    (bind (((_ errors status)
            (with-handlers ((error (condition)
                              (fail (format nil "dump child: ~a" condition))))
              (run-bounded (organism-command "warm-dump" (namestring tmp)) :seconds 120))))
      (when (eq status :timeout) (fail "the dump child ran over its deadline"))
      (unless (eql status 0)
        ;; what the child said last names what an exit status cannot: the log's
        (dolist (line (last (lines errors) 12)) (format t "~&~a~%" line))
        (fail (format nil "the dump child exited ~a" status))))
    (unless (and (probe-file tmp) (image-complete-p tmp))
      (fail "the dump child left no whole image"))
    ;; The strongest gate: boot the image itself and ask it for its own
    ;; key. A wrong answer, a crash, or a hang is a scoreless outcome, and
    ;; nothing scoreless publishes.
    (multiple-value-bind (score detail)
        (ask-child (list (namestring tmp) "trial")
                   (format nil "(if (equal nodecode.kernel::*image-folder-key* ~s) 1 0)" key)
                   30.0)
      (unless (eql 1 score)
        (fail (format nil "verify: ~a" detail))))
    (with-open-file (in tmp :element-type '(unsigned-byte 8))
      (flush-file in))
    ;; The atomic publish this design leans on (REPLACE-FILE: rename(2),
    ;; never CL's type-merging RENAME-FILE).
    (replace-file tmp (warm-image-path key))
    ;; the part's owner, which a prune for another folder asks before it
    ;; deletes this part (ABANDONED-WARM-PART-P)
    (write-file-atomically (warm-owner-path)
                           (format nil "~a~%" (namestring *cells-directory*)))
    ;; name the image as this build's published one, atomically: a launch
    ;; follows the pointer before it walks anything
    (write-file-atomically (warm-pointer-path)
                           (format nil "~a~%~a~%" (namestring (warm-image-path key)) key))
    (prune-warm-cache)
    (format t "~&warm: published ~a~%" key)))

(defun run-warm-supervisor (&aux (key (and *cells-directory* *warm-cache-directory*
                                           (ignore-errors (folder-key)))))
  "Build and publish the current key's image unless one exists, a fresh
sentinel says another builder is at it, or a marker says not to."
  ;; The supervisor loads no cells itself; the dump child does that.
  (cond ((null key)
         (format t "~&warm: no folder or no cache; nothing to build~%"))
        ((probe-file (warm-image-path key)))
        ((probe-file (warm-failed-path key))
         ;; A marker answers for this key: the image is refused until the
         ;; folder changes, and the marker holds the reason. Said here
         ;; because a caller waiting on this build must read a refusal,
         ;; not the silence that means `an image was already there'.
         (format t "~&warm: no image for ~a -- ~a~%"
                 key (or (ignore-errors (uiop:read-file-line (warm-failed-path key)))
                         "no reason recorded")))
        ;; the sentinel FRESH-SENTINEL-P reads: stale at 300 s, as there
        ((not (with-handlers ((error () nil))
                (claim-file (ensure-directories-exist (warm-sentinel-path key)) 300)))
         (format t "~&warm: another builder holds ~a~%" key))
        (t
         (unwind-protect (build-warm-image key)
           (ignore-errors (delete-file (warm-sentinel-path key)))))))

(defun warm-main ()
  "The `warm' verb: the detached supervisor."
  ;; Exits 0 whatever happened — the log is the story. Never returns.
  (run-warm-supervisor)
  (uiop:quit 0))

(defun disarm-timers ()
  "Unschedule every SBCL timer in this process, silence its alarm for good --
the kernel's timer zeroed and SIGALRM ignored, one already raised with it --
and wait out a callback still on its thread. For a process whose next act is
its dump."
  ;; Without this, a timer's SIGALRM landing inside GC-AND-SAVE would be
  ;; fatal to the runtime, and a core save has no business racing a
  ;; recurring callback. The dumped image arms its own timers at every start
  ;; (MAIN's COLLECT-WHILE-IDLE), and its runtime installs its own SIGALRM
  ;; handler, so nothing is lost by handing it a timerless process.
  ;;
  ;; Unscheduling alone would not be enough, in two ways: an alarm the
  ;; kernel has armed or already raised could still land; and a callback
  ;; runs on a thread of its own, `Timer NAME', which SAVE-LISP-AND-DIE
  ;; would refuse to save beside -- a repeating timer mid-callback is not
  ;; even on the list, and schedules itself again as the callback ends.
  ;; The scheduler's lock is held, so no thread re-arms the alarm in between.
  (flet ((unschedule-all ()
           (dolist (timer (sb-ext:list-all-timers))
             (ignore-errors (sb-ext:unschedule-timer timer)))))
    (unschedule-all)
    #-win32
    (sb-int:with-system-mutex (sb-impl::*scheduler-lock*)
      (silence-alarm))
    (dolist (thread (sb-thread:list-all-threads))
      (when (and (not (eq thread sb-thread:*current-thread*))
                 (uiop:string-prefix-p "Timer " (or (sb-thread:thread-name thread) "")))
        (sb-thread:join-thread thread :default nil :timeout 10)))
    (unschedule-all))
  ;; A nursery a load raised comes back on a timer (WITH-NURSERY); unscheduled,
  ;; the image would carry the raise and the floor it found into every
  ;; process it starts.
  (restore-nursery))

(defun warm-dump-records (&optional (stream *error-output*))
  "The records a dump freezes: the folder loaded directly, never through the
cache, with one line for the failures that will ride along in the image."
  ;; A folder whose load reported failures is frozen like any other. What a
  ;; cold boot would do with such a folder is exactly what the image
  ;; reproduces: the records travel in the image, so its boot prints the
  ;; report this load printed, and the file that failed is missing from the
  ;; image as it is missing from the loaded image a cold boot leaves behind.
  ;; The key digests every file in the folder, so an edit — the one way a
  ;; failure changes — is a different key and a fresh image rather than this
  ;; one reused. Refusing to freeze was the earlier rule; it cost a home
  ;; carrying one chronically failed file a cold boot of its whole layer on
  ;; every launch, and it could never freeze at all.
  (let* ((records (load-cells))
         (failed (count :failed records :key #'cell-state)))
    (when (plusp failed)
      (format stream
              "~&warm-dump: ~:d failed record~:p frozen with the folder; the image replays the report~%"
              failed))
    records))

(defun warm-dump-main (path)
  "The `warm-dump' verb: load the folder directly — never through the
cache — then dump this process as PATH, an executable image whose
*IMAGE-FOLDER-KEY* names what it holds. DUMP never returns."
  (warm-dump-records)
  (disarm-timers)
  (setf *image-origin* sb-ext:*runtime-pathname*
        *image-folder-key* (folder-key))
  ;; The dump is program-op's seam, spelled out. Uiop's dump hooks run
  ;; (ASDF clears its configuration); the restored flag is reset so the
  ;; new image's own RESTORE-IMAGE runs instead of refusing — this
  ;; process was itself restored once, and the flag would ride into the
  ;; dump; and the toplevel is a closure over MAIN, because the origin
  ;; carries its entry point exactly this way — UIOP:*IMAGE-ENTRY-POINT*
  ;; is NIL at runtime, so a bare #'RESTORE-IMAGE toplevel boots into
  ;; nothing (measured: the verify gate refused that image). Runtime
  ;; options ride along: the origin's heap ceiling holds and argv
  ;; reaches Lisp untouched.
  (uiop:call-image-dump-hook)
  (setf uiop/image::*image-restored-p* nil)
  (sb-ext:save-lisp-and-die
   path
   :toplevel (lambda ()
               (uiop:restore-image
                :entry-point (uiop:ensure-function
                              'nodecode.evolved::main)))
   :executable t
   :save-runtime-options t))
