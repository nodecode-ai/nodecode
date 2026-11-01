;;;; warm-test.lisp --- the warm-image launch cache (launch.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here runs hermetically: the cache directory is a fixture,
;;;; exec and spawn are stubbed through the one stubbing seam, and no test
;;;; dumps an 80 MB image — the supervisor's end-to-end test pins
;;;; *ORGANISM-COMMAND* to a shell fixture that plays the dump child and the
;;;; verify child.

(in-package #:nodecode.test)

(defmacro with-warm-fixture ((key root cache) &body body)
  "A scratch cell folder with one layer file, a scratch cache, and the
folder's KEY bound; BODY may edit the folder and recompute."
  ;; All three are rebound innermost and declared ignorable there, so a test
  ;; that wants only one of them need not open by apologising for the other
  ;; two.
  `(with-cell-fixture (,root)
     (with-temp-trees ((,cache "warm-cache"))
       (let ((nlk::*cells-directory* ,root)
             (nlk::*warm-cache-directory* ,cache)
             (nlk::*image-stamp* "stamp-w")
             (nlk::*warm-spawn-delay* 0))
         (write-layer-folder ,root "nl-warm"
                             '(("probe" . "(defun cells-probe-warm () :warm)")))
         (let ((,key (nlk::folder-key)) (,root ,root) (,cache ,cache))
           (declare (ignorable ,key ,root ,cache))
           ,@body)))))

(defun backdate (path age)
  "PATH, its times set AGE seconds from now: a negative AGE is the past."
  ;; utime takes unix seconds; universal time is 1900-based.
  (let ((unix (- (get-universal-time) 2208988800 (- age))))
    (sb-posix:utime (namestring path) unix unix))
  path)

(defun write-fake-image (image &key (length 64))
  "A published image at IMAGE that ends as SBCL ends an executable core -- the
core's offset, then the magic the core opens with -- cut to LENGTH octets."
  (ensure-directories-exist image)
  (let ((octets (make-array 64 :element-type '(unsigned-byte 8) :initial-element 7)))
    (flet ((put (at word)
             (dotimes (i 8) (setf (aref octets (+ at i)) (ldb (byte 8 (* 8 i)) word)))))
      (put 16 #x5342434C) (put 48 16) (put 56 #x5342434C))
    (alexandria:write-byte-vector-into-file (subseq octets 0 length) image :if-exists :supersede)))

(deftest warm-key-covers-the-folder-and-skips-dot-entries (with-warm-fixture (key root cache))
  (is (= 32 (length key)) "a key is 32 hex characters")
  (is (equal key (nlk::folder-key)) "and stable across calls")
  ;; Dot entries are not folder content: the scribe's staging tmp and a
  ;; .git's churn must not flap the key.
  (write-temp-file (merge-pathnames "nl-warm/.probe.lisp.tmp" root) "x")
  (write-temp-file (merge-pathnames "nl-warm/.git-not-really" root) "x")
  (is (equal key (nlk::folder-key)) "dot entries do not move it")
  ;; Content does.
  (write-temp-file (merge-pathnames "nl-warm/probe.lisp" root)
                   (evolved-source "(defun cells-probe-warm () :warmer)"))
  (let ((edited (nlk::folder-key)))
    (is (not (equal key edited)) "an edited file is a new key")
    (write-temp-file (merge-pathnames "nl-warm/fresh.lisp" root) "")
    (is (not (equal edited (nlk::folder-key))) "a new file is a new key")))

(deftest warm-key-covers-the-shipped-source-tree (with-warm-fixture (key root cache))
  ;; The stale-override check digests the shipped tree at boot, so the tree
  ;; is part of what a frozen load read.
  (with-temp-trees ((source "warm-src"))
    (write-temp-file (merge-pathnames "core/one.lisp" source) "(list 1)")
    (let* ((nlk::*source-root* source)
           (before (nlk::folder-key)))
      (is (not (equal key before)) "the source root is in the key")
      (write-temp-file (merge-pathnames "core/one.lisp" source) "(list 1 2)")
      (is (not (equal before (nlk::folder-key)))))))

(deftest warm-boot-execs-a-published-image-and-cleans-its-receipt-on-refusal ()
  (with-warm-fixture (key root cache)
    (let ((image (nlk::warm-image-path key))
          (execs '()))
      (write-fake-image image)
      (with-stubbed-fdefinition (nlk::exec-program (path argv)
                                 (push (list (namestring path) argv) execs)
                                 "stub refused")
        (let ((records (nlk:boot-cells :spawn nil)))
          (is (= 1 (length execs)) "the launcher tried the published image")
          (is (equal (namestring image) (first (first execs))))
          (is (equal sb-ext:*posix-argv* (second (first execs))))
          (is (find "nl-warm" records :key #'nlk:cell-name :test #'equal))
          (is (null (directory (make-pathname :name "booting" :type :wild
                                              :defaults (nlk::warm-key-directory key))))))))))

#-win32
(deftest a-refused-exec-says-why-and-gives-the-gc-lock-back ()
  ;; EXEC-PROGRAM holds the runtime's GC lock across execve, so no
  ;; collection's SIGUSR2 rides into the successor. A refusal
  ;; leaves this image running, and it must collect again.
  (let ((reason (nlk::exec-program #p"/nonexistent/nodecode-exec-probe" (list "probe"))))
    (is (search "execv /nonexistent/nodecode-exec-probe: No such file" reason) "the kernel's refusal is the answer")
    (sb-ext:gc)
    (is (eql 1 (sb-alien:alien-funcall (sb-alien:extern-alien "try_acquire_gc_lock" (function sb-alien:int)))) "and the lock is free")
    (sb-alien:alien-funcall (sb-alien:extern-alien "release_gc_lock" (function sb-alien:int)))))

#-win32
(deftest an-exec-leaves-its-successor-no-alarm ()
  ;; ITIMER_REAL survives execve, and so does a SIGALRM not yet taken; a
  ;; successor that meets either before its runtime has blocked its signals
  ;; would be killed by the alarm. The exec's last act zeroes the alarm, drops
  ;; the pending one and blocks nothing; a refusal gets back its alarm, its
  ;; timer and its mask. The pending alarm, were it let through, would be
  ;; taken inside the scheduler's lock and deferred -- and SBCL blocks the
  ;; mask for a deferred signal.
  (let ((timer (sb-ext:make-timer (lambda ()) :thread t :name "test-exec-alarm"))
        (alarm-only (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((alarm-left ()
             (multiple-value-bind (ok interval-sec interval-usec sec usec)
                 (sb-unix:unix-getitimer :real)
               (declare (ignore ok interval-sec interval-usec))
               (+ sec (/ usec 1000000)))))
      ;; SIGALRM's bit, as glibc and Darwin lay a sigset_t out
      (setf (ldb (byte 1 (mod (1- sb-unix:sigalrm) 8))
                 (aref alarm-only (floor (1- sb-unix:sigalrm) 8)))
            1)
      (sb-ext:schedule-timer timer 30)
      (let ((before (nlk::swap-signal-mask alarm-only)))
        (nlk:with-cleanup ((sb-ext:unschedule-timer timer) (nlk::swap-signal-mask before))
          (sb-alien:alien-funcall (sb-alien:extern-alien "raise" (function sb-alien:int sb-alien:int))
                                  sb-unix:sigalrm)
          (is (plusp (alarm-left)) "the timer arms the process alarm")
          (destructuring-bind (left mask)
              (nlk::call-as-last-act (lambda () (list (alarm-left) (nlk::swap-signal-mask nil))))
            (is (zerop left) "no alarm rides into the successor")
            (is (every #'zerop mask) "no signal blocked, the pending alarm dropped"))
          (is (plusp (alarm-left)) "a refused exec gets its alarm back")
          (is (member timer (sb-ext:list-all-timers)) "its timer still scheduled")
          (is (equalp alarm-only (nlk::swap-signal-mask nil)) "and its mask"))))))

(deftest warm-dead-receipt-poisons-the-key (with-warm-fixture (key root cache))
  ;; A receipt whose pid is gone means a published image died before MAIN;
  ;; the key goes to `failed' and the boot never execs again.
  (let ((image (nlk::warm-image-path key))
        (execs 0))
    (write-fake-image image)
    (write-temp-file (nlk::boot-receipt-path key 99999999) "0")
    (with-stubbed-fdefinition (nlk::exec-program (path argv)
                               (incf execs)
                               "never")
      (let ((records (nlk:boot-cells :spawn nil)))
        (is (zerop execs) "a poisoned key is never exec'd")
        (is (probe-file (nlk::warm-failed-path key)) "the marker records it")
        (is (find "nl-warm" records :key #'nlk:cell-name :test #'equal))))))

(deftest warm-boot-spawns-one-builder-and-markers-gate-it (with-warm-fixture (key root cache))
  (let ((spawns '())
        ;; a developer image has no organism to spawn: the command is pinned
        (nlk::*organism-command* '("/bin/true")))
    (with-stubbed-fdefinition (nlk::spawn-detached (command log)
                               (push command spawns)
                               command)
      (let ((thread (nlk::spawn-warm-builder key)))
        (is thread "a bare key spawns the builder")
        (when thread (bt2:join-thread thread)))
      (is (= 1 (length spawns)) "exactly one spawn")
      (is (find-if (lambda (argument) (search "warm" argument))
                   (first spawns)))
      ;; A fresh sentinel gates the next spawn.
      (write-temp-file (nlk::warm-sentinel-path key) "1")
      (is (null (nlk::spawn-warm-builder key)) "a fresh sentinel holds it")
      (ignore-errors (delete-file (nlk::warm-sentinel-path key)))
      ;; A failed marker gates it too, and says nothing on the way: a shell's
      ;; *ERROR-OUTPUT* is its notice lane, and the operator can do nothing
      ;; with `warm launch cache: the dump child exited 1' (xn-203).
      (nlk::mark-warm-failed key "no freezing this one")
      (is (string= "" (with-output-to-string (*error-output*)
                        (is (null (nlk::spawn-warm-builder key)) "a failed marker holds it"))))
      (is (= 1 (length spawns)) "and neither gate spawned"))))

(deftest warm-boot-of-a-build-that-cached-nothing-walks-no-key-first (with-warm-fixture (key root cache))
  ;; No stamp directory, no image of this build to find: the launcher loads at
  ;; once and keys the folder beside the load for its builder; a child keys
  ;; nothing, since the layer-compile trial forks after its boot.
  (let ((asked '()) (walks 0))
    (is (null (probe-file (nlk::warm-stamp-directory))) "nothing cached yet")
    (with-stubbed-fdefinition (nlk::spawn-warm-builder (key) (push key asked) nil)
      (is (find "nl-warm" (nlk:boot-cells :spawn t) :key #'nlk:cell-name :test #'equal))
      (is (equal (list key) asked) "the builder is still asked for, with the folder's key"))
    (with-stubbed-fdefinition (nlk::folder-key () (incf walks) key)
      (is (find "nl-warm" (nlk:boot-cells :spawn nil) :key #'nlk:cell-name :test #'equal))
      (is (zerop walks) "a child walks no key"))))

(defmacro with-warm-supervisor-run ((score) &body body)
  "Run the warm supervisor with its two children played by the shell
fixtures answering SCORE and the image taken as complete, then BODY; the
fixtures are deleted on unwind."
  (let ((outer (gensym "OUTER")) (inner (gensym "INNER")))
    ;; OUTER plays the dump child installing INNER, the image the verify leg execs.
    `(with-temp-file (,inner :type "sh" :contents (format nil "#!/bin/sh
case \"$1\" in
  trial) cat > /dev/null; echo 'nodecode-trial-score ~d' ;;
esac
exit 0~%" ,score))
       (with-temp-file (,outer :type "sh" :contents (format nil "#!/bin/sh
case \"$1\" in
  warm-dump) cp ~a \"$2\" && chmod +x \"$2\" ;;
esac
exit 0~%" ,inner))
         (with-stubbed-fdefinition (nlk::image-complete-p (image) t)
           (let ((nlk::*organism-command* (list "/bin/sh" ,outer)))
             (nlk::run-warm-supervisor)))
         ,@body))))

(deftest an-image-is-whole-when-it-ends-as-sbcl-ends-a-core (with-temp-file (path :type "img"))
  ;; The trailer SBCL writes last, not the origin's size: a fresh home's
  ;; image was a heap page smaller than the executable, and never published.
  (write-fake-image path)
  (is (nlk::image-complete-p path) "the core magic behind its offset")
  (write-fake-image path :length 40)
  (is (not (nlk::image-complete-p path)) "a truncated copy is not")
  (is (not (nlk::image-complete-p "/nonexistent/image")) "nor is a missing one"))

(deftest warm-supervisor-publishes-a-verified-image (with-warm-fixture (key root cache))
  ;; End to end with the dump child played by a shell fixture that installs
  ;; an executable fake image, which the verify leg then really runs with
  ;; the `trial' verb: the supervisor demands a whole image and a correct
  ;; key answer, publishes, and clears its sentinel.
  (with-warm-supervisor-run (1)
    (is (probe-file (nlk::warm-image-path key)))
    (is (null (probe-file (nlk::warm-sentinel-path key))))
    (is (null (probe-file (nlk::warm-failed-path key))))))

(deftest warm-supervisor-refuses-an-image-that-answers-the-wrong-key ()
  (with-warm-fixture (key root cache)
    (with-warm-supervisor-run (0)
      (is (null (probe-file (nlk::warm-image-path key))))
      (is (probe-file (nlk::warm-failed-path key)))
      (is (null (directory (make-pathname
                            :name "nodecode" :type :wild
                            :defaults (nlk::warm-key-directory key))))))))

(deftest warm-prune-keeps-the-newest-two-images-across-stamps (with-warm-fixture (key root cache))
  (flet ((publish (stamp key age &aux (nlk::*image-stamp* stamp))
           (backdate (write-temp-file (nlk::warm-image-path key) "core") age)))
    (let ((oldest (publish "stamp-a" "k-oldest" -300))
          (middle (publish "stamp-b" "k-middle" -200))
          (newest (publish "stamp-b" "k-newest" -100)))
      (let ((nlk::*image-stamp* "stamp-w"))
        (nlk::prune-warm-cache))
      (is (null (probe-file oldest)) "the oldest image is pruned")
      (is (probe-file middle) "the middle one stays")
      (is (probe-file newest) "and so does the newest")
      (is (null (probe-file (uiop:pathname-directory-pathname oldest)))))))

#+linux
(deftest warm-prune-keeps-an-image-a-live-process-runs (with-warm-fixture (key root cache))
  ;; A gateway up across two folder changes: its image is older than the two
  ;; newest, and the process still runs it. The image is a copy of the shell,
  ;; one binary everywhere (a multi-call `sleep' would answer to its name),
  ;; held open by a command it cannot exec away from.
  (flet ((publish (key age &aux (nlk::*image-stamp* "stamp-a"))
           (backdate (write-temp-file (nlk::warm-image-path key) "core") age))
         (listed-p (image)
           (await (:tries 200 :rest 0.01)
             (member (namestring image) (nlk::running-executables) :test #'string=))))
    (let ((served (let ((nlk::*image-stamp* "stamp-a"))
                    (ensure-directories-exist (nlk::warm-image-path "k-served")))))
      (uiop:copy-file (nlk::executable-on-path "sh") served)
      (sb-posix:chmod (namestring served) #o755)
      (backdate served -300)
      (let ((idle (publish "k-idle" -400))
            (middle (publish "k-middle" -200))
            (newest (publish "k-newest" -100))
            (child (uiop:launch-program (list (namestring served) "-c" "sleep 30; :"))))
        (nlk:with-cleanup ((uiop:terminate-process child :urgent t)
                           (uiop:wait-process child))
          (is (listed-p served) "the kernel lists the copy as running")
          (nlk::prune-warm-cache)
          (is (probe-file served) "the image a live process runs stays")
          (is (null (probe-file idle)) "an older image nothing runs is pruned")
          (is (probe-file middle))
          (is (probe-file newest))
          ;; Deleted under its process and put back: the kernel's name for
          ;; what the process runs now ends `(deleted)', and the path stays.
          (delete-file served)
          (backdate (write-temp-file served "restored") -300)
          (nlk::prune-warm-cache)
          (is (probe-file served) "an image put back at a running process's path stays"))))))

(deftest warm-folders-keep-their-own-images-and-pointers (with-warm-fixture (key root cache))
  ;; Every home on a machine shares the one cache, and a launch image answers
  ;; one folder only. Six gateways on one box (2026-11-01) pruned each other's
  ;; images down to the newest two, so each rebuilt its own every minute, for
  ;; ever. Other folders' newer images must not prune this one's, and their
  ;; pointers must not stand for its own.
  (flet ((publish (folder key age &aux (nlk::*cells-directory* folder))
           (let ((image (backdate (write-temp-file (nlk::warm-image-path key) "core") age)))
             (nlk::write-file-atomically (nlk::warm-pointer-path)
                                         (format nil "~a~%~a~%" (namestring image) key))
             image)))
    (with-temp-trees ((first-other "folder-a") (second-other "folder-b"))
      (let ((own (publish root key -300)))
        (publish first-other "k-a" -200)
        (publish second-other "k-b" -100)
        (let ((nlk::*cells-directory* second-other))
          (nlk::prune-warm-cache))
        (is (probe-file own) "another folder's publish never prunes this folder's image")
        (is (equal key (nth-value 1 (nlk::read-warm-pointer))) "this folder's pointer still names its own image")))))

(defun backdate-tree (directory age)
  "DIRECTORY and everything under it, its times set AGE seconds from now."
  (uiop:collect-sub*directories directory t t
                                (lambda (sub)
                                  (dolist (file (uiop:directory-files sub))
                                    (backdate file age))
                                  (backdate sub age)))
  directory)

(deftest warm-prune-deletes-what-no-folder-comes-back-for (with-warm-fixture (key root cache))
  ;; A deleted profile leaves its part of the cache behind, and a build from
  ;; before the per-folder layout left its stamps at the top. Neither has a
  ;; folder to come back for it: a prune deletes both once they have sat
  ;; still a day. A part whose folder stands stays however old, and a part
  ;; touched within the day stays -- a build of it may be under way.
  (with-temp-trees ((standing "folder-standing"))
    (ensure-directories-exist standing)
    (flet ((part (folder age &aux (nlk::*cells-directory* folder))
             (write-temp-file (nlk::warm-image-path "k-old") "core")
             (nlk::write-file-atomically (nlk::warm-owner-path) (format nil "~a~%" (namestring folder)))
             (backdate-tree (nlk::warm-folder-directory) age)))
      (let ((kept (part standing -172800))
            (gone (part (merge-pathnames "deleted/" standing) -172800))
            (busy (part (merge-pathnames "deleting/" standing) -60))
            (old-layout (merge-pathnames "stamp-old/" cache)))
        (write-temp-file (merge-pathnames "k-old/nodecode" old-layout) "core")
        (backdate-tree old-layout -172800)
        (write-temp-file (nlk::warm-image-path key) "core")
        (nlk::prune-warm-cache)
        (is (probe-file kept) "a part whose folder stands stays, however old")
        (is (null (probe-file gone)) "a part whose folder is gone goes once it sat still a day")
        (is (probe-file busy) "a part touched within the day stays")
        (is (null (probe-file old-layout)) "a stamp the old layout left at the top goes too")
        (is (probe-file (nlk::warm-image-path key)) "this folder's own image stays")))))

#+linux
(deftest warm-prune-keeps-a-part-a-live-process-runs (with-warm-fixture (key root cache))
  ;; A gateway of an older build still serves from the image it exec'd out of
  ;; its stamp at the top of the cache: deleting that would strand every
  ;; fresh child the gateway execs from the path.
  (let* ((old (merge-pathnames "stamp-run/" cache))
         (served (ensure-directories-exist (merge-pathnames "k-served/nodecode" old))))
    (uiop:copy-file (nlk::executable-on-path "sh") served)
    (sb-posix:chmod (namestring served) #o755)
    (backdate-tree old -172800)
    (let ((child (uiop:launch-program (list (namestring served) "-c" "sleep 30; :"))))
      (nlk:with-cleanup ((uiop:terminate-process child :urgent t)
                         (uiop:wait-process child))
        (is (await (:tries 200 :rest 0.01) (member (namestring served) (nlk::running-executables) :test #'string=)) "the kernel lists the copy as running")
        (write-temp-file (nlk::warm-image-path key) "core")
        (nlk::prune-warm-cache)
        (is (probe-file served) "a part holding an image a live process runs stays")))))

(deftest a-launcher-hands-over-instead-of-execing-when-asked (with-warm-fixture (key root cache))
  ;; The TUI launcher paints first and hands the process over from its boot
  ;; thread; BOOT-CELLS itself only execs for the entries that never painted
  ;; (a child, serve). A HAND-OVER answers only when the exec did not happen —
  ;; the kernel refused it, or the caller could not make it — and the boot
  ;; then loads the folder here, exactly as a refused exec always has.
  (write-fake-image (nlk::warm-image-path key))
  (nlk:bind ((seen nil)
             (asdf:*central-registry* asdf:*central-registry*)
             (asdf::*immutable-systems* asdf::*immutable-systems*)
             (records (nlk:boot-cells
                       :spawn nil
                       :hand-over (lambda (image)
                                    (setf seen image)
                                    "test: the exec did not happen"))))
    (is (equal (namestring (nlk::warm-image-path key))
               (and seen (namestring seen))))
    (is (null (probe-file (nlk::boot-receipt-path key))))
    (is records "the folder loaded here instead")))

(deftest a-dump-disarms-the-timers-it-would-race ()
  ;; A timer's SIGALRM landing inside GC-AND-SAVE is fatal to the runtime —
  ;; measured on the operator's own home: `interrupt already pending when
  ;; sig14 received', exit 1, no image, and the failure marker then kept the
  ;; folder cold for good. The dump child disarms every timer it finds; the
  ;; image it freezes arms its own at every start (MAIN's COLLECT-WHILE-IDLE).
  ;; Unscheduling alone still lost saves (xn-203): the kernel's alarm, armed
  ;; or already raised, landed; and a callback's own thread outlived it, which
  ;; SAVE-LISP-AND-DIE refuses. This image goes on after, so it gets SBCL's
  ;; handler and its collector back.
  (let* ((started (bt2:make-semaphore :name "test-warm-callback-started"))
         (timer (sb-ext:make-timer (lambda ()) :thread t :name "test-warm-idle-gc"))
         (slow (sb-ext:make-timer (lambda () (bt2:signal-semaphore started) (sleep 0.3))
                                  :thread t :name "test-warm-slow-check")))
    (sb-ext:schedule-timer timer 30 :repeat-interval 30)
    (nlk:with-cleanup ((ignore-errors (sb-ext:unschedule-timer timer))
                       (ignore-errors (sb-ext:unschedule-timer slow))
                       #-win32 (sb-sys:enable-interrupt sb-unix:sigalrm #'sb-unix::sigalrm-handler)
                       (nlk:collect-while-idle))
      (is (member timer (sb-ext:list-all-timers)) "the timer is armed")
      #-win32 (is (some #'plusp (rest (multiple-value-list (sb-unix:unix-getitimer :real)))))
      ;; a repeating check caught mid-callback: off the list, on its own thread
      (sb-ext:schedule-timer slow 0.01 :repeat-interval 30)
      (is (bt2:wait-on-semaphore started :timeout 10))
      (nlk::disarm-timers)
      (is (null (sb-ext:list-all-timers)))
      ;; its thread is done, and it did not schedule itself again
      (is (notany (lambda (thread) (equal "Timer test-warm-slow-check" (sb-thread:thread-name thread)))
                  (sb-thread:list-all-threads)))
      (is (not (sb-ext:timer-scheduled-p slow)))
      ;; no alarm left armed for the save, and none raised can land
      #-win32 (is (equal '(t 0 0 0 0) (multiple-value-list (sb-unix:unix-getitimer :real))))
      #+linux (is (logbitp (1- sb-unix:sigalrm)
                           (parse-integer (find "SigIgn:" (uiop:read-file-lines "/proc/self/status")
                                                :test #'uiop:string-prefix-p)
                                          :start 7 :radix 16))))))

(deftest a-folder-with-a-failed-file-freezes-and-says-so (with-cell-fixture (root))
  ;; The dump no longer refuses a folder that reported failures: it loads it,
  ;; says how many records it is freezing, and lets the image carry the
  ;; report. Refusing cost a home with one chronically failed file — the
  ;; operator's — a cold boot of its whole layer on every launch.
  (write-layer-folder root "nl-fail"
                      '(("good" . "(defun cells-probe-frozen-good () :here)")
                        ("bad" . "(error \"this file cannot load\")")))
  (nlk:bind ((nlk::*cells-directory* root) (asdf:*central-registry* asdf:*central-registry*)
             (asdf::*immutable-systems* asdf::*immutable-systems*) (records nil) (note nil))
    (setf note (with-output-to-string (err)
                 (setf records (nlk::warm-dump-records err))))
    (is (find :failed records :key #'nlk:cell-state))
    (is (eq :here (evolved-fn "CELLS-PROBE-FROZEN-GOOD")))
    (is (search "1 failed record" note) "the note counts the failed record")
    (is (search "frozen" note) "and says the folder was frozen with it")))

(deftest warm-image-state-loads-nothing-and-reports-drift (with-warm-fixture (key root cache))
  ;; A process whose *IMAGE-FOLDER-KEY* is set is a warm image: its folder
  ;; is already loaded, its receipt is deleted, and a moved folder is one
  ;; line, not a reload.
  (write-temp-file (nlk::boot-receipt-path key) "1")
  (let ((nlk::*image-folder-key* key)
        (nlk::*cells* (list (nlk::make-cell :name "frozen" :path root))))
    (let ((records (nlk:boot-cells :spawn t)))
      (is (equal '("frozen") (mapcar #'nlk:cell-name records)))
      (is (null (probe-file (nlk::boot-receipt-path key)))))
    ;; With no receipt to vouch for it, the image keys its folder itself, on
    ;; a thread (globals: the thread sees no binding of this one), and the
    ;; answer it serves on (AWAIT-WARM-VERIFICATION) names a moved folder.
    (flet ((verify ()
             (with-saved-globals (nlk::*warm-verification*
                                  ((sb-ext:symbol-global-value 'nlk::*image-folder-key*) key)
                                  ((sb-ext:symbol-global-value 'nlk::*cells-directory*) root)
                                  ((sb-ext:symbol-global-value 'nlk::*warm-cache-directory*) cache)
                                  ((sb-ext:symbol-global-value 'nlk::*warm-verification-failure*) nil))
               (nlk:boot-cells :spawn nil)
               (is nlk::*warm-verification* "the folder is checked")
               (when nlk::*warm-verification* (bt2:join-thread nlk::*warm-verification*))
               (sb-ext:symbol-global-value 'nlk::*warm-verification-failure*))))
      (is (null (verify)) "a folder as it was answers the image's key")
      ;; Now the folder moves under the frozen image.
      (write-temp-file (merge-pathnames "nl-warm/probe.lisp" root)
                       (evolved-source "(defun cells-probe-warm () :moved)"))
      (is (search key (or (verify) "")) "the drift is said: the key it no longer answers"))))

;;; --- fresh processes of this organism ---------------------------------------

(deftest organism-command-is-the-origin-executable-or-the-pinned-list ()
  ;; A developer SBCL has no organism to spawn; a pinned list takes the verb
  ;; and its arguments; the release binary names its origin executable.
  (is (null (nlk::organism-binary-p)))
  (is (null (nlk::organism-command "warm")) "a developer image spawns nothing")
  (let ((nlk::*organism-command* '("/bin/true")))
    (is (equal '("/bin/true" "warm-dump" "/tmp/x")
               (nlk::organism-command "warm-dump" "/tmp/x")))))

(deftest ask-child-answers-the-marked-number-and-nothing-else ()
  ;; The verify stands on this classification: a number that arrived is the
  ;; answer; a clean exit without one, a hang past the deadline, and a
  ;; program that cannot start are each a refusal that says why.
  (with-temp-file (child :type "sh" :contents "#!/bin/sh
read form
case \"$form\" in
  answer) echo 'nodecode-trial-score 42' ;;
  silent) exit 0 ;;
  hang) sleep 30 ;;
esac
exit 0
")
    (flet ((ask (form deadline)
             (nlk::ask-child (list "/bin/sh" (namestring child)) form deadline)))
      (multiple-value-bind (score detail) (ask "answer" 10.0)
        (is (eql 42 score) detail))
      (is-values (score detail) (ask "silent" 10.0) (score null "no marked line, no score")
        ((search "without a score" detail) is detail))
      (nlk:bind ((started (get-internal-real-time)) ((score detail) (ask "hang" 1.0)))
        (is (null score) "a hang scores nothing")
        (is (search "killed" detail) detail)
        (is (< (/ (- (get-internal-real-time) started) internal-time-units-per-second) 10)))
      (is (signals-error error (nlk::ask-child '("/nonexistent/program") "x" 1.0))))))

;;; --- the floor ----------------------------------------------------------------

(deftest a-self-that-died-before-serving-boots-the-next-launch-pristine (with-warm-fixture (key root cache))
  ;; A serve leaves its receipt before the load and takes it once it serves;
  ;; a launch that finds one whose pid is gone runs generation 0 and writes
  ;; the marker. One death retries the folder at the launch after, a second
  ;; in a row holds the floor, and a key move clears it.
  (with-saved-globals ((nlk:*pristine* nil) (nlk::*pristine-reason* nil))
    (flet ((dead-receipt (&optional (receipt-key key))
             (write-temp-file (nlk::self-receipt-path 4000000)
                              (nlk:encode-json-object (nlk:json-object "pid" 4000000 "key" receipt-key)))))
      (is (null (nlk:decide-pristine)) "nothing died: the folder loads")
      (nlk:boot-cells :spawn nil :receipt t)
      (is (probe-file (nlk::self-receipt-path)) "a serve's receipt stands while it boots")
      (is (find "nl-warm" (nlk:cells) :key #'nlk:cell-name :test #'equal) "and the folder loaded")
      (nlk:take-self-receipt)
      (is (null (probe-file (nlk::self-receipt-path))) "taken once it serves")
      (dead-receipt "some-other-key")
      (is (null (nlk:decide-pristine)) "a receipt of a folder since moved counts for nothing")
      (is (null (probe-file (nlk::self-receipt-path 4000000))) "and is taken")
      (dead-receipt)
      (is (eq :death (nlk:decide-pristine)) "a receipt whose pid is gone, under this key")
      (is nlk:*pristine*)
      (is (eql 1 (nlk:json-value (nlk:read-death-marker) :integer "deaths")))
      (is (search "died before it served" (nlk:pristine-reason)))
      (let ((said (with-output-to-string (*error-output*) (nlk:boot-cells :spawn nil))))
        (is (search "pristine: generation 0, no folder loaded" said) "the boot line")
        (is (null (nlk:cells)) "nothing loaded"))
      (is (search "pristine: generation 0" (nlk:generations)) "the list says so")
      (is (search "pristine" (nlk::hand-over-to-published-image)) "no image is followed")
      (let ((nlk::*organism-command* '("/bin/false")))
        (is (null (nlk::tend-launch-image)) "the keeper proves nothing on the floor")
        (is (null (nlk::read-refusal))))
      (setf nlk:*pristine* nil nlk::*pristine-reason* nil)
      (is (null (nlk:decide-pristine)) "one death: the launch after retries the folder")
      (dead-receipt)
      (is (eq :death (nlk:decide-pristine)) "a second death in a row")
      (is (eql 2 (nlk:json-value (nlk:read-death-marker) :integer "deaths")))
      (setf nlk:*pristine* nil)
      (is (eq :held (nlk:decide-pristine)) "two in a row hold the floor")
      (is nlk:*pristine*)
      (setf nlk:*pristine* nil)
      (write-temp-file (merge-pathnames "nl-warm/mended.lisp" root) "")
      (is (null (nlk:decide-pristine)) "a key move clears it")
      (is (null (nlk:read-death-marker)))
      (is (not nlk:*pristine*)))))
