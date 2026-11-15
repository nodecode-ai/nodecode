;;;; heap-image-test.lisp --- the live save's pieces (heap-image.lisp, image-save.lisp, /image).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermetic. The fork and the dump end the process's threads, so they are
;;;; proved end to end by the packaged binary in a real shell, outside this
;;;; suite; here: what a dead thread leaves in the heap, the store's lease
;;;; handed across the exec, the boot's word on a moved folder, and /image's
;;;; words.

(in-package #:nodecode.test)

(deftest image-quiescence-never-kills-and-spends-one-wait-budget ()
  ;; Synthetic clock/thread list: the two survivors never end. A timer has
  ;; already spent two of the five seconds; only three remain for ALL joins.
  (let ((now 0) (joined '()) (stopped nil) (killed nil) (timer-deadline nil))
    (with-stubbed-fdefinitions
        ((get-internal-real-time () now)
         (nlk::stop-launch-image-keeper (&optional seconds)
           (setf stopped seconds))
         (nlk::disarm-timers (&optional deadline)
           (setf timer-deadline deadline)
           (incf now (* 2 internal-time-units-per-second)))
         (sb-thread:list-all-threads ()
           (list sb-thread:*current-thread* :first :second))
         (nlk::image-thread-name (thread) (symbol-name thread))
         (sb-thread:terminate-thread (thread) (push thread killed))
         (nlk::join-image-thread (thread seconds)
           (push (list thread seconds) joined)
           (incf now (round (* seconds internal-time-units-per-second)))
           nil))
      (is (equal '("FIRST" "SECOND") (nlk::quiesce-for-image 5)))
      (is (null killed) "not one arbitrary thread is asynchronously unwound")
      (is (eql 0 stopped) "keeper stop is requested without a private wait budget")
      (is (= (* 5 internal-time-units-per-second) timer-deadline))
      (is (equal '((:first 3)) joined) "only the remaining total budget is waited"))))

(deftest image-timer-joins-share-the-callers-deadline ()
  ;; No real timer is silenced: only the join accounting runs here.
  (let ((now 0) (joined '()) (restored nil))
    (with-stubbed-fdefinitions
        ((get-internal-real-time () now)
         (sb-ext:list-all-timers () nil)
         #-win32 (nlk::silence-alarm () nil)
         (nlk::restore-nursery () (setf restored t))
         (sb-thread:list-all-threads () (list :first :second))
         (nlk::image-thread-name (thread) (format nil "Timer ~a" thread))
         (nlk::join-image-thread (thread seconds)
           (push (list thread seconds) joined)
           (incf now (round (* seconds internal-time-units-per-second)))
           nil))
      (nlk::disarm-timers (* 3 internal-time-units-per-second))
      (is (equal '((:first 3)) joined) "one budget, not three seconds per timer")
      (is restored "the nursery state is restored even when callbacks survive"))))

(deftest launch-keeper-stop-wakes-its-idle-wait ()
  (with-saved-globals ((nlk::*launch-image-keeper* nil)
                       (nlk::*launch-image-period* 60))
    (let ((worker nil) (looked nil))
      (with-stubbed-fdefinition (nlk::tend-launch-image () (setf looked t))
        (unwind-protect
             (progn
               (setf worker (nlk::start-launch-image-keeper))
               (is (eq worker (nlk::start-launch-image-keeper)) "one owner")
               (is (null (nlk::stop-launch-image-keeper .5)))
               (is (not (bt2:thread-alive-p (nlk:worker-thread worker))))
               (is (null nlk::*launch-image-keeper*) "cleared only once dead")
               (is (null looked) "stop before a look starts does no folder work")
               ;; FORGET-DEAD-THREADS clears bare thread globals, not the
               ;; worker record a saved heap can still hold.
               (setf nlk::*launch-image-keeper* worker)
               (let ((old worker))
                 (setf worker (nlk::start-launch-image-keeper))
                 (is (not (eq old worker)) "dead retained worker starts fresh"))
               (is (null (nlk::stop-launch-image-keeper .5))))
          (when worker (nlk:worker-stop worker :timeout 1)))))))

(deftest launch-keeper-retains-a-step-that-has-not-returned ()
  (with-saved-globals ((nlk::*launch-image-keeper* nil)
                       (nlk::*launch-image-period* .001))
    (let ((entered (bt2:make-semaphore)) (release (bt2:make-semaphore)) worker)
      (with-stubbed-fdefinition (nlk::tend-launch-image ()
                                 (bt2:signal-semaphore entered)
                                 (bt2:wait-on-semaphore release :timeout 5))
        (unwind-protect
             (progn
               (setf worker (nlk::start-launch-image-keeper))
               (is (bt2:wait-on-semaphore entered :timeout 1) "step really entered")
               (is (eq worker (nlk::stop-launch-image-keeper .01)))
               (is (eq worker nlk::*launch-image-keeper*) "live handle retained")
               (is (eq worker (nlk::start-launch-image-keeper)) "no overlapping keeper")
               (is (bt2:thread-alive-p (nlk:worker-thread worker)))
               (bt2:signal-semaphore release)
               (is (null (nlk::stop-launch-image-keeper 1)) "natural return completes stop")
               (is (null nlk::*launch-image-keeper*)))
          (bt2:signal-semaphore release)
          (when worker (nlk:worker-stop worker :timeout 1)))))))

(deftest a-dead-thread-a-global-holds-is-forgotten-for-the-image ()
  (let* ((package (make-package (format nil "HEAP-IMAGE-TEST-~d" (random 1000000)) :use '()))
         (dead (intern "*DEAD*" package))
         (wrapped (intern "*WRAPPED*" package))
         (live (intern "*LIVE*" package))
         (gate (sb-thread:make-semaphore)))
    (unwind-protect
         (progn
           (setf (symbol-value dead) (sb-thread:make-thread (lambda () nil))
                 (symbol-value wrapped) (bt2:make-thread (lambda () nil))
                 (symbol-value live) (sb-thread:make-thread (lambda () (sb-thread:wait-on-semaphore gate))))
           (sb-thread:join-thread (symbol-value dead))
           (bt2:join-thread (symbol-value wrapped))
           (is (eql 2 (nlk::forget-dead-threads (list package))) "the two dead ones, and nothing else")
           (is (null (symbol-value dead)) "an SBCL thread that ended reads NIL")
           (is (null (symbol-value wrapped)) "so does bordeaux-threads' wrapper, SPAWN's answer")
           (is (sb-thread:thread-alive-p (symbol-value live)) "a live one is left alone"))
      (sb-thread:signal-semaphore gate)
      (delete-package package))))

(deftest a-lease-held-across-the-exec-is-the-reopened-stores-lease (with-temp-store (store))
  (let ((path (nlk::store-path store)))
    (nlk:close-store)
    (is (not (nlk::store-lease-held-p path)) "closed, the lock is free")
    (let ((lock (nlk::hold-store-lease path)))
      (is lock "taken back with no store open")
      (is (nlk::store-lease-mine-p path) "this process holds it")
      (is (not (nlk::store-lease-held-elsewhere-p path)) "an election here reads it as its own")
      (is (eql (sb-posix:getpid) (nlk::read-store-holder path)) "the sidecar names this process")
      (let ((reopened (nlk:open-store :path path)))
        (is (eql lock (nlk::store-lock-fd reopened)) "the store that opens adopts the held lock")))))

(deftest a-heaps-memory-of-its-locks-is-no-lease-at-boot ()
  ;; A saved heap remembers the descriptors of the process that saved it: a
  ;; heap booted later by the restore pointer read that as its lease and
  ;; opened its store unlocked, a second organism beside it.
  (let ((nlk::*held-locks* (make-hash-table :test #'eql :synchronized t)))
    (nlk::note-held-lock 987 "/nonexistent/history.db.lock")
    (sb-posix:unsetenv nlk::+handed-lease-variable+)
    (is (null (nlk::adopt-handed-lease)) "nothing handed, nothing adopted")
    (is (zerop (hash-table-count nlk::*held-locks*)) "the remembered lock is forgotten")))

(deftest a-lease-handed-across-an-exec-is-adopted-and-a-forged-one-is-not (with-temp-store (store))
  #+win32 (is nil "cannot exist on Windows: no exec there, so HAND-LEASE-ACROSS-EXEC hands nothing")
  #-win32
  (let* ((path (nlk::store-path store))
         (variable nlk::+handed-lease-variable+))
    (nlk:close-store)
    (let ((lock (nlk::hold-store-lease path)))
      (nlk::hand-lease-across-exec lock)
      (is (zerop (sb-posix:fcntl lock sb-posix:f-getfd)) "it stays open across the exec")
      (is (sb-posix:getenv variable) "and the successor is told which descriptor it is")
      (let ((nlk::*held-locks* (make-hash-table :test #'eql :synchronized t)))
        (is (eql lock (nlk::adopt-handed-lease)) "the successor takes it as its own")
        (is (nlk::store-lease-mine-p path) "its election reads it as its own"))
      (let ((other (sb-posix:open "/dev/null" sb-posix:o-rdonly)))
        (unwind-protect
             (let ((nlk::*held-locks* (make-hash-table :test #'eql :synchronized t)))
               (sb-posix:setenv variable
                                (format nil "~d ~a" other (uiop:native-namestring
                                                           (nlk::lock-path-for-store path)))
                                1)
               (is (null (nlk::adopt-handed-lease)) "a descriptor on another file is no lease")
               (is (null (sb-posix:getenv variable)) "and is named to no one after"))
          (sb-posix:close other)))
      (nlk::hand-lease-across-exec lock)
      (let ((reopened (nlk:open-store :path path)))
        (is (eql lock (nlk::store-lock-fd reopened)) "the store adopts it")
        (is (= 1 (sb-posix:fcntl lock sb-posix:f-getfd)) "and closes it at a later exec again")
        (is (null (sb-posix:getenv variable)) "no child or successor is told of it")))))

#+linux
(deftest a-handed-lease-reads-as-held-while-other-locks-come-and-go (with-temp-store (store))
  ;; Read off /proc/locks, the machine's whole lock list taken a page per
  ;; read, a held lease now and then read as not held: a lock dropped between
  ;; two pages shifts the rest up past the next page's start. A test runner's
  ;; shards made that churn on CI; here this process makes it, with a page and
  ;; more of other locks listed ahead of the lease.
  (let* ((path (nlk::store-path store))
         (native (uiop:native-namestring (nlk::lock-path-for-store path)))
         (dir (uiop:pathname-directory-pathname path))
         (descriptors '())
         (churners '())
         (stop nil))
    (flet ((open-here (name)
             (let ((fd (sb-posix:open (uiop:native-namestring (merge-pathnames name dir))
                                      (logior sb-posix:o-creat sb-posix:o-rdwr) #o600)))
               (push fd descriptors)
               fd)))
      (nlk:close-store)
      (let ((lock (nlk::hold-store-lease path)))
        (unwind-protect
             (progn
               ;; Taken after the lease, these list ahead of it.
               (dotimes (i 300)
                 (sb-posix:lockf (open-here (format nil "held-~d.lock" i)) sb-posix:f-tlock 0))
               (dotimes (k 8)
                 (let ((files (loop for i below 32 collect (open-here (format nil "churn-~d-~d.lock" k i)))))
                   (push (sb-thread:make-thread
                          (lambda ()
                            (ignore-errors
                             (loop until stop
                                   do (dolist (fd files) (sb-posix:lockf fd sb-posix:f-tlock 0))
                                      (dolist (fd files) (sb-posix:lockf fd sb-posix:f-ulock 0)))))
                          :name "lock churn")
                         churners)))
               (is (loop repeat 1000 always (nlk::lock-handed-to-this-process-p lock native)) "the held lease reads as held on every read while other locks come and go"))
          (setf stop t)
          (mapc #'sb-thread:join-thread churners)
          (mapc #'sb-posix:close descriptors))
        (is (nlk::store-lease-mine-p path) "and is still held after")
        (nlk:open-store :path path)))))

(deftest a-store-closed-to-become-another-heap-keeps-its-lease (with-temp-store (store))
  ;; Released and taken back, the lock was free for a moment a rival could
  ;; take; kept, it never is.
  (let ((path (nlk::store-path store))
        (lock (nlk::store-lock-fd store)))
    (let ((nlk::*keep-store-lease* t))
      (nlk:close-store))
    (is (eql lock (nlk::held-lock (nlk::lock-path-for-store path))) "the same lock is still held")
    (is (nlk::store-lease-mine-p path) "and still this process's")
    (is (eql (sb-posix:getpid) (nlk::read-store-holder path)) "the sidecar still names this process")
    (let ((reopened (nlk:open-store :path path)))
      (is (eql lock (nlk::store-lock-fd reopened)) "a store opened after adopts it"))))

(deftest a-switch-says-why-it-cannot-run ()
  (let ((nodecode.evolved::*serving* nil))
    ;; Windows has no exec, so a switch there is refused before anything else.
    (is (search #-win32 "only the organism that serves" #+win32 "Windows has no exec"
                (nodecode.evolved::switch-refusal nil)))
    (is (search "no snapshot named nope" (nodecode.evolved::switch-refusal "nope")) "a missing heap is named first")))

(deftest a-saved-heap-says-the-folder-moved-and-never-hands-back ()
  (with-stubbed-fdefinitions ((nlk::folder-key () "key-now"))
    (with-saved-globals ((nlk::*cells-directory* (uiop:temporary-directory)))
      (with-saved-globals ((nlk::*saved-image* (list :name "before" :folder-key "key-then")))
        (let ((drift (nlk::saved-image-drift)))
          (is (search "snapshot before" drift) "names the snapshot")
          (is (search "runs as it was taken" drift) "and says the heap stays")))
      (with-saved-globals ((nlk::*saved-image* (list :name "before" :folder-key "key-now")))
        (is (null (nlk::saved-image-drift)) "an unmoved folder says nothing"))
      (with-saved-globals ((nlk::*saved-image* nil))
        (is (null (nlk::saved-image-drift)) "no saved heap, no word")))))

(deftest the-snapshots-list-is-every-snapshot-then-fresh-from-files (with-image-home (home))
  (save-fake-heap "older")
  (save-fake-heap "newer")
  (let ((rows (nle::image-picker-rows :running "newer")))
    (is (stringp (nle::picker-row-value (first rows))) "the first row is a snapshot: /snapshot takes one, no row does")
    (is (eq nle::+image-folder-action+ (nle::picker-row-value (car (last rows)))) "the last row is fresh from files")
    (is (equal "Fresh from files" (nle::picker-row-label (car (last rows)))))
    (is (= 3 (length rows)))
    (is (equal '("newer" "older") (sort (remove-if-not #'stringp (mapcar #'nle::picker-row-value rows)) #'string<))
        (format nil "a row per heap between them: ~s" (mapcar #'nle::picker-row-value rows)))
    (let ((newer (find "newer" rows :key #'nle::picker-row-value :test #'equal)))
      (is (equal "*" (nle::picker-row-mark newer)) "the running heap is marked")
      (is (search "running now" (nle::picker-row-detail newer)))))
  (let ((folder (car (last (nle::image-picker-rows :running :folder)))))
    (is (search "running now" (nle::picker-row-detail folder)) "or the folder, when no heap runs")))

(deftest a-heap-saved-without-a-name-is-named-by-its-minute (with-image-home (home))
  (let ((time (encode-universal-time 0 34 5 4 11 2026)))
    (is (equal "2026-11-04-0534" (nle::dated-image-name time)))
    (save-fake-heap "2026-11-04-0534")
    (is (equal "2026-11-04-0534-2" (nle::dated-image-name time)) "a taken name gets the next number")
    (is (null (nle::image-name-refusal (nle::dated-image-name time))) "and is a name a heap may have")))

(deftest bare-nodecode-snapshots-opens-the-shell-on-the-list ()
  (dolist (line '(("snapshots") ("snapshots" "restore")))
    (is (equal '(:image-picker) (nth-value 5 (nle::parse-launch-arguments line))) line))
  (is (equal '(:snapshot ()) (nth-value 1 (nle::parse-launch-arguments '("snapshot")))) "a snapshot's NAME is optional")
  (is (equal '(:snapshot ("demo")) (nth-value 1 (nle::parse-launch-arguments '("snapshot" "demo")))))
  (is (equal '(:snapshots ("delete" "x")) (nth-value 1 (nle::parse-launch-arguments '("snapshots" "delete" "x")))))
  (is (null (ignore-errors (nth-value 1 (nle::parse-launch-arguments '("image"))))) "image is no verb any more"))

(deftest a-shell-says-the-outage-it-asked-for-until-its-deadline ()
  (with-saved-globals ((nle::*expected-outage* nil))
    (is (null (nle::expected-outage-text)) "none asked")
    (setf nle::*expected-outage* (cons "image: keeping" (+ (get-internal-real-time) internal-time-units-per-second)))
    (is (equal "image: keeping" (nle::expected-outage-text)) "inside its deadline")
    (setf nle::*expected-outage* (cons "image: keeping" (1- (get-internal-real-time))))
    (is (null (nle::expected-outage-text)) "past it, a gateway error is an error again")))
