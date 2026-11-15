;;;; cancel-waker-test.lisp --- a cancel's waker acts only on what its wait owns.
;;;;
;;;; A cancellable wait notes the sockets it opens; a cancel shuts them down
;;;; (SHUT_RDWR) from the cancelling thread. A descriptor is a number the kernel
;;;; hands out again the moment it is closed, and a pooled connection changes
;;;; owner without changing number. So the waker must never act on a number the
;;;; wait no longer owns, nor at all once its wait has left.

(in-package #:nodecode.test)

(defun %loopback-listener ()
  (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8)))

(defun %loopback-connect (listener)
  (usocket:socket-connect "127.0.0.1" (usocket:get-local-port listener)
                          :element-type '(unsigned-byte 8)))

(defun %socket-fd (socket)
  (sb-bsd-sockets:socket-file-descriptor (usocket:socket socket)))

(defun %stream-cut-p (stream)
  "True when STREAM can no longer send: a SHUT_RDWR reached its socket."
  (handler-case (progn (write-byte 120 stream) (finish-output stream) nil)
    (error () t)))

(defun %cut-p (socket)
  (%stream-cut-p (usocket:socket-stream socket)))

(defmacro %with-cancel-fixture ((turn listener session-id) &body body)
  `(with-session-store (,session-id)
     (let ((,turn (nlk:admit-turn ,session-id "waker-command" "hello"))
           (,listener (%loopback-listener)))
       (unwind-protect (progn ,@body)
         (usocket:socket-close ,listener)))))

(deftest a-cancel-cuts-the-socket-its-wait-still-owns ()
  ;; The control: the waker is real, and %CUT-P sees it.
  (%with-cancel-fixture (turn listener "waker-owned")
    (let* ((entered (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
           (own nil)
           (worker (nlk:spawn "waker-owned-worker"
                     (ignore-errors
                      (nlk:with-cancellable-wait (turn)
                        (setf own (%loopback-connect listener))
                        (sb-thread:signal-semaphore entered)
                        (sb-thread:wait-on-semaphore release :timeout 5))))))
      (unwind-protect
           (progn
             (is (sb-thread:wait-on-semaphore entered :timeout 5))
             (nlk:request-cancel-turn "waker-owned" turn.turn-id "esc")
             (is (%cut-p own) "the socket the wait owns is shut down"))
        (sb-thread:signal-semaphore release)
        (ignore-errors (bt2:join-thread worker))
        (when own (usocket:socket-close own))))))

(deftest a-socket-the-wait-closed-is-not-cut-under-its-reused-number ()
  (%with-cancel-fixture (turn listener "waker-reuse")
    (let* ((entered (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
           (closed-fd nil) (other nil)
           (worker (nlk:spawn "waker-reuse-worker"
                     (ignore-errors
                      (nlk:with-cancellable-wait (turn)
                        (let ((own (%loopback-connect listener)))
                          (setf closed-fd (%socket-fd own))
                          (usocket:socket-close own))
                        (sb-thread:signal-semaphore entered)
                        (sb-thread:wait-on-semaphore release :timeout 5))))))
      (unwind-protect
           (progn
             (is (sb-thread:wait-on-semaphore entered :timeout 5))
             (setf other (%loopback-connect listener))
             (is (eql closed-fd (%socket-fd other)) "the kernel reused the closed number")
             (nlk:request-cancel-turn "waker-reuse" turn.turn-id "esc")
             (is (not (%cut-p other)) "another owner's socket under the reused number is untouched"))
        (sb-thread:signal-semaphore release)
        (ignore-errors (bt2:join-thread worker))
        (when other (usocket:socket-close other))))))

(deftest a-connection-returned-to-the-pool-is-not-cut-by-the-waits-cancel ()
  (%with-cancel-fixture (turn listener "waker-pool")
    (let* ((key (format nil "http://127.0.0.1:~d" (usocket:get-local-port listener)))
           (saved dexador.connection-cache::*connection-pool*)
           (entered (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
           (own nil) (stolen nil)
           (worker nil))
      (unless saved (dexador.connection-cache::make-new-connection-pool))
      (setf worker (nlk:spawn "waker-pool-worker"
                     (ignore-errors
                      (nlk:with-cancellable-wait (turn)
                        (setf own (%loopback-connect listener))
                        (dexador.connection-cache:push-connection key (usocket:socket-stream own))
                        (sb-thread:signal-semaphore entered)
                        (sb-thread:wait-on-semaphore release :timeout 5)))))
      (unwind-protect
           (progn
             (is (sb-thread:wait-on-semaphore entered :timeout 5))
             (setf stolen (dexador.connection-cache:steal-connection key))
             (is (eq stolen (usocket:socket-stream own)) "another owner took the pooled connection")
             (nlk:request-cancel-turn "waker-pool" turn.turn-id "esc")
             (is (and stolen (not (%stream-cut-p stolen))) "its new owner keeps it"))
        (sb-thread:signal-semaphore release)
        (ignore-errors (bt2:join-thread worker))
        (when own (usocket:socket-close own))
        (unless saved (setf dexador.connection-cache::*connection-pool* nil))))))

(deftest a-copied-waker-never-acts-after-its-wait-has-left ()
  ;; REQUEST-CANCEL-TURN copies the wakers under the lock and calls them
  ;; outside it. A gate waker registered last runs first and holds the cancel
  ;; there while the wait leaves; the copies it still holds must then do
  ;; nothing: not count, not cut the socket the thread kept past its wait.
  (%with-cancel-fixture (turn listener "waker-late")
    (let* ((entered (sb-thread:make-semaphore)) (leave (sb-thread:make-semaphore))
           (left (sb-thread:make-semaphore)) (gate-set (sb-thread:make-semaphore))
           (gate-in (sb-thread:make-semaphore)) (gate-out (sb-thread:make-semaphore))
           (gate-end (sb-thread:make-semaphore))
           (kept nil) (late 0)
           (worker (nlk:spawn "waker-late-worker"
                     (ignore-errors
                      (nlk:with-cancellable-wait (turn)
                        (setf kept (%loopback-connect listener))
                        (nlk::with-cancellation-waker (turn (lambda () (incf late)))
                          (sb-thread:signal-semaphore entered)
                          (sb-thread:wait-on-semaphore leave :timeout 5))))
                     (sb-thread:signal-semaphore left)))
           (gate nil) (canceller nil))
      (unwind-protect
           (progn
             (is (sb-thread:wait-on-semaphore entered :timeout 5))
             (setf gate (nlk:spawn "waker-late-gate"
                          (nlk::with-cancellation-waker
                              (turn (lambda ()
                                      (sb-thread:signal-semaphore gate-in)
                                      (sb-thread:wait-on-semaphore gate-out :timeout 5)))
                            (sb-thread:signal-semaphore gate-set)
                            (sb-thread:wait-on-semaphore gate-end :timeout 10))))
             (is (sb-thread:wait-on-semaphore gate-set :timeout 5))
             (setf canceller (nlk:spawn "waker-late-cancel"
                               (nlk:request-cancel-turn "waker-late" turn.turn-id "esc")))
             (is (sb-thread:wait-on-semaphore gate-in :timeout 5) "the cancel holds at the gate")
             (sb-thread:signal-semaphore leave)
             (is (sb-thread:wait-on-semaphore left :timeout 5) "the wait left while the cancel was held")
             (sb-thread:signal-semaphore gate-out)
             (bt2:join-thread canceller)
             (is (zerop late) "a removed waker never runs")
             (is (not (%cut-p kept)) "the socket kept past the wait is not cut"))
        (sb-thread:signal-semaphore leave)
        (sb-thread:signal-semaphore gate-out)
        (sb-thread:signal-semaphore gate-end)
        (ignore-errors (bt2:join-thread worker))
        (when gate (ignore-errors (bt2:join-thread gate)))
        (when kept (usocket:socket-close kept))))))

(defun %procs-running (&rest argv)
  "Pids whose command line is exactly ARGV."
  (loop for dir in (directory "/proc/*/" :resolve-symlinks nil)
        for pid = (ignore-errors (parse-integer (car (last (pathname-directory dir)))))
        for line = (and pid (ignore-errors
                             (with-open-file (in (format nil "/proc/~d/cmdline" pid))
                               (with-output-to-string (out)
                                 (loop for ch = (read-char in nil) while ch do (write-char ch out))))))
        when (and line (equal argv (butlast (uiop:split-string line :separator (string (code-char 0))))))
          collect pid))

(defun %eventually (test seconds)
  (loop with deadline = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        for value = (funcall test)
        when value return value
        while (< (get-internal-real-time) deadline)
        do (sleep 0.02)))

(deftest a-cancelled-eval-takes-its-shell-tree-with-it ()
  ;; The model's (sh ...) runs inside an eval, on the eval's own thread,
  ;; through the session wrapper (setsid), which may fork: the command then
  ;; leads a session of its own, out of reach of a signal to the wrapper's
  ;; group. Escape must still end the command, not leave it running.
  #+linux
  (with-session-store ("waker-sh")
    (let* ((turn (nlk:admit-turn "waker-sh" "sh-command" "hello"))
           (marker "30.4242")
           (worker (nlk:spawn "waker-sh-turn"
                     (ignore-errors
                      (let ((nle::*current-durable-turn* turn))
                        (nlk:with-cancellable-wait (turn)
                          (nle::background-capable-eval (format nil "(sh \"sleep ~a\")" marker) 600000)))))))
      (unwind-protect
           (progn
             (is (%eventually (lambda () (%procs-running "sleep" marker)) 10) "the command runs")
             (nlk:request-cancel-turn "waker-sh" turn.turn-id :operator)
             (is (%eventually (lambda () (null (%procs-running "sleep" marker))) 3) "the command is gone"))
        (dolist (pid (%procs-running "sleep" marker)) (ignore-errors (sb-posix:kill pid sb-posix:sigkill)))
        (ignore-errors (bt2:join-thread worker))))))

(deftest an-inner-wait-returning-a-socket-retires-its-outer-owner ()
  ;; Intentionally RED before the nested ownership repair: the outer region
  ;; opened OWN, but only the inner region's empty box sees the pool return.
  ;; The test thread is the new owner, distinct from the waiting worker.
  (%with-cancel-fixture (turn listener "waker-nested-pool")
    (let* ((key (format nil "http://127.0.0.1:~d" (usocket:get-local-port listener)))
           (saved dexador.connection-cache::*connection-pool*)
           (entered (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
           (own nil) (stolen nil) (worker nil))
      (unless saved (dexador.connection-cache::make-new-connection-pool))
      (unwind-protect
           (progn
             (setf worker
                   (nlk:spawn "waker-nested-pool-worker"
                     (ignore-errors
                      (nlk:with-cancellable-wait (turn)
                        (setf own (%loopback-connect listener))
                        (nlk:with-cancellable-wait (turn)
                          (dexador.connection-cache:push-connection key (usocket:socket-stream own)))
                        ;; The inner wait has exited; the outer wait still owns
                        ;; its original registration until RELEASE is signalled.
                        (sb-thread:signal-semaphore entered)
                        (sb-thread:wait-on-semaphore release :timeout 5)))))
             (is (sb-thread:wait-on-semaphore entered :timeout 5) "the nested pool return completed")
             (setf stolen (dexador.connection-cache:steal-connection key))
             (is (and own (eq stolen (usocket:socket-stream own))) "another thread took the exact pooled socket")
             (is (and stolen (not (%stream-cut-p stolen))) "the borrowed socket works before cancellation")
             (nlk:request-cancel-turn "waker-nested-pool" turn.turn-id :operator)
             (is (and stolen (not (%stream-cut-p stolen))) "the outer wait cannot cut the new owner's socket"))
        (sb-thread:signal-semaphore release)
        (when worker (ignore-errors (bt2:join-thread worker)))
        (ignore-errors (dexador.connection-cache:steal-connection key))
        (when own (ignore-errors (usocket:socket-close own)))
        (unless saved (setf dexador.connection-cache::*connection-pool* nil))))))

(deftest an-sse-read-cancel-cannot-cut-a-socket-published-to-the-pool ()
  ;; Intentionally RED before the SSE ownership repair. Exercise the real
  ;; WALK-PROVIDER-STREAM, not a copy of its waker. Dexador publishes the raw
  ;; socket in its close action before MAYBE-CLOSE clears the wrapper slot;
  ;; hold that exact interval while another thread borrows the connection.
  (%with-cancel-fixture (turn listener "waker-sse-pool")
    (let* ((key (format nil "http://127.0.0.1:~d" (usocket:get-local-port listener)))
           (saved dexador.connection-cache::*connection-pool*)
           (pooled (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
           (done (sb-thread:make-semaphore))
           (own (%loopback-connect listener))
           (raw (usocket:socket-stream own))
           (body nil) (stolen nil) (worker nil) (outcome nil))
      (unless saved (dexador.connection-cache::make-new-connection-pool))
      (setf body
            (dexador.keep-alive-stream:make-keep-alive-stream
             raw :end 0
             :on-close-or-eof
             (lambda (underlying abort)
               (declare (ignore abort))
               (when (and underlying (open-stream-p underlying))
                 (dexador.connection-cache:push-connection key underlying)
                 (sb-thread:signal-semaphore pooled)
                 (unless (sb-thread:wait-on-semaphore release :timeout 5)
                   (error "SSE pool-publication barrier timed out"))))))
      (unwind-protect
           (with-stubbed-fdefinition (dex:post (&rest args) (values body 200))
             (setf worker
                   (nlk:spawn "waker-sse-pool-worker"
                     (unwind-protect
                          (setf outcome
                                (handler-case
                                    (let ((nle::*current-durable-turn* turn))
                                      (nle::walk-provider-stream
                                       (lambda (frame finish record)
                                         (declare (ignore frame finish record)))
                                       :config (nle::make-effective-provider-config
                                                :lane "openai-completions" :endpoint key
                                                :model "fixture" :api-key "none" :request-timeout 10)
                                       :request-json "{}" :headers '() :require-done nil)
                                      :returned)
                                  (nlk:turn-cancelled-condition () :cancelled)
                                  (error (condition) condition)))
                       (sb-thread:signal-semaphore done))))
             (is (sb-thread:wait-on-semaphore pooled :timeout 5) "the real SSE read published its socket")
             (is (eq raw (dexador.keep-alive-stream:keep-alive-stream-stream body)) "the wrapper still holds the raw slot in the publication interval")
             (setf stolen (dexador.connection-cache:steal-connection key))
             (is (eq stolen raw) "another thread borrowed the published socket")
             (is (and stolen (not (%stream-cut-p stolen))) "the borrowed socket works before the old read is cancelled")
             (nlk:request-cancel-turn "waker-sse-pool" turn.turn-id :operator)
             (is (and stolen (not (%stream-cut-p stolen))) "cancelling the old SSE read leaves the new owner usable")
             (sb-thread:signal-semaphore release)
             (is (sb-thread:wait-on-semaphore done :timeout 5) "the provider worker exits after its owned barrier releases")
             (is (eq outcome :cancelled) "the real provider walk reports the requested cancellation"))
        (sb-thread:signal-semaphore release)
        (when worker (ignore-errors (bt2:join-thread worker)))
        (ignore-errors (dexador.connection-cache:steal-connection key))
        (ignore-errors (usocket:socket-close own))
        (unless saved (setf dexador.connection-cache::*connection-pool* nil))))))

(deftest audit-a-socket-enrolled-after-cancel-is-still-woken ()
  ;; Hold the actual connect hook just before ownership enrollment. The
  ;; cancel sees an empty region, then CONNECT returns into that region.
  (%with-cancel-fixture (turn listener "waker-late-enroll")
    (let ((entered (sb-thread:make-semaphore)) (enroll (sb-thread:make-semaphore))
          (ready (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
          (original (symbol-function 'nlk::note-cancel-fd))
          (own nil) (worker nil))
      (unwind-protect
           (with-stubbed-fdefinition
               (nlk::note-cancel-fd (fd)
                 (sb-thread:signal-semaphore entered)
                 (unless (sb-thread:wait-on-semaphore enroll :timeout 5)
                   (error "late enrollment barrier timed out"))
                 (funcall original fd))
             (setf worker
                   (nlk:spawn "waker-late-enroll-worker"
                     (ignore-errors
                      (nlk:with-cancellable-wait (turn)
                        (setf own (%loopback-connect listener))
                        (sb-thread:signal-semaphore ready)
                        (sb-thread:wait-on-semaphore release :timeout 5)))))
             (is (sb-thread:wait-on-semaphore entered :timeout 5) "the real connect reached enrollment")
             (nlk:request-cancel-turn "waker-late-enroll" turn.turn-id :operator)
             (sb-thread:signal-semaphore enroll)
             (is (sb-thread:wait-on-semaphore ready :timeout 5) "connect returned after cancellation")
             (is (and own (%cut-p own)) "late enrollment consumes the already requested wake"))
        (sb-thread:signal-semaphore enroll)
        (sb-thread:signal-semaphore release)
        (when worker (ignore-errors (bt2:join-thread worker)))
        (when own (ignore-errors (usocket:socket-close own)))))))
