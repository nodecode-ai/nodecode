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
             (is (eq stolen (usocket:socket-stream own)) "another request took the pooled connection")
             (nlk:request-cancel-turn "waker-pool" turn.turn-id "esc")
             (is (and stolen (not (%stream-cut-p stolen)))
                 "the connection's new owner keeps it"))
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
