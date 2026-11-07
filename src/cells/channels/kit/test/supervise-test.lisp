;;;; supervise-test.lisp --- stop discipline of supervised lap threads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The stop thunk must never destroy a lap thread: without this,
;;;; async termination inside foreign TLS frames would corrupt libssl
;;;; and wedge process exit. A lap that outlives the grace is abandoned —
;;;; warned about, left to exit on its own when its bounded wait completes.

(in-package #:nodecode.test)

(deftest channel-supervise-stop-abandons-never-destroys ()
  (let* ((finished (bt2:make-semaphore :name "channel-test-lap-finished"))
         (stop (nck:start-supervised
                "channel-test-blocked-lap"
                (lambda (stop-p)
                  (declare (ignore stop-p))
                  ;; Simulates a bounded blocking read the stop cannot
                  ;; interrupt (a getUpdates window, a TLS frame read).
                  (sleep 0.8)
                  (bt2:signal-semaphore finished)
                  :stop)
                :stop-grace-seconds 0.2)))
    (sleep 0.1) ; the lap is now inside its blocking wait
    (let ((before (get-internal-real-time)))
      (handler-bind ((warning #'muffle-warning))
        (funcall stop))
      (is (< (/ (- (get-internal-real-time) before)
                internal-time-units-per-second)
             0.7)))
    (is (bt2:wait-on-semaphore finished :timeout 5))
    (is (await () (not (member "channel-test-blocked-lap"
                               (channel-thread-names)
                               :test #'equal))))))

(deftest channel-supervise-prompt-stop-joins (let ((stop (nck:start-supervised
                                                          "channel-test-prompt-lap"
                                                          (lambda (stop-p)
                                                            (declare (ignore stop-p))
                                                            0.05)))))
  (is (await () (member "channel-test-prompt-lap"
                        (channel-thread-names) :test #'equal)))
  (funcall stop)
  (is (not (member "channel-test-prompt-lap" (channel-thread-names)
                   :test #'equal))))

(deftest channel-supervise-backoff-is-short-and-capped ()
  ;; The wait after the far end comes back is what a person sees as the bot
  ;; being down: a quarter second doubling, never past five.
  (is (equal '(0.25 0.5 1.0 2.0 4.0 5 5)
             (mapcar #'nck::lap-backoff-seconds '(1 2 3 4 5 6 20)))))

(deftest channel-supervise-a-lap-that-returns-resets-the-backoff ()
  ;; Four failed dials, a connection that later drops, then one more failure:
  ;; the wait after it is the first rung again, not the fifth (4 s).
  (let* ((laps 0)
         (stamps '())
         (stop (nck:start-supervised
                "channel-test-backoff-lap"
                (lambda (stop-p)
                  (declare (ignore stop-p))
                  (push (get-internal-real-time) stamps)
                  (case (incf laps)
                    ((1 2 3 4) (error "dial refused"))
                    (5 0)
                    (6 (error "dial refused"))
                    (t :stop))))))
    (is (await (:timeout 20) (>= laps 7)))
    (funcall stop)
    ;; STAMPS is newest first: the seventh lap came one rung after the sixth.
    (is (< (/ (- (first stamps) (second stamps)) internal-time-units-per-second)
           1))))
