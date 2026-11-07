;;;; supervise.lisp --- supervised lap threads with backoff.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The exec-watcher pattern (gateway.lisp start-background-exec-watcher)
;;;; grown a lap contract: a channel transport loop (Discord WS connection,
;;;; Telegram long poll) runs as repeated laps under one supervisor thread.
;;;; Transient transport failure is absorbed here with exponential backoff;
;;;; permanent failure (:fatal — e.g. a rejected bot token) stops the lane
;;;; with one loud warning and never retries.

(in-package #:nodecode-channel-kit)

(defparameter +lap-backoff-cap-seconds+ 5
  "The longest wait between failed laps: the wait after the far end comes
back is what a person sees as the bot being down.")

(defun lap-backoff-seconds (failures)
  "Seconds to wait after the FAILURES-th failed lap in a row: a quarter
second doubling, capped at +LAP-BACKOFF-CAP-SECONDS+."
  ;; The cap was 30, and the count never reset once a lap had connected: a
  ;; process that had seen eight failed dials waited 30 s after every drop
  ;; for the rest of its life. A dial is not an identify, so a
  ;; dial every 5 s through an outage costs Discord's daily identify budget
  ;; nothing.
  (min (* 0.25 (expt 2 (1- failures))) +lap-backoff-cap-seconds+))

(defun start-supervised (name lap-fn &key (stop-grace-seconds 5) on-degraded)
  "Run LAP-FN repeatedly on a thread named NAME until stopped."
  ;; LAP-FN is called with one argument, a STOP-P function laps poll to exit
  ;; long waits early. Its return value schedules the next lap:
  ;;
  ;;   :stop        the lane is done; the thread exits quietly.
  ;;   :fatal       permanent failure; warn once and exit. No retry.
  ;;   a real >= 0  sleep that many seconds, then lap again.
  ;;   anything else  lap again immediately.
  ;;
  ;; A lap that returns resets the backoff. A signalled error is transient by
  ;; definition: warn, back off (LAP-BACKOFF-SECONDS), lap again.
  ;; ON-DEGRADED, when given, is called with the condition on each failure —
  ;; the status-surface hook.
  ;;
  ;; Returns a stop thunk: signals stop, then waits up to STOP-GRACE-SECONDS
  ;; for the lap thread to finish its current lap and exit. A thread that is
  ;; still parked past the grace is ABANDONED with one warning — never
  ;; destroyed: async termination inside foreign TLS frames would corrupt
  ;; libssl and wedge process exit. The contract this buys from
  ;; laps: every blocking wait must be bounded (queue timeouts, HTTP read
  ;; timeouts, transport severs) so STOP-P is consulted on a real cadence; an
  ;; abandoned lap exits on its own when its current wait completes, holding
  ;; no store state.
  (let* ((stop (bt2:make-semaphore :name name))
         (stop-p nil)
         (stop-fn (lambda () stop-p))
         (thread
           (nlk:spawn name
             (let ((failures 0))
               (loop
                 (when stop-p (return))
                 (let ((verdict
                         (handler-case (prog1 (funcall lap-fn stop-fn)
                                         (setf failures 0))
                           (error (condition)
                             (incf failures)
                             (warn "~a lap signalled (attempt ~a): ~a"
                                   name failures condition)
                             (when on-degraded
                               (ignore-errors
                                (funcall on-degraded condition)))
                             (lap-backoff-seconds failures)))))
                   (cond
                     ((eq verdict :stop) (return))
                     ((eq verdict :fatal)
                      (warn "~a stopped permanently" name)
                      (return))
                     ((realp verdict)
                      (when (bt2:wait-on-semaphore stop :timeout verdict)
                        (return))))))))))
    (lambda ()
      (setf stop-p t)
      (bt2:signal-semaphore stop)
      (loop repeat (max 1 (round (* stop-grace-seconds 20)))
            while (bt2:thread-alive-p thread)
            do (sleep 0.05))
      (if (bt2:thread-alive-p thread)
          (warn "~a did not stop within ~as; abandoning (it exits when ~
                 its current wait completes)"
                name stop-grace-seconds)
          (ignore-errors (bt2:join-thread thread)))
      t)))
