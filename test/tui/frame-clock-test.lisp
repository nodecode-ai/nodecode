;;;; frame-clock-test.lisp --- ticks survive live UI replacement, without input.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(deftest frame-clock-does-not-read-mutable-ui-state ()
  ;; An opaque model pins the worker contract: no CLOS accessor, predicate,
  ;; preview read or widget inspection may run off the model's loop thread.
  (is-present (f (fboundp 'nle::run-frame-clock)) "the clock has a callable body for live delivery"
    (let* ((program (nlt:make-program :opaque-model))
           (thread nil) (failure nil))
      (setf (nlt:program-running program) t)
      (nlk:with-cleanup ((setf (nlt:program-running program) nil)
                         (when thread (bt2:join-thread thread)))
        (setf thread (nlk:spawn "test-frame-clock"
                       (handler-case (nle::run-frame-clock program)
                         (serious-condition (c) (setf failure c)))))
        (multiple-value-bind (msg found)
            (sb-concurrency:receive-message (nlt::program-mailbox program) :timeout 1)
          (is (and found (nlt:tick-msg-p msg)) "a tick arrives with no readable UI model"))
        (is (null failure) "replacement cannot kill the timer through an accessor"))
      (is (not (bt2:thread-alive-p thread)) "the timer exits with its program"))))

(deftest frame-clock-idle-ticks-owe-no-paint ()
  (is-present (f (fboundp 'nle::run-frame-clock)) "the canonical clock is present"
    (let* ((program (make-probe-program))
           (clock nil))
      (setf (probe-quiet program.model) t)
      (with-event-loop (program loop-thread)
        (nlk:with-cleanup ((setf (nlt:program-running program) nil)
                           (when clock (bt2:join-thread clock)))
          (setf clock (nlk:spawn "test-idle-clock" (nle::run-frame-clock program)))
          (is (await (:tries 100 :rest 0.01)
                (>= (length (seen-of-type program.model 'nlt::tick-msg)) 3)) "the loop, not the worker, receives idle ticks")
          (is (zerop (probe-frames program.model)) "quiet idle ticks never compose or paint"))))))

(deftest frame-clock-paints-dirty-transcript-without-input (with-temp-store ())
  (is-present (f (fboundp 'nle::run-frame-clock)) "the canonical clock is present"
    (let* ((model (make-frame-model 80 24 :session-id "clock-stream"))
           (program (nlt:make-program model))
           (renderer (nlt::program-renderer program))
           (clock nil) (failures nil)
           (hold (sb-concurrency:make-mailbox))
           (busy (sb-concurrency:make-mailbox)))
      (setf (nlt:renderer-output-stream renderer) (make-string-output-stream)
            (nlt::renderer-width renderer) 80
            (nlt::renderer-height renderer) 24
            (nlt:program-running program) t
            (nlt::program-error-handler program) (lambda (where c) (push (list where c) failures)))
      (paint-model model (nle::ingest-message-fact "user" "clock question"))
      (nlt:render renderer (nlt:view model))
      (nlt:start-worker program)
      (nlt:run-command program
        (lambda ()
          (sb-concurrency:send-message busy t)
          (sb-concurrency:receive-message hold :timeout 5)
          nil))
      (sb-concurrency:receive-message busy :timeout 1)
      (with-event-loop (program loop-thread)
        (nlk:with-cleanup ((setf (nlt:program-running program) nil)
                           (when clock (bt2:join-thread clock))
                           (sb-concurrency:send-message hold t)
                           (nlt:stop-worker program))
          (setf clock (nlk:spawn "test-stream-clock" (nle::run-frame-clock program)))
          ;; This message only mutates the reducer-owned state; terminal
          ;; I/O owes no frame. The canonical timer must flush it.
          (nlt:send program
            (nlt:make-terminal-io-msg
              :thunk (nlk:lambda-ignoring (stream)
                       (setf (nle::chat-model-state model)
                             (nle::ingest-message-fact (nle::chat-model-state model)
                                                      "assistant" "timer-only-stream-output")
                             (nle::chat-model-transcript-dirty model) t))))
          (is (await (:tries 100 :rest 0.01)
                (let ((paint (nlt:renderer-last-view-state renderer)))
                  (and paint (some (lambda (row) (search "timer-only-stream-output" row))
                                   (nlt:view-state-rows paint))))) "stream output reaches the flushed frame without a key, mouse, resize or free worker")
          (is (null failures) "the real fold and renderer complete without errors")
          (is (null (nle::chat-model-transcript-dirty model)) "the loop consumed the dirty projection"))))))
(deftest status-ticks-compose-only-when-a-chrome-clock-steps ()
  ;; A running turn keeps the ticks coming (LIVE-CHROME-ANIMATING-P), but a
  ;; tick on which neither the spinner nor the elapsed clock stepped is
  ;; quiet: the chrome is re-read, nothing is composed or painted.
  (with-frame-model (model 80 24 :session-id "status-tick" :in-flight t)
    (flet ((quiet-tick-p () (nth-value 2 (nle:update model (nlt:make-tick-msg))))
           (steps () (let ((ms (nle::since-ms (nle::chat-model-turn-started-at model))))
                       (+ (floor ms 100) (floor ms nle::*status-running-frame-ms*)))))
      (let ((before (steps)))
        (nle::render-state model)
        (let ((composed (loop repeat 60 count (not (quiet-tick-p)))))
          (is (<= composed (- (steps) before))
              (format nil "sixty ticks back to back compose once per clock step, not ~d times" composed))))
      ;; The elapsed clock a second further on reads differently: that tick composes.
      (decf (nle::chat-model-turn-started-at model) internal-time-units-per-second)
      (is (not (quiet-tick-p)) "a tick on which the elapsed clock moved composes")
      ;; Dirtied transcript state still takes its full render on the tick.
      (setf (nle::chat-model-transcript-dirty model) t)
      (is (not (quiet-tick-p)) "a dirtied transcript renders on the tick regardless"))))
