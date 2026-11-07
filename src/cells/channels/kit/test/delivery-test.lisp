;;;; delivery-test.lisp --- bounded queue, worker, typing cadence, lanes.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (l nck::channel-lane) (lane nck::channel-lane))

(deftest channel-delivery-queue-bounds ()
  (let ((queue (nck:make-work-queue "channel-test-queue" :cap 2)))
    (is (nck:queue-push queue :a))
    (is (nck:queue-push queue :b))
    (is (not (handler-bind ((warning #'muffle-warning))
               (nck:queue-push queue :c))))
    (is (= 2 (nck:queue-depth queue)))
    (is-values (item found) (nck:queue-pop queue 0) (found is) (item eq :a "FIFO order"))
    (is-values (item found) (nck:queue-pop queue 0) (found is) (item eq :b))
    (is-values (item found) (nck:queue-pop queue 0.02) (found not) (item null))))

(deftest channel-delivery-worker-runs-jobs-and-ticks ()
  (let* ((lock (bt2:make-lock :name "channel-test"))
         (ran '())
         (ticks 0)
         (worker (nck:start-delivery-worker
                  "channel-test-deliver"
                  :tick-seconds 0.05
                  :tick-fn (lambda ()
                             (bt2:with-lock-held (lock) (incf ticks))))))
    (nlk:with-cleanup ((nck:stop-delivery-worker worker))
      (dolist (job (list (lambda () (bt2:with-lock-held (lock) (push :one ran)))
                         (lambda () (error "job explodes"))
                         (lambda () (bt2:with-lock-held (lock) (push :two ran)))))
        (nck:delivery-worker-enqueue worker job))
      (is (await () (bt2:with-lock-held (lock)
                      (equal '(:two :one) ran))))
      (is (await () (bt2:with-lock-held (lock) (plusp ticks)))))
    (is (not (member "channel-test-deliver" (channel-thread-names)
                     :test #'equal)))))

(deftest channel-delivery-typing-cadence (let ((typing (nck:make-typing-state))))
  (is (not (nck:typing-due-p typing 1000)) "idle lanes never type")
  (nck:typing-note-started typing 1000)
  (is (nck:typing-due-p typing 1000))
  (nck:typing-note-sent typing 1000)
  (is (not (nck:typing-due-p typing 5000)))
  (is (nck:typing-due-p typing 9100))
  (nck:typing-note-sent typing 9100)
  (is (not (nck:typing-due-p typing 302000)))
  ;; The cap measures the turn's SILENCE, not the indicator's age: the
  ;; tick hands it the surface's newest sighting, so a turn that keeps
  ;; producing keeps its beat however long the indicator has been up.
  (is (nck:typing-due-p typing 400000 :seen-at-ms 395000))
  (is (not (nck:typing-due-p typing 400000 :seen-at-ms 50000)))
  (is (nck:typing-due-p typing 250000 :seen-at-ms 500))
  (nck:typing-note-stopped typing)
  (is (not (nck:typing-due-p typing 20000)) "stopped lanes never type"))

(deftest channel-delivery-lane-registry (let* ((lanes (nck:make-lane-table "channel-test-lanes"))
                                               (lane (nck:intern-lane lanes "chan-s1"
                                                                      :target '(:channel-id "100")
                                                                      :trigger-message-id "m1"))))
  (is (eq lane (nck:find-lane lanes "chan-s1")))
  (is (null (nck:find-lane lanes "chan-ghost")))
  (is (eq lane (nck:intern-lane lanes "chan-s1"
                                :target '(:channel-id "100"
                                          :thread-id "t9")
                                :trigger-message-id "m2")))
  (is (equal "t9" (getf lane.target :thread-id)))
  (is (equal "m2" lane.trigger-message-id))
  (let ((seen '()))
    (nck:intern-lane lanes "chan-s2")
    (nck:map-lanes lanes (lambda (l) (push l.session-id seen)))
    (is (equal '("chan-s1" "chan-s2") (sort seen #'string<)))))
