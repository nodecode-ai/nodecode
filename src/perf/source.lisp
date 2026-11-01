;;;; source.lisp --- timing a hot path at several sizes, and how its cost grows.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The gate is a contract about growth, not a budget of milliseconds: what a
;;;; frame paint costs depends on the box, but that its cost does not grow with
;;;; the length of the transcript is a property of the code. So each case runs
;;;; the same operation at sizes N, 4N, 16N and fits the slope of log(cost)
;;;; against log(size); the contract says how steep that slope may be. A runner
;;;; slower or faster than this box scales every point the same way, which the
;;;; slope cancels; only a change in how cost grows moves it.
;;;;
;;;; Two measures are fitted, since a regression shows in one or the other:
;;;; bytes consed (what a longer conversation makes every call allocate) and
;;;; CPU seconds. Each point is a batch of calls -- one call is shorter than
;;;; the clock's resolution -- and the batch is sized by a pilot call.

(in-package #:nodecode.perf)

(defstruct (scaling-contract (:constructor make-scaling-contract))
  "What a hot path's growth may be. KIND is :LINEAR (cost grows with size;
SLOPE may reach BOUND, a number near 1), :CONSTANT (cost must not see size;
SLOPE may reach BOUND, near 0), or :UNIT (cost divided by size is held to
BOUND, for a per-item cost that must stay flat). COST is the slack a fitting
slope is allowed before the bound: a slope is a statistic, and two of them
over the same code differ a little."
  (kind :LINEAR)
  (bound 1.10)
  (cpu-cost 0.15)
  (bytes-cost 0.10)
  (floor 0)
  (unit "")
  ;; A path main does not yet hold to KIND keeps KIND as its target and is
  ;; held to the growth it has today, BOUND, until it is fixed: KNOWN says
  ;; where the breach is written up, and the gate prints both, so a fix
  ;; shows as headroom and a further regression still fails.
  (known nil))

(defstruct (point (:constructor make-point))
  (size 0) (cpu 0d0) (bytes 0d0) (calls 1))

(defparameter *target-batch-seconds* 0.20d0
  "How long one point's batch should run: long enough that the clock's
resolution and the collector's pauses fade, short enough that sixteen sizes
of a suite stay a CI step.")

(defparameter *max-batch-calls* 100000)

(defun gc-flush ()
  "The bytes consed after a full collection, so what happened to be collected
stays out of the next measurement."
  (sb-ext:gc :full t)
  (sb-ext:get-bytes-consed))

(defun batch (fn calls)
  "FN run CALLS times; answers the CPU seconds per call FN's own code ran,
bytes consed per call, and the collector's seconds per call."
  ;; The collector's time is taken out of the CPU measure: what a collection
  ;; costs depends on everything else the heap holds (here the fixture, in a
  ;; shell the whole image) and on the heap's size, so its slope says more
  ;; about the fixture than about the path. The bytes consed are what the
  ;; path hands the collector; that measure carries its share.
  (let* ((before (gc-flush))
         (gc-before sb-ext:*gc-run-time*)
         (start (get-internal-run-time)))
    (dotimes (i calls)
      (funcall fn i))
    (let ((gc (- sb-ext:*gc-run-time* gc-before)))
      (values (/ (float (- (get-internal-run-time) start gc) 1d0)
                 internal-time-units-per-second calls)
              (/ (float (- (sb-ext:get-bytes-consed) before) 1d0) calls)
              (/ (float gc 1d0) internal-time-units-per-second calls)))))

(defun run-scaling-fn (fn &key (repeats 3))
  "A pilot call sizes FN's batch, then REPEATS batches are run and the median
of each measure answers, so one collection's pause cannot decide a point."
  (let* ((pilot-start (get-internal-run-time))
         (pilot (progn (funcall fn 0) (- (get-internal-run-time) pilot-start)))
         (seconds (/ (float pilot 1d0) internal-time-units-per-second))
         (calls (max 1 (min *max-batch-calls*
                            (ceiling *target-batch-seconds* (max seconds 1d-6))))))
    (let ((cpu '()) (bytes '()))
      (dotimes (repeat repeats)
        (multiple-value-bind (c b) (batch fn calls)
          (push c cpu) (push b bytes)))
      (values (median cpu) (median bytes) calls))))

(defun median (values)
  "The middle of VALUES, the lower of the two when there are an even number."
  (let ((sorted (sort (copy-list values) #'<)))
    (nth (floor (1- (length sorted)) 2) sorted)))

(defun exponent (points &key (slot 'point-cpu))
  "Least-squares slope of log(SLOT) against log(size) over POINTS; NIL when
fewer than two points have a positive cost (a zero cost has no logarithm)."
  (let* ((pairs (loop for point in points
                      for cost = (funcall slot point)
                      for size = (point-size point)
                      when (and (plusp cost) (plusp size))
                        collect (list (log size) (log cost))))
         (n (length pairs)))
    (when (< n 2)
      (return-from exponent nil))
    (let* ((mx (/ (loop for (x nil) in pairs sum x) n))
           (my (/ (loop for (nil y) in pairs sum y) n)))
      (/ (loop for (x y) in pairs sum (* (- x mx) (- y my)))
         (loop for (x nil) in pairs sum (expt (- x mx) 2))))))

(defun unit-cost (points slot)
  "The last point's SLOT divided by its size: a per-item cost."
  (let ((last (car (last points))))
    (/ (funcall slot last) (point-size last))))

(defun verdict (contract points)
  "NIL when POINTS keep CONTRACT, else the reasons it breaks it, a string
per measure. A case whose costs sit under the contract's floor is reported
breaching: a path too cheap to measure has not been held to anything."
  (let ((reasons '()))
    (flet ((held (name slot cost)
             (let* ((measured (if (eq (scaling-contract-kind contract) :unit)
                                  (unit-cost points slot)
                                  (exponent points :slot slot)))
                    (slack (if (eq (scaling-contract-kind contract) :unit)
                               (* cost (scaling-contract-bound contract))
                               cost)))
               (cond ((null measured)
                      (push (format nil "~a was too cheap to fit" name) reasons))
                     ((> measured (+ (scaling-contract-bound contract) slack))
                      (push (format nil "~a ~a ~,2f, past the bound of ~,2f"
                                    name
                                    (if (eq (scaling-contract-kind contract) :unit)
                                        "costs per item" "grows as n^")
                                    measured (scaling-contract-bound contract))
                            reasons))))))
      (held "cpu" 'point-cpu (scaling-contract-cpu-cost contract))
      (held "bytes" 'point-bytes (scaling-contract-bytes-cost contract)))
    (nreverse reasons)))
