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
;;;; work. Work is the user-space instructions this thread ran, where the
;;;; kernel counts them (Linux): a count another job on the runner cannot
;;;; bend, where CPU seconds stretch when a busy box shares a core's caches
;;;; and its hyperthread. Elsewhere work is CPU seconds. Each point is a batch
;;;; of calls -- one call is shorter than the clock's resolution -- and the
;;;; batch is sized by a pilot call.

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
  (work-cost 0.10)
  (bytes-cost 0.10)
  (floor 0)
  (unit "")
  ;; A path main does not yet hold to KIND keeps KIND as its target and is
  ;; held to the growth it has today, BOUND, until it is fixed: KNOWN says
  ;; where the breach is written up, and the gate prints both, so a fix
  ;; shows as headroom and a further regression still fails.
  (known nil))

(defstruct (point (:constructor make-point))
  (size 0) (work 0d0) (bytes 0d0) (calls 1))

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

;;; Linux's perf_event_open, for one counter: this thread's user-space
;;; instructions. The attribute block is the kernel's first version (64
;;; bytes); exclude_kernel and exclude_hv keep it within what an unprivileged
;;; process may count of itself at perf_event_paranoid 2.

(defparameter *perf-event-open-syscall*
  #+(and linux x86-64) 298 #+(and linux arm64) 241 #-(and linux (or x86-64 arm64)) nil)

(defun open-instruction-counter ()
  "A descriptor counting the calling thread's user-space instructions from
now, or NIL where this kernel or platform will not count them."
  (let ((nr *perf-event-open-syscall*))
    (when nr
      (let ((attr (sb-alien:make-alien (sb-alien:unsigned 8) 64)))
        (unwind-protect
             (let ((sap (sb-alien:alien-sap attr)))
               (dotimes (i 64) (setf (sb-sys:sap-ref-8 sap i) 0))
               (setf (sb-sys:sap-ref-32 sap 0) 0 ; PERF_TYPE_HARDWARE
                     (sb-sys:sap-ref-32 sap 4) 64 ; PERF_ATTR_SIZE_VER0
                     (sb-sys:sap-ref-64 sap 8) 1 ; PERF_COUNT_HW_INSTRUCTIONS
                     ;; exclude_kernel (bit 5) and exclude_hv (bit 6); not
                     ;; disabled, so it counts from the open.
                     (sb-sys:sap-ref-64 sap 40) (logior (ash 1 5) (ash 1 6)))
               (let ((fd (sb-alien:alien-funcall
                          (sb-alien:extern-alien
                           "syscall" (function sb-alien:long sb-alien:long sb-sys:system-area-pointer
                                               sb-alien:int sb-alien:int sb-alien:int sb-alien:unsigned-long))
                          nr sap 0 -1 -1 0)))
                 (and (>= fd 0) fd)))
          (sb-alien:free-alien attr))))))

(defun read-instruction-counter (fd)
  "The count FD's counter holds now."
  (sb-alien:with-alien ((buffer (sb-alien:unsigned 64)))
    (multiple-value-bind (read error) (sb-unix:unix-read fd (sb-alien:alien-sap (sb-alien:addr buffer)) 8)
      (unless (eql read 8)
        (error "reading the instruction counter failed: ~a" error))
      buffer)))

(defparameter *work-measure*
  (let ((fd (open-instruction-counter)))
    (if fd (progn (sb-unix:unix-close fd) :instructions) :cpu))
  "What a point's work counts: :INSTRUCTIONS where the kernel counts this
thread's, else :CPU seconds.")

(defun work-label ()
  "The name of what *WORK-MEASURE* counts, for a report."
  (if (eq *work-measure* :instructions) "instructions" "cpu"))

(defun batch (fn calls)
  "FN run CALLS times; answers the work per call FN's own code did (see
*WORK-MEASURE*), bytes consed per call, and the collector's seconds per call."
  ;; The collector's share is taken out of the work: what a collection costs
  ;; depends on everything else the heap holds (here the fixture, in a shell
  ;; the whole image) and on the heap's size, so its slope says more about the
  ;; fixture than about the path. The bytes consed are what the path hands the
  ;; collector; that measure carries its share. The collector runs on this
  ;; thread, so its instructions are taken out by its share of the time.
  (let* ((counter (and (eq *work-measure* :instructions) (open-instruction-counter))))
    (unwind-protect
         (let* ((before (gc-flush))
                (gc-before sb-ext:*gc-run-time*)
                (start (get-internal-run-time))
                (instructions-before (and counter (read-instruction-counter counter))))
           (dotimes (i calls)
             (funcall fn i))
           (let* ((instructions (and counter (- (read-instruction-counter counter) instructions-before)))
                  (run (- (get-internal-run-time) start))
                  (gc (- sb-ext:*gc-run-time* gc-before))
                  (mutator (- run gc)))
             (values (if instructions
                         (/ (* (float instructions 1d0) (if (plusp run) (/ (max mutator 0) run) 1)) calls)
                         (/ (float mutator 1d0) internal-time-units-per-second calls))
                     (/ (float (- (sb-ext:get-bytes-consed) before) 1d0) calls)
                     (/ (float gc 1d0) internal-time-units-per-second calls))))
      (when counter (sb-unix:unix-close counter)))))

(defun run-scaling-fn (fn &key (repeats 3))
  "A pilot call sizes FN's batch, then REPEATS batches are run and the median
of each measure answers, so one collection's pause cannot decide a point."
  (let* ((pilot-start (get-internal-run-time))
         (pilot (progn (funcall fn 0) (- (get-internal-run-time) pilot-start)))
         (seconds (/ (float pilot 1d0) internal-time-units-per-second))
         (calls (max 1 (min *max-batch-calls*
                            (ceiling *target-batch-seconds* (max seconds 1d-6))))))
    (let ((work '()) (bytes '()))
      (dotimes (repeat repeats)
        (multiple-value-bind (w b) (batch fn calls)
          (push w work) (push b bytes)))
      (values (median work) (median bytes) calls))))

(defun median (values)
  "The middle of VALUES, the lower of the two when there are an even number."
  (let ((sorted (sort (copy-list values) #'<)))
    (nth (floor (1- (length sorted)) 2) sorted)))

(defun exponent (points &key (slot 'point-work))
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
      (held (work-label) 'point-work (scaling-contract-work-cost contract))
      (held "bytes" 'point-bytes (scaling-contract-bytes-cost contract)))
    (nreverse reasons)))
