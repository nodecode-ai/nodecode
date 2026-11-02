;;;; perf-bench.lisp --- the workloads `just perf' measures, one per process.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by perf-gate.py into an SBCL that loaded :nodecode/test from the
;;;; tree under measure -- the change's own tree or its base -- so both trees
;;;; run this one file, the head's, against their own code. Each workload is
;;;; one function that wraps the work a person waits on in MEASURED; nothing
;;;; outside MEASURED is counted (the setup, the fixture, the GC between).
;;;;
;;;; Per process, PERF-RUN writes one plist: the workload's wall time, the
;;;; CPU time of every thread of the process, and the bytes it consed, summed
;;;; over its MEASURED spans. When perf-gate.py runs the process under
;;;; `perf stat --control', MEASURED also switches perf's counters on and off
;;;; around each span, so perf's instruction count covers exactly the same
;;;; work: user-space instructions of every thread, the one number here that
;;;; runner load does not move.
;;;;
;;;; The workloads use the suite's own fixtures, so they stand on what every
;;;; test stands on and move with them. A fixture this file names must exist
;;;; in the base tree too; when it does not, the base cannot be measured and
;;;; the gate says so (exit 2) rather than guessing.

(in-package #:nodecode.test)

(defvar *perf-ctl* nil "perf's control fifo, open for writing, or NIL.")
(defvar *perf-ack* nil "perf's ack fifo, open for reading, or NIL.")
(defvar *perf-spans* (list :wall-ns 0 :cpu-ns 0 :consed 0 :spans 0)
  "What the MEASURED spans of this process have cost so far.")

(defun perf-clock-ns (clock)
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime clock)
    (+ (* seconds 1000000000) nanoseconds)))

(defun perf-control (word)
  "Tell perf stat to count (\"enable\") or stop (\"disable\"), and wait until
it has."
  (when *perf-ctl*
    (write-line word *perf-ctl*)
    (finish-output *perf-ctl*)
    (read-line *perf-ack*)))

(defun call-measured (thunk)
  "Run THUNK as one measured span, from a heap just collected in full."
  ;; The full GC first puts every span on the same footing: an empty nursery,
  ;; so a collection inside the span happens where the span's own allocation
  ;; puts it, and the same work collects the same way in both trees.
  (sb-ext:gc :full t)
  (let ((consed (sb-ext:get-bytes-consed))
        (wall (perf-clock-ns sb-unix:clock-monotonic))
        (cpu (perf-clock-ns sb-unix:clock-process-cputime-id)))
    (perf-control "enable")
    (multiple-value-prog1 (funcall thunk)
      (perf-control "disable")
      (incf (getf *perf-spans* :wall-ns) (- (perf-clock-ns sb-unix:clock-monotonic) wall))
      (incf (getf *perf-spans* :cpu-ns) (- (perf-clock-ns sb-unix:clock-process-cputime-id) cpu))
      (incf (getf *perf-spans* :consed) (- (sb-ext:get-bytes-consed) consed))
      (incf (getf *perf-spans* :spans)))))

(defmacro measured (&body body)
  `(call-measured (lambda () ,@body)))

;;; --- the TUI: keystrokes and paging through the real program loop ----------

(defparameter +perf-typed+
  "perf gate: every key a person types reaches the composer and paints a frame"
  "What the TUI workload types, one key press per batch.")

(defun perf-tui ()
  "A 120x40 shell over a 4000-line transcript: type a sentence into the
composer, then page up 20 times and back down, each key one batch through
HANDLE-MESSAGES, the event loop's own fold-then-paint, painting into a sink."
  (with-frame-model (model 120 40 :session-id "perf-tui")
    (seed-long-transcript model 4000)
    (let ((program (nlt:make-program model)))
      (setf (nlt::renderer-output-stream (nlt::program-renderer program)) (make-broadcast-stream))
      (nlt::handle-messages program (list (nlt:make-window-size-msg :width 120 :height 40)))
      (measured
        (loop for code across +perf-typed+
              do (nlt::handle-messages program (list (nlt:make-key-press-msg :code code))))
        (dotimes (i 20)
          (nlt::handle-messages program (list (nlt:make-key-press-msg :code :page-up))))
        (dotimes (i 20)
          (nlt::handle-messages program (list (nlt:make-key-press-msg :code :page-down))))))))

;;; --- the gateway: starting it, and turns streamed through it ---------------

(defun perf-boot ()
  "Start the gateway on a fresh store, five times: the organism's boot to a
gateway that accepts."
  (dotimes (i 5)
    (with-gateway-scratch (dir "perf-boot" :store t)
      (measured (nle:start-gateway :port 0 :operator-token "perf" :workspace-captures nil)))))

(defun perf-reply ()
  "One streamed answer: 200 content deltas, then the stop."
  (apply #'make-sse-stream
         (append (loop for i below 200 collect (chunk-json (format nil "word~d " i)))
                 (list (chunk-json "." :finish-reason "stop")))))

(defun perf-await (predicate seconds)
  (loop with deadline = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        until (funcall predicate)
        when (> (get-internal-real-time) deadline)
          do (error "perf: waited ~d s for a turn" seconds)
        do (sleep 0.0005)))

(defun perf-turns ()
  "Eight turns, one after another, each a start_turn a subscribed peer sends
the gateway, run on its worker against a provider lane whose HTTP answer is a
200-delta stream: the gateway's admission, the provider lane, the store's
facts and every item_delta published to the peer."
  ;; Only the transport is played: DEX:POST answers the canned stream, so
  ;; CALL-PROVIDER, the lane's SSE parse and everything above it are real.
  (with-saved-globals (nle::*provider* nle::*model* nle::*endpoint* nle::*api-key*)
    (setf nle::*provider* "openai-completions" nle::*model* "perf-model"
          nle::*endpoint* "http://127.0.0.1:9/v1" nle::*api-key* "perf")
    (with-gateway-session (port "perf-turns")
      (with-silent-peer (peer state :perf-peer)
        (with-stubbed-fdefinition (dex:post (url &rest args) (values (perf-reply) 200))
          (measured
            (dotimes (i 8)
              (let ((before (nlk::session-head-turn-id "perf-turns")))
                (%drive-start-turn peer "perf-turns" (format nil "perf-turn-~d" i)
                                   "model" "perf-model")
                (perf-await (lambda () (and (not (equal before (nlk::session-head-turn-id "perf-turns")))
                                            (not (nlk:active-turn-p "perf-turns"))))
                            60)))))))))

(defparameter +perf-workloads+
  '(("tui" . perf-tui) ("boot" . perf-boot) ("turns" . perf-turns))
  "Every workload by the name perf-gate.py asks for.")

(defun perf-run (name out &key ctl ack)
  "Run workload NAME and write what its spans cost to OUT, a plist; CTL and
ACK name perf stat's control and ack fifos when it runs under one."
  (let ((entry (assoc name +perf-workloads+ :test #'string=)))
    (unless entry (error "perf: no workload ~s" name))
    (flet ((run ()
             (funcall (cdr entry))
             (with-open-file (s out :direction :output :if-exists :supersede)
               (with-standard-io-syntax (prin1 *perf-spans* s) (terpri s)))))
      (if ctl
          (with-open-file (*perf-ctl* ctl :direction :output :if-exists :append)
            (with-open-file (*perf-ack* ack :direction :input)
              (run)))
          (run)))))
