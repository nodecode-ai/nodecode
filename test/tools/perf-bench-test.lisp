;;;; perf-bench-test.lisp --- the Perf gate's workloads, run as perf-gate.py runs them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; tools/check/perf-bench.lisp is loaded by perf-gate.py into a fresh SBCL
;;;; that calls PERF-RUN from its command line, outside RUN-ALL. So the
;;;; posture a test gets from the suite is one PERF-RUN has to give itself,
;;;; and the gate here sets this host's state the way a Perf process finds it.

(in-package #:nodecode.test)

;;; The Perf gate runs under perf stat, which only Linux has, and
;;; perf-bench.lisp reads its clocks through SB-UNIX:CLOCK-GETTIME, which a
;;; Windows SBCL lacks: on Windows there is no Perf process to hold.

#-win32
(defun load-perf-bench ()
  "Load tools/check/perf-bench.lisp, as perf-gate.py does."
  (load (asdf:system-relative-pathname :nodecode "tools/check/perf-bench.lisp")))

#-win32
(deftest perf-run-keeps-the-turns-workload-off-the-models-catalog ()
  ;; CI's catalog cache under PERF_HOME was days past its TTL, so the turns
  ;; workload's first turn started a models.dev fetch inside its measured
  ;; span and exited with it still in flight: the turns numbers carried a
  ;; download, and the cache it never wrote stayed stale for the next run.
  (load-perf-bench)
  (with-temp-file (path :contents "{}")
    (age-file path (* 2 nle::*models-catalog-ttl-seconds*))
    (with-temp-file (out :type "sexp")
      ;; SETF, as a Perf process has them: the turn runs on a gateway worker.
      (with-saved-globals ((nle::*models-catalog-path* path)
                           ;; nothing listens there, so a fetch fails at once
                           (nle::*models-catalog-url* "http://127.0.0.1:1/api.json")
                           (nle::*models-catalog-fetch* nil)
                           (nle::*models-catalog-memo* nil)
                           (nle::*models-catalog-probe* nil))
        (unwind-protect (funcall 'perf-run "turns" (namestring out))
          ;; PERF-RUN gives a Perf process SIGUSR1's backtraces; this
          ;; suite process keeps the runtime's own.
          (sb-sys:enable-interrupt sb-unix:sigusr1 :default))
        (is (null nle::*models-catalog-fetch*) "the workload started no catalog fetch")
        (is (search ":SPANS 1" (uiop:read-file-string out)) "and its turns were measured")
        (let ((url nle::*models-catalog-url*))
          (is (equal "http://127.0.0.1:1/api.json" url) "the host's own catalog URL is back"))))))
