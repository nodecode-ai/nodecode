;;;; worker-test.lisp --- unhandled worker failures never take the harness down.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(defun worker-guard-child (body &key (guard t))
  "BODY in a disposable, debugger-disabled SBCL; never fail the test image."
  (let ((path (merge-pathnames "probe.lisp" (temp-path "worker-guard"))))
    (nlk:with-cleanup ((when (probe-file path) (delete-file path)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede)
        (format out "(require :asdf)~%(push ~s asdf:*central-registry*)~%(asdf:load-system :nodecode)~%"
                (namestring (asdf:system-source-directory :nodecode)))
        (when guard (write-line "(nle::install-worker-error-guard)" out))
        (write-line body out))
      (nlk:run-bounded (list (namestring sb-ext:*runtime-pathname*)
                             "--noinform" "--non-interactive" "--load" (namestring path))
                       :seconds 30))))

(defmacro deftest-worker (name source &body checks)
  "A DEFTEST running SOURCE through WORKER-GUARD-CHILD, CHECKS over its OUT, ERR and STATUS."
  `(deftest ,name ()
     (multiple-value-bind (out err status) (worker-guard-child ,source) ,@checks)))

(deftest-worker an-unhandled-raw-worker-error-keeps-the-process-and-unwinds
    "(let* ((cleaned nil) (worker (sb-thread:make-thread (lambda () (unwind-protect (funcall (intern \"T169-MISSING-FUNCTION\" :cl-user)) (setf cleaned t))) :name \"raw-proof\"))) (sb-thread:join-thread worker :default :failed) (assert cleaned) (format t \"WORKER-SURVIVED cleanup=~s~%\" cleaned))"
  (is (eql 0 status) "a missing function in a raw proof thread does not exit its harness")
  (is (search "WORKER-SURVIVED cleanup=T" out) "the parent runs after the worker's unwind")
  (is (and (search "raw-proof" err) (search "UNDEFINED-FUNCTION" err)) "the failed worker is named and typed, not silently swallowed"))

(deftest-worker worker-guard-preserves-handlers-restarts-warnings-and-values
    "(let ((worker (nlk:spawn \"handled-proof\" (warn \"warning remains nonfatal\") (values (handler-case (error \"handled\") (error () :handled)) (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'use-value 7)))) (restart-case (error \"restartable\") (use-value (v) v))) 9)))) (assert (equal (multiple-value-list (sb-thread:join-thread (bt2:thread-native-thread worker))) '(:handled 7 9))) (format t \"HANDLERS-SURVIVED~%\"))"
  (is (eql 0 status) "existing handlers and restarts retain authority before the debugger")
  (is (search "HANDLERS-SURVIVED" out) "warnings do not kill a worker and all return values survive")
  (is (search "warning remains nonfatal" err) "warnings still report normally"))

(deftest-worker an-unhandled-worker-serious-condition-and-broken-report-stay-local
    "(define-condition broken-worker-report (serious-condition) () (:report (lambda (c s) (declare (ignore c s)) (error \"broken report\")))) (dotimes (i 2) (let ((worker (sb-thread:make-thread (lambda () (if (zerop i) (error 'broken-worker-report) (sb-ext:with-timeout 0.02 (sleep 1)))) :name \"serious-proof\"))) (sb-thread:join-thread worker :default :failed))) (format t \"SERIOUS-SURVIVED~%\")"
  (is (eql 0 status) "non-ERROR serious conditions and broken reports cannot escape the guard")
  (is (search "SERIOUS-SURVIVED" out) "both failing workers end without ending the process")
  (is (and (search "BROKEN-WORKER-REPORT" err) (search "TIMEOUT" err)) "both failures remain diagnosed"))

(deftest-worker worker-guard-keeps-main-thread-failures-fatal
    "(error \"fatal main-thread proof\") (format t \"MAIN-SHOULD-NOT-SURVIVE~%\")"
  (is (and (integerp status) (not (zerop status))) "a main-thread failure still exits nonzero")
  (is (not (search "MAIN-SHOULD-NOT-SURVIVE" out)) "the guard never resumes a failing main thread")
  (is (search "fatal main-thread proof" err) "the original fatal report is preserved"))

(deftest-worker an-unhandled-worker-failure-is-debug-only-not-a-user-notice
    "(setf (fdefinition 'nle:notice) (lambda (&rest args) (declare (ignore args)) (format t \"RAW-WORKER-NOTICE\"))) (let ((worker (sb-thread:make-thread (lambda () (error \"debug-only worker failure\")) :name \"log-only-proof\"))) (sb-thread:join-thread worker :default :isolated)) (format t \"LOG-ONLY-SURVIVED~%\")"
  (is (eql 0 status) "the worker still unwinds without closing the harness")
  (is (search "LOG-ONLY-SURVIVED" out) "the parent remains live")
  (is (not (search "RAW-WORKER-NOTICE" out)) "no raw worker diagnostic is broadcast to user notifications")
  (is (search "debug-only worker failure" err) "the actual worker failure remains in its debug log"))
