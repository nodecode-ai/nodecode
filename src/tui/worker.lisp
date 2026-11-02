;;;; worker.lisp --- an unhandled worker failure ends its thread, not the harness.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.evolved)

(defvar *worker-error-hook* nil
  "The process debugger boundary installed by INSTALL-WORKER-ERROR-GUARD.")

(defun install-worker-error-guard ()
  "Keep unhandled worker failures local; main-thread failures remain fatal.
Ordinary handlers, restarts and warnings run before this debugger boundary.
The failure and its backtrace are debug-only, never user notifications."
  (let ((previous (sb-ext:symbol-global-value 'sb-ext:*invoke-debugger-hook*)))
    (unless (and *worker-error-hook* (eq previous *worker-error-hook*))
      (setf *worker-error-hook*
            (lambda (condition hook)
              (if (sb-thread:main-thread-p)
                  (if previous
                      (funcall previous condition hook)
                      (sb-ext:exit :code 1 :abort t))
                  ;; A cleanup or diagnostic that fails while unwinding must
                  ;; still stop only this worker, without recursive reporting.
                  (let ((sb-ext:*invoke-debugger-hook*
                          (lambda-ignoring (condition hook)
                            (sb-thread:abort-thread))))
                    (with-handlers ((serious-condition () nil))
                      (format sb-sys:*stderr* "~&worker ~s stopped (~s): ~a~%"
                              (sb-thread:thread-name sb-thread:*current-thread*)
                              (type-of condition) (nlk:safe-condition-report condition))
                      (sb-debug:print-backtrace :stream sb-sys:*stderr*
                                                :print-thread t :emergency-best-effort t)
                      (finish-output sb-sys:*stderr*))
                    (sb-thread:abort-thread))))
            (sb-ext:symbol-global-value 'sb-ext:*invoke-debugger-hook*) *worker-error-hook*))))
