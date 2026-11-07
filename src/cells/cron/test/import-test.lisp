;;;; import-test.lisp --- a home's cron jobs, landed through this folder.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest cron-cell-lands-an-imported-job (with-import-runtime (home box))
  ;; The import (the core's src/src/import/) plans a home's prompt jobs and
  ;; lands each through ADD-JOB. The core suite never loads a cell, so the
  ;; landing is proved in this folder's lane, over the import's own box.
  (make-hermes-fixture box)
  (let ((nodecode-cron::*ticker* nil))
    (nlk:with-cleanup ((nodecode-cron::stop-cell))
      (nodecode-cron:start-cell (nlk:json-object "cron" (nlk:make-json-object)))
      (with-temp-store ()
        (let ((plan (nik:apply-plan (hermes-plan home :only "cron"))))
          (is (zerop (cdr (assoc "error" (nik::summary-counts plan) :test #'string=))))
          (is (= 2 (length nodecode-cron::*jobs*))))))))
