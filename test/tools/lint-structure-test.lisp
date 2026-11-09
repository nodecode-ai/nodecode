;;;; lint-structure-test.lisp --- the structural lint, run as `just lint' runs it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; tools/check/lint-structure.lisp is a script: loading it runs MAIN and
;;;; exits. So each file here goes through a child SBCL with --script, the
;;;; way the justfile and the cells repository's CI call it, and the gate
;;;; reads the child's verdict and exit status.

(in-package #:nodecode.test)

(defun lint-structure-verdict (text)
  "Lint TEXT as one .lisp file: (values OUTPUT STATUS)."
  (let ((path (temp-path "lint" "lisp")))
    (nlk:with-cleanup ((when (probe-file path) (delete-file path)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-string text out))
      (multiple-value-bind (out err status)
          (nlk:run-bounded (list (namestring sb-ext:*runtime-pathname*) "--script"
                                 (namestring (asdf:system-relative-pathname
                                              :nodecode "tools/check/lint-structure.lisp"))
                                 (namestring path))
                           :seconds 60)
        (values (concatenate 'string out err) status)))))

(deftest lint-fails-a-defun-swallowed-by-the-one-above ()
  ;; A WHEN that never closes keeps API-KEY open, and the reader still reads
  ;; the file because a paren at its end makes up the count. The lines at
  ;; column 2 and then column 0 matched the deeper-then-shallower shape the
  ;; DEF- exemption admits, so it passed while API-KEY returned the symbol
  ;; AWAIT-READY and AWAIT-READY stayed undefined until API-KEY ran.
  (multiple-value-bind (output status)
      (lint-structure-verdict
       (format nil "(defun api-key ()~%  \"The key.\"~%  (let ((key (setting)))~%    (when (zerop (length key))~%      (setf key (fallback)))~%    key)~%~%(defun call-directory ()~%  (setting :directory))~%~%(defun await-ready (deadline)~%  (wait deadline)))~%"))
    (is (eql 1 status) "the file fails")
    (is (search "(DEFUN at line 1) is still open where a top-level form starts" output))
    (is (search "line 8 starts at column 0" output) "the first swallowed DEFUN is named")
    (is (search "line 11 starts at column 0" output) "and the second")))

(deftest lint-fails-a-deftest-swallowed-under-any-head ()
  ;; The same at a head with no exemption at all, and in a list a single
  ;; line starts: column 0 is never inside an open list.
  (multiple-value-bind (output status)
      (lint-structure-verdict
       (format nil "(deftest one () (with-store ()~%    (is (ok)))~%(deftest two ()~%  (is (ok))))~%(list a~%(b))~%"))
    (is (eql 1 status) "the file fails")
    (is (search "(DEFTEST at line 1) is still open where a top-level form starts" output))
    (is (search "line 3 starts at column 0" output))
    (is (search "(LIST at line 5) is still open where a top-level form starts" output))
    (is (search "line 6 starts at column 0" output))))

(deftest lint-passes-top-level-forms-and-the-distinguished-shape ()
  ;; Column 0 between lists is where top-level forms belong, and a deeper
  ;; protected form before shallower clauses is HANDLER-CASE's own layout.
  (multiple-value-bind (output status)
      (lint-structure-verdict
       (format nil "(defun a ()~%  \"Doc.~%Continued at column 0 inside the string.\"~%  (handler-case~%      (risky)~%    (error () nil)))~%~%(defun b () (a))~%"))
    (is (eql 0 status) output)
    (is (search "1 file checked, 0 failed" output))))
