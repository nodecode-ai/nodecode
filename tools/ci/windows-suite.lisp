;;; windows-suite.lisp --- the core suite on Windows, one result line per test.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Loaded by tools/ci/ci-windows-suite.ps1 in a fresh SBCL from the tree's
;;; root, with quicklisp under (user-homedir-pathname). `asdf:test-system'
;;; cannot run the suite on Windows: the test system stops at the first file
;;; whose reader meets a symbol this SBCL lacks (sb-posix:fork, ...), and every
;;; file after it is lost. So this loads the test files one by one, each a
;;; failure of its own, and runs every test in a thread of its own under a
;;; timeout, writing one line per test as it ends -- a crash keeps what ran.
;;;
;;; Environment:
;;;   WINSUITE_PHASE    compile: load the system and compile every test file
;;;                     into WINSUITE_FASL, then exit; run (default): load
;;;                     those fasls and run the shard.
;;;   WINSUITE_OUT      where results-TAG.tsv, load-TAG.txt, current-TAG.txt go.
;;;   WINSUITE_FASL     the test files' compiled output, shared by the shards.
;;;   WINSUITE_SHARD    I/N: run the tests whose place in the suite is I mod N.
;;;   WINSUITE_START    resume: skip this shard's tests up to and including the
;;;                     one named (the driver names the test a dead process ran).
;;;   WINSUITE_SKIP     a file of test names not to run (a test that hangs or
;;;                     kills the process); each gets a SKIP line.
;;;   WINSUITE_ONLY     a file of test names: run only those (a fix's proof).
;;;   WINSUITE_TIMEOUT  seconds one test may take (default 180).
;;;
;;; results-TAG.tsv: NAME TAB pass|FAIL|TIMEOUT|SKIP TAB SECONDS TAB FIRST-FAILURE,
;;; then SUITE-DONE.  load-TAG.txt: FILE-OK / FILE-PARTIAL / FILE-FAILED per
;;; test file, SHIM lines, THREAD-CRASH lines, TESTS, SUITE-DONE.

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (truename ".") asdf:*central-registry*)

(defpackage #:windows-suite (:use #:cl))
(in-package #:windows-suite)

(defun env (name &optional default)
  (let ((v (uiop:getenv name))) (if (and v (plusp (length v))) v default)))

(defvar *phase* (env "WINSUITE_PHASE" "run"))
(defvar *shard* (env "WINSUITE_SHARD" "0/1"))
(defvar *tag* (if (string= *phase* "compile") "compile" (substitute #\- #\/ *shard*)))
(defvar *out* (uiop:ensure-directory-pathname (env "WINSUITE_OUT" (namestring (truename ".")))))
(defvar *fasls* (uiop:ensure-directory-pathname
                 (env "WINSUITE_FASL" (namestring (merge-pathnames "windows-suite-fasl/" (uiop:temporary-directory))))))
(defvar *start* (env "WINSUITE_START"))

(defun out-file (fmt) (merge-pathnames (format nil fmt *tag*) *out*))

(ensure-directories-exist *out*)
(defvar *load-log* (open (out-file "load-~a.txt") :direction :output
                                                 :if-exists (if *start* :append :supersede)
                                                 :if-does-not-exist :create :external-format :utf-8))

(defun one-line (s &optional (max 400))
  (let ((s (substitute-if #\Space (lambda (c) (member c '(#\Tab #\Newline #\Return))) s)))
    (subseq s 0 (min max (length s)))))

(defun lg (fmt &rest args)
  (write-line (one-line (apply #'format nil fmt args) 2000) *load-log*)
  (finish-output *load-log*))

(asdf:load-system :nodecode)
(lg "SYSTEM-LOADED nodecode")

;;; A test file's reader dies on a symbol this SBCL does not export
;;; (sb-posix:fork) or a package Windows' build of a library never makes
;;; (dexador.backend.usocket: Windows' dexador is WinHTTP), and COMPILE-FILE
;;; turns that into an abort of the whole file. Export a placeholder for each
;;; such SB-* symbol and make each such dotted package first: the file then
;;; loads, and only the tests that reach the missing function go red, at run
;;; time, where they belong. Every SHIM line in load-TAG.txt is a test file
;;; that is not yet portable.
(defvar *shim-packages* '())

(defun token-char-p (c) (or (alphanumericp c) (find c "-*+%/<>=!?._")))

(defun shim-file (f)
  (let ((text (uiop:read-file-string f)))
    (loop for p = (position #\: text) then (position #\: text :start (1+ p))
          while p
          do (when (and (< 0 p (- (length text) 2)) (char/= (char text (1- p)) #\:))
               (let* ((dbl (char= (char text (1+ p)) #\:))
                      (s0 (if dbl (+ p 2) (1+ p)))
                      (b (or (position-if-not #'token-char-p text :end p :from-end t) -1))
                      (e (or (position-if-not #'token-char-p text :start s0) (length text)))
                      (pkg (string-upcase (subseq text (1+ b) p)))
                      (sym (string-upcase (subseq text s0 e))))
                 (when (and (plusp (length pkg)) (plusp (length sym)) (not (find-package pkg))
                            (or (find #\. pkg) (string= "SB-" pkg :end2 (min 3 (length pkg)))))
                   (make-package pkg :use '())
                   (push pkg *shim-packages*)
                   (lg "SHIM-PACKAGE ~a in ~a" pkg (enough-namestring f)))
                 (when (and (not dbl) (plusp (length sym)) (find-package pkg)
                            (or (and (> (length pkg) 3) (string= "SB-" pkg :end2 3))
                                (member pkg *shim-packages* :test #'string=)))
                   (multiple-value-bind (s status) (find-symbol sym pkg)
                     (declare (ignore s))
                     (unless (eq status :external)
                       (sb-ext:without-package-locks (export (intern sym pkg) pkg))
                       (lg "SHIM ~a:~a (~a) in ~a" pkg sym (or status "absent") (enough-namestring f))))))))))

(dolist (f (directory "test/**/*.lisp")) (shim-file f))

;;; The test files, one by one in the system's own order: one that cannot
;;; compile here costs its own tests and not every file after it. A reader
;;; error that offers CONTINUE takes it.
(let ((files (asdf:component-children (asdf:find-component :nodecode/test "test"))))
  (ensure-directories-exist *fasls*)
  (handler-bind ((reader-error
                   (lambda (c)
                     (let ((r (find-restart 'continue c)))
                       (lg "READFIX ~a" c)
                       (when r (invoke-restart r))))))
    (let ((uiop:*compile-file-failure-behaviour* :warn)
          (uiop:*compile-file-warnings-behaviour* :ignore))
      (loop for c in files for i from 0
            for name = (asdf:component-name c)
            for fasl = (merge-pathnames (format nil "f~3,'0d-~a.fasl" i (substitute-if #\- (complement #'alphanumericp) name))
                                        *fasls*)
            do (handler-case
                   (if (string= *phase* "compile")
                       (multiple-value-bind (out warnings-p failure-p)
                           (let ((*error-output* *standard-output*))
                             (compile-file (asdf:component-pathname c) :output-file fasl))
                         (declare (ignore warnings-p))
                         (cond ((null out) (lg "FILE-FAILED ~a no fasl" name))
                               (t (load out)
                                  (lg "~:[FILE-OK~;FILE-PARTIAL~] ~a" failure-p name))))
                       (if (probe-file fasl)
                           (progn (load fasl) (lg "FILE-OK ~a" name))
                           (lg "FILE-FAILED ~a no fasl" name)))
                 (error (e) (lg "FILE-FAILED ~a ~a" name e)))))))

(when (string= *phase* "compile")
  (lg "COMPILE-DONE")
  (sb-ext:exit :code 0 :abort t))

;;; A test's own worker thread that dies unhandled would end the whole run
;;; under --non-interactive. Log it against the running test and end only that
;;; thread.
(defvar *current-test* nil)
(defvar *main* sb-thread:*current-thread*)
(setf sb-ext:*invoke-debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (lg "THREAD-CRASH test=~a thread=~a: ~a" *current-test*
            (ignore-errors (sb-thread:thread-name sb-thread:*current-thread*))
            (or (ignore-errors (princ-to-string c)) "?"))
        (if (eq sb-thread:*current-thread* *main*)
            (sb-ext:exit :code 3 :abort t)
            (sb-thread:abort-thread))))

(defun read-names (var)
  (let ((f (env var)))
    (when (and f (probe-file f))
      (with-open-file (in f :external-format :utf-8)
        (loop for line = (read-line in nil) while line
              for name = (string-trim (list #\Space #\Tab #\Return #\Newline (code-char #xFEFF)) line)
              when (plusp (length name)) collect (string-upcase name))))))

(let* ((tests (symbol-value (uiop:find-symbol* :*tests* :nodecode.test)))
       (failures (uiop:find-symbol* :*failures* :nodecode.test))
       (slash (position #\/ *shard*))
       (idx (parse-integer *shard* :end slash))
       (cnt (parse-integer *shard* :start (1+ slash)))
       (timeout (parse-integer (env "WINSUITE_TIMEOUT" "180")))
       (skips (read-names "WINSUITE_SKIP"))
       (only (read-names "WINSUITE_ONLY"))
       (skipping (and *start* t))
       (out (open (out-file "results-~a.tsv") :direction :output
                                              :if-exists (if *start* :append :supersede)
                                              :if-does-not-exist :create :external-format :utf-8)))
  (lg "TESTS ~d shard ~d/~d timeout ~ds skip ~d" (length tests) idx cnt timeout (length skips))
  (flet ((line (name status secs why)
           (format out "~a~c~a~c~,2f~c~a~%" name #\Tab status #\Tab secs #\Tab (one-line why))
           (finish-output out)))
    (loop for (name . fn) in tests
          for i from 0
          for mine = (= (mod i cnt) idx)
          do (cond ((not mine))
                   ((and only (not (member (string name) only :test #'string=))))
                   (skipping (when (string-equal (string name) *start*) (setf skipping nil)))
                   ((member (string name) skips :test #'string=)
                    (line name "SKIP" 0 "listed in WINSUITE_SKIP"))
                   (t
                    (setf *current-test* name)
                    (with-open-file (cur (out-file "current-~a.txt") :direction :output :if-exists :supersede)
                      (write-string (string name) cur))
                    (let* ((t0 (get-internal-real-time))
                           (box (list :none))
                           (th (sb-thread:make-thread
                                (lambda ()
                                  (progv (list failures) (list '())
                                    (handler-case (funcall fn)
                                      (error (e) (push (format nil "unhandled ~a" e) (symbol-value failures))))
                                    (setf (car box) (reverse (symbol-value failures)))))
                                :name (string name))))
                      (sb-thread:join-thread th :timeout timeout :default :timeout)
                      (let ((res (car box))
                            (secs (/ (- (get-internal-real-time) t0) internal-time-units-per-second 1.0)))
                        (when (eq res :none) (ignore-errors (sb-thread:terminate-thread th)))
                        (line name (cond ((eq res :none) "TIMEOUT") (res "FAIL") (t "pass")) secs
                              (if (consp res) (princ-to-string (first res)) "")))))))
    (write-line "SUITE-DONE" out)
    (close out)
    (lg "SUITE-DONE")))
(sb-ext:exit :code 0 :abort t)
