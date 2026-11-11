;;;; load-signal-check.lisp --- the patched SBCL's proof for patches/0007,
;;;; run by the macOS lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;;   sbcl --non-interactive --no-sysinit --no-userinit \
;;;;        --load tools/build/sbcl/load-signal-check.lisp
;;;;
;;;; A fasl of 400 functions loads 300 times while another thread sends the
;;;; loading thread SIGCHLD every 0.2 ms. Each code object is copied into
;;;; dynamic space with the thread's JIT pages writable and so not executable
;;;; to it (jit_copy_code_insts); a handler run inside that window faults on
;;;; its first instruction and the runtime dies with "misuse of mprotect() on
;;;; dynamic space". The patched runtime holds the signals until the pages
;;;; are executable again. Exits 0 with a count, 1 when no signal was handled
;;;; (the check proved nothing), and an unpatched Apple silicon runtime dies
;;;; in lose() with exit 1. Elsewhere it passes: no other platform has the
;;;; window.

(defun fail (control &rest args)
  (format t "~&load-signal-check: FAIL ~?~%" control args)
  (sb-ext:exit :code 1 :abort t))

(defvar *dir*
  (let ((tmp (or (sb-ext:posix-getenv "TMPDIR") "/tmp/")))
    (format nil "~a~:[/~;~]load-signal-check-~d/"
            tmp (char= (char tmp (1- (length tmp))) #\/)
            (sb-unix:unix-getpid))))
(ensure-directories-exist *dir*)

(defvar *fasl*
  (let ((source (concatenate 'string *dir* "many.lisp")))
    (with-open-file (out source :direction :output :if-exists :supersede)
      (dotimes (i 400)
        (format out "(defun f~d (x) (list x ~d (lambda (y) (+ y ~d)) (lambda (z) (* z ~d))))~%"
                i i i i)))
    (let ((*standard-output* (make-broadcast-stream))
          (*error-output* (make-broadcast-stream)))
      (or (compile-file source) (fail "~a did not compile" source)))))

(defvar *loader* sb-thread:*current-thread*)
(defvar *done* nil)
(defvar *sent* 0)
(defvar *handled* 0)

(sb-sys:enable-interrupt sb-unix:sigchld
                        (lambda (&rest args) (declare (ignore args)) (incf *handled*)))

(defun signal-loader ()
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "pthread_kill"
                          (function sb-alien:int sb-alien:unsigned sb-alien:int))
   (sb-thread::thread-os-thread *loader*) sb-unix:sigchld))

(let ((sender (sb-thread:make-thread
               (lambda ()
                 (loop until *done*
                       do (signal-loader)
                          (incf *sent*)
                          (sleep 0.0002)))
               :name "load-signal-check sender"))
      (start (get-internal-real-time)))
  (unwind-protect
       (dotimes (i 300)
         (load *fasl*))
    (setf *done* t)
    (sb-thread:join-thread sender :default nil))
  (sb-ext:delete-directory *dir* :recursive t)
  (when (zerop *handled*)
    (fail "no SIGCHLD reached its handler (~d sent): the check proved nothing" *sent*))
  (format t "~&load-signal-check: 300 loads of 400 functions in ~,1fs, ~d signals handled~%"
          (/ (- (get-internal-real-time) start) internal-time-units-per-second)
          *handled*))
