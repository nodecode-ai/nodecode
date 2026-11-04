;;;; heap-image.lisp --- a live heap saved without ending it. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A saved image (images.lisp) is the running organism's heap kept as it
;;;; stands. SAVE-LISP-AND-DIE writes one only from a process that has a
;;;; single thread left, and ends that process: the organism that asked must
;;;; go on. So the save runs where the serve's owner parks (SERVE-NEXT
;;;; :IMAGE), after the gateway stopped and the store was let go, in steps:
;;;;   - QUIESCE: timers disarmed, every thread still running stopped and
;;;;     joined -- a cell's :START brings its own back at boot, as it does at
;;;;     every boot;
;;;;   - FORGET: a variable that held one of those threads holds NIL, so the
;;;;     `start unless running' idiom starts it again in the image;
;;;;   - FORK: with one thread left SBCL forks, the child dumps the heap
;;;;     marked a saved image (*SAVED-IMAGE*), the parent waits on it;
;;;;   - BECOME: the parent execs the image just saved -- same pid, argv and
;;;;     environ -- so the organism goes on from the snapshot, every attached
;;;;     shell dials it again as after a restart, and every save has proved
;;;;     its image boots. The store's lease is held across all of it and the
;;;;     exec (HOLD-STORE-LEASE): a shell that saw the lock free while the
;;;;     heap was written would take the organism over.
;;;; A saved image boots past the folder (BOOT-CELLS).

(in-package #:nodecode.kernel)

(defparameter *image-quiesce-seconds* 5
  "How long a save waits on each thread it stopped before it gives up.")

(defun quiesce-for-image (&optional (seconds *image-quiesce-seconds*))
  "Leave this thread the only one: SBCL's timers disarmed (DISARM-TIMERS) and
every other thread stopped and joined. => the names of the threads that
would not stop, NIL when none is left."
  ;; Called with the gateway stopped (STOP-GATEWAY: cells, execs, the
  ;; listener), so what is left are the threads nobody stops -- a keeper's
  ;; sleep loop, a cell that starts a thread and declares no :STOP. Each is
  ;; ended the way a process exit ends it, and one that will not end within
  ;; SECONDS refuses the save: a fork beside it is not allowed.
  (disarm-timers)
  (let ((others (remove sb-thread:*current-thread* (sb-thread:list-all-threads))))
    (dolist (thread others)
      (ignore-errors (sb-thread:terminate-thread thread)))
    (dolist (thread others)
      (ignore-errors (sb-thread:join-thread thread :default nil :timeout seconds)))
    (loop for thread in (sb-thread:list-all-threads)
          unless (eq thread sb-thread:*current-thread*)
            collect (or (sb-thread:thread-name thread) "a thread with no name"))))

(defun forget-dead-threads (&optional (packages (list-all-packages)))
  "Set every global variable of PACKAGES that holds a thread no longer alive
to NIL, so code that starts its thread unless one is held starts it again in
the image. SBCL's own packages are passed over. => how many were set."
  ;; SPAWN answers bordeaux-threads' wrapper, so both kinds are a thread.
  (flet ((dead-thread-p (value)
           (typecase value
             (bt2:thread (not (bt2:thread-alive-p value)))
             (sb-thread:thread (and (not (eq value sb-thread:*current-thread*))
                                    (not (sb-thread:thread-alive-p value)))))))
    (let ((count 0))
      (dolist (package packages count)
        (unless (uiop:string-prefix-p "SB-" (package-name package))
          (do-symbols (symbol package)
            (when (and (eq (symbol-package symbol) package)
                       (boundp symbol)
                       (not (constantp symbol))
                       (dead-thread-p (symbol-value symbol)))
              (when (ignore-errors (setf (symbol-value symbol) nil) t)
                (incf count)))))))))

(defun dump-this-heap (path saved-image key)
  "In the fork that saves: mark this heap the saved image SAVED-IMAGE (a
plist, *SAVED-IMAGE*) of folder KEY and dump it as PATH. Never returns."
  ;; Exactly warm-dump's seam (WARM-DUMP-MAIN): uiop's dump hooks, the
  ;; restored flag reset, MAIN behind a closure, runtime options kept.
  (handler-case
      (progn
        (setf *saved-image* saved-image
              *image-folder-key* key
              *image-origin* (or *image-origin* sb-ext:*runtime-pathname*)
              ;; a stream on a descriptor the exec closes: the boot holds the origin again
              *origin-hold* nil)
        (uiop:call-image-dump-hook)
        (setf uiop/image::*image-restored-p* nil)
        (sb-ext:save-lisp-and-die
         path
         :toplevel (lambda ()
                     (uiop:restore-image
                      :entry-point (uiop:ensure-function 'nodecode.evolved::main)))
         :executable t
         :save-runtime-options t))
    (serious-condition (condition)
      (ignore-errors (format *error-output* "~&snapshot: the dump failed: ~a~%" condition))
      (sb-ext:exit :code 1 :abort t))))

(defun fork-and-dump (path saved-image key)
  "Dump this organism's heap as PATH, the saved image SAVED-IMAGE of folder
KEY, by a fork, this process going on. => T once PATH is written whole, or
(values NIL TEXT) saying why not. Leaves this process with one thread: call
it where an exec follows (BECOME-SAVED-IMAGE)."
  #+win32 (declare (ignore path saved-image key))
  #+win32 (values nil "Windows has no fork: snapshots are taken on Linux and macOS")
  #-win32
  ;; The dump copies this process's runtime from the file it runs from: a
  ;; heap that file was, since deleted, could not be saved again -- said
  ;; before anything is stopped.
  (unless (probe-file sb-ext:*runtime-pathname*)
    (return-from fork-and-dump
      (values nil (format nil "~a, which this organism runs from, is gone: switch to fresh from files or another snapshot, then take one"
                          sb-ext:*runtime-pathname*))))
  #-win32
  (let ((stuck (quiesce-for-image)))
    (when stuck
      (return-from fork-and-dump (values nil (format nil "~{~a~^, ~} would not stop" stuck))))
    (forget-dead-threads)
    (finish-output) (finish-output *error-output*)
    (let ((pid (handler-case (sb-posix:fork)
                 (error (condition)
                   (return-from fork-and-dump
                     (values nil (format nil "fork refused: ~a" condition)))))))
      (when (zerop pid)
        (dump-this-heap path saved-image key))
      (let ((status (nth-value 1 (sb-posix:waitpid pid 0))))
        (cond ((not (and (sb-posix:wifexited status) (zerop (sb-posix:wexitstatus status))))
               (values nil (if (sb-posix:wifexited status)
                               (format nil "the dump exited with ~d" (sb-posix:wexitstatus status))
                               (format nil "the dump died on signal ~d" (sb-posix:wtermsig status)))))
              ((not (image-complete-p path))
               (values nil "the dump left an incomplete snapshot"))
              (t t))))))

(defun processes-running (path &aux (target (ignore-errors (namestring (truename path)))))
  "The pids of the live processes whose executable is the file at PATH --
a saved heap one of them booted. Linux reads /proc; elsewhere NIL."
  #-linux (declare (ignore target))
  #+linux
  (when target
    (loop for name in (ignore-errors (directory-names "/proc"))
          for pid = (parse-integer name :junk-allowed t)
          when (and pid (equal target (ignore-errors (sb-posix:readlink (format nil "/proc/~d/exe" pid)))))
            collect pid)))

(defun close-inherited-descriptors (&optional keep)
  "Close every descriptor above stderr but KEEP: an exec keeps them, and the
store's lease or a page's connection would ride into the successor."
  (dolist (name (directory-names #+linux "/proc/self/fd" #-linux "/dev/fd"))
    (let ((fd (parse-integer name :junk-allowed t)))
      (when (and fd (> fd 2) (not (eql fd keep))) (ignore-errors (sb-posix:close fd))))))

(defun exec-into-image (image &key lease (argv sb-ext:*posix-argv*))
  "Replace this process with IMAGE on ARGV, every descriptor above stderr
closed but LEASE, the store's lock, which the image adopts. Answers only the
kernel's refusal, as text."
  ;; An fcntl lock is the process's and outlives an exec while a descriptor
  ;; on its file stays open: the lease's, close-on-exec cleared. The
  ;; successor -- a heap, or the installed binary -- is told which descriptor
  ;; it is (HAND-LEASE-ACROSS-EXEC, ADOPT-HANDED-LEASE), so its election reads
  ;; it as its own (STORE-LEASE-HELD-ELSEWHERE-P) and its store takes it as
  ;; the lease. Never from the heap's own memory of it: a heap booted later
  ;; remembers a descriptor of a process that is gone.
  #-win32 (when lease (ignore-errors (hand-lease-across-exec lease)))
  (close-inherited-descriptors lease)
  (exec-program (uiop:native-namestring image) argv))
