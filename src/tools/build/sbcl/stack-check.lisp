;;;; stack-check.lisp --- the patched SBCL's own proof, run by the macOS lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;;   sbcl --control-stack-size 8MB --non-interactive --no-sysinit --no-userinit \
;;;;        --load src/tools/build/sbcl/stack-check.lisp
;;;;
;;;; Two claims of patches/0002, on a stack that grows up (arm64): a collection
;;;; leaves a thread's unused control stack unresident, and threads recursing
;;;; deep while collections run still compute exactly -- the scrub's promise
;;;; that nothing stale waits above a stack pointer. Exits 1 on either.

(defun fail (control &rest args)
  (format t "~&stack-check: FAIL ~?~%" control args)
  (sb-ext:exit :code 1 :abort t))

(unless (member :stack-grows-downward-not-upward sb-impl:+internal-features+)
  (format t "~&stack-check: control stacks grow up here~%"))

(defun page-size ()
  (sb-alien:alien-funcall (sb-alien:extern-alien "getpagesize" (function sb-alien:int))))

(defun resident-bytes (start end &aux (page (page-size)))
  "Bytes of [START, END) resident now, by mincore."
  (let ((pages (make-array (ceiling (- end start) page) :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (pages)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "mincore" (function sb-alien:int sb-alien:unsigned-long
                                                  sb-alien:unsigned-long
                                                  sb-alien:system-area-pointer))
       start (- end start) (sb-sys:vector-sap pages)))
    (* page (count-if #'oddp pages))))

(defun stack-resident-bytes (thread)
  (resident-bytes (sb-thread::thread-control-stack-start thread)
                  (sb-thread::thread-control-stack-end thread)))

(defun dive (limit &aux (thread sb-thread:*current-thread*))
  "Recurse until this thread's control stack holds more than LIMIT bytes."
  (declare (optimize (debug 2) (speed 0)))
  (let ((sp (sb-sys:sap-int (sb-vm::current-sp))))
    (if (> (if (member :stack-grows-downward-not-upward sb-impl:+internal-features+)
               (- (sb-thread::thread-control-stack-end thread) sp)
               (- sp (sb-thread::thread-control-stack-start thread)))
           limit)
        0
        (1+ (dive limit)))))

;;; 1. Sixteen threads go 2 MB deep, come back, and park; a full collection
;;; stops them all. The stock scrub writes each one's whole stack.
(let* ((count 16)
       (dove (sb-thread:make-semaphore))
       (leave (sb-thread:make-semaphore))
       (threads (loop repeat count
                      collect (sb-thread:make-thread
                               (lambda ()
                                 (dive (* 2 1024 1024))
                                 (sb-thread:signal-semaphore dove)
                                 (sb-thread:wait-on-semaphore leave))))))
  (sb-thread:wait-on-semaphore dove :n count)
  (sb-ext:gc :full t)
  (let ((held (reduce #'+ threads :key #'stack-resident-bytes))
        (span (- (sb-thread::thread-control-stack-end (first threads))
                 (sb-thread::thread-control-stack-start (first threads)))))
    (sb-thread:signal-semaphore leave count)
    (mapc #'sb-thread:join-thread threads)
    (format t "~&stack-check: ~d threads 2 MB deep on ~,1f MB stacks hold ~,2f MB after a collection~%"
            count (/ span 1048576.0) (/ held 1048576.0))
    (unless (< held (* count 256 1024))
      (fail "the scrub left ~,2f MB of stack resident" (/ held 1048576.0)))))

;;; 2. Eight threads build lists by recursion, a frame's objects live across
;;; each call, under a small nursery and a full collection every few
;;; milliseconds: stale words above a stack pointer would be scavenged as
;;; roots, and the lists would come back wrong or the image would die.
(defun build (n)
  (declare (optimize (debug 2) (speed 0)))
  (if (zerop n)
      '()
      (let* ((box (list n (make-array 3 :initial-element n)))
             (rest (build (1- n))))
        (cons (+ (first box) (- (aref (second box) 0) n)) rest))))

(setf (sb-ext:bytes-consed-between-gcs) (* 2 1024 1024))
(let* ((wrong (list 0))
       (workers (loop repeat 8
                      collect (sb-thread:make-thread
                               (lambda ()
                                 (loop repeat 150
                                       for n = (+ 500 (random 20000))
                                       for list = (build n)
                                       unless (and (= (length list) n)
                                                   (= (reduce #'+ list) (/ (* n (1+ n)) 2)))
                                         do (sb-ext:atomic-incf (car wrong))))))))
  (loop while (some #'sb-thread:thread-alive-p workers)
        do (sb-ext:gc :full t)
           (sleep 0.005))
  (mapc #'sb-thread:join-thread workers)
  (format t "~&stack-check: 1,200 deep builds under collections, ~d wrong~%" (car wrong))
  (unless (zerop (car wrong))
    (fail "~d builds came back wrong" (car wrong))))

(format t "~&stack-check: ok~%")
