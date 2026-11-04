;;;; heap-image-test.lisp --- the live save's pieces (heap-image.lisp, image-save.lisp, /image).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermetic. The fork and the dump end the process's threads, so they are
;;;; proved end to end by the packaged binary in a real shell, outside this
;;;; suite; here: what a dead thread leaves in the heap, the store's lease
;;;; handed across the exec, the boot's word on a moved folder, and /image's
;;;; words.

(in-package #:nodecode.test)

(deftest a-dead-thread-a-global-holds-is-forgotten-for-the-image ()
  (let* ((package (make-package (format nil "HEAP-IMAGE-TEST-~d" (random 1000000)) :use '()))
         (dead (intern "*DEAD*" package))
         (wrapped (intern "*WRAPPED*" package))
         (live (intern "*LIVE*" package))
         (gate (sb-thread:make-semaphore)))
    (unwind-protect
         (progn
           (setf (symbol-value dead) (sb-thread:make-thread (lambda () nil))
                 (symbol-value wrapped) (bt2:make-thread (lambda () nil))
                 (symbol-value live) (sb-thread:make-thread (lambda () (sb-thread:wait-on-semaphore gate))))
           (sb-thread:join-thread (symbol-value dead))
           (bt2:join-thread (symbol-value wrapped))
           (is (eql 2 (nlk::forget-dead-threads (list package))) "the two dead ones, and nothing else")
           (is (null (symbol-value dead)) "an SBCL thread that ended reads NIL")
           (is (null (symbol-value wrapped)) "so does bordeaux-threads' wrapper, SPAWN's answer")
           (is (sb-thread:thread-alive-p (symbol-value live)) "a live one is left alone"))
      (sb-thread:signal-semaphore gate)
      (delete-package package))))

(deftest a-lease-held-across-the-exec-is-the-reopened-stores-lease (with-temp-store (store))
  (let ((path (nlk::store-path store)))
    (nlk:close-store)
    (is (not (nlk::store-lease-held-p path)) "closed, the lock is free")
    (let ((lock (nlk::hold-store-lease path)))
      (is lock "taken back with no store open")
      (is (nlk::store-lease-mine-p path) "this process holds it")
      (is (not (nlk::store-lease-held-elsewhere-p path)) "an election here reads it as its own")
      (is (eql (sb-posix:getpid) (nlk::read-store-holder path)) "the sidecar names this process")
      (let ((reopened (nlk:open-store :path path)))
        (is (eql lock (nlk::store-lock-fd reopened)) "the store that opens adopts the held lock")))))

(deftest a-saved-heap-says-the-folder-moved-and-never-hands-back ()
  (with-stubbed-fdefinitions ((nlk::folder-key () "key-now"))
    (with-saved-globals ((nlk::*cells-directory* (uiop:temporary-directory)))
      (with-saved-globals ((nlk::*saved-image* (list :name "before" :folder-key "key-then")))
        (let ((drift (nlk::saved-image-drift)))
          (is (search "image before" drift) "names the image")
          (is (search "runs as it was saved" drift) "and says the heap stays")))
      (with-saved-globals ((nlk::*saved-image* (list :name "before" :folder-key "key-now")))
        (is (null (nlk::saved-image-drift)) "an unmoved folder says nothing"))
      (with-saved-globals ((nlk::*saved-image* nil))
        (is (null (nlk::saved-image-drift)) "no saved heap, no word")))))

(deftest slash-image-takes-a-verb-and-one-name ()
  (is (equal '("list" nil) (nle::image-words "")) "bare lists")
  (is (equal '("save" "demo") (nle::image-words "save demo")))
  (is (equal '("restore" "demo") (nle::image-words "RESTORE demo")) "the verb in any case")
  (is (equal '("delete" "x") (nle::image-words "delete x")))
  (dolist (args '("save" "save a b" "boot demo" "list demo"))
    (is (null (nle::image-words args)) args))
  (is (search "Usage" (nle::headless-image "save a b" nil)) "a room is told the usage"))

(deftest a-shell-says-the-outage-it-asked-for-until-its-deadline ()
  (with-saved-globals ((nle::*expected-outage* nil))
    (is (null (nle::expected-outage-text)) "none asked")
    (setf nle::*expected-outage* (cons "image: keeping" (+ (get-internal-real-time) internal-time-units-per-second)))
    (is (equal "image: keeping" (nle::expected-outage-text)) "inside its deadline")
    (setf nle::*expected-outage* (cons "image: keeping" (1- (get-internal-real-time))))
    (is (null (nle::expected-outage-text)) "past it, a gateway error is an error again")))
