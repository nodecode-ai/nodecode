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

(deftest a-heaps-memory-of-its-locks-is-no-lease-at-boot ()
  ;; A saved heap remembers the descriptors of the process that saved it: a
  ;; heap booted later by the restore pointer read that as its lease and
  ;; opened its store unlocked, a second organism beside it.
  (let ((nlk::*held-locks* (make-hash-table :test #'eql :synchronized t)))
    (nlk::note-held-lock 987 "/nonexistent/history.db.lock")
    (sb-posix:unsetenv nlk::+handed-lease-variable+)
    (is (null (nlk::adopt-handed-lease)) "nothing handed, nothing adopted")
    (is (zerop (hash-table-count nlk::*held-locks*)) "the remembered lock is forgotten")))

(deftest a-lease-handed-across-an-exec-is-adopted-and-a-forged-one-is-not (with-temp-store (store))
  (let* ((path (nlk::store-path store))
         (variable nlk::+handed-lease-variable+))
    (nlk:close-store)
    (let ((lock (nlk::hold-store-lease path)))
      (nlk::hand-lease-across-exec lock)
      (is (zerop (sb-posix:fcntl lock sb-posix:f-getfd)) "it stays open across the exec")
      (is (sb-posix:getenv variable) "and the successor is told which descriptor it is")
      (let ((nlk::*held-locks* (make-hash-table :test #'eql :synchronized t)))
        (is (eql lock (nlk::adopt-handed-lease)) "the successor takes it as its own")
        (is (nlk::store-lease-mine-p path) "its election reads it as its own"))
      (let ((other (sb-posix:open "/dev/null" sb-posix:o-rdonly)))
        (unwind-protect
             (let ((nlk::*held-locks* (make-hash-table :test #'eql :synchronized t)))
               (sb-posix:setenv variable
                                (format nil "~d ~a" other (uiop:native-namestring
                                                           (nlk::lock-path-for-store path)))
                                1)
               (is (null (nlk::adopt-handed-lease)) "a descriptor on another file is no lease")
               (is (null (sb-posix:getenv variable)) "and is named to no one after"))
          (sb-posix:close other)))
      (nlk::hand-lease-across-exec lock)
      (let ((reopened (nlk:open-store :path path)))
        (is (eql lock (nlk::store-lock-fd reopened)) "the store adopts it")
        (is (= 1 (sb-posix:fcntl lock sb-posix:f-getfd)) "and closes it at a later exec again")
        (is (null (sb-posix:getenv variable)) "no child or successor is told of it")))))

(deftest a-switch-says-why-it-cannot-run ()
  (let ((nodecode.evolved::*serving* nil))
    (is (search "only the organism that serves" (nodecode.evolved::switch-refusal nil)))
    (is (search "no image named nope" (nodecode.evolved::switch-refusal "nope"))
        "a missing heap is named first")))

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
