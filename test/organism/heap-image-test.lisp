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

(deftest a-store-closed-to-become-another-heap-keeps-its-lease (with-temp-store (store))
  ;; Released and taken back, the lock was free for a moment a rival could
  ;; take; kept, it never is.
  (let ((path (nlk::store-path store))
        (lock (nlk::store-lock-fd store)))
    (let ((nlk::*keep-store-lease* t))
      (nlk:close-store))
    (is (eql lock (nlk::held-lock (nlk::lock-path-for-store path))) "the same lock is still held")
    (is (nlk::store-lease-mine-p path) "and still this process's")
    (is (eql (sb-posix:getpid) (nlk::read-store-holder path)) "the sidecar still names this process")
    (let ((reopened (nlk:open-store :path path)))
      (is (eql lock (nlk::store-lock-fd reopened)) "a store opened after adopts it"))))

(deftest a-switch-says-why-it-cannot-run ()
  (let ((nodecode.evolved::*serving* nil))
    (is (search "only the organism that serves" (nodecode.evolved::switch-refusal nil)))
    (is (search "no image named nope" (nodecode.evolved::switch-refusal "nope")) "a missing heap is named first")))

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

(deftest the-image-picker-is-save-then-every-heap-then-the-folder (with-image-home (home))
  (save-fake-heap "older")
  (save-fake-heap "newer")
  (let ((rows (nle::image-picker-rows :running "newer")))
    (is (eq nle::+image-save-action+ (nle::picker-row-value (first rows))) "the first row keeps the organism now")
    (is (eq nle::+image-folder-action+ (nle::picker-row-value (car (last rows)))) "the last row is the folder")
    (is (equal '("newer" "older") (sort (remove-if-not #'stringp (mapcar #'nle::picker-row-value rows)) #'string<))
        (format nil "a row per heap between them: ~s" (mapcar #'nle::picker-row-value rows)))
    (let ((newer (find "newer" rows :key #'nle::picker-row-value :test #'equal)))
      (is (equal "*" (nle::picker-row-mark newer)) "the running heap is marked")
      (is (search "running now" (nle::picker-row-detail newer)))))
  (let ((folder (car (last (nle::image-picker-rows :running :folder)))))
    (is (search "running now" (nle::picker-row-detail folder)) "or the folder, when no heap runs")))

(deftest a-heap-saved-without-a-name-is-named-by-its-minute (with-image-home (home))
  (let ((time (encode-universal-time 0 34 5 4 11 2026)))
    (is (equal "2026-11-04-0534" (nle::dated-image-name time)))
    (save-fake-heap "2026-11-04-0534")
    (is (equal "2026-11-04-0534-2" (nle::dated-image-name time)) "a taken name gets the next number")
    (is (null (nle::image-name-refusal (nle::dated-image-name time))) "and is a name a heap may have")))

(deftest bare-nodecode-image-opens-the-shell-on-the-picker ()
  (dolist (line '(("image") ("image" "restore")))
    (is (equal '(:image-picker) (nth-value 5 (nle::parse-launch-arguments line))) line))
  (is (equal '(:image ("save")) (nth-value 1 (nle::parse-launch-arguments '("image" "save")))) "save's NAME is optional")
  (is (equal '(:image ("list")) (nth-value 1 (nle::parse-launch-arguments '("image" "list"))))))

(deftest a-shell-says-the-outage-it-asked-for-until-its-deadline ()
  (with-saved-globals ((nle::*expected-outage* nil))
    (is (null (nle::expected-outage-text)) "none asked")
    (setf nle::*expected-outage* (cons "image: keeping" (+ (get-internal-real-time) internal-time-units-per-second)))
    (is (equal "image: keeping" (nle::expected-outage-text)) "inside its deadline")
    (setf nle::*expected-outage* (cons "image: keeping" (1- (get-internal-real-time))))
    (is (null (nle::expected-outage-text)) "past it, a gateway error is an error again")))
