;;;; conditions-test.lisp --- the kernel's binders and its cleanup form.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; BIND is one binder where Common Lisp has three, so what is proved here
;;;; is that it is exactly those three: sequential like LET*, receiving
;;;; several values like MULTIPLE-VALUE-BIND, and leaving a body that still
;;;; takes declarations. WITH-CLEANUP is UNWIND-PROTECT with a body, so what
;;;; is proved is that the cleanup runs on every way out, and WITH-HANDLERS
;;;; that its clauses are HANDLER-CASE's own.

(in-package #:nodecode.test)

(deftest bind-is-sequential-like-let-star ()
  ;; A spec sees every spec above it, and the value it sees is the one just
  ;; bound -- which is what the LET/LET*/MULTIPLE-VALUE-BIND chains it
  ;; replaces already were.
  (is (equal '(1 2 3) (nlk:bind ((a 1) (b (+ a 1)) (c (+ b 1))) (list a b c))))
  (is (equal '(1 1) (nlk:bind ((a 1) (a2 a)) (list a a2)))))

(deftest bind-takes-the-several-values-a-form-answers ()
  ;; A list target is a MULTIPLE-VALUE-BIND; _ receives a value the body
  ;; never names, and two of them in one form get separate fresh symbols
  ;; rather than colliding on one.
  (is (equal '(3 1) (nlk:bind (((q r) (floor 7 2))) (list q r))))
  (is (equal '(3 7) (nlk:bind (((q _) (floor 7 2)) (n 7)) (list q n))))
  (is (equal '(2 1) (nlk:bind (((_ r) (floor 7 2)) ((_ s) (floor 5 2)))
                      (list (+ r s) r)))))

(deftest bind-leaves-an-ordinary-body ()
  ;; The body is a body: it takes a declaration, it runs several forms in
  ;; order, and with no specs at all it is itself.
  (is (= 6 (nlk:bind ((a 2)) (declare (type integer a)) (* a 3))))
  (is (= 4 (nlk:bind () 4)))
  (is (equal '(:a :b) (nlk:bind ((x :a)) (list x :b)))))

(deftest with-cleanup-runs-on-every-way-out (let ((marks '())))
  (is (= 3 (nlk:with-cleanup ((push :normal marks)) (push :body marks) 3)))
  (is (equal '(:normal :body) marks) "after a normal answer")
  (setf marks '())
  (is (eq :thrown (block out
                    (nlk:with-cleanup ((push :unwound marks))
                      (push :body marks)
                      (return-from out :thrown)))))
  (is (equal '(:unwound :body) marks) "after a transfer out")
  (setf marks '())
  (is (signals-error error (nlk:with-cleanup ((push :signalled marks))
                             (error "gone"))))
  (is (equal '(:signalled) marks) "after a signal")
  (setf marks '())
  (nlk:with-cleanup ((push :first marks) (push :second marks)) nil)
  (is (equal '(:second :first) marks) "several cleanups, in order"))

(deftest with-handlers-answers-for-a-body-it-guards ()
  ;; The clauses are HANDLER-CASE's, read in order, over a body of however
  ;; many forms; nothing they do not name still leaves.
  (is (= 3 (nlk:with-handlers ((error () :caught)) 1 2 3)))
  (is (eq :caught (nlk:with-handlers ((error () :caught)) (error "gone"))))
  (is (equal "gone" (nlk:with-handlers ((error (e) (princ-to-string e)))
                      (error "gone"))))
  (is (eq :narrow (nlk:with-handlers ((type-error () :wide) (error () :narrow))
                    (error "gone"))))
  (is (signals-error error (nlk:with-handlers ((type-error () :wrong))
                             (error "gone")))))

(deftest a-spawned-thread-prints-as-a-repl-does (let ((printed nil)))
  ;; Decoded text is kept as base-strings, and a thread starts under standard
  ;; syntax: without the spawn's own printer, ~S of a job's name read
  ;; #A((3) BASE-CHAR . "job").
  (bt2:join-thread (nlk:spawn "print-test"
                     (setf printed (format nil "~s" (coerce "job" 'base-string)))))
  (is (equal "\"job\"" printed)))

(deftest parallel-map-answers-in-order-and-signals-the-first-error ()
  ;; Each call on a lane of its own, the answers in the list's order however
  ;; the lanes finish; one that signals is signalled to the caller once the
  ;; lanes are done, and the lanes take nothing more.
  (let ((threads '()) (lock (bt2:make-lock)))
    (is (equal '(1 4 9 16 25 36 49 64 81 100)
               (nlk:parallel-map (lambda (n)
                                   (bt2:with-lock-held (lock) (pushnew (bt2:current-thread) threads))
                                   (sleep (/ (- 11 n) 1000))
                                   (* n n))
                                 '(1 2 3 4 5 6 7 8 9 10) :lanes 4)))
    (is (<= 2 (length threads) 4) "more than the caller, no more than the lanes"))
  (is (null (nlk:parallel-map #'identity '())))
  (let ((ran 0))
    (is (equal "three" (nlk:with-handlers ((error (e) (princ-to-string e)))
                         (nlk:parallel-map (lambda (n)
                                             (incf ran)
                                             (when (= n 3) (error "three"))
                                             n)
                                           '(1 2 3 4 5 6 7 8) :lanes 1))))
    (is (= 3 ran) "nothing taken past the error")))
