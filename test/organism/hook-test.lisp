;;;; hook-test.lisp --- advice over override.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The point chains (:TOOL, :FRAME) are proved where they dispatch
;;;; (turn-test, gateway-test). What this file locks is the same primitive
;;;; around a function name: keyed, stackable in registration order,
;;;; removable one key at a time, and surviving the redefinitions a layer
;;;; and an upgrade perform underneath it.

(in-package #:nodecode.test)

(defun hook-probe-define (text)
  "Define through the reader so the EVOLVED symbol is interned fresh —
the evolve tests rebuild the package mid-suite."
  (eval (let ((*package* (find-package '#:nodecode.evolved)))
          (read-from-string text))))

(deftest hook-advice-wraps-a-function-in-registration-order ()
  (let ((name (hook-probe-define "(defun hook-probe-a (x) (* x 2))")))
    (nlk:hook name "outer" (lambda (next x) (list :outer (funcall next x))))
    (nlk:hook name "inner" (lambda (next x) (list :inner (funcall next (1+ x)))))
    (is (equal '(:outer (:inner 8)) (evolved-fn "HOOK-PROBE-A" 3)))
    (is (nlk:unhook name "outer"))
    (is (equal '(:inner 8) (evolved-fn "HOOK-PROBE-A" 3)))
    (is (nlk:unhook name "inner"))
    (is (null (nlk:unhook name "inner")) "a second unhook finds nothing")
    (is (eql 6 (evolved-fn "HOOK-PROBE-A" 3)))
    (is (null (sb-int:encapsulated-p name 'nlk::hook)))))

(deftest hook-advice-survives-redefinition-and-methods ()
  ;; A layer overriding the function, or an upgrade shipping a new body,
  ;; redefines the name under the advice; the advice must still run.
  (let ((name (hook-probe-define "(defun hook-probe-b (x) (list :v1 x))")))
    (nlk:hook name "wrap" (lambda (next x) (cons :wrapped (funcall next x))))
    (hook-probe-define "(defun hook-probe-b (x) (list :v2 x))")
    (is (equal '(:wrapped :v2 1) (evolved-fn "HOOK-PROBE-B" 1)))
    (nlk:unhook name "wrap"))
  (hook-probe-define "(defgeneric hook-probe-g (x))")
  (hook-probe-define "(defmethod hook-probe-g ((x integer)) (* x 10))")
  (let ((name (nlk::evolved-symbol "HOOK-PROBE-G")))
    (nlk:hook name "wrap" (lambda (next x) (cons :g (list (funcall next x)))))
    (hook-probe-define "(defmethod hook-probe-g ((x string)) (length x))")
    (is (equal '(:g 4) (evolved-fn "HOOK-PROBE-G" "abcd")))
    (nlk:unhook name "wrap")))

(deftest hook-advice-on-a-generic-stays-readable-to-the-snapshot ()
  ;; SB-INT:ENCAPSULATED-P answers an ENCAPSULATION-INFO for a plain
  ;; function but the encapsulation's INDEX — 0 for the first — for a
  ;; generic, whose fdefinition encapsulation never replaces. Reading that
  ;; 0 as an info would make DEFINITION-SNAPSHOT signal; since the snapshot
  ;; runs around EVERY folder, an image whose layer advised a generic would
  ;; fail to boot.
  (hook-probe-define "(defgeneric hook-probe-snap (x))")
  (hook-probe-define "(defmethod hook-probe-snap ((x integer)) (* x 5))")
  (let* ((name (nlk::evolved-symbol "HOOK-PROBE-SNAP"))
         (plain (nlk::unadvised-function name)))
    (nlk:hook name "wrap" (lambda (next x) (list :wrapped (funcall next x))))
    (is (equal '(:wrapped 10) (evolved-fn "HOOK-PROBE-SNAP" 2)))
    (is (eq plain (nlk::unadvised-function name)))
    (let ((snapshot (nlk::definition-snapshot)))
      (is (eq plain (gethash (cons :function name) snapshot)))
      (is (eql 1 (gethash (cons :methods name) snapshot))))
    (nlk:unhook name "wrap")
    (is (eql 10 (evolved-fn "HOOK-PROBE-SNAP" 2)))))

(deftest hook-advice-short-circuits-and-rewrites-results ()
  (let ((name (hook-probe-define "(defun hook-probe-c (x) (error \"never reached ~a\" x))")))
    (nlk:hook name "veto" (lambda (next x) (declare (ignore next)) (list :vetoed x)))
    (is (equal '(:vetoed 7) (evolved-fn "HOOK-PROBE-C" 7)))
    (nlk:unhook name "veto")
    (is (signals-error error (evolved-fn "HOOK-PROBE-C" 7)))))

(deftest hook-advice-refuses-an-unknown-function ()
  (let ((name (intern "HOOK-PROBE-NOWHERE-AT-ALL" '#:nodecode.evolved)))
    (is (signals-error error
          (nlk:hook name "k" (lambda (next &rest args) (apply next args)))))
    (is (null (assoc name nlk:*hooks*)))))

(deftest hook-advice-from-two-folders-under-different-keys ()
  ;; Validation seam 9: two folders each wrap the same function; both run,
  ;; in load order; unhook removes one.
  (hook-probe-define "(defun hook-probe-d (x) (list :base x))")
  (with-cell-fixture (root)
    (dolist (tag '("a" "b"))
      (write-layer-folder
       root (format nil "nl-adv-~a" tag)
       `((,tag . ,(format nil "(hook 'hook-probe-d \"nl-adv-~a\" (lambda (next x) ~
                               (cons :~a (funcall next x))))" tag tag)))))
    (let ((records (load-cell-fixture root)))
      (is (every (lambda (record) (eq :loaded record.state)) records))
      (is (equal '(:a :b :base 1) (evolved-fn "HOOK-PROBE-D" 1)))
      (is (null (nlk:cell-overrides (first records))))
      (nlk:unhook (nlk::evolved-symbol "HOOK-PROBE-D") "nl-adv-a")
      (is (equal '(:b :base 1) (evolved-fn "HOOK-PROBE-D" 1)))
      (nlk:unhook (nlk::evolved-symbol "HOOK-PROBE-D") "nl-adv-b"))))

(deftest hook-define-hook-is-hook-as-a-defining-form ()
  (let ((name (hook-probe-define "(defun hook-probe-d (x) (list :body x))")))
    (is (equal "tag"
               (hook-probe-define
                "(define-hook hook-probe-d \"tag\" (next x) (cons :v1 (funcall next x)))")))
    (is (equal '(:v1 :body 1) (evolved-fn "HOOK-PROBE-D" 1)))
    (hook-probe-define
     "(define-hook hook-probe-d \"tag\" (next x) (cons :v2 (funcall next x)))")
    (is (equal '(:v2 :body 1) (evolved-fn "HOOK-PROBE-D" 1)))
    (is (= 1 (length (cdr (assoc name nlk:*hooks*)))))
    (is (nlk:unhook name "tag"))
    (is (equal '(:body 1) (evolved-fn "HOOK-PROBE-D" 1))))
  ;; The same form on a point keyword: the (op next) shape.
  (let ((nlk:*hooks* nlk:*hooks*))
    (hook-probe-define
     "(define-hook :hook-probe-point \"tag\" (op next) (funcall next (cons :seen op)))")
    (is (equal '(:final :seen :op)
               (nlk:call-hooked :hook-probe-point '(:op)
                                (lambda (op) (cons :final op)))))
    (nlk:unhook :hook-probe-point "tag")))

(deftest hook-refuses-a-fn-the-chain-could-not-call ()
  ;; a point's (op next) lambda on a function name.
  ;; The chain calls a function-name hook with (next . args); the shape
  ;; that cannot take that is refused where the lambda list is in hand,
  ;; naming both shapes, instead of erroring on its first dispatch and
  ;; staying installed.
  (let ((name (hook-probe-define "(defun hook-probe-c (a b &key c) (list a b c))")))
    (let ((report (refusal-text error (nlk:hook name "wrong" (lambda (next op) (list next op))))))
      (is (search "receives (next . the arguments of the call)" report) report)
      (is (search "HOOK-PROBE-C takes at least 2 arguments" report) "the target's arity is named")
      (is (search "(op next)" report) "and the point shape it was confused with"))
    (is (null (cell-hook "wrong" name)) "nothing was installed")
    (is (equal '(1 2 3) (evolved-fn "HOOK-PROBE-C" 1 2 :c 3)) "and the function is unwrapped")
    (is (equal "right" (nlk:hook name "right" (lambda (next &rest args) (apply next args)))))
    (is (equal '(1 2 nil) (evolved-fn "HOOK-PROBE-C" 1 2)))
    (nlk:unhook name "right"))
  (let ((name (hook-probe-define "(defun hook-probe-d (x) x)")))
    (is (equal "two" (nlk:hook name "two" (lambda (next x) (funcall next x)))))
    (nlk:unhook name "two"))
  (let ((report (refusal-text error (nlk:hook :tool "wrong" (lambda (op next extra) (list op next extra))))))
    (is (search "a hook on a point receives (op next)" report) report)
    (is (null (cell-hook "wrong" :tool)) "nothing was installed on the point")))

;;; --- a hook's own error at an isolated point ------------------------
;;; A watcher's bug is not the operation's failure: stepped over, counted, said
;;; once per registration, and cleared when the hook is registered again or
;;; unhooked. An error out of NEXT is still the operation's, and an unisolated
;;; point still fails with its hook.

(defun hook-test-standing-p (key)
  "Whether the board carries KEY."
  (and (find key (nlk:standing-notices) :key #'first :test #'string=) t))

(deftest an-isolated-point-steps-over-a-hook-that-signals ()
  (let ((said '()))
    (nlk:hook 'nlk::note-hook-failure "hook-test-said"
              (lambda (next name key condition)
                (push (list key (and condition t)) said)
                (funcall next name key condition)))
    (nlk:with-cleanup ((nlk:unhook :hook-test-point "before")
                       (nlk:unhook :hook-test-point "after")
                       (nlk:unhook 'nlk::note-hook-failure "hook-test-said"))
      (nlk:hook :hook-test-point "before"
                (lambda (op next) (declare (ignore next)) (error "before ~a" op)))
      (nlk:hook :hook-test-point "after"
                (lambda (op next) (funcall next op) (error "after ~a" op)))
      (let ((runs 0))
        (flet ((final (op) (declare (ignore op)) (incf runs)))
          (is (eql 1 (nlk:call-hooked :hook-test-point 1 #'final :isolate t)) "a hook failing before NEXT is stepped over; one failing after keeps NEXT's answer")
          (is (eql 2 (nlk:call-hooked :hook-test-point 2 #'final :isolate t)))
          (is (eql 2 runs) "the operation ran once per call")))
      (is (equal "the operation 3" (handler-case (progn (nlk:call-hooked :hook-test-point 3 (lambda (op) (error "the operation ~a" op)) :isolate t) nil) (error (e) (princ-to-string e)))) "an error out of NEXT is the operation's")
      (is (equal "before 4" (handler-case (progn (nlk:call-hooked :hook-test-point 4 #'identity) nil) (error (e) (princ-to-string e)))) "an unisolated point fails with its hook")
      (is (equal '(("before" t) ("after" t)) (reverse said)) "each failing hook was said once")
      (is (eql 3 (first (gethash (list :hook-test-point "before") nlk::*hook-failures*))))
      (is (eql 2 (first (gethash (list :hook-test-point "after") nlk::*hook-failures*))))
      (is (hook-test-standing-p "hook :hook-test-point before") "the failure stands on the board")
      (nlk:hook :hook-test-point "before" (lambda (op next) (funcall next op)))
      (is (not (hook-test-standing-p "hook :hook-test-point before")) "registering it again clears its word")
      (is (null (gethash (list :hook-test-point "before") nlk::*hook-failures*)))
      (is (equal '("before" nil) (first said)))
      (nlk:unhook :hook-test-point "after")
      (is (not (hook-test-standing-p "hook :hook-test-point after")) "unhooking it clears its word"))))
