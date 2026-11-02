;;;; evolve-test.lisp --- the floor: the lock, ownership, recorded lineage.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The organism keeps the layer under ~/.nodecode/cells/ (scribe-test,
;;;; cells-test). What remains here is the floor — the kernel lock, what
;;;; counts as floor material, and the engage/lower pair as durable facts.

(in-package #:nodecode.test)

;;; --- helpers ------------------------------------------------------------

(defmacro with-floor-store ((sid-var tag) &body body)
  "WITH-SESSION-STORE over the session named for TAG, SID-VAR bound to its id."
  `(let ((,sid-var (format nil "s-~a" ,tag)))
     (with-session-store (,sid-var) ,@body)))

;;; --- the package lock ---------------------------------------------------

(deftest lock-floor-refuses-kernel-redefinition ()
  ;; The lock is a shared global. Unlock first so this test is order-independent.
  (nodecode.kernel:unlock-floor)
  (is (eval '(defun nodecode.kernel::probe-before-lock () :allowed)))
  (nodecode.kernel:lock-floor)
  (is (nodecode.kernel:lock-floor) "a second lock returns true")
  (signals-error sb-ext:symbol-package-locked-error
    (eval '(defun nodecode.kernel::probe-after-lock () :refused)))
  (is (eval '(defun nodecode.evolved::probe-evolved () :still-allowed)))
  (nodecode.kernel:unlock-floor)
  (is (eval '(defun nodecode.kernel::unlocked-probe () :now-allowed)) "unlocked, it allows again"))

(deftest the-lock-holds-regardless-of-the-callers-package ()
  ;; SBCL exempts code running inside a package from that package's own lock.
  ;; The eval seam binds *PACKAGE* to EVOLVED around every form so that
  ;; exemption never applies to a mutation, whatever package the caller was
  ;; standing in.
  (nlk:with-cleanup ((nodecode.kernel:unlock-floor))
    (nodecode.kernel:lock-floor)
    (dolist (name '("COMMON-LISP-USER" "NODECODE.EVOLVED" "NODECODE.KERNEL"))
      (let* ((*package* (find-package name))
             (result (nle:eval-string
                      "(defun nodecode.kernel:shipped-cells-directory (&optional source) (declare (ignore source)) nil)")))
        (is (search "ERROR" result)
            (format nil "refused from ~a: ~a" name result))
        (is (search "lock" (string-downcase result)))))))

;;; --- ownership ------------------------------------------------------------

(deftest a-fresh-name-typed-in-the-kernel-is-not-floor-material ()
  ;; Ownership is about what the kernel actually owns, not where the
  ;; operator's REPL happened to be pointing when the name was interned.
  (let ((*package* (find-package '#:nodecode.kernel)))
    (read-from-string "a-brand-new-floor-name"))
  (is (null (nlk::kernel-owned-name-p "A-BRAND-NEW-FLOOR-NAME")))
  (is (nlk::kernel-owned-name-p "OPEN-STORE") "an exported function is")
  (is (nlk::kernel-owned-name-p "RECORD-ORGANISM-FACT") "an internal fbound function is")
  (is (nlk::kernel-owned-name-p "*FLOOR-LOCKED*") "an internal bound variable is")
  (is (null (nlk::kernel-owned-name-p "NO-SUCH-NAME-ANYWHERE-AT-ALL"))))

;;; --- the floor as recorded lineage ---------------------------

(deftest engage-floor-records-a-durable-boot-fact (with-floor-store (sid "floor-h1"))
  (nlk:with-cleanup ((nlk:unlock-floor))
    (is (nlk:engage-floor :session-id sid) "engagement returns an event id")
    ;; Lowering it takes who and why: a refused lowering leaves the floor held
    ;; and the log clean.
    (signals-error error
      (nlk:lower-floor :session-id sid :requested-by "" :reason ""))
    (is (null (session-event sid :type 'nlk:floor-lowered-event)))
    (signals-error sb-ext:symbol-package-locked-error
      (eval '(defun nodecode.kernel::h1-probe () :refused)))
    (is (nlk:lower-floor :session-id sid
                         :requested-by "test-operator"
                         :reason "H1 seam: assert the event pair"))
    (is (eval '(defun nodecode.kernel::h1-probe () :allowed)))
    (is-present (engaged (session-event sid :type 'nlk:floor-engaged-event))
        "store holds the floor-engaged fact"
        (is (string= "organism" (nlk:event-aggregate-type engaged))))
    (is-present (lowered (session-event sid :type 'nlk:floor-lowered-event))
        "store holds the floor-lowered fact"
        (is-shape lowered (nlk:floor-lowered-event-requested-by string= "test-operator")
          (nlk:floor-lowered-event-reason string= "H1 seam: assert the event pair")))))
