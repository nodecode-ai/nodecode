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

(deftest a-snippet-cannot-take-away-a-package-the-raised-floor-stands-on (with-floor-restored)
  ;; 2026-11-08: a snippet's cleanup deleted the packages it had found as well
  ;; as made, NODECODE.EVOLVED among them, and every later snippet of every
  ;; session died binding *PACKAGE* to NIL until the gateway restarted. A
  ;; standing stand-in carries the deletion here, so the red run harms nothing.
  (nlk:unlock-floor)
  (let* ((name "NL-STANDING-PROBE")
         (standing (or (find-package name)
                       (make-package name :use '(#:cl) :nicknames '("NL-STANDING-NICK")))))
    (nlk:with-cleanup ((nlk:unlock-floor)
                       (when (find-package name) (delete-package name)))
      (nlk:lock-floor)
      (let ((cleanup (format nil "(let ((made (list (find-package ~s) (make-package \"NL-MADE-AFTER\"))))~
                                     (dolist (p made 1) (ignore-errors (delete-package p))))"
                             name)))
        (is (equal "1" (nle:eval-string cleanup)) "the cleanup runs"))
      (is (eq standing (find-package name)) "the standing package survives the cleanup")
      (is (null (find-package "NL-MADE-AFTER")) "the package the snippet made is its own to delete")
      (is (equal "3" (nle:eval-string "(+ 1 2)")) "the next snippet runs")
      (dolist (form '("(delete-package \"NL-STANDING-NICK\")"
                      "(rename-package \"NL-STANDING-PROBE\" \"NL-STANDING-GONE\")"
                      "(rename-package \"NL-STANDING-PROBE\" \"NL-STANDING-PROBE\")"))
        (let ((result (nle:eval-string form)))
          (is (search "refused: NL-STANDING-PROBE is a package this image runs on" result)
              (format nil "~a: ~a" form result))))
      (is (eq standing (find-package "NL-STANDING-NICK")) "no rename took a name from it")
      (nle:eval-string "(rename-package \"NL-STANDING-PROBE\" \"NL-STANDING-PROBE\" '(\"NL-STANDING-NICK\" \"NL-STANDING-MORE\"))")
      (is (eq standing (find-package "NL-STANDING-MORE")) "a rename keeping every name goes through")
      (is (nlk::standing-package "NODECODE.EVOLVED") "the package every snippet reads in is held")
      (nlk:unlock-floor)
      (is (null (nlk::standing-package name)) "a lowered floor lets them go")
      (is (eq t (delete-package standing))))))

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
