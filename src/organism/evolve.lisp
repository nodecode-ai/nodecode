;;;; evolve.lisp --- the floor. One owner for the kernel lock and its lineage.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A self-modifying organism has to refuse a mutation that would sever its
;;;; ability to keep going. That refusal is the floor: the kernel package is
;;;; locked, so redefining anything it owns is an error at the eval seam and
;;;; at cell load, and lowering the floor is a recorded act with a name and
;;;; a reason attached.
;;;;
;;;; Heredity no longer lives here. What the organism keeps is on disk — the
;;;; layer under ~/.nodecode/cells/, one definition per file, under git,
;;;; loaded by every process at boot (scribe.lisp writes it, cells.lisp loads
;;;; it). The genome replay log, the generation-0 base core, the live checkout
;;;; between generations, and the kept-mutation delta a forked runner replayed
;;;; all existed to carry in-image state across a restart; a file tree under
;;;; git carries it with diff, revert and push for free. The binary is
;;;; generation 0; a proved folder is sealed as generation N and (rollback N)
;;;; restores it (generation.lisp).
;;;;
;;;; --- why ownership is a property of the floor, not of the caller ---------
;;;;
;;;; Being interned in the kernel package is not enough to be floor material:
;;;; an operator typing at a REPL with *PACKAGE* set to KERNEL interns every
;;;; new name there, and a fresh name is not floor material just because of
;;;; where it was typed. A kernel symbol is owned when the kernel exports it,
;;;; or when it already names a function or a variable — which is exactly the
;;;; set a mutation could damage by redefining. KERNEL-OWNED-NAME-P is that
;;;; one predicate; the scribe consults it before writing a name to the layer
;;;; so the two refusals cannot drift apart.

(in-package #:nodecode.kernel)

;;; --- the lock -----------------------------------------------------------

(defvar *floor-locked* nil
  "Whether the kernel package is locked. Set by LOCK-FLOOR.")

(defvar *standing-packages* nil
  "The packages standing when LOCK-FLOOR raised the floor, NIL while it is
lowered: DELETE-PACKAGE refuses to delete one, and RENAME-PACKAGE to take a
name from one (STANDING-PACKAGE).")

(defun kernel-owned-name-p (name-string)
  "Whether NAME-STRING names real floor material: a kernel symbol that is
exported, or that already denotes a function or a variable — exactly the set
a mutation could damage by redefining."
  ;; The one ownership predicate; the scribe refuses to record such a name
  ;; into the layer, and the package lock refuses to redefine it.
  (multiple-value-bind (found status)
      (find-symbol name-string (find-package '#:nodecode.kernel))
    (and found
         (or (eq status :external)
             (fboundp found)
             (boundp found)))))

(defun lock-floor ()
  "Lock the kernel package so redefining a kernel symbol is an error, and
hold every package standing now against deletion (*STANDING-PACKAGES*).
Idempotent."
  (unless *floor-locked*
    (sb-ext:lock-package '#:nodecode.kernel)
    (setf *standing-packages* (list-all-packages)
          *floor-locked* t))
  *floor-locked*)

(defun unlock-floor ()
  "Unlock the kernel package, and let the standing packages go."
  ;; The organism is allowed to lower its own floor. That is the point of a
  ;; floor that is chosen rather than imposed: a kernel that could not be
  ;; unlocked would make this a sandbox, and the mutation path stops being
  ;; honest about what it can reach.
  (when *floor-locked*
    (sb-ext:unlock-package '#:nodecode.kernel)
    (setf *standing-packages* nil
          *floor-locked* nil))
  (not *floor-locked*))

;;; --- the packages the image runs on ---------------------------------------
;;; The kernel lock guards one package; the image runs on many more: EVOLVED,
;;; which every snippet is read in, the cells', the libraries'. None of them
;;; can carry SBCL's lock -- EVOLVED is the mutation target, and a lock refuses
;;; its DEFUNs -- so DELETE-PACKAGE and RENAME-PACKAGE reached them freely. One
;;; snippet's cleanup, (dolist (p made) (ignore-errors (delete-package p))),
;;; over packages it had found as well as made, deleted NODECODE.EVOLVED, and
;;; every later snippet of every session then died binding *PACKAGE* to NIL
;;; until the gateway restarted (2026-11-08). So the raised floor also holds
;;; every package standing when it was raised: deleting one, or renaming it
;;; away from a name or nickname it answers to, is refused. A package made
;;; after is its maker's to delete, and lowering the floor lets them all go,
;;; as a live upgrade does while it loads (CONVERGE).

(defun standing-package (designator)
  "The package DESIGNATOR names when the raised floor holds it, else NIL."
  (when *standing-packages*
    (let ((package (if (packagep designator) designator (find-package designator))))
      (and package (member package *standing-packages* :test #'eq) package))))

(defun keeps-its-names-p (package new-name new-nicknames)
  "Whether renaming PACKAGE to NEW-NAME and NEW-NICKNAMES keeps every name it
answers to: a redefinition restating the package, as UIOP:DEFINE-PACKAGE does."
  (flet ((name-of (designator)
           (if (packagep designator) (package-name designator) (string designator))))
    (and (string= (package-name package) (name-of new-name))
         (subsetp (package-nicknames package) (mapcar #'name-of new-nicknames)
                  :test #'string=))))

(define-error standing-package-refused (error)
    (operation name)
  (:report "~(~a~) refused: ~a is a package this image runs on, and taking it ~
             away breaks every session until a restart. A package made after the ~
             boot stays its maker's to delete."
           operation name))

;; By name and afresh, so a reload replaces the advice instead of stacking it.
(encapsulate-afresh 'delete-package 'standing-package
                    (lambda (next designator)
                      (when-let (package (standing-package designator))
                        (error 'standing-package-refused
                               :operation 'delete-package :name (package-name package)))
                      (funcall next designator)))

(encapsulate-afresh 'rename-package 'standing-package
                    (lambda (next designator new-name &optional new-nicknames)
                      (let ((package (standing-package designator)))
                        (when (and package (not (keeps-its-names-p package new-name new-nicknames)))
                          (error 'standing-package-refused
                                 :operation 'rename-package :name (package-name package))))
                      (funcall next designator new-name new-nicknames)))

;;; --- the floor as recorded lineage ---
;;;
;;; LOCK-FLOOR and UNLOCK-FLOOR are the mechanism; ENGAGE-FLOOR and LOWER-FLOOR
;;; are the recorded paths the organism actually uses. The lock's state stops
;;; being an ambient property of whichever image happens to be running and
;;; becomes a replayable fact in the event log. Engagement lands after the
;;; folder has loaded (MAIN loads it first): a layer file naming a kernel
;;; symbol is refused by the lock when the lock is up, and the boot owner
;;; raises it before any turn runs.

(defun record-organism-fact (session-id kind action payload owner)
  "Append one organism-aggregate fact."
  ;; Returns the event id.
  ;;
  ;; The one owned path for aggregate `organism` evidence: the floor lifecycle
  ;; pair and the scribe's provenance fact all append through here, so the
  ;; invocation shape and aggregate identity cannot drift apart.
  (with-store-transaction (handle)
     (let ((event-id (make-durable-id "event"))
           (cmd (make-durable-id "command")))
       (with-event-owner owner
         (append-event-row
          handle session-id kind payload
          :aggregate-type "organism" :aggregate-id "organism"
          :command-id cmd
          :invocation (default-invocation action cmd :source "kernel")
          :event-id event-id
          :correlation-id cmd))
       event-id)))

(defun engage-floor (&key session-id)
  "Lock the kernel floor and record the engagement as a durable fact."
  ;; The lock lands first: an engagement whose fact fails to append signals
  ;; with the floor already held, and the boot owner treats that signal as a
  ;; startup refusal rather than running unrecorded. SESSION-ID attributes the
  ;; fact to the session whose boot engaged it. Returns the event id.
  (check-type session-id session-id)
  (lock-floor)
  (record-organism-fact session-id +kind-floor-engaged+ "organism.floor_engage"
                        '()
                        'engage-floor))

(defun lower-floor (&key session-id requested-by reason)
  "Record who asked to lower the floor and why, then unlock it."
  ;; The fact is appended before the unlock, so a lowering that cannot be
  ;; recorded does not happen: an unlocked kernel with no lineage naming the
  ;; decision is exactly the ambient state H1 exists to close. UNLOCK-FLOOR
  ;; itself stays callable — the floor is chosen, not imposed — but this is
  ;; the organism's path. Returns the event id.
  (check-strings session-id requested-by reason)
  (when (or (zerop (length requested-by)) (zerop (length reason)))
    (error "lowering the floor requires who asked and why"))
  (prog1 (record-organism-fact
          session-id +kind-floor-lowered+ "organism.floor_lower"
          (list :requested-by requested-by
                :reason reason)
          'lower-floor)
    (unlock-floor)))
