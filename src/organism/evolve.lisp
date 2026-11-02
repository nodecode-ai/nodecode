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
  "Lock the kernel package so redefining a kernel symbol is an error. Idempotent."
  (unless *floor-locked*
    (sb-ext:lock-package '#:nodecode.kernel)
    (setf *floor-locked* t))
  *floor-locked*)

(defun unlock-floor ()
  "Unlock the kernel package."
  ;; The organism is allowed to lower its own floor. That is the point of a
  ;; floor that is chosen rather than imposed: a kernel that could not be
  ;; unlocked would make this a sandbox, and the mutation path stops being
  ;; honest about what it can reach.
  (when *floor-locked*
    (sb-ext:unlock-package '#:nodecode.kernel)
    (setf *floor-locked* nil))
  (not *floor-locked*))

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
