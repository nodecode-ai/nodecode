;;;; activity.lisp --- which sessions are running a turn right now, and since
;;;; when: the one map every attached shell holds so a list of sessions can
;;;; say which of them is busy. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The durable authority is the log, not this file: a session is running
;;;; when it has an admitted turn without a terminal fact, and the clock its
;;;; row paints is the stamp on that turn's own TURN.STARTED fact
;;;; (NLK:ACTIVE-TURN-STARTS). What lives here is only the projection of that
;;;; read model onto the wire -- one frame, organism.activity, carrying the
;;;; whole map, published to every attached shell the moment a turn lifecycle
;;;; fact passes the frame point (PUBLISH-EVENT, gateway.lisp) and once more
;;;; to a shell as it subscribes.
;;;;
;;;; Whole map, never a delta, and only when it actually changed: the set is
;;;; as small as the turns the operator started, each frame is a fresh read
;;;; of the durable truth, and a frame lost costs the next one nothing. A
;;;; shell folds it into its own table and paints from there; nothing dials
;;;; for any of it, and an idle organism publishes nothing at all.

(in-package #:nodecode.evolved)

(defvar *session-activity-lock* (bt2:make-lock :name "session-activity")
  "Protects *SESSION-ACTIVITY* and orders the publications: a frame is
built, compared and sent under it, so two turn threads racing a start and a
settle can never publish the map out of order and leave a shell showing the
older one.")

;;; :UNREAD before the first read, at every gateway boot
;;; (RESET-SESSION-ACTIVITY) and after a projection failure, so the next
;;; reader takes the durable answer fresh; every lifecycle frame does
;;; (PUBLISH-SESSION-ACTIVITY), which is what keeps the copy current between
;;; reads. A subscribe used to re-read the store instead: the /sessions picker
;;; re-subscribes per highlight move, and that read was ~120 ms per arrow key.
;;; DEFVAR: reloading this file under a live gateway keeps the
;;; comparison honest instead of re-announcing the map.
(defvar *published-activity* :unread
  "The running-session map as this projection holds it, the same
((SESSION-ID . STARTED-AT) ...) shape NLK:ACTIVE-TURN-STARTS answers in:
what the last organism.activity frame carried -- what says whether a frame
is due at all -- and what a subscribing shell is handed.")

(defun reset-session-activity ()
  "Forget the held map: the next reader takes it from the store."
  ;; A gateway boot's projection reset.
  (bt2:with-lock-held (*session-activity-lock*)
    (setf *published-activity* :unread)))

(defun held-session-activity ()
  "The running-session map this projection holds, read fresh when it holds
none."
  ;; Callers hold *SESSION-ACTIVITY-LOCK*, so a snapshot and a publication
  ;; never interleave.
  (when (eq *published-activity* :unread)
    (setf *published-activity* (nlk:active-turn-starts)))
  *published-activity*)

(defun session-activity-payload (rows)
  "ROWS as the organism.activity wire payload: one object per running
session, its id and the ISO stamp its turn started at."
  (json-object "sessions" (map 'vector (lambda (row)
                                         (make-json-object "id" (car row)
                                                           "started_at" (cdr row)))
                               rows)))

(defun publish-session-activity ()
  "Publish the running-session map to every attached shell when it changed
since the last frame."
  ;; => the rows published, or NIL when the map stood
  ;; still. Called from PUBLISH-EVENT's tail for the frames that can have moved
  ;; it, so an idle organism sends nothing.
  (bt2:with-lock-held (*session-activity-lock*)
    (let ((rows (nlk:active-turn-starts))
          (held *published-activity*))
      (setf *published-activity* rows)
      ;; An unread map compares as the empty one: the first lifecycle frame
      ;; of an idle organism announces nothing, as it never did.
      (when (not (equal rows (if (eq held :unread) '() held)))
        (fanout-frame (nlk:encode-unsequenced-frame "organism.activity"
                                                   (session-activity-payload rows)))
        rows))))

(defun note-session-activity-frame (op)
  "PUBLISH-EVENT's tail for the running-session map: when OP is a turn
lifecycle fact, publish the map if it moved."
  ;; Runs on the publishing thread after the frame's own fan-out; a projection
  ;; that fails costs nothing but the frame, never the publish.
  ;; Only a frame carrying one of the turn lifecycle facts
  ;; (NLK::+TURN-LIFECYCLE-KINDS+) can have changed which sessions are running.
  (handler-case
      (destructuring-bind (&key kind payload &allow-other-keys) op
        (when (and (equal kind "session_stream_event")
                   (member (json-value payload :string "frame" "event" "type")
                           nlk::+turn-lifecycle-kinds+ :test #'equal))
          (publish-session-activity)))
    (error (condition)
      ;; The held copy may now lag the log: the next reader re-reads.
      (reset-session-activity)
      (warn "activity projection skipped a frame: ~a" condition)
      nil)))

(defun send-session-activity-snapshot (peer)
  "The running-session map to PEER, whole, as it subscribes: what was already
in flight before this shell attached, so its first /sessions reads right
without waiting for a turn to move."
  ;; Repeated subscribes cost one current frame each, served from the held
  ;; map -- no store read behind a re-subscribe.
  (enqueue-peer-frame
   peer (nlk:encode-unsequenced-frame "organism.activity"
         (session-activity-payload
          (bt2:with-lock-held (*session-activity-lock*)
            (held-session-activity))))))
