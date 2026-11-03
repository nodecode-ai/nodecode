;;;; follow-test.lisp --- a person watching a session steers and stops the
;;;; helpers under it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; steer_turn and cancel_turn are admitted for the session a peer follows
;;;; and for any session that hangs under it -- a fork, a team seat, a fork's
;;;; fork -- so a page showing the parent stops or steers a helper without
;;;; following it too (gateway.lisp SESSION-FOLLOWED-P); a session that hangs
;;;; under none the peer follows is still refused. Silent peers whose queues
;;;; stay inspectable, as questions-test.lisp has them.

(in-package #:nodecode.test)

(defun drive-followed (kind peer followed payload)
  "The sync command KIND, driven as the WS dispatcher drives it, for a peer
following FOLLOWED."
  (funcall (gethash kind nle::*sync-command-handlers*)
           peer (apply #'nlk::make-json-object payload)
           "follow-cmd" "follow-cmd" (list followed)))

(deftest a-peer-steers-and-stops-the-sessions-under-the-one-it-follows ()
  (with-session-store ("follow-parent")
    (nlk:create-session :id "follow-fork" :parent "follow-parent")
    (nlk:create-session :id "follow-fork-fork" :parent "follow-fork")
    (nlk:create-session :id "follow-elsewhere")
    (is (nle::session-followed-p "follow-parent" '("follow-parent")) "the session itself")
    (is (nle::session-followed-p "follow-fork" '("follow-parent")) "a session under it")
    (is (nle::session-followed-p "follow-fork-fork" '("follow-parent")) "and one under that")
    (is (not (nle::session-followed-p "follow-elsewhere" '("follow-parent"))) "never one under another")
    (is (not (nle::session-followed-p "follow-parent" '("follow-fork"))) "nor the one above")
    (is (not (nle::session-followed-p nil '("follow-parent"))) "nor no session")
    (with-silent-peer (peer state :follow-shell)
      (drive-followed :cancel-turn peer '("follow-parent") '("session_id" "follow-fork" "turn_id" "turn-none"))
      (drive-followed :cancel-turn peer '("follow-parent") '("session_id" "follow-elsewhere" "turn_id" "turn-none"))
      (drive-followed :steer-turn peer '("follow-parent")
                      '("session_id" "follow-fork-fork" "expected_turn_id" "turn-none" "prompt" "go left"))
      (drive-followed :steer-turn peer '("follow-parent")
                      '("session_id" "follow-elsewhere" "expected_turn_id" "turn-none" "prompt" "go left"))
      (destructuring-bind (stop-under stop-elsewhere steer-under steer-elsewhere) (command-results state)
        (is (equal '("rejected" "turn_mismatch") stop-under) "a stop under the followed session is heard (no turn runs there)")
        (is (equal '("rejected" "not_subscribed") stop-elsewhere) "one anywhere else is refused as before")
        (is (not (equal "not_subscribed" (second steer-under))) "a steer under it is heard too")
        (is (equal '("rejected" "not_subscribed") steer-elsewhere) "and refused anywhere else")))))
