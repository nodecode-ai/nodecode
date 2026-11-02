;;;; state-test.lisp --- C1 gates: durable session-state KV + harness sections.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The session_state.zig contract, proven against a real on-disk store:
;;;; event-sourced writes with the fold row's VER as CAS token, tombstone
;;;; deletes, key quota on new keys only, the nc: namespace hidden from
;;;; plain lists, watermark rewind, and the 64KiB harness budget enforced at
;;;; the write seam and floored at read time. The final gates prove the
;;;; injection cadence: sections land in the compiled system prompt fresh at
;;;; every round, never double-appended.

(in-package #:nodecode.test)

(nlk:access (round-0 nodecode.evolved::compiled-turn-context)
            (round-1 nodecode.evolved::compiled-turn-context))

(deftest state-put-get-delete-round-trip (with-temp-store ())
  (let ((v1 (nlk:session-state-put "s1" "greeting" "\"hello\""))
        (v2 (nlk:session-state-put "s1" "greeting" "\"again\"")))
    (is (and (integerp v1) (integerp v2) (> v2 v1)))
    (multiple-value-bind (value ver) (nlk:session-state-get "s1" "greeting")
      (is (equal "\"again\"" value) "the fold row carries the latest value")
      (is (= v2 ver) "and the ver of the event that wrote it"))
    (nlk:session-state-delete "s1" "greeting")
    (is (null (nlk:session-state-get "s1" "greeting")))
    (is (null (nlk:session-state-get "s1" "never-written")))))

(deftest state-quotas-bind-new-keys-and-value-size (with-temp-store ())
  (let ((nlk::+max-state-keys-per-ws+ 2))
    (nlk:session-state-put "s1" "a" "1")
    (nlk:session-state-put "s1" "b" "2")
    (is (signals-error nlk:session-state-quota-exceeded
          (nlk:session-state-put "s1" "c" "3")))
    (is (integerp (nlk:session-state-put "s1" "a" "9")))
    (nlk:session-state-delete "s1" "b")
    (is (integerp (nlk:session-state-put "s1" "c" "3"))))
  (let ((nlk::+max-state-value-bytes+ 4))
    (is (signals-error nlk:state-value-too-large
          (nlk:session-state-put "s1" "big" "\"toolong\"")))))

(deftest state-fold-rewinds-to-a-watermark (with-session-store ("fold"))
  ;; The KV stamps log_seq from the session's event log, so real events
  ;; advance the watermark between writes.
  (nlk:session-state-put "fold" "early" "\"kept\"")
  (let ((watermark (nlk::session-log-watermark "fold")))
    (nlk:record-event "fold" "test.marker" '(:note "advance the log"))
    (nlk:session-state-put "fold" "late" "\"doomed\"")
    (nlk:session-state-put "fold" "early" "\"rewritten\"")
    (fold-state-to-watermark "fold" watermark)
    (is (equal "\"kept\"" (nlk:session-state-get "fold" "early")))
    (is (null (nlk:session-state-get "fold" "late")))))

(deftest harness-sections-are-budgeted-and-ordered (with-temp-store ())
  (is-table (key) (signals-error nlk:invalid-harness-key (nlk:set-harness-section "s1" key "x"))
    ("bad:name")
    (""))
  (nlk:set-harness-section "s1" "tone" "stay terse")
  (nlk:set-harness-section "s1" "focus" "ship the slice")
  (is (equal '(("focus" . "ship the slice") ("tone" . "stay terse"))
             (nlk:list-harness-sections "s1")))
  (is-values (text ver) (nlk:get-harness-section "s1" "tone") (text "stay terse")
    ((integerp ver) is))
  (nlk:clear-harness-section "s1" "tone")
  (is (= 1 (length (nlk:list-harness-sections "s1"))))
  (let ((nlk::+max-harness-bytes+ 16))
    (is (signals-error nlk:harness-budget-exceeded
          (nlk:set-harness-section "s1" "over" "0123456789")))
    ;; The read floor: entries that no longer fit truncate the remainder.
    (nlk:session-state-put "s1" "nc:harness:zz-wide" "\"0123456789abcdef0\"")
    (is (equal '("focus") (mapcar #'car (nlk:list-harness-sections "s1"))))))

(deftest harness-sections-hold-for-the-turn-and-refresh-at-the-next (with-session-store ("h"))
  (nlk:set-harness-section "h" "tone" "stay terse")
  (let ((prompt (nle::harness-overlay-prompt "BASE" (nle::read-harness-sections "h"))))
    (is (eql 0 (search "BASE" prompt)) "the frozen base leads")
    (is (search (format nil "<harness key=\"tone\">~%stay terse~%</harness>")
                prompt)))
  ;; End to end through the compile seam, across rounds and turns.
  (flet ((compile-round (text &rest keys)
           (nodecode.evolved::compile-turn-context
            (apply #'nodecode.evolved::snapshot-context-request
                   "h" :messages (list (nodecode.evolved::message "user" text)) keys))))
    (let* ((nodecode.evolved::*api-key* "k")
           (nodecode.evolved::*system-prompt* "BASE")
           (round-0 (compile-round "x")))
      (is (search "<harness key=\"tone\">" (head-text round-0)))
      (is (equal "BASE"
                 round-0.base-system-prompt))
      ;; A section written mid-turn leaves the turn's standing block alone
      ;; — the provider's prefix cache is a byte match across the rounds —
      ;; and the first section is not appended twice.
      (nlk:set-harness-section "h" "update" "added mid turn")
      (let* ((round-1 (compile-round "x" :prior-context round-0))
             (prompt (head-text round-1)))
        (is (null (search "<harness key=\"update\">" prompt)))
        (is (equal (head-text round-0)
                   prompt))
        (is (= 1 (length (ppcre:all-matches-as-strings "<harness key=\"tone\">" prompt)))))
      ;; The next turn's first round reads the durable rows again.
      (let ((next (compile-round "y")))
        (is (search "<harness key=\"update\">" (head-text next)))))))

;;; --- learned context-window observations -----------------------------------

(deftest context-window-observations-bracket-the-window (with-temp-store ())
  (flet ((observation () (nlk:context-window-observation "obs-p" "obs-m")))
    (is-values (floor ceiling) (observation) (floor null "an unobserved model has no floor")
      (ceiling null "an unobserved model has no ceiling"))
    (nlk:observe-context-window-floor "obs-p" "obs-m" 1000)
    (nlk:observe-context-window-ceiling "obs-p" "obs-m" 4000)
    (is-values (floor ceiling) (observation) (floor = 1000 "success totals prove floors")
      (ceiling = 4000 "rejection estimates bound ceilings"))
    (nlk:observe-context-window-floor "obs-p" "obs-m" 600)
    (is (= 1000 (observation)))
    (nlk:observe-context-window-floor "obs-p" "obs-m" 5000)
    (is-values (floor ceiling) (observation) (floor = 5000 "a larger proof raises the floor")
      (ceiling null))
    (nlk:observe-context-window-ceiling "obs-p" "obs-m" 200)
    (is (= 5000 (nth-value 1 (observation))))))

