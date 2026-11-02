;;;; checkpoint-test.lisp --- turn-tree checkpoints: head, rewind, fork.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;   CP1. TURN.STARTED v2 stamps the admission-time head as parent at every
;;;;        admission site; legacy v1 rows chain linearly.
;;;;   CP2. The head follows appends implicitly and SESSION.HEAD_MOVED
;;;;        explicitly; the visible fold is exactly the head's ancestor chain.
;;;;   CP3. A rewind hides the tail from RECALL and the history units without
;;;;        deleting anything; a sibling branch replaces it; navigating back
;;;;        to PREVIOUS-TURN-ID restores the abandoned branch.
;;;;   CP4. The hydration caches cannot serve a pre-move prefix (head stamp).
;;;;   CP5. Turn numbers are chain depth: a sibling takes the number of the
;;;;        turn it replaced.
;;;;   CP6. A backward move folds the KV to the anchor watermark; a forward
;;;;        move never re-folds; CAS tokens minted before the move go stale.
;;;;   CP7. The eviction floor survives a rewind (monotonic, never rewound).
;;;;   CP8. Fork composes the parent chain through a frozen anchor: the child
;;;;        folds parent history it never copied, and a parent rewind after
;;;;        the fork cannot change the child.
;;;;   CP9. The checkpoint list marks head-chain membership across branches.
;;;;  CP10. The listing counts and the per-checkpoint files land in one
;;;;        child process and agree with the call-at-a-time derivation.
;;;;  CP11. The first Esc prefetches the listing the second opens the
;;;;        picker on; only that path, only inside its freshness window.
;;;;  CP12. A tool_result checkpoint carries the executable text its call's
;;;;        arguments named; the picker labels the row by it, and by the tool
;;;;        name only when the arguments named none.
;;;;  CP15. A fork inherits the standing <harness> block its chain runs
;;;;        under — the other half of the prompt prefix the anchor freezes —
;;;;        reading every anchored ancestor's sections and its own, nearest
;;;;        name winning.
;;;;  CP16. A turn no one asked — a reflection's recap, an exit's wake — is
;;;;        named by its origin in the listing and never numbered: the turn
;;;;        numbers count the asks on the chain, across a fork's lineage too.

(in-package #:nodecode.test)

(defun %started-turns (session-id)
  "SESSION-ID's TURN.STARTED facts from the whole log, oldest first — the
tree tests' addressing surface: every rewind, fork, freeze and navigation
case reaches its turn ids through this one filter."
  (session-events session-id :type 'nlk:turn-started))

(defmacro with-seeded-turns ((session-id inputs &rest turn-vars) &body body)
  "Seed SESSION-ID with completed turns over INPUTS, then run BODY with each
TURN-VAR bound to the id of the started turn at its position (NIL skips one)."
  (let ((events (gensym "EVENTS")))
    `(progn
       (seed-user-inputs ,session-id ,inputs)
       (let* ((,events (%started-turns ,session-id))
              ,@(loop for var in turn-vars
                      for position from 0
                      when var collect `(,var (nlk:event-turn-id (nth ,position ,events)))))
         (declare (ignorable ,events))
         ,@body))))

(defun %entries-of-kind (kind entries)
  "The checkpoint listing ENTRIES of wire KIND, in listing order."
  (remove-if-not (lambda (entry) (equal kind (getf entry :kind))) entries))

(defun %entry-values (key entries)
  "Each of the checkpoint listing ENTRIES' KEY value, in order."
  (mapcar (lambda (entry) (getf entry key)) entries))

(defun %navigate-command (session-id command-id &rest request-pairs)
  "One encoded :NAVIGATE-CHECKPOINT command whose request is SESSION_ID plus
REQUEST-PAIRS; COMMAND-ID doubles as the correlation id."
  (nlk:encode-client-command
   (nlk:make-gateway-command
    :kind :navigate-checkpoint
    :command-id command-id
    :correlation-id command-id
    :request (apply #'nlk:make-json-object
                    "session_id" session-id request-pairs))))

(defun %navigate-frames (port session-id command-id &rest request-pairs)
  "Subscribe to SESSION-ID on PORT, send one :NAVIGATE-CHECKPOINT command
whose request is SESSION_ID plus REQUEST-PAIRS, and return the collected
frames."
  ;; COMMAND-ID doubles as the correlation id, which is what every navigation
  ;; here asserts on.
  (frames-while (ws port :subscribe (list session-id))
    (send-command ws (apply #'%navigate-command session-id command-id request-pairs))))

(defun %command-result (frames &optional command)
  "The inner RESULT table of the first gateway_turn_command_result frame in
FRAMES, or of COMMAND's when one is named."
  (let* ((results (frames-of-type frames "gateway_turn_command_result"))
         (frame (if command (frame-carrying results command "result" "command") (first results))))
    (and frame (nlk:json-value frame :object "payload" "result"))))

(deftest checkpoint-parent-stamps-at-admission (with-temp-store ())
  (seed-user-inputs "tree" '("one" "two" "three"))
  (let ((events (%started-turns "tree")))
    (is (= 3 (length events)) "three started facts")
    (is (null (nlk:turn-started-parent-turn-id (first events))))
    (is-table (parent child) (equal (nlk:event-turn-id (parent events))
                                    (nlk:turn-started-parent-turn-id (child events)))
      (first second "second turn chains to the first")
      (second third "third turn chains to the second"))
    (is (= 2 (nlk:event-schema-version (second events))))
    (is (equal (nlk:event-turn-id (third events))
               (nlk:session-head-turn-id "tree")))))

(deftest checkpoint-rewind-hides-tail-and-branches (with-temp-store ())
  (with-seeded-turns ("branchy" '("one" "two" "three") first-turn nil third-turn)
    ;; Rewind to the first turn.
    (is (equal third-turn (getf (nlk:move-session-head "branchy" first-turn) :previous-turn-id)))
    (is (equal first-turn (nlk:session-head-turn-id "branchy")))
    (is (equal '("one") (%visible-user-inputs "branchy")))
    (is (= 3 (length (%started-turns "branchy"))))
    ;; A new turn becomes a sibling branch under the same parent.
    (seed-user-inputs "branchy" '("two-b"))
    (is (equal '("one" "two-b") (%visible-user-inputs "branchy")))
    ;; Redo: navigate back to the abandoned head.
    (nlk:move-session-head "branchy" third-turn :reason "redo")
    (is (equal '("one" "two" "three") (%visible-user-inputs "branchy")))
    (is (nlk:session-known-turn-p "branchy" third-turn))))

(deftest redo-ignores-a-retract-head-move (with-temp-store ())
  ;; A retraction's origin is an empty cancelled turn the operator took
  ;; back — the prompt went home to the composer — so it is no redo target.
  ;; An ordinary rewind keeps offering its origin.
  (with-seeded-turns ("retract-redo" '("one" "two") first-turn second-turn)
    (nlk:move-session-head "retract-redo" first-turn
                           :reason nlk:+head-move-reason-retract+
                           :boundary-kind "user_message")
    (is (equal first-turn (nlk:session-head-turn-id "retract-redo")))
    (is (null (nlk::last-head-move-origin "retract-redo")))
    (nlk:move-session-head "retract-redo" nil)
    (is (equal first-turn (nlk::last-head-move-origin "retract-redo")))
    (is (nlk:session-known-turn-p "retract-redo" second-turn))))

(deftest checkpoint-move-refuses-while-turn-active
    (with-admitted-turn (turn "active" "cmd" "in flight"))
  (is (signals-error nlk:turn-already-active
        (nlk:move-session-head "active" nil)))
  (nlk:complete-turn turn nil)
  (is (consp (nlk:move-session-head "active" nil)))
  (is (null (nlk:session-head-turn-id "active")))
  (is (null (%visible-user-inputs "active")) "root shows no turns"))

(deftest checkpoint-hydration-cache-honors-head-stamp (with-temp-store ())
  (with-seeded-turns ("cached" '("one" "two") first-turn)
    (let ((session (intern-durable-session "cached")))
      ;; Warm both caches.
      (is (= 2 (length (nle::history-turns-for-session session))))
      (nlk:move-session-head "cached" first-turn :boundary-kind "turn")
      ;; No explicit invalidation here: the stamp check alone must reject
      ;; the pre-move prefix.
      (is (= 1 (length (nle::history-turns-for-session session))))
      (is (equal '("one") (%visible-user-inputs "cached")))
      (is (plusp (nlk:session-head-stamp "cached"))))))

(deftest checkpoint-turn-numbers-are-chain-depth (with-temp-store ())
  (with-seeded-turns ("numbered" '("one" "two") first-turn)
    (nlk:move-session-head "numbered" first-turn :boundary-kind "turn")
    (seed-user-inputs "numbered" '("two-b"))
    (let ((completions (session-events "numbered" :type 'nlk:turn-completed)))
      (is (= 3 (length completions)) "three completed facts in the log")
      (is (equal '(1 2 2)
                 (mapcar #'nlk:turn-completed-turn-number completions))))))

(deftest checkpoint-kv-folds-backward-never-forward (with-temp-store ())
  (seed-user-inputs "kv" '("one"))
  (let ((anchor (nlk::session-log-cursor "kv"))
        (first-turn (nlk:session-head-turn-id "kv")))
    (nlk:session-state-put "kv" "early" "\"kept\"")
    (seed-user-inputs "kv" '("two"))
    (let ((second-turn (nlk:session-head-turn-id "kv")))
      (nlk:session-state-put "kv" "late" "\"doomed\"")
      (nlk:session-state-put "kv" "early" "\"rewritten\"")
      ;; Backward move: fold to the first turn's watermark.
      (nlk:move-session-head "kv" first-turn :boundary-kind "turn")
      (fold-state-to-watermark "kv" anchor)
      (is (equal "\"kept\"" (nlk:session-state-get "kv" "early")))
      (is (null (nlk:session-state-get "kv" "late")))
      ;; Forward move (redo): the KV must NOT re-fold — the stream is
      ;; linear and branch-blind.
      (nlk:move-session-head "kv" second-turn :boundary-kind "turn")
      (fold-state-to-watermark "kv" (nlk::session-log-cursor "kv"))
      (is (equal "\"kept\"" (nlk:session-state-get "kv" "early")))
      (is (null (nlk:session-state-get "kv" "late")))
      ;; NAVIGATE-SESSION-HEAD is the one durable navigation writer: the head
      ;; move and this same fold in a single transaction, the watermark
      ;; computed inside it; and a navigation to the root folds everything away.
      (nlk:session-state-put "kv" "late" "\"doomed\"")
      (nlk:session-state-put "kv" "early" "\"rewritten\"")
      (is (equal second-turn (getf (nlk:navigate-session-head "kv" first-turn) :previous-turn-id)))
      (is (equal first-turn (nlk:session-head-turn-id "kv")))
      (is (equal "\"kept\"" (nlk:session-state-get "kv" "early")))
      (is (null (nlk:session-state-get "kv" "late")))
      (nlk:navigate-session-head "kv" nil :boundary-kind "turn")
      (is (null (nlk:session-head-turn-id "kv")))
      (is (null (nlk:session-state-get "kv" "early"))))))

(deftest checkpoint-eviction-floor-survives-rewind (with-temp-store ())
  (with-seeded-turns ("floored" '("one" "two" "three") first-turn)
    (let ((floor (nle::persist-history-boundary
                  "floored" (nle::history-turns-from-events (nle::collect-session-events "floored"))
                  0 2 :reason "test")))
      (is (plusp floor) "eviction floor persisted")
      (nlk:move-session-head "floored" first-turn :boundary-kind "turn")
      (is (= floor (nlk:retained-history-floor "floored"))))))

(deftest fork-stands-on-the-floors-its-chain-recorded (with-temp-store ())
  ;; A fork's window is its ancestors' history, so it is their evictions too:
  ;; a Discord lane forked after its room was evicted sent the room whole,
  ;; standing on a floor of its own that was 0. The cut is the
  ;; anchor, as the fold's: what the room evicts after the fork froze it stays
  ;; the room's.
  (seed-user-inputs "room-f" '("one" "two" "three" "four"))
  (let* ((turns (nle::history-turns-from-events (nle::collect-session-events "room-f")))
         (first-floor (nle::persist-history-boundary "room-f" turns 0 2 :reason "test")))
    (nlk:create-session :id "lane-f" :parent "room-f")
    (is (= first-floor (nlk:retained-history-floor "lane-f")) "the lane stands on the room's floor")
    (is (equal (list first-floor) (nlk:retained-history-floors "lane-f")))
    (let ((later (nle::persist-history-boundary "room-f" turns 2 3 :reason "test")))
      (is (< first-floor later))
      (is (= later (nlk:retained-history-floor "room-f")))
      ;; an eviction after the anchor is the room's alone
      (is (= first-floor (nlk:retained-history-floor "lane-f"))))
    ;; Monotone over the chain: the lane's own advance cannot go below what it
    ;; inherits, and one above it is the lane's.
    (is (= first-floor (nlk:advance-history-floor "lane-f" (1- first-floor))))
    (is (= (1+ first-floor) (nlk:advance-history-floor "lane-f" (1+ first-floor))))
    (is (equal (list first-floor (1+ first-floor)) (nlk:retained-history-floors "lane-f")))
    ;; and the lane's floor never reaches its room
    (is (equal (list first-floor (nlk:retained-history-floor "room-f"))
               (nlk:retained-history-floors "room-f")))))

(deftest checkpoint-fork-composes-and-freezes (with-temp-store ())
  (with-seeded-turns ("parent-s" '("p-one" "p-two") parent-first anchor-turn)
    (let ((child (nlk:create-session :id "child-s" :parent "parent-s"
                                     :anchor-turn-id anchor-turn)))
      (is (equal anchor-turn child.anchor-turn-id))
      (is (plusp child.anchor-log-position)))
    (is (equal anchor-turn (nlk:session-head-turn-id "child-s")))
    (seed-user-inputs "child-s" '("c-one"))
    (is (equal '("p-one" "p-two" "c-one") (%visible-user-inputs "child-s")))
    ;; Parent moves after the fork are invisible to the child.
    (nlk:move-session-head "parent-s" parent-first :boundary-kind "turn")
    (is (equal '("p-one" "p-two" "c-one") (%visible-user-inputs "child-s")))
    (is (equal '("p-one") (%visible-user-inputs "parent-s")))
    ;; Anchor validation refuses foreign turns.
    (is (signals-error nlk:invalid-session-anchor
          (nlk:create-session :id "child-bad" :parent "parent-s"
                              :anchor-turn-id "turn-nonexistent")))))

(deftest fork-inherits-the-standing-block-its-chain-runs-under (with-temp-store ())
  ;; CP15. The standing <harness> sections are the other half of the prefix a
  ;; fork inherits: a lane reads its room's, nearest name winning, and a
  ;; pinned section is written on the chain's root so every later lane of
  ;; that room sends the same bytes.
  (seed-user-inputs "room" '("r-one"))
  (nlk:set-harness-section "room" "memory-index" "INDEX v1")
  (let ((lane-one "room-m1") (lane-two "room-m2"))
    (dolist (lane (list lane-one lane-two))
      (nlk:create-session :id lane :parent "room")
      (nlk:set-harness-section lane "contract" "THE ROOM"))
    (is (equal '("room") (nlk:session-standing-chain "room")))
    (is (equal (list "room" lane-one) (nlk:session-standing-chain lane-one)))
    (is (equal "room" (nlk:session-standing-owner lane-one)))
    (is (equal "room" (nlk:session-standing-owner lane-two)))
    ;; Name-ordered, the room's entry read through, the lane's own beside it.
    (is (equal '(("contract" . "THE ROOM") ("memory-index" . "INDEX v1"))
               (nlk:standing-harness-sections lane-one)))
    (is (equal (nlk:standing-harness-sections lane-one)
               (nlk:standing-harness-sections lane-two)))
    ;; A lane's own entry of a name replaces the room's, for that lane only.
    (nlk:set-harness-section lane-two "memory-index" "INDEX v2")
    (is-table (text lane)
      (equal text (cdr (assoc "memory-index" (nlk:standing-harness-sections lane) :test #'string=)))
      ("INDEX v2" lane-two) ("INDEX v1" lane-one))
    ;; A fork of a lane reads the same root: depth costs nothing.
    (let ((nested "room-m1-reflect"))
      (seed-user-inputs lane-one '("l-one"))
      (nlk:create-session :id nested :parent lane-one
                          :anchor-turn-id (nlk:session-head-turn-id lane-one))
      (is-shape nested (nlk:session-standing-owner "room")
        (nlk:standing-harness-sections '(("contract" . "THE ROOM") ("memory-index" . "INDEX v1")))))
    ;; A parent link with no anchor is provenance only: a child of a room
    ;; that has no turns yet inherits nothing.
    (nlk:create-session :id "empty-room")
    (nlk:set-harness-section "empty-room" "memory-index" "NEVER")
    (nlk:create-session :id "orphan" :parent "empty-room")
    (is (equal '("orphan") (nlk:session-standing-chain "orphan")))
    (is (null (nlk:standing-harness-sections "orphan")))))

(deftest checkpoint-list-marks-head-chain (with-temp-store ())
  (with-seeded-turns ("listed" '("one" "two") first-turn)
    (nlk:move-session-head "listed" first-turn :boundary-kind "turn")
    (seed-user-inputs "listed" '("two-b"))
    (let ((users (%entries-of-kind "user_message" (nlk:session-checkpoints "listed"))))
      (is (= 3 (length users)) "every branch appears in the picker")
      (is (equal '(t nil t) (%entry-values :on-head-chain users)))
      (is (equal '("one" "two" "two-b") (%entry-values :prompt users)))
      (is (= 1 (count-if (lambda (entry) (getf entry :is-head)) users))))))

(deftest checkpoint-list-numbers-what-was-asked-and-names-what-was-not (with-temp-store ())
  ;; CP16. A reflection's recap and an exit's wake sit on the chain, and no one
  ;; asked either: the listing names each by its origin and numbers neither, so
  ;; the asks count 1, 2, 3 however many of them stand between. A pair an import
  ;; recorded is somebody's ask and stays one.
  (seed-unasked-turns "unasked")
  (let ((users (%entries-of-kind "user_message" (nlk:session-checkpoints "unasked"))))
    (is (= 5 (length users)))
    (is (equal '(nil "reflection" nil nil "wake") (%entry-values :origin users)) "each names what began it")
    (is (equal '(1 nil 2 3 nil) (%entry-values :turn-number users)) "the asks are numbered in a row")
    (is (equal '(nil nil nil nil t) (%entry-values :is-head users)) "the head stays on the wake")
    (let ((recap (nle::checkpoint-entry-json (second users)))
          (ask (nle::checkpoint-entry-json (third users))))
      (is (equal "reflection" (gethash "origin" recap)) "the wire names it")
      (is (null (nth-value 1 (gethash "turn_number" recap))) "and gives it no number")
      (is (equal "wake" (gethash "origin" (nle::checkpoint-entry-json (fifth users)))))
      (is (= 2 (gethash "turn_number" ask)))
      (is (null (nth-value 1 (gethash "origin" ask))) "an ask says nothing of the kind")))
  ;; A fork composes the chain up to its anchor: its own ask goes on counting
  ;; the asks it inherited, not the recap between them.
  (let ((anchor (getf (third (%entries-of-kind "user_message" (nlk:session-checkpoints "unasked"))) :turn-id)))
    (nlk:create-session :id "unasked-fork" :cwd "." :parent "unasked" :anchor-turn-id anchor)
    (seed-completed-turn "unasked-fork" "fork ask" "fork answer")
    (let ((users (%entries-of-kind "user_message" (nlk:session-checkpoints "unasked-fork"))))
      (is (equal '("first ask" "[experience] recap of the turn above, by a reflection of this session" "second ask" "fork ask")
                 (%entry-values :prompt users)))
      (is (equal '(1 nil 2 3) (%entry-values :turn-number users)) "the fork's ask is the third ask"))))

(deftest checkpoint-list-carries-each-rows-span (with-temp-store ())
  ;; The /tree metro draws a turn's clock time, its span and how it ended, a
  ;; call's span, and what a recap said: the listing carries each row's span
  ;; as the log stamped it -- a turn's TURN.STARTED to its terminal event, a
  ;; call's TOOL_CALL_STARTED to its result -- with the ending, the steers and
  ;; a recap's words; a turn still running carries no end, since an end it
  ;; does not have is no zero.
  (seed-unasked-turns "spans")
  (seed-completed-turn "spans" "with a call" "done"
                       :tools '(("call-1" "eval" "{\"form\":\"(+ 1 2)\"}" "3")))
  (nlk:admit-turn "spans" (nlk::make-durable-id "command") "still going")
  (let* ((rows (mapcar #'nle::checkpoint-entry-json (nlk:session-checkpoints "spans")))
         (users (remove-if-not #'nle::checkpoint-turn-entry-p rows))
         (call (find-if-not #'nle::checkpoint-turn-entry-p rows)))
    (is (every (lambda (row) (stringp (gethash "started_at" row))) rows) "every row says when it started")
    (is (equal '("completed" "completed" "completed" "completed" "completed" "completed" nil)
               (mapcar (lambda (row) (gethash "ending" row)) users)) "each turn says how it ended, the running one nothing")
    (is (null (nth-value 1 (gethash "ended_at" (car (last users))))) "a running turn has no end")
    (is (every (lambda (row) (eql 0 (gethash "steers" row))) users) "no turn took a steer")
    (is (string<= (gethash "started_at" call) (gethash "ended_at" call)) "a call's span runs from its start to its result")
    (is (equal "Recap: nothing to carry forward." (gethash "answer" (second users))) "a recap says what it said")
    (is (null (nth-value 1 (gethash "answer" (first users)))) "an ask's row reads its ask and no answer")))

(deftest checkpoint-picker-says-what-a-turn-no-one-asked-is ()
  ;; CP16, in the /tree picker: the row of a recap or a wake names what it is
  ;; where an ask reads its number, and never `turn ?'.
  (flet ((detail (&rest fields)
           (nle::picker-row-detail
            (nle::checkpoint-picker-row (apply #'%tree-row "user_message" "t" '("h" "t") fields)))))
    (is-table (expected fields)
      (equal expected (apply #'detail fields))
      ("turn 4" '("turn_number" 4) "an ask reads its number")
      ("recap" '("origin" "reflection") "a reflection's recap says so")
      ("woke · head" '("origin" "wake" "is_head" t) "and an exit's wake, at the head"))))

(deftest checkpoint-list-labels-a-tool-row-by-its-command (with-temp-store ())
  ;; CP12. The session tree showed "eval" for every snippet; what the
  ;; operator wants to see is what the snippet was asked to evaluate.
  (seed-completed-turn
   "labeled" "explore" "done"
   :tools '(("call-form" "eval"
             "{\"form\":\"(cells)\\n(help)\",\"yield_time_ms\":10000}"
             "(#S(CELL))")
            ("call-bare" "lookup" "{\"query\":\"cells\"}" "none")))
  (let* ((form-text (format nil "(cells)~%(help)"))
         (tools (%entries-of-kind "tool_result" (nlk:session-checkpoints "labeled")))
         (call-ids (%entry-values :tool-call-id tools))
         (form (nth (position "call-form" call-ids :test #'equal) tools))
         (bare (nth (position "call-bare" call-ids :test #'equal) tools)))
    (is (= 2 (length tools)) "both calls are tool_result checkpoints")
    (is (equal form-text (getf form :tool-command)))
    (is (null (getf bare :tool-command)))
    (let ((form-json (nle::checkpoint-entry-json form))
          (bare-json (nle::checkpoint-entry-json bare)))
      ;; The WIRE carries the excerpt the row draws — the first line, 48
      ;; columns — because the picker never shows more and shipping the whole
      ;; text would put a shed-load of JSON on the keystroke that opens it.
      (is (equal (nle::checkpoint-row-excerpt form-text) (gethash "tool_command" form-json)))
      (is (equal "eval" (gethash "tool_name" form-json)))
      (is (null (nth-value 1 (gethash "tool_command" bare-json))))
      (is-table (expected json)
        (equal expected (nle::picker-row-label (nle::checkpoint-picker-row json)))
        ("  · (cells)" form-json "the picker row reads the form's first line, not eval")
        ("  · lookup" bare-json "and falls back to the tool name")))))

(deftest checkpoint-navigate-command-over-gateway (with-temp-gateway (port))
  (with-seeded-turns ("nav" '("one" "two") first-turn second-turn)
    (let ((frames (%navigate-frames port "nav" "nav-command-1"
                                    "target_turn_id" second-turn
                                    "boundary" "user_message")))
      (is-present (result (%command-result frames)) "navigation produced a command result"
          (is (equal "accepted" (gethash "status" result)))
          (is-present (response (nlk:json-value result :object "response"))
            "result carries a response"
            (is (equal "two" (gethash "undone_prompt" response)))))
      (is (find-frame frames "session_checkpoint_undo_applied"))
      (is (find-frame frames "session_resync_required"))
      ;; The durable head is the target's parent (edit-and-resubmit shape).
      (is (equal first-turn (nlk:session-head-turn-id "nav"))))))

(deftest checkpoint-navigate-refusals (with-temp-gateway (port))
  (seed-user-inputs "nav-r" '("one"))
  ;; Both refusals ride ONE connection: the sequencing is part of what
  ;; this pins, so it does not fold into %NAVIGATE-FRAMES.
  (let ((frames
          (frames-while (ws port :subscribe '("nav-r"))
            (flet ((refuse (command-id target)
                     (send-command ws (%navigate-command "nav-r" command-id
                                                         "target_turn_id" target
                                                         "boundary" "turn"))))
              (refuse "nav-refuse-1" "turn-unknown")   ; unknown turn
              (refuse "nav-refuse-2"                   ; noop: current head
                      (nlk:session-head-turn-id "nav-r"))))))
    (let ((codes (loop for frame in (frames-of-type frames "gateway_turn_command_result")
                       collect (gethash "code" (gethash "result" (gethash "payload" frame))))))
      (is (member "unknown_turn" codes :test #'equal))
      (is (member "noop" codes :test #'equal)))))

(deftest checkpoint-visibility-predicate-serves-both-streams ()
  ;; ONE predicate (EVENT-VISIBLE-P) decides transcript visibility for live
  ;; publication AND replay: turn-less facts always flow, on-chain turns
  ;; flow, everything else is an abandoned branch and drops from BOTH
  ;; streams. The old live-only TURN-LIVE-VISIBLE-P tolerance for turns
  ;; absent from the parent map was unjustified — a fresh TURN.STARTED is
  ;; the head the moment it commits, and queued inputs carry the active
  ;; turn's id — and it let the two streams diverge; it is gone.
  (let ((chain (make-hash-table :test #'equal)))
    (setf (gethash "t1" chain) t)
    (flet ((event-with-turn (turn-id)
             (make-instance 'nlk::unknown-event :turn-id turn-id)))
      (is (nlk:event-visible-p (event-with-turn nil) chain))
      (is (nlk:event-visible-p (event-with-turn "t1") chain))
      (is (not (nlk:event-visible-p (event-with-turn "t2") chain))))))

(deftest checkpoint-direction-undo-redo-over-gateway (with-temp-gateway (port))
  ;; DIRECTION lets thin shells say /undo and /redo: the server resolves
  ;; the target from its own tree — undo rewinds the head's user boundary,
  ;; redo returns to the newest head move's off-chain origin, and a redo
  ;; with nothing to return to is refused.
  (with-seeded-turns ("nav-dir" '("one" "two") first-turn second-turn)
    (flet ((result-of (direction command-id)
             (%command-result (%navigate-frames port "nav-dir" command-id
                                                "direction" direction))))
      (is-present (result (result-of "undo" "dir-undo-1")) "undo produced a command result"
          (is (equal "accepted" (gethash "status" result)) "undo accepted")
          (is (equal "two" (gethash "undone_prompt"
                                    (gethash "response" result)))))
      (is (equal first-turn (nlk:session-head-turn-id "nav-dir")))
      (is-present (result (result-of "redo" "dir-redo-1")) "redo produced a command result"
          (is (equal "accepted" (gethash "status" result)) "redo accepted"))
      (is (equal second-turn (nlk:session-head-turn-id "nav-dir")))
      (is-present (result (result-of "redo" "dir-redo-2")) "second redo answered"
          (is (equal "rejected" (gethash "status" result)))))))

;;; --- /tree files preview + restore confirm stage --------------------------
;;; Gate claims:
;;;   CP10. The files-preview labels match the Zig chrome exactly: "N file
;;;         change(s)", up to 3 paths ", "-joined with ", +K more", and the
;;;         selected-row "  ·  " composition.
;;;   CP11. The restore confirm stage interposes before any user-prompt
;;;         navigation: two options with exact copy, Enter fires the chosen
;;;         restore scope, Esc closes without navigating, and a row without
;;;         a recorded capture refuses with the Zig missing copy.
;;;   CP12. The highlighted /tree row lazily fetches its files once and the
;;;         FETCH IS the count (|paths|): rows without a capture never
;;;         fetch, a loaded row never refetches, a failed fetch retries on
;;;         the next highlight, and only the selected row renders the
;;;         suffix. The picker opens with the fetch STAGED — the rows paint
;;;         on the keystroke, the chrome lands behind the frame.
;;;   CP13. CHECKPOINT-CHANGED-FILES derives the worktree-vs-target-tree
;;;         diff per checkpoint (predecessor tree, first-of-chain uses its
;;;         own), unknown ids answer known-p NIL, and the capture kill
;;;         switch degrades to an empty list, never a wrong one.
;;;   CP14. The listing carries checkpoint_id and the tree shape, its tail
;;;         read under limit, and it carries NO file count (an absent count
;;;         says `ask the files route' rather than `nothing changed'); the
;;;         files route serves the Zig payload shape, and navigate's
;;;         restore_files:false moves the head while leaving disk alone.

(deftest checkpoint-files-labels ()
  (is-table (expected label) (equal expected label)
    ("1 file change" (nle::checkpoint-file-change-label 1) "singular drops the s")
    ("3 file changes" (nle::checkpoint-file-change-label 3) "plural keeps the s")
    ("a.zig, b.zig" (nle::checkpoint-file-list-label '("a.zig" "b.zig"))
     "short lists join with comma-space")
    ("a.zig, b.zig, c.zig, +2 more"
     (nle::checkpoint-file-list-label '("a.zig" "b.zig" "c.zig" "d.zig" "e.zig"))
     "overflow folds into +K more after three paths")
    ("  ·  3 file changes" (nle::checkpoint-files-suffix 3) "bare count before the path list lands")
    ("  ·  5 file changes: a.zig, b.zig, c.zig, +2 more"
     (nle::checkpoint-files-suffix 5 '("a.zig" "b.zig" "c.zig" "d.zig" "e.zig"))
     "landed list appends after a colon")
    ("  ·  2 file changes" (nle::checkpoint-files-suffix 2 ())
     "an empty landed list keeps the bare count"))
  ;; The listing-entry projection carries the preview fields into the row.
  (let* ((row (nle::checkpoint-picker-row
               (%tree-prompt "t-9" 4 '("t-0" "t-9") "checkpoint_id" "cp-9" "file_change_count" 3)))
         (value (nle::picker-row-value row)))
    (is (equal "cp-9" (getf value :checkpoint-id)) "checkpoint id rides the value")
    (is (= 3 (getf value :file-change-count)) "count rides the value")
    (is (equal "  ·  3 file changes" (nle::picker-row-suffix row)))
    (is (search "3 file changes"
                (nle::docked-panel-row (nle::dialog-entry-segs row t) 80 nil)))
    (is (not (search "3 file changes"
                     (nle::docked-panel-row
                      (nle::dialog-entry-segs row nil) 80 nil))))))

(deftest checkpoint-dialog-offers-continue-and-rewind ()
  ;; The /tree confirm stage over a prompt row: continue from the turn
  ;; first (the operator reads a row as being AT that turn), the
  ;; rewind to before its message second, the same with the code changes
  ;; when a capture recorded them. The head row offers the rewind alone; a
  ;; prompt without a capture offers no files rewind; Esc moves nothing.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "confirm"))
        (moves '()))
    (labels ((move (kind restore-files) (push (list kind restore-files) moves))
             (offer (&rest row) (nle::open-checkpoint-dialog model row #'move))
             (labels-of () (row-labels model.dialog)))
      (is (null (offer :turn-id "t1" :checkpoint-id "cp" :file-change-count 2
                       :detail "turn 2 · branch" :summary "yo1")))
      (let ((dialog model.dialog))
        (is-shape dialog (nle::picker-dialog-p is "the confirm stage is a dialog")
          (.title "Checkpoint" "its tab") (.context " turn 2 · branch") (.subtitle "yo1"))
        (is (equal '("Continue from this turn"
                     "Rewind to before this message"
                     "Rewind to before this message + code changes (2 files changed)")
                   (labels-of)))
        (is (zerop dialog.selection))
        (is-shape dialog (nle::picker-dialog-option-marker is) (.footer-hints eq :checkpoint))
        ;; Padding, header, the three options, footer, the gap.
        (is (= (+ 1 4 3 3 1) (nle::docked-dialog-frame-rows model)) "the panel fits its options"))
      (dialog-keys model :enter)
      (is (equal '((:continue nil)) moves))
      (is (null model.dialog) "Enter closes the dialog")
      ;; The rewind, conversation only.
      (offer :turn-id "t1" :checkpoint-id "cp" :file-change-count 1 :detail "turn 2")
      (is (equal "Rewind to before this message + code changes (1 file changed)"
                 (third (labels-of))))
      (is (equal "Continue from this turn, or rewind to before its message"
                 (nle::picker-dialog-subtitle model.dialog)))
      (dialog-keys model :down :enter)
      (is (equal '(:rewind nil) (first moves)))
      ;; The rewind with the code changes.
      (offer :turn-id "t1" :checkpoint-id "cp" :file-change-count 1 :detail "turn 2")
      (dialog-keys model :down :down :enter)
      (is (equal '(:rewind t) (first moves)))
      ;; The head row: the session is already there, so no continue.
      (offer :turn-id "t3" :head-p t :checkpoint-id "cp3" :detail "turn 3 · head")
      (is (equal '("Rewind to before this message"
                   "Rewind to before this message + code changes")
                 (labels-of)))
      (dialog-keys model :escape)
      ;; No capture recorded: the conversation rewind stands, the files one
      ;; has nothing to restore.
      (offer :turn-id "t1" :detail "turn 1")
      (is (equal '("Continue from this turn" "Rewind to before this message")
                 (labels-of)))
      (dialog-keys model :escape)
      (is (= 3 (length moves)) "Esc moves nothing")
      (is (null model.dialog) "Esc closed the dialog"))))

(deftest checkpoint-tree-order-hangs-a-branch-under-its-fork ()
  ;; The listing's tree order: the first line of turns is the trunk, a
  ;; turn's tool rows right under its prompt, and every child but the
  ;; oldest — a turn branched from it later — a line of its own one lane
  ;; deeper right under the parent's row, the oldest child continuing the
  ;; line after them. Each row says which lanes it sits inside (open while
  ;; more lines fork off below) and whether it is where a line forks off.
  (flet ((prompt (turn-id parent position on-chain)
           (list :kind "user_message" :turn-id turn-id
                 :parent-turn-id parent :log-position position
                 :on-head-chain on-chain))
         (tool (turn-id position on-chain)
           (list :kind "tool_result" :turn-id turn-id :tool-call-id "c"
                 :log-position position :on-head-chain on-chain))
         (shape (entries)
           (mapcar (lambda (row)
                     (list (getf row :turn-id) (getf row :kind)
                           (getf row :depth) (getf row :fork) (getf row :lanes)))
                   (nlk:checkpoint-tree-order entries))))
    ;; The operator's picture: yo, yo1, yo2 first; then, continuing from
    ;; yo1, hola! and que? — the head.
    (let ((entries (list (prompt "yo" nil 1 t)
                         (prompt "yo1" "yo" 2 t)
                         (tool "yo1" 3 t)
                         (prompt "yo2" "yo1" 4 nil)
                         (prompt "hola" "yo1" 5 t)
                         (prompt "que" "hola" 6 t)
                         (tool "que" 7 t)
                         (tool "t0" 0 nil))))
      (is (equal '(("t0" "tool_result" 0 nil ())
                   ("yo" "user_message" 0 nil ())
                   ("yo1" "user_message" 0 nil ())
                   ("yo1" "tool_result" 0 nil ())
                   ("hola" "user_message" 1 t (nil))
                   ("que" "user_message" 1 nil (nil))
                   ("que" "tool_result" 1 nil (nil))
                   ("yo2" "user_message" 0 nil ()))
                 (shape (reverse entries))))
      (is (equal (shape entries)
                 (shape (mapcar (lambda (entry)
                                  (append (list :on-head-chain
                                                (equal (getf entry :turn-id) "yo2"))
                                          entry))
                                entries))))
      ;; x continued from hola! after que? — a line inside the line — forks
      ;; off one lane deeper, right under hola!, before que?.
      (is (equal '(("yo" "user_message" 0 nil ())
                   ("yo1" "user_message" 0 nil ())
                   ("hola" "user_message" 1 t (nil))
                   ("x" "user_message" 2 t (nil nil))
                   ("que" "user_message" 1 nil (nil))
                   ("yo2" "user_message" 0 nil ()))
                 (shape (list (prompt "yo" nil 1 nil) (prompt "yo1" "yo" 2 nil)
                              (prompt "yo2" "yo1" 3 nil) (prompt "hola" "yo1" 4 nil)
                              (prompt "que" "hola" 5 nil) (prompt "x" "hola" 6 t)))))
      ;; Three lines off turn1 — yay, no; hello!, bye!; no! —
      ;; then turn2, the oldest child, continuing the trunk. The lane stays
      ;; open through the first two lines and closes on the last fork.
      (is (equal '(("yo" "user_message" 0 nil ())
                   ("turn1" "user_message" 0 nil ())
                   ("turn1" "tool_result" 0 nil ())
                   ("yay" "user_message" 1 t (t))
                   ("no" "user_message" 1 nil (t))
                   ("hello" "user_message" 1 t (t))
                   ("bye" "user_message" 1 nil (t))
                   ("no!" "user_message" 1 t (nil))
                   ("turn2" "user_message" 0 nil ()))
                 (shape (list (prompt "yo" nil 1 t) (prompt "turn1" "yo" 2 t)
                              (tool "turn1" 3 t) (prompt "turn2" "turn1" 4 nil)
                              (prompt "yay" "turn1" 5 nil) (prompt "no" "yay" 6 nil)
                              (prompt "hello" "turn1" 7 nil) (prompt "bye" "hello" 8 nil)
                              (prompt "no!" "turn1" 9 t)))))
      ;; Roots: a second beginning after a rewind to the root is a line one
      ;; lane deep above the first; a parent the limit cut leaves its
      ;; children as roots under the same rule.
      (is (equal '(("b" "user_message" 1 t (nil)) ("a" "user_message" 0 nil ()))
                 (shape (list (prompt "a" nil 1 nil) (prompt "b" nil 2 t)))))
      (is (equal '(("yo1" "user_message" 0 nil ())
                   ("yo1" "tool_result" 0 nil ())
                   ("hola" "user_message" 1 t (nil))
                   ("que" "user_message" 1 nil (nil))
                   ("que" "tool_result" 1 nil (nil))
                   ("yo2" "user_message" 0 nil ()))
                 (shape (subseq entries 1 7)))))))

(deftest tree-lane-prefix-draws-the-lanes ()
  ;; The glyph cells in front of a row: nothing on the trunk; on a fork row
  ;; `├ ' while more lines fork off below and `╰ ' on the last; down a lane
  ;; `│ ' while it is open and blank once it closed; nested lanes stack.
  (is-each (nle::tree-lane-prefix)
    (nil '() "" "the trunk wears nothing")
    (t '(t) "├ " "a fork with more below")
    (t '(nil) "╰ " "the last fork")
    (nil '(t) "│ " "a row down an open lane")
    (nil '(nil) "  " "a row down a closed lane")
    (t '(t nil) "│ ╰ " "a last fork inside an open lane")
    (nil '(nil t) "  │ " "a row down an open lane inside a closed one")))

(defun %tree-row (kind turn-id chain &rest pairs)
  "A /tree listing row of KIND under TURN-ID, a turn of CHAIN -- the head chain,
head first -- or of a branch off it, its capture cp-TURN-ID changing two files;
PAIRS ride beside, a key of theirs over the row's own."
  (apply #'nlk::make-json-object
         "kind" kind "turn_id" turn-id
         "is_head" (equal turn-id (first chain))
         "on_head_chain" (and (member turn-id chain :test #'equal) t)
         "log_position" 0
         "checkpoint_id" (format nil "cp-~a" turn-id)
         "file_change_count" 2
         pairs))

(defun %tree-prompt (turn-id number chain &rest pairs)
  "The %TREE-ROW of TURN-ID's prompt, `prompt NUMBER', asked as turn NUMBER."
  (apply #'%tree-row "user_message" turn-id chain
         "prompt" (format nil "prompt ~a" number)
         "turn_number" number "parent_turn_id" ""
         pairs))

(defun %tree-tool (turn-id chain &optional (name "eval"))
  "The %TREE-ROW of a NAME call under TURN-ID."
  (%tree-row "tool_result" turn-id chain
             "tool_call_id" (format nil "call-~a" name)
             "tool_name" name))

(defmacro with-tree-listing ((model session-id &rest stubs) &body body)
  "BODY over MODEL, a detached shell on SESSION-ID whose /tree listing is
LISTING's -- (HEAD ROW...), SETF by BODY -- and whose OPEN-TREE opens the
picker, under STUBS too."
  `(let ((,model (detached-shell ,session-id))
         (listing nil))
     (flet ((open-tree ()
              (nle::show-status ,model nil)
              (nle::slash-tree-op ,model)))
       (with-stubbed-fdefinitions
           ((nle::gateway-model-http-call (model method path body)
              (nlk::make-json-object "session_id" ,session-id "head_turn_id" (first listing)
                                     "checkpoints" (coerce (rest listing) 'vector)))
            ,@stubs)
         ,@body))))

(deftest checkpoint-tree-picker-returns-to-a-branch-turn (let ((sent '())))
  ;; The reported loss: a rewind from turn 3 to turn 1 (Enter on turn 2's
  ;; prompt) left turns 2 and 3 as branch rows, and Enter on turn 3's row
  ;; rewound to BEFORE it — head on turn 2, prompt 3 in the composer — so
  ;; turn 3 itself, answer included, was reachable through nothing but a
  ;; /redo whose origin that second move had already overwritten. A branch
  ;; prompt row returns to its turn (the turn boundary, no confirm stage);
  ;; only a prompt on the head chain rewinds, through the confirm stage.
  (with-tree-listing (model "tree-return"
                      (nle::send-navigate-checkpoint
                       (ws session-id &key target-turn-id boundary
                           direction (restore-files t))
                       (push (list target-turn-id boundary restore-files direction)
                             sent)))
    (flet ((enter-at (index &rest keys)
             ;; Absolute: the picker opens on the head row, so the walk
             ;; goes from wherever the selection stands. KEYS follow on the
             ;; menu the Enter opens.
             (setf (nle::picker-dialog-selection model.dialog)
                   index)
             (apply #'dialog-keys model :enter keys)))
      ;; t1 - t2 - t3 first; the operator continued from t2 and asked t4,
      ;; the head: the listing lays t4 out right under t2 at depth 1, and
      ;; t3 — the older child — continues the trunk after it.
      (let ((chain '("t4" "t2" "t1")))
        (setf listing (list "t4" (%tree-prompt "t1" 1 chain)
                            (%tree-prompt "t2" 2 chain)
                            (%tree-tool "t2" chain)
                            (%tree-prompt "t4" 3 chain "depth" 1 "fork" t "lanes" (vector nil))
                            (%tree-prompt "t3" 3 chain))))
      (is (null (open-tree)) "the picker opens without a notice")
      (is-present (dialog (nle::chat-model-dialog model)) "the tree opened"
        (let ((rows (nle::picker-dialog-rows dialog)))
          ;; The fold's own default: one row per turn, every turn closed —
          ;; t2's tool row is held back under its `▸ ' until the operator
          ;; opens it — and the selection opens on the head row.
          (is (equal '("○ prompt 1" "▸ ○ prompt 2" "╰ ● prompt 3"
                       "◇ prompt 3")
                     (mapcar #'nle::picker-row-label rows)))
          (is (= 2 (nle::picker-dialog-selection dialog)) "on the head row")
          (is (equal "turn 3 · branch" (nle::picker-row-detail (fourth rows))))
          (is (not (getf (nle::picker-row-value (fourth rows)) :on-chain)))
          (is (equal "  ·  2 file changes" (nle::picker-row-suffix (fourth rows))))
          (is (equal "turn 3 · branch"
                     (getf (nle::picker-row-value (fourth rows)) :detail)))
          (is (getf (nle::picker-row-value (first rows)) :on-chain))))
      ;; Enter on the branch row opens the menu; its first move, continue,
      ;; lands the head on turn 3 itself — the turn boundary, conversation
      ;; only — the branch returned to.
      (enter-at 3)
      (is (null sent) "the menu fires nothing yet")
      (is-present (dialog (nle::chat-model-dialog model))
          "Enter on a branch row opens the confirm stage"
          (is (equal "Checkpoint" (nle::picker-dialog-title dialog)) "its tab")
          (is (equal " turn 3 · branch" (nle::picker-dialog-context dialog)))
          (is (equal "Continue from this turn"
                     (nle::picker-row-label (first (nle::picker-dialog-rows dialog))))))
      (nle::picker-dialog-key model :enter)
      (is (equal '(("t3" "turn" nil nil)) sent))
      (is (null model.dialog) "and closes the menu")
      ;; An on-chain prompt that is not the head continues the same way:
      ;; the next prompt will branch from it.
      (open-tree)
      (enter-at 1 :enter)
      (is (equal '("t2" "turn" nil nil) (first sent)))
      ;; The head's own prompt row offers the rewind alone — the picker's
      ;; /undo — and its first option rewinds conversation only.
      (open-tree)
      (enter-at 2)
      (is (= 2 (length sent)) "the menu fires nothing yet")
      (is-present (dialog (nle::chat-model-dialog model))
          "Enter on the head row opens the confirm stage"
          (is (equal '("Rewind to before this message"
                       "Rewind to before this message + code changes (2 files changed)")
                     (row-labels dialog)))
          (is (equal " turn 3 · head" (nle::picker-dialog-context dialog))))
      (nle::picker-dialog-key model :enter)
      (is (equal '("t4" "user_message" nil nil) (first sent)))
      ;; Back on turn 3 with every prompt on the chain: the third move on
      ;; turn 2 rewinds to before it with the code changes.
      (let ((chain '("t3" "t2" "t1")))
        (setf listing (list "t3" (%tree-prompt "t1" 1 chain)
                            (%tree-prompt "t2" 2 chain)
                            (%tree-tool "t2" chain)
                            (%tree-prompt "t3" 3 chain))))
      (open-tree)
      (is-present (dialog (nle::chat-model-dialog model)) "the tree reopened"
        (is (equal '("○ prompt 1" "▸ ○ prompt 2" "● prompt 3") (row-labels dialog)))
        (is (= 2 (nle::picker-dialog-selection dialog))))
      (enter-at 1 :down :down :enter)
      (is (equal '("t2" "user_message" t nil) (first sent))))))

(deftest checkpoint-tree-folds-turns-and-opens-their-tool-rows ()
  ;; The listing is one row per checkpoint — 292 rows for this session's 21
  ;; turns — and the dialog folds it to one row per turn, every turn closed.
  ;; Right opens the highlighted turn's tool rows under it; Left closes the
  ;; turn, or — on a tool row — the turn it hangs under, parking the cursor
  ;; there. A box holding a query declines the gesture: the rows on screen are
  ;; the search's, not the fold's.
  (with-tree-listing (model "tree-fold")
    ;; t1 holds two tool rows, t2 one, and the head is t2.
    (let ((chain '("t2" "t1")))
      (setf listing (list "t2" (%tree-prompt "t1" 1 chain)
                          (%tree-tool "t1" chain)
                          (%tree-tool "t1" chain "shell")
                          (%tree-prompt "t2" 2 chain)
                          (%tree-tool "t2" chain))))
    (open-tree)
    (is-present (dialog (nle::chat-model-dialog model)) "the tree opened"
      (is (equal '("▸ ○ prompt 1" "▸ ● prompt 2") (row-labels dialog)) "one row per turn, every turn closed")
      (is (= 1 (nle::picker-dialog-selection dialog)) "on the head row")
      (let ((turn-row (first (nle::picker-dialog-rows dialog))))
        (is (equal "turn 1 · 2 calls"
                   (nle::picker-row-detail turn-row)) "a closed turn says how many calls it holds back")
        (is (equal "turn 1"
                   (getf (nle::picker-row-value turn-row) :detail)) "the picked row's descriptor stays clean for the confirm stage"))
      ;; Right opens the highlighted turn under itself; the cursor stays.
      (dialog-keys model :up :right)
      (is-shape dialog (row-labels '("▾ ○ prompt 1" "    · eval" "    · shell" "▸ ● prompt 2"))
        (nle::picker-dialog-selection = 0 "the cursor stays on the turn"))
      ;; Left on a tool row closes the turn it hangs under, and parks
      ;; the cursor there.
      (dialog-keys model :down :left)
      (is (equal '("▸ ○ prompt 1" "▸ ● prompt 2") (row-labels dialog)))
      (is (= 0 (nle::picker-dialog-selection dialog)) "the cursor parks on the turn")
      ;; Right then Left on the turn's own row is the same close.
      (dialog-keys model :right :left)
      (is (equal '("▸ ○ prompt 1" "▸ ● prompt 2") (row-labels dialog)))
      ;; The head turn opens the same way, and a box holding a query
      ;; declines the gesture: those rows are the search's, not the
      ;; fold's.
      (dialog-keys model :down :right)
      (is (equal '("▸ ○ prompt 1" "▾ ● prompt 2" "    · eval") (row-labels dialog)))
      (setf (nle::picker-dialog-search dialog) "prompt")
      (nle::picker-dialog-key model :left)
      (is (equal '("▸ ○ prompt 1" "▾ ● prompt 2" "    · eval") (row-labels dialog)) "the fold declines while a query is typed"))))

(deftest checkpoint-tree-paints-the-metro ()
  ;; The /tree rows paint the Metro (variant A): the
  ;; turns in the order they ran on one rail per line, the head's line on the
  ;; left and a branch joined to the turn it left by a fork; each row its clock
  ;; time and, flush right, how it ended, its calls and its span; a
  ;; reflection's recap folded under its turn behind a pen, the head with it.
  ;; Every painted row is the panel's width, the right lane in one column, a
  ;; selected row's files chrome and a narrow panel included. Enter's confirm
  ;; stage draws the highlighted move on the same graph and says it in a
  ;; sentence, every option still in view.
  (with-tree-listing (model "tree-metro")
    (flet ((at (minute &optional (second 0))
             (format nil "2026-10-16T12:~2,'0d:~2,'0d.000Z" minute second)))
      ;; t1 asked first; t2 left it and was cancelled; t3 continued t1, and
      ;; the recap r under t3 is the head.
      (let ((chain '("r" "t3" "t1")))
        (setf listing
              (list "r"
                    (%tree-prompt "t1" 1 chain "log_position" 1 "started_at" (at 0)
                                  "ended_at" (at 1 5) "ending" "completed" "steers" 0)
                    (%tree-row "tool_result" "t1" chain "log_position" 2 "tool_call_id" "c1"
                               "tool_name" "eval" "tool_command" "(+ 1 2)"
                               "started_at" (at 0 10) "ended_at" (at 0 12))
                    (%tree-prompt "t2" 2 chain "log_position" 3 "parent_turn_id" "t1"
                                  "started_at" (at 2) "ended_at" (at 2 3) "ending" "cancelled" "steers" 0)
                    (%tree-prompt "t3" 2 chain "log_position" 4 "parent_turn_id" "t1"
                                  "started_at" (at 5) "ended_at" (at 7) "ending" "completed" "steers" 2)
                    (%tree-row "user_message" "r" chain "log_position" 5 "parent_turn_id" "t3"
                               "origin" "reflection" "prompt" "[experience] recap"
                               "answer" "Recap: all clear." "started_at" (at 8) "ended_at" (at 8 30)
                               "ending" "completed" "steers" 0))))
      (setf (nle::chat-model-term-width model) 100
            (nle::chat-model-term-height model) 50)
      (open-tree)
      (flet ((painted (&optional (width 100))
               (mapcar #'nle::plain-frame-row (nle::dialog-panel-rows model width nil)))
             (clock (minute) (nle::metro-clock (nle::iso-8601-ms (at minute)))))
        (is-present (dialog (nle::chat-model-dialog model)) "the tree opened"
          (is (equal '("t1" "t2" "t3")
                     (mapcar (lambda (row) (getf (nle::picker-row-value row) :turn-id))
                             (nle::picker-dialog-rows dialog))) "one row per turn in the order they ran, the recap folded")
          (is (= 2 (nle::picker-dialog-selection dialog)) "on t3, whose recap is the head")
          (is (equal (format nil " 3 turns · 2 lines · ~a → ~a" (clock 0) (clock 7))
                     (nle::picker-dialog-context dialog)) "the tab says the graph's measure")
          (let* ((rows (painted))
                 (t1 (find-if (lambda (row) (search "prompt 1" row)) rows))
                 (t2 (find-if (lambda (row) (search "re-asked" row)) rows))
                 (t3 (find-if (lambda (row) (search "HEAD" row)) rows)))
            (is (every (lambda (row) (= 100 (nle::text-display-width row))) rows) "every painted row is the panel's width")
            (is (and t1 (search (format nil "~a  ●" (clock 0)) t1) (search "1 call" t1) (search "1m 05s" t1)) "t1 on the head's line: its clock, its call, its span")
            (is (and t2 (search "├─◦" t2) (search "re-asked ↓" t2)) "t2 forks off t1's rail, cancelled and asked again as t3")
            (is (and t3 (search "◉" t3) (search "+2 steers" t3) (search "✎" t3)) "t3 is the head, with its steers and its recap's pen")
            (is (and t1 t3 (= (+ (search "1m 05s" t1) 6) (+ (search "2m 00s" t3) 6))) "the spans end in one column")
            ;; No span bar: blank between a turn's calls and its time, and no
            ;; eighth-block cell on any row but the selected row's edge.
            (is (and t1 (string= "" (string-trim " " (subseq t1 (+ (search "1 call" t1) 6) (search "1m 05s" t1))))) "nothing stands between the calls and the time")
            (is (notany (lambda (row) (find-if (lambda (char) (find char "▏▎▍▋▊▉█")) row)) rows) "no row draws a bar"))
          ;; The selected row's files chrome rides the ask, and a narrow panel
          ;; clips the ask, never the row's width.
          (setf (nle::picker-row-suffix (third (nle::picker-dialog-rows dialog)))
                (nle::checkpoint-files-suffix 2 '("a.lisp" "b.lisp")))
          (is (find-if (lambda (row) (search "2 file changes: a.lisp, b.lisp" row)) (painted 140)) "the selected row shows its files where they fit")
          (is (every (lambda (row) (= 140 (nle::text-display-width row))) (painted 140)) "and keeps its width")
          (is (every (lambda (row) (= 64 (nle::text-display-width row))) (painted 64)) "a narrow panel keeps every row its width")
          ;; Right opens t3 over its recap: a heading the arrows pass over.
          (dialog-keys model :right)
          (let ((recap (fourth (nle::picker-dialog-rows dialog))))
            (is (and recap (nle::picker-row-heading recap)) "the recap row is never selected")
            (is (find-if (lambda (row) (search "✎ recap  Recap: all clear." row)) (painted)) "it says what the recap said"))
          ;; Right on t1 opens its call: how long it ran, flush right, no bar.
          (dialog-keys model :up :up :right)
          (let ((call (find-if (lambda (row) (search "┆ eval" row)) (painted))))
            (is (and call (search "2.0s" call) (notany (lambda (char) (find char "▏▎▍▋▊▉█")) call)) "a call row says how long it ran, with no bar")))
        ;; Enter on t1: the confirm stage draws each move on the graph.
        (dialog-keys model :enter)
        (is-present (dialog (nle::chat-model-dialog model)) "the confirm stage opened"
          (flet ((summary () (mapcar #'nle::segs-text (nle::picker-dialog-summary-rows dialog))))
            (is (equal "Checkpoint" (nle::picker-dialog-title dialog)))
            (is (find-if (lambda (row) (search " here  prompt 1" row)) (summary)) "continue lands the head here")
            (is (find-if (lambda (row) (search "leaves head  prompt 2" row)) (summary)) "t3 leaves the head's line")
            (is (find-if (lambda (row) (search "Continue from turn 1: the head moves here; 1 turn (0 calls, 2m 00s) stays as a branch." row)) (summary)) "and the sentence says so")
            (let ((height (length (summary))))
              (dialog-keys model :down)
              (is (find-if (lambda (row) (search " before this  prompt 1" row)) (summary)) "the rewind goes to before it")
              (is (find-if (lambda (row) (search "2 turns leave the head line" row)) (summary)) "taking both turns off the head's line")
              (dialog-keys model :down)
              (is (find-if (lambda (row) (search "2 files restored." row)) (summary)) "the files rewind restores them")
              (is (= height (length (summary))) "every option draws as many rows"))
            (is (find-if (lambda (row) (search "Continue from this turn" row)) (painted)) "the first option stays in view")
            (is (find-if (lambda (row) (search "+ code changes" row)) (painted)) "and so does the last")))
        (dialog-keys model :escape)
        (is (null (nle::chat-model-dialog model)) "Esc moves nothing and closes")))))

(deftest checkpoint-files-preview-lazy-highlight ()
  ;; The row's files chrome and its count are ONE answer: the listing carries
  ;; no count at all (a count it did not compute would read as `nothing
  ;; changed'), so a highlighted row's fetch lands the paths and the number
  ;; together — the confirm stage's `(N files changed)' reads it back.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "tree-files"))
        (fetches '())
        (fetch-ok t)
        ;; The suite's own pacing: a hand flush delivers at once.
        (nle::*picker-highlight-rest* 0))
    (labels ((row (checkpoint-id)
               (nle::make-picker-row
                :label (format nil "row-~a" (or checkpoint-id "none"))
                :value (list :kind "user_message" :turn-id "t" :head-p nil
                             :checkpoint-id checkpoint-id)))
             (highlight (model row)
               (declare (ignore model))
               (nle::checkpoint-row-ensure-files
                row (lambda (checkpoint-id)
                      (push checkpoint-id fetches)
                      (if fetch-ok
                          (values (list "a.zig" "b.zig") t)
                          (values nil nil)))))
             (open-tree (&rest rows)
               (nle::open-picker-dialog model "Tree" rows
                                        (lambda (model value) (declare (ignore model value)) nil)
                                        :on-highlight #'highlight))
             (first-row ()
               (first (nle::picker-dialog-rows model.dialog)))
             (first-suffix () (nle::picker-row-suffix (first-row)))
             (press (&rest keys)
               (apply #'dialog-keys model keys)
               (nle::picker-flush-highlight model)))
      (open-tree (row "cp-a") (row "cp-b") (row nil))
      (is (equal '("cp-a") fetches) "the opened row fetched its files")
      (is (equal "  ·  2 file changes: a.zig, b.zig" (first-suffix)))
      (is (= 2 (getf (nle::picker-row-value (first-row))
                     :file-change-count)) "the fetch stamped the count")
      ;; Arrow moves stage the highlight and the frame clock's tick delivers
      ;; it (flushed by hand here); a loaded row never refetches. FETCHES is
      ;; newest-first (the handler pushes).
      (press :down)
      (is (equal '("cp-b" "cp-a") fetches) "a capture-carrying row fetches once")
      (press :down)
      (is (equal '("cp-b" "cp-a") fetches) "a capture-less row never fetches")
      (press :up :up)
      (is (equal '("cp-b" "cp-a") fetches))
      (setf model.dialog nil)
      ;; A failed fetch leaves the row bare and retries on re-highlight.
      (setf fetch-ok nil fetches '())
      (open-tree (row "cp-c"))
      (is (equal '("cp-c") fetches) "the failing fetch was attempted")
      (is (equal "" (first-suffix)) "and leaves the row bare")
      (setf fetch-ok t)
      (press :down)
      (is (equal '("cp-c" "cp-c") fetches))
      (setf model.dialog nil))))

(defun %seed-capture-turn (session-id snap checkpoint-id input)
  "Admit and complete one turn on SESSION-ID, capturing the workspace tree
(when SNAP is live) as a user_message capture named CHECKPOINT-ID."
  ;; => the turn id.
  (let ((turn (nlk:admit-turn session-id (nlk::make-durable-id "command")
                              input)))
    (nlk:record-workspace-capture session-id
                                  :turn-id turn.turn-id
                                  :boundary-kind "user_message"
                                  :tree-sha (and snap (%anchored-capture snap checkpoint-id))
                                  :checkpoint-id checkpoint-id)
    (nlk:complete-turn turn nil)
    turn.turn-id))

(defun %seed-two-captures (session-id snap dir prefix)
  "Capture a.txt as `one' under checkpoint PREFIX-1, then a.txt as `two' and a
new b.txt under PREFIX-2: the two-turn workspace the file-count gates start
from. => the two turn ids."
  (%write-file dir "a.txt" "one")
  (let ((first (%seed-capture-turn session-id snap (format nil "~a-1" prefix) "first")))
    (%write-file dir "a.txt" "two" "b.txt" "new")
    (values first
            (%seed-capture-turn session-id snap (format nil "~a-2" prefix) "second"))))

(defmacro with-capture-workspace ((snap dir session-id tag) &body body)
  "Run BODY in a fresh workspace DIR, tagged TAG, that a new durable session
SESSION-ID has for its cwd, with SNAP its snapshot store (NIL without git)."
  `(with-snapshot-workspace (,snap ,dir :session-id ,session-id :tag ,tag)
     (nlk:create-session :id ,session-id :cwd (namestring ,dir))
     ,@body))

(defun is-changed-files (session-id checkpoint-id known files known-why files-why)
  "Assert CHECKPOINT-CHANGED-FILES answers for CHECKPOINT-ID whether it is KNOWN
(KNOWN-WHY) and the FILES it changed, in name order (FILES-WHY)."
  (is-values (changed known-p) (nlk:checkpoint-changed-files session-id checkpoint-id)
    ((and known-p t) eq known known-why) ((sort (copy-list changed) #'string<) files files-why)))

(defun %row-file-count (session-id checkpoint-id)
  "The count the live picker path stamps on a row: the files route's own
answer, |CHECKPOINT-CHANGED-FILES|. NIL ids (a row with no capture) count 0."
  (if checkpoint-id
      (nlk:bind (((files _) (nlk:checkpoint-changed-files session-id checkpoint-id)))
        (length files))
      0))

(defun %counted-prompts (session-id)
  "SESSION-ID's user_message checkpoints with each row's file count as the
picker derives it: one files read per row, the number the suffix prints."
  (%entries-of-kind
   "user_message"
   (loop for entry in (nlk:session-checkpoints session-id)
         collect (append entry
                         (list :file-change-count
                               (%row-file-count session-id (getf entry :checkpoint-id)))))))

(deftest checkpoint-changed-files-derives-per-checkpoint-diff (with-temp-store (store))
  (with-capture-workspace (snap dir "cp-files" "cpfiles")
    (cond ((nlk:git-available-p)
           (%seed-two-captures "cp-files" snap dir "cpf")
           ;; No further writes: cpf-3's predecessor tree equals
           ;; the live worktree.
           (%seed-capture-turn "cp-files" snap "cpf-3" "third")
           (with-workspace-captures (t)
             (is-changed-files "cp-files" "cpf-2" t '("a.txt" "b.txt")
                               "a recorded checkpoint is known"
                               "cpf-2 diffs the worktree against its predecessor tree")
             (is-changed-files "cp-files" "cpf-3" t nil
                               "cpf-3 is known" "an up-to-date predecessor tree yields no files")
             (is-changed-files "cp-files" "cpf-missing" nil nil
                               "unknown checkpoint ids answer known-p NIL" "and carry no files")
             (let ((users (%counted-prompts "cp-files")))
               (is (equal '("cpf-1" "cpf-2" "cpf-3") (%entry-values :checkpoint-id users)))
               (is (equal '(2 2 0) (%entry-values :file-change-count users)))))
           ;; The capture kill switch gates the read-side diff too:
           ;; absent knowledge degrades to empty, never wrong.
           (with-workspace-captures (nil)
             (is-changed-files "cp-files" "cpf-2" t nil
                               "the checkpoint is still known"
                               "captures off => empty list rather than wrong")))
          ;; Git-less host: the degradation contract is the test.
          (t (%seed-capture-turn "cp-files" nil "cpf-1" "first")
             (with-workspace-captures (t)
               (is-changed-files "cp-files" "cpf-1" t nil
                                 "a recorded checkpoint is known without git"
                                 "no git degrades to an empty list"))))))

(deftest checkpoint-counts-exclude-foreign-drift (with-temp-store (store))
  ;; A session whose first capture embalms junk a shared workspace already
  ;; carried, cleaned by foreign hands afterwards.
  ;; The advertised counts must be what a restore would ACTUALLY touch —
  ;; foreign drift is conflict-refused by the restore engine, so it never
  ;; inflates a row.
  (with-capture-workspace (snap dir "cp-drift" "cpdrift")
    (when (nlk:git-available-p)
      ;; Pre-existing junk rides into the session's first capture.
      (%write-file dir "junk-a.txt" "left behind" "junk-b.txt" "left behind too"
                   "mine.txt" "session claim")
      (%seed-capture-turn "cp-drift" snap "cpd-1" "first")
      (%write-file dir "mine.txt" "session edit")
      (%seed-capture-turn "cp-drift" snap "cpd-2" "second")
      ;; Foreign hands clean the junk and add new junk after the
      ;; session's last capture — none of it ever claimed here.
      (%write-file dir "junk-a.txt" nil "junk-b.txt" nil "foreign-new.txt" "another session")
      (with-workspace-captures (t)
        (is-changed-files "cp-drift" "cpd-2" t '("mine.txt")
                          "the checkpoint is known"
                          "only the session's own restorable path is advertised")
        (is (equal '(1 1)
                   (%entry-values :file-change-count (%counted-prompts "cp-drift"))))))))

(deftest checkpoint-files-route-and-listing-counts (with-temp-gateway (port))
  (with-capture-workspace (snap dir "cp-http" "cphttp")
    (%seed-two-captures "cp-http" snap dir "cph")
    (is-route (port :get "/api/gateway/sessions/cp-http/checkpoints/cph-2/files" :token nil) 401
              "the files route requires the operator token")
    (with-workspace-captures (t)
      (with-gateway-http (port :get "/api/gateway/sessions/cp-http/checkpoints")
        (is (= 200 status) "the listing answers")
        (let ((second-entry (second (coerce (gethash "checkpoints" body) 'list))))
          (is (equal "cph-2" (gethash "checkpoint_id" second-entry)))
          ;; The listing derives no count: it says nothing about files rather
          ;; than claiming none changed. The number lives in the files route
          ;; (the picker stamps |files| into the row when it highlights it).
          (is (not (nth-value 1 (gethash "file_change_count"
                                         second-entry))) "no count rides the listing")
          (is (= (if snap 2 0) (%row-file-count "cp-http" "cph-2")) "the files answer IS the count")))

      (with-gateway-http (port :get "/api/gateway/sessions/cp-http/checkpoints/cph-2/files")
        (is (= 200 status) "the files route answers")
        (is-table (expected key) (equal expected (gethash key body))
          ("cp-http" "session_id")
          ("cph-2" "checkpoint_id"))
        (is (equal (if snap '("a.txt" "b.txt") '())
                   (sort (coerce (gethash "files" body) 'list) #'string<))))
      (is-route (port :get "/api/gateway/sessions/cp-http/checkpoints/nope/files") 404
                "unknown checkpoints 404" "unknown_checkpoint"))
    ;; Captures off (the suite default): the same route degrades
    ;; to an empty list — never a wrong one.
    (with-gateway-http (port :get "/api/gateway/sessions/cp-http/checkpoints/cph-2/files")
      (is (= 200 status) "captures off still answers")
      (is (zerop (length (gethash "files" body)))))))

(deftest checkpoint-navigate-restore-files-scope (with-temp-gateway (port))
  (with-capture-workspace (snap dir "nav-scope" "cpscope")
    (multiple-value-bind (turn1 turn2) (%seed-two-captures "nav-scope" snap dir "cps")
      (flet ((navigate (command-id &rest extra)
               (%command-result
                (apply #'%navigate-frames port "nav-scope"
                       command-id extra))))
        (with-workspace-captures (t)
          ;; Conversation-only: restore_files false skips the
          ;; workspace restore while the head still moves.
          (is-present (result (navigate "scope-1"
                                        "target_turn_id" turn2
                                        "boundary" "user_message"
                                        "restore_files" nil))
              "conversation-only navigation answered"
              (is-shape result ("status" "accepted") ((:string "response" "workspace") "skipped")))
          (is (equal turn1 (nlk:session-head-turn-id "nav-scope")))
          (is (equal "two" (%read-file dir "a.txt")))
          (is (equal "new" (%read-file dir "b.txt")))
          (when snap
            ;; Redo to the abandoned tip, then the same rewind
            ;; with the default scope restores the files.
            (is (equal "accepted" (nlk:json-value (navigate "scope-2" "direction" "redo")
                                                  :string "status")))
            (is (equal turn2 (nlk:session-head-turn-id "nav-scope")))
            (is-present (result (navigate "scope-3"
                                          "target_turn_id" turn2
                                          "boundary" "user_message"))
                "default-scope navigation answered"
                (is (equal "restored" (nlk:json-value result :string "response" "workspace"))))
            (is (equal "one" (%read-file dir "a.txt")))
            (is (null (%read-file dir "b.txt")))))))))

(deftest checkpoint-rewind-after-a-redo-restores-the-files (with-temp-gateway (port))
  ;; Rewind with the files, redo, rewind again: the redo records the tree it
  ;; left as the first turn's newest capture, and that must not become the
  ;; baseline the second rewind measures disk against -- the head's own files
  ;; would read as foreign drift and stay on disk (found through the web page).
  (with-capture-workspace (snap dir "nav-again" "cpagain")
    (nlk:bind (((_ turn2) (%seed-two-captures "nav-again" snap dir "cpa")))
      (flet ((navigate (command-id &rest extra)
               (%command-result (apply #'%navigate-frames port "nav-again" command-id extra))))
        (with-workspace-captures (t)
          (when snap
            (dolist (step '(("again-1" "target_turn_id" :turn2 "boundary" "user_message")
                            ("again-2" "direction" "redo")
                            ("again-3" "target_turn_id" :turn2 "boundary" "user_message")))
              (is-present (result (apply #'navigate (substitute turn2 :turn2 step))) (first step)
                  (is-shape result ("status" "accepted")
                            ((:string "response" "workspace") "restored")
                            ((:integer "response" "conflicts") 0))))
            (is (equal "one" (%read-file dir "a.txt")) "the second rewind restored a.txt")
            (is (null (%read-file dir "b.txt")) "and took b.txt away again")))))))

(deftest checkpoint-listing-limit-is-the-full-listings-own-tail (with-temp-store (store))
  ;; LIMIT exists to keep the picker's open off the whole log (446 ms of scan
  ;; and payload decode on the widest session measured, against 35 ms for the
  ;; tail). The gate is exactness, not speed: the limited answer must equal
  ;; the full listing's own tail — same rows, same capture facts, same tree
  ;; order — or the picker would draw a different tree than the session has.
  (let ((full nil))
    (flet ((check (session-id)
             (setf full (nlk:session-checkpoints session-id))
             (dolist (n (list 1 2 3 4 5 (length full) (+ 4 (length full))))
               (is (equal (last full (min n (length full)))
                          (nlk:session-checkpoints session-id :limit n))))))
      (with-capture-workspace (snap dir "cp-tail" "cptail")
        (%seed-two-captures "cp-tail" snap dir "cpt")
        (%seed-capture-turn "cp-tail" snap "cpt-3" "third")
        (with-workspace-captures (t)
          (check "cp-tail")
          ;; The tree the picker draws is laid out over the tail the same way
          ;; it is over the full listing cut the same way.
          (is (equal (nlk:checkpoint-tree-order (last full 3))
                     (nlk:checkpoint-tree-order (nlk:session-checkpoints "cp-tail" :limit 3))))))
      ;; A forked child contributes an ancestor segment: the merged tail is
      ;; still the full listing's tail, which is the case the per-segment cut
      ;; exists for.
      (with-seeded-turns ("cp-tail-parent" '("one" "two") parent-anchor)
        (nlk:create-session :id "cp-tail-child" :parent "cp-tail-parent"
                            :anchor-turn-id parent-anchor)
        (with-seeded-turns ("cp-tail-child" '("child-one" "child-two"))
          (check "cp-tail-child"))))))

(deftest checkpoint-file-counts-match-the-one-at-a-time-derivation (with-temp-store (store))
  ;; A row's count is the files answer, and that answer goes down as ONE
  ;; child process — the staging plus every distinct undo-target tree's diff
  ;; in a batch — while a hand derivation spends one diff per tree. The gate
  ;; is that the two agree on a listing whose rows carry several distinct
  ;; trees, including the newest — whose target IS the conflict baseline, so
  ;; it can only count zero, and does so without a diff of its own.
  (with-capture-workspace (snap dir "cp-batch" "cpbatch")
    (%seed-two-captures "cp-batch" snap dir "cpb")
    (%write-file dir "c.txt" "third")
    (%seed-capture-turn "cp-batch" snap "cpb-3" "third")
    ;; No writes after this one: its undo target is the head tree.
    (%seed-capture-turn "cp-batch" snap "cpb-4" "fourth")
    (with-workspace-captures (t)
      (let* ((live (%entry-values :file-change-count (%counted-prompts "cp-batch")))
             (captures (nlk::session-workspace-captures "cp-batch"))
             (parents (nlk::session-composed-parents "cp-batch"))
             (staged (nlk::checkpoint-diff-store "cp-batch"))
             (baseline (nlk::conflict-baseline-tree
                        captures parents (nlk:session-head-turn-id "cp-batch")))
             (conflicts (and staged
                             (nlk::path-set
                              (nlk::staged-changed-paths staged baseline))))
             (one-at-a-time
               (loop for entry in (nlk:session-checkpoints "cp-batch")
                     for id = (getf entry :checkpoint-id)
                     collect (if (and id staged conflicts
                                      (member id (%entry-values :checkpoint-id captures)
                                              :test #'equal))
                                 (count-if-not (lambda (record)
                                                 (gethash (getf record :path) conflicts))
                                               (nlk::staged-changed-paths
                                                staged (nlk::restore-tree-for-checkpoint
                                                        captures id parents)))
                                 0))))
        (is (equal one-at-a-time live) "the batched answer is the per-tree one")
        (if (nlk:git-available-p)
            (is (equal '(3 3 1 0) live))
            (is (every #'zerop live)))))))

(deftest checkpoint-listing-prefetch-answers-the-picker ()
  ;; The prefetch is consumed at GATEWAY-MODEL-HTTP-CALL, and only for the
  ;; listing path of this model's own session: this shell carries no endpoint,
  ;; so anything that reaches the dial refuses structurally and says so.
  (let* ((nle::*checkpoint-listing-prefetch* nil)
         (model (detached-shell "cp-prefetch"))
         (path (nle::checkpoint-listing-path model))
         (landed (nlk::make-json-object "session_id" "cp-prefetch")))
    (is (equal "/api/gateway/sessions/cp-prefetch/checkpoints" path))
    (flet ((plant (until)
             (setf nle::*checkpoint-listing-prefetch*
                   (list :path path :until until
                         :semaphore (bt2:make-semaphore :count 1)
                         :box (list landed))))
           (dial (request-path)
             (handler-case (nle::gateway-model-http-call
                            model :get request-path nil)
               (nle::gateway-http-error () :dialed))))
      (plant (+ (get-internal-real-time) internal-time-units-per-second))
      (is (eq landed (dial path)))
      (is (null nle::*checkpoint-listing-prefetch*))
      (plant (+ (get-internal-real-time) internal-time-units-per-second))
      (is (eq :dialed (dial "/api/gateway/sessions")))
      (is nle::*checkpoint-listing-prefetch*)
      (plant (get-internal-real-time))
      (is (eq :dialed (dial path)))
      (is (null nle::*checkpoint-listing-prefetch*)))))

(deftest checkpoint-listing-prefetch-arms-on-the-first-esc (with-gateway-session (port "cp-arm"))
  ;; The gesture's arming half is the seam: one Esc on an idle shell starts
  ;; the listing fetch, so the second Esc opens the picker on an answer that
  ;; is already home instead of waiting for the round trip.
  (let* ((nle::*checkpoint-listing-prefetch* nil)
         (model (gateway-shell port "cp-arm")))
    (nlt:update model (nlt:make-key-press-msg :code :escape))
    (is (nle::gateway-model-last-escape-at model))
    (let ((entry nle::*checkpoint-listing-prefetch*))
      (is entry "and starts the listing prefetch")
      (is (equal (nle::checkpoint-listing-path model) (getf entry :path))))
    (let ((answer (nle::gateway-model-http-call
                   model :get (nle::checkpoint-listing-path model) nil)))
      (is-shape answer (hash-table-p is "the prefetched listing lands") ("session_id" "cp-arm"))
      (is (null nle::*checkpoint-listing-prefetch*)))))
