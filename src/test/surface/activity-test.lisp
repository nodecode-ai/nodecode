;;;; activity-test.lisp --- the running sessions: the durable read, the frame
;;;; every shell folds, and the spinner and clock the /sessions row wears.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;
;;;;   R1. A session is running when it has an admitted turn without a
;;;;       terminal fact; the clock is the stamp on that turn's own
;;;;       TURN.STARTED fact; one read answers every session at once, and a
;;;;       settled turn leaves the map.
;;;;   R2. An attached shell receives the map whole as it subscribes and one
;;;;       organism.activity frame per change -- a frame that moved nothing
;;;;       says nothing, and a settled turn publishes the map without it.
;;;;   R3. The organism.activity frame decodes session-less, outside every
;;;;       session's sequence, and refuses a payload with no sessions array.
;;;;   R4. A /sessions row for a running session wears the live turn's own
;;;;       spinner and `running 3m 12s' -- the durable start, in the one
;;;;       clock dialect the shell's dialogs speak -- where a settled row
;;;;       wears its age and no mark. A row that is not running itself but
;;;;       has a running descendant wears the same spinner one tone down.
;;;;   R5. An open /sessions picker draws that live off the frame, and drops
;;;;       it again when the turn settles.

(in-package #:nodecode.test)

(nlk:access (grand nle::picker-row) (kid nle::picker-row) (running nle::picker-row)
            (settled nle::picker-row))

(defun iso-ago (seconds)
  "An ISO-8601 UTC stamp SECONDS before now, in the shape NLK:ISO-NOW
writes -- the fixture's way of saying `a turn that started 3m 12s ago'. A test
that reads a row's age or its day runs under WITH-CLOCK-AT-LOCAL-NOON, so no
fixture crosses a midnight."
  (nlk:iso-time (- (nlk:unix-now) seconds)))

(defun activity-sessions (frame)
  "The running sessions FRAME carries, as a list."
  (coerce (nlk:json-value frame :array "payload" "sessions") 'list))

(defun activity-row-for (id rows)
  "The row of ROWS whose picker value is ID."
  (find id rows :key #'nle::picker-row-value :test #'equal))

(defun activity-spinner-p (row)
  "Whether ROW's mark is one of the live turn's own spinner frames."
  (member (nle::picker-row-mark row)
          (coerce (cdr (assoc :default nle::*status-marks*)) 'list) :test #'equal))

(defun running-map (&rest runs)
  "The sessions array of an organism.activity payload for RUNS, each (ID SECONDS-AGO)."
  (map 'vector (lambda (run) (nlk:json-object "id" (first run) "started_at" (iso-ago (second run))))
       runs))

(defun directory-entry (id parent &key (title id) (age 600) (blocks 3) relation
                                        (directory (nle::workspace-directory)))
  "One /sessions directory row as the gateway's listing carries it, opened in
the shell's own folder unless DIRECTORY says otherwise."
  (nlk:make-json-object "id" id "title" title "parent_id" parent "relation" relation
                        "updated_at" (iso-ago age)
                        "working_directory" directory
                        "message_count" blocks))

;;; --- R1: the durable read ----------------------------------------------------

(deftest running-sessions-are-read-off-the-durable-turn (with-gateway-session (port "act-run"))
  (nlk:create-session :id "act-rest")
  (is (null (nlk:active-turn-starts)) "no turn in flight, nobody is running")
  (let ((turn (nlk:admit-turn "act-run" "act-run-cmd" "hello")))
    (is-let* ((rows (nlk:active-turn-starts)))
      ((mapcar #'car rows) '("act-run") "the session with the turn is")
      ((nle::iso-8601-ms (cdr (first rows))) is) ((assoc "act-rest" rows :test #'equal) null))
    (nlk:complete-turn turn nil)
    (is (null (nlk:active-turn-starts)))))

;;; --- R2: the wire -------------------------------------------------------------

(deftest running-sessions-reach-attached-peers-when-they-change ()
  (with-gateway-session (port "act-wire")
    ;; A shell attaching before any turn: the map it is handed is empty.
    (let ((activity (frames-of-type (subscribed-frames port "act-wire")
                                    "organism.activity")))
      (is (= 1 (length activity)) "the map rides the subscription, whole")
      (let ((frame (first activity)))
        (is (eql 0 (gethash "sequence" frame)) "outside every session's sequence")
        (is (null (gethash "session_id" frame)) "and session-less")
        (is (null (activity-sessions frame)) "an idle organism: nobody is running")))
    ;; Then a turn runs: the start reaches the attached peer, the same facts
    ;; again move nothing, and the settle takes the session off the map.
    (let* ((turn (nlk:admit-turn "act-wire" "act-wire-cmd" "hello"))
           (frames (frames-while (ws port :subscribe '("act-wire"))
                     (nle::publish-turn-facts "act-wire" 0)
                     (nle::publish-turn-facts "act-wire" 0)
                     (nlk:complete-turn turn nil)
                     (nle::publish-turn-facts "act-wire" 0)))
           (activity (frames-of-type frames "organism.activity"))
           (maps (mapcar #'activity-sessions activity)))
      (is (= 3 (length activity)))
      (is-present (row (wire-row "act-wire" (second maps)))
          "the started turn puts its session on the map"
          (is (nle::iso-8601-ms (nlk:json-value row :string "started_at"))))
      (is (null (third maps)) "and the settled turn takes it off again"))))

(defun snapshot-sessions (port session-id)
  "The running sessions a fresh client of PORT is handed as it subscribes to
SESSION-ID, as a list."
  (activity-sessions
   (first (frames-of-type (subscribed-frames port session-id) "organism.activity"))))

(deftest a-re-subscribe-is-handed-the-held-map-the-frames-keep ()
  ;; The snapshot behind a subscribe is the projection's own copy, not a
  ;; store read (the /sessions picker re-subscribes per highlight move): a
  ;; turn admitted but not yet published is not on it, and the lifecycle
  ;; frame that publishes the turn is what the next subscribe answers with.
  (with-gateway-session (port "act-held")
    (is (null (snapshot-sessions port "act-held")) "the first subscribe reads the log: nobody is running")
    (let ((turn (nlk:admit-turn "act-held" "act-held-cmd" "hello")))
      (is (null (snapshot-sessions port "act-held")) "an unpublished turn is not on the held map")
      (nle::publish-turn-facts "act-held" 0)
      (let ((ids (mapcar (lambda (row) (nlk:json-value row :string "id"))
                         (snapshot-sessions port "act-held"))))
        (is (equal '("act-held") ids) "the frame that published it is what a later subscribe is handed"))
      (nlk:complete-turn turn nil)
      (nle::publish-turn-facts "act-held" 0)
      (is (null (snapshot-sessions port "act-held")) "and the settle takes it off again"))))

;;; --- R3: the decode gate ------------------------------------------------------
;;; One case of protocol-test.lisp's PROTOCOL-ORGANISM table beside the other
;;; unsequenced kinds: PROTOCOL-ORGANISM-ACTIVITY-FRAME-DECODES-SESSION-LESS-AND-NEEDS-SESSIONS.

;;; --- R4: the row -------------------------------------------------------------

(deftest sessions-row-wears-the-spinner-and-the-durable-clock ()
  (is (equal "42s" (nle::running-clock-label 42000)) "the clock dialect, under a minute")
  (is (equal "3m 12s" (nle::running-clock-label 192000)) "in minutes and seconds")
  (is (equal "1h 04m" (nle::running-clock-label 3840000)) "and in hours")
  (let ((now-ms (nle::iso-8601-ms "2026-09-06T10:00:00.000Z")))
    (is (zerop (nle::running-elapsed-ms "2026-09-06T10:00:01.000Z" now-ms)))
    (is (null (nle::running-elapsed-ms "yesterday" now-ms)) "an unreadable stamp declines")
    (let* ((entry (directory-entry "s-a" nil :title "Fix the flaky test" :age 2 :blocks 4))
           (running (nle::session-picker-row (list :id "s-a" :entry entry) now-ms
                                             :started-at "2026-09-06T09:56:48.000Z"))
           (settled (nle::session-picker-row (list :id "s-a" :entry entry) now-ms)))
      (is-shape running (activity-spinner-p is) (.mark-tone eq :tool "in the live tone"))
      (is (search "3m 12s  Fix the flaky test" (session-row-text running)) "the elapsed time stands where the age does")
      (is (not (search "ago" (session-row-text running))) "and the age is not said beside it")
      (is (equal "" settled.mark) "a settled row wears no mark")
      (is (search "just now  Fix the flaky test" (session-row-text settled)) "its age stands before its name")
      (is (search "4 msgs" (session-row-text settled)) "and its messages at the right"))))

(deftest a-parent-row-wears-the-dim-spinner-while-its-branch-runs (with-clock-at-local-noon)
  ;; A collapsed parent hides its branch whole, so the column read before its
  ;; label is where a running turn under it has to show: the same spinner,
  ;; one tone down, so the row that IS running and the row that merely holds
  ;; one are never mistaken for each other.
  (let* ((entries (list (directory-entry "p-mom" nil)
                        (directory-entry "p-kid" "p-mom")
                        (directory-entry "p-grand" "p-kid")
                        (directory-entry "p-settled" nil)))
         (children (alexandria:plist-hash-table '("p-mom" ("p-kid") "p-kid" ("p-grand"))
                                                :test #'equal))
         ;; Opened, so every row of the chain is on screen to be read; the dim
         ;; mark is what the collapsed door state shows instead (the picker gate).
         (open (alexandria:plist-hash-table '("p-mom" t "p-kid" t) :test #'equal))
         (now-ms (nle::iso-8601-ms (nlk::iso-now)))
         (activity (alexandria:plist-hash-table (list "p-grand" (iso-ago 192)) :test #'equal)))
    (labels ((rows ()
               (nle::session-picker-rows entries children open now-ms activity))
             (row (id) (activity-row-for id (rows))))
      (let ((grand (row "p-grand"))
            (settled (row "p-settled")))
        (is (and (activity-spinner-p grand)
                 (eq :tool grand.mark-tone)))
        (is (search "3m 12s  " (session-row-text grand)) "with its own clock in the age slot")
        (is-present (parent (row "p-kid")) "its parent is in the directory"
          (is (activity-spinner-p parent) "and wears the spinner too")
          (is (eq :muted (nle::picker-row-mark-tone parent)) "dimmed, not the live tone")
          (is (search "ago  ╰ p-kid" (session-row-text parent)) "its own age beside it, no clock"))
        (is (and (activity-spinner-p (row "p-mom"))
                 (eq :muted (nle::picker-row-mark-tone (row "p-mom")))))
        (is (equal "" settled.mark))
        (is (search "10m ago  p-mom" (session-row-text (row "p-mom")))))
      ;; The row's own turn, when it has one, outranks the dim mark.
      (setf (gethash "p-kid" activity) (iso-ago 30))
      (let ((kid (row "p-kid")))
        (is (eq :tool kid.mark-tone))
        (is (search "30s  ╰ p-kid" (session-row-text kid)) "and its own clock")
        (is (eq :muted (nle::picker-row-mark-tone (row "p-mom"))))))))

;;; --- R5: the picker -----------------------------------------------------------

(deftest the-same-map-again-moves-nothing (with-frame-model (model 100 30 :session-id "act-same"))
  ;; Every subscribe is answered with the whole map (the preview lane
  ;; re-subscribes per highlight): a frame carrying the map the shell
  ;; already holds says so, and the /sessions rows are not laid again.
  (let ((running (nlk:json-object "sessions" (running-map '("act-run" 30))))
        (settled (nlk:json-object "sessions" (running-map))))
    (is (not (nth-value 1 (nle::ingest-session-activity model settled))) "an empty map over an empty table")
    (is (nth-value 1 (nle::ingest-session-activity model running)) "a turn started")
    (is (not (nth-value 1 (nle::ingest-session-activity model running))) "the same map again")
    (is (equal (iso-ago 30) (gethash "act-run" (nle::session-activity-table model))) "and the table holds it")
    (is (nth-value 1 (nle::ingest-session-activity model settled)) "it settled")
    (is (zerop (hash-table-count (nle::session-activity-table model))))))

(deftest an-idle-sessions-picker-tick-owes-no-frame ()
  ;; The frame clock keeps ticking while the picker is open (its search box
  ;; and spinner rows are clocks), but a tick that moved nothing answers
  ;; QUIET and the loop paints no frame for it: a compose and a diff per
  ;; tick over a directory of settled sessions was the shell's idle cost.
  (with-frame-model (model 100 30 :session-id "act-quiet")
    (with-stubbed-fdefinition (nle::sessions-preview-arm (&rest args))
      (nle::install-session-directory
       model (list (directory-entry "act-quiet" nil :title "here")
                   (directory-entry "act-rest" nil :title "rested")))
      (is (nle::live-chrome-animating-p model) "the open picker asks for ticks")
      ;; The picker is live: the open delivered its highlight in its own
      ;; pass, so no tick is owed one.
      (is (null (nle::picker-dialog-pending-highlight model.dialog)) "nothing staged")
      (is (nth-value 2 (nle:update model (nlt:make-tick-msg))) "a tick over settled sessions is quiet")
      (feed-organism model "organism.activity" "sessions" (running-map '("act-rest" 30)))
      ;; The spinner's next frame is at most one quantum away.
      (sleep (/ (* 2 nle::*status-running-frame-ms*) 1000))
      (is (not (nth-value 2 (nle:update model (nlt:make-tick-msg)))) "a running row's spinner moves the frame"))))

(deftest sessions-dialog-draws-the-running-session-live (with-clock-at-local-noon)
  (with-frame-model (model 100 30 :session-id "act-here")
    (with-stubbed-fdefinition (nle::sessions-preview-arm (&rest args))
      (flet ((row-for (id &aux (dialog model.dialog))
               (and dialog (activity-row-for id (nle::picker-visible-rows dialog)))))
        ;; The map lands the way it does at the gateway: one frame, whole.
        ;; Two turns run -- one the shell can see, one hidden under a
        ;; collapsed parent, which is what the dim mark is for.
        (feed-organism model "organism.activity"
                       "sessions" (running-map '("act-run" 192) '("act-child" 30)))
        (nle::install-session-directory
         model (list (directory-entry "act-here" nil :title "here")
                     (directory-entry "act-parent" nil :title "a parent")
                     (directory-entry "act-child" "act-parent" :title "the fork")
                     (directory-entry "act-run" nil :title "Fix the flaky test")))
        (is-present (running (row-for "act-run")) "the running session is in the directory"
          (is (search "3m 1" (session-row-text running)) "its clock where its age was")
          (is (activity-spinner-p running) "and the spinner ahead of its name")
          (is (eq :tool (nle::picker-row-mark-tone running)) "in the live tone"))
        (is-present (idle (row-for "act-here")) "the session this shell sits on is not running"
          (is (equal "" (nle::picker-row-mark idle))))
        (is (null (row-for "act-child")))
        (is-present (held (row-for "act-parent")) "the fold's parent says so"
          (is (activity-spinner-p held) "with the spinner")
          (is (eq :muted (nle::picker-row-mark-tone held)) "dimmed, beside the live one")
          (is (search "ago  a parent" (session-row-text held)) "and its own age, no clock"))
        ;; It settles: the next frame carries the map without either turn, and
        ;; every row on screen is the settled one.
        (feed-organism model "organism.activity" "sessions" (vector))
        (is-present (settled (row-for "act-run")) "the row stays in the directory"
          (is (equal "" (nle::picker-row-mark settled)) "without its spinner")
          (is (search "ago  Fix the flaky test" (session-row-text settled)) "its age back in place"))
        (is (equal "" (nle::picker-row-mark (row-for "act-parent"))))))))
