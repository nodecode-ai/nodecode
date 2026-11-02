;;;; band-test.lisp --- background-exec SURFACE gates: the band.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Part-2 claims over the substrate exec-test proves:
;;;;
;;;;   S1. Every pending eval has one chronological, scrollable transcript block.
;;;;   S2. Status-row shape: mark at col 4, text at col 6, the preparing
;;;;       hint, the command painted as a card paints it; a command's
;;;;       own lines under it, windowed to five rows as a card's output is.
;;;;   S3. Output window shapes: all-tail, spare==1 gutter collapse, and the
;;;;       frozen-head / marker / live-tail split; band rows in the frame.
;;;;   S4. Tool-result display projection: background envelopes render
;;;;       as their streams; "exited" is a surfaced status word.
;;;;   S5. Pending and completed calls share the same transcript position.
;;;;   S6. Live status docks above the composer; exec rows remain scrollable
;;;;       and reuse the completed divider's closing pad.
;;;;   S8. A running exec's row ends in the ✕ that interrupts it: a click
;;;;       asks the gateway, and the row says `stopping…' until it exits.

(in-package #:nodecode.test)

(nlk:access (preparing nle::bg-band-row))

;;; --- fixtures ---------------------------------------------------------------

(defun band-row (&key (command "sleep 1") (status :running) shell-id
                      (activity 0) (tail "") (head "") (seen 0))
  (nle::make-bg-band-row
   :command command :status status
   :shell-session-id shell-id
   :activity-events activity
   :output-seen-lines seen
   :output-tail (string-octets tail) :output-head (string-octets head)))

;;; --- S2: status row shape ---------------------------------------------------

(defun band-first-plain (row width)
  "The plain text of ROW's first band row at WIDTH, as the live band paints it."
  (first (nth-value 1 (nle::background-band-lines row width))))

(deftest band-status-row-mark-badge-and-hint ()
  (let ((plain (band-first-plain (band-row :command (format nil "cargo build~%  --release~%  --all")
                                           :activity 9 :status :preparing)
                                 80)))
    (is (equal "⠂" (string (char plain 4))))
    (is (eql 6 (search "cargo build" plain)) "body text starts at column 6"))
  ;; A RUNNING row's mark is the clock's instead: output says nothing about
  ;; whether the command is still alive, and a silent one must not read as a
  ;; frozen dot.
  (let ((row (band-row :command "cargo build" :activity 9 :shell-id 3))
        (unit internal-time-units-per-second))
    (flet ((mark-at (age-ms)
             (setf (nle::bg-row-born row)
                   (- (get-internal-real-time) (truncate (* unit age-ms) 1000)))
             (let ((plain (band-first-plain row 80)))
               (values (string (char plain 4)) plain))))
      (multiple-value-bind (fresh plain) (mark-at 0)
        (is (find fresh nle::+background-odometer-frames+ :test #'equal))
        (is (eql 6 (search "cargo build" plain))))
      (is (not (equal (mark-at 150) (mark-at 0))))))
  (let ((plain (band-first-plain (band-row :command (format nil "sleep 5~%done") :status :preparing)
                                 80)))
    (is (search "  — preparing" plain) "the preparing hint renders")
    (is (equal "⠁" (string (char plain 4))))))

(deftest band-status-row-command-syntax-matches-tool-header ()
  (let ((segs (first (nle::command-rows "(sleep 10)" 80))))
    (is (equal '("(" "sleep" " " "10" ")")
               (mapcar #'nle::seg-text segs)))
    (is (equal '(:syntax-punctuation :syntax-command :syntax-function
                 :syntax-number :syntax-punctuation)
               (mapcar #'nle::seg-tone segs)))
    (is (every #'nle::seg-dim segs))))

;;; --- S3: output window shapes ------------------------------------------------

(deftests band-output-window (row size tail head omitted collapse)
  (is-shape (nle::background-output-window row size)
    (:tail tail "the newest tail rows") (:head head "the frozen head rows")
    (:omitted omitted "omitted = total - head - tail")
    (:collapse-p collapse "the omission collapses into the gutter, or the marker owns its own row"))
  (all-tail-when-everything-fits (band-row :tail (format nil "one~%two~%") :seen 2) 5
   '("one" "two") nil 0 nil)
  (spare-one-collapses-into-the-gutter (band-row :tail (format nil "a~%b~%c~%") :seen 10) 1
   '("c") nil 9 t)
  (head-marker-tail-split
   (band-row :head (format nil "h1~%h2~%") :tail (format nil "t1~%t2~%t3~%") :seen 12) 5
   '("t2" "t3") '("h1" "h2") 8 nil))

(deftest band-rows-render-into-the-frame ()
  (with-frame-model (model 100 24 :session-id "band-frame" :tool-detail :developer)
    (setf (nle::state-background-rows model.state)
          (list (band-row :command "make -j all" :shell-id 41
                          :activity 1
                          :tail (format nil "compiling~%")
                          :seen 1)))
    (nle::render-state model)
    (let ((rows (nle::compose-frame model)))
      (is (rows-carry-p "make -j all" rows))
      (is (find-if (lambda (row) (and (search "⌎" row)
                                      (search "compiling" row)))
                   rows)))
    ;; The exit retires the band from the frame.
    (rendering model (setf (nle::state-background-rows model.state) '()))
    (is (not (rows-carry-p "make -j all" (nle::compose-frame model))))))

;;; --- S6: tool-result display projection --------------------------------------

(deftest shell-result-envelopes-project-for-display (is-each (nle::shell-result-display-text))
  ("{\"stdout\":\"out\",\"stderr\":\"err\",\"exit_code\":0}" (format nil "out~%err")
   "foreground: stdout + newline + stderr")
  ("{\"stdout\":\"out\",\"stderr\":\"\",\"exit_code\":1}" "out" "empty stderr adds nothing")
  ("{\"status\":\"running\",\"shell_session_id\":7,\"output\":\"tick\"}" "tick"
   "background: the output stream alone")
  ("plain text" nil "non-envelope text renders unchanged")
  ("{\"a\":1}" nil "a JSON object without the envelope keys is not an envelope"))

(deftest exited-status-word-surfaces-on-tool-rows ()
  (let ((row (nle::make-transcript-row
              :kind :tool-call :role "assistant"
              :tool-call-id "call-9" :tool-name "run"
              :tool-args "{\"command\":\"sleep 100\"}"
              :tool-status "exited" :tool-exit-code 0 :hydrated t
              :text "")))
    (is (search " exited" (nle::lline-text (first (nle::tool-header-llines row)))))))

;;; --- S7: transcript ownership handoff -----------------------------------------
;;; The band and the transcript are one surface for one call, never two at
;;; once: while a band row owns the call its tool card is absent from the
;;; projection (render_plan.zig rowRepresentsRunningBackgroundProcess), and
;;; dropping the band row hands the card back.

(defun tool-blocks (state)
  "The transcript's tool cards — BG-ITEM-DELTA (exec-test) is the streamed
fragment these gates drive the band from."
  (remove :tool-call (nle:project-state state :width 60)
          :key #'nle::display-block-kind :test-not #'eq))

(defun bg-call-fact (call-id arguments &key (tool-name "run"))
  (nlk::make-json-object
   "type" "turn.tool_call_started" "turn_id" "turn-1"
   "call-id" call-id "tool-name" tool-name "arguments" arguments))

(defun bg-result-fact (call-id tool-name result &optional shell)
  "CALL-ID's TOOL-NAME result answering RESULT, SHELL its background_shell document."
  (nlk:json-object "type" "turn.tool_result" "turn_id" "turn-1"
                   "call-id" call-id "tool-name" tool-name "result" result
                   :when shell "metadata" (nlk:json-object "background_shell" shell)))

(defun bg-backgrounding-result (call-id shell-id command &optional (tool "run"))
  "The backgrounding envelope a TOOL call answers with: the run tool's shape,
and the eval snippet's with the evaluation's id where a shell session's would be."
  (bg-result-fact
   call-id tool
   (format nil "{\"status\":\"running\",\"~:[shell_session_id~;eval_id~]\":~d,\"output\":\"\"}"
           (equal tool "eval") shell-id)
   (nlk:json-object "status" "running" "shell_session_id" shell-id "command" command)))

(defun bg-process-fact (session-id shell-id call-id command status &rest fields)
  "SESSION-ID's session_background_process_updated fact: CALL-ID's COMMAND,
shell SHELL-ID, in STATUS, FIELDS riding the process beside those."
  (nlk::make-json-object
   "type" "session_background_process_updated" "session_id" session-id
   "process" (apply #'nlk::make-json-object "shell_session_id" shell-id "tool_call_id" call-id
                    "command" command "status" status fields)))

(defun bg-exit-fact (session-id shell-id call-id command &rest fields)
  "SESSION-ID's session_background_process_updated fact: CALL-ID's COMMAND,
shell SHELL-ID, exited 0, FIELDS riding the process beside those."
  (apply #'bg-process-fact session-id shell-id call-id command "exited" "exit_code" 0 fields))

(defun bg-preview-fact (session-id shell-id call-id command preview)
  "BG-PROCESS-FACT for CALL-ID's COMMAND still running, its snippet's PREVIEW whole."
  (bg-process-fact session-id shell-id call-id command "running" "output_b64" "" "offset" 0
                   "preview_b64" (cl-base64:string-to-base64-string preview)))

(defun background-eval (model call-id exec-id &optional (command "(sh \"make\")") (form command))
  "Fold into MODEL eval CALL-ID's call, carrying FORM, and its backgrounding as
exec EXEC-ID running COMMAND."
  (feed-events (nle::chat-model-state model)
    (bg-call-fact call-id (eval-args form) :tool-name "eval")
    (bg-backgrounding-result call-id exec-id command "eval")))

(defun bg-assistant-text (text)
  (nlk::make-json-object "type" "turn.assistant_message_completed"
                         "turn_id" "turn-1" "message" text
                         "metadata" (nlk:json-object "log_position" 1 "turn_id" "turn-1" "occurred_at" (nlk::iso-now))))

(defun bg-state (&rest events &aux (state (make-hash-table :test #'equal)))
  "A fresh reducer state with EVENTS folded through APPLY-GATEWAY-EVENT."
  (dolist (event events state)
    (setf state (nle::apply-gateway-event state event))))

(deftest pending-call-has-one-transcript-block-through-its-lifecycle ()
  (let ((state (bg-state (bg-assistant-text "building"))))
    ;; The first streamed fragment opens the preparing band row. From here
    ;; the call belongs to the band alone.
    (feed-events state (bg-item-delta 0 "run" "{\"command\":\"sleep 100\"}"))
    (is (= 1 (length (nle::state-background-rows state))))
    (is (= 1 (length (tool-blocks state))))
    ;; The durable fact adopts the row by call id; still band-owned.
    (feed-events state (bg-call-fact "call-9" "{\"command\":\"sleep 100\"}"))
    (is (= 1 (length (tool-blocks state))))
    ;; Backgrounded: the shell runs, the card still does not exist.
    (feed-events state (bg-backgrounding-result "call-9" 1042 "sleep 100"))
    (is (eq :running (nle::bg-band-row-status
                      (first (nle::state-background-rows state)))))
    (is (= 1 (length (tool-blocks state))))
    ;; Transcript work continues around it.
    (feed-events state (bg-assistant-text "meanwhile"))
    (is (= 1 (length (tool-blocks state))))
    ;; The exit hands the card back — stamped, and at the transcript END.
    (feed-events state (bg-exit-fact "session-a" 1042 "call-9" "sleep 100"
                                     "backgrounded" t "output_b64" ""))
    (is (null (nle::state-background-rows state)) "the band row retired")
    (flet ((run (blocks) (find-if #'nle::tool-group-p blocks :key #'nle::display-block-row)))
     (let ((blocks (nle:project-state state :width 60)))
      (is (= 1 (length (tool-blocks state))))
      (is (eq :tool-call (nle::display-block-kind (run blocks))))
      ;; Its live turn folds it into a run of one; open, the card shows.
      (is (rows-carry-p "Ran 1 command" (nle::display-block-plain-lines (run blocks))))
      (let* ((nle::*expanded-tool-call* '(:process "call-9"))
             (lines (nle::display-block-plain-lines (run (nle:project-state state :width 60)))))
        (is (rows-carry-p "Waited 100s" lines))
        (is (not (rows-carry-p "done" lines)) "a clean end says nothing"))))))

(deftest a-preview-frame-shows-in-the-band-row ()
  ;; A snippet's preview is whole each time: it replaces the row's tail.
  (flet ((running (preview) (bg-preview-fact "session-a" 1077 "call-p" "(sh \"make\")" preview)))
    (let ((state (bg-state (running (format nil "compiling a~%")))))
      (feed-events state (running (format nil "compiling a~%compiling b~%")))
      (let ((row (first (nle::state-background-rows state))))
        (is (equal (format nil "compiling a~%compiling b~%")
                   (nle::octets-to-text (nle::bg-band-row-output-tail row))))
        (is (= 2 (nle::bg-band-row-output-seen-lines row))))
      ;; A progress bar redrawn in place shows its last redraw, not all of them.
      (feed-events state (running (format nil "building~%progress 10%~cprogress 60%~cprogress 90%~c"
                                          #\Return #\Return #\Return)))
      (is (equal (format nil "building~%progress 90%")
                 (nle::octets-to-text (nle::bg-band-row-output-tail (first (nle::state-background-rows state)))))))))

(deftest spawn-less-result-hands-the-card-back-in-place ()
  (let ((state (bg-state (bg-item-delta 0 "run" "{\"command\":\"echo hi\"}"))))
    (is (= 1 (length (tool-blocks state))) "the foreground call prepares in the transcript")
    (feed-events state
      (bg-call-fact "call-3" "{\"command\":\"echo hi\"}")
      (bg-result-fact "call-3" "run" "{\"stdout\":\"hi\",\"stderr\":\"\",\"exit_code\":0}"))
    (is (null (nle::state-background-rows state)))
    (is (= 1 (length (tool-blocks state))))))

(deftest a-later-round-at-the-same-stream-index-is-not-band-owned ()
  (let ((state (bg-state (bg-item-delta 0 "run" "{\"command\":\"sleep 100\"}")
                         (bg-call-fact "call-1" "{\"command\":\"sleep 100\"}")
                         (bg-backgrounding-result "call-1" 7 "sleep 100"))))
    ;; Provider stream indices restart every tool round: the next round's
    ;; index-0 call must not inherit the live row's ownership. Developer,
    ;; where an exec's call stands alone: Standard runs them as one.
    (feed-events state (bg-item-delta 0 "read" "{\"path\":\"a.txt\"}"))
    (is (= 1 (length (nle::state-background-rows state))))
    (let ((nle::*tool-detail* :developer))
      (is (= 2 (length (tool-blocks state)))))))

(deftest eval-lives-in-the-transcript-while-it-runs ()
  ;; The organism's own exec: an in-flight eval call is band-owned from
  ;; its first streamed fragment through execution (the wire cannot tell
  ;; argument streaming from execution — the durable result is the first
  ;; fact after dispatch), and the result hands its card back in place.
  (let ((state (bg-state (bg-item-delta 0 "eval" "{\"form\":\"(sleep 10)\"}"))))
    (is-present (row (first (nle::state-background-rows state)))
      "the streamed eval fragment seeds a band row"
      (is (equal "(sleep 10)" (nle::bg-band-row-command row))))
    (is (= 1 (length (tool-blocks state))))
    ;; The durable fact adopts the row; execution follows under the bar.
    (feed-events state (bg-call-fact "call-e1" "{\"form\":\"(sleep 10)\"}" :tool-name "eval"))
    (is (= 1 (length (tool-blocks state))))
    ;; The result retires the band row and the card joins the transcript.
    (feed-events state (bg-result-fact "call-e1" "eval" "10"))
    (is (null (nle::state-background-rows state)))
    (is (= 1 (length (tool-blocks state))))))

(deftest a-foreground-calls-late-exit-leaves-its-card-where-it-was-made ()
  ;; A foreground eval's (sh ...) shows in the band while it runs, and the
  ;; watcher's frames for it ride apart from the turn's: its exit can land
  ;; after the call's result and after the next round's rows. The card stays
  ;; where the call was made, as the replay has it; only a call that was
  ;; handed off joins the end when it exits.
  (flet ((preview (call-id) (bg-preview-fact "session-a" 1201 call-id "(sh \"ls\")" "total 12"))
         (order (state)
           (mapcar (lambda (row) (or (nle::transcript-row-tool-call-id row) (nle::transcript-row-text row)))
                   (nle::transcript-rows state))))
    (let ((state (bg-state (bg-call-fact "call-e1" "{\"form\":\"(sh \\\"ls\\\")\"}" :tool-name "eval")
                           (preview "call-e1")
                           (bg-result-fact "call-e1" "eval" "total 12")
                           (bg-assistant-text "next round"))))
      (feed-events state (bg-exit-fact "session-a" 1201 "call-e1" "(sh \"ls\")" "output_b64" ""))
      (is (null (nle::state-background-rows state)) "the band row retired")
      (is (equal '("call-e1" "next round") (order state)) "the card did not move")
      (let ((row (tool-call-row state "call-e1")))
        (is (equal "complete" (nle::transcript-row-tool-status row)) "it never outlived its call")))))

;;; --- S6: the mark of work that outlives its turn ----------------------------
;;; A backgrounded process or eval keeps its band row past the turn that
;;; started it (a turn's end drops only the still-preparing ones) and the frame
;;; clock keeps ticking while any row lives. What the row's Zig mark cannot do
;;; is move on its own: it is an activity odometer, so a command that has said
;;; nothing for ten minutes reads as a frozen dot — the one row an operator
;;; watches to know the work is still alive. A RUNNING row's mark is the
;;; clock's; a PREPARING row keeps the odometer.

(deftest a-running-rows-mark-moves-on-the-clock ()
  ;; The clock is the argument, so the moments are exact: the image's own
  ;; internal time is only seconds old, and a stamp it cannot reach would read
  ;; as no clock at all.
  (let* ((unit internal-time-units-per-second)
         (born unit)
         (frame (truncate (* unit 3) 20))      ; 150ms, the turn bar's grid
         (running (band-row :command "(sleep 600)" :shell-id 7))
         (preparing (band-row :command "sleep 1" :status :preparing
                              :activity 0)))
    (setf (nle::bg-row-born running) born)
    (is-table (in want) (equal want (nle::background-band-mark running in)) (born "⠁")
      ((+ born frame) "⠂") ((+ born (* 8 frame)) "⠁"))
    (is (not (equal (nle::background-band-mark running (+ born frame))
                    (nle::background-band-mark running (+ born (* 2 frame))))))
    ;; A row that was already live when the clock arrived has no stamp: it is
    ;; born on the frame that first asks for its mark, so the command the
    ;; operator is watching starts moving instead of parking at frame 0.
    (is-let* ((old (band-row :command "no clock"))) ((nle::background-band-mark old born) "⠁")
      ((nle::bg-row-born old) born) ((nle::background-band-mark old (+ born frame)) "⠂"))
    ;; The preparing row answers a different question — how much has the
    ;; command said — so time alone moves it not at all.
    (is (equal (nle::background-band-mark preparing born)
               (nle::background-band-mark preparing (+ born (* 4 unit)))))
    (setf preparing.activity-events 3)
    (is (equal "⡀" (nle::background-band-mark preparing born)))))

(deftest a-background-row-outliving-its-turn-is-stamped-and-animating ()
  (with-frame-model (model 80 24 :session-id "bg-outlives")
    (feed-events (nle::chat-model-state model)
      (bg-item-delta 0 "eval" "{\"form\":\"(sleep 600)\"}")
      (bg-call-fact "call-e1" "{\"form\":\"(sleep 600)\"}" :tool-name "eval")
      (bg-backgrounding-result "call-e1" 1149 "(sleep 600)" "eval")
      (nlk::make-json-object "type" "turn.completed" "turn_id" "turn-1"
                             "metadata" (nlk::make-json-object
                                         "occurred_at" (nlk::iso-now) "log_position" 4 "turn_id" "turn-1")))
    (is (null model.turn-in-flight) "the turn is over")
    (let ((rows (nle::state-background-rows model.state)))
      (is (= 1 (length rows)) "its eval keeps its band row")
      (is (eq :running (nle::bg-band-row-status (first rows))) "still running")
      (is (nle::bg-row-born (first rows)))
      (is (nle::live-chrome-animating-p model))
      (is (find (nle::background-band-mark (first rows) (get-internal-real-time))
                nle::+background-odometer-frames+ :test #'equal)))
    ;; The exit retires the row.
    (feed-events (nle::chat-model-state model)
      (bg-exit-fact "bg-outlives" 1149 "call-e1" "(sleep 600)" "output_b64" "" "offset" 0))
    (is (null (nle::state-background-rows model.state)))))

(deftest a-background-call-stands-right-above-the-live-bar (dolist (mode '(:standard :developer)))
  ;; A backgrounded call still running is the line right above the live bar,
  ;; said once: under its run in Standard, its own exec row in Developer.
  (with-frame-model (model 100 30 :session-id "bg-anchor" :tool-detail mode)
    (background-eval model "call-e1" 1149 "(sleep 600)")
    (mark-turn-in-flight model :render t)
    (is (= 1 (length (tool-blocks (nle::chat-model-state model)))) "one card owns the call")
    (let* ((needle (if (eq mode :developer) "(sleep 600)" "in the background"))
           (frame (nle::compose-frame model))
           (status (frame-row-carrying model "waiting"))
           (exec (frame-row-carrying model needle)))
      (is (< exec status) "the call's line stands above the bar")
      (is (every (lambda (row) (string= "" (string-trim " " (nle::plain-frame-row row))))
                 (subseq frame exec (1- status))) "and right above it: only the gap between")
      (is (= 1 (count needle frame :test #'search)) "said once")
      (when (eq mode :developer)
        (let ((position (nle::frame-transcript-position model 12 exec)))
          (is (eql 1149 (nle::band-exec-at model position)) "paint and click share the exec owner")))
      (dolist (action '(:home :page-up :page-down :line-up :line-down :end))
        (nle::scroll-transcript model action)
        (is (= status (frame-row-carrying model "waiting")))
        (is (= exec (frame-row-carrying model needle))))
      (rendering model (setf (nle::chat-model-turn-in-flight model) nil))
      ;; The call outlives its turn, and the transcript's end comes down to
      ;; the bar's place.
      (is (= status (frame-row-carrying model needle)) "the call stands where the bar stood")
      (is (not (rows-carry-p "waiting" (coerce (nle::chat-model-line-plain model) 'list)))))))

;;; --- S7: the pending-exec view's reader --------------------------------------
;;; BG-ROW-OUTPUT-LINES is the panel's read of the same retained buffer the
;;; band windows: the tail alone when it holds the whole stream, otherwise the
;;; frozen head, an omission clause and the live tail. EXEC-VIEW-ROWS stamps
;;; every row with the exec id — the interrupt gesture's argument.

(deftest exec-view-lines-read-the-row-buffer ()
  ;; Nothing printed yet: no lines — the panel's empty-label case.
  (is-values (lines omitted total) (nle::bg-row-output-lines (band-row))
    (lines null "no output, no lines") (omitted = 0 "nothing omitted")
    (total = 0 "nothing counted"))
  ;; Everything the stream printed still fits the tail: no head, no clause.
  (is-values (lines omitted total)
      (nle::bg-row-output-lines (band-row :tail (format nil "tick 0~%tick 1~%tick 2~%") :seen 3))
    (lines '("tick 0" "tick 1" "tick 2") "the tail holds the whole stream")
    (omitted = 0 "no omission clause") (total = 3 "the stream's own count"))
  ;; More lines than the tail holds: the frozen head, the counted clause, the
  ;; live tail — the band's window shape at the panel's height.
  ;; When a tail is cut by bytes, partway into its first line, the panel
  ;; omits that partial line.
  (is-values (lines omitted total)
      (nle::bg-row-output-lines
       (band-row :head (format nil "first~%second~%")
                 :tail (format nil "l-0~%tail-1~%tail-2~%") :seen 9))
    ((subseq lines 0 2) '("first" "second") "the frozen head leads")
    ((search "hidden" (third lines)) is "the clause names what it hid")
    ((last lines 2) '("tail-1" "tail-2") "the live tail closes")
    (omitted = 5 "9 lines - 2 head - 2 tail") (total = 9 "the row's own count")))

(deftest exec-view-rows-carry-the-exec-id ()
  ;; Every row's value is the exec id: the interrupt gesture reads it back.
  (let ((rows (nle::exec-view-rows 1371 '("a" "b"))))
    (is (= 2 (length rows)) "one row per line")
    (is (equal '(1371 1371) (mapcar #'nle::picker-row-value rows)) "each row names the exec"))
  ;; No output yet: one placeholder row, same value.
  (let ((rows (nle::exec-view-rows 12 nil)))
    (is (= 1 (length rows)) "one placeholder row")
    (is (equal 12 (nle::picker-row-value (first rows))) "the placeholder still names the exec")))

;;; --- S8: the band's ✕ ---------------------------------------------------------
;;; The interrupt gesture sits on the row it stops: a running exec's status row
;;; ends in a ✕, a click on it asks the gateway (INTERRUPT-BAND-EXEC, the path
;;; the pending-exec view's Ctrl+X takes too), and the row says `stopping…'
;;; instead until its exit retires it.

(deftest band-status-row-offers-the-x-while-it-can-stop ()
  (flet ((plain (row &optional (width 80))
           (string-right-trim " " (band-first-plain row width))))
    (let ((running (plain (band-row :command "sleep 9" :shell-id 4)))
          (preparing (plain (band-row :command "sleep 9" :status :preparing))))
      (is (and (search "sleep 9" running) (uiop:string-suffix-p running "  ✕")) "a running exec's row ends in its ✕")
      (is (not (find #\✕ preparing)) "a preparing row has no exec to stop yet"))
    (is (not (find #\✕ (plain (band-row :command "sleep 9")))) "no exec id, no ✕")
    (let ((row (band-row :command "sleep 9" :shell-id 4)))
      (setf (nle::bg-row-stopping row) t)
      (is (uiop:string-suffix-p (plain row) "  stopping…") "an asked row says so")
      (is (not (find #\✕ (plain row))) "and offers the gesture no more"))
    (let ((long (plain (band-row :command (format nil "~a~%b~%c" (make-string 200 :initial-element #\a))
                                 :shell-id 4)
                       40)))
      (is (uiop:string-suffix-p long "  ✕") "the command yields width to the ✕")
      (is (<= (nle::text-display-width long) 40)))))

(deftest clicking-the-band-x-interrupts-that-exec (let ((sent '())))
  (with-frame-model (model 100 24 :session-id "band-stop")
    (let ((row (band-row :command "make -j all" :shell-id 41)))
      (rendering model (setf (nle::state-background-rows model.state) (list row)))
      (with-stubbed-fdefinition (nle::send-interrupt-exec (ws session-id exec-id)
                                 (push (list session-id exec-id) sent))
        (let* ((y (frame-row-carrying model "make -j all"))
               (line (nth (1- y) (nle::compose-frame model)))
               (x (1+ (nle::text-display-width (subseq line 0 (position #\✕ line :from-end t))))))
          (mouse-update model #'nlt:make-mouse-motion-msg x y)
          (is (equal '(:stop . 41) model.hover-target) "the pointer on the ✕ lights it")
          (click-at model x y)
          (is (equal '(("band-stop" 41)) sent) "the click asks to interrupt exec 41")
          (nle::render-state model)
          (let ((line (nth (1- y) (nle::compose-frame model))))
            (is (search "stopping…" line) "the row says it was asked")
            (is (not (find #\✕ line)) "and offers no second gesture"))
          ;; A refusal takes the mark back, so the gesture is offered again.
          (nle::gateway-command-result
           model (nlk::make-json-object "status" "rejected" "command" "interrupt_exec"
                                       "code" "exec_exited"))
          (is (not (nle::bg-row-stopping row)) "a refused interrupt clears the mark")
          (nle::render-state model)
          (click-at model 8 y)
          (is (= 1 (length sent)) "a click on the command is not the gesture")
          (is (nle::chat-model-expanded-tool-call model) "it opens the exec in place"))))))

;;; --- S9: two calls of a round, and how an exec ended -------------------------

(deftest a-rounds-calls-adopt-their-own-band-rows ()
  ;; Two calls stream in one round and their facts land in model order, so the
  ;; first fact takes the oldest preparing row. Without this, taking the newest
  ;; would give the first call the second's row: the band would show the first
  ;; command twice and the second's nowhere.
  (flet ((form (command) (format nil "{\"form\":\"(sh \\\"~a\\\")\"}" command)))
    (let ((state (bg-state (bg-item-delta 0 "eval" (form "make a"))
                           (bg-item-delta 1 "eval" (form "make b"))
                           (bg-call-fact "call-a" (form "make a") :tool-name "eval"))))
      (flet ((rows (reader) (mapcar reader (nle::state-background-rows state))))
        (is (equal '("(sh \"make a\")" "(sh \"make b\")") (rows #'nle::bg-band-row-command)))
        (is (equal '("call-a" nil) (rows #'nle::bg-band-row-tool-call-id)) "the first call, the first row")
        (feed-events state (bg-call-fact "call-b" (form "make b") :tool-name "eval"))
        (is (equal '("call-a" "call-b") (rows #'nle::bg-band-row-tool-call-id)) "the second, the second")))))

(defun backgrounded-card-title (&rest exit-fields)
  "The card title of an eval backgrounded as exec 1300 once its live exit
frame, carrying EXIT-FIELDS beside an exit code of 0, retired its band row."
  (let ((state (bg-state (bg-item-delta 0 "eval" "{\"form\":\"(sh \\\"make\\\")\"}")
                         (bg-call-fact "call-m" "{\"form\":\"(sh \\\"make\\\")\"}" :tool-name "eval")
                         (bg-backgrounding-result "call-m" 1300 "(sh \"make\")" "eval"))))
    (feed-events state (apply #'bg-exit-fact "session-a" 1300 "call-m" "(sh \"make\")"
                              "backgrounded" t "output_b64" "" exit-fields))
    (let ((title (first (nle::tool-header-llines (tool-call-row state "call-m")))))
      (values (nle::lline-text title)
              (nle::mspan-tone (car (last (nle::lline-spans title))))))))

(deftest a-backgrounded-card-says-how-it-ended ()
  ;; The card of a call that ran on in the background says how it ended,
  ;; from the exit's document -- the words the wake notice and the web card
  ;; use (EXEC-END) -- live and on replay alike. Without this, the card would
  ;; say `exited' whatever the end. A clean end says nothing, as a foreground
  ;; call's does.
  (is-each (backgrounded-card-title)
    ("shell_exit" 2 "Ran make exited 2" "its last sh's status")
    ("shell_exit" 0 "Ran make" "a clean end")
    ("exit_code" 1 "Ran make failed" "a form that signalled")
    ("exit_code" 130 "Ran make stopped" "a stop"))
  ;; The durable exit fact a replay folds says the same.
  (let ((state (bg-state (bg-call-fact "call-r" "{\"form\":\"(sh \\\"make\\\")\"}" :tool-name "eval")
                         (bg-backgrounding-result "call-r" 1301 "(sh \"make\")" "eval")
                         (bg-result-fact "call-r" "snippet" ""
                                         (nlk:exited-shell-document 1301 "(sh \"make\")" 0
                                                                    :shell-exit 2)))))
    (is (equal "Ran make exited 2" (nle::lline-text (first (nle::tool-header-llines (tool-call-row state "call-r"))))))))

(deftest the-exec-view-says-how-its-exec-ended ()
  ;; /execs holds the row it opened on: once the exit retires it from the band,
  ;; the header says how the exec ended -- `exited 2', where it said only
  ;; `exited' -- the lines are the exit's last ones, and the interrupt gesture
  ;; goes with the exec.
  (with-frame-model (model 100 30 :session-id "exec-view-end")
    (background-eval model "call-v" 1400)
    (nle::open-exec-view model 1400)
    (let ((dialog (nle::chat-model-dialog model)))
      (is (search " running" (nle::picker-dialog-context dialog)))
      (is (assoc "ctrl+x" (nle::picker-dialog-footer-hints dialog) :test #'equal) "a running exec offers the interrupt")
      (feed-events (nle::chat-model-state model)
        (bg-exit-fact "exec-view-end" 1400 "call-v" "(sh \"make\")" "backgrounded" t "output_b64" ""
                      "shell_exit" 2 "tail" (format nil "compiling~%error: no rule~%")))
      (is (null (nle::state-background-rows (nle::chat-model-state model))) "the band let it go")
      (funcall (nle::picker-dialog-on-tick dialog) model dialog)
      (is (equal " exited 2 " (nle::picker-dialog-context dialog)))
      (is (equal '("compiling" "error: no rule") (mapcar #'nle::picker-row-label (nle::picker-dialog-rows dialog))))
      (is (null (assoc "ctrl+x" (nle::picker-dialog-footer-hints dialog) :test #'equal)) "nothing left to interrupt")
      (is (null (nle::picker-dialog-on-delete dialog))))))

(deftest execs-opened-after-an-exec-ended-shows-how-it-ended ()
  ;; Without this, /execs would list running execs only, so an exec that
  ;; ended before the view was open would be out of reach: `no exec is running'.
  (with-frame-model (model 100 30 :session-id "execs-after-end")
    (is (equal "no exec has run here yet" (nle::slash-execs-op model)))
    (flet ((run-and-end (call-id id shell-exit)
             (background-eval model call-id id)
             (feed-events (nle::chat-model-state model)
               (bg-exit-fact "execs-after-end" id call-id "(sh \"make\")" "backgrounded" t "output_b64" ""
                             "shell_exit" shell-exit "tail" (format nil "compiling~%error: no rule~%")))))
      (run-and-end "call-e1" 1500 2)
      (is (null (nle::state-background-rows (nle::chat-model-state model))) "the band let it go")
      (nle::slash-execs-op model)
      (let ((dialog (nle::chat-model-dialog model)))
        (is (equal " exited 2 " (nle::picker-dialog-context dialog)) "the one exec opens, saying how it ended")
        (is (equal '("compiling" "error: no rule") (mapcar #'nle::picker-row-label (nle::picker-dialog-rows dialog))) "with the lines it ended on")
        (is (null (assoc "ctrl+x" (nle::picker-dialog-footer-hints dialog) :test #'equal)))
        (is (null (nle::picker-dialog-on-delete dialog)) "and nothing to interrupt"))
      (setf (nle::chat-model-dialog model) nil)
      (run-and-end "call-e2" 1501 0)
      (nle::slash-execs-op model)
      (is (equal '("exec 1501 · done" "exec 1500 · exited 2")
                 (mapcar #'nle::picker-row-detail (nle::picker-dialog-rows (nle::chat-model-dialog model)))) "two open the picker, newest first"))))

(deftest a-backgrounded-card-that-ended-badly-reads-in-the-danger-tone ()
  ;; Without this, a background `exited 2' would read grey beside a
  ;; foreground `failed' in red. The status word of a call that ran on takes
  ;; the danger tone for any end but done, from the exit's document
  ;; (EXEC-END); someone stopping it is no failure.
  (is-table (key code tone) (eq tone (nth-value 1 (backgrounded-card-title key code)))
    ("shell_exit" 2 :danger "its last sh's status")
    ("exit_code" 1 :danger "a form that signalled")
    ("exit_code" 130 :muted "a stop")))

(deftest the-exec-view-says-its-id-once-and-shows-its-command-whole ()
  ;; The title names the exec and the context only its state. Under them the
  ;; command reads as the call wrote it, newlines kept and wrapped at the
  ;; panel's edge; without this, the command would be one flattened line cut
  ;; at the edge.
  (with-frame-model (model 80 30 :session-id "exec-view-clip")
    (let ((command (format nil "(progn~%  (sh \"mkdir -p build\")~%  (sh \"for i in $(seq 1 24); do echo module-$i ... ok; sleep 0.5; done; echo 'error: undefined'\"))")))
      (background-eval model "call-c" 1600 command "(sh \"make\")")
      (nle::open-exec-view model 1600)
      (let* ((rows (nle::dialog-panel-rows model 80 nil))
             (header (second rows)))
        (is (search "Exec 1600  running" header) "the title, then the state")
        (is (null (search "1600" header :start2 (1+ (search "1600" header)))) "the id said once")
        (is (search "(progn" (fourth rows)) "the command's first line under the title")
        (is (search "(sh \"mkdir -p build\")" (fifth rows)) "and its second on a row of its own")
        (is (search "echo 'error: undefined'" (format nil "~{~a~}" (subseq rows 5 8))) "the long line goes on, wrapped")
        (is (notany (lambda (row) (search "…" row)) (subseq rows 3 8)) "and nothing is cut")
        (is (every (lambda (row) (= 80 (nle::text-display-width row))) (subseq rows 3 8)) "every row keeps the width")))))

(deftest the-explicit-exec-view-shows-the-whole-command-without-a-mode-toggle ()
  (with-frame-model (model 80 40 :session-id "exec-view-long")
    (let ((command (format nil "~{(sh \"step ~d\")~^~%~}" (loop for i from 1 to 9 collect i))))
      (background-eval model "call-l" 1700 command "(sh \"make\")")
      (nle::open-exec-view model 1700)
      (is (= 9 (length (nle::picker-dialog-summary-rows model.dialog))) "the explicit view is already open")
      (is (null (assoc "ctrl+o" (nle::picker-dialog-footer-hints model.dialog) :test #'equal)))
      (is (eq :standard (nle::chat-model-tool-detail model)) "opening the output never switches the operator viewport"))))

(deftest the-exec-view-ends-on-what-a-failed-exec-failed-on ()
  ;; Without this, a failed form's report reaches no operator surface: the card
  ;; and the view say `failed' and nothing of why. The exit carries it, and the
  ;; view ends on it in the danger tone.
  (with-frame-model (model 100 30 :session-id "exec-view-failed")
    (background-eval model "call-f" 1800)
    (nle::open-exec-view model 1800)
    (feed-events (nle::chat-model-state model)
      (bg-exit-fact "exec-view-failed" 1800 "call-f" "(sh \"make\")" "backgrounded" t "output_b64" ""
                    "exit_code" 1 "tail" (format nil "waiting (5/5)~%")
                    "error" "SIMPLE-ERROR: fixture server refused the connection"))
    (let ((dialog (nle::chat-model-dialog model)))
      (funcall (nle::picker-dialog-on-tick dialog) model dialog)
      (let ((rows (nle::picker-dialog-rows dialog)))
        (is (equal " failed " (nle::picker-dialog-context dialog)))
        (is (equal '("waiting (5/5)" "error: SIMPLE-ERROR: fixture server refused the connection")
                   (mapcar #'nle::picker-row-label rows)) "the last lines, then the error")
        (is (eq :danger (nle::picker-row-tone (car (last rows)))) "in the danger tone")
        (is (eq :danger (nle::seg-tone (first (nle::dialog-entry-segs (car (last rows)) t)))) "selected or not")))))

(defun band-frame-plain (model)
  "The exec rows MODEL's transcript actually composes: every line an exec owns."
  ;; They stood under the fixed status bar once; they stand in the
  ;; transcript now, where their calls are.
  (nle::render-state model)
  (loop for plain across (nle::chat-model-line-plain model)
        for owner across (nle::chat-model-line-rows model)
        when (nle::bg-band-row-p owner) collect plain))
(deftest a-band-exec-shows-its-commands-lines ()
  (with-frame-model (model 100 40 :session-id "band-lines" :tool-detail :developer)
    (setf (nle::state-background-rows model.state)
          (list (band-row :command (format nil "(progn~%  (sh \"mkdir -p build\")~%  (sh \"make\"))")
                          :shell-id 7 :tail (format nil "compiling~%") :seen 1)))
    (let ((lines (band-frame-plain model)))
      (is (search "(progn" (first lines)) "the first line beside the mark")
      (is (uiop:string-suffix-p (string-right-trim " " (first lines)) "✕") "and the ✕ ending it")
      (is (uiop:string-prefix-p "        (sh \"mkdir -p build\")" (second lines)) "the next under it, its indent kept")
      (is (search "(sh \"make\"))" (third lines)) "every line of it")
      (is (search "⌎ compiling" (fourth lines)) "then its output"))))

(deftest viewport-modes-do-not-expand-band-commands ()
  (with-frame-model (model 100 60 :session-id "band-window")
    (setf (nle::state-background-rows model.state)
          (list (band-row :command (format nil "~{(sh \"step ~d\")~^~%~}" (loop for i from 1 to 9 collect i))
                          :shell-id 8)))
    (dolist (mode '(:developer :standard))
      (setf (nle::chat-model-tool-detail model) mode)
      (let ((lines (band-frame-plain model)))
        (is (= 5 (length lines)) "neither viewport is an open-all mode")
        (is (search "+5 lines (click to expand)" (third lines)))
        (is (search "(sh \"step 9\")" (fifth lines)))))))

(deftest the-output-window-sits-under-its-own-exec ()
  (with-frame-model (model 100 40 :session-id "band-pin" :tool-detail :developer)
    (setf (nle::state-background-rows model.state)
          (list (band-row :command "(sh \"make\")" :shell-id 1 :tail (format nil "compiling~%") :seen 1)
                (band-row :command "(sleep 60)" :shell-id 2)))
    (let ((lines (band-frame-plain model)))
      (is (search "(sleep 60)" (third lines)) "the newer exec follows its predecessor and output")
      (is (search "(sh \"make\")" (first lines)))
      (is (search "⌎ compiling" (second lines)) "the output pins to the exec that wrote it, under it")
      (is (eql (position #\✕ (first lines)) (position #\✕ (first lines))) "every exec's ✕ at one column"))))

(deftest clicking-a-band-exec-opens-it-in-place ()
  ;; A click on an exec unfolds its `+N lines' in place as a card does,
  ;; under the key its card takes once the exec exits, and a second click
  ;; closes it.
  (with-frame-model (model 100 40 :session-id "band-open" :tool-detail :developer)
    (let ((row (band-row :command "(sh \"ls\")" :shell-id 41
                         :head (format nil "l1~%l2~%")
                         :tail (format nil "~{l~d~%~}" '(1 2 3 4 5 6 7 8)) :seen 8)))
      (setf (nle::bg-band-row-tool-call-id row) "call-ls"
            (nle::state-background-rows model.state) (list row))
      (nle::render-state model)
      (click-at model 12 (frame-row-carrying model "+4 lines (click to expand)"))
      (is (null (nle::chat-model-dialog model)) "no dialog")
      (is (equal "call-ls" (nle::chat-model-expanded-tool-call model)) "its card's key")
      (let ((lines (band-frame-plain model)))
        (is (= 9 (length lines)) "the command and all eight lines")
        (is (search "l5" (nth 5 lines)) "the hidden ones among them"))
      (click-at model 12 (frame-row-carrying model "(sh \"ls\")"))
      (is (null (nle::chat-model-expanded-tool-call model)) "a click on its command closes it")
      (is (= 6 (length (band-frame-plain model))) "the five-row window again"))))

(deftest a-cut-tail-is-read-past-its-first-line (is-each (nle::kept-tail-text))
  ((format nil "k~%line 2~%line 3") t (format nil "line 2~%line 3") "a cut tail begins partway into its first line")
  ((format nil "line 1~%line 2") nil (format nil "line 1~%line 2") "a whole one is read whole")
  ("one long partial line" t "one long partial line" "a line that is all of it stays"))

(deftest an-ended-background-card-shows-its-last-lines-and-its-error ()
  ;; Without this, the card of a call that ran on would say how it ended and
  ;; nothing else: the output its exit kept, and the report it failed on,
  ;; would reach no card.
  (let ((state (bg-state (bg-call-fact "call-t" "{\"form\":\"(sh \\\"make\\\")\"}" :tool-name "eval")
                         (bg-backgrounding-result "call-t" 1900 "(sh \"make\")" "eval")
                         (bg-result-fact "call-t" "snippet" ""
                                         (nlk:exited-shell-document
                                          1900 "(sh \"make\")" 1
                                          :tail (format nil "~{line ~d~^~%~}"
                                                        (loop for i from 1 to 8 collect i))
                                          :error "SIMPLE-ERROR: boom")))))
    (let* ((llines (nle::tool-output-llines (tool-call-row state "call-t") 80))
           (last (car (last llines))))
      (let ((card '("line 1" "line 2" "+4 lines (click to expand)" "line 7" "line 8" "error: SIMPLE-ERROR: boom")))
        (is (equal card (mapcar #'nle::lline-text llines)) "the kept lines in the five-row window, then the error"))
      (is (eq :danger (nle::lline-tone (first llines))) "a failed exec's lines read in the danger tone")
      (is (and (eq :danger (nle::lline-tone last)) (not (nle::lline-dim last))) "its error at full strength"))))

;;; --- a running exec's output streams under its command -------------------
;;; Without this, Standard would hide the output behind a click; the operator
;;; wants it streaming right under the command, behind the hook a queued
;;; prompt hangs under the live line.
(deftest a-running-execs-output-streams-under-its-command-in-both-viewports ()
  (with-frame-model (model 100 40 :session-id "pending-output")
    (dolist (mode '(:standard :developer))
      (setf (nle::chat-model-tool-detail model) mode)
      (let ((row (band-row :command "make build" :shell-id 41
                           :tail (format nil "STDOUT-MARKER~%STDERR-MARKER~%") :seen 2)))
        (setf (nle::state-background-rows model.state) (list row))
        (let ((lines (band-frame-plain model)))
          (is (= 3 (length lines)) "the command, then both lines")
          (is (search "⌎ STDOUT-MARKER" (second lines)) "the first behind the hook")
          (is (eql (search "make build" (first lines)) (search "⌎" (second lines))) "on the command's column")
          (is (eql (search "STDOUT" (second lines)) (search "STDERR" (third lines))) "the next two in, under it")
          ;; A click on its output opens it.
          (is (every (lambda (ref) (eq ref row)) (nth-value 2 (nle::background-band-lines row 100)))))))))

(deftest standard-band-streams-each-growing-preview-frame ()
  (with-frame-model (model 100 40 :session-id "pending-stream" :tool-detail :standard)
    (loop for text in (list (format nil "stream-one~%") (format nil "stream-one~%stream-two~%"))
          for count from 2
          do (feed-events (nle::chat-model-state model)
               (bg-preview-fact "pending-stream" 77 "pending-call" "make build" text))
             (let ((lines (band-frame-plain model)))
               (is (= count (length lines)) "each frame's lines under the command")
               (is (rows-carry-p "make build" (list (first lines))))
               (is (search "⌎ stream-one" (second lines)))))
    (is (rows-carry-p "stream-two" (band-frame-plain model)) "the newest line streams in")
    ;; A live eval keeps no head: past five lines its window is the count,
    ;; then the newest four, and the hook hangs on the count.
    (feed-events (nle::chat-model-state model)
      (bg-preview-fact "pending-stream" 77 "pending-call" "make build"
                       (format nil "~{line ~d~%~}" (loop for i from 1 to 9 collect i))))
    (let ((lines (band-frame-plain model)))
      (is (search "⌎ +5 lines (click to expand)" (second lines)) "the hook never leaves the command")
      (is (search "line 6" (third lines)))
      (is (search "line 9" (sixth lines)) "the newest line last"))))

(deftest a-listed-running-call-streams-its-output-under-it ()
  ;; A Standard run stands folded; a call still running stands under it,
  ;; the band's exec too, and what it writes hangs under its line.
  (let ((nle::*tool-detail* :standard) (nle::*expanded-tool-call* nil)
        (state (nle::ingest-tool-call-fact nil "c1" "eval" (eval-args (format nil ";; Print the clock~%(sh \"clock\")"))
                                           :turn-id "live"))
        (exec (band-row :command "(sh \"clock\")" :shell-id 5
                        :head (format nil "1 21:12:30~%2 21:12:31~%")
                        :tail (format nil "~{~d 21:12:~d~%~}" (loop for i from 1 to 9 append (list i (+ 29 i))))
                        :seen 9)))
    (setf (nle::bg-band-row-tool-call-id exec) "c1"
          (nle::state-background-rows state) (list exec))
    (let* ((rows (mapcan (lambda (block) (copy-list (nle::display-block-plain-lines block)))
                         (nle:project-state state :width 100)))
           (call (position-if (lambda (row) (search "Print the clock" row)) rows)))
      (is call "the call stands under its folded run while it runs")
      (is (not (rows-carry-p "(sh \"clock\")" rows)) "its line alone, no code")
      (is (search "⌎ 1 21:12:30" (nth (1+ call) rows)) "its output right under it, behind the hook")
      (is (eql (search "Print" (nth call rows)) (search "⌎" (nth (1+ call) rows))) "the hook on its words' column")
      (is (eql (search "2 21" (nth (+ 2 call) rows)) (search "1 21" (nth (1+ call) rows))) "the next two in, under the first")
      (is (search "+5 lines" (nth (+ 3 call) rows)) "the five-row window counts what it leaves out")
      (is (not (search "click" (nth (+ 3 call) rows))) "and promises no click: a listed call opens its run")
      (is (search "9 21:12:38" (nth (+ 5 call) rows)) "its newest line last"))))

(deftest a-running-calls-line-burns-amber-and-flares-with-its-output ()
  ;; Grey read as a command that had stopped: a
  ;; running call's listed line burns amber, each line its exec writes flares
  ;; it white-hot, and while it is quiet it breathes.
  (with-frame-model (model 100 30 :session-id "ember" :tool-detail :standard)
    (let* ((nle::*color-profile* :truecolor)
           (state (nle::ingest-tool-call-fact nil "c1" "eval" (eval-args (format nil ";; Print the clock~%(sh \"clock\")"))
                                              :turn-id "live"))
           (exec (band-row :command "(sh \"clock\")" :shell-id 5 :tail (format nil "1 21:12:30~%") :seen 1))
           (call (tool-call-row state "c1"))
           (ms (/ internal-time-units-per-second 1000)))
      (setf (nle::bg-band-row-tool-call-id exec) "c1"
            (nle::state-background-rows state) (list exec)
            (nle::chat-model-state model) state)
      (mark-turn-in-flight model :render t)
      (is (eq :tool (nle::lline-tone (nle::run-tail-lline call 100))) "its words burn amber")
      (let* ((rows (nle::compose-frame model))
             (y (1- (frame-row-carrying model "Print the clock")))
             (ember (first (nle::ember-rows model))))
        (is (and ember (= y (first ember)) (eq call (fourth ember))) "the ember finds its line on screen")
        (destructuring-bind (x0 x1) (subseq ember 1 3)
          (is (search "Print the clock" (subseq (nle::plain-frame-row (nth y rows)) x0 x1)) "over its words")
          (let ((t0 (get-internal-real-time)))
            (nle::forget-ember-stamps call)
            (is-each (nle::ember-heat call (list exec))
              (t0 20 "first seen, it flares white-hot")
              ((+ t0 (* 350 ms)) 5 "and cools")
              ((+ t0 (* 700 ms)) 0 "back to its amber")
              ((+ t0 (* 1900 ms)) 6 "quiet, it breathes"))
            (setf (nle::bg-band-row-output-seen-lines exec) 2)
            (is (= 20 (nle::ember-heat call (list exec) (+ t0 (* 1950 ms)))) "a line written flares it again"))
          (let* ((screen (nlt:parse-styled-string rows 100 30))
                 (amber (nle::color-sgr (nle::visual-style-fg (nle::style-for-tone :tool)) t :truecolor)))
            (is (equal amber (nlt:cell-fg (nlt:screen-buffer-ref screen x0 y))) "laid out amber")
            (nle::forget-ember-stamps call)
            (funcall (nle::frame-overlay model) screen)
            (is (notany (lambda (x) (equal amber (nlt:cell-fg (nlt:screen-buffer-ref screen x y))))
                        (alexandria:iota (- x1 x0) :start x0)) "the flare warms every cell of its words")
            (is (equal '(20) (nle::ember-painted model)) "and is what the frame painted")
            (is (not (nle::ember-moved-p model)) "a tick inside the same heat repaints nothing")))
        ;; Done, it is grey again and the ember lets it go.
        (setf (nle::transcript-row-tool-status call) "completed"
              (nle::state-background-rows state) nil)
        (nle::render-state model)
        (nle::compose-frame model)
        (is (null (nle::ember-rows model)) "no ember on a finished call")))))
