;;;; ui-test.lisp --- C1 gates: reducer, fold, projection, and viewport.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; D0 claims (gates appended after the C2 block below):
;;;;   8. A UI-submitted turn calls the provider and both messages are durable.
;;;;   9. The incremental fold accumulates rather than discarding state.
;;;;  10. The cursor advances to the newest seq, not the oldest.
;;;;  11. project-state emits one block per message.
;;;;
;;;; C1 claims:
;;;;   1. fold-log on an empty log yields the seed state.
;;;;   2. apply-event default method treats unknown events as identity.
;;;;   3. Folding the same log twice yields equal state (replay determinism).
;;;;   4. A mutation that redefines the reducer changes the projection.
;;;;   5. A mutation that breaks the reducer leaves the log intact.
;;;;   6. Frame cost is proportional to visible height, not log length.
;;;;   7. Wide characters measure correctly.

(in-package #:nodecode.test)

(nlk:access (body nle::lline) (chord nle::key-chord) (dialog nle::picker-dialog)
            (divider nle::transcript-row) (done nle::display-block) (fill nle::visual-style)
            (group nle::tool-group) (header nle::lline) (line nle::lline) (link-span nle::mspan)
            (meter nle::seg) (painted nle::transcript-row) (placement nlt::image-placement)
            (ref nle::transcript-row) (span nle::mspan) (thought nle::transcript-row))

;;; --- helpers -------------------------------------------------------------

(defun make-test-session (&optional (id "c1-session"))
  "A session with a stable id. Interns the created fact when the store is open."
  (intern-durable-session id))

(defun record-test-turn (session input reply)
  "Commit one synthetic turn through the kernel lifecycle."
  (ensure-durable-session (nle:session-id session))
  (let ((turn (nlk:admit-turn (nle:session-id session)
                              (nlk::make-durable-id "test-command")
                              input
                              :definition-revision "ui-test-policy")))
    (nlk:complete-turn turn (nle:message "assistant" reply) :message-count 1))
  session)

(defun do-turns (session n)
  "Commit N synthetic turns on SESSION, returning SESSION."
  (dotimes (i n session)
    (record-test-turn session
                      (format nil "turn ~d input" (1+ i))
                      (format nil "reply ~d" (1+ i)))))

(defun folded-messages (session-id)
  "The conversational rows from a fold, as (ROLE . TEXT) pairs."
  ;; Reads the transcript-row lanes and keeps text rows and system notices;
  ;; tool rows and dividers are excluded.
  (loop for row in (nle::transcript-rows (nle:fold-log :session-id session-id))
        when (member row.kind '(:text :system-notice))
          collect (cons row.role
                        row.text)))

(defmacro with-chat-model ((var session-id) &body body)
  "Run BODY with VAR an initialized shell over SESSION-ID in a fresh temp
store — the unsized fixture; WITH-FRAME-MODEL below is the sized one."
  `(with-temp-store ()
     (let ((,var (make-instance 'nle:gateway-chat-model :ws nil :session-id ,session-id)))
       (nlt:init ,var)
       ,@body)))

(defun type-string (model text &optional key)
  "Put TEXT straight into MODEL's composer editor; answers MODEL."
  ;; KEY, when given, is then pressed: (type-string model "hi" :enter) submits.
  (nle::editor-insert model.editor text)
  (when key (press model key))
  model)

(defun press (model &rest keys)
  "Press each of KEYS on MODEL through UPDATE, a key's modifier following it as :MOD."
  (loop while keys
        do (let* ((code (pop keys)) (mod (when (eq :mod (first keys)) (pop keys) (pop keys))))
             (nle:update model (apply #'nlt:make-key-press-msg :code code (and mod `(:mod ,mod))))))
  model)

(defun composer-value (model)
  (nle::editor-value model.editor))

(defun picker-selected-value (dialog)
  (nle::picker-row-value (nth dialog.selection (nle::picker-visible-rows dialog))))

(defun picker-rows (&rest specs)
  "Picker rows from SPECS: a string is a row's label, a list is the label
followed by MAKE-PICKER-ROW's own keywords."
  (mapcar (lambda (spec) (apply #'nle::make-picker-row :label (uiop:ensure-list spec))) specs))

(defun row-labels (dialog)
  "DIALOG's row labels, in order."
  (mapcar #'nle::picker-row-label dialog.rows))

(defun row-keys (dialog) "DIALOG's row keys, in order." (mapcar #'nle::picker-row-key dialog.rows))

(defun visible-labels (dialog)
  "The labels of the rows DIALOG is showing, in order: the search box and the
open tab already applied, which is what an operator is looking at."
  (mapcar #'nle::picker-row-label (nle::picker-visible-rows dialog)))

(defun session-labels (dialog)
  "The sessions the /sessions DIALOG lists, in order, under its open tab and
box: its rows less the day headings over them."
  (mapcar #'nle::picker-row-label (remove-if #'nle::picker-row-heading (nle::picker-visible-rows dialog))))

(defun dialog-keys (model &rest keys)
  "Press each of KEYS on MODEL's open dialog in turn, as an operator types them."
  (dolist (key keys model)
    (nle::picker-dialog-key model key)))


;;; --- C1 gate 1: fold-log on empty log yields seed -----------------------

(deftest fold-empty-log-yields-seed (with-temp-store ())
  (let ((seed (alexandria:alist-hash-table '(("test-key" . 42)) :test #'equal)))
    (let ((result (nle:fold-log :session-id "nonexistent" :seed seed)))
      (is (eq result seed) "folding empty log returns seed unchanged")
      (is (= 42 (gethash "test-key" result)) "seed content preserved"))))

;;; --- C1 gate 2: default method treats unknown events as identity --------

(deftest apply-event-default-is-identity (with-temp-store ())
  (do-turns (make-test-session) 2)
  ;; Write a raw event with an unregistered kind
  (nlk:record-event "c1-session" "unknown-kind" (list :foo "bar"))
  (let ((state (nle:fold-log :session-id "c1-session")))
    (is (hash-table-p state) "fold completed without error")
    (is (= 2 (gethash "turns_completed" state 0)))))

;;; --- C1 gate 3: replay determinism ---------------------------------------

(deftest fold-is-deterministic (with-temp-store ())
  (do-turns (make-test-session) 5)
  (let ((state1 (nle:fold-log :session-id "c1-session"))
        (state2 (nle:fold-log :session-id "c1-session")))
    (is-table (key) (= (gethash key state1 0) (gethash key state2 0))
      ("turns_completed") ("last_message_count"))))

;;; --- C1 gate 4: redefining reducer changes projection --------------------

(deftest redefine-reducer-changes-projection (with-temp-store ())
  (do-turns (make-test-session) 3)
  ;; D0 note: this gate redefines the MESSAGE-EVENT reducer rather than the
  ;; TURN-COMPLETED one. The claim is the C plan's — a mutation that
  ;; redefines the reducer changes the projection of an unchanged log — and
  ;; it is asserted against whichever reducer actually feeds the projection.
  ;; D0 made that the transcript reducer: the projection no longer renders
  ;; turn counters, so doubling them is now invisible on screen, which is the
  ;; intended consequence of the log carrying conversation instead of counts.
  (let* ((state-before (nle:fold-log :session-id "c1-session"))
         (blocks-before (nle:project-state state-before)))
    ;; Redefine the transcript reducer to shout every message.
    (eval '(defmethod nle:apply-event (state (event nlk::message-event))
             (nle::ingest-message-fact
              state
              (nlk::message-event-role event)
              (string-upcase (nlk::message-event-content event))
              :turn-id (nlk::event-turn-id event))))
    (let ((blocks-after (nle:project-state (nle:fold-log :session-id "c1-session"))))
      (is (= 3 (gethash "turns_completed" state-before 0)))
      ;; A block prints as its kind and size; the projection is the rows it paints.
      (flet ((painted (blocks) (format nil "~{~{~a~%~}~}" (mapcar #'block-body-lines blocks))))
        (is (search "reply 1" (painted blocks-before)))
        (is (search "REPLY 1" (painted blocks-after)))
        (is (not (equal (painted blocks-before) (painted blocks-after))))))
    ;; Restore the original reducer: the canonical method is a one-line
    ;; delegation, so the restore is faithful rather than a hand copy.
    (eval '(defmethod nle:apply-event (state (event nlk::message-event))
             (nle::ingest-message-event state event)))))

;;; --- C1 gate 5: broken reducer leaves log intact -------------------------

(deftest broken-reducer-leaves-log-intact (with-temp-store ())
  (do-turns (make-test-session) 2)
  (let ((event-count-before (nlk:event-count :session-id "c1-session")))
    ;; Break the reducer
    (eval '(defmethod nle:apply-event (state (event nlk:turn-completed))
             (error "broken reducer")))
    ;; Folding should fail
    (signals-error error (nle:fold-log :session-id "c1-session"))
    ;; But the log should be intact
    (is (= event-count-before (nlk:event-count :session-id "c1-session")))
    ;; Restore the reducer via the canonical delegation.
    (eval '(defmethod nle:apply-event (state (event nlk:turn-completed))
             (nle::ingest-turn-completed state event)))
    ;; And verify we can fold again
    (is (= 2 (gethash "turns_completed" (nle:fold-log :session-id "c1-session") 0)))))

;;; --- D1 gate: tool rounds visible through the fold -----------------------
;;; The gap closed by D1: apply-event had no methods for tool-call-event and
;;; tool-result-event, so tool activity was invisible to every consumer that
;;; went through fold-log. The row spine sharpened the invariant: a tool call
;;; and its result are ONE row keyed by call id (the Zig phase-coalescing
;;; rule), and a completed turn leaves a persisted finish-divider row.

(deftest tool-rounds-visible-through-fold (with-temp-store ())
  (let ((nle::*tool-detail* :developer)
        (sid "d1-tool-rounds"))
    ;; A single turn: user → tool-call → tool-result → assistant.
    (seed-completed-turn sid "what is (+ 1 2)" "The answer is 3."
                         :tools '(("call-1" "eval" "(+ 1 2)" "3")))
    (let ((blocks (nle:project-state (nle:fold-log :session-id sid))))
      ;; Three: the turn's end is the answer's clock, no finish divider.
      (is (= 3 (length blocks)))
      ;; Retention: all three are persisted facts from the event log.
      (is (every (lambda (b) (eq :persisted-history b.retention))
                 blocks))
      ;; Order: messages and tool rounds interleave in log order.
      (is-table (needle which) (search needle (first (block-body-lines (which blocks))))
        ("what is (+ 1 2)" first "first block is the user message")
        ("eval" second "second block shows the tool call")
        ("(+ 1 2)" second "tool header carries the evaluated form"))
      (is (search "⎿ 3" (format nil "~{~a~}" (rest (block-body-lines
                                                    (second blocks))))))
      (is (not (search "running" (first (block-body-lines (second blocks))))))
      (is (search "The answer is 3." (first (block-body-lines (third blocks))))))))

;;; --- C1 gate 7: complete-turn emits events -------------------------------

(deftest complete-turn-emits-turn-completed (with-temp-store ())
  (let ((session (make-test-session)))
    (is (= 1 (nlk:event-count :session-id "c1-session")))
    (record-test-turn session "hello" "hi")
    (is (= 5 (nlk:event-count :session-id "c1-session")))
    (let ((ev (session-event "c1-session" :kind "turn.completed")))
      (is (typep ev 'nlk:turn-completed) "event is turn-completed")
      (is (= 1 (nlk:turn-completed-turn-number ev)) "turn_number is 1")
      (is (= 1 (nlk:turn-completed-message-count ev)) "message_count is 1"))))


;;; =========================================================================
;;; C2 gates: the live program adapter, one worker, mutable generics
;;; =========================================================================

;;; --- C2 gate 1: update returns without blocking --------------------------

(deftest update-returns-while-turn-in-flight (with-chat-model (model "c2-nonblock"))
  ;; Set a turn in flight
  (setf model.turn-in-flight t)
  (let ((start (get-internal-real-time)))
    ;; Send a tick message - should return immediately
    (nle:update model (nlt:make-tick-msg :time 0.5))
    (let ((elapsed-ms (/ (- (get-internal-real-time) start)
                        (float internal-time-units-per-second)
                        0.001)))
      (is (< elapsed-ms 100)
          (format nil "update returned in ~ams (non-blocking)" elapsed-ms)))))

;;; --- C2 gate 2: one worker ensures single-writer --------------------------

(deftest program-errors-stay-in-the-debug-log (with-temp-store ())
  ;; Internal loop/input/worker failures are debug-only. Explicit notices
  ;; still own user feedback, not arbitrary condition text.
  (let* ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "c2-errors"))
         (program (nlt:make-program model))
         (notices '())
         (log (make-string-output-stream))
         (nle::*system-notice-hook*
           (lambda (text error-p) (push (cons text error-p) notices)))
         (*error-output* (make-instance 'nle::tui-error-stream :fallback log)))
    (funcall program.error-handler
             :event-loop (make-condition 'simple-error
                                         :format-control "update blew up"))
    (is (null notices) "caught program errors never enter the notification lane")
    (is (null (nle::chat-model-toasts model)) "the model gained no diagnostic toast")
    (is (search "update blew up" (get-output-stream-string log)) "the raw failure remains available in the debug log")
    (is (eq #'nlt:report-error program.error-handler) "the shell reuses the canonical terminal-core error logger")))

;;; --- C2 gate 3: redefined view takes effect ------------------------------

(deftest redefined-view-takes-effect (with-chat-model (model "c2-view"))
  ;; Original view should not include "CUSTOM"
  (is (not (rows-carry-p "CUSTOM" (nlt:view-state-rows (nle:view model)))))

  ;; Redefine view to add a custom marker
  (eval '(defmethod nle:view ((model nle::chat-model))
           (nlt:make-view (list "CUSTOM VIEW"))))

  ;; New view should include "CUSTOM"
  (is (rows-carry-p "CUSTOM" (nlt:view-state-rows (nle:view model))))

  ;; Restore original view: the method is a one-line delegation to
  ;; SHELL-VIEW, so the restore is faithful rather than a hand-copied
  ;; body that drifts from the live shell.
  (eval '(defmethod nle:view ((model nle::chat-model))
           (nle::shell-view model))))

;;; --- C2 gate 5:

;;; --- D0 gate 8: a submitted turn calls the provider and is durable -------
;;; Drives NLE:RUN, the turn every shell's start_turn reaches at the gateway,
;;; so this proves the path rather than a copy of it.

(deftest ui-turn-calls-provider-and-is-durable (with-temp-store (store))
  (with-stub-provider ((wire-message "assistant" :content "the provider answered"))
    (nle:run "what did you do" :session (intern-durable-session "d0-provider")))
  ;; Restart: the answer can only come from disk now.
  (reopen-store store.path)
  (let ((messages (folded-messages "d0-provider")))
    (is (= 2 (length messages)))
    (is-table (expected end message) (equal expected (end (message messages)))
      ("user" car first "the user message is first")
      ("what did you do" cdr first "the user text is what was submitted")
      ("assistant" car second "the assistant message is second")
      ("the provider answered" cdr second
       "the assistant text came from the provider, not a placeholder"))))

(deftest a-failed-provider-turn-still-logs-the-question (with-temp-store ())
  (with-provider-stub (nil (error "provider is down"))
    (ignore-errors (nle:run "does this survive" :session (intern-durable-session "d0-failure"))))
  (let ((messages (folded-messages "d0-failure")))
    (is (= 2 (length messages)))
    (is (equal "does this survive" (cdr (first messages))))
    (is (search "turn failed" (cdr (second messages))))))

;;; --- D0 gate 9: the incremental fold accumulates -------------------------
;;; The defect this replaces: folding :AFTER a cursor with no :SEED discarded
;;; everything folded before it, so the displayed turn count fell as the session
;;; grew. Asserted by driving the fold exactly the way UPDATE drives it.

(deftest incremental-fold-accumulates-across-turns (with-temp-store ())
  (let ((session (intern-durable-session "d0-incremental"))
        (state nil)
        (cursor 0))
    (dotimes (i 3)
      (record-test-turn session
                        (format nil "question ~d" (1+ i))
                        (format nil "answer ~d" (1+ i)))
      (setf state (nle:fold-log :session-id "d0-incremental"
                                :after cursor
                                :seed state))
      (setf cursor (nlk::session-log-watermark "d0-incremental"))
      (is (= (1+ i) (gethash "turns_completed" state 0))
          (format nil "after turn ~d the folded count is ~d" (1+ i) (1+ i))))
    (is (= (gethash "turns_completed" (nle:fold-log :session-id "d0-incremental") 0)
           (gethash "turns_completed" state 0)))))

;;; --- D0 gate 10: the cursor advances to the newest seq -------------------
;;; The defect this replaces: PAGE-EVENTS with :LIMIT 1 pages oldest-first, so
;;; the cursor stayed pinned at the log's first seq and every fold replayed all
;;; of history.

(deftest cursor-advances-to-newest-seq (with-temp-store ())
  (do-turns (intern-durable-session "d0-cursor") 3)
  (let* ((all (nlk:page-events :session-id "d0-cursor" :limit 1000))
         (highest (reduce #'max (mapcar #'nlk:event-log-position all)))
         (oldest (nlk:event-log-position (first all))))
    (is (= highest (nlk::session-log-watermark "d0-cursor")))
    (is (/= oldest (nlk::session-log-watermark "d0-cursor")))))

;;; --- D0 gate 11: one display block per message ---------------------------

(deftest project-state-emits-one-block-per-message (with-temp-store ())
  (with-stub-provider ((wire-message "assistant" :content "first")
                       (wire-message "assistant" :content "second"))
    (nle:run "one" :session (intern-durable-session "d0-blocks"))
    (nle:run "two" :session (intern-durable-session "d0-blocks")))
  (let ((blocks (nle:project-state (nle:fold-log :session-id "d0-blocks"))))
    ;; A turn's end is its messages' clocks: no finish divider stands under it.
    (is (= 4 (length blocks)))
    (is (every (lambda (b) (eq :persisted-history b.retention))
               blocks))
    (is (every (lambda (b) (eq :text b.kind)) blocks))
    (is-table (needle which) (search needle (first (block-body-lines (which blocks))))
      ("one" first "the first block shows the operator's question")
      ("first" second "the second block shows the provider's answer")
      ("two" third "the next turn's question follows the answer"))))

(deftest a-multiline-answer-projects-one-line-per-line ()
  (let* ((state (nle::ingest-message-fact
                 nil "assistant" (format nil "alpha~%beta~%gamma"))))
    (is-let* ((block (first (nle:project-state state)))) ((length (block-words block)) = 3)
      ((first (block-words block)) "   alpha") ((third (block-words block)) "   gamma"))))

(defun row-lines (width &rest row-args)
  "The display lines of the block a transcript row of ROW-ARGS paints at WIDTH."
  (nle::display-block-lines
   (nle::row-display-block (apply #'nle::make-transcript-row row-args) :width width)))

(deftest blocks-own-their-padding-rows (let ((nle::*tool-detail* :developer)))
  ;; Padded blocks own both edges; thoughts and Standard disclosures have
  ;; their own compact policy. Filled notices extend through their pads;
  ;; user prose and other unfilled blocks pad onto the canvas, and two
  ;; touching canvas pads share one row.
  (flet ((two-blocks (role)
           (nle::transcript-line-table
            (nle:project-state
             (nle::ingest-message-fact (nle::ingest-message-fact nil role "first")
                                       role "second"))))
         (blank-p (line) (zerop (length (string-trim '(#\Space) line)))))
    (loop for (kind body . args) in '((:text "answer" :role "assistant")
                                      (:text "ask" :role "user")
                                      (:system-notice "notice")
                                      (:finish-divider "completed")
                                      (:tool-call "output" :tool-name "run" :tool-args "{}"))
          do (let ((lines (apply #'row-lines 60 :kind kind :text body args)))
               ;; A filled block's pad paints its background to the full width,
               ;; so the pad is blank rather than empty.
               (is (and (> (length lines) 2)
                        (every #'blank-p (list (first lines) (car (last lines)))))
                   (format nil "a ~(~a~) block opens and closes on a padding row"
                           kind))))
    (is (equal '(:user-block nil nil :system-block)
               (mapcar (lambda (args)
                         (nle::row-block-fill (apply #'nle::make-transcript-row :text "a" args)))
                       '((:kind :text :role "user")
                         (:kind :text :role "assistant")
                         (:kind :tool-call)
                         (:kind :system-notice)))))
    (let ((lines (two-blocks "assistant")))
      (is (= 5 (length lines)) "two messages stand one row apart")
      (is (every #'blank-p (list (first lines) (third lines) (fifth lines))))
      (is (= 3 (search "first" (second lines))))
      (is (= 3 (search "second" (fourth lines)))))
    (is (= 7 (length (two-blocks "user"))))))

(deftest bodiless-rows-own-no-rhythm (let ((nle::*tool-detail* :developer)))
  ;; Padding is a property of a body. A round whose `reasoning_content` is
  ;; whitespace (deepseek-v4-pro emits a bare " " between tool rounds) folds
  ;; to a thinking row that renders no logical lines; padding it would put
  ;; three blank rows between the tool cards around it instead of one.
  (flet ((lines (reasoning)
           (nle::transcript-line-table
            (nle:project-state
             (nle::ingest-tool-result-fact
              (nle::ingest-tool-call-fact
               (nle::ingest-message-fact
                (nle::ingest-tool-result-fact
                 (nle::ingest-tool-call-fact nil "c1" "eval" "{}")
                 "c1" "eval" "2")
                "assistant" nil :reasoning reasoning)
               "c2" "eval" "{}")
              "c2" "eval" "4")))))
    (is (equal (lines nil) (lines " ")))
    (is (= 3 (count " " (lines (coerce '(#\Space #\Newline #\Tab #\Return)
                                       'string))
                    :test #'equal)))
    (is (search "Thought" (format nil "~{~a~}" (lines "thinks"))))
    (let* ((row (nle::make-transcript-row :kind :reasoning :text "thinks"))
           (nle::*expanded-tool-call* row))
      (is (rows-carry-p "thinks" (nle::display-block-lines (nle::row-display-block row :width 60))))))
  (is (null (row-lines 60 :kind :reasoning :text " ")))
  ;; Nor does a body of blank lines: one empty line between its two pads would
  ;; draw three blank rows where the rhythm keeps one.
  (is (null (row-lines 60 :kind :finish-divider :role "system" :text "")) "a divider with no words draws no rows")
  (is (null (row-lines 60 :kind :system-notice :role "system" :text " ")) "nor does a notice with none")
  (is (null (nle::markdown-llines ""))))

(deftest hovered-blocks-paint-their-hover-background (let ((nle::*color-profile* :truecolor)
       (nle::*tool-detail* :developer)))
  ;; Tool cards lift onto hover; user prompts retain their filled surface.
  (flet ((rows (row hovered &aux (nle::*hover-row* (and hovered row)))
           (nle::display-block-lines (nle::row-display-block row :width 40)))
         (bg-sgr (tone)
           (format nil "48;2;~{~d~^;~}" (nle::visual-style-bg (nle::style-for-tone tone)))))
    (let* ((tool (nle::make-transcript-row :kind :tool-call :text "out"
                                           :tool-name "run" :tool-args "{}"))
           (prompt (nle::make-transcript-row :kind :text :text "ask" :role "user")))
      (is (not (rows-carry-p (bg-sgr :hover) (rows tool nil))))
      (is (every (lambda (line) (search (bg-sgr :hover) line)) (rows tool t)))
      (dolist (hover '(nil t nil))
        (is (every (lambda (line) (search (bg-sgr (if hover :user-block-hover :user-block)) line))
                   (rows prompt hover)) "user body and padding retain the appropriate block surface")))))

(deftest reasoning-renders-as-assistant-prose-under-a-shadow ()
  ;; The Zig reasoning arm (transcript.zig): the assistant base style, then
  ;; a dark transparent overlay over the text — not a quieter tone, and not
  ;; a darker block. The lisp port paints the very llines the assistant block
  ;; paints and lays the shadow over the text alone: prose and spans at half
  ;; the light, the canvas beneath them exactly the assistant block's
  ;; (SHADOW-STYLE).
  (let ((text (format nil "Plan: use `grep` for **speed**~%- one~%- two")))
    (flet ((block-rows (kind &rest args)
             ;; The prose itself: this exact thought opened by its accordion.
             ;; A message's clock rides its first row (a thought carries none):
             ;; the comparison reads the prose, so the aside is set by.
             (let* ((row (apply #'nle::make-transcript-row :kind kind :text text args))
                    (nle::*expanded-tool-call* row))
               (nth-value 0 (nle::llines-rows
                             (mapcar (lambda (lline)
                                       (let ((lline (copy-structure lline)))
                                         (setf (nle::lline-aside lline) nil)
                                         lline))
                                     (nle::row-body-llines row 40))
                             40))))
           (fg-sgr (rgb) (format nil "38;2;~{~d~^;~}" rgb))
           (bg-sgr (rgb) (format nil "48;2;~{~d~^;~}" rgb))
           (mask-fg (row)
             ;; ROW with every truecolor foreground parameter ("38;2;r;g;b")
             ;; replaced by a placeholder: what is left is the text, the
             ;; attributes, and the backgrounds — everything a shadow may
             ;; not touch.
             (cl-ppcre:regex-replace-all "38;2;\\d+;\\d+;\\d+" row "38;2;#"))
           (on-canvas (row canvas)
             ;; ROW with every ground the canvas: a shadowed row stands on
             ;; none of its own, so a strong run's warm chip goes with it.
             (cl-ppcre:regex-replace-all "48;2;\\d+;\\d+;\\d+" row
                                         (format nil "48;2;~{~d~^;~}" canvas))))
      (let ((nle::*color-profile* :none))
        (is (equal (block-rows :text :role "assistant") (block-rows :reasoning))))
      (let* ((nle::*color-profile* :truecolor)
             (fg (nle::visual-style-fg (nle:style-for-tone :foreground)))
             (canvas (nle::visual-style-bg (nle:style-for-tone :background)))
             (code (nle::visual-style-fg (nle:style-for-tone :inline-code)))
             (muted (nle::visual-style-fg (nle:style-for-tone :muted)))
             (assistant (format nil "~{~a~}" (block-rows :text :role "assistant")))
             (reasoning (block-rows :reasoning))
             (all (format nil "~{~a~}" reasoning)))
        (is (and (search (fg-sgr fg) assistant) (search (bg-sgr canvas) assistant)))
        (is (and (> (length reasoning) 2)
                 (every (lambda (row) (search (bg-sgr canvas) row)) reasoning)))
        (is (not (rows-carry-p (bg-sgr (nle::shadow-rgb canvas)) reasoning)))
        (is (search (fg-sgr (nle::shadow-rgb fg)) all))
        (is (search (fg-sgr (nle::shadow-rgb code)) all))
        (is (not (rows-carry-p (fg-sgr fg) (butlast (rest reasoning)))))
        (let ((prose (block-rows :text :role "assistant")))
          ;; the answer's strong run stands on a chip.
          (is (notevery #'equal prose (mapcar (lambda (row) (on-canvas row canvas)) prose)))
          (is (equal (mapcar (lambda (row) (mask-fg (on-canvas row canvas))) prose)
                     (mapcar #'mask-fg reasoning))))
        (is (not (search (fg-sgr muted) all)))))))

(deftest block-bodies-drop-surrounding-whitespace ()
  ;; Zig render_plan.transcriptMessageBody trims a block body's outer
  ;; newlines so content cannot out-pad the block-padding policy that owns
  ;; the transcript's vertical rhythm. The lisp policy applies that trim to
  ;; every block kind, not just reasoning and assistant prose.
  (flet ((block-lines (kind text &rest args)
           (butlast (rest (apply #'row-lines 60 :kind kind :text text args)))))
    (loop for (kind body . args) in '((:text "answer" :role "assistant")
                                      (:text "ask" :role "user")
                                      (:reasoning "thinking")
                                      (:system-notice "notice")
                                      (:tool-call "output" :tool-name "run" :tool-args "{}"))
          do (let ((padded (apply #'block-lines kind
                                  (format nil "~%~% ~a ~%~%" body) args)))
               (is (equal (apply #'block-lines kind body args) padded)
                   (format nil "a ~(~a~) block renders the same with or without ~
surrounding whitespace" kind))))
    (is (= 1 (length (block-lines :text (format nil "~%~%answer~%~%")
                                  :role "assistant"))))))

(deftest live-heap-flat-with-ui-turns (with-temp-store ())
  (let ((session (intern-durable-session "c2-heap")))
    ;; Warm up
    (do-turns session 5)
    (sb-ext:gc :full t)
    (let ((baseline (sb-kernel:dynamic-usage)))
      ;; Simulate 100 UI-submitted turns
      (do-turns session 100)
      (sb-ext:gc :full t)
      (let* ((after (sb-kernel:dynamic-usage))
             (growth (- after baseline))
             (ceiling (* 8 1024 1024)))
        (is (= 421 (nlk:event-count :session-id "c2-heap")))
        (format t "       heap: baseline ~:D B, after 100 turns ~:D B, growth ~:D B (ceiling ~:D B)~%"
                baseline after growth ceiling)
        (is (< growth ceiling)
            (format nil "heap growth ~:D B stayed under ~:D B ceiling" growth ceiling))))))

;;; --- C2 gate 6: the terminal core is the image's own -----------------------

(deftest tuition-absent-from-image ()
  ;; The core under src/term/ is what the shells run; nothing in the system
  ;; asks any searcher for the tuition system, so its package can never be
  ;; in the image by accident. FIND-PACKAGE, never ASDF:FIND-SYSTEM: the
  ;; latter consults local-projects and would answer for the machine.
  (is (null (find-package "TUITION")) "no TUITION package is loaded")
  (is (find-package "NODECODE.TERM") "the terminal core is"))

;;;; C3 gate --- a kept definition survives a restart with the UI in the image

;;; --- C3 gate: kept definition survives restart with UI threads live --------
;;; a definition evaled through the seam lands in the layer; a restart is
;;; the folder loading over the shipped image. UI threads change nothing
;;; about it — no fork, no ordering — which is the gate this test holds.

(deftest kept-definition-survives-restart-with-ui (with-temp-directory (dir "c3" :close-store t))
  (let ((db-path (merge-pathnames "history.db" dir)))
    (with-scribe-fixture
      (nlk:open-store :path db-path)
      ;; Bring up UI threads
      (let* ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "c3-restart"))
             (program (nlt:make-program model)))
        (nlt:start-worker program)
        (nlk:with-cleanup ((nlt:stop-worker program))
          ;; Define through the seam: the layer gets the file.
          (nle:eval-string "(defun c3-restart-probe () :kept)")
          (is (layer-file-text "c3-restart-probe.lisp"))
          ;; Write some events
          (do-turns (intern-durable-session "c3-restart") 2)
          (is (= 2 (gethash "turns_completed" (nle:fold-log :session-id "c3-restart") 0)))
          ;; Simulate losing the definition
          (let ((sym (find-symbol "C3-RESTART-PROBE" '#:nodecode.evolved)))
            (when (and sym (fboundp sym)) (fmakunbound sym)))
          (is (not (let ((s (find-symbol "C3-RESTART-PROBE" '#:nodecode.evolved)))
                     (and s (fboundp s)))))
          ;; Restart: close store, load the folder with UI threads live, reopen
          (nlk:close-store)
          (let* ((asdf:*central-registry* asdf:*central-registry*)
                 (asdf::*immutable-systems* asdf::*immutable-systems*)
                 (record (first (nlk:load-cells :root nlk::*cells-directory*))))
            (is (and record (eq :loaded record.state))
                (format nil "the layer loads with UI threads live: ~a"
                        (and record record.failures))))
          (let ((sym (find-symbol "C3-RESTART-PROBE" '#:nodecode.evolved)))
            (is (and sym (fboundp sym)) "probe present after the folder loads")
            (is (eq :kept (funcall sym)) "probe returns :kept"))
          (nlk:open-store :path db-path)
          (is (= 2 (gethash "turns_completed" (nle:fold-log :session-id "c3-restart") 0))))))))

;;; =========================================================================
;;; B2 gates: streamed rows, delta coalescing, live-only row lifecycle
;;; =========================================================================

;;; --- B2 gate: a fresh model renders no live rows -------------------------

(deftest fresh-model-projects-no-live-rows ()
  (is-let* ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "b2-slot")))
    ((nle:project-state model.state) null) ((nle::status-said model) null)))

;;; --- B2 gate: streamed text becomes a live-only row ----------------------

(deftest streamed-text-projects-a-live-only-block ()
  (let ((blocks (nle:project-state (nle::ingest-stream-body nil :text "streaming..."))))
    (is (= 1 (length blocks)))
    (let ((block (first blocks)))
      (is (eq :live-only block.retention))
      (is (search "streaming..." (format nil "~{~a~}" block.lines))))))

(deftest streamed-text-coalesces-into-the-adjacent-row ()
  ;; Two deltas are one row; a durable user fact between text rounds breaks
  ;; adjacency and opens a second row (the Zig adjacent-row rule).
  (let ((state (nle::ingest-stream-body nil :text "The answer")))
    (setf state (nle::ingest-stream-body state :text " is 42."))
    (is (= 1 (length (nle::transcript-rows state))))
    (is (equal "The answer is 42."
               (nle::transcript-row-text (first (nle::transcript-rows state)))))
    (setf state (nle::ingest-stream-tool state 0 "eval" "(+ 1 2)"))
    (setf state (nle::ingest-stream-body state :text "Round two."))
    (is (= 3 (length (nle::transcript-rows state))))))

(deftest a-round-s-calls-read-in-the-order-they-were-made ()
  ;; Two calls of one round stream into rows 0 and 1, and their facts land in
  ;; model order, the first call's result before the second call's fact
  ;; (RUN-TOOL-CALL: one call at a time). Each fact takes the row
  ;; its call streamed into: paired newest first. The edit's result finds the
  ;; edit by its id, not the ls row that has none yet.
  (let* ((edit (eval-args "(edit \"notes.md\" \"Old title here.\" \"The notes.\")"))
         (ls (eval-args "(sh \"ls -la && wc -l notes.md\")"))
         (state (nle::ingest-stream-tool nil 0 "eval" edit :turn-id "t1")))
    (setf state (nle::ingest-stream-tool state 1 "eval" ls :turn-id "t1"))
    (setf state (nle::ingest-tool-call-fact state "c-edit" "eval" edit :turn-id "t1"))
    (setf state (nle::ingest-tool-result-fact state "c-edit" "eval" "edited notes.md" :turn-id "t1"))
    (setf state (nle::ingest-tool-call-fact state "c-ls" "eval" ls :turn-id "t1"))
    (setf state (nle::ingest-tool-result-fact state "c-ls" "eval" "notes.md" :turn-id "t1"))
    (let ((rows (nle::transcript-rows state)))
      (is (equal '("c-edit" "c-ls") (mapcar #'nle::transcript-row-tool-call-id rows)) "in call order")
      (is (equal (list edit ls) (mapcar #'nle::transcript-row-tool-args rows)) "each on its own row")
      (is (equal '("edited notes.md" "notes.md") (mapcar #'row-string rows)) "its result under it"))))

(deftest a-prompt-queued-mid-answer-does-not-split-it ()
  ;; A follow-up queued while the answer streams parks a :PENDING-INPUT row at
  ;; the tail; the rest of the answer still goes on in the row it began in.
  (let ((state (nle::ingest-stream-body nil :text "Working through" :turn-id "t1")))
    (setf state (nle::ingest-pending-input-fact state "then add a flag" :prompt-id "p2"))
    (setf state (nle::ingest-stream-body state :text " it step by step." :turn-id "t1"))
    (is (equal '(:text :pending-input) (mapcar #'nle::transcript-row-kind (nle::transcript-rows state))))
    (is (equal "Working through it step by step."
               (row-string (first (nle::transcript-rows state)))))))

(deftest streamed-row-appends-after-history ()
  ;; A persisted fact renders before the live tail row.
  (let ((state (nle::ingest-message-fact nil "user" "hello")))
    (setf state (nle::ingest-stream-body state :text "partial..."))
    (is (equal '(:persisted-history :live-only)
               (mapcar #'nle::display-block-retention (nle:project-state state))))))

(defun row-string (row)
  "ROW's text as a plain string, however the row accumulated it."
  (coerce row.text 'string))

(deftest stream-reset-keeps-what-streamed-and-the-seal-owns-it ()
  ;; The reset keeps the failed attempt's thinking and text rows, voided.
  ;; The next attempt opens fresh rows below them; the turn.attempt_voided
  ;; fact seals the voided rows as persisted history, and the next round's
  ;; own facts never claim them.
  (let ((state (nle::ingest-stream-body nil :reasoning "old think" :turn-id "t1")))
    (setf state (nle::ingest-stream-body state :text "old text" :turn-id "t1"))
    (setf state (nle::ingest-stream-tool state 0 "eval" "{}" :turn-id "t1"))
    (setf state (nle::ingest-stream-reset state :turn-id "t1"
                                          :retry-status "retrying 1/5 in 0s"))
    (let ((rows (nle::transcript-rows state)))
      (is (equal '(:reasoning :text) (mapcar #'nle::transcript-row-kind rows)))
      (is (every #'nle::transcript-row-voided rows))
      (is (notany #'nle::transcript-row-hydrated rows)))
    ;; The retry streams again: fresh rows, never coalesced onto the voided.
    (setf state (nle::ingest-stream-body state :reasoning "new think" :turn-id "t1"))
    (setf state (nle::ingest-stream-body state :text "new text" :turn-id "t1"))
    (is (= 4 (length (nle::transcript-rows state))))
    (is (equal "old think" (row-string (first (nle::transcript-rows state)))))
    ;; The seal: the fact pairs with the voided rows, oldest first.
    (setf state (nle::ingest-attempt-voided-fact
                 state :attempt 1 :reasoning "old think" :text "old text"
                 :detail "the stream ended before the answer finished"
                 :stream-duration-ms 1134000 :disposition "rerun"
                 :turn-id "t1"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 4 (length rows)) "the seal hydrates in place, never appends")
      (is (every #'nle::transcript-row-hydrated (subseq rows 0 2)))
      (is (eq :persisted-history
              (nle::transcript-row-retention (second rows))))
      (is (equal "Attempt 1 cut off after 18m 54s: the stream ended before the answer finished. Trying again."
                 (nle::transcript-row-voided (second rows))))
      (is (eq t (nle::transcript-row-voided (first rows))))
      (is (notany #'nle::transcript-row-hydrated (cddr rows))))
    ;; The round's own fact claims the live rows, never the sealed ones.
    (setf state (nle::ingest-message-fact state "assistant" "new text"
                                          :turn-id "t1" :reasoning "new think"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 4 (length rows)) "the assistant fact hydrated the live rows")
      (is (every #'nle::transcript-row-hydrated rows) "every row is durable")
      (is (equal "old think" (row-string (first rows)))))
    (nle::settle-tail-rows state)
    (is (= 4 (length (nle::transcript-rows state))))))

(deftest attempt-voided-fact-seals-before-the-reset-on-the-gateway-stream ()
  ;; On the gateway stream the fact lands before the stream_reset delta:
  ;; the seal hydrates the streaming rows, and the reset then has nothing
  ;; left to void. Neither path duplicates a row.
  (let ((state (nle::ingest-stream-body nil :reasoning "think" :turn-id "t1")))
    (feed-events state
      (nlk:json-object "type" "turn.attempt_voided"
                       "turn_id" "t1"
                       "attempt" 1 "reasoning" "think"
                       "text" ""
                       "detail" "cut"
                       "stream-duration-ms" 5000
                       "disposition" "rerun")
      (nlk:json-object
       "type" "item_delta" "turn_id" "t1"
       "delta" (nlk:json-object "type" "stream_reset")))
    (is-let* ((rows (nle::transcript-rows state)))
      ((length rows) = 1 "one sealed row, no duplicate")
      ((nle::transcript-row-hydrated (first rows)) is "hydrated by the fact")
      ((nle::transcript-row-voided (first rows)) "Attempt 1 cut off after 5.0s: cut. Trying again.")))
  ;; Replay, nothing streamed: the fact opens the rows itself.
  (let ((rows (nle::transcript-rows
               (nle::ingest-attempt-voided-fact
                nil :attempt 2 :reasoning "think" :text "half"
                :detail "cut" :disposition "stood" :turn-id "t1"))))
    (is (equal '(:reasoning :text) (mapcar #'nle::transcript-row-kind rows)))
    (is (every #'nle::transcript-row-hydrated rows) "both hydrated")
    (is (equal "Attempt 2 cut off: cut. Not retried."
               (nle::transcript-row-voided (second rows))))))

(deftest continued-round-hydrates-only-the-continuation ()
  ;; A round that continued a cut attempt records the merged message; the
  ;; sealed prefix already rests as the voided block, so the assistant
  ;; fact hydrates only the continuation under it — on the local fold and
  ;; on the gateway stream alike.
  (with-temp-store ()
    (with-durable-turn (turn (ensure-durable-session "continued-round") :input "think hard"
                        :complete t)
      (nlk::record-attempt-voided turn :attempt 1 :reasoning "one"
                                       :detail "cut" :stream-duration-ms 200000
                                       :disposition "continued")
      (nlk:record-assistant-message
       turn (nlk:json-object "role" "assistant" "content" "answer"
                             "reasoning_content" (format nil "one~%two"))
       :continued-from '(:reasoning 4 :content 0)))
    (let* ((state (nle::fold-log :session-id "continued-round"))
           (rows (remove-if-not (lambda (kind) (member kind '(:reasoning :text)))
                                (nle::transcript-rows state) :key #'nle::transcript-row-kind)))
      (is (equal '("think hard" "one" "two" "answer") (mapcar #'row-string rows)))
      (is (equal "Attempt 1 cut off after 3m 20s: cut. Continued below."
                 (nle::transcript-row-voided (second rows))))
      (is (not (nle::transcript-row-voided (third rows))))))
  (let* ((state (nle::apply-gateway-event
                 nil (nlk:json-object "type" "turn.attempt_voided"
                                      "turn_id" "t1" "attempt" 1
                                      "reasoning" "one" "text" ""
                                      "detail" "cut" "disposition" "continued")))
         (state (nle::apply-gateway-event
                 state (nlk:json-object
                        "type" "turn.assistant_message_completed"
                        "turn_id" "t1"
                        "message" (nlk:json-object
                                   "role" "assistant" "content" "answer"
                                   "reasoning_content" (format nil "one~%two"))
                        "continued-reasoning-chars" 4
                        "continued-content-chars" 0))))
    (is (equal '("one" "two" "answer")
               (mapcar #'row-string (nle::transcript-rows state))))))

(deftest retry-status-says-continue-for-a-continuation ()
  (let ((retry (nle::retry-status (nlk:json-object "attempt" 2 "limit" 5 "delay_seconds" 0.0
                                                   "continuation" t)
                                  0)))
    (is (equal "continue 2 of 5 sent" (nle::retry-status-text retry 0)))))

(deftest voided-rows-render-dim-with-the-seal-label ()
  (is-let* ((state (nle::ingest-attempt-voided-fact
                    nil :attempt 1 :reasoning "a thought" :text "an answer"
                    :detail "cut" :stream-duration-ms 65000 :disposition "rerun"
                    :turn-id "t1"))
            (rows (nle::transcript-rows state))
            (text-lines (nle::row-llines-with-eviction (second rows) 80))
            (think-lines (nle::row-llines-with-eviction (first rows) 80)))
    ((every #'nle::lline-dim text-lines) is)
    ((every #'nle::lline-dim think-lines) is "so does its thinking row")
    ((nle::lline-text (car (last text-lines))) "Attempt 1 cut off after 1m 05s: cut. Trying again.")
    ((nle::lline-role (car (last text-lines))) eq :status)
    ((find "cut off" think-lines :key #'nle::lline-text :test #'search) not)))

(deftest a-seal-line-says-no-length-for-a-stream-cut-at-once ()
  ;; A dropped stream would read `attempt 5 voided after 0.0s'.
  (is-let* ((state (nle::ingest-attempt-voided-fact
                    nil :attempt 5 :text "Here is the start" :stream-duration-ms 30
                    :detail "the stream ended before the answer finished"
                    :disposition "rerun" :turn-id "t1")))
    ((nle::transcript-row-voided (first (nle::transcript-rows state)))
     "Attempt 5 cut off: the stream ended before the answer finished. Trying again.")))

;;; --- B2 gate: the durable fact hydrates the streamed row -----------------

(deftest assistant-fact-hydrates-the-streamed-row ()
  ;; The defect this locks out: the durable assistant fact used to APPEND
  ;; while the stream text stayed on screen, so multi-round turns rendered
  ;; the same text twice until the turn ended.
  (let ((state (nle::ingest-stream-body nil :text "The answer")))
    (setf state (nle::ingest-stream-body state :text " is 42."))
    (setf state (nle::ingest-message-fact state "assistant" "The answer is 42."))
    (let ((rows (nle::transcript-rows state)))
      (is (= 1 (length rows)))
      (is (eq :persisted-history (nle::transcript-row-retention (first rows))))
      (is (nle::transcript-row-hydrated (first rows))))))

;;; --- the submitted prompt row: painted at Enter, hydrated by its fact ------

(deftest submitted-prompt-row-hydrates-under-its-command-id ()
  ;; Enter paints the prompt at the tail before any frame; the durable input
  ;; fact carrying the same command id hydrates that row in place, so the
  ;; transcript holds exactly one user row for one prompt.
  (let* ((state (nle::ingest-message-fact nil "user" "earlier" :turn-id "t0"))
         (state (nle::ingest-submitted-prompt state "hello there" :prompt-id "cmd-1"))
         (painted (car (last (nle::transcript-rows state)))))
    (is (= 2 (length (nle::transcript-rows state))) "the painted row sits after history")
    (is-shape painted (.role "user") (.text "hello there") (.retention eq :live-only)
      (.hydrated not))
    (setf state (nle::ingest-message-fact state "user" "hello there"
                                          :turn-id "t1" :prompt-id "p-1"
                                          :command-id "cmd-1"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 2 (length rows)) "the fact hydrated the painted row, never a second one")
      (is (eq painted (second rows)) "the same row object")
      (is-shape painted (nle::transcript-row-hydrated is) (.retention eq :persisted-history)
        (.turn-id "t1") (.prompt-id "p-1")))
    ;; A user fact under another command id — another shell's prompt — appends.
    (setf state (nle::ingest-message-fact state "user" "theirs" :command-id "cmd-2"))
    (is (= 3 (length (nle::transcript-rows state))))))

(deftest drop-submitted-prompt-spares-the-hydrated-row ()
  (let ((state (nle::ingest-submitted-prompt nil "gone" :prompt-id "cmd-1")))
    (setf state (nle::ingest-submitted-prompt state "kept" :prompt-id "cmd-2"))
    (setf state (nle::ingest-message-fact state "user" "kept" :command-id "cmd-2"))
    (setf state (nle::drop-submitted-prompt state "cmd-1"))
    (setf state (nle::drop-submitted-prompt state "cmd-2"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 1 (length rows)) "only the unhydrated row went")
      (is (equal "kept" (nle::transcript-row-text (first rows)))))))

;;; --- B2 gate: live rows present mid-turn, settled after completion -------

(deftest streamed-rows-present-mid-turn-settled-after-completion ()
  ;; Drive chat-model the way the stream's item deltas do and assert live
  ;; rows render mid-turn and settle away when the turn completes undurably.
  (with-chat-model (model "b2-lifecycle")
    ;; Simulate delta parts arriving.
    (paint-model model (nle::ingest-stream-body :text "He"))
    (is (some (lambda (b) (eq :live-only b.retention)) (nle:project-state model.state)))

    ;; More deltas accumulate on the same row.
    (paint-model model (nle::ingest-stream-body :text "llo"))

    ;; Turn completion settles the tail: a stream row no durable fact
    ;; confirmed is dropped, not promoted.
    (nle::settle-tail-rows model.state)
    (is (every (lambda (b) (eq :persisted-history b.retention))
               (or (nle:project-state model.state) '())))))

;;; --- B2 gate: a turn hands its part observer to the streaming call -------

(deftest run-turn-part-fn-fires-during-streaming (with-temp-store ())
  ;; The observer a turn binds into *TURN-PART-FN* (the gateway's worker binds
  ;; its stream fanout there) must see each content chunk as a :text-delta
  ;; part. Verify by driving a turn with a mock streaming provider.
  (let ((received '()))
    ;; Stub dex:post to return a canned SSE stream.
    (with-mock-sse-stream ((chunk-json "tok1")
                           (chunk-json "tok2")
                           (chunk-json "tok3" :finish-reason "stop"))
      (let ((nle::*turn-part-fn* (lambda (part)
                                   (when (eq :text-delta
                                             (nle::stream-part-kind part))
                                     (push (nle::stream-part-text part)
                                           received)))))
        (nle:run "test" :session (intern-durable-session "b2-delta-fn"))))
    (is (equal '("tok1" "tok2" "tok3") (nreverse received)))))

;;; =========================================================================
;;; Frame composition gates: the Zig ChatShell stack, canvas, and gutter
;;; =========================================================================
;;; COMPOSE-FRAME owns the whole terminal grid: the renderer paints uncovered
;;; cells as reset-background blanks, so parity with the Zig full-frame
;;; memset (ui.zig ChatShell.render) demands exactly TERM-HEIGHT rows of
;;; TERM-WIDTH columns. Under :none (the pinned suite profile) the frame is
;;; plain text, so geometry asserts are byte-level; the painted gate rebinds
;;; the profile and the Tungstene palette locally.

(defun make-frame-model (width height &rest initargs
                         &key (session-id "frame-gate") tool-detail state replayed typed
                         in-flight slash &allow-other-keys)
  "A shell initialized and resized to WIDTH x HEIGHT via the real resize path,
so the gates cover what a SIGWINCH covers."
  ;;
  ;; The shell is the GATEWAY-CHAT-MODEL every product shell is, with no
  ;; socket unless :WS names one. Every other keyword reaches MAKE-INSTANCE
  ;; unchanged, so `:http-port 4321' is how a gate spells its transport.
  ;; TOOL-DETAIL, when given, is the viewport the shell is left in; STATE, when
  ;; given, replaces the folded state and is rendered; REPLAYED, that the
  ;; gateway's first replay has landed (the stream stands at sequence 0); TYPED,
  ;; the text typed into the composer, IN-FLIGHT, a turn under way and SLASH, a
  ;; command run, each before the body does.
  (let ((model (apply #'make-instance 'nle::gateway-chat-model :session-id session-id
                      (append (alexandria:remove-from-plist initargs :tool-detail :state :replayed
                                                            :typed :in-flight :slash)
                              '(:ws nil)))))
    ;; A terminal-io reply (the kitty graphics query) needs a terminal.
    (dolist (cmd (remove-if-not #'functionp (uiop:ensure-list (nlt:init model))))
      (nlk:when-let (msg (funcall cmd))
        (unless (nlt:terminal-io-msg-p msg) (nle:update model msg))))
    ;; The session's durable log, head chain and all, seeds the shell where
    ;; the gateway's replay would.
    (setf model.state
          (nle:fold-log :session-id session-id :seed model.state
                        :visible-set (and (nlk:store-open-p)
                                          (nlk:session-visible-turn-set session-id))))
    (nle:update model (nlt:make-window-size-msg :width width :height height))
    (when tool-detail (setf model.tool-detail tool-detail))
    (when state (setf model.state state) (nle::render-state model))
    (when replayed (setf (nle::gateway-model-last-sequence model) 0))
    (when typed (type-string model typed))
    (when in-flight (mark-turn-in-flight model))
    (when slash (nle::execute-slash-command model slash ""))
    model))

(defun land-model-facts (model &rest facts)
  "Land FACTS, the model-facts plist a gateway read answers, on MODEL as the
worker's read lands them."
  (nle::install-gateway-model-facts model facts (nle::gateway-model-facts-epoch model)))

(defvar *link-frames* (make-hash-table :test #'eq :weakness :key)
  "What each RECORDING-LINK carried: its frames' JSON text, newest first.")

(defun recording-link (&aux (link (nle::make-gw-loopback-client nil #'identity nil)))
  "A shell's link that carries what it is handed, as an open one does: the
in-process link's handle, registered with no writer and no gateway behind it,
its dispatch keeping each frame for LINK-COMMANDS."
  ;; The registry entry is what makes it open (GATEWAY-LINK-OPEN-P), and
  ;; NLE::STOP-GW-PEER hangs it up, as a gateway ending its writer does.
  (setf (nle::gw-loopback-client-dispatch link)
        (lambda (text) (push text (gethash link *link-frames*))))
  (sb-ext:with-locked-hash-table (nle::*gateway-peer-states*)
    (setf (gethash link nle::*gateway-peer-states*) (nle::make-gw-peer-state :socket link)))
  link)

(defmacro with-recording-link ((var) &body body)
  "Run BODY with VAR a RECORDING-LINK, hung up however BODY leaves."
  `(let ((,var (recording-link)))
     (nlk:with-cleanup ((nle::stop-gw-peer ,var)) ,@body)))

(defun link-commands (link)
  "The wire names of the commands LINK carried, oldest first."
  (mapcar (lambda (text) (nlk:json-value (shasht:read-json text) :string "command" "type"))
          (reverse (gethash link *link-frames*))))

(defmacro with-frame-model ((var width height &rest args) &body body)
  "Run BODY with VAR bound to a WIDTH x HEIGHT shell inside a fresh temp store."
  ;; Every shell-level gate opens the same way — a store to fold against and
  ;; an initialized model over it — and writing the two forms out separately
  ;; says nothing the geometry and the session id do not already say. ARGS
  ;; reach MAKE-FRAME-MODEL, so the class and the transport keywords stay
  ;; expressible. :PAINT's calls are PAINT-MODEL's, run on the new shell first.
  `(with-temp-store ()
     (let ((,var (make-frame-model ,width ,height ,@(alexandria:remove-from-plist args :paint))))
       ,@(when (getf args :paint) `((paint-model ,var ,@(getf args :paint))))
       ,@body)))

(deftest shell-first-frame-stands-without-a-gateway ()
  ;; Paint-first: a gateway shell with NO endpoint — the owner behind it is
  ;; still electing — composes a complete frame from what it already holds:
  ;; the connecting placeholder, a footer that claims no endpoint, and not
  ;; one network call (the portless control seam refuses structurally).
  (with-frame-model (model 100 30 :session-id "paint-first")
    (let ((rows (nle::compose-frame model)))
      (is (= 30 (length rows)) "a full frame composes with no endpoint")
      (is (row-carrying "Connecting to Gateway" rows))
      (is (not (rows-carry-p "Gateway 127.0.0.1" rows))))))

(deftest footer-facts-never-dial-on-the-frame-path ()
  ;; Composing fetched model facts on the loop thread would let a dead
  ;; endpoint park the first paint for the connect timeout.
  ;; The frame renders the cache; the fetch is a worker command.
  (with-frame-model (model 100 30 :session-id "dead-endpoint")
    (setf (nle::gateway-model-http-port model) 1) ; a port nobody serves
    (let ((started (get-internal-real-time)))
      (is (= 30 (length (nle::compose-frame model))) "the frame composes")
      (is (< (- (get-internal-real-time) started)
             (/ internal-time-units-per-second 2)))
      (is (eq :unresolved (nle::gateway-model-model-facts model))))))

(deftest frame-covers-the-terminal-grid (with-frame-model (model 80 24))
  ;; A committed prompt, because the header's identity lane is chat
  ;; chrome: an empty shell is the landing screen, which carries none
  ;; (HOME-SCREEN-CARRIES-NO-SESSION-IDENTITY is that gate).
  (fold-model model (nle::ingest-message-fact "user" "hello"))
  (land-model-facts model :provider "p" :model "m")
  (let ((rows (nle::compose-frame model)))
    (is (= 24 (length rows)) "one frame row per terminal row")
    (is (every (lambda (row) (= 80 (nle::text-display-width row))) rows))
    (is (string= (make-string 80 :initial-element #\Space) (first rows)))
    (is (string= (make-string 80 :initial-element #\Space)
                 (first (last rows))))
    (is (eql 3 (search "hello" (second rows))))
    ;; Standard's header is the title alone: the raw session id is Developer's.
    (is (not (search "frame-gate" (second rows))))
    (is-table (needle row) (search needle (nth row rows))
      ("Ready" 19 "the composer text row (height - 5) carries the Ready placeholder")
      ("p m · ← sessions" 22 "the footer lane sits at height - 2 and carries the model and gesture"))))

(deftest ready-placeholder-paints-muted (let ((nle::*color-profile* :truecolor)))
  ;; The floor placeholder is the absence of input, not input: it reads in
  ;; the muted tone over the composer fill, never in the box's own
  ;; foreground, the way every other placeholder line does. A committed
  ;; prompt leaves home, whose own narrative the slot would carry.
  (with-frame-model (model 80 24 :session-id "ready-tone")
    (fold-model model (nle::ingest-message-fact "user" "hello"))
    (let* ((row (nth 19 (nle::compose-frame model)))
           (fill (nle::style-for-tone :composer)))
      (is (search (nle:styled (format nil "~vaReady" (nle::composer-text-inset) "")
                              (nle::style-with (nle::style-for-tone :muted) :bg fill.bg))
                  row))
      (is (not (search (nle:styled " Ready" fill) row))))))

(deftest a-running-turn-asks-for-a-steer-not-ready ()
  ;; Without this, `Ready' would read as idle while a turn runs: the empty box
  ;; says what typing does, and the keys wait until they would do something.
  (with-frame-model (model 80 24 :session-id "steer-placeholder")
    (fold-model model (nle::ingest-message-fact "user" "hello"))
    (is (search " Ready" (nth 19 (nle::compose-frame model))) "idle: the floor placeholder")
    (mark-turn-in-flight model)
    (let ((rows (nle::compose-frame model)))
      (is (search " Type to steer" (nth 19 rows)) "a running turn: what typing does")
      (is (not (search "Ready" (nth 19 rows))))
      (is (not (search "steer " (nth 20 rows))) "no keys before there is text to send"))
    ;; Typed into, the box names Enter and the queue key under the text, at
    ;; its right edge, with the amber edge at its left.
    (nle::replace-composer-text model "use the other branch")
    (let ((rows (nle::compose-frame model)))
      (is (search "use the other branch" (nth 19 rows)))
      (is (search " enter  steer   tab  queue  " (nth 20 rows)) "the keys, flush right")
      (is (every (lambda (row) (eql 1 (search "▐" row))) (subseq rows 18 21)) "the amber edge runs down the box"))
    ;; The keys come from the keymap: a rebound queue key says its chord, an
    ;; unbound one drops out.
    (let ((keymap (nle::chat-model-keymap model)))
      (setf (gethash :submit-turn-queue keymap) (list (nle::parse-key-chord "ctrl+t")))
      (is (search " ctrl+t  queue" (nth 20 (nle::compose-frame model))))
      (remhash :submit-turn-queue keymap)
      (let ((row (nth 20 (nle::compose-frame model))))
        (is (search " enter  steer" row))
        (is (not (search "queue" row)))))
    ;; The turn ends under the typed text: the warmth and the keys leave, and
    ;; Enter goes back to starting a turn.
    (setf (nle::chat-model-turn-in-flight model) nil)
    (let ((rows (nle::compose-frame model)))
      (is (search "use the other branch" (nth 19 rows)))
      (is (not (search "steer" (nth 20 rows))))
      (is (notany (lambda (row) (search "▐" row)) (subseq rows 18 21))))))

(deftest a-steering-composer-warms-and-its-keys-keep-their-caps (let ((nle::*color-profile* :truecolor)))
  ;; The box typed into while a turn runs takes the warm steer ground, and its
  ;; keys keep their own filled caps on it; the turn's end gives the plain
  ;; user-block ground back.
  (with-frame-model (model 80 24 :session-id "steer-warm")
    (fold-model model (nle::ingest-message-fact "user" "hello"))
    (mark-turn-in-flight model)
    (nle::replace-composer-text model "use the other branch")
    (flet ((prefix (tone) (nle::sgr-prefix (nle::style-for-tone tone) :truecolor)))
      (let ((rows (nle::compose-frame model)))
        (is (search (prefix :composer-steer) (nth 19 rows)) "the typed row on the warm ground")
        (is (search (nle:styled " enter " (nle::style-for-tone :key-chip)) (nth 20 rows)) "the key keeps its cap's ground"))
      (setf (nle::chat-model-turn-in-flight model) nil)
      (let ((row (nth 19 (nle::compose-frame model))))
        (is (not (search (prefix :composer-steer) row)))
        (is (search (prefix :user-block) row))))))

(deftest footer-and-composer-end-where-the-message-boxes-do (with-frame-model (model 80 24))
  ;; One right edge: the message boxes', two columns in plus the transcript's
  ;; width. Without this, the composer box would stick out three columns past
  ;; it and the footer's right text two.
  (dolist (width '(60 80 157))
    (let ((edge (+ 2 (nle::transcript-text-width width)))
          (row (nle::plain-frame-row (nle::bilateral-frame-row "left" :muted "right" width nil))))
      (is (= (nle::composer-box-width width) (nle::transcript-text-width width)) "the box spans the transcript's columns")
      (is (= edge (length (string-right-trim " " row))) "a chrome row's right text ends on the box edge")
      (is (eql 3 (search "left" row)) "and its left text starts on the gutter rail")))
  (land-model-facts model :provider "a6api-oai" :model "gpt-5.6-sol")
  (is (eql 3 (search "a6api-oai" (nth 22 (nle::compose-frame model)))) "the footer's own left text, on the rail"))

(deftest footer-carries-the-reasoning-effort-suffix (with-frame-model (model 80 24))
  ;; A stored /think selection rides the footer identity (the Zig
  ;; appendReasoningEffort ` (high)` suffix): the facts the gateway answers
  ;; carry it to the next frame, and facts without it remove the suffix.
  (land-model-facts model :provider "a6api-oai" :model "gpt-5.6-sol")
  (is (search "a6api-oai gpt-5.6-sol · ← sessions" (nth 22 (nle::compose-frame model))))
  (land-model-facts model :provider "a6api-oai" :model "gpt-5.6-sol" :effort "high")
  (is (search "a6api-oai gpt-5.6-sol (high) · ← sessions" (nth 22 (nle::compose-frame model))))
  (land-model-facts model :provider "a6api-oai" :model "gpt-5.6-sol")
  (is-carrying (row (nth 22 (nle::compose-frame model)))
    (:absent "(high)" "clearing the preference drops the suffix next frame")))

(deftest transcript-band-carries-the-scroll-gutter (with-frame-model (model 80 24))
  (let ((band (subseq (nle::compose-frame model) 3 17)))
    (is (= 14 (length band)) "the 24-row shell gives the band 14 rows")
    (is (every (lambda (row) (string= "  " (subseq row 78))) band) "a band with nothing to scroll draws no rail"))
  ;; A turn's words have one place, so the lines are a dozen turns' answers.
  (dotimes (i 12)
    (fold-model model (nle::ingest-message-fact "assistant" (format nil "line ~d" i) :turn-id (format nil "t~d" i))))
  (nle::render-state model)
  (let ((band (subseq (nle::compose-frame model) 3 17)))
    (is (some (lambda (row) (char= #\▐ (char row 79))) band) "one that overflows draws its thumb in the last column")
    (is (every (lambda (row) (find (char row 79) "▐ ")) band) "alone: off the thumb the gutter is canvas, no track")
    (is (every (lambda (row) (char= #\Space (char row 78))) band) "one blank column between the text and the rail")))

(deftest transcript-content-bottom-anchors (with-frame-model (model 80 24))
  (paint-model model (nle::ingest-message-fact "assistant" "hello organism"))
  (let ((rows (nle::compose-frame model)))
    ;; The band bottom is the block's own trailing padding row, so the
    ;; body sits one row above it.
    (is (search "hello organism" (nth 15 rows)))
    (is (eql 2 (search "   hello organism" (nth 15 rows))))
    (is (string= (make-string 77 :initial-element #\Space)
                 (subseq (nth 3 rows) 0 77)))))

(deftest painted-frame-inherits-the-tungstene-terminal-canvas ()
  (let ((nle:*color-profile* :truecolor)
        (nle:*palette* (theme-palette "Tungstene" :user nil)))
    (with-frame-model (model 40 24 :session-id "paint-gate")
      (let ((rows (nle::compose-frame model)))
        (is (string= (make-string 40 :initial-element #\Space) (first rows)) "canvas row leaves the terminal background unchanged")
        (is (search "48;2;59;57;55" (nth 19 rows)))
        (is (not (search "48;2;87;83;78" (nth 3 rows))) "a band with nothing to scroll paints no track")
        (type-string model "hi")
        (is (search "48;2;59;57;55" (nth 19 (nle::compose-frame model))))))))

(deftest short-windows-drop-footer-and-box (with-frame-model (model 60 10))
  (is-let* ((rows (nle::compose-frame model)))
    ((length rows) = 10 "short frames still cover every terminal row")
    ((every (lambda (row) (= 60 (nle::text-display-width row))) rows) is)
    ((rows-carry-p "ctrl+c quits" rows) not) ((search "Connecting to Gateway" (nth 7 rows)) is)))

;;; --- fence syntax highlighting + decoded tool headers -----------------------
;;; The tokenizer seams from theme.lisp wired into the display policy: fenced
;;; code bodies carry syntax-token spans for the fence's language (Zig
;;; appendBodyTextSegments), and a tool call whose arguments decode to a
;;; command string renders as that command with shell tokens over a
;;; function-toned base (Zig shell_command headers).

(defun span-tone-at (spans pos)
  (loop for span in spans when (and (<= span.start pos) (< pos span.end)) return span.tone))

(defun tool-row (tool-name tool-args &rest args)
  "The tool-card transcript row every tool-display gate opens with."
  ;; All of them are an assistant :TOOL-CALL — that pair is what makes the row
  ;; a card rather than prose — so the tool and its arguments are the only two
  ;; things a site ever varies. ARGS reach MAKE-TRANSCRIPT-ROW, where :TEXT,
  ;; :TOOL-STATUS, :ERROR-P and :METADATA stay expressible.
  (apply #'nle::make-transcript-row :kind :tool-call :role "assistant"
         :tool-name tool-name :tool-args tool-args args))

(defun tool-header-line (tool-name tool-args &rest args)
  "The header lline TOOL-ROW renders."
  ;; The lline and not its text, because the header gates assert on the spans
  ;; painted over it as often as on the string.
  (first (nle::tool-header-llines (apply #'tool-row tool-name tool-args args))))

(defun tool-output-texts (tool-name &rest args)
  "The plain text of the output body TOOL-ROW renders, at the 60-column width
every output gate measures against."
  (mapcar #'nle::lline-text
          (nle::tool-output-llines (apply #'tool-row tool-name nil args) 60)))

(deftest fence-bodies-carry-syntax-token-spans ()
  (let ((llines (nle::markdown-llines
                 (format nil "```python~%def foo():~%return 42~%```"))))
    ;; the language label, two body lines and the slab's blank last row; the markers hide.
    (is (= 4 (length llines)))
    (is (equal "python" (nle::lline-text (first llines))))
    (is (equal "" (nle::lline-text (fourth llines))))
    (let ((body (second llines)))
      (is (equal "def foo():" body.text))
      (is-each (span-tone-at (nle::lline-spans body))
        (0 :syntax-keyword "def is a python keyword")
        (4 :syntax-function "foo before ( reads as a function")
        (7 :syntax-punctuation "parens are punctuation")))
    (is-each (span-tone-at (nle::lline-spans (third llines)))
      (0 :syntax-keyword "return is a keyword")
      (7 :syntax-number "42 is a number")))
  (let ((llines (nle::markdown-llines (format nil "```~%plain text~%```"))))
    (is (null (span-tone-at (nle::lline-spans (first llines)) 0)))))

(deftest tool-header-decodes-the-snippet-form ()
  (let ((header (tool-header-line "eval" "{\"form\":\"(+ 1 2)\"}"
                                  :tool-status "running")))
    (is-shape header (.text "(+ 1 2) running") (.tone eq :syntax-function))
    (is-each (span-tone-at header.spans)
      (1 :syntax-command "the head of the form colors as the command")
      (3 :syntax-number "literals inside the form keep their token tones")
      (8 :tool "the running word stands out in the tool's tone")))
  (let* ((header (tool-header-line "evolved_shell"
                                   "{\"command\":\"rg -n foo | head\"}"
                                   :tool-status "error"))
         (spans header.spans))
    (is (equal "Searched 'foo' failed" header.text))
    (is (eq :tool (span-tone-at spans 0)) "the verb carries the tool tone")
    (is (eq :danger (span-tone-at spans 16)) "the failed tail is danger")))

(deftest a-tool-card-shows-its-snippet-only-while-open ()
  ;; The Lisp is one level down. A collapsed card is its title
  ;; and its output window; opening it with its own accordion
  ;; puts the snippet back under the title.
  (let* ((args "{\"form\":\"(let ((out (sh \\\"rg -n defun src\\\")))\\n  (length out))\"}")
         (row (tool-row "eval" args :tool-call-id "c7" :tool-status "complete" :text "12")))
    (let ((texts (mapcar #'nle::lline-text (nle::tool-header-llines row))))
      (is (equal '("Searched 'defun'") texts) texts))
    (is-let* ((nle::*expanded-tool-call* "c7")
              (texts (mapcar #'nle::lline-text (nle::tool-header-llines row))))
      (texts '("Searched 'defun'" "(let ((out (sh \"rg -n defun src\")))" "  (length out))") texts))
    (let ((nle::*tool-detail* :standard))
      (is (= 1 (length (nle::tool-header-llines row))) "changing mode does not open a card"))))

(deftest a-stopped-cards-output-is-what-it-wrote-and-who-stopped-it ()
  ;; Without this, a call the operator stopped would read `failed' in red,
  ;; its body the compiler's report of the abort. Its card is the lines it
  ;; wrote, then who stopped it, in the ordinary tone; the model's text stays
  ;; the model's.
  (let* ((stop (nle::stop-document :operator :after-ms 2400 :tail (format nil "step 1~%step 2")))
         (row (tool-row "eval" (eval-args "(sh \"for i in 1 2 3; do echo step $i; sleep 1; done\")")
                        :tool-call-id "c-stop" :tool-status "stopped"
                        :text (format nil "; compilation unit aborted~%ERROR: evaluation aborted by (eval-interrupt)")
                        :metadata (nlk:json-object "stopped" stop)))
         (lines (nle::tool-output-llines row 60)))
    (is (equal '("step 1" "step 2" "stopped by you after 2.4s") (mapcar #'nle::lline-text lines)))
    (is (notany (lambda (line) (eq :danger (nle::lline-tone line))) lines) "no line in the danger tone")
    (is (search "stopped" (nle::lline-text (first (nle::tool-header-llines row)))) "the header says stopped"))
  (is-each (nle::stop-words)
    ((nle::stop-document :shutdown :after-ms 900) "stopped: the gateway shut down while this ran" "a shutdown")
    ((nle::stop-document :turn :reason "the wait ran out") "stopped: the wait ran out" "a turn's cancel")))

;;; --- the collapse-parity gaps --------------------------------
;;; A closed card's omission line names the chord that opens it, and a
;;; command its title had to cut reads under the title once.

(deftest a-closed-cards-omission-line-names-the-click-that-opens-it ()
  (let ((row (tool-row "eval" (eval-args "(sh \"seq 1 40\")")
                       :tool-call-id "c20d" :tool-status "complete"
                       :text (format nil "~{~a~%~}" (loop for i from 1 to 40 collect i)))))
    (is (member "+36 lines (click to expand)"
                (mapcar #'nle::lline-text (nle::tool-output-llines row 60)) :test #'equal))))

(deftest a-command-card-shows-the-command-its-title-could-not-carry ()
  ;; ACTIVITY-SOURCE-TEXT cuts a source at *ACTIVITY-SOURCE-LIMIT*, and a snippet
  ;; that ran several is titled `Ran 2 commands' — the command itself then
  ;; reads under the title, and a command the title carries whole reads once.
  (flet ((header-texts (form id text)
           (mapcar #'nle::lline-text
                   (nle::tool-header-llines
                    (tool-row "eval" (eval-args form)
                              :tool-call-id id :tool-status "complete" :text text)))))
    (let* ((command "gh issue view 14 -R example-org/website --json title,body,comments")
           (texts (header-texts (format nil "(sh ~s)" command) "c20f" "{}")))
      (is (equal (format nil "$ ~a" command) (second texts))
          (format nil "the cut command stands under the title: ~s" texts)))
    (let ((texts (header-texts "(sh \"just lint\")" "c20g" "ok")))
      (is (equal '("Ran just lint") texts)
          (format nil "a command the title carries whole reads once: ~s" texts)))
    (let ((texts (header-texts "(progn (sh \"just lint\") (sh \"just test\"))" "c20h" "ok")))
      (is (equal '("Ran 2 commands" "$ just lint" "$ just test") texts)
          (format nil "a snippet that ran several names each: ~s" texts)))))

(deftest details-names-the-viewport-ctrlo-cycles ()
  (with-frame-model (model 100 40 :session-id "details-level")
    (is (null (nle::slash-details-op model "standard")))
    (is (equal "Switched to Standard viewport" (nle::status-said model)))
    (is (eq :standard model.tool-detail))
    (is (null (nle::slash-details-op model "developer")))
    (is (equal "Switched to Developer viewport" (nle::status-said model)))
    (is (eq :developer model.tool-detail))
    (is (search "usage: details" (nle::slash-details-op model "expanded")))
    (is-shape model (.tool-detail eq :developer) (nle::toggle-tool-detail eq :standard))
    (is (member "details" (mapcar #'nle::slash-command-name nle::*slash-commands*) :test #'equal))))

(deftest the-frame-names-the-chord-and-the-cut-command (with-temp-store ())
  ;; The render-state binding end to end: a real frame's closed card names the
  ;; chord MODEL's keymap binds, and the command its title had to cut.
  (seed-completed-turn "card-frame" "run it" "Done."
                       :tools (list
                               (list "c20i" "eval"
                                     (eval-args
                                      "(sh \"gh issue view 14 -R example-org/website --json title,body,comments\")")
                                     (format nil "~{~a~%~}"
                                             (loop for i from 1 to 30
                                                   collect "a line of output")))))
  (let* ((model (make-frame-model 100 30 :session-id "card-frame" :tool-detail :developer))
         (rows (progn (nle::render-state model)
                      (nle::compose-frame model))))
    (is (row-carrying "(click to expand)" rows))
    (is (row-carrying "$ gh issue view 14" rows))))

;;; --- the turn reads as words -----------------------------------
;;; Routine calls fold into one line per run, thoughts fold to one line, and
;;; Developer keeps this projection; Standard uses individual disclosures. The rows
;;; are never touched: a projection pass over them.

(defun mixed-turn-state (&aux (st nil))
  "One turn as its facts fold: a prompt, a thought, two reads with a thought
between them, a search, an edit, a last thought and the answer."
  (flet ((call (id form result &key error-p)
           (setf st (nle::ingest-tool-call-fact st id "eval" (eval-args form) :turn-id "t1")
                 st (nle::ingest-tool-result-fact st id "eval" result
                                                  :turn-id "t1" :error-p error-p)))
         (think (text)
           (setf st (nle::ingest-message-fact st "assistant" "" :reasoning text
                                                            :turn-id "t1"))))
    (setf st (nle::ingest-message-fact st "user" "go" :turn-id "t1"))
    (think "plan it")
    (call "c1" "(sh \"cat a.lisp\")" "(a)")
    (think "and the other")
    (call "c2" "(sh \"cat b.lisp\")" "(b)")
    (call "c3" "(sh \"rg -n needle src\")" "src/x:1:needle")
    (call "c4" "(edit \"a.lisp\" ($ \"o\") ($ \"n\"))" "edited a.lisp: line 1, -1 +1 line")
    (think "done")
    (setf st (nle::ingest-message-fact st "assistant" "The answer." :turn-id "t1")))
  st)

(defun block-text (block)
  (format nil "~{~a~^~%~}" block.plain-lines))

(deftest routine-calls-fold-into-one-line-per-run (let ((nle::*tool-detail* :developer)))
  (let* ((blocks (nle:project-state (mixed-turn-state) :width 80))
         (texts (mapcar #'block-text blocks)))
    (is (= 6 (length blocks))
        (format nil "prompt, thought, group, edit, thought, answer: ~s" texts))
    (is (search "Thought" (second texts)) "a thought before the first call stands as its line")
    (is (nle::tool-group-p (nle::display-block-row (third blocks))))
    (is (equal "Read 2 files, Searched 'needle'"
               (string-trim " " (second (nle::display-block-plain-lines (third blocks))))))
    (is (search "Edited a.lisp" (fourth texts)) "an edit is a card of its own, titled")
    (is (not (rows-carry-p "(sh " texts)))
    (is (search "The answer." (sixth texts)) "the answer stands last")))

(deftest the-standard-viewport-folds-a-turns-calls-under-one-closed-run ()
  ;; The view's four columns and its fold, seen through the projection: the
  ;; prompt, one `Ran N commands' run holding every call and thought, and
  ;; the answer. Nothing raw of a call stands outside the run.
  (is-let* ((nle::*tool-detail* :standard)
            (blocks (nle:project-state (mixed-turn-state) :width 80))
            (texts (mapcar #'block-text blocks)))
    ((length blocks) = 3)
    ((notany (lambda (block) (nle::tool-group-p block.row)) blocks) null)
    ((rows-carry-p "(sh \"cat a.lisp\")" texts) null)
    ((rows-carry-p "Ran 4 commands" texts) is)
    ((rows-carry-p "Think" texts) null)))

(deftest the-pad-arm-takes-a-folded-run-as-an-unfilled-block ()
  ;; With a band row showing and no turn in flight, the viewport pass reads the
  ;; last line's owner to decide whether the band takes the block's closing
  ;; pad. That owner went to ROW-BLOCK-FILL as a row; when the transcript ends
  ;; in a folded run it is the TOOL-GROUP, and the TYPE-ERROR — printed
  ;; unbounded — would take the shell down.
  (with-frame-model (model 100 40 :session-id "group-pad" :tool-detail :developer)
    (nle::render-state model)
    ;; A turn whose transcript ends in a folded run: a prompt, then two
    ;; reads with nothing after them.
    (let ((st nil))
      (flet ((call (id form result)
               (setf st (nle::ingest-tool-call-fact st id "eval" (eval-args form) :turn-id "t1")
                     st (nle::ingest-tool-result-fact st id "eval" result :turn-id "t1"))))
        (setf st (nle::ingest-message-fact st "user" "go" :turn-id "t1"))
        (call "c1" "(sh \"cat a.lisp\")" "(a)")
        (call "c2" "(sh \"cat b.lisp\")" "(b)"))
      (nle::upsert-preparing-band-row st "eval" "{\"form\":\"(sh \\\"sleep 50\\\")\"}"
                                      :call-id "c9")
      (is (nle::state-background-rows st) "a band row shows")
      (setf model.state st))
    (let ((notices '()))
      (let ((nle::*system-notice-hook*
              (lambda (text error-p) (declare (ignore error-p)) (push text notices))))
        (nle::render-state model))
      (let ((owner (find-if (lambda (ref) (or (nle::transcript-row-p ref)
                                              (nle::tool-group-p ref)))
                            model.line-rows :from-end t)))
        (is (nle::tool-group-p owner) "the last owned line is the folded run's")
        (is (null notices) (format nil "the frame composes without a notice: ~s" notices))))))

(deftest a-caught-error-on-a-projection-memo-reports-bounded (with-temp-store ())
  ;; The other half of the same death: the TYPE-ERROR's report printed the
  ;; group in full, and the group's memo vector holds the DISPLAY-BLOCK whose
  ;; row is the group again. The program's handler (the debug log's, never a
  ;; notice) prints bounded, and the memo structs print their identity, never
  ;; their graph.
  (let* ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "c2-cyclic"))
         (program (nlt:make-program model))
         (log (make-string-output-stream))
         (*error-output* log)
         (notices '())
         (nle::*system-notice-hook*
           (lambda (text error-p) (push (cons text error-p) notices)))
         (row (tool-row "eval" "{\"form\":\"(sleep 50)\"}"
                        :tool-call-id "c1" :tool-status "running"))
         (group (nle::make-tool-group (list :group "c1")))
         (block (nle::make-display-block :retention :persisted-history :kind :tool-call
                                         :row group :lines (list "a" "b" "c")
                                         :plain-lines (list "a" "b" "c")
                                         :refs (list group group group))))
    (setf group.members (list row)
          group.block
          (vector (nle::make-lline :text "Waiting 50s") 80 nil nil nil block))
    (is (null (nle::row-block-fill group)) "a folded run is a tool block: on the canvas")
    (is (< (length (princ-to-string group)) 120) "a group prints its identity, not its memo")
    (is (< (length (princ-to-string block)) 120) "a block prints its identity, not its row")
    (funcall program.error-handler :event-loop
             (make-condition 'type-error :datum group
                                         :expected-type 'nle::transcript-row))
    (is (null notices) "a caught error is no notice")
    (let* ((logged (get-output-stream-string log))
           (text (subseq logged 0 (search "Backtrace" logged))))
      (is (< (length text) 400)
          (format nil "the report is bounded: ~d chars" (length text)))
      (is (search "TOOL-GROUP" text) "and names the value that failed"))))

(deftest a-group-updates-in-place-as-its-calls-land (let ((nle::*tool-detail* :developer)))
  (let* ((st (mixed-turn-state))
         (st (nle::ingest-tool-call-fact st "c9" "eval" (eval-args "(sh \"cat c.lisp\")")
                                         :turn-id "t2"))
         (st (nle::ingest-tool-call-fact st "c10" "eval" (eval-args "(sh \"cat d.lisp\")")
                                         :turn-id "t2"))
         (running (car (last (nle:project-state st :width 80)))))
    (is (search "Reading 2 files…" (block-text running)))
    (let* ((st (nle::ingest-tool-result-fact st "c9" "eval" "(c)" :turn-id "t2"))
           (st (nle::ingest-tool-result-fact st "c10" "eval" "(d)" :turn-id "t2"))
           (done (car (last (nle:project-state st :width 80)))))
      (is (search "Read 2 files" (block-text done)) "then in the past")
      (is (eq (nle::display-block-row running) done.row)))))

(deftest replay-folds-the-groups-the-live-stream-folds (with-temp-store ())
  (let ((nle::*tool-detail* :developer)
        (sid "groups-replay"))
    (seed-completed-turn sid "look" "Seen."
                         :tools (list (list "c1" "eval" (eval-args "(sh \"cat a.lisp\")") "(a)")
                                      (list "c2" "eval" (eval-args "(sh \"ls src\")") "x")))
    (let ((texts (mapcar #'block-text (nle:project-state (nle:fold-log :session-id sid)
                                                         :width 80))))
      (is (rows-carry-p "Read a.lisp, Listed src" texts)
          (format nil "the folded log projects the same one line: ~s" texts)))))

(deftest a-thought-folds-to-one-line-and-keeps-its-tail-while-it-arrives (let ((nle::*tool-detail* :developer)))
  (let ((settled (nle::make-transcript-row :kind :reasoning :hydrated t
                                           :thought-ms 12400 :reasoning-tokens 4100
                                           :text (format nil "a~%b~%c"))))
    (is (equal '("Thought for 12.4s · 4.1k tokens")
               (mapcar #'nle::lline-text (nle::row-llines settled 60))))
    (let ((nle::*expanded-tool-call* settled))
      (is (equal '("a" "b" "c") (mapcar #'nle::lline-text (nle::row-llines settled 60))))))
  (is (equal '("Thought")
             (mapcar #'nle::lline-text
                     (nle::row-llines (nle::make-transcript-row :kind :reasoning :hydrated t
                                                                :text "replayed")
                                      60))))
  (let* ((streaming (nle::make-transcript-row
                     :kind :reasoning
                     :text (format nil "first paragraph~%~%second paragraph that runs long enough to wrap across the sixty column window twice over for sure~%~%third paragraph, which also runs long enough to take two rows of a sixty column window")))
         (lines (nle::row-llines streaming 60)))
    (is (equal "Thinking…" (nle::lline-text (first lines))) "a thought arriving says so")
    (is (= nle::*thinking-tail-rows*
           (loop for line in (rest lines)
                 sum (length (nle::lline-fragments line 60)))))
    (is (notany (lambda (line) (search "first paragraph" line.text)) (rest lines)))
    (is (every #'nle::lline-shadow (rest lines)) "the tail keeps the shadow")))

(deftest the-thinking-clock-and-the-round-tokens-reach-the-row ()
  (let* ((st (nle::ingest-stream-body nil :reasoning "hmm"))
         (thought (nle::last-tail-row st)))
    (is (nle::transcript-row-started-at thought) "a streamed thought starts its clock")
    (is (null thought.thought-ms) "and runs while it arrives")
    (nle::ingest-stream-body st :text "answer")
    (is (integerp thought.thought-ms)))
  (let* ((st (nle::ingest-message-fact nil "assistant" "" :reasoning "why" :turn-id "t"))
         (st (nle::ingest-turn-usage st :input 10 :output 20 :reasoning 4100)))
    (is (= 4100 (nle::transcript-row-reasoning-tokens
                 (find :reasoning (nle::transcript-rows st) :key #'nle::transcript-row-kind))))
    (let* ((st (nle::ingest-tool-call-fact st "c1" "eval" (eval-args "(+ 1 2)") :turn-id "t"))
           (st (nle::ingest-turn-usage st :input 10 :output 20 :reasoning 900)))
      (is (= 4100 (nle::transcript-row-reasoning-tokens
                   (find :reasoning (nle::transcript-rows st) :key #'nle::transcript-row-kind)))))))

(deftest tool-header-keeps-raw-shape-until-the-arguments-say-something ()
  ;; Mid-stream arguments are partial JSON. A snippet titles itself from what
  ;; has arrived; arguments that name no snippet keep the bold name + muted
  ;; args shape.
  (is (equal "(+ running" (nle::lline-text (tool-header-line "eval" "{\"form\":\"(+"
                                                             :tool-status "running"))))
  (let ((header (tool-header-line "eval" "{\"form\":"
                                  :tool-status "running")))
    (is (eq :tool (span-tone-at header.spans 0)))
    (is (search "eval" header.text)))
  (is (search "eval" (nle::lline-text (tool-header-line "eval" "{\"other\":true}")))))

;;; --- regression: JSON-null assistant content through the fold ------------

(deftest fold-survives-null-assistant-content (with-temp-store ())
  ;; A tool-only assistant round logs message content as JSON null
  ;; (turn.lisp), which shasht reads back as the truthy keyword :NULL. The
  ;; materialized message-event used to pass it through an OR guard into the
  ;; string-typed row text slot, crashing the event loop with a TYPE-ERROR.
  (record-test-turn (make-test-session "null-content") "(+ 40 2)" :null)
  (let ((rows (nle::transcript-rows (nle:fold-log :session-id "null-content"))))
    (is (plusp (length rows)) "the fold produced rows")
    (is (loop for row in rows
              always (stringp row.text)))))

(deftest picker-dialog-selects-and-closes ()
  ;; The modal picker: Down moves, Enter selects the highlighted row and
  ;; closes, Esc closes without selecting, and the on-select notice lands
  ;; in the model's notice lane.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg"))
        (picked nil))
    (nle::open-picker-dialog
     model "Pick" (picker-rows '("a" :value :a) '("b" :value :b))
     (nlk:lambda-ignoring (model value)
       (setf picked value)
       "picked"))
    (is (nle::chat-model-dialog model) "dialog opens")
    (dialog-keys model :down :enter)
    (is (eq :b picked) "Enter selects the highlighted row")
    (is-shape model (.dialog null "select closes the dialog") (nle::status-said "picked" "the notice lands"))
    (nle::open-picker-dialog model "Empty" '()
                             (nlk:lambda-ignoring (model value)
                               (setf picked :should-not-fire)
                               nil))
    (nle::picker-dialog-key model :enter)
    (is (null model.dialog))
    (is (eq :b picked) "an empty picker selects nothing")))

(deftest picker-reseat-holds-the-seat-a-vanished-row-left ()
  ;; Deleting a row on a dialog threw the cursor home: the refresh re-seated
  ;; the selection on the first row whenever its value was gone. The seat
  ;; holds instead — the row that closes the gap takes the cursor, the last
  ;; seat clamps to the row that is there, and a value that merely moved
  ;; keeps the cursor on itself.
  (flet ((rows (&rest labels)
           (loop for label in labels
                 collect (nle::make-picker-row :label label :value label))))
    (let ((dialog (nle::make-picker-dialog :title "Rows"
                                           :rows (rows "one" "two" "three")
                                           :selection 1)))
      (setf dialog.rows (rows "one" "three"))
      (nle::picker-reseat-selection dialog "two")
      (is-shape dialog (.selection = 1) (picker-selected-value "three"))
      (setf dialog.rows (rows "zero" "one" "three"))
      (nle::picker-reseat-selection dialog "three")
      (is (= 2 dialog.selection))
      (setf dialog.rows (rows "zero"))
      (nle::picker-reseat-selection dialog "gone")
      (is (= 0 dialog.selection) "the seat clamps to the list")
      (is (equal "zero" (picker-selected-value dialog)) "onto the row that is there"))))

(deftest picker-on-arrow-runs-before-the-detail-gesture ()
  ;; :ON-ARROW takes the two arrow keys the selection leaves, and only a
  ;; consumed answer ends the ladder: a declined Right still reaches
  ;; ON-DETAIL, a consumed Left never does.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "arrow-dlg"))
        (arrows '())
        (detailed '()))
    (nle::open-picker-dialog
     model "Arrows" (picker-rows '("one" :value "one") '("two" :value "two"))
     (constantly nil)
     :on-detail (nlk:lambda-ignoring (model value)
                  (push value detailed)
                  nil)
     :on-arrow (nlk:lambda-ignoring (model dialog direction)
                 (push direction arrows)
                 (eq direction :left)))
    (nle::picker-dialog-key model :left)
    (is (equal '(:left) arrows) "Left reaches the fold seam")
    (is (null detailed) "a consumed Left never reaches the detail gesture")
    (nle::picker-dialog-key model :right)
    (is (equal '(:right :left) arrows) "Right consults the fold first too")
    (is (equal '("one") detailed) "a declined Right falls through to ON-DETAIL")))

(deftest left-and-right-walk-a-pickers-tab-strip ()
  ;; In /models, /sessions and every tabbed menu, Up and Down move within
  ;; the list; Left and Right walk the strip as Tab does, stopping at its
  ;; ends, the cursor coming home with each list; a fold that takes the
  ;; arrow keeps it, and one it declines walks the strip.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "arrow-tabs"))
        (fold nil))
    (nle::open-picker-dialog
     model "Tabs" (picker-rows '("a1" :key "a" :value "a1") '("a2" :key "a" :value "a2")
                               '("b1" :key "b" :value "b1"))
     (constantly nil)
     :on-arrow (nlk:lambda-ignoring (model dialog direction) (and fold (eq direction fold))))
    (let ((dialog model.dialog))
      (nle::register-picker-tabs dialog (list (list :label "A" :value "a") (list :label "B" :value "b")))
      (dialog-keys model :down)
      (is (equal "a2" (picker-selected-value dialog)))
      (dialog-keys model :right)
      (is (= 1 (nle::picker-dialog-tab dialog)) "Right walks to the next tab")
      (is (equal "b1" (picker-selected-value dialog)) "the cursor comes home with the list")
      (dialog-keys model :right)
      (is (= 1 (nle::picker-dialog-tab dialog)) "Right stops at the last tab")
      (dialog-keys model :left)
      (is (= 0 (nle::picker-dialog-tab dialog)) "Left walks back")
      (dialog-keys model :left)
      (is (= 0 (nle::picker-dialog-tab dialog)) "Left stops at the first tab")
      (setf fold :right)
      (dialog-keys model :right)
      (is (= 0 (nle::picker-dialog-tab dialog)) "a fold that takes Right keeps it")
      (setf fold nil)
      (nle::picker-dialog-key model :right :ctrl t)
      (is (= 0 (nle::picker-dialog-tab dialog)) "Ctrl+Right is no strip key")
      (dialog-keys model :right)
      (is (= 1 (nle::picker-dialog-tab dialog)) "a declined fold hands its arrow to the strip")
      (is (eq dialog model.dialog) "and the dialog stays open throughout"))))

(deftest dialog-panel-renders-the-zig-docked-chrome ()
  ;; The Zig docked panel (dialog.zig computeDockedDialog + dialogs.zig
  ;; renderDockedPanel): one padding row, a four-row header whose tab row
  ;; sits at column 3 and whose search/summary row sits at column 4, the
  ;; list body, and a three-row footer carrying its hint on the middle row.
  (with-frame-model (model 80 40 :session-id "dlg-band")
    ;; Tall enough that the rows, not the window, are what the panel holds.
    (is (null (nle::dialog-panel-rows model 60 nil)))
    (nle::open-picker-dialog
     model "Session tree"
     (picker-rows '("one" :detail "d1") '("two" :detail "d2"))
     (lambda (model value) (declare (ignore model value)) nil)
     :context "Session history")
    (let* ((height (nle::docked-dialog-frame-rows model))
           (rows (nle::dialog-panel-rows model 60 nil)))
      (is (= (+ 1 4 2 3 1) height) "the panel fits its two rows")
      (is (= (1- height) (length rows)))
      (is (every (lambda (row) (= 60 (nle::text-display-width row))) rows))
      (is (string= "" (string-trim " " (first rows))))
      ;; Tab row: the label starts at column 3 + the tab's own leading space.
      (is (eql 4 (search "Session tree" (second rows))))
      (is (search "Session history" (second rows)))
      (is (string= "" (string-trim " " (third rows))))
      ;; Body rows start after the four header rows plus the padding row.
      (let ((first-entry (nth 5 rows)))
        (is (eql 6 (search "one" first-entry)))
        (is (search "d1" first-entry) "the detail follows the label"))
      (is (search "esc close  up/down navigate"
                  (nth (- (length rows) 2) rows)))
      (is (string= "" (string-trim " " (car (last rows))))))))

(deftest docked-panel-follows-its-rows-and-holds-under-a-query ()
  ;; The panel is its content under the cap: a full-height panel leaves a
  ;; short list blank rows the transcript above could have had.
  ;; Rows that arrive grow it and rows that leave -- a fold closing -- give
  ;; the height back; a query that narrows the list leaves the panel where it
  ;; stands, so the search row does not move under the typing; a long list
  ;; stops at the cap.
  (with-frame-model (model 80 40 :session-id "dlg-fit")
    (flet ((panel () (nle::docked-dialog-frame-rows model)))
      (nle::open-picker-dialog model "Sessions" (picker-rows "alpha" "beta" "gamma") nil
                               :searchable t)
      (is (= (+ 1 4 3 3 1) (panel)) "the panel fits its three rows")
      (let ((rows (nle::dialog-panel-rows model 60 nil)))
        (is (= (1- (panel)) (length rows)) "and paints all but its canvas gap row")
        (is (search "gamma" (nth 7 rows)) "the last row is the body's last")
        (is (search "esc close" (nth 9 rows)) "the footer's hint right under it"))
      (dialog-keys model #\a #\l #\p)
      (is (equal '("alpha") (visible-labels model.dialog)))
      (is (= (+ 1 4 3 3 1) (panel)) "a narrowing query leaves the panel as tall")
      (dialog-keys model :backspace :backspace :backspace)
      (setf (nle::picker-dialog-rows model.dialog)
            (picker-rows "alpha" "beta" "gamma" "delta" "epsilon"))
      (is (= (+ 1 4 5 3 1) (panel)) "rows that arrive grow it")
      (dialog-keys model #\e #\p)
      (is (equal '("epsilon") (visible-labels model.dialog)))
      (is (= (+ 1 4 5 3 1) (panel)) "held at the height the query began at")
      (dialog-keys model :backspace :backspace)
      (setf (nle::picker-dialog-rows model.dialog) (picker-rows "alpha" "beta"))
      (is (= (+ 1 4 2 3 1) (panel)) "rows that leave give the height back")
      (setf (nle::picker-dialog-rows model.dialog)
            (apply #'picker-rows (loop for i below 40 collect (format nil "s~d" i))))
      (is (= nle::*dialog-frame-max-height* (panel)) "a long list stops at the cap"))
    ;; Empty, the body is the one row its label takes.
    (nle::open-picker-dialog model "Sessions" '() nil)
    (is (= (+ 1 4 1 3 1) (nle::docked-dialog-frame-rows model)))
    (is (search "No items" (nth 5 (nle::dialog-panel-rows model 60 nil))))))

(deftest dialog-panel-paints-its-picture-above-the-list ()
  ;; LIST-DIALOG's :PICTURE -- the link's QR code -- paints above the rows,
  ;; two pixel rows to a line in half blocks: the paper lit on a panel whose
  ;; text is lighter than its ground, the ink on one whose text is darker, so
  ;; a camera reads it on either. The panel grows past the list's cap to hold
  ;; it whole, the rows a pointer picks start under it, and a window too short
  ;; for it says so in its place.
  (let ((pixels '("1100" "1010" "0110")))
    (is (equal '(" ▄▀█" "▀  ▀") (nle::picture-lines pixels t)) "paper lit, for a dark panel")
    (is (equal '("█▀▄ " " ▀▀ ") (nle::picture-lines pixels nil)) "ink lit, for a light one"))
  (with-frame-model (model 80 60 :session-id "dlg-picture")
    (nle::open-picker-dialog model "Link" (picker-rows "alpha" "beta")
                             (lambda (model value) (declare (ignore model value)) nil))
    (nle::register-picker-picture model.dialog (list :rows (loop repeat 30 collect "10")
                                                     :label "The code"))
    (multiple-value-bind (height above) (nle::docked-dialog-frame-rows model)
      (let ((rows (nle::dialog-panel-rows model 60 nil)))
        ;; Padding, header, 15 picture lines and a blank, the two list
        ;; rows, footer, the gap.
        (is (= (+ 1 4 16 2 3 1) height) "grown past the cap of 20")
        (is (every (lambda (row) (eql 4 (search " █" row))) (subseq rows 5 20)) "the picture opens the body")
        (is (string= "" (string-trim " " (nth 20 rows))) "a blank under it")
        (is (eql 6 (search "alpha" (nth 21 rows))) "then the list")
        (multiple-value-bind (first band) (nle::dialog-body-band model model.dialog)
          (is (= (+ above 2 4 16) first) "a pointer picks rows from under the picture")
          (is (= 2 band)))))
    (setf model.term-height 24)
    (let ((rows (nle::dialog-panel-rows model 60 nil)))
      (is (search "The code: a larger window shows it" (nth 5 rows)) "too short: the label says so")
      (is (eql 6 (search "alpha" (nth 7 rows)))))))

(deftest dialog-picture-stands-beside-the-list-when-the-panel-is-wide ()
  ;; A picture marked :BESIDE stands left of the list where the panel holds
  ;; both -- /context's grid beside its parts -- and the body is as tall as
  ;; the taller of the two; the rows a pointer picks start at the body's top.
  ;; A narrower panel stands it over the list, as any picture.
  (with-frame-model (model 100 40 :session-id "dlg-beside")
    (nle::open-picker-dialog model "Context" (picker-rows "alpha" "beta")
                             (lambda (model value) (declare (ignore model value)) nil))
    (nle::register-picker-picture model.dialog
                                  (list :rows (loop repeat 10 collect (make-string 40 :initial-element #\0))
                                        :palette '("" "context-tools") :beside t :label "The grid"))
    (multiple-value-bind (height above) (nle::docked-dialog-frame-rows model)
      ;; Padding, header, the picture's five lines beside two rows, footer, gap.
      (is (= (+ 1 4 5 3 1) height) "the picture holds the body to its lines")
      (let ((rows (nle::dialog-panel-rows model 100 nil)))
        (is (eql 3 (search "█" (nth 5 rows))) "the picture paints in the lead lane")
        ;; The lane is 44 columns, the picture and its gap; the list opens at
        ;; column 48, here with the plain-text selection gutter before it.
        (is (eql 50 (search "alpha" (nth 5 rows))) "the list stands right of it, past the gap")
        (is (eql 50 (search "beta" (nth 6 rows))))
        (is (null (search "alpha" (nth 9 rows))) "the picture's last line stands alone"))
      (multiple-value-bind (first band) (nle::dialog-body-band model model.dialog)
        (is (= (+ above 2 4) first) "a pointer picks rows from the body's top")
        (is (= 5 band))))
    (setf model.term-width 80)
    (let ((rows (nle::dialog-panel-rows model 80 nil)))
      (is (null (search "alpha" (nth 5 rows))) "too narrow: the picture stands over the list")
      (is (find-if (lambda (row) (eql 6 (search "alpha" row))) rows)))))

(deftest picture-palette-paints-each-cell-in-its-tone ()
  ;; A palette picture's pixel is the digit of its tone: two pixel rows to a
  ;; line, each cell an upper half block inked with the top pixel's color
  ;; on the bottom one's, an empty tone the panel's own ground.
  (let* ((tools (nle::visual-style-fg (nle::style-for-tone :context-tools)))
         (lines (nle::picture-palette-segs '("01" "10") '("" "context-tools") '(9 9 9))))
    (is (= 1 (length lines)) "two pixel rows to a line")
    (let ((segs (first lines)))
      (is (= 2 (length segs)) "one run a pair of colors")
      (is (equal "▀" (nle::seg-text (first segs))))
      (is (equal '(9 9 9) (nle::visual-style-fg (nle::seg-tone (first segs)))) "an empty tone is the panel's ground")
      (is (equal tools (nle::visual-style-bg (nle::seg-tone (first segs)))))
      (is (equal tools (nle::visual-style-fg (nle::seg-tone (second segs))))))))

(deftest panel-row-keeps-a-whole-styles-own-ground (let ((nle::*color-profile* :truecolor)))
  ;; A tone sits on the panel surface; a style given whole -- a picture's
  ;; cell -- keeps the ground it names.
  (let ((row (nle::docked-panel-row
              (list (nle::make-seg :text "▀" :tone (nle::vstyle :fg '(1 2 3) :bg '(4 5 6)))
                    (nle::make-seg :text "x" :tone :muted))
              20 '(9 9 9))))
    (is (search "48;2;4;5;6" row) "the cell's own ground")
    (is (search "48;2;9;9;9" row) "the surface under the tone")))

(deftest docked-panel-keeps-a-canvas-separator-above-it (let ((nle::*color-profile* :truecolor)))
  ;; Zig's reserveTranscriptAboveDialog stops the transcript at
  ;; `dialog.area.y - 1`, so one canvas row always stands between the last
  ;; transcript line and the panel's own surface. Without that row the
  ;; panel background butts straight onto the transcript and the two
  ;; surfaces read as one block.
  (with-frame-model (model 80 24 :session-id "dlg-gap")
    ;; User blocks paint their pads, so a full band's last row carries a
    ;; background: the separator can only come from the reservation, not
    ;; from an empty band's canvas padding.
    (dotimes (i 40)
      (fold-model model (nle::ingest-message-fact "user" (format nil "line ~d" i))))
    (nle::open-picker-dialog model "Shortcuts" (picker-rows "one" "two") nil)
    (multiple-value-bind (panel above) (nle::docked-dialog-frame-rows model)
      (is panel "the dialog docks")
      (is-let* ((rows (nle::compose-frame model))
                (canvas (nle::blank-frame-row 80 (nle::canvas-fill-bg))))
        ((nth (1- above) rows) string= canvas) ((string/= canvas (nth above rows)) is)
        ((string/= canvas (nth (- above 2) rows)) is)))))

(deftest panel-row-right-lane-parks-a-fact-flush ()
  ;; The row painter's right lane: a fact parked at the rect's right edge
  ;; with a tab of fill before it, the row's own text taking the truncation
  ;; when the two would meet — the dropdown's hotkey column asked for it and
  ;; nothing else has to re-derive the row's geometry to get it.
  (let ((left (list (nle::make-seg :text "/sessions")))
        (right (list (nle::make-seg :text "Ctrl+S"))))
    (is-let* ((row (nle::docked-panel-row left 40 nil :right right)))
      ((nle::text-display-width row) = 40 "the row still fills its rect")
      ((search "Ctrl+S" row) is "the right lane paints") ((search "Ctrl+S" row) = 30)
      ((count #\Space row) = 25))
    ;; A text long enough to meet the lane gives up its own tail, never the
    ;; fact parked right of it.
    (let* ((long (list (nle::make-seg :text (make-string 80 :initial-element #\x))))
           (row (nle::docked-panel-row long 40 nil :right right)))
      (is (= 40 (nle::text-display-width row)) "the row still fills its rect")
      (is (= 30 (search "Ctrl+S" row)) "the parked fact survives the clip")
      (is (= 24 (count #\x row)) "and the text beside it is what truncates"))
    ;; No lane, no gap: an empty or absent lane spends no columns.
    (is (string= (nle::docked-panel-row left 40 nil)
                 (nle::docked-panel-row left 40 nil :right nil)))))

(deftest dialog-panel-selection-is-tone-not-a-bar ()
  ;; Zig marks the selected docked row by tone alone — the composer tone
  ;; against the system tone — with no marker glyph and no highlight bar.
  ;; A profile that paints nothing has no tone to spend, so there and only
  ;; there the row keeps the Zig option-row `> ` gutter.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-tone")))
    (nle::open-picker-dialog model "Sessions" (picker-rows "alpha" "beta") nil)
    (let ((nle::*color-profile* :truecolor))
      (let* ((surface (nle::docked-panel-surface-bg))
             (row (nle::make-picker-row :label "alpha"))
             (selected (nle::docked-panel-row
                        (nle::dialog-entry-segs row t) 60 surface))
             (unselected (nle::docked-panel-row
                          (nle::dialog-entry-segs row nil) 60 surface)))
        (is-carrying selected (:absent "> ") (:absent "▸" "and no marker glyph either"))
        (is (string/= selected unselected))
        (is (= (nle::text-display-width selected)
               (nle::text-display-width unselected)))))
    (nlk:bind ((nle::*color-profile* :none) (rows (nle::dialog-panel-rows model 60 nil)))
      (is (search "> alpha" (nth 5 rows)))
      (is (search "  beta" (nth 6 rows))))))

(deftest dialog-panel-carries-the-answer-demanding-header-shape ()
  ;; Zig's renderTitledDockedHeader takes its title tone and its third-row
  ;; tone as parameters, and the panels that demand an answer rather than a
  ;; pick use the :approval title with a three-row header (the sudo prompt's
  ;; DockedDialogOptions). The panel has to be able to say that, or every
  ;; such dialog has to reinvent the chrome.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-approval")))
    (nle::open-picker-dialog
     model "Sudo password required"
     (picker-rows "●●●●●●")
     nil
     :context "rm -rf build"
     :subtitle "[sudo] password for mike:"
     :title-tone :approval
     :summary-tone :system
     :header-rows 3
     :hint "enter submit  backspace delete  ctrl+u clear  esc cancel")
    (let ((rows (nle::dialog-panel-rows model 60 nil)))
      ;; 1 padding + 3 header + 1 body + 3 footer = 8 painted rows,
      ;; so the area spans 9 and the panel shrinks to the prompt.
      (is (= 9 (nle::docked-dialog-frame-rows model)))
      (is (= 8 (length rows)) "and it paints all but its canvas gap row")
      (is-table (needle index) (search needle (nth index rows))
        ("Sudo password required" 1 "the title still leads the tab row")
        ("rm -rf build" 1 "with the command as its muted context")
        ("[sudo] password for mike:" 3 "the prompt owns the header's third row")
        ("●●●●●●" 4 "the masked body follows it")
        ("enter submit" 6 "and the footer hint sits on the middle footer row")))
    ;; The panel owns its whole background, so a tone carrying a bg of its
    ;; own must still sit on the surface (Zig shortcutPickerStyle).
    (let* ((nle::*color-profile* :truecolor)
           (surface (nle::docked-panel-surface-bg))
           (approval-bg (nle::visual-style-bg
                         (nle::style-for-tone :approval)))
           (row (first (nle::docked-dialog-header-rows model.dialog 60 surface 3))))
      (is (not (equal surface approval-bg)))
      (is (not (search (nle::styled " " (nle::vstyle :bg approval-bg)) row))))))

(deftest dialog-panel-windows-and-scrolls-like-zig ()
  ;; computeDialogListViewport pulls the window just far enough to hold the
  ;; selection, and renderShortcutPickerScrollbar sizes the thumb from the
  ;; visible fraction.
  (is-table (call expected) (equal 'expected (multiple-value-list call))
    ;; The viewport answers (SELECTED OFFSET VISIBLE), the thumb (TOP HEIGHT).
    ((nle::dialog-list-viewport 8 6 0 4) (6 3 4)
     "the selection clamps into range and the window pulls down a body's worth to hold it")
    ((nle::dialog-list-viewport 8 1 4 3) (1 1 3)
     "a selection above the window pulls it back up to the selection itself")
    ((nle::dialog-list-viewport 0 9 3 5) (0 0 0) "an empty list shows nothing")
    ((nle:scrollbar-thumb 10 10 10 0 :round-fn #'ceiling :fits-nil t) (nil nil)
     "a list that fits draws no thumb")
    ((nle:scrollbar-thumb 10 10 40 0 :round-fn #'ceiling :fits-nil t) (0 3)
     "the thumb takes the visible fraction, rounded up, at the top while the window is")
    ((nle:scrollbar-thumb 10 10 40 30 :round-fn #'ceiling :fits-nil t) (7 3)
     "a window at the end puts the thumb flush against the track's last row")))

(deftest picker-detail-key-invokes-the-detail-handler ()
  ;; The Zig isDialogDetailKey gesture (Right / i / I) on dialogs that
  ;; carry an ON-DETAIL handler: the dialog closes first (the on-select
  ;; contract) so the handler can open a successor dialog.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-detail"))
        (detailed nil))
    (flet ((reopen ()
             (nle::open-picker-dialog
              model "Children"
              (picker-rows '("a" :value :child-a) '("b" :value :child-b))
              (nlk:lambda-ignoring (model value)
                (setf detailed (list :selected value))
                nil)
              :on-detail (nlk:lambda-ignoring (model value)
                           (setf detailed (list :detail value))
                           "opened detail"))))
      (reopen)
      (dialog-keys model :down :right)
      (is (equal '(:detail :child-b) detailed))
      (is-shape model (.dialog null) (nle::status-said "opened detail"))
      (reopen)
      (nle::picker-dialog-key model #\i)
      (is (equal '(:detail :child-a) detailed))
      (reopen)
      (nle::picker-dialog-key model #\I :shift t)
      (is (equal '(:detail :child-a) detailed))
      ;; No handler: Right/i stay consumed no-ops, exactly like before.
      (nle::open-picker-dialog model "Plain" (picker-rows '("x" :value :x)) nil)
      (nle::picker-dialog-key model :right)
      (is (nle::chat-model-dialog model)))))

(deftest picker-detail-key-yields-to-search-typing ()
  ;; Searchable dialogs keep printable keys for the filter: a bare i must
  ;; land in the query, not the detail handler. Right still details.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-detail-s"))
        (detailed nil))
    (nle::open-picker-dialog
     model "Searchable"
     (picker-rows '("item" :value :item))
     nil
     :searchable t
     :on-detail (nlk:lambda-ignoring (model value)
                  (setf detailed value)
                  nil))
    (nle::picker-dialog-key model #\i)
    (is (null detailed) "i filters instead of detailing")
    (is (equal "i" (nle::picker-dialog-search model.dialog)))
    (nle::picker-dialog-key model :right)
    (is (eq :item detailed) "Right still opens detail on searchable dialogs")))

(deftest picker-key-column-and-detail-hint-render ()
  ;; The Zig DialogListItem key/label/detail triple and the detail-key hint
  ;; on dialogs that carry an ON-DETAIL handler.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-key")))
    (nle::open-picker-dialog
     model "Details"
     (picker-rows '("none" :key "Run" :detail "No run is available for this child session.")
                  '("sess-1" :key "Session" :detail ""))
     nil)
    (is-carrying (first-entry (nth 5 (nle::dialog-panel-rows model 78 nil)))
      ("Run" "the key column renders") ("none" "the value renders after the key")
      "No run is available")
    (nle::open-picker-dialog
     model "Items"
     (picker-rows '("child" :value :child))
     nil
     :on-detail (lambda (model value) (declare (ignore model value)) nil))
    (let ((rows (nle::dialog-panel-rows model 78 nil)))
      (is (search "i details" (nth (- (length rows) 2) rows))))))

(deftest dialog-panel-empty-body-carries-the-empty-label ()
  ;; Zig prints empty_label in the BODY and leaves the footer's key copy
  ;; alone; the old band spent its hint row saying there was nothing to pick.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-empty")))
    (nle::open-picker-dialog model "Extensions" '() nil
                             :searchable t
                             :empty-label "No active extensions")
    (let ((rows (nle::dialog-panel-rows model 60 nil)))
      (is (search "No active extensions" (nth 5 rows)))
      (is (search "esc close  up/down navigate  type search"
                  (nth (- (length rows) 2) rows))))
    ;; A query that matches nothing keeps the same shape.
    (nle::open-picker-dialog model "Extensions"
                             (picker-rows "alpha")
                             nil
                             :searchable t
                             :empty-label "No active extensions")
    (nle::picker-dialog-key model #\z)
    (let ((rows (nle::dialog-panel-rows model 60 nil)))
      (is (search "No active extensions" (nth 5 rows)))
      (is (search "z" (fourth rows))))))

(deftest clipped-dialog-entry-wears-the-triple-dot ()
  ;; A dialog entry too long for its row used to stop mid-string flush
  ;; against the right edge and say nothing about it; it now wears a 1-width
  ;; gap then a triple dot, and a row still paints exactly its width.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-overflow")))
    (nle::open-picker-dialog
     model "Sessions"
     (picker-rows '("1U4GKD8-PN0SW-a-title-that-runs-past-the-docked-row-edge"
                    :detail "48536095619158037  /home/me/nodecode/nodecode/  9m ago  168 blocks")
                  '("short" :detail "wd"))
     nil)
    (let* ((rows (nle::dialog-panel-rows model 60 nil))
           (clipped (nth 5 rows)))
      (is (search " … " clipped))
      (is (not (search " … " (nth 6 rows))))
      (is (= 60 (nle::text-display-width clipped))))
    (is-table (expected text width)
      (equal expected (nle::segs-text (nle::clip-segs (list (nle::make-seg :text text)) width
                                                      :mark nle::*dialog-row-overflow-mark*)))
      ;; The mark's own geometry: one gap column, the one-column triple dot,
      ;; one pad column.
      ("abcd … " "abcdefgh" 7)
      ;; A width that cannot hold the mark cuts the entry bare rather than
      ;; spending its own text on marks that do not fit.
      ("a" "abcdef" 1))))

;;; --- the list dialog's mouse -------------------------------------------------
;;; The picker's scrollbar is painted like the transcript's gutter, so the two
;;; gestures the operator has there are owed here: the wheel moves the window
;;; under a stationary pointer (HANDLE-MOUSE-WHEEL) and a held button drags
;;; the thumb (HANDLE-MOUSE-PRESS).

(defun open-scrollable-picker (model count &aux (log (list nil)))
  "MODEL with a COUNT-row picker open, => (values DIALOG CONFIRMATIONS)."
  ;; CONFIRMATIONS is a cons whose car collects every value the dialog
  ;; confirms — the dialog outlives this call, so the log has to be a shared
  ;; cons.
  (nle::open-picker-dialog
   model "Rows"
   (loop for i below count
         collect (nle::make-picker-row
                  :label (format nil "row-~2,'0d" i)
                  :value (format nil "row-~2,'0d" i)))
   (lambda (m value) (declare (ignore m)) (push value (car log)) nil))
  (values model.dialog log))

(deftest dialog-scrollbar-drag-scrolls-the-list ()
  ;; A press on the bar used to be inert — it armed the selection anchor, and
  ;; the release confirmed the row under the bar. It now grabs the thumb, the
  ;; drag moves the window with the highlight riding its edge, and the release
  ;; ends the drag without confirming anything.
  (with-frame-model (model 100 40 :session-id "dialog-bar")
    (multiple-value-bind (dialog confirmed) (open-scrollable-picker model 60)
      (let ((bar (- model.term-width nle::*docked-panel-pad-right*))
            (first (nle::dialog-body-band model dialog)))
        (is (null model.scrollbar-drag))
        (nle::handle-mouse-press
         model (nlt:make-mouse-click-msg :x bar :y first :button :left))
        (is (equal '(:dialog . 0) model.scrollbar-drag))
        (mouse-update model #'nlt:make-mouse-motion-msg bar (+ first 6) :button :left)
        (is (plusp dialog.view-offset))
        (is (<= dialog.view-offset
                dialog.selection))
        (mouse-update model #'nlt:make-mouse-release-msg bar (+ first 6) :button :left)
        (is (null model.scrollbar-drag))
        (is (null (car confirmed)))))))

(deftest dialog-scrollbar-click-jumps-the-track ()
  ;; A press on the track outside the thumb jumps the window there and keeps
  ;; the grab, so the same held button goes on dragging (the transcript's
  ;; HANDLE-MOUSE-PRESS contract, on the picker's own bar).
  (with-frame-model (model 100 40 :session-id "dialog-track")
    (let ((dialog (open-scrollable-picker model 60))
          (bar (- model.term-width nle::*docked-panel-pad-right*)))
      (multiple-value-bind (first height) (nle::dialog-body-band model dialog)
        (mouse-update model #'nlt:make-mouse-click-msg bar (+ first (1- height)) :button :left)
        (is (plusp dialog.view-offset))
        (is (consp model.scrollbar-drag))
        (mouse-update model #'nlt:make-mouse-release-msg bar (+ first (1- height)) :button :left)
        (is (null model.scrollbar-drag))))))

(deftest dialog-row-click-still-confirms ()
  ;; The bar owns its own cell and nothing else: a click on a row confirms it,
  ;; which is what the anchor the press arms is for.
  (with-frame-model (model 100 40 :session-id "dialog-row-click")
    (let* ((confirmed (nth-value 1 (open-scrollable-picker model 2)))
           (first (nle::dialog-body-band model model.dialog)))
      (nle::handle-mouse-press
       model (nlt:make-mouse-click-msg :x 20 :y first :button :left))
      (is (null model.scrollbar-drag))
      (nle::handle-mouse-release
       model (nlt:make-mouse-release-msg :x 20 :y first :button :left))
      (is (equal '("row-00") (car confirmed))))))

(deftest the-transcript-above-a-dialog-answers-the-pointer ()
  ;; An open dialog keeps the keys and its own panel, never the frame above
  ;; it: a click on a card there opens the card, the dialog staying open and
  ;; confirming nothing; a prompt opens its checkpoints only while the
  ;; transcript is the attached session's, never a preview's.
  (with-frame-model (model 100 40 :session-id "dialog-transcript")
    (fold-model model
      (nle::ingest-message-fact "user" "restore me")
      (nle::ingest-tool-call-fact "c1" "probe" "{}")
      (nle::ingest-tool-result-fact "c1" "probe" "out"))
    (let ((confirmed (nth-value 1 (open-scrollable-picker model 2))))
      ;; The frame settles the band the panel leaves before a row is measured
      ;; in it, as every paint the operator clicks on has.
      (flet ((click-line (&rest keys)
               (nle::render-state model)
               (nle::compose-frame model)
               (click-at model 6 (mouse-y-for-line model (apply #'line-row-index model keys)))))
        (click-line :kind :tool-call)
        (is-shape model (.expanded-tool-call "c1") (.dialog is) (nle::status-said null))
        (is (null (car confirmed)))
        (setf model.transcript-preview (cons "another-session" model.state))
        (click-line :role "user")
        (is (null (nle::status-said model)) "a previewed prompt is not this session's to restore")
        (setf model.transcript-preview nil)
        (click-line :role "user")
        (is (search "gateway" (or (nle::status-said model) "")))))))

(defun ending-at (&optional (universal (encode-universal-time 5 37 16 16 9 2026 0)))
  "Epoch milliseconds of a terminal turn fact at UNIVERSAL —
2026-09-16T16:37:05Z by default. The minute the divider reads back is this
machine's zone, so every expectation is FORMAT-CLOCK-MINUTE of the same
instant."
  (* 1000 (+ nlk:+unix-epoch+ universal)))

(defun at-clock (control)
  "CONTROL with its ~A read as the minute ENDING-AT lands at on this machine's clock."
  (format nil control (nle::format-clock-minute (ending-at))))

(deftest finish-divider-carries-durable-facts ()
  ;; The Zig writeTurnFinishDividerText contract: every segment joins only
  ;; when its durable fact exists — the lane, the elapse, the outcome word, and
  ;; the wall clock the terminal fact landed at. The turn's counts never join
  ;; it: the header's right lane carries the session cost.
  (is-each (nle::turn-finish-divider-text)
    ("completed" "• completed" "no facts collapses to the bare outcome")
    ("cancelled" "• cancelled" "cancelled collapses the same way")
    ("completed" :provider "prov" :model "m" "• prov/m · completed"
     "the provider prefixes the model")
    ("completed" :provider "a6api-oai" :model "gpt-5.6-sol"
     :effort "high" "• a6api-oai/gpt-5.6-sol (high) · completed"
     "the round's frozen effort rides the identity")
    ("completed" :provider "unknown" :model "m" :effort "" "• m · completed"
     "an unknown provider and an empty effort each contribute nothing")
    ("completed" :provider "prov" :model "vendor/m" "• vendor/m · completed"
     "a provider already spelled inside a slashed model id never doubles")
    ("completed" :model "big-model" :elapsed-ms 12345 "• big-model · 12.3s completed"
     "the elapse sits before the outcome word, as the Zig line had it"))
  ;; Lane, elapse, outcome and clock join; the round's tokens do not.
  (is (equal (at-clock "• big-model · 12.3s completed (~a)")
             (nle::turn-finish-divider-text
              "completed" :model "big-model" :elapsed-ms 12345 :ended-at-ms (ending-at)
              :usage (list :input 1230 :output 42 :cached 18770))))
  (is-each (nle::format-turn-elapsed)
    (125000 "2m 05s" "minutes render in units, never as a clock")
    (665000 "11m 05s" "eleven minutes is not five past eleven")
    (7500000 "2h 05m" "hours render Nh MMm")))

(defun rested-divider (state outcome elapsed-ms)
  "The text of the finish-divider row STATE's turn rests into when it ends
OUTCOME at ENDING-AT, its durable start ELAPSED-MS before it — both logged
facts, so the elapse reads between them."
  (setf (gethash "turn_started_at_ms" state) (- (ending-at) elapsed-ms))
  (nle::transcript-row-text
   (find :finish-divider
         (nle::transcript-rows
          (nle::ingest-turn-terminal state outcome :ended-at-ms (ending-at)))
         :key #'nle::transcript-row-kind)))

(deftest cancelled-divider-rests-on-its-lane-and-clock ()
  ;; A turn cancelled mid-stream rests with the same shape a completed one
  ;; does — the lane, the elapse, the outcome word, the wall clock — and the
  ;; counts the organism estimated, because the provider never reported them,
  ;; stay on the bar's meter, `~' marked.
  (let* ((state (nle::ingest-turn-usage nil :input 1100 :output 71
                                            :model "qwen3.8-vllm"
                                            :provider "loco-al"
                                            :effort "medium"
                                            :estimated t)))
    (is (equal (at-clock "• loco-al/qwen3.8-vllm (medium) · 1.6s cancelled (~a)")
               (rested-divider state "cancelled" 1600)))
    (is (equal " · ↑~1.1k ↓~71" (nle::seg-text (nle::usage-meter-seg state))))
    ;; ...and the resting line carries no counts.
    (is (not (search "↑" (rested-divider state "cancelled" 1600)))))
  ;; One estimated round makes the turn's sum an estimate; a reported round
  ;; before it does not un-mark the sum.
  (let* ((state (nle::ingest-turn-usage nil :input 500 :output 20
                                            :model "m" :provider "p"))
         (state (nle::ingest-turn-usage state :input 600 :output 30
                                              :estimated t)))
    ;; The estimate marks the turn's whole sum...
    (is (equal " · ↑~1.1k ↓~50" (nle::seg-text (nle::usage-meter-seg state))))
    ;; ...while the divider names the lane and the outcome alone.
    (is (equal "• p/m · cancelled"
               (nle::turn-finish-divider-text
                "cancelled" :provider "p" :model "m"
                :usage (gethash "usage" state))))))

(deftest the-bar-carries-the-meter-the-resting-divider-drops ()
  ;; The bar's token meter is USAGE-METER-TEXT, the one spelling of a turn's
  ;; counts; the resting divider carries the lane, the elapse, the outcome and
  ;; the clock instead, so one number never sits in two lanes — the header's
  ;; right lane is the session's own bill.
  (let* ((state (nle::ingest-turn-usage nil :input 1200 :output 42
                                            :cached 1128 :cache-write 800
                                            :reasoning 30
                                            :model "m" :provider "p"))
         (divider (nle::turn-finish-divider-text
                   "completed" :provider "p" :model "m"
                   :usage (gethash "usage" state)
                   :elapsed-ms 24800 :ended-at-ms (ending-at))))
    (is (equal " · ↑1.2k c48.5% w800 ↓42 r30" (nle::seg-text (nle::usage-meter-seg state))))
    (is (equal (at-clock "• p/m · 24.8s completed (~a)") divider)))
  (let ((state (nle::ingest-turn-usage nil :input 992 :output 146
                                           :reasoning 146 :estimated t)))
    (is (equal " · ↑~992 ↓~146 r~146"
               (nle::seg-text (nle::usage-meter-seg state)))))
  (is (null (nle::usage-meter-seg (nle::ingest-turn-usage nil :model "m")))))

(deftest cache-share-keeps-its-resolution-at-the-top ()
  ;; The cache share is a value the operator watches climb, and the meter is
  ;; where it climbs: one decimal is resolution enough while the prefix is
  ;; still being written, but at the top the first decimal is the whole story
  ;; and the second is the news — 99.4 and 99.9 are different facts about a
  ;; prompt that has nearly stopped changing.
  (is-each (nle::format-cache-hit-percent)
    (1230 18770 "93.9%" "below 99 the meter reads one decimal")
    (106 9894 "98.9%" "rounded to the tenth it shows")
    (101 9899 "98.99%"
     "a value whose tenth rounds up to 99.0 takes the second decimal rather than print a `99' with one")
    (100 9900 "99.00%" "99 exactly is the first value that reads with two")
    (58 9942 "99.42%" "and the second decimal is the point of the rule above it")
    (0 1 "100.00%" "a fully cached prompt reads as the bound, at the same precision"))
  (is (equal " · ↑58 c99.42% ↓1"
             (nle::usage-meter-text (list :input 58 :output 1 :cached 9942)))))

(deftest finish-divider-says-what-the-stream-reported ()
  ;; The divider is the operator's only record of what the provider actually
  ;; did with the round. Each segment below exists because the SSE said so
  ;; and the line used to stay silent about it.
  (is-each (nle::turn-finish-divider-text)
    ("completed" :provider "prov" :model "asked" :response-model "served"
     "• prov/asked→served · completed"
     "a relay serving another model id shows as requested→served")
    ("completed" :provider "prov" :model "same" :response-model "same"
     "• prov/same · completed" "a stream echoing the requested id adds nothing")
    ("completed" :provider "prov" :model "" :response-model "served"
     "• prov/served · completed"
     "a stream-named model still carries the identity when nothing was asked")
    ("completed" :elapsed-ms 12345 :finish-reason "length" "• 12.3s completed (length)"
     "a truncated answer never rests as an unqualified completed")
    ("completed" :finish-reason "stop" "• completed" "a clean stop stays silent")
    ("completed" :finish-reason "tool_calls" "• completed" "so does the tool-call round's reason")
    ("completed" :usage (list :input 553 :output 2100 :cached 3840
                              :cache-write 512 :reasoning 1900)
     "• completed"
     "counts the wire reported rest on the bar's meter, never on the divider")))

(deftest finish-divider-folds-the-stream-facts-from-the-usage-slot ()
  ;; The whole path: one turn.usage fold carrying the stream's own metadata,
  ;; then the terminal that rests it into a row — the lane, the elapse, the
  ;; outcome word, the reason the stop was not clean, and the clock, with the
  ;; counts left to the bar.
  (let* ((state (nle::ingest-turn-usage nil :input 553 :output 2100
                                            :cached 3840 :cache-write 512
                                            :reasoning 1900
                                            :model "gpt-5.6-sol"
                                            :response-model "gpt-5.6-sol-2026-08-01"
                                            :provider "a6api-oai"
                                            :finish-reason "length"
                                            :effort "medium")))
    (is (equal (at-clock "• a6api-oai/gpt-5.6-sol→gpt-5.6-sol-2026-08-01 (medium) · 24.8s completed (~a) (length)")
               (rested-divider state "completed" 24800)))))

(deftest the-turn-meter-bills-the-whole-turn-not-its-last-round ()
  ;; A turn is a loop: every tool round bills its own prompt and its own
  ;; completion. The bar's meter read only the round that happened to land
  ;; last, so a turn that spent three rounds read as though it had spent one.
  (let ((state (nle::ingest-turn-usage nil :input 1000 :output 40 :cached 200
                                           :reasoning 30
                                           :model "gpt-5.6-sol"
                                           :provider "a6api-oai"
                                           :finish-reason "tool_calls")))
    (setf state (nle::ingest-turn-usage state :input 1200 :output 60
                                              :cached 400 :reasoning 50
                                              :finish-reason "tool_calls"))
    (setf state (nle::ingest-turn-usage state :input 1400 :output 100
                                              :cached 600 :reasoning 70
                                              :finish-reason "stop"))
    (is (= 3 (getf (gethash "usage" state) :rounds)))
    (is (equal " · ↑3.6k c25.0% ↓200 r150" (nle::seg-text (nle::usage-meter-seg state))))
    (is-carrying (divider (rested-divider state "completed" 24800))
      ("24.8s completed" "the resting line keeps the elapse")
      (:absent "↑" "and carries none of the counts") (:absent "round"))))

(deftest a-new-turn-never-inherits-the-last-turns-bill ()
  ;; The three slots are scoped on purpose. Only the turn accumulator is a
  ;; turn fact; the session's bill and the window's occupancy outlive it.
  (let ((state (nle::ingest-turn-usage nil :input 1000 :output 40 :cached 200
                                           :model "gpt-5.6-sol")))
    (setf state (nle::begin-turn-usage state))
    (is (null (gethash "usage" state)))
    (is (= 1000 (getf (gethash "session_usage" state) :input)))
    (is (= 1200 (nle::context-occupancy-tokens (gethash "context_usage" state))))
    (setf state (nle::ingest-turn-usage state :input 300 :output 5
                                              :model "gpt-5.6-sol"))
    (is (= 300 (getf (gethash "usage" state) :input)))
    (is (= 1300 (getf (gethash "session_usage" state) :input)))
    (is (= 300 (nle::context-occupancy-tokens (gethash "context_usage" state))))))

(deftest usage-accumulation-never-invents-a-zero ()
  ;; NIL means "the provider did not say". A silent round must not zero a
  ;; count an earlier round reported, and must not fabricate one either.
  (let ((state (nle::ingest-turn-usage nil :input 1000 :output 40
                                           :model "gpt-5.6-sol")))
    (setf state (nle::ingest-turn-usage state :output 12))
    (let ((usage (gethash "usage" state)))
      (is-shape usage (:input = 1000) (:output = 52 "and still adds the count it did report")
        (:cached null) (:model "gpt-5.6-sol")))))

(defparameter *busy-retry*
  (nlk:json-object "attempt" 2 "limit" 4 "delay_seconds" 10.0 "status" 503 "provider" "deepseek"
                   "reason" (format nil "Service is too busy.~%We advise users to temporarily ~
                                         switch to alternative services."))
  "The stream_reset retry of a provider's 503, its advice after its fact.")

(deftest retry-status-counts-down-to-its-deadline ()
  ;; A retrying provider must look different from a stalled one — the
  ;; retry lands in state and stream progress retires it. Its countdown is read
  ;; at the frame's clock. Without this, a countdown formatted once would still
  ;; read `in 10s' after the retry deadline.
  (let* ((second internal-time-units-per-second)
         (retry (nle::retry-status *busy-retry* 0))
         (state (nle::ingest-stream-reset nil :retry-status retry)))
    (is (eq retry (gethash "retry_status" state)))
    ;; Who answered, and what it said, whole: its first sentence alone dropped
    ;; the advice (the operator: output the error literally).
    (is (equal (list "retry 2 of 4 in 10s" "deepseek 503" (gethash "reason" *busy-retry*))
               (multiple-value-list (nle::retry-status-text retry 0))))
    (is (equal "deepseek 503: Service is too busy. We advise users to temporarily switch to alternative services."
               (nle::retry-said retry)))
    (is-each (nle::retry-status-text retry)
      ((* 6 second) "retry 2 of 4 in 4s" "six seconds on")
      ((round (* 10.6 second)) "retry 2 of 4 sent" "past the delay, the retry is out")
      (nil "retry 2 of 4" "a reader that does not redraw counts nothing"))
    (setf state (nle::ingest-stream-body state :text "resumed"))
    (is (null (gethash "retry_status" state)))))

(deftest live-bar-paints-a-retry-amber-with-the-providers-words-whole (let ((nle::*color-profile* :truecolor)))
  ;; The bar names the retry, who answered and the way out; the provider's
  ;; words stand under it, every one of them. Without this, the bar would
  ;; cut the advice mid-word and a failing provider would look like work.
  (with-frame-model (model 120 24 :session-id "live-retry" :in-flight t)
    (setf model.state (nle::ingest-stream-reset model.state :turn-id "t1"
                                                :retry-status (nle::retry-status *busy-retry*)))
    (let ((amber (format nil "38;2;~{~d~^;~}" (nle::visual-style-fg (nle::style-for-tone :tool))))
          (bar (nle::live-status-line model 120)))
      (is-carrying bar ("retry 2 of 4 in 10s") ("deepseek 503") ("/models to switch" "a 503 names the way out")
        (:absent "Service" "the words stand under the bar, not cut on it"))
      (is (search amber bar) "in the tool amber"))
    (flet ((words (width)
             (format nil "~{~a~^ ~}"
                     (mapcan #'nlk:split-words (nth-value 1 (nle::retry-reason-lines model width))))))
      (is-table (width) (equal (substitute #\Space #\Newline (gethash "reason" *busy-retry*))
                               (words width))
        (120 "whole at a wide window")
        (40 "and wrapped, never cut, at a narrow one")))
    ;; Under the bar, in the status block.
    (is (search "We advise" (format nil "~{~a~%~}" (second (nle::compose-status-block model 120)))))))

(deftest a-turn-retried-through-leaves-one-line-under-its-answer (with-temp-store ())
  ;; The countdown is live-only and leaves with the stream's next delta: a
  ;; turn that sat on 503s and then answered rested like any other. The retry
  ;; facts reach a shell, live and replayed, and the turn leaves one line on
  ;; them under its answer -- not on the finish divider, which rests hidden.
  (is (not (nlk:event-kind-replay-suppressed-p nlk::+kind-provider-retry+)))
  (ensure-durable-session "retry-rest")
  (with-durable-turn (turn "retry-rest" :input "q" :complete t)
    (dolist (delay '(10.0 11.3))
      (nlk:record-provider-retry
       turn :status 503 :delay-seconds delay :provider "deepseek"
       :detail "{\"error\":{\"message\":\"Service is too busy. Switch to alternative services.\"}}"))
    (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "answered")))
  (let* ((rows (nle::transcript-rows (nle::fold-log :session-id "retry-rest")))
         (line (find-if (lambda (row) (search "Waited" (nle::transcript-row-text row))) rows)))
    (is-present line "the turn leaves its retry line"
      (is (equal "• Waited 21.3s through 2 retries · deepseek 503: Service is too busy. Switch to alternative services."
                 (nle::transcript-row-text line)))
      (is (not (nle::redundant-finish-divider-p line)) "it shows where the divider hides")
      ;; Under the answer.
      (is (< (position "answered" rows :key #'nle::transcript-row-text :test #'equal)
             (position line rows))))))
(deftest context-eviction-folds-divider-and-dims (with-temp-store ())
  ;; S2: /evict must leave transcript evidence — the Zig context divider
  ;; rests at the boundary and the evicted region above it renders dim.
  (seed-completed-turn "evict-ui" "old question" "old answer")
  (seed-completed-turn "evict-ui" "new question" "new answer")
  (let* ((turns (nle::history-turns-from-events
                 (nle::collect-session-events "evict-ui")))
         (previous (nle::floor-boundary turns (nlk:retained-history-floor "evict-ui"))))
    (nle::persist-history-boundary "evict-ui" turns previous 1
                                   :reason "operator_requested"))
  (let* ((rows (nle::transcript-rows (nle::fold-log :session-id "evict-ui")))
         (divider-position (position :eviction-divider rows
                                     :key #'nle::transcript-row-kind)))
    (is-present divider-position "the eviction divider folds into the transcript"
      (is (plusp divider-position))
      (is (every #'nle::transcript-row-evicted-p
                 (remove-if-not #'nle::transcript-row-log-position
                                (subseq rows 0 divider-position))))
      (is (notany #'nle::transcript-row-evicted-p
                  (subseq rows (1+ divider-position)))))
    ;; The durable per-fact counts divider: the Zig
    ;; writeContextEvictionDividerText copy, byte for byte, from the
    ;; fact's fold counts (1 turn of 2 messages evicted, 2 retained).
    (is-present (counts-row
                 (find "• evicted 1 turn · 2 messages · 2 retained · recall: see log"
                       rows :key #'nle::transcript-row-text :test #'equal))
        "the eviction fact folds its own counts divider row"
        (is (eq :finish-divider (nle::transcript-row-kind counts-row))))))

(defun workspace-diff-row (document-json &rest args)
  "A tool-call row whose result metadata carries DOCUMENT-JSON under
workspace_diff — decoded through shasht, the exact replay path shape."
  (apply #'tool-row "eval" "{\"form\":\"(f)\"}"
         :metadata (shasht:read-json
                    (format nil "{\"workspace_diff\":~a}" document-json))
         args))

(deftest workspace-diff-metadata-renders-line-diffs ()
  ;; The workspace_diff document renders the edit_diff.zig line shape from
  ;; the shipped unified patch: file headers dropped, gutters numbered from
  ;; the hunk header (removed/context old-side, added new-side), removals
  ;; before additions, the disclosure header carrying "+N -M path" stats.
  (let ((row (workspace-diff-row
              "{\"files\":[{\"path\":\"a.txt\",\"added\":1,\"removed\":1,\"patch\":\"diff --git a/a.txt b/a.txt\\nindex 111..222 100644\\n--- a/a.txt\\n+++ b/a.txt\\n@@ -1,3 +1,3 @@\\n one\\n-two old\\n+two new\\n three\\n\"}]}")))
    (let* ((nle::*expanded-tool-call* (list :diff (nle::tool-row-expand-key row) "a.txt"))
           (lines (nle::workspace-diff-llines row)))
      (is-present lines "a workspace_diff fact produces a diff body"
        (is (equal '("   +1 -1 a.txt"
                     "1   one" "2 - two old" "2 + two new" "3   three")
                   (mapcar #'nle::lline-text lines)))
        (let ((context (second lines))
              (removed (third lines))
              (added (fourth lines)))
          (is-each (span-tone-at (nle::lline-spans (first lines)))
            (3 :diff-added "\" +1\" paints in the diff-added tone")
            (6 :diff-removed "\" -1\" paints in the diff-removed tone"))
          (is (and (eq :foreground (nle::lline-tone context))
                   (nle::lline-dim context)))
          (is (eq :diff-removed-line (nle::lline-fill removed)))
          (is (eq :diff-added-line (nle::lline-fill added)))
          (is-each (span-tone-at (nle::lline-spans removed))
            (0 :diff-removed "the removed gutter keeps the diff foreground")
            (8 :diff-removed-word "the changed word rides the removed emphasis wash")
            (5 nil "the shared word stays on the plain content style"))
          (is (eq :diff-added-word (span-tone-at (nle::lline-spans added) 8)))))))
  (is (null (nle::workspace-diff-llines
             (tool-row "eval" "{\"form\":\"1\"}")))))

(deftest workspace-diff-paths-start-on-the-content-rail ()
  ;; The counts right-align against the rail, so a path starts on it whatever
  ;; the digits: `+2 -1 ' set its path one column left of `+92 -0 ''s.
  (let ((headers (mapcar #'nle::lline-text
                         (nle::workspace-diff-llines
                          (workspace-diff-row
                           "{\"files\":[{\"path\":\"a.lisp\",\"added\":2,\"removed\":1},{\"path\":\"b.lisp\",\"added\":92,\"removed\":0},{\"path\":\"c.lisp\",\"added\":120,\"removed\":45}]}")))))
    (is (equal '("   +2 -1 a.lisp" "  +92 -0 b.lisp" "+120 -45 c.lisp") headers))
    (is (equal '(9 9 9) (mapcar (lambda (text) (1+ (position #\Space text :from-end t))) headers)) "every path starts on the rail")))

(deftest workspace-diff-hunk-elisions-binary-and-truncation ()
  ;; Later hunks separate with a "..." elision row under the blank number
  ;; column; binary entries keep the stats header alone; a producer-side
  ;; file truncation surfaces as the trailing elision row.
  (let* ((row (workspace-diff-row
               "{\"files\":[{\"path\":\"a.txt\",\"added\":2,\"removed\":2,\"patch\":\"--- a/a.txt\\n+++ b/a.txt\\n@@ -1,2 +1,2 @@\\n a\\n-b\\n+B\\n@@ -10,2 +10,2 @@\\n j\\n-k\\n+K\\n\\\\ No newline at end of file\\n\"},{\"path\":\"img.png\",\"added\":0,\"removed\":0,\"binary\":true}],\"truncated\":2}"))
         (nle::*expanded-tool-call* (list :diff (nle::tool-row-expand-key row) "a.txt")))
    (is (equal '("   +2 -2 a.txt"
                 " 1   a" " 2 - b" " 2 + B"
                 "     ..."
                 "10   j" "11 - k" "11 + K"
                 "   +0 -0 img.png (binary)"
                 "... +2 more files")
               (mapcar #'nle::lline-text (nle::workspace-diff-llines row))))))

(deftest tool-output-expands-under-its-own-accordion ()
  (let ((row (tool-row "t" nil
                       :text (format nil "~{line ~d~^~%~}"
                                     (list 1 2 3 4 5 6 7 8 9 10)))))
    (let ((collapsed (nle::tool-output-llines row 60)))
      (is (< (length collapsed) 10) "output collapses by default")
      (is (find :tool-omission collapsed :key #'nle::lline-role)))
    (let* ((nle::*expanded-tool-call* row)
           (expanded (nle::tool-output-llines row 60)))
      (is (= 10 (length expanded)) "the clicked card shows the full output"))))

(deftest eval-string-result-renders-its-contents ()
  ;; An eval result that is exactly one printed string literal projects
  ;; as the produced text: no escaped \" runs, no lone closing-quote row —
  ;; the synthetic height the printed READ syntax added to the viewport.
  (let* ((nle::*tool-detail* :developer)
         (texts (tool-output-texts
                 "eval" :text (format nil "\"[{\\\"a\\\":1}]~%\""))))
    (is-shape texts (length = 1 "the literal's contents are the whole body") (first "[{\"a\":1}]")))
  (is (equal "(\"a\" \"b\")"
             (first (tool-output-texts "eval" :text "(\"a\" \"b\")"))))
  (let ((texts (tool-output-texts
                "eval" :text (format nil "side effect~%\"value\""))))
    (is-shape texts (length = 2) (first "side effect")))
  (is (equal "\"quoted\"" (first (tool-output-texts "run" :text "\"quoted\"")))))

(deftest a-tool-card-leaves-the-models-notes-out ()
  ;; A fresh box's first turn read `[text: 1 line, 5 characters]' over 6765.
  (is (equal '("6765") (tool-output-texts "eval" :text (format nil "[text: 1 line, 5 characters]~%6765~%")))))

(deftest theme-selection-persists-round-trip ()
  (with-temp-file (nle::*theme-selection-path* :type "theme")
    (is (null (nle::saved-theme-selection)))
    (is (nle::save-theme-selection "Tungstene") "the save lands")
    (is (equal "Tungstene" (nle::saved-theme-selection)))))

(defun prompts-answer (&rest lanes-and-texts)
  "The gateway's prompt-history answer: one prompt per LANE TEXT pair, in order."
  (nlk:json-object "prompts" (coerce (loop for (lane text) on lanes-and-texts by #'cddr
                                           collect (nlk:json-object "lane" lane "text" text))
                                     'vector)))

(defun rest-history-query (model dialog)
  "Flush DIALOG's prompt-history query as the frame clock does once typing rests."
  (setf (getf (nle::prompt-history-search dialog) :at) 0)
  (nle::flush-prompt-history-search model dialog))

(defmacro with-prompts-answered (answer &body body)
  "Run BODY with the gateway call the history dialog reads through answered by ANSWER."
  `(with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &key raw)
                              ,answer)
     ,@body))

(defmacro with-staged-history ((read land answer &optional (reads (gensym "READS"))) &body body)
  "WITH-PROMPTS-ANSWERED over ANSWER with the dialog's off-loop read staged, not run: READ is
the thunk it queued, LAND the continuation that takes the thunk's answer, READS the count queued."
  `(let ((,read nil) (,land nil) (,reads 0))
     (with-stubbed-fdefinition (nle::off-loop (model thunk continuation)
                                 (incf ,reads) (setf ,read thunk ,land continuation))
       (with-prompts-answered ,answer ,@body))))

(deftest history-picker-lists-every-prompt-the-shell-submitted (with-chat-model (model "hist-op"))
  ;; With no prompt in the store (a fresh one), the picker falls back to the
  ;; shell's own record of typed prompts — the ring Up/Down recall walks,
  ;; which outlives the session a prompt was typed in — never to a transcript
  ;; window of the session that happens to be attached.
  (fold-model model
    (nle::ingest-message-fact "user" "a transcript message nobody typed here"))
  (nle::note-submitted-prompt model "first question")
  (nle::note-submitted-prompt model "second question")
  (is (null (nle::execute-slash-command model "history" "")))
  (is-present (dialog (nle::chat-model-dialog model)) "the history picker is open"
    ;; The tab row counts the prompts the dialog holds.
    (is (equal "History (2)" (nle::picker-dialog-title dialog)))
    (is (equal '("second question" "first question") (row-labels dialog)))
    (is (nle::picker-dialog-searchable dialog) "typing filters it"))
  ;; Enter hands the highlighted prompt back to the composer for editing.
  (nle::picker-dialog-key model :enter)
  (is-shape model (.dialog null "choosing closes the dialog") (composer-value "second question")))

(deftest history-opens-before-reading-and-keeps-the-typed-query (with-chat-model (model "async-history"))
  (let ((requests 0))
    (with-staged-history
        (read land (progn (incf requests)
                          (prompts-answer "discord" "first prompt" "tui" "second prompt")))
      (nle::open-prompt-history-dialog model)
      (is (= 0 requests) "opening never waits for HTTP or prepares its rows")
      (is read "the read was staged off the loop")
      (is-present (dialog model.dialog) "the loading dialog is already open"
        (is (eq :prompt-history (nle::picker-dialog-kind dialog)))
        (is (search "reading" (nle::picker-dialog-title dialog)) "the count is not a fake zero"))
      (nle::picker-dialog-key model #\s)
      (when read (funcall land model (funcall read)))
      (is-present (dialog model.dialog) "the result fills the same open dialog"
        (is (equal "s" (nle::picker-dialog-search dialog)) "the query typed while reading survives")
        (is (equal "History (2)" (nle::picker-dialog-title dialog)))
        (is (equal '("first prompt" "second prompt")
                   (mapcar #'nle::picker-row-label (getf (nle::prompt-history-read model) :rows))))))))

(deftest history-late-read-does-not-reopen-or-replace-a-dialog (with-chat-model (model "dismiss-history"))
  (with-staged-history (read land (prompts-answer "tui" "past prompt"))
    (nle::replace-composer-text model "unsent draft")
    (nle::open-prompt-history-dialog model)
    (nle:update model (nlt:make-key-press-msg :code #\r :mod nlt:+mod-ctrl+))
    (is (null model.dialog) "Ctrl+R closes during the read")
    (nle::open-help-dialog model)
    (let ((help model.dialog))
      (when read (funcall land model (funcall read)))
      (is (eq help model.dialog) "a late history answer cannot replace help")
      (is (equal "unsent draft" (composer-value model)) "the draft is untouched"))))

(deftest history-reopens-from-cache-and-keeps-the-highlight-on-refresh (with-chat-model (model "cached-history"))
  (let ((texts '("newer" "older")))
    (with-staged-history
        (read land (apply #'prompts-answer (loop for text in texts collect "tui" collect text)))
      (nle::open-prompt-history-dialog model)
      (when read (funcall land model (funcall read)))
      (nle:update model (nlt:make-key-press-msg :code #\r :mod nlt:+mod-ctrl+))
      (setf texts '("newest" "newer" "older"))
      (nle::open-prompt-history-dialog model)
      (is (equal '("newer" "older") (row-labels model.dialog)) "cached rows paint before refreshing")
      (nle::picker-dialog-key model :down)
      (when read (funcall land model (funcall read)))
      (is (equal "History (3)" (nle::picker-dialog-title model.dialog)))
      (nle::picker-dialog-key model :enter)
      (is (equal "older" (composer-value model)) "new rows never move the operator's chosen prompt"))))

(deftest history-reopening-during-read-reuses-one-request (with-chat-model (model "single-flight-history"))
  (with-staged-history (read land (prompts-answer "cron" "scheduled ask") reads)
    (nle::open-prompt-history-dialog model)
    (nle:update model (nlt:make-key-press-msg :code #\r :mod nlt:+mod-ctrl+))
    (nle::open-prompt-history-dialog model)
    (let ((latest model.dialog))
      (is (= 1 reads) "repeated gestures cannot stack background reads")
      (when read (funcall land model (funcall read)))
      (is (eq latest model.dialog) "the newest requesting dialog gets the answer")
      (is (equal '("scheduled ask") (row-labels latest))))))

(deftest history-failed-refresh-keeps-loaded-store-rows-and-names-the-error
    (with-chat-model (model "failed-history"))
  (let ((fail nil))
    (with-prompts-answered (if fail (error "history probe refused")
                               (prompts-answer "discord" "room ask"))
      (nle::open-prompt-history-dialog model)
      (nle:update model (nlt:make-key-press-msg :code #\r :mod nlt:+mod-ctrl+))
      (setf fail t)
      (nle::open-prompt-history-dialog model)
      (is (equal '("room ask") (row-labels model.dialog)) "a failed refresh cannot erase good history")
      (is (search "history probe refused" (nle::picker-dialog-subtitle model.dialog)))
      (is (search "previously loaded history" (nle::picker-dialog-subtitle model.dialog))))))

(deftest history-search-echoes-before-ranking-and-rejects-stale-queries
    (with-chat-model (model "search-history"))
  (with-prompts-answered (prompts-answer "tui" "alpha" "discord" "beta")
    (nle::open-prompt-history-dialog model))
  (let ((dialog model.dialog))
    (with-staged-history (read land nil reads)
      (nle::picker-search-mutate dialog "alpha")
      (is (= 0 reads) "typing never runs the scorer or spawns one read per letter")
      (is (equal "alpha" (nle::picker-dialog-search dialog)) "the query echoes immediately")
      (is (null (nle::picker-visible-rows dialog)) "old rows are not mislabeled as matches")
      (rest-history-query model dialog)
      (is (= 1 reads) "the rested query is read once")
      (nle::picker-search-mutate dialog "beta")
      (let ((answer (funcall read))) (funcall land model answer))
      (is (null (nle::picker-visible-rows dialog)) "alpha's late answer cannot populate beta")
      (rest-history-query model dialog)
      (funcall land model (funcall read))
      (is (equal '("beta") (row-labels dialog)))
      (nle::picker-search-mutate dialog "")
      (is (equal '("alpha" "beta") (row-labels dialog)) "clear restores all prompts at once")
      (is (null (nle::picker-dialog-on-tick dialog)) "idle search stops its frame clock")
      (nle::picker-search-mutate dialog "alpha")
      (rest-history-query model dialog)
      (nle:update model (nlt:make-key-press-msg :code #\r :mod nlt:+mod-ctrl+))
      (nle::open-help-dialog model)
      (let ((help model.dialog))
        (funcall land model (funcall read))
        (is (eq help model.dialog) "late ranking cannot reopen history over help")))))

(deftest history-search-keeps-canonical-fuzzy-ranking-and-verbatim-selection
    (with-chat-model (model "rank-history"))
  (with-prompts-answered
      (prompts-answer "tui" (format nil "alpha~%beta") "discord" "select-model" "tui" "sm")
    (nle::open-prompt-history-dialog model))
  (let* ((dialog model.dialog) (rows (copy-list (nle::picker-dialog-rows dialog)))
         (expected (nle::picker-visible-rows (nle::make-picker-dialog :rows rows :search "sm"))))
    (nle::picker-search-mutate dialog "sm")
    (rest-history-query model dialog)
    (is (equal (mapcar #'nle::picker-row-value expected)
               (mapcar #'nle::picker-row-value (nle::picker-visible-rows dialog))))
    (nle::picker-dialog-key model :enter)
    (is-shape model (composer-value "sm") (.dialog null))))

(deftest history-unchanged-refresh-keeps-active-search-results (with-chat-model (model "refresh-search"))
  (with-prompts-answered (prompts-answer "tui" "alpha" "discord" "beta")
    (nle::open-prompt-history-dialog model)
    (let* ((dialog model.dialog) (rows (getf (nle::prompt-history-read model) :rows)))
      (nle::picker-search-mutate dialog "alpha")
      (rest-history-query model dialog)
      (let ((answer (nle::read-prompt-history-rows model nil rows)))
        (is (eq rows (getf answer :rows)) "the worker reuses an unchanged read's list")
        (nle::install-prompt-history-rows model answer))
      (is (equal '("alpha") (row-labels dialog)) "refresh cannot blank already-ranked hits")
      (is (null (nle::picker-dialog-on-tick dialog)) "no redundant rank was scheduled")
      (nle::picker-search-mutate dialog "beta")
      (nle::install-prompt-history-rows model (nle::read-prompt-history-rows model nil rows))
      (is (equal "Searching prompts…" (nle::picker-dialog-empty-label dialog)) "refresh keeps the pending-query label")
      (rest-history-query model dialog)
      (is (equal '("beta") (row-labels dialog))))))

(deftest a-keybind-row-defined-live-reaches-the-keyboard ()
  ;; The live-image gate: a keybind row added to the action table in a
  ;; running image — a definition evaluated into every attached shell — has
  ;; to reach the keyboard at the next keystroke, never at the next launch.
  (with-frame-model (model 80 24 :session-id "live-keybind")
    (let ((nle::*keybind-actions*
            (cons '(:live-probe "A row added at run time" ("ctrl+t"))
                  nle::*keybind-actions*)))
      (is (not (nle::model-key-matches-p model :live-probe #\t nlt:+mod-ctrl+)))
      (nle::refresh-model-keymap model)
      (is (nle::model-key-matches-p model :live-probe #\t nlt:+mod-ctrl+))
      ;; One identity check per keystroke when nothing changed.
      (let ((keymap model.keymap)) (nle::refresh-model-keymap model) (is (eq keymap model.keymap))))))

(defun open-history-over-the-route (model &aux (nle::*gateway-operator-token* "token-history"))
  "Open MODEL's history dialog with its gateway call answered by GET
/api/gateway/prompts itself, over this test's store."
  (with-prompts-answered (nlk:decode-json
                          (third (nle::handle-gateway-prompts-http
                                  (list :request-method method :request-uri path
                                        :headers (nlk:json-object
                                                  "authorization" "Bearer token-history")))))
    (nle::open-prompt-history-dialog model)))

(deftest history-reads-every-lane-the-store-committed (with-chat-model (model "store-history"))
  ;; The composer ring only ever held shells' typed drafts. Every prompt that
  ;; reached a turn is in the store — whatever lane carried it — and that is
  ;; what "every prompt" means: the dialog asks the organism's gateway for the
  ;; store's record and each row says which lane its prompt arrived on.
  (nlk:admit-turn (ensure-durable-session "discord-1-t2-m3") "command-d1" "an ask from the room")
  (nlk:admit-turn (ensure-durable-session "telegram-1-t2-m3") "command-t1" "an ask from telegram")
  (nlk:admit-turn (ensure-durable-session "s-local") "command-l1" "a prompt typed here")
  (is (equal '(("tui" "a prompt typed here") ("telegram" "an ask from telegram")
               ("discord" "an ask from the room"))
             (nle::prompt-history)))
  (is (null (open-history-over-the-route model)))
  (is-present (dialog (nle::chat-model-dialog model)) "the dialog opened over the store"
    (is-shape dialog (nle::picker-dialog-title "History (3)")
      (row-labels '("a prompt typed here" "an ask from telegram" "an ask from the room"))
      (row-keys '("tui" "telegram" "discord")))))

(deftest the-store-spans-more-prompts-than-the-ring-holds (with-chat-model (model "store-vs-ring"))
  ;; The complaint this answers: a home's ring said 164 prompts while the
  ;; store held every one the operator ever sent, on every lane. The store
  ;; reader is the surface that can answer it, so the dialog prefers it.
  (nle::note-submitted-prompt model "typed into this shell")
  (loop for i from 1 to 4
        do (nlk:admit-turn (ensure-durable-session (format nil "discord-9-t9-m~d" i))
                           (format nil "command-s~d" i)
                           (format nil "room ask ~d" i)))
  (is (= 1 (length model.prompt-history)) "the ring holds one")
  (is (= 4 (length (nle::prompt-history))) "the store holds every lane")
  (open-history-over-the-route model)
  (is-present (dialog (nle::chat-model-dialog model)) "the dialog opened"
    (is (equal "History (4)" (nle::picker-dialog-title dialog)))))

(deftest a-checklist-repaint-invalidates-the-ranking ()
  ;; The ranking is memoized per (rows, query) — a thousand-row dialog must
  ;; not re-rank per paint — and the one handler that rewrites row labels in
  ;; place has to drop that memo, or the marks a checklist paints would never
  ;; reach a filtered list.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "memo-mark")))
    (nle::open-picker-dialog
     model "Marks"
     (picker-rows '("alpha" :value :a) '("beta" :value :b))
     nil
     :searchable t
     :on-toggle (constantly nil))
    (let ((dialog model.dialog))
      (is (equal '("[ ] alpha" "[ ] beta") (visible-labels dialog)))
      (nle::picker-dialog-key model #\l)
      (is (equal '("[ ] alpha") (visible-labels dialog)))
      (nle::picker-dialog-key model #\Space)
      (is (equal '("[x] alpha") (visible-labels dialog))))))

(deftest ctrl-r-browses-past-prompts-and-returns-the-pick ()
  (with-frame-model (model 100 30 :session-id "ctrl-r-history")
    (nle::note-submitted-prompt model "oldest prompt")
    (nle::note-submitted-prompt model (format nil "middle prompt~%with a second line"))
    (nle::note-submitted-prompt model "newest prompt")
    (press model #\r :mod nlt:+mod-ctrl+)
    (is-present (dialog (nle::chat-model-dialog model)) "Ctrl+R opens the picker"
      ;; Every prompt, newest first; a multi-line prompt is one row.
      (is (equal '("newest prompt" "middle prompt with a second line"
                   "oldest prompt")
                 (row-labels dialog)))
      ;; Typing narrows through the same matcher every dialog searches with.
      (dolist (ch (coerce "old" 'list)) (press model ch))
      (is (equal "old" (nle::picker-dialog-search dialog)) "the query landed")
      ;; The ranking runs once the query rests, off the loop.
      (rest-history-query model dialog)
      (is (equal '("oldest prompt") (visible-labels dialog)))
      (press model :enter)
      (is (null (nle::chat-model-dialog model)) "the pick closes the dialog")
      (is (equal "oldest prompt" (composer-value model)))
      ;; The whole text of a multi-line prompt comes back, never the row's
      ;; flattened one-line label.
      (press model #\r :mod nlt:+mod-ctrl+)
      (is (equal '("newest prompt" "middle prompt with a second line"
                   "oldest prompt")
                 (row-labels (nle::chat-model-dialog model))))
      (press model :down :enter)
      (is (equal (format nil "middle prompt~%with a second line")
                 (composer-value model)))
      ;; Ctrl+R on its own dialog is the toggle back — the chord that opened
      ;; the panel takes it away again — and it closes outright: a toggle
      ;; toggles, query typed or not.
      (press model #\r :mod nlt:+mod-ctrl+)
      (is-present (dialog (nle::chat-model-dialog model)) "Ctrl+R reopens it"
        (press model #\t)
        (is (equal "t" (nle::picker-dialog-search dialog)))
        (press model #\r :mod nlt:+mod-ctrl+)
        (is-shape model (nle::chat-model-dialog null)
          (composer-value (format nil "middle prompt~%with a second line")))))))

(deftest ctrl-s-toggles-the-sessions-picker
    (with-stubbed-fdefinition (nle::slash-sessions-op (model) "sessions: the op ran"))
  ;; The Zig open_session_launcher chord, keySpec('s', .{ .ctrl = true }):
  ;; Ctrl+S is the /sessions door on the keyboard: the chord runs the op --
  ;; its answer lands in the notice lane, the notice /sessions writes -- and
  ;; over the picker itself it is the toggle back, running the panel's own
  ;; cancel the way Esc's close does.
  (with-frame-model (model 100 30 :session-id "ctrl-s-sessions")
    (is (nle::model-key-matches-p model :sessions-dialog #\s nlt:+mod-ctrl+))
    (press model #\s :mod nlt:+mod-ctrl+)
    (is-shape model (.dialog null "the op's answer, and no picker of its own")
      (nle::status-said "sessions: the op ran"))
    ;; Over a sessions picker the chord closes it, on-cancel and all.
    (let ((cleared nil))
      (nle::open-picker-dialog model "Sessions"
                               (list (nle::make-picker-row :label "s-1" :value "s-1"))
                               nil
                               :kind :sessions :searchable t
                               :on-cancel (nlk:lambda-ignoring (model)
                                            (setf cleared t)))
      (press model #\s :mod nlt:+mod-ctrl+)
      (is (null model.dialog) "Ctrl+S again closes its own panel")
      (is cleared "and the panel's cancel ran, as the Esc close would")
      (is (equal "sessions: the op ran" (nle::status-said model))))
    ;; A picker of another kind keeps its keys: an open dialog claims every
    ;; key before the main bindings, and the toggle is gated on its kind.
    (nle::open-picker-dialog model "History"
                             (list (nle::make-picker-row :label "old" :value "old"))
                             nil
                             :kind :prompt-history :searchable t)
    (press model #\s :mod nlt:+mod-ctrl+)
    (is (eq :prompt-history
            (nle::picker-dialog-kind model.dialog)))))

(deftest markdown-links-underline-and-hide-urls ()
  (let* ((text "see [the docs](https://x.y) now")
         (spans (nle::inline-spans text 0))
         (link (find-if #'nle::mspan-underline spans)))
    (is-present link "the link text span exists"
      (is (equal "the docs" (subseq text (nle::mspan-start link)
                                    (nle::mspan-end link)))))
    (is (= 2 (count-if #'nle::mspan-hidden spans)))))

(defun first-link-span (text)
  "The first of TEXT's inline spans that carries a link, or NIL."
  (find-if #'nle::mspan-link (nle::inline-spans text 0)))

(deftest markdown-destinations-use-openable-uris (let ((nle::*workspace-directory* "/repo")))
  ;; The explicit local destination painted as a hyperlink but stayed
  ;; a raw path, so neither the mouse gate nor terminal OSC8 could open it.
  (is-table (source expected)
    (equal expected (let ((span (first-link-span source)))
                      (and span (nle::mspan-link span))))
    ("[Full paper-to-code audit]( /home/me/nodecode-harness-research-20261012/report.md)"
     "file:///home/me/nodecode-harness-research-20261012/report.md")
    ("[audit](report.md)" "file:///repo/report.md")
    ("[audit]( ../notes/report.md )" "file:///repo/../notes/report.md")
    ("[audit](<src/a b.md>)" "file:///repo/src/a%20b.md")
    ("[résumé](src/r%C3%A9sum%C3%A9%20notes.md)" "file:///repo/src/r%C3%A9sum%C3%A9%20notes.md")
    ("[audit](file:///tmp/a%20b.md)" "file:///tmp/a%20b.md")
    ("[ui](src/x.lisp:12)" "file:///repo/src/x.lisp#12")
    ("[ui](/tmp/x.lisp#L12)" "file:///tmp/x.lisp#L12")
    ("[hash](src/a%23b.md)" "file:///repo/src/a%23b.md")
    ("[docs]( https://example.com/a%20b )" "https://example.com/a%20b")
    ("[unsafe](javascript:alert)" nil)
    ("[mail](mailto:someone@example.com)" nil)
    ("[section](#heading)" nil)
    ("[empty]( )" nil)))

(defun link-glyphs (plain)
  "Every glyph of the painted row PLAIN that a link range covers, as (COLUMN TARGET)."
  (loop for (start end target) in (nle::plain-row-links plain)
        nconc (loop for index from start below end
                    collect (list (nle::text-display-width (subseq plain 0 index)) target))))

(defun is-link-painted-alike (source width &optional uri)
  "Paint SOURCE's first markdown line at WIDTH twice -- fresh, then a memo hit
-- under every color profile: each linked glyph names URI (each range its own
target when URI is NIL) to the row's link ranges, to the pointer and, where
colour paints, to the OSC8 cell."
  (dolist (profile '(:none :ansi256 :truecolor))
    (let* ((nle:*color-profile* profile)
           (line (first (nle::markdown-llines source)))
           (model (make-instance 'nle::gateway-chat-model :session-id "link-paint" :ws nil)))
      (dotimes (pass 2)
        (multiple-value-bind (painted plains) (nle::lline-render line width)
          (setf model.line-plain (coerce plains 'vector))
          (is (some #'nle::plain-row-links plains) "fresh paint and memo hits keep the link")
          (loop for plain in plains for y from 0
                for cells = (nlt:parse-styled-string (list (nth y painted)) width 1)
                do (loop for (x target) in (link-glyphs plain)
                         for want = (or uri target)
                         for osc8 = (nlt:cell-link (nlt:screen-buffer-ref cells x 0))
                         do (is (equal want target) "the row owns the canonical file URI")
                            (is (equal want (nle::link-at-position model y x)) "pointer names it")
                            (is (or (eq profile :none) (equal want osc8)) "OSC8 names it"))))))))

(deftest markdown-bom-destinations-keep-their-root ()
  (let* ((nle::*workspace-directory* "/repo")
         (bom (string (code-char #xfeff)))
         (path "/home/me/.nodecode/artifacts/task-state-20261012/proof.txt")
         (uri (concatenate 'string "file://" path)))
    ;; Each row is a destination and the link it must name: boundary BOM normalization.
    (is-table (destination expected)
      (equal expected (let ((span (first-link-span (format nil "[Proof log](~a)" destination))))
                        (and span (nle::mspan-link span))))
      ((concatenate 'string bom path) uri)
      ((format nil " ~a~a~a " bom path bom) uri)
      ((format nil "~a<~a~a~a>~a" bom bom path bom bom) uri)
      ((format nil "~afile:///tmp/a%20b.txt~a" bom bom) "file:///tmp/a%20b.txt")
      ((format nil "~ahttps://example.com/a%20b~a" bom bom) "https://example.com/a%20b")
      ((format nil "~ajavascript:alert~a" bom bom) nil)
      ((format nil "~amailto:a@example.com~a" bom bom) nil)
      ((format nil "~a#heading~a" bom bom) nil)
      (bom nil)
      ((format nil "~areport.md~a" bom bom) "file:///repo/report.md")
      ("%EF%BB%BFreport.md" "file:///repo/%EF%BB%BFreport.md")
      ("file:///tmp/%EF%BB%BFreport.md" "file:///tmp/%EF%BB%BFreport.md")
      ((format nil "src/a~ab.txt" bom) "file:///repo/src/a%EF%BB%BFb.txt"))
    ;; The exact raw destination, ASCII/Unicode labels, fresh/memo paint,
    ;; and every color profile must agree with pointer metadata and OSC8.
    (dolist (label '("Proof log" "Rapport résumé"))
      (is-link-painted-alike (format nil "[~a](~a~a)" label bom path) 80 uri))))

(deftest markdown-local-links-paint-and-click-the-same-target ()
  (dolist (label '("Full paper-to-code audit" "Rapport résumé"))
    (let* ((uri "file:///tmp/report%20notes.md")
           (source (format nil "[~a]( /tmp/report%20notes.md )" label)))
      (is-link-painted-alike source 70 uri)
      (with-frame-model (model 100 24 :session-id "md-local-click"
                         :paint ((nle::ingest-message-fact "assistant" source)))
        (let* ((index (row-index-of label (nle::chat-model-line-plain model) :required t))
               (plain (aref (nle::chat-model-line-plain model) index))
               (column (nle::text-display-width (subseq plain 0 (search label plain))))
               (opened nil))
          (with-stubbed-fdefinition (nle::open-target (target) (setf opened target) t)
            (click-at model (+ column 4) (mouse-y-for-line model index)))
          (is (equal uri opened) "the transcript press/release opens the normalized file"))))))

(deftest a-path-becomes-one-uri-and-comes-back-whole (let ((nle::*workspace-directory* "/repo")))
  ;; One currency for both kinds of target, so the span, the OSC-8 wrap the
  ;; terminal reads and the opener all speak about the same thing. A space
  ;; or a name in another script has to survive the round trip: both parsers
  ;; would otherwise stop halfway through the name.
  (is (equal "file:///repo/src/a%20b.lisp" (nle::file-uri "src/a b.lisp")))
  (is (equal "/repo/src/a b.lisp" (nle::file-uri-path (nle::file-uri "src/a b.lisp"))))
  (is (equal "/tmp/ünïcode.png" (nle::file-uri-path (nle::file-uri "/tmp/ünïcode.png"))))
  (is (equal "file:///tmp/x" (nle::file-uri "/tmp/x")) "an absolute path keeps its root")
  (is (null (nle::file-uri-path "https://example.com")) "a page is not a file")
  (is (equal "/repo/src/x.lisp" (nle::file-uri-path "file:///repo/src/x.lisp#12")) "a fragment is a place in the file, not its name")
  (is (nle::openable-target-p "https://example.com"))
  (is (nle::openable-target-p "file:///tmp/x"))
  (is (not (nle::openable-target-p "mailto:someone@example.com"))))

(deftest only-a-path-shape-becomes-a-target-in-prose ()
  ;; A fact names its own address and is taken at its word; prose only ever
  ;; looks like one. This rule is the whole difference, and it is narrow on
  ;; purpose: `and/or' in a sentence must never paint as a file.
  (is-table (text) (nle::path-shaped-p text)
    ("src/tui/ui.lisp") ("/tmp/shot.png") ("~/notes.md") ("./run.sh")
    ("../x/y.txt") ("src/tui/"))
  (is-table (text) (not (nle::path-shaped-p text))
    ("and/or") ("24/7") ("client/server") ("https://example.com/a.html")
    ("rm -rf /tmp") ("/") ("nodecode.evolved") ("src/*.lisp"))
  ;; Inside code marks the author already said this is a path, so a folder
  ;; named without a trailing slash counts — the middle rung between a fact
  ;; and bare prose. A date or a fraction is still neither.
  (is (nle::path-shaped-p "src/tui" :in-code t))
  (is (nle::path-shaped-p "worklog/plans" :in-code t))
  (is (not (nle::path-shaped-p "src/tui")) "and only inside them")
  (is (not (nle::path-shaped-p "24/7" :in-code t)))
  (is (not (nle::path-shaped-p "2026/09/16" :in-code t))))

(deftest inline-code-and-bare-paths-carry-what-a-click-opens
    (let ((nle::*workspace-directory* "/repo")))
  (is-table (text link) (equal link (nlk:when-let (span (first-link-span text)) span.link))
    ("see `src/tui/ui.lisp` for it" "file:///repo/src/tui/ui.lisp"
     "the inline-code run carries the file it names")
    ("the folder is `src/tui` now" "file:///repo/src/tui" "a folder in code marks carries one too")
    ("see `src/tui/ui.lisp:1269` there" "file:///repo/src/tui/ui.lisp#1269" "a cited line is a place in the file, never its name")
    ("`src/a.lisp:10-20`" "file:///repo/src/a.lisp#10" "a cited range opens its file")
    ("at /tmp/a.lisp:12:5." "file:///tmp/a.lisp#12" "a bare path's line and column too")
    ("either and/or works" nil)
    ("written to /tmp/shot.png." "file:///tmp/shot.png" "an anchored bare path carries its target")
    ("a 24/7 job" nil)
    ("open http://localhost:3001 now" "http://localhost:3001" "a URL is untouched by any of this"))
  (let ((span (first-link-span "written to /tmp/shot.png.")))
    (is (equal "/tmp/shot.png" (subseq "written to /tmp/shot.png." span.start span.end)))
    (is (null span.underline) "a path is never underlined at rest"))
  (let* ((text "open http://localhost:3001 now")
         (span (first-link-span text)))
    (is (equal "http://localhost:3001" (subseq text span.start span.end)))
    (is (nle::mspan-underline span))))

(deftest a-tool-title-spans-the-file-it-names ()
  ;; The classifier decided what the source IS (activity.lisp *ACTIVITY-FAMILIES*
  ;; :SOURCE); the title writes the words and spans the run it just wrote. No
  ;; path is ever read back out of a title.
  (let* ((nle::*workspace-directory* "/repo")
         (line (nle::title-phrases-lline
                (nle::activity-title-phrases
                 (nle::tool-call-activity "eval" "(sh \"sed -n '1,80p' src/x.lisp\")"))))
         (span (find-if #'nle::mspan-link line.spans)))
    (is (equal "Read src/x.lisp (1-80)" line.text) "the words are unchanged")
    (is-present span "the path is a span of its own"
      (is (equal "src/x.lisp" (subseq (nle::lline-text line)
                                      (nle::mspan-start span) (nle::mspan-end span))))
      (is (equal "file:///repo/src/x.lisp" (nle::mspan-link span)))))
  ;; A folded run counts instead of naming, so it promises no click it
  ;; cannot deliver.
  (is-let* ((line (nle::title-phrases-lline
                   (nle::activity-entries-phrases
                    '((:read "a.lisp" nil) (:read "b.lisp" nil) (:read "c.lisp" nil))))))
    (line.text "Read 3 files") ((find-if #'nle::mspan-link line.spans) null))
  ;; A pattern is not an address, whatever it looks like.
  (let ((line (nle::title-phrases-lline
               (nle::activity-entries-phrases '((:search "src/x" nil))))))
    (is (equal "Searched 'src/x'" line.text))
    (is (null (find-if #'nle::mspan-link line.spans)))))

(deftest the-pointer-marks-the-target-it-rests-on ()
  ;; A terminal has no cursor that changes shape, so the span answers the
  ;; pointer instead: it underlines while the pointer is on it.
  (let ((line (nle::make-lline :text "see /tmp/shot.png"
                               :spans (list (nle::mspan 4 17 :link "file:///tmp/shot.png")))))
    (flet ((underlined-p (hover &aux (nle::*hover-target* hover))
             (nle::seg-underline
              (find-if #'nle::seg-link (nle::fragment-segs line 0 17)))))
      (is (null (underlined-p nil)) "nothing marks it until the pointer is there")
      (is (underlined-p "file:///tmp/shot.png") "and the pointer marks it")
      (is (null (underlined-p "file:///tmp/other.png")) "only the one it rests on")))
  ;; And the render memo knows the pointer is one of its inputs — but only
  ;; for the line that paints the target it moved on or off of. Crossing a
  ;; path in one card must not repaint every other card's.
  (let ((carrier (nle::make-lline :text "/tmp/a.png"
                                  :spans (list (nle::mspan 0 10 :link "file:///tmp/a.png"))))
        (other (nle::make-lline :text "/tmp/b.png"
                                :spans (list (nle::mspan 0 10 :link "file:///tmp/b.png"))))
        (plain (nle::make-lline :text "nothing to click here")))
    (flet ((entry (line) (gethash line nle::*lline-render-cache*)))
      (let ((nle::*hover-target* nil))
        (nle::lline-render carrier 40)
        (nle::lline-render other 40)
        (nle::lline-render plain 40))
      (let ((nle::*hover-target* "file:///tmp/a.png")
            (plain-before (entry plain))
            (other-before (entry other)))
        (nle::lline-render plain 40)
        (is (eq plain-before (entry plain)))
        (nle::lline-render other 40)
        (is (eq other-before (entry other)))
        (nle::lline-render carrier 40)
        (is (equal "file:///tmp/a.png" (nle::mspan-link (svref (entry carrier) 3))))))))

(deftest the-render-memo-keeps-a-bounded-number-of-lines-narrow
    (with-saved-globals ((nle::*lline-render-cache-limit* 8)))
  ;; The memo would otherwise hold every line a transcript ever painted,
  ;; full-width rows at four bytes a character. Past its limit it starts over,
  ;; and it keeps rows narrow.
  (clrhash nle::*lline-render-cache*)
  (let ((lines (loop for i below 20 collect (nle::make-lline :text (format nil "line ~d" i)))))
    (dolist (line lines) (nle::lline-render line 40))
    (is (<= (hash-table-count nle::*lline-render-cache*) 9) "the memo starts over past its limit")
    (multiple-value-bind (painted plain) (nle::lline-render (first lines) 40)
      (is (every (lambda (row) (typep row 'simple-base-string)) plain) "a plain ASCII row is narrow")
      (is (every (lambda (row) (typep row 'simple-base-string)) painted) "and so is its paint")
      (is (search "line 0" (first plain)) "and says what it said"))))

(deftest a-line-lane-that-shrinks-lets-go-of-the-rows-past-its-end ()
  ;; Without this, a lane keeps painted rows of an idle shell alive past its
  ;; fill pointer.
  (let* ((lane (nle::fill-line-vector (vector) (make-list 900 :initial-element "old") 900 '()))
         (again (nle::fill-line-vector lane (list "a" "b") 2 (list "tail"))))
    (is (eq lane again) "a lane within reach of the need is reused")
    (is (equal '("a" "b" "tail") (coerce again 'list)))
    (is (loop for index from 3 below 900 never (aref again index)) "no slot past the end names an old row"))
  (let* ((lane (nle::fill-line-vector (vector) (make-list 5000 :initial-element "old") 5000 '()))
         (small (nle::fill-line-vector lane (list "a") 1 '())))
    (is (< (array-dimension small 0) 5000) "and a lane four times the need and more is replaced")))

(deftest a-full-render-keeps-render-memos-only-for-what-it-projects ()
  ;; Every row ever projected kept its lines, block and paint for the life of
  ;; the shell.
  (let ((kept (nle::make-transcript-row :kind :text :role "assistant" :text "kept"))
        (gone (nle::make-transcript-row :kind :text :role "assistant" :text "gone")))
    (dolist (row (list kept gone))
      (setf (nle::row-display-cache row) :block
            (nle::row-llines-cache row) :llines
            (gethash row nle::*row-md-cache*) :markdown
            (nle::row-activity-cache row) :activity))
    (nle::forget-unprojected-rows (list (nle::make-display-block :row kept)))
    (is (eq :block (nle::row-display-cache kept)) "a projected row keeps its memos")
    (is (null (nle::row-display-cache gone)) "an unprojected row loses its block")
    (is (null (nle::row-llines-cache gone)) "its lines")
    (is (null (gethash gone nle::*row-md-cache*)) "its markdown")
    (is (null (nle::row-activity-cache gone)) "and its activity")))

;; STATE after a tool call and its result landed: the round every card gate folds in.
(defun tool-round (state id tool args result &key turn-id error-p)
  (nle::ingest-tool-result-fact (nle::ingest-tool-call-fact state id tool args :turn-id turn-id)
                                id tool result :turn-id turn-id :error-p error-p))

(deftest mouse-click-opens-a-path-a-card-names ()
  ;; The gesture the operator reported trying: a click on the file a card
  ;; names opens it, and the card underneath does not toggle. The rest of
  ;; the row is still the card's.
  (with-frame-model (model 80 24 :session-id "mouse-path" :tool-detail :developer)
    (nle::render-state model)
    (let ((nle::*workspace-directory* "/repo"))
      (paint-model model
        ;; An edit, so the card is its own and not a member of a group:
        ;; the accordion key under test is the call's.
        (tool-round "c1" "eval" (eval-args "(sh \"sed -i 's/a/b/' src/x.lisp\")") "(ok)"))
      (let* ((index (row-index-of "src/x.lisp" model.line-plain
                                  :required t))
             (plain (aref model.line-plain index))
             (column (search "src/x.lisp" plain))
             (verb (search "Edited" plain)))
        (is column "the card names the file it changed")
        (is (equal "file:///repo/src/x.lisp"
                   (nle::link-at-position model index (1+ column))))
        (is (null (nle::link-at-position model index (1+ verb))))
        (is (equal "file:///repo/src/x.lisp"
                   (click-opens model "src/x.lisp")) "the click opened the file")
        (is (null model.expanded-tool-call))
        (click-at model (+ verb 4) (mouse-y-for-line model index))
        (is (equal "c1" model.expanded-tool-call))))))

(deftest a-click-on-a-path-that-is-gone-says-so-instead-of-nothing ()
  ;; Silence is what the operator already reported as a broken click, so
  ;; every refusal names itself: the file, the missing opener, the browser.
  (with-frame-model (model 80 24 :session-id "mouse-gone")
    (paint-model model
      (nle::ingest-message-fact "assistant" "saved to /tmp/nodecode-no-such-file.png"))
    (let* ((needle "/tmp/nodecode-no-such-file.png")
           (index (row-index-of needle model.line-plain :required t))
           (column (search needle (aref model.line-plain index))))
      (click-at model (+ column 4) (mouse-y-for-line model index))
      (is-table (needle) (search needle (or (nle::status-said model) ""))
        ("Not there any more") ("/tmp/nodecode-no-such-file.png"))))
  ;; The two other refusals keep their own words.
  (is (equal "No browser to open links here — drag across one to copy it"
             (nle::open-refusal-hint "https://example.com" :no-opener)))
  (is (equal "Nothing here opens files — drag across the path to copy it"
             (nle::open-refusal-hint "file:///tmp/x" :no-opener))))

(deftest mouse-click-opens-bare-url (with-frame-model (model 80 24 :session-id "mouse-url"))
  (paint-model model
    (nle::ingest-message-fact "assistant" "open http://localhost:3001 now"))
  (is (row-index-of "http://localhost:3001" model.line-plain) "the URL is in a transcript line")
  ;; Stubbed, so no browser starts. A FLET binds the name only lexically,
  ;; and the click reaches OPEN-TARGET from inside UPDATE.
  (is (equal "http://localhost:3001" (click-opens model "http://localhost:3001")))
  (is (null model.selection)))

;; The lines MARKDOWN-LLINES draws the FORMAT-control text CONTROL as, under KEYS.
(defun md-lines (control &rest keys)
  (apply #'nle::markdown-llines (format nil control) keys))

(defun line-texts (lines)
  (mapcar (lambda (line) (string-right-trim " " (nle::lline-text line))) lines))

(deftest markdown-tables-render-aligned ()
  (let ((lines (md-lines "| name | qty |~%|---|---|~%| apple | 3 |~%| pear | 12 |")))
    (is (= 3 (length lines)) "header on its band, then two rows: the band says header where a rule row stood")
    (is-shape (line-texts lines) (first "name  │ qty") (second "apple │ 3"))
    (is (nle::lline-bold (first lines)) "the header is bold")
    (is (nle::lline-ground (first lines)) "and stands on a band"))
  (let ((lines (nle::markdown-llines "just | a pipe in prose")))
    (is (= 1 (length lines)) "a lone pipe without a separator stays prose")))

(deftest markdown-wide-tables-transpose-to-records ()
  ;; A pipe table too wide for the padded grid transposes to key/value
  ;; records (Zig emitMarkdownTableKeyValueRow golden): labels align to
  ;; the widest header cell + 3, values word-wrap into the remainder with
  ;; hanging continuations, and a dim rule separates records.
  (let ((lines (md-lines "| name | description |~%|---|---|~%| alpha | first record |~%| beta | second record |"
                         :width 28)))
    ;; content width 24: grid total 1 + (5+3) + (13+3) = 25 overflows.
    (is (= 7 (length lines)))
    (is-shape (line-texts lines) (first " name         alpha") (second " description  first")
      (third "              record") (fifth " name         beta"))
    (let ((label-span (find-if #'nle::mspan-bold
                               (nle::lline-spans (first lines)))))
      (is-present label-span "the label carries a bold span"
        (is (equal "name" (subseq (nle::lline-text (first lines))
                                  (nle::mspan-start label-span)
                                  (nle::mspan-end label-span))))))
    (is (nle::lline-rule (fourth lines))))
  ;; The same table at a comfortable width keeps the padded grid.
  (let ((lines (md-lines "| name | description |~%|---|---|~%| alpha | first record |" :width 80)))
    (is (= 2 (length lines)) "header on its band + one row as a grid")
    (is (search "│" (nle::lline-text (first lines)))))
  ;; Without a width (reasoning bodies) the grid never transposes.
  (let ((lines (md-lines "| name | description |~%|---|---|~%| alpha | first record |")))
    (is (search "│" (nle::lline-text (first lines))))))

;;; --- input/chrome ladder gates ---------------------------------------------
;;; Help dialog on `?`, in-dialog type-to-filter, keybind resolution, OSC-8
;;; hyperlinks, and the mouse suite (hit-test, tool-card click, hover,
;;; drag-select).

(defun mouse-y-for-line (model index)
  "The 1-based mouse row over visible transcript visual line INDEX."
  (nlk:bind (((height _ _ top-pad) (nle::transcript-band-metrics model)))
    (let ((row (position index (loop for row below (- height top-pad)
                                    collect (nle::band-window-row-index model.band row)))))
      (if row (+ 4 row top-pad)
          (error "Transcript line ~d is outside the viewport" index)))))

(defun mouse-y-for-text (model needle)
  "MOUSE-Y-FOR-LINE over the first transcript visual line carrying NEEDLE."
  ;; Blocks own padding rows, so a body row is never at a fixed line index.
  (mouse-y-for-line model (row-index-of needle model.line-plain :required t)))

(defun line-row-index (model &key kind role)
  "The index of MODEL's first transcript line drawn by a transcript row of
KIND and ROLE (NIL for either matches any), or NIL."
  (position-if (lambda (ref)
                 (and (nle::transcript-row-p ref)
                      (or (null kind) (eq kind ref.kind))
                      (or (null role) (equal role ref.role))))
               model.line-rows))

(defun mouse-update (model constructor x y &rest keys)
  "UPDATE MODEL with the mouse message CONSTRUCTOR builds at cell X,Y from KEYS."
  (nle:update model (apply constructor :x x :y y keys)))

(defun click-at (model x y)
  "A left press on cell X,Y of MODEL's frame and its release, through UPDATE."
  (nle:update model (nlt:make-mouse-click-msg :x x :y y :button :left))
  (nle:update model (nlt:make-mouse-release-msg :x x :y y :button :left)))

(defun click-opens (model needle)
  "What a click on the transcript text NEEDLE hands OPEN-TARGET, stubbed so
nothing opens: the target, or NIL."
  (let ((index (row-index-of needle model.line-plain :required t)) (opened nil))
    (with-stubbed-fdefinition (nle::open-target (uri) (setf opened uri) t)
      (click-at model (+ 4 (search needle (aref model.line-plain index)))
                (mouse-y-for-line model index)))
    opened))

(deftest help-dialog-opens-on-question-mark (with-frame-model (model 80 24 :session-id "help-key"))
  (nle:update model (nlt:make-key-press-msg :code #\?))
  (is (nle::chat-model-dialog model))
  (is-let* ((labels (row-labels model.dialog)))
    ((find "/help" labels :test #'equal) is) ((find "Ctrl+O" labels :test #'equal) is))
  (is (nle::picker-dialog-searchable model.dialog))
  (setf model.dialog nil)
  (type-string model "hi" #\?)
  (is-shape model (.dialog null) (composer-value "hi?")))

(deftest picker-search-filters-and-enter-selects-the-filtered-row ()
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-search"))
        (picked nil))
    (nle::open-picker-dialog
     model "Searchable"
     (picker-rows '("alpha" :detail "one" :value :alpha)
                  '("beta" :detail "two" :value :beta)
                  '("alpha-beta" :detail "three" :value :both))
     (lambda (model value) (declare (ignore model)) (setf picked value) nil)
     :searchable t)
    (is (search "Type to search" (fourth (nle::dialog-panel-rows model 60 nil))))
    (nle::picker-dialog-key model #\b)
    (let ((dialog model.dialog))
      (is-shape dialog (.search "b") (visible-labels '("beta" "alpha-beta"))))
    (nle::picker-dialog-key model #\Q)
    (is (null (nle::picker-visible-rows model.dialog)))
    (nle::picker-dialog-key model :backspace)
    (is (equal "b" (nle::picker-dialog-search model.dialog)))
    (nle::picker-dialog-key model :enter)
    (is (eq :beta picked) "Enter selects from the filtered rows")
    (is (null model.dialog) "select closes")))

(deftest dialog-search-is-fuzzy-and-scores-how-well-it-answered ()
  ;; The matcher used to carry the fuzzy name over a substring-only test:
  ;; `gtw' did not answer `gateway', and the survivors came back in list
  ;; order with nothing saying which one the operator meant.
  (is (nle::search-score "BETA" '("beta")) "case-insensitive")
  (is (nle::search-score "gtw" '("gateway")))
  (is (null (nle::search-score "wtg" '("gateway"))))
  (is-table (query better worse) (> (nle::search-score query better) (nle::search-score query worse))
    ("sm" '("select-model") '("assume"))
    ("gpt5" '("gpt-5") '("grep-the-output-5"))
    ("gate" '("gate" "x" "x") '("x" "x" "gate"))
    ("gate" '("x" "x" "gate") '("go-and-terminate" "x" "x")))
  (is (eql (nle::search-score "gpt5" '("gpt-5"))
           (nle::search-score "gate" '("gate"))))
  (is (nle::search-score "al tw" '("alpha" "two")))
  (is (null (nle::search-score "al zz" '("alpha" "two"))))
  (is (eql 0 (nle::search-score "  " '("anything"))))
  ;; The tiers, top down: the whole field, its prefix, a word-start
  ;; substring, a substring anywhere, a scattered subsequence.
  (is-let* ((tiers (mapcar (lambda (field) (nle::search-score "gate" (list field)))
                           '("gate" "gateway" "the-gate-house" "delegated"
                             "go-and-terminate"))))
    ((sort (copy-list tiers) #'>) tiers) ((length (remove-duplicates tiers)) = 5)))

;; The values of the rows DIALOG is showing, in order (VISIBLE-LABELS' twin).
(defun visible-values (dialog)
  (mapcar #'nle::picker-row-value (nle::picker-visible-rows dialog)))

(deftest dialog-search-ranks-the-better-answer-first-and-holds-list-order ()
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-rank")))
    (nle::open-picker-dialog
     model "Rank"
     (picker-rows '("go-and-terminate" :value :scattered)
                  '("the-gate-house" :value :word-start)
                  '("gate" :value :whole)
                  '("delegated" :value :substring)
                  '("gateway" :value :prefix))
     nil :searchable t)
    (dialog-keys model #\g #\a #\t #\e)
    (let ((dialog model.dialog))
      (is (equal '(:whole :prefix :word-start :substring :scattered) (visible-values dialog)))
      ;; Equal standing keeps the dialog's own order: a curated list — a
      ;; session's recency, an alias ahead of the models it names — is
      ;; lifted by the ranking, never scrambled by it.
      (setf dialog.rows
            (picker-rows '("zeta/pinned-alias" :value :a)
                         '("zeta/z-0" :value :b)
                         '("zeta/z-1" :value :c)))
      (nle::picker-search-mutate dialog "zeta")
      (is (equal '(:a :b :c) (visible-values dialog))))))

(deftest keybinds-parse-resolve-and-override ()
  (macrolet ((matches (action code mod) `(nle::keybind-matches-p keymap ,action ,code ,mod)))
    (let ((chord (nle::parse-key-chord "ctrl+shift+c")))
      (is (nle::key-chord-p chord) "the chord parses")
      (is (and chord.ctrl chord.shift (eql #\c chord.code))))
    (is (eq :none (nle::parse-key-chord "none")) "none disables")
    (is (null (nle::parse-key-chord "ctrl+wat")) "unknown keys refuse")
    (is (eq :page-up (nle::key-chord-code (nle::parse-key-chord "pgup"))))
    ;; Default resolution and match semantics.
    (let ((keymap (nle::resolve-keybinds '())))
      (is (matches :toggle-viewport #\o nlt:+mod-ctrl+))
      (is (matches :help #\? 0) "? dispatches help")
      (is (matches :help #\? nlt:+mod-shift+))
      (is (not (matches :submit :enter nlt:+mod-shift+)))
      (is (matches :insert-newline :enter nlt:+mod-shift+))
      ;; The decoder reports the bytes a legacy terminal sends for Enter and
      ;; Tab (CR, HT) bare, as the chord table binds them: the C0 ctrl bit is
      ;; ASCII, not intent. A kitty-reported Ctrl+Enter keeps its ctrl bit
      ;; and matches no default.
      (flet ((decoded (action &rest bytes)
               (let ((msg (nlt:read-key (make-string-input-stream
                                         (map 'string #'code-char bytes)))))
                 (list (matches action (nlt:key-event-code msg) (nlt:key-event-mod msg))
                       (nlt:key-event-code msg)))))
        (is-table (in) (first in) ((decoded :submit 13)) ((decoded :accept-suggestion 9))
          ((decoded :submit-turn-queue 9)))
        (is (equal '(nil :enter) (decoded :submit 27 91 49 51 59 53 117))))
      (is (not (matches :toggle-viewport #\o 0)))
      (is (equal "Ctrl+Q / Ctrl+D" (nle::keybind-chords-label keymap :quit))))
    ;; Backspace is not a bare-C0 key: 0x08 and 0x7F are distinct bytes, so
    ;; its ctrl bit is a real gesture the dialogs read for delete-word.
    (let ((keymap (nle::resolve-keybinds '((:copy-selection . "ctrl+backspace")))))
      (is (matches :copy-selection :backspace nlt:+mod-ctrl+))
      (is (not (matches :copy-selection :backspace 0))))
    ;; An override replaces every default chord for its action.
    (let ((keymap (nle::resolve-keybinds '((:toggle-viewport . "ctrl+g")
                                           (:help . "none")))))
      (is (matches :toggle-viewport #\g nlt:+mod-ctrl+))
      (is (not (matches :toggle-viewport #\o nlt:+mod-ctrl+)))
      (is (not (matches :help #\? 0)))
      (is (equal "Unbound" (nle::keybind-chords-label keymap :help))))))

(deftest keybind-override-file-drives-the-update ()
  (with-temp-file (nle::*keybinds-path*
                   :contents "{\"toggle_viewport\": \"ctrl+g\"}")
    (with-frame-model (model 80 24 :session-id "kb-file")
      (press model #\g :mod nlt:+mod-ctrl+)
      (is (eq :developer model.tool-detail))
      (press model #\o :mod nlt:+mod-ctrl+)
      (is (eq :developer model.tool-detail)))))

(deftest ctrl-f-expands-every-dialog-to-the-window ()
  ;; Ctrl+F over an open dialog lifts the docked panel from the Zig docked
  ;; height — and from a dialog's own BODY-ROWS — to every row under the
  ;; shell header: one preference for every dialog, outliving the dialog
  ;; itself, read through the keymap like every other chord.
  (is (nle::keybind-matches-p (nle::resolve-keybinds '())
                              :toggle-dialog-fullscreen #\f nlt:+mod-ctrl+))
  (with-frame-model (model 80 40 :session-id "dlg-full")
    (flet ((ctrl-f ()
             (press model #\f :mod nlt:+mod-ctrl+))
           (hint-row (&aux (rows (nle::dialog-panel-rows model 80 nil)))
             (nth (- (length rows) 2) rows)))
      (ctrl-f)
      (is (null model.dialog-fullscreen))
      (nle::open-picker-dialog model "Sessions" (picker-rows "one" "two") nil)
      (is (= (+ 1 4 2 3 1) (nle::docked-dialog-frame-rows model)) "docked, the panel fits its rows")
      (is (search "up/down navigate  ctrl+f full screen" (hint-row)))
      (ctrl-f)
      (is (nle::chat-model-dialog-fullscreen model) "Ctrl+F flips the flag")
      (is (nle::chat-model-dialog model) "and the dialog stays open")
      (is (= 35 (nle::docked-dialog-frame-rows model)))
      (is (search "ctrl+f restore" (hint-row)))
      (is-values (panel above below) (nle::docked-dialog-frame-rows model)
        (above = 3) ((+ (1- panel) above below) = 40))
      (is-let* ((rows (nle::compose-frame model))
                (canvas (nle::blank-frame-row 80 (nle::canvas-fill-bg))))
        ((length rows) = 40 "the composed frame keeps the window's rows")
        ((nth 2 rows) string= canvas) ((search "Sessions" (nth 4 rows)) is))
      ;; The preference outlives the dialog: the next one, a one-row
      ;; prompt that would size itself to it, opens full as well.
      (nle::picker-dialog-key model :escape)
      (is (null model.dialog) "Esc still closes")
      (nle::open-picker-dialog
       model "Sudo password required"
       (picker-rows "●●●●●●")
       nil
       :header-rows 3
       :hint "enter submit  esc cancel")
      (is (= 35 (nle::docked-dialog-frame-rows model)))
      (is (search "enter submit  esc cancel  ctrl+f restore" (hint-row)))
      (ctrl-f)
      (is-shape model (.dialog-fullscreen null) (nle::docked-dialog-frame-rows = 9))
      ;; Key-hint footers get the same trailing pair.
      (nle::picker-dialog-key model :escape)
      (nle::open-picker-dialog model "Shortcuts" (picker-rows "one") nil
                               :footer-hints :help)
      (is (search " up/down  navigate   ctrl+f  full screen" (hint-row)) "a key-hint set's keys are caps")))
  ;; A short window: the reach is under the Zig docked height already, so
  ;; a list long enough for the cap docks as tall as fullscreen and nothing
  ;; overflows — and neither touches the header.
  (with-frame-model (model 80 24 :session-id "dlg-full-short")
    (nle::open-picker-dialog model "Sessions"
                             (apply #'picker-rows (loop for i below 30 collect (format nil "s~d" i)))
                             nil)
    (is (= 19 (nle::docked-dialog-frame-rows model)))
    (nle:update model (nlt:make-key-press-msg :code #\f :mod nlt:+mod-ctrl+))
    (is (= 19 (nle::docked-dialog-frame-rows model)))
    (is (= 24 (length (nle::compose-frame model))) "the frame stacks"))
  ;; The chord is the keymap's: an override moves it, and the help dialog
  ;; lists the action under its configured label.
  (with-temp-file (nle::*keybinds-path*
                   :contents "{\"toggle_dialog_fullscreen\": \"ctrl+g\"}")
    (with-frame-model (model 80 40 :session-id "dlg-full-kb")
      (nle::open-help-dialog model)
      (is (find-if (lambda (row)
                     (and (string= "Ctrl+G" (nle::picker-row-label row))
                          (string= "Expand dialog to full height"
                                   (nle::picker-row-detail row))))
                   (nle::picker-dialog-rows model.dialog)))
      (nle:update model (nlt:make-key-press-msg :code #\g :mod nlt:+mod-ctrl+))
      (is (nle::chat-model-dialog-fullscreen model))
      (nle:update model (nlt:make-key-press-msg :code #\f :mod nlt:+mod-ctrl+))
      (is (nle::chat-model-dialog-fullscreen model)))))

;;; --- words stay whole across wrapped rows ---------------------------------

(defun rows-carrying-word-fragments (rows source &aux (words (nlk:split-words source)))
  "ROWS whose tokens are not whole words of SOURCE, each as (TOKEN ROW)."
  (loop for row in rows
        nconc (loop for token in (nlk:split-words row)
                    unless (member token words :test #'string=)
                      collect (list token row))))

(deftest every-body-lane-wraps-words-whole ()
  ;; The word-whole rule at the display policy's own seam: prose, a fenced
  ;; code body, and a user block all break at spaces, so no lane leaves a
  ;; fragment of a word at a row edge — the code and tool lanes used to
  ;; hard-break at the column and divide the word across the rows.
  (let* ((prose "the wrapping rule keeps every single word whole across
the rows of a narrow viewport")
         (code "alpha beta gamma delta epsilon zeta eta theta iota kappa")
         (fence (format nil "```~%~a~%```" code))
         (user (nle::make-lline :text prose :tone :user-block
                                :fill :user-block)))
    (let ((plain (nth-value 1 (nle::llines-rows (nle::markdown-llines prose)
                                                28))))
      (is (plusp (length plain)) "prose wrapped to rows")
      (is (null (rows-carrying-word-fragments plain prose)))
      (is (every (lambda (row) (<= (nle::text-display-width row) 28)) plain)))
    (let ((plain (nth-value 1 (nle::llines-rows (nle::markdown-llines fence)
                                                28))))
      (is (> (length plain) 1) "the fence body wrapped to rows")
      ;; The ↦ its later rows open on and the slab's ▌ edge are the gutter's
      ;; marks, not fragments: the copy leaves them behind.
      (is (null (rows-carrying-word-fragments
                 (mapcar #'nle::without-gutter-marks plain)
                 (format nil "``` ~a ```" code)))))
    (is (null (rows-carrying-word-fragments (nth-value 1 (nle::llines-rows (list user) 28))
                                            prose)))))

(deftest osc8-hyperlinks-ride-painted-profiles ()
  (let ((line (first (nle::markdown-llines "see [docs](https://example.com/m)"))))
    (is-let* ((link-span (find-if #'nle::mspan-link line.spans)))
      (link-span is "the link span carries its url") (link-span.link "https://example.com/m"))
    (let ((nle:*color-profile* :ansi256))
      (is (search "]8;;https://example.com/m"
                  (first (nle::lline-rows line 60)))))
    (is (not (search "]8;;" (first (nle::lline-rows line 60)))))))

(deftest mouse-click-expands-the-tool-card (with-frame-model (model 80 24 :session-id "mouse-tool"))
  (paint-model model
    (tool-round "c1" "probe" "{}" (format nil "~{out ~d~^~%~}" (list 1 2 3 4 5 6 7 8 9))))
  (let ((index (line-row-index model :kind :tool-call)))
    (is index "the line table locates the tool card")
    (let ((y (mouse-y-for-line model index)))
      (mouse-update model #'nlt:make-mouse-click-msg 6 y :button :left)
      (is (nle::chat-model-pressed-tool-row model))
      (mouse-update model #'nlt:make-mouse-release-msg 6 y :button :left)
      (is-shape model (.expanded-tool-call "c1") (.tool-detail eq :standard)
        (.pressed-tool-row null))
      ;; A second click on the expanded card collapses it (the Zig
      ;; toggleExpandedToolCall accordion).
      (click-at model 6 y)
      (is (null model.expanded-tool-call)))))

(deftest a-click-opens-a-group-and-ctrl-o-toggles-the-level ()
  (with-frame-model (model 100 40 :session-id "group-click" :tool-detail :developer
                     :state (mixed-turn-state))
    (let* ((refs model.line-rows)
           (index (position-if #'nle::tool-group-p refs))
           (group (and index (aref refs index))))
      (is index "the line table names the group line by its group")
      (click-at model 6 (mouse-y-for-line model (1+ index)))
      (is (equal group.key model.expanded-tool-call))
      (is (find-if (lambda (ref) (and (nle::transcript-row-p ref) (equal "c1" ref.tool-call-id)))
                   model.line-rows))
      ;; The open group grew the transcript and the view followed its
      ;; tail: the group's line sits at a new row now.
      (click-at model 6 (mouse-y-for-line model (1+ index)))
      (is (null model.expanded-tool-call) "a second click folds it")))
  (with-frame-model (model 100 40 :session-id "detail-cycle")
    (flet ((ctrl-o ()
             (nle:update model (nlt:make-key-press-msg :code #\o :mod nlt:+mod-ctrl+))
             (list model.tool-detail
                   (nle::status-said model))))
      (is (equal '(:developer "Switched to Developer viewport") (ctrl-o)) "Standard, then Developer")
      (is (equal '(:standard "Switched to Standard viewport") (ctrl-o)) "and back: two modes, no third"))))

(deftest tool-card-click-leaves-sibling-cards-collapsed ()
  ;; Regression: the click used to flip the global TOOLS-EXPANDED flag,
  ;; so expanding one tool card expanded every card in the transcript.
  (with-frame-model (model 80 30 :session-id "mouse-accordion" :tool-detail :developer)
    (nle::render-state model)
    (let ((long-output (format nil "~{out ~d~^~%~}"
                                (list 1 2 3 4 5 6 7 8 9))))
      (paint-model model
        (tool-round "c1" "probe" "{}" long-output)
        (tool-round "c2" "probe" "{}" long-output))
      (let ((rows (remove-duplicates
                   (loop for ref across model.line-rows
                         when (and (nle::transcript-row-p ref) (eq :tool-call ref.kind))
                           collect ref))))
        (is (= 2 (length rows)) "both tool cards render")
        (click-at model 6 (mouse-y-for-line model (line-row-index model :kind :tool-call)))
        (is (equal "c1" model.expanded-tool-call))
        (let ((omissions
                (loop for row across (nle::chat-model-line-rows model)
                      for line across (nle::chat-model-line-plain model)
                      when (and (nle::transcript-row-p row)
                                (search "+" line)
                                (search "line" line))
                        collect (nle::transcript-row-tool-call-id row))))
          (is (not (member "c1" omissions :test #'equal)))
          (is (member "c2" omissions :test #'equal)))))))

(deftest mouse-hover-tracks-tool-cards (with-frame-model (model 80 24 :session-id "mouse-hover"))
  (paint-model model
    (nle::ingest-tool-call-fact "c1" "probe" "{}")
    (nle::ingest-message-fact "assistant" "plain prose"))
  (let* ((tool-index (line-row-index model :kind :tool-call))
         (prose-index (line-row-index model :kind :text)))
    (mouse-update model #'nlt:make-mouse-motion-msg 6 (mouse-y-for-line model tool-index))
    (is (eq (aref model.line-rows tool-index) model.hover-row))
    (mouse-update model #'nlt:make-mouse-motion-msg 6 (mouse-y-for-line model prose-index))
    (is (null model.hover-row))))

(defun frame-row-carrying (model needle &key (start 3))
  "The 1-based frame row NEEDLE paints on, skipping the shell chrome
(the header row carries the session title, which is the first prompt)."
  (1+ (row-index-of needle (nle::compose-frame model) :start start :required t)))

(deftest mouse-hit-box-covers-the-block-a-terminal-points-at ()
  ;; Regression: SGR mouse reports are already 1-based cells, but the
  ;; decoder added one to each axis, so every hit-test landed one row below
  ;; and one column right of the pointer. On a user prompt — a three-row
  ;; bubble — that meant the canvas row above it hovered and its own bottom
  ;; padding row did not. The gate drives raw wire bytes so the decode stays
  ;; inside the seam under test.
  (with-frame-model (model 80 24 :session-id "mouse-hitbox"
                     :paint ((exchange "prompt bubble text" "assistant prose")))
    (let ((text-row (frame-row-carrying model "prompt bubble text")))
      (labels ((hover (x y)
                 ;; A terminal's pure-motion report over cell X,Y, decoded by the real parser.
                 (setf model.hover-row nil)
                 (nle:update model (nlt:parse-mouse-sequence
                                    (make-string-input-stream (format nil "35;~d;~dM" x y))))
                 model.hover-row)
               (hover-at (y &aux (row (hover 4 y)))
                 (and row (eq :text row.kind) (equal "user" row.role))))
        ;; The bubble owns one padding row above and below its body, and
        ;; the whole card is one hit box.
        (is-table (y) (hover-at y)
          ((1- text-row) "the bubble's top padding row hovers the prompt")
          (text-row "the bubble's body row hovers the prompt")
          ((1+ text-row) "the bubble's bottom padding row hovers the prompt"))
        ;; And nothing outside it does.
        (is (not (hover-at (- text-row 2))))
        (is (not (hover-at (+ text-row 2))))
        ;; The same offset shifted every column: the bubble's first text
        ;; column is the frame's third, and the two-column left inset is
        ;; canvas that hovers nothing.
        (is (hover 3 text-row) "column 3 is the bubble's first text cell")
        (is (not (hover 2 text-row)))))))

(defun dialog-mouse-y-for-body-row (model row)
  "Frame mouse Y (1-based) for visible body ROW of the open docked picker:
the panel's first frame row, plus its padding row, plus the header band."
  (nlk:bind (((panel above) (nle::docked-dialog-frame-rows model)) (inner (- panel 2))
             (header (min (nle::dialog-header-row-count
                           model.dialog)
                          inner)))
    (+ above 1 header row 1)))

(deftest mouse-hover-pulls-the-dialog-selection ()
  ;; The Zig handleActiveDialogHover seam: pure motion over an occupied
  ;; body row pulls the picker selection there and re-fires the highlight
  ;; seam; motion over the header band or past the last occupied row
  ;; leaves the selection alone; a click confirms through the shared
  ;; Enter path.
  (with-frame-model (model 80 24 :session-id "dialog-hover")
    (let ((highlighted '())
          (picked nil))
      (nle::open-picker-dialog
       model "Hover"
       (picker-rows '("a" :value :a) '("b" :value :b) '("c" :value :c))
       (lambda (model value) (declare (ignore model))
         (setf picked value) nil)
       :on-highlight (lambda (model row) (declare (ignore model))
                       (push (nle::picker-row-value row) highlighted)))
      (is (equal '(:a) highlighted) "opening highlights the initial row")
      (mouse-update model #'nlt:make-mouse-motion-msg 6 (dialog-mouse-y-for-body-row model 2))
      (let ((dialog model.dialog))
        (is (= 2 dialog.selection))
        (is (equal '(:c :a) highlighted))
        ;; Motion over the header band and past the occupied rows is inert.
        (nle:update model (nlt:make-mouse-motion-msg :x 6 :y 4))
        (mouse-update model #'nlt:make-mouse-motion-msg 6 (dialog-mouse-y-for-body-row model 5))
        (is (= 2 dialog.selection))
        (is (equal '(:c :a) highlighted)))
      ;; A click lands hover-then-confirm through the one Enter path, on
      ;; RELEASE: the press only arms the selection anchor, which is what lets
      ;; a drag across the panel mark its text instead of confirming.
      (let ((y (dialog-mouse-y-for-body-row model 1)))
        (nle:update model (nlt:make-mouse-click-msg :x 6 :y y :button :left))
        (is (nle::chat-model-dialog model))
        (nle:update model (nlt:make-mouse-release-msg :x 6 :y y :button :left)))
      (is (eq :b picked) "a click confirms the row it lands on")
      (is (null model.dialog) "the click closes the dialog"))))

(defun frame-cell-of (model needle)
  "=> (values X Y), the 1-based frame cell NEEDLE's first character paints
at in MODEL's last composed frame, or NIL."
  (let ((plains (nle::frame-plain-rows model)))
    (loop for y from 0 below (length plains)
          for at = (search needle (aref plains y))
          when at
            return (values (1+ (nle::text-display-width (subseq (aref plains y) 0 at))) (1+ y)))))

(deftest a-click-on-a-dialog-tab-chip-shows-that-tab ()
  ;; A tabbed dialog's strip is chips to press (/usage's ranges, /models'
  ;; providers): a click on one shows its list as Tab walking there would --
  ;; the cursor home, the highlight delivered, the dialog open -- and a drag
  ;; from one shows nothing.
  (with-frame-model (model 80 24 :session-id "tab-chip-click")
    (let ((highlighted '()))
      (nle::open-picker-dialog
       model "Tabs" (picker-rows '("a1" :key "a" :value "a1") '("a2" :key "a" :value "a2")
                                 '("b1" :key "b" :value "b1"))
       (constantly nil)
       :on-highlight (lambda (model row) (declare (ignore model))
                       (push (nle::picker-row-value row) highlighted)))
      (let ((dialog model.dialog))
        (nle::register-picker-tabs dialog (list (list :label "Alpha" :value "a")
                                                (list :label "Beta" :value "b")))
        (dialog-keys model :down)
        (nle::compose-frame model)
        (multiple-value-bind (x y) (frame-cell-of model " Beta ")
          (is (equal '(:dialog-tab . 1) (nle::frame-target-at-position model (+ x 2) y)) "the chip is its tab's target")
          (click-at model (+ x 2) y))
        (is (= 1 (nle::picker-dialog-tab dialog)) "a click on a chip shows its tab")
        (is (equal "b1" (picker-selected-value dialog)) "the cursor comes home with the list")
        (is (equal "b1" (first highlighted)) "the new list's highlight is delivered")
        (is (eq dialog model.dialog) "and the dialog stays open")
        (nle::compose-frame model)
        (multiple-value-bind (x y) (frame-cell-of model " Alpha ")
          (mouse-update model #'nlt:make-mouse-click-msg (+ x 1) y :button :left)
          (mouse-update model #'nlt:make-mouse-motion-msg (+ x 3) y :button :left)
          (mouse-update model #'nlt:make-mouse-release-msg (+ x 1) y :button :left))
        (is (= 1 (nle::picker-dialog-tab dialog)) "a drag from a chip shows nothing")))))

(deftest a-click-on-a-dialog-key-hint-is-that-key ()
  ;; The panel's key row names keys, and a click on one is that key, run as
  ;; a typed one: ctrl+f flips the panel's height, esc closes -- a hint's
  ;; words or a key-hint set's filled cap alike -- and a hint naming no
  ;; single key (up/down) is no target.
  (with-frame-model (model 80 24 :session-id "hint-click")
    (flet ((target (needle)
             (multiple-value-bind (x y) (frame-cell-of model needle)
               (and x (nle::frame-target-at-position model x y)))))
      (nle::open-picker-dialog model "Hints" (picker-rows '("a" :value :a) '("b" :value :b))
                               (constantly nil))
      (nle::compose-frame model)
      (is (equal '(:key . "esc") (target "esc close")))
      (is (null (target "up/down")) "a hint naming two keys is no target")
      (is (not (nle::chat-model-dialog-fullscreen model)))
      (multiple-value-call #'click-at model (frame-cell-of model "ctrl+f full screen"))
      (is (eq t (nle::chat-model-dialog-fullscreen model)) "ctrl+f's hint flips the panel's height")
      (is model.dialog "and the dialog stays open")
      (nle::compose-frame model)
      (multiple-value-call #'click-at model (frame-cell-of model "ctrl+f restore"))
      (is (not (nle::chat-model-dialog-fullscreen model)) "and flips it back")
      (nle::compose-frame model)
      (multiple-value-call #'click-at model (frame-cell-of model "esc close"))
      (is (null model.dialog) "esc's hint closes the dialog")
      (nle::open-picker-dialog model "Caps" (picker-rows '("a" :value :a) '("b" :value :b))
                               (constantly nil)
                               :footer-hints '(("esc" . "close") ("up/down" . "navigate")))
      (nle::compose-frame model)
      (multiple-value-bind (x y) (frame-cell-of model " esc  close")
        (click-at model (+ x 1) y))
      (is (null model.dialog) "a key-hint set's cap is its key too"))))

(defmacro with-highlight-picker ((model highlighted session
                                 &key rest throttle (rows 2) (delivery :instant))
                                &body body)
  "The highlight-pacing gates' shared fixture: an open picker over the first
ROWS of the rows a/b/c, whose ON-HIGHLIGHT pushes every delivered row's value
onto HIGHLIGHTED, under the REST and THROTTLE pacing the gate means to drive
and the DELIVERY its handler declares."
  ;; BODY runs inside WITH-FRAME-MODEL over SESSION with MODEL and HIGHLIGHTED
  ;; bound, so each gate below states only its own claim -- as PACE steps: one
  ;; step moves the selection to SELECT and stages it when NOTIFY, waits WAIT
  ;; seconds, flushes, and asserts EXPECTED is what has been delivered.
  `(with-frame-model (,model 80 24 :session-id ,session)
     (let ((,highlighted '())
           (nle::*picker-highlight-rest* ,rest)
           (nle::*picker-highlight-throttle* ,throttle))
       (nle::open-picker-dialog
        ,model "Pacing"
        (subseq (picker-rows '("a" :value :a) '("b" :value :b) '("c" :value :c)) 0 ,rows)
        (lambda (model value) (declare (ignore model value)) nil)
        :highlight-delivery ,delivery
        :on-highlight (lambda (model row) (declare (ignore model))
                        (push (nle::picker-row-value row) ,highlighted)))
       (flet ((pace (expected &key select notify wait description)
                (let ((dialog (nle::chat-model-dialog ,model)))
                  (when select (setf dialog.selection select))
                  (when notify (nle::picker-notify-highlight ,model dialog)))
                (when wait (sleep wait))
                (nle::picker-flush-highlight ,model)
                (is (equal expected ,highlighted)
                    (or description (format nil "~s delivered" expected)))))
         (declare (ignorable #'pace))
         ,@body))))

(deftest staged-highlight-waits-out-the-rest-window ()
  ;; The pacing contract itself, with a real (nonzero) rest: a row being
  ;; arrowed past never delivers; the row the operator rests on delivers
  ;; once the window elapses; restaging the same row keeps its clock.
  (with-highlight-picker (model highlighted "highlight-rest"
                          :rest 0.03 :throttle 10)
    ;; The staged lane (a search edit's notify): move the selection by
    ;; hand and notify without :instant.
    (pace '(:a) :select 1 :notify t)
    ;; Restage the same row: the clock must keep running, not restart.
    (pace '(:b :a) :notify t :wait 0.05)
    (pace '(:b :a))))

(deftest a-selection-move-stages-and-a-burst-delivers-the-row-it-rests-on ()
  ;; A held arrow is a burst: no landing hydrates in the move's own pass,
  ;; every move stages, and the tick delivers only the row the burst rests
  ;; on -- once.
  (with-highlight-picker (model highlighted "highlight-burst"
                          :rest 0 :throttle 10 :rows 3)
    ;; The open's own landing delivers at once (:INSTANT); the moves stage.
    (dialog-keys model :down :down)
    (is (equal '(:a) highlighted) "no landing delivered in the moves' own pass")
    (is (nle::picker-dialog-pending-highlight model.dialog) "the last landing is staged")
    (pace '(:c :a) :description "the row the burst rests on, once")
    (pace '(:c :a) :description "and nothing more")))

(deftest a-live-dialog-shows-every-row-a-burst-crosses ()
  ;; A handler that never blocks the loop (:HIGHLIGHT-DELIVERY :LIVE, the
  ;; /sessions preview) owes no stage: a held arrow delivers every row it
  ;; crosses in the key's own pass, whatever the pacing says.
  (with-highlight-picker (model highlighted "highlight-live"
                          :rest 10 :throttle 10 :rows 3 :delivery :live)
    (dialog-keys model :down :down)
    (is (equal '(:c :b :a) highlighted) "the open's row, then each row crossed")
    (is (null (nle::picker-dialog-pending-highlight model.dialog)) "nothing staged")))

(deftest a-key-tells-the-idle-collector-the-operator-is-working ()
  ;; GC.LISP's idle collector holds its stop-the-world pause off while the
  ;; operator types: every key and pointer event stamps the moment.
  (with-frame-model (model 80 24 :session-id "input-stamp")
    (let ((nlk::*idle-input-at* nil))
      (nle:update model (nlt:make-key-press-msg :code #\a))
      (is (integerp nlk::*idle-input-at*) "a key stamps the operator's last input")
      (setf nlk::*idle-input-at* nil)
      (nle:update model (nlt:make-tick-msg))
      (is (null nlk::*idle-input-at*) "a clock tick is not the operator"))))

(deftest moving-selection-still-previews-at-the-throttle-cadence ()
  ;; The staged lane's live-follow contract (the search-narrowing case):
  ;; with the pause gate effectively off, a stage that keeps moving still
  ;; delivers at the throttle cadence — the preview follows the query
  ;; instead of waiting for a stop.
  (with-highlight-picker (model highlighted "highlight-throttle"
                          :rest 10 :throttle 0.03 :rows 3)
    (pace '(:a) :select 1 :notify t)
    (pace '(:b :a) :wait 0.05)
    (pace '(:b :a) :select 2 :notify t)
    (pace '(:c :b :a) :wait 0.05)))

(deftest picker-escape-fires-on-cancel (with-frame-model (model 80 24 :session-id "dialog-cancel"))
  ;; The Zig cancelThemePreviewSession seam: Esc-dismiss invokes ON-CANCEL,
  ;; a selection does not, and on a searchable picker Esc clears the query
  ;; first without cancelling.
  (let ((cancelled 0))
    (flet ((reopen (&key searchable)
             (nle::open-picker-dialog
              model "Cancel"
              (picker-rows '("a" :value :a))
              (lambda (model value) (declare (ignore model value)) nil)
              :searchable searchable
              :on-cancel (lambda (model) (declare (ignore model))
                           (incf cancelled)))))
      (reopen)
      (nle::picker-dialog-key model :enter)
      (is (zerop cancelled) "a selection never cancels")
      (reopen :searchable t)
      (dialog-keys model #\a :escape)
      (is (nle::chat-model-dialog model))
      (is (equal "" (nle::picker-dialog-search model.dialog)) "Esc with a query only clears it")
      (is (zerop cancelled) "clearing the query never cancels")
      (nle::picker-dialog-key model :escape)
      (is (null model.dialog) "the second Esc dismisses")
      (is (= 1 cancelled) "the dismissal fires ON-CANCEL once"))))

(deftest mouse-drag-selection-copies-on-release ()
  (with-frame-model (model 80 24 :session-id "mouse-select"
                     :paint ((nle::ingest-message-fact "assistant" "alpha one")
                             (nle::ingest-message-fact "assistant" "beta two")))
    (let ((y0 (mouse-y-for-text model "alpha one"))
          (y1 (mouse-y-for-text model "beta two")))
      (mouse-update model #'nlt:make-mouse-click-msg 6 y0 :button :left)
      (is (nle::chat-model-selection-anchor model) "press drops the anchor")
      (mouse-update model #'nlt:make-mouse-motion-msg 13 y1 :button :left)
      (is (nle::chat-model-selection model) "drag extends the selection")
      (is (equal (cons (1- y0) 5) model.selection-anchor))
      (is-carrying (text (nle::frame-selection-text model))
        ("alpha one" "the first line is selected") ("beta two" "the drag reaches the second line"))
      (let ((cmd (nth-value 1 (mouse-update model #'nlt:make-mouse-release-msg 13 y1 :button :left))))
        (is (null cmd))
        ;; No program, so nothing was written -- and the hint says that,
        ;; never `copied' (issue #31).
        (is (equal nle::+selection-copy-failed-hint+
                   (nle::status-said model))))
      (is (nle::chat-model-selection model))
      ;; A fresh press clears it. The cell differs from the drag's own
      ;; press: two clicks on the SAME cell inside the double-click window
      ;; are the word gesture (DOUBLE-CLICK-SELECTS-THE-WORD-UNDER-THE-POINTER),
      ;; not a plain click.
      (click-at model 9 y0)
      (is (null model.selection)))))

(deftest copy-says-what-the-clipboard-took ()
  ;; Issue #31: the hint said `copied' before any write, whether or not
  ;; anything took the text. The status now waits for the writer and says
  ;; what happened: copied for a clipboard tool, sent for OSC 52 alone,
  ;; failed for neither, and nothing-to-copy for an empty mark.
  (with-frame-model (model 80 24 :session-id "copy-truth"
                     :paint ((nle::ingest-message-fact "assistant" "alpha one")))
    ;; The whole path through a program with no clipboard tool: the release
    ;; says nothing yet, the OSC 52 request reaches the loop as terminal-io,
    ;; and the status lands only after that write.
    (let ((program (nlt:make-program model)))
      (setf (nlt:program-running program) t
            (nle::chat-model-program model) program)
      (with-saved-globals ((nlt:*clipboard-use-command* nil))
        (let ((y (mouse-y-for-text model "alpha one"))
              (mailbox (nlt::program-mailbox program)))
          (mouse-update model #'nlt:make-mouse-click-msg 6 y :button :left)
          (mouse-update model #'nlt:make-mouse-motion-msg 12 y :button :left)
          (mouse-update model #'nlt:make-mouse-release-msg 12 y :button :left)
          (is (null (nle::status-said model)) "nothing is said before the write")
          (let ((io (sb-concurrency:receive-message mailbox :timeout 5)))
            (is (nlt:terminal-io-msg-p io))
            (is (search "]52;c;" (with-output-to-string (out)
                                   (funcall (nlt:terminal-io-msg-thunk io) out)))))
          (let ((notice (sb-concurrency:receive-message mailbox :timeout 5)))
            (nle:update model notice)
            ;; What reached the terminal is said as OSC 52.
            (is (equal nle::+selection-osc52-hint+ (nle::status-said model)))))))
    (flet ((said (outcome)
             (nle:update model (nle::clipboard-outcome-notice outcome))
             (nle::status-said model)))
      (is (equal nle::+selection-copied-hint+ (said '(:command "wl-copy"))))
      (is (equal nle::+selection-osc52-hint+ (said :osc52)) "OSC 52 alone is never `copied'")
      (is (equal nle::+selection-copy-failed-hint+ (said :failed))))
    (setf (nle::chat-model-selection model) nil)
    (nle::copy-frame-selection model)
    (is (equal nle::+selection-empty-hint+ (nle::status-said model)) "nothing marked says so")))

(deftest a-drag-past-the-band-edge-scrolls-and-the-mark-keeps-its-rows ()
  ;; Issue #31: a drag held past the transcript's last row scrolled nothing,
  ;; and a mark could not hold a row the window did not show. Held below the
  ;; band, the drag scrolls on every tick, the mark follows, and the copy
  ;; carries the rows on both sides of the window; a scroll after the release
  ;; moves the window, never the marked words.
  (with-frame-model (model 80 24 :session-id "drag-scroll"
                     :paint ((nle::ingest-message-fact
                              "assistant"
                              (format nil "~{line ~2,'0d~^~%~%~}" (loop for i from 1 to 40 collect i)))))
    (with-saved-globals ((nle::*drag-scroll-period-ms* 0))
      (nle::scroll-transcript model :home)
      (multiple-value-bind (top bottom) (nle::transcript-lane model)
        (let* ((window model.band)
               (start (nle::band-window-offset window))
               (y0 (mouse-y-for-text model "line 01"))
               (below (+ bottom 2)))
          ;; Line 40 starts out of sight.
          (is (not (find-if (lambda (row) (search "line 40" row)) (nle::frame-plain-rows model))))
          (mouse-update model #'nlt:make-mouse-click-msg 3 y0 :button :left)
          (mouse-update model #'nlt:make-mouse-motion-msg 20 below :button :left)
          (is (> (nle::band-window-offset window) start) "the motion past the edge scrolls at once")
          (loop repeat 300
                until (nle::band-window-at-bottom-p window)
                do (nle:update model (nlt:make-tick-msg)))
          (is (nle::band-window-at-bottom-p window) "held still, the ticks go on scrolling")
          (mouse-update model #'nlt:make-mouse-release-msg 20 below :button :left)
          (is (null (nle::drag-edge model)) "the release ends the edge scroll")
          (let ((text (nle::frame-selection-text model)))
            (is-carrying text
              ("line 01" "the anchor's row, scrolled out of the window")
              ("line 20" "a row the drag passed")
              ("line 40" "the rows the scroll brought in"))
            (is (eql 0 (search "line 01" text)) "the mark opens where the press stood")
            (nle::scroll-transcript model :home)
            (is (equal text (nle::frame-selection-text model)) "a scroll keeps the mark's words")
            ;; Scrolled back, the wash is on the anchor's row again, and
            ;; never on the shell's chrome.
            (is (find (1- y0) (nle::mark-frame-cells model model.selection) :key #'first))
            (is (<= top (first (first (nle::mark-frame-cells model model.selection)))))))))))

(deftest a-pointer-that-moves-nothing-owes-no-frame ()
  ;; Terminals report the pointer at every cell it crosses, and most
  ;; crossings move nothing on screen. Those folds are quiet (UPDATE's third
  ;; value), so the loop paints no frame for them; a crossing that moves the
  ;; mark or the hover is not.
  (with-frame-model (model 80 24 :session-id "quiet-pointer"
                     :paint ((nle::ingest-message-fact "assistant" "alpha one")
                             (nle::ingest-message-fact "assistant" "beta two")))
    (let ((y (mouse-y-for-text model "alpha one")))
      (flet ((quiet-p (x &rest keys)
               (nth-value 2 (apply #'mouse-update model #'nlt:make-mouse-motion-msg x y keys))))
        (is (quiet-p 6) "hovering prose moves nothing")
        (is (quiet-p 7) "nor does the next cell of it")
        (mouse-update model #'nlt:make-mouse-click-msg 6 y :button :left)
        (is (not (quiet-p 9 :button :left)) "the drag marks")
        (is (quiet-p 9 :button :left) "the same cell again marks nothing new")
        (is (not (quiet-p 11 :button :left)) "the next cell moves the mark")))))

;;; The painter's view of a frame: its rows laid out into cells, and the
;;; wash laid over them -- what RENDER diffs.
(defun washed-frame (model rows)
  "ROWS laid out into MODEL's frame of cells under its selection overlay."
  (let ((buffer (nlt:parse-styled-string rows (nle::frame-width model) (nle::frame-height model)))
        (overlay (nle::selection-overlay model)))
    (when overlay (funcall overlay buffer))
    buffer))

(defun cell-bg-at (buffer column row)
  "The background parameters of BUFFER's cell at COLUMN, ROW."
  (nlt:cell-bg (nlt:screen-buffer-ref buffer column row)))

(defun fill-bg (tone width)
  "The background parameters a blank row filled with TONE paints at WIDTH."
  (cell-bg-at (nlt:parse-styled-string
               (nle::lline-rows (nle::make-lline :text "" :fill tone) width) width 1) 8 0))

(deftest selection-wash-is-translucent-cell-blend (let ((nle::*color-profile* :truecolor)))
  ;; The drag selection is a wash of the :selection bg over each cell's
  ;; current bg — the Zig design_system.selectionOverlayStyle cell loop, at
  ;; this build's 35% rather than the Zig 55%, so the band reads as a light
  ;; tint. Fg is kept, so selected content stays in its own colors like a
  ;; browser text selection. Default palette: selection bg (58 63 76) over
  ;; the canvas (28 31 38) blends to (39 42 51); over the diagnostic block bg
  ;; (76 42 41) it blends to (70 49 53).
  (with-frame-model (model 80 24 :session-id "wash-blend")
    ;; The band's words stand under the frame head's three rows, past its
    ;; two-column inset (TRANSCRIPT-LANE).
    (setf model.selection '((3 . 2) . (5 . 6)))
    (let ((buffer (washed-frame model
                                (list "" "" ""
                                      (concatenate 'string "  " (nle::render-row
                                                                 '() (list (nle::make-seg :text "error text" :tone :danger))
                                                                 (nle::canvas-fill-bg) 20))
                                      ""
                                      (concatenate 'string "  " (nle::styled "block" (nle::style-for-tone :diagnostic-block)))))))
      ;; The glyph keeps its own colour.
      (is (equal "38;2;255;126;126" (nlt:cell-fg (nlt:screen-buffer-ref buffer 2 3))))
      ;; Interior rows of a multi-line drag wash the lane's whole width, blank
      ;; rows included — the Zig cell loop tints over the memset canvas —
      ;; and never the inset, the gap or the rail beside it.
      (is-table (column row bg) (equal bg (cell-bg-at buffer column row))
        (2 3 "48;2;39;42;51" "the canvas under the text blends")
        (77 3 "48;2;39;42;51" "an interior row washes to the lane's edge")
        (78 3 nil "never the gap before the rail")
        (0 4 nil "nor the band's inset")
        (40 4 "48;2;39;42;51" "a blank interior row still carries the wash")
        (2 5 "48;2;70;49;53" "a filled block blends over its own background")
        (7 5 nil "the release column closes the mark")))))

(deftest selection-wash-keeps-nominal-ground-on-inherited-canvas ()
  ;; The terminal's inherited RGB is unknown. Preserve the theme's nominal
  ;; 35% wash rather than blending the selection against itself; explicit
  ;; cell grounds still own their blend, and unmarked canvas stays inherited.
  (dolist (name '("Tungstene" "Hello Kitty Lo-Fi" "Komorebi" "Blueprint Daylight" "Sparse"))
    (let* ((bundled (if (string= name "Sparse")
                        "{\"themes\":[{\"name\":\"Sparse\",\"colors\":{\"background-void\":\"#112233\",\"selection\":\"#cc8844\"}}]}"
                        nle::*bundled-themes-json*))
           (opaque (theme-palette name :bundled bundled :user
                    (format nil "{\"themes\":[{\"name\":~s,\"transparent-background\":false}]}" name)))
           (ground (nle::visual-style-bg (gethash :background opaque)))
           (selection (nle::visual-style-bg (gethash :selection opaque)))
           (wash (nle::blend-rgb selection ground nle::+selection-overlay-alpha+))
           (panel '(59 57 55))
           (panel-wash (nle::blend-rgb selection panel nle::+selection-overlay-alpha+)))
      (dolist (transparent '(nil t))
        (let ((nle::*palette*
                (theme-palette name :bundled bundled :user
                 (format nil "{\"themes\":[{\"name\":~s,\"transparent-background\":~a}]}"
                         name (if transparent "true" "false")))))
          (when (and transparent (string= name "Tungstene"))
            (is (equal '(95 47 14) wash) "Tungstene keeps the prior faint orange, not raw #f97316"))
          (dolist (nle::*color-profile* '(:truecolor :ansi256 :ansi :none))
            (with-frame-model (model 12 24 :session-id "inherited-wash")
              ;; The lane of a 12-column frame: rows 3 on, columns 2 to 9.
              (setf model.selection '((3 . 2) . (5 . 4)))
              (let* ((rows (list "" "" ""
                                 (concatenate 'string "  " (nle::styled "語x" (nle::vstyle :fg '(120 180 220) :bold t)))
                                 ""
                                 (concatenate 'string "  " (nle::styled "panel" (nle::vstyle :fg '(210 200 190) :bg panel)))))
                     (bare (nlt:parse-styled-string rows 12 24))
                     (buffer (washed-frame model rows))
                     (overlay (nle::selection-overlay model)))
                (cond ((eq nle::*color-profile* :none)
                       (is (null overlay) "colorless selection carries no wash")
                       (is (every (lambda (cell) (null (nlt:cell-bg cell)))
                                  (nlt::screen-buffer-cells buffer)) "colorless canvas and panels stay colorless"))
                      (t (is (equal (nle::color-sgr wash nil nle::*color-profile*) (cell-bg-at buffer 2 3)) "text on default cells uses the nominal canvas wash")
                         (is (equal (nle::color-sgr wash nil nle::*color-profile*) (cell-bg-at buffer 9 3)) "selected padding uses the same wash")
                         (is (equal (nle::color-sgr wash nil nle::*color-profile*) (cell-bg-at buffer 6 4)) "blank interior row stays faint")
                         (is (equal (nle::color-sgr
                                     (nle::blend-rgb selection
                                      (nle::sgr-rgb (cell-bg-at bare 2 5))
                                      nle::+selection-overlay-alpha+) nil nle::*color-profile*)
                                    (cell-bg-at buffer 2 5)) "explicit cell ground still owns its quantized blend")
                         (when (eq nle::*color-profile* :truecolor)
                           (is (equal (nle::color-sgr panel-wash nil nle::*color-profile*) (cell-bg-at buffer 2 5)) "truecolor panel blends at 35 percent"))))
                (is (equal (cell-bg-at bare 5 5) (cell-bg-at buffer 5 5)) "unselected panel cell past the release is untouched")
                (is (null (cell-bg-at buffer 0 6)) "unselected blank canvas never acquires a fill")
                (is (null (cell-bg-at buffer 10 3)) "the gap before the rail never washes")
                (is (null (cell-bg-at buffer 0 4)) "nor the band's inset")
                (dolist (column '(2 3 4))
                  (let ((before (nlt:screen-buffer-ref bare column 3))
                        (after (nlt:screen-buffer-ref buffer column 3)))
                    (is (equal (nlt:cell-char before) (nlt:cell-char after)) "wide glyph and its continuation survive")
                    (is (equal (nlt:cell-fg before) (nlt:cell-fg after)) "selection preserves the foreground")
                    (is (= (nlt::cell-attrs before) (nlt::cell-attrs after)) "selection preserves text attributes")))))))))))

(deftest a-mark-restyles-no-row (let ((nle::*color-profile* :truecolor)))
  ;; The wash is the frame's overlay over the painter's cells (NLT:MAKE-VIEW
  ;; :OVERLAY), so a mark leaves every row the frame composed as it was: a
  ;; frame that moves only the mark lays out no row twice — without the
  ;; overlay, each marked row would be re-parsed and re-painted as a string —
  ;; and the band's rows keep the width and the scroll-gutter cell they were
  ;; guarded to.
  (with-frame-model (model 80 24 :session-id "wash-rows"
                     :paint ((nle::ingest-message-fact "assistant" "alpha one")))
    (let ((before (nle::compose-frame model)))
      (is (null (nlt:view-state-overlay (nle::shell-view model))) "nothing marked, nothing laid over")
      (setf model.selection '((0 . 0) . (23 . 79)))
      (is (equal before (nle::compose-frame model)))
      (is (functionp (nlt:view-state-overlay (nle::shell-view model)))))))

(deftest selection-overlay-spans-follow-the-drag (let ((nle::*color-profile* :truecolor)))
  ;; The mark's own geometry, the span FRAME-SELECTION-TEXT copies: the press
  ;; row from its column on, the rows between it across the transcript's
  ;; lane, the release row through its column -- never the frame head, the
  ;; band's inset, the gap or the rail (TRANSCRIPT-LANE).
  (with-frame-model (model 80 24 :session-id "wash-spans")
    (setf model.selection '((3 . 4) . (5 . 5)))
    (let ((buffer (washed-frame model (list "" "" "" "  hello world" "  hi" "  abcdefgh" "  outside the drag"))))
      (is-table (column row washed) (eq washed (equal "48;2;39;42;51" (cell-bg-at buffer column row)))
        (3 3 nil) (4 3 t) (77 3 t) (78 3 nil) (79 3 nil) (1 4 nil) (2 4 t) (77 4 t) (5 5 t) (6 5 nil) (2 6 nil) (4 2 nil)))))

(deftest word-span-follows-the-terminal-word-contract ()
  ;; The Zig IS-WORD-GRAPHEME / WORD-DISPLAY-COLUMN-BOUNDS contract: ASCII
  ;; alphanumerics and `_', and every non-ASCII character; everything else —
  ;; the hyphen, the space, punctuation — is a separator, where a double-click
  ;; selects nothing at all.
  (is-table (text at span) (equal 'span (multiple-value-list (nle::word-span-at text at)))
    ("alpha beta" 2 (0 5) "the run opens at the word's first character and closes past its last")
    ("kebab-case_name" 8 (6 15) "after the hyphen, through the underscore, to the value's end")
    ("naïve ok" 2 (0 5) "a non-ASCII rune is a word character, so the accented word is one run")
    ("alpha beta" 5 (nil) "a space is a separator")
    ("a-b" 1 (nil) "a hyphen is a separator")
    ("alpha" 9 (nil) "past the end there is no word")
    ("" 0 (nil) "an empty line holds no word")))

(deftest double-click-selects-the-word-under-the-pointer ()
  ;; The Zig selectTranscriptWordAtCell gesture: the second left press on the
  ;; same cell inside the double-click window selects the whole word under the
  ;; pointer. The release copies it and leaves the wash up, and the press pair
  ;; is consumed, so a third click starts a fresh cycle.
  (with-frame-model (model 80 24 :session-id "word-select"
                     :paint ((nle::ingest-message-fact "assistant" "alpha beta-gamma delta")))
    (let* ((line (row-index-of "beta-gamma" model.line-plain))
           (gamma (search "gamma" (aref model.line-plain line)))
           ;; The frame lane's coordinates for the same cell: the transcript
           ;; lane's column plus the band's 2-column inset, on the frame row
           ;; the line landed on.
           (row (1- (mouse-y-for-line model line)))
           (column (+ 2 gamma))
           (x (+ 3 gamma))
           (y (1+ row)))
      (is (equal (cons (cons row column) (cons row (+ column 4)))
                 (nle::word-selection-at model row (1+ column))))
      (is (null (nle::word-selection-at model row (1- column))))
      ;; One click: no selection, nothing copied.
      (click-at model x y)
      (is (null model.selection))
      ;; The second click on the same cell, inside the window.
      (nle:update model (nlt:make-mouse-click-msg :x x :y y :button :left))
      (is-shape model (.selection (nle::word-selection-at model row column))
        (nle::frame-selection-text "gamma"))
      (nle:update model (nlt:make-mouse-release-msg :x x :y y :button :left))
      ;; No program here: the copy says nothing was written (issue #31).
      (is-shape model (nle::status-said nle::+selection-copy-failed-hint+)
        (nle::chat-model-selection is))
      (is (not (nle::note-left-press model row column))))))

(deftest selection-marks-only-the-transcripts-words (let ((nle::*color-profile* :truecolor)))
  ;; A drag selects what was said, never the shell around it: the header,
  ;; the composer, the footer, the band's inset and the scroll rail are
  ;; chrome, and a mark neither washes nor copies them.
  (with-frame-model (model 80 24 :session-id "mark-words" :typed "pick me now"
                     :paint ((nle::ingest-message-fact "assistant" "alpha one")))
    (let* ((rows (nle::compose-frame model))
           (plains (mapcar #'nle::plain-frame-row rows))
           (composer (row-index-of "pick me now" plains :required t))
           (header (row-index-of "mark-words" plains))
           (answer (row-index-of "alpha one" plains :start 3 :required t))
           (column (search "pick" (nth composer plains)))
           (painted (nlt:parse-styled-string rows 80 24)))
      ;; A drag over the composer's words marks nothing and copies nothing.
      (mouse-update model #'nlt:make-mouse-click-msg (1+ column) (1+ composer) :button :left)
      (mouse-update model #'nlt:make-mouse-motion-msg (+ column 4) (1+ composer) :button :left)
      (is (null model.selection) "the composer is the shell's")
      (is (equal "" (or (nle::frame-selection-text model) "")))
      (mouse-update model #'nlt:make-mouse-release-msg (+ column 4) (1+ composer) :button :left)
      ;; A double-click on the composer's word selects none.
      (is (null (nle::word-selection-at model composer (1+ column))) "no word of the shell's")
      (when header
        (is (null (nle::word-selection-at model header (search "mark" (nth header plains)))) "nor the header's"))
      ;; A drag from the answer down over the composer and the footer marks
      ;; the answer to the band's last cell, and copies the answer alone.
      (mouse-update model #'nlt:make-mouse-click-msg 4 (1+ answer) :button :left)
      (mouse-update model #'nlt:make-mouse-motion-msg 80 24 :button :left)
      (multiple-value-bind (top bottom left right) (nle::transcript-lane model)
        (is (< top answer bottom) "the band runs on under the answer")
        (is (equal (cons (cons answer 3) (cons bottom right)) model.selection) "the mark ends on the band's last cell")
        (is (equal "alpha one" (nle::frame-selection-text model)) "the copy is the words, no prompt, no footer")
        (let ((washed (washed-frame model rows)))
          (is (equal (cell-bg-at painted column composer) (cell-bg-at washed column composer)) "the composer keeps its paint")
          (is (equal (cell-bg-at painted 79 bottom) (cell-bg-at washed 79 bottom)) "the rail's column never washes")
          (is (equal (cell-bg-at painted 0 bottom) (cell-bg-at washed 0 bottom)) "nor the band's inset")
          (is (not (equal (cell-bg-at painted left bottom) (cell-bg-at washed left bottom))) "the lane under the answer does"))))))

(deftest a-copy-leaves-the-gutters-marks-behind ()
  ;; A notice's ■ and a disclosure's ▸ stand in the gutter beside the words,
  ;; and a drag across the whole row copies the words alone, the indent the
  ;; gutter gave them taken off.
  (is-each (nle::without-gutter-marks)
    (" ■ a notice" "   a notice" "a notice's mark blanks, column for column")
    (" ▸ Ran 2 commands" "   Ran 2 commands" "a shut disclosure's")
    ("   ⎿ output" "     output" "tool output's hook")
    ("   ▌ ↦ tail" "       tail" "a code edge and its wrap mark")
    ("   a ▸ inside" "   a ▸ inside" "one inside the words stays")
    ("" "" "an empty row stays empty"))
  (with-frame-model (model 80 24 :session-id "mark-gutter"
                     :paint ((nle::ingest-message-fact "assistant" "alpha one")
                             (nle::ingest-message-fact "assistant" "beta two")))
    (let* ((plains (coerce (nle::frame-plain-rows model) 'list))
           (first-row (row-index-of "alpha one" plains :start 3 :required t))
           (last-row (row-index-of "beta two" plains :start 3 :required t)))
      (setf model.selection (cons (cons first-row 0) (cons last-row 79)))
      (let ((lines (remove "" (uiop:split-string (nle::frame-selection-text model) :separator '(#\Newline))
                           :test #'equal)))
        (is (equal '("alpha one" "beta two") lines) "the words at their own margin, no inset, gutter or rail")))))

(deftest a-press-outside-the-double-click-window-is-a-single-click ()
  (with-frame-model (model 80 24 :session-id "word-window")
    (setf (gethash model nle::*left-press-state*)
          (list 4 9 (- (get-internal-real-time)
                       internal-time-units-per-second)))
    (is (not (nle::note-left-press model 4 9)))
    (is (nle::note-left-press model 4 9))
    (is (not (nle::note-left-press model 4 10)))))

(deftest mouse-click-on-user-prompt-opens-checkpoints ()
  (with-frame-model (model 80 24 :session-id "mouse-restore"
                     :paint ((nle::ingest-message-fact "user" "restore me")))
    (let ((index (line-row-index model :role "user")))
      (is index "the line table locates the prompt")
      (mouse-update model #'nlt:make-mouse-click-msg 6 (mouse-y-for-line model index) :button :left)
      (is (search "gateway" (or (nle::status-said model) ""))))))

;;; --- block kinds: image result ---------------------------------------------

(defparameter +ui-test-png-base64+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/p9sAAAAASUVORK5CYII="
  "A 1x1 PNG, the kitty test payload.")

(deftest image-result-renders-fallback-and-kitty-band (with-temp-store ())
  (with-durable-turn (turn (ensure-durable-session "image-card") :input "draw" :complete t)
    (let ((metadata (nlk:json-object
                     "image" (nlk:json-object "payload_kind" "base64"
                                              "source" "tool_result"
                                              "provider_executed" nil
                                              "preliminary" nil))))
      (record-tool-use turn "call-img" "gen_image" "{}" +ui-test-png-base64+ :metadata metadata)))
  ;; Tall enough to hold the whole card: a band whose first row is scrolled
  ;; above the viewport anchors nothing, by design.
  (let ((model (make-frame-model 100 40 :session-id "image-card" :tool-detail :developer)))
    (nle::render-state model)
    (flet ((row-blank-count (&aux (refs model.line-rows) (plains model.line-plain))
             ;; Blank rows the image card reserves for its preview band,
             ;; past the two padding rows every block owns.
             (- (loop for index below (length refs)
                      for ref = (aref refs index)
                      count (and (nle::transcript-row-p ref)
                                 (eq :tool-call
                                     ref.kind)
                                 (zerop (length (string-trim
                                                 '(#\Space)
                                                 (aref plains index))))))
                2)))
      (let ((plains (coerce model.line-plain 'list)))
        (is (row-carrying "Viewed gen_image" plains) "titled by the :image family's own verb")
        (is (row-carrying "base64 image payload" plains))
        (is (not (rows-carry-p "iVBOR" plains))))
      (is (zerop (row-blank-count)))
      ;; The terminal answers the kitty query: the preview band reserves
      ;; its rows and the visible card anchors one placement.
      (nle:update model (nlt:make-kitty-graphics-msg :ok-p t))
      (is (= nle::*image-preview-rows* (row-blank-count)))
      ;; The call owns the transmission, so the cache is keyed by it.
      (setf (gethash "call-img" model.kitty-image-cache)
            (nlt:make-terminal-image :id 42))
      (let ((placements (nle::kitty-image-placements model)))
        (is (= 1 (length placements)) "one visible card, one placement")
        (let ((placement (first placements)))
          (is (eql 42 (nlt:terminal-image-id
                       placement.image)))
          (is-shape placement (.x = 5 "the card's body column")
            (.rows = nle::*image-preview-rows*) (.placement-id = 1))))
      ;; Payload hygiene: garbage never transmits and never retries.
      (is (null (nle::ensure-kitty-image model "call-junk" "not base64 at all!")))
      (is (eq :failed (gethash "call-junk"
                               model.kitty-image-cache)))
      (is-table (expected payload) (equal expected (nle::inferred-image-payload-kind payload))
        ("data_url" "data:image/png;base64,AAAA")
        ("url" "https://example.test/x.png")))))

(deftest look-image-card-names-the-file-and-sizes-its-band (with-temp-store ())
  ;; A LOOK stamps a file fact, never bytes (exec.lisp): the card names the
  ;; path and what the file is, the band is sized from the fact's own pixel
  ;; size, and the preview is read from the PNG the fact names.
  (with-temp-file (png :type "png")
    (alexandria:write-byte-vector-into-file
     (cl-base64:base64-string-to-usb8-array +ui-test-png-base64+) png)
    (with-durable-turn (turn (ensure-durable-session "look-file") :input "look" :complete t)
      (record-tool-use turn "call-look" "eval" "{}" "[image: shown at the next round]"
                       :metadata (nlk:json-object
                                  "image" (nlk:json-object
                                           "payload_kind" "file" "source" "look"
                                           "path" png "png_path" png
                                           "media_type" "image/png"
                                           "width" 640 "height" 480
                                           "bytes" 4096))))
    ;; Wide enough that the caption holds a temp file's whole path on one
    ;; row, wherever TMPDIR is.
    (let ((model (make-frame-model 200 60 :session-id "look-file" :tool-detail :developer)))
      (nle::render-state model)
      (nle:update model (nlt:make-kitty-graphics-msg :ok-p t))
      (is (rows-carry-p (format nil "Viewed ~a" (file-namestring png)) model.line-plain))
      (is (rows-carry-p (format nil "~a · image/png · 640x480 · 4.0 KB" png) model.line-plain))
      ;; 480px at the nominal cell height wants 27 rows and the frame
      ;; would hold 36; the ceiling decides.
      (let ((fact (nle::row-image-plist
                   (find-if #'nle::row-image-plist
                            (remove-if-not #'nle::transcript-row-p (coerce model.line-rows 'list))))))
        (is (= nle::*image-preview-max-rows* (nle::image-preview-rows fact 100)))
        (is (= 3 (nle::image-preview-rows fact 12)) "a narrow frame clamps it")
        (is (equal png (nle::image-preview-payload fact
                                                   (nle::make-transcript-row :kind :tool-call)))))
      ;; The preview is the file's own bytes, read at draw time.
      (is (equal +ui-test-png-base64+ (nle::image-payload-base64 png)))
      ;; The card reads header, picture, caption — and the caption
      ;; is what the picture is, under it.
      (let* ((header (row-index-of "Viewed " model.line-plain))
             (caption (row-index-of "image/png · 640x480" model.line-plain)))
        (is (and header caption (< header caption)) "the caption sits under the band")
        (is (= nle::*image-preview-max-rows* (- caption header 1)))
        (is (zerop (length (string-trim " " (aref model.line-plain (1+ header))))))
        (is (zerop (length (string-trim " " (aref model.line-plain (1- header))))))
        (is (nle::image-band-at model.line-rows model.line-plain (1+ header)) "the band opens under the header")
        (is (null (nle::image-band-at model.line-rows model.line-plain (1- header))) "never on the block's padding row"))
      (setf (gethash "call-look" model.kitty-image-cache)
            (nlt:make-terminal-image :id 7))
      (let ((placement (first (nle::kitty-image-placements model))))
        (is placement "the visible card anchors one placement")
        (is-shape placement (.rows = nle::*image-preview-max-rows*) (.x = 5))
        ;; The anchor is the row after the header, never the block's
        ;; own top padding row — the bug that painted every image
        ;; over its own card.
        (let* ((band-row (- placement.y 3 (nth-value 3 (nle::transcript-band-metrics model))))
               (index (nle::band-window-row-index model.band
                                                  band-row)))
          (is (zerop (length (string-trim " " (aref model.line-plain index)))))
          (is (search "Viewed " (aref model.line-plain (1- index)))))))))

(deftest an-image-with-no-carriable-png-reserves-no-band ()
  ;; A JPEG on a box with no transcoder: the fact names no png_path, so the
  ;; card says why and nothing reserves rows a placement could never fill.
  (let ((fact '(:payload-kind "file" :source "look" :path "/tmp/shot.jpg"
                :media-type "image/jpeg" :width 800 :height 600 :bytes 900)))
    (is (search "no inline preview" (nle::image-card-detail fact)))
    (is (search "/tmp/shot.jpg · image/jpeg · 800x600 · 900 B" (nle::image-card-detail fact)))
    (is (equal "shot.jpg" (nle::image-card-name fact (nle::make-transcript-row :kind :tool-call))))
    (is (null (nle::image-preview-payload fact (nle::make-transcript-row :kind :tool-call :text "x"))))))

(deftest an-answer-s-picture-draws-under-the-line-that-names-it (with-temp-store ())
  ;; An answer's image fact rides its message (exec.lisp ANSWER-IMAGES): the
  ;; row says the caption where the terminal has no graphics, and where it has
  ;; them reserves the band a card would, from the fact, and anchors the
  ;; placement on it. A picture no fact names stays a link.
  (with-temp-file (png :type "png")
    (alexandria:write-byte-vector-into-file
     (cl-base64:base64-string-to-usb8-array +ui-test-png-base64+) png)
    (with-durable-turn (turn (ensure-durable-session "answer-picture") :input "chart it" :complete t)
      (nlk:record-assistant-message
       turn (nlk:json-object
             "role" "assistant"
             "content" (format nil "Sales rose.~%~%The chart: ![sales by month](chart.png)~%~%And ![a logo](https://example.invalid/l.png) closes."))
       :metadata (nlk:json-object
                  "images" (vector (nlk:json-object
                                    "payload_kind" "file" "source" "answer"
                                    "target" "chart.png" "path" "chart.png"
                                    "copy_path" png "png_path" png
                                    "media_type" "image/png"
                                    "width" 640 "height" 480 "bytes" 4096)))))
    (let ((model (make-frame-model 100 60 :session-id "answer-picture")))
      (flet ((at (needle) (row-index-of needle (coerce model.line-plain 'list))))
        (is (at "The chart: sales by month") "the line reads as the alt text, its markup hidden")
        (is (eql (at "chart.png · image/png · 640x480 · 4.0 KB") (1+ (at "The chart: sales by month"))) "no graphics: the caption, under the line that names the picture")
        (is (at "And a logo closes.") "a remote picture is a link, and no fact names it")
        (is (null (nle::kitty-image-placements model)))
        (nle:update model (nlt:make-kitty-graphics-msg :ok-p t))
        (let ((named (at "The chart: sales by month")))
          (is (= nle::*image-preview-max-rows* (- (at "chart.png · image/png") named 1)) "graphics: the card's band, sized from the fact, between them")
          (is (< (at "chart.png · image/png") (at "And a logo closes.")) "and the answer goes on under the caption")
          ;; An answer's picture is a file named by its bytes: the path keys it.
          (setf (gethash png model.kitty-image-cache) (nlt:make-terminal-image :id 9))
          (let ((placement (first (nle::kitty-image-placements model))))
            (is placement "the visible answer anchors one placement")
            (is-shape placement (.rows = nle::*image-preview-max-rows*) (.x = 5 "the prose's own column"))
            (let ((band-row (- placement.y 3 (nth-value 3 (nle::transcript-band-metrics model)))))
              (is (= (1+ named) (nle::band-window-row-index model.band band-row)) "on the row under the line that names it"))))))))

;;; --- raw diagnostics stay logged; explicit notices alone reach the TUI ---

(deftest strip-warning-prefix-drops-cl-warn-lead-in ()
  (is-each (nle::strip-warning-prefix)
    ("WARNING: telegram delivery failed: boom" "telegram delivery failed: boom"
     "CL WARN's WARNING: prefix is chrome the notice lane already owns")
    ("Style-Warning: unused" "unused" "style-warnings strip the same way")
    ("already a notice" "already a notice" "plain text is unchanged")
    ("WARNING:   " "" "a prefix-only line collapses to empty")))

(deftest tui-error-stream-keeps-raw-diagnostics-in-the-debug-log (let ((seen '())))
  (let* ((nle::*system-notice-hook*
           (lambda (text error-p) (push (list text error-p) seen)))
         (log (make-string-output-stream))
         (stream (make-instance 'nle::tui-error-stream :fallback log)))
    (format stream "; caught 1 STYLE-WARNING condition~%; HOME-COMMAND-AT-POSITION~%WARNING: raw warning~%")
    (finish-output stream)
    (is (null seen) "raw compiler/stderr lines never become user notifications")
    (let ((text (get-output-stream-string log)))
      (is (search "STYLE-WARNING" text) "compiler diagnostics retain their debug-log evidence")
      (is (search "HOME-COMMAND-AT-POSITION" text) "continuation lines stay in the same log")
      (is (search "raw warning" text) "ordinary stderr is logged, not text-classified"))
    (nle::surface-system-notice "telegram delivery failed: status 400" :error-p t)
    (is (equal '(("telegram delivery failed: status 400" t)) seen) "explicit operator-facing action failures remain visible")))

(deftest tui-error-capture-routes-the-debugger-report-to-the-fallback ()
  ;; The disabled debugger reports on *ERROR-OUTPUT*; while a TUI is live
  ;; that is the notice lane, which the quitting process never paints. Under
  ;; the capture the report lands on the fallback stream — the sidecar log
  ;; in a shell — and the hook the capture found is restored afterwards.
  (let* ((log (make-string-output-stream))
         (seen-stream nil)
         (stub (nlk:lambda-ignoring (condition hook)
                 (setf seen-stream *error-output*))))
    (with-saved-globals ((sb-ext:*invoke-debugger-hook* stub)
                         ((sb-ext:symbol-global-value '*error-output*) log))
      (nle::call-with-tui-error-capture
       nil
       (lambda ()
         (is (not (eq sb-ext:*invoke-debugger-hook* stub)))
         (funcall sb-ext:*invoke-debugger-hook*
                  (make-condition 'simple-error :format-control "boom")
                  nil)
         (is (eq seen-stream log))
         (let ((inner sb-ext:*invoke-debugger-hook*))
           (nle::call-with-tui-error-capture nil (lambda () nil))
           (is (eq sb-ext:*invoke-debugger-hook* inner)))))
      (is (eq sb-ext:*invoke-debugger-hook* stub)))))

(deftest system-notice-msg-toasts-and-an-answer-lands-in-the-composer-status ()
  (with-frame-model (model 80 24 :session-id "warn-notice")
    (nle:update model (nle::make-system-notice-msg
                       :text "telegram delivery failed: status 400" :error-p t))
    ;; Said from another thread, it is a toast, an error one, which stays —
    ;; not the composer status, and never a row: the toast is the overlay's.
    (let ((toast (first (nle::chat-model-toasts model))))
      (is (and toast (equal "telegram delivery failed: status 400" (nle::toast-text toast))))
      (is (and toast (nle::toast-stays toast))))
    (is (null (nle::status-said model)))
    (is-let* ((rows (nle::compose-frame model)))
      ((rows-carry-p "WARNING:" rows) not) ((rows-carry-p "telegram delivery failed" rows) not))
    ;; An answer lands in the composer status, never a ■ block over it, and
    ;; the same answer again owes no frame.
    (nle:update model (nle::make-system-notice-msg
                       :text "gateway: running under systemd" :answer t))
    (is (equal "gateway: running under systemd" (nle::status-said model)))
    (is (find-if (lambda (row)
                   (and (search "gateway: running under systemd" row)
                        (not (search "■" row))))
                 (nle::compose-frame model)))
    (is (nth-value 2 (nle:update model (nle::make-system-notice-msg
                                        :text "gateway: running under systemd" :answer t))))))

(deftest a-notice-carrying-an-address-carries-a-link-span ()
  ;; The one line an operator is in the transcript to copy — the address a
  ;; cell says (web) — was painted as prose: SYSTEM-NOTICE-LLINES built
  ;; its lines with no spans at all, so nothing underlined it, no OSC-8 wrap
  ;; reached the terminal and a click landed on nothing.
  (let ((lines (nle::system-notice-llines
                (format nil "web: this machine's page:~%~
                             http://127.0.0.1:4777/web/#t=8f3c~%~
                             anyone with that whole line can drive it.")
                nil)))
    (is (= 3 (length lines)))
    (is (null (nle::lline-spans (first lines))) "prose stays prose")
    (let ((span (first (nle::lline-spans (second lines)))))
      (is span "the address line carries one span")
      (is-shape span (.link "http://127.0.0.1:4777/web/#t=8f3c") (.tone eq :link)
        (nle::mspan-underline is))
      (is (zerop span.start))
      (is (= (length (nle::lline-text (second lines))) span.end)))
    (is (null (nle::lline-spans (third lines))))))

(deftest a-failing-render-reports-once-instead-of-feeding-the-loop ()
  ;; The freeze, reproduced. BREAK-FRAGMENTS lost a parameter in THEME.LISP
  ;; while its call sites in UI.LISP still passed the old one — the window a
  ;; live cross-file load always opens, because a load is per file and an
  ;; arity is not — so every compose signalled. The program's handler made
  ;; that a system notice, the notice was a message, the message's reducer
  ;; composed, and the compose signalled again: the mailbox refilled as fast
  ;; as it drained, so MAILBOX-DRAIN's wait — the loop's only rate limit —
  ;; never armed. Two shells pinned a core each and painted nothing, and the
  ;; error never reached the operator because the lane it was written to is
  ;; the lane the wedged loop could not paint.
  ;;
  ;; A caught error is the debug log's now (NLT:REPORT-ERROR), never a
  ;; notice, and a notice paints as a toast only with popups on: they are
  ;; on here, so the notice goes through the wrap engine the stub breaks.
  (with-temp-file (nle::*shared-config-path* :setf t
                                             :contents "{\"tui\": {\"notification_popups\": true}}")
  (with-frame-model (model 80 24 :session-id "render-storm")
    (let* ((program (nlt:make-program model))
           (report program.error-handler)
           (reports 0)
           (saved-program nle::*active-tui-program*)
           (loop-thread nil))
      (setf (nlt:renderer-output-stream program.renderer)
            (make-string-output-stream)
            program.error-handler
            (lambda (where condition)
              (incf reports)
              (funcall report where condition))
            program.running t
            ;; SETF, never LET: the loop runs on its own thread and would
            ;; never see this thread's binding.
            nle::*active-tui-program* program
            loop-thread (nlk:spawn "storm-loop" (nlt:event-loop program)))
      (nlk:with-cleanup ((setf (nlt:program-running program) nil
                               nle::*active-tui-program* saved-program)
                         (ignore-errors (bt2:join-thread loop-thread)))
        (is (await () program.loop-thread))
        (with-stubbed-fdefinition
            (nle::break-fragments (text policy first-width cont-width)
             ;; Four parameters against three-argument call sites: the
             ;; shape a half-loaded cross-file change leaves in a live
             ;; image. The body never runs — the arity signals first,
             ;; and that signal is the failure.
             (list text policy first-width cont-width))
          (nlt:send program (nle::make-system-notice-msg
                             :text "a warning arriving mid-edit"))
          (is (await () (plusp reports)))
          (let ((settled reports))
            (sleep 0.25)
            (is (< (- reports settled) 5)
                (format nil "the loop settles rather than feeding itself: ~
                                  ~a further reports in a quarter second"
                        (- reports settled))))
          (is (null (sb-concurrency:list-mailbox-messages (nlt::program-mailbox program))))
          ;; The failure stays in the debug log: a report made a notice is a
          ;; message, and the message is what fed the loop.
          (is (notany (lambda (toast) (search "event-loop" (nle::toast-text toast)))
                      (nle::chat-model-toasts model))))
        ;; The shell was never dead: it kept its last good frame and its
        ;; input, and the notice paints on the first frame that can.
        (nlt:send program (nle::make-system-notice-msg
                           :text "the wrap engine is whole again"))
        (is (await () (nlk:when-let (buffer (nlt::renderer-last-buffer program.renderer))
                        (loop for y below (nlt::screen-buffer-height buffer)
                              thereis (search "the wrap engine is whole again"
                                              (with-output-to-string (out)
                                                (dotimes (x (nlt::screen-buffer-width buffer))
                                                  (let ((cell (nlt:screen-buffer-ref buffer x y)))
                                                    (when (plusp (nlt:cell-width cell))
                                                      (write-char (nlt:cell-char cell) out)))))))))))))))

;;; --- OS-level stdio severance: fd writes cannot reach the terminal -------
;;; eval capture binds special variables only, so a
;;; child process handed fd 1 raw painted over the raw-mode viewport. The
;;; severance seam moves fd 0 to /dev/null and fds 1/2 into the sidecar log
;;; while a TUI runs. fstat identity (dev . ino) is the tty-independent way
;;; to watch dup2 move the fds and put them back.

(deftest severed-stdio-moves-fd-writes-into-log-and-restores ()
  (labels ((identity-of (stat) (cons (sb-posix:stat-dev stat) (sb-posix:stat-ino stat)))
           (fd-identity (fd) (identity-of (sb-posix:fstat fd))))
    (let* ((log (format nil "/tmp/nl-stdio-sever-~a.log" (sb-posix:getpid)))
           ;; The seam skips severance without a controlling terminal (the
           ;; log must never eat the only visible output), so the test
           ;; asserts whichever contract this environment can exercise.
           (tty-p (ignore-errors (sb-posix:close (sb-posix:open "/dev/tty" sb-posix:o-rdwr)) t))
           (before (mapcar #'fd-identity '(0 1 2)))
           (inside nil))
      (nlk:with-cleanup ((ignore-errors (delete-file log)))
        (nle::call-with-severed-stdio
         (pathname log)
         (lambda ()
           (setf inside (mapcar #'fd-identity '(0 1 2)))
           ;; The incident's exact shape — a child handed fd 1 raw —
           ;; run only once fd 1 points away from the suite's own
           ;; stdout.
           (unless (equal (second inside) (second before))
             (sb-ext:run-program "/bin/sh" (list "-c" "echo LEAK-MARKER")
                                 :output (sb-sys:make-fd-stream 1 :output t)
                                 :wait t))))
        (dolist (fd '(0 1 2))
          (is (equal (nth fd before) (fd-identity fd))
              (format nil "fd ~d is restored on unwind" fd)))
        (cond (tty-p
               (is-table (file fd) (equal (identity-of (sb-posix:stat file)) (nth fd inside))
                 (log 1) (log 2) ("/dev/null" 0))
               (is-carrying (text (uiop:read-file-string log))
                 ("LEAK-MARKER" "a child handed fd 1 raw lands in the log, not the terminal")
                 (";; stdio severed: pid" "the log stamps which run's bytes follow")))
              (t (is (equal (first before) (first inside)))
                 (is (equal (second before) (second inside)))))))))

#-win32
(deftest an-exec-inside-severed-stdio-hands-its-successor-the-terminal ()
  ;; A successor exec'd from behind a TUI's frame inherits fds 0, 1 and 2:
  ;; without the exec putting the originals back first, its resume line and
  ;; `could not start' would go to the log, not the terminal. The exec
  ;; puts the originals back first, then severs again if the kernel refuses it.
  (labels ((fd-identity (fd) (let ((stat (sb-posix:fstat fd)))
                               (cons (sb-posix:stat-dev stat) (sb-posix:stat-ino stat))))
           (identities () (mapcar #'fd-identity '(0 1 2))))
    (let ((before (identities))
          (inside nil) (unsevered nil) (refused nil) (after-refusal nil))
      (with-temp-directory (dir "unsever")
        (nle::call-with-severed-stdio
         (merge-pathnames "history.db.stdio.log" dir)
         (lambda ()
           (setf inside (identities)
                 unsevered (nlk::call-with-unsevered-stdio #'identities)
                 refused (nlk::exec-program #p"/nonexistent/nodecode-exec-probe" (list "probe"))
                 after-refusal (identities)))))
      (is (equal before unsevered) "the process's own descriptors, for the exec")
      (is (search "No such file" refused) "the kernel refused this one")
      (is (equal inside after-refusal) "and the severance stands again")
      (is (null nlk::*severed-stdio*) "nothing is severed once the shell is gone"))))


;;; --- the truecolor probe reply -------------------------------------------
;;; The shell asks the terminal itself (ssh strips COLORTERM, so the
;;; environment can only guess): a confirming reply upgrades the paint, a
;;; no changes nothing, and postures the probe must not touch stay put.

(deftest truecolor-reply-upgrades-the-live-profile (with-chat-model (model "c2-truecolor"))
  (loop for (profile ok-p expected why) in '((:ansi256 t :truecolor "a confirming reply upgrades the environment's guess")
                                             (:ansi256 nil :ansi256 "a no keeps the guess: silence and refusal never downgrade")
                                             (:truecolor t :truecolor "truecolor stays truecolor")
                                             (:none t :none "a colorless posture ignores the probe"))
        do (let ((nle:*color-profile* profile))
             (nle:update model (nlt:make-truecolor-msg :ok-p ok-p))
             (is (eq expected nle:*color-profile*) why))))

;;; --- the executable's command line ---------------------------------------

(defun launch-plan (&rest args)
  ;; the first seven values; the eighth, --pristine, has its own line below
  (subseq (multiple-value-list (nle::parse-launch-arguments args)) 0 7))

(defun launch-refusal (&rest args)
  (refusal-text nle::launch-usage-error (nle::parse-launch-arguments args)))

(deftest launch-arguments-parse-and-refuse ()
  (is (equal '(nil nil nil nil nil nil nil) (launch-plan)) "bare: attach to or own the default store")
  (is (equal '(t nil nil "/tmp/x.db" nil nil nil) (launch-plan "serve" "--store" "/tmp/x.db")))
  (is (equal '(nil nil "s-1" nil nil nil nil) (launch-plan "resume" "s-1")))
  (is (equal '(nil (:trial) nil nil nil nil nil) (launch-plan "trial")))
  (is (equal '(nil (:warm-dump "/p") nil nil nil nil nil) (launch-plan "warm-dump" "/p")))
  (is (equal '(nil (:warm) nil nil nil nil nil) (launch-plan "warm")))
  (is (equal '(nil nil nil nil t nil nil) (launch-plan "--ephemeral")) "ephemeral: a store of the shell's own")
  (is (equal '(nil nil nil nil nil nil nil t) (multiple-value-list (nle::parse-launch-arguments '("--pristine")))) "pristine: the floor by hand")
  (is (equal '(t nil nil nil nil nil nil t) (multiple-value-list (nle::parse-launch-arguments '("serve" "--pristine")))) "and a serve stands on it too")
  (is-table (in in2) (search in in2) ("no option \"--bogus\"." (launch-refusal "--bogus"))
    ("needs a PATH" (launch-refusal "--store")) ("needs a SESSION-ID" (launch-refusal "resume"))
    ("cannot resume" (launch-refusal "--ephemeral" "resume" "s-1"))
    ("shell's flag" (launch-refusal "serve" "--ephemeral"))
    ("chooses the store" (launch-refusal "--ephemeral" "--store" "/tmp/x.db")))
  (let ((path (nle::ephemeral-store-path 4242)))
    (is-shape path (pathname-name "nodecode-ephemeral-4242" "named by the process")
      (pathname-type "db") (pathname-directory (pathname-directory (uiop:temporary-directory))))))

;;; --- a recorded exchange rests on its own divider ---------------------------

(deftest recorded-exchange-rests-on-a-recorded-divider ()
  ;; The durable fold: a pair written whole by RECORD-EXCHANGE-TURN ran no
  ;; provider round, so its divider says `recorded', never `0.0s completed'.
  (with-temp-store ()
    (let ((sid "recorded-divider"))
      (nlk:create-session :id sid :cwd "/tmp")
      (nlk:record-exchange-turn sid "[experience] recap of the turn above" "the recap")
      (nlk:record-exchange-turn sid "[experience] recap of turn 2" "the second recap"
                                :facts '(:recorder "reflection" :provider "a6api" :model "glm-5.3-flash"
                                         :elapsed-ms 18400 :input-tokens 3562 :cached-input-tokens 6528
                                         :output-tokens 205 :reasoning-tokens 58))
      (let* ((rows (nle::transcript-rows (nle:fold-log :session-id sid)))
             (dividers (remove :finish-divider rows :key #'nle::transcript-row-kind :test-not #'eq)))
        (is (= 6 (length rows)) "user, assistant, divider, twice")
        (is (= 2 (length dividers)))
        ;; Neither input was asked here: the row says so, and who recorded it when known.
        (is (equal '(t "reflection")
                   (mapcar #'nle::transcript-row-recorded
                           (remove "user" rows :key #'nle::transcript-row-role :test-not #'equal))))
        (is-present (bare (first dividers)) "the pair without facts"
          ;; Reads recorded, and the clock its fact landed at — nothing else.
          (is (ppcre:scan "\\A• recorded \\(\\d\\d?:\\d\\d [AP]M\\)\\z" (nle::transcript-row-text bare)))
          (is (not (search "completed" (nle::transcript-row-text bare)))))
        (is-present (told (second dividers)) "the pair with facts"
          (let ((text (nle::transcript-row-text told)))
            ;; Recorder, lane, the reflection's own elapse, the outcome word,
            ;; and the wall clock: its meter is the one thing not repeated here.
            (is (uiop:string-prefix-p "• reflection · a6api/glm-5.3-flash · 18.4s recorded " text))
            (is (ppcre:scan
                 "\\A• reflection · a6api/glm-5.3-flash · 18\\.4s recorded \\(\\d\\d?:\\d\\d [AP]M\\)\\z"
                 text))
            (is (not (search "↑" text))))))))
  ;; The gateway stream converges: the input's recorded disposition marks
  ;; the turn, its completion rests on the same divider, and the operator's
  ;; next turn rests on the timed one.
  (flet ((fact (type turn-id &rest fields)
           (apply #'nlk:make-json-object "type" type "turn_id" turn-id
                  "metadata" (nlk:json-object "occurred_at" "2026-09-07T00:00:01.000Z")
                  fields)))
    (let ((state nil)
          (clock (nle::format-clock-minute (nle::iso-8601-ms "2026-09-07T00:00:01.000Z"))))
      (feed-events state
        (fact "turn.input_committed" "t1" "role" "user"
              "message" "[experience] recap" "disposition" "recorded")
        (fact "turn.started" "t1")
        (fact "turn.assistant_message_completed" "t1"
              "message" (nlk:json-object "role" "assistant" "content" "the recap"))
        (fact "turn.completed" "t1" "recorder" "reflection"
              "provider" "a6api" "model" "glm-5.3-flash" "elapsed-ms" 18400
              "input-tokens" 3562 "cached-input-tokens" 6528
              "output-tokens" 205 "reasoning-tokens" 58)
        (fact "turn.input_committed" "t2" "role" "user"
              "message" "next" "disposition" "started")
        (fact "turn.started" "t2")
        (fact "turn.assistant_message_completed" "t2"
              "message" (nlk:json-object "role" "assistant" "content" "answer"))
        (fact "turn.completed" "t2"))
      (let ((dividers (remove :finish-divider (nle::transcript-rows state)
                              :key #'nle::transcript-row-kind :test-not #'eq)))
        (is (= 2 (length dividers)) "one divider per turn")
        ;; The operator's next prompt is theirs.
        (is (equal '("reflection" nil)
                   (mapcar #'nle::transcript-row-recorded
                           (remove "user" (nle::transcript-rows state)
                                   :key #'nle::transcript-row-role :test-not #'equal))))
        (is (equal (format nil "• reflection · a6api/glm-5.3-flash · 18.4s recorded (~a)" clock)
                   (nle::transcript-row-text (first dividers))))
        ;; The operator's own turn rests on the same shape — its elapse the
        ;; difference between the fixture's two logged instants (0.0s: the
        ;; start and the terminal fact share one).
        (is (equal (format nil "• 0.0s completed (~a)" clock)
                   (nle::transcript-row-text (second dividers))))))))

(deftest launch-import-verb-reads-its-flags ()
  ;; `nodecode import [WORLD] ...' asks the first frame for the wizard's
  ;; import stage alone, its flags on the request; the launch's own flags
  ;; still parse after it, naming no world reads every home on the box, and
  ;; a world this build does not know is refused by name.
  (flet ((request (&rest args)
           (nth-value 5 (nle::parse-launch-arguments args))))
    (is-shape (request "import" "hermes") (:import "hermes") (:dry-run null))
    (is (eq t (getf (request "import") :import)))
    (is (eq t (getf (request "import" "--dry-run") :import)))
    (let ((full (request "import" "codex" "--source" "/tmp/h" "--only" "sessions,memory"
                         "--dry-run" "--overwrite" "--keep-running" "--yes" "--ephemeral")))
      (is-shape full (:source "/tmp/h") (:only "sessions,memory"))
      (is (and (getf full :dry-run) (getf full :overwrite) (getf full :keep-running) (getf full :yes))))
    (is (nth-value 4 (nle::parse-launch-arguments '("import" "hermes" "--ephemeral"))))
    (is (search "needs a PATH" (launch-refusal "import" "hermes" "--source")))
    (is (search "import is a shell's verb" (launch-refusal "serve" "import" "hermes")))))

(deftest launch-gateway-verb-is-the-operating-systems-and-run-is-the-gateway ()
  ;; `nodecode gateway ...' answers before any folder loads, like a
  ;; profile's verbs; `gateway run' is the gateway itself: the headless
  ;; owner that waits for the store instead of losing it.
  (is (equal '(nil (:gateway nil) nil nil nil nil nil) (launch-plan "gateway")) "bare: the state")
  (is (equal '(nil (:gateway ("on")) nil nil nil nil nil) (launch-plan "gateway" "on")))
  (is (equal '(nil (:gateway ("logs" "--follow")) nil nil nil nil "coder")
             (launch-plan "-p" "coder" "gateway" "logs" "--follow")))
  (is (equal '(nil (:gateway ("--help")) nil nil nil nil nil) (launch-plan "gateway" "--help")))
  (is (equal '(:wait nil nil nil nil nil nil) (launch-plan "gateway" "run")))
  (is (equal '(:wait nil nil "/tmp/x.db" nil nil nil) (launch-plan "gateway" "run" "--store" "/tmp/x.db")))
  (is (search "shell's flag" (launch-refusal "gateway" "run" "--ephemeral")))
  (is (search "gateway is a command of its own" (launch-refusal "--ephemeral" "gateway" "on")))
  (is (search "profile is a command of its own" (launch-refusal "--ephemeral" "profile" "list")))
  (is (search "usage: nodecode gateway [VERB]" (nle::launch-help-text "gateway")))
  (is (search "gateway [VERB]" (nle::launch-help-text)) "and --help lists it"))

(deftest workspace-memo-is-forgotten-before-a-dump ()
  ;; The cwd memo is a fact about one launch, and warm.lisp renders a frame
  ;; as the last act of the load -- so the dump carried the BUILD directory:
  ;; `just release' runs from src/, and every shipped binary showed the
  ;; builder's own `.../src' in its footer, on every box, forever. Caught by
  ;; unpacking a release tarball in a container, where the footer read a
  ;; packer's throwaway `/tmp/tmp.XXXXXXXX/src'.
  (is (member 'nle::forget-workspace-memos sb-ext:*save-hooks*))
  (let ((nle::*workspace-footer-label* "~/the/builders/tree")
        (nle::*workspace-directory* "/the/builders/tree"))
    (nle::forget-workspace-memos)
    (is (null nle::*workspace-footer-label*) "the label memo is dropped")
    (is (null nle::*workspace-directory*) "and so is the one a click resolves against")
    (is (plusp (length (nle::workspace-footer-label))))
    (is (plusp (length (nle::workspace-directory))))))

(deftest a-failed-call-draws-failed-live-and-on-replay (with-temp-store ())
  ;; Without this, every failed call would draw as a success: no fold would
  ;; pass the failure, and the fact would have none to pass. The fact's own
  ;; "failed" reaches the row through both folds -- the durable log a resume
  ;; reads, the wire frame the live stream carries -- and the card's title
  ;; ends " failed" in the danger tone, as the web page's row says "error"
  ;; (its failed mark); the call that worked stays plain.
  (with-durable-turn (turn (ensure-durable-session "failed-card") :input "fail" :complete t)
    (record-tool-use turn "c1" "eval" "{\"form\":\"(car 5)\"}" "ERROR: TYPE-ERROR: 5 is not a list"
                     :failed t)
    (record-tool-use turn "c2" "eval" "{\"form\":\"(+ 1 2)\"}" "3"))
  (flet ((check (state how)
           (destructuring-bind (failed worked)
               (remove :tool-call (nle::transcript-rows state)
                       :key #'nle::transcript-row-kind :test-not #'eq)
             (let ((title (first (nle::tool-header-llines failed))))
               (is (nle::transcript-row-error-p failed) how)
               (is (uiop:string-suffix-p (nle::lline-text title) " failed") how)
               (is (not (search "[EVAL-FAILED]" (nle::lline-text title))) "no stand-in code repeats the word")
               (is (eq :danger (span-tone-at (nle::lline-spans title)
                                             (1- (length (nle::lline-text title)))))
                   how))
             (let ((json (nle::row-json failed 1 nil)))
               (is (eq t (gethash "error" json)) how)
               (is (equal "error" (gethash "status" (gethash "tool" json))) how))
             (is (not (nle::transcript-row-error-p worked)) how)
             (is (not (search "failed" (nle::lline-text (first (nle::tool-header-llines worked)))))
                 how))))
    (check (nle:fold-log :session-id "failed-card") "the durable fold")
    (let ((state nil))
      (dolist (event (nle::collect-visible-session-events "failed-card"))
        (when (member event.kind '("turn.tool_call_started" "turn.tool_result") :test #'equal)
          (feed-events state (gethash "event" (nle::accumulated-event-frame "failed-card" event)))))
      (check state "the wire fold"))))

;;; --- markdown the TUI draws itself ------------------------------------------
;;; The TUI's own Markdown pass behind the web page's: pipes that split a
;;; cell, ***both***, escapes printed with their backslash,
;;; the ``` lines drawn, a diff fence in plain text.

(defun shown-text (markdown)
  "MARKDOWN's first line as its plain row paints it, the gutter trimmed."
  (string-trim " " (first (nth-value 1 (nle::lline-rows (first (nle::markdown-llines markdown))
                                                         200)))))

(deftest markdown-table-cells-keep-escaped-and-code-pipes ()
  (is-each (nle::table-row-cells)
    ("| a \\| b | `x | y` | `p \\| q` |" '("a | b" "`x | y`" "`p | q`")
     "an escaped pipe and a code span's pipe stay in their cells")
    ("a | b" '("a" "b") "the outer pipes are optional")
    ("| x \\\\| y |" '("x \\\\" "y") "an escaped backslash leaves the pipe a divider")
    ("|" '("") "an empty row still has its one empty cell"))
  (let ((lines (nle::markdown-llines
                (format nil "| case | source |~%|---|---|~%| escaped | a \\| b |~%| code | `x | y` |"))))
    (is (= 3 (length lines)) "header + two rows: the band is the header's")
    (is (equal "escaped │ a | b" (second (line-texts lines))))
    ;; A cell's code marks leave the grid, so its columns line up, and the
    ;; cell keeps its code tone.
    (is (equal "code    │ x | y" (third (line-texts lines))))
    (is (eq :inline-code (span-tone-at (nle::lline-spans (third lines)) 10)))))

(deftest markdown-triple-emphasis-is-strong-and-italic (let* ((text "and ***both*** here")
                                                              (spans (nle::inline-spans text 0))))
  (is-present (both (find-if #'nle::mspan-italic spans)) "the run is emphasised"
    (is (equal "both" (subseq text (nle::mspan-start both) (nle::mspan-end both))))
    (is-shape both
      (nle::mspan-bold is "and strong")
      (nle::mspan-tone eq :inline-strong)))
  (is (equal "and both here" (shown-text text)) "no stray star either side"))

(deftest markdown-backslash-escapes-print-the-character ()
  (is-each (shown-text)
    ("2 \\* 3 = 6" "2 * 3 = 6" "an escaped star opens no emphasis")
    ("\\_not em\\_" "_not em_" "nor an escaped underscore")
    ("\\`not code\\`" "`not code`" "an escaped backtick opens no code span")
    ("a \\| b" "a | b" "an escaped pipe")
    ("back\\\\slash" "back\\slash" "an escaped backslash is one backslash")
    ("\\# not a heading" "# not a heading" "an escaped # heads nothing")
    ("*em \\* still em*" "em * still em" "an escaped star neither closes a run nor shows its backslash")
    ("`a\\*b`" "a\\*b" "inside code a backslash is literal")
    ("C:\\Users" "C:\\Users" "a backslash before a letter stays")))

(deftest markdown-fence-markers-hide-and-the-language-labels-the-block ()
  (let ((lines (md-lines "Run:~%~%```sh~%just release~%```~%~%Done.")))
    (is (notany (lambda (line) (search "```" (nle::lline-text line))) lines) "no ``` line is drawn")
    (let ((label (find "sh" lines :key #'nle::lline-text :test #'equal))
          (body (find "just release" lines :key #'nle::lline-text :test #'equal)))
      (is-present label "the language labels the block"
        (is (eq :muted (nle::lline-tone label)) "in the dim role"))
      (is-present body "the body is drawn as it was written"
        (is (eq :code (nle::lline-role body)) "on the slab, beside its edge")
        (is (equal "   ▌ just release" (first (nth-value 1 (nle::lline-rows body 80))))))))
  ;; A fence with no language draws its body alone, at the list's indent.
  ;; The bullet is a dot; the slab's blank rows (its label row, its last,
  ;; the one standing it off the step) carry no words.
  (let ((lines (md-lines "- step~%   ```~%   plain~%   ```")))
    (is (equal '("• step" "   plain")
               (remove "" (mapcar #'nle::lline-text lines) :test #'equal)))))

(deftest markdown-diff-fences-take-the-diff-tones ()
  (let ((lines (md-lines "```diff~%@@ -1,2 +1,2 @@~% (keep)~%-(old)~%+(new)~%```")))
    (is (equal "diff" (nle::lline-text (first lines))))
    ;; The hunk header dim, context plain, - removed, + added.
    ;; the slab's blank last row is no line of the diff.
    (is (equal '(:muted :foreground :diff-removed :diff-added)
               (mapcar #'nle::lline-tone (butlast (rest lines)))))))

(deftest a-code-block-stands-a-blank-row-off-what-touches-it ()
  ;; Without a blank row, a fence written straight under a paragraph would
  ;; put the block's ground edge to edge with the words above it. A blank
  ;; row stands between a code block and whatever touches it, above and
  ;; below, and a blank already written is that row, never a second.
  (flet ((shape (control)
           (mapcar (lambda (line) (list (nle::lline-role line) (nle::lline-text line)))
                   (md-lines control))))
    (is (equal '((:prose "Blocked:") (:prose "") (:code "") (:code "rm -rf x") (:code "")
                 (:prose "") (:prose "Then."))
               (shape "Blocked:~%```~%rm -rf x~%```~%Then.")) "a blank row above the block's first row and below its last")
    (is (equal (shape "Blocked:~%~%```~%rm -rf x~%```~%~%Then.")
               (shape "Blocked:~%```~%rm -rf x~%```~%Then.")) "a written blank is the gap, never a second one")
    (is (equal '((:code "") (:code "a") (:code "") (:prose "") (:code "") (:code "b") (:code ""))
               (shape "```~%a~%```~%```~%b~%```")) "two blocks stand apart, and the body's own edges take no gap")
    (is (equal '(:prose "") (second (shape "# Head~%```~%x~%```"))) "a heading stands off too"))
  ;; Streaming re-parses from the last blank line on: the gap a closing
  ;; fence owes the next line comes out the same as in a whole parse.
  (let* ((whole (format nil "Intro.~%~%Blocked:~%```~%rm -rf x~%```~%Then."))
         (buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (row (nle::make-transcript-row :kind :text :role "assistant" :text buffer)))
    (flet ((grow (upto)
             (loop while (< (length buffer) upto)
                   do (vector-push-extend (char whole (length buffer)) buffer))
             (line-texts (nle::row-markdown-llines row buffer :width 80))))
      (grow (1- (search "Then." whole)))
      (is (nle::md-cache-cp-offset (nle::row-md-cache row)) "the next parse reuses the head")
      (is (equal (line-texts (md-lines whole)) (grow (length whole))) "the incremental parse draws the gap the whole parse does"))))

(deftest home-profile-path-keeps-its-last-folder ()
  (is-each (nle::clipped-path)
    ("~/nc" 20 "~/nc" "a path that fits is whole")
    ("/tmp/a-long-scratch-folder/campaign/markdown/home/nc" 30 "/…/campaign/markdown/home/nc"
     "cut from the middle, whole folders kept from the end")
    ("~/work/one/two/three/leaf" 16 "~/…/three/leaf" "the home's ~ stays at the head")
    ("/a/an-unusually-long-last-folder" 12 "/…ast-folder" "a last folder wider than the room shows its end"))
  (let* ((row (with-saved-globals ((nodecode.kernel::*home* #p"/tmp/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/profile-leaf/"))
                (assoc "/profile" (nle::home-meta-rows 80) :test #'string=)))
         (value (second row)))
    (is (<= (nle::text-display-width value) (nle::home-meta-value-width 80)) "the row keeps to the value lane")
    (is (search "profile-leaf · /" value) "the active home is named even when its path must clip")
    (is (uiop:string-suffix-p value "le-leaf") "the path keeps its last folder's tail")))

;;; --- a reflection's recap is one quiet line ---------------------------------

(deftest a-reflection-recap-folds-to-one-line-that-opens ()
  ;; The experience cell records a recap behind every tool turn: a question
  ;; no one here asked, its answer and its divider, nine rows under the
  ;; operator's answer. Closed it is one line under the turn it recaps; a click
  ;; opens it on its answer and its divider, its question still out of sight.
  ;; Standard hides the whole recap; Developer keeps the clickable quiet line.
  ;; Switching viewports changes projection only, not the recorded facts or the
  ;; open accordion. A pair from another recorder still draws whole.
  (with-frame-model (model 100 40 :session-id "recap-fold" :tool-detail :developer)
    (nle::render-state model)
    (paint-model model
      (exchange "fix the title" "Fixed it." :turn-id "t1")
      (nle::ingest-turn-terminal "completed" :turn-id "t1")
      (nle::ingest-message-fact "user" "[experience] recap of the turn above" :turn-id "t2" :recorded t)
      (nle::ingest-message-fact "assistant" (format nil "Recap: the title was fixed.~%Nothing to carry forward.")
                                :turn-id "t2")
      (nle::ingest-turn-terminal "completed" :turn-id "t2" :facts '(:recorder "reflection"))
      (nle::ingest-message-fact "user" "imported ask" :turn-id "t3" :recorded t)
      (nle::ingest-message-fact "assistant" "3" :turn-id "t3")
      (nle::ingest-turn-terminal "completed" :turn-id "t3" :facts '(:recorder "claude-code")))
    (flet ((rows () (map 'list (lambda (row) (string-trim " " row)) model.line-plain))
           (recap-line () (position-if (lambda (ref) (and (nle::tool-group-p ref)
                                                           (eq :recap (first (nle::tool-group-key ref)))))
                                       model.line-rows))
           (ctrl-o ()
             (press model #\o :mod nlt:+mod-ctrl+) (nle::render-state model) model.tool-detail))
      (let ((rows (rows)))
        (is (member "Reflection recap · Recap: the title was fixed. Nothing to carry forward." rows :test #'equal))
        (is (not (rows-carry-p "[experience]" rows)) "its question is out of sight")
        (is (not (rows-carry-p "• reflection" rows)) "and its divider, while it is closed")
        (is (= 3 (count-if (lambda (ref) (eq ref (aref model.line-rows (recap-line)))) model.line-rows)) "one line and its pads")
        (is (rows-carry-p "imported ask" rows) "an import's pair draws whole"))
      (click-at model 6 (mouse-y-for-line model (1+ (recap-line))))
      (is (equal '(:recap "t2") model.expanded-tool-call))
      (let ((rows (rows)))
        (is (member "Reflection recap" rows :test #'equal) "open, the line drops its words")
        (is (rows-carry-p "Recap: the title was fixed." rows) "for the answer under it")
        (is (not (rows-carry-p "[experience]" rows))))
      ;; Switching with the accordion open must hide the entire recap too.
      (is (eq :standard (ctrl-o)))
      (dolist (expanded '(nil (:recap "t2")))
        (rendering model (setf model.expanded-tool-call expanded))
        (let ((rows (rows)))
          (is (not (rows-carry-p "Reflection recap" rows)) "Standard hides the recap header even if expanded")
          (is (not (rows-carry-p "Recap: the title was fixed." rows)) "and its answer")
          (is (not (rows-carry-p "[experience]" rows)) "and its recorded question")
          (is (not (rows-carry-p "• reflection" rows)) "and its divider")
          (is (null (recap-line)) "no recap-owned lines or padding remain")
          (is (rows-carry-p "Fixed it." rows) "the operator's answer remains")
          (is (rows-carry-p "imported ask" rows) "an unrelated recorder still draws whole")))
      (is (eq :developer (ctrl-o)))
      (let ((rows (rows)))
        (is (member "Reflection recap" rows :test #'equal) "Developer restores the open accordion")
        (is (rows-carry-p "Recap: the title was fixed." rows) "its retained answer is unchanged")
        (is (not (rows-carry-p "[experience]" rows)) "its question still stays out of sight"))
      (click-at model 6 (mouse-y-for-line model (1+ (recap-line))))
      (is (null model.expanded-tool-call))
      (is (member "Reflection recap · Recap: the title was fixed. Nothing to carry forward." (rows) :test #'equal) "the restored Developer recap still closes"))))

(deftest ctrl-c-stops-a-running-turn-and-quits-only-an-idle-shell ()
  ;; Without this, Ctrl+C would quit the shell mid-turn and lose the turn.
  ;; It clears a composer that holds text first, then stops a running turn
  ;; with the very cancel Esc sends, and quits only an idle shell with
  ;; nothing to clear.
  (with-frame-model (model 80 24 :session-id "ctrl-c")
    (let ((cancels '()))
      (with-stubbed-fdefinitions ((nle::send-cancel-turn (ws session-id turn-id &key retract)
                                    (push (list session-id turn-id retract) cancels)))
        (flet ((ctrl-c ()
                 ;; Whether the press answered the quit command.
                 (let ((cmd (nth-value 1 (nle:update model (nlt:make-key-press-msg
                                                            :code #\c :mod nlt:+mod-ctrl+)))))
                   (and cmd (nlt:quit-msg-p (funcall cmd))))))
          (setf model.turn-in-flight t
                (nle::gateway-model-active-turn-id model) "turn-live")
          (type-string model "draft")
          (is (not (ctrl-c)) "with text in the composer it never quits")
          (is (equal "" (composer-value model)) "it clears the text first")
          (is (null cancels) "and leaves the turn running")
          (is (not (ctrl-c)) "on an empty composer mid-turn it does not quit")
          (is (equal '(("ctrl-c" "turn-live" nil)) cancels) "it cancels the running turn")
          (press model :escape)
          (is (equal (first cancels) (second cancels)) "the cancel Esc sends")
          (nle::open-help-dialog model)
          (is (not (ctrl-c)) "a dialog open over the turn does not make it quit")
          (is (= 3 (length cancels)))
          (nle::dismiss-dialog model nil)
          (nle::note-turn-flight-settled model)
          (is (ctrl-c) "idle with an empty composer it quits")
          (is (= 3 (length cancels)) "and cancels nothing"))))))

;;; --- the TUI's own Markdown: strike, task boxes, quotes, headings, breaks ---
;;; Without this, ~~tildes~~, `- [x]', `>' marks and heading hashes would print
;;; as typed, every heading level alike.

(deftest markdown-strike-draws-struck-through (let ((nle::*color-profile* :truecolor)))
  (let* ((text "Some ~~struck~~ text")
         (struck (find-if #'nle::mspan-strike (nle::inline-spans text 0))))
    (is-present struck "the run is struck"
      (is (equal "struck" (subseq text (nle::mspan-start struck) (nle::mspan-end struck)))))
    (is (equal "Some struck text" (shown-text text)) "the tildes hide")
    ;; SGR 9 reaches the cells over the run and nowhere else.
    (let ((buffer (nlt:parse-styled-string
                   (nle::lline-rows (first (nle::markdown-llines text)) 40) 40 1)))
      (is (equal '(nil t nil)
                 (loop for x in '(3 8 15)
                       collect (logtest nlt::+attr-strikethrough+
                                        (nlt:cell-attrs (nlt:screen-buffer-ref buffer x 0)))))))))

(deftest markdown-task-items-draw-a-box-and-a-done-one-dims ()
  (let ((lines (md-lines "- [x] lint green~%- [ ] macOS notarized~%  - [X] nested done~%- plain")))
    ;; The box stands where the marker stood; a plain item keeps its bullet.
    (is (equal '("▣ lint green" "□ macOS notarized" "  ▣ nested done" "• plain")
               (mapcar #'nle::lline-text lines)))
    (is (equal '(t nil t nil) (mapcar #'nle::lline-dim lines)) "a done task dims whole")
    (is (= 2 (nle::lline-hang (second lines))) "a wrapped task hangs past its box")
    ;; An open box in the marker's tone, a done one in the muted tone.
    (is (equal '(:markdown-line :list-mark)
               (mapcar (lambda (line) (span-tone-at (nle::lline-spans line) 0)) (subseq lines 0 2))))))

(deftest markdown-quotes-draw-a-bar-per-level ()
  (let ((lines (md-lines "> one **strong**~%>~%> > two")))
    ;; A bar per level, the > marks gone, the text in the muted tone.
    (is (equal '("▌ one **strong**" "▌ " "▌ ▌ two") (mapcar #'nle::lline-text lines)))
    (is (equal "▌ one strong" (shown-text "> one **strong**")))
    (is (every (lambda (line) (eq :muted (nle::lline-tone line))) lines)))
  ;; A long quoted line wraps with both bars down every row, at the prose
  ;; column, the text under the text.
  (let ((plain (nth-value 1 (nle::lline-rows
                             (first (nle::markdown-llines
                                     (format nil "> > ~{word~a~^ ~}" (alexandria:iota 20))))
                             40))))
    (is (> (length plain) 2) "the line wraps")
    (is (every (lambda (row) (eql 3 (search "▌ ▌ word" row))) plain))))

(deftest markdown-heading-levels-draw-apart-without-hashes ()
  (let ((lines (md-lines "# One~%## Two~%### Three `x`~%#### Four ##~%###### C#")))
    ;; The hashes hide, a closing run with them; a # inside a word stays.
    (is (equal '("One" "Two" "Three `x`" "Four" "C#") (mapcar #'nle::lline-text lines)))
    ;; # bold and underlined on the title band, ## bold on the heading band,
    ;; ### bold in the cool hue, deeper in that hue alone; none dims.
    (is (equal '((t t nil :heading :title-band) (t nil nil :heading :heading-band)
                 (t nil nil :subheading nil) (nil nil nil :subheading nil)
                 (nil nil nil :subheading nil))
               (mapcar (lambda (line)
                         (list (nle::lline-bold line) (nle::lline-underline line) (nle::lline-dim line)
                               (nle::lline-tone line) (first (nle::lline-ground line))))
                       lines))))
  (is (equal "Three x" (shown-text "### Three `x`")) "a heading takes the inline pass"))

(deftest markdown-every-newline-breaks-and-a-hard-breaks-backslash-hides ()
  ;; A soft break keeps its row, as the web page's <br> does. A backslash
  ;; ending a line the paragraph goes on after is a hard break and hides; a
  ;; paragraph's last line and an escaped backslash keep theirs.
  (let ((lines (md-lines "one~%two  ~%three\\~%four\\~%~%five\\\\~%six\\")))
    (is (equal '("one" "two" "three" "four\\" "" "five\\" "six\\")
               (mapcar (lambda (row) (string-trim " " row))
                       (nth-value 1 (nle::llines-rows lines 80)))))))

;;; --- a panel row that does not fit clips where it says the most -------------

(deftest sessions-panel-drops-whole-hints-and-keeps-the-last-folder ()
  ;; The /sessions hint row was cut mid-word at the right edge, `ctrl+f
  ;; expan', and the selected session's folder lost its tail, the part that
  ;; names it. Hints that do not fit drop whole from the end of
  ;; the list, and the folder is cut from its middle to the room the row
  ;; leaves (CLIPPED-PATH). The keys are caps, which paint as ` key ' bare.
  (let ((model (make-instance 'nle:gateway-chat-model :ws nil :session-id "dlg-clip")))
    (setf model.term-height 40)
    (nle::open-picker-dialog
     model "Sessions"
     (picker-rows '("hi" :detail "s-3KIWR4SB  1m ago  49 blocks"
                    :path "/tmp/a-long-scratch-folder/campaign/clip/project" :value "s-1")
                  '("other" :detail "s-2" :path "/tmp/elsewhere" :value "s-2"))
     nil
     :footer-hints :sessions)
    (flet ((rows (width) (nle::dialog-panel-rows model width nil)))
      (flet ((hints (width) (string-right-trim " " (nth (- (length (rows width)) 2) (rows width)))))
        (is (uiop:string-suffix-p (hints 89) "f2  rename   ctrl+f  full screen") "every hint where they all fit")
        (is (uiop:string-suffix-p (hints 88) "→  forks   f2  rename") "one column short: the last drops whole")
        (is (uiop:string-suffix-p (hints 60) "tab  this folder/all   →  forks") "and on 60 columns, the three that fit")
        (is (notany (lambda (width) (search "…" (hints width))) '(60 88 89)) "none of them cut"))
      (let ((selected (nth 5 (rows 80)))
            (other (nth 6 (rows 80))))
        (is (search "49 blocks  /…/campaign/clip/project" selected) "the path keeps its last folders")
        (is (not (search " … " selected)) "and the row fits without a cut")
        (is (= 80 (nle::text-display-width selected)))
        (is (not (search "/tmp/elsewhere" other)) "a row's path is the selected row's chrome")
        (is (search "49 blocks  /tmp/a-long-scratch-folder/campaign/clip/project" (nth 5 (rows 140))) "whole where it fits")))))

;;; --- tables and code blocks the TUI draws itself ----------------
;;; Without this, an alignment row would read as nothing, a table would
;;; widen under every token while it streams, a bare ``` block would paint as
;;; keywords, and a wrapped code line would carry nothing saying it goes on.

(defun table-texts (text &rest args)
  "The texts MARKDOWN-LLINES draws TEXT as under ARGS, each right-trimmed."
  (line-texts (apply #'nle::markdown-llines text args)))

(deftest markdown-table-columns-stand-as-the-alignment-row-says ()
  (is-each (nle::table-alignments)
    ("|:--|--:|:-:|---|" '(:left :right :center :left) "left, right, centre, and left by default")
    ("| :---: | ---: |" '(:center :right) "spaces round the dashes change nothing"))
  ;; --: pads on the left, :-: on both sides (the odd column after), the
  ;; header as its column; a cell's spans move with its text.
  (let ((lines (nle::markdown-llines
                (format nil "| name | qty | mark |~%|:--|--:|:-:|~%| apple | 3 | ok |~%| pear | `12` | wait |"))))
    (is (equal '("name  │ qty │ mark" "apple │   3 │  ok" "pear  │  12 │ wait")
               (line-texts lines)))
    (is (null (span-tone-at (nle::lline-spans (third lines)) 8)) "the pad before 12 is bare")
    (is (eq :inline-code (span-tone-at (nle::lline-spans (third lines)) 9)) "and 12 keeps its code tone")))

(deftest a-streaming-table-draws-only-its-whole-rows ()
  ;; While a message arrives, a table's row still being typed widened its
  ;; columns token by token and the whole table jumped: what is still
  ;; arriving waits, so the table lays out once per row.
  (let ((typing (format nil "| Lane | p50 |~%|---|---|~%| a | 1 |~%| openai-completions | 4")))
    (is-each (table-texts)
      (typing :arriving t '("Lane │ p50" "a    │ 1") "the row being typed waits and widens no column")
      ((format nil "Lanes:~%| Lane | p5") :arriving t '("Lanes:") "a header being typed waits")
      ((format nil "Lanes:~%| Lane | p50 |~%|-") :arriving t '("Lanes:")
       "and then its separator, so it never shows as a line of pipes")
      ((format nil "Lanes:~%| Lane | p50 |~%|---|--") :arriving t '("Lanes:" "Lane │ p50")
       "once the separator reads as one, the header draws on its band")
      ("Some prose | a pipe, still arriv" :arriving t '("Some prose | a pipe, still arriv") "prose never waits"))
    (is (= 3 (length (table-texts typing))) "arrived, every row draws")
    ;; The seam: an assistant row no fact owns yet is a stream still arriving.
    (flet ((drawn (&rest fields)
             (length (nle::row-llines (apply #'nle::make-transcript-row :kind :text :role "assistant"
                                                                         :text typing fields)
                                      100))))
      (is (= 3 (drawn)) "a streaming row holds its typed row back")
      (is (= 4 (drawn :hydrated t)) "the durable fact draws it")
      (is (= 4 (drawn :voided t)) "and so does a failed attempt, which streams no more"))))

(deftest a-wrapped-code-line-goes-on-under-a-mark-the-copy-leaves ()
  ;; A code line longer than the row wrapped back to the body's own column
  ;; with nothing saying it went on, so it read as a new line indented less.
  ;; Its later rows open on ↦ in the gutter; the copy reads the painted rows
  ;; and leaves the mark behind.
  (let* ((code "alpha beta gamma delta epsilon zeta eta theta")
         (fence (format nil "```~%~a~%```" code))
         ;; The slab's first row is its (here empty) language label.
         (plain (nth-value 1 (nle::lline-rows (second (nle::markdown-llines fence)) 30))))
    ;; The first row as written, the rest under the mark beside the slab's edge.
    (is (equal '("   ▌ alpha beta gamma delta" "   ▌↦epsilon zeta eta theta")
               (mapcar (lambda (row) (string-right-trim " " row)) plain)))
    ;; Prose wraps unmarked.
    (is (notany (lambda (row) (search "↦" row))
                (nth-value 1 (nle::llines-rows (nle::markdown-llines code) 30))))
    (is-each (nle::without-gutter-marks)
      ("     ↦ zeta eta" "       zeta eta" "the mark opening a row blanks, column for column")
      ("     alpha ↦ beta" "     alpha ↦ beta" "one inside the text stays")
      ("" "" "an empty row stays empty"))
    (with-frame-model (model 40 24 :session-id "code-wrap-copy"
                       :paint ((nle::ingest-message-fact "assistant" fence)))
      (let* ((rows (coerce (nle::frame-plain-rows model) 'list))
             (first-row (position-if (lambda (row) (search "alpha" row)) rows))
             (last-row (position-if (lambda (row) (search "theta" row)) rows)))
        (is (search "▌↦epsilon" (nth last-row rows)) "the frame paints the mark")
        (setf (nle::chat-model-selection model) (cons (cons first-row 0) (cons last-row 39)))
        (let ((copied (nle::frame-selection-text model)))
          (is (not (search "↦" copied)) "the copy leaves it behind")
          (is (search "epsilon zeta eta theta" copied) "and keeps the code it marked"))))))

(deftest cached-transcript-links-survive-string-compaction (dolist (width '(24 70 160)))
  ;; ASCII rows were narrowed AFTER their EQ-keyed click ranges
  ;; were registered. Unicode rows kept their identity and still clicked.
  (dolist (source '("### [hell0ks/GLM-5.3-Flash-Uncensored-AWQ](https://huggingface.co/hell0ks/GLM-5.3-Flash-Uncensored-AWQ)"
                    "[dealignai/GLM-5.3-Flash-CYBERSECURITY-W4A16](https://huggingface.co/dealignai/GLM-5.3-Flash-CYBERSECURITY-W4A16) also exists—the old name redirects there."
                    "see https://example.com/docs and `src/tui/ui.lisp`"))
    (is-link-painted-alike source width)))

(deftest model-label-links-open-through-transcript-mouse ()
  (with-frame-model (model 160 40 :session-id "model-links")
    (let ((labels '("hell0ks/GLM-5.3-Flash-Uncensored-AWQ"
                    "dealignai/GLM-5.3-Flash-CYBERSECURITY-W4A16")))
      (paint-model model
        (nle::ingest-message-fact "assistant"
          (format nil "### [~a](https://huggingface.co/~a)~%~%[~a](https://huggingface.co/~a) also exists—the old name redirects there."
                  (first labels) (first labels) (second labels) (second labels))))
      (dolist (label labels)
        (is (equal (concatenate 'string "https://huggingface.co/" label) (click-opens model label)) "ASCII heading and Unicode paragraph both open their model")))))

;;; --- Developer / Standard viewport contract --------------------------------
(deftest viewport-standard-is-default-and-expanded-is-not-a-mode ()
  (with-frame-model (model 100 40 :session-id "viewport-modes")
    (is (eq :standard nle::*tool-detail*) "the render-scope default matches fresh shells")
    (is (eq :standard model.tool-detail))
    (is (equal "viewport Standard" (nle::slash-details-op model "")))
    (is (search "usage:" (nle::slash-details-op model "expanded")))
    (is (eq :standard model.tool-detail) "invalid mode leaves the viewport unchanged")
    (is (eq :developer (nle::toggle-tool-detail model)))))

(deftest viewport-standard-is-one-disclosure-per-call ()
  ;; Opened, a run names each call on a line of its own, and nothing raw:
  ;; each call's own disclosure still holds its input and output.
  (let* ((nle::*tool-detail* :standard)
         (state (mixed-turn-state))
         (run (find-if #'nle::tool-group-p (mapcar #'nle::display-block-row
                                                   (nle:project-state state :width 100))))
         (nle::*expanded-tool-call* (nle::tool-group-key run))
         (rows (mapcan (lambda (block) (copy-list block.plain-lines))
                       (nle:project-state state :width 100))))
    ;; one line per call.
    (is (equal '("Read a.lisp" "Read b.lisp" "Searched 'needle'" "Edited a.lisp")
               (loop for name in '("Read a.lisp" "Read b.lisp" "Searched 'needle'" "Edited a.lisp")
                     when (= 1 (count-if (lambda (row) (search name row)) rows)) collect name)))
    (is (not (rows-carry-p "(a)" rows)) "tool output stays behind the disclosure")
    (is (not (rows-carry-p "(edit" rows)) "tool input stays behind the disclosure")))

;;; A live turn's whole work trail folds under one
;;; `Ran N commands' that counts up as calls land, its thoughts with it, and
;;; the model's words always stand under it. The run stands folded, live or
;;; done; only a call still running stands a row under the line, until it
;;; ends. Opened, it names every call a line each with no label,
;;; on the column of its words, and while the turn runs the run holds its
;;; newest thought for the live bar to show under itself
;;; (A-RUNNING-TURNS-NEWEST-THOUGHT-STANDS-UNDER-THE-LIVE-BAR).
(deftest a-turn-folds-its-calls-as-they-land ()
  (let ((nle::*tool-detail* :standard) (nle::*expanded-tool-call* nil) (state nil))
    (flet ((call (id &optional (done t))
             (setf state (nle::ingest-tool-call-fact state id "eval" (eval-args (format nil ";; Step ~a~%(+ 1 2)" id)) :turn-id "live"))
             (when done (setf state (nle::ingest-tool-result-fact state id "eval" "3" :turn-id "live"))))
           (think (text) (setf state (nle::ingest-message-fact state "assistant" "" :reasoning text :turn-id "live")))
           (say (text) (setf state (nle::ingest-message-fact state "assistant" text :turn-id "live")))
           (shown () (mapcan (lambda (block) (copy-list (nle::display-block-plain-lines block))) (nle:project-state state :width 100)))
           (thought () (let ((run (find-if #'nle::tool-group-p (mapcar #'nle::display-block-row (nle:project-state state :width 100)))))
                         (and run (nle::tool-group-thought run) (nle::transcript-row-text (nle::tool-group-thought run)))))
           (at (needle rows) (position-if (lambda (row) (search needle row)) rows))
           (tone (text) (nle::lline-tone (first (nle::row-llines-with-eviction
                                                 (find text (nle::transcript-rows state) :key #'nle::transcript-row-text :test #'equal)
                                                 100)))))
      (think "**Planning** the check")
      (let ((rows (shown)))
        (is (rows-carry-p "Thought" rows) "a run with no commands yet says what it did")
        (is (not (rows-carry-p "Ran 0" rows)) "not that it ran nothing")
        (is (not (rows-carry-p "Planning the check" rows)) "its thought stands under the live bar, not under the line")
        (is (search "Planning" (thought)) "the run holds it for the bar")
        (is (not (rows-carry-p "Think" rows)) "with no label"))
      (let ((nle::*expanded-tool-call* '(:process "live")))
        (call "c1")
        (is (rows-carry-p "Step c1" (shown)) "a run opened while it was all thought stays open as its first call lands"))
      (is (rows-carry-p "Ran 1 command" (shown)))
      (is (rows-carry-p "Step c1" (shown)) "a call that ended stands under the folded run until the next round lists a call")
      (call "c2" nil)
      (let ((rows (shown)))
        (is (rows-carry-p "Ran 2 commands…" rows) "a running call adds the ellipsis")
        (is (rows-carry-p "Step c2" rows) "and stands under the folded line while it runs")
        (is (not (rows-carry-p "Step c1" rows)) "alone"))
      (setf state (nle::ingest-tool-result-fact state "c2" "eval" "3" :turn-id "live"))
      (is (rows-carry-p "Step c2" (shown)) "ended, it stands while no call follows it")
      (say "Checking the sum next.")
      (is (rows-carry-p "Checking the sum next." (shown)) "words after the last call stand under the run")
      (is (not (eq :muted (tone "Checking the sum next."))) "bright while nothing has followed them")
      (think "**Weighing** the next step")
      (call "c3")
      (call "c4")
      (let ((rows (shown)))
        (is (rows-carry-p "Ran 4 commands" rows) "thoughts and words between calls split nothing")
        (is (= 1 (count-if (lambda (row) (search "Ran " row)) rows)) "one line for the turn")
        (is (and (rows-carry-p "Step c4" rows) (notany (lambda (id) (rows-carry-p (format nil "Step ~a" id) rows)) '("c1" "c2" "c3"))) "folded, it names only its newest round's call")
        (is (not (rows-carry-p "Weighing the next step" rows)) "the newest thought stands under the live bar")
        (is (search "Weighing" (thought)) "the run holds the newest, in place of the one before it")
        (let ((newest (nle::transcript-row-text (find :reasoning (nle::transcript-rows state) :key #'nle::transcript-row-kind :from-end t))))
          (is (every #'nle::lline-shadow (nle::thought-paragraph-llines newest (length newest))) "the thought under the reasoning shadow, a step quieter than the calls"))
        (is (< (at "Ran 4 commands" rows) (at "Checking the sum next." rows)) "and the words stand under the run")
        (is (not (eq :muted (tone "Checking the sum next."))) "the model's words keep the foreground as later calls land"))
      (setf state (nle::ingest-system-notice state "[guard.lisp — this shell: 4]" :turn-id "live" :retention :live-only
                                             :metadata (nlk:json-object "summary" (nlk:json-object "name" "Mutate" "path" "/w/guard.lisp" "comment" "Guard the sum"))))
      (let ((receipt (car (last (nle::transcript-rows state)))))
        (is (rows-carry-p "Ran 4 commands, Mutated 1 time" (shown)))
        (is (rows-carry-p "Guard the sum" (shown)) "a Mutate stands with the call it followed")
        (let* ((nle::*expanded-tool-call* '(:process "c1"))
               (rows (shown)))
          (is (every (lambda (id) (rows-carry-p (format nil "Step ~a" id) rows)) '("c1" "c2" "c3" "c4")) "opened, it names every call")
          (is (rows-carry-p "Guard the sum" rows) "and the Mutate with them")
          (is (eql (search "Ran " (nth (at "Ran " rows) rows)) (search "Step c4" (nth (at "Step c4" rows) rows))) "on the column of the title's words")
          (is (not (rows-carry-p "Eval" rows)) "with no label")
          (is (not (rows-carry-p "(+ 1 2)" rows)) "a name each: no forms, no output")
          (is (not (rows-carry-p "Think" rows)) "no Think card")
          (is (and (eql (+ 2 (at "Ran 4 commands" rows)) (at "Step c1" rows))
                   (equal "" (string-trim " " (nth (1+ (at "Ran 4 commands" rows)) rows)))) "a canvas row under the line that names them"))
        (is (eq :success (nle::lline-tone (nle::run-tail-lline receipt 100))) "a Mutate in teal")
        (is (eq :muted (nle::lline-tone (nle::run-tail-lline (find "c4" (nle::transcript-rows state) :key #'nle::transcript-row-tool-call-id :test #'equal) 100))) "a call in grey"))
      (say "The sum holds.")
      (setf state (nle::ingest-turn-terminal state "completed" :turn-id "live"))
      (let ((rows (shown)))
        (is (rows-carry-p "Ran 4 commands" rows))
        (is (notany (lambda (id) (rows-carry-p (format nil "Step ~a" id) rows)) '("c1" "c2" "c3" "c4")) "a finished turn stays folded")
        (is (null (thought)) "and lets its thought go")
        (is (rows-carry-p "The sum holds." rows) "its newest words stand under the run")
        (is (not (rows-carry-p "Checking the sum next." rows)) "in the place the words before them held")
        (is (< (at "Ran 4 commands" rows) (at "The sum holds." rows)) "under the run")
        (is (not (eq :muted (tone "The sum holds."))) "in the foreground"))
      (setf state (nle::ingest-message-fact state "user" "again" :turn-id "stopped"))
      (dolist (id '("s1" "s2"))
        (setf state (tool-round state id "eval" (eval-args (format nil ";; Step ~a~%(+ 1 2)" id)) "3" :turn-id "stopped")))
      (setf state (nle::ingest-tool-call-fact state "s3" "eval" (eval-args (format nil ";; Step s3~%(sleep 9)")) :turn-id "stopped"))
      (is (rows-carry-p "Step s3" (shown)) "a call running stands under its run")
      (setf state (nle::ingest-turn-terminal state "cancelled" :turn-id "stopped"))
      (let ((rows (shown)))
        (is (notany (lambda (id) (rows-carry-p (format nil "Step ~a" id) rows)) '("s1" "s2" "s3")) "a turn no answer followed folds too, its call cut off with it")
        (is (not (rows-carry-p "Step c1" rows)) "as does the answered one")))))

(deftest a-runs-arrow-says-what-it-lists-and-a-click-folds-or-opens-it ()
  ;; `▸ Ran 11 commands' over the calls it listed read as shut; ▾ is a run
  ;; listing its whole trail, and a click folds it, a live one too. A run
  ;; stands folded; opened, it names its calls a line each: the Eval, Think
  ;; and Mutate cards were the view before that one, and each call's code and
  ;; output after it.
  (with-frame-model (model 100 40 :session-id "run-arrow")
    (flet ((rows () (nle::compose-frame model) (coerce (nle::frame-plain-rows model) 'list))
           (click (needle)
             (let* ((y (frame-row-carrying model needle))
                    (x (1+ (search needle (aref (nle::frame-plain-rows model) (1- y))))))
               (click-at model x y))))
      (paint-model model
        (nle::ingest-message-fact "user" "check the sum" :turn-id "t1")
        (nle::ingest-message-fact "assistant" "" :reasoning "**Planning** the check" :turn-id "t1")
        (tool-round "c1" "eval" (eval-args (format nil ";; Step one~%(+ 1 2)")) "3" :turn-id "t1")
        (tool-round "c2" "eval" (eval-args (format nil ";; Step two~%(+ 2 2)")) "4" :turn-id "t1"))
      (let ((rows (rows)))
        (is (rows-carry-p "▸ Ran 2 commands" rows) "a live run stands folded under ▸")
        (is (not (rows-carry-p "Step one" rows)) "its calls with it"))
      (click "Ran 2 commands")
      (is (and (rows-carry-p "▾ Ran 2 commands" (rows)) (rows-carry-p "Step two" (rows))) "a click opens it")
      (click "Ran 2 commands")
      (is (and (rows-carry-p "▸ Ran 2 commands" (rows)) (not (rows-carry-p "Step one" (rows)))) "and folds it again, to the call still standing")
      (click "Ran 2 commands")
      (paint-model model
        (nle::ingest-message-fact "assistant" "The sum holds." :turn-id "t1")
        (nle::ingest-turn-terminal "completed" :turn-id "t1"))
      (is (rows-carry-p "▾ Ran 2 commands" (rows)) "opened by a click, it stays open past the answer")
      (click "Ran 2 commands")
      (let ((rows (rows)))
        (is (rows-carry-p "▸ Ran 2 commands" rows) "a click folds it")
        (is (not (rows-carry-p "Step one" rows))))
      (click "Ran 2 commands")
      (let ((rows (rows)))
        (is (rows-carry-p "▾ Ran 2 commands" rows) "a click opens it")
        (is (and (rows-carry-p "Step one" rows) (rows-carry-p "Step two" rows)) "to every call")
        (is (notany (lambda (label) (rows-carry-p label rows)) '("Eval" "Think" "Planning")) "a line each, no cards"))
      (click "Step two")
      (let* ((rows (rows)) (line (row-index-of "Step two" rows :required t)))
        (is (search "(+ 2 2)" (nth (1+ line) rows)) "a call clicked open shows its form under its line")
        (is (eql (+ 2 (search "Step two" (nth line rows))) (search "(+ 2 2)" (nth (1+ line) rows))) "two columns in")
        (is (= 1 (count-if (lambda (row) (search "Step two" row)) rows)) "its note said once, on its line")
        (is (not (rows-carry-p "(+ 1 2)" rows)) "the others stay a line"))
      (click "Step two")
      (is (and (not (rows-carry-p "(+ 2 2)" (rows))) (rows-carry-p "▾ Ran 2 commands" (rows))) "a second click closes it, the run still open"))))

(deftest a-call-clicked-under-a-folded-run-opens-alone ()
  ;; Without this, a click on a call running under a shut `Ran N
  ;; commands' would open the whole run: the accordion would read a
  ;; member's key as its run's. The call opens under the shut title,
  ;; and stays listed there once its round is over, until a click closes it.
  (with-frame-model (model 100 40 :session-id "fold-call")
    (flet ((rows () (nle::compose-frame model) (coerce (nle::frame-plain-rows model) 'list))
           (click (needle)
             (let* ((y (frame-row-carrying model needle))
                    (x (1+ (search needle (aref (nle::frame-plain-rows model) (1- y))))))
               (click-at model x y))))
      (paint-model model
        (nle::ingest-message-fact "user" "check the sum" :turn-id "t1")
        (tool-round "c1" "eval" (eval-args (format nil ";; Step one~%(+ 1 2)")) "3" :turn-id "t1")
        (tool-round "c2" "eval" (eval-args (format nil ";; Step two~%(+ 2 2)")) "4" :turn-id "t1")
        (nle::ingest-tool-call-fact "c3" "eval" (eval-args (format nil ";; Step three~%(sleep 9)")) :turn-id "t1"))
      (let ((rows (rows)))
        (is (and (rows-carry-p "▸ Ran 3 commands" rows) (rows-carry-p "Step three" rows)) "the running call stands under the shut run")
        (is (not (rows-carry-p "(sleep 9)" rows)) "a line"))
      (click "Step three")
      (let ((rows (rows)))
        (is (rows-carry-p "▸ Ran 3 commands" rows) "a click on it leaves the run shut")
        (is (not (rows-carry-p "Step one" rows)) "its other calls unlisted")
        (is (rows-carry-p "(sleep 9)" rows) "and opens the call alone"))
      (paint-model model
        (nle::ingest-tool-result-fact "c3" "eval" "nil" :turn-id "t1")
        (nle::ingest-tool-call-fact "c4" "eval" (eval-args (format nil ";; Step four~%(sleep 8)")) :turn-id "t1"))
      (let ((rows (rows)))
        (is (and (rows-carry-p "▸ Ran 4 commands" rows) (rows-carry-p "Step four" rows)) "the next round stands under the shut run")
        (is (rows-carry-p "(sleep 9)" rows) "the call opened stays open there past its round")
        (is (not (rows-carry-p "Step two" rows)) "alone"))
      (click "Step three")
      (let ((rows (rows)))
        (is (rows-carry-p "▸ Ran 4 commands" rows) "a second click closes it, the run still shut")
        (is (not (rows-carry-p "Step three" rows)) "its round over, it leaves the fold"))
      (click "Step four")
      (click "Ran 4 commands")
      (let ((rows (rows)))
        (is (and (rows-carry-p "▾ Ran 4 commands" rows) (rows-carry-p "Step one" rows)) "the title opens the whole run")
        (is (rows-carry-p "(sleep 8)" rows) "the call opened in the fold still open in it"))
      (click "Ran 4 commands")
      (is (not (rows-carry-p "(sleep 8)" (rows))) "and folds it all"))))

(deftest a-running-turns-newest-thought-stands-under-the-live-bar ()
  ;; Without this, the thought would stand a row above the bar saying the
  ;; model was thinking. It stands a row under the bar, on the column of its
  ;; words and the run title's, two rows off the composer as the bar is; its
  ;; newest paragraph shows whole until the next one takes its place.
  (with-frame-model (model 100 30 :session-id "thought-under-bar")
    (flet ((rows () (nle::compose-frame model) (copy-seq (nle::frame-plain-rows model)))
           (at (needle rows) (position-if (lambda (row) (search needle row)) rows))
           (column (row) (position #\Space row :test-not #'char=)))
      (fold-model model
        (nle::ingest-message-fact "user" "hello" :turn-id "t1")
        (tool-round "c1" "eval" (eval-args (format nil ";; Read the bar~%(+ 1 2)")) "3" :turn-id "t1"))
      (mark-turn-in-flight model :render t)
      (let ((before (rows)))
        (paint-model model (nle::ingest-message-fact "assistant" "" :reasoning "**Weighing** the next step" :turn-id "t1"))
        (let* ((rows (rows))
               (bar (at "waiting" rows))
               (thought (at "Weighing the next step" rows)))
          (is (eql (+ 2 bar) thought) "a row under the bar")
          (is (equal "" (string-trim " " (aref rows (1+ bar)))) "a blank one")
          (is (eql (search "waiting" (aref rows bar)) (search "Weighing" (aref rows thought))) "on the bar's words' column")
          (is (eql (search "Ran " (aref rows (at "Ran 1 command" rows))) (search "Weighing" (aref rows thought))) "which is the run title's")
          (is (< (at "Ran 1 command" rows) bar) "the run stays above the bar")
          (is (equalp (subseq before 0 bar) (subseq rows 0 bar)) "nothing above the bar moves")
          (is (every (lambda (at) (equal "" (string-trim " " (aref rows at)))) (list (+ 1 thought) (+ 2 thought))) "two canvas rows off the composer")
          (is (eql (at "Type to steer" before) (at "Type to steer" rows)) "and the composer stays put"))
        (paint-model model (nle::ingest-message-fact "assistant" "" :reasoning (format nil "**Weighing** the next step~%~%The sum is three.~%~%**Counting down**~%~%The label is built once, when the retry is announced, so every frame after that repaints the same words. Passing the due time down lets each frame count the seconds itself, and the tests can pin the clock.") :turn-id "t1"))
        (let* ((rows (rows))
               (bar (at "waiting" rows))
               (end (at "pin the clock." rows)))
          (is (eql (+ 2 bar) (at "Counting down" rows)) "the newest paragraph under the heading that opens it")
          (is (eql (+ 3 bar) (at "The label is built once" rows)) "its words right under it")
          (is (and end (> end (+ 3 bar))) "whole, over as many rows as it takes")
          (is (every (lambda (row) (eql (search "waiting" (aref rows bar)) (column (aref rows row))))
                     (list (at "Counting down" rows) (at "The label is built once" rows) end)) "every row on the bar's words' column, the wrapped ones too")
          (is (not (at "Weighing" rows)) "the paragraph before it gone")
          (is (not (at "The sum is three" rows)) "every one before it")
          (is (eql (at "Type to steer" before) (at "Type to steer" rows)) "the composer stays at the foot"))
        (nle::toggle-tool-detail model :developer)
        (nle::render-state model)
        (let ((rows (rows)))
          (is (and (< (at "Thought" rows) (at "waiting" rows)) (not (at "Counting down" (subseq rows (at "waiting" rows))))) "the Developer view keeps its own Thought block, above the bar"))
        (nle::toggle-tool-detail model :standard))
      (fold-model model (nle::ingest-turn-terminal "completed" :turn-id "t1"))
      (rendering model (setf model.turn-in-flight nil))
      (is (not (at "Counting down" (rows))) "a finished turn takes it with the bar"))))

;;; A place on the screen hands what it shows to a newer thing by a fade: the
;;; old goes out into the canvas, then the newcomer comes up out of it.
(deftest a-place-hands-its-thing-to-a-newer-one-by-a-fade ()
  (let* ((ms (/ internal-time-units-per-second 1000))
         (fade (nle::make-fade :a 100 200))
         (t0 (nle::fade-at fade)))
    (is-each (nle::fade-light fade :a)
      (t0 0 "the first comes up out of the canvas")
      ((+ t0 (* 100 ms)) 50 "over its time in")
      ((+ t0 (* 200 ms)) 100 "all the way")
      ((+ t0 (* 300 ms)) 100 "and stays"))
    (is (nle::fade-settled-p fade (+ t0 (* 200 ms))) "a place all the way up stands still")
    (nle::fade-to fade :b (+ t0 (* 300 ms)))
    (is-each (nle::fade-on fade)
      ((+ t0 (* 350 ms)) :a "what stood there shows while it goes out")
      ((+ t0 (* 400 ms)) :b "then the newcomer"))
    (is-each (nle::fade-light fade :a)
      ((+ t0 (* 300 ms)) 100 "going out from all its light")
      ((+ t0 (* 350 ms)) 50 "over its time out")
      ((+ t0 (* 400 ms)) 0 "into the canvas"))
    (is-each (nle::fade-light fade :b)
      ((+ t0 (* 400 ms)) 0 "the newcomer waits for it")
      ((+ t0 (* 500 ms)) 50 "then comes up")
      ((+ t0 (* 600 ms)) 100 "all the way"))
    (nle::fade-to fade :c (+ t0 (* 350 ms)))
    (is (eq :a (nle::fade-on fade (+ t0 (* 380 ms)))) "what is going out keeps going")
    (is (eq :c (nle::fade-on fade (+ t0 (* 400 ms)))) "and a newer one waits behind it in place of the one it overtook")))

;;; A fast model overtakes a thing before it is up: it goes out from the light
;;; it had, at the same pace, and the newcomer never waits on the dark.
(deftest a-thing-overtaken-before-it-is-up-goes-out-from-the-light-it-had ()
  (let* ((ms (/ internal-time-units-per-second 1000))
         (fade (nle::make-fade :a 100 200))
         (t0 (nle::fade-at fade)))
    (nle::fade-to fade :b (+ t0 (* 100 ms)))
    (is-each (nle::fade-on fade)
      ((+ t0 (* 140 ms)) :a "a thing half up goes out")
      ((+ t0 (* 150 ms)) :b "in half the time"))
    (nle::fade-to fade :c (+ t0 (* 150 ms)))
    (is (eq :c (nle::fade-on fade (+ t0 (* 150 ms)))) "one not up at all goes at once")
    (is (= 50 (nle::fade-light fade :c (+ t0 (* 250 ms)))) "and its newcomer comes up from the moment it came")))

;;; A thought comes up a token at a time, in reading order; fade every
;;; token.
(deftest a-place-can-bring-its-tokens-up-one-by-one ()
  (let* ((ms (/ internal-time-units-per-second 1000))
         (fade (nle::make-fade :a 100 200))
         (t0 (nle::fade-at fade)))
    (setf (nle::fade-step fade) 50 (nle::fade-tokens fade) 3)
    (is-each (nle::fade-light fade :a)
      ((+ t0 (* 100 ms)) 0 50 "the first token half up")
      ((+ t0 (* 100 ms)) 1 25 "the next a step behind it")
      ((+ t0 (* 100 ms)) 2 0 "the last not started")
      ((+ t0 (* 300 ms)) 2 100 "all up once the last has had its time"))
    (is (= 50 (nle::fade-light fade :a (+ t0 (* 150 ms)))) "the whole of it rises over its tokens' time")
    (is (not (nle::fade-settled-p fade (+ t0 (* 250 ms)))))
    (is (nle::fade-settled-p fade (+ t0 (* 300 ms))))))

(deftest a-finished-thought-comes-up-a-token-at-a-time ()
  (with-frame-model (model 100 30 :session-id "thought-tokens")
    (let ((nle::*color-profile* :truecolor)
          (ms (/ internal-time-units-per-second 1000)))
      (setf (nle::chat-model-program model) :terminal)
      (fold-model model
        (nle::ingest-message-fact "user" "hello" :turn-id "t1")
        (nle::ingest-tool-call-fact "c1" "eval" (eval-args (format nil ";; Read the bar~%(+ 1 2)")) :turn-id "t1")
        (nle::ingest-tool-result-fact "c1" "eval" "3" :turn-id "t1")
        (nle::ingest-message-fact "assistant" "" :reasoning "first second third fourth fifth sixth" :turn-id "t1"))
      (mark-turn-in-flight model)
      (nle::render-state model)
      (let* ((rows (nle::compose-frame model))
             (fade (nle::thought-fades model))
             (y (1- (frame-row-carrying model "first second")))
             (plain (nle::plain-frame-row (nth y rows)))
             (first (search "first" plain))
             (last (search "sixth" plain)))
        (is (and fade (= 6 (nle::fade-tokens fade)) (= nle::+thought-token-step-ms+ (nle::fade-step fade))) "its six tokens, a step apart")
        (setf (nle::fade-at fade) (- (get-internal-real-time) (* (floor (nle::fade-rise fade) 2) ms)))
        (let ((screen (nlt:parse-styled-string rows 100 30)))
          (funcall (nle::frame-overlay model) screen)
          (flet ((ink (x) (nle::sgr-ink-rgb (nlt:cell-fg (nlt:screen-buffer-ref screen x y)))))
            (is (> (reduce #'+ (ink first)) (reduce #'+ (ink last))) "the first token stands higher than the last")
            (is (equal (ink first) (ink (+ first 4))) "a token comes up as one")))))))

(deftest a-turns-words-take-one-place-and-fade-through-it ()
  ;; Every message of a turn stands under its run, stacked: the newest
  ;; takes the place, the one before fading out of it first, and the
  ;; newest fading in after, quickly enough to read a fast model by.
  (let* ((ms (/ internal-time-units-per-second 1000))
         (out (first nle::+words-fade-ms+))
         (in (second nle::+words-fade-ms+))
         (nle::*fades* t)
         (nle::*turn-words* (make-hash-table :test 'equal))
         (a (nle::make-transcript-row :kind :text :role "assistant" :turn-id "t" :text "Looking at the file."))
         (b (nle::make-transcript-row :kind :text :role "assistant" :turn-id "t" :text "The sum holds.")))
    (is (eq a (nle::turn-words-on a t)) "the first words take the place")
    (let ((fade (gethash "t" nle::*turn-words*)))
      (is (= 0 (nle::fade-light fade a (nle::fade-at fade))) "coming up out of the canvas")
      (decf (nle::fade-at fade) (* in ms))
      (is (eq a (nle::turn-words-on b t)) "newer words wait while the words before them go out")
      (is (= 100 (nle::fade-light fade b (+ (nle::fade-at fade) (* 500 ms)))) "and are all the way up within half a second")
      (decf (nle::fade-at fade) (* out ms))
      (is (eq b (nle::turn-words-on b t)) "then take their place")
      (let ((copy (copy-structure b)))
        (is (eq copy (nle::turn-words-on copy nil)) "the same words seated again take it with no fade")
        (is (not (nle::fade-settled-p fade)) "the place still coming up")
        (decf (nle::fade-at fade) (* in ms))
        (is (eq copy (nle::turn-words-on copy nil)))))
    (is (null (gethash "t" nle::*turn-words*)) "an ended turn's place, once still, is let go")
    (is (eq b (nle::turn-words-on b nil)) "history lays out as it stands")
    (is (null (gethash "t" nle::*turn-words*)) "and never fades")
    (let ((nle::*fades* nil))
      (is (eq b (nle::turn-words-on b t)) "nor does a shell on no terminal")
      (is (null (gethash "t" nle::*turn-words*))))))

(deftest the-newest-words-fade-through-their-place-on-the-screen ()
  (with-frame-model (model 100 30 :session-id "words-fade" :tool-detail :standard)
    (let ((nle::*color-profile* :truecolor)
          (nle::*turn-words* (make-hash-table :test 'equal))
          (ms (/ internal-time-units-per-second 1000))
          (out (first nle::+words-fade-ms+))
          (in (second nle::+words-fade-ms+)))
      ;; A shell painting on a terminal: the fades are its.
      (setf (nle::chat-model-program model) :terminal)
      (fold-model model
        (exchange "check the sum" "Looking at the file." :turn-id "t1"))
      (mark-turn-in-flight model)
      (nle::render-state model)
      (let* ((rows (nle::compose-frame model))
             (y (1- (frame-row-carrying model "Looking at the file.")))
             (row (find y (nle::fade-rows model) :key #'first))
             (fade (fourth row)))
        (is (and row (eq fade (gethash "t1" nle::*turn-words*))) "the words' row is found in its place")
        (destructuring-bind (x0 x1) (subseq row 1 3)
          (is (search "Looking" (subseq (nle::plain-frame-row (nth y rows)) x0 x1)) "over its words")
          (let ((screen (nlt:parse-styled-string rows 100 30))
                (canvas (nle::color-sgr (nle::canvas-fill-bg) t :truecolor)))
            (funcall (nle::frame-overlay model) screen)
            (is (equal canvas (nlt:cell-fg (nlt:screen-buffer-ref screen x0 y))) "coming up out of the canvas")
            (decf (nle::fade-at fade) (* in ms))
            (is (nle::fade-moved-p model) "the light moving repaints")
            (is (null (nle::fade-overlay model)) "all the way up, nothing to paint over")
            (is (not (nle::fade-moved-p model)) "and a tick at the same light repaints nothing"))
          (fold-model model
            (nle::ingest-tool-call-fact "c1" "eval" (eval-args (format nil ";; Step one~%(+ 1 2)")) :turn-id "t1")
            (nle::ingest-tool-result-fact "c1" "eval" "3" :turn-id "t1")
            (nle::ingest-message-fact "assistant" "The sum holds." :turn-id "t1"))
          (nle::render-state model)
          (let ((rows (coerce (progn (nle::compose-frame model) (nle::frame-plain-rows model)) 'list)))
            (is (rows-carry-p "Looking at the file." rows) "the words before go out where the newest stand")
            (is (not (rows-carry-p "The sum holds." rows)) "which wait for them")
            (is (< (row-index-of "Ran 1 command" rows :required t) (row-index-of "Looking at the file." rows :required t)) "under the run"))
          (is (not (nle::fade-due-p model)))
          (decf (nle::fade-at fade) (* out ms))
          (is (nle::fade-due-p model) "gone out, the place is due its newcomer")
          (nle::render-state model)
          (let ((rows (coerce (progn (nle::compose-frame model) (nle::frame-plain-rows model)) 'list)))
            (is (rows-carry-p "The sum holds." rows) "which comes up where they stood")
            (is (not (rows-carry-p "Looking at the file." rows)) "alone"))
          (decf (nle::fade-at fade) (* in ms))
          (nle::compose-frame model)
          (is (null (nle::fade-rows model)) "a place standing still leaves the frame's paint alone"))))))

(deftest a-streaming-thought-shows-only-what-it-has-finished ()
  ;; The thought under the bar shows only what it has finished: a
  ;; paragraph a blank line has closed stands there, under the heading that
  ;; opens it; a heading alone opens the next.
  (with-frame-model (model 100 30 :session-id "finished-thought")
    (flet ((rows () (nle::compose-frame model) (coerce (nle::frame-plain-rows model) 'list))
           (think (text)
             (fold-model model (nle::ingest-stream-body :reasoning text :turn-id "t1"))
             (nle::render-state model)))
      (fold-model model
        (nle::ingest-message-fact "user" "hello" :turn-id "t1")
        (nle::ingest-tool-call-fact "c1" "eval" (eval-args (format nil ";; Read the bar~%(+ 1 2)")) :turn-id "t1")
        (nle::ingest-tool-result-fact "c1" "eval" "3" :turn-id "t1"))
      (mark-turn-in-flight model)
      (think "**Planning**")
      (is (not (rows-carry-p "Planning" (rows))) "a heading still being written shows nothing")
      (think (format nil "~%~%First I read the file"))
      (is (not (rows-carry-p "First I read" (rows))) "nor the paragraph under it while it arrives")
      (think (format nil ".~%~%**Checking**~%~%Now the su"))
      (let ((rows (rows)))
        (is (rows-carry-p "Planning" rows) "a closed paragraph stands, under its heading")
        (is (rows-carry-p "First I read the file." rows))
        (is (not (rows-carry-p "Checking" rows)) "the heading over the paragraph being written waits with it")
        (is (not (rows-carry-p "Now the su" rows))))
      (think (format nil "m.~%~%"))
      (is (rows-carry-p "Now the sum." (rows)) "a blank line closes a paragraph")
      (think "**Next**")
      (is (rows-carry-p "Now the sum." (rows)) "a heading after it opens the next, and waits")
      (nle::end-thinking (find :reasoning (nle::transcript-rows (nle::chat-model-state model)) :key #'nle::transcript-row-kind))
      (nle::render-state model)
      (is (rows-carry-p "Next" (rows)) "a thought done is finished whole"))))

(deftest a-context-edit-stands-on-the-rails-under-its-label ()
  ;; `Context' stands in the label lane where Eval and Think do, its words
  ;; on the content rail, on the canvas.
  (let* ((nle::*tool-detail* :standard)
         (state (nle::ingest-system-notice nil "~118.7k → ~734 tokens · 145 removed · by the model"
                                           :turn-id "t" :metadata (nlk:json-object "label" "Context")))
         (block (first (nle:project-state state :width 100)))
         (line (find-if (lambda (text) (search "Context" text)) (nle::display-block-plain-lines block))))
    (is (eql 3 (search "Context" line)) "the label in the label lane")
    (is (eql 12 (search "~118.7k" line)) "its words on the content rail")
    (is (not (search "■" line)))
    (is (null (nle::row-block-fill (nle::display-block-row block))) "on the canvas")
    (is (= 3 (length (nle::display-block-plain-lines block))) "a row apart from its neighbours, as a block is")))

(deftest the-live-bar-rides-the-transcripts-tail-one-row-over-the-composer ()
  ;; The bar is the transcript's last row: it scrolls with it, the scroll
  ;; gutter runs down through it, and one canvas row stands between it and
  ;; the composer. Without this, when the bar is docked between band and box,
  ;; the gutter stops four rows short of the box (a tall transcript:
  ;; LIVE-STATUS-RIDES-THE-TRANSCRIPTS-TAIL). One canvas row above the bar;
  ;; no rows while nothing runs.
  (with-frame-model (model 100 30 :session-id "bar-zone")
    (nle::render-state model)
    (is (null (first (nle::status-blocks model))) "an idle shell holds no rows for a bar")
    (fold-model model (nle::ingest-message-fact "user" "hello" :turn-id "t1"))
    (mark-turn-in-flight model :render t)
    (let* ((rows (nle::frame-plain-rows model))
           (bar (position-if (lambda (row) (search "waiting" row)) rows))
           ;; The last `hello' above the bar: the header names the session too.
           (prompt (position-if (lambda (row) (search "hello" row)) rows :end bar :from-end t))
           (box (position-if (lambda (row) (search "Type to steer" row)) rows)))
      (is (and (< prompt bar (+ prompt 4))
               (loop for row from (1+ prompt) below bar always (equal "" (string-trim " " (aref rows row))))) "right under the prompt band: past its pad row, one canvas row")
      (is (= (- (length rows) 5) box) "the composer's text row stays at the foot of the frame")))
  (with-frame-model (model 100 14 :session-id "bar-zone-short")
    (fold-model model (nle::ingest-message-fact "user" "hello" :turn-id "t1"))
    (mark-turn-in-flight model :render t)
    (is (= 1 (length (first (nle::status-blocks model)))) "a short window gives the bar no row above")))

(deftest viewport-notes-survive-both-modes-and-developer-grouping ()
  (let* ((state (tool-round nil "note-a" "eval"
                            (eval-args (format nil ";; Inspect α~%(sh \"cat a.lisp\")")) "body-a"
                            :turn-id "t-note")))
    (setf state (tool-round state "note-b" "eval"
                            (eval-args (format nil ";; Inspect β~%(sh \"cat b.lisp\")")) "body-b"
                            :turn-id "t-note"))
    (dolist (nle::*tool-detail* '(:developer :standard))
      ;; Standard folds a live turn's calls into one run as they land; its
      ;; comments are a click away.
      (let* ((nle::*expanded-tool-call* (and (eq nle::*tool-detail* :standard) '(:process "note-a")))
             (blocks (nle:project-state state :width 100))
             (rows (mapcan (lambda (block) (copy-list block.plain-lines)) blocks)))
        (is (rows-carry-p "Inspect α" rows) "the first call's comment is visible")
        (is (rows-carry-p "Inspect β" rows) "the second call's comment is visible")
        (is (= 1 (length blocks)) "both viewports group the run"))
      (let ((nle::*expanded-tool-call* nil))
        (when (eq nle::*tool-detail* :standard)
          (is (rows-carry-p "Ran 2 commands" (nle::display-block-plain-lines (first (nle:project-state state :width 100)))) "shut, the run counts"))))))

(deftest viewport-standard-opening-is-local-and-shows-the-whole-body ()
  (let* ((nle::*tool-detail* :standard)
         (row (tool-row "eval" (eval-args (format nil ";; Explain the result~%(+ 1 1)"))
                        :tool-call-id "local-note" :tool-status "complete"
                        :text (format nil "~{~a~^~%~}" (loop for i below 10 collect (format nil "line ~d" i))))))
    (is (equal '("Eval     Explain the result") (mapcar #'nle::lline-text (nle::row-llines row 100))))
    (let* ((nle::*expanded-tool-call* "local-note")
           (lines (nle::row-llines row 100)))
      (is (find "(+ 1 1)" lines :key #'nle::lline-text :test #'equal))
      (is (= 10 (count-if (lambda (line) (search "line " (nle::lline-text line))) lines)))
      (is (notany (lambda (line) (eq :tool-omission (nle::lline-role line))) lines)))))

(deftest viewport-standard-shows-generic-tool-comments-without-raw-arguments ()
  (let* ((nle::*tool-detail* :standard)
         (row (tool-row "fetch" "{\"description\":\"Inspect the page\",\"url\":\"https://example.org\"}"
                        :tool-call-id "generic-note" :text "page content" :tool-status "complete")))
    (is (equal '("fetch    Inspect the page") (mapcar #'nle::lline-text (nle::row-llines row 100))))
    (let ((nle::*expanded-tool-call* "generic-note"))
      (is (find "page content" (nle::row-llines row 100) :key #'nle::lline-text :test #'equal)))))

(deftest user-block-boundaries-own-a-canvas-separator ()
  (dolist (profile '(:none :truecolor :ansi256 :ansi))
    (dolist (width '(40 100))
      (let* ((nle::*tool-detail* :standard) (nle::*color-profile* profile)
             (state (nle::ingest-message-fact nil "user" "question" :turn-id "gap-turn"))
             (state (tool-round state "gap-call" "eval"
                 (eval-args (format nil ";; Inspect boundary~%(+ 1 1)")) "2" :turn-id "gap-turn"))
             (state (nle::ingest-turn-terminal state "completed" :turn-id "gap-turn"))
             (blocks (nle:project-state state :width width))
             (user (first blocks)) (group (nle::display-block-row (second blocks))))
        (dolist (open '(nil t))
          (let* ((nle::*expanded-tool-call* (when open (nle::tool-group-key group)))
                 (summary (nle::tool-group-display-block group width)))
            (dolist (pair (list (list user summary) (list summary user)))
              (multiple-value-bind (painted plains refs) (nle::transcript-line-table pair)
                (let* ((boundary (length (nle::display-block-lines (first pair))))
                       (screen (nlt:parse-styled-string painted width (length painted))))
                  (is (= (length painted) (length plains) (length refs)))
                  (is (null (nth boundary refs)) "user-block boundary is canvas, not filled padding or a disclosure target")
                  (is (string= "" (nth boundary plains)) "a whole blank canvas row separates the blocks")
                  (is (null (cell-bg-at screen 8 boundary)) "separator stays canvas under every color profile")
                  (is (eq (nth (1+ boundary) refs) (nle::display-block-row (second pair))) "next block begins after the shared separator")
                  (unless (eq profile :none)
                    (let ((user-pad (if (eq user (first pair)) (1- boundary) (1+ boundary))))
                      (is (equal (fill-bg :user-block width) (cell-bg-at screen 8 user-pad)) "user padding retains its own fill"))))))))))))

;;; --- Standard tool blocks own no vertical padding --------------------------
(deftest standard-tool-blocks-have-no-top-bottom-or-inter-call-padding ()
  (let* ((nle::*tool-detail* :standard)
         (nle::*color-profile* :none)
         (rows (list (tool-row "eval" (eval-args (format nil ";; Inspect α~%(+ 1 1)")) :tool-call-id "tight-a")
                     (tool-row "eval" (eval-args (format nil ";; Inspect β~%(+ 2 2)")) :tool-call-id "tight-b")))
         (blocks (mapcar (lambda (row) (nle::row-display-block row :width 80)) rows)))
    (is (equal '(1 1) (mapcar (lambda (block) (length block.plain-lines)) blocks)))
    (is-values (painted plains refs) (nle::transcript-line-table blocks)
      ((length painted) = 2) (plains '("   Eval     Inspect α" "   Eval     Inspect β"))
      (refs rows "each adjacent row still targets its own call"))))

(deftest standard-tool-padding-policy-is-mode-keyed-on-memo-hits ()
  (let* ((nle::*color-profile* :none)
         (row (tool-row "eval" (eval-args (format nil ";; Local computation~%(+ 1 1)"))
                        :tool-call-id "tight-cache" :text "2")))
    (dolist (nle::*tool-detail* '(:developer :standard :developer :standard))
      (let* ((first (nle::row-display-block row :width 80))
             (again (nle::row-display-block row :width 80))
             (lines (nle::display-block-plain-lines first)))
        (is (eq first again) "memo hits keep the same block")
        (is (= (if (eq nle::*tool-detail* :standard) 1 4) (length lines)))
        (is (eq (eq nle::*tool-detail* :developer) (zerop (length (string-trim " " (first lines))))) "only Developer has a leading pad")
        (is (eq (eq nle::*tool-detail* :developer) (zerop (length (string-trim " " (car (last lines)))))) "only Developer has a trailing pad")))))

(deftest standard-keeps-non-tool-padding-and-body-owned-blank-lines ()
  (let ((nle::*tool-detail* :standard) (nle::*color-profile* :none))
    (dolist (args '((:kind :text :role "assistant" :text "answer")
                    (:kind :text :role "user" :text "ask")
                    (:kind :system-notice :text "notice")
                    (:kind :finish-divider :text "completed")))
      (let ((block (nle::row-display-block (apply #'nle::make-transcript-row args) :width 80)))
        (is (= 3 (length block.plain-lines)) "non-tool spacing stays unchanged")))
    (let* ((row (tool-row "eval" (eval-args (format nil ";; Read the body~%(+ 1 1)"))
                          :tool-call-id "tight-open" :text (format nil "first~%~%last")))
           (nle::*expanded-tool-call* "tight-open")
           (body (nth-value 1 (nle::llines-rows (nle::row-body-llines row 80) 80)))
           (block (nle::row-display-block row :width 80)))
      (is (equal body block.plain-lines) "remove structural padding, never body blanks")
      (is (some (lambda (line) (zerop (length (string-trim " " line)))) body) "the output's own blank remains"))))

(deftest standard-adjacent-tool-cards-remain-clickable-after-spacing-change ()
  (with-frame-model (model 100 40 :session-id "tight-mouse" :tool-detail :standard
                     :paint ((tool-round "tight-mouse-a" "eval" (eval-args (format nil ";; First card~%(+ 1 1)")) "2")
                             (tool-round "tight-mouse-b" "eval" (eval-args (format nil ";; Second card~%(+ 2 2)")) "4")))
    (let ((a (row-index-of "Eval     First card" model.line-plain :required t))
          (b (row-index-of "Eval     Second card" model.line-plain :required t)))
      (is (= (1+ a) b) "the real viewport paints adjacent tool headings")
      (click-at model 8 (mouse-y-for-line model b))
      (is (equal "tight-mouse-b" model.expanded-tool-call))
      (is (rows-carry-p "(+ 2 2)" (coerce model.line-plain 'list)))
      (is (not (rows-carry-p "(+ 1 1)" (coerce model.line-plain 'list))) "the sibling stays closed"))))

(deftest standard-unpadded-image-cards-keep-reserved-bands-and-anchor ()
  (let ((nle::*kitty-images-enabled* t)
        (nle::*color-profile* :none)
        (nle::*expanded-tool-call* "tight-image")
        (row (tool-row "eval" "{}" :tool-call-id "tight-image"
                       :text +ui-test-png-base64+
                       :metadata (nlk:json-object "image" (nlk:json-object "payload_kind" "base64" "source" "result" "media_type" "image/png")))))
    (dolist (nle::*tool-detail* '(:developer :standard))
      (let* ((block (nle::row-display-block row :width 80))
             (plains (coerce block.plain-lines 'vector))
             (refs (make-array (length plains) :initial-element row))
             (first-image (if (eq nle::*tool-detail* :standard) 1 2)))
        (is (nle::row-image-plist row))
        (is (nle::image-band-at refs plains first-image) "image band starts directly under its header")
        (is (null (nle::image-band-at refs plains (1- first-image))) "never paint over the header")
        (is (= (+ nle::*image-preview-rows* (if (eq nle::*tool-detail* :standard) 2 4)) (length plains)) "reserved image blanks are not structural padding")))))

(defun footer-leaves-the-blank-be (row)
  "ROW, an unpadded block whose body is one blank, composed in both viewports
with no live bar: the blank stays the band's one scrollable line."
  (with-frame-model (model 80 30 :session-id "content-blank")
    (dolist (mode '(:standard :developer))
      (setf (nle::chat-model-tool-detail model) mode)
      (with-stubbed-fdefinition (nle::live-status-line (ignored width))
        (nle::compose-transcript-viewport model 80 '("") '("") (list row))
        (is (= 1 (nle::band-window-count (nle::chat-model-band model))) "the body's blank remains scrollable content")
        (is (eq row (aref (nle::chat-model-line-rows model) 0)))
        (is (null (first (nle::status-blocks model))) "no live block adds rows of its own")))))

(deftest standard-footer-never-steals-an-unpadded-tools-owned-blank ()
  (footer-leaves-the-blank-be (tool-row "eval" "{}" :tool-call-id "tight-blank")))
;;; --- Standard completed work trail ----------------------------------------
(deftest standard-completed-turn-collapses-tools-and-thoughts-not-the-answer ()
  (let* ((nle::*tool-detail* :standard)
         (state (mixed-turn-state))
         (before (nle:project-state state :width 100)))
    ;; the run folds as its calls land, before the turn ends.
    (is (= 1 (count-if (lambda (block) (nle::tool-group-p block.row)) before)))
    (setf state (nle::ingest-turn-terminal state "completed" :turn-id "t1"))
    (let* ((blocks (nle:project-state state :width 100))
           (group (second blocks))
           (rows (mapcan (lambda (block) (copy-list (nle::display-block-plain-lines block))) blocks)))
      (is (= 3 (length blocks)) "prompt, work summary, answer: the answer's clock says the turn ended")
      (is (nle::tool-group-p (nle::display-block-row group)))
      (is (equal '(" ▸ Ran 4 commands") (nle::display-block-plain-lines group)))
      (is (not (rows-carry-p "Think" rows)))
      (is (not (rows-carry-p "Eval" rows)))
      (is (rows-carry-p "The answer." rows))
      (let ((nle::*tool-detail* :developer))
        (is (= 6 (length (nle:project-state state :width 100))) "Developer keeps its current grouping")))))

(deftest standard-completed-summary-reopens-members-in-chronological-order ()
  (let* ((nle::*tool-detail* :standard)
         (state (mixed-turn-state)))
    (setf state (nle::ingest-turn-terminal state "completed" :turn-id "t1"))
    (let* ((group (nle::display-block-row (second (nle:project-state state :width 100))))
           (nle::*expanded-tool-call* (nle::tool-group-key group))
           (rows (nle::display-block-plain-lines (second (nle:project-state state :width 100)))))
      ;; Opened, a run names its calls, in the order they ran; its thoughts
      ;; stand in no card of their own.
      (is (< (row-index-of "Read a.lisp" rows :required t) (row-index-of "Read b.lisp" rows :required t)
             (row-index-of "Searched 'needle'" rows :required t)
             (row-index-of "Edited a.lisp" rows :required t)))
      (is (not (rows-carry-p "plan it" rows)) "no Think card")
      (is (not (rows-carry-p "(sh " rows)) "members remain individual closed disclosures")
      (let ((nle::*expanded-tool-call* "c2"))
        (let ((rows (nle::display-block-plain-lines (second (nle:project-state state :width 100)))))
          (is (rows-carry-p "Ran 4 commands" rows) "opening a member keeps its parent open")
          (is (rows-carry-p "(sh \"cat b.lisp\")" rows))
          (is (not (rows-carry-p "(sh \"cat a.lisp\")" rows))))))))

(deftest standard-terminal-settlement-closes-an-already-open-disclosure ()
  (with-frame-model (model 100 40 :session-id "settle-summary")
    (dolist (mode '(:standard :developer))
      (setf model.tool-detail mode model.turn-in-flight t model.expanded-tool-call "open-call")
      (nle::note-turn-flight-settled model)
      (is (equal (if (eq mode :standard) nil "open-call") model.expanded-tool-call))
      (is (null model.turn-in-flight)))
    (setf model.tool-detail :standard model.expanded-tool-call '(:process "older-work"))
    (nle::note-turn-flight-settled model)
    (is (equal '(:process "older-work") model.expanded-tool-call) "replay/idle settlement does not erase an operator-opened old summary")))

(deftest standard-completion-counts-commands-only-and-keeps-a-failure-in-view ()
  ;; A turn's end is its answer's clock (REDUNDANT-FINISH-DIVIDER-P): no
  ;; outcome line stands under a turn that ended, and a failure keeps its
  ;; words, as an error notice.
  (dolist (outcome '("completed" "cancelled" "steered" "failed"))
    (let* ((nle::*tool-detail* :standard)
           (state (nle::ingest-message-fact nil "user" "ask" :turn-id "terminal-count")))
      (setf state (nle::ingest-message-fact state "assistant" "" :reasoning "plan" :turn-id "terminal-count")
            state (tool-round state "one" "eval" "{}" "result" :turn-id "terminal-count" :error-p t)
            state (nle::ingest-message-fact state "assistant" "answer" :turn-id "terminal-count")
            state (nle::ingest-turn-terminal state outcome :turn-id "terminal-count" :detail "visible failure"))
      (let ((rows (mapcan (lambda (block) (copy-list (nle::display-block-plain-lines block))) (nle:project-state state :width 100))))
        (is (rows-carry-p "Ran 1 command" rows))
        (is (not (rows-carry-p "Ran 1 commands" rows)))
        (is (rows-carry-p "answer" rows))
        (if (equal outcome "failed")
            (is (rows-carry-p "visible failure" rows) "a failure says why")
            (is (not (rows-carry-p outcome rows)) "an ending rides the answer's clock"))))))

(deftest standard-completed-summary-tail-never-cuts-a-work-trail ()
  (let* ((nle::*tool-detail* :standard)
         (state (nle::ingest-message-fact nil "user" "many commands" :turn-id "long-process")))
    (setf state (nle::ingest-message-fact state "assistant" "" :reasoning "plan" :turn-id "long-process"))
    (dotimes (i 100)
      (let ((id (format nil "many-~d" i)))
        (setf state (tool-round state id "eval" "{}" "2" :turn-id "long-process"))))
    (setf state (nle::ingest-message-fact state "assistant" "finished answer" :turn-id "long-process")
          state (nle::ingest-turn-terminal state "completed" :turn-id "long-process"))
    (dolist (budget '(1 4 7 20))
      (let* ((start (nle::tail-projection-start state 100 budget))
             (blocks (nle:project-state state :width 100 :start start)))
        (when (>= budget 7)
          (is (rows-carry-p "Ran 100 commands" (mapcar #'block-text blocks)) "the budget reaches the complete process summary"))
        (dolist (block blocks)
          (when (nle::tool-group-p (nle::display-block-row block))
            (let ((group (nle::display-block-row block)))
              (is (= 100 (count :tool-call (nle::tool-group-members group) :key #'nle::transcript-row-kind)))
              (is (search "Ran 100 commands" (block-text block))))))))))

(deftest standard-completed-summary-mouse-opens-and-closes-the-work-trail ()
  (with-frame-model (model 100 40 :session-id "summary-mouse" :tool-detail :standard
                     :state (nle::ingest-turn-terminal (mixed-turn-state) "completed" :turn-id "t1"))
    (click-at model 8 (mouse-y-for-text model "Ran 4 commands"))
    (is (rows-carry-p "Read a.lisp" (coerce model.line-plain 'list)))
    (click-at model 8 (mouse-y-for-text model "Read b.lisp"))
    (is (rows-carry-p "(sh \"cat b.lisp\")" (coerce model.line-plain 'list)))
    (click-at model 8 (mouse-y-for-text model "Ran 4 commands"))
    (is (not (rows-carry-p "Read a.lisp" (coerce model.line-plain 'list))) "parent click closes even with a member open")
    (is (rows-carry-p "The answer." (coerce model.line-plain 'list)))))

(deftest standard-summary-leaves-the-words-between-calls-under-it-and-separate-turns ()
  (let* ((nle::*tool-detail* :standard)
         (state (nle::ingest-message-fact nil "user" "first" :turn-id "one")))
    (setf state (tool-round state "a" "eval" "{}" "2" :turn-id "one")
          state (nle::ingest-message-fact state "assistant" "between calls" :turn-id "one")
          state (tool-round state "b" "eval" "{}" "4" :turn-id "one")
          state (nle::ingest-message-fact state "assistant" "final one" :turn-id "one")
          state (nle::ingest-turn-terminal state "completed" :turn-id "one")
          state (nle::ingest-message-fact state "user" "second" :turn-id "two")
          state (nle::ingest-message-fact state "assistant" "" :reasoning "still thinking" :turn-id "two"))
    (let* ((blocks (nle:project-state state :width 100))
           (group (nle::display-block-row (second blocks)))
           (members (nle::tool-group-members group)))
      (is (equal '(:tool-call :tool-call) (mapcar #'nle::transcript-row-kind members)))
      (is (equal '("final one")
                 (loop for block in blocks
                       for row = (nle::display-block-row block)
                       when (and (nle::transcript-row-p row) (member (nle::transcript-row-text row) '("between calls" "final one") :test #'equal))
                         collect (nle::transcript-row-text row))) "the newest words stand under the run, in the place the words before them held")
      (is (rows-carry-p "Thought" (mapcar #'block-text blocks)) "the live turn's thought folds under a run of its own")
      (is (equal '("two") (mapcar #'nle::transcript-row-turn-id
                                  (nle::tool-group-members (nle::display-block-row (car (last blocks)))))))
      (is (= 2 (count-if (lambda (b) (nle::tool-group-p (nle::display-block-row b))) blocks)))
      (dolist (nle::*tool-detail* '(:developer :standard :developer :standard))
        (let* ((rows (mapcar #'block-text (nle:project-state state :width 100))))
          (is (eq (eq nle::*tool-detail* :standard) (not (null (rows-carry-p "Ran 2 commands" rows)))) "mode changes never reuse the other mode's group title"))))))

(deftest standard-thought-only-turn-says-it-thought ()
  (let* ((nle::*tool-detail* :standard)
         (state (nle::ingest-message-fact nil "user" "think" :turn-id "zero")))
    (setf state (nle::ingest-message-fact state "assistant" "" :reasoning "plan" :turn-id "zero")
          state (nle::ingest-message-fact state "assistant" "answer" :turn-id "zero")
          state (nle::ingest-turn-terminal state "completed" :turn-id "zero"))
    (let ((rows (mapcar #'block-text (nle:project-state state))))
      (is (rows-carry-p "Thought" rows) "a run with no commands is named by its thinking")
      (is (not (rows-carry-p "Ran 0" rows))))))

(deftest standard-steered-handoff-closes-disclosures-without-ending-flight ()
  (with-frame-model (model 100 40 :session-id "handoff-summary")
    (setf model.tool-detail :standard model.turn-in-flight t model.turn-started-at 123
          model.expanded-tool-call "opened-call" (nle::gateway-model-active-turn-id model) "old-turn")
    (nle::note-turn-flight-settled model :handoff t)
    (is (null model.expanded-tool-call))
    (is model.turn-in-flight "the promoted turn cannot fall through an idle gap")
    (is-shape model (.turn-started-at = 123) (nle::gateway-model-active-turn-id null))))

(deftest gateway-terminal-frame-collapses-standard-work-through-the-real-consumer ()
  (dolist (mode '(:standard :developer))
    (dolist (steered '(nil t))
      (with-frame-model (model 100 40 :session-id "wire-summary")
        (setf model.tool-detail mode model.state (mixed-turn-state)
              model.turn-in-flight t model.expanded-tool-call "c2"
              (nle::gateway-model-active-turn-id model) "t1")
        (let* ((facts (nlk:json-object "turn_id" "t1"))
               (event (nlk::encode-inner-event "turn.completed" facts
                       :metadata (nlk:json-object "turn_id" "t1" "occurred_at" "2026-10-11T00:00:01.000Z"))))
          (when steered (setf (gethash "steered" facts) t))
          (let ((frame (nlk:decode-sync-message
                        (nlk:encode-json-object
                         (nlk:json-object "sequence" 1 "cursor" 1 "session_id" "wire-summary"
                                          "payload" (nlk:json-object "type" "session_stream_event"
                                                     "frame" (nlk:json-object "seq" 1 "event" event)))))))
            (nlt:update model (make-instance 'nle::gw-projection-msg :frame frame))))
        (nle::render-state model)
        (is (equal (if (eq mode :standard) nil "c2") model.expanded-tool-call))
        (is (eq steered (and model.turn-in-flight t)) "handoff retains flight; a normal completion settles it")
        (is (eq (eq mode :standard) (rows-carry-p "Ran 4 commands" model.line-plain)))
        (is (rows-carry-p "The answer." (coerce model.line-plain 'list)))))))

;;; --- Think previews use the existing Markdown formatter --------------------
(deftest standard-think-preview-renders-bold-markdown-not-literal-delimiters ()
  (let* ((nle::*tool-detail* :standard)
         (nle::*color-profile* :truecolor)
         (row (nle::make-transcript-row :kind :reasoning :hydrated t :text "**Testing palette functionality**")))
    (multiple-value-bind (painted plains) (nle::llines-rows (nle::row-llines row 100) 100)
      (is (equal '("   Think    Testing palette functionality") plains))
      (let ((screen (nlt:parse-styled-string painted 100 1)))
        (is (plusp (logand nlt::+attr-bold+ (nlt:cell-attrs (nlt:screen-buffer-ref screen 13 0)))) "the preview's text is actually bold")))))

(deftest standard-think-preview-preserves-canonical-markdown-styles-and-link-columns ()
  (dolist (profile '(:truecolor :ansi256 :ansi :none))
    (let ((nle::*tool-detail* :standard) (nle::*color-profile* profile))
      (dolist (text '("## Testing palette functionality" "**Bold** and *italic* with `code`" "[λ 界 link](https://example.org/path)"))
        (let ((row (nle::make-transcript-row :kind :reasoning :hydrated t :text text))
              (previous nil))
          (dotimes (pass 2)
            (multiple-value-bind (painted plains) (nle::llines-rows (nle::row-body-llines row 100) 100)
              (when previous (is (eq previous (first plains)) "memo hits return the same link-metadata key"))
              (setf previous (first plains))
              (is (not (rows-carry-p "**" plains)))
              (is-table (in) (not (rows-carry-p in plains)) ("## ") ("`code`") ("](https://"))
              (when (search "link" text)
                (let* ((plain (first plains))
                       (range (first (nle::plain-row-links plain)))
                       (index (search "λ" plain))
                       (column (nle::text-display-width (subseq plain 0 index)))
                       (screen (nlt:parse-styled-string painted 100 1)))
                  (is (equal "   Think    λ 界 link" plain))
                  (is range "fresh and memoized previews retain mouse link metadata")
                  (is-shape range (first = index) (third "https://example.org/path"))
                  (unless (eq profile :none)
                    (is (equal "https://example.org/path" (nlt:cell-link (nlt:screen-buffer-ref screen column 0))))))))))))))

(deftest standard-think-preview-renders-the-streaming-tail-and-settled-head ()
  (let* ((nle::*tool-detail* :standard)
         (nle::*color-profile* :none)
         (text (format nil "**First heading**~%~%*Current tail*~%")))
    (dolist (hydrated '(nil t))
      (let* ((row (nle::make-transcript-row :kind :reasoning :hydrated hydrated :text text))
             (plains (nth-value 1 (nle::llines-rows (nle::row-llines row 100) 100))))
        (is (equal (list (if hydrated "   Think    First heading" "   Think    Current tail")) plains))))))

(deftest standard-think-preview-does-not-change-open-thoughts-or-developer ()
  (let* ((text (format nil "**Testing palette functionality**~%~%Use `code`."))
         (row (nle::make-transcript-row :kind :reasoning :hydrated t :text text))
         (nle::*expanded-tool-call* row))
    (let* ((nle::*tool-detail* :developer)
           (developer (nle::row-llines row 100)))
      (let ((nle::*tool-detail* :standard))
        (is (equalp developer (nle::row-llines row 100)) "opened thoughts use the exact same Markdown pipeline")))))

;;; --- Successful mutation notices are named, live-only disclosures ----------
(defun mutate-row (text comment &rest keys)
  "A system-notice row of TEXT carrying a Mutate summary with COMMENT, under KEYS."
  (apply #'nle::make-transcript-row :kind :system-notice :text text :metadata
         (nlk:json-object "summary" (nlk:json-object "name" "Mutate" "comment" comment)) keys))

(deftest mutation-notice-is-a-compact-comment-disclosure-in-both-viewports ()
  (let* ((details "[ui.lisp — this shell: live here: group-rows, project-state]")
         (row (mutate-row details "Fold finished work" :retention :live-only)))
    (dolist (nle::*tool-detail* '(:standard :developer))
      (dotimes (pass 2)
        (let ((plains (nle::display-block-plain-lines (nle::row-display-block row :width 100))))
          (is (rows-carry-p (activity-label-text "Mutate   Fold finished work" "Mutate · Fold finished work") plains))
          (is (not (rows-carry-p "group-rows" plains)))
          (is (not (rows-carry-p "■" plains))))))))

(deftest mutation-notice-click-opens-and-closes-its-full-load-receipt ()
  (with-frame-model (model 100 40 :session-id "mutation-click")
    ;; Under a prompt: a promptless short band stands at the bottom, and an
    ;; opened title keeps its row there, its receipt below the band
    ;; (DISCLOSURE-EXPANSION-RETAINS-CLICKED-TITLE-SCREEN-ROW).
    (fold-model model (nle::ingest-message-fact "user" "hello"))
    (let ((row (mutate-row "[ui.lisp — this shell: live here: group-rows, project-state]"
                           "Fold finished work" :retention :live-only)))
      (nle::append-tail-row (nle::chat-model-state model) row)
      (nle::render-state model)
      (is (nle::togglable-item-p model row))
      (click-at model 12 (frame-row-carrying model (activity-label-text "Mutate   Fold finished work" "Mutate · Fold finished work")))
      (is (eq row (nle::chat-model-expanded-tool-call model)))
      (is (rows-carry-p "group-rows, project-state" (nle::compose-frame model)))
      (click-at model 12 (frame-row-carrying model (activity-label-text "Mutate   Fold finished work" "Mutate · Fold finished work")))
      (is (null (nle::chat-model-expanded-tool-call model)))
      (is (not (rows-carry-p "group-rows" (nle::compose-frame model)))))))

(deftest mutation-summary-does-not-hide-errors-or-ordinary-system-notices ()
  (dolist (error-p '(nil t))
    (let* ((row (nle::make-transcript-row :kind :system-notice :text "Important diagnostic" :error-p error-p
                 :metadata (and error-p (nlk:json-object "summary" (nlk:json-object "name" "Mutate" "comment" "Try rewrite")))))
           (plains (nle::display-block-plain-lines (nle::row-display-block row :width 100))))
      (is (rows-carry-p "Important diagnostic" plains))
      (is (not (rows-carry-p "Mutate" plains))))))

;;; --- Summarized mutation blocks own no structural blank padding ------------
(deftest mutation-blocks-have-no-top-bottom-or-inter-notice-padding ()
  (let ((rows (loop for note in '("First rewrite" "Second rewrite" "Third rewrite")
                   collect (mutate-row "receipt" note :retention :live-only))))
    (dolist (nle::*tool-detail* '(:standard :developer))
      (let ((lines (mapcan (lambda (row)
                            (copy-list (nle::display-block-plain-lines (nle::row-display-block row :width 100)))) rows)))
        (is (= 3 (length lines)) "each closed mutation is exactly one painted row")
        (is (equal (list (activity-label-text "   Mutate   First rewrite" "   Mutate · First rewrite") (activity-label-text "   Mutate   Second rewrite" "   Mutate · Second rewrite") (activity-label-text "   Mutate   Third rewrite" "   Mutate · Third rewrite")) lines))
        (is (every (lambda (row) (not (nle::row-block-padding-p row nle::*tool-detail*))) rows))))))

(deftest mutation-padding-stays-gapless-on-mode-switch-and-memo-hits ()
  (let ((row (mutate-row "receipt" "Rewrite")))
    (dolist (nle::*tool-detail* '(:developer :standard :developer :standard))
      (dotimes (pass 2)
        (is (= 1 (length (nle::display-block-plain-lines (nle::row-display-block row :width 100)))))
        (is (null (nle::row-block-fill row)) "compact mutation sits on the canvas, not a padded system fill")))))

(deftest mutation-spacing-keeps-open-receipt-blanks-and-ordinary-notice-padding ()
  (let* ((row (mutate-row (format nil "receipt one~%~%receipt two") "Rewrite")))
    (let ((nle::*expanded-tool-call* row))
      (let ((lines (nle::display-block-plain-lines (nle::row-display-block row :width 100))))
        (is (= 4 (length lines)) "header and three receipt rows, no structural pads")
        (is (search (activity-label-text "Mutate   Rewrite" "Mutate · Rewrite") (first lines)))
        (is (search "receipt one" (second lines)))
        (is (string= "" (string-trim " " (third lines))))
        (is (search "receipt two" (fourth lines)))))
    (dolist (error-p '(nil t))
      (let ((ordinary (nle::make-transcript-row :kind :system-notice :text "Notice" :error-p error-p
                        :metadata (and error-p (nlk:json-object "summary" (nlk:json-object "name" "Mutate" "comment" "Rewrite"))))))
        (is (nle::row-block-padding-p ordinary))
        (is (= 3 (length (nle::display-block-plain-lines (nle::row-display-block ordinary :width 100)))))))))

;;; --- Transcript labels have distinct, palette-owned semantic colors --------
;; Whether CELL's foreground is TONE's (at its shadow when SHADOW) under PROFILE.
(defun tone-fg-p (cell tone &key shadow (profile :truecolor))
  (let ((rgb (nle::visual-style-fg (nle:style-for-tone tone))))
    (equal (nle::color-sgr (if shadow (nle::shadow-rgb rgb) rgb) t profile)
           (and cell (nlt:cell-fg cell)))))
(deftest think-label-paints-the-blue-complement-of-evals-orange ()
  (let* ((nle::*tool-detail* :standard)
         (nle::*color-profile* :truecolor)
         (tool '(252 168 100))
         (opposite '(100 184 252))
         (nle::*palette* (nle::palette-from-theme-colors
                          (alexandria:plist-hash-table (list :syntax-string tool) :test 'eq)))
         (row (nle::make-transcript-row :kind :reasoning :hydrated t :text "**Testing palette functionality**")))
    (dotimes (pass 2)
      (multiple-value-bind (painted plains) (nle::llines-rows (nle::row-body-llines row 100) 100)
        (is (equal '("   Think    Testing palette functionality") plains))
        (let ((screen (nlt:parse-styled-string painted 100 1)))
          (is (equal (nle::color-sgr opposite t :truecolor) (nlt:cell-fg (nlt:screen-buffer-ref screen 3 0))))
          (is (plusp (logand nlt::+attr-bold+ (nlt:cell-attrs (nlt:screen-buffer-ref screen 3 0)))))
          (is (tone-fg-p (nlt:screen-buffer-ref screen 13 0) :thought-strong :shadow t) "Markdown strong text keeps the thought's strong tone, shadowed"))))))

(deftest mutate-label-paints-success-green-without-recoloring-the-comment ()
  (let* ((nle::*color-profile* :truecolor)
         (green '(101 163 13))
         (nle::*palette* (nle::palette-from-theme-colors
                          (alexandria:plist-hash-table (list :success green) :test 'eq)))
         (row (mutate-row "full receipt" "Rewrite definitions" :retention :live-only)))
    (dolist (nle::*tool-detail* '(:standard :developer))
      (dotimes (pass 2)
        (let* ((lines (nle::display-block-lines (nle::row-display-block row :width 100)))
               (screen (nlt:parse-styled-string lines 100 1)))
          (is (= 1 (length lines)))
          (is (equal (nle::color-sgr green t :truecolor) (nlt:cell-fg (nlt:screen-buffer-ref screen 3 0))))
          (is (plusp (logand nlt::+attr-bold+ (nlt:cell-attrs (nlt:screen-buffer-ref screen 3 0)))))
          (is (tone-fg-p (nlt:screen-buffer-ref screen 12 0) :muted)))))))

(deftest eval-label-keeps-the-existing-orange-tool-tone ()
  (let* ((nle::*tool-detail* :standard) (nle::*color-profile* :truecolor)
         (row (nle::make-transcript-row :kind :tool-call :tool-name "eval"
                :tool-call-id "color-eval" :tool-status "complete"
                :tool-args (eval-args (format nil ";; Check color~%(+ 1 1)"))))
         (screen (nlt:parse-styled-string (nle::llines-rows (nle::row-llines row 100) 100) 100 1)))
    (is (tone-fg-p (nlt:screen-buffer-ref screen 3 0) :tool))))

(deftest thought-labels-keep-their-tone-across-viewport-and-terminal-profiles ()
  (let ((nle::*palette* (nle::palette-from-theme-colors (nle:theme-colors-for-name "Tungstene"))))
    (dolist (nle::*color-profile* '(:truecolor :ansi256 :ansi :none))
      (dolist (nle::*tool-detail* '(:standard :developer))
        (dolist (hydrated '(nil t))
          (let* ((row (nle::make-transcript-row :kind :reasoning :hydrated hydrated :text "Think text" :thought-ms (and hydrated 1200)))
                 (painted (nth-value 0 (nle::llines-rows (nle::row-llines row 100) 100)))
                 (cell (nlt:screen-buffer-ref (nlt:parse-styled-string painted 100 5) 3 0)))
            (unless (eq nle::*color-profile* :none)
              (is (plusp (logand nlt::+attr-bold+ (nlt:cell-attrs cell))))
              (is (tone-fg-p cell :thought :profile nle::*color-profile*)))))))))

;;; --- Think blocks have no structural padding; original text styling stays ---
(deftest think-blocks-have-no-top-bottom-gaps-in-either-viewport ()
  (dolist (mode '(:standard :developer))
    (let ((nle::*tool-detail* mode) (nle::*color-profile* :none))
      (dolist (hydrated '(nil t))
        (let ((row (nle::make-transcript-row :kind :reasoning :text "Think text" :hydrated hydrated)))
          (dotimes (pass 2)
            (is (not (nle::row-block-padding-p row mode)))
            (let ((body (nth-value 1 (nle::llines-rows (nle::row-body-llines row 100) 100)))
                  (block (nle::display-block-plain-lines (nle::row-display-block row :width 100))))
              (is (equal body block) "no structural padding above or below the thought"))))))))

(deftest standard-think-eval-and-mutate-are-consecutive-owned-clickable-rows ()
  (with-frame-model (model 100 40 :session-id "think-tight" :tool-detail :standard)
    (let ((think (nle::make-transcript-row :kind :reasoning :text (format nil "First thought~%Second thought") :hydrated t))
          (eval (nle::make-transcript-row :kind :tool-call :tool-name "eval" :tool-call-id "tight-eval" :tool-status "complete"
                  :tool-args (eval-args (format nil ";; Check result~%(+ 1 1)")) :text "2"))
          (mutate (mutate-row "full receipt" "Rewrite" :retention :live-only)))
      (dolist (row (list think eval mutate)) (nle::append-tail-row (nle::chat-model-state model) row))
      (nle::render-state model)
      (is (= (1+ (frame-row-carrying model "Think    First thought")) (frame-row-carrying model "Eval     Check result")))
      (is (= (1+ (frame-row-carrying model "Eval     Check result")) (frame-row-carrying model (activity-label-text "Mutate   Rewrite" "Mutate · Rewrite"))))
      (click-at model 12 (frame-row-carrying model "Think    First thought"))
      (is (eq think (nle::chat-model-expanded-tool-call model)))
      (is (rows-carry-p "Second thought" (nle::compose-frame model)))
      (click-at model 12 (frame-row-carrying model "First thought"))
      (is (null (nle::chat-model-expanded-tool-call model))))))

(deftest think-footer-never-steals-an-unpadded-thoughts-content-blank ()
  (footer-leaves-the-blank-be (nle::make-transcript-row :kind :reasoning :text "Thought")))
(deftest think-text-reversion-keeps-original-markdown-colors-shadow-and-links ()
  (let* ((nle::*color-profile* :truecolor)
         (nle::*palette* (nle::palette-from-theme-colors (nle:theme-colors-for-name "Tungstene")))
         (row (nle::make-transcript-row :kind :reasoning :hydrated t
                :text "**Bold** plain [λ 界](https://example.org/think)")))
    (dolist (nle::*tool-detail* '(:standard :developer :standard))
      (when (eq nle::*tool-detail* :standard)
        (dotimes (pass 2)
          (multiple-value-bind (painted plains) (nle::llines-rows (nle::row-body-llines row 100) 100)
            (let* ((plain (first plains))
                   (screen (nlt:parse-styled-string painted 100 1)))
              (is (equal "   Think    Bold plain λ 界" plain))
              (is (tone-fg-p (nlt:screen-buffer-ref screen 3 0) :thought))
              (is (tone-fg-p (nlt:screen-buffer-ref screen 13 0) :thought-strong :shadow t))
              (is (tone-fg-p (nlt:screen-buffer-ref screen 18 0) :foreground :shadow t))
              (is (equal "https://example.org/think" (third (first (nle::plain-row-links plain)))))
              (is (equal "https://example.org/think" (nlt:cell-link (nlt:screen-buffer-ref screen (search "λ" plain) 0))))))))
      (let ((nle::*expanded-tool-call* row))
        (is (equalp (nle::row-markdown-llines row (nle::transcript-row-text row) :shadow t)
                    (nle::row-llines row 100)))
        (is (every #'nle::lline-shadow (nle::row-llines row 100)))))))

(deftest gapless-open-thought-keeps-only-its-content-owned-blank-lines ()
  (dolist (nle::*tool-detail* '(:standard :developer))
    (let* ((row (nle::make-transcript-row :kind :reasoning :hydrated t :text (format nil "first~%~%last")))
           (nle::*expanded-tool-call* row)
           (lines (nle::display-block-plain-lines (nle::row-display-block row :width 100))))
      (is (equal '("   first" "   " "   last") lines)))))


(deftest mutation-disclosure-clickability-is-a-strict-boolean ()
  ;; Running hover/click callers compiled before Mutate disclosures retain
  ;; a Boolean result check. Metadata truthiness breaks that live contract.
  (let ((summary (nlk:json-object "name" "Mutate" "comment" "Hover safely")))
    (dolist (kind '(:system-notice :tool-call :reasoning :text))
      (dolist (error-p '(nil t))
        (let* ((row (nle::make-transcript-row :kind kind :error-p error-p
                      :metadata (nlk:json-object "summary" summary)))
               (expected (and (or (member kind '(:tool-call :reasoning))
                                  (and (eq kind :system-notice) (not error-p))) t)))
          (is (eq expected (nle::togglable-item-p nil row)) "clickability returns exactly T or NIL, never summary metadata")
          (when (and (eq kind :system-notice) (not error-p))
            (is (eq summary (nle::row-notice-summary row)) "the metadata reader retains the full summary object")))))))

(deftest pinned-user-boundary-keeps-close-pad-and-canvas-gap ()
  (let ((nle::*color-profile* :truecolor) (nle:*palette* (theme-palette "Tungstene")))
    (dolist (mode '(:standard :developer))
      (dolist (height '(8 10 12 24 40))
        (with-frame-model (model 80 height :session-id "pin-user-gap")
          (let* ((state (nle::ingest-message-fact nil "user"
                         (format nil "~{prompt ~d~^~%~}" (loop for i below 10 collect i)) :turn-id "pin-gap"))
                 (state (tool-round state "pin-gap-call" "eval"
                                    (eval-args (format nil ";; Inspect pinned boundary~%(+ 1 1)")) "2" :turn-id "pin-gap")))
            (setf (nle::chat-model-tool-detail model) mode (nle::chat-model-state model) state)
            (nle::render-state model)
            (let* ((window (nle::chat-model-band model))
                   (visible (nle::band-window-visible window (nle::projection-width model)))
                   (screen (nlt:parse-styled-string (nle::compose-frame model) 80 height)))
              (is (<= (length visible) (nle::band-window-height window)) "small heights never overdraw the pin budget")
              (when (>= (nle::band-window-height window) 7)
                (let* ((span (first (nle::band-window-prompt-spans window)))
                       (pinned (nth-value 1 (nle::band-window-pinned window)))
                       ;; A cut pin paints the closing pad under its rows; a
                       ;; band with room for the whole prompt ends the pin on it.
                       (pad (if (rows-carry-p "lines omitted]" visible) pinned (1- pinned))))
                  (is (eql (1- (cdr span)) (nle::band-window-row-index window pad)) "the pinned prompt still closes with its own pad")
                  (is (null (nle::band-window-row-index window (1+ pad))) "the following canvas row targets no prompt or tool")
                  (is (string= "" (nth (1+ pad) visible)) "canvas separation survives pinned slicing")
                  (is (not (equal (cell-bg-at screen 8 (+ 3 pad)) (cell-bg-at screen 8 (+ 4 pad)))) "the filled user pad and canvas gap remain distinct")
                  (is (equal (cell-bg-at screen 0 0) (cell-bg-at screen 8 (+ 4 pad))))
                  (is (eql (1+ (cdr span)) (nle::band-window-row-index window (+ 2 pad))) "the following tool starts after one shared canvas gap, not a duplicate"))))))))))


(deftest user-prose-and-pinned-omission-retain-fill ()
  (dolist (profile '(:none :truecolor :ansi256 :ansi))
    (dolist (palette (list (theme-palette "Tungstene") (theme-palette "Blueprint Daylight")))
      (dolist (mode '(:standard :developer))
      (dolist (width '(20 80))
        (let* ((nle::*color-profile* profile) (nle::*tool-detail* mode) (nle:*palette* palette)
               (fill (fill-bg :user-block width))
               (row (nle::make-transcript-row :kind :text :role "user"
                        :text (format nil "α界 prompt~%second line"))))
          (is (eq :user-block (nle::row-block-fill row)))
          (dolist (hover '(nil t nil))
            (let* ((nle::*hover-row* (and hover row))
                   (block (nle::row-display-block row :width width))
                   (again (nle::row-display-block row :width width))
                   (lines (nle::display-block-lines block))
                   (buffer (nlt:parse-styled-string lines width (length lines))))
              (is (eq block again) "memo hits preserve the filled block")
              (is (every (lambda (line) (and (eq :user-block (nle::lline-fill line))
                                             (eq :user-block (nle::lline-tone line))))
                         (nle::row-llines row width)) "user text uses its filled surface")
              (let ((fill (fill-bg (if hover :user-block-hover :user-block) width)))
                (is (loop for y below (length lines) always
                          (loop for x below width always (equal fill (cell-bg-at buffer x y)))) "body and both pads retain the user surface"))
              (unless (eq profile :none)
                (let ((tone (if hover :user-block-hover :user-block))
                      (cell (loop for y below (length lines) thereis
                                  (loop for x below width
                                        for cell = (nlt:screen-buffer-ref buffer x y)
                                        when (char= #\α (nlt:cell-char cell)) return cell))))
                  (is cell "the foreground check targets body prose, not the muted clock")
                  (is (tone-fg-p cell tone :profile profile) "hover retains the user surface foreground")))
              (is (eq :user-block (nle::lline-tone (first (nle::row-llines row width)))) "hover leaves the shared base line untouched")))
          (let ((window (nle::make-band-window :height 1)))
            ;; Force the real viewport's synthetic omission branch, retaining its owner.
            (with-saved-globals (((symbol-function 'nle::band-window-pieces)
                                  (lambda (ignored) (declare (ignore ignored)) '((:omitted 12 0)))))
            (dolist (hover '(nil 0))
              (let* ((lines (nle::band-window-visible window width hover))
                     (buffer (nlt:parse-styled-string lines width 1)))
                (is (search "[+12" (first lines)))
                (is (null (nle::band-window-row-index window 0)))
                (is (eql 0 (nle::band-window-row-index window 0 t)))
                (let ((expected (if hover (fill-bg :user-block-hover width) fill)))
                  (is (loop for x below width always (equal expected (cell-bg-at buffer x 0))) "a pinned omission retains the user fill"))
                (when (and hover (not (eq profile :none)))
                  (let ((column (loop for x below width
                                      when (char= #\[ (nlt:cell-char (nlt:screen-buffer-ref buffer x 0))) return x)))
                    (is column)
                    (is (equal (nle::color-sgr (nle::visual-style-fg (nle:style-for-tone :user-block-hover)) t profile)
                               (nlt:cell-fg (nlt:screen-buffer-ref buffer column 0))) "pinned omission shares the user hover surface")))))))))))))
