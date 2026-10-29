;;;; exec-test.lisp --- background-evaluation substrate gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Foundation claims for the registry (exec.lisp), the gateway watcher's
;;;; durable exit fact, the live-only frame lexicon, and the client band
;;;; reducer:
;;;;
;;;;   E1. spawn -> output -> exit -> durable exit fact -> replay
;;;;       convergence (live band suppression + durable restamp).
;;;;   E2. registry cleanup (reap after both observers).
;;;;   E3. ring trim keeps a parseable boundary.
;;;;   E4. golden wire fixtures fold and the encoder round-trips.
;;;;   E5. the inline await lane honors the tool call's yield window.
;;;;   E6. sh children lead their own sessions; a job-control-stopped
;;;;       tree collapses with a named verdict instead of hanging forever.
;;;;   E7. an exited backgrounded evaluation wakes its completed turn's
;;;;       session exactly once: a turn no one typed, opening on an
;;;;       (eval-status ID) call whose result carries the exit.

(in-package #:nodecode.test)

(nlk:access (band nle::bg-band-row) (final nle::exec-update) (registry nle::exec-registry))

;;; --- fixtures ---------------------------------------------------------------

(defun drain-exec-registry (&aux (registry (nle::exec-registry-instance)))
  "Kill and forget every process in the global registry so tests stay
isolated. Wait threads finish on their own after the kill."
  ;; The registry's own reclaim, the one gateway shutdown takes: every entry
  ;; leaves the list, each running one is stopped and its thread joined, so a
  ;; drained image costs no fixed tick.
  (nle::destroy-all-execs registry)
  registry)

(defmacro with-exec-registry ((registry-var) &body body)
  "Run BODY against a drained global registry and drain again afterwards."
  `(let ((,registry-var (drain-exec-registry)))
     (declare (ignorable ,registry-var))
     (nlk:with-cleanup ((drain-exec-registry)) ,@body)))

(defun spawn-test-eval (registry &key (prints "") (linger 0) (value "NIL") form
                                   (session-id "local") turn-id (tool-call-id ""))
  "Register a background evaluation of FORM, a test-eval form unless named, that
prints PRINTS, stays alive LINGER seconds and answers VALUE. Returns its id."
  (nle::spawn-background-eval
   registry (or form (format nil "(test-eval ~s ~a)" prints linger))
   (lambda (stream)
     (write-string prints stream)
     (force-output stream)
     (when (plusp linger) (sleep linger))
     ;; a function VALUE is called there, on the evaluating thread
     (values (if (functionp value) (funcall value) value) nil nil))
   :session-id session-id :turn-id turn-id :tool-call-id tool-call-id))

(defun snippet-exit-fact (session-id &optional call-id)
  "The durable snippet tool_result fact in SESSION-ID's log — the background
evaluation's exit fact — narrowed to CALL-ID when one is named."
  ;; The watcher, the terminal-turn appender and the shutdown ladder all prove
  ;; themselves by this one read, so it is a fixture, not an assertion.
  (find-if (lambda (event)
             (and (typep event 'nlk:tool-result-event)
                  (equal "snippet" (nlk:tool-result-event-tool-name event))
                  (or (null call-id)
                      (equal call-id (nlk:tool-result-event-call-id event)))))
           (nle::collect-visible-session-events session-id)))

(defun detached-turn (session-id turn-id)
  "A durable turn naming TURN-ID of SESSION-ID, admitted by no one here."
  (nlk::%make-durable-turn :session-id session-id :turn-id turn-id
                           :command-id "c" :definition-revision "test"))

(defun tool-call-round (id tool &rest arguments)
  "An assistant round calling TOOL once with ARGUMENTS, MAKE-JSON-OBJECT's keys and values."
  (let ((json (nlk:encode-json-object (apply #'nlk:make-json-object arguments))))
    (wire-message "assistant" :tool-calls (vector (wire-tool-call id tool json)))))

(defun string-octets (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

;;; --- sanitizer --------------------------------------------------------------

(deftest exec-sanitizer-strips-controls-and-survives-torn-chunks ()
  (let ((sanitizer (nle::make-text-sanitizer)))
    (flet ((feed (text)
             (nle::octets-to-text (nle::sanitize-octets sanitizer (string-octets text)))))
      (is (equal "red" (feed (format nil "~c[38;2;255;0;0mred~c[0" #\Escape #\Escape))))
      (is (equal (format nil " plain~cnext" #\Newline)
                 (feed (format nil "m plain~c~cnext" #\Return #\Newline))))))
  (let ((sanitizer (nle::make-text-sanitizer)))
    ;; U+2500 torn across two chunks must arrive whole.
    (nle::sanitize-octets sanitizer (octets 226 148))
    (is (equal "─" (nle::octets-to-text
                    (nle::sanitize-octets sanitizer (octets 128)))))))

(deftest exec-sanitizer-counts-what-it-removes ()
  ;; SGR on, SGR off, an OSC title closed by ST, a bare BEL, a CSI aborted
  ;; by the next ESC, and that ESC's own SGR: six controls. CRLF is a
  ;; rewrite, not a drop.
  (let ((esc (string #\Escape)))
    (multiple-value-bind (clean dropped)
        (nle::sanitize-octets
         (nle::make-text-sanitizer)
         (string-octets (format nil "~a[38;2;231;229;228mReady~a[0m~a]0;t~a\\~c~a[3~a[1mx~c~c"
                                esc esc esc esc #\Bel esc esc #\Return #\Newline)))
      (is (equal (format nil "Readyx~c" #\Newline) (nle::octets-to-text clean)))
      (is (= 6 dropped) "every removed control sequence is counted"))
    (is (= 0 (nth-value 1 (nle::sanitize-octets (nle::make-text-sanitizer)
                                                 (string-octets "plain"))))))
  (is (equal "plain" (nle::note-removed-controls "plain" 0)))
  (is (equal (format nil "Ready~%[2 terminal control sequences removed by the output sanitizer]")
             (nle::note-removed-controls "Ready" 2))))

;;; --- the printed value is the model's lane too ------------------------------

(deftest eval-value-reaches-the-model-sanitized (with-exec-registry (registry))
  ;; A form RETURNING terminal bytes — a
  ;; (uiop:run-program ... :output :string) — would otherwise ride into the
  ;; result as its printed value while only the stream was sanitized; the
  ;; model would read raw escapes and the TUI would paint them. The value
  ;; is clean at FINISH-BACKGROUND-EVAL, once, so the in-window result and
  ;; the follow-up plists say the same thing.
  (is-carrying (result (nle::background-capable-eval
                        "(format nil \"~c[0;91mERROR: ~c[1;97mFailed to get size of terminal!~c[0m~c~%\" (code-char 27) (code-char 27) (code-char 27) (code-char 13))"
                        5000))
    (is (not (find #\Escape result)) "no ESC reaches the model")
    ("ERROR: Failed to get size of terminal!" "the printed text survives")
    ("[3 terminal control sequences removed by the output sanitizer]"
     "the removal is disclosed with its count (CRLF is a rewrite, not a drop)")))

(deftest eval-await-value-is-the-same-clean-text (with-exec-registry (registry))
  (let ((id (spawn-test-eval registry :value (format nil "\"~c[31mred~c[0m\"" #\Escape #\Escape))))
    (let ((plist (nle::eval-await id :until-exit t :yield-ms 5000)))
      (is (eq :exited (getf plist :status)) "the eval exited")
      (is (equal (nle::note-removed-controls "\"red\"" 2) (getf plist :value))))))

(deftest disclose-sanitized-text-is-the-string-contract ()
  (is (equal "plain" (nle::disclose-sanitized-text "plain")))
  (is (equal (format nil "x~%[1 terminal control sequence removed by the output sanitizer]")
             (nle::disclose-sanitized-text (format nil "x~c[2J" #\Escape))))
  (is (equal "a?b" (nle::disclose-sanitized-text
                    (format nil "a~cb" (code-char #xD800))))))

;;; --- ring trim --------------------------------------------------------------

(deftest exec-ring-trim-keeps-parseable-boundary (let ((registry (nle::make-exec-registry))
                                                       (process (nle::make-exec-process))))
  ;; 1 MiB + spill of 'a' with an ESC every 4096 bytes: the trim must snap
  ;; its drop point to an ESC so the retained stream stays sequence-aligned.
  (let ((chunk (make-array 4096 :element-type '(unsigned-byte 8)
                                :initial-element (char-code #\a))))
    (setf (aref chunk 0) #x1b)
    (loop repeat 260
          do (nle::exec-append-output registry process chunk)))
  (let ((output process.output))
    (is (<= (fill-pointer output) nle::+exec-max-output-bytes+))
    (is (plusp process.base-offset))
    (is (= (nle::exec-total-len process) (* 260 4096)))
    (is (= #x1b (aref output 0)))))

;;; --- registry cleanup -------------------------------------------------------

(deftest exec-registry-reaps-after-both-observers (with-exec-registry (registry))
  (let ((id (spawn-test-eval registry :prints "done")))
    (nle::exec-park-until registry id 5000)
    ;; The model observes the exit first; the projection has not, so the
    ;; process must survive this snapshot.
    (let ((snapshot (nle::exec-snapshot-since registry id 0)))
      (is (eq :exited (getf snapshot :status)) "the evaluation exited")
      (is (equal "done" (getf snapshot :output)) "output was captured"))
    (is (= 1 (length registry.processes)))
    ;; The watcher's collection projects the exit; both observers have now
    ;; seen it, so the registry reaps.
    (let ((updates (nle::exec-collect-updates registry)))
      (is (plusp (length updates)) "the exit projects at least one update")
      (is (notany #'nle::exec-update-final updates)))
    (is (null registry.processes))))

(deftest exec-snapshot-discloses-removed-controls (with-exec-registry (registry))
  ;; The seam the model reads through: a form that printed a styled string
  ;; answers the plain text plus the one line saying what was taken.
  (let ((id (spawn-test-eval
             registry
             :prints (format nil "~c[38;2;231;229;228mReady~c[0m"
                             #\Escape #\Escape))))
    (nle::exec-park-until registry id 5000)
    (is (equal (format nil "Ready~%[2 terminal control sequences removed by the output sanitizer]")
               (getf (nle::exec-snapshot-since registry id 0) :output)))))

(deftest exec-destroy-all-drains-the-image-lifetime-registry (with-exec-registry (registry))
  (spawn-test-eval registry :prints "alive" :linger 30)
  (is (= 1 (length registry.processes)))
  (is-shape registry (nle::destroy-all-execs is "destroy-all returns") (.processes null)))

(deftest a-running-sh-snippet-previews-what-its-child-writes (with-exec-registry (registry))
  ;; SH answers its capture only when the command ends, so a band saw nothing
  ;; of a long command. The preview carries what the child wrote as it came;
  ;; the model's output stays what the form printed -- nothing.
  (let* ((nle::*exec-preview-interval* 0)
         (id (spawn-test-eval registry :form "(sh \"echo one; sleep 1; echo two\")"
                                       :value (lambda () (nle::sh "echo one; sleep 1; echo two")))))
    ;; the updates for ID, until one carries a preview naming "one"
    (is-eventually (nlk:when-let (update (find id (nle::exec-collect-updates registry)
                                               :key #'nle::exec-update-shell-session-id))
                     (search "one" (nle::octets-to-text (nle::exec-update-preview update))))
                   "the first line shows while the command runs")
    (is (equal "" (getf (nle::exec-snapshot-since registry id 0) :output)) "and never reaches the output")
    (nle::exec-park-until registry id 5000)
    (let ((process (find id (nle::exec-registry-processes registry) :key #'nle::exec-process-id)))
      (is (equal (format nil "one~%two~%") (nle::exec-process-eval-value process)) "the value is the whole capture"))))

(deftest a-silent-snippet-announces-its-handoff (with-exec-registry (registry))
  ;; A backgrounded snippet that writes nothing still runs in a band.
  (let ((nle::*exec-preview-interval* 0)
        (id (spawn-test-eval registry :linger 2 :session-id "handoff")))
    (nle::exec-collect-updates registry)
    (nle::mark-exec-backgrounded registry id)
    (let ((update (find id (nle::exec-collect-updates registry) :key #'nle::exec-update-shell-session-id)))
      (is-present (update update) "the handoff sends a running update"
          (is (eq :running (nle::exec-update-status update)))
          (is (equalp #() (nle::exec-update-preview update)))))
    (let ((again (find id (nle::exec-collect-updates registry) :key #'nle::exec-update-shell-session-id)))
      (is (null again) "and only once"))))

(deftest exec-collect-updates-marks-final-for-backgrounded-exits (with-exec-registry (registry))
  (let ((id (spawn-test-eval registry :prints "x" :linger 0.2
                                      :session-id "final-session"
                                      :tool-call-id "call-final")))
    (nle::mark-exec-backgrounded registry id)
    (nle::exec-park-until registry id 5000)
    (let ((updates (nle::exec-collect-updates registry)))
      (is (plusp (length updates)) "updates were collected")
      (is-present (final (find-if #'nle::exec-update-final updates))
          "the closing update of a backgrounded exit is FINAL"
          (is-shape final (.status eq :exited "it is exited") (.tool-call-id "call-final"))))
    ;; FINAL may only ever fire once.
    (is (notany #'nle::exec-update-final (nle::exec-collect-updates registry)))))

(deftest a-failed-backgrounded-exit-carries-its-error-report (with-exec-registry (registry))
  ;; Without this, the report stays only in the eval value; a card says
  ;; "failed" and no operator surface says why.
  (let ((id (nle::spawn-background-eval
             registry "(error \"boom\")"
             (nlk:lambda-ignoring (stream)
               (sleep 0.2)
               (values (format nil "[closed 1 paren]~%ERROR: SIMPLE-ERROR: boom") t nil))
             :session-id "failed-session" :tool-call-id "call-failed")))
    (nle::mark-exec-backgrounded registry id)
    (nle::exec-park-until registry id 5000)
    (let* ((final (find-if #'nle::exec-update-final (nle::exec-collect-updates registry)))
           (report (and final (nle::exec-update-error final))))
      (is (equal "SIMPLE-ERROR: boom" report) "the closing update carries the report, what led it left off")
      (is-carrying (text (nlk:encode-json-object
                          (nlk:exited-shell-document id "(error \"boom\")" 1 :error report)))
        ("\"error\":\"SIMPLE-ERROR: boom\"" "and so does the exit's document"))))
  (let ((report (nle::exec-error-report "ERROR in form 2 of 2 (form 1 evaluated and stands: 1): SIMPLE-ERROR: x")))
    (is (equal "in form 2 of 2 (form 1 evaluated and stands: 1): SIMPLE-ERROR: x" report) "a snippet failing at a later form keeps where it stopped"))
  (is (null (nle::exec-error-report "the value")) "a value with no error line has no report"))

;;; --- durable exit fact ------------------------------------------------------

(deftest bg-exit-fact-closes-the-detached-workspace-window (with-temp-store ())
  ;; A call that detaches past its yield window keeps writing after the
  ;; tool_result bracket closed. The exit fact carries the CUMULATIVE
  ;; workspace_diff (original baseline -> completion), superseding the
  ;; yield-window document at fold time, and the completion tree stamps
  ;; its own capture (pre = the yield tree). A completion with no late
  ;; writes ships no diff and stamps nothing.
  (with-temp-directory (workspace "bgdiff")
    (with-workspace-captures (t)
      (flet ((write-notes (content)
               (write-temp-file (merge-pathnames "notes.txt" workspace) content))
             (document (fact &aux (metadata (and fact (nlk::tool-result-event-metadata fact))))
               (and (hash-table-p metadata) (gethash "workspace_diff" metadata)))
             (captures (call-id)
               (remove-if-not (lambda (entry) (equal call-id (getf entry :tool-call-id)))
                              (nlk:session-workspace-captures "bgdiff"))))
        (nlk:create-session :id "bgdiff" :cwd (namestring workspace))
        (write-notes (format nil "one~%"))
        (let ((turn (nlk:admit-turn "bgdiff" "bgdiff-cmd" "go")))
          (flet ((detach (call-id &optional yield-notes tail-notes)
                   ;; CALL-ID's window: the eval's pre-yield write, the yield
                   ;; bracket, the detached tail's write after the bracket
                   ;; closed, then the exit. Answers CALL-ID's exit fact.
                   (let ((pre (nth-value 1 (nlk:open-workspace-bracket turn :label call-id))))
                     (when yield-notes (write-notes yield-notes))
                     (nlk:checkpoint-capture-boundary
                      turn :boundary-kind "tool_result"
                      :tool-call-id call-id :pre-tree-sha pre)
                     (when tail-notes (write-notes tail-notes))
                     (nle::record-background-shell-exit-from-update
                      (nle::make-exec-update
                       :shell-session-id 7
                       :session-id "bgdiff"
                       :turn-id turn.turn-id
                       :tool-call-id call-id
                       :command "(loop)"
                       :status :exited :exit-code 0
                       :backgrounded t :final t))
                     (snippet-exit-fact "bgdiff" call-id))))
            (let ((document (document (detach "call-bg" (format nil "one~%two~%")
                                              (format nil "one~%two~%three~%"))))
                  (captures (captures "call-bg")))
              (cond ((nlk:git-available-p)
                     (is (hash-table-p document))
                     (let ((patch (and document
                                       (gethash "patch" (aref (gethash "files" document) 0)))))
                       (is (search "+two" (or patch "")))
                       (is (search "+three" (or patch ""))))
                     (is (= 2 (length captures)))
                     (let ((yield (first captures))
                           (completion (second captures)))
                       (is (equal (getf completion :pre-tree-sha)
                                  (getf yield :tree-sha)))
                       (is (not (equal (getf completion :tree-sha)
                                       (getf yield :tree-sha))))))
                    (t
                     (is (null document))
                     ;; The yield boundary still records its
                     ;; unavailable fact; only the completion stamp
                     ;; must stay absent.
                     (is (<= (length captures) 1)))))
            ;; The idle twin: no writes inside its window at all.
            (let ((fact (detach "call-idle")))
              (is fact "the idle exit fact still records")
              (is (null (document fact)))
              (is (<= (length (captures "call-idle")) 1)))))))))

(deftest record-background-shell-exit-appends-after-terminal-turn (with-temp-store ())
  (let* ((sid "bg-exit-fact")
         (turn-id (nlk:durable-turn-turn-id (seed-completed-turn sid "start it" "started"))))
    ;; The owning turn is terminal — RECORD-TOOL-RESULT would refuse.
    (signals-error nlk:turn-already-terminal
      (nlk:record-tool-result (detached-turn sid turn-id) "x" "snippet" ""))
    (nlk:record-background-shell-exit sid turn-id "call-9"
                                      "sleep 100" 1042 0)
    (is-present (fact (snippet-exit-fact sid)) "the exit fact landed despite the terminal turn"
        (is-shape fact (nlk:tool-result-event-call-id "call-9") (nlk:tool-result-event-result "")
          (nlk:event-turn-id turn-id))
        (is-shape (nlk:json-value (nlk::tool-result-event-metadata fact) :object "background_shell")
          ("status" "exited") ("shell_session_id" eql 1042) ("exit_code" eql 0)
          ("command" "sleep 100")))))

(deftest bg-exit-fact-inside-a-later-tool-round-keeps-call-ids-unique (with-temp-store ())
  ;; The operator's room, 2026-10-22: a steered turn's background eval exited
  ;; between the next turn's two parallel calls, and every later round of the
  ;; session answered 400 "Duplicate 'call_id'" from deepseek.
  (let* ((sid (ensure-durable-session "bg-exit-mid-round"))
         (earlier (nlk:admit-turn sid "bg-mid-1" "start it")))
    (record-tool-use earlier "call-bg" "eval" "{\"form\":\"(sleep 99)\"}"
                     "{\"status\":\"running\",\"eval_id\":1042}"
                     :metadata (nle::running-shell-metadata 1042 "(sleep 99)"))
    (nlk:complete-turn earlier nil)
    (let ((turn (nlk:admit-turn sid "bg-mid-2" "go on")))
      (nlk:record-assistant-message
       turn (nle::message "assistant" nil
                          :tool-calls (vector (nle::chat-tool-call-object "call-1" "eval" "{}")
                                              (nle::chat-tool-call-object "call-2" "eval" "{}"))))
      (nlk:record-tool-call turn "call-1" "eval" "{}")
      ;; the earlier turn's shell ends inside this round
      (nlk:record-background-shell-exit sid (nlk:durable-turn-turn-id earlier)
                                        "call-bg" "(sleep 99)" 1042 0)
      (nlk:record-tool-result turn "call-1" "eval" "one")
      (record-tool-use turn "call-2" "eval" "{}" "two")
      (let* ((units (nle::history-turns-from-events (nle::collect-visible-session-events sid)))
             (messages (loop for unit in units append (nle::history-turn-messages unit))))
        ;; the exit fact cuts no unit: one per turn
        (is (equal (mapcar #'nlk:durable-turn-turn-id (list earlier turn))
                   (mapcar #'nle::history-turn-turn-id units)))
        ;; each call is issued once and answered once
        (dolist (call '("call-1" "call-2"))
          (is (= 1 (count call (loop for msg in messages
                                     append (map 'list (lambda (c) (gethash "id" c))
                                                 (or (gethash "tool_calls" msg) #())))
                          :test #'equal)))
          (is (= 1 (count call messages :key (lambda (msg) (gethash "tool_call_id" msg))
                                        :test #'equal))))
        ;; call-1 carries its recorded result, not the orphan stub
        (is (search "one" (gethash "content" (find "call-1" messages
                                                  :key (lambda (msg) (gethash "tool_call_id" msg))
                                                  :test #'equal))))))))

(deftest bg-exit-fact-never-reaches-provider-context (with-temp-store ())
  (let* ((sid (ensure-durable-session "bg-exit-fold"))
         (turn (nlk:admit-turn sid "bg-fold-cmd" "run it")))
    (record-tool-use turn "call-bg" "eval" "{\"form\":\"(sleep 99)\"}"
                     "{\"status\":\"running\",\"eval_id\":1042}"
                     :metadata (nle::running-shell-metadata 1042 "(sleep 99)"))
    (nlk:complete-turn turn nil)
    ;; A later completed turn puts the exit fact in its own late turn-id
    ;; group — the fold topology a per-group dedupe would miss.
    (seed-completed-turn sid "next" "ok")
    (nlk:record-background-shell-exit sid turn.turn-id "call-bg" "(sleep 99)" 1042 0)
    (let* ((messages (wire-fold sid))
           (tool-messages (remove-if-not
                           (lambda (msg)
                             (equal "tool" (gethash "role" msg)))
                           messages)))
      (is (= 1 (count "call-bg" tool-messages
                      :key (lambda (msg) (gethash "tool_call_id" msg))
                      :test #'equal)))
      (is (notany (lambda (msg)
                    (zerop (length (gethash "content" msg))))
                  tool-messages))
      ;; The anthropic lane merges consecutive tool results into one user
      ;; message; a surviving duplicate tool_use_id is a hard 400 on every
      ;; later round of the session — the fold guard is what prevents it.
      (let ((ids (loop for msg across (nle::anthropic-messages (coerce messages 'vector))
                       nconc (loop for block across (gethash "content" msg)
                                   when (equal "tool_result" (nlk:json-value block :string "type"))
                                     collect (gethash "tool_use_id" block)))))
        (is (= (length ids)
               (length (remove-duplicates ids :test #'equal))))))))

;;; --- golden wire fixtures ---------------------------------------------------

(defparameter +bg-fixture-running+
  "{\"sequence\":0,\"session_id\":\"session-a\",\"payload\":{\"type\":\"session_background_process_updated\",\"session_id\":\"session-a\",\"process\":{\"shell_session_id\":1042,\"tool_call_id\":\"call-9\",\"command\":\"sleep 100\",\"status\":\"running\",\"output_b64\":\"dGljawo=\",\"offset\":128,\"rows\":40,\"cols\":132}}}")

(defparameter +bg-fixture-exited+
  "{\"sequence\":0,\"session_id\":\"session-a\",\"payload\":{\"type\":\"session_background_process_updated\",\"session_id\":\"session-a\",\"process\":{\"shell_session_id\":1042,\"tool_call_id\":\"call-9\",\"command\":\"sleep 1\",\"status\":\"exited\",\"exit_code\":0,\"output_b64\":\"\",\"offset\":0}}}")

(deftest bg-golden-fixtures-decode-and-fold ()
  (let* ((frame (nlk:decode-sync-message +bg-fixture-running+))
         (payload frame.payload)
         (state (bg-state payload))
         (rows (nle::state-background-rows state)))
    (is (equal "session-a" frame.session-id))
    (is (= 1 (length rows)) "the running chunk seeds one band row")
    (nlk:when-let (band (first rows))
      (is-shape band (.shell-session-id eql 1042) (.tool-call-id "call-9") (.command "sleep 100")
        (.status eq :running "status is running") (.activity-events = 1))
      (is (equal "tick" (string-trim '(#\Newline) (nle::octets-to-text band.output-tail)))))
    ;; The exited fixture retires the same row by shell id.
    (feed-events state (nlk:sync-message-payload
                        (nlk:decode-sync-message +bg-fixture-exited+)))
    (is (null (nle::state-background-rows state)))))

(deftest bg-encoder-produces-the-wire-shape ()
  (let* ((update (nle::make-exec-update
                  :shell-session-id 1042
                  :session-id "session-a"
                  :tool-call-id "call-9"
                  :command "sleep 100"
                  :status :running
                  :chunk (string-octets (format nil "tick~c" #\Newline))
                  :chunk-offset 128))
         (payload (nle::background-process-update-payload update))
         (wire (nlk:encode-projection-event
                "session-a" 0 "session_background_process_updated" payload))
         (decoded (nlk:sync-message-payload (nlk:decode-sync-message wire)))
         (process (nlk:json-value decoded :object "process")))
    (is-present process "the frame round-trips through the codec"
      (is-shape process ("shell_session_id" eql 1042 "shell id rides")
        ("status" "running" "status spelling") ("output_b64" "dGljawo=")
        ("offset" eql 128 "the absolute offset rides"))
      (is (not (nth-value 1 (gethash "exit_code" process))))
      (is (not (nth-value 1 (gethash "backgrounded" process)))))))

;;; --- band reducer -----------------------------------------------------------

(defun bg-item-delta (index name arguments)
  (nlk::make-json-object
   "type" "item_delta" "turn_id" "turn-1"
   "delta" (nlk::make-json-object "type" "tool_call" "index" index
                                  "name" name "arguments" arguments)))

(defun tool-call-row (state call-id)
  "STATE's transcript row for the tool call CALL-ID, or NIL."
  (find-if (lambda (row)
             (and (eq :tool-call row.kind) (equal call-id (nle::transcript-row-tool-call-id row))))
           (nle::transcript-rows state)))

(defun replay-state (session-id &aux (state (make-hash-table :test #'equal))
                                     (nle::*folding-replay-p* t))
  "SESSION-ID's visible durable log folded exactly as the client replay folds it."
  (dolist (event (nle::collect-visible-session-events session-id) state)
    (unless (nlk:event-kind-replay-suppressed-p event.kind)
      (feed-events state (gethash "event" (nle::accumulated-event-frame session-id event))))))

(deftest bg-preparing-rows-seed-from-stream-and-promote-in-place ()
  ;; Streamed fragments open the preparing row keyed by provider index.
  (let ((state (bg-state (bg-item-delta 0 "run" "{\"command\":\"sle"))))
    (is-present (band (first (nle::state-background-rows state)))
        "the first fragment seeds a preparing row"
        (is-shape band (.status eq :preparing "UI-only phase") (.command "sle")))
    (feed-events state (bg-item-delta 0 "run" "{\"command\":\"sleep 100\"}"))
    (is (= 1 (length (nle::state-background-rows state))))
    (nlk:when-let (row (first (nle::state-background-rows state)))
      (is-shape row (nle::bg-band-row-command "sleep 100") (nle::bg-band-row-activity-events = 2)))
    ;; The durable fact adopts the row by call id.
    (feed-events state (bg-call-fact "call-9" "{\"command\":\"sleep 100\"}"))
    (is (= 1 (length (nle::state-background-rows state))))
    (is (equal "call-9" (nle::bg-band-row-tool-call-id
                         (first (nle::state-background-rows state)))))
    ;; Spawn handoff: the running frame promotes the row in place.
    (feed-events state (nlk:sync-message-payload
                        (nlk:decode-sync-message +bg-fixture-running+)))
    (let ((rows (nle::state-background-rows state)))
      (is (= 1 (length rows)) "one call never owns two rows")
      (nlk:when-let (band (first rows))
        (is-shape band (.status eq :running) (.shell-session-id eql 1042))))
    ;; The exited frame (backgrounded) retires the row and stamps the
    ;; transcript rows the tool_call fact opened.
    (feed-events state
      (let* ((frame (nlk:sync-message-payload
                     (nlk:decode-sync-message +bg-fixture-exited+)))
             (process (gethash "process" frame)))
        (setf (gethash "backgrounded" process) t
              (gethash "exit_code" process) 7)
        frame))
    (is (null (nle::state-background-rows state)) "the band row retired")
    (is-present (row (tool-call-row state "call-9")) "the call's transcript row exists"
        (is-shape row (.tool-status "exited") (.tool-exit-code eql 7)))))

(deftest bg-preparing-rows-drop-on-turn-terminal-and-never-seed-on-replay ()
  (let ((state (bg-state (bg-item-delta 0 "run" "{\"command\":\"sleep 9\"}"))))
    (is (= 1 (length (nle::state-background-rows state))) "seeded live")
    (feed-events state (nlk::make-json-object "type" "turn.completed"
                                              "turn_id" "turn-1"))
    (is (null (nle::state-background-rows state))))
  (let ((nle::*folding-replay-p* t))
    (is (null (nle::state-background-rows
               (bg-state (bg-item-delta 0 "run" "{\"command\":\"sleep 9\"}")
                         (bg-call-fact "call-9" "{\"command\":\"sleep 9\"}")))))))

(deftest bg-non-exec-tools-never-seed-the-band ()
  ;; eval is on the seeding side: it is the organism's own exec
  ;; (SEEDS-BACKGROUND-BAND-P), gated in band-test's S7 cluster.
  (is (null (nle::state-background-rows
             (bg-state (bg-item-delta 0 "read" "{\"path\":\"a.txt\"}")
                       (bg-item-delta 1 "edit" "{\"path\":\"a.txt\"}"))))))

;;; --- E1: spawn -> exit -> durable fact -> replay convergence ----------------

(defmacro with-backgrounded-eval ((sid session-id command-id &rest calls) &body body)
  "A gateway, a drained registry, and SID's one completed turn whose CALLS, each
(CALL-ID FORM), backgrounded their FORMs in order through the exact
execute-tool-call seam: each call journaled, the tool run with the
executing-call binding, its result and metadata journaled; then the turn
completed. BODY runs with SID bound."
  `(with-temp-gateway (port)
     (drain-exec-registry)
     (let* ((,sid (ensure-durable-session ,session-id))
            (turn (nlk:admit-turn ,sid ,command-id "run it")))
       (loop for (call-id form) in ',calls
             do (let ((nle::*current-durable-turn* turn)
                      (nle::*executing-tool-call-id* call-id)
                      (nle::*tool-result-metadata* nil))
                  (nlk:record-tool-call turn call-id "eval" (eval-args form))
                  (let ((result (nle::background-capable-eval form 50)))
                    (is (search "\"running\"" result) "the call backgrounded")
                    (nlk:record-tool-result turn call-id "eval" result
                                            :metadata nle::*tool-result-metadata*))))
       (nlk:complete-turn turn nil)
       ,@body)))

(deftest bg-watcher-appends-durable-exit-fact-and-replay-converges ()
  (with-backgrounded-eval (sid "bg-e2e" "bg-e2e-command"
                           ("call-bg" "(progn (princ 1) (sleep 0.4))"))
    ;; The evaluation exits ~0.4s in; the 200ms watcher then projects the
    ;; exit and appends the durable fact — after the turn is terminal.
    (is-present (fact (await () (snippet-exit-fact sid))) "the watcher appended the exit fact"
      (is-shape fact (nlk:tool-result-event-call-id "call-bg") (nlk:tool-result-event-result "")))
    ;; Replay convergence: fold the visible durable log exactly as the
    ;; client replay does — zero band rows, and the call's row restamped.
    (let ((state (replay-state sid)))
      (is (null (nle::state-background-rows state)))
      (is-present (row (tool-call-row state "call-bg"))
          "the call's row folds from the durable pair"
          (is-shape row (.tool-status "exited") (.tool-exit-code eql 0))
          (is (search "\"running\"" row.text))))))

(deftest bg-shutdown-appends-exit-facts-before-watcher-dies ()
  (with-backgrounded-eval (sid "bg-shutdown" "bg-shutdown-cmd"
                           ("call-shutdown" "(loop (sleep 0.05))"))
    ;; The evaluation is still running when the gateway stops. The
    ;; shutdown ladder aborts it, waits for the exit, lands the durable
    ;; exit fact via one final collection, and drains the registry — the
    ;; pre-fix order stopped the watcher first, the fact was lost, and
    ;; replay reported the work running forever.
    (nle:stop-gateway)
    (is-present (fact (snippet-exit-fact sid))
        "shutdown landed the durable exit fact for the aborted eval"
        (is (equal "call-shutdown" (nlk:tool-result-event-call-id fact))))
    (is (null (nle::exec-registry-processes (nle::exec-registry-instance))))))

(deftest inline-eval-await-honors-the-tool-yield-window (with-exec-registry (registry))
  ;; The tool window bounds a deliberate park as its default. Without it,
  ;; (eval-await ID :until-exit t) would park to the ten-minute poll default.
  ;; Earlier evolve tests unintern evolved-owned names
  ;; (CLEAR-EVOLVED-PACKAGE); a live image always resolves the operator,
  ;; so restore the original symbol before the form string is read.
  (shadowing-import 'nle::eval-await '#:nodecode.evolved)
  (let ((id (spawn-test-eval registry :linger 30)))
    (flet ((await (form tool-window-ms)
             (let* ((start (get-internal-real-time))
                    (result (nle::background-capable-eval (format nil form id) tool-window-ms))
                    (elapsed-ms (round (* 1000 (- (get-internal-real-time) start))
                                       internal-time-units-per-second)))
               (is (< elapsed-ms 10000))
               (is (search ":RUNNING" (string-upcase result))))))
      (await "(eval-await ~d :until-exit t)" 200)
      ;; An explicit :yield-ms in the form still outranks the tool window.
      (await "(eval-await ~d :yield-ms 100 :until-exit t)" 600000))))

;; With no yield_time_ms the call's window is the schema's 10000, and a bare
;; (eval-await N) inside it parks that long at most, never the ten-minute
;; operator default.
(deftest inline-eval-await-parks-the-default-window-when-the-call-names-none (with-exec-registry (registry))
  (shadowing-import 'nle::eval-await '#:nodecode.evolved)
  (let* ((id (spawn-test-eval registry :linger 30))
         (start (get-internal-real-time))
         (result (nle::background-capable-eval (format nil "(eval-await ~d :until-exit t)" id) nil))
         (elapsed-ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))
    (is (< 8000 elapsed-ms 15000) elapsed-ms)
    (is (search ":RUNNING" (string-upcase result)))))

;; The look from the turn that backgrounded an evaluation says to end the
;; turn; a later turn's look — a wake's opener — is no poll.
(deftest a-poll-from-the-backgrounding-turn-is-told-to-end-the-turn (with-exec-registry (registry))
  (let ((id (spawn-test-eval registry :linger 30 :turn-id "t-poll")))
    (flet ((note (turn-id &aux (nle::*current-durable-turn* (detached-turn "local" turn-id)))
             (getf (nle::eval-snapshot-plist registry id) :note)))
      (is (search "end your turn" (or (note "t-poll") "")))
      (is (null (note "t-later")) "a later turn's look carries no note"))))

;;; --- subscribe snapshots and WS control ops ---------------------------------

(deftest bg-subscribe-snapshot-reseeds-still-running-work (with-temp-gateway (port))
  (drain-exec-registry)
  (let ((sid (ensure-durable-session "bg-snap")))
    (let ((id (spawn-test-eval (nle::exec-registry-instance)
                               :prints "seed" :linger 30
                               :session-id sid
                               :tool-call-id "call-snap")))
      (nle::mark-exec-backgrounded (nle::exec-registry-instance) id)
      ;; Let the output land (and the watcher advance its cursor past it)
      ;; BEFORE subscribing: the snapshot must still carry the full
      ;; retained buffer from offset 0.
      (sleep 0.6)
      (let* ((frames (subscribed-frames port sid))
             (snapshot (find-frame frames "session_background_process_updated")))
        (is (find-frame frames "session_stream_resumed"))
        (is-present snapshot "a registry snapshot frame arrived on subscribe"
            (let ((process (gethash "process"
                                    (gethash "payload" snapshot))))
              (is-shape process ("shell_session_id" eql id) ("offset" eql 0))
              (is (search "seed"
                          (nle::octets-to-text
                           (cl-base64:base64-string-to-usb8-array
                            (gethash "output_b64" process)))))))))))

;;; --- sh: the one shell verb --------------------------------------------

(deftest sh-rejects-unified-diff-payloads-before-launch (multiple-value-bind (output status))
  (nle:sh "*** Begin Patch\n*** Update File: x")
(is (= 2 status) "a diff sent to sh is rejected as a command")
(is (search "(edit path old new)" output)))

;; gpt-6-astra passed whole commands as the name, 30 times, and the error
;; echoed each back as the key to send.
(deftest a-strings-read-given-its-text-says-to-send-it-as-an-entry (with-exec-registry (registry))
  (let ((strings (nlk::make-json-object "script" "echo hi")))
    (dolist (result (list (nle::background-capable-eval "(sh ($ \"echo hi && ls -la\"))" 5000 strings)
                          (nle::background-capable-eval "(sh ($ \"echo hi && ls -la\"))" 5000)))
      (is-carrying result "takes an entry's name, not its text" "($ \"script\")"))
    ;; a short name still lists what the call carries
    (is (search "no strings entry named \"absent\"" (nle::background-capable-eval "($ \"absent\")" 5000 strings)))))

;; A ($ "K") example shown without its strings entry reads as the text
;; itself: a model wraps its edit's old text in $ and the first edit fails.
(deftest eval-description-shows-each-strings-read-beside-its-entry ()
  (let* ((description (getf (second (nle::find-tool "eval")) :description))
         (marker "($ \"")
         (keys (loop for start = (search marker description)
                       then (search marker description :start2 end)
                     for end = (and start (position #\" description :start (+ start (length marker))))
                     while end
                     collect (subseq description (+ start (length marker)) end))))
    (is (member "old" keys :test #'string=) "the description shows edit's strings read")
    (dolist (key keys)
      (is (search (format nil "\"~a\": \"" key) description) (format nil "($ ~s) is shown beside its strings entry" key)))
    (is (search "never the text" description) "and says $ takes an entry's name, never the text")))

(deftests sh-answers (command expected expected-status status-why)
  (multiple-value-bind (output status) (nle:sh command)
    (is (equal expected output) output)
    (is (eql expected-status status) status-why))
  (merged-output-and-marks-a-failing-status "echo out; echo err 1>&2; exit 3"
   (format nil "out~%err~%[exit 3]") 3 "the status is the second value")
  (a-clean-run-verbatim "printf hi" "hi" 0 "zero status"))

(deftest sh-runs-bash-where-the-machine-has-it ()
  ;; Ubuntu's /bin/sh is dash, which has no `time' keyword and no [[ ]]:
  ;; three of four benchmark runs lost a call to `time: not found'.
  #-win32
  (when (probe-file "/bin/bash")
    (is (search "bash" (uiop:native-namestring nlk:*shell*)))
    (is (equal (format nil "yes~%") (nle:sh "[[ a == a ]] && echo yes")))
    (is (eql 0 (nth-value 1 (nle:sh "time true"))) "time is the shell's own")))

(deftest sh-runs-argv-without-a-shell ()
  (is (equal "a b" (nle:sh (list "printf" "%s" "a b"))) "argv is not re-split"))

(deftest sh-feeds-end-of-file-on-stdin ()
  ;; A program reading stdin finds it empty rather than a terminal nobody
  ;; holds; a heredoc in the command is the way to feed it.
  (is (equal "" (nle:sh "cat")) "cat sees EOF")
  (is (equal (format nil "fed~%") (nle:sh (format nil "cat <<'EOF'~%fed~%EOF")))))

(deftest sh-runs-in-the-named-directory ()
  (is (equal (format nil "/tmp~%") (nle:sh "pwd" :directory "/tmp"))))

(deftest sh-rides-the-eval-snippet ()
  (is (equal "\"hi\"" (nle:eval-string "(sh \"printf hi\")")))
  (is (search "[exit 1]" (nle:eval-string "(sh \"exit 1\")"))))

(deftest sh-kills-its-child-when-the-evaluation-unwinds ()
  ;; eval-interrupt on a stuck (sh ...) must not leave the child running.
  (let* ((marker "31.7")
         (thread (bt2:make-thread
                  (lambda ()
                    (handler-case (nle:sh (list "sleep" marker)) (error () :unwound))))))
    (sleep 0.3)
    (bt2:interrupt-thread thread (lambda () (error "eval-interrupt")))
    (is (eq :unwound (bt2:join-thread thread)) "the evaluation unwound")
    (is (equal "" (nle:sh (format nil "pgrep -x -f 'sleep ~a' || true" marker))))))

(deftest cancel-of-a-parked-call-stops-its-evaluation-and-its-child (with-exec-registry (registry))
  ;; Cancelling a turn parked in eval takes the call's evaluation with it:
  ;; the soft abort, the child tree killed by SH's unwind, the exit named
  ;; as the user's interruption. Without this, a cancelled turn would leave
  ;; its child running with no reader and the row still `running`.
  (with-session-store ("cancel-parked")
    (let* ((marker "43.1")
           (turn (nlk:admit-turn "cancel-parked" "cp-cmd" "run it"))
           (turn-id turn.turn-id))
      (with-provider-stub (nil (tool-call-round "c1" "eval" "yield_time_ms" 60000
                                               "form" (format nil "(sh \"sleep ~a\")" marker)))
        (let ((worker (nlk:spawn "cancel-parked-worker"
                        (nlk:with-handlers ((nlk:turn-cancelled-condition () :cancelled))
                          (nle:run "run it"
                                   :session (nle:intern-session "cancel-parked")
                                   :command-id "cp-cmd" :admitted-turn turn)))))
          (flet ((spawned () (first registry.processes)))
            (await (:tries 250 :rest 0.02)
              (and (nlk::cancellation-signal-waiting-threads (nlk::turn-signal turn))
                   (spawned) (nle::exec-process-child-pid (spawned))))
            (is-present (process (spawned)) "the call parked, the child live"
              (nlk:request-cancel-turn "cancel-parked" turn-id :operator)
              (is (eq :cancelled (bt2:join-thread worker)))
              (await (:tries 300 :rest 0.01) (eq :exited (nle::exec-process-status process)))
              (is-shape process (.status eq :exited) (.exit-code eql 130)
                (.eval-value "interrupted by user"))
              (is (equal "" (nle:sh (format nil "pgrep -x -f 'sleep ~a' || true"
                                            marker))))
              (is (equal "turn.cancelled" (nlk:turn-terminal-kind turn-id)))
              (is (= 1 (%turn-kind-count turn-id nlk::+kind-tool-result+)))
              (let ((result (first (session-events "cancel-parked" :type 'nlk:tool-result-event))))
                (is (equal "interrupted by user" (nlk:tool-result-event-result result)) "the model's words")
                ;; The operator's Stop names the call theirs, not a failure.
                (is (not (nlk:tool-result-event-failed result)))
                (is-shape (nlk:tool-result-event-metadata result)
                  ((:string "stopped" "by") "operator" "stopped by the operator's own Stop")
                  ((:integer "stopped" "after_ms") is "after how long")
                  ((:string "stopped" "reason") null "the operator's stop needs no reason"))))))))))

(deftest an-operators-stop-keeps-what-the-call-wrote (with-exec-registry (registry))
  ;; The stop rides the result as its own end: who stopped it, after how
  ;; long, and what it had written; the model's text is left as it was.
  (with-session-store ("stop-x")
    (let ((turn (nlk:admit-turn "stop-x" "sx-cmd" "run it")))
      (with-stub-provider ((tool-call-round "c1" "eval" "yield_time_ms" 60000
                                           "form" "(sh \"for i in 1 2 3 4 5 6 7 8 9; do echo step $i; sleep 0.4; done\")")
                           (wire-message "assistant" :content "it stopped"))
        (let ((worker (nlk:spawn "stop-x-worker"
                        (nle:run "run it" :session (nle:intern-session "stop-x")
                                          :command-id "sx-cmd" :admitted-turn turn))))
          (flet ((spawned () (first registry.processes)))
            (is-eventually (and (spawned) (nle::exec-process-preview (spawned))
                                (search "step 2" (nle::octets-to-text (nle::exec-process-preview (spawned)))))
                           "the box shows step 2" :seconds 6)
            ;; what the band's x sends (interrupt_exec)
            (nle::exec-stop-process (spawned) :by :operator)
            (bt2:join-thread worker)
            (let ((result (first (session-events "stop-x" :type 'nlk:tool-result-event))))
              (is (not (nlk:tool-result-event-failed result)) "a stop is no failure")
              (is (search "(eval-interrupt)" (nlk:tool-result-event-result result)) "the model's text as it was")
              (let ((stop (nlk:json-value (nlk:tool-result-event-metadata result) :object "stopped"))
                    (tail (nlk:json-value (nlk:tool-result-event-metadata result) :string "stopped" "tail")))
                (is-shape stop ("by" "operator") ("after_ms" is "after how long"))
                (is (and tail (search "step 1" tail) (search "step 2" tail)) "what it had written")
                (is (not (search "compilation" (or tail ""))) "and nothing the abort printed")))))))))

(deftest a-read-of-a-background-job-is-carded-as-what-the-job-wrote (with-exec-registry (registry))
  ;; A snippet that is the read alone carries, for its card, how the job ended
  ;; and the lines it wrote -- or who stopped it.
  (flet ((read-card (id)
           (let ((nle::*executing-tool-call-id* "c-read")
                 (nle::*tool-result-metadata* nil)
                 (nle::*read-is-the-snippet* t))
             (nle::eval-status id)
             (nlk:json-value nle::*tool-result-metadata* :object "background_read"))))
    (let ((done (spawn-test-eval registry :value (format nil "[text: 2 lines, 13 characters]~%tick 1~%tick 2~%"))))
      (await (:tries 100 :rest 0.02) (eq :exited (nle::exec-process-status (nle::find-background-eval registry done))))
      (is-shape (read-card done) ("status" "exited") ("end" "done")
        ("text" (format nil "tick 1~%tick 2") "the lines, not the header and the plist")))
    (let* ((running (spawn-test-eval registry :prints (format nil "step 1~%") :linger 30))
           (process (nle::find-background-eval registry running)))
      (await (:tries 100 :rest 0.02) (plusp (fill-pointer (nle::exec-process-output process))))
      (nle::exec-stop-process process :by :operator)
      (await (:tries 150 :rest 0.02) (eq :exited (nle::exec-process-status process)))
      (let ((card (read-card running)))
        (is-shape card ("end" "stopped") ("text" "" "its lines are its box's"))
        (is (equal "operator" (nlk:json-value card :string "stopped" "by")) "who stopped it")))))

(deftest a-gateway-stop-names-itself-on-the-evaluations-it-cuts (with-exec-registry (registry))
  ;; A gateway stopping mid-call closed it `failed' with `; compilation unit
  ;; aborted ... ERROR: evaluation terminated'. The shutdown's kill says it
  ;; was the shutdown, and keeps what the form had printed before it.
  (let* ((id (spawn-test-eval registry :prints (format nil "half way~%") :linger 30))
         (process (nle::find-background-eval registry id)))
    (await (:tries 100 :rest 0.02) (plusp (fill-pointer (nle::exec-process-output process))))
    (nle::terminate-all-execs registry)
    (nle::exec-await-all-exits registry :timeout-seconds 3)
    (is-shape (nle::exec-stop-document process) ("by" "shutdown"))
    (is (search "half way" (gethash "tail" (nle::exec-stop-document process))) "what it had printed")
    (is (equal "stopped" (nle::exec-end (nle::exec-process-exit-code process) nil)) "a stop's end word")))

;;; --- the strings side-channel and the file verbs ------------------------

(deftest eval-runaway-recursion-is-a-tool-error-not-an-image-death (with-exec-registry (registry))
  ;; A runaway recursive evaluation that allocates an array on each call
  ;; would cross the guard page inside the allocator and kill the image.
  ;; The watcher's probe unwinds the evaluation at half its stack, long
  ;; before the page: the same form is one ERROR line naming the
  ;; recursion.
  (let* ((stop nil)
         (scanner (nlk:spawn "runaway-scanner"
                    (loop until stop
                          do (nle::scan-eval-runaways registry)
                             (sleep 0.02)))))
    (nlk:with-cleanup ((setf stop t) (bt2:join-thread scanner))
      (is-carrying (result (nle::background-capable-eval
                            "(labels ((deep (n) (let ((a (make-array 8192 :initial-element n))) (+ (aref a 0) (deep (1+ n)))))) (deep 0))"
                            60000))
        ("ERROR: EVAL-RUNAWAY: control stack runaway" result)
        ("DEEP" "the verdict names the recursion")
        ("unwound before the guard page" "and says why it was stopped")))))

(deftest control-stack-usage-grows-with-depth (labels ((deep (n)
                                                         (if (zerop n)
                                                             (list (nle::control-stack-usage))
                                                             (cons n (deep (1- n)))))))
  ;; Whichever way the control stack grows, usage must rise with depth,
  ;; measured in a fresh thread as an evaluation's is, and a shallow thread
  ;; takes no verdict.
  (destructuring-bind (top deep-used span verdict)
      (bt2:join-thread
       (nlk:spawn "stack-usage"
         (multiple-value-bind (used span) (nle::control-stack-usage)
           (list used (car (last (deep 2000))) span
                 (handler-case (progn (nle::eval-runaway-thread-probe) :quiet)
                   (nle::eval-runaway (e) (princ-to-string e)))))))
    (is (< 0 top (* 1024 1024)) "a fresh thread has used under a megabyte")
    (is (< top deep-used (floor span 2)) "two thousand frames deeper reads as more")
    (is (eq :quiet verdict) "and a shallow thread takes no verdict")))

(deftest eval-strings-ride-to-the-evaluating-thread (with-exec-registry (registry))
  ;; Source text tunnels JSON -> Lisp string -> shell -> python heredoc.
  ;; The side-channel is the one-escaping lane, and it must hold on the
  ;; background snippet's own thread — where every non-inline form runs —
  ;; not just the caller's.
  (let* ((gnarly (format nil "a \"quoted\" \\slash~c~%last" #\Tab))
         (strings (nlk::make-json-object "text" gnarly)))
    (is (equal (format nil "~s" (sxhash gnarly))
               (nle::background-capable-eval
                "(sxhash ($ \"text\"))" 5000 strings)))
    (is-carrying (result (nle::background-capable-eval "($ \"absent\")" 5000 strings))
      ("no strings entry named \"absent\"" result)
      ("\"text\"" "the error lists what the call carries"))
    (is (search "no strings object"
                (nle::background-capable-eval "($ \"any\")" 5000)))))

(deftest eval-tool-decodes-the-strings-object (with-exec-registry (registry))
  ;; The dispatch seam: shasht decodes the nested object into a hash table
  ;; and the tool lambda must hand exactly that through, and advertise the
  ;; property so a provider can send it at all.
  (let ((entry (nle::find-tool "eval")))
    (is (hash-table-p (gethash "strings"
                               (gethash "properties"
                                        (getf (second entry) :parameters)))))
    (is (equal "3" (funcall (third entry)
                            (nlk::make-json-object
                             "form" "(length ($ \"text\"))"
                             "strings" (nlk::make-json-object "text" "abc")))))))

(deftest edit-replaces-the-one-occurrence-and-names-the-miss ()
  (with-temp-file (path :type "txt"
                        :contents (format nil "alpha~%beta gamma~%delta~%"))
    (is-carrying (receipt (nle:edit path "beta" "BETA")) "line 2")
    (is (equal (format nil "alpha~%BETA gamma~%delta~%")
               (uiop:read-file-string path)))
    ;; A miss answers the divergence — where the file stops matching OLD
    ;; and how each side continues — never a bare not-found.
    (is-carrying (report (refusal-text error (nle:edit path "BETA gamme" "x")))
      "diverges" "OLD continues")
    ;; A crowd answers the count: widen OLD until it names one place.
    (is-carrying (report (refusal-text error (nle:edit path "a" "x"))) "occurs 5 times")))

(deftest write-file-writes-whole-files-with-parents (with-temp-trees ((root "write-file")))
  (let* ((path (namestring (merge-pathnames "deep/file.txt" root)))
         (receipt (nle:write-file path (format nil "one~%two~%"))))
    (is (search "2 lines" receipt) receipt)
    (is (equal (format nil "one~%two~%") (uiop:read-file-string path)))))

;;; --- look: an image file, shown at the next round ------------------------
;;; The verb stages the file on the live turn under its own call id; the
;;; engine's placement is context-test's; here: the receipt, the staging,
;;; the refusals, and the whole loop — the image reaching the provider
;;; behind the round that looked and leaving with the turn.

(defparameter *one-pixel-png*
  (octets 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 1 0 0 0 1 8 6
          0 0 0 31 21 196 137 0 0 0 13 73 68 65 84 120 156 99 248 207 192 240
          31 0 5 0 1 255 137 153 61 29 0 0 0 0 73 69 78 68 174 66 96 130)
  "A 1x1 red PNG, 70 bytes.")

(defmacro with-look-image ((path tag &key turn-id call) &body body)
  "Run BODY with PATH a one-pixel PNG file named after TAG, deleted on unwind
with the images staged under TURN-ID, when given; CALL, when given, is the
executing tool call of a current durable turn carrying that id."
  `(let ((,path (namestring (temp-path ,tag "png")))
         ,@(when call `((nle::*executing-tool-call-id* ,call)
                        (nle::*current-durable-turn* (detached-turn "look-session" ,turn-id)))))
     (nlk:with-cleanup (,@(when turn-id `((nle::forget-turn-images ,turn-id)))
                        (ignore-errors (delete-file ,path)))
       (alexandria:write-byte-vector-into-file *one-pixel-png* ,path)
       ,@body)))

(defun image-message-p (message)
  "Whether MESSAGE carries an image part."
  (some (lambda (part) (equal "image_url" (nle::content-part-type part)))
        (nle::message-content-parts (gethash "content" message))))

(deftest look-attaches-the-image-to-the-turn-under-its-call ()
  (with-look-image (path "look" :turn-id "look-turn" :call "call-look")
    (nle::forget-turn-images "look-turn")
    (let* ((nle::*tool-result-metadata* nil)
           (receipt (nle:look path))
           (groups (nle::turn-images "look-turn"))
           (fact (nlk:json-value nle::*tool-result-metadata* :object "image")))
      ;; The transcript's half of the verb: a durable fact naming the file,
      ;; never its bytes, and the PNG a terminal can be shown (tui/ui.lisp).
      (is-each ((lambda (key) (nlk:json-value fact :text key)))
        ("payload_kind" "file" "the fact names a file, not a payload")
        ("source" "look" "and the verb that made it")
        ("media_type" "image/png" "what the bytes are")
        ("path" path "the file the model looked at")
        ("png_path" path "a PNG is its own preview — no transcode, no copy"))
      (is-shape fact ((:integer "width") eql 1) ((:integer "height") eql 1)
        ((:integer "bytes") eql 70))
      (is (search "image/png 1x1" receipt) receipt)
      (is-table (in) (search in receipt) ("70 bytes" "the receipt names the size")
        (path "and the file") ("next round" "and says when the image arrives"))
      (is (equal `(("call-look"
                    ("image/png" . ,(cl-base64:usb8-array-to-base64-string *one-pixel-png*))))
                 groups))
      (nle:look path)
      (is (= 2 (length (cdr (first (nle::turn-images "look-turn")))))))))

(deftest look-refuses-what-is-not-an-image-and-a-missing-turn ()
  (with-temp-file (path :type "png" :contents "not a png at all")
    (is-carrying (report (refusal-text error (nle:look path)))
      ("not an image" report)
      ("PNG, JPEG, GIF or WebP" "the refusal names what would do")))
  (let ((report (refusal-text error (nle:look (namestring (temp-path "look-absent" "png"))))))
    (is (search "cannot read" report) report))
  (with-look-image (path "look-noturn")
    (let* ((nle::*current-durable-turn* nil)
           (report (refusal-text error (nle:look path))))
      (is (search "no turn is running" report) report))))

(deftest look-refuses-a-model-that-reads-text-only ()
  ;; The codex view_image gate: models.dev's modalities.input decides before
  ;; any bytes are staged; a model the catalog has no row for stays unknown
  ;; and looks, the provider's own answer deciding.
  (with-temp-file (nle::*models-catalog-path*
                   :contents "{\"zai\":{\"models\":{\"glm-5.3\":{\"modalities\":{\"input\":[\"text\"],\"output\":[\"text\"]}}}},\"anthropic\":{\"models\":{\"claude-opus-5\":{\"modalities\":{\"input\":[\"text\",\"image\"],\"output\":[\"text\"]}}}}}")
    (with-look-image (path "look-modality" :turn-id "look-modality-turn" :call "call-look")
      (is (null (nle::model-reads-images-p "glm-5.3" "zai")))
      (is (eq t (nle::model-reads-images-p "claude-opus-5" "anthropic")))
      (is (eq :unknown (nle::model-reads-images-p "some-local-model" "lan")))
      (let ((nle::*model* "glm-5.3") (nle::*provider* "zai"))
        (is-carrying (report (refusal-text error (nle:look path))) "reads text only" "select-model"
          (is (null (nle::turn-images "look-modality-turn")))))
      (let ((nle::*model* "claude-opus-5") (nle::*provider* "anthropic"))
        (is (search "image/png" (nle:look path))))
      (let ((nle::*model* "some-local-model") (nle::*provider* "lan"))
        (is (search "image/png" (nle:look path)))))))

(deftest look-refuses-once-this-turn-had-an-image-refused ()
  ;; When the provider refuses a second request that carries the turn's
  ;; images, they are withheld and the staging marked. A second look in the
  ;; same turn is refused with the reason instead of sent into the same
  ;; refusal — the turn is the horizon of that evidence, as it is of the
  ;; images themselves.
  (with-look-image (path "look-withheld" :turn-id "look-withheld-turn" :call "call-second")
    (nle::forget-turn-images "look-withheld-turn")
    (nle::attach-turn-images "look-withheld-turn" '(("image/png" . "QUJD"))
                             :anchor "call-first")
    (is (= 1 (nle::withhold-turn-images
              "look-withheld-turn"
              (lambda (n) (format nil "~d withheld" n)))))
    (is (nle::turn-images-withheld-p "look-withheld-turn"))
    (is (zerop (nle::withhold-turn-images
                "look-withheld-turn"
                (lambda (n) (format nil "~d again" n)))))
    (let* ((nle::*provider* "relay")
           (nle::*model* "alias")
           (report (refusal-text error (nle:look path))))
      (is (search "relay alias did not accept" report) report)
      (is (search path report) "the refusal names the file it will not send")
      (is (search "select-model" report) "and the way out")
      (is (equal '(("call-first" (:withheld . "1 withheld")))
                 (nle::turn-images "look-withheld-turn"))))))

(deftest look-reaches-the-provider-behind-the-round-that-looked (with-session-store ("look-loop"))
  ;; The whole loop: the model calls (look path) through eval, the
  ;; next round carries the receipt as the tool result and the image as a
  ;; user message behind it, and the next turn carries the receipt alone —
  ;; the image was live turn state.
  (with-look-image (path "look-loop" :turn-id "look-loop")
    (let ((rounds '())
          (session (nle:intern-session "look-loop")))
      (with-provider-stub (context
                           (push (coerce context.messages 'list) rounds)
                           (if (= 1 (length rounds))
                               (tool-call-round "call-look" "eval" "form" (format nil "(look ~s)" path))
                               (wire-message "assistant" :content "a red pixel")))
        (nle:run "what is in the file?" :session session
                 :command-id "look-loop-1")
        (nle:run "and now?" :session session :command-id "look-loop-2"))
      (is (= 3 (length rounds)) "two rounds for the looking turn, one for the next")
      (destructuring-bind (call result image) (last (second (reverse rounds)) 3)
        (is (vectorp (gethash "tool_calls" call)))
        (is (equal "call-look" (gethash "tool_call_id" result)))
        (is (search "[image:" (gethash "content" result)))
        (is (equal "user" (gethash "role" image)))
        (let ((parts (nle::message-content-parts (gethash "content" image))))
          (is (= 1 (length parts)) "carrying the image alone")
          (is (equal (format nil "data:image/png;base64,~a"
                             (cl-base64:usb8-array-to-base64-string
                              *one-pixel-png*))
                     (gethash "url" (gethash "image_url" (first parts)))))))
      (let ((next-turn (first rounds)))
        (is (notany #'image-message-p next-turn))
        (is (find-if (lambda (message)
                       (and (equal "tool" (gethash "role" message))
                            (search "[image:" (gethash "content" message))))
                     next-turn))))))

;;; --- a picture an answer names ---------------------------------------------
;;; The answer's half of the image fact: what ![alt](path) in a completed
;;; message leaves on that message's fact -- a LOOK's fact and the target as
;;; written -- and that a target which is no local image leaves nothing and
;;; fails nothing.

(deftest markdown-image-targets-are-the-images-outside-code ()
  (let ((prose (format nil "See ![a](a.png) and ![](https://x.test/b.png).~%- ![c](file:///tmp/c.png) ![again](a.png)~%![d](</tmp/d e.png>) ![d again](<a.png>)"))
        (code (format nil "`![a](a.png)`~%```md~%![b](b.png)~%```~%[link](c.png) ![spaced](d e.png)")))
    (is (equal '("a.png" "https://x.test/b.png" "file:///tmp/c.png" "/tmp/d e.png") (nle::markdown-image-targets prose)) "each target once, in order, a bracketed one without its brackets")
    (is (null (nle::markdown-image-targets code)) "a code span's, a fence's, a link and a target with a space are no image")))

(deftest an-answer-s-local-picture-is-an-image-fact (with-temp-workspace (cache))
  (with-look-image (path "answer-image")
    (with-temp-file (prose :type "png" :contents "not a png at all")
      (with-stubbed-fdefinition (nlk:cache-path (relative) (merge-pathnames relative cache))
        (let* ((facts (nle::answer-images
                       (format nil "![dot](~a) ![web](https://example.invalid/x.png) ![gone](~a) ![prose](~a) ![data](data:image/png;base64,AAAA) ![url](file://~a)"
                               path (namestring (temp-path "answer-absent" "png")) prose path)))
               (fact (and facts (aref facts 0)))
               (copy (nlk:json-value fact :text "copy_path")))
          (is (= 2 (length facts)) "the file, and the file: URL of it: a remote, a missing, a non-image and a data: target are none")
          (is-shape fact ("payload_kind" "file" "a LOOK's shape") ("source" "answer" "said by an answer")
            ("target" path "the target as the answer wrote it") ("path" path "the file it names here")
            ("media_type" "image/png") ("bytes" 70) ("width" 1) ("height" 1)
            ("png_path" copy "a PNG's copy is its own preview"))
          (is (= 64 (length (gethash "id" fact))) "named by its bytes, for the route")
          (is (uiop:subpathp (pathname copy) cache) "the bytes are kept in the cache")
          (is (equalp *one-pixel-png* (alexandria:read-file-into-byte-vector copy)) "as the file held them")
          (is-shape (aref facts 1) ("target" (format nil "file://~a" path) "a file: URL keeps its spelling")
            ("path" path "and names the same file") ("id" (gethash "id" fact) "the same bytes"))
          (is (null (nle::answer-images "No picture here, only [a link](x.png).")) "an answer naming none has no facts")
          (let ((nle::*answer-image-limit* 1))
            (is (= 1 (length (nle::answer-images (format nil "![a](~a) ![b](file://~a)" path path)))) "one answer carries no more than the limit")))))))

(deftest an-answer-naming-a-picture-keeps-its-fact-and-sends-the-model-nothing (with-temp-store ())
  ;; The whole loop: the round's message fact carries the picture's fact
  ;; beside the wire message, a replay folds it onto the answer's row, and the
  ;; next round's request holds the text alone.
  (with-temp-workspace (folder)
    (alexandria:write-byte-vector-into-file *one-pixel-png* (merge-pathnames "dot.png" folder))
    (nlk:create-session :id "answer-loop" :cwd (namestring folder))
    (let ((rounds '())
          (session (nle:intern-session "answer-loop"))
          (answer "Here it is: ![dot](dot.png), not ![gone](absent.png) nor ![web](https://example.invalid/x.png)."))
      (with-stubbed-fdefinition (nlk:cache-path (relative) (merge-pathnames relative folder))
        (with-provider-stub (context
                             (push (coerce context.messages 'list) rounds)
                             (wire-message "assistant" :content answer))
          (is (equal answer (nle:run "draw it" :session session :command-id "answer-loop-1")) "the turn ends on its answer")
          (nle:run "and again" :session session :command-id "answer-loop-2")))
      (let* ((payload (first (nlk:events :session-id "answer-loop" :kind nlk::+kind-assistant-completed+
                                         :as :payloads)))
             (images (nlk:json-array payload "metadata" "images")))
        (is (= 1 (length images)) "the one local image; the missing file and the remote stay links")
        (is-shape (aref images 0) ("target" "dot.png")
          ("path" (namestring (merge-pathnames "dot.png" folder)) "resolved against the session's folder, as LOOK resolves"))
        (is (null (gethash "metadata" (gethash "message" payload))) "the wire message is as the provider wrote it"))
      (is (search "![alt](path)" (cdr (assoc "folder" (nle::read-harness-sections "answer-loop") :test #'string=))) "every session's standing text says an answer may")
      (let ((said (remove "assistant" (first rounds) :test-not #'equal :key (lambda (message) (gethash "role" message))))
            (pictured (lambda (message)
                        (or (gethash "metadata" message) (image-message-p message)))))
        (is (equal (list answer) (mapcar (lambda (message) (gethash "content" message)) said)) "the next turn's request carries the answer's text")
        (is (notany pictured (first rounds)) "and no picture: an answer's is the operator's alone"))
      (let* ((row (find-if (lambda (row) (and (eq :text (nle::transcript-row-kind row))
                                              (equal "assistant" (nle::transcript-row-role row))))
                           (nle::session-transcript "answer-loop")))
             (images (nlk:json-array (nle::transcript-row-metadata row) "images")))
        (is (equal '("dot.png") (map 'list (lambda (image) (gethash "target" image)) images)) "a replay folds the fact onto the answer's row")))))

;;; --- a picture sent with a prompt --------------------------------------------
;;; The prompt's half: the attachment reaches the model with the turn and
;;; leaves with it, and the operator's own message keeps an image fact for it,
;;; its bytes kept the way an answer's are, on the live stream and in every
;;; replay a reloaded page is sent.

(deftest a-prompt-s-pictures-are-image-facts-with-their-bytes-kept (with-temp-workspace (cache))
  (with-stubbed-fdefinition (nlk:cache-path (relative) (merge-pathnames relative cache))
    (let* ((facts (nle::prompt-images
                   (list (cons "image/png" (cl-base64:usb8-array-to-base64-string *one-pixel-png*))
                         (cons "image/png" "aGk=")
                         (cons "image/png" "not base64 at all"))))
           (fact (and facts (aref facts 0)))
           (copy (nlk:json-value fact :text "copy_path")))
      (is (= 1 (length facts)) "the picture: bytes that open no image are none")
      (is-shape fact ("payload_kind" "file") ("source" "prompt" "sent with a prompt")
        ("media_type" "image/png" "read off the bytes") ("bytes" 70) ("width" 1) ("height" 1)
        ("png_path" copy "a PNG's copy is its own preview"))
      (is (null (gethash "path" fact)) "it came as bytes: no file to name")
      (is (= 64 (length (gethash "id" fact))) "named by its bytes, for the route")
      (is (equalp *one-pixel-png* (alexandria:read-file-into-byte-vector copy)) "the bytes kept in the cache")
      (is (null (nle::prompt-images (list (cons "image/png" "aGk=")))) "a prompt with no picture has no facts"))))

(deftest a-picture-sent-with-a-prompt-stays-in-the-rows-a-reload-receives ()
  ;; The wire's whole seam: a start_turn's image item, the input fact's
  ;; picture, the row a page reloading is sent, the bytes the route serves by
  ;; that row's id, and the Markdown copy; a follow-up keeps its picture
  ;; through the queue to the turn its promotion starts.
  (with-temp-workspace (cache)
    (with-stubbed-fdefinition (nlk:cache-path (relative) (merge-pathnames relative cache))
      (with-gateway-session (port "prompt-picture")
        (let ((png (cl-base64:usb8-array-to-base64-string *one-pixel-png*)))
          (with-provider-stub (nil (wire-message "assistant" :content "a red dot"))
            (frames-while (ws port :subscribe '("prompt-picture"))
              (nle::send-start-turn ws "prompt-picture" "what is this?"
                                    :command-id "prompt-picture-1" :images `(("image/png" . ,png))))
            (is-eventually (not (nlk:active-turn-p "prompt-picture")) "the turn ran"))
          (multiple-value-bind (rows order)
              (rows-client (frames-while (ws port) (send-command ws (rows-subscribe "prompt-picture"))))
            (let* ((ask (find-if (lambda (row) (equal "user" (gethash "role" row)))
                                 (mapcar (lambda (id) (gethash id rows)) order)))
                   (images (nlk:json-array ask "metadata" "images"))
                   (id (and (plusp (length images)) (gethash "id" (aref images 0)))))
              (is (equal "what is this?" (gethash "text" ask)) "the operator's row")
              (is (= 1 (length images)) "carries the picture sent with it")
              (is-shape (aref images 0) ("source" "prompt") ("media_type" "image/png") ("width" 1) ("height" 1))
              (multiple-value-bind (body status)
                  (nlk:http :get (format nil "http://127.0.0.1:~d/api/gateway/sessions/prompt-picture/images/~a" port id)
                            :headers '(("authorization" . "Bearer test-token")) :binary t)
                (is (and (= 200 status) (equalp *one-pixel-png* (coerce body '(vector (unsigned-byte 8))))) "the route serves its bytes by the row's id"))))
          (is (search "_Sent with 1 picture_" (nle::session-markdown "prompt-picture")) "the Markdown copy says so")
          (let ((busy (nlk::active-input-admission-turn
                       (nlk:admit-active-turn-input "prompt-picture" "pp-busy" "busy"))))
            (nlk:admit-active-turn-input "prompt-picture" "pp-follow" "and this one?"
                                         :images `(("image/png" . ,png))
                                         :metadata (nlk:json-object "images" (nle::prompt-images `(("image/png" . ,png)))))
            (nlk:complete-turn busy nil)
            (let* ((next (nlk::dequeue-next-turn "prompt-picture"))
                   (started (nlk:events :turn-id (nlk:durable-turn-turn-id (first next))
                                        :kind nlk::+kind-input-committed+ :as :payload)))
              (is (= 1 (length (nlk:json-array started "metadata" "images"))) "a follow-up's picture rides the turn it promotes to")
              (nlk:complete-turn (first next) nil))))))))

;;; --- child liveness and the exit wake ------------------------------------

(deftest sh-children-lead-their-own-sessions (let ((answer (nle:sh "echo $$ $(ps -o sid= -p $$)"))))
  ;; setsid gives every sh child a session of its own, so there is no
  ;; controlling terminal to be stopped over: /dev/tty simply fails.
  ;; Without setsid, a sh child would inherit the serving TUI's controlling
  ;; terminal; a background write would stop the tree with SIGTTOU, and the
  ;; timeout cannot kill it.
  (destructuring-bind (pid sid) (nlk:split-words answer)
    (is (equal pid sid)
        (format nil "the child is its own session leader: ~a" answer))))

(deftest stopped-child-tree-collapses-with-a-verdict (with-exec-registry (registry))
  ;; A job-control-stopped process can never finish here: nothing holds
  ;; job control to resume it, and SIGTERM pends undelivered on a stopped
  ;; process. The liveness scan names the verdict on the output stream and
  ;; SIGKILLs the tree, so the evaluation exits instead of parking its
  ;; readers forever.
  (let ((id (spawn-test-eval
             registry :form "(sh \"kill -STOP $$\")"
             :value (lambda ()
                      (format nil "~s" (nth-value 1 (nle:sh "kill -STOP $$; echo resumed")))))))
    (await (:rest 0.02) (nle::exec-process-child-pid (nle::find-background-eval registry id)))
    (is (await (:tries (* 4 nle::+exec-stall-ticks+) :rest 0.02)
          (nle::scan-exec-child-stalls registry)))
    (nle::exec-park-until registry id 5000)
    (let ((snapshot (nle::eval-status id)))
      (is (eq :exited (getf snapshot :status)) "the evaluation exited")
      (is (search "stopped by job control" (getf snapshot :output))))))

(deftest an-exit-wake-is-claimed-once-and-reads-nothing (with-exec-registry (registry))
  ;; The wake fires at most once per evaluation: the claim decides between
  ;; the watcher edge and the turn-end edge, and an observed or already
  ;; claimed exit never wakes again. The claim reads nothing -- the wake
  ;; turn's (eval-status ID) call is the read, so the output waits for it.
  (let ((id (spawn-test-eval registry :form "(wake-fixture)" :prints "out-line" :value "42"
                                      :session-id "wake-sid" :turn-id "wake-turn"
                                      :tool-call-id "call-w")))
    (nle::mark-exec-backgrounded registry id)
    (nle::exec-park-until registry id 5000)
    (let ((pending (nle::unobserved-exited-evals registry "wake-sid")))
      (is (equal (list (list :id id :turn-id "wake-turn")) pending)))
    (is (null (nle::unobserved-exited-evals registry "other-sid")))
    (is (nle::claim-exit-wake registry id) "the first claim wins")
    (is (null (nle::claim-exit-wake registry id)) "and there is no second")
    (is (null (nle::unobserved-exited-evals registry "wake-sid")))
    ;; A turn admitted since the idle check keeps the wake turn from
    ;; starting: the claim is given back, and that turn's end claims it.
    (nle::release-exit-wakes registry (list id))
    (is (nle::claim-exit-wake registry id) "a released wake is claimed again")
    (let ((status (nle::eval-status id)))
      (is (equal "out-line" (getf status :output)) "the output waits for the wake turn's read")
      (is (equal "42" (getf status :value))))))

(defun wake-turns (session-id)
  "SESSION-ID's turns no one typed: an exit wake's."
  (remove-if-not (lambda (event) (equal "" (nlk:turn-started-input event)))
                 (session-events session-id :type 'nlk:turn-started)))

(defun wake-reads (session-id turn-id)
  "The tool results TURN-ID of SESSION-ID recorded: an exit wake's reads first."
  (loop for event in (session-events session-id :type 'nlk:tool-result-event)
        when (equal turn-id (nlk:event-turn-id event))
          collect (nlk:tool-result-event-result event)))

(deftest bg-exit-wakes-the-completed-turns-session (let ((asked '())))
  ;; The watcher edge end to end: the model ends its turn with the
  ;; evaluation still running; the exit's final update finds the spawning
  ;; turn completed and wakes the session -- a turn no one typed, with no
  ;; user message, opening on an (eval-status ID) call whose result carries
  ;; the exit: the output reaches the model as a tool result, never as a
  ;; prompt the operator did not write.

  (with-provider-stub (context (push (nle::compiled-turn-context-messages context) asked)
                               (wire-message "assistant" :content "noted"))
    (with-backgrounded-eval (sid "bg-wake" "bg-wake-command"
                             ("call-wake" "(progn (sleep 0.4) 7)"))
      (is-present (wake (await () (first (wake-turns sid))))
        "the exit woke the session with a durable turn"
        (is (notany (lambda (event) (equal (nlk:event-turn-id wake) (nlk:event-turn-id event)))
                    (session-events sid :kind nlk::+kind-input-committed+)) "and no user message")
        (is-present (read (await () (first (wake-reads sid (nlk:event-turn-id wake)))))
          "the wake opened on the read"
          (is-carrying (text read) ":EXITED" ":VALUE \"7\"")))
      ;; What the provider is asked: the run turn's one prompt, then the
      ;; read as an assistant call and its tool result -- no second user.
      (let ((messages (await () (first asked))))
        (let ((users (count "user" messages :key (lambda (m) (gethash "role" m)) :test #'equal)))
          (is (= 1 users) "the only user message is the one the operator typed"))
        (is (equal "tool" (gethash "role" (elt messages (1- (length messages))))) "the provider reads the wake as a tool result")))))

(deftest bg-exits-on-a-busy-session-wake-one-turn-once-it-idles ()
  ;; A busy session leaves its exits unclaimed, and the idle session wakes
  ;; once.
  (with-backgrounded-eval (sid "bg-merge" "bg-merge-command" ("call-m1" "(progn (sleep 0.2) 11)")
                           ("call-m2" "(progn (sleep 0.3) 22)"))
    (with-durable-turn (busy sid :command-id "bg-merge-busy" :input "keep busy" :complete t)
      (is-eventually (and (snippet-exit-fact sid "call-m1") (snippet-exit-fact sid "call-m2"))
                     "the watcher saw both exits while the turn ran")
      ;; One more watcher tick past the exit facts: the wake edge runs after them.
      (sleep 0.4)
      (is (zerop (length (nlk::queue-prompts sid)))))
    (with-provider-stub (nil (wire-message "assistant" :content "noted"))
      (nle::wake-idle-session sid)
      (let ((wakes (wake-turns sid)))
        (is (= 1 (length wakes)) "one wake turn")
        (is-present (wake (first wakes)) "the idle session woke"
          (let ((both (await () (= 2 (length (wake-reads sid (nlk:event-turn-id wake)))))))
            (is both "one read per exit, in the one turn"))
          (is-carrying (text (format nil "~{~a~^ ~}" (wake-reads sid (nlk:event-turn-id wake))))
            ":VALUE \"11\"" ":VALUE \"22\""))))))

(deftest edit-names-the-paren-verdict-on-a-lisp-file ()
  ;; The receipt says.
  (with-temp-file (path :type "lisp"
                        :contents (format nil "(defun a (x)~%  (list x))~%~%(defun b (y)~%  y)~%"))
    (is (search "parens balance" (nle:edit path "  y)" "  y) ")) "a balanced edit says so")
    (is-carrying (receipt (nle:edit path "(list x))" "(list x)"))
      ("the form opened at line 1 ((defun a (x)) never closes: 1 paren short" receipt))
    (is (search "parens balance" (nle:edit path "(list x)" "(list x))")))
    (is-carrying (receipt (nle:edit path "  y) " "  y))")) "a \")\" at line 5 closes nothing")
    (is-carrying (receipt (nle:edit path "  y))" "  \"y)")) "string opened at char")
    (is (search "parens balance" (nle:write-file path (format nil "(defun c () 1)~%")))))
  (with-temp-file (path :type "txt" :contents "(unbalanced")
    (is (null (search "paren" (nle:edit path "unbalanced" "still"))))))

(deftest edit-makes-the-change-live-in-this-image (with-source-tree (root))
  ;; A write to a file of this image's own tree has its changed definitions
  ;; evaluated back into it, and the receipt names what runs here.
  (let ((path (merge-pathnames "verb.lisp" root)))
    (write-temp-file path (evolved-source "(defun exec-probe-verb () :first)"))
    (nlk:load-source path)
    (is (eq :first (evolved-fn "EXEC-PROBE-VERB")) "the file starts live")
    (let ((receipt (nle:edit path ":first" ":second")))
      (is-carrying receipt "parens balance" "live here: exec-probe-verb"))
    (is (eq :second (evolved-fn "EXEC-PROBE-VERB")))
    (let ((receipt (nle:write-file path (evolved-source "(defun exec-probe-verb () :third)"))))
      (is (search "live here: exec-probe-verb" receipt) receipt))
    (is (eq :third (evolved-fn "EXEC-PROBE-VERB")) "write-file lands the same way")
    ;; A tree that does not close is not loaded a piece at a time.
    (is-carrying (receipt (nle:edit path ":third)" ":fourth"))
      ("never closes" receipt)
      (:absent "live:" "a broken file makes nothing live"))
    (is (eq :third (evolved-fn "EXEC-PROBE-VERB")))))

(deftest edit-receipt-counts-the-shells-told (with-source-tree (root))
  ;; A live image should already have the change; the receipt says where the
  ;; change went beyond this process: the seam the serving gateway advises
  ;; answers how many shells were sent the file and the definitions, and the
  ;; clause carries it.
  (with-told-shells (seen 2)
    (let ((path (merge-pathnames "told.lisp" root)))
      (write-temp-file path (evolved-source "(defun exec-told-verb () :first)"))
      (let ((receipt (nle:edit path ":first" ":second")))
        (is (search "live here: exec-told-verb; sent to 2 shells" receipt) receipt))
      (is (equal '(("told.lisp" ("DEFUN" . "exec-told-verb"))) seen)))))

;;; --- the settle contract ----------------------------------------------------

(defmacro with-local-snippet-tables (&body body &aux (history (gensym "HISTORY")))
  "Run BODY with the \"local\" session's history saved on entry and put back
on unwind, so a settle test leaves the image as it found it."
  `(let ((,history (gethash "local" nle::*snippet-histories*)))
     (nlk:with-cleanup ((setf (gethash "local" nle::*snippet-histories*) ,history)) ,@body)))

(deftest snippets-settle-star-in-the-order-they-are-settled (with-exec-registry (registry))
  ;; Two snippets' bodies run, then their settle thunks are called in order:
  ;; nothing reaches the session's tables until a settle, and after
  ;; both * is the second snippet's value, / its values, ** the first's.
  (with-local-snippet-tables
    (nlk:bind ((before (gethash "local" nle::*snippet-histories*))
               ((text-a settle-a) (nle::background-capable-eval "(sh \"printf first\")" 5000))
               ((text-b settle-b) (nle::background-capable-eval "(sh \"printf second\")" 5000)))
      (is (search "first" text-a) text-a)
      (is (search "second" text-b) text-b)
      (is (functionp settle-a) "the first snippet answers a settle thunk")
      (is (functionp settle-b) "the second snippet answers a settle thunk")
      (is (eq before (gethash "local" nle::*snippet-histories*)))
      (funcall settle-a)
      (funcall settle-b)
      (let ((history (nle::snippet-history "local")))
        (is (equal "second" (first (first history))) "* is the second snippet's value")
        (is (equal "first" (first (second history))) "** is the first snippet's value"))
      (is (search "\"second\"" (nle::background-capable-eval "*" 5000))))))

(deftest settle-thunk-rides-the-eval-tool-and-the-inline-lane (with-exec-registry (registry))
  ;; The tool function hands the settle thunk through as its second value,
  ;; and the inline lane — an operator that never backgrounds — files its
  ;; snippet the same way.
  (with-local-snippet-tables
    (multiple-value-bind (text settle)
        (funcall (third (nle::find-tool "eval"))
                 (nlk::make-json-object "form" "(+ 1 2)"))
      (is (search "3" text) text)
      (is (functionp settle) "the tool answers the settle thunk"))
    (nlk:bind ((before (nle::snippet-history "local"))
               ((text settle) (nle::background-capable-eval "(list-evals)" 5000)))
      (is (stringp text))
      (is (functionp settle) "an inline operator's snippet settles too")
      (is (eq before (nle::snippet-history "local")) "nothing filed before the settle")
      (funcall settle)
      (is-let* ((history (nle::snippet-history "local")))
        ((eq before history) not "the settle files the snippet") ((length (first history)) = 1)))))

(deftest a-snippet-past-its-window-settles-at-its-wake (with-exec-registry (registry))
  ;; A snippet still running at the yield window answers the running envelope
  ;; and no settle thunk; its values enter the session's history when its
  ;; wake is claimed, since the wake turn is where the model reads them.
  (with-local-snippet-tables
    (multiple-value-bind (text settle)
        (nle::background-capable-eval "(progn (sleep 0.4) :slow)" 50)
      (is (null settle) "a running snippet has nothing to settle yet")
      (let ((id (gethash "eval_id" (nlk:decode-json text))))
        (is (integerp id) text)
        ;; Wait for the exit without observing it: a snapshot read marks
        ;; the exit observed, and the wake claims only an unobserved one.
        (nle::exec-await-all-exits registry :timeout-seconds 5)
        (is (not (equal '(:slow) (first (nle::snippet-history "local")))))
        (is (nle::claim-exit-wake registry id) "the wake is claimed")
        (is (equal '(:slow) (first (nle::snippet-history "local"))))))))
;;; --- the preparing row's literal preview ------------------------------------
;;; The band row streams the call's arguments; an eval snippet's ($ "name")
;;; grows into its strings entry as that entry streams.

(deftest the-preparing-rows-preview-resolves-eval-snippet-literals ()
  (is-table (preview arguments) (equal preview (nle::shell-command-preview-from-args arguments))
    ("(sh sleep 12)" "{\"form\": \"(sh ($ \\\"cmd\\\"))\", \"strings\": {\"cmd\": \"sleep 12")
    ("(sh sleep 12; echo done)"
     "{\"form\": \"(sh ($ \\\"cmd\\\"))\", \"strings\": {\"cmd\": \"sleep 12; echo done\"}}")
    ("(sh ($ \"cmd\"))" "{\"form\": \"(sh ($ \\\"cmd\\\"))\"}"))
  ;; The band shows a command's lines, so the streamed one keeps its newlines.
  (let ((preview (nle::shell-command-preview-from-args "{\"form\": \"(progn\\n  (sh \\\"make\\\"))")))
    (is (equal (format nil "(progn~%  (sh \"make\"))") preview) "a newline escape is a newline")))

;;; --- how a background shell ended ---------------------------------------------
;;; A backgrounded snippet whose (sh ...) exited 2 said so only as the `[exit 2]'
;;; line of its value, its evaluation exiting 0: eval-status answered
;;; :EXIT-CODE 0 and the page drew nothing wrong. The shell's status is a
;;; field of its own from SH to the closed card a surface draws.

(deftest a-snippets-last-sh-exit-rides-beside-its-exit-code (with-exec-registry (registry))
  (let* ((form "(sh \"echo one; echo two; exit 2\")")
         (id (spawn-test-eval registry :form form
                                       :value (lambda () (nle:sh "echo one; echo two; exit 2"))
                                       :session-id "shell-exit-sid" :turn-id "shell-exit-turn"
                                       :tool-call-id "call-se")))
    (nle::mark-exec-backgrounded registry id)
    (nle::exec-park-until registry id 5000)
    (let* ((update (find :exited (nle::exec-collect-updates registry) :key #'nle::exec-update-status))
           (payload (and update (nle::background-process-update-payload update)))
           (frame (and payload (nlk:sync-message-payload
                                (nlk:decode-sync-message
                                 (nlk:encode-projection-event "shell-exit-sid" 0
                                                              "session_background_process_updated"
                                                              payload))))))
      (is-present (process (nlk:json-value frame :object "process")) "the exit's frame"
        (is-shape process ("exit_code" eql 0 "the evaluation returned a value")
          ("shell_exit" eql 2 "the shell's own status rides beside it")
          ("tail" (format nil "one~%two~%") "and the last lines it wrote")))
      ;; Folded onto the call's row, the frame is the closed card: how it
      ;; ended and what it wrote, what it ran for its title.
      (let ((state (bg-state (bg-call-fact "call-se" (eval-args form) :tool-name "eval")
                             (bg-backgrounding-result "call-se" id form "eval"))))
        (feed-events state frame)
        (is-present (exec (nle::row-exec-json (tool-call-row state "call-se"))) "the closed card"
          (is-shape exec ("end" "exited") ("shell_exit" eql 2) ("exec_id" eql id) ("call_id" "call-se")
            ("command" "echo one; echo two; exit 2" "titled by the command, not the form")
            ("tail" (format nil "one~%two"))))))
    ;; One read: the look that observes an exit reaps it.
    (let ((status (nle::eval-status id)))
      (is-shape status (:exit-code eql 0) (:shell-exit eql 2 "eval-status says it too")))))

(deftest bg-shell-exit-is-durable-and-names-the-wake ()
  ;; The same exit on the durable path: the watcher's exit fact carries the
  ;; status and the lines, the wake turn's start fact names the exit that
  ;; woke it, and a replay folds both -- the closed card, and the line the
  ;; wake opens on -- with no frame of the live stream.
  (with-provider-stub (nil (wire-message "assistant" :content "noted"))
    (with-backgrounded-eval (sid "bg-shell-exit" "bg-shell-exit-command"
                             ("call-sx" "(sh \"sleep 0.4; echo built; exit 2\")"))
      (is-present (fact (await () (snippet-exit-fact sid))) "the watcher appended the exit fact"
        (is-shape (nlk:json-value (nlk::tool-result-event-metadata fact) :object "background_shell")
          ("exit_code" eql 0) ("shell_exit" eql 2) ("tail" (format nil "built~%"))))
      (is-present (wake (await () (first (wake-turns sid)))) "the exit woke the session"
        (is-shape (aref (nlk:turn-started-wake wake) 0) ("exit_code" eql 0) ("shell_exit" eql 2)
          ("eval_id" is "naming the evaluation")))
      (let ((state (replay-state sid)))
        (is-present (exec (nle::row-exec-json (tool-call-row state "call-sx"))) "replay closes the card"
          (is-shape exec ("end" "exited") ("shell_exit" eql 2) ("tail" (format nil "built"))))
        (let ((notice (find :system-notice (nle::transcript-rows state) :key #'nle::transcript-row-kind)))
          (is-present (row notice) "the wake turn opens on its line"
            (is-carrying row.text "woke: eval " " exited 2")
            (is-shape (aref (nlk:json-value row.metadata :array "wake") 0) ("end" "exited"))))))))

(deftest a-failed-backgrounded-forms-report-is-durable-and-reaches-the-card ()
  ;; The report of what a background form failed on lived in its eval value
  ;; alone: the exit fact, the TUI card and the web card said `failed' and
  ;; nothing of why. The exit fact carries it, and a replay's
  ;; card sends it on.
  (with-provider-stub (nil (wire-message "assistant" :content "noted"))
    (with-backgrounded-eval (sid "bg-failed-form" "bg-failed-form-command"
                             ("call-ff" "(progn (sleep 0.4) (error \"fixture refused\"))"))
      (is-present (fact (await () (snippet-exit-fact sid))) "the watcher appended the exit fact"
        (is-shape (nlk:json-value (nlk::tool-result-event-metadata fact) :object "background_shell")
          ("exit_code" eql 1) ("error" "SIMPLE-ERROR: fixture refused")))
      (let ((state (replay-state sid)))
        (is-present (exec (nle::row-exec-json (tool-call-row state "call-ff"))) "replay closes the card"
          (is-shape exec ("end" "failed") ("error" "SIMPLE-ERROR: fixture refused")))))))

;;; --- a failed eval says so -------------------------------------------------

(deftest a-failed-eval-marks-its-call (with-exec-registry (registry))
  ;; An eval whose form signals, inline or in its window, or whose last (sh ...)
  ;; exits non-zero -- grep's 1 as much as any, EXEC-END's rule -- marks its call
  ;; failed. Without this, a failed eval would draw as a success.
  ;; EVAL-VERDICT: whether FORM, run as a tool body runs under the dispatch seam, recorded
  ;; its call failed (NLE:FAILURE), the flag the result fact carries.
  (flet ((eval-verdict (form)
           (let ((nle::*executing-tool-call-id* "call-verdict")
                 (nle::*tool-call-failed* nil))
             (nle::background-capable-eval form 5000)
             nle::*tool-call-failed*)))
    (is (eval-verdict "(car (list* 5))") "a form that signals")
    (is (eval-verdict "(eval-status)") "an inline form that signals")
    (is (eval-verdict "(sh \"echo out; exit 3\")") "a last sh that exits 3")
    (is (eval-verdict "(sh \"grep -q absent /dev/null\")") "grep's 1")
    (is (not (eval-verdict "(sh \"true\")")) "a clean sh")
    (is (not (eval-verdict "(+ 1 2)")) "a value")))
