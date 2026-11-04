;;;; rows-test.lisp --- a session's transcript as rows (gateway/rows.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun rows-subscribe (&rest session-ids)
  "The subscription command asking for SESSION-IDS' rows."
  (nlk:encode-client-command
   (nlk:make-gateway-command
    :kind :set-session-subscriptions
    :request (nlk:json-object
              "subscriptions" (map 'vector (lambda (sid) (nlk:json-object "session_id" sid "rows" t))
                                   session-ids)))))

(defun rows-client (frames &aux (rows (make-hash-table)) (order '()))
  "The rows a client holds after FRAMES, as a browser folds them: id -> row,
each row's text whole; and the ids in their order."
  (dolist (frame (frames-of-type frames "session_rows"))
    (let ((payload (gethash "payload" frame)))
      (when (gethash "reset" payload) (clrhash rows) (setf order '()))
      (loop for row across (gethash "rows" payload)
            for id = (gethash "id" row)
            for held = (gethash id rows)
            do (when (gethash "append" row)
                 (setf (gethash "text" row)
                       (concatenate 'string (gethash "text" held) (gethash "append" row))))
               (unless held (setf order (append order (list id))))
               (setf (gethash id rows) row))
      (loop for id across (or (gethash "gone" payload) #())
            do (remhash id rows) (setf order (remove id order)))
      (when (gethash "order" payload)
        (setf order (coerce (gethash "order" payload) 'list)))))
  (values rows order))

(defun rows-texts (frames)
  (multiple-value-bind (rows order) (rows-client frames)
    (mapcar (lambda (id) (gethash "text" (gethash id rows))) order)))

(deftest a-rows-subscription-is-sent-rows-and-never-the-frames (with-temp-gateway (port))
  (seed-completed-turn "rows-a" "hello" "answer"
                       :tools '(("c1" "eval" "{\"form\": \"(+ 1 2)\"}" "3")))
  (let* ((frames (frames-while (ws port) (send-command ws (rows-subscribe "rows-a"))))
         (sent (frames-of-type frames "session_rows")))
    (is (plusp (length sent)) "the rows arrive")
    (is (eq t (nlk:json-value (first sent) :boolean "payload" "reset")) "the first is the whole transcript")
    (is (subsetp '("hello" "3" "answer") (rows-texts frames) :test #'equal) "prompt, result and answer")
    (let ((tool (find "tool_call" (alexandria:hash-table-values (rows-client frames))
                      :key (lambda (row) (gethash "kind" row)) :test #'equal)))
      (is-shape tool ((:string "tool" "name") "eval") ((:string "tool" "status") "complete"))
      (is (stringp (nlk:json-value tool :string "tool" "title")) "titled in the shell's own words"))
    (is (null (frames-of-type frames "session_stream_resumed")) "the replay's frames stay home")))

(deftest a-rows-client-is-sent-what-a-live-delta-changed (with-temp-gateway (port))
  (seed-completed-turn "rows-b" "hello" "answer")
  (flet ((delta (text)
           (nle::publish-event "rows-b" "item_delta"
                               (nlk:json-object "turn_id" "t-live"
                                                "delta" (nlk:json-object "type" "text" "text" text)))))
    (let ((frames (frames-while (ws port)
                    (send-command ws (rows-subscribe "rows-b"))
                    (sleep 0.5)
                    (delta "stream")
                    (sleep 0.2)
                    (delta "ing"))))
      (is (member "streaming" (rows-texts frames) :test #'equal) "the streamed text, whole")
      (is (null (frames-of-type frames "item_delta")) "and never the deltas themselves"))))

(deftest a-rows-client-is-sent-how-a-calls-forks-stand (with-temp-gateway (port))
  ;; The forks of a running call work in sessions of their own; the gateway
  ;; says how they stand (session_fork_progress), and a rows client has it on
  ;; the call's row, whose forks a page lists, steers and stops.
  (seed-completed-turn "rows-f" "split it" "done"
                       :tools '(("call-fork" "eval" "{\"form\": \"(fork \\\"a\\\")\"}" "working")))
  (let* ((frames (frames-while (ws port)
                   (send-command ws (rows-subscribe "rows-f"))
                   (sleep 0.5)
                   (nle::publish-event "rows-f" "session_fork_progress"
                                       (nlk:json-object "session_id" "rows-f" "tool_call_id" "call-fork"
                                                        "forks" (vector (nlk:json-object "n" 1 "label" "a" "session_id" "fork-x-1"
                                                                                         "turn_id" "turn-x" "status" "running"
                                                                                         "calls" 2))))
                   (sleep 0.3)))
         (tool (find "tool_call" (alexandria:hash-table-values (rows-client frames))
                     :key (lambda (row) (gethash "kind" row)) :test #'equal))
         (fork (aref (nlk:json-value tool :array "metadata" "forks") 0)))
    (is-shape fork ("session_id" "fork-x-1") ("turn_id" "turn-x") ("status" "running") ("calls" 2))
    (is (null (frames-of-type frames "session_fork_progress")) "and never the frame itself")))

(deftest a-rows-client-that-subscribes-mid-round-reads-the-answer-from-its-first-word
    (with-temp-gateway (port))
  ;; Without this, a rows client that subscribes mid-sentence would open
  ;; on whatever streams after it subscribes, not from the first word.
  (ensure-durable-session "rows-mid")
  (let ((stream (round-in-flight "rows-mid" "slow please")))
    (funcall stream "text" "text" "### Part 1")
    (funcall stream "text" "text" (format nil "~%~%The gateway"))
    (let ((frames (frames-while (ws port)
                    (send-command ws (rows-subscribe "rows-mid"))
                    (sleep 0.5)
                    (funcall stream "text" "text" " keeps one writer"))))
      (let ((answer (format nil "### Part 1~%~%The gateway keeps one writer")))
        (is (equal (list "slow please" answer) (rows-texts frames)) "from its first word, once")))))

(deftest a-rows-view-closes-with-its-client (with-temp-gateway (port))
  (seed-completed-turn "rows-c" "hello" "answer")
  (frames-while (ws port) (send-command ws (rows-subscribe "rows-c")))
  (is-eventually (zerop (hash-table-count nle::*rows-views*)) "no view outlives its socket"))

(deftest an-in-process-shells-rows-view-closes-with-its-link (with-temp-gateway (port))
  ;; Given the shell that boots this image's gateway, attached in-process, and
  ;; subscribed to a session's rows,
  ;; When it hangs up through GATEWAY-LINK-CLOSE, as it does on its way out,
  ;; Then its rows view closes with it, as a socket's close closes a page's.
  (declare (ignorable port))
  (seed-completed-turn "rows-lb" "hello" "answer")
  (let ((link (nle::attach-loopback-gateway-peer (lambda (text) (declare (ignore text)) nil))))
    (is (typep link 'nle::gw-loopback-client) "the shell attaches in-process")
    (nle::send-gateway-command
     link :set-session-subscriptions
     (nlk:json-object "subscriptions" (vector (nlk:json-object "session_id" "rows-lb" "rows" t))))
    (is-eventually (gethash link nle::*rows-views*) "the subscribe opens its rows view")
    (nle::gateway-link-close link)
    (is-eventually (null (gethash link nle::*rows-views*)) "no view outlives the in-process link")))

(defun output-band-row (output &rest keys)
  (apply #'nle::make-bg-band-row :output-tail (nlk:utf-8-octets output) keys))

(deftest a-rows-view-sends-its-context-and-execs-when-they-change (with-temp-store (store))
  ;; A browser cannot read the band or the usage off the frames it is never
  ;; sent, so the payload carries them -- once each, when they change.
  (let* ((view (nle::make-rows-view :client nil :session-id "rows-d"))
         (state (nle::rows-view-state view)))
    (setf (gethash "context_usage" state) (list :input 1000 :cached 200 :cache-write 0 :model "m-1")
          (gethash "background_processes" state)
          (list (output-band-row (format nil "compiling~%compiled~%")
                                 :shell-session-id 7 :command "just build")))
    (let* ((first (nle::rows-view-payload view))
           (exec (aref (gethash "execs" first) 0)))
      (is (= 1200 (nlk:json-value first :integer "context" "used")) "input and cache reads fill the window")
      (is-shape exec ((:integer "exec_id") 7) ((:string "command") "just build")
                ((:string "tail") (format nil "compiling~%compiled"))))
    (is (null (nle::rows-view-payload view)) "nothing changed, nothing sent")
    (setf (gethash "background_processes" state) '())
    (let ((next (nle::rows-view-payload view)))
      (is (equalp #() (gethash "execs" next)) "an exec that ended leaves an empty list")
      (is (null (gethash "context" next)) "and the context is not sent again"))))

(deftest a-rows-client-is-sent-a-parked-steer-live (with-temp-gateway (port))
  ;; A steer parks with no fact of its own until promotion, and its durable
  ;; queue snapshot is evidence that never streams: the queue's live frame is
  ;; the only way an attached peer -- a browser folding rows included -- ever
  ;; sees it, and so the only way to take it back from there.
  (seed-completed-turn "rows-e" "hello" "answer")
  (with-stubbed-fdefinition (nle::newest-queue-snapshot (session-id)
                              (nlk:json-object "type" "session_input_queue_snapshot_updated"
                                               "session_id" session-id
                                               "snapshot" (nlk:json-object
                                                           "count" 0
                                                           "pending_prompts"
                                                           (vector (nlk:json-object
                                                                    "prompt_id" "p-steer"
                                                                    "content" "go left"
                                                                    "disposition" "steer")))))
    (let ((rows (alexandria:hash-table-values
                 (rows-client (frames-while (ws port)
                                (send-command ws (rows-subscribe "rows-e"))
                                (sleep 0.5)
                                (nle::publish-queue-snapshot "rows-e"))))))
      (is-present (row (wire-row "pending_input" rows "kind")) "the parked steer arrives as a row"
          (is-shape row ((:string "prompt_id") "p-steer") ((:string "text") "go left")
                    ((:boolean "steer") t))))))

(deftest a-withdrawn-follow-up-stays-gone-for-a-page-that-joins-later (with-temp-gateway (port))
  ;; A follow-up's admission fact is on the record, so a replay folds it as
  ;; pending; the queue snapshot a subscription is sent after its replay --
  ;; a turn running or none -- is what takes a withdrawn one back out.
  (nlk:create-session :id "rows-w")
  (flet ((pending ()
           (sort (loop for row in (alexandria:hash-table-values
                                   (rows-client (frames-while (ws port)
                                                  (send-command ws (rows-subscribe "rows-w")))))
                       when (equal "pending_input" (gethash "kind" row))
                         collect (gethash "text" row))
                 #'string<)))
    (let ((turn (%admitted-active-turn "rows-w" "rw-busy" "busy")))
      (nlk:admit-active-turn-input "rows-w" "rw-keep" "keep me")
      (nlk:admit-active-turn-input "rows-w" "rw-back" "take me back")
      (is (equal '("keep me" "take me back") (pending)) "both wait")
      (nlk:withdraw-pending-prompt "rows-w" "rw-back")
      (is (equal '("keep me") (pending)) "the withdrawn one is gone while the turn runs")
      (nlk:withdraw-pending-prompt "rows-w" "rw-keep")
      (nlk:complete-turn turn nil)
      (is (null (pending)) "and after it ended"))))

(deftest a-rows-client-reads-how-a-turn-ended-off-its-resting-row (with-temp-store (store))
  ;; The page marks a stopped answer where it was cut and offers a failed
  ;; prompt again: it reads the terminal fact's kind off the row the turn
  ;; rested on, never the row's words.
  (let* ((view (nle::make-rows-view :client nil :session-id "rows-f"))
         (state (nle::rows-view-state view)))
    (nle::ingest-turn-terminal state "cancelled" :turn-id "t-stopped")
    (nle::ingest-turn-terminal state "failed" :turn-id "t-failed" :detail "status 500")
    (let ((rows (coerce (gethash "rows" (nle::rows-view-payload view)) 'list)))
      (flet ((rested (turn-id) (wire-row turn-id rows "turn_id")))
        (is-shape (rested "t-stopped") ((:string "kind") "finish_divider") ((:string "outcome") "cancelled"))
        (is-shape (rested "t-failed") ((:string "kind") "system_notice") ((:string "outcome") "failed")
                  ((:boolean "error") t))))))

(deftest a-rows-tool-card-reads-for-the-operator (with-temp-store (store))
  ;; Without this, a card would print what the harness writes for the model --
  ;; the offer to set a watch, the `[text: N lines]' header, a shell command
  ;; as its Lisp form, the background handoff's instructions, a stop's
  ;; compiler report under `failed'. The rows carry the operator's reading
  ;; beside the model's text: the command a `$ ' line shows, the result without
  ;; the notes, a code-titled snippet's code apart from its words, and the end
  ;; each card says -- stopped, in the background -- as the status.
  (let* ((view (nle::make-rows-view :client nil :session-id "rows-cards"))
         (state (nle::rows-view-state view)))
    (flet ((call (id form result &optional metadata)
             (nle::ingest-tool-call-fact state id "eval" (nlk:encode-json-object (nlk:json-object "form" form))
                                         :turn-id "t1")
             (nle::ingest-tool-result-fact state id "eval" result :turn-id "t1" :metadata metadata)))
      (call "c-sh" "(sh \"ls -la\")"
            (format nil "[text: 2 lines, 3 characters]~%a~%b"))
      (call "c-code" "(+ 1 2)" "3")
      (call "c-handoff" "(sh \"just build\")"
            "{\"status\":\"running\",\"eval_id\":7,\"output\":\"\",\"message\":\"Form is still evaluating in the background.\"}"
            (nlk:json-object "background_shell" (nlk:json-object "shell_session_id" 7 "status" "running"
                                                                 "command" "(sh \"just build\")")))
      (call "c-stop" "(sh \"for i in 1 2 3; do echo step $i; sleep 1; done\")"
            (format nil "; compilation unit aborted~%ERROR: evaluation aborted by (eval-interrupt)")
            (nlk:json-object "stopped" (nle::stop-document :operator :after-ms 2400
                                                           :tail (format nil "step 1~%step 2"))))
      (call "c-read" "(eval-status 7)" "(:ID 7 :STATUS :EXITED :OUTPUT \"\" :VALUE \"a\" :EXIT-CODE 0)"
            (nlk:json-object "background_read" (nlk:json-object "status" "exited" "end" "done" "shell_exit" 0
                                                                "text" (format nil "tick 1~%tick 2")))))
    (let ((rows (coerce (gethash "rows" (nle::rows-view-payload view)) 'list)))
      (flet ((tool (call-id) (gethash "tool" (wire-row call-id rows "tool" "call_id"))))
        (is-shape (tool "c-sh")
          ("shown" (format nil "a~%b") "the output, no note to the model and no header")
          ("commands" equalp #("ls -la") "the command a shell prints")
          ("status" "complete"))
        (is-shape (tool "c-code") ("title" "Evaluated") ("code" "(+ 1 2)" "the code beside the words")
          ("shown" "3"))
        (is-shape (tool "c-handoff")
          ("status" "background" "a job handed off is in the background, not done")
          ("shown" null "its box has the output; the handoff's words were the model's"))
        (is (search "Running" (gethash "title" (tool "c-handoff"))) "present tense while the job runs")
        (is-shape (tool "c-stop")
          ("status" "stopped" "a stop is its own end")
          ("shown" null "the compiler's report of the abort is the model's"))
        (let ((row (wire-row "c-stop" rows "tool" "call_id")))
          (is (null (nlk:json-value row :boolean "error")) "and no failure"))
        (is-shape (tool "c-read") ("title" "Checked the background job's result")
          ("shown" (format nil "tick 1~%tick 2") "what the job wrote, not the plist"))))))

(deftest an-exec-is-titled-by-the-command-its-one-sh-runs ()
  ;; The box read `(sh "for i in $(seq 1 24); do ...' where the operator
  ;; looks for the command itself; a snippet that does anything more keeps its
  ;; form, and one still streaming reads as far as it has come.
  (is-table (title snippet) (equal title (nle::exec-title snippet))
    ("for i in $(seq 1 24); do echo $i; done" "(sh \"for i in $(seq 1 24); do echo $i; done\")")
    ("make -C src" (format nil ";; build it~%(sh \"make -C src\" :directory \"/tmp\")") "past its note")
    ("echo \"hi\"" "(sh \"echo \\\"hi\\\"\")" "its escapes read")
    ("sleep 1" "(sh \"sleep 1" "streaming")
    ("(progn (sh \"a\") (sh \"b\"))" "(progn (sh \"a\") (sh \"b\"))" "two commands")
    ("(length (sh \"ls\"))" "(length (sh \"ls\"))" "a form around it")
    ("just build" "just build" "a shell tool's own command")))

(deftest a-rows-exec-sends-every-kept-line-and-says-when-more-went ()
  ;; The box showed the last 8 lines and no more; it is sent every line the
  ;; band keeps, and CUT when the command wrote more before them, so a page
  ;; can open to all of it and say what it no longer has -- less the first
  ;; kept line then, which a tail kept by bytes begins partway into.
  (let* ((lines (loop for n from 1 to 12 collect (format nil "line ~d" n)))
         (row (output-band-row (format nil "~{~a~%~}" lines) :shell-session-id 9
                               :command "(sh \"yes | head\")" :output-seen-lines 40)))
    (is-shape (nle::exec-json row) ("tail" (format nil "~{~a~^~%~}" (rest lines)) "every whole line kept")
      ("cut" t "forty were seen") ("command" "yes | head")))
  ;; A tail holding everything it was sent is not cut.
  (is (null (gethash "cut" (nle::exec-json (output-band-row (format nil "a~%") :shell-session-id 9
                                                            :command "ls" :output-seen-lines 1))))))

(deftest a-rows-verb-reads-a-retrying-provider-until-the-stream-moves (with-temp-store (store))
  ;; A provider being retried read `running' on the page for the whole
  ;; backoff, where a shell's live bar says which try it is on and why
  ;; (INGEST-STREAM-RESET keeps that as retry_status). The verb the rows carry
  ;; is that bar's head: the countdown ahead of the activity, until the next
  ;; delta of the stream retires it.
  (let ((view (nle::make-rows-view :client nil :session-id "rows-g")))
    (setf (nle::rows-view-active-p view) t)
    (flet ((delta (type &rest fields)
             (nle::rows-view-receive
              view (nlk:encode-projection-event
                    "rows-g" 0 "item_delta"
                    (nlk:json-object "turn_id" "t1"
                                     "delta" (apply #'nlk::make-json-object "type" type fields)))))
           (verb ()
             (nlk:json-value (nle::rows-view-payload view) :string "turn" "verb")))
      (delta "text" "text" "The order desk")
      (is (equal "streaming" (verb)))
      (delta "stream_reset" "retry" (nlk:json-object "attempt" 2 "limit" 5 "delay_seconds" 8.0
                                                     "status" 429 "provider" "openai"
                                                     "reason" "Rate limit reached. Try again later."))
      ;; The rows go out when something changes, never on a clock: a countdown
      ;; would stand at its first second.
      ;; Which try, and what the provider said, whole.
      (is (equal "retry 2 of 5 · openai 429: Rate limit reached. Try again later." (verb)))
      (delta "stream_reset" "retry" (nlk:json-object "attempt" 3 "limit" 5 "delay_seconds" 0.0
                                                     "continuation" t))
      (is (equal "continue 3 of 5" (verb)) "a retry that carries the cut output back says so")
      (delta "text" "text" " has three parts")
      (is (equal "streaming" (verb)) "the stream moving retires the countdown"))))

(deftest a-rows-cut-off-attempt-carries-its-sentence-on-the-row-that-closes-it (with-temp-gateway (port))
  ;; The page draws what a shell says of an attempt the stream cut off, under
  ;; the attempt. The sentence is made once, in the fold, and rides the row that
  ;; closes the attempt -- once per attempt, the rows above it only voided --
  ;; and the replay a page earns on opening the session sends it as the live
  ;; fold did, so a reload shows what the live page showed.
  (ensure-durable-session "rows-h")
  (with-durable-turn (turn "rows-h" :input "tell me" :complete t)
    (nlk::record-attempt-voided turn :attempt 1 :reasoning "a thought" :text "half an answer"
                                     :detail "the stream ended before the answer finished"
                                     :stream-duration-ms 2100 :disposition "rerun")
    (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "the answer")))
  (multiple-value-bind (held order)
      (rows-client (frames-while (ws port) (send-command ws (rows-subscribe "rows-h"))))
    (let* ((rows (mapcar (lambda (id) (gethash id held)) order))
           (sentence "Attempt 1 cut off after 2.1s: the stream ended before the answer finished. Trying again.")
           (said (count sentence rows :key (lambda (row) (nlk:json-value row :string "voided")) :test #'equal)))
      (is (equal '("user" "assistant" "assistant" "assistant" "system")
                 (mapcar (lambda (row) (gethash "role" row)) rows)))
      (is-shape (second rows) ((:string "kind") "reasoning") ((:boolean "voided") t "voided, nothing said of it"))
      (is-shape (third rows) ((:string "text") "half an answer") ((:string "voided") sentence "the sentence closes it"))
      (is (null (nlk:json-value (fourth rows) :any "voided")) "the answer that stood is no attempt")
      (is (= 1 said) "said once for the attempt"))))

(deftest a-rows-fork-standing-by-is-sent-the-turns-it-carries (with-temp-gateway (port))
  ;; A fork is minted standing by and has no fact of its own until its first
  ;; prompt, so its replay was empty: the page opened it blank and showed the
  ;; turns it carries only after a reload. It is sent its ancestors' rows up to
  ;; the turn it forks at, as the replay after that prompt sends them; a session
  ;; standing by with no fork behind it is sent none.
  (seed-completed-turn "rows-p" "first" "one")
  (let ((anchor (nlk:session-head-turn-id "rows-p")))
    (seed-completed-turn "rows-p" "second" "two")
    (nlk:standby-session :id "rows-old" :parent "rows-p" :anchor-turn-id anchor)
    (nlk:standby-session :id "rows-new" :parent "rows-p")
    (nlk:standby-session :id "rows-deep" :parent "rows-new" :anchor-turn-id anchor)
    (nlk:standby-session :id "rows-plain")
    (let ((frames (frames-while (ws port)
                    (send-command ws (rows-subscribe "rows-old" "rows-new" "rows-deep" "rows-plain")))))
      (flet ((carried (id)
               (rows-texts (remove-if-not (lambda (frame)
                                            (equal id (nlk:json-value frame :string "payload" "session_id")))
                                          frames))))
        (is (subsetp '("first" "one") (carried "rows-old") :test #'equal) "a fork carries the turn it forks at")
        (is (null (intersection '("second" "two") (carried "rows-old") :test #'equal)) "and none after it")
        (is (subsetp '("first" "one" "second" "two") (carried "rows-new") :test #'equal) "one forked at the head carries all")
        (is (subsetp '("first" "one") (carried "rows-deep") :test #'equal) "a fork of a fork, the turn it forks at")
        (is (null (intersection '("second" "two") (carried "rows-deep") :test #'equal)) "and none after it")
        (is (null (carried "rows-plain")) "a session with no fork behind it, none")))))
