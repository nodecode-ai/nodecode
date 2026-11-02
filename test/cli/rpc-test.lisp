;;;; rpc-test.lisp --- a program's shell (cli/rpc.lisp): its commands in,
;;;; the gateway's frames out.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun rpc-command (line &optional (session-id "s-rpc"))
  "LINE as the command RPC-COMMAND-TEXT sends for SESSION-ID: the
client_command frame's inner command object."
  (gethash "command" (nlk:decode-json (nle::rpc-command-text line session-id))))

(deftest rpc-command-text-fills-what-a-shell-fills ()
  ;; A line names what it means; the shell adds what a shell's own senders
  ;; add (SEND-GATEWAY-COMMAND) and nothing else, and the codec is the
  ;; gateway's both ways.
  (let ((start (rpc-command "{\"type\":\"start_turn\",\"request\":{\"prompt\":\"hi\"}}")))
    (is-shape start ("type" "start_turn"))
    (is (plusp (length (gethash "command_id" start))) "a command_id is minted")
    ;; The correlation is the command's own.
    (is (equal (gethash "command_id" start) (gethash "correlation_id" start)))
    (is (equal "s-rpc" (nlk:json-value start :string "request" "session_id")) "the shell's session")
    ;; The prompt rides as the wire's content.
    (is (equal "hi" (gethash "text" (aref (nlk:json-value start :array "request" "content") 0)))))
  (let ((named (rpc-command "{\"type\":\"start_turn\",\"command_id\":\"c1\",\"request\":{\"prompt\":\"hi\",\"session_id\":\"s-other\"}}")))
    ;; What a line names stands.
    (is (equal '("c1" "c1" "s-other")
               (list (gethash "command_id" named) (gethash "correlation_id" named)
                     (nlk:json-value named :string "request" "session_id")))))
  ;; A flat command takes the session beside its own fields.
  (is (equal '("s-rpc" "t1")
             (let ((cancel (rpc-command "{\"type\":\"cancel_turn\",\"turn_id\":\"t1\"}")))
               (list (gethash "session_id" cancel) (gethash "turn_id" cancel)))))
  (let ((subscribe (rpc-command "{\"type\":\"set_session_subscriptions\",\"subscriptions\":[{\"session_id\":\"a\"}]}")))
    (is (null (gethash "command_id" subscribe)) "a subscription carries no identity")
    (is (null (gethash "session_id" subscribe)) "and names its sessions itself"))
  ;; A line the gateway would refuse never leaves, refused in its words.
  (flet ((code (line)
           (let ((refusal (signals-error nlk:protocol-error (nle::rpc-command-text line "s-rpc"))))
             (and refusal (nlk:protocol-error-code refusal)))))
    (is (eq :invalid-json (code "not json")))
    (is (eq :invalid-envelope (code "[1]")) "an array is not a command")
    (is (eq :unsupported-command (code "{\"type\":\"bogus\"}")))
    (is (eq :invalid-envelope (code "{\"type\":\"start_turn\"}")) "start_turn needs its request")))

(defun rpc-pipe ()
  "A pipe as (values READ-STREAM WRITE-STREAM), UTF-8 both ends."
  (multiple-value-bind (in out) (sb-posix:pipe)
    (values (sb-sys:make-fd-stream in :input t :external-format :utf-8 :buffering :full)
            (sb-sys:make-fd-stream out :output t :external-format :utf-8 :buffering :full))))

(defmacro with-rpc ((port session-id send frames outcome &key cursor) &body body)
  "Run RUN-RPC on PORT's gateway for SESSION-ID, resumed past CURSOR when
given, over two pipes, with SEND
writing one line to its stdin, FRAMES answering every line it wrote to its
stdout so far, parsed, and OUTCOME answering what it returned once it has.
BODY's end closes its stdin."
  (alexandria:with-gensyms (thread result lines stdin feed stdout drain reader)
    `(multiple-value-bind (,stdin ,feed) (rpc-pipe)
       (multiple-value-bind (,drain ,stdout) (rpc-pipe)
         (let* ((,result (list :running))
                (,lines (sb-concurrency:make-queue))
                (,reader (nlk:spawn "rpc-test-reader"
                           (loop for line = (read-line ,drain nil nil)
                                 while line do (sb-concurrency:enqueue line ,lines))))
                (,thread (nlk:spawn "rpc-test"
                           (setf (car ,result)
                                 (nlk:with-cleanup ((ignore-errors (close ,stdout)))
                                   (nle::run-rpc :port ,port :token "test-token"
                                                 :session-id ,session-id :cursor ,cursor
                                                 :input ,stdin :output ,stdout))))))
           (flet ((,send (line) (write-line line ,feed) (finish-output ,feed))
                  (,frames () (mapcar #'nlk:decode-json (sb-concurrency:list-queue-contents ,lines)))
                  (,outcome () (car ,result)))
             (declare (ignorable #',send #',frames #',outcome))
             (nlk:with-cleanup ((ignore-errors (close ,feed))
                            (ignore-errors (bt2:join-thread ,thread))
                            (ignore-errors (bt2:join-thread ,reader))
                            (ignore-errors (close ,stdin))
                            (ignore-errors (close ,drain)))
               ,@body)))))))

(defun rpc-completed (frames)
  "How many turn.completed facts FRAMES streamed as they landed."
  (count "turn.completed" (frames-of-type frames "session_stream_event") :test #'equal
         :key (lambda (frame) (nlk:json-value frame :string "payload" "frame" "event" "type"))))

(defun rpc-result (frames command-id)
  "The gateway_turn_command_result among FRAMES answering COMMAND-ID."
  (frame-carrying (frames-of-type frames "gateway_turn_command_result") command-id
                  "result" "command_id"))

(deftest rpc-carries-a-programs-lines-to-the-gateway-and-its-frames-back ()
  ;; The whole shell over a real gateway: a program's start_turn reaches a
  ;; turn, the turn's facts come back one frame a line, a line that is not a
  ;; command is answered and the next one still read, and stdin's end is
  ;; the shell's.
  (with-gateway-session (port "rpc-s" :cwd (namestring (fixture-workspace)))
    (with-stub-provider ((wire-message "assistant" :content "pong"))
      (with-rpc (port "rpc-s" send frames outcome)
        ;; Attached: the session's replay comes first.
        (is (await (:timeout 10) (frames-of-type (frames) "session_stream_resumed")))
        (send "not json")
        (send "{\"type\":\"start_turn\",\"command_id\":\"rpc-1\",\"request\":{\"prompt\":\"ping\"}}")
        ;; The turn the line started runs to its end.
        (is (await (:timeout 10) (plusp (rpc-completed (frames)))))
        (let* ((frames (frames))
               (result (rpc-result frames "rpc-1"))
               (refusal (first (frames-of-type frames "sync_protocol_error"))))
          ;; Every line is a sync frame: the handshake stays the shell's.
          (is (every #'frame-type frames))
          (is (equal "session_stream_resumed" (frame-type (first frames))))
          (is (equal "rpc-s" (nlk:json-value (first frames) :string "session_id")))
          (is-present result "the command's result, by the id the line gave"
            (is (equal "accepted" (nlk:json-value result :string "payload" "result" "status"))))
          (is-present refusal "the line that was not a command is refused"
            (is (equal "invalid-json" (nlk:json-value refusal :string "payload" "code"))))
          ;; The answer is among the facts.
          (is (frame-carrying (frames-of-type frames "session_stream_event")
                              "pong" "frame" "event" "payload" "message" "content")))))
    (let (seen)
      (with-rpc (port "rpc-s" send frames outcome)
        (await (:timeout 10) (frames))
        ;; The gateway's end is the shell's, as a reason and not as :EOF: its
        ;; side of the socket closes, as an organism's exit closes it. Not
        ;; WSD:CLOSE-CONNECTION: under Hunchentoot that sends a close frame
        ;; and only flushes the socket, which stays open in this image.
        (dolist (peer (sb-ext:with-locked-hash-table (nle::*gateway-peer-states*)
                        (alexandria:hash-table-keys nle::*gateway-peer-states*)))
          (ignore-errors (close (clack.socket:socket-stream (websocket-driver.ws.base:socket peer))
                                :abort t)))
        (setf seen (await (:timeout 10) (and (stringp (outcome)) (outcome)))))
      ;; The gateway's end ends the shell, with why.
      (is (stringp seen)))))

(deftest rpc-resumes-past-the-cursor-a-program-read ()
  ;; A program that lost its rpc names the newest cursor it read and is sent
  ;; only what landed past it, onto what it folded. Where that would not hold
  ;; -- the head moved since, a cursor past the log -- the whole replay comes,
  ;; and says so by saying no resumed_after.
  (with-gateway-session (port "rpc-r" :cwd (namestring (fixture-workspace)))
    (with-stub-provider ((wire-message "assistant" :content "one")
                         (wire-message "assistant" :content "two"))
      (let (held)
        (with-rpc (port "rpc-r" send frames outcome)
          (send "{\"type\":\"start_turn\",\"request\":{\"prompt\":\"first\"}}")
          (is (await (:timeout 10) (= 1 (rpc-completed (frames)))))
          ;; The newest log position the frames carried: what a program resumes past.
          (setf held (loop for frame in (frames)
                           maximize (or (nlk:json-value frame :integer "cursor") 0))))
        ;; A turn lands while the program is away.
        (with-rpc (port "rpc-r" send frames outcome)
          (send "{\"type\":\"start_turn\",\"request\":{\"prompt\":\"second\"}}")
          (is (await (:timeout 10) (= 1 (rpc-completed (frames))))))
        (with-rpc (port "rpc-r" send frames outcome :cursor held)
          (send "{\"type\":\"get_state\",\"command_id\":\"st-1\"}")
          (send "{\"type\":\"get_state\",\"command_id\":\"st-2\",\"session_id\":\"s-none\"}")
          (is (await (:timeout 10) (rpc-result (frames) "st-2")))
          (let* ((frames (frames))
                 (resumed (first frames))
                 (facts (coerce (nlk:json-array resumed "payload" "accumulated_frames") 'list))
                 (state (nlk:json-value (rpc-result frames "st-1") :object "payload" "result" "response")))
            (is (equal "session_stream_resumed" (frame-type resumed)))
            (is (eql held (nlk:json-value resumed :integer "payload" "resumed_after")))
            ;; Past the cursor alone: the second turn's answer, none of the first's.
            (is (every (lambda (fact) (> (gethash "seq" fact) held)) facts))
            (is (wire-row "two" facts "event" "payload" "message" "content"))
            ;; get_state: the session as it stands, its cursor the replay's own.
            (is-present state "get_state answers in its result"
              (is (equal "ready" (gethash "admission_state" state)))
              (is (eql (nlk:json-value resumed :integer "cursor") (gethash "cursor" state)))
              (is (equal "rpc-r" (nlk:json-value state :string "session" "session_id")))
              (is (null (gethash "turn_id" state)) "no turn runs"))
            (is (equal "unknown_session"
                       (nlk:json-value (rpc-result frames "st-2") :string "payload" "result" "code")))))
        (flet ((whole-p (cursor)
                 ;; The first frame of a shell resumed past CURSOR is a whole replay.
                 (with-rpc (port "rpc-r" send frames outcome :cursor cursor)
                   (and (await (:timeout 10) (frames))
                        (equal "session_stream_resumed" (frame-type (first (frames))))
                        (null (nlk:json-value (first (frames)) :integer "payload" "resumed_after"))))))
          (is (whole-p (+ held 100000)) "a cursor past the log")
          (nlk::move-session-head "rpc-r" nil)
          (is (whole-p held) "the head moved since"))))))

(deftest rpc-ends-on-stdin-and-names-the-outcome ()
  ;; Closing stdin is the program letting go: :EOF, whatever ran before.
  (with-gateway-session (port "rpc-eof" :cwd (namestring (fixture-workspace)))
    (is (eq :eof (funcall (with-rpc (port "rpc-eof" send frames outcome)
                            (await (:timeout 10) (frames))
                            #'outcome))))))

(deftest launch-rpc-verb-takes-a-session-and-a-store ()
  ;; `nodecode rpc [SESSION-ID]' is a command of its own that names its store
  ;; the way a shell does; it reads no terminal, and --help lists it.
  (is (equal '(nil (:rpc nil nil) nil nil nil nil nil) (launch-plan "rpc")))
  (is (equal '(nil (:rpc "s-1" nil) nil "/tmp/x.db" nil nil nil)
             (launch-plan "rpc" "s-1" "--store" "/tmp/x.db")))
  (is (equal '(nil (:rpc nil nil) nil nil t nil "coder") (launch-plan "-p" "coder" "rpc" "--ephemeral")))
  ;; --cursor N resumes the session named, past N.
  (is (equal '(nil (:rpc "s-1" 42) nil "/tmp/x.db" nil nil nil)
             (launch-plan "rpc" "s-1" "--cursor" "42" "--store" "/tmp/x.db")))
  (is (search "--cursor resumes a session" (launch-refusal "rpc" "--cursor" "4")))
  (is (search "--cursor takes a log position" (launch-refusal "rpc" "s-1" "--cursor" "x")))
  (is (search "rpc is a command of its own" (launch-refusal "rpc" "serve")))
  (is (search "rpc is a command of its own" (launch-refusal "resume" "s-1" "rpc")))
  ;; A program has no terminal to give, and --help lists the verb.
  (is (null (nle::terminal-refusal '(:rpc nil) nil '("rpc"))))
  (is (search "rpc [SESSION-ID]" (nle::launch-help-text)))
  (is (search "usage: nodecode rpc [SESSION-ID [--cursor N]]" (nle::launch-help-text "rpc"))))
