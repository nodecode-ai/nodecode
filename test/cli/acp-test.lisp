;;;; acp-test.lisp --- an ACP client's agent (cli/acp.lisp): its JSON-RPC in,
;;;; the gateway's sessions out as session/update.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defmacro with-acp ((port send messages outcome) &body body)
  "Run RUN-ACP on PORT's gateway over two pipes, with SEND writing one
message (a JSON object) to its stdin, MESSAGES answering every line it wrote
to its stdout so far, parsed, and OUTCOME answering what it returned once it
has. BODY's end closes its stdin."
  (alexandria:with-gensyms (thread result lines stdin feed stdout drain reader)
    `(multiple-value-bind (,stdin ,feed) (rpc-pipe)
       (multiple-value-bind (,drain ,stdout) (rpc-pipe)
         (let* ((,result (list :running))
                (,lines (sb-concurrency:make-queue))
                (,reader (nlk:spawn "acp-test-reader"
                           (loop for line = (read-line ,drain nil nil)
                                 while line do (sb-concurrency:enqueue line ,lines))))
                (,thread (nlk:spawn "acp-test"
                           (setf (car ,result)
                                 (nlk:with-cleanup ((ignore-errors (close ,stdout)))
                                   (nle::run-acp :port ,port :token "test-token"
                                                 :input ,stdin :output ,stdout))))))
           (flet ((,send (object)
                    (write-line (nlk:encode-json-object object) ,feed)
                    (finish-output ,feed))
                  (,messages () (mapcar #'nlk:decode-json (sb-concurrency:list-queue-contents ,lines)))
                  (,outcome () (car ,result)))
             (declare (ignorable #',send #',messages #',outcome))
             (nlk:with-cleanup ((ignore-errors (close ,feed))
                                (ignore-errors (bt2:join-thread ,thread))
                                (ignore-errors (bt2:join-thread ,reader))
                                (ignore-errors (close ,stdin))
                                (ignore-errors (close ,drain)))
               ,@body)))))))

(defun acp-call (id method params)
  "The JSON-RPC request ID calling METHOD with PARAMS."
  (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params))

(defun acp-answer (messages id)
  "The answer among MESSAGES to the request ID."
  (find-if (lambda (message) (and (equal id (gethash "id" message))
                                  (null (gethash "method" message))))
           messages))

(defun acp-updates (messages &optional kind)
  "The session/update payloads among MESSAGES, those of KIND when given."
  (loop for message in messages
        for update = (and (equal "session/update" (gethash "method" message))
                          (nlk:json-value message :object "params" "update"))
        when (and update (or (null kind) (equal kind (gethash "sessionUpdate" update))))
          collect update))

(defun acp-chunks-text (messages kind)
  "The text every chunk of KIND among MESSAGES carried, in order."
  (format nil "~{~a~}" (mapcar (lambda (update) (nlk:json-value update :string "content" "text"))
                               (acp-updates messages kind))))

(defun acp-open-session (send messages)
  "Initialize and open a session in the fixture folder over SEND; => its id."
  (funcall send (acp-call 1 "initialize" (nlk:json-object "protocolVersion" 1)))
  (funcall send (acp-call 2 "session/new" (nlk:json-object "cwd" (namestring (fixture-workspace))
                                                           "mcpServers" #())))
  (await (:timeout 10) (nlk:json-value (acp-answer (funcall messages) 2) :string "result" "sessionId")))

(deftest acp-initialize-says-what-the-agent-does ()
  (with-gateway-session (port "acp-init" :cwd (namestring (fixture-workspace)))
    (with-acp (port send messages outcome)
      (send (acp-call 1 "initialize" (nlk:json-object "protocolVersion" 1)))
      (let ((answer (await (:timeout 10) (acp-answer (messages) 1))))
        (is-present answer "initialize is answered"
          (is (eql 1 (nlk:json-value answer :integer "result" "protocolVersion")))
          (is (eq t (nlk:json-value answer :boolean "result" "agentCapabilities" "loadSession")))
          (is (eq t (nlk:json-value answer :boolean "result" "agentCapabilities" "promptCapabilities" "image")))
          (is (nlk:json-value answer :object "result" "agentCapabilities" "sessionCapabilities" "list"))
          (is (equal "nodecode" (nlk:json-value answer :string "result" "agentInfo" "name"))))))))

(deftest acp-a-prompt-is-a-turn-answered-with-its-stop-reason ()
  ;; A session/new is a gateway session in the client's folder, and a
  ;; session/prompt the turn that runs there: its words come back as message
  ;; chunks before the prompt is answered end_turn, and the client's own
  ;; prompt is never sent back to it.
  (with-temp-gateway (port)
    (with-stub-provider ((wire-message "assistant" :content "pong"))
      (with-acp (port send messages outcome)
        (let ((id (acp-open-session #'send #'messages)))
          (is (stringp id) "session/new names the session")
          (is (nlk:session-exists-p id) "a gateway session the shell can open too")
          (send (acp-call 3 "session/prompt"
                          (nlk:json-object "sessionId" id
                                           "prompt" (vector (nlk:json-object "type" "text" "text" "ping")))))
          (let ((answer (await (:timeout 15) (acp-answer (messages) 3))))
            (is-present answer "the prompt is answered once its turn ends"
              (is (equal "end_turn" (nlk:json-value answer :string "result" "stopReason")))))
          (let* ((all (messages))
                 (answered (position 3 all :key (lambda (m) (and (null (gethash "method" m)) (gethash "id" m))))))
            (is (equal "pong" (acp-chunks-text all "agent_message_chunk")))
            (is (null (acp-updates all "user_message_chunk")))
            (is (every (lambda (update) (< (position update all :key (lambda (m) (nlk:json-value m :object "params" "update")))
                                         answered))
                       (acp-updates all "agent_message_chunk"))
                "every chunk comes before the answer")))))))

(deftest acp-a-tool-call-opens-and-closes-in-the-client ()
  (with-temp-gateway (port)
    (with-stub-provider ((wire-message "assistant"
                                       :tool-calls (vector (wire-tool-call "call-1" "eval"
                                                                           "{\"form\":\";; add them\\n(+ 1 2)\"}")))
                         (wire-message "assistant" :content "three"))
      (with-acp (port send messages outcome)
        (let ((id (acp-open-session #'send #'messages)))
          (send (acp-call 3 "session/prompt"
                          (nlk:json-object "sessionId" id
                                           "prompt" (vector (nlk:json-object "type" "text" "text" "add")))))
          (is (await (:timeout 15) (acp-answer (messages) 3)))
          (let ((call (first (acp-updates (messages) "tool_call")))
                (done (first (acp-updates (messages) "tool_call_update"))))
            (is-present call "the call opens as it starts"
              (is (equal "call-1" (gethash "toolCallId" call)))
              (is (equal "add them" (gethash "title" call)) "titled by its note")
              (is (equal "in_progress" (gethash "status" call))))
            (is-present done "and closes with its result"
              (is (equal "completed" (gethash "status" done)))
              (is (search "3" (nlk:json-value (aref (gethash "content" done) 0) :string "content" "text")))))
          (is (equal "three" (acp-chunks-text (messages) "agent_message_chunk"))))))))

(deftest acp-load-sends-the-conversation-again-before-it-answers ()
  (with-temp-gateway (port)
    (let (id)
      (with-stub-provider ((wire-message "assistant" :content "pong"))
        (with-acp (port send messages outcome)
          (setf id (acp-open-session #'send #'messages))
          (send (acp-call 3 "session/prompt"
                          (nlk:json-object "sessionId" id
                                           "prompt" (vector (nlk:json-object "type" "text" "text" "ping")))))
          (await (:timeout 15) (acp-answer (messages) 3))))
      (with-acp (port send messages outcome)
        (send (acp-call 1 "initialize" (nlk:json-object "protocolVersion" 1)))
        (send (acp-call 2 "session/load" (nlk:json-object "sessionId" id
                                                          "cwd" (namestring (fixture-workspace))
                                                          "mcpServers" #())))
        (let ((answer (await (:timeout 10) (acp-answer (messages) 2))))
          (is-present answer "session/load is answered"
            (is (vectorp (nlk:json-value answer :any "result" "configOptions"))))
          (let* ((all (messages))
                 (answered (position answer all)))
            (is (equal "ping" (acp-chunks-text (subseq all 0 answered) "user_message_chunk")))
            (is (equal "pong" (acp-chunks-text (subseq all 0 answered) "agent_message_chunk"))
                "the history comes before the answer")))
        (send (acp-call 3 "session/list" (nlk:json-object "cwd" (namestring (fixture-workspace)))))
        (let ((listed (await (:timeout 10) (acp-answer (messages) 3))))
          (is (find id (nlk:json-value listed :array "result" "sessions")
                    :key (lambda (row) (gethash "sessionId" row)) :test #'equal)
              "session/list lists it under its folder"))
        (send (acp-call 4 "session/load" (nlk:json-object "sessionId" "s-none" "cwd" "/" "mcpServers" #())))
        (is (eql -32002 (nlk:json-value (await (:timeout 10) (acp-answer (messages) 4)) :integer "error" "code"))
            "an unknown session is not found")))))

(deftest acp-a-question-is-the-clients-permission-prompt ()
  ;; What the organism asks whoever watches (the guard's ask) reaches the
  ;; client as session/request_permission, and the option it picks is the
  ;; answer.
  (with-temp-gateway (port)
    (with-acp (port send messages outcome)
      (let* ((id (acp-open-session #'send #'messages))
             (asked (list nil))
             (asker (nlk:spawn "acp-test-ask"
                      (setf (car asked)
                            (multiple-value-list
                             (nle::ask-operator "Run rm -rf build?" :choices '("Allow" "Deny")
                                                                     :session-id id :seconds 10))))))
        (let ((request (await (:timeout 10)
                         (find "session/request_permission" (messages)
                               :key (lambda (m) (gethash "method" m)) :test #'equal))))
          (is-present request "the question is asked as a permission prompt"
            (is (equal id (nlk:json-value request :string "params" "sessionId")))
            (is (equal "Run rm -rf build?" (nlk:json-value request :string "params" "toolCall" "title")))
            (is (equal '("allow_once" "reject_once")
                       (map 'list (lambda (option) (gethash "kind" option))
                            (nlk:json-value request :array "params" "options"))))
            (send (nlk:json-object "jsonrpc" "2.0" "id" (gethash "id" request)
                                   "result" (nlk:json-object "outcome" (nlk:json-object "outcome" "selected"
                                                                                        "optionId" "Allow"))))))
        (bt2:join-thread asker)
        (is (equal '("Allow" :answered) (car asked)) "the option picked is the answer")))))

(deftest acp-refuses-what-it-cannot-do-in-json-rpc-terms ()
  (with-temp-gateway (port)
    (with-acp (port send messages outcome)
      (send (acp-call 1 "session/bogus" (nlk:json-object)))
      (send (acp-call 2 "session/prompt" (nlk:json-object "sessionId" "s-none" "prompt" #())))
      (send (acp-call 3 "session/new" (nlk:json-object "cwd" "relative/path" "mcpServers" #())))
      (is (await (:timeout 10) (acp-answer (messages) 3)))
      (flet ((code (id) (nlk:json-value (acp-answer (messages) id) :integer "error" "code")))
        (is (eql -32601 (code 1)) "an unknown method")
        (is (eql -32002 (code 2)) "a session never opened")
        (is (eql -32602 (code 3)) "a folder that is not absolute"))))
  (is (equal "allow_always" (nle::acp-choice-kind "Allow always")))
  (is (equal "reject_once" (nle::acp-choice-kind "Deny"))))
