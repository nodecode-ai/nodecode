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
                 (answered (position 3 all :key (lambda (m) (and (null (gethash "method" m)) (gethash "id" m)))))
                 (chunks-after (remove-if (lambda (message) (< (position message all) answered))
                                          (remove-if-not (lambda (m) (equal "session/update" (gethash "method" m))) all))))
            (is (equal "pong" (acp-chunks-text all "agent_message_chunk")))
            (is (null (acp-updates all "user_message_chunk")))
            (is (null (acp-updates chunks-after "agent_message_chunk")) "every chunk comes before the answer")))))))

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
            (let ((before (subseq all 0 answered)))
              (is (equal "pong" (acp-chunks-text before "agent_message_chunk")) "the history comes before the answer"))))
        (send (acp-call 3 "session/list" (nlk:json-object "cwd" (namestring (fixture-workspace)))))
        (let ((listed (await (:timeout 10) (acp-answer (messages) 3))))
          (let ((ids (map 'list (lambda (row) (gethash "sessionId" row)) (nlk:json-value listed :array "result" "sessions"))))
            (is (member id ids :test #'equal) "session/list lists it under its folder")))
        (send (acp-call 4 "session/load" (nlk:json-object "sessionId" "s-none" "cwd" "/" "mcpServers" #())))
        (let ((code (nlk:json-value (await (:timeout 10) (acp-answer (messages) 4)) :integer "error" "code")))
          (is (eql -32002 code) "an unknown session is not found"))))))

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

(deftest acp-a-name-given-elsewhere-reaches-the-client ()
  ;; A session's name is no fact of its stream: the organism says it to every
  ;; shell, and the client is told it as a session_info_update.
  (with-gateway-session (port "acp-named" :cwd (namestring (fixture-workspace)))
    (with-acp (port send messages outcome)
      (send (acp-call 1 "session/resume" (nlk:json-object "sessionId" "acp-named"
                                                          "cwd" (namestring (fixture-workspace))
                                                          "mcpServers" #())))
      (is (await (:timeout 10) (acp-answer (messages) 1)))
      (nle::set-session-title "acp-named" "Renamed elsewhere")
      (let ((update (await (:timeout 10) (first (acp-updates (messages) "session_info_update")))))
        (is-present update "the name reaches the client"
          (is (equal "Renamed elsewhere" (gethash "title" update))))))))

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

(deftest acp-a-workspace-diff-is-the-clients-diff ()
  ;; A changed file's git patch reaches the client as ACP diff content, a
  ;; block a hunk with the text either side of it, the path absolute; a new
  ;; file's old side is null, a deleted one's new side empty.
  (let* ((session (nle::make-acp-session :id "s" :cwd "/work/repo"))
         (metadata (nlk:json-object
                    "workspace_diff"
                    (nlk:json-object
                     "files" (vector (nlk:json-object
                                      "path" "src/a.txt" "added" 1 "removed" 1
                                      "patch" (format nil "diff --git a/src/a.txt b/src/a.txt~%--- a/src/a.txt~%+++ b/src/a.txt~%@@ -3,3 +3,3 @@ head~% one~%-two~%+TWO~% three~%@@ -20,2 +20,3 @@~% x~%+y~% z~%"))
                                     (nlk:json-object
                                      "path" "new.txt" "added" 2 "removed" 0
                                      "patch" (format nil "diff --git a/new.txt b/new.txt~%new file mode 100644~%--- /dev/null~%+++ b/new.txt~%@@ -0,0 +1,2 @@~%+hello~%+world~%"))
                                     (nlk:json-object
                                      "path" "gone.txt" "added" 0 "removed" 1
                                      "patch" (format nil "diff --git a/gone.txt b/gone.txt~%deleted file mode 100644~%--- a/gone.txt~%+++ /dev/null~%@@ -1 +0,0 @@~%-bye~%"))
                                     (nlk:json-object "path" "pic.png" "added" 0 "removed" 0 "binary" t)))))
         (files (nle::acp-diff-files session metadata))
         (blocks (nle::acp-diff-content files)))
    (let ((paths (mapcar #'first files))
          (texts '("/work/repo/src/a.txt" "/work/repo/new.txt" "/work/repo/gone.txt")))
      (is (equal texts paths) "every text file, absolute; a binary one has no text to show"))
    (is (eql 4 (length blocks)) "a block a hunk")
    (is-shape (first blocks)
      ("type" "diff") ("path" "/work/repo/src/a.txt")
      ("oldText" (format nil "one~%two~%three~%")) ("newText" (format nil "one~%TWO~%three~%")))
    (is-shape (second blocks) ("oldText" (format nil "x~%z~%")) ("newText" (format nil "x~%y~%z~%")))
    (is (eq :null (gethash "oldText" (third blocks))) "a new file has no old side")
    (is (equal (format nil "hello~%world~%") (gethash "newText" (third blocks))))
    (is (equal "" (gethash "newText" (fourth blocks))) "a deleted file has an empty new side")
    (let ((locations (nle::acp-locations session nil files)))
      (is (equal "/work/repo/src/a.txt" (gethash "path" (aref locations 0))))
      (is (eql 3 (gethash "line" (aref locations 0))) "at its first hunk's line"))
    (is (eql 2 (nle::acp-exit-code (format nil "built~%[exit 0]~%tests~%[exit 2]"))) "the last command's exit")
    (is (null (nle::acp-exit-code "no command ran")))))

(deftest acp-a-call-reads-as-the-transcript-tells-it ()
  ;; A call opens titled and kinded by what its snippet does, and closes said
  ;; again from what it did: its receipts, its diff, its command.
  (let ((session (nle::make-acp-session :id "s" :cwd "/work/repo")))
    (flet ((open-call (id form)
             (nle::acp-tool-call session (nlk:json-object "call-id" id "tool-name" "eval"
                                                          "arguments" (nlk:encode-json-object
                                                                       (nlk:json-object "form" form)))))
           (close-call (id result &optional metadata)
             (nle::acp-tool-result session (nlk:json-object "call-id" id "tool-name" "eval"
                                                            "result" result :opt "metadata" metadata))))
      (let ((call (open-call "c1" "(sh \"just lint\")")))
        (is-shape call ("kind" "execute") ("status" "in_progress")
          ((:string "rawInput" "command") "just lint" "the command rides where a client reads one")))
      (let ((done (close-call "c1" (format nil "ok~%[exit 0]")
                              (nlk:json-object "calls" (vector (nlk:json-object "verb" "sh" "source" "just lint"))))))
        (is-shape done ("status" "completed") ("kind" "execute")
          ((:integer "rawOutput" "exitCode") = 0)))
      (let ((call (open-call "c2" "(edit \"src/a.txt\" ($ \"old\") ($ \"new\"))")))
        (is (equal "edit" (gethash "kind" call)))
        (is (equal "/work/repo/src/a.txt" (nlk:json-value (aref (gethash "locations" call) 0) :string "path"))))
      (let ((done (close-call "c2" "edited /work/repo/src/a.txt: line 4, -1 +1"
                              (nlk:json-object
                               "calls" (vector (nlk:json-object "verb" "edit" "source" "src/a.txt"))
                               "workspace_diff"
                               (nlk:json-object "files" (vector (nlk:json-object
                                                                 "path" "src/a.txt" "added" 1 "removed" 1
                                                                 "patch" (format nil "--- a/src/a.txt~%+++ b/src/a.txt~%@@ -4 +4 @@~%-old~%+new~%"))))))))
        (is-shape done ("kind" "edit") ("status" "completed"))
        (is (equal "diff" (nlk:json-value (aref (gethash "content" done) 0) :string "type")) "the diff comes first")
        (is (eql 4 (nlk:json-value (aref (gethash "locations" done) 0) :integer "line"))))
      (open-call "c3" "(sleep 1)")
      (let ((agent (nle::make-acp-agent :output (make-broadcast-stream))))
        (nle::acp-fail-open-calls agent session)
        (is (zerop (hash-table-count (nle::acp-session-calls session))) "a turn's end closes what it left open")))))
