(in-package #:nodecode.test)

;;; --- the free lane: one prompt, the tools as text ---------------------------
;;; api.nodecode.ai answers a free key only for a body that is one prompt with
;;; no tools array: a second turn, an assistant message or a tools array all
;;; answer 403 free_needs_nodecode. So the chat lane folds the conversation
;;; into one user message, spells the tools out in the system prompt, and reads
;;; the model's fenced ```tool block back as the tool call it meant. These
;;; drive the reading and the folding.

(defun free-block (name arguments)
  "TEXT with one fenced tool block naming NAME, its arguments as the JSON OBJECT's text."
  (format nil "Looking.~%~%```tool~%{\"name\":~s,\"arguments\":~s}~%```"
          name (let ((stream (make-string-output-stream)))
                 (shasht:write-json arguments stream)
                 (get-output-stream-string stream))))

(deftest a-fenced-tool-block-becomes-the-call-it-names
  (let* ((answer (nle::free-lane-answer
                  (nlk:json-object "role" "assistant"
                                   "content" (free-block "eval" (nlk:json-object "form" "(+ 1 2)")))))
         (calls (gethash "tool_calls" answer)))
    (is (equal "Looking." (gethash "content" answer)) "the operator-facing text keeps its place")
    (is (= 1 (length calls)) "one block is one call")
    (is (equal "eval" (nlk:json-value (aref calls 0) :string "function" "name")))
    (is (equal "(+ 1 2)"
               (nlk:json-value (nlk:decode-json (nlk:json-value (aref calls 0) :string "function" "arguments"))
                           :string "form")))))

(deftest a-block-with-no-text-before-it-answers-null-content
  (let* ((answer (nle::free-lane-answer
                  (nlk:json-object "role" "assistant"
                                   "content" (format nil "```tool~%{\"name\":\"eval\",\"arguments\":{\"form\":\"1\"}}~%```")))))
    (is (eq :null (gethash "content" answer)) "an answer that is only a call has no content")
    (is (= 1 (length (gethash "tool_calls" answer))))))

(deftest two-blocks-are-two-calls-with-their-own-ids
  (let* ((both (format nil "~a~%~a" (free-block "eval" (nlk:json-object "form" "1"))
                        (free-block "current_time" (nlk:json-object))))
         (answer (nle::free-lane-answer (nlk:json-object "role" "assistant" "content" both)))
         (calls (gethash "tool_calls" answer)))
    (is (= 2 (length calls)))
    (is (not (equal (nlk:json-value (aref calls 0) :string "id")
                    (nlk:json-value (aref calls 1) :string "id")))
        "each call carries an id of its own")
    (is (equal "current_time" (nlk:json-value (aref calls 1) :string "function" "name")))))

(deftest an-answer-without-a-block-is-left-alone
  (let ((answer (nle::free-lane-answer (nlk:json-object "role" "assistant" "content" "just words"))))
    (is (equal "just words" (gethash "content" answer)))
    (is (null (gethash "tool_calls" answer)))))

(deftest a-fence-that-names-no-call-stays-text
  (let ((answer (nle::free-lane-answer
                 (nlk:json-object "role" "assistant"
                                  "content" (format nil "a snippet:~%```tool~%not json~%```")))))
    (is (search "```tool" (gethash "content" answer)))
    (is (null (gethash "tool_calls" answer)))))

(deftest the-transcript-names-who-spoke-and-what-the-tool-returned
  (let* ((history (vector (nlk:json-object "role" "user" "content" "which files?")
                          (nlk:json-object "role" "assistant" "content" :null
                                           "tool_calls" (vector (nle::chat-tool-call-object "c1" "eval" "(sh \"ls\")")))
                          (nlk:json-object "role" "tool" "content" "a.txt" "tool_call_id" "c1")
                          (nlk:json-object "role" "assistant" "content" "One file: a.txt.")
                          (nlk:json-object "role" "user" "content" "and /tmp?")))
         (text (nle::free-lane-transcript history)))
    (is (search "[1] OPERATOR" text))
    (is (search "ASSISTANT CALLED" text) "the call reads as the call it was")
    (is (search "[3] TOOL RESULT" text))
    (is (search "One file: a.txt." text))
    (is (search "and /tmp?" text) "the newest request is in the prompt")))

(deftest a-free-lane-body-carries-no-tools-and-one-user-message
  (let* ((history (vector (nlk:json-object "role" "user" "content" "hi")
                          (nlk:json-object "role" "assistant" "content" "hello")))
         (transcript (nle::free-lane-transcript history)))
    (is (search "hello" transcript))
    (is (not (search "\"role\":\"assistant\"" transcript))
        "the wire the free key answers has no assistant message in it")))
