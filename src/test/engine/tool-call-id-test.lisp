(in-package #:nodecode.test)

;;; --- a tool call streamed with no id (GH #32) -------------------------------
;;; Some OpenAI-compatible routes stream tool_call frames with no `id`, on
;;; every call of a batch or now and then. Stored as "", the call and its
;;; result replay with empty ids, and a strict endpoint (OpenRouter's
;;; validator) refuses the whole history with a 400 -- on every later turn,
;;; since each one re-sends it. These drive turn-test's canned SSE helpers.

(defun call-chunk-json (idx fn-name fn-args &key (id nil id-p))
  "One streaming chunk carrying a tool_call delta at IDX: no id key at all,
or ID verbatim when one is given."
  (json-line (nlk:json-object
              "choices"
              (vector (nlk:json-object
                       "finish_reason" "tool_calls"
                       "delta" (nlk:json-object
                                "tool_calls"
                                (vector (nlk:json-object
                                         "index" idx
                                         :when id-p "id" id
                                         "type" "function"
                                         "function" (nlk:json-object
                                                     "name" fn-name
                                                     "arguments" fn-args)))))))))

(defun present-id-p (id)
  (and (stringp id) (plusp (length id))))

(defun strict-endpoint-refusal (body)
  "What a strict OpenAI-compatible validator (OpenRouter's) answers the chat
request BODY with, NIL when it admits it: the first assistant tool_calls entry
or tool message whose id is not a non-empty string."
  (loop for message across (gethash "messages" (nlk:decode-json body))
        for role = (gethash "role" message)
        for calls = (gethash "tool_calls" message)
        thereis (cond ((equal role "tool")
                       (unless (present-id-p (gethash "tool_call_id" message)) "tool messages must include a non-empty string tool_call_id"))
                      ((and (equal role "assistant") (vectorp calls))
                       (loop for call across calls
                             for k from 0
                             unless (present-id-p (gethash "id" call))
                               return (format nil "tool_calls[~d].id must be a non-empty string" k))))))

(defun wire-call-pairs (messages)
  "(CALL-IDS . RESULT-IDS) across MESSAGES, each in wire order."
  (let ((calls '()) (results '()))
    (dolist (message messages)
      (let ((tool-calls (gethash "tool_calls" message)))
        (when (vectorp tool-calls)
          (loop for call across tool-calls do (push (gethash "id" call) calls))))
      (when (equal "tool" (gethash "role" message))
        (push (gethash "tool_call_id" message) results)))
    (cons (nreverse calls) (nreverse results))))

(deftest an-id-less-streamed-tool-call-is-given-an-id ()
  ;; one call with no id key, one with an empty id: both leave the lane named
  (let* ((message (streamed-message (call-chunk-json 0 "eval" "{\"form\": \"1\"}")
                                    (call-chunk-json 1 "eval" "{\"form\": \"2\"}" :id "")))
         (ids (map 'list (lambda (call) (gethash "id" call)) (gethash "tool_calls" message))))
    (is (= 2 (length ids)))
    (is (every #'present-id-p ids) "every streamed call carries a non-empty id")
    (is (not (equal (first ids) (second ids))) "the minted ids are distinct")))

(deftest a-streamed-id-still-names-its-call ()
  ;; the id arrives on the call's first delta, the arguments on later id-less ones
  (let ((message (streamed-message (call-chunk-json 0 "eval" "{\"form\": " :id "call_real")
                                   (call-chunk-json 0 "" "\"1\"}"))))
    (is (equal "call_real" (gethash "id" (aref (gethash "tool_calls" message) 0))))
    (is (equal "{\"form\": \"1\"}"
               (gethash "arguments" (gethash "function" (aref (gethash "tool_calls" message) 0)))))))

(deftest an-id-less-call-round-trips-to-a-strict-endpoint (with-session-store ("blank-stream"))
  ;; Round one streams a call with no id; round two is what a strict endpoint
  ;; receives: the call and its result under one non-empty id.
  (let ((nle::*api-key* "k")
        (streams (list (make-sse-stream (call-chunk-json 0 "eval" "{\"form\": \"1\"}"))
                       (make-sse-stream (chunk-json "done" :finish-reason "stop"))))
        (bodies '()))
    (with-stubbed-fdefinition (dex:post (url &rest args)
                               (push (getf args :content) bodies)
                               (values (pop streams) 200))
      (nle:run "go" :session (nle:intern-session "blank-stream")
               :command-id "blank-stream-cmd"))
    (is (= 2 (length bodies)) "two rounds were sent")
    (is (null (strict-endpoint-refusal (first bodies))) "the round after the id-less call passes a strict validator")
    (destructuring-bind (calls . results)
        (wire-call-pairs (coerce (gethash "messages" (nlk:decode-json (first bodies))) 'list))
      (is (= 1 (length calls)))
      (is (equal calls results) "the result answers the call's own id"))))

(defun seed-blank-id-turn (session-id &key (cut-off t))
  "Journal on SESSION-ID the history an id-less provider left before the fix:
a stored assistant message of three calls whose ids are all \"\", their call
facts and results under \"\" too -- the third cut off with no result when
CUT-OFF -- and complete the turn."
  (ensure-durable-session session-id)
  (let ((turn (nlk:admit-turn session-id (format nil "~a-cmd" session-id) "go"))
        (args '("{\"form\":\"1\"}" "{\"form\":\"2\"}" "{\"form\":\"3\"}")))
    (nlk:record-assistant-message
     turn (wire-message "assistant" :content ""
                        :tool-calls (map 'vector (lambda (a) (wire-tool-call "" "eval" a)) args)))
    (record-tool-use turn "" "eval" (first args) "1")
    (record-tool-use turn "" "eval" (second args) "2")
    (if cut-off
        (nlk:record-tool-call turn "" "eval" (third args))
        (record-tool-use turn "" "eval" (third args) "3"))
    (nlk:record-assistant-message turn (wire-message "assistant" :content "ok"))
    (nlk:complete-turn turn nil)))

(deftest durable-calls-with-empty-ids-fold-named-and-paired (with-session-store ("blank-fold"))
  (seed-blank-id-turn "blank-fold")
  (let* ((messages (wire-fold "blank-fold"))
         (pairs (wire-call-pairs messages))
         (calls (car pairs)))
    (is (equal '("user" "assistant" "tool" "tool" "tool" "assistant")
               (mapcar (lambda (message) (gethash "role" message)) messages)))
    (is (= 3 (length calls)))
    (is (every #'present-id-p calls) "each empty call id folds as a non-empty one")
    (is (= 3 (length (remove-duplicates calls :test #'equal))) "the healed ids are distinct")
    (is (equal calls (cdr pairs)) "results pair with their calls in order, the cut-off one answered as such")
    (is (equal nle::+orphan-tool-result+ (gethash "content" (fifth messages))))
    (is (equal calls (car (wire-call-pairs (wire-fold "blank-fold")))) "the healed ids are the same on every fold: the cached prefix holds")))

(deftest a-session-bricked-by-empty-ids-recovers-on-its-next-turn
    (with-session-store ("blank-heal"))
  (seed-blank-id-turn "blank-heal" :cut-off nil)
  (let ((nle::*api-key* "k")
        (body nil))
    (with-stubbed-fdefinition (dex:post (url &rest args)
                               (setf body (getf args :content))
                               (values (make-sse-stream (chunk-json "done" :finish-reason "stop")) 200))
      (nle:run "next" :session (nle:intern-session "blank-heal")
               :command-id "blank-heal-next"))
    (is body "the next turn reached the provider")
    (is (null (strict-endpoint-refusal body)) "the old empty ids no longer reach the wire")))
