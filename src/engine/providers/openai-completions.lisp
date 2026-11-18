;;;; openai-completions.lisp --- the OpenAI chat-completions lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One dialect behind the STREAM-PART waist engine/provider.lisp owns: this
;;;; file shapes the dialect's request and walks its stream. The lane registry,
;;;; the usage invariants, retry policy and connection reuse stay in
;;;; provider.lisp, with what more than one lane shares.

(in-package #:nodecode.evolved)

;;; --- the openai-completions chat lane ---------------------------------------

(defun reasoning-content-endpoint-p (endpoint)
  "Whether ENDPOINT's chat wire takes the reasoning_content message field.
Mistral's (api.mistral.ai) does not: it validates each message against its
own schema and answers 422 extra_forbidden for a field the schema does not
name, and it carries a model's thinking as a thinking chunk inside content
(docs.mistral.ai/inference/conversations/reasoning)."
  (not (endpoint-on-domain-p endpoint "mistral.ai")))

(defun chat-wire-message (message reasoning &aux (role (json-value message :string "role")))
  "MESSAGE projected onto the Chat Completions wire schema. REASONING is what
becomes of an assistant message's reasoning_content: :CARRY passes it on,
:BACKFILL also gives \"\" to a message that never streamed one, NIL drops it."
  ;; History is shared across provider lanes, but its assistant object can carry
  ;; lane-owned replay state: Responses reasoning_items, Google
  ;; thought_signatures, or Anthropic reasoning_blocks. Never pass those
  ;; fields through merely because this lane received the object. The projection
  ;; also makes the system-stub re-role explicit instead of copying an unknown
  ;; future field into a request.
  (let ((wire (json-object
               "role" (if (equal role "system") "user" role)
               "content" (gethash "content" message)
               :opt "name" (gethash "name" message)
               :opt "tool_call_id" (gethash "tool_call_id" message)
               :opt "tool_calls" (gethash "tool_calls" message)
               :opt "reasoning_content" (and reasoning (gethash "reasoning_content" message)))))
    ;; Thinking mode wants the field on every assistant message, including a
    ;; replayed message that never streamed reasoning_content.
    (when (and (eq reasoning :backfill)
               (equal role "assistant")
               (not (json-value wire :string "reasoning_content")))
      (setf (gethash "reasoning_content" wire) ""))
    wire))

(defun empty-user-message-p (message)
  "Whether MESSAGE goes on the chat wire as a user message with nothing in it:
no text and no image."
  ;; A channel room records an answer no new message asked for (a background
  ;; evaluation's wake, a turn a restart resumed) with "" for its user half,
  ;; and every lane forked off the room carries it. A strict chat endpoint
  ;; refuses the whole request for that one message (Moonshot: 400 `the
  ;; message at position N with role 'user' must not be empty'), so it is left
  ;; out, as the Messages lane leaves it out (ANTHROPIC-MESSAGES). The answers
  ;; on either side of it then stand next to each other, as an exit wake's
  ;; first round already stands next to the answer before it.
  (and (member (json-value message :string "role") '("user" "system") :test #'equal)
       (null (map-content-parts (gethash "content" message)
                                :text-fn #'identity
                                :image-fn #'identity
                                :drop-empty-text-p t))))

;;; --- the free lane: one prompt, the tools as text ---------------------------
;;;
;;; A free key on api.nodecode.ai is answered only for a body that is a single
;;; prompt: at most one system message and one user message, and no "tools"
;;; array at all. Any other shape answers 403 free_needs_nodecode ("A free key
;;; works only from the Nodecode CLI"): a tools array, a second turn, an
;;; assistant message. So this lane, on such a provider, folds the whole
;;; conversation into one user message, spells the tools out in the system
;;; prompt, and reads the model's fenced ```tool block back as the tool call it
;;; meant.

(defparameter *free-lane-providers* '("nodecode-free")
  "Provider ids whose endpoint takes one prompt and no tool schemas.")

(defun free-lane-provider-p (provider)
  "Whether PROVIDER is a free-lane provider id."
  (and (stringp provider)
       (member provider *free-lane-providers* :test #'string-equal)))

(defun free-lane-protocol-heading (tools)
  "The paragraph teaching the fenced call, and the tools it names, or NIL when
this turn carries no tool at all: a turn with none teaches nothing, so a model
cannot call a tool that is not there."
  (when (plusp (length (or tools #())))
    (format nil "TOOLS. This endpoint takes no tool schemas, so a tool call is
text: to call one, end your message with a fenced block in exactly this shape,
its JSON whole and its closing fence after it:

```tool
{\"name\": \"eval\", \"arguments\": {\"form\": \"(+ 1 2)\"}}
```

One block is one call, and two blocks in one message are two calls. The
operator-facing text goes before the block. The result comes back in the next
message as a line beginning TOOL RESULT. The tools, each with the arguments
it takes:~%~a"
            (free-lane-tools-text tools))))

(defparameter +free-lane-transcript-heading+
  "The conversation follows as one transcript: a line [n] names who spoke, and
the lines under it are what they said. Answer the last OPERATOR entry.")

(defun free-lane-content-text (content)
  "CONTENT's text, an image named wherever one stood."
  (format nil "~{~a~^~%~}"
          (map-content-parts content
                             :text-fn #'identity
                             :image-fn (lambda (url)
                                          (declare (ignore url))
                                          "[image]"))))

(defun free-lane-lines (message)
  "MESSAGE as the transcript lines that carry it: a header, then its text."
  (let* ((role (or (json-value message :string "role") "user"))
         (text (or (free-lane-content-text (gethash "content" message)) ""))
         (calls (gethash "tool_calls" message))
         (lines '()))
    (cond
      ((equal role "assistant")
       (when (plusp (length text))
         (push (format nil "ASSISTANT~%~a" text) lines))
       (when (vectorp calls)
         (loop for call across calls
               do (multiple-value-bind (name input)
                      (tool-call-function-input call)
                    (push (format nil "ASSISTANT CALLED~%~a ~a"
                                  (or name "") (encode-json-object input))
                          lines)))))
      ((equal role "tool")
       (push (format nil "TOOL RESULT~%~a" text) lines))
      ((equal role "system")
       (when (plusp (length text))
         (push (format nil "NOTE~%~a" text) lines)))
      (t (push (format nil "OPERATOR~%~a" text) lines)))
    (nreverse lines)))

(defun free-lane-transcript (messages)
  "MESSAGES as one text prompt, every entry numbered."
  (let ((out (make-string-output-stream)))
    (loop for message across messages
          for n from 1
          do (loop for line in (free-lane-lines message)
                   do (format out "[~d] ~a~%" n line)))
    (get-output-stream-string out)))

(defun free-lane-tools-text (tools)
  "TOOLS as the protocol's list: its name, its description, its arguments."
  (with-output-to-string (out)
    (loop for wrapper across (or tools #())
          for fn = (json-value wrapper :any "function")
          when fn
            do (format out "~%- ~a~@[ — ~a~]~%  arguments: ~a"
                       (or (json-value fn :string "name") "")
                       (let ((description (json-value fn :string "description")))
                         (cond ((null description) nil)
                               ((> (length description) 600)
                                (concatenate 'string (subseq description 0 600) "…"))
                               (t description)))
                       (encode-json-object
                        (or (json-value fn :any "parameters") (json-object)))))))

(defun free-lane-request-body (context)
  "One prompt carrying the whole conversation and the tools as text."
  (with-request-shape context
      ((provider (effective-provider-config-provider config)))
    (declare (ignore provider))
    (let ((system (format nil "~a~@[~%~%~a~]~%~%~a"
                          (or context.system-prompt "")
                          (free-lane-protocol-heading tools)
                          +free-lane-transcript-heading+)))
      (json-object
       "model" config.model
       "messages" (vector (json-object "role" "system" "content" system)
                           (json-object "role" "user" "content"
                                        (free-lane-transcript (request-messages context))))
       "prompt_cache_key" (openai-prompt-cache-key context)
       :opt "max_tokens" (effective-max-output-tokens context)
       :when (and effort (not (string-equal effort "off"))) "reasoning_effort" effort))))

(defparameter +free-lane-call-fence+
  "(?s)```[ \\t]*(?i:tool|json)?[ \\t]*\\r?\\n?(.*?)(?:```|\\z)"
  "A fenced block that may carry a call: the info string a model reaches for,
the body, and the close -- or the answer's own end, when the block was cut
short.")

(defun free-lane-close-brackets (text)
  "TEXT with every bracket its end left open closed, a string it left open
closed too: a body the answer cut short still reads."
  (let ((closers '()) (in-string nil) (escape nil))
    (loop for ch across text
          do (cond (escape (setf escape nil))
                   ((and in-string (char= ch #\\)) (setf escape t))
                   ((char= ch #\") (setf in-string (not in-string)))
                   (in-string)
                   ((char= ch #\{) (push #\} closers))
                   ((char= ch #\[) (push #\] closers))
                   ((equal (first closers) ch) (pop closers))))
    (concatenate 'string text (if in-string "\"" "") (coerce closers 'string))))

(defun free-lane-sanitize (text)
  "TEXT with a control character the model wrote raw inside a JSON string
escaped, which JSON forbids and models do anyway."
  (with-output-to-string (o)
    (let ((in-string nil) (escape nil))
      (loop for ch across text
            do (cond (escape (write-char ch o) (setf escape nil))
                     ((char= ch #\\) (write-char ch o) (setf escape t))
                     ((char= ch #\") (write-char ch o) (setf in-string (not in-string)))
                     ((and in-string (member ch '(#\Newline #\Return #\Tab)))
                      (write-string (ecase ch (#\Newline "\\n") (#\Return "\\r") (#\Tab "\\t")) o))
                     (t (write-char ch o)))))))

(defun free-lane-decode-call (raw)
  "RAW as the object it holds, repaired where the answer cut it short, or NIL."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (labels ((object (string)
               (let ((json (ignore-errors (nlk:decode-json string))))
                 (and (hash-table-p json) json))))
      (flet ((decoded (string)
               (or (object string)
                   (object (free-lane-sanitize string))
                   (object (free-lane-close-brackets (free-lane-sanitize string))))))
        (or (decoded text)
            (let ((cut (position #\} text :from-end t)))
              (and cut (decoded (subseq text 0 (1+ cut))))))))))

(defun free-lane-call-of (json)
  "The (NAME . ARGUMENTS) JSON names, or NIL when it names no tool."
  (let* ((function (json-value json :any "function"))
         (name (or (json-value json :string "name")
                   (json-value json :string "tool")
                   (and (hash-table-p function) (json-value function :string "name"))))
         (arguments (or (json-value json :any "arguments")
                        (json-value json :any "input")
                        (json-value json :any "parameters")
                        (and (hash-table-p function)
                             (json-value function :any "arguments")))))
    (when (and (stringp name) (plusp (length name)))
      (cons name
            (cond ((and (stringp arguments)
                        (plusp (length (string-trim '(#\Space #\Tab #\Newline) arguments))))
                   arguments)
                  ((hash-table-p arguments) (encode-json-object arguments))
                  (t "{}"))))))

(defun free-lane-split-tool-text (text)
  "TEXT without its call blocks, and those blocks as (NAME . ARGUMENTS) calls,
in the order they stood. A block that names no call stays the text it was."
  (let ((calls '()) (out (make-string-output-stream)) (cursor 0))
    (loop
      (multiple-value-bind (start end)
          (ppcre:scan +free-lane-call-fence+ text :start cursor)
        (unless start (return))
        (let* ((piece (subseq text start end))
               (body (multiple-value-bind (match registers)
                         (ppcre:scan-to-strings +free-lane-call-fence+ piece)
                       (declare (ignore match))
                       (and registers (aref registers 0))))
               (json (and body (free-lane-decode-call body)))
               (call (and json (free-lane-call-of json))))
          (cond (call
                 (push call calls)
                 (write-string text out :start cursor :end start))
                (t (write-string text out :start cursor :end end)))
          (setf cursor end))))
    (write-string text out :start cursor)
    (values (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (get-output-stream-string out))
            (nreverse calls))))

(defvar *free-lane-call-sequence* 0
  "How many free-lane tool calls this process has read: every id its own.")

(defun free-lane-dangling-fence (text)
  "TEXT without the fence it ends on when that fence was never closed: a marker
with no call in it is not what the operator asked to read."
  (let ((count 0) (last 0) (at 0))
    (loop while (setf at (search "```" text :start2 at))
          do (incf count) (setf last at at (+ at 3)))
    (when (oddp count)
      (string-right-trim '(#\Space #\Tab #\Newline #\Return) (subseq text 0 last)))))

(defun free-lane-answer (message)
  "MESSAGE with its call blocks read back as the tool calls they name."
  (let ((content (json-value message :string "content")))
    (when (and content (search "```" content))
      (multiple-value-bind (clean calls) (free-lane-split-tool-text content)
        (cond
          (calls
           (setf (gethash "content" message) (if (zerop (length clean)) :null clean))
           (setf (gethash "tool_calls" message)
                 (map 'vector
                      (lambda (call)
                        (chat-tool-call-object
                         (format nil "free-~a" (incf *free-lane-call-sequence*))
                         (car call) (cdr call)))
                      calls)))
          (t (let ((cut (free-lane-dangling-fence clean)))
               (when (and cut (not (equal cut clean)))
                 (setf (gethash "content" message)
                       (if (zerop (length cut)) :null cut))))))))
    message))

(defun request-body (context)
  "The chat body for CONTEXT: the free lane's one prompt where the provider
answers no other shape, the plain chat body otherwise."
  (if (free-lane-provider-p
       (effective-provider-config-provider
        (compiled-turn-context-provider-config context)))
      (free-lane-request-body context)
      (%request-body context)))

(defun %request-body (context)
  "Build a chat-completions request solely from compiled CONTEXT."
  ;; Controls are emitted only when configured: an unset control is an absent
  ;; key. The history rides through the turn's memo: a message the turn
  ;; already sent under this shape is its verbatim (JSON-MEMO-ITEMS), one it
  ;; has not is CHAT-WIRE-MESSAGE's object, encoded and remembered at the
  ;; serialize seam.
  (with-request-shape context
      ((messages (request-messages context))
       ;; thinking mode wants reasoning_content on every assistant message: ""
       ;; where none streamed -- unless the endpoint's schema has no such field
       (reasoning (cond ((not (reasoning-content-endpoint-p config.endpoint)) nil)
                        ((or (thinking-mode-model-p config.model)
                             (some (lambda (msg)
                                     (and (equal "assistant" (json-value msg :string "role"))
                                          (json-value msg :string "reasoning_content")))
                                   messages))
                         :backfill)
                        (t :carry)))
       (wire (concatenate 'vector
                          (vector (message "system"
                                           context.system-prompt))
                          (json-memo-items context.memo
                                           messages
                                           (ecase reasoning
                                             (:backfill :chat-thinking)
                                             (:carry :chat)
                                             ((nil) :chat-unreasoned))
                                           (lambda (message)
                                             (unless (empty-user-message-p message)
                                               (list (chat-wire-message message reasoning))))))))
    (json-object
     "model" config.model
     "messages" wire
     "prompt_cache_key" (openai-prompt-cache-key context)
     :opt "tools" tools
     :when (and tools choice) "tool_choice" choice
     :opt "temperature" config.temperature
     :opt "top_p" config.top-p
     ;; Configured ceiling first, else capability shrink-to-fit; the key
     ;; stays absent when neither source knows (the chat lane has no
     ;; wire-required max_tokens).
     :opt "max_tokens" (effective-max-output-tokens context)
     ;; Chat-completions dialects have no reliable disable spelling; an
     ;; off selection omits the field so the provider default applies
     ;; (ai/model_controls.zig applyOpenAICompatible).
     :when (and effort (not (string-equal effort "off")))
     "reasoning_effort" effort)))

(defun chat-content-chunks (asm chunks)
  "A streamed delta.content that came as the list CHUNKS, read in order: a
thinking chunk is reasoning, a text chunk is content."
  ;; Mistral streams a reasoning model this way
  ;; (docs.mistral.ai/inference/conversations/reasoning): lists holding a
  ;; thinking chunk while it thinks, one list holding the closing thinking
  ;; chunk and the first text chunk, then plain strings. A thinking chunk's
  ;; trace is itself a list of text chunks. Any other chunk type (a
  ;; reference, an image) carries no text for the transcript.
  (loop for chunk across chunks
        when (hash-table-p chunk)
          do (with-json ((kind :string "type")
                         (text :string "text")
                         (thinking :any "thinking"))
                 chunk
               (cond ((equal kind "thinking")
                      (if (stringp thinking)
                          (assembly-reasoning-delta asm "reasoning-0" thinking)
                          (when (vectorp thinking)
                            (loop for inner across thinking
                                  for trace = (and (hash-table-p inner)
                                                   (json-value inner :string "text"))
                                  when trace
                                    do (assembly-reasoning-delta asm "reasoning-0" trace)))))
                     ((and (equal kind "text") text)
                      (assembly-text-delta asm "txt-0" text :close-reasoning t))))))

(define-provider-lane call-provider-streaming
    ;; OpenRouter's own package names the same chat-completions wire.
    (:name "openai-completions" :family :openai-completions
     :npm '("@ai-sdk/openai-compatible" "@openrouter/ai-sdk-provider")
     :default-endpoint "https://opencode.ai/zen/v1/chat/completions" :path "/chat/completions")
  "POST the chat request with stream:true, walk the SSE body, and return
(values MESSAGE USAGE FINISH-REASON) where MESSAGE is the assembled
chat-shaped assistant message."
  ;; ON-PART, when non-nil, receives every STREAM-PART in arrival order. It
  ;; defaults to *TURN-PART-FN* so the turn loop's callers inject the observer
  ;; without touching the call chain.
  ;;
  ;; Usage is requested via stream_options.include_usage and captured from the
  ;; terminal usage chunk (which arrives with an empty choices array). A
  ;; stream that ends without [DONE] signals PROVIDER-STREAM-INCOMPLETE.
  ;; In-stream error frames signal PROVIDER-ERROR. finish_reason is returned,
  ;; never written onto the message: the message goes back to the provider
  ;; verbatim on the next round, and finish_reason is not part of that
  ;; contract.
  ;;
  ;; Tool call buffers are plain strings rather than streams so the
  ;; accumulated state is readable at any point without consuming/resetting
  ;; the buffer.
  ;; Accumulate content tokens and tool_call delta fragments. Thinking-mode
  ;; endpoints stream reasoning_content deltas and refuse the next request
  ;; unless the assembled message carries them back verbatim, so the
  ;; assembly accumulates them beside the content.
  (chunk :request (with-phase-note ("serialize")
                    (let ((body (request-body context)))
                      (setf (gethash "stream" body) t
                            (gethash "stream_options" body)
                            (json-object "include_usage" t))
                      (encode-json-body body "messages"
                                        (compiled-turn-context-memo context))))
         :state ((finish-reason nil))
         ;; Close every open span before :finish (the chat.zig finish order:
         ;; drain tools, close reasoning, close text), then reconstruct the
         ;; message in the chat wire shape.
         :close-order (:tools :reasoning :text)
         :after-finish ((when (free-lane-provider-p
                               (effective-provider-config-provider config))
                          (free-lane-answer message))
                        (note-phase "stream-done finish=~a tool-calls=~d" finish-reason
                                    (length (lane-assembly-tool-buffers asm)))))
  (:finish-on-sentinel t
   ;; An error frame's numeric code is the status retry reads; a code-less one is transient.
   :signal-error-fn (lambda (frame)
                      (error 'provider-error
                             :status (json-value frame :integer "code")
                             :detail (format nil "in-stream provider error: ~a"
                                             (or (json-value frame :string "message")
                                                 (encode-json-object frame))))))
  (assembly-response-metadata asm chunk)
  ;; An in-stream error frame is recorded, never signalled here:
  ;; whether it is round-fatal depends on how the stream ends (the
  ;; bracket's degraded-error rule).
  (when-let (frame-error (json-value chunk :object "error"))
    (record-stream-error frame-error frame-error))
  (apply-chat-usage (lane-assembly-usage asm) (gethash "usage" chunk))
  (let ((choices (gethash "choices" chunk)))
    (when (and (vectorp choices) (plusp (length choices)))
      (with-json ((fr :string "finish_reason")
                  (sr :string "stop_reason")
                  (delta :object "delta")
                  (dc :any "delta" "content")
                  (tc-deltas :any "delta" "tool_calls"))
          (aref choices 0)
        ;; A server that stops a looping generation itself says so: vLLM's
        ;; repetition detection answers finish_reason "repetition", or
        ;; stop_reason "repetition_detected" beside a plain "stop" or no
        ;; finish at all. Either is the round's finish "repetition", which a
        ;; later clean finish never masks (the loop guard reads it).
        (flet ((repetition-p (reason)
                 (member reason '("repetition" "repetition_detected") :test #'string-equal)))
          (cond ((or (repetition-p fr) (repetition-p sr)) (setf finish-reason "repetition"))
                ((and fr (not (equal finish-reason "repetition"))) (setf finish-reason fr))))
        ;; Content chunk. Its arrival closes an open
        ;; thinking span (the chat.zig rule: text and
        ;; reasoning never interleave within one span).
        ;; A content list is typed chunks, thinking and text in the order sent.
        (typecase dc
          (string (assembly-text-delta asm "txt-0" dc :close-reasoning t))
          (vector (chat-content-chunks asm dc)))
        ;; Reasoning chunk (thinking mode). WIRE-REASONING
        ;; spans the field names the endpoints disagree on
        ;; (llama.cpp reasoning_content, vLLM reasoning) and
        ;; folds a doubled trace to one. Kept even when empty:
        ;; the key's presence, not its text, is what the
        ;; endpoint checks on replay -- hence PRESENT.
        (multiple-value-bind (rc present) (wire-reasoning delta)
          (when present
            (assembly-reasoning-delta asm "reasoning-0" rc)))
        ;; Tool call fragments, accumulated into plain
        ;; strings so the live state is readable at any
        ;; point; the lifecycle brackets each call and
        ;; fires a delta per fragment so the UI can
        ;; render in-flight tool arguments.
        (when (vectorp tc-deltas)
          (lifecycle-close-reasoning asm)
          (loop for tc across tc-deltas do
            (with-json ((idx :integer "index" :default 0)
                        (id :string "id")
                        (name :string "function" "name")
                        (fragment :string "function" "arguments")
                        ;; created empty (bracket shut) on the first delta naming its
                        ;; index, under that delta's id or one minted for a call sent without
                        (buf (or (cdr (assoc idx (lane-assembly-tool-buffers asm)))
                                 (open-tool-buffer asm idx :id (wire-call-id id)
                                                           :name "" :arguments ""))))
                tc
              ;; a real id arriving after the first delta still names the
              ;; call, until its bracket opened under the minted one
              (when (and (plusp (length id)) (not (getf buf :started-p)))
                (setf (getf buf :id) id))
              (when name
                (setf (getf buf :name) (concatenate 'string (getf buf :name) name)))
              (if fragment
                  (assembly-tool-fragment asm idx fragment)
                  (lifecycle-tool-progress asm idx buf nil)))))))))
