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

(defun chat-wire-message (message thinking-p &aux (role (json-value message :string "role")))
  "MESSAGE projected onto the Chat Completions wire schema."
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
               :opt "reasoning_content" (gethash "reasoning_content" message))))
    ;; Thinking mode wants the field on every assistant message, including a
    ;; replayed message that never streamed reasoning_content.
    (when (and thinking-p
               (equal role "assistant")
               (not (json-value wire :string "reasoning_content")))
      (setf (gethash "reasoning_content" wire) ""))
    wire))

(defun request-body (context)
  "Build a chat-completions request solely from compiled CONTEXT."
  ;; Controls are emitted only when configured: an unset control is an absent
  ;; key. The history rides through the turn's memo: a message the turn
  ;; already sent under this shape is its verbatim (JSON-MEMO-ITEMS), one it
  ;; has not is CHAT-WIRE-MESSAGE's object, encoded and remembered at the
  ;; serialize seam.
  (with-request-shape context
      ((messages (request-messages context))
       ;; thinking mode wants reasoning_content on every assistant message: "" where none streamed
       (thinking-p (or (thinking-mode-model-p config.model)
                       (some (lambda (msg)
                               (and (equal "assistant" (json-value msg :string "role"))
                                    (json-value msg :string "reasoning_content")))
                             messages)))
       (wire (concatenate 'vector
                          (vector (message "system"
                                           context.system-prompt))
                          (json-memo-items context.memo
                                           messages
                                           (if thinking-p :chat-thinking :chat)
                                           (lambda (message)
                                             (list (chat-wire-message message thinking-p)))))))
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
         :after-finish ((note-phase "stream-done finish=~a tool-calls=~d" finish-reason
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
                  (delta :object "delta")
                  (dc :string "delta" "content")
                  (tc-deltas :any "delta" "tool_calls"))
          (aref choices 0)
        (when fr (setf finish-reason fr))
        ;; Content chunk. Its arrival closes an open
        ;; thinking span (the chat.zig rule: text and
        ;; reasoning never interleave within one span).
        (when dc
          (assembly-text-delta asm "txt-0" dc :close-reasoning t))
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
