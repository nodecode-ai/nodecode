;;;; openai-responses.lisp --- the OpenAI Responses lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One dialect behind the STREAM-PART waist engine/provider.lisp owns: this
;;;; file shapes the dialect's request and walks its stream. The lane registry,
;;;; the usage invariants, retry policy and connection reuse stay in
;;;; provider.lisp, with what more than one lane shares.

(in-package #:nodecode.evolved)

;;; --- the OpenAI Responses lane ----------------------------------------------
;;; The openai/responses.zig distillation, HTTP SSE only: the Zig lane's
;;; websocket beta transport (codex default, single-reconnect rule, frame
;;; envelopes) is deliberately out of scope — the ChatGPT backend accepts the
;;; same SSE POST, and one transport keeps the walk identical to the other
;;; lanes. The second deliberate divergence: STORE IS ALWAYS FALSE. Zig
;;; leaves store unset (server default true) and replays history as
;;; item_reference stubs against server-side state; this organism's history
;;; authority is its own durable log, so every request is stateless and
;;; reasoning replays by value with its encrypted content — the path Zig's
;;; own model_controls comment names as required for stateless multi-turn.

(defun responses-input-text-part (text)
  (json-object "type" "input_text" "text" text))

(defun responses-reasoning-items (items)
  "ITEMS, the reasoning items the Responses lane collected (newest first, each
(:id :encrypted :parts)), as an assistant message's reasoning_items: output
order, each its id, its own text (its parts in index order, a blank line
apart, the way the lane's reasoning text joins them) and its encrypted content."
  (map 'vector
       (lambda (item)
         (let ((text (format nil "~{~a~^~%~%~}"
                             (remove "" (mapcar #'cdr (sort (copy-list (getf item :parts)) #'<
                                                            :key #'car))
                                     :test #'string=))))
           (json-object "id" (getf item :id)
                        :when (plusp (length text)) "text" text
                        :opt "encrypted_content" (getf item :encrypted))))
       (reverse items)))

(defun responses-message-items (msg thinking-p &optional continued-p &aux (items '()))
  "One wire message's input[] items, in order: a system message rides as a
role string, user content as typed parts, and an assistant message replays
BY VALUE (store:false) — its reasoning item first with the encrypted
content, then its output text, then a function_call per tool call — and a
tool result as function_call_output. Under THINKING-P an assistant message
with no item to replay carries a reasoning item of its own text; under
CONTINUED-P (CONTINUED-ANSWERS) a replayed item carries its text as a
reasoning_text part in place of the summary."
  ;; Reasoning without an OpenAI item id cannot replay and is dropped (the Zig
  ;; rule) — except under THINKING-P, where DeepSeek's endpoint refuses an
  ;; assistant item with no reasoning_text before it (400 "The
  ;; `reasoning_text` in the thinking mode must be passed back"): the exit
  ;; wake's call (EXIT-WAKE-MESSAGE) and a round another lane answered carry
  ;; no item id. Probed: "" and a summary alone are refused, a content part
  ;; of any other text passes. The same 400 comes for an answer's own item
  ;; when the wake's call follows it with no user message between: the
  ;; endpoint takes an answer round's encrypted handle only while a user
  ;; message closes the turn behind it (probed the failed request rebuilt:
  ;; the handle alone refused, the text as a content part beside it passes).
  ;; Not every item rides that way: the part is billed where the summary is
  ;; not, 7.7K of 48K input tokens across one session's rounds. Anything
  ;; that is not a wire message yields nothing.
  (flet ((add (item) (push item items))
         (reasoning-item (id text encrypted content-p)
           (json-object "type" "reasoning" :opt "id" id
                        "summary" (if (and text (not content-p))
                                      (vector (json-object "type" "summary_text" "text" text))
                                      #())
                        :when content-p
                        "content" (vector (json-object "type" "reasoning_text"
                                                       "text" (or text " ")))
                        :opt "encrypted_content" encrypted)))
    (when (hash-table-p msg)
      (let ((role (gethash "role" msg))
            (content (gethash "content" msg)))
        (nlk:dispatch role equal
          ("system" (add (json-object "role" "system"
                                      "content" (content-text content))))
           ;; text stays input_text, images ride input_image data or remote URLs
          ("user" (add (json-object
                        "role" "user"
                        "content" (coerce (map-content-parts
                                           content
                                           :text-fn #'responses-input-text-part
                                           :image-fn (lambda (url)
                                                       (json-object "type" "input_image"
                                                                    "image_url" url)))
                                          'vector))))
          ("assistant" (with-json ((reasoning :text "reasoning_content")
                                   (text (content-text content)))
                           msg
                         ;; Each item replays on its ID, and what it carries is its
                         ;; ENCRYPTED CONTENT — the state the model reasons from.
                         ;; SUMMARY is the visible gloss and is often absent: a round
                         ;; whose summary the API did not emit still has reasoning to
                         ;; replay. Without this, requiring a non-empty summary would
                         ;; drop the encrypted state with it. Replay whenever the id
                         ;; is there and anything to carry is: the encrypted state, a
                         ;; summary, or both; the summary rides only when there is
                         ;; one, because an empty summary_text part is not a part.
                         ;; Every item of the round replays, in order, each with its
                         ;; own state.
                         (let ((replayed nil))
                           (loop for item across (json-array msg "reasoning_items")
                                 for item-id = (json-value item :string "id")
                                 for item-text = (json-value item :string "text")
                                 for encrypted = (json-value item :string "encrypted_content")
                                 when (and item-id (or encrypted item-text))
                                   do (setf replayed t)
                                      (add (reasoning-item item-id item-text encrypted
                                                           continued-p)))
                           (when (and thinking-p (not replayed))
                             (add (reasoning-item nil reasoning nil t))))
                         (when (plusp (length text))
                           (add (json-object
                                 "role" "assistant"
                                 "content" (vector (json-object "type" "output_text"
                                                                "text" text)))))
                         (loop for call across (json-array msg "tool_calls")
                               do (add (json-object
                                        "type" "function_call"
                                        "call_id" (or (gethash "id" call) "")
                                        "name" (or (json-value call :string "function" "name")
                                                   "")
                                        "arguments"
                                        (or (json-value call :string "function" "arguments")
                                            "{}"))))))
          ("tool" (add (json-object
                        "type" "function_call_output"
                        "call_id" (or (gethash "tool_call_id" msg) "")
                        "output" (content-text content))))))))
  (nreverse items))

(defun continued-answers (messages &aux (role (lambda (message)
                                                 (json-value message :string "role"))))
  "The answers in MESSAGES — assistant messages that call no tool — that
another assistant message follows before any user message, past system ones:
the turn an exit wake's call (EXIT-WAKE-MESSAGE) comes straight behind."
  (loop for (message . rest) on (coerce messages 'list)
        when (and (equal "assistant" (funcall role message))
                  (zerop (length (json-array message "tool_calls")))
                  (equal "assistant"
                         (funcall role (find-if-not (lambda (next)
                                                      (equal "system" (funcall role next)))
                                                    rest))))
          collect message))

(defun responses-input-items (context)
  "The input[] item list: every history message's items
(RESPONSES-MESSAGE-ITEMS) through the turn's memo — a message the turn
already sent in this shape is its verbatims (JSON-MEMO-ITEMS), one it has not
is the fresh items, encoded and remembered at the serialize seam. The system
prompt is not among them: it rides as instructions (RESPONSES-REQUEST-BODY)."
  ;; A continued answer's shape hangs on what follows it, so it is a dialect
  ;; of its own: the memo answers by message and dialect alone.
  (let* ((thinking-p (thinking-mode-model-p
                      (effective-provider-config-model (compiled-turn-context-provider-config context))))
         (messages (request-messages context))
         (continued (and thinking-p (continued-answers messages))))
    (loop for message in (coerce messages 'list)
          for continued-p = (and (member message continued :test #'eq) t)
          append (coerce (json-memo-items context.memo
                                          (list message)
                                          (cond (continued-p :responses-continued)
                                                (thinking-p :responses-thinking)
                                                (t :responses))
                                          (lambda (message)
                                            (responses-message-items message thinking-p continued-p)))
                         'list))))

(defun responses-request-body (context)
  "Build a Responses API request from compiled CONTEXT."
  ;; Reasoning mode is
  ;; driven by the configured effort: it adds the reasoning config, requests the
  ;; encrypted content (stateless replay needs it back), and drops temperature
  ;; (the API rejects the combination). stream is always true; store is always
  ;; false (see the lane header).
  (with-request-shape context
      ;; Off is an explicit disable, not the provider default: Responses
      ;; spells it "none" and must receive that value (Pi's Responses
      ;; builder). Only NIL leaves the control absent.
      ((effort (if (and (stringp effort) (string-equal effort "off")) "none" effort))
       (reasoning-p (and (stringp effort)
                         (not (string-equal effort "none")))))
    (json-object
     "model" config.model
     "store" :false
     "stream" t
     ;; The system prompt rides as instructions, never as an input item: the
     ;; OpenAI API reads the two alike, but without this a ChatGPT/Codex
     ;; backend would fill the absent instructions with its own prompt.
     "instructions" context.system-prompt
     "input" (coerce (responses-input-items context) 'vector)
     "prompt_cache_key" (openai-prompt-cache-key context)
     :opt "temperature" (and (not reasoning-p)
                             config.temperature)
     :opt "max_output_tokens" (effective-max-output-tokens context)
     :opt "reasoning" (and effort
                           (json-object "effort" effort
                                        :opt "summary"
                                        (and reasoning-p config.reasoning-summary)))
     :when reasoning-p "include" (vector "reasoning.encrypted_content")
     :opt "tools"
     (and tools
          ;; Flat tool shape (no nested function object); description is
          ;; always present, null when absent.
          (tool-declarations tools "parameters" :type "function" :description :null))
     :opt "tool_choice" (and tools
                             config.tool-choice))))

(define-provider-lane call-responses-streaming
    ;; the Responses API endpoint (responses.zig default_base_url + path)
    (:name "openai-responses" :family :openai :reasoning-carry :text :path "/responses"
     :default-endpoint "https://api.openai.com/v1/responses" :npm "@ai-sdk/openai")
  "POST to the Responses API with stream:true, walk the typed event stream,
and return (values MESSAGE USAGE FINISH-REASON REQUEST-JSON) in the chat
wire shape."
  ;; The message additionally carries reasoning_items, every reasoning item of
  ;; the response in output order with its own id, text and encrypted content,
  ;; so the next round can replay each by value. One response can carry several;
  ;; without this, keeping only the last item's encrypted content would drop the
  ;; rest of the model's reasoning from every later round.
  ;;
  ;; Completion requires an explicit terminal event (response.completed,
  ;; .incomplete, .failed, or error): finish_on_end_of_stream is false for
  ;; this lane, so EOF without one is truncation. An error event is itself
  ;; terminal — the stream is well-formed through :finish and then the
  ;; classified error signals (there is no degraded-error arm here; that rule
  ;; exists for lanes whose error frames are non-terminal).
  (frame :request (with-phase-note ("serialize")
                    (encode-json-body (responses-request-body context) "input" context.memo))
         :state ((reasoning-item-id nil)
                 ;; The thinking span currently accumulating, or NIL before the
                 ;; first one opens. Reasoning arrives as parts under one
                 ;; reasoning item — summary parts, or the reasoning itself as
                 ;; content parts — and their boundary is the only thing
                 ;; separating two adjacent paragraphs — see
                 ;; OPEN-REASONING-PART below.
                 (reasoning-span-id nil)
                 ;; Every reasoning item seen, newest first: (:id :encrypted :parts).
                 (reasoning-items '())
                 (finish-reason nil)
                 (error-frame nil))
         ;; The responses.zig finish order: close text, close reasoning,
         ;; drain tools, then finish; then a terminal error signals.
         :close-order (:text :reasoning :tools)
         :message (:when reasoning-items
                   "reasoning_items" (responses-reasoning-items reasoning-items))
         :after-finish
         ((when error-frame
            ;; responses.zig:1117: overflow keeps its code for eviction; the fatal table is 400.
            (let* ((code (json-first error-frame :string '("code") '("error" "code")))
                   ;; the context does not fit (responses.zig:1131)
                   (overflow (member code '("context_length_exceeded"
                                            "model_context_window_exceeded")
                                     :test #'string-equal))
                   ;; no retry can clear (responses.zig:1121)
                   (fatal (member code '("insufficient_quota" "usage_not_included"
                                         "invalid_prompt" "cyber_policy")
                                  :test #'string-equal)))
              (error 'provider-error
                     :status (and fatal (not overflow) 400)
                     :detail (format nil (cond (overflow "~a: ~a")
                                               (fatal "terminal provider error ~a: ~a")
                                               (t "in-stream provider error~@[ ~a~]: ~a"))
                                     code (or (json-value error-frame :string "message")
                                              (json-value error-frame :string "error" "message")
                                              "")))))
          (note-phase "stream-done finish=~a" finish-reason)))
  ;; No :ENDPOINT clause: the walk defaults to the endpoint the config froze
  ;; unless the credential names one of its own — a backend a saved login is
  ;; served at — and appends whatever headers the credential carries. The lane
  ;; owns its bearer (its family's, the definer's default headers) and its
  ;; wire, nothing about who the caller is.
  ()
  (labels ((reasoning-item (id)
             ;; The cell whose car is the plist of item ID, opened on first sight.
             (or (member id reasoning-items :key (lambda (item) (getf item :id)) :test #'equal)
                 (push (list :id id :encrypted nil :parts '()) reasoning-items)))
           (note-reasoning-text (id index delta &aux (cell (reasoning-item id))
                                                     (part (assoc index (getf (car cell) :parts))))
             ;; DELTA appended to part INDEX of item ID: the item's own text.
             (if part
                 (setf (cdr part) (concatenate 'string (cdr part) delta))
                 (push (cons index delta) (getf (car cell) :parts))))
           (open-reasoning-part (item-id index)
             ;; The item id a frame carries is authority for the part it opens.
             (setf reasoning-item-id (or item-id reasoning-item-id))
             ;; a span per part (formatReasoningId), its boundary in the text too: not `**A****B**`
             (let ((span-id (format nil "~a:~d" (or reasoning-item-id "rs") index)))
               (unless (equal span-id reasoning-span-id)
                 (when reasoning-span-id
                   (lifecycle-close-reasoning asm))
                 (lifecycle-open-reasoning asm span-id)
                 (when (and reasoning-span-id (lane-assembly-reasoning-seen-p asm))
                   (assembly-reasoning-delta asm span-id (format nil "~%~%")))
                 (setf reasoning-span-id span-id)))))
    (let ((type (json-value frame :string "type")))
      (cond
        ((equal type "response.created")
         (assembly-response-metadata
          asm (json-value frame :object "response")
          ;; The effort the provider says it is running
          ;; under — the answer to the request's ask, which
          ;; a relay resolving an unset effort to its own
          ;; default is free to make differ.
          (json-value frame :string "response" "reasoning" "effort")))
        ((equal type "response.output_text.delta")
         (with-json ((delta :text "delta")
                     (item-id :string "item_id"))
             frame
           (when (and delta item-id)
             (assembly-text-delta asm item-id delta :close-reasoning t))))
        ((equal type "response.output_item.added")
         (with-json ((item :object "item")
                     (item-type :string "item" "type")
                     (id :string "item" "id")
                     (ec :string "item" "encrypted_content"))
             frame
           (cond
             ((equal item-type "reasoning")
              (when-let (cell (and id (reasoning-item id)))
                (when ec (setf (getf (car cell) :encrypted) ec)))
              (open-reasoning-part id 0))
             ;; Tool buffers keyed by output_index; the ids are
             ;; call_ids.
             ((equal item-type "function_call")
              (open-tool-buffer asm (or (json-value frame :integer "output_index") 0)
                                :id (wire-call-id (json-value item :string "call_id"))
                                :name (or (json-value item :string "name") ""))))))
        ((equal type "response.function_call_arguments.delta")
         (assembly-tool-fragment asm (or (json-value frame :integer "output_index") 0)
                                 (gethash "delta" frame)))
        ((equal type "response.reasoning_summary_part.added")
         ;; A new summary part under the same reasoning item.
         ;; The parts are separate paragraphs of one thinking
         ;; trace, so their boundary has to survive into the
         ;; text; the delta handler below only ever appends.
         (with-json ((item-id :string "item_id")
                     (index :integer "summary_index" :default 0))
             frame
           (open-reasoning-part item-id index)))
        ((member type '("response.reasoning_summary_text.delta"
                        "response.reasoning_text.delta")
                 :test #'equal)
         ;; A summary part (summary_index) or the reasoning itself as a
         ;; content part (content_index, the shape DeepSeek streams):
         ;; paragraphs of one thinking trace, folded the same way.
         (with-json ((delta :text "delta")
                     (item-id :string "item_id")
                     (summary-index :integer "summary_index")
                     (content-index :integer "content_index"))
             frame
           (when delta
             ;; A provider that streams deltas without ever
             ;; announcing the part still gets the boundary:
             ;; the index the delta carries is authority.
             (open-reasoning-part item-id (or summary-index content-index 0))
             (assembly-reasoning-delta asm reasoning-span-id delta)
             (when reasoning-item-id
               (note-reasoning-text reasoning-item-id (or summary-index content-index 0)
                                    delta)))))
        ((equal type "response.output_item.done")
         (with-json ((ec :string "item" "encrypted_content")
                     (id :string "item" "id")
                     (item-type :string "item" "type"))
             frame
           (cond
             ((equal item-type "reasoning")
              ;; The done event's encrypted content overrides
              ;; the start value (responses.zig:2485), for its own item.
              (when-let (item-id (and ec (or id reasoning-item-id)))
                (setf (getf (car (reasoning-item item-id)) :encrypted) ec))
              (lifecycle-close-reasoning asm))
             ((equal item-type "message")
              (lifecycle-close-text asm)))))
        ((member type '("response.completed" "response.incomplete" "response.failed")
                 :test #'equal)
         ;; A failed response may have streamed half a tool call. Its
         ;; error is the attempt's verdict, never a message to dispatch.
         ;; Share the error event's classifier and the turn's retry path.
         (when (equal type "response.failed")
           (setf error-frame
                 (or (json-value frame :object "response" "error")
                     (json-object "message" "response.failed without an error object")))
           (emit-stream-part on-part :error :payload error-frame))
         (apply-chat-usage (lane-assembly-usage asm) (json-value frame :any "response" "usage"))
         (setf finish-reason
               (cond ((equal type "response.incomplete") "length")
                     ((equal type "response.failed") "error")
                     ((lane-assembly-tool-buffers asm) "tool_calls")
                     (t "stop")))
         (finish-stream))
        ((equal type "error")
         ;; Terminal by contract: the part and :finish still
         ;; land, then the classified error signals below.
         (emit-stream-part on-part :error :payload frame)
         (setf finish-reason "error"
               error-frame frame)
         (finish-stream))))))
