;;;; google.lisp --- the Google GenAI lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One dialect behind the STREAM-PART waist engine/provider.lisp owns: this
;;;; file shapes the dialect's request and walks its stream. The lane registry,
;;;; the usage invariants, retry policy and connection reuse stay in
;;;; provider.lisp, with what more than one lane shares.

(in-package #:nodecode.evolved)

;;; --- the Google GenAI lane -------------------------------------------------
;;; The gen_ai.zig distillation. Contract corners that differ from the chat
;;; lanes, all deliberate Zig behaviors: completion is END OF STREAM (no
;;; [DONE], no terminal event — a truncated socket is indistinguishable from
;;; success, so truncation surfaces only as finishReason MAX_TOKENS); a
;;; {"error": ...} frame carries no candidates and is silently ignored (the
;;; lane emits no :error parts); function calls arrive whole, one per part,
;;; with locally minted ids (Gemini has none on the wire); topP and the other
;;; sampling knobs are unexpressible (CallOptions carries none for google).

(defun google-stream-url (base model)
  "BASE/models/MODEL:streamGenerateContent?alt=sse — the models/ segment is
skipped when MODEL already carries a path (tunedModels/..., gen_ai.zig:288)."
  (format nil "~a/~a~a:streamGenerateContent?alt=sse"
          (string-right-trim "/" base)
          (if (find #\/ model) "" "models/")
          model))

(defun google-text-part (text)
  (json-object "text" text))

(defun google-image-part (url)
  "One GenAI image part: a data: URI rides inlineData, anything else rides
fileData."
  ;; image/* is a filter, not a wire mime type, so it is pinned to image/jpeg.
  (multiple-value-bind (media-type data) (parse-data-uri url)
    (if media-type
        (json-object "inlineData"
                     (json-object "mimeType" (if (equal media-type "image/*")
                                                 "image/jpeg" media-type)
                                  "data" data))
        (json-object "fileData"
                     (json-object "mimeType" "image/jpeg" "fileUri" url)))))

(defun google-message-content (msg &aux (role (gethash "role" msg))
                                        (content (gethash "content" msg)))
  "(values role parts) for one chat-shaped message in GenAI form."
  ;; System messages inside the history (the eviction stub) become user text:
  ;; GenAI admits system content only before the first non-system message, and
  ;; the anthropic lane set this precedent.
  (nlk:dispatch role equal
    ("assistant" (let ((parts '())
                       (text (content-text content)))
                   (when (plusp (length text))
                     (push (google-text-part text) parts))
                   ;; A call replays with the thoughtSignature it arrived with:
                   ;; Without this, Gemini 3 would refuse a tool turn's next
                   ;; round (400) when the first functionCall of a step comes
                   ;; back unsigned. Reasoning itself is never replayed as a
                   ;; thought part.
                   (loop for call across (json-array msg "tool_calls")
                         do (multiple-value-bind (name args)
                                (tool-call-function-input call)
                              (push (json-object
                                     "functionCall"
                                     (json-object "name" name "args" args)
                                     :opt "thoughtSignature"
                                     (json-value msg :text "thought_signatures" (gethash "id" call)))
                                    parts)))
                   (values "model" (nreverse parts))))
    ("tool" (let ((name (or (json-value msg :text "name")
                            (gethash "tool_call_id" msg)
                            "")))
              ;; The name is repeated inside response, and the payload nests
              ;; under content (gen_ai_request_body.zig:287-295).
              (values "user"
                      (list (json-object
                             "functionResponse"
                             (json-object
                              "name" name
                              "response" (json-object
                                          "name" name
                                          "content" (content-text content))))))))
    ("system" (values "user" (list (google-text-part (system-stub-text content)))))
    (t (values "user"
               (map-content-parts content
                                  :text-fn #'google-text-part
                                  :image-fn #'google-image-part
                                  :drop-empty-text-p t)))))

(defun google-request-body (context)
  "Build a GenAI generateContent request solely from compiled CONTEXT."
  ;; generationConfig is always present, even empty (the one non-optional
  ;; field); absent controls stay absent keys. topP is deliberately never
  ;; sent.
  (with-request-shape context ()
    (json-object
     "generationConfig"
     (json-object
      :opt "temperature" config.temperature
      :opt "maxOutputTokens" (effective-max-output-tokens context)
      ;; The model_controls.zig materialization: effort becomes a
      ;; thinkingLevel and thoughts are requested whenever it is on;
      ;; an off selection still crosses the wire as thinkingLevel "off"
      ;; with thoughts suppressed (applyGoogle). Gemini's vocabulary stops
      ;; at `high` — it has no adaptive rungs — so an extended ask clamps
      ;; rather than naming a level this API has never defined.
      :opt "thinkingConfig"
      (and effort
           (json-object
            "includeThoughts" (if (string-equal effort "off") :false t)
            "thinkingLevel" (clamp-effort effort "high"))))
     "systemInstruction"
     (json-object "parts" (vector (google-text-part
                                   context.system-prompt)))
     "contents"
     (coerce (loop for msg across (request-messages context)
                   when (hash-table-p msg)
                     collect (multiple-value-bind (role parts)
                                 (google-message-content msg)
                               (json-object "role" role
                                            "parts" (coerce parts 'vector))))
             'vector)
     :opt "tools"
     (and tools
          (vector
           (json-object
            "functionDeclarations"
            ;; The schema rides verbatim, as parametersJsonSchema: `parameters'
            ;; is Gemini's OpenAPI subset; without this field, it would refuse
            ;; the eval tool's nested additionalProperties. This field takes
            ;; JSON Schema whole.
            (tool-declarations tools "parametersJsonSchema"))))
     :when (and tools choice) "toolConfig"
     (json-object
      "functionCallingConfig"
      (json-object "mode" (nlk:dispatch choice equal
                            ("none" "NONE") ("required" "ANY") (t "AUTO")))))))

(define-provider-lane call-google-streaming
    ;; a BASE (gen_ai.zig default_base_url): the model and method suffix append per request
    (:name "google" :family :google :reasoning-carry :text :npm "@ai-sdk/google"
     :default-endpoint "https://generativelanguage.googleapis.com/v1beta")
  "POST to :streamGenerateContent?alt=sse, fold the frame stream, and return
(values MESSAGE USAGE FINISH-REASON REQUEST-JSON) in the chat wire shape."
  ;; End of stream IS the done marker for this lane, so the walk never signals
  ;; PROVIDER-STREAM-INCOMPLETE and carries no degraded-error bookkeeping:
  ;; error frames have no candidates and fold to nothing (the gen_ai.zig
  ;; posture). Usage merges field-by-field across frames — a late partial
  ;; usageMetadata cannot null out earlier counts — and promptTokenCount is
  ;; cache-inclusive, so input is the saturated remainder after
  ;; cachedContentTokenCount.
  (frame :request (encode-json-octets (google-request-body context))
         :state ((tool-counter 0)
                 ;; call id -> the thoughtSignature it carried, for
                 ;; GOOGLE-MESSAGE-CONTENT to replay on the same part
                 (signatures (make-hash-table :test #'equal))
                 (finish-raw nil)
                 (raw-prompt nil))
         :message (:when (plusp (hash-table-count signatures)) "thought_signatures" signatures)
         ;; The gen_ai.zig finish order: drain tools, close reasoning, close
         ;; text, then the terminal finish part.
         :close-order (:tools :reasoning :text)
         ;; The GenAI finishReason in the chat vocabulary (gen_ai.zig:772).
         :finish-reason (let ((tool-calls-p (lane-assembly-tool-buffers asm)))
                          (cond ((null finish-raw) (and tool-calls-p "tool_calls"))
                                ((equal finish-raw "STOP") (if tool-calls-p "tool_calls" "stop"))
                                ((equal finish-raw "MAX_TOKENS") "length")
                                ((member finish-raw '("IMAGE_SAFETY" "RECITATION" "SAFETY"
                                                      "BLOCKLIST" "PROHIBITED_CONTENT" "SPII")
                                         :test #'equal)
                                 "content_filter")
                                ((equal finish-raw "MALFORMED_FUNCTION_CALL") "error")
                                ((member finish-raw '("FINISH_REASON_UNSPECIFIED" "OTHER")
                                         :test #'equal)
                                 "other")
                                (t finish-raw))))
  ;; EOF is normal completion: finish_on_end_of_stream is the google
  ;; contract, and a [DONE] sentinel is a silent no-op.
  (:endpoint (google-stream-url config.endpoint config.model)
   :require-done nil)
  ;; Counts land as they arrive; the raw prompt count refolds input every frame.
  (let ((accum (lane-assembly-usage asm)))
    (loop for (slot key) in '((output-tokens "candidatesTokenCount")
                              (total-tokens "totalTokenCount")
                              (reasoning-tokens "thoughtsTokenCount")
                              (cached-input-tokens "cachedContentTokenCount"))
          for reported = (json-value frame :integer "usageMetadata" key)
          when reported do (setf (slot-value accum slot) reported))
    (setf raw-prompt (or (json-value frame :integer "usageMetadata" "promptTokenCount")
                         raw-prompt))
    (when raw-prompt
      (setf accum.input-tokens
            (max 0 (- raw-prompt (or accum.cached-input-tokens 0))))))
  (loop for candidate across (json-array frame "candidates")
        when (hash-table-p candidate) do
    (loop for part across (json-array candidate "content" "parts")
          when (hash-table-p part) do
      (with-json ((text :string "text")
                  (fc :object "functionCall")
                  (signature :string "thoughtSignature"))
          part
        (when signature
          (assembly-signature-delta asm signature))
        (cond
          (text
           ;; thought is a strict JSON bool; text and
           ;; reasoning fold into one fixed block each
           ;; (the gen_ai.zig text-0/reasoning-0 rule).
           ;; The empty-chunk guard stays here rather than
           ;; inside the helper: unlike the chat lane,
           ;; gen_ai never marks reasoning seen on an empty
           ;; thought.
           (when (plusp (length text))
             (if (eq (gethash "thought" part) t)
                 (assembly-reasoning-delta asm "reasoning-0" text :close-text t)
                 (assembly-text-delta asm "text-0" text :close-reasoning t))))
          (fc
           ;; One whole call per part, id minted
           ;; locally: tool-<n>.
           (let ((args (gethash "args" fc))
                 (id (format nil "tool-~d" tool-counter)))
             (when signature (setf (gethash id signatures) signature))
             (open-tool-buffer
              asm tool-counter
              :id id
              :name (or (json-value fc :string "name") "")
              :arguments (cond
                           ((hash-table-p args) (encode-json-object args))
                           ((eq args :null) "null")
                           (t "{}")))
             (incf tool-counter))))))
    (setf finish-raw (or (json-value candidate :string "finishReason") finish-raw))))
