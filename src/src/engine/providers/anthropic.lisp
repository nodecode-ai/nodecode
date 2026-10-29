;;;; anthropic.lisp --- the Anthropic Messages lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One dialect behind the STREAM-PART waist engine/provider.lisp owns: this
;;;; file shapes the dialect's request and walks its stream. The lane registry,
;;;; the usage invariants, retry policy and connection reuse stay in
;;;; provider.lisp, with what more than one lane shares.

(in-package #:nodecode.evolved)

;;; --- the anthropic messages lane -------------------------------------------
;;; Requests convert the chat-shaped durable history into Anthropic content
;;; blocks; responses assemble back into the chat wire shape, because the
;;; event log stores exactly one dialect. This two-way translation at the lane
;;; boundary is what lets a second dialect exist at all against raw wire-shape
;;; history.

;;; The
;;; table stops at `high` because the rungs above it are an adaptive-thinking
;;; vocabulary, not a bigger number: a budget model asked for `xhigh` or `max`
;;; clamps to `high` rather than inventing a tier the provider never named
;;; (pi-mono clampReasoning). Mutable material.
(defparameter *anthropic-thinking-budgets*
  '(("minimal" . 1024) ("low" . 4000) ("medium" . 8000) ("high" . 16000))
  "Reasoning-effort name to thinking budget_tokens (the model_controls.zig
budget tiers), for the models that spend thinking as a token budget.")

(defun apply-anthropic-usage (accum table)
  "Fold one Messages usage object into ACCUM (messages.zig applyUsage)."
  ;; Anthropic's input_tokens are already cache-exclusive, so no subtraction;
  ;; the cache-write split prefers the cache_creation breakdown over the
  ;; scalar, and a trailing cumulative frame reporting zero never clobbers an
  ;; earlier positive.
  (when (hash-table-p table)
    (with-json ((total    :integer "total_tokens")
                (creation :object  "cache_creation")
                (scalar   :integer "cache_creation_input_tokens"))
        table
      ;; the counts that land as reported, the google lane's way
      (loop for (slot key) in '((input-tokens "input_tokens") (output-tokens "output_tokens")
                                (cached-input-tokens "cache_read_input_tokens"))
            for reported = (json-value table :integer key)
            when reported do (setf (slot-value accum slot) reported))
      (cond (total (setf accum.total-tokens total))
            ((or accum.input-tokens accum.output-tokens)
             (setf accum.total-tokens
                   (+ (or accum.input-tokens 0)
                      (or accum.output-tokens 0)))))
      (cond
        (creation
         (with-json ((five :integer "ephemeral_5m_input_tokens" :default 0)
                     (one :integer "ephemeral_1h_input_tokens" :default 0)
                     (write (+ five one)))
             creation
           (when (or (plusp write) (null accum.cache-write-tokens))
             (setf accum.cache-write-tokens write))
           (when (plusp five) (setf accum.cache-write-5m-tokens five))
           (when (plusp one) (setf accum.cache-write-1h-tokens one))))
        (scalar
         (when (or (plusp scalar) (null accum.cache-write-tokens))
           (setf accum.cache-write-tokens scalar)))))))

(defun anthropic-content-blocks (content)
  "Wire CONTENT as Anthropic content blocks: text parts become text blocks,
image_url parts become image sources, empty and unrenderable parts are dropped."
  ;; The Messages API refuses an empty text block (400 `text content blocks
  ;; must be non-empty'), and the chat wire carries "" freely. Empty content is
  ;; no blocks.
  (map-content-parts content
                     :text-fn #'make-text-content-part
                     :drop-empty-text-p t
                     ;; base64 source for a data: URI, url source else
                     :image-fn (lambda (url)
                                 (multiple-value-bind (media-type data) (parse-data-uri url)
                                   (json-object
                                    "type" "image"
                                    "source" (if media-type
                                                 (json-object "type" "base64"
                                                              "media_type" media-type
                                                              "data" data)
                                                 (json-object "type" "url" "url" url)))))))

(defun anthropic-message-blocks (msg thinking-p &aux (role (gethash "role" msg))
                                                     (content (gethash "content" msg)))
  "Return (values role blocks) for one chat-shaped message, in Anthropic
content-block form; an assistant message's thinking among them only under
THINKING-P."
  ;; System messages inside the history (the eviction stub) become user text:
  ;; Anthropic admits system content only at the top level. Content may be a
  ;; string or a typed part vector; both shapes convert through the one
  ;; content-part seam.
  (nlk:dispatch role equal
    ("assistant" (let ((blocks '())
                       (text (content-text content)))
                   (when thinking-p
                     ;; Redacted thinking replays first, as opaque data blocks: the model
                     ;; reasoned, the API encrypted it, and the replay contract requires
                     ;; the ciphertext back verbatim ahead of the visible content
                     ;; (messages_request_body.zig makeAnthropicReasoningContent).
                     (loop for data across (json-array msg "reasoning_redacted_data")
                           when (stringp data)
                             do (push (json-object "type" "redacted_thinking" "data" data)
                                      blocks))
                     ;; Every signed thinking block of the reply replays, in wire
                     ;; order, ahead of text/tool_use: a budget-thinking model refuses
                     ;; a tool-use continuation without it, and the API binds each
                     ;; block to the one before it, so folding a reply's two blocks
                     ;; into one would fail the check. A block whose text is empty
                     ;; replays too: its signature holds the reasoning. Unsigned
                     ;; reasoning (from a chat-lane history) cannot be replayed as
                     ;; thinking and is dropped.
                     (loop for block across (json-array msg "reasoning_blocks")
                           do (with-json ((reasoning :string "thinking" :default "")
                                          (signature :text "signature"))
                                  block
                                (when signature
                                  (push (json-object "type" "thinking"
                                                     "thinking" reasoning
                                                     "signature" signature)
                                        blocks)))))
                   (when (plusp (length text))
                     (push (make-text-content-part text) blocks))
                   (loop for call across (json-array msg "tool_calls")
                         do (multiple-value-bind (name input) (tool-call-function-input call)
                              (push (json-object "type" "tool_use"
                                                 "id" (or (gethash "id" call) "")
                                                 "name" name
                                                 "input" input)
                                    blocks)))
                   ;; A fully empty assistant message (unsigned reasoning alone) has
                   ;; no blocks: ANTHROPIC-MESSAGES leaves it out.
                   (values "assistant" (nreverse blocks))))
    ("tool" (values "user"
                    (list (json-object
                           "type" "tool_result"
                           "tool_use_id" (or (gethash "tool_call_id" msg) "")
                           ;; A structured tool result rides as blocks; a plain
                           ;; string rides as itself (both are wire-legal
                           ;; tool_result content).
                           "content" (if (stringp content)
                                         content
                                         (let ((blocks (anthropic-content-blocks content)))
                                           (if blocks (coerce blocks 'vector) "")))))))
    ("system" (values "user" (list (make-text-content-part (system-stub-text content)))))
    (t (values "user" (anthropic-content-blocks content)))))

;;; --- the prefix a thinking block is bound to --------------------------------
;;; The newest adaptive models (claude-opus-5-5 among them) sign each thinking
;;; block against the conversation that produced it -- the system prompt, the
;;; set of tools, every message before the block, the thinking that rode ahead
;;; of it included. A request whose prefix
;;; changed behind a replayed one would be refused: 400 `Invalid signature in
;;; thinking block. The block is bound to a different conversation'. An eviction
;;; is such a change (the stub joins the first user message, the oldest rounds
;;; leave), and so is a turn whose prompt pinned another section or registered
;;; another tool. So the lane keeps the API's check itself: the message it
;;; assembles carries the digest of the prefix its request sent
;;; (reasoning_prefix), and its thinking replays only while the prefix before
;;; it still digests the same. A changed prefix sheds every block behind the
;;; change, a leading run the API lets a client drop, and the request goes out
;;; without that reasoning; the API accepts the request with the in-flight
;;; round's tool_use still waiting on that result. An adaptive model that binds
;;; nothing loses only that reasoning, once per change. A budget-thinking model
;;; binds no prefix and wants its in-flight thinking back, so a request that
;;; spends a budget digests nothing and replays every signed block as before.

(defun anthropic-prefix (prefix item)
  "PREFIX, the digest of a request up to here, extended by ITEM: a role name
or the system prompt as itself, a block or a tool declaration as its JSON."
  ;; PREFIX is a fixed-width digest, so no two item runs share one text.
  (nlk:sha256-text (concatenate 'string prefix
                                (if (stringp item) item (encode-json-object item)))))

(defun anthropic-frame-prefix (model system-prompt tools)
  "The digest an adaptive request's messages extend: MODEL, SYSTEM-PROMPT, then
TOOLS, the declarations, by name -- the API binds the tools as a set, so their
order is no change."
  ;; MODEL first: a block one model signed never replays to another, so a
  ;; round a refusal sent to a candidate (CALL-PROVIDER) sheds the selection's
  ;; thinking, and the selection's next round sheds the candidate's.
  (reduce #'anthropic-prefix
          (sort (map 'list #'identity tools) #'string< :key (lambda (tool) (or (gethash "name" tool) "")))
          :initial-value (anthropic-prefix (anthropic-prefix "" model) (or system-prompt ""))))

(defun anthropic-messages (wire &optional prefix)
  "Convert the chat-shaped message vector to Anthropic messages, merging
consecutive same-role entries into one message's content blocks. => (values
MESSAGES PREFIX): PREFIX, an adaptive request's ANTHROPIC-FRAME-PREFIX,
extended over every block the request carries -- what the reply's thinking
is bound to -- or NIL under budget thinking."
  ;; A message with no blocks is left out (the wire refuses empty content);
  ;; its neighbours then merge, so the roles still alternate. Under PREFIX an
  ;; assistant message's thinking replays only behind the prefix its round was
  ;; sent with. The thinking that replays is part of that prefix: the API binds
  ;; a block to the thinking ahead of it too (400 `created as the first
  ;; thinking block after its prompt, but another one now precedes it'), so a
  ;; block shed or replayed again changes the prefix of every round after it,
  ;; and their thinking sheds with it.
  (loop with messages = (make-array 0 :adjustable t :fill-pointer t)
        with role-before = nil
        for msg across wire
        when (hash-table-p msg)
          do (multiple-value-bind (role blocks)
                 (anthropic-message-blocks msg (or (null prefix)
                                                   (equal prefix (gethash "reasoning_prefix" msg))))
               (when blocks
                 (when prefix
                   (unless (equal role role-before)
                     (setf prefix (anthropic-prefix prefix role)))
                   (dolist (block blocks)
                     (setf prefix (anthropic-prefix prefix block))))
                 ;; BLOCKS join the last message when it has ROLE, else stand as one of their own
                 (if (equal role (shiftf role-before role))
                     (let ((last (aref messages (1- (length messages)))))
                       (setf (gethash "content" last)
                             (concatenate 'vector (gethash "content" last) blocks)))
                     (vector-push-extend (json-object "role" role "content" (coerce blocks 'vector))
                                         messages))))
        finally (return (values messages prefix))))

(defun signed-thinking-blocks (entries)
  "ENTRIES, a reply's thinking blocks newest first as (INDEX TEXT SIGNATURE)
streams, as the reasoning_blocks its message replays: wire order, each
block's own text and signature, none without a signature; NIL when none is
signed."
  (let ((blocks (loop for (nil text signature) in (reverse entries)
                      for thought = (get-output-stream-string text)
                      for signed = (get-output-stream-string signature)
                      when (plusp (length signed))
                        collect (json-object "thinking" thought "signature" signed))))
    (and blocks (coerce blocks 'vector))))

(defun anthropic-thinking-budget (effort)
  "EFFORT's thinking budget_tokens, or NIL when it names no thinking at all."
  ;; An extended rung clamps to `high` first, so asking a budget model to
  ;; think harder than its vocabulary can say still buys its deepest budget
  ;; instead of silently sending no thinking block.
  (and (stringp effort)
       (cdr (assoc (clamp-effort effort "high") *anthropic-thinking-budgets*
                   :test #'string-equal))))

(defun anthropic-adaptive-p (effort-levels &aux (ceiling (effort-rank "high")))
  "True when EFFORT-LEVELS carries a rung above `high`."
  ;; Anthropic's adaptive-thinking models are exactly the ones that publish a
  ;; rung above `high`: adaptive thinking is what `xhigh` and `max` name, and
  ;; a budget-based model's ladder stops at `high`. Reading the declared
  ;; ladder keeps this a capability question — the request builders never test
  ;; a model id.
  (and (some (lambda (level) (> (or (effort-rank level) -1) ceiling)) effort-levels)
       t))

;;; --- how a model takes thinking off -----------------------------------------
;;; The adaptive models disagree on how a request turns thinking off. Most
;;; take {"type": "disabled"}; Claude Sonnet 5.5 refuses it and takes
;;; {"type": "between_tools"}; Claude Opus 5.5, like the Fable and Mythos
;;; models, refuses both, because it always thinks. A spelling the model
;;; refuses is a 400 that fails the turn. Anthropic's Models API declares what
;;; each model takes (capabilities.thinking.types, a type false exactly when a
;;; request sending it gets that 400), so the lane asks it, once per endpoint
;;; and model, and only for a request that turns thinking off. An endpoint
;;; that declares nothing, a relay serving no Models API, keeps "disabled",
;;; the spelling every model took before these.

(defvar *anthropic-thinking-types* (make-hash-table :test #'equal :synchronized t)
  "(MODELS-URL . MODEL) -> the thinking types the endpoint at MODELS-URL
declares MODEL takes, a list of type names, or :UNDECLARED when its answer
declared none.")

(defun anthropic-thinking-effort-p (effort)
  "True when EFFORT asks for thinking: a rung, and not off. An effort never
set asks for none."
  (and (stringp effort) (not (string-equal effort "off"))))

(defun anthropic-models-url (endpoint)
  "The Models API beside ENDPOINT, a Messages API URL, or NIL when ENDPOINT
is not one."
  (and (stringp endpoint) (uiop:string-suffix-p endpoint "/messages")
       (uiop:strcat (subseq endpoint 0 (- (length endpoint) (length "/messages"))) "/models")))

(defun fetch-anthropic-thinking-types (url config)
  "(values TYPES DEFINITE-P): the thinking types GET URL/<model> declares
CONFIG's model takes, NIL when it declares none, and whether that answer is
the endpoint's own to keep."
  ;; A body that is no Models API answer (a relay's web page) declares none.
  ;; A refusal is the endpoint's answer too; a timeout, a 408, a 429 or a 5xx
  ;; is not, and the next request asks again.
  (multiple-value-bind (body reason transient)
      (http-fetch (format nil "~a/~a" url (quri:url-encode config.model))
                  :headers (provider-auth-headers :anthropic config.api-key)
                  :timeout *provider-models-fetch-timeout-seconds* :proxy config.proxy)
    (if body
        (let ((types (json-value (ignore-errors (nlk:decode-json (nlk:body-text body)))
                                 :object "capabilities" "thinking" "types")))
          (values (and types
                       (loop for name being the hash-keys of types using (hash-value support)
                             when (json-value support :boolean "supported") collect name))
                  t))
        (values nil (and (not transient)
                         (ppcre:register-groups-bind ((#'parse-integer status))
                             ("^HTTP (\\d+)" reason)
                           (and (<= 400 status 499) (not (member status '(408 429))))))))))

(defun anthropic-thinking-types (config &key fetch)
  "The thinking types CONFIG's endpoint declares its model takes, or NIL when
it declares none or was never asked. FETCH asks it when no answer is kept."
  ;; The hermetic posture, which has no listing cache, dials nothing.
  (let* ((url (anthropic-models-url config.endpoint))
         (key (and url (cons url config.model))))
    (when key
      (multiple-value-bind (kept present) (gethash key *anthropic-thinking-types*)
        (cond (present (and (listp kept) kept))
              ((and fetch *provider-models-cache-path*)
               (multiple-value-bind (types definite) (fetch-anthropic-thinking-types url config)
                 (when definite
                   (setf (gethash key *anthropic-thinking-types*) (or types :undeclared)))
                 types)))))))

(defun anthropic-thinking-off (config)
  "How a request takes thinking off on CONFIG's model: \"disabled\",
\"between_tools\", or NIL when the model always thinks."
  (let ((types (anthropic-thinking-types config)))
    (cond ((or (null types) (member "disabled" types :test #'string=)) "disabled")
          ((member "between_tools" types :test #'string=) "between_tools"))))

(defun anthropic-lowest-effort (levels)
  "The weakest rung LEVELS names above off, or NIL."
  (let ((rungs (remove-if-not (lambda (level) (plusp (or (effort-rank level) 0))) levels)))
    (and rungs (reduce (lambda (a b) (if (< (effort-rank b) (effort-rank a)) b a)) rungs))))

;;; --- anthropic prompt-cache breakpoints ------------------------------------
;;; Anthropic caches by prefix in tools -> system -> messages order, and
;;; honours at most four cache_control breakpoints per request. The tail
;;; marker is the one that makes a multi-round turn cheap (round N+1 reads
;;; round N's whole prompt); the static markers are fallback match points for
;;; when the tail no longer matches (messages_request_body.zig
;;; applyCacheBreakpoints, budget and spend order verbatim).

;;; TTLs are "5m" or "1h". Both 5m is the turn_engine.zig default routing (the
;;; five-minute lifetime is accepted on every Anthropic-compatible route). NIL
;;; disables placement entirely. Mutable material.
(defparameter *anthropic-cache-breakpoints* '(:static "5m" :conversation "5m")
  "Cache breakpoint policy: :STATIC ttl covers tools+system, :CONVERSATION
ttl covers the message tail.")

(defun apply-anthropic-cache-breakpoints (body &aux (policy *anthropic-cache-breakpoints*)
                                                    (betas '()))
  "Place prompt-cache breakpoints on BODY per *ANTHROPIC-CACHE-BREAKPOINTS*."
  ;; Returns the list of anthropic-beta header values the placement requires.
  ;; Spend order is the Zig order: conversation tail, previous round boundary,
  ;; then system (covers tools + system) and tools last, because the tool
  ;; marker only covers tools and only survives system-block churn.
  (when (and policy (or (getf policy :static) (getf policy :conversation)))
    ;; Anthropic honours four breakpoints; explicit markers always win the budget
    (let ((remaining 4))
      (flet ((count-blocks (blocks)
               (when (vectorp blocks)
                 (decf remaining (count-if (lambda (block) (json-value block :any "cache_control"))
                                           blocks))))
             (spend (ttl blocks)
               ;; BLOCKS is the content vector whose last block takes the marker,
               ;; unless one is already set there: only a marker placed spends one.
               (when (plusp remaining)
                 (let* ((one-hour-p (string-equal ttl "1h"))
                        (marker (json-object "type" "ephemeral"
                                             :when one-hour-p "ttl" "1h"))
                        (last (and (vectorp blocks) (plusp (length blocks))
                                   (aref blocks (1- (length blocks))))))
                   (when one-hour-p
                     (pushnew "extended-cache-ttl-2025-04-11" betas :test #'equal))
                   (when (and (hash-table-p last) (null (gethash "cache_control" last)))
                     (setf (gethash "cache_control" last) marker)
                     (decf remaining))))))
        (count-blocks (gethash "system" body))
        (count-blocks (gethash "tools" body))
        (loop for message across (json-array body "messages")
              do (count-blocks (json-value message :any "content")))
        (let ((conversation (getf policy :conversation))
              (messages (json-value body :array "messages")))
          (when conversation
            ;; the final block of the final message: this round's whole prompt, read next round
            (spend conversation (and (plusp (length messages))
                                     (json-value (alexandria:last-elt messages) :any "content")))
            ;; the newest user-role message below it: the previous round's boundary
            (spend conversation (loop for index from (- (length messages) 2) downto 0
                                      for message = (aref messages index)
                                      when (user-role-message-p message)
                                        return (gethash "content" message)))))
        (when-let (static (getf policy :static))
          (spend static (gethash "system" body))
          (spend static (gethash "tools" body))))))
  betas)

(defun anthropic-request-body (context)
  "Build a Messages API request from compiled CONTEXT: system hoisted to the
top level, chat history converted to content blocks, controls applied, cache
breakpoints placed by policy."
  ;; Returns (values BODY BETAS PREFIX) where BETAS is the
  ;; list of anthropic-beta header values the breakpoint TTLs require and PREFIX
  ;; the digest the reply's thinking is bound to (ANTHROPIC-MESSAGES). max_tokens
  ;; comes from the operator's ceiling, else the capability shrink-to-fit, else
  ;; the 32k lane fallback. With thinking enabled, temperature is omitted
  ;; (Anthropic rejects the combination).
  ;;
  ;; Thinking crosses the wire two ways, and which one is the model's own
  ;; answer. A model whose declared ladder reaches above `high` thinks
  ;; adaptively: it decides how much to spend, and the request carries only
  ;; the effort name in a top-level output_config. An adaptive request that
  ;; asks for no thinking, its effort off or never set, says so in the
  ;; model's own spelling (ANTHROPIC-THINKING-OFF), and a model that always
  ;; thinks spends its weakest rung instead. Every other model spends a
  ;; budget, clamped under max_tokens minus answer headroom (the
  ;; planRequestTokens rule); a budget clamped below the wire minimum drops
  ;; thinking rather than sending a request the API refuses.
  (with-request-shape context
      ;; the lane fallback, the wire requiring max_tokens: every current model takes 32k
      ((max-tokens (or (effective-max-output-tokens context) 32000))
       (thinking-p (anthropic-thinking-effort-p effort))
       (adaptive-p (anthropic-adaptive-p
                    config.effort-levels))
       ;; no thinking, as this model spells it: NIL when it always thinks
       (off (and adaptive-p (not thinking-p) (anthropic-thinking-off config)))
       ;; the effort an adaptive request names: the pick, or the weakest rung
       ;; of a model that cannot stop thinking
       (adaptive-effort (and adaptive-p
                             (if thinking-p
                                 effort
                                 (and (null off) (anthropic-lowest-effort config.effort-levels)))))
       ;; 1024 tokens of answer headroom inside max_tokens, and 1024 the wire's minimum budget
       (tier (and (not adaptive-p) (anthropic-thinking-budget effort)))
       (budget (let ((clamped (and tier (min tier (max 0 (- max-tokens 1024))))))
                 (and clamped (>= clamped 1024) clamped)))
       (tools (and tools (tool-declarations tools "input_schema")))
       (mapped-choice (and tools (stringp choice)
                           (cdr (assoc choice
                                       '(("auto" . "auto") ("required" . "any") ("any" . "any"))
                                       :test #'string-equal))))
       (sampling-p (not (or budget adaptive-effort)))
       ;; the head joins before the breakpoints: it is the prefix
       (conversation (multiple-value-list
                      (anthropic-messages (messages-with-head context)
                                          (and adaptive-p
                                               (anthropic-frame-prefix config.model
                                                                       context.system-prompt
                                                                       tools)))))
       (body (json-object
              "model" config.model
              "max_tokens" max-tokens
              "system" (vector (make-text-content-part
                                context.system-prompt))
              "messages" (first conversation)
              :opt "tools" tools
              :opt "tool_choice" (and mapped-choice
                                      (json-object "type" mapped-choice))
              ;; Adaptive thinking: the model decides how much to think,
              ;; so the request names only the effort. `display` is
              ;; load-bearing — the API default on the newest models is
              ;; "omitted", which would blank the thinking text the
              ;; transcript renders. The off spellings take no `display`:
              ;; between_tools refuses one.
              :opt "thinking" (cond (adaptive-effort
                                     (json-object "type" "adaptive" "display" "summarized"))
                                    (off (json-object "type" off)))
              :opt "output_config" (and adaptive-effort
                                        (json-object "effort" adaptive-effort))
              ;; Budget thinking: the older contract, a token allowance
              ;; carved out of max_tokens.
              :opt "thinking" (and budget
                                   (json-object "type" "enabled"
                                                "budget_tokens" budget))
              :opt "temperature" (and sampling-p
                                      config.temperature)
              :opt "top_p" (and sampling-p config.top-p)))
       (betas (apply-anthropic-cache-breakpoints body)))
    (values body betas (second conversation))))

(define-provider-lane call-anthropic-streaming
    (:name "anthropic" :family :anthropic :reasoning-carry :text :path "/messages"
     :default-endpoint "https://api.anthropic.com/v1/messages" :npm "@ai-sdk/anthropic")
  "POST to the Messages API with stream:true, fold the typed event stream, and
return (values MESSAGE USAGE FINISH-REASON) with MESSAGE assembled back into
the chat wire shape (content string, tool_calls vector, reasoning_content)."
  ;; Anthropic streams carry no [DONE]; message_stop is the done marker, and a
  ;; stream ending without it signals PROVIDER-STREAM-INCOMPLETE. Usage
  ;; arrives split across message_start (input side) and message_delta (output
  ;; side), and Anthropic's input_tokens already exclude cache reads, so the
  ;; disjoint invariant holds without subtraction.
  (frame :state ((request-betas nil)
                 ;; The digest of what the request carried before the reply:
                 ;; the prefix its thinking is bound to, NIL under a budget.
                 (request-prefix nil)
                 ;; Redacted thinking blocks: opaque ciphertext captured for
                 ;; replay, newest first here, wire order on the message.
                 (redacted-data '())
                 ;; Thinking blocks, one (INDEX TEXT SIGNATURE) each, the
                 ;; streams the block's own deltas fill; newest first here.
                 (thinking-blocks '())
                 (stop-reason nil))
         :request (progn
                    ;; A request about to take thinking off first learns how
                    ;; its model spells that (ANTHROPIC-THINKING-OFF).
                    (when (and (anthropic-adaptive-p config.effort-levels)
                               (not (anthropic-thinking-effort-p config.reasoning-effort)))
                      (anthropic-thinking-types config :fetch t))
                    (multiple-value-bind (body betas prefix) (anthropic-request-body context)
                      (setf request-betas betas
                            request-prefix prefix)
                      (setf (gethash "stream" body) t)
                      (encode-json-octets body)))
         ;; The messages.zig flushOpenBlocks order: text, reasoning, then the
         ;; tool drain. Runs when a stop_reason arrives and again before
         ;; :finish; every helper is idempotent.
         :close-order (:text :reasoning :tools)
         ;; The message's thinking replays behind this prefix only
         ;; (ANTHROPIC-MESSAGES); redacted thinking ciphertext, wire order,
         ;; for verbatim replay.
         :message (:when (and request-prefix (or redacted-data asm.signature-seen-p))
                   "reasoning_prefix" request-prefix
                   :when redacted-data "reasoning_redacted_data"
                   (coerce (nreverse redacted-data) 'vector)
                   :opt "reasoning_blocks" (signed-thinking-blocks thinking-blocks))
         ;; the stop_reason in the chat finish_reason vocabulary the rest of the system speaks
         :finish-reason (nlk:dispatch stop-reason equal
                          ("end_turn" "stop") ("stop_sequence" "stop") ("tool_use" "tool_calls")
                          ("max_tokens" "length") (t stop-reason)))
  (:headers `(,@(provider-auth-headers :anthropic config.api-key)
              ,@(when request-betas
                  `(("anthropic-beta" . ,(format nil "~{~a~^,~}" request-betas)))))
   ;; the error type as the HTTP status it corresponds to: retry reads transport facts
   :signal-error-fn (lambda (frame)
                      (with-json ((type :string "error" "type")
                                  (message :string "error" "message" :default ""))
                          frame
                        (error 'provider-error
                               :status (cdr (assoc type '(("rate_limit_error" . 429)
                                                          ("overloaded_error" . 529)
                                                          ("api_error" . 500)
                                                          ("invalid_request_error" . 400)
                                                          ("authentication_error" . 401)
                                                          ("permission_error" . 403)
                                                          ("not_found_error" . 404))
                                                   :test #'equal))
                               :detail (format nil "in-stream provider error: ~@[~a: ~]~a"
                                               type message)))))
  (let ((type (json-value frame :string "type")))
    (nlk:dispatch type equal
       ;; Recorded, never signalled here: fatality is decided at
       ;; stream end by the bracket's degraded-error rule.
      ("error" (record-stream-error (or (gethash "error" frame) frame) frame))
      ("message_start" (assembly-response-metadata asm (json-value frame :object "message"))
                       (apply-anthropic-usage asm.usage (json-value frame :any "message" "usage")))
      ("content_block_start" (with-json ((index :integer "index" :default 0)
                                         (block :object "content_block")
                                         (block-type :string "content_block" "type")
                                         (data :string "content_block" "data"))
                                 frame
                               (cond
                                 ((equal block-type "tool_use")
                                  ;; id and name are both known at block start, so
                                  ;; the bracket opens immediately.
                                  (open-tool-buffer asm index
                                                    :id (wire-call-id (gethash "id" block))
                                                    :name (or (gethash "name" block) "")))
                                 ;; Redacted thinking: the API sends ciphertext at
                                 ;; block start and no deltas. The span still opens
                                 ;; (the messages.zig openReasoningWithIndex rule)
                                 ;; and the data is captured for verbatim replay.
                                 ((member block-type '("thinking" "redacted_thinking") :test #'equal)
                                  (when (and data (equal block-type "redacted_thinking"))
                                    (push data redacted-data))
                                  (when (equal block-type "thinking")
                                    (push (list index (make-string-output-stream) (make-string-output-stream))
                                          thinking-blocks))
                                  (lifecycle-open-reasoning asm (format nil "anthropic_reasoning_~d" index))))))
      ("content_block_delta" (with-json ((index :integer "index" :default 0)
                                         (delta-type :string "delta" "type")
                                         (text :string "delta" "text")
                                         (thinking :string "delta" "thinking")
                                         (signature :string "delta" "signature"))
                                 frame
                               (cond
                                 ;; No :CLOSE-REASONING here on purpose: an anthropic
                                 ;; thinking span closes in CLOSE-SPANS, not on
                                 ;; the first text delta after it.
                                 ((and (equal delta-type "text_delta") text)
                                  (assembly-text-delta asm (format nil "anthropic_text_~d" index) text))
                                 ((and (equal delta-type "thinking_delta") thinking)
                                  (when-let (entry (assoc index thinking-blocks))
                                    (write-string thinking (second entry)))
                                  (assembly-reasoning-delta asm (format nil "anthropic_reasoning_~d" index) thinking))
                                 ;; The signature licenses replaying the thinking
                                 ;; block on the next round. It surfaces as its own
                                 ;; part — replay material consumers may persist —
                                 ;; and never as display text.
                                 ((and (equal delta-type "signature_delta") signature)
                                  (when-let (entry (assoc index thinking-blocks))
                                    (write-string signature (third entry)))
                                  (assembly-signature-delta asm signature))
                                 ((equal delta-type "input_json_delta")
                                  (assembly-tool-fragment
                                   asm index (json-value frame :string "delta" "partial_json"))))))
      ("message_delta" (when-let (sr (json-value frame :string "delta" "stop_reason"))
                         (setf stop-reason sr)
                         (close-spans))
                       (apply-anthropic-usage (lane-assembly-usage asm) (gethash "usage" frame)))
      ("message_stop" (finish-stream)))))
